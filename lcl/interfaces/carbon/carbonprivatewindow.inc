{%MainUnit carbonprivate.pp}
{ $Id: $}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

// ==================================================================
// H A N D L E R S 
// ==================================================================

function GetCarbonMsgKeyState: PtrInt;
var
  Modifiers, ButtonState: UInt32;
begin
  Result:=0;

  Modifiers := GetCurrentKeyModifiers; // shift, cpntrol, option, command
  ButtonState := GetCurrentEventButtonState; // Bit 0 first button (left),
   // bit 1 second (right), bit2 third (middle) ...

  if (ButtonState and 1)>0 then
    inc(Result,MK_LButton);
  if (ButtonState and 2)>0 then
    inc(Result,MK_RButton);
  if (ButtonState and 4)>0 then
    inc(Result,MK_MButton);
  if (shiftKey and Modifiers)>0 then
    inc(Result,MK_Shift);
  if (cmdKey and Modifiers)>0 then
    inc(Result,MK_Control);

  //debugln('GetCarbonMsgKeyState Result=',dbgs(KeysToShiftState(Result)),' Modifiers=',hexstr(Modifiers,8),' ButtonState=',hexstr(ButtonState,8));
end;

function GetCarbonShiftState: TShiftState;
var
  Modifiers, ButtonState: UInt32;
begin
  Result:=[];

  Modifiers := GetCurrentKeyModifiers; // shift, cpntrol, option, command
  ButtonState := GetCurrentEventButtonState; // Bit 0 first button (left),
   // bit 1 second (right), bit2 third (middle) ...

  if (ButtonState and 1)>0 then
    Include(Result,ssLeft);
  if (ButtonState and 2)>0 then
    Include(Result,ssRight);
  if (ButtonState and 4)>0 then
    Include(Result,ssMiddle);
  if (shiftKey and Modifiers)>0 then
    Include(Result,ssShift);
  if (cmdKey and Modifiers)>0 then
    Include(Result,ssCtrl);
  if (controlKey and Modifiers)>0 then
    Include(Result,ssMeta);
  if (optionKey and Modifiers)>0 then
    Include(Result,ssAlt);
  if (alphaLock and Modifiers)>0 then
    Include(Result,ssCaps);

  //debugln('GetCarbonShiftState Result=',dbgs(Result),' Modifiers=',hexstr(Modifiers,8),' ButtonState=',hexstr(ButtonState,8));
end;


function CarbonPrivateWindow_Close(ANextHandler: EventHandlerCallRef;
                                   AEvent: EventRef;
                                   AInfo: PWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Msg: TLMessage;
begin
  // Do canclose query, if false then exit

  FillChar(Msg, SizeOf(Msg),0);
  Msg.msg := LM_CLOSEQUERY;

  // Message results : 0 - do nothing, 1 - destroy or hide window
  if DeliverMessage(AInfo^.LCLObject, Msg) = 0
  then begin
    Result := noErr;
    Exit;
  end;

  Result := CallNextEventHandler(ANextHandler, AEvent);
end;

function CarbonPrivateWindow_Closed(ANextHandler: EventHandlerCallRef;
                                    AEvent: EventRef;
                                    AInfo: PWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Msg: TLMessage;
  PrivateClass: TCarbonPrivateWindowClass;
begin
  //debugln('CarbonPrivateWindow_Closed ',DbgSName(AInfo^.LCLObject));
  Result := CallNextEventHandler(ANextHandler, AEvent);
  PrivateClass := TCarbonPrivateWindowClass(AInfo^.WSClass.WSPrivate);

  FillChar(Msg, SizeOf(Msg),0);
  Msg.msg := LM_DESTROY;
  DeliverMessage(AInfo^.LCLObject, Msg);
  
  PrivateClass.UnregisterEvents;
  FreeWidgetInfo(AInfo);
end;


{ TrackProgress:
  Generic function that handles all mouse dragging events }
procedure TrackProgress(AControl: ControlRef; APartCode: ControlPartCode); {$IFDEF darwin}mwpascal;{$ENDIF}
var
  MousePoint: HIPoint;
  AbsMousePos: FPCMacOSAll.Point;
  Window: WindowRef;
  R: FPCMacOSAll.Rect;
  Msg: TLMMouseMove;
  Info:PWidgetInfo;
begin
  {$IFDEF VerboseMouse}
  debugln('TrackProgress');
  {$ENDIF}
  GetGlobalMouse(AbsMousePos);
  Window := HIViewGetWindow(AControl);
  GetWindowBounds(Window, kWindowStructureRgn, R);
  MousePoint.X := AbsMousePos.h - R.Left;
  MousePoint.Y := AbsMousePos.v - R.Top;

  HIViewConvertPoint(MousePoint, nil, AControl);

  FillChar(Msg.Msg,SizeOf(Msg),0);
  Msg.Msg := LM_MOUSEMOVE;
  Msg.XPos := Trunc(MousePoint.X);
  Msg.YPos := Trunc(MousePoint.Y);
  Msg.Keys := GetCarbonMsgKeyState;

  Info := GetWidgetInfo(AControl);
  if Info = nil then begin
    //AControl should be fine but if it isn't, default to the window
    Info := GetWidgetInfo(Window);//HIViewGetFirstSubview(HiViewRef(AControl)));
  end;
    
  if Info <> nil 
  then DeliverMessage(Info^.LCLObject, Msg);
end;

{ CarbonPrivateWindow_ControlTrack
  Handles/Creates LM_MOUSEMOVE events while dragging.
  For mouse move events while not dragging see below
  CarbonPrivateWindow_MouseProc }
function CarbonPrivateWindow_ControlTrack(ANextHandler: EventHandlerCallRef;
                                   AEvent: EventRef;
                                   AInfo: PWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
const
    MSGKIND: array[1..3] of Integer = (LM_LBUTTONUP, LM_RBUTTONUP, LM_MBUTTONUP);
var
  Control: ControlRef;
  MousePoint: HIPoint;//QDPoint;
  ActionUPP, OldActionUPP: ControlActionUPP;
  pt: FPCMacOSAll.Point;
  MouseButton: EventMouseButton;
  Window: WindowRef = nil;
  R: FPCMacOSAll.Rect;
  Msg: TLMMouseMove;
begin                                   
  {$IFDEF VerboseMouse}
  DebugLn('CarbonPrivateWindow_ControlTrack');
  {$ENDIF}
  GetEventParameter(AEvent, kEventParamControlAction, typeControlActionUPP, nil, sizeof(ActionUPP), nil, @OldActionUPP);
  GetEventParameter(AEvent, kEventParamMouseButton, typeMouseButton, nil, SizeOf(MouseButton), nil, @MouseButton);
  GetEventParameter(AEvent, kEventParamWindowRef, typeWindowRef, nil, SizeOf(window), nil, @window);

  ActionUPP := NewControlActionUPP(@TrackProgress);   
  SetEventParameter(AEvent, kEventParamControlAction, typeControlActionUPP, sizeof(ActionUPP), @ActionUPP);

  // this does not return until the mouse is released
  Result := CallNextEventHandler(ANextHandler, AEvent);
                                                   
  SetEventParameter(AEvent, kEventParamControlAction, typeControlActionUPP, sizeof(OldActionUPP), @OldActionUPP);
  DisposeControlActionUPP(ActionUPP);
  
  // now we will create a MouseUp message to send the LCL 
  
  GetGlobalMouse(Pt);

  Control := ControlRef(AInfo^.Widget);
  // if control is the rootview or rootcontrol (parent = nil)
  // use the content area. No conversion is needed
  if HIViewGetSuperView(Control) = nil
  then begin
    // root
    GetWindowBounds(Window, kWindowContentRgn, R);
    MousePoint.X := pt.h - R.left;
    MousePoint.Y := pt.v - R.Top;
  end
  else begin
    // non root
    GetWindowBounds(Window, kWindowStructureRgn, R);
    MousePoint.X := pt.h - R.left;
    MousePoint.Y := pt.v - R.Top;
    HIViewConvertPoint(MousePoint, nil, Control);
  end;
  
  FillChar(Msg,SizeOf(Msg),0);
  if  (MouseButton >= Low(MSGKIND))
  and (MouseButton <= High(MSGKIND))
  then Msg.Msg := MSGKIND[MouseButton];

  Msg.XPos := Trunc(MousePoint.X);
  Msg.YPos := Trunc(MousePoint.Y);
  Msg.Keys := GetCarbonMsgKeyState;
                 
  DeliverMessage(AInfo^.LCLObject, Msg);
end;
                                   
function CarbonPrivateWindow_MouseProc(ANextHandler: EventHandlerCallRef;
                                AEvent: EventRef;
                                AWIndowInfo: PWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Root: ControlRef;     // the root control of the window
  Control: ControlRef;  // the control we are dealing with
                        // or the rootcontrol if none found          
  Info: PWidgetInfo;    // the info specific to the mouse event
                        // or the window's widgetinfo if none found
                                
//
// helper functions used commonly
//
  function GetClickCount: Integer;
  var
    ClickCount: UInt32;
  begin
    GetEventParameter(AEvent, kEventParamClickCount, typeUInt32, nil,
                      SizeOf(ClickCount), nil, @ClickCount);
    Result := ClickCount;
    //debugln('GetClickCount ClickCount=',dbgs(ClickCount));
  end;

  function GetMouseButton:Integer;
  // 1 = left
  // 2 = right
  // 3 = middle
  var
    MouseButton: EventMouseButton;
  begin
    GetEventParameter(AEvent, kEventParamMouseButton, typeMouseButton, nil,
                      SizeOf(MouseButton), nil, @MouseButton);
    Result := MouseButton;
  end;

  function GetMousePoint: TPoint;
  var
    MousePoint: HIPoint;
  begin
    GetEventParameter(AEvent, kEventParamWindowMouseLocation, typeHIPoint, nil,
                      SizeOf(MousePoint), nil, @MousePoint); 
    
    HIViewConvertPoint(MousePoint, nil, Control);
    Result.X := Round(MousePoint.X);
    Result.Y := Round(MousePoint.Y);
  end;

  function GetMouseWheelDelta: Integer;
  var
    WheelDelta: SInt32;
  begin
    GetEventParameter(AEvent, kEventParamMouseWheelDelta, typeSInt32, nil,
                      SizeOf(WheelDelta), nil, @WheelDelta);
    Result := WheelDelta;
    {$IFDEF VerboseMouse}
    debugln('GetMouseWheelDelta WheelDelta=',dbgs(WheelDelta),' ',hexstr(WheelDelta,8));
    {$ENDIF}
  end;

//
// handler functions
//
  procedure HandleMouseDownEvent(var AMsg);
  const
    // array of clickcount x buttontype
    MSGKIND: array[1..4, 1..3] of Integer = (
      (LM_LBUTTONDOWN, LM_RBUTTONDOWN, LM_MBUTTONDOWN),
      (LM_LBUTTONDBLCLK, LM_RBUTTONDBLCLK, LM_MBUTTONDBLCLK),
      (LM_LBUTTONTRIPLECLK, LM_RBUTTONTRIPLECLK, LM_MBUTTONTRIPLECLK),
      (LM_LBUTTONQUADCLK, LM_RBUTTONQUADCLK, LM_MBUTTONQUADCLK)
    );
  var
    MouseButton: Integer;
    ClickCount: Integer;
    MousePoint: TPoint;
    Msg: ^TLMMouse;
    Spec: EventTypeSpec;
  begin
    {$IFDEF VerboseMouse}
    DebugLN('HandleMouseDownEvent');
    {$ENDIF}
    Msg := @AMsg;

    ClickCount := GetClickCount;
    MouseButton := GetMouseButton;
    MousePoint := GetMousePoint;
    
    if (ClickCount < Low(MSGKIND))
    or (ClickCount > High(MSGKIND))
    then ClickCount := 1;
    
    if  (MouseButton < Low(MSGKIND[1]))
    or (MouseButton > High(MSGKIND[1])) then
      exit;
    Msg^.Msg := MSGKIND[ClickCount, MouseButton];
    //debugln('HandleMouseDownEvent CliCount=',dbgs(ClickCount),' MouseButton=',dbgs(MouseButton),' Msg^.Msg=',dbgs(Msg^.Msg));

    Msg^.XPos := MousePoint.X;                         
    Msg^.YPos := MousePoint.Y;
    Msg^.Keys := GetCarbonMsgKeyState;

    Spec := MakeEventSpec(kEventClassControl, kEventControlTrack);
    InstallControlEventHandler(Control, RegisterEventHandler(@CarbonPrivateWindow_ControlTrack), 
      1, @Spec, Info, nil);
  end;

  procedure HandleMouseUpEvent(var AMsg);
  const 
    MSGKIND: array[1..3] of Integer = (LM_LBUTTONUP, LM_RBUTTONUP, LM_MBUTTONUP);
  var
    MouseButton: Integer;
    MousePoint: TPoint;
    Msg: ^TLMMouse;
  begin
    {$IFDEF VerboseMouse}
    DebugLN('HandleMouseUpEvent');
    {$ENDIF}
    // this is not called if NextHandler is called on MouseDown
    // perhaps mousetracking can fix this
    Msg := @AMsg;

    MouseButton := GetMouseButton;
    MousePoint := GetMousePoint;
    
    if  (MouseButton >= Low(MSGKIND))
    and (MouseButton <= High(MSGKIND))
    then Msg^.Msg := MSGKIND[MouseButton];

    Msg^.XPos := MousePoint.X;
    Msg^.YPos := MousePoint.Y;
    Msg^.Keys := GetCarbonMsgKeyState;
  end;

  procedure HandleMouseMovedEvent(var AMsg);
  var
    MousePoint: TPoint;
    MSg: ^TLMMouseMove;
  begin
    {$IFDEF VerboseMouse}
    DebugLN('HandleMouseMovedEvent');
    {$ENDIF}
    Msg := @AMsg;
    
    MousePoint := GetMousePoint;
    
    Msg^.Msg := LM_MOUSEMOVE;
    Msg^.XPos := MousePoint.X;
    Msg^.YPos := MousePoint.Y;
    Msg^.Keys := GetCarbonMsgKeyState;
  end;

  procedure HandleMouseDraggedEvent(var AMsg);
  begin
    {$IFDEF VerboseMouse}
    DebugLN('-- mouse dragged --');
    {$ENDIF}
    //TODO
  end;

  procedure HandleMouseWheelEvent(var AMsg);
  var
    MousePoint: TPoint;
    MSg: ^TLMMouseEvent;
  begin
    {$IFDEF VerboseMouse}
    DebugLN('HandleMouseWheelEvent');
    {$ENDIF}
    Msg := @AMsg;

    MousePoint := GetMousePoint;

    Msg^.Msg := LM_MOUSEWHEEL;
    Msg^.Button := GetMouseButton;
    Msg^.X := MousePoint.X;
    Msg^.Y := MousePoint.Y;
    Msg^.State := GetCarbonShiftState;
    Msg^.WheelDelta := GetMouseWheelDelta;
  end;
  
var
  Msg: record
    Message: TLMessage;
    Extra: array[0..20] of Byte; // some messages are a bit larger, make some room
  end;  
  EventKind: UInt32;
begin
  FillChar(Msg, SizeOf(Msg), 0);

  //Find out which control the mouse event should occur for
  GetRootControl(AWindowInfo^.Widget, Root);
  Control := nil;
  HIViewGetViewForMouseEvent(Root, AEvent, Control);
  if Control = nil 
  then Control := Root;
  
  // if a control other than root is found, send the message 
  // to the control instead of the window 
  // if a lower control without widgetInfo is found, use its parent
  // Note: HIViewGetViewForMouseEvent returns the root if nothing found
  Info := nil;
  while Control <> Root do
  begin
    Info := GetWidgetInfo(Pointer(Control));
    if Info <> nil then Break;
    Control := HIViewGetSuperview(Control);
  end;
  if (Info = nil) or (Control = Root)
  then Info := AWindowInfo;
  
  EventKind := GetEventKind(AEvent);
  case EventKind of
    kEventMouseDown       : HandleMouseDownEvent(Msg);
    kEventMouseUp         : HandleMouseUpEvent(Msg);
    kEventMouseMoved,//      : HandleMouseMovedEvent(Msg);
    kEventMouseDragged    : HandleMouseMovedEvent(Msg);//HandleMouseDraggedEvent(Msg);

    //For the enter and exit events tracking must be enabled
    //tracking is enabled by defining a rect that you want to track
    // TODO: Tracking
    kEventMouseEntered    : Msg.Message.Msg := CM_MOUSEENTER;
    kEventMouseExited     : Msg.Message.Msg := CM_MOUSELEAVE;

    kEventMouseWheelMoved : HandleMouseWheelEvent(Msg);
  else
    exit(EventNotHandledErr);
  end;
  
  // Msg is set in the Appropriate HandleMousexxx procedure
  if DeliverMessage(Info^.LCLObject, Msg) = 0 then begin
    Result :=  EventNotHandledErr; //CallNextEventHandler(ANextHandler, AEvent);
  end
  else begin
    // the LCL does not want the event propagated
    Result := noErr;
  end;
end;

function CarbonPrivateWindow_KeyboardProc(ANextHandler: EventHandlerCallRef;
                                          AEvent: EventRef;
                                          AWIndowInfo: PWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Control: ControlRef;  // the control we are dealing with
                        // or the rootcontrol if none found
  Info: PWidgetInfo;    // the info specific to the mouse event
                        // or the window's widgetinfo if none found

  function GetVKKeyCode: word;
  var
    KeyCode: UInt32;
    KeyChar: char;
    Buf: array[1..6] of byte;
    TextLen: UInt32;
  begin
    KeyChar:=#0;
    KeyCode:=0;
    FillChar(Buf,SizeOf(Buf),0);

    GetEventParameter(AEvent, kEventParamKeyCode, typeUInt32, nil,
                      SizeOf(KeyCode), nil, @KeyCode);
    GetEventParameter(AEvent, kEventParamKeyMacCharCodes, typeChar, nil,
                      SizeOf(KeyChar), nil, @KeyChar);
    Result:=0;
    case KeyCode of
    // Where is the "official" list of Mac key codes?

    // alphabet
    0: Result:=VK_A;
    11: Result:=VK_B;
    8: Result:=VK_C;
    2: Result:=VK_D;
    14: Result:=VK_E;
    3: Result:=VK_F;
    5: Result:=VK_G;
    4: Result:=VK_H;
    34: Result:=VK_I;
    38: Result:=VK_J;
    40: Result:=VK_K;
    37: Result:=VK_L;
    46: Result:=VK_M;
    45: Result:=VK_N;
    31: Result:=VK_O;
    35: Result:=VK_P;
    12: Result:=VK_Q;
    15: Result:=VK_R;
    1: Result:=VK_S;
    17: Result:=VK_T;
    32: Result:=VK_U;
    9: Result:=VK_V;
    13: Result:=VK_W;
    7: Result:=VK_X;
    16: Result:=VK_Y;
    6: Result:=VK_Z;

    // Numbers
    29: Result:=VK_0;
    18: Result:=VK_1;
    19: Result:=VK_2;
    20: Result:=VK_3;
    21: Result:=VK_4;
    23: Result:=VK_5;
    22: Result:=VK_6;
    26: Result:=VK_7;
    28: Result:=VK_8;
    25: Result:=VK_9;

    // Symbols
    {18: Result:=VK_exclam;
    19: Result:=VK_at;
    20: Result:=VK_numbersign;
    21: Result:=VK_dollar;
    23: Result:=VK_percent;
    22: Result:=VK_asciicircum;
    26: Result:=VK_ampersand;
    28: Result:=VK_asterisk;
    25: Result:=VK_parenleft;
    29: Result:=VK_parenright;
    27: Result:=VK_minus;
    27: Result:=VK_underscore;
    24: Result:=VK_equal;
    24: Result:=VK_plus;}
    //10: Result:=VK_grave;
    //10: Result:=VK_asciitilde;
    //33: Result:=VK_bracketleft;
    //33: Result:=VK_braceleft;
    //30: Result:=VK_bracketright;
    //30: Result:=VK_braceright;
    //41: Result:=VK_semicolon;
    //41: Result:=VK_colon;
    //39: Result:=VK_apostrophe;
    //39: Result:=VK_quotedbl;
    //43: Result:=VK_comma;
    //43: Result:=VK_less;
    //47: Result:=VK_period;
    //47: Result:=VK_greater;
    //44: Result:=VK_slash;
    //44: Result:=VK_question;
    //42: Result:=VK_backslash;
    //42: Result:=VK_bar;

    // "Special" keys
    49: Result:=VK_space;
    36: Result:=VK_Return;
    117: Result:=VK_Delete;
    48: Result:=VK_Tab;
    53: Result:=VK_Escape;
    //57: Result:=VK_Caps_Lock;
    71: Result:=VK_NumLock;
    107: Result:=VK_Scroll;
    113: Result:=VK_Pause;
    51: Result:=VK_Back;
    114: Result:=VK_Insert;

    // Cursor movement
    126: Result:=VK_Up;
    125: Result:=VK_Down;
    123: Result:=VK_Left;
    124: Result:=VK_Right;
    116: Result:=VK_PRIOR;
    121: Result:=VK_NEXT;
    115: Result:=VK_Home;
    119: Result:=VK_End;

    // Numeric keypad
    82: Result:=VK_0;
    83: Result:=VK_1;
    84: Result:=VK_2;
    85: Result:=VK_3;
    86: Result:=VK_4;
    87: Result:=VK_5;
    88: Result:=VK_6;
    89: Result:=VK_7;
    91: Result:=VK_8;
    92: Result:=VK_9;
    76: Result:=VK_RETURN;
    65: Result:=VK_Decimal;
    69: Result:=VK_Add;
    78: Result:=VK_Subtract;
    67: Result:=VK_Multiply;
    75: Result:=VK_Divide;

    // Function keys
    122: Result:=VK_F1;
    120: Result:=VK_F2;
    99: Result:=VK_F3;
    118: Result:=VK_F4;
    96: Result:=VK_F5;
    97: Result:=VK_F6;
    98: Result:=VK_F7;
    100: Result:=VK_F8;
    101: Result:=VK_F9;
    109: Result:=VK_F10;
    103: Result:=VK_F11;
    111: Result:=VK_F12;

    // Modifier keys
    56: Result:=VK_LShift;
    //56: Result:=VK_RShift;
    59: Result:=VK_LControl;
    //59: Result:=VK_RControl;
    //58: Result:=VK_Meta_L;
    //58: Result:=VK_Meta_R;
    55: Result:=VK_LMenu;
    //55: Result:=VK_RMenu;
    else
      case KeyChar of
      'a'..'z': Result:=VK_A+ord(KeyChar)-ord('a');
      'A'..'Z': Result:=VK_A+ord(KeyChar)-ord('A');
      '+': Result:=VK_ADD;
      '-': Result:=VK_SUBTRACT;
      ' ': Result:=VK_SPACE;
      #28: Result:=VK_LEFT;
      #29: Result:=VK_RIGHT;
      #30: Result:=VK_UP;
      #31: Result:=VK_DOWN;
      end;
    end;

    GetEventParameter(AEvent, kEventParamKeyUnicodes, typeUnicodeText, nil,
                      6, @TextLen, @Buf[1]);
    {$IFDEF VerboseKeyboard}
    debugln('GetVKKeyCode Result=',dbgs(Result),'=',DbgsVKCode(Result),' KeyCode='+dbgs(KeyCode),' KeyChar='+DbgStr(KeyChar),' TextLen='+dbgs(TextLen),
      ' '+dbgs(Buf[1])+','+dbgs(Buf[2])+','+dbgs(Buf[3])+','+dbgs(Buf[4])+','+dbgs(Buf[5])+','+dbgs(Buf[6]));
    {$ENDIF}
  end;
  
  function GetCharacterCode: word;
  var
    KeyChar: char;
  begin
    KeyChar:=#0;
    GetEventParameter(AEvent, kEventParamKeyMacCharCodes, typeChar, nil,
                      SizeOf(KeyChar), nil, @KeyChar);
    Result:=ord(KeyChar);
    {$IFDEF VerboseKeyboard}
    debugln('GetCharacterCode Result=',dbgs(Result),' KeyChar='+DbgStr(KeyChar));
    {$ENDIF}
  end;
  
  function GetUTF8Character: TUTF8Char;
  var
    UTF16Buf: array[1..3] of word;
    TextLen: UInt32;
    CharLen: integer;
    u: cardinal;
  begin
    // get the character as UTF16
    GetEventParameter(AEvent, kEventParamKeyUnicodes, typeUnicodeText, nil,
                      6, @TextLen, @UTF16Buf[1]);
    u:=UTF16CharacterToUnicode(PWideChar(@UTF16Buf[1]),CharLen);
    if CharLen=0 then ;
    Result:=UnicodeToUTF8(u);
    {$IFDEF VerboseKeyboard}
    debugln('GetUTF8Character Result=',dbgstr(Result),' unicode='+DbgS(u),
      ' '+hexStr(UTF16Buf[1],4)+','+hexStr(UTF16Buf[2],4)+','+hexStr(UTF16Buf[3],4));
    debugln('GetUTF8Character "',Result,'"');
    {$ENDIF}
  end;

  procedure HandleRawKeyDownEvent;
  var
    KeyMsg: TLMKeyDown;
    CharMsg: TLMChar;
    IsSysKey: Boolean;
    UTF8Character: TUTF8Char;
  begin
    {$IFDEF VerboseKeyboard}
    DebugLN('HandleRawKeyDownEvent Info^.LCLObject=',DbgSName(Info^.LCLObject));
    {$ENDIF}
    IsSysKey:=(GetCurrentKeyModifiers and optionKey)>0;
    
    // create the CN_KEYDOWN message
    FillChar(KeyMsg, SizeOf(KeyMsg), 0);
    KeyMsg.Msg := CN_KEYDOWN;
    KeyMsg.KeyData := GetCarbonMsgKeyState;
    KeyMsg.CharCode := GetVKKeyCode;

    if KeyMsg.CharCode<>0 then begin
      // Msg is set in the Appropriate HandleKeyxxx procedure
      if DeliverMessage(Info^.LCLObject, KeyMsg) = 0 then begin
        Result :=  EventNotHandledErr;
        if KeyMsg.CharCode=0 then
          exit;
      end
      else begin
        // the LCL does not want the event propagated
        Result := noErr;
        exit;
      end;
    end;
    
    // send the UTF8 keypress
    // try to get the UTF8 representation of the key
    UTF8Character := GetUTF8Character;
    if UTF8Character<>'' then begin
      if TWinControl(Info^.LCLObject).IntfUTF8KeyPress(UTF8Character,1,IsSysKey)
      then begin
        // the LCL has handled the key
        Result := noErr;
        exit;
      end;
    end;

    // create the LM_CHAR / LM_SYSCHAR message
    FillChar(CharMsg, SizeOf(CharMsg), 0);
    if IsSysKey then
      CharMsg.Msg := LM_SYSCHAR
    else
      CharMsg.Msg := LM_CHAR;
    CharMsg.KeyData := GetCarbonMsgKeyState;
    CharMsg.CharCode := GetCharacterCode;

    if CharMsg.CharCode<>0 then begin
      // Msg is set in the Appropriate HandleKeyxxx procedure
      if DeliverMessage(Info^.LCLObject, CharMsg) = 0 then begin
        Result :=  EventNotHandledErr;
      end
      else begin
        // the LCL does not want the event propagated
        Result := noErr;
      end;
    end;
  end;

  procedure HandleRawKeyUpEvent;
  var
    KeyMsg: TLMKeyUp;
  begin
    {$IFDEF VerboseKeyboard}
    DebugLN('HandleRawKeyUpEvent Info^.LCLObject=',DbgSName(Info^.LCLObject));
    {$ENDIF}

    // create the CN_KEYUP message
    FillChar(KeyMsg, SizeOf(KeyMsg), 0);
    KeyMsg.Msg := CN_KEYUP;
    KeyMsg.KeyData := GetCarbonMsgKeyState;
    KeyMsg.CharCode := GetVKKeyCode;

    if KeyMsg.CharCode<>0 then begin
      // Msg is set in the Appropriate HandleKeyxxx procedure
      if DeliverMessage(Info^.LCLObject, KeyMsg) = 0 then begin
        Result :=  EventNotHandledErr;
        if KeyMsg.CharCode=0 then
          exit;
      end
      else begin
        // the LCL does not want the event propagated
        Result := noErr;
        exit;
      end;
    end;
  end;

var
  Msg: record
    Message: TLMessage;
    Extra: array[0..20] of Byte; // some messages are a bit larger, make some room
  end;
  EventKind: UInt32;
  Root: ControlRef;
begin
  Result := EventNotHandledErr;
  FillChar(Msg, SizeOf(Msg), 0);

  GetRootControl(AWindowInfo^.Widget, Root);
  Control := nil;
  //HIViewGetViewForMouseEvent(Root, AEvent, Control);
  if Control = nil
  then Control := Root;

  // if a control other than root is found, send the message
  // to the control instead of the window
  // if a lower control without widgetInfo is found, use its parent
  Info := nil;
  while Control <> Root do
  begin
    Info := GetWidgetInfo(Pointer(Control));
    if Info <> nil then Break;
    Control := HIViewGetSuperview(Control);
  end;
  if (Info = nil) or (Control = Root)
  then Info := AWindowInfo;

  EventKind := GetEventKind(AEvent);
  case EventKind of
    kEventRawKeyDown: HandleRawKeyDownEvent;
    kEventRawKeyRepeat: HandleRawKeyDownEvent;
    kEventRawKeyUp: HandleRawKeyUpEvent;
  else
    exit(EventNotHandledErr);
  end;
  
  if Msg.Message.Msg=0 then
    exit(EventNotHandledErr);

  // Msg is set in the Appropriate HandleKeyxxx procedure
  if DeliverMessage(Info^.LCLObject, Msg) = 0 then begin
    Result :=  EventNotHandledErr; //CallNextEventHandler(ANextHandler, AEvent);
  end
  else begin
    // the LCL does not want the event propagated
    Result := noErr;
  end;
end;

function CarbonPrivateWindow_Update(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWindowInfo: PWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  debugln('CarbonPrivateWindow_Update ',DbgSName(AWindowInfo^.LCLObject));
  Result := CallNextEventHandler(ANextHandler, AEvent);
end;

function CarbonPrivateWindow_DrawContent(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWindowInfo: PWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  debugln('CarbonPrivateWindow_DrawContent ',DbgSName(AWindowInfo^.LCLObject));
  Result := CallNextEventHandler(ANextHandler, AEvent);
end;

// ==================================================================
// C L A S S 
// ==================================================================

  { TCarbonPrivateWindow }

class procedure TCarbonPrivateWindow.RegisterEvents(AInfo: PWidgetInfo);
var
  MouseSpec: array [0..6] of EventTypeSpec;
  TmpSpec: EventTypeSpec;
  KeySpecs: array[0..2] of EventTypeSpec;
begin
  TmpSpec := MakeEventSpec(kEventClassWindow, kEventWindowClose);
  InstallWindowEventHandler(AInfo^.Widget,
    RegisterEventHandler(@CarbonPrivateWindow_Close),
    1, @TmpSpec, Pointer(AInfo), nil);

  TmpSpec := MakeEventSpec(kEventClassWindow, kEventWindowClosed);
  InstallWindowEventHandler(AInfo^.Widget,
    RegisterEventHandler(@CarbonPrivateWindow_Closed),
    1, @TmpSpec, Pointer(AInfo), nil);

  MouseSpec[0].eventClass := kEventClassMouse;
  MouseSpec[0].eventKind := kEventMouseDown;
  MouseSpec[1].eventClass := kEventClassMouse;
  MouseSpec[1].eventKind := kEventMouseUp;
  MouseSpec[2].eventClass := kEventClassMouse;
  MouseSpec[2].eventKind := kEventMouseMoved;
  MouseSpec[3].eventClass := kEventClassMouse;
  MouseSpec[3].eventKind := kEventMouseDragged;
  MouseSpec[4].eventClass := kEventClassMouse;
  MouseSpec[4].eventKind := kEventMouseEntered;
  MouseSpec[5].eventClass := kEventClassMouse;
  MouseSpec[5].eventKind := kEventMouseExited;
  MouseSpec[6].eventClass := kEventClassMouse;
  MouseSpec[6].eventKind := kEventMouseWheelMoved;

  InstallWindowEventHandler(AInfo^.Widget,
    RegisterEventHandler(@CarbonPrivateWindow_MouseProc),
    7, @MouseSpec[0], Pointer(AInfo), nil);
    
  KeySpecs[0].eventClass := kEventClassKeyboard;
  KeySpecs[0].eventKind := kEventRawKeyDown;
  KeySpecs[1].eventClass := kEventClassKeyboard;
  KeySpecs[1].eventKind := kEventRawKeyRepeat;
  KeySpecs[2].eventClass := kEventClassKeyboard;
  KeySpecs[2].eventKind := kEventRawKeyUp;

  InstallWindowEventHandler(AInfo^.Widget,
    RegisterEventHandler(@CarbonPrivateWindow_KeyboardProc),
    3, @KeySpecs[0], Pointer(AInfo), nil);
    
  debugln('TCarbonPrivateWindow.RegisterEvents ',DbgSName(AInfo^.LCLObject));
  TmpSpec := MakeEventSpec(kEventClassWindow, kEventWindowUpdate);
  InstallWindowEventHandler(AInfo^.Widget,
    RegisterEventHandler(@CarbonPrivateWindow_Update),
    1, @TmpSpec, Pointer(AInfo), nil);

  TmpSpec := MakeEventSpec(kEventClassWindow, kEventWindowDrawContent);
  InstallWindowEventHandler(AInfo^.Widget,
    RegisterEventHandler(@CarbonPrivateWindow_DrawContent),
    1, @TmpSpec, Pointer(AInfo), nil);
end;

class procedure TCarbonPrivateWindow.UnregisterEvents;
begin                      
  UnregisterEventHandler(@CarbonPrivateWindow_Close);
  UnregisterEventHandler(@CarbonPrivateWindow_Closed); 
  UnregisterEventHandler(@CarbonPrivateWindow_MouseProc);
end;
