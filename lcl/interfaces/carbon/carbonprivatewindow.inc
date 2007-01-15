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

  Modifiers := GetCurrentEventKeyModifiers; // shift, control, option, command
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

  Modifiers := GetCurrentEventKeyModifiers; // shift, control, option, command
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

  Info := GetCtrlWidgetInfo(AControl);
  if Info = nil then begin
    //AControl should be fine but if it isn't, default to the window
    Info := GetWndWidgetInfo(Window);//HIViewGetFirstSubview(HiViewRef(AControl)));
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

//???? What are we doing here????
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
    Info := GetCtrlWidgetInfo(Pointer(Control));
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
  Control: ControlRef;      // the control we are dealing with
                            // or the rootcontrol if none found
  Info: PWidgetInfo;        // the info specific to the mouse event
                            // or the window's widgetinfo if none found
  KeyChar : char;           //Ascii char, when possible (xx_(SYS)CHAR)
  UTF8Character: TUTF8Char; //char to send via IntfUtf8KeyPress
  VKKeyCode : word;         //VK_ code
  SendChar : boolean;       //Should we send char?
  IsSysKey: Boolean;        //Is alt (option) key down?
  KeyData : PtrInt;         //Modifiers (ctrl, alt, mouse buttons...)
  EventKind: UInt32;        //The kind of this event

  //See what changed in the modifiers flag so that we can emulate a keyup/keydown
  //note: this function assumes that only a bit of the flag can be modified at
  //once
  function EmulateModifiersDownUp : boolean;
  var CurMod, diff : UInt32;
  begin
    Result:=false;
    SendChar:=false;
    if GetEventParameter(AEvent, kEventParamKeyModifiers, typeUInt32, nil,
                         sizeof(CurMod), nil, @CurMod)<>noErr then exit;

    //see what changed. we only care of bits 8 through 12
    diff:=(PrevKeyModifiers xor CurMod) and $1F00;

    //diff is now equal to the mask of the bit that changed, so we can determine
    //if this change is a keydown (PrevKeyModifiers didn't have the bit set) or
    //a keyup (PrevKeyModifiers had the bit set)
    if (PrevKeyModifiers and diff)=0 then EventKind:=kEventRawKeyDown
    else EventKind:=kEventRawKeyUp;

    PrevKeyModifiers:=CurMod;

    case diff of
      0          : exit;  //nothing (that we cared of) changed
      cmdKey     : VKKeyCode := VK_CONTROL; //command mapped to control
      shiftKey   : VKKeyCode := VK_SHIFT;
      alphaLock  : VKKeyCode := VK_CAPITAL; //caps lock
      optionKey  : VKKeyCode := VK_MENU;    //option is alt
      controlKey : VKKeyCode := VK_LWIN;    //meta... map to left Windows Key?
      else exit; //Error! More that one bit changed in the modifiers?
    end;
    Result:=true;
    {$IFDEF VerboseKeyboard}
    debugln('[EmulateModifiersDownUp] VK=',DbgsVKCode(VKKeyCode));
    {$ENDIF}
  end;


(*
  Mac keycodes handling is not so straight. For an explanation, see
  mackeycodes.inc
  In this function, we do the following:
   1) Get the raw keycode, if it is a known "non-printable" key, translate it
      to a known VK_ keycode.
      This will be reported via xx_KeyDown/KeyUP messages only, and we can stop
      here.
   2) else, we must send both KeyDown/KeyUp and IntfUTF8KeyPress/xx_(SYS)CHAR
      So, get the unicode character and the "ascii" character (note: if it's
      not a true ascii character (>127) use the Mac character).
    2a) Try to determine a known VK_ keycode (e.g: VK_A, VK_SPACE and so on)
    2b) If no VK_ keycode exists, use a dummy keycode to trigger LCL events
        (see later in the code for a more in depth explanation)
*)

  function TranslateMacKeyCode : boolean;
  var KeyCode: UInt32;
      TextLen : UInt32;
      CharLen : integer;
      buf : array[1..6] of byte; //isn't 4 bytes enough?
      u : cardinal;
  begin
    Result:=false;
    SendChar:=false;
    VKKeyCode:=VK_UNKNOWN;

    KeyData:=GetCarbonMsgKeyState;
    IsSysKey:=(GetCurrentEventKeyModifiers and optionKey)>0;

    if GetEventParameter(AEvent, kEventParamKeyCode, typeUInt32, nil,
                         sizeof(KeyCode), nil, @KeyCode)<>noErr then exit;

    //non-printable keys (see mackeycodes.inc)
    //for these keys, only send keydown/keyup (not char or UTF8KeyPress)
    case KeyCode of
      MK_F1       : VKKeyCode:=VK_F1;
      MK_F2       : VKKeyCode:=VK_F2;
      MK_F3       : VKKeyCode:=VK_F3;
      MK_F4       : VKKeyCode:=VK_F4;
      MK_F5       : VKKeyCode:=VK_F5;
      MK_F6       : VKKeyCode:=VK_F6;
      MK_F7       : VKKeyCode:=VK_F7;
      MK_F8       : VKKeyCode:=VK_F8;
      MK_F9       : VKKeyCode:=VK_F9;
      MK_F10      : VKKeyCode:=VK_F10;
      MK_F11      : VKKeyCode:=VK_F11;
      MK_F12      : VKKeyCode:=VK_F12;
      MK_F13      : VKKeyCode:=VK_SNAPSHOT;
      MK_F14      : VKKeyCode:=VK_SCROLL;
      MK_F15      : VKKeyCode:=VK_PAUSE;
      MK_POWER    : VKKeyCode:=VK_SLEEP; //?
      MK_TAB      : VKKeyCode:=VK_TAB; //strangely enough, tab is "non printable"
      MK_INS      : VKKeyCode:=VK_INSERT;
      MK_DEL      : VKKeyCode:=VK_DELETE;
      MK_HOME     : VKKeyCode:=VK_HOME;
      MK_END      : VKKeyCode:=VK_END;
      MK_PAGUP    : VKKeyCode:=VK_PRIOR;
      MK_PAGDN    : VKKeyCode:=VK_NEXT;
      MK_UP       : VKKeyCode:=VK_UP;
      MK_DOWN     : VKKeyCode:=VK_DOWN;
      MK_LEFT     : VKKeyCode:= VK_LEFT;
      MK_RIGHT    : VKKeyCode:= VK_RIGHT;
      MK_NUMLOCK  : VKKeyCode:= VK_NUMLOCK;
    end;
    
    if VKKeyCode<>VK_UNKNOWN then
    begin
      //stop here, we won't send char or UTF8KeyPress
      {$IFDEF VerboseKeyboard}
      debugln('[TranslateMacKeyCode] VK=',DbgsVKCode(VKKeyCode));
      {$ENDIF}
      Result:=true;
      exit;
    end;
    
    //printable keys
    //for these keys, send char or UTF8KeyPress
    if GetEventParameter(AEvent, kEventParamKeyUnicodes, typeUnicodeText, nil,
                      6, @TextLen, @Buf[1])<>noErr then exit;
    if TextLen>0 then
    begin
      SendChar:=true;
      u:=UTF16CharacterToUnicode(PWideChar(@Buf[1]),CharLen);
      if CharLen=0 then exit;
      UTF8Character:=UnicodeToUTF8(u);
      if ord(Utf8Character[1])<=127 then //It's (true) ascii.
        KeyChar:=Utf8Character[1]
      else //not ascii, get the Mac character.
        if GetEventParameter(AEvent, kEventParamKeyMacCharCodes, typeChar, nil,
                             sizeof(KeyChar), nil, @KeyChar)<>noErr then exit;
      case KeyChar of
        'a'..'z' : VKKeyCode:=VK_A+ord(KeyChar)-ord('a');
        'A'..'Z' : VKKeyCode:=ord(KeyChar);
        #27      : VKKeyCode:=VK_ESCAPE;
        #8     : VKKeyCode:=VK_BACK;
        ' '      : VKKeyCode:=VK_SPACE;
        #13      : VKKeyCode:=VK_RETURN;
        '0'..'9' : case KeyCode of
                     MK_NUMPAD0 : VKKeyCode:=VK_NUMPAD0;
                     MK_NUMPAD1 : VKKeyCode:=VK_NUMPAD1;
                     MK_NUMPAD2 : VKKeyCode:=VK_NUMPAD2;
                     MK_NUMPAD3 : VKKeyCode:=VK_NUMPAD3;
                     MK_NUMPAD4 : VKKeyCode:=VK_NUMPAD4;
                     MK_NUMPAD5 : VKKeyCode:=VK_NUMPAD5;
                     MK_NUMPAD6 : VKKeyCode:=VK_NUMPAD6;
                     MK_NUMPAD7 : VKKeyCode:=VK_NUMPAD7;
                     MK_NUMPAD8 : VKKeyCode:=VK_NUMPAD8;
                     MK_NUMPAD9 : VKKeyCode:=VK_NUMPAD9
                     else VKKeyCode:=ord(KeyChar);
                   end
        else
        case KeyCode of
          MK_PADDIV  : VKKeyCode:=VK_DIVIDE;
          MK_PADMULT : VKKeyCode:=VK_MULTIPLY;
          MK_PADSUB  : VKKeyCode:=VK_SUBTRACT;
          MK_PADADD  : VKKeyCode:=VK_ADD;
          MK_PADDEC  : VKKeyCode:=VK_DECIMAL;
          MK_PADENTER: begin VKKeyCode:=VK_RETURN; KeyChar:=#13; UTF8Character:=KeyChar; end;
        end;
      end;

      //There is no known VK_ code for this characther. Use a dummy keycode
      //(E8, which is unused by Windows) so that KeyUp/KeyDown events will be
      //triggered by LCL.
      //Note: we can't use the raw mac keycode, since it could collide with
      //well known VK_ keycodes (e.g on my italian ADB keyboard, keycode for
      //"&egrave;" is 33, which is the same as VK_PRIOR)
      if VKKeyCode=VK_UNKNOWN then VKKeyCode:=$E8;

      {$IFDEF VerboseKeyboard}
      debugln('[TranslateMacKeyCode] VK=',DbgsVKCode(VKKeyCode),' Utf8="',UTF8Character,'" KeyChar="',KeyChar,'"');
      {$ENDIF}
      Result:=true;
    end
    else Debugln('[TranslateMacKeyCode] ***WARNING: Can''t get unicode character!***');
  end;

  function HandleRawKeyDownEvent : OSStatus;
  var
    KeyMsg: TLMKeyDown;
    CharMsg: TLMChar;
  begin
    Result:=EventNotHandledErr;
    {$IFDEF VerboseKeyboard}
    DebugLN('[HandleRawKeyDownEvent] Info^.LCLObject=',DbgSName(Info^.LCLObject));
    {$ENDIF}

    // create the CN_KEYDOWN message
    FillChar(KeyMsg, SizeOf(KeyMsg), 0);
    if IsSysKey then KeyMsg.Msg := CN_SYSKEYDOWN
    else KeyMsg.Msg := CN_KEYDOWN;
    KeyMsg.KeyData := KeyData;
    KeyMsg.CharCode := VKKeyCode;

    //Send message to LCL
    if VKKeyCode<>VK_UNKNOWN then
    begin
      if (DeliverMessage(Info^.LCLObject, KeyMsg) <> 0) or (KeyMsg.CharCode=VK_UNKNOWN) then
      begin
        // the LCL handled the key
        {$IFDEF VerboseKeyboard}
        Debugln('[HandleRawKeyDownEvent] LCL handled CN_KEYDOWN, exiting');
        {$ENDIF}
        Result := noErr;
        exit;
      end;

      //Here is where we (interface) can do something with the key
      //Call the standard handler.
      Result := CallNextEventHandler(ANextHandler, AEvent);

      //Send a LM_(SYS)KEYDOWN
      if IsSysKey then KeyMsg.Msg := LM_SYSKEYDOWN
      else KeyMsg.Msg := LM_KEYDOWN;
      if (DeliverMessage(Info^.LCLObject, KeyMsg) <> 0) or (KeyMsg.CharCode=VK_UNKNOWN) then
      begin
        // the LCL handled the key
        {$IFDEF VerboseKeyboard}
        Debugln('[HandleRawKeyDownEvent] LCL handled LM_KEYDOWN, exiting');
        {$ENDIF}
        //Result already set by CallNextEventHandler
        exit;
      end;
    end;

    //We should send a character
    if SendChar then
    begin
      // send the UTF8 keypress
      if TWinControl(Info^.LCLObject).IntfUTF8KeyPress(UTF8Character,1,IsSysKey) then
      begin
        // the LCL has handled the key
        {$IFDEF VerboseKeyboard}
        Debugln('[HandleRawKeyDownEvent] LCL handled IntfUTF8KeyPress, exiting');
        {$ENDIF}
        if Result=EventNotHandledErr then
          Result := noErr;
        exit;
      end;

      // create the CN_CHAR / CN_SYSCHAR message
      FillChar(CharMsg, SizeOf(CharMsg), 0);
      if IsSysKey then CharMsg.Msg := CN_SYSCHAR
      else CharMsg.Msg := CN_CHAR;
      CharMsg.KeyData := KeyData;
      CharMsg.CharCode := ord(KeyChar);

      //Send message to LCL
      if (DeliverMessage(Info^.LCLObject, CharMsg) <> 0) or (CharMsg.CharCode=VK_UNKNOWN) then
      begin
        // the LCL handled the key
        {$IFDEF VerboseKeyboard}
        Debugln('[HandleRawKeyDownEvent] LCL handled CN_CHAR, exiting');
        {$ENDIF}
        if Result=EventNotHandledErr then
          Result := noErr;
        exit;
      end;

      //Here is where we (interface) can do something with the key
      //Call the standard handler if not called already
      if Result=EventNotHandledErr then
        Result := CallNextEventHandler(ANextHandler, AEvent);

      //Send a LM_(SYS)CHAR
      if IsSysKey then CharMsg.Msg := LM_SYSCHAR
      else CharMsg.Msg := LM_CHAR;
      if DeliverMessage(Info^.LCLObject, CharMsg) <> 0 then
      begin
        // the LCL handled the key
        {$IFDEF VerboseKeyboard}
        Debugln('[HandleRawKeyDownEvent] LCL handled LM_CHAR, exiting');
        {$ENDIF}
        if Result=EventNotHandledErr then
          Result := noErr;
        exit;
      end;

    end;
  end;

  function HandleRawKeyUpEvent : OSStatus;
  var
    KeyMsg: TLMKeyUp;
  begin
    Result:=EventNotHandledErr;
    {$IFDEF VerboseKeyboard}
    DebugLN('[HandleRawKeyUpEvent] Info^.LCLObject=',DbgSName(Info^.LCLObject));
    {$ENDIF}

    // create the CN_KEYUP message
    FillChar(KeyMsg, SizeOf(KeyMsg), 0);
    if IsSysKey then KeyMsg.Msg := CN_SYSKEYUP
    else KeyMsg.Msg := CN_KEYUP;
    KeyMsg.KeyData := KeyData;
    KeyMsg.CharCode := VKKeyCode;

    //Send message to LCL
    if VKKeyCode<>VK_UNKNOWN then
    begin
      if (DeliverMessage(Info^.LCLObject, KeyMsg) <> 0) or (KeyMsg.CharCode=VK_UNKNOWN) then
      begin
        // the LCL has handled the key
        {$IFDEF VerboseKeyboard}
        Debugln('[HandleRawKeyUpEvent] LCL handled CN_KEYUP, exiting');
        {$ENDIF}
        Result := noErr;
        exit;
      end;
      
      //Here is where we (interface) can do something with the key
      //Call the standard handler.
      Result := CallNextEventHandler(ANextHandler, AEvent);

      //Send a LM_(SYS)KEYUP
      if IsSysKey then KeyMsg.Msg := LM_SYSKEYUP
      else KeyMsg.Msg := LM_KEYUP;
      if DeliverMessage(Info^.LCLObject, KeyMsg) <> 0 then
      begin
        // the LCL handled the key
        {$IFDEF VerboseKeyboard}
        Debugln('[HandleRawKeyUpEvent] LCL handled LM_KEYUP, exiting');
        {$ENDIF}
        if Result=EventNotHandledErr then
          Result := noErr;
        exit;
      end;
    end;

  end;

var Root: ControlRef;
begin
  Result := EventNotHandledErr;
  //In compositing mode, this is the Content View.
  GetRootControl(AWindowInfo^.Widget, Root);
  Control := nil;
  GetKeyboardFocus(AWindowInfo^.Widget,Control);
  if Control = nil
  then Control := Root;

  // if a control other than root is found, send the message
  // to the control instead of the window
  // if a lower control without widgetInfo is found, use its parent
  Info := nil;
  while Control <> Root do
  begin
    Info := GetCtrlWidgetInfo(Pointer(Control));
    if Info <> nil then Break;
    Control := HIViewGetSuperview(Control);
  end;
  if (Info = nil) or (Control = Root)
  then Info := AWindowInfo;

  EventKind := GetEventKind(AEvent);
  if EventKind = kEventRawKeyModifiersChanged then
  begin
    if not EmulateModifiersDownUp then exit;
  end
  else
  if not TranslateMacKeyCode then
  begin
    Debugln('[CarbonPrivateWindow_KeyboardProc] ***WARNING: TranslateMacKeyCode failed***');
    exit;
  end;
  case EventKind of
    kEventRawKeyDown: Result:=HandleRawKeyDownEvent;
    kEventRawKeyRepeat: Result:=HandleRawKeyDownEvent;
    kEventRawKeyUp: Result:=HandleRawKeyUpEvent;
  end;

end;

//These two functions will never be called in compositing mode!

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
  KeySpecs: array[0..3] of EventTypeSpec;
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
  KeySpecs[3].eventClass := kEventClassKeyboard;
  KeySpecs[3].eventKind := kEventRawKeyModifiersChanged;

  InstallWindowEventHandler(AInfo^.Widget,
    RegisterEventHandler(@CarbonPrivateWindow_KeyboardProc),
    4, @KeySpecs[0], Pointer(AInfo), nil);

  debugln('TCarbonPrivateWindow.RegisterEvents ',DbgSName(AInfo^.LCLObject));

//In compositing mode these events are never sent!
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
  UnRegisterEventHandler(@CarbonPrivateWindow_KeyboardProc);
  
  UnRegisterEventHandler(@CarbonPrivateWindow_Update);
  UnRegisterEventHandler(@CarbonPrivateWindow_DrawContent);
end;
