{%MainUnit carbonprivate.pp}
{ $Id: $}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
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

  debugln('GetCarbonMsgKeyState Result=',dbgs(KeysToShiftState(Result)),' Modifiers=',hexstr(Modifiers,8),' ButtonState=',hexstr(ButtonState,8));
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

  debugln('GetCarbonShiftState Result=',dbgs(Result),' Modifiers=',hexstr(Modifiers,8),' ButtonState=',hexstr(ButtonState,8));
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
  debugln('TrackProgress');
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
  Window: WindowRef;
  R: FPCMacOSAll.Rect;
  Msg: TLMMouseMove;
begin                                   
  DebugLn('CarbonPrivateWindow_ControlTrack');
  GetEventParameter(AEvent, kEventParamWindowMouseLocation, typeHIPoint, nil, SizeOf(MousePoint), nil, @MousePoint);
  GetEventParameter(AEvent, kEventParamControlAction, typeControlActionUPP, nil, sizeof(ActionUPP), nil, @OldActionUPP);
  GetEventParameter(AEvent, kEventParamMouseButton, typeMouseButton, nil, SizeOf(MouseButton), nil, @MouseButton);

  ActionUPP := NewControlActionUPP(@TrackProgress);   
  SetEventParameter(AEvent, kEventParamControlAction, typeControlActionUPP, sizeof(ActionUPP), @ActionUPP);

  // this does not return until the mouse is released
  Result := CallNextEventHandler(ANextHandler, AEvent);
                                                   
  SetEventParameter(AEvent, kEventParamControlAction, typeControlActionUPP, sizeof(OldActionUPP), @OldActionUPP);
  DisposeControlActionUPP(ActionUPP);
  
  // now we will create a MouseUp message to send the LCL
  Control := ControlRef(AInfo^.Widget);
  GetGlobalMouse(Pt);
  Window := HIViewGetWindow(Control);
  GetWindowBounds(Window, kWindowStructureRgn, R);
  MousePoint.X := pt.h - R.left;
  MousePoint.Y := pt.v - R.Top;

  HIViewConvertPoint(MousePoint, nil, Control);

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
    MousePoint: HIPoint;//QDPoint;
  begin
    GetEventParameter(AEvent, kEventParamWindowMouseLocation, typeHIPoint, nil,
                      SizeOf(MousePoint), nil, @MousePoint);
    HIViewConvertPoint(MousePoint, nil, Control);
    Result.X := Round(MousePoint.X);
    Result.Y := Round(MousePoint.Y);
    // WriteLn('Mouse to Widget Coords: X=',Result.X,' Y=',Result.Y);
  end;

  function GetMouseWheelDelta: Integer;
  var
    WheelDelta: SInt32;
  begin
    GetEventParameter(AEvent, kEventParamMouseWheelDelta, typeSInt32, nil,
                      SizeOf(WheelDelta), nil, @WheelDelta);
    Result := WheelDelta;
    debugln('GetMouseWheelDelta WheelDelta=',dbgs(WheelDelta),' ',hexstr(WheelDelta,8));
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
    DebugLN('-- mouse down --');
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
    DebugLN('-- mouse up --');
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
    DebugLN('HandleMouseMovedEvent');
    Msg := @AMsg;
    
    MousePoint := GetMousePoint;
    
    Msg^.Msg := LM_MOUSEMOVE;
    Msg^.XPos := MousePoint.X;
    Msg^.YPos := MousePoint.Y;
    Msg^.Keys := GetCarbonMsgKeyState;
  end;

  procedure HandleMouseDraggedEvent(var AMsg);
  begin
    DebugLN('-- mouse dragged --');
    //TODO
  end;

  procedure HandleMouseWheelEvent(var AMsg);
  var
    MousePoint: TPoint;
    MSg: ^TLMMouseEvent;
  begin
    DebugLN('HandleMouseWheelEvent');
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
  Root: ControlRef;
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
    0:
      case KeyChar of
      'a'..'z': Result:=VK_A+ord(KeyChar)-ord('a');
      'A'..'Z': Result:=VK_A+ord(KeyChar)-ord('A');
      '+': Result:=VK_ADD;
      end;
    end;

    GetEventParameter(AEvent, kEventParamKeyMacCharCodes, typeChar, nil,
                      SizeOf(KeyChar), nil, @KeyChar);
    GetEventParameter(AEvent, kEventParamKeyUnicodes, typeUnicodeText, nil,
                      6, @TextLen, @Buf[1]);
    debugln('GetVKKeyCode Result=',dbgs(Result),' KeyCode='+dbgs(KeyCode),' KeyChar='+DbgStr(KeyChar),' TextLen='+dbgs(TextLen),
      ' '+dbgs(Buf[1])+','+dbgs(Buf[2])+','+dbgs(Buf[3])+','+dbgs(Buf[4])+','+dbgs(Buf[5])+','+dbgs(Buf[6]));
  end;
  
  function GetCharacterCode: word;
  var
    KeyChar: char;
    Buf: array[1..6] of byte;
    TextLen: UInt32;
  begin
    KeyChar:=#0;
    FillChar(Buf,SizeOf(Buf),0);

    GetEventParameter(AEvent, kEventParamKeyMacCharCodes, typeChar, nil,
                      SizeOf(KeyChar), nil, @KeyChar);
    GetEventParameter(AEvent, kEventParamKeyUnicodes, typeUnicodeText, nil,
                      6, @TextLen, @Buf[1]);
    Result:=ord(KeyChar);
    debugln('GetCharacterCode Result=',dbgs(Result),' KeyChar='+DbgStr(KeyChar),
      ' '+dbgs(Buf[1])+','+dbgs(Buf[2])+','+dbgs(Buf[3])+','+dbgs(Buf[4])+','+dbgs(Buf[5])+','+dbgs(Buf[6]));
  end;

  procedure HandleRawKeyDownEvent;
  var
    KeyMsg: TLMKeyDown;
    CharMsg: TLMChar;
  begin
    DebugLN('HandleRawKeyDownEvent Info^.LCLObject=',DbgSName(Info^.LCLObject));
    
    // create the LM_KEYDOWN message
    FillChar(KeyMsg, SizeOf(KeyMsg), 0);
    KeyMsg.Msg := LM_KEYDOWN;
    KeyMsg.KeyData := GetCarbonMsgKeyState;
    KeyMsg.CharCode := GetVKKeyCode;
    
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


    // create the LM_CHAR / LM_SYSCHAR message
    FillChar(CharMsg, SizeOf(CharMsg), 0);
    if (GetCurrentKeyModifiers and optionKey)>0 then
      CharMsg.Msg := LM_SYSCHAR
    else
      CharMsg.Msg := LM_CHAR;
    CharMsg.KeyData := GetCarbonMsgKeyState;
    CharMsg.CharCode := GetCharacterCode;

    // Msg is set in the Appropriate HandleKeyxxx procedure
    if DeliverMessage(Info^.LCLObject, CharMsg) = 0 then begin
      Result :=  EventNotHandledErr;
    end
    else begin
      // the LCL does not want the event propagated
      Result := noErr;
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

// ==================================================================
// C L A S S 
// ==================================================================

  { TCarbonPrivateWindow }

procedure TCarbonPrivateWindow.RegisterEvents(AInfo: PWidgetInfo);
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

  InstallWindowEventHandler(AInfo^.Widget,
    RegisterEventHandler(@CarbonPrivateWindow_KeyboardProc),
    1, @KeySpecs[0], Pointer(AInfo), nil);
end;

procedure TCarbonPrivateWindow.UnregisterEvents;
begin                      
  UnregisterEventHandler(@CarbonPrivateWindow_Close);
  UnregisterEventHandler(@CarbonPrivateWindow_Closed); 
  UnregisterEventHandler(@CarbonPrivateWindow_MouseProc);
end;

