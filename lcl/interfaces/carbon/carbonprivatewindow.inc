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

function CarbonWindow_Close(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Msg: TLMessage;
begin
  DebugLn('CarbonWindow_Close: ', DbgSName(AWidget.LCLObject));
  // Do canclose query, if false then exit

  FillChar(Msg, SizeOf(Msg),0);
  Msg.msg := LM_CLOSEQUERY;

  // Message results : 0 - do nothing, 1 - destroy window
  if DeliverMessage(AWidget.LCLObject, Msg) = 0 then
  begin
    Result := noErr;
    Exit;
  end;
  
  DebugLn('CarbonWindow_Close Free: ', DbgSName(AWidget.LCLObject));
  Result := CallNextEventHandler(ANextHandler, AEvent);
end;

{------------------------------------------------------------------------------
  Name: CarbonWindow_MouseProc
  Handles mouse events
 ------------------------------------------------------------------------------}
function CarbonWindow_MouseProc(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Control: ControlRef;    // the control we are dealing with
                          // or the rootcontrol if none found
  Widget: TCarbonWidget;  // the widget specific to the mouse event
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

  function GetMouseButton: Integer;
  // 1 = left, 2 = right, 3 = middle
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
    Result.X := Trunc(MousePoint.X);
    Result.Y := Trunc(MousePoint.Y);
  end;

  function GetMouseWheelDelta: Integer;
  var
    WheelDelta: SInt32;
  begin
    GetEventParameter(AEvent, kEventParamMouseWheelDelta, typeSInt32, nil,
                      SizeOf(WheelDelta), nil, @WheelDelta);
    Result := WheelDelta;
    {$IFDEF VerboseMouse}
    DebugLn('GetMouseWheelDelta WheelDelta=', DbgS(WheelDelta), ' ', HexStr(WheelDelta, 8));
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
  begin
    {$IFDEF VerboseMouse}
    DebugLN('HandleMouseDownEvent');
    {$ENDIF}
    Msg := @AMsg;

    ClickCount := GetClickCount;
    MouseButton := GetMouseButton;
    MousePoint := GetMousePoint;
    
    if (ClickCount < Low(MSGKIND)) or (ClickCount > High(MSGKIND)) then
      ClickCount := 1;
    
    if (MouseButton < Low(MSGKIND[1])) or (MouseButton > High(MSGKIND[1])) then Exit;
    Msg^.Msg := MSGKIND[ClickCount, MouseButton];
    //debugln('HandleMouseDownEvent CliCount=',dbgs(ClickCount),' MouseButton=',dbgs(MouseButton),' Msg^.Msg=',dbgs(Msg^.Msg));

    Msg^.XPos := MousePoint.X;                         
    Msg^.YPos := MousePoint.Y;
    Msg^.Keys := GetCarbonMsgKeyState;
  end;

  procedure HandleMouseUpEvent(var AMsg);
  const 
    MSGKIND: array[1..3] of Integer = (LM_LBUTTONUP, LM_RBUTTONUP, LM_MBUTTONUP);
  var
    MouseButton: Integer;
    MousePoint: TPoint;
    Msg: ^TLMMouse;
  begin
    DebugLn('HandleMouseUpEvent');
    {$IFDEF VerboseMouse}
    DebugLn('HandleMouseUpEvent');
    {$ENDIF}
    // this is not called if NextHandler is called on MouseDown
    // perhaps mousetracking can fix this
    Msg := @AMsg;

    MouseButton := GetMouseButton;
    MousePoint := GetMousePoint;
    
    if  (MouseButton >= Low(MSGKIND)) and (MouseButton <= High(MSGKIND)) then
      Msg^.Msg := MSGKIND[MouseButton];

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
    // TODO
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
  Result := EventNotHandledErr;
  
  //Find out which control the mouse event should occur for
  Control := nil;
  if HIViewGetViewForMouseEvent(AWidget.Content, AEvent, Control) <> noErr then Exit;
  if Control = nil then Exit;
  
  Widget := GetCarbonWidget(Control);
  if Widget = nil then Exit;

  FillChar(Msg, SizeOf(Msg), 0);
  
  EventKind := GetEventKind(AEvent);
  case EventKind of
    kEventMouseDown       : HandleMouseDownEvent(Msg);
    kEventMouseUp         : HandleMouseUpEvent(Msg);
    kEventMouseMoved,//      : HandleMouseMovedEvent(Msg);
    kEventMouseDragged    : HandleMouseMovedEvent(Msg);//HandleMouseDraggedEvent(Msg);

    // For the enter and exit events tracking must be enabled
    // tracking is enabled by defining a rect that you want to track
    // TODO: Tracking
    kEventMouseEntered    : Msg.Message.Msg := CM_MOUSEENTER;
    kEventMouseExited     : Msg.Message.Msg := CM_MOUSELEAVE;

    kEventMouseWheelMoved : HandleMouseWheelEvent(Msg);
  else
    Exit(EventNotHandledErr);
  end;
  
  // Msg is set in the Appropriate HandleMousexxx procedure
  if DeliverMessage(Widget.LCLObject, Msg) = 0 then
    Result := EventNotHandledErr //CallNextEventHandler(ANextHandler, AEvent);
  else
    // the LCL does not want the event propagated
    Result := noErr;
    
  NotifyApplicationUserInput(Msg.Message.Msg);
end;

function CarbonWindow_KeyboardProc(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Control: ControlRef;      // the control we are dealing with
                            // or the rootcontrol if none found
  Widget: TCarbonWidget;    // the widget specific to the mouse event
                            // or the window's widget if none found
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
    DebugLN('[HandleRawKeyDownEvent] Widget.LCLObject=',DbgSName(Widget.LCLObject));
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
      if (DeliverMessage(Widget.LCLObject, KeyMsg) <> 0) or (KeyMsg.CharCode=VK_UNKNOWN) then
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
      if (DeliverMessage(Widget.LCLObject, KeyMsg) <> 0) or (KeyMsg.CharCode=VK_UNKNOWN) then
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
      if TWinControl(Widget.LCLObject).IntfUTF8KeyPress(UTF8Character,1,IsSysKey) then
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
      if (DeliverMessage(Widget.LCLObject, CharMsg) <> 0) or (CharMsg.CharCode=VK_UNKNOWN) then
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
      if DeliverMessage(Widget.LCLObject, CharMsg) <> 0 then
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
    DebugLN('[HandleRawKeyUpEvent] Widget.LCLObject=',DbgSName(Widget.LCLObject));
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
      if (DeliverMessage(Widget.LCLObject, KeyMsg) <> 0) or (KeyMsg.CharCode=VK_UNKNOWN) then
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
      if DeliverMessage(Widget.LCLObject, KeyMsg) <> 0 then
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

begin
  Result := EventNotHandledErr;
  //In compositing mode, this is the content view
  Control := nil;
  GetKeyboardFocus(AWidget.Widget, Control);
  if Control = nil then Control := AWidget.Content;

  // if a control other than root is found, send the message
  // to the control instead of the window
  // if a lower control without widget is found, use its parent
  Widget := nil;
  while Control <> AWidget.Content do
  begin
    Widget := GetCarbonControl(Pointer(Control));
    if Widget <> nil then Break;
    Control := HIViewGetSuperview(Control);
  end;
  if (Widget = nil) or (Control = AWidget.Content)
  then Widget := AWidget;

  EventKind := GetEventKind(AEvent);
  if EventKind = kEventRawKeyModifiersChanged then
  begin
    if not EmulateModifiersDownUp then exit;
  end
  else
    if not TranslateMacKeyCode then
    begin
      Debugln('[CarbonWindow_KeyboardProc] ***WARNING: TranslateMacKeyCode failed***');
      exit;
    end;
  
  case EventKind of
    kEventRawKeyDown  : Result := HandleRawKeyDownEvent;
    kEventRawKeyRepeat: Result := HandleRawKeyDownEvent;
    kEventRawKeyUp    : Result := HandleRawKeyUpEvent;
  end;
end;

function CarbonWindow_ActivateProc(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Msg: TLMessage;
  EventKind: UInt32;
begin
  DebugLn('CarbonWindow_ActivateProc');
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  FillChar(Msg, SizeOf(Msg), 0);
  
  EventKind := GetEventKind(AEvent);
  case EventKind of
    kEventWindowActivated:   Msg.msg := LM_ACTIVATE;
    kEventWindowDeactivated: Msg.msg := LM_DEACTIVATE;
  else Exit;
  end;
  DeliverMessage(AWidget.LCLObject, Msg);
end;

function CarbonWindow_ShowWindow(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Msg: TLMShowWindow;
  EventKind: UInt32;
begin
  DebugLn('CarbonWindow_ShowWindow');
  Result := CallNextEventHandler(ANextHandler, AEvent);

  FillChar(Msg, SizeOf(Msg), 0);
  Msg.msg := LM_SHOWWINDOW;

  EventKind := GetEventKind(AEvent);
  case EventKind of
  kEventWindowCollapsed:
    begin
      Msg.Show := False;
      Msg.Status := SW_MINIMIZE;
    end;
  kEventWindowExpanded, kEventWindowZoomed:
    begin
      Msg.Show := True;
      if IsWindowInStandardState(WindowRef(AWidget.Widget), nil, nil) then
        Msg.Status := SW_SHOWNORMAL
      else
        Msg.Status := SW_SHOWMAXIMIZED;
    end;
  else Exit;
  end;
  
  DeliverMessage(AWidget.LCLObject, Msg);
end;

{ TCarbonWindow }

procedure TCarbonWindow.RegisterEvents;
var
  MouseSpec: array [0..6] of EventTypeSpec;
  TmpSpec: EventTypeSpec;
  KeySpecs: array[0..3] of EventTypeSpec;
  ActivateSpecs: array[0..1] of EventTypeSpec;
  ShowWindowSpecs: array[0..2] of EventTypeSpec;
begin
  TmpSpec := MakeEventSpec(kEventClassWindow, kEventWindowClose);
  InstallWindowEventHandler(Widget,
    RegisterEventHandler(@CarbonWindow_Close),
    1, @TmpSpec, Pointer(Self), nil);

  TmpSpec := MakeEventSpec(kEventClassWindow, kEventWindowClosed);
  InstallWindowEventHandler(Widget,
    RegisterEventHandler(@CarbonCommon_Dispose),
    1, @TmpSpec, Pointer(Self), nil);

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

  InstallWindowEventHandler(Widget,
    RegisterEventHandler(@CarbonWindow_MouseProc),
    7, @MouseSpec[0], Pointer(Self), nil);

  KeySpecs[0].eventClass := kEventClassKeyboard;
  KeySpecs[0].eventKind := kEventRawKeyDown;
  KeySpecs[1].eventClass := kEventClassKeyboard;
  KeySpecs[1].eventKind := kEventRawKeyRepeat;
  KeySpecs[2].eventClass := kEventClassKeyboard;
  KeySpecs[2].eventKind := kEventRawKeyUp;
  KeySpecs[3].eventClass := kEventClassKeyboard;
  KeySpecs[3].eventKind := kEventRawKeyModifiersChanged;

  InstallWindowEventHandler(Widget,
    RegisterEventHandler(@CarbonWindow_KeyboardProc),
    4, @KeySpecs[0], Pointer(Self), nil);

  ActivateSpecs[0].eventClass := kEventClassWindow;
  ActivateSpecs[0].eventKind := kEventWindowActivated;
  ActivateSpecs[1].eventClass := kEventClassWindow;
  ActivateSpecs[1].eventKind := kEventWindowDeactivated;

  InstallWindowEventHandler(Widget,
    RegisterEventHandler(@CarbonWindow_ActivateProc),
    2, @ActivateSpecs[0], Pointer(Self), nil);

  ShowWindowSpecs[0].eventClass := kEventClassWindow;
  ShowWindowSpecs[0].eventKind := kEventWindowCollapsed;
  ShowWindowSpecs[1].eventClass := kEventClassWindow;
  ShowWindowSpecs[1].eventKind := kEventWindowExpanded;
  ShowWindowSpecs[2].eventClass := kEventClassWindow;
  ShowWindowSpecs[2].eventKind := kEventWindowZoomed;

  InstallWindowEventHandler(Widget,
    RegisterEventHandler(@CarbonWindow_ShowWindow),
    3, @ShowWindowSpecs[0], Pointer(Self), nil);

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlDraw);
  InstallControlEventHandler(Content,
    RegisterEventHandler(@CarbonCommon_Draw),
    1, @TmpSpec, Pointer(Self), nil);
    
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlTrack);
  InstallControlEventHandler(Content,
    RegisterEventHandler(@CarbonCommon_Track),
    1, @TmpSpec, Pointer(Self), nil);

  TmpSpec := MakeEventSpec(kEventClassWindow, kEventWindowBoundsChanged);
  InstallWindowEventHandler(Widget,
    RegisterEventHandler(@CarbonCommon_BoundsChanged),
    1, @TmpSpec, Pointer(Self), nil);

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlContextualMenuClick);
  InstallControlEventHandler(Content,
    RegisterEventHandler(@CarbonCommon_ContextualMenuClick),
    1, @TmpSpec, Pointer(Self), nil);
    
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlSetFocusPart);
  InstallControlEventHandler(Content,
    RegisterEventHandler(@CarbonCommon_SetFocusPart),
    1, @TmpSpec, Pointer(Self), nil);

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlGetNextFocusCandidate);
  InstallControlEventHandler(Content,
    RegisterEventHandler(@CarbonCommon_GetNextFocusCandidate),
    1, @TmpSpec, Pointer(Self), nil);
    
  // cursor set
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlSetCursor);
  InstallControlEventHandler(Content,
    RegisterEventHandler(@CarbonCommon_SetCursor),
    1, @TmpSpec, Pointer(Self), nil);
	
  // cursor change
  TmpSpec := MakeEventSpec(kEventClassWindow, kEventWindowCursorChange);
  InstallWindowEventHandler(Widget,
    RegisterEventHandler(@CarbonCommon_CursorChange),
    1, @TmpSpec, Pointer(Self), nil);	

  DebugLn('TCarbonWindow.RegisterEvents ', DbgSName(LCLObject));
end;

procedure TCarbonWindow.UnregisterEvents;
begin
  UnregisterEventHandler(@CarbonWindow_Close);
  UnregisterEventHandler(@CarbonCommon_Dispose);

  UnregisterEventHandler(@CarbonWindow_MouseProc);
  UnregisterEventHandler(@CarbonWindow_KeyboardProc);
  UnregisterEventHandler(@CarbonWindow_ActivateProc);
  UnregisterEventHandler(@CarbonWindow_ShowWindow);

  UnregisterEventHandler(@CarbonCommon_Draw);
  UnregisterEventHandler(@CarbonCommon_Track);
  UnregisterEventHandler(@CarbonCommon_BoundsChanged);
  UnregisterEventHandler(@CarbonCommon_ContextualMenuClick);
  UnregisterEventHandler(@CarbonCommon_SetFocusPart);
  UnregisterEventHandler(@CarbonCommon_GetNextFocusCandidate);
  UnregisterEventHandler(@CarbonCommon_SetCursor);
  UnregisterEventHandler(@CarbonCommon_CursorChange);
end;

procedure TCarbonWindow.CreateWidget(const AParams: TCreateParams);
var
  Window: WindowRef;
  NewWindowClass: Integer;
  MinSize, MaxSize: HISize;
  Attributes: WindowAttributes;
begin
  // apply appropriate form border style and form style
  case (LCLObject as TCustomForm).FormStyle of
  fsStayOnTop, fsSplash: NewWindowClass := kUtilityWindowClass;
  else
    NewWindowClass := kDocumentWindowClass;
  end;

  case (LCLObject as TCustomForm).BorderStyle of
  bsNone:
    Attributes := kWindowNoTitleBarAttribute;
  bsToolWindow, bsSingle:
    Attributes := kWindowCloseBoxAttribute or kWindowCollapseBoxAttribute;
  bsSizeable:
    Attributes := kWindowCloseBoxAttribute or kWindowCollapseBoxAttribute
      or kWindowFullZoomAttribute or kWindowResizableAttribute;
  bsDialog:
    Attributes := kWindowCloseBoxAttribute;
  bsSizeToolWin:
    Attributes := kWindowCloseBoxAttribute or kWindowResizableAttribute;
  end;

  if CreateNewWindow(NewWindowClass,
    Attributes or kWindowCompositingAttribute or
    kWindowStandardHandlerAttribute or kWindowLiveResizeAttribute or
    kWindowInWindowMenuAttribute, ParamsToCarbonRect(AParams),
    Window) = noErr then
  begin
    Widget := Window;
    
    SetWindowProperty(Widget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self);
    SetControlProperty(Content, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self);
  end
  else RaiseCreateWidgetError(LCLObject);

  SetText(AParams.Caption);
  SetColor(LCLObject.Color);

  MinSize.width := LCLObject.Constraints.EffectiveMinWidth;
  MinSize.height := LCLObject.Constraints.EffectiveMinHeight;
  MaxSize.width := LCLObject.Constraints.EffectiveMaxWidth;
  MaxSize.height := LCLObject.Constraints.EffectiveMaxHeight;
  if MaxSize.width <= 0 then MaxSize.width := 10000;
  if MaxSize.height <= 0 then MaxSize.height := 10000;

  SetWindowResizeLimits(Window, @MinSize, @MaxSize);
end;

procedure TCarbonWindow.DestroyWidget;
begin
  RemoveWindowProperty(Widget, LAZARUS_FOURCC, WIDGETINFO_FOURCC);
  RemoveControlProperty(Content, LAZARUS_FOURCC, WIDGETINFO_FOURCC);
        
  DisposeWindow(WindowRef(Widget));
  Widget := nil;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWindow.GetContent
  Returns: Window content area control
 ------------------------------------------------------------------------------}
function TCarbonWindow.GetContent: ControlRef;
begin
  if HIViewFindByID(HIViewGetRoot(WindowRef(Widget)), kHIViewWindowContentID,
    Result) <> noErr then Result := nil;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWindow.GetMousePos
  Returns: The position of mouse cursor in local coordinates
 ------------------------------------------------------------------------------}
function TCarbonWindow.GetMousePos: TPoint;
var
  P: FPCMacOSAll.Point;
  R: FPCMacOSAll.Rect;
begin
  GetGlobalMouse(P);

  GetWindowBounds(WindowRef(Widget), kWindowContentRgn, R);
  Result.X := P.h - R.left;
  Result.Y := P.v - R.Top;
end;

function TCarbonWindow.GetTopParentWindow: WindowRef;
begin
  Result := WindowRef(Widget);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWindow.GetClientRect
  Params:  ARect - Record for client area coordinates
  Returns: If the function succeeds

  Returns the window client rectangle relative to the window frame origin
 ------------------------------------------------------------------------------}
function TCarbonWindow.GetClientRect(var ARect: TRect): Boolean;
var
  AWndRect, AClientRect: FPCMacOSAll.Rect;
begin
  Result := False;

  if FPCMacOSAll.GetWindowBounds(WindowRef(Widget), kWindowStructureRgn,
    AWndRect) <> noErr then Exit;
  if FPCMacOSAll.GetWindowBounds(WindowRef(Widget), kWindowContentRgn,
    AClientRect) <> noErr then Exit;

  ARect.Left := AClientRect.Left - AWndRect.Left;
  ARect.Top := AClientRect.Top - AWndRect.Top;
  ARect.Right := AClientRect.Right - AWndRect.Left;
  ARect.Bottom := AClientRect.Bottom - AWndRect.Top;

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWindow.Invalidate
  Params:  Rect - Pointer to rect (optional)
  Returns: Nothing

  Invalidates the specified rect or entire area of window content
 ------------------------------------------------------------------------------}
procedure TCarbonWindow.Invalidate(Rect: PRect);
begin
  if Rect = nil then HiViewSetNeedsDisplay(HIViewRef(Content), True)
  else
    HiViewSetNeedsDisplayInRect(HIViewRef(Content), RectToCGRect(Rect^), True);
end;

function TCarbonWindow.IsEnabled: Boolean;
begin
  Result := IsControlEnabled(Content);
end;

function TCarbonWindow.IsVisible: Boolean;
begin
  Result := FPCMacOSAll.IsWindowVisible(WindowRef(Widget));
end;

function TCarbonWindow.Enable(AEnable: Boolean): boolean;
begin
  Result := not FPCMacOSAll.IsControlEnabled(Content);

  // enable/disable window content
  // add/remove standard handler
  if AEnable then
  begin
    FPCMacOSAll.EnableControl(Content);
    ChangeWindowAttributes(WindowRef(Widget), kWindowStandardHandlerAttribute,
      kWindowNoAttributes);
  end
  else
  begin
    FPCMacOSAll.DisableControl(Content);
    ChangeWindowAttributes(WindowRef(Widget), kWindowNoAttributes,
      kWindowStandardHandlerAttribute);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWindow.GetBounds
  Params:  ARect         - Record for window coordinates
  Returns: If function succeeds

  Returns the window bounding rectangle relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCarbonWindow.GetBounds(var ARect: TRect): Boolean;
var
  AWndRect: FPCMacOSAll.Rect;
begin
  Result := FPCMacOSAll.GetWindowBounds(WindowRef(Widget), kWindowStructureRgn,
    AWndRect) = noErr;
  if Result then ARect := CarbonRectToRect(AWndRect);
end;

function TCarbonWindow.GetScreenBounds(var ARect: TRect): Boolean;
begin
  Result := GetBounds(ARect);
end;

function TCarbonWindow.SetBounds(const ARect: TRect): Boolean;
begin
  Result := FPCMacOSAll.SetWindowBounds(WindowRef(Widget), kWindowContentRgn,
    GetCarbonRect(ARect)) = noErr;
end;

procedure TCarbonWindow.SetColor(const AColor: TColor);
begin
   SetWindowContentColor(WindowRef(Widget), ColorToRGBColor(AColor));
end;

procedure TCarbonWindow.SetFont(const AFont: TFont);
begin
  //
end;

procedure TCarbonWindow.ShowHide(AVisible: Boolean);
begin
  if AVisible then
    FPCMacOSAll.ShowWindow(WindowRef(Widget))
  else
    FPCMacOSAll.HideWindow(WindowRef(Widget));
end;

function TCarbonWindow.GetText(var S: String): Boolean;
var
  CFString: CFStringRef;
begin
  Result := False;
  
  if CopyWindowTitleAsCFString(WindowRef(Widget), CFString) = NoErr then
  try
    S := CFStringToStr(CFString);
    Result := True;
  finally
    FreeCFString(CFString);
  end;
end;

function TCarbonWindow.SetText(const S: String): Boolean;
var
  CFString: CFStringRef;
begin
  Result := False;
  CreateCFString(S, CFString);
  try
    Result := SetWindowTitleWithCFString(WindowRef(Widget), CFString) = noErr;
  finally
    FreeCFString(CFString);
  end;
end;

function TCarbonWindow.Update: Boolean;
begin
  Result := HIViewRender(Content) = noErr;
end;
