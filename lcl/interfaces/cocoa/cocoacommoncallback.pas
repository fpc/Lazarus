unit CocoaCommonCallback;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$include cocoadefines.inc}

interface

uses
  Types, Classes, Controls, SysUtils, Math,
  LCLType, LCLMessageGlue, LMessages, LCLProc, LCLIntf, Graphics, Forms,
  CocoaAll,
  CocoaPrivate, CocoaWSService, CocoaWSModalService,
  CocoaWindows,  Cocoa_Extra, CocoaConfig, CocoaUtils,
  CocoaGDIObjects, CocoaCursor, CocoaCaret;

type

  {$scopedEnums on}
  TCocoaCbState = (
    boundsReported,
    mouseRouting
  );

  TLCLCommonCallbackStates = set of TCocoaCbState;

  TCocoaCbTrait = (
    blockUpDown,
    treatTabAsChar,  // all tabs should be suppressed, so Cocoa would not switch focus
    forceSendReturn, // send keyDown/LM_KEYDOWN for Return even if handled by IntfUTF8KeyPress/CN_CHAR
    opaque
  );

  TLCLCommonCallbackTraits = set of TCocoaCbTrait;

  { TLCLCommonCallback }

  TLCLCommonCallback = class(TObject, ICommonCallBack)
  protected
    _owner: NSObject;
    _target: TWinControl;
    _propStorage: TStringList;
    _context: TCocoaContext;
    _handleFrame: NSView; // HWND and "frame" (rectangle) of the a control
    _states: TLCLCommonCallbackStates;
  public
    traits: TLCLCommonCallbackTraits;
    property owner: NSObject read _owner;
    property target: TWinControl read _target;

  protected
    function deliverMessage(var msg): LRESULT;
    function doSendKeyMessage(
      const msg: Cardinal;
      var charCode: Word;
      const keyData: PtrInt;
      const notifyUserInput: Boolean ): PtrInt;
    procedure send_UTF8KeyPress();
    procedure send_CN_CHAR_Message();
    procedure send_LM_KEYDOWN_Message();
    procedure send_LM_CHAR_Message();

    procedure KeyEvBeforeDown;
    procedure KeyEvBeforeUp;
    procedure KeyEvAfterUp;
    procedure KeyEvAfterDown;
    function KeyEvBefore(const Event: NSEvent): Boolean;
    procedure KeyEvAfter;

    function getCaptureControlCallback: ICommonCallBack;
    procedure sendContextMenu(Event: NSEvent; out ContextMenuHandled: Boolean);

    procedure OffsetMousePos(LocInWin: NSPoint; out PtInBounds, PtInClient, PtForChildCtrls: TPoint );
    procedure ScreenMousePos(var Point: NSPoint);

  protected
    procedure createKeyStateFromKeyUpDown(
      const event: NSEvent;
      var state: TCocoaKeyEventState ); virtual;
    procedure createKeyStateFromFlagsChanged(
      const event: NSEvent;
      var state: TCocoaKeyEventState ); virtual;

  public
    constructor Create(AOwner: NSObject; ATarget: TWinControl; AHandleFrame: NSView = nil); virtual;
    destructor Destroy; override;
    function GetPropStorage: TStringList; inline;
    function GetContext: HDC; inline;
    function GetTarget: TObject; inline;
    function GetCallbackObject: TObject; inline;
    procedure RemoveTarget; inline;
    function HandleFrame: NSView; inline;
    procedure SetHandleFrame(const AHandleFrame: NSView ); inline;
  public
    function handleEventBeforeCocoa( const theEvent: NSEvent ): Boolean; virtual;
    procedure handleEventAfterCocoa( const theEvent: NSEvent ); virtual;

    function MouseUpDownEvent(
      const Event: NSEvent;
      const AForceAsMouseUp: Boolean = False;
      const AOverrideBlock: Boolean = False): Boolean; virtual;
    function MouseMove(const Event: NSEvent): Boolean; virtual;
    function scrollWheel(const Event: NSEvent): Boolean; virtual;
    procedure scroll(
      const isVert: Boolean;
      const Pos: Integer;
      const AScrollPart: NSScrollerPart); virtual;

    procedure frameDidChange(const sender: id); virtual;
    procedure boundsDidChange(const sender: id); virtual;

    procedure Draw(const ControlContext: NSGraphicsContext; const bounds, dirty: NSRect); virtual;
    procedure DrawOverlay(const ControlContext: NSGraphicsContext; const bounds, dirty: NSRect); virtual;
  end;

  TLCLCommonCallBackClass = class of TLCLCommonCallBack;

implementation

var
  LastMouse: TLastMouseInfo;
  LastMouseLeftButtonAsRight: Boolean;

function CocoaModifiersToShiftState(
  const AModifiers: NSUInteger;
  const AMouseButtons: NSUInteger): TShiftState;
begin
  Result := [];
  if AModifiers and NSShiftKeyMask <> 0 then Include(Result, ssShift);
  if AModifiers and NSControlKeyMask <> 0 then Include(Result, ssCtrl);
  if AModifiers and NSAlternateKeyMask <> 0 then Include(Result, ssAlt);
  if AModifiers and NSCommandKeyMask <> 0 then Include(Result, ssMeta);

  if AMouseButtons and (1 shl 0) <> 0 then Include(Result, ssLeft);
  if AMouseButtons and (1 shl 1) <> 0 then Include(Result, ssRight);
  if AMouseButtons and (1 shl 2) <> 0 then Include(Result, ssMiddle);
  if AMouseButtons and (1 shl 3) <> 0 then Include(Result, ssExtra1);
  if AMouseButtons and (1 shl 4) <> 0 then Include(Result, ssExtra2);
end;

function CocoaModifiersToKeyState(AModifiers: NSUInteger): PtrInt;
begin
  Result := 0;
  if AModifiers and NSShiftKeyMask <> 0 then
    Result := Result or MK_SHIFT;
  if AModifiers and NSControlKeyMask <> 0 then
    Result := Result or MK_CONTROL;
  if AModifiers and NSAlternateKeyMask <> 0 then
    Result := Result or MK_ALT;
end;

function CocoaPressedMouseButtonsToKeyState(AMouseButtons: NSUInteger): PtrInt;
begin
  Result := 0;
  if AMouseButtons and (1 shl 0) <> 0 then
    Result := Result or MK_LBUTTON;
  if AMouseButtons and (1 shl 1) <> 0 then
    Result := Result or MK_RBUTTON;
  if AMouseButtons and (1 shl 2) <> 0 then
    Result := Result or MK_MBUTTON;
  if AMouseButtons and (1 shl 3) <> 0 then
    Result := Result or MK_XBUTTON1;
  if AMouseButtons and (1 shl 4) <> 0 then
    Result := Result or MK_XBUTTON2;
end;

function isValidMouseControl( control:TWinControl ): Boolean;
begin
  if (control is TCustomForm) and (csDesigning in control.ComponentState) then
    exit( True );
  Result:= control.Visible and control.Enabled;
end;

function isContextMenuEvent(event: NSEvent): Boolean;
begin
  Result := Assigned(event)
    and (
      (Event.type_ = NSRightMouseUp)
      or(
        (Event.type_ = NSLeftMouseUp)
        and (event.modifierFlags and NSControlKeyMask <> 0)
        and (event.clickCount = 1)
      )
    );
end;

{ TLCLCommonCallback }

procedure TLCLCommonCallback.OffsetMousePos(LocInWin: NSPoint; out PtInBounds, PtInClient, PtForChildCtrls: TPoint);
var
  lView: NSView;
  pt: NSPoint;
  cr: TRect;
begin
  if _owner.isKindOfClass(NSWindow) then
  begin
    PtInBounds.x := Round(LocInWin.x);
    PtInBounds.y := Round(NSWindow(_owner).contentView.bounds.size.height - LocInWin.y);
    PtInClient := PtInBounds; // todo: it's different. But Owner is never NSWindow (it's TConentWindowView instead)
    PtForChildCtrls := PtInClient;
  end
  else if _owner.isKindOfClass(NSView) then
  begin
    pt := LocInWin;

    NSView(_owner).lclOffsetMousePos(pt);
    PtInBounds.x := Round(pt.x);
    PtInBounds.y := Round(pt.y);

    //pt := NSView(_owner).frame.origin;
    //if NSView(_owner).frame.
    cr := NSView(_owner).lclClientFrame;
    PtInClient.x := Round({PtInBounds.x - }pt.x - cr.Left);
    PtInClient.y := Round({PtInBounds.y - }pt.y - cr.Top);

    // child ctrls need not LayoutDelta
    cr := NSView(_owner).lclGetFrameToLayoutDelta;
    PtForChildCtrls := PtInClient;
    PtForChildCtrls.x := PtForChildCtrls.x + cr.Left;
    PtForChildCtrls.y := PtForChildCtrls.y + cr.Top;

    if target is TScrollingWinControl then
      TCocoaViewUtil.lclOffsetWithEnclosingScrollView(NSView(_owner), PtForChildCtrls.x, PtForChildCtrls.y);

  end else
  begin
    PtInBounds.x := Round(LocInWin.x);
    PtInBounds.y := Round(LocInWin.y);
    PtInClient := PtInBounds;
    PtForChildCtrls := PtInClient;
  end;
end;

procedure TLCLCommonCallback.ScreenMousePos(var Point: NSPoint);
var
  f: NSRect;
  lWindow: NSWindow;
begin
  lWindow := NSWindow(TCocoaWindowUtil.getWindow(_owner));
  if lWindow <> nil then
  begin
    f := lWindow.frame;
    Point.x := Point.x + f.origin.x;
    Point.y := TCocoaScreenUtil.globalScreenBottom - ( Point.y + f.origin.y );
  end;
end;

procedure TLCLCommonCallback.createKeyStateFromKeyUpDown(
  const event: NSEvent;
  var state: TCocoaKeyEventState );
var
  KeyCode: word;
  UTF8Character: TUTF8Char;   // char to send via IntfUtf8KeyPress
  KeyChar : char;          // Ascii char, when possible (xx_(SYS)CHAR)
  SendChar: boolean;       // Should we send char?
  VKKeyCode: word;         // VK_ code
  IsSysKey: Boolean;       // Is alt (option) key down?
  KeyData: PtrInt;         // Modifiers (ctrl, alt, mouse buttons...)
  ignModChr: NSString;
begin
  SendChar := False;

  UTF8Character := '';
  KeyChar := #0;

  IsSysKey := (Event.modifierFlags and NSCommandKeyMask) <> 0;
  KeyData := (Ord(Event.isARepeat) + 1) or Event.keyCode shl 16;
  if (Event.modifierFlags and NSAlternateKeyMask) <> 0 then
    KeyData := KeyData or MK_ALT;   // So that MsgKeyDataToShiftState recognizes Alt key, see bug 30129
  KeyCode := Event.keyCode;

  ignModChr := Event.charactersIgnoringModifiers;
  if Assigned(ignModChr)
    and (ignModChr.length=1)
    and ((Event.modifierFlags and NSNumericPadKeyMask) = 0) // num pad should be checked by KeyCode
  then
  begin
    VKKeyCode := TCocoaKeyUtil.charToVK(ignModChr.characterAtIndex(0));
    if VKKeyCode = VK_UNKNOWN then
      VKKeyCode := TCocoaKeyUtil.codeToVK(KeyCode); // fallback
  end
  else
    VKKeyCode := TCocoaKeyUtil.codeToVK(KeyCode);

  if Assigned(CocoaConfigApplication.events.keyEventToVK) then
    VKKeyCode := CocoaConfigApplication.events.keyEventToVK(Event);

  case VKKeyCode of
    // for sure, these are "non-printable" keys (see http://wiki.lazarus.freepascal.org/LCL_Key_Handling)
    VK_F1..VK_F24,                     // Function keys (F1-F12)
    VK_PRINT, VK_SCROLL, VK_PAUSE,     // Print Screen, Scroll Lock, Pause
    VK_CAPITAL, VK_TAB,                // Caps Lock, Tab
    VK_INSERT, VK_DELETE,              // Insert,  Delete
    VK_HOME, VK_END,                   // Home, End
    VK_PRIOR,VK_NEXT,                  // Page Up,Down
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, // Arrow Keys
    VK_NUMLOCK,                        // Num Lock
    VK_SLEEP, VK_APPS  // power/sleep, context menu
    :
      SendChar := false;

    // for sure, these are "printable" keys
    VK_ESCAPE,
    VK_BACK,
    VK_RETURN:
    begin
      SendChar := true;
      KeyChar := char(VKKeyCode);
      UTF8Character := KeyChar;
    end;
  else
    //printable keys
    //for these keys, send char or UTF8KeyPress
    UTF8Character := NSStringToString(Event.characters);

    if Length(UTF8Character) > 0 then
    begin
      SendChar := True;
      if Length(UTF8Character)=1 then
        // ANSI layout character
        KeyChar := Utf8Character[1]
      else
        // it's non ANSI character. KeyChar must be assinged anything but #0
        // otherise the message could be surpressed.
        // In Windows world this would be an "Ansi" char in current locale
        KeyChar := '?';
    end;
  end;

  CocoaWidgetSetState.keyEvent.keyCode:= VKKeyCode;
  CocoaWidgetSetState.keyEvent.charCode:= Word(KeyChar);
  CocoaWidgetSetState.keyEvent.keyData:= KeyData;
  CocoaWidgetSetState.keyEvent.utf8Character:= UTF8Character;
  CocoaWidgetSetState.keyEvent.isKeyDown:= (Event.type_ = NSKeyDown);
  CocoaWidgetSetState.keyEvent.isSysKey:= IsSysKey;
  CocoaWidgetSetState.keyEvent.shouldSendCharMessage:= SendChar;
end;

procedure TLCLCommonCallback.createKeyStateFromFlagsChanged(
  const event: NSEvent;
  var state: TCocoaKeyEventState );
const
  cModifiersOfInterest: NSUInteger = (NSControlKeyMask or NSShiftKeyMask or NSAlphaShiftKeyMask or NSAlternateKeyMask or NSCommandKeyMask);
var
  CurMod, Diff: NSUInteger;
  VKKeyCode: word; // VK_ code
  KeyData: PtrInt; // Modifiers (ctrl, alt, mouse buttons...)
begin
  CocoaWidgetSetState.keyEvent.shouldSendCharMessage:= False;
  CurMod := Event.modifierFlags;
  //see what changed. we only care of bits 16 through 20
  Diff := (CocoaWidgetSetState.keyEvent.prevModifiers xor CurMod) and cModifiersOfInterest;

  case Diff of
    0                  : VKKeyCode := VK_UNKNOWN; //nothing (that we cared of) changed
    NSControlKeyMask   : VKKeyCode := VK_CONTROL; //command mapped to control
    NSShiftKeyMask     : VKKeyCode := VK_SHIFT;
    NSAlphaShiftKeyMask: VKKeyCode := VK_CAPITAL; //caps lock
    NSAlternateKeyMask : VKKeyCode := VK_MENU;    //option is alt
    NSCommandKeyMask   : VKKeyCode := VK_LWIN;    //meta... map to left Windows Key?
  end;
  KeyData:= CocoaModifiersToKeyState(CurMod);

  //diff is now equal to the mask of the bit that changed, so we can determine
  //if this change is a keydown (PrevKeyModifiers didn't have the bit set) or
  //a keyup (PrevKeyModifiers had the bit set)
  CocoaWidgetSetState.keyEvent.isKeyDown := ((CocoaWidgetSetState.keyEvent.prevModifiers and Diff) = 0);
  CocoaWidgetSetState.keyEvent.keyData := KeyData;
  CocoaWidgetSetState.keyEvent.keyCode := VKKeyCode;
  CocoaWidgetSetState.keyEvent.isSysKey := (VKKeyCode = VK_LWIN);
end;

constructor TLCLCommonCallback.Create(AOwner: NSObject; ATarget: TWinControl; AHandleFrame: NSView);
begin
  inherited Create;
  _owner := AOwner;
  if Assigned(AHandleFrame) then
    _handleFrame := AHandleFrame
  else if _owner.isKindOfClass(NSView) then
    _handleFrame := NSView(AOwner);
  _target := ATarget;
  _context := nil;
  _propStorage := TStringList.Create;
  _propStorage.Sorted := True;
  _propStorage.Duplicates := dupAccept;
end;

destructor TLCLCommonCallback.Destroy;
begin
  _context.Free;
  _propStorage.Free;
  _target := nil;
  inherited Destroy;
end;

function TLCLCommonCallback.GetPropStorage: TStringList;
begin
  Result := _propStorage;
end;

function TLCLCommonCallback.GetContext: HDC;
begin
  Result := HDC( _context );
end;

function TLCLCommonCallback.GetTarget: TObject;
begin
  Result := target;
end;

function TLCLCommonCallback.GetCallbackObject: TObject;
begin
  Result := Self;
end;

function TLCLCommonCallback.handleEventBeforeCocoa(const theEvent: NSEvent
  ): Boolean;
begin
  Result:= True;

  case theEvent.type_ of
    NSKeyDown,
    NSKeyUp,
    NSFlagsChanged: ;
    else
      Exit;
  end;

  // in IME state
  if CocoaWidgetSetState.CocoaOnlyState then
    Exit;

  // not in IME state
  Result:= self.KeyEvBefore(theEvent);
end;

procedure TLCLCommonCallback.handleEventAfterCocoa(const theEvent: NSEvent);

  procedure forwardMouseMovedEventToTheWindowAtPos( const windowAtPos: NSWindow; const originalEvent: NSEvent );
  var
    location: NSPoint;
    newEvent: NSEvent;
  begin
    location:= originalEvent.mouseLocation;
    location.x := location.x - windowAtPos.frame.origin.x;
    location.y := location.y - windowAtPos.frame.origin.y;
    newEvent := NSEvent.mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressure(
      originalEvent.type_,
      location,
      originalEvent.modifierFlags,
      originalEvent.timestamp,
      windowAtPos.windowNumber,
      originalEvent.context,
      originalEvent.eventNumber,
      originalEvent.clickCount,
      originalEvent.pressure
    );
    windowAtPos.sendEvent( newEvent );
  end;

  procedure handleKeyEventAfterCocoa;
  begin
    // if in IME state, pass KeyEvAfter
    if NOT CocoaWidgetSetState.CocoaOnlyState then
      self.KeyEvAfter;
  end;

  procedure handleMouseMovedEventAfterCocoa;
  var
    mousePos: NSPoint;
    windowAtPos: NSWindow;
    windowClientFrame: NSRect;
  begin
    if NOT NSAPP.isActive then
      Exit;

    mousePos:= theEvent.mouseLocation;
    windowAtPos:= TCocoaWindowUtil.getWindowAtPos( mousePos );;
    if NOT Assigned(windowAtPos) then begin
      Application.DoBeforeMouseMessage( nil );
      Exit;
    end;

    windowClientFrame := windowAtPos.contentRectForFrameRect(windowAtPos.frame);
    // if mouse outside of ClientFrame of Window,
    // Cursor should be forced to default.
    // see also: https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/40515
    if not NSPointInRect(mousePos, windowClientFrame) then begin
      if Screen.Cursor=crDefault then
        CursorHelper.ForceSetDefaultCursor
      else
        CursorHelper.SetScreenCursor;
      Application.DoBeforeMouseMessage( nil );
    end;

    // mouse in the keyWindow, complete, Exit
    if (NOT Assigned(theEvent.window)) or (windowAtPos=theEvent.window) then
      Exit;

    // mouse NOT in the keyWindow, forward the Mouse Moved Event to
    // the Window at Mouse Cursor Pos, according the LCL specification
    forwardMouseMovedEventToTheWindowAtPos( windowAtPos, theEvent );
  end;

begin
  case theEvent.type_ of
    NSKeyDown,
    NSKeyUp,
    NSFlagsChanged:
      handleKeyEventAfterCocoa;
    NSMouseMoved:
      handleMouseMovedEventAfterCocoa;
  end;
end;

function TLCLCommonCallback.getCaptureControlCallback: ICommonCallBack;
var
  obj: NSObject;
  lCaptureView: NSView;
begin
  Result := nil;
  if CocoaWidgetSetState.lclCaptureControl = 0 then Exit;
  obj := NSObject(CocoaWidgetSetState.lclCaptureControl);
  lCaptureView := obj.lclContentView;
  if obj = _owner then
    Exit;
  if lCaptureView = _owner then
    Exit;
  if TCocoaCbState.mouseRouting in _states then
    Exit;
  Result:= lCaptureView.lclGetCallback;
end;

{ If a window does not display a shortcut menu it should pass
  this message to the DefWindowProc function. If a window is
  a child window, DefWindowProc sends the message to the parent. }
procedure TLCLCommonCallback.sendContextMenu(Event: NSEvent; out
  ContextMenuHandled: Boolean);
var
  MsgContext: TLMContextMenu;
  MousePos : NSPoint;
  Res: PtrInt;
  Rcp : NSObject;
  Trg : TObject;
  cb    : ICommonCallback;
  obj   : TObject;
  cbobj : TLCLCommonCallback;
  ed : NSText;
begin
  ContextMenuHandled := false;
  FillChar(MsgContext, SizeOf(MsgContext), #0);
  MsgContext.Msg := LM_CONTEXTMENU;
  MsgContext.hWnd := HWND(_handleFrame);
  MousePos := Event.locationInWindow;
  ScreenMousePos(MousePos);
  MsgContext.XPos := Round(MousePos.X);
  MsgContext.YPos := Round(MousePos.Y);
  Rcp := _owner;
  Res := 1;
  repeat
    cb := Rcp.lclGetCallback;
    if Assigned(cb) then
    begin
      Trg := cb.GetTarget;
      Res := LCLMessageGlue.DeliverMessage(Trg, MsgContext);
      if (Res = 0) and (Rcp.isKindOfClass(NSView)) then
      begin
        if Assigned(NSView(Rcp).menuForEvent(Event)) then
          Break; // Cocoa has it's own menu for the control

        if Rcp.isKindOfClass(NSControl) then
        begin
          ed := NSControl(Rcp).currentEditor;
          if Assigned(ed) and Assigned(ed.menuForEvent(Event)) then
            Break; // Cocoa has it's own menu for the editor of the control
        end;
      end;

      // not processed, need to find parent
      if Res = 0 then
      begin
        cbobj := nil;
        if Assigned(cb) then
        begin
          obj := cb.GetCallbackObject;
          if obj is TLCLCommonCallback then cbobj := TLCLCommonCallback(obj);
        end;
        if not Assigned(cbobj) then
          Rcp := nil
        else
          Rcp := cbobj.HandleFrame.superView;
      end;
    end else
      Rcp := nil;
  until (Res <> 0) or not Assigned(Rcp);
  ContextMenuHandled := Res <> 0;
end;

procedure TLCLCommonCallback.KeyEvBeforeDown;
var
  msg: Cardinal;
  ret: PtrInt;
begin
  if CocoaWidgetSetState.keyEvent.keyCode = VK_UNKNOWN then
    Exit;

  if CocoaWidgetSetState.keyEvent.isSysKey then
    msg := CN_SYSKEYDOWN
  else
    msg := CN_KEYDOWN;

  ret:= self.doSendKeyMessage( msg, CocoaWidgetSetState.keyEvent.keyCode, CocoaWidgetSetState.keyEvent.keyData, True );
  if (ret<>0) or (CocoaWidgetSetState.keyEvent.keyCode=VK_UNKNOWN) then
    CocoaWidgetSetState.keyEvent.handled:= True;
end;

procedure TLCLCommonCallback.KeyEvBeforeUp;
var
  msg: Cardinal;
  ret: PtrInt;
begin
  if CocoaWidgetSetState.keyEvent.keyCode = VK_UNKNOWN then
    Exit;

  if CocoaWidgetSetState.keyEvent.isSysKey then
    msg := CN_SYSKEYUP
  else
    msg := CN_KEYUP;

  ret:= self.doSendKeyMessage( msg, CocoaWidgetSetState.keyEvent.keyCode, CocoaWidgetSetState.keyEvent.keyData, True );
  if (ret<>0) or (CocoaWidgetSetState.keyEvent.keyCode=VK_UNKNOWN) then
    CocoaWidgetSetState.keyEvent.handled:= True;
end;

procedure TLCLCommonCallback.send_UTF8KeyPress();
var
  lclHandled: Boolean;
begin
  if not CocoaWidgetSetState.keyEvent.shouldSendCharMessage then
    Exit;

  lclHandled:= TCocoaLCLMessageUtil.IntfUTF8KeyPress(
                 _owner,
                 CocoaWidgetSetState.keyEvent.utf8Character,
                 CocoaWidgetSetState.keyEvent.isSysKey );

  if lclHandled then begin
    if (TCocoaCbTrait.forceSendReturn in self.traits) and (CocoaWidgetSetState.keyEvent.keyCode=VK_RETURN) then
      CocoaWidgetSetState.keyEvent.shouldSendCharMessage:= False
    else
      CocoaWidgetSetState.keyEvent.handled:= True;
  end;
end;

procedure TLCLCommonCallback.send_CN_CHAR_Message();
var
  msg: Cardinal;
  ret: PtrInt;
begin
  if NOT CocoaWidgetSetState.keyEvent.shouldSendCharMessage then
    Exit;

  if CocoaWidgetSetState.keyEvent.isSysKey then
    msg := CN_SYSCHAR
  else
    msg := CN_CHAR;

  ret:= self.doSendKeyMessage( msg, CocoaWidgetSetState.keyEvent.charCode, CocoaWidgetSetState.keyEvent.keyData, False );
  if (ret<>0) or (CocoaWidgetSetState.keyEvent.charCode=VK_UNKNOWN) then
    CocoaWidgetSetState.keyEvent.handled:= True;
end;

procedure TLCLCommonCallback.send_LM_KEYDOWN_Message();
var
  msg: Cardinal;
  ret: PtrInt;
begin
  if CocoaWidgetSetState.keyEvent.keyCode = VK_UNKNOWN then
    Exit;

  if CocoaWidgetSetState.keyEvent.isSysKey then
    msg := LM_SYSKEYDOWN
  else
    msg := LM_KEYDOWN;

  ret:= self.doSendKeyMessage( msg, CocoaWidgetSetState.keyEvent.keyCode, CocoaWidgetSetState.keyEvent.keyData, False );
  if (ret<>0) or (CocoaWidgetSetState.keyEvent.keyCode=VK_UNKNOWN) then
    CocoaWidgetSetState.keyEvent.handled:= True;
end;

procedure TLCLCommonCallback.send_LM_CHAR_Message();
var
  msg: Cardinal;
  ret: PtrInt;
begin
  if NOT CocoaWidgetSetState.keyEvent.shouldSendCharMessage then
    Exit;

  if CocoaWidgetSetState.keyEvent.isSysKey then
    msg := LM_SYSCHAR
  else
    msg := LM_CHAR;

  ret:= self.doSendKeyMessage( msg, CocoaWidgetSetState.keyEvent.charCode, CocoaWidgetSetState.keyEvent.keyData, False );
  if ret <> 0 then
    CocoaWidgetSetState.keyEvent.handled:= True;
end;

procedure TLCLCommonCallback.KeyEvAfterDown;
begin
  if CocoaWidgetSetState.keyEvent.handled then exit;
  send_UTF8KeyPress;

  if CocoaWidgetSetState.keyEvent.handled then exit;
  send_CN_CHAR_Message;

  if CocoaWidgetSetState.keyEvent.handled then exit;
  send_LM_KEYDOWN_Message;

  if CocoaWidgetSetState.keyEvent.handled then exit;
  send_LM_CHAR_Message;
end;

procedure TLCLCommonCallback.KeyEvAfterUp;
var
  msg: Cardinal;
begin
  if CocoaWidgetSetState.keyEvent.handled then
    Exit;
  CocoaWidgetSetState.keyEvent.handled:= True;

  if CocoaWidgetSetState.keyEvent.isSysKey then
    msg := LM_SYSKEYUP
  else
    msg := LM_KEYUP;

  self.doSendKeyMessage( msg, CocoaWidgetSetState.keyEvent.keyCode, CocoaWidgetSetState.keyEvent.keyData, True );
end;

function TLCLCommonCallback.KeyEvBefore( const Event: NSEvent ): Boolean;
begin
  Result := True;
  CocoaWidgetSetState.keyEvent.handled:= False;

  if Event.type_ = NSFlagsChanged then
    createKeyStateFromFlagsChanged( Event, CocoaWidgetSetState.keyEvent )
  else
    createKeyStateFromKeyUpDown( Event, CocoaWidgetSetState.keyEvent );

  if CocoaWidgetSetState.keyEvent.isKeyDown then begin
    KeyEvBeforeDown;
    if NOT (TCocoaCbTrait.treatTabAsChar in self.traits) and (CocoaWidgetSetState.keyEvent.keyCode = VK_TAB) then
      Result:= False;
  end else
    KeyEvBeforeUp;

  if CocoaWidgetSetState.keyEvent.handled then
    Result:= False;

  // flagsChanged always needs to be passed on to Cocoa
  if Event.type_ = NSFlagsChanged then
    Result:= True;
end;

procedure TLCLCommonCallback.KeyEvAfter;
begin
  if NOT Assigned(self.target) then
    Exit;
  if CocoaWidgetSetState.keyEvent.isKeyDown then
    KeyEvAfterDown
  else
    KeyEvAfterUp;
end;

function TLCLCommonCallback.MouseUpDownEvent(
  const Event: NSEvent;
  const AForceAsMouseUp: Boolean = False;
  const AOverrideBlock: Boolean = False): Boolean;
const
  MSGKINDUP: array[0..3] of Integer = (LM_LBUTTONUP, LM_RBUTTONUP, LM_MBUTTONUP, LM_XBUTTONUP);
var
  Msg: TLMMouse;
  MousePos: NSPoint;
  MButton: NSInteger;
  lCaptureControlCallback: ICommonCallback;
  //Str: string;
  lEventType: NSEventType;

  bndPt, clPt, srchPt: TPoint; // clPt - is the one to send to LCL
                               // srchPt - is the one to use for each chidlren (clPt<>srchPt for TScrollBox)
  menuHandled : Boolean;
  mc: Integer; // modal counter

  function shouldBypassCocoa: Boolean;
  var
    ret: id;
  begin
    Result:= False;
    if NOT Assigned(target) then
      Exit;
    if NOT (csDesigning in target.ComponentState) then
      Exit;
    Result:= True;
    if NOT _owner.respondsToSelector( ObjcSelector('lclBypassCocoa:') ) then
      Exit;
    ret:= _owner.performSelector_withObject( ObjcSelector('lclBypassCocoa:'), Event );
    Result:= TCocoaNumberUtil.toBoolean( ret );
  end;

begin
  if Assigned(_owner) and not TCocoaViewUtil.isLCLEnabled(_owner) then
  begin
    Result := True; // Cocoa should not handle the message.
    Exit;           // LCL should not get the notification either, as the control is disabled.
  end;

  // If LCL control is provided and it's in designing state.
  // The default resolution: Notify LCL about event, but don't let Cocoa
  // do anything with it. (Result=true)
  Result := shouldBypassCocoa();

  lCaptureControlCallback := getCaptureControlCallback();
  //Str := (Format('MouseUpDownEvent Target=%s Self=%x CaptureControlCallback=%x', [target.name, PtrUInt(Self), PtrUInt(lCaptureControlCallback)]));
  if lCaptureControlCallback <> nil then
  begin
    Include( _states, TCocoaCbState.mouseRouting );
    Result := lCaptureControlCallback.MouseUpDownEvent(Event, AForceAsMouseUp);
    Exclude( _states, TCocoaCbState.mouseRouting );
    exit;
  end;

  // The following check prevents the same event to be handled twice
  // Because of the compositive nature of cocoa.
  // For example NSTextField (TEdit) may contains NSTextView and BOTH
  // will signal mouseDown when the field is selected by mouse the first time.
  // In this case only 1 mouseDown should be passed to LCL
  if (CocoaWidgetSetState.mouseEvent.lastDownUpTime = Event.timestamp) then begin
    if not AForceAsMouseUp then Exit; // the same mouse event from a composite child
    if CocoaWidgetSetState.mouseEvent.lastMouseWithForce then Exit; // the same forced mouseUp event from a composite child
  end;
  CocoaWidgetSetState.mouseEvent.lastDownUpTime := Event.timestamp;
  CocoaWidgetSetState.mouseEvent.lastMouseWithForce := AForceAsMouseUp;

  MousePos := Event.locationInWindow;
  OffsetMousePos(MousePos, bndPt, clPt, srchPt);

  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Keys := CocoaModifiersToKeyState(Event.modifierFlags) or CocoaPressedMouseButtonsToKeyState(NSEvent.pressedMouseButtons);
  Msg.XPos := clPt.X;
  Msg.YPos := clPt.Y;

  MButton := event.buttonNumber;
  if MButton >= 3 then
  begin
    // high word of XButton messages indicate the X button which is pressed
    Msg.Keys := Msg.Keys or (MButton - 2) shl 16;
    MButton := 3;
  end;

  lEventType := Event.type_;
  if AForceAsMouseUp then
    lEventType := NSLeftMouseUp;

  if CocoaConfigMouse.controlLeftToRightClick then begin
    // treat ctrl+left button as right button
    if (lEventType = NSLeftMouseDown) and
       (Event.modifierFlags and NSControlKeyMask <> 0) then
      LastMouseLeftButtonAsRight := True;
    if LastMouseLeftButtonAsRight then
    begin
      if MButton = 0 then
        MButton := 1;
      if Msg.Keys and MK_LBUTTON <> 0 then
        Msg.Keys := (Msg.Keys or MK_RBUTTON) and not MK_LBUTTON;
      if lEventType = NSLeftMouseUp then
        LastMouseLeftButtonAsRight := False;
    end;
  end;

  Result := Result or ( (TCocoaCbTrait.blockUpDown in self.traits) and NOT AOverrideBlock);
  mc := CocoaWidgetSetModalService.count;
  case lEventType of
    NSLeftMouseDown,
    NSRightMouseDown,
    NSOtherMouseDown:
    begin
      Msg.Msg := CheckMouseButtonDownUp(TLCLHandle(_owner),_target,LastMouse,
        _target.ClientToScreen(Point(Msg.XPos, Msg.YPos)),MButton+1,True);

      case LastMouse.ClickCount of
        2: Msg.Keys := msg.Keys or MK_DOUBLECLICK;
        3: Msg.Keys := msg.Keys or MK_TRIPLECLICK;
        4: Msg.Keys := msg.Keys or MK_QUADCLICK;
      end;

      NotifyApplicationUserInput(target, PLMessage(@Msg)^);
      self.deliverMessage(Msg);

    end;
    NSLeftMouseUp,
    NSRightMouseUp,
    NSOtherMouseUp:
    begin
      Msg.Msg := CheckMouseButtonDownUp(TLCLHandle(_owner),_target,LastMouse,
        _target.ClientToScreen(Point(Msg.XPos, Msg.YPos)),MButton+1,False);
      case LastMouse.ClickCount of
        2: Msg.Keys := msg.Keys or MK_DOUBLECLICK;
        3: Msg.Keys := msg.Keys or MK_TRIPLECLICK;
        4: Msg.Keys := msg.Keys or MK_QUADCLICK;
      end;

      NotifyApplicationUserInput(target, PLMessage(@Msg)^);
      self.deliverMessage(Msg);

      // TODO: Check if Cocoa has special context menu check event
      //       it does (menuForEvent:), but it doesn't work all the time
      //       http://sound-of-silence.com/?article=20150923
      if (GetTarget is TControl) and isContextMenuEvent(Event) then
      begin
        sendContextMenu(Event, menuHandled);
        if menuHandled then Result := true;
      end;
    end;
  end;

  if mc <> CocoaWidgetSetModalService.count then
  begin
    // showing of a modal window is causing "mouse" event to be lost.
    // so, preventing Cocoa from handling it
    Result := true;
    Exit;
  end;

  //debugln('MouseUpDownEvent:'+DbgS(Msg.Msg)+' Target='+target.name+);
  if not Result then
  //Result := Result or (TCocoaCbTrait.blockUpDown and not AOverrideBlock);
    case lEventType of
      NSLeftMouseDown,
      NSRightMouseDown,
      NSOtherMouseDown:
        CocoaWidgetSetState.mouseEvent.trackedControl:= _owner;
      NSLeftMouseUp,
      NSRightMouseUp,
      NSOtherMouseUp:
        if CocoaWidgetSetState.mouseEvent.trackedControl = _owner then
          CocoaWidgetSetState.mouseEvent.trackedControl:= nil;
    end;
end;

function TLCLCommonCallback.MouseMove(const Event: NSEvent): Boolean;
var
  Msg: TLMMouseMove;
  MousePos: NSPoint;
  i: integer;
  rect: TRect;
  //mp: TPoint;
  obj: NSObject;
  callback: ICommonCallback;
  targetControl: TWinControl;
  childControl:TWinControl;
  bndPt, clPt: TPoint;
  MouseTargetLookup: Boolean;
  srchPt: TPoint;
begin
  Result:= true;

  if not NSApp.isActive then
    exit;

  if Assigned(_owner) and not TCocoaViewUtil.isLCLEnabled(_owner) then
    exit;           // LCL should get the notification either.

  // If LCL control is provided and it's in designing state.
  // The default resolution: Notify LCL about event, but don't let Cocoa
  // do anything with it. (Result=true)
  Result := Assigned(target) and (csDesigning in target.ComponentState);

  MousePos := Event.locationInWindow;
  OffsetMousePos(MousePos, bndPt, clPt, srchPt);

  // For "dragged" events, the same "target" should be used
  MouseTargetLookup := Event.type_ = NSMouseMoved;

  if MouseTargetLookup then
  begin
    rect:=_owner.lclClientFrame;
    targetControl:=nil;

    callback := getCaptureControlCallback();
    if callback <> nil then
    begin
      Include( _states, TCocoaCbState.mouseRouting );
      Result := callback.MouseMove(Event);
      Exclude( _states, TCocoaCbState.mouseRouting );
      exit;
    end
    else
    begin
      if Event.window<>TCocoaWindowUtil.getWindowAtPos(TCocoaScreenUtil.getScreenPoint(Event)) then
        exit;

      rect:=target.BoundsRect;
      OffsetRect(rect, -rect.Left, -rect.Top);
      if (event.type_ = NSMouseMoved) and (not Types.PtInRect(rect, bndPt)) then
      begin
        // do not send negative coordinates (unless dragging mouse)
        Exit;
      end;

      if assigned(target.Parent) and not Types.PtInRect(rect, bndPt) then
         targetControl:=target.Parent // outside myself then route to parent
      else
      for i:=target.ControlCount-1 downto 0  do // otherwise check, if over child and route to child
        if target.Controls[i] is TWinControl then
        begin
          childControl:=TWinControl(target.Controls[i]);
          rect:=childControl.BoundsRect;
          if Types.PtInRect(rect, srchPt) and isValidMouseControl(childControl) then
          begin
            targetControl:=childControl;
            break;
          end;
        end;
    end;

    if assigned(targetControl) and NOT (TCocoaCbState.mouseRouting in _states) then
    begin
      if not targetControl.HandleAllocated then Exit; // Fixes crash due to events being sent after ReleaseHandle
      Include( _states, TCocoaCbState.mouseRouting );
       //debugln(target.name+' -> '+targetControl.Name+'- is parent:'+dbgs(targetControl=target.Parent)+' Point: '+dbgs(br)+' Rect'+dbgs(rect));
      obj := NSObject(targetControl.Handle).lclContentView;
      if obj = nil then Exit;
      callback := obj.lclGetCallback;
      if callback = nil then Exit; // Avoids crashes
      result := callback.MouseMove(Event);
      Exclude( _states, TCocoaCbState.mouseRouting );
      exit;
    end;

    if (Event.type_ = NSMouseMoved) and _owner.lclIsMouseInAuxArea(Event) then
    begin
      // mouse is over auxillary area that's "blind" to mouse moves
      // even though the mouse cursos is within the control bounds.
      // (i.e. scrollbars)
      CursorHelper.ForceSetDefaultCursor;
      Result := false;
      Exit;
    end;
  end;

  // debugln('Send to: '+target.name+' Point: '+dbgs(mp));

  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_MOUSEMOVE;
  Msg.Keys := CocoaModifiersToKeyState(Event.modifierFlags) or CocoaPressedMouseButtonsToKeyState(NSEvent.pressedMouseButtons);
  Msg.XPos := clPt.X;
  Msg.YPos := clPt.Y;

  //debugln('MouseMove x='+dbgs(MousePos.X)+' y='+dbgs(MousePos.Y)+' Target='+target.Name);

  NotifyApplicationUserInput(target, PLMessage(@Msg)^);
  // LCL/LM_MOUSEMOVE always return false, so we should discard return value
  self.deliverMessage(Msg);
  // 1. for MouseMove Event, it has been processed by LCL,
  //    and does not need Cocoa to continue processing.
  // 2. for MouseDragged Event, it needs Cocoa to continue processing
  //    (when not Dragging yet)
  if Event.type_ = NSMouseMoved then
    Result:= True
  else
    Result:= target.Dragging;

  // if Screen.Cursor set, LCL won't call TCocoaWSWinControl.SetCursor().
  // we need to set the cursor ourselves
  CursorHelper.SetScreenCursorWhenNotDefault;
end;

function TLCLCommonCallback.scrollWheel(const Event: NSEvent): Boolean;
var
  Msg: TLMMouseEvent;
  MousePos: NSPoint;
  MButton: NSInteger;
  bndPt, clPt, srchPt: TPoint;
  scrollDelta: Single;
  wheelDelta: Integer;
begin
  Result := False; // allow cocoa to handle message

  if Assigned(target)
    and not (csDesigning in target.ComponentState)
    and not TCocoaViewUtil.isLCLEnabled(_owner) then
    Exit;

  MousePos := Event.locationInWindow;
  OffsetMousePos(MousePos, bndPt, clPt, srchPt);

  MButton := event.buttonNumber;
  if MButton >= 3 then
     MButton := 3;

  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Button := MButton;
  Msg.X := round(clPt.X);
  Msg.Y := round(clPt.Y);
  Msg.State := CocoaModifiersToShiftState(Event.modifierFlags, NSEvent.pressedMouseButtons);

  // Some info on event.deltaY can be found here:
  // https://developer.apple.com/library/mac/releasenotes/AppKit/RN-AppKitOlderNotes/
  // It says that deltaY=1 means 1 line, and in the LCL 1 line is 120
  if (event.scrollingDeltaY <> 0) and
     ((event.scrollingDeltaX = 0) or not CocoaWidgetSetState.mouseEvent.isLastWheelHorz) then
  begin
    Msg.Msg := LM_MOUSEWHEEL;
    if event.hasPreciseScrollingDeltas then
      scrollDelta := event.scrollingDeltaY / 15 // Average line height
    else
      scrollDelta := event.scrollingDeltaY;
    wheelDelta := round(scrollDelta * 120);
  end
  else
  if event.scrollingDeltaX <> 0 then
  begin
    Msg.Msg := LM_MOUSEHWHEEL;
    // see "deltaX" documentation.
    // on macOS: -1 = right, +1 = left
    // on LCL:   -1 = left,  +1 = right
    wheelDelta := round(-event.scrollingDeltaX * 120);
  end
  else
  begin
    // Filter out empty events - See bug 28491
    Result := true;
    Exit;
  end;

  // Filter scrolls that affect both X and Y towards whatever the last scroll was
  CocoaWidgetSetState.mouseEvent.isLastWheelHorz := (Msg.Msg = LM_MOUSEHWHEEL);

  // Avoid overflow/underflow in message
  if wheelDelta > High(SmallInt) then
    wheelDelta := High(SmallInt)
  else if wheelDelta < Low(SmallInt) then
    wheelDelta := Low(SmallInt);
  Msg.WheelDelta := wheelDelta;

  NotifyApplicationUserInput(target, PLMessage(@Msg)^);
  Result := self.deliverMessage(Msg) <> 0;
end;

procedure TLCLCommonCallback.frameDidChange(const sender: id);
begin
  boundsDidChange(sender);
end;

procedure TLCLCommonCallback.boundsDidChange(const sender: id);
var
  NewBounds, OldBounds: TRect;
  PosMsg: TLMWindowPosChanged;
  Resized, Moved, ClientResized: Boolean;
  SizeType: Integer;
begin
  NewBounds := _handleFrame.lclFrame;

  //debugln('Newbounds='+ dbgs(newbounds));
  // send window pos changed
  PosMsg.Msg := LM_WINDOWPOSCHANGED;
  PosMsg.Result := 0;
  New(PosMsg.WindowPos);
  try
    with PosMsg.WindowPos^ do
    begin
      hWndInsertAfter := 0;
      x := NewBounds.Left;
      y := NewBounds.Right;
      cx := NewBounds.Right - NewBounds.Left;
      cy := NewBounds.Bottom - NewBounds.Top;
      flags := 0;
    end;
    LCLMessageGlue.DeliverMessage(target, PosMsg);
  finally
    Dispose(PosMsg.WindowPos);
  end;

  OldBounds := target.BoundsRect;
  //debugln('OldBounds Target='+target.Name+':'+ dbgs(OldBounds));

  Resized :=
    (OldBounds.Right - OldBounds.Left <> NewBounds.Right - NewBounds.Left) or
    (OldBounds.Bottom - OldBounds.Top <> NewBounds.Bottom - NewBounds.Top);

  Moved :=
    (OldBounds.Left <> NewBounds.Left) or
    (OldBounds.Top <> NewBounds.Top);

  ClientResized := (sender <> _handleFrame)
    and not EqualRect(target.ClientRect, _handleFrame.lclClientFrame);

  // update client rect
  if ClientResized or Resized or target.ClientRectNeedsInterfaceUpdate then begin
    target.InvalidateClientRectCache(false);
    ClientResized := True;
  end;

  // then send a LM_SIZE message
  if Resized or ClientResized then begin
    LCLSendSizeMsg(target, Max(NewBounds.Right - NewBounds.Left,0),
      Max(NewBounds.Bottom - NewBounds.Top,0), _owner.lclWindowState, True);
  end;

  // then send a LM_MOVE message
  if Moved then begin
    LCLSendMoveMsg(target, NewBounds.Left,
      NewBounds.Top, Move_SourceIsInterface);
  end;

  if NOT (TCocoaCbState.boundsReported in _states) then begin
    // first time we need this to update non cocoa based client rects
    target.InvalidateClientRectCache(true);
    Include( _states, TCocoaCbState.boundsReported );
  end;
end;

procedure TLCLCommonCallback.scroll(
  const isVert: Boolean;
  const Pos: Integer;
  const AScrollPart: NSScrollerPart);
var
  LMScroll: TLMScroll;
  b: Boolean;
  lclCode: Integer;
begin
  FillChar(LMScroll{%H-}, SizeOf(LMScroll), #0);
  //todo: this should be a part of a parameter
  //LMScroll.ScrollBar := target.Handle;

  if IsVert then
    LMScroll.Msg := LM_VSCROLL
  else
    LMScroll.Msg := LM_HSCROLL;

  LMScroll.Pos := Pos;
  case AScrollPart of
    NSScrollerDecrementPage: lclCode := SB_PAGELEFT;
    NSScrollerIncrementPage: lclCode := SB_PAGERIGHT;
    NSScrollerDecrementLine: lclCode := SB_LINELEFT;
    NSScrollerIncrementLine: lclCode := SB_LINERIGHT;
  else
    lclCode := SB_THUMBPOSITION;
  end;
  LMScroll.ScrollCode := lclCode; //SIF_POS;

  LCLMessageGlue.DeliverMessage(target, LMScroll);
end;

procedure TLCLCommonCallback.Draw(
  const ControlContext: NSGraphicsContext;
  const bounds, dirty: NSRect);
var
  PS: TPaintStruct;
  nsr:NSRect;
begin
  // todo: think more about draw call while previous draw still active
  if Assigned(_context) then
    Exit;
  _context := TCocoaContext.Create(ControlContext);
  _context.Control := _target;
  _context.isControlDC := True;
  try
    // debugln('Draw '+target.name+' bounds='+Dbgs(NSRectToRect(bounds))+' dirty='+Dbgs(NSRectToRect(dirty)));
    if _context.InitDraw(Round(bounds.size.width), Round(bounds.size.height)) then
    begin
      nsr:=dirty;
      if NOT _owner.isKindOfClass(NSView) or NOT NSView(_owner).isFlipped then
         nsr.origin.y:=bounds.size.height-dirty.origin.y-dirty.size.height;

      if (TCocoaCbTrait.opaque in self.traits) and (target.Color<>clDefault) then
      begin
        _context.BkMode:=OPAQUE;
        _context.BkColor:=target.Color;
        _context.BackgroundFill(nsr);
        //debugln('Background '+target.name+Dbgs(NSRectToRect(dirty)));
      end;

      FillChar(PS, SizeOf(TPaintStruct), 0);
      PS.hdc := HDC(_context);
      PS.rcPaint := TCocoaTypeUtil.toRect(nsr);
      LCLSendPaintMsg(target, HDC(_context), @PS);
      TCocoaCaretUtil.drawCaret( _owner.lclContentView );
    end;
  finally
    FreeAndNil(_context);
  end;
end;

procedure TLCLCommonCallback.DrawOverlay(
  const ControlContext: NSGraphicsContext;
  const bounds, dirty: NSRect);
var
  PS  : TPaintStruct;
  nsr : NSRect;
begin
  // todo: think more about draw call while previous draw still active
  if Assigned(_context) then
    Exit;
  _context := TCocoaContext.Create(ControlContext);
  _context.isControlDC := True;
  _context.isDesignDC := True;
  try
    // debugln('Draw '+target.name+' bounds='+Dbgs(NSRectToRect(bounds))+' dirty='+Dbgs(NSRectToRect(dirty)));
    if _context.InitDraw(Round(bounds.size.width), Round(bounds.size.height)) then
    begin
      nsr:=dirty;
      if NOT _owner.isKindOfClass(NSView) or NOT NSView(_owner).isFlipped then
         nsr.origin.y:=bounds.size.height-dirty.origin.y-dirty.size.height;

      FillChar(PS, SizeOf(TPaintStruct), 0);
      PS.hdc := HDC(_context);
      PS.rcPaint := TCocoaTypeUtil.toRect(nsr);
      LCLSendPaintMsg(target, HDC(_context), @PS);
    end;
  finally
    FreeAndNil(_context);
  end;
end;

procedure TLCLCommonCallback.RemoveTarget;
begin
  _target := nil;
end;

function TLCLCommonCallback.HandleFrame: NSView;
begin
  Result:= _handleFrame;
end;

procedure TLCLCommonCallback.SetHandleFrame(const AHandleFrame: NSView);
begin
  _handleFrame:= AHandleFrame;
end;

function TLCLCommonCallback.deliverMessage(var msg): LRESULT;
begin
  Result:= LCLMessageGlue.DeliverMessage(target, msg);
end;

function TLCLCommonCallback.doSendKeyMessage(
  const msg: Cardinal;
  var charCode: Word;
  const keyData: PtrInt;
  const notifyUserInput: Boolean ): PtrInt;
var
  message: TLMKey;
begin
  Result:= 0;
  if charCode = VK_UNKNOWN then
    Exit;

  FillChar( message, SizeOf(message), 0 );
  message.Msg:= msg;
  message.CharCode:= charCode;
  message.KeyData := keyData;

  if notifyUserInput then
    NotifyApplicationUserInput( target, PLMessage(@message)^ );

  if message.CharCode = VK_UNKNOWN then begin
    Result:= 1;
    Exit;
  end;

  Result:= self.DeliverMessage( message );
  charCode:= message.CharCode;
end;

end.

