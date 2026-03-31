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

  { TLCLCommonCallback }

  TLCLCommonCallback = class(TObject, ICommonCallBack)
  private
    var
      FPropStorage: TStringList;
      FContext: TCocoaContext;
      FBoundsReportedToChildren: boolean;
      FIsOpaque:boolean;
      FIsEventRouting:boolean;
      FLastWheelWasHorz:boolean;
  protected
    function deliverMessage(var msg): LRESULT;

    function GetIsOpaque: Boolean; inline;
    procedure SetIsOpaque(const AValue: Boolean); inline;
  protected
    _target    : TWinControl;
    _KeyMsg    : TLMKey;
    _CharMsg   : TLMKey;
    _SendChar  : Boolean;
    _IsSysKey  : Boolean;
    _IsKeyDown : Boolean;
    _KeyHandled: Boolean;
    _UTF8Character : array [0..7] of TUTF8Char;
    _UTF8Charcount : Integer;
    _handleFrame: NSView; // HWND and "frame" (rectangle) of the a control
  private
    procedure send_UTF8KeyPress();
    procedure send_CN_CHAR_Message();
    procedure send_LM_KEYDOWN_Message();
    procedure send_LM_CHAR_Message();
    function getCaptureControlCallback: ICommonCallBack;
    procedure sendContextMenu(Event: NSEvent; out ContextMenuHandled: Boolean);
  protected
    procedure OffsetMousePos(LocInWin: NSPoint; out PtInBounds, PtInClient, PtForChildCtrls: TPoint );
    procedure ScreenMousePos(var Point: NSPoint);
    procedure KeyEvBeforeDown;
    procedure KeyEvBeforeUp;
    procedure KeyEvAfterUp;
    procedure KeyEvFlagsChanged(Event: NSEvent);
    procedure KeyEvPrepare(Event: NSEvent); virtual;
  public
    Owner: NSObject;
    BlockCocoaUpDown: Boolean;
    BlockCocoaKeyBeep: Boolean;
    BlockCocoaMouseMove: Boolean;
    SuppressTabDown: Boolean; // all tabs should be suppressed, so Cocoa would not switch focus
    ForceReturnKeyDown: Boolean; // send keyDown/LM_KEYDOWN for Return even if handled by IntfUTF8KeyPress/CN_CHAR

    lastMouseDownUp: NSTimeInterval; // the last processed mouse Event
    lastMouseWithForce: Boolean;

    constructor Create(AOwner: NSObject; ATarget: TWinControl; AHandleFrame: NSView = nil); virtual;
    destructor Destroy; override;
    function GetPropStorage: TStringList; inline;
    function GetContext: HDC; inline;
    function GetTarget: TObject; inline;
    function GetCallbackObject: TObject; inline;
    function MouseUpDownEvent(
      const Event: NSEvent;
      const AForceAsMouseUp: Boolean = False;
      const AOverrideBlock: Boolean = False): Boolean; virtual;

    procedure KeyEvAfterDown(out AllowCocoaHandle: boolean);
    procedure KeyEvBefore(const Event: NSEvent; out AllowCocoaHandle: boolean);
    procedure KeyEvAfter;
    procedure KeyEvHandled; inline;
    procedure SetTabSuppress(const ASuppress: Boolean); inline;

    function MouseMove(const Event: NSEvent): Boolean; virtual;
    function scrollWheel(const Event: NSEvent): Boolean; virtual;
    procedure frameDidChange(const sender: id); virtual;
    procedure boundsDidChange(const sender: id); virtual;
    procedure scroll(
      const isVert: Boolean;
      const Pos: Integer;
      const AScrollPart: NSScrollerPart); virtual;
    procedure Draw(const ControlContext: NSGraphicsContext; const bounds, dirty: NSRect); virtual;
    procedure DrawOverlay(const ControlContext: NSGraphicsContext; const bounds, dirty: NSRect); virtual;
    procedure RemoveTarget; virtual;

    function HandleFrame: NSView; inline;
    procedure SetHandleFrame(const AHandleFrame: NSView ); inline;

    property Target: TWinControl read _target;
    property IsOpaque: Boolean read GetIsOpaque write SetIsOpaque;
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
  if Owner.isKindOfClass(NSWindow) then
  begin
    PtInBounds.x := Round(LocInWin.x);
    PtInBounds.y := Round(NSWindow(Owner).contentView.bounds.size.height - LocInWin.y);
    PtInClient := PtInBounds; // todo: it's different. But Owner is never NSWindow (it's TConentWindowView instead)
    PtForChildCtrls := PtInClient;
  end
  else if Owner.isKindOfClass(NSView) then
  begin
    pt := LocInWin;

    NSView(Owner).lclOffsetMousePos(pt);
    PtInBounds.x := Round(pt.x);
    PtInBounds.y := Round(pt.y);

    //pt := NSView(Owner).frame.origin;
    //if NSView(Owner).frame.
    cr := NSView(Owner).lclClientFrame;
    PtInClient.x := Round({PtInBounds.x - }pt.x - cr.Left);
    PtInClient.y := Round({PtInBounds.y - }pt.y - cr.Top);

    // child ctrls need not LayoutDelta
    cr := NSView(Owner).lclGetFrameToLayoutDelta;
    PtForChildCtrls := PtInClient;
    PtForChildCtrls.x := PtForChildCtrls.x + cr.Left;
    PtForChildCtrls.y := PtForChildCtrls.y + cr.Top;

    if Target is TScrollingWinControl then
      TCocoaViewUtil.lclOffsetWithEnclosingScrollView(NSView(Owner), PtForChildCtrls.x, PtForChildCtrls.y);

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
  lWindow := NSWindow(TCocoaWindowUtil.getWindow(Owner));
  if lWindow <> nil then
  begin
    f := lWindow.frame;
    Point.x := Point.x + f.origin.x;
    Point.y := TCocoaScreenUtil.globalScreenBottom - ( Point.y + f.origin.y );
  end;
end;

constructor TLCLCommonCallback.Create(AOwner: NSObject; ATarget: TWinControl; AHandleFrame: NSView);
begin
  inherited Create;
  Owner := AOwner;
  if Assigned(AHandleFrame) then
    _handleFrame := AHandleFrame
  else if Owner.isKindOfClass(NSView) then
    _handleFrame := NSView(AOwner);
  _target := ATarget;
  FContext := nil;
  FPropStorage := TStringList.Create;
  FPropStorage.Sorted := True;
  FPropStorage.Duplicates := dupAccept;
  FBoundsReportedToChildren:=false;
  FIsOpaque:=false;
  FIsEventRouting:=false;
  SuppressTabDown := true; // by default all Tabs would not be allowed for Cocoa.
                           // it should be enabled, i.e. for TMemo with WantTabs=true
end;

destructor TLCLCommonCallback.Destroy;
begin
  FContext.Free;
  FPropStorage.Free;
  _target := nil;
  inherited Destroy;
end;

function TLCLCommonCallback.GetPropStorage: TStringList;
begin
  Result := FPropStorage;
end;

function TLCLCommonCallback.GetContext: HDC;
begin
  Result := HDC( FContext );
end;

function TLCLCommonCallback.GetTarget: TObject;
begin
  Result := Target;
end;

function TLCLCommonCallback.GetCallbackObject: TObject;
begin
  Result := Self;
end;

function TLCLCommonCallback.getCaptureControlCallback: ICommonCallBack;
var
  obj: NSObject;
  lCaptureView: NSView;
begin
  Result := nil;
  if CocoaWidgetSetState.captureControl = 0 then Exit;
  obj := NSObject(CocoaWidgetSetState.captureControl);
  lCaptureView := obj.lclContentView;
  if (obj <> Owner) and (lCaptureView <> Owner) and not FIsEventRouting then
  begin
    Result := lCaptureView.lclGetCallback;
  end;
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
  Rcp := Owner;
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

procedure TLCLCommonCallback.KeyEvFlagsChanged(Event: NSEvent);
const
  cModifiersOfInterest: NSUInteger = (NSControlKeyMask or NSShiftKeyMask or NSAlphaShiftKeyMask or NSAlternateKeyMask or NSCommandKeyMask);
var
  CurMod, Diff: NSUInteger;
  VKKeyCode: word; // VK_ code
  KeyData: PtrInt; // Modifiers (ctrl, alt, mouse buttons...)
begin
  _SendChar := False;
  CurMod := Event.modifierFlags;
  //see what changed. we only care of bits 16 through 20
  Diff := (CocoaWidgetSetState.prevKeyModifiers xor CurMod) and cModifiersOfInterest;

  case Diff of
    0                  : VKKeyCode := VK_UNKNOWN; //nothing (that we cared of) changed
    NSControlKeyMask   : VKKeyCode := VK_CONTROL; //command mapped to control
    NSShiftKeyMask     : VKKeyCode := VK_SHIFT;
    NSAlphaShiftKeyMask: VKKeyCode := VK_CAPITAL; //caps lock
    NSAlternateKeyMask : VKKeyCode := VK_MENU;    //option is alt
    NSCommandKeyMask   : VKKeyCode := VK_LWIN;    //meta... map to left Windows Key?
  end;
  KeyData := CocoaModifiersToKeyState(CurMod);

  //diff is now equal to the mask of the bit that changed, so we can determine
  //if this change is a keydown (PrevKeyModifiers didn't have the bit set) or
  //a keyup (PrevKeyModifiers had the bit set)
  _IsKeyDown := ((CocoaWidgetSetState.prevKeyModifiers and Diff) = 0);

  FillChar(_KeyMsg, SizeOf(_KeyMsg), 0);
  _KeyMsg.KeyData := KeyData;
  _KeyMsg.CharCode := VKKeyCode;
  _IsSysKey := (VKKeyCode = VK_LWIN);
end;

procedure TLCLCommonCallback.KeyEvPrepare(Event: NSEvent);
var
  KeyCode: word;
  UTF8Character: TUTF8Char;   // char to send via IntfUtf8KeyPress
  KeyChar : char;          // Ascii char, when possible (xx_(SYS)CHAR)
  SendChar: boolean;       // Should we send char?
  VKKeyCode: word;         // VK_ code
  IsSysKey: Boolean;       // Is alt (option) key down?
  KeyData: PtrInt;         // Modifiers (ctrl, alt, mouse buttons...)
  ignModChr: NSString;
  i,c,j : integer;
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

  FillChar(_KeyMsg, SizeOf(_KeyMsg), 0);
  _KeyMsg.KeyData := KeyData;
  _KeyMsg.CharCode := VKKeyCode;
  _SendChar := SendChar;
  _IsSysKey := IsSysKey;
  _IsKeyDown := (Event.type_ = NSKeyDown);

  c:=0;
  i:=1;
  j:=0;
  while (i<=length(UTF8Character)) and (j<length(_UTF8Character)) do
  begin
    c := Utf8CodePointLen(@UTF8Character[i], length(UTF8Character)-i+1, false);
    if (j=0) and (c = length(UTF8Character)) then
    begin
      _UTF8Character[0] := UTF8Character;
      j := 1;
      break;
    end
    else if (c > 0) then
    begin
      _UTF8Character[j] := Copy(UTF8Character, i, c);
      inc(i,c);
      inc(j);
    end else
      break;
  end;
  if (j = 0) then _UTF8Character[0] := '';
  _UTF8Charcount := j;

  FillChar(_CharMsg, SizeOf(_CharMsg), 0);
  _CharMsg.KeyData := _KeyMsg.KeyData;
  _CharMsg.CharCode := ord(KeyChar);
end;

procedure TLCLCommonCallback.KeyEvBeforeDown;
begin
  // is the key combination help key (Cmd + ?)
  if _SendChar and _IsSysKey and (_UTF8Character[0] = '?') then
    Application.ShowHelpForObject(Target);

  //Send message to LCL
  if _KeyMsg.CharCode = VK_UNKNOWN then
    exit;

  // create the CN_KEYDOWN message
  if _IsSysKey then
    _KeyMsg.Msg := CN_SYSKEYDOWN
  else
    _KeyMsg.Msg := CN_KEYDOWN;

  NotifyApplicationUserInput(Target, PLMessage(@_KeyMsg)^);
  if (self.deliverMessage(_KeyMsg) <> 0) or (_KeyMsg.CharCode = VK_UNKNOWN) then
    // the LCL handled the key
    KeyEvHandled;
end;

procedure TLCLCommonCallback.KeyEvBeforeUp;
begin
  if _IsSysKey then
    _KeyMsg.Msg := CN_SYSKEYUP
  else
    _KeyMsg.Msg := CN_KEYUP;

  //Send message to LCL
  if _KeyMsg.CharCode <> VK_UNKNOWN then
  begin
    NotifyApplicationUserInput(Target, PLMessage(@_KeyMsg)^);
    if (self.deliverMessage(_KeyMsg) <> 0) or (_KeyMsg.CharCode = VK_UNKNOWN) then
    begin
      // the LCL has handled the key
      KeyEvHandled;
      Exit;
    end;
  end;
end;

procedure TLCLCommonCallback.send_UTF8KeyPress();
var
  i: integer;
  lclHandled: Boolean;
begin
  if not _sendChar then exit;

  // send the UTF8 keypress
  i := 0;
  lclHandled := false;
  for i := 0 to _UTF8Charcount -1 do
  begin
    lclHandled := false;
    if Target.IntfUTF8KeyPress(_UTF8Character[i], 1, _IsSysKey) then
      lclHandled := true;
  end;

  if lclHandled then
  begin
    // the LCL has handled the key
    if ForceReturnKeyDown and (_KeyMsg.CharCode = VK_RETURN) then
      _SendChar := False
    else
      KeyEvHandled;
  end;
end;

procedure TLCLCommonCallback.send_CN_CHAR_Message();
begin
  if not _SendChar then exit;

  if _IsSysKey then
    _CharMsg.Msg := CN_SYSCHAR
  else
    _CharMsg.Msg := CN_CHAR;

  if (self.deliverMessage(_CharMsg) <> 0) or (_CharMsg.CharCode=VK_UNKNOWN) then
    KeyEvHandled;
end;

procedure TLCLCommonCallback.send_LM_KEYDOWN_Message();
begin
  if _KeyMsg.CharCode = VK_UNKNOWN then exit;

  if _IsSysKey then
    _KeyMsg.Msg := LM_SYSKEYDOWN
  else
    _KeyMsg.Msg := LM_KEYDOWN;

  if (self.deliverMessage(_KeyMsg) <> 0) or (_KeyMsg.CharCode = VK_UNKNOWN) then
    KeyEvHandled;
end;

procedure TLCLCommonCallback.send_LM_CHAR_Message();
begin
  if not _SendChar then exit;

  if _IsSysKey then
    _CharMsg.Msg := LM_SYSCHAR
  else
    _CharMsg.Msg := LM_CHAR;

  if self.deliverMessage(_CharMsg) <> 0 then
    KeyEvHandled;
end;

procedure TLCLCommonCallback.KeyEvAfterDown(out AllowCocoaHandle: boolean);
begin
  AllowCocoaHandle:= false;

  if _KeyHandled then exit;
  send_UTF8KeyPress;

  if _KeyHandled then exit;
  send_CN_CHAR_Message;

  if _KeyHandled then exit;
  send_LM_KEYDOWN_Message;

  if _KeyHandled then exit;
  send_LM_CHAR_Message;

  if _KeyHandled then exit;
  AllowCocoaHandle:= not BlockCocoaKeyBeep;
end;

procedure TLCLCommonCallback.KeyEvAfterUp;
begin
  if _KeyHandled then Exit;
  KeyEvHandled;

  //Send a LM_(SYS)KEYUP
  if _IsSysKey then
    _KeyMsg.Msg := LM_SYSKEYUP
  else
    _KeyMsg.Msg := LM_KEYUP;

  if self.deliverMessage(_KeyMsg) <> 0 then
  begin
    // the LCL handled the key
    NotifyApplicationUserInput(Target, PLMessage(@_KeyMsg)^);
    Exit;
  end;
end;

procedure TLCLCommonCallback.KeyEvBefore(
  const  Event: NSEvent;
  out AllowCocoaHandle: boolean);
begin
  _keyHandled := False;
  AllowCocoaHandle := true;

  if Event.type_ = NSFlagsChanged then
    KeyEvFlagsChanged(Event)
  else
    KeyEvPrepare(Event);

  if _IsKeyDown then begin
    KeyEvBeforeDown;
    if SuppressTabDown and (_KeyMsg.CharCode = VK_TAB) then
      AllowCocoaHandle := false;
  end else
    KeyEvBeforeUp;

  if _keyHandled then
    AllowCocoaHandle := false;

  // flagsChanged always needs to be passed on to Cocoa
  if Event.type_ = NSFlagsChanged then
    AllowCocoaHandle := true;
end;

procedure TLCLCommonCallback.KeyEvAfter;
var
  AllowCocoaHandle: Boolean;
begin
  if NOT Assigned(self.Target) then
    Exit;
  if _IsKeyDown then KeyEvAfterDown(AllowCocoaHandle)
  else KeyEvAfterUp;
end;

procedure TLCLCommonCallback.KeyEvHandled;
begin
  _KeyHandled := True;
end;

procedure TLCLCommonCallback.SetTabSuppress(const ASuppress: Boolean);
begin
  SuppressTabDown := ASuppress;
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
    if NOT Assigned(Target) then
      Exit;
    if NOT (csDesigning in Target.ComponentState) then
      Exit;
    Result:= True;
    if NOT Owner.respondsToSelector( ObjcSelector('lclBypassCocoa:') ) then
      Exit;
    ret:= Owner.performSelector_withObject( ObjcSelector('lclBypassCocoa:'), Event );
    Result:= TCocoaNumberUtil.toBoolean( ret );
  end;

begin
  if Assigned(Owner) and not TCocoaViewUtil.isLCLEnabled(Owner) then
  begin
    Result := True; // Cocoa should not handle the message.
    Exit;           // LCL should not get the notification either, as the control is disabled.
  end;

  // If LCL control is provided and it's in designing state.
  // The default resolution: Notify LCL about event, but don't let Cocoa
  // do anything with it. (Result=true)
  Result := shouldBypassCocoa();

  lCaptureControlCallback := getCaptureControlCallback();
  //Str := (Format('MouseUpDownEvent Target=%s Self=%x CaptureControlCallback=%x', [Target.name, PtrUInt(Self), PtrUInt(lCaptureControlCallback)]));
  if lCaptureControlCallback <> nil then
  begin
    FIsEventRouting:=true;
    Result := lCaptureControlCallback.MouseUpDownEvent(Event, AForceAsMouseUp);
    FIsEventRouting:=false;
    exit;
  end;

  // The following check prevents the same event to be handled twice
  // Because of the compositive nature of cocoa.
  // For example NSTextField (TEdit) may contains NSTextView and BOTH
  // will signal mouseDown when the field is selected by mouse the first time.
  // In this case only 1 mouseDown should be passed to LCL
  if (lastMouseDownUp = Event.timestamp) then begin
    if not AForceAsMouseUp then Exit; // the same mouse event from a composite child
    if lastMouseWithForce then Exit; // the same forced mouseUp event from a composite child
  end;
  lastMouseDownUp := Event.timestamp;
  lastMouseWithForce := AForceAsMouseUp;


  FillChar(Msg, SizeOf(Msg), #0);

  MousePos := Event.locationInWindow;
  OffsetMousePos(MousePos, bndPt, clPt, srchPt);

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

  Result := Result or (BlockCocoaUpDown and not AOverrideBlock);
  mc := CocoaWidgetSetModalService.count;
  case lEventType of
    NSLeftMouseDown,
    NSRightMouseDown,
    NSOtherMouseDown:
    begin
      Msg.Msg := CheckMouseButtonDownUp(TLCLHandle(Owner),_target,LastMouse,
        _target.ClientToScreen(Point(Msg.XPos, Msg.YPos)),MButton+1,True);

      case LastMouse.ClickCount of
        2: Msg.Keys := msg.Keys or MK_DOUBLECLICK;
        3: Msg.Keys := msg.Keys or MK_TRIPLECLICK;
        4: Msg.Keys := msg.Keys or MK_QUADCLICK;
      end;

      NotifyApplicationUserInput(Target, PLMessage(@Msg)^);
      self.deliverMessage(Msg);

    end;
    NSLeftMouseUp,
    NSRightMouseUp,
    NSOtherMouseUp:
    begin
      Msg.Msg := CheckMouseButtonDownUp(TLCLHandle(Owner),_target,LastMouse,
        _target.ClientToScreen(Point(Msg.XPos, Msg.YPos)),MButton+1,False);
      case LastMouse.ClickCount of
        2: Msg.Keys := msg.Keys or MK_DOUBLECLICK;
        3: Msg.Keys := msg.Keys or MK_TRIPLECLICK;
        4: Msg.Keys := msg.Keys or MK_QUADCLICK;
      end;

      NotifyApplicationUserInput(Target, PLMessage(@Msg)^);
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

  //debugln('MouseUpDownEvent:'+DbgS(Msg.Msg)+' Target='+Target.name+);
  if not Result then
  //Result := Result or (BlockCocoaUpDown and not AOverrideBlock);
    case lEventType of
      NSLeftMouseDown,
      NSRightMouseDown,
      NSOtherMouseDown:
        TrackedControl := Owner;
      NSLeftMouseUp,
      NSRightMouseUp,
      NSOtherMouseUp:
      begin
        if TrackedControl = Owner then TrackedControl := nil;
        if lEventType = NSLeftMouseUp then
          BlockCocoaMouseMove := false;
      end;
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

  if Assigned(Owner) and not TCocoaViewUtil.isLCLEnabled(Owner) then
    exit;           // LCL should get the notification either.

  // If LCL control is provided and it's in designing state.
  // The default resolution: Notify LCL about event, but don't let Cocoa
  // do anything with it. (Result=true)
  Result := Assigned(Target) and (csDesigning in Target.ComponentState);

  MousePos := Event.locationInWindow;
  OffsetMousePos(MousePos, bndPt, clPt, srchPt);

  // For "dragged" events, the same "Target" should be used
  MouseTargetLookup := Event.type_ = NSMouseMoved;

  if MouseTargetLookup then
  begin
    rect:=Owner.lclClientFrame;
    targetControl:=nil;

    callback := getCaptureControlCallback();
    if callback <> nil then
    begin
      FIsEventRouting:=true;
      Result := callback.MouseMove(Event);
      FIsEventRouting:=false;
      exit;
    end
    else
    begin
      if Event.window<>TCocoaWindowUtil.getWindowAtPos(TCocoaScreenUtil.getScreenPoint(Event)) then
        exit;

      rect:=Target.BoundsRect;
      OffsetRect(rect, -rect.Left, -rect.Top);
      if (event.type_ = NSMouseMoved) and (not Types.PtInRect(rect, bndPt)) then
      begin
        // do not send negative coordinates (unless dragging mouse)
        Exit;
      end;

      if assigned(Target.Parent) and not Types.PtInRect(rect, bndPt) then
         targetControl:=Target.Parent // outside myself then route to parent
      else
      for i:=Target.ControlCount-1 downto 0  do // otherwise check, if over child and route to child
        if Target.Controls[i] is TWinControl then
        begin
          childControl:=TWinControl(Target.Controls[i]);
          rect:=childControl.BoundsRect;
          if Types.PtInRect(rect, srchPt) and isValidMouseControl(childControl) then
          begin
            targetControl:=childControl;
            break;
          end;
        end;
    end;

    if assigned(targetControl) and not FIsEventRouting then
    begin
      if not targetControl.HandleAllocated then Exit; // Fixes crash due to events being sent after ReleaseHandle
      FIsEventRouting:=true;
       //debugln(Target.name+' -> '+targetControl.Name+'- is parent:'+dbgs(targetControl=Target.Parent)+' Point: '+dbgs(br)+' Rect'+dbgs(rect));
      obj := NSObject(targetControl.Handle).lclContentView;
      if obj = nil then Exit;
      callback := obj.lclGetCallback;
      if callback = nil then Exit; // Avoids crashes
      result := callback.MouseMove(Event);
      FIsEventRouting := false;
      exit;
    end;

    if (Event.type_ = NSMouseMoved) and Owner.lclIsMouseInAuxArea(Event) then
    begin
      // mouse is over auxillary area that's "blind" to mouse moves
      // even though the mouse cursos is within the control bounds.
      // (i.e. scrollbars)
      CursorHelper.ForceSetDefaultCursor;
      Result := false;
      Exit;
    end;
  end;

  // debugln('Send to: '+Target.name+' Point: '+dbgs(mp));

  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_MOUSEMOVE;
  Msg.Keys := CocoaModifiersToKeyState(Event.modifierFlags) or CocoaPressedMouseButtonsToKeyState(NSEvent.pressedMouseButtons);
  Msg.XPos := clPt.X;
  Msg.YPos := clPt.Y;

  //debugln('MouseMove x='+dbgs(MousePos.X)+' y='+dbgs(MousePos.Y)+' Target='+Target.Name);

  NotifyApplicationUserInput(Target, PLMessage(@Msg)^);
  // LCL/LM_MOUSEMOVE always return false, so we should discard return value
  self.deliverMessage(Msg);
  // 1. for MouseMove Event, it has been processed by LCL,
  //    and does not need Cocoa to continue processing.
  // 2. for MouseDragged Event, it needs Cocoa to continue processing
  //    (when not Dragging yet)
  if Event.type_ = NSMouseMoved then
    Result:= True
  else
    Result:= Target.Dragging;

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

  if Assigned(Target)
    and not (csDesigning in Target.ComponentState)
    and not TCocoaViewUtil.isLCLEnabled(Owner) then
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
     ((event.scrollingDeltaX = 0) or not FLastWheelWasHorz) then
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
  FLastWheelWasHorz := (Msg.Msg = LM_MOUSEHWHEEL);

  // Avoid overflow/underflow in message
  if wheelDelta > High(SmallInt) then
    wheelDelta := High(SmallInt)
  else if wheelDelta < Low(SmallInt) then
    wheelDelta := Low(SmallInt);
  Msg.WheelDelta := wheelDelta;

  NotifyApplicationUserInput(Target, PLMessage(@Msg)^);
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
  NewBounds := HandleFrame.lclFrame;

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
    LCLMessageGlue.DeliverMessage(Target, PosMsg);
  finally
    Dispose(PosMsg.WindowPos);
  end;

  OldBounds := Target.BoundsRect;
  //debugln('OldBounds Target='+Target.Name+':'+ dbgs(OldBounds));

  Resized :=
    (OldBounds.Right - OldBounds.Left <> NewBounds.Right - NewBounds.Left) or
    (OldBounds.Bottom - OldBounds.Top <> NewBounds.Bottom - NewBounds.Top);

  Moved :=
    (OldBounds.Left <> NewBounds.Left) or
    (OldBounds.Top <> NewBounds.Top);

  ClientResized := (sender <> HandleFrame)
    and not EqualRect(Target.ClientRect, HandleFrame.lclClientFrame);

  // update client rect
  if ClientResized or Resized or Target.ClientRectNeedsInterfaceUpdate then
  begin
    Target.InvalidateClientRectCache(false);
    ClientResized := True;
  end;

  // then send a LM_SIZE message
  if Resized or ClientResized then
  begin
    LCLSendSizeMsg(Target, Max(NewBounds.Right - NewBounds.Left,0),
      Max(NewBounds.Bottom - NewBounds.Top,0), Owner.lclWindowState, True);
  end;

  // then send a LM_MOVE message
  if Moved then
  begin
    LCLSendMoveMsg(Target, NewBounds.Left,
      NewBounds.Top, Move_SourceIsInterface);
  end;

  if not FBoundsReportedToChildren then // first time we need this to update non cocoa based client rects
  begin
    Target.InvalidateClientRectCache(true);
    FBoundsReportedToChildren:=true;
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
  //LMScroll.ScrollBar := Target.Handle;

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

  LCLMessageGlue.DeliverMessage(Target, LMScroll);
end;

procedure TLCLCommonCallback.Draw(
  const ControlContext: NSGraphicsContext;
  const bounds, dirty: NSRect);
var
  PS: TPaintStruct;
  nsr:NSRect;
begin
  // todo: think more about draw call while previous draw still active
  if Assigned(FContext) then
    Exit;
  FContext := TCocoaContext.Create(ControlContext);
  FContext.Control := _target;
  FContext.isControlDC := True;
  try
    // debugln('Draw '+Target.name+' bounds='+Dbgs(NSRectToRect(bounds))+' dirty='+Dbgs(NSRectToRect(dirty)));
    if FContext.InitDraw(Round(bounds.size.width), Round(bounds.size.height)) then
    begin
      nsr:=dirty;
      if NOT Owner.isKindOfClass(NSView) or NOT NSView(Owner).isFlipped then
         nsr.origin.y:=bounds.size.height-dirty.origin.y-dirty.size.height;

      if FIsOpaque and (Target.Color<>clDefault) then
      begin
        FContext.BkMode:=OPAQUE;
        FContext.BkColor:=Target.Color;
        FContext.BackgroundFill(nsr);
        //debugln('Background '+Target.name+Dbgs(NSRectToRect(dirty)));
      end;

      FillChar(PS, SizeOf(TPaintStruct), 0);
      PS.hdc := HDC(FContext);
      PS.rcPaint := TCocoaTypeUtil.toRect(nsr);
      LCLSendPaintMsg(Target, HDC(FContext), @PS);
      TCocoaCaretUtil.drawCaret( Owner.lclContentView );
    end;
  finally
    FreeAndNil(FContext);
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
  if Assigned(FContext) then
    Exit;
  FContext := TCocoaContext.Create(ControlContext);
  FContext.isControlDC := True;
  FContext.isDesignDC := True;
  try
    // debugln('Draw '+Target.name+' bounds='+Dbgs(NSRectToRect(bounds))+' dirty='+Dbgs(NSRectToRect(dirty)));
    if FContext.InitDraw(Round(bounds.size.width), Round(bounds.size.height)) then
    begin
      nsr:=dirty;
      if NOT Owner.isKindOfClass(NSView) or NOT NSView(Owner).isFlipped then
         nsr.origin.y:=bounds.size.height-dirty.origin.y-dirty.size.height;

      FillChar(PS, SizeOf(TPaintStruct), 0);
      PS.hdc := HDC(FContext);
      PS.rcPaint := TCocoaTypeUtil.toRect(nsr);
      LCLSendPaintMsg(Target, HDC(FContext), @PS);
    end;
  finally
    FreeAndNil(FContext);
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
  Result:= LCLMessageGlue.DeliverMessage(Target, msg);
end;

function TLCLCommonCallback.GetIsOpaque: Boolean;
begin
  Result:= FIsOpaque;
end;

procedure TLCLCommonCallback.SetIsOpaque(const AValue: Boolean);
begin
  FIsOpaque:=AValue;
end;

end.

