unit CocoaWSCommon;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$include cocoadefines.inc}
{.$DEFINE COCOA_DEBUG_SETBOUNDS}

interface

uses
  Types, Classes, Controls, SysUtils,
  WSControls, LCLType, LCLMessageGlue, LMessages, LCLProc, LCLIntf, Graphics, Forms,
  StdCtrls,
  CocoaAll, CocoaInt, CocoaConfig, CocoaPrivate, CocoaCallback, CocoaUtils,
  CocoaCustomControl, CocoaScrollers, CocoaWSScrollers, CocoaFullControlEdit,
  CocoaGDIObjects, CocoaCursor, CocoaCaret, cocoa_extra;

type
  { TLCLCommonCallback }

  TLCLCommonCallback = class(TObject, ICommonCallBack)
  private
    var
      FPropStorage: TStringList;
      FContext: TCocoaContext;
      FHasCaret: Boolean;
      FBoundsReportedToChildren: boolean;
      FIsOpaque:boolean;
      FIsEventRouting:boolean;
      FLastWheelWasHorz:boolean;
  protected
    function GetHasCaret: Boolean;
    procedure SetHasCaret(AValue: Boolean);
    function GetIsOpaque: Boolean;
    procedure SetIsOpaque(AValue: Boolean);
    function GetShouldBeEnabled: Boolean;
  protected
    FTarget: TWinControl;
    _KeyMsg    : TLMKey;
    _CharMsg   : TLMKey;
    _SendChar  : Boolean;
    _IsSysKey  : Boolean;
    _IsKeyDown : Boolean;
    _KeyHandled: Boolean;
    _isCocoaOnlyState: Boolean;
    _UTF8Character : array [0..7] of TUTF8Char;
    _UTF8Charcount : Integer;
    _handleFrame: NSView; // HWND and "frame" (rectangle) of the a control
  private
    procedure send_UTF8KeyPress();
    procedure send_CN_CHAR_Message();
    procedure send_LM_KEYDOWN_Message();
    procedure send_LM_CHAR_Message();
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
    function GetPropStorage: TStringList;
    function GetContext: TCocoaContext;
    function GetTarget: TObject;
    function GetCallbackObject: TObject;
    function GetCaptureControlCallback: ICommonCallBack;
    procedure SendContextMenu(Event: NSEvent; out ContextMenuHandled: Boolean);
    function MouseUpDownEvent(Event: NSEvent; AForceAsMouseUp: Boolean = False; AOverrideBlock: Boolean = False): Boolean; virtual;

    procedure KeyEvAfterDown(out AllowCocoaHandle: boolean);
    procedure KeyEvBefore(Event: NSEvent; out AllowCocoaHandle: boolean);
    procedure KeyEvAfter;
    procedure KeyEvHandled;
    procedure SetTabSuppress(ASuppress: Boolean);
    function CanFocus: Boolean; virtual;

    function IsCocoaOnlyState: Boolean;
    procedure SetCocoaOnlyState(state: Boolean);

    procedure MouseClick; virtual;
    function MouseMove(Event: NSEvent): Boolean; virtual;
    function scrollWheel(Event: NSEvent): Boolean; virtual;
    procedure frameDidChange(sender: id); virtual;
    procedure boundsDidChange(sender: id); virtual;
    procedure BecomeFirstResponder; virtual;
    procedure ResignFirstResponder; virtual;
    procedure DidBecomeKeyNotification; virtual;
    procedure DidResignKeyNotification; virtual;
    function SendOnEditCut: Boolean; virtual;
    function SendOnEditPaste: Boolean; virtual;
    procedure SendOnChange; virtual;
    procedure SendOnTextChanged; virtual; // text controls (like spin) respond to OnChange for this event, but not for SendOnChange
    procedure scroll(isVert: Boolean; Pos: Integer; AScrollPart: NSScrollerPart); virtual;
    function DeliverMessage(var Msg): LRESULT; virtual; overload;
    function DeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult; virtual; overload;
    procedure Draw(ControlContext: NSGraphicsContext; const bounds, dirty: NSRect); virtual;
    procedure DrawBackground(ctx: NSGraphicsContext; const bounds, dirtyRect: NSRect); virtual;
    procedure DrawOverlay(ControlContext: NSGraphicsContext; const bounds, dirty: NSRect); virtual;
    procedure RemoveTarget; virtual;

    procedure InputClientInsertText(const utf8: string);

    function HandleFrame: NSView;
    procedure SetHandleFrame( AHandleFrame: NSView );

    property HasCaret: Boolean read GetHasCaret write SetHasCaret;
    property Target: TWinControl read FTarget;
    property IsOpaque: Boolean read GetIsOpaque write SetIsOpaque;
  end;

  TLCLCommonCallBackClass = class of TLCLCommonCallBack;

  { TLCLFullControlEditCallBack }

  // CallBack for LCL Full Control Edit (such as SynEdit/ATSynEdit)
  TLCLFullControlEditCallBack = class(TLCLCommonCallBack)
  protected
    procedure KeyEvPrepare(Event: NSEvent); override;
  end;

  { TCocoaWSWinControl }

  { TCocoaWSControl }

  TCocoaWSControl = class(TWSControl)
  published
    class function GetCanvasScaleFactor(const AControl: TControl): Double; override;
  end;

  { TCocoaWSWinControl }

  TCocoaWSWinControl = class(TWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function GetCanvasScaleFactor(const AControl: TControl): Double; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;

    class function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;
    class procedure PaintTo(const AWinControl: TWinControl; ADC: HDC; X, Y: Integer); override;
  end;

  { TCocoaWSCustomControl }

  TCocoaWSCustomControl = class(TWSCustomControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetBorderStyle(const AWinControl: TWinControl;
      const ABorderStyle: TBorderStyle); override;
  end;

procedure UpdateControlFocusRing( cocoaControl: NSView; lclControl: TWinControl );
procedure ScrollViewSetScrollStyles(AScroll: TCocoaScrollView; AStyles: TScrollStyle);
procedure ScrollViewSetBorderStyle(sv: NSScrollView; astyle: TBorderStyle);

function ButtonStateToShiftState(BtnState: PtrUInt): TShiftState;
function CocoaModifiersToKeyState(AModifiers: NSUInteger): PtrInt;
function CocoaPressedMouseButtonsToKeyState(AMouseButtons: NSUInteger): PtrInt;
function CocoaModifiersToShiftState(AModifiers: NSUInteger; AMouseButtons: NSUInteger): TShiftState;

function NSObjectDebugStr(obj: NSObject): string;
function CallbackDebugStr(cb: ICommonCallback): string;
procedure DebugDumpParents(fromView: NSView);

const
  VerticalScrollerVisible: array[TScrollStyle] of boolean = (
 {ssNone          } false,
 {ssHorizontal    } false,
 {ssVertical      } true,
 {ssBoth          } true,
 {ssAutoHorizontal} false,
 {ssAutoVertical  } true,
 {ssAutoBoth      } true
  );

  HorizontalScrollerVisible: array[TScrollStyle] of boolean = (
 {ssNone          } false,
 {ssHorizontal    } true,
 {ssVertical      } false,
 {ssBoth          } true,
 {ssAutoHorizontal} true,
 {ssAutoVertical  } false,
 {ssAutoBoth      } true
  );

  ScrollerAutoHide: array[TScrollStyle] of boolean = (
 {ssNone          } false,
 {ssHorizontal    } false,
 {ssVertical      } false,
 {ssBoth          } false,
 {ssAutoHorizontal} true,
 {ssAutoVertical  } true,
 {ssAutoBoth      } true
  );

implementation

uses
  Math;

type
  TWinControlAccess = class(TWinControl);

var
  LastMouse: TLastMouseInfo;
  LastMouseLeftButtonAsRight: Boolean;

function ButtonStateToShiftState(BtnState: PtrUInt): TShiftState;
begin
  Result := [];
  if BtnState and MK_SHIFT > 0 then Include(Result, ssShift);
  if BtnState and MK_CONTROL > 0 then Include(Result, ssCtrl);
  if BtnState and MK_ALT > 0 then Include(Result, ssAlt);
  if BtnState and MK_LBUTTON > 0 then Include(Result, ssLeft);
  if BtnState and MK_RBUTTON > 0 then Include(Result, ssRight);
  if BtnState and MK_MBUTTON > 0 then Include(Result, ssMiddle);
  if BtnState and MK_XBUTTON1 > 0 then Include(Result, ssExtra1);
  if BtnState and MK_XBUTTON2 > 0 then Include(Result, ssExtra2);
  // what MK_xxx used for Meta?
end;

function CocoaModifiersToShiftState(AModifiers: NSUInteger; AMouseButtons: NSUInteger): TShiftState;
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

procedure UpdateControlFocusRing(cocoaControl: NSView; lclControl: TWinControl);
const
  NSFocusRing : array [TBorderStyle] of NSBorderType = (
    NSFocusRingTypeNone,   // bsNone
    NSFocusRingTypeDefault // bsSingle
  );
var
  frs: TCocoaConfigFocusRing.Strategy;
  borderStyle: TBorderStyle;
begin
  frs:= CocoaConfigFocusRing.getStrategy( cocoaControl.className );
  case frs of
    TCocoaConfigFocusRing.Strategy.none:
      cocoaControl.setFocusRingType( NSFocusRingTypeNone );
    TCocoaConfigFocusRing.Strategy.required:
      cocoaControl.setFocusRingType( NSFocusRingTypeExterior );
    TCocoaConfigFocusRing.Strategy.border: begin
      borderStyle:= TWinControlAccess(lclControl).BorderStyle;
      cocoaControl.setFocusRingType( NSFocusRing[borderStyle] );
    end;
 // TCocoaFocusRingStrategy.default: no need to set FocusRing
  end;
end;

procedure ScrollViewSetScrollStyles(AScroll: TCocoaScrollView; AStyles: TScrollStyle);
begin
  AScroll.setHasVerticalScroller(VerticalScrollerVisible[AStyles]);
  AScroll.setHasHorizontalScroller(HorizontalScrollerVisible[AStyles]);
  AScroll.setAutohidesScrollers(ScrollerAutoHide[AStyles]);
end;

{ TLCLCommonCallback }

function TLCLCommonCallback.GetHasCaret: Boolean;
begin
  Result := FHasCaret;
end;

procedure TLCLCommonCallback.SetHasCaret(AValue: Boolean);
begin
  FHasCaret := AValue;
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

procedure TLCLCommonCallback.OffsetMousePos(LocInWin: NSPoint; out PtInBounds, PtInClient, PtForChildCtrls: TPoint);
var
  lView: NSView;
  pt: NSPoint;
  cr: TRect;
  es: NSScrollView;
  r: NSRect;
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

    es := NSView(Owner).enclosingScrollView;
    if Assigned(es) and (es.documentView = NSView(Owner)) then begin
      r := es.documentVisibleRect;
      if NSView(Owner).isFlipped then
        r.origin.y := (es.documentView.frame.size.height - r.size.height - r.origin.y);
      inc(PtForChildCtrls.y, Round(r.origin.y));
      inc(PtForChildCtrls.x, Round(r.origin.x));
    end;

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
  lWindow := NSWindow(GetNSObjectWindow(Owner));
  if lWindow <> nil then
  begin
    f := lWindow.frame;
    Point.x := Point.x + f.origin.x;
    Point.y := NSGlobalScreenBottom - ( Point.y + f.origin.y );
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
  FTarget := ATarget;
  FContext := nil;
  FHasCaret := False;
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
  FTarget := nil;
  inherited Destroy;
end;

function TLCLCommonCallback.GetPropStorage: TStringList;
begin
  Result := FPropStorage;
end;

function TLCLCommonCallback.GetContext: TCocoaContext;
begin
  Result := FContext;
end;

function TLCLCommonCallback.GetTarget: TObject;
begin
  Result := Target;
end;

function TLCLCommonCallback.GetCallbackObject: TObject;
begin
  Result := Self;
end;

function TLCLCommonCallback.GetCaptureControlCallback: ICommonCallBack;
var
  obj: NSObject;
  lCaptureView: NSView;
begin
  Result := nil;
  if CocoaWidgetSet.CaptureControl = 0 then Exit;
  obj := NSObject(CocoaWidgetSet.CaptureControl);
  lCaptureView := obj.lclContentView;
  if (obj <> Owner) and (lCaptureView <> Owner) and not FIsEventRouting then
  begin
    Result := lCaptureView.lclGetCallback;
  end;
end;

{ If a window does not display a shortcut menu it should pass
  this message to the DefWindowProc function. If a window is
  a child window, DefWindowProc sends the message to the parent. }
procedure TLCLCommonCallback.SendContextMenu(Event: NSEvent; out
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
  Diff := (TCocoaApplication(NSAPP).PrevKeyModifiers xor CurMod) and cModifiersOfInterest;

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
  _IsKeyDown := ((TCocoaApplication(NSAPP).PrevKeyModifiers and Diff) = 0);

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
    VKKeyCode := MacCharToVK(ignModChr.characterAtIndex(0));
    if VKKeyCode = VK_UNKNOWN then
      VKKeyCode := MacCodeToVK(KeyCode); // fallback
  end
  else
    VKKeyCode := MacCodeToVK(KeyCode);

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

  NotifyApplicationUserInput(Target, _KeyMsg.Msg);
  if (DeliverMessage(_KeyMsg) <> 0) or (_KeyMsg.CharCode = VK_UNKNOWN) then
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
    NotifyApplicationUserInput(Target, _KeyMsg.Msg);
    if (DeliverMessage(_KeyMsg) <> 0) or (_KeyMsg.CharCode = VK_UNKNOWN) then
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

  if (DeliverMessage(_CharMsg) <> 0) or (_CharMsg.CharCode=VK_UNKNOWN) then
    KeyEvHandled;
end;

procedure TLCLCommonCallback.send_LM_KEYDOWN_Message();
begin
  if _KeyMsg.CharCode = VK_UNKNOWN then exit;

  if _IsSysKey then
    _KeyMsg.Msg := LM_SYSKEYDOWN
  else
    _KeyMsg.Msg := LM_KEYDOWN;

  if (DeliverMessage(_KeyMsg) <> 0) or (_KeyMsg.CharCode = VK_UNKNOWN) then
    KeyEvHandled;
end;

procedure TLCLCommonCallback.send_LM_CHAR_Message();
begin
  if not _SendChar then exit;

  if _IsSysKey then
    _CharMsg.Msg := LM_SYSCHAR
  else
    _CharMsg.Msg := LM_CHAR;

  if DeliverMessage(_CharMsg) <> 0 then
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

  if DeliverMessage(_KeyMsg) <> 0 then
  begin
    // the LCL handled the key
    NotifyApplicationUserInput(Target, _KeyMsg.Msg);
    Exit;
  end;
end;

procedure TLCLCommonCallback.KeyEvBefore(Event: NSEvent;
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
  if _IsKeyDown then KeyEvAfterDown(AllowCocoaHandle)
  else KeyEvAfterUp;
end;

procedure TLCLCommonCallback.KeyEvHandled;
begin
  _KeyHandled := True;
end;

function TLCLCommonCallback.IsCocoaOnlyState: Boolean;
begin
  Result := _isCocoaOnlyState;
end;

procedure TLCLCommonCallback.SetCocoaOnlyState( state:Boolean );
begin
  _isCocoaOnlyState := state;
end;

procedure TLCLCommonCallback.SetTabSuppress(ASuppress: Boolean);
begin
  SuppressTabDown := ASuppress;
end;

function TLCLCommonCallback.CanFocus: Boolean;
begin
  Result := not Assigned(Target) or not (csDesigning in Target.ComponentState);
end;

procedure TLCLCommonCallback.MouseClick;
begin
  LCLSendClickedMsg(Target);
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

function TLCLCommonCallback.MouseUpDownEvent(Event: NSEvent; AForceAsMouseUp: Boolean = False; AOverrideBlock: Boolean = False): Boolean;
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
begin
  if Assigned(Owner) and not NSObjectIsLCLEnabled(Owner) then
  begin
    Result := True; // Cocoa should not handle the message.
    Exit;           // LCL should not get the notification either, as the control is disabled.
  end;

  // If LCL control is provided and it's in designing state.
  // The default resolution: Notify LCL about event, but don't let Cocoa
  // do anything with it. (Result=true)
  Result := Assigned(Target) and (csDesigning in Target.ComponentState);

  lCaptureControlCallback := GetCaptureControlCallback();
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
  mc := CocoaWidgetSet.ModalCounter;
  case lEventType of
    NSLeftMouseDown,
    NSRightMouseDown,
    NSOtherMouseDown:
    begin
      Msg.Msg := CheckMouseButtonDownUp(TLCLHandle(Owner),FTarget,LastMouse,
        FTarget.ClientToScreen(Point(Msg.XPos, Msg.YPos)),MButton+1,True);

      case LastMouse.ClickCount of
        2: Msg.Keys := msg.Keys or MK_DOUBLECLICK;
        3: Msg.Keys := msg.Keys or MK_TRIPLECLICK;
        4: Msg.Keys := msg.Keys or MK_QUADCLICK;
      end;

      NotifyApplicationUserInput(Target, Msg.Msg);
      DeliverMessage(Msg);

    end;
    NSLeftMouseUp,
    NSRightMouseUp,
    NSOtherMouseUp:
    begin
      Msg.Msg := CheckMouseButtonDownUp(TLCLHandle(Owner),FTarget,LastMouse,
        FTarget.ClientToScreen(Point(Msg.XPos, Msg.YPos)),MButton+1,False);
      case LastMouse.ClickCount of
        2: Msg.Keys := msg.Keys or MK_DOUBLECLICK;
        3: Msg.Keys := msg.Keys or MK_TRIPLECLICK;
        4: Msg.Keys := msg.Keys or MK_QUADCLICK;
      end;

      NotifyApplicationUserInput(Target, Msg.Msg);
      DeliverMessage(Msg);

      // TODO: Check if Cocoa has special context menu check event
      //       it does (menuForEvent:), but it doesn't work all the time
      //       http://sound-of-silence.com/?article=20150923
      if (GetTarget is TControl) and isContextMenuEvent(Event) then
      begin
        SendContextMenu(Event, menuHandled);
        if menuHandled then Result := true;
      end;
    end;
  end;

  if mc <> CocoaWidgetSet.ModalCounter then
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

function isValidMouseControl( control:TWinControl ): Boolean;
begin
  if (control is TCustomForm) and (csDesigning in control.ComponentState) then
    exit( True );
  Result:= control.Visible and control.Enabled;
end;

function TLCLCommonCallback.MouseMove(Event: NSEvent): Boolean;
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

  if Assigned(Owner) and not NSObjectIsLCLEnabled(Owner) then
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

    callback := GetCaptureControlCallback();
    if callback <> nil then
    begin
      FIsEventRouting:=true;
      Result := callback.MouseMove(Event);
      FIsEventRouting:=false;
      exit;
    end
    else
    begin
      if Event.window<>GetCocoaWindowAtPos(GetScreenPointFromEvent(Event)) then
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

  NotifyApplicationUserInput(Target, Msg.Msg);
  // LCL/LM_MOUSEMOVE always return false, so we should discard return value
  DeliverMessage(Msg);
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

function TLCLCommonCallback.scrollWheel(Event: NSEvent): Boolean;
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
    and not NSObjectIsLCLEnabled(Owner) then
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

  NotifyApplicationUserInput(Target, Msg.Msg);
  Result := DeliverMessage(Msg) <> 0;
end;

procedure TLCLCommonCallback.frameDidChange(sender: id);
begin
  boundsDidChange(sender);
end;

procedure TLCLCommonCallback.boundsDidChange(sender: id);
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

procedure TLCLCommonCallback.BecomeFirstResponder;
begin
  if not Assigned(Target) then Exit;
  // LCL is unable to determine the "already focused" message
  // thus Cocoa related code is doing that.
  //if not Target.Focused then
    LCLSendSetFocusMsg(Target);
end;

procedure TLCLCommonCallback.ResignFirstResponder;
begin
  if not Assigned(Target) then Exit;
  CocoaWidgetSet.KillingFocus:= true;
  try
    LCLSendKillFocusMsg(Target);
  finally
    CocoaWidgetSet.KillingFocus:= false;
  end;
end;

procedure TLCLCommonCallback.DidBecomeKeyNotification;
begin
  if not Assigned(Target) then Exit;
  LCLSendActivateMsg(Target, WA_ACTIVE, false);
  LCLSendSetFocusMsg(Target);
end;

procedure TLCLCommonCallback.DidResignKeyNotification;
begin
  if not Assigned(Target) then Exit;
  LCLSendActivateMsg(Target, WA_INACTIVE, false);
  LCLSendKillFocusMsg(Target);
end;

function TLCLCommonCallback.SendOnEditCut: Boolean;
begin
  Result:= false;
  if Assigned(Target) then
    Result:= SendSimpleMessage(Target, LM_CUT)=0;
end;

function TLCLCommonCallback.SendOnEditPaste: Boolean;
begin
  Result:= false;
  if Assigned(Target) then
    Result:= SendSimpleMessage(Target, LM_PASTE)=0;
end;

procedure TLCLCommonCallback.SendOnChange;
begin
  if not Assigned(Target) then Exit;
  SendSimpleMessage(Target, LM_CHANGED);
end;

procedure TLCLCommonCallback.SendOnTextChanged;
begin
  if not Assigned(Target) then Exit;
  SendSimpleMessage(Target, CM_TEXTCHANGED);
end;

procedure TLCLCommonCallback.scroll(isVert: Boolean; Pos: Integer;
  AScrollPart: NSScrollerPart);
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

function TLCLCommonCallback.DeliverMessage(var Msg): LRESULT;
begin
  if Assigned(Target) then
    Result := LCLMessageGlue.DeliverMessage(Target, Msg)
  else
    Result := 0;
end;

function TLCLCommonCallback.DeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
var
  Message: TLMessage;
begin
  Message.Msg := Msg;
  Message.WParam := WParam;
  Message.LParam := LParam;
  Message.Result := 0;
  Result := DeliverMessage(Message);
end;

procedure TLCLCommonCallback.Draw(ControlContext: NSGraphicsContext;
  const bounds, dirty: NSRect);
var
  PS: TPaintStruct;
  nsr:NSRect;
begin
  // todo: think more about draw call while previous draw still active
  if Assigned(FContext) then
    Exit;
  FContext := TCocoaContext.Create(ControlContext);
  FContext.Control := FTarget;
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
      PS.rcPaint := NSRectToRect(nsr);
      LCLSendPaintMsg(Target, HDC(FContext), @PS);
      if FHasCaret then
        DrawCaret;
    end;
  finally
    FreeAndNil(FContext);
  end;
end;

procedure TLCLCommonCallback.DrawBackground(ctx: NSGraphicsContext; const bounds, dirtyRect: NSRect);
var
  lTarget: TWinControl;
begin
  // Implement Color property
  lTarget := TWinControl(GetTarget());
  if (lTarget.Color <> clDefault) and (lTarget.Color <> clBtnFace)  and (lTarget.Color <> clNone) then
  begin
    ColorToNSColor(ColorToRGB(lTarget.Color)).set_();
    // NSRectFill() always requires the coordinate system of the lower left corner
    // of the origin, contrary to the Rect provided to LCL in
    // TLCLCommonCallback.Draw() and TLCLCommonCallback.DrawOverlay()
    NSRectFill(dirtyRect);
  end;
end;

procedure TLCLCommonCallback.DrawOverlay(ControlContext: NSGraphicsContext;
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
      PS.rcPaint := NSRectToRect(nsr);
      LCLSendPaintMsg(Target, HDC(FContext), @PS);
    end;
  finally
    FreeAndNil(FContext);
  end;
end;

procedure TLCLCommonCallback.RemoveTarget;
begin
  FTarget := nil;
end;

procedure TLCLCommonCallback.InputClientInsertText(const utf8: string);
var
  i : integer;
  c : integer;
  ch : TUTF8Char;
begin
  if (utf8 = '') then Exit;
  i:=1;
  while (i<=length(utf8)) do
  begin
    c := Utf8CodePointLen(@utf8[i], length(utf8)-i+1, false);
    ch := Copy(utf8, i, c);
    FTarget.IntfUTF8KeyPress(ch, 1, false);
    inc(i, c);
  end;

end;

function TLCLCommonCallback.HandleFrame: NSView;
begin
  Result:= _handleFrame;
end;

procedure TLCLCommonCallback.SetHandleFrame(AHandleFrame: NSView);
begin
  _handleFrame:= AHandleFrame;
end;

function TLCLCommonCallback.GetIsOpaque: Boolean;
begin
  Result:= FIsOpaque;
end;

procedure TLCLCommonCallback.SetIsOpaque(AValue: Boolean);
begin
  FIsOpaque:=AValue;
end;

function TLCLCommonCallback.GetShouldBeEnabled: Boolean;
begin
  Result := Assigned(FTarget) and FTarget.Enabled;
end;

{ TLCLFullControlEditCallBack }

{
  Key Step for IME (such as Chinese/Japanese/Korean and DeadKeys)
  1. set _sendChar:=false to avoid KeyDown Event being eaten
     in IntfUTF8KeyPress() or CN_CHAR message.
  2. KeyDown Event will be handled in TCocoaFullControlEdit.keyDown(),
     and NSInputContext.sendEvent() will be called in it,
     and function in NSTextInputClient will be called.
}
procedure TLCLFullControlEditCallback.KeyEvPrepare(Event: NSEvent);
begin
  inherited;
  _sendChar := false;
end;

{ TCocoaWSControl }

class function TCocoaWSControl.GetCanvasScaleFactor(const AControl: TControl
  ): Double;
begin
  if Assigned(AControl.Parent) then
    Result := AControl.Parent.GetCanvasScaleFactor
  else
    Result := 1;
end;

{ TCocoaWSWinControl }

class function TCocoaWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
begin
  Result := TCocoaWSCustomControl.CreateHandle(AWinControl, AParams);
end;

class procedure TCocoaWSWinControl.DestroyHandle(const AWinControl: TWinControl);
var
  obj: NSObject;
  Callback: ICommonCallback;
  CallbackObject: TObject;
begin
  if not AWinControl.HandleAllocated then
    Exit;

  if Assigned(CocoaWidgetSet) and (AWinControl.Handle = CocoaWidgetSet.GetCapture) then
    CocoaWidgetSet.ReleaseCapture;

  obj := NSObject(AWinControl.Handle);
  Callback := obj.lclGetCallback;

  if AWinControl.Focused and Assigned(Callback) then
    Callback.ResignFirstResponder;   // dont' call LCLSendKillFocusMsg
  LCLSendDestroyMsg( AWinControl );

  if obj.isKindOfClass_(NSView) then
  begin
    // no need to "retain" prior to "removeFromSuperview"
    // the original referecnce count with "alloc" is not being released
    // after "addToSuperview"
    NSView(obj).removeFromSuperview;
  end
  else
  if obj.isKindOfClass_(NSWindow) then
    NSWindow(obj).close;

  // destroy the callback
  if Assigned(Callback) then
  begin
    if Callback.HasCaret then DestroyCaret(nil);
    CallbackObject := Callback.GetCallbackObject;
    Callback.RemoveTarget;
    Callback := nil;
    obj.lclClearCallback;
    // Do not free the callback object here. It might be processing an event
    // and is performing a self destruction. Thus there might be a code performing
    // even after DestroyHandle() was called. The destruction needs to be delayed
    // until after the event processing is done
    CocoaWidgetSet.AddToCollect(CallbackObject);
  end;
  obj.release;
end;

class function TCocoaWSWinControl.GetCanvasScaleFactor(const AControl: TControl
  ): Double;
var
  obj: NSObject;
  win: NSWindow;
begin
  win := nil;
  Result := 1;

  if TWinControl(AControl).HandleAllocated then
  begin
    obj := NSObject(TWinControl(AControl).Handle);
    if obj.isKindOfClass_(NSView) then
      win := NSView(obj).window
    else if obj.isKindOfClass_(NSWindow) then
      win := NSWindow(obj);
  end;

  if Assigned(win) then
  begin
    if win.respondsToSelector( ObjCSelector('backingScaleFactor')) then
      Result := win.backingScaleFactor
    else if win.respondsToSelector( ObjCSelector('userSpaceScaleFactor')) then // for older OSX
      Result := win.userSpaceScaleFactor;
  end;
end;

class procedure TCocoaWSWinControl.SetText(const AWinControl: TWinControl; const AText: String);
var
  obj: NSObject;
begin
  if not AWinControl.HandleAllocated then
    Exit;
  obj := NSObject(AWinControl.Handle);
  if obj.isKindOfClass_(NSControl) then
    SetNSControlValue(NSControl(obj), AText);
end;

class function TCocoaWSWinControl.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  obj: NSObject;
begin
  Result := AWinControl.HandleAllocated;
  if not Result then
    Exit;
  obj := NSObject(AWinControl.Handle);
  Result := obj.isKindOfClass_(NSControl);
  if Result then
    AText := GetNSControlValue(NSControl(obj));
end;

class function TCocoaWSWinControl.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  obj: NSObject;
  s: NSString;
begin
  Result := AWinControl.HandleAllocated;
  if not Result then
    Exit;

  obj := NSObject(AWinControl.Handle);
  Result := obj.isKindOfClass_(NSControl);
  if not Result then Exit;

  s := NSControl(obj).stringValue;
  if Assigned(s) then
    ALength := s.length
  else
    ALength := 0
end;

class function TCocoaWSWinControl.GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  Result := AWinControl.HandleAllocated;
  if Result then
    ARect := NSObject(AWinControl.Handle).lclClientFrame;
end;

class function TCocoaWSWinControl.GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  Result:=(AWinControl.Handle<>0);
  if not Result then Exit;
  ARect:=NSObject(AWinControl.Handle).lclClientFrame;
  if (ARect.Left<>0) or (ARect.Top<>0) then
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
end;

class procedure TCocoaWSWinControl.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  lView: NSView;
  Size: NSSize;
  r: TRect;
begin
  if not AWinControl.HandleAllocated then Exit;

  lView := NSObject(AWinControl.Handle).lclContentView;
  if lView = nil then Exit;

  //todo: using fittingSize is wrong - it's based on constraints of the control solely.
  //CocoaWidgetset is not using these constrains. As a result, CocoaComboBox
  //produces wrong size: width 3 and height 26 (or OSX 10.9)
  //as well as SpinEdit itself. The better approach is to use intrinsicContentSize method.
  // Felipe: intrinsicContentSize doesn't give any better results in my tests, it results in even smaller controls
  if lView.respondsToSelector(objcselector('fittingSize')) then // fittingSize is 10.7+
  begin
    Size := lView.fittingSize();
    r := lview.lclGetFrameToLayoutDelta;
    PreferredWidth := Round(Size.width) - r.Left + r.Right;
    PreferredHeight := Round(Size.height) - r.Top + r.Bottom;
  end;
end;

class procedure TCocoaWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  cb : ICommonCallBack;
  r  : TRect;
begin
  if AWinControl.HandleAllocated then
  begin
    {$IFDEF COCOA_DEBUG_SETBOUNDS}
    writeln(Format('TCocoaWSWinControl.SetBounds: %s Bounds=%s',
      [AWinControl.Name, dbgs(Bounds(ALeft, ATop, AWidth, AHeight))]));
    {$ENDIF}
    NSObject(AWinControl.Handle).lclSetFrame(Bounds(ALeft, ATop, AWidth, AHeight));
  end;
end;

function getWinControlAtMouse(): TControl;
begin
  Result:= Application.GetControlAtMouse;
  if not Assigned(Result) then
    exit;

  // find TWinControl (not TGraphicControl)
  // see also TControl.SetTempCursor()
  while not (Result is TWinControl) do
  begin
    Result:= Result.Parent;
    if not Assigned(Result) then
      exit;
  end;
end;

class procedure TCocoaWSWinControl.SetCursor(const AWinControl: TWinControl;
  const ACursor: HCursor);
var
  control: TControl;
  topParent: TControl;
begin
  //debugln('SetCursor '+AWinControl.name+' '+dbgs(ACursor));

  // screen cursor has higher priority than control cursor.
  if Screen.Cursor<>crDefault
    then exit;

  // control cursor only should be set when mouse in the keyWindow or hintWindow.
  // for the MacOS system automatic restore cursor feature(also an issue),
  // it is more appropriate to set the default cursor when the mouse is out
  // of the keyWindow, which has been set in TLCLCommonCallback.MouseMove().
  // the keyWindow here refers to the Client Frame, excluding TitleBar.
  // see also: https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/40515
  topParent:= AWinControl.GetTopParent;
  if (topParent is TCustomForm) and not (topParent is THintWindow) then
  begin
    if NSView(TCustomForm(topParent).handle).window <> NSApp.keyWindow then
      exit;
  end;

  // control cursor only need be set when mouse in AWinControl.
  // suppose there is a Button, which is to set a Cursor of a ListBox.
  // without the code here, it will be set to the Cursor of the ListBox
  // after clicking the Button.
  if not ((AWinControl is TCustomForm) and (csDesigning in AWinControl.ComponentState)) then
  begin
    control:= getWinControlAtMouse;
    if control<>AWinControl then
      exit;
  end;

  CursorHelper.SetNewCursor( TCocoaCursor(ACursor) );
end;

type
  NSFontSetter = objccategory external(NSObject)
    procedure setFont(afont: NSFont); message 'setFont:';
    procedure setTextColor(clr: NSColor); message 'setTextColor:';
  end;

class procedure TCocoaWSWinControl.SetFont(const AWinControl: TWinControl; const AFont: TFont);
var
  Obj: NSObject;
  Cell: NSCell;
  Str: NSAttributedString;
  NewStr: NSMutableAttributedString;
  Range: NSRange;
begin
  if not (AWinControl.HandleAllocated) then Exit;

  Obj := NSObject(AWinControl.Handle).lclContentView;

  if Obj.respondsToSelector(ObjCSelector('setFont:')) then
    Obj.setFont(TCocoaFont(AFont.Reference.Handle).Font);

  if Obj.respondsToSelector(ObjCSelector('setTextColor:')) then
  begin
    if AFont.Color = clDefault then
      Obj.setTextColor(NSColor.controlTextColor)
    else
      Obj.setTextColor(ColorToNSColor(ColorToRGB(AFont.Color)));
  end;
end;

class procedure TCocoaWSWinControl.SetColor(const AWinControl: TWinControl);
begin
  invalidate(AWinControl);
end;

function indexInList(ctrl: id; l: TFPList): integer;
var
 i : integer;
begin
  for i:=0 to l.Count-1 do
    if PtrUInt(TWinControl(l[i]).Handle)=PtrUInt(ctrl) then
    begin
      Result:=i;
      exit;
    end;
  Result:=-1;
end;

function SortHandles(param1: id; param2: id; param3: Pointer): NSComparisonResult; cdecl;
var
  i1,i2: integer;
begin
  i1:=indexInList(param1, TFPList(param3));
  i2:=indexInList(param2, TFPList(param3));
  if i1<i2 then Result:=NSOrderedDescending
  else if i1>i2 then Result:=NSOrderedAscending
  else Result:=NSOrderedSame;
end;

class procedure TCocoaWSWinControl.SetChildZPosition(const AWinControl,
  AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList
  );
var
  pr : NSView;
begin
  if (not AWinControl.HandleAllocated) or (not AChild.HandleAllocated) then Exit;

  pr := NSView(AWinControl.Handle).lclContentView;

  if pr.subviews.count <= 1 then exit;

  // 1. sorting is a better option than removing / adding a view.
  //    whenever a focused (firstrepsonder view) is removed and added to front,
  //    focus is lost. the issue was exposed by
  //    https://gitlab.com/freepascal.org/lazarus/lazarus/-/commit/853461fed65fed2ce1614bcc06eca5b880810f0e
  //    <LCL: fixed TWinControl.SetChildZPosition, WS must be informed about change of order in any case. issue #40450>
  // 2. regarding performance, though on every comparison an index must be
  //    searched withing a list, but performance still does not need to be
  //    paid too much attention to.
  // 3. it's because SetChildZPosition() rarely needs to be called at runtime,
  //    and usually there are only a few sibling controls.
  pr.sortSubviewsUsingFunction_context(@SortHandles, AChildren);
end;

class procedure TCocoaWSWinControl.ShowHide(const AWinControl: TWinControl);
var
  lShow: Boolean;
begin
  //WriteLn(Format('[TCocoaWSWinControl.ShowHide] AWinControl=%s %s', [AWinControl.Name, AWinControl.ClassName]));
  if AWinControl.HandleAllocated then
  begin
    lShow := AWinControl.HandleObjectShouldBeVisible;

    NSObject(AWinControl.Handle).lclSetVisible(lShow);
  end;
end;

class procedure TCocoaWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
  if AWinControl.HandleAllocated then
     NSObject(AWinControl.Handle).lclInvalidate;
end;

class procedure TCocoaWSWinControl.PaintTo(const AWinControl: TWinControl;
  ADC: HDC; X, Y: Integer);
var
  bc : TCocoaBitmapContext;
  v  : NSView;
  b  : NSBitmapImageRep;
  obj : NSObject;
  f  : NSRect;
begin
  if not (TObject(ADC) is TCocoaBitmapContext) then Exit;
  if not NSObject(AWinControl.Handle).isKindOfClass(NSView) then Exit;
  bc := TCocoaBitmapContext(ADC);
  v := NSView(AWinControl.Handle);
  f := v.frame;
  f.origin.x := 0;
  f.origin.y := 0;

  b := v.bitmapImageRepForCachingDisplayInRect(f);

  v.cacheDisplayInRect_toBitmapImageRep(f, b);
  bc.DrawImageRep(
    NSMakeRect(0,0, f.size.width, f.size.height),
    f, b);
end;

{ TCocoaWSCustomControl }

function SendIMCompostionMessage(
  const control: TWinControl; const WParam: LclType.WPARAM ): PtrInt;
var
  Mess : TLMessage;
begin
  FillChar(Mess,SizeOf(Mess),0);
  Mess.Msg:= LM_IM_COMPOSITION;
  Mess.WParam:= WParam;
  Result:= DeliverMessage( control,  Mess );
end;

// get IMEHandler by LM_IM_COMPOSITION message
function getControlIMEHandler(const control: TWinControl): ICocoaIMEControl;
var
  handle : PtrInt;
begin
  handle := SendIMCompostionMessage( control, IM_MESSAGE_WPARAM_GET_IME_HANDLER );
  Result := TObject(handle) as ICocoaIMEControl;
end;

// get Lookup Word Handler by LM_IM_COMPOSITION message
function getControlLWHandler(const control: TWinControl): ICocoaLookupWord;
var
  handle: PtrInt;
begin
  Result:= nil;
  handle := SendIMCompostionMessage( control, IM_MESSAGE_WPARAM_GET_LW_HANDLER );
  if TObject(handle) is ICocoaLookupWord then
    Result:= TObject(handle) as ICocoaLookupWord;
end;

class function TCocoaWSCustomControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  ctrl : TCocoaCustomControl;
  sl   : TCocoaManualScrollView;
  hs   : TCocoaManualScrollHost;
  lcl  : TLCLCommonCallback;
  imeHandler : ICocoaIMEControl;
begin
  imeHandler := getControlIMEHandler(AWinControl);
  if Assigned(imeHandler) then
  begin
    // AWinControl implements ICocoaIMEControl
    // AWinControl is a Full Control Edit (such as SynEdit/ATSynEdit)
    ctrl := TCocoaFullControlEdit.alloc.lclInitWithCreateParams(AParams);
    lcl := TLCLFullControlEditCallback.Create(ctrl, AWinControl);
    TCocoaFullControlEdit(ctrl).imeHandler:= imeHandler;
    TCocoaFullControlEdit(ctrl).lwHandler:= getControlLWHandler(AWinControl);
  end
  else
  begin
    // AWinControl not implements ICocoaIMEControl
    // AWinControl is a normal Custom Control
    ctrl := TCocoaCustomControlWithBaseInputClient.alloc.lclInitWithCreateParams(AParams);
    lcl := TLCLCommonCallback.Create(ctrl, AWinControl);
  end;
  lcl.BlockCocoaUpDown := true;
  lcl.BlockCocoaKeyBeep := true; // prevent "dings" on keyDown for custom controls (i.e. SynEdit)
  ctrl.callback := lcl;

  sl := EmbedInManualScrollView(ctrl);
  sl.callback := ctrl.callback;

  hs := EmbedInManualScrollHost(sl);
  hs.callback := ctrl.callback;
  lcl.SetHandleFrame(hs);

  ScrollViewSetBorderStyle(hs, TCustomControl(AWinControl).BorderStyle );

  Result := TLCLHandle(hs);
end;

class procedure TCocoaWSCustomControl.SetBorderStyle(
  const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
begin
  if not Assigned(AWinControl) or not (AWinControl.HandleAllocated) then Exit;
  ScrollViewSetBorderStyle(  TCocoaManualScrollHost(AWinControl.Handle), ABorderStyle );
end;

function NSObjectDebugStr(obj: NSObject): string;
begin
  Result := IntToStr(PtrUInt(obj));
  if Assigned(obj) then
    Result := Result +' '+obj.lclClassName+' lcl: '+CallbackDebugStr(obj.lclGetCallback);
end;

function CallbackDebugStr(cb: ICommonCallback): string;
var
  trg : TObject;
begin
  Result := IntToStr(PtrUInt(cb));
  if Assigned(cb) then
  begin
    trg := cb.GetTarget;
    Result := Result + ' trg: '+IntToStr(PtrUInt(trg));
    if Assigned(trg) then
    begin
      Result := Result + ' '+trg.ClassName;
      if trg is TWinControl then
        Result := Result +' '+TWinControl(trg).Name;
    end;
  end;
end;

procedure DebugDumpParents(fromView: NSView);
begin
  while Assigned(fromView) do begin
    writeln(fromView.lclClassName);
    fromView := fromView.superView;
  end;
end;

procedure ScrollViewSetBorderStyle(sv: NSScrollView; astyle: TBorderStyle);
const
  NSBorderStyle : array [TBorderStyle] of NSBorderType = (
    NSNoBorder,   // bsNone
    NSBezelBorder // bsSingle     (NSLineBorder is too thick)
  );
begin
  if not Assigned(sv) then Exit;
  sv.setBorderType( NSBorderStyle[astyle] );
end;

end.

