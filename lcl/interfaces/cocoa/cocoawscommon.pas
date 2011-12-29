unit CocoaWSCommon;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Types,
  CocoaAll,
  Classes, Controls, SysUtils,
  //
  WSControls, LCLType, LMessages, LCLProc, Graphics, Forms,
  CocoaPrivate, CocoaGDIObjects, CocoaCaret, CocoaUtils, LCLMessageGlue;

type

  { LCLWSViewExtension }

  LCLWSViewExtension = objccategory(NSView)
    function lclInitWithCreateParams(const AParams: TCreateParams): id; message 'lclInitWithCreateParams:';
  end;

  { TLCLCommonCallback }

  TLCLCommonCallback = class(TObject, ICommonCallBack)
  private
    FPropStorage: TStringList;
    FContext: TCocoaContext;
    FHasCaret: Boolean;
    function GetHasCaret: Boolean;
    procedure SetHasCaret(AValue: Boolean);
  protected
    class function CocoaModifiersToKeyState(AModifiers: NSUInteger): PtrInt; static;
    class function CocoaPressedMouseButtonsToKeyState(AMouseButtons: NSUInteger): PtrInt; static;
    procedure OffsetMousePos(var Point: NSPoint);
  public
    Owner: NSObject;
    Target: TWinControl;
    constructor Create(AOwner: NSObject; ATarget: TWinControl); virtual;
    destructor Destroy; override;
    function GetPropStorage: TStringList;
    function GetContext: TCocoaContext;
    function GetTarget: TObject;
    function MouseUpDownEvent(Event: NSEvent): Boolean; virtual;
    procedure MouseClick; virtual;
    procedure MouseMove(x,y: Integer); virtual;
    procedure frameDidChange; virtual;
    procedure boundsDidChange; virtual;
    procedure BecomeFirstResponder; virtual;
    procedure ResignFirstResponder; virtual;
    function DeliverMessage(var Msg): LRESULT; virtual; overload;
    function DeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult; virtual; overload;
    procedure Draw(ControlContext: NSGraphicsContext; const bounds, dirty: NSRect); virtual;
    function ResetCursorRects: Boolean; virtual;

    property HasCaret: Boolean read GetHasCaret write SetHasCaret;
  end;

  TLCLCommonCallBackClass = class of TLCLCommonCallBack;

  { TCocoaWSWinControl }

  TCocoaWSWinControl = class(TWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;

    class function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
  end;


  { TLCLCustomControlCallback }

  TLCLCustomControlCallback = class(TLCLCommonCallback)
  public
    function MouseUpDownEvent(Event: NSEvent): Boolean; override;
  end;

  { TCocoaWSCustomControl }

  TCocoaWSCustomControl = class(TWSCustomControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  TLastMouseInfo = record
    Owner: NSObject;
    MousePos: NSPoint;
    TimeStamp: NSTimeInterval;
    ClickCount: Integer;
  end;

var
  LastMouse: TLastMouseInfo = (Owner: nil; MousePos: (x:0; y:0); TimeStamp:0; ClickCount: 0);
const
  DblClickThreshold = 3;// max Movement between two clicks of a DblClick

// Utility WS functions

function AllocCustomControl(const AWinControl: TWinControl): TCocoaCustomControl;
function EmbedInScrollView(AView: NSView): TCocoaScrollView;
procedure SetViewDefaults(AView: NSView);

implementation

uses
  CocoaInt;

function AllocCustomControl(const AWinControl: TWinControl): TCocoaCustomControl;
begin
  if not Assigned(AWinControl) then
    Exit(nil);
  Result := TCocoaCustomControl(TCocoaCustomControl.alloc).init;
  Result.callback := TLCLCommonCallback.Create(Result, AWinControl);
end;

function EmbedInScrollView(AView: NSView): TCocoaScrollView;
var
  r: TRect;
  p: NSView;
begin
  if not Assigned(AView) then
    Exit(nil);
  r := AView.lclFrame;
  p := AView.superview;
  Result := TCocoaScrollView.alloc.initWithFrame(NSNullRect);
  if Assigned(p) then p.addSubView(Result);
  Result.lclSetFrame(r);
  Result.setDocumentView(AView);
  SetViewDefaults(Result);
end;

procedure SetViewDefaults(AView:NSView);
begin
  if not Assigned(AView) then Exit;
  AView.setAutoresizingMask(NSViewMinYMargin or NSViewMaxXMargin);
end;

{ TLCLCustomControlCallback }

function TLCLCustomControlCallback.MouseUpDownEvent(Event: NSEvent): Boolean;
begin
  inherited MouseUpDownEvent(Event);
  Result := True;
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

class function TLCLCommonCallback.CocoaModifiersToKeyState(AModifiers: NSUInteger): PtrInt;
begin
  Result := 0;
  if AModifiers and NSShiftKeyMask <> 0 then
    Result := Result or MK_SHIFT;
  if AModifiers and NSControlKeyMask <> 0 then
    Result := Result or MK_CONTROL;
  if AModifiers and NSAlternateKeyMask <> 0 then
    Result := Result or $20000000;
end;

class function TLCLCommonCallback.CocoaPressedMouseButtonsToKeyState(AMouseButtons: NSUInteger): PtrInt;
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

procedure TLCLCommonCallback.OffsetMousePos(var Point: NSPoint);
begin
  if Owner.isKindOfClass(NSWindow) then
    Point.y := NSWindow(Owner).contentView.bounds.size.height - Point.y
  else
  if Owner.isKindOfClass(NSView) then
  begin
    Point := NSView(Owner).convertPoint_fromView(Point, nil);
    Point.y := NSView(Owner).bounds.size.height - Point.y;
  end;
end;

constructor TLCLCommonCallback.Create(AOwner: NSObject; ATarget: TWinControl);
begin
  inherited Create;
  Owner := AOwner;
  Target := ATarget;
  FContext := nil;
  FHasCaret := False;
  FPropStorage := TStringList.Create;
  FPropStorage.Sorted := True;
  FPropStorage.Duplicates := dupAccept;
end;

destructor TLCLCommonCallback.Destroy;
begin
  FContext.Free;
  FPropStorage.Free;
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

function TLCLCommonCallback.MouseUpDownEvent(Event: NSEvent): Boolean;
const
  // array of clickcount x buttontype
  MSGKIND: array[0..3, 1..4] of Integer =
  (
    (LM_LBUTTONDOWN, LM_LBUTTONDBLCLK, LM_LBUTTONTRIPLECLK, LM_LBUTTONQUADCLK),
    (LM_RBUTTONDOWN, LM_RBUTTONDBLCLK, LM_RBUTTONTRIPLECLK, LM_RBUTTONQUADCLK),
    (LM_MBUTTONDOWN, LM_MBUTTONDBLCLK, LM_MBUTTONTRIPLECLK, LM_MBUTTONQUADCLK),
    (LM_XBUTTONDOWN, LM_XBUTTONDBLCLK, LM_XBUTTONTRIPLECLK, LM_XBUTTONQUADCLK)
  );
  MSGKINDUP: array[0..3] of Integer = (LM_LBUTTONUP, LM_RBUTTONUP, LM_MBUTTONUP, LM_XBUTTONUP);

var
  Msg: TLMMouse;
  MousePos: NSPoint;
  MButton: NSInteger;

  function CheckMouseButtonDown(AButton: Integer): Cardinal;
  begin
    LastMouse.ClickCount := Event.clickCount;
    if LastMouse.clickCount > 4 then
      LastMouse.clickCount := 1;
    LastMouse.TimeStamp := Event.timestamp;
    LastMouse.MousePos := MousePos;
    LastMouse.Owner := Owner;

    Result := MSGKIND[AButton][LastMouse.ClickCount];
  end;
begin
  Result := False; // allow cocoa to handle message

  if Assigned(Target) and (not (csDesigning in Target.ComponentState) and not Owner.lclIsEnabled) then
    Exit;

  // idea of multi click implementation is taken from gtk

  FillChar(Msg, SizeOf(Msg), #0);

  MousePos := Event.locationInWindow;
  OffsetMousePos(MousePos);

  Msg.Keys := CocoaModifiersToKeyState(Event.modifierFlags) or CocoaPressedMouseButtonsToKeyState(Event.pressedMouseButtons);

  Msg.XPos := Round(MousePos.X);
  Msg.YPos := Round(MousePos.Y);

  MButton := event.buttonNumber;
  if MButton >= 3 then
  begin
    // high word of XButton messages indicate the X button which is pressed
    Msg.Keys := Msg.Keys or (MButton - 2) shl 16;
    MButton := 3;
  end;


  case Event.type_ of
    NSLeftMouseDown,
    NSRightMouseDown,
    NSOtherMouseDown:
    begin
      Msg.Msg := CheckMouseButtonDown(MButton);

      NotifyApplicationUserInput(Msg.Msg);
      Result := DeliverMessage(Msg) <> 0;
    end;
    NSLeftMouseUp,
    NSRightMouseUp,
    NSOtherMouseUp:
    begin
      LastMouse.Owner := Owner;
      LastMouse.MousePos := MousePos;
      Msg.Msg := MSGKINDUP[MButton];

      NotifyApplicationUserInput(Msg.Msg);
      Result := DeliverMessage(Msg) <> 0;
    end;
  end;
end;

procedure TLCLCommonCallback.MouseClick;
begin
  LCLSendClickedMsg(Target);
end;

procedure TLCLCommonCallback.MouseMove(x, y: Integer);
begin
  LCLSendMouseMoveMsg(Target, x,y, []);
end;

procedure TLCLCommonCallback.frameDidChange;
begin
  boundsDidChange;
end;

procedure TLCLCommonCallback.boundsDidChange;
var
  NewBounds, OldBounds: TRect;
  PosMsg: TLMWindowPosChanged;
  Resized, Moved, ClientResized: Boolean;
  SizeType: Integer;
begin
  NewBounds := Owner.lclFrame;

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

  Resized :=
    (OldBounds.Right - OldBounds.Left <> NewBounds.Right - NewBounds.Left) or
    (OldBounds.Bottom - OldBounds.Top <> NewBounds.Bottom - NewBounds.Top);
  Moved :=
    (OldBounds.Left <> NewBounds.Left) or
    (OldBounds.Top <> NewBounds.Top);

  ClientResized := False;

  // update client rect
  if Resized or Target.ClientRectNeedsInterfaceUpdate then
  begin
    Target.InvalidateClientRectCache(False);
    ClientResized := True;
  end;

  // then send a LM_SIZE message
  if Resized or ClientResized then
  begin
    LCLSendSizeMsg(Target, NewBounds.Right - NewBounds.Left,
      NewBounds.Bottom - NewBounds.Top, Owner.lclWindowState, True);
  end;

  // then send a LM_MOVE message
  if Moved then
  begin
    LCLSendMoveMsg(Target, NewBounds.Left,
      NewBounds.Top, Move_SourceIsInterface);
  end;
end;

procedure TLCLCommonCallback.BecomeFirstResponder;
begin
  LCLSendSetFocusMsg(Target);
end;

procedure TLCLCommonCallback.ResignFirstResponder;
begin
  LCLSendKillFocusMsg(Target);
end;

function TLCLCommonCallback.DeliverMessage(var Msg): LRESULT;
begin
  Result := LCLMessageGlue.DeliverMessage(Target, Msg);
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
  struct: TPaintStruct;
begin
  // todo: think more about draw call while previous draw still active
  if Assigned(FContext) then
    Exit;
  FContext := TCocoaContext.Create;
  try
    FContext.ctx := ControlContext;
    if FContext.InitDraw(Round(bounds.size.width), Round(bounds.size.height)) then
    begin
      FillChar(struct, SizeOf(TPaintStruct), 0);
      struct.hdc := HDC(FContext);
      NSToLCLRect(dirty, struct.rcPaint);
      LCLSendPaintMsg(Target, HDC(FContext), @struct);
      if FHasCaret then
        DrawCaret;
    end;
  finally
    FreeAndNil(FContext);
  end;
end;

function TLCLCommonCallback.ResetCursorRects: Boolean;
var
  ACursor: TCursor;
  View: NSView;
begin
  Result := False;
  if Owner.isKindOfClass_(NSWindow) then
    View := NSwindow(Owner).contentView
  else
  if Owner.isKindOfClass_(NSView) then
    View := NSView(Owner)
  else
    Exit;
  if not (csDesigning in Target.ComponentState) then
  begin
    ACursor := Screen.Cursor;
    if ACursor = crDefault then
    begin
      // traverse visible child controls
      ACursor := Target.Cursor;
    end;
    Result := ACursor <> crDefault;
    if Result then
      View.addCursorRect_cursor(View.visibleRect, TCocoaCursor(Screen.Cursors[ACursor]).Cursor);
  end;
end;

{ TCocoaWSWinControl }

class function TCocoaWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TCocoaWSCustomControl.CreateHandle(AWinControl, AParams);
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
  Result:=(AWinControl.Handle<>0);
  if not Result then Exit;
  ARect:=NSObject(AWinControl.Handle).lclClientFrame;
end;

class function TCocoaWSWinControl.GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  Result:=(AWinControl.Handle<>0);
  if not Result then Exit;
  ARect:=NSObject(AWinControl.Handle).lclClientFrame;
end;

class procedure TCocoaWSWinControl.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  Obj: NSObject;
  Size: NSSize;
begin
  if (AWinControl.Handle <> 0) then
  begin
    Obj := NSObject(AWinControl.Handle);
{
    if Obj.isKindOfClass_(NSView) and obj.respondsToSelector(objcselector('fittingSize')) then
    begin
      Size := NSView(Obj).fittingSize;
      PreferredWidth := Round(Size.width);
      PreferredHeight := Round(Size.height);
    end;
}
  end;
end;

class procedure TCocoaWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (AWinControl.Handle<>0) then
    NSObject(AWinControl.Handle).lclSetFrame(Bounds(ALeft, ATop, AWidth, AHeight));
end;

class procedure TCocoaWSWinControl.SetCursor(const AWinControl: TWinControl;
  const ACursor: HCursor);
var
  Obj: NSObject;
begin
  if (AWinControl.Handle <> 0) then
  begin
    Obj := NSObject(AWinControl.Handle);
    if Obj.isKindOfClass_(NSWindow) then
      NSWindow(Obj).resetCursorRects
    else
    if Obj.isKindOfClass_(NSView) then
      NSView(Obj).resetCursorRects;
  end;
end;

class procedure TCocoaWSWinControl.SetFont(const AWinControl: TWinControl; const AFont: TFont);
var
  Obj: NSObject;
  Cell: NSCell;
  Str: NSAttributedString;
  NewStr: NSMutableAttributedString;
  Dict: NSDictionary;
  Range: NSRange;
begin
  if (AWinControl.HandleAllocated) then
  begin
    Obj := NSObject(AWinControl.Handle);
    if Obj.isKindOfClass(NSScrollView) then
      Obj := NSScrollView(Obj).documentView;
    if Obj.isKindOfClass(NSControl) then
    begin
      Cell := NSCell(NSControl(Obj).cell);
      Cell.setFont(TCocoaFont(AFont.Reference.Handle).Font);
      // try to assign foreground color?
      Str := Cell.attributedStringValue;
      if Assigned(Str) then
      begin
        NewStr := NSMutableAttributedString.alloc.initWithAttributedString(Str);
        Range.location := 0;
        Range.length := NewStr.length;
        if AFont.Color = clDefault then
          NewStr.removeAttribute_range(NSForegroundColorAttributeName, Range)
        else
          NewStr.addAttribute_value_range(NSForegroundColorAttributeName, ColorToNSColor(ColorToRGB(AFont.Color)), Range);
        Cell.setAttributedStringValue(NewStr);
        NewStr.release;
      end;
    end
    else
    if Obj.isKindOfClass(NSText) then
    begin
      NSText(Obj).setFont(TCocoaFont(AFont.Reference.Handle).Font);
      if AFont.Color = clDefault then
        NSText(Obj).setTextColor(nil)
      else
        NSText(Obj).setTextColor(ColorToNSColor(ColorToRGB(AFont.Color)));
    end;
  end;
end;

{ TCocoaWSCustomControl }

class function TCocoaWSCustomControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  ctrl: TCocoaCustomControl;
begin
  ctrl := TCocoaCustomControl(NSView(TCocoaCustomControl.alloc).lclInitWithCreateParams(AParams));
  ctrl.callback := TLCLCustomControlCallback.Create(ctrl, AWinControl);
  Result := TLCLIntfHandle(ctrl);
end;

{ LCLWSViewExtension }

function LCLWSViewExtension.lclInitWithCreateParams(const AParams:TCreateParams): id;
var
  p: NSView;
  ns: NSRect;
begin
  p:=nil;
  if (AParams.WndParent<>0) then begin
    if (NSObject(AParams.WndParent).isKindOfClass_(NSView)) then
      p:=NSView(AParams.WndParent)
    else if (NSObject(AParams.WndParent).isKindOfClass_(NSWindow)) then
      p:=NSWindow(AParams.WndParent).contentView;
  end;
  with AParams do
    if Assigned(p)
      then LCLToNSRect(Types.Bounds(X,Y,Width, Height), p.frame.size.height, ns)
      else LCLToNSRect(Types.Bounds(X,Y,Width, Height), ns);

  Result:=initWithFrame(ns);
  if not Assigned(Result) then Exit;

  if Assigned(p) then p.addSubview(Self);
  SetViewDefaults(Self);
end;

end.

