{ $Id: $}
{                  --------------------------------------------
                  cocoaprivate.pp  -  Cocoa internal classes
                  --------------------------------------------

 This unit contains the private classhierarchy for the Cocoa implemetations

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaPrivate;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}
{$include cocoadefines.inc}

{.$DEFINE COCOA_DEBUG_SETBOUNDS}

interface

uses
  Types, Classes, SysUtils, LCLType, Controls, Forms, Graphics,
  LazUTF8,
  MacOSAll, CocoaAll,
  CocoaGDIObjects, CocoaCallback, CocoaCursor, CocoaConfig, Cocoa_Extra, CocoaUtils;

type

  { TCocoaViewUtil }

  TCocoaViewUtil = class
    class function isLCLEnabled(v: NSView): Boolean;
    class function isLCLEnabled(obj: NSObject): Boolean;
    class function canLCLFocus(v: NSView): Boolean;
    class function getSuperViewHeight(const view: NSView): CGFloat;
    class procedure hideAllSubviews( const parent: NSView );
    class procedure addLayoutDelta(const layout: TRect; var frame: TRect);
    class procedure subLayoutDelta(const layout: TRect; var frame: TRect);
    class procedure setDefaultMargin(const AView: NSView);
    class procedure updateFocusRing(
      const cocoaControl: NSView;
      const lclControl: TWinControl );
    class procedure setSize(
      const ctrl: NSView;
      const newHeight, miniHeight, smallHeight: Integer;
      const AutoChangeFont: Boolean );
    class procedure drawBackground(
      const view: NSView;
      const lclBrush: TBrush );
    class procedure lclOffsetWithEnclosingScrollView(
      const view: NSView;
      var x: Integer;
      var y: Integer );
  end;

  { TCocoaControlUtil }

  TCocoaControlUtil = class
  public
    class procedure setStringValue(const c: NSControl; const S: String); inline;
    class procedure setStringValueAndSendEvent(const ctrl: NSControl; const text: String);
    class function getStringValue(const c: NSControl): String; inline;
    class function toMacOSTitle(const ATitle: String): NSString;
    class procedure moveCaretToTheEnd(const c: NSControl);
  end;

  // Some components might be using CocoaPrivate for use of LCLObjCBoolean
  // Thus this declaration needs to be here.
  LCLObjCBoolean = cocoa_extra.LCLObjCBoolean;

  { LCLObjectExtension }

  LCLObjectExtension = objccategory(NSObject)
    function lclIsEnabled: Boolean; message 'lclIsEnabled';
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:';
    function lclIsVisible: Boolean; message 'lclIsVisible';
    procedure lclSetVisible(AVisible: Boolean); message 'lclSetVisible:';
    function lclWindowState: Integer; message 'lclWindowState';

    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:';
    procedure lclInvalidate; message 'lclInvalidate';
    procedure lclUpdate; message 'lclUpdate';

    // Returns the position of the view or window, in the immediate
    // parent (view or screen), relative to its client coordinates system
    // Left and Top are always returned in LCL coordinate system.
    procedure lclLocalToScreen(var X, Y: Integer); message 'lclLocalToScreen::';
    procedure lclScreenToLocal(var X, Y: Integer); message 'lclScreenToLocal::';
    function lclParent: id; message 'lclParent';
    function lclFrame: TRect; message 'lclFrame';
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:';

    // returns rectangle describing deltas to get "Layout" rectangle from "Frame" rectangle
    //   left, top  - return offsets from top-left corner of the control (not reversed as in Cocoa coordinates)
    //                    (values are typically positive)
    //   right, bottom -  offsets for bottom-right corner
    //                    (typically negative)
    function lclGetFrameToLayoutDelta: TRect; message 'lclGetFrameToLayoutDelta';

    function lclClientFrame: TRect; message 'lclClientFrame';
    function lclGetCallback: ICommonCallback; message 'lclGetCallback';
    procedure lclClearCallback; message 'lclClearCallback';
    function lclGetPropStorage: TStringList; message 'lclGetPropStorage';
    function lclGetTarget: TObject; message 'lclGetTarget';
    function lclDeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult; message 'lclDeliverMessage:::';
    function lclContentView: NSView; message 'lclContentView';
    procedure lclOffsetMousePos(var Point: NSPoint); message 'lclOffsetMousePos:';
    procedure lclExpectedKeys(var wantTabs, wantArrows, wantReturn, wantAll: Boolean); message 'lclExpectedKeys::::';
    function lclIsMouseInAuxArea(Event: NSEvent): Boolean; message 'lclMouseInAuxArea:';
  end;

  { LCLViewExtension }

  LCLViewExtension = objccategory(NSView)
    function lclInitWithCreateParams(const AParams: TCreateParams): id; message 'lclInitWithCreateParams:';
    function lclIsEnabled: Boolean; message 'lclIsEnabled'; reintroduce;
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:'; reintroduce;

    function lclIsVisible: Boolean; message 'lclIsVisible'; reintroduce;
    procedure lclSetVisible(AVisible: Boolean); message 'lclSetVisible:'; reintroduce;
    function lclIsPainting: Boolean; message 'lclIsPainting';
    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:'; reintroduce;
    procedure lclInvalidate; message 'lclInvalidate'; reintroduce;
    procedure lclUpdate; message 'lclUpdate'; reintroduce;
    procedure lclLocalToScreen(var X, Y: Integer); message 'lclLocalToScreen::'; reintroduce;
    procedure lclScreenToLocal(var X, Y: Integer); message 'lclScreenToLocal::'; reintroduce;
    function lclParent: id; message 'lclParent'; reintroduce;
    function lclFrame: TRect; message 'lclFrame'; reintroduce;
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:'; reintroduce;
    function lclClientFrame: TRect; message 'lclClientFrame'; reintroduce;
    function lclContentView: NSView; message 'lclContentView'; reintroduce;
    procedure lclOffsetMousePos(var Point: NSPoint); message 'lclOffsetMousePos:'; reintroduce;
  end;

  { LCLControlExtension }

  LCLControlExtension = objccategory(NSControl)
    function lclIsEnabled: Boolean; message 'lclIsEnabled'; reintroduce;
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:'; reintroduce;
  end;

var
  // todo: this should be a threadvar
  TrackedControl : NSObject = nil;

  {$ifdef COCOALOOPHIJACK}
  // The flag is set to true once hi-jacked loop is finished (at the end of app)
  // The flag is checked in Menus to avoid "double" Cmd+Q menu
  LoopHiJackEnded : Boolean = false;
  {$endif}

implementation

type
  TWinControlAccess = class(TWinControl);

class function TCocoaViewUtil.isLCLEnabled(obj: NSObject): Boolean;
begin
  if obj.isKindOfClass(NSView) then
    Result := isLCLEnabled(NSView(obj))
  else
    Result := obj.lclIsEnabled;
end;

class function TCocoaViewUtil.isLCLEnabled(v: NSView): Boolean;
begin
  Result := true;
  while Assigned(v) do
  begin
    if not v.lclIsEnabled then begin
      Result := false;
      break;
    end;
    v:=v.superview;
  end;
end;

class function TCocoaViewUtil.canLCLFocus(v: NSView): Boolean;
var
  cb: ICommonCallback;
begin
  if Assigned(v) then
  begin
    cb := v.lclGetCallback;
    if Assigned(cb) then
      Result := cb.CanFocus
    else
      Result := true;
  end
  else
    Result := false;
end;

class function TCocoaViewUtil.getSuperViewHeight(const view: NSView
  ): CGFloat;
begin
  Result := -1;
  if not Assigned(view) then Exit;
  if not Assigned(view.superview) then Exit;
  //if view.superview.isKindOfClass_(TCocoaTabPageView) then
    //Result := TCocoaTabPageView(view.superview).tabview.contentRect.size.height
  //else
    Result := view.superview.frame.size.height;
  {$IFDEF COCOA_SUPERVIEW_HEIGHT}
  WriteLn(Format('GetNSViewSuperViewHeight Result=%f', [Result]));
  {$ENDIF}
end;

class procedure TCocoaViewUtil.hideAllSubviews( const parent: NSView );
var
  view: NSView;
begin
  for view in parent.subviews do
    view.setHidden( True );
end;

class procedure TCocoaViewUtil.addLayoutDelta(const layout: TRect; var frame: TRect);
begin
  inc(frame.Left, layout.Left);
  inc(frame.Top, layout.Top);
  inc(frame.Right, layout.Right);
  inc(frame.Bottom, layout.Bottom);
end;

class procedure TCocoaViewUtil.subLayoutDelta(const layout: TRect; var frame: TRect);
begin
  dec(frame.Left, layout.Left);
  dec(frame.Top, layout.Top);
  dec(frame.Right, layout.Right);
  dec(frame.Bottom, layout.Bottom);
end;

class procedure TCocoaViewUtil.setDefaultMargin(const AView: NSView);
var
  mask: NSUInteger;
begin
  if not Assigned(AView) then Exit;
  if Assigned(AView.superview) and AView.superview.isFlipped then
    mask:= NSViewMaxYMargin or NSViewMaxXMargin
  else
    mask:= NSViewMinYMargin or NSViewMaxXMargin;
  AView.setAutoresizingMask(mask);
end;

class procedure TCocoaViewUtil.updateFocusRing(
  const cocoaControl: NSView;
  const lclControl: TWinControl );
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

class procedure TCocoaViewUtil.setSize(
  const ctrl: NSView;
  const newHeight, miniHeight, smallHeight: Integer;
  const AutoChangeFont: Boolean);
var
  sz : NSControlSize;
begin
  if (miniHeight>0) and (newHeight<=miniHeight) then
    sz:=NSMiniControlSize
  else if (smallHeight>0) and (newHeight<=smallHeight) then
    sz:=NSSmallControlSize
  else
    sz:=NSRegularControlSize;

  if ctrl.respondsToSelector(ObjCSelector('setControlSize:')) then
    ctrl.setControlSize(sz)
  else if ctrl.respondsToSelector(ObjCSelector('cell')) then
  begin
    if NSCell(ctrl.cell).controlSize<>sz then
        NSCell(ctrl.cell).setControlSize(sz);
  end;
  if AutoChangeFont and (ctrl.respondsToSelector(ObjCSelector('setFont:'))) then
    ctrl.setFont(NSFont.systemFontOfSize(NSFont.systemFontSizeForControlSize(sz)));
end;

class procedure TCocoaViewUtil.drawBackground(
  const view: NSView;
  const lclBrush: TBrush );
var
  ctx: TCocoaContext;
  cocoaBrush: TCocoaBrush;
  width: Integer;
  height: Integer;
begin
  if lclBrush.Color = clWhite then   // see also TBrush.create
    Exit;

  width:= Round( view.bounds.size.width );
  height:= Round( view.bounds.size.height );

  ctx := TCocoaContext.Create( NSGraphicsContext.currentContext );
  ctx.InitDraw( width, height );
  try
    cocoaBrush:= TCocoaBrush( lclBrush.Reference.Handle );
    ctx.Rectangle( 0, 0, width, height, True, cocoaBrush );
  finally
    ctx.Free;
  end;
end;

class procedure TCocoaViewUtil.lclOffsetWithEnclosingScrollView(
  const view: NSView;
  var x: Integer;
  var y: Integer );
var
  es: NSScrollView;
  r: NSRect;
begin
  es:= view.enclosingScrollView;
  if NOT Assigned(es) then
    Exit;
  if es.documentView <> view then
    Exit;
  r:= es.documentVisibleRect;
  if NOT view.isFlipped then
    r.origin.y:= es.documentView.frame.size.height - r.size.height - r.origin.y;
  inc( x, Round(r.origin.x) );
  inc( y, Round(r.origin.y) );
end;

{ TCocoaControlUtil }

class procedure TCocoaControlUtil.setStringValue(const c: NSControl; const S: String); inline;
var
  ns: NSString;
begin
  if Assigned(c) then
  begin
    ns := NSStringUtf8(S);
    c.setStringValue(ns);
    ns.release;
  end;
end;

// Sets the control text and then calls controls callback (if any)
// with TextChange (CM_TEXTCHANGED) event.
// Cocoa control do not fire a notification, if text is changed programmatically
// LCL expects a change notification in either way. (by software or by user)
class procedure TCocoaControlUtil.setStringValueAndSendEvent(
  const ctrl: NSControl;
  const text: String );
var
  cb: ICommonCallBack;
begin
  TCocoaControlUtil.setStringValue(ctrl, text);
  cb:= ctrl.lclGetcallback;
  if Assigned(cb) then // cb.SendOnChange;
    cb.SendOnTextChanged;
end;

class function TCocoaControlUtil.getStringValue(const c: NSControl): String; inline;
begin
  if Assigned(c) then
    Result := NSStringToString(c.stringValue)
  else
    Result := '';
end;

class procedure TCocoaControlUtil.moveCaretToTheEnd(const c: NSControl);
var
  range: NSRange;
begin
  if c.currentEditor <> nil then begin
    range.location:= NSUIntegerMax;
    range.length:= 0;
    c.currentEditor.setSelectedRange( range );
  end;
end;

class function TCocoaControlUtil.toMacOSTitle(const ATitle: String): NSString;
var
  t: String;
begin
  t:= TCocoaStringUtil.removeAcceleration(ATitle);
  if t = '' then
    Result:= NSString.string_ // empty string
  else
    Result:= NSString.stringWithUTF8String( @t[1] );
end;

{ LCLObjectExtension }

function LCLObjectExtension.lclIsEnabled: Boolean;
begin
  Result := False;
end;

procedure LCLObjectExtension.lclSetEnabled(AEnabled: Boolean);
begin
end;

function LCLObjectExtension.lclIsVisible: Boolean;
begin
  Result := False;
end;

procedure LCLObjectExtension.lclSetVisible(AVisible: Boolean);
begin
end;

function LCLObjectExtension.lclWindowState: Integer;
begin
  Result := SIZE_RESTORED;
end;

procedure LCLObjectExtension.lclInvalidateRect(const r: TRect);
begin
end;

procedure LCLObjectExtension.lclInvalidate;
begin
end;

procedure LCLObjectExtension.lclUpdate;
begin
end;

procedure LCLObjectExtension.lclLocalToScreen(var X,Y: Integer);
begin
end;

procedure LCLObjectExtension.lclScreenToLocal(var X, Y: Integer);
begin
end;

function LCLObjectExtension.lclParent:id;
begin
  Result:=nil;
end;

function LCLObjectExtension.lclFrame:TRect;
begin
  FillChar(Result, sizeof(Result), 0);
end;

procedure LCLObjectExtension.lclSetFrame(const r:TRect);
begin

end;

function LCLObjectExtension.lclGetFrameToLayoutDelta: TRect;
begin
  Result.Top := 0;
  Result.Left := 0;
  Result.Right := 0;
  Result.Bottom := 0;
end;

function LCLObjectExtension.lclClientFrame:TRect;
begin
  FillChar(Result, sizeof(Result), 0);
end;

function LCLObjectExtension.lclGetCallback: ICommonCallback;
begin
  Result := nil;
end;

procedure LCLObjectExtension.lclClearCallback;
begin
end;

function LCLObjectExtension.lclGetPropStorage: TStringList;
var
  Callback: ICommonCallback;
begin
  Callback := lclGetCallback;
  if Assigned(Callback) then
    Result := Callback.GetPropStorage
  else
    Result := nil;
end;

function LCLObjectExtension.lclGetTarget: TObject;
var
  Callback: ICommonCallback;
begin
  Callback := lclGetCallback;
  if Assigned(Callback) then
    Result := Callback.GetTarget
  else
    Result := nil;
end;

function LCLObjectExtension.lclDeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
var
  Callback: ICommonCallback;
begin
  Callback := lclGetCallback;
  if Assigned(Callback) then
    Result := Callback.DeliverMessage(Msg, WParam, LParam)
  else
    Result := 0;
end;

function LCLObjectExtension.lclContentView: NSView;
begin
  Result := nil;
end;

procedure LCLObjectExtension.lclOffsetMousePos(var Point: NSPoint);
begin

end;

procedure LCLObjectExtension.lclExpectedKeys(var wantTabs, wantArrows,
  wantReturn, wantAll: Boolean);
begin
  wantTabs := false;
  wantArrows := false;
  wantReturn := false;
  wantAll := false;
end;

{ The method should return TRUE, if mouse is located above an auxilary area
  of a (composited) control, and thus MOUSE MOVE event should not be propagated
  to LCL. For example, controls with Scrollbars should not report mouse events
  if mouse cursor is above ScrollBar and scroll bar is visible. (ScrollBar = Auxillary area)

  By default, the whole area is considered to be non-auxillary and must be
  reported to LCL.
  }
function LCLObjectExtension.lclIsMouseInAuxArea(Event: NSEvent): Boolean;
begin
  Result := false;
end;

{ LCLControlExtension }

function RectToViewCoord(view: NSView; const r: TRect): NSRect;
var
  b: NSRect;
begin
  b := view.bounds;
  Result.origin.x := r.Left;
  Result.size.width := r.Right - r.Left;
  Result.size.height := r.Bottom - r.Top;
  if Assigned(view) and (view.isFlipped) then
    Result.origin.y := r.Top
  else
    Result.origin.y := b.size.height - r.Bottom;
end;

function LCLControlExtension.lclIsEnabled:Boolean;
begin
  Result := IsEnabled;
end;

procedure LCLControlExtension.lclSetEnabled(AEnabled:Boolean);
begin
  {$ifdef BOOLFIX}
  SetEnabled_( Ord(AEnabled and NSViewIsLCLEnabled(self.superview) ));
  {$else}
  SetEnabled( AEnabled and TCocoaViewUtil.isLCLEnabled(self.superview) );
  {$endif}
  inherited lclSetEnabled(AEnabled);
end;

function LCLViewExtension.lclInitWithCreateParams(const AParams: TCreateParams): id;
var
  p: NSView;
  ns: NSRect;
  {$IFDEF COCOA_DEBUG_SETBOUNDS}
  pstr: string;
  {$ENDIF}
begin
  p := nil;
  if (AParams.WndParent <> 0) then
    p := NSView(AParams.WndParent).lclContentView;

  if Assigned(p) and p.isFlipped then
    TCocoaTypeUtil.toRect(Types.Bounds(AParams.X, AParams.Y, AParams.Width, AParams.Height),
      p.frame.size.height, ns)
  else
    ns := NSMakeRect(AParams.X, AParams.Y, AParams.Width, AParams.Height);

  {$IFDEF COCOA_DEBUG_SETBOUNDS}
  if Assigned(p) then
  begin
    pstr := NSStringToString(p.className);
    if NSStringToString(NSObject(AParams.WndParent).className) = 'TCocoaTabPage' then
      pstr := pstr + ' ' + NSStringToString(TCocoaTabPage(AParams.WndParent).label_);
  end
  else
    pstr := '';
  WriteLn(Format('[LCLViewExtension.lclInitWithCreateParams] Class=%s Caption=%s ParentClass=%s ParentClassView=%s rect=%d %d %d %d Visible=%d',
    [NSStringToString(Self.className), AParams.Caption,
     NSStringToString(NSObject(AParams.WndParent).className), pstr,
     Round(ns.Origin.x), Round(ns.Origin.y), Round(ns.size.width), Round(ns.size.height),
     AParams.Style and WS_VISIBLE]));
  {$ENDIF}

  Result := initWithFrame(ns);
  if not Assigned(Result) then
    Exit;

  {$ifdef BOOLFIX}
  setHidden_(Ord(AParams.Style and WS_VISIBLE = 0));
  {$else}
  setHidden(AParams.Style and WS_VISIBLE = 0);
  {$endif}

  if Assigned(p) then
    p.lclContentView.addSubview(Result);
  TCocoaViewUtil.setDefaultMargin(Result);
end;

function LCLViewExtension.lclIsEnabled: Boolean;
begin
  Result := true;
end;

procedure LCLViewExtension.lclSetEnabled(AEnabled: Boolean);
var
  cb : ICommonCallback;
  obj : NSObject;
begin
  for obj in subviews do begin
    cb := obj.lclGetCallback;
    obj.lclSetEnabled(AEnabled and ((not Assigned(cb)) or cb.GetShouldBeEnabled) );
  end;
end;

function LCLViewExtension.lclIsVisible: Boolean;
begin
  Result := not isHidden;
end;

procedure LCLViewExtension.lclSetVisible(AVisible: Boolean);
begin
  {$ifdef BOOLFIX}
  setHidden_(Ord(not AVisible));
  {$else}
  setHidden(not AVisible);
  {$endif}
  {$IFDEF COCOA_DEBUG_SETBOUNDS}
  WriteLn(Format('LCLViewExtension.lclSetVisible: %s AVisible=%d',
    [NSStringToString(Self.ClassName), Integer(AVisible)]));
  {$ENDIF}
end;

function LCLViewExtension.lclIsPainting: Boolean;
begin
  Result := Assigned(lclGetCallback) and Assigned(lclGetCallback.GetContext);
end;

procedure LCLViewExtension.lclInvalidateRect(const r:TRect);
var
  view : NSView;
begin
  view:=lclContentView;
  if Assigned(view) then
    view.setNeedsDisplayInRect(RectToViewCoord(view, r))
  else
    self.setNeedsDisplayInRect(RectToViewCoord(Self, r));
  //todo: it might be necessary to always invalidate self
  //      just need to get offset of the contentView relative for self
end;

procedure LCLViewExtension.lclInvalidate;
begin
  {$ifdef BOOLFIX}
  setNeedsDisplay__(Ord(True));
  {$else}
  setNeedsDisplay_(True);
  {$endif}
end;

procedure LCLViewExtension.lclUpdate;
begin
  {$ifdef BOOLFIX}
  setNeedsDisplay__(Ord(True));
  {$else}
  setNeedsDisplay_(True);
  {$endif}
  //display;
end;

procedure LCLViewExtension.lclLocalToScreen(var X, Y:Integer);
var
  rect: NSRect;
begin
  // Convert from View-lcl to View-cocoa
  rect.size:= NSZeroSize;
  rect.origin.x:= X;
  if isFlipped then
    rect.origin.y:= Y
  else
    rect.origin.y:= frame.size.height - y;

  // Convert from View-cocoa to Window-cocoa
  rect.origin:= self.convertPoint_ToView( rect.origin, nil );

  // Convert from Window-cocoa to Screen-cocoa
  rect:= window.convertRectToScreen( rect );

  // Convert from Screen-cocoa to Screen-lcl
  x:= Round( rect.origin.x );
  y:= Round( TCocoaScreenUtil.globalScreenBottom - rect.origin.y );

  if NOT (lclGetTarget is TScrollingWinControl) then
    TCocoaViewUtil.lclOffsetWithEnclosingScrollView(self, x, y );
end;

procedure LCLViewExtension.lclScreenToLocal(var X, Y: Integer);
var
  P: NSPoint;
begin
  // 1. convert from screen to window
  // use window function to onvert from Screen-lcl to Window-lcl
  window.lclScreenToLocal(X, Y);
  // Convert from Window-lcl to Window-cocoa
  P.x := X;
  P.y := Round(window.frame.size.height-Y); // convert to Cocoa system

  // 2. convert from window to local
  //    Convert from Window-cocoa to View-cocoa
  P := convertPoint_FromView(P, nil);

  // Convert from View-cocoa to View-lcl
  X := Round(P.x);
  if isFlipped then
    Y := Round(p.y)
  else
    Y := Round(frame.size.height-P.y);   // convert to Cocoa system
end;

function LCLViewExtension.lclParent:id;
begin
  Result := superView;
end;

function LCLViewExtension.lclFrame: TRect;
var
  v: NSView;
begin
  v := superview;
  if Assigned(v) and not v.isFlipped then
    TCocoaTypeUtil.toRect(frame, v.frame.size.height, Result)
  else
    Result := TCocoaTypeUtil.toRect(frame);
  TCocoaViewUtil.addLayoutDelta( lclGetFrameToLayoutDelta, Result);
end;

procedure LCLViewExtension.lclSetFrame(const r: TRect);
var
  ns: NSRect;
  svHeight: CGFloat;
  rr : TRect;
begin
  rr := r;
  TCocoaViewUtil.subLayoutDelta( lclGetFrameToLayoutDelta, rr);

  svHeight := TCocoaViewUtil.getSuperViewHeight(Self);
  if Assigned(superview) and not superview.isFlipped then
  begin
    TCocoaTypeUtil.toRect(rr, svHeight, ns)
  end
  else
    ns := TCocoaTypeUtil.toRect(rr);

  if ns.size.width<1 then ns.size.width:=1;
  if ns.size.height<1 then ns.size.height:=1;

  {$IFDEF COCOA_DEBUG_SETBOUNDS}
  WriteLn(Format('LCLViewExtension.lclSetFrame: %s Bounds=%s height=%d ns_pos=%d %d ns_size=%d %d',
    [NSStringToString(Self.ClassName), dbgs(r), Round(svHeight),
     Round(ns.origin.x), Round(ns.origin.y), Round(ns.size.width), Round(ns.size.height)]));
  {$ENDIF}
  setFrame(ns);
end;

function LCLViewExtension.lclClientFrame: TRect;
begin
  Result := lclFrame;
  Types.OffsetRect(Result, -Result.Left, -Result.Top);
end;

function LCLViewExtension.lclContentView: NSView;
begin
  Result := self;
end;

procedure LCLViewExtension.lclOffsetMousePos(var Point: NSPoint);
var
  es : NSScrollView;
  r  : NSRect;
  dlt : TRect;
begin
  Point := convertPoint_fromView(Point, nil);

  es := enclosingScrollView;
  if es.documentView <> self then es := nil;
  if not isFlipped then
    Point.y := bounds.size.height - Point.y;

  if Assigned(es) then
  begin
    r := es.documentVisibleRect;
    if isFlipped then
      Point.y := Point.y - r.origin.y
    else
      Point.y := Point.y - (es.documentView.frame.size.height - r.size.height - r.origin.y);
    Point.X := Point.X - r.origin.x;
  end;

  dlt := lclGetFrameToLayoutDelta;
  Point.X := Point.X - dlt.Left;
  Point.Y := Point.Y - dlt.Top;
end;

end.

