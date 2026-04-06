unit CocoaScrolling;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}
{$include cocoadefines.inc}

interface

uses
  Classes, SysUtils, Math,
  LCLType, Controls, StdCtrls, Forms,
  MacOSAll, CocoaAll,
  CocoaPrivate, CocoaCursor, CocoaConst, CocoaConfig, CocoaUtils;

type

  { TCocoaScrollView }
  // TCocoaScrollView is based on NSScrollView with native Scroller.
  // it requires the document to have the full size it claims, so it is usually
  // suitable for components of limited size.
  // it corresponds to components such as ListBox, ListView, ScrollBox, Form.

  TCocoaScrollView = objcclass(NSScrollView)
  private
    _lastScrollX: Integer;
    _lastScrollY: Integer;
  public
    callback: ICommonCallback;
    isCustomRange: Boolean;
    scrollingLockCount: Integer;

    // the corresponding LCL ScrollInfo,
    // which are needed when documentView.frame changes.
    lclHorzScrollInfo: TScrollInfo;
    lclVertScrollInfo: TScrollInfo;

    docrect    : NSRect;    // have to remember old
    holdscroll : Integer; // do not send scroll messages

    procedure dealloc; override;
    procedure setFrame(aframe: NSRect); override;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclClientFrame: TRect; override;
    function lclContentView: NSView; override;
    procedure setDocumentView(aView: NSView); override;
    procedure scrollWheel(theEvent: NSEvent); override;
    procedure reflectScrolledClipView(cView: NSClipView); override;
    procedure ensureDocumentViewSizeChanged(newSize: NSSize;
      ensureWidth: Boolean; ensureHeight: Boolean);
      message 'ensureDocumentViewSizeChanged:newSize:ensureWidth:';
    procedure resetScrollData; message 'resetScrollData';

    procedure lclUpdate; override;
    procedure lclInvalidateRect(const r: TRect); override;
    procedure lclInvalidate; override;

    procedure fillScrollInfo(barFlag: Integer; var scrollInfo: TScrollInfo); message 'fillScrollInfo:barFlag:';
    procedure applyScrollInfo(barFlag: Integer; const scrollInfo: TScrollInfo); message 'applyScrollInfo:barFlag:';
  end;

  {
    TCocoaManualScrollView is based on NSView without native Scroller.
    it doesn't require the document to have the full size it claims, so it is
    usually suitable for components of unlimited size.
    it corresponds to components such as TreeView, Grid, SynEdit.

    the following implements modern macOS scrollbars for TCocoaManualScrollView.
    Legacy and Overlay style Scroller were implemented.

    in the architecture design, it is mainly decomposed into four structures:
    1. ScrollBar: the Scroll Info Modal, not related to a specific style
    2. ScrollView: the Container, not related to a specific style
    3. Manager: core component, called by ScrollBar and ScrollView.
                currently ther are two implementations: Legacy and Overlay style.
    4. Effect: animation effects, called by the Manager.
               currently there are Alpha and Overlay Style.

    it would be more appropriate to use Interface to define the interface of
    each component, but FreePascal's Interface is so weak that it can only be
    replaced by abstract classes here.
  }

  { TCocoaScrollBarEffect }
  // there are many animation effects, which are concentrated in TCocoaScrollBarEffect.
  // currently there are Alpha and Overlay (DelayHidding/Expand) implementations.

  TCocoaScrollBarEffect = objcclass(NSObject)
  protected
    procedure onDestroy; message 'onDestroy';
  end;

  { TCocoaScrollBarStyleManager }
  // manager for Scroller, with Legacy and Overlay style implementations

  TCocoaScrollBarStyleManager = class
    // called by TCocoaScrollBar when the value changes.
    procedure onKnobValueUpdated( scroller:NSScroller;
      var knobPosition:Double; var knobProportion:Double ); virtual; abstract;

    // draw the Knob, called by TCocoaScrollBar
    procedure onDrawKnob( scroller:NSScroller ); virtual; abstract;

    // draw the KnobSlot, called by TCocoaScrollBar
    function onDrawKnobSlot( scroller:NSScroller; var slotRect: NSRect ):
      Boolean; virtual; abstract;

    // called by TCocoaScrollBar after the mouse entered
    procedure onMouseEntered( scroller:NSScroller ); virtual; abstract;
    // called by TCocoaScrollBar after the mouse exited
    procedure onMouseExited( scroller:NSScroller ); virtual; abstract;

    // Create the corresponding Effect
    function createScrollBarEffect( scroller:NSScroller ):
      TCocoaScrollBarEffect; virtual; abstract;

    // Make isAvailableScrollBar return Ture(Available)/False(not Available) if necessary
    procedure availScrollBar( scroller:NSScroller; available:Boolean ); virtual; abstract;
    // Returns the Availability of the Scroller
    // with Legacy Style, Availability means 'Shown/not Hidden'
    // with Overlay Style, Availability means 'KnobProportion<1'
    function isAvailableScrollBar( scroller:NSScroller ): Boolean; virtual; abstract;
    // Show the Scroller if it can be made Available
    procedure showScrollBar( scroller:NSScroller; now:Boolean=True ); virtual; abstract;
    // Hide the Scroller but keep the Availability, only for Overlay Style
    procedure tempHideScrollBar( scroller:NSScroller ); virtual; abstract;
  end;

  TCocoaManualScrollView = objcclass;

  { TCocoaScrollStyleManager }
  // manager for ScrollView, with Legacy and Overlay style implementations.
  // TCocoaScrollStyleManager and TCocoaScrollBarStyleManager should be two
  // independent interfaces, but due to the limitations of FreePascal mentioned
  // earlier, only abstract classes can be used here, so they can only become
  // an inheritance relationship.
  // the existence of createForScrollBar() and createForScrollView()
  // is a compromise in this situation.

  TCocoaScrollStyleManager = class(TCocoaScrollBarStyleManager)
  protected
    _scrollView: TCocoaManualScrollView;
    function isBarOccupyBound: Boolean; virtual; abstract;
  public
    // place the document, horizontal scroller, and vertical scroller
    // in the appropriate positions
    procedure updateLayout; virtual;
  public
    constructor createForScrollBar;
    constructor createForScrollView( const scrollView: TCocoaManualScrollView );
  end;

  { TCocoaScrollBar }

  TCocoaScrollBar = objcclass(NSScroller)
  private
    _scrollView: TCocoaManualScrollView;
    _manager: TCocoaScrollBarStyleManager;
    _effect: TCocoaScrollBarEffect;
    _trackingArea: NSTrackingArea;
  private
    procedure releaseManager; message 'releaseManager';
  public
    callback: ICommonCallback;
    preventBlock : Boolean;
    // minInt,maxInt are used to calculate position for lclPos and lclSetPos
    minInt  : Integer;
    maxInt  : Integer;
    pageInt : Integer;
    suppressLCLMouse: Boolean;
    largeInc: Integer;
    smallInc: Integer;

    procedure dealloc; override;

    function manager: TCocoaScrollBarStyleManager;
      message 'manager';
    function effect: TCocoaScrollBarEffect;
      message 'effect';
    procedure setManager( newManager:TCocoaScrollBarStyleManager );
      message 'setManager:';

    // Note:
    // Do not override isCompatibleWithOverlayScrollers(), which will cause
    // the Scroller to enable NSView.Layer, leading to various problems.
    // class function isCompatibleWithOverlayScrollers: ObjCBOOL; override;

    procedure resetCursorRects; override;
    procedure drawKnob; override;
    procedure drawKnobSlotInRect_highlight(slotRect: NSRect; flag: ObjCBOOL); override;
    procedure setDoubleValue(newValue: double); override;
    procedure setKnobProportion(newValue: CGFloat); override;
    procedure updateTrackingAreas; override;
    procedure mouseEntered(theEvent: NSEvent); override;
    procedure mouseExited(theEvent: NSEvent); override;

    procedure actionScrolling(sender: NSObject); message 'actionScrolling:';
    function IsHorizontal: Boolean; message 'IsHorizontal';

    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclPos: Integer; message 'lclPos';
    procedure lclSetPos(aPos: integer); message 'lclSetPos:';
    // mouse
    function acceptsFirstMouse(event: NSEvent): LCLObjCBoolean; override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure rightMouseDragged(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure otherMouseDragged(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;
  end;

  { TCocoaManualScrollView }

  TCocoaManualScrollView = objcclass(NSView)
  private
    _manager: TCocoaScrollStyleManager;
    _tapping: Integer;             // two finger tapping, SB_VERT / SB_HORZ / SB_BOTH
    _documentView: NSView;
    _horzScrollBar : TCocoaScrollBar;
    _vertScrollBar : TCocoaScrollBar;
  public
    callback: ICommonCallback;
    function initWithFrame(frameRect: NSRect): id; override;
    procedure dealloc; override;
    procedure setFrame(newValue: NSRect); override;

    procedure resetManager; message 'resetManager';
    procedure onScrollerStyleUpdated; message 'onScrollerStyleUpdated';

    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclContentView: NSView; override;
    function lclClientFrame: TRect; override;
    function lclIsMouseInAuxArea(event: NSEvent): Boolean; override;
    procedure lclUpdate; override;
    procedure lclInvalidateRect(const r: TRect); override;
    procedure lclInvalidate; override;

    procedure setDocumentView(AView: NSView); message 'setDocumentView:';
    function documentView: NSView; message 'documentView';

    procedure delayShowScrollBars; message 'delayShowScrollBars';
    procedure showScrollBarsAndAutoHide( tapping:Integer ); message 'showScrollBarsAndAutoHide:';

    function isTapping( scroller:NSScroller ): Boolean; message 'isTapping:';
    procedure onBarScrolled( scroller:NSScroller ); message 'onBarScrolled:';
    procedure touchesBeganWithEvent(event: NSEvent); override;
    procedure touchesEndedWithEvent(event: NSEvent); override;
    procedure touchesCancelledWithEvent(event: NSEvent); override;

    procedure setHasVerticalScroller(doshow: Boolean); message 'setHasVerticalScroller:';
    procedure setHasHorizontalScroller(doshow: Boolean); message 'setHasHorizontalScroller:';
    function hasVerticalScroller: Boolean; message 'hasVerticalScroller';
    function hasHorizontalScroller: Boolean; message 'hasHorizontalScroller';

    function horizontalScroller: NSScroller; message 'horizontalScroller';
    function verticalScroller: NSScroller; message 'verticalScroller';

    function allocHorizontalScroller(avisible: Boolean): TCocoaScrollBar; message 'allocHorizontalScroller:';
    function allocVerticalScroller(avisible: Boolean): TCocoaScrollBar; message 'allocVerticalScroller:';
    // mouse
    function acceptsFirstMouse(event: NSEvent): LCLObjCBoolean; override;
  end;

  { TCocoaManualScrollHost }

  TCocoaManualScrollHost = objcclass(TCocoaScrollView)
    function lclContentView: NSView; override;
    function lclClientFrame: TRect; override;
    procedure scrollWheel(theEvent: NSEvent); override;
    procedure setFrame(newValue: NSRect); override;

    procedure setScrollerStyle(newValue: NSScrollerStyle); override;
  end;

  { TCocoaScrollingUtil }

  TCocoaScrollingUtil = class
  public
    class function createScroller(
      const parent: TCocoaManualScrollView;
      const dst: NSRect ): TCocoaScrollBar;

    class function isMouseEventInScrollBar(
      const host: TCocoaManualScrollView;
      const event: NSEvent ): Boolean;

    class procedure setBorderStyle(
      const sv: NSScrollView;
      const astyle: TBorderStyle );
    class procedure setScrollStyle(
      const AScroll: TCocoaScrollView;
      const AStyles: TScrollStyle );
  end;

type
  TCocoaScrollStyleManagerCreator = function (
    const sv: NSView;
    const style: NSScrollerStyle ): TCocoaScrollStyleManager;

var
  CocoaScrollStyleManagerCreator: TCocoaScrollStyleManagerCreator;

implementation

{ TCocoaScrollView }

function TCocoaScrollView.lclClientFrame: TRect;
begin
  Result:= TCocoaTypeUtil.toRect(contentView.frame, frame.size.height);
end;

function TCocoaScrollView.lclContentView: NSView;
begin
  Result:=documentView;
end;

procedure TCocoaScrollView.setDocumentView(aView: NSView);
begin
  inherited setDocumentView(aView);
  resetScrollData;
end;

// ensure documentView Size be changed
// setHasXxxScroller() only takes effect after documentView Size is changed
procedure TCocoaScrollView.ensureDocumentViewSizeChanged(newSize: NSSize;
  ensureWidth: Boolean; ensureHeight: Boolean);
var
  oldSize: NSSize;
  tempSize: NSSize;
begin
  oldSize:= self.documentView.frame.size;
  tempSize:= newSize;
  if ensureWidth and (oldSize.width=tempSize.width) then
    tempSize.width:= tempSize.width + 1;
  if ensureHeight and (oldSize.height=tempSize.height) then
    tempSize.height:= tempSize.height + 1;
  if ensureWidth or ensureHeight then
    self.documentView.setFrameSize( tempSize );
  self.documentView.setFrameSize( newSize );
end;

procedure TCocoaScrollView.resetScrollData;
begin
  docrect:=documentVisibleRect;
  lclHorzScrollInfo.fMask:= 0;
  lclVertScrollInfo.fMask:= 0;
end;

procedure TCocoaScrollView.scrollWheel(theEvent: NSEvent);
begin
  Inc( self.scrollingLockCount );
  if self.hasHorizontalScroller or self.hasVerticalScroller then begin
    inherited scrollWheel( theEvent );
    callback.scrollWheel( theEvent );
  end else if Assigned(self.enclosingScrollView) then
    self.enclosingScrollView.scrollWheel( theEvent )
  else
    inherited scrollWheel( theEvent );
  Dec( self.scrollingLockCount );
end;

procedure TCocoaScrollView.reflectScrolledClipView(cView: NSClipView);
var
  currentX: Integer;
  currentY: Integer;
  control: TWinControl;
begin
  inherited reflectScrolledClipView(cView);

  if NOT isCustomRange then
    Exit;

  currentX:= Round( cView.bounds.origin.x );
  if not self.documentView.isFlipped then begin
    currentY:= Round( self.documentView.frame.size.height - cView.bounds.origin.y - self.contentSize.height );
  end else begin
    currentY:= Round( cView.bounds.origin.y );
  end;

  control:= TWinControl( self.lclGetTarget );
  if Assigned(control) and (self.scrollingLockCount=0) and Assigned(callback) and control.HandleAllocated then begin
    if _lastScrollX <> currentX then
      callback.scroll( False, currentX, NSScrollerKnob );
    if _lastScrollY <> currentY then
      callback.scroll( True, currentY, NSScrollerKnob );
  end;

  _lastScrollX:= currentX;
  _lastScrollY:= currentY;
end;

procedure TCocoaScrollView.lclUpdate;
begin
  documentView.lclUpdate;
end;

procedure TCocoaScrollView.lclInvalidateRect(const r: TRect);
begin
  documentView.lclInvalidateRect(r);
end;

procedure TCocoaScrollView.lclInvalidate;
begin
  documentView.lclInvalidate;
end;

procedure TCocoaScrollView.fillScrollInfo(barFlag: Integer;
  var scrollInfo: TScrollInfo);
var
  docSize: NSSize;

  procedure fillScrollerScrollInfo(maxValue: CGFloat; pageValue: CGFloat;
    scroller: NSScroller);
  begin
    scrollInfo.cbSize:=sizeof(scrollInfo);
    scrollInfo.fMask:=SIF_ALL;
    scrollInfo.nPos:=round(scroller.doubleValue*(maxValue-pageValue));
    scrollInfo.nTrackPos:=ScrollInfo.nPos;
    scrollInfo.nMin:=0;
    scrollInfo.nMax:=round(maxValue);
    scrollInfo.nPage:=round(scroller.knobProportion*maxValue);
  end;

begin
  if not Assigned(self.documentView) then begin
    FillChar(scrollInfo, sizeof(scrollInfo),0);
    scrollInfo.cbSize:=sizeof(scrollInfo);
    Exit;
  end;

  docSize:= self.documentView.frame.size;
  if barFlag = SB_Vert then
    fillScrollerScrollInfo(docSize.height, self.contentSize.height, self.verticalScroller)
  else
    fillScrollerScrollInfo(docSize.width, self.contentSize.width, self.horizontalScroller);
end;

procedure TCocoaScrollView.applyScrollInfo(barFlag: Integer;
  const scrollInfo: TScrollInfo);
var
  newOrigin : NSPoint;
begin
  if not Assigned(self.documentView) then Exit;

  newOrigin:= self.contentView.bounds.origin;
  if BarFlag = SB_Vert then
  begin
    self.lclVertScrollInfo:= scrollInfo;
    _lastScrollY:= scrollInfo.nPos;
    if not self.documentView.isFlipped then
      newOrigin.y := self.documentView.frame.size.height - scrollInfo.nPos - self.contentSize.height
    else
      newOrigin.y := scrollInfo.nPos;
  end
  else
  begin
    self.lclHorzScrollInfo:= scrollInfo;
    _lastScrollX:= scrollInfo.nPos;
    newOrigin.x:= scrollInfo.nPos;
  end;
  self.contentView.setBoundsOrigin( newOrigin );
end;

procedure TCocoaScrollView.dealloc;
begin
  NSNotificationCenter.defaultCenter
    .removeObserver(self);
  inherited dealloc;
end;

procedure TCocoaScrollView.setFrame(aframe: NSRect);
var
  newDocSize: NSSize;
  lclControl : TScrollingWinControl;
  lclBar: TControlScrollBar;
begin
  if not isCustomRange then begin
    inherited setFrame(aframe);
    Exit;
  end;

  inherited setFrame(aframe);

  if lclGetTarget is TScrollingWinControl then begin
    lclControl:= TScrollingWinControl(lclGetTarget);
    newDocSize:= contentSize;

    lclBar:= lclControl.HorzScrollBar;
    if lclBar.Visible and (lclBar.Range<>0) and (lclBar.Range>lclBar.Page) then begin
      if lclBar.Range > newDocSize.Width then
        newDocSize.Width:= lclBar.Range;
    end;

    lclBar:= lclControl.VertScrollBar;
    if lclBar.Visible and (lclBar.Range<>0) and (lclBar.Range>lclBar.Page) then begin
      if lclBar.Range > newDocSize.Height then
        newDocSize.Height:= lclBar.Range;
    end;

    documentView.setFrameSize(newDocSize);
  end;

  if lclHorzScrollInfo.fMask<>0 then
    applyScrollInfo(SB_Horz, lclHorzScrollInfo);

  if lclVertScrollInfo.fMask<>0 then
    applyScrollInfo(SB_Vert, lclVertScrollInfo);
end;

function TCocoaScrollView.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := false;
end;

function TCocoaScrollView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaScrollView.lclClearCallback;
begin
  callback := nil;
  if Assigned(self.documentView) then
    self.documentView.lclClearCallback;
end;

{ TCocoaScrollStyleManager }

procedure TCocoaScrollStyleManager.updateLayout;
var
  doc: NSView;
  docFrame  : NSRect;
  horzScroller: NSScroller;
  vertScroller: NSScroller;
  horzScrollerFrame : NSRect;
  vertScrollerFrame : NSRect;
  horzScrollerHeight : CGFLoat;
  vertScrollerWidth : CGFLoat;
begin
  doc:= _scrollView.documentView;
  if NOT Assigned(doc) then
    Exit;

  docFrame := _scrollView.frame;
  docFrame.origin := NSZeroPoint;
  horzScrollerFrame := docFrame;
  vertScrollerFrame := docFrame;

  horzScroller:= _scrollView._horzScrollBar;
  vertScroller:= _scrollView._vertScrollBar;

  if Assigned(horzScroller) then begin
    if NOT isBarOccupyBound or isAvailableScrollBar(horzScroller) then begin
      horzScrollerHeight := NSScroller.scrollerWidthForControlSize_scrollerStyle(
              horzScroller.controlSize, horzScroller.scrollerStyle);
      horzScrollerFrame.size.height := horzScrollerHeight;

      if isBarOccupyBound then begin
        docFrame.size.height := docFrame.size.height - horzScrollerHeight;
        if docFrame.size.height < 0 then
          docFrame.size.height := 0;
        docFrame.origin.y := horzScrollerHeight;
      end;
    end;
  end;

  if Assigned(vertScroller) then begin
    if NOT isBarOccupyBound or isAvailableScrollBar(vertScroller) then begin
      vertScrollerWidth := NSScroller.scrollerWidthForControlSize_scrollerStyle(
              vertScroller.controlSize, vertScroller.scrollerStyle);
      vertScrollerFrame.size.width := vertScrollerWidth;

      if isBarOccupyBound then begin
        docFrame.size.width := docFrame.size.width - vertScrollerWidth;
        if docFrame.size.width < 0 then
          docFrame.size.width:= 0;
      end;
    end;
  end;

  horzScrollerFrame.size.width := docFrame.size.width;
  vertScrollerFrame.size.height := docFrame.size.height;
  if isBarOccupyBound then
    vertScrollerFrame.origin.x := docFrame.size.width
  else
    vertScrollerFrame.origin.x := docFrame.size.width - vertScrollerFrame.size.width;
  vertScrollerFrame.origin.y := docFrame.origin.y;

  if Assigned(horzScroller) then begin
    if horzScrollerFrame.size.width <= horzScrollerFrame.size.height then
      horzScrollerFrame.size.width:= horzScrollerFrame.size.height + 1;
    horzScroller.setFrame(horzScrollerFrame);
  end;

  if Assigned(vertScroller) then begin
    if vertScrollerFrame.size.height <= vertScrollerFrame.size.width then
      vertScrollerFrame.size.height:= vertScrollerFrame.size.width + 1;
    vertScroller.setFrame(vertScrollerFrame);
  end;

  if not NSEqualRects(doc.frame, docFrame) then
  begin
    doc.setFrame(docFrame);
    {$ifdef BOOLFIX}
    doc.setNeedsDisplay__(Ord(true));
    {$else}
    doc.setNeedsDisplay_(true);
    {$endif}
  end;
end;

constructor TCocoaScrollStyleManager.createForScrollBar;
begin
end;

constructor TCocoaScrollStyleManager.createForScrollView(
  const scrollView: TCocoaManualScrollView );
begin
  _scrollView:= scrollView;
end;

{ TCocoaScrollBar }

function SysPrefScrollClick: Integer; // 0 - adjust by page, 1 - jump to the position
begin
  Result := Integer(NSUserDefaults.standardUserDefaults.integerForKey(NSSTR('AppleScrollerPagingBehavior')));
end;

function isPagePart(prt: NSScrollerPart): Boolean; inline;
begin
  Result := (prt = NSScrollerDecrementPage)
         or (prt = NSScrollerIncrementPage);
end;

function AdjustScrollerPage(sc: TCocoaScrollBar; prt: NSScrollerPart): Boolean;
var
  adj : integer;
  sz  : Integer;
  dlt : double;
  v   : double;
begin
  Result := false;
  case prt of
    NSScrollerDecrementPage: begin
      adj := -sc.largeInc;
      if adj = 0 then adj := -sc.pageInt;
    end;
    NSScrollerIncrementPage: begin
      adj := sc.largeInc;
      if adj = 0 then adj := sc.pageInt;
    end;
    NSScrollerDecrementLine: begin
      adj := -sc.smallInc;
      if adj = 0 then adj := -1;
    end;
    NSScrollerIncrementLine: begin
      adj := sc.smallInc;
      if adj = 0 then adj := 1;
    end;
  else
    adj := 0;
  end;
  if adj = 0 then Exit;

  sz := sc.maxInt - sc.minInt - sc.pageInt;
  if sz = 0 then Exit; // do nothing!

  dlt := adj / sz;

  v := sc.doubleValue;
  v := v + dlt;
  if v < 0 then v := 0
  else if v > 1 then v := 1;

  // todo: call scroll event?
  sc.setDoubleValue(v);

  {$ifdef BOOLFIX}
  sc.setNeedsDisplay__(Ord(true));
  {$else}
  sc.setNeedsDisplay_(true);
  {$endif}
end;

procedure HandleMouseDown(sc: TCocoaScrollBar; locInWin: NSPoint; prt: NSScrollerPart);
var
  adj : Integer;
  sz  : Integer;
  pt  : NSPoint;
  ps  : double;
  newPos: Integer;
begin
  adj := SysPrefScrollClick;
  if adj = 0 then begin
    // single page jump
    AdjustScrollerPage(sc, prt);
  end
  else
  begin
    // direct jump
    pt := sc.convertPoint_fromView(locInWin, nil);
    if sc.IsHorizontal then begin
      if sc.frame.size.width = 0 then Exit; // do nothing
      ps := pt.x / sc.frame.size.width;
    end
    else
    begin
      if sc.frame.size.height = 0 then Exit; // do nothing
      ps := pt.y / sc.frame.size.height;
    end;
    sz := (sc.maxInt - sc.minInt - sc.pageInt);
    newPos := Round(sc.minInt + sz * ps);
    sc.lclSetPos(NewPos);
  end;
end;

procedure TCocoaScrollBar.releaseManager;
begin
  if NOT Assigned(_manager) then
    Exit;

  if NOT Assigned(_scrollView) then
    FreeAndNil(_manager);

  if Assigned(_effect) then begin
    _effect.onDestroy;
    _effect.release;
    _effect:= nil;
  end;
end;

procedure TCocoaScrollBar.dealloc;
begin
  releaseManager;
  inherited dealloc;
end;

function TCocoaScrollBar.manager: TCocoaScrollBarStyleManager;
begin
  Result:= _manager;
end;

function TCocoaScrollBar.effect: TCocoaScrollBarEffect;
begin
  Result:= _effect;
end;

procedure TCocoaScrollBar.setManager( newManager: TCocoaScrollBarStyleManager);
begin
  releaseManager;
  _scrollView:= nil;
  _manager:= newManager;
  if Assigned(_manager) then begin
    if _manager is TCocoaScrollStyleManager then
      _scrollView:= TCocoaScrollStyleManager(_manager)._scrollView;
    _effect:= _manager.createScrollBarEffect(self);
  end;
end;

procedure TCocoaScrollBar.resetCursorRects;
begin
  inherited resetCursorRects;
  TCocoaCursorUtil.setCursorByCurrentPos;
end;

procedure TCocoaScrollBar.drawKnob;
begin
  _manager.onDrawKnob(self);
end;

procedure TCocoaScrollBar.drawKnobSlotInRect_highlight(slotRect: NSRect;
  flag: ObjCBOOL);
var
  drawSlot: Boolean;
begin
  drawSlot:= _manager.onDrawKnobSlot(self, slotRect);
  if drawSlot then
    Inherited;
end;

procedure TCocoaScrollBar.setDoubleValue(newValue: double);
var
  proportion: CGFloat;
begin
  if newValue < 0 then newValue:= 0;
  if newValue > 1 then newValue:= 1;
  proportion:= self.knobProportion;
  _manager.onKnobValueUpdated( self, newValue, proportion );
  inherited;
end;

procedure TCocoaScrollBar.setKnobProportion(newValue: CGFloat);
var
  position: CGFloat;
begin
  if newValue < 0 then newValue:= 0;
  if newValue > 1 then newValue:= 1;
  position:= self.doubleValue;
  _manager.onKnobValueUpdated( self, position, newValue );
  inherited;
end;

procedure TCocoaScrollBar.updateTrackingAreas;
var
  i: NSTrackingArea;
const
  options: NSTrackingAreaOptions = NSTrackingMouseEnteredAndExited
                                or NSTrackingActiveAlways;
begin
  if Assigned(_trackingArea) then begin
    removeTrackingArea(_trackingArea);
    _trackingArea:= nil;
  end;

  _trackingArea:= NSTrackingArea.alloc.initWithRect_options_owner_userInfo(
                 self.bounds,
                 options,
                 self,
                 nil);
  self.addTrackingArea( _trackingArea );
  _trackingArea.release;
end;

procedure TCocoaScrollBar.mouseEntered(theEvent: NSEvent);
begin
  _manager.onMouseEntered(self);
end;

procedure TCocoaScrollBar.mouseExited(theEvent: NSEvent);
begin
  _manager.onMouseExited( self );
end;

procedure TCocoaScrollBar.actionScrolling(sender: NSObject);
var
  event : NSEvent;
  prt : NSScrollerPart;
  locInWin : NSPoint;
begin
  event := NSApplication.sharedApplication.currentEvent;
  if not Assigned(event) then Exit;

  if not Assigned(event.window) then
  begin
    locInWin := event.mouseLocation;
    if Assigned(window) then
      locInWin := window.convertScreenToBase(locInWin);
  end else
    locInWin := event.locationInWindow;

  prt := testPart(locInWin);
  if isPagePart(prt) then
    HandleMouseDown(self, locInWin, prt);

  if Assigned(callback) then
    callback.scroll(not IsHorizontal(), lclPos, prt);
end;

function TCocoaScrollBar.IsHorizontal: Boolean;
begin
  Result := frame.size.width > frame.size.height;
end;

function TCocoaScrollBar.lclPos: Integer;
begin
  Result:=round( doubleValue * (maxint-minInt-pageInt)) + minInt;
end;

procedure TCocoaScrollBar.lclSetPos(aPos: integer);
var
  d : integer;
begin
  d := maxInt - minInt - pageInt;
  if d <= 0 then
    setDoubleValue(0)
  else
  begin
    if aPos < minInt then aPos:=minInt
    else if aPos > maxInt then aPos:=maxInt;
    setDoubleValue( (aPos - minInt) / d );
  end;
end;

function TCocoaScrollBar.acceptsFirstMouse(event: NSEvent): LCLObjCBoolean;
begin
  Result:=true;
end;

procedure TCocoaScrollBar.mouseDown(event: NSEvent);
begin
  if suppressLCLMouse then
  begin
    inherited mouseDown(event);
  end
  else
  if not Assigned(callback) or not callback.MouseUpDownEvent(event, false, preventBlock) then
  begin
    inherited mouseDown(event);

    if Assigned(callback) then
      callback.MouseUpDownEvent(event, true, preventBlock);
  end;
end;

procedure TCocoaScrollBar.mouseUp(event: NSEvent);
begin
  if suppressLCLMouse then
  begin
    inherited mouseDown(event)
  end
  else
  if not Assigned(callback) or not callback.MouseUpDownEvent(event, false, preventBlock) then
    inherited mouseUp(event);
end;

procedure TCocoaScrollBar.rightMouseDown(event: NSEvent);
begin
  if suppressLCLMouse then
    inherited rightMouseDown(event)
  else
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaScrollBar.rightMouseUp(event: NSEvent);
begin
  if suppressLCLMouse then
    inherited rightMouseUp(event)
  else
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaScrollBar.rightMouseDragged(event: NSEvent);
begin
  if suppressLCLMouse then
    inherited rightMouseDragged(event)
  else
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaScrollBar.otherMouseDown(event: NSEvent);
begin
  if suppressLCLMouse then
    inherited otherMouseDown(event)
  else
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaScrollBar.otherMouseUp(event: NSEvent);
begin
  if suppressLCLMouse then
    inherited otherMouseUp(event)
  else
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaScrollBar.otherMouseDragged(event: NSEvent);
begin
  if suppressLCLMouse then
    inherited otherMouseDragged(event)
  else
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaScrollBar.mouseDragged(event: NSEvent);
begin
  if suppressLCLMouse then
    inherited mouseDragged(event)
  else
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaScrollBar.mouseMoved(event: NSEvent);
begin
  if suppressLCLMouse then
    inherited mouseMoved(event)
  else
  if (not Assigned(callback) or not callback.MouseMove(event)) then
    inherited mouseMoved(event)
end;

procedure TCocoaScrollBar.scrollWheel(event: NSEvent);
begin
  if Assigned(callback) then
    callback.scrollWheel(event)
  else
    Inherited;
end;

function TCocoaScrollBar.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := True;
end;

function TCocoaScrollBar.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaScrollBar.lclClearCallback;
begin
  callback := nil;
end;

{ TCocoaScrollBarEffect }

procedure TCocoaScrollBarEffect.onDestroy;
begin
end;

{ TCocoaManualScrollView }

function TCocoaManualScrollView.initWithFrame(frameRect: NSRect): id;
begin
  Result:= inherited;
  _tapping:= -1;
  resetManager;
end;

procedure TCocoaManualScrollView.dealloc;
begin
  if Assigned(_horzScrollBar) then begin
    _horzScrollBar.removeFromSuperview;
    _horzScrollBar.setManager( nil );
    _horzScrollBar.release;
  end;
  if Assigned(_vertScrollBar) then begin
    _vertScrollBar.removeFromSuperview;
    _vertScrollBar.setManager( nil );
    _vertScrollBar.release;
  end;
  FreeAndNil( _manager );
end;

procedure TCocoaManualScrollView.setFrame(newValue: NSRect);
begin
  inherited setFrame(newValue);
  _manager.updateLayout;
end;

procedure TCocoaManualScrollView.resetManager;
var
  oldManager: TCocoaScrollStyleManager;
  style: NSScrollerStyle;
begin
  oldManager:= _manager;

  style:= CocoaConfigScroller.preferredStyle;
  if style < 0 then
    style:= NSScroller.preferredScrollerStyle;

  _manager:= CocoaScrollStyleManagerCreator( self, style );

  if Assigned(_horzScrollBar) then begin
    _horzScrollBar.setManager( _manager );
  end;
  if Assigned(_vertScrollBar) then begin
    _vertScrollBar.setManager( _manager );
  end;

  oldManager.Free;
end;

procedure TCocoaManualScrollView.onScrollerStyleUpdated;
var
  horzAvailable: Boolean;
  vertAvailabl: Boolean;
begin
  horzAvailable:= _manager.isAvailableScrollBar( _horzScrollBar );
  vertAvailabl:= _manager.isAvailableScrollBar( _vertScrollBar );

  self.resetManager;

  _manager.availScrollBar( _horzScrollBar, horzAvailable );
  _manager.availScrollBar( _vertScrollBar, vertAvailabl );

  if self.lclGetTarget is TWinControl then
    TWinControl(self.lclGetTarget).AdjustSize;

  _manager.updateLayout;

  _manager.showScrollBar(_horzScrollBar);
  _manager.showScrollBar(_vertScrollBar);
end;

function TCocoaManualScrollView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaManualScrollView.lclClearCallback;
begin
  callback := nil;
  if Assigned(self.documentView) then
    self.documentView.lclClearCallback;
end;

function TCocoaManualScrollView.lclContentView: NSView;
begin
  Result:=_documentView;
end;

function TCocoaManualScrollView.lclClientFrame: TRect;
begin
  if Assigned(_documentView) then
  begin
    Result:=_documentView.lclClientFrame;
  end
  else Result:=inherited lclClientFrame;
end;

function TCocoaManualScrollView.lclIsMouseInAuxArea(event: NSEvent): Boolean;
begin
  Result := TCocoaScrollingUtil.isMouseEventInScrollBar(Self, event);
end;

procedure TCocoaManualScrollView.lclUpdate;
begin
  documentView.lclUpdate;
end;

procedure TCocoaManualScrollView.lclInvalidateRect(const r: TRect);
begin
  documentView.lclInvalidateRect(r);
end;

procedure TCocoaManualScrollView.lclInvalidate;
begin
  documentView.lclInvalidate;
end;

procedure TCocoaManualScrollView.setDocumentView(AView: NSView);
var
  f  : NSrect;
begin
  if _documentView=AView then Exit;
  if Assigned(_documentView) then
    _documentView.removeFromSuperview;

  _documentView:=AView;
  if Assigned(_documentView) then
  begin
    addSubview(_documentView);
    f:=_documentView.frame;
    f.origin.x:=0;
    f.origin.y:=0;
    _documentView.setFrame(f);
    _documentView.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
  end;
end;

function TCocoaManualScrollView.documentView: NSView;
begin
  Result:=_documentView;
end;

function TCocoaManualScrollView.isTapping(scroller: NSScroller): Boolean;
begin
  if _tapping < 0 then
    Exit( False );
  if _tapping = SB_BOTH then
    Exit( True );
  if (_tapping = SB_HORZ) and (scroller=_horzScrollBar) then
    Exit( True );
  if (_tapping = SB_VERT) and (scroller=_vertScrollBar) then
    Exit( True );
  Result:= False;
end;

procedure TCocoaManualScrollView.onBarScrolled(scroller: NSScroller);
begin
  if _tapping < 0 then
    Exit;

  if scroller=_vertScrollBar then begin
    _tapping:= SB_VERT;
    _manager.tempHideScrollBar( _horzScrollBar );
  end else if scroller=_horzScrollBar then begin
    _tapping:= SB_HORZ;
    _manager.tempHideScrollBar( _vertScrollBar );
  end;
end;

procedure TCocoaManualScrollView.delayShowScrollBars();
begin
  _manager.showScrollBar( _horzScrollBar, False );
  _manager.showScrollBar( _vertScrollBar, False );
end;

procedure TCocoaManualScrollView.showScrollBarsAndAutoHide( tapping:Integer );
begin
  if (tapping=SB_HORZ) or (tapping=SB_BOTH) then
    _manager.showScrollBar(_horzScrollBar);
  if (tapping=SB_VERT) or (tapping=SB_BOTH) then
    _manager.showScrollBar(_vertScrollBar);
end;

procedure TCocoaManualScrollView.touchesBeganWithEvent(event: NSEvent);
var
  touches: NSSet;
  show: Boolean = False;
begin
  inherited;

  touches:= event.touchesMatchingPhase_inView(NSTouchPhaseTouching, self);
  if touches.count = 2 then begin
    show:= True;
    _tapping:= SB_BOTH;
  end else begin
    if _tapping >= 0 then
      show:= True;
    _tapping:= -1;
  end;

  if show then
    delayShowScrollBars;
end;

procedure TCocoaManualScrollView.touchesEndedWithEvent(event: NSEvent);
var
  touches: NSSet;
  lastTapping: Integer;
  show: Boolean = False;
begin
  inherited;

  touches:= event.touchesMatchingPhase_inView(NSTouchPhaseTouching, self);
  if (touches.count=0) and (_tapping=SB_BOTH) then begin
    _tapping:= -1;
    show:= True;
  end else if (touches.count=2) and (_tapping<>SB_BOTH) then begin
    _tapping:= SB_BOTH;
    show:= True;
  end else if (touches.count<>2) and (_tapping>=0) then begin
    lastTapping:= _tapping;
    _tapping:= -1;
    showScrollBarsAndAutoHide( lastTapping );
  end else begin
    _tapping:= -1;
  end;

  if show then
    delayShowScrollBars;
end;

procedure TCocoaManualScrollView.touchesCancelledWithEvent(event: NSEvent);
var
  lastTapping: Integer;
begin
  inherited;
  lastTapping:= _tapping;
  _tapping:= -1;
  showScrollBarsAndAutoHide( lastTapping );
end;

procedure TCocoaManualScrollView.setHasVerticalScroller(doshow: Boolean);
var
  available: Boolean;
begin
  available:= _manager.isAvailableScrollBar(_vertScrollBar);
  if NOT Assigned(_vertScrollBar) and doshow then
    _vertScrollBar:= self.allocVerticalScroller( True );
  _manager.availScrollBar( _vertScrollBar, doshow );
  if available <> _manager.isAvailableScrollBar(_vertScrollBar) then
    _manager.updateLayout;
end;

procedure TCocoaManualScrollView.setHasHorizontalScroller(doshow: Boolean);
var
  available: Boolean;
begin
  available:= _manager.isAvailableScrollBar(_horzScrollBar);
  if NOT Assigned(_horzScrollBar) and doshow then
    _horzScrollBar:= self.allocHorizontalScroller( True );
  _manager.availScrollBar( _horzScrollBar, doshow );
  if available <> _manager.isAvailableScrollBar(_horzScrollBar) then
    _manager.updateLayout;
end;

function TCocoaManualScrollView.hasVerticalScroller: Boolean;
begin
  Result:= _manager.isAvailableScrollBar(_vertScrollBar);
end;

function TCocoaManualScrollView.hasHorizontalScroller: Boolean;
begin
  Result:= _manager.isAvailableScrollBar(_horzScrollBar);
end;

function TCocoaManualScrollView.horizontalScroller: NSScroller;
begin
  Result:=_horzScrollBar;
end;

function TCocoaManualScrollView.verticalScroller: NSScroller;
begin
  Result:=_vertScrollBar;
end;

function TCocoaManualScrollView.allocHorizontalScroller(avisible: Boolean): TCocoaScrollBar;
var
  r : NSRect;
  f : NSRect;
  w : CGFloat;
begin
  if Assigned(_horzScrollBar) then
    Result := _horzScrollBar
  else
  begin
    f := frame;
    w := NSScroller.scrollerWidthForControlSize_scrollerStyle(
           _horzScrollBar.controlSize, _horzScrollBar.preferredScrollerStyle);
    r := NSMakeRect(0, 0, Max(f.size.width,w+1), w); // width<height to create a horizontal scroller
    _horzScrollBar := TCocoaScrollingUtil.createScroller( self, r );
        _horzScrollBar.setAutoresizingMask(NSViewWidthSizable);
    Result := _horzScrollBar;
    _manager.updateLayout;
  end;
end;

function TCocoaManualScrollView.allocVerticalScroller(avisible: Boolean): TCocoaScrollBar;
var
  r : NSRect;
  f : NSRect;
  w : CGFloat;
begin
  if Assigned(_vertScrollBar) then
    Result := _vertScrollBar
  else
  begin
    f := frame;
    w := NSScroller.scrollerWidthForControlSize_scrollerStyle(
           _vertScrollBar.controlSize, _vertScrollBar.preferredScrollerStyle);
    r := NSMakeRect(0, 0, w, Max(f.size.height,w+1)); // height<width to create a vertical scroller
    _vertScrollBar := TCocoaScrollingUtil.createScroller( self, r );
    if self.isFlipped then
      _vertScrollBar.setAutoresizingMask(NSViewHeightSizable or NSViewMaxXMargin)
    else
      _vertScrollBar.setAutoresizingMask(NSViewHeightSizable or NSViewMinXMargin);
    Result := _vertScrollBar;
    _manager.updateLayout;
  end;
end;

function TCocoaManualScrollView.acceptsFirstMouse(event: NSEvent): LCLObjCBoolean;
begin
  Result:=true;
end;

{ TCocoaManualScrollHost }

function TCocoaManualScrollHost.lclContentView: NSView;
begin
  if Assigned(documentView) then
    Result := documentView.lclContentView
  else
    Result := inherited lclContentView;
end;

function TCocoaManualScrollHost.lclClientFrame: TRect;
begin
  if Assigned(documentView) then
  begin
    Result:=documentView.lclClientFrame;
  end
  else Result:=inherited lclClientFrame;
end;

procedure TCocoaManualScrollHost.scrollWheel(theEvent: NSEvent);
var
  nr : NSResponder;
begin
  nr := nextResponder;
  // do not call inherited scrollWheel, it suppresses the scroll event
  if Assigned(nr) then nr.scrollWheel(theEvent)
  else inherited scrollWheel(theEvent);
end;

procedure TCocoaManualScrollHost.setFrame(newValue: NSRect);
var
  sc: NSView;
  scFrame: NSRect;
begin
  inherited setFrame(newValue);
  sc:= self.documentView;
  scFrame.origin:= NSZeroPoint;
  scFrame.size:= self.contentSize;
  sc.setFrame( scFrame );
end;

procedure TCocoaManualScrollHost.setScrollerStyle(newValue: NSScrollerStyle);
begin
  inherited setScrollerStyle(newValue);
  if Assigned(self.lclGetTarget) and Assigned(self.documentView) then
    TCocoaManualScrollView(self.documentView).onScrollerStyleUpdated;
end;

{ TCocoaScrollingUtil }

class procedure TCocoaScrollingUtil.setBorderStyle(
  const sv: NSScrollView;
  const astyle: TBorderStyle );
const
  NSBorderStyle : array [TBorderStyle] of NSBorderType = (
    NSNoBorder,   // bsNone
    NSBezelBorder // bsSingle     (NSLineBorder is too thick)
  );
begin
  if not Assigned(sv) then Exit;
  sv.setBorderType( NSBorderStyle[astyle] );
end;

class procedure TCocoaScrollingUtil.setScrollStyle(
  const AScroll: TCocoaScrollView; const AStyles: TScrollStyle);
begin
  AScroll.setHasVerticalScroller(VERT_SCROLLER_VISIBLE[AStyles]);
  AScroll.setHasHorizontalScroller(HORZ_SCROLLER_VISIBLE[AStyles]);
  AScroll.setAutohidesScrollers(SCROLLER_AUTO_HIDE_STYLE[AStyles]);
end;

class function TCocoaScrollingUtil.createScroller(
  const parent: TCocoaManualScrollView;
  const dst: NSRect ): TCocoaScrollBar;
var
  scrollBar: TCocoaScrollBar Absolute Result;
begin
  scrollBar:= TCocoaScrollBar(TCocoaScrollBar.alloc).initWithFrame(dst);
  scrollBar.setManager( parent._manager );
  parent.addSubview(scrollBar);
  scrollBar.preventBlock := true;
  //Suppress scrollers notifications.
  scrollBar.callback := parent.callback;
  scrollBar.suppressLCLMouse := true;
  scrollBar.setTarget(scrollBar);
  scrollBar.setAction(objcselector('actionScrolling:'));
end;

class function TCocoaScrollingUtil.isMouseEventInScrollBar(
  const host: TCocoaManualScrollView;
  const event: NSEvent ): Boolean;
var
  pt : NSPoint;
begin
  Result := false;
  if Assigned(host._horzScrollBar) and (not host._horzScrollBar.isHidden) then
  begin
    pt := host._horzScrollBar.convertPoint_fromView(event.locationInWindow, nil);
    if NSPointInRect(pt, host._horzScrollBar.bounds) then
    begin
      Result := true;
      Exit;
    end;
  end;

  if Assigned(host._vertScrollBar) and (not host._vertScrollBar.isHidden) then
  begin
    pt := host._vertScrollBar.convertPoint_fromView(event.locationInWindow, nil);
    if NSPointInRect(pt, host._vertScrollBar.bounds) then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

end.

