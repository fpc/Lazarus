{ $Id: $}
{                  --------------------------------------------
                  cocoascrollers.pas  -  Cocoa internal classes
                  --------------------------------------------

 This unit contains the private classhierarchy for the Cocoa implemetations

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaScrollers;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$modeswitch objectivec2}
{$interfaces corba}
{$include cocoadefines.inc}

interface

uses
  // rtl+ftl
  Math, Classes, SysUtils, LclType,
  // Libs
  Controls, Forms,
  MacOSAll, CocoaAll, CocoaUtils, CocoaPrivate;

type
  { TCocoaScrollView }

  TCocoaScrollView = objcclass(NSScrollView)
  public
    callback: ICommonCallback;
    isCustomRange: Boolean;

    // the corresponding LCL ScrollInfo,
    // which are needed when documentView.frame changes.
    lclHoriScrollInfo: TScrollInfo;
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

  // it would be more appropriate to use Interface to define the interface of
  // each component, but FreePascal's Interface is so weak that it can only be
  // replaced by abstract classes here.

  { TCocoaScrollBarEffect }

  TCocoaScrollBarEffect = objcclass(NSObject)
  protected
    procedure onDestroy; message 'onDestroy';
  end;

  { TCocoaScrollBarStyleManager }

  TCocoaScrollBarStyleManager = class
    procedure onKnobValueUpdated( scroller:NSScroller;
      var knobPosition:Double; var knobProportion:Double ); virtual; abstract;
    procedure onDrawKnob( scroller:NSScroller ); virtual; abstract;
    function onDrawKnobSlot( scroller:NSScroller; var slotRect: NSRect ):
      Boolean; virtual; abstract;

    procedure onMouseEntered( scroller:NSScroller ); virtual; abstract;
    procedure onMouseExited( scroller:NSScroller ); virtual; abstract;

    function createScrollBarEffect( scroller:NSScroller ):
      TCocoaScrollBarEffect; virtual; abstract;
    procedure activeScrollBar( scroller:NSScroller; active:Boolean ); virtual; abstract;
  end;

  TCocoaManualScrollView = objcclass;

  { TCocoaScrollStyleManager }

  TCocoaScrollStyleManager = class(TCocoaScrollBarStyleManager)
  protected
    _scrollView: TCocoaManualScrollView;
  public
    procedure updateLayout; virtual; abstract;
  public
    constructor createForScrollBar;
    constructor createForScrollView( scrollView:TCocoaManualScrollView );
  end;

  { TCocoaScrollBar }

  TCocoaScrollBar = objcclass(NSScroller)
  private
    manager: TCocoaScrollBarStyleManager;
    effect: TCocoaScrollBarEffect;
    trackingArea: NSTrackingArea;
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
    manager: TCocoaScrollStyleManager;
    fdocumentView: NSView;
    fhscroll : TCocoaScrollBar;
    fvscroll : TCocoaScrollBar;
  public
    callback: ICommonCallback;
    function initWithFrame(frameRect: NSRect): id; override;
    procedure dealloc; override;
    procedure setFrame(newValue: NSRect); override;

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
  end;

function createLegacyScroller: TCocoaScrollBar;

function isMouseEventInScrollBar(host: TCocoaManualScrollView; event: NSEvent): Boolean;

// These settings are set by a user in "System Preferences"
// One can check the values by running the command line:
// $defaults read
// "AppleShowScrollBars": Automatic, Always, WhenScrolling
function SysPrefScrollShow: string;
// "AppleScrollerPagingBehavior": 0 - adjust by page, 1 - jump to the position
function SysPrefScrollClick: Integer;

function isIncDecPagePart(prt: NSScrollerPart): Boolean; inline;
procedure HandleMouseDown(sc: TCocoaScrollBar; locInWin: NSPoint; prt: NSScrollerPart);
function AdjustScrollerArrow(sc: TCocoaScrollBar; prt: NSScrollerPart): Boolean;
function AdjustScrollerPage(sc: TCocoaScrollBar; prt: NSScrollerPart): Boolean;

implementation

type

  { TCocoaScrollBarEffectAlpha }

  TCocoaScrollBarEffectAlpha = objcclass(TCocoaScrollBarEffect)
  private
    procedure onAlphaTimer( timer:NSTimer ); message 'onAlphaTimer:';
  protected
    currentAlpha: Double;
    stepAlpha: Double;
    maxAlpha: Double;

    manager: TCocoaScrollBarStyleManager;
    scroller: NSScroller;
    alphaTimer: NSTimer;
  protected
    procedure fade( inc:Double; max:Double );
      message 'fadeInc:max:';
    procedure onDestroy; override;
  end;

  { TCocoaScrollBarEffectOverlay }

  TCocoaScrollBarEffectOverlay = objcclass(TCocoaScrollBarEffectAlpha)
  private
    procedure onDelayHidingTimer( timer:NSTimer ); message 'onDelayHidingTimer:';
    procedure onExpandTimer( timer:NSTimer ); message 'onExpandTimer:';
  protected
    currentKnobPosition: Double;
    currentKnobProportion: Double;
    entered: Boolean;
    expandedSize: Integer;

    delayHidingTimer: NSTimer;
    expandTimer: NSTimer;
  protected
    procedure setDelayHidingTimer; message 'setDelayHidingTimer';
    procedure setExpandTimer; message 'setExpandTimer';

    procedure onDestroy; override;
  end;

  { TCocoaScrollStyleManagerLegacy }

  TCocoaScrollStyleManagerLegacy = class(TCocoaScrollStyleManager)
  public
    procedure onKnobValueUpdated( scroller:NSScroller;
      var knobPosition:Double; var knobProportion:Double ); override;
    procedure onDrawKnob( scroller:NSScroller );  override;
    function onDrawKnobSlot( scroller:NSScroller; var slotRect: NSRect ):
      Boolean; override;
    procedure onMouseEntered( scroller:NSScroller );  override;
    procedure onMouseExited( scroller:NSScroller ); override;
    function createScrollBarEffect( scroller:NSScroller ): TCocoaScrollBarEffect; override;
    procedure activeScrollBar( scroller:NSScroller; active:Boolean ); override;
    procedure updateLayOut; override;
  end;

  { TCocoaScrollStyleManagerOverlay }

  TCocoaScrollStyleManagerOverlay = class(TCocoaScrollStyleManager)
  public
    procedure onKnobValueUpdated( scroller:NSScroller;
      var knobPosition:Double; var knobProportion:Double ); override;
    procedure onDrawKnob( scroller:NSScroller );  override;
    function onDrawKnobSlot( scroller:NSScroller; var slotRect: NSRect ):
      Boolean; override;
    procedure onMouseEntered( scroller:NSScroller );  override;
    procedure onMouseExited( scroller:NSScroller ); override;
    function createScrollBarEffect( scroller:NSScroller ): TCocoaScrollBarEffect; override;
    procedure activeScrollBar( scroller:NSScroller; active:Boolean ); override;
    procedure updateLayOut; override;
  end;

function SysPrefScrollShow: string;
begin
  Result := NSStringToString(NSUserDefaults.standardUserDefaults.stringForKey(NSSTR('AppleShowScrollBars')));
end;

function SysPrefScrollClick: Integer; // 0 - adjust by page, 1 - jump to the position
begin
  Result := Integer(NSUserDefaults.standardUserDefaults.integerForKey(NSSTR('AppleScrollerPagingBehavior')));
end;

function isIncDecPagePart(prt: NSScrollerPart): Boolean; inline;
begin
  Result := (prt = NSScrollerDecrementPage)
         or (prt = NSScrollerIncrementPage)
         or (prt = NSScrollerDecrementLine)
         or (prt = NSScrollerIncrementLine);
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

function AdjustScrollerArrow(sc: TCocoaScrollBar; prt: NSScrollerPart): Boolean;
var
  adj : Integer;
  sz  : Integer;
  dlt : double;
  v   : double;
begin
  Result := (prt = NSScrollerDecrementLine)
         or (prt = NSScrollerIncrementLine);
  if not Result then Exit;

  sz := sc.maxInt - sc.minInt - sc.pageInt;
  if sz = 0 then Exit; // do nothing!

  if prt = NSScrollerDecrementLine
    then adj := -1
    else adj := 1;
  dlt := 1 / sz * adj;

  v := sc.doubleValue;
  v := v + dlt;
  if v < 0 then v := 0
  else if v > 1 then v := 1;

  if v <> sc.doubleValue then
  begin
  // todo: call scroll event?
    sc.setDoubleValue(v);
    {$ifdef BOOLFIX}
    sc.setNeedsDisplay__(Ord(true));
    {$else}
    sc.setNeedsDisplay_(true);
    {$endif}
  end;
end;


procedure HandleMouseDown(sc: TCocoaScrollBar; locInWin: NSPoint; prt: NSScrollerPart);
var
  adj : Integer;
  sz  : Integer;
  pt  : NSPoint;
  ps  : double;
  newPos: Integer;
begin
  if (prt = NSScrollerDecrementLine) or (prt = NSScrollerIncrementLine) then
  begin
    AdjustScrollerArrow(sc, prt);
    Exit;
  end;
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

// for the Scroller created separately through TCocoaWSScrollBar.CreateHandle(),
// due to the lack of the control over the Layout by TCocoaManualScrollView,
// only the Legacy Style can be used for compatibility.
// it's the same logical relationship as NSScrollView and NSScroller.
function createLegacyScroller: TCocoaScrollBar;
begin
  Result:= TCocoaScrollBar(TCocoaScrollBar.alloc);
  Result.manager:= TCocoaScrollStyleManagerLegacy.createForScrollBar;
  Result.effect:= Result.manager.createScrollBarEffect(Result);
end;

function allocScroller(parent: TCocoaManualScrollView; dst: NSRect; aVisible: Boolean)
  :TCocoaScrollBar;
var
  scrollBar: TCocoaScrollBar;
begin
  scrollBar:= TCocoaScrollBar(TCocoaScrollBar.alloc).initWithFrame(dst);
  scrollBar.manager:= parent.manager;
  scrollBar.effect:= parent.manager.createScrollBarEffect(scrollBar);
  parent.addSubview(scrollBar);
  {$ifdef BOOLFIX}
  scrollBar.setEnabled_(Ord(true));
  scrollBar.setHidden_(Ord(not AVisible));
  {$else}
  scrollBar.setEnabled(true);
  scrollBar.setHidden(not AVisible);
  {$endif}
  scrollBar.preventBlock := true;
  //Suppress scrollers notifications.
  scrollBar.callback := parent.callback;
  scrollBar.suppressLCLMouse := true;
  scrollBar.setTarget(scrollBar);
  scrollBar.setAction(objcselector('actionScrolling:'));
  Result:= scrollBar;
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
  sc: TCocoaManualScrollView;
  scFrame: NSRect;
begin
  inherited setFrame(newValue);
  sc:= TCocoaManualScrollView(self.documentView);
  scFrame.origin:= NSZeroPoint;
  scFrame.size:= self.contentSize;
  sc.setFrame( scFrame );
end;

{ TCocoaManualScrollView }

function TCocoaManualScrollView.initWithFrame(frameRect: NSRect): id;
begin
  Result:= inherited;
  self.manager:= TCocoaScrollStyleManagerOverlay.createForScrollView(self);
end;

procedure TCocoaManualScrollView.dealloc;
begin
  if Assigned(fhscroll) then begin
    fhscroll.removeFromSuperview;
    fhscroll.manager:= nil;
    fhscroll.release;
  end;
  if Assigned(fvscroll) then begin
    fvscroll.removeFromSuperview;
    fvscroll.manager:= nil;
    fvscroll.release;
  end;
  FreeAndNil(manager);
end;

procedure TCocoaManualScrollView.setFrame(newValue: NSRect);
begin
  inherited setFrame(newValue);
  manager.updateLayout;
end;

function TCocoaManualScrollView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaManualScrollView.lclClearCallback;
begin
  callback := nil;
end;

function TCocoaManualScrollView.lclContentView: NSView;
begin
  Result:=fdocumentView;
end;

function TCocoaManualScrollView.lclClientFrame: TRect;
begin
  if Assigned(fdocumentView) then
  begin
    Result:=fdocumentView.lclClientFrame;
  end
  else Result:=inherited lclClientFrame;
end;

function TCocoaManualScrollView.lclIsMouseInAuxArea(event: NSEvent): Boolean;
begin
  Result := isMouseEventInScrollBar(Self, event);
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
  if fdocumentView=AView then Exit;
  if Assigned(fdocumentView) then
    fdocumentView.removeFromSuperview;

  fdocumentView:=AView;
  if Assigned(fdocumentView) then
  begin
    addSubview(fdocumentView);
    f:=fdocumentView.frame;
    f.origin.x:=0;
    f.origin.y:=0;
    fdocumentView.setFrame(f);
    fdocumentView.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
  end;
end;

function TCocoaManualScrollView.documentView: NSView;
begin
  Result:=fdocumentView;
end;

procedure TCocoaManualScrollView.setHasVerticalScroller(doshow: Boolean);
begin
  if NOT Assigned(fvscroll) and doshow then
    fvscroll:= self.allocVerticalScroller( True );
  manager.activeScrollBar( fvscroll, doshow );
  manager.updateLayout;
end;

procedure TCocoaManualScrollView.setHasHorizontalScroller(doshow: Boolean);
begin
  if NOT Assigned(fhscroll) and doshow then
    fhscroll:= self.allocHorizontalScroller( True );
  manager.activeScrollBar( fhscroll, doshow );
  manager.updateLayout;
end;

function TCocoaManualScrollView.hasVerticalScroller: Boolean;
begin
  Result:=Assigned(fvscroll) and (not fvscroll.isHidden);
end;

function TCocoaManualScrollView.hasHorizontalScroller: Boolean;
begin
  Result:=Assigned(fhscroll) and (not fhscroll.isHidden);
end;

function TCocoaManualScrollView.horizontalScroller: NSScroller;
begin
  Result:=fhscroll;
end;

function TCocoaManualScrollView.verticalScroller: NSScroller;
begin
  Result:=fvscroll;
end;

function TCocoaManualScrollView.allocHorizontalScroller(avisible: Boolean): TCocoaScrollBar;
var
  r : NSRect;
  f : NSRect;
  w : CGFloat;
begin
  if Assigned(fhscroll) then
    Result := fhscroll
  else
  begin
    f := frame;
    w := NSScroller.scrollerWidthForControlSize_scrollerStyle(
           fhscroll.controlSize, fhscroll.preferredScrollerStyle);
    r := NSMakeRect(0, 0, Max(f.size.width,w+1), w); // width<height to create a horizontal scroller
    fhscroll := allocScroller( self, r, avisible);
        fhscroll.setAutoresizingMask(NSViewWidthSizable);
    Result := fhscroll;
  end;
end;

function TCocoaManualScrollView.allocVerticalScroller(avisible: Boolean): TCocoaScrollBar;
var
  r : NSRect;
  f : NSRect;
  w : CGFloat;
begin
  if Assigned(fvscroll) then
    Result := fvscroll
  else
  begin
    f := frame;
    w := NSScroller.scrollerWidthForControlSize_scrollerStyle(
           fvscroll.controlSize, fvscroll.preferredScrollerStyle);
    r := NSMakeRect(0, 0, w, Max(f.size.height,w+1)); // height<width to create a vertical scroller
    fvscroll := allocScroller( self, r, avisible);
    if self.isFlipped then
      fvscroll.setAutoresizingMask(NSViewHeightSizable or NSViewMaxXMargin)
    else
      fvscroll.setAutoresizingMask(NSViewHeightSizable or NSViewMinXMargin);
    Result := fvscroll;
  end;
end;

function TCocoaManualScrollView.acceptsFirstMouse(event: NSEvent): LCLObjCBoolean;
begin
  Result:=true;
end;

function isMouseEventInScrollBar(host: TCocoaManualScrollView; event: NSEvent): Boolean;
var
  pt : NSPoint;
begin
  Result := false;
  if Assigned(host.fhscroll) and (not host.fhscroll.isHidden) then
  begin
    pt := host.fhscroll.convertPoint_fromView(event.locationInWindow, nil);
    if NSPointInRect(pt, host.fhscroll.bounds) then
    begin
      Result := true;
      Exit;
    end;
  end;

  if Assigned(host.fvscroll) and (not host.fvscroll.isHidden) then
  begin
    pt := host.fvscroll.convertPoint_fromView(event.locationInWindow, nil);
    if NSPointInRect(pt, host.fvscroll.bounds) then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

{ TCocoaScrollView }

function TCocoaScrollView.lclClientFrame: TRect;
begin
  NSToLCLRect(contentView.frame, frame.size.height, Result);
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
  lclHoriScrollInfo.fMask:= 0;
  lclVertScrollInfo.fMask:= 0;
end;

procedure TCocoaScrollView.scrollWheel(theEvent: NSEvent);
begin
  if self.hasHorizontalScroller or self.hasVerticalScroller then begin
    inherited scrollWheel( theEvent );
    callback.scrollWheel( theEvent );
  end else if Assigned(self.enclosingScrollView) then
    self.enclosingScrollView.scrollWheel( theEvent )
  else
    inherited scrollWheel( theEvent );
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
    if not self.documentView.isFlipped then
      newOrigin.y := self.documentView.frame.size.height - scrollInfo.nPos - self.contentSize.height
    else
      newOrigin.y := scrollInfo.nPos;
  end
  else
  begin
    self.lclHoriScrollInfo:= scrollInfo;
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

  lclControl:= TScrollingWinControl(lclGetTarget);
  newDocSize:= contentSize;

  lclBar:= lclControl.HorzScrollBar;
  if lclBar.Visible and (lclBar.Range<>0) and (lclBar.Range>lclBar.Page) then
    newDocSize.Width:= lclBar.Range;

  lclBar:= lclControl.VertScrollBar;
  if lclBar.Visible and (lclBar.Range<>0) and (lclBar.Range>lclBar.Page) then
    newDocSize.Height:= lclBar.Range;

  documentView.setFrameSize(newDocSize);

  if lclHoriScrollInfo.fMask<>0 then
    applyScrollInfo(SB_Horz, lclHoriScrollInfo);

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
end;

{ TCocoaScrollBar }

procedure TCocoaScrollBar.dealloc;
begin
  FreeAndNil(manager);
  effect.onDestroy;
  effect.release;
  inherited dealloc;
end;

procedure TCocoaScrollBar.drawKnob;
var
  rect: NSRect;
  path: NSBezierPath;
begin
  self.manager.onDrawKnob(self);
end;

procedure TCocoaScrollBar.drawKnobSlotInRect_highlight(slotRect: NSRect;
  flag: ObjCBOOL);
var
  drawSlot: Boolean;
begin
  drawSlot:= self.manager.onDrawKnobSlot(self, slotRect);
  if drawSlot then
    Inherited;
end;

procedure TCocoaScrollBar.setDoubleValue(newValue: double);
var
  proportion: CGFloat;
begin
  proportion:= self.knobProportion;
  manager.onKnobValueUpdated( self, newValue, proportion );
  inherited;
end;

procedure TCocoaScrollBar.setKnobProportion(newValue: CGFloat);
var
  position: CGFloat;
begin
  position:= self.doubleValue;
  manager.onKnobValueUpdated( self, position, newValue );
  inherited;
end;

procedure TCocoaScrollBar.updateTrackingAreas;
var
  i: NSTrackingArea;
const
  options: NSTrackingAreaOptions = NSTrackingMouseEnteredAndExited
                                or NSTrackingActiveAlways;
begin
  if Assigned(trackingArea) then begin
    removeTrackingArea(trackingArea);
    trackingArea:= nil;
  end;

  trackingArea:= NSTrackingArea.alloc.initWithRect_options_owner_userInfo(
                 self.bounds,
                 options,
                 self,
                 nil);
  self.addTrackingArea( trackingArea );
  trackingArea.release;
end;

procedure TCocoaScrollBar.mouseEntered(theEvent: NSEvent);
begin
  manager.onMouseEntered(self);
end;

procedure TCocoaScrollBar.mouseExited(theEvent: NSEvent);
begin
  manager.onMouseExited( self );
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
  if isIncDecPagePart(prt) then
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

{ TCocoaScrollBarEffectAlpha }

procedure TCocoaScrollBarEffectAlpha.onAlphaTimer(timer: NSTimer);
var
  done: Boolean = false;
begin
  if timer<>alphaTimer then begin
    timer.invalidate;
    Exit;
  end;

  self.currentAlpha:= self.currentAlpha + self.stepAlpha;
  if self.stepAlpha > 0 then begin
    if self.currentAlpha >= self.maxAlpha then begin
      self.currentAlpha:= self.maxAlpha;
      done:= True;
    end;
  end else begin
    if self.currentAlpha <= self.maxAlpha then begin
      self.currentAlpha:= self.maxAlpha;
      done:= True;
    end;
  end;

  if done then begin
    timer.invalidate;
    alphaTimer:= nil;
  end;

  self.scroller.setNeedsDisplay_(True);
end;

procedure TCocoaScrollBarEffectAlpha.fade( inc: Double; max: Double );
begin
  if Assigned(self.alphaTimer) then
    self.alphaTimer.invalidate;

  self.stepAlpha:= inc;
  self.maxAlpha:= max;
  self.alphaTimer:= NSTimer.scheduledTimerWithTimeInterval_target_selector_userInfo_repeats(
    0.02,
    self,
    ObjCSelector('onAlphaTimer:'),
    nil,
    true );
end;

procedure TCocoaScrollBarEffectAlpha.onDestroy;
begin
  if Assigned(self.alphaTimer) then begin
    self.alphaTimer.invalidate;
    self.alphaTimer:= nil;
  end;
end;

{ TCocoaScrollBarEffectOverlay }

procedure TCocoaScrollBarEffectOverlay.onDelayHidingTimer( timer:NSTimer );
begin
  self.delayHidingTimer:= nil;
  self.fade( -0.1, 0 );
end;

procedure TCocoaScrollBarEffectOverlay.onExpandTimer(timer: NSTimer);
var
  done: Boolean = false;
begin
  if timer<>expandTimer then begin
    timer.invalidate;
    Exit;
  end;

  if self.expandedSize < 4 then begin
    self.expandedSize:= self.expandedSize + 1;
  end else begin
    done:= True;
  end;

  if done then begin
    timer.invalidate;
    self.expandTimer:= nil;
  end;

  self.scroller.setNeedsDisplay_(True);
end;

procedure TCocoaScrollBarEffectOverlay.setDelayHidingTimer;
begin
  if Assigned(self.delayHidingTimer) then
    self.delayHidingTimer.invalidate;

  self.delayHidingTimer:= NSTimer.scheduledTimerWithTimeInterval_target_selector_userInfo_repeats(
    0.9,
    self,
    ObjCSelector('onDelayHidingTimer:'),
    nil,
    false );
end;

procedure TCocoaScrollBarEffectOverlay.setExpandTimer;
begin
  if Assigned(self.delayHidingTimer) then begin
    self.delayHidingTimer.invalidate;
    self.delayHidingTimer:= nil;
  end;

  if Assigned(self.expandTimer) then
    self.expandTimer.invalidate;

  self.expandTimer:= NSTimer.scheduledTimerWithTimeInterval_target_selector_userInfo_repeats(
    0.03,
    self,
    ObjCSelector('onExpandTimer:'),
    nil,
    True );
end;

procedure TCocoaScrollBarEffectOverlay.onDestroy;
begin
  if Assigned(self.delayHidingTimer) then begin
    self.delayHidingTimer.invalidate;
    self.delayHidingTimer:= nil;
  end;
  if Assigned(self.expandTimer) then begin
    self.expandTimer.invalidate;
    self.expandTimer:= nil;
  end;
  inherited onDestroy;
end;

{ TCocoaScrollStyleManager }

constructor TCocoaScrollStyleManager.createForScrollBar;
begin
end;

constructor TCocoaScrollStyleManager.createForScrollView(scrollView: TCocoaManualScrollView
  );
begin
  _scrollView:= scrollView;
end;

{ TCocoaScrollViewStyleManagerLegacy }

procedure TCocoaScrollStyleManagerLegacy.updateLayOut;
var
  doc: NSView;
  docFrame  : NSRect;
  hScroller: NSScroller;
  vScroller: NSScroller;
  hScrollerFrame : NSRect;
  vScrollerFrame : NSRect;
  hScrollerHeight : CGFLoat;
  vScrollerWidth : CGFLoat;
begin
  doc:= _scrollView.documentView;
  if NOT Assigned(doc) then
    Exit;

  docFrame := _scrollView.frame;
  docFrame.origin := NSZeroPoint;
  hScrollerFrame := docFrame;
  vScrollerFrame := docFrame;

  hScroller:= _scrollView.fhscroll;
  vScroller:= _scrollView.fvscroll;

  if Assigned(hScroller) and (not hScroller.isHidden) then
  begin
    hScrollerHeight := NSScroller.scrollerWidthForControlSize_scrollerStyle(
            hScroller.controlSize, hScroller.preferredScrollerStyle);
    hScrollerFrame.size.height := hScrollerHeight;

    docFrame.size.height := docFrame.size.height - hScrollerHeight;
    if docFrame.size.height < 0 then
      docFrame.size.height := 0;
    docFrame.origin.y := hScrollerHeight;
  end;

  if Assigned(vScroller) and (not vScroller.isHidden) then
  begin
    vScrollerWidth := NSScroller.scrollerWidthForControlSize_scrollerStyle(
            vScroller.controlSize, vScroller.preferredScrollerStyle);
    vScrollerFrame.size.width := vScrollerWidth;

    docFrame.size.width := docFrame.size.width - vScrollerWidth;
    if docFrame.size.width < 0 then
      docFrame.size.width:= 0;
  end;

  hScrollerFrame.size.width := docFrame.size.width;
  vScrollerFrame.size.height := docFrame.size.height;
  vScrollerFrame.origin.x := docFrame.size.width;
  vScrollerFrame.origin.y := docFrame.origin.y;

  if Assigned(hScroller) then
    hScroller.setFrame(hScrollerFrame);

  if Assigned(vScroller) then
    vScroller.setFrame(vScrollerFrame);

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

function TCocoaScrollStyleManagerLegacy.createScrollBarEffect( scroller:NSScroller ):
  TCocoaScrollBarEffect;
var
  effect: TCocoaScrollBarEffectAlpha;
begin
  effect:= TCocoaScrollBarEffectAlpha.alloc.Init;
  effect.scroller:= scroller;
  effect.manager:= self;
  effect.currentAlpha:= 0.25;
  Result:= effect;
end;

procedure TCocoaScrollStyleManagerLegacy.activeScrollBar(
  scroller: NSScroller; active: Boolean);
begin
  if NOT active then begin
    if Assigned(scroller) and NOT scroller.isHidden then
      scroller.setHidden( True );
    Exit;
  end;

  if NOT Assigned(scroller) then
    scroller := _scrollView.allocVerticalScroller(true);

  if scroller.isHidden then
  begin
    scroller.setHidden(false);
    scroller.setAlphaValue(1);
  end;
end;

procedure TCocoaScrollStyleManagerLegacy.onDrawKnob(scroller: NSScroller);
var
  scrollBar: TCocoaScrollBar Absolute scroller;
  rect: NSRect;
  path: NSBezierPath;
  color: CIColor;
  alpha: CGFloat;
begin
  rect:= scrollBar.rectForPart(NSScrollerKnob);
  rect:= NSInsetRect(rect, 1, 1);
  if scrollBar.IsHorizontal then begin
    rect.origin.y:= rect.origin.y + 3;
    rect.size.height:= rect.size.height - 5;
  end else begin
    rect.origin.x:= rect.origin.x + 3;
    rect.size.width:= rect.size.width - 5;
  end;

  alpha:= TCocoaScrollBarEffectAlpha(scrollBar.effect).currentAlpha;
  path:= NSBezierPath.bezierPathWithRoundedRect_xRadius_yRadius( rect, 4, 4 );
  color:= CIColor.alloc.initWithColor( NSColor.controlTextColor );
  NSColor.colorWithRed_green_blue_alpha(
    color.red,
    color.green,
    color.blue,
    alpha ).set_;
  path.fill;
end;

function TCocoaScrollStyleManagerLegacy.onDrawKnobSlot(scroller: NSScroller;
  var slotRect: NSRect): Boolean;
begin
  Result:= true;
end;

procedure TCocoaScrollStyleManagerLegacy.onKnobValueUpdated( scroller:NSScroller;
  var knobPosition:Double; var knobProportion:Double );
begin
end;

procedure TCocoaScrollStyleManagerLegacy.onMouseEntered(scroller: NSScroller
  );
var
  effect: TCocoaScrollBarEffectAlpha;
begin
  effect:= TCocoaScrollBarEffectAlpha(TCocoaScrollBar(scroller).effect);
  effect.fade( 0.05, 0.5 );
end;

procedure TCocoaScrollStyleManagerLegacy.onMouseExited(scroller: NSScroller
  );
var
  effect: TCocoaScrollBarEffectAlpha;
begin
  effect:= TCocoaScrollBarEffectAlpha(TCocoaScrollBar(scroller).effect);
  effect.fade( -0.05, 0.25 );
end;

{ TCocoaScrollStyleManagerOverlay }

procedure TCocoaScrollStyleManagerOverlay.onDrawKnob(scroller: NSScroller);
var
  scrollBar: TCocoaScrollBar Absolute scroller;
  effect: TCocoaScrollBarEffectOverlay;
  rect: NSRect;
  path: NSBezierPath;
  radius: CGFloat;
begin
  effect:= TCocoaScrollBarEffectOverlay(scrollBar.effect);

  radius:= 4;
  rect:= scrollBar.rectForPart(NSScrollerKnob);
  rect:= NSInsetRect(rect, 1, 1);
  if scrollBar.IsHorizontal then begin
    rect.origin.y:= rect.origin.y + 5 - effect.expandedSize;
    rect.size.height:= rect.size.height - 6 + effect.expandedSize;
  end else begin
    rect.origin.x:= rect.origin.x + 5 - effect.expandedSize;
    rect.size.width:= rect.size.width - 6 + effect.expandedSize;
  end;

  radius:= radius + effect.expandedSize/2;
  path:= NSBezierPath.bezierPathWithRoundedRect_xRadius_yRadius( rect, radius, radius );
  NSColor.controlTextColor.set_;
  path.fill;

  scroller.setAlphaValue( effect.currentAlpha );
  if effect.currentAlpha = 0 then begin
    scroller.setHidden( true );
    effect.expandedSize:= 0;
  end else begin
    if scroller.knobProportion < 1 then
      scroller.setHidden( false );
  end;
end;

function TCocoaScrollStyleManagerOverlay.onDrawKnobSlot(scroller: NSScroller;
  var slotRect: NSRect): Boolean;
var
  scrollBar: TCocoaScrollBar Absolute scroller;
  effect: TCocoaScrollBarEffectOverlay;
begin
  effect:= TCocoaScrollBarEffectOverlay(scrollBar.effect);
  Result:= effect.expandedSize>0;
end;

procedure TCocoaScrollStyleManagerOverlay.onKnobValueUpdated( scroller:NSScroller;
  var knobPosition:Double; var knobProportion:Double );
var
  scrollBar: TCocoaScrollBar Absolute scroller;
  effect: TCocoaScrollBarEffectOverlay;
  slotRect: NSRect;
  slotSize: CGFloat;
begin
  effect:= TCocoaScrollBarEffectOverlay(scrollBar.effect);

  if (effect.currentKnobPosition=knobPosition) and (effect.currentKnobProportion=knobProportion) then
    Exit;

  slotRect:= scroller.rectForPart(NSScrollerKnobSlot);
  if scrollBar.IsHorizontal then
    slotSize:= slotRect.size.width
  else
    slotSize:= slotRect.size.height;

  if knobProportion*slotSize <= 25 then begin
    if slotSize<=25 then
      knobProportion:= 0.99
    else
      knobProportion:= 25/slotSize;
  end;

  effect.currentKnobPosition:= knobPosition;
  effect.currentKnobProportion:= knobProportion;

  if knobProportion=1 then begin
    scroller.setAlphaValue(0);
    scroller.setHidden(True);
    Exit;
  end;

  if NOT effect.entered then begin
    effect.setDelayHidingTimer;
    effect.currentAlpha:= 0.5;
  end;

  // on old versions of macOS, alpha=0 is considered hidden.
  // that is, to be truly visible, not only Hidden=false, but Alpha must also be set.
  // otherwise it is considered hidden and setNeedsDisplay() does not take effect.
  scroller.setAlphaValue( effect.currentAlpha );
  scroller.setHidden( False );
  scroller.setNeedsDisplay_( true );
end;

procedure TCocoaScrollStyleManagerOverlay.onMouseEntered(scroller: NSScroller);
var
  scrollBar: TCocoaScrollBar Absolute scroller;
  effect: TCocoaScrollBarEffectOverlay;
begin
  effect:= TCocoaScrollBarEffectOverlay(scrollBar.effect);
  effect.entered:= True;
  effect.setExpandTimer;
  scroller.setNeedsDisplay_(true);
end;

procedure TCocoaScrollStyleManagerOverlay.onMouseExited(scroller: NSScroller);
var
  scrollBar: TCocoaScrollBar Absolute scroller;
  effect: TCocoaScrollBarEffectOverlay;
begin
  effect:= TCocoaScrollBarEffectOverlay(scrollBar.effect);
  effect.entered:= False;
  effect.setDelayHidingTimer;
end;

function TCocoaScrollStyleManagerOverlay.createScrollBarEffect(
  scroller: NSScroller): TCocoaScrollBarEffect;
var
  effect: TCocoaScrollBarEffectOverlay;
begin
  effect:= TCocoaScrollBarEffectOverlay.alloc.Init;
  effect.scroller:= scroller;
  effect.manager:= self;
  effect.currentKnobPosition:= -1;
  effect.currentKnobProportion:= -1;
  effect.currentAlpha:= 0.25;
  Result:= effect;
end;

procedure TCocoaScrollStyleManagerOverlay.activeScrollBar(
  scroller: NSScroller; active: Boolean);
begin
  if NOT Assigned(scroller) then
    Exit;

  if scroller.knobProportion=1 then begin
    scroller.setAlphaValue( 0 );
    scroller.setHidden( True );
    Exit;
  end;
end;

procedure TCocoaScrollStyleManagerOverlay.updateLayOut;
var
  doc: NSView;
  docFrame  : NSRect;
  hScroller: NSScroller;
  vScroller: NSScroller;
  hScrollerFrame : NSRect;
  vScrollerFrame : NSRect;
  hScrollerHeight : CGFLoat;
  vScrollerWidth : CGFLoat;
begin
  doc:= _scrollView.documentView;
  if NOT Assigned(doc) then
    Exit;

  docFrame := _scrollView.frame;
  docFrame.origin := NSZeroPoint;
  hScrollerFrame := docFrame;
  vScrollerFrame := docFrame;

  hScroller:= _scrollView.fhscroll;
  vScroller:= _scrollView.fvscroll;

  if Assigned(hScroller) then
  begin
    hScrollerHeight := NSScroller.scrollerWidthForControlSize_scrollerStyle(
            hScroller.controlSize, hScroller.preferredScrollerStyle);
    hScrollerFrame.size.height := hScrollerHeight;
  end;

  if Assigned(vScroller) then
  begin
    vScrollerWidth := NSScroller.scrollerWidthForControlSize_scrollerStyle(
            vScroller.controlSize, vScroller.preferredScrollerStyle);
    vScrollerFrame.size.width := vScrollerWidth;
  end;

  hScrollerFrame.size.width := docFrame.size.width;
  vScrollerFrame.size.height := docFrame.size.height;
  vScrollerFrame.origin.x := docFrame.size.width - vScrollerFrame.size.width;
  vScrollerFrame.origin.y := docFrame.origin.y;

  if Assigned(hScroller) then begin
    hScroller.setFrame(hScrollerFrame);
  end;

  if Assigned(vScroller) then begin
    vScroller.setFrame(vScrollerFrame);
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

end.

