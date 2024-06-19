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

    function initWithFrame(ns: NSRect): id; override;
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
    procedure scrollContentViewBoundsChanged(notify: NSNotification); message 'scrollContentViewBoundsChanged:';
    procedure resetScrollData; message 'resetScrollData';

    procedure lclUpdate; override;
    procedure lclInvalidateRect(const r: TRect); override;
    procedure lclInvalidate; override;

    procedure fillScrollInfo(barFlag: Integer; var scrollInfo: TScrollInfo); message 'fillScrollInfo:barFlag:';
    procedure applyScrollInfo(barFlag: Integer; const scrollInfo: TScrollInfo); message 'applyScrollInfo:barFlag:';
  end;

  { TCocoaManualScrollView }

  TCocoaManualScrollView = objcclass(NSView)
  private
    fdocumentView: NSView;
    fhscroll : NSScroller;
    fvscroll : NSScroller;
  public
    callback: ICommonCallback;
    procedure dealloc; override;
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

    function allocHorizontalScroller(avisible: Boolean): NSScroller; message 'allocHorizontalScroller:';
    function allocVerticalScroller(avisible: Boolean): NSScroller; message 'allocVerticalScroller:';
    // mouse
    function acceptsFirstMouse(event: NSEvent): LCLObjCBoolean; override;
  end;

  { TCocoaScrollBar }

  TCocoaScrollBar = objcclass(NSScroller)
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

  { TCocoaManualScrollHost }

  TCocoaManualScrollHost = objcclass(TCocoaScrollView)
    function lclContentView: NSView; override;
    function lclClientFrame: TRect; override;
    procedure scrollWheel(theEvent: NSEvent); override;
    procedure setFrame(newValue: NSRect); override;
  end;

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

procedure allocScroller(parent: TCocoaManualScrollView; var sc: NSScroller; dst: NSRect; aVisible: Boolean);
begin
  sc:=TCocoaScrollBar(TCocoaScrollBar.alloc).initWithFrame(dst);
  parent.addSubview(sc);
  {$ifdef BOOLFIX}
  sc.setEnabled_(Ord(true));
  sc.setHidden_(Ord(not AVisible));
  {$else}
  sc.setEnabled(true);
  sc.setHidden(not AVisible);
  {$endif}
  TCocoaScrollBar(sc).preventBlock := true;
  //Suppress scrollers notifications.
  TCocoaScrollBar(sc).callback := parent.callback;
  TCocoaScrollBar(sc).suppressLCLMouse := true;
  sc.setTarget(sc);
  sc.setAction(objcselector('actionScrolling:'));
end;

procedure updateDocSize(parent: NSView; doc: NSView; hScroller, vScroller: NSScroller);
var
  docFrame  : NSRect;
  hScrollerFrame : NSRect;
  vScrollerFrame : NSRect;
  hScrollerHeight : CGFLoat;
  vScrollerWidth : CGFLoat;
begin
  if not Assigned(parent) or not Assigned(doc) then
    Exit;

  docFrame := parent.frame;
  docFrame.origin := NSZeroPoint;
  hScrollerFrame := docFrame;
  vScrollerFrame := docFrame;

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
  updateDocSize(sc, sc.documentView, sc.horizontalScroller, sc.verticalScroller);
end;

{ TCocoaManualScrollView }

procedure TCocoaManualScrollView.dealloc;
begin
  if Assigned(fhscroll) then fhscroll.release;
  if Assigned(fvscroll) then fvscroll.release;
  inherited dealloc;
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
  if doshow then
  begin
    if not Assigned(fvscroll) then
      fvscroll := allocVerticalScroller(true);

    if fvscroll.isHidden then
    begin
      {$ifdef BOOLFIX}
      fvscroll.setHidden_(Ord(false));
      {$else}
      fvscroll.setHidden(false);
      {$endif}
    end;
  end
  else if Assigned(fvscroll) and not fvscroll.isHidden then
  begin
    {$ifdef BOOLFIX}
    fvscroll.setHidden_(Ord(true));
    {$else}
    fvscroll.setHidden(true);
    {$endif}
  end;

  updateDocSize(self, fdocumentView, fhscroll, fvscroll);
end;

procedure TCocoaManualScrollView.setHasHorizontalScroller(doshow: Boolean);
begin
  if doshow then
  begin
    if not Assigned(fhscroll) then
      fhscroll := allocHorizontalScroller(true);

    if fhscroll.isHidden then
    begin
      {$ifdef BOOLFIX}
      fhscroll.setHidden_(Ord(false));
      {$else}
      fhscroll.setHidden(false);
      {$endif}
    end;
  end
  else if Assigned(fhscroll) and (not fhscroll.isHidden) then
  begin
    {$ifdef BOOLFIX}
    fhscroll.setHidden_(Ord(true));
    {$else}
    fhscroll.setHidden(true);
    {$endif}
  end;

  updateDocSize(self, fdocumentView, fhscroll, fvscroll);
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

function TCocoaManualScrollView.allocHorizontalScroller(avisible: Boolean): NSScroller;
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
    allocScroller( self, fhscroll, r, avisible);
    fhscroll.setAutoresizingMask(NSViewWidthSizable);
    Result := fhscroll;
  end;
end;

function TCocoaManualScrollView.allocVerticalScroller(avisible: Boolean): NSScroller;
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
    allocScroller( self, fvscroll, r, avisible);
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

procedure TCocoaScrollView.scrollContentViewBoundsChanged(notify: NSNotification
  );
var
  nw    : NSRect;
  dx,dy : CGFloat;
  scrollY: CGFloat;
begin
  if not assigned(documentView) then Exit;
  nw:=documentVisibleRect;

  dx:=nw.origin.x-docrect.origin.x;
  dy:=docrect.origin.y-nw.origin.y; // cocoa flipped coordinates

  docrect:=nw;
  if (dx=0) and (dy=0) then Exit;

  if holdscroll>0 then Exit;
  inc(holdscroll);
  try
    if self.documentView.isFlipped then
      scrollY:= nw.origin.y
    else
      scrollY:= self.documentView.frame.size.height - nw.origin.y - nw.size.height;

    // update scrollers (this is required, if scrollWheel was called)
    // so processing LM_xSCROLL will not cause any actually scrolling,
    // as the current position will match!
    self.reflectScrolledClipView(contentView);
    if (dx<>0) and assigned(callback) then
      callback.scroll(false, round(nw.origin.x));

    if (dy<>0) and assigned(callback) then
      callback.scroll(true, round(scrollY));
  finally
    dec(holdscroll);
  end;
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

function TCocoaScrollView.initWithFrame(ns: NSRect): id;
var
  sc : TCocoaScrollView;
begin
  Result:=inherited initWithFrame(ns);
  sc:=TCocoaScrollView(Result);

  if NOT sc.isCustomRange then
    Exit;

  //sc.contentView.setPostsBoundsChangedNotifications(true);
  NSNotificationCenter.defaultCenter
    .addObserver_selector_name_object(sc, ObjCSelector('scrollContentViewBoundsChanged:')
      ,NSViewBoundsDidChangeNotification
      ,sc.contentView);
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

  lclControl:= TScrollingWinControl(lclGetTarget);


  inherited setFrame(aframe);


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

end.

