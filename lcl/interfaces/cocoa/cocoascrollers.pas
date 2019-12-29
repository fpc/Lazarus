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
  Types, Classes, SysUtils,
  CGGeometry,
  // Libs
  MacOSAll, CocoaAll, CocoaUtils, CocoaGDIObjects,
  cocoa_extra, CocoaPrivate;

type

  { TCocoaScrollView }

  TCocoaScrollView = objcclass(NSScrollView)
  public
    callback: ICommonCallback;
    isCustomRange: Boolean;

    docrect    : NSRect;    // have to remember old
    holdscroll : Integer; // do not send scroll messages
    function initWithFrame(ns: NSRect): id; override;
    procedure dealloc; override;
    procedure setFrame(aframe: NSRect); override;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclClientFrame: TRect; override;
    function lclContentView: NSView; override;
    procedure setDocumentView(aView: NSView); override;
    procedure scrollContentViewBoundsChanged(notify: NSNotification); message 'scrollContentViewBoundsChanged:';
    procedure resetScrollRect; message 'resetScrollRect';

    procedure lclUpdate; override;
    procedure lclInvalidateRect(const r: TRect); override;
    procedure lclInvalidate; override;
  end;

  { TCocoaManualScrollView }

  TCocoaManualScrollView = objcclass(NSView)
  private
    fdocumentView: NSView;
    fhscroll : NSScroller;
    fvscroll : NSScroller;
  public
    isHosted: Boolean;
    callback: ICommonCallback;
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

    procedure actionScrolling(sender: NSObject); message 'actionScrolling:';
    function IsHorizontal: Boolean; message 'IsHorizontal';
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
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
    procedure setDocumentView(aview: NSView); override;
    function lclContentView: NSView; override;
    function lclClientFrame: TRect; override;
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
  adj : Integer;
  sz  : Integer;
  dlt : double;
  v   : double;
begin
  Result := isIncDecPagePart(prt);
  if not Result then Exit;
  sz := sc.maxInt - sc.minInt - sc.pageInt;
  if sz = 0 then Exit; // do nothing!

  if sc.pageInt = 0 then dlt := 1 / sz
  else dlt := sc.pageInt / sz;
  if prt = NSScrollerDecrementPage then dlt := -dlt;

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
    sz := (sc.maxInt - sc.minInt - sc.pageInt);;
    newPos := Round(sc.minInt + sz * ps);
    sc.lclSetPos(NewPos);
  end;
end;

{ TCocoaManualScrollHost }

procedure TCocoaManualScrollHost.setDocumentView(aview: NSView);
begin
  inherited setDocumentView(aview);
  if Assigned(aview) and (aview.isKindOfClass(TCocoaManualScrollView)) then
    TCocoaManualScrollView(aview).isHosted := true;
end;

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

{ TCocoaManualScrollView }

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

procedure updateDocSize(parent: NSView; doc: NSView; hrz, vrt: NSScroller);
var
  f  : NSRect;
  hr : NSRect;
  vr : NSRect;
  hw : CGFLoat;
  vw : CGFLoat;
begin
  if not Assigned(parent) or not Assigned(doc) then Exit;

  f := parent.frame;
  f.origin.x := 0;
  f.origin.y := 0;
  hr := f;
  vr := f;
  hw := NSScroller.scrollerWidth;
  vw := NSScroller.scrollerWidth;
  vr.size.width:=vw;
  vr.origin.x:=f.size.width-vr.size.width;
  hr.size.height:=hw;

  if Assigned(hrz) and (not hrz.isHidden) then
  begin
    f.size.height := f.size.height - hw;
    f.origin.y := hw;

    vr.origin.y := hw;
    vr.size.height := vr.size.height - hw;
    if Assigned(vrt) and (not vrt.isHidden) then
      hr.size.width:=hr.size.width-vw;

    hrz.setFrame(hr);
  end;

  if Assigned(vrt) and (not vrt.isHidden) then
  begin
    f.size.width := f.size.width-vw;
    vrt.setFrame(vr);
  end;


  if not NSEqualRects(doc.frame, f) then
  begin
    doc.setFrame(f);
    {$ifdef BOOLFIX}
    doc.setNeedsDisplay__(Ord(true));
    {$else}
    doc.setNeedsDisplay_(true);
    {$endif}
  end;
end;

procedure TCocoaManualScrollView.setHasVerticalScroller(doshow: Boolean);
var
  ch : Boolean;
begin
  ch := false;
  if doshow then
  begin
    if not Assigned(fvscroll) then
    begin
      fvscroll := allocVerticalScroller(true);
      ch := true;
    end;

    if fvscroll.isHidden then
    begin
      {$ifdef BOOLFIX}
      fvscroll.setHidden_(Ord(false));
      {$else}
      fvscroll.setHidden(false);
      {$endif}
      ch := true;
    end;
  end
  else if Assigned(fvscroll) and not fvscroll.isHidden then
  begin
    {$ifdef BOOLFIX}
    fvscroll.setHidden_(Ord(true));
    {$else}
    fvscroll.setHidden(true);
    {$endif}
    ch := true;
  end;
  if ch then
    updateDocSize(self, fdocumentView, fhscroll, fvscroll);
end;

procedure TCocoaManualScrollView.setHasHorizontalScroller(doshow: Boolean);
var
  r : NSRect;
  f : NSRect;
  ch : Boolean;
begin
  f:=frame;
  ch:=false;
  if doshow then
  begin
    if not Assigned(fhscroll) then
    begin
      fhscroll := allocHorizontalScroller(true);
      ch := true;
    end;
    if fhscroll.isHidden then
    begin
      {$ifdef BOOLFIX}
      fhscroll.setHidden_(Ord(false));
      {$else}
      fhscroll.setHidden(false);
      {$endif}
      ch := true;
    end;
  end
  else if Assigned(fhscroll) and (not fhscroll.isHidden) then
  begin
    {$ifdef BOOLFIX}
    fhscroll.setHidden_(Ord(true));
    {$else}
    fhscroll.setHidden(true);
    {$endif}
    ch := true;
  end;

  if ch then
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
    w := NSScroller.scrollerWidth;
    r := NSMakeRect(0, 0, f.size.width, NSScroller.scrollerWidth);
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
    w := NSScroller.scrollerWidth;
    r := NSMakeRect(f.size.width-w, 0, w, f.size.height);
    allocScroller( self, fvscroll, r, avisible);
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

procedure TCocoaManualScrollView.mouseDown(event: NSEvent);
begin
  if isMouseEventInScrollBar(self, event) or not Assigned(callback) or not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseDown(event);
  end;
end;

procedure TCocoaManualScrollView.mouseUp(event: NSEvent);
begin
  if isMouseEventInScrollBar(self, event) or not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaManualScrollView.rightMouseDown(event: NSEvent);
begin
  if isMouseEventInScrollBar(self, event) or not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaManualScrollView.rightMouseUp(event: NSEvent);
begin
  if isMouseEventInScrollBar(self, event) or not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaManualScrollView.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaManualScrollView.otherMouseDown(event: NSEvent);
begin
  if isMouseEventInScrollBar(self, event) or not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaManualScrollView.otherMouseUp(event: NSEvent);
begin
  if isMouseEventInScrollBar(self, event) or not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaManualScrollView.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaManualScrollView.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaManualScrollView.mouseMoved(event: NSEvent);
begin
  if isMouseEventInScrollBar(self, event) then
  begin
    inherited mouseMoved(event)
  end
  else if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaManualScrollView.scrollWheel(event: NSEvent);
begin
  // when hosted, the processing of scrollWheel is duplciated
  if not isHosted then
  begin
    if isMouseEventInScrollBar(self, event) or not Assigned(callback) or not callback.scrollWheel(event) then
      inherited scrollWheel(event);
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
  resetScrollRect;
end;

procedure TCocoaScrollView.scrollContentViewBoundsChanged(notify: NSNotification
  );
var
  nw    : NSRect;
  dx,dy : CGFloat;
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
    if (dx<>0) and assigned(callback) then
      callback.scroll(false, round(nw.origin.x));

    if (dy<>0) and assigned(callback) then
      callback.scroll(true, round(self.documentView.frame.size.height - self.documentVisibleRect.origin.y - self.documentVisibleRect.size.height));
  finally
    dec(holdscroll);
  end;
end;

procedure TCocoaScrollView.resetScrollRect;
begin
  docrect:=documentVisibleRect;
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

function TCocoaScrollView.initWithFrame(ns: NSRect): id;
var
  sc : TCocoaScrollView;
begin
  Result:=inherited initWithFrame(ns);
  sc:=TCocoaScrollView(Result);

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
  flg  : NSUInteger;
  iflg : NSUInteger;
  docsz : NSSize;
  scrlsz : NSSize;

  viewRect : NSRect;
  dRect  : NSRect;
  hh       : Single;
  r        : NSRect;
const
  NSBothSizable = NSViewWidthSizable or NSViewHeightSizable;
begin
  if not isCustomRange then begin
    inherited setFrame(aframe);
    Exit;
  end;

  viewRect := documentVisibleRect;
  dRect := NSView(documentView).frame;

  inherited setFrame(aframe);

  flg := documentView.autoresizingMask;
  if (flg and NSBothSizable) = NSBothSizable then Exit; // no need to checl

  iflg := flg;
  docsz := NSView(documentView).frame.size;
  scrlsz := frame.size;

  if (docsz.width<scrlsz.width) and (flg and NSViewWidthSizable = 0) then
    flg := flg or NSViewWidthSizable;

  if (docsz.height<scrlsz.height) and (flg and NSViewHeightSizable = 0) then
  begin
    // force automatic resize for isCustomRange
    flg := flg or NSViewHeightSizable;
    documentView.setFrameOrigin( NSMakePoint(0, aframe.size.height));
    documentView.setFrameSize(aframe.size);
    //documentView.setAutoresizingMask(flg);
  end;

  if iflg <> flg then
    documentView.setAutoresizingMask(flg);

  // the reason for this code here, is the need to re-allign the position
  // if the control Size adjusted after ScrollInfo got changed.
  if (documentVisibleRect.size.height < viewRect.size.height)
    and (flg and NSViewHeightSizable = 0) then
  begin
    hh := dRect.size.height - viewRect.origin.y - viewRect.size.height;

    r := documentVisibleRect;
    r.origin.y := NSView(documentView).frame.size.height - r.size.height - hh;

    NSView(documentView).scrollRectToVisible(r);
  end;
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

procedure TCocoaScrollView.resetCursorRects;
begin
  if not Assigned(callback) or not callback.resetCursorRects then
    inherited resetCursorRects;
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
    callback.scroll( not IsHorizontal(), lclPos);
end;

function TCocoaScrollBar.IsHorizontal: Boolean;
begin
  Result := frame.size.width > frame.size.height;
end;

function TCocoaScrollBar.lclPos: Integer;
begin
  Result:=round( floatValue * (maxint-minInt-pageInt)) + minInt;
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
  if suppressLCLMouse then
    inherited scrollWheel(event)
  else
  if not Assigned(callback) or not callback.scrollWheel(event) then
    inherited scrollWheel(event);
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

procedure TCocoaScrollBar.resetCursorRects;
begin
  if not Assigned(callback) or not callback.resetCursorRects then
    inherited resetCursorRects;
end;



end.

