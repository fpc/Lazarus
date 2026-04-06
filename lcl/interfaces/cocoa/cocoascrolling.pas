unit CocoaScrolling;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}
{$include cocoadefines.inc}

interface

uses
  Math, Classes, SysUtils, LclType,
  Controls, StdCtrls, Forms,
  MacOSAll, CocoaAll, CocoaCursor,
  CocoaPrivate, CocoaConst, CocoaConfig, CocoaUtils;

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

end.

