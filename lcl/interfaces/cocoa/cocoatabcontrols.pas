{ $Id: $}
{                  --------------------------------------------
                  cocoatabcontrols.pas  -  Cocoa internal classes
                  --------------------------------------------

 This unit contains the private classhierarchy for the Cocoa implemetations

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaTabControls;

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
  MacOSAll, CocoaAll, CocoaUtils, //CocoaGDIObjects,
  cocoa_extra, CocoaPrivate;

type

  ITabControlCallback = interface(ICommonCallback)
    procedure willSelectTabViewItem(aTabIndex: Integer);
    procedure didSelectTabViewItem(aTabIndex: Integer);
  end;

  { TCocoaTabPage }

  TCocoaTabPage = objcclass(NSTabViewItem)
  public
    callback: ICommonCallback;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclFrame: TRect; override;
    function lclClientFrame: TRect; override;
    procedure setLabel(label__: NSString); override;
  end;

  { TCocoaTabControl }

  TCocoaTabControl = objcclass(NSTabView, NSTabViewDelegateProtocol)
  private
    prevarr  : NSButton;
    nextarr  : NSButton;

    leftmost : Integer;         // index of the left-most tab shown

  public
    ignoreChange: Boolean;
    callback: ITabControlCallback;

    fulltabs : NSMutableArray;  // the full list of NSTabViewItems
    lclEnabled: Boolean;
    // cocoa
    class function alloc: id; override;
    procedure setFrame(aframe: NSRect); override;
    // lcl
    function lclIsEnabled: Boolean; override;
    procedure lclSetEnabled(AEnabled: Boolean); override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclClientFrame: TRect; override;
    function lclGetFrameToLayoutDelta: TRect; override;
    // NSTabViewDelegateProtocol
    function tabView_shouldSelectTabViewItem(tabView: NSTabView; tabViewItem: NSTabViewItem): Boolean; message 'tabView:shouldSelectTabViewItem:';
    procedure tabView_willSelectTabViewItem(tabView: NSTabView; tabViewItem: NSTabViewItem); message 'tabView:willSelectTabViewItem:';
    procedure tabView_didSelectTabViewItem(tabView: NSTabView; tabViewItem: NSTabViewItem); message 'tabView:didSelectTabViewItem:';
    procedure tabViewDidChangeNumberOfTabViewItems(TabView: NSTabView); message 'tabViewDidChangeNumberOfTabViewItems:';
    // mouse events
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
    // lcl
    procedure exttabInsertTabViewItem_atIndex(lTabPage: NSTabViewItem; AIndex: integer);
      message 'exttabInsertTabViewItem:atIndex:';
    procedure exttabRemoveTabViewItem(lTabPage: NSTabViewItem);
      message 'exttabRemoveTabViewItem:';
    function exttabIndexOfTabViewItem(lTabPage: NSTabViewItem): NSInteger;
      message 'exttabIndexOfTabViewItem:';
    procedure extTabPrevButtonClick(sender: id);
      message 'extTabPrevButtonClick:';
    procedure extTabNextButtonClick(sender: id);
      message 'extTabNextButtonClick:';
    procedure extselectTabViewItemAtIndex(index: NSInteger);
      message 'extselectTabViewItemAtIndex:';
  end;

  { TCocoaTabPageView }

  TCocoaTabPageView = objcclass(TCocoaCustomControl)
  public
    tabView: TCocoaTabControl;
    tabPage: TCocoaTabPage;
    procedure setFrame(arect: NSRect); override;
  end;

function IndexOfTab(ahost: TCocoaTabControl; atab: NSTabViewItem): Integer;

implementation

function AllocArrowButton(isPrev: Boolean): NSButton;
var
  btn : NSButton;
  r   : NSRect;
begin
  btn:=NSButton(NSButton.alloc).initWithFrame(NSZeroRect);
  btn.setBezelStyle(NSRegularSquareBezelStyle);
  btn.setButtonType(NSMomentaryLightButton);

  if isPrev then
    btn.setImage( NSImage.imageNamed( NSImageNameLeftFacingTriangleTemplate  ))
  else
    btn.setImage( NSImage.imageNamed( NSImageNameRightFacingTriangleTemplate ));

  {$ifdef BOOLFIX}
  btn.setBordered_(Ord(false));
  {$else}
  btn.setBordered(false);
  {$endif}
  btn.setTitle(NSString.string_);
  btn.sizeToFit();
  if not isPrev then btn.setAutoresizingMask(NSViewMinXMargin);
  Result:=btn;
end;

const
  arrow_hofs = 12;
  arrow_vofs = 20;

procedure PlaceButton(isPrev: Boolean; abtn: NSButton; dst: NSTabView);
var
  org: NSPoint;
begin
  if not assigned(abtn) then Exit;

  if isPrev then org:=NSMakePoint(arrow_hofs, arrow_vofs)
  else org:=NSMakePoint(dst.frame.size.width - abtn.frame.size.width - arrow_hofs , arrow_vofs);

  abtn.setFrameOrigin(org);
end;


procedure AllocPrevNext(aview: TCocoaTabControl);
begin
  aview.prevarr := AllocArrowButton(true);
  aview.addSubview(aview.prevarr);
  aview.nextarr := AllocArrowButton(false);
  aview.addSubview(aview.nextarr);
  aview.prevarr.setTarget(aview);
  aview.prevarr.setAction( ObjCSelector('extTabPrevButtonClick:'));
  aview.nextarr.setTarget(aview);
  aview.nextarr.setAction( ObjCSelector('extTabNextButtonClick:'));


  PlaceButton(true, aview.prevarr, aview);
  PlaceButton(false, aview.nextarr, aview);
end;

procedure RemoveAllTabs(aview: TCocoaTabControl);
var
  arr: NSArray;
  i : integer;
begin
  arr := aview.tabViewItems;
  for i := Integer(arr.count) - 1  downto 0 do
    aview.removeTabViewItem( arr.objectAtIndex(i) );
end;

procedure AttachAllTabs(aview: TCocoaTabControl);
var
  i : integer;
begin
  RemoveAllTabs(aview);
  for i := 0 to aview.fulltabs.count - 1 do
    aview.insertTabViewItem_atIndex( aview.fulltabs.objectAtIndex(i), i);
end;

procedure ReviseTabs(aview: TCocoaTabControl; out ShowPrev,ShowNext: Boolean);
var
  minw: double;
  i: integer;
  arr: NSArray;
  vi : NSTabViewItem;
  sz : NSSize;
  x,y: integer;
  lw  : double;
  tbext : double;
  lwid : array of double;
  xd  : double;
  j : integer;
  ofs : integer;
  frw: double;
  sel: NSTabViewItem;
  v : NSView;
begin
  ShowPrev := false;
  ShowNext := false;

  sel := aview.selectedTabViewItem;
  try
    if (aview.fulltabs.count>aview.tabViewItems.count) then
      AttachAllTabs(aview);

    minw := aview.minimumSize.width;
    if (minw<aview.frame.size.width) then Exit;

    arr := aview.tabViewItems;

    lw := 0;
    SetLength(lwid, arr.count);
    for i := 0 to arr.count - 1 do
    begin
      vi := NSTabViewItem( arr.objectAtIndex(i) );
      sz := vi.sizeOfLabel(false);
      lw := lw + sz.width;
      lwid[i] := sz.width;
    end;

    tbext := (minw - lw) / arr.count;
    for i:=0 to length(lwid)-1 do
      lwid[i] := lwid[i] + tbext;

    ofs := aview.leftmost;
    if ofs>=length(lwid) then ofs:=length(lwid)-1;
    if (ofs < 0) then Exit;
    ShowPrev := ofs > 0;


    xd := lwid[ofs];
    frw := aview.frame.size.width;
    frw := frw - ((arrow_hofs + aview.nextarr.frame.size.width) * 2);
    if frw<0 then frw := 0;

    //aview.prevarr.isHidden((aview.leftmost=0));

    for i := ofs+1 to length(lwid)-1 do begin
      if xd + lwid[i] > frw then begin
        //aview.nextarr.isHidden((aview.leftmost=0));
        for j:=length(lwid)-1 downto i do
          aview.removeTabViewItem( arr.objectAtIndex(j));
        ShowNext := true;
        Break;
      end;
      xd := xd + lwid[i];
    end;

    if not ShowNext then begin
      // shown all right-side tabs, there might be a tab, that can be shown on the left
      while (ofs>0) and (xd+lwid[ofs]<frw) do begin
        xd := xd + lwid[ofs];
        dec(ofs);
      end;
      aview.leftmost:=ofs;
    end;

    for i := ofs - 1 downto 0 do
      aview.removeTabViewItem( arr.objectAtIndex(i));

    // todo: automatic resizing still has its effect on the tabs.
    //       the content page fails to pick up the proper size and place
    //       and it seems that the content of the tab is shifted.
    //       Needs investiage and a fix.

  finally
    if (aview.indexOfTabViewItem(sel)<>NSNotFound) then
      aview.selectTabViewItem(sel);
  end;
end;

procedure UpdateTabAndArrowVisibility(aview: TCocoaTabControl);
var
  showNext : Boolean;
  showPrev : Boolean;
begin
  ReviseTabs(aview, showPrev, showNExt);
  if Assigned(aview.prevarr) then
    {$ifdef BOOLFIX}
    aview.prevarr.setHidden_(Ord(not showPrev));
    {$else}
    aview.prevarr.setHidden(not showPrev);
    {$endif}
  if Assigned(aview.nextarr) then
    {$ifdef BOOLFIX}
    aview.nextarr.setHidden_(Ord(not showNext));
    {$else}
    aview.nextarr.setHidden(not showNext);
    {$endif}
end;

function IndexOfTab(ahost: TCocoaTabControl; atab: NSTabViewItem): Integer;
var
  idx : NSUInteger;
begin
  idx := ahost.fulltabs.indexOfObject(atab);
  if idx=NSUIntegerMax then Result:=-1
  else begin
    if idx>MaxInt then Result:=-1
    else Result:=Integer(idx);
  end;
end;

{ TCocoaTabPageView }

procedure TCocoaTabPageView.setFrame(arect: NSRect);
begin
  // It's possible for a tab page view to go in negative height.
  // However, automatic resizing flags (for whatever reason) prevent
  // TCocoaTabPageView to go into negative height (remaining at 0 pixels)
  // The code below makes sure that resizing is actually happening
  if Assigned(superView) and (superView.frame.size.height < arect.size.height) then
    arect.size.height := superView.frame.size.height;

  inherited setFrame(arect);
end;

{ TCocoaTabPage }

function TCocoaTabPage.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaTabPage.lclClearCallback;
begin
  callback := nil;
end;

function TCocoaTabPage.lclFrame: TRect;
var
  svh: CGFloat;
  lParent: NSTabView;
begin
  lParent := tabView;
  if lParent <> nil then
  begin
    svh := lParent.contentRect.size.height;
    NSToLCLRect(lParent.contentRect, svh, Result);
  end
  else
  begin
    svh := tabView.frame.size.height;
    NSToLCLRect(tabView.contentRect, svh, Result);
  end;
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaTabPage.lclFrame] '+dbgs(Result)+' '+NSStringToString(Self.label_));
  {$ENDIF}
end;

function TCocoaTabPage.lclClientFrame: TRect;
begin
  Result := lclFrame();
end;

procedure TCocoaTabPage.setLabel(label__: NSString);
begin
  inherited setLabel(label__);
  //todo: revise the parent labels
end;

{ TCocoaTabControl }

class function TCocoaTabControl.alloc: id;
begin
  Result := inherited alloc;
  TCocoaTabControl(Result).fulltabs := NSMutableArray(NSMutableArray.alloc).init;
end;

procedure TCocoaTabControl.setFrame(aframe: NSRect);
begin
  inherited setFrame(aframe);

  if not Assigned(nextarr) then
    AllocPrevNext( self );

  UpdateTabAndArrowVisibility(self);
end;

procedure TCocoaTabControl.extselectTabViewItemAtIndex(index: NSInteger);
var
  idx : integer;
  itm : NSTabViewItem;
  i   : NSUInteger;
begin
  if (index<0) or (index>=fulltabs.count) then Exit;
  itm := NSTabViewItem(fulltabs.objectAtIndex(index));

  i := tabViewItems.indexOfObject(itm);
  if i <> NSNotFound then
  begin
    inherited selectTabViewItemAtIndex(NSInteger(i));
  end
  else begin
    leftmost := index;
    UpdateTabAndArrowVisibility(self);
    i := tabViewItems.indexOfObject(itm);
    inherited selectTabViewItemAtIndex(NSInteger(i));
  end;
end;

function TCocoaTabControl.lclIsEnabled: Boolean;
begin
  Result:=lclEnabled and ((Assigned(superview) and superview.lclIsEnabled) or not Assigned(superview));
end;

procedure TCocoaTabControl.lclSetEnabled(AEnabled: Boolean);
begin
  lclEnabled := AEnabled;
end;

function TCocoaTabControl.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaTabControl.lclClearCallback;
begin
  callback := nil;
end;

function TCocoaTabControl.lclClientFrame: TRect;
var
  r : TRect;
  f : NSRect;
begin
  case tabViewType of
    NSNoTabsNoBorder:
    begin
      f := frame;
      f.origin.x := 0;
      f.origin.y := 0;
      Result := NSRectToRect( f );
    end;
  else
    if isFlipped then
      Result:=NSRectToRect( contentRect )
    else
      NSToLCLRect( contentRect, frame.size.height, Result );
  end;

  //if tabs are hidden, frame layout should not be taken into account
  //r:=lclGetFrameToLayoutDelta;
  //Types.OffsetRect(Result, -r.Left, -r.Top);
end;

function TCocoaTabControl.lclGetFrameToLayoutDelta: TRect;
begin
  case tabViewType of
    NSNoTabsNoBorder: begin
      Result.Left := 0;
      Result.Top := 0;
      Result.Bottom := 0;
      Result.Right := 0;
    end;
  else
    Result.Bottom := -10;
    Result.Top := 6;
    Result.Left := 7;
    Result.Right := -7;
  end;
end;

function TCocoaTabControl.tabView_shouldSelectTabViewItem(tabView: NSTabView;
  tabViewItem: NSTabViewItem): Boolean;
begin
  Result := True;
end;

procedure TCocoaTabControl.tabView_willSelectTabViewItem(tabView: NSTabView;
  tabViewItem: NSTabViewItem);
begin
  if ignoreChange then Exit;
  if Assigned(callback) then
  begin
    callback.willSelectTabViewItem( IndexOfTab( self, tabViewItem) );
  end;
end;

procedure TCocoaTabControl.tabView_didSelectTabViewItem(tabView: NSTabView;
  tabViewItem: NSTabViewItem);
begin
  //it's called together with "willSelect"

  if Assigned(callback) then
  begin
    // Expected LCL Focus changing goes as following:
    //   First page becomes visible
    //   Then focus is switching to the control of the page
    // In Cocoa world, first "willSelect" runs,
    //  then "firstResponder" changes to Window
    //  then the views are reorded and the new View becomes a part
    //  of views chain (and attaches to the window
    //  the view is made "firstResponder"
    //  and finally "didSelect" is fired
    callback.didSelectTabViewItem( IndexOfTab( self, tabViewItem) );
  end;

  // The recent clean up, drove the workaround below unnecessary
  // (at least the problem is not observed)
  // The issue, is that the controls are being placed incorrectly, below
  // the actual height of the control. Refactoring, removed direct LCL bindings.
  // And it seemed to helped with returning invalid control bounds?!

  // Update the coordinates of all children of this tab
  // Fixes bug 31914: TPageControl problems with Cocoa
  {lTabView := tabViewItem.view.subViews.objectAtIndex(0);
  for i := 0 to lTabView.subViews.count-1 do
  begin
    lCurSubview := lTabView.subViews.objectAtIndex(i);
    lCurCallback := lCurSubview.lclGetCallback();
    if Assigned(lCurCallback) then
    begin
      lLCLControl := TWinControl(lCurCallback.GetTarget());
      lBounds := Classes.Bounds(lLCLControl.Left, lLCLControl.Top, lLCLControl.Width, lLCLControl.Height);
      lCurSubview.lclSetFrame(lBounds);
    end;
  end;}
end;

procedure TCocoaTabControl.tabViewDidChangeNumberOfTabViewItems(
  TabView: NSTabView);
begin

end;

procedure TCocoaTabControl.mouseDown(event: NSEvent);
var
  res : Boolean;
begin
  res := callback.MouseUpDownEvent(event, false, true);
  if not Assigned(callback) or not res then
  begin
    inherited mouseDown(event);

    if Assigned(callback) then
    begin
      callback.MouseUpDownEvent(event, True);
    end;
  end
end;

procedure TCocoaTabControl.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) then callback.MouseUpDownEvent(event);
  inherited mouseUp(event);
end;

procedure TCocoaTabControl.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaTabControl.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaTabControl.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaTabControl.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaTabControl.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaTabControl.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaTabControl.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaTabControl.mouseMoved(event: NSEvent);
begin
  if Assigned(callback) then callback.MouseMove(event);
  inherited mouseMoved(event);
end;

procedure TCocoaTabControl.exttabInsertTabViewItem_atIndex(
  lTabPage: NSTabViewItem; AIndex: integer);
begin
  if AIndex>fulltabs.count then AIndex:=fulltabs.count;
  fulltabs.insertObject_atIndex(lTabPage, AIndex);

  UpdateTabAndArrowVisibility(self);
end;

procedure TCocoaTabControl.exttabRemoveTabViewItem(lTabPage: NSTabViewItem);
var
  idx : NSInteger;
begin
  idx := indexOfTabViewItem(lTabPage);
  if (idx>=0) and (idx<>NSNotFound) then
    removeTabViewItem(lTabPage);

  fulltabs.removeObject(lTabPage);

  UpdateTabAndArrowVisibility(self);
end;

function TCocoaTabControl.exttabIndexOfTabViewItem(lTabPage: NSTabViewItem
  ): NSInteger;
begin
  Result := fulltabs.indexOfObject(lTabPage);
end;

procedure TCocoaTabControl.extTabPrevButtonClick(sender: id);
begin
  if leftmost = 0 then Exit;

  leftmost := leftmost - 1;
  UpdateTabAndArrowVisibility(self);
end;

procedure TCocoaTabControl.extTabNextButtonClick(sender: id);
begin
  if leftmost = fulltabs.count - 1 then Exit;

  leftmost := leftmost + 1;
  UpdateTabAndArrowVisibility(self);
end;


end.

