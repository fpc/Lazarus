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
    function shouldSelectTabViewItem(aTabIndex: Integer): Boolean;
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

  public
    currentIndex : Integer;     // index of the current tab shown
    callback: ITabControlCallback;

    fulltabs : NSMutableArray;  // the full list of NSTabViewItems
    lclEnabled: Boolean;
    // cocoa
    class function alloc: id; override;
    procedure dealloc; override;
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

// Hack: The function attempts to determine the tabs view
// if the view is found it would return its frame rect in LCL coordinates
// if the view cannot be determinted, the function returns false
// This is implemented as ObjC method, because "prevarr" and "nextarr"
// are private methods.
// It's unknown, if it's safe to cache the result, so the search is performed
// everytime
function GetTabsRect(tabs: TCocoaTabControl; var r: TRect): Boolean;

procedure UpdateTabAndArrowVisibility(aview: TCocoaTabControl);

implementation

function GetTabsRect(tabs: TCocoaTabControl; var r: TRect): Boolean;
var
  i  : integer;
  sv : NSView;
  f  : NSRect;
begin
  Result:=Assigned(tabs);
  if not Result then Exit;

  for i:=0 to Integer(tabs.subviews.count)-1 do
  begin
    sv:=NSView(tabs.subviews.objectAtIndex(i));
    if not Assigned(sv)
       or (sv = tabs.nextarr)
       or (sv = tabs.prevarr)
       or (sv.isKindOfClass(TCocoaTabPageView))
    then Continue;

    f := sv.frame;
    if tabs.isFlipped then
      r := NSRectToRect( f )
    else
      NSToLCLRect( f, tabs.frame.size.height, r );
    Result := true;
    Exit;
  end;
  Result:=false;
end;

function AllocArrowButton(isPrev: Boolean): NSButton;
var
  btn : NSButton;
begin
  btn:=NSButton(NSButton.alloc).initWithFrame(NSZeroRect);
  btn.setBezelStyle(NSRegularSquareBezelStyle);
  btn.setButtonType(NSMomentaryLightButton);

  if isPrev then
    btn.setTitle( StrToNSString('◀') )
  else
     btn.setTitle( StrToNSString('▶') );

  {$ifdef BOOLFIX}
  btn.setBordered_(Ord(false));
  {$else}
  btn.setBordered(false);
  {$endif}
  btn.sizeToFit();
  if not isPrev then btn.setAutoresizingMask(NSViewMinXMargin);
  Result:=btn;
end;

const
  arrow_hofs = 12;
  arrow_vofs = 10;

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

// only missing ViewItems inserted, RemoveAllTabs() is no longer needed,
// and tabView_didSelectTabViewItem is not triggered anymore
procedure AttachAllTabs(aview: TCocoaTabControl);
var
  i : integer;
  itm: NSTabViewItem;
begin
  for i := 0 to aview.fulltabs.count - 1 do begin
    itm := NSTabViewItem( aview.fulltabs.objectAtIndex(i) );
    if aview.indexOfTabViewItem(itm) = NSNotFound then
      aview.insertTabViewItem_atIndex( itm, i);
  end;
end;

// by fine-tuning the algorithm, guarantee that the `selectedTabViewItem`
// remains unchanged when TabControl is not wide enough,
// so as to avoid a lot of useless `tabView_didSelectTabViewItem` events are triggered
// (Resize TabControl, Add Tab, Remove Tab、Prev Tab、Next Tab)
// 1. change `leftmost` to `currentIndex`, and change the meaning to the index of
//    the currently selected Tab (mapping selectedTabViewItem)
// 2. currentIndex remains unchanged in ReviseTabs()
// 3. selectedTabViewItem is no longer removed and then added,
//    but always remains in tabViewItems,
//    thus tabView_didSelectTabViewItem is not triggered anymore
// 4. taking currentIndex as the intermediate value,
//    try to keep the tabs on the right side, and then the left side
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
  v : NSView;
begin
  ShowPrev := false;
  ShowNext := false;

  if aview.fulltabs.count=0 then exit;

  // AttachAllTabs() has been modified to not remove the selectedTabViewItem first,
  // and no longer trigger tabView_didSelectTabViewItem
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

  frw := aview.frame.size.width;
  frw := frw - ((arrow_hofs + aview.nextarr.frame.size.width) * 2);
  if frw<0 then frw := 0;

  ofs := aview.currentIndex;
  if ofs>=length(lwid) then ofs:=length(lwid)-1;
  if (ofs < 0) then ofs:=0;

  // 1. keep the current tab first
  //    selectedTabViewItem is guaranteed to remain in the updated tabViewItems
  xd := lwid[ofs];

  // 2. try to keep the tabs on the right side until it's not wide enough
  //    and ShowNext if necessary
  for i := ofs+1 to length(lwid)-1 do begin
    if xd + lwid[i] > frw then begin
      for j:=length(lwid)-1 downto i do
        aview.removeTabViewItem( arr.objectAtIndex(j));
      ShowNext := true;
      Break;
    end;
    xd := xd + lwid[i];
  end;

  // 3. try to keep the tabs on the left side until it's not wide enough
  //    and ShowPrev if necessary
  for i := ofs-1 downto 0 do begin
    if xd + lwid[i] > frw then begin
      for j:=i downto 0 do
        aview.removeTabViewItem( arr.objectAtIndex(j));
      ShowPrev := true;
      Break;
    end;
    xd := xd + lwid[i];
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

procedure TCocoaTabControl.dealloc;
begin
  if Assigned(fulltabs) then begin
    fulltabs.release;
    fulltabs := nil;
  end;
  inherited dealloc;
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
  currentIndex := index;

  itm := NSTabViewItem(fulltabs.objectAtIndex(index));

  i := tabViewItems.indexOfObject(itm);
  if i <> NSNotFound then
  begin
    inherited selectTabViewItemAtIndex(NSInteger(i));
  end
  else begin
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
  inherited lclSetEnabled(AEnabled);
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
  if Assigned(callback) then
  begin
    Result:= callback.shouldSelectTabViewItem( IndexOfTab( self, tabViewItem) );
  end;
end;

procedure TCocoaTabControl.tabView_willSelectTabViewItem(tabView: NSTabView;
  tabViewItem: NSTabViewItem);
begin
  if Assigned(callback) then
  begin
    callback.willSelectTabViewItem( IndexOfTab( self, tabViewItem) );
  end;
end;

procedure TCocoaTabControl.tabView_didSelectTabViewItem(tabView: NSTabView;
  tabViewItem: NSTabViewItem);
begin
  //it's called together with "willSelect"

  currentIndex:= IndexOfTab( self, tabViewItem );

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
    callback.didSelectTabViewItem( currentIndex );
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
  itm : NSTabViewItem;
begin
  itm := self.tabViewItemAtPoint( self.convertPoint_fromView(event.locationInWindow, nil ));
  if not Assigned(itm) then
  begin
    inherited mouseDown(event);
    Exit;
  end;

  if not (Assigned(callback) and callback.MouseUpDownEvent(event, false, true)) then
  begin
    inherited mouseDown(event);

    if Assigned(callback) then
    begin
      callback.MouseUpDownEvent(event, True);
    end;
  end
end;

procedure TCocoaTabControl.mouseUp(event: NSEvent);
var
  itm : NSTabViewItem;
begin
  itm := self.tabViewItemAtPoint( self.convertPoint_fromView(event.locationInWindow, nil ));
  if not Assigned(itm) then
  begin
    inherited mouseUp(event);
    Exit;
  end;

  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaTabControl.rightMouseDown(event: NSEvent);
var
  itm : NSTabViewItem;
begin
  itm := self.tabViewItemAtPoint( self.convertPoint_fromView(event.locationInWindow, nil ));
  if not Assigned(itm) then
  begin
    inherited rightMouseDown(event);
    Exit;
  end;

  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaTabControl.rightMouseUp(event: NSEvent);
var
  itm : NSTabViewItem;
begin
  itm := self.tabViewItemAtPoint( self.convertPoint_fromView(event.locationInWindow, nil ));
  if not Assigned(itm) then
  begin
    inherited rightMouseUp(event);
    Exit;
  end;

  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaTabControl.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaTabControl.otherMouseDown(event: NSEvent);
var
  itm : NSTabViewItem;
begin
  itm := self.tabViewItemAtPoint( self.convertPoint_fromView(event.locationInWindow, nil ));
  if not Assigned(itm) then
  begin
    inherited otherMouseDown(event);
    Exit;
  end;

  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaTabControl.otherMouseUp(event: NSEvent);
var
  itm : NSTabViewItem;
begin
  itm := self.tabViewItemAtPoint( self.convertPoint_fromView(event.locationInWindow, nil ));
  if not Assigned(itm) then
  begin
    inherited otherMouseUp(event);
    Exit;
  end;

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
var
  itm : NSTabViewItem;
begin
  itm := self.tabViewItemAtPoint( self.convertPoint_fromView(event.locationInWindow, nil ));
  if not Assigned(itm) then
  begin
    inherited mouseMoved(event);
    Exit;
  end;

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
  if currentIndex = 0 then Exit;
  extselectTabViewItemAtIndex( currentIndex-1 );
end;

procedure TCocoaTabControl.extTabNextButtonClick(sender: id);
begin
  if currentIndex = fulltabs.count - 1 then Exit;
  extselectTabViewItemAtIndex( currentIndex+1 );
end;


end.

