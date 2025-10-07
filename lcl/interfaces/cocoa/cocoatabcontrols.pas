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
  Types, Classes, SysUtils,
  MacOSAll, CocoaAll, CocoaUtils, CocoaPrivate, CocoaCallback, CocoaConst,
  CocoaCustomControl;

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
    triggeringByLCL: Boolean;

    { various indexes in fulltabs }
    currentIndex : Integer;     // index of the current tab
    visibleLeftIndex: Integer;  // index shown in TabView on the left
    leftKeepAmount: Integer;    // left tab amount to keep, equals currentIndex-visibleLeftIndex

    procedure attachAllTabs; message 'attachAllTabs';
    procedure updateVariousIndex; message 'updateVariousIndex';

  public
    callback: ITabControlCallback;

    fulltabs : NSMutableArray;  // the full list of NSTabViewItems
    lclEnabled: Boolean;
    // cocoa
    class function alloc: id; override;
    procedure dealloc; override;
    procedure setFrame(aframe: NSRect); override;
    procedure setTabViewType(newValue: NSTabViewType); override;
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
    procedure exttabInsertTabViewItem_atIndex(lTabPage: NSTabViewItem; index: integer);
      message 'exttabInsertTabViewItem:atIndex:';
    procedure exttabMoveTabViewItem_toIndex(lTabPage: NSTabViewItem; NewIndex: integer);
      message 'exttabMoveTabViewItem:toIndex:';
    procedure exttabRemoveTabViewItem(removedTabPage: NSTabViewItem);
      message 'exttabRemoveTabViewItem:';
    function exttabIndexOfTabViewItem(lTabPage: NSTabViewItem): NSInteger;
      message 'exttabIndexOfTabViewItem:';
    procedure extselectTabViewItemAtIndex(index: NSInteger);
      message 'extselectTabViewItemAtIndex:';
  end;

  { TCocoaTabControlArrow }

  TCocoaTabControlArrow = objcclass(NSButton)
  private
    _tabControl: TCocoaTabControl;
    _lastMouseDownTime: NSDate;
  private
    function shouldSpeedUp(): Boolean; message 'shouldSpeedUp';
  public
    procedure mouseDown(theEvent: NSEvent); override;
    procedure prevClick(sender: id); message 'prevClick:';
    procedure nextClick(sender: id); message 'nextClick:';
  end;

  { TCocoaTabPageView }

  TCocoaTabPageView = objcclass(TCocoaCustomControl)
  public
    tabView: TCocoaTabControl;
    tabPage: TCocoaTabPage;
    procedure setFrame(arect: NSRect); override;
  end;

  { TCocoaTabControlContainer }

  // according to TTabControl (not Ancestors, not descendants)
  // it's not a real Tab Control, just a contianer
  // the real Tab Control is TNoteBookStringsTabControl

  TCocoaTabControlContainer = objcclass(NSView)
  private
    _boxView: NSBox;
  public
    function init: id; override;
    procedure setFrame(newValue: NSRect); override;
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

function AllocArrowButton(tabControl:TCocoaTabControl; isPrev:Boolean): NSButton;
var
  btn : TCocoaTabControlArrow;
begin
  btn := TCocoaTabControlArrow.alloc.initWithFrame(NSZeroRect);
  btn._tabControl := tabControl;
  btn.setBezelStyle(NSRegularSquareBezelStyle);
  btn.setButtonType(NSMomentaryLightButton);

  if isPrev then
    btn.setTitle( NSSTR_TABCONTROL_PREV_ARROW )
  else
    btn.setTitle( NSSTR_TABCONTROL_NEXT_ARROW );

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

  if dst.tabViewType = NSTopTabsBezelBorder then
    org.y := arrow_vofs
  else
    org.y := dst.frame.size.height - abtn.frame.size.height - arrow_vofs;

  if isPrev then
    org.x := arrow_hofs
  else
    org.x := dst.frame.size.width - abtn.frame.size.width - arrow_hofs;

  abtn.setFrameOrigin(org);
end;


procedure AllocPrevNext(aview: TCocoaTabControl);
begin
  aview.prevarr := AllocArrowButton(aview, true);
  aview.addSubview(aview.prevarr);
  aview.prevarr.setTarget(aview.prevarr);
  aview.prevarr.setAction( ObjCSelector('prevClick:') );

  aview.nextarr := AllocArrowButton(aview, false);
  aview.addSubview(aview.nextarr);
  aview.nextarr.setTarget(aview.nextarr);
  aview.nextarr.setAction( ObjCSelector('nextClick:') );
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

  tryToKeepIndex : NSInteger;
  leftIndex : Integer;
begin
  ShowPrev := false;
  ShowNext := false;

  // ReviseTabs() supports tpTop and tpBottom only
  if (aview.tabViewType=NSLeftTabsBezelBorder) or (aview.tabViewType=NSRightTabsBezelBorder) then exit;

  if aview.fulltabs.count=0 then exit;

  tryToKeepIndex:= aview.currentIndex - aview.leftKeepAmount;
  if tryToKeepIndex < 0 then
    tryToKeepIndex:= 0;

  // AttachAllTabs() has been modified to not remove the selectedTabViewItem first,
  // and no longer trigger tabView_didSelectTabViewItem
  // regardless of whether aview.fulltabs.count>aview.tabViewItems.count,
  // tabs need to be attached because the order may have been adjusted.
  aview.attachAllTabs;

  minw := aview.minimumSize.width;
  if (minw<aview.frame.size.width) then Exit;

  arr := aview.tabViewItems;

  lw := 0;
  SetLength(lwid, arr.count);
  for i := 0 to Integer(arr.count) - 1 do
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

  // 2. try to keep the tabs on the left side, the amount not exceed leftKeepAmount,
  //    in order to fix the position of currentTab.
  leftIndex := ofs;
  for i := ofs-1 downto tryToKeepIndex do begin
    if xd + lwid[i] > frw then begin
      ShowPrev := true;
      Break;
    end;
    xd := xd + lwid[i];
    leftIndex := i;
  end;

  // 3. try to keep the tabs on the right side until it's not wide enough
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

  // 4. try to keep the tabs on the left side until it's not wide enough
  //    and ShowPrev if necessary
  if leftIndex <= 0 then
    exit;
  for i := leftIndex-1 downto 0 do begin
    if xd + lwid[i] > frw then begin
      ShowPrev := true;
      Break;
    end;
    xd := xd + lwid[i];
    leftIndex := i;
  end;
  for j:=leftIndex-1 downto 0 do
    aview.removeTabViewItem( arr.objectAtIndex(j));
end;

procedure UpdateTabAndArrowVisibility(aview: TCocoaTabControl);
var
  showNext : Boolean;
  showPrev : Boolean;
  responder: NSResponder;
begin
  responder:= nil;
  if Assigned(aview.window) then
    responder:= aview.window.firstResponder;

  ReviseTabs(aview, showPrev, showNExt);
  aview.updateVariousIndex;
  if Assigned(aview.prevarr) then
  begin
    PlaceButton(true, aview.prevarr, aview);
    {$ifdef BOOLFIX}
    aview.prevarr.setHidden_(Ord(not showPrev));
    {$else}
    aview.prevarr.setHidden(not showPrev);
    {$endif}
  end;
  if Assigned(aview.nextarr) then
  begin
    PlaceButton(false, aview.nextarr, aview);
    {$ifdef BOOLFIX}
    aview.nextarr.setHidden_(Ord(not showNext));
    {$else}
    aview.nextarr.setHidden(not showNext);
    {$endif}
  end;

  if Assigned(aview.window) then begin
    if Assigned(responder) and (responder<>aview.window.firstResponder) then
      aview.window.makeFirstResponder(responder);
  end;
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

{ TCocoaTabControlContainer }

function TCocoaTabControlContainer.init: id;
begin
  Result:=inherited init;
  _boxView:= TCocoaGroupBox.new;
  _boxView.setTitle( NSString.string_ );
  _boxView.setBorderType( NSLineBorder );
  self.addSubview( _boxView );
  _boxView.release;
end;

procedure TCocoaTabControlContainer.setFrame(newValue: NSRect);
var
  lclRect: TRect;
begin
  inherited;
  newValue.origin:= NSZeroPoint;
  newValue.size.height:= newValue.size.height + 4;
  NSToLCLRect( newValue, self.frame.size.height, lclRect );
  _boxView.lclSetFrame( lclRect );
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

// by ensuring that selectedTabViewItem cannot be removed
// and tabView_didSelectTabViewItem is not triggered anymore
procedure TCocoaTabControl.attachAllTabs;
var
  i : integer;
  itm: NSTabViewItem;
begin
  // only selectedItem reserved
  for itm{%H-} in tabViewItems do begin
    if itm <> selectedTabViewItem then
      removeTabViewItem( itm );
  end;

  // insert all tabs in the order of fulltabs again
  i:= 0;
  for itm{%H-} in fulltabs do begin
    if itm <> selectedTabViewItem then
      insertTabViewItem_atIndex( itm, i );
    inc( i );
  end;
end;

procedure TCocoaTabControl.updateVariousIndex;
begin
  if numberOfTabViewItems > 0 then begin
    visibleLeftIndex:= fulltabs.indexOfObject( tabViewItemAtIndex(0) );
    leftKeepAmount:= currentIndex - visibleLeftIndex;
  end else begin
    visibleLeftIndex:= -1;
    leftKeepAmount:= 0;
  end;
end;

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

procedure TCocoaTabControl.setTabViewType(newValue: NSTabViewType);
begin
  Inherited;
  UpdateTabAndArrowVisibility(self);
end;

procedure TCocoaTabControl.extselectTabViewItemAtIndex( index:NSInteger );
var
  itm: NSTabViewItem;
  visibleIndex: NSInteger;
  oldKeepAmount: Integer;
begin
  if (index<0) or (index>=fulltabs.count) then Exit;

  itm:= NSTabViewItem( fulltabs.objectAtIndex(index) );
  visibleIndex:= indexOfTabViewItem( itm );
  if visibleIndex <> NSNotFound then begin
    inherited selectTabViewItemAtIndex( visibleIndex );
  end else begin
    oldKeepAmount:= leftKeepAmount;
    attachAllTabs;
    inherited selectTabViewItemAtIndex( index );
    leftKeepAmount:= oldKeepAmount;
    UpdateTabAndArrowVisibility( self );
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
    Result.Bottom := -4;
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

  if triggeringByLCL then
    exit;

  currentIndex:= IndexOfTab( self, tabViewItem );
  leftKeepAmount:= currentIndex - visibleLeftIndex;

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

procedure TCocoaTabControl.exttabMoveTabViewItem_toIndex(
  lTabPage: NSTabViewItem; NewIndex: integer);
var
  isMovingCurrentPage: Boolean;
  OldIndex: Integer;
begin
  if fulltabs.count=0 then
    Exit;

  OldIndex := exttabIndexOfTabViewItem( lTabPage );
  if OldIndex < 0 then
    Exit;

  if NewIndex > fulltabs.count then
    NewIndex:= fulltabs.count;

  isMovingCurrentPage := (OldIndex=currentIndex);

  fulltabs.removeObjectAtIndex( OldIndex );
  fulltabs.insertObject_atIndex( lTabPage, NewIndex );

  if isMovingCurrentPage then begin
    currentIndex:= NewIndex;
    leftKeepAmount:= currentIndex - visibleLeftIndex;
  end else begin
    if (OldIndex<currentIndex) and (NewIndex>=currentIndex) then
      dec( currentIndex )
    else if (OldIndex>currentIndex) and (NewIndex<=currentIndex) then
      inc( currentIndex );
  end;

  UpdateTabAndArrowVisibility( self );
end;

procedure TCocoaTabControl.exttabInsertTabViewItem_atIndex(
  lTabPage:NSTabViewItem; index:integer );
begin
  if index > fulltabs.count then
    index:= fulltabs.count;
  fulltabs.insertObject_atIndex( lTabPage, index );
  if index <= currentIndex then
    inc( currentIndex );

  UpdateTabAndArrowVisibility( self );
end;

procedure TCocoaTabControl.exttabRemoveTabViewItem( removedTabPage: NSTabViewItem );
var
  isRemovingCurrentPage: Boolean;
  removedIndex: Integer;
  nextTabPage: NSTabViewItem;
  prevTabPage: NSTabViewItem;
begin
  triggeringByLCL:= true;
  try
    removedIndex := exttabIndexOfTabViewItem( removedTabPage );
    if removedIndex < 0 then
      Exit;
    isRemovingCurrentPage:= (removedIndex=currentIndex);

    fulltabs.removeObjectAtIndex( removedIndex );

    if isRemovingCurrentPage then begin
      // removing current page
      attachAllTabs;
      if currentIndex = fulltabs.count then begin
        dec( currentIndex );
        removeTabViewItem( removedTabPage );
      end else begin
        selectTabViewItemAtIndex( currentIndex );
      end;
    end else begin
      // not removing current page
      // only fulltabs need to be changed,
      // visible TabView auto updated in UpdateTabAndArrowVisibility()
      if removedIndex < currentIndex then begin
        dec( currentIndex );
        if (removedIndex=visibleLeftIndex) and (removedIndex=currentIndex) then
          leftKeepAmount:= 0;
      end;
    end;

    UpdateTabAndArrowVisibility( self );
  finally
    triggeringByLCL:= false;
  end;
end;

function TCocoaTabControl.exttabIndexOfTabViewItem(lTabPage: NSTabViewItem
  ): NSInteger;
begin
  Result := fulltabs.indexOfObject(lTabPage);
  if Result = NSNotFound then
    Result:= -1;
end;

{ TCocoaTabControlArrow }

procedure TCocoaTabControlArrow.mouseDown(theEvent: NSEvent);
begin
  _lastMouseDownTime := NSDate.date;
  inherited;
  _lastMouseDownTime := nil;
end;

function TCocoaTabControlArrow.shouldSpeedUp(): Boolean;
const
  FOUR_MODIFIER_FLAGS = NSShiftKeyMask
                     or NSControlKeyMask
                     or NSAlternateKeyMask
                     or NSCommandKeyMask;
begin
  if (NSApp.currentEvent.modifierFlags and FOUR_MODIFIER_FLAGS)<>0 then
    exit(true);
  if NSDate.date.timeIntervalSinceDate(_lastMouseDownTime) > NSEvent.doubleClickInterval then
    exit(true);
  Result := false;
end;

procedure TCocoaTabControlArrow.prevClick(sender: id);
var
  currentIndex: Integer;
begin
  currentIndex := _tabControl.currentIndex;
  if currentIndex = 0 then
    Exit;
  if shouldSpeedUp() then
    currentIndex := 0
  else
    dec(currentIndex);
  _tabControl.extselectTabViewItemAtIndex(currentIndex);
end;

procedure TCocoaTabControlArrow.nextClick(sender: id);
var
  currentIndex: Integer;
begin
  currentIndex := _tabControl.currentIndex;
  if currentIndex = Integer(_tabControl.fulltabs.count) - 1 then
    Exit;
  if shouldSpeedUp() then
    currentIndex := Integer(_tabControl.fulltabs.count) - 1
  else
    inc(currentIndex);
  _tabControl.extselectTabViewItemAtIndex(currentIndex);
end;

end.

