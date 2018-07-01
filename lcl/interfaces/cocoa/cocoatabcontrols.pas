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

{.$DEFINE COCOA_DEBUG_SETBOUNDS}
{.$DEFINE COCOA_DEBUG_LISTVIEW}
{.$DEFINE COCOA_SPIN_DEBUG}
{.$DEFINE COCOA_SPINEDIT_INSIDE_CONTAINER}
{.$DEFINE COCOA_SUPERVIEW_HEIGHT}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  CGGeometry,
  // Libs
  MacOSAll, CocoaAll, CocoaUtils, //CocoaGDIObjects,
  cocoa_extra, CocoaPrivate,
  // LCL
  Controls;

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
  end;

  { TCocoaTabControl }

  TCocoaTabControl = objcclass(NSTabView, NSTabViewDelegateProtocol)
  public
    callback: ITabControlCallback;

    lclEnabled: Boolean;
    // lcl
    function lclIsEnabled: Boolean; override;
    procedure lclSetEnabled(AEnabled: Boolean); override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclClientFrame: TRect; override;
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
end;

  { TCocoaTabPageView }

  TCocoaTabPageView = objcclass(TCocoaCustomControl)
  public
    tabView: TCocoaTabControl;
    tabPage: TCocoaTabPage;
    procedure setHidden(Ahidden: Boolean); override;
  end;

implementation

uses CocoaWSComCtrls; //todo: get rid of use of WS unit

{ TCocoaTabPageView }

procedure TCocoaTabPageView.setHidden(Ahidden: Boolean);
begin
  // Should never be hidden. (The parent NSView would show/hide tabs)
  // it seems, that lclSetVisible interferes with
  // control visibility TCocoaCustomControl
  // todo: there should be a cleaner solution than overriding this method
  inherited setHidden(false);
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

{ TCocoaTabControl }

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
begin
  if isFlipped then
    Result:=NSRectToRect( contentRect )
  else
    NSToLCLRect( contentRect, frame.size.height, Result );
end;

function TCocoaTabControl.tabView_shouldSelectTabViewItem(tabView: NSTabView;
  tabViewItem: NSTabViewItem): Boolean;
begin
  Result := True;
end;

procedure TCocoaTabControl.tabView_willSelectTabViewItem(tabView: NSTabView;
  tabViewItem: NSTabViewItem);
begin
  if Assigned(callback) then
    callback.willSelectTabViewItem( tabview.indexOfTabViewItem(tabViewItem) );
end;

procedure TCocoaTabControl.tabView_didSelectTabViewItem(tabView: NSTabView;
  tabViewItem: NSTabViewItem);
var
  i: Integer;
  lTabView, lCurSubview: NSView;
  lLCLControl: TWinControl;
  lBounds: TRect;
  lCurCallback: ICommonCallback;
begin
  if Assigned(callback) then
    callback.didSelectTabViewItem( tabview.indexOfTabViewItem(tabViewItem) );

  // Update the coordinates of all children of this tab
  // Fixes bug 31914: TPageControl problems with Cocoa
  lTabView := tabViewItem.view.subViews.objectAtIndex(0);
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
  end;
end;

procedure TCocoaTabControl.tabViewDidChangeNumberOfTabViewItems(
  TabView: NSTabView);
begin

end;

procedure TCocoaTabControl.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) then callback.MouseUpDownEvent(event);
  // do not block?
  inherited mouseDown(event);
end;

procedure TCocoaTabControl.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) then callback.MouseUpDownEvent(event);
  // do not block?
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


end.

