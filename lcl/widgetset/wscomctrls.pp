{ $Id$}
{
 *****************************************************************************
 *                               WSComCtrls.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit WSComCtrls;

{$mode objfpc}{$H+}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
  Classes,
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  ComCtrls, Controls, ImgList, StdCtrls,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSExtCtrls, WSStdCtrls,
  WSToolwin;

type
  { TWSStatusBar }

  TWSStatusBarClass = class of TWSStatusBar;
  TWSStatusBar = class(TWSWinControl)
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); virtual;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); virtual;
    class procedure SetSizeGrip(const AStatusBar: TStatusBar; SizeGrip: Boolean); virtual;
    class procedure Update(const AStatusBar: TStatusBar); virtual;
  end;

  { TWSTabSheet }

  TWSTabSheet = class(TWSCustomPage)
  end;

  { TWSPageControl }

  TWSPageControl = class(TWSCustomNotebook)
  end;

  { TWSCustomListView }
  TWSListViewItemChange = (lvicText, lvicImage);
  TWSListViewItemChanges = set of TWSListViewItemChange;

  TWSCustomListViewClass = class of TWSCustomListView;
  TWSCustomListView = class(TWSWinControl)
    // Column
    class procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); virtual;
    class function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer; virtual;
    class procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); virtual;
    class procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); virtual;
    class procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment); virtual;
    class procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean); virtual;
    class procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String); virtual;
    class procedure ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer); virtual;
    class procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer); virtual;
    class procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer); virtual;
    class procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer); virtual;
    class procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean); virtual;
              
    // Item          
    class procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); virtual;
    class function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; virtual;
    class function  ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem): Boolean; virtual;
    class function  ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint; virtual;
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; virtual; // returns True if supported
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); virtual;
    class procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean); virtual;
    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); virtual;
    class function ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean; virtual;
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); virtual;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); virtual;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean); virtual;
    
    // LV
    class procedure BeginUpdate(const ALV: TCustomListView); virtual;
    class procedure EndUpdate(const ALV: TCustomListView); virtual;

    class function GetBoundingRect(const ALV: TCustomListView): TRect; virtual;
    class function GetDropTarget(const ALV: TCustomListView): Integer; virtual;
    class function GetFocused(const ALV: TCustomListView): Integer; virtual;
    class function GetHoverTime(const ALV: TCustomListView): Integer; virtual;
    class function GetItemAt(const ALV: TCustomListView; x,y: integer): Integer; virtual;
    class function GetSelCount(const ALV: TCustomListView): Integer; virtual;
    class function GetSelection(const ALV: TCustomListView): Integer; virtual;
    class function GetTopItem(const ALV: TCustomListView): Integer; virtual;
    class function GetViewOrigin(const ALV: TCustomListView): TPoint; virtual;
    class function GetVisibleRowCount(const ALV: TCustomListView): Integer; virtual;
    
    class procedure SetAllocBy(const ALV: TCustomListView; const AValue: Integer); virtual;
    class procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer); virtual;
    class procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles); virtual;
    class procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer); virtual;
//    class procedure SetIconOptions(const ALV: TCustomListView; const AValue: TIconOptions); virtual;
    class procedure SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageList); virtual;
    class procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); virtual;
    class procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); virtual;
    class procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); virtual;
    class procedure SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer); virtual;
    class procedure SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint); virtual;
    class procedure SetViewStyle(const ALV: TCustomListView; const Avalue: TViewStyle); virtual;
  end;

  { TWSListView }                             

  TWSListView = class(TWSCustomListView)
  end;

  { TWSProgressBar }

  TWSProgressBarClass = class of TWSProgressBar;
  TWSProgressBar = class(TWSWinControl)
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); virtual;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); virtual;
  end;

  { TWSCustomUpDown }

  TWSCustomUpDown = class(TWSCustomControl)
  end;

  { TWSUpDown }

  TWSUpDown = class(TWSCustomUpDown)
  end;

  { TWSToolButton }

  TWSToolButton = class(TWSCustomControl)
  end;

  { TWSToolBar }

  TWSToolbarClass = class of TWSToolbar;
  TWSToolBar = class(TWSToolWindow)
{$ifdef OldToolbar}  
    class function  GetButtonCount(const AToolBar: TToolBar): integer; virtual;
    class procedure InsertToolButton(const AToolBar: TToolbar; const AControl: TControl); virtual;
    class procedure DeleteToolButton(const AToolBar: TToolbar; const AControl: TControl); virtual;
{$endif}    
  end;

  { TWSTrackBar }

  TWSTrackBarClass = class of TWSTrackBar;
  TWSTrackBar = class(TWSWinControl)
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); virtual;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; virtual;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); virtual;
  end;

  { TWSCustomTreeView }

  TWSCustomTreeView = class(TWSCustomControl)
  end;

  { TWSTreeView }

  TWSTreeView = class(TWSCustomTreeView)
  end;


implementation


{ TWSStatusBar }

class procedure TWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer);
begin
end;

class procedure TWSStatusBar.SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
begin
end;

class procedure TWSStatusBar.SetSizeGrip(const AStatusBar: TStatusBar;
  SizeGrip: Boolean);
begin
end;

class procedure TWSStatusBar.Update(const AStatusBar: TStatusBar);
begin
end;
    
{ TWSCustomListView }

class procedure TWSCustomListView.ColumnDelete(const ALV: TCustomListView;
  const AIndex: Integer);
begin
end;

class function TWSCustomListView.ColumnGetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn): Integer;
begin
  Result := -1;
end;

class procedure TWSCustomListView.ColumnInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn);
begin
end;

class procedure TWSCustomListView.ColumnMove(const ALV: TCustomListView;
  const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
begin
end;

class procedure TWSCustomListView.ColumnSetAlignment(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn;
  const AAlignment: TAlignment);
begin
end;

class procedure TWSCustomListView.ColumnSetAutoSize(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
begin
end;

class procedure TWSCustomListView.ColumnSetCaption(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const ACaption: String);
begin
end;

class procedure TWSCustomListView.ColumnSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer);
begin
end;

class procedure TWSCustomListView.ColumnSetMaxWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer);
begin
end;

class procedure TWSCustomListView.ColumnSetMinWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
begin
end;

class procedure TWSCustomListView.ColumnSetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
begin
end;

class procedure TWSCustomListView.ColumnSetVisible(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
begin
end;

class procedure TWSCustomListView.ItemDelete(const ALV: TCustomListView;
  const AIndex: Integer);
begin
end;

class function TWSCustomListView.ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect;
begin
  Result := Rect(0,0,0,0);
end;

class function TWSCustomListView.ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem): Boolean;
begin
  Result := False;
end;

class function TWSCustomListView.ItemGetPosition(const ALV: TCustomListView;
  const AIndex: Integer): TPoint;
begin
  Result := Point(0, 0);
end;

class function TWSCustomListView.ItemGetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  out AIsSet: Boolean): Boolean;
begin
  // returns True if supported
  Result := False;
  AIsSet:=false;
end;

class procedure TWSCustomListView.ItemInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem);
begin
end;

class procedure TWSCustomListView.ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean);
begin
end;

class procedure TWSCustomListView.ItemSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem;
  const ASubIndex, AImageIndex: Integer);
begin
end;

class function TWSCustomListView.ItemSetPosition(const ALV: TCustomListView;
  const AIndex: Integer; const ANewPosition: TPoint): Boolean;
begin
  Result := False;
end;

class procedure TWSCustomListView.ItemSetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  const AIsSet: Boolean);
begin
end;

class procedure TWSCustomListView.ItemSetText(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer;
  const AText: String);
begin
end;

class procedure TWSCustomListView.ItemShow(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
begin
end;

class procedure TWSCustomListView.BeginUpdate(const ALV: TCustomListView);
begin
end;

class procedure TWSCustomListView.EndUpdate(const ALV: TCustomListView);
begin
end;

class function TWSCustomListView.GetBoundingRect(const ALV: TCustomListView): TRect;
begin
  Result := Rect(0,0,0,0);
end;

class function TWSCustomListView.GetDropTarget(const ALV: TCustomListView): Integer;
begin       
  Result := -1;
end;

class function TWSCustomListView.GetFocused(const ALV: TCustomListView): Integer;
begin
  Result := -1;
end;

class function TWSCustomListView.GetHoverTime(const ALV: TCustomListView): Integer;
begin
  Result := -1;
end;

class function TWSCustomListView.GetItemAt(const ALV: TCustomListView; x,y: integer): Integer;
begin
  result:=-1;
end;

class function TWSCustomListView.GetSelCount(const ALV: TCustomListView): Integer;
begin
  Result := 0;
end;

class function TWSCustomListView.GetSelection(const ALV: TCustomListView): Integer;
begin
  Result := -1;
end;

class function TWSCustomListView.GetTopItem(const ALV: TCustomListView): Integer;
begin
  Result := -1;
end;

class function TWSCustomListView.GetViewOrigin(const ALV: TCustomListView): TPoint;
begin
  Result := Point(0, 0);
end;

class function TWSCustomListView.GetVisibleRowCount(const ALV: TCustomListView): Integer;
begin
  Result := 0;
end;

class procedure TWSCustomListView.SetAllocBy(const ALV: TCustomListView; const AValue: Integer);
begin
end;

class procedure TWSCustomListView.SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer);
begin
end;

class procedure TWSCustomListView.SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles);
begin
end;

class procedure TWSCustomListView.SetHoverTime(const ALV: TCustomListView; const AValue: Integer);
begin
end;

class procedure TWSCustomListView.SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageList);
begin
end;

class procedure TWSCustomListView.SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean);
begin
end;

class procedure TWSCustomListView.SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties);
begin
end;

class procedure TWSCustomListView.SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle);
begin
end;

class procedure TWSCustomListView.SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer);
begin
end;

class procedure TWSCustomListView.SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint);
begin
end;

class procedure TWSCustomListView.SetViewStyle(const ALV: TCustomListView; const Avalue: TViewStyle);
begin
end;


{ TWSProgressBar }

class procedure TWSProgressBar.ApplyChanges(const AProgressBar: TCustomProgressBar);
begin
end;

class procedure TWSProgressBar.SetPosition(const AProgressBar: TCustomProgressBar;
  const NewPosition: integer);
begin
end;

{ TWSToolbar }

{$ifdef OldToolbar}

class function TWSToolbar.GetButtonCount(const AToolBar: TToolBar): integer;
begin
  Result := 0;
end;

class procedure TWSToolbar.InsertToolButton(const AToolBar: TToolbar; const AControl: TControl);
begin
end;

class procedure TWSToolbar.DeleteToolButton(const AToolBar: TToolbar; const AControl: TControl);
begin
end;

{$endif}

{ TWSTrackBar }

class procedure TWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
begin
end;

class function  TWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar): integer;
begin
  Result := 0;
end;

class procedure TWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer);
begin
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomStatusBar, TWSStatusBar);
//  RegisterWSComponent(TCustomTabSheet, TWSTabSheet);
//  RegisterWSComponent(TCustomPageControl, TWSPageControl);
//  RegisterWSComponent(TCustomListView, TWSCustomListView);
//  RegisterWSComponent(TCustomListView, TWSListView);
//  RegisterWSComponent(TCustomProgressBar, TWSProgressBar);
//  RegisterWSComponent(TCustomCustomUpDown, TWSCustomUpDown);
//  RegisterWSComponent(TCustomUpDown, TWSUpDown);
//  RegisterWSComponent(TCustomToolButton, TWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TWSToolBar);
//  RegisterWSComponent(TCustomToolButton, TWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TWSToolBar);
//  RegisterWSComponent(TCustomTrackBar, TWSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TWSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TWSTreeView);
////////////////////////////////////////////////////
end.
