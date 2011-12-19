{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSComCtrls.pp                          *
 *                              -------------------                          *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonWSComCtrls;

{$mode objfpc}{$H+}

interface

// defines
{$I carbondefines.inc}

uses
  // libs
  MacOSAll,
  // LCL
  Classes, Controls, ComCtrls, StdCtrls, LCLType, LCLProc, Graphics, Math, ImgList,
  // widgetset
  WSComCtrls, WSLCLClasses, WSControls, WSProc,
  // LCL Carbon
  CarbonDef, CarbonPrivate, CarbonBars, CarbonStrings, CarbonWSControls,
  CarbonListViews;

type

  { TCarbonWSStatusBar }

  TCarbonWSStatusBar = class(TWSStatusBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure Update(const AStatusBar: TStatusBar); override;
  end;

  { TCarbonWSTabSheet }

  TCarbonWSTabSheet = class(TWSTabSheet)
  published
  end;


  { TCarbonWSCustomPage }

  TCarbonWSCustomPage = class(TWSCustomPage)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
  end;

  { TCarbonWSCustomNotebook }

  TCarbonWSCustomNotebook = class(TWSCustomTabControl)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure AddPage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemovePage(const ATabControl: TCustomTabControl; const AIndex: integer); override;

    //class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; override;
    //class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; override;
    //class function GetPageRealIndex(const ATabControl: TCustomTabControl; AIndex: Integer): Integer; override;
    class function GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer; override;
    class procedure SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer); override;
    class procedure SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean); override;
  end;

  { TCarbonWSPageControl }

  TCarbonWSPageControl = class(TWSPageControl)
  published
  end;

  { TCarbonWSCustomListView }

  TCarbonWSCustomListView = class(TWSCustomListView)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    // Column
    class procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer; override;
    class procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment); override;
    //class procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean); override;
    class procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String); override;
    class procedure ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer); override;
    class procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer); override;
    class procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer); override;
    class procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer); override;
    class procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean); override;

    // Item
    class procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; override;
    class function  ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem): Boolean; override;
    class function  ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint; override;
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; override; // returns True if supported
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); override;
    class procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean); override;
    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); override;
    //class function ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean; override;
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); override;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean); override;

    // LV
    //class procedure BeginUpdate(const ALV: TCustomListView); override;
    //class procedure EndUpdate(const ALV: TCustomListView); override;

    class function GetBoundingRect(const ALV: TCustomListView): TRect; override;
    //class function GetDropTarget(const ALV: TCustomListView): Integer; override;
    class function GetFocused(const ALV: TCustomListView): Integer; override;
    //class function GetHoverTime(const ALV: TCustomListView): Integer; override;
    class function GetItemAt(const ALV: TCustomListView; x,y: integer): Integer; override;
    class function GetSelCount(const ALV: TCustomListView): Integer; override;
    //class function GetSelection(const ALV: TCustomListView): Integer; override;
    class function GetTopItem(const ALV: TCustomListView): Integer; override;
    class function GetViewOrigin(const ALV: TCustomListView): TPoint; override;
    class function GetVisibleRowCount(const ALV: TCustomListView): Integer; override;

    //class procedure SetAllocBy(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer); override;
    //class procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles); override;
    //class procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageList); override;
    class procedure SetItemsCount(const ALV: TCustomListView; const Avalue: Integer); override;
    class procedure SetOwnerData(const ALV: TCustomListView; const AValue: Boolean); override;
    class procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); override;
    class procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); override;
    class procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); override;
    class procedure SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer;
      const ASortDirection: TSortDirection); override;
    class procedure SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint); override;
    class procedure SetViewStyle(const ALV: TCustomListView; const AValue: TViewStyle); override;
  end;

  { TCarbonWSListView }

  TCarbonWSListView = class(TWSListView)
  published
  end;

  { TCarbonWSProgressBar }

  TCarbonWSProgressBar = class(TWSProgressBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
    class procedure SetStyle(const AProgressBar: TCustomProgressBar; const AStyle: TProgressBarStyle); override;
  end;

  { TCarbonWSCustomUpDown }

  TCarbonWSCustomUpDown = class(TWSCustomUpDown)
  published
  end;

  { TCarbonWSUpDown }

  TCarbonWSUpDown = class(TWSUpDown)
  published
  end;

  { TCarbonWSToolButton }

  TCarbonWSToolButton = class(TWSToolButton)
  published
  end;

  { TCarbonWSToolBar }

  TCarbonWSToolBar = class(TWSToolBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCarbonWSTrackBar }

  TCarbonWSTrackBar = class(TWSTrackBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); override;
  end;

  { TCarbonWSCustomTreeView }

  TCarbonWSCustomTreeView = class(TWSCustomTreeView)
  published
  end;

  { TCarbonWSTreeView }

  TCarbonWSTreeView = class(TWSTreeView)
  published
  end;


implementation

uses
  CarbonProc, CarbonTabs, CarbonDbgConsts;

{ TCarbonWSToolBar }

class function TCarbonWSToolBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonToolBar.Create(AWinControl, AParams));
  // TCarbonCustomControl(Result).
end;

{ TCarbonWSStatusBar }

{------------------------------------------------------------------------------
  Method:  TCarbonWSStatusBar.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new status bar in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSStatusBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonStatusBar.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSStatusBar.PanelUpdate
  Params:  AStatusBar - LCL status bar
           PanelIndex - Index of panel to update

  Updates the status bar panel
 ------------------------------------------------------------------------------}
class procedure TCarbonWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar;
  PanelIndex: integer);
begin
  if not CheckHandle(AStatusBar, Self, 'PanelUpdate') then Exit;
  
  TCarbonStatusBar(AStatusBar.Handle).UpdatePanel(PanelIndex);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSStatusBar.SetPanelText
  Params:  AStatusBar - LCL status bar
           PanelIndex - Index of panel

  Updates the text of status bar panel
 ------------------------------------------------------------------------------}
class procedure TCarbonWSStatusBar.SetPanelText(const AStatusBar: TStatusBar;
  PanelIndex: integer);
begin
  if not CheckHandle(AStatusBar, Self, 'SetPanelText') then Exit;
  
  TCarbonStatusBar(AStatusBar.Handle).UpdatePanel(PanelIndex);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSStatusBar.Update
  Params:  AStatusBar - LCL status bar

  Updates the status bar
 ------------------------------------------------------------------------------}
class procedure TCarbonWSStatusBar.Update(const AStatusBar: TStatusBar);
begin
  if not CheckHandle(AStatusBar, Self, 'Update') then Exit;
  
  TCarbonStatusBar(AStatusBar.Handle).UpdatePanel;
end;

{ TCarbonWSCustomPage }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomPage.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new custom page in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomPage.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonTab.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomPage.UpdateProperties
  Params:  ACustomPage - LCL custom page

  Update properties of the specified custom page in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
begin
  if not CheckHandle(ACustomPage, Self, 'UpdateProperties') then Exit;

  TCarbonTab(ACustomPage.Handle).UpdateTab;
end;

{ TCarbonWSCustomNotebook }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new custom notebook in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomNotebook.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  if AWinControl is TTabControl then
  begin
    Result := TLCLIntfHandle(TCarbonCustomControl.Create(AWinControl, AParams));
    TCarbonCustomControl(Result).CarbonWidgetFlag := cwdTTabControl;
  end else
    Result := TLCLIntfHandle(TCarbonTabsControl.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.AddPage
  Params:  ATabControl - LCL custom notebook
           AChild    - New tab
           AIndex    - New tab index

  Adds tab with the specified index in notebook in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomNotebook.AddPage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const AIndex: integer);
begin
  if not CheckHandle(ATabControl, Self, 'AddPage') then Exit;
  if AChild.HandleAllocated and not CheckHandle(AChild, Self, 'AddPage AChild') then Exit;

  // create child handle
  AChild.HandleNeeded;
  // add page
  TCarbonTabsControl(ATabControl.Handle).Add(TCarbonTab(AChild.Handle), AIndex);
  // sync PageIndex with LCL
  TCarbonTabsControl(ATabControl.Handle).SetPageIndex(ATabControl.PageIndex);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.MovePage
  Params:  ATabControl - LCL custom notebook
           AChild    - Moved tab
           AIndex    - New tab index

  Moves tab to the specified index in notebook in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomNotebook.MovePage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const NewIndex: integer);
begin
  if not CheckHandle(ATabControl, Self, 'MovePage') then Exit;
  if not CheckHandle(AChild, Self, 'MovePage AChild') then Exit;

  TCarbonTabsControl(ATabControl.Handle).Remove(AChild.PageIndex);
  TCarbonTabsControl(ATabControl.Handle).Add(TCarbonTab(AChild.Handle), NewIndex);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.RemovePage
  Params:  ATabControl - LCL custom notebook
           AIndex    - Removed tab index

  Removes tab with the specified index from notebook in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomNotebook.RemovePage(const ATabControl: TCustomTabControl;
  const AIndex: integer);
begin
  if not CheckHandle(ATabControl, Self, 'RemovePage') then Exit;

  TCarbonTabsControl(ATabControl.Handle).Remove(AIndex);
  // sync PageIndex with LCL
  TCarbonTabsControl(ATabControl.Handle).SetPageIndex(ATabControl.PageIndex);
end;


class function TCarbonWSCustomNotebook.GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer;
var
  p : TPoint;
begin
  if not CheckHandle(ATabControl, Self, 'GetTabIndexAtPos') then Exit;
  p := AClientPos;
  inc(p.y, 35); // todo: find out why AClientPos incorrect for TNotebook
  Result := TCarbonTabsControl(ATabControl.Handle).GetPageIndexAtCursor(p);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.SetPageIndex
  Params:  ATabControl - LCL custom notebook
           AIndex    - New tab index

  Selects tab with the specified index in notebook in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomNotebook.SetPageIndex(const ATabControl: TCustomTabControl;
  const AIndex: integer);
begin
  if not CheckHandle(ATabControl, Self, 'SetPageIndex') then Exit;
  if (AIndex < 0) or (AIndex > ATabControl.PageCount - 1) then
    exit;
  TCarbonTabsControl(ATabControl.Handle).SetPageIndex(AIndex);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.SetTabPosition
  Params:  ATabControl    - LCL custom notebook
           ATabPosition - New position of tabs

  Changes position of the tabs of notebook in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomNotebook.SetTabPosition(const ATabControl: TCustomTabControl;
  const ATabPosition: TTabPosition);
begin
  if not CheckHandle(ATabControl, Self, 'SetTabPosition') then Exit;

  TCarbonTabsControl(ATabControl.Handle).SetTabPosition(ATabPosition);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.ShowTabs
  Params:  ATabControl - LCL custom notebook
           AShowTabs - Tabs visibility

  Changes visibility of all tabs of notebook in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomNotebook.ShowTabs(const ATabControl: TCustomTabControl;
  AShowTabs: boolean);
begin
  if not CheckHandle(ATabControl, Self, 'ShowTabs') then Exit;
  if TCarbonControl(ATabControl.Handle).CarbonWidgetFlag <> cwdTTabControl then
    TCarbonTabsControl(ATabControl.Handle).ShowTabs(AShowTabs);
end;

{ TCarbonWSCustomListView }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomListView.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new list view in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomListView.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  clv : TCarbonListView;
begin
  clv:=TCarbonListView.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(clv);
  if Assigned(clv) then
    clv.SetSelectionMode(True, TListView(AWinControl).MultiSelect);
end;

class procedure TCarbonWSCustomListView.ColumnDelete
  (const ALV: TCustomListView; const AIndex: Integer);
begin
  if not CheckHandle(ALV, Self, 'ColumnDelete') then Exit;
  
  TCarbonListView(ALV.Handle).DeleteColumn(AIndex);
end;

class function TCarbonWSCustomListView.ColumnGetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn): Integer;
begin
  Result := 0;
  if not CheckHandle(ALV, Self, 'ColumnGetWidth') then Exit;

  Result := TCarbonListView(ALV.Handle).GetColumn(AIndex).GetWidth;
end;

class procedure TCarbonWSCustomListView.ColumnInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn);
begin
  if not CheckHandle(ALV, Self, 'ColumnInsert') then Exit;

  TCarbonListView(ALV.Handle).InsertColumn(AIndex, AColumn);
end;

class procedure TCarbonWSCustomListView.ColumnMove(const ALV: TCustomListView;
  const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
begin
  if not CheckHandle(ALV, Self, 'ColumnMove') then Exit;

  TCarbonListView(ALV.Handle).MoveColumn(AOldIndex, ANewIndex, AColumn);
end;

class procedure TCarbonWSCustomListView.ColumnSetAlignment(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment);
begin
  if not CheckHandle(ALV, Self, 'ColumnSetAlignment') then Exit;

  TCarbonListView(ALV.Handle).GetColumn(AIndex).SetAlignment(AAlignment);
end;

class procedure TCarbonWSCustomListView.ColumnSetCaption(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const ACaption: String);
begin
  if not CheckHandle(ALV, Self, 'ColumnSetCaption') then Exit;

  TCarbonListView(ALV.Handle).GetColumn(AIndex).SetCaption(ACaption);
end;

class procedure TCarbonWSCustomListView.ColumnSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer);
begin
  if not CheckHandle(ALV, Self, 'ColumnSetImage') then Exit;

  TCarbonListView(ALV.Handle).GetColumn(AIndex).SetImageIndex(AImageIndex);
end;

class procedure TCarbonWSCustomListView.ColumnSetMaxWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer);
begin
  if not CheckHandle(ALV, Self, 'ColumnSetMaxWidth') then Exit;

  TCarbonListView(ALV.Handle).GetColumn(AIndex).SetMaxWidth(AMaxWidth);
end;

class procedure TCarbonWSCustomListView.ColumnSetMinWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
begin
  if not CheckHandle(ALV, Self, 'ColumnSetMinWidth') then Exit;

  TCarbonListView(ALV.Handle).GetColumn(AIndex).SetMinWidth(AMinWidth);
end;

class procedure TCarbonWSCustomListView.ColumnSetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
var
  Column: TCarbonListColumn;
begin
  if not CheckHandle(ALV, Self, 'ColumnSetWidth') then Exit;

  Column := TCarbonListView(ALV.Handle).GetColumn(AIndex);
  if Column <> nil then Column.SetWidth(AWidth, AColumn.AutoSize); // Avoids crash
end;

class procedure TCarbonWSCustomListView.ColumnSetVisible(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
begin
  if not CheckHandle(ALV, Self, 'ColumnSetVisible') then Exit;

  TCarbonListView(ALV.Handle).GetColumn(AIndex).SetVisible(AVisible);
end;

class procedure TCarbonWSCustomListView.ItemDelete(const ALV: TCustomListView;
  const AIndex: Integer);
begin
  if not CheckHandle(ALV, Self, 'ItemDelete') then Exit;

  TCarbonListView(ALV.Handle).DeleteItem(AIndex);
end;

class function TCarbonWSCustomListView.ItemDisplayRect(const ALV: TCustomListView;
  const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect;
begin
  if not CheckHandle(ALV, Self, 'ItemDisplayRect') then Exit;

  Result := TCarbonListView(ALV.Handle).GetItemRect(AIndex, ASubItem, ACode);
end;

class function TCarbonWSCustomListView.ItemGetChecked(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem): Boolean;
begin
  Result := False;
  if not CheckHandle(ALV, Self, 'ItemGetChecked') then Exit;

  Result := TCarbonListView(ALV.Handle).GetItemChecked(AIndex);
end;

class function TCarbonWSCustomListView.ItemGetPosition(const ALV: TCustomListView;
  const AIndex: Integer): TPoint;
begin
  if not CheckHandle(ALV, Self, 'ItemGetPosition') then Exit;

  Result := TCarbonListView(ALV.Handle).GetItemRect(AIndex).TopLeft;
end;

class function TCarbonWSCustomListView.ItemGetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  out AIsSet: Boolean): Boolean;
begin
  Result := False;
  if not CheckHandle(ALV, Self, 'ItemGetState') then Exit;

  Result := TCarbonListView(ALV.Handle).GetItemState(AIndex, AState, AIsSet);
end;

class procedure TCarbonWSCustomListView.ItemInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem);
begin
  if not CheckHandle(ALV, Self, 'ItemInsert') then Exit;

  TCarbonListView(ALV.Handle).InsertItem(AIndex);
end;

class procedure TCarbonWSCustomListView.ItemSetChecked(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean);
begin
  if not CheckHandle(ALV, Self, 'ItemSetChecked') then Exit;

  TCarbonListView(ALV.Handle).SetItemChecked(AIndex, AChecked);
end;

class procedure TCarbonWSCustomListView.ItemSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer);
begin
  if not CheckHandle(ALV, Self, 'ItemSetImage') then Exit;

  TCarbonListView(ALV.Handle).UpdateItem(AIndex);
end;

class procedure TCarbonWSCustomListView.ItemSetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  const AIsSet: Boolean);
begin
  if not CheckHandle(ALV, Self, 'ItemSetState') then Exit;

  TCarbonListView(ALV.Handle).SetItemState(AIndex, AState, AIsSet);
end;

class procedure TCarbonWSCustomListView.ItemSetText(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer;
  const AText: String);
begin
  if not CheckHandle(ALV, Self, 'ItemSetText') then Exit;

  TCarbonListView(ALV.Handle).UpdateItem(AIndex);
end;

class procedure TCarbonWSCustomListView.ItemShow(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
begin
  if not CheckHandle(ALV, Self, 'ItemShow') then Exit;

  TCarbonListView(ALV.Handle).ShowItem(AIndex, PartialOK);
end;

class function TCarbonWSCustomListView.GetBoundingRect(const ALV: TCustomListView): TRect;
begin
  if not CheckHandle(ALV, Self, 'GetBoundingRect') then Exit;

  Result := TCarbonListView(ALV.Handle).GetItemsRect;
end;

class function TCarbonWSCustomListView.GetFocused(const ALV: TCustomListView): Integer;
begin
  Result := -1;
  if not CheckHandle(ALV, Self, 'GetFocused') then Exit;

  Result := TCarbonListView(ALV.Handle).GetItemIndex;
end;

class function TCarbonWSCustomListView.GetItemAt(const ALV: TCustomListView; x,
  y: integer): Integer;
begin
  Result := -1;
  if not CheckHandle(ALV, Self, 'GetItemAt') then Exit;

  Result := TCarbonListView(ALV.Handle).GetItemAt(X, Y);
end;

class function TCarbonWSCustomListView.GetSelCount(const ALV: TCustomListView): Integer;
begin
  Result := 0;
  if not CheckHandle(ALV, Self, 'GetSelCount') then Exit;

  Result := TCarbonListView(ALV.Handle).GetSelCount;
end;

class function TCarbonWSCustomListView.GetTopItem(const ALV: TCustomListView): Integer;
begin
  Result := 0;
  if not CheckHandle(ALV, Self, 'GetTopItem') then Exit;

  Result := TCarbonListView(ALV.Handle).GetTopItem;
end;

class function TCarbonWSCustomListView.GetViewOrigin(const ALV: TCustomListView): TPoint;
begin
  if not CheckHandle(ALV, Self, 'GetViewOrigin') then Exit;

  Result := TCarbonListView(ALV.Handle).GetViewOrigin;
end;

class function TCarbonWSCustomListView.GetVisibleRowCount(const ALV: TCustomListView): Integer;
begin
  Result := 0;
  if not CheckHandle(ALV, Self, 'GetVisibleRowCount') then Exit;

  Result := TCarbonListView(ALV.Handle).GetVisibleRowCount;
end;

class procedure TCarbonWSCustomListView.SetDefaultItemHeight(const ALV: TCustomListView;
  const AValue: Integer);
begin
  if not CheckHandle(ALV, Self, 'SetDefaultItemHeight') then Exit;

  TCarbonListView(ALV.Handle).SetItemsHeight(AValue);
end;

class procedure TCarbonWSCustomListView.SetImageList(const ALV: TCustomListView;
  const AList: TListViewImageList; const AValue: TCustomImageList);
begin
  if not CheckHandle(ALV, Self, 'SetImageList') then Exit;

  TCarbonListView(ALV.Handle).ClearIconCache;
  TCarbonListView(ALV.Handle).UpdateItems;
  TCarbonListView(ALV.Handle).UpdateColumnView;
end;

class procedure TCarbonWSCustomListView.SetItemsCount(const ALV: TCustomListView; const Avalue: Integer);  
begin
  if not CheckHandle(ALV, Self, 'SetOwnerData') then Exit;
  TCarbonListView(ALV.Handle).SetItemsCount(Avalue);
end;

class procedure TCarbonWSCustomListView.SetOwnerData(const ALV: TCustomListView; const AValue: Boolean);  
begin
  if not CheckHandle(ALV, Self, 'SetOwnerData') then Exit;
  TCarbonListView(ALV.Handle).OwnerData := true;
end;

class procedure TCarbonWSCustomListView.SetProperty(const ALV: TCustomListView;
  const AProp: TListViewProperty; const AIsSet: Boolean);
begin
  if not CheckHandle(ALV, Self, 'SetProperty') then Exit;

  // TODO
  case AProp of
  //lvpAutoArrange
    lvpCheckboxes: TCarbonListView(ALV.Handle).ShowCheckboxes(AIsSet);
  //lvpColumnClick
  //lvpFlatScrollBars
  //lvpFullDrag
  //lvpGridLines
  //lvpHideSelection
  //lvpHotTrack
    lvpMultiSelect: TCarbonListView(ALV.Handle).SetSelectionMode(AIsSet, AIsSet);
    lvpOwnerDraw: TCarbonListView(ALV.Handle).SetOwnerDraw(AIsSet);
  //lvpReadOnly
    lvpRowSelect: TCarbonListView(ALV.Handle).SetRowSelect((TListView(ALV).ViewStyle <> vsReport) or AIsSet);
    lvpShowColumnHeaders: TCarbonListView(ALV.Handle).ShowColumnHeaders((TListView(ALV).ViewStyle = vsReport) and AIsSet);
  //lvpShowWorkAreas
  //lvpWrapText
  //lvpToolTips
  end;
end;

class procedure TCarbonWSCustomListView.SetProperties(const ALV: TCustomListView;
  const AProps: TListViewProperties);
var
  AProp: TListViewProperty;
begin
  if not CheckHandle(ALV, Self, 'SetProperties') then Exit;
  
  for AProp := Low(TListViewProperty) to High(TListViewProperty) do
    if AProp in AProps then SetProperty(ALV, AProp, True);
end;

class procedure TCarbonWSCustomListView.SetScrollBars(const ALV: TCustomListView;
  const AValue: TScrollStyle);
begin
  if not CheckHandle(ALV, Self, 'SetScrollBars') then Exit;

  TCarbonListView(ALV.Handle).SetScrollBars(AValue);
end;

class procedure TCarbonWSCustomListView.SetSort(const ALV: TCustomListView;
  const AType: TSortType; const AColumn: Integer;
  const ASortDirection: TSortDirection);
begin
  if not CheckHandle(ALV, Self, 'SetSort') then Exit;
  TCarbonListView(ALV.Handle).UpdateItems;
end;

class procedure TCarbonWSCustomListView.SetViewOrigin(const ALV: TCustomListView;
  const AValue: TPoint);
begin
  if not CheckHandle(ALV, Self, 'SetViewOrigin') then Exit;

  TCarbonListView(ALV.Handle).SetViewOrigin(AValue);
end;

class procedure TCarbonWSCustomListView.SetViewStyle(const ALV: TCustomListView;
  const AValue: TViewStyle);
begin
  if not CheckHandle(ALV, Self, 'SetViewStyle') then Exit;

  TCarbonListView(ALV.Handle).SetViewStyle(AValue);
end;

{ TCarbonWSProgressBar }

{------------------------------------------------------------------------------
  Method:  TCarbonWSProgressBar.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new progress bar in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSProgressBar.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonProgressBar.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSProgressBar.ApplyChanges
  Params:  AProgressBar - LCL custom progress bar

  Sets the parameters (Min, Max, Position) of progress bar in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSProgressBar.ApplyChanges(
  const AProgressBar: TCustomProgressBar);
begin
  if not CheckHandle(AProgressBar, Self, 'ApplyChanges') then Exit;

  TCarbonProgressBar(AProgressBar.Handle).ApplyChanges;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSProgressBar.SetPosition
  Params:  AProgressBar - LCL custom progress bar
           NewPosition  - New position

  Sets the position of progress bar in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSProgressBar.SetPosition(
  const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  if not CheckHandle(AProgressBar, Self, 'SetPosition') then Exit;

  TCarbonCustomBar(AProgressBar.Handle).SetPosition(AProgressBar.Position);
end;

class procedure TCarbonWSProgressBar.SetStyle(const AProgressBar: TCustomProgressBar;
  const AStyle: TProgressBarStyle);
begin
  TCarbonCustomBar(AProgressBar.Handle).SetIndetermine(AStyle = pbstMarquee)
end;

{ TCarbonWSTrackBar }

{------------------------------------------------------------------------------
  Method:  TCarbonWSTrackBar.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new track bar in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSTrackBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonTrackBar.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSTrackBar.ApplyChanges
  Params:  ATrackBar - LCL custom track bar

  Sets the parameters (Min, Max, Position, Ticks) of slider in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
begin
  if not CheckHandle(ATrackBar, Self, 'ApplyChanges') then Exit;

  TCarbonTrackBar(ATrackBar.Handle).ApplyChanges;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSTrackBar.GetPosition
  Params:  ATrackBar - LCL custom track bar
  Returns: Position of sliderr in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar
  ): integer;
begin
  Result := 0;
  if not CheckHandle(ATrackBar, Self, 'GetPosition') then Exit;

  Result := TCarbonTrackBar(ATrackBar.Handle).GetPosition;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSTrackBar.SetPosition
  Params:  ATrackBar - LCL custom track bar
           NewPosition  - New position

  Sets the position of slider in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar;
  const NewPosition: integer);
begin
  if not CheckHandle(ATrackBar, Self, 'SetPosition') then Exit;

  TCarbonTrackBar(ATrackBar.Handle).SetPosition(ATrackBar.Position);
end;

end.
