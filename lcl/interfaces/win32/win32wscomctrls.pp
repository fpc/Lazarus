{ $Id$}
{
 *****************************************************************************
 *                            Win32WSComCtrls.pp                             * 
 *                            ------------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Win32WSComCtrls;

{$mode objfpc}{$H+}

interface

uses        
  // FCL
  Classes, Windows, SysUtils, WinExt,
  // LCL
  ComCtrls, LCLType, Controls, Graphics,
  LCLProc, InterfaceBase, Win32Int,
  // widgetset
  WSComCtrls, WSLCLClasses, WSProc,
  // win32 widgetset
  Win32WSControls;

type

  { TWin32WSStatusBar }

  TWin32WSStatusBar = class(TWSStatusBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure Update(const AStatusBar: TStatusBar); override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
  end;

  { TWin32WSTabSheet }

  TWin32WSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TWin32WSPageControl }

  TWin32WSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TWin32WSCustomListView }

  TWin32WSCustomListView = class(TWSCustomListView)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer; override;
    class procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment); override;
    class procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean); override;
    class procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String); override;
    class procedure ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer); override;
    class procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer); override;
    class procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer); override;
    class procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer); override;
    class procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean); override;

    class procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; var AIsSet: Boolean): Boolean; override; // returns True if supported
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); override;
    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); override;
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); override;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean); override;
  
    class procedure UpdateProperties(const ACustomListView: TCustomListView); override;
  end;

  { TWin32WSListView }

  TWin32WSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TWin32WSProgressBar }

  TWin32WSProgressBar = class(TWSProgressBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
  end;

  { TWin32WSCustomUpDown }

  TWin32WSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TWin32WSUpDown }

  TWin32WSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TWin32WSToolButton }

  TWin32WSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TWin32WSToolBar }

  TWin32WSToolBar = class(TWSToolBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
{$ifdef OldToolbar}  
    class function  GetButtonCount(const AToolBar: TToolBar): integer; override;
    class procedure InsertToolButton(const AToolBar: TToolbar; const AControl: TControl); override;
    class procedure DeleteToolButton(const AToolBar: TToolbar; const AControl: TControl); override;
{$endif}    
  end;

  { TWin32WSTrackBar }

  TWin32WSTrackBar = class(TWSTrackBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); override;
  end;

  { TWin32WSCustomTreeView }

  TWin32WSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TWin32WSTreeView }

  TWin32WSTreeView = class(TWSTreeView)
  private
  protected
  public
  end;


implementation

{ --- Helper routines for TWin32WSStatusBar --- }

{------------------------------------------------------------------------------
  Method: UpdateStatusBarPanel
  Params: StatusPanel - StatusPanel which needs to be update
  Returns: Nothing

  Called by StatusBarPanelUpdate and StatusBarSetText
  Everything is updated except the panel width
 ------------------------------------------------------------------------------}
procedure UpdateStatusBarPanel(const StatusPanel: TStatusPanel);
var
  BevelType: integer;
  Text: string;
begin
  Text := StatusPanel.Text;
  case StatusPanel.Alignment of
    taCenter: Text := #9 + Text;
    taRightJustify: Text := #9#9 + Text;
  end;
  case StatusPanel.Bevel of
    pbNone: BevelType := Windows.SBT_NOBORDERS;
    pbLowered: BevelType := 0;
    pbRaised: BevelType := Windows.SBT_POPOUT;
  end;
  Windows.SendMessage(StatusPanel.StatusBar.Handle, SB_SETTEXT, StatusPanel.Index or BevelType, LPARAM(PChar(Text)));
end;

procedure UpdateStatusBarPanelWidths(const StatusBar: TStatusBar);
var
  Rights: PInteger;
  PanelIndex: integer;
  CurrentRight: integer;
begin
  if StatusBar.Panels.Count=0 then begin
    Windows.SendMessage(StatusBar.Handle, SB_SETPARTS, 0, 0);
    exit;
  end;
  Getmem(Rights, StatusBar.Panels.Count * sizeof(integer));
  try
    CurrentRight := 0;
    for PanelIndex := 0 to StatusBar.Panels.Count-2 do begin
      CurrentRight := CurrentRight + StatusBar.Panels[PanelIndex].Width;
      Rights[PanelIndex] := CurrentRight;
    end;
    Rights[StatusBar.Panels.Count-1] := -1; //Last extends to end;
    Windows.SendMessage(StatusBar.Handle, SB_SETPARTS, StatusBar.Panels.Count, LPARAM(Rights));
  finally
    Freemem(Rights);
  end;
end;

{ TWin32WSStatusBar }

function TWin32WSStatusBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := STATUSCLASSNAME;
    WindowTitle := StrCaption;
    Left := LongInt(CW_USEDEFAULT);
    Top := LongInt(CW_USEDEFAULT);
    Width := LongInt(CW_USEDEFAULT);
    Height := LongInt(CW_USEDEFAULT);
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // need to set handle for Update method
  AWinControl.Handle := Params.Window;
  Update(TStatusBar(AWinControl));
  Result := Params.Window;
end;

procedure TWin32WSStatusBar.PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer);
begin
  UpdateStatusBarPanelWidths(AStatusBar);
  UpdateStatusBarPanel(AStatusBar.Panels[PanelIndex]);
end;

procedure TWin32WSStatusBar.SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
begin
  if AStatusBar.SimplePanel then
    Windows.SendMessage(AStatusBar.Handle, SB_SETTEXT, 255, LPARAM(PChar(AStatusBar.SimpleText)))
  else
    UpdateStatusBarPanel(AStatusBar.Panels[PanelIndex]);
end;

procedure TWin32WSStatusBar.Update(const AStatusBar: TStatusBar);
var
  PanelIndex: integer;
begin
  Windows.SendMessage(AStatusBar.Handle, SB_SIMPLE, WPARAM(AStatusBar.SimplePanel), 0);
  if AStatusBar.SimplePanel then
    SetPanelText(AStatusBar, 0)
  else begin
    UpdateStatusBarPanelWidths(AStatusBar);
    for PanelIndex := 0 to AStatusBar.Panels.Count-1 do
      UpdateStatusBarPanel(AStatusBar.Panels[PanelIndex]);
  end;
end;

{ TWin32WSCustomListView } 
     
const                        
  // TODO: move to windows unit     
  LVCFMT_JUSTIFYMASK = LVCFMT_LEFT or LVCFMT_RIGHT or LVCFMT_CENTER;
  LVCF_IMAGE = $0010;
  LVCF_ORDER = $0020;            
  
  LVM_GETHEADER = $1000 + 31;
 
type
  // TODO: add iImage and iOrder to exiting TLvColumn
  // this is a hack !!!
  TLvColumn_v4_7 = record
    lvc: TLvColumn;
    iImage: Integer;
    iOrder: Integer;
  end;

function TWin32WSCustomListView.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
const  
  LISTVIEWSTYLES: array[TViewStyle] of DWORD = (LVS_LIST, LVS_REPORT);
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := WC_LISTVIEW;
    WindowTitle := StrCaption;
    Flags := Flags or LISTVIEWSTYLES[TListView(AWinControl).ViewStyle] or LVS_SINGLESEL;
    FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;   
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

procedure TWin32WSCustomListView.ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); 
var
  H: THandle;
  Count: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnDelete')
  then Exit;            
  
  // Move column to the last, otherwise out items get shuffeled
  
  //  H := ListView_GetHeader(Handle);
  H := SendMessage(ALV.Handle, LVM_GETHEADER, 0, 0);
  Count := Header_GetItemCount(H);
  if Count <= Aindex then Exit;
  
  ColumnMove(ALV, AIndex, Count - 1, nil);
  ListView_DeleteColumn(ALV.Handle, Count - 1);
end;

function TWin32WSCustomListView.ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer;
var
  lvc: TLvColumn;
begin
  Result := -1;                     
  // this implementation uses columnwidht = 0 for invisible
  // so fallback to default (= AColumn.FWidth)
  // Don't return AColumn.Width, this will cause a loop
  if not AColumn.Visible then Exit; 

  if not WSCheckHandleAllocated(ALV, 'ColumnGetSize')
  then Exit;    
  
  // dont use ListView_GetColumnWidth since we cant detect errors
  lvc.Mask := LVCF_WIDTH;
  if ListView_GetColumn(ALV.Handle, AIndex, lvc) <> 0
  then Result := lvc.cx;
end;

procedure TWin32WSCustomListView.ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); 
var
  lvc: TLvColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnInsert')
  then Exit;
  
  lvc.Mask := LVCF_TEXT;
  lvc.pszText := PChar(AColumn.Caption);
  
  ListView_InsertColumn(ALV.Handle, AIndex, lvc);
end;

procedure TWin32WSCustomListView.ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
var
  lvc, oldlvc: TLvColumn_v4_7;
  buf, oldbuf: array[0..1024] of Char; 
  Count, idx: Integer;      
  
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnMove')
  then Exit;

  Count := AOldIndex - ANewIndex;
  
  // Fetch old column values
  oldlvc.lvc.Mask := LVCF_FMT or LVCF_IMAGE or LVCF_TEXT or LVCF_WIDTH;
  oldlvc.lvc.pszText := @oldbuf;
  oldlvc.lvc.cchTextMax := SizeOF(oldbuf);
  ListView_GetColumn(ALV.Handle, AOldIndex, oldlvc.lvc);
  
  idx := AOldIndex;
  while Count <> 0 do
  begin
    // get next index 
    if Count < 0
    then Inc(idx)
    else Dec(idx);
    // and data
    lvc.lvc.Mask := LVCF_FMT or LVCF_IMAGE or LVCF_TEXT or LVCF_WIDTH;
    lvc.lvc.pszText := @buf;
    lvc.lvc.cchTextMax := SizeOF(buf);
    ListView_GetColumn(ALV.Handle, idx, lvc.lvc);
    // set data
    ListView_SetColumn(ALV.Handle, ANewIndex + Count, lvc.lvc);
    
    if Count < 0
    then Inc(Count)
    else Dec(Count);
  end;
  // finally copy original data to new column
  ListView_SetColumn(ALV.Handle, ANewIndex, oldlvc.lvc);
end;

procedure TWin32WSCustomListView.ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment);
const
  JUSTIFICATION: array[TAlignment] of Integer = (
    LVCFMT_LEFT,
    LVCFMT_RIGHT,
    LVCFMT_CENTER
  );
var
  lvc: TLvColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAlignment')
  then Exit;
  
  lvc.Mask := LVCF_FMT;
  ListView_GetColumn(ALV.Handle, AIndex, lvc);
  lvc.fmt := (lvc.fmt and not LVCFMT_JUSTIFYMASK) or JUSTIFICATION[AAlignment];
  ListView_SetColumn(ALV.Handle, AIndex, lvc);
end;

procedure TWin32WSCustomListView.ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAutoSize')
  then Exit;

  if AAutoSize
  then ListView_SetColumnWidth(ALV.Handle, AIndex, LVSCW_AUTOSIZE)
  else ListView_SetColumnWidth(ALV.Handle, AIndex, AColumn.Width);
end;

procedure TWin32WSCustomListView.ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String);
var
  lvc: TLvColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetCaption')
  then Exit;
  
  lvc.Mask := LVCF_TEXT;
  lvc.pszText := PChar(ACaption);
  
  ListView_SetColumn(ALV.Handle, AIndex, lvc);
end;

procedure TWin32WSCustomListView.ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer);
var
  lvc: TLvColumn_v4_7;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetImage')
  then Exit;
  
  lvc.lvc.Mask := LVCF_IMAGE;
  lvc.iImage := AImageIndex;
  
  ListView_SetColumn(ALV.Handle, AIndex, lvc.lvc);
end;

procedure TWin32WSCustomListView.ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer);
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetMaxWidth')
  then Exit;                                             
  
  // TODO: in messageHandler
end;

procedure TWin32WSCustomListView.ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetMinWidth')
  then Exit;      
  
  // TODO: in messageHandler
end;

procedure TWin32WSCustomListView.ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetWidth')
  then Exit;

  ListView_SetColumnWidth(ALV.Handle, AIndex, AWidth)
end;

procedure TWin32WSCustomListView.ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetVisible')
  then Exit;
                        
  // TODO: implement with LV_COLUMN.subitem (associate different columns and insert/delete last.
                        
  if AVisible
  then ListView_SetColumnWidth(ALV.Handle, AIndex, AColumn.Width)
  else ListView_SetColumnWidth(ALV.Handle, AIndex, 0);
end;

procedure TWin32WSCustomListView.ItemDelete(const ALV: TCustomListView; const AIndex: Integer);
begin
  if not WSCheckHandleAllocated(ALV, 'ItemDelete')
  then Exit;             
  
  ListView_DeleteItem(ALV.Handle, AIndex);
end;

function TWin32WSCustomListView.ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; var AIsSet: Boolean): Boolean; 
const  
  // lisCut, lisDropTarget, lisFocused, lisSelected
  FLAGS: array[TListItemState] of Integer = (LVIS_CUT, LVIS_DROPHILITED, LVIS_FOCUSED, LVIS_SELECTED);
begin
  Result := False;
  
  if not WSCheckHandleAllocated(ALV, 'ItemGetState')
  then Exit;
  
  AIsSet := 0 <> ListView_GetItemState(ALV.Handle, AIndex, FLAGS[AState]);
  Result := True;
end;
  
procedure TWin32WSCustomListView.ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem);
var
  lvi: TLvItem;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemInsert')
  then Exit;

  lvi.Mask := LVIF_TEXT;
  lvi.iItem := AIndex;
  lvi.iSubItem := 0;
  lvi.pszText := PChar(AItem.Caption);

  ListView_InsertItem(ALV.Handle, lvi);
end;

procedure TWin32WSCustomListView.ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer);
var
  lvi: TLvItem;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetImage')
  then Exit;

  lvi.Mask := LVIF_IMAGE;
  lvi.iItem := AIndex;
  lvi.iSubItem := ASubIndex;
  lvi.iImage := AImageIndex;

  ListView_SetItem(ALV.Handle, lvi);
end;

procedure TWin32WSCustomListView.ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); 
const  
  // lisCut, lisDropTarget, lisFocused, lisSelected
  FLAGS: array[TListItemState] of Integer = (LVIS_CUT, LVIS_DROPHILITED, LVIS_FOCUSED, LVIS_SELECTED);
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetState')
  then Exit;

  if AIsSet
  then ListView_SetItemState(ALV.Handle, AIndex, FLAGS[AState], FLAGS[AState])
  else ListView_SetItemState(ALV.Handle, AIndex, 0, FLAGS[AState]);
end;

procedure TWin32WSCustomListView.ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String);
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetText')
  then Exit;
  
  ListView_SetItemText(ALV.Handle, AIndex, ASubIndex, PChar(AText));
end;

procedure TWin32WSCustomListView.ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
begin
  if not WSCheckHandleAllocated(ALV, 'ItemShow')
  then Exit;  
  
  ListView_EnsureVisible(ALV.Handle, AIndex, Ord(PartialOK));
end;

procedure TWin32WSCustomListView.UpdateProperties(const ACustomListView: TCustomListView);
const
  LVS_TYPEMASK = LVS_LIST or LVS_REPORT or LVS_SMALLICON or LVS_ICON;
  LISTVIEWSTYLES: array[TViewStyle] of DWORD = (
    LVS_LIST, LVS_REPORT);
var
  I, Count: Integer;
  LVC: LV_COLUMN;
  Style: dword;
  H: THandle;
begin
  with TListView(ACustomListView) do
  begin
    Style := dword(GetWindowLong(Handle, GWL_STYLE));
    if (Style and LVS_TYPEMASK) <> LISTVIEWSTYLES[ViewStyle]
    then begin
      Style := Style and not LVS_TYPEMASK or LISTVIEWSTYLES[ViewStyle];
      SetWindowLong(Handle, GWL_STYLE, Style);
    end;
    
    if ViewStyle = vsReport then
    begin 
//        H := ListView_GetHeader(Handle);
      H := SendMessage(Handle, $1000 + 31 {LVM_GETHEADER}, 0, 0);
      Count := Header_GetItemCount(H);
      
      for I := 0 to Columns.Count - 1 do
      begin
        with LVC do
        begin
          Mask := LVCF_FMT or LVCF_TEXT or LVCF_WIDTH;
          Fmt := Integer(Columns.Items[I].Alignment);
          CX := Columns.Items[I].Width;
          PSzText := PChar(Columns.Items[I].Caption);
        end;
        if i >= Count 
        then ListView_InsertColumn(Handle, i, lvc)
        else ListView_SetColumn(Handle, I, LVC);
      end;
      for i := Columns.Count to Count - 1 do
        ListView_DeleteColumn(Handle, i);  
      Count := Header_GetItemCount(H);
    end;
    //If Sorted Then
      //ListView_SortItems(Handle, @CompareFunc, 0);
    if MultiSelect then
      SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not LVS_SINGLESEL);
    if SmallImages <> Nil then
      ListView_SetImageList(Handle, SmallImages.Handle, LVSIL_NORMAL);
  end;
end;

{ TWin32WSProgressBar }

function TWin32WSProgressBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    with TCustomProgressBar(AWinControl) do
    begin
      if Smooth then
        Flags := Flags or PBS_SMOOTH;
      if (Orientation = pbVertical) or (Orientation = pbTopDown) then
        Flags := Flags or PBS_VERTICAL;
    end;
    pClassName := PROGRESS_CLASS;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

procedure TWin32WSProgressBar.ApplyChanges(
  const AProgressBar: TCustomProgressBar);
begin
  with AProgressBar do
  begin
    { smooth and vertical need window recreation }
    if ((GetWindowLong(Handle, GWL_STYLE) and PBS_SMOOTH  ) <>
         Integer(Smooth) * PBS_SMOOTH) or
       ((GetWindowLong(Handle, GWL_STYLE) and PBS_VERTICAL) <>
         Integer((Orientation = pbVertical) or (Orientation = pbTopDown)) * PBS_VERTICAL) then
      TWin32WidgetSet(InterfaceObject).RecreateWnd(AProgressBar);

    SendMessage(Handle, PBM_SETRANGE, 0, MakeLParam(Min, Max));
    SendMessage(Handle, PBM_SETPOS, Position, 0);

{ TODO: Implementable?
    If BarShowText Then
    Begin
      SetWindowText(Handle, StrToPChar((Sender As TControl).Caption));
    End
    Else
      SetWindowText(Handle, Nil);
}
  end;
end;

procedure TWin32WSProgressBar.SetPosition(
  const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  Windows.SendMessage(AProgressBar.Handle, PBM_SETPOS, Windows.WPARAM(NewPosition), 0);
end;

{ TWin32WSToolbar}

function TWin32WSToolBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := TOOLBARCLASSNAME;
    Flags := Flags or CCS_ADJUSTABLE;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

{$ifdef OldToolbar}

function  TWin32WSToolbar.GetButtonCount(const AToolBar: TToolBar): integer;
begin
  Result := SendMessage(AToolbar.Handle, TB_BUTTONCOUNT, 0, 0)
end;

procedure TWin32WSToolbar.InsertToolButton(const AToolBar: TToolbar; const AControl: TControl);
var
  PStr, PStr2: PChar;
  Num: Integer;
  TBB: TBBUTTON;
begin
  // TODO: check correctness / clean up
  Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!');
  Assert(False, 'Trace:Toolbutton being inserted');
  Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!');
  If (AControl is TWinControl) Then
  Begin
    PStr := StrAlloc(Length(TToolButton(AControl).Caption) + 1);
    StrPCopy(PStr, TToolButton(AControl).Caption);
    PStr2 := StrAlloc(Length(TControl(AControl).Hint) + 1);
    StrPCopy(PStr2, TControl(AControl).Hint);
  End
  Else
  Begin
    Raise Exception.Create('Can not assign this control to the toolbar');
    Exit;
  End;

  Num := TToolbar(TWinControl(AControl).Parent).Buttonlist.IndexOf(TControl(AControl));
  If Num < 0 Then
    Num := TToolbar(TWinControl(AControl).Parent).Buttonlist.Count + 1;
  Assert(False, Format('Trace:Num = %d in LM_INSERTTOOLBUTTON', [Num]));

  With tbb Do
  Begin
    iBitmap := Num;
    idCommand := Num;
    fsState := TBSTATE_ENABLED;
    fsStyle := TBSTYLE_BUTTON;
    iString := Integer(PStr);
  End;

  SendMessage(TWinControl(AControl).Parent.Handle, TB_BUTTONSTRUCTSIZE, SizeOf(TBBUTTON), 0);
  SendMessage(TWinControl(AControl).Parent.Handle, TB_ADDBUTTONS, 1, LParam(LPTBButton(@tbb)));
  StrDispose(pStr);
  StrDispose(pStr2);
  Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!');
end;

procedure TWin32WSToolbar.DeleteToolButton(const AToolBar: TToolbar; const AControl: TControl);
begin
  // TODO: code buggy, Index of button to delete ?!
  SendMessage(AToolBar.Handle, TB_DELETEBUTTON, 0, 0);
end;

{$endif}

{ TWin32WSTrackBar }

function TWin32WSTrackBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := TRACKBAR_CLASS;
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

procedure TWin32WSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  wHandle: HWND;
begin
  with ATrackBar do
  begin
    { cache handle }
    wHandle := Handle;
    Windows.SendMessage(wHandle, TBM_SETRANGEMAX, Windows.WPARAM(true), Max);
    Windows.SendMessage(wHandle, TBM_SETRANGEMIN, Windows.WPARAM(true), Min);
    Windows.SendMessage(wHandle, TBM_SETPOS, Windows.WPARAM(true), Position);
    Windows.SendMessage(wHandle, TBM_SETLINESIZE, 0, LineSize);
    Windows.SendMessage(wHandle, TBM_SETPAGESIZE, 0, PageSize);
    case Orientation of
      trVertical:
        SetWindowLong(wHandle, GWL_STYLE, GetWindowLong(wHandle, GWL_STYLE) or TBS_VERT);
      trHorizontal:
        SetWindowLong(wHandle, GWL_STYLE, GetWindowLong(wHandle, GWL_STYLE) or TBS_HORZ);
    end;
    if ShowScale then
    begin
      case ScalePos of
        trLeft:
          SetWindowLong(wHandle, GWL_STYLE, GetWindowLong(wHandle, GWL_STYLE) or TBS_LEFT or TBS_VERT);
        trRight:
          SetWindowLong(wHandle, GWL_STYLE, GetWindowLong(wHandle, GWL_STYLE) or TBS_RIGHT or TBS_VERT);
        trTop:
          SetWindowLong(wHandle, GWL_STYLE, GetWindowLong(wHandle, GWL_STYLE) or TBS_TOP or TBS_HORZ);
        trBottom:
          SetWindowLong(wHandle, GWL_STYLE, GetWindowLong(wHandle, GWL_STYLE) or TBS_BOTTOM or TBS_HORZ);
      end;
    end;
  end;
end;

function  TWin32WSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar): integer;
begin
  Result := SendMessage(ATrackBar.Handle, TBM_GETPOS, 0, 0)
end;

procedure TWin32WSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer);
begin
  Windows.SendMessage(ATrackBar.Handle, TBM_SETPOS, Windows.WPARAM(true), Windows.LPARAM(NewPosition));
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TStatusBar, TWin32WSStatusBar);
//  RegisterWSComponent(TCustomTabSheet, TWin32WSTabSheet);
//  RegisterWSComponent(TCustomPageControl, TWin32WSPageControl);
  RegisterWSComponent(TCustomListView, TWin32WSCustomListView);
//  RegisterWSComponent(TCustomListView, TWin32WSListView);
  RegisterWSComponent(TCustomProgressBar, TWin32WSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TWin32WSCustomUpDown);
//  RegisterWSComponent(TCustomUpDown, TWin32WSUpDown);
//  RegisterWSComponent(TCustomToolButton, TWin32WSToolButton);
  RegisterWSComponent(TToolBar, TWin32WSToolBar);
  RegisterWSComponent(TCustomTrackBar, TWin32WSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TWin32WSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TWin32WSTreeView);
////////////////////////////////////////////////////
end.
