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
  Classes, Windows,
  // LCL
  ComCtrls, LCLType, Controls, Graphics,
  LCLProc,
  // widgetset
  WSComCtrls, WSLCLClasses, WSProc;

type

  { TWin32WSStatusBar }

  TWin32WSStatusBar = class(TWSStatusBar)
  private
  protected
  public
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
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); override;
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
  end;

  { TWin32WSTrackBar }

  TWin32WSTrackBar = class(TWSTrackBar)
  private
  protected
  public
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

procedure TWin32WSCustomListView.ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); 
begin
  if not WSCheckHandleAllocated(ALV, 'ItemShow')
  then Exit;  
  
  ListView_EnsureVisible(ALV.Handle, AIndex, 1);
end;



initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomStatusBar, TWin32WSStatusBar);
//  RegisterWSComponent(TCustomTabSheet, TWin32WSTabSheet);
//  RegisterWSComponent(TCustomPageControl, TWin32WSPageControl);
  RegisterWSComponent(TCustomListView, TWin32WSCustomListView);
//  RegisterWSComponent(TCustomListView, TWin32WSListView);
//  RegisterWSComponent(TCustomProgressBar, TWin32WSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TWin32WSCustomUpDown);
//  RegisterWSComponent(TCustomUpDown, TWin32WSUpDown);
//  RegisterWSComponent(TCustomToolButton, TWin32WSToolButton);
//  RegisterWSComponent(TCustomToolBar, TWin32WSToolBar);
//  RegisterWSComponent(TCustomToolButton, TWin32WSToolButton);
//  RegisterWSComponent(TCustomToolBar, TWin32WSToolBar);
//  RegisterWSComponent(TCustomTrackBar, TWin32WSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TWin32WSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TWin32WSTreeView);
////////////////////////////////////////////////////
end.
