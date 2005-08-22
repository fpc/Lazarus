{%MainUnit ../comctrls.pp}
{ $Id$

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

{------------------------------------------------------------------------------}
{   TListColumn                                                                }
{------------------------------------------------------------------------------}

procedure TListColumn.Assign(ASource: TPersistent);
var
  Col: TListColumn;
begin
  if ASource is TListColumn 
  then begin
    Col := TListColumn(ASource);
    FAlignment := Col.Alignment;
    FAutoSize := Col.AutoSize;
    FCaption := Col.Caption;
    FMaxWidth := Col.MaxWidth;
    FMinWidth := Col.MinWidth;
    FVisible := Col.Visible;
    FWidth := Col.Width;
    Changed(False);
  end
  else inherited Assign(ASource);
end;

constructor TListColumn.Create(ACollection: TCollection);
begin
  FAlignment := taLeftJustify;
  FCaption := '';
  FWidth := 50;
  FVisible := True;
  FMinWidth := 0;
  FMaxWidth := 0;
  FAutoSize := False;
  FTag := 0;         
  FImageIndex := -1;
  inherited Create(ACollection);
  
  if not IntfUpdateAllowed then Exit;
  IntfCreateColumn;
end;

destructor TListColumn.Destroy;
var
  Columns: TListColumns;
  idx: Integer;
  UpdAllowed: Boolean;
  LV: TCustomListView;
  WSC: TWSCustomListViewClass;
begin
  idx := Index;
  UpdAllowed := IntfUpdateAllowed;
  
  if Collection is TListColumns
  then begin
    Columns:=TListColumns(Collection);
    if Columns.FItemNeedsUpdate=Self then Columns.FItemNeedsUpdate:=nil;
  end;      
  inherited Destroy;

  // MWE: I don't think a changed is needed here
  //Changed(False);  
  
  if not UpdAllowed then Exit;
  
  LV := Columns.FOwner;
  WSC := TWSCustomListViewClass(LV.WidgetSetClass);
  WSC.ColumnDelete(LV, Idx);
end;

procedure TListColumn.IntfCreateColumn;
var
  LV: TCustomListView;
  WSC: TWSCustomListViewClass;
begin
  LV := TListColumns(Collection).FOwner;
  WSC := TWSCustomListViewClass(LV.WidgetSetClass);
  WSC.ColumnInsert(LV, Index, Self);
  WSC.ColumnSetAlignment(LV, Index, Self, FAlignment);
  WSC.ColumnSetAutosize(LV, Index, Self, FAutosize);
  WSC.ColumnSetCaption(LV, Index, Self, FCaption);
  WSC.ColumnSetMaxWidth(LV, Index, Self, FMaxWidth);
  WSC.ColumnSetMinWidth(LV, Index, Self, FMinWidth);
  WSC.ColumnSetWidth(LV, Index, Self, FWidth);
  WSC.ColumnSetImage(LV, Index, Self, FImageIndex);
  WSC.ColumnSetVisible(LV, Index, Self, FVisible);
end;

function TListColumn.IntfUpdateAllowed: Boolean;
begin
  Result := (Collection <> nil)
    and (TListColumns(Collection).FOwner <> nil)
    and TListColumns(Collection).FOwner.HandleAllocated
    and not (csDestroying in TListColumns(Collection).FOwner.ComponentState);
end;

function TListColumn.GetWidth: TWidth;
var
  LV: TCustomListView;
  WSC: TWSCustomListViewClass;
begin
  if IntfUpdateAllowed
  then begin
    LV := TListColumns(Collection).FOwner;
    WSC := TWSCustomListViewClass(LV.WidgetSetClass);
    Result := WSC.ColumnGetWidth(LV, Index, Self);
    if Result < 0 
    then Result := FWidth;
  end
  else Result := FWidth;
end;

procedure TListColumn.SetAlignment(const AValue: TAlignment);
var
  LV: TCustomListView;
  WSC: TWSCustomListViewClass;
begin
  if FAlignment = AValue then Exit;
  FAlignment := AValue;
  Changed(False);  
  if not IntfUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  WSC := TWSCustomListViewClass(LV.WidgetSetClass);
  WSC.ColumnSetAlignment(LV, Index, Self, FAlignment);
end;

procedure TListColumn.SetCaption(const AValue: String);
var
  LV: TCustomListView;
  WSC: TWSCustomListViewClass;
begin
  if AValue = FCaption then Exit;
  FCaption := AValue;
  Changed(False);  
  if not IntfUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  WSC := TWSCustomListViewClass(LV.WidgetSetClass);
  WSC.ColumnSetCaption(LV, Index, Self, FCaption);
end;

procedure TListColumn.SetWidth(const AValue: TWidth);
var
  LV: TCustomListView;
  WSC: TWSCustomListViewClass;
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
  Changed(False);  
  if not IntfUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  WSC := TWSCustomListViewClass(LV.WidgetSetClass);
  WSC.ColumnSetWidth(LV, Index, Self, FWidth);
end;

procedure TListColumn.SetMaxWidth(const AValue: TWidth);
var
  LV: TCustomListView;
  WSC: TWSCustomListViewClass;
begin
  if FMaxWidth = AValue then Exit;
  FMaxWidth := AValue;
  Changed(False);  
  if not IntfUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  WSC := TWSCustomListViewClass(LV.WidgetSetClass);
  WSC.ColumnSetMaxWidth(LV, Index, Self, FMaxWidth);
end;

procedure TListColumn.SetMinWidth(const AValue: TWidth);
var
  LV: TCustomListView;
  WSC: TWSCustomListViewClass;
begin
  if FMinWidth = AValue then Exit;
  FMinWidth := AValue;
  Changed(False);  
  if not IntfUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  WSC := TWSCustomListViewClass(LV.WidgetSetClass);
  WSC.ColumnSetMinWidth(LV, Index, Self, FMinWidth);
end;

procedure TListColumn.SetAutoSize(const AValue: Boolean);
var
  LV: TCustomListView;
  WSC: TWSCustomListViewClass;
begin
  if FAutoSize = AValue then Exit;
  FAutoSize := AValue;
  Changed(False);  
  if not IntfUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  WSC := TWSCustomListViewClass(LV.WidgetSetClass);
  WSC.ColumnSetAutosize(LV, Index, Self, FAutosize);
end;

procedure TListColumn.SetImageIndex(const AValue: TImageIndex);
var
  LV: TCustomListView;
  WSC: TWSCustomListViewClass;
begin
  if FImageIndex = AValue then Exit;
  FImageIndex := AValue;
  Changed(False);
  if not IntfUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  WSC := TWSCustomListViewClass(LV.WidgetSetClass);
  WSC.ColumnSetImage(LV, Index, Self, FImageIndex);
end;

procedure TListColumn.SetIndex(AValue: Integer);
var
  OldIndex: Integer;
  LV: TCustomListView;
  WSC: TWSCustomListViewClass;
begin               
  OldIndex := Index;
  inherited;
                     
  if OldIndex = Index then Exit;
  if not IntfUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  WSC := TWSCustomListViewClass(LV.WidgetSetClass);
  WSC.ColumnMove(LV, OldIndex, Index, Self);
end;

function TListColumn.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TListColumn.SetVisible(const AValue: Boolean);
var
  LV: TCustomListView;
  WSC: TWSCustomListViewClass;
begin
  if FVisible = AValue then Exit;
  FVisible := AValue;
  Changed(False);  
  if not IntfUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  WSC := TWSCustomListViewClass(LV.WidgetSetClass);
  WSC.ColumnSetVisible(LV, Index, Self, FVisible);
end;

{
  $Log$
  Revision 1.10  2005/02/26 17:08:41  marc
  * Reworked listviews to match new interface

  Revision 1.9  2004/07/24 00:00:33  mattias
  started TCollectionPropertyEditor

  Revision 1.8  2004/07/11 17:20:47  marc
  * Implemented most of TListColoum/Item in the Ws for gtk and win32

  Revision 1.7  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.6  2003/12/26 15:23:30  mattias
  started message editor and fixed some range checks

  Revision 1.5  2003/12/21 16:01:58  mattias
  workaround for inherited bug in fpc 1.9

  Revision 1.4  2002/10/09 11:46:04  lazarus
  MG: fixed loading TListView from stream

  Revision 1.3  2002/05/10 06:05:53  lazarus
  MG: changed license to LGPL

  Revision 1.2  2002/03/23 15:49:22  lazarus
  MWE: Fixed more compatebility issues (Sort, SelectedItem)

  Revision 1.1  2002/03/12 23:55:37  lazarus
  MWE:
    * More delphi compatibility added/updated to TListView
    * Introduced TDebugger.locals
    * Moved breakpoints dialog to debugger dir
    * Changed breakpoints dialog to read from resource

}

