{%MainUnit ../comctrls.pp}
{ $Id$

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
  
  if not WSUpdateAllowed then Exit;
  WSCreateColumn;
end;

destructor TListColumn.Destroy;
var
  Columns: TListColumns;
  idx: Integer;
  UpdAllowed: Boolean;
  LV: TCustomListView;
begin
  idx := Index;
  UpdAllowed := WSUpdateAllowed;
  
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
  TWSCustomListViewClass(LV.WidgetSetClass).ColumnDelete(LV, Idx);
end;

procedure TListColumn.WSCreateColumn;
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
  if FAutoSize then begin
    WSC.ColumnSetAutosize(LV, Index, Self, FAutosize);
  end else begin
    WSC.ColumnSetWidth(LV, Index, Self, FWidth);
  end;
  WSC.ColumnSetImage(LV, Index, Self, FImageIndex);
  WSC.ColumnSetVisible(LV, Index, Self, FVisible);
  if FSortIndicator<>siNone then
    WSC.ColumnSetSortIndicator(LV, Index, Self, FSortIndicator);
end;

procedure TListColumn.WSDestroyColumn;
var
  LV: TCustomListView;
begin
  LV := TListColumns(Collection).FOwner;
  TWSCustomListViewClass(LV.WidgetSetClass).ColumnDelete(LV, Index);
end;

function TListColumn.WSUpdateAllowed: Boolean;
begin
  Result := (Collection <> nil)
    and (TListColumns(Collection).FOwner <> nil)
    and TListColumns(Collection).FOwner.HandleAllocated
    and not (csReading in TListColumns(Collection).FOwner.ComponentState)
    and not (csDestroying in TListColumns(Collection).FOwner.ComponentState);
end;

function TListColumn.WSReadAllowed: Boolean;
begin
  Result := (Collection <> nil)
    and (TListColumns(Collection).FOwner <> nil)
    and TListColumns(Collection).FOwner.HandleAllocated
    and ([csReading, csLoading] * TListColumns(Collection).FOwner.ComponentState = [])
    and not (wcfInitializing in TCustomListView(TListColumns(Collection).FOwner).FWinControlFlags);
end;

function TListColumn.GetWidth: TWidth;
var
  LV: TCustomListView;
  WSWidth: integer;
begin
  if WSReadAllowed
  then begin
    LV := TListColumns(Collection).FOwner;
    WSWidth := TWSCustomListViewClass(LV.WidgetSetClass).ColumnGetWidth(LV, Index, Self);
    // update stored width
    if WSWidth >= 0 then
      FWidth := WSWidth;
  end;
  Result := FWidth;
end;

procedure TListColumn.SetSortIndicator(AValue: TSortIndicator);
var
  LV: TCustomListView;
begin
  if FSortIndicator = AValue then Exit;
  FSortIndicator := AValue;
  Changed(False);
  if not WSUpdateAllowed then Exit;

  LV := TListColumns(Collection).FOwner;
  TWSCustomListViewClass(LV.WidgetSetClass).ColumnSetSortIndicator(LV, Index, Self, FSortIndicator);
end;

procedure TListColumn.SetAlignment(const AValue: TAlignment);
var
  LV: TCustomListView;
begin
  if FAlignment = AValue then Exit;
  FAlignment := AValue;
  Changed(False);  
  if not WSUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  TWSCustomListViewClass(LV.WidgetSetClass).ColumnSetAlignment(LV, Index, Self, FAlignment);
end;

procedure TListColumn.SetCaption(const AValue: TTranslateString);
var
  LV: TCustomListView;
begin
  if AValue = FCaption then Exit;
  FCaption := AValue;
  Changed(False);  
  if not WSUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  TWSCustomListViewClass(LV.WidgetSetClass).ColumnSetCaption(LV, Index, Self, FCaption);
  if FAutoSize then
    TWSCustomListViewClass(LV.WidgetSetClass).ColumnSetAutosize(LV, Index, Self, FAutosize);
end;

procedure TListColumn.SetWidth(const AValue: TWidth);
var
  LV: TCustomListView;
  W: TWidth;
begin
  W := AValue;
  if (MinWidth > 0) and (W < MinWidth) then
    W := MinWidth
  else
  if (MaxWidth > 0) and (W > MaxWidth) then
    W := MaxWidth;
  if Width = W then Exit; // compare with Width instead of FWidth - FWidth is not updated from the WS automatically
  FWidth := W;
  Changed(False);
  if not WSUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  if (LV <> nil) or (LV.WidgetsetClass <> nil) then // Avoids crash
    TWSCustomListViewClass(LV.WidgetSetClass).ColumnSetWidth(LV, Index, Self, FWidth);
end;

procedure TListColumn.SetMaxWidth(const AValue: TWidth);
var
  LV: TCustomListView;
begin
  if FMaxWidth = AValue then Exit;
  FMaxWidth := AValue;
  Changed(False);  
  if not WSUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  TWSCustomListViewClass(LV.WidgetSetClass).ColumnSetMaxWidth(LV, Index, Self, FMaxWidth);
end;

procedure TListColumn.SetMinWidth(const AValue: TWidth);
var
  LV: TCustomListView;
begin
  if FMinWidth = AValue then Exit;
  FMinWidth := AValue;
  Changed(False);  
  if not WSUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  TWSCustomListViewClass(LV.WidgetSetClass).ColumnSetMinWidth(LV, Index, Self, FMinWidth);
end;

procedure TListColumn.SetAutoSize(const AValue: Boolean);
var
  LV: TCustomListView;
begin
  if FAutoSize = AValue then Exit;
  FAutoSize := AValue;
  Changed(False);  
  if not WSUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  TWSCustomListViewClass(LV.WidgetSetClass).ColumnSetAutosize(LV, Index, Self, FAutosize);
end;

procedure TListColumn.SetImageIndex(const AValue: TImageIndex);
var
  LV: TCustomListView;
begin
  if FImageIndex = AValue then Exit;
  FImageIndex := AValue;
  Changed(False);
  if not WSUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  TWSCustomListViewClass(LV.WidgetSetClass).ColumnSetImage(LV, Index, Self, FImageIndex);
end;

procedure TListColumn.SetIndex(AValue: Integer);
var
  OldIndex: Integer;
  LV: TCustomListView;
begin
  OldIndex := Index;
  inherited;
                     
  if OldIndex = Index then Exit;
  if not WSUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  TWSCustomListViewClass(LV.WidgetSetClass).ColumnMove(LV, OldIndex, Index, Self);
end;

function TListColumn.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then Result := inherited GetDisplayName;
end;

function TListColumn.GetStoredWidth: Integer;
begin
  Result := FWidth;
end;

procedure TListColumn.SetVisible(const AValue: Boolean);
var
  LV: TCustomListView;
begin
  if FVisible = AValue then Exit;
  FVisible := AValue;
  Changed(False);  
  if not WSUpdateAllowed then Exit;
  
  LV := TListColumns(Collection).FOwner;
  TWSCustomListViewClass(LV.WidgetSetClass).ColumnSetVisible(LV, Index, Self, FVisible);
end;

