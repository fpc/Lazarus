{%MainUnit ../dbctrls.pp}

{******************************************************************************
                                     TDBListBox
                    data aware ListBox, base found in dbctrls.pp
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

// included by dbctrls.pp
{ TDBLookupListBox }

constructor TDBLookupListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLookup:= TDBLookup.Create(Self);
  FDataLink.OnActiveChange:= @ActiveChange;
end;

procedure TDBLookupListBox.UpdateData(Sender: TObject);
begin
  if (ItemIndex < 0) then
    Exit;
  FLookup.UpdateData(ItemIndex, FScrollListDataset);
end;

function TDBLookupListBox.IsUnbound: boolean;
begin
  Result := (FDataLink.DataSource = nil) or (DataField = '');
end;

procedure TDBLookupListBox.ActiveChange(Sender: TObject);
begin
  if FDataLink.Active then
    UpdateLookup;
end;

procedure TDBLookupListBox.DataChange(Sender: TObject);
begin
  if FDatalink.Active then
    ItemIndex := FLookup.GetKeyIndex
  else
    ItemIndex := -1;
end;

procedure TDBLookupListBox.DestroyWnd;
begin
  inherited;
  //after handle destroy Items address changes
  FLookup.ControlItems := Items;
end;

procedure TDBLookupListBox.DoSelectionChange(User: Boolean);
begin
  if User then
    if IsUnbound then
      UpdateData(Self)
    else
    begin
      if FDataLink.CanModify then
      begin
        FDataLink.Modified;
        FDataLink.UpdateRecord;
      end
      else
        DataChange(Self);
    end;
  inherited DoSelectionChange(User);
end;

procedure TDBLookupListBox.InitializeWnd;
begin
  inherited InitializeWnd;
  //after handle allocation Items address changes
  FLookup.ControlItems := Items;
end;

procedure TDBLookupListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FLookup.HandleNullKey(Key, Shift) then
  begin
    //clear selection
    if not FDataLink.Active then
      GetItemIndex; // a list-only dblookuplist do not automatically update itemindex
    ItemIndex := -1;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TDBLookupListBox.Loaded;
begin
  inherited Loaded;
  UpdateLookup;
end;

function TDBLookupListBox.GetKeyField: string;
begin
  Result := FLookup.KeyField;
end;

function TDBLookupListBox.GetKeyValue: Variant;
begin
  Result := FLookup.GetKeyValue(ItemIndex);
end;

function TDBLookupListBox.GetListField: string;
begin
  Result := FLookup.ListField;
end;

function TDBLookupListBox.GetListFieldIndex: Integer;
begin
  Result := FLookup.ListFieldIndex;
end;

function TDBLookupListBox.GetListSource: TDataSource;
begin
  Result := FLookup.ListSource;
end;

function TDBLookupListBox.GetLookupCache: boolean;
begin
  Result := FLookup.LookupCache;
end;

function TDBLookupListBox.GetNullValueKey: TShortCut;
begin
  Result := FLookup.NullValueKey;
end;

procedure TDBLookupListBox.SetKeyField(const Value: string);
begin
  FLookup.KeyField:= Value;
  UpdateLookup;
end;

procedure TDBLookupListBox.SetKeyValue(const AValue: Variant);
begin
  ItemIndex := FLookup.GetKeyIndex(AValue);
end;

procedure TDBLookupListBox.SetListField(const Value: string);
begin
  FLookup.ListField:= Value;
  UpdateLookup;
end;

procedure TDBLookupListBox.SetListFieldIndex(const Value: Integer);
begin
  FLookup.ListFieldIndex:= Value;
  UpdateLookup;
end;

procedure TDBLookupListBox.SetListSource(const Value: TDataSource);
begin
  FLookup.ListSource:= Value;
  UpdateLookup;
end;

procedure TDBLookupListBox.SetLookupCache(const Value: boolean);
begin
  FLookup.LookupCache := Value;
  UpdateLookup;
end;

procedure TDBLookupListBox.SetNullValueKey(const AValue: TShortCut);
begin
  FLookup.NullValueKey := AValue;
end;

procedure TDBLookupListBox.UpdateLookup;
begin
  if [csLoading, csDestroying] * ComponentState = [] then
  begin
    FLookup.Initialize(FDataLink, Items);
    ItemIndex := FLookup.GetKeyIndex;
  end;
end;

