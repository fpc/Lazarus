{%MainUnit ../dbctrls.pas}

{******************************************************************************
                                     TDBLookupComboBox
                    data aware lookup Combo Box, base found in dbctrls.pp
 ******************************************************************************

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

// included by dbctrls.pp

{ TDBLookupComboBox }

constructor TDBLookupComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLookup := TDBLookup.Create(Self);
  FDataLink.OnActiveChange := @ActiveChange;
end;

procedure TDBLookupComboBox.UpdateData(Sender: TObject);
var
  i: Integer;
begin
  // combo that has edit control may have unreliable itemindex like bug 20950
  if Style <> csDropDownList then
    ItemIndex := Items.IndexOf(Text);
  i := ItemIndex;
  if i <> -1 then
    FLookup.UpdateData(i);
end;

procedure TDBLookupComboBox.ActiveChange(Sender: TObject);
begin
  UpdateLookup;
end;

procedure TDBLookupComboBox.DataChange(Sender: TObject);
var
  i: Integer;
begin
  if FDatalink.Active then
    i := FLookup.GetKeyIndex
  else
    i := -1;
  ItemIndex := i;
  if i = -1 then
    Text := '';
end;

procedure TDBLookupComboBox.Select;
begin
  FDataLink.Modified;
  inherited Select;
end;

procedure TDBLookupComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FLookup.HandleNullKey(Key, Shift) then
  begin
    //clear selection
    ItemIndex := -1;
    //Text := '';
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TDBLookupComboBox.InitializeWnd;
begin
  inherited InitializeWnd;
  UpdateLookup;
end;

function TDBLookupComboBox.GetKeyField: string;
begin
  Result := FLookup.KeyField;
end;

function TDBLookupComboBox.GetKeyValue: variant;
begin
  result := FLookup.GetKeyValue(ItemIndex);
end;

function TDBLookupComboBox.GetListField: string;
begin
  Result := FLookup.ListField;
end;

function TDBLookupComboBox.GetListFieldIndex: Integer;
begin
  Result := FLookup.ListFieldIndex;
end;

function TDBLookupComboBox.GetListSource: TDataSource;
begin
  Result := FLookup.ListSource;
end;

function TDBLookupComboBox.GetLookupCache: boolean;
begin
  Result := FLookup.LookupCache;
end;

function TDBLookupComboBox.GetNullValueKey: TShortCut;
begin
  result := FLookup.NullValueKey;
end;

procedure TDBLookupComboBox.SetKeyField(const Value: string);
begin
  FLookup.KeyField := Value;
  UpdateLookup;
end;

procedure TDBLookupComboBox.SetKeyValue(const AValue: variant);
begin
  ItemIndex := FLookup.GetKeyIndex(AValue);
end;

procedure TDBLookupComboBox.SetListField(const Value: string);
begin
  FLookup.ListField := Value;
  UpdateLookup;
end;

procedure TDBLookupComboBox.SetListFieldIndex(const Value: Integer);
begin
  FLookup.ListFieldIndex := Value;
  UpdateLookup;
end;

procedure TDBLookupComboBox.SetListSource(const Value: TDataSource);
begin
  FLookup.ListSource := Value;
  UpdateLookup;
end;

procedure TDBLookupComboBox.SetLookupCache(const Value: boolean);
begin
  FLookup.LookupCache := Value;
  UpdateLookup;
end;

procedure TDBLookupComboBox.SetNullValueKey(const AValue: TShortCut);
begin
  FLookup.NullValueKey := AValue;
end;

procedure TDBLookupComboBox.UpdateLookup;
var
  i: Integer;
begin
  if ([csLoading, csDestroying] * ComponentState) = [] then
  begin
    FLookup.Initialize(FDataLink, Items);
    i := FLookup.GetKeyIndex;
    ItemIndex := i;
    if i = -1 then
      Text := '';
  end;
end;

procedure TDBLookupComboBox.CloseUp;
begin
  if Style = csDropDownList then
    FDataLink.UpdateRecord;
  inherited CloseUp;
end;

