{%MainUnit ../dbctrls.pas}

{******************************************************************************
                                     TDBListBox
                    data aware ListBox, base found in dbctrls.pp
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
  FLookup.UpdateData(Items[ItemIndex]);
end;

procedure TDBLookupListBox.ActiveChange(Sender: TObject);
begin
  UpdateLookup;
end;

procedure TDBLookupListBox.DataChange(Sender: TObject);
begin
  if FDatalink.Active then
    ItemIndex := Items.IndexOf(FLookup.ListFieldValue)
  else
    ItemIndex := -1;
end;

procedure TDBLookupListBox.DoSelectionChange(User: Boolean);
begin
  if User then
  begin
    FDataLink.Modified;
    FDataLink.UpdateRecord;
  end;
  inherited DoSelectionChange(User);
end;

procedure TDBLookupListBox.InitializeWnd;
begin
  inherited InitializeWnd;
  UpdateLookup;
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

function TDBLookupListBox.GetKeyField: string;
begin
  Result := FLookup.KeyField;
end;

function TDBLookupListBox.GetKeyValue: Variant;
begin
  if ItemIndex<0 then
    result := NULL
  else
    result := FLookup.GetKeyValue(Items[ItemIndex]);
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
var
  AItemIndex: Integer;
begin
  if FLookup.GetListValue(AValue, AItemIndex) then
    ItemIndex := AItemIndex;
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
    ItemIndex := Items.IndexOf(FLookup.ListFieldValue);
  end;
end;

