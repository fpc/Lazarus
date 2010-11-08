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
  FLookup.OnClearSelection := @RemoveSelection;
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
  if not (csLoading in ComponentState) and HandleAllocated then
  begin
    LockSelectionChange;
    ItemIndex := -1;
    FLookup.Initialize(FDataLink, Items);
    UnLockSelectionChange;
    DataChange(Self);
  end;
end;

procedure TDBLookupListBox.DataChange(Sender: TObject);
begin
  if FDatalink.Active then
  begin
    LockSelectionChange;
    ItemIndex := Items.IndexOf(FLookup.ListFieldValue);
    UnlockSelectionChange;
  end;
end;

procedure TDBLookupListBox.InitializeWnd;
begin
  inherited InitializeWnd;
  ActiveChange(Self);
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
  ActiveChange(Self);
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
  ActiveChange(Self);
end;

procedure TDBLookupListBox.SetListFieldIndex(const Value: Integer);
begin
  FLookup.ListFieldIndex:= Value;
  ActiveChange(Self);
end;

procedure TDBLookupListBox.SetListSource(const Value: TDataSource);
begin
  FLookup.ListSource:= Value;
  ActiveChange(Self);
end;

procedure TDBLookupListBox.SetLookupCache(const Value: boolean);
begin
  FLookup.LookupCache := Value;
  ActiveChange(Self);
end;

procedure TDBLookupListBox.RemoveSelection(Sender: TObject);
begin
  if not assigned(FDatalink.DataSet) or not FDatalink.DataSet.Active or
     not assigned(FDatalink.Field) then
  GetItemIndex; // a list-only dblookuplist do not automatically update itemindex

  ItemIndex := -1;
end;

procedure TDBLookupListBox.SetNullValueKey(const AValue: TShortCut);
begin
  FLookup.NullValueKey := AValue;
end;

