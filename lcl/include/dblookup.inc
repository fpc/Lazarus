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


{ TDBLookup }

{
Note:
TField lookup properties
  KeyFields: Semicolon separate list of Data fields in TField's dataset
  LookupDataSet: The Dataset to search for values
  LookupKeyFields: Semicolon separated list of key field names in LookupDataset
  lookupResultField: Name of the field in the LookupDataset which must be the
    same data type as the TField
The lookup Value of the TField is the Value of the LookupResultField determined
  by a Locate in the lookupDataSet of the LookupKeyFields based on the Values of
  the KeyFields

Lookup DataControl properties
  KeyField: Name of the field in the LookupDataset which must be the
    same data type as the TField
  ListSource: The Datasource linking to the lookup dataset
  ListField: The Name of the field in the lookup dataset to list in the control

Usage
TDBLookup
fields:
  FDataFields is the list of fields in the dataset which provide the lookup
    values and can be edited based on the Control's selected item
    If the Control's Datafield is a normal field then the Datafield is pointed
    to by FDataFields[0] and FDataFields.Count is 1.
    If the Control's Datafield is a lookup field then the FDataFields point to
    the field's KeyFields
  FKeyFields is the list of fields in the lookup dataset used
    to locate the lookup result based on the values of the FDataFields
  FKeyFields.Count must equal the FDataFields.Count and the fields be of
    corresponding types

}

constructor TDBLookup.Create(AOwner: TComponent);
begin
  inherited;
  FDataFields := TList.Create;
  FKeyFields := TList.Create;
  FListLink:= TFieldDataLink.Create;
  FListLink.DataSource := TDataSource.Create(Self);
  FListLink.Control := Self;
  FListLink.OnActiveChange := @ActiveChange;
  FListLink.OnEditingChange:=@EditingChange;
  //FHasLookUpField:= False;
  //FListLinkTmpSetActive := False;
  //FLookupCache := False;
end;

destructor TDBLookup.Destroy;
begin
  FDataFields.Destroy;
  FKeyFields.Destroy;
  FListLink.Destroy;
  inherited Destroy;
end;

procedure TDBLookup.ActiveChange(Sender: TObject);
begin
  if (csDestroying in ComponentState) then
    Exit;
  if FListLink.Active and not FListLinkTmpSetActive then
    Initialize(FControlLink, FControlItems);
end;

procedure TDBLookup.EditingChange(Sender: TObject);
begin
  if FListLink.Active and not (FListLink.Editing) then
    FetchLookupData;
end;

// do not show in property inspector if FHasLookUpField
function TDBLookup.GetKeyFieldName: string;
begin
  if FHasLookUpField then
    Result:= ''
  else
    Result := FKeyFieldNames;
end;

function TDBLookup.GetListSource: TDataSource;
begin
  if FHasLookUpField then
    Result:= nil
  else
    Result:= FListSource;
end;

procedure TDBLookup.SetKeyFieldName(const Value: string);
begin
  FKeyFieldNames := Value;
end;

procedure TDBLookup.SetListFieldName(const Value: string);
begin
  FListFieldName := Value;
  FListLink.FieldName := Value;
end;

procedure TDBLookup.SetListSource(Value: TDataSource);
begin
  if FListSource = Value then
    Exit;
  if Assigned(FListSource) then
    FListSource.RemoveFreeNotification(Self);
  FListSource:= Value;
  if Assigned(FListSource) then
    FListSource.FreeNotification(Self);
end;

procedure TDBLookup.SetLookupCache(const Value: boolean);
begin
  FLookupCache := Value;
end;

{
Returns True if selection must be cleared
If NullKey is detected set Key to VK_UNKNOWN
}

function TDBLookup.HandleNullKey(var Key: Word; Shift: TShiftState): Boolean;
var
  i: Integer;
begin
  Result:=False;
  if FNullValueKey=KeyToShortCut(Key, Shift) then
  begin
    // null associated field
    if Assigned(FControlLink) and (FDataFieldNames<>'') and FControlLink.Active then
    begin
      FControlLink.DataSet.Edit;
      for i:=0 to FDataFields.Count-1 do
        TField(FDataFields[i]).Clear;
    end else
      Result:=Assigned(FListLink.DataSet) and FListLink.DataSet.Active and Assigned(FListField);
    Key:=VK_UNKNOWN;
  end;
end;

procedure TDBLookup.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then
    if (AComponent = FListSource) then
      FListSource:= nil;
end;

procedure TDBLookup.LinkGetBookMark;
begin
  FListLinkTmpSetActive := not FListLink.DataSet.Active;
  if FListLinkTmpSetActive then
    FListLink.DataSet.Active := True
  else
  begin
    FLinkBookMark := FListLink.DataSet.GetBookmark;
    FListLink.DataSet.DisableControls;
  end;
end;

procedure TDBLookup.LinkGotoBookMark;
begin
  if FListLinkTmpSetActive then
  begin
    FListLink.DataSet.Active := False;
    FListLinkTmpSetActive := False;
  end
  else
  try
    FListLink.DataSet.GotoBookmark(FLinkBookMark);
    FListLink.DataSet.FreeBookmark(FLinkBookMark);
  finally
    FListLink.DataSet.EnableControls;
  end;
end;

procedure TDBLookup.FetchLookupData;
var
  s: String;
  i: Integer;
begin
  if not Assigned(FControlItems) then
    Exit;
  FControlItems.Clear;
  if not (Assigned(FListLink.DataSet) and Assigned(FListField)) then
    Exit;
  LinkGetBookMark;
  try
    FListLink.DataSet.First;
    SetLength(FListKeys, FListLink.Dataset.RecordCount);
    i := 0;
    while not FListLink.DataSet.EOF do
    begin
      s := FListField.DisplayText;
      FListKeys[i] := FListLink.DataSet.FieldValues[FKeyFieldNames];
      Inc(i);
      FControlItems.Add(s);
      FListLink.DataSet.Next;
    end;
  finally
    LinkGotoBookMark;
  end;
end;

procedure TDBLookup.Initialize(AControlDataLink: TFieldDataLink; AControlItems: TStrings);
var
  ListFields: TList;
  S: string;
begin
  FDataFields.Clear;
  FKeyFields.Clear;
  FListField := nil;
  FHasLookUpField := False;
  FLookUpFieldIsCached := False;
  FControlItems := AControlItems;
  FControlLink := AControlDataLink;
  if Assigned(FControlLink) and Assigned(FControlLink.DataSet)
    and FControlLink.DataSet.Active then
  begin
    if not Assigned(FControlLink.Field) then
    // should be but (sometimes) not (bug?)
    begin
      S:= FControlLink.FieldName;
      FControlLink.FieldName:= '';
      FControlLink.FieldName:= S;
    end;
    if Assigned(FControlLink.Field) then
    begin
      FHasLookUpField := (FControlLink.Field.FieldKind = fkLookup);
      FLookUpFieldIsCached := (FHasLookupField and FControlLink.Field.LookupCache);
      if FHasLookUpField then
      begin
        if (FListLink.DataSource.DataSet <> FControlLink.Field.LookupDataSet) then
          FListLink.DataSource.DataSet:= FControlLink.Field.LookupDataSet;
        FDataFieldNames := FControlLink.Field.KeyFields;
        FKeyFieldNames := FControlLink.Field.LookupKeyFields;
      end else
        FDataFieldNames := FControlLink.Field.FieldName;
      FControlLink.DataSet.GetFieldList(FDataFields, FDataFieldNames);
    end;
  end;
  if not FHasLookUpField then
    if Assigned(FListSource) then
    begin
      if (FListLink.DataSource.DataSet <> FListSource.DataSet) then
        FListLink.DataSource.DataSet := FListSource.DataSet;
    end
    else
      FListLink.Datasource.DataSet := nil;

  if (FKeyFieldNames > '') and Assigned(FListLink.Dataset) then
  begin
    FListLinkTmpSetActive := not FListLink.DataSet.Active;
    if FListLinkTmpSetActive then
    try
      FListLink.DataSet.Open;
    except
      FListLinkTmpSetActive := False;
      Exit;
    end;
    ListFields := TList.Create;
    try
      if FListLink.DataSet.Active then
      begin
        FListLink.DataSet.GetFieldList(ListFields, FListFieldName);
        FListLink.DataSet.GetFieldList(FKeyFields, FKeyFieldNames);
        if FHasLookUpField then
        begin
          FListField := FListLink.DataSet.FindField(AControlDataLink.Field.LookupResultField);
          if (Assigned(FListField) and (ListFields.IndexOf(FListField) < 0)) then
            ListFields.Insert(0, FListField);
          if (ListFields.Count > 0) then
            FListField := TField(ListFields[0]);
        end else
        begin
          if ((FKeyFields.Count > 0) and (ListFields.Count = 0)) then
            ListFields.Add(FKeyFields[0]);
          if ((FListFieldIndex > -1) and (FListFieldIndex < ListFields.Count)) then
            FListField := TField(ListFields[FListFieldIndex])
          else
            FListField := TField(ListFields[0]);
        end;
        if Assigned(FListField) then
          FListLink.FieldName:= FListField.FieldName;
      end;
    finally
      ListFields.Free;
      if FListLinkTmpSetActive then
      begin
        FListLink.DataSet.Active := False;
        FListLinkTmpSetActive := False;
      end;
    end;
    FetchLookupData;
  end;
end;

function TDBLookup.KeyFieldValue: Variant;
begin
  if Assigned(FControlLink) and FControlLink.Active and (FDataFieldNames <> '') then
    Result := FControlLink.DataSet.FieldValues[FDataFieldNames]
  else
    Result := Null;
end;

procedure TDBLookup.UpdateData(ValueIndex: Integer; ScrollDataset: Boolean);
var
  I: Integer;
  Key: Variant;
  SavedEvent: TNotifyEvent;
begin
  if (ValueIndex < 0) or (ValueIndex >= Length(FListKeys)) then
    Exit;
  Key := FListKeys[ValueIndex];
  if FControlLink.Active then
  begin
    if VarSameValue(Key, FControlLink.DataSet.FieldValues[FDataFieldNames]) then
      Exit;
    SavedEvent := FControlLink.OnDataChange;
    FControlLink.OnDataChange := nil;
    FControlLink.Modified;
    FControlLink.Edit;
    FControlLink.OnDataChange := SavedEvent;
    if FDataFields.Count = 1 then
      TField(FDataFields[0]).Value := Key
    else
    begin
      for I := 0 to FDataFields.Count -1 do
        TField(FDataFields[I]).Value := Key[I];
    end;
  end;
  if ScrollDataset then
    FListLink.DataSet.Locate(FKeyFieldNames, Key, []);
end;

function TDBLookup.GetKeyValue(ValueIndex: Integer): Variant;
begin
  if (ValueIndex < 0) or (ValueIndex >= Length(FListKeys)) then
    Result := Null
  else
    Result := FListKeys[ValueIndex];
end;

function TDBLookup.GetKeyIndex: Integer;
begin
  if Assigned(FControlLink) and FControlLink.Active and (FDataFieldNames <> '') then
    Result := GetKeyIndex(FControlLink.DataSet.FieldValues[FDataFieldNames])
  else
    Result := -1;
end;

function TDBLookup.GetKeyIndex(const AKeyValue: Variant): Integer;
begin
  Result := 0;
  while Result < Length(FListKeys) do
  begin
    if VarSameValue(AKeyValue, FListKeys[Result]) then
      Exit;
    Inc(Result);
  end;
  Result := -1;
end;
