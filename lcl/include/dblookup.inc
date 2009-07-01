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

{$IF (FPC_VERSION = 2) AND (FPC_RELEASE = 2) AND (FPC_PATCH <= 2)}
  {$DEFINE HAS_NOT_FIRSTKEYBYVALUE}
{$ENDIF}

constructor TDBLookup.Create(AOwner: TComponent);
begin
  inherited;
  FDataFields := TList.Create;
  FKeyFields := TList.Create;
  FListLink:= TFieldDataLink.Create;
  FListLink.DataSource := TDataSource.Create(Self);
  FListLink.Control := Self;
  FListLink.OnEditingChange:=@EditingChange;
  FHasLookUpField:= False;
  FLookupCache := False;
end;

destructor TDBLookup.Destroy;
begin
  FDataFields.Free;
  FKeyFields.Free;
  FListLink.Free;
  FLookupList.Free;
  inherited Destroy;
end;

procedure TDBLookup.EditingChange(Sender: TObject);
begin
  if not (FListLink.Editing) then
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

procedure TDBLookup.SetListSource(Value: TDataSource);
begin
  FListSource:= Value;
end;

procedure TDBLookup.SetLookupCache(const Value: boolean);
begin
  FLookupCache := Value;
  if (Value and not Assigned(FLookupList)) then
    FLookupList := TLookupList.Create;
end;

procedure TDBLookup.LinkGetBookMark;
begin
  FLinkBookMark := FListLink.DataSet.GetBookmark;
  FListLink.DataSet.DisableControls;
end;

procedure TDBLookup.LinkGotoBookMark;
begin
  try
    FListLink.DataSet.GotoBookmark(FLinkBookMark);
    FListLink.DataSet.FreeBookmark(FLinkBookMark);
  finally
    FListLink.DataSet.EnableControls;
  end;
end;

procedure TDBLookup.FetchLookupData;
var
  TmpActive: Boolean;
begin
  if not Assigned(FControlItems) then
    Exit;
  FControlItems.Clear;
  if not (Assigned(FListLink.DataSet) and Assigned(FListField)) then
    Exit;
  TmpActive := FListLink.DataSet.Active;
  if TmpActive then
    LinkGetBookMark
  else
    FListLink.DataSet.Active := True;
  try
    if FLookupCache then
      FLookupList.Clear;
    FListLink.DataSet.First;
    while not FListLink.DataSet.EOF do
    begin
      if (FLookupCache and not FLookUpFieldIsCached) then
        FLookupList.Add(FListLink.DataSet.FieldValues[FKeyFieldNames],
          FListField.Value);
      FControlItems.Add(FListField.AsString);
      FListLink.DataSet.Next;
    end;
  finally
    if TmpActive then
      LinkGotoBookMark
    else
      FListLink.DataSet.Active := False;
  end;
end;

procedure TDBLookup.Initialize(AControlDataLink: TFieldDataLink; AControlItems: TStrings);
var
  ListFields: TList;
  TmpActive: Boolean;
  S: string;
begin
  FDataFields.Clear;
  FKeyFields.Clear;
  FListField := nil;
  if not (Assigned(AControlDataLink) and Assigned(AControlItems)) then
    Exit; // Closing or our DataLink is Active but not the Control's DataLink
  FControlLink:= AControlDataLink;
  FControlItems:= AControlItems;
  if not Assigned(AControlDataLink.Field) then
  // should be but (sometimes) not (bug?)
  begin
    S:= AControlDataLink.FieldName;
    AControlDataLink.FieldName:= '';
    AControlDataLink.FieldName:= S;
  end;
  if not Assigned(AControlDataLink.Field) then
    Exit;
  FHasLookUpField := (AControlDataLink.Field.FieldKind = fkLookup);
  FLookUpFieldIsCached := (FHasLookupField and AControlDataLink.Field.LookupCache);
  if FHasLookUpField then
  begin
    FListLink.DataSource.DataSet:= AControlDataLink.Field.LookupDataSet;
    FDataFieldNames := AControlDataLink.Field.KeyFields;
    FKeyFieldNames := AControlDataLink.Field.LookupKeyFields;
  end
  else
  if FListSource<>nil then begin
    FListLink.DataSource.DataSet := FListSource.DataSet;
    FDataFieldNames := AControlDataLink.Field.FieldName;
  end else
    FListLink.Datasource.DataSet := nil;

  if (FKeyFieldNames <> '') and (FListLink.Dataset<>nil) then
  begin
    ListFields := TList.Create;
    TmpActive := FListLink.DataSet.Active;
    try
      AControlDataLink.DataSet.GetFieldList(FDataFields, FDataFieldNames);
      FListLink.DataSet.Active := True;
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
        if ((FListFieldIndex >= 0) and (FListFieldIndex < ListFields.Count)) then
          FListField := TField(ListFields[FListFieldIndex]) else
          FListField := TField(ListFields[0]);
      end;
      if Assigned(FListField) then
        FListLink.FieldName:= FListField.FieldName;
    finally
      ListFields.Free;
      FListLink.DataSet.Active := TmpActive;
    end;
    FetchLookupData;
  end;
end;

function TDBLookup.ListFieldValue: string;
var
  Key: Variant;
  TmpActive: Boolean;
begin
  Result := '';
  if not ((Assigned(FControlLink) and assigned(FListField) and FControlLink.Active)) then
    Exit;
  Key := FControlLink.DataSet.FieldValues[FDataFieldNames];
  if FHasLookupField then
  begin
    if (FLookupCache and not FLookUpFieldIsCached) then
      Result := FLookupList.ValueOfKey(Key)
    else
      Result := FControlLink.Field.AsString;
    Exit;
  end;
  if FLookupCache then
  begin
    Result := FLookupList.ValueOfKey(Key);
    Exit;
  end;
  TmpActive := FListLink.DataSet.Active;
  if TmpActive then
    LinkGetBookMark
  else
    FListLink.DataSet.Active := True;
  try
    if FListLink.DataSet.Locate(FKeyFieldNames,
      FControlLink.DataSet.FieldValues[FDataFieldNames], []) then
      Result := FListField.AsString
    else Result:= '';
  finally
    if TmpActive then
      LinkGotoBookMark
    else
      FListLink.DataSet.Active := False;
  end;
end;

procedure TDBLookup.UpdateData(const AListFieldValue: string);
{$IFDEF HAS_NOT_FIRSTKEYBYVALUE}
begin
end;
{$ELSE}
var
  I: Integer;
  TmpActive: Boolean;
  Key: Variant;
begin
  if AListFieldValue = FControlLink.Field.AsString then
    Exit;
  if FLookupCache and not FLookupFieldIsCached then
  begin
    Key := FLookupList.FirstKeyByValue(AListFieldValue);
    if not VarIsNull(Key) then
    begin
      FControlLink.DataSet.Edit;
      for I := 0 to FDataFields.Count -1 do
        TField(FDataFields[I]).Value := Key[I];
      if FHasLookupField then
        FControlLink.Field.AsString := AListFieldValue;
    end;
    Exit;
  end;
  if not (Assigned(FListLink.DataSet) and Assigned(FListField)) then
    Exit;
  TmpActive := FListLink.DataSet.Active;
  if TmpActive then
    LinkGetBookMark
  else
    FListLink.DataSet.Active := True;
  try
    if FListLink.DataSet.Locate(FListField.FieldName, VarArrayOf([AListFieldValue]), []) then
    begin
      FControlLink.DataSet.Edit;
      for I := 0 to FDataFields.Count -1 do
        TField(FDataFields[I]).Value := TField(FKeyFields[I]).Value;
      if FHasLookupField then
        FControlLink.Field.AsString := AListFieldValue;
    end;
  finally
    if TmpActive then
      LinkGotoBookMark
    else
      FListLink.DataSet.Active := False;
  end;
end;
{$ENDIF}
