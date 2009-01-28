{%MainUnit ../dbctrls.pas}

{******************************************************************************
                                     TDBListBox
                    data aware ListBox, base found in dbctrls.pp
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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

constructor TDBLookup.Create(AOwner: TComponent);
begin
  inherited;
  FListLink:= TFieldDataLink.Create;
  FListLink.DataSource := TDataSource.Create(Self);
  FListLink.Control := Self;
  FListLink.OnActiveChange:= @ActiveChange;
  FListLink.OnEditingChange:=@EditingChange;
  FHasLookUpField:= False;
end;

destructor TDBLookup.Destroy;
begin
  FListLink.Free;
  inherited Destroy;
end;

procedure TDBLookup.ActiveChange(Sender: TObject);
begin
  if FListLink.Active then
    Initialize(FControlLink, FControlItems)
  else Initialize(nil,nil);
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
    Result := FKeyFieldName;
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
  FKeyFieldName:= Value;
end;

procedure TDBLookup.SetListSource(Value: TDataSource);
begin
  FListSource:= Value;
end;

procedure TDBLookup.FetchLookupData;
begin
  if not Assigned(FControlItems) then
    Exit;
  FControlItems.Clear;
  if (Assigned(FListLink.DataSet) and FListLink.DataSet.Active) then
  begin
    LinkGetBookMark;
    try
      FListLink.DataSet.First;
      while not FListLink.DataSet.EOF do
      begin
        FControlItems.Add(FListField.AsString);
        FListLink.DataSet.Next;
      end;
    finally
      LinkGotoBookMark;
    end;
  end;
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

procedure TDBLookup.Initialize(AControlDataLink: TFieldDataLink; AControlItems: TStrings);
var
  DataField: TField;
  ListFields: TList;
  S: string;
begin
  FKeyField := nil;
  FKeyFieldValue:= '';
  FListField := nil;
  FListFieldValue:= '';
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
  DataField := AControlDataLink.Field;
  if not Assigned(DataField) then
    Exit;
//  TDBLookupListBox(Owner).Items.Add('Assigned DataField');
  FHasLookUpField:= (DataField.FieldKind = fkLookup);
  if FHasLookUpField then
  begin
    FListLink.DataSource.DataSet:= DataField.LookupDataSet;
    FKeyFieldName:= DataField.LookupKeyFields;
  end
  else
    FListLink.DataSource.DataSet:= FListSource.DataSet;
  if FListLink.Active and (FKeyFieldName <> '') then
  begin
    ListFields := TList.Create;
    try
      FListLink.DataSet.GetFieldList(ListFields, FListFieldName);
      if FHasLookUpField then
      begin
        FKeyField := FListLink.DataSet.FindField(DataField.LookupResultField);
        if (Assigned(FKeyField) and (ListFields.IndexOf(FKeyField) < 0)) then
          ListFields.Insert(0, FKeyField);
        if (ListFields.Count > 0) then
          FListField := TField(ListFields[0]);
      end else
      begin
        FKeyField:= FListLink.DataSet.FindField(FKeyFieldName);
        if (Assigned(FKeyField) and (ListFields.Count = 0)) then
          ListFields.Add(FKeyField);
        if ((FListFieldIndex >= 0) and (FListFieldIndex < ListFields.Count)) then
          FListField := TField(ListFields[FListFieldIndex]) else
          FListField := TField(ListFields[0]);
      end;
      if Assigned(FListField) then
        FListLink.FieldName:= FListField.FieldName;
    finally
      ListFields.Free;
    end;
    FetchLookupData;
  end;
end;

function TDBLookup.KeyFieldValueOf(const AListFieldValue: string): string;
begin
  if (FListFieldValue <> AListFieldValue) then
  begin
    FListFieldValue:= AListFieldValue;
    if FHasLookUpField or (AListFieldValue = '') or not Assigned(FKeyField) then
      FKeyFieldValue := AListFieldValue
    else if (Assigned(FListLink.DataSet) and FListLink.DataSet.Active) then
    begin
      LinkGetBookMark;
      try
        if FListLink.DataSet.Locate(FListFieldName, VarArrayOf([AListFieldValue]), []) then
          FKeyFieldValue := FKeyField.AsString
        else FKeyFieldValue:= '';
      finally
        LinkGotoBookMark;
      end;
    end;
  end;
  Result := FKeyFieldValue;
end;

function TDBLookup.ListFieldValueOf(const AKeyFieldValue: string): string;
begin
  if (FKeyFieldValue <> AKeyFieldValue) then
  begin
    FKeyFieldValue:= AKeyFieldValue;
    if FHasLookUpField or (AKeyFieldValue = '') or not Assigned(FKeyField) then
      FListFieldValue := AKeyFieldValue
    else if (Assigned(FListLink.DataSet) and FListLink.DataSet.Active) then
    begin
      LinkGetBookMark;
      try
        if FListLink.DataSet.Locate(FKeyFieldName, VarArrayOf([AKeyFieldValue]), []) then
          FListFieldValue := FListField.AsString
        else FListFieldValue:= '';
      finally
        LinkGotoBookMark;
      end;
    end;
  end;
  Result := FListFieldValue;
end;
