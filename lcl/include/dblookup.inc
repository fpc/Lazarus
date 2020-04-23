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

type

  { TDBLookupDataLink }

  TDBLookupDataLink = class(TDataLink)
  private
    FLookup: TDBLookup;
    FRecordUpdated: Boolean;
  protected
    procedure ActiveChanged; override;
    procedure DataEvent(Event: TDataEvent; Info: Ptrint); override;
  public
    constructor Create(Lookup: TDBLookup);
  end;

{ TDBLookupDataLink }

procedure TDBLookupDataLink.ActiveChanged;
begin
  inherited ActiveChanged;
  FLookup.ActiveChange(Self);
end;

procedure TDBLookupDataLink.DataEvent(Event: TDataEvent; Info: Ptrint);
var
  I: Integer;
  S: TDataSetState;
begin
  inherited DataEvent(Event, Info);
  if Event = deDataSetChange then
  begin
    I := DataSet.RecordCount;
    S := DataSet.State;
    if FRecordUpdated or ((FLookup.ControlItems <> nil) and (FLookup.ControlItems.Count <> DataSet.RecordCount)) then
    begin
      FRecordUpdated := False;
      FLookup.DatasetChange(Self);
    end;
  end else if Event = deUpdateRecord then
    FRecordUpdated := True;
end;

constructor TDBLookupDataLink.Create(Lookup: TDBLookup);
begin
  inherited Create;
  FLookup := Lookup;
end;

constructor TDBLookup.Create(AOwner: TComponent);
begin
  inherited;
  FDataFields := TList.Create;
  FKeyFields := TList.Create;
  FListLink := TDBLookupDataLink.Create(Self);
  FDisplayEmpty := '';
  FEmptyValue := '';
  //FHasLookUpField := False;
  //FLookupCache := False;
end;

destructor TDBLookup.Destroy;
begin
  FDataFields.Destroy;
  FKeyFields.Destroy;
  FListLink.Destroy;
  inherited Destroy;
end;

procedure TDBLookup.Initialize(AControlDataLink: TFieldDataLink;
  AControlItems: TStrings);
begin
  if FInitializing then
    Exit;
  FInitializing := True;
  try
    FControlLink := AControlDataLink;
    FControlItems := AControlItems;
    DoInitialize;
    if FScrollListDataset
    and Assigned(FListLink) and Assigned(FListLink.DataSet) and FListLink.DataSet.Active
    and Assigned(FControlLink) and Assigned(FControlLink.DataSet) and FControlLink.DataSet.Active
    then
      FListLink.DataSet.Locate(FKeyFieldNames, FControlLink.DataSet.FieldValues[FDataFieldNames], []);
  finally
    FInitializing := False;
  end;
end;

procedure TDBLookup.ActiveChange(Sender: TObject);
begin
  if (csDestroying in ComponentState) then
    Exit;
  if FListLink.Active then
    Initialize(FControlLink, FControlItems)
  else
  begin
    SetLength(FListKeys, 0);
    FControlItems.Clear;
  end;
  if Assigned(FControlLink) and FControlLink.Active then
    FControlLink.Reset;
end;

procedure TDBLookup.DatasetChange(Sender: TObject);
begin
  if FListLink.Active and not FListLink.Editing then
  begin
    FetchLookupData;
    if Assigned(FControlLink) and FControlLink.Active then
      FControlLink.Reset;
  end;
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
  if (Operation=opRemove) and (AComponent = FListSource) then
    FListSource := nil;
end;

procedure SlideKeyList(var KeyList: array of Variant; Index, ListCount: Integer);
var
  i: Integer;
begin
  for i := ListCount - 1 downto Index do
    KeyList[i + 1] := KeyList[i];
end;

{$IF FPC_FULLVERSION < 30000}
function IsClass(Instance: TObject; const ClassName: ShortString): Boolean;
var
  ClassRef: TClass;
begin
  Result := False;
  ClassRef := Instance.ClassType;
  while ClassRef <> nil do
  begin
    Result := ClassRef.ClassNameIs(ClassName);
    if Result then
      Exit;
    ClassRef := ClassRef.ClassParent;
  end;
end;
{$ENDIF}

procedure TDBLookup.FetchLookupData;
var
  KeyIndex, KeyListCount: Integer;
  ListLinkDataSet: TDataSet;
  Bookmark: TBookmark;
  {$IF FPC_FULLVERSION < 30000}
  DatasetSupportsBlockRead: Boolean;
  {$ENDIF}
begin
  if not Assigned(FControlItems) then
    Exit;
  {$IF FPC_FULLVERSION < 30000}
   if FFetchingLookupData then
     Exit;
  {$ENDIF}
  FControlItems.Clear;
  ListLinkDataSet := FListLink.DataSet;
  if not (Assigned(ListLinkDataSet) and Assigned(FListField)) then
    Exit;

  if ListLinkDataSet.IsEmpty then begin
    // Add Empty value if no recs into dataset
    if FEmptyValue<>'' then begin
      FControlItems.BeginUpdate;
      try
        KeyIndex := FControlItems.Add(FDisplayEmpty);
        SetLength(FListKeys, 1);
        FListKeys[KeyIndex] := FEmptyValue;
        KeyListCount := 1;
      finally
        FControlItems.EndUpdate;
      end;
    end;
    Exit;
  end;
  Bookmark := ListLinkDataSet.GetBookmark;
  //in fpc 2.6.4, TMemDataset does not supports BlockRead. Issues 26356, 27959
  {$IF FPC_FULLVERSION < 30000}
  DatasetSupportsBlockRead := not IsClass(ListLinkDataSet, 'TMemDataset') and not IsClass(ListLinkDataSet, 'TFixedFormatDataSet');
  if DatasetSupportsBlockRead then
    ListLinkDataSet.BlockReadSize := 1
  else
  begin
    FFetchingLookupData := True;
    ListLinkDataSet.DisableControls;
  end;
  {$ELSE}
  ListLinkDataSet.BlockReadSize := 1;
  {$ENDIF}
  FControlItems.BeginUpdate;
  try
    //needed to handle sqldb.TSQLQuery that does not has a reliable recordcount after Open
    ListLinkDataSet.Last;
    ListLinkDataSet.First;
    KeyListCount := 0;
    SetLength(FListKeys, ListLinkDataSet.RecordCount);
    // Handle Empty Value and Empty Display
    if FEmptyValue<>'' then begin
      KeyIndex := FControlItems.Add(FDisplayEmpty);
      SetLength(FListKeys, ListLinkDataSet.RecordCount+1); // Add one more
      FListKeys[KeyIndex] := FEmptyValue;
      KeyListCount := 1;
    end;

    while not ListLinkDataSet.EOF do
    begin
      KeyIndex := FControlItems.Add(FListField.DisplayText);
      //check if item was really added (in sorted list duplicate values are not added)
      if FControlItems.Count > KeyListCount then
      begin
        if KeyIndex < KeyListCount then
          SlideKeyList(FListKeys, KeyIndex, KeyListCount);
        FListKeys[KeyIndex] := ListLinkDataSet.FieldValues[FKeyFieldNames];
        Inc(KeyListCount);
      end;
      ListLinkDataSet.Next;
    end;
    SetLength(FListKeys, KeyListCount);
  finally
    FControlItems.EndUpdate;
    ListLinkDataSet.GotoBookmark(Bookmark);
    ListLinkDataSet.FreeBookmark(Bookmark);
    {$IF FPC_FULLVERSION < 30000}
    if DatasetSupportsBlockRead then
      ListLinkDataSet.BlockReadSize := 0
    else
    begin
      ListLinkDataSet.EnableControls;
      FFetchingLookupData := False;
    end;
    {$ELSE}
    ListLinkDataSet.BlockReadSize := 0;
    {$ENDIF}
  end;
end;

procedure TDBLookup.DoInitialize;
var
  ListFields: TList;
  ListLinkDataset: TDataSet;
begin
  FDataFields.Clear;
  FKeyFields.Clear;
  FListField := nil;
  FHasLookUpField := False;
  FLookUpFieldIsCached := False;
  if Assigned(FControlLink) and Assigned(FControlLink.DataSet)
    and FControlLink.DataSet.Active then
  begin
    if Assigned(FControlLink.Field) then
    begin
      FHasLookUpField := (FControlLink.Field.FieldKind = fkLookup);
      FLookUpFieldIsCached := (FHasLookupField and FControlLink.Field.LookupCache);
      if FHasLookUpField then
      begin
        if FLookupSource = nil then
          FLookupSource := TDataSource.Create(Self);
        if (FLookupSource.DataSet <> FControlLink.Field.LookupDataSet) then
          FLookupSource.DataSet:= FControlLink.Field.LookupDataSet;
        FListLink.DataSource := FLookupSource;
        FDataFieldNames := FControlLink.Field.KeyFields;
        FKeyFieldNames := FControlLink.Field.LookupKeyFields;
      end else
        FDataFieldNames := FControlLink.Field.FieldName;
      FControlLink.DataSet.GetFieldList(FDataFields, FDataFieldNames);
    end;
  end;
  if not FHasLookUpField then
    FListLink.DataSource := FListSource;

  if (FKeyFieldNames > '') and FListLink.Active then
  begin
    ListLinkDataset := FListLink.DataSet;
    ListFields := TList.Create;
    try
      ListLinkDataset.GetFieldList(ListFields, FListFieldName);
      ListLinkDataset.GetFieldList(FKeyFields, FKeyFieldNames);
      if FHasLookUpField then
      begin
        FListField := ListLinkDataset.FindField(FControlLink.Field.LookupResultField);
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
    finally
      ListFields.Free;
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

procedure TDBLookup.UpdateData(ValueIndex: Integer);
var
  I: Integer;
  Key: Variant;
  SavedEvent: TNotifyEvent;
begin
  if (ValueIndex < 0) or (ValueIndex >= Length(FListKeys)) then
    Exit;
  Key := FListKeys[ValueIndex];
  if FScrollListDataset then
    FListLink.DataSet.Locate(FKeyFieldNames, Key, []);
  if Assigned(FControlLink) and FControlLink.Active then
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
