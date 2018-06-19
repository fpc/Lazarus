{%MainUnit ../dbctrls.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TDBRadioGroup }

function TDBRadioGroup.GetDataField: string;
begin
  Result:=FDataLink.FieldName;
end;

function TDBRadioGroup.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TDBRadioGroup.GetField: TField;
begin
  Result:=FDataLink.Field;
end;

function TDBRadioGroup.GetReadOnly: Boolean;
begin
  Result:=FDataLink.ReadOnly;
end;

procedure TDBRadioGroup.SetDataField(const AValue: string);
begin
  FDataLink.FieldName:=AValue;
end;

procedure TDBRadioGroup.SetDataSource(const AValue: TDataSource);
begin
  ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TDBRadioGroup.SetItems(const AValue: TStrings);
begin
  if Items=AValue then exit;
  Items.Assign(AValue);
  DataChange(Self);
end;

procedure TDBRadioGroup.SetReadOnly(const AValue: Boolean);
begin
  FDataLink.ReadOnly := AValue;
end;

procedure TDBRadioGroup.SetValue(const AValue: string);
var
  i: Integer;
begin
  if FValue=AValue then exit;
  FInSetValue := True;
  try
    i:=Items.Count-1;
    while (i>=0) and (AValue<>GetButtonValue(i)) do dec(i);
    ItemIndex:=i;
  finally
    FInSetValue := False;
  end;
  FValue:=AValue;
  Change;
end;

procedure TDBRadioGroup.SetValues(const AValue: TStrings);
begin
  if (FValues=AValue) then exit;
  FValues.Assign(AValue);
  DataChange(Self);
end;

procedure TDBRadioGroup.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TDBRadioGroup.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TDBRadioGroup.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TDBRadioGroup.DataChange(Sender: TObject);
begin
  if FDataLink.Field<>nil then
    Value:=FDataLink.Field.Text
  else
    Value:='';
end;

procedure TDBRadioGroup.UpdateData(Sender: TObject);
begin
  if FDataLink.Field<>nil then
    FDataLink.Field.Text:=Value;
end;

function TDBRadioGroup.GetButtonValue(Index: Integer): string;
begin
  if Index<0 then
    Result:=''
  else if (Index<FValues.Count) and (FValues[Index]<>'') then
    Result:=FValues[Index]
  else if Index<Items.Count then
    Result:=Items[Index]
  else
    Result:='';
end;

procedure TDBRadioGroup.UpdateRadioButtonStates;
var
  OldValue: String;
  NewValue: String;
  SettingValue: boolean;
begin
  OldValue:=Value;
  inherited UpdateRadioButtonStates;
  SettingValue := FInSetValue;
  NewValue := GetButtonValue(ItemIndex);
  if (NewValue<>OldValue) and FDatalink.CanModify and not SettingValue then
  begin
    if FDatalink.Edit then begin // ensure that we are in edit state
      FDataLink.Modified;
      Value := NewValue;
    end
    else
      DataChange(Self);
  end
  else
  // Workaround for TDBRadiogroup not having a ReadOnly property (i.e. ItemIndex
  // can be changed even if Dataset is ReadOnly)
  // See issues https://bugs.freepascal.org/view.php?id=31993
  // and https://bugs.freepascal.org/view.php?id=32077
  if (not FDataLink.CanModify) and
     ((FDataLink.Field = nil) or (FDataLink.Field.AsString <> NewValue)) then
  begin
    if FValue = '' then FValue := 'x' else FValue := '';
      // Otherwise, SetValue in next line would exit immediately
    Value:= OldValue;
  end;
end;

procedure TDBRadioGroup.DoEditingDone;
begin
  FDataLink.UpdateRecord;
  inherited DoEditingDone;
end;

function TDBRadioGroup.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(AAction) or
            (DataLink <> nil) and DataLink.ExecuteAction(AAction);
end;

function TDBRadioGroup.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(AAction) or
            (DataLink <> nil) and DataLink.UpdateAction(AAction);
end;

constructor TDBRadioGroup.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FValues := TStringList.Create;
end;

destructor TDBRadioGroup.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FValues.Free;
  FValues:=nil;
  inherited Destroy;
end;

// included by dbctrls.pp
