{%MainUnit ../dbctrls.pas}
{
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
    // ToDo: Use Field.Text
    Value:=FDataLink.Field.DisplayText
  else
    Value:='';
end;

procedure TDBRadioGroup.UpdateData(Sender: TObject);
begin
  if FDataLink.Field<>nil then
    // ToDo: Use Field.Text
    FDataLink.Field.AsString:=Value;
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
  SettingValue: boolean;
begin
  OldValue:=Value;
  inherited UpdateRadioButtonStates;
  SettingValue := FInSetValue;
  Value := GetButtonValue(ItemIndex);
  if (Value<>OldValue) and FDatalink.CanModify and not SettingValue then
  begin
    if FDatalink.Edit then  // ensure that we are in edit state
      FDataLink.Modified
    else
      DataChange(Self);
  end;
end;

procedure TDBRadioGroup.EditingDone;
begin
  FDataLink.UpdateRecord;
  inherited EditingDone;
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

// included by dbctrls.pas
