{%MainUnit ../dbctrls.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

// included by dbctrls.pp

{ TDBComboBox }

procedure TDBComboBox.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

procedure TDBComboBox.Change;
begin
  try
    if FDataLink.CanModify then begin
      FDataLink.Field.AsString := Text;
      FDatalink.Modified;
    end;
  finally
    inherited Change;
  end;
end;

procedure TDBComboBox.DataChange(Sender: TObject);
var
  DataLinkField: TField;
begin
  DataLinkField := FDataLink.Field;
  if Assigned(DataLinkField) then
    Text := DataLinkField.Text
  else
    Text := '';
end;

procedure TDBComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key=VK_ESCAPE then begin
    //cancel out of editing by reset on esc
    FDataLink.Reset;
    SelectAll;
    Key := VK_UNKNOWN;
  end else
  if Key in [VK_DELETE, VK_BACK] then begin
    if not FDataLink.Edit then
      Key := VK_UNKNOWN;
  end;
end;

procedure TDBComboBox.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  case Key of
    #8: // special keys
      if not FDatalink.Edit then
        Key:=#0;

    #32..#255: //standard keys
      if not FieldCanAcceptKey(FDataLink.Field, Key) or not FDatalink.Edit then
        Key:=#0;
  end;//case
end;

procedure TDBComboBox.Select;
begin
  //avoid reseting text when calling select
  FDataLink.OnDataChange := nil;
  try
    if FDataLink.Edit then
    begin
       FDataLink.UpdateData;
       inherited Select;
    end
    else
    begin
       // if cannot modify, let it reset
       FDatalink.Reset;
       DataChange(Self);
    end;
  finally
    FDataLink.OnDataChange := @DataChange;
  end;
end;
