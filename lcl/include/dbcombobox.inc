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

procedure TDBComboBox.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  case Key of
    #32..#255: //standard keys
      if not FieldCanAcceptKey(FDataLink.Field, Key) then
        Key:=#0;
  end;//case
end;

