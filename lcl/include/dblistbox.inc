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

{ TDBListBox }

{ Protected Methods }

procedure TDBListBox.DataChange(Sender: TObject);
var
  DataLinkField: TField;
begin
  DataLinkField := FDataLink.Field;
  if Assigned(DataLinkField) then
    ItemIndex := Items.IndexOf(DataLinkField.Text)
  else
    ItemIndex := -1;
end;

procedure TDBListBox.DoSelectionChange(User: Boolean);
begin
  if User then
  begin
    if FDataLink.CanModify then
    begin
      //protect against undesired call to DataChange
      FDataLink.OnDataChange := nil;
      if FDataLink.Edit then
        FDataLink.Modified;
      FDataLink.OnDataChange := @DataChange;
    end
    else
      DataChange(Self);
  end;
  inherited DoSelectionChange(User);
end;

procedure TDBListBox.UpdateData(Sender: TObject);
begin
  if ItemIndex >= 0 then
    FDataLink.Field.Text := Items[ItemIndex];
end;

procedure TDBListBox.DoEditingDone;
begin
  FDataLink.UpdateRecord;
  inherited DoEditingDone;
end;

