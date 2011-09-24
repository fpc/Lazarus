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
    FDataLink.Modified;
  inherited DoSelectionChange(User);
end;

procedure TDBListBox.UpdateData(Sender: TObject);
begin
  if ItemIndex >= 0 then
    FDataLink.Field.Text := Items[ItemIndex];
end;

procedure TDBListBox.EditingDone;
begin
  FDataLink.UpdateRecord;
  inherited EditingDone;
end;

