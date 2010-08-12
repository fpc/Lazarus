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
begin
  if Assigned(FDataLink.Field) then
    ItemIndex := Items.IndexOf(FDataLink.Field.AsString)
  else
    ItemIndex := -1;
end;

procedure TDBListBox.UpdateData(Sender: TObject);
begin
  if (ItemIndex >= 0) then
    FDataLink.Field.AsString := Items[ItemIndex]
  else
    FDataLink.Field.AsString := '';
end;

class procedure TDBListBox.WSRegisterClass;
const
  Done: Boolean = False;
begin
  if Done then
    Exit;
  inherited WSRegisterClass;
  RegisterPropertyToSkip(TDBListBox, 'Items', 'Removed in 0.9.29. DB control should not save/load their data from stream.', '');
  Done := True;
end;

