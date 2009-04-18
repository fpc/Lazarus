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

// included by dbctrls.pas

{ TDBComboBox }

{
  procedure TDBComboBox.DataChange(Sender: TObject);
  begin
    inherited DataChange(Sender);
  end;
}

procedure TDBComboBox.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
  FDataLink.Field.AsString := Text;
end;

procedure TDBComboBox.UpdateText;
begin
  if Assigned(FDataLink.Field ) then
    Text := FDataLink.Field.AsString
  else
    inherited;
end;
