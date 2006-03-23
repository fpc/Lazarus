{ This example uses a check box on a form. When the application runs, 
  the check box is initially checked. When the user clicks it,
  the check box is unchecked. Clicking it again grays the check box. }
procedure TForm1.FormCreate(Sender: TObject);
begin
  Checkbox1.AllowGrayed := True;
  Checkbox1.State := cbChecked;
end;
