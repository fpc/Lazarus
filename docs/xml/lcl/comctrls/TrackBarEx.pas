 
procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  ProgressBar1.Position := (TrackBar1.Position)*10
end;