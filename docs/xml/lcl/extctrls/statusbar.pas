procedure TTrivialForm1.Splitter1Moved(Sender: TObject);
begin
  StatusBar1.SimpleText := IntToStr(Splitter1.Left);
end;
