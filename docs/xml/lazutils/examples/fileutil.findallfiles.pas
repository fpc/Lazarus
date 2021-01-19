uses
  FileUtil;
var
  PascalFiles: TStringList;
begin
  PascalFiles := TStringList.Create;
  try
    FindAllFiles(PascalFiles, LazarusDirectory, '*.pas;*.pp;*.p;*.inc', true); //find e.g. all pascal sourcefiles
    ShowMessage(Format('Found %d Pascal source files', [PascalFiles.Count]));
  finally
    PascalFiles.Free;
  end;
end;
// or
begin
  //No need to create the stringlist; the function does that for you
  PascalFiles := FindAllFiles(LazarusDirectory, '*.pas;*.pp;*.p;*.inc', true); //find e.g. all pascal sourcefiles
  try
    ShowMessage(Format('Found %d Pascal source files', [PascalFiles.Count]));
  finally
    PascalFiles.Free;
  end;
end;
