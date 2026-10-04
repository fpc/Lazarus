program ReplaceText;

uses Classes, windows, strutils, LazLogger;

var
  s: TStringList;
  i, j: Integer;
  x, FileName: String;
  t1,t2,t3: FILETIME;
  FileStream: TFileStream;
begin
  FileName := ParamStr(1);
  FileStream := TFileStream.Create(ParamStr(1), fmOpenReadWrite);
  GetFileTime(FileStream.Handle, @t1, @t2, @t3);
  FileStream.Free;

  s := TStringList.Create;
  s.LoadFromFile(ParamStr(1));
  j := 0;
  for i := 0 to s.Count-1 do begin
    x := AnsiReplaceText(s[i], ParamStr(2), ParamStr(3));
    if s[i] <> x then inc(j);
    s[i] := x;
  end;

  if j = 0 then begin
    DebugLn(['NO Replacement in file ', FileName, ' for ',ParamStr(2)]);
    s.Free;
    exit;
  end;

  s.SaveToFile(ParamStr(1));
  s.Free;
  Sleep(50);

  FileStream := TFileStream.Create(FileName, fmOpenReadWrite);
  SetFileTime(FileStream.Handle, @t1, @t2, @t3);
  FileStream.Free;
  Sleep(50);

  DebugLn(['Replaced in ', j, ' lines for file ', ParamStr(1), ' ',ParamStr(2)]);
end.

