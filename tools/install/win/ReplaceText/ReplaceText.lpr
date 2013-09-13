program ReplaceText;

uses Classes, strutils, LazLogger;

var
  s: TStringList;
  i, j: Integer;
  x: String;
begin
  s := TStringList.Create;
  s.LoadFromFile(ParamStr(1));
  j := 0;
  for i := 0 to s.Count-1 do begin
    x := AnsiReplaceText(s[i], ParamStr(2), ParamStr(3));
    if s[i] <> x then inc(j);
    s[i] := x;
  end;
  s.SaveToFile(ParamStr(1));
  s.Free;
  DebugLn(['Replaced in ', j, ' lines for file ', ParamStr(1), ' ',ParamStr(2)]);
end.

