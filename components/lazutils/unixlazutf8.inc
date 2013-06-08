{%MainUnit lazutf8.pas}



function ConsoleToUTF8(const s: string): string;// converts UTF8 string to console encoding (used by Write, WriteLn)
begin
  Result := SysToUTF8(S);
end;

function UTF8ToConsole(const s: string): string;
begin
  Result := UTF8ToSys(s);
end;

function ParamStrUTF8(Param: Integer): string;
begin
  Result:=SysToUTF8(ObjPas.ParamStr(Param));
end;

procedure InitLazUtf8;
begin
  //dummy procedure
end;
