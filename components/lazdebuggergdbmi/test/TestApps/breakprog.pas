program breakprog;

procedure WriteLnIpc(aStr: String);
begin
  writeln(aStr);
  writeln(aStr+aStr);
end;

function CalcNextUpdTime(aTime: Integer): Integer;
begin
  writeln(aTime);
  writeln(aTime+aTime);
  Result := 5*aTime;
end;

function StripFileDrive(const FileName: string): string;
begin
  writeln(FileName);
  writeln(FileName+FileName);
  Result := FileName;
  Delete(Result, 1, 1);
end;

begin
  {$IFDEF CALL_ALL} StripFileDrive('11'); {$ENDIF}
  WriteLnIpc('drive="%s", dir="%s", path="%s", nodrv=%s.');
  CalcNextUpdTime(1);
  WriteLnIpc('Now = ');
  WriteLnIpc('UpdateTime = ');
  WriteLnIpc('RealUpdateTime = ');
end.


