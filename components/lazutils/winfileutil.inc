{%MainUnit fileutil.pas}




{------------------------------------------------------------------------------
  function ReadAllLinks(const Filename: string;
    ExceptionOnError: boolean): string;
 ------------------------------------------------------------------------------}
function ReadAllLinks(const Filename: string;
  ExceptionOnError: boolean): string;
begin
  Result:=Filename;
end;



function ExtractShortPathNameUTF8(const FileName: String): String;
var
  lPathSize: DWORD;
  WideFileName, WideResult: UnicodeString;
begin
  // WinCE doesnt have this concept
  {$ifdef WinCE}
  Result := FileName;
  {$else}
  if Win32MajorVersion >= 5 then
  begin
    WideFileName := UTF8ToUTF16(FileName);
    SetLength(WideResult,Max_Path);
    lPathSize := GetShortPathNameW(PWideChar(WideFileName), PWideChar(WideResult), Length(WideResult));
    SetLength(WideResult,lPathSize);
    Result := UTF16ToUTF8(WideResult);
  end
  else
    Result:=SysToUTF8(SysUtils.ExtractShortPathName(UTF8ToSys(FileName)));
  {$endif}
end;



