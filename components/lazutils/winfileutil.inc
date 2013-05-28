{%MainUnit fileutil.pas}

{------------------------------------------------------------------------------
  GetFileDescription
 ------------------------------------------------------------------------------}
function GetFileDescription(const AFilename: string): string;
begin
  // date + time
  Result:=lrsModified;
  try
    Result:=Result+FormatDateTime('DD/MM/YYYY hh:mm',
                           FileDateToDateTime(FileAgeUTF8(AFilename)));
  except
    Result:=Result+'?';
  end;
end;

{------------------------------------------------------------------------------
  function ReadAllLinks(const Filename: string;
    ExceptionOnError: boolean): string;
 ------------------------------------------------------------------------------}
function ReadAllLinks(const Filename: string;
  ExceptionOnError: boolean): string;
begin
  Result:=Filename;
end;

function ConsoleToUTF8(const s: string): string;// converts UTF8 string to console encoding (used by Write, WriteLn)
var
  Dst: PChar;
begin
  {$ifdef WinCE}
  Result := SysToUTF8(s);
  {$else}
  Dst := AllocMem((Length(s) + 1) * SizeOf(Char));
  if OemToChar(PChar(s), Dst) then
    Result := StrPas(Dst)
  else
    Result := s;
  FreeMem(Dst);
  Result := SysToUTF8(Result);
  {$endif}
end;

function UTF8ToConsole(const s: string): string;
var
  Dst: PChar;
begin
  {$ifdef WinCE}
  Result := UTF8ToSys(s);
  {$else}
  Result := UTF8ToSys(s);
  Dst := AllocMem((Length(Result) + 1) * SizeOf(Char));
  if CharToOEM(PChar(Result), Dst) then
    Result := StrPas(Dst);
  FreeMem(Dst);
  {$endif}
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


