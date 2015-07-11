{%MainUnit ../lclintf.pas}

// Open a given URL with the default browser
function OpenURL(AURL: String): Boolean;
var
{$IFDEF WinCE}
  Info: SHELLEXECUTEINFO;
{$ELSE}
  ws: WideString;
  ans: AnsiString;
  IsFileUriWithSpaces: Boolean;
const
  FileURIScheme = 'file://';
{$ENDIF}
begin
  Result := False;
  if AURL = '' then Exit;
  {$IFDEF WinCE}
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(Info);
  Info.fMask := SEE_MASK_FLAG_NO_UI;
  Info.lpVerb := 'open';
  Info.lpFile := PWideChar(UTF8Decode(AURL));
  Result := ShellExecuteEx(@Info);
  {$ELSE}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    //Urls that start with file:// are allowed to contain spaces and should be quoted on NT platform,
    //but on Win9x quoting it fails
    //Since on Windows filenames cannot contain the " character, we need not care about it and simply enclose the AURL
    IsFileUriWithSpaces := (Pos(#32,AURL) > 0) and (CompareText(Copy(AURL,1,Length(FileURIScheme)), FileURIScheme) = 0);
    if IsFileUriWithSpaces then AURL := '"' + AURL + '"';
    ws := UTF8Decode(AURL);
    Result := ShellExecuteW(0, nil, PWideChar(ws), nil, nil, SW_SHOWNORMAL) > 32;
  end
  else
  begin
    ans := Utf8ToAnsi(AURL); // utf8 must be converted to Windows Ansi-codepage
    Result := ShellExecute(0, nil, PAnsiChar(ans), nil, nil, SW_SHOWNORMAL) > 32;
  end;
  {$ENDIF}
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
begin
  Result := OpenURL(APath);
end;
