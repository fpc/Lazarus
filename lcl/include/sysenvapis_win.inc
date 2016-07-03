{%MainUnit ../lclintf.pas}

{$I ../../components/lazutils/lazutils_defines.inc} //LCL depends on LazUtils, so this is OK

{$IFnDEF WinCE}
function AssocQueryStringW(Flags: Integer; Str: Integer; pszAssoc, pszExtra, pszOut: PWChar;
  var pcchOut: DWORD): HRESULT; stdcall; external 'shlwapi.dll' name 'AssocQueryStringW';

function AssocQueryStringA(Flags: Integer; Str: Integer; pszAssoc, pszExtra, pszOut: PChar;
  var pcchOut: DWORD): HRESULT; stdcall; external 'shlwapi.dll' name 'AssocQueryStringA';

function GetDefaultBrowserWide: WideString;
const
  ASSOCF_NOTRUNCATE = $00000020;
  ASSOCSTR_EXECUTABLE = 2;
  Extension = '.htm';
var
  BufSize: DWORD;
begin
  BufSize := MAX_PATH;
  SetLength(Result, BufSize);
  if AssocQueryStringW(ASSOCF_NOTRUNCATE, ASSOCSTR_EXECUTABLE, PWChar(Extension), 'open', PWChar(Result), BufSize) = S_OK then
    SetLength(Result, BufSize - 1)
  else
    Result := '';
end;

function GetDefaultBrowserAnsi: AnsiString;
const
  ASSOCF_NOTRUNCATE = $00000020;
  ASSOCSTR_EXECUTABLE = 2;
  Extension = '.htm';
var
  BufSize: DWORD;
begin
  BufSize := MAX_PATH;
  SetLength(Result, BufSize);
  if AssocQueryStringA(ASSOCF_NOTRUNCATE, ASSOCSTR_EXECUTABLE, PChar(Extension), 'open', PChar(Result), BufSize) = S_OK then
    SetLength(Result, BufSize - 1)
  else
    Result := '';
end;
{$ENDIF WinCE}

function FindDefaultBrowserAnsi(out ABrowser, AParams: String): Boolean;
begin
  ABrowser := '';
  {$IFnDEF WinCE}
  ABrowser := GetDefaultBrowserAnsi;
  AParams := '"%s"';
  if (ABrowser = '') then
  begin
    if FindBrowserExecutable('rundll32', ABrowser) then
      AParams := 'url.dll,FileProtocolHandler %s';
  end;
  {$ENDIF}
  Result := (ABrowser <> '');
  if not Result then
  begin
    Result := FindPredefinedBrowser(ABrowser, AParams);
  end;
end;

function FindDefaultBrowser(out ABrowser, AParams: String): Boolean;
begin
  Result := FindDefaultBrowserAnsi(ABrowser, AParams);
end;

function FindDefaultBrowserWide(out ABrowser, AParams: WideString): Boolean;
var
  AnsiBrowser, AnsiParams: String;
begin
  ABrowser := '';
  {$IFnDEF WinCE}
  ABrowser := GetDefaultBrowserWide;
  AParams := '"%s"';
  if (ABrowser = '') then
  begin
    if FindBrowserExecutable('rundll32', AnsiBrowser) then
    begin
      AParams := 'url.dll,FileProtocolHandler %s';
      {$IFnDEF ACP_RTL}
      ABrowser := Utf8ToUTF16(AnsiBrowser);
      {$else}
      ABrowser := WideString(AnsiBrowser);
      {$ENDIF ACP_RTL}
    end;
  end;
  {$ENDIF}
  Result := (ABrowser <> '');
  if not Result then
  begin
    Result := FindPredefinedBrowser(AnsiBrowser, AnsiParams);
    if Result then
    begin
      {$IFnDEF ACP_RTL}
      ABrowser := Utf8ToUtf16(AnsiBrowser);
      AParams := Utf8ToUtf16(AnsiParams);
      {$else}
      ABrowser := WideString(AnsiBrowser);
      AParams := WideString(AnsiParams);
      {$ENDIF ACP_RTL}
    end;
  end;
end;


function FindDefaultBrowserAndOpenUrl(AURL: String): Boolean;
var
  ABrowser, AParams: WideString;
  H: HINST;
  AParamsUtf8: String;
begin
  Result := False;
  if AURL = '' then Exit;
  if FindDefaultBrowserWide(ABrowser, AParams)then
  begin
    if (Pos('%s', AParams) > 0) then
    begin
      {$IFnDEF ACP_RTL}
      AParamsUtf8 := Utf16ToUtf8(AParams);
      {$ELSE}
      AParamsUtf8 := AParams;
      {$ENDIF ACP_RTL}
      AParamsUtf8 := Format(AParamsUtf8,[AURL]);
      {$IFnDEF ACP_RTL}
      AParams := Utf8ToUtf16(AParamsUtf8);
      {$ELSE}
      AParams := WideString(AParamsUtf8);
      {$ENDIF ACP_RTL}
    end
    else
      {$IFnDEF ACP_RTL}
      AParams := Utf8ToUtf16(AURL);
      {$ELSE}
      AParams := WideString(AURL);
      {$ENDIF ACP_RTL}
    //this happens if ABrowser is rundll.exe
    if (Pos(#32, AParams) > 0) and (AParams[1] <> '"')
      then AParams := '"' + AParams + '"';
    H := ShellExecuteW(0, 'open', PWChar(ABrowser), PWChar(AParams), nil, SW_SHOWNORMAL);
  end
  else
  begin
    {$IFnDEF ACP_RTL}
    AParams := Utf8ToUtf16(AURL);
    {$ELSE}
    AParams := WideString(AURL);
    {$ENDIF ACP_RTL}
    H := ShellExecuteW(0, nil, PWideChar(AParams), nil, nil, SW_SHOWNORMAL) ;
  end;
  Result := (H > 32);
end;

// Open a given URL with whatever Windows thinks is appropriate
function OpenURL(AURL: String): Boolean;
var
{$IFDEF WinCE}
  Info: SHELLEXECUTEINFO;
{$ELSE}
  ws: WideString;
  ans: AnsiString;
  IsFileUriWithSpaces, IsFileURI: Boolean;
const
  FileURIScheme = 'file://';

  function IsHtmlWithAnchor(S: String): Boolean;
  var
    AnchorPos, HtmlPos: SizeInt;
  begin
    Result := False;
    //Anchor will be defined by last '#' in AURL;
    AnchorPos := Length(AURL);
    while (AnchorPos < 0) and (S[AnchorPos] <> '#') do Dec(AnchorPos);
    if (AnchorPos > 0) then
    begin
      S := UpperCase(S); //don't care about UTF8
      HtmlPos := Pos('.HTM', S);
      if (HtmlPos = 0) then HtmlPos := Pos('.HTML', S);
      Result := (HtmlPos > 0) and (AnchorPos > HtmlPos);
    end;
  end;
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
  IsFileURI := (CompareText(Copy(AURL,1,Length(FileURIScheme)), FileURIScheme) = 0);
  //Html FileURI's that have a local anchor cannot be opened via a direct call to ShellExecute,
  //in that case we need to find the actual default browser and execute that.
  //Notice that this will still fail to open the html at the correct anchor
  //if FindDefaultBrowserWide returns 'rundll.exe'
  //See: issue #0030326 and related
  if IsFileURI and IsHtmlWithAnchor(AURL) then
    Result := FindDefaultBrowserAndOpenURL(AURL)
  else
  begin
    //Urls that start with file:// are allowed to contain spaces and should be quoted
    //Since on Windows filenames cannot contain the " character, we need not care about it and simply enclose the AURL
    IsFileUriWithSpaces := IsFileURI and (Pos(#32,AURL) > 0);
    if IsFileUriWithSpaces then AURL := '"' + AURL + '"';
    ws := UTF8Decode(AURL);
    Result := ShellExecuteW(0, nil, PWideChar(ws), nil, nil, SW_SHOWNORMAL) > 32;
  end;
  {$ENDIF}
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
begin
  Result := OpenURL(APath);
end;
