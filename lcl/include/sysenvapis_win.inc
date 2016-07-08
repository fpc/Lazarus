{%MainUnit ../lclintf.pas}

{$I ../../components/lazutils/lazutils_defines.inc} //LCL depends on LazUtils, so this is OK

{$IFnDEF WinCE}
const
  ASSOCSTR_COMMAND = 1;
  //ASSOCSTR_EXECUTABLE = 2;
  //ASSOCSTR_FRIENDLYDOCNAME = 3;
  //ASSOCSTR_FRIENDLYAPPNAME = 4;
  //ASSOCSTR_NOOPEN = 5;
  //ASSOCSTR_SHELLNEWVALUE = 6;
  //ASSOCSTR_DDECOMMAND = 7;
  //ASSOCSTR_DDEIFEXEC = 8;
  //ASSOCSTR_DDEAPPLICATION = 9;
  //ASSOCSTR_DDETOPIC = 10;
  //ASSOCSTR_INFOTIP = 11;
  //ASSOCSTR_QUICKTIP = 12;
  //ASSOCSTR_TILEINFO = 13;
  //ASSOCSTR_CONTENTTYPE = 14;
  //ASSOCSTR_DEFAULTICON = 15;
  //ASSOCSTR_SHELLEXTENSION = 16;
  //ASSOCSTR_DROPTARGET = 17;
  //ASSOCSTR_DELEGATEEXECUTE = 18;
  //ASSOCSTR_SUPPORTED_URI_PROTOCOLS = 19;
  //ASSOCSTR_PROGID = 20;
  ASSOCSTR_APPID = 21;
  //ASSOCSTR_APPPUBLISHER = 22;
  //ASSOCSTR_APPICONREFERENCE = 23;
  //ASSOCSTR_MAX = ;

  //ASSOCF_NONE                  = $00000000;
  //ASSOCF_INIT_NOREMAPCLSID     = $00000001;
  //ASSOCF_INIT_BYEXENAME        = $00000002;
  //ASSOCF_OPEN_BYEXENAME        = $00000002;
  //ASSOCF_INIT_DEFAULTTOSTAR    = $00000004;
  //ASSOCF_INIT_DEFAULTTOFOLDER  = $00000008;
  //ASSOCF_NOUSERSETTINGS        = $00000010;
  ASSOCF_NOTRUNCATE            = $00000020;
  //ASSOCF_VERIFY                = $00000040;
  //ASSOCF_REMAPRUNDLL           = $00000080;
  //ASSOCF_NOFIXUPS              = $00000100;
  //ASSOCF_IGNOREBASECLASS       = $00000200;
  //ASSOCF_INIT_IGNOREUNKNOWN    = $00000400;
  //ASSOCF_INIT_FIXED_PROGID     = $00000800;
  //ASSOCF_IS_PROTOCOL           = $00001000;
  //ASSOCF_INIT_FOR_FILE         = $00002000;

const
  //List of WinAppBrwosers (Win 10) that are capable of handling local filenames with anchors
  //Strings must be in uppercase
  //The string must be the "easy part" that can be detected in a AppUserModelID like
  //shell:AppsFolder\Microsoft.MicrosoftEdge_8wekyb3d8bbwe!MicrosoftEdge
  //Currently Edge is the only one that can handle this, but others may follow
  CapableWinAppBrowsers: Array[1..1] of WideString = (
    'MICROSOFTEDGE'
    );

function AssocQueryStringW(Flags: Integer; Str: Integer; pszAssoc, pszExtra, pszOut: PWChar;
  var pcchOut: DWORD): HRESULT; stdcall; external 'shlwapi.dll' name 'AssocQueryStringW';

function IsLaunchWinApp(ABrowser: WideString): Boolean;
begin
  Result := (Pos('LAUNCHWINAPP.EXE', WideUpperCase(ABrowser)) > 0)
end;

//not every AppUserModelID we retrieve using GetDefaultBrowserWideByAppID
//accepts paramters (e.g. the URL)
function LaunchWinAppBrowserCanHandleParams(ABrowser: WideString): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(CapableWinAppBrowsers) to High(CapableWinAppBrowsers) do
    if (Pos(CapableWinAppBrowsers[i], WideUpperCase(ABrowser)) > 0) then Exit(True);
end;

function GetDefaultBrowserWideByAppID: WideString;
const
  Extension = '.htm';
var
  BufSize: DWORD;
begin
  BufSize := MAX_PATH;
  SetLength(Result, BufSize);
  if AssocQueryStringW(ASSOCF_NOTRUNCATE, ASSOCSTR_APPID, PWChar(Extension), 'open', PWChar(Result), BufSize) = S_OK then
    SetLength(Result, BufSize - 1)
  else
    Result := '';
  if (Result <> '') then
    Result := 'shell:AppsFolder\' + Result;
end;

function GetDefaultBrowserWideByCmd: WideString;
const
  Extension = '.htm';
var
  BufSize: DWORD;
begin
  BufSize := MAX_PATH;
  SetLength(Result, BufSize);
  if AssocQueryStringW(ASSOCF_NOTRUNCATE, ASSOCSTR_COMMAND, PWChar(Extension), 'open', PWChar(Result), BufSize) = S_OK then
    SetLength(Result, BufSize - 1)
  else
    Result := '';
end;


procedure ExtractBrowserAndParamsWide(const S: WideString; out ABrowser, AParams: WideString);
var
  P: Integer;
begin
  ABrowser := S;
  AParams := '%s';
  if length(S) < 4 then Exit; //minimal executable name: a.exe
  if S[1] = '"' then
  begin
    P := 2;
    while (P <= length(S)) and (S[P] <> '"') do Inc(P);
    if P > length(S) then Exit;  //malformed string: "abc foo bar
    ABrowser := Copy(S, 1, P);
    AParams := Trim(Copy(S, P+1, MaxInt));
  end
  else
  begin
    P := Pos(#32,S);
    if (P = 0) then
    begin
      ABrowser := S;
      AParams := '"%s"';
    end
    else
    begin
      ABrowser := Copy(S, 1, P-1);
      AParams := Trim(Copy(S, P+1, MaxInt));
    end;
  end;
  AParams := Utf16StringReplace(AParams, '%1', '%s', []);
end;
{$ENDIF WinCE}

function FindDefaultBrowserWide(out ABrowser, AParams: WideString): Boolean;
var
  AnsiBrowser, AnsiParams: String;
  QueryRes, SavedBrowser, SavedParams: WideString;
begin
  ABrowser := '';
  AParams := '"%s"';
  {$IFnDEF WinCE}
  QueryRes := GetDefaultBrowserWideByCmd;
  if (QueryRes = '') then
  begin
    if FindBrowserExecutable('rundll32', AnsiBrowser) then
    begin
      AParams := 'url.dll,FileProtocolHandler "%s"';
      {$IFnDEF ACP_RTL}
      ABrowser := Utf8ToUTF16(AnsiBrowser);
      {$else}
      ABrowser := WideString(AnsiBrowser);
      {$ENDIF ACP_RTL}
    end
  end
  else
  begin
    ExtractBrowserAndParamsWide(QueryRes, ABrowser, AParams);
    // On Windows 10, the default loading of files is done by LaunchWinApp. It calls
    // the linked default program. We have to find it and use it, without quotation marks!
    // See http://bugs.freepascal.org/view.php?id=30326
    // Till now, only Edge is working correct
    if IsLaunchWinApp(ABrowser) then
    begin
      SavedBrowser := ABrowser;
      SavedParams := AParams;
      ABrowser := GetDefaultBrowserWideByAppID;
      if LaunchWinAppBrowserCanHandleParams(ABrowser) then
        AParams := '%s' //Edge seems to require that AParams is NOT double quoted
      else
      begin // not MS Edge (or compatible w.r.t. arguments)
        ABrowser := SavedBrowser;
        AParams  := SavedParams;
      end;
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

function FindDefaultBrowserUtf8(out ABrowser, AParams: String): Boolean;
var
  QueryRes: String;
  WideBrowser, WideParams: WideString;
begin
  Result := FindDefaultBrowserWide(WideBrowser, WideParams);
  ABrowser := Utf16ToUtf8(WideBrowser);
  AParams := Utf16ToUtf8(WideParams);
end;

function FindDefaultBrowser(out ABrowser, AParams: String): Boolean;
begin
  Result := FindDefaultBrowserUtf8(ABrowser, AParams);
  {$IFDEF ACP_RTL}
  ABrowser := Utf8ToWinCp(ABrowser);
  AParams := Utf8ToWinCp(AParams);
  {$ENDIF ACP_RTL}
end;

{$IFnDEF WinCE}
function IsFileUriScheme(const AURL: String): Boolean;
const
  FileURIScheme = 'file://';
begin
  Result := (CompareText(Copy(AURL,1,Length(FileURIScheme)), FileURIScheme) = 0);
end;

function IsHtmlWithAnchor(AURL: String): Boolean;
var
  AnchorPos, HtmlPos: SizeInt;
begin
  Result := False;
  //Anchor will be defined by last '#' in AURL;
  AnchorPos := Length(AURL);
  while (AnchorPos < 0) and (AURL[AnchorPos] <> '#') do Dec(AnchorPos);
  if (AnchorPos > 0) then
  begin
    AURL := UpperCase(AURL); //don't care about UTF8
    HtmlPos := Pos('.HTM', AURL);
    if (HtmlPos = 0) then HtmlPos := Pos('.HTML', AURL);
    Result := (HtmlPos > 0) and (AnchorPos > HtmlPos);
  end;
end;

//Currently only used to open a local html file with a specified anchor
//but in theory should be able to handle all URL's
function FindDefaultBrowserAndOpenUrl(AURL: String; IsFileURI: Boolean=False{; IsLocalWithAnchor: Boolean=False}): Boolean;
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
      //MS IE returns quoted or unquoted %s, depending on version and OS
      //file:// needs to be quoted if filename contains spaces
      if (Pos('"%s"', AParams) = 0) and IsFileUri and (not LaunchWinAppBrowserCanHandleParams(ABrowser)) then
        AURL := '"'+ AURL + '"';
      //at least FireFox does not like -url -osint "%s" for local files, it wants "%s"
      //if IsFileUri and IsLocalWithAnchor then
      //  AParams := '"%s"';
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
    begin
      //file:// needs to be quoted if filename contains spaces
      if IsFileURI and (Pos(#32, AURL) > 0) {and (not LaunchWinAppBrowserCanHandleParams(ABrowser))} then
        AURL := '"' + AURL + '"';
      {$IFnDEF ACP_RTL}
      AParams := Utf8ToUtf16(AURL);
      {$ELSE}
      AParams := WideString(AURL);
      {$ENDIF ACP_RTL}
    end;

    //debugln('FindDefaultBrowserAndOpenUrl:');
    //debugln(['  ABrowser = ',ABrowser]);
    //debugln(['  AParams  = ',AParams]);
    H := ShellExecuteW(0, 'open', PWChar(ABrowser), PWChar(AParams), nil, SW_SHOWNORMAL);
  end  //FindDefaultBrowserWide
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
{$ENDIF WinCE}

// Open a given URL with whatever Windows thinks is appropriate
function OpenURL(AURL: String): Boolean;
var
{$IFDEF WinCE}
  Info: SHELLEXECUTEINFO;
{$ELSE}
  ws: WideString;
  ans: AnsiString;
  IsFileUriWithSpaces, IsFileURI: Boolean;
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
  IsFileURI := IsFileUriScheme(AURL);
  //Html FileURI's that have a local anchor cannot be opened via a direct call to ShellExecute,
  //in that case we need to find the actual default browser and execute that.
  //Notice that this will still fail to open the html at the correct anchor
  //if FindDefaultBrowserWide returns 'rundll.exe'
  //See: issue #0030326 and related
  if IsFileURI and IsHtmlWithAnchor(AURL) then
    Result := FindDefaultBrowserAndOpenURL(AURL, True{, True})
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
