{%MainUnit ../lclintf.pas}

{$I ../../components/lazutils/lazutils_defines.inc} //LCL depends on LazUtils, so this is OK


function IsLaunchWinApp(ABrowser: WideString): Boolean;
begin
  Result := False;
end;

//not every AppUserModelID we retrieve using GetDefaultBrowserWideByAppID
//accepts paramters (e.g. the URL)
function LaunchWinAppBrowserCanHandleParams(ABrowser: WideString): Boolean;
begin
  Result := False;
end;

function GetDefaultBrowserWideByAppID: WideString;
begin
  Result := '';
end;

function GetDefaultBrowserWideByCmd: WideString;
begin
  Result := '';
end;


procedure ExtractBrowserAndParamsWide(const S: WideString; out ABrowser, AParams: WideString);
begin
  ABrowser := S;
  AParams := '%s';
end;


function FindDefaultBrowserWide(out ABrowser, AParams: WideString): Boolean;
begin
  ABrowser := '';
  AParams := '"%s"';
  Result := False;
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
begin
  Result := False;
end;

// Open a given URL with whatever Windows thinks is appropriate
function OpenURL(AURL: String): Boolean;
begin
  Result := False;
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
begin
  Result := OpenURL(APath);
end;
