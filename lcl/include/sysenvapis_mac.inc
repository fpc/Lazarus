{%MainUnit ../lclintf.pas}


function FindDefaultBrowser(out ABrowser, AParams: String): Boolean;
begin
  // open command launches url in the appropriate browser under Mac OS X
  FindBrowserExecutable('open', ABrowser);
  AParams := '"%s"';
  Result := (ABrowser <> '');
  if not Result then
  begin
    Result := FindPredefinedBrowser(ABrowser, AParams);
  end;
end;

// Open a given URL with the default browser
function OpenURL(AURL: String): Boolean;
var
  url: NSURL;
  ws: NSWorkspace;
begin
  Result := False;
  if AURL = '' then
    Exit(False);
  url := NSURL.URLWithString(NSStr(@AURL[1]));
  if not Assigned(url) then
    Exit(False);
  ws := NSWorkspace.sharedWorkspace;
  Result := ws.openURL(url);
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
var
  ResultingPath: string;
begin
  Result := True;
  if not FileExistsUTF8(APath) then exit(false);
  // Paths with spaces need to be quoted, see bug 21651
  if (APath<>'') and (APath[1]<>'''') then
    ResultingPath:=QuotedStr(APath)
  else
    ResultingPath:=APath;
  RunCmdFromPath('open',ResultingPath);
end;
