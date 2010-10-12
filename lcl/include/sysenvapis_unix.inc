{%MainUnit ../lclintf.pp}

// Open a given URL with the default browser
function OpenURL(AURL: String): Boolean;
var
  ABrowser, AParams: String;
begin
  Result := FindDefaultBrowser(ABrowser, AParams) and FileExistsUTF8(ABrowser) and FileIsExecutable(ABrowser);
  if not Result then
    Exit;
  RunCmdFromPath(ABrowser,Format(AParams, [AURL]));
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
var
  lApp: string;
begin
  Result := True;
  if not FileExistsUTF8(APath) then exit(false);

  lApp:=FindFilenameOfCmd('xdg-open'); // Portland OSDL/FreeDesktop standard on Linux
  if lApp='' then
    lApp:=FindFilenameOfCmd('kfmclient'); // KDE command
  if lApp='' then
    lApp:=FindFilenameOfCmd('gnome-open'); // GNOME command
  if lApp='' then
    Exit(False);

  if (APath<>'') and (APath[1]<>'"') then
    APath:=QuotedStr(APath);
  RunCmdFromPath(lApp,APath);
end;
