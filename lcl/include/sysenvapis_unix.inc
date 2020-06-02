{%MainUnit ../lclintf.pas}


function FindDefaultBrowser(out ABrowser, AParams: String): Boolean;
begin
  Result := FindPredefinedBrowser(ABrowser, AParams);
end;

// Open a given URL with the default browser
function OpenURL(AURL: String): Boolean;
var
  ABrowser, AParams: String;
begin
  // Android uses this
  if Assigned(OpenURLWidgetsetImplementation) then
    Exit(OpenURLWidgetsetImplementation(AURL));

  Result := FindDefaultBrowser(ABrowser, AParams) and
            FileExistsUTF8(ABrowser) and FileIsExecutable(ABrowser);
  if not Result then
    Exit;
  RunCmdFromPath(ABrowser,Format(AParams, [AURL]));
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
var
  lApp: string;
begin
  // Android uses this
  if Assigned(OpenDocumentWidgetsetImplementation) then
    Exit(OpenDocumentWidgetsetImplementation(APath));

  Result := True;
  if not (FileExistsUTF8(APath) or DirectoryExistsUTF8(APath)) then
    Exit(false);

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
