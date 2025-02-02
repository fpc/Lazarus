{%MainUnit ../lclintf.pas}


function FindDefaultBrowser(out ABrowser, AParams: String): Boolean;
begin
  Result := FindPredefinedBrowser(ABrowser, AParams);
end;

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

function OpenDocument(APath: String): Boolean;
var
  lApp: string;
begin
  // Android uses this
  if Assigned(OpenDocumentWidgetsetImplementation) then
    Exit(OpenDocumentWidgetsetImplementation(APath));

  Result := True;
  if not (FileExists(APath) or DirectoryExists(APath)) then
    Exit(false);

  lApp:=FindFilenameOfCmd('xdg-open'); // Portland OSDL/FreeDesktop standard on Linux
  if lApp='' then
    // Does not work. Tested in Manjaro Linux + KDE. Could be removed.
    lApp:=FindFilenameOfCmd('kfmclient'); // KDE command
  if lApp='' then
    // Does this work? Please test somebody.
    lApp:=FindFilenameOfCmd('gnome-open'); // GNOME command
  if lApp='' then
    Exit(False);

  if (APath<>'') and (APath[1]<>'"') then
    APath:=APath.QuotedString('"');
  RunCmdFromPath(lApp,APath);
end;

function SelectInFolder(AFullPath: String): Boolean;
var
  lApp, DesktopEnv: string;
  // Parameters for a filename or directory, and for filemanager selection.
  FileParam, SelParam: string;

  procedure SelectNautilus;
  begin
    lApp := 'nautilus';
    SelParam := '--select ';
  end;

  procedure SelectDolphin;
  begin
    lApp := 'dolphin';
    SelParam := '--select ';
  end;

begin
  if not (FileExists(AFullPath) or DirectoryExists(AFullPath)) then
    Exit(false);
  FileParam := AFullPath;
  SelParam := '';
  DesktopEnv := GetEnvironmentVariable('XDG_CURRENT_DESKTOP');

  //pcmanfm does not have a --select option i can find but may be another way
  //caja has a --select option but i cannot work it out.  keeps trying to open file as a path issue
  //try to get desktop env first.  but could be any file manager... seems like a good order
  if Pos('GNOME', DesktopEnv) > 0 then
    SelectNautilus
  else if Pos('KDE', DesktopEnv) > 0 then
    SelectDolphin
  else if FileExists('/usr/bin/nautilus') then
    SelectNautilus
  else if FileExists('/usr/bin/dolphin') then
    SelectDolphin
  else if FileExists('/usr/bin/thunar') then
    lApp := 'thunar'                  // Unable to select but opens directory
  else if FileExists('/usr/bin/nemo') then
    lApp := 'nemo'
  else if FileExists('/usr/bin/caja') then
    // Can't figure out the --select option which help says is there but does not behave
    lApp := 'caja'
  else if FileExists('/usr/bin/krusader') then
  begin
    // Not possible to select the file but better open the dir in the left pane
    lApp := 'krusader';
    SelParam := '--left ';
    FileParam := ExtractFilePath(AFullPath); // Just the directory
  end
  else begin  // No known file manager found. Trying xdg-open for the directory
    lApp := FindFilenameOfCmd('xdg-open');
    FileParam := ExtractFilePath(AFullPath);
  end;

  if (FileParam <> '') and (FileParam[1] <> '"') then
    FileParam := FileParam.QuotedString('"');
  RunCmdFromPath(lApp, SelParam + FileParam, []);
  Result := True;
end;

