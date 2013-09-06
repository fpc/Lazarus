
var
  wpAskConfDir: TInputDirWizardPage;

  // Additional Elements on TargetDir wizard page
  CheckSecondInstall: TCheckBox;  //   Also used by GetAppId
  CheckSecondLabel: TLabel;

  CfgLoadedFromDir: String;  // The directory from which lazarus was uninstalled
  CFGFileForLoadedFromDir: TStringList;
  CFGPathForLoadedFromDir: String; // the PCP
  CFGStateForLoadedFromDir: TCfgFileState;

Procedure ClearExistingConfigForFolder;
begin
  CfgLoadedFromDir := '';
  if CFGFileForLoadedFromDir <> nil then
    CFGFileForLoadedFromDir.Clear;
  CFGPathForLoadedFromDir := '';
  CFGStateForLoadedFromDir := csNoFile;
end;

Procedure LoadExistingConfigForFolder(AFolder: String);
begin
  CfgLoadedFromDir := AFolder;
  LoadCFGFile(AFolder, CFGFileForLoadedFromDir);
  CFGStateForLoadedFromDir := ParseCFGList(CFGFileForLoadedFromDir, CFGPathForLoadedFromDir);
end;

function HasConfigLoadedFromDir(AFolder: String; FallBackToUninstallDir: Boolean): Boolean;
begin
  Result := (CfgLoadedFromDir = AFolder) and
            (CFGFileForLoadedFromDir <> nil) and
            (CFGFileForLoadedFromDir.Count > 0); // only if content
  if (not Result) and FallBackToUninstallDir then
    Result := HasSavedConfigFromUninstall(AFolder);
end;

// Did the loadedconf contain a pcp?
function HasPCPLoadedFromDir(AFolder: String; FallBackToUninstallDir: Boolean): Boolean;
begin
  Result := False;
  if HasConfigLoadedFromDir(AFolder, False) then
    Result :=  CFGPathForLoadedFromDir <> ''
  else
  if FallBackToUninstallDir and HasSavedConfigFromUninstall(AFolder) then
    Result := GetSavedPCPFromUninstall(AFolder) <> '';
end;

function GetConfigLoadedFromDir(AFolder: String; FallBackToUninstallDir: Boolean): TStringList;
begin
  Result := nil;
  if HasConfigLoadedFromDir(AFolder, False) then
    Result :=  CFGFileForLoadedFromDir
  else
  if FallBackToUninstallDir and HasSavedConfigFromUninstall(AFolder) then
    Result := GetSavedConfigFromUninstall(AFolder);
end;

function GetPCPLoadedFromDir(AFolder: String; FallBackToUninstallDir: Boolean): String;
begin
  Result := '';
  if HasConfigLoadedFromDir(AFolder, False) then
    Result :=  CFGPathForLoadedFromDir
  else
  if FallBackToUninstallDir and HasSavedConfigFromUninstall(AFolder) then
    Result := GetSavedPCPFromUninstall(AFolder);
end;

function GetStateLoadedFromDir(AFolder: String; FallBackToUninstallDir: Boolean): TCfgFileState;
begin
  Result := csNoFile;
  if HasConfigLoadedFromDir(AFolder, False) then
    Result :=  CFGStateForLoadedFromDir
  else
  if FallBackToUninstallDir and HasSavedConfigFromUninstall(AFolder) then
    Result := GetSavedStateFromUninstall(AFolder);
end;



Procedure AddSecondaryCheckBoxToTargetDirWizzard;
begin
  if (CheckSecondInstall <> nil) then
    exit;

  WizardForm.DirEdit.Parent.Handle;

  CheckSecondInstall := TCheckBox.Create(WizardForm);
  AddComponentToPage(CheckSecondInstall, WizardForm.DirEdit, 10, 0, 1, 0);
  CheckSecondInstall.Caption := CustomMessage('CheckSecondClick');

  CheckSecondLabel := TLabel.Create(WizardForm);
  AddComponentToPage(CheckSecondLabel, CheckSecondInstall, 10, 0, 1, -10);
  CheckSecondLabel.AutoSize := False;
  CheckSecondLabel.WordWrap := True;
  CheckSecondLabel.Caption := CustomMessage('CheckSecondInfo');
end;

procedure CreateSecondaryConfFolderAndNameWizardPage;
begin
  wpAskConfDir := CreateInputDirPage(
    wpSelectDir,
    CustomMessage('SecondConfCapt'),
    CustomMessage('SecondConfCapt2'),
    CustomMessage('SecondConfBody'),
    False,
    'laz_conf'
  );
  wpAskConfDir.Add(CustomMessage('FolderForConfig'));
end;

function IsSecondaryCheckBoxChecked: Boolean;
begin
  Result := (CheckSecondInstall <> nil) and (CheckSecondInstall.Checked);
end;

Procedure CreateOrSaveConfigFile;
var
  CfgFile: TStringList;
begin
  if not (IsSecondaryCheckBoxChecked or IsSecondaryUpdate) then
    exit;

  if IsSecondaryCheckBoxChecked then begin
    CfgFile := GetConfigLoadedFromDir(WizardDirValue, True);
    if (GetPCPLoadedFromDir(WizardDirValue, True) <> SecondPCP) or (CfgFile = nil) then
      CreateCFGFile(SecondPCP, CfgFile);

    if (SecondPCP <> '') then
      try
        CfgFile.SaveToFile(AddBackslash(WizardDirValue) + 'lazarus.cfg')
        ForceDirectories(SecondPCP);
      except
        MsgBox('Internal Error (1): Could not save CFG for secondary install', mbConfirmation, MB_OK);
      end
    else begin
      MsgBox('Internal Error (2): Could not save CFG for secondary install', mbConfirmation, MB_OK);
    end;
  end

  else
  // NO checkbox.checked
  if (DidRunUninstaller) and (IsSecondaryUpdate) and
     (not FileExists(AddBackslash(WizardDirValue) + 'lazarus.cfg'))
  then begin
    // cfg was uninstalled / restore
    CfgFile := GetConfigLoadedFromDir(WizardDirValue, True);

    if (CfgFile <> nil) then
      try
        CfgFile.SaveToFile(AddBackslash(WizardDirValue) + 'lazarus.cfg')
      except
        MsgBox('Internal Error (3): Could not restore CFG for secondary install', mbConfirmation, MB_OK);
      end
    else
    begin
      MsgBox('Internal Error (5): Could not restore CFG for secondary install', mbConfirmation, MB_OK);
    end;
  end

  else
// NO checkbox.checked
  if (IsSecondaryUpdate) and
    (not FileExists(AddBackslash(WizardDirValue) + 'lazarus.cfg'))
  then begin
    // where is the config gone ???????
      MsgBox('Internal Error (4): Pre-Existing Configfile was removed?', mbConfirmation, MB_OK);
  end
  ;
end;

