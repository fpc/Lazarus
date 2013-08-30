
var
  wpAskConfDir: TInputDirWizardPage;

Procedure AddSecondaryCheckBoxToTargetDirWizzard;
begin
  if (CheckSecondInstall <> nil) then
    exit;

  WizardForm.DirEdit.Parent.Handle;

  CheckSecondInstall := TCheckBox.Create(WizardForm);
  CheckSecondInstall.Parent:=WizardForm.DirEdit.Parent;
  CheckSecondInstall.Top := WizardForm.DirEdit.Top + WizardForm.DirEdit.Height + 10;
  CheckSecondInstall.Left := WizardForm.DirEdit.Left;
  CheckSecondInstall.Width := WizardForm.DirEdit.Parent.Width - WizardForm.DirEdit.Left;
  CheckSecondInstall.Caption := CustomMessage('CheckSecondClick');

  CheckSecondLabel := TLabel.Create(WizardForm);
  CheckSecondLabel.Parent:=WizardForm.DirEdit.Parent;
  CheckSecondLabel.AutoSize := False;
  CheckSecondLabel.WordWrap := True;
  CheckSecondLabel.Top := CheckSecondInstall.Top + CheckSecondInstall.Height + 10;
  CheckSecondLabel.Left := WizardForm.DirEdit.Left;
  CheckSecondLabel.Width := WizardForm.DirEdit.Parent.Width - WizardForm.DirEdit.Left;
  CheckSecondLabel.Height := WizardForm.DirEdit.Parent.Height - CheckSecondLabel.Top - 15;
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
  wpAskConfDir.Add('Folder for config');
end;

Procedure CreateOrSaveConfigFile;
begin
  if (CheckSecondInstall <> nil) and (CheckSecondInstall.Checked) then begin
    if (NewCFGFile <> nil) then
      try
        NewCFGFile.SaveToFile(AddBackslash(WizardDirValue) + 'lazarus.cfg')
        ForceDirectories(SecondPCP);
      except
        MsgBox('Internal Error (1): Could not save CFG for secondary install', mbConfirmation, MB_OK);
      end
    else begin
      MsgBox('Internal Error (2): Could not save CFG for secondary install', mbConfirmation, MB_OK);
    end;
  end
  else
  if (UninstallDoneState <> uiUnknown) and (IsSecondaryUpdate) and
     (not FileExists(AddBackslash(WizardDirValue) + 'lazarus.cfg'))
  then begin
    // cfg was uninstalled / restore
    if (NewCFGFile <> nil) then
      try
        NewCFGFile.SaveToFile(AddBackslash(WizardDirValue) + 'lazarus.cfg')
      except
        MsgBox('Internal Error (3): Could not restore CFG for secondary install', mbConfirmation, MB_OK);
      end
    else
    if (UninstDir = WizardDirValue) and (CFGFileForUninstDir <> nil) and
       (CFGFileForUninstDir.count > 0)
    then begin
      try
        CFGFileForUninstDir.SaveToFile(AddBackslash(WizardDirValue) + 'lazarus.cfg')
      except
        MsgBox('Internal Error (4): Could not restore CFG for secondary install', mbConfirmation, MB_OK);
      end
    end
    else begin
      MsgBox('Internal Error (5): Could not restore CFG for secondary install', mbConfirmation, MB_OK);
    end;
  end;
end;

