
type
  TUninstallState = (
    uiUnknown,
    UIDone,         // There IS no uninstaller, OR it was already executed during this install
    UIOtherNeeded,  // The uninstaller ('Inno Setup: App Path') points to a different Path than "WizardFolder"
                    // Uninstall for OTHER folder NEEDED
    uiDestNeeded,   // Uninstaller for "WizardFolder" found
                    // Uninstall for DESTINATION folder NEEDED
    uiInconsistent  // Path of uninstaller and lazarus to be removed, do not match
  );

var
  UninstallState: TUninstallState;
  UninstallDoneState: TUninstallState; // Set only if uninstall was executed
  UnInstallerInAppPath: Boolean; // The uninstaller is in the directory that it will remove

  OldPath,              // Registry 'Inno Setup: App Path'
  OldName,              // Registry 'DisplayName'
  UnInstaller: String;  // Registry 'UninstallString'
  PathEqual: Boolean;

  UninstDir: String;  // The directory from which lazarus was uninstalled
  CFGFileForUninstDir: TStringList;
  CFGPathForUninstDir: String; // the PCP
  CFGStateForUninstDir: TCfgFileState;

var
  wpAskUnistall: TWizardPage;
  wpLabel1, wpLabel2, wpLabel3, wpLabel4: TNewStaticText;
  wpCheckBox: TNewCheckBox;
  wpButton: TNewButton;

function dbgsUiState(u: TUninstallState): String;
begin
  case u of
    uiUnknown:      Result := 'uiUnknown';
    UIDone:         Result := 'UIDone';
    UIOtherNeeded:  Result := 'UIOtherNeeded';
    uiDestNeeded:   Result := 'uiDestNeeded';
    uiInconsistent: Result := 'uiInconsistent';
  end;
end;

function DidRunUninstaller: Boolean;
begin
  Result := (UninstallDoneState <> uiUnknown);
end;

// check if: unistall was run, and run in AFolder, and did have a lazarus.cfg file
function HasSavedConfigFromUninstall(AFolder: String): Boolean;
begin
  Result := DidRunUninstaller and
            (UninstDir = AFolder) and
            (CFGFileForUninstDir <> nil) and
            (CFGFileForUninstDir.Count > 0); // only if content
end;

function GetSavedConfigFromUninstall(AFolder: String): TStringList;
begin
  Result := nil;
  if HasSavedConfigFromUninstall(AFolder) then
    Result :=  CFGFileForUninstDir;
end;

function GetSavedPCPFromUninstall(AFolder: String): String;
begin
  Result := '';
  if HasSavedConfigFromUninstall(AFolder) then
    Result :=  CFGPathForUninstDir;
end;

function GetSavedStateFromUninstall(AFolder: String): TCfgFileState;
begin
  Result := csNoFile;
  if HasSavedConfigFromUninstall(AFolder) then
    Result :=  CFGStateForUninstDir;
end;

function GetUninstallData(ARegName: String): String; // Get one entry from registry e.g. 'UninstallString'
var
  Path: String;
begin
  Path := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\'+GetAppId('')+'_is1');
  Result := '';
  if not RegQueryStringValue(HKLM, Path, ARegName, Result) then
    RegQueryStringValue(HKCU, Path, ARegName, Result);
end;

procedure InitializeUninstallInfo;
begin
  UninstallState := uiUnknown;
  UninstallDoneState := uiUnknown;
end;

procedure UpdateUninstallInfo;
begin
  Log('Enter UninstallState '+dbgsUiState(UninstallState));
  OldPath := '';
  OldName := '';
  UnInstaller := '';
  PathEqual := False;
  if UninstallState = uiDone then exit;

  UnInstaller := RemoveQuotes(GetUninstallData('UninstallString'));
  Log(' UnInstaller:  '+UnInstaller);  
  if (UnInstaller <> '') and FileExists(UnInstaller) then
  begin
    OldPath := RemoveQuotes((GetUninstallData('Inno Setup: App Path')));
	OldName := GetUninstallData('DisplayName');

    PathEqual := (OldPath <> '') and
                 (CompareText(RemoveBackslashUnlessRoot(OldPath), RemoveBackslashUnlessRoot(WizardDirValue)) = 0);
    Log(' OldPath: '+OldPath+'  OldName: '+ OldName);
	if PathEqual then
      UninstallState := uiDestNeeded
	else
      UninstallState := uiOtherNeeded;

    UnInstallerInAppPath := (CompareText(RemoveBackslashUnlessRoot(OldPath), RemoveBackslashUnlessRoot(ExtractFilePath(UnInstaller))) = 0);
    if (not UnInstallerInAppPath) and
       ( (CompareText(RemoveBackslashUnlessRoot(OldPath), RemoveBackslashUnlessRoot(WizardDirValue)) = 0) or
         (CompareText(RemoveBackslashUnlessRoot(ExtractFilePath(UnInstaller)), RemoveBackslashUnlessRoot(WizardDirValue)) = 0)
       )
    then
      UninstallState := uiInconsistent;

  end
  else
  begin
	if (IsSecondaryCheckBoxChecked) or IsSecondaryUpdate then
    begin
	  if ForcePrimaryAppId then begin
	    Log('UpdateUninstallInfo recursion detected');
	    UninstallState := uiInconsistent;
	    exit;
	  end;
      ForcePrimaryAppId := True;
      Log('REDO UninstallState '+GetUninstallData('Inno Setup: App Path')+' // '+WizardDirValue);
      if CompareText(RemoveBackslashUnlessRoot(RemoveQuotes(GetUninstallData('Inno Setup: App Path'))),
           RemoveBackslashUnlessRoot(WizardDirValue)) = 0
      then
        UpdateUninstallInfo // use the plain installer
      else
        UninstallState := uiDone;
      ForcePrimaryAppId := False;
    end
    else
      UninstallState := uiDone;
  end;

  Log('UninstallState is now '+dbgsUiState(UninstallState)+', OldPath='+OldPath+' OldName='+OldName+' UnInstaller='+UnInstaller);
end;

// *** Display/Use Wizzard Page

procedure InitAskUninstall(s1, s2, s3, s4: String);
var
  y: integer;
begin
  wpLabel1.Caption := s1;
  wpLabel2.Caption := s2;
  wpLabel3.Caption := s3;
  wpLabel4.Caption := s4;

  wpLabel1.AdjustHeight;
  wpLabel2.AdjustHeight;
  wpLabel3.AdjustHeight;
  wpLabel4.AdjustHeight;

  wpLabel2.Top := wpLabel1.Top + wpLabel1.Height + ScaleY(5);
  wpLabel3.Top := wpLabel2.Top + wpLabel2.Height + ScaleY(5);
  wpLabel4.Top := wpLabel3.Top + wpLabel3.Height + ScaleY(5);
  y := wpLabel4.Top + wpLabel4.Height + ScaleY(20);
  if y > wpAskUnistall.SurfaceHeight - wpCheckBox.Height - wpButton.Height then
    y := wpAskUnistall.SurfaceHeight - wpCheckBox.Height - wpButton.Height;
  wpButton.Top := y;
end;

procedure UnInstUpdateGUI;
begin
  UpdateUninstallInfo;
    Log('UnInstUpdateGUI UninstallState='+dbgsUiState(UninstallState)+
       ' IsSecondaryUpdate='+dbgsBool(IsSecondaryUpdate)+
       '  Check='+dbgsBool(IsSecondaryCheckBoxChecked)
       );

  WizardForm.NextButton.Enabled := (UninstallState = uiDone) or (UninstallState = uiDestNeeded) or wpCheckBox.Checked;
  wpCheckBox.Enabled := not(UninstallState = uiDone);
  wpButton.Enabled := not(UninstallState = uiDone);
end;

procedure ActivateAskUninst(Sender: TWizardPage);
begin
  UnInstUpdateGUI;
end;

function SkipAskUninst(Sender: TWizardPage): Boolean;
begin
    Log('SkipAskUninst UninstallState='+dbgsUiState(UninstallState)+
       ', OldPath='+OldPath+' OldName='+OldName+' UnInstaller='+UnInstaller +
       ' IsSecondaryUpdate='+dbgsBool(IsSecondaryUpdate)+
       '  Check='+dbgsBool(IsSecondaryCheckBoxChecked)
       );
  Result := UninstallState = uiDone;
  if Result Then exit;

  UnInstUpdateGUI;
  //UpdateUninstallInfo;

  // OldName, Registry 'DisplayName'
  // OldPath,  Registry 'Inno Setup: App Path'
  // UnInstaller

  case UninstallState of
    uiDestNeeded: begin
	    wpLabel2.Font.Color := clDefault;
        wpCheckBox.Visible := False;
        if IsSecondaryUpdate then
          InitAskUninstall(Format(SaveCustomMessage('OldSecondInDestFolder1', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
	                       Format(SaveCustomMessage('OldSecondInDestFolder2', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldSecondInDestFolder3', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldSecondInDestFolder4', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]))
        else
          InitAskUninstall(Format(SaveCustomMessage('OldInDestFolder1', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
	                       Format(SaveCustomMessage('OldInDestFolder2', ''), {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldInDestFolder3', ''), {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldInDestFolder4', ''), {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]));
      end;
    UIOtherNeeded: begin
	    wpLabel2.Font.Color := clRed;
	    wpLabel2.Font.Color := clRed;
        wpCheckBox.Visible := True;
        //if IsSecondaryUpdate then
        //  InitAskUninstall(Format(SaveCustomMessage('InOtherFolder1', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
	       //                Format(SaveCustomMessage('OldSecondInOtherFolder2', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
        //                   Format(SaveCustomMessage('OldSecondInOtherFolder3', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
        //                   Format(SaveCustomMessage('OldSecondInOtherFolder4', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]));
        //else
          InitAskUninstall(Format(SaveCustomMessage('OldInOtherFolder1', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
	                       Format(SaveCustomMessage('OldInOtherFolder2', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldInOtherFolder3', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldInOtherFolder4', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]));
        //end;
      end;
    uiInconsistent: begin
	    wpLabel1.Font.Color := clRed;
	    wpLabel2.Font.Color := clRed;
        wpCheckBox.Visible := True;
        //if IsSecondaryUpdate then
        //  InitAskUninstall(Format(SaveCustomMessage('OldSecondInBadFolder1', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
	       //                Format(SaveCustomMessage('OldSecondInBadFolder2', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
        //                   Format(SaveCustomMessage('OldSecondInBadFolder3', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
        //                   Format(SaveCustomMessage('OldSecondInBadFolder4', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]));
        //else
          InitAskUninstall(Format(SaveCustomMessage('OldInBadFolder1', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
	                       Format(SaveCustomMessage('OldInBadFolder2', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldInBadFolder3', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldInBadFolder4', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]));
        //end;
      end;
  end;

end;

procedure UnInstBtnClick(Sender: TObject);
var
  s, UnInstaller: String;
  b, FolderEmpty : Boolean;
  i: integer;
begin
  UninstallDoneState := UninstallState;
  UninstallState := uiDone;

  UnInstaller := RemoveQuotes(GetUninstallData('UninstallString'));

  b := (UnInstaller <> '') and FileExists(UnInstaller);
  if b then begin
    if PathEqual then begin
      LoadCFGFile(OldPath, CFGFileForUninstDir);
      CFGStateForUninstDir := ParseCFGList(CFGFileForUninstDir, CFGPathForUninstDir);
      UninstDir := OldPath;
    end;

    if UninstallState = uiInconsistent then
      b := Exec(UnInstaller, '/VERBOSE /NORESTART','', SW_SHOW, ewWaitUntilTerminated, i)
    else
      b := Exec(UnInstaller, '/SILENT /NORESTART','', SW_SHOW, ewWaitUntilTerminated, i);
  end;
  if not b then
    MsgBox('Uninstall failed.', mbConfirmation, MB_OK)
  else begin
    if (UninstallDoneState = uiDestNeeded) then
    begin
      FolderEmpty := IsDirEmpty(WizardDirValue);
	  if not FolderEmpty then begin Sleep(500); FolderEmpty := IsDirEmpty(WizardDirValue); end;
	  if not FolderEmpty then begin Sleep(500); FolderEmpty := IsDirEmpty(WizardDirValue); end;
	  if not FolderEmpty then begin Sleep(500); FolderEmpty := IsDirEmpty(WizardDirValue); end;
      if not(FolderEmpty) then begin
        // Dir NOT empty, after uninstall
	    try
		  s := CustomMessage('FolderNotEmpty2');
	    except
		  s := 'The target folder is not empty.';
        end;
        MsgBox(s, mbConfirmation, MB_OK);
      end;
    end;
  end;

  UnInstUpdateGUI;
end;

procedure UnInstCheckboxClick(Sender: TObject);
begin
  UnInstUpdateGUI;
end;

// *** Create Wizzard Page

procedure CreateUninstallWizardPage;
var
  s, s2 : String;
begin
  try
    s := CustomMessage('AskUninstallTitle1');
    s2 := CustomMessage('AskUninstallTitle2');
  except
    s := 'Previous Installation';
	s2 := 'Do you want to run the uninstaller?';
  end;
  wpAskUnistall := CreateCustomPage(wpSelectDir, s, s2);
  wpAskUnistall.OnShouldSkipPage := @SkipAskUninst;
  wpAskUnistall.OnActivate := @ActivateAskUninst;

  wpLabel1 := TNewStaticText.Create(wpAskUnistall);
  wpLabel1.Parent := wpAskUnistall.Surface;
  wpLabel1.Top := 0;
  wpLabel1.Left := 0;
  wpLabel1.Width := wpAskUnistall.SurfaceWidth;
  wpLabel1.Autosize:= False;
  wpLabel1.WordWrap := True;
  wpLabel1.Caption := '';

  wpLabel2 := TNewStaticText.Create(wpAskUnistall);
  wpLabel2.Parent := wpAskUnistall.Surface;
  wpLabel2.Left := 0;
  wpLabel2.Width := wpAskUnistall.SurfaceWidth;
  wpLabel2.Autosize:= False;
  wpLabel2.WordWrap := True;
  wpLabel2.Caption := '';

  wpLabel3 := TNewStaticText.Create(wpAskUnistall);
  wpLabel3.Parent := wpAskUnistall.Surface;
  wpLabel3.Left := 0;
  wpLabel3.Width := wpAskUnistall.SurfaceWidth;
  wpLabel3.Autosize:= False;
  wpLabel3.WordWrap := True;
  wpLabel3.Caption := '';

  wpLabel4 := TNewStaticText.Create(wpAskUnistall);
  wpLabel4.Parent := wpAskUnistall.Surface;
  wpLabel4.Left := 0;
  wpLabel4.Width := wpAskUnistall.SurfaceWidth;
  wpLabel4.Autosize:= False;
  wpLabel4.WordWrap := True;
  wpLabel4.Caption := '';

  try
    s := CustomMessage('BtnUninstall');
  except
    s := 'Uninstall';
  end;
  wpButton := TNewButton.Create(wpAskUnistall);
  wpButton.Parent := wpAskUnistall.Surface;
  wpButton.Width := ScaleX(80);
  wpButton.Left := (wpAskUnistall.SurfaceWidth div 2) - ScaleX(40);
  wpButton.Caption := s;
  wpButton.OnClick := @UnInstBtnClick;

  try
    s := CustomMessage('ChkContinue');
  except
    s := 'Continue without uninstall';
  end;
  wpCheckBox := TNewCheckBox.Create(wpAskUnistall);
  wpCheckBox.Parent := wpAskUnistall.Surface;
  wpCheckBox.Top := wpAskUnistall.SurfaceHeight - wpCheckBox.Height - 1;
  wpCheckBox.Width := wpAskUnistall.SurfaceWidth;
  wpCheckBox.Caption := s;
  wpCheckBox.OnClick := @UnInstCheckboxClick;
end;


