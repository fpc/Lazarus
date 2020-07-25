{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Abtract:
    Frame for environment options for main paths, like
    Lazarus directory, compiler path.
}
unit files_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  StdCtrls, Dialogs, Controls, Spin,
  // LazUtils
  FileUtil, LazFileUtils,
  // IDE
  EnvironmentOpts,
  // CodeTools
  CodeToolManager, DefineTemplates,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEDialogs, IDEUtils,
  // IDE
  LazarusIDEStrConsts, InputHistory, LazConf, DialogProcs, InitialSetupProc, Classes;

type

  { TFilesOptionsFrame }

  TFilesOptionsFrame = class(TAbstractIDEOptionsEditor)
    AutoCloseCompileDialogCheckBox: TCheckBox;
    MultipleInstancesComboBox: TComboBox;
    CompilerTranslationFileButton:TButton;
    CompilerTranslationFileComboBox:TComboBox;
    CompilerTranslationFileLabel:TLabel;
    CompilerPathButton:TButton;
    CompilerPathComboBox:TComboBox;
    CompilerPathLabel:TLabel;
    FPCSourceDirButton:TButton;
    FPCSourceDirComboBox:TComboBox;
    FPCSourceDirLabel:TLabel;
    MultipleInstancesLabel: TLabel;
    lblCenter: TLabel;
    LazarusDirButton:TButton;
    LazarusDirComboBox:TComboBox;
    LazarusDirLabel:TLabel;
    MakePathButton:TButton;
    MakePathComboBox:TComboBox;
    MakePathLabel:TLabel;
    MaxRecentOpenFilesSpin: TSpinEdit;
    MaxRecentOpenFilesLabel: TLabel;
    MaxRecentProjectFilesSpin: TSpinEdit;
    MaxRecentProjectFilesLabel: TLabel;
    OpenLastProjectAtStartCheckBox: TCheckBox;
    ShowCompileDialogCheckBox: TCheckBox;
    TestBuildDirButton:TButton;
    TestBuildDirComboBox:TComboBox;
    TestBuildDirLabel:TLabel;
    FppkgConfigurationFileLabel: TLabel;
    FppkgConfigurationFileComboBox: TComboBox;
    FppkgConfigurationFileButton: TButton;
    procedure CompilerTranslationFileButtonClick(Sender:TObject);
    procedure FilesButtonClick(Sender: TObject);
    procedure DirectoriesButtonClick(Sender: TObject);
    procedure ShowCompileDialogCheckBoxChange(Sender: TObject);
  private
    FOldLazarusDir: string;
    FOldRealLazarusDir: string;
    FOldCompilerFilename: string;
    FOldRealCompilerFilename: string;
    FOldFPCSourceDir: string;
    FOldRealFPCSourceDir: string;
    FOldMakeFilename: string;
    FOldRealMakeFilename: string;
    FOldTestDir: string;
    FOldRealTestDir: string;
    fOldCompilerMessagesFilename: string;
    fOldRealCompilerMessagesFilename: string;
    fOldFppkcConfigurationFilename: string;
    FOldMaxRecentOpenFiles: integer;
    FOldMaxRecentProjectFiles: integer;
    FOldOpenLastProjectAtStart: boolean;
    fOldShowCompileDialog: boolean;
    fOldAutoCloseCompileDialog: boolean;
    function CheckLazarusDir(Buttons: TMsgDlgButtons): boolean;
    function CheckCompiler(Buttons: TMsgDlgButtons): boolean;
    function CheckFPCSourceDir(Buttons: TMsgDlgButtons): boolean;
    function CheckTestDir: boolean;
    function CheckMake: boolean;
    function CheckFPCMsgFile: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function Check: Boolean; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    procedure RestoreSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TFilesOptionsFrame }

procedure TFilesOptionsFrame.FilesButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  lDirText : string;
  lExpandedName: string; // Expanded name before Dialog
  lDirName, lDirNameF : string;
begin
  OpenDialog := IDEOpenDialogClass.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options := OpenDialog.Options+[ofPathMustExist];
    // set title
    if Sender = CompilerPathButton then begin
      OpenDialog.Title := Format(lisChooseCompilerExecutable,[GetDefaultCompilerFilename]);
      lDirText := CompilerPathComboBox.Text;
    end
    else if Sender=MakePathButton then begin
      OpenDialog.Title := lisChooseMakeExecutable;
      lDirText := MakePathComboBox.Text;
    end
    else
      exit;

    lDirName := EnvironmentOptions.GetParsedValue(eopCompilerFilename, lDirText);
    lExpandedName := CleanAndExpandFilename(lDirName);
    lDirName := GetValidDirectoryAndFilename(lDirName, {out} lDirNameF);
    OpenDialog.InitialDir := lDirName;
    OpenDialog.FileName := lDirNameF;

    if OpenDialog.Execute then begin
      lDirNameF := CleanAndExpandFilename(OpenDialog.Filename);
      if UpperCase(lExpandedName) <> UpperCase(lDirNameF) then begin // Changed ?
        lDirText := lDirNameF;
        if Sender=CompilerPathButton then begin
          // check compiler filename
          SetComboBoxText(CompilerPathComboBox,lDirText,cstFilename);
          CheckCompiler([mbOk]);
        end
        else if Sender = MakePathButton then begin
          // check make filename
          SetComboBoxText(MakePathComboBox,lDirText,cstFilename);
          CheckMake;
        end;
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TFilesOptionsFrame.CompilerTranslationFileButtonClick(Sender:TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=IDEOpenDialogClass.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:=lisChooseCompilerMessages;
    OpenDialog.Filter:=dlgFilterFPCMessageFile+' (*.msg)|*.msg|'+dlgFilterAll+'|'+
      GetAllFilesMask;
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(CompilerTranslationFileComboBox,AFilename,cstFilename);
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TFilesOptionsFrame.DirectoriesButtonClick(Sender: TObject);
var
  OpenDialog: TSelectDirectoryDialog;
  lDirText : string;
  lExpandedName: string;
  lDirName, lDirNameF: string;
begin
  OpenDialog := TSelectDirectoryDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options := OpenDialog.Options+[ofPathMustExist];
    // set title
    if Sender = LazarusDirButton then begin
      OpenDialog.Title := lisChooseLazarusSourceDirectory;
      lDirText := LazarusDirComboBox.Text;
    end
    else if Sender = FPCSourceDirButton then begin
      OpenDialog.Title := lisChooseFPCSourceDir;
      lDirText := FPCSourceDirComboBox.Text;
    end
    else if Sender=TestBuildDirButton then begin
      OpenDialog.Title := lisChooseTestBuildDir;
      lDirText := TestBuildDirComboBox.Text;
    end
    else
      exit;

    if lDirText = '' then
      lDirName := EnvironmentOptions.GetParsedValue(eopLazarusDirectory, '')
    else
      lDirName := EnvironmentOptions.GetParsedValue(eopLazarusDirectory, lDirText);

    lExpandedName := CleanAndExpandDirectory(lDirName);
    lDirName := GetValidDirectoryAndFilename(lDirName, lDirNameF);
    {
    if lDirNameF = '' then begin
      lDirName := ExtractFilePath(lDirName);
      lDirNameF := ExtractFileName(lDirName);
    end;
    }

    OpenDialog.InitialDir := IncludeTrailingBackslash(lDirName);
    OpenDialog.FileName := lDirNameF;

    if OpenDialog.Execute then begin
      lDirName := CleanAndExpandDirectory(OpenDialog.Filename);
      if UpperCase(lDirName)<>UpperCase(lExpandedName) then begin
        lDirText := lDirName;
        if Sender = LazarusDirButton then begin
          // check lazarus directory
          SetComboBoxText(LazarusDirComboBox,lDirText,cstFilename);
          CheckLazarusDir([mbOk]);
        end
        else if Sender = FPCSourceDirButton then begin
          // check fpc source directory
          SetComboBoxText(FPCSourceDirComboBox,lDirText,cstFilename);
          CheckFPCSourceDir([mbOK]);
        end
        else if Sender = TestBuildDirButton then begin
          // check test directory
          SetComboBoxText(TestBuildDirComboBox,lDirText,cstFilename);
          CheckTestDir;
        end;
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TFilesOptionsFrame.ShowCompileDialogCheckBoxChange(Sender: TObject);
begin
  AutoCloseCompileDialogCheckBox.Enabled := ShowCompileDialogCheckBox.Checked;
end;

procedure TFilesOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  MaxRecentOpenFilesLabel.Caption:=dlgMaxRecentFiles;
  MaxRecentOpenFilesLabel.Hint:=dlgMaxRecentHint;
  MaxRecentProjectFilesLabel.Caption:=dlgMaxRecentProjs;
  MaxRecentProjectFilesLabel.Hint:=dlgMaxRecentHint;
  OpenLastProjectAtStartCheckBox.Caption:=dlgQOpenLastPrj;
  ShowCompileDialogCheckBox.Visible:=false;
  AutoCloseCompileDialogCheckBox.Visible:=false;
  LazarusDirLabel.Caption:=dlgLazarusDir;
  with LazarusDirComboBox.Items do
  begin
    BeginUpdate;
    Add(ProgramDirectoryWithBundle);
    EndUpdate;
  end;
  MultipleInstancesLabel.Caption := dlgMultipleInstances;
  with MultipleInstancesComboBox.Items do
  begin
    BeginUpdate;
    Add(dlgMultipleInstances_AlwaysStartNew);
    Add(dlgMultipleInstances_OpenFilesInRunning);
    Add(dlgMultipleInstances_ForceSingleInstance);
    EndUpdate;
  end;
  Assert(MultipleInstancesComboBox.Items.Count = Ord(High(TIDEMultipleInstancesOption))+1);

  CompilerPathLabel.Caption:=Format(dlgFpcExecutable,[GetDefaultCompilerFilename]);
  FPCSourceDirLabel.Caption:=dlgFpcSrcPath;
  FppkgConfigurationFileLabel.Caption:=dlgFppkgConfigurationFile;
  MakePathLabel.Caption:=dlgMakeExecutable;
  with MakePathComboBox.Items do
  begin
    BeginUpdate;
    Add('make');
    Add('gmake');
    EndUpdate;
  end;

  TestBuildDirLabel.Caption:=dlgTestPrjDir;
  with TestBuildDirComboBox.Items do
  begin
    BeginUpdate;
    {$IFDEF Unix}
    Add('~/tmp');
    Add('/tmp');
    Add('/var/tmp');
    {$ELSE}
    Add('c:\tmp');
    Add('c:\windows\temp');
    {$ENDIF}
    EndUpdate;
  end;

  CompilerTranslationFileLabel.Caption:=dlgCompilerMessages;
  CompilerTranslationFileLabel.Hint:=
    lisSetThisToTranslateTheCompilerMessagesToAnotherLang;
  CompilerTranslationFileButton.Hint:=CompilerTranslationFileLabel.Hint;
  CompilerTranslationFileComboBox.Hint:=CompilerTranslationFileLabel.Hint;
  with CompilerTranslationFileComboBox.Items do
  begin
    Add(GetForcedPathDelims('$(FPCSrcDir)/compiler/msg/errordu.msg'));
  end;
end;

function TFilesOptionsFrame.GetTitle: String;
begin
  Result := dlgEnvFiles;
end;

function TFilesOptionsFrame.Check: Boolean;
begin
  Result := False;
  with EnvironmentOptions do
  begin
    LazarusDirectory:=LazarusDirComboBox.Text;
    CompilerFilename:=CompilerPathComboBox.Text;
    FPCSourceDirectory:=FPCSourceDirComboBox.Text;
    MakeFilename:=MakePathComboBox.Text;
    TestBuildDirectory:=TestBuildDirComboBox.Text;
    CompilerMessagesFilename:=CompilerTranslationFileComboBox.Text;
    FppkgConfigFile:=FppkgConfigurationFileComboBox.Text;
  end;
  // check lazarus directory
  if not CheckLazarusDir([mbIgnore,mbCancel]) then exit;
  // check compiler filename
  if not CheckCompiler([mbIgnore,mbCancel]) then exit;
  // check fpc source directory
  if not CheckFPCSourceDir([mbIgnore,mbCancel]) then exit;
  // check make filename
  if not CheckMake then exit;
  // check test directory
  if not CheckTestDir then exit;
  // check fpc messages file
  if not CheckFPCMsgFile then exit;
  Result := True;
end;

procedure TFilesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    // Lazarus dir
    FOldLazarusDir:=LazarusDirectory;
    FOldRealLazarusDir:=GetParsedLazarusDirectory;
    if LazarusDirHistory.Count>0 then
      LazarusDirComboBox.Items.Assign(LazarusDirHistory);
    SetComboBoxText(LazarusDirComboBox,LazarusDirectory,cstFilename,MaxComboBoxCount);

    // compiler filename
    FOldCompilerFilename:=CompilerFilename;
    FOldRealCompilerFilename:=GetParsedCompilerFilename;
    with CompilerPathComboBox do
    begin
      Items.BeginUpdate;
      Items.Assign(CompilerFileHistory);
      AddFilenameToList(Items,FindDefaultCompilerPath);
      AddFilenameToList(Items,FindDefaultExecutablePath('fpc'+GetExecutableExt));
      Items.EndUpdate;
    end;
    SetComboBoxText(CompilerPathComboBox,CompilerFilename,cstFilename,MaxComboBoxCount);

    // FPC src dir
    FOldFPCSourceDir:=FPCSourceDirectory;
    FOldRealFPCSourceDir:=GetParsedFPCSourceDirectory;
    if FPCSourceDirHistory.Count>0 then
      FPCSourceDirComboBox.Items.Assign(FPCSourceDirHistory);
    SetComboBoxText(FPCSourceDirComboBox,FPCSourceDirectory,cstFilename,MaxComboBoxCount);

    // "make"
    FOldMakeFilename:=MakeFilename;
    FOldRealMakeFilename:=GetParsedMakeFilename;
    if MakeFileHistory.Count>0 then
      MakePathComboBox.Items.Assign(MakeFileHistory);
    SetComboBoxText(MakePathComboBox,MakeFilename,cstFilename,MaxComboBoxCount);

    // test build dir
    FOldTestDir:=TestBuildDirectory;
    FOldRealTestDir:=GetParsedTestBuildDirectory;
    if TestBuildDirHistory.Count>0 then
      TestBuildDirComboBox.Items.Assign(TestBuildDirHistory);
    SetComboBoxText(TestBuildDirComboBox,TestBuildDirectory,cstFilename,MaxComboBoxCount);

    // compiler messages file
    fOldCompilerMessagesFilename:=CompilerMessagesFilename;
    fOldRealCompilerMessagesFilename:=GetParsedCompilerMessagesFilename;
    if CompilerMessagesFileHistory.Count>0 then
      CompilerTranslationFileComboBox.Items.Assign(CompilerMessagesFileHistory);
    SetComboBoxText(CompilerTranslationFileComboBox,CompilerMessagesFilename,cstFilename,MaxComboBoxCount);

    // fppkg configuration  file
    fOldFppkcConfigurationFilename:=FppkgConfigFile;
    fOldRealCompilerMessagesFilename:=GetParsedFppkgConfig;
    if FppkgConfigFileHistory.Count>0 then
      FppkgConfigurationFileComboBox.Items.Assign(FppkgConfigFileHistory);
    SetComboBoxText(FppkgConfigurationFileComboBox,FppkgConfigFile,cstFilename,MaxComboBoxCount);

    // recent files and directories
    FOldMaxRecentOpenFiles := MaxRecentOpenFiles;
    MaxRecentOpenFilesSpin.Value := MaxRecentOpenFiles;
    FOldMaxRecentProjectFiles := MaxRecentProjectFiles;
    MaxRecentProjectFilesSpin.Value := MaxRecentProjectFiles;
    FOldOpenLastProjectAtStart := OpenLastProjectAtStart;

    // open last project at start
    OpenLastProjectAtStartCheckBox.Checked:=OpenLastProjectAtStart;

    MultipleInstancesComboBox.ItemIndex := Ord(MultipleInstances);

    // compile dialog
    fOldShowCompileDialog:=ShowCompileDialog;
    ShowCompileDialogCheckBox.Checked:=ShowCompileDialog;
    fOldAutoCloseCompileDialog:=AutoCloseCompileDialog;
    AutoCloseCompileDialogCheckBox.Checked:=AutoCloseCompileDialog;
    AutoCloseCompileDialogCheckBox.Enabled:=ShowCompileDialogCheckBox.Checked;
  end;
end;

procedure TFilesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    LazarusDirectory:=LazarusDirComboBox.Text;
    LazarusDirHistory.Assign(LazarusDirComboBox.Items);
    CompilerFilename:=CompilerPathComboBox.Text;
    CompilerFileHistory.Assign(CompilerPathComboBox.Items);
    FPCSourceDirectory:=FPCSourceDirComboBox.Text;
    FPCSourceDirHistory.Assign(FPCSourceDirComboBox.Items);
    MakeFilename:=MakePathComboBox.Text;
    MakeFileHistory.Assign(MakePathComboBox.Items);
    TestBuildDirHistory.Assign(TestBuildDirComboBox.Items);
    TestBuildDirectory:=TestBuildDirComboBox.Text;
    CompilerMessagesFileHistory.Assign(CompilerTranslationFileComboBox.Items);
    CompilerMessagesFilename:=CompilerTranslationFileComboBox.Text;
    FppkgConfigFileHistory.Assign(FppkgConfigurationFileComboBox.Items);
    FppkgConfigFile:=FppkgConfigurationFileComboBox.Text;

    // recent files and directories
    MaxRecentOpenFiles := MaxRecentOpenFilesSpin.Value;
    MaxRecentProjectFiles := MaxRecentProjectFilesSpin.Value;
    OpenLastProjectAtStart:=OpenLastProjectAtStartCheckBox.Checked;
    MultipleInstances := TIDEMultipleInstancesOption(MultipleInstancesComboBox.ItemIndex);
    ShowCompileDialog := ShowCompileDialogCheckBox.Checked;
    AutoCloseCompileDialog := AutoCloseCompileDialogCheckBox.Checked;
  end;
end;

procedure TFilesOptionsFrame.RestoreSettings(AOptions: TAbstractIDEOptions);
begin
  inherited RestoreSettings(AOptions);
  with AOptions as TEnvironmentOptions do
  begin
    LazarusDirectory:=FOldLazarusDir;
    CompilerFilename:=FOldCompilerFilename;
    FPCSourceDirectory:=FOldFPCSourceDir;
    MakeFilename:=FOldMakeFilename;
    TestBuildDirectory:=FOldTestDir;
    CompilerMessagesFilename:=fOldCompilerMessagesFilename;
    FppkgConfigFile:=fOldFppkcConfigurationFilename;

    // recent files and directories
    MaxRecentOpenFiles := FOldMaxRecentOpenFiles;
    MaxRecentProjectFiles := FOldMaxRecentProjectFiles;
    OpenLastProjectAtStart := FOldOpenLastProjectAtStart;
    ShowCompileDialog := fOldShowCompileDialog;
    AutoCloseCompileDialog := fOldAutoCloseCompileDialog;
  end;
end;

function TFilesOptionsFrame.CheckLazarusDir(Buttons: TMsgDlgButtons): boolean;
var
  NewLazarusDir: string;
  Quality: TSDFilenameQuality;
  Note: string;
begin
  if EnvironmentOptions.LazarusDirectory=FOldLazarusDir then exit(true);
  Result := False;
  EnvironmentOptions.LazarusDirectory:=LazarusDirComboBox.Text;
  NewLazarusDir := EnvironmentOptions.GetParsedLazarusDirectory;
  Quality:=CheckLazarusDirectoryQuality(NewLazarusDir,Note);
  if Quality<>sddqCompatible then
  begin
    if IDEMessageDialog(lisCCOWarningCaption,
      Format(lisTheLazarusDirectoryDoesNotLookCorrect,
             [NewLazarusDir, LineEnding, Note]),
      mtWarning, Buttons)<>mrIgnore
    then
      exit;
  end;
  Result := true;
end;

function TFilesOptionsFrame.CheckFPCSourceDir(Buttons: TMsgDlgButtons): boolean;
var
  NewFPCSrcDir: string;
  Note: string;
  Quality: TSDFilenameQuality;
  CfgCache: TPCTargetConfigCache;
  FPCVer: String;
begin
  if EnvironmentOptions.FPCSourceDirectory=FOldFPCSourceDir then exit(true);
  Result:=false;
  CfgCache:=CodeToolBoss.CompilerDefinesCache.ConfigCaches.Find(
    EnvironmentOptions.GetParsedCompilerFilename,'','','',true);
  FPCVer:=CfgCache.GetFPCVer;
  EnvironmentOptions.FPCSourceDirectory:=FPCSourceDirComboBox.Text;
  NewFPCSrcDir:=EnvironmentOptions.GetParsedFPCSourceDirectory;
  Quality:=CheckFPCSrcDirQuality(NewFPCSrcDir,Note,FPCVer);
  if Quality<>sddqCompatible then
  begin
    if IDEMessageDialog(lisCCOWarningCaption,
      Format(lisTheFPCSourceDirectoryDoesNotLookCorrect,
             [NewFPCSrcDir, LineEnding, Note]),
      mtWarning, Buttons)<>mrIgnore
    then
      exit;
  end;
  Result:=true;
end;

function TFilesOptionsFrame.CheckCompiler(Buttons: TMsgDlgButtons): boolean;
var
  NewCompilerFilename: String;
  Note: string;
  Quality: TSDFilenameQuality;
begin
  if EnvironmentOptions.CompilerFilename=FOldCompilerFilename then exit(true);
  Result:=false;
  EnvironmentOptions.CompilerFilename:=CompilerPathComboBox.Text;
  NewCompilerFilename:=EnvironmentOptions.GetParsedCompilerFilename;
  Quality:=CheckFPCExeQuality(NewCompilerFilename,Note,
                                CodeToolBoss.CompilerDefinesCache.TestFilename);
  if Quality<>sddqCompatible then
  begin
    if IDEMessageDialog(lisCCOWarningCaption,
      Format(lisTheCompilerFileDoesNotLookCorrect,
             [NewCompilerFilename, LineEnding, Note]),
      mtWarning, Buttons)<>mrIgnore
    then
      exit;
  end;
  Result:=true;
end;

function TFilesOptionsFrame.CheckTestDir: boolean;
var
  NewTestDir: string;
  StopChecking: boolean;
begin
  if EnvironmentOptions.TestBuildDirectory=FOldTestDir then exit(true);
  EnvironmentOptions.TestBuildDirectory:=TestBuildDirComboBox.Text;
  NewTestDir:=EnvironmentOptions.GetParsedTestBuildDirectory;
  Result:=SimpleDirectoryCheck(FOldRealTestDir,NewTestDir,
                               lisEnvOptDlgTestDirNotFoundMsg,StopChecking);
end;

function TFilesOptionsFrame.CheckMake: boolean;
var
  NewMakeFilename: String;
begin
  if EnvironmentOptions.MakeFilename=FOldMakeFilename then exit(true);
  EnvironmentOptions.MakeFilename:=MakePathComboBox.Text;
  NewMakeFilename:=EnvironmentOptions.GetParsedMakeFilename;
  Result:=CheckExecutable(FOldRealMakeFilename,NewMakeFilename,
    lisCCOWarningCaption, Format(lisThePathOfMakeIsNotCorrect, [NewMakeFilename]));
end;

function TFilesOptionsFrame.CheckFPCMsgFile: boolean;
var
  NewMsgFile: String;
begin
  if EnvironmentOptions.CompilerMessagesFilename=FOldCompilerFilename then exit(true);
  EnvironmentOptions.CompilerMessagesFilename:=CompilerTranslationFileComboBox.Text;
  if EnvironmentOptions.CompilerMessagesFilename<>'' then begin
    NewMsgFile:=EnvironmentOptions.GetParsedCompilerMessagesFilename;
    if not FileExistsUTF8(NewMsgFile) then begin
      if IDEMessageDialog(lisCCOErrorCaption, Format(
        lisCompilerMessagesFileNotFound, [#13, NewMsgFile]), mtError, [mbCancel,
        mbIgnore])<>mrIgnore
      then
        exit(false);
    end;
  end;
  Result:=true;
end;

constructor TFilesOptionsFrame.Create(AOwner: TComponent); // ~bk to be removed
begin
  inherited Create(AOwner);
end;

class function TFilesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TFilesOptionsFrame, EnvOptionsFiles);
end.

