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

  Abstract:
    Frame for environment options for main paths, like
    Lazarus directory, compiler path.
}
unit Files_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  StdCtrls, Dialogs, Controls, Spin,
  // LazUtils
  FileUtil, LazFileUtils,
  // CodeTools
  CodeToolManager, DefineTemplates,
  // BuildIntf
  IDEOptionsIntf,
  // IdeIntf
  IDEOptEditorIntf, IDEDialogs, IDEUtils,
  // IdeUtils
  InputHistory,
  // IdeConfig
  EnvironmentOpts, DialogProcs, LazConf, IdeConfStrConsts,
  // IDE
  LazarusIDEStrConsts, InitialSetupProc, EnvGuiOptions;

type

  { TFilesOptionsFrame }

  TFilesOptionsFrame = class(TAbstractIDEOptionsEditor)
    CompilerTranslationFileButton:TButton;
    CompilerTranslationFileComboBox:TComboBox;
    CompilerTranslationFileLabel:TLabel;
    CompilerPathButton:TButton;
    CompilerPathComboBox:TComboBox;
    CompilerPathLabel:TLabel;
    FPCSourceDirButton:TButton;
    FPCSourceDirComboBox:TComboBox;
    FPCSourceDirLabel:TLabel;
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
    TestBuildDirButton:TButton;
    TestBuildDirComboBox:TComboBox;
    TestBuildDirLabel:TLabel;
    FppkgConfigurationFileLabel: TLabel;
    FppkgConfigurationFileComboBox: TComboBox;
    FppkgConfigurationFileButton: TButton;
    procedure CompilerTranslationFileButtonClick(Sender:TObject);
    procedure FilesButtonClick(Sender: TObject);
    procedure DirectoriesButtonClick(Sender: TObject);
    procedure FppkgConfigurationFileButtonClick(Sender: TObject);
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
    fOldFppkgConfigurationFilename: string;
    FOldMaxRecentOpenFiles: integer;
    FOldMaxRecentProjectFiles: integer;
    function CheckLazarusDir(Buttons: TMsgDlgButtons): boolean;
    function CheckCompiler(Buttons: TMsgDlgButtons): boolean;
    function CheckFPCSourceDir(Buttons: TMsgDlgButtons): boolean;
    function CheckTestDir: boolean;
    function CheckMake: boolean;
    function CheckCompilerTranslationFile: boolean;
    function CheckFppkgConfigurationFile: boolean;
  public
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
    OpenDialog.Options := OpenDialog.Options + [ofFileMustExist];
    OpenDialog.Filter:=
      Format('%s (*%s)|*%s|', [dlgFilterExecutable, GetExecutableExt, GetExecutableExt]) +
      Format('%s (%s)|%s', [dlgFilterAll, GetAllFilesMask, GetAllFilesMask]);
    // set title
    if Sender = CompilerPathButton then begin
      OpenDialog.Title := Format(lisChooseCompilerExecutable,[GetDefaultCompilerFilename]);
      lDirText := CompilerPathComboBox.Text;
      lDirName := EnvironmentOptions.GetParsedValue(eopCompilerFilename, lDirText);
    end
    else if Sender=MakePathButton then begin
      OpenDialog.Title := lisChooseMakeExecutable;
      lDirText := MakePathComboBox.Text;
      lDirName := EnvironmentOptions.GetParsedValue(eopMakeFilename, lDirText);
    end
    else
      exit;

    lExpandedName := CleanAndExpandFilename(lDirName);
    lDirName := GetValidDirectoryAndFilename(lDirName, {out} lDirNameF);
    OpenDialog.InitialDir := lDirName;
    OpenDialog.FileName := lDirNameF;

    if OpenDialog.Execute then begin
      lDirNameF := CleanAndExpandFilename(OpenDialog.Filename);
      if CompareText(lExpandedName, lDirNameF) <> 0 then begin
        lDirText := lDirNameF;
        if Sender=CompilerPathButton then begin
          // check compiler filename
          SetComboBoxText(CompilerPathComboBox,lDirText,cstFilename);
          EnvironmentOptions.CompilerFilename:=CompilerPathComboBox.Text;
          CheckCompiler([mbOk]);
        end
        else if Sender = MakePathButton then begin
          // check make filename
          SetComboBoxText(MakePathComboBox,lDirText,cstFilename);
          EnvironmentOptions.MakeFilename:=MakePathComboBox.Text;
          CheckMake;
        end;
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TFilesOptionsFrame.DirectoriesButtonClick(Sender: TObject);
var
  OpenDialog: TSelectDirectoryDialog;
  lDirText, lDirName, loDirNameF, lExpandedName: string;
begin
  OpenDialog := TSelectDirectoryDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    // set title
    if Sender = LazarusDirButton then begin
      OpenDialog.Title := lisChooseLazarusSourceDirectory;
      OpenDialog.Options := OpenDialog.Options + [ofPathMustExist];
      lDirText := LazarusDirComboBox.Text;
      lDirName := EnvironmentOptions.GetParsedValue(eopLazarusDirectory, lDirText);
    end
    else if Sender = FPCSourceDirButton then begin
      OpenDialog.Title := lisChooseFPCSourceDir;
      OpenDialog.Options := OpenDialog.Options + [ofPathMustExist];
      lDirText := FPCSourceDirComboBox.Text;
      lDirName := EnvironmentOptions.GetParsedValue(eopFPCSourceDirectory, lDirText);
    end
    else if Sender=TestBuildDirButton then begin
      OpenDialog.Title := lisChooseTestBuildDir;
      OpenDialog.Options := OpenDialog.Options
        - [ofPathMustExist]     // allow to choose a non-existent path
        + [ofNoReadOnlyReturn]; // the folder must be writable
      lDirText := TestBuildDirComboBox.Text;
      lDirName := EnvironmentOptions.GetParsedValue(eopTestBuildDirectory, lDirText);
    end
    else
      exit;

    lExpandedName := CleanAndExpandDirectory(lDirName);
    lDirName := GetValidDirectoryAndFilename(lDirName, loDirNameF);

    OpenDialog.InitialDir := IncludeTrailingBackslash(lDirName);
    OpenDialog.FileName := loDirNameF;

    if OpenDialog.Execute then begin
      lDirName := CleanAndExpandDirectory(OpenDialog.Filename);
      if CompareText(lDirName, lExpandedName) <> 0 then begin
        lDirText := lDirName;
        if Sender = LazarusDirButton then begin
          // check lazarus directory
          SetComboBoxText(LazarusDirComboBox,lDirText,cstFilename);
          EnvironmentOptions.LazarusDirectory:=LazarusDirComboBox.Text;
          CheckLazarusDir([mbOk]);
        end
        else if Sender = FPCSourceDirButton then begin
          // check fpc source directory
          SetComboBoxText(FPCSourceDirComboBox,lDirText,cstFilename);
          EnvironmentOptions.FPCSourceDirectory:=FPCSourceDirComboBox.Text;
          CheckFPCSourceDir([mbOK]);
        end
        else if Sender = TestBuildDirButton then begin
          // check test directory
          SetComboBoxText(TestBuildDirComboBox,lDirText,cstFilename);
          EnvironmentOptions.TestBuildDirectory:=TestBuildDirComboBox.Text;
          CheckTestDir;
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
    OpenDialog.Filter:=
      Format('%s (%s)|%s|', [dlgFilterFPCMessageFile, '*.msg', '*.msg']) +
      Format('%s (%s)|%s', [dlgFilterAll, GetAllFilesMask, GetAllFilesMask]);
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(CompilerTranslationFileComboBox,AFilename,cstFilename);
      EnvironmentOptions.CompilerMessagesFilename:=CompilerTranslationFileComboBox.Text;
      CheckCompilerTranslationFile;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TFilesOptionsFrame.FppkgConfigurationFileButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=IDEOpenDialogClass.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:=lisChooseFppkgConfigurationFile;
    OpenDialog.Filter:=
      Format('%s (%s)|%s|', [dlgFilterFppkgConfigurationFile, '*.cfg', '*.cfg']) +
      Format('%s (%s)|%s', [dlgFilterAll, GetAllFilesMask, GetAllFilesMask]);
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(FppkgConfigurationFileComboBox,AFilename,cstFilename);
      EnvironmentOptions.FppkgConfigFile:=FppkgConfigurationFileComboBox.Text;
      CheckFppkgConfigurationFile;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TFilesOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  MaxRecentOpenFilesLabel.Caption:=dlgMaxRecentFiles;
  MaxRecentOpenFilesLabel.Hint:=dlgMaxRecentHint;
  MaxRecentProjectFilesLabel.Caption:=dlgMaxRecentProjs;
  MaxRecentProjectFilesLabel.Hint:=dlgMaxRecentHint;
  LazarusDirLabel.Caption:=dlgLazarusDir;
  LazarusDirLabel.Hint:=Format(lisLazarusDirHint,[GetPrimaryConfigPath]);
  LazarusDirComboBox.Hint:=LazarusDirLabel.Hint;
  LazarusDirButton.Hint:=LazarusDirLabel.Hint;
  with LazarusDirComboBox.Items do
  begin
    BeginUpdate;
    Add(ProgramDirectoryWithBundle);
    EndUpdate;
  end;

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
  if not CheckCompilerTranslationFile then exit;
  // check fppkg configuration file
  if not CheckFppkgConfigurationFile then exit;
  Result := True;
end;

procedure TFilesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  CompFiles: TStringList;
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

    CompFiles:=TStringList.Create;
    try
      CompFiles.Assign(CompilerFileHistory);
      if CompFiles.Count=0 then
        GetDefaultCompilerFilenames(CompFiles);

      with CompilerPathComboBox do
      begin
        Items.BeginUpdate;
        Items.Assign(CompFiles);
        AddFilenameToList(Items,FindDefaultCompilerPath);
        AddFilenameToList(Items,FindDefaultExecutablePath('fpc'+GetExecutableExt));
        Items.EndUpdate;
      end;
      SetComboBoxText(CompilerPathComboBox,CompilerFilename,cstFilename,MaxComboBoxCount);
    finally
      CompFiles.Free;
    end;

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
    fOldFppkgConfigurationFilename:=FppkgConfigFile;
    fOldRealCompilerMessagesFilename:=GetParsedFppkgConfig;
    if FppkgConfigFileHistory.Count>0 then
      FppkgConfigurationFileComboBox.Items.Assign(FppkgConfigFileHistory);
    SetComboBoxText(FppkgConfigurationFileComboBox,FppkgConfigFile,cstFilename,MaxComboBoxCount);

    // recent files and directories
    FOldMaxRecentOpenFiles := MaxRecentOpenFiles;
    MaxRecentOpenFilesSpin.Value := MaxRecentOpenFiles;
    FOldMaxRecentProjectFiles := MaxRecentProjectFiles;
    MaxRecentProjectFilesSpin.Value := MaxRecentProjectFiles;
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
    FppkgConfigFile:=fOldFppkgConfigurationFilename;

    // recent files and directories
    MaxRecentOpenFiles := FOldMaxRecentOpenFiles;
    MaxRecentProjectFiles := FOldMaxRecentProjectFiles;
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

function TFilesOptionsFrame.CheckCompilerTranslationFile: boolean;
var
  NewMsgFile: String;
begin
  if EnvironmentOptions.CompilerMessagesFilename=FOldCompilerFilename then exit(true);
  EnvironmentOptions.CompilerMessagesFilename:=CompilerTranslationFileComboBox.Text;
  if EnvironmentOptions.CompilerMessagesFilename<>'' then begin
    NewMsgFile:=EnvironmentOptions.GetParsedCompilerMessagesFilename;
    if not FileExistsUTF8(NewMsgFile) then begin
      if IDEMessageDialog(lisCCOErrorCaption,
        Format(lisCompilerMessagesFileNotFound, [LineEnding, NewMsgFile]),
        mtError, [mbCancel,mbIgnore]) <> mrIgnore
      then
        exit(false);
    end;
  end;
  Result:=true;
end;

function TFilesOptionsFrame.CheckFppkgConfigurationFile: boolean;
var
  NewFppkgCfgFile, Note: String;
begin
  if EnvironmentOptions.FppkgConfigFile=fOldFppkgConfigurationFilename then exit(true);
  EnvironmentOptions.FppkgConfigFile:=FppkgConfigurationFileComboBox.Text;

  if EnvironmentOptions.FppkgConfigFile<>'' then begin
    NewFppkgCfgFile:=EnvironmentOptions.GetParsedFppkgConfig;

    if not FileExistsUTF8(NewFppkgCfgFile) then
    begin
      if IDEMessageDialog(lisCCOErrorCaption,
        Format(lisFppkgConfigurationFileNotFound, [LineEnding, NewFppkgCfgFile]),
        mtError, [mbCancel,mbIgnore]) <> mrIgnore
      then
        exit(false);
    end else begin
      if (CheckFppkgConfigFile   (NewFppkgCfgFile, Note) <> sddqCompatible) or
         (CheckFppkgConfiguration(NewFppkgCfgFile, Note) <> sddqCompatible) then
        if IDEMessageDialog(lisCCOWarningCaption,
          Format(lisTheFppkgConfigurationFileDoesNotLookCorrect, [NewFppkgCfgFile, LineEnding, Note]),
          mtError, [mbCancel,mbIgnore]) <> mrIgnore
        then
          exit(false);
    end;
  end;

  Result:=true;
end;

class function TFilesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TFilesOptionsFrame, EnvOptionsFiles);
end.

