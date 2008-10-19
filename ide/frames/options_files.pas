unit options_files;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, Dialogs, Controls,
  EnvironmentOpts, LazarusIDEStrConsts, InputHistory, LazConf;

type

  { TFilesOptionsFrame }

  TFilesOptionsFrame = class(TFrame)
    CompilerPathButton: TButton;
    CompilerPathComboBox: TComboBox;
    CompilerPathGroupBox: TGroupBox;
    FPCSourceDirButton: TButton;
    FPCSourceDirComboBox: TComboBox;
    FPCSourceDirGroupBox: TGroupBox;
    LazarusDirButton: TButton;
    LazarusDirComboBox: TComboBox;
    LazarusDirGroupBox: TGroupBox;
    MakePathButton: TButton;
    MakePathComboBox: TComboBox;
    MakePathGroupBox: TGroupBox;
    MaxRecentOpenFilesComboBox: TComboBox;
    MaxRecentOpenFilesLabel: TLabel;
    MaxRecentProjectFilesComboBox: TComboBox;
    MaxRecentProjectFilesLabel: TLabel;
    OpenLastProjectAtStartCheckBox: TCheckBox;
    ShowCompileDialogCheckBox: TCheckBox;
    TestBuildDirButton: TButton;
    TestBuildDirComboBox: TComboBox;
    TestBuildDirGroupBox: TGroupBox;
    procedure FilesButtonClick(Sender: TObject);
    procedure DirectoriesButtonClick(Sender: TObject);
  private
    FOldLazarusDir: string;
    FOldCompilerFilename: string;
    FOldFPCSourceDir: string;
    FOldMakeFilename: string;
    FOldTestDir: string;
    function CheckLazarusDir: boolean;
    function IsFPCSourceDir: boolean;
    function CheckTestDir: boolean;
  public
    function Check: Boolean;
    function GetTitle: String;
    procedure Setup;
    procedure ReadSettings(AOptions: TEnvironmentOptions);
    procedure WriteSettings(AOptions: TEnvironmentOptions);
  end;

implementation

{ TFilesOptionsFrame }

procedure TFilesOptionsFrame.FilesButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    // set title
    if Sender=CompilerPathButton then
      OpenDialog.Title:=
                      Format(lisChooseCompilerPath,[GetDefaultCompilerFilename])
    else if Sender=MakePathButton then
      OpenDialog.Title:=lisChooseMakePath
    else
      exit;

    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);

      if Sender=CompilerPathButton then begin
        // check compiler filename
        SetComboBoxText(CompilerPathComboBox,AFilename);
        CheckExecutable(FOldCompilerFilename,CompilerPathComboBox.Text,
          lisEnvOptDlgInvalidCompilerFilename,
          lisEnvOptDlgInvalidCompilerFilenameMsg);
      end else if Sender=MakePathButton then begin
        //check make filename
        SetComboBoxText(MakePathComboBox,AFilename);
        CheckExecutable(FOldMakeFilename,MakePathComboBox.Text,
          lisEnvOptDlgInvalidMakeFilename,
          lisEnvOptDlgInvalidMakeFilenameMsg);
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
  ADirectoryName: string;
begin
  OpenDialog:=TSelectDirectoryDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    // set title
    if Sender=LazarusDirButton then
      OpenDialog.Title:=lisChooseLazarusSourceDirectory
    else if Sender=FPCSourceDirButton then
      OpenDialog.Title:=lisChooseFPCSourceDir
    else if Sender=TestBuildDirButton then
      OpenDialog.Title:=lisChooseTestBuildDir
    else
      exit;

    if OpenDialog.Execute then begin
      ADirectoryName:=CleanAndExpandDirectory(OpenDialog.Filename);

      if Sender=LazarusDirButton then begin
        // check lazarus directory
        SetComboBoxText(LazarusDirComboBox,ADirectoryName);
        CheckLazarusDir;
      end else if Sender=FPCSourceDirButton then begin
        // check fpc source directory
        SetComboBoxText(FPCSourceDirComboBox,ADirectoryName);
        IsFPCSourceDir;
      end else if Sender=TestBuildDirButton then begin
        // check test directory
        SetComboBoxText(TestBuildDirComboBox,ADirectoryName);
        CheckTestDir;
      end;

    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TFilesOptionsFrame.Setup;
begin
  MaxRecentOpenFilesLabel.Caption:=dlgMaxRecentFiles;
  MaxRecentProjectFilesLabel.Caption:=dlgMaxRecentProjs;
  OpenLastProjectAtStartCheckBox.Caption:=dlgQOpenLastPrj;
  ShowCompileDialogCheckBox.Caption:=dlgQShowCompileDialog;
  LazarusDirGroupBox.Caption:=dlgLazarusDir;

  with LazarusDirComboBox.Items do
  begin
    BeginUpdate;
    Add(ProgramDirectory);
    EndUpdate;
  end;

  CompilerPathGroupBox.Caption:=Format(dlgFpcPath,[GetDefaultCompilerFilename]);
  FPCSourceDirGroupBox.Caption:=dlgFpcSrcPath;
  MakePathGroupBox.Caption:=dlgMakePath;

  with MakePathComboBox.Items do
  begin
    BeginUpdate;
    Add('/usr/bin/make');
    EndUpdate;
  end;

  TestBuildDirGroupBox.Caption:=dlgTestPrjDir;

  with TestBuildDirComboBox.Items do
  begin
    BeginUpdate;
    Add('/tmp');
    Add('/var/tmp');
    Add('c:/tmp');
    Add('c:/windows/temp');
    EndUpdate;
  end;
end;

function TFilesOptionsFrame.GetTitle: String;
begin
  Result := dlgEnvFiles;
end;

function TFilesOptionsFrame.Check: Boolean;
begin
  Result := False;
  // check lazarus directory
  if not CheckLazarusDir then
    Exit;
  // check compiler filename
  if not CheckExecutable(FOldCompilerFilename,CompilerPathComboBox.Text,
    lisEnvOptDlgInvalidCompilerFilename,lisEnvOptDlgInvalidCompilerFilenameMsg) then 
    Exit;
  // check fpc source directory
  if not IsFPCSourceDir then 
    Exit;
  // check make filename
  if not CheckExecutable(FOldMakeFilename,MakePathComboBox.Text,
    lisEnvOptDlgInvalidMakeFilename,lisEnvOptDlgInvalidMakeFilenameMsg) then 
    Exit;
  // check test directory
  if not CheckTestDir then 
    Exit;
  Result := True;
end;

procedure TFilesOptionsFrame.ReadSettings(AOptions: TEnvironmentOptions);
begin
  with AOptions do
  begin
    LazarusDirComboBox.Items.Assign(LazarusDirHistory);
    FOldLazarusDir:=LazarusDirectory;
    SetComboBoxText(LazarusDirComboBox,LazarusDirectory,MaxComboBoxCount);
    with CompilerPathComboBox do
    begin
      Items.BeginUpdate;
      Items.Assign(CompilerFileHistory);
      AddFilenameToList(Items,FindDefaultCompilerPath);
      AddFilenameToList(Items,FindDefaultExecutablePath('fpc'+GetExecutableExt));
      GetDefaultCompilerFilenames(Items);
      Items.EndUpdate;
    end;

    FOldCompilerFilename:=CompilerFilename;
    SetComboBoxText(CompilerPathComboBox,CompilerFilename,MaxComboBoxCount);
    FPCSourceDirComboBox.Items.Assign(FPCSourceDirHistory);
    FOldFPCSourceDir:=FPCSourceDirectory;
    SetComboBoxText(FPCSourceDirComboBox,FPCSourceDirectory,MaxComboBoxCount);
    MakePathComboBox.Items.Assign(MakeFileHistory);
    FOldMakeFilename:=MakeFilename;
    SetComboBoxText(MakePathComboBox,MakeFilename,MaxComboBoxCount);
    TestBuildDirComboBox.Items.Assign(TestBuildDirHistory);
    FOldTestDir:=TestBuildDirectory;
    SetComboBoxText(TestBuildDirComboBox,TestBuildDirectory,MaxComboBoxCount);

    // recent files and directories
    SetComboBoxText(MaxRecentOpenFilesComboBox,IntToStr(MaxRecentOpenFiles));
    SetComboBoxText(MaxRecentProjectFilesComboBox,IntToStr(MaxRecentProjectFiles));
    OpenLastProjectAtStartCheckBox.Checked:=OpenLastProjectAtStart;
    ShowCompileDialogCheckBox.Checked:=ShowCompileDialog;
  end;
end;

procedure TFilesOptionsFrame.WriteSettings(AOptions: TEnvironmentOptions);
begin
  with AOptions do
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

    // recent files and directories
    MaxRecentOpenFiles:=StrToIntDef(
        MaxRecentOpenFilesComboBox.Text,MaxRecentOpenFiles);
    MaxRecentProjectFiles:=StrToIntDef(
        MaxRecentProjectFilesComboBox.Text,MaxRecentProjectFiles);
    OpenLastProjectAtStart:=OpenLastProjectAtStartCheckBox.Checked;
    ShowCompileDialog := ShowCompileDialogCheckBox.Checked;
  end;
end;

function TFilesOptionsFrame.CheckLazarusDir: boolean;
var
  NewLazarusDir: string;
  StopChecking: boolean;
begin
  Result := False;
  NewLazarusDir := LazarusDirComboBox.Text;
  Result := SimpleDirectoryCheck(FOldLazarusDir,NewLazarusDir,
                              lisEnvOptDlgLazarusDirNotFoundMsg,StopChecking);
  if (not Result) or StopChecking then
    Exit;

  // lazarus directory specific tests
  NewLazarusDir := AppendPathDelim(NewLazarusDir);
  if not CheckLazarusDirectory(NewLazarusDir) then
  begin
    Result:=(MessageDlg(Format(lisEnvOptDlgInvalidLazarusDir,[NewLazarusDir]),
               mtWarning,[mbIgnore,mbCancel],0)=mrIgnore);
    exit;
  end;
  Result := true;
end;

function TFilesOptionsFrame.IsFPCSourceDir: boolean;
var
  NewFPCSrcDir: string;
  StopChecking: boolean;
begin
  NewFPCSrcDir:=FPCSourceDirComboBox.Text;
  Result:=SimpleDirectoryCheck(FOldFPCSourceDir,NewFPCSrcDir,
                               lisEnvOptDlgFPCSrcDirNotFoundMsg,StopChecking);
  if (not Result) or StopChecking then exit;

  // FPC source directory specific tests
  if not CheckFPCSourceDir(NewFPCSrcDir) then begin
    Result:=(MessageDlg(Format(lisEnvOptDlgInvalidFPCSrcDir,[NewFPCSrcDir]),
               mtWarning,[mbIgnore,mbCancel],0)=mrIgnore);
    exit;
  end;
  Result:=true;
end;

function TFilesOptionsFrame.CheckTestDir: boolean;
var
  NewTestDir: string;
  StopChecking: boolean;
begin
  NewTestDir:=TestBuildDirComboBox.Text;
  Result:=SimpleDirectoryCheck(FOldTestDir,NewTestDir,
                               lisEnvOptDlgTestDirNotFoundMsg,StopChecking);
  if (not Result) or StopChecking then exit;
end;

initialization
  {$I options_files.lrs}

end.

