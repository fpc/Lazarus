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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit files_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, Dialogs, Controls,
  EnvironmentOpts, MacroIntf, LazarusIDEStrConsts, InputHistory, LazConf,
  IDEProcs, IDEOptionsIntf;

type

  { TFilesOptionsFrame }

  TFilesOptionsFrame = class(TAbstractIDEOptionsEditor)
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
    function Check: Boolean; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
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
        // check make filename
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

procedure TFilesOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
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
    Add('make');
    Add('gmake');
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
    lisEnvOptDlgInvalidMakeFilename,lisEnvOptDlgInvalidMakeFilenameMsg,true)
  then
    Exit;
  // check test directory
  if not CheckTestDir then 
    Exit;
  Result := True;
end;

procedure TFilesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
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
  Result:=IDEMacros.SubstituteMacros(NewFPCSrcDir);
  if not Result then begin
    Result:=(MessageDlg(Format(lisEnvOptDlgInvalidFPCSrcDir,[NewFPCSrcDir]),
               mtWarning,[mbIgnore,mbCancel],0)=mrIgnore);
    exit;
  end;
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

class function TFilesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  {$I files_options.lrs}
  RegisterIDEOptionsEditor(GroupEnvironment, TFilesOptionsFrame, EnvOptionsFiles);
end.

