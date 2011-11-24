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
  Classes, SysUtils, LCLProc, FileUtil, Forms, StdCtrls, Dialogs, Controls,
  EnvironmentOpts, MacroIntf, LazarusIDEStrConsts, InputHistory, LazConf,
  IDEProcs, IDEOptionsIntf;

type

  { TFilesOptionsFrame }

  TFilesOptionsFrame = class(TAbstractIDEOptionsEditor)
    AutoCloseCompileDialogCheckBox: TCheckBox;
    CompilerMessagesButton:TButton;
    CompilerPathButton:TButton;
    CompilerPathComboBox:TComboBox;
    FPCSourceDirButton:TButton;
    FPCSourceDirComboBox:TComboBox;
    CompilerPathLabel:TLabel;
    FPCSourceDirLabel:TLabel;
    CompilerMessagesLabel:TLabel;
    MakePathButton:TButton;
    MakePathComboBox:TComboBox;
    TestBuildDirButton:TButton;
    TestBuildDirComboBox:TComboBox;
    CompilerMessagesComboBox:TComboBox;
    TestBuildDirLabel:TLabel;
    MakePathLabel:TLabel;
    LazarusDirButton:TButton;
    LazarusDirComboBox:TComboBox;
    LazarusDirLabel:TLabel;
    MaxRecentOpenFilesComboBox: TComboBox;
    MaxRecentOpenFilesLabel: TLabel;
    MaxRecentProjectFilesComboBox: TComboBox;
    MaxRecentProjectFilesLabel: TLabel;
    OpenLastProjectAtStartCheckBox: TCheckBox;
    ShowCompileDialogCheckBox: TCheckBox;
    procedure CompilerMessagesButtonClick(Sender:TObject);
    procedure FilesButtonClick(Sender: TObject);
    procedure DirectoriesButtonClick(Sender: TObject);
    procedure ShowCompileDialogCheckBoxChange(Sender: TObject);
  private
    fLoaded: Boolean;
    FOldLazarusDir: string;
    FOldCompilerFilename: string;
    FOldFPCSourceDir: string;
    FOldMakeFilename: string;
    FOldTestDir: string;
    FSaved: Boolean;
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

{$R *.lfm}

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
        SetComboBoxText(CompilerPathComboBox,AFilename,cstFilename);
        CheckExecutable(FOldCompilerFilename,CompilerPathComboBox.Text,
          lisEnvOptDlgInvalidCompilerFilename,
          lisEnvOptDlgInvalidCompilerFilenameMsg);
      end else if Sender=MakePathButton then begin
        // check make filename
        SetComboBoxText(MakePathComboBox,AFilename,cstFilename);
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

procedure TFilesOptionsFrame.CompilerMessagesButtonClick(Sender:TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:=lisChooseCompilerMessages;
    OpenDialog.Filter:='FPC message file (*.msg)|*.msg|Any file|'+AllFilesMask;
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(CompilerMessagesComboBox,AFilename,cstFilename);
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
        SetComboBoxText(LazarusDirComboBox,ADirectoryName,cstFilename);
        CheckLazarusDir;
      end else if Sender=FPCSourceDirButton then begin
        // check fpc source directory
        SetComboBoxText(FPCSourceDirComboBox,ADirectoryName,cstFilename);
        IsFPCSourceDir;
      end else if Sender=TestBuildDirButton then begin
        // check test directory
        SetComboBoxText(TestBuildDirComboBox,ADirectoryName,cstFilename);
        CheckTestDir;
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
  MaxRecentProjectFilesLabel.Caption:=dlgMaxRecentProjs;
  OpenLastProjectAtStartCheckBox.Caption:=dlgQOpenLastPrj;
  ShowCompileDialogCheckBox.Caption:=dlgQShowCompileDialog;
  AutoCloseCompileDialogCheckBox.Caption:=dlgQAutoCloseCompileDialog;
  LazarusDirLabel.Caption:=dlgLazarusDir;

  with LazarusDirComboBox.Items do
  begin
    BeginUpdate;
    Add(ProgramDirectory(true));
    EndUpdate;
  end;

  CompilerPathLabel.Caption:=Format(dlgFpcPath,[GetDefaultCompilerFilename]);
  FPCSourceDirLabel.Caption:=dlgFpcSrcPath;
  MakePathLabel.Caption:=dlgMakePath;

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
    Add('/tmp');
    Add('/var/tmp');
    Add('c:/tmp');
    Add('c:/windows/temp');
    EndUpdate;
  end;
  CompilerMessagesLabel.Caption:=dlgCompilerMessages;
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
  if fLoaded then exit;
  fLoaded:=true;
  with AOptions as TEnvironmentOptions do
  begin
    LazarusDirComboBox.Items.Assign(LazarusDirHistory);
    FOldLazarusDir:=LazarusDirectory;
    SetComboBoxText(LazarusDirComboBox,LazarusDirectory,cstFilename,MaxComboBoxCount);
    with CompilerPathComboBox do
    begin
      Items.BeginUpdate;
      Items.Assign(CompilerFileHistory);
      AddFilenameToList(Items,FindDefaultCompilerPath);
      AddFilenameToList(Items,FindDefaultExecutablePath('fpc'+GetExecutableExt));
      Items.EndUpdate;
    end;

    FOldCompilerFilename:=CompilerFilename;
    SetComboBoxText(CompilerPathComboBox,CompilerFilename,cstFilename,MaxComboBoxCount);
    FPCSourceDirComboBox.Items.Assign(FPCSourceDirHistory);
    FOldFPCSourceDir:=FPCSourceDirectory;
    SetComboBoxText(FPCSourceDirComboBox,FPCSourceDirectory,cstFilename,MaxComboBoxCount);
    MakePathComboBox.Items.Assign(MakeFileHistory);
    FOldMakeFilename:=MakeFilename;
    SetComboBoxText(MakePathComboBox,MakeFilename,cstFilename,MaxComboBoxCount);
    TestBuildDirComboBox.Items.Assign(TestBuildDirHistory);
    FOldTestDir:=TestBuildDirectory;
    SetComboBoxText(TestBuildDirComboBox,TestBuildDirectory,cstFilename,MaxComboBoxCount);
    CompilerMessagesComboBox.Items.Assign(CompilerMessagesFileHistory);
    SetComboBoxText(CompilerMessagesComboBox,CompilerMessagesFilename,cstFilename,MaxComboBoxCount);

    // recent files and directories
    SetComboBoxText(MaxRecentOpenFilesComboBox,IntToStr(MaxRecentOpenFiles),cstCaseInsensitive);
    SetComboBoxText(MaxRecentProjectFilesComboBox,IntToStr(MaxRecentProjectFiles),cstCaseInsensitive);
    OpenLastProjectAtStartCheckBox.Checked:=OpenLastProjectAtStart;
    ShowCompileDialogCheckBox.Checked:=ShowCompileDialog;
    AutoCloseCompileDialogCheckBox.Checked:=AutoCloseCompileDialog;
    AutoCloseCompileDialogCheckBox.Enabled:=ShowCompileDialogCheckBox.Checked;
  end;
end;

procedure TFilesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if FSaved then exit;
  FSaved:=true;
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
    CompilerMessagesFileHistory.Assign(CompilerMessagesComboBox.Items);
    CompilerMessagesFilename:=CompilerMessagesComboBox.Text;

    // recent files and directories
    MaxRecentOpenFiles:=StrToIntDef(
        MaxRecentOpenFilesComboBox.Text,MaxRecentOpenFiles);
    MaxRecentProjectFiles:=StrToIntDef(
        MaxRecentProjectFilesComboBox.Text,MaxRecentProjectFiles);
    OpenLastProjectAtStart:=OpenLastProjectAtStartCheckBox.Checked;
    ShowCompileDialog := ShowCompileDialogCheckBox.Checked;
    AutoCloseCompileDialog := AutoCloseCompileDialogCheckBox.Checked;
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
  RegisterIDEOptionsEditor(GroupEnvironment, TFilesOptionsFrame, EnvOptionsFiles);
end.

