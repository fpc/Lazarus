{ /***************************************************************************
                 publishprojectdlg.pp  -  Lazarus IDE unit
                 -----------------------------------------

 ***************************************************************************/

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

  Author: Juha Manninen

  Abstract:
    - TPublishProjectDialog
    The dialog for TPublishModuleOptions to publish projects and packages.

}
unit PublishProjectDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  // LCL
  LCLType, Forms, Controls, StdCtrls, Dialogs, ExtCtrls, Buttons, ButtonPanel,
  // LazUtils
  FileUtil, LazFileUtils, LazLoggerBase,
  // IdeIntf
  IDEWindowIntf, IDEHelpIntf, IDEDialogs, IDEImagesIntf, ProjPackIntf, CompOptsIntf,
  // IDE
  ProjectDefs, Project, PackageDefs, PublishModule, IDEOptionDefs, InputHistory,
  LazarusIDEStrConsts, IDEProcs, EnvironmentOpts;

type
  { TPublishProjectDialog }

  TPublishProjectDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    DestDirGroupBox: TGroupBox;
    DestDirComboBox: TComboBox;
    BrowseDestDirBitBtn: TBitBtn;
    ExcFilterSimpleSyntaxCheckbox: TCheckBox;
    ExcludeFilterCombobox: TComboBox;
    ExcludeFilterLabel: TLabel;
    FiltersPanel: TPanel;
    IncFilterSimpleSyntaxCheckbox: TCheckBox;
    IncludeFilterCombobox: TComboBox;
    IncludeFilterLabel: TLabel;
    Label1: TLabel;
    NoteLabel: TLabel;
    OptionsGroupbox: TGroupBox;
    CompressCheckbox: TCheckBox;
    UseFiltersCheckbox: TCheckBox;
    procedure BrowseDestDirBitBtnCLICK(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OkButtonCLICK(Sender: TObject);
    procedure SaveSettingsButtonClick(Sender: TObject);
    procedure UseFiltersCheckboxClick(Sender: TObject);
  private
    FOptions: TPublishModuleOptions;
    procedure SetComboBox(AComboBox: TComboBox; const NewText: string;
                          MaxItemCount: integer);
    procedure LoadHistoryLists;
    procedure SaveHistoryLists;
    procedure SetOptions(const AValue: TPublishModuleOptions);
    function CheckFilter: boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromOptions(SrcOpts: TPublishModuleOptions);
    procedure SaveToOptions(DestOpts: TPublishModuleOptions);
    property Options: TPublishModuleOptions read FOptions write SetOptions;
  end;

  { TPublisher }

  TPublisher = class(TFileSearcher)
  private
    FOptions: TPublishModuleOptions;
    FProjPack: TIDEProjPackBase;
    FSrcDir, FDestDir: string;
    // TopDir contains all project/package directories.
    //  Some of them may be above the main project/package directory.
    FTopDir: string;
    // Project/package member files already copied. Not copied again by filters.
    FCopiedFiles: TStringList;
    // Copying by filters failed.
    FCopyFailedCount: Integer;
    procedure AdjustTopDir(const AFileName: string);
    function CopyAFile(const AFileName: string): TModalResult;
    function CopyProjectFiles: TModalResult;
    function CopyPackageFiles: TModalResult;
    function WriteProjectInfo: TModalResult;
    function WritePackageInfo: TModalResult;
    function Compress: TModalResult;
  protected
    procedure DoFileFound; override;    // Called by TFileSearcher.Search.
    procedure DoDirectoryFound; override;
  public
    constructor Create(AOptions: TPublishModuleOptions);
    destructor Destroy; override;
    function Run: TModalResult;
  end;


function ShowPublishDialog(AOptions: TPublishModuleOptions): TModalResult;
function PublishAModule(AOptions: TPublishModuleOptions): TModalResult;


implementation

{$R *.lfm}

function ShowPublishDialog(AOptions: TPublishModuleOptions): TModalResult;
var
  PublishProjectDialog: TPublishProjectDialog;
begin
  PublishProjectDialog:=TPublishProjectDialog.Create(nil);
  with PublishProjectDialog do
  begin
    Options:=AOptions;
    Result:=ShowModal;
    Free;
  end;
end;

function PublishAModule(AOptions: TPublishModuleOptions): TModalResult;
begin
  with TPublisher.Create(AOptions) do
  try
    Result := Run;
  finally
    Free;
  end;
end;

{ TPublisher }

constructor TPublisher.Create(AOptions: TPublishModuleOptions);
begin
  inherited Create;
  FOptions := AOptions;
  FProjPack := FOptions.Owner as TIDEProjPackBase;
  FTopDir := FProjPack.Directory;     // Initial value for TopDir. It may change.
  FSrcDir := FTopDir;
  FDestDir:=TrimAndExpandDirectory(RealPublishDir(FOptions));
  DebugLn(['TPublisher: Source      Directory = ', FSrcDir]);
  DebugLn(['TPublisher: Destination Directory = ', FDestDir]);
  FCopiedFiles := TStringList.Create;
end;

destructor TPublisher.Destroy;
begin
  FCopiedFiles.Free;
  inherited Destroy;
end;

// The next 2 methods will be called from TFileSearcher.Search.

procedure TPublisher.DoFileFound;
// Copy a found file if it passes the filter.
var
  NewPath: string;
begin
  if FCopiedFiles.IndexOf(FileName) >= 0 then
    DebugLn(['DoFileFound: Already copied file ', FileName])
  else if FOptions.FileCanBePublished(FileName) then
  begin
    NewPath:=StringReplace(FileName, FSrcDir, FDestDir, []);
    DebugLn(['DoFileFound: Copying file ', FileName, ' -> ', NewPath]);
    if not CopyFile(FileName, NewPath, [cffCreateDestDirectory,cffPreserveTime]) then
      Inc(FCopyFailedCount);
  end
  else begin
    DebugLn(['DoFileFound: Rejected file ', FileName]);
  end;
end;

procedure TPublisher.DoDirectoryFound;
begin
  DebugLn(['DoDirectoryFound: ', FileName]);
  // Directory is already created by the cffCreateDestDirectory flag.
end;

// Methods below are for copying project/package members.

procedure TPublisher.AdjustTopDir(const AFileName: string);
// Adjust the actual top directory. It will differ from the
//  main dir when project/package has files up in directory structure.
var
  RelPath: string;
  Adjusted: Boolean;
begin
  Assert(Pos('\',AFileName) = 0, 'AdjustTopDir: File name contains a backslash.');
  RelPath := ExtractRelativePath(FTopDir, AFilename);
  Adjusted := False;
  while Copy(RelPath, 1, 3) = '../' do
  begin
    FTopDir := FTopDir + '../'; // This file is in upper dir. Move TopDir up, too.
    Delete(RelPath, 1, 3);
    Adjusted := True;
  end;
  if Adjusted then
  begin
    FTopDir := ResolveDots(FTopDir);
    DebugLn(['Adjusted TopDir: ', FTopDir, ' based on file ', AFilename]);
  end;
end;

function TPublisher.CopyAFile(const AFileName: string): TModalResult;
var
  RelPath, LfmFile: string;
begin
  Result := mrOK;
  if FCopiedFiles.IndexOf(AFilename) >= 0 then exit;     // Already copied.
  RelPath := ExtractRelativePath(FTopDir, AFilename);
  DebugLn(['CopyAFile: File ', AFilename, ' -> ', FDestDir+RelPath]);
  if CopyFile(AFilename, FDestDir+RelPath,
              [cffCreateDestDirectory,cffPreserveTime]) then
  begin
    FCopiedFiles.Add(AFilename);
    if FilenameIsPascalUnit(AFilename) then
    begin    // Copy .lfm or .dfm file even if it is not part of project/package.
      LfmFile := ChangeFileExt(AFilename, '.lfm');
      if not FileExistsUTF8(LfmFile) then
      begin
        LfmFile := ChangeFileExt(AFilename, '.dfm');
        if not FileExistsUTF8(LfmFile) then
          LfmFile := '';
      end;
      if LfmFile <> '' then
        Result := CopyAFile(LfmFile);  // Recursive call.
    end;
  end
  else
    Result := mrCancel;
end;

function TPublisher.CopyProjectFiles: TModalResult;
var
  CurProject: TProject;
  CurUnitInfo: TUnitInfo;
  i: Integer;
  ResFile: String;
begin
  Result := mrOK;
  CurProject := TProject(FProjPack);
  // First adjust the TopDir based on relative file paths.
  for i:=0 to CurProject.UnitCount-1 do
  begin
    CurUnitInfo := CurProject.Units[i];
    if CurUnitInfo.IsPartOfProject then
      AdjustTopDir(CurUnitInfo.Filename);
  end;
  // Then copy the project files
  for i:=0 to CurProject.UnitCount-1 do
  begin
    CurUnitInfo := CurProject.Units[i];
    if CurUnitInfo.IsPartOfProject then
    begin
      CopyAFile(CurUnitInfo.Filename);
      if CurUnitInfo.IsMainUnit then
      begin                // Copy the main resource file in any case.
        ResFile := ChangeFileExt(CurUnitInfo.Filename, '.res');
        if FileExistsUTF8(ResFile) then
          Result := CopyAFile(ResFile);
      end;
    end;
  end;
end;

function TPublisher.CopyPackageFiles: TModalResult;
var
  CurPackage: TLazPackage;
  PkgFile: TPkgFile;
  i: Integer;
begin
  // Copy Package
  Result := mrOK;
  CurPackage := TLazPackage(FProjPack);
  // First adjust the TopDir based on relative file paths.
  for i:=0 to CurPackage.FileCount-1 do
  begin
    PkgFile := CurPackage.Files[i];
    AdjustTopDir(PkgFile.Filename);
  end;
  // Then copy the package files
  for i:=0 to CurPackage.FileCount-1 do
  begin
    PkgFile := CurPackage.Files[i];
    CopyAFile(PkgFile.Filename);
  end;
end;

function TPublisher.WriteProjectInfo: TModalResult;
var
  CurProject: TProject;
  NewProjFilename: string;
begin
  CurProject := TProject(FOptions.Owner);
  NewProjFilename := FDestDir + ExtractRelativePath(FTopDir, CurProject.ProjectInfoFile);
  DeleteFileUTF8(NewProjFilename);
  DebugLn(['WriteProjectInfo: ProjectInfo = ', CurProject.ProjectInfoFile,
           ', NewProjFilename = ', NewProjFilename]);
  Assert(CurProject.PublishOptions = FOptions, 'CurProject.PublishOptions <> FOptions');
  FCopiedFiles.Add(CurProject.ProjectInfoFile); // Do not later by filter.
  Result := CurProject.WriteProject(
      CurProject.PublishOptions.WriteFlags+pwfSkipSessionInfo+[pwfIgnoreModified],
      NewProjFilename,nil);
  if Result<>mrOk then
    DebugLn('Hint: [TPublisher] CurProject.WriteProject failed');
end;

function TPublisher.WritePackageInfo: TModalResult;
begin
  Result := mrOK;
  // ToDo
end;

function TPublisher.Compress: TModalResult;
begin
  Result := mrOK;
  // ToDo
end;

function TPublisher.Run: TModalResult;
begin
  Result:=mrCancel;

  if FDestDir='' then begin
    IDEMessageDialog('Invalid publishing Directory',
      'Destination directory for publishing is empty.',mtError,
      [mbCancel]);
    exit;
  end;
  // Don't try to copy to a subdirectory of FSrcDir.
  if (CompareFilenames(FSrcDir,FDestDir)=0)
  {$ifdef CaseInsensitiveFilenames}
  or AnsiStartsText(FSrcDir, FDestDir)
  {$ELSE}
  or AnsiStartsStr(FSrcDir, FDestDir)
  {$ENDIF}
  then begin
    IDEMessageDialog(lisInvalidPublishingDirectory,
      Format(lisSourceDirectoryAndDestinationDirectoryAreTheSameMa,
             [FSrcDir, LineEnding, FDestDir, LineEnding, LineEnding]),
      mtError, [mbCancel]);
    exit;
  end;

  // checking "command after" was here

  // clear destination directory
  if DirPathExists(FDestDir) then
  begin
    // ask user, if destination can be deleted
    if IDEMessageDialog(lisClearDirectory,
      Format(lisInOrderToCreateACleanCopyOfTheProjectPackageAllFil,
             [LineEnding+LineEnding, FDestDir]),
      mtConfirmation, [mbYes,mbNo])<>mrYes
    then
      exit;

    if (not DeleteDirectory(ChompPathDelim(FDestDir),true)) then
    begin
      IDEMessageDialog(lisUnableToCleanUpDestinationDirectory,
        Format(lisUnableToCleanUpPleaseCheckPermissions, [FDestDir, LineEnding]),
        mtError,[mbOk]);
      exit;
    end;
  end;

  // Write a project/package files and then info file
  if FOptions is TPublishProjectOptions then
  begin
    Result := CopyProjectFiles;
    if Result<>mrOk then exit;
    Result := WriteProjectInfo;
    // Store project's BackupDir and UnitOutputDir for filtering files.
    TPublishProjectOptions(FOptions).InitValues(
        EnvironmentOptions.BackupInfoProjectFiles.SubDirectory,
        TProject(FOptions.Owner).CompilerOptions.GetUnitOutPath(True,coptUnparsed)
    );
  end
  else begin
    Result := CopyPackageFiles;
    if Result<>mrOk then exit;
    Result := WritePackageInfo;
  end;
  if Result<>mrOk then exit;

  // Copy extra files in project/package directory
  if FOptions.UseFileFilters then
  begin
    FCopyFailedCount:=0;
    Search(FSrcDir); // Copy only under project/package main dir (FSrcDir).
    if FCopyFailedCount <> 0 then
    begin
      DebugLn('Hint: [TPublisher] Copying files failed');
      exit(mrCancel);
    end;
  end;

  // Compress the resulting project/package
  if FOptions.CompressFinally then
    Result := Compress;

  if Result = mrOK then
    IDEMessageDialog(lisSuccess, 'Published to '+FDestDir, mtInformation,[mbOk]);
end;

{ TPublishProjectDialog }

constructor TPublishProjectDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self, 600, 400);
  LoadHistoryLists;
end;

destructor TPublishProjectDialog.Destroy;
begin
  SaveHistoryLists;
  inherited Destroy;
end;

procedure TPublishProjectDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TPublishProjectDialog.FormCreate(Sender: TObject);
begin
  DestDirGroupBox.Caption:=lisDestinationDirectory;
  NoteLabel.Caption:=lisPublishModuleNote;
  UseFiltersCheckbox.Caption:=lisUseFiltersForExtraFiles;

  IncludeFilterLabel.Caption:=lisIncludeFilter;
  IncFilterSimpleSyntaxCheckbox.Caption:=lisSimpleSyntax;
  IncFilterSimpleSyntaxCheckbox.Hint:=
    lisNormallyTheFilterIsARegularExpressionInSimpleSynta;

  ExcludeFilterLabel.Caption:=lisExcludeFilter;
  ExcFilterSimpleSyntaxCheckbox.Caption:=lisSimpleSyntax;
  ExcFilterSimpleSyntaxCheckbox.Hint:=
    lisNormallyTheFilterIsARegularExpressionInSimpleSynta;

  OptionsGroupbox.Caption:=lisOptions;
  CompressCheckbox.Caption:=lisCompress;
  CompressCheckbox.Hint:=lisCompressHint;

  ButtonPanel1.OkButton.Caption := lisMenuOk;
  ButtonPanel1.OKButton.OnClick := @OkButtonCLICK;

  ButtonPanel1.CloseButton.Caption := lisSaveSettings;
  ButtonPanel1.CloseButton.ModalResult := mrNone;
  ButtonPanel1.CloseButton.Kind := bkCustom;
  ButtonPanel1.CloseButton.LoadGlyphFromStock(idButtonSave);
  if ButtonPanel1.CloseButton.Glyph.Empty then
    IDEImages.AssignImage(ButtonPanel1.CloseButton, 'laz_save');
  ButtonPanel1.CloseButton.OnClick := @SaveSettingsButtonCLICK;

  ButtonPanel1.HelpButton.OnClick := @HelpButtonClick;
end;

procedure TPublishProjectDialog.BrowseDestDirBitBtnCLICK(Sender: TObject);
var
  SelectDirDialog: TSelectDirectoryDialog;
  NewDir: String;
begin
  SelectDirDialog:=TSelectDirectoryDialog.Create(Self);
  InputHistories.ApplyFileDialogSettings(SelectDirDialog);
  SelectDirDialog.Title:=lisChooseDirectory;
  if SelectDirDialog.Execute then begin
    NewDir:=ExpandFileNameUTF8(SelectDirDialog.Filename);
    SetComboBox(DestDirComboBox,NewDir,20);
  end;
  SelectDirDialog.Free;
end;

procedure TPublishProjectDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TPublishProjectDialog.OkButtonCLICK(Sender: TObject);
begin
  if not CheckFilter then exit;
  if Options<>nil then SaveToOptions(Options);
end;

procedure TPublishProjectDialog.SaveSettingsButtonClick(Sender: TObject);
begin
  if not CheckFilter then exit;
  if Options<>nil then SaveToOptions(Options);
end;

procedure TPublishProjectDialog.UseFiltersCheckboxClick(Sender: TObject);
begin
  FiltersPanel.Enabled := (Sender as TCheckBox).Checked;
end;

procedure TPublishProjectDialog.SetComboBox(AComboBox: TComboBox;
  const NewText: string; MaxItemCount: integer);
begin
  AComboBox.AddHistoryItem(NewText,MaxItemCount,true,false);
end;

procedure TPublishProjectDialog.LoadHistoryLists;
var
  hl: THistoryList;
begin
  // destination directories
  hl:=InputHistories.HistoryLists.GetList(hlPublishProjectDestDirs,true,rltFile);
  hl.AppendEntry(GetForcedPathDelims('$(TestDir)/publishedproject/'));
  hl.AppendEntry(GetForcedPathDelims('$(TestDir)/publishedpackage/'));
  hl.AppendEntry(GetForcedPathDelims('$(ProjPath)/published/'));
  DestDirComboBox.Items.Assign(hl);

  // file filters
  hl:=InputHistories.HistoryLists.GetList(hlPublishProjectIncludeFileFilter,
                                          true,rltFile);
  if hl.Count=0 then
    hl.Add(DefPublModIncFilter);
  IncludeFilterCombobox.Items.Assign(hl);

  hl:=InputHistories.HistoryLists.GetList(hlPublishProjectExcludeFileFilter,
                                          true,rltFile);
  if hl.Count=0 then
    hl.Add(DefPublModExcFilter);
  ExcludeFilterCombobox.Items.Assign(hl);
end;

procedure TPublishProjectDialog.SaveHistoryLists;
var
  hl: THistoryList;
begin
  // destination directories
  SetComboBox(DestDirComboBox,DestDirComboBox.Text,20);
  hl:=InputHistories.HistoryLists.GetList(hlPublishProjectDestDirs,true,rltFile);
  hl.Assign(DestDirComboBox.Items);

  // file filters
  SetComboBox(IncludeFilterCombobox,IncludeFilterCombobox.Text,20);
  hl:=InputHistories.HistoryLists.GetList(hlPublishProjectIncludeFileFilter,true,rltFile);
  hl.Assign(IncludeFilterCombobox.Items);
  SetComboBox(ExcludeFilterCombobox,ExcludeFilterCombobox.Text,20);
  hl:=InputHistories.HistoryLists.GetList(hlPublishProjectExcludeFileFilter,true,rltFile);
  hl.Assign(ExcludeFilterCombobox.Items);
end;

procedure TPublishProjectDialog.SetOptions(const AValue: TPublishModuleOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
  if FOptions is TPublishPackageOptions then
    Caption:=lisPkgEditPublishPackage
  else
    Caption:=lisPublishProject;
  LoadFromOptions(FOptions);
end;

function TPublishProjectDialog.CheckFilter: boolean;
begin
  Result:=false;
  if Options<>nil then begin
    if not Options.IncludeFilterValid then begin
      if IDEMessageDialog(lisCCOErrorCaption, lisPublProjInvalidIncludeFilter,
                          mtError, [mbIgnore,mbCancel]) = mrCancel
      then exit;
    end;
    if not Options.ExcludeFilterValid then begin
      if IDEMessageDialog(lisCCOErrorCaption, lisPublProjInvalidExcludeFilter,
        mtError, [mbIgnore,mbCancel]) = mrCancel
      then exit;
    end;
  end;
  Result:=true;
end;

procedure TPublishProjectDialog.LoadFromOptions(SrcOpts: TPublishModuleOptions);
begin
  // destination
  SeTComboBox(DestDirComboBox,SrcOpts.DestinationDirectory,20);

  // file filters
  CompressCheckbox.Checked:=SrcOpts.CompressFinally;
  UseFiltersCheckbox.Checked:=SrcOpts.UseFileFilters;
  IncFilterSimpleSyntaxCheckbox.Checked:=SrcOpts.IncludeFilterSimpleSyntax;
  SeTComboBox(IncludeFilterCombobox,SrcOpts.IncludeFileFilter,20);
  ExcFilterSimpleSyntaxCheckbox.Checked:=SrcOpts.ExcludeFilterSimpleSyntax;
  SeTComboBox(ExcludeFilterCombobox,SrcOpts.ExcludeFileFilter,20);
end;

procedure TPublishProjectDialog.SaveToOptions(DestOpts: TPublishModuleOptions);
begin
  // destination
  DestOpts.DestinationDirectory:=DestDirComboBox.Text;

  // file filters
  DestOpts.CompressFinally:=CompressCheckbox.Checked;
  DestOpts.UseFileFilters:=UseFiltersCheckbox.Checked;
  DestOpts.IncludeFilterSimpleSyntax:=IncFilterSimpleSyntaxCheckbox.Checked;
  DestOpts.IncludeFileFilter:=IncludeFilterCombobox.Text;
  DestOpts.ExcludeFilterSimpleSyntax:=ExcFilterSimpleSyntaxCheckbox.Checked;
  DestOpts.ExcludeFileFilter:=ExcludeFilterCombobox.Text;
end;

end.

