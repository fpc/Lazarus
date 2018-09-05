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

  Author: Juha Manninen

  Abstract:
    - TPublishModuleDialog
    The dialog for TPublishModuleOptions to publish projects and packages.

}
unit PublishModuleDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Zipper,
  // LCL
  LCLType, Forms, Controls, StdCtrls, Dialogs, ExtCtrls, Buttons, ButtonPanel,
  // LazUtils
  FileUtil, LazFileUtils, LazLoggerBase,
  // IdeIntf
  IDEWindowIntf, IDEHelpIntf, IDEDialogs, IDEImagesIntf, ProjPackIntf, CompOptsIntf, LazIDEIntf,
  // IDE
  ProjectDefs, Project, PackageDefs, PublishModule, IDEOptionDefs, InputHistory,
  LazarusIDEStrConsts, IDEProcs, EnvironmentOpts, CompilerOptions;

type
  { TPublishModuleDialog }

  TPublishModuleDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    DestDirGroupBox: TGroupBox;
    DestDirComboBox: TComboBox;
    BrowseDestDirBitBtn: TBitBtn;
    FilterCombobox: TComboBox;
    FilterSimpleSyntaxCheckbox: TCheckBox;
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
    FProjDirs: TStringList;
    FBackupDir, FLibDir: String;
    procedure AdjustTopDir(const AFileName: string);
    function CopyAFile(const AFileName: string): TModalResult;
    function CopyProjectFiles: TModalResult;
    function CopyPackageFiles: TModalResult;
    function WriteProjectInfo: TModalResult;
    function WritePackageInfo: TModalResult;
    function IsDrive(const AName: String): Boolean;
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
begin
  with TPublishModuleDialog.Create(nil) do
  try
    Options:=AOptions;
    Result:=ShowModal;
  finally
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
var
  COpts: TBaseCompilerOptions;
begin
  inherited Create;
  FOptions := AOptions;
  FProjPack := FOptions.Owner as TIDEProjPackBase;
  FTopDir := FProjPack.Directory;     // Initial value for TopDir. It may change.
  FSrcDir := FTopDir;
  FDestDir := TrimAndExpandDirectory(RealPublishDir(FOptions));
  FBackupDir := AppendPathDelim(FSrcDir
                       + EnvironmentOptions.BackupInfoProjectFiles.SubDirectory);
  COpts := FProjPack.LazCompilerOptions as TBaseCompilerOptions;
  FLibDir := COpts.GetUnitOutPath(True,coptParsed);
  DebugLn(['TPublisher: Source      Directory = ', FSrcDir]);
  DebugLn(['TPublisher: Destination Directory = ', FDestDir]);
  DebugLn(['TPublisher: Backup      Directory = ', FBackupDir]);
  DebugLn(['TPublisher: Lib         Directory = ', FLibDir]);
  FCopiedFiles := TStringList.Create;
  FProjDirs := TStringList.Create;
end;

destructor TPublisher.Destroy;
begin
  FCopiedFiles.Free;
  FProjDirs.Free;
  inherited Destroy;
end;

// The next 2 methods will be called from TFileSearcher.Search.

procedure TPublisher.DoFileFound;
// Copy a found file if it passes the filter.
var
  CurDir: string;
begin
  if FCopiedFiles.IndexOf(FileName) >= 0 then
  begin
    DebugLn(['DoFileFound: Already copied file ', FileName]);
    Exit;
  end;
  CurDir := ExtractFilePath(FileName);
  if (CurDir = FBackupDir) or (CurDir = FLibDir) then
  begin
    DebugLn(['DoFileFound: In Backup or Output directory, not copied: ', FileName]);
    Exit;
  end;
  if AnsiStartsStr(FDestDir, CurDir) then
  begin
    DebugLn(['DoFileFound: In destination directory, not copied: ', FileName]);
    Exit;
  end;
  if FOptions.FileCanBePublished(FileName) then
  begin
    if CopyAFile(FileName) <> mrOK then
      Inc(FCopyFailedCount);
  end
  else
    DebugLn(['DoFileFound: Rejected file ', FileName]);
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
  RelPath, UpALevel: string;
  Adjusted: Boolean;
begin
  UpALevel := '..' + DirectorySeparator;
  RelPath := ExtractRelativePath(FTopDir, AFilename);
  Adjusted := False;
  while Copy(RelPath, 1, 3) = UpALevel do
  begin
    FTopDir := FTopDir + UpALevel; // This file is in upper dir. Move TopDir up, too.
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
  Drive: String;
begin
  Result := mrOK;
  if FCopiedFiles.IndexOf(AFilename) >= 0 then exit;     // Already copied.
  RelPath := ExtractRelativePath(FTopDir, AFilename);
  Drive := ExtractFileDrive(RelPath);
  if Trim(Drive) <> '' then
    RelPath := StringReplace(RelPath, AppendPathDelim(Drive), '', [rfIgnoreCase]);
  DebugLn(['CopyAFile: File ', AFilename, ' -> ', FDestDir+RelPath]);
  if CopyFile(AFilename, FDestDir + RelPath,
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
  RelatedFile: String;
  CurDir: String;
begin
  Result := mrOK;
  CurProject := TProject(FProjPack);
  FProjDirs.Clear;
  FProjDirs.Sorted := True;
  FProjDirs.Duplicates := dupIgnore;
  // First adjust the TopDir based on relative file paths.
  for i:=0 to CurProject.UnitCount-1 do
  begin
    CurUnitInfo := CurProject.Units[i];
    if CurUnitInfo.IsPartOfProject then
    begin
      AdjustTopDir(CurUnitInfo.Filename);
      CurDir := ExtractFilePath(CurUnitInfo.Filename);
      FProjDirs.Add(CurDir);
    end;
  end;
  // Then copy the project files
  for i:=0 to CurProject.UnitCount-1 do
  begin
    CurUnitInfo := CurProject.Units[i];
    if CurUnitInfo.IsPartOfProject then
    begin
      Result := CopyAFile(CurUnitInfo.Filename);
      if Result<> mrOk then Exit;
      if CurUnitInfo.IsMainUnit then
      begin                // Copy the main resource file in any case.
        RelatedFile := ChangeFileExt(CurUnitInfo.Filename, '.res');
        if FileExistsUTF8(RelatedFile) then
          Result := CopyAFile(RelatedFile);
        RelatedFile := ChangeFileExt(CurUnitInfo.Filename, '.ico');
        if FileExistsUTF8(RelatedFile) then
          Result := CopyAFile(RelatedFile);
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

function TPublisher.IsDrive(const AName: String): Boolean;
var
  Drive: String;
begin
  Drive := ExtractFileDrive(AName);
  Result := (Trim(Drive) <> '') and (AppendPathDelim(Drive) = AName);
end;

function TPublisher.Compress: TModalResult;
var
  Zipper: TZipper;
  ZipFileEntries: TZipFileEntries;
  CurProject: TProject;
  I: Integer;
  ZipDestDir: String;
  RelPath, Drive: String;
begin
  Result := mrNone;
  CurProject := TProject(FOptions.Owner);
  if CurProject = nil then
    Exit;

  ZipFileEntries := TZipFileEntries.Create(TZipFileEntry);
  try
    if DirPathExists(FDestDir) then
    begin
      for I := 0 to FCopiedFiles.Count - 1 do
      begin
        RelPath := ExtractRelativePath(FTopDir, FCopiedFiles.Strings[I]);
        Drive := ExtractFileDrive(RelPath);
        if Trim(Drive) <> '' then
          RelPath := StringReplace(RelPath, AppendPathDelim(Drive), '', [rfIgnoreCase]);
        ZipFileEntries.AddFileEntry(FDestDir + RelPath, RelPath);
      end;

      if (ZipFileEntries.Count > 0) then
      begin
        Zipper := TZipper.Create;
        try
          ZipDestDir := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath);
          Zipper.FileName := ChangeFileExt(ZipDestDir + ExtractFileName(CurProject.ProjectInfoFile), '.zip');
          try
            Zipper.ZipFiles(ZipFileEntries);
            if FileExists(Zipper.FileName) then
            begin
              DeleteDirectory(FDestDir, True);
              if CopyFile(Zipper.FileName, AppendPathDelim(FDestDir) + ExtractFileName(Zipper.FileName), [cffCreateDestDirectory,cffPreserveTime]) then
              begin
                DeleteFile(Zipper.FileName);
                Result := mrOK;
              end;
            end;
            except
              on E: EZipError do
                IDEMessageDialog(lisError, E.Message, mtInformation, [mbOk]);
            end;
        finally
          Zipper.Free;
        end;
      end;
    end;
  finally
    ZipFileEntries.Free;
  end;
end;

function TPublisher.Run: TModalResult;
var
  I: Integer;
begin
  Result:=mrCancel;

  if FDestDir='' then begin
    IDEMessageDialog(lisInvalidPublishingDirectory,
      lisEmptyDestinationForPublishing, mtError, [mbCancel]);
    exit;
  end;
  // Don't try to copy to a subdirectory of FSrcDir.
  if (CompareFilenames(FSrcDir,FDestDir)=0) then
  begin
    IDEMessageDialog(lisInvalidPublishingDirectory,
      Format(lisSourceAndDestinationAreSame,
             [FSrcDir, LineEnding, FDestDir, LineEnding]),
      mtError, [mbCancel]);
    exit;
  end;

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
    if Result <> mrOk then
    begin
      IDEMessageDialog(lisError, lisCopyFilesFailed, mtInformation,[mbOk]);
      exit;
    end;
    Result := WriteProjectInfo;
    if Result <> mrOk then
    begin
      IDEMessageDialog(lisError, lisWriteProjectInfoFailed, mtInformation,[mbOk]);
      exit;
    end;
  end
  else begin
    Result := CopyPackageFiles;
    if Result <> mrOk then
    begin
      IDEMessageDialog(lisError, lisCopyPackagesFailed, mtInformation,[mbOk]);
      exit;
    end;
    Result := WritePackageInfo;
    if Result <> mrOk then
    begin
      IDEMessageDialog(lisError, lisWritePackageInfoFailed, mtInformation,[mbOk]);
      exit;
    end;

  end;
  if Result<>mrOk then exit;

  // Copy extra files in project/package directory
  if FOptions.UseFileFilters then
  begin
    FCopyFailedCount:=0;
    for I := 0 to FProjDirs.Count - 1 do
    begin
      DebugLn(['Copy extra directory ', FProjDirs.Strings[I]]);
      if IsDrive(FProjDirs.Strings[I]) then
        Search(FProjDirs.Strings[I], '', False)
      else
        Search(FProjDirs.Strings[I]);
    end;
    if FCopyFailedCount <> 0 then
    begin
      IDEMessageDialog(lisError, lisCopyFilesFailed, mtInformation,[mbOk]);
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

{ TPublishModuleDialog }

constructor TPublishModuleDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self, 600, 350);
  LoadHistoryLists;
end;

destructor TPublishModuleDialog.Destroy;
begin
  SaveHistoryLists;
  inherited Destroy;
end;

procedure TPublishModuleDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TPublishModuleDialog.FormCreate(Sender: TObject);
begin
  DestDirGroupBox.Caption:=lisDestinationDirectory;
  NoteLabel.Caption:=lisPublishModuleNote;
  UseFiltersCheckbox.Caption:=lisUseFilterForExtraFiles;
  FilterSimpleSyntaxCheckbox.Caption:=lisSimpleSyntax;
  FilterSimpleSyntaxCheckbox.Hint:=lisNormallyTheFilterIsARegularExpressionInSimpleSynta;

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

procedure TPublishModuleDialog.BrowseDestDirBitBtnCLICK(Sender: TObject);
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

procedure TPublishModuleDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TPublishModuleDialog.OkButtonCLICK(Sender: TObject);
begin
  if not CheckFilter then exit;
  if Options<>nil then
    SaveToOptions(Options);
end;

procedure TPublishModuleDialog.SaveSettingsButtonClick(Sender: TObject);
begin
  if not CheckFilter then exit;
  if Options<>nil then
    SaveToOptions(Options);
end;

procedure TPublishModuleDialog.UseFiltersCheckboxClick(Sender: TObject);
begin
  FilterCombobox.Enabled := (Sender as TCheckBox).Checked;
  FilterSimpleSyntaxCheckbox.Enabled := FilterCombobox.Enabled;
end;

procedure TPublishModuleDialog.SetComboBox(AComboBox: TComboBox;
  const NewText: string; MaxItemCount: integer);
begin
  AComboBox.AddHistoryItem(NewText,MaxItemCount,true,false);
end;

procedure TPublishModuleDialog.LoadHistoryLists;
var
  hl: THistoryList;
begin
  // destination directories
  hl:=InputHistories.HistoryLists.GetList(hlPublishModuleDestDirs,true,rltFile);
  hl.AppendEntry(GetForcedPathDelims('$(TestDir)/publishedproject/'));
  hl.AppendEntry(GetForcedPathDelims('$(TestDir)/publishedpackage/'));
  hl.AppendEntry(GetForcedPathDelims('$(ProjPath)/published/'));
  DestDirComboBox.Items.Assign(hl);

  // file filter
  hl:=InputHistories.HistoryLists.GetList(hlPublishModuleFileFilter,true,rltFile);
  if hl.Count=0 then
    hl.Add(DefPublModIncFilter);
  FilterCombobox.Items.Assign(hl);
end;

procedure TPublishModuleDialog.SaveHistoryLists;
var
  hl: THistoryList;
begin
  // destination directories
  SetComboBox(DestDirComboBox,DestDirComboBox.Text,20);
  hl:=InputHistories.HistoryLists.GetList(hlPublishModuleDestDirs,true,rltFile);
  hl.Assign(DestDirComboBox.Items);

  // file filters
  SetComboBox(FilterCombobox,FilterCombobox.Text,20);
  hl:=InputHistories.HistoryLists.GetList(hlPublishModuleFileFilter,true,rltFile);
  hl.Assign(FilterCombobox.Items);
end;

procedure TPublishModuleDialog.SetOptions(const AValue: TPublishModuleOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
  if FOptions is TPublishPackageOptions then
    Caption:=lisPkgEditPublishPackage
  else
    Caption:=lisPublishProject;
  LoadFromOptions(FOptions);
end;

function TPublishModuleDialog.CheckFilter: boolean;
begin
  Result:=false;
  if Options<>nil then begin
    if not Options.FilterValid then begin
      if IDEMessageDialog(lisCCOErrorCaption, lisInvalidFilter,
                          mtError, [mbIgnore,mbCancel]) = mrCancel
      then exit;
    end;
  end;
  Result:=true;
end;

procedure TPublishModuleDialog.LoadFromOptions(SrcOpts: TPublishModuleOptions);
begin
  // destination
  SetComboBox(DestDirComboBox,SrcOpts.DestinationDirectory,20);

  // file filters
  CompressCheckbox.Checked:=SrcOpts.CompressFinally;
  UseFiltersCheckbox.Checked:=SrcOpts.UseFileFilters;
  FilterSimpleSyntaxCheckbox.Checked:=SrcOpts.FilterSimpleSyntax;
  SetComboBox(FilterCombobox,SrcOpts.FileFilter,20);
end;

procedure TPublishModuleDialog.SaveToOptions(DestOpts: TPublishModuleOptions);
begin
  // destination
  DestOpts.DestinationDirectory:=DestDirComboBox.Text;

  // file filters
  DestOpts.CompressFinally:=CompressCheckbox.Checked;
  DestOpts.UseFileFilters:=UseFiltersCheckbox.Checked;
  DestOpts.FilterSimpleSyntax:=FilterSimpleSyntaxCheckbox.Checked;
  DestOpts.FileFilter:=FilterCombobox.Text;
end;

end.

