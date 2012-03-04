{
 /***************************************************************************
                          addtoprojectdlg.pas
                          -------------------


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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
}
unit AddToProjectDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, LCLProc, Forms, Controls, Buttons,
  ComCtrls, StdCtrls, ExtCtrls, Menus, Dialogs, Graphics, FileUtil, ButtonPanel,
  AVL_Tree,
  IDEWindowIntf, PackageIntf,
  LazarusIDEStrConsts, IDEProcs, IDEOptionDefs,
  EnvironmentOpts, Project, PackageDefs, PackageSystem, InputHistory;
  
type
  TAddToProjectType = (
    a2pFiles,
    a2pRequiredPkg
    );

  TAddToProjectResult = class
  public
    AddType: TAddToProjectType;
    Dependency: TPkgDependency;
    FileNames: TStrings;
    destructor Destroy; override;
  end;

  { TAddToProjectDialog }

  TAddToProjectDialog = class(TForm)
    AddFileListView: TListView;
    ButtonPanel: TButtonPanel;
    FilesBrowseButton: TBitBtn;
    FilesDeleteButton: TBitBtn;
    FilesDirButton: TBitBtn;
    FilesShortenButton: TBitBtn;
    // notebook
    NoteBook: TPageControl;
    AddEditorFilePage: TTabSheet;
    NewDependPage: TTabSheet;
    AddFilesPage: TTabSheet;
    // add file page
    AddFileLabel: TLabel;
    // new required package
    DependPkgNameLabel: TLabel;
    DependPkgNameComboBox: TComboBox;
    DependMinVersionLabel: TLabel;
    DependMinVersionEdit: TEdit;
    DependMaxVersionLabel: TLabel;
    DependMaxVersionEdit: TEdit;
    // add files page
    FilesListView: TListView;
    procedure AddFileButtonClick(Sender: TObject);
    procedure AddToProjectDialogClose(Sender: TObject;
                                      var CloseAction: TCloseAction);
    procedure DependPkgNameComboBoxChange(Sender: TObject);
    procedure FilesDirButtonClick(Sender: TObject);
    procedure FilesListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure NewDependButtonClick(Sender: TObject);
    procedure FilesAddButtonClick(Sender: TObject);
    procedure FilesBrowseButtonClick(Sender: TObject);
    procedure FilesDeleteButtonClick(Sender: TObject);
    procedure FilesShortenButtonClick(Sender: TObject);
    procedure NotebookChange(Sender: TObject);
  private
    fPackages: TAVLTree;// tree of  TLazPackage or TPackageLink
    function CheckAddingFile(NewFiles: TStringList; var NewFilename: string
      ): TModalResult;
    procedure SetupComponents;
    procedure SetupAddEditorFilePage;
    procedure SetupAddRequirementPage;
    procedure SetupAddFilesPage;
    function CheckNewReqOk: Boolean;
    procedure OnIteratePackages(APackageID: TLazPackageID);
  public
    AddResult: TAddToProjectResult;
    TheProject: TProject;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateAvailableDependencyNames;
    procedure UpdateAvailableFiles;
    procedure UpdateFilesButtons;
  end;
  
function ShowAddToProjectDlg(AProject: TProject;
  var AddResult: TAddToProjectResult): TModalResult;
function CheckAddingDependency(LazProject: TProject;
  NewDependency: TPkgDependency): boolean;


implementation

{$R *.lfm}

function ShowAddToProjectDlg(AProject: TProject;
  var AddResult: TAddToProjectResult): TModalResult;
var
  AddToProjectDialog: TAddToProjectDialog;
begin
  AddToProjectDialog:=TAddToProjectDialog.Create(nil);
  AddToProjectDialog.TheProject:=AProject;
  AddToProjectDialog.UpdateAvailableFiles;
  AddToProjectDialog.UpdateAvailableDependencyNames;
  Result:=AddToProjectDialog.ShowModal;
  if Result=mrOk then begin
    AddResult:=AddToProjectDialog.AddResult;
    AddToProjectDialog.AddResult:=nil;
  end else begin
    AddResult:=nil;
  end;
  AddToProjectDialog.Free;
end;

function CheckAddingDependency(LazProject: TProject;
  NewDependency: TPkgDependency): boolean;
var
  NewPkgName: String;
begin
  Result:=false;
  
  NewPkgName:=NewDependency.PackageName;

  // check Max-Min version
  if (pdfMinVersion in NewDependency.Flags)
  and (pdfMaxVersion in NewDependency.Flags)
  and (NewDependency.MaxVersion.Compare(NewDependency.MinVersion)<0) then
  begin
    MessageDlg(lisProjAddInvalidMinMaxVersion,
      lisProjAddTheMaximumVersionIsLowerThanTheMinimimVersion,
      mtError,[mbCancel],0);
    exit;
  end;

  // check packagename
  if (NewPkgName='') or (not IsValidIdent(NewPkgName)) then begin
    MessageDlg(lisProjAddInvalidPackagename,
      Format(lisProjAddThePackageNameIsInvalidPlaseChooseAnExistingPackag, [
        '"', NewPkgName, '"', #13]),
      mtError,[mbCancel],0);
    exit;
  end;

  // check if package is already required
  if LazProject.FindDependencyByName(NewPkgName)<>nil then begin
    MessageDlg(lisProjAddDependencyAlreadyExists,
      Format(lisProjAddTheProjectHasAlreadyADependency, ['"', NewPkgName, '"']),
      mtError,[mbCancel],0);
    exit;
  end;

  // check if required package exists
  if not PackageGraph.DependencyExists(NewDependency,fpfSearchAllExisting)
  then begin
    MessageDlg(lisProjAddPackageNotFound,
      Format(lisProjAddTheDependencyWasNotFound, ['"', NewDependency.AsString,
        '"', #13]),
      mtError,[mbCancel],0);
    exit;
  end;

  Result:=true;
end;

{ TAddToProjectDialog }
procedure TAddToProjectDialog.AddToProjectDialogClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TAddToProjectDialog.DependPkgNameComboBoxChange(Sender: TObject);
begin
  CheckNewReqOk;
end;

procedure TAddToProjectDialog.FilesDirButtonClick(Sender: TObject);
var
  DirDlg: TSelectDirectoryDialog;
  sl: TStringList;
  NewListItem: TListItem;
  NewPgkFileType: TPkgFileType;
  AFileName: String;
  i: Integer;
begin
  sl:=Nil;
  DirDlg:=TSelectDirectoryDialog.Create(nil);
  try
    DirDlg.InitialDir:=TheProject.ProjectDirectory;
    DirDlg.Options:=DirDlg.Options+[ofPathMustExist];
    InputHistories.ApplyFileDialogSettings(DirDlg);
    if DirDlg.Execute then begin
      sl:=FindAllFiles(DirDlg.FileName, '*.pas;*.pp;*.p;*.inc', False);
      for i := 0 to sl.Count-1 do begin
        AFilename:=CleanAndExpandFilename(sl[i]);
        NewPgkFileType:=FileNameToPkgFileType(AFilename);
        if TheProject.ProjectDirectory<>'' then
          AFilename:=CreateRelativePath(AFilename,TheProject.ProjectDirectory);
        NewListItem:=FilesListView.Items.Add;
        NewListItem.Caption:=AFilename;
        NewListItem.SubItems.Add(GetPkgFileTypeLocalizedName(NewPgkFileType));
        NewListItem.Selected:=True;
      end;
      UpdateFilesButtons;
    end;
    InputHistories.StoreFileDialogSettings(DirDlg);
  finally
    sl.Free;
    DirDlg.Free;
  end;
end;

procedure TAddToProjectDialog.FilesListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateFilesButtons;
end;

procedure TAddToProjectDialog.NewDependButtonClick(Sender: TObject);
var
  NewDependency: TPkgDependency;
begin
  NewDependency:=TPkgDependency.Create;
  try
    // check minimum version
    if DependMinVersionEdit.Text<>'' then begin
      if not NewDependency.MinVersion.ReadString(DependMinVersionEdit.Text) then
      begin
        MessageDlg(lisProjAddInvalidVersion,
          Format(lisProjAddTheMinimumVersionIsInvalid, ['"',
            DependMinVersionEdit.Text, '"', #13, #13]),
          mtError,[mbCancel],0);
        exit;
      end;
      NewDependency.Flags:=NewDependency.Flags+[pdfMinVersion];
    end;
    // check maximum version
    if DependMaxVersionEdit.Text<>'' then begin
      if not NewDependency.MaxVersion.ReadString(DependMaxVersionEdit.Text) then
      begin
        MessageDlg(lisProjAddInvalidVersion,
          Format(lisProjAddTheMaximumVersionIsInvalid, ['"',
            DependMaxVersionEdit.Text, '"', #13, #13]),
          mtError,[mbCancel],0);
        exit;
      end;
      NewDependency.Flags:=NewDependency.Flags+[pdfMaxVersion];
    end;

    NewDependency.PackageName:=DependPkgNameComboBox.Text;
    if not CheckAddingDependency(TheProject,NewDependency) then exit;

    // ok
    AddResult:=TAddToProjectResult.Create;
    AddResult.Dependency:=NewDependency;
    NewDependency:=nil;
    AddResult.AddType:=a2pRequiredPkg;

    ModalResult:=mrOk;
  finally
    NewDependency.Free;
  end;
end;

procedure TAddToProjectDialog.AddFileButtonClick(Sender: TObject);
var
  i: Integer;
  NewFilename: string;
  NewFiles: TStringList;
begin
  try
    NewFiles:=TStringList.Create;
    for i:=0 to AddFileListView.Items.Count-1 do
      if AddFileListView.Items[i].Selected then begin
        NewFilename:=AddFileListView.Items[i].Caption;
        case CheckAddingFile(NewFiles, NewFilename) of
          mrOk: ;
          mrIgnore: continue;
        else
          exit;
        end;
        NewFiles.Add(NewFilename);
      end;
    // everything ok
    AddResult:=TAddToProjectResult.Create;
    AddResult.AddType:=a2pFiles;
    AddResult.FileNames:=NewFiles;
    NewFiles:=nil;
  finally
    NewFiles.Free;
  end;
  ModalResult:=mrOk;
end;

procedure TAddToProjectDialog.FilesAddButtonClick(Sender: TObject);
var
  i: Integer;
  NewFilename: string;
  NewFiles: TStringList;
begin
  try
    NewFiles:=TStringList.Create;
    for i:=0 to FilesListView.Items.Count-1 do
      if FilesListView.Items[i].Selected then begin
        NewFilename:=FilesListView.Items[i].Caption;
        case CheckAddingFile(NewFiles, NewFilename) of
          mrOk: ;
          mrIgnore: continue;
        else
          exit;
        end;
        NewFiles.Add(NewFilename);
      end;
    // everything ok
    AddResult:=TAddToProjectResult.Create;
    AddResult.AddType:=a2pFiles;
    AddResult.FileNames:=NewFiles;
    NewFiles:=nil;
  finally
    NewFiles.Free;
  end;
  ModalResult:=mrOk;
end;

procedure TAddToProjectDialog.FilesBrowseButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
  i: Integer;
  NewListItem: TListItem;
  NewPgkFileType: TPkgFileType;
  ADirectory: String;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    ADirectory:=TheProject.ProjectDirectory;
    if not FilenameIsAbsolute(ADirectory) then ADirectory:='';
    if ADirectory<>'' then
      OpenDialog.InitialDir:=ADirectory;
    OpenDialog.Title:=lisOpenFile;
    OpenDialog.Options:=OpenDialog.Options
                          +[ofFileMustExist,ofPathMustExist,ofAllowMultiSelect];
    OpenDialog.Filter:=dlgAllFiles+' ('+GetAllFilesMask+')|'+GetAllFilesMask
                 +'|'+lisLazarusUnit+' (*.pas;*.pp)|*.pas;*.pp'
                 +'|'+lisLazarusInclude+' (*.inc)|*.inc'
                 +'|'+lisLazarusForm+' (*.lfm;*.dfm)|*.lfm;*.dfm';
    if OpenDialog.Execute then begin
      for i:=0 to OpenDialog.Files.Count-1 do begin
        AFilename:=CleanAndExpandFilename(OpenDialog.Files[i]);
        if FileExistsUTF8(AFilename) then begin
          NewPgkFileType:=FileNameToPkgFileType(AFilename);
          if ADirectory<>'' then
            AFilename:=CreateRelativePath(AFilename,ADirectory);
          NewListItem:=FilesListView.Items.Add;
          NewListItem.Caption:=AFilename;
          NewListItem.SubItems.Add(GetPkgFileTypeLocalizedName(NewPgkFileType));
          NewListItem.Selected:=True;
        end;
      end;
      UpdateFilesButtons;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TAddToProjectDialog.FilesDeleteButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=FilesListView.Items.Count-1 downto 0 do
    if FilesListView.Items[i].Selected then
      FilesListView.Items.Delete(i);
  UpdateFilesButtons;
end;

procedure TAddToProjectDialog.FilesShortenButtonClick(Sender: TObject);
var
  SwitchToAbsolute: Boolean;
  i: Integer;
  Filename: String;
  ADirectory: String;
begin
  if FilesListView.Items.Count=0 then exit;
  if (TheProject=nil)
  or (not FilenameIsAbsolute(TheProject.ProjectDirectory)) then exit;
  ADirectory:=TheProject.ProjectDirectory;
  SwitchToAbsolute:=not FilenameIsAbsolute(FilesListView.Items[0].Caption);
  for i:=0 to FilesListView.Items.Count-1 do begin
    Filename:=FilesListView.Items[i].Caption;
    if SwitchToAbsolute then
      Filename:=CreateAbsoluteSearchPath(Filename,ADirectory)
    else
      Filename:=CreateRelativePath(Filename,ADirectory);
    FilesListView.Items[i].Caption:=Filename;
  end;
end;

procedure TAddToProjectDialog.NotebookChange(Sender: TObject);
begin
  case NoteBook.PageIndex of
    0: begin              // Add Editor Files
      ButtonPanel.OKButton.Caption:=lisA2PAddFiles;
      ButtonPanel.OkButton.OnClick:=@AddFileButtonClick;
      ButtonPanel.OkButton.Enabled:=AddFileListView.Items.Count>0;
    end;
    1: begin              // New Requirement
      ButtonPanel.OkButton.Caption:=lisA2PCreateNewReq;
      ButtonPanel.OkButton.OnClick:=@NewDependButtonClick;
      CheckNewReqOk;
    end;
    2: begin              // Add Files
      ButtonPanel.OkButton.Caption:=lisProjAddAddFilesToProject;
      ButtonPanel.OkButton.OnClick:=@FilesAddButtonClick;
      UpdateFilesButtons;
    end;
  end;
end;

procedure TAddToProjectDialog.SetupComponents;
begin
  ButtonPanel.OKButton.ModalResult := mrNone;
  ButtonPanel.OKButton.OnClick := @AddFileButtonClick;
  NoteBook.PageIndex:=0;
  NotebookChange(NoteBook);

  SetupAddEditorFilePage;
  SetupAddRequirementPage;
  SetupAddFilesPage;
end;

procedure TAddToProjectDialog.SetupAddEditorFilePage;
begin
  AddEditorFilePage.Caption := lisProjAddEditorFile;
  AddFileLabel.Caption:=lisProjFiles;
end;

procedure TAddToProjectDialog.SetupAddRequirementPage;
begin
  NewDependPage.Caption := lisProjAddNewRequirement;

  DependPkgNameLabel.Caption:=lisProjAddPackageName;
  DependPkgNameComboBox.Text:='';

  DependMinVersionLabel.Caption:=lisProjAddMinimumVersionOptional;
  DependMinVersionEdit.Text:='';

  DependMaxVersionLabel.Caption:=lisProjAddMaximumVersionOptional;
  DependMaxVersionEdit.Text:='';
end;

procedure TAddToProjectDialog.SetupAddFilesPage;
var
  CurColumn: TListColumn;
begin
  AddFilesPage.Caption := lisA2PAddFiles;

  with FilesListView do begin
    CurColumn:=Columns.Add;
    CurColumn.Width:=200;
    CurColumn.Caption:=lisA2PFilename2;
    CurColumn:=Columns.Add;
    CurColumn.Caption:=dlgEnvType;
  end;

  with FilesBrowseButton do begin
    Caption:=lisA2PAddFiles;
    LoadGlyphFromLazarusResource('laz_add');
  end;

  with FilesDirButton do begin
    Caption:=lisAddFilesInDirectory;
    LoadGlyphFromLazarusResource('pkg_files');
  end;

  with FilesShortenButton do begin
    Caption:=lisA2PSwitchPaths;
    ShowHint:=true;
    Hint:=lisToggleShowingFilenamesWithFullPathOrWithRelativePa;
  end;

  with FilesDeleteButton do begin
    Caption:=lisDelete;
    ShowHint:=true;
    Hint:=lisDeleteSelectedFiles;
    LoadGlyphFromLazarusResource('laz_delete');
  end;

  UpdateFilesButtons;
end;

function TAddToProjectDialog.CheckNewReqOk: Boolean;
begin
  Result:=(DependPkgNameComboBox.Text<>'');
  ButtonPanel.OkButton.Enabled:=Result;
end;

procedure TAddToProjectDialog.OnIteratePackages(APackageID: TLazPackageID);
begin
  if (fPackages.Find(APackageID)=nil) then
    fPackages.Add(APackageID);
end;

function TAddToProjectDialog.CheckAddingFile(NewFiles: TStringList;
  var NewFilename: string): TModalResult;
var
  ConflictFile: TUnitInfo;
  OtherUnitName: String;
  OtherFile: string;
  j: Integer;
  NewFile: TUnitInfo;
  NewUnitName: String;
begin
  Result:=mrCancel;
  // expand filename
  if not FilenameIsAbsolute(NewFilename) then
    NewFilename:=TrimFilename(TheProject.ProjectDirectory+PathDelim+NewFilename);
  // check if file is already part of project
  NewFile:=TheProject.UnitInfoWithFilename(NewFilename);
  if (NewFile<>nil) and NewFile.IsPartOfProject then begin
    Result:=mrIgnore;
    exit;
  end;
  // check unit name
  if FilenameIsPascalUnit(NewFilename) then begin
    // check unitname is valid pascal identifier
    NewUnitName:=ExtractFileNameOnly(NewFilename);
    if (NewUnitName='') or not (IsValidIdent(NewUnitName)) then begin
      MessageDlg(lisProjAddInvalidPascalUnitName,
        Format(lisProjAddTheUnitNameIsNotAValidPascalIdentifier, ['"',
          NewUnitName, '"']),
        mtWarning, [mbIgnore, mbCancel], 0);
      exit;
    end;
    // check if unitname already exists in project
    ConflictFile:=TheProject.UnitWithUnitname(NewUnitName);
    if ConflictFile<>nil then begin
      MessageDlg(lisProjAddUnitNameAlreadyExists,
        Format(lisProjAddTheUnitNameAlreadyExistsInTheProject, ['"',
          NewUnitName, '"', #13, '"', ConflictFile.Filename, '"']),
        mtWarning, [mbCancel, mbIgnore], 0);
      exit;
    end;
    // check if unitname already exists in selection
    for j:=0 to NewFiles.Count-1 do begin
      OtherFile:=NewFiles[j];
      if FilenameIsPascalUnit(OtherFile) then begin
        OtherUnitName:=ExtractFileNameOnly(OtherFile);
        if CompareText(OtherUnitName, NewUnitName)=0 then begin
          MessageDlg(lisProjAddUnitNameAlreadyExists,
            Format(lisProjAddTheUnitNameAlreadyExistsInTheSelection, ['"',
              NewUnitName, '"', #13, '"', OtherFile, '"']),
            mtWarning, [mbCancel], 0);
          exit;
        end;
      end;
    end;
  end;
  Result:=mrOk;
end;

constructor TAddToProjectDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption:=lisProjAddToProject;
  fPackages:=TAVLTree.Create(@CompareLazPackageIDNames);
  IDEDialogLayoutList.ApplyLayout(Self,500,300);
  SetupComponents;
end;

destructor TAddToProjectDialog.Destroy;
begin
  FreeAndNil(fPackages);
  inherited Destroy;
end;

procedure TAddToProjectDialog.UpdateAvailableDependencyNames;
var
  ANode: TAVLTreeNode;
  sl: TStringList;
begin
  fPackages.Clear;
  PackageGraph.IteratePackages(fpfSearchAllExisting,@OnIteratePackages);
  sl:=TStringList.Create;
  ANode:=fPackages.FindLowest;
  while ANode<>nil do begin
    sl.Add(TLazPackageID(ANode.Data).Name);
    ANode:=fPackages.FindSuccessor(ANode);
  end;
  DependPkgNameComboBox.Items.Assign(sl);
  sl.Free;
end;

procedure TAddToProjectDialog.UpdateAvailableFiles;
var
  CurFile: TUnitInfo;
  NewListItem: TListItem;
  NewFilename: String;
begin
  AddFileListView.Items.BeginUpdate;
  if TheProject<>nil then begin
    CurFile:=TheProject.FirstUnitWithEditorIndex;
    while CurFile<>nil do begin
      if (not CurFile.IsPartOfProject) and (not CurFile.IsVirtual) then begin
        NewFilename:=CreateRelativePath(CurFile.Filename,TheProject.ProjectDirectory);
        NewListItem:=AddFileListView.Items.Add;
        NewListItem.Caption:=NewFilename;
        NewListItem.Selected:=True;
      end;
      CurFile:=CurFile.NextUnitWithEditorIndex;
    end;
  end;
  AddFileListView.Items.EndUpdate;
  ButtonPanel.OkButton.Enabled:=AddFileListView.Items.Count>0;
end;

procedure TAddToProjectDialog.UpdateFilesButtons;
begin
  FilesShortenButton.Enabled:=FilesListView.Items.Count>0;
  FilesDeleteButton.Enabled:=FilesListView.SelCount>0;
  ButtonPanel.OKButton.Enabled:=FilesListView.Items.Count>0;
end;

{ TAddToProjectResult }

destructor TAddToProjectResult.Destroy;
begin
  FileNames.Free;
  inherited Destroy;
end;

end.

