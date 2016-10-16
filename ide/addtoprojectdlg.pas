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
  Classes, SysUtils, AVL_Tree,
  // LCL
  Forms, Controls, Buttons, ComCtrls, StdCtrls, Dialogs, ButtonPanel,
  // LazUtils
  FileUtil, LazFileUtils,
  // IDEIntf
  IDEWindowIntf, PackageIntf,
  // IDE
  LazarusIDEStrConsts, Project, InputHistory, PackageDefs, ProjPackChecks;
  
type
  TAddToProjectType = (
    a2pFiles,
    a2pEditorFiles
    );

  TAddToProjectResult = class
  public
    AddType: TAddToProjectType;
    FileNames: TStrings;
    destructor Destroy; override;
  end;

  { TAddToProjectDialog }

  TAddToProjectDialog = class(TForm)
    AddFileListView: TListView;
    ButtonPanel: TButtonPanel;
    FilesDeleteButton: TBitBtn;
    FilesDirButton: TBitBtn;
    FilesShortenButton: TBitBtn;
    // notebook
    NoteBook: TPageControl;
    AddEditorFilePage: TTabSheet;
    AddFilesPage: TTabSheet;
    // add file page
    AddFileLabel: TLabel;
    // add files page
    FilesListView: TListView;
    procedure AddFileButtonClick(Sender: TObject);
    procedure AddFileListViewSelectItem(Sender: TObject; {%H-}Item: TListItem;
      {%H-}Selected: Boolean);
    procedure AddToProjectDialogClose(Sender: TObject;
                                      var {%H-}CloseAction: TCloseAction);
    procedure AddToProjectDialogShow(Sender: TObject);
    procedure FilesDirButtonClick(Sender: TObject);
    procedure FilesListViewSelectItem(Sender: TObject; {%H-}Item: TListItem;
      {%H-}Selected: Boolean);
    procedure FilesAddButtonClick(Sender: TObject);
    procedure FilesDeleteButtonClick(Sender: TObject);
    procedure FilesShortenButtonClick(Sender: TObject);
    procedure NotebookChange(Sender: TObject);
  private
    fPackages: TAVLTree;// tree of  TLazPackage or TPackageLink
    fProject: TProject;
    procedure SetupComponents;
    procedure SetupAddEditorFilePage;
    procedure SetupAddFilesPage;
    procedure UpdateAvailableFiles;
    procedure UpdateFilesButtons;
  public
    AddResult: TAddToProjectResult;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
function ShowAddToProjectDlg(AProject: TProject;
  var AddResult: TAddToProjectResult;
  AInitTab: TAddToProjectType): TModalResult;


implementation

{$R *.lfm}

function ShowAddToProjectDlg(AProject: TProject;
  var AddResult: TAddToProjectResult; AInitTab: TAddToProjectType): TModalResult;
var
  AddToProjectDialog: TAddToProjectDialog;
begin
  AddToProjectDialog:=TAddToProjectDialog.Create(nil);
  AddToProjectDialog.fProject:=AProject;
  AddToProjectDialog.UpdateAvailableFiles;

  case AInitTab of
    a2pFiles: AddToProjectDialog.NoteBook.ActivePageIndex:=1;
    a2pEditorFiles: AddToProjectDialog.NoteBook.ActivePageIndex:=0;
  end;
  // hide tabs for simple look
  AddToProjectDialog.NoteBook.ShowTabs:=false;
  AddToProjectDialog.NoteBook.TabStop:=false;
  // press "Add files" btn
  if AInitTab=a2pFiles then
    AddToProjectDialog.FilesDirButton.Click;

  Result:=AddToProjectDialog.ShowModal;
  if Result=mrOk then begin
    AddResult:=AddToProjectDialog.AddResult;
    AddToProjectDialog.AddResult:=nil;
  end else begin
    AddResult:=nil;
  end;
  AddToProjectDialog.Free;
end;

{ TAddToProjectDialog }

procedure TAddToProjectDialog.AddToProjectDialogClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TAddToProjectDialog.AddToProjectDialogShow(Sender: TObject);
begin
  SelectNext(NoteBook.ActivePage, True, True);
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
    DirDlg.InitialDir:=fProject.ProjectDirectory;
    DirDlg.Options:=DirDlg.Options+[ofPathMustExist];
    InputHistories.ApplyFileDialogSettings(DirDlg);
    if DirDlg.Execute then begin
      sl:=FindAllFiles(DirDlg.FileName, '*.pas;*.pp;*.p;*.inc', False);
      for i := 0 to sl.Count-1 do begin
        AFilename:=CleanAndExpandFilename(sl[i]);
        NewPgkFileType:=FileNameToPkgFileType(AFilename);
        if fProject.ProjectDirectory<>'' then
          AFilename:=CreateRelativePath(AFilename,fProject.ProjectDirectory);
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
        case CheckAddingProjectFile(fProject, NewFiles, NewFilename) of
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

procedure TAddToProjectDialog.AddFileListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  ButtonPanel.OkButton.Enabled:=AddFileListView.SelCount>0;
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
        case CheckAddingProjectFile(fProject, NewFiles, NewFilename) of
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
  if (fProject=nil) or (not FilenameIsAbsolute(fProject.ProjectDirectory)) then exit;
  ADirectory:=fProject.ProjectDirectory;
  SwitchToAbsolute:=not FilenameIsAbsolute(FilesListView.Items[0].Caption);
  for i:=0 to FilesListView.Items.Count-1 do begin
    Filename:=FilesListView.Items[i].Caption;
    if SwitchToAbsolute then
      Filename:=CreateAbsolutePath(Filename,ADirectory)
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
      ButtonPanel.OkButton.Enabled:=AddFileListView.SelCount>0;
    end;
    1: begin              // Add Files
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
  SetupAddFilesPage;
end;

procedure TAddToProjectDialog.SetupAddEditorFilePage;
var
  CurColumn: TListColumn;
begin
  AddEditorFilePage.Caption := lisProjAddEditorFile;
  AddFileLabel.Caption:=lisProjFiles;
  CurColumn:=AddFileListView.Columns.Add;
  CurColumn.Caption:=lisA2PFilename2;
end;

procedure TAddToProjectDialog.SetupAddFilesPage;
var
  CurColumn: TListColumn;
begin
  AddFilesPage.Caption := lisA2PAddFiles;

  CurColumn:=FilesListView.Columns.Add;
  CurColumn.Width:=200;
  CurColumn.Caption:=lisA2PFilename2;
  CurColumn:=FilesListView.Columns.Add;
  CurColumn.Caption:=dlgEnvType;

  with FilesDirButton do begin
    Caption:=lisAddFilesInDirectory;
    LoadGlyphFromResourceName(HInstance, 'pkg_files');
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
    LoadGlyphFromResourceName(HInstance, 'laz_delete');
  end;

  UpdateFilesButtons;
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

procedure TAddToProjectDialog.UpdateAvailableFiles;
var
  CurFile: TUnitInfo;
  NewListItem: TListItem;
  NewFilename: String;
begin
  AddFileListView.Items.BeginUpdate;
  if fProject<>nil then begin
    CurFile:=fProject.FirstUnitWithEditorIndex;
    while CurFile<>nil do begin
      if (not CurFile.IsPartOfProject) and (not CurFile.IsVirtual) then begin
        NewFilename:=CreateRelativePath(CurFile.Filename,fProject.ProjectDirectory);
        NewListItem:=AddFileListView.Items.Add;
        NewListItem.Caption:=NewFilename;
        NewListItem.Selected:=True;
      end;
      CurFile:=CurFile.NextUnitWithEditorIndex;
    end;
  end;
  AddFileListView.Items.EndUpdate;
  ButtonPanel.OkButton.Enabled:=AddFileListView.SelCount>0;
end;

procedure TAddToProjectDialog.UpdateFilesButtons;
begin
  FilesShortenButton.Enabled:=FilesListView.Items.Count>0;
  FilesDeleteButton.Enabled:=FilesListView.SelCount>0;
  ButtonPanel.OKButton.Enabled:=FilesListView.SelCount>0;
end;

{ TAddToProjectResult }

destructor TAddToProjectResult.Destroy;
begin
  FileNames.Free;
  inherited Destroy;
end;

end.

