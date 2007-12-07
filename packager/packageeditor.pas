{  $Id$  }
{
 /***************************************************************************
                            packageeditor.pas
                            -----------------


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

  Abstract:
    TPackageEditorForm is the form of a package editor.
}
unit PackageEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, Buttons, LResources,
  Graphics, LCLType, LCLProc, Menus, Dialogs, FileUtil,
  HelpIntfs, AVL_Tree, Laz_XMLCfg, LazIDEIntf, ProjectIntf, FormEditingIntf,
  IDEProcs, LazConf, LazarusIDEStrConsts, IDEOptionDefs, IDEDefs,
  IDEContextHelpEdit, CompilerOptions, CompilerOptionsDlg, ComponentReg,
  PackageDefs, PkgOptionsDlg, AddToPackageDlg, PkgVirtualUnitEditor,
  PackageSystem;
  
type
  TOnCreatePkgMakefile =
    function(Sender: TObject; APackage: TLazPackage): TModalResult of object;
  TOnOpenFile =
    function(Sender: TObject; const Filename: string): TModalResult of object;
  TOnOpenPkgFile =
    function(Sender: TObject; PkgFile: TPkgFile): TModalResult of object;
  TOnOpenPackage =
    function(Sender: TObject; APackage: TLazPackage): TModalResult of object;
  TOnSavePackage =
    function(Sender: TObject; APackage: TLazPackage;
             SaveAs: boolean): TModalResult of object;
  TOnRevertPackage =
    function(Sender: TObject; APackage: TLazPackage): TModalResult of object;
  TOnPublishPackage =
    function(Sender: TObject; APackage: TLazPackage): TModalResult of object;
  TOnCompilePackage =
    function(Sender: TObject; APackage: TLazPackage;
             CompileClean, CompileRequired: boolean): TModalResult of object;
  TOnAddPkgToProject =
    function(Sender: TObject; APackage: TLazPackage;
             OnlyTestIfPossible: boolean): TModalResult of object;
  TOnInstallPackage =
    function(Sender: TObject; APackage: TLazPackage): TModalResult of object;
  TOnUninstallPackage =
    function(Sender: TObject; APackage: TLazPackage): TModalResult of object;
  TOnViewPackageSource =
    function(Sender: TObject; APackage: TLazPackage): TModalResult of object;
  TOnCreateNewPkgFile =
    function(Sender: TObject; Params: TAddToPkgResult): TModalResult  of object;
  TOnDeleteAmbiguousFiles =
    function(Sender: TObject; APackage: TLazPackage;
             const Filename: string): TModalResult of object;
  TOnFreePkgEditor = procedure(APackage: TLazPackage) of object;


  { TPackageEditorLayout }

  TPackageEditorLayout = class
  public
    Filename: string;
    Rectangle: TRect;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;
  

  { TPackageEditorForm }

  TPackageEditorForm = class(TBasePackageEditor)
    // buttons
    SaveBitBtn: TBitBtn;
    CompileBitBtn: TBitBtn;
    AddBitBtn: TBitBtn;
    RemoveBitBtn: TBitBtn;
    InstallBitBtn: TBitBtn;
    OptionsBitBtn: TBitBtn;
    CompilerOptionsBitBtn: TBitBtn;
    MoreBitBtn: TBitBtn;
    HelpBitBtn: TBitBtn;
    // items
    FilesTreeView: TTreeView;
    // properties
    FilePropsGroupBox: TGroupBox;
    // file properties
    CallRegisterProcCheckBox: TCheckBox;
    AddToUsesPkgSectionCheckBox: TCheckBox;
    RegisteredPluginsGroupBox: TGroupBox;
    RegisteredListBox: TListBox;
    // dependency properties
    UseMinVersionCheckBox: TCheckBox;
    MinVersionEdit: TEdit;
    UseMaxVersionCheckBox: TCheckBox;
    MaxVersionEdit: TEdit;
    ApplyDependencyButton: TButton;
    // statusbar
    StatusBar: TStatusBar;
    // hidden components
    FilesPopupMenu: TPopupMenu;
    procedure AddBitBtnClick(Sender: TObject);
    procedure AddToUsesPkgSectionCheckBoxChange(Sender: TObject);
    procedure AddToProjectClick(Sender: TObject);
    procedure ApplyDependencyButtonClick(Sender: TObject);
    procedure CallRegisterProcCheckBoxChange(Sender: TObject);
    procedure ChangeFileTypeMenuItemClick(Sender: TObject);
    procedure CompileAllCleanClick(Sender: TObject);
    procedure CompileBitBtnClick(Sender: TObject);
    procedure CompileCleanClick(Sender: TObject);
    procedure CompilerOptionsBitBtnClick(Sender: TObject);
    procedure CreateMakefileClick(Sender: TObject);
    procedure FilePropsGroupBoxResize(Sender: TObject);
    procedure FilesPopupMenuPopup(Sender: TObject);
    procedure FilesTreeViewDblClick(Sender: TObject);
    procedure FilesTreeViewSelectionChanged(Sender: TObject);
    procedure FixFilesCaseMenuItemClick(Sender: TObject);
    procedure HelpBitBtnClick(Sender: TObject);
    procedure InstallBitBtnClick(Sender: TObject);
    procedure MaxVersionEditChange(Sender: TObject);
    procedure MinVersionEditChange(Sender: TObject);
    procedure MoreBitBtnClick(Sender: TObject);
    procedure MoveDependencyDownClick(Sender: TObject);
    procedure MoveDependencyUpClick(Sender: TObject);
    procedure MoveFileDownMenuItemClick(Sender: TObject);
    procedure MoveFileUpMenuItemClick(Sender: TObject);
    procedure SetDependencyFilenameMenuItemClick(Sender: TObject);
    procedure ClearDependencyFilenameMenuItemClick(Sender: TObject);
    procedure OpenFileMenuItemClick(Sender: TObject);
    procedure OptionsBitBtnClick(Sender: TObject);
    procedure PackageEditorFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure PackageEditorFormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure PackageEditorFormResize(Sender: TObject);
    procedure PublishClick(Sender: TObject);
    procedure ReAddMenuItemClick(Sender: TObject);
    procedure RegisteredListBoxDrawItem(Control: TWinControl; Index: Integer;
                                        ARect: TRect; State: TOwnerDrawState);
    procedure RemoveBitBtnClick(Sender: TObject);
    procedure EditVirtualUnitMenuItemClick(Sender: TObject);
    procedure RevertClick(Sender: TObject);
    procedure SaveAsClick(Sender: TObject);
    procedure SaveBitBtnClick(Sender: TObject);
    procedure SortFilesMenuItemClick(Sender: TObject);
    procedure UninstallClick(Sender: TObject);
    procedure UseMaxVersionCheckBoxChange(Sender: TObject);
    procedure UseMinVersionCheckBoxChange(Sender: TObject);
    procedure ViewPkgSourceClick(Sender: TObject);
  private
    FLazPackage: TLazPackage;
    FilesNode: TTreeNode;
    RequiredPackagesNode: TTreeNode;
    RemovedFilesNode: TTreeNode;
    RemovedRequiredNode: TTreeNode;
    FPlugins: TStringList;
    procedure SetLazPackage(const AValue: TLazPackage); override;
    procedure SetupComponents;
    procedure UpdateAll; override;
    procedure UpdateTitle;
    procedure UpdateButtons;
    procedure UpdateFiles;
    procedure UpdateRequiredPkgs;
    procedure UpdateSelectedFile;
    procedure UpdateApplyDependencyButton;
    procedure UpdateStatusBar;
    function GetCurrentDependency(var Removed: boolean): TPkgDependency;
    function GetCurrentFile(var Removed: boolean): TPkgFile;
    function StoreCurrentTreeSelection: TStringList;
    procedure ApplyTreeSelection(ASelection: TStringList; FreeList: boolean);
    procedure ExtendUnitIncPathForNewUnit(const AnUnitFilename,
      AnIncludeFile: string);
    function CanBeAddedToProject: boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoCompile(CompileClean, CompileRequired: boolean);
    procedure DoFixFilesCase;
    procedure DoMoveCurrentFile(Offset: integer);
    procedure DoPublishProject;
    procedure DoEditVirtualUnit;
    procedure DoRevert;
    procedure DoSave(SaveAs: boolean);
    procedure DoSortFiles;
    procedure DoOpenPkgFile(PkgFile: TPkgFile);
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
  end;
  
  
  { TPackageEditors }
  
  TPackageEditors = class
  private
    FItems: TList; // list of TPackageEditorForm
    fLayouts: TAVLTree;// tree of TPackageEditorLayout sorted for filename
    FOnAddToProject: TOnAddPkgToProject;
    FOnCompilePackage: TOnCompilePackage;
    FOnCreateNewFile: TOnCreateNewPkgFile;
    FOnCreatePkgMakefile: TOnCreatePkgMakefile;
    FOnDeleteAmbiguousFiles: TOnDeleteAmbiguousFiles;
    FOnFreeEditor: TOnFreePkgEditor;
    FOnGetIDEFileInfo: TGetIDEFileStateEvent;
    FOnGetUnitRegisterInfo: TOnGetUnitRegisterInfo;
    FOnImExportCompilerOptions: TNotifyEvent;
    FOnInstallPackage: TOnInstallPackage;
    FOnOpenFile: TOnOpenFile;
    FOnOpenPackage: TOnOpenPackage;
    FOnOpenPkgFile: TOnOpenPkgFile;
    FOnPublishPackage: TOnPublishPackage;
    FOnRevertPackage: TOnRevertPackage;
    FOnSavePackage: TOnSavePackage;
    FOnUninstallPackage: TOnUninstallPackage;
    FOnViewPackageSource: TOnViewPackageSource;
    function GetEditors(Index: integer): TPackageEditorForm;
    procedure ApplyLayout(AnEditor: TPackageEditorForm);
    procedure SaveLayout(AnEditor: TPackageEditorForm);
    procedure LoadLayouts;
    function GetLayoutConfigFilename: string;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    procedure Clear;
    procedure SaveLayouts;
    procedure Remove(Editor: TPackageEditorForm);
    function IndexOfPackage(Pkg: TLazPackage): integer;
    function FindEditor(Pkg: TLazPackage): TPackageEditorForm;
    function OpenEditor(Pkg: TLazPackage): TPackageEditorForm;
    function OpenFile(Sender: TObject; const Filename: string): TModalResult;
    function OpenPkgFile(Sender: TObject; PkgFile: TPkgFile): TModalResult;
    function OpenDependency(Sender: TObject;
                            Dependency: TPkgDependency): TModalResult;
    procedure DoFreeEditor(Pkg: TLazPackage);
    function CreateNewFile(Sender: TObject; Params: TAddToPkgResult): TModalResult;
    function SavePackage(APackage: TLazPackage; SaveAs: boolean): TModalResult;
    function RevertPackage(APackage: TLazPackage): TModalResult;
    function PublishPackage(APackage: TLazPackage): TModalResult;
    function CompilePackage(APackage: TLazPackage;
                            CompileClean,CompileRequired: boolean): TModalResult;
    procedure UpdateAllEditors;
    function InstallPackage(APackage: TLazPackage): TModalResult;
    function UninstallPackage(APackage: TLazPackage): TModalResult;
    function ViewPkgSourcePackage(APackage: TLazPackage): TModalResult;
    function DeleteAmbiguousFiles(APackage: TLazPackage;
                                  const Filename: string): TModalResult;
    function AddToProject(APackage: TLazPackage;
                          OnlyTestIfPossible: boolean): TModalResult;
    function CreateMakefile(APackage: TLazPackage): TModalResult;
  public
    property Editors[Index: integer]: TPackageEditorForm read GetEditors;
    property OnCreateNewFile: TOnCreateNewPkgFile read FOnCreateNewFile
                                                  write FOnCreateNewFile;
    property OnOpenFile: TOnOpenFile read FOnOpenFile write FOnOpenFile;
    property OnOpenPkgFile: TOnOpenPkgFile read FOnOpenPkgFile
                                           write FOnOpenPkgFile;
    property OnOpenPackage: TOnOpenPackage read FOnOpenPackage
                                           write FOnOpenPackage;
    property OnGetIDEFileInfo: TGetIDEFileStateEvent read FOnGetIDEFileInfo
                                                     write FOnGetIDEFileInfo;
    property OnGetUnitRegisterInfo: TOnGetUnitRegisterInfo
                       read FOnGetUnitRegisterInfo write FOnGetUnitRegisterInfo;
    property OnFreeEditor: TOnFreePkgEditor read FOnFreeEditor
                                            write FOnFreeEditor;
    property OnSavePackage: TOnSavePackage read FOnSavePackage
                                           write FOnSavePackage;
    property OnRevertPackage: TOnRevertPackage read FOnRevertPackage
                                               write FOnRevertPackage;
    property OnPublishPackage: TOnPublishPackage read FOnPublishPackage
                                               write FOnPublishPackage;
    property OnCompilePackage: TOnCompilePackage read FOnCompilePackage
                                                 write FOnCompilePackage;
    property OnInstallPackage: TOnInstallPackage read FOnInstallPackage
                                                 write FOnInstallPackage;
    property OnUninstallPackage: TOnUninstallPackage read FOnUninstallPackage
                                                 write FOnUninstallPackage;
    property OnViewPackageSource: TOnViewPackageSource read FOnViewPackageSource
                                                 write FOnViewPackageSource;
    property OnDeleteAmbiguousFiles: TOnDeleteAmbiguousFiles
                     read FOnDeleteAmbiguousFiles write FOnDeleteAmbiguousFiles;
    property OnImExportCompilerOptions: TNotifyEvent
               read FOnImExportCompilerOptions write FOnImExportCompilerOptions;
    property OnAddToProject: TOnAddPkgToProject read FOnAddToProject
                                                write FOnAddToProject;
    property OnCreateMakefile: TOnCreatePkgMakefile read FOnCreatePkgMakefile
                                                     write FOnCreatePkgMakefile;
  end;
  
var
  PackageEditors: TPackageEditors;


implementation

uses Math, IDEImagesIntf;

var
  ImageIndexFiles: integer;
  ImageIndexRemovedFiles: integer;
  ImageIndexRequired: integer;
  ImageIndexRemovedRequired: integer;
  ImageIndexUnit: integer;
  ImageIndexRegisterUnit: integer;
  ImageIndexLFM: integer;
  ImageIndexLRS: integer;
  ImageIndexInclude: integer;
  ImageIndexText: integer;
  ImageIndexBinary: integer;
  ImageIndexConflict: integer;

function CompareLayouts(Data1, Data2: Pointer): integer;
var
  Layout1: TPackageEditorLayout;
  Layout2: TPackageEditorLayout;
begin
  Layout1:=TPackageEditorLayout(Data1);
  Layout2:=TPackageEditorLayout(Data2);
  Result:=CompareFilenames(Layout1.Filename,Layout2.Filename);
end;

function CompareFilenameWithLayout(Key, Data: Pointer): integer;
var
  Filename: String;
  Layout: TPackageEditorLayout;
begin
  Filename:=String(Key);
  Layout:=TPackageEditorLayout(Data);
  Result:=CompareFilenames(Filename,Layout.Filename);
end;

{ TPackageEditorForm }

procedure TPackageEditorForm.PackageEditorFormResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
  y1: Integer;
  y2: Integer;
begin
  x:=0;
  y:=0;
  w:=ClientWidth div 5;
  h:=SaveBitBtn.Height;
  y1:=y;
  y2:=y1+h;
  // first column of buttons
  SaveBitBtn.SetBounds(x,y1,w,h);
  CompileBitBtn.SetBounds(x,y2,w,h);
  inc(x,w);

  // second column of buttons
  AddBitBtn.SetBounds(x,y1,w,h);
  RemoveBitBtn.SetBounds(x,y2,w,h);
  inc(x,w);

  // third and forth column of buttons
  OptionsBitBtn.SetBounds(x,y1,w,h);
  CompilerOptionsBitBtn.SetBounds(x,y2,2*w,h);
  inc(x,w);
  InstallBitBtn.SetBounds(x,y1,w,h);
  inc(x,w);

  // fifth column of buttons
  HelpBitBtn.SetBounds(x,y1,ClientWidth-x,h);
  MoreBitBtn.SetBounds(x,y2,ClientWidth-x,h);

  FilePropsGroupBox.Height:=120;
end;

procedure TPackageEditorForm.PublishClick(Sender: TObject);
begin
  DoPublishProject;
end;

procedure TPackageEditorForm.ReAddMenuItemClick(Sender: TObject);
var
  PkgFile: TPkgFile;
  AFilename: String;
  Dependency: TPkgDependency;
  Removed: boolean;
begin
  PkgFile:=GetCurrentFile(Removed);
  if (PkgFile<>nil) then begin
    if Removed then begin
      // re-add file
      AFilename:=PkgFile.Filename;
      if PkgFile.FileType=pftUnit then begin
        if not CheckAddingUnitFilename(LazPackage,d2ptUnit,
          PackageEditors.OnGetIDEFileInfo,AFilename) then exit;
      end else if PkgFile.FileType=pftVirtualUnit then begin
        if not CheckAddingUnitFilename(LazPackage,d2ptVirtualUnit,
          PackageEditors.OnGetIDEFileInfo,AFilename) then exit;
      end else begin
        if not CheckAddingUnitFilename(LazPackage,d2ptFile,
          PackageEditors.OnGetIDEFileInfo,AFilename) then exit;
      end;
      PkgFile.Filename:=AFilename;
      LazPackage.UnremovePkgFile(PkgFile);
      UpdateAll;
    end;
  end else begin
    Dependency:=GetCurrentDependency(Removed);
    if (Dependency<>nil) and (Removed) then begin
      // re-add dependency
      if not CheckAddingDependency(LazPackage,Dependency) then exit;
      LazPackage.RemoveRemovedDependency(Dependency);
      PackageGraph.AddDependencyToPackage(LazPackage,Dependency);
    end;
  end;
end;

procedure TPackageEditorForm.FilesPopupMenuPopup(Sender: TObject);
var
  ItemCnt: Integer;
  CurDependency: TPkgDependency;
  Removed: boolean;
  CurFile: TPkgFile;
  Writable: Boolean;
  FileIndex: Integer;

  function AddPopupMenuItem(const ACaption: string; AnEvent: TNotifyEvent;
    EnabledFlag: boolean): TMenuItem;
  begin
    if FilesPopupMenu.Items.Count<=ItemCnt then begin
      Result:=TMenuItem.Create(Self);
      FilesPopupMenu.Items.Add(Result);
    end else begin
      Result:=FilesPopupMenu.Items[ItemCnt];
      while Result.Count>0 do Result.Delete(Result.Count-1);
    end;
    Result.Caption:=ACaption;
    Result.OnClick:=AnEvent;
    Result.Enabled:=EnabledFlag;
    inc(ItemCnt);
  end;
  
  procedure AddFileTypeMenuItem;
  var
    FileTypeMenuItem: TMenuItem;
    CurPFT: TPkgFileType;
    NewMenuItem: TMenuItem;
    VirtualFileExists: Boolean;
  begin
    FileTypeMenuItem:=AddPopupMenuItem(lisAF2PFileType, nil, true);
    VirtualFileExists:=(CurFile.FileType=pftVirtualUnit)
                       and FileExists(CurFile.Filename);
    for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
      NewMenuItem:=TMenuItem.Create(Self);
      NewMenuItem.Caption:=GetPkgFileTypeLocalizedName(CurPFT);
      NewMenuItem.OnClick:=@ChangeFileTypeMenuItemClick;
      if CurPFT=CurFile.FileType then begin
        // menuitem to keep the current type
        NewMenuItem.Enabled:=true;
        NewMenuItem.Checked:=true;
      end else if VirtualFileExists then
        // a virtual unit that exists can be changed into anything
        NewMenuItem.Enabled:=true
      else if (not (CurPFT in PkgFileUnitTypes)) then
        // all other files can be changed into all non unit types
        NewMenuItem.Enabled:=true
      else if FilenameIsPascalUnit(CurFile.Filename) then
        // a pascal file can be changed into anything
        NewMenuItem.Enabled:=true
      else
        // default is to not allow
        NewMenuItem.Enabled:=false;
      FileTypeMenuItem.Add(NewMenuItem);
    end;
  end;
  
begin
  ItemCnt:=0;
  CurDependency:=GetCurrentDependency(Removed);
  Writable:=(not LazPackage.ReadOnly);
  if CurDependency=nil then
    CurFile:=GetCurrentFile(Removed)
  else
    CurFile:=nil;

  if CurFile<>nil then begin
    FileIndex:=LazPackage.IndexOfPkgFile(CurFile);
    if not Removed then begin
      AddPopupMenuItem(lisOpenFile, @OpenFileMenuItemClick, true);
      AddPopupMenuItem(lisPckEditRemoveFile, @RemoveBitBtnClick,
                       RemoveBitBtn.Enabled);
      AddPopupMenuItem('Move file up', @MoveFileUpMenuItemClick,
                       (FileIndex>0) and Writable);
      AddPopupMenuItem('Move file down', @MoveFileDownMenuItemClick,
                       (FileIndex<LazPackage.FileCount-1) and Writable);
      AddFileTypeMenuItem;
      if CurFile.FileType=pftVirtualUnit then
        AddPopupMenuItem('Edit Virtual Unit',@EditVirtualUnitMenuItemClick,
                         Writable);
    end else begin
      AddPopupMenuItem(lisOpenFile, @OpenFileMenuItemClick, true);
      AddPopupMenuItem(lisPckEditReAddFile, @ReAddMenuItemClick,
                       AddBitBtn.Enabled);
    end;
  end;
  if LazPackage.FileCount>1 then begin
    AddPopupMenuItem('Sort files', @SortFilesMenuItemClick, Writable);
    AddPopupMenuItem('Fix Files Case', @FixFilesCaseMenuItemClick, Writable);
  end;

  if CurDependency<>nil then begin
    if (not Removed) then begin
      AddPopupMenuItem(lisMenuOpenPackage, @OpenFileMenuItemClick, true);
      AddPopupMenuItem(lisPckEditRemoveDependency, @RemoveBitBtnClick,
                       RemoveBitBtn.Enabled);
      AddPopupMenuItem(lisPckEditMoveDependencyUp, @MoveDependencyUpClick,
                       (CurDependency.PrevRequiresDependency<>nil)
                       and Writable);
      AddPopupMenuItem(lisPckEditMoveDependencyDown, @MoveDependencyDownClick,
                       (CurDependency.NextRequiresDependency<>nil)
                       and Writable);
      AddPopupMenuItem(lisPckEditSetDependencyDefaultFilename,
                       @SetDependencyFilenameMenuItemClick,
                       Writable and (CurDependency.RequiredPackage<>nil));
      AddPopupMenuItem(lisPckEditClearDependencyDefaultFilename,
                       @ClearDependencyFilenameMenuItemClick,
                       Writable and (CurDependency.DefaultFilename<>''));
    end else begin
      AddPopupMenuItem(lisMenuOpenPackage, @OpenFileMenuItemClick, true);
      AddPopupMenuItem(lisPckEditReAddDependency, @ReAddMenuItemClick,
                       AddBitBtn.Enabled);
    end;
  end;
  
  if ItemCnt>0 then
    AddPopupMenuItem('-',nil,true);

  AddPopupMenuItem(lisPckEditAddToProject, @AddToProjectClick,
                   CanBeAddedToProject);
  AddPopupMenuItem(lisPckEditInstall, @InstallBitBtnClick, InstallBitBtn.Enabled
    );
  AddPopupMenuItem(lisPckEditUninstall, @UninstallClick,
          (LazPackage.Installed<>pitNope) or (LazPackage.AutoInstall<>pitNope));
  AddPopupMenuItem('-',nil,true);
  AddPopupMenuItem(lisMenuSave, @SaveBitBtnClick, SaveBitBtn.Enabled);
  AddPopupMenuItem(lisMenuSaveAs, @SaveAsClick, not LazPackage.AutoCreated);
  AddPopupMenuItem(lisMenuRevert, @RevertClick, (not LazPackage.AutoCreated) and
                                                FileExists(LazPackage.Filename));
  AddPopupMenuItem(lisPkgEditPublishPackage, @PublishClick,
                   (not LazPackage.AutoCreated) and (LazPackage.HasDirectory));
  AddPopupMenuItem('-',nil,true);
  AddPopupMenuItem(lisPckEditCompile, @CompileBitBtnClick, CompileBitBtn.Enabled
    );
  AddPopupMenuItem(lisPckEditRecompileClean, @CompileCleanClick,
                   CompileBitBtn.Enabled);
  AddPopupMenuItem(lisPckEditRecompileAllRequired, @CompileAllCleanClick,
                   CompileBitBtn.Enabled);
  AddPopupMenuItem(lisPckEditCreateMakefile, @CreateMakefileClick,
                   CompileBitBtn.Enabled);
  AddPopupMenuItem('-',nil,true);
  AddPopupMenuItem(lisCodeTemplAdd, @AddBitBtnClick, AddBitBtn.Enabled);
  AddPopupMenuItem(lisExtToolRemove, @RemoveBitBtnClick, RemoveBitBtn.Enabled);
  AddPopupMenuItem('-',nil,true);
  AddPopupMenuItem(lisPckEditGeneralOptions, @OptionsBitBtnClick,
    OptionsBitBtn.Enabled);
  AddPopupMenuItem(dlgCompilerOptions, @CompilerOptionsBitBtnClick,
    CompilerOptionsBitBtn.Enabled);
  AddPopupMenuItem(lisPckEditViewPackgeSource, @ViewPkgSourceClick,true);

  // remove unneeded menu items
  while FilesPopupMenu.Items.Count>ItemCnt do
    FilesPopupMenu.Items.Delete(FilesPopupMenu.Items.Count-1);
end;

procedure TPackageEditorForm.FilesTreeViewDblClick(Sender: TObject);
begin
  OpenFileMenuItemClick(Self);
end;

procedure TPackageEditorForm.FilesTreeViewSelectionChanged(Sender: TObject);
begin
  UpdateSelectedFile;
  UpdateButtons;
end;

procedure TPackageEditorForm.HelpBitBtnClick(Sender: TObject);
begin
  ShowContextHelpForIDE(HelpBitBtn);
end;

procedure TPackageEditorForm.InstallBitBtnClick(Sender: TObject);
begin
  PackageEditors.InstallPackage(LazPackage);
end;

procedure TPackageEditorForm.MaxVersionEditChange(Sender: TObject);
begin
  UpdateApplyDependencyButton;
end;

procedure TPackageEditorForm.MinVersionEditChange(Sender: TObject);
begin
  UpdateApplyDependencyButton;
end;

procedure TPackageEditorForm.MoreBitBtnClick(Sender: TObject);
var
  p: TPoint;
begin
  p:=MoreBitBtn.ClientToScreen(Point(0,MoreBitBtn.Height));
  FilesPopupMenu.Popup(p.x,p.y);
end;

procedure TPackageEditorForm.MoveDependencyUpClick(Sender: TObject);
var
  CurDependency: TPkgDependency;
  Removed: boolean;
  OldSelection: TStringList;
begin
  CurDependency:=GetCurrentDependency(Removed);
  if (CurDependency=nil) or Removed then exit;
  FilesTreeView.BeginUpdate;
  OldSelection:=StoreCurrentTreeSelection;
  PackageGraph.MoveRequiredDependencyUp(CurDependency);
  ApplyTreeSelection(OldSelection,true);
  FilesTreeView.EndUpdate;
end;

procedure TPackageEditorForm.MoveDependencyDownClick(Sender: TObject);
var
  CurDependency: TPkgDependency;
  Removed: boolean;
  OldSelection: TStringList;
begin
  CurDependency:=GetCurrentDependency(Removed);
  if (CurDependency=nil) or Removed then exit;
  FilesTreeView.BeginUpdate;
  OldSelection:=StoreCurrentTreeSelection;
  PackageGraph.MoveRequiredDependencyDown(CurDependency);
  ApplyTreeSelection(OldSelection,true);
  FilesTreeView.EndUpdate;
end;

procedure TPackageEditorForm.MoveFileUpMenuItemClick(Sender: TObject);
begin
  DoMoveCurrentFile(-1);
end;

procedure TPackageEditorForm.SetDependencyFilenameMenuItemClick(Sender: TObject
  );
var
  Removed: boolean;
  CurDependency: TPkgDependency;
  NewFilename: String;
begin
  CurDependency:=GetCurrentDependency(Removed);
  if (CurDependency=nil) or Removed then exit;
  if LazPackage.ReadOnly then exit;
  if CurDependency.RequiredPackage=nil then exit;
  NewFilename:=CurDependency.RequiredPackage.Filename;
  if NewFilename=CurDependency.DefaultFilename then exit;
  CurDependency.DefaultFilename:=NewFilename;
  LazPackage.Modified:=true;
  UpdateRequiredPkgs;
  UpdateButtons;
end;

procedure TPackageEditorForm.ClearDependencyFilenameMenuItemClick(
  Sender: TObject);
var
  Removed: boolean;
  CurDependency: TPkgDependency;
begin
  CurDependency:=GetCurrentDependency(Removed);
  if (CurDependency=nil) or Removed then exit;
  if LazPackage.ReadOnly then exit;
  if CurDependency.RequiredPackage=nil then exit;
  if CurDependency.DefaultFilename='' then exit;
  CurDependency.DefaultFilename:='';
  LazPackage.Modified:=true;
  UpdateRequiredPkgs;
  UpdateButtons;
end;

procedure TPackageEditorForm.MoveFileDownMenuItemClick(Sender: TObject);
begin
  DoMoveCurrentFile(1);
end;

procedure TPackageEditorForm.OpenFileMenuItemClick(Sender: TObject);
var
  CurNode: TTreeNode;
  NodeIndex: Integer;
  CurFile: TPkgFile;
  CurDependency: TPkgDependency;
begin
  CurNode:=FilesTreeView.Selected;
  if CurNode=nil then exit;
  NodeIndex:=CurNode.Index;
  if CurNode.Parent<>nil then begin
    if CurNode.Parent=FilesNode then begin
      CurFile:=LazPackage.Files[NodeIndex];
      DoOpenPkgFile(CurFile);
    end else if CurNode.Parent=RequiredPackagesNode then begin
      CurDependency:=LazPackage.RequiredDepByIndex(NodeIndex);
      PackageEditors.OpenDependency(Self,CurDependency);
    end else if CurNode.Parent=RemovedFilesNode then begin
      CurFile:=LazPackage.RemovedFiles[NodeIndex];
      DoOpenPkgFile(CurFile);
    end else if CurNode.Parent=RemovedRequiredNode then begin
      CurDependency:=LazPackage.RemovedDepByIndex(NodeIndex);
      PackageEditors.OpenDependency(Self,CurDependency);
    end;
  end;
end;

procedure TPackageEditorForm.OptionsBitBtnClick(Sender: TObject);
begin
  ShowPackageOptionsDlg(LazPackage);
  UpdateButtons;
  UpdateTitle;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.PackageEditorFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if LazPackage=nil then exit;
  PackageEditors.SaveLayout(Self);
end;

procedure TPackageEditorForm.PackageEditorFormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  MsgResult: Integer;
begin
  if (LazPackage=nil) or (lpfDestroying in LazPackage.Flags)
  or (LazPackage.ReadOnly) or (not LazPackage.Modified) then exit;

  MsgResult:=MessageDlg(lisPckEditSaveChanges,
    Format(lisPckEditPackageHasChangedSavePackage, ['"', LazPackage.IDAsString,
      '"', #13]),
    mtConfirmation,[mbYes,mbNo,mbAbort],0);
  if MsgResult=mrYes then begin
    MsgResult:=PackageEditors.SavePackage(LazPackage,false);
  end;
  if MsgResult=mrAbort then CanClose:=false;
end;

procedure TPackageEditorForm.RegisteredListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  CurComponent: TPkgComponent;
  CurStr: string;
  CurObject: TObject;
  TxtH: Integer;
  CurIcon: TBitmap;
  IconWidth: Integer;
  IconHeight: Integer;
  CurRect: TRect;
begin
  //DebugLn('TPackageEditorForm.RegisteredListBoxDrawItem START');
  if LazPackage=nil then exit;
  if (Index<0) or (Index>=FPlugins.Count) then exit;
  CurObject:=FPlugins.Objects[Index];
  if CurObject is TPkgComponent then begin
    // draw registered component
    CurComponent:=TPkgComponent(CurObject);
    with RegisteredListBox.Canvas do begin
      CurStr:=Format(lisPckEditPage, [CurComponent.ComponentClass.ClassName,
        CurComponent.Page.PageName]);
      TxtH:=TextHeight(CurStr);
      CurRect:=ARect;
      inc(CurRect.Left,25);
      FillRect(CurRect);
      Brush.Color:=clLtGray;
      CurRect:=ARect;
      CurRect.Right:=ARect.Left+25;
      FillRect(CurRect);
      CurIcon:=CurComponent.Icon;
      //DebugLn('TPackageEditorForm.RegisteredListBoxDrawItem ',DbgSName(CurIcon),' ',CurComponent.ComponentClass.ClassName);
      if CurIcon<>nil then begin
        IconWidth:=CurIcon.Width;
        IconHeight:=CurIcon.Height;
        Draw(ARect.Left+(25-IconWidth) div 2,
             ARect.Top+(ARect.Bottom-ARect.Top-IconHeight) div 2,
             CurIcon);
      end;
      TextOut(ARect.Left+25,
              ARect.Top+(ARect.Bottom-ARect.Top-TxtH) div 2,
              CurStr);
    end;
  end;
end;

procedure TPackageEditorForm.RemoveBitBtnClick(Sender: TObject);
var
  ANode: TTreeNode;
  NodeIndex: Integer;
  CurFile: TPkgFile;
  CurDependency: TPkgDependency;
begin
  ANode:=FilesTreeView.Selected;
  if (ANode=nil) or LazPackage.ReadOnly then begin
    UpdateButtons;
    exit;
  end;
  NodeIndex:=ANode.Index;
  if ANode.Parent=FilesNode then begin
    // get current package file
    CurFile:=LazPackage.Files[NodeIndex];
    if CurFile<>nil then begin
      // confirm deletion
      if MessageDlg(lisPckEditRemoveFile2,
        Format(lisPckEditRemoveFileFromPackage, ['"', CurFile.Filename, '"',
          #13, '"', LazPackage.IDAsString, '"']),
        mtConfirmation,[mbYes,mbNo],0)=mrNo
      then
        exit;
      LazPackage.RemoveFile(CurFile);
    end;
    UpdateAll;
  end else if ANode.Parent=RequiredPackagesNode then begin
    // get current dependency
    CurDependency:=LazPackage.RequiredDepByIndex(NodeIndex);
    if CurDependency<>nil then begin
      // confirm deletion
      if MessageDlg(lisPckEditRemoveDependency2,
        Format(lisPckEditRemoveDependencyFromPackage, ['"',
          CurDependency.AsString, '"', #13, '"', LazPackage.IDAsString, '"']),
        mtConfirmation,[mbYes,mbNo],0)=mrNo
      then
        exit;
      PackageGraph.RemoveDependencyFromPackage(LazPackage,CurDependency,true);
    end;
  end;
end;

procedure TPackageEditorForm.EditVirtualUnitMenuItemClick(Sender: TObject);
begin
  DoEditVirtualUnit;
end;

procedure TPackageEditorForm.RevertClick(Sender: TObject);
begin
  DoRevert;
end;

procedure TPackageEditorForm.SaveBitBtnClick(Sender: TObject);
begin
  DoSave(false);
end;

procedure TPackageEditorForm.SaveAsClick(Sender: TObject);
begin
  DoSave(true);
end;

procedure TPackageEditorForm.SortFilesMenuItemClick(Sender: TObject);
begin
  DoSortFiles;
end;

procedure TPackageEditorForm.FixFilesCaseMenuItemClick(Sender: TObject);
begin
  DoFixFilesCase;
end;

procedure TPackageEditorForm.UninstallClick(Sender: TObject);
begin
  PackageEditors.UninstallPackage(LazPackage);
end;

procedure TPackageEditorForm.ViewPkgSourceClick(Sender: TObject);
begin
  PackageEditors.ViewPkgSourcePackage(LazPackage);
end;

procedure TPackageEditorForm.UseMaxVersionCheckBoxChange(Sender: TObject);
begin
  MaxVersionEdit.Enabled:=UseMaxVersionCheckBox.Checked;
  UpdateApplyDependencyButton;
end;

procedure TPackageEditorForm.UseMinVersionCheckBoxChange(Sender: TObject);
begin
  MinVersionEdit.Enabled:=UseMinVersionCheckBox.Checked;
  UpdateApplyDependencyButton;
end;

procedure TPackageEditorForm.FilePropsGroupBoxResize(Sender: TObject);
var
  y: Integer;
  x: Integer;
  w: Integer;
begin
  // components for files
  w:=(ClientWidth-15) div 2;
  with CallRegisterProcCheckBox do
    SetBounds(3,0,w,Height);
  x:=3+w+9;
    
  with AddToUsesPkgSectionCheckBox do
    SetBounds(x,0,w,Height);

  y:=CallRegisterProcCheckBox.Top+CallRegisterProcCheckBox.Height+3;
  with RegisteredPluginsGroupBox do
    SetBounds(0,y,Parent.ClientWidth,Parent.ClientHeight-y);
    
  // components for dependencies
  x:=5;
  y:=3;
  with UseMinVersionCheckBox do
    SetBounds(x,y,150,MinVersionEdit.Height);
  inc(x,UseMinVersionCheckBox.Width+5);

  with MinVersionEdit do
    SetBounds(x,y,120,Height);
    
  x:=5;
  inc(y,MinVersionEdit.Height+5);
  with UseMaxVersionCheckBox do
    SetBounds(x,y,UseMinVersionCheckBox.Width,MaxVersionEdit.Height);
  inc(x,UseMaxVersionCheckBox.Width+5);

  with MaxVersionEdit do
    SetBounds(x,y,MinVersionEdit.Width,Height);
  inc(y,MaxVersionEdit.Height+10);

  x:=5;
  with ApplyDependencyButton do
    SetBounds(x,y,150,Height);
end;

procedure TPackageEditorForm.AddBitBtnClick(Sender: TObject);

  procedure AddUnit(AddParams: TAddToPkgResult);
  var
    NewLFMFilename: String;
    NewLRSFilename: String;
  begin
    NewLFMFilename:='';
    NewLRSFilename:='';
    // add lfm file
    if AddParams.AutoAddLFMFile then begin
      NewLFMFilename:=ChangeFileExt(AddParams.UnitFilename,'.lfm');
      if FileExists(NewLFMFilename)
      and (LazPackage.FindPkgFile(NewLFMFilename,false,true,false)=nil) then
        LazPackage.AddFile(NewLFMFilename,'',pftLFM,[],cpNormal)
      else
        NewLFMFilename:='';
    end;
    // add lrs file
    if AddParams.AutoAddLRSFile then begin
      NewLRSFilename:=ChangeFileExt(AddParams.UnitFilename,'.lrs');
      if FileExists(NewLRSFilename)
      and (LazPackage.FindPkgFile(NewLRSFilename,false,true,false)=nil) then
        LazPackage.AddFile(NewLRSFilename,'',pftLRS,[],cpNormal)
      else
        NewLRSFilename:='';
    end;
    ExtendUnitIncPathForNewUnit(AddParams.UnitFilename,NewLRSFilename);
    // add unit file
    with AddParams do
      LazPackage.AddFile(UnitFilename,UnitName,FileType,PkgFileFlags,cpNormal);
    PackageEditors.DeleteAmbiguousFiles(LazPackage,AddParams.UnitFilename);
    UpdateAll;
  end;
  
  procedure AddVirtualUnit(AddParams: TAddToPkgResult);
  begin
    with AddParams do
      LazPackage.AddFile(UnitFilename,UnitName,FileType,PkgFileFlags,cpNormal);
    PackageEditors.DeleteAmbiguousFiles(LazPackage,AddParams.UnitFilename);
    UpdateAll;
  end;
  
  procedure AddNewComponent(AddParams: TAddToPkgResult);
  begin
    ExtendUnitIncPathForNewUnit(AddParams.UnitFilename,'');
    // add file
    with AddParams do
      LazPackage.AddFile(UnitFilename,UnitName,FileType,PkgFileFlags,cpNormal);
    // add dependency
    if AddParams.Dependency<>nil then begin
      PackageGraph.AddDependencyToPackage(LazPackage,AddParams.Dependency);
    end;
    // open file in editor
    PackageEditors.CreateNewFile(Self,AddParams);
    UpdateAll;
  end;
  
  procedure AddRequiredPkg(AddParams: TAddToPkgResult);
  begin
    // add dependency
    PackageGraph.AddDependencyToPackage(LazPackage,AddParams.Dependency);
  end;
  
  procedure AddFile(AddParams: TAddToPkgResult);
  begin
    // add file
    with AddParams do
      LazPackage.AddFile(UnitFilename,UnitName,FileType,PkgFileFlags,cpNormal);
    UpdateAll;
  end;
  
  procedure AddNewFile(AddParams: TAddToPkgResult);
  var
    NewFilename: String;
    DummyResult: TModalResult;
    NewFileType: TPkgFileType;
    NewPkgFileFlags: TPkgFileFlags;
    Desc: TProjectFileDescriptor;
    NewUnitName: String;
    HasRegisterProc: Boolean;
  begin
    // create new file
    if AddParams.NewItem is TNewItemProjectFile then begin
      // create new file
      Desc:=TNewItemProjectFile(AddParams.NewItem).Descriptor;
      NewFilename:='';
      DummyResult:=LazarusIDE.DoNewFile(Desc,NewFilename,'',
        [nfOpenInEditor,nfCreateDefaultSrc,nfIsNotPartOfProject],LazPackage);
      if DummyResult=mrOk then begin
        // success
        // -> now add it to package
        NewUnitName:='';
        NewFileType:=FileNameToPkgFileType(NewFilename);
        NewPkgFileFlags:=[];
        if (NewFileType in PkgFileUnitTypes) then begin
          Include(NewPkgFileFlags,pffAddToPkgUsesSection);
          NewUnitName:=ExtractFilenameOnly(NewFilename);
          if Assigned(PackageEditors.OnGetUnitRegisterInfo) then begin
            HasRegisterProc:=false;
            PackageEditors.OnGetUnitRegisterInfo(Self,NewFilename,
              NewUnitName,HasRegisterProc);
            if HasRegisterProc then
              Include(NewPkgFileFlags,pffHasRegisterProc);
          end;
        end;
        LazPackage.AddFile(NewFilename,NewUnitName,NewFileType,NewPkgFileFlags,
                           cpNormal);
        UpdateAll;
      end;
    end;
  end;

var
  AddParams: TAddToPkgResult;
  OldParams: TAddToPkgResult;
begin
  if LazPackage.ReadOnly then begin
    UpdateButtons;
    exit;
  end;
  
  if ShowAddToPackageDlg(LazPackage,AddParams,PackageEditors.OnGetIDEFileInfo,
    PackageEditors.OnGetUnitRegisterInfo)
    <>mrOk
  then
    exit;

  PackageGraph.BeginUpdate(false);
  while AddParams<>nil do begin
    case AddParams.AddType of

    d2ptUnit:
      AddUnit(AddParams);

    d2ptVirtualUnit:
      AddVirtualUnit(AddParams);

    d2ptNewComponent:
      AddNewComponent(AddParams);

    d2ptRequiredPkg:
      AddRequiredPkg(AddParams);

    d2ptFile:
      AddFile(AddParams);
      
    d2ptNewFile:
      AddNewFile(AddParams);

    end;
    OldParams:=AddParams;
    AddParams:=AddParams.Next;
    OldParams.Next:=nil;
    OldParams.Free;
  end;
  AddParams.Free;
  LazPackage.Modified:=true;
  PackageGraph.EndUpdate;
end;

procedure TPackageEditorForm.AddToUsesPkgSectionCheckBoxChange(Sender: TObject);
var
  CurFile: TPkgFile;
  Removed: boolean;
begin
  if LazPackage=nil then exit;
  CurFile:=GetCurrentFile(Removed);
  if (CurFile=nil) then exit;
  if CurFile.AddToUsesPkgSection=AddToUsesPkgSectionCheckBox.Checked then exit;
  CurFile.AddToUsesPkgSection:=AddToUsesPkgSectionCheckBox.Checked;
  LazPackage.Modified:=not Removed;
  UpdateAll;
end;

procedure TPackageEditorForm.AddToProjectClick(Sender: TObject);
begin
  if LazPackage=nil then exit;
  PackageEditors.AddToProject(LazPackage,false);
end;

procedure TPackageEditorForm.ApplyDependencyButtonClick(Sender: TObject);
var
  CurDependency: TPkgDependency;
  Removed: boolean;
  NewDependency: TPkgDependency;
begin
  CurDependency:=GetCurrentDependency(Removed);
  if (CurDependency=nil) or Removed then exit;

  NewDependency:=TPkgDependency.Create;
  try
    NewDependency.Assign(CurDependency);

    // read minimum version
    if UseMinVersionCheckBox.Checked then begin
      NewDependency.Flags:=NewDependency.Flags+[pdfMinVersion];
      if not NewDependency.MinVersion.ReadString(MinVersionEdit.Text) then begin
        MessageDlg(lisPckEditInvalidMinimumVersion,
          Format(lisPckEditTheMinimumVersionIsNotAValidPackageVersion, ['"',
            MinVersionEdit.Text, '"', #13]),
          mtError,[mbCancel],0);
        exit;
      end;
    end else begin
      NewDependency.Flags:=NewDependency.Flags-[pdfMinVersion];
    end;

    // read maximum version
    if UseMaxVersionCheckBox.Checked then begin
      NewDependency.Flags:=NewDependency.Flags+[pdfMaxVersion];
      if not NewDependency.MaxVersion.ReadString(MaxVersionEdit.Text) then begin
        MessageDlg(lisPckEditInvalidMaximumVersion,
          Format(lisPckEditTheMaximumVersionIsNotAValidPackageVersion, ['"',
            MaxVersionEdit.Text, '"', #13]),
          mtError,[mbCancel],0);
        exit;
      end;
    end else begin
      NewDependency.Flags:=NewDependency.Flags-[pdfMaxVersion];
    end;

    PackageGraph.ChangeDependency(CurDependency,NewDependency);
  finally
    NewDependency.Free;
  end;
end;

procedure TPackageEditorForm.CallRegisterProcCheckBoxChange(Sender: TObject);
var
  CurFile: TPkgFile;
  Removed: boolean;
begin
  if LazPackage=nil then exit;
  CurFile:=GetCurrentFile(Removed);
  if (CurFile=nil) then exit;
  if CurFile.HasRegisterProc=CallRegisterProcCheckBox.Checked then exit;
  CurFile.HasRegisterProc:=CallRegisterProcCheckBox.Checked;
  LazPackage.Modified:=not Removed;
  UpdateAll;
end;

procedure TPackageEditorForm.ChangeFileTypeMenuItemClick(Sender: TObject);
var
  i: Integer;
  CurItem: TMenuItem;
  CurPFT: TPkgFileType;
  Removed: boolean;
  CurFile: TPkgFile;
begin
  CurItem:=TMenuItem(Sender);
  i:=CurItem.Parent.IndexOf(CurItem);
  if i<0 then exit;
  CurFile:=GetCurrentFile(Removed);
  if CurFile=nil then exit;
  for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
    if CurItem.Caption=GetPkgFileTypeLocalizedName(CurPFT) then begin
      if (not FilenameIsPascalUnit(CurFile.Filename))
      and (CurPFT in [pftUnit,pftVirtualUnit]) then exit;
      if CurFile.FileType<>CurPFT then begin
        CurFile.FileType:=CurPFT;
        LazPackage.Modified:=true;
        UpdateAll;
      end;
      exit;
    end;
  end;
end;

procedure TPackageEditorForm.CompileAllCleanClick(Sender: TObject);
begin
  if MessageDlg(lisPckEditCompileEverything,
    lisPckEditReCompileThisAndAllRequiredPackages,
    mtConfirmation,[mbYes,mbNo],0)<>mrYes then exit;
  DoCompile(true,true);
end;

procedure TPackageEditorForm.CompileCleanClick(Sender: TObject);
begin
  DoCompile(true,false);
end;

procedure TPackageEditorForm.CompileBitBtnClick(Sender: TObject);
begin
  DoCompile(false,false);
end;

procedure TPackageEditorForm.CompilerOptionsBitBtnClick(Sender: TObject);
var
  CompilerOptsDlg: TfrmCompilerOptions;
begin
  CompilerOptsDlg:=TfrmCompilerOptions.Create(Self);
  CompilerOptsDlg.CompilerOpts:=LazPackage.CompilerOptions;
  CompilerOptsDlg.OnImExportCompilerOptions:=
                                       PackageEditors.OnImExportCompilerOptions;
  with CompilerOptsDlg do begin
    GetCompilerOptions;
    Caption:=Format(lisPckEditCompilerOptionsForPackage,[LazPackage.IDAsString]);
    ReadOnly:=LazPackage.ReadOnly;
    if ShowModal=mrOk then
      LazPackage.DefineTemplates.AllChanged;
    Free;
  end;
  UpdateTitle;
  UpdateButtons;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.CreateMakefileClick(Sender: TObject);
begin
  PackageEditors.CreateMakefile(LazPackage);
end;

procedure TPackageEditorForm.SetLazPackage(const AValue: TLazPackage);
begin
  if FLazPackage=AValue then exit;
  if FLazPackage<>nil then FLazPackage.Editor:=nil;
  FLazPackage:=AValue;
  if FLazPackage=nil then exit;
  FLazPackage.Editor:=Self;
  PackageEditors.ApplyLayout(Self);
  // update components
  UpdateAll;
  // show files
  FilesNode.Expanded:=true;
end;

procedure TPackageEditorForm.SetupComponents;
  
  procedure LoadBitBtnGlyph(ABitBtn: TBitBtn; const ResName: string);
  begin
    ABitBtn.Glyph.LoadFromLazarusResource(ResName);
  end;

begin
  ImageIndexFiles := IDEImages.LoadImage(16, 'pkg_files');
  ImageIndexRemovedFiles := IDEImages.LoadImage(16, 'pkg_removedfiles');
  ImageIndexRequired := IDEImages.LoadImage(16, 'pkg_required');
  ImageIndexRemovedRequired := IDEImages.LoadImage(16, 'pkg_removedrequired');
  ImageIndexUnit := IDEImages.LoadImage(16, 'pkg_unit');
  ImageIndexRegisterUnit := IDEImages.LoadImage(16, 'pkg_registerunit');
  ImageIndexLFM := IDEImages.LoadImage(16, 'pkg_lfm');
  ImageIndexLRS := IDEImages.LoadImage(16, 'pkg_lrs');
  ImageIndexInclude := IDEImages.LoadImage(16, 'pkg_include');
  ImageIndexText := IDEImages.LoadImage(16, 'pkg_text');
  ImageIndexBinary := IDEImages.LoadImage(16, 'pkg_binary');
  ImageIndexConflict := IDEImages.LoadImage(16, 'pkg_conflict');
  
  SaveBitBtn:=TBitBtn.Create(Self);
  with SaveBitBtn do begin
    Name:='SaveBitBtn';
    Parent:=Self;
    Caption:=lisMenuSave;
    OnClick:=@SaveBitBtnClick;
    Hint:=lisPckEditSavePackage;
    ShowHint:=true;
  end;

  CompileBitBtn:=TBitBtn.Create(Self);
  with CompileBitBtn do begin
    Name:='CompileBitBtn';
    Parent:=Self;
    Caption:=lisPckEditCompile;
    OnClick:=@CompileBitBtnClick;
    Hint:=lisPckEditCompilePackage;
    ShowHint:=true;
  end;
  
  AddBitBtn:=TBitBtn.Create(Self);
  with AddBitBtn do begin
    Name:='AddBitBtn';
    Parent:=Self;
    Caption:=lisCodeTemplAdd;
    OnClick:=@AddBitBtnClick;
    Hint:=lisPckEditAddAnItem;
    ShowHint:=true;
  end;

  RemoveBitBtn:=TBitBtn.Create(Self);
  with RemoveBitBtn do begin
    Name:='RemoveBitBtn';
    Parent:=Self;
    Caption:=lisExtToolRemove;
    OnClick:=@RemoveBitBtnClick;
    Hint:=lisPckEditRemoveSelectedItem;
    ShowHint:=true;
  end;

  InstallBitBtn:=TBitBtn.Create(Self);
  with InstallBitBtn do begin
    Name:='InstallBitBtn';
    Parent:=Self;
    Caption:=lisPckEditInstall;
    OnClick:=@InstallBitBtnClick;
    Hint:=lisPckEditInstallPackageInTheIDE;
    ShowHint:=true;
  end;

  OptionsBitBtn:=TBitBtn.Create(Self);
  with OptionsBitBtn do begin
    Name:='OptionsBitBtn';
    Parent:=Self;
    Caption:=dlgFROpts;
    OnClick:=@OptionsBitBtnClick;
    Hint:=lisPckEditEditGeneralOptions;
    ShowHint:=true;
  end;

  CompilerOptionsBitBtn:=TBitBtn.Create(Self);
  with CompilerOptionsBitBtn do begin
    Name:='CompilerOptionsBitBtn';
    Parent:=Self;
    Caption:=lisPckEditCompOpts;
    OnClick:=@CompilerOptionsBitBtnClick;
    Hint:=lisPckEditEditOptionsToCompilePackage;
    ShowHint:=true;
  end;

  HelpBitBtn:=TBitBtn.Create(Self);
  with HelpBitBtn do begin
    Name:='HelpBitBtn';
    Parent:=Self;
    Caption:=lisPckEditHelp;
    OnClick:=@HelpBitBtnClick;
    Hint:=lisPkgEdThereAreMoreFunctionsInThePopupmenu;
    ShowHint:=true;
  end;

  MoreBitBtn:=TBitBtn.Create(Self);
  with MoreBitBtn do begin
    Name:='MoreBitBtn';
    Parent:=Self;
    Caption:=lisPckEditMore;
    Enabled:=true;
    OnClick:=@MoreBitBtnClick;
    Hint:=lisPkgEdThereAreMoreFunctionsInThePopupmenu;
    ShowHint:=true;
  end;

  FilesPopupMenu:=TPopupMenu.Create(Self);
  with FilesPopupMenu do begin
    Name:='FilesPopupMenu';
    OnPopup:=@FilesPopupMenuPopup;
  end;

  FilesTreeView:=TTreeView.Create(Self);
  with FilesTreeView do begin
    Name:='FilesTreeView';
    Parent:=Self;
    BeginUpdate;
    Images := IDEImages.Images_16;
    FilesNode:=Items.Add(nil, dlgEnvFiles);
    FilesNode.ImageIndex:=ImageIndexFiles;
    FilesNode.SelectedIndex:=FilesNode.ImageIndex;
    RequiredPackagesNode:=Items.Add(nil, lisPckEditRequiredPackages);
    RequiredPackagesNode.ImageIndex:=ImageIndexRequired;
    RequiredPackagesNode.SelectedIndex:=RequiredPackagesNode.ImageIndex;
    EndUpdate;
    PopupMenu:=FilesPopupMenu;
    OnSelectionChanged:=@FilesTreeViewSelectionChanged;
    Options:=Options+[tvoRightClickSelect];
    OnDblClick:=@FilesTreeViewDblClick;
    Anchors:=[akLeft,akRight,akTop,akBottom];
  end;

  FilePropsGroupBox:=TGroupBox.Create(Self);
  with FilePropsGroupBox do begin
    Name:='FilePropsGroupBox';
    Parent:=Self;
    Caption:=lisPckEditFileProperties;
    OnResize:=@FilePropsGroupBoxResize;
    Anchors:=[akLeft,akRight,akBottom];
  end;

  CallRegisterProcCheckBox:=TCheckBox.Create(Self);
  with CallRegisterProcCheckBox do begin
    Name:='CallRegisterProcCheckBox';
    Parent:=FilePropsGroupBox;
    Caption:=lisPckEditRegisterUnit;
    UseOnChange:=true;
    OnChange:=@CallRegisterProcCheckBoxChange;
    Hint:=Format(lisPckEditCallRegisterProcedureOfSelectedUnit, ['"', '"']);
    ShowHint:=true;
  end;

  AddToUsesPkgSectionCheckBox:=TCheckBox.Create(Self);
  with AddToUsesPkgSectionCheckBox do begin
    Name:='AddToUsesPkgSectionCheckBox';
    Caption:=lisPkgMangUseUnit;
    UseOnChange:=true;
    OnChange:=@AddToUsesPkgSectionCheckBoxChange;
    Hint:=lisPkgMangAddUnitToUsesClauseOfPackageDisableThisOnlyForUnit;
    ShowHint:=true;
    Parent:=FilePropsGroupBox;
  end;

  RegisteredPluginsGroupBox:=TGroupBox.Create(Self);
  with RegisteredPluginsGroupBox do begin
    Name:='RegisteredPluginsGroupBox';
    Caption:=lisPckEditRegisteredPlugins;
    Parent:=FilePropsGroupBox;
  end;

  RegisteredListBox:=TListBox.Create(Self);
  with RegisteredListBox do begin
    Name:='RegisteredListBox';
    Align:=alClient;
    ItemHeight:=ComponentPaletteImageHeight;
    OnDrawItem:=@RegisteredListBoxDrawItem;
    Style:= lbOwnerDrawFixed;
    Parent:=RegisteredPluginsGroupBox;
    //DebugLn('TPackageEditorForm.SetupComponents AAAAAAAAAA');
  end;
  
  UseMinVersionCheckBox:=TCheckBox.Create(Self);
  with UseMinVersionCheckBox do begin
    Name:='UseMinVersionCheckBox';
    Caption:=lisPckEditMinimumVersion;
    UseOnChange:=true;
    OnChange:=@UseMinVersionCheckBoxChange;
    Parent:=FilePropsGroupBox;
  end;
  
  MinVersionEdit:=TEdit.Create(Self);
  with MinVersionEdit do begin
    Name:='MinVersionEdit';
    Text:='';
    OnChange:=@MinVersionEditChange;
    Parent:=FilePropsGroupBox;
  end;

  UseMaxVersionCheckBox:=TCheckBox.Create(Self);
  with UseMaxVersionCheckBox do begin
    Name:='UseMaxVersionCheckBox';
    Caption:=lisPckEditMaximumVersion;
    UseOnChange:=true;
    OnChange:=@UseMaxVersionCheckBoxChange;
    Parent:=FilePropsGroupBox;
  end;

  MaxVersionEdit:=TEdit.Create(Self);
  with MaxVersionEdit do begin
    Name:='MaxVersionEdit';
    Parent:=FilePropsGroupBox;
    Text:='';
    OnChange:=@MaxVersionEditChange;
  end;
  
  ApplyDependencyButton:=TButton.Create(Self);
  with ApplyDependencyButton do begin
    Name:='ApplyDependencyButton';
    Parent:=FilePropsGroupBox;
    Caption:=lisPckEditApplyChanges;
    OnClick:=@ApplyDependencyButtonClick;
  end;

  StatusBar:=TStatusBar.Create(Self);
  with StatusBar do begin
    Name:='StatusBar';
    Parent:=Self;
    Align:=alBottom;
  end;
  
  FilePropsGroupBox.AnchorParallel(akLeft,0,Self);
  FilePropsGroupBox.AnchorParallel(akRight,0,Self);
  FilePropsGroupBox.AnchorToNeighbour(akBottom,0,StatusBar);
  FilePropsGroupBox.Height:=120;
  
  FilesTreeView.AnchorToNeighbour(akTop,0,CompileBitBtn);
  FilesTreeView.AnchorParallel(akLeft,0,Self);
  FilesTreeView.AnchorParallel(akRight,0,Self);
  FilesTreeView.AnchorToNeighbour(akBottom,0,FilePropsGroupBox);
end;

procedure TPackageEditorForm.UpdateAll;
begin
  if LazPackage=nil then exit;
  FilesTreeView.BeginUpdate;
  UpdateTitle;
  UpdateButtons;
  UpdateFiles;
  UpdateRequiredPkgs;
  UpdateSelectedFile;
  UpdateStatusBar;
  FilesTreeView.EndUpdate;
end;

procedure TPackageEditorForm.UpdateTitle;
var
  NewCaption: String;
begin
  if LazPackage=nil then exit;
  NewCaption:=Format(lisPckEditPackage, [FLazPackage.Name]);
  if LazPackage.Modified then
    NewCaption:=NewCaption+'*';
  Caption:=NewCaption;
end;

procedure TPackageEditorForm.UpdateButtons;
begin
  if LazPackage=nil then exit;
  SaveBitBtn.Enabled:=(not LazPackage.ReadOnly)
                              and (LazPackage.IsVirtual or LazPackage.Modified);
  CompileBitBtn.Enabled:=(not LazPackage.IsVirtual);
  AddBitBtn.Enabled:=not LazPackage.ReadOnly;
  RemoveBitBtn.Enabled:=(not LazPackage.ReadOnly)
     and (FilesTreeView.Selected<>nil)
     and ((FilesTreeView.Selected.Parent=FilesNode)
           or (FilesTreeView.Selected.Parent=RequiredPackagesNode));
  InstallBitBtn.Enabled:=(not LazPackage.AutoCreated);
  OptionsBitBtn.Enabled:=true;
  CompilerOptionsBitBtn.Enabled:=true;
end;

procedure TPackageEditorForm.UpdateFiles;

  procedure SetImageIndex(ANode: TTreeNode; PkgFile: TPkgFile);
  begin
    case PkgFile.FileType of
    pftUnit,pftVirtualUnit:
      if PkgFile.HasRegisterProc then
        ANode.ImageIndex:=ImageIndexRegisterUnit
      else
        ANode.ImageIndex:=ImageIndexUnit;
    pftLFM: ANode.ImageIndex:=ImageIndexLFM;
    pftLRS: ANode.ImageIndex:=ImageIndexLRS;
    pftInclude: ANode.ImageIndex:=ImageIndexInclude;
    pftText: ANode.ImageIndex:=ImageIndexText;
    pftBinary: ANode.ImageIndex:=ImageIndexBinary;
    else
      ANode.ImageIndex:=-1;
    end;
    ANode.SelectedIndex:=ANode.ImageIndex;
  end;

var
  Cnt: Integer;
  i: Integer;
  CurFile: TPkgFile;
  CurNode: TTreeNode;
  NextNode: TTreeNode;
begin
  if LazPackage=nil then exit;
  FilesTreeView.BeginUpdate;
  
  // files
  CurNode:=FilesNode.GetFirstChild;
  Cnt:=LazPackage.FileCount;
  for i:=0 to Cnt-1 do begin
    if CurNode=nil then
      CurNode:=FilesTreeView.Items.AddChild(FilesNode,'');
    CurFile:=LazPackage.Files[i];
    CurNode.Text:=CurFile.GetShortFilename(true);
    SetImageIndex(CurNode,CurFile);
    CurNode:=CurNode.GetNextSibling;
  end;
  while CurNode<>nil do begin
    NextNode:=CurNode.GetNextSibling;
    CurNode.Free;
    CurNode:=NextNode;
  end;
  FilesNode.Expanded:=true;
  
  // removed files
  if LazPackage.RemovedFilesCount>0 then begin
    if RemovedFilesNode=nil then begin
      RemovedFilesNode:=
        FilesTreeView.Items.Add(RequiredPackagesNode,
                lisPckEditRemovedFilesTheseEntriesAreNotSavedToTheLpkFile);
      RemovedFilesNode.ImageIndex:=ImageIndexRemovedFiles;
      RemovedFilesNode.SelectedIndex:=RemovedFilesNode.ImageIndex;
    end;
    CurNode:=RemovedFilesNode.GetFirstChild;
    Cnt:=LazPackage.RemovedFilesCount;
    for i:=0 to Cnt-1 do begin
      if CurNode=nil then
        CurNode:=FilesTreeView.Items.AddChild(RemovedFilesNode,'');
      CurFile:=LazPackage.RemovedFiles[i];
      CurNode.Text:=CurFile.GetShortFilename(true);
      SetImageIndex(CurNode,CurFile);
      CurNode:=CurNode.GetNextSibling;
    end;
    while CurNode<>nil do begin
      NextNode:=CurNode.GetNextSibling;
      CurNode.Free;
      CurNode:=NextNode;
    end;
    RemovedFilesNode.Expanded:=true;
  end else begin
    FreeAndNil(RemovedFilesNode);
  end;
  
  FilesTreeView.EndUpdate;
end;

procedure TPackageEditorForm.UpdateRequiredPkgs;
var
  CurNode: TTreeNode;
  CurDependency: TPkgDependency;
  NextNode: TTreeNode;
  CurNodeText: String;
begin
  if LazPackage=nil then exit;
  FilesTreeView.BeginUpdate;
  
  // required packages
  CurNode:=RequiredPackagesNode.GetFirstChild;
  CurDependency:=LazPackage.FirstRequiredDependency;
  while CurDependency<>nil do begin
    if CurNode=nil then
      CurNode:=FilesTreeView.Items.AddChild(RequiredPackagesNode,'');
    CurNodeText:=CurDependency.AsString;
    if CurDependency.DefaultFilename<>'' then
      CurNodeText:=CurNodeText+' in '
                   +CurDependency.MakeFilenameRelativeToOwner(
                                                 CurDependency.DefaultFilename);
    CurNode.Text:=CurNodeText;
    if CurDependency.LoadPackageResult=lprSuccess then
      CurNode.ImageIndex:=ImageIndexRequired
    else
      CurNode.ImageIndex:=ImageIndexConflict;
    CurNode.SelectedIndex:=CurNode.ImageIndex;
    CurNode:=CurNode.GetNextSibling;
    CurDependency:=CurDependency.NextRequiresDependency;
  end;
  while CurNode<>nil do begin
    NextNode:=CurNode.GetNextSibling;
    CurNode.Free;
    CurNode:=NextNode;
  end;
  RequiredPackagesNode.Expanded:=true;
  
  // removed required packages
  CurDependency:=LazPackage.FirstRemovedDependency;
  if CurDependency<>nil then begin
    if RemovedRequiredNode=nil then begin
      RemovedRequiredNode:=
        FilesTreeView.Items.Add(nil,
          lisPckEditRemovedRequiredPackagesTheseEntriesAreNotSaved);
      RemovedRequiredNode.ImageIndex:=ImageIndexRemovedRequired;
      RemovedRequiredNode.SelectedIndex:=RemovedRequiredNode.ImageIndex;
    end;
    CurNode:=RemovedRequiredNode.GetFirstChild;
    while CurDependency<>nil do begin
      if CurNode=nil then
        CurNode:=FilesTreeView.Items.AddChild(RemovedRequiredNode,'');
      CurNode.Text:=CurDependency.AsString;
      CurNode.ImageIndex:=RemovedRequiredNode.ImageIndex;
      CurNode.SelectedIndex:=CurNode.ImageIndex;
      CurNode:=CurNode.GetNextSibling;
      CurDependency:=CurDependency.NextRequiresDependency;
    end;
    while CurNode<>nil do begin
      NextNode:=CurNode.GetNextSibling;
      CurNode.Free;
      CurNode:=NextNode;
    end;
    RemovedRequiredNode.Expanded:=true;
  end else begin
    FreeAndNil(RemovedRequiredNode);
  end;

  FilesTreeView.EndUpdate;
end;

procedure TPackageEditorForm.UpdateSelectedFile;
var
  CurFile: TPkgFile;
  i: Integer;
  CurComponent: TPkgComponent;
  CurLine: string;
  CurListIndex: Integer;
  RegCompCnt: Integer;
  Dependency: TPkgDependency;
  Removed: boolean;
begin
  if LazPackage=nil then exit;
  FPlugins.Clear;
  Dependency:=nil;
  CurFile:=GetCurrentFile(Removed);
  if CurFile=nil then
    Dependency:=GetCurrentDependency(Removed);

  // make components visible
  UseMinVersionCheckBox.Visible:=Dependency<>nil;
  MinVersionEdit.Visible:=Dependency<>nil;
  UseMaxVersionCheckBox.Visible:=Dependency<>nil;
  MaxVersionEdit.Visible:=Dependency<>nil;
  ApplyDependencyButton.Visible:=Dependency<>nil;

  CallRegisterProcCheckBox.Visible:=CurFile<>nil;
  AddToUsesPkgSectionCheckBox.Visible:=CurFile<>nil;
  RegisteredPluginsGroupBox.Visible:=CurFile<>nil;

  if CurFile<>nil then begin
    FilePropsGroupBox.Enabled:=true;
    FilePropsGroupBox.Caption:=lisPckEditFileProperties;
    // set Register Unit checkbox
    CallRegisterProcCheckBox.Enabled:=(not LazPackage.ReadOnly)
                             and (CurFile.FileType in [pftUnit,pftVirtualUnit]);
    CallRegisterProcCheckBox.Checked:=pffHasRegisterProc in CurFile.Flags;
    AddToUsesPkgSectionCheckBox.Checked:=pffAddToPkgUsesSection in CurFile.Flags;
    AddToUsesPkgSectionCheckBox.Enabled:=(not LazPackage.ReadOnly)
                                     and (CurFile.FileType in PkgFileUnitTypes);
    // fetch all registered plugins
    CurListIndex:=0;
    RegCompCnt:=CurFile.ComponentCount;
    for i:=0 to RegCompCnt-1 do begin
      CurComponent:=CurFile.Components[i];
      CurLine:=CurComponent.ComponentClass.ClassName;
      FPlugins.AddObject(CurLine,CurComponent);
      inc(CurListIndex);
    end;
    // put them in the RegisteredListBox
    RegisteredListBox.Items.Assign(FPlugins);
  end else if Dependency<>nil then begin
    FilePropsGroupBox.Enabled:=not Removed;
    FilePropsGroupBox.Caption:=lisPckEditDependencyProperties;
    UseMinVersionCheckBox.Checked:=pdfMinVersion in Dependency.Flags;
    MinVersionEdit.Text:=Dependency.MinVersion.AsString;
    MinVersionEdit.Enabled:=pdfMinVersion in Dependency.Flags;
    UseMaxVersionCheckBox.Checked:=pdfMaxVersion in Dependency.Flags;
    MaxVersionEdit.Text:=Dependency.MaxVersion.AsString;
    MaxVersionEdit.Enabled:=pdfMaxVersion in Dependency.Flags;
    UpdateApplyDependencyButton;
  end else begin
    FilePropsGroupBox.Enabled:=false;
  end;
end;

procedure TPackageEditorForm.UpdateApplyDependencyButton;
var
  DepencyChanged: Boolean;
  CurDependency: TPkgDependency;
  AVersion: TPkgVersion;
  Removed: boolean;
begin
  if LazPackage=nil then exit;
  DepencyChanged:=false;
  CurDependency:=GetCurrentDependency(Removed);
  if (CurDependency<>nil) then begin
    // check min version
    if UseMinVersionCheckBox.Checked
    <>(pdfMinVersion in CurDependency.Flags)
    then begin
      DepencyChanged:=true;
    end;
    if UseMinVersionCheckBox.Checked then begin
      AVersion:=TPkgVersion.Create;
      if AVersion.ReadString(MinVersionEdit.Text)
      and (AVersion.Compare(CurDependency.MinVersion)<>0) then begin
        DepencyChanged:=true;
      end;
      AVersion.Free;
    end;
    // check max version
    if UseMaxVersionCheckBox.Checked
    <>(pdfMaxVersion in CurDependency.Flags)
    then begin
      DepencyChanged:=true;
    end;
    if UseMaxVersionCheckBox.Checked then begin
      AVersion:=TPkgVersion.Create;
      if AVersion.ReadString(MaxVersionEdit.Text)
      and (AVersion.Compare(CurDependency.MaxVersion)<>0) then begin
        DepencyChanged:=true;
      end;
      AVersion.Free;
    end;
  end;
  ApplyDependencyButton.Enabled:=DepencyChanged;
end;

procedure TPackageEditorForm.UpdateStatusBar;
var
  StatusText: String;
begin
  if LazPackage=nil then exit;
  if LazPackage.IsVirtual and (not LazPackage.ReadOnly) then begin
    StatusText:=Format(lisPckEditpackageNotSaved, [LazPackage.Name]);
  end else begin
    StatusText:=LazPackage.Filename;
  end;
  if LazPackage.ReadOnly then
    StatusText:=Format(lisPckEditReadOnly, [StatusText]);
  if LazPackage.Modified then
    StatusText:=Format(lisPckEditModified, [StatusText]);
  StatusBar.SimpleText:=StatusText;
end;

function TPackageEditorForm.GetCurrentDependency(var Removed: boolean
  ): TPkgDependency;
var
  CurNode: TTreeNode;
  NodeIndex: Integer;
begin
  Result:=nil;
  CurNode:=FilesTreeView.Selected;
  if (CurNode<>nil) and (CurNode.Parent<>nil) then begin
    NodeIndex:=CurNode.Index;
    if CurNode.Parent=RequiredPackagesNode then begin
      Result:=LazPackage.RequiredDepByIndex(NodeIndex);
      Removed:=false;
    end else if CurNode.Parent=RemovedRequiredNode then begin
      Result:=LazPackage.RemovedDepByIndex(NodeIndex);
      Removed:=true;
    end;
  end;
end;

function TPackageEditorForm.GetCurrentFile(var Removed: boolean): TPkgFile;
var
  CurNode: TTreeNode;
  NodeIndex: Integer;
begin
  Result:=nil;
  CurNode:=FilesTreeView.Selected;
  if (CurNode<>nil) and (CurNode.Parent<>nil) then begin
    NodeIndex:=CurNode.Index;
    if CurNode.Parent=FilesNode then begin
      Result:=LazPackage.Files[NodeIndex];
      Removed:=false;
    end else if CurNode.Parent=RemovedFilesNode then begin
      Result:=LazPackage.RemovedFiles[NodeIndex];
      Removed:=true;
    end;
  end;
end;

function TPackageEditorForm.StoreCurrentTreeSelection: TStringList;
var
  ANode: TTreeNode;
begin
  Result:=TStringList.Create;
  ANode:=FilesTreeView.Selected;
  while ANode<>nil do begin
    Result.Insert(0,ANode.Text);
    ANode:=ANode.Parent;
  end;
end;

procedure TPackageEditorForm.ApplyTreeSelection(ASelection: TStringList;
  FreeList: boolean);
var
  ANode: TTreeNode;
  CurText: string;
begin
  ANode:=nil;
  while ASelection.Count>0 do begin
    CurText:=ASelection[0];
    if ANode=nil then
      ANode:=FilesTreeView.Items.GetFirstNode
    else
      ANode:=ANode.GetFirstChild;
    while ANode.Text<>CurText do ANode:=ANode.GetNextSibling;
    if ANode=nil then break;
    ASelection.Delete(0);
  end;
  if ANode<>nil then FilesTreeView.Selected:=ANode;
  if FreeList then ASelection.Free;
end;

procedure TPackageEditorForm.ExtendUnitIncPathForNewUnit(const AnUnitFilename,
  AnIncludeFile: string);
var
  NewDirectory: String;
  UnitPath: String;
  ShortDirectory: String;
  NewIncDirectory: String;
  ShortIncDirectory: String;
  IncPath: String;
  UnitPathPos: Integer;
  IncPathPos: Integer;
begin
  if LazPackage=nil then exit;
  // check if directory is already in the unit path of the package
  NewDirectory:=ExtractFilePath(AnUnitFilename);
  ShortDirectory:=NewDirectory;
  LazPackage.ShortenFilename(ShortDirectory,false);
  if ShortDirectory='' then exit;
  LazPackage.LongenFilename(NewDirectory);
  
  UnitPath:=LazPackage.GetUnitPath(false);
  UnitPathPos:=SearchDirectoryInSearchPath(UnitPath,NewDirectory,1);
  IncPathPos:=1;
  if AnIncludeFile<>'' then begin
    NewIncDirectory:=ExtractFilePath(AnIncludeFile);
    ShortIncDirectory:=NewIncDirectory;
    LazPackage.ShortenFilename(ShortIncDirectory,false);
    if ShortIncDirectory<>'' then begin
      LazPackage.LongenFilename(NewIncDirectory);
      IncPath:=LazPackage.GetIncludePath(false);
      IncPathPos:=SearchDirectoryInSearchPath(IncPath,NewIncDirectory,1);
    end;
  end;
  if UnitPathPos<1 then begin
    // ask user to add the unit path
    if MessageDlg(lisPkgEditNewUnitNotInUnitpath,
        Format(lisPkgEditTheFileIsCurrentlyNotInTheUnitpathOfThePackage, ['"',
          AnUnitFilename, '"', #13, #13, #13, '"', ShortDirectory, '"']),
        mtConfirmation,[mbYes,mbNo],0)<>mrYes
    then exit;
    // add path
    with LazPackage.CompilerOptions do
      OtherUnitFiles:=MergeSearchPaths(OtherUnitFiles,ShortDirectory);
  end;
  if IncPathPos<1 then begin
    // the unit is in unitpath, but the include file not in the incpath
    // -> auto extend the include path
    with LazPackage.CompilerOptions do
      IncludePath:=MergeSearchPaths(IncludePath,ShortIncDirectory);
  end;
end;

function TPackageEditorForm.CanBeAddedToProject: boolean;
begin
  if LazPackage=nil then exit;
  Result:=PackageEditors.AddToProject(LazPackage,true)=mrOk;
end;

procedure TPackageEditorForm.DoSave(SaveAs: boolean);
begin
  PackageEditors.SavePackage(LazPackage,SaveAs);
  UpdateButtons;
  UpdateTitle;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.DoCompile(CompileClean, CompileRequired: boolean);
begin
  PackageEditors.CompilePackage(LazPackage,CompileClean,CompileRequired);
  UpdateButtons;
  UpdateTitle;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.DoRevert;
begin
  if MessageDlg(lisPkgEditRevertPackage,
    Format(lisPkgEditDoYouReallyWantToForgetAllChangesToPackageAnd, [
      LazPackage.IDAsString]),
    mtConfirmation,[mbYes,mbNo],0)<>mrYes
  then exit;
  PackageEditors.RevertPackage(LazPackage);
  UpdateAll;
end;

procedure TPackageEditorForm.DoPublishProject;
begin
  PackageEditors.PublishPackage(LazPackage);
  UpdateAll;
end;

procedure TPackageEditorForm.DoEditVirtualUnit;
var
  Removed: boolean;
  CurFile: TPkgFile;
begin
  CurFile:=GetCurrentFile(Removed);
  if (CurFile=nil) or Removed then exit;
  if ShowEditVirtualPackageDialog(CurFile)=mrOk then
    UpdateAll;
end;

procedure TPackageEditorForm.DoMoveCurrentFile(Offset: integer);
var
  Removed: boolean;
  CurFile: TPkgFile;
  OldIndex: Integer;
  NewIndex: Integer;
  TreeSelection: TStringList;
begin
  CurFile:=GetCurrentFile(Removed);
  if (CurFile=nil) or Removed then exit;
  OldIndex:=LazPackage.IndexOfPkgFile(CurFile);
  NewIndex:=OldIndex+Offset;
  if (NewIndex<0) or (NewIndex>=LazPackage.FileCount) then exit;
  TreeSelection:=StoreCurrentTreeSelection;
  LazPackage.MoveFile(OldIndex,NewIndex);
  UpdateFiles;
  ApplyTreeSelection(TreeSelection,true);
  UpdateButtons;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.DoSortFiles;
var
  TreeSelection: TStringList;
begin
  TreeSelection:=StoreCurrentTreeSelection;
  LazPackage.SortFiles;
  UpdateAll;
  ApplyTreeSelection(TreeSelection,true);
end;

procedure TPackageEditorForm.DoOpenPkgFile(PkgFile: TPkgFile);
begin
  PackageEditors.OpenPkgFile(Self,PkgFile);
end;

procedure TPackageEditorForm.DoFixFilesCase;
begin
  if LazPackage.FixFilesCaseSensitivity then
    LazPackage.Modified:=true;
  UpdateFiles;
  UpdateButtons;
end;

constructor TPackageEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPlugins:=TStringList.Create;
  SetupComponents;
  OnResize:=@PackageEditorFormResize;
  OnCloseQuery:=@PackageEditorFormCloseQuery;
  OnClose:=@PackageEditorFormClose;
end;

destructor TPackageEditorForm.Destroy;
begin
  PackageEditors.DoFreeEditor(LazPackage);
  FreeAndNil(FPlugins);
  inherited Destroy;
end;

{ TPackageEditors }

function TPackageEditors.GetEditors(Index: integer): TPackageEditorForm;
begin
  Result:=TPackageEditorForm(FItems[Index]);
end;

procedure TPackageEditors.ApplyLayout(AnEditor: TPackageEditorForm);
var
  PkgFilename: String;
  ANode: TAVLTreeNode;
  ARect: TRect;
begin
  if fLayouts=nil then LoadLayouts;
  PkgFilename:=AnEditor.LazPackage.Filename;
  ANode:=fLayouts.FindKey(Pointer(PkgFilename),@CompareFilenameWithLayout);
  // find a nice position for the editor
  if ANode<>nil then
    ARect:=TPackageEditorLayout(ANode.Data).Rectangle
  else
    ARect:=Rect(0,0,0,0);
  if (ARect.Bottom<ARect.Top+50) or (ARect.Right<ARect.Left+50)
  or (ARect.Bottom>Screen.Height) or (ARect.Right>Screen.Width) then
    ARect:=CreateNiceWindowPosition(500,400);
  AnEditor.SetBounds(ARect.Left,ARect.Top,
                     ARect.Right-ARect.Left,ARect.Bottom-ARect.Top);
end;

procedure TPackageEditors.SaveLayout(AnEditor: TPackageEditorForm);
var
  PkgFilename: String;
  ANode: TAVLTreeNode;
  CurLayout: TPackageEditorLayout;
begin
  if fLayouts=nil then exit;
  PkgFilename:=AnEditor.LazPackage.Filename;
  ANode:=fLayouts.FindKey(Pointer(PkgFilename),@CompareFilenameWithLayout);
  if ANode<>nil then begin
    CurLayout:=TPackageEditorLayout(ANode.Data);
    fLayouts.Remove(CurLayout);
  end else begin
    CurLayout:=TPackageEditorLayout.Create;
  end;
  CurLayout.Filename:=PkgFilename;
  with AnEditor do
    CurLayout.Rectangle:=Bounds(Left,Top,Width,Height);
  fLayouts.Add(CurLayout);
end;

procedure TPackageEditors.LoadLayouts;
var
  Filename: String;
  Path: String;
  XMLConfig: TXMLConfig;
  LayoutCount: Integer;
  NewLayout: TPackageEditorLayout;
  i: Integer;
begin
  if fLayouts=nil then fLayouts:=TAVLTree.Create(@CompareLayouts);
  fLayouts.FreeAndClear;
  Filename:=GetLayoutConfigFilename;
  if not FileExists(Filename) then exit;
  try
    XMLConfig:=TXMLConfig.Create(Filename);
  except
    DebugLn('ERROR: unable to open package editor layouts "',Filename,'"');
    exit;
  end;
  try
    try
      Path:='PackageEditorLayouts/';
      LayoutCount:=XMLConfig.GetValue(Path+'Count/Value',0);
      for i:=1 to LayoutCount do begin
        NewLayout:=TPackageEditorLayout.Create;
        NewLayout.LoadFromXMLConfig(XMLConfig,Path+'Layout'+IntToStr(i));
        if (NewLayout.Filename='') or (fLayouts.Find(NewLayout)<>nil) then
          NewLayout.Free
        else
          fLayouts.Add(NewLayout);
      end;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('ERROR: unable read miscellaneous options from "',Filename,'": ',E.Message);
    end;
  end;
end;

function TPackageEditors.GetLayoutConfigFilename: string;
begin
  Result:=SetDirSeparators(GetPrimaryConfigPath+'/packageeditorlayouts.xml');
end;

constructor TPackageEditors.Create;
begin
  FItems:=TList.Create;
end;

destructor TPackageEditors.Destroy;
begin
  Clear;
  FItems.Free;
  if fLayouts<>nil then begin
    fLayouts.FreeAndClear;
    fLayouts.Free;
  end;
  inherited Destroy;
end;

function TPackageEditors.Count: integer;
begin
  Result:=FItems.Count;
end;

procedure TPackageEditors.Clear;
begin
  FItems.Clear;
end;

procedure TPackageEditors.SaveLayouts;
var
  Filename: String;
  XMLConfig: TXMLConfig;
  Path: String;
  LayoutCount: Integer;
  ANode: TAVLTreeNode;
  CurLayout: TPackageEditorLayout;
begin
  if fLayouts=nil then exit;
  Filename:=GetLayoutConfigFilename;
  try
    XMLConfig:=TXMLConfig.CreateClean(Filename);
  except
    on E: Exception do begin
      DebugLn('ERROR: unable to open miscellaneous options "',Filename,'": ',E.Message);
      exit;
    end;
  end;
  try
    try
      Path:='PackageEditorLayouts/';
      LayoutCount:=0;
      ANode:=fLayouts.FindLowest;
      while ANode<>nil do begin
        inc(LayoutCount);
        CurLayout:=TPackageEditorLayout(ANode.Data);
        CurLayout.SaveToXMLConfig(XMLConfig,Path+'Layout'+IntToStr(LayoutCount));
        ANode:=fLayouts.FindSuccessor(ANode);
      end;
      XMLConfig.SetDeleteValue(Path+'Count/Value',LayoutCount,0);

      InvalidateFileStateCache;
      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    DebugLn('ERROR: unable read miscellaneous options from "',Filename,'"');
  end;
end;

procedure TPackageEditors.Remove(Editor: TPackageEditorForm);
begin
  FItems.Remove(Editor);
end;

function TPackageEditors.IndexOfPackage(Pkg: TLazPackage): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Editors[Result].LazPackage<>Pkg) do dec(Result);
end;

function TPackageEditors.FindEditor(Pkg: TLazPackage): TPackageEditorForm;
var
  i: Integer;
begin
  i:=IndexOfPackage(Pkg);
  if i>=0 then
    Result:=Editors[i]
  else
    Result:=nil;
end;

function TPackageEditors.OpenEditor(Pkg: TLazPackage): TPackageEditorForm;
begin
  Result:=FindEditor(Pkg);
  if Result=nil then begin
    Result:=TPackageEditorForm.Create(Application);
    Result.LazPackage:=Pkg;
    FItems.Add(Result);
  end;
end;

function TPackageEditors.OpenFile(Sender: TObject; const Filename: string
  ): TModalResult;
begin
  if Assigned(OnOpenFile) then
    Result:=OnOpenFile(Sender,Filename)
  else
    Result:=mrCancel;
end;

function TPackageEditors.OpenPkgFile(Sender: TObject; PkgFile: TPkgFile
  ): TModalResult;
begin
  if Assigned(OnOpenPkgFile) then
    Result:=OnOpenPkgFile(Sender,PkgFile)
  else
    Result:=mrCancel;
end;

function TPackageEditors.OpenDependency(Sender: TObject;
  Dependency: TPkgDependency): TModalResult;
var
  APackage: TLazPackage;
begin
  Result:=mrCancel;
  if PackageGraph.OpenDependency(Dependency)=lprSuccess then
  begin
    APackage:=Dependency.RequiredPackage;
    if Assigned(OnOpenPackage) then Result:=OnOpenPackage(Sender,APackage);
  end;
end;

procedure TPackageEditors.DoFreeEditor(Pkg: TLazPackage);
begin
  FItems.Remove(Pkg.Editor);
  if Assigned(OnFreeEditor) then OnFreeEditor(Pkg);
end;

function TPackageEditors.CreateNewFile(Sender: TObject;
  Params: TAddToPkgResult): TModalResult;
begin
  Result:=mrCancel;
  if Assigned(OnCreateNewFile) then
    Result:=OnCreateNewFile(Sender,Params)
  else
    Result:=mrCancel;
end;

function TPackageEditors.SavePackage(APackage: TLazPackage;
  SaveAs: boolean): TModalResult;
begin
  if Assigned(OnSavePackage) then
    Result:=OnSavePackage(Self,APackage,SaveAs)
  else
    Result:=mrCancel;
end;

function TPackageEditors.CompilePackage(APackage: TLazPackage;
  CompileClean, CompileRequired: boolean): TModalResult;
begin
  if Assigned(OnCompilePackage) then
    Result:=OnCompilePackage(Self,APackage,CompileClean,CompileRequired)
  else
    Result:=mrCancel;
end;

procedure TPackageEditors.UpdateAllEditors;
var
  i: Integer;
begin
  for i:=0 to Count-1 do Editors[i].UpdateAll;
end;

function TPackageEditors.InstallPackage(APackage: TLazPackage): TModalResult;
begin
  if Assigned(OnInstallPackage) then
    Result:=OnInstallPackage(Self,APackage)
  else
    Result:=mrCancel;
end;

function TPackageEditors.UninstallPackage(APackage: TLazPackage): TModalResult;
begin
  if Assigned(OnUninstallPackage) then
    Result:=OnUninstallPackage(Self,APackage)
  else
    Result:=mrCancel;
end;

function TPackageEditors.ViewPkgSourcePackage(APackage: TLazPackage
  ): TModalResult;
begin
  if Assigned(OnViewPackageSource) then
    Result:=OnViewPackageSource(Self,APackage)
  else
    Result:=mrCancel;
end;

function TPackageEditors.DeleteAmbiguousFiles(APackage: TLazPackage;
  const Filename: string): TModalResult;
begin
  if Assigned(OnDeleteAmbiguousFiles) then
    Result:=OnDeleteAmbiguousFiles(Self,APackage,Filename)
  else
    Result:=mrOk;
end;

function TPackageEditors.AddToProject(APackage: TLazPackage;
  OnlyTestIfPossible: boolean): TModalResult;
begin
  if Assigned(OnAddToProject) then
    Result:=OnAddToProject(Self,APackage,OnlyTestIfPossible)
  else
    Result:=mrCancel;
end;

function TPackageEditors.CreateMakefile(APackage: TLazPackage): TModalResult;
begin
  if Assigned(OnCreateMakefile) then
    Result:=OnCreateMakefile(Self,APackage)
  else
    Result:=mrCancel;
end;

function TPackageEditors.RevertPackage(APackage: TLazPackage): TModalResult;
begin
  if Assigned(OnRevertPackage) then
    Result:=OnRevertPackage(Self,APackage)
  else
    Result:=mrCancel;
end;

function TPackageEditors.PublishPackage(APackage: TLazPackage): TModalResult;
begin
  if Assigned(OnPublishPackage) then
    Result:=OnPublishPackage(Self,APackage)
  else
    Result:=mrCancel;
end;

{ TPackageEditorLayout }

constructor TPackageEditorLayout.Create;
begin

end;

destructor TPackageEditorLayout.Destroy;
begin
  inherited Destroy;
end;

procedure TPackageEditorLayout.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  Filename:=XMLConfig.GetValue(Path+'Filename/Value','');
  LoadRect(XMLConfig,Path+'Rect/',Rectangle);
end;

procedure TPackageEditorLayout.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'Filename/Value',Filename,'');
  SaveRect(XMLConfig,Path+'Rect/',Rectangle);
end;

initialization
  PackageEditors:=nil;

end.

