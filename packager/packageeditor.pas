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
  // LCL FCL
  Classes, SysUtils, Math, Forms, Controls, StdCtrls, ComCtrls, Buttons,
  LResources, Graphics, LCLType, LCLProc, Menus, Dialogs, FileUtil, AVL_Tree,
  // IDEIntf CodeTools
  IDEImagesIntf, MenuIntf, HelpIntfs, ExtCtrls, LazIDEIntf, ProjectIntf,
  CodeToolsStructs, FormEditingIntf, Laz_XMLCfg, TreeFilterEdit, PackageIntf,
  IDEDialogs, IDEHelpIntf, IDEOptionsIntf,
  // IDE
  MainIntf, IDEProcs, LazConf, LazarusIDEStrConsts, IDEOptionDefs, IDEDefs,
  IDEContextHelpEdit, CompilerOptions, ComponentReg,
  PackageDefs, AddToPackageDlg, PkgVirtualUnitEditor,
  MissingPkgFilesDlg, PackageSystem;
  
const
  PackageEditorMenuRootName = 'PackageEditor';
  PackageEditorWindowPrefix = 'PackageEditor_';
var
  PkgEditMenuOpenFile: TIDEMenuCommand;
  PkgEditMenuRemoveFile: TIDEMenuCommand;
  PkgEditMenuReAddFile: TIDEMenuCommand;
  PkgEditMenuMoveFileUp: TIDEMenuCommand;
  PkgEditMenuMoveFileDown: TIDEMenuCommand;
  PkgEditMenuEditVirtualUnit: TIDEMenuCommand;
  PkgEditMenuSectionFileType: TIDEMenuSection;

  PkgEditMenuExpandDirectory: TIDEMenuCommand;
  PkgEditMenuCollapseDirectory: TIDEMenuCommand;
  PkgEditMenuUseAllUnitsInDirectory: TIDEMenuCommand;
  PkgEditMenuUseNoUnitsInDirectory: TIDEMenuCommand;

  PkgEditMenuOpenPackage: TIDEMenuCommand;
  PkgEditMenuRemoveDependency: TIDEMenuCommand;
  PkgEditMenuReAddDependency: TIDEMenuCommand;
  PkgEditMenuMoveDependencyUp: TIDEMenuCommand;
  PkgEditMenuMoveDependencyDown: TIDEMenuCommand;
  PkgEditMenuDependencyStoreFileNameAsDefault: TIDEMenuCommand;
  PkgEditMenuDependencyStoreFileNameAsPreferred: TIDEMenuCommand;
  PkgEditMenuDependencyClearStoredFileName: TIDEMenuCommand;

  PkgEditMenuSortFiles: TIDEMenuCommand;
  PkgEditMenuFixFilesCase: TIDEMenuCommand;
  PkgEditMenuShowMissingFiles: TIDEMenuCommand;

  PkgEditMenuAddToProject: TIDEMenuCommand;
  PkgEditMenuInstall: TIDEMenuCommand;
  PkgEditMenuUninstall: TIDEMenuCommand;

  PkgEditMenuSave: TIDEMenuCommand;
  PkgEditMenuSaveAs: TIDEMenuCommand;
  PkgEditMenuRevert: TIDEMenuCommand;
  PkgEditMenuPublish: TIDEMenuCommand;

  PkgEditMenuCompile: TIDEMenuCommand;
  PkgEditMenuRecompileClean: TIDEMenuCommand;
  PkgEditMenuRecompileAllRequired: TIDEMenuCommand;
  PkgEditMenuCreateMakefile: TIDEMenuCommand;

  PkgEditMenuAdd: TIDEMenuCommand;
  PkgEditMenuRemove: TIDEMenuCommand;

  PkgEditMenuGeneralOptions: TIDEMenuCommand;
  PkgEditMenuViewPackageSource: TIDEMenuCommand;

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
  TOnViewPackageToDos =
    function(Sender: TObject; APackage: TLazPackage): TModalResult of object;
  TOnCreateNewPkgFile =
    function(Sender: TObject; Params: TAddToPkgResult): TModalResult  of object;
  TOnDeleteAmbiguousFiles =
    function(Sender: TObject; APackage: TLazPackage;
             const Filename: string): TModalResult of object;
  TOnFreePkgEditor = procedure(APackage: TLazPackage) of object;

  { TPackageEditorForm }

  TPackageEditorForm = class(TBasePackageEditor)
    DirectoryHierarchySpeedButton: TSpeedButton;
    FilterEdit: TTreeFilterEdit;
    ItemsPanel: TPanel;
    SortAlphabeticallySpeedButton: TSpeedButton;
    Splitter1: TSplitter;
    // toolbar
    ToolBar: TToolBar;
    // buttons
    SaveBitBtn: TToolButton;
    CompileBitBtn: TToolButton;
    AddBitBtn: TToolButton;
    RemoveBitBtn: TToolButton;
    UseBitBtn: TToolButton;
    OptionsBitBtn: TToolButton;
    MoreBitBtn: TToolButton;
    HelpBitBtn: TToolButton;
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
    UsePopupMenu: TPopupMenu;
    FilesPopupMenu: TPopupMenu;
    procedure AddBitBtnClick(Sender: TObject);
    procedure AddToProjectClick(Sender: TObject);
    procedure AddToUsesPkgSectionCheckBoxChange(Sender: TObject);
    procedure ApplyDependencyButtonClick(Sender: TObject);
    procedure CallRegisterProcCheckBoxChange(Sender: TObject);
    procedure ChangeFileTypeMenuItemClick(Sender: TObject);
    procedure ClearDependencyFilenameMenuItemClick(Sender: TObject);
    procedure CollapseDirectoryMenuItemClick(Sender: TObject);
    procedure CompileAllCleanClick(Sender: TObject);
    procedure CompileBitBtnClick(Sender: TObject);
    procedure CompileCleanClick(Sender: TObject);
    procedure CreateMakefileClick(Sender: TObject);
    procedure DirectoryHierarchySpeedButtonClick(Sender: TObject);
    procedure EditVirtualUnitMenuItemClick(Sender: TObject);
    procedure ExpandDirectoryMenuItemClick(Sender: TObject);
    procedure FilesPopupMenuPopup(Sender: TObject);
    procedure FilesTreeViewDblClick(Sender: TObject);
    procedure FilesTreeViewKeyPress(Sender: TObject; var Key: Char);
    procedure FilesTreeViewSelectionChanged(Sender: TObject);
    procedure FixFilesCaseMenuItemClick(Sender: TObject);
    procedure HelpBitBtnClick(Sender: TObject);
    procedure InstallClick(Sender: TObject);
    procedure MaxVersionEditChange(Sender: TObject);
    procedure MinVersionEditChange(Sender: TObject);
    procedure MoveDependencyDownClick(Sender: TObject);
    procedure MoveDependencyUpClick(Sender: TObject);
    procedure MoveFileDownMenuItemClick(Sender: TObject);
    procedure MoveFileUpMenuItemClick(Sender: TObject);
    procedure OpenFileMenuItemClick(Sender: TObject);
    procedure OptionsBitBtnClick(Sender: TObject);
    procedure PackageEditorFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure PackageEditorFormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure PublishClick(Sender: TObject);
    procedure ReAddMenuItemClick(Sender: TObject);
    procedure RegisteredListBoxDrawItem(Control: TWinControl; Index: Integer;
                                        ARect: TRect; State: TOwnerDrawState);
    procedure RemoveBitBtnClick(Sender: TObject);
    procedure RevertClick(Sender: TObject);
    procedure SaveAsClick(Sender: TObject);
    procedure SaveBitBtnClick(Sender: TObject);
    procedure SetDependencyDefaultFilenameMenuItemClick(Sender: TObject);
    procedure SetDependencyPreferredFilenameMenuItemClick(Sender: TObject);
    procedure ShowMissingFilesMenuItemClick(Sender: TObject);
    procedure SortAlphabeticallySpeedButtonClick(Sender: TObject);
    procedure SortFilesMenuItemClick(Sender: TObject);
    procedure UninstallClick(Sender: TObject);
    procedure UseAllUnitsInDirectoryMenuItemClick(Sender: TObject);
    procedure UseMaxVersionCheckBoxChange(Sender: TObject);
    procedure UseMinVersionCheckBoxChange(Sender: TObject);
    procedure UseNoUnitsInDirectoryMenuItemClick(Sender: TObject);
    procedure UsePopupMenuPopup(Sender: TObject);
    procedure ViewPkgSourceClick(Sender: TObject);
    procedure ViewPkgTodosClick(Sender: TObject);
  private
    FLazPackage: TLazPackage;
    FNextSelectedPart: TObject;// select this file/dependency on next update
    FFilesNode: TTreeNode;
    FRequiredPackagesNode: TTreeNode;
    FRemovedFilesNode: TTreeNode;
    FRemovedRequiredNode: TTreeNode;
    FPlugins: TStringList;
    FShowDirectoryHierarchy: boolean;
    FSortAlphabetically: boolean;
    FDirSummaryLabel: TLabel;
    procedure SetDependencyDefaultFilename(AsPreferred: boolean);
    procedure SetShowDirectoryHierarchy(const AValue: boolean);
    procedure SetSortAlphabetically(const AValue: boolean);
    procedure SetupComponents;
    function ChooseImageIndex(Str: String; Data: TObject; var AIsEnabled: Boolean): Integer;
    procedure UpdateTitle;
    procedure UpdateButtons;
    procedure UpdateFiles;
    procedure UpdateRequiredPkgs;
    procedure UpdateSelectedFile;
    procedure UpdateApplyDependencyButton;
    procedure UpdateStatusBar;
    function GetCurrentDependency(out Removed: boolean): TPkgDependency;
    function GetCurrentFile(out Removed: boolean): TPkgFile;
    function IsDirectoryNode(Node: TTreeNode): boolean;
    procedure GetDirectorySummary(DirNode: TTreeNode;
        out FileCount, HasRegisterProcCount, AddToUsesPkgSectionCount: integer);
    function StoreCurrentTreeSelection: TStringList;
    procedure ApplyTreeSelection(ASelection: TStringList; FreeList: boolean);
    procedure ExtendUnitIncPathForNewUnit(const AnUnitFilename,
      AnIncludeFile: string; var IgnoreUnitPaths: TFilenameToStringTree);
    procedure ExtendIncPathForNewIncludeFile(const AnIncludeFile: string;
      var IgnoreIncPaths: TFilenameToStringTree);
    function CanBeAddedToProject: boolean;
  protected
    procedure SetLazPackage(const AValue: TLazPackage); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoCompile(CompileClean, CompileRequired: boolean);
    procedure DoFixFilesCase;
    procedure DoShowMissingFiles;
    procedure DoMoveCurrentFile(Offset: integer);
    procedure DoPublishProject;
    procedure DoEditVirtualUnit;
    procedure DoExpandDirectory;
    procedure DoCollapseDirectory;
    procedure DoUseUnitsInDirectory(Use: boolean);
    procedure DoRevert;
    procedure DoSave(SaveAs: boolean);
    procedure DoSortFiles;
    procedure DoOpenPkgFile(PkgFile: TPkgFile);
    procedure UpdateAll(Immediately: boolean); override;
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
    property SortAlphabetically: boolean read FSortAlphabetically write SetSortAlphabetically;
    property ShowDirectoryHierarchy: boolean read FShowDirectoryHierarchy write SetShowDirectoryHierarchy;
  end;
  
  
  { TPackageEditors }
  
  TPackageEditors = class
  private
    FItems: TList; // list of TPackageEditorForm
    FOnAddToProject: TOnAddPkgToProject;
    FOnAfterWritePackage: TIDEOptionsWriteEvent;
    FOnBeforeReadPackage: TNotifyEvent;
    FOnCompilePackage: TOnCompilePackage;
    FOnCreateNewFile: TOnCreateNewPkgFile;
    FOnCreatePkgMakefile: TOnCreatePkgMakefile;
    FOnDeleteAmbiguousFiles: TOnDeleteAmbiguousFiles;
    FOnFreeEditor: TOnFreePkgEditor;
    FOnGetIDEFileInfo: TGetIDEFileStateEvent;
    FOnGetUnitRegisterInfo: TOnGetUnitRegisterInfo;
    FOnInstallPackage: TOnInstallPackage;
    FOnOpenFile: TOnOpenFile;
    FOnOpenPackage: TOnOpenPackage;
    FOnOpenPkgFile: TOnOpenPkgFile;
    FOnPublishPackage: TOnPublishPackage;
    FOnRevertPackage: TOnRevertPackage;
    FOnSavePackage: TOnSavePackage;
    FOnUninstallPackage: TOnUninstallPackage;
    FOnViewPackageSource: TOnViewPackageSource;
    FOnViewPackageToDos: TOnViewPackageToDos;
    function GetEditors(Index: integer): TPackageEditorForm;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    procedure Clear;
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
    procedure UpdateAllEditors(Immediately: boolean);
    function ShouldNotBeInstalled(APackage: TLazPackage): boolean;// possible, but probably a bad idea
    function InstallPackage(APackage: TLazPackage): TModalResult;
    function UninstallPackage(APackage: TLazPackage): TModalResult;
    function ViewPkgSource(APackage: TLazPackage): TModalResult;
    function ViewPkgToDos(APackage: TLazPackage): TModalResult;
    function DeleteAmbiguousFiles(APackage: TLazPackage;
                                  const Filename: string): TModalResult;
    function AddToProject(APackage: TLazPackage;
                          OnlyTestIfPossible: boolean): TModalResult;
    function CreateMakefile(APackage: TLazPackage): TModalResult;
  public
    property Editors[Index: integer]: TPackageEditorForm read GetEditors;
    property OnAddToProject: TOnAddPkgToProject read FOnAddToProject
                                                write FOnAddToProject;
    property OnAfterWritePackage: TIDEOptionsWriteEvent read FOnAfterWritePackage
                                               write FOnAfterWritePackage;
    property OnBeforeReadPackage: TNotifyEvent read FOnBeforeReadPackage
                                               write FOnBeforeReadPackage;
    property OnCompilePackage: TOnCompilePackage read FOnCompilePackage
                                                 write FOnCompilePackage;
    property OnCreateMakefile: TOnCreatePkgMakefile read FOnCreatePkgMakefile
                                                     write FOnCreatePkgMakefile;
    property OnCreateNewFile: TOnCreateNewPkgFile read FOnCreateNewFile
                                                  write FOnCreateNewFile;
    property OnDeleteAmbiguousFiles: TOnDeleteAmbiguousFiles
                     read FOnDeleteAmbiguousFiles write FOnDeleteAmbiguousFiles;
    property OnFreeEditor: TOnFreePkgEditor read FOnFreeEditor
                                            write FOnFreeEditor;
    property OnGetIDEFileInfo: TGetIDEFileStateEvent read FOnGetIDEFileInfo
                                                     write FOnGetIDEFileInfo;
    property OnGetUnitRegisterInfo: TOnGetUnitRegisterInfo
                       read FOnGetUnitRegisterInfo write FOnGetUnitRegisterInfo;
    property OnInstallPackage: TOnInstallPackage read FOnInstallPackage
                                                 write FOnInstallPackage;
    property OnOpenFile: TOnOpenFile read FOnOpenFile write FOnOpenFile;
    property OnOpenPackage: TOnOpenPackage read FOnOpenPackage
                                           write FOnOpenPackage;
    property OnOpenPkgFile: TOnOpenPkgFile read FOnOpenPkgFile
                                           write FOnOpenPkgFile;
    property OnPublishPackage: TOnPublishPackage read FOnPublishPackage
                                               write FOnPublishPackage;
    property OnRevertPackage: TOnRevertPackage read FOnRevertPackage
                                               write FOnRevertPackage;
    property OnSavePackage: TOnSavePackage read FOnSavePackage
                                           write FOnSavePackage;
    property OnUninstallPackage: TOnUninstallPackage read FOnUninstallPackage
                                                 write FOnUninstallPackage;
    property OnViewPackageSource: TOnViewPackageSource read FOnViewPackageSource
                                                 write FOnViewPackageSource;
    property OnViewPackageToDos: TOnViewPackageToDos read FOnViewPackageToDos
                                                 write FOnViewPackageToDos;
  end;
  
var
  PackageEditors: TPackageEditors;

procedure RegisterStandardPackageEditorMenuItems;

implementation

{$R *.lfm}

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
  ImageIndexIssues: integer;
  ImageIndexText: integer;
  ImageIndexBinary: integer;
  ImageIndexConflict: integer;
  ImageIndexDirectory: integer;

procedure RegisterStandardPackageEditorMenuItems;
var
  AParent: TIDEMenuSection;
begin
  PackageEditorMenuRoot:=RegisterIDEMenuRoot(PackageEditorMenuRootName);

  // register the section for operations on single files
  PkgEditMenuSectionFile:=RegisterIDEMenuSection(PackageEditorMenuRoot,'File');
  AParent:=PkgEditMenuSectionFile;
  PkgEditMenuOpenFile:=RegisterIDEMenuCommand(AParent,'Open File',lisOpenFile);
  PkgEditMenuRemoveFile:=RegisterIDEMenuCommand(AParent,'Remove File',lisPckEditRemoveFile);
  PkgEditMenuReAddFile:=RegisterIDEMenuCommand(AParent,'ReAdd File',lisPckEditReAddFile);
  PkgEditMenuMoveFileUp:=RegisterIDEMenuCommand(AParent,'Move File Up',lisPEMoveFileUp);
  PkgEditMenuMoveFileDown:=RegisterIDEMenuCommand(AParent,'Move File Down',lisPEMoveFileDown);
  PkgEditMenuEditVirtualUnit:=RegisterIDEMenuCommand(AParent,'Edit Virtual File',lisPEEditVirtualUnit);
  PkgEditMenuSectionFileType:=RegisterIDESubMenu(AParent,'File Type',lisAF2PFileType);

  // register the section for operations on directories
  PkgEditMenuSectionDirectory:=RegisterIDEMenuSection(PackageEditorMenuRoot,'Directory');
  AParent:=PkgEditMenuSectionDirectory;
  PkgEditMenuExpandDirectory:=RegisterIDEMenuCommand(AParent,'Expand directory',lisPEExpandDirectory);
  PkgEditMenuCollapseDirectory:=RegisterIDEMenuCommand(AParent, 'Collapse directory', lisPECollapseDirectory);
  PkgEditMenuUseAllUnitsInDirectory:=RegisterIDEMenuCommand(AParent, 'Use all units in directory', lisPEUseAllUnitsInDirectory);
  PkgEditMenuUseNoUnitsInDirectory:=RegisterIDEMenuCommand(AParent, 'Use no units in directory', lisPEUseNoUnitsInDirectory);

  // register the section for operations on single dependencies
  PkgEditMenuSectionDependency:=RegisterIDEMenuSection(PackageEditorMenuRoot,'Dependency');
  AParent:=PkgEditMenuSectionDependency;
  PkgEditMenuOpenPackage:=RegisterIDEMenuCommand(AParent,'Open Package',lisMenuOpenPackage);
  PkgEditMenuRemoveDependency:=RegisterIDEMenuCommand(AParent,'Remove Dependency',lisPckEditRemoveDependency);
  PkgEditMenuReAddDependency:=RegisterIDEMenuCommand(AParent,'ReAdd Dependency',lisPckEditReAddDependency);
  PkgEditMenuMoveDependencyUp:=RegisterIDEMenuCommand(AParent,'Move Dependency Up',lisPckEditMoveDependencyUp);
  PkgEditMenuMoveDependencyDown:=RegisterIDEMenuCommand(AParent,'Move Dependency Down',lisPckEditMoveDependencyDown);
  PkgEditMenuDependencyStoreFileNameAsDefault:=RegisterIDEMenuCommand(AParent,'Dependency Store Filename As Default',lisPckEditStoreFileNameAsDefaultForThisDependency);
  PkgEditMenuDependencyStoreFileNameAsPreferred:=RegisterIDEMenuCommand(AParent,'Dependency Store Filename As Preferred',lisPckEditStoreFileNameAsPreferredForThisDependency);
  PkgEditMenuDependencyClearStoredFileName:=RegisterIDEMenuCommand(AParent,'Dependency Clear Stored Filename',lisPckEditClearDefaultPreferredFilenameOfDependency);

  // register the section for operations on all files
  PkgEditMenuSectionFiles:=RegisterIDEMenuSection(PackageEditorMenuRoot,'Files');
  AParent:=PkgEditMenuSectionFiles;
  PkgEditMenuSortFiles:=RegisterIDEMenuCommand(AParent,'Sort Files',lisPESortFiles);
  PkgEditMenuFixFilesCase:=RegisterIDEMenuCommand(AParent,'Fix Files Case',lisPEFixFilesCase);
  PkgEditMenuShowMissingFiles:=RegisterIDEMenuCommand(AParent, 'Show Missing Files', lisPEShowMissingFiles);

  // register the section for using the package
  PkgEditMenuSectionUse:=RegisterIDEMenuSection(PackageEditorMenuRoot,'Use');
  AParent:=PkgEditMenuSectionUse;
  PkgEditMenuAddToProject:=RegisterIDEMenuCommand(AParent,'Add To Project',lisPckEditAddToProject);
  PkgEditMenuInstall:=RegisterIDEMenuCommand(AParent,'Install',lisPckEditInstall);
  PkgEditMenuUninstall:=RegisterIDEMenuCommand(AParent,'Uninstall',lisPckEditUninstall);

  // register the section for saving the package
  PkgEditMenuSectionSave:=RegisterIDEMenuSection(PackageEditorMenuRoot,'Save');
  AParent:=PkgEditMenuSectionSave;
  PkgEditMenuSave:=RegisterIDEMenuCommand(AParent, 'Save', lisPckEditSavePackage);
  PkgEditMenuSaveAs:=RegisterIDEMenuCommand(AParent, 'Save As', lisPESavePackageAs);
  PkgEditMenuRevert:=RegisterIDEMenuCommand(AParent, 'Revert', lisPERevertPackage);
  PkgEditMenuPublish:=RegisterIDEMenuCommand(AParent,'Publish',lisPkgEditPublishPackage);

  // register the section for compiling the package
  PkgEditMenuSectionCompile:=RegisterIDEMenuSection(PackageEditorMenuRoot,'Compile');
  AParent:=PkgEditMenuSectionCompile;
  PkgEditMenuCompile:=RegisterIDEMenuCommand(AParent,'Compile',lisPckEditCompile);
  PkgEditMenuRecompileClean:=RegisterIDEMenuCommand(AParent,'Recompile Clean',lisPckEditRecompileClean);
  PkgEditMenuRecompileAllRequired:=RegisterIDEMenuCommand(AParent,'Recompile All Required',lisPckEditRecompileAllRequired);
  PkgEditMenuCreateMakefile:=RegisterIDEMenuCommand(AParent,'Create Makefile',lisPckEditCreateMakefile);

  // register the section for adding to or removing from package
  PkgEditMenuSectionAddRemove:=RegisterIDEMenuSection(PackageEditorMenuRoot,'AddRemove');
  AParent:=PkgEditMenuSectionCompile;
  PkgEditMenuAdd:=RegisterIDEMenuCommand(AParent,'Add',lisCodeTemplAdd);
  PkgEditMenuRemove:=RegisterIDEMenuCommand(AParent,'Remove',lisExtToolRemove);

  // register the section for other things
  PkgEditMenuSectionMisc:=RegisterIDEMenuSection(PackageEditorMenuRoot,'Misc');
  AParent:=PkgEditMenuSectionMisc;
  PkgEditMenuGeneralOptions:=RegisterIDEMenuCommand(AParent,'General Options',lisPckEditGeneralOptions);
  PkgEditMenuViewPackageSource:=RegisterIDEMenuCommand(AParent,'View Package Source',lisPckEditViewPackageSource);
end;

{ TPackageEditorForm }

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
      if PkgFile.FileType in PkgFileRealUnitTypes then begin
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
      UpdateAll(true);
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
  CurDependency: TPkgDependency;
  Removed: boolean;
  CurFile: TPkgFile;
  Writable: Boolean;
  FileIndex: Integer;
  CurNode: TTreeNode;
  IsDir: Boolean;

  procedure SetItem(Item: TIDEMenuCommand; AnOnClick: TNotifyEvent;
                    aShow: boolean = true; AEnable: boolean = true);
  begin
    //debugln(['SetItem ',Item.Caption,' Visible=',aShow,' Enable=',AEnable]);
    Item.OnClick:=AnOnClick;
    Item.Visible:=aShow;
    Item.Enabled:=AEnable;
  end;

  procedure AddFileTypeMenuItem;
  var
    CurPFT: TPkgFileType;
    VirtualFileExists: Boolean;
    NewMenuItem: TIDEMenuCommand;
  begin
    PkgEditMenuSectionFileType.Clear;
    VirtualFileExists:=(CurFile.FileType=pftVirtualUnit)
                       and FileExistsUTF8(CurFile.Filename);
    for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
      NewMenuItem:=RegisterIDEMenuCommand(PkgEditMenuSectionFileType,
                      'SetFileType'+IntToStr(ord(CurPFT)),
                      GetPkgFileTypeLocalizedName(CurPFT),
                      @ChangeFileTypeMenuItemClick);
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
    end;
  end;

begin
  //debugln(['TPackageEditorForm.FilesPopupMenuPopup START ',FilesPopupMenu.Items.Count]);
  PackageEditorMenuRoot.MenuItem:=FilesPopupMenu.Items;
  //debugln(['TPackageEditorForm.FilesPopupMenuPopup START after connect ',FilesPopupMenu.Items.Count]);
  PackageEditorMenuRoot.BeginUpdate;
  try
    CurNode:=FilesTreeView.Selected;
    CurDependency:=GetCurrentDependency(Removed);
    Writable:=(not LazPackage.ReadOnly);
    if (CurDependency=nil) then
      CurFile:=GetCurrentFile(Removed)
    else
      CurFile:=nil;
    IsDir:=IsDirectoryNode(CurNode) or (CurNode=FFilesNode);

    PkgEditMenuSectionFileType.Clear;

    // items for single files
    PkgEditMenuSectionFile.Visible:=CurFile<>nil;
    if (CurFile<>nil) then begin
      FileIndex:=LazPackage.IndexOfPkgFile(CurFile);
      SetItem(PkgEditMenuOpenFile,@OpenFileMenuItemClick);
      SetItem(PkgEditMenuReAddFile,@ReAddMenuItemClick,Removed);
      SetItem(PkgEditMenuRemoveFile,@RemoveBitBtnClick,not Removed,RemoveBitBtn.Enabled);
      SetItem(PkgEditMenuMoveFileUp,@MoveFileUpMenuItemClick,not Removed,
                   (FileIndex>0) and Writable and (not SortAlphabetically));
      SetItem(PkgEditMenuMoveFileDown,@MoveFileDownMenuItemClick,
                   not Removed,
                   (FileIndex<LazPackage.FileCount-1) and Writable and (not SortAlphabetically));
      PkgEditMenuSectionFileType.Visible:=true;
      AddFileTypeMenuItem;
      SetItem(PkgEditMenuEditVirtualUnit,@EditVirtualUnitMenuItemClick,
        (CurFile.FileType=pftVirtualUnit) and not Removed,Writable);
    end;

    // items for directories
    PkgEditMenuSectionDirectory.Visible:=IsDir and ShowDirectoryHierarchy;
    if IsDir and ShowDirectoryHierarchy then begin
      SetItem(PkgEditMenuExpandDirectory,@ExpandDirectoryMenuItemClick);
      SetItem(PkgEditMenuCollapseDirectory,@CollapseDirectoryMenuItemClick);
      SetItem(PkgEditMenuUseAllUnitsInDirectory,@UseAllUnitsInDirectoryMenuItemClick);
      SetItem(PkgEditMenuUseNoUnitsInDirectory,@UseNoUnitsInDirectoryMenuItemClick);
    end;

    // items for all files
    SetItem(PkgEditMenuSortFiles,@SortFilesMenuItemClick,(LazPackage.FileCount>1),Writable);
    SetItem(PkgEditMenuFixFilesCase,@FixFilesCaseMenuItemClick,(LazPackage.FileCount>1),Writable);
    SetItem(PkgEditMenuShowMissingFiles,@ShowMissingFilesMenuItemClick,(LazPackage.FileCount>1),Writable);

    if CurDependency<>nil then begin
      PkgEditMenuSectionDependency.Visible:=true;
      SetItem(PkgEditMenuOpenPackage,@OpenFileMenuItemClick,CurDependency.RequiredPackage<>nil);
      SetItem(PkgEditMenuRemoveDependency,@RemoveBitBtnClick,not Removed,
              RemoveBitBtn.Enabled);
      SetItem(PkgEditMenuReAddDependency,@ReAddMenuItemClick,Removed and AddBitBtn.Enabled);
      SetItem(PkgEditMenuMoveDependencyUp,@MoveDependencyUpClick,not Removed,
              (CurDependency.PrevRequiresDependency<>nil) and Writable);
      SetItem(PkgEditMenuMoveDependencyDown,@MoveDependencyDownClick,not Removed,
              (CurDependency.NextRequiresDependency<>nil) and Writable);
      SetItem(PkgEditMenuDependencyStoreFileNameAsDefault,
              @SetDependencyDefaultFilenameMenuItemClick,not Removed,
              Writable and (CurDependency.RequiredPackage<>nil));
      SetItem(PkgEditMenuDependencyStoreFileNameAsPreferred,
              @SetDependencyPreferredFilenameMenuItemClick,not Removed,
              Writable and (CurDependency.RequiredPackage<>nil));
      SetItem(PkgEditMenuDependencyClearStoredFileName,
              @ClearDependencyFilenameMenuItemClick,not Removed,
              Writable and (CurDependency.DefaultFilename<>''));
    end else
      PkgEditMenuSectionDependency.Visible:=false;

    SetItem(PkgEditMenuAddToProject,@AddToProjectClick,true,CanBeAddedToProject);
    SetItem(PkgEditMenuInstall,@InstallClick,true,not LazPackage.AutoCreated);
    SetItem(PkgEditMenuUninstall,@UninstallClick,true,
            (LazPackage.Installed<>pitNope) or (LazPackage.AutoInstall<>pitNope));

    SetItem(PkgEditMenuSave,@SaveBitBtnClick,true,SaveBitBtn.Enabled);
    SetItem(PkgEditMenuSaveAs,@SaveAsClick,true,not LazPackage.AutoCreated);
    SetItem(PkgEditMenuRevert,@RevertClick,true,
            (not LazPackage.AutoCreated) and FileExistsUTF8(LazPackage.Filename));
    SetItem(PkgEditMenuPublish,@PublishClick,true,
            (not LazPackage.AutoCreated) and LazPackage.HasDirectory);

    SetItem(PkgEditMenuCompile,@CompileBitBtnClick,true,CompileBitBtn.Enabled);
    SetItem(PkgEditMenuRecompileClean,@CompileCleanClick,true,CompileBitBtn.Enabled);
    SetItem(PkgEditMenuRecompileAllRequired,@CompileAllCleanClick,true,CompileBitBtn.Enabled);
    SetItem(PkgEditMenuCreateMakefile,@CreateMakefileClick,true,CompileBitBtn.Enabled);

    SetItem(PkgEditMenuAdd,@AddBitBtnClick,true,AddBitBtn.Enabled);
    SetItem(PkgEditMenuRemove,@RemoveBitBtnClick,true,RemoveBitBtn.Enabled);

    SetItem(PkgEditMenuGeneralOptions,@OptionsBitBtnClick,true,OptionsBitBtn.Enabled);
    SetItem(PkgEditMenuViewPackageSource,@ViewPkgSourceClick);
  finally
    PackageEditorMenuRoot.EndUpdate;
  end;
  //debugln(['TPackageEditorForm.FilesPopupMenuPopup END ',FilesPopupMenu.Items.Count]); PackageEditorMenuRoot.WriteDebugReport('  ',true);
end;

procedure TPackageEditorForm.SortAlphabeticallySpeedButtonClick(Sender: TObject);
begin
  SortAlphabetically:=SortAlphabeticallySpeedButton.Down;
end;

procedure TPackageEditorForm.UsePopupMenuPopup(Sender: TObject);
var
  ItemCnt: Integer;

  function AddPopupMenuItem(const ACaption: string; AnEvent: TNotifyEvent;
    EnabledFlag: boolean): TMenuItem;
  begin
    if UsePopupMenu.Items.Count<=ItemCnt then begin
      Result:=TMenuItem.Create(Self);
      UsePopupMenu.Items.Add(Result);
    end else begin
      Result:=UsePopupMenu.Items[ItemCnt];
      while Result.Count>0 do Result.Delete(Result.Count-1);
    end;
    Result.Caption:=ACaption;
    Result.OnClick:=AnEvent;
    Result.Enabled:=EnabledFlag;
    inc(ItemCnt);
  end;

begin
  ItemCnt:=0;

  AddPopupMenuItem(lisPckEditAddToProject, @AddToProjectClick,
                   CanBeAddedToProject);
  AddPopupMenuItem(lisPckEditInstall, @InstallClick,not LazPackage.AutoCreated);
  AddPopupMenuItem(lisPckEditUninstall, @UninstallClick,
          (LazPackage.Installed<>pitNope) or (LazPackage.AutoInstall<>pitNope));

  // remove unneeded menu items
  while UsePopupMenu.Items.Count>ItemCnt do
    UsePopupMenu.Items.Delete(UsePopupMenu.Items.Count-1);
end;

procedure TPackageEditorForm.FilesTreeViewDblClick(Sender: TObject);
begin
  OpenFileMenuItemClick(Self);
end;

procedure TPackageEditorForm.FilesTreeViewKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    OpenFileMenuItemClick(Self);
end;

procedure TPackageEditorForm.FilesTreeViewSelectionChanged(Sender: TObject);
begin
  UpdateSelectedFile;
  UpdateButtons;
end;

procedure TPackageEditorForm.HelpBitBtnClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TPackageEditorForm.InstallClick(Sender: TObject);
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

procedure TPackageEditorForm.SetDependencyDefaultFilenameMenuItemClick(
  Sender: TObject);
begin
  SetDependencyDefaultFilename(false);
end;

procedure TPackageEditorForm.SetDependencyPreferredFilenameMenuItemClick(
  Sender: TObject);
begin
  SetDependencyDefaultFilename(true);
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
  CurDependency.PreferDefaultFilename:=false;
  LazPackage.Modified:=true;
  UpdateRequiredPkgs;
  UpdateButtons;
end;

procedure TPackageEditorForm.CollapseDirectoryMenuItemClick(Sender: TObject);
begin
  DoCollapseDirectory;
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
  Removed: boolean;
begin
  CurNode:=FilesTreeView.Selected;
  if CurNode=nil then exit;
  NodeIndex:=CurNode.Index;
  if CurNode.Parent<>nil then begin
    if TObject(CurNode.Data) is TFileNameItem then begin
      CurFile:=GetCurrentFile(Removed);
      if CurFile=nil then exit;
      DoOpenPkgFile(CurFile);
    end else if CurNode.Parent=FRequiredPackagesNode then begin
      CurDependency:=LazPackage.RequiredDepByIndex(NodeIndex);
      PackageEditors.OpenDependency(Self,CurDependency);
    end else if CurNode.Parent=FRemovedFilesNode then begin
      CurFile:=LazPackage.RemovedFiles[NodeIndex];
      DoOpenPkgFile(CurFile);
    end else if CurNode.Parent=FRemovedRequiredNode then begin
      CurDependency:=LazPackage.RemovedDepByIndex(NodeIndex);
      PackageEditors.OpenDependency(Self,CurDependency);
    end;
  end;
end;

procedure TPackageEditorForm.OptionsBitBtnClick(Sender: TObject);
const
  Settings: array[Boolean] of TIDEOptionsEditorSettings = (
    [],
    [ioesReadOnly]
  );
begin
  Package1 := LazPackage;
  Package1.OnBeforeRead:=PackageEditors.OnBeforeReadPackage;
  Package1.OnAfterWrite:=PackageEditors.OnAfterWritePackage;
  LazarusIDE.DoOpenIDEOptions(nil,
    Format(lisPckEditCompilerOptionsForPackage, [LazPackage.IDAsString]),
    [TLazPackage, TPkgCompilerOptions], Settings[LazPackage.ReadOnly]);
  UpdateButtons;
  UpdateTitle;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.PackageEditorFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if LazPackage=nil then exit;
end;

procedure TPackageEditorForm.PackageEditorFormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  MsgResult: Integer;
begin
  if (LazPackage=nil) or (lpfDestroying in LazPackage.Flags)
  or (LazPackage.ReadOnly) or (not LazPackage.Modified) then exit;

  MsgResult:=MessageDlg(lisPckEditSaveChanges,
    Format(lisPckEditPackageHasChangedSavePackage,['"',LazPackage.IDAsString,'"',#13]),
    mtConfirmation,[mbYes,mbNo,mbAbort],0);
  case MsgResult of
  mrYes:
    MsgResult:=PackageEditors.SavePackage(LazPackage,false);
  mrNo:
    LazPackage.UserIgnoreChangeStamp:=LazPackage.ChangeStamp;
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
  CurIcon: TCustomBitmap;
  IconWidth: Integer;
  IconHeight: Integer;
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
      FillRect(ARect);
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
  s: String;
  mt: TMsgDlgType;
  Removed: boolean;
begin
  ANode:=FilesTreeView.Selected;
  if (ANode=nil) or LazPackage.ReadOnly then begin
    UpdateButtons;
    exit;
  end;
  NodeIndex:=ANode.Index;
  if TObject(ANode.Data) is TFileNameItem then begin
    // get current package file
    CurFile:=GetCurrentFile(Removed);
    if CurFile<>nil then begin
      // confirm deletion
      s:='';
      mt:=mtConfirmation;
      if CurFile.FileType=pftMainUnit then begin
        s:=Format(lisWarningThisIsTheMainUnitTheNewMainUnitWillBePas, [#13#13,
          lowercase(LazPackage.Name)]);
        mt:=mtWarning;
      end;
      if MessageDlg(lisPckEditRemoveFile2,
        Format(lisPckEditRemoveFileFromPackage, ['"', CurFile.Filename, '"',
          #13, '"', LazPackage.IDAsString, '"'])+s,
        mt,[mbYes,mbNo],0)=mrNo
      then
        exit;
      LazPackage.RemoveFile(CurFile);
    end;
    UpdateAll(false);
  end else if ANode.Parent=FRequiredPackagesNode then begin
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

procedure TPackageEditorForm.ExpandDirectoryMenuItemClick(Sender: TObject);
begin
  DoExpandDirectory;
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

procedure TPackageEditorForm.ShowMissingFilesMenuItemClick(Sender: TObject);
begin
  DoShowMissingFiles;
end;

procedure TPackageEditorForm.UninstallClick(Sender: TObject);
begin
  PackageEditors.UninstallPackage(LazPackage);
end;

procedure TPackageEditorForm.UseAllUnitsInDirectoryMenuItemClick(Sender: TObject);
begin
  DoUseUnitsInDirectory(true);
end;

procedure TPackageEditorForm.ViewPkgSourceClick(Sender: TObject);
begin
  PackageEditors.ViewPkgSource(LazPackage);
end;

procedure TPackageEditorForm.ViewPkgTodosClick(Sender: TObject);
begin
  PackageEditors.ViewPkgToDos(LazPackage);
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

procedure TPackageEditorForm.UseNoUnitsInDirectoryMenuItemClick(Sender: TObject);
begin
  DoUseUnitsInDirectory(false);
end;

procedure TPackageEditorForm.AddBitBtnClick(Sender: TObject);
var
  IgnoreUnitPaths, IgnoreIncPaths: TFilenameToStringTree;

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
      if FileExistsUTF8(NewLFMFilename)
      and (LazPackage.FindPkgFile(NewLFMFilename,true,false)=nil) then
        LazPackage.AddFile(NewLFMFilename,'',pftLFM,[],cpNormal)
      else
        NewLFMFilename:='';
    end;
    // add lrs file
    if AddParams.AutoAddLRSFile then begin
      NewLRSFilename:=ChangeFileExt(AddParams.UnitFilename,'.lrs');
      if FileExistsUTF8(NewLRSFilename)
      and (LazPackage.FindPkgFile(NewLRSFilename,true,false)=nil) then
        LazPackage.AddFile(NewLRSFilename,'',pftLRS,[],cpNormal)
      else
        NewLRSFilename:='';
    end;
    ExtendUnitIncPathForNewUnit(AddParams.UnitFilename,NewLRSFilename,
                                IgnoreUnitPaths);
    // add unit file
    with AddParams do
      FNextSelectedPart := LazPackage.AddFile(UnitFilename,Unit_Name,
                                          FileType,PkgFileFlags,cpNormal);
    PackageEditors.DeleteAmbiguousFiles(LazPackage,AddParams.UnitFilename);
    UpdateAll(false);
  end;
  
  procedure AddVirtualUnit(AddParams: TAddToPkgResult);
  begin
    with AddParams do
      FNextSelectedPart := LazPackage.AddFile(UnitFilename,Unit_Name,FileType,
                                          PkgFileFlags,cpNormal);
    PackageEditors.DeleteAmbiguousFiles(LazPackage,AddParams.UnitFilename);
    UpdateAll(false);
  end;
  
  procedure AddNewComponent(AddParams: TAddToPkgResult);
  begin
    ExtendUnitIncPathForNewUnit(AddParams.UnitFilename,'',IgnoreUnitPaths);
    // add file
    with AddParams do
      FNextSelectedPart := LazPackage.AddFile(UnitFilename,Unit_Name,FileType,
                                              PkgFileFlags,cpNormal);
    // add dependency
    if (AddParams.Dependency<>nil)
    and (LazPackage.FindDependencyByName(AddParams.Dependency.PackageName)=nil)
    then
      PackageGraph.AddDependencyToPackage(LazPackage,AddParams.Dependency);
    if (AddParams.IconFile<>'')
    and (LazPackage.FindDependencyByName('LCL')=nil) then
      PackageGraph.AddDependencyToPackage(LazPackage,PackageGraph.LCLPackage);
    PackageEditors.DeleteAmbiguousFiles(LazPackage,AddParams.UnitFilename);
    // open file in editor
    PackageEditors.CreateNewFile(Self,AddParams);
    UpdateAll(false);
  end;
  
  procedure AddRequiredPkg(AddParams: TAddToPkgResult);
  begin
    // add dependency
    PackageGraph.AddDependencyToPackage(LazPackage,AddParams.Dependency);
    FNextSelectedPart := AddParams.Dependency;
    UpdateAll(false);
  end;
  
  procedure AddFile(AddParams: TAddToPkgResult);
  begin
    // add file
    with AddParams do begin
      if (CompareFileExt(UnitFilename,'.inc',false)=0)
      or (CompareFileExt(UnitFilename,'.lrs',false)=0) then
        ExtendIncPathForNewIncludeFile(UnitFilename,IgnoreIncPaths);
      FNextSelectedPart := LazPackage.AddFile(UnitFilename,Unit_Name,FileType,
                                          PkgFileFlags,cpNormal);
    end;
    UpdateAll(false);
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
        FNextSelectedPart := LazPackage.AddFile(NewFilename,NewUnitName,NewFileType,
                                            NewPkgFileFlags, cpNormal);
        UpdateAll(true);
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
  IgnoreUnitPaths:=nil;
  IgnoreIncPaths:=nil;
  try
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
  finally
    IgnoreUnitPaths.Free;
    IgnoreIncPaths.Free;
    PackageGraph.EndUpdate;
  end;
end;

procedure TPackageEditorForm.AddToUsesPkgSectionCheckBoxChange(Sender: TObject);
var
  CurFile: TPkgFile;
  Removed: boolean;
  i: Integer;
  OtherFile: TPkgFile;
begin
  if LazPackage=nil then exit;
  CurFile:=GetCurrentFile(Removed);
  if (CurFile=nil) then exit;
  if CurFile.AddToUsesPkgSection=AddToUsesPkgSectionCheckBox.Checked then exit;
  CurFile.AddToUsesPkgSection:=AddToUsesPkgSectionCheckBox.Checked;
  if not Removed then begin
    if CurFile.AddToUsesPkgSection then begin
      // mark all other units with the same name as unused
      for i:=0 to LazPackage.FileCount-1 do begin
        OtherFile:=LazPackage.Files[i];
        if (OtherFile<>CurFile)
        and (SysUtils.CompareText(OtherFile.Unit_Name,CurFile.Unit_Name)=0) then
          OtherFile.AddToUsesPkgSection:=false;
      end;
    end;
    LazPackage.Modified:=true;
  end;
  UpdateAll(true);
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
  if not Removed then begin
    LazPackage.Modified:=true;
  end;
  UpdateAll(true);
end;

procedure TPackageEditorForm.ChangeFileTypeMenuItemClick(Sender: TObject);
var
  CurPFT: TPkgFileType;
  Removed: boolean;
  CurFile: TPkgFile;
  CurItem: TIDEMenuCommand;
begin
  CurItem:=TIDEMenuCommand(Sender);
  CurFile:=GetCurrentFile(Removed);
  if CurFile=nil then exit;
  for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
    if CurItem.Caption=GetPkgFileTypeLocalizedName(CurPFT) then begin
      if (not FilenameIsPascalUnit(CurFile.Filename))
      and (CurPFT in PkgFileUnitTypes) then exit;
      if CurFile.FileType<>CurPFT then begin
        CurFile.FileType:=CurPFT;
        LazPackage.Modified:=true;
        UpdateAll(true);
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

procedure TPackageEditorForm.CreateMakefileClick(Sender: TObject);
begin
  PackageEditors.CreateMakefile(LazPackage);
end;

procedure TPackageEditorForm.DirectoryHierarchySpeedButtonClick(Sender: TObject);
begin
  ShowDirectoryHierarchy:=DirectoryHierarchySpeedButton.Down;
end;

procedure TPackageEditorForm.SetLazPackage(const AValue: TLazPackage);
begin
  if FLazPackage=AValue then exit;
  if FLazPackage<>nil then FLazPackage.Editor:=nil;
  FLazPackage:=AValue;
  if FLazPackage=nil then begin
    Name:=Name+'___off___';
    exit;
  end;
  Name:=PackageEditorWindowPrefix+LazPackage.Name;
  FLazPackage.Editor:=Self;
  // update components
  UpdateAll(true);
  // show files
//  FFilesNode.Expanded:=true;
end;

procedure TPackageEditorForm.SetupComponents;

  function CreateToolButton(AName, ACaption, AHint, AImageName: String; AOnClick: TNotifyEvent): TToolButton;
  begin
    Result := TToolButton.Create(Self);
    Result.Name := AName;
    Result.Caption := ACaption;
    Result.Hint := AHint;
    if AImageName <> '' then
      Result.ImageIndex := IDEImages.LoadImage(16, AImageName);
    Result.ShowHint := True;
    Result.OnClick := AOnClick;
    Result.AutoSize := True;
    Result.Parent := ToolBar;
  end;

  function CreateDivider: TToolButton;
  begin
    Result := TToolButton.Create(Self);
    Result.Style := tbsDivider;
    Result.AutoSize := True;
    Result.Parent := ToolBar;
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
  ImageIndexIssues := IDEImages.LoadImage(16, 'pkg_issues');
  ImageIndexText := IDEImages.LoadImage(16, 'pkg_text');
  ImageIndexBinary := IDEImages.LoadImage(16, 'pkg_binary');
  ImageIndexConflict := IDEImages.LoadImage(16, 'pkg_conflict');
  ImageIndexDirectory := IDEImages.LoadImage(16, 'pkg_files');

  FilterEdit.OnGetImageIndex:=@ChooseImageIndex;
  SortAlphabeticallySpeedButton.Hint:=lisPESortFilesAlphabetically;
  SortAlphabeticallySpeedButton.LoadGlyphFromLazarusResource('pkg_sortalphabetically');
  DirectoryHierarchySpeedButton.Hint:=lisPEShowDirectoryHierarchy;
  DirectoryHierarchySpeedButton.LoadGlyphFromLazarusResource('pkg_hierarchical');

  ToolBar.Images := IDEImages.Images_16;

  SaveBitBtn := CreateToolButton('SaveBitBtn', lisMenuSave, lisPckEditSavePackage, 'laz_save', @SaveBitBtnClick);
  CompileBitBtn := CreateToolButton('CompileBitBtn', lisPckEditCompile, lisPckEditCompilePackage, 'pkg_compile', @CompileBitBtnClick);
  UseBitBtn := CreateToolButton('UseBitBtn', lisPckEditInstall, lisPckEditInstallPackageInTheIDE, 'pkg_install', nil);
  CreateDivider;
  AddBitBtn := CreateToolButton('AddBitBtn', lisCodeTemplAdd, lisPckEditAddAnItem, 'laz_add', @AddBitBtnClick);
  RemoveBitBtn := CreateToolButton('RemoveBitBtn', lisExtToolRemove, lisPckEditRemoveSelectedItem, 'laz_delete', @RemoveBitBtnClick);
  CreateDivider;
  OptionsBitBtn := CreateToolButton('OptionsBitBtn', dlgFROpts, lisPckEditEditGeneralOptions, 'pkg_properties', @OptionsBitBtnClick);
  HelpBitBtn := CreateToolButton('HelpBitBtn', GetButtonCaption(idButtonHelp), lisPkgEdThereAreMoreFunctionsInThePopupmenu, 'menu_help', @HelpBitBtnClick);
  MoreBitBtn := CreateToolButton('MoreBitBtn', lisPckEditMore, lisPkgEdThereAreMoreFunctionsInThePopupmenu, '', nil);

  MoreBitBtn.DropdownMenu := FilesPopupMenu;

  FilesTreeView.BeginUpdate;
  FFilesNode:=FilesTreeView.Items.Add(nil, dlgEnvFiles);
  FFilesNode.ImageIndex:=ImageIndexFiles;
  FFilesNode.SelectedIndex:=FFilesNode.ImageIndex;
  FRequiredPackagesNode:=FilesTreeView.Items.Add(nil, lisPckEditRequiredPackages);
  FRequiredPackagesNode.ImageIndex:=ImageIndexRequired;
  FRequiredPackagesNode.SelectedIndex:=FRequiredPackagesNode.ImageIndex;
  FilesTreeView.EndUpdate;
  FilesTreeView.Images := IDEImages.Images_16;
  // ToDo: Options:=Options+[tvoRightClickSelect]

  FilePropsGroupBox.Caption:=lisPckEditFileProperties;

  CallRegisterProcCheckBox.Caption:=lisPckEditRegisterUnit;
  CallRegisterProcCheckBox.Hint:=Format(lisPckEditCallRegisterProcedureOfSelectedUnit, ['"', '"']);

  AddToUsesPkgSectionCheckBox.Caption:=lisPkgMangUseUnit;
  AddToUsesPkgSectionCheckBox.Hint:=lisPkgMangAddUnitToUsesClauseOfPackageDisableThisOnlyForUnit;

  UseMinVersionCheckBox.Caption:=lisPckEditMinimumVersion;
  UseMaxVersionCheckBox.Caption:=lisPckEditMaximumVersion;
  ApplyDependencyButton.Caption:=lisPckEditApplyChanges;
  RegisteredPluginsGroupBox.Caption:=lisPckEditRegisteredPlugins;
  RegisteredListBox.ItemHeight:=ComponentPaletteImageHeight;

  FDirSummaryLabel:=TLabel.Create(Self);
  with FDirSummaryLabel do
  begin
    Name:='DirSummaryLabel';
    Parent:=FilePropsGroupBox;
  end;
end;

procedure TPackageEditorForm.SetDependencyDefaultFilename(AsPreferred: boolean);
var
  NewFilename: String;
  CurDependency: TPkgDependency;
  Removed: boolean;
begin
  CurDependency:=GetCurrentDependency(Removed);
  if (CurDependency=nil) or Removed then exit;
  if LazPackage.ReadOnly then exit;
  if CurDependency.RequiredPackage=nil then exit;
  NewFilename:=CurDependency.RequiredPackage.Filename;
  if (NewFilename=CurDependency.DefaultFilename)
  and (CurDependency.PreferDefaultFilename=AsPreferred) then
    exit;
  CurDependency.DefaultFilename:=NewFilename;
  CurDependency.PreferDefaultFilename:=AsPreferred;
  LazPackage.Modified:=true;
  UpdateRequiredPkgs;
  UpdateButtons;
end;

procedure TPackageEditorForm.SetShowDirectoryHierarchy(const AValue: boolean);
begin
  //debugln(['TPackageEditorForm.SetShowDirectoryHierachy Old=',FShowDirectoryHierarchy,' New=',AValue]);
  if FShowDirectoryHierarchy=AValue then exit;
  FShowDirectoryHierarchy:=AValue;
  DirectoryHierarchySpeedButton.Down:=FShowDirectoryHierarchy;
  FilterEdit.ShowDirHierarchy:=FShowDirectoryHierarchy;
  FilterEdit.InvalidateFilter;
end;

procedure TPackageEditorForm.SetSortAlphabetically(const AValue: boolean);
begin
  if FSortAlphabetically=AValue then exit;
  FSortAlphabetically:=AValue;
  SortAlphabeticallySpeedButton.Down:=FSortAlphabetically;
  FilterEdit.ShowDirHierarchy:=FShowDirectoryHierarchy;
  FilterEdit.InvalidateFilter;
end;

procedure TPackageEditorForm.UpdateAll(Immediately: boolean);
begin
  if LazPackage=nil then exit;
  Name:=PackageEditorWindowPrefix+LazPackage.Name;
  UpdateTitle;
  UpdateButtons;
  UpdateFiles;
  UpdateRequiredPkgs;
  UpdateSelectedFile;
  UpdateStatusBar;
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
     and ((TObject(FilesTreeView.Selected.Data) is TFileNameItem)
           or (FilesTreeView.Selected.Parent=FRequiredPackagesNode));
  UseBitBtn.Caption:=lisUse;
  UseBitBtn.Hint:=lisClickToSeeThePossibleUses;
  UseBitBtn.OnClick:=nil;
  UseBitBtn.DropdownMenu:=UsePopupMenu;
  OptionsBitBtn.Enabled:=true;
end;

function TPackageEditorForm.ChooseImageIndex(Str: String; Data: TObject;
                                             var AIsEnabled: Boolean): Integer;
begin
  case TPkgFile(Data).FileType of
    pftUnit,pftVirtualUnit,pftMainUnit:
      if TPkgFile(Data).HasRegisterProc then
        Result:=ImageIndexRegisterUnit
      else
        Result:=ImageIndexUnit;
    pftLFM: Result:=ImageIndexLFM;
    pftLRS: Result:=ImageIndexLRS;
    pftInclude: Result:=ImageIndexInclude;
    pftIssues: Result:=ImageIndexIssues;
    pftText: Result:=ImageIndexText;
    pftBinary: Result:=ImageIndexBinary;
    else
      Result:=-1;
  end;
end;

procedure TPackageEditorForm.UpdateFiles;
var
  Cnt, i: Integer;
  CurFile: TPkgFile;
  CurNode: TTreeNode;
  NextNode: TTreeNode;
  Filename: String;
  ena: Boolean;
begin
  if LazPackage=nil then exit;
  FilterEdit.RootNode:=FFilesNode;
  FilterEdit.SelectedPart:=FNextSelectedPart;
  FilterEdit.ShowDirHierarchy:=ShowDirectoryHierarchy;
  FilterEdit.SortData:=SortAlphabetically;
  FilterEdit.ImageIndexDirectory:=ImageIndexDirectory;
  FilterEdit.Data.Clear;
  // collect and sort files
  for i:=0 to LazPackage.FileCount-1 do begin
    CurFile:=LazPackage.Files[i];
    Filename:=CurFile.GetShortFilename(true);
    if Filename<>'' then begin
      FilterEdit.Data.AddObject(Filename, CurFile);
      FilterEdit.MapShortToFullFilename(Filename, CurFile.Filename);
    end;
  end;
  FilterEdit.InvalidateFilter;               // Data is shown by FilterEdit.

  // removed files
  if LazPackage.RemovedFilesCount>0 then begin
    if FRemovedFilesNode=nil then begin
      FRemovedFilesNode:=
        FilesTreeView.Items.Add(FRequiredPackagesNode,
                lisPckEditRemovedFilesTheseEntriesAreNotSavedToTheLpkFile);
      FRemovedFilesNode.ImageIndex:=ImageIndexRemovedFiles;
      FRemovedFilesNode.SelectedIndex:=FRemovedFilesNode.ImageIndex;
    end;
    CurNode:=FRemovedFilesNode.GetFirstChild;
    Cnt:=LazPackage.RemovedFilesCount;
    for i:=0 to Cnt-1 do begin
      if CurNode=nil then
        CurNode:=FilesTreeView.Items.AddChild(FRemovedFilesNode,'');
      CurFile:=LazPackage.RemovedFiles[i];
      CurNode.Text:=CurFile.GetShortFilename(true); // SetImageIndex(CurNode,CurFile);
      CurNode.ImageIndex:=ChooseImageIndex('', CurFile, ena);
      CurNode:=CurNode.GetNextSibling;
    end;
    while CurNode<>nil do begin
      NextNode:=CurNode.GetNextSibling;
      CurNode.Free;
      CurNode:=NextNode;
    end;
    FRemovedFilesNode.Expanded:=true;
  end else begin
    FreeAndNil(FRemovedFilesNode);
  end;
end;

procedure TPackageEditorForm.UpdateRequiredPkgs;
var
  CurNode: TTreeNode;
  CurDependency: TPkgDependency;
  NextNode: TTreeNode;
  CurNodeText, aFilename: String;
begin
  if LazPackage=nil then exit;
  FilesTreeView.BeginUpdate;
  
  // required packages
  CurNode:=FRequiredPackagesNode.GetFirstChild;
  CurDependency:=LazPackage.FirstRequiredDependency;
  while CurDependency<>nil do begin
    if CurNode=nil then
      CurNode:=FilesTreeView.Items.AddChild(FRequiredPackagesNode,'');
    CurNodeText:=CurDependency.AsString;
    if CurDependency.DefaultFilename<>'' then begin
      aFilename:=CurDependency.MakeFilenameRelativeToOwner(
                                                 CurDependency.DefaultFilename);
      if CurDependency.PreferDefaultFilename then
        CurNodeText:=CurNodeText+' in '+aFilename // like the 'in' keyword the uses section
      else
        CurNodeText:=Format(lisPckEditDefault, [CurNodeText, aFilename]);
    end;
    CurNode.Text:=CurNodeText;
    if CurDependency.LoadPackageResult=lprSuccess then
      CurNode.ImageIndex:=ImageIndexRequired
    else
      CurNode.ImageIndex:=ImageIndexConflict;
    CurNode.Selected:=CurDependency=FNextSelectedPart;
    CurNode.SelectedIndex:=CurNode.ImageIndex;
    CurNode:=CurNode.GetNextSibling;
    CurDependency:=CurDependency.NextRequiresDependency;
  end;
  while CurNode<>nil do begin
    NextNode:=CurNode.GetNextSibling;
    CurNode.Free;
    CurNode:=NextNode;
  end;
  FRequiredPackagesNode.Expanded:=true;
  
  // removed required packages
  CurDependency:=LazPackage.FirstRemovedDependency;
  if CurDependency<>nil then begin
    if FRemovedRequiredNode=nil then begin
      FRemovedRequiredNode:=
        FilesTreeView.Items.Add(nil,
          lisPckEditRemovedRequiredPackagesTheseEntriesAreNotSaved);
      FRemovedRequiredNode.ImageIndex:=ImageIndexRemovedRequired;
      FRemovedRequiredNode.SelectedIndex:=FRemovedRequiredNode.ImageIndex;
    end;
    CurNode:=FRemovedRequiredNode.GetFirstChild;
    while CurDependency<>nil do begin
      if CurNode=nil then
        CurNode:=FilesTreeView.Items.AddChild(FRemovedRequiredNode,'');
      CurNode.Text:=CurDependency.AsString;
      CurNode.ImageIndex:=FRemovedRequiredNode.ImageIndex;
      CurNode.SelectedIndex:=CurNode.ImageIndex;
      CurNode:=CurNode.GetNextSibling;
      CurDependency:=CurDependency.NextRequiresDependency;
    end;
    while CurNode<>nil do begin
      NextNode:=CurNode.GetNextSibling;
      CurNode.Free;
      CurNode:=NextNode;
    end;
    FRemovedRequiredNode.Expanded:=true;
  end else begin
    FreeAndNil(FRemovedRequiredNode);
  end;

  FilesTreeView.EndUpdate;
  FNextSelectedPart:=nil;
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
  CurNode: TTreeNode;
  IsDir: Boolean;
  FileCount: integer;
  HasRegisterProcCount: integer;
  AddToUsesPkgSectionCount: integer;
begin
  if LazPackage=nil then exit;
  FPlugins.Clear;
  Dependency:=nil;
  CurFile:=GetCurrentFile(Removed);
  if CurFile=nil then
    Dependency:=GetCurrentDependency(Removed);
  CurNode:=FilesTreeView.Selected;
  IsDir:=IsDirectoryNode(CurNode) or (CurNode=FFilesNode);

  // make components visible
  UseMinVersionCheckBox.Visible:=Dependency<>nil;
  MinVersionEdit.Visible:=Dependency<>nil;
  UseMaxVersionCheckBox.Visible:=Dependency<>nil;
  MaxVersionEdit.Visible:=Dependency<>nil;
  ApplyDependencyButton.Visible:=Dependency<>nil;

  CallRegisterProcCheckBox.Visible:=CurFile<>nil;
  AddToUsesPkgSectionCheckBox.Visible:=CurFile<>nil;
  RegisteredPluginsGroupBox.Visible:=CurFile<>nil;

  FDirSummaryLabel.Visible:=IsDir;

  if CurFile<>nil then begin
    FilePropsGroupBox.Enabled:=true;
    FilePropsGroupBox.Caption:=lisPckEditFileProperties;
    // set Register Unit checkbox
    CallRegisterProcCheckBox.Enabled:=(not LazPackage.ReadOnly)
                             and (CurFile.FileType in [pftUnit,pftVirtualUnit]);
    CallRegisterProcCheckBox.Checked:=pffHasRegisterProc in CurFile.Flags;
    AddToUsesPkgSectionCheckBox.Checked:=(pffAddToPkgUsesSection in CurFile.Flags)
                                              or (CurFile.FileType=pftMainUnit);
    AddToUsesPkgSectionCheckBox.Enabled:=(not LazPackage.ReadOnly)
                             and (CurFile.FileType in [pftUnit,pftVirtualUnit]);
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
  end else if IsDir then begin
    FilePropsGroupBox.Enabled:=true;
    GetDirectorySummary(CurNode,FileCount,HasRegisterProcCount,AddToUsesPkgSectionCount);
    FDirSummaryLabel.Caption:='Files: '+IntToStr(FileCount)
             +', has Register procedure: '+IntToStr(HasRegisterProcCount)
             +', in package uses section: '+IntToStr(AddToUsesPkgSectionCount);
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

function TPackageEditorForm.GetCurrentDependency(out Removed: boolean
  ): TPkgDependency;
var
  CurNode: TTreeNode;
  NodeIndex: Integer;
begin
  Result:=nil;
  Removed:=false;
  CurNode:=FilesTreeView.Selected;
  if (CurNode<>nil) and (CurNode.Parent<>nil) then begin
    NodeIndex:=CurNode.Index;
    if CurNode.Parent=FRequiredPackagesNode then begin
      Result:=LazPackage.RequiredDepByIndex(NodeIndex);
      Removed:=false;
    end else if CurNode.Parent=FRemovedRequiredNode then begin
      Result:=LazPackage.RemovedDepByIndex(NodeIndex);
      Removed:=true;
    end;
  end;
end;

function TPackageEditorForm.GetCurrentFile(out Removed: boolean): TPkgFile;
var
  CurNode: TTreeNode;
  NodeIndex: Integer;
  Item: TFileNameItem;
  i: Integer;
begin
  Result:=nil;
  Removed:=false;
  CurNode:=FilesTreeView.Selected;
  if (CurNode=nil) or (CurNode.Parent=nil) then exit;
  NodeIndex:=CurNode.Index;
  if CurNode.Parent=FRemovedFilesNode then
  begin
    Result:=LazPackage.RemovedFiles[NodeIndex];
    Removed:=true;
    exit;
  end;
  //debugln(['TPackageEditorForm.GetCurrentFile ',DbgSName(TObject(CurNode.Data)),' ',CurNode.Text]);
  if TObject(CurNode.Data) is TFileNameItem then
  begin
    Item:=TFileNameItem(CurNode.Data);
    //debugln(['TPackageEditorForm.GetCurrentFile Item=',Item.Filename,' ',Item.IsDirectory]);
    for i:=0 to LazPackage.FileCount-1 do
    begin
      //if ExtractFIlename(LazPackage.Files[i].Filename)=ExtractFIlename(Item.Filename) then
      //  debugln(['TPackageEditorForm.GetCurrentFile FOUND ',LazPackage.Files[i].Filename,' ',Item.Filename=LazPackage.Files[i].Filename]);
      if Item.Filename=LazPackage.Files[i].Filename then
      begin
        Result:=LazPackage.Files[i];
        Removed:=false;
        exit;
      end;
    end;
  end;
end;

function TPackageEditorForm.IsDirectoryNode(Node: TTreeNode): boolean;
begin
  Result:=(Node<>nil) and (Node.Data=nil) and Node.HasAsParent(FFilesNode);
end;

procedure TPackageEditorForm.GetDirectorySummary(DirNode: TTreeNode; out
  FileCount, HasRegisterProcCount, AddToUsesPkgSectionCount: integer);

  procedure Traverse(Node: TTreeNode);
  var
    Item: TFileNameItem;
    CurFile: TPkgFile;
  begin
    if TObject(Node.Data) is TFileNameItem then begin
      Item:=TFileNameItem(Node.Data);
      CurFile:=LazPackage.FindPkgFile(Item.Filename,true,true);
      if CurFile<>nil then begin
        inc(FileCount);
        if CurFile.HasRegisterProc then inc(HasRegisterProcCount);
        if CurFile.AddToUsesPkgSection then inc(AddToUsesPkgSectionCount);
      end;
    end;
    Node:=Node.GetFirstChild;
    while Node<>nil do begin
      Traverse(Node);
      Node:=Node.GetNextSibling;
    end;
  end;

begin
  FileCount:=0;
  HasRegisterProcCount:=0;
  AddToUsesPkgSectionCount:=0;
  Traverse(DirNode);
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
    while (ANode<>nil) and (ANode.Text<>CurText) do
      ANode:=ANode.GetNextSibling;
    if ANode=nil then break;
    ASelection.Delete(0);
  end;
  if ANode<>nil then FilesTreeView.Selected:=ANode;
  if FreeList then
    ASelection.Free;
end;

procedure TPackageEditorForm.ExtendUnitIncPathForNewUnit(const AnUnitFilename,
  AnIncludeFile: string;
  var IgnoreUnitPaths: TFilenameToStringTree);
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
  NewDirectory:=ChompPathDelim(NewDirectory);
  
  UnitPath:=LazPackage.GetUnitPath(false);
  UnitPathPos:=SearchDirectoryInSearchPath(UnitPath,NewDirectory,1);
  IncPathPos:=1;
  if AnIncludeFile<>'' then begin
    NewIncDirectory:=ChompPathDelim(ExtractFilePath(AnIncludeFile));
    ShortIncDirectory:=NewIncDirectory;
    LazPackage.ShortenFilename(ShortIncDirectory,false);
    if ShortIncDirectory<>'' then begin
      LazPackage.LongenFilename(NewIncDirectory);
      NewIncDirectory:=ChompPathDelim(NewIncDirectory);
      IncPath:=LazPackage.GetIncludePath(false);
      IncPathPos:=SearchDirectoryInSearchPath(IncPath,NewIncDirectory,1);
    end;
  end;
  if UnitPathPos<1 then begin
    // ask user to add the unit path
    if (IgnoreUnitPaths<>nil) and (IgnoreUnitPaths.Contains(ShortDirectory))
    then exit;
    if MessageDlg(lisPkgEditNewUnitNotInUnitpath,
        Format(lisPkgEditTheFileIsCurrentlyNotInTheUnitpathOfThePackage, ['"',
          AnUnitFilename, '"', #13, #13, #13, '"', ShortDirectory, '"']),
        mtConfirmation,[mbYes,mbNo],0)<>mrYes
    then begin
      if IgnoreUnitPaths=nil then
        IgnoreUnitPaths:=TFilenameToStringTree.Create(false);
      IgnoreUnitPaths.Add(ShortDirectory,'');
      exit;
    end;
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

procedure TPackageEditorForm.ExtendIncPathForNewIncludeFile(
  const AnIncludeFile: string; var IgnoreIncPaths: TFilenameToStringTree);
var
  NewDirectory: String;
  ShortDirectory: String;
  IncPath: String;
  IncPathPos: LongInt;
begin
  if LazPackage=nil then exit;
  // check if directory is already in the unit path of the package
  NewDirectory:=ExtractFilePath(AnIncludeFile);
  ShortDirectory:=NewDirectory;
  LazPackage.ShortenFilename(ShortDirectory,false);
  if ShortDirectory='' then exit;
  LazPackage.LongenFilename(NewDirectory);
  NewDirectory:=ChompPathDelim(NewDirectory);
  IncPath:=LazPackage.GetIncludePath(false);
  IncPathPos:=SearchDirectoryInSearchPath(IncPath,NewDirectory,1);
  if IncPathPos>0 then exit;
  // ask user to add the unit path
  if (IgnoreIncPaths<>nil) and (IgnoreIncPaths.Contains(ShortDirectory))
  then exit;
  if MessageDlg(lisPENewFileNotInIncludePath,
     Format(lisPETheFileIsCurrentlyNotInTheIncludePathOfThePackageA, [
       AnIncludeFile, #13, ShortDirectory]),
      mtConfirmation,[mbYes,mbNo],0)<>mrYes
  then begin
    if IgnoreIncPaths=nil then
      IgnoreIncPaths:=TFilenameToStringTree.Create(false);
    IgnoreIncPaths.Add(ShortDirectory,'');
    exit;
  end;
  // add path
  with LazPackage.CompilerOptions do
    IncludePath:=MergeSearchPaths(IncludePath,ShortDirectory);
end;

function TPackageEditorForm.CanBeAddedToProject: boolean;
begin
  if LazPackage=nil then exit(false);
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
  UpdateAll(true);
end;

procedure TPackageEditorForm.DoPublishProject;
begin
  PackageEditors.PublishPackage(LazPackage);
  UpdateAll(true);
end;

procedure TPackageEditorForm.DoEditVirtualUnit;
var
  Removed: boolean;
  CurFile: TPkgFile;
begin
  CurFile:=GetCurrentFile(Removed);
  if (CurFile=nil) or Removed then exit;
  if ShowEditVirtualPackageDialog(CurFile)=mrOk then
    UpdateAll(true);
end;

procedure TPackageEditorForm.DoExpandDirectory;
var
  CurNode: TTreeNode;
begin
  if not ShowDirectoryHierarchy then exit;
  CurNode:=FilesTreeView.Selected;
  if not (IsDirectoryNode(CurNode) or (CurNode=FFilesNode)) then exit;
  FilesTreeView.BeginUpdate;
  CurNode.Expand(true);
  FilesTreeView.EndUpdate;
end;

procedure TPackageEditorForm.DoCollapseDirectory;
var
  CurNode: TTreeNode;
  Node: TTreeNode;
begin
  if not ShowDirectoryHierarchy then exit;
  CurNode:=FilesTreeView.Selected;
  if not (IsDirectoryNode(CurNode) or (CurNode=FFilesNode)) then exit;
  FilesTreeView.BeginUpdate;
  Node:=CurNode.GetFirstChild;
  while Node<>nil do
  begin
    Node.Collapse(true);
    Node:=Node.GetNextSibling;
  end;
  FilesTreeView.EndUpdate;
end;

procedure TPackageEditorForm.DoUseUnitsInDirectory(Use: boolean);

  procedure Traverse(Node: TTreeNode);
  var
    PkgFile: TPkgFile;
  begin
    if TObject(Node.Data) is TFileNameItem then
    begin
      PkgFile:=LazPackage.FindPkgFile(TFileNameItem(Node.Data).Filename,true,true);
      if (PkgFile<>nil) and (PkgFile.FileType in [pftUnit,pftVirtualUnit]) then
      begin
        if PkgFile.AddToUsesPkgSection<>Use then
        begin
          PkgFile.AddToUsesPkgSection:=Use;
          LazPackage.Modified:=true;
        end;
      end;
    end;
    Node:=Node.GetFirstChild;
    while Node<>nil do
    begin
      Traverse(Node);
      Node:=Node.GetNextSibling;
    end;
  end;

var
  CurNode: TTreeNode;
begin
  if not ShowDirectoryHierarchy then exit;
  CurNode:=FilesTreeView.Selected;
  if not (IsDirectoryNode(CurNode) or (CurNode=FFilesNode)) then exit;
  Traverse(CurNode);
  UpdateSelectedFile;
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
  UpdateAll(true);
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

procedure TPackageEditorForm.DoShowMissingFiles;
begin
  ShowMissingPkgFilesDialog(LazPackage);
  UpdateFiles;
end;

constructor TPackageEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPlugins:=TStringList.Create;
  SetupComponents;
end;

destructor TPackageEditorForm.Destroy;
begin
  if PackageEditorMenuRoot.MenuItem=FilesPopupMenu.Items then
    PackageEditorMenuRoot.MenuItem:=nil;
  PackageEditors.DoFreeEditor(LazPackage);
  FreeAndNil(FPlugins);
  inherited Destroy;
end;

{ TPackageEditors }

function TPackageEditors.GetEditors(Index: integer): TPackageEditorForm;
begin
  Result:=TPackageEditorForm(FItems[Index]);
end;

constructor TPackageEditors.Create;
begin
  FItems:=TList.Create;
end;

destructor TPackageEditors.Destroy;
begin
  Clear;
  FItems.Free;
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
    Result:=TPackageEditorForm.Create(LazarusIDE.OwningComponent);
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
  if PackageGraph.OpenDependency(Dependency,false)=lprSuccess then
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

procedure TPackageEditors.UpdateAllEditors(Immediately: boolean);
var
  i: Integer;
begin
  for i:=0 to Count-1 do Editors[i].UpdateAll(Immediately);
end;

function TPackageEditors.ShouldNotBeInstalled(APackage: TLazPackage): boolean;
begin
  Result:=APackage.AutoCreated
     or ((APackage.FindUnitWithRegister=nil) and (APackage.Provides.Count=0));
end;

function TPackageEditors.InstallPackage(APackage: TLazPackage): TModalResult;
begin
  if ShouldNotBeInstalled(APackage) then begin
    if IDEQuestionDialog(lisNotAnInstallPackage,
      Format(lisThePackageDoesNotHaveAnyRegisterProcedureWhichTypi, [APackage.
        Name, #13, #13]),
      mtWarning,
      [mrIgnore, lisInstallItILikeTheFat, mrCancel, dlgCancel], '')<>mrIgnore
    then exit(mrCancel);
  end;
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

function TPackageEditors.ViewPkgSource(APackage: TLazPackage): TModalResult;
begin
  if Assigned(OnViewPackageSource) then
    Result:=OnViewPackageSource(Self,APackage)
  else
    Result:=mrCancel;
end;

function TPackageEditors.ViewPkgToDos(APackage: TLazPackage): TModalResult;
begin
  if Assigned(OnViewPackageToDos) then
    Result:=OnViewPackageToDos(Self,APackage)
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

initialization
  PackageEditors:=nil;

end.

