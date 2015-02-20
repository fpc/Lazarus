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

  Author: Mattias Gaertner

  Abstract:
    TPackageEditorForm is the form of a package editor.
}
unit PackageEditor;

{$mode objfpc}{$H+}

{off $DEFINE VerbosePkgEditDrag}

interface

uses
  // LCL FCL
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, Buttons, Graphics,
  LCLType, LCLProc, Menus, Dialogs, FileUtil, LazFileCache, ExtCtrls,
  contnrs,
  // IDEIntf CodeTools
  CodeToolManager, CodeCache, CodeTree,
  TreeFilterEdit,
  IDEImagesIntf, MenuIntf, LazIDEIntf, ProjectIntf, CodeToolsStructs,
  FormEditingIntf, PackageIntf, IDEHelpIntf, IDEOptionsIntf, SrcEditorIntf,
  IDEMsgIntf, IDEExternToolIntf,
  // IDE
  IDEDialogs, IDEProcs, LazarusIDEStrConsts, IDEDefs, CompilerOptions,
  ComponentReg, UnitResources, EnvironmentOpts, DialogProcs, InputHistory,
  PackageDefs, AddToPackageDlg, PkgVirtualUnitEditor, MissingPkgFilesDlg,
  PackageSystem, CleanPkgDeps;
  
const
  PackageEditorMenuRootName = 'PackageEditor';
  PackageEditorMenuFilesRootName = 'PackageEditorFiles';
  PackageEditorWindowPrefix = 'PackageEditor_';
var
  // selected files
  PkgEditMenuOpenFile: TIDEMenuCommand;
  PkgEditMenuRemoveFile: TIDEMenuCommand;
  PkgEditMenuReAddFile: TIDEMenuCommand;
  PkgEditMenuCopyMoveToDirectory: TIDEMenuCommand;
  PkgEditMenuEditVirtualUnit: TIDEMenuCommand;
  PkgEditMenuSectionFileType: TIDEMenuSection;

  // directories
  PkgEditMenuExpandDirectory: TIDEMenuCommand;
  PkgEditMenuCollapseDirectory: TIDEMenuCommand;
  PkgEditMenuUseAllUnitsInDirectory: TIDEMenuCommand;
  PkgEditMenuUseNoUnitsInDirectory: TIDEMenuCommand;

  // dependencies
  PkgEditMenuOpenPackage: TIDEMenuCommand;
  PkgEditMenuRemoveDependency: TIDEMenuCommand;
  PkgEditMenuReAddDependency: TIDEMenuCommand;
  PkgEditMenuDependencyStoreFileNameAsDefault: TIDEMenuCommand;
  PkgEditMenuDependencyStoreFileNameAsPreferred: TIDEMenuCommand;
  PkgEditMenuDependencyClearStoredFileName: TIDEMenuCommand;
  PkgEditMenuCleanDependencies: TIDEMenuCommand;

  // all files
  PkgEditMenuFindInFiles: TIDEMenuCommand;
  PkgEditMenuSortFiles: TIDEMenuCommand;
  PkgEditMenuFixFilesCase: TIDEMenuCommand;
  PkgEditMenuShowMissingFiles: TIDEMenuCommand;

  // package
  PkgEditMenuSave: TIDEMenuCommand;
  PkgEditMenuSaveAs: TIDEMenuCommand;
  PkgEditMenuRevert: TIDEMenuCommand;
  PkgEditMenuPublish: TIDEMenuCommand;

  // compile
  PkgEditMenuCompile: TIDEMenuCommand;
  PkgEditMenuRecompileClean: TIDEMenuCommand;
  PkgEditMenuRecompileAllRequired: TIDEMenuCommand;
  PkgEditMenuCreateMakefile: TIDEMenuCommand;
  PkgEditMenuCreateFpmakeFile: TIDEMenuCommand;
  PkgEditMenuViewPackageSource: TIDEMenuCommand;

type
  TOnPkgEvent = function(Sender: TObject; APackage: TLazPackage): TModalResult of object;
  TOnAddPkgToProject =
    function(Sender: TObject; APackage: TLazPackage;
             OnlyTestIfPossible: boolean): TModalResult of object;
  TOnCompilePackage =
    function(Sender: TObject; APackage: TLazPackage;
             CompileClean, CompileRequired: boolean): TModalResult of object;
  TOnCreateNewPkgFile =
    function(Sender: TObject; Params: TAddToPkgResult): TModalResult  of object;
  TOnDeleteAmbiguousFiles =
    function(Sender: TObject; APackage: TLazPackage;
             const Filename: string): TModalResult of object;
  TOnFreePkgEditor = procedure(APackage: TLazPackage) of object;
  TOnOpenFile =
    function(Sender: TObject; const Filename: string): TModalResult of object;
  TOnOpenPkgFile =
    function(Sender: TObject; PkgFile: TPkgFile): TModalResult of object;
  TOnSavePackage =
    function(Sender: TObject; APackage: TLazPackage;
             SaveAs: boolean): TModalResult of object;

  TPENodeType = (
    penFile,
    penDependency
    );

  { TPENodeData }

  TPENodeData = class(TTFENodeData)
  public
    Typ: TPENodeType;
    Name: string; // file or package name
    Removed: boolean;
    FileType: TPkgFileType;
    Next: TPENodeData;
    constructor Create(aTyp: TPENodeType; aName: string; aRemoved: boolean);
  end;

  { IFilesEditorInterface
    An editor with a TTreeView with files and dependencies }

  IFilesEditorInterface = interface
    function FilesEditTreeView: TTreeView;
    function TVNodeFiles: TTreeNode;
    function TVNodeRequiredPackages: TTreeNode;
    function FilesEditForm: TCustomForm;
    function FilesOwner: TObject; // TProject or TLazPackage
    function FilesOwnerName: string; // for debugging purposes
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetNodeData(TVNode: TTreeNode): TPENodeData;
    function GetNodeItem(NodeData: TPENodeData): TObject;
    function GetNodeDataItem(TVNode: TTreeNode; out NodeData: TPENodeData;
      out Item: TObject): boolean;
    function GetNodeFilename(Node: TTreeNode): string;
    function IsDirectoryNode(Node: TTreeNode): boolean;
    function FilesBaseDirectory: string;
    function FilesOwnerReadOnly: boolean;
    function FirstRequiredDependency: TPkgDependency;
    function ExtendUnitSearchPath(NewUnitPaths: string): boolean;
    function ExtendIncSearchPath(NewIncPaths: string): boolean;
    procedure UpdateAll(Immediately: boolean = false);
  end;

  TPEFlag = (
    pefNeedUpdateTitle,
    pefNeedUpdateFiles,
    pefNeedUpdateRemovedFiles,
    pefNeedUpdateRequiredPkgs,
    pefNeedUpdateProperties,
    pefNeedUpdateButtons,
    pefNeedUpdateApplyDependencyButton,
    pefNeedUpdateStatusBar
    );
  TPEFlags = set of TPEFlag;

  { TPackageEditorForm }

  TPackageEditorForm = class(TBasePackageEditor,IFilesEditorInterface)
    MoveDownBtn: TSpeedButton;
    MoveUpBtn: TSpeedButton;
    DirectoryHierarchyButton: TSpeedButton;
    OpenButton: TSpeedButton;
    DisableI18NForLFMCheckBox: TCheckBox;
    FilterEdit: TTreeFilterEdit;
    FilterPanel: TPanel;
    SortAlphabeticallyButton: TSpeedButton;
    Splitter1: TSplitter;
    // toolbar
    ToolBar: TToolBar;
    // toolbuttons
    SaveBitBtn: TToolButton;
    CompileBitBtn: TToolButton;
    UseBitBtn: TToolButton;
    AddBitBtn: TToolButton;
    AddMoreBitBtn: TToolButton;
    RemoveBitBtn: TToolButton;
    OptionsBitBtn: TToolButton;
    MoreBitBtn: TToolButton;
    HelpBitBtn: TToolButton;
    // items
    ItemsTreeView: TTreeView;
    // properties
    PropsGroupBox: TGroupBox;
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
    ItemsPopupMenu: TPopupMenu;
    MorePopupMenu: TPopupMenu;
    procedure AddBitBtnClick(Sender: TObject);
    procedure AddMoreBitBtnClick(Sender: TObject);
    procedure AddToProjectClick(Sender: TObject);
    procedure AddToUsesPkgSectionCheckBoxChange(Sender: TObject);
    procedure ApplyDependencyButtonClick(Sender: TObject);
    procedure CallRegisterProcCheckBoxChange(Sender: TObject);
    procedure ChangeFileTypeMenuItemClick(Sender: TObject);
    procedure CleanDependenciesMenuItemClick(Sender: TObject);
    procedure ClearDependencyFilenameMenuItemClick(Sender: TObject);
    procedure CollapseDirectoryMenuItemClick(Sender: TObject);
    procedure CompileAllCleanClick(Sender: TObject);
    procedure CompileBitBtnClick(Sender: TObject);
    procedure CompileCleanClick(Sender: TObject);
    procedure CopyMoveToDirMenuItemClick(Sender: TObject);
    procedure CreateMakefileClick(Sender: TObject);
    procedure CreateFpmakeFileClick(Sender: TObject);
    procedure DirectoryHierarchyButtonClick(Sender: TObject);
    procedure DisableI18NForLFMCheckBoxChange(Sender: TObject);
    procedure EditVirtualUnitMenuItemClick(Sender: TObject);
    procedure ExpandDirectoryMenuItemClick(Sender: TObject);
    procedure FindInFilesMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure ItemsPopupMenuPopup(Sender: TObject);
    procedure ItemsTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure ItemsTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ItemsTreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ItemsTreeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MorePopupMenuPopup(Sender: TObject);
    procedure ItemsTreeViewDblClick(Sender: TObject);
    procedure ItemsTreeViewSelectionChanged(Sender: TObject);
    procedure FixFilesCaseMenuItemClick(Sender: TObject);
    procedure HelpBitBtnClick(Sender: TObject);
    procedure InstallClick(Sender: TObject);
    procedure MaxVersionEditChange(Sender: TObject);
    procedure MinVersionEditChange(Sender: TObject);
    procedure MoveDownBtnClick(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
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
    procedure SortAlphabeticallyButtonClick(Sender: TObject);
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
    FIdleConnected: boolean;
    FLazPackage: TLazPackage;
    FNextSelectedPart: TPENodeData;// select this file/dependency on next update
    FFilesNode: TTreeNode;
    FRequiredPackagesNode: TTreeNode;
    FRemovedFilesNode: TTreeNode;
    FRemovedRequiredNode: TTreeNode;
    FPlugins: TStringList; // ComponentClassName, Objects=TPkgComponent
    FShowDirectoryHierarchy: boolean;
    FSortAlphabetically: boolean;
    FDirSummaryLabel: TLabel;
    FSingleSelectedFile: TPkgFile;
    FSingleSelectedDep: TPkgDependency;
    FFirstNodeData: array[TPENodeType] of TPENodeData;
    fUpdateLock: integer;
    fForcedFlags: TPEFlags;
    function AddOneFile(aFilename: string; var NewUnitPaths, NewIncPaths: String): TModalResult;
    procedure FreeNodeData(Typ: TPENodeType);
    function CreateNodeData(Typ: TPENodeType; aName: string; aRemoved: boolean): TPENodeData;
    procedure SetDependencyDefaultFilename(AsPreferred: boolean);
    procedure SetIdleConnected(AValue: boolean);
    procedure SetShowDirectoryHierarchy(const AValue: boolean);
    procedure SetSortAlphabetically(const AValue: boolean);
    procedure SetupComponents;
    function OnTreeViewGetImageIndex(Str: String; Data: TObject; var AIsEnabled: Boolean): Integer;
    procedure UpdateNodeImage(TVNode: TTreeNode; NodeData: TPENodeData);
    procedure UpdatePending;
    function CanUpdate(Flag: TPEFlag; Immediately: boolean): boolean;
    procedure UpdateTitle(Immediately: boolean = false);
    procedure UpdateFiles(Immediately: boolean = false);
    procedure UpdateRemovedFiles(Immediately: boolean = false);
    procedure UpdateRequiredPkgs(Immediately: boolean = false);
    procedure UpdatePEProperties(Immediately: boolean = false);
    procedure UpdateButtons(Immediately: boolean = false);
    procedure UpdateApplyDependencyButton(Immediately: boolean = false);
    procedure UpdateStatusBar(Immediately: boolean = false);
    procedure GetDirectorySummary(DirNode: TTreeNode;
        out FileCount, HasRegisterProcCount, AddToUsesPkgSectionCount: integer);
    procedure ExtendUnitIncPathForNewUnit(const AnUnitFilename,
      AnIncludeFile: string; var IgnoreUnitPaths: TFilenameToStringTree);
    procedure ExtendIncPathForNewIncludeFile(const AnIncludeFile: string;
      var IgnoreIncPaths: TFilenameToStringTree);
    function CanBeAddedToProject: boolean;
  protected
    fFlags: TPEFlags;
    fLastDlgPage: TAddToPkgType;
    procedure SetLazPackage(const AValue: TLazPackage); override;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoCompile(CompileClean, CompileRequired: boolean);
    procedure DoFindInFiles;
    procedure DoFixFilesCase;
    procedure DoShowMissingFiles;
    procedure DoMoveCurrentFile(Offset: integer);
    procedure DoMoveDependency(Offset: integer);
    procedure DoPublishProject;
    procedure DoEditVirtualUnit;
    procedure DoExpandDirectory;
    procedure DoCollapseDirectory;
    procedure DoUseUnitsInDirectory(Use: boolean);
    procedure DoRevert;
    procedure DoSave(SaveAs: boolean);
    procedure DoSortFiles;
    function DoOpenPkgFile(PkgFile: TPkgFile): TModalResult;
    function ShowAddDialog(var DlgPage: TAddToPkgType): TModalResult;
  public
    // IFilesEditorInterface
    function FilesEditTreeView: TTreeView;
    function FilesEditForm: TCustomForm;
    function FilesOwner: TObject; // = Lazpackage
    function FilesOwnerName: string;
    function TVNodeFiles: TTreeNode;
    function TVNodeRequiredPackages: TTreeNode;
    function FilesBaseDirectory: string;
    function FilesOwnerReadOnly: boolean;
    function FirstRequiredDependency: TPkgDependency;
    function ExtendUnitSearchPath(NewUnitPaths: string): boolean;
    function ExtendIncSearchPath(NewIncPaths: string): boolean;
    function GetNodeData(TVNode: TTreeNode): TPENodeData;
    function GetNodeItem(NodeData: TPENodeData): TObject;
    function GetNodeDataItem(TVNode: TTreeNode; out NodeData: TPENodeData;
      out Item: TObject): boolean;
    function GetNodeFilename(Node: TTreeNode): string;
    function IsDirectoryNode(Node: TTreeNode): boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateAll(Immediately: boolean = false); override;
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
    property SortAlphabetically: boolean read FSortAlphabetically write SetSortAlphabetically;
    property ShowDirectoryHierarchy: boolean read FShowDirectoryHierarchy write SetShowDirectoryHierarchy;
    property FilesNode: TTreeNode read FFilesNode;
    property RequiredPackagesNode: TTreeNode read FRequiredPackagesNode;
  end;
  
  
  { TPackageEditors }
  
  TPackageEditors = class
  private
    FItems: TFPList; // list of TPackageEditorForm
    FOnAddToProject: TOnAddPkgToProject;
    FOnAfterWritePackage: TIDEOptionsWriteEvent;
    FOnBeforeReadPackage: TNotifyEvent;
    FOnCompilePackage: TOnCompilePackage;
    FOnCopyMoveFiles: TNotifyEvent;
    FOnCreateNewFile: TOnCreateNewPkgFile;
    FOnCreateMakefile: TOnPkgEvent;
    FOnCreateFpmakeFile: TOnPkgEvent;
    FOnDeleteAmbiguousFiles: TOnDeleteAmbiguousFiles;
    FOnDragDropTreeView: TDragDropEvent;
    FOnDragOverTreeView: TOnDragOverTreeView;
    FOnShowFindInFiles: TOnPkgEvent;
    FOnFreeEditor: TOnFreePkgEditor;
    FOnGetIDEFileInfo: TGetIDEFileStateEvent;
    FOnGetUnitRegisterInfo: TOnGetUnitRegisterInfo;
    FOnInstallPackage: TOnPkgEvent;
    FOnOpenFile: TOnOpenFile;
    FOnOpenPackage: TOnPkgEvent;
    FOnOpenPkgFile: TOnOpenPkgFile;
    FOnPublishPackage: TOnPkgEvent;
    FOnRevertPackage: TOnPkgEvent;
    FOnSavePackage: TOnSavePackage;
    FOnUninstallPackage: TOnPkgEvent;
    FOnViewPackageSource: TOnPkgEvent;
    FOnViewPackageToDos: TOnPkgEvent;
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
    function FindInFiles(APackage: TLazPackage): TModalResult;
    function DeleteAmbiguousFiles(APackage: TLazPackage;
                                  const Filename: string): TModalResult;
    function AddToProject(APackage: TLazPackage;
                          OnlyTestIfPossible: boolean): TModalResult;
    function CreateMakefile(APackage: TLazPackage): TModalResult;
    function CreateFpmakeFile(APackage: TLazPackage): TModalResult;
    function TreeViewToPkgEditor(TV: TTreeView): TPackageEditorForm;
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
    property OnCopyMoveFiles: TNotifyEvent read FOnCopyMoveFiles
                                           write FOnCopyMoveFiles;
    property OnCreateFpmakeFile: TOnPkgEvent read FOnCreateFpmakeFile
                                                     write FOnCreateFpmakeFile;
    property OnCreateMakeFile: TOnPkgEvent read FOnCreateMakefile
                                                     write FOnCreateMakefile;
    property OnCreateNewFile: TOnCreateNewPkgFile read FOnCreateNewFile
                                                  write FOnCreateNewFile;
    property OnDeleteAmbiguousFiles: TOnDeleteAmbiguousFiles
                     read FOnDeleteAmbiguousFiles write FOnDeleteAmbiguousFiles;
    property OnDragDropTreeView: TDragDropEvent read FOnDragDropTreeView
                                                      write FOnDragDropTreeView;
    property OnDragOverTreeView: TOnDragOverTreeView read FOnDragOverTreeView
                                                      write FOnDragOverTreeView;
    property OnShowFindInFiles: TOnPkgEvent read FOnShowFindInFiles write FOnShowFindInFiles;
    property OnFreeEditor: TOnFreePkgEditor read FOnFreeEditor
                                            write FOnFreeEditor;
    property OnGetIDEFileInfo: TGetIDEFileStateEvent read FOnGetIDEFileInfo
                                                     write FOnGetIDEFileInfo;
    property OnGetUnitRegisterInfo: TOnGetUnitRegisterInfo
                       read FOnGetUnitRegisterInfo write FOnGetUnitRegisterInfo;
    property OnInstallPackage: TOnPkgEvent read FOnInstallPackage
                                                 write FOnInstallPackage;
    property OnOpenFile: TOnOpenFile read FOnOpenFile write FOnOpenFile;
    property OnOpenPackage: TOnPkgEvent read FOnOpenPackage
                                           write FOnOpenPackage;
    property OnOpenPkgFile: TOnOpenPkgFile read FOnOpenPkgFile
                                           write FOnOpenPkgFile;
    property OnPublishPackage: TOnPkgEvent read FOnPublishPackage
                                               write FOnPublishPackage;
    property OnRevertPackage: TOnPkgEvent read FOnRevertPackage
                                               write FOnRevertPackage;
    property OnSavePackage: TOnSavePackage read FOnSavePackage
                                           write FOnSavePackage;
    property OnUninstallPackage: TOnPkgEvent read FOnUninstallPackage
                                                 write FOnUninstallPackage;
    property OnViewPackageSource: TOnPkgEvent read FOnViewPackageSource
                                                 write FOnViewPackageSource;
    property OnViewPackageToDos: TOnPkgEvent read FOnViewPackageToDos
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
  PackageEditorMenuRoot     :=RegisterIDEMenuRoot(PackageEditorMenuRootName);
  PackageEditorMenuFilesRoot:=RegisterIDEMenuRoot(PackageEditorMenuFilesRootName);

  // register the section for operations on selected files
  PkgEditMenuSectionFile:=RegisterIDEMenuSection(PackageEditorMenuFilesRoot,'File');
  AParent:=PkgEditMenuSectionFile;
  PkgEditMenuOpenFile:=RegisterIDEMenuCommand(AParent,'Open File',lisOpenFile);
  PkgEditMenuRemoveFile:=RegisterIDEMenuCommand(AParent,'Remove File',lisPckEditRemoveFile);
  PkgEditMenuReAddFile:=RegisterIDEMenuCommand(AParent,'ReAdd File',lisPckEditReAddFile);
  PkgEditMenuCopyMoveToDirectory:=RegisterIDEMenuCommand(AParent, 'Copy/Move File to Directory', lisCopyMoveFileToDirectory);
  PkgEditMenuEditVirtualUnit:=RegisterIDEMenuCommand(AParent,'Edit Virtual File',lisPEEditVirtualUnit);
  PkgEditMenuSectionFileType:=RegisterIDESubMenu(AParent,'File Type',lisAF2PFileType);

  // register the section for operations on directories
  PkgEditMenuSectionDirectory:=RegisterIDEMenuSection(PackageEditorMenuFilesRoot,'Directory');
  AParent:=PkgEditMenuSectionDirectory;
  PkgEditMenuExpandDirectory:=RegisterIDEMenuCommand(AParent,'Expand directory',lisPEExpandDirectory);
  PkgEditMenuCollapseDirectory:=RegisterIDEMenuCommand(AParent, 'Collapse directory', lisPECollapseDirectory);
  PkgEditMenuUseAllUnitsInDirectory:=RegisterIDEMenuCommand(AParent, 'Use all units in directory', lisPEUseAllUnitsInDirectory);
  PkgEditMenuUseNoUnitsInDirectory:=RegisterIDEMenuCommand(AParent, 'Use no units in directory', lisPEUseNoUnitsInDirectory);

  // register the section for operations on dependencies
  PkgEditMenuSectionDependency:=RegisterIDEMenuSection(PackageEditorMenuFilesRoot,'Dependency');
  AParent:=PkgEditMenuSectionDependency;
  PkgEditMenuOpenPackage:=RegisterIDEMenuCommand(AParent,'Open Package',lisMenuOpenPackage);
  PkgEditMenuRemoveDependency:=RegisterIDEMenuCommand(AParent,'Remove Dependency',lisPckEditRemoveDependency);
  PkgEditMenuReAddDependency:=RegisterIDEMenuCommand(AParent,'ReAdd Dependency',lisPckEditReAddDependency);
  PkgEditMenuDependencyStoreFileNameAsDefault:=RegisterIDEMenuCommand(AParent,'Dependency Store Filename As Default',lisPckEditStoreFileNameAsDefaultForThisDependency);
  PkgEditMenuDependencyStoreFileNameAsPreferred:=RegisterIDEMenuCommand(AParent,'Dependency Store Filename As Preferred',lisPckEditStoreFileNameAsPreferredForThisDependency);
  PkgEditMenuDependencyClearStoredFileName:=RegisterIDEMenuCommand(AParent,'Dependency Clear Stored Filename',lisPckEditClearDefaultPreferredFilenameOfDependency);
  PkgEditMenuCleanDependencies:=RegisterIDEMenuCommand(AParent, 'Clean up dependencies', lisPckEditCleanUpDependencies);

  // register the section for operations on all files
  PkgEditMenuSectionFiles:=RegisterIDEMenuSection(PackageEditorMenuRoot,'Files');
  AParent:=PkgEditMenuSectionFiles;
  PkgEditMenuFindInFiles:=RegisterIDEMenuCommand(AParent,'Find in files',srkmecFindInFiles);
  PkgEditMenuSortFiles:=RegisterIDEMenuCommand(AParent,'Sort Files Permanently',lisPESortFiles);
  PkgEditMenuFixFilesCase:=RegisterIDEMenuCommand(AParent,'Fix Files Case',lisPEFixFilesCase);
  PkgEditMenuShowMissingFiles:=RegisterIDEMenuCommand(AParent, 'Show Missing Files', lisPEShowMissingFiles);

  // register the section for using the package
  PkgEditMenuSectionUse:=RegisterIDEMenuSection(PackageEditorMenuRoot,'Use');

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
  PkgEditMenuCompile:=RegisterIDEMenuCommand(AParent,'Compile',lisCompile);
  PkgEditMenuRecompileClean:=RegisterIDEMenuCommand(AParent,'Recompile Clean',lisPckEditRecompileClean);
  PkgEditMenuRecompileAllRequired:=RegisterIDEMenuCommand(AParent,'Recompile All Required',lisPckEditRecompileAllRequired);
  PkgEditMenuCreateFpmakeFile:=RegisterIDEMenuCommand(AParent,'Create fpmake.pp',lisPckEditCreateFpmakeFile);
  PkgEditMenuCreateMakefile:=RegisterIDEMenuCommand(AParent,'Create Makefile',lisPckEditCreateMakefile);

  // register the section for adding to or removing from package
  PkgEditMenuSectionAddRemove:=RegisterIDEMenuSection(PackageEditorMenuRoot,'AddRemove');

  // register the section for other things
  PkgEditMenuSectionMisc:=RegisterIDEMenuSection(PackageEditorMenuRoot,'Misc');
  AParent:=PkgEditMenuSectionMisc;
  PkgEditMenuViewPackageSource:=RegisterIDEMenuCommand(AParent,'View Package Source',lisPckEditViewPackageSource);
end;

{ TPENodeData }

constructor TPENodeData.Create(aTyp: TPENodeType; aName: string;
  aRemoved: boolean);
begin
  Typ:=aTyp;
  Name:=aName;;
  Removed:=aRemoved;
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
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
begin
  BeginUpdate;
  try
    for i:=ItemsTreeView.SelectionCount-1 downto 0 do begin
      TVNode:=ItemsTreeView.Selections[i];
      if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
      if not NodeData.Removed then continue;
      if Item is TPkgFile then begin
        // re-add file
        PkgFile:=TPkgFile(Item);
        AFilename:=PkgFile.GetFullFilename;
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
      end
      else if Item is TPkgDependency then begin
        Dependency:=TPkgDependency(Item);
        // Re-add dependency
        fForcedFlags:=[pefNeedUpdateRemovedFiles,pefNeedUpdateRequiredPkgs];
        if CheckAddingDependency(LazPackage,Dependency,false,true)<>mrOk then exit;
        LazPackage.RemoveRemovedDependency(Dependency);
        PackageGraph.AddDependencyToPackage(LazPackage,Dependency);
      end;
    end;
    LazPackage.Modified:=True;
  finally
    EndUpdate;
  end;
end;

procedure TPackageEditorForm.ItemsPopupMenuPopup(Sender: TObject);

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
    if FSingleSelectedFile=nil then exit;
    VirtualFileExists:=(FSingleSelectedFile.FileType=pftVirtualUnit)
                    and FileExistsCached(FSingleSelectedFile.GetFullFilename);
    for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
      NewMenuItem:=RegisterIDEMenuCommand(PkgEditMenuSectionFileType,
                      'SetFileType'+IntToStr(ord(CurPFT)),
                      GetPkgFileTypeLocalizedName(CurPFT),
                      @ChangeFileTypeMenuItemClick);
      if CurPFT=FSingleSelectedFile.FileType then begin
        // menuitem to keep the current type
        NewMenuItem.Enabled:=true;
        NewMenuItem.Checked:=true;
      end else if VirtualFileExists then
        // a virtual unit that exists can be changed into anything
        NewMenuItem.Enabled:=true
      else if (not (CurPFT in PkgFileUnitTypes)) then
        // all other files can be changed into all non unit types
        NewMenuItem.Enabled:=true
      else if FilenameIsPascalUnit(FSingleSelectedFile.Filename) then
        // a pascal file can be changed into anything
        NewMenuItem.Enabled:=true
      else
        // default is to not allow
        NewMenuItem.Enabled:=false;
    end;
  end;

var
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
  SingleSelectedRemoved: Boolean;
  SelDepCount: Integer;
  SelFileCount: Integer;
  SelDirCount: Integer;
  SelRemovedFileCount: Integer;
  Writable: Boolean;
  CurDependency: TPkgDependency;
  CurFile: TPkgFile;
begin
  //debugln(['TPackageEditorForm.FilesPopupMenuPopup START ',ItemsPopupMenu.Items.Count]);
  PackageEditorMenuFilesRoot.MenuItem:=ItemsPopupMenu.Items;
  //debugln(['TPackageEditorForm.FilesPopupMenuPopup START after connect ',ItemsPopupMenu.Items.Count]);
  PackageEditorMenuRoot.BeginUpdate;
  try
    SelFileCount:=0;
    SelDepCount:=0;
    SelDirCount:=0;
    SelRemovedFileCount:=0;
    SingleSelectedRemoved:=false;
    for i:=0 to ItemsTreeView.SelectionCount-1 do begin
      TVNode:=ItemsTreeView.Selections[i];
      if GetNodeDataItem(TVNode,NodeData,Item) then begin
        if Item is TPkgFile then begin
          CurFile:=TPkgFile(Item);
          inc(SelFileCount);
          FSingleSelectedFile:=CurFile;
          SingleSelectedRemoved:=NodeData.Removed;
          if NodeData.Removed then
            inc(SelRemovedFileCount);
        end else if Item is TPkgDependency then begin
          CurDependency:=TPkgDependency(Item);
          inc(SelDepCount);
          FSingleSelectedDep:=CurDependency;
          SingleSelectedRemoved:=NodeData.Removed;
        end;
      end else if IsDirectoryNode(TVNode) or (TVNode=FFilesNode) then begin
        inc(SelDirCount);
      end;
    end;

    if (SelFileCount+SelDepCount+SelDirCount>1) then begin
      // it is a multi selection
      FSingleSelectedFile:=nil;
      FSingleSelectedDep:=nil;
    end;

    Writable:=(not LazPackage.ReadOnly);

    PkgEditMenuSectionFileType.Clear;

    // items for selected files, under section PkgEditMenuSectionFile
    PkgEditMenuSectionFile.Visible:=SelFileCount>0;
    if PkgEditMenuSectionFile.Visible then begin
      SetItem(PkgEditMenuOpenFile,@OpenFileMenuItemClick);
      SetItem(PkgEditMenuReAddFile,@ReAddMenuItemClick,SingleSelectedRemoved);
      SetItem(PkgEditMenuCopyMoveToDirectory,@CopyMoveToDirMenuItemClick,(SelRemovedFileCount=0) and LazPackage.HasDirectory);
      SetItem(PkgEditMenuRemoveFile,@RemoveBitBtnClick,SelRemovedFileCount>0,RemoveBitBtn.Enabled);
      AddFileTypeMenuItem;
      SetItem(PkgEditMenuEditVirtualUnit,@EditVirtualUnitMenuItemClick,
              (FSingleSelectedFile<>nil) and (FSingleSelectedFile.FileType=pftVirtualUnit)
              and not SingleSelectedRemoved,Writable);
    end;

    // items for directories, under section PkgEditMenuSectionDirectory
    PkgEditMenuSectionDirectory.Visible:=(SelDirCount>0) and ShowDirectoryHierarchy;
    if PkgEditMenuSectionDirectory.Visible then begin
      SetItem(PkgEditMenuExpandDirectory,@ExpandDirectoryMenuItemClick);
      SetItem(PkgEditMenuCollapseDirectory,@CollapseDirectoryMenuItemClick);
      SetItem(PkgEditMenuUseAllUnitsInDirectory,@UseAllUnitsInDirectoryMenuItemClick);
      SetItem(PkgEditMenuUseNoUnitsInDirectory,@UseNoUnitsInDirectoryMenuItemClick);
    end;

    // items for dependencies, under section PkgEditMenuSectionDependency
    PkgEditMenuSectionDependency.Visible:=(SelDepCount>0)
      or (ItemsTreeView.Selected=FRequiredPackagesNode);
    SetItem(PkgEditMenuOpenPackage,@OpenFileMenuItemClick,
            (FSingleSelectedDep<>nil) and (FSingleSelectedDep.RequiredPackage<>nil));
    SetItem(PkgEditMenuRemoveDependency,@RemoveBitBtnClick,
            (FSingleSelectedDep<>nil) and (not SingleSelectedRemoved),
            Writable);
    SetItem(PkgEditMenuReAddDependency,@ReAddMenuItemClick,
            (FSingleSelectedDep<>nil) and SingleSelectedRemoved,
            Writable);
    SetItem(PkgEditMenuDependencyStoreFileNameAsDefault,
            @SetDependencyDefaultFilenameMenuItemClick,
            (FSingleSelectedDep<>nil) and (not SingleSelectedRemoved),
            Writable and (FSingleSelectedDep<>nil)
            and (FSingleSelectedDep.RequiredPackage<>nil));
    SetItem(PkgEditMenuDependencyStoreFileNameAsPreferred,
            @SetDependencyPreferredFilenameMenuItemClick,
            (FSingleSelectedDep<>nil) and (not SingleSelectedRemoved),
            Writable and (FSingleSelectedDep<>nil)
            and (FSingleSelectedDep.RequiredPackage<>nil));
    SetItem(PkgEditMenuDependencyClearStoredFileName,
            @ClearDependencyFilenameMenuItemClick,
            (FSingleSelectedDep<>nil) and (not SingleSelectedRemoved),
            Writable and (FSingleSelectedDep<>nil)
            and (FSingleSelectedDep.RequiredPackage<>nil));
    SetItem(PkgEditMenuCleanDependencies,
            @CleanDependenciesMenuItemClick,LazPackage.FirstRequiredDependency<>nil,
            Writable);

  finally
    PackageEditorMenuRoot.EndUpdate;
  end;
  //debugln(['TPackageEditorForm.FilesPopupMenuPopup END ',ItemsPopupMenu.Items.Count]); PackageEditorMenuRoot.WriteDebugReport('  ',true);
end;

procedure TPackageEditorForm.ItemsTreeViewAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  NodeData: TPENodeData;
  r: TRect;
  y: Integer;
begin
  if Stage=cdPostPaint then begin
    NodeData:=GetNodeData(Node);
    if (NodeData<>nil) then begin
      if  (NodeData.Typ=penFile) and (not NodeData.Removed)
      and (NodeData.FileType<>pftVirtualUnit) and FilenameIsAbsolute(NodeData.Name)
      and (not FileExistsCached(NodeData.Name))
      then begin
        r:=Node.DisplayRect(true);
        ItemsTreeView.Canvas.Pen.Color:=clRed;
        y:=(r.Top+r.Bottom) div 2;
        ItemsTreeView.Canvas.Line(r.Left,y,r.Right,y);
      end;
    end;
  end;
end;

procedure TPackageEditorForm.ItemsTreeViewDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  PackageEditors.OnDragDropTreeView(Sender, Source, X, Y);
end;

procedure TPackageEditorForm.ItemsTreeViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  TargetTVNode: TTreeNode;
  TargetTVType: TTreeViewInsertMarkType;
begin
  //debugln(['TPackageEditorForm.ItemsTreeViewDragOver ',DbgSName(Source),' State=',ord(State),' FromSelf=',Source=ItemsTreeView]);

  if not PackageEditors.OnDragOverTreeView(Sender, Source, X, Y,
    TargetTVNode, TargetTVType)
  then begin
    ItemsTreeView.SetInsertMark(nil,tvimNone);
    Accept:=false;
    exit;
  end;

  if State=dsDragLeave then
    ItemsTreeView.SetInsertMark(nil,tvimNone)
  else
    ItemsTreeView.SetInsertMark(TargetTVNode,TargetTVType);
  Accept:=true;
end;

procedure TPackageEditorForm.MorePopupMenuPopup(Sender: TObject);
var
  Writable: Boolean;

  procedure SetItem(Item: TIDEMenuCommand; AnOnClick: TNotifyEvent;
                    aShow: boolean = true; AEnable: boolean = true);
  begin
    //debugln(['SetItem ',Item.Caption,' Visible=',aShow,' Enable=',AEnable]);
    Item.OnClick:=AnOnClick;
    Item.Visible:=aShow;
    Item.Enabled:=AEnable;
  end;

begin
  PackageEditorMenuRoot.MenuItem:=MorePopupMenu.Items;
  PackageEditorMenuRoot.BeginUpdate;
  try
    Writable:=(not LazPackage.ReadOnly);

    PkgEditMenuSectionFileType.Clear;

    // under section PkgEditMenuSectionFiles
    SetItem(PkgEditMenuFindInFiles,@FindInFilesMenuItemClick);
    SetItem(PkgEditMenuSortFiles,@SortFilesMenuItemClick,(LazPackage.FileCount>1),Writable);
    SetItem(PkgEditMenuFixFilesCase,@FixFilesCaseMenuItemClick,(LazPackage.FileCount>0),Writable);
    SetItem(PkgEditMenuShowMissingFiles,@ShowMissingFilesMenuItemClick,(LazPackage.FileCount>0),Writable);

    // under section PkgEditMenuSectionSave
    SetItem(PkgEditMenuSave,@SaveBitBtnClick,true,SaveBitBtn.Enabled);
    SetItem(PkgEditMenuSaveAs,@SaveAsClick,true,true);
    SetItem(PkgEditMenuRevert,@RevertClick,true,
            (not LazPackage.IsVirtual) and FileExistsUTF8(LazPackage.Filename));
    SetItem(PkgEditMenuPublish,@PublishClick,true,
            (not LazPackage.Missing) and LazPackage.HasDirectory);

    // under section PkgEditMenuSectionCompile
    SetItem(PkgEditMenuCompile,@CompileBitBtnClick,true,CompileBitBtn.Enabled);
    SetItem(PkgEditMenuRecompileClean,@CompileCleanClick,true,CompileBitBtn.Enabled);
    SetItem(PkgEditMenuRecompileAllRequired,@CompileAllCleanClick,true,CompileBitBtn.Enabled);
    SetItem(PkgEditMenuCreateFpmakeFile,@CreateFpmakeFileClick,true,CompileBitBtn.Enabled);
    SetItem(PkgEditMenuCreateMakefile,@CreateMakefileClick,true,CompileBitBtn.Enabled);

    // under section PkgEditMenuSectionMisc
    SetItem(PkgEditMenuViewPackageSource,@ViewPkgSourceClick);
  finally
    PackageEditorMenuRoot.EndUpdate;
  end;
end;

procedure TPackageEditorForm.SortAlphabeticallyButtonClick(Sender: TObject);
begin
  SortAlphabetically:=SortAlphabeticallyButton.Down;
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
  AddPopupMenuItem(lisPckEditInstall, @InstallClick,(not LazPackage.Missing)
           and (LazPackage.PackageType in [lptDesignTime,lptRunAndDesignTime]));
  AddPopupMenuItem(lisPckEditUninstall, @UninstallClick,
          (LazPackage.Installed<>pitNope) or (LazPackage.AutoInstall<>pitNope));

  // remove unneeded menu items
  while UsePopupMenu.Items.Count>ItemCnt do
    UsePopupMenu.Items.Delete(UsePopupMenu.Items.Count-1);
end;

procedure TPackageEditorForm.ItemsTreeViewDblClick(Sender: TObject);
begin
  OpenFileMenuItemClick(Self);
end;

procedure TPackageEditorForm.ItemsTreeViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Handled: Boolean;
begin
  Handled := True;
  if (ssCtrl in Shift) then
  begin
    if Key = VK_UP then
      MoveUpBtnClick(Nil)
    else if Key = VK_DOWN then
      MoveDownBtnClick(Nil)
    else
      Handled := False;
  end
  else if Key = VK_RETURN then
    OpenFileMenuItemClick(Nil)
  else if Key = VK_DELETE then
    RemoveBitBtnClick(Nil)
  else if Key = VK_INSERT then
    AddBitBtnClick(Nil)
  else
    Handled := False;

  if Handled then
    Key := VK_UNKNOWN;
end;

procedure TPackageEditorForm.ItemsTreeViewSelectionChanged(Sender: TObject);
begin
  if fUpdateLock>0 then exit;
  UpdatePEProperties;
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

procedure TPackageEditorForm.SetDependencyDefaultFilenameMenuItemClick(Sender: TObject);
begin
  SetDependencyDefaultFilename(false);
end;

procedure TPackageEditorForm.SetDependencyPreferredFilenameMenuItemClick(Sender: TObject);
begin
  SetDependencyDefaultFilename(true);
end;

procedure TPackageEditorForm.ClearDependencyFilenameMenuItemClick(Sender: TObject);
begin
  if LazPackage=nil then exit;
  if FSingleSelectedDep=nil then exit;
  if LazPackage.ReadOnly then exit;
  if FSingleSelectedDep.RequiredPackage=nil then exit;
  if FSingleSelectedDep.DefaultFilename='' then exit;
  FSingleSelectedDep.DefaultFilename:='';
  FSingleSelectedDep.PreferDefaultFilename:=false;
  LazPackage.Modified:=true;
  UpdateRequiredPkgs;
end;

procedure TPackageEditorForm.CollapseDirectoryMenuItemClick(Sender: TObject);
begin
  DoCollapseDirectory;
end;

procedure TPackageEditorForm.MoveUpBtnClick(Sender: TObject);
begin
  if SortAlphabetically then exit;
  if Assigned(FSingleSelectedFile) then
    DoMoveCurrentFile(-1)
  else if Assigned(FSingleSelectedDep) then
    DoMoveDependency(-1)
end;

procedure TPackageEditorForm.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if fUpdateLock>0 then exit;
  IdleConnected:=false;
  UpdatePending;
end;

procedure TPackageEditorForm.MoveDownBtnClick(Sender: TObject);
begin
  if SortAlphabetically then exit;
  if Assigned(FSingleSelectedFile) then
    DoMoveCurrentFile(1)
  else if Assigned(FSingleSelectedDep) then
    DoMoveDependency(1)
end;

procedure TPackageEditorForm.OpenFileMenuItemClick(Sender: TObject);
var
  CurFile: TPkgFile;
  CurDependency: TPkgDependency;
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
begin
  for i:=0 to ItemsTreeView.SelectionCount-1 do begin
    TVNode:=ItemsTreeView.Selections[i];
    if GetNodeDataItem(TVNode,NodeData,Item) then begin
      if Item is TPkgFile then begin
        CurFile:=TPkgFile(Item);
        if DoOpenPkgFile(CurFile)<>mrOk then exit;
      end else if Item is TPkgDependency then begin
        CurDependency:=TPkgDependency(Item);
        if PackageEditors.OpenDependency(Self,CurDependency)<>mrOk then exit;
      end;
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
  Package1.IDEOptions.OnBeforeRead:=PackageEditors.OnBeforeReadPackage;
  Package1.IDEOptions.OnAfterWrite:=PackageEditors.OnAfterWritePackage;
  LazarusIDE.DoOpenIDEOptions(nil,
    Format(lisPckEditCompilerOptionsForPackage, [LazPackage.IDAsString]),
    [TPackageIDEOptions, TPkgCompilerOptions], Settings[LazPackage.ReadOnly]);
  UpdateTitle;
  UpdateButtons;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.PackageEditorFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  //debugln(['TPackageEditorForm.PackageEditorFormClose ',Caption]);
  if LazPackage=nil then exit;
end;

procedure TPackageEditorForm.PackageEditorFormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  MsgResult: Integer;
begin
  //debugln(['TPackageEditorForm.PackageEditorFormCloseQuery ',Caption]);
  if (LazPackage<>nil) and (not (lpfDestroying in LazPackage.Flags))
  and (not LazPackage.ReadOnly) and LazPackage.Modified then begin

    MsgResult:=MessageDlg(lisPkgMangSavePackage,
      Format(lisPckEditPackageHasChangedSavePackage, [LazPackage.IDAsString, LineEnding]),
      mtConfirmation,[mbYes,mbNo,mbAbort],0);
    case MsgResult of
      mrYes:
        MsgResult:=PackageEditors.SavePackage(LazPackage,false);
      mrNo:
        LazPackage.UserIgnoreChangeStamp:=LazPackage.ChangeStamp;
    end;
    if MsgResult=mrAbort then CanClose:=false;
    LazPackage.Modified:=false; // clear modified flag, so that it will be closed
  end;
  //debugln(['TPackageEditorForm.PackageEditorFormCloseQuery CanClose=',CanClose,' ',Caption]);
  if CanClose then
    Application.ReleaseComponent(Self);
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
      if Assigned(CurComponent.RealPage) then
        CurStr:=Format(lisPckEditPage,[CurComponent.ComponentClass.ClassName,
                                       CurComponent.RealPage.PageName])
      else
        CurStr:=CurComponent.ComponentClass.ClassName;
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
  MainUnitSelected: Boolean;
  FileWarning, PkgWarning: String;
  FileCount, PkgCount: Integer;

  procedure CheckFileSelection(CurFile: TPkgFile);
  begin
    inc(FileCount);
    if CurFile.FileType=pftMainUnit then
      MainUnitSelected:=true;
    if FileWarning='' then
      FileWarning:=Format(lisPckEditRemoveFileFromPackage,
                          [CurFile.Filename, LineEnding, LazPackage.IDAsString]);
  end;

  procedure CheckPkgSelection(CurDependency: TPkgDependency);
  begin
    inc(PkgCount);
    if PkgWarning='' then
      PkgWarning:=Format(lisPckEditRemoveDependencyFromPackage,
                    [CurDependency.AsString, LineEnding, LazPackage.IDAsString]);
  end;

  function ConfirmFileDeletion: TModalResult;
  var
    mt: TMsgDlgType;
    s: String;
  begin
    mt:=mtConfirmation;
    if FileCount=1 then
      s:=FileWarning
    else
      s:=Format(lisRemoveFilesFromPackage, [IntToStr(FileCount), LazPackage.Name]);
    if MainUnitSelected then begin
      s+=Format(lisWarningThisIsTheMainUnitTheNewMainUnitWillBePas,
                [LineEnding+LineEnding, lowercase(LazPackage.Name)]);
      mt:=mtWarning;
    end;
    Result:=IDEMessageDialog(lisPckEditRemoveFile2, s, mt, [mbYes,mbNo]);
  end;

  function ConfirmPkgDeletion: TModalResult;
  var
    mt: TMsgDlgType;
    s: String;
  begin
    mt:=mtConfirmation;
    if PkgCount=1 then
      s:=PkgWarning
    else
      s:=Format(lisRemoveDependenciesFromPackage, [IntToStr(PkgCount), LazPackage.Name]);
    Result:=IDEMessageDialog(lisRemove2, s, mt, [mbYes, mbNo]);
  end;

var
  ANode: TTreeNode;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
  FilesBranch: TTreeFilterBranch;
  i: Integer;
begin
  BeginUpdate;
  try
    ANode:=ItemsTreeView.Selected;
    if (ANode=nil) or LazPackage.ReadOnly then begin
      UpdateButtons;
      exit;
    end;

    MainUnitSelected:=false;
    FileCount:=0;
    FileWarning:='';
    PkgCount:=0;
    PkgWarning:='';

    // check selection
    for i:=0 to ItemsTreeView.SelectionCount-1 do begin
      TVNode:=ItemsTreeView.Selections[i];
      if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
      if NodeData.Removed then continue;
      if Item is TPkgFile then
        CheckFileSelection(TPkgFile(Item))
      else if Item is TPkgDependency then
        CheckPkgSelection(TPkgDependency(Item));
    end;
    if (FileCount=0) and (PkgCount=0) then begin
      UpdateButtons;
      exit;
    end;

    // confirm deletion
    if FileCount>0 then begin
      if ConfirmFileDeletion<>mrYes then Exit;
      FilesBranch:=FilterEdit.GetExistingBranch(FFilesNode);
    end;
    if (PkgCount>0) and (ConfirmPkgDeletion<>mrYes) then Exit;

    // remove
    for i:=ItemsTreeView.SelectionCount-1 downto 0 do begin
      TVNode:=ItemsTreeView.Selections[i];
      if not GetNodeDataItem(TVNode, NodeData, Item) then continue;
      if NodeData.Removed then continue;
      if Item is TPkgFile then begin
        FilesBranch.DeleteData(TVNode);
        LazPackage.RemoveFileSilently(TPkgFile(Item));
      end
      else if Item is TPkgDependency then
        LazPackage.RemoveRequiredDepSilently(TPkgDependency(Item));
    end;
    if FileCount>0 then        // Force update for removed files only.
      fForcedFlags:=fForcedFlags+[pefNeedUpdateRemovedFiles];
    if PkgCount>0 then
      fForcedFlags:=fForcedFlags+[pefNeedUpdateRemovedFiles,pefNeedUpdateRequiredPkgs];
    LazPackage.Modified:=True; // This will update also other possible editors.

  finally
    EndUpdate;
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

procedure TPackageEditorForm.FindInFilesMenuItemClick(Sender: TObject);
begin
  DoFindInFiles;
end;

procedure TPackageEditorForm.FormCreate(Sender: TObject);
begin
  FPlugins:=TStringList.Create;
  SetupComponents;
  SortAlphabetically := EnvironmentOptions.PackageEditorSortAlphabetically;
  ShowDirectoryHierarchy := EnvironmentOptions.PackageEditorShowDirHierarchy;
end;

procedure TPackageEditorForm.FormDestroy(Sender: TObject);
var
  nt: TPENodeType;
begin
  IdleConnected:=true;
  FreeAndNil(FNextSelectedPart);
  EnvironmentOptions.PackageEditorSortAlphabetically := SortAlphabetically;
  EnvironmentOptions.PackageEditorShowDirHierarchy := ShowDirectoryHierarchy;
  FilterEdit.ForceFilter('');
  for nt:=Low(TPENodeType) to High(TPENodeType) do
    FreeNodeData(nt);
  if PackageEditorMenuRoot.MenuItem=ItemsPopupMenu.Items then
    PackageEditorMenuRoot.MenuItem:=nil;
  PackageEditors.DoFreeEditor(LazPackage);
  FLazPackage:=nil;
  FreeAndNil(FPlugins);
end;

procedure TPackageEditorForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  i: Integer;
  NewUnitPaths, NewIncPaths: String;
begin
  {$IFDEF VerbosePkgEditDrag}
  debugln(['TPackageEditorForm.FormDropFiles ',length(FileNames)]);
  {$ENDIF}
  if length(FileNames)=0 then exit;
  BeginUpdate;
  try
    NewUnitPaths:='';
    NewIncPaths:='';
    for i:=0 to high(Filenames) do
      if not (AddOneFile(FileNames[i], NewUnitPaths, NewIncPaths) in [mrOk, mrIgnore]) then break;
    //UpdateAll(false);
    // extend unit and include search path
    if not ExtendUnitSearchPath(NewUnitPaths) then exit;
    if not ExtendIncSearchPath(NewIncPaths) then exit;
  finally
    EndUpdate;
  end;
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

procedure TPackageEditorForm.FreeNodeData(Typ: TPENodeType);
var
  NodeData: TPENodeData;
  n: TPENodeData;
begin
  NodeData:=FFirstNodeData[Typ];
  while NodeData<>nil do begin
    n:=NodeData;
    NodeData:=NodeData.Next;
    if Assigned(n.Branch) Then
      n.Branch.FreeNodeData(n.Node);
    n.Free;
  end;
  FFirstNodeData[Typ]:=nil;
end;

function TPackageEditorForm.CreateNodeData(Typ: TPENodeType; aName: string;
  aRemoved: boolean): TPENodeData;
begin
  Result:=TPENodeData.Create(Typ,aName,aRemoved);
  Result.Next:=FFirstNodeData[Typ];
  FFirstNodeData[Typ]:=Result;
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

function TPackageEditorForm.AddOneFile(aFilename: string;
  var NewUnitPaths, NewIncPaths: String): TModalResult;
var
  NewFileType: TPkgFileType;
  NewUnitName: String;
  HasRegister: Boolean;
  NewFlags: TPkgFileFlags;
  Code: TCodeBuffer;
  CurDir: String;
begin
  Result := mrOK;
  aFilename:=CleanAndExpandFilename(aFileName);
  if not FileExistsUTF8(aFilename) then Exit(mrIgnore);
  if DirPathExists(aFilename) then Exit(mrIgnore);
  if LazPackage.FindPkgFile(aFilename,true,false)<>nil then Exit(mrIgnore);
  NewFileType:=FileNameToPkgFileType(aFilename);
  NewFlags:=[];
  HasRegister:=false;
  NewUnitName:='';
  if (NewFileType=pftUnit) then begin
    Code:=CodeToolBoss.LoadFile(aFilename,true,false);
    NewUnitName:=CodeToolBoss.GetSourceName(Code,false);
    if NewUnitName='' then
      NewUnitName:=ExtractFileNameOnly(aFilename);
    if LazPackage.FindUsedUnit(NewUnitName)=nil then
      Include(NewFlags,pffAddToPkgUsesSection);
    CodeToolBoss.HasInterfaceRegisterProc(Code,HasRegister);
    if HasRegister then
      Include(NewFlags,pffHasRegisterProc);
  end;
  {$IFDEF VerbosePkgEditDrag}
  debugln(['TPackageEditorForm.FormDropFiles Adding files: ',aFilename,' Unit=',NewUnitName,' Type=',PkgFileTypeIdents[NewFileType],' use=',pffAddToPkgUsesSection in NewFlags,' hasregister=',pffHasRegisterProc in NewFlags]);
  {$ENDIF}
  LazPackage.AddFile(aFilename,NewUnitName,NewFileType,NewFlags,cpNormal);
  CurDir:=ChompPathDelim(ExtractFilePath(aFilename));
  if NewFileType=pftUnit then
    NewUnitPaths:=MergeSearchPaths(NewUnitPaths,CurDir)
  else
    NewIncPaths:=MergeSearchPaths(NewIncPaths,CurDir);
end;

procedure TPackageEditorForm.AddBitBtnClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  i: Integer;
  NewUnitPaths, NewIncPaths: String;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.InitialDir:=LazPackage.GetFileDialogInitialDir(OpenDialog.InitialDir);
    OpenDialog.Title:=lisOpenFile;
    OpenDialog.Options:=OpenDialog.Options
                          +[ofFileMustExist,ofPathMustExist,ofAllowMultiSelect];
    OpenDialog.Filter:=dlgAllFiles+' ('+GetAllFilesMask+')|'+GetAllFilesMask
                 +'|'+lisLazarusUnit+' (*.pas;*.pp)|*.pas;*.pp'
                 +'|'+lisLazarusProject+' (*.lpi)|*.lpi'
                 +'|'+lisLazarusForm+' (*.lfm;*.dfm)|*.lfm;*.dfm'
                 +'|'+lisLazarusPackage+' (*.lpk)|*.lpk'
                 +'|'+lisLazarusProjectSource+' (*.lpr)|*.lpr';
    if OpenDialog.Execute then begin
      NewUnitPaths:='';
      NewIncPaths:='';
      for i:=0 to OpenDialog.Files.Count-1 do
        if not (AddOneFile(OpenDialog.Files[i], NewUnitPaths, NewIncPaths) in [mrOk, mrIgnore]) then break;
      InputHistories.StoreFileDialogSettings(OpenDialog);
      //UpdateAll(false);
      // extend unit and include search path
      if not ExtendUnitSearchPath(NewUnitPaths) then exit;
      if not ExtendIncSearchPath(NewIncPaths) then exit;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TPackageEditorForm.AddMoreBitBtnClick(Sender: TObject);
begin
  if LazPackage=nil then exit;
  BeginUpdate;
  try
    ShowAddDialog(fLastDlgPage);
  finally
    EndUpdate;
  end;
end;

procedure TPackageEditorForm.AddToUsesPkgSectionCheckBoxChange(Sender: TObject);
var
  CurFile: TPkgFile;
  OtherFile: TPkgFile;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
  i, j: Integer;
begin
  if LazPackage=nil then exit;
  for i:=0 to ItemsTreeView.SelectionCount-1 do begin
    TVNode:=ItemsTreeView.Selections[i];
    if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
    if not (Item is TPkgFile) then continue;
    CurFile:=TPkgFile(Item);
    if not (CurFile.FileType in PkgFileUnitTypes) then continue;
    if CurFile.AddToUsesPkgSection=AddToUsesPkgSectionCheckBox.Checked then continue;
    // change flag
    CurFile.AddToUsesPkgSection:=AddToUsesPkgSectionCheckBox.Checked;
    if (not NodeData.Removed) and CurFile.AddToUsesPkgSection then begin
      // mark all other units with the same name as unused
      for j:=0 to LazPackage.FileCount-1 do begin
        OtherFile:=LazPackage.Files[j];
        if (OtherFile<>CurFile)
        and (CompareText(OtherFile.Unit_Name,CurFile.Unit_Name)=0) then
          OtherFile.AddToUsesPkgSection:=false;
      end;
    end;
    LazPackage.ModifySilently;
  end;
end;

procedure TPackageEditorForm.AddToProjectClick(Sender: TObject);
begin
  if LazPackage=nil then exit;
  PackageEditors.AddToProject(LazPackage,false);
end;

procedure TPackageEditorForm.ApplyDependencyButtonClick(Sender: TObject);
var
  Flags: TPkgDependencyFlags;
  MinVers, MaxVers: TPkgVersion;
begin
  if LazPackage=nil then exit;
  if FSingleSelectedDep=nil then exit;
  if LazPackage.FindDependencyByName(FSingleSelectedDep.PackageName)<>FSingleSelectedDep
  then exit;

  MinVers:=TPkgVersion.Create;
  MaxVers:=TPkgVersion.Create;
  try
    // Assign relevant data to temp variables
    Flags:=FSingleSelectedDep.Flags;
    MinVers.Assign(FSingleSelectedDep.MinVersion);
    MaxVers.Assign(FSingleSelectedDep.MinVersion);

    // read minimum version
    if UseMinVersionCheckBox.Checked then begin
      Include(Flags, pdfMinVersion);
      if not MinVers.ReadString(MinVersionEdit.Text) then begin
        MessageDlg(lisPckEditInvalidMinimumVersion,
          Format(lisPckEditTheMinimumVersionIsNotAValidPackageVersion,
                 [MinVersionEdit.Text, LineEnding]),
          mtError,[mbCancel],0);
        exit;
      end;
    end
    else
      Exclude(Flags, pdfMinVersion);

    // read maximum version
    if UseMaxVersionCheckBox.Checked then begin
      Include(Flags, pdfMaxVersion);
      if not MaxVers.ReadString(MaxVersionEdit.Text) then begin
        MessageDlg(lisPckEditInvalidMaximumVersion,
          Format(lisPckEditTheMaximumVersionIsNotAValidPackageVersion,
                 [MaxVersionEdit.Text, LineEnding]),
          mtError,[mbCancel],0);
        exit;
      end;
    end
    else
      Exclude(Flags, pdfMaxVersion);

    // Assign changes back to the dependency
    FSingleSelectedDep.Flags := Flags;
    FSingleSelectedDep.MinVersion.Assign(MinVers);
    FSingleSelectedDep.MaxVersion.Assign(MaxVers);
    LazPackage.ModifySilently;
  finally
    MaxVers.Free;
    MinVers.Free;
  end;
end;

procedure TPackageEditorForm.CallRegisterProcCheckBoxChange(Sender: TObject);
var
  CurFile: TPkgFile;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
  i: Integer;
begin
  if LazPackage=nil then exit;
  for i:=0 to ItemsTreeView.SelectionCount-1 do begin
    TVNode:=ItemsTreeView.Selections[i];
    if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
    if not (Item is TPkgFile) then continue;
    CurFile:=TPkgFile(Item);
    if not (CurFile.FileType in PkgFileUnitTypes) then continue;
    if CurFile.HasRegisterProc=CallRegisterProcCheckBox.Checked then continue;
    CurFile.HasRegisterProc:=CallRegisterProcCheckBox.Checked;
    if not NodeData.Removed then
      LazPackage.ModifySilently;
    UpdateNodeImage(TVNode, NodeData);
  end;
end;

procedure TPackageEditorForm.ChangeFileTypeMenuItemClick(Sender: TObject);
var
  CurPFT: TPkgFileType;
  CurFile: TPkgFile;
  CurItem: TIDEMenuCommand;
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
begin
  if LazPackage=nil then exit;
  CurItem:=TIDEMenuCommand(Sender);
  for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
    if CurItem.Caption=GetPkgFileTypeLocalizedName(CurPFT) then begin
      for i:=0 to ItemsTreeView.SelectionCount-1 do begin
        TVNode:=ItemsTreeView.Selections[i];
        if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
        if not (Item is TPkgFile) then continue;
        CurFile:=TPkgFile(Item);
        if CurFile.FileType=CurPFT then continue;
        if (not FilenameIsPascalUnit(CurFile.Filename))
        and (CurPFT in PkgFileUnitTypes) then
          continue;
        CurFile.FileType:=CurPFT;
        if not NodeData.Removed then
          LazPackage.ModifySilently;
      end;
      exit;
    end;
  end;
end;

procedure TPackageEditorForm.CleanDependenciesMenuItemClick(Sender: TObject);
var
  ListOfNodeInfos: TObjectList;
  i: Integer;
  Info: TCPDNodeInfo;
  Dependency: TPkgDependency;
begin
  if LazPackage=nil then exit;
  BeginUpdate;
  ListOfNodeInfos:=nil;
  try
    if ShowCleanPkgDepDlg(LazPackage,ListOfNodeInfos)<>mrOk then exit;
    for i:=0 to ListOfNodeInfos.Count-1 do begin
      Info:=TCPDNodeInfo(ListOfNodeInfos[i]);
      Dependency:=LazPackage.FindDependencyByName(Info.Dependency);
      if Dependency<>nil then begin
        fForcedFlags:=[pefNeedUpdateRemovedFiles,pefNeedUpdateRequiredPkgs];
        PackageGraph.RemoveDependencyFromPackage(LazPackage,Dependency,true);
      end;
    end;
  finally
    ListOfNodeInfos.Free;
    EndUpdate;
  end;
end;

procedure TPackageEditorForm.CompileAllCleanClick(Sender: TObject);
begin
  if LazPackage=nil then exit;
  if MessageDlg(lisPckEditCompileEverything,
    lisPckEditReCompileThisAndAllRequiredPackages,
    mtConfirmation,[mbYes,mbNo],0)<>mrYes then exit;
  DoCompile(true,true);
end;

procedure TPackageEditorForm.CompileCleanClick(Sender: TObject);
begin
  DoCompile(true,false);
end;

procedure TPackageEditorForm.CopyMoveToDirMenuItemClick(Sender: TObject);
begin
  PackageEditors.OnCopyMoveFiles(Self);
end;

procedure TPackageEditorForm.CompileBitBtnClick(Sender: TObject);
begin
  DoCompile(false,false);
end;

procedure TPackageEditorForm.CreateMakefileClick(Sender: TObject);
begin
  PackageEditors.CreateMakefile(LazPackage);
end;

procedure TPackageEditorForm.CreateFpmakeFileClick(Sender: TObject);
begin
  PackageEditors.CreateFpmakeFile(LazPackage);
end;

procedure TPackageEditorForm.DirectoryHierarchyButtonClick(Sender: TObject);
begin
  ShowDirectoryHierarchy:=DirectoryHierarchyButton.Down;
end;

procedure TPackageEditorForm.DisableI18NForLFMCheckBoxChange(Sender: TObject);
var
  CurFile: TPkgFile;
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
begin
  if LazPackage=nil then exit;
  BeginUpdate;
  try
    for i:=0 to ItemsTreeView.SelectionCount-1 do begin
      TVNode:=ItemsTreeView.Selections[i];
      if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
      if not (Item is TPkgFile) then continue;
      CurFile:=TPkgFile(Item);
      if not (CurFile.FileType in PkgFileUnitTypes) then continue;
      if CurFile.DisableI18NForLFM=DisableI18NForLFMCheckBox.Checked then
        continue;
      CurFile.DisableI18NForLFM:=DisableI18NForLFMCheckBox.Checked;
      if not NodeData.Removed then
        LazPackage.Modified:=true;
    end;
  finally
    EndUpdate;
  end;
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
  ImageIndexFiles           := IDEImages.LoadImage(16, 'pkg_files');
  ImageIndexRemovedFiles    := IDEImages.LoadImage(16, 'pkg_removedfiles');
  ImageIndexRequired        := IDEImages.LoadImage(16, 'pkg_required');
  ImageIndexRemovedRequired := IDEImages.LoadImage(16, 'pkg_removedrequired');
  ImageIndexUnit            := IDEImages.LoadImage(16, 'pkg_unit');
  ImageIndexRegisterUnit    := IDEImages.LoadImage(16, 'pkg_registerunit');
  ImageIndexLFM             := IDEImages.LoadImage(16, 'pkg_lfm');
  ImageIndexLRS             := IDEImages.LoadImage(16, 'pkg_lrs');
  ImageIndexInclude         := IDEImages.LoadImage(16, 'pkg_include');
  ImageIndexIssues          := IDEImages.LoadImage(16, 'pkg_issues');
  ImageIndexText            := IDEImages.LoadImage(16, 'pkg_text');
  ImageIndexBinary          := IDEImages.LoadImage(16, 'pkg_binary');
  ImageIndexConflict        := IDEImages.LoadImage(16, 'pkg_conflict');
  ImageIndexDirectory       := IDEImages.LoadImage(16, 'pkg_files');

  ItemsTreeView.Images := IDEImages.Images_16;
  ToolBar.Images := IDEImages.Images_16;
  FilterEdit.OnGetImageIndex:=@OnTreeViewGetImageIndex;

  SaveBitBtn    := CreateToolButton('SaveBitBtn', lisMenuSave, lisPckEditSavePackage, 'laz_save', @SaveBitBtnClick);
  CompileBitBtn := CreateToolButton('CompileBitBtn', lisCompile, lisPckEditCompilePackage, 'pkg_compile', @CompileBitBtnClick);
  UseBitBtn     := CreateToolButton('UseBitBtn', lisPckEditInstall, lisPckEditInstallPackageInTheIDE, 'pkg_install', nil);
  CreateDivider;
  AddBitBtn     := CreateToolButton('AddBitBtn', lisAdd, lisPckEditAddFiles, 'laz_add', @AddBitBtnClick);
  AddMoreBitBtn := CreateToolButton('AddMoreBitBtn', lisDlgAdd, lisPckEditAddOtherItems, 'laz_addmore', @AddMoreBitBtnClick);
  RemoveBitBtn  := CreateToolButton('RemoveBitBtn', lisRemove, lisPckEditRemoveSelectedItem, 'laz_delete', @RemoveBitBtnClick);
  CreateDivider;
  OptionsBitBtn := CreateToolButton('OptionsBitBtn', dlgFROpts, lisPckEditEditGeneralOptions, 'pkg_properties', @OptionsBitBtnClick);
  HelpBitBtn    := CreateToolButton('HelpBitBtn', GetButtonCaption(idButtonHelp), lisPkgEdThereAreMoreFunctionsInThePopupmenu, 'menu_help', @HelpBitBtnClick);
  MoreBitBtn    := CreateToolButton('MoreBitBtn', lisMoreSub, lisPkgEdThereAreMoreFunctionsInThePopupmenu, '', nil);

  MoreBitBtn.DropdownMenu := MorePopupMenu;

  // Buttons on FilterPanel
  OpenButton.LoadGlyphFromResourceName(HInstance, 'laz_open');
  OpenButton.Hint:=lisOpenFile2;
  SortAlphabeticallyButton.Hint:=lisPESortFilesAlphabetically;
  SortAlphabeticallyButton.LoadGlyphFromResourceName(HInstance, 'pkg_sortalphabetically');
  DirectoryHierarchyButton.Hint:=lisPEShowDirectoryHierarchy;
  DirectoryHierarchyButton.LoadGlyphFromResourceName(HInstance, 'pkg_hierarchical');

  // Up / Down buttons
  MoveUpBtn.LoadGlyphFromResourceName(HInstance, 'arrow_up');
  MoveDownBtn.LoadGlyphFromResourceName(HInstance, 'arrow_down');
  MoveUpBtn.Hint:=lisMoveSelectedUp;
  MoveDownBtn.Hint:=lisMoveSelectedDown;

  ItemsTreeView.BeginUpdate;
  FFilesNode:=ItemsTreeView.Items.Add(nil, dlgEnvFiles);
  FFilesNode.ImageIndex:=ImageIndexFiles;
  FFilesNode.SelectedIndex:=FFilesNode.ImageIndex;
  FRequiredPackagesNode:=ItemsTreeView.Items.Add(nil, lisPckEditRequiredPackages);
  FRequiredPackagesNode.ImageIndex:=ImageIndexRequired;
  FRequiredPackagesNode.SelectedIndex:=FRequiredPackagesNode.ImageIndex;
  ItemsTreeView.EndUpdate;

  PropsGroupBox.Caption:=lisPckEditFileProperties;

  CallRegisterProcCheckBox.Caption:=lisPckEditRegisterUnit;
  CallRegisterProcCheckBox.Hint:=Format(lisPckEditCallRegisterProcedureOfSelectedUnit, ['"', '"']);

  AddToUsesPkgSectionCheckBox.Caption:=lisPkgMangUseUnit;
  AddToUsesPkgSectionCheckBox.Hint:=lisPkgMangAddUnitToUsesClauseOfPackageDisableThisOnlyForUnit;

  DisableI18NForLFMCheckBox.Caption:=lisPckDisableI18NOfLfm;
  DisableI18NForLFMCheckBox.Hint:=lisPckWhenTheFormIsSavedTheIDECanStoreAllTTranslateString;

  UseMinVersionCheckBox.Caption:=lisPckEditMinimumVersion;
  UseMaxVersionCheckBox.Caption:=lisPckEditMaximumVersion;
  ApplyDependencyButton.Caption:=lisPckEditApplyChanges;
  RegisteredPluginsGroupBox.Caption:=lisPckEditRegisteredPlugins;
  RegisteredListBox.ItemHeight:=ComponentPaletteImageHeight;

  FDirSummaryLabel:=TLabel.Create(Self);
  with FDirSummaryLabel do
  begin
    Name:='DirSummaryLabel';
    Parent:=PropsGroupBox;
  end;
end;

procedure TPackageEditorForm.SetDependencyDefaultFilename(AsPreferred: boolean);
var
  NewFilename: String;
begin
  if LazPackage=nil then exit;
  if FSingleSelectedDep=nil then exit;
  if LazPackage.FindDependencyByName(FSingleSelectedDep.PackageName)<>FSingleSelectedDep
  then exit;
  if LazPackage.ReadOnly then exit;
  if FSingleSelectedDep.RequiredPackage=nil then exit;
  NewFilename:=FSingleSelectedDep.RequiredPackage.Filename;
  if (NewFilename=FSingleSelectedDep.DefaultFilename)
  and (FSingleSelectedDep.PreferDefaultFilename=AsPreferred) then
    exit;
  BeginUpdate;
  try
    FSingleSelectedDep.DefaultFilename:=NewFilename;
    FSingleSelectedDep.PreferDefaultFilename:=AsPreferred;
    LazPackage.Modified:=true;
    UpdateRequiredPkgs;
  finally
    EndUpdate;
  end;
end;

procedure TPackageEditorForm.SetIdleConnected(AValue: boolean);
begin
  if csDestroying in ComponentState then
    AValue:=false;
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TPackageEditorForm.SetShowDirectoryHierarchy(const AValue: boolean);
begin
  //debugln(['TPackageEditorForm.SetShowDirectoryHierachy Old=',FShowDirectoryHierarchy,' New=',AValue]);
  if FShowDirectoryHierarchy=AValue then exit;
  FShowDirectoryHierarchy:=AValue;
  DirectoryHierarchyButton.Down:=FShowDirectoryHierarchy;
  FilterEdit.ShowDirHierarchy:=FShowDirectoryHierarchy;
  FilterEdit.InvalidateFilter;
end;

procedure TPackageEditorForm.SetSortAlphabetically(const AValue: boolean);
begin
  if FSortAlphabetically=AValue then exit;
  FSortAlphabetically:=AValue;
  SortAlphabeticallyButton.Down:=FSortAlphabetically;
  FilterEdit.SortData:=FSortAlphabetically;
  FilterEdit.InvalidateFilter;
end;

procedure TPackageEditorForm.UpdateAll(Immediately: boolean);
begin
  if csDestroying in ComponentState then exit;
  if LazPackage=nil then exit;
  Name:=PackageEditorWindowPrefix+LazPackage.Name;
  if fForcedFlags<>[] then
    fFlags:=fFlags+fForcedFlags  // Flags forcing a partial update
  else
    fFlags:=fFlags+[             // Otherwise all flags.
      pefNeedUpdateTitle,
      pefNeedUpdateFiles,
      pefNeedUpdateRemovedFiles,
      pefNeedUpdateRequiredPkgs,
      pefNeedUpdateProperties,
      pefNeedUpdateButtons,
      pefNeedUpdateApplyDependencyButton,
      pefNeedUpdateStatusBar];
  IdleConnected:=true;
end;

function TPackageEditorForm.ShowAddDialog(var DlgPage: TAddToPkgType): TModalResult;
var
  IgnoreUnitPaths, IgnoreIncPaths: TFilenameToStringTree;

  function PkgDependsOn(PkgName: string): boolean;
  begin
    if PkgName='' then exit(false);
    Result:=PackageGraph.FindDependencyRecursively(LazPackage.FirstRequiredDependency,PkgName)<>nil;
  end;

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
      LazPackage.AddFile(UnitFilename,Unit_Name,FileType,PkgFileFlags,cpNormal);
    FreeAndNil(FNextSelectedPart);
    FNextSelectedPart:=TPENodeData.Create(penFile,AddParams.UnitFilename,false);
    PackageEditors.DeleteAmbiguousFiles(LazPackage,AddParams.UnitFilename);
  end;

  procedure AddVirtualUnit(AddParams: TAddToPkgResult);
  begin
    with AddParams do
      LazPackage.AddFile(UnitFilename,Unit_Name,FileType,PkgFileFlags,cpNormal);
    FreeAndNil(FNextSelectedPart);
    FNextSelectedPart:=TPENodeData.Create(penFile,AddParams.UnitFilename,false);
    PackageEditors.DeleteAmbiguousFiles(LazPackage,AddParams.UnitFilename);
  end;

  procedure AddNewComponent(AddParams: TAddToPkgResult);
  begin
    ExtendUnitIncPathForNewUnit(AddParams.UnitFilename,'',IgnoreUnitPaths);
    // add file
    with AddParams do
      LazPackage.AddFile(UnitFilename,Unit_Name,FileType,PkgFileFlags,cpNormal);
    FreeAndNil(FNextSelectedPart);
    FNextSelectedPart:=TPENodeData.Create(penFile,AddParams.UnitFilename,false);
    // add dependency
    if (AddParams.Dependency<>nil)
    and (not PkgDependsOn(AddParams.Dependency.PackageName)) then
      PackageGraph.AddDependencyToPackage(LazPackage,AddParams.Dependency);
    if (AddParams.IconFile<>'')
    and (not PkgDependsOn('LCL')) then
      PackageGraph.AddDependencyToPackage(LazPackage,PackageGraph.LCLPackage);
    PackageEditors.DeleteAmbiguousFiles(LazPackage,AddParams.UnitFilename);
    // open file in editor
    PackageEditors.CreateNewFile(Self,AddParams);
  end;

  procedure AddRequiredPkg(AddParams: TAddToPkgResult);
  begin
    // add dependency
    fForcedFlags:=[pefNeedUpdateRequiredPkgs];
    PackageGraph.AddDependencyToPackage(LazPackage,AddParams.Dependency);
    FreeAndNil(FNextSelectedPart);
    FNextSelectedPart:=TPENodeData.Create(penDependency,
                                        AddParams.Dependency.PackageName,false);
  end;

  procedure AddFile(AddParams: TAddToPkgResult);
  begin
    // add file
    with AddParams do begin
      if (CompareFileExt(UnitFilename,'.inc',false)=0)
      or (CompareFileExt(UnitFilename,'.lrs',false)=0) then
        ExtendIncPathForNewIncludeFile(UnitFilename,IgnoreIncPaths);
      LazPackage.AddFile(UnitFilename,Unit_Name,FileType,PkgFileFlags,cpNormal);
    end;
    FreeAndNil(FNextSelectedPart);
    FNextSelectedPart:=TPENodeData.Create(penFile,AddParams.UnitFilename,false);
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
    if AddParams.NewItem is TNewItemProjectFile then begin
      // create new file
      Desc:=TNewItemProjectFile(AddParams.NewItem).Descriptor;
      NewFilename:='';
      DummyResult:=LazarusIDE.DoNewFile(Desc,NewFilename,'',
        [nfOpenInEditor,nfCreateDefaultSrc,nfIsNotPartOfProject],LazPackage);
      if DummyResult=mrOk then begin
        // success -> now add it to package
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
        LazPackage.AddFile(NewFilename,NewUnitName,NewFileType,
                                                NewPkgFileFlags, cpNormal);
        FreeAndNil(FNextSelectedPart);
        FNextSelectedPart:=TPENodeData.Create(penFile,NewFilename,false);
      end;
    end;
  end;

var
  AddParams: TAddToPkgResult;
  OldParams: TAddToPkgResult;
begin
  if LazPackage.ReadOnly then begin
    UpdateButtons;
    exit(mrCancel);
  end;

  Result:=ShowAddToPackageDlg(LazPackage,AddParams,PackageEditors.OnGetIDEFileInfo,
    PackageEditors.OnGetUnitRegisterInfo,DlgPage);
  fLastDlgPage:=DlgPage;
  if Result<>mrOk then exit;

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
    Assert(LazPackage.Modified, 'ShowAddDialog: LazPackage.Modified = False');
  finally
    IgnoreUnitPaths.Free;
    IgnoreIncPaths.Free;
    PackageGraph.EndUpdate;
  end;
end;

procedure TPackageEditorForm.BeginUpdate;
begin
  inc(fUpdateLock);
end;

procedure TPackageEditorForm.EndUpdate;
begin
  if fUpdateLock=0 then
    RaiseException('');
  dec(fUpdateLock);
  if fUpdateLock=0 then
    IdleConnected:=true;
end;

procedure TPackageEditorForm.UpdateTitle(Immediately: boolean);
var
  NewCaption: String;
  s: string;
begin
  if not CanUpdate(pefNeedUpdateTitle,Immediately) then exit;
  s:=FLazPackage.Name+' V'+FLazPackage.Version.AsString;
  NewCaption:=Format(lisPckEditPackage, [s]);
  if LazPackage.Modified then
    NewCaption:=NewCaption+'*';
  Caption:=NewCaption;
end;

procedure TPackageEditorForm.UpdateNodeImage(TVNode: TTreeNode; NodeData: TPENodeData);
var
  ena: Boolean;
  ImgIndex: Integer;
begin
  ena := True;                   // String param is not used.
  ImgIndex:=OnTreeViewGetImageIndex('', NodeData, ena);
  TVNode.ImageIndex:=ImgIndex;
  TVNode.SelectedIndex:=ImgIndex;
end;

procedure TPackageEditorForm.UpdateButtons(Immediately: boolean);
var
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
  Writable: Boolean;
  ActiveFileCnt: Integer;
  ActiveDepCount: Integer;
  FileCount: Integer;
  DepCount: Integer;
begin
  if not CanUpdate(pefNeedUpdateButtons,Immediately) then exit;

  FileCount:=0;
  DepCount:=0;
  ActiveFileCnt:=0;
  ActiveDepCount:=0;
  for i:=0 to ItemsTreeView.SelectionCount-1 do begin
    TVNode:=ItemsTreeView.Selections[i];
    if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
    if Item is TPkgFile then begin
      inc(FileCount);
      if not NodeData.Removed then
        inc(ActiveFileCnt);
    end else if Item is TPkgDependency then begin
      inc(DepCount);
      if not NodeData.Removed then
        inc(ActiveDepCount);
    end;
  end;

  Writable:=not LazPackage.ReadOnly;
  SaveBitBtn.Enabled:=Writable and (LazPackage.IsVirtual or LazPackage.Modified);
  CompileBitBtn.Enabled:=(not LazPackage.IsVirtual) and LazPackage.CompilerOptions.HasCommands;
  AddBitBtn.Enabled:=Writable;
  RemoveBitBtn.Enabled:=Writable and (ActiveFileCnt+ActiveDepCount>0);
  OpenButton.Enabled:=(FileCount+DepCount>0);
  UseBitBtn.Caption:=lisUseSub;
  UseBitBtn.Hint:=lisClickToSeeThePossibleUses;
  UseBitBtn.OnClick:=nil;
  UseBitBtn.DropdownMenu:=UsePopupMenu;
  OptionsBitBtn.Enabled:=true;
end;

function TPackageEditorForm.OnTreeViewGetImageIndex(Str: String; Data: TObject;
                                             var AIsEnabled: Boolean): Integer;
var
  PkgFile: TPkgFile;
  Item: TObject;
  PkgDependency: TPkgDependency;
  NodeData: TPENodeData;
begin
  Result:=-1;
  if not (Data is TPENodeData) then exit;
  NodeData:=TPENodeData(Data);
  Item:=GetNodeItem(NodeData);
  if Item=nil then exit;
  if Item is TPkgFile then begin
    PkgFile:=TPkgFile(Item);
    case PkgFile.FileType of
      pftUnit,pftVirtualUnit,pftMainUnit:
        if PkgFile.HasRegisterProc then
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
  end
  else if Item is TPkgDependency then begin
    PkgDependency:=TPkgDependency(Item);
    if PkgDependency.Removed then
      Result:=ImageIndexRemovedRequired
    else if PkgDependency.LoadPackageResult=lprSuccess then
      Result:=ImageIndexRequired
    else
      Result:=ImageIndexConflict;
  end;
end;

procedure TPackageEditorForm.UpdatePending;
begin
  ItemsTreeView.BeginUpdate;
  try
    if pefNeedUpdateTitle in fFlags then
      UpdateTitle(true);
    if pefNeedUpdateFiles in fFlags then
      UpdateFiles(true);
    if pefNeedUpdateRemovedFiles in fFlags then
      UpdateRemovedFiles(true);
    if pefNeedUpdateRequiredPkgs in fFlags then
      UpdateRequiredPkgs(true);
    if pefNeedUpdateProperties in fFlags then
      UpdatePEProperties(true);
    if pefNeedUpdateButtons in fFlags then
      UpdateButtons(true);
    if pefNeedUpdateApplyDependencyButton in fFlags then
      UpdateApplyDependencyButton(true);
    if pefNeedUpdateStatusBar in fFlags then
      UpdateStatusBar(true);
    IdleConnected:=false;
  finally
    ItemsTreeView.EndUpdate;
    fForcedFlags:=[];
  end;
end;

function TPackageEditorForm.CanUpdate(Flag: TPEFlag; Immediately: boolean): boolean;
begin
  Result:=false;
  if csDestroying in ComponentState then exit;
  if LazPackage=nil then exit;
  if (fUpdateLock>0) and not Immediately then begin
    Include(fFlags,Flag);
    IdleConnected:=true;
    Result:=false;
  end else begin
    Exclude(fFlags,Flag);
    Result:=true;
  end;
end;

procedure TPackageEditorForm.UpdateFiles(Immediately: boolean);
var
  i: Integer;
  CurFile: TPkgFile;
  FilesBranch: TTreeFilterBranch;
  Filename: String;
  NodeData: TPENodeData;
  OldFilter : String;
begin
  if not CanUpdate(pefNeedUpdateFiles,Immediately) then exit;
  OldFilter := FilterEdit.ForceFilter('');

  // files belonging to package
  FilesBranch:=FilterEdit.GetCleanBranch(FFilesNode);
  FreeNodeData(penFile);
  FilesBranch.ClearNodeData;
  FilterEdit.SelectedPart:=nil;
  FilterEdit.ShowDirHierarchy:=ShowDirectoryHierarchy;
  FilterEdit.SortData:=SortAlphabetically;
  FilterEdit.ImageIndexDirectory:=ImageIndexDirectory;
  // collect and sort files
  for i:=0 to LazPackage.FileCount-1 do begin
    CurFile:=LazPackage.Files[i];
    NodeData:=CreateNodeData(penFile,CurFile.Filename,false);
    NodeData.FileType:=CurFile.FileType;
    Filename:=CurFile.GetShortFilename(true);
    if Filename='' then continue;
    if (FNextSelectedPart<>nil) and (FNextSelectedPart.Typ=penFile)
    and (FNextSelectedPart.Name=NodeData.Name)
    then
      FilterEdit.SelectedPart:=NodeData;
    FilesBranch.AddNodeData(Filename, NodeData, CurFile.Filename);
  end;
  if (FNextSelectedPart<>nil) and (FNextSelectedPart.Typ=penFile) then
    FreeAndNil(FNextSelectedPart);

  FilterEdit.Filter := OldFilter;            // This triggers ApplyFilter
  FilterEdit.InvalidateFilter;
  UpdatePEProperties;
  UpdateButtons;
end;

procedure TPackageEditorForm.UpdateRemovedFiles(Immediately: boolean = false);
var
  i: Integer;
  CurFile: TPkgFile;
  RemovedBranch: TTreeFilterBranch;
  NodeData: TPENodeData;
begin
  if not CanUpdate(pefNeedUpdateRemovedFiles,Immediately) then exit;

  if LazPackage.RemovedFilesCount>0 then begin
    // Create root node for removed files if not done yet.
    if FRemovedFilesNode=nil then begin
      FRemovedFilesNode:=ItemsTreeView.Items.Add(FRequiredPackagesNode,
                                                 lisPckEditRemovedFiles);
      FRemovedFilesNode.ImageIndex:=ImageIndexRemovedFiles;
      FRemovedFilesNode.SelectedIndex:=FRemovedFilesNode.ImageIndex;
    end;
    RemovedBranch:=FilterEdit.GetCleanBranch(FRemovedFilesNode);
    RemovedBranch.ClearNodeData;
    for i:=0 to LazPackage.RemovedFilesCount-1 do begin
      CurFile:=LazPackage.RemovedFiles[i];
      NodeData:=CreateNodeData(penFile,CurFile.Filename,true);
      RemovedBranch.AddNodeData(CurFile.GetShortFilename(true), NodeData);
    end;
    RemovedBranch.InvalidateBranch;
  end
  else begin
    // No more removed files left -> delete the root node
    if FRemovedFilesNode<>nil then begin
      FilterEdit.DeleteBranch(FRemovedFilesNode);
      FreeAndNil(FRemovedFilesNode);
      FilterEdit.InvalidateFilter;
    end;
  end;

  UpdatePEProperties;
  UpdateButtons;
end;

procedure TPackageEditorForm.UpdateRequiredPkgs(Immediately: boolean);
var
  CurDependency: TPkgDependency;
  RequiredBranch, RemovedBranch: TTreeFilterBranch;
  CurNodeText, aFilename, OldFilter: String;
  NodeData: TPENodeData;
begin
  if not CanUpdate(pefNeedUpdateRequiredPkgs,Immediately) then exit;

  OldFilter := FilterEdit.ForceFilter('');

  // required packages
  RequiredBranch:=FilterEdit.GetCleanBranch(FRequiredPackagesNode);
  FreeNodeData(penDependency);
  RequiredBranch.ClearNodeData;
  CurDependency:=LazPackage.FirstRequiredDependency;
  FilterEdit.SelectedPart:=nil;
  while CurDependency<>nil do begin
    CurNodeText:=CurDependency.AsString;
    if CurDependency.DefaultFilename<>'' then begin
      aFilename:=CurDependency.MakeFilenameRelativeToOwner(CurDependency.DefaultFilename);
      if CurDependency.PreferDefaultFilename then
        CurNodeText:=CurNodeText+' in '+aFilename // like the 'in' keyword in uses section
      else
        CurNodeText:=Format(lisPckEditDefault, [CurNodeText, aFilename]);
    end;
    NodeData:=CreateNodeData(penDependency,CurDependency.PackageName,false);
    if (FNextSelectedPart<>nil) and (FNextSelectedPart.Typ=penDependency)
    and (FNextSelectedPart.Name=NodeData.Name)
    then
      FilterEdit.SelectedPart:=NodeData;
    RequiredBranch.AddNodeData(CurNodeText, NodeData);
    CurDependency:=CurDependency.NextRequiresDependency;
  end;
  if (FNextSelectedPart<>nil) and (FNextSelectedPart.Typ=penDependency) then
    FreeAndNil(FNextSelectedPart);
  RequiredBranch.InvalidateBranch;

  // removed required packages
  CurDependency:=LazPackage.FirstRemovedDependency;
  if CurDependency<>nil then begin
    if FRemovedRequiredNode=nil then begin
      FRemovedRequiredNode:=ItemsTreeView.Items.Add(nil,lisPckEditRemovedRequiredPackages);
      FRemovedRequiredNode.ImageIndex:=ImageIndexRemovedRequired;
      FRemovedRequiredNode.SelectedIndex:=FRemovedRequiredNode.ImageIndex;
    end;
    RemovedBranch:=FilterEdit.GetCleanBranch(FRemovedRequiredNode);
    RemovedBranch.ClearNodeData;
    while CurDependency<>nil do begin
      NodeData:=CreateNodeData(penDependency,CurDependency.PackageName,true);
      RemovedBranch.AddNodeData(CurDependency.AsString, NodeData);
      CurDependency:=CurDependency.NextRequiresDependency;
    end;
    RemovedBranch.InvalidateBranch;
  end else begin
    if FRemovedRequiredNode<>nil then begin
      FilterEdit.DeleteBranch(FRemovedRequiredNode);
      FreeAndNil(FRemovedRequiredNode);
    end;
  end;
  FNextSelectedPart:=nil;
  if OldFilter <> '' then begin
    FilterEdit.Filter := OldFilter;            // This triggers ApplyFilter
    FilterEdit.InvalidateFilter;
  end;
  UpdatePEProperties;
  UpdateButtons;
end;

procedure TPackageEditorForm.UpdatePEProperties(Immediately: boolean);
type
  TMultiBool = (mubNone, mubAllTrue, mubAllFalse, mubMixed);

  procedure MergeMultiBool(var b: TMultiBool; NewValue: boolean);
  begin
    case b of
    mubNone: if NewValue then b:=mubAllTrue else b:=mubAllFalse;
    mubAllTrue: if not NewValue then b:=mubMixed;
    mubAllFalse: if NewValue then b:=mubMixed;
    mubMixed: ;
    end;
  end;

  procedure SetCheckBox(Box: TCheckBox; aVisible: boolean; State: TMultiBool);
  begin
    Box.Visible:=aVisible;
    case State of
    mubAllTrue:
      begin
        Box.State:=cbChecked;
        Box.AllowGrayed:=false;
      end;
    mubAllFalse:
      begin
        Box.State:=cbUnchecked;
        Box.AllowGrayed:=false;
      end;
    mubMixed:
      begin
        Box.AllowGrayed:=true;
        Box.State:=cbGrayed;
      end;
    end;
  end;

var
  CurFile: TPkgFile;
  CurDependency: TPkgDependency;
  CurComponent: TPkgComponent;
  CurLine, CurFilename: string;
  i, j: Integer;
  NodeData: TPENodeData;
  Item: TObject;
  SelFileCount: Integer;
  SelDepCount: Integer;
  SelHasRegisterProc: TMultiBool;
  SelAddToUsesPkgSection: TMultiBool;
  SelDisableI18NForLFM: TMultiBool;
  SelUnitCount: Integer;
  SelDirCount: Integer;
  SelHasLFMCount: Integer;
  OnlyFilesSelected: Boolean;
  OnlyFilesWithUnitsSelected: Boolean;
  aVisible: Boolean;
  TVNode: TTreeNode;
  SingleSelectedDirectory: TTreeNode;
  SingleSelectedRemoved: Boolean;
  SingleSelected: TTreeNode;
  FileCount: integer;
  HasRegisterProcCount: integer;
  AddToUsesPkgSectionCount: integer;
begin
  if not CanUpdate(pefNeedUpdateProperties,Immediately) then exit;

  FPlugins.Clear;

  // check selection
  FSingleSelectedDep:=nil;
  FSingleSelectedFile:=nil;
  SingleSelectedDirectory:=nil;
  SingleSelectedRemoved:=false;
  SingleSelected:=nil;
  SelFileCount:=0;
  SelDepCount:=0;
  SelHasRegisterProc:=mubNone;
  SelAddToUsesPkgSection:=mubNone;
  SelDisableI18NForLFM:=mubNone;
  SelUnitCount:=0;
  SelHasLFMCount:=0;
  SelDirCount:=0;
  for i:=0 to ItemsTreeView.SelectionCount-1 do begin
    TVNode:=ItemsTreeView.Selections[i];
    if GetNodeDataItem(TVNode,NodeData,Item) then begin
      if Item is TPkgFile then begin
        CurFile:=TPkgFile(Item);
        inc(SelFileCount);
        FSingleSelectedFile:=CurFile;
        SingleSelected:=TVNode;
        SingleSelectedRemoved:=NodeData.Removed;
        MergeMultiBool(SelHasRegisterProc,CurFile.HasRegisterProc);
        if CurFile.FileType in PkgFileUnitTypes then begin
          inc(SelUnitCount);
          MergeMultiBool(SelAddToUsesPkgSection,CurFile.AddToUsesPkgSection);
          if (CurFile.FileType in PkgFileRealUnitTypes) then
          begin
            CurFilename:=CurFile.GetFullFilename;
            if FilenameIsAbsolute(CurFilename)
                and FileExistsCached(ChangeFileExt(CurFilename,'.lfm'))
            then begin
              inc(SelHasLFMCount);
              MergeMultiBool(SelDisableI18NForLFM,CurFile.DisableI18NForLFM);
            end;
          end;
          // fetch all registered plugins
          for j:=0 to CurFile.ComponentCount-1 do begin
            CurComponent:=CurFile.Components[j];
            CurLine:=CurComponent.ComponentClass.ClassName;
            FPlugins.AddObject(CurLine,CurComponent);
          end;
        end;
      end else if Item is TPkgDependency then begin
        inc(SelDepCount);
        CurDependency:=TPkgDependency(Item);
        FSingleSelectedDep:=CurDependency;
        SingleSelected:=TVNode;
        SingleSelectedRemoved:=NodeData.Removed;
      end;
    end else if IsDirectoryNode(TVNode) or (TVNode=FFilesNode) then begin
      inc(SelDirCount);
      SingleSelectedDirectory:=TVNode;
      SingleSelected:=TVNode;
    end;
  end;

  if (SelFileCount+SelDepCount+SelDirCount>1) then begin
    // it is a multi selection
    FSingleSelectedFile:=nil;
    FSingleSelectedDep:=nil;
    SingleSelectedDirectory:=nil;
    SingleSelected:=nil;
  end;
  OnlyFilesSelected:=(SelFileCount>0) and (SelDepCount=0) and (SelDirCount=0);
  OnlyFilesWithUnitsSelected:=OnlyFilesSelected and (SelUnitCount>0);

  //debugln(['TPackageEditorForm.UpdatePEProperties SelFileCount=',SelFileCount,' SelDepCount=',SelDepCount,' SelDirCount=',SelDirCount,' SelUnitCount=',SelUnitCount]);
  //debugln(['TPackageEditorForm.UpdatePEProperties FSingleSelectedFile=',FSingleSelectedFile<>nil,' FSingleSelectedDependency=',FSingleSelectedDep<>nil,' SingleSelectedDirectory=',SingleSelectedDirectory<>nil]);

  DisableAlign;
  try
    // move up/down (only single selection)
    aVisible:=(not (SortAlphabetically or SingleSelectedRemoved))
       and ((FSingleSelectedFile<>nil) or (FSingleSelectedDep<>nil));
    MoveUpBtn.Enabled  :=aVisible and Assigned(SingleSelected.GetPrevVisibleSibling);
    MoveDownBtn.Enabled:=aVisible and Assigned(SingleSelected.GetNextVisibleSibling);

    // Min/Max version of dependency (only single selection)
    aVisible:=FSingleSelectedDep<>nil;
    UseMinVersionCheckBox.Visible:=aVisible;
    MinVersionEdit.Visible:=aVisible;
    UseMaxVersionCheckBox.Visible:=aVisible;
    MaxVersionEdit.Visible:=aVisible;
    ApplyDependencyButton.Visible:=aVisible;

    // 'RegisterProc' of files (supports multi selection)
    SetCheckBox(CallRegisterProcCheckBox,OnlyFilesWithUnitsSelected, SelHasRegisterProc);
    CallRegisterProcCheckBox.Enabled:=(not LazPackage.ReadOnly);

    // 'Add to uses' of files (supports multi selection)
    SetCheckBox(AddToUsesPkgSectionCheckBox,OnlyFilesWithUnitsSelected, SelAddToUsesPkgSection);
    AddToUsesPkgSectionCheckBox.Enabled:=(not LazPackage.ReadOnly);

    // disable i18n for lfm (supports multi selection)
    SetCheckBox(DisableI18NForLFMCheckBox,
     OnlyFilesWithUnitsSelected and (SelHasLFMCount>0) and LazPackage.EnableI18N
     and LazPackage.EnableI18NForLFM,
     SelDisableI18NForLFM);
    DisableI18NForLFMCheckBox.Enabled:=(not LazPackage.ReadOnly);

    // registered plugins (supports multi selection)
    RegisteredPluginsGroupBox.Visible:=OnlyFilesWithUnitsSelected;
    RegisteredPluginsGroupBox.Enabled:=(not LazPackage.ReadOnly);
    if not RegisteredPluginsGroupBox.Visible then
      FPlugins.Clear;
    RegisteredListBox.Items.Assign(FPlugins);

    // directory summary (only single selection)
    FDirSummaryLabel.Visible:=(SelFileCount=0) and (SelDepCount=0) and (SelDirCount=1);

    if SelFileCount>0 then begin
      PropsGroupBox.Enabled:=true;
      PropsGroupBox.Caption:=lisPckEditFileProperties;
    end
    else if FSingleSelectedDep<>nil then begin
      PropsGroupBox.Enabled:=not SingleSelectedRemoved;
      PropsGroupBox.Caption:=lisPckEditDependencyProperties;
      UseMinVersionCheckBox.Checked:=pdfMinVersion in FSingleSelectedDep.Flags;
      MinVersionEdit.Text:=FSingleSelectedDep.MinVersion.AsString;
      MinVersionEdit.Enabled:=pdfMinVersion in FSingleSelectedDep.Flags;
      UseMaxVersionCheckBox.Checked:=pdfMaxVersion in FSingleSelectedDep.Flags;
      MaxVersionEdit.Text:=FSingleSelectedDep.MaxVersion.AsString;
      MaxVersionEdit.Enabled:=pdfMaxVersion in FSingleSelectedDep.Flags;
      UpdateApplyDependencyButton;
    end
    else if SingleSelectedDirectory<>nil then begin
      PropsGroupBox.Enabled:=true;
      GetDirectorySummary(SingleSelectedDirectory,
        FileCount,HasRegisterProcCount,AddToUsesPkgSectionCount);
      FDirSummaryLabel.Caption:=Format(
        lisFilesHasRegisterProcedureInPackageUsesSection, [IntToStr(FileCount),
        IntToStr(HasRegisterProcCount), IntToStr(AddToUsesPkgSectionCount)]);
    end
    else begin
      PropsGroupBox.Enabled:=false;
    end;
  finally
    EnableAlign;
  end;
end;

procedure TPackageEditorForm.UpdateApplyDependencyButton(Immediately: boolean);
var
  DependencyChanged: Boolean;
  AVersion: TPkgVersion;
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
begin
  if not CanUpdate(pefNeedUpdateApplyDependencyButton,Immediately) then exit;

  FSingleSelectedDep:=nil;
  for i:=0 to ItemsTreeView.SelectionCount-1 do begin
    TVNode:=ItemsTreeView.Selections[i];
    if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
    if Item is TPkgFile then begin
      FSingleSelectedDep:=nil;
      break;
    end else if Item is TPkgDependency then begin
      if FSingleSelectedDep<>nil then begin
        FSingleSelectedDep:=nil;
        break;
      end;
      FSingleSelectedDep:=TPkgDependency(Item);
    end;
  end;

  DependencyChanged:=false;
  if (FSingleSelectedDep<>nil) then begin
    // check min version
    if UseMinVersionCheckBox.Checked<>(pdfMinVersion in FSingleSelectedDep.Flags) then
      DependencyChanged:=true;
    if UseMinVersionCheckBox.Checked then begin
      AVersion:=TPkgVersion.Create;
      if AVersion.ReadString(MinVersionEdit.Text)
      and (AVersion.Compare(FSingleSelectedDep.MinVersion)<>0) then
        DependencyChanged:=true;
      AVersion.Free;
    end;
    // check max version
    if UseMaxVersionCheckBox.Checked<>(pdfMaxVersion in FSingleSelectedDep.Flags) then
      DependencyChanged:=true;
    if UseMaxVersionCheckBox.Checked then begin
      AVersion:=TPkgVersion.Create;
      if AVersion.ReadString(MaxVersionEdit.Text)
      and (AVersion.Compare(FSingleSelectedDep.MaxVersion)<>0) then
        DependencyChanged:=true;
      AVersion.Free;
    end;
  end;
  ApplyDependencyButton.Enabled:=DependencyChanged;
end;

procedure TPackageEditorForm.UpdateStatusBar(Immediately: boolean);
var
  StatusText: String;
begin
  if not CanUpdate(pefNeedUpdateStatusBar,Immediately) then exit;

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

function TPackageEditorForm.GetNodeData(TVNode: TTreeNode): TPENodeData;
var
  o: TObject;
begin
  Result:=nil;
  if (TVNode=nil) then exit;
  o:=TObject(TVNode.Data);
  if o is TFileNameItem then
    o:=TObject(TFileNameItem(o).Data);
  if o is TPENodeData then
    Result:=TPENodeData(o);
end;

function TPackageEditorForm.GetNodeItem(NodeData: TPENodeData): TObject;
begin
  Result:=nil;
  if (LazPackage=nil) or (NodeData=nil) then exit;
  case NodeData.Typ of
  penFile:
    if NodeData.Removed then
      Result:=LazPackage.FindRemovedPkgFile(NodeData.Name)
    else
      Result:=LazPackage.FindPkgFile(NodeData.Name,true,true);
  penDependency:
    if NodeData.Removed then
      Result:=LazPackage.FindRemovedDependencyByName(NodeData.Name)
    else
      Result:=LazPackage.FindDependencyByName(NodeData.Name);
  end;
end;

function TPackageEditorForm.GetNodeDataItem(TVNode: TTreeNode; out
  NodeData: TPENodeData; out Item: TObject): boolean;
begin
  Result:=false;
  Item:=nil;
  NodeData:=GetNodeData(TVNode);
  Item:=GetNodeItem(NodeData);
  Result:=Item<>nil;
end;

function TPackageEditorForm.IsDirectoryNode(Node: TTreeNode): boolean;
begin
  Result:=(Node<>nil) and (Node.Data=nil) and Node.HasAsParent(FFilesNode);
end;

function TPackageEditorForm.GetNodeFilename(Node: TTreeNode): string;
var
  Item: TFileNameItem;
begin
  Result:='';
  if Node=nil then exit;
  if Node=FFilesNode then
    exit(FilesBaseDirectory);
  Item:=TFileNameItem(Node.Data);
  if (Item is TFileNameItem) then begin
    Result:=Item.Filename;
  end else if Node.HasAsParent(FFilesNode) then begin
    // directory node
    Result:=Node.Text;
  end else
    exit;
  if not FilenameIsAbsolute(Result) then
    Result:=AppendPathDelim(FilesBaseDirectory)+Result;
end;

procedure TPackageEditorForm.GetDirectorySummary(DirNode: TTreeNode; out
  FileCount, HasRegisterProcCount, AddToUsesPkgSectionCount: integer);

  procedure Traverse(Node: TTreeNode);
  var
    CurFile: TPkgFile;
    NodeData: TPENodeData;
  begin
    NodeData:=GetNodeData(Node);
    if NodeData<>nil then begin
      if NodeData.Typ=penFile then begin
        CurFile:=LazPackage.FindPkgFile(NodeData.Name,true,true);
        if CurFile<>nil then begin
          inc(FileCount);
          if CurFile.HasRegisterProc then inc(HasRegisterProcCount);
          if CurFile.AddToUsesPkgSection then inc(AddToUsesPkgSectionCount);
        end;
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
  LazPackage.ShortenFilename(ShortDirectory,true);
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
        Format(lisPkgEditTheFileIsCurrentlyNotInTheUnitpathOfThePackage,
               [AnUnitFilename, LineEnding, LineEnding+LineEnding, ShortDirectory]),
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
     Format(lisPETheFileIsCurrentlyNotInTheIncludePathOfThePackageA,
            [AnIncludeFile, LineEnding, ShortDirectory]),
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

function TPackageEditorForm.ExtendUnitSearchPath(NewUnitPaths: string): boolean;
var
  CurUnitPaths: String;
  r: TModalResult;
begin
  CurUnitPaths:=LazPackage.CompilerOptions.ParsedOpts.GetParsedValue(pcosUnitPath);
  NewUnitPaths:=RemoveSearchPaths(NewUnitPaths,CurUnitPaths);
  if NewUnitPaths<>'' then begin
    NewUnitPaths:=CreateRelativeSearchPath(NewUnitPaths,LazPackage.Directory);
    r:=IDEMessageDialog(lisExtendUnitPath,
      Format(lisExtendUnitSearchPathOfPackageWith, [LazPackage.Name, #13,
        NewUnitPaths]), mtConfirmation, [mbYes, mbNo, mbCancel]);
    case r of
    mrYes: LazPackage.CompilerOptions.OtherUnitFiles:=
      MergeSearchPaths(LazPackage.CompilerOptions.OtherUnitFiles,NewUnitPaths);
    mrNo: ;
    else exit(false);
    end;
  end;
  Result:=true;
end;

function TPackageEditorForm.ExtendIncSearchPath(NewIncPaths: string): boolean;
var
  CurIncPaths: String;
  r: TModalResult;
begin
  CurIncPaths:=LazPackage.CompilerOptions.ParsedOpts.GetParsedValue(pcosIncludePath);
  NewIncPaths:=RemoveSearchPaths(NewIncPaths,CurIncPaths);
  if NewIncPaths<>'' then begin
    NewIncPaths:=CreateRelativeSearchPath(NewIncPaths,LazPackage.Directory);
    r:=IDEMessageDialog(lisExtendIncludePath,
      Format(lisExtendIncludeFileSearchPathOfPackageWith, [LazPackage.Name, #13,
        NewIncPaths]), mtConfirmation, [mbYes, mbNo, mbCancel]);
    case r of
    mrYes: LazPackage.CompilerOptions.IncludePath:=
      MergeSearchPaths(LazPackage.CompilerOptions.IncludePath,NewIncPaths);
    mrNo: ;
    else exit(false);
    end;
  end;
end;

function TPackageEditorForm.FilesEditTreeView: TTreeView;
begin
  Result:=ItemsTreeView;
end;

function TPackageEditorForm.FilesEditForm: TCustomForm;
begin
  Result:=Self;
end;

function TPackageEditorForm.FilesOwner: TObject;
begin
  Result:=LazPackage;
end;

function TPackageEditorForm.FilesOwnerName: string;
begin
  Result:=Format(lisPackage2, [LazPackage.Name]);
end;

function TPackageEditorForm.TVNodeFiles: TTreeNode;
begin
  Result:=FFilesNode;
end;

function TPackageEditorForm.TVNodeRequiredPackages: TTreeNode;
begin
  Result:=FRequiredPackagesNode;
end;

function TPackageEditorForm.FilesBaseDirectory: string;
begin
  Result:=LazPackage.DirectoryExpanded;
end;

function TPackageEditorForm.FilesOwnerReadOnly: boolean;
begin
  Result:=LazPackage.ReadOnly;
end;

function TPackageEditorForm.FirstRequiredDependency: TPkgDependency;
begin
  Result:=LazPackage.FirstRequiredDependency;
end;

function TPackageEditorForm.CanBeAddedToProject: boolean;
begin
  if LazPackage=nil then exit(false);
  Result:=PackageEditors.AddToProject(LazPackage,true)=mrOk;
end;

procedure TPackageEditorForm.DoSave(SaveAs: boolean);
begin
  PackageEditors.SavePackage(LazPackage,SaveAs);
  UpdateTitle;
  UpdateButtons;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.DoCompile(CompileClean, CompileRequired: boolean);
begin
  PackageEditors.CompilePackage(LazPackage,CompileClean,CompileRequired);
  UpdateTitle;
  UpdateButtons;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.DoFindInFiles;
begin
  PackageEditors.FindInFiles(LazPackage);
end;

procedure TPackageEditorForm.DoRevert;
begin
  if MessageDlg(lisPkgEditRevertPackage,
    Format(lisPkgEditDoYouReallyWantToForgetAllChangesToPackageAnd, [LazPackage.IDAsString]),
    mtConfirmation,[mbYes,mbNo],0)<>mrYes
  then exit;
  PackageEditors.RevertPackage(LazPackage);
  UpdateAll(false);
end;

procedure TPackageEditorForm.DoPublishProject;
begin
  PackageEditors.PublishPackage(LazPackage);
end;

procedure TPackageEditorForm.DoEditVirtualUnit;
begin
  if LazPackage=nil then exit;
  if (FSingleSelectedFile=nil)
  or (FSingleSelectedFile.FileType<>pftVirtualUnit)
  or (LazPackage.IndexOfPkgFile(FSingleSelectedFile)<0)
  then exit;
  if ShowEditVirtualPackageDialog(FSingleSelectedFile)=mrOk then
    UpdateFiles;
end;

procedure TPackageEditorForm.DoExpandDirectory;
var
  CurNode: TTreeNode;
begin
  if not ShowDirectoryHierarchy then exit;
  CurNode:=ItemsTreeView.Selected;
  if not (IsDirectoryNode(CurNode) or (CurNode=FFilesNode)) then exit;
  ItemsTreeView.BeginUpdate;
  CurNode.Expand(true);
  ItemsTreeView.EndUpdate;
end;

procedure TPackageEditorForm.DoCollapseDirectory;
var
  CurNode: TTreeNode;
  Node: TTreeNode;
begin
  if not ShowDirectoryHierarchy then exit;
  CurNode:=ItemsTreeView.Selected;
  if not (IsDirectoryNode(CurNode) or (CurNode=FFilesNode)) then exit;
  ItemsTreeView.BeginUpdate;
  Node:=CurNode.GetFirstChild;
  while Node<>nil do
  begin
    Node.Collapse(true);
    Node:=Node.GetNextSibling;
  end;
  ItemsTreeView.EndUpdate;
end;

procedure TPackageEditorForm.DoUseUnitsInDirectory(Use: boolean);

  procedure Traverse(Node: TTreeNode);
  var
    PkgFile: TPkgFile;
    NodeData: TPENodeData;
  begin
    NodeData:=GetNodeData(Node);
    if (NodeData<>nil) and (NodeData.Typ=penFile) then
    begin
      PkgFile:=LazPackage.FindPkgFile(NodeData.Name,true,true);
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
  CurNode:=ItemsTreeView.Selected;
  if not (IsDirectoryNode(CurNode) or (CurNode=FFilesNode)) then exit;
  Traverse(CurNode);
  UpdatePEProperties;
end;

procedure TPackageEditorForm.DoMoveCurrentFile(Offset: integer);
var
  OldIndex, NewIndex: Integer;
  FilesBranch: TTreeFilterBranch;
begin
  if (LazPackage=nil) or (FSingleSelectedFile=nil) then exit;
  OldIndex:=LazPackage.IndexOfPkgFile(FSingleSelectedFile);
  if OldIndex<0 then exit;
  NewIndex:=OldIndex+Offset;
  if (NewIndex<0) or (NewIndex>=LazPackage.FileCount) then exit;
  FilesBranch:=FilterEdit.GetExistingBranch(FFilesNode);
  LazPackage.MoveFile(OldIndex,NewIndex);
  FilesBranch.MoveFile(OldIndex,NewIndex);
  UpdatePEProperties;
  UpdateStatusBar;
  FilterEdit.InvalidateFilter;
end;

procedure TPackageEditorForm.DoMoveDependency(Offset: integer);
var
  OldSelection: TStringList;
begin
  ItemsTreeView.BeginUpdate;
  OldSelection:=ItemsTreeView.StoreCurrentSelection;
  if Offset<0 then
    PackageGraph.MoveRequiredDependencyUp(FSingleSelectedDep)
  else
    PackageGraph.MoveRequiredDependencyDown(FSingleSelectedDep);
  ItemsTreeView.ApplyStoredSelection(OldSelection);
  ItemsTreeView.EndUpdate;
end;

procedure TPackageEditorForm.DoSortFiles;
var
  TreeSelection: TStringList;
begin
  TreeSelection:=ItemsTreeView.StoreCurrentSelection;
  LazPackage.SortFiles;
  ItemsTreeView.ApplyStoredSelection(TreeSelection);
end;

function TPackageEditorForm.DoOpenPkgFile(PkgFile: TPkgFile): TModalResult;
begin
  Result:=PackageEditors.OpenPkgFile(Self,PkgFile);
end;

procedure TPackageEditorForm.DoFixFilesCase;
begin
  LazPackage.FixFilesCaseSensitivity;
end;

procedure TPackageEditorForm.DoShowMissingFiles;
begin
  ShowMissingPkgFilesDialog(LazPackage);
end;

constructor TPackageEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TPackageEditorForm.Destroy;
begin
  inherited Destroy;
end;

{ TPackageEditors }

function TPackageEditors.GetEditors(Index: integer): TPackageEditorForm;
begin
  Result:=TPackageEditorForm(FItems[Index]);
end;

constructor TPackageEditors.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TPackageEditors.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
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
  if FItems<>nil then
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

function TPackageEditors.OpenFile(Sender: TObject; const Filename: string): TModalResult;
begin
  if Assigned(OnOpenFile) then
    Result:=OnOpenFile(Sender,Filename)
  else
    Result:=mrCancel;
end;

function TPackageEditors.OpenPkgFile(Sender: TObject; PkgFile: TPkgFile): TModalResult;
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
  if FItems<>nil then
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
  for i:=0 to Count-1 do
    Editors[i].UpdateAll(Immediately);
end;

function TPackageEditors.ShouldNotBeInstalled(APackage: TLazPackage): boolean;
begin
  Result:=APackage.Missing
     or ((APackage.FindUnitWithRegister=nil) and (APackage.Provides.Count=0));
end;

function TPackageEditors.InstallPackage(APackage: TLazPackage): TModalResult;
begin
  if ShouldNotBeInstalled(APackage) then begin
    if IDEQuestionDialog(lisNotAnInstallPackage,
      Format(lisThePackageDoesNotHaveAnyRegisterProcedureWhichTypi,
             [APackage.Name, LineEnding+LineEnding]),
      mtWarning,
      [mrIgnore, lisInstallItILikeTheFat, mrCancel, lisCancel], '')<>mrIgnore
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

function TPackageEditors.FindInFiles(APackage: TLazPackage): TModalResult;
begin
  if Assigned(OnShowFindInFiles) then
    Result:=OnShowFindInFiles(Self,APackage)
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
  if Assigned(OnCreateMakeFile) then
    Result:=OnCreateMakeFile(Self,APackage)
  else
    Result:=mrCancel;
end;

function TPackageEditors.CreateFpmakeFile(APackage: TLazPackage): TModalResult;
begin
  if Assigned(OnCreateFpmakefile) then
    Result:=OnCreateFpmakefile(Self,APackage)
  else
    Result:=mrCancel;
end;

function TPackageEditors.TreeViewToPkgEditor(TV: TTreeView): TPackageEditorForm;
var
  aParent: TWinControl;
begin
  Result:=nil;
  if TV.Name<>'ItemsTreeView' then exit;
  aParent:=TV;
  repeat
    if aParent=nil then exit;
    aParent:=aParent.Parent;
  until aParent is TPackageEditorForm;
  Result:=TPackageEditorForm(aParent);
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

