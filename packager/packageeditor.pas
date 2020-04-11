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

  Author: Mattias Gaertner

  Abstract:
    TPackageEditorForm is the form of a package editor.
}
unit PackageEditor;

{$mode objfpc}{$H+}

{off $DEFINE VerbosePkgEditDrag}

interface

uses
  // RTL, FCL
  Classes, SysUtils, contnrs,
  // LCL
  Forms, Controls, StdCtrls, ComCtrls, Buttons, Graphics, Menus, Dialogs,
  ExtCtrls, ImgList, LCLType, LCLIntf,
  TreeFilterEdit,
  // LazUtils
  FileUtil, LazFileUtils, LazFileCache, AvgLvlTree, LazLoggerBase, LazTracer,
  // IDEIntf
  IDEImagesIntf, MenuIntf, LazIDEIntf, ProjectIntf,
  FormEditingIntf, PackageDependencyIntf, PackageIntf, IDEHelpIntf, IDEOptionsIntf,
  NewItemIntf, IDEWindowIntf, IDEDialogs, ComponentReg, IDEOptEditorIntf,
  // IDE
  MainBase, IDEProcs, LazarusIDEStrConsts, IDEDefs, CompilerOptions,
  EnvironmentOpts, DialogProcs, InputHistory, PackageDefs, AddToPackageDlg,
  AddPkgDependencyDlg, AddFPMakeDependencyDlg, ProjPackChecks, PkgVirtualUnitEditor,
  MissingPkgFilesDlg, PackageSystem, CleanPkgDeps;
  
const
  PackageEditorMenuRootName = 'PackageEditor';
  PackageEditorMenuFilesRootName = 'PackageEditorFiles';
  PackageEditorWindowPrefix = 'PackageEditor_';
var
  // General actions for the Files and Required packages root nodes.
  // Duplicates actions found under the "Add" button.
  PkgEditMenuAddDiskFile: TIDEMenuCommand;
  PkgEditMenuAddNewFile: TIDEMenuCommand;
  PkgEditMenuAddNewComp: TIDEMenuCommand;
  PkgEditMenuAddNewReqr: TIDEMenuCommand;
  PkgEditMenuAddNewFPMakeReqr: TIDEMenuCommand;

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
  PkgEditMenuOpenFolder: TIDEMenuCommand;

  // dependencies
  PkgEditMenuRemoveDependency: TIDEMenuCommand;
  PkgEditMenuReAddDependency: TIDEMenuCommand;
  PkgEditMenuDepStoreFileNameDefault: TIDEMenuCommand;
  PkgEditMenuDepStoreFileNamePreferred: TIDEMenuCommand;
  PkgEditMenuDepClearStoredFileName: TIDEMenuCommand;
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

  TIDEPackageOptsDlgAction = (
    iodaRead,
    iodaWrite,
    iodaRestore
    );

  { TPackageEditorForm }

  TPackageEditorForm = class(TBasePackageEditor,IFilesEditorInterface)
    MenuItem1: TMenuItem;
    mnuAddFPMakeReq: TMenuItem;
    mnuAddDiskFile: TMenuItem;
    mnuAddNewFile: TMenuItem;
    mnuAddNewComp: TMenuItem;
    mnuAddNewReqr: TMenuItem;
    MoveDownBtn: TSpeedButton;
    MoveUpBtn: TSpeedButton;
    DirectoryHierarchyButton: TSpeedButton;
    OpenButton: TSpeedButton;
    DisableI18NForLFMCheckBox: TCheckBox;
    FilterEdit: TTreeFilterEdit;
    FilterPanel: TPanel;
    AddPopupMenu: TPopupMenu;
    SortAlphabeticallyButton: TSpeedButton;
    Splitter1: TSplitter;
    // toolbar
    ToolBar: TToolBar;
    // toolbuttons
    SaveBitBtn: TToolButton;
    CompileBitBtn: TToolButton;
    UseBitBtn: TToolButton;
    AddBitBtn: TToolButton;
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
    PropsPageControl: TPageControl;
    CommonOptionsTabSheet: TTabSheet;
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
    procedure FilterEditKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FindInFilesMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure ItemsPopupMenuPopup(Sender: TObject);
    procedure ItemsTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; {%H-}State: TCustomDrawState; Stage: TCustomDrawStage;
      var {%H-}PaintImages, {%H-}DefaultDraw: Boolean);
    procedure ItemsTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ItemsTreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ItemsTreeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mnuAddDiskFileClick(Sender: TObject);
    procedure mnuAddFPMakeReqClick(Sender: TObject);
    procedure mnuAddNewCompClick(Sender: TObject);
    procedure mnuAddNewReqrClick(Sender: TObject);
    procedure mnuAddNewFileClick(Sender: TObject);
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
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure OpenFileMenuItemClick(Sender: TObject);
    procedure OpenFolderMenuItemClick(Sender: TObject);
    procedure OptionsBitBtnClick(Sender: TObject);
    procedure PackageEditorFormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure PackageEditorFormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure PublishClick(Sender: TObject);
    procedure ReAddMenuItemClick(Sender: TObject);
    procedure RegisteredListBoxDrawItem({%H-}Control: TWinControl; Index: Integer;
                                        ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure RemoveBitBtnClick(Sender: TObject);
    procedure RevertClick(Sender: TObject);
    procedure SaveAsClick(Sender: TObject);
    procedure SaveBitBtnClick(Sender: TObject);
    procedure SetDepDefaultFilenameMenuItemClick(Sender: TObject);
    procedure SetDepPreferredFilenameMenuItemClick(Sender: TObject);
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
    FCompiling: boolean;
    FCompileDesignTimePkg: boolean;
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
    FOptionsShownOfFile: TPkgFile;
    FFirstNodeData: array[TPENodeType] of TPENodeData;
    fUpdateLock: integer;
    fForcedFlags: TPEFlags;
    procedure DoAddNewFile(NewItem: TNewIDEItemTemplate);
    procedure FreeNodeData(Typ: TPENodeType);
    function CreateNodeData(Typ: TPENodeType; aName: string; aRemoved: boolean): TPENodeData;
    procedure SetDependencyDefaultFilename(AsPreferred: boolean);
    procedure SetIdleConnected(AValue: boolean);
    procedure SetShowDirectoryHierarchy(const AValue: boolean);
    procedure SetSortAlphabetically(const AValue: boolean);
    procedure SetupComponents;
    procedure CreatePackageFileEditors;
    function OnTreeViewGetImageIndex({%H-}Str: String; Data: TObject; var {%H-}AIsEnabled: Boolean): Integer;
    procedure UpdateNodeImage(TVNode: TTreeNode);
    procedure UpdateNodeImage(TVNode: TTreeNode; NodeData: TPENodeData; Item: TObject);
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
    function PassesFilter(rec: PIDEOptionsGroupRec): Boolean;
    procedure TraverseSettings(AOptions: TAbstractPackageFileIDEOptions; anAction: TIDEPackageOptsDlgAction);
    procedure FileOptionsToGui;
    procedure GuiToFileOptions(Restore: boolean);
    procedure FileOptionsChange(Sender: TObject);
  protected
    fFlags: TPEFlags;
    procedure SetLazPackage(const AValue: TLazPackage); override;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function CanCloseEditor: TModalResult; override;
    procedure DoCompile(CompileClean, CompileRequired, WarnIDEPkg: boolean);
    procedure DoFindInFiles;
    procedure DoFixFilesCase;
    procedure DoShowMissingFiles;
    procedure DoMoveCurrentFile(Offset: integer);
    procedure DoMoveDependency(Offset: integer);
    procedure DoPublishPackage;
    procedure DoEditVirtualUnit;
    procedure DoExpandCollapseDirectory(ExpandIt: Boolean);
    procedure DoUseUnitsInDirectory(Use: boolean);
    procedure DoRevert;
    procedure DoSave(SaveAs: boolean);
    procedure DoSortFiles;
    function DoOpenPkgFile(PkgFile: TPkgFile): TModalResult;
    function ShowNewCompDialog: TModalResult;
    function ShowAddDepDialog: TModalResult;
    function ShowAddFPMakeDepDialog: TModalResult;
    function PkgNameToFormName(const PkgName: string): string;
    function GetSingleSelectedDependency: TPkgDependency;
    function GetSingleSelectedFile: TPkgFile;
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
    function IndexOfPackage(const PkgName: string): integer;
    function FindEditor(Pkg: TLazPackage): TPackageEditorForm; overload;
    function FindEditor(const PkgName: string): TPackageEditorForm; overload;
    function CreateEditor(Pkg: TLazPackage; DoDisableAutoSizing: boolean): TPackageEditorForm;
    function OpenEditor(Pkg: TLazPackage; BringToFront: boolean): TPackageEditorForm;
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

uses
  NewDialog;

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
  PkgEditMenuAddDiskFile:=RegisterIDEMenuCommand(AParent,'Add disk file',lisPckEditAddFilesFromFileSystem);
  PkgEditMenuAddNewFile:=RegisterIDEMenuCommand(AParent,'New file',lisA2PNewFile);
  PkgEditMenuAddNewComp:=RegisterIDEMenuCommand(AParent,'New component',lisMenuNewComponent);
  PkgEditMenuAddNewReqr:=RegisterIDEMenuCommand(AParent,'New requirement',lisProjAddNewRequirement);
  PkgEditMenuAddNewFPMakeReqr:=RegisterIDEMenuCommand(AParent,'New FPMake requirement',lisProjAddNewFPMakeRequirement);
  //
  PkgEditMenuOpenFile:=RegisterIDEMenuCommand(AParent,'Open File',lisOpen);
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
  PkgEditMenuOpenFolder:=RegisterIDEMenuCommand(AParent, 'Open folder', lisMenuOpenFolder);

  // register the section for operations on dependencies
  PkgEditMenuSectionDependency:=RegisterIDEMenuSection(PackageEditorMenuFilesRoot,'Dependency');
  AParent:=PkgEditMenuSectionDependency;
  PkgEditMenuRemoveDependency:=RegisterIDEMenuCommand(AParent,'Remove Dependency',lisPckEditRemoveDependency);
  PkgEditMenuReAddDependency:=RegisterIDEMenuCommand(AParent,'ReAdd Dependency',lisPckEditReAddDependency);
  PkgEditMenuDepStoreFileNameDefault:=RegisterIDEMenuCommand(AParent,'Dependency Store Filename As Default',lisPckEditStoreFileNameAsDefaultForThisDependency);
  PkgEditMenuDepStoreFileNamePreferred:=RegisterIDEMenuCommand(AParent,'Dependency Store Filename As Preferred',lisPckEditStoreFileNameAsPreferredForThisDependency);
  PkgEditMenuDepClearStoredFileName:=RegisterIDEMenuCommand(AParent,'Dependency Clear Stored Filename',lisPckEditClearDefaultPreferredFilenameOfDependency);
  PkgEditMenuCleanDependencies:=RegisterIDEMenuCommand(AParent, 'Clean up dependencies', lisPckEditCleanUpDependencies);

  // register the section for operations on all files
  PkgEditMenuSectionFiles:=RegisterIDEMenuSection(PackageEditorMenuRoot,'Files');
  AParent:=PkgEditMenuSectionFiles;
  PkgEditMenuFindInFiles:=RegisterIDEMenuCommand(AParent,'Find in files',srkmecFindInFiles + ' ...');
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

function DependencyAsString(ADependency: TPkgDependency): string;
// A display text for a dependency including min/max versions + other info.
// ToDo: Modify Dependency.AsString output to replace this function,
//       or create a new method for the task.
var
  aFilename: String;
begin
  Result:=ADependency.AsString;
  if ADependency.DefaultFilename<>'' then begin
    aFilename:=ADependency.MakeFilenameRelativeToOwner(ADependency.DefaultFilename);
    if ADependency.PreferDefaultFilename then
      Result:=Result+' in '+aFilename // like the 'in' keyword in uses section
    else
      Result:=Format(lisPckEditDefault, [Result, aFilename]);
  end;
  if ADependency.DependencyType=pdtFPMake then
    Result:=Result+' '+lisPckEditFPMakePackage;
end;

{ TPENodeData }

constructor TPENodeData.Create(aTyp: TPENodeType; aName: string; aRemoved: boolean);
begin
  Typ:=aTyp;
  Name:=aName;
  Removed:=aRemoved;
end;

{ TPackageEditorForm }

procedure TPackageEditorForm.PublishClick(Sender: TObject);
begin
  DoPublishPackage;
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
          if not CheckAddingPackageUnit(LazPackage,d2ptUnit,
            PackageEditors.OnGetIDEFileInfo,AFilename) then exit;
        end else if PkgFile.FileType=pftVirtualUnit then begin
          if not CheckAddingPackageUnit(LazPackage,d2ptVirtualUnit,
            PackageEditors.OnGetIDEFileInfo,AFilename) then exit;
        end else begin
          if not CheckAddingPackageUnit(LazPackage,d2ptFile,
            PackageEditors.OnGetIDEFileInfo,AFilename) then exit;
        end;
        PkgFile.Filename:=AFilename;
        LazPackage.UnremovePkgFile(PkgFile);
      end
      else if Item is TPkgDependency then begin
        Dependency:=TPkgDependency(Item);
        // Re-add dependency
        fForcedFlags:=[pefNeedUpdateRemovedFiles,pefNeedUpdateRequiredPkgs];
        if CheckAddingPackageDependency(LazPackage,Dependency,false,true)<>mrOk then exit;
        LazPackage.RemoveRemovedDependency(Dependency);
        PackageGraph.AddDependencyToPackage(LazPackage,Dependency);
      end;
    end;
    LazPackage.Modified:=True;
  finally
    EndUpdate;
  end;
end;

type
  PackageSelType = (pstFile, pstDir, pstDep, pstFPMake, pstFilesNode,
                     pstReqPackNode, pstRemFile, pstRemDep);
  PackageSelTypes = set of PackageSelType;

procedure TPackageEditorForm.ItemsPopupMenuPopup(Sender: TObject);
var
  UserSelection: PackageSelTypes;
  SingleSelectedFile: TPkgFile;
  SingleSelectedDep: TPkgDependency;

  procedure CollectSelected;
  var
    TVNode: TTreeNode;
    NodeData: TPENodeData;
    Item: TObject;
    CurDependency: TPkgDependency;
    CurFile: TPkgFile;
    i: Integer;
  begin
    UserSelection := [];
    SingleSelectedFile := Nil;
    SingleSelectedDep := Nil;
    for i := 0 to ItemsTreeView.SelectionCount-1 do begin
      TVNode := ItemsTreeView.Selections[i];
      if GetNodeDataItem(TVNode,NodeData,Item) then begin
        if Item is TPkgFile then begin
          CurFile := TPkgFile(Item);
          if ItemsTreeView.SelectionCount=1 then
            SingleSelectedFile := CurFile;
          if NodeData.Removed then
            Include(UserSelection, pstRemFile)
          else
            Include(UserSelection, pstFile);
        end else if Item is TPkgDependency then begin
          CurDependency := TPkgDependency(Item);
          if (ItemsTreeView.SelectionCount=1) and Assigned(CurDependency.RequiredPackage) then
            SingleSelectedDep:=CurDependency;
          if CurDependency.DependencyType=pdtFPMake then
            Include(UserSelection, pstFPMake);
          if NodeData.Removed then
            Include(UserSelection, pstRemDep)
          else
            Include(UserSelection, pstDep);
        end;
      end
      else if IsDirectoryNode(TVNode) then
        Include(UserSelection, pstDir)
      else if TVNode=FFilesNode then
        Include(UserSelection, pstFilesNode)
      else if TVNode=FRequiredPackagesNode then
        Include(UserSelection, pstReqPackNode);
    end;
  end;

  procedure SetItem(Item: TIDEMenuCommand; AnOnClick: TNotifyEvent;
                    aShow: boolean = true; AEnable: boolean = true);
  begin
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
    PkgEditMenuSectionFileType.Visible:=Assigned(SingleSelectedFile);
    if not PkgEditMenuSectionFileType.Visible then Exit;
    PkgEditMenuSectionFileType.Clear;
    VirtualFileExists:=(SingleSelectedFile.FileType=pftVirtualUnit)
                    and FileExistsCached(SingleSelectedFile.GetFullFilename);
    for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
      NewMenuItem:=RegisterIDEMenuCommand(PkgEditMenuSectionFileType,
                      'SetFileType'+IntToStr(ord(CurPFT)),
                      GetPkgFileTypeLocalizedName(CurPFT),
                      @ChangeFileTypeMenuItemClick);
      if CurPFT=SingleSelectedFile.FileType then
      begin
        // menuitem to keep the current type
        NewMenuItem.Enabled:=true;
        NewMenuItem.Checked:=true;
      end else if VirtualFileExists then
        // a virtual unit that exists can be changed into anything
        NewMenuItem.Enabled:=true
      else if not (CurPFT in PkgFileUnitTypes) then
        // all other files can be changed into all non unit types
        NewMenuItem.Enabled:=true
      else if FilenameIsPascalUnit(SingleSelectedFile.Filename) then
        // a pascal file can be changed into anything
        NewMenuItem.Enabled:=true
      else
        // default is to not allow
        NewMenuItem.Enabled:=false;
    end;
  end;

var
  Writable: Boolean;
begin
  //debugln(['TPackageEditorForm.FilesPopupMenuPopup START ',ItemsPopupMenu.Items.Count]);
  PackageEditorMenuFilesRoot.MenuItem:=ItemsPopupMenu.Items;
  //debugln(['TPackageEditorForm.FilesPopupMenuPopup START after connect ',ItemsPopupMenu.Items.Count]);
  //PackageEditorMenuRoot.BeginUpdate;
  try
    CollectSelected;
    Writable := not LazPackage.ReadOnly;

    // items for Files node and for selected files, under section PkgEditMenuSectionFile
    PkgEditMenuSectionFile.Visible := not (pstDir in UserSelection);
    if PkgEditMenuSectionFile.Visible then
    begin
      // Files root node
      SetItem(PkgEditMenuAddDiskFile, @mnuAddDiskFileClick, UserSelection=[pstFilesNode],
              Writable);
      SetItem(PkgEditMenuAddNewFile, @mnuAddNewFileClick, UserSelection=[pstFilesNode],
              Writable);
      SetItem(PkgEditMenuAddNewComp, @mnuAddNewCompClick, UserSelection=[pstFilesNode],
              Writable);
      // Required packages root node
      SetItem(PkgEditMenuAddNewReqr, @mnuAddNewReqrClick, UserSelection=[pstReqPackNode],
              Writable);
      SetItem(PkgEditMenuAddNewFPMakeReqr, @mnuAddFPMakeReqClick, UserSelection=[pstReqPackNode],
              Writable);
      // selected files
      SetItem(PkgEditMenuOpenFile, @OpenFileMenuItemClick,
              UserSelection*[pstFilesNode,pstReqPackNode,pstFPMake]=[]);
      SetItem(PkgEditMenuRemoveFile, @RemoveBitBtnClick,
              UserSelection=[pstFile], RemoveBitBtn.Enabled);
      SetItem(PkgEditMenuReAddFile, @ReAddMenuItemClick,
              UserSelection=[pstRemFile]);
      SetItem(PkgEditMenuCopyMoveToDirectory, @CopyMoveToDirMenuItemClick,
              (UserSelection=[pstFile]) and LazPackage.HasDirectory);
      AddFileTypeMenuItem;
      SetItem(PkgEditMenuEditVirtualUnit, @EditVirtualUnitMenuItemClick,
              Assigned(SingleSelectedFile) and (SingleSelectedFile.FileType=pftVirtualUnit),
              Writable);
    end;

    // items for directories, under section PkgEditMenuSectionDirectory
    PkgEditMenuSectionDirectory.Visible := UserSelection<=[pstDir,pstFilesNode];
    if PkgEditMenuSectionDirectory.Visible then
    begin
      SetItem(PkgEditMenuExpandDirectory, @ExpandDirectoryMenuItemClick);
      SetItem(PkgEditMenuCollapseDirectory, @CollapseDirectoryMenuItemClick);
      SetItem(PkgEditMenuUseAllUnitsInDirectory, @UseAllUnitsInDirectoryMenuItemClick);
      SetItem(PkgEditMenuUseNoUnitsInDirectory, @UseNoUnitsInDirectoryMenuItemClick);
      SetItem(PkgEditMenuOpenFolder, @OpenFolderMenuItemClick);
    end;

    // items for dependencies, under section PkgEditMenuSectionDependency
    PkgEditMenuSectionDependency.Visible := (UserSelection*[pstDep,pstRemDep] <> [])
                                  or (ItemsTreeView.Selected=FRequiredPackagesNode);
    if PkgEditMenuSectionDependency.Visible then
    begin
      SetItem(PkgEditMenuRemoveDependency, @RemoveBitBtnClick,
              pstdep in UserSelection, Writable);
      SetItem(PkgEditMenuReAddDependency,@ReAddMenuItemClick,
              pstRemDep in UserSelection, Writable);
      SetItem(PkgEditMenuDepStoreFileNameDefault, @SetDepDefaultFilenameMenuItemClick,
              Assigned(SingleSelectedDep), Writable);
      SetItem(PkgEditMenuDepStoreFileNamePreferred, @SetDepPreferredFilenameMenuItemClick,
              Assigned(SingleSelectedDep), Writable);
      SetItem(PkgEditMenuDepClearStoredFileName, @ClearDependencyFilenameMenuItemClick,
              Assigned(SingleSelectedDep), Writable);
      SetItem(PkgEditMenuCleanDependencies, @CleanDependenciesMenuItemClick,
              Assigned(LazPackage.FirstRequiredDependency), Writable);
    end;

  finally
    //PackageEditorMenuRoot.EndUpdate;
    SingleSelectedFile := Nil;
    SingleSelectedDep := Nil;
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

procedure TPackageEditorForm.ItemsTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  PackageEditors.OnDragDropTreeView(Sender, Source, X, Y);
end;

procedure TPackageEditorForm.ItemsTreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  TargetTVNode: TTreeNode;
  TargetTVType: TTreeViewInsertMarkType;
begin
  //debugln(['TPackageEditorForm.ItemsTreeViewDragOver ',DbgSName(Source),' State=',ord(State),' FromSelf=',Source=ItemsTreeView]);
  Accept:=PackageEditors.OnDragOverTreeView(Sender,Source,X,Y,TargetTVNode,TargetTVType);
  if Accept and (State<>dsDragLeave) then
    ItemsTreeView.SetInsertMark(TargetTVNode,TargetTVType)
  else
    ItemsTreeView.SetInsertMark(nil,tvimNone);
end;

procedure TPackageEditorForm.MorePopupMenuPopup(Sender: TObject);

  procedure SetItem(Item: TIDEMenuCommand; AnOnClick: TNotifyEvent;
                    aShow: boolean = true; AEnable: boolean = true);
  begin
    //debugln(['SetItem ',Item.Caption,' Visible=',aShow,' Enable=',AEnable]);
    Item.OnClick:=AnOnClick;
    Item.Visible:=aShow;
    Item.Enabled:=AEnable;
  end;

var
  Writable, CanPublish: Boolean;
  pcos: TParsedCompilerOptString;
  CurrentPath: String;
begin
  PackageEditorMenuRoot.MenuItem:=MorePopupMenu.Items;
  //PackageEditorMenuRoot.BeginUpdate;
  try
    Writable:=(not LazPackage.ReadOnly);

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
    CanPublish:=(not LazPackage.Missing) and LazPackage.HasDirectory;
    for pcos in [pcosUnitPath,pcosIncludePath] do
    begin
      CurrentPath:=LazPackage.CompilerOptions.ParsedOpts.GetParsedValue(pcos);
      CurrentPath:=CreateRelativeSearchPath(CurrentPath,LazPackage.DirectoryExpanded);
      //debugln(['TPackageEditorForm.MorePopupMenuPopup Unit=',CurrentPath]);
      if Pos('..',CurrentPath)>0 then
        CanPublish:=false;
    end;
    SetItem(PkgEditMenuPublish,@PublishClick,true,CanPublish);

    // under section PkgEditMenuSectionCompile
    SetItem(PkgEditMenuCompile,@CompileBitBtnClick,true,CompileBitBtn.Enabled);
    SetItem(PkgEditMenuRecompileClean,@CompileCleanClick,true,CompileBitBtn.Enabled);
    SetItem(PkgEditMenuRecompileAllRequired,@CompileAllCleanClick,true,CompileBitBtn.Enabled);
    SetItem(PkgEditMenuCreateFpmakeFile,@CreateFpmakeFileClick,true,CompileBitBtn.Enabled);
    SetItem(PkgEditMenuCreateMakefile,@CreateMakefileClick,true,CompileBitBtn.Enabled);

    // under section PkgEditMenuSectionMisc
    SetItem(PkgEditMenuViewPackageSource,@ViewPkgSourceClick);
  finally
    //PackageEditorMenuRoot.EndUpdate;
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
    mnuAddDiskFileClick(Nil)
  else
    Handled := False;

  if Handled then
    Key := VK_UNKNOWN;
end;

procedure TPackageEditorForm.mnuAddFPMakeReqClick(Sender: TObject);
begin
  ShowAddFPMakeDepDialog
end;

procedure TPackageEditorForm.mnuAddNewCompClick(Sender: TObject);
begin
  ShowNewCompDialog;
end;

procedure TPackageEditorForm.mnuAddNewReqrClick(Sender: TObject);
begin
  ShowAddDepDialog;
end;

procedure TPackageEditorForm.mnuAddNewFileClick(Sender: TObject);
var
  NewItem: TNewIDEItemTemplate;
begin
  // Reuse the "New..." dialog for "Add new file".
  if ShowNewIDEItemDialog(NewItem, true)=mrOk then
    DoAddNewFile(NewItem);
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

procedure TPackageEditorForm.SetDepDefaultFilenameMenuItemClick(Sender: TObject);
begin
  SetDependencyDefaultFilename(false);
end;

procedure TPackageEditorForm.SetDepPreferredFilenameMenuItemClick(Sender: TObject);
begin
  SetDependencyDefaultFilename(true);
end;

procedure TPackageEditorForm.ClearDependencyFilenameMenuItemClick(Sender: TObject);
var
  CurDependency: TPkgDependency;
begin
  CurDependency:=GetSingleSelectedDependency;
  if CurDependency=nil then exit;
  if (LazPackage=nil) or LazPackage.ReadOnly then exit;
  if CurDependency.DefaultFilename='' then exit;
  CurDependency.DefaultFilename:='';
  CurDependency.PreferDefaultFilename:=false;
  LazPackage.Modified:=true;
  UpdateRequiredPkgs;
end;

procedure TPackageEditorForm.CollapseDirectoryMenuItemClick(Sender: TObject);
begin
  DoExpandCollapseDirectory(False);
end;

procedure TPackageEditorForm.MoveUpBtnClick(Sender: TObject);
begin
  if SortAlphabetically then exit;
  if Assigned(GetSingleSelectedFile) then
    DoMoveCurrentFile(-1)
  else if Assigned(GetSingleSelectedDependency) then
    DoMoveDependency(-1);
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
  if Assigned(GetSingleSelectedFile) then
    DoMoveCurrentFile(1)
  else if Assigned(GetSingleSelectedDependency) then
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
begin
  CanClose:=CanCloseEditor=mrOK;
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
  IconWidth: Integer;
  IconHeight: Integer;
  IL: TCustomImageList;
  II: TImageIndex;
  Res: TScaledImageListResolution;
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
      IL:=CurComponent.Images;
      II:=CurComponent.ImageIndex;
      //DebugLn('TPackageEditorForm.RegisteredListBoxDrawItem ',DbgSName(CurIcon),' ',CurComponent.ComponentClass.ClassName);
      if (IL<>nil) and (II>=0) then begin
        Res := IL.ResolutionForControl[0, Self];
        IconWidth:=Res.Width;
        IconHeight:=Res.Height;
        Res.Draw(RegisteredListBox.Canvas,
             ARect.Left+(25-IconWidth) div 2,
             ARect.Top+(ARect.Bottom-ARect.Top-IconHeight) div 2,
             II);
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
    LazPackage.Modified:=True;

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
  DoExpandCollapseDirectory(True);
end;

procedure TPackageEditorForm.FilterEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    OpenFileMenuItemClick(Nil);
    Key := VK_UNKNOWN;
  end;
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
      LazPackage.AddFileByName(FileNames[i], NewUnitPaths, NewIncPaths);
    //UpdateAll(false);
    // extend unit and include search path
    if not LazPackage.ExtendUnitSearchPath(NewUnitPaths) then exit;
    if not LazPackage.ExtendIncSearchPath(NewIncPaths) then exit;
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

procedure TPackageEditorForm.UseAllUnitsInDirectoryMenuItemClick(Sender: TObject);
begin
  DoUseUnitsInDirectory(true);
end;

procedure TPackageEditorForm.UseNoUnitsInDirectoryMenuItemClick(Sender: TObject);
begin
  DoUseUnitsInDirectory(false);
end;

procedure TPackageEditorForm.OpenFolderMenuItemClick(Sender: TObject);
begin
  OpenDocument(LazPackage.Directory);
end;

procedure TPackageEditorForm.mnuAddDiskFileClick(Sender: TObject);
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
    OpenDialog.Filter:=
      dlgFilterAll+' ('+GetAllFilesMask+')|'+GetAllFilesMask
      +'|'+dlgFilterLazarusUnit+' (*.pas;*.pp)|*.pas;*.pp'
      +'|'+dlgFilterLazarusProject+' (*.lpi)|*.lpi'
      +'|'+dlgFilterLazarusForm+' (*.lfm;*.dfm)|*.lfm;*.dfm'
      +'|'+dlgFilterLazarusPackage+' (*.lpk)|*.lpk'
      +'|'+dlgFilterLazarusProjectSource+' (*.lpr)|*.lpr';
    if OpenDialog.Execute then begin
      InputHistories.StoreFileDialogSettings(OpenDialog);
      NewUnitPaths:='';
      NewIncPaths:='';
      for i:=0 to OpenDialog.Files.Count-1 do
        LazPackage.AddFileByName(OpenDialog.Files[i], NewUnitPaths, NewIncPaths);
      //UpdateAll(false);
      // extend unit and include search path
      if not LazPackage.ExtendUnitSearchPath(NewUnitPaths) then exit;
      if not LazPackage.ExtendIncSearchPath(NewIncPaths) then exit;
    end;
  finally
    OpenDialog.Free;
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
  CurDependency: TPkgDependency;
begin
  CurDependency:=GetSingleSelectedDependency;
  if (LazPackage=nil) or (CurDependency=nil)
  then exit;

  MinVers:=TPkgVersion.Create;
  MaxVers:=TPkgVersion.Create;
  try
    // Assign relevant data to temp variables
    Flags:=CurDependency.Flags;
    MinVers.Assign(CurDependency.MinVersion);
    MaxVers.Assign(CurDependency.MinVersion);

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
    CurDependency.Flags := Flags;
    CurDependency.MinVersion.Assign(MinVers);
    CurDependency.MaxVersion.Assign(MaxVers);

    UpdateNodeImage(ItemsTreeView.Selected);
    //fForcedFlags:=[pefNeedUpdateRequiredPkgs];
    LazPackage.Modified:=True;
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
    UpdateNodeImage(TVNode, NodeData, Item);
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
  DoCompile(true,true,true);
end;

procedure TPackageEditorForm.CompileCleanClick(Sender: TObject);
begin
  DoCompile(true,false,true);
end;

procedure TPackageEditorForm.CopyMoveToDirMenuItemClick(Sender: TObject);
begin
  PackageEditors.OnCopyMoveFiles(Self);
end;

procedure TPackageEditorForm.CompileBitBtnClick(Sender: TObject);
begin
  DoCompile(false,false,true);
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
  //force editor name change when package name changed!
  if (FLazPackage=Nil)
  and ( (AValue=Nil) or (Name=PkgNameToFormName(AValue.Name)) )
  then
    exit;
  if FLazPackage<>nil then
  begin
    FLazPackage.Editor:=nil;
    if EnvironmentOptions.LastOpenPackages.Remove(FLazPackage.Filename) then
      MainIDE.SaveEnvironment;
  end;
  FLazPackage:=AValue;
  if FLazPackage=nil then begin
    Name:=Name+'___off___';
    exit;
  end;
  EnvironmentOptions.LastOpenPackages.Add(FLazPackage.Filename);
  MainIDE.SaveEnvironment;
  FLazPackage.Editor:=Self;
  // set Name and update components.
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
      Result.ImageIndex := IDEImages.LoadImage(AImageName);
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
  ImageIndexFiles           := IDEImages.LoadImage('pkg_files');
  ImageIndexRemovedFiles    := IDEImages.LoadImage('pkg_removedfiles');
  ImageIndexRequired        := IDEImages.LoadImage('pkg_required');
  ImageIndexRemovedRequired := IDEImages.LoadImage('pkg_removedrequired');
  ImageIndexUnit            := IDEImages.LoadImage('pkg_unit');
  ImageIndexRegisterUnit    := IDEImages.LoadImage('pkg_registerunit');
  ImageIndexLFM             := IDEImages.LoadImage('pkg_lfm');
  ImageIndexLRS             := IDEImages.LoadImage('pkg_lrs');
  ImageIndexInclude         := IDEImages.LoadImage('pkg_include');
  ImageIndexIssues          := IDEImages.LoadImage('pkg_issues');
  ImageIndexText            := IDEImages.LoadImage('pkg_text');
  ImageIndexBinary          := IDEImages.LoadImage('pkg_binary');
  ImageIndexConflict        := IDEImages.LoadImage('pkg_conflict');
  ImageIndexDirectory       := IDEImages.LoadImage('pkg_files');

  ItemsTreeView.Images := IDEImages.Images_16;
  ToolBar.Images := IDEImages.Images_16;
  FilterEdit.OnGetImageIndex:=@OnTreeViewGetImageIndex;

  SaveBitBtn    := CreateToolButton('SaveBitBtn', lisMenuSave, lisPckEditSavePackage, 'laz_save', @SaveBitBtnClick);
  CompileBitBtn := CreateToolButton('CompileBitBtn', lisCompile, lisPckEditCompilePackage, 'pkg_compile', @CompileBitBtnClick);
  UseBitBtn     := CreateToolButton('UseBitBtn', lisUse, lisClickToSeeTheChoices, 'pkg_install', nil);
  UseBitBtn.Style:=tbsButtonDrop;
  CreateDivider;
  AddBitBtn     := CreateToolButton('AddBitBtn', lisAdd, lisClickToSeeTheChoices, 'laz_add', nil);
  AddBitBtn.Style:=tbsButtonDrop;
  RemoveBitBtn  := CreateToolButton('RemoveBitBtn', lisRemove, lisPckEditRemoveSelectedItem, 'laz_delete', @RemoveBitBtnClick);
  CreateDivider;
  OptionsBitBtn := CreateToolButton('OptionsBitBtn', lisOptions, lisPckEditEditGeneralOptions, 'pkg_properties', @OptionsBitBtnClick);
  HelpBitBtn    := CreateToolButton('HelpBitBtn', GetButtonCaption(idButtonHelp), lisMenuOnlineHelp, 'btn_help', @HelpBitBtnClick);
  MoreBitBtn    := CreateToolButton('MoreBitBtn', lisMoreSub, lisPkgEdMoreFunctionsForThePackage, '', nil);
  MoreBitBtn.Style:=tbsButtonDrop;

  UseBitBtn.DropdownMenu := UsePopupMenu;
  AddBitBtn.DropdownMenu := AddPopupMenu;
  MoreBitBtn.DropdownMenu := MorePopupMenu;

  mnuAddDiskFile.Caption := lisPckEditAddFilesFromFileSystem;
  mnuAddNewFile.Caption := lisA2PNewFile;
  mnuAddNewComp.Caption := lisMenuNewComponent;
  mnuAddNewReqr.Caption := lisProjAddNewRequirement;
  mnuAddFPMakeReq.Caption := lisProjAddNewFPMakeRequirement;

  // Buttons on FilterPanel
  IDEImages.AssignImage(OpenButton, 'laz_open');
  OpenButton.Hint:=lisOpenFile2;
  SortAlphabeticallyButton.Hint:=lisPESortFilesAlphabetically;
  IDEImages.AssignImage(SortAlphabeticallyButton, 'pkg_sortalphabetically');
  DirectoryHierarchyButton.Hint:=lisPEShowDirectoryHierarchy;
  IDEImages.AssignImage(DirectoryHierarchyButton, 'pkg_hierarchical');

  // Up / Down buttons
  IDEImages.AssignImage(MoveUpBtn, 'arrow_up');
  IDEImages.AssignImage(MoveDownBtn, 'arrow_down');
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
  CommonOptionsTabSheet.Caption:=lisPckEditCommonOptions;

  CallRegisterProcCheckBox.Caption:=lisPckEditRegisterUnit;
  CallRegisterProcCheckBox.Hint:=Format(lisPckEditCallRegisterProcedureOfSelectedUnit, ['"', '"']);

  AddToUsesPkgSectionCheckBox.Caption:=lisPkgMangUseUnit;
  AddToUsesPkgSectionCheckBox.Hint:=lisPkgMangAddUnitToUsesClause;

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
    Parent:=CommonOptionsTabSheet;
  end;

  CreatePackageFileEditors;
end;

procedure TPackageEditorForm.SetDependencyDefaultFilename(AsPreferred: boolean);
var
  NewFilename: String;
  CurDependency: TPkgDependency;
begin
  if LazPackage=nil then exit;
  CurDependency:=GetSingleSelectedDependency;
  if CurDependency=nil then begin
    debugln(['Info: [TPackageEditorForm.SetDependencyDefaultFilename] CurDependency=nil']);
    exit;
  end;
  if LazPackage.ReadOnly then begin
    debugln(['Info: [TPackageEditorForm.SetDependencyDefaultFilename] ReadOnly']);
    exit;
  end;
  if CurDependency.RequiredPackage=nil then begin
    debugln(['Info: [TPackageEditorForm.SetDependencyDefaultFilename] RequiredPackage=nil']);
    exit;
  end;
  NewFilename:=CurDependency.RequiredPackage.Filename;
  if (NewFilename=CurDependency.DefaultFilename)
  and (CurDependency.PreferDefaultFilename=AsPreferred) then begin
    debugln(['Info: [TPackageEditorForm.SetDependencyDefaultFilename] PreferDefaultFilename=AsPreferred']);
    exit;
  end;
  BeginUpdate;
  try
    CurDependency.DefaultFilename:=NewFilename;
    CurDependency.PreferDefaultFilename:=AsPreferred;
    LazPackage.Modified:=true;
    UpdateRequiredPkgs;
    debugln(['Info: TPackageEditorForm.SetDependencyDefaultFilename ',CurDependency.PackageName,' DefaultFilename:=',NewFilename,' AsPreferred=',AsPreferred]);
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
  Name:=PkgNameToFormName(LazPackage.Name);
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
  if Immediately then
    UpdatePending
  else
    IdleConnected:=true;
end;

procedure TPackageEditorForm.DoAddNewFile(NewItem: TNewIDEItemTemplate);
var
  NewFilename: String;
  DummyResult: TModalResult;
  NewFileType: TPkgFileType;
  NewPkgFileFlags: TPkgFileFlags;
  Desc: TProjectFileDescriptor;
  NewUnitName: String;
  HasRegisterProc: Boolean;
begin
  if NewItem is TNewItemProjectFile then begin
    // create new file
    Desc:=TNewItemProjectFile(NewItem).Descriptor;
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

function TPackageEditorForm.ShowNewCompDialog: TModalResult;
var
  IgnoreUnitPaths: TFilenameToStringTree;

  function PkgDependsOn(PkgName: string): boolean;
  begin
    if PkgName='' then exit(false);
    Result:=PackageGraph.FindDependencyRecursively(LazPackage.FirstRequiredDependency,PkgName)<>nil;
  end;

  procedure AddNewComponent(AddParams: TAddToPkgResult);
  begin
    ExtendUnitIncPathForNewUnit(AddParams.UnitFilename, '', IgnoreUnitPaths);
    // add file
    with AddParams do
    begin
      Assert(FilenameIsAbsolute(UnitFilename), 'AddNewComponent: Filename is relative.');
      // This file can also replace an existing file.
      if LazPackage.FindPkgFile(UnitFilename,true,false)=nil then
        LazPackage.AddFile(UnitFilename, Unit_Name, FileType, PkgFileFlags, cpNormal)
      else
        LazPackage.Modified:=True;
      FreeAndNil(FNextSelectedPart);
      FNextSelectedPart:=TPENodeData.Create(penFile, UnitFilename, false);
      PackageEditors.DeleteAmbiguousFiles(LazPackage, UnitFilename);
    end;
    // open file in editor
    PackageEditors.CreateNewFile(Self,AddParams);
  end;

var
  AddParams, OldParams: TAddToPkgResult;
begin
  if LazPackage.ReadOnly then begin
    UpdateButtons;
    exit(mrCancel);
  end;

  Result:=ShowAddToPackageDlg(LazPackage, AddParams);
  if Result<>mrOk then exit;

  PackageGraph.BeginUpdate(false);
  IgnoreUnitPaths:=nil;
  try
    while AddParams<>nil do begin
      AddNewComponent(AddParams);
      OldParams:=AddParams;
      AddParams:=AddParams.Next;
      OldParams.Next:=nil;
      OldParams.Free;
    end;
    AddParams.Free;
    Assert(LazPackage.Modified, 'TPackageEditorForm.ShowAddDialog: LazPackage.Modified = False');
  finally
    IgnoreUnitPaths.Free;
    PackageGraph.EndUpdate;
  end;
end;

function TPackageEditorForm.ShowAddDepDialog: TModalResult;
var
  Deps: TPkgDependencyList;
  i: Integer;
begin
  if LazPackage.ReadOnly then begin
    UpdateButtons;
    exit(mrCancel);
  end;
  Result:=ShowAddPkgDependencyDlg(LazPackage, Deps);
  try
    if (Result<>mrOk) or (Deps.Count=0) then exit;
    PackageGraph.BeginUpdate(false);
    try
      // add all dependencies
      fForcedFlags := [pefNeedUpdateRequiredPkgs];
      FreeAndNil(FNextSelectedPart);
      for i := 0 to Deps.Count-1 do
        PackageGraph.AddDependencyToPackage(LazPackage, Deps[i]);
      FNextSelectedPart := TPENodeData.Create(penDependency,
                                            Deps[Deps.Count-1].PackageName, false);
      Assert(LazPackage.Modified, 'TPackageEditorForm.ShowAddDepDialog: LazPackage.Modified = False');
    finally
      PackageGraph.EndUpdate;
    end;
  finally
    Deps.Free;
  end;
end;

function TPackageEditorForm.ShowAddFPMakeDepDialog: TModalResult;
var
  Deps: TPkgDependencyList;
  i: Integer;
begin
  if LazPackage.ReadOnly then begin
    UpdateButtons;
    exit(mrCancel);
  end;
  Result:=ShowAddFPMakeDependencyDlg(LazPackage, Deps);
  try
    if (Result<>mrOk) or (Deps.Count=0) then exit;
    PackageGraph.BeginUpdate(false);
    try
      // add all dependencies
      fForcedFlags := [pefNeedUpdateRequiredPkgs];
      FreeAndNil(FNextSelectedPart);
      for i := 0 to Deps.Count-1 do
        PackageGraph.AddDependencyToPackage(LazPackage, Deps[i]);
      FNextSelectedPart := TPENodeData.Create(penDependency,
                                            Deps[Deps.Count-1].PackageName, false);
      Assert(LazPackage.Modified, 'TPackageEditorForm.ShowAddFPMakeDepDialog: LazPackage.Modified = False');
    finally
      PackageGraph.EndUpdate;
    end;
  finally
    Deps.Free;
  end;
end;

function TPackageEditorForm.PkgNameToFormName(const PkgName: string): string;
begin
  Result:=PackageEditorWindowPrefix+StringReplace(PkgName,'.','_',[rfReplaceAll]);
end;

procedure TPackageEditorForm.BeginUpdate;
begin
  inc(fUpdateLock);
end;

procedure TPackageEditorForm.EndUpdate;
begin
  if fUpdateLock=0 then
    RaiseGDBException('');
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

procedure TPackageEditorForm.UpdateNodeImage(TVNode: TTreeNode);
var
  NodeData: TPENodeData;
  Item: TObject;
begin
  if GetNodeDataItem(TVNode, NodeData, Item) then
    UpdateNodeImage(TVNode, NodeData, Item);
end;

procedure TPackageEditorForm.UpdateNodeImage(TVNode: TTreeNode;
  NodeData: TPENodeData; Item: TObject);
var
  PkgDependency: TPkgDependency;
  ImgIndex: Integer;
  Ena: Boolean;
begin
  Assert(Assigned(Item), 'TPackageEditorForm.UpdateNodeImage: Item = Nil.');
  if Item is TPkgDependency then begin
    PkgDependency:=TPkgDependency(Item);
    // Try to load the package again. Min/max version may have changed.
    PkgDependency.LoadPackageResult := lprUndefined;
    PackageGraph.OpenDependency(PkgDependency, False);
  end;
  Ena := True;                      // Neither Ena nor the String param are used.
  ImgIndex := OnTreeViewGetImageIndex('', NodeData, Ena);
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
  OldFilter: String;
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
    NodeData:=CreateNodeData(penDependency,CurDependency.PackageName,false);
    if (FNextSelectedPart<>nil) and (FNextSelectedPart.Typ=penDependency)
    and (FNextSelectedPart.Name=NodeData.Name)
    then
      FilterEdit.SelectedPart:=NodeData;
    RequiredBranch.AddNodeData(DependencyAsString(CurDependency), NodeData);
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
      RemovedBranch.AddNodeData(DependencyAsString(CurDependency), NodeData);
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
  CurFile, SingleSelectedFile: TPkgFile;
  CurDependency, SingleSelectedDep: TPkgDependency;
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
  SingleSelectedDirectory, SingleSelectedNode: TTreeNode;
  SingleSelectedRemoved: Boolean;
  FileCount: integer;
  HasRegisterProcCount: integer;
  AddToUsesPkgSectionCount: integer;
begin
  if not CanUpdate(pefNeedUpdateProperties,Immediately) then exit;

  GuiToFileOptions(False);

  FPlugins.Clear;

  // check selection
  SingleSelectedNode:=nil;
  SingleSelectedDep:=nil;
  SingleSelectedFile:=nil;
  SingleSelectedDirectory:=nil;
  SingleSelectedRemoved:=false;
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
        SingleSelectedFile:=CurFile;
        SingleSelectedNode:=TVNode;
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
        SingleSelectedDep:=CurDependency;
        SingleSelectedNode:=TVNode;
        SingleSelectedRemoved:=NodeData.Removed;
      end;
    end else if IsDirectoryNode(TVNode) or (TVNode=FFilesNode) then begin
      inc(SelDirCount);
      SingleSelectedDirectory:=TVNode;
      SingleSelectedNode:=TVNode;
    end;
  end;

  if (SelFileCount+SelDepCount+SelDirCount>1) then begin
    // it is a multi selection
    SingleSelectedFile:=nil;
    SingleSelectedDep:=nil;
    SingleSelectedDirectory:=nil;
    SingleSelectedNode:=nil;
  end;
  OnlyFilesSelected:=(SelFileCount>0) and (SelDepCount=0) and (SelDirCount=0);
  OnlyFilesWithUnitsSelected:=OnlyFilesSelected and (SelUnitCount>0);

  //debugln(['TPackageEditorForm.UpdatePEProperties SelFileCount=',SelFileCount,' SelDepCount=',SelDepCount,' SelDirCount=',SelDirCount,' SelUnitCount=',SelUnitCount]);
  //debugln(['TPackageEditorForm.UpdatePEProperties FSingleSelectedFile=',SingleSelectedFile<>nil,' FSingleSelectedDependency=',SingleSelectedDep<>nil,' SingleSelectedDirectory=',SingleSelectedDirectory<>nil]);

  DisableAlign;
  try
    // move up/down (only single selection)
    aVisible:=(not (SortAlphabetically or SingleSelectedRemoved))
       and ((SingleSelectedFile<>nil) or (SingleSelectedDep<>nil));
    MoveUpBtn.Enabled  :=aVisible and Assigned(SingleSelectedNode.GetPrevVisibleSibling);
    MoveDownBtn.Enabled:=aVisible and Assigned(SingleSelectedNode.GetNextVisibleSibling);

    // Min/Max version of dependency (only single selection)
    aVisible:=SingleSelectedDep<>nil;
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
    else if SingleSelectedDep<>nil then begin
      PropsGroupBox.Enabled:=not SingleSelectedRemoved;
      PropsGroupBox.Caption:=lisPckEditDependencyProperties;
      UseMinVersionCheckBox.Checked:=pdfMinVersion in SingleSelectedDep.Flags;
      MinVersionEdit.Text:=SingleSelectedDep.MinVersion.AsString;
      MinVersionEdit.Enabled:=pdfMinVersion in SingleSelectedDep.Flags;
      UseMaxVersionCheckBox.Checked:=pdfMaxVersion in SingleSelectedDep.Flags;
      MaxVersionEdit.Text:=SingleSelectedDep.MaxVersion.AsString;
      MaxVersionEdit.Enabled:=pdfMaxVersion in SingleSelectedDep.Flags;
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

    if SingleSelectedFile<>nil then begin
      for i := 2 to PropsPageControl.PageCount -1 do
        PropsPageControl.pages[i].Visible := True;
      FileOptionsToGui;
      PropsPageControl.ShowTabs := PropsPageControl.PageCount > 1;
    end else begin
      for i := 2 to PropsPageControl.PageCount -1 do
        PropsPageControl.pages[i].Visible := False;
      PropsPageControl.ShowTabs := False;
    end;
  finally
    EnableAlign;
  end;
end;

function TPackageEditorForm.GetSingleSelectedDependency: TPkgDependency;
var
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
begin
  Result:=nil;
  for i:=0 to ItemsTreeView.SelectionCount-1 do begin
    TVNode:=ItemsTreeView.Selections[i];
    if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
    if Item is TPkgFile then begin
      Result:=nil;
      break;
    end else if Item is TPkgDependency then begin
      if Result<>nil then begin
        // not single selected
        Result:=nil;
        break;
      end;
      Result:=TPkgDependency(Item);
    end;
  end;
end;

function TPackageEditorForm.GetSingleSelectedFile: TPkgFile;
var
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
begin
  Result:=nil;
  for i:=0 to ItemsTreeView.SelectionCount-1 do begin
    TVNode:=ItemsTreeView.Selections[i];
    if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
    if Item is TPkgFile then begin
      if Result<>nil then begin
        // not single selected
        Result:=nil;
        break;
      end;
      Result:=TPkgFile(Item);
      break;
    end else if Item is TPkgDependency then begin
      Result:=nil;
      break;
    end;
  end;
end;

procedure TPackageEditorForm.UpdateApplyDependencyButton(Immediately: boolean);
var
  DependencyChanged: Boolean;
  AVersion: TPkgVersion;
  CurDependency: TPkgDependency;
begin
  if not CanUpdate(pefNeedUpdateApplyDependencyButton,Immediately) then exit;
  CurDependency:=GetSingleSelectedDependency;
  DependencyChanged:=false;
  if (CurDependency<>nil) then begin
    // check min version
    if UseMinVersionCheckBox.Checked<>(pdfMinVersion in CurDependency.Flags) then
      DependencyChanged:=true;
    if UseMinVersionCheckBox.Checked then begin
      AVersion:=TPkgVersion.Create;
      if AVersion.ReadString(MinVersionEdit.Text)
      and (AVersion.Compare(CurDependency.MinVersion)<>0) then
        DependencyChanged:=true;
      AVersion.Free;
    end;
    // check max version
    if UseMaxVersionCheckBox.Checked<>(pdfMaxVersion in CurDependency.Flags) then
      DependencyChanged:=true;
    if UseMaxVersionCheckBox.Checked then begin
      AVersion:=TPkgVersion.Create;
      if AVersion.ReadString(MaxVersionEdit.Text)
      and (AVersion.Compare(CurDependency.MaxVersion)<>0) then
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
  ShortIncDirectory:='';
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
    LazPackage.CompilerOptions.MergeToUnitPaths(ShortDirectory);
  end;
  if IncPathPos<1 then
    // the unit is in unitpath, but the include file not in the incpath
    // -> auto extend the include path
    LazPackage.CompilerOptions.MergeToIncludePaths(ShortIncDirectory);
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
  LazPackage.CompilerOptions.MergeToIncludePaths(ShortDirectory);
end;

function TPackageEditorForm.ExtendUnitSearchPath(NewUnitPaths: string): boolean;
begin
  Result:=LazPackage.ExtendUnitSearchPath(NewUnitPaths);
end;

function TPackageEditorForm.ExtendIncSearchPath(NewIncPaths: string): boolean;
begin
  Result:=LazPackage.ExtendIncSearchPath(NewIncPaths);
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
  GuiToFileOptions(False);
  PackageEditors.SavePackage(LazPackage,SaveAs);
  UpdateTitle;
  UpdateButtons;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.DoCompile(CompileClean, CompileRequired,
  WarnIDEPkg: boolean);
var
  MsgResult: Integer;
begin
  if WarnIDEPkg and not FCompileDesignTimePkg
      and (LazPackage.PackageType=lptDesignTime) then
  begin
    MsgResult:=IDEQuestionDialog(dlgMsgWinColorUrgentWarning,
        Format(lisPackageIsDesigntimeOnlySoItShouldOnlyBeCompiledInt, [
          LazPackage.Name, #13]),
        mtWarning, [mrYes, lisCompileWithProjectSettings,
        mrYesToAll, lisCompileAndDoNotAskAgain, mrCancel]);
    case MsgResult of
    mrYes: ;
    mrYesToAll:
      FCompileDesignTimePkg:=true; // store setting only while running the IDE
                                   // when IDE restarts, ask again
    else exit;
    end;
  end;
  CompileBitBtn.Enabled:=False;
  FCompiling:=True;
  PackageEditors.CompilePackage(LazPackage,CompileClean,CompileRequired);
  FCompiling:=False;
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

procedure TPackageEditorForm.DoPublishPackage;
begin
  PackageEditors.PublishPackage(LazPackage);
end;

procedure TPackageEditorForm.DoEditVirtualUnit;
var
  PkgFile: TPkgFile;
begin
  if LazPackage=nil then exit;
  PkgFile:=GetSingleSelectedFile;
  if (PkgFile=nil)
  or (PkgFile.FileType<>pftVirtualUnit)
  or (LazPackage.IndexOfPkgFile(PkgFile)<0)
  then exit;
  if ShowEditVirtualPackageDialog(PkgFile)=mrOk then
    UpdateFiles;
end;

procedure TPackageEditorForm.DoExpandCollapseDirectory(ExpandIt: Boolean);
var
  CurNode: TTreeNode;
begin
  CurNode:=ItemsTreeView.Selected;
  if not (IsDirectoryNode(CurNode) or (CurNode=FFilesNode)) then exit;
  ItemsTreeView.BeginUpdate;
  if ExpandIt then
    CurNode.Expand(true)
  else
    CurNode.Collapse(true);
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
  PkgFile: TPkgFile;
begin
  PkgFile:=GetSingleSelectedFile;
  if (LazPackage=nil) or (PkgFile=nil) then exit;
  OldIndex:=LazPackage.IndexOfPkgFile(PkgFile);
  if OldIndex<0 then exit;
  NewIndex:=OldIndex+Offset;
  if (NewIndex<0) or (NewIndex>=LazPackage.FileCount) then exit;
  FilesBranch:=FilterEdit.GetExistingBranch(FFilesNode);
  LazPackage.MoveFile(OldIndex,NewIndex);
  FilesBranch.Move(OldIndex,NewIndex);
  UpdatePEProperties;
  UpdateStatusBar;
  FilterEdit.InvalidateFilter;
end;

procedure TPackageEditorForm.DoMoveDependency(Offset: integer);
var
  OldIndex, NewIndex: Integer;
  RequiredBranch: TTreeFilterBranch;
  Moved: Boolean;
  CurDependency: TPkgDependency;
begin
  CurDependency:=GetSingleSelectedDependency;
  if (LazPackage=nil) or (CurDependency=nil) then exit;
  if Offset<0 then
    Moved := LazPackage.MoveRequiredDependencyUp(CurDependency)
  else
    Moved := LazPackage.MoveRequiredDependencyDown(CurDependency);
  if not Moved then exit;
  LazPackage.ModifySilently;
  RequiredBranch:=FilterEdit.GetExistingBranch(FRequiredPackagesNode);
  OldIndex:=RequiredBranch.Items.IndexOf(DependencyAsString(CurDependency));
  NewIndex:=OldIndex+Offset;
  RequiredBranch.Move(OldIndex,NewIndex);
  UpdatePEProperties;
  UpdateStatusBar;
  FilterEdit.InvalidateFilter;
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

function TPackageEditorForm.CanCloseEditor: TModalResult;
var
  MsgResult: Integer;
begin
  Result:=mrOK;
  //debugln(['TPackageEditorForm.CanCloseEditor ',Caption]);
  if (LazPackage<>nil) and (not (lpfDestroying in LazPackage.Flags)) then
  begin
    if (not LazPackage.ReadOnly) and LazPackage.Modified then
    begin
      MsgResult:=MessageDlg(lisPkgMangSavePackage,
        Format(lisPckEditPackageHasChangedSavePackage, [LazPackage.IDAsString, LineEnding]),
        mtConfirmation,[mbYes,mbNo,mbAbort],0);
      case MsgResult of
        mrYes:
          MsgResult:=PackageEditors.SavePackage(LazPackage,false);
        mrNo:
          LazPackage.UserIgnoreChangeStamp:=LazPackage.ChangeStamp;
      end;
      if MsgResult=mrAbort then
        Result:=mrAbort
      else
        LazPackage.Modified:=false; // clear modified flag, so that it will be closed
    end;
    if (Result=mrOK) and not MainIDE.IDEIsClosing then
    begin
      EnvironmentOptions.LastOpenPackages.Remove(LazPackage.Filename);
      MainIDE.SaveEnvironment;
      if FCompiling then begin
        DebugLn(['TPackageEditorForm.CanCloseEditor: ', Caption, ' compiling, do not close.']);
        Result:=mrCancel;
      end;
    end;
  end;
  //debugln(['TPackageEditorForm.CanCloseEditor Result=',dbgs(Result),' ',Caption]);
end;

procedure TPackageEditorForm.TraverseSettings(AOptions: TAbstractPackageFileIDEOptions;
  anAction: TIDEPackageOptsDlgAction);

  procedure Traverse(Control: TWinControl);
  var
    i: Integer;
  begin
    if Control <> nil then
    begin
      if Control is TAbstractIDEOptionsEditor then
        with TAbstractIDEOptionsEditor(Control) do
        begin
          case anAction of
          iodaRead: ReadSettings(AOptions);
          iodaWrite: WriteSettings(AOptions);
          iodaRestore: RestoreSettings(AOptions);
          end;
        end;
      for i := 0 to Control.ControlCount -1 do
        if Control.Controls[i] is TWinControl then
        begin
          Traverse(TWinControl(Control.Controls[i]));
        end;
    end;
  end;

begin
  Traverse(PropsPageControl);
end;

procedure TPackageEditorForm.CreatePackageFileEditors;

var
  Instance: TAbstractIDEOptionsEditor;
  i, j: integer;
  Rec: PIDEOptionsGroupRec;
  AGroupCaption: string;
  ACaption: string;
  ItemTabSheet: TTabSheet;
begin
  IDEEditorGroups.Resort;

  for i := 0 to IDEEditorGroups.Count - 1 do
  begin
    Rec := IDEEditorGroups[i];
    //DebugLn(['TPackageEditorForm.CreatePackageFileEditors ',Rec^.GroupClass.ClassName]);
    if PassesFilter(Rec) then
    begin
      if Rec^.GroupClass<>nil then
        AGroupCaption := Rec^.GroupClass.GetGroupCaption
      else
        AGroupCaption := '';

      for j := 0 to Rec^.Items.Count - 1 do
      begin
        ItemTabSheet := PropsPageControl.AddTabSheet;
        ItemTabSheet.Align := alClient;

        Instance := Rec^.Items[j]^.EditorClass.Create(Self);
//        Instance.OnLoadIDEOptions := @LoadIDEOptions;
//        Instance.OnSaveIDEOptions := @SaveIDEOptions;
        // In principle the parameter should be a TAbstractOptionsEditorDialog,
        // but in this case this is not available, so pass nil.
        // Better would be to change the structure of the classes to avoid this
        // problem.
        Instance.Setup(Nil);
        Instance.OnChange := @FileOptionsChange;
        Instance.Tag := Rec^.Items[j]^.Index;
        Instance.Parent := ItemTabSheet;
        Instance.Rec := Rec^.Items[j];
        ACaption := Instance.GetTitle;
        if AGroupCaption <> ACaption then
          ACaption := AGroupCaption + ' - ' + ACaption;
        ItemTabSheet.Caption := ACaption;
      end;
    end;
  end;
end;

procedure TPackageEditorForm.FileOptionsToGui;
type
  TStage = (sBefore, sRead, sAfter);
var
  i: integer;
  Rec: PIDEOptionsGroupRec;
  Instance: TAbstractPackageFileIDEOptions;
  InstanceList: TFPList;
  stag: TStage;
  PkgFile: TPkgFile;
begin
  PkgFile:=GetSingleSelectedFile;
  if not Assigned(PkgFile) then exit;
  FOptionsShownOfFile := PkgFile;
  for stag:=low(TStage) to High(TStage) do
  begin
    InstanceList:=TFPList.Create;
    for i := 0 to IDEEditorGroups.Count - 1 do
    begin
      Rec := IDEEditorGroups[i];
      if not PassesFilter(Rec) then
        Continue;
      Instance := TAbstractPackageFileIDEOptions(TAbstractPackageFileIDEOptionsClass(Rec^.GroupClass).GetInstance(LazPackage, FOptionsShownOfFile));
      if (InstanceList.IndexOf(Instance)<0) and Assigned(Instance) then
      begin
        InstanceList.Add(Instance);
        case stag of
        sBefore:
          Instance.DoBeforeRead;
        sRead:
          TraverseSettings(Instance,iodaRead);
        sAfter:
          Instance.DoAfterRead;
        end;
      end;
    end;
    if stag=sRead then
      TraverseSettings(nil,iodaRead); // load settings that does not belong to any group
    InstanceList.Free;
  end;
end;

procedure TPackageEditorForm.GuiToFileOptions(Restore: boolean);
type
  TStage = (sBefore, sWrite, sAfter);
var
  i: integer;
  Rec: PIDEOptionsGroupRec;
  Instance: TAbstractPackageFileIDEOptions;
  stag: TStage;
begin
  if Assigned(FOptionsShownOfFile) then
  begin
    for stag:=low(TStage) to High(TStage) do
    begin
      for i := 0 to IDEEditorGroups.Count - 1 do
      begin
        Rec := IDEEditorGroups[i];
        if not PassesFilter(Rec) then
          Continue;
        Instance := TAbstractPackageFileIDEOptions(TAbstractPackageFileIDEOptionsClass(Rec^.GroupClass).GetInstance(LazPackage, FOptionsShownOfFile));
        if Assigned(Instance) then
        begin
          case stag of
          sBefore:
            Instance.DoBeforeWrite(Restore);
          sWrite:
            if Restore then
              TraverseSettings(Instance,iodaRestore)
            else
              TraverseSettings(Instance,iodaWrite);
          sAfter:
            Instance.DoAfterWrite(Restore);
          end;
        end;
      end;

      // save settings that do not belong to any group
      if stag=sWrite then
        if Restore then
          TraverseSettings(nil,iodaRestore)
        else
          TraverseSettings(nil,iodaWrite);
    end;
  end;
end;

procedure TPackageEditorForm.FileOptionsChange(Sender: TObject);
begin
  LazPackage.Modified := True;
end;

function TPackageEditorForm.PassesFilter(rec: PIDEOptionsGroupRec): Boolean;
begin
  Result := (Rec^.GroupClass.InheritsFrom(TAbstractPackageFileIDEOptions)) and (Rec^.Items <> nil);
end;

{ TPackageEditors }

function TPackageEditors.GetEditors(Index: integer): TPackageEditorForm;
begin
  Result:=TPackageEditorForm(FItems[Index]);
end;

function TPackageEditors.IndexOfPackage(const PkgName: string): integer;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    if Assigned(Editors[I].LazPackage) and
      SameText(ExtractFileNameOnly(Editors[I].LazPackage.Filename), PkgName)
    then
      Exit(I);

  Result := -1;
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

function TPackageEditors.CreateEditor(Pkg: TLazPackage;
  DoDisableAutoSizing: boolean): TPackageEditorForm;
begin
  Result:=FindEditor(Pkg);
  if Result<>nil then begin
    if DoDisableAutoSizing then
      Result.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TAnchorDockMaster Delayed'){$ENDIF};
  end else begin
    Result:=TPackageEditorForm(TPackageEditorForm.NewInstance);
    {$IFDEF DebugDisableAutoSizing}
    if DoDisableAutoSizing then
      Result.DisableAutoSizing('TAnchorDockMaster Delayed')
    else
      Result.DisableAutoSizing('TPackageEditors.OpenEditor');
    {$ELSE}
    Result.DisableAutoSizing;
    {$ENDIF}
    Result.Create(LazarusIDE.OwningComponent);
    Result.LazPackage:=Pkg;
    FItems.Add(Result);
    if not DoDisableAutoSizing then
      Result.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TPackageEditors.OpenEditor'){$ENDIF};
  end;
end;

function TPackageEditors.OpenEditor(Pkg: TLazPackage; BringToFront: boolean
  ): TPackageEditorForm;
begin
  Result:=CreateEditor(Pkg,true);
  try
    IDEWindowCreators.ShowForm(Result, BringToFront);
  finally
    Result.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TAnchorDockMaster Delayed'){$ENDIF};
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
    if Dependency.DependencyType=pdtLazarus then
    begin
      APackage:=Dependency.RequiredPackage;
      if Assigned(OnOpenPackage) then Result:=OnOpenPackage(Sender,APackage);
    end
    else
      ShowMessage('It is not possible to open FPMake packages.');
  end;
end;

procedure TPackageEditors.DoFreeEditor(Pkg: TLazPackage);
begin
  if FItems<>nil then
    FItems.Remove(Pkg.Editor);
  if Assigned(OnFreeEditor) then OnFreeEditor(Pkg);
end;

function TPackageEditors.FindEditor(const PkgName: string): TPackageEditorForm;
var
  i: Integer;
begin
  i:=IndexOfPackage(PkgName);
  if i>=0 then
    Result:=Editors[i]
  else
    Result:=nil;
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
var
  Dep: TPkgDependency;
  CurPkg: TLazPackage;
begin
  if APackage.Missing then
    exit(true)
  else if (APackage.FindUnitWithRegister<>nil) or (APackage.Provides.Count>0) then
    exit(false);
  Dep:=APackage.FirstRequiredDependency;
  while Dep<>nil do begin
    CurPkg:=Dep.RequiredPackage;
    if (CurPkg<>nil) then begin
      if (CurPkg.FindUnitWithRegister<>nil) or (CurPkg.Provides.Count>0) then
        exit(false);
    end;
    Dep:=Dep.NextRequiresDependency;
  end;
  Result:=true;
end;

function TPackageEditors.InstallPackage(APackage: TLazPackage): TModalResult;
begin
  if ShouldNotBeInstalled(APackage) then begin
    if IDEQuestionDialog(lisNotAnInstallPackage,
      Format(lisThePackageDoesNotHaveAnyRegisterProcedureWhichTypi,
             [APackage.Name, LineEnding+LineEnding]),
      mtWarning, [mrIgnore, lisInstallItILikeTheFat,
                  mrCancel, lisCancel], '') <> mrIgnore
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
  IDEWindowsGlobalOptions.Add(PackageEditorWindowPrefix, False);

end.

