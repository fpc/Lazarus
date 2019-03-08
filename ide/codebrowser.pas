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
    Browser for packages, classes, methods, functions.
    Scope:
      Browse units of IDE, or a project or a package.
      Browse with required packages or without.
    Sort:
      Owner, unit, class, visibility, type (procedure, var, const, ...), identifier

  Notes:
    The codetools provides TCodeTree of every unit.

  ToDo:
    - pause
    - scan recently used packages
    - scan packages in global links
}
unit CodeBrowser;

{$mode objfpc}{$H+}

{off $DEFINE VerboseCodeBrowser}

interface

uses
  // RTL + FCL
  Classes, SysUtils, types, Laz_AVL_Tree,
  // LCL
  LCLProc, Forms, Controls, Graphics, Dialogs, Clipbrd, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, Menus, HelpIntfs,
  // CodeTools
  BasicCodeTools, DefineTemplates, CodeTree, CodeCache, CodeToolManager,
  PascalParserTool, LinkScanner, FileProcs, CodeIndex, StdCodeTools, SourceLog,
  // LazUtils
  LazFileUtils, LazStringUtils, AvgLvlTree,
  // IDEIntf
  IDEWindowIntf, SrcEditorIntf, IDEMsgIntf, IDEDialogs, LazConfigStorage,
  IDEHelpIntf, PackageIntf, IDECommands, LazIDEIntf, IDEExternToolIntf,
  IDEImagesIntf,
  // IDE
  Project, DialogProcs, PackageSystem, PackageDefs, LazarusIDEStrConsts,
  IDEOptionDefs, etFPCMsgParser, BasePkgManager, EnvironmentOpts;


type
  TCodeBrowserLevel = (
    cblPackages,
    cblUnits,
    cblIdentifiers
    );
    
  TCodeBrowserTextFilter = (
    cbtfBegins,
    cbtfContains
    );
    
const
  CodeBrowserLevelNames: array[TCodeBrowserLevel] of string = (
    'Packages',
    'Units',
    'Identifiers'
    );
    
  CodeBrowserTextFilterNames: array[TCodeBrowserTextFilter] of string = (
    'Begins',
    'Contains'
    );

  CodeBrowserIDEName     = ' '+'Lazarus IDE';// Note: space is needed to avoid name clashing
  CodeBrowserProjectName = ' '+'Project';
  CodeBrowserHidden = ' ';
  CodeBrowserMaxTVIdentifiers = 5000; // the maximum amount of identifiers shown in the treeview

type

  { TCodeBrowserViewOptions }

  TCodeBrowserViewOptions = class
  private
    FChangeStamp: integer;
    FModified: boolean;
    FScope: string;
    FLevels: TStrings;
    FShowEmptyNodes: boolean;
    FShowPrivate: boolean;
    FShowProtected: boolean;
    FStoreWithRequiredPackages: boolean;
    FWithRequiredPackages: boolean;
    FLevelFilterText: array[TCodeBrowserLevel] of string;
    FLevelFilterType: array[TCodeBrowserLevel] of TCodeBrowserTextFilter;
    function GetLevelFilterText(Level: TCodeBrowserLevel): string;
    function GetLevelFilterType(Level: TCodeBrowserLevel): TCodeBrowserTextFilter;
    procedure SetLevelFilterText(Level: TCodeBrowserLevel; const AValue: string);
    procedure SetLevelFilterType(Level: TCodeBrowserLevel;
      const AValue: TCodeBrowserTextFilter);
    procedure SetModified(const AValue: boolean);
    procedure SetScope(const AValue: string);
    procedure SetLevels(const AValue: TStrings);
    procedure SetShowEmptyNodes(const AValue: boolean);
    procedure SetShowPrivate(const AValue: boolean);
    procedure SetShowProtected(const AValue: boolean);
    procedure SetStoreWithRequiredPackages(const AValue: boolean);
    procedure SetWithRequiredPackages(const AValue: boolean);
    procedure IncreaseChangeStamp;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromConfig(ConfigStore: TConfigStorage; const Path: string);
    procedure SaveToConfig(ConfigStore: TConfigStorage; const Path: string);
    function HasLevel(Level: TCodeBrowserLevel): boolean;
  public
    property Scope: string read FScope write SetScope;
    property WithRequiredPackages: boolean read FWithRequiredPackages write SetWithRequiredPackages;
    property StoreWithRequiredPackages: boolean read FStoreWithRequiredPackages write SetStoreWithRequiredPackages;
    property Levels: TStrings read FLevels write SetLevels;
    property ShowPrivate: boolean read FShowPrivate write SetShowPrivate;
    property ShowProtected: boolean read FShowProtected write SetShowProtected;
    property ShowEmptyNodes: boolean read FShowEmptyNodes write SetShowEmptyNodes;
    property LevelFilterText[Level: TCodeBrowserLevel]: string read GetLevelFilterText write SetLevelFilterText;
    property LevelFilterType[Level: TCodeBrowserLevel]: TCodeBrowserTextFilter read GetLevelFilterType write SetLevelFilterType;
    property Modified: boolean read FModified write SetModified;
    property ChangeStamp: integer read FChangeStamp;
  end;


  TCodeBrowserWorkStage = (
    cbwsGetScopeOptions,
    cbwsGatherPackages,
    cbwsFreeUnusedPackages,
    cbwsAddNewPackages,
    cbwsGatherFiles,
    cbwsGatherOutdatedFiles,
    cbwsUpdateUnits,
    cbwsGetViewOptions,
    cbwsUpdateTreeView,
    cbwsFinished
    );
    
  TExpandableNodeType = (
    entPackage,
    entUnit,
    entClass
    );
    
  TCopyNodeType = (
    cntIdentifier,
    cntDescription
    );

  { TCodeBrowserView }

  TCodeBrowserView = class(TForm)
    AllClassesSeparatorMenuItem: TMenuItem;
    AllPackagesSeparatorMenuItem: TMenuItem;
    AllUnitsSeparatorMenuItem: TMenuItem;
    BrowseTreeView: TTreeView;
    UseIdentifierInCurUnitMenuItem: TMenuItem;
    UseUnitInCurUnitMenuItem: TMenuItem;
    RescanButton: TButton;
    IdleTimer1: TIdleTimer;
    UsePkgInProjectMenuItem: TMenuItem;
    UsePkgInCurUnitMenuItem: TMenuItem;
    UseSeparatorMenuItem: TMenuItem;
    ShowEmptyNodesCheckBox: TCheckBox;
    CollapseAllClassesMenuItem: TMenuItem;
    CollapseAllPackagesMenuItem: TMenuItem;
    CollapseAllUnitsMenuItem: TMenuItem;
    CopyDescriptionMenuItem: TMenuItem;
    CopyIdentifierMenuItem: TMenuItem;
    CopySeparatorMenuItem: TMenuItem;
    ExpandAllClassesMenuItem: TMenuItem;
    ExpandAllPackagesMenuItem: TMenuItem;
    ExpandAllUnitsMenuItem: TMenuItem;
    ExportMenuItem: TMenuItem;
    IdentifierFilterBeginsSpeedButton: TSpeedButton;
    IdentifierFilterContainsSpeedButton: TSpeedButton;
    IdentifierFilterEdit: TEdit;
    LevelsGroupBox: TGroupBox;
    OpenMenuItem: TMenuItem;
    OptionsGroupBox: TGroupBox;
    PackageFilterBeginsSpeedButton: TSpeedButton;
    PackageFilterContainsSpeedButton: TSpeedButton;
    PackageFilterEdit: TEdit;
    PopupMenu1: TPopupMenu;
    ProgressBar1: TProgressBar;
    ScopeComboBox: TComboBox;
    ScopeGroupBox: TGroupBox;
    ScopeWithRequiredPackagesCheckBox: TCheckBox;
    ShowIdentifiersCheckBox: TCheckBox;
    ShowPackagesCheckBox: TCheckBox;
    ShowPrivateCheckBox: TCheckBox;
    ShowProtectedCheckBox: TCheckBox;
    ShowUnitsCheckBox: TCheckBox;
    StatusBar1: TStatusBar;
    UnitFilterBeginsSpeedButton: TSpeedButton;
    UnitFilterContainsSpeedButton: TSpeedButton;
    UnitFilterEdit: TEdit;
    procedure BrowseTreeViewMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X,
      {%H-}Y: Integer);
    procedure FormDeactivate(Sender: TObject);
    procedure UseIdentifierInCurUnitMenuItemClick(Sender: TObject);
    procedure UsePkgInCurUnitMenuItemClick(Sender: TObject);
    procedure UsePkgInProjectMenuItemClick(Sender: TObject);
    procedure UseUnitInCurUnitMenuItemClick(Sender: TObject);
    procedure BrowseTreeViewMouseDown(Sender: TOBject; {%H-}Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BrowseTreeViewShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure CollapseAllPackagesMenuItemClick(Sender: TObject);
    procedure CollapseAllUnitsMenuItemClick(Sender: TObject);
    procedure CollapseAllClassesMenuItemClick(Sender: TObject);
    procedure CopyDescriptionMenuItemClick(Sender: TObject);
    procedure CopyIdentifierMenuItemClick(Sender: TObject);
    procedure ExpandAllClassesMenuItemClick(Sender: TObject);
    procedure ExpandAllPackagesMenuItemClick(Sender: TObject);
    procedure ExpandAllUnitsMenuItemClick(Sender: TObject);
    procedure ExportMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure PackageFilterEditChange(Sender: TObject);
    procedure PackageFilterEditEditingDone(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure RescanButtonClick(Sender: TObject);
    procedure ScopeComboBoxChange(Sender: TObject);
    procedure ScopeWithRequiredPackagesCheckBoxChange(Sender: TObject);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure ShowIdentifiersCheckBoxChange(Sender: TObject);
    procedure ShowPackagesCheckBoxChange(Sender: TObject);
    procedure ShowPrivateCheckBoxChange(Sender: TObject);
    procedure ShowUnitsCheckBoxChange(Sender: TObject);
  private
    FHintManager: THintWindowManager;
    FIDEDescription: string;
    FIdleConnected: boolean;
    FOptions: TCodeBrowserViewOptions;
    FOptionsChangeStamp: integer;
    FProjectDescription: string;
    FParserRoot: TCodeBrowserUnitList;
    FScannedBytes: PtrInt;
    FScannedIdentifiers: PtrInt;
    FScannedLines: PtrInt;
    FScannedPackages: integer;
    FScannedUnits: integer;
    FUpdateNeeded: boolean;
    FViewRoot: TCodeBrowserUnitList;
    FVisibleIdentifiers: PtrInt;
    FVisiblePackages: integer;
    FVisibleUnits: integer;
    FWorkingParserRoot: TCodeBrowserUnitList;
    fUpdateCount: integer;
    fStage: TCodeBrowserWorkStage;
    fOutdatedFiles: TAVLTree;// tree of TCodeBrowserUnit
    fLastStatusBarUpdate: TDateTime;
    ImgIDDefault: integer;
    ImgIDProgramCode: Integer;
    ImgIDUnitCode: Integer;
    ImgIDInterfaceSection: Integer;
    ImgIDImplementation: Integer;
    ImgIDInitialization: Integer;
    ImgIDFinalization: Integer;
    ImgIDTypeSection: Integer;
    ImgIDType: Integer;
    ImgIDVarSection: Integer;
    ImgIDVariable: Integer;
    ImgIDConstSection: Integer;
    ImgIDConst: Integer;
    ImgIDClass: Integer;
    ImgIDProc: Integer;
    ImgIDProperty: Integer;
    ImgIDPackage: Integer;
    ImgIDProject: Integer;
    procedure LoadOptions;
    procedure LoadLevelsGroupBox;
    procedure LoadFilterGroupbox;
    procedure FillScopeComboBox;
    procedure SetIdleConnected(AValue: boolean);
    procedure SetScannedBytes(const AValue: PtrInt);
    procedure SetScannedIdentifiers(const AValue: PtrInt);
    procedure SetScannedLines(const AValue: PtrInt);
    procedure SetScannedPackages(const AValue: integer);
    procedure SetScannedUnits(const AValue: integer);
    procedure SetUpdateNeeded(const AValue: boolean);
    procedure SetVisibleIdentifiers(const AValue: PtrInt);
    procedure SetVisiblePackages(const AValue: integer);
    procedure SetVisibleUnits(const AValue: integer);
    procedure Work(var Done: Boolean);
    procedure WorkGetScopeOptions;
    procedure WorkGatherPackages;
    procedure WorkFreeUnusedPackages;
    procedure WorkAddNewUnitLists;
    procedure WorkGatherFileLists;
    procedure WorkUpdateFileList(List: TCodeBrowserUnitList);
    procedure WorkGatherOutdatedFiles;
    procedure WorkUpdateUnits;
    procedure WorkUpdateUnit(AnUnit: TCodeBrowserUnit);
    procedure WorkGetViewOptions;
    procedure WorkUpdateTreeView;
    procedure FreeUnitList(List: TCodeBrowserUnitList);
    procedure UpdateStatusBar(Lazy: boolean);
    procedure RemoveUnit(AnUnit: TCodeBrowserUnit);
    function CountIdentifiers(Tool: TCodeTool): integer;
    procedure UpdateTreeView;
    procedure ClearTreeView;
    procedure InitTreeView;
    function ListOwnerToText(const ListOwner: string): string;
    procedure InitImageList;
    function GetNodeImage(CodeNode: TObject): integer;
    function GetTVNodeHint(TVNode: TTreeNode): string;
    function GetCodeHelp(TVNode: TTreeNode; out BaseURL, HTMLHint: string): boolean;
    procedure ExpandCollapseAllNodesInTreeView(NodeType: TExpandableNodeType;
                                               Expand: boolean);
    procedure CopyNode(TVNode: TTreeNode; NodeType: TCopyNodeType);
    function GetCodeTool(AnUnit: TCodeBrowserUnit): TStandardCodeTool;
    procedure GetNodeIdentifier(Tool: TStandardCodeTool;
      CTNode: TCodeTreeNode; out Identifier: string);
    procedure GetNodeDescription(Tool: TStandardCodeTool;
      CTNode: TCodeTreeNode; Identifier: string; out Description: string);
    function GetSelectedUnit: TCodeBrowserUnit;
    function GetSelectedPackage: TLazPackage;
    function GetCurUnitInSrcEditor(out FileOwner: TObject;
                                   out UnitCode: TCodeBuffer): boolean;
    function GetCurPackageInSrcEditor: TLazPackage;
    procedure OpenTVNode(TVNode: TTreeNode);
    procedure UseUnitInSrcEditor(InsertIdentifier: boolean);
    procedure CloseHintWindow;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    function ExportTree: TModalResult;
    function ExportTreeAsText(Filename: string): TModalResult;
    function GetScopeToCurUnitOwner(UseFCLAsDefault: boolean): string;
    function SetScopeToCurUnitOwner(UseFCLAsDefault,
                                    WithRequiredPackages: boolean): boolean;
    procedure SetFilterToSimpleIdentifier(Identifier: string);
    procedure InvalidateStage(AStage: TCodeBrowserWorkStage);
  public
    property ParserRoot: TCodeBrowserUnitList read FParserRoot;
    property WorkingParserRoot: TCodeBrowserUnitList read FWorkingParserRoot;
    property ViewRoot: TCodeBrowserUnitList read FViewRoot;
    property Options: TCodeBrowserViewOptions read FOptions;
    property IDEDescription: string read FIDEDescription;
    property ProjectDescription: string read FProjectDescription;
    property ScannedPackages: integer read FScannedPackages write SetScannedPackages;
    property ScannedUnits: integer read FScannedUnits write SetScannedUnits;
    property ScannedLines: PtrInt read FScannedLines write SetScannedLines;
    property ScannedBytes: PtrInt read FScannedBytes write SetScannedBytes;
    property ScannedIdentifiers: PtrInt read FScannedIdentifiers write SetScannedIdentifiers;
    property VisiblePackages: integer read FVisiblePackages write SetVisiblePackages;
    property VisibleUnits: integer read FVisibleUnits write SetVisibleUnits;
    property VisibleIdentifiers: PtrInt read FVisibleIdentifiers write SetVisibleIdentifiers;
    property UpdateNeeded: boolean read FUpdateNeeded write SetUpdateNeeded;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

type

  { TQuickFixIdentifierNotFound_Search }

  TQuickFixIdentifierNotFound_Search = class(TMsgQuickFix)
  public
    function IsApplicable(Msg: TMessageLine; out Identifier: string): boolean;
    procedure CreateMenuItems(Fixes: TMsgQuickFixes); override;
    procedure QuickFix({%H-}Fixes: TMsgQuickFixes; Msg: TMessageLine); override;
  end;
var
  CodeBrowserView: TCodeBrowserView = nil;
  
function StringToCodeBrowserTextFilter(const s: string): TCodeBrowserTextFilter;

procedure InitCodeBrowserQuickFixItems;
procedure CreateCodeBrowser(DisableAutoSizing: boolean);
procedure ShowCodeBrowser(const Identifier: string);

implementation

{$R *.lfm}

const
  ProgressGetScopeStart=0;
  ProgressGetScopeSize=10;
  ProgressGatherPackagesStart=ProgressGetScopeStart+ProgressGetScopeSize;
  ProgressGatherPackagesSize=30;
  ProgressFreeUnusedPkgStart=ProgressGatherPackagesStart+ProgressGatherPackagesSize;
  ProgressFreeUnusedPkgSize=100;
  ProgressAddNewUnitListsStart=ProgressFreeUnusedPkgStart+ProgressFreeUnusedPkgSize;
  ProgressAddNewUnitListsSize=300;
  ProgressGatherFileListsStart=ProgressAddNewUnitListsStart+ProgressAddNewUnitListsSize;
  ProgressGatherFileListsSize=300;
  ProgressGatherOutdatedFilesStart=ProgressGatherFileListsStart+ProgressGatherFileListsSize;
  ProgressGatherOutdatedFilesSize=300;
  ProgressUpdateUnitsStart=ProgressGatherOutdatedFilesStart+ProgressGatherOutdatedFilesSize;
  ProgressUpdateUnitsSize=3000;
  ProgressGetViewOptionsStart=ProgressUpdateUnitsStart+ProgressUpdateUnitsSize;
  ProgressGetViewOptionsSize=10;
  ProgressUpdateTreeViewStart=ProgressGetViewOptionsStart+ProgressGetViewOptionsSize;
  ProgressUpdateTreeViewSize=1000;
  ProgressTotal=ProgressUpdateTreeViewStart+ProgressUpdateTreeViewSize;
const
  ProcDescFlags = [phpWithStart,phpWithParameterNames,
                   phpWithVarModifiers,phpWithResultType,phpWithoutSemicolon];
  ProcIdentifierFlags = [phpWithoutClassKeyword,phpWithParameterNames,
                   phpWithoutSemicolon];
  PropDescFlags = [phpWithoutClassKeyword,phpWithParameterNames,
                   phpWithVarModifiers,phpWithResultType];

function StringToCodeBrowserTextFilter(const s: string): TCodeBrowserTextFilter;
begin
  for Result:=Low(TCodeBrowserTextFilter) to High(TCodeBrowserTextFilter) do
    if SysUtils.CompareText(CodeBrowserTextFilterNames[Result],s)=0 then exit;
  Result:=cbtfBegins;
end;

procedure InitCodeBrowserQuickFixItems;
begin
  RegisterIDEMsgQuickFix(TQuickFixIdentifierNotFound_Search.Create);
end;

procedure CreateCodeBrowser(DisableAutoSizing: boolean);
begin
  if CodeBrowserView=nil then
    IDEWindowCreators.CreateForm(CodeBrowserView,TCodeBrowserView,
      DisableAutoSizing,LazarusIDE.OwningComponent)
  else if DisableAutoSizing then
    CodeBrowserView.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('CreateCodeBrowser'){$ENDIF};
end;

procedure ShowCodeBrowser(const Identifier: string);
begin
  IDEWindowCreators.ShowForm(NonModalIDEWindowNames[nmiwCodeBrowser],true);
  CodeBrowserView.SetScopeToCurUnitOwner(true,true);
  CodeBrowserView.SetFilterToSimpleIdentifier(Identifier);
end;


{ TCodeBrowserView }

procedure TCodeBrowserView.FormCreate(Sender: TObject);
begin
  FHintManager:=THintWindowManager.Create;
  FOptions:=TCodeBrowserViewOptions.Create;
  
  FIDEDescription:=lisLazarusIDE;
  FProjectDescription:=dlgProject;

  Name:=NonModalIDEWindowNames[nmiwCodeBrowser];
  Caption:=lisCodeBrowser;

  ScopeGroupBox.Caption:=dlgSearchScope;
  ScopeWithRequiredPackagesCheckBox.Caption:=lisWithRequiredPackages;
  RescanButton.Caption:=lisRescan;
  LevelsGroupBox.Caption:=lisLevels;
  ShowPackagesCheckBox.Caption:=lisShowPackages;
  ShowUnitsCheckBox.Caption:=lisShowUnits;
  ShowIdentifiersCheckBox.Caption:=lisShowIdentifiers;

  OptionsGroupBox.Caption:=lisFilter;
  ShowPrivateCheckBox.Caption:=lisPrivate;
  ShowProtectedCheckBox.Caption:=lisProtected;
  ShowEmptyNodesCheckBox.Caption:=lisShowEmptyUnitsPackages;

  ExpandAllPackagesMenuItem.Caption:=lisExpandAllPackages;
  CollapseAllPackagesMenuItem.Caption:=lisCollapseAllPackages;
  ExpandAllUnitsMenuItem.Caption:=lisExpandAllUnits;
  CollapseAllUnitsMenuItem.Caption:=lisCollapseAllUnits;
  ExpandAllClassesMenuItem.Caption:=lisExpandAllClasses;
  CollapseAllClassesMenuItem.Caption:=lisCollapseAllClasses;
  ExportMenuItem.Caption:=lisDlgExport;
  OpenMenuItem.Caption:=lisOpen;
  // UsePkgInProjectMenuItem.Caption: see PopupMenu1Popup
  // UsePkgInCurUnitMenuItem.Caption: see PopupMenu1Popup
  // UseUnitInCurUnitMenuItem.Caption: see PopupMenu1Popup

  PackageFilterBeginsSpeedButton.Caption:=lisBegins;
  PackageFilterBeginsSpeedButton.Hint:=lisPackageNameBeginsWith;
  PackageFilterContainsSpeedButton.Caption:=lisContains;
  PackageFilterContainsSpeedButton.Hint:=lisPackageNameContains;
  UnitFilterBeginsSpeedButton.Caption:=lisBegins;
  UnitFilterBeginsSpeedButton.Hint:=lisUnitNameBeginsWith;
  UnitFilterContainsSpeedButton.Caption:=lisContains;
  UnitFilterContainsSpeedButton.Hint:=lisUnitNameContains;
  IdentifierFilterBeginsSpeedButton.Caption:=lisBegins;
  IdentifierFilterBeginsSpeedButton.Hint:=lisIdentifierBeginsWith;
  IdentifierFilterContainsSpeedButton.Caption:=lisContains;
  IdentifierFilterContainsSpeedButton.Hint:=lisIdentifierContains;
  
  ProgressBar1.Max:=ProgressTotal;
  InitImageList;
  LoadOptions;
  FillScopeComboBox;
  ScopeComboBox.ItemIndex:=0;
  IdleConnected:=true;
end;

procedure TCodeBrowserView.FormDestroy(Sender: TObject);
begin
  IdleConnected:=false;
  ClearTreeView;
  FreeAndNil(fOutdatedFiles);
  FreeAndNil(FViewRoot);
  FreeAndNil(FParserRoot);
  FreeAndNil(FWorkingParserRoot);
  FreeAndNil(FOptions);
  FreeAndNil(FHintManager);
  IdleConnected:=false;
end;

procedure TCodeBrowserView.IdleTimer1Timer(Sender: TObject);
begin
  InvalidateStage(cbwsGetViewOptions);
  IdleTimer1.Enabled:=false;
end;

procedure TCodeBrowserView.PackageFilterEditChange(Sender: TObject);
begin
  IdleTimer1.Enabled:=true;
end;

procedure TCodeBrowserView.PackageFilterEditEditingDone(Sender: TObject);
begin
  InvalidateStage(cbwsGetViewOptions);
end;

procedure TCodeBrowserView.PopupMenu1Popup(Sender: TObject);
var
  TVNode: TTreeNode;
  Node: TObject;
  Identifier: String;
  UnitList: TCodeBrowserUnitList;
  EnableUsePkgInProject: Boolean;
  APackage: TLazPackage;
  EnableUsePkgInCurUnit: Boolean;
  TargetPackage: TLazPackage;
  EnableUseUnitInCurUnit: Boolean;
  CurUnit: TCodeBrowserUnit;
  SrcEditUnitOwner: TObject;
  SrcEditUnitCode: TCodeBuffer;
  CurUnitName: String;
  SrcEditUnitName: String;
  CBNode: TCodeBrowserNode;
  EnableUseIdentifierInCurUnit: Boolean;
  SrcEdit: TSourceEditorInterface;
begin
  ExpandAllPackagesMenuItem.Visible:=Options.HasLevel(cblPackages);
  CollapseAllPackagesMenuItem.Visible:=ExpandAllPackagesMenuItem.Visible;
  AllPackagesSeparatorMenuItem.Visible:=ExpandAllPackagesMenuItem.Visible;
  
  ExpandAllUnitsMenuItem.Visible:=Options.HasLevel(cblUnits);
  CollapseAllUnitsMenuItem.Visible:=ExpandAllUnitsMenuItem.Visible;
  AllUnitsSeparatorMenuItem.Visible:=ExpandAllUnitsMenuItem.Visible;

  ExpandAllClassesMenuItem.Visible:=Options.HasLevel(cblIdentifiers);
  CollapseAllClassesMenuItem.Visible:=ExpandAllClassesMenuItem.Visible;
  AllClassesSeparatorMenuItem.Visible:=ExpandAllClassesMenuItem.Visible;

  TVNode:=BrowseTreeView.Selected;
  Node:=nil;
  if TVNode<>nil then
    Node:=TObject(TVNode.Data);
  EnableUsePkgInProject:=false;
  EnableUsePkgInCurUnit:=false;
  EnableUseUnitInCurUnit:=false;
  EnableUseIdentifierInCurUnit:=false;
  if Node<>nil then begin
    Identifier:='';
    APackage:=nil;
    UnitList:=nil;
    CurUnit:=nil;
    TargetPackage:=nil;
    if Node is TCodeBrowserNode then begin
      Identifier:=TCodeBrowserNode(Node).Identifier;
      CBNode:=TCodeBrowserNode(Node);
      CurUnit:=CBNode.CBUnit;
      if CurUnit<>nil then
        UnitList:=CurUnit.UnitList;
    end else if Node is TCodeBrowserUnit then begin
      CurUnit:=TCodeBrowserUnit(Node);
      UnitList:=CurUnit.UnitList;
    end else if Node is TCodeBrowserUnitList then begin
      UnitList:=TCodeBrowserUnitList(Node);
    end;
    if UnitList<>nil then begin
      if UnitList.Owner=CodeBrowserProjectName then begin
        // project
      end else if UnitList.Owner=CodeBrowserIDEName then begin
        // IDE
      end else if UnitList.Owner=CodeBrowserHidden then begin
        // nothing
      end else begin
        // package
        APackage:=PackageGraph.FindPackageWithName(UnitList.Owner,nil);
        if APackage<>nil then begin
          // check if package can be added to project
          if Project1.FindDependencyByName(APackage.Name)=nil then begin
            EnableUsePkgInProject:=true;
            UsePkgInProjectMenuItem.Caption:=Format(lisUsePackageInProject, [
              APackage.Name]);
          end;
          // check if package can be added to package of src editor unit
          TargetPackage:=GetCurPackageInSrcEditor;
          if (TargetPackage<>nil)
          and (SysUtils.CompareText(TargetPackage.Name,APackage.Name)<>0)
          and (TargetPackage.FindDependencyByName(APackage.Name)=nil) then begin
            EnableUsePkgInCurUnit:=true;
            UsePkgInCurUnitMenuItem.Caption:=Format(
              lisUsePackageInPackage, [APackage.Name,
              TargetPackage.Name]);
          end;
          // check if unit can be added to project/package
          GetCurUnitInSrcEditor(SrcEditUnitOwner,SrcEditUnitCode);
          if (CurUnit<>nil) and (SrcEditUnitOwner<>nil) then begin
            CurUnitName:=ExtractFileNameOnly(CurUnit.Filename);
            SrcEditUnitName:=ExtractFileNameOnly(SrcEditUnitCode.Filename);
            if SysUtils.CompareText(CurUnitName,SrcEditUnitName)<>0 then begin
              EnableUseUnitInCurUnit:=true;
              UseUnitInCurUnitMenuItem.Caption:=
                Format(lisUseUnitInUnit, [CurUnitName, SrcEditUnitName]);
              if (Node is TCodeBrowserNode) and (Identifier<>'') then begin
                EnableUseIdentifierInCurUnit:=true;
                SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
                UseIdentifierInCurUnitMenuItem.Caption:=
                  Format(lisUseIdentifierInAt, [Identifier, ExtractFilename(
                    SrcEdit.FileName), dbgs(SrcEdit.CursorScreenXY)]);
              end;
            end;
          end;
        end;
      end;
    end;
    OpenMenuItem.Visible:=true;
    CopyDescriptionMenuItem.Caption:=lisCopyDescription;
    CopyIdentifierMenuItem.Caption:=Format(lisCopyIdentifier, [Identifier]);
    CopyDescriptionMenuItem.Visible:=true;
    CopyIdentifierMenuItem.Visible:=Identifier<>'';
    CopySeparatorMenuItem.Visible:=true;

    UseUnitInCurUnitMenuItem.Enabled:=EnableUseUnitInCurUnit;
    UseUnitInCurUnitMenuItem.Visible:=true;
    if not EnableUseUnitInCurUnit then
      UseUnitInCurUnitMenuItem.Caption:=lisPkgMangUseUnit;

    UseIdentifierInCurUnitMenuItem.Enabled:=EnableUseIdentifierInCurUnit;
    UseIdentifierInCurUnitMenuItem.Visible:=true;
    if not EnableUseIdentifierInCurUnit then
      UseIdentifierInCurUnitMenuItem.Caption:=lisUseIdentifier;

    UsePkgInProjectMenuItem.Enabled:=EnableUsePkgInProject;
    UsePkgInProjectMenuItem.Visible:=true;
    if not EnableUsePkgInProject then
      UsePkgInProjectMenuItem.Caption:=lisUsePackageInProject2;

    UsePkgInCurUnitMenuItem.Enabled:=EnableUsePkgInCurUnit;
    UsePkgInCurUnitMenuItem.Visible:=true;
    if not EnableUsePkgInCurUnit then
      UsePkgInCurUnitMenuItem.Caption:=lisUsePackageInPackage2;
  end else begin
    OpenMenuItem.Visible:=false;
    CopyDescriptionMenuItem.Visible:=false;
    CopyIdentifierMenuItem.Visible:=false;
    CopySeparatorMenuItem.Visible:=false;
    UseUnitInCurUnitMenuItem.Visible:=false;
    UseIdentifierInCurUnitMenuItem.Visible:=false;
    UsePkgInProjectMenuItem.Visible:=false;
    UsePkgInCurUnitMenuItem.Visible:=false;
    UseSeparatorMenuItem.Visible:=false;
  end;
end;

procedure TCodeBrowserView.RescanButtonClick(Sender: TObject);
begin
  UpdateNeeded:=true;
  InvalidateStage(cbwsGetScopeOptions);
end;

procedure TCodeBrowserView.ScopeComboBoxChange(Sender: TObject);
begin
  InvalidateStage(cbwsGetScopeOptions);
end;

procedure TCodeBrowserView.ScopeWithRequiredPackagesCheckBoxChange(Sender: TObject);
begin
  InvalidateStage(cbwsGetScopeOptions);
end;

procedure TCodeBrowserView.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if (Screen.GetCurrentModalForm<>nil) then exit;
  Work(Done);
end;

procedure TCodeBrowserView.OpenMenuItemClick(Sender: TObject);
begin
  OpenTVNode(BrowseTreeView.Selected);
end;

procedure TCodeBrowserView.ShowIdentifiersCheckBoxChange(Sender: TObject);
begin
  InvalidateStage(cbwsGetViewOptions);
end;

procedure TCodeBrowserView.ShowPackagesCheckBoxChange(Sender: TObject);
begin
  //DebugLn(['TCodeBrowserView.ShowPackagesCheckBoxChange ']);
  InvalidateStage(cbwsGetViewOptions);
end;

procedure TCodeBrowserView.ShowPrivateCheckBoxChange(Sender: TObject);
begin
  InvalidateStage(cbwsGetViewOptions);
end;

procedure TCodeBrowserView.ShowUnitsCheckBoxChange(Sender: TObject);
begin
  InvalidateStage(cbwsGetViewOptions);
end;

procedure TCodeBrowserView.LoadOptions;
begin
  BeginUpdate;
  ScopeWithRequiredPackagesCheckBox.Checked:=Options.WithRequiredPackages;
  ScopeComboBox.Text:=Options.Scope;
  LoadLevelsGroupBox;
  LoadFilterGroupbox;
  EndUpdate;
end;

procedure TCodeBrowserView.LoadLevelsGroupBox;
begin
  ShowPackagesCheckBox.Checked:=Options.HasLevel(cblPackages);
  ShowUnitsCheckBox.Checked:=Options.HasLevel(cblUnits);
  ShowIdentifiersCheckBox.Checked:=Options.HasLevel(cblIdentifiers);
end;

procedure TCodeBrowserView.LoadFilterGroupbox;
begin
  ShowPrivateCheckBox.Checked:=Options.ShowPrivate;
  ShowProtectedCheckBox.Checked:=Options.ShowProtected;
  ShowEmptyNodesCheckBox.Checked:=Options.ShowEmptyNodes;

  PackageFilterEdit.Text:=Options.LevelFilterText[cblPackages];
  case Options.LevelFilterType[cblPackages] of
  cbtfBegins:   PackageFilterBeginsSpeedButton.Down:=true;
  cbtfContains: PackageFilterContainsSpeedButton.Down:=true;
  end;

  UnitFilterEdit.Text:=Options.LevelFilterText[cblUnits];
  case Options.LevelFilterType[cblUnits] of
  cbtfBegins:   UnitFilterBeginsSpeedButton.Down:=true;
  cbtfContains: UnitFilterContainsSpeedButton.Down:=true;
  end;

  IdentifierFilterEdit.Text:=Options.LevelFilterText[cblIdentifiers];
  case Options.LevelFilterType[cblIdentifiers] of
  cbtfBegins:   IdentifierFilterBeginsSpeedButton.Down:=true;
  cbtfContains: IdentifierFilterContainsSpeedButton.Down:=true;
  end;
end;

procedure TCodeBrowserView.FillScopeComboBox;
var
  sl: TStringList;
  i: Integer;
begin
  if ScopeComboBox.Items.Count=0 then begin
    sl:=TStringList.Create;
    try
      if PackageGraph<>nil then begin
        for i:=0 to PackageGraph.Count-1 do
          sl.Add(PackageGraph.Packages[i].Name);
      end;
      sl.Sort;
      sl.Insert(0,IDEDescription);
      sl.Insert(1,ProjectDescription);
      ScopeComboBox.Items.Assign(sl);
    finally
      sl.Free;
    end;
  end;
end;

procedure TCodeBrowserView.SetIdleConnected(AValue: boolean);
begin
  if csDestroying in ComponentState then AValue:=false;
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TCodeBrowserView.InitImageList;
begin
  BrowseTreeView.Images := IDEImages.Images_16;
  ImgIDDefault := IDEImages.GetImageIndex('ce_default');
  ImgIDProgramCode := IDEImages.GetImageIndex('ce_program');
  ImgIDUnitCode := IDEImages.GetImageIndex('cc_unit');
  ImgIDInterfaceSection := IDEImages.GetImageIndex('ce_interface');
  ImgIDImplementation := IDEImages.GetImageIndex('ce_implementation');
  ImgIDInitialization := IDEImages.GetImageIndex('ce_initialization');
  ImgIDFinalization := IDEImages.GetImageIndex('ce_finalization');
  ImgIDTypeSection := IDEImages.GetImageIndex('cc_type');
  ImgIDType := IDEImages.GetImageIndex('cc_type');
  ImgIDVarSection := IDEImages.GetImageIndex('cc_variable');
  ImgIDVariable := IDEImages.GetImageIndex('cc_variable');
  ImgIDConstSection := IDEImages.GetImageIndex('cc_constant');
  ImgIDConst := IDEImages.GetImageIndex('cc_constant');
  ImgIDClass := IDEImages.GetImageIndex('cc_class');
  ImgIDProc := IDEImages.GetImageIndex('cc_procedure');
  ImgIDProperty := IDEImages.GetImageIndex('cc_property');
  ImgIDPackage := IDEImages.GetImageIndex('item_package');
  ImgIDProject := IDEImages.GetImageIndex('item_project');
end;

procedure TCodeBrowserView.SetScannedBytes(const AValue: PtrInt);
begin
  if FScannedBytes=AValue then exit;
  FScannedBytes:=AValue;
end;

procedure TCodeBrowserView.SetScannedIdentifiers(const AValue: PtrInt);
begin
  if FScannedIdentifiers=AValue then exit;
  FScannedIdentifiers:=AValue;
end;

procedure TCodeBrowserView.SetScannedLines(const AValue: PtrInt);
begin
  if FScannedLines=AValue then exit;
  FScannedLines:=AValue;
end;

procedure TCodeBrowserView.SetScannedPackages(const AValue: integer);
begin
  if FScannedPackages=AValue then exit;
  FScannedPackages:=AValue;
end;

procedure TCodeBrowserView.SetScannedUnits(const AValue: integer);
begin
  if FScannedUnits=AValue then exit;
  FScannedUnits:=AValue;
end;

procedure TCodeBrowserView.SetUpdateNeeded(const AValue: boolean);

  procedure InvalidateFileList(StartList: TCodeBrowserUnitList);
  var
    APackage: TCodeBrowserUnitList;
    Node: TAVLTreeNode;
  begin
    if StartList=nil then exit;
    StartList.UnitsValid:=false;
    if (StartList.UnitLists=nil) then exit;
    Node:=StartList.UnitLists.FindLowest;
    while Node<>nil do begin
      APackage:=TCodeBrowserUnitList(Node.Data);
      InvalidateFileList(APackage);
      Node:=StartList.UnitLists.FindSuccessor(Node);
    end;
  end;

begin
  if FUpdateNeeded=AValue then exit;
  FUpdateNeeded:=AValue;
  if FUpdateNeeded then begin
    InvalidateFileList(FParserRoot);
    InvalidateFileList(FWorkingParserRoot);
    InvalidateStage(cbwsGetScopeOptions);
  end;
end;

procedure TCodeBrowserView.SetVisibleIdentifiers(const AValue: PtrInt);
begin
  if FVisibleIdentifiers=AValue then exit;
  FVisibleIdentifiers:=AValue;
end;

procedure TCodeBrowserView.SetVisiblePackages(const AValue: integer);
begin
  if FVisiblePackages=AValue then exit;
  FVisiblePackages:=AValue;
end;

procedure TCodeBrowserView.SetVisibleUnits(const AValue: integer);
begin
  if FVisibleUnits=AValue then exit;
  FVisibleUnits:=AValue;
end;

procedure TCodeBrowserView.UseUnitInSrcEditor(InsertIdentifier: boolean);
var
  // temporary data, that can be freed on next idle
  SelectedUnit: TCodeBrowserUnit;
  TVNode: TTreeNode;
  Node: TObject;
  IdentifierNode: TCodeBrowserNode;
  // normal vars
  SelectedUnitName: String;
  SelectedCode: TCodeBuffer;
  List: TFPList;
  SelectedOwner: TObject;
  APackage: TLazPackage;
  TargetCode: TCodeBuffer;
  TargetOwner: TObject;
  SrcEdit: TSourceEditorInterface;
  Code: TCodeBuffer;
  CodeMarker: TSourceLogMarker;
  Identifier: String;
  SelectedUnitFilename: String;
  IdentStart: integer;
  IdentEnd: integer;
  InsertStartPos: TPoint;
  InsertEndPos: TPoint;
begin
  TVNode:=BrowseTreeView.Selected;
  if TVNode=nil then exit;
  Node:=TObject(TVNode.Data);
  IdentifierNode:=nil;
  SelectedUnit:=nil;
  if Node is TCodeBrowserNode then begin
    IdentifierNode:=TCodeBrowserNode(Node);
    Identifier:=IdentifierNode.Identifier;
    SelectedUnit:=IdentifierNode.CBUnit;
  end else if Node is TCodeBrowserUnit then begin
    SelectedUnit:=TCodeBrowserUnit(Node);
  end else
    exit;
  if (SelectedUnit=nil) then exit;
  SelectedUnitFilename:=SelectedUnit.Filename;

  if InsertIdentifier then begin
    if (IdentifierNode=nil) or (Identifier='') then exit;
  end;
  if SelectedUnit.UnitList=nil then begin
    DebugLn(['TCodeBrowserView.UseUnitInSrcEditor not implemented: '
      +'SelectedUnit.UnitList=nil']);
    IDEMessageDialog('Implement me',
      'TCodeBrowserView.UseUnitInSrcEditor not implemented: '
        +'SelectedUnit.UnitList=nil',
      mtInformation, [mbOk]);
    exit;
  end;
  SelectedOwner:=nil;
  if SelectedUnit.UnitList.Owner=CodeBrowserProjectName then begin
    // project
    SelectedOwner:=Project1;
  end else if SelectedUnit.UnitList.Owner=CodeBrowserIDEName then begin
    // IDE can not be added as dependency
    DebugLn(['TCodeBrowserView.UseUnitInSrcEditor IDE can not be '
      +'added as dependency']);
    exit;
  end else if SelectedUnit.UnitList.Owner=CodeBrowserHidden then begin
    // nothing
    DebugLn(['TCodeBrowserView.UseUnitInSrcEditor hidden unitlist']
      );
    exit;
  end else begin
    // package
    APackage:=PackageGraph.FindPackageWithName(SelectedUnit.UnitList.Owner,nil);
    if APackage=nil then begin
      DebugLn(['TCodeBrowserView.UseUnitInSrcEditor package not '
        +'found: ', SelectedUnit.UnitList.Owner]);
      exit;
    end;
    SelectedOwner:=APackage;
  end;

  // get target unit
  if not GetCurUnitInSrcEditor(TargetOwner, TargetCode) then exit;
  if (not (TargetOwner is TProject))
  and (not (TargetOwner is TLazPackage)) then begin
    DebugLn(['TCodeBrowserView.UseUnitInSrcEditor not implemented: '
      +'TargetOwner=', DbgSName(TargetOwner)]);
    IDEMessageDialog('Implement me',
      'TCodeBrowserView.UseUnitInSrcEditor not implemented: '
        +'TargetOwner='+DbgSName(TargetOwner),
      mtInformation, [mbOk]);
    exit;
  end;

  if (SelectedOwner is TProject) and (TargetOwner<>SelectedOwner) then begin
    // unit of project can not be used by other packages/projects
    IDEMessageDialog(lisImpossible,
      lisAProjectUnitCanNotBeUsedByOtherPackagesProjects,
      mtError, [mbCancel]);
    exit;
  end;

  // safety first: clear the references, they will become invalid on next idle
  SelectedUnit:=nil;
  IdentifierNode:=nil;
  Node:=nil;
  TVNode:=nil;


  List:=TFPList.Create;
  CodeMarker:=nil;
  try
    SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
    if SrcEdit=nil then exit;
    InsertStartPos:=SrcEdit.CursorTextXY;
    Code:=TCodeBuffer(SrcEdit.CodeToolsBuffer);
    CodeMarker:=Code.AddMarkerXY(InsertStartPos.Y,InsertStartPos.X,Self);

    List.Add(TargetOwner);
    if (SelectedOwner is TLazPackage) then begin
      // add package to TargetOwner
      APackage:=TLazPackage(SelectedOwner);
      if PkgBoss.AddDependencyToOwners(List, APackage)<>mrOk then begin
        DebugLn(['TCodeBrowserView.UseUnitInSrcEditor PkgBoss.'
          +'AddDependencyToOwners failed']);
        exit;
      end;
    end;

    // get nice unit name
    LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);
    SelectedCode:=CodeToolBoss.LoadFile(SelectedUnitFilename, true, false);
    if SelectedCode=nil then begin
      debugln(['TCodeBrowserView.UseUnitInSrcEditor failed to load SelectedUnitFilename=',SelectedUnitFilename]);
      exit;
    end;
    SelectedUnitName:=CodeToolBoss.GetSourceName(SelectedCode, false);

    // add unit to uses section
    if not CodeToolBoss.AddUnitToMainUsesSection(TargetCode, SelectedUnitName,'') then
    begin
      DebugLn(['TCodeBrowserView.UseUnitInSrcEditor CodeToolBoss.'
        +'AddUnitToMainUsesSection failed: TargetCode=', TargetCode.Filename,
        ' SelectedUnitName=', SelectedUnitName]);
      LazarusIDE.DoJumpToCodeToolBossError;
    end;

    // insert identifier
    if InsertIdentifier then begin
      if CodeMarker.Deleted then begin
        DebugLn(['TCodeBrowserView.UseUnitInSrcEditor insert place was deleted']);
        exit;
      end;
      GetIdentStartEndAtPosition(Code.Source,CodeMarker.NewPosition,
                                 IdentStart,IdentEnd);
      Code.AbsoluteToLineCol(IdentStart,InsertStartPos.Y,InsertStartPos.X);
      InsertEndPos:=InsertStartPos;
      inc(InsertEndPos.X,IdentEnd-IdentStart);
      SrcEdit.ReplaceText(InsertStartPos,InsertEndPos,Identifier);
    end;
  finally
    List.Free;
    CodeMarker.Free;
  end;
end;

procedure TCodeBrowserView.Work(var Done: Boolean);
// do some work
// This is called during OnIdle, so progress in small steps
var
  OldStage: TCodeBrowserWorkStage;
begin
  OldStage:=fStage;
  case fStage of
  cbwsGetScopeOptions:     WorkGetScopeOptions;
  cbwsGatherPackages:      WorkGatherPackages;
  cbwsFreeUnusedPackages:  WorkFreeUnusedPackages;
  cbwsAddNewPackages:      WorkAddNewUnitLists;
  cbwsGatherFiles:         WorkGatherFileLists;
  cbwsGatherOutdatedFiles: WorkGatherOutdatedFiles;
  cbwsUpdateUnits:         WorkUpdateUnits;
  cbwsGetViewOptions:      WorkGetViewOptions;
  cbwsUpdateTreeView:      WorkUpdateTreeView;
  else
    FOptionsChangeStamp:=Options.ChangeStamp;
    UpdateNeeded:=false;
    Done:=true;
    ProgressBar1.Position:=ProgressTotal;
    ProgressBar1.Visible:=false;
    exit;
  end;
  if ord(OldStage)<ord(cbwsFinished) then begin
    Done:=false;
    ProgressBar1.Visible:=true;
    UpdateStatusBar(fStage<cbwsFinished);
  end;
  //if fStage=cbwsFinished then CodeToolBoss.WriteMemoryStats;
end;

procedure TCodeBrowserView.WorkGetScopeOptions;
var
  CurChangStamp: LongInt;
begin
  DebugLn(['TCodeBrowserView.WorkGetScopeOptions START']);
  IdleTimer1.Enabled:=false;

  ProgressBar1.Position:=ProgressGetScopeStart;
  CurChangStamp:=Options.ChangeStamp;
  Options.WithRequiredPackages:=ScopeWithRequiredPackagesCheckBox.Checked;
  Options.Scope:=ScopeComboBox.Text;

  // this stage finished -> next stage
  if UpdateNeeded or (Options.ChangeStamp<>CurChangStamp) then
    fStage:=cbwsGatherPackages
  else
    fStage:=cbwsGetViewOptions;
  ProgressBar1.Position:=ProgressGetScopeStart+ProgressGetScopeSize;
end;

procedure TCodeBrowserView.WorkGatherPackages;

  procedure AddPackage(APackage: TLazPackage);
  begin
    TCodeBrowserUnitList.Create(APackage.Name,FWorkingParserRoot);
  end;
  
  procedure AddPackages(FirstDependency: TPkgDependency);
  var
    List: TFPList;
    i: Integer;
  begin
    List:=nil;
    try
      PackageGraph.GetAllRequiredPackages(nil,FirstDependency,List);
      if (List=nil) then exit;
      for i:=0 to List.Count-1 do begin
        if TObject(List[i]) is TLazPackage then
          AddPackage(TLazPackage(List[i]));
      end;
    finally
      List.Free;
    end;
  end;

var
  APackage: TLazPackage;
  RootOwner: string;
  i: Integer;
begin
  // clean up
  if fOutdatedFiles<>nil then fOutdatedFiles.Clear;

  // find ParserRoot
  RootOwner:='';
  if Options.Scope=IDEDescription then begin
    RootOwner:=CodeBrowserIDEName;
  end else if Options.Scope=ProjectDescription then begin
    RootOwner:=CodeBrowserProjectName;
  end else begin
    APackage:=PackageGraph.FindPackageWithName(Options.Scope,nil);
    if APackage<>nil then
      RootOwner:=APackage.Name;
  end;
  DebugLn(['TCodeBrowserView.WorkGatherPackages RootOwner="',RootOwner,'"']);
  FreeAndNil(FWorkingParserRoot);
  FWorkingParserRoot:=TCodeBrowserUnitList.Create(RootOwner,nil);
  
  // find required packages
  if Options.WithRequiredPackages then begin
    if SysUtils.CompareText(FWorkingParserRoot.Owner,CodeBrowserIDEName)=0 then begin
      for i:=0 to PackageGraph.Count-1 do
        AddPackage(PackageGraph[i]);
    end else if SysUtils.CompareText(FWorkingParserRoot.Owner,CodeBrowserProjectName)=0
    then begin
      AddPackages(Project1.FirstRequiredDependency);
    end else if FWorkingParserRoot.Owner<>'' then begin
      APackage:=PackageGraph.FindPackageWithName(FWorkingParserRoot.Owner,nil);
      if APackage<>nil then
        AddPackages(APackage.FirstRequiredDependency);
    end;
  end;
  
  // update ParserRoot item (children will be updated on next Idle)
  if FParserRoot=nil then begin
    FParserRoot:=TCodeBrowserUnitList.Create(FWorkingParserRoot.Owner,nil);
    inc(FScannedPackages);
  end else begin
    FParserRoot.Owner:=FWorkingParserRoot.Owner;
  end;
  
  // this stage finished -> next stage
  fStage:=cbwsFreeUnusedPackages;
  ProgressBar1.Position:=ProgressGatherPackagesStart+ProgressGatherPackagesSize;
end;

procedure TCodeBrowserView.WorkFreeUnusedPackages;

  function FindUnusedUnitList: TCodeBrowserUnitList;
  var
    Node: TAVLTreeNode;
    UnusedPackage: TCodeBrowserUnitList;
    PackageName: String;
  begin
    // find an unused package (a package in ParserRoot but not in WorkingParserRoot)
    Result:=nil;
    if (FParserRoot=nil) or (FParserRoot.UnitLists=nil) then exit;
    Node:=FParserRoot.UnitLists.FindLowest;
    while Node<>nil do begin
      UnusedPackage:=TCodeBrowserUnitList(Node.Data);
      PackageName:=UnusedPackage.Owner;
      if (FWorkingParserRoot=nil)
      or (FWorkingParserRoot.UnitLists=nil)
      or (FWorkingParserRoot.UnitLists.FindKey(Pointer(PackageName),
         @CompareAnsiStringWithUnitListOwner)=nil)
      then begin
        Result:=UnusedPackage;
        exit;
      end;
      Node:=FParserRoot.UnitLists.FindSuccessor(Node);
    end;
  end;
  
var
  UnusedPackage: TCodeBrowserUnitList;
begin
  DebugLn(['TCodeBrowserView.WorkFreeUnusedPackages START']);

  // find an unused package
  UnusedPackage:=FindUnusedUnitList;
  if UnusedPackage=nil then begin
    // this stage finished -> next stage
    fStage:=cbwsAddNewPackages;
    ProgressBar1.Position:=ProgressFreeUnusedPkgStart+ProgressFreeUnusedPkgSize;
    exit;
  end;

  // free the unused package
  FreeUnitList(UnusedPackage);
end;

procedure TCodeBrowserView.WorkAddNewUnitLists;
var
  Node: TAVLTreeNode;
  List: TCodeBrowserUnitList;
begin
  ProgressBar1.Position:=ProgressAddNewUnitListsStart;
  if (FWorkingParserRoot<>nil) and (FWorkingParserRoot.UnitLists<>nil)
  and (FParserRoot<>nil) then begin
    Node:=FWorkingParserRoot.UnitLists.FindLowest;
    while Node<>nil do begin
      List:=TCodeBrowserUnitList(Node.Data);
      if FParserRoot.FindUnitList(List.Owner)=nil then begin
        // new unit list
        TCodeBrowserUnitList.Create(List.Owner,FParserRoot);
        inc(FScannedPackages);
      end;
      Node:=FWorkingParserRoot.UnitLists.FindSuccessor(Node);
    end;
  end;
  
  // this stage finished -> next stage
  fStage:=cbwsGatherFiles;
  ProgressBar1.Position:=ProgressAddNewUnitListsStart+ProgressAddNewUnitListsSize;
end;

procedure TCodeBrowserView.WorkGatherFileLists;

  function ListFilesAreValid(List: TCodeBrowserUnitList): boolean;
  begin
    Result:=List.UnitsValid;
  end;

  function FindListWithInvalidFileList(StartList: TCodeBrowserUnitList
    ): TCodeBrowserUnitList;
  var
    APackage: TCodeBrowserUnitList;
    Node: TAVLTreeNode;
  begin
    Result:=nil;
    if StartList=nil then exit;
    if not ListFilesAreValid(StartList) then begin
      Result:=StartList;
      exit;
    end;
    if (StartList.UnitLists=nil) then exit;
    Node:=StartList.UnitLists.FindLowest;
    while Node<>nil do begin
      APackage:=TCodeBrowserUnitList(Node.Data);
      Result:=FindListWithInvalidFileList(APackage);
      if Result<>nil then exit;
      Node:=StartList.UnitLists.FindSuccessor(Node);
    end;
  end;

var
  List: TCodeBrowserUnitList;
begin
  DebugLn(['TCodeBrowserView.WorkGatherFiles START']);
  // find a unit list which needs update
  List:=FindListWithInvalidFileList(FParserRoot);
  if List=nil then begin
    // this stage finished -> next stage
    fStage:=cbwsGatherOutdatedFiles;
    ProgressBar1.Position:=ProgressGatherFileListsStart+ProgressGatherFileListsSize;
    exit;
  end;
  
  WorkUpdateFileList(List);
end;

procedure TCodeBrowserView.WorkUpdateFileList(List: TCodeBrowserUnitList);
var
  NewFileList: TAVLTree;

  procedure AddFile(const Filename: string; ClearIncludedByInfo: boolean);
  begin
    //DebugLn(['AddFile Filename="',Filename,'"']);
    if Filename='' then exit;
    if System.Pos('$',Filename)>0 then begin
      DebugLn(['WARNING: TCodeBrowserView.WorkUpdateFiles Macros in filename ',Filename]);
      exit;
    end;
    if NewFileList.FindKey(Pointer(Filename),@CompareAnsiStringWithUnitFilename)<>nil
    then exit;
    //DebugLn(['TCodeBrowserView.WorkUpdateFiles AddFile ',Filename]);
    NewFileList.Add(TCodeBrowserUnit.Create(Filename));
    if ClearIncludedByInfo then begin
      CodeToolBoss.SourceCache.ClearIncludedByEntry(Filename);
    end;
  end;
  
  procedure AddFilesOfProject(AProject: TProject);
  var
    AnUnitInfo: TUnitInfo;
  begin
    if AProject=nil then exit;
    AnUnitInfo:=AProject.FirstPartOfProject;
    //DebugLn(['AddFilesOfProject ',AnUnitInfo<>nil]);
    while AnUnitInfo<>nil do begin
      //DebugLn(['AddFilesOfProject ',AnUnitInfo.Filename]);
      if FilenameIsPascalUnit(AnUnitInfo.Filename)
      or (AnUnitInfo=aProject.MainUnitInfo) then
        AddFile(AnUnitInfo.Filename,false);
      AnUnitInfo:=AnUnitInfo.NextPartOfProject;
    end;
  end;
  
  procedure AddFilesOfPackageFCL;
  var
    LazDir: String;
    UnitSetID: string;
    UnitSetChanged: Boolean;
    UnitSet: TFPCUnitSetCache;
    Filename: String;
    ConfigCache: TPCTargetConfigCache;
    Node: TAVLTreeNode;
    Item: PStringToStringItem;
  begin
    // use unitset of the lazarus source directory
    LazDir:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory);
    if (LazDir='') or (not FilenameIsAbsolute(LazDir)) then exit;
    UnitSetID:=CodeToolBoss.GetUnitSetIDForDirectory(LazDir);
    if UnitSetID='' then exit;
    UnitSetChanged:=false;
    UnitSet:=CodeToolBoss.CompilerDefinesCache.FindUnitSetWithID(UnitSetID,
                                                          UnitSetChanged,false);
    if UnitSet=nil then exit;
    ConfigCache:=UnitSet.GetConfigCache(false);
    if (ConfigCache=nil) or (ConfigCache.Units=nil) then exit;
    Node:=ConfigCache.Units.Tree.FindLowest;
    while Node<>nil do begin
      Item:=PStringToStringItem(Node.Data);
      Filename:=Item^.Value;
      if (CompareFileExt(Filename,'ppu',false)=0) then begin
        // search source in fpc sources
        Filename:=UnitSet.GetUnitSrcFile(ExtractFileNameOnly(Filename));
      end;
      if FilenameIsPascalUnit(Filename) then
        AddFile(Filename,false);
      Node:=ConfigCache.Units.Tree.FindSuccessor(Node);
    end;
  end;
  
  procedure AddFilesOfPackage(APackage: TLazPackage);
  var
    i: Integer;
    PkgFile: TPkgFile;
    aFilename: String;
  begin
    if APackage=nil then exit;
    for i:=0 to APackage.FileCount-1 do begin
      PkgFile:=APackage.Files[i];
      if (PkgFile.FileType in PkgFileUnitTypes) then begin
        aFilename:=PkgFile.GetFullFilename;
        if not FilenameIsPascalUnit(aFilename) then begin
          debugln(['WARNING: AddFilesOfPackage: package ',APackage.Filename,' has a unit with a non unit extension: ',aFilename]);
        end;
        AddFile(aFilename,true);
      end;
    end;
    if APackage.Name='FCL' then begin
      AddFilesOfPackageFCL;
    end;
  end;
  
  procedure AddFilesOfDirectory(const Directory: string;
    ClearIncludedByInfo: boolean);
  // ! needs ending PathDelim !
  var
    FileInfo: TSearchRec;
  begin
    //DebugLn(['AddFilesOfDirectory Directory="',Directory,'"']);
    if (not FilenameIsAbsolute(Directory))
    or (not DirectoryExistsUTF8(Directory)) then begin
      DebugLn(['AddFilesOfDirectory WARNING: does not exist: "',Directory,'"']);
      exit;
    end;
    if FindFirstUTF8(Directory+FileMask,faAnyFile,FileInfo)=0 then begin
      repeat
        // check if special file
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
        then
          continue;
        if FilenameIsPascalUnit(FileInfo.Name) then
          AddFile(Directory+FileInfo.Name,ClearIncludedByInfo);
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
  end;
  
  procedure AddFilesOfSearchPath(const SrcPath, BaseDir: string;
    ClearIncludedByInfo: boolean);
  var
    Dir: String;
    p: Integer;
  begin
    //DebugLn(['AddFilesOfSearchPath SrcPath="',SrcPath,'" BaseDir="',BaseDir,'"']);
    p:=1;
    while (p<=length(SrcPath)) do begin
      Dir:=GetNextDelimitedItem(SrcPath,';',p);
      if Dir<>'' then begin
        if not FilenameIsAbsolute(Dir) then
          Dir:=BaseDir+PathDelim+Dir;
        Dir:=CleanAndExpandDirectory(Dir);
        AddFilesOfDirectory(Dir,ClearIncludedByInfo);
      end;
    end;
  end;
  
  procedure AddFilesOfIDE;
  var
    LazDefines: TDefineTemplate;
    LazSrcDir: TDefineTemplate;
    LazIDEDir: TDefineTemplate;
    LazIDESrcPath: TDefineTemplate;
    SrcPath: String;
    LazDir: String;
  begin
    LazDir:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory);
    if not DirectoryExistsUTF8(LazDir) then begin
      DebugLn(['AddFilesOfIDE WARNING: lazarus directory not found: "',LazDir,'"']);
      exit;
    end;
    // get the SrcPath template of the lazarus/ide directory
    LazDefines:=CodeToolBoss.DefineTree
                      .FindDefineTemplateByName(StdDefTemplLazarusSources,true);
    if LazDefines=nil then begin
      DebugLn(['AddFilesOfIDE WARNING: codetools define templates for lazarus not found']);
      exit;
    end;
    LazSrcDir:=LazDefines.FindChildByName(StdDefTemplLazarusSrcDir);
    if LazSrcDir=nil then begin
      DebugLn(['AddFilesOfIDE WARNING: codetools define templates for lazarus directory not found']);
      exit;
    end;
    LazIDEDir:=LazSrcDir.FindChildByName('ide');
    if LazIDEDir=nil then begin
      DebugLn(['AddFilesOfIDE WARNING: codetools define templates for lazarus ide directory not found']);
      exit;
    end;
    LazIDESrcPath:=LazIDEDir.FindChildByName('IDE path addition');
    if LazIDESrcPath=nil then begin
      DebugLn(['AddFilesOfIDE WARNING: codetools define templates for src path of lazarus ide directory not found']);
      exit;
    end;
    SrcPath:=StringReplace(LazIDESrcPath.Value,'$(#LazarusDir)',LazDir,
                           [rfReplaceAll, rfIgnoreCase]);
    AddFilesOfSearchPath(SrcPath+';.',LazDir+'ide'+PathDelim,true);
  end;

  procedure DeleteUnusedFiles;
  var
    Node: TAVLTreeNode;
    CurUnit: TCodeBrowserUnit;
    NextNode: TAVLTreeNode;
  begin
    if List.Units=nil then exit;
    Node:=List.Units.FindLowest;
    while Node<>nil do begin
      NextNode:=List.Units.FindSuccessor(Node);
      CurUnit:=TCodeBrowserUnit(Node.Data);
      if NewFileList.FindKey(Pointer(CurUnit.Filename),
        @CompareAnsiStringWithUnitFilename)=nil
      then begin
        // this unit is not part of List anymore -> delete
        RemoveUnit(CurUnit);
        List.DeleteUnit(CurUnit);
      end;
      Node:=NextNode;
    end;
  end;
  
  procedure AddNewFiles;
  var
    Node: TAVLTreeNode;
    AnUnit: TCodeBrowserUnit;
  begin
    Node:=NewFileList.FindLowest;
    while Node<>nil do begin
      AnUnit:=TCodeBrowserUnit(Node.Data);
      //DebugLn(['AddNewFiles ',AnUnit.Filename,' exists=',List.FindUnit(AnUnit.Filename)<>nil]);
      if List.FindUnit(AnUnit.Filename)=nil then begin
        // this unit was not part of List -> add
        //DebugLn(['AddNewFiles "',List.Owner,'" "',AnUnit.Filename,'"']);
        List.AddUnit(AnUnit.Filename);
      end;
      Node:=NewFileList.FindSuccessor(Node);
    end;
  end;

var
  APackage: TLazPackage;
begin
  DebugLn(['TCodeBrowserView.WorkUpdateFiles ',List.Owner]);
  NewFileList:=TAVLTree.Create(@CompareUnitFilenames);
  try
    // get new list of files
    if List.Owner=CodeBrowserIDEName then begin
      AddFilesOfIDE;
    end else if List.Owner=CodeBrowserProjectName then begin
      AddFilesOfProject(Project1);
    end else begin
      APackage:=PackageGraph.FindPackageWithName(List.Owner,nil);
      AddFilesOfPackage(APackage);
    end;
    
    // update file list
    DeleteUnusedFiles;
    AddNewFiles;
    
    List.UnitsValid:=true;
  finally
    NewFileList.FreeAndClear;
    NewFileList.Free;
  end;
end;

procedure TCodeBrowserView.WorkGatherOutdatedFiles;
// add all files to fOutdatedFiles

  procedure AddFile(AnUnit: TCodeBrowserUnit);
  begin
    if fOutdatedFiles=nil then
      fOutdatedFiles:=TAVLTree.Create(@CompareUnitFilenames);
    if fOutdatedFiles.Find(AnUnit)<>nil then exit;
    fOutdatedFiles.Add(AnUnit);
  end;

  procedure AddFiles(List: TCodeBrowserUnitList);
  var
    Node: TAVLTreeNode;
  begin
    if List.Units<>nil then begin
      Node:=List.Units.FindLowest;
      while Node<>nil do begin
        AddFile(TCodeBrowserUnit(Node.Data));
        Node:=List.Units.FindSuccessor(Node);
      end;
    end;
    if List.UnitLists<>nil then begin
      Node:=List.UnitLists.FindLowest;
      while Node<>nil do begin
        AddFiles(TCodeBrowserUnitList(Node.Data));
        Node:=List.UnitLists.FindSuccessor(Node);
      end;
    end;
  end;

begin
  if fOutdatedFiles<>nil then
    fOutdatedFiles.Clear;
  AddFiles(ParserRoot);
  
  // this stage finished -> next stage
  fStage:=cbwsUpdateUnits;
  ProgressBar1.Position:=ProgressGatherOutdatedFilesStart+ProgressGatherOutdatedFilesSize;
end;

procedure TCodeBrowserView.WorkUpdateUnits;

  function FindOutdatedUnit: TCodeBrowserUnit;
  var
    Node: TAVLTreeNode;
  begin
    Result:=nil;
    if fOutdatedFiles=nil then exit;
    Node:=fOutdatedFiles.FindLowest;
    if Node=nil then exit;
    Result:=TCodeBrowserUnit(Node.Data);
  end;

const
  SmallTimeStep = (1/86400)/5;
var
  AnUnit: TCodeBrowserUnit;
  StartTime: TDateTime;
begin
  //DebugLn(['TCodeBrowserView.WorkUpdateUnits START']);
  CodeToolBoss.ActivateWriteLock;
  try
    // parse units
    StartTime:=Now;
    repeat
      AnUnit:=FindOutdatedUnit;
      if AnUnit=nil then begin
        // this stage finished -> next stage
        fStage:=cbwsGetViewOptions;
        ProgressBar1.Position:=ProgressUpdateUnitsStart+ProgressUpdateUnitsSize;
        exit;
      end;

      WorkUpdateUnit(AnUnit);
    until Abs(Now-StartTime)>SmallTimeStep;
  finally
    CodeToolBoss.DeactivateWriteLock;
  end;
end;

procedure TCodeBrowserView.WorkUpdateUnit(AnUnit: TCodeBrowserUnit);

  procedure UpdateScannedCounters(Tool: TCodeTool);
  var
    LineCnt: Integer;
    ByteCnt: Integer;
    i: Integer;
    Link: TSourceLink;
    CodeBuf: TCodeBuffer;
    LastCode: TCodeBuffer;
  begin
    if (Tool=nil) or (Tool.Scanner=nil) then exit;
    LineCnt:=0;
    ByteCnt:=0;
    LastCode:=nil;
    for i:=0 to Tool.Scanner.LinkCount-1 do begin
      Link:=Tool.Scanner.Links[i];
      CodeBuf:=TCodeBuffer(Link.Code);
      if CodeBuf=nil then continue;
      if CodeBuf<>LastCode then begin
        inc(LineCnt,CodeBuf.LineCount);
        inc(ByteCnt,CodeBuf.SourceLength);
        LastCode:=CodeBuf;
      end;
    end;
    AnUnit.ScannedBytes:=ByteCnt;
    AnUnit.ScannedLines:=LineCnt;
    AnUnit.ScannedIdentifiers:=CountIdentifiers(Tool);
    AnUnit.CodeTool:=Tool;
    inc(FScannedBytes,AnUnit.ScannedBytes);
    inc(FScannedLines,AnUnit.ScannedLines);
    inc(FScannedIdentifiers,AnUnit.ScannedIdentifiers);
    //DebugLn(['UpdateScannedCounters ',ExtractFileName(AnUnit.Filename),' LineCnt=',LineCnt,' ByteCnt=',ByteCnt,' ',DbgSName(AnUnit.CodeTool)]);
  end;

var
  MainCodeBuf: TCodeBuffer;
  Tool: TCodeTool;
begin
  //DebugLn(['TCodeBrowserView.WorkUpdateUnit START ',AnUnit.Filename]);
  // mark as updated
  fOutdatedFiles.Remove(AnUnit);
  // reset scanning counters
  if AnUnit.Scanned then begin
    dec(FScannedBytes,AnUnit.ScannedBytes);
    dec(FScannedLines,AnUnit.ScannedLines);
    dec(FScannedIdentifiers,AnUnit.ScannedIdentifiers);
    AnUnit.ScannedBytes:=0;
    AnUnit.ScannedLines:=0;
    AnUnit.ScannedIdentifiers:=0;
    dec(FScannedUnits);
  end;
  AnUnit.Scanned:=true;
  inc(FScannedUnits);
  // load the file
  AnUnit.CodeBuffer:=CodeToolBoss.LoadFile(AnUnit.Filename,false,false);
  if AnUnit.CodeBuffer=nil then exit;
  // check if this is a unit
  MainCodeBuf:=CodeToolBoss.GetMainCode(AnUnit.CodeBuffer);
  if MainCodeBuf<>AnUnit.CodeBuffer then begin
    // this file was used as an include file
    DebugLn(['TCodeBrowserView.WorkUpdateUnit HINT: this is not a unit: ',AnUnit.Filename,
      ' (it was included by ',MainCodeBuf.Filename,')']);
    exit;
  end;
  // scan
  CodeToolBoss.Explore(AnUnit.CodeBuffer,Tool,false,true);
  UpdateScannedCounters(Tool);
  //DebugLn(['TCodeBrowserView.WorkUpdateUnit END ',AnUnit.Filename]);
end;

procedure TCodeBrowserView.WorkGetViewOptions;
var
  NewLevels: TStringList;
begin
  //DebugLn(['TCodeBrowserView.WorkGetViewOptions START']);
  Options.ShowPrivate:=ShowPrivateCheckBox.Checked;
  Options.ShowProtected:=ShowProtectedCheckBox.Checked;
  Options.ShowEmptyNodes:=ShowEmptyNodesCheckBox.Checked;

  // levels
  NewLevels:=TStringList.Create;
  if ShowPackagesCheckBox.Checked then
    NewLevels.Add(CodeBrowserLevelNames[cblPackages]);
  if ShowUnitsCheckBox.Checked then
    NewLevels.Add(CodeBrowserLevelNames[cblUnits]);
  if ShowIdentifiersCheckBox.Checked then
    NewLevels.Add(CodeBrowserLevelNames[cblIdentifiers]);
  Options.Levels:=NewLevels;
  NewLevels.Free;

  // level filter
  Options.LevelFilterText[cblPackages]:=PackageFilterEdit.Text;
  if PackageFilterBeginsSpeedButton.Down then
    Options.LevelFilterType[cblPackages]:=cbtfBegins;
  if PackageFilterContainsSpeedButton.Down then
    Options.LevelFilterType[cblPackages]:=cbtfContains;

  Options.LevelFilterText[cblUnits]:=UnitFilterEdit.Text;
  //DebugLn(['TCodeBrowserView.WorkGetOptions UnitFIlter=',Options.LevelFilterText[cblUnits],' Edit=',UnitFilterEdit.Text]);
  if UnitFilterBeginsSpeedButton.Down then
    Options.LevelFilterType[cblUnits]:=cbtfBegins;
  if UnitFilterContainsSpeedButton.Down then
    Options.LevelFilterType[cblUnits]:=cbtfContains;

  Options.LevelFilterText[cblIdentifiers]:=IdentifierFilterEdit.Text;
  if IdentifierFilterBeginsSpeedButton.Down then
    Options.LevelFilterType[cblIdentifiers]:=cbtfBegins;
  if IdentifierFilterContainsSpeedButton.Down then
    Options.LevelFilterType[cblIdentifiers]:=cbtfContains;

  DebugLn(['TCodeBrowserView.WorkGetViewOptions UpdateNeeded=',UpdateNeeded,' ChangeStamp=',Options.ChangeStamp<>FOptionsChangeStamp]);

  // this stage finished -> next stage
  if UpdateNeeded or (Options.ChangeStamp<>FOptionsChangeStamp) then
    fStage:=cbwsUpdateTreeView
  else
    fStage:=cbwsFinished;
  ProgressBar1.Position:=ProgressGetViewOptionsStart+ProgressGetViewOptionsSize;
end;

procedure TCodeBrowserView.WorkUpdateTreeView;
begin
  ProgressBar1.Position:=ProgressUpdateTreeViewStart;
  UpdateTreeView;
  // this stage finished -> next stage
  fStage:=cbwsFinished;
  ProgressBar1.Position:=ProgressUpdateTreeViewStart+ProgressUpdateTreeViewSize;
end;

procedure TCodeBrowserView.FreeUnitList(List: TCodeBrowserUnitList);
var
  Node: TAVLTreeNode;
  AnUnit: TCodeBrowserUnit;
begin
  //DebugLn(['TCodeBrowserView.FreeUnitList ',List.Owner]);
  dec(FScannedPackages);
  if List.Units<>nil then begin
    Node:=List.Units.FindLowest;
    while Node<>nil do begin
      AnUnit:=TCodeBrowserUnit(Node.Data);
      RemoveUnit(AnUnit);
      Node:=List.Units.FindSuccessor(Node);
    end;
  end;
  List.Free;
end;

procedure TCodeBrowserView.UpdateStatusBar(Lazy: boolean);
const
  SmallTimeStep = 1/86400;

  function BigIntToStr(i: integer): string;
  var
    p: Integer;
    ThousandSep: String;
  begin
    if i=0 then begin
      Result:='0';
      exit;
    end;
    Result:='';
    if i>=100000 then begin
      i:=i div 1000;
      Result:='k';
      if i>=100000 then begin
        i:=i div 1000;
        Result:='m';
        if i>=100000 then begin
          i:=i div 1000;
          Result:='g';
          if i>=100000 then begin
            i:=i div 1000;
            Result:='t';
          end;
        end;
      end;
    end;

    p:=0;
    ThousandSep:=AnsiToUTF8(DefaultFormatSettings.ThousandSeparator);
    while i>0 do begin
      if p=3 then begin
        Result:=ThousandSep+Result;
        p:=0;
      end;
      Result:=chr((i mod 10)+ord('0'))+Result;
      i:=i div 10;
      inc(p);
    end;
  end;
var
  s: String;
begin
  if Lazy and (Abs(Now-fLastStatusBarUpdate)<SmallTimeStep) then begin
    // the last update is not long ago
    // => skip update
    exit;
  end;
  fLastStatusBarUpdate:=Now;
  s:=Format(lisPackagesUnitsIdentifiersLinesBytes, [BigIntToStr(VisiblePackages)
    , BigIntToStr(ScannedPackages), BigIntToStr(VisibleUnits), BigIntToStr(
    ScannedUnits), BigIntToStr(VisibleIdentifiers), BigIntToStr(
    ScannedIdentifiers), BigIntToStr(ScannedLines), BigIntToStr(ScannedBytes)]);
  if fStage<>cbwsFinished then
    s:=Format(lisScanning2, [s]);
  StatusBar1.SimpleText:=s;
end;

function TCodeBrowserView.GetCodeTool(AnUnit: TCodeBrowserUnit): TStandardCodeTool;
begin
  //DebugLn(['TCodeBrowserView.GetCodeTool ',AnUnit.CodeTool<>nil,' ',AnUnit.CodeBuffer<>nil]);
  Result:=AnUnit.CodeTool;
  if Result<>nil then exit;
  if AnUnit.CodeBuffer=nil then exit;
  Result:=CodeToolBoss.GetCodeToolForSource(AnUnit.CodeBuffer,true,false)
              as TCodeTool;
  AnUnit.CodeTool:=Result;
  //DebugLn(['TCodeBrowserView.GetCodeTool END ',AnUnit.Filename,' ',Result<>nil]);
end;

procedure TCodeBrowserView.GetNodeIdentifier(Tool: TStandardCodeTool;
  CTNode: TCodeTreeNode; out Identifier: string);

  function Shorten(const s: string): string;
  const
    MAX_LEN=100;
  begin
    Result:=DbgStr(s);
    if Length(Result)>MAX_LEN then
      Result:=LeftStr(Result, MAX_LEN)+'...';
  end;

begin
  if CTNode.StartPos>=CTNode.EndPos then begin
    Identifier:='';
    exit;
  end;
  case CTNode.Desc of
  ctnProcedure:
    begin
      Identifier:=Tool.ExtractProcName(CTNode,ProcIdentifierFlags);
    end;
  ctnVarDefinition:
    begin
      Identifier:=Tool.ExtractDefinitionName(CTNode);
    end;
  ctnConstDefinition:
    begin
      Identifier:=Tool.ExtractDefinitionName(CTNode);
    end;
  ctnTypeDefinition,ctnGenericType:
    begin
      Identifier:=Tool.ExtractDefinitionName(CTNode);
    end;
  ctnProperty:
    begin
      Identifier:=Tool.ExtractPropName(CTNode,false);
    end;
  ctnEnumIdentifier:
    begin
      Identifier:=Tool.ExtractIdentifier(CTNode.StartPos);
    end;
  end;
end;

procedure TCodeBrowserView.GetNodeDescription(Tool: TStandardCodeTool;
  CTNode: TCodeTreeNode; Identifier: string; out Description: string);

  function Shorten(const s: string): string;
  const
    MAX_LEN=100;
  begin
    Result:=DbgStr(s);
    if Length(Result)>MAX_LEN then
      Result:=LeftStr(Result, MAX_LEN)+'...';
  end;

const
  NodeFlags = [];
var
  Inheritance: String;
begin
  if CTNode.StartPos>=CTNode.EndPos then begin
    Description:='';
    exit;
  end;
  case CTNode.Desc of
  ctnProcedure:
    begin
      Description:=Tool.ExtractProcHead(CTNode,ProcDescFlags);
    end;
  ctnVarDefinition:
    begin
      Description:='var '+Identifier
                 +' : '+Shorten(Tool.ExtractDefinitionNodeType(CTNode));
    end;
  ctnConstDefinition:
    begin
      Description:='const '+Shorten(Tool.ExtractNode(CTNode,NodeFlags));
    end;
  ctnTypeDefinition,ctnGenericType:
    begin
      Description:='type '+Identifier;
      if CTNode.FirstChild<>nil then begin
        case CTNode.FirstChild.Desc of
        ctnClass,ctnClassInterface,ctnObject,
        ctnObjCClass,ctnObjCCategory,ctnObjCProtocol,
        ctnCPPClass,
        ctnClassHelper,ctnRecordHelper,ctnTypeHelper:
          begin
            case CTNode.FirstChild.Desc of
            ctnClassInterface:
              Description:=Description+' = interface';
            ctnObject:
              Description:=Description+' = object';
            ctnObjCClass:
              Description:=Description+' = objcclass';
            ctnObjCCategory:
              Description:=Description+' = objccategory';
            ctnObjCProtocol:
              Description:=Description+' = objcprotocol';
            ctnCPPClass:
              Description:=Description+' = cppclass';
            ctnClassHelper:
              Description:=Description+' = class helper';
            ctnRecordHelper:
              Description:=Description+' = record helper';
            ctnTypeHelper:
              Description:=Description+' = type helper';
            else
              Description:=Description+' = class';
            end;
            Inheritance:=Tool.ExtractClassInheritance(CTNode.FirstChild,[]);
            if Inheritance<>'' then
              Description:=Description+'('+Inheritance+')';
          end;
        ctnRecordType:
          Description:=Description+' = record';
        end;
      end;
    end;
  ctnProperty:
    begin
      Description:='property '+Shorten(Tool.ExtractProperty(CTNode,PropDescFlags));
    end;
  ctnEnumIdentifier:
    begin
      Description:='enum '+Identifier;
    end;
  end;
end;

procedure TCodeBrowserView.UpdateTreeView;
var
  ShowPackages: boolean;
  ShowUnits: boolean;
  ShowIdentifiers: boolean;
  ShowPrivate: boolean;
  ShowProtected: boolean;
  ShowEmptyNodes: boolean;
  NewPackageCount: integer;
  NewUnitCount: integer;
  NewIdentifierCount, ShownIdentifierCount: PtrInt;
  UsedMem: PtrUInt;
  
  LevelFilterText: array[TCodeBrowserLevel] of string;
  LevelFilterType: array[TCodeBrowserLevel] of TCodeBrowserTextFilter;

  function IncUsedMem(c: integer): boolean;
  begin
    Result:=(UsedMem div 16384)<>((UsedMem{%H-}+c) div 16384);
    {%H-}inc(UsedMem,c);
  end;

  function IdentifierFitsFilter(LvlType: TCodeBrowserLevel;
    const Identifier: string): boolean;
  begin
    //DebugLn(['IdentifierFitsFilter Identifier=',Identifier,' Filter=',LevelFilterText[LvlType]]);
    if (LevelFilterText[LvlType]='') then exit(true);
    if Identifier='' then exit(false);

    case LevelFilterType[LvlType] of
    cbtfBegins:
      Result:=ComparePrefixIdent(PChar(Pointer(LevelFilterText[LvlType])),
                                 PChar(Pointer(Identifier)));
    cbtfContains:
      begin
      Result:=IdentifierPos(PChar(Pointer(LevelFilterText[LvlType])),
                            PChar(Pointer(Identifier)))>=0;
      //if Result then
      //  debugln(['IdentifierFitsFilter Identifier="',Identifier,'" Filter="',LevelFilterText[LvlType],'"']);
      end
    else
      Result:=true;
    end;
  end;

  procedure AddUnitNodes(SrcUnit: TCodeBrowserUnit; var DestUnit: TObject);
  var
    CTTool: TStandardCodeTool;
    
    procedure AddChildNode(ParentBrowserNode: TCodeBrowserNode;
      CTNode: TCodeTreeNode);
    var
      NewChildNode: TCodeBrowserNode;
      ChildDescription, ChildIdentifier: string;
      NewCodePos: TCodePosition;
    begin
      //DebugLn(['AddChildNode ',ChildCTNode.DescAsString,' ',ChildDescription]);
      if ShownIdentifierCount>=CodeBrowserMaxTVIdentifiers then exit;

      if (CTNode.Parent.Desc=ctnClassPrivate) and (not ShowPrivate) then
        exit;
      if (CTNode.Parent.Desc=ctnClassProtected) and (not ShowProtected)
      then
        exit;
      GetNodeIdentifier(CTTool,CTNode,ChildIdentifier);

      if IdentifierFitsFilter(cblIdentifiers,ChildIdentifier) then begin
        inc(ShownIdentifierCount);
        GetNodeDescription(CTTool,CTNode,ChildIdentifier,ChildDescription);
        NewChildNode:=ParentBrowserNode.AddNode(ChildDescription,ChildIdentifier);
        if NewChildNode<>nil then begin
          NewChildNode.Desc:=CTNode.Desc;
          CTTool.CleanPosToCodePos(CTNode.StartPos,NewCodePos);
          NewChildNode.CodePos:=NewCodePos;
          {$IFDEF VerboseCodeBrowser}
          if (length(ChildDescription)>1000) then
            debugln(['AddChildNode WARNING: big description ',SrcUnit.Filename,' desc=',ChildDescription]);
          if IncUsedMem(NewChildNode.GetMemSize) then
            debugln(['AddChildNode used mem ',UsedMem]);
          {$ENDIF}
        end;
      end;
    end;

    procedure AddIdentifierNode(CTNode: TCodeTreeNode);
    var
      NewNode: TCodeBrowserNode;
      ChildCTNode: TCodeTreeNode;
      Description, Identifier: string;
      CurUnit: TCodeBrowserUnit;
      NewCodePos: TCodePosition;
    begin
      if not ShowIdentifiers then exit;
      if ShownIdentifierCount>CodeBrowserMaxTVIdentifiers then exit;

      if DestUnit=nil then
        DestUnit:=TCodeBrowserUnit.Create('');
      CurUnit:=TCodeBrowserUnit(DestUnit);
      //DebugLn(['AddIdentifierNode ',CTNode.DescAsString,' Description="',Description,'"']);
      GetNodeIdentifier(CTTool,CTNode,Identifier);
      NewNode:=CurUnit.AddNode('',Identifier);
      {$IFDEF VerboseCodeBrowser}
      if (length(Description)>100) then
        debugln(['AddIdentifierNode WARNING: big description ',CurUnit.Filename,' desc=',Description]);
      if IncUsedMem(NewNode.GetMemSize) then
        debugln(['AddIdentifierNode used mem ',UsedMem,' ',CurUnit.Filename,' ',CurUnit.ChildNodeCount]);
      {$ENDIF}
      NewNode.Desc:=CTNode.Desc;
      CTTool.CleanPosToCodePos(CTNode.StartPos,NewCodePos);
      NewNode.CodePos:=NewCodePos;
      //DebugLn(['AddIdentifierNode Code=',NewNode.FCodePos.Code<>nil,' P=',NewNode.FCodePos.P]);
      
      if (CTNode.Desc in [ctnTypeDefinition,ctnGenericType])
      and (CTNode.FirstChild<>nil)
      and (CTNode.FirstChild.Desc in AllClasses+[ctnRecordType,ctnEnumerationType])
      then begin
        // add child nodes
        ChildCTNode:=CTNode.FirstChild;
        while (ChildCTNode<>nil) and (ChildCTNode.StartPos<CTNode.EndPos) do
        begin
          if ChildCTNode.Desc in
          [ctnProcedure,ctnVarDefinition,ctnProperty,ctnEnumIdentifier]
          then begin
            AddChildNode(NewNode,ChildCTNode);
          end;
          if ChildCTNode.Desc=ctnProcedureHead then
            ChildCTNode:=ChildCTNode.NextSkipChilds
          else
            ChildCTNode:=ChildCTNode.Next;
        end;
      end;
      
      if (NewNode.ChildNodes=nil)
      and (not IdentifierFitsFilter(cblIdentifiers,Identifier)) then begin
        // identifier is not needed -> remove
        // ToDo: remove nodes later
        CurUnit.DeleteNode(NewNode);
      end else begin
        // keep node, set Description
        GetNodeDescription(CTTool,CTNode,Identifier,Description);
        NewNode.Description:=Description;
        inc(ShownIdentifierCount);
      end;
    end;
    
  var
    CTNode: TCodeTreeNode;
  begin
    if SrcUnit=nil then exit;
    //DebugLn(['AddUnitNodes SrcUnit.Filename="',SrcUnit.Filename,'"']);
    CTTool:=GetCodeTool(SrcUnit);
    if CTTool=nil then exit;
    if CTTool.Tree=nil then exit;
    
    CTNode:=CTTool.Tree.Root;
    while CTNode<>nil do begin
      //DebugLn(['AddUnitNodes ',CTNode.DescAsString]);
      case CTNode.Desc of
      ctnProcedure:
        if not CTTool.NodeIsMethodBody(CTNode) then
          AddIdentifierNode(CTNode);
      ctnVarDefinition,ctnConstDefinition,ctnTypeDefinition,ctnGenericType:
        if not CTTool.NodeIsForwardDeclaration(CTNode) then
          AddIdentifierNode(CTNode);
      end;

      // go to next node
      case CTNode.Desc of
      ctnProgram,ctnLibrary,ctnPackage,ctnUnit,ctnInterface,
      ctnTypeSection,ctnConstSection,ctnVarSection,ctnResStrSection:
        // go into child nodes
        CTNode:=CTNode.Next;
      ctnImplementation, ctnBeginBlock, ctnAsmBlock: break;
      else
        // skip children and go to next sibling or parent
        CTNode:=CTNode.NextSkipChilds;
      end;
    end;
  end;
  
  procedure AddUnits(SrcList: TCodeBrowserUnitList; var DestParentList: TCodeBrowserUnitList);
  var
    Node: TAVLTreeNode;
    CurUnit, NewUnit: TCodeBrowserUnit;
    List, OldDestParentList: TCodeBrowserUnitList;
  begin
    if SrcList=nil then exit;
    //DebugLn(['AddUnits SrcList.Owner="',SrcList.Owner,'" HasUnits=',SrcList.Units<>nil]);
    if SrcList.Units=nil then exit;
    OldDestParentList:=DestParentList;
    NewUnit:=nil;
    Node:=SrcList.Units.FindLowest;
    while Node<>nil do begin
      CurUnit:=TCodeBrowserUnit(Node.Data);
      if (CurUnit.Filename='')
      or IdentifierFitsFilter(cblUnits,ExtractFileNameOnly(CurUnit.Filename))
      then begin
        if DestParentList=nil then
          DestParentList:=TCodeBrowserUnitList.Create(CodeBrowserHidden,nil);
        List:=DestParentList;
        if ShowUnits then begin
          // create a unit node
          NewUnit:=List.AddUnit(CurUnit.Filename);
          NewUnit.CodeBuffer:=CurUnit.CodeBuffer;
          NewUnit.CodeTool:=CurUnit.CodeTool;
        end else if NewUnit=nil then begin
          // create a dummy unit node to add all identifiers
          NewUnit:=List.FindUnit('');
          if NewUnit=nil then
            NewUnit:=List.AddUnit('');
        end;
        //DebugLn(['AddUnits AddUnitNodes ',CurUnit.Filename]);
        AddUnitNodes(CurUnit,TObject(NewUnit));
        if (not ShowEmptyNodes) and (NewUnit.ChildNodeCount=0) then begin
          // remove empty unit
          List.DeleteUnit(NewUnit);
          NewUnit:=nil;
          if OldDestParentList=nil then begin
            FreeAndNil(DestParentList);
          end;
        end;
        if (NewUnit<>nil) and (NewUnit.UnitList=nil) and (List<>nil) then
          List.AddUnit(NewUnit);
      end;
      Node:=SrcList.Units.FindSuccessor(Node);
    end;
  end;

  procedure AddUnitLists(SrcList: TCodeBrowserUnitList;
    var DestParentList: TCodeBrowserUnitList);
  var
    Node: TAVLTreeNode;
    SubList, NewList, OldDestParentList: TCodeBrowserUnitList;
    NewListCreated, CreateNode: Boolean;
  begin
    if SrcList=nil then exit;
    //DebugLn(['AddUnitLists SrcList.Owner="',SrcList.Owner,'"']);

    OldDestParentList:=DestParentList;
    
    // check filter
    CreateNode:=IdentifierFitsFilter(cblPackages,SrcList.Owner);
    
    // create node
    NewListCreated:=false;
    NewList:=Nil;
    if CreateNode then begin
      if ShowPackages then begin
        if DestParentList=nil then
          DestParentList:=TCodeBrowserUnitList.Create(CodeBrowserHidden,nil);
        NewList:=TCodeBrowserUnitList.Create(SrcList.Owner,DestParentList);
        NewListCreated:=true;
      end else
        NewList:=DestParentList;
    end;
    // create nodes for unitlists
    if SrcList.UnitLists<>nil then begin
      Node:=SrcList.UnitLists.FindLowest;
      while Node<>nil do begin
        SubList:=TCodeBrowserUnitList(Node.Data);
        AddUnitLists(SubList,DestParentList);// DestParentList because: as sibling not as child!
        Node:=SrcList.UnitLists.FindSuccessor(Node);
      end;
    end;
    if CreateNode then begin
      // create nodes for units
      AddUnits(SrcList,NewList);
      // remove empty unit lists
      if (not ShowEmptyNodes) and NewListCreated and (NewList.IsEmpty) then begin
        //DebugLn(['AddUnitLists EMPTY ',NewList.Owner,' ',NewList.UnitListCount,' ',NewList.UnitCount]);
        if DestParentList=NewList then
          DestParentList:=nil;
        FreeAndNil(NewList);
        if (OldDestParentList=nil) and (DestParentList<>nil) and DestParentList.IsEmpty then
          FreeAndNil(DestParentList);
      end;
      // update DestParentList
      if (DestParentList=nil) then
        DestParentList:=NewList;
    end;
  end;

  procedure AddTreeNodes(CodeNode: TObject; ParentViewNode: TTreeNode);
  var
    TVNode: TTreeNode;

    procedure RecursiveAdd(Tree: TAVLTree);
    var
      Node: TAVLTreeNode;
    begin
      if Tree<>nil then begin
        Node:=Tree.FindLowest;
        while Node<>nil do begin
          AddTreeNodes(TObject(Node.Data), TVNode);
          Node:=Tree.FindSuccessor(Node);
        end;
      end;
    end;

    {off $DEFINE DisableTreeViewNodes}
    procedure AddToTreeView(Name: String);
    begin
      {$IFNDEF DisableTreeViewNodes}
      TVNode:=BrowseTreeView.Items.AddChildObject(
        ParentViewNode, Name, CodeNode);
      TVNode.ImageIndex:=GetNodeImage(CodeNode);
      TVNode.SelectedIndex:=TVNode.ImageIndex;
      {$ENDIF}
    end;

  // create visual nodes (TTreeNode)
  var
    CurList: TCodeBrowserUnitList;
    CurListName: String;
    CurUnit: TCodeBrowserUnit;
    CurUnitName: String;
    CurTool: TStandardCodeTool;
    CurNode: TCodeBrowserNode;
    ExpandParent: Boolean;
  begin
    if CodeNode=nil then exit;
    ExpandParent:=true;
    //DebugLn(['AddTreeNodes ',DbgSName(CodeNode)]);
    TVNode:=ParentViewNode;

    if CodeNode is TCodeBrowserUnitList then begin
      CurList:=TCodeBrowserUnitList(CodeNode);
      //DebugLn(['AddTreeNodes ',CurList.Owner]);
      if CurList.Owner=CodeBrowserHidden then begin
        TVNode:=ParentViewNode;
      end else begin
        CurListName:=ListOwnerToText(CurList.Owner);
        inc(NewPackageCount);
        AddToTreeView(CurListName);
      end;
      RecursiveAdd(CurList.UnitLists);
      RecursiveAdd(CurList.Units);
    end
    else if CodeNode is TCodeBrowserUnit then begin
      CurUnit:=TCodeBrowserUnit(CodeNode);
      CurTool:=nil;
      if CurUnit.Filename<>'' then
        CurTool:=GetCodeTool(CurUnit);
      if CurTool<>nil then begin
        // add a tree node for this unit
        CurUnitName:=TCodeTool(CurTool).GetCachedSourceName;
        if CurUnitName='' then
          CurUnitName:=ExtractFileNameOnly(CurTool.MainFilename);
        inc(NewUnitCount);
        AddToTreeView(CurUnitName);
      end else begin
        // do not add a tree node for this unit
        TVNode:=ParentViewNode;
      end;
      // create tree nodes for code nodes
      RecursiveAdd(CurUnit.ChildNodes);
    end
    else if CodeNode is TCodeBrowserNode then begin
      CurNode:=TCodeBrowserNode(CodeNode);
      if CurNode.Description<>'' then begin
        inc(NewIdentifierCount);
        //if (NewIdentifierCount mod 100)=0 then
        //  DebugLn(['AddTreeNodes ',NewIdentifierCount,' ',CurNode.Description]);
        AddToTreeView(CurNode.Description);
        // create tree nodes for child code nodes
        RecursiveAdd(CurNode.ChildNodes);
        // do not expand unit nodes
        if (ParentViewNode<>nil)
        and (TObject(ParentViewNode.Data) is TCodeBrowserUnit) then
          ExpandParent:=false;
      end;
    end;
    if ParentViewNode<>nil then
      ParentViewNode.Expanded:=ExpandParent;
  end;

var
  lvl: TCodeBrowserLevel;
  i: Integer;
begin
  UsedMem:=0;
  ShowPackages:=Options.HasLevel(cblPackages);
  ShowUnits:=Options.HasLevel(cblUnits);
  ShowIdentifiers:=Options.HasLevel(cblIdentifiers);
  ShowPrivate:=Options.ShowPrivate;
  ShowProtected:=Options.ShowProtected;
  ShowEmptyNodes:=Options.ShowEmptyNodes;
  NewPackageCount:=0;
  NewUnitCount:=0;
  NewIdentifierCount:=0;
  ShownIdentifierCount:=0;

  for lvl:=Low(TCodeBrowserLevel) to High(TCodeBrowserLevel) do begin
    LevelFilterText[lvl]:=Options.LevelFilterText[lvl];
    LevelFilterType[lvl]:=Options.LevelFilterType[lvl];
    debugln(['TCodeBrowserView.UpdateTreeView lvl=',ord(lvl),' type=',ord(LevelFilterType[lvl]),' filter="',LevelFilterText[lvl],'"']);
  end;

  //DebugLn(['TCodeBrowserView.UpdateTreeView ShowPackages=',ShowPackages,' ShowUnits=',ShowUnits,' ShowIdentifiers=',ShowIdentifiers]);

  BrowseTreeView.Cursor:=crHourGlass;
  BrowseTreeView.BeginUpdate;
  CodeToolBoss.ActivateWriteLock;
  try
    InitTreeView;

    // create internal nodes
    AddUnitLists(ParserRoot,fViewRoot);

    // create treeview nodes
    AddTreeNodes(ViewRoot,nil);

    // if there are only a few items expand the whole tree
    if BrowseTreeView.Items.Count<30 then
      for i:=0 to BrowseTreeView.Items.TopLvlCount-1 do
        BrowseTreeView.Items.TopLvlItems[i].Expand(true);
  finally
    CodeToolBoss.DeactivateWriteLock;
    //DebugLn(['TCodeBrowserView.UpdateTreeView EndUpdate']);
    BrowseTreeView.EndUpdate;
    //DebugLn(['TCodeBrowserView.UpdateTreeView AFER ENDUPDATE']);
    BrowseTreeView.Cursor:=crDefault;
  end;
  VisiblePackages:=NewPackageCount;
  VisibleUnits:=NewUnitCount;
  VisibleIdentifiers:=NewIdentifierCount;
  UpdateStatusBar(false);
end;

procedure TCodeBrowserView.RemoveUnit(AnUnit: TCodeBrowserUnit);
begin
  if AnUnit.Scanned then begin
    dec(FScannedUnits);
    dec(FScannedLines,AnUnit.ScannedLines);
    dec(FScannedBytes,AnUnit.ScannedBytes);
    dec(FScannedIdentifiers,AnUnit.ScannedIdentifiers);
    AnUnit.Scanned:=false;
    if fOutdatedFiles<>nil then
      fOutdatedFiles.Remove(AnUnit);
  end;
end;

function TCodeBrowserView.CountIdentifiers(Tool: TCodeTool): integer;
var
  Node: TCodeTreeNode;
begin
  Result:=0;
  if (Tool=nil) or (Tool.Tree=nil) then exit;
  Node:=Tool.Tree.Root;
  while Node<>nil do begin
    if Node.Desc=ctnImplementation then break;
    if (Node.Desc in (AllIdentifierDefinitions+[ctnProcedure,ctnProperty]))
    and (not Tool.NodeIsForwardDeclaration(Node)) then
      inc(Result);
    if not (Node.Desc in [ctnProcedure,ctnBeginBlock,ctnAsmBlock]) then
      Node:=Node.Next
    else
      Node:=Node.NextSkipChilds;
  end;
end;

procedure TCodeBrowserView.ClearTreeView;
begin
  BrowseTreeView.Items.Clear;
  FreeAndNil(FViewRoot);
end;

procedure TCodeBrowserView.InitTreeView;
begin
  ClearTreeView;
end;

function TCodeBrowserView.ListOwnerToText(const ListOwner: string): string;
begin
  if ListOwner=CodeBrowserIDEName then
    Result:=IDEDescription
  else if ListOwner=CodeBrowserProjectName then
    Result:=ProjectDescription
  else
    Result:=ListOwner;
end;

function TCodeBrowserView.GetNodeImage(CodeNode: TObject): integer;
var
  List: TCodeBrowserUnitList;
  Node: TCodeBrowserNode;
begin
  Result:=ImgIDDefault;
  
  if CodeNode is TCodeBrowserUnit then begin
    Result:=ImgIDUnitCode;
  end else if CodeNode is TCodeBrowserUnitList then begin
    List:=TCodeBrowserUnitList(CodeNode);
    if List.Owner=IDEDescription then
      Result:=ImgIDProject
    else if List.Owner=ProjectDescription then
      Result:=ImgIDProject
    else
      Result:=ImgIDPackage;
  end else if CodeNode is TCodeBrowserNode then begin
    Node:=TCodeBrowserNode(CodeNode);
    case Node.Desc of
    ctnProgram,ctnLibrary,ctnPackage:
      Result:=ImgIDProgramCode;
    ctnUnit:
      Result:=ImgIDUnitCode;
    ctnInterface:
      Result:=ImgIDInterfaceSection;
    ctnImplementation:
      Result:=ImgIDImplementation;
    ctnInitialization:
      Result:=ImgIDInitialization;
    ctnFinalization:
      Result:=ImgIDFinalization;
    ctnTypeSection:
      Result:=ImgIDTypeSection;
    ctnTypeDefinition,ctnGenericType:
      Result:=ImgIDType;
    ctnVarSection:
      Result:=ImgIDVarSection;
    ctnVarDefinition:
      Result:=ImgIDVariable;
    ctnConstSection,ctnResStrSection:
      Result:=ImgIDConstSection;
    ctnConstDefinition:
      Result:=ImgIDConst;
    ctnClass,ctnObject,ctnObjCClass,ctnObjCCategory,ctnObjCProtocol,ctnCPPClass:
      Result:=ImgIDClass;
    ctnProcedure:
      Result:=ImgIDProc;
    ctnProperty:
      Result:=ImgIDProperty;
    end;
  end;
end;

function TCodeBrowserView.GetTVNodeHint(TVNode: TTreeNode): string;
var
  NodeData: TObject;
  CurUnit: TCodeBrowserUnit;
  Node: TCodeBrowserNode;
  Line, Column: integer;
  BaseURL, HTMLHint: String;
begin
  Result:='';
  if (TVNode=nil) or (TVNode.Data=nil) then exit;
  NodeData:=TObject(TVNode.Data);
  //DebugLn(['TCodeBrowserView.GetTVNodeHint ',DbgSName(NodeData)]);
  if NodeData is TCodeBrowserUnitList then begin

  end else if NodeData is TCodeBrowserUnit then begin
    CurUnit:=TCodeBrowserUnit(NodeData);
    if CurUnit.Filename<>'' then
      Result:=TVNode.Text+LineEnding+CurUnit.Filename;
  end else if NodeData is TCodeBrowserNode then begin
    Node:=TCodeBrowserNode(NodeData);
    if Node.CodePos.Code<>nil then begin
      Result:=TVNode.Text+LineEnding+Node.CodePos.Code.Filename;
      Node.CodePos.Code.AbsoluteToLineCol(Node.CodePos.P,Line,Column);
      if Line>0 then
        Result:=Result+' ('+IntToStr(Line)+','+IntToStr(Column)+')';
        if GetCodeHelp(TVNode, BaseURL, HTMLHint) then
          Result := HTMLHint;
    end;
  end;
end;

function TCodeBrowserView.GetCodeHelp(TVNode: TTreeNode; out BaseURL,
  HTMLHint: string): boolean;
var
  NodeData: TObject;
  Node: TCodeBrowserNode;
  Tool: TCodeTool;
  CleanPos: integer;
  CTNode: TCodeTreeNode;
  NewCodePos: TCodeXYPosition;
begin
  Result:=false;
  BaseURL:='';
  HTMLHint:='';
  if (TVNode=nil) or (TVNode.Data=nil) then exit;
  NodeData:=TObject(TVNode.Data);
  if NodeData is TCodeBrowserNode then begin
    Node:=TCodeBrowserNode(NodeData);
    if Node.CodePos.Code=nil then exit;
    if not LazarusIDE.BeginCodeTools then // commit source editor changes to codetools
      exit;
    // parse unit
    CodeToolBoss.Explore(Node.CodePos.Code,Tool,false,false);
    if Tool=nil then exit;
    // find source position in parsed code
    if Tool.CodePosToCleanPos(Node.CodePos,CleanPos)<>0 then exit;
    // find node
    CTNode:=Tool.FindDeepestNodeAtPos(CleanPos,false);
    if (CTNode=nil) or (CTNode.Desc<>Node.Desc) then
      exit; // source has changed

    // find cleanpos of identifier
    case CTNode.Desc of
    ctnProcedure:
      begin
        if SysUtils.CompareText(Tool.ExtractProcName(CTNode,ProcIdentifierFlags),
          Node.Identifier)<>0
        then
          exit; // source has changed
        Tool.MoveCursorToProcName(CTNode,true);
        CleanPos:=Tool.CurPos.StartPos;
      end;
    ctnProperty:
      begin
        if SysUtils.CompareText(Tool.ExtractPropName(CTNode,false),Node.Identifier)<>0
        then
          exit; // source has changed
        Tool.MoveCursorToPropName(CTNode);
        CleanPos:=Tool.CurPos.StartPos;
      end;
    ctnGenericType:
      begin
        Tool.ExtractDefinitionName(CTNode);
        if CTNode.FirstChild<>nil then
          CleanPos:=CTNode.FirstChild.StartPos;
        if SysUtils.CompareText(Tool.ExtractIdentifier(CleanPos),Node.Identifier)<>0
        then
          exit; // source has changed
      end;
    ctnVarDefinition,ctnTypeDefinition,ctnConstDefinition,
    ctnEnumIdentifier:
      if SysUtils.CompareText(Tool.ExtractIdentifier(CleanPos),Node.Identifier)<>0
      then
        exit; // source has changed
    else
      exit;
    end;

    // get source position
    if not Tool.CleanPosToCaret(CleanPos,NewCodePos) then exit;

    // ask the help system about the identifier
    if LazarusHelp.GetHintForSourcePosition(NewCodePos.Code.Filename,
      Point(NewCodePos.X,NewCodePos.Y),BaseURL,HTMLHint)<>shrSuccess then exit;

    if HTMLHint <> '' then
       Result:=true;
  end;
end;

procedure TCodeBrowserView.ExpandCollapseAllNodesInTreeView(
  NodeType: TExpandableNodeType; Expand: boolean);
var
  Node: TTreeNode;
begin
  BrowseTreeView.BeginUpdate;
  Node:=BrowseTreeView.Items.GetFirstNode;
  while Node<>nil do begin
    if (Node.Data<>nil) then begin
      case NodeType of
      entPackage:
        if  TObject(Node.Data) is TCodeBrowserUnitList then
          Node.Expanded:=Expand;
      entUnit:
        if  TObject(Node.Data) is TCodeBrowserUnit then
          Node.Expanded:=Expand;
      entClass:
        if  (TObject(Node.Data) is TCodeBrowserNode) then
          Node.Expanded:=Expand;
      end;
    end;
    Node:=Node.GetNext;
  end;
  BrowseTreeView.EndUpdate;
end;

procedure TCodeBrowserView.CopyNode(TVNode: TTreeNode; NodeType: TCopyNodeType);
var
  Node: TCodeBrowserNode;
  s: string;
begin
  if (TVNode=nil) or (TVNode.Data=nil) then exit;
  s:='';
  if  TObject(TVNode.Data) is TCodeBrowserUnitList then begin
    s:=TVNode.Text;
  end;
  if  TObject(TVNode.Data) is TCodeBrowserUnit then begin
    s:=TVNode.Text;
  end;
  if  (TObject(TVNode.Data) is TCodeBrowserNode) then begin
    Node:=TCodeBrowserNode(TVNode.Data);
    if NodeType=cntIdentifier then
      s:=Node.Identifier
    else
      s:=Node.Description;
  end;
  Clipboard.AsText:=s;
end;

procedure TCodeBrowserView.InvalidateStage(AStage: TCodeBrowserWorkStage);
begin
  if ord(fStage)<=ord(AStage) then exit;
  fStage:=AStage;
  IdleConnected:=true;
end;

function TCodeBrowserView.GetSelectedUnit: TCodeBrowserUnit;
var
  TVNode: TTreeNode;
  Node: TObject;
begin
  Result:=nil;
  TVNode:=BrowseTreeView.Selected;
  if TVNode=nil then exit;
  Node:=TObject(TVNode.Data);
  if Node=nil then exit;
  if not (Node is TCodeBrowserUnit) then exit;
  Result:=TCodeBrowserUnit(Node);
end;

function TCodeBrowserView.GetSelectedPackage: TLazPackage;
var
  TVNode: TTreeNode;
  Node: TObject;
  UnitList: TCodeBrowserUnitList;
begin
  Result:=nil;
  TVNode:=BrowseTreeView.Selected;
  if TVNode=nil then exit;
  Node:=TObject(TVNode.Data);
  if Node=nil then exit;
  if not (Node is TCodeBrowserUnitList) then exit;
  UnitList:=TCodeBrowserUnitList(Node);
  Result:=PackageGraph.FindPackageWithName(UnitList.Owner,nil);
end;

function TCodeBrowserView.GetCurUnitInSrcEditor(out FileOwner: TObject; out
  UnitCode: TCodeBuffer): boolean;
var
  SrcEdit: TSourceEditorInterface;
  Code: TCodeBuffer;
  Owners: TFPList;
begin
  FileOwner:=nil;
  UnitCode:=nil;
  Result:=false;
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then exit;
  Code:=CodeToolBoss.GetMainCode(TCodeBuffer(SrcEdit.CodeToolsBuffer));
  if Code=nil then exit;
  Owners:=PkgBoss.GetOwnersOfUnit(Code.FileName);
  try
    if (Owners=nil) or (Owners.Count=0) then exit;
    FileOwner:=TObject(Owners[0]);
    UnitCode:=Code;
    Result:=true;
  finally
    Owners.Free;
  end;
end;

function TCodeBrowserView.GetCurPackageInSrcEditor: TLazPackage;
var
  SrcEdit: TSourceEditorInterface;
  Owners: TFPList;
  i: Integer;
begin
  Result:=nil;
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then exit;
  Owners:=PkgBoss.GetOwnersOfUnit(SrcEdit.FileName);
  try
    if (Owners=nil) then exit;
    for i:=0 to Owners.Count-1 do begin
      if TObject(Owners[i]) is TLazPackage then begin
        Result:=TLazPackage(Owners[i]);
        exit;
      end;
    end;
  finally
    Owners.Free;
  end;
end;

procedure TCodeBrowserView.OpenTVNode(TVNode: TTreeNode);
var
  NodeData: TObject;
  List: TCodeBrowserUnitList;
  APackage: TLazPackage;
  CurUnit: TCodeBrowserUnit;
  Node: TCodeBrowserNode;
  Line,Column: integer;
begin
  if (TVNode=nil) or (TVNode.Data=nil) then exit;
  NodeData:=TObject(TVNode.Data);
  if NodeData is TCodeBrowserUnitList then begin
    List:=TCodeBrowserUnitList(NodeData);
    DebugLn(['TCodeBrowserView.OpenSelected "',List.Owner,'=',CodeBrowserProjectName,'"']);
    if List.Owner=CodeBrowserProjectName then begin
      // open project inspector
      DebugLn(['TCodeBrowserView.OpenSelected open project inspector']);
      ExecuteIDECommand(Self,ecProjectInspector);
    end else if List.Owner=CodeBrowserIDEName then begin
      // open the IDE -> already open
    end else if List.Owner=CodeBrowserHidden then begin
      // nothing
    end else begin
      // open package
      APackage:=PackageGraph.FindPackageWithName(List.Owner,nil);
      if APackage<>nil then begin
        PackageEditingInterface.DoOpenPackageWithName(List.Owner,[],false);
      end;
    end;
  end else if NodeData is TCodeBrowserUnit then begin
    CurUnit:=TCodeBrowserUnit(NodeData);
    if CurUnit.Filename<>'' then begin
      LazarusIDE.DoOpenEditorFile(CurUnit.Filename,-1,-1,[ofOnlyIfExists]);
    end;
  end else if NodeData is TCodeBrowserNode then begin
    Node:=TCodeBrowserNode(NodeData);
    if (Node.CodePos.Code<>nil)
    and (Node.CodePos.Code.Filename<>'') then begin
      Node.CodePos.Code.AbsoluteToLineCol(Node.CodePos.P,Line,Column);
      LazarusIDE.DoOpenFileAndJumpToPos(Node.CodePos.Code.Filename,
        Point(Column,Line),-1,-1,-1,[ofOnlyIfExists]);
    end;
  end;
end;

procedure TCodeBrowserView.BeginUpdate;
begin
  inc(fUpdateCount);
  BrowseTreeView.BeginUpdate;
end;

procedure TCodeBrowserView.EndUpdate;
begin
  dec(fUpdateCount);
  BrowseTreeView.EndUpdate;
end;

function TCodeBrowserView.ExportTree: TModalResult;
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog:=IDESaveDialogClass.Create(nil);
  try
    InitIDEFileDialog(SaveDialog);
    SaveDialog.Title:='Save tree as text (*.txt) ...';
    SaveDialog.FileName:='identifiers.txt';
    SaveDialog.DefaultExt:='txt';
    if not SaveDialog.Execute then exit(mrCancel);
    Result:=ExportTreeAsText(SaveDialog.FileName);
  finally
    StoreIDEFileDialog(SaveDialog);
    SaveDialog.Free;
  end;
end;

function TCodeBrowserView.ExportTreeAsText(Filename: string): TModalResult;

  procedure WriteNode(var List: TStrings; Node: TTreeNode; Prefix: String='');
  const
    CodeBrowserTypes: array[1..3] of TClass =
      (TCodeBrowserUnitList, TCodeBrowserUnit, TCodeBrowserNode);
    NodeIndent = '  ';
  var
    Child: TTreeNode;
    i: Integer;
  begin
    if Node=nil then exit;
    for i:=Low(CodeBrowserTypes) to High(CodeBrowserTypes) do begin
      if TObject(Node.Data) is CodeBrowserTypes[i] then begin
        List.Add(prefix+Node.Text);
        Prefix:=Prefix+NodeIndent;
        break;
      end;
    end;
    Child:=Node.GetFirstChild;
    while Child<>nil do begin
      WriteNode(List,Child,Prefix);
      Child:=Child.GetNextSibling;
    end;
  end;

var
  List: TStrings;
begin
  Filename:=TrimAndExpandFilename(Filename);
  if Filename='' then exit(mrCancel);
  Result:=CheckCreatingFile(Filename,true,true,true);
  if Result<>mrOk then exit;
  List:=TStringList.Create;
  try
    WriteNode(List,BrowseTreeView.Items.GetFirstNode);
    Result:=SaveStringToFile(Filename,List.Text,[],
      'exporting identifiers as text');
  finally
    List.Free;
  end;
end;

function TCodeBrowserView.GetScopeToCurUnitOwner(UseFCLAsDefault: boolean): string;
var
  SrcEdit: TSourceEditorInterface;
  Code: TCodeBuffer;
  MainCode: TCodeBuffer;
  Owners: TFPList;
begin
  Result:='';
  if UseFCLAsDefault then
    Result:=PackageGraph.FCLPackage.Name;
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then exit;
  Code:=TCodeBuffer(SrcEdit.CodeToolsBuffer);
  if Code=nil then exit;
  MainCode:=CodeToolBoss.GetMainCode(Code);
  if MainCode<>nil then
    Code:=MainCode;

  Owners:=PkgBoss.GetPossibleOwnersOfUnit(Code.FileName,[]);
  try
    if (Owners=nil) or (Owners.Count=0) then exit;
    if TObject(Owners[0])=Project1 then begin
      Result:=ProjectDescription;
      exit;
    end;
    if TObject(Owners[0]) is TLazPackage then begin
      Result:=TLazPackage(Owners[0]).Name;
      exit;
    end;
  finally
    Owners.Free;
  end;
end;

function TCodeBrowserView.SetScopeToCurUnitOwner(UseFCLAsDefault,
  WithRequiredPackages: boolean): boolean;
var
  NewScope: String;
begin
  Result:=false;
  NewScope:=GetScopeToCurUnitOwner(UseFCLAsDefault);
  if NewScope='' then exit;
  ScopeComboBox.Text:=NewScope;
  ScopeWithRequiredPackagesCheckBox.Checked:=WithRequiredPackages;
  InvalidateStage(cbwsGetScopeOptions);
end;

procedure TCodeBrowserView.SetFilterToSimpleIdentifier(Identifier: string);
begin
  ShowPackagesCheckBox.Checked:=true;
  PackageFilterEdit.Text:='';
  PackageFilterContainsSpeedButton.Down:=true;

  ShowUnitsCheckBox.Checked:=true;
  UnitFilterEdit.Text:='';
  UnitFilterContainsSpeedButton.Down:=true;

  ShowIdentifiersCheckBox.Checked:=true;
  IdentifierFilterEdit.Text:=Identifier;
  IdentifierFilterBeginsSpeedButton.Down:=true;

  ShowEmptyNodesCheckBox.Checked:=false;

  InvalidateStage(cbwsGetViewOptions);
end;

procedure TCodeBrowserView.BrowseTreeViewShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  TVNode: TTreeNode;
  HintStr: String;
begin
  //DebugLn(['TCodeBrowserView.BrowseTreeViewShowHint ',dbgs(HintInfo^.CursorPos)]);
  HintStr:='';
  TVNode:=BrowseTreeView.GetNodeAt(HintInfo^.CursorPos.X, HintInfo^.CursorPos.Y);
  if TVNode<>nil then
    HintStr:=GetTVNodeHint(TVNode);
  HintInfo^.HintStr:=''; // do not use the normal mechanism,
  // ... open a THintWindow with LazarusHelp instead
  if csDestroying in ComponentState then exit;
  FHintManager.ShowHint(HintInfo^.HintPos, HintStr);
end;

procedure TCodeBrowserView.CloseHintWindow;
begin
  FHintManager.HideHint;
end;

procedure TCodeBrowserView.CollapseAllPackagesMenuItemClick(Sender: TObject);
begin
  ExpandCollapseAllNodesInTreeView(entPackage,false);
end;

procedure TCodeBrowserView.CollapseAllUnitsMenuItemClick(Sender: TObject);
begin
  ExpandCollapseAllNodesInTreeView(entUnit,false);
end;

procedure TCodeBrowserView.CollapseAllClassesMenuItemClick(Sender: TObject);
begin
  ExpandCollapseAllNodesInTreeView(entClass,false);
end;

procedure TCodeBrowserView.CopyDescriptionMenuItemClick(Sender: TObject);
begin
  CopyNode(BrowseTreeView.Selected,cntDescription);
end;

procedure TCodeBrowserView.CopyIdentifierMenuItemClick(Sender: TObject);
begin
  CopyNode(BrowseTreeView.Selected,cntIdentifier);
end;

procedure TCodeBrowserView.ExpandAllClassesMenuItemClick(Sender: TObject);
begin
  ExpandCollapseAllNodesInTreeView(entClass,true);
end;

procedure TCodeBrowserView.ExpandAllPackagesMenuItemClick(Sender: TObject);
begin
  ExpandCollapseAllNodesInTreeView(entPackage,true);
end;

procedure TCodeBrowserView.ExpandAllUnitsMenuItemClick(Sender: TObject);
begin
  ExpandCollapseAllNodesInTreeView(entUnit,true);
end;

procedure TCodeBrowserView.ExportMenuItemClick(Sender: TObject);
begin
  ExportTree;
end;

procedure TCodeBrowserView.BrowseTreeViewMouseDown(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssDouble in Shift then
    OpenTVNode(BrowseTreeView.GetNodeAt(X,Y));
end;

procedure TCodeBrowserView.UsePkgInProjectMenuItemClick(Sender: TObject);
var
  APackage: TLazPackage;
begin
  APackage:=GetSelectedPackage;
  if APackage=nil then exit;
  PkgBoss.AddProjectDependency(Project1,APackage);
end;

procedure TCodeBrowserView.UseUnitInCurUnitMenuItemClick(Sender: TObject);
begin
  UseUnitInSrcEditor(false);
end;

procedure TCodeBrowserView.UsePkgInCurUnitMenuItemClick(Sender: TObject);
var
  APackage: TLazPackage;
  TargetPackage: TLazPackage;
  List: TFPList;
begin
  APackage:=GetSelectedPackage;
  if APackage=nil then exit;
  TargetPackage:=GetCurPackageInSrcEditor;
  if TargetPackage=nil then exit;
  List:=TFPList.Create;
  try
    List.Add(TargetPackage);
    if PkgBoss.AddDependencyToOwners(List,APackage)=mrOk then begin
      PackageEditingInterface.DoOpenPackageWithName(TargetPackage.Name,[],false);
    end;
  finally
    List.Free;
  end;
end;

procedure TCodeBrowserView.UseIdentifierInCurUnitMenuItemClick(Sender: TObject);
begin
  UseUnitInSrcEditor(true);
end;

procedure TCodeBrowserView.BrowseTreeViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  CloseHintWindow;
end;

procedure TCodeBrowserView.FormDeactivate(Sender: TObject);
begin
  CloseHintWindow;
end;

{ TCodeBrowserViewOptions }

procedure TCodeBrowserViewOptions.SetModified(const AValue: boolean);
begin
  if AValue then
    IncreaseChangeStamp;
  if FModified=AValue then exit;
  FModified:=AValue;
end;

function TCodeBrowserViewOptions.GetLevelFilterText(Level: TCodeBrowserLevel
  ): string;
begin
  Result:=FLevelFilterText[Level];
end;

function TCodeBrowserViewOptions.GetLevelFilterType(Level: TCodeBrowserLevel
  ): TCodeBrowserTextFilter;
begin
  Result:=FLevelFilterType[Level];
end;

procedure TCodeBrowserViewOptions.SetLevelFilterText(Level: TCodeBrowserLevel;
  const AValue: string);
begin
  if FLevelFilterText[Level]=AValue then exit;
  FLevelFilterText[Level]:=AValue;
  Modified:=true;
end;

procedure TCodeBrowserViewOptions.SetLevelFilterType(Level: TCodeBrowserLevel;
  const AValue: TCodeBrowserTextFilter);
begin
  if FLevelFilterType[Level]=AValue then exit;
  FLevelFilterType[Level]:=AValue;
  Modified:=true;
end;

procedure TCodeBrowserViewOptions.SetScope(const AValue: string);
begin
  if FScope=AValue then exit;
  FScope:=AValue;
  Modified:=true;
end;

procedure TCodeBrowserViewOptions.SetLevels(const AValue: TStrings);
begin
  if FLevels=AValue then exit;
  if FLevels.Text=AValue.Text then exit;
  FLevels.Assign(AValue);
  Modified:=true;
end;

procedure TCodeBrowserViewOptions.SetShowEmptyNodes(const AValue: boolean);
begin
  if FShowEmptyNodes=AValue then exit;
  FShowEmptyNodes:=AValue;
  Modified:=true;
end;

procedure TCodeBrowserViewOptions.SetShowPrivate(const AValue: boolean);
begin
  if FShowPrivate=AValue then exit;
  FShowPrivate:=AValue;
  Modified:=true;
end;

procedure TCodeBrowserViewOptions.SetShowProtected(const AValue: boolean);
begin
  if FShowProtected=AValue then exit;
  FShowProtected:=AValue;
  Modified:=true;
end;

procedure TCodeBrowserViewOptions.SetStoreWithRequiredPackages(
  const AValue: boolean);
begin
  if FStoreWithRequiredPackages=AValue then exit;
  FStoreWithRequiredPackages:=AValue;
end;

procedure TCodeBrowserViewOptions.SetWithRequiredPackages(const AValue: boolean);
begin
  if FWithRequiredPackages=AValue then exit;
  FWithRequiredPackages:=AValue;
  Modified:=true;
end;

procedure TCodeBrowserViewOptions.IncreaseChangeStamp;
begin
  CTIncreaseChangeStamp(FChangeStamp);
end;

constructor TCodeBrowserViewOptions.Create;
begin
  FLevels:=TStringList.Create;
  FChangeStamp:=CTInvalidChangeStamp;
  Clear;
end;

destructor TCodeBrowserViewOptions.Destroy;
begin
  FreeAndNil(FLevels);
  inherited Destroy;
end;

procedure TCodeBrowserViewOptions.Clear;
var
  l: TCodeBrowserLevel;
begin
  FLevels.Clear;
  FLevels.Text:=CodeBrowserLevelNames[cblPackages]+#13
               +CodeBrowserLevelNames[cblUnits]+#13
               +CodeBrowserLevelNames[cblIdentifiers];
  WithRequiredPackages:=false;
  ShowPrivate:=false;
  ShowProtected:=true;
  Scope:='Project';
  for l:=Low(TCodeBrowserLevel) to High(TCodeBrowserLevel) do begin
    FLevelFilterType[l]:=cbtfContains;
    FLevelFilterText[l]:='';
  end;
  IncreaseChangeStamp;
  Modified:=false;
end;

procedure TCodeBrowserViewOptions.LoadFromConfig(ConfigStore: TConfigStorage;
  const Path: string);
var
  l: TCodeBrowserLevel;
  SubPath: String;
begin
  Clear;
  WithRequiredPackages:=ConfigStore.GetValue(Path+'WithRequiredPackages/Value',false);
  Scope:=ConfigStore.GetValue(Path+'Scope/Value','Project');
  ShowPrivate:=ConfigStore.GetValue(Path+'ShowPrivate/Value',false);
  ShowProtected:=ConfigStore.GetValue(Path+'ShowProtected/Value',true);
  ShowEmptyNodes:=ConfigStore.GetValue(Path+'ShowEmptyNodes/Value',true);
  ConfigStore.GetValue(Path+'Levels/',FLevels);
  for l:=Low(TCodeBrowserLevel) to High(TCodeBrowserLevel) do begin
    SubPath:=Path+'LevelFilter/'+CodeBrowserLevelNames[l];
    FLevelFilterType[l]:=StringToCodeBrowserTextFilter(
      ConfigStore.GetValue(SubPath+'/Type',''));
    FLevelFilterText[l]:=ConfigStore.GetValue(SubPath+'/Text','');
  end;
  Modified:=false;
end;

procedure TCodeBrowserViewOptions.SaveToConfig(ConfigStore: TConfigStorage;
  const Path: string);
var
  l: TCodeBrowserLevel;
  SubPath: String;
  b: Boolean;
begin
  b:=WithRequiredPackages;
  if not StoreWithRequiredPackages then
    b:=false;
  ConfigStore.SetDeleteValue(Path+'WithRequiredPackages/Value',b,false);
  ConfigStore.SetDeleteValue(Path+'Scope/Value',Scope,'Project');
  ConfigStore.SetDeleteValue(Path+'ShowPrivate/Value',ShowPrivate,false);
  ConfigStore.SetDeleteValue(Path+'ShowProtected/Value',ShowProtected,true);
  ConfigStore.SetDeleteValue(Path+'ShowEmptyNodes/Value',ShowEmptyNodes,true);
  ConfigStore.SetValue(Path+'Levels/',FLevels);
  for l:=Low(TCodeBrowserLevel) to High(TCodeBrowserLevel) do begin
    SubPath:=Path+'LevelFilter/'+CodeBrowserLevelNames[l];
    ConfigStore.SetDeleteValue(SubPath+'/Type',
      CodeBrowserTextFilterNames[FLevelFilterType[l]],
      CodeBrowserTextFilterNames[cbtfBegins]);
    ConfigStore.SetDeleteValue(SubPath+'/Text',FLevelFilterText[l],'');
  end;
  Modified:=false;
end;

function TCodeBrowserViewOptions.HasLevel(Level: TCodeBrowserLevel): boolean;
begin
  Result:=Levels.IndexOf(CodeBrowserLevelNames[Level])>=0;
end;

{ TQuickFixIdentifierNotFound_Search }

function TQuickFixIdentifierNotFound_Search.IsApplicable(Msg: TMessageLine; out
  Identifier: string): boolean;
var
  Dummy: string;
begin
  Result:=false;
  Identifier:='';
  if not Msg.HasSourcePosition then exit;
  Result:=TIDEFPCParser.MsgLineIsId(Msg,5000,Identifier,Dummy);
end;

procedure TQuickFixIdentifierNotFound_Search.CreateMenuItems(
  Fixes: TMsgQuickFixes);
var
  Msg: TMessageLine;
  Identifier: string;
  i: Integer;
begin
  for i:=0 to Fixes.LineCount-1 do begin
    Msg:=Fixes.Lines[i];
    if not IsApplicable(Msg,Identifier) then continue;
    Fixes.AddMenuItem(Self,Msg,lisQuickFixSearchIdentifier);
    exit;
  end;
end;

procedure TQuickFixIdentifierNotFound_Search.QuickFix(Fixes: TMsgQuickFixes;
  Msg: TMessageLine);
var
  Identifier: String;
  KnownFilename: String;
  Caret: TPoint;
  Filename: String;
begin
  if not IsApplicable(Msg,Identifier) then exit;
  if not LazarusIDE.BeginCodeTools then begin
    DebugLn(['TQuickFixIdentifierNotFound_Search.Execute failed because IDE busy']);
    exit;
  end;

  // get identifier
  if not IsValidIdent(Identifier) then begin
    DebugLn(['TQuickFixIdentifierNotFound_Search.Execute not an identifier "',dbgstr(Identifier),'"']);
    exit;
  end;

  Filename:=Msg.GetFullFilename;
  KnownFilename:= LazarusIDE.FindSourceFile(Filename, Project1.Directory,
                    [fsfSearchForProject, fsfUseIncludePaths, fsfMapTempToVirtualFiles]);
  Caret:=Point(Msg.Line,Msg.Column);

  if (KnownFilename <> '') and (KnownFilename <> Filename) then begin
    if LazarusIDE.DoOpenFileAndJumpToPos(KnownFilename,Caret,-1,-1,-1,OpnFlagsPlainFile)<>mrOk
    then
    if LazarusIDE.DoOpenFileAndJumpToPos(Filename,Caret,-1,-1,-1,OpnFlagsPlainFile)<>mrOk
    then exit;
  end
  else
  if LazarusIDE.DoOpenFileAndJumpToPos(Filename,Caret,-1,-1,-1,OpnFlagsPlainFile
    )<>mrOk
  then exit;

  // start code browser
  ShowCodeBrowser(Identifier);
end;

end.

