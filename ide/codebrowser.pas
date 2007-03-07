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
    Browser for packages, classes, methods, functions.
    Scope:
      Browse units of IDE, or a project or a package.
      Browse with required packages or without.
    Sort:
      Owner, unit, class, visibility, type (procedure, var, const, ...), identifier

  Notes:
    The codetools provides TCodeTree of every unit.
    
}
unit CodeBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  LCLIntf, AvgLvlTree, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  // codetools
  CodeAtom, BasicCodeTools, DefineTemplates, CodeTree, CodeCache, CodeToolManager,
  PascalParserTool, LinkScanner, FileProcs,
  // IDEIntf
  LazConfigStorage, Project, PackageIntf, IDECommands, LazIDEIntf,
  // IDE
  PackageSystem, PackageDefs, LazarusIDEStrConsts, IDEOptionDefs,
  EnvironmentOpts;

type
  TCodeBrowserUnit = class;
  TCodeBrowserUnitList = class;


  { TCodeBrowserNode }

  TCodeBrowserNode = class
  private
    FCBUnit: TCodeBrowserUnit;
    FChildNodes: TAvgLvlTree;
    FCodePos: TCodePosition;
    FDesc: TCodeTreeNodeDesc;
    FDescription: string;
    FParentNode: TCodeBrowserNode;
  public
    constructor Create(TheUnit: TCodeBrowserUnit;
                       TheParent: TCodeBrowserNode;
                       const TheDescription: string);
    destructor Destroy; override;
    procedure Clear;
    function AddNode(const Description: string): TCodeBrowserNode;
    property CBUnit: TCodeBrowserUnit read FCBUnit;
    property Desc: TCodeTreeNodeDesc read FDesc write FDesc;
    property CodePos: TCodePosition read FCodePos write FCodePos;
    property ParentNode: TCodeBrowserNode read FParentNode;
    property ChildNodes: TAvgLvlTree read FChildNodes;
    property Description: string read FDescription;
  end;


  { TCodeBrowserUnit }

  TCodeBrowserUnit = class
  private
    FChildNodes: TAvgLvlTree;
    FCodeBuffer: TCodeBuffer;
    FCodeTool: TCodeTool;
    FCodeTreeChangeStep: integer;
    FFilename: string;
    FScanned: boolean;
    FScannedBytes: integer;
    FScannedIdentifiers: integer;
    FScannedLines: integer;
    FUnitList: TCodeBrowserUnitList;
    procedure SetCodeBuffer(const AValue: TCodeBuffer);
    procedure SetCodeTool(const AValue: TCodeTool);
    procedure SetScanned(const AValue: boolean);
  public
    constructor Create(const TheFilename: string);
    destructor Destroy; override;
    procedure Clear;
    function AddNode(const Description: string): TCodeBrowserNode;
    property Filename: string read FFilename;
    property CodeBuffer: TCodeBuffer read FCodeBuffer write SetCodeBuffer;
    property CodeTool: TCodeTool read FCodeTool write SetCodeTool;
    property CodeTreeChangeStep: integer read FCodeTreeChangeStep;
    property UnitList: TCodeBrowserUnitList read FUnitList;
    property ChildNodes: TAvgLvlTree read FChildNodes;
    property ScannedLines: integer read FScannedLines write FScannedLines;
    property ScannedBytes: integer read FScannedBytes write FScannedBytes;
    property ScannedIdentifiers: integer read FScannedIdentifiers write FScannedIdentifiers;
    property Scanned: boolean read FScanned write SetScanned;
  end;
  

  { TCodeBrowserUnitList }

  TCodeBrowserUnitList = class
  private
    FOwner: string;
    FParentList: TCodeBrowserUnitList;
    FScannedUnits: integer;
    FUnitLists: TAvgLvlTree;
    FUnits: TAvgLvlTree;
    FUnitsValid: boolean;
    procedure SetOwner(const AValue: string);
    procedure InternalAddUnitList(List: TCodeBrowserUnitList);
    procedure InternalRemoveUnitList(List: TCodeBrowserUnitList);
    procedure InternalAddUnit(AnUnit: TCodeBrowserUnit);
    procedure InternalRemoveUnit(AnUnit: TCodeBrowserUnit);
  public
    constructor Create(TheOwner: string; TheParent: TCodeBrowserUnitList);
    destructor Destroy; override;
    procedure Clear;
    function FindUnit(const Filename: string): TCodeBrowserUnit;
    function FindUnitList(const OwnerName: string): TCodeBrowserUnitList;
    procedure DeleteUnit(AnUnit: TCodeBrowserUnit);
    function AddUnit(const Filename: string): TCodeBrowserUnit;
    property Owner: string read FOwner write SetOwner;// IDE, project, package
    property ParentList: TCodeBrowserUnitList read FParentList;
    property Units: TAvgLvlTree read FUnits;
    property UnitLists: TAvgLvlTree read FUnitLists;
    property UnitsValid: boolean read FUnitsValid write FUnitsValid;
    property ScannedUnits: integer read FScannedUnits write FScannedUnits;
  end;
  
type
  TCodeBrowserLevel = (
    cblPackages,
    cblUnits
    );
    
const
  CodeBrowserLevelNames: array[TCodeBrowserLevel] of string = (
    'Packages',
    'Units'
    );

  CodeBrowserIDEName     = ' '+'Lazarus IDE';// Note: space is needed to avoid name clashing
  CodeBrowserProjectName = ' '+'Project';
  CodeBrowserHidden = ' ';

type

  { TCodeBrowserViewOptions }

  TCodeBrowserViewOptions = class
  private
    FModified: boolean;
    FScope: string;
    FLevels: TStrings;
    FShowPrivate: boolean;
    FShowProtected: boolean;
    FWithRequiredPackages: boolean;
    procedure SetModified(const AValue: boolean);
    procedure SetScope(const AValue: string);
    procedure SetLevels(const AValue: TStrings);
    procedure SetShowPrivate(const AValue: boolean);
    procedure SetShowProtected(const AValue: boolean);
    procedure SetWithRequiredPackages(const AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromConfig(ConfigStore: TConfigStorage; const Path: string);
    procedure SaveToConfig(ConfigStore: TConfigStorage; const Path: string);
    function HasLevel(Level: TCodeBrowserLevel): boolean;
    property Scope: string read FScope write SetScope;
    property WithRequiredPackages: boolean read FWithRequiredPackages write SetWithRequiredPackages;
    property Levels: TStrings read FLevels write SetLevels;
    property ShowPrivate: boolean read FShowPrivate write SetShowPrivate;
    property ShowProtected: boolean read FShowProtected write SetShowProtected;
    property Modified: boolean read FModified write SetModified;
  end;


  TCodeBrowserWorkStage = (
    cbwsGetOptions,
    cbwsGatherPackages,
    cbwsFreeUnusedPackages,
    cbwsAddNewPackages,
    cbwsGatherFiles,
    cbwsGatherOutdatedFiles,
    cbwsUpdateUnits,
    cbwsUpdateTreeView,
    cbwsFinished
    );

  { TCodeBrowserView }

  TCodeBrowserView = class(TForm)
    ShowProtectedCheckBox: TCheckBox;
    ShowPrivateCheckBox: TCheckBox;
    ImageList1: TImageList;
    FilterGroupBox: TGroupBox;
    LevelsCheckGroup: TCheckGroup;
    ScopeWithRequiredPackagesCheckBox: TCheckBox;
    ScopeComboBox: TComboBox;
    ScopeGroupBox: TGroupBox;
    BrowseTreeView: TTreeView;
    StatusBar1: TStatusBar;
    procedure BrowseTreeViewMouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BrowseTreeViewShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LevelsCheckGroupItemClick(Sender: TObject; Index: integer);
    procedure ScopeComboBoxDropDown(Sender: TObject);
    procedure ScopeComboBoxEditingDone(Sender: TObject);
    procedure ScopeWithRequiredPackagesCheckBoxChange(Sender: TObject);
    procedure OnIdle(Sender: TObject);
    procedure ShowPrivateCheckBoxChange(Sender: TObject);
    procedure ShowProtectedCheckBoxChange(Sender: TObject);
  private
    FIDEDescription: string;
    FOptions: TCodeBrowserViewOptions;
    FProjectDescription: string;
    FParserRoot: TCodeBrowserUnitList;
    FScannedBytes: PtrInt;
    FScannedIdentifiers: PtrInt;
    FScannedLines: PtrInt;
    FScannedPackages: integer;
    FScannedUnits: integer;
    FUpdateNeeded: boolean;
    FViewRoot: TObject;
    FWorkingParserRoot: TCodeBrowserUnitList;
    fUpdateCount: integer;
    fStage: TCodeBrowserWorkStage;
    fOutdatedFiles: TAvgLvlTree;// tree of TCodeBrowserUnit
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
    procedure LoadLevelsCheckGroup;
    procedure LoadFilterGroupbox;
    procedure FillScopeComboBox;
    procedure SetScannedBytes(const AValue: PtrInt);
    procedure SetScannedIdentifiers(const AValue: PtrInt);
    procedure SetScannedLines(const AValue: PtrInt);
    procedure SetScannedPackages(const AValue: integer);
    procedure SetScannedUnits(const AValue: integer);
    procedure SetUpdateNeeded(const AValue: boolean);
    procedure Work;
    procedure WorkGetOptions;
    procedure WorkGatherPackages;
    procedure WorkFreeUnusedPackages;
    procedure WorkAddNewUnitLists;
    procedure WorkGatherFileLists;
    procedure WorkUpdateFileList(List: TCodeBrowserUnitList);
    procedure WorkGatherOutdatedFiles;
    procedure WorkUpdateUnits;
    procedure WorkUpdateUnit(AnUnit: TCodeBrowserUnit);
    procedure WorkUpdateTreeView;
    procedure FreeUnitList(List: TCodeBrowserUnitList);
    procedure UpdateStatusBar(Lazy: boolean);
    procedure RemoveUnit(AnUnit: TCodeBrowserUnit);
    function CountIdentifiers(Tool: TCodeTool): integer;
    procedure UpdateTreeView;
    procedure ClearTreeView;
    function InitTreeView: TTreeNode;
    function ListOwnerToText(const ListOwner: string): string;
    procedure InitImageList;
    function GetNodeImage(CodeNode: TObject): integer;
    function GetTVNodeHint(TVNode: TTreeNode): string;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    property ParserRoot: TCodeBrowserUnitList read FParserRoot;
    property WorkingParserRoot: TCodeBrowserUnitList read FWorkingParserRoot;
    property ViewRoot: TObject read FViewRoot;// can be TCodeBrowserUnitList or TCodeBrowserUnit or TCodeBrowserNode
    property Options: TCodeBrowserViewOptions read FOptions;
    property IDEDescription: string read FIDEDescription;
    property ProjectDescription: string read FProjectDescription;
    property ScannedPackages: integer read FScannedPackages write SetScannedPackages;
    property ScannedUnits: integer read FScannedUnits write SetScannedUnits;
    property ScannedLines: PtrInt read FScannedLines write SetScannedLines;
    property ScannedBytes: PtrInt read FScannedBytes write SetScannedBytes;
    property ScannedIdentifiers: PtrInt read FScannedIdentifiers write SetScannedIdentifiers;
    property UpdateNeeded: boolean read FUpdateNeeded write SetUpdateNeeded;
  end;
  
var
  CodeBrowserView: TCodeBrowserView = nil;
  
function CompareUnitListOwners(Data1, Data2: Pointer): integer;
function ComparePAnsiStringWithUnitListOwner(Data1, Data2: Pointer): integer;
function CompareUnitFilenames(Data1, Data2: Pointer): integer;
function ComparePAnsiStringWithUnitFilename(Data1, Data2: Pointer): integer;
function CompareNodeDescriptions(Data1, Data2: Pointer): integer;
function ComparePAnsiStringWithNodeDescription(Data1, Data2: Pointer): integer;


implementation


function CompareUnitListOwners(Data1, Data2: Pointer): integer;
begin
  Result:=SysUtils.CompareText(TCodeBrowserUnitList(Data1).Owner,
                               TCodeBrowserUnitList(Data2).Owner);
end;

function ComparePAnsiStringWithUnitListOwner(Data1, Data2: Pointer): integer;
begin
  Result:=SysUtils.CompareText(PAnsiString(Data1)^,
                               TCodeBrowserUnitList(Data2).Owner);
end;

function CompareUnitFilenames(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(TCodeBrowserUnit(Data1).Filename,
                           TCodeBrowserUnit(Data2).Filename);
end;

function ComparePAnsiStringWithUnitFilename(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(PAnsiString(Data1)^,
                           TCodeBrowserUnit(Data2).Filename);
end;

function CompareNodeDescriptions(Data1, Data2: Pointer): integer;
begin
  Result:=SysUtils.CompareText(TCodeBrowserNode(Data1).Description,
                               TCodeBrowserNode(Data2).Description);
end;

function ComparePAnsiStringWithNodeDescription(Data1, Data2: Pointer): integer;
begin
  Result:=SysUtils.CompareText(PAnsiString(Data1)^,
                               TCodeBrowserNode(Data2).Description);
end;


{ TCodeBrowserView }

procedure TCodeBrowserView.FormCreate(Sender: TObject);
begin
  FOptions:=TCodeBrowserViewOptions.Create;
  
  FIDEDescription:='Lazarus IDE';
  FProjectDescription:='Project';

  Name:=NonModalIDEWindowNames[nmiwCodeBrowser];
  Caption := 'Code browser';
  EnvironmentOptions.IDEWindowLayoutList.Apply(Self,Name);
  
  ScopeGroupBox.Caption:='Scope';
  ScopeWithRequiredPackagesCheckBox.Caption:='With required packages';
  LevelsCheckGroup.Caption:='Levels';
  LevelsCheckGroup.Items[0]:='Packages';
  LevelsCheckGroup.Items[1]:='Units';

  FilterGroupBox.Caption:='Filter';
  ShowPrivateCheckBox.Caption:='Private';
  ShowProtectedCheckBox.Caption:='Protected';

  InitImageList;
  LoadOptions;
  
  UpdateNeeded:=true;
  Application.AddOnIdleHandler(@OnIdle);
end;

procedure TCodeBrowserView.FormDestroy(Sender: TObject);
begin
  ClearTreeView;
  FreeAndNil(fOutdatedFiles);
  FreeAndNil(FViewRoot);
  FreeAndNil(FParserRoot);
  FreeAndNil(FWorkingParserRoot);
  FreeAndNil(FOptions);
end;

procedure TCodeBrowserView.LevelsCheckGroupItemClick(Sender: TObject;
  Index: integer);
begin
  fStage:=cbwsGetOptions;
end;

procedure TCodeBrowserView.ScopeComboBoxDropDown(Sender: TObject);
begin
  FillScopeComboBox;
end;

procedure TCodeBrowserView.ScopeComboBoxEditingDone(Sender: TObject);
begin
  fStage:=cbwsGetOptions;
end;

procedure TCodeBrowserView.ScopeWithRequiredPackagesCheckBoxChange(
  Sender: TObject);
begin
  fStage:=cbwsGetOptions;
end;

procedure TCodeBrowserView.OnIdle(Sender: TObject);
var
  AControl: TWinControl;
begin
  AControl:=FindOwnerControl(GetFocus);
  if (AControl=nil) or (GetFirstParentForm(AControl)<>Self) then exit;
  // this form is focused -> let's work
  Work;
end;

procedure TCodeBrowserView.ShowPrivateCheckBoxChange(Sender: TObject);
begin
  fStage:=cbwsGetOptions;
end;

procedure TCodeBrowserView.ShowProtectedCheckBoxChange(Sender: TObject);
begin
  fStage:=cbwsGetOptions;
end;

procedure TCodeBrowserView.LoadOptions;
begin
  BeginUpdate;
  ScopeWithRequiredPackagesCheckBox.Checked:=Options.WithRequiredPackages;
  ScopeComboBox.Text:=Options.Scope;
  LoadLevelsCheckGroup;
  LoadFilterGroupbox;
  EndUpdate;
end;

procedure TCodeBrowserView.LoadLevelsCheckGroup;
var
  i: Integer;
begin
  for i:=0 to LevelsCheckGroup.Items.Count-1 do
    LevelsCheckGroup.Checked[i]:=Options.HasLevel(TCodeBrowserLevel(i));
end;

procedure TCodeBrowserView.LoadFilterGroupbox;
begin
  ShowPrivateCheckBox.Checked:=Options.ShowPrivate;
  ShowProtectedCheckBox.Checked:=Options.ShowProtected;
end;

procedure TCodeBrowserView.FillScopeComboBox;
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  if PackageGraph<>nil then begin
    for i:=0 to PackageGraph.Count-1 do
      sl.Add(PackageGraph.Packages[i].Name);
  end;
  sl.Sort;
  sl.Insert(0,IDEDescription);
  sl.Insert(1,ProjectDescription);
  ScopeComboBox.Items.Assign(sl);
  sl.Free;
end;

procedure TCodeBrowserView.InitImageList;

  procedure AddResImg(ImgList: TImageList; const ResName: string;
    out ImgID: integer);
  var Pixmap: TPixmap;
  begin
    Pixmap:=TPixmap.Create;
    if LazarusResources.Find(ResName)=nil then
      DebugLn('TCodeExplorerView.CodeExplorerViewCREATE: ',
        ' WARNING: icon not found: "',ResName,'"');
    Pixmap.LoadFromLazarusResource(ResName);
    ImgID:=ImgList.AddDirect(Pixmap,nil)
  end;

begin
  AddResImg(Imagelist1,'ce_default',ImgIDDefault);
  AddResImg(Imagelist1,'ce_program',ImgIDProgramCode);
  AddResImg(Imagelist1,'ce_unit',ImgIDUnitCode);
  AddResImg(Imagelist1,'ce_interface',ImgIDInterfaceSection);
  AddResImg(Imagelist1,'ce_implementation',ImgIDImplementation);
  AddResImg(Imagelist1,'ce_initialization',ImgIDInitialization);
  AddResImg(Imagelist1,'ce_finalization',ImgIDFinalization);
  AddResImg(Imagelist1,'ce_type',ImgIDTypeSection);
  AddResImg(Imagelist1,'ce_type',ImgIDType);
  AddResImg(Imagelist1,'ce_variable',ImgIDVarSection);
  AddResImg(Imagelist1,'ce_variable',ImgIDVariable);
  AddResImg(Imagelist1,'ce_const',ImgIDConstSection);
  AddResImg(Imagelist1,'ce_const',ImgIDConst);
  AddResImg(Imagelist1,'ce_class',ImgIDClass);
  AddResImg(Imagelist1,'ce_procedure',ImgIDProc);
  AddResImg(Imagelist1,'ce_property',ImgIDProperty);
  AddResImg(Imagelist1,'pkg_package',ImgIDPackage);
  AddResImg(Imagelist1,'pkg_project',ImgIDProject);
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
begin
  if FUpdateNeeded=AValue then exit;
  FUpdateNeeded:=AValue;
  if FUpdateNeeded then
    fStage:=cbwsGetOptions;
end;

procedure TCodeBrowserView.Work;
// do some work
// This is called during OnIdle, so progress in small steps
var
  OldStage: TCodeBrowserWorkStage;
begin
  OldStage:=fStage;
  case fStage of
  cbwsGetOptions:          WorkGetOptions;
  cbwsGatherPackages:      WorkGatherPackages;
  cbwsFreeUnusedPackages:  WorkFreeUnusedPackages;
  cbwsAddNewPackages:      WorkAddNewUnitLists;
  cbwsGatherFiles:         WorkGatherFileLists;
  cbwsGatherOutdatedFiles: WorkGatherOutdatedFiles;
  cbwsUpdateUnits:         WorkUpdateUnits;
  cbwsUpdateTreeView:      WorkUpdateTreeView;
  else
    UpdateNeeded:=false;
    exit;
  end;
  if ord(OldStage)<ord(cbwsFinished) then begin
    UpdateStatusBar(fStage<cbwsFinished);
  end;
end;

procedure TCodeBrowserView.WorkGetOptions;
var
  i: Integer;
  NewLevels: TStringList;
begin
  DebugLn(['TCodeBrowserView.WorkGetOptions START']);
  Options.WithRequiredPackages:=ScopeWithRequiredPackagesCheckBox.Checked;
  Options.Scope:=ScopeComboBox.Text;
  Options.ShowPrivate:=ShowPrivateCheckBox.Checked;
  Options.ShowProtected:=ShowProtectedCheckBox.Checked;
  NewLevels:=TStringList.Create;
  for i:=0 to LevelsCheckGroup.Items.Count-1 do
    if LevelsCheckGroup.Checked[i] then
      NewLevels.Add(CodeBrowserLevelNames[TCodeBrowserLevel(i)]);
  Options.Levels:=NewLevels;
  NewLevels.Free;

  // this stage finished -> next stage
  if UpdateNeeded or Options.Modified then
    fStage:=cbwsGatherPackages
  else
    fStage:=cbwsFinished;
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
      PackageGraph.GetAllRequiredPackages(Project1.FirstRequiredDependency,List);
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
  // find ParserRoot
  RootOwner:='';
  if Options.Scope=IDEDescription then begin
    RootOwner:=CodeBrowserIDEName;
  end else if Options.Scope=ProjectDescription then begin
    RootOwner:=CodeBrowserProjectName;
  end else begin
    APackage:=PackageGraph.FindAPackageWithName(Options.Scope,nil);
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
      APackage:=PackageGraph.FindAPackageWithName(FWorkingParserRoot.Owner,nil);
      if APackage<>nil then
        AddPackages(APackage.FirstRequiredDependency);
    end;
  end;
  
  // update ParserRoot item (childs will be updated on next Idle)
  if FParserRoot=nil then begin
    FParserRoot:=TCodeBrowserUnitList.Create(FWorkingParserRoot.Owner,nil);
  end else begin
    FParserRoot.Owner:=FWorkingParserRoot.Owner;
  end;
  
  // this stage finished -> next stage
  fStage:=cbwsFreeUnusedPackages;
end;

procedure TCodeBrowserView.WorkFreeUnusedPackages;

  function FindUnusedUnitList: TCodeBrowserUnitList;
  var
    Node: TAvgLvlTreeNode;
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
      or (FWorkingParserRoot.UnitLists.FindKey(@PackageName,
         @ComparePAnsiStringWithUnitListOwner)=nil)
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
    exit;
  end;

  // free the unused package
  FreeUnitList(UnusedPackage);
end;

procedure TCodeBrowserView.WorkAddNewUnitLists;
var
  Node: TAvgLvlTreeNode;
  List: TCodeBrowserUnitList;
begin
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
    Node: TAvgLvlTreeNode;
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
    exit;
  end;
  
  WorkUpdateFileList(List);
end;

procedure TCodeBrowserView.WorkUpdateFileList(List: TCodeBrowserUnitList);
var
  NewFileList: TAvgLvlTree;

  procedure AddFile(const Filename: string);
  begin
    //DebugLn(['AddFile Filename="',Filename,'"']);
    if Filename='' then exit;
    if System.Pos('$',Filename)>0 then begin
      DebugLn(['WARNING: TCodeBrowserView.WorkUpdateFiles Macros in filename ',Filename]);
      exit;
    end;
    if NewFileList.FindKey(@Filename,@ComparePAnsiStringWithUnitFilename)<>nil
    then exit;
    //DebugLn(['TCodeBrowserView.WorkUpdateFiles AddFile ',Filename]);
    NewFileList.Add(TCodeBrowserUnit.Create(Filename));
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
        AddFile(AnUnitInfo.Filename);
      AnUnitInfo:=AnUnitInfo.NextPartOfProject;
    end;
  end;
  
  procedure AddFilesOfPackageFCL;
  var
    LazDir: String;
    UnitLinks: String;
    SpacePos: LongInt;
    Filename: String;
    StartPos: Integer;
    EndPos: LongInt;
  begin
    // use unitlinks of the lazarus source directory
    LazDir:=AppendPathDelim(EnvironmentOptions.LazarusDirectory);
    if (LazDir='') or (not FilenameIsAbsolute(LazDir)) then exit;
    UnitLinks:=CodeToolBoss.GetUnitLinksForDirectory(LazDir);
    StartPos:=1;
    while StartPos<=length(UnitLinks) do begin
      EndPos:=StartPos;
      while (EndPos<=length(UnitLinks))
      and (not (UnitLinks[EndPos] in [#10,#13])) do
        inc(EndPos);
      if EndPos>StartPos then begin
        SpacePos:=StartPos;
        while (SpacePos<=length(UnitLinks)) and (UnitLinks[SpacePos]<>' ') do
          inc(SpacePos);
        if (SpacePos>StartPos) and (SpacePos<EndPos) then begin
          Filename:=copy(UnitLinks,SpacePos+1,EndPos-SpacePos-1);
          AddFile(Filename);
        end;
      end;
      StartPos:=EndPos;
      while (StartPos<=length(UnitLinks))
      and (UnitLinks[StartPos] in [#10,#13]) do
        inc(StartPos);
    end;
  end;
  
  procedure AddFilesOfPackage(APackage: TLazPackage);
  var
    i: Integer;
    PkgFile: TPkgFile;
  begin
    if APackage=nil then exit;
    for i:=0 to APackage.FileCount-1 do begin
      PkgFile:=APackage.Files[i];
      if (PkgFile.FileType in PkgFileUnitTypes) then
        AddFile(PkgFile.GetFullFilename);
    end;
    if APackage.Name='FCL' then begin
      AddFilesOfPackageFCL;
    end;
  end;
  
  procedure AddFilesOfDirectory(const Directory: string);
  // ! needs ending PathDelim !
  var
    FileInfo: TSearchRec;
  begin
    //DebugLn(['AddFilesOfDirectory Directory="',Directory,'"']);
    if (not FilenameIsAbsolute(Directory))
    or (not DirectoryExists(Directory)) then begin
      DebugLn(['AddFilesOfDirectory WARNING: does not exist: "',Directory,'"']);
      exit;
    end;
    if SysUtils.FindFirst(Directory+FileMask,faAnyFile,FileInfo)=0 then begin
      repeat
        // check if special file
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
        then
          continue;
        if FilenameIsPascalUnit(FileInfo.Name) then
          AddFile(Directory+FileInfo.Name);
      until SysUtils.FindNext(FileInfo)<>0;
    end;
    SysUtils.FindClose(FileInfo);
  end;
  
  procedure AddFilesOfSearchPath(const SrcPath, BaseDir: string);
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
        AddFilesOfDirectory(Dir);
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
    LazDir:=AppendPathDelim(EnvironmentOptions.LazarusDirectory);
    if not DirectoryExists(LazDir) then begin
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
    SrcPath:=LazIDESrcPath.Value;
    AddFilesOfSearchPath(SrcPath+';.',LazDir+'ide'+PathDelim);
  end;

  procedure DeleteUnusedFiles;
  var
    Node: TAvgLvlTreeNode;
    CurUnit: TCodeBrowserUnit;
    NextNode: TAvgLvlTreeNode;
  begin
    if List.Units=nil then exit;
    Node:=List.Units.FindLowest;
    while Node<>nil do begin
      NextNode:=List.Units.FindSuccessor(Node);
      CurUnit:=TCodeBrowserUnit(Node.Data);
      if NewFileList.FindKey(@CurUnit.Filename,
        @ComparePAnsiStringWithUnitFilename)=nil
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
    Node: TAvgLvlTreeNode;
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
  NewFileList:=TAvgLvlTree.Create(@CompareUnitFilenames);
  try
    // get new list of files
    if List.Owner=CodeBrowserIDEName then begin
      AddFilesOfIDE;
    end else if List.Owner=CodeBrowserProjectName then begin
      AddFilesOfProject(Project1);
    end else begin
      APackage:=PackageGraph.FindAPackageWithName(List.Owner,nil);
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
      fOutdatedFiles:=TAvgLvlTree.Create(@CompareUnitFilenames);
    if fOutdatedFiles.Find(AnUnit)<>nil then exit;
    fOutdatedFiles.Add(AnUnit);
  end;

  procedure AddFiles(List: TCodeBrowserUnitList);
  var
    Node: TAvgLvlTreeNode;
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
    fOutdatedFiles.FreeAndClear;
  AddFiles(ParserRoot);

  // this stage finished -> next stage
  fStage:=cbwsUpdateUnits;
end;

procedure TCodeBrowserView.WorkUpdateUnits;

  function FindOutdatedUnit: TCodeBrowserUnit;
  var
    Node: TAvgLvlTreeNode;
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
        fStage:=cbwsUpdateTreeView;
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
      if CodeBuf<>LastCode then begin
        inc(LineCnt,LineEndCount(CodeBuf.Source));
        inc(ByteCnt,length(CodeBuf.Source));
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
  AnUnit.FCodeBuffer:=CodeToolBoss.LoadFile(AnUnit.Filename,false,false);
  if AnUnit.CodeBuffer=nil then exit;
  // check if this is a unit
  MainCodeBuf:=CodeToolBoss.GetMainCode(AnUnit.CodeBuffer);
  if MainCodeBuf<>AnUnit.CodeBuffer then begin
    // this is not a unit, but an include file
    DebugLn(['TCodeBrowserView.WorkUpdateUnit HINT: this is not a unit: ',AnUnit.Filename]);
    exit;
  end;
  // scan
  CodeToolBoss.Explore(AnUnit.CodeBuffer,Tool,false,true);
  UpdateScannedCounters(Tool);
  //DebugLn(['TCodeBrowserView.WorkUpdateUnit END ',AnUnit.Filename]);
end;

procedure TCodeBrowserView.WorkUpdateTreeView;
begin
  UpdateTreeView;
  // this stage finished -> next stage
  fStage:=cbwsFinished;
end;

procedure TCodeBrowserView.FreeUnitList(List: TCodeBrowserUnitList);
var
  Node: TAvgLvlTreeNode;
  AnUnit: TCodeBrowserUnit;
begin
  DebugLn(['TCodeBrowserView.FreeUnitList ',List.Owner]);
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
var
  s: String;
begin
  if Lazy and (Abs(Now-fLastStatusBarUpdate)<SmallTimeStep) then begin
    // the last update is not long ago
    // => skip update
    exit;
  end;
  fLastStatusBarUpdate:=Now;
  s:='packages='+IntToStr(ScannedPackages)
    +' units='+IntToStr(ScannedUnits)
    +' identifiers='+IntToStr(ScannedIdentifiers)
    +' lines='+IntToStr(ScannedLines)
    +' bytes='+IntToStr(ScannedBytes);
  if fStage<>cbwsFinished then
    s:=s+'. Scanning ...';
  StatusBar1.SimpleText:=s;
end;

procedure TCodeBrowserView.UpdateTreeView;
var
  ShowPackages: boolean;
  ShowUnits: boolean;
  ShowPrivate: boolean;
  ShowProtected: boolean;

  function GetCodeTool(AnUnit: TCodeBrowserUnit): TCodeTool;
  begin
    //DebugLn(['GetCodeTool ',AnUnit.CodeTool<>nil,' ',AnUnit.CodeBuffer<>nil]);
    Result:=AnUnit.CodeTool;
    if Result<>nil then exit;
    if AnUnit.CodeBuffer=nil then exit;
    Result:=CodeToolBoss.GetCodeToolForSource(AnUnit.CodeBuffer,true,false)
                as TCodeTool;
    //DebugLn(['GetCodeTool END ',Result<>nil]);
  end;
  
  procedure AddUnitNodes(SrcUnit: TCodeBrowserUnit; var DestUnit: TObject);
  var
    Tool: TCodeTool;
    
    procedure AddUnit;
    begin
      if DestUnit=nil then
        DestUnit:=TCodeBrowserUnit.Create('');
    end;

    procedure AddIdentifierNode(CTNode: TCodeTreeNode;
      const Description: string);
    var
      NewNode: TCodeBrowserNode;
      ChildCTNode: TCodeTreeNode;

      procedure AddChildNode(const ChildDescription: string);
      var
        NewChildNode: TCodeBrowserNode;
      begin
        //DebugLn(['AddChildNode ',ChildCTNode.DescAsString,' ',ChildDescription]);
        if (ChildCTNode.Parent.Desc=ctnClassPrivate) and (not ShowPrivate) then
          exit;
        if (ChildCTNode.Parent.Desc=ctnClassProtected) and (not ShowProtected)
        then
          exit;
        NewChildNode:=NewNode.AddNode(ChildDescription);
        if NewChildNode<>nil then begin
          NewChildNode.Desc:=ChildCTNode.Desc;
          Tool.CleanPosToCodePos(ChildCTNode.StartPos,NewChildNode.FCodePos);
        end;
      end;

    begin
      AddUnit;
      //DebugLn(['AddIdentifierNode ',CTNode.DescAsString,' Description="',Description,'"']);
      NewNode:=TCodeBrowserUnit(DestUnit).AddNode(Description);
      NewNode.Desc:=CTNode.Desc;
      Tool.CleanPosToCodePos(CTNode.StartPos,NewNode.FCodePos);
      //DebugLn(['AddIdentifierNode Code=',NewNode.FCodePos.Code<>nil,' P=',NewNode.FCodePos.P]);
      
      if (CTNode.Desc=ctnTypeDefinition)
      and (CTNode.FirstChild<>nil)
      and (CTNode.FirstChild.Desc in [ctnClass,ctnClassInterface,ctnRecordType,
         ctnEnumerationType])
      then begin
        // add child nodes
        ChildCTNode:=CTNode.FirstChild;
        while (ChildCTNode<>nil) and (ChildCTNode.StartPos<CTNode.EndPos) do
        begin
          case ChildCTNode.Desc of
          ctnProcedure:
            AddChildNode(Tool.ExtractProcHead(ChildCTNode,
                [phpWithoutClassKeyword,phpWithParameterNames,
                 phpWithVarModifiers,phpWithResultType]));
          ctnVarDefinition:
            AddChildNode(Tool.ExtractDefinitionName(ChildCTNode));
          ctnProperty:
            AddChildNode(Tool.ExtractProperty(ChildCTNode,
              [phpWithoutClassKeyword,phpWithParameterNames,
               phpWithVarModifiers,phpWithResultType]));
          ctnEnumIdentifier:
            AddChildNode(Tool.ExtractIdentifier(ChildCTNode.StartPos));
          end;
          if ChildCTNode.Desc=ctnProcedureHead then
            ChildCTNode:=ChildCTNode.NextSkipChilds
          else
            ChildCTNode:=ChildCTNode.Next;
        end;
      end;
    end;
    
  var
    CTNode: TCodeTreeNode;
  begin
    if SrcUnit=nil then exit;
    //DebugLn(['AddUnitNodes SrcUnit.Filename="',SrcUnit.Filename,'"']);
    Tool:=GetCodeTool(SrcUnit);
    if Tool=nil then exit;
    if Tool.Tree=nil then exit;
    
    CTNode:=Tool.Tree.Root;
    while CTNode<>nil do begin
      //DebugLn(['AddUnitNodes ',CTNode.DescAsString]);
      case CTNode.Desc of
      ctnProcedure:
        if not Tool.NodeIsMethodBody(CTNode) then
          AddIdentifierNode(CTNode,Tool.ExtractProcHead(CTNode,
            [phpWithoutClassKeyword,phpWithoutClassName,phpWithParameterNames,
             phpWithVarModifiers,phpWithResultType]));
      ctnVarDefinition,ctnConstDefinition,ctnTypeDefinition:
        if not Tool.NodeIsForwardDeclaration(CTNode) then
          AddIdentifierNode(CTNode,Tool.ExtractDefinitionName(CTNode));
      end;

      // go to next node
      case CTNode.Desc of
      ctnProgram,ctnLibrary,ctnPackage,ctnUnit,ctnInterface,
      ctnTypeSection,ctnConstSection,ctnVarSection,ctnResStrSection:
        // go into child nodes
        CTNode:=CTNode.Next;
      ctnImplementation, ctnBeginBlock, ctnAsmBlock: break;
      else
        // skip childs and go to next sibling or parent
        CTNode:=CTNode.NextSkipChilds;
      end;
    end;
  end;
  
  procedure AddUnits(SrcList: TCodeBrowserUnitList;
    var DestParentList: TObject);
    
    procedure RaiseParentNotUnitList;
    begin
      raise Exception.Create('TCodeBrowserView.UpdateTreeView.AddUnits.RaiseParentNotUnitList');
    end;
    
  var
    Node: TAvgLvlTreeNode;
    CurUnit: TCodeBrowserUnit;
    NewUnit: TCodeBrowserUnit;
  begin
    if SrcList=nil then exit;
    //DebugLn(['AddUnits SrcList.Owner="',SrcList.Owner,'" HasUnits=',SrcList.Units<>nil]);
    if SrcList.Units<>nil then begin
      Node:=SrcList.Units.FindLowest;
      NewUnit:=nil;
      while Node<>nil do begin
        CurUnit:=TCodeBrowserUnit(Node.Data);
        if DestParentList=nil then begin
          DestParentList:=TCodeBrowserUnitList.Create(CodeBrowserHidden,nil);
        end else if not (DestParentList is TCodeBrowserUnitList) then
          RaiseParentNotUnitList;
        if ShowUnits then begin
          // create a unit node
          NewUnit:=TCodeBrowserUnitList(DestParentList).AddUnit(CurUnit.Filename);
          NewUnit.CodeBuffer:=CurUnit.CodeBuffer;
          NewUnit.CodeTool:=CurUnit.CodeTool;
        end else if NewUnit=nil then begin
          // create a dummy unit node to add all identifiers
          NewUnit:=TCodeBrowserUnitList(DestParentList).AddUnit('');
        end;
        //DebugLn(['AddUnits AddUnitNodes ',CurUnit.Filename]);
        AddUnitNodes(CurUnit,NewUnit);
        if (DestParentList=nil) then
          DestParentList:=NewUnit;
        Node:=SrcList.Units.FindSuccessor(Node);
      end;
    end;
  end;

  procedure AddUnitLists(SrcList: TCodeBrowserUnitList;
    var DestParentList: TObject);
  var
    Node: TAvgLvlTreeNode;
    SubList: TCodeBrowserUnitList;
    NewList: TCodeBrowserUnitList;
  begin
    if SrcList=nil then exit;
    //DebugLn(['AddUnitLists SrcList.Owner="',SrcList.Owner,'"']);
    // create node
    if ShowPackages then begin
      if DestParentList=nil then begin
        DestParentList:=TCodeBrowserUnitList.Create(CodeBrowserHidden,nil);
      end;
      NewList:=TCodeBrowserUnitList.Create(SrcList.Owner,
                                          TCodeBrowserUnitList(DestParentList));
    end else begin
      NewList:=TCodeBrowserUnitList(DestParentList);
    end;
    // create nodes for unitlists
    if SrcList.UnitLists<>nil then begin
      Node:=SrcList.UnitLists.FindLowest;
      while Node<>nil do begin
        SubList:=TCodeBrowserUnitList(Node.Data);
        AddUnitLists(SubList,DestParentList);
        Node:=SrcList.UnitLists.FindSuccessor(Node);
      end;
    end;
    // create nodes for units
    AddUnits(SrcList,NewList);
    // update DestParentList
    if (DestParentList=nil) then
      DestParentList:=NewList;
  end;

  procedure AddTreeNodes(CodeNode: TObject; ParentViewNode: TTreeNode);
  // create visual nodes (TTreeNode)
  var
    List: TCodeBrowserUnitList;
    ListName: String;
    Node: TAvgLvlTreeNode;
    TVNode: TTreeNode;
    CurUnit: TCodeBrowserUnit;
    CurUnitName: String;
    CurTool: TCodeTool;
    CurNode: TCodeBrowserNode;
  begin
    if CodeNode=nil then exit;
    //DebugLn(['AddTreeNodes ',DbgSName(CodeNode)]);
    if CodeNode is TCodeBrowserUnitList then begin
      // unit list
      List:=TCodeBrowserUnitList(CodeNode);
      if List.Owner=CodeBrowserHidden then begin
        TVNode:=ParentViewNode;
      end else begin
        ListName:=ListOwnerToText(List.Owner);
        TVNode:=BrowseTreeView.Items.AddChildObject(
                                              ParentViewNode,ListName,CodeNode);
        TVNode.ImageIndex:=GetNodeImage(CodeNode);
        TVNode.StateIndex:=TVNode.ImageIndex;
      end;
      if List.UnitLists<>nil then begin
        Node:=List.UnitLists.FindLowest;
        while Node<>nil do begin
          AddTreeNodes(TObject(Node.Data),TVNode);
          Node:=List.UnitLists.FindSuccessor(Node);
        end;
      end;
      if List.Units<>nil then begin
        Node:=List.Units.FindLowest;
        while Node<>nil do begin
          AddTreeNodes(TObject(Node.Data),TVNode);
          Node:=List.Units.FindSuccessor(Node);
        end;
      end;
    end
    else if CodeNode is TCodeBrowserUnit then begin
      // unit
      CurUnit:=TCodeBrowserUnit(CodeNode);
      CurTool:=nil;
      if CurUnit.Filename<>'' then
        CurTool:=GetCodeTool(CurUnit);
      if CurTool<>nil then begin
        // add a treenode for this unit
        CurUnitName:=CurTool.GetCachedSourceName;
        if CurUnitName='' then
          CurUnitName:=ExtractFileNameOnly(CurTool.MainFilename);
        TVNode:=BrowseTreeView.Items.AddChildObject(ParentViewNode,
                                                    CurUnitName,CodeNode);
        TVNode.ImageIndex:=GetNodeImage(CodeNode);
        TVNode.StateIndex:=TVNode.ImageIndex;
      end else begin
        // do not add a tree node for this unit
        TVNode:=ParentViewNode;
      end;
      // create tree nodes for code nodes
      if CurUnit.ChildNodes<>nil then begin
        Node:=CurUnit.ChildNodes.FindLowest;
        while Node<>nil do begin
          AddTreeNodes(TObject(Node.Data),TVNode);
          Node:=CurUnit.ChildNodes.FindSuccessor(Node);
        end;
      end;
    end
    else if CodeNode is TCodeBrowserNode then begin
      // code node
      CurNode:=TCodeBrowserNode(CodeNode);
      if CurNode.Description<>'' then begin
        TVNode:=BrowseTreeView.Items.AddChildObject(ParentViewNode,
                                                  CurNode.Description,CodeNode);
        TVNode.ImageIndex:=GetNodeImage(CodeNode);
        TVNode.StateIndex:=TVNode.ImageIndex;

        // create tree nodes for child code nodes
        if CurNode.ChildNodes<>nil then begin
          Node:=CurNode.ChildNodes.FindLowest;
          while Node<>nil do begin
            AddTreeNodes(TObject(Node.Data),TVNode);
            Node:=CurNode.ChildNodes.FindSuccessor(Node);
          end;
        end;
      end;
    end;
    
    ParentViewNode.Expanded:=true;
  end;

var
  RootTVNode: TTreeNode;
begin
  ShowPackages:=Options.HasLevel(cblPackages);
  ShowUnits:=Options.HasLevel(cblUnits);
  ShowPrivate:=Options.ShowPrivate;
  ShowProtected:=Options.ShowProtected;
  DebugLn(['TCodeBrowserView.UpdateTreeView ShowPackages=',ShowPackages,' ShowUnits=',ShowUnits]);

  BrowseTreeView.BeginUpdate;
  CodeToolBoss.ActivateWriteLock;
  try
    RootTVNode:=InitTreeView;

    // create internal nodes
    AddUnitLists(ParserRoot,fViewRoot);

    // create treeview nodes
    AddTreeNodes(ViewRoot,RootTVNode);
  finally
    CodeToolBoss.DeactivateWriteLock;
    BrowseTreeView.EndUpdate;
  end;
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

function TCodeBrowserView.InitTreeView: TTreeNode;
begin
  ClearTreeView;
  Result:=BrowseTreeView.Items.Add(nil,'Root');
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
    ctnTypeDefinition:
      Result:=ImgIDType;
    ctnVarSection:
      Result:=ImgIDVarSection;
    ctnVarDefinition:
      Result:=ImgIDVariable;
    ctnConstSection,ctnResStrSection:
      Result:=ImgIDConstSection;
    ctnConstDefinition:
      Result:=ImgIDConst;
    ctnClass:
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
begin
  Result:='';
  if (TVNode=nil) or (TVNode.Data=nil) then exit;
  NodeData:=TObject(TVNode.Data);
  //DebugLn(['TCodeBrowserView.GetTVNodeHint ',DbgSName(NodeData)]);
  if NodeData is TCodeBrowserUnitList then begin

  end else if NodeData is TCodeBrowserUnit then begin
    CurUnit:=TCodeBrowserUnit(NodeData);
    if CurUnit.Filename<>'' then
      Result:=CurUnit.Filename;
  end else if NodeData is TCodeBrowserNode then begin
    Node:=TCodeBrowserNode(NodeData);
    if Node.CodePos.Code<>nil then begin
      Result:=Node.CodePos.Code.Filename;
      Node.CodePos.Code.AbsoluteToLineCol(Node.CodePos.P,Line,Column);
      if Line>0 then
        Result:=Result+' ('+IntToStr(Line)+','+IntToStr(Column)+')';
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

procedure TCodeBrowserView.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin

end;

procedure TCodeBrowserView.BrowseTreeViewShowHint(Sender: TObject;
  HintInfo: PHintInfo);
var
  TVNode: TTreeNode;
  HintStr: String;
  MousePos: TPoint;
begin
  //DebugLn(['TCodeBrowserView.BrowseTreeViewShowHint ',dbgs(HintInfo^.CursorPos)]);
  HintStr:='';
  MousePos:=HintInfo^.CursorPos;
  TVNode:=BrowseTreeView.GetNodeAt(MousePos.X,MousePos.Y);
  if TVNode<>nil then begin
    HintStr:=GetTVNodeHint(TVNode);
    //DebugLn(['TCodeBrowserView.BrowseTreeViewShowHint HintStr="',HintStr,'"']);
  end;
  HintInfo^.HintStr:=HintStr;
end;

procedure TCodeBrowserView.BrowseTreeViewMouseDown(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NodeData: TObject;
  Node: TCodeBrowserNode;
  CurUnit: TCodeBrowserUnit;
  Line, Column: integer;
  TVNode: TTreeNode;
  List: TCodeBrowserUnitList;
  APackage: TLazPackage;
begin
  if ssDouble in Shift then begin
    TVNode:=BrowseTreeView.GetNodeAt(X,Y);
    if (TVNode=nil) or (TVNode.Data=nil) then exit;
    NodeData:=TObject(TVNode.Data);
    if NodeData is TCodeBrowserUnitList then begin
      List:=TCodeBrowserUnitList(NodeData);
      DebugLn(['TCodeBrowserView.BrowseTreeViewMouseDown "',List.Owner,'=',CodeBrowserProjectName,'"']);
      if List.Owner=CodeBrowserProjectName then begin
        // open project inspector
        DebugLn(['TCodeBrowserView.BrowseTreeViewMouseDown open project inspector']);
        ExecuteIDECommand(Self,ecProjectInspector);
      end else if List.Owner=CodeBrowserIDEName then begin
        // open the IDE -> already open
      end else if List.Owner=CodeBrowserHidden then begin
        // nothing
      end else begin
        // open package
        APackage:=PackageGraph.FindAPackageWithName(List.Owner,nil);
        if APackage<>nil then begin
          PackageEditingInterface.DoOpenPackageFile(APackage.Filename,[]);
        end;
      end;
    end else if NodeData is TCodeBrowserUnit then begin
      CurUnit:=TCodeBrowserUnit(NodeData);
      if CurUnit.Filename<>'' then begin
        LazarusIDE.DoOpenEditorFile(CurUnit.Filename,-1,[ofOnlyIfExists]);
      end;
    end else if NodeData is TCodeBrowserNode then begin
      Node:=TCodeBrowserNode(NodeData);
      if (Node.CodePos.Code<>nil)
      and (Node.CodePos.Code.Filename<>'') then begin
        Node.CodePos.Code.AbsoluteToLineCol(Node.CodePos.P,Line,Column);
        LazarusIDE.DoOpenFileAndJumpToPos(Node.CodePos.Code.Filename,
          Point(Column,Line),-1,-1,[ofOnlyIfExists]);
      end;
    end;
  end;
end;

{ TCodeBrowserNode }

constructor TCodeBrowserNode.Create(TheUnit: TCodeBrowserUnit;
  TheParent: TCodeBrowserNode; const TheDescription: string);
begin
  FCBUnit:=TheUnit;
  FParentNode:=TheParent;
  FDescription:=TheDescription;
end;

destructor TCodeBrowserNode.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCodeBrowserNode.Clear;
begin
  if FChildNodes<>nil then
    FChildNodes.FreeAndClear;
  FreeAndNil(FChildNodes);
end;

function TCodeBrowserNode.AddNode(const Description: string): TCodeBrowserNode;
begin
  Result:=TCodeBrowserNode.Create(nil,Self,Description);
  if FChildNodes=nil then
    FChildNodes:=TAvgLvlTree.Create(@CompareNodeDescriptions);
  FChildNodes.Add(Result);
end;

{ TCodeBrowserUnit }

procedure TCodeBrowserUnit.SetScanned(const AValue: boolean);
begin
  if FScanned=AValue then exit;
  FScanned:=AValue;
  FScannedBytes:=0;
  FScannedLines:=0;
  FScannedIdentifiers:=0;
  if UnitList<>nil then begin
    if FScanned then
      inc(UnitList.FScannedUnits)
    else
      dec(UnitList.FScannedUnits);
  end;
end;

procedure TCodeBrowserUnit.SetCodeTool(const AValue: TCodeTool);
begin
  if FCodeTool=nil then exit;
  FCodeTool:=AValue;
end;

procedure TCodeBrowserUnit.SetCodeBuffer(const AValue: TCodeBuffer);
begin
  if FCodeBuffer=AValue then exit;
  FCodeBuffer:=AValue;
end;

constructor TCodeBrowserUnit.Create(const TheFilename: string);
begin
  FFilename:=TheFilename;
end;

destructor TCodeBrowserUnit.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCodeBrowserUnit.Clear;
begin
  if FChildNodes<>nil then
    FChildNodes.FreeAndClear;
  FreeAndNil(FChildNodes);
end;

function TCodeBrowserUnit.AddNode(const Description: string): TCodeBrowserNode;
begin
  Result:=TCodeBrowserNode.Create(Self,nil,Description);
  if FChildNodes=nil then
    FChildNodes:=TAvgLvlTree.Create(@CompareNodeDescriptions);
  FChildNodes.Add(Result);
end;

{ TCodeBrowserUnitList }

procedure TCodeBrowserUnitList.SetOwner(const AValue: string);
begin
  if Owner=AValue then exit;
  if ParentList<>nil then RaiseGDBException('not allowed');
  FOwner:=AValue;
  FUnitsValid:=false;
end;

procedure TCodeBrowserUnitList.InternalAddUnitList(List: TCodeBrowserUnitList);
begin
  if FUnitLists=nil then
    FUnitLists:=TAvgLvlTree.Create(@CompareUnitListOwners);
  FUnitLists.Add(List);
end;

procedure TCodeBrowserUnitList.InternalRemoveUnitList(List: TCodeBrowserUnitList
  );
begin
  if FUnitLists<>nil then
    FUnitLists.Remove(List);
end;

procedure TCodeBrowserUnitList.InternalAddUnit(AnUnit: TCodeBrowserUnit);
begin
  if FUnits=nil then
    FUnits:=TAvgLvlTree.Create(@CompareUnitFilenames);
  FUnits.Add(AnUnit);
end;

procedure TCodeBrowserUnitList.InternalRemoveUnit(AnUnit: TCodeBrowserUnit);
begin
  if FUnits<>nil then
    FUnits.Remove(AnUnit);
end;

constructor TCodeBrowserUnitList.Create(TheOwner: string;
  TheParent: TCodeBrowserUnitList);
begin
  FOwner:=TheOwner;
  FParentList:=TheParent;
  if FParentList<>nil then
    FParentList.InternalAddUnitList(Self);
end;

destructor TCodeBrowserUnitList.Destroy;
begin
  Clear;
  if FParentList<>nil then begin
    FParentList.InternalRemoveUnitList(Self);
    FParentList:=nil;
  end;
  inherited Destroy;
end;

procedure TCodeBrowserUnitList.Clear;

  procedure FreeTree(var Tree: TAvgLvlTree);
  var
    TmpTree: TAvgLvlTree;
  begin
    if Tree=nil then exit;
    TmpTree:=Tree;
    Tree:=nil;
    TmpTree.FreeAndClear;
    TmpTree.Free;
  end;

begin
  FreeTree(FUnits);
  FreeTree(FUnitLists);
  FUnitsValid:=false;
end;

function TCodeBrowserUnitList.FindUnit(const Filename: string
  ): TCodeBrowserUnit;
var
  Node: TAvgLvlTreeNode;
begin
  Result:=nil;
  if Filename='' then exit;
  if FUnits=nil then exit;
  Node:=FUnits.FindKey(@Filename,@ComparePAnsiStringWithUnitFilename);
  if Node=nil then exit;
  Result:=TCodeBrowserUnit(Node.Data);
end;

function TCodeBrowserUnitList.FindUnitList(const OwnerName: string
  ): TCodeBrowserUnitList;
var
  Node: TAvgLvlTreeNode;
begin
  Result:=nil;
  if FUnitLists=nil then exit;
  if OwnerName='' then exit;
  Node:=FUnitLists.FindKey(@OwnerName,@ComparePAnsiStringWithUnitListOwner);
  if Node=nil then exit;
  Result:=TCodeBrowserUnitList(Node.Data);
end;

procedure TCodeBrowserUnitList.DeleteUnit(AnUnit: TCodeBrowserUnit);
begin
  if AnUnit=nil then exit;
  if FUnits=nil then exit;
  FUnits.Remove(AnUnit);
  AnUnit.Free;
end;

function TCodeBrowserUnitList.AddUnit(const Filename: string
  ): TCodeBrowserUnit;
begin
  Result:=TCodeBrowserUnit.Create(Filename);
  InternalAddUnit(Result);
end;

{ TCodeBrowserViewOptions }

procedure TCodeBrowserViewOptions.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
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

procedure TCodeBrowserViewOptions.SetWithRequiredPackages(const AValue: boolean
  );
begin
  if FWithRequiredPackages=AValue then exit;
  FWithRequiredPackages:=AValue;
  Modified:=true;
end;

constructor TCodeBrowserViewOptions.Create;
begin
  FLevels:=TStringList.Create;
  Clear;
end;

destructor TCodeBrowserViewOptions.Destroy;
begin
  FreeAndNil(FLevels);
  inherited Destroy;
end;

procedure TCodeBrowserViewOptions.Clear;
begin
  FLevels.Clear;
  FLevels.Text:=CodeBrowserLevelNames[cblPackages]+#13
               +CodeBrowserLevelNames[cblUnits];
  WithRequiredPackages:=false;
  ShowPrivate:=false;
  ShowProtected:=true;
  Scope:='Project';
  Modified:=false;
end;

procedure TCodeBrowserViewOptions.LoadFromConfig(ConfigStore: TConfigStorage;
  const Path: string);
begin
  Clear;
  WithRequiredPackages:=
                  ConfigStore.GetValue(Path+'WithRequiredPackages/Value',false);
  Scope:=ConfigStore.GetValue(Path+'Scope/Value','Project');
  ShowPrivate:=ConfigStore.GetValue(Path+'ShowPrivate/Value',false);
  ShowProtected:=ConfigStore.GetValue(Path+'ShowProtected/Value',true);
  ConfigStore.GetValue(Path+'Levels/',FLevels);
  Modified:=false;
end;

procedure TCodeBrowserViewOptions.SaveToConfig(ConfigStore: TConfigStorage;
  const Path: string);
begin
  ConfigStore.SetDeleteValue(Path+'WithRequiredPackages/Value',
                             WithRequiredPackages,false);
  ConfigStore.SetDeleteValue(Path+'Scope/Value',Scope,'Project');
  ConfigStore.SetDeleteValue(Path+'ShowPrivate/Value',ShowPrivate,false);
  ConfigStore.SetDeleteValue(Path+'ShowProtected/Value',ShowProtected,true);
  ConfigStore.SetValue(Path+'Levels/',FLevels);
  Modified:=false;
end;

function TCodeBrowserViewOptions.HasLevel(Level: TCodeBrowserLevel): boolean;
begin
  Result:=Levels.IndexOf(CodeBrowserLevelNames[Level])>=0;
end;

initialization
  {$I codebrowser.lrs}

end.

