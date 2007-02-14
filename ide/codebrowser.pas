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
  BasicCodeTools, DefineTemplates, CodeTree, CodeCache, CodeToolManager,
  LinkScanner, FileProcs,
  // IDEIntf
  LazConfigStorage, Project,
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
    FNode: TCodeTreeNode;
    FParentNode: TCodeBrowserNode;
  public
    constructor Create(TheUnit: TCodeBrowserUnit; TheNode: TCodeTreeNode;
                       TheParent: TCodeBrowserNode);
    destructor Destroy; override;
    procedure Clear;
    property CBUnit: TCodeBrowserUnit read FCBUnit;
    property Node: TCodeTreeNode read FNode;
    property ParentNode: TCodeBrowserNode read FParentNode;
    property ChildNodes: TAvgLvlTree read FChildNodes;
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
    FScannedLines: integer;
    FUnitList: TCodeBrowserUnitList;
    procedure SetScanned(const AValue: boolean);
  public
    constructor Create(const TheFilename: string);
    destructor Destroy; override;
    procedure Clear;
    property Filename: string read FFilename;
    property CodeBuffer: TCodeBuffer read FCodeBuffer;
    property CodeTool: TCodeTool read FCodeTool;
    property CodeTreeChangeStep: integer read FCodeTreeChangeStep;
    property UnitList: TCodeBrowserUnitList read FUnitList;
    property ChildNodes: TAvgLvlTree read FChildNodes;
    property ScannedLines: integer read FScannedLines write FScannedLines;
    property ScannedBytes: integer read FScannedBytes write FScannedBytes;
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
    cblUnits,
    cblClasses,
    cblSections
    );
    
const
  CodeBrowserLevelNames: array[TCodeBrowserLevel] of string = (
    'Packages',
    'Units',
    'Classes',
    'Sections'
    );

type
  TCodeBrowserSortItem = (
    cbsiPackages,
    cbsiUnits,
    cbsiClasses,
    cbsiSections,
    cbsiAlphabetically
    );

const
  CodeBrowserSortItemNames: array[TCodeBrowserSortItem] of string = (
    'Packages',
    'Units',
    'Classes',
    'Sections',
    'Alphabetically'
    );
    
  CodeBrowserIDEAlias     = ' '+'Lazarus IDE';// Note: space is needed to avoid name clashing
  CodeBrowserProjectAlias = ' '+'Project';

type

  { TCodeBrowserViewOptions }

  TCodeBrowserViewOptions = class
  private
    FModified: boolean;
    FScope: string;
    FLevels: TStrings;
    FSortItems: TStrings;
    FWithRequiredPackages: boolean;
    procedure SetModified(const AValue: boolean);
    procedure SetScope(const AValue: string);
    procedure SetLevels(const AValue: TStrings);
    procedure SetSortItems(const AValue: TStrings);
    procedure SetWithRequiredPackages(const AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromConfig(ConfigStore: TConfigStorage; const Path: string);
    procedure SaveToConfig(ConfigStore: TConfigStorage; const Path: string);
    property Scope: string read FScope write SetScope;
    property WithRequiredPackages: boolean read FWithRequiredPackages write SetWithRequiredPackages;
    property Levels: TStrings read FLevels write SetLevels;
    property SortItems: TStrings read FSortItems write SetSortItems;
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
    cbwsFinished
    );

  { TCodeBrowserView }

  TCodeBrowserView = class(TForm)
    SortAddsListBox: TListBox;
    SortListBox: TListBox;
    SortGroupBox: TGroupBox;
    LevelsCheckGroup: TCheckGroup;
    ScopeWithRequiredPackagesCheckBox: TCheckBox;
    ScopeComboBox: TComboBox;
    ScopeGroupBox: TGroupBox;
    SortUpSpeedButton: TSpeedButton;
    SortDownSpeedButton: TSpeedButton;
    SortAddSpeedButton: TSpeedButton;
    SortRemoveSpeedButton: TSpeedButton;
    BrowseTreeView: TTreeView;
    StatusBar1: TStatusBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LevelsCheckGroupClick(Sender: TObject);
    procedure ScopeComboBoxDropDown(Sender: TObject);
    procedure ScopeComboBoxEditingDone(Sender: TObject);
    procedure ScopeWithRequiredPackagesCheckBoxChange(Sender: TObject);
    procedure SortAddSpeedButtonClick(Sender: TObject);
    procedure SortDownSpeedButtonClick(Sender: TObject);
    procedure SortRemoveSpeedButtonClick(Sender: TObject);
    procedure SortUpSpeedButtonClick(Sender: TObject);
    procedure OnIdle(Sender: TObject);
  private
    FIDEAlias: string;
    fLocalizedSortItems: TStrings;
    FOptions: TCodeBrowserViewOptions;
    FProjectAlias: string;
    FRoot: TCodeBrowserUnitList;
    FScannedBytes: PtrInt;
    FScannedLines: PtrInt;
    FScannedPackages: integer;
    FScannedUnits: integer;
    FWorkingRoot: TCodeBrowserUnitList;
    fUpdateCount: integer;
    fStage: TCodeBrowserWorkStage;
    fOutdatedFiles: TAvgLvlTree;// tree of TCodeBrowserUnit
    fLastStatusBarUpdate: TDateTime;
    procedure LoadOptions;
    procedure LoadLevelsCheckGroup;
    procedure LoadSortListBoxes;
    procedure MoveSortItem(Offset: integer);
    procedure AddSortItem;
    procedure RemoveSortItem;
    procedure FillScopeComboBox;
    procedure SetScannedBytes(const AValue: PtrInt);
    procedure SetScannedLines(const AValue: PtrInt);
    procedure SetScannedPackages(const AValue: integer);
    procedure SetScannedUnits(const AValue: integer);
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
    procedure FreeUnitList(List: TCodeBrowserUnitList);
    procedure UpdateStatusBar(Lazy: boolean);
    procedure RemoveUnit(AnUnit: TCodeBrowserUnit);
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    property Root: TCodeBrowserUnitList read FRoot;
    property WorkingRoot: TCodeBrowserUnitList read FWorkingRoot;
    property Options: TCodeBrowserViewOptions read FOptions;
    property IDEAlias: string read FIDEAlias;
    property ProjectAlias: string read FProjectAlias;
    property ScannedPackages: integer read FScannedPackages write SetScannedPackages;
    property ScannedUnits: integer read FScannedUnits write SetScannedUnits;
    property ScannedLines: PtrInt read FScannedLines write SetScannedLines;
    property ScannedBytes: PtrInt read FScannedBytes write SetScannedBytes;
  end;
  
var
  CodeBrowserView: TCodeBrowserView = nil;
  
function CompareUnitListOwners(Data1, Data2: Pointer): integer;
function ComparePAnsiStringWithUnitListOwner(Data1, Data2: Pointer): integer;
function CompareUnitFilenames(Data1, Data2: Pointer): integer;
function ComparePAnsiStringWithUnitFilename(Data1, Data2: Pointer): integer;


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


{ TCodeBrowserView }

procedure TCodeBrowserView.FormCreate(Sender: TObject);
begin
  FOptions:=TCodeBrowserViewOptions.Create;
  
  FIDEAlias:='Lazarus IDE';
  FProjectAlias:='Project';

  fLocalizedSortItems:=TStringList.Create;
  fLocalizedSortItems.Values[CodeBrowserSortItemNames[cbsiPackages]]:='Packages';
  fLocalizedSortItems.Values[CodeBrowserSortItemNames[cbsiUnits]]:='Units';
  fLocalizedSortItems.Values[CodeBrowserSortItemNames[cbsiClasses]]:='Classes';
  fLocalizedSortItems.Values[CodeBrowserSortItemNames[cbsiSections]]:='Sections';
  fLocalizedSortItems.Values[CodeBrowserSortItemNames[cbsiAlphabetically]]:='Alphabetically';

  Name:=NonModalIDEWindowNames[nmiwCodeBrowser];
  Caption := 'Code browser';
  EnvironmentOptions.IDEWindowLayoutList.Apply(Self,Name);
  
  ScopeGroupBox.Caption:='Scope';
  ScopeWithRequiredPackagesCheckBox.Caption:='With required packages';
  LevelsCheckGroup.Caption:='Levels';
  LevelsCheckGroup.Items[0]:='Packages';
  LevelsCheckGroup.Items[1]:='Units';
  LevelsCheckGroup.Items[2]:='Classes';
  LevelsCheckGroup.Items[3]:='Sections';

  SortGroupBox.Caption:='Sort';
  SortUpSpeedButton.Hint:='Move up';
  DebugLn(['TCodeBrowserView.FormCreate ',LazarusResources.Find('arrow_up')<>nil]);
  SortUpSpeedButton.Glyph.LoadFromLazarusResource('arrow_up');
  SortDownSpeedButton.Hint:='Move down';
  SortDownSpeedButton.Glyph.LoadFromLazarusResource('arrow_down');
  SortAddSpeedButton.Hint:='Add';
  SortAddSpeedButton.Glyph.LoadFromLazarusResource('arrow_left');
  SortRemoveSpeedButton.Hint:='Remove';
  SortRemoveSpeedButton.Glyph.LoadFromLazarusResource('arrow_right');

  LoadOptions;
  
  fStage:=cbwsGetOptions;
  Application.AddOnIdleHandler(@OnIdle);
end;

procedure TCodeBrowserView.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fOutdatedFiles);
  FreeAndNil(FRoot);
  FreeAndNil(FWorkingRoot);
  FreeAndNil(FOptions);
end;

procedure TCodeBrowserView.LevelsCheckGroupClick(Sender: TObject);
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

procedure TCodeBrowserView.SortAddSpeedButtonClick(Sender: TObject);
begin
  AddSortItem;
end;

procedure TCodeBrowserView.SortDownSpeedButtonClick(Sender: TObject);
begin
  MoveSortItem(+1);
end;

procedure TCodeBrowserView.SortRemoveSpeedButtonClick(Sender: TObject);
begin
  RemoveSortItem;
end;

procedure TCodeBrowserView.SortUpSpeedButtonClick(Sender: TObject);
begin
  MoveSortItem(-1);
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

procedure TCodeBrowserView.LoadOptions;
begin
  BeginUpdate;
  ScopeWithRequiredPackagesCheckBox.Checked:=Options.WithRequiredPackages;
  ScopeComboBox.Text:=Options.Scope;
  LoadLevelsCheckGroup;
  LoadSortListBoxes;
  EndUpdate;
end;

procedure TCodeBrowserView.LoadLevelsCheckGroup;
var
  i: Integer;
begin
  for i:=0 to LevelsCheckGroup.Items.Count-1 do
    LevelsCheckGroup.Checked[i]:=
      Options.Levels.IndexOf(CodeBrowserLevelNames[TCodeBrowserLevel(i)])>=0;
end;

procedure TCodeBrowserView.LoadSortListBoxes;
var
  i: Integer;
  sl: TStringList;
  CurItem: string;
begin
  BeginUpdate;
  sl:=TStringList.Create;
  
  // SortListBox
  for i:=0 to Options.SortItems.Count-1 do begin
    CurItem:=fLocalizedSortItems.Values[Options.SortItems[i]];
    if CurItem='' then continue;
    sl.Add(CurItem);
  end;
  if SortListBox.ItemIndex>=0 then
    CurItem:=SortListBox.Items[SortListBox.ItemIndex]
  else
    CurItem:='';
  SortListBox.Items.Assign(sl);
  if CurItem<>'' then
    SortListBox.ItemIndex:=sl.IndexOf(CurItem);
    
  // SortAddsListBox
  sl.Clear;
  for i:=0 to fLocalizedSortItems.Count-1 do begin
    CurItem:=fLocalizedSortItems.Names[i];
    if Options.SortItems.IndexOf(CurItem)<0 then
      sl.Add(fLocalizedSortItems.ValueFromIndex[i]);
  end;
  if SortAddsListBox.ItemIndex>=0 then
    CurItem:=SortAddsListBox.Items[SortAddsListBox.ItemIndex]
  else
    CurItem:='';
  SortAddsListBox.Items.Assign(sl);
  if CurItem<>'' then
    SortAddsListBox.ItemIndex:=sl.IndexOf(CurItem);

  sl.Free;
  EndUpdate;
end;

procedure TCodeBrowserView.MoveSortItem(Offset: integer);
var
  i: LongInt;
begin
  if Offset=0 then exit;
  i:=SortListBox.ItemIndex;
  if i<0 then exit;
  if (i+Offset)<0 then exit;
  if (i+Offset)>=SortListBox.Items.Count then exit;
  SortListBox.Items.Move(i,i+Offset);
  fStage:=cbwsGetOptions;
end;

procedure TCodeBrowserView.AddSortItem;
var
  i: LongInt;
  NewItem: string;
begin
  i:=SortAddsListBox.ItemIndex;
  if i<0 then exit;
  NewItem:=SortAddsListBox.Items[i];
  SortAddsListBox.Items.Delete(i);
  i:=SortListBox.ItemIndex;
  if i<0 then i:=SortListBox.Items.Count;
  SortListBox.Items.Insert(i,NewItem);
  fStage:=cbwsGetOptions;
end;

procedure TCodeBrowserView.RemoveSortItem;
var
  i: LongInt;
  NewItem: string;
begin
  i:=SortListBox.ItemIndex;
  if i<0 then exit;
  NewItem:=SortListBox.Items[i];
  SortListBox.Items.Delete(i);
  i:=SortAddsListBox.ItemIndex;
  if i<0 then i:=SortAddsListBox.Items.Count;
  SortAddsListBox.Items.Insert(i,NewItem);
  fStage:=cbwsGetOptions;
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
  sl.Insert(0,IDEAlias);
  sl.Insert(1,ProjectAlias);
  ScopeComboBox.Items.Assign(sl);
  sl.Free;
end;

procedure TCodeBrowserView.SetScannedBytes(const AValue: PtrInt);
begin
  if FScannedBytes=AValue then exit;
  FScannedBytes:=AValue;
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
  else
    exit;
  end;
  if ord(OldStage)<ord(cbwsFinished) then begin
    UpdateStatusBar(cbwsFinished<fStage);
  end;
end;

procedure TCodeBrowserView.WorkGetOptions;
var
  i: Integer;
begin
  DebugLn(['TCodeBrowserView.WorkGetOptions START']);
  Options.WithRequiredPackages:=ScopeWithRequiredPackagesCheckBox.Checked;
  Options.Scope:=ScopeComboBox.Text;
  Options.Levels.Clear;
  for i:=0 to LevelsCheckGroup.Items.Count-1 do
    if LevelsCheckGroup.Checked[i] then
      Options.Levels.Add(CodeBrowserLevelNames[TCodeBrowserLevel(i)]);
  Options.SortItems.Clear;
  Options.SortItems.Assign(SortListBox.Items);

  // this stage finished -> next stage
  fStage:=cbwsGatherPackages;
end;

procedure TCodeBrowserView.WorkGatherPackages;

  procedure AddPackage(APackage: TLazPackage);
  begin
    TCodeBrowserUnitList.Create(APackage.Name,fWorkingRoot);
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
  // find root
  RootOwner:='';
  if Options.Scope=IDEAlias then begin
    RootOwner:=CodeBrowserIDEAlias;
  end else if Options.Scope=ProjectAlias then begin
    RootOwner:=CodeBrowserProjectAlias;
  end else begin
    APackage:=PackageGraph.FindAPackageWithName(Options.Scope,nil);
    if APackage<>nil then
      RootOwner:=APackage.Name;
  end;
  DebugLn(['TCodeBrowserView.WorkGatherPackages RootOwner="',RootOwner,'"']);
  FreeAndNil(FWorkingRoot);
  FWorkingRoot:=TCodeBrowserUnitList.Create(RootOwner,nil);
  
  // find required packages
  if Options.WithRequiredPackages then begin
    if SysUtils.CompareText(FWorkingRoot.Owner,CodeBrowserIDEAlias)=0 then begin
      for i:=0 to PackageGraph.Count-1 do
        AddPackage(PackageGraph[i]);
    end else if SysUtils.CompareText(FWorkingRoot.Owner,CodeBrowserProjectAlias)=0
    then begin
      AddPackages(Project1.FirstRequiredDependency);
    end else if FWorkingRoot.Owner<>'' then begin
      APackage:=PackageGraph.FindAPackageWithName(FWorkingRoot.Owner,nil);
      if APackage<>nil then
        AddPackages(APackage.FirstRequiredDependency);
    end;
  end;
  
  // update root item (childs will be updated on next Idle)
  if FRoot=nil then begin
    FRoot:=TCodeBrowserUnitList.Create(FWorkingRoot.Owner,nil);
  end else begin
    FRoot.Owner:=FWorkingRoot.Owner;
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
    // find an unused package (a package in Root but not in WorkingRoot)
    Result:=nil;
    if (FRoot=nil) or (FRoot.UnitLists=nil) then exit;
    Node:=FRoot.UnitLists.FindLowest;
    while Node<>nil do begin
      UnusedPackage:=TCodeBrowserUnitList(Node.Data);
      PackageName:=UnusedPackage.Owner;
      if (FWorkingRoot=nil)
      or (FWorkingRoot.UnitLists=nil)
      or (FWorkingRoot.UnitLists.FindKey(@PackageName,
         @ComparePAnsiStringWithUnitListOwner)=nil)
      then begin
        Result:=UnusedPackage;
        exit;
      end;
      Node:=FRoot.UnitLists.FindSuccessor(Node);
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
  if (FWorkingRoot<>nil) and (FWorkingRoot.UnitLists<>nil)
  and (FRoot<>nil) then begin
    Node:=FWorkingRoot.UnitLists.FindLowest;
    while Node<>nil do begin
      List:=TCodeBrowserUnitList(Node.Data);
      if FRoot.FindUnitList(List.Owner)=nil then begin
        // new unit list
        TCodeBrowserUnitList.Create(List.Owner,FRoot);
        inc(FScannedPackages);
      end;
      Node:=FWorkingRoot.UnitLists.FindSuccessor(Node);
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
  List:=FindListWithInvalidFileList(FRoot);
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
    while AnUnitInfo<>nil do begin
      if FilenameIsPascalUnit(AnUnitInfo.Filename) then
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
      if List.FindUnit(AnUnit.Filename)=nil then begin
        // this unit was not part of List -> add
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
    if List.Owner=CodeBrowserIDEAlias then begin
      AddFilesOfIDE;
    end else if List.Owner=CodeBrowserProjectAlias then begin
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
  AddFiles(Root);

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
        fStage:=cbwsFinished;
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
    inc(FScannedBytes,ByteCnt);
    inc(FScannedLines,LineCnt);
    //DebugLn(['UpdateScannedCounters ',ExtractFileName(AnUnit.Filename),' LineCnt=',LineCnt,' ByteCnt=',ByteCnt]);
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
    AnUnit.ScannedBytes:=0;
    AnUnit.ScannedLines:=0;
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
      if fOutdatedFiles<>nil then
        fOutdatedFiles.Remove(AnUnit);
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
    +' lines='+IntToStr(ScannedLines)
    +' bytes='+IntToStr(ScannedBytes);
  if fStage<>cbwsFinished then
    s:=s+'. Scanning ...';
  StatusBar1.SimpleText:=s;
end;

procedure TCodeBrowserView.RemoveUnit(AnUnit: TCodeBrowserUnit);
begin
  if AnUnit.Scanned then begin
    dec(FScannedUnits);
    dec(FScannedLines,AnUnit.ScannedLines);
    dec(FScannedBytes,AnUnit.ScannedBytes);
    AnUnit.Scanned:=false;
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

{ TCodeBrowserNode }

constructor TCodeBrowserNode.Create(TheUnit: TCodeBrowserUnit;
  TheNode: TCodeTreeNode; TheParent: TCodeBrowserNode);
begin
  FCBUnit:=TheUnit;
  FNode:=TheNode;
  FParentNode:=TheParent;
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

{ TCodeBrowserUnit }

procedure TCodeBrowserUnit.SetScanned(const AValue: boolean);
begin
  if FScanned=AValue then exit;
  FScanned:=AValue;
  FScannedBytes:=0;
  FScannedLines:=0;
  if UnitList<>nil then begin
    if FScanned then
      inc(UnitList.FScannedUnits)
    else
      dec(UnitList.FScannedUnits);
  end;
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
    FUnitLists.Add(List);
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
    FUnits.Add(AnUnit);
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

procedure TCodeBrowserViewOptions.SetSortItems(const AValue: TStrings);
begin
  if FSortItems=AValue then exit;
  if FSortItems.Text=AValue.Text then exit;
  FSortItems.Assign(AValue);
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
  FSortItems:=TStringList.Create;
  FLevels:=TStringList.Create;
  Clear;
end;

destructor TCodeBrowserViewOptions.Destroy;
begin
  FreeAndNil(FSortItems);
  FreeAndNil(FLevels);
  inherited Destroy;
end;

procedure TCodeBrowserViewOptions.Clear;
begin
  FLevels.Clear;
  FSortItems.Text:=CodeBrowserSortItemNames[cbsiAlphabetically];
  FLevels.Text:=CodeBrowserLevelNames[cblPackages]+#13
               +CodeBrowserLevelNames[cblUnits]+#13
               +CodeBrowserLevelNames[cblClasses]+#13
               +CodeBrowserLevelNames[cblSections];
  WithRequiredPackages:=false;
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
  ConfigStore.GetValue(Path+'SortItems/',fSortItems);
  ConfigStore.GetValue(Path+'Levels/',FLevels);
  Modified:=false;
end;

procedure TCodeBrowserViewOptions.SaveToConfig(ConfigStore: TConfigStorage;
  const Path: string);
begin
  ConfigStore.SetDeleteValue(Path+'WithRequiredPackages/Value',
                             WithRequiredPackages,false);
  ConfigStore.SetDeleteValue(Path+'Scope/Value',Scope,'Project');
  ConfigStore.SetValue(Path+'SortItems/',fSortItems);
  ConfigStore.SetValue(Path+'Levels/',FLevels);
  Modified:=false;
end;

initialization
  {$I codebrowser.lrs}

end.

