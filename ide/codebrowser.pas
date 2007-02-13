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
  CodeTree, CodeCache, CodeToolManager,
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
    FCodeTool: TCodeTool;
    FCodeTreeChangeStep: integer;
    FFilename: string;
    FUnitList: TCodeBrowserUnitList;
  public
    constructor Create(const TheFilename: string);
    destructor Destroy; override;
    procedure Clear;
    property Filename: string read FFilename;
    property CodeTool: TCodeTool read FCodeTool;
    property CodeTreeChangeStep: integer read FCodeTreeChangeStep;
    property UnitList: TCodeBrowserUnitList read FUnitList;
    property ChildNodes: TAvgLvlTree read FChildNodes;
  end;
  

  { TCodeBrowserUnitList }

  TCodeBrowserUnitList = class
  private
    FOwner: string;
    FParentList: TCodeBrowserUnitList;
    FUnitLists: TAvgLvlTree;
    FUnits: TAvgLvlTree;
    procedure SetOwner(const AValue: string);
  public
    constructor Create(TheOwner: string; TheParent: TCodeBrowserUnitList);
    destructor Destroy; override;
    procedure Clear;
    property Owner: string read FOwner write SetOwner;// IDE, project, package
    property ParentList: TCodeBrowserUnitList read FParentList;
    property Units: TAvgLvlTree read FUnits;
    property UnitLists: TAvgLvlTree read FUnitLists;
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
    cbwsGatherFiles,
    cbwsUpdateUnits,
    cbwsUpdateNodes,
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
    FWorkingRoot: TCodeBrowserUnitList;
    fUpdateCount: integer;
    fStage: TCodeBrowserWorkStage;
    procedure LoadOptions;
    procedure LoadLevelsCheckGroup;
    procedure LoadSortListBoxes;
    procedure MoveSortItem(Offset: integer);
    procedure AddSortItem;
    procedure RemoveSortItem;
    procedure FillScopeComboBox;
    procedure Work;
    procedure WorkGetOptions;
    procedure WorkGatherPackages;
    procedure WorkFreeUnusedPackages;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    property Root: TCodeBrowserUnitList read FRoot;
    property WorkingRoot: TCodeBrowserUnitList read FWorkingRoot;
    property Options: TCodeBrowserViewOptions read FOptions;
    property IDEAlias: string read FIDEAlias;
    property ProjectAlias: string read FProjectAlias;
  end;
  
var
  CodeBrowserView: TCodeBrowserView = nil;
  

implementation


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

procedure TCodeBrowserView.Work;
// do some work
// This is called during OnIdle, so progress in small steps
begin
  DebugLn(['TCodeBrowserView.Work START']);
  case fStage of
  cbwsGetOptions:     WorkGetOptions;
  cbwsGatherPackages: WorkGatherPackages;
  cbwsFreeUnusedPackages: WorkFreeUnusedPackages;
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
    if CompareText(FWorkingRoot.Owner,CodeBrowserIDEAlias)=0 then begin
      for i:=0 to PackageGraph.Count-1 do
        AddPackage(PackageGraph[i]);
    end else if CompareText(FWorkingRoot.Owner,CodeBrowserProjectAlias)=0 then begin
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
  begin
    // find an unused package
    Result:=nil;
    if (FRoot<>nil) and (FRoot.UnitLists<>nil) then begin
      Node:=FRoot.UnitLists.FindLowest;
      while Node<>nil do begin
        UnusedPackage:=TCodeBrowserUnitList(Node.Data);
        if UnusedPackage<>nil then begin

        end;
        Node:=FRoot.UnitLists.FindSuccessor(Node);
      end;
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
    fStage:=cbwsFreeUnusedPackages;
    exit;
  end;

  // free this unused package
  
end;

procedure TCodeBrowserView.BeginUpdate;
begin
  inc(fUpdateCount);
end;

procedure TCodeBrowserView.EndUpdate;
begin
  dec(fUpdateCount);
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
end;

constructor TCodeBrowserUnitList.Create(TheOwner: string;
  TheParent: TCodeBrowserUnitList);
begin
  FOwner:=TheOwner;
  FParentList:=TheParent;
end;

destructor TCodeBrowserUnitList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCodeBrowserUnitList.Clear;
begin
  if FUnits<>nil then
    FUnits.FreeAndClear;
  FreeAndNil(FUnits);
  if FUnitLists<>nil then
    FUnitLists.FreeAndClear;
  FreeAndNil(FUnitLists);
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

