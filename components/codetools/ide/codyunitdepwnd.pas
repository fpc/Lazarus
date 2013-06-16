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
    IDE Window showing dependencies of units and packages.

  ToDo:
    - add refresh button to rescan
    - delay update pages when not visible
    - update pages when becoming visible
    - additional files as start units
    - view:
      - flag show nodes for project/package
      - flag show nodes for directories
      - flag allow multiselect
      - filter units
      - text search with highlight, next, previous
      - double click: open one unit
    - selected units
      - show owner units as tree structure
      - show connected units: used via interface, via implementation, used by interface, used by implementation
      - expand node: show connected units
      - collapse node: free child nodes
      - text search with highlight, next, previous
      - double click: open one unit
    - resourcestrings
}
unit CodyUnitDepWnd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, LazLogger, LazFileUtils,
  Forms, Controls, ExtCtrls, ComCtrls, StdCtrls, Buttons, LvlGraphCtrl,
  LazIDEIntf, ProjectIntf, IDEWindowIntf, PackageIntf, SrcEditorIntf,
  CodeToolManager, DefineTemplates, CodeToolsStructs,
  CTUnitGraph, CTUnitGroupGraph, FileProcs;

const
  GroupPrefixProject = '-Project-';
  GroupPrefixFPCSrc = 'FPC:';
  GroupNone = '-None-';
type

  { TUnitDependenciesWindow }

  TUnitDependenciesWindow = class(TForm)
    AllUnitsFilterEdit: TEdit;
    AllUnitsMultiselectSpeedButton: TSpeedButton;
    AllUnitsSearchEdit: TEdit;
    AllUnitsSearchNextSpeedButton: TSpeedButton;
    AllUnitsSearchPrevSpeedButton: TSpeedButton;
    AllUnitsGroupBox: TGroupBox;
    AllUnitsShowDirsSpeedButton: TSpeedButton;
    AllUnitsShowGroupNodesSpeedButton: TSpeedButton;
    AllUnitsTreeView: TTreeView;
    BtnPanel: TPanel;
    MainPageControl: TPageControl;
    ProgressBar1: TProgressBar;
    GroupsTabSheet: TTabSheet;
    GroupsSplitter: TSplitter;
    SelectedUnitsGroupBox: TGroupBox;
    SelUnitsSearchEdit: TEdit;
    SelUnitsSearchNextSpeedButton: TSpeedButton;
    SelUnitsSearchPrevSpeedButton: TSpeedButton;
    SelUnitsTreeView: TTreeView;
    UnitScopeAddFilesButton: TButton;
    UnitScopeAddFilesCheckBox: TCheckBox;
    ScopePanel: TPanel;
    UnitScopeAddFilesComboBox: TComboBox;
    UnitsSplitter: TSplitter;
    UnitsTabSheet: TTabSheet;
    Timer1: TTimer;
    procedure AllUnitsMultiselectSpeedButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GroupsLvlGraphSelectionChanged(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    FAllUnitsMultiSelect: boolean;
    FCurrentUnit: TUGUnit;
    FIdleConnected: boolean;
    FUsesGraph: TUsesGraph;
    FGroups: TUGGroups; // referenced by Nodes.Data of GroupsLvlGraph
    procedure SetAllUnitsMultiSelect(AValue: boolean);
    procedure SetCurrentUnit(AValue: TUGUnit);
    procedure SetIdleConnected(AValue: boolean);
    procedure CreateGroups;
    function CreateProjectGroup(AProject: TLazProject): TUGGroup;
    function CreatePackageGroup(APackage: TIDEPackage): TUGGroup;
    procedure CreateFPCSrcGroups;
    procedure GuessGroupOfUnits;
    procedure AddStartAndTargetUnits;
    procedure AddAdditionalFilesAsStartUnits;
    procedure SetupGroupsTabSheet;
    procedure SetupUnitsTabSheet;
    procedure UpdateAll;
    procedure UpdateGroupsLvlGraph;
    procedure UpdateUnitsLvlGraph;
    function NodeTextToUnit(NodeText: string): TUGUnit;
    function UGUnitToNodeText(UGUnit: TUGUnit): string;
    function GetFPCSrcDir: string;
    function IsFPCSrcGroup(Group: TUGGroup): boolean;
    function IsProjectGroup(Group: TUGGroup): boolean;
  public
    GroupsLvlGraph: TLvlGraphControl; // Nodes.Data are TUGGroup of Groups
    UnitsLvlGraph: TLvlGraphControl; // Nodes.Data are Units in Groups
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    property UsesGraph: TUsesGraph read FUsesGraph;
    property Groups: TUGGroups read FGroups;
    property CurrentUnit: TUGUnit read FCurrentUnit write SetCurrentUnit;
    property AllUnitsMultiSelect: boolean read FAllUnitsMultiSelect write SetAllUnitsMultiSelect;
  end;

var
  UnitDependenciesWindow: TUnitDependenciesWindow;

procedure ShowUnitDependenciesClicked(Sender: TObject);
procedure ShowUnitDependencies(Show, BringToFront: boolean);

implementation

procedure ShowUnitDependenciesClicked(Sender: TObject);
begin
  ShowUnitDependencies(true,true);
end;

procedure ShowUnitDependencies(Show, BringToFront: boolean);
begin
  if UnitDependenciesWindow = Nil then
    Application.CreateForm(TUnitDependenciesWindow, UnitDependenciesWindow);
  if Show then
  begin
    IDEWindowCreators.ShowForm(UnitDependenciesWindow,BringToFront);
  end;
end;

{ TUnitDependenciesWindow }

procedure TUnitDependenciesWindow.FormCreate(Sender: TObject);
begin
  FUsesGraph:=CodeToolBoss.CreateUsesGraph;
  FGroups:=TUGGroups.Create(FUsesGraph);
  ProgressBar1.Style:=pbstMarquee;
  AddStartAndTargetUnits;

  Caption:='Unit Dependencies';

  MainPageControl.ActivePage:=UnitsTabSheet;

  SetupUnitsTabSheet;
  SetupGroupsTabSheet;

  IdleConnected:=true;
end;

procedure TUnitDependenciesWindow.AllUnitsMultiselectSpeedButtonClick(
  Sender: TObject);
begin
  AllUnitsMultiSelect:=AllUnitsMultiselectSpeedButton.Down;
end;

procedure TUnitDependenciesWindow.FormDestroy(Sender: TObject);
begin
  IdleConnected:=false;
  GroupsLvlGraph.Clear;
  UnitsLvlGraph.Clear;
  FreeAndNil(FGroups);
  FreeAndNil(FUsesGraph);
end;

procedure TUnitDependenciesWindow.GroupsLvlGraphSelectionChanged(Sender: TObject
  );
begin
  UpdateUnitsLvlGraph;
end;

procedure TUnitDependenciesWindow.OnIdle(Sender: TObject; var Done: Boolean);
var
  Completed: boolean;
begin
  UsesGraph.Parse(true,Completed,200);
  if Completed then begin
    CreateGroups;
    IdleConnected:=false;
    ProgressBar1.Visible:=false;
    ProgressBar1.Style:=pbstNormal;
    Timer1.Enabled:=false;
    UpdateAll;
  end;
end;

procedure TUnitDependenciesWindow.Timer1Timer(Sender: TObject);
begin
  UpdateAll;
end;

procedure TUnitDependenciesWindow.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TUnitDependenciesWindow.CreateGroups;
var
  i: Integer;
begin
  CreateProjectGroup(LazarusIDE.ActiveProject);
  for i:=0 to PackageEditingInterface.GetPackageCount-1 do
    CreatePackageGroup(PackageEditingInterface.GetPackages(i));
  CreateFPCSrcGroups;
  GuessGroupOfUnits;
end;

function TUnitDependenciesWindow.CreateProjectGroup(AProject: TLazProject
  ): TUGGroup;
var
  i: Integer;
  Filename: String;
  CurUnit: TUGUnit;
  ProjFile: TLazProjectFile;
begin
  if AProject=nil then exit;
  Result:=Groups.GetGroup(GroupPrefixProject,true);
  //debugln(['TUnitDependenciesDialog.CreateProjectGroup ',Result.Name,' FileCount=',AProject.FileCount]);
  for i:=0 to AProject.FileCount-1 do begin
    ProjFile:=AProject.Files[i];
    if not ProjFile.IsPartOfProject then continue;
    Filename:=AProject.Files[i].Filename;
    CurUnit:=UsesGraph.GetUnit(Filename,false);
    if CurUnit=nil then continue;
    if not (CurUnit is TUGGroupUnit) then begin
      debugln(['TUnitDependenciesDialog.CreateProjectGroup WARNING: ',CurUnit.Filename,' ',CurUnit.Classname,' should be TUGGroupUnit']);
      continue;
    end;
    if TUGGroupUnit(CurUnit).Group<>nil then continue;
    Result.AddUnit(TUGGroupUnit(CurUnit));
  end;
end;

function TUnitDependenciesWindow.CreatePackageGroup(APackage: TIDEPackage
  ): TUGGroup;
var
  i: Integer;
  Filename: String;
  CurUnit: TUGUnit;
begin
  if APackage=nil then exit;
  Result:=Groups.GetGroup(APackage.Name,true);
  //debugln(['TUnitDependenciesDialog.CreatePackageGroup ',Result.Name]);
  for i:=0 to APackage.FileCount-1 do begin
    Filename:=APackage.Files[i].GetFullFilename;
    CurUnit:=UsesGraph.GetUnit(Filename,false);
    if CurUnit is TUGGroupUnit then begin
      if TUGGroupUnit(CurUnit).Group<>nil then continue;
      Result.AddUnit(TUGGroupUnit(CurUnit));
    end;
  end;
end;

procedure TUnitDependenciesWindow.CreateFPCSrcGroups;

  function ExtractFilePathStart(Filename: string; DirCount: integer): string;
  var
    p: Integer;
  begin
    p:=1;
    while p<=length(Filename) do begin
      if Filename[p]=PathDelim then begin
        DirCount-=1;
        if DirCount=0 then begin
          Result:=LeftStr(Filename,p-1);
          exit;
        end;
      end;
      inc(p);
    end;
    Result:=Filename;
  end;

var
  FPCSrcDir: String;
  Node: TAVLTreeNode;
  CurUnit: TUGGroupUnit;
  Directory: String;
  Grp: TUGGroup;
begin
  FPCSrcDir:=AppendPathDelim(GetFPCSrcDir);

  // for each unit in the fpc source directory:
  // if in rtl/ put into group GroupPrefixFPCSrc+RTL
  // if in packages/<name>, put in group GroupPrefixFPCSrc+<name>
  Node:=UsesGraph.FilesTree.FindLowest;
  while Node<>nil do begin
    CurUnit:=TUGGroupUnit(Node.Data);
    Node:=UsesGraph.FilesTree.FindSuccessor(Node);
    if TUGGroupUnit(CurUnit).Group<>nil then continue;
    if CompareFilenames(FPCSrcDir,LeftStr(CurUnit.Filename,length(FPCSrcDir)))<>0
    then
      continue;
    // a unit in the FPC sources
    Directory:=ExtractFilePath(CurUnit.Filename);
    Directory:=copy(Directory,length(FPCSrcDir)+1,length(Directory));
    Directory:=ExtractFilePathStart(Directory,2);
    if LeftStr(Directory,length('rtl'))='rtl' then
      Directory:='RTL'
    else if LeftStr(Directory,length('packages'))='packages' then
      System.Delete(Directory,1,length('packages'+PathDelim));
    Grp:=Groups.GetGroup(GroupPrefixFPCSrc+Directory,true);
    //debugln(['TUnitDependenciesDialog.CreateFPCSrcGroups ',Grp.Name]);
    Grp.AddUnit(TUGGroupUnit(CurUnit));
  end;
end;

procedure TUnitDependenciesWindow.GuessGroupOfUnits;
var
  Node: TAVLTreeNode;
  CurUnit: TUGGroupUnit;
  Filename: String;
  Owners: TFPList;
  i: Integer;
  Group: TUGGroup;
  CurDirectory: String;
  LastDirectory: Char;
begin
  Owners:=nil;
  LastDirectory:='.';
  Node:=UsesGraph.FilesTree.FindLowest;
  while Node<>nil do begin
    CurUnit:=TUGGroupUnit(Node.Data);
    if TUGGroupUnit(CurUnit).Group=nil then begin
      Filename:=CurUnit.Filename;
      debugln(['TUnitDependenciesDialog.GuessGroupOfUnits no group for ',Filename]);
      CurDirectory:=ExtractFilePath(Filename);
      if CompareFilenames(CurDirectory,LastDirectory)<>0 then begin
        FreeAndNil(Owners);
        Owners:=PackageEditingInterface.GetPossibleOwnersOfUnit(Filename,[piosfIncludeSourceDirectories]);
      end;
      Group:=nil;
      if (Owners<>nil) then begin
        for i:=0 to Owners.Count-1 do begin
          if TObject(Owners[i]) is TLazProject then begin
            Group:=Groups.GetGroup(GroupPrefixProject,true);
            debugln(['TUnitDependenciesDialog.GuessGroupOfUnits ',Group.Name]);
            break;
          end else if TObject(Owners[i]) is TIDEPackage then begin
            Group:=Groups.GetGroup(TIDEPackage(Owners[i]).Name,true);
            debugln(['TUnitDependenciesDialog.GuessGroupOfUnits ',Group.Name]);
            break;
          end;
        end;
      end;
      if Group=nil then begin
        Group:=Groups.GetGroup(GroupNone,true);
        debugln(['TUnitDependenciesDialog.GuessGroupOfUnits ',Group.Name]);
      end;
      Group.AddUnit(TUGGroupUnit(CurUnit));
    end;
    Node:=UsesGraph.FilesTree.FindSuccessor(Node);
  end;
  FreeAndNil(Owners);
end;

procedure TUnitDependenciesWindow.SetCurrentUnit(AValue: TUGUnit);
begin
  if FCurrentUnit=AValue then Exit;
  FCurrentUnit:=AValue;
end;

procedure TUnitDependenciesWindow.SetAllUnitsMultiSelect(AValue: boolean);
begin
  if FAllUnitsMultiSelect=AValue then Exit;
  FAllUnitsMultiSelect:=AValue;
  AllUnitsMultiselectSpeedButton.Down:=AllUnitsMultiSelect;
  AllUnitsTreeView.MultiSelect:=AllUnitsMultiSelect;
end;

procedure TUnitDependenciesWindow.AddStartAndTargetUnits;
var
  aProject: TLazProject;
  i: Integer;
  SrcEdit: TSourceEditorInterface;
  AFilename: String;
begin
  UsesGraph.TargetAll:=true;

  // project lpr
  aProject:=LazarusIDE.ActiveProject;
  if (aProject<>nil) and (aProject.MainFile<>nil) then
    UsesGraph.AddStartUnit(aProject.MainFile.Filename);

  // ToDo: add all open packages

  // add all source editor files
  for i:=0 to SourceEditorManagerIntf.SourceEditorCount-1 do begin
    SrcEdit:=SourceEditorManagerIntf.SourceEditors[i];
    AFilename:=SrcEdit.FileName;
    if FilenameIsPascalUnit(AFilename) then
      UsesGraph.AddStartUnit(AFilename);
  end;

  // additional units and directories
  AddAdditionalFilesAsStartUnits;
end;

procedure TUnitDependenciesWindow.AddAdditionalFilesAsStartUnits;
var
  List: TCaption;
  aFilename: String;
  Files: TStrings;
  i: Integer;
  p: Integer;
begin
  List:=UnitScopeAddFilesComboBox.Text;
  p:=1;
  while p<=length(List) do begin
    aFilename:=TrimAndExpandFilename(GetNextDelimitedItem(List,';',p));
    if (AFilename='') then continue;
    if not FileExistsCached(aFilename) then continue;
    if DirPathExistsCached(aFilename) then begin
      aFilename:=AppendPathDelim(aFilename);
      // add all units in directory
      Files:=nil;
      try
        CodeToolBoss.DirectoryCachePool.GetListing(aFilename,Files,false);
        if Files<>nil then begin
          for i:=0 to Files.Count-1 do begin
            if FilenameIsPascalUnit(Files[i]) then
              UsesGraph.AddStartUnit(aFilename+Files[i]);
          end;
        end;
      finally
        Files.Free;
      end;
    end else begin
      // add a single file
      UsesGraph.AddStartUnit(aFilename);
    end;
  end;
end;

procedure TUnitDependenciesWindow.SetupGroupsTabSheet;
begin
  GroupsTabSheet.Caption:='Projects and packages';

  GroupsLvlGraph:=TLvlGraphControl.Create(Self);
  with GroupsLvlGraph do
  begin
    Name:='GroupsLvlGraph';
    Caption:='';
    Align:=alTop;
    Height:=200;
    NodeStyle.GapBottom:=5;
    Parent:=GroupsTabSheet;
    OnSelectionChanged:=@GroupsLvlGraphSelectionChanged;
  end;

  GroupsSplitter.Top:=GroupsLvlGraph.Height;

  UnitsLvlGraph:=TLvlGraphControl.Create(Self);
  with UnitsLvlGraph do
  begin
    Name:='UnitsLvlGraph';
    Caption:='';
    Align:=alClient;
    NodeStyle.GapBottom:=5;
    Parent:=GroupsTabSheet;
  end;
end;

procedure TUnitDependenciesWindow.SetupUnitsTabSheet;
begin
  UnitsTabSheet.Caption:='Units';

  // start searching
  UnitScopeAddFilesCheckBox.Caption:='Additional directories:';
  UnitScopeAddFilesCheckBox.Hint:='By default only the project units and the source editor units are searched. Add here a list of directories separated by semicolon to search as well.';
  UnitScopeAddFilesComboBox.Text:='';
  UnitScopeAddFilesButton.Caption:='Browse';

  // view all units
  AllUnitsFilterEdit.Text:='(Filter)';
  AllUnitsMultiselectSpeedButton.Hint:='Allow to select multiple units';
  AllUnitsShowDirsSpeedButton.Hint:='Show nodes for directories';
  AllUnitsShowDirsSpeedButton.LoadGlyphFromLazarusResource('pkg_hierarchical');
  AllUnitsShowGroupNodesSpeedButton.Hint:='Show nodes for project and packages';
  AllUnitsShowGroupNodesSpeedButton.LoadGlyphFromLazarusResource('pkg_hierarchical');

  AllUnitsSearchEdit.Text:='(Filter)';
  AllUnitsSearchNextSpeedButton.Hint:='Search next occurence of this phrase';
  AllUnitsSearchNextSpeedButton.LoadGlyphFromLazarusResource('arrow_down');
  AllUnitsSearchPrevSpeedButton.Hint:='Search previous occurence of this phrase';
  AllUnitsSearchPrevSpeedButton.LoadGlyphFromLazarusResource('arrow_up');

  // selected units
  SelUnitsSearchEdit.Text:='(Filter)';
  SelUnitsSearchNextSpeedButton.Hint:='Search next unit of this phrase';
  SelUnitsSearchNextSpeedButton.LoadGlyphFromLazarusResource('arrow_down');
  SelUnitsSearchPrevSpeedButton.Hint:='Search previous unit of this phrase';
  SelUnitsSearchPrevSpeedButton.LoadGlyphFromLazarusResource('arrow_up');
end;

procedure TUnitDependenciesWindow.UpdateAll;
begin
  UpdateGroupsLvlGraph;
  UpdateUnitsLvlGraph;
end;

procedure TUnitDependenciesWindow.UpdateGroupsLvlGraph;
var
  AVLNode: TAVLTreeNode;
  Group: TUGGroup;
  Graph: TLvlGraph;
  PkgList: TFPList;
  i: Integer;
  RequiredPkg: TIDEPackage;
  GroupObj: TObject;
  GraphGroup: TLvlGraphNode;
  UnitNode: TAVLTreeNode;
  GrpUnit: TUGGroupUnit;
  UsedUnit: TUGGroupUnit;
begin
  GroupsLvlGraph.BeginUpdate;
  Graph:=GroupsLvlGraph.Graph;
  Graph.Clear;
  AVLNode:=Groups.Groups.FindLowest;
  while AVLNode<>nil do begin
    Group:=TUGGroup(AVLNode.Data);
    AVLNode:=Groups.Groups.FindSuccessor(AVLNode);
    // ToDo: IsFPCSrcGroup
    GraphGroup:=Graph.GetNode(Group.Name,true);
    GraphGroup.Data:=Group;
    GroupObj:=nil;
    if IsProjectGroup(Group) then begin
      // project
      GroupObj:=LazarusIDE.ActiveProject;
      GraphGroup.Selected:=true;
    end else begin
      // package
      GroupObj:=PackageEditingInterface.FindPackageWithName(Group.Name);
    end;
    if GroupObj<>nil then begin
      // add lpk dependencies
      PkgList:=nil;
      try
        PackageEditingInterface.GetRequiredPackages(GroupObj,PkgList,[pirNotRecursive]);
        if (PkgList<>nil) then begin
          // add for each dependency an edge in the Graph
          for i:=0 to PkgList.Count-1 do begin
            RequiredPkg:=TIDEPackage(PkgList[i]);
            Graph.GetEdge(GraphGroup,Graph.GetNode(RequiredPkg.Name,true),true);
          end;
        end;
      finally
        PkgList.Free;
      end;
    end else if IsFPCSrcGroup(Group) then begin
      // add FPC source dependencies
      UnitNode:=Group.Units.FindLowest;
      while UnitNode<>nil do begin
        GrpUnit:=TUGGroupUnit(UnitNode.Data);
        UnitNode:=Group.Units.FindSuccessor(UnitNode);
        if GrpUnit.UsesUnits=nil then continue;
        for i:=0 to GrpUnit.UsesUnits.Count-1 do begin
          UsedUnit:=TUGGroupUnit(TUGUses(GrpUnit.UsesUnits[i]).UsesUnit);
          if (UsedUnit.Group=nil) or (UsedUnit.Group=Group) then continue;
          Graph.GetEdge(GraphGroup,Graph.GetNode(UsedUnit.Group.Name,true),true);
        end;
      end;
    end;
  end;
  GroupsLvlGraph.EndUpdate;
end;

procedure TUnitDependenciesWindow.UpdateUnitsLvlGraph;

  function UnitToCaption(AnUnit: TUGUnit): string;
  begin
    Result:=ExtractFileNameOnly(AnUnit.Filename);
  end;

var
  GraphGroup: TLvlGraphNode;
  NewUnits: TFilenameToPointerTree;
  UnitGroup: TUGGroup;
  AVLNode: TAVLTreeNode;
  GroupUnit: TUGGroupUnit;
  i: Integer;
  HasChanged: Boolean;
  Graph: TLvlGraph;
  CurUses: TUGUses;
  SourceGraphNode: TLvlGraphNode;
  TargetGraphNode: TLvlGraphNode;
  NewGroups: TStringToPointerTree;
  UsedUnit: TUGGroupUnit;
begin
  NewGroups:=TStringToPointerTree.Create(false);
  NewUnits:=TFilenameToPointerTree.Create(false);
  try
    // fetch new list of units
    GraphGroup:=GroupsLvlGraph.Graph.FirstSelected;
    while GraphGroup<>nil do begin
      UnitGroup:=TUGGroup(GraphGroup.Data);
      if UnitGroup<>nil then begin
        NewGroups[UnitGroup.Name]:=UnitGroup;
        AVLNode:=UnitGroup.Units.FindLowest;
        while AVLNode<>nil do begin
          GroupUnit:=TUGGroupUnit(AVLNode.Data);
          NewUnits[GroupUnit.Filename]:=GroupUnit;
          AVLNode:=UnitGroup.Units.FindSuccessor(AVLNode);
        end;
      end;
      GraphGroup:=GraphGroup.NextSelected;
    end;

    // check if something changed
    Graph:=UnitsLvlGraph.Graph;
    HasChanged:=false;
    i:=0;
    AVLNode:=NewUnits.Tree.FindLowest;
    while AVLNode<>nil do begin
      GroupUnit:=TUGGroupUnit(NewUnits.GetNodeData(AVLNode)^.Value);
      if (Graph.NodeCount<=i) or (Graph.Nodes[i].Data<>Pointer(GroupUnit)) then
      begin
        HasChanged:=true;
        break;
      end;
      i+=1;
      AVLNode:=NewUnits.Tree.FindSuccessor(AVLNode);
    end;
    if i<Graph.NodeCount then HasChanged:=true;
    if not HasChanged then exit;

    // units changed -> update level graph of units
    UnitsLvlGraph.BeginUpdate;
    Graph.Clear;
    AVLNode:=NewUnits.Tree.FindLowest;
    while AVLNode<>nil do begin
      GroupUnit:=TUGGroupUnit(NewUnits.GetNodeData(AVLNode)^.Value);
      SourceGraphNode:=Graph.GetNode(UnitToCaption(GroupUnit),true);
      if GroupUnit.UsesUnits<>nil then begin
        for i:=0 to GroupUnit.UsesUnits.Count-1 do begin
          CurUses:=TUGUses(GroupUnit.UsesUnits[i]);
          UsedUnit:=TUGGroupUnit(CurUses.UsesUnit);
          if UsedUnit.Group=nil then continue;
          if not NewGroups.Contains(UsedUnit.Group.Name) then continue;
          TargetGraphNode:=Graph.GetNode(UnitToCaption(UsedUnit),true);
          Graph.GetEdge(SourceGraphNode,TargetGraphNode,true);
        end;
      end;
      AVLNode:=NewUnits.Tree.FindSuccessor(AVLNode);
    end;

    UnitsLvlGraph.EndUpdate;
  finally
    NewGroups.Free;
    NewUnits.Free;
  end;
end;

function TUnitDependenciesWindow.NodeTextToUnit(NodeText: string): TUGUnit;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=UsesGraph.FilesTree.FindLowest;
  while AVLNode<>nil do begin
    Result:=TUGUnit(AVLNode.Data);
    if NodeText=UGUnitToNodeText(Result) then exit;
    AVLNode:=UsesGraph.FilesTree.FindSuccessor(AVLNode);
  end;
  Result:=nil;
end;

function TUnitDependenciesWindow.UGUnitToNodeText(UGUnit: TUGUnit): string;
begin
  Result:=ExtractFileName(UGUnit.Filename);
end;

function TUnitDependenciesWindow.GetFPCSrcDir: string;
var
  UnitSet: TFPCUnitSetCache;
begin
  UnitSet:=CodeToolBoss.GetUnitSetForDirectory('');
  Result:=UnitSet.FPCSourceDirectory;
end;

function TUnitDependenciesWindow.IsFPCSrcGroup(Group: TUGGroup): boolean;
begin
  Result:=(Group<>nil) and (LeftStr(Group.Name,length(GroupPrefixFPCSrc))=GroupPrefixFPCSrc);
end;

function TUnitDependenciesWindow.IsProjectGroup(Group: TUGGroup): boolean;
begin
  Result:=(Group<>nil) and (Group.Name=GroupPrefixProject);
end;

{$R *.lfm}

end.

