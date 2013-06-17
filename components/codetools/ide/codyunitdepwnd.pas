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
  Classes, SysUtils, AVL_Tree, LazLogger, LazFileUtils, LazUTF8, Forms,
  Controls, ExtCtrls, ComCtrls, StdCtrls, Buttons, Dialogs, LvlGraphCtrl,
  LazIDEIntf, ProjectIntf, IDEWindowIntf, PackageIntf, SrcEditorIntf,
  IDEDialogs, IDEImagesIntf, CodeToolManager, DefineTemplates, CodeToolsStructs,
  CTUnitGraph, CTUnitGroupGraph, FileProcs;

const
  GroupPrefixProject = '-Project-';
  GroupPrefixFPCSrc = 'FPC:';
  GroupNone = '-None-';
type
  TUDNodeType = (
    udnNone,
    udnGroup,
    udnDirectory,
    udnInterface,
    udnImplementation,
    udnUsedByInterface,
    udnUsedByImplementation,
    udnUnit
    );
  TUDNodeTypes = set of TUDNodeType;

  { TUDBaseNode }

  TUDBaseNode = class
  public
    TVNode: TTreeNode;
    NodeText: string;
    Typ: TUDNodeType;
    Identifier: string; // GroupName, Directory, Filename
    Group: string;
  end;

  { TUDNode }

  TUDNode = class(TUDBaseNode)
  public
    Parent: TUDNode;
    ChildNodes: TAVLTree; // tree of TUDNode sorted for Typ and NodeText
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetNode(aTyp: TUDNodeType; const ANodeText: string;
      CreateIfNotExists: boolean = false): TUDNode;
    function Count: integer;
  end;

  TUDWFlag = (
    udwParsing,
    udwNeedUpdateGroupsLvlGraph,
    udwNeedUpdateUnitsLvlGraph,
    udwNeedUpdateAllUnitsTreeView
    );
  TUDWFlags = set of TUDWFlag;

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
    AllUnitsTreeView: TTreeView; // Node.Data is TUDNode
    BtnPanel: TPanel;
    MainPageControl: TPageControl;
    ProgressBar1: TProgressBar;
    GroupsTabSheet: TTabSheet;
    GroupsSplitter: TSplitter;
    SearchPkgsCheckBox: TCheckBox;
    SearchSrcEditCheckBox: TCheckBox;
    SelectedUnitsGroupBox: TGroupBox;
    SelUnitsSearchEdit: TEdit;
    SelUnitsSearchNextSpeedButton: TSpeedButton;
    SelUnitsSearchPrevSpeedButton: TSpeedButton;
    SelUnitsTreeView: TTreeView;
    SearchCustomFilesBrowseButton: TButton;
    SearchCustomFilesCheckBox: TCheckBox;
    ScopePanel: TPanel;
    SearchCustomFilesComboBox: TComboBox;
    UnitsSplitter: TSplitter;
    UnitsTabSheet: TTabSheet;
    Timer1: TTimer;
    procedure AllUnitsFilterEditChange(Sender: TObject);
    procedure AllUnitsMultiselectSpeedButtonClick(Sender: TObject);
    procedure AllUnitsShowDirsSpeedButtonClick(Sender: TObject);
    procedure AllUnitsShowGroupNodesSpeedButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GroupsLvlGraphSelectionChanged(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure SearchPkgsCheckBoxChange(Sender: TObject);
    procedure SearchSrcEditCheckBoxChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SearchCustomFilesBrowseButtonClick(Sender: TObject);
    procedure SearchCustomFilesCheckBoxChange(Sender: TObject);
    procedure SearchCustomFilesComboBoxChange(Sender: TObject);
  private
    FAllUnitsMultiSelect: boolean;
    FCurrentUnit: TUGUnit;
    FIdleConnected: boolean;
    FUsesGraph: TUsesGraph;
    FGroups: TUGGroups; // referenced by Nodes.Data of GroupsLvlGraph
    FAllUnitsRootUDNode: TUDNode;
    FFlags: TUDWFlags;
    fImgIndexProject: integer;
    fImgIndexUnit: integer;
    fImgIndexPackage: integer;
    fImgIndexDirectory: integer;
    function CreateAllUnitsTree: TUDNode;
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
    procedure UpdateUnitsButtons;
    procedure UpdateAll;
    procedure UpdateGroupsLvlGraph;
    procedure UpdateUnitsLvlGraph;
    procedure UpdateAllUnitsTreeView;
    function GetImgIndex(Node: TUDNode): integer;
    function NodeTextToUnit(NodeText: string): TUGUnit;
    function UGUnitToNodeText(UGUnit: TUGUnit): string;
    function GetFPCSrcDir: string;
    function IsFPCSrcGroup(Group: TUGGroup): boolean;
    function IsProjectGroup(Group: TUGGroup): boolean;
    function GetAllUnitsFilter: string;
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

function CompareUDBaseNodes(UDNode1, UDNode2: Pointer): integer;

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

function CompareUDBaseNodes(UDNode1, UDNode2: Pointer): integer;
var
  Node1: TUDBaseNode absolute UDNode1;
  Node2: TUDBaseNode absolute UDNode2;
begin
  Result:=ord(Node1.Typ)-ord(Node2.Typ);
  if Result<>0 then exit;
  case Node1.Typ of
  udnDirectory: Result:=CompareFilenames(Node1.NodeText,Node2.NodeText);
  else Result:=SysUtils.CompareText(Node1.NodeText,Node2.NodeText);
  end;
end;

{ TUDNode }

constructor TUDNode.Create;
begin
  ChildNodes:=TAVLTree.Create(@CompareUDBaseNodes);
end;

destructor TUDNode.Destroy;
begin
  Clear;
  FreeAndNil(ChildNodes);
  inherited Destroy;
end;

procedure TUDNode.Clear;
begin
  ChildNodes.FreeAndClear;
end;

function TUDNode.GetNode(aTyp: TUDNodeType; const ANodeText: string;
  CreateIfNotExists: boolean): TUDNode;
var
  Node: TUDBaseNode;
  AVLNode: TAVLTreeNode;
begin
  Node:=TUDBaseNode.Create;
  Node.Typ:=aTyp;
  Node.NodeText:=ANodeText;
  AVLNode:=ChildNodes.Find(Node);
  Node.Free;
  if AVLNode<>nil then begin
    Result:=TUDNode(AVLNode.Data);
  end else if CreateIfNotExists then begin
    Result:=TUDNode.Create;
    Result.Typ:=aTyp;
    Result.NodeText:=ANodeText;
    ChildNodes.Add(Result);
    Result.Parent:=Self;
  end else
    Result:=nil;
end;

function TUDNode.Count: integer;
begin
  Result:=ChildNodes.Count;
end;

{ TUnitDependenciesWindow }

procedure TUnitDependenciesWindow.FormCreate(Sender: TObject);
begin
  FUsesGraph:=CodeToolBoss.CreateUsesGraph;
  FGroups:=TUGGroups.Create(FUsesGraph);
  ProgressBar1.Style:=pbstMarquee;
  AddStartAndTargetUnits;

  fImgIndexProject   := IDEImages.LoadImage(16, 'item_project');
  fImgIndexUnit      := IDEImages.LoadImage(16, 'item_unit');
  fImgIndexPackage   := IDEImages.LoadImage(16, 'pkg_required');
  fImgIndexDirectory := IDEImages.LoadImage(16, 'pkg_files');
  AllUnitsTreeView.Images:=IDEImages.Images_16;

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

procedure TUnitDependenciesWindow.AllUnitsShowDirsSpeedButtonClick(
  Sender: TObject);
begin
  Include(FFlags,udwNeedUpdateAllUnitsTreeView);
  IdleConnected:=true;
end;

procedure TUnitDependenciesWindow.AllUnitsShowGroupNodesSpeedButtonClick(
  Sender: TObject);
begin
  Include(FFlags,udwNeedUpdateAllUnitsTreeView);
  IdleConnected:=true;
end;

procedure TUnitDependenciesWindow.AllUnitsFilterEditChange(Sender: TObject);
begin
  Include(FFlags,udwNeedUpdateAllUnitsTreeView);
  IdleConnected:=true;
end;

procedure TUnitDependenciesWindow.FormDestroy(Sender: TObject);
begin
  IdleConnected:=false;
  GroupsLvlGraph.Clear;
  UnitsLvlGraph.Clear;
  FreeAndNil(FGroups);
  FreeAndNil(FAllUnitsRootUDNode);
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
  if udwParsing in FFlags then begin
    UsesGraph.Parse(true,Completed,200);
    if Completed then begin
      Exclude(FFlags,udwParsing);
      CreateGroups;
      ProgressBar1.Visible:=false;
      ProgressBar1.Style:=pbstNormal;
      Timer1.Enabled:=false;
      UpdateAll;
    end;
  end else if udwNeedUpdateGroupsLvlGraph in FFlags then
    UpdateGroupsLvlGraph
  else if udwNeedUpdateUnitsLvlGraph in FFlags then
    UpdateUnitsLvlGraph
  else if udwNeedUpdateAllUnitsTreeView in FFlags then
    UpdateAllUnitsTreeView
  else
    IdleConnected:=false;
end;

procedure TUnitDependenciesWindow.SearchPkgsCheckBoxChange(Sender: TObject);
begin
  // ToDo: reparse
  IdleConnected:=true;
end;

procedure TUnitDependenciesWindow.SearchSrcEditCheckBoxChange(Sender: TObject);
begin
  // ToDo: reparse
  IdleConnected:=true;
end;

procedure TUnitDependenciesWindow.Timer1Timer(Sender: TObject);
begin

end;

procedure TUnitDependenciesWindow.SearchCustomFilesBrowseButtonClick(Sender: TObject);
var
  Dlg: TSelectDirectoryDialog;
  s: TCaption;
  aFilename: String;
  p: Integer;
begin
  Dlg:=TSelectDirectoryDialog.Create(nil);
  try
    InitIDEFileDialog(Dlg);
    Dlg.Options:=Dlg.Options+[ofPathMustExist];
    if not Dlg.Execute then exit;
    aFilename:=TrimFilename(Dlg.FileName);
    s:=SearchCustomFilesComboBox.Text;
    p:=1;
    if FindNextDelimitedItem(s,';',p,aFilename)<>'' then exit;
    if s<>'' then s+=';';
    s+=aFilename;
    SearchCustomFilesComboBox.Text:=s;
    // ToDo: Reparse
    IdleConnected:=true;
  finally
    Dlg.Free;
  end;
end;

procedure TUnitDependenciesWindow.SearchCustomFilesCheckBoxChange(
  Sender: TObject);
begin
  UpdateUnitsButtons;
  // ToDo: reparse
  IdleConnected:=true;
end;

procedure TUnitDependenciesWindow.SearchCustomFilesComboBoxChange(
  Sender: TObject);
begin
  // ToDo: reparse
  IdleConnected:=true;
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
  Result.BaseDir:=ExtractFilePath(AProject.ProjectInfoFile);
  if not FilenameIsAbsolute(Result.BaseDir) then
    Result.BaseDir:='';
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
  Result.BaseDir:=APackage.DirectoryExpanded;
  if not FilenameIsAbsolute(Result.BaseDir) then
    Result.BaseDir:='';
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
  BaseDir: String;
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
    BaseDir:=ExtractFilePath(CurUnit.Filename);
    Directory:=copy(BaseDir,length(FPCSrcDir)+1,length(BaseDir));
    Directory:=ExtractFilePathStart(Directory,2);
    if LeftStr(Directory,length('rtl'))='rtl' then
      Directory:='RTL'
    else if LeftStr(Directory,length('packages'))='packages' then
      System.Delete(Directory,1,length('packages'+PathDelim));
    Grp:=Groups.GetGroup(GroupPrefixFPCSrc+Directory,true);
    if Grp.BaseDir='' then
      Grp.BaseDir:=BaseDir;
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
    if CurUnit.Group=nil then begin
      Filename:=CurUnit.Filename;
      //debugln(['TUnitDependenciesDialog.GuessGroupOfUnits no group for ',Filename]);
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
            //debugln(['TUnitDependenciesDialog.GuessGroupOfUnits ',Group.Name]);
            break;
          end else if TObject(Owners[i]) is TIDEPackage then begin
            Group:=Groups.GetGroup(TIDEPackage(Owners[i]).Name,true);
            //debugln(['TUnitDependenciesDialog.GuessGroupOfUnits ',Group.Name]);
            break;
          end;
        end;
      end;
      if Group=nil then begin
        Group:=Groups.GetGroup(GroupNone,true);
        //debugln(['TUnitDependenciesDialog.GuessGroupOfUnits ',Group.Name]);
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

function TUnitDependenciesWindow.CreateAllUnitsTree: TUDNode;
var
  Node: TUDNode;
  ParentNode: TUDNode;
  GroupName: String;
  ShowDirectories: Boolean;
  ShowGroups: Boolean;
  NodeText: String;
  RootNode: TUDNode;
  Filter: String;
  UGUnit: TUGGroupUnit;
  AVLNode: TAVLTreeNode;
  Group: TUGGroup;
  GroupNode: TUDNode;
  Filename: String;
  p: Integer;
  Dir: String;
  DirNode: TUDNode;
  BaseDir: String;
  CurDir: String;
begin
  Filter:=UTF8LowerCase(GetAllUnitsFilter);
  ShowGroups:=AllUnitsShowGroupNodesSpeedButton.Down;
  ShowDirectories:=AllUnitsShowDirsSpeedButton.Down;
  RootNode:=TUDNode.Create;
  for AVLNode in UsesGraph.FilesTree do begin
    UGUnit:=TUGGroupUnit(AVLNode.Data);
    Filename:=UGUnit.Filename;
    NodeText:=ExtractFileName(Filename);
    if (Filter<>'') and (Pos(Filter, UTF8LowerCase(NodeText))<1) then
      continue;
    Group:=UGUnit.Group;
    BaseDir:='';
    if Group=nil then begin
      GroupName:=GroupNone
    end else begin
      GroupName:=Group.Name;
      if FilenameIsAbsolute(Group.BaseDir) then
        BaseDir:=ChompPathDelim(Group.BaseDir);
    end;
    ParentNode:=RootNode;
    if ShowGroups then begin
      // create group nodes
      GroupNode:=ParentNode.GetNode(udnGroup,GroupName,true);
      if GroupNode.Identifier='' then begin
        GroupNode.Identifier:=GroupName;
        GroupNode.Group:=GroupName;
      end;
      ParentNode:=GroupNode;
      if FilenameIsAbsolute(BaseDir) and FilenameIsAbsolute(Filename) then
        Filename:=CreateRelativePath(Filename,BaseDir);
    end;
    if ShowDirectories then begin
      // create directory nodes
      CurDir:=BaseDir;
      p:=1;
      repeat
        Dir:=FindNextDirectoryInFilename(Filename,p);
        if p>length(Filename) then break;
        if Dir<>'' then begin
          DirNode:=ParentNode.GetNode(udnDirectory,Dir,true);
          CurDir+=PathDelim+Dir;
          if DirNode.Identifier='' then begin
            DirNode.Identifier:=CurDir;
          end;
          ParentNode:=DirNode;
        end;
      until false;
    end;
    Node:=ParentNode.GetNode(udnUnit, NodeText, true);
    Node.Identifier:=UGUnit.Filename;
    Node.Group:=GroupName;
  end;
  Result:=RootNode;
end;

procedure TUnitDependenciesWindow.AddStartAndTargetUnits;
var
  aProject: TLazProject;
  i: Integer;
  SrcEdit: TSourceEditorInterface;
  AFilename: String;
  Pkg: TIDEPackage;
  j: Integer;
  PkgFile: TLazPackageFile;
begin
  Include(FFlags,udwParsing);
  UsesGraph.TargetAll:=true;

  // project lpr
  aProject:=LazarusIDE.ActiveProject;
  if (aProject<>nil) and (aProject.MainFile<>nil) then
    UsesGraph.AddStartUnit(aProject.MainFile.Filename);

  // add all open packages
  if SearchPkgsCheckBox.Checked then begin
    for i:=0 to PackageEditingInterface.GetPackageCount-1 do begin
      Pkg:=PackageEditingInterface.GetPackages(i);
      if not FilenameIsAbsolute(Pkg.Filename) then continue;
      for j:=0 to Pkg.FileCount-1 do begin
        PkgFile:=Pkg.Files[j];
        if PkgFile.Removed then continue;
        aFilename:=PkgFile.GetFullFilename;
        if FilenameIsPascalUnit(AFilename) then
          UsesGraph.AddStartUnit(AFilename);
      end;
    end;
  end;

  // add all source editor files
  if SearchSrcEditCheckBox.Checked then begin
    for i:=0 to SourceEditorManagerIntf.SourceEditorCount-1 do begin
      SrcEdit:=SourceEditorManagerIntf.SourceEditors[i];
      AFilename:=SrcEdit.FileName;
      if FilenameIsPascalUnit(AFilename) then
        UsesGraph.AddStartUnit(AFilename);
    end;
  end;

  // additional units and directories
  if SearchCustomFilesCheckBox.Checked then
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
  List:=SearchCustomFilesComboBox.Text;
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
  SearchCustomFilesCheckBox.Caption:='Additional directories:';
  SearchCustomFilesCheckBox.Hint:='By default only the project units and the source editor units are searched. Add here a list of directories separated by semicolon to search as well.';
  SearchCustomFilesComboBox.Text:='';
  SearchCustomFilesBrowseButton.Caption:='Browse';

  SearchPkgsCheckBox.Caption:='All package units';
  SearchSrcEditCheckBox.Caption:='All source editor units';

  // view all units
  AllUnitsGroupBox.Caption:='All units';

  AllUnitsFilterEdit.Text:='(Filter)';
  AllUnitsMultiselectSpeedButton.Hint:='Allow to select multiple units';
  AllUnitsShowDirsSpeedButton.Hint:='Show nodes for directories';
  AllUnitsShowDirsSpeedButton.LoadGlyphFromLazarusResource('pkg_hierarchical');
  AllUnitsShowGroupNodesSpeedButton.Hint:='Show nodes for project and packages';
  AllUnitsShowGroupNodesSpeedButton.LoadGlyphFromLazarusResource('pkg_hierarchical');

  AllUnitsSearchEdit.Text:='(Search)';
  AllUnitsSearchNextSpeedButton.Hint:='Search next occurence of this phrase';
  AllUnitsSearchNextSpeedButton.LoadGlyphFromLazarusResource('arrow_down');
  AllUnitsSearchPrevSpeedButton.Hint:='Search previous occurence of this phrase';
  AllUnitsSearchPrevSpeedButton.LoadGlyphFromLazarusResource('arrow_up');

  // selected units
  SelectedUnitsGroupBox.Caption:='Selected units';
  SelUnitsSearchEdit.Text:='(Search)';
  SelUnitsSearchNextSpeedButton.Hint:='Search next unit of this phrase';
  SelUnitsSearchNextSpeedButton.LoadGlyphFromLazarusResource('arrow_down');
  SelUnitsSearchPrevSpeedButton.Hint:='Search previous unit of this phrase';
  SelUnitsSearchPrevSpeedButton.LoadGlyphFromLazarusResource('arrow_up');

  UpdateUnitsButtons;
end;

procedure TUnitDependenciesWindow.UpdateUnitsButtons;
begin
  SearchCustomFilesComboBox.Enabled:=SearchCustomFilesCheckBox.Checked;
  SearchCustomFilesBrowseButton.Enabled:=SearchCustomFilesCheckBox.Checked;
end;

procedure TUnitDependenciesWindow.UpdateAll;
begin
  UpdateGroupsLvlGraph;
  UpdateUnitsLvlGraph;
  UpdateAllUnitsTreeView;
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
  Exclude(FFlags,udwNeedUpdateGroupsLvlGraph);
  GroupsLvlGraph.BeginUpdate;
  Graph:=GroupsLvlGraph.Graph;
  Graph.Clear;
  AVLNode:=Groups.Groups.FindLowest;
  while AVLNode<>nil do begin
    Group:=TUGGroup(AVLNode.Data);
    AVLNode:=Groups.Groups.FindSuccessor(AVLNode);
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
  Exclude(FFlags,udwNeedUpdateUnitsLvlGraph);
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

procedure TUnitDependenciesWindow.UpdateAllUnitsTreeView;

  procedure CreateTVNodes(TV: TTreeView; ParentTVNode: TTreeNode;
    ParentUDNode: TUDNode);
  var
    AVLNode: TAVLTreeNode;
    UDNode: TUDNode;
    TVNode: TTreeNode;
  begin
    if ParentUDNode=nil then exit;
    AVLNode:=ParentUDNode.ChildNodes.FindLowest;
    while AVLNode<>nil do begin
      UDNode:=TUDNode(AVLNode.Data);
      TVNode:=TV.Items.AddChild(ParentTVNode,UDNode.NodeText);
      TVNode.Data:=UDNode;
      TVNode.ImageIndex:=GetImgIndex(UDNode);
      TVNode.StateIndex:=TVNode.ImageIndex;
      CreateTVNodes(TV,TVNode,UDNode);
      TVNode.Expanded:=true;
      AVLNode:=ParentUDNode.ChildNodes.FindSuccessor(AVLNode);
    end;
  end;

var
  TV: TTreeView;
  OldExpanded: TTreeNodeExpandedState;
begin
  Exclude(FFlags,udwNeedUpdateAllUnitsTreeView);
  TV:=AllUnitsTreeView;
  TV.BeginUpdate;
  // save old expanded state
  if TV.Items.Count>1 then
    OldExpanded:=TTreeNodeExpandedState.Create(TV)
  else
    OldExpanded:=nil;
  // clear
  FreeAndNil(FAllUnitsRootUDNode);
  TV.Items.Clear;
  // create nodes
  FAllUnitsRootUDNode:=CreateAllUnitsTree;
  CreateTVNodes(TV,nil,FAllUnitsRootUDNode);
  // restore old expanded state
  if OldExpanded<>nil then begin
    OldExpanded.Apply(TV);
    OldExpanded.Free;
  end;
  TV.EndUpdate;
end;

function TUnitDependenciesWindow.GetImgIndex(Node: TUDNode): integer;
begin
  case Node.Typ of
  //udnNone: ;
  udnGroup:
    if Node.Group=GroupPrefixProject then
      Result:=fImgIndexProject
    else
      Result:=fImgIndexPackage;
  udnDirectory: Result:=fImgIndexDirectory;
  //udnInterface: ;
  //udnImplementation: ;
  //udnUsedByInterface: ;
  //udnUsedByImplementation: ;
  udnUnit: Result:=fImgIndexUnit;
  else
    Result:=fImgIndexDirectory;
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

function TUnitDependenciesWindow.GetAllUnitsFilter: string;
begin
  Result:=AllUnitsFilterEdit.Text;
  if Result='(Filter)' then
    Result:='';
end;

{$R *.lfm}

end.

