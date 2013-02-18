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
    IDE Window showing dependecies of units and packages.
}
unit CodyUnitDepWnd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, AVL_Tree, FPCanvas,
  FileUtil, lazutf8classes, LazLogger,
  TreeFilterEdit, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, LCLType,
  LazIDEIntf, ProjectIntf, IDEWindowIntf, PackageIntf,
  CTUnitGraph, CodeToolManager, DefineTemplates, CTUnitGroupGraph,
  CodyCtrls;

const  // ToDo: make resourcestring
  lisSelectAUnit = 'Select an unit';
  lisClose = 'Close';
const
  GroupPrefixProject = '-Project-';
  GroupPrefixFPCSrc = 'FPC:';
  GroupNone = '-None-';
type
  TUDDUsesType = (
    uddutInterfaceUses,
    uddutImplementationUses,
    uddutUsedByInterface,
    uddutUsedByImplementation
    );
  TUDDUsesTypes = set of TUDDUsesType;

  { TUnitDependenciesDialog }

  TUnitDependenciesDialog = class(TForm)
    BtnPanel: TPanel;
    CloseBitBtn: TBitBtn;
    CurUnitPanel: TPanel;
    CurUnitSplitter: TSplitter;
    CurUnitTreeView: TTreeView;
    MainPageControl: TPageControl;
    ProgressBar1: TProgressBar;
    GroupsTabSheet: TTabSheet;
    UnitsTabSheet: TTabSheet;
    Timer1: TTimer;
    CurUnitTreeFilterEdit: TTreeFilterEdit;
    procedure CloseBitBtnClick(Sender: TObject);
    procedure CurUnitTreeViewSelectionChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    FCurrentUnit: TUGUnit;
    FIdleConnected: boolean;
    FUsesGraph: TUsesGraph;
    FGroups: TUGGroups;
    fCircleCategories: array[TUDDUsesType] of TCircleDiagramCategory;
    procedure SetCurrentUnit(AValue: TUGUnit);
    procedure SetIdleConnected(AValue: boolean);
    procedure CreateGroups;
    function CreateProjectGroup(AProject: TLazProject): TUGGroup;
    function CreatePackageGroup(APackage: TIDEPackage): TUGGroup;
    procedure CreateFPCSrcGroups;
    procedure GuessGroupOfUnits;
    procedure AddStartAndTargetUnits;
    procedure UpdateAll;
    procedure UpdateCurUnitDiagram;
    procedure UpdateCurUnitTreeView;
    procedure UpdateGroupsLvlGraph;
    function NodeTextToUnit(NodeText: string): TUGUnit;
    function UGUnitToNodeText(UGUnit: TUGUnit): string;
    function GetFPCSrcDir: string;
  public
    CurUnitDiagram: TCircleDiagramControl;
    GroupsLvlGraph: TLvlGraphControl;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    property UsesGraph: TUsesGraph read FUsesGraph;
    property Groups: TUGGroups read FGroups;
    property CurrentUnit: TUGUnit read FCurrentUnit write SetCurrentUnit;
  end;

var
  UnitDependenciesDialog: TUnitDependenciesDialog;

procedure ShowUnitDependenciesDialog(Sender: TObject);

function dbgs(t: TUDDUsesType): string; overload;

implementation

procedure ShowUnitDependenciesDialog(Sender: TObject);
var
  Dlg: TUnitDependenciesDialog;
begin
  Dlg:=TUnitDependenciesDialog.Create(nil);
  try
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

function dbgs(t: TUDDUsesType): string;
begin
  Result:=GetEnumName(typeinfo(TUDDUsesType),ord(t));
end;

{ TUnitDependenciesDialog }

procedure TUnitDependenciesDialog.CloseBitBtnClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TUnitDependenciesDialog.CurUnitTreeViewSelectionChanged(
  Sender: TObject);
var
  CurUnit: TUGUnit;
begin
  if CurUnitTreeView.Selected=nil then exit;
  CurUnit:=NodeTextToUnit(CurUnitTreeView.Selected.Text);
  if CurUnit=nil then exit;
  CurrentUnit:=CurUnit;
end;

procedure TUnitDependenciesDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TUnitDependenciesDialog.FormCreate(Sender: TObject);
begin
  FUsesGraph:=CodeToolBoss.CreateUsesGraph;
  FGroups:=TUGGroups.Create(FUsesGraph);
  ProgressBar1.Style:=pbstMarquee;
  AddStartAndTargetUnits;

  Caption:='Unit Dependencies';
  CloseBitBtn.Caption:=lisClose;

  IDEDialogLayoutList.ApplyLayout(Self,600,400);

  UnitsTabSheet.Caption:='Units';
  GroupsTabSheet.Caption:='Projects and packages';

  CurUnitDiagram:=TCircleDiagramControl.Create(Self);
  with CurUnitDiagram do begin
    Name:='CurUnitDiagram';
    Align:=alClient;
    FirstCategoryDegree16:=90*16;
    fCircleCategories[uddutUsedByInterface]:=AddCategory('Used by interfaces');
    fCircleCategories[uddutUsedByInterface].Color:=clOlive;
    fCircleCategories[uddutUsedByImplementation]:=AddCategory('Used by implementations');
    fCircleCategories[uddutUsedByImplementation].Color:=clMaroon;
    fCircleCategories[uddutImplementationUses]:=AddCategory('Implementation');
    fCircleCategories[uddutImplementationUses].Color:=clRed;
    fCircleCategories[uddutInterfaceUses]:=AddCategory('Interface');
    fCircleCategories[uddutInterfaceUses].Color:=clGreen;
    CenterCaption:=lisSelectAUnit;
    Parent:=UnitsTabSheet;
  end;

  GroupsLvlGraph:=TLvlGraphControl.Create(Self);
  with GroupsLvlGraph do
  begin
    Name:='GroupsLvlGraph';
    Caption:='';
    Align:=alClient;
    Parent:=GroupsTabSheet;
  end;

  MainPageControl.ActivePage:=GroupsTabSheet;

  IdleConnected:=true;
end;

procedure TUnitDependenciesDialog.FormDestroy(Sender: TObject);
begin
  IdleConnected:=false;
  FreeAndNil(FGroups);
  FreeAndNil(FUsesGraph);
end;

procedure TUnitDependenciesDialog.OnIdle(Sender: TObject; var Done: Boolean);
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

procedure TUnitDependenciesDialog.Timer1Timer(Sender: TObject);
begin
  UpdateAll;
end;

procedure TUnitDependenciesDialog.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TUnitDependenciesDialog.CreateGroups;
var
  i: Integer;
begin
  CreateProjectGroup(LazarusIDE.ActiveProject);
  for i:=0 to PackageEditingInterface.GetPackageCount-1 do
    CreatePackageGroup(PackageEditingInterface.GetPackages(i));
  CreateFPCSrcGroups;
  GuessGroupOfUnits;
end;

function TUnitDependenciesDialog.CreateProjectGroup(AProject: TLazProject
  ): TUGGroup;
var
  i: Integer;
  Filename: String;
  CurUnit: TUGUnit;
begin
  if AProject=nil then exit;
  Result:=Groups.GetGroup(GroupPrefixProject,true);
  //debugln(['TUnitDependenciesDialog.CreateProjectGroup ',Result.Name]);
  for i:=0 to AProject.FileCount-1 do begin
    Filename:=AProject.Files[i].Filename;
    CurUnit:=UsesGraph.GetUnit(Filename,false);
    if CurUnit is TUGGroupUnit then begin
      if TUGGroupUnit(CurUnit).Group<>nil then continue;
      Result.AddUnit(TUGGroupUnit(CurUnit));
    end;
  end;
end;

function TUnitDependenciesDialog.CreatePackageGroup(APackage: TIDEPackage
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

procedure TUnitDependenciesDialog.CreateFPCSrcGroups;
var
  FPCSrcDir: String;
  Node: TAVLTreeNode;
  CurUnit: TUGGroupUnit;
  Directory: String;
  Grp: TUGGroup;
begin
  FPCSrcDir:=AppendPathDelim(GetFPCSrcDir);

  Node:=UsesGraph.FilesTree.FindLowest;
  while Node<>nil do begin
    CurUnit:=TUGGroupUnit(Node.Data);
    if (TUGGroupUnit(CurUnit).Group=nil)
    and (CompareFilenames(FPCSrcDir,LeftStr(CurUnit.Filename,length(FPCSrcDir)))=0)
    then begin
      // a unit in the FPC sources
      Directory:=ExtractFilePath(CurUnit.Filename);
      Directory:=copy(Directory,length(FPCSrcDir)+1,length(Directory));
      Grp:=Groups.GetGroup(GroupPrefixFPCSrc+Directory,true);
      debugln(['TUnitDependenciesDialog.CreateFPCSrcGroups ',Grp.Name]);
      Grp.AddUnit(TUGGroupUnit(CurUnit));
    end;
    Node:=UsesGraph.FilesTree.FindSuccessor(Node);
  end;
end;

procedure TUnitDependenciesDialog.GuessGroupOfUnits;
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

procedure TUnitDependenciesDialog.SetCurrentUnit(AValue: TUGUnit);
begin
  if FCurrentUnit=AValue then Exit;
  FCurrentUnit:=AValue;
  UpdateCurUnitDiagram;
end;

procedure TUnitDependenciesDialog.AddStartAndTargetUnits;
var
  aProject: TLazProject;
begin
  UsesGraph.TargetAll:=true;

  // project lpr
  aProject:=LazarusIDE.ActiveProject;
  if (aProject<>nil) and (aProject.MainFile<>nil) then
    UsesGraph.AddStartUnit(aProject.MainFile.Filename);

  // ToDo: add all open packages

end;

procedure TUnitDependenciesDialog.UpdateAll;
begin
  UpdateCurUnitTreeView;
  UpdateGroupsLvlGraph;
end;

procedure TUnitDependenciesDialog.UpdateCurUnitDiagram;

  procedure UpdateCircleCategory(List: TFPList; t: TUDDUsesType);
  // List is CurrentUnit.UsesUnits or CurrentUnit.UsedByUnits
  var
    i: Integer;
    CurUses: TUGUses;
    Item: TCircleDiagramItem;
    CurUnit: TUGUnit;
    Cnt: Integer;
    s: String;
  begin
    Cnt:=0;
    if List<>nil then begin
      for i:=0 to List.Count-1 do begin
        CurUses:=TUGUses(List[i]);
        if CurUses.InImplementation<>(t in [uddutImplementationUses,uddutUsedByImplementation])
        then continue;
        if t in [uddutInterfaceUses,uddutImplementationUses] then
          CurUnit:=CurUses.Owner
        else
          CurUnit:=CurUses.UsesUnit;
        s:=ExtractFileName(CurUnit.Filename);
        debugln(['UpdateCircleCategory ',s,' ',dbgs(t)]);
        if fCircleCategories[t].Count>Cnt then begin
          Item:=fCircleCategories[t].Items[Cnt];
          Item.Caption:=s
        end else
          Item:=fCircleCategories[t].AddItem(s);
        inc(Cnt);
      end;
    end;
    while fCircleCategories[t].Count>Cnt do
      fCircleCategories[t].Items[Cnt].Free;
  end;

begin
  CurUnitDiagram.BeginUpdate;
  try
    if CurrentUnit<>nil then begin
      debugln(['TUnitDependenciesDialog.UpdateCurUnitDiagram ',CurrentUnit.Filename]);
      CurUnitDiagram.CenterCaption:=ExtractFilename(CurrentUnit.Filename);
      UpdateCircleCategory(CurrentUnit.UsesUnits,uddutInterfaceUses);
      UpdateCircleCategory(CurrentUnit.UsesUnits,uddutImplementationUses);
      UpdateCircleCategory(CurrentUnit.UsedByUnits,uddutUsedByInterface);
      UpdateCircleCategory(CurrentUnit.UsedByUnits,uddutUsedByImplementation);
    end else begin
      CurUnitDiagram.CenterCaption:=lisSelectAUnit;
    end;
  finally
    CurUnitDiagram.EndUpdate;
  end;
end;

procedure TUnitDependenciesDialog.UpdateCurUnitTreeView;
var
  AVLNode: TAVLTreeNode;
  CurUnit: TUGUnit;
  sl: TStringListUTF8;
  i: Integer;
begin
  CurUnitTreeView.BeginUpdate;
  sl:=TStringListUTF8.Create;
  try
    CurUnitTreeView.Items.Clear;

    AVLNode:=UsesGraph.FilesTree.FindLowest;
    while AVLNode<>nil do begin
      CurUnit:=TUGUnit(AVLNode.Data);
      sl.Add(UGUnitToNodeText(CurUnit));
      AVLNode:=UsesGraph.FilesTree.FindSuccessor(AVLNode);
    end;

    sl.CustomSort(@CompareStringListItemsUTF8LowerCase);
    for i:=0 to sl.Count-1 do begin
      CurUnitTreeView.Items.Add(nil,sl[i]);
    end;
  finally
    sl.Free;
    CurUnitTreeView.EndUpdate;
  end;
end;

procedure TUnitDependenciesDialog.UpdateGroupsLvlGraph;
var
  AVLNode: TAVLTreeNode;
  Group: TUGGroup;
  Graph: TLvlGraph;
  PkgList: TFPList;
  i: Integer;
  RequiredPkg: TIDEPackage;
  GroupObj: TObject;
begin
  GroupsLvlGraph.BeginUpdate;
  Graph:=GroupsLvlGraph.Graph;
  Graph.Clear;
  AVLNode:=Groups.Groups.FindLowest;
  while AVLNode<>nil do begin
    Group:=TUGGroup(AVLNode.Data);
    Graph.GetNode(Group.Name,true);
    GroupObj:=nil;
    if Group.Name=GroupPrefixProject then begin
      // project
      GroupObj:=LazarusIDE.ActiveProject;
    end else begin
      // package
      GroupObj:=PackageEditingInterface.FindPackageWithName(Group.Name);
    end;
    PkgList:=nil;
    try
      PackageEditingInterface.GetRequiredPackages(GroupObj,PkgList,[pirNotRecursive]);
      if (PkgList<>nil) then begin
        // add for each dependency an edge in the Graph
        for i:=0 to PkgList.Count-1 do begin
          RequiredPkg:=TIDEPackage(PkgList[i]);
          Graph.GetEdge(Group.Name,RequiredPkg.Name,true);
        end;
      end;
    finally
      PkgList.Free;
    end;
    AVLNode:=Groups.Groups.FindSuccessor(AVLNode);
  end;
  GroupsLvlGraph.EndUpdate;
end;

function TUnitDependenciesDialog.NodeTextToUnit(NodeText: string): TUGUnit;
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

function TUnitDependenciesDialog.UGUnitToNodeText(UGUnit: TUGUnit): string;
begin
  Result:=ExtractFileName(UGUnit.Filename);
end;

function TUnitDependenciesDialog.GetFPCSrcDir: string;
var
  UnitSet: TFPCUnitSetCache;
begin
  UnitSet:=CodeToolBoss.GetUnitSetForDirectory('');
  Result:=UnitSet.FPCSourceDirectory;
end;

{$R *.lfm}

end.

