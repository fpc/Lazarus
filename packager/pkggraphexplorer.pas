{
 /***************************************************************************
                            pkggraphexplorer.pas
                            --------------------


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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    TPkgGraphExplorerDlg is the IDE window showing the whole package graph.
}
unit PkgGraphExplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, Laz_AVL_Tree,
  // LCL
  LCLType, LCLIntf, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  Menus, ButtonPanel,
  // LazUtils
  LazLoggerBase, LazTracer, LazStringUtils,
  // LazControls
  LvlGraphCtrl,
  // IdeIntf
  IDECommands, PackageIntf, IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts, IDEOptionDefs, Project,
  PackageDefs, PackageSystem, PackageEditor, CleanPkgDeps;
  
const
  GroupPrefixProject = '-Project-';
  GroupPrefixIDE = '-IDE-';
type
  TOnOpenProject =
    function(Sender: TObject; AProject: TProject): TModalResult of object;

  { TPkgGraphExplorerDlg }

  TPkgGraphExplorerDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    CleanPkgDepsMenuItem: TMenuItem;
    PkgTreeView: TTreeView;
    InfoMemo: TMemo;
    LvlGraphControl1: TLvlGraphControl;
    Panel1: TPanel;
    PkgPopupMenu: TPopupMenu;
    Splitter1: TSplitter;
    VerticalSplitter: TSplitter;
    UninstallMenuItem: TMenuItem;
    procedure CleanPkgDepsMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure LvlGraphControl1DblClick(Sender: TObject);
    procedure LvlGraphControl1SelectionChanged(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PkgGraphExplorerShow(Sender: TObject);
    procedure PkgPopupMenuPopup(Sender: TObject);
    procedure InfoMemoKeyDown(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    procedure PkgTreeViewDblClick(Sender: TObject);
    procedure PkgTreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure PkgTreeViewSelectionChanged(Sender: TObject);
    procedure UninstallMenuItemClick(Sender: TObject);
  private
    FOnOpenProject: TOnOpenProject;
    FOnUninstallPackage: TOnPkgEvent;
    ImgIndexProject: integer;
    ImgIndexPackage: integer;
    ImgIndexInstallPackage: integer;
    ImgIndexInstalledPackage: integer;
    ImgIndexUninstallPackage: integer;
    ImgIndexCyclePackage: integer;
    ImgIndexMissingPackage: integer;
    FOnOpenPackage: TOnPkgEvent;
    FChangedDuringLock: boolean;
    FUpdateLock: integer;
    FUpdatingSelection: boolean;
    procedure OpenDependencyOwner(DependencyOwner: TObject);
    procedure SetupComponents;
    function GetPackageImageIndex(Pkg: TLazPackage; InstallPkgList: TFPList): integer;
    function GetSelectedPackage: TLazPackage;
    function FindPackage(const NodeText: string): TLazPackage;
    function PackageAsNodeText(Pkg: TLazPackage): string;
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdateLocked: boolean;
    procedure UpdateAll;
    procedure UpdateTree;
    procedure UpdateLvlGraph;
    procedure UpdateInfo;
    procedure UpdatePackageName({%H-}Pkg: TLazPackage; const {%H-}OldName: string);
    procedure UpdatePackageID({%H-}Pkg: TLazPackage);
    procedure UpdatePackageAdded({%H-}Pkg: TLazPackage);
    procedure SelectPackage(Pkg: TLazPackage);
    function FindLvlGraphNodeWithText(const s: string): TLvlGraphNode;
    procedure ShowPath(PathList: TFPList);
  public
    property OnOpenPackage: TOnPkgEvent read FOnOpenPackage write FOnOpenPackage;
    property OnOpenProject: TOnOpenProject read FOnOpenProject write FOnOpenProject;
    property OnUninstallPackage: TOnPkgEvent read FOnUninstallPackage
                                                    write FOnUninstallPackage;
  end;
  
var
  PackageGraphExplorer: TPkgGraphExplorerDlg = nil;

implementation

{$R *.lfm}

{ TPkgGraphExplorerDlg }

procedure TPkgGraphExplorerDlg.PkgGraphExplorerShow(Sender: TObject);
begin
  UpdateAll;
end;

procedure TPkgGraphExplorerDlg.LvlGraphControl1DblClick(Sender: TObject);
var
  Pkg: TLazPackage;
  LGNode: TLvlGraphNode;
begin
  LGNode:=LvlGraphControl1.SelectedNode;
  if LGNode=nil then exit;
  if LGNode.Caption=GroupPrefixProject then begin
    if Assigned(OnOpenProject) and (Project1<>nil) then
      OnOpenProject(Self,Project1);
    exit;
  end;
  if LGNode.Caption=GroupPrefixIDE then begin
    ExecuteIDECommand(Self,ecEditInstallPkgs);
    exit;
  end;
  Pkg:=GetSelectedPackage;
  if Pkg=nil then exit;
  if Assigned(OnOpenPackage) then
    OnOpenPackage(Self,Pkg);
end;

procedure TPkgGraphExplorerDlg.CleanPkgDepsMenuItemClick(Sender: TObject);
var
  Owners: TFPList;
  ListOfNodeInfos: TObjectList;
  i: Integer;
  Info: TCPDNodeInfo;
  Pkg: TLazPackage;
  Dependency: TPkgDependency;
begin
  Owners:=TFPList.Create;
  ListOfNodeInfos:=nil;
  try
    //if (Project1<>nil) then
    //  Owners.Add(Project1);
    for i:=0 to PackageGraph.Count-1 do
      Owners.Add(PackageGraph[i]);

    if ShowCleanPkgDepDlg(Owners,true,ListOfNodeInfos)<>mrOk then exit;
    for i:=0 to ListOfNodeInfos.Count-1 do begin
      Info:=TCPDNodeInfo(ListOfNodeInfos[i]);
      debugln(['TPkgGraphExplorerDlg.CleanPkgDepsMenuItemClick ',Info.Owner,'->',Info.Dependency]);
      if Info.Owner=CPDProjectName then begin

      end else begin
        Pkg:=PackageGraph.FindPackageWithName(Info.Owner,nil);
        debugln(['TPkgGraphExplorerDlg.CleanPkgDepsMenuItemClick Pkg=',Pkg<>nil]);
        if Pkg=nil then continue;
        Dependency:=Pkg.FindDependencyByName(Info.Dependency);
        debugln(['TPkgGraphExplorerDlg.CleanPkgDepsMenuItemClick Dep=',Dependency<>nil]);
        if Dependency=nil then continue;
        // open package editor
        if Assigned(OnOpenPackage) then
          OnOpenPackage(Self,Pkg);
        // remove dependency
        PackageGraph.RemoveDependencyFromPackage(Pkg,Dependency,true);
      end;
    end;

    UpdateLvlGraph;
  finally
    ListOfNodeInfos.Free;
  end;
end;

procedure TPkgGraphExplorerDlg.FormCreate(Sender: TObject);
begin
  ButtonPanel1.OKButton.Caption:= lisClose;
end;

procedure TPkgGraphExplorerDlg.HelpButtonClick(Sender: TObject);
begin
  OpenUrl('http://wiki.freepascal.org/IDE_Window:_Package_Graph');
end;

procedure TPkgGraphExplorerDlg.LvlGraphControl1SelectionChanged(Sender: TObject
  );
var
  LGNode: TLvlGraphNode;
  TVNode: TTreeNode;
begin
  if FUpdatingSelection then exit;
  LGNode:=LvlGraphControl1.SelectedNode;
  if LGNode<>nil then
  begin
    TVNode:=PkgTreeView.Items.FindNodeWithText(LGNode.Caption);
    FUpdatingSelection:=true;
    PkgTreeView.Selected:=TVNode;
    FUpdatingSelection:=false;
  end;
  UpdateInfo;
end;

procedure TPkgGraphExplorerDlg.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TPkgGraphExplorerDlg.PkgPopupMenuPopup(Sender: TObject);
var
  Pkg: TLazPackage;
begin
  Pkg:=GetSelectedPackage;
  //DebugLn(['TPkgGraphExplorerDlg.PkgPopupMenuPopup ',Pkg<>nil,' ',(Pkg<>nil) and (Pkg.AutoInstall<>pitNope)]);
  UninstallMenuItem.Visible:=(Pkg<>nil) and (Pkg.AutoInstall<>pitNope);
  if UninstallMenuItem.Visible then
    UninstallMenuItem.Caption:=Format(lisPckExplUninstallPackage, [Pkg.IDAsString]);
  CleanPkgDepsMenuItem.Caption:=lisPckShowUnneededDependencies;
end;

procedure TPkgGraphExplorerDlg.InfoMemoKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Parent=nil) then
    Close;
end;

procedure TPkgGraphExplorerDlg.PkgTreeViewDblClick(Sender: TObject);
var
  TVNode: TTreeNode;
  Pkg: TLazPackage;
begin
  TVNode:=PkgTreeView.Selected;
  if TVNode=nil then exit;
  Pkg:=FindPackage(TVNode.Text);
  if Pkg=nil then exit;
  if Assigned(OnOpenPackage) then
    OnOpenPackage(Self,Pkg);
end;

procedure TPkgGraphExplorerDlg.PkgTreeViewExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  TV: TTreeView;
  Pkg: TLazPackage;
  Dependency: TPkgDependency;
  i: Integer;
  CycleNode: TTreeNode;
  SubNode: TTreeNode;
  ImgIndex: Integer;
  NodeText: String;
  ReqPkg: TLazPackage;
  InstallPkgList: TFPList;
begin
  TV:=PkgTreeView;
  Pkg:=FindPackage(Node.Text);
  //debugln(['TPkgGraphExplorerDlg.PkgTreeViewExpanding ',Node.Text,' Pkg=',Pkg<>nil]);
  if (Pkg=nil) or (Pkg.FirstRequiredDependency=nil) then begin
    AllowExpansion:=false;
    Node.HasChildren:=false;
    exit;
  end;
  InstallPkgList:=nil;
  try
    PackageGraph.GetAllRequiredPackages(nil,
      PackageGraph.FirstAutoInstallDependency,InstallPkgList,[]);
    i:=0;
    Dependency:=Pkg.FirstRequiredDependency;
    while Dependency<>nil do begin
      ReqPkg:=Dependency.RequiredPackage;
      if ReqPkg<>nil then
        NodeText:=PackageAsNodeText(ReqPkg)
      else
        NodeText:=Dependency.AsString;
      if Node.Count=i then
        SubNode:=TV.Items.AddChild(Node,NodeText)
      else begin
        SubNode:=Node[i];
        SubNode.Text:=NodeText;
      end;
      if ReqPkg<>nil then begin
        CycleNode:=Node;
        while (CycleNode<>nil) and (CycleNode.Text<>NodeText) do
          CycleNode:=CycleNode.Parent;
        if CycleNode<>nil then
          ImgIndex:=ImgIndexCyclePackage
        else
          ImgIndex:=GetPackageImageIndex(ReqPkg,InstallPkgList);
        SubNode.HasChildren:=ReqPkg.FirstRequiredDependency<>nil;
      end else begin
        ImgIndex:=ImgIndexMissingPackage;
        SubNode.HasChildren:=false;
      end;
      SubNode.ImageIndex:=ImgIndex;
      SubNode.SelectedIndex:=ImgIndex;
      inc(i);
      Dependency:=Dependency.NextRequiresDependency;
    end;
    while Node.Count>i do
      Node[Node.Count-1].Free;
  finally
    InstallPkgList.Free;
  end;
end;

procedure TPkgGraphExplorerDlg.PkgTreeViewSelectionChanged(Sender: TObject);
var
  TVNode: TTreeNode;
  LGNode: TLvlGraphNode;
begin
  if FUpdatingSelection then exit;
  TVNode:=PkgTreeView.Selected;
  if TVNode=nil then exit;
  LGNode:=LvlGraphControl1.Graph.GetNode(TVNode.Text,false);
  FUpdatingSelection:=true;
  LvlGraphControl1.SelectedNode:=LGNode;
  FUpdatingSelection:=false;
  UpdateInfo;
end;

procedure TPkgGraphExplorerDlg.UninstallMenuItemClick(Sender: TObject);
var
  Pkg: TLazPackage;
begin
  Pkg:=GetSelectedPackage;
  if Pkg<>nil then begin
    if Assigned(OnUninstallPackage) then OnUninstallPackage(Self,Pkg);
  end;
end;

procedure TPkgGraphExplorerDlg.SetupComponents;
begin
  ImgIndexProject          := IDEImages.LoadImage('item_project');
  ImgIndexPackage          := IDEImages.LoadImage('item_package');
  ImgIndexInstalledPackage := IDEImages.LoadImage('pkg_installed');
  ImgIndexInstallPackage   := IDEImages.LoadImage('pkg_package_autoinstall');
  ImgIndexUninstallPackage := IDEImages.LoadImage('pkg_package_uninstall');
  ImgIndexCyclePackage     := IDEImages.LoadImage('pkg_package_circle');
  ImgIndexMissingPackage   := IDEImages.LoadImage('pkg_conflict');

  PkgTreeView.Images:=IDEImages.Images_16;

  LvlGraphControl1.Images:=IDEImages.Images_16;
  LvlGraphControl1.NodeStyle.DefaultImageIndex:=ImgIndexPackage;
  LvlGraphControl1.Caption:='';
end;

function TPkgGraphExplorerDlg.GetPackageImageIndex(Pkg: TLazPackage;
  InstallPkgList: TFPList): integer;
var
  WillInstall: Boolean;
begin
  WillInstall:=(Pkg.AutoInstall<>pitNope)
    or ((InstallPkgList<>nil) and (InstallPkgList.IndexOf(Pkg)>=0));
  if Pkg.Installed<>pitNope then begin
    // installed
    if WillInstall then begin
      Result:=ImgIndexInstalledPackage;
    end else begin
      Result:=ImgIndexUninstallPackage;
    end;
  end else begin
    // not installed
    if WillInstall then begin
      Result:=ImgIndexInstallPackage;
    end else begin
      Result:=ImgIndexPackage;
    end;
  end;
end;

function TPkgGraphExplorerDlg.GetSelectedPackage: TLazPackage;
var
  Node: TLvlGraphNode;
begin
  Result:=nil;
  Node:=nil;
  if assigned(LvlGraphControl1) and assigned(LvlGraphControl1.Graph) Then
    Node:=LvlGraphControl1.Graph.FirstSelected;
  if Node=nil then exit;
  Result:=FindPackage(Node.Caption);
end;

function TPkgGraphExplorerDlg.FindPackage(const NodeText: string): TLazPackage;
var
  NodePackageID: TLazPackageID;
begin
  Result:=nil;
  NodePackageID:=TLazPackageID.Create;
  try
    if NodePackageID.StringToID(NodeText) then
      Result:=PackageGraph.FindPackageWithID(NodePackageID);
  finally
    NodePackageID.Free;
  end;
end;

function TPkgGraphExplorerDlg.PackageAsNodeText(Pkg: TLazPackage): string;
begin
  Result:=Pkg.IDAsString;
end;

procedure TPkgGraphExplorerDlg.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  ExecuteIDEShortCut(Self,Key,Shift,nil);
end;

constructor TPkgGraphExplorerDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FUpdateLock:=PackageGraph.UpdateLock;
  Name:=NonModalIDEWindowNames[nmiwPkgGraphExplorer];
  Caption:=lisMenuPackageGraph;

  SetupComponents;
end;

destructor TPkgGraphExplorerDlg.Destroy;
begin
  inherited Destroy;
end;

procedure TPkgGraphExplorerDlg.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TPkgGraphExplorerDlg.EndUpdate;
begin
  if FUpdateLock<=0 then RaiseGDBException('TPkgGraphExplorerDlg.EndUpdate');
  dec(FUpdateLock);
  if FChangedDuringLock then UpdateAll;
end;

function TPkgGraphExplorerDlg.IsUpdateLocked: boolean;
begin
  Result:=FUpdateLock>0;
end;

procedure TPkgGraphExplorerDlg.UpdateAll;
begin
  if IsUpdateLocked then begin
    FChangedDuringLock:=true;
    exit;
  end;
  FChangedDuringLock:=false;
  if not Visible then exit;
  UpdateTree;
  UpdateLvlGraph;
  UpdateInfo;
end;

procedure TPkgGraphExplorerDlg.UpdateTree;
var
  Cnt: Integer;
  i: Integer;
  CurIndex: Integer;
  ViewNode: TTreeNode;
  NextViewNode: TTreeNode;
  HiddenNode: TAVLTreeNode;
  CurPkg: TLazPackage;
  OldExpanded: TTreeNodeExpandedState;
  fSortedPackages: TAVLTree;
  InstallPkgList: TFPList;
begin
  InstallPkgList:=nil;
  fSortedPackages:=TAVLTree.Create(@CompareLazPackageID);
  try
    // get list of packages
    Cnt:=PackageGraph.Count;
    for i:=0 to Cnt-1 do
      fSortedPackages.Add(PackageGraph[i]);
    PackageGraph.GetAllRequiredPackages(nil,
      PackageGraph.FirstAutoInstallDependency,InstallPkgList,[]);
    // rebuild the TreeView
    PkgTreeView.BeginUpdate;
    // save old expanded state
    OldExpanded:=TTreeNodeExpandedState.Create(PkgTreeView);
    // create first level
    CurIndex:=0;
    HiddenNode:=fSortedPackages.FindLowest;
    ViewNode:=PkgTreeView.Items.GetFirstNode;
    while HiddenNode<>nil do begin
      CurPkg:=TLazPackage(HiddenNode.Data);
      if ViewNode=nil then
        ViewNode:=PkgTreeView.Items.Add(nil,PackageAsNodeText(CurPkg))
      else
        ViewNode.Text:=PackageAsNodeText(CurPkg);
      ViewNode.HasChildren:=CurPkg.FirstRequiredDependency<>nil;
      ViewNode.Expanded:=false;
      ViewNode.ImageIndex:=GetPackageImageIndex(CurPkg,InstallPkgList);
      ViewNode.SelectedIndex:=ViewNode.ImageIndex;
      ViewNode:=ViewNode.GetNextSibling;
      HiddenNode:=fSortedPackages.FindSuccessor(HiddenNode);
      inc(CurIndex);
    end;
    while ViewNode<>nil do begin
      NextViewNode:=ViewNode.GetNextSibling;
      ViewNode.Free;
      ViewNode:=NextViewNode;
    end;
    // restore old expanded state
    OldExpanded.Apply(PkgTreeView);
    OldExpanded.Free;
    // completed
  finally
    InstallPkgList.Free;
    fSortedPackages.Free;
    PkgTreeView.EndUpdate;
  end;
end;

procedure TPkgGraphExplorerDlg.UpdateLvlGraph;

  procedure AddEdges(LGNode: TLvlGraphNode; Dependency: TPkgDependency);
  begin
    while Dependency<>nil do begin
      if Dependency.RequiredPackage<>nil then
        LvlGraphControl1.Graph.GetEdge(LGNode,
          LvlGraphControl1.Graph.GetNode(PackageAsNodeText(Dependency.RequiredPackage),true),true);
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;

var
  AVLNode: TAVLTreeNode;
  CurPkg: TLazPackage;
  Cnt: Integer;
  i: Integer;
  ViewNode: TLvlGraphNode;
  OldSelected: String;
  ProjectNode: TLvlGraphNode;
  IDENode: TLvlGraphNode;
  fSortedPackages: TAVLTree;
  InstallPkgList: TFPList;
begin
  InstallPkgList:=nil;
  fSortedPackages:=TAVLTree.Create(@CompareLazPackageID);
  LvlGraphControl1.BeginUpdate;
  try
    // get list of packages
    Cnt:=PackageGraph.Count;
    for i:=0 to Cnt-1 do
      fSortedPackages.Add(PackageGraph[i]);
    PackageGraph.GetAllRequiredPackages(nil,
      PackageGraph.FirstAutoInstallDependency,InstallPkgList,[]);

    // save old selection
    OldSelected:='';
    ViewNode:=LvlGraphControl1.Graph.FirstSelected;
    if ViewNode<>nil then
      OldSelected:=ViewNode.Caption;

    LvlGraphControl1.Graph.Clear;

    // add a node for the project
    ProjectNode:=nil;
    if Project1<>nil then begin
      ProjectNode:=LvlGraphControl1.Graph.GetNode(GroupPrefixProject,true);
      ProjectNode.ImageIndex:=ImgIndexProject;
    end;

    // add a node for the IDE
    IDENode:=LvlGraphControl1.Graph.GetNode(GroupPrefixIDE,true);
    IDENode.ImageIndex:=ImgIndexProject;

    // add nodes for packages
    AVLNode:=fSortedPackages.FindLowest;
    while AVLNode<>nil do begin
      CurPkg:=TLazPackage(AVLNode.Data);
      ViewNode:=LvlGraphControl1.Graph.GetNode(PackageAsNodeText(CurPkg),true);
      ViewNode.ImageIndex:=GetPackageImageIndex(CurPkg,InstallPkgList);
      AVLNode:=fSortedPackages.FindSuccessor(AVLNode);
    end;

    // add project dependencies
    if ProjectNode<>nil then
      AddEdges(ProjectNode,Project1.FirstRequiredDependency);

    // add IDE dependencies
    AddEdges(IDENode,PackageGraph.FirstAutoInstallDependency);

    // add package dependencies
    AVLNode:=fSortedPackages.FindLowest;
    while AVLNode<>nil do begin
      CurPkg:=TLazPackage(AVLNode.Data);
      ViewNode:=LvlGraphControl1.Graph.GetNode(PackageAsNodeText(CurPkg),true);
      AddEdges(ViewNode,CurPkg.FirstRequiredDependency);
      AVLNode:=fSortedPackages.FindSuccessor(AVLNode);
    end;

    LvlGraphControl1.SelectedNode:=LvlGraphControl1.Graph.GetNode(OldSelected,false);
  finally
    InstallPkgList.Free;
    fSortedPackages.Free;
    LvlGraphControl1.EndUpdate;
  end;
end;

procedure TPkgGraphExplorerDlg.UpdateInfo;
var
  Pkg: TLazPackage;
  InfoStr: String;

  procedure AddState(const NewState: string);
  begin
    if (InfoStr<>'') and (InfoStr[length(InfoStr)]<>' ') then
      InfoStr:=InfoStr+', ';
    InfoStr:=InfoStr+NewState;
  end;

begin
  InfoStr:='';
  Pkg:=GetSelectedPackage;
  if Pkg<>nil then begin
    // filename and title
    InfoStr:=Format(lisOIPFilename, [Pkg.Filename]);
    // state
    InfoStr:=Format(lisPckExplState, [InfoStr+LineEnding]);
    if Pkg.Missing then
      AddState(lisLpkIsMissing);
    if Pkg.Installed<>pitNope then
      AddState(lisPckExplInstalled);
    if (Pkg.AutoInstall<>pitNope) and (Pkg.Installed=pitNope) then
      AddState(lisPckExplInstallOnNextStart);
    if (Pkg.AutoInstall=pitNope) and (Pkg.Installed<>pitNope) then
      AddState(lisPckExplUninstallOnNextStart);
    InfoStr:=Format(lisOIPDescriptionDescription, [InfoStr+LineEnding,
      BreakString(Pkg.Description, 60, length(lisOIPDescription))]);
  end;
  InfoMemo.Text:=InfoStr;
end;

procedure TPkgGraphExplorerDlg.UpdatePackageName(Pkg: TLazPackage;
  const OldName: string);
begin
  UpdateAll;
end;

procedure TPkgGraphExplorerDlg.UpdatePackageID(Pkg: TLazPackage);
begin
  UpdateAll;
end;

procedure TPkgGraphExplorerDlg.UpdatePackageAdded(Pkg: TLazPackage);
begin
  UpdateAll;
end;

procedure TPkgGraphExplorerDlg.SelectPackage(Pkg: TLazPackage);
begin
  if Pkg=nil then exit;
  LvlGraphControl1.SelectedNode:=FindLvlGraphNodeWithText(PackageAsNodeText(Pkg));
end;

procedure TPkgGraphExplorerDlg.OpenDependencyOwner(DependencyOwner: TObject);
begin
  if DependencyOwner is TLazPackage then begin
    if Assigned(OnOpenPackage) then
      OnOpenPackage(Self,TLazPackage(DependencyOwner));
  end else if DependencyOwner is TProject then begin
    if Assigned(OnOpenProject) then
      OnOpenProject(Self,TProject(DependencyOwner));
  end;
end;

function TPkgGraphExplorerDlg.FindLvlGraphNodeWithText(const s: string): TLvlGraphNode;
begin
  Result:=LvlGraphControl1.Graph.GetNode(s,false);
end;

procedure TPkgGraphExplorerDlg.ShowPath(PathList: TFPList);
// PathList is a list of TLazPackage and TPkgDependency

  procedure SelectChild(var Node: TTreeNode; const NodeText: string);
  var
    i: Integer;
    CurNode: TTreeNode;
  begin
    if Node=nil then begin
      for i:=0 to PkgTreeView.Items.TopLvlCount-1 do begin
        CurNode:=PkgTreeView.Items.TopLvlItems[i];
        if SysUtils.CompareText(CurNode.Text,NodeText)=0 then begin
          Node:=CurNode;
          exit;
        end;
      end;
    end
    else begin
      Node.Expanded:=true;
      for i:=0 to Node.Count-1 do begin
        if SysUtils.CompareText(Node[i].Text,NodeText)=0 then begin
          Node:=Node[i];
          exit;
        end;
      end;
    end;
    Node:=nil;
  end;

var
  AnObject: TObject;
  i: Integer;
  CurNode, LastNode: TTreeNode;
begin
  LvlGraphControl1.BeginUpdate;
  PkgTreeView.BeginUpdate;

  CurNode:=nil;
  LastNode:=nil;
  for i:=0 to PathList.Count-1 do begin
    AnObject:=TObject(PathList[i]);
    LastNode:=CurNode;
    if AnObject is TLazPackage then begin
      SelectChild(CurNode,TLazPackage(AnObject).IDAsString);
    end else if AnObject is TPkgDependency then begin
      SelectChild(CurNode,TPkgDependency(AnObject).AsString);
    end else
      break;
    if CurNode=nil then break;
  end;
  if CurNode<>nil then Lastnode:=CurNode;
  PkgTreeView.Selected:=LastNode;

  PkgTreeView.EndUpdate;
  LvlGraphControl1.EndUpdate;
end;

end.

