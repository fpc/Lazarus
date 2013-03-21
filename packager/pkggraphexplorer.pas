{  $Id$  }
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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
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
  Classes, SysUtils, Math, LCLProc, Forms, Controls, Buttons, ComCtrls,
  StdCtrls, Menus, Dialogs, Graphics, FileCtrl, LCLType, ExtCtrls,
  AVL_Tree,
  IDECommands, PackageIntf, IDEImagesIntf,
  LvlGraphCtrl,
  LazConf, LazarusIDEStrConsts, IDEProcs, IDEOptionDefs, EnvironmentOpts,
  Project, PackageDefs, PackageSystem, PackageEditor;
  
const
  GroupPrefixProject = '-Project-';
  GroupPrefixIDE = '-IDE-';
type
  TOnOpenProject =
    function(Sender: TObject; AProject: TProject): TModalResult of object;

  { TPkgGraphExplorerDlg }

  TPkgGraphExplorerDlg = class(TForm)
    InfoMemo: TMemo;
    LvlGraphControl1: TLvlGraphControl;
    PkgPopupMenu: TPopupMenu;
    VerticalSplitter: TSplitter;
    UninstallMenuItem: TMenuItem;
    procedure LvlGraphControl1DblClick(Sender: TObject);
    procedure LvlGraphControl1SelectionChanged(Sender: TObject);
    procedure PkgGraphExplorerShow(Sender: TObject);
    procedure PkgPopupMenuPopup(Sender: TObject);
    procedure InfoMemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure UninstallMenuItemClick(Sender: TObject);
  private
    FOnOpenProject: TOnOpenProject;
    FOnUninstallPackage: TOnUninstallPackage;
    ImgIndexProject: integer;
    ImgIndexPackage: integer;
    ImgIndexInstallPackage: integer;
    ImgIndexInstalledPackage: integer;
    ImgIndexUninstallPackage: integer;
    ImgIndexCyclePackage: integer;
    ImgIndexMissingPackage: integer;
    FOnOpenPackage: TOnOpenPackage;
    fSortedPackages: TAVLTree;
    FChangedDuringLock: boolean;
    FUpdateLock: integer;
    procedure OpenDependencyOwner(DependencyOwner: TObject);
    procedure SetupComponents;
    function GetPackageImageIndex(Pkg: TLazPackage): integer;
    function GetSelectedPackage: TLazPackage;
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdateLocked: boolean;
    procedure UpdateAll;
    procedure UpdateLvlGraph;
    procedure UpdateInfo;
    procedure UpdatePackageName(Pkg: TLazPackage; const OldName: string);
    procedure UpdatePackageID(Pkg: TLazPackage);
    procedure UpdatePackageAdded(Pkg: TLazPackage);
    procedure SelectPackage(Pkg: TLazPackage);
    function FindViewNodeWithText(const s: string): TLvlGraphNode;
    procedure ShowPath(PathList: TFPList);
  public
    property OnOpenPackage: TOnOpenPackage read FOnOpenPackage write FOnOpenPackage;
    property OnOpenProject: TOnOpenProject read FOnOpenProject write FOnOpenProject;
    property OnUninstallPackage: TOnUninstallPackage read FOnUninstallPackage
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
begin
  Pkg:=GetSelectedPackage;
  if Pkg<>nil then begin
    if Assigned(OnOpenPackage) then
      OnOpenPackage(Self,Pkg);
  end;
end;

procedure TPkgGraphExplorerDlg.LvlGraphControl1SelectionChanged(Sender: TObject
  );
begin
  UpdateInfo;
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
end;

procedure TPkgGraphExplorerDlg.InfoMemoKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Parent=nil) then
    Close;
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
  ImgIndexProject          := IDEImages.LoadImage(16, 'item_project');
  ImgIndexPackage          := IDEImages.LoadImage(16, 'item_package');
  ImgIndexInstalledPackage := IDEImages.LoadImage(16, 'pkg_installed');
  ImgIndexInstallPackage   := IDEImages.LoadImage(16, 'pkg_package_autoinstall');
  ImgIndexUninstallPackage := IDEImages.LoadImage(16, 'pkg_package_uninstall');
  ImgIndexCyclePackage     := IDEImages.LoadImage(16, 'pkg_package_circle');
  ImgIndexMissingPackage   := IDEImages.LoadImage(16, 'pkg_conflict');

  LvlGraphControl1.Images:=IDEImages.Images_16;
  LvlGraphControl1.NodeStyle.DefaultImageIndex:=ImgIndexPackage;
  LvlGraphControl1.Caption:='';
end;

function TPkgGraphExplorerDlg.GetPackageImageIndex(Pkg: TLazPackage): integer;
begin
  if Pkg.Installed<>pitNope then begin
    if Pkg.AutoInstall<>pitNope then begin
      Result:=ImgIndexInstalledPackage;
    end else begin
      Result:=ImgIndexInstallPackage;
    end;
  end else begin
    if Pkg.AutoInstall<>pitNope then begin
      Result:=ImgIndexUninstallPackage;
    end else begin
      Result:=ImgIndexPackage;
    end;
  end;
end;

function TPkgGraphExplorerDlg.GetSelectedPackage: TLazPackage;
var
  Node: TLvlGraphNode;
  NodePackageID: TLazPackageID;
  NodeText: String;
begin
  Result:=nil;
  Node:=LvlGraphControl1.Graph.FirstSelected;
  if Node=nil then exit;
  NodePackageID:=TLazPackageID.Create;
  try
    NodeText:=Node.Caption;
    if NodePackageID.StringToID(NodeText) then
      Result:=PackageGraph.FindPackageWithID(NodePackageID);
  finally
    NodePackageID.Free;
  end;
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
  fSortedPackages:=TAVLTree.Create(@CompareLazPackageID);
  Name:=NonModalIDEWindowNames[nmiwPkgGraphExplorer];
  Caption:=lisMenuPackageGraph;

  SetupComponents;
end;

destructor TPkgGraphExplorerDlg.Destroy;
begin
  FreeAndNil(fSortedPackages);
  inherited Destroy;
end;

procedure TPkgGraphExplorerDlg.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TPkgGraphExplorerDlg.EndUpdate;
begin
  if FUpdateLock<=0 then RaiseException('TPkgGraphExplorerDlg.EndUpdate');
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
  UpdateLvlGraph;
  UpdateInfo;
end;

procedure TPkgGraphExplorerDlg.UpdateLvlGraph;

  procedure AddEdges(LGNode: TLvlGraphNode; Dependency: TPkgDependency);
  begin
    while Dependency<>nil do begin
      if Dependency.RequiredPackage<>nil then
        LvlGraphControl1.Graph.GetEdge(LGNode,
          LvlGraphControl1.Graph.GetNode(Dependency.RequiredPackage.IDAsString,true),true);
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;

var
  AVLNode: TAVLTreeNode;
  CurPkg: TLazPackage;
  Cnt: Integer;
  i: Integer;
  PkgName: String;
  ViewNode: TLvlGraphNode;
  OldSelected: String;
  ProjectNode: TLvlGraphNode;
  IDENode: TLvlGraphNode;
begin
  LvlGraphControl1.BeginUpdate;

  // rebuild internal sorted packages
  fSortedPackages.Clear;
  Cnt:=PackageGraph.Count;
  for i:=0 to Cnt-1 do
    fSortedPackages.Add(PackageGraph[i]);

  // save old selection
  OldSelected:='';
  ViewNode:=LvlGraphControl1.Graph.FirstSelected;
  if ViewNode<>nil then
    OldSelected:=ViewNode.Caption;

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
    PkgName:=CurPkg.IDAsString;
    ViewNode:=LvlGraphControl1.Graph.GetNode(PkgName,true);
    ViewNode.ImageIndex:=GetPackageImageIndex(CurPkg);
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
    ViewNode:=LvlGraphControl1.Graph.GetNode(CurPkg.IDAsString,true);
    AddEdges(ViewNode,CurPkg.FirstRequiredDependency);
    AVLNode:=fSortedPackages.FindSuccessor(AVLNode);
  end;

  LvlGraphControl1.SelectedNode:=LvlGraphControl1.Graph.GetNode(OldSelected,false);
  LvlGraphControl1.EndUpdate;
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
    if Pkg.AutoCreated then
      AddState(lisPckExplAutoCreated);
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
  LvlGraphControl1.SelectedNode:=FindViewNodeWithText(Pkg.IDAsString);
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

function TPkgGraphExplorerDlg.FindViewNodeWithText(const s: string
  ): TLvlGraphNode;
begin
  Result:=LvlGraphControl1.Graph.GetNode(s,false);
end;

procedure TPkgGraphExplorerDlg.ShowPath(PathList: TFPList);
// PathList is a list of TLazPackage and TPkgDependency
var
  AnObject: TObject;
begin
  LvlGraphControl1.BeginUpdate;
  if (PathList.Count>0) then begin
    AnObject:=TObject(PathList[PathList.Count-1]);
    if AnObject is TLazPackage then
      SelectPackage(TLazPackage(AnObject));
  end;
  LvlGraphControl1.EndUpdate;
end;

end.

