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
  Classes, SysUtils, LCLProc, Forms, Controls, Buttons, ComCtrls,
  StdCtrls, Menus, Dialogs, Graphics, FileCtrl, LCLType,
  AVL_Tree,
  IDECommands, PackageIntf,
  LazConf, LazarusIDEStrConsts, IDEProcs, IDEOptionDefs, EnvironmentOpts,
  Project, PackageDefs, PackageSystem, PackageEditor, ExtCtrls;
  
type
  TOnOpenProject =
    function(Sender: TObject; AProject: TProject): TModalResult of object;

  { TPkgGraphExplorerDlg }

  TPkgGraphExplorerDlg = class(TForm)
    PkgTreeLabel: TLabel;
    PkgTreeView: TTreeView;
    PkgListPanel: TPanel;
    PkgTreePanel: TPanel;
    PkgListLabel: TLabel;
    PkgListBox: TListBox;
    InfoMemo: TMemo;
    PkgPopupMenu: TPopupMenu;
    VerticalSplitter: TSplitter;
    HorizontalSplitter: TSplitter;
    UninstallMenuItem: TMenuItem;
    procedure PkgGraphExplorerShow(Sender: TObject);
    procedure PkgListBoxClick(Sender: TObject);
    procedure PkgListBoxDblClick(Sender: TObject);
    procedure PkgPopupMenuPopup(Sender: TObject);
    procedure PkgTreeViewDblClick(Sender: TObject);
    procedure PkgTreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure InfoMemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PkgTreeViewSelectionChanged(Sender: TObject);
    procedure UninstallMenuItemClick(Sender: TObject);
  private
    FOnOpenProject: TOnOpenProject;
    FOnUninstallPackage: TOnUninstallPackage;
    ImgIndexPackage: integer;
    ImgIndexInstallPackage: integer;
    ImgIndexInstalledPackage: integer;
    ImgIndexUninstallPackage: integer;
    ImgIndexCirclePackage: integer;
    ImgIndexMissingPackage: integer;
    FOnOpenPackage: TOnOpenPackage;
    fSortedPackages: TAVLTree;
    FChangedDuringLock: boolean;
    FUpdateLock: integer;
    procedure SetupComponents;
    function GetPackageImageIndex(Pkg: TLazPackage): integer;
    procedure GetDependency(ANode: TTreeNode; var Pkg: TLazPackage;
      var Dependency: TPkgDependency);
    procedure GetCurrentIsUsedBy(var Dependency: TPkgDependency);
    function SearchParentNodeWithText(ANode: TTreeNode;
      const NodeText: string): TTreeNode;
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
    procedure UpdateList;
    procedure UpdateInfo;
    procedure UpdatePackageName(Pkg: TLazPackage; const OldName: string);
    procedure UpdatePackageID(Pkg: TLazPackage);
    procedure UpdatePackageAdded(Pkg: TLazPackage);
    procedure SelectPackage(Pkg: TLazPackage);
    procedure OpenDependencyOwner(DependencyOwner: TObject);
    function FindMainNodeWithText(const s: string): TTreeNode;
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

uses 
  Math, IDEImagesIntf;

{ TPkgGraphExplorerDlg }
procedure TPkgGraphExplorerDlg.PkgGraphExplorerShow(Sender: TObject);
begin
  UpdateAll;
end;

procedure TPkgGraphExplorerDlg.PkgListBoxClick(Sender: TObject);
var
  Dependency: TPkgDependency;
begin
  GetCurrentIsUsedBy(Dependency);
  if (Dependency=nil) or (Dependency.Owner=nil) then begin
    PkgListBox.ItemIndex:=-1;
    exit;
  end;
  if Dependency.Owner is TLazPackage then
    SelectPackage(TLazPackage(Dependency.Owner));
end;

procedure TPkgGraphExplorerDlg.PkgListBoxDblClick(Sender: TObject);
var
  Dependency: TPkgDependency;
begin
  GetCurrentIsUsedBy(Dependency);
  if (Dependency=nil) or (Dependency.Owner=nil) then begin
    PkgListBox.ItemIndex:=-1;
    exit;
  end;
  if Dependency.Owner<>nil then
    OpenDependencyOwner(Dependency.Owner);
end;

procedure TPkgGraphExplorerDlg.PkgPopupMenuPopup(Sender: TObject);
var
  Pkg: TLazPackage;
  Dependency: TPkgDependency;
begin
  GetDependency(PkgTreeView.Selected,Pkg,Dependency);
  //DebugLn(['TPkgGraphExplorerDlg.PkgPopupMenuPopup ',Pkg<>nil,' ',(Pkg<>nil) and (Pkg.AutoInstall<>pitNope)]);
  UninstallMenuItem.Visible:=(Pkg<>nil) and (Pkg.AutoInstall<>pitNope);
  if UninstallMenuItem.Visible then
    UninstallMenuItem.Caption:='Uninstall package '+Pkg.IDAsString;
end;

procedure TPkgGraphExplorerDlg.PkgTreeViewDblClick(Sender: TObject);
var
  Pkg: TLazPackage;
  Dependency: TPkgDependency;
begin
  GetDependency(PkgTreeView.Selected,Pkg,Dependency);
  if Pkg<>nil then begin
    if Assigned(OnOpenPackage) then
      OnOpenPackage(Self,Pkg);
  end;
end;

procedure TPkgGraphExplorerDlg.PkgTreeViewExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  Pkg, ChildPackage: TLazPackage;
  Dependency: TPkgDependency;
  ViewNode: TTreeNode;
  NodeText: String;
  NodeImgIndex: Integer;
  NextViewNode: TTreeNode;
begin
  // add child nodes
  GetDependency(Node,Pkg,Dependency);
  if Dependency<>nil then begin
    // node is a not fullfilled dependency
    AllowExpansion:=false;
  end else if Pkg<>nil then begin
    // node is a package
    ViewNode:=Node.GetFirstChild;
    Dependency:=Pkg.FirstRequiredDependency;
    while Dependency<>nil do begin
      // find required package
      if PackageGraph.OpenDependency(Dependency,false)=lprSuccess then
      begin
        ChildPackage:=Dependency.RequiredPackage;
        // package found
        NodeText:=ChildPackage.IDAsString;
        if SearchParentNodeWithText(Node,NodeText)<>nil then
          NodeImgIndex:=ImgIndexCirclePackage
        else
          NodeImgIndex:=GetPackageImageIndex(ChildPackage);
      end else begin
        // package not found
        NodeText:=Dependency.AsString;
        NodeImgIndex:=ImgIndexMissingPackage;
        ChildPackage:=nil;
        // Todo broken packages
      end;
      // add node
      if ViewNode=nil then
        ViewNode:=PkgTreeView.Items.AddChild(Node,NodeText)
      else
        ViewNode.Text:=NodeText;
      ViewNode.ImageIndex:=NodeImgIndex;
      ViewNode.SelectedIndex:=ViewNode.ImageIndex;
      ViewNode.Expanded:=false;
      ViewNode.HasChildren:=
            (ChildPackage<>nil) and (ChildPackage.FirstRequiredDependency<>nil);
      ViewNode:=ViewNode.GetNextSibling;
      Dependency:=Dependency.NextRequiresDependency;
    end;
    // delete unneeded nodes
    while ViewNode<>nil do begin
      NextViewNode:=ViewNode.GetNextSibling;
      ViewNode.Free;
      ViewNode:=NextViewNode;
    end;
  end else begin
    DebugLn(['TPkgGraphExplorerDlg.PkgTreeViewExpanding Node has no package ',Node.Text]);
  end;
end;

procedure TPkgGraphExplorerDlg.InfoMemoKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Parent=nil) then
    Close;
end;

procedure TPkgGraphExplorerDlg.PkgTreeViewSelectionChanged(Sender: TObject);
begin
  UpdateInfo;
  UpdateList;
end;

procedure TPkgGraphExplorerDlg.UninstallMenuItemClick(Sender: TObject);
var
  Pkg: TLazPackage;
  Dependency: TPkgDependency;
begin
  GetDependency(PkgTreeView.Selected,Pkg,Dependency);
  if Pkg<>nil then begin
    if Assigned(OnUninstallPackage) then OnUninstallPackage(Self,Pkg);
  end;
end;

procedure TPkgGraphExplorerDlg.SetupComponents;
begin
  PkgTreeView.Images := IDEImages.Images_16;
  ImgIndexPackage := IDEImages.LoadImage(16, 'item_package');
  ImgIndexInstalledPackage := IDEImages.LoadImage(16, 'pkg_installed');
  ImgIndexInstallPackage := IDEImages.LoadImage(16, 'pkg_package_autoinstall');
  ImgIndexUninstallPackage := IDEImages.LoadImage(16, 'pkg_package_uninstall');
  ImgIndexCirclePackage := IDEImages.LoadImage(16, 'pkg_package_circle');
  ImgIndexMissingPackage := IDEImages.LoadImage(16, 'pkg_conflict');

  PkgTreeLabel.Caption:=lisPckExplLoadedPackages;
  PkgListLabel.Caption:=lisPckExplIsRequiredBy;
end;

function TPkgGraphExplorerDlg.GetPackageImageIndex(Pkg: TLazPackage): integer;
begin
  if Pkg.Installed<>pitNope then begin
    if Pkg.AutoInstall<>pitNope then begin
      Result:=ImgIndexInstalledPackage;
    end else begin
      Result:=ImgIndexUninstallPackage;
    end;
  end else begin
    if Pkg.AutoInstall<>pitNope then begin
      Result:=ImgIndexInstallPackage;
    end else begin
      Result:=ImgIndexPackage;
    end;
  end;
end;

procedure TPkgGraphExplorerDlg.GetDependency(ANode: TTreeNode;
  var Pkg: TLazPackage; var Dependency: TPkgDependency);
// if Dependency<>nil then Pkg is the Parent
var
  NodeText: String;
  NodePackageID: TLazPackageID;
begin
  // keep in mind, that packages can be deleted and the node is outdated
  Pkg:=nil;
  Dependency:=nil;
  if ANode=nil then exit;
  NodePackageID:=TLazPackageID.Create;
  try
    // try to find a package
    NodeText:=ANode.Text;
    if NodePackageID.StringToID(NodeText) then
      Pkg:=PackageGraph.FindPackageWithID(NodePackageID);
    if Pkg<>nil then exit;
    // try to find the parent package
    if (ANode.Parent=nil) or (not NodePackageID.StringToID(ANode.Parent.Text))
    then
      exit;
    Pkg:=PackageGraph.FindPackageWithID(NodePackageID);
    if Pkg=nil then exit;
    // there is a parent package -> search the dependency
    Dependency:=Pkg.FirstRequiredDependency;
    while Dependency<>nil do begin
      if Dependency.AsString=NodeText then exit;
      Dependency:=Dependency.NextRequiresDependency;
    end;
  finally
    NodePackageID.Free;
  end;
end;

procedure TPkgGraphExplorerDlg.GetCurrentIsUsedBy(var Dependency: TPkgDependency);
var
  TreePkg: TLazPackage;
  TreeDependency: TPkgDependency;
  ListIndex: Integer;
  UsedByDep: TPkgDependency;
  SelName: string;
begin
  Dependency:=nil;
  ListIndex:=PkgListBox.ItemIndex;
  if ListIndex<0 then exit;
  GetDependency(PkgTreeView.Selected,TreePkg,TreeDependency);
  if (Dependency=nil) and (TreePkg<>nil) then begin
    SelName:=PkgListBox.Items[ListIndex];
    UsedByDep:=TreePkg.FirstUsedByDependency;
    while UsedByDep<>nil do begin
      if UsedByDep.Owner<>PackageGraph then begin
        if SelName=GetDependencyOwnerAsString(UsedByDep) then begin
          Dependency:=UsedByDep;
        end;
      end;
      UsedByDep:=UsedByDep.NextUsedByDependency;
    end;
  end;
end;

function TPkgGraphExplorerDlg.SearchParentNodeWithText(ANode: TTreeNode;
  const NodeText: string): TTreeNode;
begin
  Result:=ANode;
  while Result<>nil do begin
    if Result.Text=NodeText then exit;
    Result:=Result.Parent;
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
  Caption:=dlgPackageGraph;

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
  UpdateTree;
  UpdateList;
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
begin
  // rebuild internal sorted packages
  fSortedPackages.Clear;
  Cnt:=PackageGraph.Count;
  for i:=0 to Cnt-1 do
    fSortedPackages.Add(PackageGraph[i]);
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
      ViewNode:=PkgTreeView.Items.Add(nil,CurPkg.IDAsString)
    else
      ViewNode.Text:=CurPkg.IDAsString;
    ViewNode.HasChildren:=CurPkg.FirstRequiredDependency<>nil;
    ViewNode.Expanded:=false;
    ViewNode.ImageIndex:=GetPackageImageIndex(CurPkg);
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
  PkgTreeView.EndUpdate;
end;

procedure TPkgGraphExplorerDlg.UpdateList;
var
  Pkg: TLazPackage;
  Dependency: TPkgDependency;
  UsedByDep: TPkgDependency;
  sl: TStringList;
  NewItem: String;
begin
  GetDependency(PkgTreeView.Selected,Pkg,Dependency);
  if Dependency<>nil then begin
    PkgListBox.Items.Clear;
  end else if Pkg<>nil then begin
    UsedByDep:=Pkg.FirstUsedByDependency;
    sl:=TStringList.Create;
    while UsedByDep<>nil do begin
      if UsedByDep.Owner<>PackageGraph then begin
        NewItem:=GetDependencyOwnerAsString(UsedByDep);
        sl.Add(NewItem);
      end;
      UsedByDep:=UsedByDep.NextUsedByDependency;
    end;
    PkgListBox.Items.Assign(sl);
    PkgListBox.ItemIndex:=-1;
    sl.Free;
  end;
end;

procedure TPkgGraphExplorerDlg.UpdateInfo;
var
  Pkg: TLazPackage;
  Dependency: TPkgDependency;
  InfoStr: String;

  procedure AddState(const NewState: string);
  begin
    if (InfoStr<>'') and (InfoStr[length(InfoStr)]<>' ') then
      InfoStr:=InfoStr+', ';
    InfoStr:=InfoStr+NewState;
  end;

begin
  InfoStr:='';
  GetDependency(PkgTreeView.Selected,Pkg,Dependency);
  if Dependency<>nil then begin
    InfoStr:=Format(lisPckExplPackageNotFound, [Dependency.AsString]);
  end else if Pkg<>nil then begin
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
var
  NewNode: TTreeNode;
begin
  if Pkg=nil then exit;
  NewNode:=FindMainNodeWithText(Pkg.IDAsString);
  if NewNode<>nil then
    PkgTreeView.Selected:=NewNode;
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

function TPkgGraphExplorerDlg.FindMainNodeWithText(const s: string): TTreeNode;
begin
  Result:=nil;
  if PkgTreeView.Items.Count=0 then exit;
  Result:=PkgTreeView.Items[0];
  while (Result<>nil) and (Result.Text<>s) do Result:=Result.GetNextSibling;
end;

procedure TPkgGraphExplorerDlg.ShowPath(PathList: TFPList);
var
  AnObject: TObject;
  CurNode, LastNode: TTreeNode;
  i: Integer;
  
  procedure SelectChild(var Node: TTreeNode; const NodeText: string);
  var
    i: Integer;
  begin
    if Node=nil then
      Node:=FindMainNodeWithText(NodeText)
    else begin
      Node.Expanded:=true;
      i:=Node.IndexOfText(NodeText);
      if i>=0 then
        Node:=Node.Items[i]
      else
        Node:=nil;
    end;
  end;
  
begin
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
end;

end.

