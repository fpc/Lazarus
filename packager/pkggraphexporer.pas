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
    TPkgGraphExplorer is the IDE window showing the whole package graph.
}
unit PkgGraphExporer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Buttons, ComCtrls, StdCtrls,
  ExtCtrls, Menus, Dialogs, AVL_Tree, LazarusIDEStrConsts, IDEProcs,
  IDEOptionDefs, EnvironmentOpts, PackageDefs, PackageSystem;
  
type
  TPkgGraphExplorer = class(TForm)
    ImageList: TImageList;
    PkgTreeLabel: TLabel;
    PkgTreeView: TTreeView;
    PkgListLabel: TLabel;
    PkgListBox: TListBox;
    InfoMemo: TMemo;
    procedure PkgGraphExplorerResize(Sender: TObject);
    procedure PkgGraphExplorerShow(Sender: TObject);
  private
    fSortedPackages: TAVLTree;
    procedure SetupComponents;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateAll;
    procedure UpdateTree;
    procedure UpdateList;
    procedure UpdateInfo;
    procedure UpdatePackageName(Pkg: TLazPackage; const OldName: string);
    procedure UpdatePackageID(Pkg: TLazPackage);
    procedure UpdatePackageAdded(Pkg: TLazPackage);
  end;
  
var
  PackageGraphExplorer: TPkgGraphExplorer;

implementation

uses Math;

{ TPkgGraphExplorer }

procedure TPkgGraphExplorer.PkgGraphExplorerResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
begin
  x:=1;
  y:=1;
  w:=((ClientWidth-3*x) div 2);
  with PkgTreeLabel do
    SetBounds(x,y,w,Height);
  inc(x,PkgTreeLabel.Width+PkgTreeLabel.Left);
  
  with PkgListLabel do
    SetBounds(x,y,w,Height);
  x:=1;
  inc(y,PkgTreeLabel.Height+3);

  with PkgTreeView do
    SetBounds(x,y,w,Max(Parent.ClientHeight-y-80,10));
  inc(x,PkgTreeView.Width+PkgTreeView.Left);

  with PkgListBox do
    SetBounds(x,y,w,PkgTreeView.Height);
  x:=1;
  inc(y,PkgTreeView.Height+1);
  
  with InfoMemo do
    SetBounds(x,y,Parent.ClientWidth-2*x,Max(10,Parent.ClientHeight-y-x));
end;

procedure TPkgGraphExplorer.PkgGraphExplorerShow(Sender: TObject);
begin
  UpdateAll;
end;

procedure TPkgGraphExplorer.SetupComponents;
begin
  ImageList:=TImageList.Create(Self);
  with ImageList do begin
    Width:=16;
    Height:=16;
    Name:='ImageList';
  end;
  
  PkgTreeLabel:=TLabel.Create(Self);
  with PkgTreeLabel do begin
    Name:='PkgTreeLabel';
    Parent:=Self;
    Caption:='Required Packages Tree:';
  end;
  
  PkgTreeView:=TTreeView.Create(Self);
  with PkgTreeView do begin
    Name:='PkgTreeView';
    Parent:=Self;
    Options:=Options+[tvoRightClickSelect];
  end;

  PkgListLabel:=TLabel.Create(Self);
  with PkgListLabel do begin
    Name:='PkgListLabel';
    Parent:=Self;
    Caption:='Packages requiring the selected package:';
  end;

  PkgListBox:=TListBox.Create(Self);
  with PkgListBox do begin
    Name:='PkgListBox';
    Parent:=Self;
  end;

  InfoMemo:=TMemo.Create(Self);
  with InfoMemo do begin
    Name:='InfoMemo';
    Parent:=Self;
  end;
end;

constructor TPkgGraphExplorer.Create(TheOwner: TComponent);
var
  ALayout: TIDEWindowLayout;
begin
  inherited Create(TheOwner);
  fSortedPackages:=TAVLTree.Create(@CompareLazPackageID);
  Name:=NonModalIDEWindowNames[nmiwPkgGraphExplorer];
  Caption:='Package Graph Explorer';

  ALayout:=EnvironmentOptions.IDEWindowLayoutList.ItemByFormID(Name);
  ALayout.Form:=TForm(Self);
  ALayout.Apply;
  
  SetupComponents;
  OnResize:=@PkgGraphExplorerResize;
  OnResize(Self);
  OnShow:=@PkgGraphExplorerShow;
end;

destructor TPkgGraphExplorer.Destroy;
begin
  FreeAndNil(fSortedPackages);
  inherited Destroy;
end;

procedure TPkgGraphExplorer.UpdateAll;
begin
  UpdateTree;
  UpdateList;
  UpdateInfo;
end;

procedure TPkgGraphExplorer.UpdateTree;
var
  Cnt: Integer;
  i: Integer;
  CurIndex: Integer;
  ViewNode: TTreeNode;
  NextViewNode: TTreeNode;
  HiddenNode: TAVLTreeNode;
  CurPkg: TLazPackage;
begin
  // rebuild internal sorted packages
  fSortedPackages.Clear;
  Cnt:=PackageGraph.Count;
  for i:=0 to Cnt-1 do
    fSortedPackages.Add(PackageGraph[i]);
  // rebuild the TreeView
  PkgTreeView.BeginUpdate;
  CurIndex:=0;
  HiddenNode:=fSortedPackages.FindLowest;
  ViewNode:=PkgTreeView.Items.GetFirstNode;
  while HiddenNode<>nil do begin
    CurPkg:=TLazPackage(HiddenNode.Data);
    if ViewNode=nil then
      ViewNode:=PkgTreeView.Items.Add(nil,CurPkg.IDAsString)
    else
      ViewNode.Text:=CurPkg.IDAsString;
    ViewNode:=ViewNode.GetNextSibling;
    HiddenNode:=fSortedPackages.FindSuccessor(HiddenNode);
    inc(CurIndex);
  end;
  while ViewNode<>nil do begin
    NextViewNode:=ViewNode.GetNextSibling;
    ViewNode.Free;
    ViewNode:=NextViewNode;
  end;
  PkgTreeView.EndUpdate;
end;

procedure TPkgGraphExplorer.UpdateList;
begin

end;

procedure TPkgGraphExplorer.UpdateInfo;
begin

end;

procedure TPkgGraphExplorer.UpdatePackageName(Pkg: TLazPackage;
  const OldName: string);
begin
  UpdateAll;
end;

procedure TPkgGraphExplorer.UpdatePackageID(Pkg: TLazPackage);
begin
  UpdateAll;
end;

procedure TPkgGraphExplorer.UpdatePackageAdded(Pkg: TLazPackage);
begin
  UpdateAll;
end;

initialization
  PackageGraphExplorer:=nil;

end.

