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
}
unit CleanPkgDeps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, FileUtil, AvgLvlTree, LazLogger, LvlGraphCtrl,
  Forms, Controls, Graphics, Dialogs, ButtonPanel, ComCtrls, ExtCtrls, StdCtrls,
  Buttons, LazarusIDEStrConsts, Project, PackageDefs, IDEImagesIntf;

const
  CPDProjectName = '-Project-';
type

  { TCPDNodeInfo }

  TCPDNodeInfo = class
  public
    Owner: string; // CPDProjectName or package name
    Dependency: string; // required package name
  end;

  { TCleanPkgDepsDlg }

  TCleanPkgDepsDlg = class(TForm)
    CancelBitBtn: TBitBtn;
    DeleteSelectedBitBtn: TBitBtn;
    BtnPanel: TPanel;
    SelectAllBitBtn: TBitBtn;
    SelectNoneBitBtn: TBitBtn;
    TransitivityGroupBox: TGroupBox;
    TransitivityLabel: TLabel;
    TransitivityTreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SelectAllBitBtnClick(Sender: TObject);
    procedure SelectNoneBitBtnClick(Sender: TObject);
    procedure TransitivityTreeViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    function GetTVNodeChecked(TVNode: TTreeNode): boolean;
    procedure SetTVNodeChecked(TVNode: TTreeNode; AValue: boolean);
  private
    FOwners: TFPList;
    ImgIndexProject: integer;
    ImgIndexPackage: integer;
    ImgIndexDelete: integer;
    ImgIndexKeep: integer;
    procedure SetOwners(AValue: TFPList);
    procedure ClearTreeData;
    procedure UpdateTransitivityTree;
    procedure UpdateButtons;
    procedure AddTransitivities(NodeCaption: string; ImgIndex: integer;
      FirstDependency: TPkgDependency);
    function FindAlternativeRoute(Dependency, StartDependency: TPkgDependency): TFPList;
    property TVNodeChecked[TVNode: TTreeNode]: boolean read GetTVNodeChecked write SetTVNodeChecked;
  public
    property Owners: TFPList read FOwners write SetOwners;
    function FetchDeletes: TObjectList; // list of TCPDNodeInfo
  end;

var
  CleanPkgDepsDlg: TCleanPkgDepsDlg;

function ShowCleanPkgDepDlg(Pkg: TLazPackage; out ListOfNodeInfos: TObjectList): TModalResult;
function ShowCleanPkgDepDlg(AProject: TProject; out ListOfNodeInfos: TObjectList): TModalResult;
function ShowCleanPkgDepDlg(Owners: TFPList; FreeOwners: boolean;
  out ListOfNodeInfos: TObjectList): TModalResult;

implementation

function ShowCleanPkgDepDlg(Pkg: TLazPackage; out ListOfNodeInfos: TObjectList): TModalResult;
var
  Owners: TFPList;
begin
  Owners:=TFPList.Create;
  Owners.Add(Pkg);
  Result:=ShowCleanPkgDepDlg(Owners,true,ListOfNodeInfos);
end;

function ShowCleanPkgDepDlg(AProject: TProject;
  out ListOfNodeInfos: TObjectList): TModalResult;
var
  Owners: TFPList;
begin
  Owners:=TFPList.Create;
  Owners.Add(AProject);
  Result:=ShowCleanPkgDepDlg(Owners,true,ListOfNodeInfos);
end;

function ShowCleanPkgDepDlg(Owners: TFPList; FreeOwners: boolean;
  out ListOfNodeInfos: TObjectList): TModalResult;
var
  Dlg: TCleanPkgDepsDlg;
begin
  ListOfNodeInfos:=nil;
  Dlg:=TCleanPkgDepsDlg.Create(nil);
  try
    Dlg.Owners:=Owners;
    Result:=Dlg.ShowModal;
    if Result=mrOk then
      ListOfNodeInfos:=Dlg.FetchDeletes;
  finally
    if FreeOwners then
      Owners.Free;
    Dlg.Free;
  end;
end;

{$R *.lfm}

{ TCleanPkgDepsDlg }

procedure TCleanPkgDepsDlg.FormCreate(Sender: TObject);
begin
  ImgIndexProject          := IDEImages.LoadImage(16, 'item_project');
  ImgIndexPackage          := IDEImages.LoadImage(16, 'item_package');
  ImgIndexDelete           := IDEImages.LoadImage(16, 'laz_delete');
  ImgIndexKeep             := IDEImages.LoadImage(16, 'menu_run');

  Caption:=lisPkgCleanUpPackageDependencies;
  TransitivityGroupBox.Caption:=lisPkgTransitivity;
  TransitivityLabel.Caption:=
    lisPkgTheFollowingDependenciesAreNotNeededBecauseOfTheAu;
  TransitivityTreeView.Images:=IDEImages.Images_16;

  SelectAllBitBtn.Caption:=lisMenuSelectAll;
  SelectNoneBitBtn.Caption:=lisPkgClearSelection;
  DeleteSelectedBitBtn.Caption:=lisPkgDeleteDependencies;
end;

procedure TCleanPkgDepsDlg.FormDestroy(Sender: TObject);
begin
  ClearTreeData;
end;

procedure TCleanPkgDepsDlg.SelectAllBitBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to TransitivityTreeView.Items.Count-1 do
    TVNodeChecked[TransitivityTreeView.Items[i]]:=true;
end;

procedure TCleanPkgDepsDlg.SelectNoneBitBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to TransitivityTreeView.Items.Count-1 do
    TVNodeChecked[TransitivityTreeView.Items[i]]:=false;
end;

procedure TCleanPkgDepsDlg.TransitivityTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TVNode: TTreeNode;
begin
  TVNode:=TransitivityTreeView.GetNodeAt(X,Y);
  if TVNode=nil then exit;
  if X>=TVNode.DisplayIconLeft then begin
    TVNodeChecked[TVNode]:=not TVNodeChecked[TVNode];
  end;
end;

function TCleanPkgDepsDlg.GetTVNodeChecked(TVNode: TTreeNode): boolean;
begin
  Result:=(TVNode<>nil) and (TVNode.Data<>nil) and (TVNode.ImageIndex=ImgIndexDelete);
end;

procedure TCleanPkgDepsDlg.SetTVNodeChecked(TVNode: TTreeNode; AValue: boolean);
begin
  if TVNode.Data=nil then exit;
  if TVNodeChecked[TVNode]=AValue then exit;
  if AValue then
    TVNode.ImageIndex:=ImgIndexDelete
  else
    TVNode.ImageIndex:=ImgIndexKeep;
  TVNode.SelectedIndex:=TVNode.ImageIndex;
  UpdateButtons;
end;

procedure TCleanPkgDepsDlg.SetOwners(AValue: TFPList);
begin
  if FOwners=AValue then Exit;
  FOwners:=AValue;
  UpdateTransitivityTree;
  UpdateButtons;
end;

procedure TCleanPkgDepsDlg.ClearTreeData;
var
  i: Integer;
  TVNode: TTreeNode;
begin
  for i:=0 to TransitivityTreeView.Items.Count-1 do begin
    TVNode:=TransitivityTreeView.Items[i];
    if TVNode.Data<>nil then begin
      TObject(TVNode.Data).Free;
      TVNode.Data:=nil;
    end;
  end;
end;

procedure TCleanPkgDepsDlg.UpdateTransitivityTree;
var
  i: Integer;
  CurOwner: TObject;
  AProject: TProject;
  APackage: TLazPackage;
begin
  TransitivityTreeView.BeginUpdate;
  ClearTreeData;
  TransitivityTreeView.Items.Clear;
  for i:=0 to Owners.Count-1 do begin
    CurOwner:=TObject(Owners[i]);
    if CurOwner is TProject then begin
      AProject:=TProject(CurOwner);
      AddTransitivities(CPDProjectName,ImgIndexProject,AProject.FirstRequiredDependency);
    end else if CurOwner is TLazPackage then begin
      APackage:=TLazPackage(CurOwner);
      AddTransitivities(APackage.IDAsString,ImgIndexPackage,APackage.FirstRequiredDependency);
    end;
  end;
  TransitivityTreeView.EndUpdate;
end;

procedure TCleanPkgDepsDlg.UpdateButtons;
var
  i: Integer;
  TVNode: TTreeNode;
  CheckCnt: Integer;
begin
  CheckCnt:=0;
  for i:=0 to TransitivityTreeView.Items.Count-1 do begin
    TVNode:=TransitivityTreeView.Items[i];
    if TVNodeChecked[TVNode] then
      CheckCnt+=1;
  end;
  DeleteSelectedBitBtn.Enabled:=CheckCnt>0;
end;

procedure TCleanPkgDepsDlg.AddTransitivities(NodeCaption: string;
  ImgIndex: integer; FirstDependency: TPkgDependency);
var
  Dependency: TPkgDependency;
  AltRoute: TFPList;
  MainTVNode: TTreeNode;
  TVNode: TTreeNode;
  Info: TCPDNodeInfo;
  s: String;
  i: Integer;
begin
  MainTVNode:=nil;
  Dependency:=FirstDependency;
  while Dependency<>nil do begin
    AltRoute:=FindAlternativeRoute(Dependency,FirstDependency);
    if AltRoute<>nil then begin
      if MainTVNode=nil then begin
        MainTVNode:=TransitivityTreeView.Items.Add(nil,NodeCaption);
        MainTVNode.ImageIndex:=ImgIndex;
        MainTVNode.SelectedIndex:=MainTVNode.ImageIndex;
      end;
      s:=Dependency.AsString+' = ';
      for i:=0 to AltRoute.Count-1 do begin
        if i>0 then
          s+='-';
        s+=TLazPackage(AltRoute[i]).Name;
      end;
      TVNode:=TransitivityTreeView.Items.AddChild(MainTVNode,s);
      TVNode.ImageIndex:=ImgIndexDelete;
      TVNode.SelectedIndex:=TVNode.ImageIndex;
      Info:=TCPDNodeInfo.Create;
      TVNode.Data:=Info;
      Info.Owner:=NodeCaption;
      Info.Dependency:=Dependency.RequiredPackage.Name;
      MainTVNode.Expand(true);
      AltRoute.Free;
    end;
    Dependency:=Dependency.NextRequiresDependency;
  end;
end;

function TCleanPkgDepsDlg.FindAlternativeRoute(Dependency,
  StartDependency: TPkgDependency): TFPList;
var
  Visited: TAvgLvlTree;

  function Search(Pkg: TLazPackage; Level: integer; var AltRoute: TFPList): boolean;
  var
    CurDependency: TPkgDependency;
  begin
    Result:=false;
    if Pkg=nil then exit;
    if Pkg=Dependency.Owner then exit; // cycle detected
    if (Level>0) and (Pkg=Dependency.RequiredPackage) then begin
      // alternative route found
      AltRoute:=TFPList.Create;
      AltRoute.Add(Pkg);
      exit(true);
    end;

    if Visited.Find(Pkg)<>nil then exit;
    Visited.Add(Pkg);
    CurDependency:=Pkg.FirstRequiredDependency;
    while CurDependency<>nil do begin
      if Search(CurDependency.RequiredPackage,Level+1,AltRoute) then begin
        AltRoute.Insert(0,Pkg);
        exit(true);
      end;
      CurDependency:=CurDependency.NextRequiresDependency;
    end;
  end;

var
  CurDependency: TPkgDependency;
begin
  Result:=nil;
  if Dependency=nil then exit;
  if Dependency.RequiredPackage=nil then exit;
  Visited:=TAvgLvlTree.Create;
  try
    CurDependency:=StartDependency;
    while CurDependency<>nil do begin
      if CurDependency<>Dependency then
        if Search(CurDependency.RequiredPackage,0,Result) then exit;
      CurDependency:=CurDependency.NextRequiresDependency;
    end;
  finally
    Visited.Free;
  end;
end;

function TCleanPkgDepsDlg.FetchDeletes: TObjectList;
var
  i: Integer;
  TVNode: TTreeNode;
  Info: TCPDNodeInfo;
begin
  Result:=TObjectList.Create(true);
  for i:=0 to TransitivityTreeView.Items.Count-1 do begin
    TVNode:=TransitivityTreeView.Items[i];
    if TObject(TVNode.Data) is TCPDNodeInfo then begin
      Info:=TCPDNodeInfo(TVNode.Data);
      TVNode.Data:=nil;
      Result.Add(Info);
    end;
  end;
end;

end.

