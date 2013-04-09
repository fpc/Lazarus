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
  Classes, SysUtils, FileUtil, AvgLvlTree, LazLogger, LvlGraphCtrl, Forms,
  Controls, Graphics, Dialogs, ButtonPanel, ComCtrls, ExtCtrls, StdCtrls,
  LazarusIDEStrConsts, Project, PackageDefs, IDEImagesIntf;

const
  CPDProjectName = '-Project-';
type

  TCPDNodeInfo = class
  public
    Owner: string; // CPDProjectName or package name
    Dependency: string; // required package name
  end;

  { TCleanPkgDepsDlg }

  TCleanPkgDepsDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    TransitivityGroupBox: TGroupBox;
    TransitivityLabel: TLabel;
    TransitivityTreeView: TTreeView;
    procedure ButtonPanel1OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FOwners: TFPList;
    ImgIndexProject: integer;
    ImgIndexPackage: integer;
    procedure SetOwners(AValue: TFPList);
    procedure ClearTreeData;
    procedure UpdateTransitivityTree;
    procedure UpdateButtons;
    function IsTVNodeChecked(TVNode: TTreeNode): boolean;
    procedure AddTransitivities(NodeCaption: string; ImgIndex: integer;
      FirstDependency: TPkgDependency);
    function FindAlternativeRoute(Dependency, StartDependency: TPkgDependency): TFPList;
  public
    property Owners: TFPList read FOwners write SetOwners;
  end;

var
  CleanPkgDepsDlg: TCleanPkgDepsDlg;

function ShowCleanPkgDepDlg(Pkg: TLazPackage): TModalResult;
function ShowCleanPkgDepDlg(AProject: TProject): TModalResult;
function ShowCleanPkgDepDlg(Owners: TFPList; FreeList: boolean): TModalResult;

implementation

function ShowCleanPkgDepDlg(Pkg: TLazPackage): TModalResult;
var
  Owners: TFPList;
begin
  Owners:=TFPList.Create;
  Owners.Add(Pkg);
  Result:=ShowCleanPkgDepDlg(Owners,true);
end;

function ShowCleanPkgDepDlg(AProject: TProject): TModalResult;
var
  Owners: TFPList;
begin
  Owners:=TFPList.Create;
  Owners.Add(AProject);
  Result:=ShowCleanPkgDepDlg(Owners,true);
end;

function ShowCleanPkgDepDlg(Owners: TFPList; FreeList: boolean): TModalResult;
var
  Dlg: TCleanPkgDepsDlg;
begin
  Dlg:=TCleanPkgDepsDlg.Create(nil);
  try
    Dlg.Owners:=Owners;
    Result:=Dlg.ShowModal;
  finally
    if FreeList then
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

  Caption:='Clean up package dependencies';
  TransitivityGroupBox.Caption:='Transitivity';
  TransitivityLabel.Caption:='The following dependencies are not needed, because of the automatic transitivity between package dependencies.';
  TransitivityTreeView.Images:=IDEImages.Images_16;
  ButtonPanel1.OKButton.Caption:='Delete dependencies';
  ButtonPanel1.OKButton.OnClick:=@ButtonPanel1OKButtonClick;
end;

procedure TCleanPkgDepsDlg.FormDestroy(Sender: TObject);
begin
  ClearTreeData;
end;

procedure TCleanPkgDepsDlg.ButtonPanel1OKButtonClick(Sender: TObject);
begin
  ShowMessage('Not implemented yet');
  ModalResult:=mrNone;
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
    if IsTVNodeChecked(TVNode) then
      CheckCnt+=1;
  end;
  ButtonPanel1.OKButton.Enabled:=CheckCnt>0;
end;

function TCleanPkgDepsDlg.IsTVNodeChecked(TVNode: TTreeNode): boolean;
begin
  Result:=(TVNode<>nil) and (TVNode.StateIndex=1);
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
      end;
      s:=Dependency.AsString+' = ';
      for i:=0 to AltRoute.Count-1 do begin
        if i>0 then
          s+='-';
        s+=TLazPackage(AltRoute[i]).Name;
      end;
      TVNode:=TransitivityTreeView.Items.AddChild(MainTVNode,s);
      TVNode.ImageIndex:=ImgIndexPackage;
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

end.

