unit CleanPkgDeps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LvlGraphCtrl, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, ComCtrls, ExtCtrls, StdCtrls, LazarusIDEStrConsts, Project,
  PackageDefs;

type

  { TCleanPkgDepsDlg }

  TCleanPkgDepsDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    TransitivityGroupBox: TGroupBox;
    TransitivityLabel: TLabel;
    TransitivityTreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
  private
    FOwners: TFPList;
    procedure SetOwners(AValue: TFPList);
    procedure UpdateTransitivityTree;
    procedure UpdateButtons;
    function IsTVNodeChecked(TVNode: TTreeNode): boolean;
    procedure AddTransitivities(NodeCaption: string;
      FirstDependency: TPkgDependency);
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
  Caption:='Clean up package dependencies';
  TransitivityGroupBox.Caption:='Transitivity';
  TransitivityLabel.Caption:='The following dependencies are not needed, because of the automatic transitivity between package dependencies.';
  ButtonPanel1.OKButton.Caption:='Delete dependencies';
end;

procedure TCleanPkgDepsDlg.SetOwners(AValue: TFPList);
begin
  if FOwners=AValue then Exit;
  FOwners:=AValue;
  UpdateTransitivityTree;
  UpdateButtons;
end;

procedure TCleanPkgDepsDlg.UpdateTransitivityTree;
var
  i: Integer;
  CurOwner: TObject;
  AProject: TProject;
  APackage: TLazPackage;
begin
  TransitivityTreeView.BeginUpdate;
  TransitivityTreeView.Items.Clear;
  for i:=0 to Owners.Count-1 do begin
    CurOwner:=TObject(Owners[i]);
    if CurOwner is TProject then begin
      AProject:=TProject(CurOwner);
      AddTransitivities('-Project-',AProject.FirstRequiredDependency);
    end else if CurOwner is TLazPackage then begin
      APackage:=TLazPackage(CurOwner);
      AddTransitivities(APackage.IDAsString,APackage.FirstRequiredDependency);
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
  FirstDependency: TPkgDependency);
var
  Dependency: TPkgDependency;
begin
  Dependency:=FirstDependency;
  while Dependency<>nil do begin

    Dependency:=Dependency.NextRequiresDependency;
  end;
end;

end.

