unit AddFPMakeDependencyDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, ListFilterEdit,
  ProjPackCommon,
  PackageDefs,
  LazarusIDEStrConsts,
  IDEWindowIntf,
  AddPkgDependencyDlg,
  // fppkg
  FppkgHelper;

type

  { TAddFPMakeDependencyDialog }

  TAddFPMakeDependencyDialog = class(TForm)
    BP: TButtonPanel;
    DependPkgNameFilter: TListFilterEdit;
    DependPkgNameLabel: TLabel;
    DependPkgNameListBox: TListBox;
    procedure OKButtonClick(Sender: TObject);
  private
    FResultDependencies: TPkgDependencyList;
    FPackageNameList: TStringList;
    procedure UpdateAvailableDependencyNames;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

function ShowAddFPMakeDependencyDlg({%H-}AProjPack: IProjPack;
  out AResultDependencies: TPkgDependencyList): TModalResult;

implementation

{$R *.lfm}

function ShowAddFPMakeDependencyDlg(AProjPack: IProjPack; out AResultDependencies: TPkgDependencyList): TModalResult;
var
  AddDepDialog: TAddFPMakeDependencyDialog;
begin
  AddDepDialog:=TAddFPMakeDependencyDialog.Create(nil);
  AddDepDialog.UpdateAvailableDependencyNames;

  Result:=AddDepDialog.ShowModal;
  if Result=mrOk then
    begin
    AResultDependencies:=AddDepDialog.FResultDependencies;
    AddDepDialog.fResultDependencies:=nil;
    end
  else
    begin
    AResultDependencies:=nil;
    end;
  AddDepDialog.Free;
end;

{ TAddFPMakeDependencyDialog }

procedure TAddFPMakeDependencyDialog.OKButtonClick(Sender: TObject);
var
  NewDependency: TPkgDependency;
  i: Integer;
begin
  // Add all selected packages.
  FResultDependencies := TPkgDependencyList.Create; // Will be freed by the caller.
  if DependPkgNameListBox.SelCount > 0 then
  begin
    for i := 0 to DependPkgNameListBox.Count-1 do
    begin
      if DependPkgNameListBox.Selected[i] then
      begin
        NewDependency := TPkgDependency.Create;   // Will be added to package graph.
        NewDependency.PackageName := DependPkgNameListBox.Items[i];
        NewDependency.DependencyType := pdtFPMake;
        //if not CheckAddingDependency(fProjPack, NewDependency) then exit;
        FResultDependencies.Add(NewDependency);
        NewDependency := nil;
      end;
    end;
  end;
  ModalResult := mrOk;
end;

procedure TAddFPMakeDependencyDialog.UpdateAvailableDependencyNames;
begin
  FPackageNameList.Clear;
  TFppkgHelper.Instance.ListPackages(FPackageNameList);

  FPackageNameList.Sort;

  DependPkgNameFilter.Items.BeginUpdate;
  try
    DependPkgNameFilter.Items.Clear;
    DependPkgNameFilter.Items.Assign(FPackageNameList);
    DependPkgNameFilter.InvalidateFilter;
  finally
    DependPkgNameFilter.Items.EndUpdate;
  end;
end;

constructor TAddFPMakeDependencyDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPackageNameList := TStringList.Create;

  Caption:=lisProjAddNewRequirement;

  DependPkgNameLabel.Caption:=lisProjAddPackageName;
  BP.CloseButton.Caption := lisPckEditInstall;

  IDEDialogLayoutList.ApplyLayout(Self,450,300);
end;

destructor TAddFPMakeDependencyDialog.Destroy;
begin
  FPackageNameList.Free;
  inherited Destroy;
end;

end.

