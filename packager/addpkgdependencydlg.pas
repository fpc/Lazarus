unit AddPkgDependencyDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, fgl,
  // LCL
  Forms, Controls, Dialogs, StdCtrls, ButtonPanel, LCLProc,
  // LazControls
  ListFilterEdit,
  // IDEIntf
  IDEWindowIntf, PackageIntf, IDEDialogs,
  // IDE
  LazarusIDEStrConsts, PackageDefs, PackageSystem, ProjPackBase, ProjPackChecks;

type

  TPkgDependencyList = specialize TFPGList<TPkgDependency>;

  { TAddPkgDependencyDialog }

  TAddPkgDependencyDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    DependMaxVersionEdit: TEdit;
    DependMaxVersionLabel: TLabel;
    DependMinVersionEdit: TEdit;
    DependMinVersionLabel: TLabel;
    DependPkgNameFilter: TListFilterEdit;
    DependPkgNameLabel: TLabel;
    DependPkgNameListBox: TListBox;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure OKButtonClick(Sender: TObject);
  private
    fPackages: TAVLTree;  // tree of  TLazPackage or TPackageLink
    fProjPack: IProjPack;
    fResultDependencies: TPkgDependencyList;
    procedure AddUniquePackagesToList(APackageID: TLazPackageID);
    procedure UpdateAvailableDependencyNames;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  AddPkgDependencyDialog: TAddPkgDependencyDialog;

function ShowAddPkgDependencyDlg(AProjPack: IProjPack;
  out AResultDependencies: TPkgDependencyList): TModalResult;

implementation

{$R *.lfm}

function ShowAddPkgDependencyDlg(AProjPack: IProjPack;
  out AResultDependencies: TPkgDependencyList): TModalResult;
var
  AddDepDialog: TAddPkgDependencyDialog;
begin
  AddDepDialog:=TAddPkgDependencyDialog.Create(nil);
  AddDepDialog.fProjPack:=AProjPack;
  AddDepDialog.UpdateAvailableDependencyNames;

  Result:=AddDepDialog.ShowModal;
  if Result=mrOk then begin
    AResultDependencies:=AddDepDialog.fResultDependencies;
    AddDepDialog.fResultDependencies:=nil;
  end else begin
    AResultDependencies:=nil;
  end;
  AddDepDialog.Free;
end;

{ TAddPkgDependencyDialog }

constructor TAddPkgDependencyDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption:=lisProjAddNewRequirement;
  fPackages:=TAVLTree.Create(@CompareLazPackageIDNames);
  IDEDialogLayoutList.ApplyLayout(Self,400,360);

  DependPkgNameLabel.Caption:=lisProjAddPackageName;
  DependMinVersionLabel.Caption:=lisProjAddMinimumVersionOptional;
  DependMinVersionEdit.Text:='';
  DependMaxVersionLabel.Caption:=lisProjAddMaximumVersionOptional;
  DependMaxVersionEdit.Text:='';
end;

destructor TAddPkgDependencyDialog.Destroy;
begin
  FreeAndNil(fPackages);
  inherited Destroy;
end;

procedure TAddPkgDependencyDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TAddPkgDependencyDialog.AddUniquePackagesToList(APackageID: TLazPackageID);
begin
  if (APackageID.IDAsString<>fProjPack.IDAsString) and (fPackages.Find(APackageID)=Nil) then
    fPackages.Add(APackageID);
end;

procedure TAddPkgDependencyDialog.UpdateAvailableDependencyNames;
var
  ANode: TAVLTreeNode;
  sl: TStringList;
begin
  fPackages.Clear;
  PackageGraph.IteratePackages(fpfSearchAllExisting,@AddUniquePackagesToList);
  sl:=TStringList.Create;
  try
    ANode:=fPackages.FindLowest;
    while ANode<>nil do begin
      sl.Add(TLazPackageID(ANode.Data).Name);
      ANode:=fPackages.FindSuccessor(ANode);
    end;
    DependPkgNameFilter.Items.Assign(sl);
    DependPkgNameFilter.InvalidateFilter;
  finally
    sl.Free;
  end;
end;

procedure TAddPkgDependencyDialog.OKButtonClick(Sender: TObject);
var
  NewDependency: TPkgDependency;
  MinVerTest, MaxVerTest: TPkgVersion;
  MinMaxVerFlags: TPkgDependencyFlags;
  i: Integer;
begin
  MinVerTest := Nil;
  MaxVerTest := Nil;
  MinMaxVerFlags := [];
  try
    // check minimum version
    if DependMinVersionEdit.Text <> '' then
    begin
      MinVerTest := TPkgVersion.Create;
      if not MinVerTest.ReadString(DependMinVersionEdit.Text) then
      begin
        IDEMessageDialog(lisProjAddInvalidVersion,
          Format(lisProjAddTheMinimumVersionIsInvalid,
                 [DependMinVersionEdit.Text, LineEnding, LineEnding]),
          mtError,[mbCancel]);
        exit;
      end;
      MinMaxVerFlags := [pdfMinVersion];
    end;
    // check maximum version
    if DependMaxVersionEdit.Text <> '' then
    begin
      MaxVerTest := TPkgVersion.Create;
      if not MaxVerTest.ReadString(DependMaxVersionEdit.Text) then
      begin
        IDEMessageDialog(lisProjAddInvalidVersion,
          Format(lisProjAddTheMaximumVersionIsInvalid,
                 [DependMaxVersionEdit.Text, LineEnding, LineEnding]),
          mtError,[mbCancel]);
        exit;
      end;
      MinMaxVerFlags := MinMaxVerFlags + [pdfMaxVersion];
    end;

    // Add all selected packages.
    fResultDependencies := TPkgDependencyList.Create; // Will be freed by the caller.
    if DependPkgNameListBox.SelCount > 0 then
    begin
      for i := 0 to DependPkgNameListBox.Count-1 do
      begin
        if DependPkgNameListBox.Selected[i] then
        begin
          NewDependency := TPkgDependency.Create;   // Will be added to package graph.
          NewDependency.PackageName := DependPkgNameListBox.Items[i];
          if Assigned(MinVerTest) then
            NewDependency.MinVersion.Assign(MinVerTest);
          if Assigned(MaxVerTest) then
            NewDependency.MaxVersion.Assign(MaxVerTest);
          NewDependency.Flags := NewDependency.Flags + MinMaxVerFlags;
          if not CheckAddingDependency(fProjPack, NewDependency) then exit;
          fResultDependencies.Add(NewDependency);
          NewDependency := nil;
        end;
      end;
    end;
    ModalResult := mrOk;
  finally
    MinVerTest.Free;
    MaxVerTest.Free;
  end;
end;

end.

