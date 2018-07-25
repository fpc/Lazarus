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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Defines TOpenLoadedPackagesDlg - The dialog let the user choose one of
    the loaded packages.
}
unit OpenInstalledPkgDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  Forms, Controls, ComCtrls, StdCtrls, ExtCtrls, LCLType, ButtonPanel,
  // LazUtils
  LazUtilities,
  // LazControls
  ListViewFilterEdit,
  // IdeIntf
  IDEHelpIntf, IDEWindowIntf, PackageIntf,
  // IDE
  PackageDefs, LazarusIDEStrConsts, PackageSystem;

type

  { TOpenLoadedPackagesDlg }

  TOpenLoadedPackagesDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    FilterEdit: TListViewFilterEdit;
    PkgListView: TListView;
    HintMemo: TMemo;
    Splitter1: TSplitter;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure PkgListViewDblClick(Sender: TObject);
    procedure PkgListViewKeyPress(Sender: TObject; var Key: char);
    procedure PkgListViewSelectItem(Sender: TObject; {%H-}Item: TListItem; {%H-}Selected: Boolean);
  private
    Package: TLazPackage;
  public
    procedure UpdateSelection;
    procedure UpdatePackageList;
  end;
  
function ShowOpenLoadedPkgDlg(out OpenPackage: TLazPackage): TModalResult;

implementation

{$R *.lfm}

function ShowOpenLoadedPkgDlg(out OpenPackage: TLazPackage): TModalResult;
var
  Dlg: TOpenLoadedPackagesDlg;
begin
  Dlg:=TOpenLoadedPackagesDlg.Create(nil);
  try
    Dlg.UpdatePackageList;
    Dlg.UpdateSelection;
    Result:=Dlg.ShowModal;
    OpenPackage:=Dlg.Package;
    Assert((Result=mrOK) or (OpenPackage=nil));
  finally
    Dlg.Free;
  end;
end;

{ TOpenLoadedPackagesDlg }

procedure TOpenLoadedPackagesDlg.PkgListViewDblClick(Sender: TObject);
begin
  OpenButtonClick(Sender);
end;

procedure TOpenLoadedPackagesDlg.PkgListViewKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Char(VK_RETURN) then
    OpenButtonClick(Nil);
end;

procedure TOpenLoadedPackagesDlg.PkgListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateSelection;
end;

procedure TOpenLoadedPackagesDlg.UpdateSelection;
var
  CurPkg: TLazPackage;
  LI: TListItem;
  HintStr: String;
begin
  LI:=PkgListView.Selected;
  ButtonPanel1.OKButton.Enabled:=Assigned(LI);
  if Assigned(LI) then begin
    CurPkg:=PackageGraph.FindPackageWithName(LI.Caption,nil);
    if CurPkg=nil then
      HintMemo.Text:='Package "'+LI.Caption+'" was already closed'
    else begin
      HintStr:=Format(lisOIPFilename, [CurPkg.Filename]);
      if CurPkg.Missing then
        HintStr:=Format(lisOIPThisPackageIsInstalledButTheLpkFileWasNotFound,[HintStr+LineEnding]);
      HintStr:=Format(lisOIPDescriptionDescription, [HintStr+LineEnding,
        BreakString(CurPkg.Description, 60, length(lisOIPDescription))]);
      if CurPkg.License<>'' then //use same indent as previous entry
        HintStr:=Format(lisOIPLicenseLicense, [HintStr+LineEnding,
        BreakString(CurPkg.License, 60, length(lisOIPDescription))]);
      HintMemo.Text:=HintStr;
    end;
  end else begin
    HintMemo.Text:=lisOIPPleaseSelectAPackage;
  end;
end;

procedure TOpenLoadedPackagesDlg.OpenButtonClick(Sender: TObject);
begin
  if Assigned(PkgListView.Selected) then
  begin
    Package:=PackageGraph.FindPackageWithName(PkgListView.Selected.Caption,nil);
    if Assigned(Package) then
    begin
      ModalResult:=mrOk;
      Exit;
    end;
  end;
  ModalResult:=mrCancel;
end;

procedure TOpenLoadedPackagesDlg.FormCreate(Sender: TObject);
var
  NewColumn: TListColumn;
begin
  Caption:=lisOIPOpenLoadedPackage;
  IDEDialogLayoutList.ApplyLayout(Self,450,450);

  with PkgListView do begin
    NewColumn:=Columns.Add;
    NewColumn.Caption:=lisOIPPackageName;
    NewColumn.Width:=150;
    NewColumn:=Columns.Add;
    NewColumn.Caption:=lisVersion;
    NewColumn.Width:=80;
    NewColumn:=Columns.Add;
    NewColumn.Caption:=lisOIPState;
    NewColumn.Width:=300;
  end;
  ButtonPanel1.OKButton.Caption:=lisOpen;
  ButtonPanel1.OKButton.Enabled:=False;
end;

procedure TOpenLoadedPackagesDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TOpenLoadedPackagesDlg.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

function PkgStateToString(APackage: TLazPackage): string;
  
  procedure AddState(const s: string);
  begin
    if Result='' then
      Result:=Result+s
    else
      Result:=Result+', '+s;
  end;
  
begin
  Result:='';
  if APackage.Modified then AddState(lisOIPmodified);
  if APackage.Missing then AddState(lisOIPmissing);
  case APackage.Installed of
    pitStatic: AddState(lisOIPinstalledStatic);
    pitDynamic: AddState(lisOIPinstalledDynamic);
  end;
  case APackage.AutoInstall of
    pitStatic: AddState(lisOIPautoInstallStatic);
    pitDynamic: AddState(lisOIPautoInstallDynamic);
  end;
  if APackage.ReadOnly then AddState(lisOIPreadonly);
end;

procedure TOpenLoadedPackagesDlg.UpdatePackageList;

  procedure UpdateOnePackage(aPkg: TLazPackage);
  var
    ListItem: TListViewDataItem;
  begin
    //ListItem.Initialize(3);
    ListItem.Data := Nil;
    SetLength(ListItem.StringArray, 3);
    ListItem.StringArray[0] := aPkg.Name;
    ListItem.StringArray[1] := aPkg.Version.AsString;
    ListItem.StringArray[2] := PkgStateToString(aPkg);
    FilterEdit.Items.Add(ListItem);
  end;

var
  i: Integer;
begin
  FilterEdit.Items.Clear;
  for i:=0 to PackageGraph.Count-1 do
    UpdateOnePackage(PackageGraph[i]);
  FilterEdit.InvalidateFilter;
end;

end.

