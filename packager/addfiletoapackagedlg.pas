{  $Id$  }
{
 /***************************************************************************
                         addfiletoapackagedlg.pas
                         ------------------------


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
    The dialog for selecting the package to add a file to.
}

unit AddFileToAPackageDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  Forms, Controls, StdCtrls, Dialogs, ButtonPanel,
  // LazUtils
  FileUtil, LazFileUtils,
  // IdeIntf
  IDEWindowIntf, PackageIntf, IDEHelpIntf,
  // IDE
  LazarusIDEStrConsts, PackageDefs, PackageSystem, EnvironmentOpts;

type

  { TAddFileToAPackageDialog }

  TAddFileToAPackageDialog = class(TForm)
    BtnPanel: TButtonPanel;
    FileNameEdit: TEdit;
    FileGroupBox: TGroupBox;
    PackagesGroupBox: TGroupBox;
    PackagesComboBox: TComboBox;
    ShowAllCheckBox: TCheckBox;
    procedure AddFileToAPackageDlgClose(Sender: TObject;
      var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure PackagesGroupBoxResize(Sender: TObject);
    procedure ShowAllCheckBoxClick(Sender: TObject);
  private
    fPackages: TAVLTree;// tree of TLazPackage
    function GetFilename: string;
    procedure SetFilename(const AValue: string);
    procedure SetupComponents;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateAvailablePackages;
    property Filename: string read GetFilename write SetFilename;
  end;


function ShowAddFileToAPackageDlg(const Filename: string): TModalResult;


implementation

{$R *.lfm}

function ShowAddFileToAPackageDlg(const Filename: string): TModalResult;
var
  AddFileToAPackageDialog: TAddFileToAPackageDialog;
begin
  AddFileToAPackageDialog:=TAddFileToAPackageDialog.Create(nil);
  AddFileToAPackageDialog.Filename:=Filename;
  AddFileToAPackageDialog.UpdateAvailablePackages;
  Result:=AddFileToAPackageDialog.ShowModal;
  AddFileToAPackageDialog.Free;
end;

{ TAddFileToAPackageDialog }

procedure TAddFileToAPackageDialog.AddFileToAPackageDlgClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TAddFileToAPackageDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TAddFileToAPackageDialog.OkButtonClick(Sender: TObject);
var
  PkgID: TLazPackageID;
  APackage: TLazPackage;
  aFilename: String;
  NewUnitPaths, NewIncPaths: String;
begin
  aFilename:=Filename;
  PkgID:=TLazPackageID.Create;
  try
    // check package ID
    if not PkgID.StringToID(PackagesComboBox.Text) then begin
      MessageDlg(lisAF2PInvalidPackage,
        Format(lisAF2PInvalidPackageID, [PackagesComboBox.Text]),
        mtError,[mbCancel],0);
      exit;
    end;
    // search package
    APackage:=PackageGraph.FindPackageWithID(PkgID);
    if APackage=nil then begin
      MessageDlg(lisProjAddPackageNotFound,
        Format(lisAF2PPackageNotFound, [PkgID.IDAsString]),
        mtError,[mbCancel],0);
      exit;
    end;

    // check if package is readonly
    if APackage.ReadOnly then begin
      MessageDlg(lisAF2PPackageIsReadOnly,
        Format(lisAF2PThePackageIsReadOnly, [APackage.IDAsString]),
        mtError,[mbCancel],0);
      exit;
    end;

    // ok -> add file to package
    APackage.BeginUpdate;
    NewUnitPaths:='';
    NewIncPaths:='';
    APackage.AddFileByName(aFilename, NewUnitPaths, NewIncPaths);
    // extend unit and include search path
    if not APackage.ExtendUnitSearchPath(NewUnitPaths) then exit;
    if not APackage.ExtendIncSearchPath(NewIncPaths) then exit;
    if APackage.Editor<>nil then APackage.Editor.UpdateAll(true);
    APackage.EndUpdate;

    ModalResult:=mrOk;
  finally
    PkgID.Free;
  end;
end;

procedure TAddFileToAPackageDialog.PackagesGroupBoxResize(Sender: TObject);
begin
  with ShowAllCheckBox do
    SetBounds(10,30,200,Height);
end;

procedure TAddFileToAPackageDialog.ShowAllCheckBoxClick(Sender: TObject);
begin
  UpdateAvailablePackages;
end;

procedure TAddFileToAPackageDialog.SetupComponents;
begin
  FileGroupBox.Caption:=lisFile;
  FileNameEdit.Text:='';
  PackagesGroupBox.Caption:=lisAF2PDestinationPackage;
  ShowAllCheckBox.Caption:=lisAF2PShowAll;
  BtnPanel.OkButton.Caption:=lisMenuOk;
  BtnPanel.OkButton.OnClick:=@OkButtonClick;
  BtnPanel.OkButton.ModalResult:=mrNone;
  BtnPanel.HelpButton.OnClick:=@HelpButtonClick;
end;

procedure TAddFileToAPackageDialog.SetFilename(const AValue: string);
begin
  if FileNameEdit.Text=AValue then exit;
  FileNameEdit.Text:=AValue;
end;

function TAddFileToAPackageDialog.GetFilename: string;
begin
  Result:=FileNameEdit.Text;
end;

constructor TAddFileToAPackageDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption:=lisAF2PAddFileToAPackage;
  fPackages:=TAVLTree.Create(@CompareLazPackageID);
  IDEDialogLayoutList.ApplyLayout(Self,448,280);
  SetupComponents;
end;

destructor TAddFileToAPackageDialog.Destroy;
begin
  FreeAndNil(fPackages);
  inherited Destroy;
end;

procedure TAddFileToAPackageDialog.FormCreate(Sender: TObject);
begin
  PackagesComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
end;

procedure TAddFileToAPackageDialog.UpdateAvailablePackages;
var
  i: Integer;
  APackage: TLazPackage;
  AFilename: String;
  ADirectory: String;
  sl: TStringList;
  ANode: TAVLTreeNode;
begin
  fPackages.Clear;
  AFilename:=Filename;
  ADirectory:=ExtractFilePath(Filename);
  for i:=0 to PackageGraph.Count-1 do begin
    APackage:=PackageGraph[i];
    // skip readonly packages
    if APackage.ReadOnly then continue;
    // skip packages, that already contains the file
    if APackage.FindPkgFile(AFilename,true,false)<>nil then continue;
    if not ShowAllCheckBox.Checked then begin
      // skip packages, where the filename is not in the package directory
      // or one of its source directories
      if (not FileIsInPath(AFilename,APackage.Directory))
      and (APackage.SourceDirectories.GetFileReference(ADirectory)=nil) then
        continue;
    end;
    fPackages.Add(APackage);
  end;
  sl:=TStringList.Create;
  ANode:=fPackages.FindLowest;
  while ANode<>nil do begin
    sl.Add(TLazPackage(ANode.Data).IDAsString);
    ANode:=fPackages.FindSuccessor(ANode);
  end;
  PackagesComboBox.Items.Assign(sl);
  if PackagesComboBox.Items.Count>0 then
    PackagesComboBox.Text:=PackagesComboBox.Items[0]
  else
    PackagesComboBox.Text:='';
  sl.Free;
end;

end.
