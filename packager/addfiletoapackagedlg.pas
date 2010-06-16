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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
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
  Classes, SysUtils, Forms, Controls, Buttons, ExtCtrls, StdCtrls,
  Dialogs, AVL_Tree, FileUtil, ButtonPanel,
  IDEWindowIntf, PackageIntf,
  LazarusIDEStrConsts, IDEProcs,
  ComponentReg, PackageDefs, PackageSystem, IDEContextHelpEdit;

type

  { TAddFileToAPackageDialog }

  TAddFileToAPackageDialog = class(TForm)
    BtnPanel: TButtonPanel;
    HasRegisterProcCheckBox: TCheckBox;
    FileTypeRadioGroup: TRadioGroup;
    UnitNameEdit: TEdit;
    FileNameEdit: TEdit;
    FileGroupBox: TGroupBox;
    PackagesGroupBox: TGroupBox;
    UnitNameLabel: TLabel;
    PackagesComboBox: TComboBox;
    ShowAllCheckBox: TCheckBox;
    procedure AddFileToAPackageDlgClose(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure HelpButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure PackagesGroupBoxResize(Sender: TObject);
    procedure ShowAllCheckBoxClick(Sender: TObject);
  private
    fPackages: TAVLTree;// tree of TLazPackage
    function GetFileType: TPkgFileType;
    function GetFilename: string;
    function GetHasRegisterProc: boolean;
    function GetUnitName: string;
    procedure SetFileType(const AValue: TPkgFileType);
    procedure SetFilename(const AValue: string);
    procedure SetHasRegisterProc(const AValue: boolean);
    procedure SetUnitName(const AValue: string);
    procedure SetupComponents;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateAvailablePackages;
    property Filename: string read GetFilename write SetFilename;
    property Unit_Name: string read GetUnitName write SetUnitName;
    property FileType: TPkgFileType read GetFileType write SetFileType;
    property HasRegisterProc: boolean read GetHasRegisterProc write SetHasRegisterProc;
  end;


function ShowAddFileToAPackageDlg(const Filename, AUnitName: string;
  HasRegisterProc: boolean): TModalResult;


implementation

{$R *.lfm}

function ShowAddFileToAPackageDlg(const Filename, AUnitName: string;
  HasRegisterProc: boolean): TModalResult;
var
  AddFileToAPackageDialog: TAddFileToAPackageDialog;
begin
  AddFileToAPackageDialog:=TAddFileToAPackageDialog.Create(nil);
  AddFileToAPackageDialog.Filename:=Filename;
  AddFileToAPackageDialog.Unit_Name:=AUnitName;
  AddFileToAPackageDialog.HasRegisterProc:=HasRegisterProc;
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
  ShowContextHelpForIDE(Self);
end;

procedure TAddFileToAPackageDialog.OkButtonClick(Sender: TObject);
var
  PkgID: TLazPackageID;
  APackage: TLazPackage;
  PkgFile: TPkgFile;
  FileFlags: TPkgFileFlags;
begin
  PkgID:=TLazPackageID.Create;
  try
    // check package ID
    if not PkgID.StringToID(PackagesComboBox.Text) then begin
      MessageDlg(lisAF2PInvalidPackage,
        Format(lisAF2PInvalidPackageID, ['"', PackagesComboBox.Text, '"']),
        mtError,[mbCancel],0);
      exit;
    end;
    // search package
    APackage:=PackageGraph.FindPackageWithID(PkgID);
    if APackage=nil then begin
      MessageDlg(lisProjAddPackageNotFound,
        Format(lisAF2PPackageNotFound, ['"', PkgID.IDAsString, '"']),
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

    // check if file is already in the package
    PkgFile:=APackage.FindPkgFile(Filename,true,false);
    if PkgFile<>nil then begin
      MessageDlg(lisPkgMangFileIsAlreadyInPackage,
        Format(lisAF2PTheFileIsAlreadyInThePackage, ['"', Filename, '"', #13,
          APackage.IDAsString]),
        mtError,[mbCancel],0);
      exit;
    end;

    // ok -> add file to package
    APackage.BeginUpdate;
    FileFlags:=[];
    if FileType in PkgFileUnitTypes then
      Include(FileFlags,pffAddToPkgUsesSection);
    if HasRegisterProc then
      Include(FileFlags,pffHasRegisterProc);
    APackage.AddFile(Filename,Unit_Name,FileType,FileFlags,cpNormal);
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
var
  pft: TPkgFileType;
begin
  FileGroupBox.Caption:=dlgFoldDiffFile;
  FileNameEdit.Text:='';
  UnitNameLabel.Caption:=lisAF2PUnitName;
  UnitNameEdit.Text:='';
  HasRegisterProcCheckBox.Caption:=lisAF2PHasRegisterProcedure;
  PackagesGroupBox.Caption:=lisAF2PDestinationPackage;
  ShowAllCheckBox.Caption:=lisAF2PShowAll;
  BtnPanel.OkButton.Caption:=lisLazBuildOk;
  BtnPanel.OkButton.OnClick:=@OkButtonClick;
  BtnPanel.OkButton.ModalResult:=mrNone;
  BtnPanel.HelpButton.OnClick:=@HelpButtonClick;

  with FileTypeRadioGroup do begin
    Caption:=lisAF2PFileType;
    with Items do begin
      BeginUpdate;
      for pft:=Low(TPkgFileType) to High(TPkgFileType) do begin
        if pft in PkgFileUnitTypes then continue;
        Add(GetPkgFileTypeLocalizedName(pft));
      end;
      EndUpdate;
    end;
    ItemIndex:=0;
    Columns:=2;
  end;
end;

procedure TAddFileToAPackageDialog.SetFilename(const AValue: string);
var
  NewPFT: TPkgFileType;
begin
  if FileNameEdit.Text=AValue then exit;
  FileNameEdit.Text:=AValue;
  if FilenameIsPascalUnit(AValue) then
    NewPFT:=pftUnit
  else if CompareFileExt(AValue,'.lfm',true)=0 then
    NewPFT:=pftLFM
  else if CompareFileExt(AValue,'.lrs',true)=0 then
    NewPFT:=pftLRS
  else if CompareFileExt(AValue,'.inc',true)=0 then
    NewPFT:=pftInclude
  else if FileIsText(AValue) then
    NewPFT:=pftText
  else
    NewPFT:=pftBinary;
  FileType:=NewPFT;
end;

procedure TAddFileToAPackageDialog.SetHasRegisterProc(const AValue: boolean);
begin
  if HasRegisterProc=AValue then exit;
  HasRegisterProcCheckBox.Checked:=AValue;
end;

procedure TAddFileToAPackageDialog.SetUnitName(const AValue: string);
begin
  if Unit_Name=AValue then exit;
  UnitNameEdit.Text:=AValue;
end;

function TAddFileToAPackageDialog.GetFilename: string;
begin
  Result:=FileNameEdit.Text;
end;

function TAddFileToAPackageDialog.GetFileType: TPkgFileType;
var
  i: Integer;
  CurPFT: TPkgFileType;
begin
  if FileTypeRadioGroup.Visible then begin
    i:=0;
    for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
      if CurPFT in PkgFileUnitTypes then continue;
      if FileTypeRadioGroup.ItemIndex=i then begin
        Result:=CurPFT;
        exit;
      end;
      inc(i);
    end;
    Result:=pftText;
  end else begin
    Result:=pftUnit;
  end;
end;

function TAddFileToAPackageDialog.GetHasRegisterProc: boolean;
begin
  Result:=HasRegisterProcCheckBox.Checked;
end;

function TAddFileToAPackageDialog.GetUnitName: string;
begin
  Result:=UnitNameEdit.Text;
end;

procedure TAddFileToAPackageDialog.SetFileType(const AValue: TPkgFileType);
var
  ShowUnitProps: Boolean;
  i: Integer;
  CurPFT: TPkgFileType;
begin
  if FileType=AValue then exit;
  i:=0;
  for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
    if CurPFT in PkgFileUnitTypes then continue;
    if CurPFT=AValue then break;
    inc(i);
  end;
  if i<FileTypeRadioGroup.Items.Count then
    FileTypeRadioGroup.ItemIndex:=i
  else
    FileTypeRadioGroup.ItemIndex:=-1;

  ShowUnitProps:=(AValue in PkgFileUnitTypes);
  UnitNameLabel.Visible:=ShowUnitProps;
  UnitNameEdit.Visible:=ShowUnitProps;
  HasRegisterProcCheckBox.Visible:=ShowUnitProps;
  FileTypeRadioGroup.Visible:=not ShowUnitProps;
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
  fPackages.Free;
  inherited Destroy;
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
