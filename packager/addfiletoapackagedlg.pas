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
  LazarusIDEStrConsts, Dialogs, AVL_Tree, FileUtil, IDEProcs, IDEOptionDefs,
  ComponentReg, PackageDefs, PackageSystem;
  
type
  TAddFileToAPackageDlg = class(TForm)
    FileGroupBox: TGroupBox;
    FileNameEdit: TEdit;
    UnitNameLabel: TLabel;
    UnitNameEdit: TEdit;
    HasRegisterProcCheckBox: TCheckBox;
    FileTypeRadioGroup: TRadioGroup;
    PackagesGroupBox: TGroupBox;
    PackagesComboBox: TComboBox;
    ShowAllCheckBox: TCheckBox;
    OkButton: TButton;
    CancelButton: TButton;
    procedure AddFileToAPackageDlgClose(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure AddFileToAPackageDlgResize(Sender: TObject);
    procedure FileGroupBoxResize(Sender: TObject);
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
    property UnitName: string read GetUnitName write SetUnitName;
    property FileType: TPkgFileType read GetFileType write SetFileType;
    property HasRegisterProc: boolean read GetHasRegisterProc write SetHasRegisterProc;
  end;
  
function ShowAddFileToAPackageDlg(const Filename, UnitName: string;
  HasRegisterProc: boolean): TModalResult;

implementation

function ShowAddFileToAPackageDlg(const Filename, UnitName: string;
  HasRegisterProc: boolean): TModalResult;
var
  Dialog: TAddFileToAPackageDlg;
begin
  Dialog:=TAddFileToAPackageDlg.Create(Application);
  Dialog.Filename:=Filename;
  Dialog.UnitName:=UnitName;
  Dialog.HasRegisterProc:=HasRegisterProc;
  Dialog.UpdateAvailablePackages;
  Result:=Dialog.ShowModal;
  Dialog.Free;
end;

{ TAddFileToAPackageDlg }

procedure TAddFileToAPackageDlg.AddFileToAPackageDlgClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TAddFileToAPackageDlg.AddFileToAPackageDlgResize(Sender: TObject);
begin
  with FileGroupBox do
    SetBounds(10,10,Parent.ClientWidth-20,120);

  with PackagesGroupBox do
    SetBounds(10,FileGroupBox.Top+FileGroupBox.Height+5,Parent.ClientWidth-20,75);

  with OkButton do
    SetBounds(Parent.ClientWidth-200,Parent.ClientHeight-30,80,Height);

  with CancelButton do
    SetBounds(Parent.ClientWidth-100,Parent.ClientHeight-30,80,Height);
end;

procedure TAddFileToAPackageDlg.FileGroupBoxResize(Sender: TObject);
begin
  with UnitNameLabel do
    SetBounds(5,33,90,Height);
  with UnitNameEdit do
    SetBounds(UnitNameLabel.Left+UnitNameLabel.Width+3,30,250,Height);
  with HasRegisterProcCheckBox do
    SetBounds(5,60,200,Height);
    
  with FileTypeRadioGroup do
    SetBounds(0,30,Parent.ClientWidth,Parent.ClientHeight-30);
end;

procedure TAddFileToAPackageDlg.OkButtonClick(Sender: TObject);
var
  PkgID: TLazPackageID;
  APackage: TLazPackage;
  PkgFile: TPkgFile;
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
    PkgFile:=APackage.FindPkgFile(Filename,false,true);
    if PkgFile<>nil then begin
      MessageDlg(lisPkgMangFileIsAlreadyInPackage,
        Format(lisAF2PTheFileIsAlreadyInThePackage, ['"', Filename, '"', #13,
          APackage.IDAsString]),
        mtError,[mbCancel],0);
      exit;
    end;

    // ok -> add file to package
    APackage.BeginUpdate;
    APackage.AddFile(Filename,UnitName,FileType,[],cpNormal);
    if APackage.Editor<>nil then APackage.Editor.UpdateAll;
    APackage.EndUpdate;
    
    ModalResult:=mrOk;
  finally
    PkgID.Free;
  end;
end;

procedure TAddFileToAPackageDlg.PackagesGroupBoxResize(Sender: TObject);
begin
  with ShowAllCheckBox do
    SetBounds(10,30,200,Height);
end;

procedure TAddFileToAPackageDlg.ShowAllCheckBoxClick(Sender: TObject);
begin
  UpdateAvailablePackages;
end;

procedure TAddFileToAPackageDlg.SetupComponents;
var
  pft: TPkgFileType;
begin
  FileGroupBox:=TGroupBox.Create(Self);
  with FileGroupBox do begin
    Name:='FileGroupBox';
    Parent:=Self;
    Caption:=lisToDoLFile;
    OnResize:=@FileGroupBoxResize;
  end;

  FileNameEdit:=TEdit.Create(Self);
  with FileNameEdit do begin
    Name:='FileNameEdit';
    Parent:=FileGroupBox;
    Text:='';
    Align:=alTop;
    ReadOnly:=true;
  end;
  
  UnitNameLabel:=TLabel.Create(Self);
  with UnitNameLabel do begin
    Name:='UnitNameLabel';
    Parent:=FileGroupBox;
    Caption:=lisAF2PUnitName;
  end;
  
  UnitNameEdit:=TEdit.Create(Self);
  with UnitNameEdit do begin
    Name:='UnitNameEdit';
    Parent:=FileGroupBox;
    Text:='';
  end;
  
  HasRegisterProcCheckBox:=TCheckBox.Create(Self);
  with HasRegisterProcCheckBox do begin
    Name:='HasRegisterProcCheckBox';
    Parent:=FileGroupBox;
    Caption:=lisAF2PHasRegisterProcedure;
  end;
  
  FileTypeRadioGroup:=TRadioGroup.Create(Self);
  with FileTypeRadioGroup do begin
    Name:='FileTypeRadioGroup';
    Parent:=FileGroupBox;
    Caption:=lisAF2PFileType;
    with Items do begin
      BeginUpdate;
      for pft:=Low(TPkgFileType) to High(TPkgFileType) do begin
        if pft in [pftUnit,pftVirtualUnit] then continue;
        Add(GetPkgFileTypeLocalizedName(pft));
      end;
      EndUpdate;
    end;
    ItemIndex:=0;
    Columns:=2;
  end;

  PackagesGroupBox:=TGroupBox.Create(Self);
  with PackagesGroupBox do begin
    Name:='PackagesGroupBox';
    Parent:=Self;
    Caption:=lisAF2PDestinationPackage;
    OnResize:=@PackagesGroupBoxResize;
  end;
  
  PackagesComboBox:=TComboBox.Create(Self);
  with PackagesComboBox do begin
    Name:='PackagesComboBox';
    Parent:=PackagesGroupBox;
    Align:=alTop;
  end;
  
  ShowAllCheckBox:=TCheckBox.Create(Self);
  with ShowAllCheckBox do begin
    Name:='ShowAllCheckBox';
    Parent:=PackagesGroupBox;
    Caption:=lisAF2PShowAll;
    Checked:=false;
    OnClick:=@ShowAllCheckBoxClick;
  end;
  
  OkButton:=TButton.Create(Self);
  with OkButton do begin
    Name:='OkButton';
    Parent:=Self;
    Caption:=lisLazBuildOk;
    OnClick:=@OkButtonClick;
  end;
  
  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Name:='CancelButton';
    Parent:=Self;
    Caption:=dlgCancel;
    ModalResult:=mrCancel;
  end;
end;

procedure TAddFileToAPackageDlg.SetFilename(const AValue: string);
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

procedure TAddFileToAPackageDlg.SetHasRegisterProc(const AValue: boolean);
begin
  if HasRegisterProc=AValue then exit;
  HasRegisterProcCheckBox.Checked:=AValue;
end;

procedure TAddFileToAPackageDlg.SetUnitName(const AValue: string);
begin
  if UnitName=AValue then exit;
  UnitNameEdit.Text:=AValue;
end;

function TAddFileToAPackageDlg.GetFilename: string;
begin
  Result:=FileNameEdit.Text;
end;

function TAddFileToAPackageDlg.GetFileType: TPkgFileType;
var
  i: Integer;
  CurPFT: TPkgFileType;
begin
  if FileTypeRadioGroup.Visible then begin
    i:=0;
    for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
      if CurPFT in [pftUnit,pftVirtualUnit] then continue;
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

function TAddFileToAPackageDlg.GetHasRegisterProc: boolean;
begin
  Result:=HasRegisterProcCheckBox.Checked;
end;

function TAddFileToAPackageDlg.GetUnitName: string;
begin
  Result:=UnitNameEdit.Text;
end;

procedure TAddFileToAPackageDlg.SetFileType(const AValue: TPkgFileType);
var
  ShowUnitProps: Boolean;
  i: Integer;
  CurPFT: TPkgFileType;
begin
  if FileType=AValue then exit;
  i:=0;
  for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
    if CurPFT in [pftUnit,pftVirtualUnit] then continue;
    if CurPFT=AValue then break;
    inc(i);
  end;
  if i<FileTypeRadioGroup.Items.Count then
    FileTypeRadioGroup.ItemIndex:=i
  else
    FileTypeRadioGroup.ItemIndex:=-1;

  ShowUnitProps:=(AValue in [pftUnit,pftVirtualUnit]);
  UnitNameLabel.Visible:=ShowUnitProps;
  UnitNameEdit.Visible:=ShowUnitProps;
  HasRegisterProcCheckBox.Visible:=ShowUnitProps;
  FileTypeRadioGroup.Visible:=not ShowUnitProps;
end;

constructor TAddFileToAPackageDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name:='AddFileToAPackageDlg';
  Caption:=lisAF2PAddFileToAPackage;
  fPackages:=TAVLTree.Create(@CompareLazPackageID);
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,320,170);
  SetupComponents;
  OnClose:=@AddFileToAPackageDlgClose;
  OnResize:=@AddFileToAPackageDlgResize;
  OnResize(Self);
end;

destructor TAddFileToAPackageDlg.Destroy;
begin
  fPackages.Free;
  inherited Destroy;
end;

procedure TAddFileToAPackageDlg.UpdateAvailablePackages;
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
    if APackage.FindPkgFile(AFilename,false,true)<>nil then continue;
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

