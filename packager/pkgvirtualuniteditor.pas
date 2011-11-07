{  $Id$  }
{
 /***************************************************************************
                          pkgvirtualuniteditor.pas
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
    TEditVirtualUnitDialog is a dialog to edit the properties of a virtual unit.
}
unit PkgVirtualUnitEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, FileUtil, LazarusIDEStrConsts, PackageDefs;

type

  { TEditVirtualUnitDialog }

  TEditVirtualUnitDialog = class(TForm)
    CancelButton: TBitBtn;
    FilenameEdit: TEdit;
    OkButton: TBitBtn;
    UnitnameEdit: TEdit;
    FilenameLabel: TLabel;
    UnitnameLabel: TLabel;
    procedure EditVirtualUnitDialogCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    FPkgFile: TPkgFile;
    procedure SetPkgFile(const AValue: TPkgFile);
  public
    property PkgFile: TPkgFile read FPkgFile write SetPkgFile;
  end;

function ShowEditVirtualPackageDialog(PkgFile: TPkgFile): TModalResult;

implementation

{$R *.lfm}

function ShowEditVirtualPackageDialog(PkgFile: TPkgFile): TModalResult;
var
  EditVirtualUnitDialog: TEditVirtualUnitDialog;
begin
  EditVirtualUnitDialog:=TEditVirtualUnitDialog.Create(nil);
  try
    EditVirtualUnitDialog.PkgFile:=PkgFile;
    Result:=EditVirtualUnitDialog.ShowModal;
  finally
    EditVirtualUnitDialog.Free;
  end;
end;

{ TEditVirtualUnitDialog }

procedure TEditVirtualUnitDialog.EditVirtualUnitDialogCreate(Sender: TObject);
begin
  Caption:=lisPVUEditVirtualUnit;
  FilenameLabel.Caption:=lisPEFilename;
  UnitnameLabel.Caption:=lisPEUnitname;
  UnitnameEdit.Hint:=lisPVUTheUnitnameIsUsedWhenTheIDEExtendsUsesClauses;
  UnitnameEdit.ShowHint:=true;
end;

procedure TEditVirtualUnitDialog.OkButtonClick(Sender: TObject);
var
  NewFilename: String;
  NewUnitName: String;
  NewFilenameOnly: String;
  LazPackage: TLazPackage;
  ConflictUnit: TPkgFile;
begin
  NewFilename:=FilenameEdit.Text;
  NewUnitName:=UnitnameEdit.Text;
  if not FilenameIsPascalUnit(NewFilename) then begin
    MessageDlg(lisPEInvalidUnitFilename,
      lisPVUAPascalUnitMustHaveTheExtensionPpOrPas,
      mtError,[mbCancel],0);
    exit;
  end;
  NewFilenameOnly:=ExtractFilenameOnly(NewFilename);
  if CompareText(NewUnitName,NewFilenameOnly)<>0 then begin
    MessageDlg(lisPEInvalidUnitname,
      Format(lisPVUUnitnameAndFilenameDoNotMatchExampleUnit1PasAndUni, [#13]),
      mtError,[mbCancel],0);
    exit;
  end;
  if (NewUnitName='') or (not IsValidUnitName(NewUnitName)) then begin
    MessageDlg(lisPEInvalidUnitname,
      lisPVUTheUnitnameIsNotAValidPascalIdentifier,
      mtError,[mbCancel],0);
    exit;
  end;
  LazPackage:=PkgFile.LazPackage;
  if LazPackage<>nil then begin
    ConflictUnit:=LazPackage.FindUnit(NewUnitName,true,PkgFile);
    if ConflictUnit<>nil then begin
      MessageDlg(lisPEConflictFound,
        Format(lisPVUThereIsAlreadyAnUnitWithThisNameFile, [#13,
          ConflictUnit.Filename]),
        mtError,[mbCancel],0);
      exit;
    end;
  end;

  // commit
  if (PkgFile.Filename<>NewFilename)
  or (PkgFile.Unit_name<>NewUnitName) then begin
    PkgFile.Filename:=NewFilename;
    PkgFile.Unit_name:=NewUnitName;
    if LazPackage<>nil then LazPackage.Modified:=true;
  end;
  
  ModalResult:=mrOk;
end;

procedure TEditVirtualUnitDialog.SetPkgFile(const AValue: TPkgFile);
begin
  if FPkgFile=AValue then exit;
  FPkgFile:=AValue;
  if PkgFile<>nil then begin
    FilenameEdit.Text:=PkgFile.Filename;
    UnitnameEdit.Text:=PkgFile.Unit_Name;
  end;
end;

end.

