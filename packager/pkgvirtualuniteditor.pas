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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, FileUtil, PackageDefs;

type
  TEditVirtualUnitDialog = class(TForm)
    FilenameEdit: TEdit;
    UnitnameEdit: TEdit;
    FilenameLabel: TLabel;
    UnitnameLabel: TLabel;
    OkButton: TButton;
    CancelButton: TButton;
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
  Caption:='Edit virtual unit';
  OkButton.Caption:='Ok';
  CancelButton.Caption:='Cancel';
  FilenameLabel.Caption:='Filename:';
  UnitnameLabel.Caption:='Unitname:';
  UnitnameEdit.Hint:='The unitname is used when the IDE extends uses clauses.';
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
    MessageDlg('Invalid unit filename',
      'A pascal unit must have the extension .pp or .pas',
      mtError,[mbCancel],0);
    exit;
  end;
  NewFilenameOnly:=ExtractFilenameOnly(NewFilename);
  if CompareText(NewUnitName,NewFilenameOnly)<>0 then begin
    MessageDlg('Invalid unitname',
      'Unitname and Filename do not match.'#13
      +'Example: unit1.pas and Unit1',
      mtError,[mbCancel],0);
    exit;
  end;
  if (NewUnitName='') or (not IsValidIdent(NewUnitName)) then begin
    MessageDlg('Invalid unitname',
      'The unitname is not a valid pascal identifier.',
      mtError,[mbCancel],0);
    exit;
  end;
  LazPackage:=PkgFile.LazPackage;
  if LazPackage<>nil then begin
    ConflictUnit:=LazPackage.FindUnit(NewUnitName,true,PkgFile);
    if ConflictUnit<>nil then begin
      MessageDlg('Conflict found',
        'There is already an unit with this name.'#13
        +'File: '+ConflictUnit.Filename,
        mtError,[mbCancel],0);
      exit;
    end;
  end;

  // commit
  if (PkgFile.Filename<>NewFilename)
  or (PkgFile.Unitname<>NewUnitName) then begin
    PkgFile.Filename:=NewFilename;
    PkgFile.Unitname:=NewUnitName;
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
    UnitnameEdit.Text:=PkgFile.UnitName;
  end;
end;

initialization
  {$I pkgvirtualuniteditor.lrs}

end.

