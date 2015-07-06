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
  Classes, SysUtils, Forms, Controls, Dialogs,
  PackageDefs, FileUtil, LazFileUtils, LazarusIDEStrConsts;

function ShowEditVirtualPackageDialog(PkgFile: TPkgFile): TModalResult;

implementation

type
  TDummyForClose = class
  public
    PkgFile: TPkgFile;
    procedure CloseEvent(Sender: TObject; const AValues: array of string;
      var ACanClose: boolean);
  end;

function ShowEditVirtualPackageDialog(PkgFile: TPkgFile): TModalResult;
var
  Str: array of string;
  Dummy: TDummyForClose;
begin
  Result:= mrCancel;
  if not Assigned(PkgFile) then exit;

  SetLength(Str, 2);
  Str[0]:= PkgFile.Filename;
  Str[1]:= PkgFile.Unit_Name;

  Dummy:= TDummyForClose.Create;
  try
    Dummy.PkgFile:= PkgFile;
    if not InputQuery(lisPVUEditVirtualUnit,
      [lisPEFilename, lisPEUnitname], Str, @Dummy.CloseEvent) then exit;
  finally
    FreeAndNil(Dummy);
  end;

  if (PkgFile.Filename=Str[0]) and
     (PkgFile.Unit_name=Str[1]) then exit;

  PkgFile.Filename:= Str[0];
  PkgFile.Unit_name:= Str[1];
  if Assigned(PkgFile.LazPackage) then
    PkgFile.LazPackage.Modified:= true;
  Result:= mrOk;
end;

{ TDummyForClose }

procedure TDummyForClose.CloseEvent(Sender: TObject; const AValues: array of string;
  var ACanClose: boolean);
var
  NewFilename: String;
  NewUnitName: String;
  NewFilenameOnly: String;
  LazPackage: TLazPackage;
  ConflictUnit: TPkgFile;
begin
  ACanClose:=false;
  NewFilename:=AValues[0];
  NewUnitName:=AValues[1];

  if not FilenameIsPascalUnit(NewFilename) then begin
    MessageDlg(lisPEInvalidUnitFilename,
      lisPVUAPascalUnitMustHaveTheExtensionPpOrPas,
      mtError,[mbCancel],0);
    exit;
  end;
  NewFilenameOnly:=ExtractFilenameOnly(NewFilename);
  if CompareText(NewUnitName,NewFilenameOnly)<>0 then begin
    MessageDlg(lisPEInvalidUnitname,
      Format(lisPVUUnitnameAndFilenameDoNotMatchExampleUnit1PasAndUni, [LineEnding]),
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
        Format(lisPVUThereIsAlreadyAnUnitWithThisNameFile, [LineEnding,
          ConflictUnit.Filename]),
        mtError,[mbCancel],0);
      exit;
    end;
  end;

  ACanClose:=true;
end;

end.

