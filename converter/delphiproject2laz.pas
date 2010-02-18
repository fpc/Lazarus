{  $Id: delphiunit2laz.pas 8788 2006-02-20 23:48:13Z mattias $  }
{
 /***************************************************************************
                          delphiunit2laz.pas
                          ------------------

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
    Functions to convert delphi projects/packages to lazarus projects/packages.

    The process of converting a delphi project/package/unit to lazarus contains
    some monotone and boring work. These functions try to help here.
    Because any conversion step can fail and can need manual fix before
    continuing, the functions are written to recognize, what have been done.
    So, you can call the delphi conversion, abort at any step, fix a few things,
    and invoke it again.
}
unit DelphiProject2Laz;

{$mode objfpc}{$H+}

interface

uses ConvertDelphi,
  // LCL+FCL
  Classes, SysUtils, LCLProc, Forms, Controls, Dialogs, FileProcs, FileUtil;
  // codetools
{  ExprEval, DefineTemplates, CodeCache, CodeToolManager, CodeToolsStructs,
  LinkScanner,
  // IDEIntf
  SrcEditorIntf, ComponentReg, IDEMsgIntf, MainIntf, LazIDEIntf, PackageIntf,
  ProjectIntf,
  // IDE
  IDEProcs, DelphiUnit2Laz, Project, DialogProcs, CheckLFMDlg,
  EditorOptions, ProjectInspector, CompilerOptions, PackageDefs, PackageSystem,
  PackageEditor,
  BasePkgManager, PkgManager; }
  
const
  SettingDelphiModeTemplName = 'Setting Delphi Mode';
  
type
  TConvertDelphiToLazarusUnitFlag = (
    cdtlufRenameLowercase, // rename the unit lowercase
    cdtlufIsSubProc, // this is part of a big conversion -> add Abort button to all questions
    cdtlufCheckLFM,  // check and fix LFM
    cdtlufIgnoreUsedUnits, // skip steps that require loading used units
    cdtlufDoNotSetDelphiMode, // do not set delphi mode for project directories
    cdtlufCanAbort   // show 'Cancel all' button in error messages using mrAbort
    );
  TConvertDelphiToLazarusUnitFlags = set of TConvertDelphiToLazarusUnitFlag;

function ConvertDelphiToLazarusUnit(const DelphiFilename: string;
    Flags: TConvertDelphiToLazarusUnitFlags): TModalResult;
function ConvertDelphiToLazarusProject(const ProjectFilename: string
                                       ): TModalResult;
function ConvertDelphiToLazarusPackage(const PackageFilename: string
                                       ): TModalResult;


implementation

function ConvertDelphiToLazarusUnit(const DelphiFilename: string;
  Flags: TConvertDelphiToLazarusUnitFlags): TModalResult;
var
  Converter: TConvertDelphiUnit;
begin
  Converter := TConvertDelphiUnit.Create(nil, DelphiFilename, []);
  try
    Result:=Converter.Convert;
  finally
    Converter.Free;
  end;
end;

function ConvertDelphiToLazarusProject(const ProjectFilename: string): TModalResult;
var
  Converter: TConvertDelphiProject;
begin
  Converter := TConvertDelphiProject.Create(ProjectFilename);
  try
    Result:=Converter.Convert;
  finally
    Converter.Free;
  end;
end;

function ConvertDelphiToLazarusPackage(const PackageFilename: string): TModalResult;
var
  Converter: TConvertDelphiPackage;
begin
  Converter := TConvertDelphiPackage.Create(PackageFilename);
  try
    Result:=Converter.Convert;
  finally
    Converter.Free;
  end;
end;


end.

