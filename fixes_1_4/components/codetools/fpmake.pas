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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Usage:
    fppkg build

  Abstract:
    Compile codetools with fpmake

  Compile with:
    fpc fpmake.pas
}
program fpmake;

{$mode objfpc}{$H+}

uses
  SysUtils, fpmkunit;

procedure AddUnitDependency(P: TPackage; T: TTarget; Filename: string);
var
  FileUnitName: String;
begin
  if Filename='' then exit;
  FileUnitName:=copy(Filename,1,length(Filename)-length(ExtractFileExt(Filename)));
  T.Dependencies.AddUnit(FileUnitName);
  P.Targets.AddUnit(Filename);
end;

procedure AddUnitDependencies(P: TPackage; T: TTarget; Filenames: array of const);
var
  i: Integer;
begin
  for i:=low(Filenames) to high(Filenames) do
    AddUnitDependency(P,T,AnsiString(Filenames[i].VAnsiString));
end;

var
  P: TPackage;
  T: TTarget;
begin
  with Installer do begin
    P:=AddPackage('codetools');
    P.Version:='1.0-0';
    P.Author:='Mattias Gaertner';
    P.License:='GPL-2 or any later';
    P.Description:='CodeTools - '
                +'tools and functions to parse, browse and edit pascal sources';
    P.OSes:=AllOSes;
    T:=P.Targets.AddUnit('allcodetoolunits.pp');
    T.Options.Add('-gl');
    T.Dependencies.AddInclude('codetools.inc');
    AddUnitDependencies(P,T,[
      'basiccodetools.pas',
      'cachecodetools.pas',
      'ccodeparsertool.pas',
      'codeatom.pas',
      'codebeautifier.pas',
      'codecache.pas',
      'codecompletiontool.pas',
      'codegraph.pas',
      'codeindex.pas',
      'codetemplatestool.pas',
      'codetoolmanager.pas',
      'codetoolmemmanager.pas',
      'codetoolsconfig.pas',
      'codetoolsstrconsts.pas',
      'codetoolsstructs.pas',
      'codetree.pas',
      'customcodetool.pas',
      'definetemplates.pas',
      'directivestree.pas',
      'directorycacher.pas',
      'eventcodetool.pas',
      'expreval.pas',
      'extractproctool.pas',
      'fileprocs.pas',
      'finddeclarationcache.pas',
      'finddeclarationtool.pas',
      'findoverloads.pas',
      'h2pastool.pas',
      'identcompletiontool.pas',
      'keywordfunclists.pas',
      'laz_dom.pas',
      'laz_xmlcfg.pas',
      'laz_xmlread.pas',
      'laz_xmlstreaming.pas',
      'laz_xmlwrite.pas',
      'lfmtrees.pas',
      'linkscanner.pas',
      'methodjumptool.pas',
      'multikeywordlisttool.pas',
      'nonpascalcodetools.pas',
      'pascalparsertool.pas',
      'pascalreadertool.pas',
      'ppucodetools.pas',
      'ppugraph.pas',
      'ppuparser.pas',
      'resourcecodetool.pas',
      'sourcechanger.pas',
      'sourcelog.pas',
      'stdcodetools.pas',
      '']);
    Run;
  end;
end.

