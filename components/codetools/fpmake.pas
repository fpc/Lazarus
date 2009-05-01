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
  SysUtils, fpmkunit2;

procedure AddDependencies(T: TTarget);
var
  Info: TSearchRec;
  Ext: String;
begin
  if FindFirst('*',faAnyFile,Info)=0 then
  begin
    repeat
      if (Info.Name<>T.Name) then begin
        Ext:=ExtractFileExt(Info.Name);
        if (Ext='.pas') or (Ext='.pp') then
          //T.Dependencies.AddUnit(copy(Info.Name,1,length(Info.Name)-length(Ext)))
          //writeln(copy(Info.Name,1,length(Info.Name)-length(Ext)))
        else if Ext='.inc' then
          T.Dependencies.AddInclude(Info.Name)
        else if Ext='.lpk' then
          T.Dependencies.Add(Info.Name);
      end;
    until FindNext(Info)<>0;
    writeln();
  end;
  FindClose(Info);
end;

var
  P: TPackage;
  T: TTarget;
begin
  with Installer do begin
    P:=AddPackage('codetools');
    P.OSes:=AllOSes;
    T:=P.Targets.AddUnit('allcodetoolunits.pp');
    T.Options:='-gl';
    // AddUnit gives error: Unknown target in dependencies for allcodetoolunits: keywordfunclists
    //T.Dependencies.AddUnit('keywordfunclists');
    AddDependencies(T);
    Run;
  end;
end.

