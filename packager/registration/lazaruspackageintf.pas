{  $Id$  }
{
 /***************************************************************************
                            lazaruspackageintf.pas
                            -------------------


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
    API for packages and registration of units and packages.
}
unit LazarusPackageIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  TRegisterProc = procedure;

  TRegisterUnitProc = procedure(const TheUnitName: string;
                                RegisterProc: TRegisterProc) of object;

var
  RegisterUnitProc: TRegisterUnitProc;
  
procedure RegisterUnit(const TheUnitName: string; RegisterProc: TRegisterProc);
procedure RegisterPackage(const ThePackageName: string;
                          RegisterProc: TRegisterProc);

implementation

type
  TRegisteredPackage = record
    Name: string;
    RegisterProc: TRegisterProc;
  end;
  PRegisteredPackage = ^TRegisteredPackage;

var
  RegisteredPackages: TList;

procedure RegisterUnit(const TheUnitName: string; RegisterProc: TRegisterProc);
begin
  RegisterUnitProc(TheUnitName,RegisterProc);
end;

procedure RegisterPackage(const ThePackageName: string;
  RegisterProc: TRegisterProc);
var
  NewRegisteredPackage: PRegisteredPackage;
begin
  if RegisteredPackages=nil then RegisteredPackages:=TList.Create;
  New(NewRegisteredPackage);
  NewRegisteredPackage^.Name:=ThePackageName;
  NewRegisteredPackage^.RegisterProc:=RegisterProc;
  RegisteredPackages.Add(NewRegisteredPackage);
end;

procedure InternalInit;
begin
  RegisterUnitProc:=nil;
  RegisteredPackages:=nil;
end;

procedure InternalFinal;
var
  RegisteredPackage: PRegisteredPackage;
  i: Integer;
begin
  if RegisteredPackages<>nil then begin
    for i:=0 to RegisteredPackages.Count-1 do begin
      RegisteredPackage:=PRegisteredPackage(RegisteredPackages[i]);
      Dispose(RegisteredPackage);
    end;
    RegisteredPackages.Free;
    RegisteredPackages:=nil;
  end;
end;

initialization
  InternalInit;

finalization
  InternalFinal;

end.

