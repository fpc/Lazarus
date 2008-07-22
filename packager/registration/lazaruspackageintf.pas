{  $Id$  }
{
 /***************************************************************************
                            lazaruspackageintf.pas
                            -------------------


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

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

type
  TRegisteredPackage = record
    Name: string;
    RegisterProc: TRegisterProc;
  end;
  PRegisteredPackage = ^TRegisteredPackage;

var
  RegisteredPackages: TFPList; // list of PRegisteredPackage
  RegisterUnitProc: TRegisterUnitProc;
  
procedure RegisterUnit(const TheUnitName: string; RegisterProc: TRegisterProc);
procedure RegisterPackage(const ThePackageName: string;
                          RegisterProc: TRegisterProc);
procedure ClearRegisteredPackages;

implementation

procedure RegisterUnit(const TheUnitName: string; RegisterProc: TRegisterProc);
begin
  RegisterUnitProc(TheUnitName,RegisterProc);
end;

procedure RegisterPackage(const ThePackageName: string;
  RegisterProc: TRegisterProc);
var
  NewRegisteredPackage: PRegisteredPackage;
begin
  if RegisteredPackages=nil then RegisteredPackages:=TFPList.Create;
  New(NewRegisteredPackage);
  NewRegisteredPackage^.Name:=ThePackageName;
  NewRegisteredPackage^.RegisterProc:=RegisterProc;
  RegisteredPackages.Add(NewRegisteredPackage);
end;

procedure ClearRegisteredPackages;
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

procedure InternalInit;
begin
  RegisterUnitProc:=nil;
  RegisteredPackages:=nil;
end;

procedure InternalFinal;
begin
  ClearRegisteredPackages;
end;

initialization
  InternalInit;

finalization
  InternalFinal;

end.

