{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit fpcunitide;

interface

uses
  FPCUnitLazIDEIntf, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FPCUnitLazIDEIntf',@FPCUnitLazIDEIntf.Register);
end;

initialization
  RegisterPackage('fpcunitide',@Register);
end.
