{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LR_ZeosDB;

interface

uses
  LR_DB_Zeos, LR_EditVariables, lr_EditParams, lrDBZeosConst, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LR_DB_Zeos', @LR_DB_Zeos.Register);
end;

initialization
  RegisterPackage('LR_ZeosDB', @Register);
end.
