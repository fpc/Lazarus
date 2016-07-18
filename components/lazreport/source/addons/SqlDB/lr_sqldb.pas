{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LR_SqlDB;

{$warn 5023 off : no warning about unused units}
interface

uses
  LR_PQConnection, lr_SQLQuery, LR_IBConnection, lr_EditSQLDBParamsUnit, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LR_PQConnection', @LR_PQConnection.Register);
  RegisterUnit('LR_IBConnection', @LR_IBConnection.Register);
end;

initialization
  RegisterPackage('LR_SqlDB', @Register);
end.
