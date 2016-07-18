{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fcllaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  RegisterFCL, db, process, simpleipc, eventlog, XMLConf, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegisterFCL', @RegisterFCL.Register);
end;

initialization
  RegisterPackage('FCL', @Register);
end.
