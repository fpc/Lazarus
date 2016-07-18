{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit InstantFPCLaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  InstantFPCRegisterLaz, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('InstantFPCRegisterLaz', @InstantFPCRegisterLaz.Register);
end;

initialization
  RegisterPackage('InstantFPCLaz', @Register);
end.
