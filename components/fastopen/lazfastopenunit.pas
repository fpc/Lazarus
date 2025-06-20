{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazfastopenunit;

{$warn 5023 off : no warning about unused units}
interface

uses
  regfastopen, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regfastopen', @regfastopen.Register);
end;

initialization
  RegisterPackage('lazfastopenunit', @Register);
end.
