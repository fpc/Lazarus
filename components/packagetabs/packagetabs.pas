{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit packagetabs;

{$warn 5023 off : no warning about unused units}
interface

uses
  packagetabs_impl, packagetabsstr, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('packagetabs_impl', @packagetabs_impl.Register);
end;

initialization
  RegisterPackage('packagetabs', @Register);
end.
