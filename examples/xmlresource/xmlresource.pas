{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit xmlresource;

{$warn 5023 off : no warning about unused units}
interface

uses
  xmlresourcefile, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('xmlresourcefile', @xmlresourcefile.Register);
end;

initialization
  RegisterPackage('xmlresource', @Register);
end.
