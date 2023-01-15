{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit charactermap_ide_pkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  charactermap_reg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('charactermap_reg', @charactermap_reg.Register);
end;

initialization
  RegisterPackage('charactermap_ide_pkg', @Register);
end.
