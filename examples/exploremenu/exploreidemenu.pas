{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit exploreidemenu;

{$warn 5023 off : no warning about unused units}
interface

uses
  frmExploreMenu, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('frmExploreMenu', @frmExploreMenu.Register);
end;

initialization
  RegisterPackage('exploreidemenu', @Register);
end.
