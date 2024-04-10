{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IdePackager;

{$warn 5023 off : no warning about unused units}
interface

uses
  PackageDefs, PackageLinks, PackageSystem, PkgSysBasePkgs, IdePackagerStrConsts, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('IdePackager', @Register);
end.
