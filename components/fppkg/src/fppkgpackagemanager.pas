{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fppkgpackagemanager;

{$warn 5023 off : no warning about unused units}
interface

uses
  lazfppkgmanagerintf, fppkg_const, fppkg_details, fppkg_mainfrm, fppkg_optionsfrm, laz_pkgrepos, 
  FppkgWorkerThread, Fppkg_EnvironmentOptions, Fppkg_Interface, fppkg_packageoptionsfrm, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lazfppkgmanagerintf', @lazfppkgmanagerintf.Register);
  RegisterUnit('Fppkg_EnvironmentOptions', @Fppkg_EnvironmentOptions.Register);
end;

initialization
  RegisterPackage('fppkgpackagemanager', @Register);
end.
