{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazarusPackageManager; 

interface

uses
  LazPackageManagerIntf, fppkg_const, fppkg_details, fppkg_mainfrm, 
  fppkg_optionsfrm, laz_pkgcommands, laz_pkghandler, laz_pkgrepos, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('LazPackageManagerIntf', @LazPackageManagerIntf.Register); 
end; 

initialization
  RegisterPackage('LazarusPackageManager', @Register); 
end.
