{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit OnlinePackageManager;

{$warn 5023 off : no warning about unused units}
interface

uses
  onlinepackagemanagerintf, opkman_mainfrm, opkman_optionsfrm, opkman_const, 
  opkman_visualtree, opkman_serializablepackages, opkman_downloader, 
  opkman_common, opkman_progressfrm, opkman_zipper, opkman_timer, 
  opkman_installer, opkman_packagelistfrm, opkman_options, 
  opkman_createrepositorypackage, opkman_categoriesfrm, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('onlinepackagemanagerintf', @onlinepackagemanagerintf.Register);
end;

initialization
  RegisterPackage('OnlinePackageManager', @Register);
end.
