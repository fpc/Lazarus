{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit OnlinePackageManager;

{$warn 5023 off : no warning about unused units}
interface

uses
  onlinepackagemanagerintf, opkman_mainfrm, opkman_optionsfrm, opkman_const, 
  opkman_visualtree, opkman_serializablepackages, opkman_downloader, 
  opkman_common, opkman_progressfrm, opkman_zipper, opkman_installer, 
  opkman_packagelistfrm, opkman_options, opkman_createrepositorypackagefrm, 
  opkman_categoriesfrm, opkman_packagedetailsfrm, opkman_updates, 
  opkman_createjsonforupdatesfrm, opkman_uploader, opkman_repositories, 
  opkman_createrepositoryfrm, opkman_repositorydetailsfrm, 
  opkman_addrepositorypackagefrm, opkman_intf, opkman_intf_packagelistfrm, 
  opkman_showhint, opkman_showhintbase, opkman_colorsfrm, opkman_maindm, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('onlinepackagemanagerintf', @onlinepackagemanagerintf.Register);
end;

initialization
  RegisterPackage('OnlinePackageManager', @Register);
end.
