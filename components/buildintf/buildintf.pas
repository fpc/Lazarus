{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit BuildIntf;

{$warn 5023 off : no warning about unused units}
interface

uses
  BaseIDEIntf, BuildStrConsts, CompOptsIntf, FppkgIntf, IDEExternToolIntf, 
  IDEOptionsIntf, LazMsgDialogs, MacroDefIntf, MacroIntf, NewItemIntf, 
  PackageDependencyIntf, PackageIntf, PackageLinkIntf, ProjectIntf, 
  ProjectResourcesIntf, ProjPackIntf, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('BuildIntf', @Register);
end.
