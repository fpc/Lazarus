{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IdeProject;

{$warn 5023 off : no warning about unused units}
interface

uses
  BuildManager, IdeBookmark, IdeProjectStrConsts, Project, ProjectDefs, 
  ProjectIcon, ProjectResources, ProjectUserResources, RunParamOptions, 
  W32Manifest, W32VersionInfo, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('IdeProject', @Register);
end.
