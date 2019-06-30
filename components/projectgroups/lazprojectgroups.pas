{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazprojectgroups;

{$warn 5023 off : no warning about unused units}
interface

uses
  ProjectGroupIntf, ProjectGroup, ProjectGroupEditor, RegProjectGroup, 
  ProjectGroupStrConst, PrjGrpOptionsFrm, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegProjectGroup', @RegProjectGroup.Register);
end;

initialization
  RegisterPackage('lazprojectgroups', @Register);
end.
