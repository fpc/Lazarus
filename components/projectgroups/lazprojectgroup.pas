{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazprojectgroup;

interface

uses
  projectgroupintf, projectgroup, projectgroupeditor, regprojectgroup, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regprojectgroup', @regprojectgroup.Register);
end;

initialization
  RegisterPackage('lazprojectgroup', @Register);
end.
