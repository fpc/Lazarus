{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ProjTemplates;

{$warn 5023 off : no warning about unused units}
interface

uses
  ProjectTemplates, IDETemplateProject, frmTemplateVariables, ptstrconst, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IDETemplateProject', @IDETemplateProject.Register);
end;

initialization
  RegisterPackage('ProjTemplates', @Register);
end.
