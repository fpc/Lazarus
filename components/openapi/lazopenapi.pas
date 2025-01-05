{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazopenapi;

{$warn 5023 off : no warning about unused units}
interface

uses
  fraopenapisettings, frmopenapiwizard, reglazopenapi, frmopenapiproject, lazopenapictrl, lazopenapistr, 
  fraopenapiprojectsettings, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('reglazopenapi', @reglazopenapi.Register);
end;

initialization
  RegisterPackage('lazopenapi', @Register);
end.
