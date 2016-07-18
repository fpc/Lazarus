{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ExternHelp;

{$warn 5023 off : no warning about unused units}
interface

uses
  ExternHelpFrm, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ExternHelpFrm', @ExternHelpFrm.Register);
end;

initialization
  RegisterPackage('ExternHelp', @Register);
end.
