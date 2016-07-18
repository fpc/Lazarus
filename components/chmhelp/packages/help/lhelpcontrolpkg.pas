{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lhelpcontrolpkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  LHelpControl, LazHelpCHM, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazHelpCHM', @LazHelpCHM.Register);
end;

initialization
  RegisterPackage('lhelpcontrolpkg', @Register);
end.

