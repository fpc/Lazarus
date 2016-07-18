{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ChmHelpPkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  LazChmHelp, ChmLangRef, ChmLcl, ChmProg, LazCHMHelpRegister, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazChmHelp', @LazChmHelp.Register);
  RegisterUnit('LazCHMHelpRegister', @LazCHMHelpRegister.Register);
end;

initialization
  RegisterPackage('ChmHelpPkg', @Register);
end.
