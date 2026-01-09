{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SynEditSpellCheckerDsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  SynSpellCheckDsgnRegister, syn_spell_options, SynSpellCheckDsgnStrings, 
  SynSpellCheckDsgnOptions, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SynSpellCheckDsgnRegister', @SynSpellCheckDsgnRegister.Register);
end;

initialization
  RegisterPackage('SynEditSpellCheckerDsgn', @Register);
end.
