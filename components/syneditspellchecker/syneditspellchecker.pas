{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SynEditSpellChecker;

{$warn 5023 off : no warning about unused units}
interface

uses
  SynSpellCheckPlugin, SynASpellDef, SynSpellDictionary, SynSpellCheckWordBreaker, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('SynEditSpellChecker', @Register);
end.
