{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_aissist;

{$warn 5023 off : no warning about unused units}
interface

uses
  frmaissistchat, RegLazAIssist, AIssistController, StrAIssist, fraAIssistConfig, frmaixplain, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegLazAIssist', @RegLazAIssist.Register);
end;

initialization
  RegisterPackage('laz_aissist', @Register);
end.
