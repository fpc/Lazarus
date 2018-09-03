{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit appforms;

{$warn 5023 off : no warning about unused units}
interface

uses
  AppForm, dbappform, regappforms, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regappforms', @regappforms.Register);
end;

initialization
  RegisterPackage('appforms', @Register);
end.
