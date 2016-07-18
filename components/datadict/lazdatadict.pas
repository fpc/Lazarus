{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazdatadict;

{$warn 5023 off : no warning about unused units}
interface

uses
  frmconfprojdatadict, idedatadict, reglazdatadict, frmconfdatadict, 
  frmSelectCodeGenerator, fpcodegenerator, frmBaseConfigCodeGenerator, 
  frmgeneratedcode, ldd_consts, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('reglazdatadict', @reglazdatadict.Register);
end;

initialization
  RegisterPackage('lazdatadict', @Register);
end.
