{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lr_extexp;

{$warn 5023 off : no warning about unused units}
interface

uses
  LR_e_img, LR_e_htmldiv, LR_e_extreg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LR_e_extreg', @LR_e_extreg.Register);
end;

initialization
  RegisterPackage('lr_extexp', @Register);
end.
