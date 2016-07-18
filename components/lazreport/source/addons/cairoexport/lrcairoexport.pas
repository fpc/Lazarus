{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lrcairoexport;

{$warn 5023 off : no warning about unused units}
interface

uses
  lr_e_cairo, lr_cairoexp_reg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lr_cairoexp_reg', @lr_cairoexp_reg.Register);
end;

initialization
  RegisterPackage('lrcairoexport', @Register);
end.
