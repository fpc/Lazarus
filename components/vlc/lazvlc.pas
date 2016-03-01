{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazvlc;

{$warn 5023 off : no warning about unused units}
interface

uses
  reglazvlc, lclvlc, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('reglazvlc', @reglazvlc.Register);
end;

initialization
  RegisterPackage('lazvlc', @Register);
end.
