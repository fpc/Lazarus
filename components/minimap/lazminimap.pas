{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazminimap;

{$warn 5023 off : no warning about unused units}
interface

uses
  reglazminimap, pnlminimap, framinimapconfig, strminimap, ctrlminimap, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('reglazminimap', @reglazminimap.Register);
end;

initialization
  RegisterPackage('lazminimap', @Register);
end.
