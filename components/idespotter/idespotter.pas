{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit idespotter;

{$warn 5023 off : no warning about unused units}
interface

uses
  RegIDESpotter, frmspotter, IDESPotterOptions, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegIDESpotter', @RegIDESpotter.Register);
end;

initialization
  RegisterPackage('idespotter', @Register);
end.
