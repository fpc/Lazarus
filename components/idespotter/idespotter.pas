{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit idespotter;

{$warn 5023 off : no warning about unused units}
interface

uses
  regidespotter, frmspotter, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regidespotter', @regidespotter.Register);
end;

initialization
  RegisterPackage('idespotter', @Register);
end.
