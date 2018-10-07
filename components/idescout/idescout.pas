{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit idescout;

{$warn 5023 off : no warning about unused units}
interface

uses
  RegIDEScout, frmscout, IDEScoutOptions, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegIDEScout', @RegIDEScout.Register);
end;

initialization
  RegisterPackage('idescout', @Register);
end.
