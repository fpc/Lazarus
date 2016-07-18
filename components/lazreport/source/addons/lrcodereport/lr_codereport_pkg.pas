{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lr_codereport_pkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  LR_CodeReport, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LR_CodeReport', @LR_CodeReport.Register);
end;

initialization
  RegisterPackage('lr_codereport_pkg', @Register);
end.
