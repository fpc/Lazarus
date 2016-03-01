{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lrEmailExport;

{$warn 5023 off : no warning about unused units}
interface

uses
  lrEmailExportFilter, lrEmailExportFilterSetup, lrEmailAppFreeSoft, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lrEmailExportFilter', @lrEmailExportFilter.Register);
end;

initialization
  RegisterPackage('lrEmailExport', @Register);
end.
