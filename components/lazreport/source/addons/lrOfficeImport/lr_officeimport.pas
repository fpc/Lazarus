{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lr_OfficeImport;

{$warn 5023 off : no warning about unused units}
interface

uses
  lrOfficeImport, lrSpreadSheetImportUnit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lrOfficeImport', @lrOfficeImport.Register);
end;

initialization
  RegisterPackage('lr_OfficeImport', @Register);
end.
