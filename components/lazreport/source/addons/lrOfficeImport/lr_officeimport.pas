{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lr_OfficeImport;

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
