{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lrSpreadSheetExport;

{$warn 5023 off : no warning about unused units}
interface

uses
  lrSpreadSheetExp, le_e_spreadsheet, le_e_spreadsheet_consts, 
  le_e_spreadsheet_params, LR_ExportMatrix, le_e_spreadsheet_types, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lrSpreadSheetExp', @lrSpreadSheetExp.Register);
end;

initialization
  RegisterPackage('lrSpreadSheetExport', @Register);
end.
