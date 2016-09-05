{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lr_pdfexport;

{$warn 5023 off : no warning about unused units}
interface

uses
  lrPDFExport, lr_e_fclpdf, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lrPDFExport', @lrPDFExport.Register);
end;

initialization
  RegisterPackage('lr_pdfexport', @Register);
end.
