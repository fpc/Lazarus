{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit lazreportpdfexport; 

interface

uses
lr_e_pdf, lr_pdfexp_reg, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('lr_pdfexp_reg', @lr_pdfexp_reg.Register); 
end; 

initialization
  RegisterPackage('lazreportpdfexport', @Register); 
end.
