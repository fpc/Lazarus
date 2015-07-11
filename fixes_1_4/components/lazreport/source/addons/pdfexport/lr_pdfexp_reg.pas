unit lr_pdfexp_reg;

{$mode objfpc}{$H+}

interface

uses
  classes, LResources, lr_e_pdf;

  procedure register;

implementation

{$R lr_pdfexp_reg.res}

procedure register;
begin
  RegisterComponents('LazReport', [TfrTNPDFExport]);
end;

end.

