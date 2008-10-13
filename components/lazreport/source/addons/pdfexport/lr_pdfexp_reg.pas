unit lr_pdfexp_reg;

{$mode objfpc}{$H+}

interface

uses
  classes, LResources, lr_e_pdf;

  procedure register;

implementation

procedure register;
begin
  RegisterComponents('LazReport', [TfrTNPDFExport]);
end;

initialization
  {$I lr_pdfexp_reg.lrs}

end.

