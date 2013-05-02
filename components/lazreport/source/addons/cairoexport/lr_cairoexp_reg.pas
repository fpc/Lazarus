unit lr_cairoexp_reg;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, lr_e_cairo;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('LazReport',[TlrCairoExport]);
end;

initialization
  {$I lr_e_cairo_icon.lrs}
end.

