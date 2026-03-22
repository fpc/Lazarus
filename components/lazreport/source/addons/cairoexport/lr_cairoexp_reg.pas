unit lr_cairoexp_reg;

{$mode objfpc}{$H+}

interface

uses
  Classes, lr_e_cairo;

procedure register;

implementation

{$R lr_e_cairo_icon.res}

procedure register;
begin
  RegisterComponents('LazReport',[TlrCairoExport]);
end;

end.

