unit LR_e_extreg;

{$mode objfpc}{$H+}

interface

uses
  Classes, LR_e_img, LR_e_htmldiv, LResources;

procedure Register;

implementation

{$R lr_e_extexp.res}

procedure Register;
begin
  RegisterComponents('LazReport', [TfrImageExport, TfrHtmlDivExport]);
end;

end.

