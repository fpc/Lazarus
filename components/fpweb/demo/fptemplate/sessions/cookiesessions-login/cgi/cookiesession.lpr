program cookiesession;

{$mode objfpc}{$H+}

uses
  fpWeb,fpCGI, webmodule;

{$IFDEF WINDOWS}{$R cookiesession.rc}{$ENDIF}

begin
  Application.Title:='cookiesession';
  Application.Initialize;
  Application.CreateForm(TFPWebModule1, FPWebModule1);
  Application.Run;
end.

