program tagparam;

{$mode objfpc}{$H+}

uses
  fpWeb,fpCGI, webmodule;

{$IFDEF WINDOWS}{$R tagparam.rc}{$ENDIF}

begin
  Application.Title:='tagparam';
  Application.Initialize;
  Application.CreateForm(TFPWebModule1, FPWebModule1);
  Application.Run;
end.

