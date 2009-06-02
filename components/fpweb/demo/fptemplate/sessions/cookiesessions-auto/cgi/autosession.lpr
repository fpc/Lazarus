program autosession;

{$mode objfpc}{$H+}

uses
  fpWeb,fpCGI, webmodule;

{$IFDEF WINDOWS}{$R autosession.rc}{$ENDIF}

begin
  Application.Title:='autosession';
  Application.Initialize;
  Application.CreateForm(TFPWebModule1, FPWebModule1);
  Application.Run;
end.

