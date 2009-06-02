program urlsession;

{$mode objfpc}{$H+}

uses
  fpWeb,fpCGI, websession;

{$IFDEF WINDOWS}{$R urlsession.rc}{$ENDIF}

begin
  Application.Title:='urlsession';
  Application.Initialize;
  Application.CreateForm(TFPWebModule1, FPWebModule1);
  Application.Run;
end.

