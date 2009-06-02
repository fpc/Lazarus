program helloworld;

{$mode objfpc}{$H+}

uses
  fpWeb,fpCGI, webmodule;

{$IFDEF WINDOWS}{$R helloworld.rc}{$ENDIF}

begin
  Application.Title:='helloworld';
  Application.Initialize;
  Application.CreateForm(TFPWebModule1, FPWebModule1);
  Application.Run;
end.

