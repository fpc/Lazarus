program sessiondemo;

{$mode objfpc}{$H+}

uses
  fpWeb,fpCGI, wmsession;

begin
  Application.Initialize;
  Application.CreateForm(TSessionModule, SessionModule);
  Application.Run;
end.

