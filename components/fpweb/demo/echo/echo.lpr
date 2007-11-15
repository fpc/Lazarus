program echo;

{$mode objfpc}{$H+}

uses
  fpWeb,fpCGI, wmecho;

begin
  Application.Initialize;
  Application.CreateForm(TEchoModule, EchoModule);
  Application.Run;
end.

