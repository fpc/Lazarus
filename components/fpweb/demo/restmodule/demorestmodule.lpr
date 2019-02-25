program demorestmodule;

{$mode objfpc}{$H+}

uses
  fphttpapp, dmmyrest, pqconnection;

begin
  Application.Title:='httpproject1';
  Application.Port:=8080;
  Application.Initialize;
  Application.Run;
end.

