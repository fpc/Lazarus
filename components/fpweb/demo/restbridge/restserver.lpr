program restserver;

{$mode objfpc}{$H+}

uses
  fphttpapp, dmRestBridge;

begin
  Application.CreateForm(TRestDataModule,RestDataModule);
  Application.Title:='restserver';
  Application.Port:=8080;
  Application.Initialize;
  Application.Run;
end.

