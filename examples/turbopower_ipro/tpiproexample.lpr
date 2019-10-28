program TPIProExample;

{$mode objfpc}{$H+}

uses
  //MemCheck,
  Interfaces,
  Forms, printer4lazarus, MainUnit;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

