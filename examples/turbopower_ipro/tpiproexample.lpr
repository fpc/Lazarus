program TPIProExample;

{$mode objfpc}{$H+}

uses
  //MemCheck,
  Interfaces,
  Forms, printer4lazarus, MainUnit;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

