program TPIProExample;

{$mode objfpc}{$H+}

uses
  //MemCheck,
  Interfaces,
  Forms, MainUnit;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

