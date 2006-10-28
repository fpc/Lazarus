program TPIProExample;

{$mode objfpc}{$H+}

uses
  //MemCheck,
  Interfaces,
  Forms, MainUnit, JPEGForLazarus;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

