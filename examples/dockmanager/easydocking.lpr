program EasyDocking;

{.$MODE Delphi}

{.$APPTYPE CONSOLE}

uses
  Interfaces,
  Forms, Controls,
  fMain in 'fMain.pas' {EasyDockMain},
  fDockable in 'fDockable.pas' {Dockable},
  EasyDockSite in 'EasyDockSite.pas',
  fTree in 'fTree.pas', EasyDockHelpers {DumpBox};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TEasyDockMain, EasyDockMain);
  Application.CreateForm(TDumpBox, DumpBox);
  Application.Run;
end.

