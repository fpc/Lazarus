program EasyDocking;

{.$MODE Delphi}

{.$APPTYPE CONSOLE}

uses
  EasyDockMgr, Interfaces,
  Forms,
  fMain in 'fmain.pas' {EasyDockMain},
  fDockable in 'fdockable.pas' {Dockable},
  fTree in 'ftree.pas';

{.$R *.res}

begin
  Application.Title:='EasyDocking';
  Application.Initialize;
  Application.CreateForm(TEasyDockMain, EasyDockMain);
  Application.CreateForm(TDumpBox, DumpBox);
  Application.Run;
end.

