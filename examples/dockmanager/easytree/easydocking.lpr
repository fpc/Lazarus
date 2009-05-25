program EasyDocking;

{.$MODE Delphi}

{.$APPTYPE CONSOLE}

uses
  Interfaces,
  Forms,
  fMain in 'fmain.pas' {EasyDockMain},
  fDockable in 'fdockable.pas' {Dockable},
  EasyDockSite in 'easydocksite.pas',
  fTree in 'ftree.pas', fdockbook;

{.$R *.res}

begin
  Application.Title:='EasyDocking';
  Application.Initialize;
  Application.CreateForm(TEasyDockMain, EasyDockMain);
  Application.CreateForm(TDumpBox, DumpBox);
  Application.CreateForm(TEasyDockBook, EasyDockBook);
  Application.Run;
end.

