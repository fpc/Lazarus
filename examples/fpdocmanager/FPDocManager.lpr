program FPDocManager;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, dGlobals, umakeskel, fMain, fConfig, uManager, fLogView,
  fUpdateView, ulpk, ConfigFile;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TLogView, LogView);
  Application.CreateForm(TUpdateView, UpdateView);
  Application.Run;
end.

