program ADRestoreDebugger;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit, anchordockpkg, adlayoutviewer
  { you can add units after this };

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TADRestDbg, ADRestDbg);
  Application.Run;
end.

