program miniide1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, Controls, SimpleFrm, AnchorDocking, anchordockpkg
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainIDE, MainIDE);
  DockMaster.MakeDockSite(MainIDE,[akBottom]);
  Application.Run;
end.

