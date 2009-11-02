program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fElastic, EasyDockMgr, LResources, fDockClient, fTestPanels
  { you can add units after this };

{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

begin
  {$I project1.lrs}
  Application.Initialize;
  Application.CreateForm(TDockingSite, DockingSite);
  Application.CreateForm(TDockingClient, DockingClient);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

