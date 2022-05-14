program aggpasdemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  aggpaslcl, tachartaggpas, tachartlazaruspkg, Interfaces, // this includes the LCL widgetset
  Forms, Main;

{$R *.res}

begin
  Application.Title:='';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

