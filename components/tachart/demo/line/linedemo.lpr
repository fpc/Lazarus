program linedemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main;

{$R *.res}

begin
  Application.Scaled := True;
  Application.Title := 'TAChart LineSeries Demo';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

