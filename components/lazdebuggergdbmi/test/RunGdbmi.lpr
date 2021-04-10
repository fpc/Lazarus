program RunGdbmi;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  TestDbgControlForm, // use first
  Interfaces, // this includes the LCL widgetset
  Forms, fpcunittestrunner, RunGdbmiForm
  { you can add units after this };

{$R *.res}

begin
  Application.Title := '';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

