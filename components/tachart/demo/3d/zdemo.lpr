program zdemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, main, TAChartLazarusPkg;

{$R *.res}

begin
  Application.Title:='TAChart 3D look demo';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

