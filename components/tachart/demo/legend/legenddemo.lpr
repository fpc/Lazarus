program legenddemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, main, TAChartLazarusPkg
  { you can add units after this };

{$IFDEF WINDOWS}{$R legenddemo.rc}{$ENDIF}

begin
  {$I legenddemo.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

