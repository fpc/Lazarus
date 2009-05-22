program dragdropdemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, TAChartLazarusPkg
  { you can add units after this };

{$IFDEF WINDOWS}{$R dragdropdemo.rc}{$ENDIF}

begin
  Application.Title := 'Chart mouse events demo';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

