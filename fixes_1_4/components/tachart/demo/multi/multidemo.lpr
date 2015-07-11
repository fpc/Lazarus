program multidemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, tachartlazaruspkg;

{$R *.res}

begin
  Application.Title := 'Multi-valued series demo';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

