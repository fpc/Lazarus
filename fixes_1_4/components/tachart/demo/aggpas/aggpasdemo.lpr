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
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

