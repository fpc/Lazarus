program tsqlscriptsample;
{
This is the accompanying project for
http://wiki.lazarus.freepascal.org/SQLdb_Tutorial3

Please see that article for instructions and requirements.
(You'll need database clients and a sample database; see the article)
}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainform, dbconfiggui;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

