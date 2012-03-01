program WikiSearchDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, WikiSearchMain, WikiHelpManager
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='WikiSearchDemo';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TWikiSearchDemoForm, WikiSearchDemoForm);
  Application.Run;
end.

