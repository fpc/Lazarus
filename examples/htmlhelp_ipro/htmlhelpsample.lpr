program HTMLHelpSample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, HtmlHelp2Unit1, TurboPowerIPro, HtmlHelp2Viewer;

begin
  Application.Title:='htmlhelp1';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(THelpViewerForm, HelpViewerForm);
  Application.Run;
end.

