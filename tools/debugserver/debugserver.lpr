program debugserver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmmain, LResources, frmOptions
  { you can add units after this };

{$IFDEF WINDOWS}{$R debugserver.rc}{$ENDIF}

begin
  Application.Title:='FPC/Lazarus debug message server';
  {$I debugserver.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

