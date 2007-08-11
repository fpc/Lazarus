program confcleandirs;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here },
  sysutils,
  dircleaner, frmmain, frmlog;
{$ifdef win32}
{$R manifest.res}
{$R confcleandirs.res}
{$endif}
begin
  Application.Title:='Configure directory cleaner';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

