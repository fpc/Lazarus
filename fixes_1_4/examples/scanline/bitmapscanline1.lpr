program BitmapScanLine1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, Unit1;

begin
  Application.Title:='project1';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

