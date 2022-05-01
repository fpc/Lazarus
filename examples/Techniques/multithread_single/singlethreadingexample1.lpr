program SingleThreadingExample1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{xx$IFDEF UseCThreads}
  cmem, cthreads,
  {$ENDIF}{xx$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, ProcessMessagesUnit1;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

