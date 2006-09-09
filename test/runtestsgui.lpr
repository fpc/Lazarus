program runtestsgui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestLpi;

begin
  Application.Title:='Run Lazarus tests';
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

