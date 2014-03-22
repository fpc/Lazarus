program TestFpGdbmi;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestGDBMIControl, TestWatches;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.CreateForm(TTestControlForm, TestControlForm);
  Application.Run;
end.

