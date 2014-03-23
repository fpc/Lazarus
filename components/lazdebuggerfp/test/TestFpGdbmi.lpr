program TestFpGdbmi;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestGDBMIControl, TestWatches, FpGdbmiDebugger, TestBase;

{$R *.res}

begin
  TestBase.TestGdbClass := TFpGDBMIDebugger;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.CreateForm(TTestControlForm, TestControlForm);
  Application.Run;
end.

