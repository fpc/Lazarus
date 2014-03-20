program TestGdbmi;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, CompileHelpers, TestGdbType, TestInstructionQueue,
  TestDisAss, TestGDBMIControl, TestBase, TestException, Testwatches, TestBreakPoint,
  TestEnvironment, TestArgV;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.CreateForm(TTestControlForm, TestControlForm);
  Application.Run;
end.

