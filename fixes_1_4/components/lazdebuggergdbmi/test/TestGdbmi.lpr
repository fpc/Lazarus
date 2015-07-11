program TestGdbmi;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, CompileHelpers, TestGdbType, TestInstructionQueue,
  TestDisAss, TestBase, TestException, Testwatches, TestBreakPoint, TestGDBMIControl,
  TestEnvironment, TestArgV;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.CreateForm(TTestControlForm, TestControlForm);
  Application.Run;
end.

