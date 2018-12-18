program TestGdbmi;

{$mode objfpc}{$H+}

uses
  TestDbgControlForm, // use first
  Interfaces, Forms, GuiTestRunner, TestGdbType, TestInstructionQueue,
  TestDisAss, TestException, Testwatches, TestBreakPoint,
  TestEnvironment, TestArgV, LazLogger;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

