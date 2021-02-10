program TestGdbmi;

{$mode objfpc}{$H+}

uses
  TestDbgControlForm, // use first
  Interfaces, Forms, GuiTestRunner, TestGdbType, TestInstructionQueue,
  TestDisAss, TestException, Testwatches, TestBreakPoint,
  TestEnvironment, TestArgV, LazLogger;

begin
  Application.Title:='';
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

