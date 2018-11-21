program TestFpGdbmi;

{$mode objfpc}{$H+}

uses
  TestDbgControlForm,
  Interfaces, Forms, GuiTestRunner, TestWatches, FpGdbmiDebugger, TestBase;

{$R *.res}

begin
  TestBase.TestGdbClass := TFpGDBMIDebugger;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

