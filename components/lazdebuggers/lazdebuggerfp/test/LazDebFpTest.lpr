program LazDebFpTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  TestDbgControlForm, Interfaces, Forms, GuiTestRunner, LazClasses, TestVarious,
  TestAsm, TestWatches, TestBase, TestBreakPoint, TestStepping;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

