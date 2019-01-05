program TestFpLldb;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} cthreads, {$ENDIF}
  TestDbgControlForm, Interfaces, Forms, GuiTestRunner, TestWatches,
  FpLldbDebugger, TestBase;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

