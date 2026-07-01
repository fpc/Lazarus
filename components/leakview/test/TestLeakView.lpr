program TestLeakView;

{$mode objfpc}{$H+}

uses
  // LCL
  Interfaces, Forms,
  // LazUtils
  LazLogger,
  // FPCUnit
  GuiTestRunner,
  // test suites
  TestParser;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

