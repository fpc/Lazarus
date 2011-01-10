program TestGdbmi;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, CompileHelpers,
  TestGdbType,
  TestBase, TestException, Testwatches;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

