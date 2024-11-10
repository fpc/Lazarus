program FpTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestHelperClasses,
  TestDwarfVarious, TestMemManager, TestPascalParser,
  TestErrorHandler, TestLineMap;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

