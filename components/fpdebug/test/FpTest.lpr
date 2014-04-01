program FpTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestTypeInfo, TestHelperClasses, TestDwarfSetup1,
  TestDwarfSetupBasic, TestDwarfVarious, testdwarfsetupArray, TestMemManager, TestPascalParser,
  TestErrorHandler;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

