program FpTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestPascalParser, TestTypeInfo, TestHelperClasses,
TestDwarfSetup1, TestDwarfSetupBasic, TestDwarfVarious, testdwarfsetupArray, TestMemManager;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

