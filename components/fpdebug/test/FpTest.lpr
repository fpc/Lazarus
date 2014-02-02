program FpTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestPascalParser, TestTypeInfo, TestHelperClasses,
TestDwarfSetup1;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

