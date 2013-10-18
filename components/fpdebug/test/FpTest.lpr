program FpTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestPascalParser, TestTypeInfo;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

