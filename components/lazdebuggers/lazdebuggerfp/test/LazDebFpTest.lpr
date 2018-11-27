program LazDebFpTest;

{$mode objfpc}{$H+}

uses
  TestDbgControlForm,
  Interfaces, Forms, GuiTestRunner,  TestWatches, TestBase;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

