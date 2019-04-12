program TestLazStorageMem;

{$mode objfpc}{$H+}
{$apptype console}
uses
  Interfaces, Forms, GuiTestRunner, TestLazStorageMemCase1, TestConfigMemStorage;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

