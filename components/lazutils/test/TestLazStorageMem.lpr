program TestLazStorageMem;

{$mode objfpc}{$H+}
{$apptype console}
uses
  Interfaces, Forms, SysUtils, GuiTestRunner, consoletestrunner, TestLazStorageMemCase1,
  TestConfigMemStorage;

{$R *.res}

var
  ConsoleApplication: consoletestrunner.TTestRunner;

begin
  if (argc > 1) and (
      ((strlicomp(argv[1], pchar('-c'), 1)) = 0) or
      ((strlicomp(argv[1], pchar('c'), 1)) = 0) or
      ((strlicomp(argv[1], pchar('/c'), 1)) = 0)
     )
  then begin
    DefaultRunAllTests:=True;
    DefaultFormat:=fXML;
    ConsoleApplication := consoletestrunner.TTestRunner.Create(nil);
    ConsoleApplication.Initialize;
    ConsoleApplication.Title := '';
    ConsoleApplication.Run;
    ConsoleApplication.Free;
  end
  else begin
    Forms.Application.Initialize;
    Forms.Application.CreateForm(TGuiTestRunner, TestRunner);
    Forms.Application.Run;
  end;
end.

