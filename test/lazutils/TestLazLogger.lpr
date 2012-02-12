program TestLazLogger;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestLazLoggerCase;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

