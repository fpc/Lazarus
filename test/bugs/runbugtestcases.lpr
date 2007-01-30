program runbugtestcases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, BugTestCase;

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

