program TestIdeDebugger;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestWatchResult;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

