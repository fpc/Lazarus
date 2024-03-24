program TestIdeDebugger;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestWatchResult, TestXmlOpts, TestVarious, TestWatchResPrinter;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

