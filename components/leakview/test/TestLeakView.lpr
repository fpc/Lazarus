program TestLeakView;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestParser, leakview, simpleideintf;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

