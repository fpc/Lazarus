program TestMacroScript;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestScriptProcs;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

