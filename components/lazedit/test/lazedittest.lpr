program LazEditTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestTextMateGrammar, TestAsyncRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

