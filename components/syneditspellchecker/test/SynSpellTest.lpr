program SynSpellTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestWordChecker;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

