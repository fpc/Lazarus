program SynTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LResources,
  TestBase,
  TestSynSelection, TestSynBeautifier, TestSyncroEdit, TestTrimSpace;

{$IFDEF WINDOWS}{$R SynTest.rc}{$ENDIF}

begin
  { $I SynTest.lrs}
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

