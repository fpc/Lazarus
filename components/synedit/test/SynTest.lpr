program SynTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestBase,
  TestBasicSynEdit, TestSynSelection, TestBookMarks,
  TestSynBeautifier, TestTrimSpace, TestSyncroEdit,
  TestHighlightPas, TestMarkupwordGroup, TestFoldedView,
  TestHighlightXml, TestHighlightMulti;

{$IFDEF WINDOWS}{  $R SynTest.rc}{$ENDIF}

{$R *.res}

begin
  { $I SynTest.lrs}
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

