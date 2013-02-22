program SynTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestBase,
  TestBasicSynEdit, TestNavigation, TestSynSelection, TestBlockIndent, TestBookMarks,
  TestSearch, TestSynBeautifier, TestTrimSpace, TestSyncroEdit, TestSynTextArea,
  TestHighlightPas, TestHighlightXml, TestHighlightMulti,
  TestMarkupwordGroup, TestMarkupHighAll, TestFoldedView, TestSynSharedEdits,
  TestHighlighterLfm, TestHighlightFoldBase, TestMarkupIfDef;

{$IFDEF WINDOWS}{  $R SynTest.rc}{$ENDIF}

{$R *.res}

begin
  { $I SynTest.lrs}
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

