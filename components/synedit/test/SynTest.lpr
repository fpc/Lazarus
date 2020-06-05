program SynTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestBase, TestBasicSynEdit, TestNavigation,
  TestSynSelection, TestSynMultiCaret, TestBlockIndent, TestBookMarks, TestSearch,
  TestSynBeautifier, TestTrimSpace, TestSyncroEdit, TestSynTextArea,
  TestHighlightPas, TestHighlightXml, TestHighlightMulti, TestMarkupwordGroup,
  TestMarkupHighAll, TestFoldedView, TestSynSharedEdits, TestHighlighterLfm,
  TestNestedFoldsList, TestMarkupIfDef, testPaintColorMerging,
  TestMarkupFoldColoring, TestWordWrap;

{$IFDEF WINDOWS}{  $R SynTest.rc}{$ENDIF}

{$R *.res}

begin
  { $I SynTest.lrs}
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

