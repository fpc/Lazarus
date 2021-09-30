program SynTest;

{$mode objfpc}{$H+}
{off $DEFINE NOGUI}

uses
  Interfaces, Forms,
  {$ifdef NOGUI} consoletestrunner, {$ELSE}   GuiTestRunner, {$ENDIF}
  TestBase, TestBasicSynEdit, TestNavigation,
  TestSynSelection, TestSynMultiCaret, TestBlockIndent, TestBookMarks, TestSearch,
  TestSynBeautifier, TestTrimSpace, TestSyncroEdit, TestSynTextArea,
  TestHighlightPas, TestHighlightXml, TestHighlightMulti, TestMarkupwordGroup,
  TestMarkupHighAll, TestFoldedView, TestSynSharedEdits, TestHighlighterLfm,
  TestNestedFoldsList, TestMarkupIfDef, testPaintColorMerging,
  TestMarkupFoldColoring, TestWordWrap
  ;

{$IFDEF WINDOWS}{  $R SynTest.rc}{$ENDIF}

{$R *.res}

{$ifdef NOGUI}
var
  Application: TTestRunner;
  LclApp: TApplication;
{$ENDIF}

begin
  {$ifdef NOGUI}
  LclApp := Forms.Application.Create(nil);
  LclApp.Initialize;
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := '';
  Application.Run;
  Application.Free;
  LclApp.Free;
  {$ELSE}
  { $I SynTest.lrs}
  Application.Title := '';
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
  {$ENDIF}
end.

