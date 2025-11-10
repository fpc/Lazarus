program SynTest;

{$mode objfpc}{$H+}
{off $DEFINE NOGUI}

uses
  Interfaces, Forms, consoletestrunner,
  {$ifNdef NOGUI} GuiTestRunner, {$ENDIF}
  LazUTF8, TestBase, TestBasicSynEdit, TestNavigation,
  TestSynSelection, TestSynMultiCaret, TestBlockIndent, TestBookMarks, TestSearch,
  TestSynBeautifier, TestTrimSpace, TestSyncroEdit, TestSynTextArea,
  TestHighlightPas, TestHighlightXml, TestHighlightMulti, TestMarkupwordGroup,
  TestMarkupHighAll, TestFoldedView, TestSynSharedEdits, TestHighlighterLfm,
  TestNestedFoldsList, TestMarkupIfDef, testPaintColorMerging,
  TestMarkupFoldColoring, TestWordWrap
  ;

{$IFDEF WINDOWS}{  $R SynTest.rc}{$ENDIF}

{$R *.res}

var
  Application: consoletestrunner.TTestRunner;

begin
  {$ifNdef NOGUI}
  if (ParamStrUTF8(1) = '-a') then begin
  {$ENDIF}
    DefaultRunAllTests:=True;
    DefaultFormat:=fXML;
    Forms.Application.Initialize;
    Application := TTestRunner.Create(nil);
    Application.Initialize;
    Application.Title := '';
    Application.Run;
    Application.Free;
    Forms.Application.Free;
    exit;
  {$ifNdef NOGUI}
  end;
  {$ENDIF}
  {$ifNdef NOGUI}
  Forms.Application.Title := '';
  Forms.Application.Initialize;
  Forms.Application.CreateForm(TGuiTestRunner, TestRunner);
  Forms.Application.Run;
  {$ENDIF}
end.

