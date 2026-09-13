unit TestSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase, SynEdit, SynEditSearch, SynHighlighterPas,
  SynEditTypes, SynEditMarkupBracket, LazEditMatchingBracketUtils, LazEditTypes;

type

  { TTestSynSearch }

  TTestSynSearch = class(TTestBase)
  private
  protected
    fTSearch: TSynEditSearch;
    //procedure ReCreateEdit; reintroduce;
    function TextLinesSimple: TStringArray;
    function TextLinesSimple2: TStringArray;
    procedure ReCreateEditWithLinesSimple;
    procedure TestFindNext(AName, ASearchTxt: String;
      AStartX, AStartY, AEndX, AEndY: Integer;
      ExpFound: Boolean; ExpStartX, ExpStartY, ExpEndX, ExpEndY: Integer);
  published
    procedure TestSearchSimple;
    procedure TestSearchSimpleUtf8;
    procedure TestSearchSimpleRegEx;
    procedure TestSearchZeroLengthRegEx;
    procedure FindMatchingBracket;
    procedure TestSearchMultiLine; // not regex
  end;

implementation

{ TTestSynSearch }

function TTestSynSearch.TextLinesSimple: TStringArray;
begin
  SetLength(Result, 12);
  Result[ 0] := 'Some text to search Text';
  Result[ 1] := 'Text and more Text, texting';
  Result[ 2] := 'text and more text, texting';
  Result[ 3] := 'Text';
  Result[ 4] := '';
  Result[ 5] := 'utf8: äöü äää äöü ÄÖÜ ÄÄÄ ÄÖÜ  äÖü ÄäÄ äääää äöüäöüä ä Ä';
  Result[ 6] := '';
  Result[ 7] := '';
  Result[ 8] := '';
  Result[ 9] := 'Test or Dest or Destination. Test.';
  Result[10] := 'test or dest or destination. test.';
  Result[11] := '';
end;

function TTestSynSearch.TextLinesSimple2: TStringArray;
begin
  SetLength(Result, 6);
  Result[ 0] := 'foo bar some more';
  Result[ 1] := 'äöü äää äöü';
  Result[ 2] := 'abc def 123';
  Result[ 3] := 'mno xyz 789';
  Result[ 4] := 'Text';
  Result[ 5] := '';
end;

procedure TTestSynSearch.ReCreateEditWithLinesSimple;
begin
  ReCreateEdit;
  SetLines(TextLinesSimple);
end;

procedure TTestSynSearch.TestFindNext(AName, ASearchTxt: String; AStartX, AStartY, AEndX,
  AEndY: Integer; ExpFound: Boolean; ExpStartX, ExpStartY, ExpEndX, ExpEndY: Integer);
var
  ptStart, ptEnd, ptFoundStart, ptFoundEnd: TPoint;
  r: Boolean;
begin
  AName := Format('%s FindNext: "%s" from (%d,%d) to (%d,%d): ', [AName, ASearchTxt, AStartX, AStartY, AEndX, AEndY]);
  ptStart := Point(AStartX, AStartY);
  ptEnd   := Point(AEndX, AEndY);
  fTSearch.Pattern     := ASearchTxt;
  r := fTSearch.FindNextOne(SynEdit.ViewedTextBuffer, ptStart, ptEnd, ptFoundStart, ptFoundEnd);

  AssertEquals(AName + 'Result', ExpFound, r);
  if ExpFound then begin
    AssertEquals(AName + 'StartX', ExpStartX, ptFoundStart.X);
    AssertEquals(AName + 'StartY', ExpStarty, ptFoundStart.Y);
    AssertEquals(AName + 'EndX',   ExpEndX,   ptFoundEnd.X);
    AssertEquals(AName + 'EndY',   ExpEndY,   ptFoundEnd.Y);
  end;
end;

procedure TTestSynSearch.TestSearchSimple;
begin
  ReCreateEditWithLinesSimple;

  fTSearch := TSynEditSearch.Create;
  fTSearch.Sensitive := False;
  fTSearch.Whole     := False;
  fTSearch.Backwards := False;
  fTSearch.RegularExpressions := False;
  fTSearch.RegExprMultiLine   := False;
  fTSearch.Replacement := '';

  TestFindNext('Simple',                'text',   1,1,  1,7,  true,   6,1, 10,1);
  TestFindNext('Simple again',          'text',   1,1,  1,7,  true,   6,1, 10,1);
  TestFindNext('Simple Start:-1',       'text',   5,1,  1,7,  true,   6,1, 10,1);
  TestFindNext('Simple Start: 0',       'text',   6,1,  1,7,  true,   6,1, 10,1);
  TestFindNext('Simple Start:+1',       'text',   7,1,  1,7,  true,  21,1, 25,1);
  TestFindNext('Simple Start:+1 end',   'text',   7,1, 12,1,  false,  0,0,  0,0);
  TestFindNext('Simple End: -1',        'text',   1,1,  9,1,  false,  0,0,  0,0);
  TestFindNext('Simple End:  0',        'text',   1,1, 10,1,  true,   6,1, 10,1);
  TestFindNext('Simple End: +1',        'text',   1,1, 11,1,  true,   6,1, 10,1);

  TestFindNext('Simple Next',           'text',  10,1,  1,7,  true,  21,1, 25,1);
  TestFindNext('Simple Next line',      'text',  22,1,  1,7,  true,   1,2,  5,2);
  TestFindNext('Simple at start line',  'text',   1,2,  1,7,  true,   1,2,  5,2);

  fTSearch.Sensitive := True;
  TestFindNext('Simple casesense',      'text',  10,1,  1,7,  true,  21,2, 25,2); // skip lower
  TestFindNext('Simple casesense',      'Text',  10,1,  1,7,  true,  21,1, 25,1);

  fTSearch.Sensitive := False;
  TestFindNext('Simple part word',      'text',  17,2,  1,7,  true,  21,2, 25,2);
  TestFindNext('Simple part word',      'Text',  17,2,  1,7,  true,  21,2, 25,2);

  fTSearch.Sensitive := True;
  TestFindNext('Simple part word case', 'text',  17,2,  1,7,  true,  21,2, 25,2);
  TestFindNext('Simple part word case', 'Text',  17,2,  1,7,  true,   1,4,  5,4); // skip lower

  fTSearch.Sensitive := False;
  fTSearch.Whole     := True;
  TestFindNext('Simple whole word',      'text',  17,2,  1,7,  true,   1,3,  5,3); // skip part word
  TestFindNext('Simple whole word comma','text',  10,2,  1,7,  true,  15,2, 19,2); // find with comma at end


  // backward
  fTSearch.Backwards := True;
  fTSearch.Sensitive := False;
  fTSearch.Whole     := False;
  TestFindNext('Back',                'text',   1,1,  12,1,  true,   6,1, 10,1);
  TestFindNext('Back prev line part', 'text',   1,1,   1,3,  true,  21,2, 25,2);
  fTSearch.Sensitive := True;
  TestFindNext('Back case',           'text',   1,1,  12,1,  true,   6,1, 10,1);
  TestFindNext('Back case',           'Text',   1,1,  12,1,  false,  0,0,  0,0);

  fTSearch.Sensitive := False;
  fTSearch.Whole     := true;
  TestFindNext('Back whole',           'text',   1,1,   1,3,  true,  15,2, 19,2);

  fTSearch.Free;
end;

procedure TTestSynSearch.TestSearchSimpleUtf8;
begin
  ReCreateEditWithLinesSimple;

  fTSearch := TSynEditSearch.Create;
  fTSearch.Sensitive := False;
  fTSearch.Whole     := False;
  fTSearch.Backwards := False;
  fTSearch.RegularExpressions := False;
  fTSearch.RegExprMultiLine   := False;
  fTSearch.Replacement := '';

  fTSearch.Sensitive := True;
  TestFindNext('Case',                'äöü',   1,1,  1,8,  true,   7,6, 13,6);
  TestFindNext('Case',                'ÄÖÜ',   1,1,  1,8,  true,  28,6, 34,6); // in BYTES
  TestFindNext('Case',                'äää',   1,1,  1,8,  true,  14,6, 20,6);
  TestFindNext('Case',                'ÄÄÄ',   1,1,  1,8,  true,  35,6, 41,6);

  //fTSearch.Sensitive := False;
  //TestFindNext('none Case',           'ÄÖÜ',   1,1,  1,8,  true,   7,6, 13,6); // in BYTES
  //TestFindNext('none Case',           'ÄÄÄ',   1,1,  1,8,  true,  14,6, 20,6);

  fTSearch.Free;
end;

procedure TTestSynSearch.TestSearchSimpleRegEx;
begin
  ReCreateEditWithLinesSimple;

  fTSearch := TSynEditSearch.Create;
  fTSearch.Sensitive := False;
  fTSearch.Whole     := False;
  fTSearch.Backwards := False;
  fTSearch.RegularExpressions := True;
  fTSearch.RegExprMultiLine   := False;
  fTSearch.Replacement := 'a${1}B';

  TestFindNext('RegEx',                '(t...),',   1,2,  25,3,  true,   15,2, 20,2);
  AssertEquals('RegexRepl', 'aTextB', fTSearch.RegExprReplace);

  // Not supported by fpc 3.2.0 ASupportUnicodeCase
  {$IF FPC_FULLVERSION >= 030202}
  fTSearch.Sensitive := True;
  TestFindNext('RegEx Case',           '(t...),',   1,2,  25,3,  true,   15,3, 20,3);
  AssertEquals('RegexRepl Case', 'atextB', fTSearch.RegExprReplace);
  {$ENDIF}

  fTSearch.Free;
end;

procedure TTestSynSearch.TestSearchZeroLengthRegEx;
var
  aBackwards: Boolean;

  procedure DoTestSearch(AName: String;
    AStartX, AStartY: Integer; ASearch: String;
    AExpX, AExpY: Integer;
    AOpts: TSynSearchOptions = [ssoRegExpr]);
  begin
    AName := AName + ' # Search: "' + ASearch + '" ';
    if aBackwards then
      AOpts := AOpts + [ssoBackwards];
    ReCreateEdit;
    SetLines(TextLinesSimple2);
    SetCaret(AStartX, AStartY);
    SynEdit.SearchReplaceEx(ASearch, '', AOpts, SynEdit.CaretObj.LineBytePos);

    TestIsCaret(AName+' (Caret): ', AExpX, AExpY);
  end;

  procedure DoTestRepl(AName: String;
    AStartX, AStartY: Integer; ASearch, ARepl: String;
    AExpX, AExpY: Integer; const AExpTextRepl: Array of const;
    AOpts: TSynSearchOptions = [ssoRegExpr, ssoReplaceAll]);
  begin
    AName := AName + ' # Replace "' + ASearch + '" => "' + ARepl + '"';
    if aBackwards then
      AOpts := AOpts + [ssoBackwards];
    ReCreateEdit;
    SetLines(TextLinesSimple2);
    SetCaret(AStartX, AStartY);
    SynEdit.SearchReplaceEx(ASearch, ARepl, AOpts, SynEdit.CaretObj.LineBytePos);

    TestIsCaret(AName+' (Caret): ', AExpX, AExpY);
    TestIsText(AName+' (Text): ', TextLinesSimple2, AExpTextRepl);
  end;

begin
  aBackwards := False;

  // Search ^
  DoTestSearch('Cont',  3, 3,  '^',   1, 4, [ssoRegExpr, ssoFindContinue]);
  DoTestSearch('',      3, 3,  '^',   1, 4);
  DoTestSearch('',      1, 4,  '^',   1, 4);
  DoTestSearch('Cont',  1, 4,  '^',   1, 5, [ssoRegExpr, ssoFindContinue]);

  // Search $
  DoTestSearch('Cont',  3, 3,  '$',  12, 3, [ssoRegExpr, ssoFindContinue]);
  DoTestSearch('',      3, 3,  '$',  12, 3);
// TODO: fTSearch.FindNextOne does not find the end in line 3 (when it is the start point)
//  DoTestSearch('',     12, 3,  '$',  12, 3);
  DoTestSearch('Cont', 12, 3,  '$',  12, 4, [ssoRegExpr, ssoFindContinue]);

  // Search ^|$
  DoTestSearch('',      3, 3,  '^|$',  12, 3);
//  DoTestSearch('',     12, 3,  '^|$',  12, 3);
  DoTestSearch('Cont', 12, 3,  '^|$',   1, 4, [ssoRegExpr, ssoFindContinue]);
  DoTestSearch('',      1, 4,  '^|$',   1, 4);
  DoTestSearch('Cont',  1, 4,  '^|$',  12, 4, [ssoRegExpr, ssoFindContinue]);

  // Search ()
  DoTestSearch('',      3, 3,  '()',  3, 3);
  DoTestSearch('Cont',  3, 3,  '()',  4, 3, [ssoRegExpr, ssoFindContinue]);

  DoTestSearch('UTF ',      3, 2,  '()',  3, 2);
  DoTestSearch('UTF Cont',  3, 2,  '()',  5, 2, [ssoRegExpr, ssoFindContinue]);


  // Replace ^
  DoTestRepl('',  3, 3,  '^', 'X',    2, 4, [4, 'Xmno xyz 789'], [ssoRegExpr, ssoReplace]);
  DoTestRepl('',  1, 4,  '^', 'X',    2, 4, [4, 'Xmno xyz 789'], [ssoRegExpr, ssoReplace]);
  DoTestRepl('',  1, 4,  '^', 'X',    2, 5, [5, 'XText'], [ssoRegExpr, ssoReplace, ssoFindContinue]);

  DoTestRepl('',  3, 3,  '^', '',    1, 5, []);
  DoTestRepl('',  1, 4,  '^', '',    1, 5, []);
  DoTestRepl('',  1, 4,  '^', '',    1, 5, [], [ssoRegExpr, ssoReplaceAll, ssoFindContinue]);

  // Replace-All ^
  DoTestRepl('',  3, 3,  '^', 'X',   2, 5, [4, 'Xmno xyz 789', 5, 'XText']);
  DoTestRepl('',  1, 4,  '^', 'X',   2, 5, [4, 'Xmno xyz 789', 5, 'XText']);
  DoTestRepl('',  1, 4,  '^', 'X',   2, 5, [5, 'XText'], [ssoRegExpr, ssoReplaceAll, ssoFindContinue]);

  DoTestRepl('',  3, 3,  '^', '',    1, 4, [], [ssoRegExpr, ssoReplace]);
  DoTestRepl('',  1, 4,  '^', '',    1, 4, [], [ssoRegExpr, ssoReplace]);
  DoTestRepl('',  1, 4,  '^', '',    1, 5, [], [ssoRegExpr, ssoReplace, ssoFindContinue]);


  // Replace $
  DoTestRepl('',  4, 4,  '$', 'X',  13, 4, [4, 'mno xyz 789X'], [ssoRegExpr, ssoReplace]);
//  DoTestRepl('', 12, 4,  '$', 'X',  13, 4, [4, 'mno xyz 789X'], [ssoRegExpr, ssoReplace]);
  DoTestRepl('', 12, 4,  '$', 'X',   6, 5, [5, 'TextX'], [ssoRegExpr, ssoReplace, ssoFindContinue]);

  DoTestRepl('',  4, 4,  '$', '',  12, 4, [], [ssoRegExpr, ssoReplace]);
//  DoTestRepl('', 12, 4,  '$', '',  12, 4, [], [ssoRegExpr, ssoReplace]);
  DoTestRepl('', 12, 4,  '$', '',   5, 5, [], [ssoRegExpr, ssoReplace, ssoFindContinue]);

  // Replace-All $
  DoTestRepl('',  4, 4,  '$', 'X',   6, 5, [4, 'mno xyz 789X', 5, 'TextX']);
//  DoTestRepl('', 12, 4,  '$', 'X',   6, 5, [4, 'mno xyz 789X', 5, 'TextX']);
  DoTestRepl('', 12, 4,  '$', 'X',   6, 5, [5, 'TextX'], [ssoRegExpr, ssoReplaceAll, ssoFindContinue]);

  DoTestRepl('',  4, 4,  '$', '',    5, 5, []);
  DoTestRepl('', 12, 4,  '$', '',    5, 5, []);
  DoTestRepl('', 12, 4,  '$', '',    5, 5, [], [ssoRegExpr, ssoReplaceAll, ssoFindContinue]);


  // Replace ^|$
  DoTestRepl('',  1, 4,  '^|$', 'X',   2, 4, [4, 'Xmno xyz 789'], [ssoRegExpr, ssoReplace]);
  DoTestRepl('',  1, 4,  '^|$', 'X',  13, 4, [4, 'mno xyz 789X'], [ssoRegExpr, ssoReplace, ssoFindContinue]);
//  DoTestRepl('', 12, 4,  '^|$', 'X',  13, 4, [4, 'mno xyz 789X'], [ssoRegExpr, ssoReplace]);
  DoTestRepl('', 12, 4,  '^|$', 'X',   2, 5, [5, 'XText'], [ssoRegExpr, ssoReplace, ssoFindContinue]);

  // Replace-All ^|$
  DoTestRepl('',  1, 4,  '^|$', 'X',   7, 5, [4, 'Xmno xyz 789X', 5, 'XTextX']);
  DoTestRepl('',  4, 4,  '^|$', 'X',   7, 5, [4, 'mno xyz 789X', 5, 'XTextX']);
//  DoTestRepl('', 12, 4,  '^|$', 'X',   7, 5, [4, 'mno xyz 789X', 5, 'XTextX']);
  DoTestRepl('', 12, 4,  '^|$', 'X',   7, 5, [5, 'XTextX'], [ssoRegExpr, ssoReplaceAll, ssoFindContinue]);


  // Replace ()
  DoTestRepl('',  1,4,  '()', 'X',   2, 4, [4, 'Xmno xyz 789'], [ssoRegExpr, ssoReplace]);
  DoTestRepl('',  1,4,  '()', 'X',   3, 4, [4, 'mXno xyz 789'], [ssoRegExpr, ssoReplace, ssoFindContinue]);
  DoTestRepl('',  2,4,  '()', 'X',   3, 4, [4, 'mXno xyz 789'], [ssoRegExpr, ssoReplace]);
  DoTestRepl('',  2,4,  '()', 'X',   4, 4, [4, 'mnXo xyz 789'], [ssoRegExpr, ssoReplace, ssoFindContinue]);
//  DoTestRepl('', 12,4,  '()', 'X',  13, 4, [4, 'mno xyz 789X'], [ssoRegExpr, ssoReplace]);
  DoTestRepl('', 12,4,  '()', 'X',   2, 5, [5, 'XText'], [ssoRegExpr, ssoReplace, ssoFindContinue]);

  // Replace-All ()
//  DoTestRepl('', 11,4,  '()', 'X',  10, 5, [4, 'mno xyz 78X9X', 5, 'XTXeXxXtX']);
//  DoTestRepl('', 11,4,  '()', '',    5, 5, []);
// Incorrect results BELOW (expect wrong caret and text, but test that it does not hang
DoTestRepl('', 11,4,  '()', 'X',   8, 5, [4, 'mno xyz 78X9', 5, 'XTXeXxXt']);
DoTestRepl('', 11,4,  '()', '',    4, 5, []);
// << end incorrect result


  // ****************************
  aBackwards := True;
//exit; // backward reg-ex is not (fully?) implemented

  // Search ^
  DoTestSearch('Cont',  3, 4,  '^',   1, 4, [ssoRegExpr, ssoFindContinue]);
  DoTestSearch('',      3, 4,  '^',   1, 4);
  DoTestSearch('',      1, 4,  '^',   1, 4);
//  DoTestSearch('Cont',  1, 4,  '^',   1, 3, [ssoRegExpr, ssoFindContinue]);

  // Search $
//  DoTestSearch('',      4, 4,  '$',  12, 3);
//  DoTestSearch('Cont',  1, 4,  '$',  12, 3);
//  DoTestSearch('Cont',  1, 4,  '$',  12, 3, [ssoRegExpr, ssoFindContinue]);
//  DoTestSearch('',     12, 4,  '$',  12, 4);
//  DoTestSearch('Cont', 12, 4,  '$',  12, 3, [ssoRegExpr, ssoFindContinue]);

  // Search ^|$
//  DoTestSearch('',      4, 4,  '^|$',   1, 4);
//  DoTestSearch('',      1, 4,  '^|$',   1, 4);
//  DoTestSearch('Cont',  1, 4,  '^|$',  12, 3, [ssoRegExpr, ssoFindContinue]);
//  DoTestSearch('Cont', 12, 3,  '^|$',  12, 3);
//  DoTestSearch('Cont', 12, 3,  '^|$',   1, 3, [ssoRegExpr, ssoFindContinue]);

  // Search ()
//  DoTestSearch('',      3, 3,  '()',  3, 3);
//  DoTestSearch('Cont',  3, 3,  '()',  2, 3, [ssoRegExpr, ssoFindContinue]);

//  DoTestSearch('UTF ',      3, 2,  '()',  3, 2);
//  DoTestSearch('UTF Cont',  5, 2,  '()',  3, 2, [ssoRegExpr, ssoFindContinue]);

  // Replace ^  // selection is backward on inserted text
//  DoTestRepl('',  3, 4,  '^', 'X',    1, 4, [4, 'Xmno xyz 789'], [ssoRegExpr, ssoReplace]);
  DoTestRepl('',  1, 4,  '^', 'X',    1, 4, [4, 'Xmno xyz 789'], [ssoRegExpr, ssoReplace]);
//  DoTestRepl('',  1, 4,  '^', 'X',    1, 3, [5, 'XText'], [ssoRegExpr, ssoReplace, ssoFindContinue]);

//  DoTestRepl('',  3, 4,  '^', '',    1, 4, []);
//  DoTestRepl('',  1, 4,  '^', '',    1, 4, []);
//  DoTestRepl('',  1, 4,  '^', '',    1, 3, [], [ssoRegExpr, ssoReplaceAll, ssoFindContinue]);

  // Replace-All ^
//  DoTestRepl('',  3, 2,  '^', 'X',   1, 1, [1, 'Xfoo bar some more', 2, 'Xäöü äää äöü']);
//  DoTestRepl('',  1, 2,  '^', 'X',   1, 1, [1, 'Xfoo bar some more', 2, 'Xäöü äää äöü']);
//  DoTestRepl('',  1, 2,  '^', 'X',   1, 1, [1, 'Xfoo bar some more'], [ssoRegExpr, ssoReplaceAll, ssoFindContinue]);

//  DoTestRepl('',  3, 2,  '^', '',   1, 1, []);
//  DoTestRepl('',  1, 2,  '^', '',   1, 1, []);
//  DoTestRepl('',  1, 2,  '^', '',   1, 1, [], [ssoRegExpr, ssoReplaceAll, ssoFindContinue]);

  // Replace $
//  DoTestRepl('',  4, 5,  '$', 'X',  12, 4, [4, 'mno xyz 789X'], [ssoRegExpr, ssoReplace]);
//  DoTestRepl('', 12, 4,  '$', 'X',  12, 4, [4, 'mno xyz 789X', 5, 'TextX'], [ssoRegExpr, ssoReplace]);
//  DoTestRepl('', 12, 4,  '$', 'X',  12, 3, [3, 'abc def 123X'], [ssoRegExpr, ssoReplace, ssoFindContinue]);

//  DoTestRepl('',  4, 5,  '$', '',  12, 4, [], [ssoRegExpr, ssoReplace]);
//  DoTestRepl('', 12, 4,  '$', '',  12, 4, [], [ssoRegExpr, ssoReplace]);
//  DoTestRepl('', 12, 4,  '$', '',  12, 3, [], [ssoRegExpr, ssoReplace, ssoFindContinue]);

  // Replace-All $
//  DoTestRepl('',  3, 3,  '$', 'X',  18, 1, [1, 'foo bar some moreX', 2, 'äöü äää äöüX']);
//  DoTestRepl('', 21, 2,  '$', 'X',  18, 1, [1, 'foo bar some moreX', 2, 'äöü äää äöüX']);
//  DoTestRepl('', 21, 2,  '$', 'X',  18, 1, [1, 'foo bar some moreX'], [ssoRegExpr, ssoReplaceAll, ssoFindContinue]);

//  DoTestRepl('',  3, 3,  '$', '',  18, 1, []);
//  DoTestRepl('', 21, 2,  '$', '',  18, 1, []);
//  DoTestRepl('', 21, 2,  '$', '',  18, 1, [], [ssoRegExpr, ssoReplaceAll, ssoFindContinue]);


end;

procedure TTestSynSearch.FindMatchingBracket;
  procedure TestBoth(AName: String; X,Y: integer;
    ExpLeftX, ExpLeftY, ExpLeftLen: Integer;
    ExpRightX, ExpRightY, ExpRightLen: Integer;
    ExpLeftStartX: integer = 0;
    ExpRightStartX: integer = 0);
  var
    FndStart, FndEnd: TLogTokenPos;
    ExpLeftStartLen, ExpRightStartLen: integer;
  begin
    ExpLeftStartLen := ExpLeftLen;
    ExpRightStartLen := ExpRightLen;
    if ExpLeftStartX  = 0 then ExpLeftStartX  := ExpLeftStartX - ExpLeftStartLen; // open has same len as close
    if ExpRightStartX = 0 then ExpRightStartX := ExpRightStartX;

    // find left
    FndStart := Point(x,y);
    FndEnd := SynEdit.FindMatchingBracketLogical(FndStart, bsdLeft, False, False, False);
    AssertEquals(AName+': X', ExpLeftX, FndEnd.x);
    if ExpLeftX <> -1 then begin
      AssertEquals(AName+': Y', ExpLeftY, FndEnd.y);
      AssertEquals(AName+': L', ExpLeftLen, FndEnd.Len);
      if ExpLeftStartX > 0 then begin
        AssertEquals(AName+': Start',     ExpLeftStartX, FndStart.X);
        AssertEquals(AName+': Start Len', ExpLeftStartLen, FndStart.Len);
      end;
    end;

    // find left in both
    if (ExpLeftX <> -1) or (ExpRightX = -1) then begin
      FndStart := Point(x,y);
      FndEnd := SynEdit.FindMatchingBracketLogical(FndStart, bsdLeftThenRight, False, False, False);
      AssertEquals(AName+': X', ExpLeftX, FndEnd.x);
      if ExpLeftX <> -1 then begin
        AssertEquals(AName+': Y', ExpLeftY, FndEnd.y);
        AssertEquals(AName+': L', ExpLeftLen, FndEnd.Len);
        if ExpLeftStartX > 0 then begin
          AssertEquals(AName+': Start',     ExpLeftStartX, FndStart.X);
          AssertEquals(AName+': Start Len', ExpLeftStartLen, FndStart.Len);
        end;
      end;
    end;

    // find left, as fallback from right
    if (ExpRightX = -1) then begin
      FndStart := Point(x,y);
      FndEnd := SynEdit.FindMatchingBracketLogical(FndStart, bsdRightThenLeft, False, False, False);
      AssertEquals(AName+': X', ExpLeftX, FndEnd.x);
      if ExpLeftX <> -1 then begin
        AssertEquals(AName+': Y', ExpLeftY, FndEnd.y);
        AssertEquals(AName+': L', ExpLeftLen, FndEnd.Len);
        if ExpLeftStartX > 0 then begin
          AssertEquals(AName+': Start',     ExpLeftStartX, FndStart.X);
          AssertEquals(AName+': Start Len', ExpLeftStartLen, FndStart.Len);
        end;
      end;
    end;


    // find right
    FndStart := Point(x,y);
    FndEnd := SynEdit.FindMatchingBracketLogical(FndStart, bsdRight, False, False, False);
    AssertEquals(AName+': X', ExpRightX, FndEnd.x);
    if ExpRightX <> -1 then begin
      AssertEquals(AName+': Y', ExpRightY, FndEnd.y);
      AssertEquals(AName+': L', ExpRightLen, FndEnd.Len);
      if ExpRightStartX > 0 then begin
        AssertEquals(AName+': Start',     ExpRightStartX, FndStart.X);
        AssertEquals(AName+': Start Len', ExpRightStartLen, FndStart.Len);
      end;
    end;

    // find right in both
    if (ExpRightX <> -1) or (ExpLeftX = -1) then begin
      FndStart := Point(x,y);
      FndEnd := SynEdit.FindMatchingBracketLogical(FndStart, bsdRightThenLeft, False, False, False);
      AssertEquals(AName+': X', ExpRightX, FndEnd.x);
      if ExpRightX <> -1 then begin
        AssertEquals(AName+': Y', ExpRightY, FndEnd.y);
        AssertEquals(AName+': L', ExpRightLen, FndEnd.Len);
        if ExpRightStartX > 0 then begin
          AssertEquals(AName+': Start',     ExpRightStartX, FndStart.X);
          AssertEquals(AName+': Start Len', ExpRightStartLen, FndStart.Len);
        end;
      end;
    end;

    // find right, as fallback from left
    if (ExpLeftX = -1) then begin
      FndStart := Point(x,y);
      FndEnd := SynEdit.FindMatchingBracketLogical(FndStart, bsdLeftThenRight, False, False, False);
      AssertEquals(AName+': X', ExpRightX, FndEnd.x);
      if ExpRightX <> -1 then begin
        AssertEquals(AName+': Y', ExpRightY, FndEnd.y);
        AssertEquals(AName+': L', ExpRightLen, FndEnd.Len);
        if ExpRightStartX > 0 then begin
          AssertEquals(AName+': Start',     ExpRightStartX, FndStart.X);
          AssertEquals(AName+': Start Len', ExpRightStartLen, FndStart.Len);
        end;
      end;
    end;

  end;

  procedure OnlyLeft(AName: String; X,Y: integer;
    ExpLeftX, ExpLeftY, ExpLeftLen: Integer;
    ExpLeftStartX: integer = 0);
  begin
    TestBoth(AName, X, Y,  ExpLeftX, ExpLeftY, ExpLeftLen,  -1,-1,-1,  ExpLeftStartX, -1);
  end;

  procedure OnlyRight(AName: String; X,Y: integer;
    ExpRightX, ExpRightY, ExpRightLen: Integer;
    ExpRightStartX: integer = 0);
  begin
    TestBoth(AName, X, Y, -1,-1,-1,  ExpRightX, ExpRightY, ExpRightLen,  -1, ExpRightStartX);
  end;

var
  p: TPoint;
  y,a : Integer;
  hl: TSynPasSyn;
begin
  ReCreateEdit;
  SetLines(['program a; begin',
            ' if (A or (B> 0)) and (C > length(L)) then ;',    // 2
           'a:=''(A or (B> 0)) and (C > length(L)) then '';',  // 3
           '  a := ('')'');',               // 4
           'a:=b((( 1+2 )))[(({}0))]',      // 5
           '  =(b[(0)]);',                   // 6
           'a:=b(((**) 1+2 ))(**){};{}',       // 7
            'end.',
            '']);

  hl := TSynPasSyn.Create(nil);

  for a := 0 to 1 do begin
    if a = 1 then
      SynEdit.Highlighter := hl;

    for y := 2 to 3 do begin
      //  if |(A or (B> 0)) and (C > length(L)) then ;
      p := SynEdit.FindMatchingBracket(point(5, y), False, False, False, False);
      AssertEquals('', y, p.y);
      AssertEquals('',17, p.x);

      p := SynEdit.FindMatchingBracket(point(6, y), True, False, False, False);
      AssertEquals('', y, p.y);
      AssertEquals('',17, p.x);

      p := SynEdit.FindMatchingBracket(point(6, y), False, False, False, False);
      AssertEquals('',-1, p.y);
      AssertEquals('',-1, p.x);

      //  if (A or |(B> 0)) and (C > length(L)) then ;
      p := SynEdit.FindMatchingBracket(point(11, y), False, False, False, False);
      AssertEquals('', y, p.y);
      AssertEquals('',16, p.x);

      p := SynEdit.FindMatchingBracket(point(12, y), True, False, False, False);
      AssertEquals('', y, p.y);
      AssertEquals('',16, p.x);

      //  if (A or (B> 0|)) and (C > length(L)) then ;
      p := SynEdit.FindMatchingBracket(point(16, y), False, False, False, False);
      AssertEquals('', y, p.y);
      AssertEquals('',11, p.x);

      p := SynEdit.FindMatchingBracket(point(17, y), True, False, False, False);
      AssertEquals('', y, p.y);
      AssertEquals('', 5, p.x);

      //  if (A or (B> 0))| and (C > length(L)) then ;
      p := SynEdit.FindMatchingBracket(point(18, y), True, False, False, False);
      AssertEquals('', y, p.y);
      AssertEquals('', 5, p.x);
    end;

    if a = 1 then begin
      //  a := |('')'');
      p := SynEdit.FindMatchingBracket(point(8, 4), False, False, False, False);
      AssertEquals('', 4, p.y);
      AssertEquals('',12, p.x);
    end;

    //  1   5    A    5    X
    // 'a:=b((( 1+2 )))[(({}0))]',      // 5
    OnlyRight('Before 1st (',    5, 5,   15, 5, 1);
    TestBoth('Between ((',       6, 5,   15, 5, 1,   14, 5, 1 );
    TestBoth('Between (((',      7, 5,   14, 5, 1,   13, 5, 1 );
    OnlyLeft('After (((',        8, 5,   13, 5, 1 );
    OnlyRight('Before )',       13, 5,    7, 5, 1 );
    TestBoth('Between ))',      14, 5,    7, 5, 1,    6, 5, 1 );
    TestBoth('Between )))',     15, 5,    6, 5, 1,    5, 5, 1 );
    TestBoth('Between )[',      16, 5,    5, 5, 1,   24, 5, 1 );
    TestBoth('Between [(',      17, 5,   24, 5, 1,   23, 5, 1 );
    TestBoth('Between [((',     18, 5,   23, 5, 1,   22, 5, 1 );
    TestBoth('Between ({',      19, 5,   22, 5, 1,   20, 5, 1 );
    TestBoth('Between {}',      20, 5,   20, 5, 1,   19, 5, 1 );
    OnlyLeft('After }',         21, 5,   19, 5, 1 );
    OnlyRight('Before ))]',     22, 5,   18, 5, 1 );
    TestBoth('Between ))]',     23, 5,   18, 5, 1,   17, 5, 1 );
    TestBoth('Between )]',      24, 5,   17, 5, 1,   16, 5, 1 );
    OnlyLeft('After ]',         25, 5,   16, 5, 1 );

    // '  =(b[(0)]);',                   // 6
    OnlyRight('Before 1st (',    4, 6,   11, 6, 1);
    OnlyLeft('After (',          5, 6,   11, 6, 1 );
    OnlyRight('Before 1st [',    6, 6,   10, 6, 1);
    TestBoth('Between [(',       7, 6,   10, 6, 1,   9, 6, 1 );
    OnlyLeft('After [(',         8, 6,    9, 6, 1 );
    OnlyRight('Before )',        9, 6,    7, 6, 1);
    TestBoth('Between )]',      10, 6,    7, 6, 1,   6, 6, 1 );
    TestBoth('Between ])',      11, 6,    6, 6, 1,   4, 6, 1 );
    OnlyLeft('After ])',        12, 6,    4, 6, 1 );

    // 'a:=b(((**) 1+2 ))(**){};{}',       // 7
    OnlyRight('Before 1st (',    5, 7,   17, 7, 1);
    TestBoth('Between ((',       6, 7,   17, 7, 1,  16, 7, 1 );
    if a = 0 then begin // no Pas HL
    TestBoth('Between (((*',     7, 7,   16, 7, 1,  10, 7, 1 );
    OnlyLeft('On (*',            8, 7,   10, 7, 1 );
    OnlyRight('On *)',          10, 7,    7, 7, 1 );
    end else begin // with Pas HL
    TestBoth('Between (((*',     7, 7,   16, 7, 1,   9, 7, 2 );
    OnlyLeft('On (*',            8, 7,    9, 7, 2,   7);
    //TestBoth('On (*',            8, 7,    9, 7, 2,   9, 7, 2,   7, 7);
    TestBoth('Between (**)',     9, 7,    9, 7, 2,   7, 7, 2 );
    OnlyLeft('On *)',           10, 7,    7, 7, 2,   9);
    //TestBoth('On *)',           10, 7,    7, 7, 2,   7, 7, 2,   9, 9);
    OnlyLeft('After *)',        11, 7,    7, 7, 2,   9);
    end;
    OnlyRight('Before )',        16, 7,    6, 7, 1 );
    TestBoth('Between ))',      17, 7,    6, 7, 1,   5, 7, 1 );
    if a = 0 then begin // no Pas HL
    end else begin // with Pas HL
    TestBoth('Between )(*',     18, 7,    5, 7, 1,   20, 7, 2 );
    end;



    SynEdit.Highlighter := nil;
  end;

  hl.Free;
end;

procedure TTestSynSearch.TestSearchMultiLine;
begin
  ReCreateEdit;
  SetLines(['', // 1
            'a',
            '',
            'b',
            '', // 5
            'a',
            'x',
            'b',
            '',
            'a', // 10
            'x2',
            'b',
            '']);

  fTSearch := TSynEditSearch.Create;
  fTSearch.Sensitive := False;
  fTSearch.Whole     := False;
  fTSearch.Backwards := False;
  fTSearch.RegularExpressions := False;
  fTSearch.RegExprMultiLine   := False;
  fTSearch.Replacement := '';

  TestFindNext('3 lines middle empty',  'a'+LineEnding+LineEnding+'b',  1,1,  1,9,  true,   1,2, 2,4);
  TestFindNext('3 lines middle empty - no match',  'a'+LineEnding+LineEnding+'b',  1,5,  1,9,  False,   1,2, 2,4);

  fTSearch.Free;
end;

//more ftsearch:
    //function FindAll(const NewText: string): integer;
    //function FindFirstUTF8(const NewText: string): Integer;
    //procedure FixResults(First, Delta: integer);
    //function Next: Integer;

initialization

  RegisterTest(TTestSynSearch);

end.

