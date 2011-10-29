unit TestSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, LCLProc, LCLType, Forms, TestBase,
  SynEdit, SynEditTextTrimmer, SynEditKeyCmds, SynEditSearch;

type

  { TTestSynSearch }

  TTestSynSearch = class(TTestBase)
  private
  protected
    fTSearch: TSynEditSearch;
    //procedure ReCreateEdit; reintroduce;
    procedure ReCreateEditWithLinesSimple;
    procedure TestFindNext(AName, ASearchTxt: String;
      AStartX, AStartY, AEndX, AEndY: Integer;
      ExpFound: Boolean; ExpStartX, ExpStartY, ExpEndX, ExpEndY: Integer);
  published
    procedure TestSearchSimple;
    procedure TestSearchSimpleUtf8;
  end;

implementation

{ TTestSynSearch }

procedure TTestSynSearch.ReCreateEditWithLinesSimple;
begin
  ReCreateEdit;
  SetLines
    ([ 'Some text to search Text',
       'Text and more Text, texting',
       'text and more text, texting',
       'Text',
       '',
       'utf8: äöü äää äöü ÄÖÜ ÄÄÄ ÄÖÜ  äÖü ÄäÄ äääää äöüäöüä ä Ä',
       '',
       '',
       '',
       'Test or Dest or Destination. Test.',
       'test or dest or destination. test.',
       ''
    ]);

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

//more ftsearch:
    //function FindAll(const NewText: string): integer;
    //function FindFirstUTF8(const NewText: string): Integer;
    //procedure FixResults(First, Delta: integer);
    //function Next: Integer;

initialization

  RegisterTest(TTestSynSearch);

end.

