unit TestMarkupHighAll;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, testregistry, TestBase, Controls, Graphics, LazLoggerBase, IntegerList,
  SynEdit, SynEditMarkupHighAll, SynEditMiscProcs;

type

  { TMatchLoc }

  TMatchLoc = record
    y1, y2, x1, x2: Integer;
    function p1: TPoint;
    function p2: TPoint;
  end;

  { TTestSynEditMarkupHighlightAllMulti }

  TTestSynEditMarkupHighlightAllMulti = class(TSynEditMarkupHighlightAllMulti)
  public
    procedure BeginSendingInvalidation; override;
    procedure EndSendingInvalidation; override;
    procedure TestAddMatch(const m: TMatchLoc);
    procedure TestAddMatches(const m: array of TMatchLoc);
    procedure TestClearAndAddMatches(const m: array of TMatchLoc);
    property Matches;
  end;

  { TTestMarkupHighAll }

  TTestMarkupHighAll = class(TTestBase)
  private
    FTestMarkup: TTestSynEditMarkupHighlightAllMulti;
    FMatchList: Array of record
        p: PChar;
        l: Integer;
      end;
    FStopAtMatch, FNoMatchCount: Integer;
    FReceivedInvalidateLines: TIntegerList;
    procedure DoDictMatch(Match: PChar; MatchIdx: Integer; var IsMatch: Boolean;
      var StopSeach: Boolean);
    procedure DoReceiveInvalidateLine(FirstLine, LastLine: integer);
    procedure CheckReceiveInvalidateLine(const AName: String; const ALines: array of integer);
  protected
    procedure SetLinesInWindow(ALinesInWindow: Integer);
    procedure TestHasMatches(AName: String; const AExp: Array of TMAtchLoc; ExpMusNotExist: Boolean = False);
    procedure TestHasMCount(AName: String; AExpMin: Integer; AExpMax: Integer = -1);
    procedure TestHasMatches(AName: String; AExpCount: Integer; const AExp: Array of TMAtchLoc; ExpMusNotExist: Boolean = False);
    procedure TestHasMatches(AName: String; AExpCountMin, AExpCountMax: Integer; const AExp: Array of TMAtchLoc; ExpMusNotExist: Boolean = False);
    //procedure SetUp; override;
    procedure TearDown; override;
    //procedure ReCreateEdit; reintroduce;
    //function TestText1: TStringArray;
  published
    procedure TestMatchList;
    procedure TestMatchListIndex;
    procedure TestValidList;
    procedure TestDictionary;
    procedure TestValidateMatches;
  end; 

implementation

function l(y, x1, x2: Integer) : TMatchLoc;
begin
  Result.y1 := y;
  Result.x1 := x1;
  Result.y2 := y;
  Result.x2 := x2;
end;
function l(y, x, y2,x2: Integer) : TMatchLoc;
begin
  Result.y1 := y;
  Result.x1 := x;
  Result.y2 := y2;
  Result.x2 := x2;
end;
function p2(x,y, x2,y2: Integer) : TMatchLoc;
begin
  Result.y1 := y;
  Result.x1 := x;
  Result.y2 := y2;
  Result.x2 := x2;
end;

type

  { TSynMarkupHighAllMatchListHelper }

  TSynMarkupHighAllMatchListHelper = class helper for TSynMarkupHighAllMatchList
    procedure TestStartValidation(AnFirstInvalidMatchLine, AnLastInvalidMatchLine, AnMatchLinesDiffCount: Integer); // create gap
    procedure TestEndValidation; // limit gap size
  end;

{ TMatchLoc }

function TMatchLoc.p1: TPoint;
begin
  Result := Point(x1, y1);
end;

function TMatchLoc.p2: TPoint;
begin
  Result := Point(x2, y2);
end;

{ TTestSynEditMarkupHighlightAllMulti }

procedure TTestSynEditMarkupHighlightAllMulti.BeginSendingInvalidation;
begin
  inherited BeginSendingInvalidation;
end;

procedure TTestSynEditMarkupHighlightAllMulti.EndSendingInvalidation;
begin
  inherited EndSendingInvalidation;
end;

procedure TTestSynEditMarkupHighlightAllMulti.TestAddMatch(const m: TMatchLoc);
begin
  Matches.Insert(Point(m.x1,m.y1), Point(m.x2,m.y2));
end;

procedure TTestSynEditMarkupHighlightAllMulti.TestAddMatches(const m: array of TMatchLoc);
var
  i: Integer;
begin
  for i := low(m) to high(m) do
    TestAddMatch(m[i]);
end;

procedure TTestSynEditMarkupHighlightAllMulti.TestClearAndAddMatches(const m: array of TMatchLoc);
begin
  Matches.Clear;
  TestAddMatches(m);
end;

{ TSynMarkupHighAllMatchListHelper }

procedure TSynMarkupHighAllMatchListHelper.TestStartValidation(AnFirstInvalidMatchLine,
  AnLastInvalidMatchLine, AnMatchLinesDiffCount: Integer);
var
  p1, p2: TPoint;
begin
  StartValidation(AnFirstInvalidMatchLine, AnLastInvalidMatchLine,
      AnMatchLinesDiffCount, p1, p2);
end;

procedure TSynMarkupHighAllMatchListHelper.TestEndValidation;
begin
  EndValidation;
end;

{ TTestMarkupHighAll }

procedure TTestMarkupHighAll.DoDictMatch(Match: PChar; MatchIdx: Integer;
  var IsMatch: Boolean; var StopSeach: Boolean);
var
  i: Integer;
begin
  i := length(FMatchList);
  SetLength(FMatchList, i+1);
DebugLn([copy(Match, 1, MatchIdx)]);
  FMatchList[i].p := Match;
  FMatchList[i].l := MatchIdx;
  StopSeach := FStopAtMatch <> 0;
  IsMatch   := FNoMatchCount <= 0;
  dec(FStopAtMatch);
  dec(FNoMatchCount);
end;

procedure TTestMarkupHighAll.DoReceiveInvalidateLine(FirstLine, LastLine: integer);
var
  i: Integer;
begin
  if FReceivedInvalidateLines = nil then
    exit;

  for i := FirstLine to LastLine do begin
    //AssertTrue(FReceivedInvalidateLines.IndexOf());
    if FReceivedInvalidateLines.IndexOf(i) < 0 then
      FReceivedInvalidateLines.Add(i);
  end;
end;

procedure TTestMarkupHighAll.CheckReceiveInvalidateLine(const AName: String;
  const ALines: array of integer);
var
  i: Integer;
begin
  FReceivedInvalidateLines.Sort;
  AssertEquals(AName, Length(ALines), FReceivedInvalidateLines.Count);

  for i := 0 to Length(ALines) - 1 do
    AssertEquals(AName, ALines[i], FReceivedInvalidateLines[i]);
  FReceivedInvalidateLines.Clear;
end;

procedure TTestMarkupHighAll.SetLinesInWindow(ALinesInWindow: Integer);
var
  offs: Integer;
begin
  offs := SynEdit.Height - SynEdit.ClientHeight;
  AssertTrue('Clientheight <= height', offs >= 0);
  SynEdit.Height := SynEdit.LineHeight * ALinesInWindow + SynEdit.LineHeight div 2 + offs;
  AssertEquals('LinesInWindow', ALinesInWindow, SynEdit.LinesInWindow);
end;

procedure TTestMarkupHighAll.TestHasMatches(AName: String; const AExp: array of TMAtchLoc;
  ExpMusNotExist: Boolean);
var
  i, j: Integer;
begin
  for i := 0 to High(AExp) do begin
    if AExp[i].y1 = -2 then continue;
    j := FTestMarkup.Matches.Count - 1;
    while (j >= 0) and
      ( (FTestMarkup.Matches.StartPoint[j].y <> AExp[i].y1) or (FTestMarkup.Matches.StartPoint[j].x <> AExp[i].x1) or
        (FTestMarkup.Matches.EndPoint[j].y <> AExp[i].y2) or (FTestMarkup.Matches.EndPoint[j].x <> AExp[i].x2) )
    do
      dec(j);
    AssertEquals(BaseTestName+' '+AName+'('+IntToStr(i)+')', not ExpMusNotExist, j >= 0);
  end;
  // check: no duplicates
  for j := 0 to FTestMarkup.Matches.Count - 1 do begin
    AssertTrue('Not zero len', CompareCarets(FTestMarkup.Matches.StartPoint[j], FTestMarkup.Matches.EndPoint[j]) > 0);
    if j > 0 then
      AssertTrue('no overlap', CompareCarets(FTestMarkup.Matches.EndPoint[j-1], FTestMarkup.Matches.StartPoint[j]) >= 0);
  end;
end;

procedure TTestMarkupHighAll.TestHasMCount(AName: String; AExpMin: Integer; AExpMax: Integer);
begin
  AName := AName + '(CNT)';
  if AExpMax < 0 then begin
    AssertEquals(BaseTestName+' '+AName, AExpMin, FTestMarkup.Matches.Count);
  end
  else begin
    AssertTrue(BaseTestName+' '+AName+ '(Min)', AExpMin <= FTestMarkup.Matches.Count);
    AssertTrue(BaseTestName+' '+AName+ '(Max)', AExpMax >= FTestMarkup.Matches.Count);
  end;
end;

procedure TTestMarkupHighAll.TestHasMatches(AName: String; AExpCount: Integer;
  const AExp: array of TMAtchLoc; ExpMusNotExist: Boolean);
begin
  TestHasMatches(AName, AExp, ExpMusNotExist);
  TestHasMCount(AName, AExpCount);
end;

procedure TTestMarkupHighAll.TestHasMatches(AName: String; AExpCountMin, AExpCountMax: Integer;
  const AExp: array of TMAtchLoc; ExpMusNotExist: Boolean);
begin
  TestHasMatches(AName, AExp, ExpMusNotExist);
  TestHasMCount(AName, AExpCountMin, AExpCountMax);
end;

procedure TTestMarkupHighAll.TearDown;
begin
  FreeAndNil(FReceivedInvalidateLines);
  inherited TearDown;
end;

procedure TTestMarkupHighAll.TestMatchList;
  procedure Init;
  begin
    FTestMarkup.Matches.Clear;
  end;
  procedure StartInv(AnFirstInvalidMatchLine, AnLastInvalidMatchLine, AnMatchLinesDiffCount: Integer);
  begin
    FreeAndNil(FReceivedInvalidateLines);
    FReceivedInvalidateLines := TIntegerList.Create;
    FTestMarkup.BeginSendingInvalidation;
    FTestMarkup.Matches.TestStartValidation(AnFirstInvalidMatchLine, AnLastInvalidMatchLine, AnMatchLinesDiffCount);
  end;
  procedure EndInv;
  begin
    FTestMarkup.Matches.TestEndValidation;
    FTestMarkup.EndSendingInvalidation;
  end;
  procedure ValidationAdd(const m: array of TMatchLoc);
  begin
    FTestMarkup.TestAddMatches(m);
    EndInv;
  end;

  var
    TheName: string;
  procedure RunOne(const n: string;
    const AnInitMatches: array of TMatchLoc;
    const AnInvalStart, AnInvalEnd, AnInvalDiff: integer;
    const AnExpAfterInval: array of TMatchLoc;
    const AnAddMatches: array of TMatchLoc;
    const AnExpLinesSent: array of integer;
    const AnExpAfterAdd: array of TMatchLoc
    );
  begin
    Init;
    TheName := n;
    FTestMarkup.TestAddMatches(AnInitMatches);
    StartInv(AnInvalStart, AnInvalEnd, AnInvalDiff);
    TestHasMatches(n+': After inval', length(AnExpAfterInval), AnExpAfterInval);
    ValidationAdd(AnAddMatches);
    CheckReceiveInvalidateLine(n+': inval line sent', AnExpLinesSent);
    TestHasMatches(n+': After add', length(AnExpAfterAdd), AnExpAfterAdd);
  end;

  procedure RunMore(n: string;
    const AnInvalStart, AnInvalEnd, AnInvalDiff: integer;
    const AnExpAfterInval: array of TMatchLoc;
    const AnAddMatches: array of TMatchLoc;
    const AnExpLinesSent: array of integer;
    const AnExpAfterAdd: array of TMatchLoc
  );
  begin
    n := TheName + n;
    StartInv(AnInvalStart, AnInvalEnd, AnInvalDiff);
    TestHasMatches(n+': After inval', length(AnExpAfterInval), AnExpAfterInval);
    ValidationAdd(AnAddMatches);
    CheckReceiveInvalidateLine(n+': inval line sent', AnExpLinesSent);
    TestHasMatches(n+': After add', length(AnExpAfterAdd), AnExpAfterAdd);
  end;

var
  m1, m3, m3a, m5, m5a, m6, m6a, m7, m9, m10, m11: TMatchLoc;
begin
  FTestMarkup := TTestSynEditMarkupHighlightAllMulti.Create(SynEdit);
  FTestMarkup.InvalidateLinesMethod := @DoReceiveInvalidateLine;
(* TODO: check first last point by StartValidation(...out xxx) *)

  m1  := l(1, 2,3);
  m3  := l(3, 5,6);  m3a := l(3, 8,9);
  m5  := l(5, 7,8);  m5a := l(5, 11,18);
  m6  := l(6, 8,9);  m6a := l(6, 12,19);
  m7  := l(7, 9,10);
  m9  := l(9, 9,11);
  m10 := l(10, 8,9);
  m11 := l(11, 4,10);

  RunOne('Invalidate ONE line (new match on same line, but diff)',
         [m3],   1,5,0, // init / inval lines
         [],
         [m3a], [3],    // (re-)add / exp inval lines
         [m3a]
  );

  RunOne('Invalidate NO line (new match is SAME as old)',
         [m3],   1,5,0, // init / inval lines
         [],
         [m3], [],      // (re-)add / exp inval lines
         [m3]
  );

  RunOne('Invalidate ONE lines (new match on new line)',
         [m3],   1,5,0, // init / inval lines
         [],
         [m7], [3,7],   // (re-)add / exp inval lines
         [m7]
  );


  // Tests with several matches / replace some

  RunOne('Invalidate TWO lines (new match on same line, but diff)',
         [m3, m5, m6, m7],   4,6,0, // init / inval lines
         [m3, m7],
         [m5a, m6a], [5,6],    // (re-)add / exp inval lines
         [m3, m5a, m6a, m7]
  );

  RunOne('Invalidate TWO lines (new match on same line, ONLY ONE diff)',
         [m3, m5, m6, m7],   4,6,0, // init / inval lines
         [m3, m7],
         [m5a, m6], [5],    // (re-)add / exp inval lines
         [m3, m5a, m6, m7]
  );

  RunOne('Invalidate TWO lines (new match BOTH SAME as old)',
         [m3, m5, m6, m7],   4,6,0, // init / inval lines
         [m3, m7],
         [m5, m6], [],    // (re-)add / exp inval lines
         [m3, m5, m6, m7]
  );




  RunOne('Invalidate ONE lines - insert outside GAP (before)',
         [m3, m5, m6, m7],   6,6,0, // init / inval lines
         [m3, m5, m7],
         [m3a], [3,6],    // (re-)add / exp inval lines
         [m3, m3a, m5, m7]
  );

  RunOne('Invalidate ONE lines - insert outside GAP (after)',
         [m3, m5, m6, m7],   5,5,0, // init / inval lines
         [m3, m6, m7],
         [m6a], [5,6],    // (re-)add / exp inval lines
         [m3, m6, m6a, m7]
  );

  RunOne('Invalidate LAST line - insert outside GAP (FIRST)',
         [m3, m5, m6, m7],   7,7,0, // init / inval lines
         [m3, m5, m6],
         [m1], [1,7],    // (re-)add / exp inval lines
         [m1, m3, m5, m6]
  );
  RunMore(' / Invalidate FIRST line - insert outside GAP (LAST)',
         1,1,0,
         [m3, m5, m6],
         [m11], [1,11],
         [m3, m5, m6, m11]
  );
  RunMore(' / Invalidate ALL',
         1,99,0,
         [],
         [m6a], [3,5,6,11],
         [m6a]
  );

  RunOne('Invalidate FIRST line - insert outside GAP (LAST)',
         [m3, m5, m6, m7],   3,3,0, // init / inval lines
         [m5, m6, m7],
         [m11], [3,11],
         [m5, m6, m7, m11]
  );
  RunMore(' / Invalidate LAST line - insert outside GAP (FIRST)',
         11,11,0,
         [m5, m6, m7],
         [m1], [1,11],
         [m1, m5, m6, m7]
  );
  RunMore(' / Invalidate ALL - no new added',
         1,99,0,
         [],
         [], [1,5,6,7],
         []
  );
  RunMore(' / Invalidate ALL',
         1,99,0,
         [],
         [m6a], [6],
         [m6a]
  );




  RunOne('Invalidate TWO, restore 1, leave gap of 1',
         [m3, m5, m6, m7],   4,6,0,
         [m3, m7],
         [m5], [6],
         [m3, m5, m7]
  );
  RunMore(' / Inval empty, Add SAME value into existing gap',
         6,6,0,
         [m3, m5, m7],
         [m6], [6],
         [m3, m5, m6, m7]
  );

  RunOne('Invalidate TWO, restore 1, leave gap of 1',
         [m3, m5, m6, m7, m9, m11],   4,6,0,
         [m3, m7, m9, m11],
         [m5], [6],
         [m3, m5, m7, m9, m11]
  );
  RunMore(' / Inval one more, Add NEW value on new line',
         9,9,0,
         [m3, m5, m7, m11],
         [m10], [9,10],
         [m3, m5, m7, m10, m11]
  );


  // checks for GAP // multiple iterations of Inval/Add // MOVE GAP
  RunOne('Invalidate TWO, restore 1, leave gap of 1',
         [m3, m5, m6, m7, m9, m11],   4,6,0,
         [m3, m7, m9, m11],
         [m5], [6],
         [m3, m5, m7, m9, m11]
  );
  RunMore(' / Inval empty, Add NEW value on new line',
         10,10,0,
         [m3, m5, m7, m9, m11],
         [m10], [10],
         [m3, m5, m7, m9, m10, m11]
  );



  FreeAndNil(FReceivedInvalidateLines);
  FreeAndNil(FTestMarkup);
end;

procedure TTestMarkupHighAll.TestMatchListIndex;
var
  v0, v1, v2, v3, v4, v5, v6: TMatchLoc;
  m: TSynMarkupHighAllMatchList;
begin
  FTestMarkup := TTestSynEditMarkupHighlightAllMulti.Create(SynEdit);
  m := FTestMarkup.Matches;

  v0 := p2(2,10,  4,10);

  v1 := p2(2,15,  4,15);
  v2 := p2(7,15,  9,15);

  v3 := p2(2,20,  3,22);

  v4 := p2(2,30,  3,32);
  v5 := p2(5,32,  7,32);
  v6 := p2(9,32,  7,34);

  FTestMarkup.Clear;
  FTestMarkup.TestAddMatches([v0, v1, v2, v3, v4, v5, v6]);
  TestHasMatches('', [v0, v1, v2, v3, v4, v5, v6]);

  AssertEquals('idx for  1, 1 ', 0, m.IndexOf(Point( 1, 1)));
  AssertEquals('idx for  2,10 ', 0, m.IndexOf(Point( 2,10)));
  AssertEquals('idx for  3,10 ', 0, m.IndexOf(Point( 3,10)));
  AssertEquals('idx for  4,10 ', 1, m.IndexOf(Point( 4,10)));
  AssertEquals('idx for  5,10 ', 1, m.IndexOf(Point( 5,10)));
  AssertEquals('idx for  1,11 ', 1, m.IndexOf(Point( 1,11)));

  AssertEquals('idx for  1,15 ', 1, m.IndexOf(Point( 1,15)));
  AssertEquals('idx for  2,15 ', 1, m.IndexOf(Point( 2,15)));
  AssertEquals('idx for  3,15 ', 1, m.IndexOf(Point( 3,15)));
  AssertEquals('idx for  4,15 ', 2, m.IndexOf(Point( 4,15)));
  AssertEquals('idx for  5,15 ', 2, m.IndexOf(Point( 5,15)));
  AssertEquals('idx for  6,15 ', 2, m.IndexOf(Point( 6,15)));
  AssertEquals('idx for  7,15 ', 2, m.IndexOf(Point( 7,15)));
  AssertEquals('idx for  8,15 ', 2, m.IndexOf(Point( 8,15)));
  AssertEquals('idx for  9,15 ', 3, m.IndexOf(Point( 9,15)));
  AssertEquals('idx for 10,15 ', 3, m.IndexOf(Point(10,15)));

  AssertEquals('idx for  1,20 ', 3, m.IndexOf(Point( 1,20)));
  AssertEquals('idx for  2,20 ', 3, m.IndexOf(Point( 2,20)));
  AssertEquals('idx for  3,20 ', 3, m.IndexOf(Point( 3,20)));
  AssertEquals('idx for  1,21 ', 3, m.IndexOf(Point( 1,21)));
  AssertEquals('idx for  2,22 ', 3, m.IndexOf(Point( 2,22)));
  AssertEquals('idx for  3,22 ', 4, m.IndexOf(Point( 3,22)));
  AssertEquals('idx for  4,22 ', 4, m.IndexOf(Point( 4,22)));

  AssertEquals('idx for  1,30 ', 4, m.IndexOf(Point( 1,30)));
  AssertEquals('idx for  2,30 ', 4, m.IndexOf(Point( 2,30)));
  AssertEquals('idx for  3,30 ', 4, m.IndexOf(Point( 3,30)));
  AssertEquals('idx for  2,32 ', 4, m.IndexOf(Point( 2,32)));
  AssertEquals('idx for  3,32 ', 5, m.IndexOf(Point( 3,32)));
  AssertEquals('idx for  4,32 ', 5, m.IndexOf(Point( 4,32)));
  AssertEquals('idx for  5,32 ', 5, m.IndexOf(Point( 5,32)));
  AssertEquals('idx for  6,32 ', 5, m.IndexOf(Point( 6,32)));
  AssertEquals('idx for  7,32 ', 6, m.IndexOf(Point( 7,32)));
  AssertEquals('idx for  8,32 ', 6, m.IndexOf(Point( 8,32)));
  AssertEquals('idx for  9,32 ', 6, m.IndexOf(Point( 9,32)));
  AssertEquals('idx for 10,32 ', 6, m.IndexOf(Point(10,32)));
  AssertEquals('idx for  1,34 ', 6, m.IndexOf(Point( 1,34)));
  AssertEquals('idx for  6,34 ', 6, m.IndexOf(Point( 6,34)));
  AssertEquals('idx for  7,34 ', 7, m.IndexOf(Point( 7,34)));
  AssertEquals('idx for  8,34 ', 7, m.IndexOf(Point( 8,34)));


  AssertEquals('idx first for  1', 0, m.IndexOfFirstMatchForLine( 1));
  AssertEquals('idx first for  9', 0, m.IndexOfFirstMatchForLine( 9));
  AssertEquals('idx first for 10', 0, m.IndexOfFirstMatchForLine(10));
  AssertEquals('idx first for 11', 1, m.IndexOfFirstMatchForLine(11));

  AssertEquals('idx first for 14', 1, m.IndexOfFirstMatchForLine(14));
  AssertEquals('idx first for 15', 1, m.IndexOfFirstMatchForLine(15));
  AssertEquals('idx first for 16', 3, m.IndexOfFirstMatchForLine(16));

  AssertEquals('idx first for 19', 3, m.IndexOfFirstMatchForLine(19));
  AssertEquals('idx first for 20', 3, m.IndexOfFirstMatchForLine(20));
  AssertEquals('idx first for 21', 3, m.IndexOfFirstMatchForLine(21));
  AssertEquals('idx first for 22', 3, m.IndexOfFirstMatchForLine(22));
  AssertEquals('idx first for 23', 4, m.IndexOfFirstMatchForLine(23));

  AssertEquals('idx first for 29', 4, m.IndexOfFirstMatchForLine(29));
  AssertEquals('idx first for 30', 4, m.IndexOfFirstMatchForLine(30));
  AssertEquals('idx first for 31', 4, m.IndexOfFirstMatchForLine(31));
  AssertEquals('idx first for 32', 4, m.IndexOfFirstMatchForLine(32));
  AssertEquals('idx first for 33', 6, m.IndexOfFirstMatchForLine(33));
  AssertEquals('idx first for 34', 6, m.IndexOfFirstMatchForLine(34));
  AssertEquals('idx first for 35', 7, m.IndexOfFirstMatchForLine(35));  // no item


  AssertEquals('idx last for  1',-1, m.IndexOfLastMatchForLine( 1));  // no item
  AssertEquals('idx last for  9',-1, m.IndexOfLastMatchForLine( 9));  // no item
  AssertEquals('idx last for 10', 0, m.IndexOfLastMatchForLine(10));
  AssertEquals('idx last for 11', 0, m.IndexOfLastMatchForLine(11));

  AssertEquals('idx last for 14', 0, m.IndexOfLastMatchForLine(14));
  AssertEquals('idx last for 15', 2, m.IndexOfLastMatchForLine(15));
  AssertEquals('idx last for 16', 2, m.IndexOfLastMatchForLine(16));

  AssertEquals('idx last for 19', 2, m.IndexOfLastMatchForLine(19));
  AssertEquals('idx last for 20', 3, m.IndexOfLastMatchForLine(20));
  AssertEquals('idx last for 21', 3, m.IndexOfLastMatchForLine(21));
  AssertEquals('idx last for 22', 3, m.IndexOfLastMatchForLine(22));
  AssertEquals('idx last for 23', 3, m.IndexOfLastMatchForLine(23));

  AssertEquals('idx last for 29', 3, m.IndexOfLastMatchForLine(29));
  AssertEquals('idx last for 30', 4, m.IndexOfLastMatchForLine(30));
  AssertEquals('idx last for 31', 4, m.IndexOfLastMatchForLine(31));
  AssertEquals('idx last for 32', 6, m.IndexOfLastMatchForLine(32));
  AssertEquals('idx last for 33', 6, m.IndexOfLastMatchForLine(33));
  AssertEquals('idx last for 34', 6, m.IndexOfLastMatchForLine(34));
  AssertEquals('idx last for 35', 6, m.IndexOfLastMatchForLine(35));


  FreeAndNil(FTestMarkup);
end;

procedure TTestMarkupHighAll.TestValidList;
var
  L: TSynMarkupHighAllValidRanges;

  procedure TestValid(AName: String; const AExp: Array of TMatchLoc);
  var
    i: Integer;
  begin
    AssertEquals('%s Count', [AName], Length(AExp), l.Count);
    for i := 0 to Length(AExp) - 1 do begin
      AssertEquals('%s: %d Start Y', [AName, i], AExp[i].y1, l[i].StartPoint.y);
      AssertEquals('%s: %d Start X', [AName, i], AExp[i].x1, l[i].StartPoint.x);
      AssertEquals('%s: %d End Y',   [AName, i], AExp[i].y2, l[i].EndPoint.y);
      AssertEquals('%s: %d End X',   [AName, i], AExp[i].x2, l[i].EndPoint.x);
    end;
  end;

  procedure TestGap(AName: String; APoint: TPoint; AReturnNext: Boolean; AnExpIdx: Integer; AnExpGap: TMatchLoc);
  var
    i: Integer;
    r: TSynMarkupHighAllValidRange;
  begin
    i := L.FindGapFor(APoint, AReturnNext);
    AssertEquals('%s idx', [AName], AnExpIdx, i);
    if i < 0 then exit;
    r := L.Gap[i];
    AssertEquals('%s Start Y', [AName], AnExpGap.y1, r.StartPoint.y);
    AssertEquals('%s Start X', [AName], AnExpGap.x1, r.StartPoint.x);
    AssertEquals('%s End Y',   [AName], AnExpGap.y2, r.EndPoint.y);
    AssertEquals('%s End X',   [AName], AnExpGap.x2, r.EndPoint.x);
  end;

  procedure InitValid(const AList: array of TMatchLoc);
  var
    i: Integer;
  begin
    L := nil;
    for i := 0 to Length(AList) - 1 do
      L.MarkValid(AList[i].p1, AList[i].p2);
    TestValid('init', AList);
  end;

const
  LOC_E: TMatchLoc = (y1: -1; y2: -1; x1: -1; x2: -1);
  LOC_A: TMatchLoc = (y1: 1; y2: MaxInt; x1: 1; x2: 1);
var
  v1, v2, v3, v4, v5: TMatchLoc;
begin

  L := nil;
  TestValid('Empty', []);

  TestGap('', Point(9,9), False, 0, LOC_A);
  TestGap('', Point(9,9), True,  0, LOC_A);

  v1 := p2(5,10, 2,12);
  L.MarkValid(v1.p1, v1.p2);
  TestValid('one', [v1]);

  v4 := p2(5,10, 2,12); // same again
  L.MarkValid(v4.p1, v4.p2);
  TestValid('one', [v1]);
  v4 := p2(9,10, 3,11); // subset
  L.MarkValid(v4.p1, v4.p2);
  TestValid('one', [v1]);

  TestGap('', Point(9,9), False, 0, p2(1,1,  5,10));
  TestGap('', Point(9,9), True,  0, p2(1,1,  5,10));
  TestGap('', Point(4,10), False, 0, p2(1,1,  5,10));
  TestGap('', Point(4,10), True,  0, p2(1,1,  5,10));

  TestGap('', Point(5,10), False, -1, LOC_E);
  TestGap('', Point(5,10), True,  1, p2(2,12,  1,MaxInt));
  TestGap('', Point(9,11), False, -1, LOC_E);
  TestGap('', Point(9,11), True,  1, p2(2,12,  1,MaxInt));

  TestGap('', Point(2,12), False, 1, p2(2,12,  1,MaxInt));
  TestGap('', Point(2,12), True,  1, p2(2,12,  1,MaxInt));
  TestGap('', Point(9,19), False, 1, p2(2,12,  1,MaxInt));
  TestGap('', Point(9,19), True,  1, p2(2,12,  1,MaxInt));


  v2 := p2(5,20, 3,22);
  L.MarkValid(v2.p1, v2.p2);
  TestValid('two', [v1, v2]);

  TestGap('', Point(4,10), False, 0, p2(1,1,  5,10));
  TestGap('', Point(4,10), True,  0, p2(1,1,  5,10));

  TestGap('', Point(5,10), False, -1, LOC_E);
  TestGap('', Point(5,10), True,  1, p2(2,12,  5,20));

  TestGap('', Point(2,12), False, 1, p2(2,12,  5,20));
  TestGap('', Point(2,12), True,  1, p2(2,12,  5,20));
  TestGap('', Point(4,20), False, 1, p2(2,12,  5,20));
  TestGap('', Point(4,20), True,  1, p2(2,12,  5,20));

  TestGap('', Point(5,20), False, -1, LOC_E);
  TestGap('', Point(5,20), True,  2, p2(3,22,  1,MaxInt));

  TestGap('', Point(2,22), False, -1, LOC_E);
  TestGap('', Point(2,22), True,  2, p2(3,22,  1,MaxInt));

  TestGap('', Point(3,22), False, 2, p2(3,22,  1,MaxInt));
  TestGap('', Point(3,22), True,  2, p2(3,22,  1,MaxInt));
  TestGap('', Point(5,25), False, 2, p2(3,22,  1,MaxInt));
  TestGap('', Point(5,25), True,  2, p2(3,22,  1,MaxInt));


  v3 := p2(5,30, 3,32);
  L.MarkValid(v3.p1, v3.p2);
  TestValid('two', [v1, v2, v3]);

  v4 := p2(5,30, 3,32); // same again
  L.MarkValid(v4.p1, v4.p2);
  TestValid('two', [v1, v2, v3]);
  v4 := p2(6,31, 1,32); // subset
  L.MarkValid(v4.p1, v4.p2);
  TestValid('two', [v1, v2, v3]);
  // repeat first point
  v4 := p2(5,10, 2,12); // same again
  L.MarkValid(v4.p1, v4.p2);
  TestValid('two', [v1, v2, v3]);
  v4 := p2(9,10, 3,11); // subset
  L.MarkValid(v4.p1, v4.p2);
  TestValid('two', [v1, v2, v3]);


  // modify end

  v4 := p2(3,32, 3,34);
  L.MarkValid(v4.p1, v4.p2);
  v3 := p2(5,30, 3,34); // merge end of 3
  TestValid('two', [v1, v2, v3]);

  v4 := p2(3,33, 1,36);
  L.MarkValid(v4.p1, v4.p2);
  v3 := p2(5,30, 1,36); // merge overlap end of 3
  TestValid('two', [v1, v2, v3]);

  v4 := p2(2,36, 3,36); // leave gap after end of 3
  L.MarkValid(v4.p1, v4.p2);
  TestValid('two', [v1, v2, v3, v4]);
  v5 := p2(1,36, 1,36); // insert empty
  L.MarkValid(v5.p1, v5.p2);
  TestValid('two', [v1, v2, v3, v4]);
  v4 := p2(1,36, 2,36);
  L.MarkValid(v4.p1, v4.p2);
  v3 := p2(5,30, 3,36); // fill gap after end of 3
  TestValid('two', [v1, v2, v3]);

  v4 := p2(9,29, 5,30);
  L.MarkValid(v4.p1, v4.p2);
  v3 := p2(9,29, 3,36); // merge begin of 3
  TestValid('two', [v1, v2, v3]);

  v4 := p2(6,29, 1,31);
  L.MarkValid(v4.p1, v4.p2);
  v3 := p2(6,29, 3,36); // merge overlap begin of 3
  TestValid('two', [v1, v2, v3]);

  v3 := p2(1,29, 1,37); // replace 3 with bigger
  L.MarkValid(v3.p1, v3.p2);
  TestValid('two', [v1, v2, v3]);


  // modify middle

  v4 := p2(3,22, 5,22);
  L.MarkValid(v4.p1, v4.p2);
  v2 := p2(5,20, 5,22); // merge end of 2
  TestValid('two', [v1, v2, v3]);

  v4 := p2(1,22, 1,23);
  L.MarkValid(v4.p1, v4.p2);
  v2 := p2(5,20, 1,23); // merge overlap end of 2
  TestValid('two', [v1, v2, v3]);

  v4 := p2(2,20, 5,20);
  L.MarkValid(v4.p1, v4.p2);
  v2 := p2(2,20, 1,23); // merge begin of 2
  TestValid('two', [v1, v2, v3]);

  v4 := p2(8,19, 1,21);
  L.MarkValid(v4.p1, v4.p2);
  v2 := p2(8,19, 1,23); // merge overlap begin of 2
  TestValid('two', [v1, v2, v3]);

  v4 := p2(6,19, 7,19); // gap begin of 2
  L.MarkValid(v4.p1, v4.p2);
  TestValid('two', [v1, v4, v2, v3]);
  v5 := p2(4,19, 5,19); // 2nd gap begin of 2
  L.MarkValid(v5.p1, v5.p2);
  TestValid('two', [v1, v5, v4, v2, v3]);
  v4 := p2(5,19, 8,19); // close gap begin of 2
  L.MarkValid(v4.p1, v4.p2);
  v2 := p2(4,19, 1,23);
  TestValid('two', [v1, v2, v3]);

  // modify start

  v4 := p2(2,12, 5,12);
  L.MarkValid(v4.p1, v4.p2);
  v1 := p2(5,10, 5,12); // merge end of 1
  TestValid('two', [v1, v2, v3]);

  v4 := p2(1,11, 2,13);
  L.MarkValid(v4.p1, v4.p2);
  v1 := p2(5,10, 2,13); // merge overlap end of 1
  TestValid('two', [v1, v2, v3]);

  v4 := p2(2,10, 5,10);
  L.MarkValid(v4.p1, v4.p2);
  v1 := p2(2,10, 2,13); // merge begin of 1
  TestValid('two', [v1, v2, v3]);

  v4 := p2(11,9, 1,13);
  L.MarkValid(v4.p1, v4.p2);
  v1 := p2(11,9, 2,13); // merge overlap begin of 1
  TestValid('two', [v1, v2, v3]);

  v4 := p2(9,9, 2,13);
  L.MarkValid(v4.p1, v4.p2);
  v1 := p2(9,9, 2,13); // merge overlap begin of 1
  TestValid('two', [v1, v2, v3]);


  v4 := p2(1,9, 2,9); // before begin / gap
  L.MarkValid(v4.p1, v4.p2);
  TestValid('two', [v4, v1, v2, v3]);
  v5 := p2(1,8, 2,8); // before begin / gap
  L.MarkValid(v5.p1, v5.p2);
  TestValid('two', [v5, v4, v1, v2, v3]);
  v4 := p2(3,7, 2,10); // close gap before begin / overlap
  L.MarkValid(v4.p1, v4.p2);
  v1 := p2(3,7, 2,13); // merge overlap begin of 1
  TestValid('two', [v1, v2, v3]);

  v1 := p2(3,2, 2,50); // merge all
  L.MarkValid(v1.p1, v1.p2);
  TestValid('one', [v1]);

  // remove before

  v1 := p2(3,2, 2,50);
  InitValid([v1]);

  L.RemoveBefore(Point(3,1));
  TestValid('remove before', [v1]);
  L.RemoveBefore(Point(3,2));
  TestValid('remove before', [v1]);

  L.RemoveBefore(Point(4,2));
  v1 := p2(4,2, 2,50);
  TestValid('remove before', [v1]);

  L.RemoveBefore(Point(2,50));
  TestValid('remove before', []);
  L.RemoveBefore(Point(2,50));
  TestValid('remove before', []);

  v1 := p2(3,12, 2,15);
  v2 := p2(1,22, 1,25);
  InitValid([v1, v2]);

  L.RemoveBefore(Point(2,14));
  v1 := p2(2,14, 2,15);
  TestValid('remove before', [v1, v2]);

  L.RemoveBefore(Point(2,15));
  TestValid('remove before', [v2]);

  v1 := p2(3,12, 2,15);
  v2 := p2(1,22, 1,25);
  InitValid([v1, v2]);
  L.RemoveBefore(Point(3,22));
  v2 := p2(3,22, 1,25);
  TestValid('remove before', [v2]);

  v1 := p2(3,12, 2,15);
  v2 := p2(1,22, 1,25);
  InitValid([v1, v2]);
  L.RemoveBefore(Point(1,25));
  TestValid('remove before', []);

  InitValid([v1, v2]);
  L.RemoveBefore(Point(2,25));
  TestValid('remove before', []);

  // remove after

  v1 := p2(3,2, 2,50);
  InitValid([v1]);

  L.RemoveAfter(Point(3,50));
  TestValid('remove after', [v1]);
  L.RemoveAfter(Point(2,50));
  TestValid('remove after', [v1]);

  L.RemoveAfter(Point(1,50));
  v1 := p2(3,2, 1,50);
  TestValid('remove after', [v1]);

  L.RemoveAfter(Point(3,2));
  TestValid('remove after', []);
  L.RemoveAfter(Point(3,2));
  TestValid('remove after', []);

  v1 := p2(3,12, 2,15);
  v2 := p2(1,22, 1,25);
  InitValid([v1, v2]);

  L.RemoveAfter(Point(2,24));
  v2 := p2(1,22, 2,24);
  TestValid('remove after', [v1, v2]);

  L.RemoveAfter(Point(1,22));
  TestValid('remove after', [v1]);

  v1 := p2(3,12, 2,15);
  v2 := p2(1,22, 1,25);
  InitValid([v1, v2]);
  L.RemoveAfter(Point(3,14));
  v1 := p2(3,12, 3,14);
  TestValid('remove after', [v1]);

  v1 := p2(3,12, 2,15);
  v2 := p2(1,22, 1,25);
  InitValid([v1, v2]);
  L.RemoveAfter(Point(3,12));
  TestValid('remove after', []);

  InitValid([v1, v2]);
  L.RemoveAfter(Point(2,12));
  TestValid('remove after', []);

  // remove between

  v1 := p2(3,2, 2,50);
  InitValid([v1]);
  L.RemoveBetween(Point(3,50), Point(3,51));
  TestValid('remove between', [v1]);
  L.RemoveBetween(Point(3,1), Point(1,2));
  TestValid('remove between', [v1]);
  L.RemoveBetween(Point(3,1), Point(1,55));
  TestValid('remove between', []);
  L.RemoveBetween(Point(3,1), Point(1,55));
  TestValid('remove between', []);

  v1 := p2(3,2, 2,50);
  InitValid([v1]);
  L.RemoveBetween(Point(2,50), Point(3,51));
  TestValid('remove between', [v1]);
  L.RemoveBetween(Point(1,50), Point(3,51));
  v1 := p2(3,2, 1,50);
  TestValid('remove between', [v1]);

  L.RemoveBetween(Point(1,1), Point(3,2));
  TestValid('remove between', [v1]);
  L.RemoveBetween(Point(1,1), Point(3,3));
  v1 := p2(3,3, 1,50);
  TestValid('remove between', [v1]);

  L.RemoveBetween(Point(1,10), Point(2,10));
  v1 := p2(3,3, 1,10);
  v2 := p2(2,10, 1,50);
  TestValid('remove between', [v1, v2]);
  L.RemoveBetween(Point(3,1), Point(1,55));
  TestValid('remove between', []);

  v1 := p2(3,3, 1,11);
  v2 := p2(2,15, 1,17);
  v3 := p2(2,20, 1,25);
  InitValid([v1, v2, v3]);
  L.RemoveBetween(Point(1,10), Point(2,20));
  v1 := p2(3,3, 1,10);
  TestValid('remove between', [v1, v3]);

  v1 := p2(3,3, 1,11);
  v2 := p2(2,15, 1,17);
  v3 := p2(2,20, 1,25);
  InitValid([v1, v2, v3]);
  L.RemoveBetween(Point(1,11), Point(2,21));
  v3 := p2(2,21, 1,25);
  TestValid('remove between', [v1, v3]);

  v1 := p2(3,3, 1,11);
  v2 := p2(2,15, 1,17);
  v3 := p2(2,20, 1,25);
  InitValid([v1, v2, v3]);
  L.RemoveBetween(Point(1,10), Point(2,21));
  v1 := p2(3,3, 1,10);
  v3 := p2(2,21, 1,25);
  TestValid('remove between', [v1, v3]);

  // AdjustForLinesChanged

  v1 := p2(3,3, 1,11);
  v2 := p2(2,15, 2,17);
  v3 := p2(2,20, 1,25);
  InitValid([v1, v2, v3]);
  L.AdjustForLinesChanged(16, 1);
  v2 := p2(2,15, 2,18);
  v3 := p2(2,21, 1,26);
  TestValid('adjust', [v1, v2, v3]);

  v1 := p2(3,3, 1,11);
  v2 := p2(2,15, 2,17);
  v3 := p2(2,20, 1,25);
  InitValid([v1, v2, v3]);
  L.AdjustForLinesChanged(16, -1);
  v2 := p2(2,15, 2,16);
  v3 := p2(2,19, 1,24);
  TestValid('adjust', [v1, v2, v3]);

  v1 := p2(3,3, 1,11);
  v2 := p2(2,15, 2,17);
  v3 := p2(2,20, 1,25);
  InitValid([v1, v2, v3]);
  L.AdjustForLinesChanged(16, -3);
  v2 := p2(2,15, 1,16);
  v3 := p2(2,17, 1,22);
  TestValid('adjust', [v1, v2, v3]);

  v1 := p2(3,3, 1,11);
  v2 := p2(2,15, 2,17);
  v3 := p2(2,20, 1,25);
  InitValid([v1, v2, v3]);
  L.AdjustForLinesChanged(15, -3);
  v3 := p2(2,17, 1,22);
  TestValid('adjust', [v1, v3]);

  v1 := p2(3,3, 1,11);
  v2 := p2(2,15, 2,17);
  v3 := p2(2,20, 1,25);
  InitValid([v1, v2, v3]);
  L.AdjustForLinesChanged(15, -6);
  v3 := p2(1,15, 1,19);
  TestValid('adjust', [v1, v3]);

  v1 := p2(3,3, 1,11);
  v2 := p2(2,15, 2,17);
  v3 := p2(2,20, 1,25);
  InitValid([v1, v2, v3]);
  L.AdjustForLinesChanged(15, -10);
  TestValid('adjust', [v1]);
end;

procedure TTestMarkupHighAll.TestDictionary;
var
  Dict: TSynSearchDictionary;
  Name, LineText: String;
  Res1, Res2: PChar;

  procedure InitTest(AName, ALineText: String; AStopAtMatch: Integer = -1; ANoMatchCount: Integer = 0);
  begin
    Name := AName + '[in: "'+ALineText+'", StopAt='+IntToStr(AStopAtMatch)+', NoMatchCnt='+IntToStr(ANoMatchCount)+']';
    LineText := ALineText;
    SetLength(FMatchList, 0);
    FStopAtMatch := AStopAtMatch;
    FNoMatchCount := ANoMatchCount;;
    if LineText = '' then begin
      Res1 := Dict.Search(nil, Length(LineText), nil);
      Res2 := Dict.Search(nil, Length(LineText), @DoDictMatch);
    end
    else begin
      Res1 := Dict.Search(@LineText[1], Length(LineText), nil);
      Res2 := Dict.Search(@LineText[1], Length(LineText), @DoDictMatch);
      //dict.GetMatchAtChar();
    end;
  end;

  procedure CheckExp(ExpRes1, ExpRes2: Integer);
  begin
    if ExpRes1 = 0
    then AssertTrue(Name+' Result (no event)', nil = Res1)
    else if ExpRes1 > 0
    then AssertEquals(Name+' Result (no event)', ExpRes1, Res1 - @LineText[1]);
    if ExpRes2 = 0
    then AssertTrue(Name+' Result (event)', nil = Res2)
    else if ExpRes2 > 0
    then AssertEquals(Name+' Result (event)', ExpRes2, Res2 - @LineText[1]);
  end;

  procedure CheckExp(AExpCount: Integer; const AExpList: array of Integer);
  var
    i: Integer;
  begin
    AssertEquals(Name+' (len list)', AExpCount, Length(FMatchList));
    for i := 0 to Length(AExpList) div 2 -1 do begin
      AssertEquals(Name+' (start '+IntToStr(i)+')', AExpList[i*2], FMatchList[i].p - @LineText[1]);
      AssertEquals(Name+' (len '+IntToStr(i)+')', AExpList[i*2+1], FMatchList[i].l);
    end;
  end;

  procedure CheckExp(ExpRes1, ExpRes2, AExpCount: Integer; const AExpList: array of Integer);
  begin
    CheckExp(ExpRes1, ExpRes2);
    CheckExp(AExpCount, AExpList);
  end;


var
  i: Integer;
begin
  Dict := TSynSearchDictionary.Create;
  Dict.Add('debugln',1);
  Dict.Add('debuglnenter',2);
  Dict.Add('debuglnexit',3);
  Dict.Add('dbgout',4);
  //Dict.DebugPrint();
  Dict.Free;


  Dict := TSynSearchDictionary.Create;
  Dict.Add('foo'       , 0); // IDX 0
  Dict.Add('Hello'     , 1);
  Dict.Add('yello12345', 2);
  Dict.Add(   'lo123'  , 3);
  Dict.Add(   'lo789'  , 4);
  Dict.Add('hell'      , 5);  // IDX 5
  Dict.Add('hatter'    , 6);
  Dict.Add('log'       , 7);
  Dict.Add('lantern'   , 8);
  Dict.Add('terminal'  , 9);
  Dict.Add('all'       ,10);
  Dict.Add('alt'       ,11);
  Dict.Add('YESTERDAY' ,12);
  Dict.Add(  'STER'    ,13);
  Dict.Add(   'TE'     ,14);
  Dict.Add(    'ER'    ,15);
  Dict.Add(      'DAY' ,16);

(*
Dict.Add('Algoritmus', 0);
Dict.Add('Aho', 0);
Dict.Add('Corasick', 0);
Dict.Add('je', 0);
Dict.Add('vyhledávací', 0);
Dict.Add('algoritmus', 0);
Dict.Add('vynalezený', 0);
Dict.Add('Alfredem', 0);
Dict.Add('Ahem', 0);
Dict.Add('a', 0);
Dict.Add('Margaret', 0);
Dict.Add('J', 0);
Dict.Add('Corasickovou', 0);
Dict.Add('Je', 0);
Dict.Add('to', 0);
Dict.Add('druh', 0);
Dict.Add('slovníkového', 0);
Dict.Add('vyhledávacího', 0);
Dict.Add('algoritmu', 0);
Dict.Add('který', 0);
Dict.Add('ve', 0);
Dict.Add('vstupním', 0);
Dict.Add('textu', 0);
Dict.Add('hledá', 0);
Dict.Add('prvky', 0);
Dict.Add('konečné', 0);
Dict.Add('množiny', 0);
Dict.Add('řetězců', 0);
Dict.Add('Vyhledává', 0);
Dict.Add('všechny', 0);
Dict.Add('prvky', 0);
Dict.Add('množiny', 0);
Dict.Add('najednou', 0);2 pi *
Dict.Add('jeho', 0);
Dict.Add('asymptotická', 0);
Dict.Add('složitost', 0);
Dict.Add('je', 0);
Dict.Add('proto', 0);
Dict.Add('lineární', 0);
Dict.Add('k', 0);
Dict.Add('délce', 0);
Dict.Add('všech', 0);
Dict.Add('vyhledávaných', 0);
Dict.Add('prvků', 0);
Dict.Add('plus', 0);
Dict.Add('délce', 0);
Dict.Add('vstupního', 0);
Dict.Add('textu', 0);
Dict.Add('plus', 0);
Dict.Add('délce', 0);
Dict.Add('výstupu', 0);
Dict.Add('Jelikož', 0);
Dict.Add('algoritmus', 0);
Dict.Add('najde', 0);
Dict.Add('všechny', 0);
Dict.Add('výskyty', 0);
Dict.Add('celkový', 0);
Dict.Add('počet', 0);
Dict.Add('výskytů', 0);
Dict.Add('pro', 0);
Dict.Add('celou', 0);
Dict.Add('množinu', 0);
Dict.Add('může', 0);
Dict.Add('být', 0);
Dict.Add('až', 0);
Dict.Add('kvadratický', 0);
Dict.Add('(například', 0);
Dict.Add('v', 0);
Dict.Add('případě', 0);
Dict.Add('kdy', 0);
Dict.Add('vyhledávané', 0);
Dict.Add('řetězce', 0);
Dict.Add('jsou', 0);
Dict.Add('a', 0);
Dict.Add('aa', 0);
Dict.Add('aaa', 0);
Dict.Add('aaaa', 0);
Dict.Add('a', 0);
Dict.Add('vstupní', 0);
Dict.Add('text', 0);
Dict.Add('je', 0);
Dict.Add('aaaa)', 0);
Dict.Add('Neformálně', 0);
Dict.Add('řečeno', 0);
Dict.Add('algoritmus', 0);
Dict.Add('konstruuje', 0);
Dict.Add('trie', 0);
Dict.Add('se', 0);
Dict.Add('zpětnými', 0);
Dict.Add('odkazy', 0);
Dict.Add('pro', 0);
Dict.Add('každý', 0);
Dict.Add('vrchol', 0);
Dict.Add('(například', 0);
Dict.Add('abc)', 0);
Dict.Add('na', 0);
Dict.Add('nejdelší', 0);
Dict.Add('vlastní', 0);
Dict.Add('sufix', 0);
Dict.Add('(pokud', 0);
Dict.Add('existuje', 0);
Dict.Add('tak', 0);
Dict.Add('bc', 0);
Dict.Add('jinak', 0);
Dict.Add('pokud', 0);
Dict.Add('existuje', 0);
Dict.Add('c', 0);
Dict.Add('jinak', 0);
Dict.Add('do', 0);
Dict.Add('kořene)', 0);
Dict.Add('Obsahuje', 0);
Dict.Add('také', 0);
Dict.Add('odkazy', 0);
Dict.Add('z', 0);
Dict.Add('každého', 0);
Dict.Add('vrcholu', 0);
Dict.Add('na', 0);
Dict.Add('prvek', 0);
Dict.Add('slovníku', 0);
Dict.Add('obsahující', 0);
Dict.Add('odpovídající', 0);
Dict.Add('nejdelší', 0);
Dict.Add('sufix', 0);
Dict.Add('Tudíž', 0);
Dict.Add('všechny', 0);
Dict.Add('výsledky', 0);
Dict.Add('mohou', 0);
Dict.Add('být', 0);
Dict.Add('vypsány', 0);
Dict.Add('procházením', 0);
Dict.Add('výsledného', 0);
Dict.Add('spojového', 0);
Dict.Add('seznamu', 0);
Dict.Add('Algoritmus', 0);
Dict.Add('pak', 0);
Dict.Add('pracuje', 0);
Dict.Add('tak', 0);
Dict.Add('že', 0);
Dict.Add('postupně', 0);
Dict.Add('zpracovává', 0);
Dict.Add('vstupní', 0);
Dict.Add('řetězec', 0);
Dict.Add('a', 0);
Dict.Add('pohybuje', 0);
Dict.Add('se', 0);
Dict.Add('po', 0);
Dict.Add('nejdelší', 0);
Dict.Add('odpovídající', 0);
Dict.Add('cestě', 0);
Dict.Add('stromu', 0);
Dict.Add('Pokud', 0);
Dict.Add('algoritmus', 0);
Dict.Add('načte', 0);
Dict.Add('znak', 0);
Dict.Add('který', 0);
Dict.Add('neodpovídá', 0);
Dict.Add('žádné', 0);
Dict.Add('další', 0);
Dict.Add('možné', 0);
Dict.Add('cestě', 0);
Dict.Add('přejde', 0);
Dict.Add('po', 0);
Dict.Add('zpětném', 0);
Dict.Add('odkazu', 0);
Dict.Add('na', 0);
Dict.Add('nejdelší', 0);
Dict.Add('odpovídající', 0);
Dict.Add('sufix', 0);
Dict.Add('a', 0);
Dict.Add('pokračuje', 0);
Dict.Add('tam', 0);
Dict.Add('(případně', 0);
Dict.Add('opět', 0);
Dict.Add('přejde', 0);
Dict.Add('zpět)', 0);
Dict.Add('Pokud', 0);
Dict.Add('je', 0);
Dict.Add('množina', 0);
Dict.Add('vyhledávaných', 0);
Dict.Add('řetězců', 0);
Dict.Add('známa', 0);
Dict.Add('předem', 0);
Dict.Add('(např', 0);
Dict.Add('databáze', 0);
Dict.Add('počítačových', 0);
Dict.Add('virů)', 0);
Dict.Add('je', 0);
Dict.Add('možné', 0);
Dict.Add('zkonstruovat', 0);
Dict.Add('automat', 0);
Dict.Add('předem', 0);
Dict.Add('a', 0);
Dict.Add('ten', 0);
Dict.Add('pak', 0);
Dict.Add('uložit', 0);
//*)

  //Dict.Search('aallhellxlog', 12, nil);
  //Dict.DebugPrint();

  InitTest('Nothing to find: empty input', '', -1);
  CheckExp(0, 0,  0, []);
  InitTest('Nothing to find: short input', '@', -1);
  CheckExp(0, 0,  0, []);
  InitTest('Nothing to find: long  input', StringOfChar('#',100), -1);
  CheckExp(0, 0,  0, []);

  // result points to end of word (0 based)
  // end, end, count, idx(from add)
  InitTest('find hell', 'hell', 0);
  CheckExp(4, 4,  1, [4, 5]);
  InitTest('find hell', 'hell', -1);
  CheckExp(4, 4,  1, [4, 5]);

  InitTest('find hell', 'hell1');
  CheckExp(4, 4,  1, [4, 5]);

  InitTest('find hell', '2hell');
  CheckExp(5, 5,  1, [5, 5]);

  InitTest('find hell', '2hell1');
  CheckExp(5, 5,  1, [5, 5]);

  InitTest('find hell', 'hell hell'); // no event stops after 1st
  CheckExp(4, 9,  2, [4, 5,   9, 5]);

  InitTest('find hell', 'hellhell'); // no event stops after 1st
  CheckExp(4, 8,  2, [4, 5,   8, 5]);

  InitTest('find hell', 'hell hell', 0);
  CheckExp(4, 4,  1, [4, 5]);

  InitTest('find hell', 'hellog', -1, 0); // hell is match, log can not be found
  CheckExp(4, 4,  1, [4, 5]);

  InitTest('find log', 'hellog', -1, 1); // skip hell (still in list), find log
  CheckExp(-1, 6,  2, [4, 5,  6, 7]);

  InitTest('find hell', 'hehell', 0);
  CheckExp(6, 6,  1, [6, 5]);

  InitTest('find hell', 'thehell', 0);
  CheckExp(7, 7,  1, [7, 5]);

  InitTest('lantern', 'lantern');
  CheckExp(7, 7,  1, [7, 8]);

  InitTest('find terminal', 'lanterminal');
  CheckExp(11, 11,  1, [11, 9]);

  InitTest('find lo123', 'yello123AA');
  CheckExp(8, 8,  1, [8, 3]);

  InitTest('find lo123', 'yello1234AA');
  CheckExp(8, 8,  1, [8, 3]);

  InitTest('find yello12345 and lo123', 'yello12345AA', -1, 99);
  CheckExp(-1, 10,  2, [8, 3,  10, 2]);

  InitTest('find many', 'YESTERDAY', -1, 99);
  CheckExp(-1, 9,  5, [{TE} 5, 14,  {STER} 6, 13,  {ER} 6, 15,  {YESTERDAY} 9, 12,  {DAY} 9, 16 ]);

  InitTest('find many', 'YESTERDAY'); // restart after each match
  CheckExp(-1, 9,  2, [{TE} 5, 14,  {DAY} 9, 16 ]);



  Dict.Search('aallhellxlog', 12, @DoDictMatch);
  //Dict.BuildDictionary;
  //Dict.DebugPrint();

  //Randomize;
  //Dict.Clear;
  //for i := 0 to 5000 do begin
  //  s := '';
  //  for j := 10 to 11+Random(20) do s := s + chr(Random(127));
  //  Dict.Add(s);
  //end;
  //Dict.BuildDictionary;
  //Dict.DebugPrint(true);

  AssertEquals('GetMatchAtChar longer', 12,Dict.GetMatchAtChar('YESTERDAY ', 10, nil));
  AssertEquals('GetMatchAtChar exact len', 12,Dict.GetMatchAtChar('YESTERDAY ', 9, nil));
  AssertEquals('GetMatchAtChar too short', -1,Dict.GetMatchAtChar('YESTERDAY ', 8, nil));

  Dict.Free;
end;

procedure TTestMarkupHighAll.TestValidateMatches;
  function Mtxt(i: Integer): String;
  begin
    Result := 'a'+IntToStr(i)+'a';
  end;

  function l_a(n: Integer) : TMatchLoc; // location for a match of a123a
  begin
    Result := l(n, 3, 3+Length(Mtxt(n)));
  end;

  procedure StartMatch(const Words: Array of string);
  var
    i: Integer;
  begin
    SynEdit.BeginUpdate;
    FTestMarkup.Clear;
    for i := 0 to high(Words) do
      FTestMarkup.AddSearchTerm(Words[i]);
    SynEdit.EndUpdate;
    FTestMarkup.MarkupInfo.Foreground := clRed;
  end;

  procedure SetText(ATopLine: Integer = 1; HideSingle: Boolean = False);
  var
    i: Integer;
  begin
    (* Create lines:  a123a  b  c123d
       40.5 visible lines
    *)
    ReCreateEdit;
    SynEdit.BeginUpdate;
    for i := 1 to 700 do
      SynEdit.Lines.Add('  a'+IntToStr(i)+'a  b  c'+IntToStr(i)+'d');
    SynEdit.Align := alTop;
    SynEdit.EndUpdate;

    SetLinesInWindow(40);

    FTestMarkup := TTestSynEditMarkupHighlightAllMulti.Create(SynEdit);
    FTestMarkup.HideSingleMatch := HideSingle;
    SynEdit.MarkupMgr.AddMarkUp(FTestMarkup);
    SynEdit.TopLine := ATopLine;
  end;

  procedure SetTextAndStart(ATopLine: Integer = 1; HideSingle: Boolean = False; const Words: Array of string);
  begin
    SetText(ATopLine, HideSingle);
    StartMatch(Words);
  end;

  procedure SetTextAndMatch(ATopLine: Integer; HideSingle: Boolean; const Words: Array of string;
    AName: String= ''; AExpMin: Integer = -1; AExpMax: Integer = -1);
  begin
    SetTextAndStart(ATopLine, HideSingle, Words);
    if AExpMin >= 0 then
      TestHasMCount(AName + ' init', AExpMin, AExpMax);
  end;

  procedure SetTextAndMatch(ATopLine: Integer; HideSingle: Boolean; const Words: Array of string;
    const AExp: Array of TMAtchLoc; AExpMax: Integer;
    AName: String = '');
  begin
    SetText(ATopLine, HideSingle);
    StartMatch(Words);
    TestHasMatches(AName + ' init', length(AExp), AExpMax, AExp);
  end;

  procedure SetTextAndMatch(ATopLine: Integer; HideSingle: Boolean; const Words: Array of string;
    const AExp: Array of TMAtchLoc;
    AName: String = '');
  begin
    SetTextAndMatch(ATopLine, HideSingle, Words, AExp, -1, AName);
  end;

  procedure ScrollAndMatch(ATopLine: Integer; const AExp: Array of TMAtchLoc; AExpMax: Integer;
    AName: String = '');
  begin
    SynEdit.TopLine := ATopLine;
    TestHasMatches(AName + ' scrolled ', length(AExp), AExpMax, AExp);
  end;

  procedure ScrollAndMatch(ATopLine: Integer; const AExp: Array of TMAtchLoc;
    AName: String = '');
  begin
    ScrollAndMatch(ATopLine, AExp, -1, AName);
  end;

  procedure HeightAndMatch(ALineCnt: Integer; const AExp: Array of TMAtchLoc; AExpMax: Integer;
    AName: String = '');
  begin
    SetLinesInWindow(ALineCnt);
    TestHasMatches(AName + ' height ', length(AExp), AExpMax, AExp);
  end;

  procedure HeightAndMatch(ALineCnt: Integer; const AExp: Array of TMAtchLoc;
    AName: String = '');
  begin
    HeightAndMatch(ALineCnt, AExp, -1, AName);
  end;

  procedure EditReplaceAndMatch(Y, X, Y2, X2: Integer; ATxt: String; const AExp: Array of TMAtchLoc; AExpMax: Integer;
    AName: String = '');
  begin
    SynEdit.TextBetweenPoints[point(X, Y), point(X2, Y2)] := ATxt;
    SynEdit.SimulatePaintText;
    TestHasMatches(AName + ' inserted ', length(AExp), AExpMax, AExp);
  end;

  procedure EditReplaceAndMatch(Y, X, Y2, X2: Integer; ATxt: String; const AExp: Array of TMAtchLoc;
    AName: String = '');
  begin
    EditReplaceAndMatch(Y, X, Y2, X2, ATxt, AExp, -1, AName);
  end;

  procedure EditInsertAndMatch(Y, X: Integer; ATxt: String; const AExp: Array of TMAtchLoc; AExpMax: Integer;
    AName: String = '');
  begin
    EditReplaceAndMatch(Y, X, Y, X, ATxt, AExp, AExpMax, AName);
  end;

  procedure EditInsertAndMatch(Y, X: Integer; ATxt: String; const AExp: Array of TMAtchLoc;
    AName: String = '');
  begin
    EditInsertAndMatch(Y, X, ATxt, AExp, -1, AName);
  end;

var
  N: string;
  i, j: integer;
  a, b: integer;
begin

  {%region Searchrange}
    PushBaseName('Searchrange');
    PushBaseName('HideSingleMatch=False');

    SetTextAndMatch(250, False, ['a250a'],                   [l(250, 3, 8)], 'Find match on first line');
    SetTextAndMatch(250, False, ['a289a'],                   [l(289, 3, 8)], 'Find match on last line');
    SetTextAndMatch(250, False, ['a290a'],                   [l(290, 3, 8)], 'Find match on last part visible) line');
    SetTextAndMatch(250, False, ['a249a'],                   [], 'NOT Found before topline');
    SetTextAndMatch(250, False, ['a291a'],                   [], 'NOT Found after lastline');
    // first and last
    SetTextAndMatch(250, False, ['a250a', 'a290a'],          [l(250, 3, 8), l(290, 3, 8)], 'Found on first and last line');
    // first and last + before
    SetTextAndMatch(250, False, ['a250a', 'a290a', 'a249a'], [l(250, 3, 8), l(290, 3, 8)], 'Found on first/last (but not before) line');
    // first and last + after
    SetTextAndMatch(250, False, ['a250a', 'a290a', 'a291a'], [l(250, 3, 8), l(290, 3, 8)], 'Found on first/last (but not after) line');

    PopPushBaseName('HideSingleMatch=True');

    SetTextAndMatch(250, True, ['a250a'],                    [l(250, 3, 8)], 'Found on first line');
    SetTextAndMatch(250, True, ['a289a'],                    [l(289, 3, 8)], 'Found on last line');
    SetTextAndMatch(250, True, ['a290a'],                    [l(290, 3, 8)], 'Found on last (partly) line');
    SetTextAndMatch(250, True, ['a249a'],                    [], 'NOT Found before topline');
    SetTextAndMatch(250, True, ['a291a'],                    [], 'NOT Found after lastline');
    // first and last
    SetTextAndMatch(250, True, ['a250a', 'a290a'],           [l(250, 3, 8), l(290, 3, 8)], 'Found on first and last line');
    // first and last + before
    SetTextAndMatch(250, True, ['a250a', 'a290a', 'a249a'],  [l(250, 3, 8), l(290, 3, 8)], 'Found on first/last (but not before) line');
    // first and last + after
    SetTextAndMatch(250, True, ['a250a', 'a290a', 'a291a'],  [l(250, 3, 8), l(290, 3, 8)], 'Found on first/last (but not after) line');

    // extend for HideSingle, before
    SetTextAndMatch(250, True, ['a250a', 'a249a'],           [l(250, 3, 8), l(249, 3, 8)], 'Look for 2nd match before startpoint (first match at topline)');
    SetTextAndMatch(250, True, ['a290a', 'a249a'],           [l(290, 3, 8), l(249, 3, 8)], 'Look for 2nd match before startpoint (first match at lastline)');
    SetTextAndMatch(250, True, ['a250a', 'a151a'],           [l(250, 3, 8), l(151, 3, 8)], 'Look for 2nd match FAR (99l) before startpoint (first match at topline)');
    SetTextAndMatch(250, True, ['a290a', 'a151a'],           [l(290, 3, 8), l(151, 3, 8)], 'Look for 2nd match FAR (99l) before startpoint (first match at lastline)');
    SetTextAndMatch(250, True, ['a250a', 'a200a', 'a210a'],  [l(250, 3, 8), l( -2, 0, 0)], 'Look for 2nd match before startpoint, find ONE of TWO');

    // TODO: Not extend too far...

    // extend for HideSingle, after
    SetTextAndMatch(250, True, ['a250a', 'a291a'],           [l(250, 3, 8), l(291, 3, 8)], 'Found on first/ext-after line');
    SetTextAndMatch(250, True, ['a290a', 'a291a'],           [l(290, 3, 8), l(291, 3, 8)], 'Found on last/ext-after line');
    SetTextAndMatch(250, True, ['a250a', 'a389a'],           [l(250, 3, 8), l(389, 3, 8)], 'Found on first/ext-after-99 line');
    SetTextAndMatch(250, True, ['a290a', 'a389a'],           [l(290, 3, 8), l(389, 3, 8)], 'Found on last/ext-after-99 line');

    PopBaseName;
    PopBaseName;
  {%endregion}

  {%region Scroll / LinesInWindow}
    PushBaseName('Scroll/LinesInWindow');
    PushBaseName('HideSingleMatch=False');


    SetTextAndMatch(250, False, ['a249a'], [], 'Not Found before first line');
    ScrollAndMatch (251,                   [], 'Not Found before first line (250=>251)');

    ScrollAndMatch(249,                   [l(249, 3, 8)], 'Found on first line (251=>249)');


    SetTextAndMatch(250, False, ['a291a'], [], 'Not Found after last line');
    ScrollAndMatch (249,                   [], 'Not Found after last line (250=>249)');

    ScrollAndMatch (251,                   [l(291, 3, 8)], 'Found on last line (249=>251)');


    SetTextAndMatch(250, False, ['a291a'], [], 'Not Found after last line');
    HeightAndMatch (41,                    [l(291, 3, 8)], 'Found on last line (40=>41)' );

    PopPushBaseName('HideSingleMatch=True');

    SetTextAndMatch(250, True, ['a249a', 'a248a'], [], 'Not Found before first line');
    ScrollAndMatch (251,                           [], 'Not Found before first line (250=>251)');

    ScrollAndMatch (249,                           [l(249, 3, 8), l(248, 3, 8)], 'Found on first line+ext (251=>249)');


    SetTextAndMatch(250, True, ['a291a', 'a292a'], [], 'Not Found after last line');
    ScrollAndMatch (249,                           [], 'Not Found after last line (250=>249)');

    ScrollAndMatch (251,                           [l(291, 3, 8), l(292, 3, 8)], 'Found on last line+ext (249=>251)');


    for i := -249 to 315 do begin
      // MATCHES_CLEAN_LINE_THRESHOLD = 300  div 4 = 75
      // MATCHES_CLEAN_LINE_KEEP = 200       div 4 = 50
      // SynEdit.LinesInWindow = 50          div 4 = 10
      if not( (abs(i) div 4) in [1,    9,10,   49,50,51,   74,75,76 ]) then continue;

      SetTextAndMatch(250, False, [Mtxt(250), Mtxt(290), Mtxt(290+i)], [l_a(250), l_a(290)],   3);
      ScrollAndMatch (250 + i,                                         [          l_a(290+i)], 3, 'Far Scroll '+IntToStr(i)+' to %d matches top/last > last ' + 'Found ');

      SetTextAndMatch(250, False, [Mtxt(250), Mtxt(290), Mtxt(250+i)], [l_a(250), l_a(290)], 3);
      ScrollAndMatch (250 + i,                                         [l_a(250+i)        ], 3,  'Far Scroll '+IntToStr(i)+' to %d matches top/last > top ' + 'Found ');

      SetTextAndMatch(250, False, [Mtxt(250), Mtxt(290+i)], [l_a(250)],   2);
      ScrollAndMatch (250 + i,                              [l_a(290+i)], 2, 'Far Scroll '+IntToStr(i)+' to %d matches top > last ' + 'Found ');

      SetTextAndMatch(250, False, [Mtxt(250), Mtxt(250+i)], [l_a(250)],   2);
      ScrollAndMatch (250 + i,                              [l_a(250+i)], 2, 'Far Scroll '+IntToStr(i)+' to %d matches top > top ' + 'Found ');

      SetTextAndMatch(250, False, [Mtxt(290), Mtxt(290+i)], [l_a(290)],   2);
      ScrollAndMatch (250 + i,                              [l_a(290+i)], 2, 'Far Scroll '+IntToStr(i)+' to %d matches last > last ' + 'Found ');

      SetTextAndMatch(250, False, [Mtxt(290), Mtxt(250+i)], [l_a(290)],   2);
      ScrollAndMatch (250 + i,                              [l_a(250+i)], 2, 'Far Scroll '+IntToStr(i)+' to %d matches last > top ' + 'Found ');
    end;


    PopBaseName;
    PopBaseName;
  {%endregion}

  {%region edit}
    PushBaseName('Searchrange');
    //PushBaseName('HideSingleMatch=False');

    for i := 245 to 295 do begin
      if ((i > 259) and (i < 280)) then continue;

      N := 'Edit at '+IntToStr(i)+' / NO match';
      SetTextAndMatch(250, False,   ['DontMatchMe'], [],  N+' init/found');
      EditInsertAndMatch(i,1,'X',                    [],  N+' Found after edit');


      N := 'Edit (new line) at '+IntToStr(i)+' / NO match';
      SetTextAndMatch(250, False,   ['DontMatchMe'], [],  N+' init/found');
      EditInsertAndMatch (i,1,LineEnding,            [],  N+' Found after edit');


      N := 'Edit (join line) at '+IntToStr(i)+' / NO match';
      SetTextAndMatch(250, False,   ['DontMatchMe'], [],  N+' init/found');
      EditReplaceAndMatch (i,10, i+1,1, '',          [],  N+' Found after edit');

    end;


    for j := 245 to 295 do begin
      if ((j > 255) and (j < 270)) or ((j > 270) and (j < 285)) then
        continue;

      for i := 245 to 295 do begin
        N := 'Edit at '+IntToStr(i)+' / single match at '+IntToStr(j);
        SetTextAndMatch(250, False,   ['a'+IntToStr(j)+'a']);
        if (j >= 250) and (j <= 290)
        then TestHasMatches(N+' init/found',   1,   [l(j, 3, 8)])
        else TestHasMCount (N+' init/not found', 0);

        if (j >= 250) and (j <= 290) then begin
          if i = j
          then EditInsertAndMatch(i,1,'X', [l(j, 4, 9)],  N+' Found after edit')
          else EditInsertAndMatch(i,1,'X', [l(j, 3, 8)],  N+' Found after edit');
        end
        else
          EditInsertAndMatch(i,1,'X', [],  N+' still not found after edit');

      end;


      for i := 245 to 295 do begin
        N := 'Edit (new line) at '+IntToStr(i)+' / single match at '+IntToStr(j);
        SetTextAndMatch(250, False,   ['a'+IntToStr(j)+'a']);
        if (j >= 250) and (j <= 290)
        then TestHasMatches(N+' init/found',   1,   [l(j, 3, 8)])
        else TestHasMCount (N+' init/not found', 0);

        SynEdit.BeginUpdate;
        SynEdit.TextBetweenPoints[point(1, i), point(1, i)] := LineEnding;
        SynEdit.TopLine := 250;
        SynEdit.EndUpdate;
        SynEdit.SimulatePaintText;
        a := j;
        if i <= j then inc(a);
        if (a >= 250) and (a <= 290) then begin
          if i = a
          then TestHasMatches(N+' Found after edit',   1,  [l(a, 4, 9)])
          else TestHasMatches(N+' Found after edit',   1,  [l(a, 3, 8)]);
        end
        else
          TestHasMCount (N+' still not Found after edit', 0);
      end;

    end;



    for j := 0 to 6 do begin
      case j of
        0: begin a := 260; b := 270 end;
        1: begin a := 250; b := 270 end;
        2: begin a := 251; b := 270 end;
        3: begin a := 270; b := 288 end;
        4: begin a := 270; b := 289 end;
        5: begin a := 270; b := 290 end;
        6: begin a := 250; b := 290 end;
      end;

      for i := 245 to 295 do begin
        N := 'Edit at '+IntToStr(i)+' / TWO match at '+IntToStr(a)+', '+IntToStr(b);
        SetTextAndMatch(250, False,   ['a'+IntToStr(a)+'a', 'a'+IntToStr(b)+'a']);
        TestHasMatches(N+' init/found',   2,  [l(a, 3, 8), l(b, 3,8)]);

        SynEdit.TextBetweenPoints[point(10, i), point(10, i)] := 'X';
        SynEdit.SimulatePaintText;
        TestHasMCount (N+' Found after edit', 2);
        TestHasMatches(N+' init/found', [l(a, 3, 8), l(b, 3,8)]);

      end;
    end;


    N := 'Edit/Topline/LastLine ';
    SetTextAndMatch(250, False,   ['a265a', 'a275a']);
    TestHasMatches(N+' init/found',   2,  [l(265, 3, 8), l(275, 3,8)]);
    SynEdit.BeginUpdate;
    SynEdit.TextBetweenPoints[point(10, i), point(10, i)] := 'X';
    SynEdit.TopLine := 248; // 2 new lines
    SetLinesInWindow(44);
    SynEdit.EndUpdate;
    SynEdit.SimulatePaintText;
    TestHasMatches(N+' Found after edit',   2,  [l(265, 3, 8), l(275, 3,8)]);


    N := 'Edit/Topline/LastLine find new points';
    SetTextAndMatch(250, False,   ['a265a', 'a275a', 'a248a', 'a292a']);
    TestHasMatches(N+' init/found',   2,  [l(265, 3, 8), l(275, 3,8)]);
    SynEdit.BeginUpdate;
    SynEdit.TextBetweenPoints[point(10, i), point(10, i)] := 'X';
    SynEdit.TopLine := 248; // 2 new lines
    SetLinesInWindow(44);
    SynEdit.EndUpdate;
    SynEdit.SimulatePaintText;
    TestHasMatches(N+' Found after edit',   4,  [l(265, 3, 8), l(275, 3,8), l(248, 3,8), l(292, 3,8)]);

    PopBaseName;
  {%endregion}

  // Edit / before,after,gap
  for i := -2 to 2 do begin
    SetTextAndMatch   (250, False, [Mtxt(260), 'FOO'], [l_a(260)]);
    EditInsertAndMatch(260+i,10, ' FOO ',              [l_a(260), l(260+i,11,14)]);

    for j := 1 to 3 do begin  // distance
      // 2 lines
      SetTextAndMatch   (250, False, [Mtxt(260),Mtxt(260+j),'FOO'], [l_a(260),l_a(260+j)]);
      EditInsertAndMatch(260+i,10, ' FOO ',                         [l_a(260),l_a(260+j), l(260+i,11,14)]);

      SetTextAndMatch   (250, False, [Mtxt(260),Mtxt(260+j),'FOO'], [l_a(260),l_a(260+j)]);
      EditInsertAndMatch(260+j+i,10, ' FOO ',                       [l_a(260),l_a(260+j), l(260+j+i,11,14)]);

      // 3 lines
      SetTextAndMatch   (250, False, [Mtxt(260),Mtxt(260+j),Mtxt(260+j*2),'FOO'],
                                                      [l_a(260),l_a(260+j),l_a(260+j*2)]);
      EditInsertAndMatch(260+i,10, ' FOO ',           [l_a(260),l_a(260+j),l_a(260+j*2), l(260+i,11,14)]);

      SetTextAndMatch   (250, False, [Mtxt(260),Mtxt(260+j),Mtxt(260+j*2),'FOO'],
                                                      [l_a(260),l_a(260+j),l_a(260+j*2)]);
      EditInsertAndMatch(260+j+i,10, ' FOO ',         [l_a(260),l_a(260+j),l_a(260+j*2), l(260+j+i,11,14)]);

      SetTextAndMatch   (250, False, [Mtxt(260),Mtxt(260+j),Mtxt(260+j*2),'FOO'],
                                                      [l_a(260),l_a(260+j),l_a(260+j*2)]);
      EditInsertAndMatch(260+j*2+i,10, ' FOO ',         [l_a(260),l_a(260+j),l_a(260+j*2), l(260+j*2+i,11,14)]);

    end;
  end;

  //Delete part of match

  SetTextAndMatch   (250, False, [Mtxt(260), Mtxt(263), Mtxt(266), Mtxt(269)],
                                       [l_a(260),l_a(263),l_a(266),l_a(269)]);
  EditReplaceAndMatch(260,3,260,7, '', [         l_a(263),l_a(266),l_a(269)]);
  EditReplaceAndMatch(266,3,266,7, '', [         l_a(263),         l_a(269)]);
  EditReplaceAndMatch(269,3,269,7, '', [         l_a(263)                  ]);
  EditReplaceAndMatch(263,3,263,7, '', [                                   ]);


  SetTextAndMatch   (250, False, [Mtxt(260), Mtxt(263), Mtxt(266), Mtxt(269)],
                                       [l_a(260),l_a(263),l_a(266),l_a(269)]);
  EditReplaceAndMatch(263,3,263,7, '', [l_a(260),         l_a(266),l_a(269)]);
  EditReplaceAndMatch(269,3,269,7, '', [l_a(260),         l_a(266)         ]);
  EditReplaceAndMatch(260,3,260,7, '', [                  l_a(266)         ]);
  EditReplaceAndMatch(266,3,266,7, '', [                                   ]);

  SetTextAndMatch   (250, False, [Mtxt(260), Mtxt(263), Mtxt(266), Mtxt(269)],
                                       [l_a(260),l_a(263),l_a(266),l_a(269)]);
  EditReplaceAndMatch(266,3,266,7, '', [l_a(260),l_a(263),         l_a(269)]);
  EditReplaceAndMatch(269,3,269,7, '', [l_a(260),l_a(263)                  ]);
  EditReplaceAndMatch(260,3,260,7, '', [         l_a(263)                  ]);

  SetTextAndMatch   (250, False, [Mtxt(260), Mtxt(263), Mtxt(266), Mtxt(269)],
                                       [l_a(260),l_a(263),l_a(266),l_a(269)]);
  EditReplaceAndMatch(269,3,269,7, '', [l_a(260),l_a(263),l_a(266)         ]);
  EditReplaceAndMatch(263,3,263,7, '', [l_a(260),         l_a(266)         ]);


end;

initialization

  RegisterTest(TTestMarkupHighAll);
end.

