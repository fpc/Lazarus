unit TestMarkupHighAll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase, LCLProc, Controls,
  Graphics, SynEdit, SynEditMarkupHighAll;

type

  { TTestMarkupHighAll }

  TTestMarkupHighAll = class(TTestBase)
  private
    FMatchList: Array of record
        p: PChar;
        l: Integer;
      end;
    FStopAtMatch, FNoMatchCount: Integer;
    procedure DoDictMatch(Match: PChar; MatchIdx: Integer; var IsMatch: Boolean;
      var StopSeach: Boolean);
  protected
    //procedure SetUp; override; 
    //procedure TearDown; override; 
    //procedure ReCreateEdit; reintroduce;
    //function TestText1: TStringArray;
  published
    procedure TestDictionary;
    procedure TestValidateMatches;
  end; 

implementation

  type

    { TTestSynEditMarkupHighlightAllMulti }

    TTestSynEditMarkupHighlightAllMulti = class(TSynEditMarkupHighlightAllMulti)
    private
      FScannedLineCount: Integer;
    protected
      function FindMatches(AStartPoint, AEndPoint: TPoint; var AIndex: Integer;
        AStopAfterLine: Integer = - 1; ABackward: Boolean = False): TPoint; override;
    public
      procedure ResetScannedCount;
      property Matches;
      property ScannedLineCount: Integer read FScannedLineCount;
    end;

{ TTestSynEditMarkupHighlightAllMulti }

function TTestSynEditMarkupHighlightAllMulti.FindMatches(AStartPoint, AEndPoint: TPoint;
  var AIndex: Integer; AStopAfterLine: Integer; ABackward: Boolean): TPoint;
begin
  FScannedLineCount := FScannedLineCount + AEndPoint.y - AStartPoint.y + 1;
  Result := inherited FindMatches(AStartPoint, AEndPoint, AIndex, AStopAfterLine, ABackward);
end;

procedure TTestSynEditMarkupHighlightAllMulti.ResetScannedCount;
begin
  FScannedLineCount := 0;
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

  procedure CheckExp(AExpCount: Integer; AExpList: array of Integer);
  var
    i: Integer;
  begin
    AssertEquals(Name+' (len list)', AExpCount, Length(FMatchList));
    for i := 0 to Length(AExpList) div 2 -1 do begin
      AssertEquals(Name+' (start '+IntToStr(i)+')', AExpList[i*2], FMatchList[i].p - @LineText[1]);
      AssertEquals(Name+' (len '+IntToStr(i)+')', AExpList[i*2+1], FMatchList[i].l);
    end;
  end;

  procedure CheckExp(ExpRes1, ExpRes2, AExpCount: Integer; AExpList: array of Integer);
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
type
  TMatchLoc = record
    y1, y2, x1, x2: Integer;
  end;
var
  M: TTestSynEditMarkupHighlightAllMulti;

  function l(y, x1, x2: Integer) : TMatchLoc;
  begin
    Result.y1 := y;
    Result.x1 := x1;
    Result.y2 := y;
    Result.x2 := x2;
  end;

  procedure StartMatch(Words: Array of string);
  var
    i: Integer;
  begin
    SynEdit.BeginUpdate;
    M.Clear;
    for i := 0 to high(Words) do
      M.AddSearchTerm(Words[i]);
    SynEdit.EndUpdate;
    m.MarkupInfo.Foreground := clRed;
  end;

  Procedure TestHasMCount(AName: String; AExpMin: Integer; AExpMax: Integer = -1);
  begin
    AName := AName + '(CNT)';
    if AExpMax < 0 then begin
      AssertEquals(BaseTestName+' '+AName, AExpMin, M.Matches.Count);
    end
    else begin
      AssertTrue(BaseTestName+' '+AName+ '(Min)', AExpMin <= M.Matches.Count);
      AssertTrue(BaseTestName+' '+AName+ '(Max)', AExpMax >= M.Matches.Count);
    end;
  end;

  Procedure TestHasMatches(AName: String; AExp: Array of TMAtchLoc; ExpMusNotExist: Boolean = False);
  var
    i, j: Integer;
  begin
    for i := 0 to High(AExp) do begin
      j := M.Matches.Count - 1;
      while (j >= 0) and
        ( (M.Matches.StartPoint[j].y <> AExp[i].y1) or (M.Matches.StartPoint[j].x <> AExp[i].x1) or
          (M.Matches.EndPoint[j].y <> AExp[i].y2) or (M.Matches.EndPoint[j].x <> AExp[i].x2) )
      do
        dec(j);
      AssertEquals(BaseTestName+' '+AName+'('+IntToStr(i)+')', not ExpMusNotExist, j >= 0);
    end
  end;

  Procedure TestHasMatches(AName: String; AExpCount: Integer; AExp: Array of TMAtchLoc; ExpMusNotExist: Boolean = False);
  begin
    TestHasMatches(AName, AExp, ExpMusNotExist);
    TestHasMCount(AName, AExpCount);
  end;

  Procedure TestHasMatches(AName: String; AExpCountMin, AExpCountMax: Integer; AExp: Array of TMAtchLoc; ExpMusNotExist: Boolean = False);
  begin
    TestHasMatches(AName, AExp, ExpMusNotExist);
    TestHasMCount(AName, AExpCountMin, AExpCountMax);
  end;

  Procedure TestHasScanCnt(AName: String; AExpMin: Integer; AExpMax: Integer = -1);
  begin
    AName := AName + '(SCANNED)';
    if AExpMax < 0 then begin
      AssertEquals(BaseTestName+' '+AName, AExpMin, M.ScannedLineCount);
    end
    else begin
      AssertTrue(BaseTestName+' '+AName+ '(Min)', AExpMin <= M.ScannedLineCount);
      AssertTrue(BaseTestName+' '+AName+ '(Max)', AExpMax >= M.ScannedLineCount);
    end;
  end;

  procedure SetText(ATopLine: Integer = 1; HideSingle: Boolean = False);
  var
    i: Integer;
  begin
    ReCreateEdit;
    SynEdit.BeginUpdate;
    for i := 1 to 700 do
      SynEdit.Lines.Add('  a'+IntToStr(i)+'a  b  c'+IntToStr(i)+'d');
    SynEdit.Align := alTop;
    SynEdit.Height := SynEdit.LineHeight * 40 + SynEdit.LineHeight div 2;
    M := TTestSynEditMarkupHighlightAllMulti.Create(SynEdit);
    M.HideSingleMatch := HideSingle;
    SynEdit.MarkupMgr.AddMarkUp(M);
    SynEdit.TopLine := ATopLine;
    SynEdit.EndUpdate;
  end;

  procedure SetTextAndMatch(ATopLine: Integer; HideSingle: Boolean;
    Words: Array of string;
    AName: String= ''; AExpMin: Integer = -1; AExpMax: Integer = -1);
  begin
    SetText(ATopLine, HideSingle);
    StartMatch(Words);
    if AExpMin >= 0 then
      TestHasMCount(AName + ' init', AExpMin, AExpMax);
  end;

  function Mtxt(i: Integer): String;
  begin
    Result := 'a'+IntToStr(i)+'a';
  end;

var
  N: string;
  i, j: integer;
  a, b: integer;
begin

  {%region Searchrange}
    PushBaseName('Searchrange');
    PushBaseName('HideSingleMatch=False');

    N := 'Find match on first line';
    SetText(250);
    M.HideSingleMatch := False;
    StartMatch(['a250a']);
    TestHasMCount (N, 1);
    TestHasMatches(N, [l(250, 3, 8)]);

    N := 'Find match on last line';
    SetText(250);
    M.HideSingleMatch := False;
    StartMatch(['a289a']);
    TestHasMCount (N, 1);
    TestHasMatches(N, [l(289, 3, 8)]);

    N := 'Find match on last part visible) line';
    SetText(250);
    M.HideSingleMatch := False;
    StartMatch(['a290a']);
    TestHasMCount (N, 1);
    TestHasMatches(N, [l(290, 3, 8)]);

    // Before topline
    SetText(250);
    M.HideSingleMatch := False;
    StartMatch(['a249a']);
    TestHasMCount ('NOT Found before topline', 0);

    // after lastline
    SetText(250);
    M.HideSingleMatch := False;
    StartMatch(['a291a']);
    TestHasMCount ('NOT Found after lastline', 0);

    // first and last
    SetText(250);
    M.HideSingleMatch := False;
    StartMatch(['a250a', 'a290a']);
    TestHasMCount ('Found on first and last line', 2);
    TestHasMatches('Found on first and last line', [l(250, 3, 8), l(290, 3, 8)]);

    // first and last + before
    SetText(250);
    M.HideSingleMatch := False;
    StartMatch(['a250a', 'a290a', 'a249a']);
    TestHasMCount ('Found on first/last (but not before) line', 2);
    TestHasMatches('Found on first/last (but not before) line', [l(250, 3, 8), l(290, 3, 8)]);

    // first and last + after
    SetText(250);
    M.HideSingleMatch := False;
    StartMatch(['a250a', 'a290a', 'a291a']);
    TestHasMCount ('Found on first/last (but not after) line', 2);
    TestHasMatches('Found on first/last (but not after) line', [l(250, 3, 8), l(290, 3, 8)]);

    PopPushBaseName('HideSingleMatch=True');

    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a250a']);
    TestHasMCount ('Found on first line', 1);
    TestHasMatches('Found on first line', [l(250, 3, 8)]);

    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a289a']);
    TestHasMCount ('Found on last line', 1);
    TestHasMatches('Found on last line', [l(289, 3, 8)]);

    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a290a']);
    TestHasMCount ('Found on last (partly) line', 1);
    TestHasMatches('Found on last (partly) line', [l(290, 3, 8)]);

    // Before topline
    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a249a']);
    TestHasMCount ('NOT Found before topline', 0);

    // after lastline
    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a291a']);
    TestHasMCount ('NOT Found after lastline', 0);

    // first and last
    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a250a', 'a290a']);
    TestHasMCount ('Found on first and last line', 2);
    TestHasMatches('Found on first and last line', [l(250, 3, 8), l(290, 3, 8)]);

    // first and last + before
    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a250a', 'a290a', 'a249a']);
    TestHasMCount ('Found on first/last (but not before) line', 2);
    TestHasMatches('Found on first/last (but not before) line', [l(250, 3, 8), l(290, 3, 8)]);

    // first and last + after
    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a250a', 'a290a', 'a291a']);
    TestHasMCount ('Found on first/last (but not after) line', 2);
    TestHasMatches('Found on first/last (but not after) line', [l(250, 3, 8), l(290, 3, 8)]);

    // extend for HideSingle, before
    N := 'Look for 2nd match before startpoint (first match at topline)';
    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a250a', 'a249a']);
    TestHasMCount (N, 2);
    TestHasMatches(N, [l(250, 3, 8), l(249, 3, 8)]);

    N := 'Look for 2nd match before startpoint (first match at lastline)';
    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a290a', 'a249a']);
    TestHasMCount (N, 2);
    TestHasMatches(N, [l(290, 3, 8), l(249, 3, 8)]);

    N := 'Look for 2nd match FAR (99l) before startpoint (first match at topline)';
    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a250a', 'a151a']);
    TestHasMCount (N, 2);
    TestHasMatches(N, [l(250, 3, 8), l(151, 3, 8)]);

    N := 'Look for 2nd match FAR (99l) before startpoint (first match at lastline)';
    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a290a', 'a151a']);
    TestHasMCount (N, 2);
    TestHasMatches(N, [l(290, 3, 8), l(151, 3, 8)]);

    N := 'Look for 2nd match before startpoint, find ONE of TWO';
    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a250a', 'a200a', 'a210a']);
    TestHasMCount (N, 2);
    TestHasMatches(N, [l(250, 3, 8), l(210, 3, 8)]);

    // TODO: Not extend too far...

    // extend for HideSingle, after
    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a250a', 'a291a']);
    TestHasMCount ('Found on first/ext-after line', 2);
    TestHasMatches('Found on first/ext-after line', [l(250, 3, 8), l(291, 3, 8)]);

    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a290a', 'a291a']);
    TestHasMCount ('Found on last/ext-after line', 2);
    TestHasMatches('Found on last/ext-after line', [l(290, 3, 8), l(291, 3, 8)]);

    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a250a', 'a389a']);
    TestHasMCount ('Found on first/ext-after-99 line', 2);
    TestHasMatches('Found on first/ext-after-99 line', [l(250, 3, 8), l(389, 3, 8)]);

    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a290a', 'a389a']);
    TestHasMCount ('Found on last/ext-after-99 line', 2);
    TestHasMatches('Found on last/ext-after-99 line', [l(290, 3, 8), l(389, 3, 8)]);


    PopBaseName;
    PopBaseName;
  {%endregion}

  {%region Scroll / LinesInWindow}
    PushBaseName('Scroll/LinesInWindow');
    PushBaseName('HideSingleMatch=False');


    SetText(250);
    M.HideSingleMatch := False;
    StartMatch(['a249a']);
    TestHasMCount ('Not Found before first line', 0);

    M.ResetScannedCount;
    SynEdit.TopLine := 251;
    TestHasMCount ('Not Found before first line (250=>251)', 0);
    TestHasScanCnt('Not Found before first line (250=>251)', 1, 2); // Allow some range

    M.ResetScannedCount;
    SynEdit.TopLine := 249;
    TestHasMCount ('Found on first line (251=<249', 1);
    TestHasMatches('Found on first line (251=>249)', [l(249, 3, 8)]);
    TestHasScanCnt('Found on first line (251=>249)', 1, 2); // Allow some range


    SetText(250);
    M.HideSingleMatch := False;
    StartMatch(['a291a']);
    TestHasMCount ('Not Found after last line', 0);

    M.ResetScannedCount;
    SynEdit.TopLine := 249;
    TestHasMCount ('Not Found after last line (250=>249)', 0);
    TestHasScanCnt('Not Found after last line (250=>249)', 1, 2); // Allow some range

    M.ResetScannedCount;
    SynEdit.TopLine := 251;
    TestHasMCount ('Found on last line (249=<251', 1);
    TestHasMatches('Found on last line (249=>251)', [l(291, 3, 8)]);
    TestHasScanCnt('Found on last line (249=>251)', 1, 2); // Allow some range


    SetText(250);
    M.HideSingleMatch := False;
    StartMatch(['a291a']);
    TestHasMCount ('Not Found after last line', 0);

    M.ResetScannedCount;
    SynEdit.Height := SynEdit.LineHeight * 41 + SynEdit.LineHeight div 2;
    TestHasMCount ('Found on last line (40=>41', 1);
    TestHasMatches('Found on last line (40=>41)', [l(291, 3, 8)]);
    TestHasScanCnt('Found on last line (40=>41)', 1, 2); // Allow some range


    PopPushBaseName('HideSingleMatch=True');

    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a249a', 'a248a']);
    TestHasMCount ('Not Found before first line', 0);

    M.ResetScannedCount;
    SynEdit.TopLine := 251;
    TestHasMCount ('Not Found before first line (250=>251)', 0);
    TestHasScanCnt('Not Found before first line (250=>251)', 1, 2); // Allow some range

    M.ResetScannedCount;
    SynEdit.TopLine := 249;
    TestHasMCount ('Found on first line+ext (251=<249', 2);
    TestHasMatches('Found on first line+ext (251=>249)', [l(249, 3, 8), l(248, 3, 8)]);


    SetText(250);
    M.HideSingleMatch := True;
    StartMatch(['a291a', 'a292a']);
    TestHasMCount ('Not Found after last line', 0);

    M.ResetScannedCount;
    SynEdit.TopLine := 249;
    TestHasMCount ('Not Found after last line (250=>249)', 0);
    TestHasScanCnt('Not Found after last line (250=>249)', 1, 2); // Allow some range

    M.ResetScannedCount;
    SynEdit.TopLine := 251;
    TestHasMCount ('Found on last line+ext (249=<251', 2);
    TestHasMatches('Found on last line+ext (249=>251)', [l(291, 3, 8), l(292, 3, 8)]);


    for i := -205 to 205 do begin
      if abs(i) in [0..2, 5..15, 25..35, 50..95, 105..195] then continue;

      N := 'Far Scroll '+IntToStr(i)+' to %d matches top/last > last ';
      SetTextAndMatch(250, False, [Mtxt(250), Mtxt(290), Mtxt(290+i)]);
      SynEdit.TopLine := 250 + i;
      TestHasMatches(N + 'Found ', [l(290+i, 3, 3+length(Mtxt(290+i)))]);


      N := 'Far Scroll '+IntToStr(i)+' to %d matches top/last > top ';
      SetTextAndMatch(250, False, [Mtxt(250), Mtxt(290), Mtxt(250+i)]);
      SynEdit.TopLine := 250 + i;
      TestHasMatches(N + 'Found ', [l(250+i, 3, 3+length(Mtxt(250+i)))]);


      N := 'Far Scroll '+IntToStr(i)+' to %d matches top > last ';
      SetTextAndMatch(250, False, [Mtxt(250), Mtxt(290+i)]);
      SynEdit.TopLine := 250 + i;
      TestHasMatches(N + 'Found ', [l(290+i, 3, 3+length(Mtxt(290+i)))]);


      N := 'Far Scroll '+IntToStr(i)+' to %d matches top > top ';
      SetTextAndMatch(250, False, [Mtxt(250), Mtxt(250+i)]);
      SynEdit.TopLine := 250 + i;
      TestHasMatches(N + 'Found ', [l(250+i, 3, 3+length(Mtxt(250+i)))]);


      N := 'Far Scroll '+IntToStr(i)+' to %d matches last > last ';
      SetTextAndMatch(250, False, [Mtxt(290), Mtxt(290+i)]);
      SynEdit.TopLine := 250 + i;
      TestHasMatches(N + 'Found ', [l(290+i, 3, 3+length(Mtxt(290+i)))]);


      N := 'Far Scroll '+IntToStr(i)+' to %d matches last > top ';
      SetTextAndMatch(250, False, [Mtxt(290), Mtxt(250+i)]);
      SynEdit.TopLine := 250 + i;
      TestHasMatches(N + 'Found ', [l(250+i, 3, 3+length(Mtxt(250+i)))]);


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
      SetTextAndMatch(250, False,   ['DontMatchMe'],   N+' init/found', 0);
      M.ResetScannedCount;
      SynEdit.TextBetweenPoints[point(1, i), point(1, i)] := 'X';
      SynEdit.SimulatePaintText;
      TestHasMCount (N+' Found after edit', 0);
      if (i >= 250) and (i <= 290)
      then TestHasScanCnt(N+' Found after edit', 1, 3)
      else
      if (i < 247) or (i > 293)
      then TestHasScanCnt(N+' Found after edit', 0)
      else TestHasScanCnt(N+' Found after edit', 0, 3);


      N := 'Edit (new line) at '+IntToStr(i)+' / NO match';
      SetTextAndMatch(250, False,   ['DontMatchMe'],   N+' init/found', 0);
      M.ResetScannedCount;
      SynEdit.TextBetweenPoints[point(1, i), point(1, i)] := LineEnding;
      SynEdit.SimulatePaintText;
      TestHasMCount (N+' Found after edit', 0);
      //if (i >= 250) and (i <= 290)
      //then TestHasScanCnt(N+' Found after edit', 1, 3)
      //else
      //if (i < 247) or (i > 293)
      //then TestHasScanCnt(N+' Found after edit', 0)
      //else TestHasScanCnt(N+' Found after edit', 0, 3);


      N := 'Edit (join line) at '+IntToStr(i)+' / NO match';
      SetTextAndMatch(250, False,   ['DontMatchMe'],   N+' init/found', 0);
      M.ResetScannedCount;
      SynEdit.TextBetweenPoints[point(10, i), point(1, i+1)] := '';
      SynEdit.SimulatePaintText;
      TestHasMCount (N+' Found after edit', 0);
      //if (i >= 250) and (i <= 290)
      //then TestHasScanCnt(N+' Found after edit', 1, 3)
      //else
      //if (i < 247) or (i > 293)
      //then TestHasScanCnt(N+' Found after edit', 0)
      //else TestHasScanCnt(N+' Found after edit', 0, 3);

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

        M.ResetScannedCount;
        SynEdit.TextBetweenPoints[point(1, i), point(1, i)] := 'X';
        SynEdit.SimulatePaintText;
        if (j >= 250) and (j <= 290) then begin
          if i = j
          then TestHasMatches(N+' Found after edit',   1,  [l(j, 4, 9)])
          else TestHasMatches(N+' Found after edit',   1,  [l(j, 3, 8)]);
        end
        else
          TestHasMCount (N+' still not Found after edit', 0);

        if (i >= 250) and (i <= 290)
        then TestHasScanCnt(N+' Found after edit', 1, 3)
        else
        if (i < 247) or (i > 293)
        then TestHasScanCnt(N+' Found after edit', 0)
        else TestHasScanCnt(N+' Found after edit', 0, 3);
      end;


      for i := 245 to 295 do begin
        N := 'Edit (new line) at '+IntToStr(i)+' / single match at '+IntToStr(j);
        SetTextAndMatch(250, False,   ['a'+IntToStr(j)+'a']);
        if (j >= 250) and (j <= 290)
        then TestHasMatches(N+' init/found',   1,   [l(j, 3, 8)])
        else TestHasMCount (N+' init/not found', 0);

        M.ResetScannedCount;
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

        //if (i >= 250) and (i <= 290)
        //then TestHasScanCnt(N+' Found after edit', 1, 3)
        //else
        //if (i < 247) or (i > 293)
        //then TestHasScanCnt(N+' Found after edit', 0)
        //else TestHasScanCnt(N+' Found after edit', 0, 3);
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

        M.ResetScannedCount;
        SynEdit.TextBetweenPoints[point(10, i), point(10, i)] := 'X';
        SynEdit.SimulatePaintText;
        TestHasMCount (N+' Found after edit', 2);
        TestHasMatches(N+' init/found', [l(a, 3, 8), l(b, 3,8)]);

        if (i >= 250) and (i <= 290)
        then TestHasScanCnt(N+' Found after edit', 1, 3)
        else
        if (i < 247) or (i > 293)
        then TestHasScanCnt(N+' Found after edit', 0)
        else TestHasScanCnt(N+' Found after edit', 0, 3);
      end;
    end;


    N := 'Edit/Topline/LastLine ';
    SetTextAndMatch(250, False,   ['a265a', 'a275a']);
    TestHasMatches(N+' init/found',   2,  [l(265, 3, 8), l(275, 3,8)]);
    M.ResetScannedCount;
    SynEdit.BeginUpdate;
    SynEdit.TextBetweenPoints[point(10, i), point(10, i)] := 'X';
    SynEdit.TopLine := 248; // 2 new lines
    SynEdit.Height := SynEdit.LineHeight * 44 + SynEdit.LineHeight div 2; // another 2 lines
    SynEdit.EndUpdate;
    SynEdit.SimulatePaintText;
    TestHasMatches(N+' Found after edit',   2,  [l(265, 3, 8), l(275, 3,8)]);
    TestHasScanCnt(N+' Found after edit', 1, 12);


    N := 'Edit/Topline/LastLine find new points';
    SetTextAndMatch(250, False,   ['a265a', 'a275a', 'a248a', 'a292a']);
    TestHasMatches(N+' init/found',   2,  [l(265, 3, 8), l(275, 3,8)]);
    M.ResetScannedCount;
    SynEdit.BeginUpdate;
    SynEdit.TextBetweenPoints[point(10, i), point(10, i)] := 'X';
    SynEdit.TopLine := 248; // 2 new lines
    SynEdit.Height := SynEdit.LineHeight * 44 + SynEdit.LineHeight div 2; // another 2 lines
    SynEdit.EndUpdate;
    SynEdit.SimulatePaintText;
    TestHasMatches(N+' Found after edit',   4,  [l(265, 3, 8), l(275, 3,8), l(248, 3,8), l(292, 3,8)]);
    TestHasScanCnt(N+' Found after edit', 1, 12);





    PopBaseName;
  {%endregion}



end;

initialization

  RegisterTest(TTestMarkupHighAll);
end.

