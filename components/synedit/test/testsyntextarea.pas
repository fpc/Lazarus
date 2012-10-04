unit TestSynTextArea;

{$mode objfpc}{$H+}
{$INLINE OFF}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TestBase, LazSynTextArea,
  SynEditTypes, SynEditMarkupBracket, SynEdit, SynHighlighterPosition, Graphics;

type

  { TTestSynTextArea }

  TTestSynTextArea = class(TTestBase)
  private
    FTheHighLighter: TSynPositionHighlighter;
    FtkRed, FtkGreen, FtkBlue, FtkYellow: TtkTokenKind;
  protected
    FTokenBreaker: TLazSynPaintTokenBreaker;

    procedure ReCreateEdit; reintroduce;
    function CreateTheHighLighter: TSynPositionHighlighter;
    procedure SetUp; override;
    procedure TearDown; override;

    procedure SetRealLinesText;
  published
    procedure TestPaintTokenBreaker;
  end;

implementation


{ TTestSynTextArea }

procedure TTestSynTextArea.ReCreateEdit;
begin
  if Assigned(SynEdit) then
    SynEdit.Highlighter := nil;
  FreeAndNil(FTheHighLighter);
  inherited ReCreateEdit;
  FTheHighLighter := CreateTheHighLighter;
  SynEdit.Highlighter := FTheHighLighter;
end;

function TTestSynTextArea.CreateTheHighLighter: TSynPositionHighlighter;
begin
  Result   := TSynPositionHighlighter.Create(nil);
  FtkRed    := Result.CreateTokenID('red',    clRed,    clDefault, []);
  FtkGreen  := Result.CreateTokenID('green',  clGreen,  clDefault, []);
  FtkBlue   := Result.CreateTokenID('blue',   clBlue,   clDefault, []);
  FtkYellow := Result.CreateTokenID('yellow', clYellow, clDefault, []);
end;

procedure TTestSynTextArea.SetUp;
begin
  inherited SetUp;
  FTokenBreaker := TLazSynPaintTokenBreaker.Create;
end;

procedure TTestSynTextArea.TearDown;
begin
  FTokenBreaker.Free;
  inherited TearDown;
end;

procedure TTestSynTextArea.SetRealLinesText;
begin
  ReCreateEdit;
  SetLines(['unit foo;',                       // 1
            'interface//',                     //     Hl-token
            'const',
            '  test =''abcDEF'';',
            '  testa=''a あアア F'';',        // 5
            '  testb=''aääDEF'';　// föö bar',
            #9'i=123;',                       // 7 // Hl-token
            '  a'#9'=0;',
            #9#9#9#9'end',
            '',                               // 10
            'شس',
            'شس ي',
            'ABشس يCD',
            #9'i'#9'12'#9,
            ''
           ]);
end;

procedure TTestSynTextArea.TestPaintTokenBreaker;
var
  BaseName, Name: String;
  TkCnt: Integer;
  Token: TLazSynDisplayTokenInfoEx;
  UseViewTokenOnly: Boolean;
  SkipPreFirst: Boolean;

  procedure TestToken(LStart, LEnd, PStart, PEnd, DStart, DEnd: Integer; IsRtl: Boolean; AText: String);
  begin
    AssertEquals(Name + 'LStart', LStart, Token.LogicalStart);
    AssertEquals(Name + 'LEnd',   LEnd,   Token.LogicalEnd);
    AssertEquals(Name + 'PStart', PStart, Token.PhysicalStart);
    AssertEquals(Name + 'PEnd',   PEnd,   Token.PhysicalEnd);
    AssertEquals(Name + 'PaintStart', DStart, Token.PhysicalPaintStart);
    AssertEquals(Name + 'PaintEnd',   DEnd,   Token.PhysicalPaintEnd);
    AssertEquals(Name + 'IsRtl',  IsRtl,   Token.IsRtl);
    AssertEquals(Name + 'Text',   AText,  copy(Token.Tk.TokenStart, 1, Token.Tk.TokenLength));
  end;

  procedure TestTokenNxt(LStart, PStart: Integer; IsRtl: Boolean; Offs: Integer = -1);
  begin
    if LStart = -99 then exit;
    AssertEquals(Name + 'Nxt LStart', LStart, Token.NextLogicStart);
    AssertEquals(Name + 'Nxt PStart', PStart, Token.NextPhysicalPaintStart);
    if LStart < 0 then exit;
    AssertEquals(Name + 'Nxt IsRtl',  IsRtl,  Token.NextIsRtl);
    if Offs < 0 then exit;
    AssertEquals(Name + 'Nxt Offs',  Offs,  Token.NextLogicStartPhysOffs);
  end;

  procedure TestStart(AName: String; ALine, AFirst, ALast: Integer; ExpRLine: Integer = -1);
  // ALine, ExpRLine: 1-based
  var
    RLine: TLineIdx;
  begin
    BaseName := Format('%s::%s (Line=%d, F/L=%d-%d): ', [BaseTestName, AName, ALine, AFirst, ALast]);
    Name := BaseName;
    TkCnt := 0;
    FTokenBreaker.Prepare(SynEdit.ViewedTextBuffer.DisplayView,
                          SynEdit.ViewedTextBuffer,
                          SynEdit.TextArea.MarkupManager,
                          AFirst, ALast);
    FTokenBreaker.SetHighlighterTokensLine(ALine-1, RLine);
    if ExpRLine >= 0 then
      AssertEquals(Name+'Got RealLine', ExpRLine-1, RLine);
  end;

  procedure TestPreFirst(LStart, PStart: Integer; IsRtl: Boolean; Offs: Integer = -1);
  var
    R: Boolean;
  begin
    if SkipPreFirst then exit;
    Name := Format('%sL (Pre-First): ', [BaseName]);
    R := FTokenBreaker.GetNextHighlighterTokenFromView(Token, -1, 1);
    AssertTrue(Name + 'Got Token', R);
    TestTokenNxt(LStart, PStart, IsRtl, Offs);
  end;

  procedure TestNext(ALimits: Array of Integer);
  var
    R: Boolean;
    PhysLimit, LogLimit: Integer;
  begin
    inc(TkCnt);
    PhysLimit := -1;
    LogLimit := -1;
    if Length(ALimits) > 0 then PhysLimit := ALimits[0];
    if Length(ALimits) > 1 then LogLimit  := ALimits[1];
    Name := Format('%sL=%d (%d): ', [BaseName, PhysLimit, TkCnt]);
    if UseViewTokenOnly
    then R := FTokenBreaker.GetNextHighlighterTokenFromView(Token, PhysLimit, LogLimit)
    else R := FTokenBreaker.GetNextHighlighterTokenEx(Token);
    AssertTrue(Name + 'Got Token', R);
  end;

  procedure TestNext(ALimits: Array of Integer; LStart, LEnd, PStart, PEnd, DStart, DEnd: Integer;
    IsRtl: Boolean; AText: String; NxtLStart: Integer = -99; NxtPStart: Integer = -1;
    NxtIsRtl: Boolean = False; NxtOffs: Integer = -1);
  begin
    TestNext(ALimits);
    TestToken(LStart, LEnd, PStart, PEnd, DStart, DEnd, IsRtl, AText);
    TestTokenNxt(NxtLStart, NxtPStart, NxtIsRtl, NxtOffs);
  end;

  procedure TestEnd;
  var
    R: Boolean;
  begin
    inc(TkCnt);
    Name := Format('%sL=(%d): ', [BaseName, TkCnt]);
    if UseViewTokenOnly
    then R := FTokenBreaker.GetNextHighlighterTokenFromView(Token)
    else R := FTokenBreaker.GetNextHighlighterTokenEx(Token);
    AssertFalse(Name + ' No further Token', R);
  end;

  procedure DoTests;
  begin
    UseViewTokenOnly := True;
    SetRealLinesText;
    SynEdit.TabWidth := 4;
    SynEdit.ViewedTextBuffer.DisplayView.InitHighlighterTokens(SynEdit.Highlighter);

    FTheHighLighter.AddToken(2-1,  9, FtkBlue);   // interface
    FTheHighLighter.AddToken(7-1,  1, FtkYellow); // #9
    FTheHighLighter.AddToken(7-1,  2, FtkGreen);  // i
    FTheHighLighter.AddToken(7-1,  3, FtkRed);    // =
    FTheHighLighter.AddToken(7-1,  6, FtkGreen);  // 123

    {%region  LTR only }
      PushBaseName('LTR-Only');

      {%region  full line}
        TestStart('Scan full line / hl-token',   2,   1, 100,   2);
        TestNext([100],    1, 10,   1, 10,   1, 10,  False,  'interface',  10, 10, False, 0);
        TestNext([100],   10, 12,  10, 12,  10, 12,  False,  '//'); //,         12, 12, False, 0);
        TestEnd;

        TestStart('Scan full line / hl-token',   2,   1, 100,   2);
        TestPreFirst(1, 1, False, 0);
        TestNext([],    1, 10,   1, 10,   1, 10,  False,  'interface',  10, 10, False, 0);
        TestNext([],   10, 12,  10, 12,  10, 12,  False,  '//');
        TestEnd;
      {%endregion}

      {%region  cut off end of line}
        TestStart('Cut off end',   2,   1, 5,   2);

        TestNext([100],   1, 5,  1, 5,  1, 5, False,  'inte');
        TestEnd;
      {%endregion}

      {%region  cut off start of line}
        TestStart('Cut off start',   2,   3, 100,   2);
        TestNext([100],    3, 10,   3, 10,   3, 10,  False,  'terface');
        TestNext([100],   10, 12,  10, 12,  10, 12,  False,  '//');
        TestEnd;

        TestStart('Cut off start 1 tok',   2,  10, 100,   2);
        TestNext([100],   10, 12,  10, 12,  10, 12,  False,  '//');
        TestEnd;

        TestStart('Cut off start 1.5 tok',   2,  11, 100,   2);
        TestNext([100],   11, 12,  11, 12,  11, 12,  False,  '/');
        TestEnd;
      {%endregion}

      {%region  cut off both}
        TestStart('Cut off both',   2,   3, 10,   2);
        TestNext([100],    3, 10,   3, 10,   3, 10,  False,  'terface');
        //TestNext([100],   10, 12,  10, 12,  10, 12,  False,  '//');
        TestEnd;

        TestStart('Cut off both - 2 token',   2,   3, 11,   2);
        TestNext([100],    3, 10,   3, 10,   3, 10,  False,  'terface');
        TestNext([100],   10, 11,  10, 11,  10, 11,  False,  '/');
        TestEnd;

        TestStart('Cut off both - 1 token, skip 1st',   2,   10, 11,   2);
        TestNext([100],   10, 11,  10, 11,  10, 11,  False,  '/');
        TestEnd;
      {%endregion}

      {%region  1 token 2 parts}
        TestStart('1 token 2 parts',   2,   1, 100,   2);

        TestNext([  3],    1,  3,   1,  3,   1,  3,  False,  'in');
        TestNext([100],    3, 10,   3, 10,   3, 10,  False,  'terface');
        TestNext([100],   10, 12,  10, 12,  10, 12,  False,  '//');
        TestEnd;
      {%endregion}

      // part chars/tabs

      {%region  cut off PART of char }
        // start at 1
        TestStart('cut off PART of char (end)',     7,   1, 100,   7);
        TestPreFirst(1, 1, False, 0);
        TestNext([  3],   1, 2,  1, 5,  1, 3,  False,  #9,    1, 3, False, 2);
        TestNext([100],   1, 2,  1, 5,  3, 5,  False,  #9,    2, 5, False, 0);
        TestNext([100],   2, 3,  5, 6,  5, 6,  False,  'i');

        TestStart('cut off PART of char (end) next-limit',   7,   1, 100,   7);
        TestPreFirst(1, 1, False, 0);
        TestNext([  3],   1, 2,  1, 5,  1, 3,  False,  #9,    1, 3, False, 2);
        TestNext([  5],   1, 2,  1, 5,  3, 5,  False,  #9,    2, 5, False, 0);
        TestNext([100],   2, 3,  5, 6,  5, 6,  False,  'i');

        TestStart('cut off PART of char (end) next-limit-log',   7,   1, 100,   7);
        TestPreFirst(1, 1, False, 0);
        TestNext([  3],   1, 2,  1, 5,  1, 3,  False,  #9,    1, 3, False, 2);
        TestNext([99,2],  1, 2,  1, 5,  3, 5,  False,  #9,    2, 5, False, 0);
        TestNext([100],   2, 3,  5, 6,  5, 6,  False,  'i');

        // start at 2
        TestStart('cut off PART of char (begin)',   7,   2, 100,   7);
        TestPreFirst(1, 2, False, 1);
        TestNext([  5],   1, 2,  1, 5,  2, 5,  False,  #9);
        TestNext([100],   2, 3,  5, 6,  5, 6,  False,  'i');

        TestStart('cut off PART of char (both) continue',   7,   2, 100,   7);
        TestPreFirst(1, 2, False, 1);
        TestNext([  3],   1, 2,  1, 5,  2, 3,  False,  #9);
        TestNext([100],   1, 2,  1, 5,  3, 5,  False,  #9);
        TestNext([100],   2, 3,  5, 6,  5, 6,  False,  'i');

        TestStart('cut off PART of char (both) global-limit',   7,   2, 3,   7);
        TestPreFirst(1, 2, False, 1);
        TestNext([100],   1, 2,  1, 5,  2, 3,  False,  #9);
        TestEnd;

        TestStart('cut off PART of char (both) next-limit',   7,   2, 100,   7);
        TestPreFirst(1, 2, False, 1);
        TestNext([3],   1, 2,  1, 5,  2, 3,  False,  #9);
        //TestEnd(3);

        // start at 3
        TestStart('cut off PART of char (begin)',   7,   3, 100,   7);
        TestPreFirst(1, 3, False, 2);
        TestNext([  5],   1, 2,  1, 5,  3, 5,  False,  #9);
        TestNext([100],   2, 3,  5, 6,  5, 6,  False,  'i');

        // start at 4
        TestStart('cut off PART of char (begin)',   7,   4, 100,   7);
        TestPreFirst(1, 4, False, 3);
        TestNext([  5],   1, 2,  1, 5,  4, 5,  False,  #9);
        TestNext([100],   2, 3,  5, 6,  5, 6,  False,  'i');

        // start at 5
        TestStart('cut off PART of char (begin)',   7,   5, 100,   7);
        TestPreFirst(2, 5, False, 0);
        TestNext([100],   2, 3,  5, 6,  5, 6,  False,  'i');
      {%endregion}

      {%region  cut tab in many}
        TestStart('cut tab in many',   9,   2, 100,   9);

        TestNext([  3],   1, 2,   1,  5,   2,  3,  False,  #9);
        TestNext([  6],   1, 3,   1,  9,   3,  6,  False,  #9#9);
        TestNext([ 11],   2, 4,   5, 13,   6, 11,  False,  #9#9);
        TestNext([ 13],   3, 4,   9, 13,  11, 13,  False,  #9);
        TestNext([ 15],   4, 5,  13, 17,  13, 15,  False,  #9);
      {%endregion}

      {%region  break line // phys log}
        TestStart('Scan full line',  14,   1, 100,   14);
        TestPreFirst(1, 1, False, 0);
        TestNext([-1, -1],    1, 7,   1, 13,   1, 13,  False,  #9'i'#9'12'#9);

        TestStart('Scan full line',  14,   1, 100,   14);
        TestPreFirst(1, 1, False, 0);
        TestNext([-1, 2],    1, 2,   1,  5,   1,  5,  False,  #9,               2, 5, False, 0);
        TestNext([-1, 3],    2, 3,   5,  6,   5,  6,  False,  'i');

        TestStart('Scan full line',  14,   1, 100,   14);
        TestPreFirst(1, 1, False, 0);
        TestNext([99, 2],    1, 2,   1,  5,   1,  5,  False,  #9,               2, 5, False, 0);
        TestNext([-1, 3],    2, 3,   5,  6,   5,  6,  False,  'i');

        TestStart('Scan full line',  14,   1, 100,   14);
        TestPreFirst(1, 1, False, 0);
        TestNext([ 5, 2],    1, 2,   1,  5,   1,  5,  False,  #9,               2, 5, False, 0);
        TestNext([-1, 3],    2, 3,   5,  6,   5,  6,  False,  'i');

        TestStart('Scan full line',  14,   1, 100,   14);
        TestPreFirst(1, 1, False, 0);
        TestNext([ 4, 2],    1, 2,   1,  5,   1,  4,  False,  #9,               1, 4, False, 3);
        TestNext([-1, 2],    1, 2,   1,  5,   4,  5,  False,  #9,               2, 5, False, 0);
        TestNext([-1, 3],    2, 3,   5,  6,   5,  6,  False,  'i');


      {%endregion}

      PopBaseName;
    {%endregion}

    SynEdit.ViewedTextBuffer.DisplayView.FinishHighlighterTokens;
    SynEdit.Highlighter := nil;
    SynEdit.ViewedTextBuffer.DisplayView.InitHighlighterTokens(SynEdit.Highlighter);
    {%region  RTL only }
      PushBaseName('RTL-Only');
      {%region  full line}
        TestStart('Scan full line',  11,   1, 100,   11);
        TestPreFirst(1,3, True, 0);
        TestNext([100],    1,  5,   1,  3,   1,  3,  True,  'شس');
        TestEnd;

        TestStart('Scan full line',  11,   1, 100,   11);
        TestPreFirst(1, 3, True, 0);
        TestNext([],    1,  5,   1,  3,   1,  3,  True,  'شس');
        TestEnd;

        TestStart('Scan full line (2 words)',  12,   1, 100,   12);
        TestPreFirst(1, 5, True, 0);
        TestNext([100],    1, 8,   1,  5,   1,  5,  True,  'شس ي');
        TestEnd;
      {%endregion}

      {%region  part line}
        // 1 char parts
        TestStart('part line - begin',  12,   1, 2,  12);
        TestPreFirst(6, 2, True, 0);
        TestNext([100],    6, 8,   1,  2,   1,  2,  True,  'ي');
        TestEnd;

        TestStart('part line - mid',  12,   2, 3,  12);
        TestPreFirst(5, 3, True, 0);
        TestNext([100],    5, 6,   2,  3,   2,  3,  True,  ' ');
        TestEnd;

        TestStart('part line - mid',  12,   3, 4,  12);
        TestPreFirst(3, 4, True, 0);
        TestNext([100],    3, 5,   3,  4,   3,  4,  True,  'س');
        TestEnd;

        TestStart('part line - end',  12,   4, 5,  12);
        TestPreFirst(1, 5, True, 0);
        TestNext([100],    1, 3,   4,  5,   4,  5,  True,  'ش');
        TestEnd;

        // 2 char parts
        TestStart('part line - begin(2)',  12,   1, 3,  12);
        TestPreFirst(5, 3, True, 0);
        TestNext([100],    5, 8,   1,  3,   1,  3,  True,  ' ي');
        TestEnd;

        TestStart('part line - mid(2)',  12,   2, 4,  12);
        TestPreFirst(3, 4, True, 0);
        TestNext([100],    3, 6,   2,  4,   2,  4,  True,  'س ');
        TestEnd;

        TestStart('part line - end(2)',  12,   3, 5,  12);
        TestPreFirst(1, 5, True, 0);
        TestNext([100],    1, 5,   3,  5,   3,  5,  True,  'شس');
        TestEnd;

        // 1 char parts, several chunks
        TestStart('part line - begin',  12,   1, 100,  12);
        TestPreFirst(1, 5, True, 0);
        TestNext([4],    1, 3,   4,  5,   4,  5,  True,  'ش');
        TestNext([3],    3, 5,   3,  4,   3,  4,  True,  'س');
        TestNext([2],    5, 6,   2,  3,   2,  3,  True,  ' ');
        TestNext([1],    6, 8,   1,  2,   1,  2,  True,  'ي');
        TestEnd;

        // 1 char parts, several chunks
        TestStart('part line - begin',  12,   1, 100,  12);
        TestPreFirst(1, 5, True, 0);
        TestNext([4],    1, 3,   4,  5,   4,  5,  True,  'ش');
        TestNext([3],    3, 5,   3,  4,   3,  4,  True,  'س');
        TestNext([0],    5, 8,   1,  3,   1,  3,  True,  ' ي');
        TestEnd;

        //TestStart('part line - begin',  12,   1, 100,  12);
        //TestNext(2,    1, 6,   2,  5,   2,  5,  True,  'شس ');
        //TestNext(5,    1, 3,   4,  5,   4,  5,  True,  'ي');
        //TestEnd;
        //
        //TestStart('part line - begin',  12,   1, 100,  12);
        //TestNext(  2,    1, 6,   2,  5,   2,  5,  True,  'شس ');
        //TestNext([100],    1, 3,   4,  5,   4,  5,  True,  'شس ي');
        //TestEnd;

      {%endregion}

      PopBaseName;
    {%endregion}

    {%region  MIXED Rtl/Ltr }
      PushBaseName('MIXED Rtl/Ltr');
      {%region  full line}
        TestStart('Scan full line',  13,   1, 100,   13);
        TestPreFirst(1, 1, False, 0);
        TestNext([],    1,  3,   1,  3,   1,  3,  False,  'AB',     3,  7, True,  0);
        TestNext([],    3, 10,   3,  7,   3,  7,  True,  'شس ي',   10,  7, False, 0);
        TestNext([],   10, 12,   7,  9,   7,  9,  False,  'CD'); //,     3,  7, True,  0);
        TestEnd;

      {%endregion}

      {%region  parts}
        TestStart('Scan part line, cut at start',  13,   2, 100,   13);
        TestPreFirst(2, 2, False, 0);
        TestNext([],    2,  3,   2,  3,   2,  3,  False,  'B',     3,  7, True,  0);
        TestNext([],    3, 10,   3,  7,   3,  7,  True,  'شس ي',  10,  7, False, 0);
        TestNext([],   10, 12,   7,  9,   7,  9,  False,  'CD');
        TestEnd;

        TestStart('Scan part line, cut at start',  13,   3, 100,   13);
        TestPreFirst(3, 7, True, 0);
        TestNext([],    3, 10,   3,  7,   3,  7,  True,  'شس ي',  10,  7, False, 0);
        TestNext([],   10, 12,   7,  9,   7,  9,  False,  'CD');
        TestEnd;

        TestStart('Scan part line, cut at start',  13,   4, 100,   13);
        TestPreFirst(3, 7, True, 0);
        TestNext([],    3,  8,   4,  7,   4,  7,  True,  'شس ',  10,  7, False, 0);
        TestNext([],   10, 12,   7,  9,   7,  9,  False,  'CD');
        TestEnd;

        TestStart('Scan part line, cut at start',  13,   6, 100,   13);
        TestPreFirst(3, 7, True, 0);
        TestNext([],    3,  5,   6,  7,   6,  7,  True,  'ش',  10,  7, False, 0);
        TestNext([],   10, 12,   7,  9,   7,  9,  False,  'CD');
        TestEnd;

        TestStart('Scan part line, cut at start',  13,   7, 100,   13);
        TestPreFirst(10, 7, False, 0);
        TestNext([],   10, 12,   7,  9,   7,  9,  False,  'CD');
        TestEnd;

        TestStart('Scan part line, cut at start',  13,   8, 100,   13);
        TestPreFirst(11, 8, False, 0);
        TestNext([],   11, 12,   8,  9,   8,  9,  False,  'D');
        TestEnd;

        TestStart('Scan part line, cut at start',  13,   9, 100,   13);
        TestEnd;


        TestStart('Scan part line, cut at end',  13,   1, 8,   13);
        TestNext([],    1,  3,   1,  3,   1,  3,  False,  'AB',    3,  7, True,  0);
        TestNext([],    3, 10,   3,  7,   3,  7,  True,  'شس ي',  10,  7, False, 0);
        TestNext([],   10, 11,   7,  8,   7,  8,  False,  'C');
        TestEnd;

        TestStart('Scan part line, cut at end',  13,   1, 7,   13);
        TestNext([],    1,  3,   1,  3,   1,  3,  False,  'AB',    3,  7, True,  0);
        TestNext([],    3, 10,   3,  7,   3,  7,  True,  'شس ي');
        TestEnd;

        TestStart('Scan part line, cut at end',  13,   1, 6,   13);
        TestNext([],    1,  3,   1,  3,   1,  3,  False,  'AB',    5,  6, True,  0);
        TestNext([],    5, 10,   3,  6,   3,  6,  True,  'س ي');
        TestEnd;

        TestStart('Scan part line, cut at end',  13,   1, 4,   13);
        TestNext([],    1,  3,   1,  3,   1,  3,  False,  'AB',    8,  4, True,  0);
        TestNext([],    8, 10,   3,  4,   3,  4,  True,  'ي');
        TestEnd;

        TestStart('Scan part line, cut at end',  13,   1, 3,   13);
        TestNext([],    1,  3,   1,  3,   1,  3,  False,  'AB');
        TestEnd;

        TestStart('Scan part line, cut at end',  13,   1, 2,   13);
        TestNext([],    1,  2,   1,  2,   1,  2,  False,  'A');
        TestEnd;


      {%endregion}

      PopBaseName;
    {%endregion}


    SynEdit.ViewedTextBuffer.DisplayView.FinishHighlighterTokens;

  end;
begin
  PushBaseName('Incl PreFirst');
  SkipPreFirst := False;
  DoTests;
  PopPushBaseName('Excl PreFirst');
  SkipPreFirst := True;
  DoTests;
  PopBaseName;
end;

initialization
  RegisterTest(TTestSynTextArea);

end.

