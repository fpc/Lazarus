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
            'interface//',
            'const',
            '  test =''abcDEF'';',
            '  testa=''a あアア F'';',        // 5
            '  testb=''aääDEF'';　// föö bar',
            #9'i=123;',
            '  a'#9'=0;',
            #9#9#9#9'end',
            '',                               // 10
            'شس',
            'شس ي',
            'ABشس يCD',
            ''
           ]);
end;

procedure TTestSynTextArea.TestPaintTokenBreaker;
var
  BaseName, Name: String;
  TkCnt: Integer;
  Token: TLazSynDisplayTokenInfoEx;
  UseViewTokenOnly: Boolean;

  procedure TestToken(LStart, LEnd, PStart, PEnd, DStart, DEnd: Integer; AText: String);
  begin
    AssertEquals(Name + 'LStart', LStart, Token.LogicalStart);
    AssertEquals(Name + 'LEnd',   LEnd,   Token.LogicalEnd);
    AssertEquals(Name + 'PStart', PStart, Token.PhysicalStart);
    AssertEquals(Name + 'PEnd',   PEnd,   Token.PhysicalEnd);
    AssertEquals(Name + 'PaintStart', DStart, Token.PhysicalPaintStart);
    AssertEquals(Name + 'PaintEnd',   DEnd,   Token.PhysicalPaintEnd);
    AssertEquals(Name + 'Text',   AText,  copy(Token.Tk.TokenStart, 1, Token.Tk.TokenLength));
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

  procedure TestNext(APhysLimit: Integer);
  var
    R: Boolean;
  begin
    inc(TkCnt);
    Name := Format('%sL=%d (%d): ', [BaseName, APhysLimit, TkCnt]);
    if UseViewTokenOnly
    then R := FTokenBreaker.GetNextHighlighterTokenFromView(Token, APhysLimit)
    else R := FTokenBreaker.GetNextHighlighterTokenEx(Token);
    AssertTrue(Name + 'Got Token', R);
  end;

  procedure TestNext(APhysLimit: Integer; LStart, LEnd, PStart, PEnd, DStart, DEnd: Integer; AText: String);
  begin
    TestNext(APhysLimit);
    TestToken(LStart, LEnd, PStart, PEnd, DStart, DEnd, AText);
  end;

  procedure TestEnd(APhysLimit: Integer);
  var
    R: Boolean;
  begin
    inc(TkCnt);
    Name := Format('%sL=%d (%d): ', [BaseName, APhysLimit, TkCnt]);
    if UseViewTokenOnly
    then R := FTokenBreaker.GetNextHighlighterTokenFromView(Token, APhysLimit)
    else R := FTokenBreaker.GetNextHighlighterTokenEx(Token);
    AssertFalse(Name + ' No further Token', R);
  end;

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
      TestStart('Scan full line',   2,   1, 100,   2);
      TestNext(100,    1, 10,   1, 10,   1, 10,  'interface');
      TestNext(100,   10, 12,  10, 12,  10, 12,  '//');
      TestEnd(100);

      TestStart('Scan full line',   2,   1, 100,   2);
      TestNext(-1,    1, 10,   1, 10,   1, 10,  'interface');
      TestNext(-1,   10, 12,  10, 12,  10, 12,  '//');
      TestEnd(-1);
    {%endregion}

    {%region  cut off end of line}
      TestStart('Cut off end',   2,   1, 5,   2);

      TestNext(100,   1, 5,  1, 5,  1, 5, 'inte');
      TestEnd(100);
    {%endregion}

    {%region  cut off start of line}
      TestStart('Cut off start',   2,   3, 100,   2);

      TestNext(100,    3, 10,   3, 10,   3, 10,  'terface');
      TestNext(100,   10, 12,  10, 12,  10, 12,  '//');
      TestEnd(100);
    {%endregion}

    {%region  cut off start of line}
      TestStart('Cut off start 1 tok',   2,  10, 100,   2);

      TestNext(100,   10, 12,  10, 12,  10, 12,  '//');
      TestEnd(100);
    {%endregion}

    {%region  cut off start of line}
      TestStart('Cut off start 1.5 tok',   2,  11, 100,   2);

      TestNext(100,   11, 12,  11, 12,  11, 12,  '/');
      TestEnd(100);
    {%endregion}

    {%region  cut off both}
      TestStart('Cut off both',   2,   3, 10,   2);

      TestNext(100,    3, 10,   3, 10,   3, 10,  'terface');
      //TestNext(100,   10, 12,  10, 12,  10, 12,  '//');
      TestEnd(100);
    {%endregion}

    {%region  cut off both - 2 token}
      TestStart('Cut off both - 2 token',   2,   3, 11,   2);

      TestNext(100,    3, 10,   3, 10,   3, 10,  'terface');
      TestNext(100,   10, 11,  10, 11,  10, 11,  '/');
      TestEnd(100);
    {%endregion}

    {%region  cut off both - 1 token, skip first}
      TestStart('Cut off both - 1 token, skip 1st',   2,   10, 11,   2);

      TestNext(100,   10, 11,  10, 11,  10, 11,  '/');
      TestEnd(100);
    {%endregion}

    {%region  1 token 2 parts}
      TestStart('1 token 2 parts',   2,   1, 100,   2);

      TestNext(  3,    1,  3,   1,  3,   1,  3,  'in');
      TestNext(100,    3, 10,   3, 10,   3, 10,  'terface');
      TestNext(100,   10, 12,  10, 12,  10, 12,  '//');
      TestEnd(100);
    {%endregion}

    // part chars/tabs

    {%region  cut off PART of char}
      TestStart('cut off PART of char (begin)',   7,   2, 100,   7);

      TestNext(  5,   1, 2,  1, 5,  2, 5,  #9);
      TestNext(100,   2, 3,  5, 6,  5, 6,  'i');
    {%endregion}

    {%region  cut off PART of char}
      TestStart('cut off PART of char (end)',   7,   1, 100,   7);

      TestNext(  3,   1, 2,  1, 5,  1, 3,  #9);
      TestNext(100,   1, 2,  1, 5,  3, 5,  #9);
      TestNext(100,   2, 3,  5, 6,  5, 6,  'i');
    {%endregion}

    {%region  cut off PART of char}
      TestStart('cut off PART of char (end) next-limit',   7,   1, 100,   7);

      TestNext(  3,   1, 2,  1, 5,  1, 3,  #9);
      TestNext(  5,   1, 2,  1, 5,  3, 5,  #9);
      TestNext(100,   2, 3,  5, 6,  5, 6,  'i');
    {%endregion}

    {%region  cut off PART of char}
      TestStart('cut off PART of char (both) continue',   7,   2, 100,   7);

      TestNext(  3,   1, 2,  1, 5,  2, 3,  #9);
      TestNext(100,   1, 2,  1, 5,  3, 5,  #9);
      TestNext(100,   2, 3,  5, 6,  5, 6,  'i');
    {%endregion}

    {%region  cut off PART of char}
      TestStart('cut off PART of char (both) global-limit',   7,   2, 3,   7);

      TestNext(100,   1, 2,  1, 5,  2, 3,  #9);
      TestEnd(100);
    {%endregion}

    {%region  cut off PART of char}
      TestStart('cut off PART of char (both) next-limit',   7,   2, 100,   7);

      TestNext(3,   1, 2,  1, 5,  2, 3,  #9);
      TestEnd(3);
    {%endregion}

    {%region  cut tab in many}
      TestStart('cut tab in many',   9,   2, 100,   9);

      TestNext(  3,   1, 2,   1,  5,   2,  3,  #9);
      TestNext(  6,   1, 3,   1,  9,   3,  6,  #9#9);
      TestNext( 11,   2, 4,   5, 13,   6, 11,  #9#9);
      TestNext( 13,   3, 4,   9, 13,  11, 13,  #9);
      TestNext( 15,   4, 5,  13, 17,  13, 15,  #9);
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
      TestNext(100,    1,  5,   1,  3,   1,  3,  'شس');
      TestEnd(100);

      TestStart('Scan full line',  11,   1, 100,   11);
      TestNext(-1,    1,  5,   1,  3,   1,  3,  'شس');
      TestEnd(-1);

      TestStart('Scan full line (2 words)',  12,   1, 100,   12);
      TestNext(100,    1, 8,   1,  5,   1,  5,  'شس ي');
      TestEnd(100);
    {%endregion}

    {%region  part line}
      // 1 char parts
      TestStart('part line - begin',  12,   1, 2,  12);
      TestNext(100,    6, 8,   1,  2,   1,  2,  'ي');
      TestEnd(100);

      TestStart('part line - mid',  12,   2, 3,  12);
      TestNext(100,    5, 6,   2,  3,   2,  3,  ' ');
      TestEnd(100);

      TestStart('part line - mid',  12,   3, 4,  12);
      TestNext(100,    3, 5,   3,  4,   3,  4,  'س');
      TestEnd(100);

      TestStart('part line - end',  12,   4, 5,  12);
      TestNext(100,    1, 3,   4,  5,   4,  5,  'ش');
      TestEnd(100);

      // 2 char parts
      TestStart('part line - begin(2)',  12,   1, 3,  12);
      TestNext(100,    5, 8,   1,  3,   1,  3,  ' ي');
      TestEnd(100);

      TestStart('part line - mid(2)',  12,   2, 4,  12);
      TestNext(100,    3, 6,   2,  4,   2,  4,  'س ');
      TestEnd(100);

      TestStart('part line - end(2)',  12,   3, 5,  12);
      TestNext(100,    1, 5,   3,  5,   3,  5,  'شس');
      TestEnd(100);

      // 1 char parts, several chunks
      TestStart('part line - begin',  12,   1, 100,  12);
      TestNext(4,    1, 3,   4,  5,   4,  5,  'ش');
      TestNext(3,    3, 5,   3,  4,   3,  4,  'س');
      TestNext(2,    5, 6,   2,  3,   2,  3,  ' ');
      TestNext(1,    6, 8,   1,  2,   1,  2,  'ي');
      TestEnd(100);

      // 1 char parts, several chunks
      TestStart('part line - begin',  12,   1, 100,  12);
      TestNext(4,    1, 3,   4,  5,   4,  5,  'ش');
      TestNext(3,    3, 5,   3,  4,   3,  4,  'س');
      TestNext(0,    5, 8,   1,  3,   1,  3,  ' ي');
      TestEnd(100);

      //TestStart('part line - begin',  12,   1, 100,  12);
      //TestNext(2,    1, 6,   2,  5,   2,  5,  'شس ');
      //TestNext(5,    1, 3,   4,  5,   4,  5,  'ي');
      //TestEnd(100);
      //
      //TestStart('part line - begin',  12,   1, 100,  12);
      //TestNext(  2,    1, 6,   2,  5,   2,  5,  'شس ');
      //TestNext(100,    1, 3,   4,  5,   4,  5,  'شس ي');
      //TestEnd(100);

    {%endregion}

    PopBaseName;
  {%endregion}

  {%region  MIXED Rtl/Ltr }
    PushBaseName('MIXED Rtl/Ltr');
    {%region  full line}
      TestStart('Scan full line',  13,   1, 100,   13);
      TestNext(-1,    1,  3,   1,  3,   1,  3,  'AB');
      TestNext(-1,    3, 10,   3,  7,   3,  7,  'شس ي');
      TestNext(-1,   10, 12,   7,  9,   7,  9,  'CD');
      TestEnd(-1);

    {%endregion}

    {%region  parts}
      TestStart('Scan part line, cut at start',  13,   2, 100,   13);
      TestNext(-1,    2,  3,   2,  3,   2,  3,  'B');
      TestNext(-1,    3, 10,   3,  7,   3,  7,  'شس ي');
      TestNext(-1,   10, 12,   7,  9,   7,  9,  'CD');
      TestEnd(-1);

      TestStart('Scan part line, cut at start',  13,   3, 100,   13);
      TestNext(-1,    3, 10,   3,  7,   3,  7,  'شس ي');
      TestNext(-1,   10, 12,   7,  9,   7,  9,  'CD');
      TestEnd(-1);

      TestStart('Scan part line, cut at start',  13,   4, 100,   13);
      TestNext(-1,    3,  8,   4,  7,   4,  7,  'شس ');
      TestNext(-1,   10, 12,   7,  9,   7,  9,  'CD');
      TestEnd(-1);

      TestStart('Scan part line, cut at start',  13,   6, 100,   13);
      TestNext(-1,    3,  5,   6,  7,   6,  7,  'ش');
      TestNext(-1,   10, 12,   7,  9,   7,  9,  'CD');
      TestEnd(-1);

      TestStart('Scan part line, cut at start',  13,   7, 100,   13);
      TestNext(-1,   10, 12,   7,  9,   7,  9,  'CD');
      TestEnd(-1);

      TestStart('Scan part line, cut at start',  13,   8, 100,   13);
      TestNext(-1,   11, 12,   8,  9,   8,  9,  'D');
      TestEnd(-1);

      TestStart('Scan part line, cut at start',  13,   9, 100,   13);
      TestEnd(-1);


      TestStart('Scan part line, cut at end',  13,   1, 8,   13);
      TestNext(-1,    1,  3,   1,  3,   1,  3,  'AB');
      TestNext(-1,    3, 10,   3,  7,   3,  7,  'شس ي');
      TestNext(-1,   10, 11,   7,  8,   7,  8,  'C');
      TestEnd(-1);

      TestStart('Scan part line, cut at end',  13,   1, 7,   13);
      TestNext(-1,    1,  3,   1,  3,   1,  3,  'AB');
      TestNext(-1,    3, 10,   3,  7,   3,  7,  'شس ي');
      TestEnd(-1);

      TestStart('Scan part line, cut at end',  13,   1, 6,   13);
      TestNext(-1,    1,  3,   1,  3,   1,  3,  'AB');
      TestNext(-1,    5, 10,   3,  6,   3,  6,  'س ي');
      TestEnd(-1);

      TestStart('Scan part line, cut at end',  13,   1, 4,   13);
      TestNext(-1,    1,  3,   1,  3,   1,  3,  'AB');
      TestNext(-1,    8, 10,   3,  4,   3,  4,  'ي');
      TestEnd(-1);

      TestStart('Scan part line, cut at end',  13,   1, 3,   13);
      TestNext(-1,    1,  3,   1,  3,   1,  3,  'AB');
      TestEnd(-1);

      TestStart('Scan part line, cut at end',  13,   1, 2,   13);
      TestNext(-1,    1,  2,   1,  2,   1,  2,  'A');
      TestEnd(-1);


    {%endregion}

    PopBaseName;
  {%endregion}


  SynEdit.ViewedTextBuffer.DisplayView.FinishHighlighterTokens;

end;

initialization
  RegisterTest(TTestSynTextArea);

end.

