unit TestSynTextArea;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TestBase, SynHighlighterPas,
  LazSynTextArea, SynEditTypes;

type

  { TTestSynTextArea }

  TTestSynTextArea = class(TTestBase)
  private
    FTheHighLighter: TSynPasSyn;
  protected
    FTokenBreaker: TLazSynPaintTokenBreaker;

    procedure ReCreateEdit; reintroduce;
    function CreateTheHighLighter: TSynPasSyn;
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

function TTestSynTextArea.CreateTheHighLighter: TSynPasSyn;
begin
  Result := TSynPasSyn.Create(nil);
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
  SetLines(['unit foo;',
            'interface//',
            'const',
            '  test =''abcDEF'';',
            '  testa=''a あアア F'';',
            '  testb=''aääDEF'';　// föö bar',
            #9'i=123;',
            '  a'#9'=0;',
            #9#9#9#9'end'
           ]);
end;

procedure TTestSynTextArea.TestPaintTokenBreaker;
var
  BaseName, Name: String;
  TkCnt: Integer;
  Token: TLazSynDisplayTokenInfoEx;

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
    BaseName := Format('%s (Line=%d, F/L=%d-%d): ', [AName, ALine, AFirst, ALast]);
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
    R := FTokenBreaker.GetNextHighlighterTokenFromView(Token, APhysLimit);
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
    R := FTokenBreaker.GetNextHighlighterTokenFromView(Token, APhysLimit);
    AssertFalse(Name + ' No further Token', R);
  end;

begin
  SetRealLinesText;
  SynEdit.TabWidth := 4;
  SynEdit.ViewedTextBuffer.DisplayView.InitHighlighterTokens(FTheHighLighter);

  {%region  full line}
    TestStart('Scan full line',   2,   1, 100,   2);

    TestNext(100,    1, 10,   1, 10,   1, 10,  'interface');
    TestNext(100,   10, 12,  10, 12,  10, 12,  '//');
    TestEnd(100);
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



end;

initialization
  RegisterTest(TTestSynTextArea);

end.

