unit TestSynTextArea;

{$mode objfpc}{$H+}
{$INLINE OFF}

interface

uses
  Classes, SysUtils, testregistry, TestBase, LazSynTextArea, SynEditTypes,
  SynEditMarkupBracket, SynEdit, SynHighlighterPosition, SynEditMiscClasses,
  Graphics;

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
  SynEdit.Color := clWhite;
  SynEdit.Font.Color := clBlack;
  SynEdit.SelectedColor.Foreground := clPurple;
  FTokenBreaker.ForegroundColor := clBlack;
  FTokenBreaker.BackgroundColor := clWhite;
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
            'ي'#9'شس',                        // 15
            #9'('#9')',
            '//شس يشس ي(شس ي',
            'a     ''شس''b',
            'a     ''شسقف',
            'a     ''شسقف',                  // 20
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
type
  TBoundExpectation = record
    ExpPhys, ExpLog, ExpOffs: Integer;
    WantOffs: Boolean;
  end;

  function b(AExpPhys, AExpLog: Integer): TBoundExpectation;
  begin
    Result.ExpPhys := AExpPhys;
    Result.ExpLog  := AExpLog;
    Result.WantOffs := False;
  end;

  function b(AExpPhys, AExpLog, AExpOffs: Integer): TBoundExpectation;
  begin
    Result.ExpPhys := AExpPhys;
    Result.ExpLog  := AExpLog;
    Result.ExpOffs := AExpOffs;
    Result.WantOffs := True;
  end;

  procedure TestBound(AName: String; ABound: TLazSynDisplayTokenBound; AExpect: TBoundExpectation);
  begin
    AssertEquals(AName + ' Bound.Phys',  AExpect.ExpPhys, ABound.Physical);
    AssertEquals(AName + ' Bound.Log',   AExpect.ExpLog,  ABound.Logical);
    if AExpect.WantOffs then
      AssertEquals(AName + ' Bound.Offs',  AExpect.ExpOffs, ABound.Offset);
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

    if not UseViewTokenOnly then
      SynEdit.TextArea.MarkupManager.PrepareMarkupForRow(ALine);
  end;

  procedure TestPreFirst(ExpNext: TBoundExpectation; ExpNxtIsRtl: Boolean);
  var
    R: Boolean;
  begin
    if SkipPreFirst then exit;
    Name := Format('%sL (Pre-First): ', [BaseName]);
    R := FTokenBreaker.GetNextHighlighterTokenFromView(Token, -1, 1);

    AssertTrue(Name + 'Got Token', R);
    TestBound(Name + ' Next(BOL)', Token.NextPos, ExpNext);
    AssertEquals(Name + 'Nxt IsRtl',  ExpNxtIsRtl,  Token.NextRtlInfo.IsRtl);
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

  procedure TestNext(ALimits: Array of Integer;
    ExpStart, ExpEnd: TBoundExpectation;
    ExpCharStart, ExpCharEnd: Integer; ExpClipStart, ExpClipEnd: Integer;
    ExpIsRtl: Boolean; ExpText: String);
  begin
    TestNext(ALimits);

    TestBound(Name + ' Start', Token.StartPos, ExpStart);
    TestBound(Name + ' End',   Token.EndPos, ExpEnd);
    AssertEquals(Name + ' PhysicalCharStart', ExpCharStart, Token.PhysicalCharStart);
    AssertEquals(Name + ' PhysicalCharEnd',   ExpCharEnd,   Token.PhysicalCharEnd);
    AssertEquals(Name + ' PhysicalClipStart', ExpClipStart, Token.PhysicalClipStart);
    AssertEquals(Name + ' PhysicalClipEnd',   ExpClipEnd,   Token.PhysicalClipEnd);
    AssertEquals(Name + ' ExpIsRtl',  ExpIsRtl,   Token.RtlInfo.IsRtl);
    AssertEquals(Name + ' Text',   ExpText,  copy(Token.Tk.TokenStart, 1, Token.Tk.TokenLength));
  end;

  procedure TestNext(ALimits: Array of Integer;
    ExpStart, ExpEnd: TBoundExpectation;
    ExpCharStart, ExpCharEnd: Integer; ExpClipStart, ExpClipEnd: Integer;
    ExpIsRtl: Boolean; ExpText: String;
    ExpFColor: TColor);
  begin
    TestNext(ALimits, ExpStart, ExpEnd, ExpCharStart, ExpCharEnd, ExpClipStart,
      ExpClipEnd, ExpIsRtl, ExpText);

    AssertEquals(Name + ' F-Color',   ExpFColor,  Token.Attr.Foreground);
  end;

  procedure TestNext(ALimits: Array of Integer;
    ExpStart, ExpEnd: TBoundExpectation;
    ExpCharStart, ExpCharEnd: Integer; ExpClipStart, ExpClipEnd: Integer;
    ExpIsRtl: Boolean; ExpText: String;
    ExpNext: TBoundExpectation; ExpNxtIsRtl: Boolean);
  begin
    TestNext(ALimits, ExpStart, ExpEnd, ExpCharStart, ExpCharEnd, ExpClipStart,
      ExpClipEnd, ExpIsRtl, ExpText);

    TestBound(Name + ' Next', Token.NextPos, ExpNext);
    AssertEquals(Name + 'Nxt IsRtl',  ExpNxtIsRtl,  Token.NextRtlInfo.IsRtl);
  end;

  procedure TestNext(ALimits: Array of Integer;
    ExpStart, ExpEnd: TBoundExpectation;
    ExpCharStart, ExpCharEnd: Integer; ExpClipStart, ExpClipEnd: Integer;
    ExpIsRtl: Boolean; ExpText: String;
    ExpFColor: TColor;
    ExpNext: TBoundExpectation; ExpNxtIsRtl: Boolean);
  begin
    TestNext(ALimits, ExpStart, ExpEnd, ExpCharStart, ExpCharEnd, ExpClipStart,
      ExpClipEnd, ExpIsRtl, ExpText, ExpNext, ExpNxtIsRtl);

    AssertEquals(Name + ' F-Color',   ExpFColor,  Token.Attr.Foreground);
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

  procedure TestEnd(ExpStart: TBoundExpectation);
  begin
    TestEnd;
    TestBound(Name + ' Start(EOL)', Token.StartPos, ExpStart);
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
    FTheHighLighter.AddToken(17-1, 2, FtkBlue);   // //

    FTheHighLighter.AddToken(18-1, 6, FtkGreen);   // '      ' 6 spaces
    FTheHighLighter.AddToken(18-1, 7, FtkYellow);  // '
    FTheHighLighter.AddToken(18-1,11, FtkBlue);    // شس
    FTheHighLighter.AddToken(18-1,12, FtkYellow);  // '
    FTheHighLighter.AddToken(18-1,13, FtkRed);  // ,

    FTheHighLighter.AddToken(19-1, 6, FtkGreen);   // '      ' 6 spaces
    FTheHighLighter.AddToken(19-1, 7, FtkYellow);  // '
    FTheHighLighter.AddToken(19-1,15, FtkBlue);    // شس

    FTheHighLighter.AddToken(20-1, 6, FtkGreen);   // '      ' 6 spaces
    FTheHighLighter.AddToken(20-1, 7, FtkYellow);  // '
    FTheHighLighter.AddToken(20-1,13, FtkRed);    // شس 3 chars, ekcept last will be red
    FTheHighLighter.AddToken(20-1,15, FtkBlue);    // شس

    {%region  LTR only }
      PushBaseName('LTR-Only');

      {%region  full line}
        TestStart('Scan full line / hl-token',   2,   1, 100,   2);
        TestNext([100],    b( 1, 1, 0),  b(10,10, 0),    1,10,   1,10,   False, 'interface', clBlue,   b(10,10, 0), False);
        TestNext([100],    b(10,10, 0),  b(12,12, 0),   10,12,  10,12,   False, '//',        clBlack,  b(12,12, 0), False);
        TestEnd(b(12,12,0));

        TestStart('Scan full line / hl-token',   2,   1, 100,   2);
        TestPreFirst(b( 1, 1, 0), False);
        TestNext([],    b( 1, 1),  b(10,10),    1,10,   1,10,   False, 'interface',  b(10,10, 0), False);
        TestNext([],    b(10,10),  b(12,12),   10,12,  10,12,   False, '//',         b(12,12, 0), False);
        TestEnd(b(12,12, 0));
      {%endregion}

      {%region  cut off end of line}
        TestStart('Cut off end',   2,   1, 5,   2);

        TestNext([100],   b( 1, 1),  b( 5, 5),    1, 5,   1, 5,   False, 'inte',  b( 5, 5, 0), False);
        TestEnd(b( 5, 5, 0));
      {%endregion}

      {%region  cut off start of line}
        TestStart('Cut off start',   2,   3, 100,   2);
        TestNext([100],    b( 3, 3),  b(10,10),    3,10,   3,10,   False, 'terface',  b(10,10, 0), False);
        TestNext([100],    b(10,10),  b(12,12),   10,12,  10,12,   False, '//',       b(12,12, 0), False);
        TestEnd(b(12,12, 0));

        TestStart('Cut off start 1 tok',   2,  10, 100,   2);
        TestNext([100],    b(10,10),  b(12,12),   10,12,  10,12,   False, '//',   b(12,12, 0), False);
        TestEnd(b(12,12, 0));

        TestStart('Cut off start 1.5 tok',   2,  11, 100,   2);
        TestNext([100],    b(11,11),  b(12,12),   11,12,  11,12,   False, '/',   b(12,12, 0), False);
        TestEnd(b(12,12, 0));

        TestStart('Cut off entire line',   2,  20, 50,   2);
        AssertFalse(Name + 'Got NO Token', FTokenBreaker.GetNextHighlighterTokenFromView(Token, -1, 1));
        TestBound(Name + ' Start(BOL)', Token.StartPos, b(12,12, 0));
      {%endregion}

      {%region  cut off both}
        TestStart('Cut off both',   2,   3, 10,   2);
        TestNext([100],    b( 3, 3),  b(10,10),    3,10,   3,10,   False, 'terface');
        //TestNext([100],    b(10,10),  b(12,12),   10,12,  10,12,   False, '//');
        TestEnd;

        TestStart('Cut off both - 2 token',   2,   3, 11,   2);
        TestNext([100],    b( 3, 3),  b(10,10),    3,10,   3,10,   False, 'terface');
        TestNext([100],    b(10,10),  b(11,11),   10,11,  10,11,   False, '/');
        TestEnd;

        TestStart('Cut off both - 1 token, skip 1st',   2,   10,11,   2);
        TestNext([100],    b(10,10),  b(11,11),   10,11,  10,11,   False, '/');
        TestEnd;
      {%endregion}

      {%region  1 token 2 parts}
        TestStart('1 token 2 parts',   2,   1, 100,   2);

        TestNext([  3],    b( 1, 1, 0),  b( 3, 3, 0),    1, 3,   1, 3,   False, 'in',clBlue );
        TestNext([100],    b( 3, 3, 0),  b(10,10, 0),    3,10,   3,10,   False, 'terface', clBlue);
        TestNext([100],    b(10,10, 0),  b(12,12, 0),   10,12,  10,12,   False, '//', clBlack);
        TestEnd;
      {%endregion}

      // part chars/tabs

      {%region  cut off PART of char }
        // start at 1
        TestStart('cut off PART of char (end)',     7,   1, 100,   7);
        TestPreFirst(b( 1, 1, 0), False);
        TestNext([  3],   b( 1, 1, 0),  b( 3, 2,-2),    1, 5,   1, 3,   False, #9,  b( 3, 1, 2), False);
        TestNext([100],   b( 3, 1, 2),  b( 5, 2, 0),    1, 5,   3, 5,   False, #9,  b( 5, 2, 0), False);
        TestNext([100],   b( 5, 2, 0),  b( 6, 3, 0),    5, 6,   5, 6,   False, 'i');

        TestStart('cut off PART of char (end) next-limit',   7,   1, 100,   7);
        TestPreFirst(b( 1, 1, 0), False);
        TestNext([  3],   b( 1, 1),  b( 3, 2),    1, 5,   1, 3,   False, #9,  b( 3, 1, 2), False);
        TestNext([  5],   b( 3, 1),  b( 5, 2),    1, 5,   3, 5,   False, #9,  b( 5, 2, 0), False);
        TestNext([100],   b( 5, 2),  b( 6, 3),    5, 6,   5, 6,   False, 'i');

        TestStart('cut off PART of char (end) next-limit-log',   7,   1, 100,   7);
        TestPreFirst(b( 1, 1, 0), False);
        TestNext([  3],   b( 1, 1),  b( 3, 2),    1, 5,   1, 3,   False, #9,  b( 3, 1, 2), False);
        TestNext([99,2],  b( 3, 1),  b( 5, 2),    1, 5,   3, 5,   False, #9,  b( 5, 2, 0), False);
        TestNext([100],   b( 5, 2),  b( 6, 3),    5, 6,   5, 6,   False, 'i');

        // start at 2
        TestStart('cut off PART of char (begin)',   7,   2, 100,   7);
        TestPreFirst(b( 2, 1, 1), False);
        TestNext([  5],   b( 2, 1),  b( 5, 2),     1, 5,    2,  5,    False, #9);
        TestNext([100],   b( 5, 2),  b( 6, 3),    5, 6,   5, 6,   False, 'i');

        TestStart('cut off PART of char (both) continue',   7,   2, 100,   7);
        TestPreFirst(b( 2, 1, 1), False);
        TestNext([  3],   b( 2, 1),  b( 3, 2),     1, 5,    2,  3,    False, #9);
        TestNext([100],   b( 3, 1),  b( 5, 2),     1, 5,    3,  5,    False, #9);
        TestNext([100],   b( 5, 2),  b( 6, 3),    5, 6,   5, 6,   False, 'i');

        TestStart('cut off PART of char (both) global-limit',   7,   2, 3,   7);
        TestPreFirst(b( 2, 1, 1), False);
        TestNext([100],   b( 2, 1),  b( 3, 2),     1, 5,    2,  3,    False, #9);
        TestEnd;

        TestStart('cut off PART of char (both) next-limit',   7,   2, 100,   7);
        TestPreFirst(b( 2, 1, 1), False);
        TestNext([3],   b( 2, 1),  b( 3, 2),     1, 5,    2,  3,    False, #9);
        //TestEnd(3);

        // start at 3
        TestStart('cut off PART of char (begin)',   7,   3, 100,   7);
        TestPreFirst(b( 3, 1, 2), False);
        TestNext([  5],   b( 3, 1),  b( 5, 2),     1, 5,    3,  5,    False, #9);
        TestNext([100],   b( 5, 2),  b( 6, 3),    5, 6,   5, 6,   False, 'i');

        // start at 4
        TestStart('cut off PART of char (begin)',   7,   4, 100,   7);
        TestPreFirst(b( 4, 1, 3), False);
        TestNext([  5],   b( 4, 1),  b( 5, 2),     1, 5,    4,  5,    False, #9);
        TestNext([100],   b( 5, 2),  b( 6, 3),    5, 6,   5, 6,   False, 'i');

        // start at 5
        TestStart('cut off PART of char (begin)',   7,   5, 100,   7);
        TestPreFirst(b( 5, 2, 0), False);
        TestNext([100],   b( 5, 2),  b( 6, 3),    5, 6,   5, 6,   False, 'i');
      {%endregion}

      {%region  cut tab in many}
        TestStart('cut tab in many',   9,   2, 100,   9);

        TestNext([  3],   b( 2, 1),  b( 3, 2),     1, 5,    2,  3,    False, #9);
        TestNext([  6],   b( 3, 1),  b( 6, 3),    1, 9,   3, 6,   False, #9#9);
        TestNext([ 11],   b( 6, 2),  b(11, 4),    5,13,   6,11,   False, #9#9);
        TestNext([ 13],   b(11, 3),  b(13, 4),     9,13,   11, 13,    False, #9);
        TestNext([ 15],   b(13, 4),  b(15, 5),    13,17,   13, 15,    False, #9);
      {%endregion}

      {%region  break line // phys log}
        TestStart('Scan full line',  14,   1, 100,   14);
        TestPreFirst(b( 1, 1, 0), False);
        TestNext([-1, -1],    b( 1, 1),  b(13, 7),    1,13,   1,13,   False, #9'i'#9'12'#9);

        TestStart('Scan full line',  14,   1, 100,   14);
        TestPreFirst(b( 1, 1, 0), False);
        TestNext([-1, 2],    b( 1, 1),  b( 5, 2),    1, 5,   1, 5,   False, #9,  b( 5, 2, 0), False);
        TestNext([-1, 3],    b( 5, 2),  b( 6, 3),    5, 6,   5, 6,   False, 'i');

        TestStart('Scan full line',  14,   1, 100,   14);
        TestPreFirst(b( 1, 1, 0), False);
        TestNext([99, 2],    b( 1, 1),  b( 5, 2),    1, 5,   1, 5,   False, #9,  b( 5, 2, 0), False);
        TestNext([-1, 3],    b( 5, 2),  b( 6, 3),    5, 6,   5, 6,   False, 'i');

        TestStart('Scan full line',  14,   1, 100,   14);
        TestPreFirst(b( 1, 1, 0), False);
        TestNext([ 5, 2],    b( 1, 1),  b( 5, 2),    1, 5,   1, 5,   False, #9,  b( 5, 2, 0), False);
        TestNext([-1, 3],    b( 5, 2),  b( 6, 3),    5, 6,   5, 6,   False, 'i');

        TestStart('Scan full line',  14,   1, 100,   14);
        TestPreFirst(b( 1, 1, 0), False);
        TestNext([ 4, 2],    b( 1, 1),  b( 4, 2),    1, 5,   1, 4,   False, #9,  b( 4, 1, 3), False);
        TestNext([-1, 2],    b( 4, 1),  b( 5, 2),    1, 5,   4, 5,   False, #9,  b( 5, 2, 0), False);
        TestNext([-1, 3],    b( 5, 2),  b( 6, 3),    5, 6,   5, 6,   False, 'i');


      {%endregion}

      PopBaseName;
    {%endregion}

    {%region  RTL only }
      PushBaseName('RTL-Only');
      {%region  full line}
        TestStart('Scan full line',  11,   1, 100,   11);
        TestPreFirst(b( 3, 1, 0), True);
        TestNext([100],    b( 3, 1, 0),  b( 1, 5, 0),    1, 3,   1, 3,   True,  'شس',  b( 3, 5, 0), False);
        TestEnd(b( 3, 5, 0));

        TestStart('Scan full line',  11,   1, 100,   11);
        TestPreFirst(b( 3, 1, 0), True);
        TestNext([],    b( 3, 1, 0),  b( 1, 5, 0),    1, 3,   1, 3,   True,  'شس',   b( 3, 5, 0), False);
        TestEnd(b( 3, 5, 0));

        TestStart('Scan full line (2 words)',  12,   1, 100,   12);
        TestPreFirst(b( 5, 1, 0), True);
        TestNext([100],    b( 5, 1, 0),  b( 1, 8, 0),    1, 5,   1, 5,   True,  'شس ي',   b( 5, 8, 0), False);
        TestEnd(b( 5, 8, 0));
      {%endregion}

      {%region  Cut tab}
        (*
          Tab implemetation may change. currently it is calculated on the logical pos of the tab
        *)
        // tab will be 3 width
        TestStart('Scan full line',  15,   3, 7,   15);
        TestPreFirst(b( 7, 1, 0), True);
        TestNext([],    b( 7, 1, 0),  b( 3, 4, 0),    3, 7,   3, 7,   True,  'ي'#9,   b( 7, 8, 0), False);
        TestEnd(b( 7, 8, 0));

        TestStart('Scan full line',  15,   4, 7,   15);
        TestPreFirst(b( 7, 1, 0), True);
        TestNext([],    b( 7, 1, 0),  b( 4, 4,-1),    3, 7,   4, 7,   True,  'ي'#9,   b( 7, 8, 0), False);
        TestEnd(b( 7, 8, 0));

        TestStart('Scan full line',  15,   5, 7,   15);
        TestPreFirst(b( 7, 1, 0), True);
        TestNext([],    b( 7, 1, 0),  b( 5, 4,-2),    3, 7,   5, 7,   True,  'ي'#9,   b( 7, 8, 0), False);
        TestEnd(b( 7, 8, 0));


        TestStart('Scan full line',  15,   1, 7,   15);
        TestPreFirst(b( 7, 1, 0), True);
        TestNext([],    b( 7, 1, 0),  b( 1, 8, 0),    1, 7,   1, 7,   True,  'ي'#9'شس',   b( 7, 8, 0), False);
        TestEnd(b( 7, 8, 0));

        TestStart('Scan full line',  15,   1, 6,   15);
        TestPreFirst(b( 6, 3, 0), True);
        // TODO NextPos and TestEnd: Phys pos is allowed to be 6 OR 7
        TestNext([],    b( 6, 3, 0),  b( 1, 8, 0),    1, 6,   1, 6,   True,  #9'شس',   b( 7, 8, 0), False);
        TestEnd(b( 7, 8, 0));

        TestStart('Scan full line',  15,   1, 5,   15);
        TestPreFirst(b( 5, 3, 1), True);
        TestNext([],    b( 5, 3, 1),  b( 1, 8, 0),    1, 6,   1, 5,   True,  #9'شس');

        TestStart('Scan full line',  15,   1, 4,   15);
        TestPreFirst(b( 4, 3, 2), True);
        TestNext([],    b( 4, 3, 2),  b( 1, 8, 0),    1, 6,   1, 4,   True,  #9'شس');

        // RTL tab in 2 pieces
        TestStart('RTL tab in 2 pieces',  15,   1, 7,   15);
        TestPreFirst(b( 7, 1, 0), True);
        TestNext([5],   b( 7, 1, 0),  b( 5, 4,-2),    3, 7,   5, 7,   True,  'ي'#9,    b( 5, 3, 1), True);
        TestNext([],    b( 5, 3, 1),  b( 1, 8, 0),    1, 6,   1, 5,   True,  #9'شس',   b( 7, 8, 0), False);
        TestEnd(b( 7, 8, 0));

      {%endregion}

      {%region  part line}
        // 1 char parts
        TestStart('part line - begin',  12,   1, 2,  12);
        TestPreFirst(b( 2, 6, 0), True);
        TestNext([100],    b( 2, 6),  b( 1, 8),    1, 2,   1, 2,   True,  'ي',   b( 5, 8, 0), False);
        TestEnd(b( 5, 8, 0));

        TestStart('part line - mid',  12,   2, 3,  12);
        TestPreFirst(b( 3, 5, 0), True);
        TestNext([100],    b( 3, 5),  b( 2, 6),    2, 3,   2, 3,   True,  ' ',   b( 5, 8, 0), False);
        TestEnd(b( 5, 8, 0));

        TestStart('part line - mid',  12,   3, 4,  12);
        TestPreFirst(b( 4, 3, 0), True);
        TestNext([100],    b( 4, 3),  b( 3, 5),    3, 4,   3, 4,   True,  'س',   b( 5, 8, 0), False);
        TestEnd(b( 5, 8, 0));

        TestStart('part line - end',  12,   4, 5,  12);
        TestPreFirst(b( 5, 1, 0), True);
        TestNext([100],    b( 5, 1),  b( 4, 3),    4, 5,   4, 5,   True,  'ش',   b( 5, 8, 0), False);
        TestEnd(b( 5, 8, 0));

        // 2 char parts
        TestStart('part line - begin(2)',  12,   1, 3,  12);
        TestPreFirst(b( 3, 5, 0), True);
        TestNext([100],    b( 3, 5),  b( 1, 8),    1, 3,   1, 3,   True,  ' ي');
        TestEnd;

        TestStart('part line - mid(2)',  12,   2, 4,  12);
        TestPreFirst(b( 4, 3, 0), True);
        TestNext([100],    b( 4, 3),  b( 2, 6),    2, 4,   2, 4,   True,  'س ');
        TestEnd;

        TestStart('part line - end(2)',  12,   3, 5,  12);
        TestPreFirst(b( 5, 1, 0), True);
        TestNext([100],    b( 5, 1),  b( 3, 5),    3, 5,   3, 5,   True,  'شس');
        TestEnd;

        // 1 char parts, several chunks
        TestStart('part line - begin',  12,   1, 100,  12);
        TestPreFirst(b( 5, 1, 0), True);
        TestNext([4],    b( 5, 1),  b( 4, 3),    4, 5,   4, 5,   True,  'ش',   b( 4, 3, 0), True);
        TestNext([3],    b( 4, 3),  b( 3, 5),    3, 4,   3, 4,   True,  'س',   b( 3, 5, 0), True);
        TestNext([2],    b( 3, 5),  b( 2, 6),    2, 3,   2, 3,   True,  ' ',   b( 2, 6, 0), True);
        TestNext([1],    b( 2, 6),  b( 1, 8),    1, 2,   1, 2,   True,  'ي',   b( 5, 8, 0), False);
        TestEnd(b( 5, 8, 0));

        // 1 char parts, several chunks
        TestStart('part line - begin',  12,   1, 100,  12);
        TestPreFirst(b( 5, 1, 0), True);
        TestNext([4],    b( 5, 1),  b( 4, 3),    4, 5,   4, 5,   True,  'ش');
        TestNext([3],    b( 4, 3),  b( 3, 5),    3, 4,   3, 4,   True,  'س');
        TestNext([0],    b( 3, 5),  b( 1, 8),    1, 3,   1, 3,   True,  ' ي');
        TestEnd;

        //TestStart('part line - begin',  12,   1, 100,  12);
        //TestNext(2,    1, 6,   2,  5,   2,  5,  True,  'شس ');
        //TestNext(5,    1, 3,   4,  5,   4,  5,  True,  'ي');
        //TestEnd;
        //
        //TestStart('part line - begin',  12,   1, 100,  12);
        //TestNext(  2,    1, 6,   2,  5,   2,  5,  True,  'شس ');
        //TestNext([100],    b( 5, 1),  b( 4, 3),    4, 5,   4, 5,   True,  'شس ي');
        //TestEnd;

      {%endregion}

      PopBaseName;
    {%endregion}

    {%region  MIXED Rtl/Ltr }
      PushBaseName('MIXED Rtl/Ltr');
      {%region  full line}
        TestStart('Scan full line',  13,   1, 100,   13);
        TestPreFirst(b( 1, 1, 0), False);
        TestNext([],    b( 1, 1),  b( 3, 3),    1, 3,   1, 3,   False, 'AB',    b( 7, 3, 0), True);
        TestNext([],    b( 7, 3),  b( 3,10),    3, 7,   3, 7,   True,  'شس ي',  b( 7,10, 0), False);
        TestNext([],    b( 7,10),  b( 9,12),    7, 9,   7, 9,   False, 'CD',    b( 9,12, 0), False);
        TestEnd(b( 9,12, 0));

      {%endregion}

      {%region  parts}
        TestStart('Scan part line, cut at start',  13,   2, 100,   13);
        TestPreFirst(b( 2, 2, 0), False);
        TestNext([],    b( 2, 2),  b( 3, 3),    2, 3,   2, 3,   False, 'B',  b( 7, 3, 0), True);
        TestNext([],    b( 7, 3),  b( 3,10),    3, 7,   3, 7,   True,  'شس ي',  b( 7,10, 0), False);
        TestNext([],    b( 7,10),  b( 9,12),    7, 9,   7, 9,   False, 'CD');
        TestEnd;

        TestStart('Scan part line, cut at start',  13,   3, 100,   13);
        TestPreFirst(b( 7, 3, 0), True);
        TestNext([],    b( 7, 3),  b( 3,10),    3, 7,   3, 7,   True,  'شس ي',  b( 7,10, 0), False);
        TestNext([],    b( 7,10),  b( 9,12),    7, 9,   7, 9,   False, 'CD');
        TestEnd;

        TestStart('Scan part line, cut at start',  13,   4, 100,   13);
        TestPreFirst(b( 7, 3, 0), True);
        TestNext([],    b( 7, 3),  b( 4, 8),    4, 7,   4, 7,   True,  'شس ',  b( 7,10, 0), False);
        TestNext([],    b( 7,10),  b( 9,12),    7, 9,   7, 9,   False, 'CD');
        TestEnd;

        TestStart('Scan part line, cut at start',  13,   6, 100,   13);
        TestPreFirst(b( 7, 3, 0), True);
        TestNext([],    b( 7, 3),  b( 6, 5),    6, 7,   6, 7,   True,  'ش',  b( 7,10, 0), False);
        TestNext([],    b( 7,10),  b( 9,12),    7, 9,   7, 9,   False, 'CD');
        TestEnd;

        TestStart('Scan part line, cut at start',  13,   7, 100,   13);
        TestPreFirst(b( 7,10, 0), False);
        TestNext([],    b( 7,10),  b( 9,12),    7, 9,   7, 9,   False, 'CD');
        TestEnd;

        TestStart('Scan part line, cut at start',  13,   8, 100,   13);
        TestPreFirst(b( 8,11, 0), False);
        TestNext([],    b( 8,11),  b( 9,12),    8, 9,   8, 9,   False, 'D');
        TestEnd;

        TestStart('Scan part line, cut at start',  13,   9, 100,   13);
        TestEnd;


        TestStart('Scan part line, cut at end',  13,   1, 8,   13);
        TestNext([],    b( 1, 1),  b( 3, 3),    1, 3,   1, 3,   False, 'AB',  b( 7, 3, 0), True);
        TestNext([],    b( 7, 3),  b( 3,10),    3, 7,   3, 7,   True,  'شس ي',  b( 7,10, 0), False);
        TestNext([],    b( 7,10),  b( 8,11),    7, 8,   7, 8,   False, 'C');
        TestEnd;

        TestStart('Scan part line, cut at end',  13,   1, 7,   13);
        TestNext([],    b( 1, 1),  b( 3, 3),    1, 3,   1, 3,   False, 'AB',  b( 7, 3, 0), True);
        TestNext([],    b( 7, 3),  b( 3,10),    3, 7,   3, 7,   True,  'شس ي');
        TestEnd;

        TestStart('Scan part line, cut at end',  13,   1, 6,   13);
        TestNext([],    b( 1, 1),  b( 3, 3),    1, 3,   1, 3,   False, 'AB',  b( 6, 5, 0), True);
        TestNext([],    b( 6, 5),  b( 3,10),    3, 6,   3, 6,   True,  'س ي');
        TestEnd;

        TestStart('Scan part line, cut at end',  13,   1, 4,   13);
        TestNext([],    b( 1, 1),  b( 3, 3),    1, 3,   1, 3,   False, 'AB',  b( 4, 8, 0), True);
        TestNext([],    b( 4, 8),  b( 3,10),    3, 4,   3, 4,   True,  'ي');
        TestEnd;

        TestStart('Scan part line, cut at end',  13,   1, 3,   13);
        TestNext([],    b( 1, 1),  b( 3, 3),    1, 3,   1, 3,   False, 'AB');
        TestEnd;

        TestStart('Scan part line, cut at end',  13,   1, 2,   13);
        TestNext([],    b( 1, 1),  b( 2, 2),    1, 2,   1, 2,   False, 'A');
        TestEnd;

      {%endregion}


      {%region}
        TestStart('Scan full line 18 / hl-token',   20,   9, 30,   20);
        TestNext([],    b(12, 8, 0),  b( 9,14, 0),    9,12,   9,12,   True,  'شسق', clRed,   b(12, 16, 0), False);
        TestEnd(b(12, 16, 0));

        TestStart('Scan full line 18 / hl-token',   20,  10, 30,   20);
        TestNext([],    b(12, 8, 0),  b(10,12, 0),   10,12,  10,12,   True,  'شس', clRed,   b(12, 16, 0), False);
        TestEnd(b(12, 16, 0));

      {%endregion}


      PopBaseName;
    {%endregion}


    UseViewTokenOnly := False;

    {%region  LTR only }
      PushBaseName('LTR-Only - WITH MARKUP');

      {%region  full line}
        SynEdit.BlockBegin := Point( 1,1); // clear selection
        TestStart('Scan full line / hl-token',   1,   1, 99,   1);
        TestNext([],    b( 1, 1, 0),  b(10,10, 0),    1,10,   1,10,   False, 'unit foo;', clBlack);
        TestNext([],    b(10,10, 0),  b(99,99, 0),   10,99,  10,99,   False, ' ',         clBlack);
        TestEnd;

        SynEdit.BlockBegin := Point(2,1);
        SynEdit.BlockEnd   := Point(4,1);
        TestStart('Scan full line / hl-token',   1,   1, 99,   1);
        TestNext([],    b( 1, 1, 0),  b( 2, 2, 0),    1, 2,   1, 2,   False, 'u',         clBlack);
        TestNext([],    b( 2, 2, 0),  b( 4, 4, 0),    2, 4,   2, 4,   False, 'ni',        clPurple);
        TestNext([],    b( 4, 4, 0),  b(10,10, 0),    4,10,   4,10,   False, 't foo;',    clBlack);
        TestNext([],    b(10,10, 0),  b(99,99, 0),   10,99,  10,99,   False, ' ',         clBlack);
        TestEnd;

        SynEdit.BlockBegin := Point(2,1);
        SynEdit.BlockEnd   := Point(4,1);
        TestStart('Scan full line / hl-token',   1,   6, 99,   1);
        TestNext([],    b( 6, 6, 0),  b(10,10, 0),    6,10,   6,10,   False, 'foo;',      clBlack);
        TestNext([],    b(10,10, 0),  b(99,99, 0),   10,99,  10,99,   False, ' ',         clBlack);
        TestEnd;

        SynEdit.BlockBegin := Point(20,1);
        SynEdit.BlockEnd   := Point(30,1);
        TestStart('Scan full line / hl-token',   1,   1, 99,   1);
        TestNext([],    b( 1, 1, 0),  b(10,10, 0),    1,10,   1,10,   False, 'unit foo;', clBlack);
        TestNext([],    b(10,10, 0),  b(20,20, 0),   10,20,  10,20,   False, ' ',         clBlack);
        TestNext([],    b(20,20, 0),  b(30,30, 0),   20,30,  20,30,   False, ' ',         clPurple);
        TestNext([],    b(30,30, 0),  b(99,99, 0),   30,99,  30,99,   False, ' ',         clBlack);
        TestEnd;

        SynEdit.BlockBegin := Point( 8,1);
        SynEdit.BlockEnd   := Point(30,1);
        TestStart('Scan full line / hl-token',   1,   1, 99,   1);
        TestNext([],    b( 1, 1, 0),  b( 8, 8, 0),    1, 8,   1, 8,   False, 'unit fo',    clBlack);
        TestNext([],    b( 8, 8, 0),  b(10,10, 0),    8,10,   8,10,   False, 'o;',       clPurple);
        TestNext([],    b(10,10, 0),  b(30,30, 0),   10,30,  10,30,   False, ' ',         clPurple);
        TestNext([],    b(30,30, 0),  b(99,99, 0),   30,99,  30,99,   False, ' ',         clBlack);
        TestEnd;


        // Paint eol only
        SynEdit.BlockBegin := Point( 1,1); // clear selection
        TestStart('Scan full line / hl-token',   1,  10, 99,   1);
        TestNext([],    b(10,10, 0),  b(99,99, 0),   10,99,  10,99,   False, ' ',         clBlack);
        TestEnd;

        SynEdit.BlockBegin := Point(20,1);
        SynEdit.BlockEnd   := Point(30,1);
        TestStart('Scan full line / hl-token',   1,  25, 99,   1);
        TestNext([],    b(25,25, 0),  b(30,30, 0),   25,30,  25,30,   False, ' ',         clPurple);
        TestNext([],    b(30,30, 0),  b(99,99, 0),   30,99,  30,99,   False, ' ',         clBlack);
        TestEnd;
      {%endregion}

      {%region}
        SynEdit.Options :=SynEdit.Options + [eoBracketHighlight];
        SynEdit.BracketHighlightStyle := sbhsBoth;
        SynEdit.BracketMatchColor.Foreground := clMaroon;
        SynEdit.BracketMatchColor.FrameColor := clBlack;
        SynEdit.BracketMatchColor.FrameEdges := sfeAround;
        SynEdit.BracketMatchColor.FrameStyle := slsSolid;

        SynEdit.LogicalCaretXY := point(2, 16);
        TestStart('Scan full line / hl-token',   16,  1, 10,   16);
        TestNext([],    b( 1, 1, 0),  b( 5, 2, 0),   1, 5,   1, 5,   False, #9,         clBlack);
        AssertEquals(Name + ' #9 No-Frame l',   clNone,  Token.Attr.FrameSideColors[bsLeft]);
        AssertEquals(Name + ' #9 No-Frame r',   clNone,  Token.Attr.FrameSideColors[bsRight]);
        TestNext([],    b( 5, 2, 0),  b( 6, 3, 0),   5, 6,   5, 6,   False, '(',        clMaroon);
        AssertEquals(Name + ' "(" Frame t',   clBlack,  Token.Attr.FrameSideColors[bsTop]);
        AssertEquals(Name + ' "(" Frame l',   clBlack,  Token.Attr.FrameSideColors[bsLeft]);
        AssertEquals(Name + ' "(" Frame r',   clBlack,  Token.Attr.FrameSideColors[bsRight]);
        TestNext([],    b( 6, 3, 0),  b( 9, 4, 0),   6, 9,   6, 9,   False, #9,         clBlack);
      {%endregion}



    {%endregion}

    {%region  RTL only }
      PushBaseName('RTL-Only - WITH MARKUP');
      {%region  full line}
        SynEdit.BlockBegin := Point( 1,1); // clear selection
        TestStart('Scan full line',  11,   1, 90,   11);
        TestPreFirst(b( 3, 1, 0), True);
        TestNext([],    b( 3, 1, 0),  b( 1, 5, 0),    1, 3,   1, 3,   True,  'شس',   clBlack);
        TestNext([],    b( 3, 5, 0),  b(90,92, 0),    3,90,   3,90,   False, ' ',    clBlack);
        TestEnd;

        SynEdit.BlockBegin := Point(3,12); // 2nd logical char
        SynEdit.BlockEnd   := Point(5,12);
        TestStart('Scan full line',  12,   1, 90,   12);
        TestPreFirst(b( 5, 1, 0), True);
        TestNext([],    b( 5, 1, 0),  b( 4, 3, 0),    4, 5,   4, 5,   True,  'ش',    clBlack);
        TestNext([],    b( 4, 3, 0),  b( 3, 5, 0),    3, 4,   3, 4,   True,  'س',    clPurple);
        TestNext([],    b( 3, 5, 0),  b( 1, 8, 0),    1, 3,   1, 3,   True,  ' ي',   clBlack);
        TestNext([],    b( 5, 8, 0),  b(90,93, 0),    5,90,   5,90,   False, ' ',    clBlack);
        TestEnd;
      {%endregion}
      PopBaseName;
    {%endregion}


        // HL token ends exactly at LTR to RTL change

        SynEdit.BlockBegin := Point(1,1);
        TestStart('Scan full line 17 / hl-token',   17,   1, 30,   17);
        TestNext([],    b( 1, 1, 0),  b( 3, 3, 0),    1, 3,   1, 3,   False, '//',            clBlue);
        TestNext([],    b(16, 3, 0),  b( 3,25, 0),    3,16,   3,16,   True,  'شس يشس ي(شس ي', clBlack);
        TestNext([],    b(16,25, 0),  b(30,39, 0),   16,30,  16,30,   False, ' ',             clBlack);
        TestEnd;

        SynEdit.BlockBegin := Point( 3,17);
        SynEdit.BlockEnd   := Point(25,17);
        TestStart('Scan full line 17 / hl-token',   17,   1, 30,   17);
        TestNext([],    b( 1, 1, 0),  b( 3, 3, 0),    1, 3,   1, 3,   False, '//',            clBlue);
        TestNext([],    b(16, 3, 0),  b( 3,25, 0),    3,16,   3,16,   True,  'شس يشس ي(شس ي', clPurple);
        TestNext([],    b(16,25, 0),  b(30,39, 0),   16,30,  16,30,   False, ' ',             clBlack);
        TestEnd;

        SynEdit.BlockBegin := Point( 3,17);
        SynEdit.BlockEnd   := Point(22,17); // 2 chars off
        TestStart('Scan full line 17 / hl-token',   17,   1, 30,   17);
        TestNext([],    b( 1, 1, 0),  b( 3, 3, 0),    1, 3,   1, 3,   False, '//',          clBlue);
        TestNext([],    b(16, 3, 0),  b( 5,22, 0),    5,16,   5,16,   True,  'شس يشس ي(شس', clPurple);
        TestNext([],    b( 5,22, 0),  b( 3,25, 0),    3, 5,   3, 5,   True,  ' ي',          clBlack);
        TestNext([],    b(16,25, 0),  b(30,39, 0),   16,30,  16,30,   False, ' ',           clBlack);
        TestEnd;

        SynEdit.BlockBegin := Point( 7,17); // 2 chars off
        SynEdit.BlockEnd   := Point(25,17);
        TestStart('Scan full line 17 / hl-token',   17,   1, 30,   17);
        TestNext([],    b( 1, 1, 0),  b( 3, 3, 0),    1, 3,   1, 3,   False, '//',          clBlue);
        TestNext([],    b(16, 3, 0),  b(14, 7, 0),   14,16,  14,16,   True,  'شس',          clBlack);
        TestNext([],    b(14, 7, 0),  b( 3,25, 0),    3,14,   3,14,   True,  ' يشس ي(شس ي', clPurple);
        TestNext([],    b(16,25, 0),  b(30,39, 0),   16,30,  16,30,   False, ' ',           clBlack);
        TestEnd;

        SynEdit.BlockBegin := Point( 7,17);
        SynEdit.BlockEnd   := Point(22,17);
        TestStart('Scan full line 17 / hl-token',   17,   1, 30,   17);
        TestNext([],    b( 1, 1, 0),  b( 3, 3, 0),    1, 3,   1, 3,   False, '//',          clBlue);
        TestNext([],    b(16, 3, 0),  b(14, 7, 0),   14,16,  14,16,   True,  'شس',          clBlack);
        TestNext([],    b(14, 7, 0),  b( 5,22, 0),    5,14,   5,14,   True,  ' يشس ي(شس', clPurple);
        TestNext([],    b( 5,22, 0),  b( 3,25, 0),    3, 5,   3, 5,   True,  ' ي',          clBlack);
        TestNext([],    b(16,25, 0),  b(30,39, 0),   16,30,  16,30,   False, ' ',           clBlack);
        TestEnd;

        // Mixed RTL (and weak)
        // increase LeftChar
        SynEdit.BlockBegin := Point(1,1);
        TestStart('Scan full line 18 / hl-token',   18,   1, 30,   18);
        TestNext([],    b( 1, 1, 0),  b( 7, 7, 0),    1, 7,   1, 7,   False, 'a     ',  clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(10, 8, 0),  b( 8,12, 0),    8,10,   8,10,   True,  'شس',      clBlue);
        TestNext([],    b(10,12, 0),  b(11,13, 0),   10,11,  10,11,   False, '''',      clYellow);
        TestNext([],    b(11,13, 0),  b(12,14, 0),   11,12,  11,12,   False, 'b',       clRed);
        TestNext([],    b(12,14, 0),  b(30,32, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   18,   2, 30,   18);
        TestNext([],    b( 2, 2, 0),  b( 7, 7, 0),    2, 7,   2, 7,   False, '     ',   clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(10, 8, 0),  b( 8,12, 0),    8,10,   8,10,   True,  'شس',      clBlue);
        TestNext([],    b(10,12, 0),  b(11,13, 0),   10,11,  10,11,   False, '''',      clYellow);
        TestNext([],    b(11,13, 0),  b(12,14, 0),   11,12,  11,12,   False, 'b',       clRed);
        TestNext([],    b(12,14, 0),  b(30,32, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   18,   3, 30,   18);
        TestNext([],    b( 3, 3, 0),  b( 7, 7, 0),    3, 7,   3, 7,   False, '    ',    clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(10, 8, 0),  b( 8,12, 0),    8,10,   8,10,   True,  'شس',      clBlue);
        TestNext([],    b(10,12, 0),  b(11,13, 0),   10,11,  10,11,   False, '''',      clYellow);
        TestNext([],    b(11,13, 0),  b(12,14, 0),   11,12,  11,12,   False, 'b',       clRed);
        TestNext([],    b(12,14, 0),  b(30,32, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   18,   4, 30,   18);
        TestNext([],    b( 4, 4, 0),  b( 7, 7, 0),    4, 7,   4, 7,   False, '   ',     clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(10, 8, 0),  b( 8,12, 0),    8,10,   8,10,   True,  'شس',      clBlue);
        TestNext([],    b(10,12, 0),  b(11,13, 0),   10,11,  10,11,   False, '''',      clYellow);
        TestNext([],    b(11,13, 0),  b(12,14, 0),   11,12,  11,12,   False, 'b',       clRed);
        TestNext([],    b(12,14, 0),  b(30,32, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   18,   5, 30,   18);
        TestNext([],    b( 5, 5, 0),  b( 7, 7, 0),    5, 7,   5, 7,   False, '  ',      clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(10, 8, 0),  b( 8,12, 0),    8,10,   8,10,   True,  'شس',      clBlue);
        TestNext([],    b(10,12, 0),  b(11,13, 0),   10,11,  10,11,   False, '''',      clYellow);
        TestNext([],    b(11,13, 0),  b(12,14, 0),   11,12,  11,12,   False, 'b',       clRed);
        TestNext([],    b(12,14, 0),  b(30,32, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   18,   6, 30,   18);
        TestNext([],    b( 6, 6, 0),  b( 7, 7, 0),    6, 7,   6, 7,   False, ' ',       clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(10, 8, 0),  b( 8,12, 0),    8,10,   8,10,   True,  'شس',      clBlue);
        TestNext([],    b(10,12, 0),  b(11,13, 0),   10,11,  10,11,   False, '''',      clYellow);
        TestNext([],    b(11,13, 0),  b(12,14, 0),   11,12,  11,12,   False, 'b',       clRed);
        TestNext([],    b(12,14, 0),  b(30,32, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   18,   7, 30,   18);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(10, 8, 0),  b( 8,12, 0),    8,10,   8,10,   True,  'شس',      clBlue);
        TestNext([],    b(10,12, 0),  b(11,13, 0),   10,11,  10,11,   False, '''',      clYellow);
        TestNext([],    b(11,13, 0),  b(12,14, 0),   11,12,  11,12,   False, 'b',       clRed);
        TestNext([],    b(12,14, 0),  b(30,32, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   18,   8, 30,   18);
        TestNext([],    b(10, 8, 0),  b( 8,12, 0),    8,10,   8,10,   True,  'شس',      clBlue);
        TestNext([],    b(10,12, 0),  b(11,13, 0),   10,11,  10,11,   False, '''',      clYellow);
        TestNext([],    b(11,13, 0),  b(12,14, 0),   11,12,  11,12,   False, 'b',       clRed);
        TestNext([],    b(12,14, 0),  b(30,32, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   18,   9, 30,   18);
        TestNext([],    b(10, 8, 0),  b( 9,10, 0),    9,10,   9,10,   True,  'ش',      clBlue);
        TestNext([],    b(10,12, 0),  b(11,13, 0),   10,11,  10,11,   False, '''',      clYellow);
        TestNext([],    b(11,13, 0),  b(12,14, 0),   11,12,  11,12,   False, 'b',       clRed);
        TestNext([],    b(12,14, 0),  b(30,32, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   18,  10, 30,   18);
        TestNext([],    b(10,12, 0),  b(11,13, 0),   10,11,  10,11,   False, '''',      clYellow);
        TestNext([],    b(11,13, 0),  b(12,14, 0),   11,12,  11,12,   False, 'b',       clRed);
        TestNext([],    b(12,14, 0),  b(30,32, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   18,  11, 30,   18);
        TestNext([],    b(11,13, 0),  b(12,14, 0),   11,12,  11,12,   False, 'b',       clRed);
        TestNext([],    b(12,14, 0),  b(30,32, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        // Mixed RTL (and weak), end in RTL
        // increase LeftChar
        SynEdit.BlockBegin := Point(1,1);
        TestStart('Scan full line 18 / hl-token',   19,   1, 30,   19);
        TestNext([],    b( 1, 1, 0),  b( 7, 7, 0),    1, 7,   1, 7,   False, 'a     ',  clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(12, 8, 0),  b( 8,16, 0),    8,12,   8,12,   True,  'شسقف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   19,   2, 30,   19);
        TestNext([],    b( 2, 2, 0),  b( 7, 7, 0),    2, 7,   2, 7,   False, '     ',   clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(12, 8, 0),  b( 8,16, 0),    8,12,   8,12,   True,  'شسقف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   19,   3, 30,   19);
        TestNext([],    b( 3, 3, 0),  b( 7, 7, 0),    3, 7,   3, 7,   False, '    ',    clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(12, 8, 0),  b( 8,16, 0),    8,12,   8,12,   True,  'شسقف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   19,   4, 30,   19);
        TestNext([],    b( 4, 4, 0),  b( 7, 7, 0),    4, 7,   4, 7,   False, '   ',     clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(12, 8, 0),  b( 8,16, 0),    8,12,   8,12,   True,  'شسقف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   19,   5, 30,   19);
        TestNext([],    b( 5, 5, 0),  b( 7, 7, 0),    5, 7,   5, 7,   False, '  ',      clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(12, 8, 0),  b( 8,16, 0),    8,12,   8,12,   True,  'شسقف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   19,   6, 30,   19);
        TestNext([],    b( 6, 6, 0),  b( 7, 7, 0),    6, 7,   6, 7,   False, ' ',       clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(12, 8, 0),  b( 8,16, 0),    8,12,   8,12,   True,  'شسقف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   19,   7, 30,   19);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(12, 8, 0),  b( 8,16, 0),    8,12,   8,12,   True,  'شسقف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   19,   8, 30,   19);
        TestNext([],    b(12, 8, 0),  b( 8,16, 0),    8,12,   8,12,   True,  'شسقف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   19,   9, 30,   19);
        TestNext([],    b(12, 8, 0),  b( 9,14, 0),    9,12,   9,12,   True,  'شسق',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   19,  10, 30,   19);
        TestNext([],    b(12, 8, 0),  b(10,12, 0),   10,12,  10,12,   True,  'شس',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   19,  11, 30,   19);
        TestNext([],    b(12, 8, 0),  b(11,10, 0),   11,12,  11,12,   True,  'ش',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   19,  12, 30,   19);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   19,  13, 30,   19);
        TestNext([],    b(13,17, 0),  b(30,34, 0),   13,30,  13,30,   False, ' ',       clBlack);
        TestEnd;

    // Change highlight, so last char is diff color

        SynEdit.BlockBegin := Point(1,1);
        TestStart('Scan full line 18 / hl-token',   20,   1, 30,   20);
        TestNext([],    b( 1, 1, 0),  b( 7, 7, 0),    1, 7,   1, 7,   False, 'a     ',  clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(12, 8, 0),  b( 9,14, 0),    9,12,   9,12,   True,  'شسق',      clRed);
        TestNext([],    b( 9,14, 0),  b( 8,16, 0),    8, 9,   8, 9,   True,  'ف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   20,   2, 30,   20);
        TestNext([],    b( 2, 2, 0),  b( 7, 7, 0),    2, 7,   2, 7,   False, '     ',   clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(12, 8, 0),  b( 9,14, 0),    9,12,   9,12,   True,  'شسق',      clRed);
        TestNext([],    b( 9,14, 0),  b( 8,16, 0),    8, 9,   8, 9,   True,  'ف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   20,   3, 30,   20);
        TestNext([],    b( 3, 3, 0),  b( 7, 7, 0),    3, 7,   3, 7,   False, '    ',    clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(12, 8, 0),  b( 9,14, 0),    9,12,   9,12,   True,  'شسق',      clRed);
        TestNext([],    b( 9,14, 0),  b( 8,16, 0),    8, 9,   8, 9,   True,  'ف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   20,   4, 30,   20);
        TestNext([],    b( 4, 4, 0),  b( 7, 7, 0),    4, 7,   4, 7,   False, '   ',     clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(12, 8, 0),  b( 9,14, 0),    9,12,   9,12,   True,  'شسق',      clRed);
        TestNext([],    b( 9,14, 0),  b( 8,16, 0),    8, 9,   8, 9,   True,  'ف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   20,   5, 30,   20);
        TestNext([],    b( 5, 5, 0),  b( 7, 7, 0),    5, 7,   5, 7,   False, '  ',      clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(12, 8, 0),  b( 9,14, 0),    9,12,   9,12,   True,  'شسق',      clRed);
        TestNext([],    b( 9,14, 0),  b( 8,16, 0),    8, 9,   8, 9,   True,  'ف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   20,   6, 30,   20);
        TestNext([],    b( 6, 6, 0),  b( 7, 7, 0),    6, 7,   6, 7,   False, ' ',       clGreen);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(12, 8, 0),  b( 9,14, 0),    9,12,   9,12,   True,  'شسق',      clRed);
        TestNext([],    b( 9,14, 0),  b( 8,16, 0),    8, 9,   8, 9,   True,  'ف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   20,   7, 30,   20);
        TestNext([],    b( 7, 7, 0),  b( 8, 8, 0),    7, 8,   7, 8,   False, '''',      clYellow);
        TestNext([],    b(12, 8, 0),  b( 9,14, 0),    9,12,   9,12,   True,  'شسق',      clRed);
        TestNext([],    b( 9,14, 0),  b( 8,16, 0),    8, 9,   8, 9,   True,  'ف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   20,   8, 30,   20);
        TestNext([],    b(12, 8, 0),  b( 9,14, 0),    9,12,   9,12,   True,  'شسق',      clRed);
        TestNext([],    b( 9,14, 0),  b( 8,16, 0),    8, 9,   8, 9,   True,  'ف',      clBlue);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   20,   9, 30,   20);
        TestNext([],    b(12, 8, 0),  b( 9,14, 0),    9,12,   9,12,   True,  'شسق',      clRed);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   20,  10, 30,   20);
        TestNext([],    b(12, 8, 0),  b(10,12, 0),   10,12,  10,12,   True,  'شس',      clRed);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   20,  11, 30,   20);
        TestNext([],    b(12, 8, 0),  b(11,10, 0),   11,12,  11,12,   True,  'ش',      clRed);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   20,  12, 30,   20);
        TestNext([],    b(12,16, 0),  b(30,34, 0),   12,30,  12,30,   False, ' ',       clBlack);
        TestEnd;

        TestStart('Scan full line 18 / hl-token',   20,  13, 30,   20);
        TestNext([],    b(13,17, 0),  b(30,34, 0),   13,30,  13,30,   False, ' ',       clBlack);
        TestEnd;







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

