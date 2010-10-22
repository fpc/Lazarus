unit TestFoldedView;

(* TODO:
   - test without highlighter / not-folding-highlighter (CalculateMaps must still work)
   - Need a hook, to see which lines are invalidated

*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase, TestHighlightPas, Forms, LCLProc,
  SynEdit, SynHighlighterPas, SynEditFoldedView, SynEditHighlighterFoldBase,
  SynGutterCodeFolding;

type

  { TTestFoldedView }

  TTestFoldedView = class(TTestBaseHighlighterPas)
  private
    FoldedView: TSynEditFoldedView;
    DoAutoFoldDescTests: Boolean;
    DoAllowScrollPastEof: Boolean;
    EnableDebug: Boolean;

    procedure TestFoldedText(AName: String; ALines: Array of Integer);
    procedure SetLines(AText: Array of String); reintroduce;
  protected
    procedure SetUp; override;
    procedure ReCreateEdit; reintroduce;
    function TestText: TStringArray;
    function TestText2: TStringArray;
    function TestText3: TStringArray;
    function TestTextPasHl(AIfCol: Integer): TStringArray;
    function TestText4: TStringArray;
    function TestText5: TStringArray;
    function TestTextHide(ALen: Integer): TStringArray;
    function TestTextHide2(ALen: Integer): TStringArray;
    function TestTextHide3: TStringArray;
    function TestTextHide4: TStringArray;
  published
    procedure TestFold;
    procedure TestFoldStateFromText;
    procedure TestFoldStateDesc;
  end;

implementation

procedure TTestFoldedView.TestFoldedText(AName: String; ALines: array of Integer);
var
  ExpTxt: String;
  i: Integer;
  tmp: String;
  function GetFoldedText: String;
  var I: Integer;
  begin
    Result :=  '';
    for i := 0 to FoldedView.Count - 1 do  Result := Result + FoldedView[i] + LineEnding;
  end;
begin
  PushBaseName(AName);
  //if EnableDebug then FoldedView.debug;
  ExpTxt := '';
  for i := 0 to high(ALines) do ExpTxt := ExpTxt + SynEdit.Lines[ALines[i]] + LineEnding;
  TestCompareString('', ExpTxt, GetFoldedText);

  FoldedView.FixFoldingAtTextIndex(0, SynEdit.Lines.Count-1);
  TestCompareString('after FixFolding', ExpTxt, GetFoldedText);

  if DoAutoFoldDescTests then begin
    tmp := FoldedView.GetFoldDescription(0, 1, -1, -1, False, False);
    //debugln(MyDbg(tmp));
    FoldedView.UnfoldAll;
    FoldedView.ApplyFoldDescription(0,1,-1,-1, PChar(tmp), length(tmp), False);
    TestCompareString('Restore FoldDesc (NOT AsText, Not Ext)', ExpTxt, GetFoldedText);

    tmp := FoldedView.GetFoldDescription(0, 1, -1, -1, False, True);
    //debugln(MyDbg(tmp));
    FoldedView.UnfoldAll;
    FoldedView.ApplyFoldDescription(0,1,-1,-1, PChar(tmp), length(tmp), False);
    TestCompareString('Restore FoldDesc (NOT AsText, Ext)', ExpTxt, GetFoldedText);

    tmp := FoldedView.GetFoldDescription(0, 1, -1, -1, True, False);
    //debugln(MyDbg(tmp));
    FoldedView.UnfoldAll;
    FoldedView.ApplyFoldDescription(0,1,-1,-1, PChar(tmp), length(tmp), True);
    TestCompareString('Restore FoldDesc (AsText, Not Ext)', ExpTxt, GetFoldedText);

    tmp := FoldedView.GetFoldDescription(0, 1, -1, -1, True, True);
    //debugln(MyDbg(tmp));
    FoldedView.UnfoldAll;
    FoldedView.ApplyFoldDescription(0,1,-1,-1, PChar(tmp), length(tmp), True);
    TestCompareString('Restore FoldDesc (AsText, Ext)', ExpTxt, GetFoldedText);
  end;
  PopBaseName;
end;

procedure TTestFoldedView.SetLines(AText: Array of String);
begin
  inherited SetLines(AText);
  FoldedView.TopLine := 1;
  FoldedView.LinesInWindow := Length(AText) + 2;
end;

procedure TTestFoldedView.SetUp;
begin
  DoAllowScrollPastEof := False;
  DoAutoFoldDescTests := False;
  inherited SetUp;
end;

procedure TTestFoldedView.ReCreateEdit;
begin
  inherited ReCreateEdit;
  FoldedView := SynEdit.TextView;
  if DoAllowScrollPastEof then SynEdit.Options := SynEdit.Options + [eoScrollPastEof];
  EnableFolds([cfbtBeginEnd.. cfbtNone], [cfbtSlashComment]);
end;

function TTestFoldedView.TestText: TStringArray;
begin
  SetLength(Result, 6);
  Result[0] := 'program Foo;';
  Result[1] := 'procedure a;';
  Result[2] := 'begin';
  Result[3] := 'writeln()';
  Result[4] := 'end;';
  Result[5] := '';
end;

function TTestFoldedView.TestText2: TStringArray;
begin
  SetLength(Result, 13);
  Result[0] := 'program Foo;';
  Result[1] := 'procedure a; procedure b;';          // 2 folds open on one line
  Result[2] := '  begin';
  Result[3] := '  end;';
  Result[4] := '{%region} {%endregion} begin';       // begin end on same line (in front of 2nd begin
  Result[5] := '  if b then begin';
  Result[6] := '    writeln(1)';
  Result[7] := '  end else begin';                   // close/open line
  Result[8] := '    writeln(2)';
  Result[9] := '  if c then begin x; end;   end;';   // begin end on same line (in front of 2nd end
  Result[10]:= 'end;';
  Result[11]:= '{$note}';
  Result[12]:= '';
end;

function TTestFoldedView.TestText3: TStringArray;
begin
  SetLength(Result, 13);
  Result[0] := 'program Foo;';
  Result[1] := '{$IFDEF x}';
  Result[2] := 'procedure a;';
  Result[3] := '{$ENDIF}';                      // overlapping
  Result[4] := 'begin';
  Result[5] := '{%region}  if a then begin';
  Result[6] := '    writeln(1)';
  Result[7] := '{%endregion}  end else begin';  //semi-overlapping: endregion, hides start-line of "else begin"
  Result[8] := '    writeln(2)';
  Result[9] := '  end';
  Result[10]:= 'end;';
  Result[11]:= '//';
  Result[12]:= '';
end;

function TTestFoldedView.TestTextPasHl(AIfCol: Integer): TStringArray;
begin
  // various mixed of pascal and ifdef blocks => actually a test for pascal highlighter
  SetLength(Result, 8);
  Result[0] := 'program p;';
  Result[1] := 'procedure A;';
  case AIfCol of
    0: Result[2] := '{$IFDEF} begin  if a then begin';
    1: Result[2] := 'begin {$IFDEF} if a then begin';
    2: Result[2] := 'begin  if a then begin {$IFDEF}';
  end;
  Result[3] := '  end; // 2';
  Result[4] := 'end; // 1';
  Result[5] := '{$ENDIF}';
  Result[6] := '//';
  Result[7] := '';
end;

function TTestFoldedView.TestText4: TStringArray;
begin
  SetLength(Result, 8);
  Result[0] := 'program Foo; procedure a; begin';
  Result[1] := 'if a then begin';
  Result[2] := 'end;';
  Result[3] := 'if b then begin'; // consecutive begin-end
  Result[4] := 'end;';
  Result[5] := 'if b then begin'; // consecutive begin-end
  Result[6] := 'end;';
  Result[7] := '';
end;

function TTestFoldedView.TestText5: TStringArray;
begin
  SetLength(Result, 5000);
  Result[0] := 'program Foo; procedure a; begin';
  Result[1] := 'if a then begin';
  Result[2] := 'end;';
  // for fold-desc text, this should force a new ' p' node
  Result[4900] := 'if b then begin'; // consecutive begin-end => long lines down
  Result[4901] := 'end;';
  Result[7] := '';
end;

function TTestFoldedView.TestTextHide(ALen: Integer): TStringArray;
begin
  SetLength(Result, 3+ALen);
  Result[0] := 'program Foo;';

  Result[1+ALen] := 'begin end';
  Result[2+ALen] := '';

  while ALen > 0 do begin
    Result[ALen] := '//'+IntToStr(ALen);
    dec(ALen);
  end;
end;

function TTestFoldedView.TestTextHide2(ALen: Integer): TStringArray;
begin
  SetLength(Result, 2+ALen);
  Result[ALen] := 'program foo;';
  Result[1+ALen] := '';
  while ALen > 0 do begin;
    Result[ALen-1] := '// '+IntToStr(ALen); // hide first line
    dec(ALen);
  end;
end;

function TTestFoldedView.TestTextHide3: TStringArray;
begin
  SetLength(Result, 3);
  Result[0] := '// test'; // hide ALL
  Result[1] := '// FOO';
  Result[2] := '';
end;

function TTestFoldedView.TestTextHide4: TStringArray;
begin
  SetLength(Result, 13);
  Result[0] := '{ABC}'; // hide individual blocks, following each other
  Result[1] := '{def}';
  Result[2] := '{XYZ}';
  Result[3] := '{foo}';
  Result[4] := '//abc';
  Result[5] := '{foo';
  Result[6] := '}';
  Result[7] := '{bar';
  Result[8] := '-';
  Result[9] := '}';
  Result[10]:= '{foo';
  Result[11]:= '}';
  Result[12]:= '';

end;


procedure TTestFoldedView.TestFold;

  procedure TstSetText(AName: String; AText: TStringArray);
  begin
    PopBaseName;
    ReCreateEdit;
    SetLines(AText);
    PushBaseName(AName);
  end;

  procedure TstFold(AName: String; AFoldAtIndex: integer; AExpectedLines: Array of Integer);
  begin
    FoldedView.FoldAtTextIndex(AFoldAtIndex);
    TestFoldedText(AName, AExpectedLines);
  end;
  procedure TstFold(AName: String; AFoldAtIndex, AFoldAtColum: integer; AExpectedLines: Array of Integer);
  begin
    FoldedView.FoldAtTextIndex(AFoldAtIndex, AFoldAtColum);
    TestFoldedText(AName, AExpectedLines);
  end;
  procedure TstFold(AName: String; AFoldAtIndex, AFoldAtColum, AFoldAtColCnt: integer;
    AExpectedLines: Array of Integer);
  begin
    FoldedView.FoldAtTextIndex(AFoldAtIndex, AFoldAtColum, AFoldAtColCnt);
    TestFoldedText(AName, AExpectedLines);
  end;
  procedure TstFold(AName: String; AFoldAtIndex, AFoldAtColum, AFoldAtColCnt: integer;
    AFoldAtSkip: Boolean; AExpectedLines: Array of Integer);
  begin
    FoldedView.FoldAtTextIndex(AFoldAtIndex, AFoldAtColum, AFoldAtColCnt, AFoldAtSkip);
    TestFoldedText(AName, AExpectedLines);
  end;
  procedure TstFold(AName: String; AFoldAtIndex, AFoldAtColum, AFoldAtColCnt: integer;
    AFoldAtSkip: Boolean; AVisibleLines: Integer; AExpectedLines: Array of Integer);
  begin
    FoldedView.FoldAtTextIndex(AFoldAtIndex, AFoldAtColum, AFoldAtColCnt, AFoldAtSkip, AVisibleLines);
    TestFoldedText(AName, AExpectedLines);
  end;

  procedure TstUnFoldAtCaret(AName: String; X, Y: integer; AExpectedLines: Array of Integer);
  begin
    SynEdit.CaretXY := Point(X, Y);
    TestFoldedText('UnfoldCaret - '+AName, AExpectedLines);
  end;

  // ViewPos is 1 based
  procedure TstTxtIndexToViewPos(AName: String; AExpectedPairs: Array of Integer; ADoReverse: Boolean = false);
  var i: Integer;
  begin
    i := 0;
    while i < high(AExpectedPairs)-1 do begin
      AssertEquals(AName+' TxtIdx('+IntToStr( AExpectedPairs[i])+') to ViewPos[1-based]('+IntToStr( AExpectedPairs[i+1])+') ',
                   AExpectedPairs[i+1], FoldedView.TextIndexToViewPos(AExpectedPairs[i]));
      if ADoReverse then
        AssertEquals(AName+' ViewPos[1-based]('+IntToStr( AExpectedPairs[i+1])+') to TxtIdx('+IntToStr( AExpectedPairs[i])+') [R]',
                   AExpectedPairs[i], FoldedView.ViewPosToTextIndex(AExpectedPairs[i+1]));
      inc(i, 2);
    end;
  end;
  // ViewPos is 1 based // Reverse of the above
  procedure TstViewPosToTextIndex(AName: String; AExpectedPairs: Array of Integer; ADoReverse: Boolean = false);
  var i: Integer;
  begin
    i := 0;
    while i < high(AExpectedPairs)-1 do begin
      AssertEquals(AName+' ViewPos[1-based]('+IntToStr( AExpectedPairs[i])+') to TxtIdx('+IntToStr( AExpectedPairs[i+1])+')',
                   AExpectedPairs[i+1], FoldedView.ViewPosToTextIndex(AExpectedPairs[i]));
      if ADoReverse then
        AssertEquals(AName+' TxtIdx('+IntToStr( AExpectedPairs[i+1])+') to ViewPos[1-based]('+IntToStr( AExpectedPairs[i])+') [R]',
                   AExpectedPairs[i], FoldedView.TextIndexToViewPos(AExpectedPairs[i+1]));
      inc(i, 2);
    end;
  end;

  // ScreenLine is 0 based
  procedure TstTextIndexToScreenLine(AName: String; AExpectedPairs: Array of Integer; ADoReverse: Boolean = false);
  var i: Integer;
  begin
    i := 0;
    while i < high(AExpectedPairs)-1 do begin
      AssertEquals(AName+' TxtIdx('+IntToStr( AExpectedPairs[i])+') to ScreenLine[0-based]('+IntToStr( AExpectedPairs[i+1])+') ',
                   AExpectedPairs[i+1], FoldedView.TextIndexToScreenLine(AExpectedPairs[i]));
      if ADoReverse then
        AssertEquals(AName+' ScreenLine[0-based]('+IntToStr( AExpectedPairs[i+1])+') to TxtIdx('+IntToStr( AExpectedPairs[i])+') [R]',
                   AExpectedPairs[i], FoldedView.ScreenLineToTextIndex(AExpectedPairs[i+1]));
      inc(i, 2);
    end;
  end;
  // ScreenLine is 0 based // Reverse of the above
  procedure TstScreenLineToTextIndex(AName: String; AExpectedPairs: Array of Integer; ADoReverse: Boolean = false);
  var i: Integer;
  begin
    i := 0;
    while i < high(AExpectedPairs)-1 do begin
      AssertEquals(AName+' ScreenLine[0-based]('+IntToStr( AExpectedPairs[i])+') to TxtIdx('+IntToStr( AExpectedPairs[i+1])+') ',
                   AExpectedPairs[i+1], FoldedView.ScreenLineToTextIndex(AExpectedPairs[i]));
      if ADoReverse then
        AssertEquals(AName+' TxtIdx('+IntToStr( AExpectedPairs[i+1])+') to ScreenLine[0-based]('+IntToStr( AExpectedPairs[i])+') [R]',
                   AExpectedPairs[i], FoldedView.TextIndexToScreenLine(AExpectedPairs[i+1]));
      inc(i, 2);
    end;
  end;

  procedure RunTest;
  begin
    PushBaseName('');

    {%region 'simple folds'}
    TstSetText('Prg Prc Beg (1)', TestText);
     TstFold('fold Prg', 0, [0]);
     //IsFoldedAtTextIndex

    TstSetText('Prg Prc Beg (2)', TestText);
     TstFold('fold Prc', 1, [0, 1]);
     TstFold('fold Prg', 0, [0]);

    TstSetText('Prg Prc Beg (3)', TestText);
     TstFold('fold Beg', 2, [0, 1, 2]);
     TstFold('fold Prg', 0, [0]);

    TstSetText('Prg Prc Beg (4)', TestText);
     TstFold('fold Beg', 2, [0, 1, 2]);
     TstFold('fold Prc', 1, [0, 1]);
     TstFold('fold Prg', 0, [0]);
    {%endregion}

    {%region 'lines with many folds starting/ending'}
    // Fold at column (positive ColIndex)
    TstSetText('Text2 (a)', TestText2);
     TstFold('fold PrcB (col 1)', 1, 1, [0, 1, 4, 5, 6, 7, 8, 9, 10, 11]);
     TstFold('fold PrcA (col 0)', 1, 0, [0, 1, 11]);

    // Fold at column (negative ColIndex)
    TstSetText('Text2 (b)', TestText2);
     TstFold('fold PrcB (col -1)', 1, -1, [0, 1, 4, 5, 6, 7, 8, 9, 10, 11]);
     TstFold('again PrcB (col -1, NO skip)', 1, -1, [0, 1, 4, 5, 6, 7, 8, 9, 10, 11]);
     TstFold('fold PrcA (col -1, skip)', 1, -1, 1, True, [0, 1, 11]);

    // Fold at column, Cnt=2 (negative ColIndex)
    TstSetText('Text2 (b)', TestText2);
     TstFold('fold Prc (col -1, Cnt=2)', 1, -1, 2, True, [0, 1, 11]);

    // Fold at column, after same line open/close (positive ColIndex)
    TstSetText('Text2 (c)', TestText2);
     TstFold('fold BegB (col 0)', 4, 0, [0, 1, 2, 3, 4, 11]);
     //DebugLn(MyDbg(SynEdit.FoldState));

    // Fold at column, after same line open/close (negative ColIndex)
    TstSetText('Text2 (d)', TestText2);
     TstFold('fold BegB (col -1)', 4, -1, [0, 1, 2, 3, 4, 11]);

    // Fold block with end on re-opening line
    TstSetText('Text2 (e)', TestText2);
     TstFold('fold Beg', 5, [0, 1, 2, 3, 4, 5, 7, 8, 9, 10, 11]);
    {%endregion}

    {%region 'Overlaps / Semi overlaps'}
    TstSetText('Text3 (a)', TestText3);
     TstFold('fold ifdef', 1, [0, 1, 4, 5, 6, 7, 8, 9, 10, 11]);
     DebugLn(MyDbg(SynEdit.FoldState));
     TstFold('fold Beg',   4, [0, 1, 4, 11]);
     DebugLn(MyDbg(SynEdit.FoldState));

    TstSetText('Text3 (b, overlap)', TestText3);
     TstFold('fold Prc',   2, [0, 1, 2, 11]);
     DebugLn(MyDbg(SynEdit.FoldState));
     TstFold('fold ifdef', 1, [0, 1, 11]);
     DebugLn(MyDbg(SynEdit.FoldState));

    TstSetText('Text3 (c, NO semi-overlap)', TestText3);
     TstFold('fold Else',  7, [0, 1, 2, 3, 4, 5, 6, 7, 10, 11]);
     DebugLn(MyDbg(SynEdit.FoldState));
     TstFold('fold Beg',   5,1, [0, 1, 2, 3, 4, 5, 7, 10, 11]);
     DebugLn(MyDbg(SynEdit.FoldState));

    TstSetText('Text3 (d, semi-overlap)', TestText3);
     TstFold('fold Else',   7, [0, 1, 2, 3, 4, 5, 6, 7, 10, 11]);
     DebugLn(MyDbg(SynEdit.FoldState));
     TstFold('fold Region', 5,0, [0, 1, 2, 3, 4, 5, 10, 11]);
     DebugLn(MyDbg(SynEdit.FoldState));
     TstTxtIndexToViewPos('', [0,1,  1,2,  2,3,  3,4,  4,5,  5,6,  10,7,  11,8], True);

     TstUnFoldAtCaret('Unfold Else', 1,8+1, [0, 1, 2, 3, 4, 5, 8, 9, 10, 11]);

    TstSetText('Text3 (e, semi-overlap)', TestText3);
     TstFold('fold Else',   7, [0, 1, 2, 3, 4, 5, 6, 7, 10, 11]);
     TstFold('fold Region', 5,0, [0, 1, 2, 3, 4, 5, 10, 11]);
     TstUnFoldAtCaret('Unfold Else 2', 1,9+1, [0, 1, 2, 3, 4, 5, 8, 9, 10, 11]);

    TstSetText('Text3 (f, semi-overlap)', TestText3);
     TstFold('fold Else',   7, [0, 1, 2, 3, 4, 5, 6, 7, 10, 11]);
     TstFold('fold Region', 5,0, [0, 1, 2, 3, 4, 5, 10, 11]);
     TstUnFoldAtCaret('Unfold Region', 1,6+1, [0, 1, 2, 3, 4, 5, 6, 7, 10, 11]);

    TstSetText('Text3 (g, semi-overlap)', TestText3);
     TstFold('fold Else',   7, [0, 1, 2, 3, 4, 5, 6, 7, 10, 11]);
     TstFold('fold Region', 5,0, [0, 1, 2, 3, 4, 5, 10, 11]);
     TstUnFoldAtCaret('Unfold Region 2', 1,7+1, [0, 1, 2, 3, 4, 5, 6, 7, 10, 11]);
    {%endregion}

    {%region 'Mixed pascal and ifdef in opening line'}
    TstSetText('Text4 PasIfDef (0)', TestTextPasHl(0));
     TstFold('fold IfDef(0)', 2, 0, [0, 1, 2, 6]);
    TstSetText('Text4 PasIfDef (0)', TestTextPasHl(0));
     TstFold('fold Begin(1)', 2, 1, [0, 1, 2, 5, 6]);
    TstSetText('Text4 PasIfDef (0)', TestTextPasHl(0));
     TstFold('fold Begin(2)', 2, 2, [0, 1, 2, 4, 5, 6]);

    TstSetText('Text4 PasIfDef (1)', TestTextPasHl(1));
     TstFold('fold Begin(0)', 2, 0, [0, 1, 2, 5, 6]);
    TstSetText('Text4 PasIfDef (1)', TestTextPasHl(1));
     TstFold('fold Ifdef(1)', 2, 1, [0, 1, 2, 6]);
    TstSetText('Text4 PasIfDef (1)', TestTextPasHl(1));
     TstFold('fold Begin(2)', 2, 2, [0, 1, 2, 4, 5, 6]);

    TstSetText('Text4 PasIfDef (2)', TestTextPasHl(2));
     TstFold('fold Begin(0)', 2, 0, [0, 1, 2, 5, 6]);
    TstSetText('Text4 PasIfDef (2)', TestTextPasHl(2));
     TstFold('fold Begin(1)', 2, 1, [0, 1, 2, 4, 5, 6]);
    TstSetText('Text4 PasIfDef (2)', TestTextPasHl(2));
     TstFold('fold IfDef(2)', 2, 2, [0, 1, 2, 6]);
    {%endregion}

    {%region 'Hide'}
      {%region 'Hide in middle of source'}
      TstSetText('Text5 Hide 1', TestTextHide(1));
        TstFold('fold //)', 1, 0, 1, False, 0, [0, 2]);

      TstSetText('Text5 Hide 2', TestTextHide(2));
        TstFold('fold //(2)', 1, 0, 1, False, 0, [0, 3]);

      TstSetText('Text5 Hide 3', TestTextHide(3));
        TstFold('fold //(3)', 1, 0, 1, False, 0, [0, 4]);
        TstTxtIndexToViewPos    ('', [0,1,  4,2 ], True); // 0-base => 1 base
        TstTextIndexToScreenLine('', [0,0,  4,1 ], True); // 0-base => 0 base
        TstScreenLineToTextIndex('', [-1,-1,  5,-1 ]);    // 0-base => 0-base
       SynEdit.Options := SynEdit.Options + [eoScrollPastEof];
       SynEdit.TopLine := 5; // now the visible TxtIdx=0 line, is just before the screen [-1]
        AssertEquals('FoldView.topline', 2, FoldedView.TopLine);
        TstTextIndexToScreenLine('TopLine=2', [0,-1,  4,0 ], True); // 0-base => 0 base
        TstScreenLineToTextIndex('TopLine=2', [-1,0,  4,-1 ]);    // 0-base => 0-base

      {%endregion}

      {%region 'Hide at very begin of source'}
      ReCreateEdit;
      TstSetText('Text6 Hide 1st line', TestTextHide2(1)); // *** one line at the top
        TstFold('fold //)', 0, 0, 1, False, 0, [1]);
        AssertEquals('FoldedView.TextIndex 0', 1, FoldedView.TextIndex[0]);
        AssertEquals('FoldedView.TextIndex -1', -1, FoldedView.TextIndex[-1]);

        TstTxtIndexToViewPos    ('', [1,1 ], True); // 0-base => 1 base
        TstTextIndexToScreenLine('', [1,0 ], True); // 0-base => 0 base
        TstScreenLineToTextIndex('', [-1,-1,  0,1,  1,-1 ]); // 0-base => 0-base

        AssertEquals('FoldedView.FoldedAtTextIndex 0', True,  FoldedView.FoldedAtTextIndex[0]);
        AssertEquals('FoldedView.FoldedAtTextIndex 1', False, FoldedView.FoldedAtTextIndex[1]);
//TODO check FoldedView.FoldType;
      {%endregion}

      {%region 'Hide full text'}
      TstSetText('Hide all', TestTextHide3);
        TstFold('fold //)', 0, 0, 1, False, 0, []);
      {%endregion}

      {%region 'Hide, none-foldable'}
      TstSetText('Hide //', TestTextHide(2));
      EnableFolds([cfbtBeginEnd..cfbtNone], [cfbtBorCommand, cfbtAnsiComment, cfbtSlashComment], [cfbtBeginEnd..cfbtNone]);
        TstFold('fold //)', 1, 0, 1, False, 0, [0, 3]);
      TstSetText('Hide {} one line', TestTextHide4);
      EnableFolds([cfbtBeginEnd..cfbtNone], [cfbtBorCommand, cfbtAnsiComment, cfbtSlashComment], [cfbtBeginEnd..cfbtNone]);
        TstFold('fold {})', 3, 0, 1, False, 0, [0, 1, 2,   4, 5, 6, 7, 8, 9, 10, 11]);
      TstSetText('Hide {} multi line', TestTextHide4);
      EnableFolds([cfbtBeginEnd..cfbtNone], [cfbtBorCommand, cfbtAnsiComment, cfbtSlashComment], [cfbtBeginEnd..cfbtNone]);
        TstFold('fold {})', 7, 0, 1, False, 0, [0, 1, 2, 3, 4, 5, 6,   10, 11]);
      {%endregion}

      {%region 'Hide consecutive individual folds'}
      TstSetText('Hide consecutive', TestTextHide4);
      EnableFolds([cfbtBeginEnd..cfbtNone], [cfbtBorCommand, cfbtAnsiComment, cfbtSlashComment]);
        TstFold('fold 3)', 3, 0, 1, False, 0, [0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11]);
        TstFold('fold 2)', 2, 0, 1, False, 0, [0, 1, 4, 5, 6, 7, 8, 9, 10, 11]);
        TstFold('fold 1)', 1, 0, 1, False, 0, [0, 4, 5, 6, 7, 8, 9, 10, 11]);
      {%endregion}

    {%endregion}

    {%region}
    // consecutive begin-end
    // for text desc
    TstSetText('Text4 consecutive begin (all)', TestText4);
     TstFold('fold 1st', 1, [0, 1, 3, 4, 5, 6]);
     TstFold('fold 2nd', 3, [0, 1, 3, 5, 6]);
     TstFold('fold 3rd', 5, [0, 1, 3, 5]);

    TstSetText('Text4 consecutive begin (1,3)', TestText4);
     TstFold('fold 1st', 1, [0, 1, 3, 4, 5, 6]);
     TstFold('fold 3rd', 5, [0, 1, 3, 4, 5]);

    TstSetText('Text4 consecutive begin (2,3)', TestText4);
     TstFold('fold 2nd', 3, [0, 1, 2, 3, 5, 6]);
     TstFold('fold 3rd', 5, [0, 1, 2, 3, 5]);

   TstSetText('Text5 consecutive begin (long distance)', TestText5);
    AssertEquals(FoldedView.Count, 4999);
    FoldedView.FoldAtTextIndex(1);
    FoldedView.FoldAtTextIndex(4900);
    AssertEquals(FoldedView.Count, 4999-2);
  {%endregion}

  end;

begin
  DoAllowScrollPastEof := False;
  DoAutoFoldDescTests := False;
  RunTest;

  DoAutoFoldDescTests:= True;
  RunTest;

  DoAllowScrollPastEof := True;
  RunTest;
end;

procedure TTestFoldedView.TestFoldStateFromText;
  procedure TstSetText(AName: String; AText: TStringArray);
  begin
    PopBaseName;
    ReCreateEdit;
    SetLines(AText);
    PushBaseName(AName);
  end;

  procedure TstFoldState(AName, AFoldDesc: String; AExpectedLines: Array of Integer);
  begin
    // Use to test text-desc as stored in IDE xml session files
    FoldedView.UnfoldAll;
    SynEdit.FoldState := AFoldDesc;
    TestFoldedText('FoldState - '+AName, AExpectedLines);
  end;

begin
  DoAutoFoldDescTests := False;

  TstSetText('Prg Prc Beg (1)', TestText);
   TstFoldState('fold Prg', ' TA004,',             [0]); // from ide session xml

  TstSetText('Prg Prc Beg (2)', TestText);
   TstFoldState('fold Prc', ' T3103M',             [0, 1]); // from ide session xml
   TstFoldState('fold Prg', ' TA004 T3103#',       [0]); // from ide session xml

  TstSetText('Prg Prc Beg (3)', TestText);
   TstFoldState('fold Beg', ' T1202E',             [0, 1, 2]); // from ide session xml
   TstFoldState('fold Prg', ' TA004 T12025',       [0]); // from ide session xml

  TstSetText('Prg Prc Beg (4)', TestText);
   TstFoldState('fold Beg', ' T1202E',             [0, 1, 2]); // from ide session xml
   TstFoldState('fold Prc', ' T3103 T1202{',       [0,1]); // from ide session xml
   TstFoldState('fold Prg', ' TA004 T3103 T1202Y', [0]); // from ide session xml

  TstSetText('Text2 (a)', TestText2);
   TstFoldState('fold PrcB (col 1)',   ' T31D21',  [0, 1, 4, 5, 6, 7, 8, 9, 10, 11]);
   TstFoldState('fold PrcA+B (col 0)', ' T31091a', [0, 1, 11]);
   TstFoldState('fold BegB (col 0)',   ' T14N69',  [0, 1, 2, 3, 4, 11]);
   TstFoldState('fold Beg',            ' T05C1''', [0, 1, 2, 3, 4, 5, 7, 8, 9, 10, 11]);

  TstSetText('Text3 (a)', TestText3);
   TstFoldState('fold ifdef', ' TI122;',       [0, 1, 4, 5, 6, 7, 8, 9, 10, 11]);
   TstFoldState('fold Beg',   ' TI122 T1406e', [0, 1, 4, 11]);

  TstSetText('Text3 (b, overlap)', TestText3);
   TstFoldState('fold Prc',   ' T3208Y',       [0, 1, 2, 11]);
//   TstFoldState('fold ifdef', ' TI129 T3208$', [0, 1, 11]); // TODO

  TstSetText('Text3 (c, NO semi-overlap)', TestText3);
   TstFoldState('fold Else',  ' T07N2O', [0, 1, 2, 3, 4, 5, 6, 7, 10, 11]);
   TstFoldState('fold Beg',   ' T05L11K', [0, 1, 2, 3, 4, 5, 7, 10, 11]);

  TstSetText('Text2 (a) without region', TestText2);
   PasHighLighter.FoldConfig[ord(cfbtRegion)].Enabled := False;
   TstFoldState('fold PrcB (col 1)',   ' T31D21',  [0, 1, 4, 5, 6, 7, 8, 9, 10, 11]);
   TstFoldState('fold PrcA+B (col 0)', ' T31091a', [0, 1, 11]);
   TstFoldState('fold BegB (col 0)',   ' T14N69',  [0, 1, 2, 3, 4, 11]);
   TstFoldState('fold Beg',            ' T05C1''', [0, 1, 2, 3, 4, 5, 7, 8, 9, 10, 11]);

  TstSetText('Text4 consecutive begin (all)', TestText4);
   TstFoldState('fold 3rd', ' T01A12q', [0, 1, 3, 5]);
     DebugLn(MyDbg(SynEdit.FoldState));

  TstSetText('Text4 consecutive begin (1,3)', TestText4);
   TstFoldState('fold 3rd', ' T01A1011p', [0, 1, 3, 4, 5]);
     DebugLn(MyDbg(SynEdit.FoldState));

  TstSetText('Text4 consecutive begin (2,3)', TestText4);
   TstFoldState('fold 3rd', ' T03A11r', [0, 1, 2, 3, 5]);
     DebugLn(MyDbg(SynEdit.FoldState));

  TstSetText('Text5 consecutive begin (long distance)', TestText5);
  AssertEquals(FoldedView.Count, 4999);
  SynEdit.FoldState := ' T01A1 p0j*eA1i';
  AssertEquals(FoldedView.Count, 4999-2);
end;

procedure TTestFoldedView.TestFoldStateDesc;
begin
  (* - The values returned by GetFoldDescription can change in future versions
       Therefore there is only a limited number of tests.
       Test should only ensure that there are different results depending on text/extended flags

     - If they do for the not-extended-text, then new results should be added to TestFoldStateFromText
       (as ide will save new results / old result must still be supported for reading)
   *)
  ReCreateEdit;
  SetLines(TestText);
  FoldedView.FoldAtLine(2);
  FoldedView.FoldAtLine(0);
  //DebugLn(MyDbg(FoldedView.GetFoldDescription(0,1,-1,-1, False, False)));
  //DebugLn(MyDbg(FoldedView.GetFoldDescription(0,1,-1,-1, True,  False)));
  //DebugLn(MyDbg(FoldedView.GetFoldDescription(0,1,-1,-1, False, True)));
  //DebugLn(MyDbg(FoldedView.GetFoldDescription(0,1,-1,-1, True,  True)));
  TestCompareString('FoldDesc (NOT txt / NOT ext)',
                    #$00#$00#$00#$00#$00#$00#$00#$00#$07#$00#$00#$00#$04#$00#$00#$00#$04#$00#$00#$00#$04#$00#$00#$00#$0A#$00#$00#$00#$04#$00#$00#$00#$02#$00#$00#$00#$00#$00#$00#$00#$05#$00#$00#$00#$04#$00#$00#$00#$00#$00#$00#$00#$03#$00#$00#$00#$01#$00#$00#$00#$02#$00#$00#$00,
                    FoldedView.GetFoldDescription(0,1,-1,-1, False, False)
                   );
  TestCompareString('FoldDesc (txt / NOT ext)', ' TA004 T12025',
                    FoldedView.GetFoldDescription(0,1,-1,-1, True,  False)
                   );
  // TODO: Extended is not yet implemented
  //TestCompareString('FoldDesc (NOT txt / ext)',
  //                  #$00#$00#$00#$00#$00#$00#$00#$00#$07#$00#$00#$00#$04#$00#$00#$00#$00#$00#$00#$00#$03#$00#$00#$00#$0A#$00#$00#$00#$02#$00#$00#$00#$00#$00#$00#$00#$05#$00#$00#$00#$04#$00#$00#$00#$00#$00#$00#$00#$03#$00#$00#$00#$01#$00#$00#$00,
  //                  FoldedView.GetFoldDescription(0,1,-1,-1, False, True)
  //                 );
  //TestCompareString('FoldDesc (txt / ext)', ' TA004 T12025',
  //                  FoldedView.GetFoldDescription(0,1,-1,-1, True,  True)
  //                 );
end;

initialization

  RegisterTest(TTestFoldedView); 
end.

