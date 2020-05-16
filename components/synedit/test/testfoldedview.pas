unit TestFoldedView;

(* TODO:
   - test without highlighter / not-folding-highlighter (CalculateMaps must still work)
   - Need a hook, to see which lines are invalidated

*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, testregistry, TestBase, TestHighlightPas, Forms,
  LCLProc, SynEdit, SynHighlighterPas, SynEditFoldedView,
  SynEditHighlighterFoldBase, SynGutterCodeFolding, SynEditKeyCmds,
  SynEditTypes, SynEditMiscProcs;

type

  { TTestFoldedView }

  TTestFoldedView = class(TTestBaseHighlighterPas)
  private
    FoldedView: TSynEditFoldedView;
    DoAutoFoldDescTests, DoAutoFoldDescTestsReadOnly: Boolean;
    DoAllowScrollPastEof: Boolean;
    //EnableDebug: Boolean;

    // fold nest list
    PrepareLine, PrepareMax: integer;


    procedure TestFoldedText(AName: String; ALines: Array of Integer);
    procedure SetLines(AText: Array of String); reintroduce;
  protected
    procedure SetUp; override;
    procedure ReCreateEdit; reintroduce;
    function TestText: TStringArray;
    function TestText1: TStringArray;
    function TestText2: TStringArray;
    function TestText3: TStringArray;
    function TestTextPasHl(AIfCol: Integer): TStringArray;
    function TestText4: TStringArray;
    function TestText5: TStringArray;
    function TestText6: TStringArray;
    function TestText7: TStringArray;
    function TestText8: TStringArray;
    function TestText9: TStringArray;
    function TestText10: TStringArray;
    function TestText11: TStringArray;
    function TestText12: TStringArray;
    function TestTextHide(ALen: Integer): TStringArray;
    function TestTextHide2(ALen: Integer): TStringArray;
    function TestTextHide3: TStringArray;
    function TestTextHide4: TStringArray;
    function TestTextPlain: TStringArray;
    function TestTextBug21473: TStringArray;
  protected
    procedure TstSetText(AName: String; AText: Array of String);
    procedure TstFold(AName: String; AFoldAtIndex: integer; AExpectedLines: Array of Integer);
    procedure TstFold(AName: String; AFoldAtIndex, AFoldAtColum: integer; AExpectedLines: Array of Integer);
    procedure TstFold(AName: String; AFoldAtIndex, AFoldAtColum, AFoldAtColCnt: integer;
      AExpectedLines: Array of Integer);
    procedure TstFold(AName: String; AFoldAtIndex, AFoldAtColum, AFoldAtColCnt: integer;
      AFoldAtSkip: Boolean; AExpectedLines: Array of Integer);
    procedure TstFold(AName: String; AFoldAtIndex, AFoldAtColum, AFoldAtColCnt: integer;
      AFoldAtSkip: Boolean; AVisibleLines: Integer; AExpectedLines: Array of Integer);
    procedure TstUnFold(AName: String; AFoldAtIndex, AFoldAtColum, AFoldAtColCnt: integer;
      AFoldAtSkip: Boolean; AVisibleLines: Integer; AExpectedLines: Array of Integer);
    procedure TstUnFoldAtCaret(AName: String; X, Y: integer; AExpectedLines: Array of Integer);
    procedure TstTxtIndexToViewPos(AName: String; AExpectedPairs: Array of Integer; ADoReverse: Boolean = false);
    procedure TstViewPosToTextIndex(AName: String; AExpectedPairs: Array of Integer; ADoReverse: Boolean = false);
    procedure TstTextIndexToScreenLine(AName: String; AExpectedPairs: Array of Integer; ADoReverse: Boolean = false);
    procedure TstScreenLineToTextIndex(AName: String; AExpectedPairs: Array of Integer; ADoReverse: Boolean = false);

    // fold nest list
    Procedure CheckNode(nd: TSynFoldNodeInfo; ALine: TLineIdx; AColumn: integer;
      LogXStart, LogXEnd,  FoldLvlStart, FoldLvlEnd,  NestLvlStart, NestLvlEnd: Integer;
      FoldType: integer;  FoldTypeCompatible: integer; FoldGroup: Integer;
      FoldAction: TSynFoldActions);
    Procedure CheckNodeLines(AList: TLazSynEditNestedFoldsList; ALines: array of integer);
    Procedure CheckNodeEndLines(AList: TLazSynEditNestedFoldsList; ALines: array of integer);
    procedure InitList(const AName: String; AList: TLazSynEditNestedFoldsList;
      ALine, AGroup: Integer; AFlags: TSynFoldBlockFilterFlags;
      AInclOpening: Boolean; AClear: Boolean = True);
  published
    procedure TestFold;
    procedure TestFoldEdit;
    procedure TestFoldStateFromText;
    procedure TestFoldStateDesc;
    procedure TestFoldProvider;
    procedure TestNestedFoldsList;
    procedure TestNestedFoldsListCache;
  end;

implementation

type
  TSynEditFoldedViewHack = class(TSynEditFoldedView) end;

procedure TTestFoldedView.TstSetText(AName: String; AText: array of String);
begin
  PopBaseName;
  ReCreateEdit;
  SetLines(AText);
  PushBaseName(AName);
end;

procedure TTestFoldedView.TstFold(AName: String; AFoldAtIndex: integer;
  AExpectedLines: array of Integer);
begin
  FoldedView.FoldAtTextIndex(AFoldAtIndex);
  TestFoldedText(AName, AExpectedLines);
end;
procedure TTestFoldedView.TstFold(AName: String; AFoldAtIndex,
  AFoldAtColum: integer; AExpectedLines: array of Integer);
begin
  FoldedView.FoldAtTextIndex(AFoldAtIndex, AFoldAtColum);
  TestFoldedText(AName, AExpectedLines);
end;
procedure TTestFoldedView.TstFold(AName: String; AFoldAtIndex, AFoldAtColum,
  AFoldAtColCnt: integer; AExpectedLines: array of Integer);
begin
  FoldedView.FoldAtTextIndex(AFoldAtIndex, AFoldAtColum, AFoldAtColCnt);
  TestFoldedText(AName, AExpectedLines);
end;
procedure TTestFoldedView.TstFold(AName: String; AFoldAtIndex, AFoldAtColum,
  AFoldAtColCnt: integer; AFoldAtSkip: Boolean; AExpectedLines: array of Integer
  );
begin
  FoldedView.FoldAtTextIndex(AFoldAtIndex, AFoldAtColum, AFoldAtColCnt, AFoldAtSkip);
  TestFoldedText(AName, AExpectedLines);
end;
procedure TTestFoldedView.TstFold(AName: String; AFoldAtIndex, AFoldAtColum,
  AFoldAtColCnt: integer; AFoldAtSkip: Boolean; AVisibleLines: Integer;
  AExpectedLines: array of Integer);
begin
  FoldedView.FoldAtTextIndex(AFoldAtIndex, AFoldAtColum, AFoldAtColCnt, AFoldAtSkip, AVisibleLines);
  TestFoldedText(AName, AExpectedLines);
end;

procedure TTestFoldedView.TstUnFold(AName: String; AFoldAtIndex, AFoldAtColum,
  AFoldAtColCnt: integer; AFoldAtSkip: Boolean; AVisibleLines: Integer;
  AExpectedLines: array of Integer);
begin
  FoldedView.UnFoldAtTextIndex(AFoldAtIndex, AFoldAtColum, AFoldAtColCnt, AFoldAtSkip, AVisibleLines);
  TestFoldedText(AName, AExpectedLines);
end;

procedure TTestFoldedView.TstUnFoldAtCaret(AName: String; X, Y: integer;
  AExpectedLines: array of Integer);
begin
  SynEdit.CaretXY := Point(X, Y);
  TestFoldedText('UnfoldCaret - '+AName, AExpectedLines);
end;

// ViewPos is 1 based
procedure TTestFoldedView.TstTxtIndexToViewPos(AName: String;
  AExpectedPairs: array of Integer; ADoReverse: Boolean);
var i: Integer;
begin
  i := 0;
  while i < high(AExpectedPairs)-1 do begin
    AssertEquals(AName+' TxtIdx('+IntToStr( AExpectedPairs[i])+') to ViewPos[1-based]('+IntToStr( AExpectedPairs[i+1])+') ',
                 AExpectedPairs[i+1], ToPos(FoldedView.TextToViewIndex(AExpectedPairs[i])));
    if ADoReverse then
      AssertEquals(AName+' ViewPos[1-based]('+IntToStr( AExpectedPairs[i+1])+') to TxtIdx('+IntToStr( AExpectedPairs[i])+') [R]',
                 AExpectedPairs[i], FoldedView.ViewToTextIndex(ToIdx(AExpectedPairs[i+1])));
    inc(i, 2);
  end;
end;
// ViewPos is 1 based // Reverse of the above
procedure TTestFoldedView.TstViewPosToTextIndex(AName: String;
  AExpectedPairs: array of Integer; ADoReverse: Boolean);
var i: Integer;
begin
  i := 0;
  while i < high(AExpectedPairs)-1 do begin
    AssertEquals(AName+' ViewPos[1-based]('+IntToStr( AExpectedPairs[i])+') to TxtIdx('+IntToStr( AExpectedPairs[i+1])+')',
                 AExpectedPairs[i+1], FoldedView.ViewToTextIndex(ToIdx(AExpectedPairs[i])));
    if ADoReverse then
      AssertEquals(AName+' TxtIdx('+IntToStr( AExpectedPairs[i+1])+') to ViewPos[1-based]('+IntToStr( AExpectedPairs[i])+') [R]',
                 AExpectedPairs[i], ToPos(FoldedView.TextToViewIndex(AExpectedPairs[i+1])));
    inc(i, 2);
  end;
end;

// ScreenLine is 0 based
procedure TTestFoldedView.TstTextIndexToScreenLine(AName: String;
  AExpectedPairs: array of Integer; ADoReverse: Boolean);
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
procedure TTestFoldedView.TstScreenLineToTextIndex(AName: String;
  AExpectedPairs: array of Integer; ADoReverse: Boolean);
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




procedure TTestFoldedView.TestFoldedText(AName: String; ALines: array of Integer);
var
  ExpTxt: String;
  i: Integer;
  tmp, tmp1, tmp2, tmp3: String;
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

  if DoAutoFoldDescTests or DoAutoFoldDescTestsReadOnly then begin
    tmp  := FoldedView.GetFoldDescription(0, 1, -1, -1, False, False);
    tmp1 := FoldedView.GetFoldDescription(0, 1, -1, -1, False, True);
    tmp2 := FoldedView.GetFoldDescription(0, 1, -1, -1, True, False);
    tmp3 := FoldedView.GetFoldDescription(0, 1, -1, -1, True, True);
  end;


  FoldedView.FixFoldingAtTextIndex(0, SynEdit.Lines.Count-1);
  TestCompareString('after FixFolding', ExpTxt, GetFoldedText);

  if DoAutoFoldDescTests or DoAutoFoldDescTestsReadOnly then begin
    TestCompareString('GetFoldDesc after Fix fold 1', tmp,  FoldedView.GetFoldDescription(0, 1, -1, -1, False, False));
    TestCompareString('GetFoldDesc after Fix fold 2', tmp1, FoldedView.GetFoldDescription(0, 1, -1, -1, False, True));
    TestCompareString('GetFoldDesc after Fix fold 3', tmp2, FoldedView.GetFoldDescription(0, 1, -1, -1, True, False));
    TestCompareString('GetFoldDesc after Fix fold 4', tmp3, FoldedView.GetFoldDescription(0, 1, -1, -1, True, True));
  end;

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

procedure TTestFoldedView.SetLines(AText: array of String);
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

function TTestFoldedView.TestText1: TStringArray;
begin
  SetLength(Result, 10);
  Result[0] := 'program Foo;';
  Result[1] := 'procedure a;';
  Result[2] := 'begin';
  Result[3] := 'writeln()';
  Result[4] := 'end;';
  Result[5] := '';
  Result[6] := 'begin';
  Result[7] := '';
  Result[8] := 'end.';
  Result[9] := '';
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

function TTestFoldedView.TestText6: TStringArray;
begin
  SetLength(Result, 8);
  Result[0] := 'program Foo;';
  Result[1] := 'procedure a; procedure b;';          // 2 folds open on one line
  Result[2] := '  begin writeln(1);';
  Result[3] := '  end; // inner';
  Result[4] := 'begin';
  Result[5] := '    writeln(2)';
  Result[6]:= 'end;';
  Result[7]:= '';
end;

function TTestFoldedView.TestText7: TStringArray;
begin
  SetLength(Result, 27);
  Result[0] := 'program Foo;';
  Result[1] := '{$IFDEF x1}';
  Result[2] := '{$IFDEF x2} {$IFDEF x3}';
  Result[3] := '{$IFDEF x4} {$IFDEF x5} {$IFDEF x6} {$IFDEF x7}';
  Result[4] := '{$IFDEF x8} {$IFDEF x9} {$IFDEF xA}';
  Result[5] := '//foo A';
  Result[6] := '{$ENDIF XA}';
  Result[7] := '//foo 9';
  Result[8] := '{$ENDIF X9}';
  Result[9] := '//foo 8';
  Result[10] := '{$ENDIF X8}';
  Result[11] := '//foo 7';
  Result[12] := '{$ENDIF X7}';
  Result[13] := '//foo 6';
  Result[14] := '{$ENDIF X6}';
  Result[15] := '//foo 5';
  Result[16] := '{$ENDIF X5}';
  Result[17] := '//foo 4';
  Result[18] := '{$ENDIF X4}';
  Result[19] := '//foo 3';
  Result[20] := '{$ENDIF X3}';
  Result[21] := '//foo 2';
  Result[22] := '{$ENDIF X2}';
  Result[23] := '//foo 1';
  Result[24] := '{$ENDIF X1}';
  Result[25] := '//bar';
  Result[26] := '';
end;

function TTestFoldedView.TestText8: TStringArray;
begin
  // end begin lines, with mixed type
  SetLength(Result, 20);
  Result[0]  := 'program Foo;';
  Result[1]  := 'procedure a;';
  Result[2]  := 'begin';
  Result[3]  := '{%region}';
  Result[4]  := '{%endregion} {$ifdef x}';
  Result[5]  := '             {$endif} if a then begin';
  Result[6]  := '                      end;             {%region}';
  Result[7]  := '{%endregion} {$ifdef x}';
  Result[8]  := '             {$endif} if a then begin';
  Result[9]  := '                        writeln(1);';
  Result[10] := '{%region}             end;';
  Result[11] := '  writeln(1);';
  Result[12] := '{%endregion}  if a then begin';
  Result[13] := '                        writeln(1);';
  Result[14] := '{$ifdef x}    end;';
  Result[15] := '  writeln(1);';
  Result[16] := '{$endif}';
  Result[17] := '  writeln(1);';
  Result[18] := 'end';
  Result[19] := '';
end;

function TTestFoldedView.TestText9: TStringArray;
begin
  // end begin lines, with mixed type
  SetLength(Result, 9);
  Result[0]  := 'program Foo;';
  Result[1]  := 'procedure a;';
  Result[2]  := 'begin  {%region}';
  Result[3]  := '';
  Result[4]  := '{%endregion} ';
  Result[5]  := '';
  Result[6]  := 'end;';
  Result[7] := 'end';
  Result[8] := '';

end;

function TTestFoldedView.TestText10: TStringArray;
begin
  SetLength(Result, 17);
  Result[0]  := 'program Project1;';
  Result[1]  := 'begin';
  Result[2]  := '';
  Result[3]  := '  if 1=2 then begin';
  Result[4]  := '';
  Result[5]  := '  end;';
  Result[6]  := '';
  Result[7]  := '  if 1=3 then begin';
  Result[8]  := '';
  Result[9]  := '';
  Result[10] := '';
  Result[11] := '';
  Result[12] := '  end;';
  Result[13] := '';
  Result[14] := 'end.';
  Result[15] := '';
  Result[16] := '';

end;

function TTestFoldedView.TestText11: TStringArray;
begin
  SetLength(Result, 21);
  Result[ 0] := 'program Foo;';
  Result[ 1] := '{$IFDEF a} {$ENDIF}'; // lines with same fold-end-level
  Result[ 2] := '  {$IFDEF b} {$ENDIF}   {$IFDEF c} {$ENDIF}';
  Result[ 3] := '{$IFDEF X} ';
  Result[ 4] := '  {$ENDIF}   {$IFDEF Y}';
  Result[ 5] := '    {$ENDIF}';
  Result[ 6] := 'procedure a;';
  Result[ 7] := '  procedure inner;';
  Result[ 8] := '  begin  writeln();  end;';
  Result[ 9] := 'begin  writeln();  end;';
  Result[10] := '';
  Result[11] := 'procedure a;';
  Result[12] := '  procedure inner;';
  Result[13] := '  begin  writeln();  end;';
  Result[14] := 'begin  writeln();';
  Result[15] := 'end;';
  Result[16] := '//';
  Result[17] := '';
  Result[18] := '//';
  Result[19] := '//';
  Result[20] := '';

end;

function TTestFoldedView.TestText12: TStringArray;
begin
  SetLength(Result, 18);
  Result[0]  := 'program Project1;';
  Result[1]  := 'begin';
  Result[2]  := '';
  Result[3]  := '  if 1=2 then begin';
  Result[4]  := '';
  Result[5]  := '  end else begin;';
  Result[6]  := '';
  Result[7]  := '    if 1=3 then begin';
  Result[8]  := '';
  Result[9]  := '';
  Result[10] := '    end;';
  Result[11] := '';
  Result[12] := '  end;';
  Result[13] := '';
  Result[14] := 'end.';
  Result[15] := '';
  Result[16] := '';
  Result[17] := '';
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

function TTestFoldedView.TestTextPlain: TStringArray;
begin
  SetLength(Result, 11);
  Result[0] := 'begin';
  Result[1] := 'l1';
  Result[2] := 'end';
  Result[3] := 'l2';
  Result[4] := 'l3';
  Result[5] := 'l4';
  Result[6] := 'l5';
  Result[7] := 'begin';
  Result[8] := 'l6';
  Result[9] := 'end';
  Result[10] := '';

end;

function TTestFoldedView.TestTextBug21473: TStringArray;
begin
  SetLength(Result, 35);
  Result[ 0] := 'program a;';
  Result[ 1] := '';
  Result[ 2] := '// 1';
  Result[ 3] := '// 2';
  Result[ 4] := 'procedure Bar;';
  Result[ 5] := '';
  Result[ 6] := '  procedure BarA;';
  Result[ 7] := '  begin';
  Result[ 8] := '  end;';
  Result[ 9] := '';
  Result[10] := '  procedure BarB;';
  Result[11] := '  begin';
  Result[12] := '  end;';
  Result[13] := '';
  Result[14] := 'begin';
  Result[15] := 'end;';
  Result[16] := '';
  Result[17] := '// 1';
  Result[18] := '// 2';
  Result[19] := 'procedure Foo;';
  Result[20] := '';
  Result[21] := '  procedure FooA;';
  Result[22] := '  begin';
  Result[23] := '  end;';
  Result[24] := '';
  Result[25] := '  procedure FooB;';
  Result[26] := '  begin';
  Result[27] := '  end;';
  Result[28] := '';
  Result[29] := 'begin';
  Result[30] := 'end;';
  Result[31] := '';
  Result[32] := 'end.';
  Result[33] := '';
  Result[34] := '';
end;


procedure TTestFoldedView.TestFold;
  procedure RunTest;
  var
    i: Integer;
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

  {%region Text7 fold at indes, skip, ...}
    (* Arguments for (Un)FoldAt* (Line, ViewPos, TextIndex):
       - ColumnIndex (0-based)
           Can be negative, to access the highest(-1) available, 2nd highest(-2) ...
           If negative, count points downward
       - ColCount = 0 => all
       - Skip => Do not count nodes that are already in the desired state
           (or can not archive the desired state: e.g. can not hide)
       - AVisibleLines: 0 = Hide / 1 = Fold
    *)
    TstSetText('Text7 fold at indes, skip, ...', TestText7);
    {%region fold one}
    for i := 0 to 1 do begin
      PushBaseName('X='+IntToStr(i));
      SynEdit.UnfoldAll;
      TstFold('fold one col (pos): 0,1,x', 3,   0, 1, i=0, 1,  [0, 1, 2, 3,  19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('fold one col (pos): 1,1,x', 3,   1, 1, i=0, 1,  [0, 1, 2, 3,  17,18,19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('fold one col (pos): 2,1,x', 3,   2, 1, i=0, 1,  [0, 1, 2, 3,  15,16,17,18,19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('fold one col (pos): 3,1,x', 3,   3, 1, i=0, 1,  [0, 1, 2, 3,  13,14,15,16,17,18,19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('NOT fold one col (pos): 4,1,x', 3,   4, 1, i=0, 1,  [0, 1, 2, 3, 4,5,6,7,8,9,10,11,12, 13,14,15,16,17,18,19, 20, 21, 22, 23, 24, 25]);

      SynEdit.UnfoldAll;
      TstFold('fold one col (neg): -4,1,x', 3,   -4, 1, i=0, 1,  [0, 1, 2, 3,  19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('fold one col (neg): -3,1,x', 3,   -3, 1, i=0, 1,  [0, 1, 2, 3,  17,18,19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('fold one col (neg): -2,1,x', 3,   -2, 1, i=0, 1,  [0, 1, 2, 3,  15,16,17,18,19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('fold one col (neg): -1,1,x', 3,   -1, 1, i=0, 1,  [0, 1, 2, 3,  13,14,15,16,17,18,19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('NOT fold one col (neg): -5,1,x', 3,   -5, 1, i=0, 1,  [0, 1, 2, 3, 4,5,6,7,8,9,10,11,12, 13,14,15,16,17,18,19, 20, 21, 22, 23, 24, 25]);


      DoAutoFoldDescTestsReadOnly := DoAutoFoldDescTests;
      DoAutoFoldDescTests := False;
      // SKIP, if DoAutoFoldDescTests, since fold-info-apply checks for correct node type, and this code force hide.

      SynEdit.UnfoldAll;
      TstFold('hide one col (pos): 0,1,x', 3,   0, 1, i=0, 0,  [0, 1, 2,  19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('hide one col (pos): 1,1,x', 3,   1, 1, i=0, 0,  [0, 1, 2,  17,18,19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('hide one col (pos): 2,1,x', 3,   2, 1, i=0, 0,  [0, 1, 2,  15,16,17,18,19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('hide one col (pos): 3,1,x', 3,   3, 1, i=0, 0,  [0, 1, 2,  13,14,15,16,17,18,19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('NOT hide one col (pos): 4,1,x', 3,   4, 1, i=0, 0,  [0, 1, 2, 3, 4,5,6,7,8,9,10,11,12, 13,14,15,16,17,18,19, 20, 21, 22, 23, 24, 25]);

      SynEdit.UnfoldAll;
      TstFold('hide all-after col (pos): 0,1,x', 3,   0, 0, i=0, 0,  [0, 1, 2,  19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('hide all-after col (pos): 1,1,x', 3,   1, 0, i=0, 0,  [0, 1, 2,  17,18,19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('hide all-after col (pos): 2,1,x', 3,   2, 0, i=0, 0,  [0, 1, 2,  15,16,17,18,19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('hide all-after col (pos): 3,1,x', 3,   3, 0, i=0, 0,  [0, 1, 2,  13,14,15,16,17,18,19, 20, 21, 22, 23, 24, 25]);
      SynEdit.UnfoldAll;
      TstFold('NOT hide all-after col (pos): 4,1,x', 3,   4, 1, i=0, 0,  [0, 1, 2, 3, 4,5,6,7,8,9,10,11,12, 13,14,15,16,17,18,19, 20, 21, 22, 23, 24, 25]);

      DoAutoFoldDescTests := DoAutoFoldDescTestsReadOnly;


      PopBaseName;
    end;
    {%endregion}

    {%region fold two}
      {%region  1st:: 0,1,F}
        // 1st:: 0,1,F // SKIP=False
        SynEdit.UnfoldAll; PushBaseName('(1st:: 0,1,F / 2nd::  x=1 no-sk c=1)');
        TstFold  ('  fold pre-one col (pos):   0,1,F', 3,   0, 1, False, 1,  [0,1,2,3,  19, 20, 21, 22, 23, 24, 25]);
        TstFold  ('  fold 2nd col (pos/no-sk): 1,1,F', 3,   1, 1, False, 1,  [0,1,2,3,  19, 20, 21, 22, 23, 24, 25]);
        TstUnFold('UNfold 1st col (pos/no-sk): 0,1,F', 3,   0, 1, False, 1,  [0,1,2,3,  17,18,19, 20, 21, 22, 23, 24, 25]);

        SynEdit.UnfoldAll; PushBaseName('(1st:: 0,1,F / 2nd::  x=2 no-sk c=1)');
        TstFold  ('  fold pre-one col (pos):   0,1,F', 3,   0, 1, False, 1,  [0,1,2,3,  19, 20, 21, 22, 23, 24, 25]);
        TstFold  ('  fold 3rd col (pos/no-sk): 2,1,F', 3,   2, 1, False, 1,  [0,1,2,3,  19, 20, 21, 22, 23, 24, 25]);
        TstUnFold('UNfold 1st col (pos/no-sk): 0,1,F', 3,   0, 1, False, 1,  [0,1,2,3,  15,16,17,18,19, 20, 21, 22, 23, 24, 25]);

        SynEdit.UnfoldAll; PushBaseName('(1st:: 0,1,F / 2nd::  x=3 no-sk c=1)');
        TstFold  ('  fold pre-one col (pos):   0,1,F', 3,   0, 1, False, 1,  [0,1,2,3,  19, 20, 21, 22, 23, 24, 25]);
        TstFold  ('  fold 4th col (pos/no-sk): 3,1,F', 3,   3, 1, False, 1,  [0,1,2,3,  19, 20, 21, 22, 23, 24, 25]);
        TstUnFold('UNfold 1st col (pos/no-sk): 0,1,F', 3,   0, 1, False, 1,  [0,1,2,3,  13,14,15,16,17,18,19, 20, 21, 22, 23, 24, 25]);

        // 1st:: 0,1,F // SKIP=True
        SynEdit.UnfoldAll; PopPushBaseName('(1st:: 0,1,F / 2nd::  x=0 skip c=1)');
        TstFold  ('  fold pre-one col (pos):   0,1,F', 3,   0, 1, False, 1,  [0,1,2,3,  19, 20, 21, 22, 23, 24, 25]);
        TstFold  ('  fold 2nd col (pos/skip):  0,1,T', 3,   0, 1, True,  1,  [0,1,2,3,  19, 20, 21, 22, 23, 24, 25]);
  //      TstUnFold('UNfold 1st col (pos/no-sk): 0,1,F', 3,   0, 1, False, 1,  [0,1,2,3,  17,18,19, 20, 21, 22, 23, 24, 25]);

        SynEdit.UnfoldAll; PopPushBaseName('(1st:: 0,1,F / 2nd::  x=1 skip c=1)');
        TstFold  ('  fold pre-one col (pos):   0,1,F', 3,   0, 1, False, 1,  [0,1,2,3,  19, 20, 21, 22, 23, 24, 25]);
        TstFold  ('  fold 3rd col (pos/skip):  1,1,T', 3,   1, 1, True,  1,  [0,1,2,3,  19, 20, 21, 22, 23, 24, 25]);
  //      TstUnFold('UNfold 1st col (pos/no-sk): 0,1,F', 3,   0, 1, False, 1,  [0,1,2,3,  15,16,17,18,19, 20, 21, 22, 23, 24, 25]);

        SynEdit.UnfoldAll; PopPushBaseName('(1st:: 0,1,F / 2nd::  x=2 skip c=1)');
        TstFold  ('  fold pre-one col (pos):   0,1,F', 3,   0, 1, False, 1,  [0,1,2,3,  19, 20, 21, 22, 23, 24, 25]);
        TstFold  ('  fold 4th col (pos/skip):  2,1,T', 3,   2, 1, True,  1,  [0,1,2,3,  19, 20, 21, 22, 23, 24, 25]);
  //      TstUnFold('UNfold 1st col (pos/no-sk): 0,1,F', 3,   0, 1, False, 1,  [0,1,2,3,  13,14,15,16,17,18,19, 20, 21, 22, 23, 24, 25]);
      {%endregion}

      {%region  1st:: 1,1,F}
      {%endregion}

      {%region  1st:: -1,1,F}
      {%endregion}

      {%region  1st:: -2,1,F}
      {%endregion}
    {%endregion}
  {%endregion Text7 fold at indes, skip, ...}


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

procedure TTestFoldedView.TestFoldEdit;

  procedure DoChar(x, y: integer; char: String);
  begin
    SynEdit.CaretXY := Point(x,y);
    SynEdit.CommandProcessor(ecChar, char, nil);
  end;
  procedure DoNewLine(x, y: integer);
  begin
    SynEdit.CaretXY := Point(x,y);
    SynEdit.CommandProcessor(ecLineBreak, '', nil);
  end;
  procedure DoBackspace(x, y: integer);
  begin
    SynEdit.CaretXY := Point(x,y);
    SynEdit.CommandProcessor(ecDeleteLastChar, '', nil);
  end;

  procedure TestNodeAtPos(name: string; x, y: integer; ExpClassification: TFoldNodeClassification = fncHighlighter);
  var
    n: TSynTextFoldAVLNode;
  begin
    n := TSynEditFoldedViewHack(FoldedView).FoldTree.FindFoldForLine(y, true);
    AssertTrue(BaseTestName+' '+ name+ ' got node for line '+inttostr(y), n.IsInFold);
    AssertTrue(BaseTestName+' '+ name+ ' got node Classification for line '+inttostr(y), n.Classification = ExpClassification);
    AssertEquals(BaseTestName+' '+ name+ ' got node for src-line '+inttostr(y), y, n.SourceLine);
    AssertEquals(BaseTestName+' '+ name+ ' got node for src-line '+inttostr(y)+' col='+inttostr(x), x, n.FoldColumn);
  end;

var
  n: string;
  i: integer;
  s: String;
begin

  {%region simple}
    TstSetText('Simple: fold Prc', TestText);

    TstFold('', 1, [0, 1]);
    TestNodeAtPos('', 1, 2);

    DoChar(1,2, ' ');
    TestFoldedText('(ins char)', [0, 1]);
    TestNodeAtPos('(ins char)', 2, 2);

    DoNewLine(13,1);
    TestFoldedText('(newline before)', [0, 1, 2]);
    TestNodeAtPos('(newline before)', 2, 3);

    DoBackspace(1,2);
    TestFoldedText('(del newline)', [0, 1]);
    TestNodeAtPos('(del newline)', 2, 2);

    DoBackspace(2,2);
    TestFoldedText('(del char)', [0, 1]);
    TestNodeAtPos('(del char)', 1, 2);

    DoBackspace(1,2);
    TestFoldedText('(del to prev line)', [0]);
    TestNodeAtPos('(del to prev line)', 13, 1);

    DoNewLine(13,1);  // newline, on same line
    TestFoldedText('(newline on srcline)', [0, 1]);
    TestNodeAtPos('(newline on srcline)', 1, 2);
    PopBaseName;

    TstSetText('Simple 2: edit del foldable line', TestText3);
    TstFold('', 7, [0, 1, 2, 3, 4, 5, 6, 7,  10, 11]);
    SetCaretAndSel(1,3, 1,4);
    SynEdit.CommandProcessor(ecDeleteChar, '', nil);
    TestFoldedText('fold after', [0, 1, 2, 3, 4, 5, 6,  9, 10]);


    PopBaseName;
  {%endregion}

  {%region Nested}
    TstSetText('Nested: fold Prc Beg ', TestText);

    for i := 0 to 63 do begin
      PushBaseName(inttostr(i));
      SetLines(TestText);
      SynEdit.UnfoldAll;
      n := '';
      TstFold(n, 2, [0, 1, 2]);             TestNodeAtPos(n, 1, 3);
      n := 'outer';
      TstFold(n, 1, [0, 1]);                TestNodeAtPos(n, 1, 2);

      n := '(ins char)';
      //debugln(['############### ',n]);
      DoChar(1,2, ' ');
      TestFoldedText(n, [0, 1]);            TestNodeAtPos(n, 2, 2);
      if (i and 1) <> 0 then begin
        n := '(ins char) nested';
        //debugln(['############### ',n]);
        TstUnFoldAtCaret(n, 1,3, [0,1,2]);    TestNodeAtPos(n, 1, 3);
        n := '(ins char) refold';
        TstFold(n, 1, [0, 1]);                TestNodeAtPos(n, 2, 2); // re-folded
      end;

      n := '(newline before)';
        //debugln(['############### ',n]);
      DoNewLine(13,1);
      TestFoldedText(n, [0, 1, 2]);         TestNodeAtPos(n, 2, 3);
      if (i and 2) <> 0 then begin
        n := '(newline before) nested';
        //debugln(['############### ',n]);
        TstUnFoldAtCaret(n, 1,4, [0,1,2,3]);  TestNodeAtPos(n, 1, 4);
        n := '(newline before) refold';
        TstFold(n, 2, [0, 1, 2]);             TestNodeAtPos(n, 2, 3); // re-folded
      end;

      n := '(del newline)';
      //debugln(['############### ',n]);
      DoBackspace(1,2);
      TestFoldedText(n, [0, 1]);            TestNodeAtPos(n, 2, 2);
      if (i and 4) <> 0 then begin
        n := '(del newline) nested';
        //debugln(['############### ',n]);
        TstUnFoldAtCaret(n, 1,3, [0,1,2]);    TestNodeAtPos(n, 1, 3);
        n := '(del newline) refold';
        TstFold(n, 1, [0, 1]);                TestNodeAtPos(n, 2, 2); // re-folded
      end;

      n := '(del char)';
      //debugln(['############### ',n]);
      DoBackspace(2,2);
      TestFoldedText(n, [0, 1]);            TestNodeAtPos(n, 1, 2);
      if (i and 8) <> 0 then begin
        n := '(del char) nested';
        //debugln(['############### ',n]);
        TstUnFoldAtCaret(n, 1,3, [0,1,2]);    TestNodeAtPos(n, 1, 3);
        n := '(del char) refold';
        TstFold(n, 1, [0, 1]);                TestNodeAtPos(n, 1, 2); // re-folded
      end;

      n := '(del to prev line)';
      //debugln(['############### ',n]);
      DoBackspace(1,2);
      TestFoldedText(n, [0]);               TestNodeAtPos(n, 13, 1);
      if (i and 16) <> 0 then begin
        n := '(del to prev line) nested';
        TstUnFoldAtCaret(n, 1,2, [0,1]);    TestNodeAtPos(n, 1, 2);
        n := '(del to prev line) refold';
        TstFold(n, 0, [0]);                TestNodeAtPos(n, 13, 1); // re-folded
      end;

      n := '(newline on srcline)';
      DoNewLine(13,1);  // newline, on same line
      TestFoldedText(n, [0, 1]);           TestNodeAtPos(n, 1, 2);
      if (i and 32) <> 0 then begin
        n := '(del to prev line) nested';
        TstUnFoldAtCaret(n, 1,3, [0,1,2]);    TestNodeAtPos(n, 1, 3);
        n := '(del to prev line) refold';
        TstFold(n, 1, [0,1]);                TestNodeAtPos(n, 1, 2); // re-folded
      end;

      PopBaseName;
    end;
    PopBaseName;
  {%endregion}

  {%region Nested}
  TstSetText('Nested, same line: fold Prc Beg', TestText6);

    for i := 0 to 255 do begin
      PushBaseName(inttostr(i));
      SetLines(TestText6);
      SynEdit.UnfoldAll;
      n := '';
      TstFold(n, 1, 1, [0, 1, 4,5,6]);             TestNodeAtPos(n, 14, 2);
      n := 'outer';
      TstFold(n, 1, 0, [0, 1]);                    TestNodeAtPos(n, 1, 2);

      n := '(ins char)';
      //debugln(['############### ',n]);
      DoChar(1,2, ' ');
      TestFoldedText(n, [0, 1]);                    TestNodeAtPos(n, 2, 2);
      if (i and 1) <> 0 then begin
        n := '(ins char) nested';
        //debugln(['############### ',n]);
        TstUnFoldAtCaret(n, 1,5, [0,1,4,5,6]);      TestNodeAtPos(n, 15, 2);
        n := '(ins char) refold';
        TstFold(n, 1, 0, [0, 1]);                   TestNodeAtPos(n, 2, 2); // re-folded
      end;

      n := '(ins char middle)';
      //debugln(['############### ',n]);
      DoChar(14,2, ' ');
      TestFoldedText(n, [0, 1]);                    TestNodeAtPos(n, 2, 2);
      if (i and 2) <> 0 then begin
        n := '(ins char middle) nested';
        //debugln(['############### ',n]);
        TstUnFoldAtCaret(n, 1,5, [0,1,4,5,6]);      TestNodeAtPos(n, 16, 2);
        n := '(ins char middle) refold';
        TstFold(n, 1, 0, [0, 1]);                   TestNodeAtPos(n, 2, 2); // re-folded
      end;


      n := '(newline before)';
        //debugln(['############### ',n]);
      DoNewLine(13,1);
      TestFoldedText(n, [0, 1, 2]);                 TestNodeAtPos(n, 2, 3);
      if (i and 4) <> 0 then begin
        n := '(newline before) nested';
        //debugln(['############### ',n]);
        TstUnFoldAtCaret(n, 1,6, [0,1,2, 5,6,7]);   TestNodeAtPos(n, 16, 3);
        n := '(newline before) refold';
        TstFold(n, 2, 0, [0, 1, 2]);                TestNodeAtPos(n, 2, 3); // re-folded
      end;

      n := '(del newline)';
      //debugln(['############### ',n]);
      DoBackspace(1,2);
      TestFoldedText(n, [0, 1]);                   TestNodeAtPos(n, 2, 2);
      if (i and 8) <> 0 then begin
        n := '(del newline) nested';
        //debugln(['############### ',n]);
        TstUnFoldAtCaret(n, 1,5, [0,1, 4,5,6]);    TestNodeAtPos(n, 16, 2);
        n := '(del newline) refold';
        TstFold(n, 1, 0, [0, 1]);                  TestNodeAtPos(n, 2, 2); // re-folded
      end;

      n := '(del char)';
      //debugln(['############### ',n]);
      DoBackspace(2,2);
      TestFoldedText(n, [0, 1]);                   TestNodeAtPos(n, 1, 2);
      if (i and 16) <> 0 then begin
        n := '(del char) nested';
        //debugln(['############### ',n]);
        TstUnFoldAtCaret(n, 1,5, [0,1,4,5,6]);     TestNodeAtPos(n, 15, 2);
        n := '(del char) refold';
        TstFold(n, 1, 0, [0, 1]);                  TestNodeAtPos(n, 1, 2); // re-folded
      end;

      n := '(del char middle)';
      //debugln(['############### ',n]);
      DoBackspace(15,2);
      TestFoldedText(n, [0, 1]);                   TestNodeAtPos(n, 1, 2);
      if (i and 32) <> 0 then begin
        n := '(del char middle) nested';
        //debugln(['############### ',n]);
        TstUnFoldAtCaret(n, 1,5, [0,1,4,5,6]);     TestNodeAtPos(n, 14, 2);
        n := '(del char middle) refold';
        TstFold(n, 1, 0, [0, 1]);                  TestNodeAtPos(n, 1, 2); // re-folded
      end;


      n := '(del to prev line)';
      //debugln(['############### ',n]);
      DoBackspace(1,2);
      TestFoldedText(n, [0]);               TestNodeAtPos(n, 13, 1);
      if (i and 64) <> 0 then begin
        n := '(del to prev line) nested';
        TstUnFoldAtCaret(n, 1,4, [0,3,4,5]);    TestNodeAtPos(n, 26, 1);
        n := '(del to prev line) refold';
        TstFold(n, 0,1, [0]);                TestNodeAtPos(n, 13, 1); // re-folded idx=1, prg is at 0
      end;

      n := '(newline on srcline)';
      DoNewLine(13,1);  // newline, on same line
      TestFoldedText(n, [0, 1]);           TestNodeAtPos(n, 1, 2);
      if (i and 128) <> 0 then begin
        n := '(del to prev line) nested';
        TstUnFoldAtCaret(n, 1,5, [0,1,4,5,6]);    TestNodeAtPos(n, 14, 2);
        n := '(del to prev line) refold';
        TstFold(n, 1, 0, [0,1]);                TestNodeAtPos(n, 1, 2); // re-folded
      end;

      PopBaseName;
    end;
  {%endregion}

  {%region}
    TstSetText('Nested, same line, new line in middle:', TestText6);
    SynEdit.UnfoldAll;
    n := '';
    TstFold(n, 1, 1, [0, 1, 4,5,6]);             TestNodeAtPos(n, 14, 2);
    n := 'outer';
    TstFold(n, 1, 0, [0, 1]);                    TestNodeAtPos(n, 1, 2);
    n := '(new line)';
    //debugln(['############### ',n]);
    DoNewLine(14,2);
    TestFoldedText(n, [0, 1, 2, 5,6,7]);
    TestNodeAtPos(n, 1, 3);
    PopBaseName;

    TstSetText('Nested, same line, new line in middle: (2)', TestText6);
    SynEdit.UnfoldAll;
    n := '';
    TstFold(n, 1, 1, [0, 1, 4,5,6]);             TestNodeAtPos(n, 14, 2);
    TstFold(n, 1, 0, [0, 1]);                    TestNodeAtPos(n, 1, 2);
    n := '(new line)';
    //debugln(['############### ',n]);
    DoNewLine(13,2);
    TestFoldedText(n, [0, 1, 2, 5,6,7]);
    TestNodeAtPos(n, 2, 3);
    PopBaseName;
  {%endregion}

  {%region simple, block edit}
    TstSetText('Simple: block edit', TestText);

    TstFold('', 1, [0, 1]);
    TestNodeAtPos('', 1, 2);

    SynEdit.TextBetweenPoints[point(1,2), point(1,2)] := ' ';
    TestFoldedText('(ins char)', [0, 1]);
    TestNodeAtPos('(ins char)', 2, 2);

    SynEdit.TextBetweenPoints[point(13,1), point(13,1)] := LineEnding;
    TestFoldedText('(newline before)', [0, 1, 2]);
    TestNodeAtPos('(newline before)', 2, 3);

    SynEdit.TextBetweenPoints[point(13,1), point(1,2)] := '';
    TestFoldedText('(del newline)', [0, 1]);
    TestNodeAtPos('(del newline)', 2, 2);

    SynEdit.TextBetweenPoints[point(1,2), point(2,2)] := '';
    TestFoldedText('(del char)', [0, 1]);
    TestNodeAtPos('(del char)', 1, 2);

    SynEdit.TextBetweenPoints[point(13,1), point(1,2)] := '';
    TestFoldedText('(del to prev line)', [0]);
    TestNodeAtPos('(del to prev line)', 13, 1);

    SynEdit.TextBetweenPoints[point(13,1), point(13,1)] := LineEnding;
    TestFoldedText('(newline on srcline)', [0, 1]);
    TestNodeAtPos('(newline on srcline)', 1, 2);


    SynEdit.TextBetweenPoints[point(1,3), point(1,3)] := LineEnding;
    TestFoldedText('(newline, 1st fold line)', [0, 1]);
    TestNodeAtPos('(newline 1st fold line)', 1, 2);

    SynEdit.TextBetweenPoints[point(1,3), point(1,4)] := '';
    TestFoldedText('(del newline, 1st fold line)', [0, 1]);
    TestNodeAtPos('(del newline 1st fold line)', 1, 2);

    PopBaseName;
  {%endregion}

  {%region Nested block edit}
    TstSetText('Nested: block edit ', TestText);
    //SetLines(TestText);

    n := '(ins char)';
    TstFold(n, 2, [0, 1, 2]);             TestNodeAtPos(n, 1, 3);
    TstFold(n, 1, [0, 1]);                TestNodeAtPos(n, 1, 2);
    //debugln(['############### ',n]);
    SynEdit.TextBetweenPoints[point(1,3), point(1,3)] := ' ';
    TestFoldedText(n, [0, 1]);            TestNodeAtPos(n, 1, 2);
    TstUnFoldAtCaret(n, 1,3, [0,1,2]);    TestNodeAtPos(n, 2, 3);

    n := '(repl char to newline)';
    TstFold(n, 1, [0, 1]);                TestNodeAtPos(n, 1, 2);
    //debugln(['############### ',n]);
    SynEdit.TextBetweenPoints[point(1,3), point(2,3)] := LineEnding;
    TestFoldedText(n, [0, 1]);            TestNodeAtPos(n, 1, 2);
    TstUnFoldAtCaret(n, 1,4, [0,1,2,3]);    TestNodeAtPos(n, 1, 4);

    n := '(repl newline to char)';
    TstFold(n, 1, [0, 1]);                TestNodeAtPos(n, 1, 2);
    //debugln(['############### ',n]);
    SynEdit.TextBetweenPoints[point(1,3), point(1,4)] := '  ';
    TestFoldedText(n, [0, 1]);            TestNodeAtPos(n, 1, 2);
    TstUnFoldAtCaret(n, 1,3, [0,1,2]);    TestNodeAtPos(n, 3, 3);

    n := '(del char)';
    TstFold(n, 1, [0, 1]);                TestNodeAtPos(n, 1, 2);
    //debugln(['############### ',n]);
    SynEdit.TextBetweenPoints[point(1,3), point(3,3)] := '';
    TestFoldedText(n, [0, 1]);            TestNodeAtPos(n, 1, 2);
    TstUnFoldAtCaret(n, 1,3, [0,1,2]);    TestNodeAtPos(n, 1, 3);

  PopBaseName;
  {%endregion}

  {%region simple, lines access}
    TstSetText('Simple: lines access', TestText);

    TstFold('', 1, [0, 1]);
    TestNodeAtPos('', 1, 2);

    SynEdit.Lines.Insert(1,'// foo');
    TestFoldedText('(insert before)', [0, 1, 2]);
    TestNodeAtPos('(insert before)', 1, 3);

    SynEdit.Lines.Delete(1);
    TestFoldedText('(del before)', [0, 1]);
    TestNodeAtPos('(del before)', 1, 2);

    SynEdit.Lines.Insert(2,'// foo');
    TestFoldedText('(insert inside)', [0, 1]);
    TestNodeAtPos('(insert inside)', 1, 2);

    SynEdit.Lines.Delete(2);
    TestFoldedText('(del inside)', [0, 1]);
    TestNodeAtPos('(del inside)', 1, 2);

    PopBaseName;
  {%endregion}


  {%region hide}
    TstSetText('Simple HIDE', TestTextHide(3));

    TstFold('', 1, -1, 1, False, 0, [0, 4]);
    TestNodeAtPos('', 1, 2);

    DoNewLine(13,1);
    TestFoldedText('(ins newline)', [0, 1, 5]);
    TestNodeAtPos('(ins newline)', 1, 3);

    SynEdit.Undo; // cannot use backspace, since caret would unfold
    TestFoldedText('(del newline)', [0, 4]);
    TestNodeAtPos('(del newline)', 1, 2);

    PopBaseName;
  {%endregion}

  {%region hide, block edit}
    TstSetText('Simple HIDE: block edit', TestTextHide(3));

// TODO /newline BEFORE
    TstFold('', 1, -1, 1, False, 0, [0, 4]);
    TestNodeAtPos('', 1, 2);

    SynEdit.TextBetweenPoints[point(13,1), point(13,1)] := LineEnding;
    TestFoldedText('(newline before)', [0, 1, 5]);
    TestNodeAtPos('(newline before)', 1, 3);

    SynEdit.TextBetweenPoints[point(13,1), point(1,2)] := '';
    TestFoldedText('(del newline before)', [0, 4]);
    TestNodeAtPos('(del newline before)', 1, 2);



    SynEdit.TextBetweenPoints[point(1,2), point(1,2)] := ' ';
    TestFoldedText('(ins char)', [0, 4]);
    TestNodeAtPos('(ins char)', 2, 2);

    debugln(['############### ins newline']);
    SynEdit.TextBetweenPoints[point(1,2), point(2,2)] := LineEnding;
    TestFoldedText('(ins newline)', [0, 1, 5]);
    TestNodeAtPos('(ins newline)', 1, 3);

    debugln(['############### del newline']);
    SynEdit.TextBetweenPoints[point(1,2), point(1,3)] := '  ';
    TestFoldedText('(del newline)', [0, 4]);
    TestNodeAtPos('(del newline)', 3, 2);

    debugln(['############### del char']);
    SynEdit.TextBetweenPoints[point(1,2), point(3,2)] := ' ';
    TestFoldedText('(del char)', [0, 4]);
    TestNodeAtPos('(del char)', 2, 2);

    debugln(['############### ins newline (again)']);
    SynEdit.TextBetweenPoints[point(1,2), point(2,2)] := LineEnding;
    TestFoldedText('(ins newline)', [0, 1, 5]);
    TestNodeAtPos('(ins newline)', 1, 3);

    debugln(['############### del TWO newline']);
    SynEdit.TextBetweenPoints[point(1,2), point(1,4)] := '';
    TestFoldedText('(del newline)', [0, 3]);
    TestNodeAtPos('(del newline)', 1, 2);

    PopBaseName;
  {%endregion}

  {%region lines access}
    TstSetText('Simple HIDE: lines access', TestTextHide(3));

    TstFold('', 1, -1, 1, False, 0, [0, 4]);
    TestNodeAtPos('', 1, 2);

    SynEdit.Lines.Insert(1,'var a: integer;');
    TestFoldedText('(ins newline before)', [0, 1, 5]);
    TestNodeAtPos('(ins newline before)', 1, 3);

    SynEdit.Lines.Delete(1);
    TestFoldedText('(del newline before)', [0, 4]);
    TestNodeAtPos('(del newline before)', 1, 2);

    SynEdit.Lines.Insert(2,'// foo bar');
    TestFoldedText('(ins newline inside)', [0, 5]);
    TestNodeAtPos('(ins newline inside)', 1, 2);

    SynEdit.Lines.Delete(2);
    TestFoldedText('(del newline inside)', [0, 4]);
    TestNodeAtPos('(del newline inside)', 1, 2);

    PopBaseName;
  {%endregion}

  {%region}
    TstSetText('TestText10 remove one entire fold', TestText10);
    TstFold('f1', 7, -1, 1, False, 1, [0, 1, 2, 3, 4, 5, 6, 7, 13, 14, 15]);
    TstFold('f2', 3, -1, 1, False, 1, [0, 1, 2, 3, 6, 7, 13, 14, 15]);
    TestNodeAtPos('n1', 15, 4);
    TestNodeAtPos('n2', 15, 8);

    SetCaretAndSel(1, 4, 1, 8);
    SynEdit.CutToClipboard;

    TstFold('f2', 3, -1, 1, False, 1, [0, 1, 2, 3, 9, 10, 11]);
    TestNodeAtPos('n3', 15, 4);

    PopBaseName;
  {%endregion}

  {%region}
    // 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33
    TstSetText('TestTextBug21473', TestTextBug21473);
    TstFold('FooB', 25, -1, 1, False, 1, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,  28,29,30,31,32,33]);
    TstFold('FooA', 21, -1, 1, False, 1, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,  24,25,  28,29,30,31,32,33]);
    TstFold('Foo ', 19, -1, 1, False, 1, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,  31,32,33]);

    TestNodeAtPos('n1', 1, 20);

    s := SynEdit.TextBetweenPoints[point(1,7), point(1,10)];
    SynEdit.TextBetweenPoints[point(1,6), point(1,9)] := '';

    TestFoldedText('Cut', [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,   28,29,30]);
    TestNodeAtPos('n1 Cut', 1, 17);

    SynEdit.TextBetweenPoints[point(1,7), point(1,7)] := s;

    TestFoldedText('Restore', [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,  31,32,33]);
    TestNodeAtPos('n1 Restore', 1, 20);

    SynEdit.Undo;
    //debugln('*********AFTER UNDO');  FoldedView.debug;

    TestFoldedText('Undone', [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,   28,29,30]);
    TestNodeAtPos('n1 Undone', 1, 17);

    //SetCaretAndSel(1, 4, 1, 8);
    //SynEdit.CutToClipboard;
    PopBaseName;
  {%endregion}


    TstSetText('Simple: fold Prc', TestTextBug21473);
    FoldedView.FoldAtTextIndex(7);
    FoldedView.FoldAtTextIndex(11);
    FoldedView.FoldAtTextIndex(6);
    FoldedView.FoldAtTextIndex(10);
    FoldedView.FoldAtTextIndex(4);

    FoldedView.FoldAtTextIndex(22);
    FoldedView.FoldAtTextIndex(26);
    FoldedView.FoldAtTextIndex(21);
    FoldedView.FoldAtTextIndex(25);
    FoldedView.FoldAtTextIndex(19);

//FoldedView.debug;

    SynEdit.TextBetweenPoints[point(1,3), point(1,31)] := '';

    DebugLn('#############################');
//FoldedView.debug;


end;

procedure TTestFoldedView.TestFoldStateFromText;

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
var
  a1,a2{, a3, a4}: String;
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
{$ifdef ENDIAN_LITTLE }
                    #$00#$00#$00#$00#$00#$00#$00#$00#$07#$00#$00#$00#$04#$00#$00#$00#$04#$00#$00#$00#$04#$00#$00#$00#$0A#$00#$00#$00#$04#$00#$00#$00#$02#$00#$00#$00#$00#$00#$00#$00#$05#$00#$00#$00#$04#$00#$00#$00#$00#$00#$00#$00#$03#$00#$00#$00#$01#$00#$00#$00#$02#$00#$00#$00,
{$else                }
                    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$07#$00#$00#$00#$04#$00#$00#$00#$04#$00#$00#$00#$04#$00#$00#$00#$0A#$00#$00#$00#$04#$00#$00#$00#$02#$00#$00#$00#$00#$00#$00#$00#$05#$00#$00#$00#$04#$00#$00#$00#$00#$00#$00#$00#$03#$00#$00#$00#$01#$00#$00#$00#$02,
{$endif ENDIAN_LITTLE }
                    FoldedView.GetFoldDescription(0,1,-1,-1, False, False)
                   );
  TestCompareString('FoldDesc (txt / NOT ext)', ' TA004 T12025',
                    FoldedView.GetFoldDescription(0,1,-1,-1, True,  False)
                   );
  // TODO: Extended is not yet implemented
  //TestCompareString('FoldDesc (NOT txt / ext)',
{$ifdef ENDIAN_LITTLE }
  //                  #$00#$00#$00#$00#$00#$00#$00#$00#$07#$00#$00#$00#$04#$00#$00#$00#$00#$00#$00#$00#$03#$00#$00#$00#$0A#$00#$00#$00#$02#$00#$00#$00#$00#$00#$00#$00#$05#$00#$00#$00#$04#$00#$00#$00#$00#$00#$00#$00#$03#$00#$00#$00#$01#$00#$00#$00,
{$else                }
  //                  #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$07#$00#$00#$00#$04#$00#$00#$00#$00#$00#$00#$00#$03#$00#$00#$00#$0A#$00#$00#$00#$02#$00#$00#$00#$00#$00#$00#$00#$05#$00#$00#$00#$04#$00#$00#$00#$00#$00#$00#$00#$03#$00#$00#$00#$01,
{$endif ENDIAN_LITTLE }
  //                  FoldedView.GetFoldDescription(0,1,-1,-1, False, True)
  //                 );
  //TestCompareString('FoldDesc (txt / ext)', ' TA004 T12025',
  //                  FoldedView.GetFoldDescription(0,1,-1,-1, True,  True)
  //                 );




  // No crash,if folded selection
  ReCreateEdit;
  SetLines(TestTextPlain);
  SetCaretAndSel(1,5, 2,6);
  FoldedView.FoldAtTextIndex(4, 0, 1, False, 0);
  AssertEquals(FoldedView.Count, 8);

  FoldedView.GetFoldDescription(0, 0, -1, -1, True,  False);
  FoldedView.GetFoldDescription(0, 0, -1, -1, False, False);
  FoldedView.GetFoldDescription(0, 0, -1, -1, True,  True);
  FoldedView.GetFoldDescription(0, 0, -1, -1, False, True);

  // compare fold desc with/without selection-fold
  ReCreateEdit;
  SetLines(TestTextPlain);
  FoldedView.FoldAtTextIndex(0);
  FoldedView.FoldAtTextIndex(7);
  AssertEquals(FoldedView.Count, 6);

  a1 := FoldedView.GetFoldDescription(0, 0, -1, -1, True,  False);
  a2 := FoldedView.GetFoldDescription(0, 0, -1, -1, False, False);
  {a3 := }FoldedView.GetFoldDescription(0, 0, -1, -1, True,  True);
  {a4 := }FoldedView.GetFoldDescription(0, 0, -1, -1, False, True);

  SetCaretAndSel(1,5, 2,6);
  FoldedView.FoldAtTextIndex(4, 0, 1, False, 0);
  AssertEquals(FoldedView.Count, 4);

  TestCompareString('1', a1, FoldedView.GetFoldDescription(0, 0, -1, -1, True,  False));
  TestCompareString('2', a2, FoldedView.GetFoldDescription(0, 0, -1, -1, False, False));
//  a3 := FoldedView.GetFoldDescription(0, 0, -1, -1, True,  True);
//  a4 := FoldedView.GetFoldDescription(0, 0, -1, -1, False, True);

end;

procedure TTestFoldedView.TestFoldProvider;
  procedure DoTestOpenCounts(AName: string; AType: Integer; AExp: Array of Integer);
  var
    i: Integer;
  begin
    AName := AName + ' (type=' + IntToStr(AType)+') ';
    for i := low(AExp) to high(AExp) do
      DebugLn([BaseTestName+AName+ ' line=' + IntToStr(i)+ ' exp=', AExp[i],'   Got=', FoldedView.FoldProvider.FoldOpenCount(i, AType)]);
    for i := low(AExp) to high(AExp) do
      AssertEquals(BaseTestName+AName+ ' line=' + IntToStr(i),
                   AExp[i], FoldedView.FoldProvider.FoldOpenCount(i, AType));
  end;

var
  i: Integer;
begin
  // TSynEditFoldProvider.FoldOpenCount(ALineIdx: Integer; AType: Integer = 0): Integer;
  PushBaseName('');

  TstSetText('TestText1', TestText);
  EnableFolds([cfbtBeginEnd..cfbtNone]);
  //                       p  P  B  ~  -
  DoTestOpenCounts('', 0, [1, 1, 1, 0, 0]); // all (fold conf)
  DoTestOpenCounts('', 1, [1, 1, 1, 0, 0]); // pas
  //DoTestOpenCounts('', 4, [1, 1, 1, 0, 0]); // pas (incl unfolded)
  DoTestOpenCounts('', 2, [0, 0, 0, 0, 0]); // %region
  DoTestOpenCounts('', 3, [0, 0, 0, 0, 0]); // $if

  TstSetText('TestText1 (2)', TestText);
  EnableFolds([cfbtTopBeginEnd]);
  //                       p  P  B  ~  -
  DoTestOpenCounts('', 0, [0, 0, 1, 0, 0]); // all (fold conf)
  DoTestOpenCounts('', 1, [0, 0, 1, 0, 0]); // pas
  //DoTestOpenCounts('', 4, [1, 1, 1, 0, 0]); // pas (incl unfolded)
  DoTestOpenCounts('', 2, [0, 0, 0, 0, 0]); // %region
  DoTestOpenCounts('', 3, [0, 0, 0, 0, 0]); // $if

  TstSetText('TestText1 (3)', TestText);
  EnableFolds([cfbtProcedure, cfbtBeginEnd]);
  //                       p  P  B  ~  -
  DoTestOpenCounts('', 0, [0, 1, 0, 0, 0]); // all (fold conf)
  DoTestOpenCounts('', 1, [0, 1, 0, 0, 0]); // pas
  //DoTestOpenCounts('', 4, [1, 1, 1, 0, 0]); // pas (incl unfolded)
  DoTestOpenCounts('', 2, [0, 0, 0, 0, 0]); // %region
  DoTestOpenCounts('', 3, [0, 0, 0, 0, 0]); // $if



  TstSetText('TestText2', TestText2);
  EnableFolds([cfbtBeginEnd..cfbtNone]);
  //                                      if    else
  //                       p  PP B  -  B  B  ~  -B ~  -  -  ~
  DoTestOpenCounts('', 0, [1, 2, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0]);
  DoTestOpenCounts('', 1, [1, 2, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0]);
  //DoTestOpenCounts('', 4, [1, 2, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0]);
  DoTestOpenCounts('', 2, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
  DoTestOpenCounts('', 3, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);

  TstSetText('TestText2 (2)', TestText2);
  EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtProgram, cfbtRegion]);
  //                       p  PP B  -  B  B  ~  -B ~  -  -  ~
  DoTestOpenCounts('', 0, [0, 2, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0]);
  DoTestOpenCounts('', 1, [0, 2, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0]);
  //DoTestOpenCounts('', 4, [1, 2, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0]);
  DoTestOpenCounts('', 2, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
  DoTestOpenCounts('', 3, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);



  TstSetText('TestText3', TestText3);
  EnableFolds([cfbtBeginEnd..cfbtNone],  [cfbtSlashComment]);
  //                                      if    else        // one-line-comment
  //                       p  $  P  -  B  %B ~  --B~  -  -  /
  DoTestOpenCounts('', 0, [1, 1, 1, 0, 1, 2, 0, 1, 0, 0, 0, 1]);
  DoTestOpenCounts('', 1, [1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1]);
  //DoTestOpenCounts('', 4, [1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1]);
  DoTestOpenCounts('', 2, [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0]); // %region
  DoTestOpenCounts('', 3, [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]); // %if

  TstSetText('TestText3 (2)', TestText3);
  EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtProgram, cfbtRegion],  [cfbtSlashComment]);
  //                       p  $  P  -  B  %B ~  --B~  -  -  /
  DoTestOpenCounts('', 0, [0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1]);
  DoTestOpenCounts('', 1, [0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1]);
  //DoTestOpenCounts('', 4, [1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1]);
  DoTestOpenCounts('', 2, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]); // %region
  DoTestOpenCounts('', 3, [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]); // %if

  TstSetText('TestText3 (3)', TestText3);
  EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtProgram, cfbtIfDef], []);
  //                       p  $  P  -  B  %B ~  --B~  -  -  /
  DoTestOpenCounts('', 0, [0, 0, 1, 0, 1, 2, 0, 1, 0, 0, 0, 0]);
  DoTestOpenCounts('', 1, [0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0]);
  //DoTestOpenCounts('', 4, [1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1]);
  DoTestOpenCounts('', 2, [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0]); // %region
  DoTestOpenCounts('', 3, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]); // %if



  for i := 0 to 2 do begin // pos of $IFDEF does not matter
    TstSetText('TestTextPasHl-'+IntToStr(i)+'', TestTextPasHl(i));
    EnableFolds([cfbtBeginEnd..cfbtNone],  [cfbtSlashComment]);
    //                             if       $E // one-line-comment
    //                       p  P  $bb-  -  -  /
    DoTestOpenCounts('', 0, [1, 1, 3, 0, 0, 0, 1]);
    DoTestOpenCounts('', 1, [1, 1, 2, 0, 0, 0, 1]);
    //DoTestOpenCounts('', 4, [1, 1, 2, 0, 0, 0, 1]);
    DoTestOpenCounts('', 2, [0, 0, 0, 0, 0, 0, 0]); // %region
    DoTestOpenCounts('', 3, [0, 0, 1, 0, 0, 0, 0]); // %if

    TstSetText('TestTextPasHl-'+IntToStr(i)+'', TestTextPasHl(i));
    EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtBeginEnd],  [cfbtSlashComment]);
    //                             if       $E // one-line-comment
    //                       p  P  $bb-  -  -  /
    DoTestOpenCounts('', 0, [1, 1, 2, 0, 0, 0, 1]);
    DoTestOpenCounts('', 1, [1, 1, 1, 0, 0, 0, 1]);
    //DoTestOpenCounts('', 4, [1, 1, 1, 0, 0, 0, 1]);
    DoTestOpenCounts('', 2, [0, 0, 0, 0, 0, 0, 0]); // %region
    DoTestOpenCounts('', 3, [0, 0, 1, 0, 0, 0, 0]); // %if

    TstSetText('TestTextPasHl-'+IntToStr(i)+'', TestTextPasHl(i));
    EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtIfDef],  [cfbtSlashComment]);
    //                             if       $E // one-line-comment
    //                       p  P  $bb-  -  -  /
    DoTestOpenCounts('', 0, [1, 1, 2, 0, 0, 0, 1]);
    DoTestOpenCounts('', 1, [1, 1, 2, 0, 0, 0, 1]);
    //DoTestOpenCounts('', 4, [1, 1, 2, 0, 0, 0, 1]);
    DoTestOpenCounts('', 2, [0, 0, 0, 0, 0, 0, 0]); // %region
    DoTestOpenCounts('', 3, [0, 0, 0, 0, 0, 0, 0]); // %if
  end;



  TstSetText('TestText4', TestText4);
  EnableFolds([cfbtBeginEnd..cfbtNone],  [cfbtSlashComment]);
  //                       pPBB  -  B  -  B  -
  DoTestOpenCounts('', 0, [3, 1, 0, 1, 0, 1]);
  DoTestOpenCounts('', 1, [3, 1, 0, 1, 0, 1]);
  //DoTestOpenCounts('', 4, [3, 1, 0, 1, 0, 1]);
  DoTestOpenCounts('', 2, [0, 0, 0, 0, 0, 0]);
  DoTestOpenCounts('', 3, [0, 0, 0, 0, 0, 0]);



  TstSetText('TestText8', TestText8);
  EnableFolds([cfbtBeginEnd..cfbtNone],  [cfbtSlashComment]);
  //                       p  P  B  %  $  B  %  $  B  ~  %  ~  B  ~  $  ~  -  ~  -
  DoTestOpenCounts('', 0, [1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0]);
  DoTestOpenCounts('', 1, [1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0]);
//DoTestOpenCounts('', 4, [1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0]);
  DoTestOpenCounts('', 2, [0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0]);
  DoTestOpenCounts('', 3, [0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0]);

end;

Procedure TTestFoldedView.CheckNode(nd: TSynFoldNodeInfo; ALine: TLineIdx; AColumn: integer;
  LogXStart, LogXEnd,  FoldLvlStart, FoldLvlEnd,  NestLvlStart, NestLvlEnd: Integer;
  FoldType: integer;  FoldTypeCompatible: integer; FoldGroup: Integer;
  FoldAction: TSynFoldActions);
begin
  CheckPasFoldNodeInfo('', nd, ALine, AColumn, LogXStart, LogXEnd, FoldLvlStart,
    FoldLvlEnd, NestLvlStart, NestLvlEnd,
    TPascalCodeFoldBlockType(FoldType), TPascalCodeFoldBlockType(FoldTypeCompatible),
    FoldGroup, FoldAction);
end;

Procedure TTestFoldedView.CheckNodeLines(AList: TLazSynEditNestedFoldsList; ALines: array of integer);
var
  i: Integer;
begin
  for i := 0 to high(ALines) do
    AssertEquals(BaseTestName+ ' Node line=' + IntToStr(i), ALines[i], AList.NodeLine[i]);
end;

Procedure TTestFoldedView.CheckNodeEndLines(AList: TLazSynEditNestedFoldsList; ALines: array of integer);
var
  i: Integer;
begin
  for i := 0 to high(ALines) do
    AssertEquals(BaseTestName+ ' Node end line=' + IntToStr(i), ALines[i], AList.NodeEndLine[i]);
end;

procedure TTestFoldedView.InitList(const AName: String; AList: TLazSynEditNestedFoldsList;
  ALine, AGroup: Integer; AFlags: TSynFoldBlockFilterFlags;
  AInclOpening: Boolean; AClear: Boolean = True);
var
  i: Integer;
begin
  PopPushBaseName(Format('%s (Line=%d / Grp=%d / FLG=%s / IncOpen=%s / Prep=%d,%d)', [AName, ALine, AGroup, dbgs(AFlags), dbgs(AInclOpening), PrepareLine, PrepareMax]));
  AList.ResetFilter;
  if AClear then AList.Clear;
  AList.FoldGroup := AGroup;
  AList.FoldFlags := AFlags;
  AList.IncludeOpeningOnLine := AInclOpening;
  if (PrepareLine >= 0) and (PrepareLine < SynEdit.Lines.Count) then begin
    AList.Line := PrepareLine;
    for i := 0 to Min(AList.Count-1, PrepareLine) do AList.NodeLine[i];
  end;
  AList.Line := ALine;
end;

procedure TTestFoldedView.TestNestedFoldsList;
var
  TheList: TLazSynEditNestedFoldsList;
  i1, i2, i3, i, pl, pm: Integer;
begin
// L= *(\d+).*?(\d+).*?(\d+).*?(\d+).*?(\d+).*?(\d+).*?(\d+).*?(\d+).*?(\d+).*?(\d+).*?(\d+).*?A=(.*)
// CheckNode(TheList.HLNode[2],  $1, $2,  $3, $4,  $5, $6,  $7, $8,  $9, $10, $11, $12);

  PushBaseName(''); // TstSetText();
  PushBaseName(''); // InitList();

  {%region TestText1}
  For pl := -1 to 5 do begin
  PrepareLine := pl;
  For pm := 1 to Max(1, Min(PrepareLine+1, 3)) do begin
    PrepareMax := pm;
    TstSetText('TestText1', TestText1);
    TheList := FoldedView.FoldProvider.NestedFoldsList;
    EnableFolds([cfbtBeginEnd..cfbtNone]);

    InitList('All Enabled ',  TheList,  2, 0, [], True);
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[2],  2, 0,  0, 5,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNodeLines   (TheList, [0,1,2]);
    CheckNodeEndLines(TheList, [8,4,4]);

    InitList('All Enabled ',  TheList,  2, 0, [], True);
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNodeLines   (TheList, [0,1,2]);
    CheckNodeEndLines(TheList, [8,4,4]);

    InitList('All Enabled Reverse order',  TheList,  2, 0, [], True);
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[2],  2, 0,  0, 5,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNodeLines   (TheList, [0,1,2]);
    CheckNodeEndLines(TheList, [8,4,4]);


    InitList('All Enabled',  TheList,  2, FOLDGROUP_PASCAL, [], True);
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[2],  2, 0,  0, 5,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNodeLines   (TheList, [0,1,2]);
    CheckNodeEndLines(TheList, [8,4,4]);


    InitList('All Enabled',  TheList,  2, FOLDGROUP_REGION, [], True);
    AssertEquals(BaseTestName + 'Cnt', 0, TheList.Count);


    InitList('All Enabled',  TheList,  2, 0, [], False);
    AssertEquals(BaseTestName + 'Cnt', 2, TheList.Count);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNodeLines   (TheList, [0,1]);
    CheckNodeEndLines(TheList, [8,4]);


    InitList('All Enabled',  TheList,  3, 0, [], False);
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[2],  2, 0,  0, 5,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNodeLines   (TheList, [0,1,2]);
    CheckNodeEndLines(TheList, [8,4,4]);

    InitList('All Enabled Reverse Order',  TheList,  3, 0, [], False);
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[2],  2, 0,  0, 5,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);

    InitList('All Enabled Mixed Order',  TheList,  3, 0, [], False);
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[2],  2, 0,  0, 5,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);

    InitList('All Enabled Mixed Order 2',  TheList,  3, 0, [], False);
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[2],  2, 0,  0, 5,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);

    InitList('All Enabled Mixed Order 3',  TheList,  3, 0, [], False);
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[2],  2, 0,  0, 5,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);


    EnableFolds([cfbtTopBeginEnd]);
    InitList('cfbtTopBeginEnd Enabled',  TheList,  2, 0, [], True);
    AssertEquals(BaseTestName + 'Cnt', 1, TheList.Count);
    CheckNode(TheList.HLNode[0],  2, 0,  0, 5,  0, 1,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);


    EnableFolds([cfbtTopBeginEnd]);
    InitList('cfbtTopBeginEnd Enabled',  TheList,  2, FOLDGROUP_PASCAL, [], True);
    AssertEquals(BaseTestName + 'Cnt', 1, TheList.Count);
    CheckNode(TheList.HLNode[0],  2, 0,  0, 5,  0, 1,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);


    EnableFolds([cfbtTopBeginEnd]);
    InitList('cfbtTopBeginEnd Enabled',  TheList,  2, 0, [], False);
    AssertEquals(BaseTestName + 'Cnt', 0, TheList.Count);


    EnableFolds([cfbtTopBeginEnd]);
    InitList('cfbtTopBeginEnd Enabled',  TheList,  3, 0, [], False);
    AssertEquals(BaseTestName + 'Cnt', 1, TheList.Count);
    CheckNode(TheList.HLNode[0],  2, 0,  0, 5,  0, 1,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);


    InitList('cfbtTopBeginEnd Enabled',  TheList,  2, 0, [sfbIncludeDisabled], True);
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[2],  2, 0,  0, 5,  0, 1,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9, -1,-1,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7, -1,-1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold, sfaMultiLine]);


    EnableFolds([cfbtTopBeginEnd]);
    InitList('cfbtTopBeginEnd Enabled',  TheList,  2, FOLDGROUP_PASCAL, [sfbIncludeDisabled], True);
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[2],  2, 0,  0, 5,  0, 1,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9, -1,-1,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7, -1,-1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold, sfaMultiLine]);


    EnableFolds([cfbtTopBeginEnd]);
    PopPushBaseName('cfbtTopBeginEnd Enabled - group 2 - sfbIncludeDisabled');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 2;
    TheList.FoldGroup := 2;
    TheList.FoldFlags := [sfbIncludeDisabled];
    AssertEquals(BaseTestName + 'Cnt', 0, TheList.Count);


    EnableFolds([cfbtTopBeginEnd]);
    PopPushBaseName('cfbtTopBeginEnd Enabled - group 0 - NoCurrentLine - sfbIncludeDisabled');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 2;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [sfbIncludeDisabled];
    TheList.IncludeOpeningOnLine := False;
    AssertEquals(BaseTestName + 'Cnt', 2, TheList.Count);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9, -1,-1,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7, -1,-1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold, sfaMultiLine]);


    EnableFolds([cfbtTopBeginEnd]);
    PopPushBaseName('cfbtTopBeginEnd Enabled - group 0 - NoCurrentLine line 3 - sfbIncludeDisabled');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 3;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [sfbIncludeDisabled];
    TheList.IncludeOpeningOnLine := False;
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[2],  2, 0,  0, 5,  0, 1,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9, -1,-1,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7, -1,-1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold, sfaMultiLine]);


    // TODO line, currently ignores the opening "begin" on current line
    EnableFolds([]);
    PopPushBaseName('None Enabled - group 0 - sfbIncludeDisabled - line 3');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 3;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [sfbIncludeDisabled];
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[2],  2, 0,  0, 5, -1,-1,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9, -1,-1,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7, -1,-1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold, sfaMultiLine]);

    PopPushBaseName('group 0 - line 4 (with "end"');
    for i2 := 0 to 1 do begin // sfbIncludeDisabled
      if i2 = 0
      then PushBaseName('sfbIncludeDisabled')
      else PushBaseName('NOT sfbIncludeDisabled');
      for i3 := 0 to 2 do begin // EnableFolds
        case i3 of
          0: begin
              EnableFolds([cfbtBeginEnd..cfbtNone]);
              PushBaseName('all enabled');
              i := 3;
            end;
          1: begin
              EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtTopBeginEnd]);
              PushBaseName('all enabled');
              i := 2;
            end;
          2: begin
              EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtTopBeginEnd, cfbtProcedure]);
              PushBaseName('all enabled');
              i := 1;
            end;
        end;
        if i2 = 0 then i := 3;

        TheList.Clear;
        TheList.ResetFilter;
        TheList.Line := 4;
        TheList.FoldGroup := 0;
        if i2 = 0
        then TheList.FoldFlags := [sfbIncludeDisabled]
        else TheList.FoldFlags := [];

        for i1 := 0 to 3 do begin // IncludeOpeningOnLine (switch on<>off<>on without clear)
          if i1 in [0,2]
          then PushBaseName('IncludeOpeningOnLine')
          else PushBaseName('NOT IncludeOpeningOnLine');

          TheList.IncludeOpeningOnLine := i1 in [0,2];
          AssertEquals(BaseTestName + 'Cnt', i, TheList.Count);
          if i3 in [0] then
            CheckNode(TheList.HLNode[2],  2, 0,  0, 5,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine])
          else if i2 = 0 then
            CheckNode(TheList.HLNode[2],  2, 0,  0, 5,  -1, -1,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold, sfaMultiLine]);
          if i3 in [0,1] then
            CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine])
          else if i2 = 0 then
            CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  -1, -1,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold, sfaMultiLine]);
          CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);

          PopBaseName;
        end;
        PopBaseName;
      end;
      PopBaseName;
    end;

    PopBaseName;
  end;
  end;
  {%endregion TestText1}

  {%region TestText2}
  For pl := -1 to 12 do begin
  PrepareLine := pl;
  For pm := 1 to Max(1, Min(PrepareLine+1, 5)) do begin
    PrepareMax := pm;
    TstSetText('TestText2', TestText2);
    TheList := FoldedView.FoldProvider.NestedFoldsList;
    EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtIfThen,cfbtForDo,cfbtWhileDo,cfbtWithDo]);

    PushBaseName('All Enabled - group 0 - line 1');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 1;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [];
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[2],  1, 1,  13, 22,  2, 3,  2, 3,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNodeLines   (TheList, [0,1,1]);
    CheckNodeEndLines(TheList, [11,10,3]);


    PopPushBaseName('All Enabled - group 0 - line 1 - no current');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 1;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [];
    TheList.IncludeOpeningOnLine := False;
    AssertEquals(BaseTestName + 'Cnt', 1, TheList.Count);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);


    PopPushBaseName('All Enabled - group 0 - line 3');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 3;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [];
    AssertEquals(BaseTestName + 'Cnt', 4, TheList.Count);
    CheckNode(TheList.HLNode[3],  2, 0,  2, 7,  3, 4,  3, 4,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[2],  1, 1,  13, 22,  2, 3,  2, 3,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNodeLines   (TheList, [0,1,1,2]);
    CheckNodeEndLines(TheList, [11,10,3,3]);


    PopPushBaseName('All Enabled - group 0 - line 4');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 4;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [];
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[2],  4, 0,  23, 28,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);


    PopPushBaseName('All Enabled - group 0 - line 4 - NO IncludeOpeningOnLine');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 4;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [];
    TheList.IncludeOpeningOnLine := False;
    AssertEquals(BaseTestName + 'Cnt', 2, TheList.Count);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);


    PopPushBaseName('All Enabled - group 0 - line 5');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 5;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [];
    AssertEquals(BaseTestName + 'Cnt', 4, TheList.Count);
    CheckNode(TheList.HLNode[3],  5, 0,  12, 17,  3, 4,  3, 4,  0, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[2],  4, 0,  23, 28,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);

    PopPushBaseName('All Enabled - group 0 - line 7 mixed');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 7;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [];
    TheList.IncludeOpeningOnLine := True;
    AssertEquals(BaseTestName + 'Cnt', 5, TheList.Count);
    CheckNode(TheList.HLNode[4],  7, 0,  11, 16,  3, 4,  3, 4,  0, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[3],  5, 0,  12, 17,  3, 4,  3, 4,  0, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[2],  4, 0,  23, 28,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);

    PopPushBaseName('All Enabled - group 0 - line 7 mixed // NOT IncludeOpeningOnLine');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 7;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [];
    TheList.IncludeOpeningOnLine := False;
    AssertEquals(BaseTestName + 'Cnt', 4, TheList.Count);
    CheckNode(TheList.HLNode[3],  5, 0,  12, 17,  3, 4,  3, 4,  0, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[2],  4, 0,  23, 28,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);

    PopBaseName;
  end;
  end;
  {%endregion TestText2}

  {%region TestText3}
  For pl := -1 to 12 do begin
  PrepareLine := pl;
  For pm := 1 to Max(1, Min(PrepareLine+1, 5)) do begin
    PrepareMax := pm;

    TstSetText('TestText3', TestText3);
    TheList := FoldedView.FoldProvider.NestedFoldsList;
    EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtIfThen,cfbtForDo,cfbtWhileDo,cfbtWithDo]);

    PushBaseName('All Enabled - group 0 - line 3');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 3;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [];
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[2],  2, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  1, 0,  1, 7,  0, 1,  0, 1,  18, 18, 3, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNodeLines   (TheList, [0,1,2]);
    CheckNodeEndLines(TheList, [11,3,10]);


    PopPushBaseName('All Enabled - group 1 - line 3');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 3;
    TheList.FoldGroup := 1;
    TheList.FoldFlags := [];
    AssertEquals(BaseTestName + 'Cnt', 2, TheList.Count);
    CheckNode(TheList.HLNode[1],  2, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);


    PopPushBaseName('All Enabled - group 3 - line 3');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 3;
    TheList.FoldGroup := 3;
    TheList.FoldFlags := [];
    AssertEquals(BaseTestName + 'Cnt', 1, TheList.Count);
    CheckNode(TheList.HLNode[0],  1, 0,  1, 7,  0, 1,  0, 1,  18, 18, 3, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);


    PopPushBaseName('All Enabled - group 0 - line 3');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 4;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [];
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[2],  4, 0,  0, 5,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  2, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);

    PopBaseName;
  end;
  end;
  {%endregion TestText2}

  {%region TestText11}
  For pl := -1 to 20 do begin
  PrepareLine := pl;
  For pm := 1 to Max(1, Min(PrepareLine+1, 5)) do begin
    PrepareMax := pm;
    TstSetText('TestText11', TestText11);
    TheList := FoldedView.FoldProvider.NestedFoldsList;
    EnableFolds([cfbtBeginEnd..cfbtNone]);

    PushBaseName('All Enabled - group 0 - incl line 4');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 4;
    TheList.IncludeOpeningOnLine := True;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [];
    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
    CheckNode(TheList.HLNode[2],  4, 0,  14, 20,  0, 1,  0, 1,  18, 18, FOLDGROUP_IFDEF, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[1],  3, 0,   1,  7,  0, 1,  0, 1,  18, 18, FOLDGROUP_IFDEF, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,   0,  7,  0, 1,  0, 1,  10, 10, FOLDGROUP_PASCAL, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);

    PopPushBaseName('All Enabled - group 0 - excl line 4');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 4;
    TheList.IncludeOpeningOnLine := False;
    TheList.FoldGroup := 0;
    TheList.FoldFlags := [];
    AssertEquals(BaseTestName + 'Cnt', 2, TheList.Count);
    CheckNode(TheList.HLNode[1],  3, 0,   1,  7,  0, 1,  0, 1,  18, 18, FOLDGROUP_IFDEF, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  0, 0,   0,  7,  0, 1,  0, 1,  10, 10, FOLDGROUP_PASCAL, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);


    PushBaseName('All Enabled - group IF - incl line 4');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 4;
    TheList.IncludeOpeningOnLine := True;
    TheList.FoldGroup := FOLDGROUP_IFDEF;
    TheList.FoldFlags := [];
    AssertEquals(BaseTestName + 'Cnt', 2, TheList.Count);
    CheckNode(TheList.HLNode[1],  4, 0,  14, 20,  0, 1,  0, 1,  18, 18, FOLDGROUP_IFDEF, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
    CheckNode(TheList.HLNode[0],  3, 0,   1,  7,  0, 1,  0, 1,  18, 18, FOLDGROUP_IFDEF, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);

    PopPushBaseName('All Enabled - group IF - excl line 4');
    TheList.ResetFilter;  TheList.Clear;
    TheList.Line := 4;
    TheList.IncludeOpeningOnLine := False;
    TheList.FoldGroup := FOLDGROUP_IFDEF;
    TheList.FoldFlags := [];
//TheList.Count; TheList.HLNode[0]; TheList.Debug;
    AssertEquals(BaseTestName + 'Cnt', 1, TheList.Count);
    CheckNode(TheList.HLNode[0],  3, 0,   1,  7,  0, 1,  0, 1,  18, 18, FOLDGROUP_IFDEF, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);

    PopBaseName;
  end;
  end;
  {%endregion TestText11}


  TstSetText('TestText9', TestText9);
  TheList := FoldedView.FoldProvider.NestedFoldsList;
  EnableFolds([cfbtBeginEnd..cfbtNone]);

  PushBaseName('All Enabled - group 0 - line 3');
  TheList.ResetFilter;  TheList.Clear;
  TheList.Line := 5;
  TheList.FoldGroup := 0;
  TheList.FoldFlags := [];
  AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
  CheckNode(TheList.HLNode[2],  2, 0,  0, 5,  2, 3,  2, 3,  1, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
  CheckNode(TheList.HLNode[1],  1, 0,  0, 9,  1, 2,  1, 2,  3, 3, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
  CheckNode(TheList.HLNode[0],  0, 0,  0, 7,  0, 1,  0, 1,  10, 10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
  PopBaseName;


  {%region TestText12}
  TstSetText('TestText12', TestText12);
  TheList := FoldedView.FoldProvider.NestedFoldsList;
  EnableFolds([cfbtBeginEnd..cfbtSlashComment]); // not include IF then ...

  For pl := -1 to 12 do begin
  PrepareLine := pl;
  For pm := 1 to Max(1, Min(PrepareLine+1, 4)) do begin
  PrepareMax := pm;
  for i := 0 to 16 do begin;
    InitList('',  TheList,  i, 0, [], False);
    case i of
      0:          AssertEquals(BaseTestName + 'Cnt', 0, TheList.Count);
      1:          AssertEquals(BaseTestName + 'Cnt', 1, TheList.Count);
      2,3:        AssertEquals(BaseTestName + 'Cnt', 2, TheList.Count);
      4,5,6,7:    AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
      8,9,10:     AssertEquals(BaseTestName + 'Cnt', 4, TheList.Count);
      11,12:      AssertEquals(BaseTestName + 'Cnt', 3, TheList.Count);
      13,14:      AssertEquals(BaseTestName + 'Cnt', 2, TheList.Count);
      15,16:      AssertEquals(BaseTestName + 'Cnt', 0, TheList.Count);
    end;

    for i1 := TheList.Count-1 downto 0 do begin
      i2 := -1;
      case i of
        1:             case i1 of
                         0: i2 := 0;
                       end;
        2,3, 13,14:    case i1 of
                         0: i2 := 0;
                         1: i2 := 1;
                       end;
        4,5:           case i1 of
                         0: i2 := 0;
                         1: i2 := 1;
                         2: i2 := 3;
                       end;
        6,7, 11,12:    case i1 of
                         0: i2 := 0;
                         1: i2 := 1;
                         2: i2 := 5;
                       end;
        8,9,10:        case i1 of
                         0: i2 := 0;
                         1: i2 := 1;
                         2: i2 := 5;
                         3: i2 := 7;
                       end;
      end;

      case i2 of
        0: CheckNode(TheList.HLNode[i1],  0, 0,  0, 7,  0, 1,  0, 1, 10,10, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
        1: CheckNode(TheList.HLNode[i1],  1, 0,  0, 5,  1, 2,  1, 2,  0{1}, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
        3: CheckNode(TheList.HLNode[i1],  3, 0, 14,19,  2, 3,  2, 3,  0, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
        5: CheckNode(TheList.HLNode[i1],  5, 0, 11,16,  2, 3,  2, 3,  0, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
        7: CheckNode(TheList.HLNode[i1],  7, 0, 16,21,  3, 4,  3, 4,  0, 0, 1, [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
        else AssertTrue(BaseTestName + ' internal ', false);
      end;
    end;

  end;
  end;
  end;
  {%endregion TestText12}


  PopBaseName;PopBaseName;
end;

procedure TTestFoldedView.TestNestedFoldsListCache;
var
  TheList: TLazSynEditNestedFoldsList;
begin
  PushBaseName('cache');
  TstSetText('TestText1', TestText1);
  TheList := FoldedView.FoldProvider.NestedFoldsList;
  EnableFolds([cfbtBeginEnd..cfbtNone]);
  PrepareLine := -1;

  InitList('All Enabled ',  TheList,  1, 0, [], False, True);
  TheList.Count; // only access count
  TheList.Line := 8;
  TheList.HLNode[0]; // do not crash // group levels are not initialized



  // Issue 0033996
  TstSetText('TestText11', TestText11);
  TheList := FoldedView.FoldProvider.NestedFoldsList;
  EnableFolds([cfbtBeginEnd..cfbtNone]);
  PrepareLine := -1;

  InitList('All Enabled ',  TheList,  8, FOLDGROUP_PASCAL, [], False, True);
  TheList.HLNode[TheList.Count-1];
  TheList.HLNode[TheList.Count-2];

  TheList.Line := 11;
  TheList.HLNode[TheList.Count-1];
  TheList.HLNode[TheList.Count-2];
  TheList.HLNode[TheList.Count-3];

  PopBaseName;
end;

initialization

  RegisterTest(TTestFoldedView); 
end.

