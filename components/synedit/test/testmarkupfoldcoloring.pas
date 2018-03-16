unit TestMarkupFoldColoring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, testregistry, TestBase, TestHighlightPas, Forms,
  LCLProc, LCLType, SynEdit, SynHighlighterPas, SynEditMarkupFoldColoring,
  SynEditMiscClasses, SynEditMarkup, SynEditHighlighterFoldBase;

type

  { TTestMarkupFoldColoring }

  TTestMarkupFoldColoring = class(TTestBaseHighlighterPas)
  private
    Markup: TSynEditMarkupFoldColors;
    FFirstInvalidatedLine,FLastInvalidatedLine: integer;
    procedure ClearInvalidatedLines;
    procedure CaptureTestInvalidateLines(FirstLine, LastLine: integer);

    procedure TestNoInvalidate(aName: string = '');
    procedure TestInvalidate(aName: string; aExpFrom, aExpTo: Integer);
    procedure TestBeginMarkup(aName: string = '');
    procedure TestRowColumns(aName: string; aRow: Integer;
      aExpColumns, aExpColors: Array of Integer; aScrollOffs: Integer = 0); overload;
    (* TestRowColumns( name, row,
       [], // exp (phys) column for vert lines // must be complete
       [], // exp colors // can be empty -> no tests
       [], // exp pos, len, pos2, len2,... of marked words // must be complete
       [], // exp colors of words // can be empty -> no tests
       scroll // simulate horiz scrolled line
    *)
    procedure TestRowColumns(aName: string; aRow: Integer;
      aExpColumns, aExpColors, aExpWords, aExpWordsColor: Array of Integer; aScrollOffs: Integer = 0); overload;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ReCreateEdit; reintroduce;
    function TestText1: TStringArray;
    procedure EnableOutlines(AEnbledTypes: TPascalCodeFoldBlockTypes);
  published
    procedure TestColors;
  end;

implementation

{ TTestMarkupFoldColoring }

procedure TTestMarkupFoldColoring.ClearInvalidatedLines;
begin
  FFirstInvalidatedLine := -1;
  FLastInvalidatedLine := -1;
end;

procedure TTestMarkupFoldColoring.CaptureTestInvalidateLines(FirstLine,
  LastLine: integer);
begin
  if (FFirstInvalidatedLine < 0) or (FirstLine < FFirstInvalidatedLine) then
    FFirstInvalidatedLine := FirstLine;
  if (LastLine > FLastInvalidatedLine) then
    FLastInvalidatedLine := LastLine;
  SynEdit.InvalidateLines(FirstLine, LastLine);
end;

procedure TTestMarkupFoldColoring.TestNoInvalidate(aName: string);
begin
  AssertEquals(BaseTestName + ' '+ aName + 'Painting row did not call invalidate', -1, FFirstInvalidatedLine);
  AssertEquals(BaseTestName + ' '+ aName + 'Painting row did not call invalidate', -1, FLastInvalidatedLine);
end;

procedure TTestMarkupFoldColoring.TestInvalidate(aName: string; aExpFrom,
  aExpTo: Integer);
begin
  AssertEquals(BaseTestName + ' '+ aName + ' row did call invalidate', aExpFrom, FFirstInvalidatedLine);
  AssertEquals(BaseTestName + ' '+ aName + ' row did call invalidate', aExpTo, FLastInvalidatedLine);
end;

procedure TTestMarkupFoldColoring.TestBeginMarkup(aName: string);
begin
  ClearInvalidatedLines;
  Markup.BeginMarkup;
  TestNoInvalidate(aName);
end;

procedure TTestMarkupFoldColoring.TestRowColumns(aName: string; aRow: Integer;
  aExpColumns, aExpColors: array of Integer; aScrollOffs: Integer);
begin
  TestRowColumns(aName, aRow, aExpColumns, aExpColors, [], [], aScrollOffs);
end;

procedure TTestMarkupFoldColoring.TestRowColumns(aName: string; aRow: Integer;
  aExpColumns, aExpColors, aExpWords, aExpWordsColor: array of Integer;
  aScrollOffs: Integer);
var
  i, nextP, nextL: Integer;
  rtl: TLazSynDisplayRtlInfo;
  startCol: TLazSynDisplayTokenBound;
  gotColor: TSynSelectedColor;
begin
  PushBaseName(aName);
  ClearInvalidatedLines;
  rtl.IsRtl := False;
  startCol.Offset := 0;

  Markup.PrepareMarkupForRow(aRow);
  nextL := 0;
  nextP := 0;
  if (aScrollOffs > 0) and (aScrollOffs <= Length(aExpColumns)) then
    nextP := aExpColumns[aScrollOffs - 1];

  for i := aScrollOffs to Length(aExpColumns)-1 do begin
    startCol.Logical := nextL;
    startCol.Physical := nextP;
    Markup.GetNextMarkupColAfterRowCol(aRow, startCol, rtl, nextP, nextL);

    AssertEquals(BaseTestName+' Correct pos for frame at '+IntToStr(i), aExpColumns[i], Max(nextP, nextL)); // Testing logigal or phys

    if i < Length(aExpColors) then begin
      startCol.Logical := nextL;
      startCol.Physical := nextP;
      gotColor := Markup.GetMarkupAttributeAtRowCol(aRow, startCol, rtl);
      AssertEquals(BaseTestName+' correct color for frame at idx '+IntToStr(i), aExpColors[i], gotColor.FrameColor);
    end;
  end;

  for i := 0 to Length(aExpWords) div 2 - 1 do begin
    startCol.Logical := nextL;
    startCol.Physical := nextP;
    Markup.GetNextMarkupColAfterRowCol(aRow, startCol, rtl, nextP, nextL);

    AssertEquals(BaseTestName+' Correct pos for word at '+IntToStr(i), aExpWords[i*2], Max(nextP, nextL));

    startCol.Logical := nextL;
    startCol.Physical := nextP;
    if i < Length(aExpWordsColor) then begin
      gotColor := Markup.GetMarkupAttributeAtRowCol(aRow, startCol, rtl);
      AssertEquals(BaseTestName+' correct color for word at idx '+IntToStr(i), aExpWordsColor[i], gotColor.Foreground);
    end;

// TODO: missing implementation for token end
//    Markup.GetNextMarkupColAfterRowCol(aRow, startCol, rtl, nextP, nextL);
//    AssertEquals(BaseTestName+' Correct pos for word end at '+IntToStr(i), aExpWords[i*2]+aExpWords[i*2+1], Max(nextP, nextL));
//    startCol.Logical := nextL;
//    startCol.Physical := nextP;
//    gotColor := Markup.GetMarkupAttributeAtRowCol(aRow, startCol, rtl);
//    AssertTrue(BaseTestName+' correct color after word at idx '+IntToStr(i), nil = gotColor);
  end;

  startCol.Logical := nextL;
  startCol.Physical := nextP;
  Markup.GetNextMarkupColAfterRowCol(aRow, startCol, rtl, nextP, nextL);
  AssertEquals(BaseTestName+' no more markup ', -1, Max(nextP, nextL));

  TestNoInvalidate;
  PopBaseName;
end;

procedure TTestMarkupFoldColoring.SetUp;
begin
  Markup := nil;
  inherited SetUp;
end;

procedure TTestMarkupFoldColoring.TearDown;
begin
  inherited TearDown;
//  FreeAndNil(Markup); // done by synedit
end;

procedure TTestMarkupFoldColoring.ReCreateEdit;
begin
  inherited ReCreateEdit;
  Markup := TSynEditMarkupFoldColors.Create(SynEdit);
  SynEdit.MarkupMgr.AddMarkUp(Markup);
  Markup.Lines := SynEdit.TextBuffer;
  Markup.InvalidateLinesMethod:=@CaptureTestInvalidateLines;
  Markup.Color[0].Foreground := 1;
  Markup.Color[1].Foreground := 2;
  Markup.Color[2].Foreground := 3;
  Markup.Color[3].Foreground := 4;
  Markup.Color[4].Foreground := 5;
  Markup.Color[5].Foreground := 6;
  //Markup.Highlighter := SynEdit.Highlighter;
end;

function TTestMarkupFoldColoring.TestText1: TStringArray;
begin
  SetLength(Result, 26);
  Result[ 0] := 'program Foo;';
  Result[ 1] := '';
  Result[ 2] := 'procedure a;';
  Result[ 3] := 'var';
  Result[ 4] := '  a: boolean;';
  Result[ 5] := '  procedure inner;';
  Result[ 6] := '    writeln(1)';
  Result[ 7] := '  end;';
  Result[ 8] := '';
  Result[ 9] := 'begin';
  Result[10] := '  if a then begin';
  Result[11] := '    writeln(2)';
  Result[12] := ' if b then';
  Result[13] := '';
  Result[14] := '   ;';
  Result[15] := '    writeln(3)';
  Result[16] := '//    writeln(4)';
  Result[17] := '    writeln(5)';
  Result[18] := '  end;';
  Result[19] := '';
  Result[20] := 'end;';
  Result[21] := '';
  Result[22] := 'begin';
  Result[23] := '';
  Result[24] := 'end.';
  Result[25] := '';
end;

procedure TTestMarkupFoldColoring.EnableOutlines(AEnbledTypes: TPascalCodeFoldBlockTypes);
var
  i: TPascalCodeFoldBlockType;
begin
  for i := low(TPascalCodeFoldBlockType) to high(TPascalCodeFoldBlockType) do begin
    PasHighLighter.FoldConfig[ord(i)].Enabled := i in AEnbledTypes;
    if (i in AEnbledTypes) then
      PasHighLighter.FoldConfig[ord(i)].Modes := PasHighLighter.FoldConfig[ord(i)].Modes + [fmOutline]
    else
      PasHighLighter.FoldConfig[ord(i)].Modes := PasHighLighter.FoldConfig[ord(i)].Modes - [fmOutline]
  end;
end;


procedure TTestMarkupFoldColoring.TestColors;
begin
  ReCreateEdit;

  PushBaseName('All folds');

  SetLines(TestText1);
  EnableFolds([cfbtBeginEnd.. cfbtNone], [cfbtSlashComment]);
  EnableOutlines([cfbtBeginEnd.. cfbtNone]);

  TestBeginMarkup('');
  TestRowColumns('Line  7',  7,  [1, 3], [1, 2]); //writeln 1
  TestRowColumns('Line 11', 11,  [1],    [2],      [3,2,  8,4,  13,5], [3, 3, 3]); // if a then begin
  TestRowColumns('Line 12', 12,  [1, 3], [2, 3]); //writeln 2
  TestRowColumns('Line 13', 13,  [1],    [2],      [2,2,  7,4], [4, 4]); // if b then begin
  TestRowColumns('Line 14', 14,  [1, 2], [2, 4]); //
  TestRowColumns('Line 15', 15,  [1, 2], [2, 4],   [4,1], [4]); // ;
  TestRowColumns('Line 16', 16,  [1, 3], [2, 3]); //writeln 3
  TestRowColumns('Line 17', 17,  [], []);    // //writeln 4
  TestRowColumns('Line 18', 18,  [1, 3], [2, 3]); //writeln 5
  Markup.EndMarkup;

  TestBeginMarkup('');
  TestRowColumns('Line  7 scrolled 1',  7, [3], [2], 1); //writeln
  Markup.EndMarkup;

  PushBaseName('edit');
    SetSynEditHeight(40);
    ClearInvalidatedLines;
    DoKeyPressAtPos(1, 13, [VK_SPACE, VK_SPACE]); // if b then
    TestInvalidate('after edit', 14, 21); // what about 13, ok it is already invalidated....
  PopBaseName;

  PopBaseName;
end;

initialization

  RegisterTest(TTestMarkupFoldColoring);
end.

