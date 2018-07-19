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
    FOnlyTestVisibleRows: Boolean;
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
    procedure ReCreateEdit(AText: TStringArray = nil; AHeight: integer = 30; ATopLine: Integer = 1); reintroduce;
    function TestText1: TStringArray;
    function TestText2: TStringArray; // case indent
    function TestTextMultiLineIfIndent: TStringArray;
    function TestTextInval1: TStringArray;
    function TestTextScroll1: TStringArray;
    procedure EnableOutlines(AEnbledTypes: TPascalCodeFoldBlockTypes);
  published
    procedure TestColors;
    procedure TestCaseLabelIndent; // issue https://bugs.freepascal.org/view.php?id=33154
    procedure TestMultiLineIfIndent; // issue https://bugs.freepascal.org/view.php?id=32852
    procedure TestInvalidateIfElseChain;
    procedure TestInvalidateScroll;
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
  i, nextP, nextL, srow: Integer;
  rtl: TLazSynDisplayRtlInfo;
  startCol: TLazSynDisplayTokenBound;
  gotColor: TSynSelectedColor;
begin
  if FOnlyTestVisibleRows then begin
    srow := SynEdit.RowToScreenRow(aRow);
    if (srow < 0) or (srow > SynEdit.LinesInWindow) then
      exit;
// TODO: verify next 2 lines
//    if (aRow > 1) and (srow = SynEdit.RowToScreenRow(aRow-1)) then // in fold
//      exit;
  end;
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

    AddErrorTestEqual(BaseTestName+' Correct pos for frame at '+IntToStr(i), aExpColumns[i], Max(nextP, nextL)); // Testing logigal or phys

    if i < Length(aExpColors) then begin
      startCol.Logical := nextL;
      startCol.Physical := nextP;
      gotColor := Markup.GetMarkupAttributeAtRowCol(aRow, startCol, rtl);
      if AddErrorTestTrue(BaseTestName+' color not nil for frame at idx '+IntToStr(i), gotColor <> nil) then
        AddErrorTestEqual(BaseTestName+' correct color for frame at idx '+IntToStr(i), aExpColors[i], gotColor.FrameColor);
    end;
  end;

  for i := 0 to Length(aExpWords) div 2 - 1 do begin
    startCol.Logical := nextL;
    startCol.Physical := nextP;
    Markup.GetNextMarkupColAfterRowCol(aRow, startCol, rtl, nextP, nextL);

    AddErrorTestEqual(BaseTestName+' Correct pos for word at '+IntToStr(i), aExpWords[i*2], Max(nextP, nextL));

    startCol.Logical := nextL;
    startCol.Physical := nextP;
    if i < Length(aExpWordsColor) then begin
      gotColor := Markup.GetMarkupAttributeAtRowCol(aRow, startCol, rtl);
      if AddErrorTestTrue(BaseTestName+' color <> nil for word at idx '+IntToStr(i), gotColor <> nil) then
        AddErrorTestEqual(BaseTestName+' correct color for word at idx '+IntToStr(i), aExpWordsColor[i], gotColor.Foreground);
    end;

// TODO: missing implementation for token end
//    Markup.GetNextMarkupColAfterRowCol(aRow, startCol, rtl, nextP, nextL);
//    AddErrorTestEqual(BaseTestName+' Correct pos for word end at '+IntToStr(i), aExpWords[i*2]+aExpWords[i*2+1], Max(nextP, nextL));
//    startCol.Logical := nextL;
//    startCol.Physical := nextP;
//    gotColor := Markup.GetMarkupAttributeAtRowCol(aRow, startCol, rtl);
//    AddErrorTestTrue(BaseTestName+' correct color after word at idx '+IntToStr(i), nil = gotColor);
  end;

  startCol.Logical := nextL;
  startCol.Physical := nextP;
  Markup.GetNextMarkupColAfterRowCol(aRow, startCol, rtl, nextP, nextL);
  AddErrorTestEqual(BaseTestName+' no more markup ', -1, Max(nextP, nextL));

  MaybeThrowError;
  TestNoInvalidate;
  PopBaseName;
end;

procedure TTestMarkupFoldColoring.SetUp;
begin
  Markup := nil;
  inherited SetUp;
  FOnlyTestVisibleRows := False;
end;

procedure TTestMarkupFoldColoring.TearDown;
begin
  inherited TearDown;
//  FreeAndNil(Markup); // done by synedit
end;

procedure TTestMarkupFoldColoring.ReCreateEdit(AText: TStringArray;
  AHeight: integer; ATopLine: Integer);
begin
  inherited ReCreateEdit;
  SetSynEditHeight(AHeight);
  SetLines(AText);
  SynEdit.TopLine := ATopLine;
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

function TTestMarkupFoldColoring.TestText2: TStringArray;
begin
  SetLength(Result, 20);
  Result[0] := 'program a;';
  Result[1] := 'procedure TEditorFrame.NotifChanged(Sender: TObject);';
  Result[2] := 'begin';
  Result[3] := '  //silent reload if: not modified, and undo empty';
  Result[4] := '  if (not Modified) and (Ed1.UndoCount<=1) then';
  Result[5] := '  begin';
  Result[6] := '    DoFileReload;';
  Result[7] := '    exit';
  Result[8] := '  end;';
  Result[9] := '';
  Result[10] := '  case MsgBox(msgConfirmFileChangedOutside+0000010+FileName+';
  Result[11] := '         0000010#10+msgConfirmReloadIt+0000010+msgConfirmReloadItHotkeys,';
  Result[12] := '         MB_YESNOCANCEL or MB_ICONQUESTION) of';
  Result[13] := '    ID_YES:';
  Result[14] := '      DoFileReload;';
  Result[15] := '    ID_CANCEL:';
  Result[16] := '      NotifEnabled:= false;';
  Result[17] := '  end;';
  Result[18] := 'end;';
end;

function TTestMarkupFoldColoring.TestTextMultiLineIfIndent: TStringArray;
begin
  SetLength(Result, 29);
  Result[ 0] := 'program a;';
  Result[ 1] := 'procedure foo;';
  Result[ 2] := 'begin';
  Result[ 3] := '  if condition1 or';
  Result[ 4] := '     condition2 then';
  Result[ 5] := '    Exit;';
  Result[ 6] := '';
  Result[ 7] := '  if condition1 or';
  Result[ 8] := '     condition2 then';
  Result[ 9] := '  begin';
  Result[10] := '';
  Result[11] := '  end;';
  Result[12] := '';
  Result[13] := '  if condition1 or';
  Result[14] := '     condition2 then';
  Result[15] := '     begin';
  Result[16] := '';
  Result[17] := '     end;';
  Result[18] := '';
  Result[19] := '  if condition then'; // comment 104823 // issue 32852
  Result[20] := '    begin';
  Result[21] := '      code;';
  Result[22] := '    end';
  Result[23] := '  else';
  Result[24] := '    begin';
  Result[25] := '      code;';
  Result[26] := '    end;';
  Result[27] := '';
  Result[28] := 'end;';
end;

function TTestMarkupFoldColoring.TestTextInval1: TStringArray;
begin
  SetLength(Result, 20);
  Result[0] := 'procedure';
  Result[1] := 'begin';
  Result[2] := '';
  Result[3] := '';
  Result[4] := '    if x then';
  Result[5] := '      code';
  Result[6] := '';
  Result[7] := '';
  Result[8] := '    else if y then';
  Result[9] := '      code';
  Result[10] := '';
  Result[11] := '';
  Result[12] := '    else';
  Result[13] := '      code';
  Result[14] := '';
  Result[15] := '';
  Result[16] := '      ;';
  Result[17] := '';
  Result[18] := 'end';
  Result[19] := '';
end;

function TTestMarkupFoldColoring.TestTextScroll1: TStringArray;
var
  i: Integer;
begin
  SetLength(Result, 112);
  Result[0] := 'unit TestUnit;';
  Result[1] := '';
  Result[2] := '{$mode objfpc}{$H+}';
  Result[3] := '';
  Result[4] := 'interface';
  Result[5] := '';
  Result[6] := 'uses';
  Result[7] := '  Classes, SysUtils;';
  Result[8] := '';
  Result[9] := 'implementation';
  Result[10] := '';
  Result[11] := 'procedure Test;';
  Result[12] := 'begin';
  Result[13] := '  if condition then';
  Result[14] := '      begin';
  Result[15] := '        code;';
  Result[16] := '      end';
  Result[17] := '    else';
  Result[18] := '      begin';
  Result[19] := '        code;';
  Result[20] := '      end;';
  Result[21] := '';
  Result[22] := '  if condition1 or';
  Result[23] := '     condition2 then';
  Result[24] := '    begin';
  Result[25] := '      code;';
  Result[26] := '    end';
  Result[27] := '  else';
  Result[28] := '    begin';
  Result[29] := '      code;';
  Result[30] := '    end;';
  for i := 31 to 99 do
    Result[i] := '';
  Result[100] := '  if condition1 or';
  Result[101] := '     condition2 then';
  Result[102] := '    begin';
  Result[103] := '      code;';
  Result[104] := '    end';
  Result[105] := '  else';
  Result[106] := '    begin';
  Result[107] := '      code;';
  Result[108] := '    end;';
  Result[109] := 'end;';
  Result[110] := '';
  Result[111] := 'end.';
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

procedure TTestMarkupFoldColoring.TestCaseLabelIndent;
begin
  ReCreateEdit(TestText2);
  EnableFolds([cfbtBeginEnd.. cfbtNone], [cfbtSlashComment]);
  EnableOutlines([cfbtBeginEnd.. cfbtNone]);
  PushBaseName('case label indent');

  TestBeginMarkup('');
  TestRowColumns('Line  1',  1, [],     []);
  TestRowColumns('Line  2',  2, [],     []);
  TestRowColumns('Line  3',  3, [],     [],      [1,5], [1]);
  TestRowColumns('Line  4',  4, [1],    [1]);
  TestRowColumns('Line  5',  5, [1],    [1],     [3,2,  44,4], [2, 2]);
  TestRowColumns('Line  6',  6, [1],    [1],     [3,5], [2]);
  TestRowColumns('Line  7',  7, [1, 3], [1, 2]);
  TestRowColumns('Line  8',  8, [1, 3], [1, 2]);
  TestRowColumns('Line  9',  9, [1],    [1],     [3,3, 6,1], [2, 2]); // or merged [3,4]
  TestRowColumns('Line 10', 10, [1],    [1]);
  TestRowColumns('Line 11', 11, [1],    [1],     [3,4], [2]);  // case
  TestRowColumns('Line 12', 12, [1, 3], [1, 2]);
  TestRowColumns('Line 13', 13, [1, 3], [1, 2],  [45,2], [2]); // of
  TestRowColumns('Line 14', 14, [1, 3], [1, 2]);
  TestRowColumns('Line 15', 15, [1, 3], [1, 2]);
  TestRowColumns('Line 16', 16, [1, 3], [1, 2]);
  TestRowColumns('Line 17', 17, [1, 3], [1, 2]);
  TestRowColumns('Line 18', 18, [1],    [1],     [3,3], [2]); // end
  TestRowColumns('Line 19', 19, [],     [],      [1,3], [1]);
  Markup.EndMarkup;

  PopBaseName;
end;

procedure TTestMarkupFoldColoring.TestMultiLineIfIndent;
begin
  ReCreateEdit(TestTextMultiLineIfIndent);
  EnableFolds([cfbtBeginEnd.. cfbtNone], [cfbtSlashComment]);
  EnableOutlines([cfbtBeginEnd.. cfbtNone]);
  PushBaseName('if indent');

  TestBeginMarkup('');
  TestRowColumns('Line  1',  1, [],     []);
  TestRowColumns('Line  2',  2, [],     []);
  TestRowColumns('Line  3',  3, [],     [],      [1,5], [1]);
  TestRowColumns('Line  4',  4, [1],    [1],     [3,2], [2]); // if
  TestRowColumns('Line  5',  5, [1, 3], [1, 2],  [17,4], [2]); // then
  TestRowColumns('Line  6',  6, [1, 3], [1, 2],  [9,1], [2]);
  TestRowColumns('Line  7',  7, [1],    [1]);

  TestRowColumns('Line  8',  8, [1],    [1],     [3,2], [2]); // if
  TestRowColumns('Line  9',  9, [1, 3], [1, 2],  [17,4], [2]); // then
  TestRowColumns('Line 10', 10, [1],    [1],     [3,5], [2]); // begin
  TestRowColumns('Line 11', 11, [1, 3], [1, 2]);
  TestRowColumns('Line 12', 12, [1],    [1],     [3,3, 6,1], [2, 2]); // end;
  TestRowColumns('Line 13', 13, [1],    [1]);

  TestRowColumns('Line 14', 14, [1],    [1],     [3,2], [2]); // if
  TestRowColumns('Line 15', 15, [1, 3], [1, 2],  [17,4], [2]); // then
  TestRowColumns('Line 16', 16, [1, 3], [1, 2],  [6,5], [2]); // begin
  TestRowColumns('Line 17', 17, [1, 3], [1, 2]);
  TestRowColumns('Line 18', 18, [1, 3], [1, 2],  [6,3, 9,1], [2, 2]); // end
  TestRowColumns('Line 19', 19, [1],    [1]);

  TestRowColumns('Line 20', 20, [1],    [1],     [3,2, 16,4], [2, 2]); // if then
  TestRowColumns('Line 21', 21, [1, 3], [1, 2],  [5,5], [2]); // begin
  TestRowColumns('Line 22', 22, [1, 3], [1, 2]);
  TestRowColumns('Line 23', 23, [1, 3], [1, 2],  [5,3], [2]); // end
  TestRowColumns('Line 24', 24, [1],    [1],     [3,4], [2]); // else
  TestRowColumns('Line 25', 25, [1, 3], [1, 2],  [5,5], [2]); // begin
  TestRowColumns('Line 26', 26, [1, 3], [1, 2]);
  TestRowColumns('Line 27', 27, [1, 3], [1, 2],  [5,3], [2]); // end // what about ";"
  TestRowColumns('Line 28', 28, [1],    [1]);

  TestRowColumns('Line 29', 29, [],     [],      [1,3], [1]);
  Markup.EndMarkup;

  PopBaseName;
end;

procedure TTestMarkupFoldColoring.TestInvalidateIfElseChain;
var
  i: Integer;
begin
  ReCreateEdit;
  PushBaseName('Invalidate, if else if else ...');
    SetLines(TestTextInval1);
    EnableFolds([cfbtBeginEnd.. cfbtNone], [cfbtSlashComment]);
    EnableOutlines([cfbtIfThen.. cfbtNone]);
    SetSynEditHeight(40);

    PushBaseName('before edit');
    TestBeginMarkup('');
    for i := 1 to 19 do
    case i of
      1..4, 18..19:
        TestRowColumns('Line '+IntToStr(i),  i, [], []);
      5:
        TestRowColumns('Line '+IntToStr(i),  i, [], [], [5,2, 10,4], [1, 1]);
      9:
        TestRowColumns('Line '+IntToStr(i),  i, [], [], [5,4, 10,2, 15,4], [1, 1, 1]);
      13:
        TestRowColumns('Line '+IntToStr(i),  i, [], [], [5,4], [1]);
      17:
        TestRowColumns('Line '+IntToStr(i),  i, [5], [1], [7,1], [1]);
      else
        TestRowColumns('Line '+IntToStr(i),  i, [5], [1]);
    end;
    Markup.EndMarkup;

    ClearInvalidatedLines;
    DoKeyPressAtPos(2, 5, [VK_DELETE]); // if b then

    PopPushBaseName('after edit');
    TestInvalidate('after edit', 6, 17);

    TestBeginMarkup('');
    for i := 1 to 19 do
    case i of
      1..4, 18..19:
        TestRowColumns('Line '+IntToStr(i),  i, [], []);
      5:
        TestRowColumns('Line '+IntToStr(i),  i, [], [], [4,2, 9,4], [1, 1]);
      9:
        TestRowColumns('Line '+IntToStr(i),  i, [4], [1], [5,4, 10,2, 15,4], [1, 1, 1]);
      13:
        TestRowColumns('Line '+IntToStr(i),  i, [4], [1], [5,4], [1]);
      17:
        TestRowColumns('Line '+IntToStr(i),  i, [4], [1], [7,1], [1]);
      else
        TestRowColumns('Line '+IntToStr(i),  i, [4], [1]);
    end;
    Markup.EndMarkup;

    PopBaseName;
  PopBaseName;
end;

procedure TTestMarkupFoldColoring.TestInvalidateScroll;
  procedure SubTestScroll1;
  var
    i: Integer;
  begin
    FOnlyTestVisibleRows := True;
    TestBeginMarkup('');
    for i := 1 to 112 do
    case i of
      1..6, 9..12:
          TestRowColumns('Line '+IntToStr(i),  i, [],     []);
      7:  TestRowColumns('Line '+IntToStr(i),  i, [],     [],      [1,4], [1]);
      8:  TestRowColumns('Line '+IntToStr(i),  i, [1],    [1],     [20,1], [1]);  // ";"

      13: TestRowColumns('Line '+IntToStr(i),  i, [],     [],      [1,5], [1]);
      14: TestRowColumns('Line '+IntToStr(i),  i, [1],    [1],     [3,2,  16,4], [2, 2]); // if then
      15: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [7,5], [2]);
      16: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2]);
      17: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [7,3], [2]);
      18: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [5,4], [2]);  // else
      19: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [7,5], [2]);
      20: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2]);
      21: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [7,3], [2]); // should the ";" be marked too?

      23: TestRowColumns('Line '+IntToStr(i),  i, [1],    [1],     [3,2], [2]); // if  or
      24: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [17,4], [2]); //   then
      25: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [5,5], [2]);
      26: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2]);
      27: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [5,3], [2]);
      28: TestRowColumns('Line '+IntToStr(i),  i, [1],    [1],     [3,4], [2]);  // else
      29: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [5,5], [2]);
      30: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2]);
      31: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [5,3], [2]); // should the ";" be marked too?

      101: TestRowColumns('Line '+IntToStr(i),  i, [1],    [1],     [3,2], [2]); // if  or
      102: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [17,4], [2]); //   then
      103: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [5,5], [2]);
      104: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2]);
      105: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [5,3], [2]);
      106: TestRowColumns('Line '+IntToStr(i),  i, [1],    [1],     [3,4], [2]);  // else
      107: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [5,5], [2]);
      108: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2]);
      109: TestRowColumns('Line '+IntToStr(i),  i, [1, 3], [1, 2],  [5,3], [2]); // should the ";" be marked too?
      110: TestRowColumns('Line '+IntToStr(i),  i, [],     [],      [1,3], [1]);
      111..112:
           TestRowColumns('Line '+IntToStr(i),  i, [],     []);
      else
           TestRowColumns('Line '+IntToStr(i),  i, [1],    [1]); // only 1 line for outer "begin"
    end;
    Markup.EndMarkup;
  end;
var
  i: Integer;
begin
  PushBaseName('scroll');

    ReCreateEdit(TestTextScroll1, 35, 90);
    EnableFolds([cfbtBeginEnd.. cfbtNone], [cfbtSlashComment]);
    EnableOutlines([cfbtBeginEnd.. cfbtNone]);

    PushBaseName('before scroll');
    SubTestScroll1;

    for i := 89 downto 1 do begin
      SynEdit.TopLine := i;
      ClearInvalidatedLines;
      PopPushBaseName('after scroll '+IntToStr(i));
      SubTestScroll1;
    end;

    PopBaseName;

  PopBaseName;
end;

initialization

  RegisterTest(TTestMarkupFoldColoring);
end.

