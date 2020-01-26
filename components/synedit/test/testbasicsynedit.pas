unit TestBasicSynEdit;

(* TODO:
   - TestEditEmpty:
     Test with different sets of VirtualViews (with/without trimming (enabled/module present at all)
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, LCLProc, LCLType, Forms, TestBase, SynEdit,
  SynEditTextTrimmer, SynEditKeyCmds, LazSynEditText, SynEditPointClasses,
  SynEditMiscClasses, SynEditTypes, SynEditMiscProcs;

type

  { TTestBasicSynEdit }

  TTestBasicSynEdit = class(TTestBase)
  private
    InsertFlag: Boolean;
    TrimType: TSynEditStringTrimmingType;
    TrimEnabled: Boolean;
  private
    AllowPastEOL, FPersistSel: Boolean;
    FDoInit: procedure of object;
    FTestLines: function: TStringArray of object;
    Procedure DoInit1;
    function TestLines1: TStringArray;
    function TestLines2: TStringArray;
  private
    FTestCommand:TSynEditorCommand;
    procedure TestCommand(Name:String; X, Y: Integer;
                           ExpX1, ExpY1: Integer; Repl: Array of const);
    procedure TestCommand(Name:String; X, Y, X2, Y2: Integer;
                           ExpX1, ExpY1, ExpX2, ExpY2: Integer;
                           Repl: Array of const);
    procedure TestCommand(Name:String; X, Y: Integer;
                           ExpX1, ExpY1: Integer; Repl: Array of const;
                           ExpX2, ExpY2: Integer; Repl2: Array of const);
  protected
    function TestMaxLeftProc: Integer;
    procedure ReCreateEdit; reintroduce;
  published
    procedure TestEditEmpty;
    procedure TestEditTabs;
    procedure TestEditEcChar;
    procedure TestPhysicalLogical; // TODO adjust to char tests
    procedure TestLogicalAdjust;
    procedure TestCaretObject;
    procedure TestCaretAutoMove;
    procedure TestCaretEcDeleteWord;
    procedure TestCaretEcDeleteLastWord;
    procedure TestCaretEcDeleteEOL;
    procedure TestWordBreaker;
    procedure TestSearchReplace;
    procedure TestMoveLineUpDown;
  end;

implementation

procedure TTestBasicSynEdit.DoInit1;
begin
  InsertFlag := False;
  TrimEnabled := False;;
  ReCreateEdit;
  if AllowPastEOL
  then SynEdit.Options := SynEdit.Options + [eoScrollPastEol]
  else SynEdit.Options := SynEdit.Options - [eoScrollPastEol];
  if FPersistSel
  then SynEdit.Options2 := SynEdit.Options2 + [eoPersistentBlock]
  else SynEdit.Options2 := SynEdit.Options2 - [eoPersistentBlock];
  SynEdit.TabWidth := 7;
  SetLines(FTestLines());
end;

function TTestBasicSynEdit.TestLines1: TStringArray;
begin
  SetLength(Result, 16);
              // 1    6    11 14
  Result[0]  := 'Some text to test';                 //1
              // 1   5   9
  Result[1]  := 'Foo bar abc';                       // 2
              //   8     14   19   24
  Result[2]  := #9'Other line with tab';             // 3
              // 1      8  11  15
  Result[3]  := 'tab'#9'in the middle';              // 4
              // 1       9  12  16     23   28
  Result[4]  := 'tab'#9' in the middle with space';  // 5
              // 1       9   13 16
  Result[5]  := 'umlaute äää in text';               // 6
  Result[6]  := 'normal line';
              //    4         14     21     28  32
  Result[7]  := '   untrimmed spaces around line   '; // 8
  Result[8]  := 'normal line';
              //   8      15      22  26
  Result[9]  := #9'tab'#9'only'#9'line'#9;            // 10
              // 1      8
  Result[10] := 'normal line';
  Result[11] := '';                                    // 12 (empty)
  Result[12] := 'normal line';
  Result[13] := '     '; // space only empty line      // 14
  Result[14] := 'normal line';
  Result[15] := '';
end;

function TTestBasicSynEdit.TestLines2: TStringArray;
begin
  SetLength(Result, 7);
              // 1    6    11 14
  Result[0]  := 'abc def ghi'; // 1
  Result[1]  := 'ABC DEF GHI'; // 2
  Result[2]  := '123 456 789'; // 3
  Result[3]  := 'mno pqr stu'; // 4
  Result[4]  := 'MNO PQR STU';  //5
  Result[5]  := 'xyz XYZ vw';
  Result[6] := '';
end;

procedure TTestBasicSynEdit.TestCommand(Name: String; X, Y: Integer; ExpX1,
  ExpY1: Integer; Repl: array of const);
begin
  FDoInit();
  SetCaretPhys(X,Y);
  SynEdit.CommandProcessor(FTestCommand, '', nil);
  TestIsCaretPhys(Name + '(1st)', ExpX1, ExpY1);
  TestIsFullText(Name +  '(1st)', FTestLines(), Repl);
end;

procedure TTestBasicSynEdit.TestCommand(Name: String; X, Y, X2, Y2: Integer;
  ExpX1, ExpY1, ExpX2, ExpY2: Integer; Repl: array of const);
begin
  FDoInit();
  SetCaretAndSel(X,Y, x2,y2);
  SynEdit.CommandProcessor(FTestCommand, '', nil);
  TestIsCaretAndSel(Name + '(1st)', ExpX1, ExpY1, ExpX2, ExpY2);
  TestIsFullText(Name +  '(1st)', FTestLines(), Repl);
end;

procedure TTestBasicSynEdit.TestCommand(Name: String; X, Y: Integer; ExpX1,
  ExpY1: Integer; Repl: array of const; ExpX2, ExpY2: Integer;
  Repl2: array of const);
begin
    TestCommand(Name, X, Y, ExpX1, ExpY1, Repl);
    SynEdit.CommandProcessor(FTestCommand, '', nil);
    TestIsCaretPhys(Name + '(2nd)', ExpX2, ExpY2);
    TestIsFullText(Name +  '(2nd)', FTestLines(), Repl2);
end;

function TTestBasicSynEdit.TestMaxLeftProc: Integer;
begin
  Result := 6000;
end;

procedure TTestBasicSynEdit.ReCreateEdit;
begin
  inherited ReCreateEdit;
  SynEdit.InsertMode := InsertFlag;
  SynEdit.TrimSpaceType := TrimType;
  if TrimEnabled then
    SynEdit.Options := SynEdit.Options + [eoTrimTrailingSpaces]
  else
    SynEdit.Options := SynEdit.Options - [eoTrimTrailingSpaces];

end;

procedure TTestBasicSynEdit.TestEditEmpty;
  procedure CheckText(aName: String; ExpText: String; ExpLines: Integer);
  var
    s: String;
  begin
    AssertEquals(BaseTestName + aName+' Count', ExpLines, SynEdit.Lines.Count);
    // TestIsText (without Views => just real text)
    // TestIsFullText (with Views => eg trimmed spaces)
    s:='';
    if ExpLines > 0 then
      s := LineEnding;
    TestIsText(aName+' Text', ExpText+s);
  end;
  procedure DoChecks;
  begin
    ReCreateEdit;
    CheckText('Empty', '', 0);
    SynEdit.CommandProcessor(ecChar, 'x', nil);
    CheckText('After Insert "x"', 'x', 1);

    ReCreateEdit;
    SynEdit.CommandProcessor(ecChar, ' ', nil);
    if TrimEnabled then begin
      CheckText('After Insert <space>', '', 1);
      if TrimType = settIgnoreAll then begin
        TestIsFullText('After Insert <space> (FullText)', ''+LineEnding);
        TestIsCaret('After Insert <space>', 2,1);
      end else begin
        TestIsFullText('After Insert <space> (FullText)', ' '+LineEnding);
        TestIsCaret('After Insert <space>', 2,1);
      end;
    end else begin
      CheckText('After Insert <space>', ' ', 1);
      TestIsFullText('After Insert <space> (FullText)', ' '+LineEnding);
      TestIsCaret('After Insert <space>', 2,1);
    end;

    ReCreateEdit;
    CheckText('Empty', '', 0);
    SynEdit.CommandProcessor(ecDeleteChar, '', nil);
    CheckText('After ecDeleteChar', '', 0);

    ReCreateEdit;
    SynEdit.CommandProcessor(ecDeleteLastChar, '', nil);
    CheckText('After ecDeleteLastChar', '', 0);

    ReCreateEdit;
    SynEdit.CommandProcessor(ecDeleteWord, '', nil);
    CheckText('After ecDeleteWord', '', 0);

    ReCreateEdit;
    SynEdit.CommandProcessor(ecDeleteLastWord, '', nil);
    CheckText('After ecDeleteLastWord', '', 0);

    ReCreateEdit;
    SynEdit.CommandProcessor(ecInsertLine, '', nil);
    CheckText('After ecInsertLine', LineEnding, 2);

    ReCreateEdit;
    SynEdit.CommandProcessor(ecLineBreak, '', nil);
    CheckText('After ecLineBreak', LineEnding, 2);

  end;
begin
  FPersistSel := False;
  TrimEnabled := True;
  TrimType := settEditLine;
  PushBaseName('Trim=EditLine');
    PushBaseName('InsertMode');
      InsertFlag := True;
      DoChecks;
    PopPushBaseName('OverwriteMode');
      InsertFlag := False;
      DoChecks;
    PopBaseName;

  TrimType := settIgnoreAll;
  PopPushBaseName('Trim=IgnoreAll');
    PushBaseName('InsertMode');
      InsertFlag := True;
      DoChecks;
    PopPushBaseName('OverwriteMode');
      InsertFlag := False;
      DoChecks;
    PopBaseName;

  TrimEnabled := False;
  PopPushBaseName('Trim=Disabled');
    PushBaseName('InsertMode');
      InsertFlag := True;
      DoChecks;
    PopPushBaseName('OverwriteMode');
      InsertFlag := False;
      DoChecks;
    PopBaseName;

end;

procedure TTestBasicSynEdit.TestEditTabs;
begin
  FPersistSel := False;
  ReCreateEdit;
  // witout eoAutoIndent
  SynEdit.Options  := SynEdit.Options
                    - [eoTabIndent, eoTabsToSpaces, eoSpacesToTabs, eoAutoIndent, eoSmartTabs, eoSmartTabDelete];
  SynEdit.TabWidth := 4;
  SetLines(['  abc', #9'abcde', '']);
  SetCaret(2, 2); // after tab
  TestIsCaretPhys('Before delete tab', 5, 2);
  SynEdit.CommandProcessor(ecDeleteLastChar, '', nil);

  TestIsCaret('After delete tab', 1, 2);
  TestIsCaretPhys('After delete tab', 1, 2);
  TestIsText('After delete tab', ['  abc', 'abcde', '']);

  ReCreateEdit;
  // with eoAutoIndent
  SynEdit.Options  := SynEdit.Options + [eoSmartTabs, eoSmartTabDelete, eoAutoIndent]
                    - [eoTabIndent, eoTabsToSpaces, eoSpacesToTabs];
  SynEdit.TabWidth := 4;
  SetLines(['  abc', #9'abcde', '']);
  SetCaret(2, 2); // after tab
  TestIsCaretPhys('Before delete tab', 5, 2);
  SynEdit.CommandProcessor(ecDeleteLastChar, '', nil);

  // reuqired indent is filled up with spaces
  TestIsCaret('After delete tab (smart)', 3, 2);
  TestIsCaretPhys('After delete tab (smart)', 3, 2);
  TestIsText('After delete tab (smart)', ['  abc', '  abcde', '']);

end;

procedure TTestBasicSynEdit.TestEditEcChar;
var
  CaretPEol: Boolean;

  function TestText1: TStringArray;
  begin
    SetLength(Result, 4);
    Result[0] := 'abc';
    Result[1] := 'öbc';
    Result[2] := #9'abc';
    Result[3] := '';
  end;
  procedure InitEdit;
  begin
    ReCreateEdit;
    SynEdit.Options := [];
    if CaretPEol then SynEdit.Options := SynEdit.Options + [eoScrollPastEol];
    SynEdit.TabWidth := 6;
    SetLines(TestText1);
  end;

  // All Logical pos
  Procedure DoTest(ATestName: String; StartX, StartY: Integer;
                   Char1: String; ExpX1, ExpY1: Integer; Repl1: Array of const;
                   Char2: String; ExpX2, ExpY2: Integer; Repl2: Array of const;
                   AStartIsPhys: Boolean = False
                  );
  begin
    BaseTestName := Format('%s -Ins=%s - PastEol=%s - StartXY=(%d,%d) - ',
                           [ATestName, dbgs(InsertFlag), dbgs(CaretPEol), StartX, StartY]);
    InitEdit;

    if AStartIsPhys
    then SetCaretPhys(StartX, StartY)
    else SetCaret(StartX, StartY);

    SynEdit.TestTypeText(Char1);
    TestIsCaret('After Char 1', ExpX1, ExpY1);
    TestIsText ('After Char 1', TestText1, Repl1);

    SynEdit.TestTypeText(Char2);
    TestIsCaret('After Char 2', ExpX2, ExpY2);
    TestIsText ('After Char 2', TestText1, Repl2);

    SynEdit.Undo;
    TestIsCaret('Undo 1', ExpX1, ExpY1);
    TestIsText ('Undo 1', TestText1, Repl1);

    SynEdit.Redo;
    TestIsCaret('Redo 1', ExpX2, ExpY2);
    TestIsText ('Redo 1', TestText1, Repl2);

    SynEdit.Undo;
    TestIsCaret('Undo 2a', ExpX1, ExpY1);
    TestIsText ('Undo 2a', TestText1, Repl1);

    SynEdit.Undo;
    if AStartIsPhys
    then TestIsCaretPhys('Undo 2b', StartX, StartY)
    else TestIsCaret('Undo 2b', StartX, StartY);
    TestIsText ('Undo 2b', TestText1);

    SynEdit.Redo;
    TestIsCaret('Redo 2a', ExpX1, ExpY1);
    TestIsText ('Redo 2a', TestText1, Repl1);

    SynEdit.Redo;
    TestIsCaret('Redo 2b', ExpX2, ExpY2);
    TestIsText ('Redo 2b', TestText1, Repl2);

    InitEdit;

    if AStartIsPhys
    then SetCaretPhys(StartX, StartY)
    else SetCaret(StartX, StartY);
    SynEdit.TestTypeText(Char1);
    SynEdit.TestTypeText(Char2);
    TestIsCaret('After Char 1+2', ExpX2, ExpY2);
    TestIsText ('After Char 1+2', TestText1, Repl2);

  end;
begin
  FPersistSel := False;
  TrimEnabled := False; // Trim has its own test

  // Testing ecChar. Tab is ecTab, so not included

  {%region  Normal Line}
    {%region  Normal Line -- Normal Char}
    InsertFlag := True;
    CaretPEol := False;
    DoTest('normal line - BOL',      1, 1,     'X', 2, 1, [1,'Xabc'],    'Y', 3,1, [1,'XYabc']);
    DoTest('normal line - mid',      2, 1,     'X', 3, 1, [1,'aXbc'],    'Y', 4,1, [1,'aXYbc']);
    DoTest('normal line - EOL',      4, 1,     'X', 5, 1, [1,'abcX'],    'Y', 6,1, [1,'abcXY']);
    CaretPEol := True;
    DoTest('normal line - EOL',      4, 1,     'X', 5, 1, [1,'abcX'],    'Y', 6,1, [1,'abcXY']);
    DoTest('normal line - Past',     6, 1,     'X', 7, 1, [1,'abc  X'],  'Y', 8,1, [1,'abc  XY']);

    InsertFlag := False;
    CaretPEol := False;
    DoTest('normal line - BOL',      1, 1,     'X', 2, 1, [1,'Xbc'],     'Y', 3,1, [1,'XYc']);
    DoTest('normal line - mid',      2, 1,     'X', 3, 1, [1,'aXc'],     'Y', 4,1, [1,'aXY']);
    DoTest('normal line - mid',      3, 1,     'X', 4, 1, [1,'abX'],     'Y', 5,1, [1,'abXY']);
    DoTest('normal line - EOL',      4, 1,     'X', 5, 1, [1,'abcX'],    'Y', 6,1, [1,'abcXY']);
    CaretPEol := True;
    DoTest('normal line - EOL',      4, 1,     'X', 5, 1, [1,'abcX'],    'Y', 6,1, [1,'abcXY']);
    DoTest('normal line - Past',     6, 1,     'X', 7, 1, [1,'abc  X'],  'Y', 8,1, [1,'abc  XY']);
    {%endregion}

    {%region  Normal Line -- Space (and char)}
    InsertFlag := True;
    CaretPEol := False;
    DoTest('normal line - space,char - BOL',      1, 1,     ' ', 2, 1, [1,' abc'],    'Y', 3,1, [1,' Yabc']);
    DoTest('normal line - space,char - mid',      2, 1,     ' ', 3, 1, [1,'a bc'],    'Y', 4,1, [1,'a Ybc']);
    DoTest('normal line - space,char - EOL',      4, 1,     ' ', 5, 1, [1,'abc '],    'Y', 6,1, [1,'abc Y']);
    CaretPEol := True;
    DoTest('normal line - space,char - EOL',      4, 1,     ' ', 5, 1, [1,'abc '],    'Y', 6,1, [1,'abc Y']);
    DoTest('normal line - space,char - Past',     6, 1,     ' ', 7, 1, [1,'abc   '],  'Y', 8,1, [1,'abc   Y']);

    InsertFlag := False;
    CaretPEol := False;
    DoTest('normal line - space,char - BOL',      1, 1,     ' ', 2, 1, [1,' bc'],     'Y', 3,1, [1,' Yc']);
    DoTest('normal line - space,char - mid',      2, 1,     ' ', 3, 1, [1,'a c'],     'Y', 4,1, [1,'a Y']);
    DoTest('normal line - space,char - mid',      3, 1,     ' ', 4, 1, [1,'ab '],     'Y', 5,1, [1,'ab Y']);
    DoTest('normal line - space,char - EOL',      4, 1,     ' ', 5, 1, [1,'abc '],    'Y', 6,1, [1,'abc Y']);
    CaretPEol := True;
    DoTest('normal line - space,char - EOL',      4, 1,     ' ', 5, 1, [1,'abc '],    'Y', 6,1, [1,'abc Y']);
    DoTest('normal line - space,char - Past',     6, 1,     ' ', 7, 1, [1,'abc   '],  'Y', 8,1, [1,'abc   Y']);
    {%endregion}

    {%region  Normal Line -- Space}
    InsertFlag := True;
    CaretPEol := False;
    DoTest('normal line - space - BOL',      1, 1,     ' ', 2, 1, [1,' abc'],    ' ', 3,1, [1,'  abc']);
    DoTest('normal line - space - mid',      2, 1,     ' ', 3, 1, [1,'a bc'],    ' ', 4,1, [1,'a  bc']);
    DoTest('normal line - space - EOL',      4, 1,     ' ', 5, 1, [1,'abc '],    ' ', 6,1, [1,'abc  ']);
    CaretPEol := True;
    DoTest('normal line - space - EOL',      4, 1,     ' ', 5, 1, [1,'abc '],    ' ', 6,1, [1,'abc  ']);
    DoTest('normal line - space - Past',     6, 1,     ' ', 7, 1, [1,'abc   '],  ' ', 8,1, [1,'abc    ']);

    InsertFlag := False;
    CaretPEol := False;
    DoTest('normal line - space - BOL',      1, 1,     ' ', 2, 1, [1,' bc'],     ' ', 3,1, [1,'  c']);
    DoTest('normal line - space - mid',      2, 1,     ' ', 3, 1, [1,'a c'],     ' ', 4,1, [1,'a  ']);
    DoTest('normal line - space - mid',      3, 1,     ' ', 4, 1, [1,'ab '],     ' ', 5,1, [1,'ab  ']);
    DoTest('normal line - space - EOL',      4, 1,     ' ', 5, 1, [1,'abc '],    ' ', 6,1, [1,'abc  ']);
    CaretPEol := True;
    DoTest('normal line - space - EOL',      4, 1,     ' ', 5, 1, [1,'abc '],    ' ', 6,1, [1,'abc  ']);
    DoTest('normal line - space - Past',     6, 1,     ' ', 7, 1, [1,'abc   '],  ' ', 8,1, [1,'abc    ']);
    {%endregion}

    {%region  Normal Line -- utf8 2 byte Char}
    InsertFlag := True;
    CaretPEol := False;
    DoTest('normal line - 2byte utf8 - BOL',      1, 1,     'Ä', 3, 1, [1,'Äabc'],    'Ü', 5,1, [1,'ÄÜabc']);
    DoTest('normal line - 2byte utf8 - mid',      2, 1,     'Ä', 4, 1, [1,'aÄbc'],    'Ü', 6,1, [1,'aÄÜbc']);
    DoTest('normal line - 2byte utf8 - EOL',      4, 1,     'Ä', 6, 1, [1,'abcÄ'],    'Ü', 8,1, [1,'abcÄÜ']);
    CaretPEol := True;
    DoTest('normal line - 2byte utf8 - EOL',      4, 1,     'Ä', 6, 1, [1,'abcÄ'],    'Ü', 8,1, [1,'abcÄÜ']);
    DoTest('normal line - 2byte utf8 - Past',     6, 1,     'Ä', 8, 1, [1,'abc  Ä'],  'Ü',10,1, [1,'abc  ÄÜ']);

    InsertFlag := False;
    CaretPEol := False;
    DoTest('normal line - 2byte utf8 - BOL',      1, 1,     'Ä', 3, 1, [1,'Äbc'],     'Ü', 5,1, [1,'ÄÜc']);
    DoTest('normal line - 2byte utf8 - mid',      2, 1,     'Ä', 4, 1, [1,'aÄc'],     'Ü', 6,1, [1,'aÄÜ']);
    DoTest('normal line - 2byte utf8 - mid',      3, 1,     'Ä', 5, 1, [1,'abÄ'],     'Ü', 7,1, [1,'abÄÜ']);
    DoTest('normal line - 2byte utf8 - EOL',      4, 1,     'Ä', 6, 1, [1,'abcÄ'],    'Ü', 8,1, [1,'abcÄÜ']);
    CaretPEol := True;
    DoTest('normal line - 2byte utf8 - EOL',      4, 1,     'Ä', 6, 1, [1,'abcÄ'],    'Ü', 8,1, [1,'abcÄÜ']);
    DoTest('normal line - 2byte utf8 - Past',     6, 1,     'Ä', 8, 1, [1,'abc  Ä'],  'Ü',10,1, [1,'abc  ÄÜ']);
    {%endregion}

    {%region  Normal Line -- full width Char - 3 bytes}
    // May change in future. overwrite only one char
    InsertFlag := True;
    CaretPEol := False;
    DoTest('normal line - 3 byte full witdh - BOL',      1, 1,     'あ', 4, 1, [1,'あabc'],    '吾', 7,1, [1,'あ吾abc']);
    DoTest('normal line - 3 byte full witdh - mid',      2, 1,     'あ', 5, 1, [1,'aあbc'],    '吾', 8,1, [1,'aあ吾bc']);
    DoTest('normal line - 3 byte full witdh - EOL',      4, 1,     'あ', 7, 1, [1,'abcあ'],    '吾',10,1, [1,'abcあ吾']);
    CaretPEol := True;
    DoTest('normal line - 3 byte full witdh - EOL',      4, 1,     'あ', 7, 1, [1,'abcあ'],    '吾',10,1, [1,'abcあ吾']);
    DoTest('normal line - 3 byte full witdh - Past',     6, 1,     'あ', 9, 1, [1,'abc  あ'],  '吾',12,1, [1,'abc  あ吾']);

    InsertFlag := False;
    CaretPEol := False;
    DoTest('normal line - 3 byte full witdh - BOL',      1, 1,     'あ', 4, 1, [1,'あbc'],     '吾', 7,1, [1,'あ吾c']);
    DoTest('normal line - 3 byte full witdh - mid',      2, 1,     'あ', 5, 1, [1,'aあc'],     '吾', 8,1, [1,'aあ吾']);
    DoTest('normal line - 3 byte full witdh - mid',      3, 1,     'あ', 6, 1, [1,'abあ'],     '吾', 9,1, [1,'abあ吾']);
    DoTest('normal line - 3 byte full witdh - EOL',      4, 1,     'あ', 7, 1, [1,'abcあ'],    '吾',10,1, [1,'abcあ吾']);
    CaretPEol := True;
    DoTest('normal line - 3 byte full witdh - EOL',      4, 1,     'あ', 7, 1, [1,'abcあ'],    '吾',10,1, [1,'abcあ吾']);
    DoTest('normal line - 3 byte full witdh - Past',     6, 1,     'あ', 9, 1, [1,'abc  あ'],  '吾',12,1, [1,'abc  あ吾']);
    {%endregion}
  {%endregion}

  {%region  Line with utf8 at start}
    {%region   Normal Char}
    InsertFlag := True;
    CaretPEol := False;
    DoTest('utf8 at start - BOL',      1, 2,     'X', 2, 2, [2,'Xöbc'],    'Y', 3,2, [2,'XYöbc']);
    DoTest('utf8 at start - mid',      3, 2,     'X', 4, 2, [2,'öXbc'],    'Y', 5,2, [2,'öXYbc']);
    DoTest('utf8 at start - EOL',      5, 2,     'X', 6, 2, [2,'öbcX'],    'Y', 7,2, [2,'öbcXY']);
    CaretPEol := True;
    DoTest('utf8 at start - EOL',      5, 2,     'X', 6, 2, [2,'öbcX'],    'Y', 7,2, [2,'öbcXY']);
    DoTest('utf8 at start - Past',     7, 2,     'X', 8, 2, [2,'öbc  X'],  'Y', 9,2, [2,'öbc  XY']);

    InsertFlag := False;
    CaretPEol := False;
    DoTest('utf8 at start - BOL',      1, 2,     'X', 2, 2, [2,'Xbc'],     'Y', 3,2, [2,'XYc']);
    DoTest('utf8 at start - mid',      3, 2,     'X', 4, 2, [2,'öXc'],     'Y', 5,2, [2,'öXY']);
    DoTest('utf8 at start - mid',      4, 2,     'X', 5, 2, [2,'öbX'],     'Y', 6,2, [2,'öbXY']);
    DoTest('utf8 at start - EOL',      5, 2,     'X', 6, 2, [2,'öbcX'],    'Y', 7,2, [2,'öbcXY']);
    CaretPEol := True;
    DoTest('utf8 at start - EOL',      5, 2,     'X', 6, 2, [2,'öbcX'],    'Y', 7,2, [2,'öbcXY']);
    DoTest('utf8 at start - Past',     7, 2,     'X', 8, 2, [2,'öbc  X'],  'Y', 9,2, [2,'öbc  XY']);
    {%endregion}

    {%region   utf8 2 byte Char}
    InsertFlag := True;
    CaretPEol := False;
    DoTest('utf8 at start - 2byte utf8 - BOL',      1, 2,     'Ä', 3, 2, [2,'Äöbc'],    'Ü', 5,2, [2,'ÄÜöbc']);
    DoTest('utf8 at start - 2byte utf8 - mid',      3, 2,     'Ä', 5, 2, [2,'öÄbc'],    'Ü', 7,2, [2,'öÄÜbc']);
    DoTest('utf8 at start - 2byte utf8 - EOL',      5, 2,     'Ä', 7, 2, [2,'öbcÄ'],    'Ü', 9,2, [2,'öbcÄÜ']);
    CaretPEol := True;
    DoTest('utf8 at start - 2byte utf8 - EOL',      5, 2,     'Ä', 7, 2, [2,'öbcÄ'],    'Ü', 9,2, [2,'öbcÄÜ']);
    DoTest('utf8 at start - 2byte utf8 - Past',     7, 2,     'Ä', 9, 2, [2,'öbc  Ä'],  'Ü',11,2, [2,'öbc  ÄÜ']);

    InsertFlag := False;
    CaretPEol := False;
    DoTest('utf8 at start - 2byte utf8 - BOL',      1, 2,     'Ä', 3, 2, [2,'Äbc'],     'Ü', 5,2, [2,'ÄÜc']);
    DoTest('utf8 at start - 2byte utf8 - mid',      3, 2,     'Ä', 5, 2, [2,'öÄc'],     'Ü', 7,2, [2,'öÄÜ']);
    DoTest('utf8 at start - 2byte utf8 - mid',      4, 2,     'Ä', 6, 2, [2,'öbÄ'],     'Ü', 8,2, [2,'öbÄÜ']);
    DoTest('utf8 at start - 2byte utf8 - EOL',      5, 2,     'Ä', 7, 2, [2,'öbcÄ'],    'Ü', 9,2, [2,'öbcÄÜ']);
    CaretPEol := True;
    DoTest('utf8 at start - 2byte utf8 - EOL',      5, 2,     'Ä', 7, 2, [2,'öbcÄ'],    'Ü', 9,2, [2,'öbcÄÜ']);
    DoTest('utf8 at start - 2byte utf8 - Past',     7, 2,     'Ä', 9, 2, [2,'öbc  Ä'],  'Ü',11,2, [2,'öbc  ÄÜ']);
    {%endregion}

  {%endregion}

  {%region  Line with tab at start}
    {%region   Normal Char}
    InsertFlag := True;
    CaretPEol := False;
    // Phys start pos
    DoTest('tab at start - BOL',       1, 3,     'X', 2, 3, [3,'X'#9'abc'],    'Y', 3,3, [3,'XY'#9'abc'], True);
    DoTest('tab at start - after tab', 7, 3,     'X', 3, 3, [3,#9'Xabc'],      'Y', 4,3, [3,#9'XYabc'],   True);
    DoTest('tab at start - in tab',    3, 3,     'X', 2, 3, [3,'X'#9'abc'],    'Y', 3,3, [3,'XY'#9'abc'], True);

    InsertFlag := False;
    CaretPEol := False;
    DoTest('tab at start - BOL',       1, 3,     'X', 2, 3, [3,'Xabc'],        'Y', 3,3, [3,'XYbc'],    True);
    DoTest('tab at start - after tab', 7, 3,     'X', 3, 3, [3,#9'Xbc'],       'Y', 4,3, [3,#9'XYc'],   True);
    DoTest('tab at start - in tab',    3, 3,     'X', 2, 3, [3,'Xabc'],        'Y', 3,3, [3,'XYbc'],    True);
    {%endregion}
  {%endregion}

  // TODO:  2 byte at EOL
  // TODO Overwrite selection
end;

procedure TTestBasicSynEdit.TestPhysicalLogical;

  function LogPhysConv: TSynLogicalPhysicalConvertor;
  begin
    Result := SynEdit.ViewedTextBuffer.LogPhysConvertor;
  end;

  procedure TestPhysLog(name: string; y, x: integer;
    expX: integer; expCol: integer = -1;
    expXcsRight: integer = -1; expColCsRight: integer = -1;
    expXcsLtr: integer = -1; expColCsLtr: integer = -1;
    expXcsRtl: integer = -1; expColCsRtl: integer = -1);
  var
    gotX, gotCol: Integer;
    expDef: Integer;
  begin
    name := name + ' y='+inttostr(y)+' x='+inttostr(x);

    expDef := expX;
    if expXcsLtr >= 0 then expDef := expXcsLtr;

    gotX := SynEdit.PhysicalToLogicalPos(Point(x, y)).x;
    AssertEquals(name+'  PhysicalToLogicalPos', expDef, gotX);

    gotX := SynEdit.PhysicalToLogicalCol(SynEdit.Lines[y-1], y-1, x);
    AssertEquals(name+'  PhysicalToLogicalCol', expDef, gotX);



    gotX := LogPhysConv.PhysicalToLogical(y-1, x, gotCol, cspLeft);
    AssertEquals(name+'  c.PhysicalToLogical', expX, gotX);
    if expCol >= 0 then
      AssertEquals(name+'  c.PhysicalToLogical  COL', expCol, gotCol);


    if expXcsRight >= 0 then begin
      gotX := LogPhysConv.PhysicalToLogical(y-1, x, gotCol, cspRight);
      AssertEquals(name+'  c.PhysicalToLogical csRight', expXcsRight, gotX);
      if expColCsRight >= 0 then
        AssertEquals(name+'  c.PhysicalToLogical csRight COL', expColCsRight, gotCol);
    end;


    if expXcsLtr >= 0 then begin
      gotX := LogPhysConv.PhysicalToLogical(y-1, x, gotCol, cspFollowLtr);
      AssertEquals(name+'  c.PhysicalToLogical csLtr', expXcsLtr, gotX);
      if expColCsLtr >= 0 then
        AssertEquals(name+'  c.PhysicalToLogical csLtr COL', expColCsLtr, gotCol);
    end;


    if expXcsRtl >= 0 then begin
      gotX := LogPhysConv.PhysicalToLogical(y-1, x, gotCol, cspFollowRtl);
      AssertEquals(name+'  c.PhysicalToLogical csRtl', expXcsRtl, gotX);
      if expColCsRtl >= 0 then
        AssertEquals(name+'  c.PhysicalToLogical csRtl COL', expColCsRtl, gotCol);
    end;
  end;

  procedure TestLogPhys(name: string; y, x, aCol: integer;
    expX: integer; expXcsAfter: integer = -1; expXcsLtr: integer = -1; expXcsRtl: integer = -1);
  var gotX: Integer;
    expDef: Integer;
  begin
    name := name + ' y='+inttostr(y)+' x='+inttostr(x)+' c='+IntToStr(aCol);

    if aCol = 0 then begin
      expDef := expX;
      if expXcsLtr >= 0 then expDef := expXcsLtr; // default is now cslFollowLtr

      gotX := SynEdit.LogicalToPhysicalPos(Point(x, y)).x;
      AssertEquals(name+'  LogicalToPhysicalPos', expDef, gotX);

      gotX := SynEdit.LogicalToPhysicalCol(SynEdit.Lines[y-1], y-1, x);
      AssertEquals(name+'  LogicalToPhysicalCol', expDef, gotX);
    end;

    gotX := LogPhysConv.LogicalToPhysical(y-1, x, aCol, cslBefore);
    AssertEquals(name+'  c.LogicalToPhysical', expX, gotX);

    if expXcsAfter >= 0 then begin
      gotX := LogPhysConv.LogicalToPhysical(y-1, x, aCol, cslAfter);
      AssertEquals(name+'  c.LogicalToPhysical cslAfter', expXcsAfter, gotX);
    end;

    if expXcsLtr >= 0 then begin
      gotX := LogPhysConv.LogicalToPhysical(y-1, x, aCol, cslFollowLtr);
      AssertEquals(name+'  c.LogicalToPhysical cslFollowLtr', expXcsLtr, gotX);
    end;

    if expXcsRtl >= 0 then begin
      gotX := LogPhysConv.LogicalToPhysical(y-1, x, aCol, cslFollowRtl);
      AssertEquals(name+'  c.LogicalToPhysical cslFollowRtl', expXcsRtl, gotX);
    end;
  end;

begin
  FPersistSel := False;
  ReCreateEdit;
  SynEdit.TabWidth := 6;

  // Todo Log2Phys: test column is cut off

  SetLines(['abc',  ' ääX',  #9'mn',  'abc'#9'de',  #9'Xää.',  'ab'#9,  'あ吾' ]);

  TestLogPhys('simple line (abc)',           1,  1, 0,    1);
  TestLogPhys('simple line (abc)',           1,  2, 0,    2);
  TestLogPhys('simple line (abc)',           1,  4, 0,    4);
  TestLogPhys('simple line (abc)',           1,  5, 0,    5);
  TestLogPhys('simple line (abc)',           1,  6, 0,    6);
  TestLogPhys('line with 2byte-char',        2,  1, 0,    1);
  TestLogPhys('line with 2byte-char',        2,  2, 0,    2);
  TestLogPhys('line with 2byte-char',        2,  4, 0,    3); // after ae
  TestLogPhys('line with 2byte-char',        2,  6, 0,    4);
  TestLogPhys('line with 2byte-char',        2,  7, 0,    5);
  TestLogPhys('line with 2byte-char',        2,  8, 0,    6);
  TestLogPhys('line with 2byte-char',        2, 11, 0,    9);
  TestLogPhys('line with tab (start)',       3,  1, 0,    1);
  TestLogPhys('line with tab (start)',       3,  2, 0,    7);
  TestLogPhys('line with tab (middle)',      4,  3, 0,    3);
  TestLogPhys('line with tab (middle)',      4,  4, 0,    4); // before tab
  TestLogPhys('line with tab (middle)',      4,  4, 1,    5); // inside tab
  TestLogPhys('line with tab (middle)',      4,  4, 2,    6); // inside tab
  TestLogPhys('line with tab (middle)',      4,  5, 0,    7); // after tab
  TestLogPhys('line with tab (middle)',      4,  6, 0,    8);
  TestLogPhys('line with tab (middle)',      4,  9, 0,   11);
  TestLogPhys('line with tab (start) + 2bc', 5,  1, 0,    1);
  TestLogPhys('line with tab (start) + 2bc', 5,  2, 0,    7);
  TestLogPhys('line with tab (start) + 2bc', 5,  3, 0,    8);
  TestLogPhys('line with tab (start) + 2bc', 5,  5, 0,    9);
  TestLogPhys('line with tab (end)',         6,  3, 0,    3);
  TestLogPhys('line with tab (end)',         6,  4, 0,    7);
  TestLogPhys('line with tab (end)',         6,  5, 0,    8);
  TestLogPhys('line with tab (end)',         6,  3, 1,    4);
  TestLogPhys('line with tab (end)',         6,  3, 2,    5);
  TestLogPhys('line with tab (end)',         6,  3, 3,    6);
  TestLogPhys('line with double-width/3byte',7,  1, 0,    1);
  TestLogPhys('line with double-width/3byte',7,  1, 1,    2);
  TestLogPhys('line with double-width/3byte',7,  4, 0,    3);
  TestLogPhys('line with double-width/3byte',7,  4, 1,    4);
  TestLogPhys('line with double-width/3byte',7,  7, 0,    5);

  TestPhysLog('simple line (abc)',     1,  1,   1);
  TestPhysLog('simple line (abc)',     1,  2,   2);
  TestPhysLog('simple line (abc)',     1,  4,   4);
  TestPhysLog('simple line (abc)',     1,  5,   5);
  TestPhysLog('simple line (abc)',     1,  6,   6);
  TestPhysLog('line with 3byte-char',  2,  1,   1);
  TestPhysLog('line with 3byte-char',  2,  2,   2);
  TestPhysLog('line with 3byte-char',  2,  3,   4);
  TestPhysLog('line with 3byte-char',  2,  4,   6);
  TestPhysLog('line with 3byte-char',  2,  5,   7);
  TestPhysLog('line with 3byte-char',  2,  6,   8);
  TestPhysLog('line with 3byte-char',  2,  7,   9);
  TestPhysLog('line with tab (start)', 3,  1,   1);
  TestPhysLog('line with tab (start)', 3,  2,   1);
  TestPhysLog('line with tab (start)', 3,  5,   1);
  TestPhysLog('line with tab (start)', 3,  6,   1);
  TestPhysLog('line with tab (start)', 3,  7,   2);
  TestPhysLog('line with tab (start)', 3,  8,   3);
  TestPhysLog('line with tab (start)', 3,  9,   4);
  TestPhysLog('line with tab (start)', 3, 11,   6);
  TestPhysLog('line with double-width/3byte', 7,  1,   1, 0);
  TestPhysLog('line with double-width/3byte', 7,  2,   1, 1);
  TestPhysLog('line with double-width/3byte', 7,  3,   4, 0);
  TestPhysLog('line with double-width/3byte', 7,  4,   4, 1);
  TestPhysLog('line with double-width/3byte', 7,  5,   7, 0);

  //abc def ghi // 2bytes per char

  (*  Order in String "123" / Order on Screen "321"
    LogicalToPhys
      Log = 1
      <|321    L2p (csLeft)  => 1 // Log 1 is after  it's LEFT  neighbour (BOL)
        321<|  L2p (csRight) => 4 // Log 1 is before it's RIGHT neighbour (char "1") // logical right, byte order in string
      Log = 4
      |>321    L2p (csLeft)  => 1 // Log 4 is after  it's LEFT  neighbour (char "3") // logical left, byte order in string
        321|>  L2p (csRight) => 4 // Log 4 is before it's RIGHT neighbour (EOL)

    PhysToLog:
      Phys = 1
      <|321    L2p (csLeft)  => 1
      |>321    L2p (csRight) => 4
      Phys = 4
        321<|  L2p (csLeft)  => 1
        321|>  L2p (csRight) => 4

  *)

  SetLines(['شىه ايغ عتل', 'ABCشىه ايغ عتلDEF', 'abcشىه ايغ عتل', 'شىه ايغ عتلdef', '']);

  //                                                           B,  A,  L,  R
  TestLogPhys('empty line',                   5,   1, 0,       1,  1,  1,  1);
  TestLogPhys('empty line',                   5,   2, 0,       2,  2,  2,  2);

  TestLogPhys('bidi line (arab only)',        1,   1, 0,       1, 12,  1, 12);
  TestLogPhys('bidi line (arab only)',        1,   3, 0,      11, 11, 11, 11);
  TestLogPhys('bidi line (arab only)',        1,   5, 0,      10, 10, 10, 10);
  TestLogPhys('bidi line (arab only)',        1,   7, 0,       9,  9,  9,  9);
  TestLogPhys('bidi line (arab only)',        1,   8, 0,       8,  8,  8,  8); // after space
  TestLogPhys('bidi line (arab only)',        1,  15, 0,       4,  4,  4,  4); // after space
  TestLogPhys('bidi line (arab only)',        1,  19, 0,       2,  2,  2,  2);
  TestLogPhys('bidi line (arab only)',        1,  21, 0,       1, 12, 12,  1); // at EOL
  TestLogPhys('bidi line (arab only)',        1,  22, 0,      13, 13, 13, 13); // past eol
  TestLogPhys('bidi line (arab only)',        1,  23, 0,      14, 14, 14, 14);

  TestLogPhys('bidi line (mixed arab/latin)', 2,   1, 0,       1,  1,  1,  1);
  TestLogPhys('bidi line (mixed arab/latin)', 2,   4, 0,       4, 15,  4, 15); // after C
  TestLogPhys('bidi line (mixed arab/latin)', 2,   6, 0,      14, 14, 14, 14); // 1 into arabic
  TestLogPhys('bidi line (mixed arab/latin)', 2,  22, 0,       5,  5,  5,  5); // 1 before end arabic
  TestLogPhys('bidi line (mixed arab/latin)', 2,  24, 0,       4, 15, 15,  4); // at end arabic
  TestLogPhys('bidi line (mixed arab/latin)', 2,  25, 0,      16, 16, 16, 16); // after D
  TestLogPhys('bidi line (mixed arab/latin)', 2,  27, 0,      18, 18, 18, 18); // at eol
  TestLogPhys('bidi line (mixed arab/latin)', 2,  28, 0,      19, 19, 19, 19); // after eol

  //                                                     Lft      Rght     LTR      RTL
  TestPhysLog('empty line',                 5,   1,      1, 0,    1, 0,    1, 0,    1, 0);
  TestPhysLog('empty line',                 5,   2,      2, 0,    2, 0,    2, 0,    2, 0);

  TestPhysLog('bidi line (arab only)',      1,   1,      1, 0,   21, 0,    1, 0,   21, 0);
  TestPhysLog('bidi line (arab only)',      1,   2,     19, 0,   19, 0,   19, 0,   19, 0);
  TestPhysLog('bidi line (arab only)',      1,   3,     17, 0,   17, 0,   17, 0,   17, 0);
  TestPhysLog('bidi line (arab only)',      1,   4,     15, 0,   15, 0,   15, 0,   15, 0); // before space
  TestPhysLog('bidi line (arab only)',      1,   5,     14, 0,   14, 0,   14, 0,   14, 0); // after space
  TestPhysLog('bidi line (arab only)',      1,  10,      5, 0,    5, 0,    5, 0,    5, 0);
  TestPhysLog('bidi line (arab only)',      1,  11,      3, 0,    3, 0,    3, 0,    3, 0);
  TestPhysLog('bidi line (arab only)',      1,  12,      1, 0,   21, 0,   21, 0,    1, 0); // at eol
  TestPhysLog('bidi line (arab only)',      1,  13,     22, 0,   22, 0,   22, 0,   22, 0);
  TestPhysLog('bidi line (arab only)',      1,  14,     23, 0,   23, 0,   23, 0,   23, 0);

  TestPhysLog('bidi line (mixed arab/latin)',2,  1,      1, 0,    1, 0,    1, 0,    1, 0);
  TestPhysLog('bidi line (mixed arab/latin)',2,  4,      4, 0,   24, 0,    4, 0,   24, 0);
  TestPhysLog('bidi line (mixed arab/latin)',2, 15,      4, 0,   24, 0,   24, 0,    4, 0);
end;

procedure TTestBasicSynEdit.TestLogicalAdjust;
var
  tb: TSynEditStrings;
begin
  FPersistSel := False;
  tb := SynEdit.TextBuffer;
  // #$CC#$81 Combining
  AssertEquals('LogicPosIsAtChar 1 ', True, tb.LogicPosIsAtChar('aüb'#$CC#$81'c', 1)); // a
  AssertEquals('LogicPosIsAtChar 2 ', True, tb.LogicPosIsAtChar('aüb'#$CC#$81'c', 2)); // ü
  AssertEquals('LogicPosIsAtChar 3 ', False,tb.LogicPosIsAtChar('aüb'#$CC#$81'c', 3)); // mid ü
  AssertEquals('LogicPosIsAtChar 4 ', True, tb.LogicPosIsAtChar('aüb'#$CC#$81'c', 4)); // b
  AssertEquals('LogicPosIsAtChar 5 ', False,tb.LogicPosIsAtChar('aüb'#$CC#$81'c', 5)); // AT Combining
  AssertEquals('LogicPosIsAtChar 6 ', False,tb.LogicPosIsAtChar('aüb'#$CC#$81'c', 6)); // mid Combining
  AssertEquals('LogicPosIsAtChar 7 ', True, tb.LogicPosIsAtChar('aüb'#$CC#$81'c', 7)); // c

  AssertEquals('LogicPosIsAtChar 1 C', True, tb.LogicPosIsAtChar('aüb'#$CC#$81'c', 1, [lpStopAtCodePoint])); // a
  AssertEquals('LogicPosIsAtChar 2 C', True, tb.LogicPosIsAtChar('aüb'#$CC#$81'c', 2, [lpStopAtCodePoint])); // ü
  AssertEquals('LogicPosIsAtChar 3 C', False,tb.LogicPosIsAtChar('aüb'#$CC#$81'c', 3, [lpStopAtCodePoint])); // mid ü
  AssertEquals('LogicPosIsAtChar 4 C', True, tb.LogicPosIsAtChar('aüb'#$CC#$81'c', 4, [lpStopAtCodePoint])); // b
  AssertEquals('LogicPosIsAtChar 5 C', True ,tb.LogicPosIsAtChar('aüb'#$CC#$81'c', 5, [lpStopAtCodePoint])); // AT Combining
  AssertEquals('LogicPosIsAtChar 6 C', False,tb.LogicPosIsAtChar('aüb'#$CC#$81'c', 6, [lpStopAtCodePoint])); // mid Combining
  AssertEquals('LogicPosIsAtChar 7 C', True, tb.LogicPosIsAtChar('aüb'#$CC#$81'c', 7, [lpStopAtCodePoint])); // c

  // broken text
  AssertEquals('LogicPosIsAtChar 1 Comb', True, tb.LogicPosIsAtChar(#$CC#$81'c', 1, []));
  AssertEquals('LogicPosIsAtChar 1 Comb C', True, tb.LogicPosIsAtChar(#$CC#$81'c', 1, [lpStopAtCodePoint]));


  AssertEquals('LogicPosAdjustToChar 1 ', 1, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 1)); // a
  AssertEquals('LogicPosAdjustToChar 2 ', 2, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 2)); // ü
  AssertEquals('LogicPosAdjustToChar 3 ', 2, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 3)); // mid ü
  AssertEquals('LogicPosAdjustToChar 4 ', 4, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 4)); // b
  AssertEquals('LogicPosAdjustToChar 5 ', 4, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 5)); // AT Combining
  AssertEquals('LogicPosAdjustToChar 6 ', 4, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 6)); // mid Combining
  AssertEquals('LogicPosAdjustToChar 7 ', 7, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 7)); // c

  AssertEquals('LogicPosAdjustToChar 1N ', 1, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 1, [lpAdjustToNext])); // a
  AssertEquals('LogicPosAdjustToChar 2N ', 2, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 2, [lpAdjustToNext])); // ü
  AssertEquals('LogicPosAdjustToChar 3N ', 4, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 3, [lpAdjustToNext])); // mid ü
  AssertEquals('LogicPosAdjustToChar 4N ', 4, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 4, [lpAdjustToNext])); // b
  AssertEquals('LogicPosAdjustToChar 5N ', 7, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 5, [lpAdjustToNext])); // AT Combining
  AssertEquals('LogicPosAdjustToChar 6N ', 7, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 6, [lpAdjustToNext])); // mid Combining
  AssertEquals('LogicPosAdjustToChar 7N ', 7, tb.LogicPosAdjustToChar('aüb'#$CC#$81'ü', 7, [lpAdjustToNext])); // ü
  AssertEquals('LogicPosAdjustToChar 8N ', 7, tb.LogicPosAdjustToChar('aüb'#$CC#$81'ü', 8, [lpAdjustToNext])); // mid ü
  AssertEquals('LogicPosAdjustToChar 8NE', 9, tb.LogicPosAdjustToChar('aüb'#$CC#$81'ü', 8, [lpAdjustToNext, lpAllowPastEol])); // mid ü

  AssertEquals('LogicPosAdjustToChar 1 C', 1, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 1, [lpStopAtCodePoint])); // a
  AssertEquals('LogicPosAdjustToChar 2 C', 2, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 2, [lpStopAtCodePoint])); // ü
  AssertEquals('LogicPosAdjustToChar 3 C', 2, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 3, [lpStopAtCodePoint])); // mid ü
  AssertEquals('LogicPosAdjustToChar 4 C', 4, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 4, [lpStopAtCodePoint])); // b
  AssertEquals('LogicPosAdjustToChar 5 C', 5, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 5, [lpStopAtCodePoint])); // AT Combining
  AssertEquals('LogicPosAdjustToChar 6 C', 5, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 6, [lpStopAtCodePoint])); // mid Combining
  AssertEquals('LogicPosAdjustToChar 7 C', 7, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 7, [lpStopAtCodePoint])); // c

  AssertEquals('LogicPosAdjustToChar 1N C', 1, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 1, [lpStopAtCodePoint, lpAdjustToNext])); // a
  AssertEquals('LogicPosAdjustToChar 2N C', 2, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 2, [lpStopAtCodePoint, lpAdjustToNext])); // ü
  AssertEquals('LogicPosAdjustToChar 3N C', 4, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 3, [lpStopAtCodePoint, lpAdjustToNext])); // mid ü
  AssertEquals('LogicPosAdjustToChar 4N C', 4, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 4, [lpStopAtCodePoint, lpAdjustToNext])); // b
  AssertEquals('LogicPosAdjustToChar 5N C', 5, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 5, [lpStopAtCodePoint, lpAdjustToNext])); // AT Combining
  AssertEquals('LogicPosAdjustToChar 6N C', 7, tb.LogicPosAdjustToChar('aüb'#$CC#$81'c', 6, [lpStopAtCodePoint, lpAdjustToNext])); // mid Combining
  AssertEquals('LogicPosAdjustToChar 7N C', 7, tb.LogicPosAdjustToChar('aüb'#$CC#$81'ü', 7, [lpStopAtCodePoint, lpAdjustToNext])); // ü
  AssertEquals('LogicPosAdjustToChar 8N C', 7, tb.LogicPosAdjustToChar('aüb'#$CC#$81'ü', 8, [lpStopAtCodePoint, lpAdjustToNext])); // mid ü
  AssertEquals('LogicPosAdjustToChar 8NE C', 9, tb.LogicPosAdjustToChar('aüb'#$CC#$81'ü', 8, [lpStopAtCodePoint, lpAdjustToNext, lpAllowPastEol])); // mid ü

  // broken text
  AssertEquals('LogicPosAdjustToChar 1 Comb', 1, tb.LogicPosAdjustToChar(#$CC#$81'c', 1, []));
  AssertEquals('LogicPosAdjustToChar 2 Comb', 1, tb.LogicPosAdjustToChar(#$CC#$81'c', 2, []));
  AssertEquals('LogicPosAdjustToChar 1 Comb N', 1, tb.LogicPosAdjustToChar(#$CC#$81'c', 1, [lpAdjustToNext]));
  AssertEquals('LogicPosAdjustToChar 2 Comb N', 3, tb.LogicPosAdjustToChar(#$CC#$81'c', 2, [lpAdjustToNext]));
  AssertEquals('LogicPosAdjustToChar 1 Comb', 1, tb.LogicPosAdjustToChar(#$CC#$81'c', 1, [lpStopAtCodePoint]));
  AssertEquals('LogicPosAdjustToChar 2 Comb', 1, tb.LogicPosAdjustToChar(#$CC#$81'c', 2, [lpStopAtCodePoint]));
  AssertEquals('LogicPosAdjustToChar 1 Comb N', 1, tb.LogicPosAdjustToChar(#$CC#$81'c', 1, [lpStopAtCodePoint, lpAdjustToNext]));
  AssertEquals('LogicPosAdjustToChar 2 Comb N', 3, tb.LogicPosAdjustToChar(#$CC#$81'c', 2, [lpStopAtCodePoint, lpAdjustToNext]));


  AssertEquals('LogicPosAddChars  1, 1  ', 2, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 1, 1)); // a
  AssertEquals('LogicPosAddChars  2, 1  ', 4, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 2, 1)); // ü
  AssertEquals('LogicPosAddChars  3, 1  ', 4, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 3, 1)); // mid ü
  AssertEquals('LogicPosAddChars  4, 1  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 4, 1)); // b
  AssertEquals('LogicPosAddChars  5, 1  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 5, 1)); // comb
  AssertEquals('LogicPosAddChars  6, 1  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 6, 1)); // mid
  AssertEquals('LogicPosAddChars  7, 1  ', 8, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 7, 1)); // c
  AssertEquals('LogicPosAddChars  8, 1  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 8, 1)); // ü
  AssertEquals('LogicPosAddChars  9, 1  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 9, 1)); // mid ü
  AssertEquals('LogicPosAddChars 10, 1  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',10, 1)); // ü
  AssertEquals('LogicPosAddChars 11, 1  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',11, 1)); // mid ü
  AssertEquals('LogicPosAddChars 10, 1 E',12, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',10, 1, [lpAllowPastEol])); // ü
  AssertEquals('LogicPosAddChars 11, 1 E',12, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',11, 1, [lpAllowPastEol])); // mid ü

  AssertEquals('LogicPosAddChars  1, 2  ', 4, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 1, 2)); // a
  AssertEquals('LogicPosAddChars  2, 2  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 2, 2)); // ü
  AssertEquals('LogicPosAddChars  3, 2  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 3, 2)); // mid ü
  AssertEquals('LogicPosAddChars  4, 2  ', 8, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 4, 2)); // b
  AssertEquals('LogicPosAddChars  5, 2  ', 8, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 5, 2)); // comb
  AssertEquals('LogicPosAddChars  6, 2  ', 8, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 6, 2)); // mid
  AssertEquals('LogicPosAddChars  7, 2  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 7, 2)); // c
  AssertEquals('LogicPosAddChars  8, 2  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 8, 2)); // ü
  AssertEquals('LogicPosAddChars  9, 2  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 9, 2)); // mid ü
  AssertEquals('LogicPosAddChars 10, 2  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',10, 2)); // ü
  AssertEquals('LogicPosAddChars 11, 2  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',11, 2)); // mid ü
  AssertEquals('LogicPosAddChars  8, 2 E',12, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 8, 2, [lpAllowPastEol])); // ü
  AssertEquals('LogicPosAddChars  9, 2 E',12, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 9, 2, [lpAllowPastEol])); // mid ü
  AssertEquals('LogicPosAddChars 10, 2 E',13, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',10, 2, [lpAllowPastEol])); // ü
  AssertEquals('LogicPosAddChars 11, 2 E',13, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',11, 2, [lpAllowPastEol])); // mid ü

  AssertEquals('LogicPosAddChars  1, -1  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 1, -1)); // a
  AssertEquals('LogicPosAddChars  2, -1  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 2, -1)); // ü
//AssertEquals('LogicPosAddChars  3, -1  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 3, -1)); // mid ü
  AssertEquals('LogicPosAddChars  4, -1  ', 2, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 4, -1)); // b
//AssertEquals('LogicPosAddChars  5, -1  ', 2, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 5, -1)); // comb
//AssertEquals('LogicPosAddChars  6, -1  ', 2, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 6, -1)); // mid
  AssertEquals('LogicPosAddChars  7, -1  ', 4, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 7, -1)); // c
  AssertEquals('LogicPosAddChars  8, -1  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 8, -1)); // ü
//AssertEquals('LogicPosAddChars  9, -1  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 9, -1)); // mid ü
  AssertEquals('LogicPosAddChars 10, -1  ', 8, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',10, -1)); // ü
//AssertEquals('LogicPosAddChars 11, -1  ', 8, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',11, -1)); // mid ü

  AssertEquals('LogicPosAddChars  1, -2  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 1, -2)); // a
  AssertEquals('LogicPosAddChars  2, -2  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 2, -2)); // ü
//AssertEquals('LogicPosAddChars  3, -2  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 3, -2)); // mid ü
  AssertEquals('LogicPosAddChars  4, -2  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 4, -2)); // b
//AssertEquals('LogicPosAddChars  5, -2  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 5, -2)); // comb
//AssertEquals('LogicPosAddChars  6, -2  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 6, -2)); // mid
  AssertEquals('LogicPosAddChars  7, -2  ', 2, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 7, -2)); // c
  AssertEquals('LogicPosAddChars  8, -2  ', 4, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 8, -2)); // ü
//AssertEquals('LogicPosAddChars  9, -2  ', 4, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 9, -2)); // mid ü
  AssertEquals('LogicPosAddChars 10, -2  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',10, -2)); // ü
//AssertEquals('LogicPosAddChars 11, -2  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',11, -2)); // mid ü


  AssertEquals('LogicPosAddChars  1, 1C  ', 2, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 1, 1, [lpStopAtCodePoint])); // a
  AssertEquals('LogicPosAddChars  2, 1C  ', 4, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 2, 1, [lpStopAtCodePoint])); // ü
  AssertEquals('LogicPosAddChars  3, 1C  ', 4, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 3, 1, [lpStopAtCodePoint])); // mid ü
  AssertEquals('LogicPosAddChars  4, 1C  ', 5, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 4, 1, [lpStopAtCodePoint])); // b
  AssertEquals('LogicPosAddChars  5, 1C  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 5, 1, [lpStopAtCodePoint])); // comb
  AssertEquals('LogicPosAddChars  6, 1C  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 6, 1, [lpStopAtCodePoint])); // mid
  AssertEquals('LogicPosAddChars  7, 1C  ', 8, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 7, 1, [lpStopAtCodePoint])); // c
  AssertEquals('LogicPosAddChars  8, 1C  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 8, 1, [lpStopAtCodePoint])); // ü
  AssertEquals('LogicPosAddChars  9, 1C  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 9, 1, [lpStopAtCodePoint])); // mid ü
  AssertEquals('LogicPosAddChars 10, 1C  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',10, 1, [lpStopAtCodePoint])); // ü
  AssertEquals('LogicPosAddChars 11, 1C  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',11, 1, [lpStopAtCodePoint])); // mid ü
  AssertEquals('LogicPosAddChars 10, 1C E',12, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',10, 1, [lpAllowPastEol, lpStopAtCodePoint])); // ü
  AssertEquals('LogicPosAddChars 11, 1C E',12, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',11, 1, [lpAllowPastEol, lpStopAtCodePoint])); // mid ü

  AssertEquals('LogicPosAddChars  1, 2C  ', 4, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 1, 2, [lpStopAtCodePoint])); // a
  AssertEquals('LogicPosAddChars  2, 2C  ', 5, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 2, 2, [lpStopAtCodePoint])); // ü
  AssertEquals('LogicPosAddChars  3, 2C  ', 5, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 3, 2, [lpStopAtCodePoint])); // mid ü
  AssertEquals('LogicPosAddChars  4, 2C  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 4, 2, [lpStopAtCodePoint])); // b
  AssertEquals('LogicPosAddChars  5, 2C  ', 8, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 5, 2, [lpStopAtCodePoint])); // comb
  AssertEquals('LogicPosAddChars  6, 2C  ', 8, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 6, 2, [lpStopAtCodePoint])); // mid
  AssertEquals('LogicPosAddChars  7, 2C  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 7, 2, [lpStopAtCodePoint])); // c
  AssertEquals('LogicPosAddChars  8, 2C  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 8, 2, [lpStopAtCodePoint])); // ü
  AssertEquals('LogicPosAddChars  9, 2C  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 9, 2, [lpStopAtCodePoint])); // mid ü
  AssertEquals('LogicPosAddChars 10, 2C  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',10, 2, [lpStopAtCodePoint])); // ü
  AssertEquals('LogicPosAddChars 11, 2C  ',10, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',11, 2, [lpStopAtCodePoint])); // mid ü
  AssertEquals('LogicPosAddChars  8, 2C E',12, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 8, 2, [lpAllowPastEol, lpStopAtCodePoint])); // ü
  AssertEquals('LogicPosAddChars  9, 2C E',12, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 9, 2, [lpAllowPastEol, lpStopAtCodePoint])); // mid ü
  AssertEquals('LogicPosAddChars 10, 2C E',13, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',10, 2, [lpAllowPastEol, lpStopAtCodePoint])); // ü
  AssertEquals('LogicPosAddChars 11, 2C E',13, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',11, 2, [lpAllowPastEol, lpStopAtCodePoint])); // mid ü

  AssertEquals('LogicPosAddChars  1, -1C  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 1, -1, [lpStopAtCodePoint])); // a
  AssertEquals('LogicPosAddChars  2, -1C  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 2, -1, [lpStopAtCodePoint])); // ü
//AssertEquals('LogicPosAddChars  3, -1C  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 3, -1, [lpStopAtCodePoint])); // mid ü
  AssertEquals('LogicPosAddChars  4, -1C  ', 2, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 4, -1, [lpStopAtCodePoint])); // b
  AssertEquals('LogicPosAddChars  5, -1C  ', 4, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 5, -1, [lpStopAtCodePoint])); // comb
//AssertEquals('LogicPosAddChars  6, -1C  ', 4, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 6, -1, [lpStopAtCodePoint])); // mid
  AssertEquals('LogicPosAddChars  7, -1C  ', 5, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 7, -1, [lpStopAtCodePoint])); // c
  AssertEquals('LogicPosAddChars  8, -1C  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 8, -1, [lpStopAtCodePoint])); // ü
//AssertEquals('LogicPosAddChars  9, -1C  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 9, -1, [lpStopAtCodePoint])); // mid ü
  AssertEquals('LogicPosAddChars 10, -1C  ', 8, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',10, -1, [lpStopAtCodePoint])); // ü
//AssertEquals('LogicPosAddChars 11, -1C  ', 8, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',11, -1, [lpStopAtCodePoint])); // mid ü

  AssertEquals('LogicPosAddChars  1, -2C  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 1, -2, [lpStopAtCodePoint])); // a
  AssertEquals('LogicPosAddChars  2, -2C  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 2, -2, [lpStopAtCodePoint])); // ü
//AssertEquals('LogicPosAddChars  3, -2C  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 3, -2, [lpStopAtCodePoint])); // mid ü
  AssertEquals('LogicPosAddChars  4, -2C  ', 1, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 4, -2, [lpStopAtCodePoint])); // b
  AssertEquals('LogicPosAddChars  5, -2C  ', 2, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 5, -2, [lpStopAtCodePoint])); // comb
//AssertEquals('LogicPosAddChars  6, -2C  ', 2, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 6, -2, [lpStopAtCodePoint])); // mid
  AssertEquals('LogicPosAddChars  7, -2C  ', 4, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 7, -2, [lpStopAtCodePoint])); // c
  AssertEquals('LogicPosAddChars  8, -2C  ', 5, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 8, -2, [lpStopAtCodePoint])); // ü
//AssertEquals('LogicPosAddChars  9, -2C  ', 5, tb.LogicPosAddChars('aüb'#$CC#$81'cüü', 9, -2, [lpStopAtCodePoint])); // mid ü
  AssertEquals('LogicPosAddChars 10, -2C  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',10, -2, [lpStopAtCodePoint])); // ü
//AssertEquals('LogicPosAddChars 11, -2C  ', 7, tb.LogicPosAddChars('aüb'#$CC#$81'cüü',11, -2, [lpStopAtCodePoint])); // mid ü



end;

procedure TTestBasicSynEdit.TestCaretObject;
var
  TestCaret, TestCaret2: TSynEditCaret;
  TestOrder: Integer;
  UseAdjustToNextChar, UseIncAdjustToNextChar: Boolean;
  UseAllowPastEOL, UseIncAllowPastEOL: Boolean;
  UseKeepCaretX: Boolean;
  UseLock: Boolean;
  UseChangeOnTouch: Boolean;
  UseIncAutoMoveOnEdit: Boolean;
  UseSkipTabs: Boolean;
  UseMaxLeft: Boolean;


  procedure CheckPhys(AName: String; ExpY, ExpX: Integer);
    procedure CheckEach;
    begin
      AssertEquals(AName + 'Phys.Y', ExpY, TestCaret.LinePos);
      AssertEquals(AName + 'Phys.X', ExpX, TestCaret.CharPos);
    end;
    procedure CheckPoint;
    begin
      AssertEquals(AName + 'Phys.XY.Y', ExpY, TestCaret.LineCharPos.Y);
      AssertEquals(AName + 'Phys.XY.X', ExpX, TestCaret.LineCharPos.x);
    end;
  begin
    if ExpX <= 0 then exit;
    AName := BaseTestName + ' ' + AName;
    if (TestOrder and 1) = 0 then begin
      CheckEach;
      CheckPoint;
    end else begin
      CheckPoint;
      CheckEach;
    end;
  end;
  procedure CheckIsAtChar(AName: String; ExpY, ExpX: Integer);
  begin
    if ExpX <= 0 then exit;
    AName := BaseTestName + ' ' + AName;
    if (TestOrder and 1) = 1 then exit; // Only one order
    AssertEquals(AName + 'IsAtLineChar',     True,  TestCaret.IsAtLineChar(point(ExpX, ExpY)));
    AssertEquals(AName + 'NOT IsAtLineChar', False, TestCaret.IsAtLineChar(point(ExpX+1, ExpY)));
  end;

  procedure CheckLog(AName: String; ExpY, ExpX, ExpOffs: Integer);
    procedure CheckEach;
    begin
      AssertEquals(AName + 'Log.Y', ExpY, TestCaret.LinePos);
      AssertEquals(AName + 'Log.X', ExpX, TestCaret.BytePos);
      AssertEquals(AName + 'Log.Offs', ExpOffs, TestCaret.BytePosOffset);
    end;
    procedure CheckPoint;
    begin
      AssertEquals(AName + 'Log.XY.Y', ExpY, TestCaret.LineBytePos.y);
      AssertEquals(AName + 'Log.XY.X', ExpX, TestCaret.LineBytePos.x);
    end;
  begin
    if ExpX <= 0 then exit;
    AName := BaseTestName + ' ' + AName;
    AName := BaseTestName + ' ' + AName;
    if (TestOrder and 1) = 0 then begin
      CheckEach;
      CheckPoint;
    end else begin
      CheckPoint;
      CheckEach;
    end;
  end;
  procedure CheckIsAtByte(AName: String; ExpY, ExpX, ExpOffs: Integer);
  begin
    if ExpX <= 0 then exit;
    AName := BaseTestName + ' ' + AName;
    if (TestOrder and 1) = 1 then exit; // Only one order
    AssertEquals(AName + 'IsAtLineChar',     True,  TestCaret.IsAtLineByte(point(ExpX, ExpY), ExpOffs));
    AssertEquals(AName + 'NOT IsAtLineByte', False, TestCaret.IsAtLineByte(point(ExpX+1, ExpY), ExpOffs));
    if ExpOffs = 0 then begin
      AssertEquals(AName + 'IsAtLineChar',     True,  TestCaret.IsAtLineByte(point(ExpX, ExpY)));
      AssertEquals(AName + 'NOT IsAtLineByte', False, TestCaret.IsAtLineByte(point(ExpX+1, ExpY)));
      AssertEquals(AName + 'NOT IsAtLineByte', False, TestCaret.IsAtLineByte(point(ExpX, ExpY), 1));
    end;
  end;


  procedure CheckLogPhys(AName: String; ExpY, ExpX, ExpLogX, ExpOffs:  Integer);
    procedure CheckAtPos;
      procedure CheckAtPosChar;
      begin
        TestCaret2.LineBytePos := point(1, 1);
        TestCaret2.LineCharPos := point(ExpX, ExpY);
        AssertTrue(AName + 'IsAtPos(Char)', TestCaret.IsAtPos(TestCaret2));
        TestCaret2.LineCharPos := point(1, ExpY-1);
        AssertFalse(AName + 'not IsAtPos(Char)', TestCaret.IsAtPos(TestCaret2));
      end;
      procedure CheckAtPosByte;
      begin
        TestCaret2.LineCharPos := point(1, 1);
        TestCaret2.LineBytePos := point(ExpLogX, ExpY);
        //TestCaret2.BytePosOffset := ExpOffs;
        if ExpOffs = 0 then // TODO
        AssertTrue(AName + 'IsAtPos(Byte)', TestCaret.IsAtPos(TestCaret2));
        TestCaret2.LineBytePos := point(1, ExpY-1);
        AssertFalse(AName + 'not IsAtPos(Byte)', TestCaret.IsAtPos(TestCaret2));
      end;
    begin
      if ((TestOrder and 1) = 0) and (ExpX > 0)    then CheckAtPosChar;
      if                             (ExpLogX > 0) then CheckAtPosByte;
      if ((TestOrder and 1) = 1) and (ExpX > 0)    then CheckAtPosChar;
    end;
    procedure CheckPos;
    begin
      if (TestOrder and 2) = 0 then begin
        if ExpX > 0    then CheckPhys(AName, ExpY, ExpX);
        if ExpLogX > 0 then CheckLog (AName, ExpY, ExpLogX, ExpOffs);
      end else begin
        if ExpLogX > 0 then CheckLog (AName, ExpY, ExpLogX, ExpOffs);
        if ExpX > 0    then CheckPhys(AName, ExpY, ExpX);
      end;
    end;
  begin
    if (TestOrder and 8) = 8 then
      CheckAtPos;

    if (TestOrder and 4) = 4 then
      CheckPos;

    if (TestOrder and 2) = 2 then begin
      CheckIsAtChar(AName, ExpY, ExpX);
      CheckIsAtByte(AName, ExpY, ExpLogX, ExpOffs);
    end else begin
      CheckIsAtByte(AName, ExpY, ExpLogX, ExpOffs);
      CheckIsAtChar(AName, ExpY, ExpX);
    end;

    if (TestOrder and 4) = 0 then
      CheckPos;

    if (TestOrder and 8) = 0 then
      CheckAtPos;
  end;


  Procedure DoOneTest(AName: String; AY, AX, ALogX, ALogOffs: Integer;
    ExpY, ExpX, ExpLogX, ExpLogOffs: Integer;
    // X,LogX,LogOffs, [X,LogX,LogOffs]
    AMoveHorizNext: array of integer; AMoveHorizPrev: array of integer;
    ExpMoveHorizFalse: Boolean = False;
    ALineForKeepX: Integer = -1; ExpNotKeptX: Integer = -1; ExpNotKeptLogX: Integer = -1
    );
    procedure DoOneSetLine(Y: Integer);
    begin
      if UseLock then TestCaret.Lock;
      if UseChangeOnTouch then TestCaret.ChangeOnTouch;
      TestCaret.LinePos := Y;
      if UseLock then TestCaret.Unlock;
    end;
    procedure DoOneSetChar(Y, X: Integer; ChangeToLine: Integer = -1);
    begin
      if UseLock then TestCaret.Lock;
      if UseChangeOnTouch then TestCaret.ChangeOnTouch;
      TestCaret.LineCharPos := point(X, Y);
      if ChangeToLine > 0 then TestCaret.LinePos := ChangeToLine;
      if UseLock then TestCaret.Unlock;
    end;
    procedure DoOneSetByte(Y, X, {%H-}O: Integer; ChangeToLine: Integer = -1);
    begin
      if UseLock then TestCaret.Lock;
      if UseChangeOnTouch then TestCaret.ChangeOnTouch;
      TestCaret.LineBytePos := point(X, Y);
      //TestCaret.BytePosOffset := O;
      if ChangeToLine > 0 then TestCaret.LinePos := ChangeToLine;
      if UseLock then TestCaret.Unlock;
    end;
    procedure DoOneMoveHoriz(C: Integer);
    begin
      if UseLock then TestCaret.Lock;
      if UseChangeOnTouch then TestCaret.ChangeOnTouch;
      AssertEquals(AName + 'MoveHoriz is '+dbgs(ExpMoveHorizFalse), not ExpMoveHorizFalse, TestCaret.MoveHoriz(C));
      if UseLock then TestCaret.Unlock;
    end;
    procedure DoOneMoveHorizFromChar(C, Y, X: Integer);
    begin
      if UseLock then TestCaret.Lock;
      if UseChangeOnTouch then TestCaret.ChangeOnTouch;
      TestCaret.LineCharPos := point(X, Y);
      AssertEquals(AName + 'MoveHoriz is '+dbgs(ExpMoveHorizFalse), not ExpMoveHorizFalse, TestCaret.MoveHoriz(C));
      if UseLock then TestCaret.Unlock;
    end;
    procedure DoOneMoveHorizFromByte(C, Y, X, {%H-}O: Integer);
    begin
      if UseLock then TestCaret.Lock;
      if UseChangeOnTouch then TestCaret.ChangeOnTouch;
      TestCaret.LineBytePos := point(X, Y);
      //TestCaret.BytePosOffset := O;
      AssertEquals(AName + 'MoveHoriz is '+dbgs(ExpMoveHorizFalse), not ExpMoveHorizFalse, TestCaret.MoveHoriz(C));
      if UseLock then TestCaret.Unlock;
    end;
  begin
    DoOneSetChar(AY, AX);
    CheckLogPhys(AName + ' from CharPos',  ExpY,  ExpX,  ExpLogX, ExpLogOffs);

    // KeepX
    if (ALineForKeepX > 0) and UseKeepCaretX then begin
      if (TestOrder and 4) = 4 then begin
        TestCaret.LineCharPos := point(1, 1);
        if (TestOrder and 6) = 4 then
          TestCaret.LineCharPos := point(AX, AY);
      end;

      if (TestOrder and 6) = 6
      then DoOneSetChar(AY, AX, ALineForKeepX)
      else DoOneSetLine(ALineForKeepX);
      CheckLogPhys(AName + ' from CharPos',  ALineForKeepX,  ExpNotKeptX,  ExpNotKeptLogX, 0);
      if ExpNotKeptX < 0    then AssertFalse(AName + '(char) keepx moved (c)', ExpX    = TestCaret.CharPos);
      if ExpNotKeptLogX < 0 then AssertFalse(AName + '(char) keepx moved (b)', ExpLogX = TestCaret.BytePos);

      DoOneSetLine(AY);
      CheckLogPhys(AName + ' from CharPos',  ExpY,  ExpX,  ExpLogX, ExpLogOffs);
    end;

    if length(AMoveHorizNext) >= 3 then begin
      DoOneMoveHorizFromChar(1, AY, AX);
      CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizNext[0],  AMoveHorizNext[1], AMoveHorizNext[2]);

      if not ExpMoveHorizFalse then begin
        DoOneMoveHoriz(-1);
        CheckLogPhys(AName + ' from CharPos',  ExpY,  ExpX,  ExpLogX, ExpLogOffs);
      end;
    end;
    if length(AMoveHorizNext) >= 6 then begin
      TestCaret.LineCharPos := point(AX, AY);
      if UseLock then TestCaret.Lock;
      if UseChangeOnTouch then TestCaret.ChangeOnTouch;
      AssertEquals(AName + 'MoveHoriz is '+dbgs(ExpMoveHorizFalse), not ExpMoveHorizFalse, TestCaret.MoveHoriz(1));
      AssertEquals(AName + 'MoveHoriz is '+dbgs(ExpMoveHorizFalse), not ExpMoveHorizFalse, TestCaret.MoveHoriz(1));
      if UseLock then TestCaret.Unlock;
      CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizNext[3],  AMoveHorizNext[4], AMoveHorizNext[5]);

      if not ExpMoveHorizFalse then begin
        DoOneMoveHoriz(-1);
        CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizNext[0],  AMoveHorizNext[1], AMoveHorizNext[2]);
        DoOneMoveHoriz(-1);
        CheckLogPhys(AName + ' from CharPos',  ExpY,  ExpX,  ExpLogX, ExpLogOffs);

        TestCaret.LineCharPos := point(AX, AY);
        DoOneMoveHoriz(2);
        CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizNext[3],  AMoveHorizNext[4], AMoveHorizNext[5]);
      end;
    end;

    if length(AMoveHorizPrev) >= 3 then begin
      DoOneMoveHorizFromChar(-1, AY, AX);
      CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizPrev[0],  AMoveHorizPrev[1], AMoveHorizPrev[2]);

      if not ExpMoveHorizFalse then begin
        DoOneMoveHoriz(1);
        CheckLogPhys(AName + ' from CharPos',  ExpY,  ExpX,  ExpLogX, ExpLogOffs);
      end;
    end;
    if length(AMoveHorizPrev) >= 6 then begin
      TestCaret.LineCharPos := point(AX, AY);
      if UseLock then TestCaret.Lock;
      if UseChangeOnTouch then TestCaret.ChangeOnTouch;
      AssertEquals(AName + 'MoveHoriz is '+dbgs(ExpMoveHorizFalse), not ExpMoveHorizFalse, TestCaret.MoveHoriz(-1));
      AssertEquals(AName + 'MoveHoriz is '+dbgs(ExpMoveHorizFalse), not ExpMoveHorizFalse, TestCaret.MoveHoriz(-1));
      if UseLock then TestCaret.Unlock;
      CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizPrev[3],  AMoveHorizPrev[4], AMoveHorizPrev[5]);

      if not ExpMoveHorizFalse then begin
        DoOneMoveHoriz(1);
        CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizPrev[0],  AMoveHorizPrev[1], AMoveHorizPrev[2]);
        DoOneMoveHoriz(1);
        CheckLogPhys(AName + ' from CharPos',  ExpY,  ExpX,  ExpLogX, ExpLogOffs);

        TestCaret.LineCharPos := point(AX, AY);
        DoOneMoveHoriz(-2);
        CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizPrev[3],  AMoveHorizPrev[4], AMoveHorizPrev[5]);
      end;
    end;



    // Logical
    if ALogOffs <> 0 then exit; // TODO;
    if ALogX > 0 then begin
      DoOneSetByte(AY, ALogX, ALogOffs);
      CheckLogPhys(AName + ' from BytePos',  ExpY,  ExpX,  ExpLogX, ExpLogOffs);

      // KeepX
      if (ALineForKeepX > 0) and UseKeepCaretX then begin
        if (TestOrder and 4) = 4 then begin
          TestCaret.LineBytePos := point(1, 1);
          if (TestOrder and 6) = 4 then
            TestCaret.LineBytePos := point(ALogX, AY);
        end;

        if (TestOrder and 6) = 6
        then DoOneSetByte(AY, ALogX, ALogOffs, ALineForKeepX)
        else DoOneSetLine(ALineForKeepX);
        CheckLogPhys(AName + ' from CharPos',  ALineForKeepX,  ExpNotKeptX,  ExpNotKeptLogX, 0);
        if ExpNotKeptX < 0    then AssertFalse(AName + '(char) keepx moved (c)', ExpX    = TestCaret.CharPos);
        if ExpNotKeptLogX < 0 then AssertFalse(AName + '(char) keepx moved (b)', ExpLogX = TestCaret.BytePos);

        DoOneSetLine(AY);
        CheckLogPhys(AName + ' from CharPos',  ExpY,  ExpX,  ExpLogX, ExpLogOffs);
      end;

      if length(AMoveHorizNext) >= 3 then begin
        DoOneMoveHorizFromByte(1, AY, ALogX, ALogOffs);
        CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizNext[0],  AMoveHorizNext[1], AMoveHorizNext[2]);

        if not ExpMoveHorizFalse then begin
          DoOneMoveHoriz(-1);
          CheckLogPhys(AName + ' from CharPos',  ExpY,  ExpX,  ExpLogX, ExpLogOffs);
        end;
      end;
      if length(AMoveHorizNext) >= 6 then begin
        TestCaret.LineBytePos := point(ALogX, AY);
        if UseLock then TestCaret.Lock;
        if UseChangeOnTouch then TestCaret.ChangeOnTouch;
        TestCaret.MoveHoriz(1);
        TestCaret.MoveHoriz(1);
        if UseLock then TestCaret.Unlock;
        CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizNext[3],  AMoveHorizNext[4], AMoveHorizNext[5]);

        if not ExpMoveHorizFalse then begin
          DoOneMoveHoriz(-1);
          CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizNext[0],  AMoveHorizNext[1], AMoveHorizNext[2]);
          DoOneMoveHoriz(-1);
          CheckLogPhys(AName + ' from CharPos',  ExpY,  ExpX,  ExpLogX, ExpLogOffs);

          TestCaret.LineBytePos := point(ALogX, AY);
          DoOneMoveHoriz(2);
          CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizNext[3],  AMoveHorizNext[4], AMoveHorizNext[5]);
        end;
      end;

      if length(AMoveHorizPrev) >= 3 then begin
        DoOneMoveHorizFromByte(-1, AY, ALogX, ALogOffs);
        CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizPrev[0],  AMoveHorizPrev[1], AMoveHorizPrev[2]);

        if not ExpMoveHorizFalse then begin
          DoOneMoveHoriz(1);
          CheckLogPhys(AName + ' from CharPos',  ExpY,  ExpX,  ExpLogX, ExpLogOffs);
        end;
      end;
      if length(AMoveHorizPrev) >= 6 then begin
        TestCaret.LineBytePos := point(ALogX, AY);
        if UseLock then TestCaret.Lock;
        if UseChangeOnTouch then TestCaret.ChangeOnTouch;
        TestCaret.MoveHoriz(-1);
        TestCaret.MoveHoriz(-1);
        if UseLock then TestCaret.Unlock;
        CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizPrev[3],  AMoveHorizPrev[4], AMoveHorizPrev[5]);

        if not ExpMoveHorizFalse then begin
          DoOneMoveHoriz(1);
          CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizPrev[0],  AMoveHorizPrev[1], AMoveHorizPrev[2]);
          DoOneMoveHoriz(1);
          CheckLogPhys(AName + ' from CharPos',  ExpY,  ExpX,  ExpLogX, ExpLogOffs);

          TestCaret.LineBytePos := point(ALogX, AY);
          DoOneMoveHoriz(-2);
          CheckLogPhys(AName + ' from CharPos',  ExpY,  AMoveHorizPrev[3],  AMoveHorizPrev[4], AMoveHorizPrev[5]);
        end;
      end;

    end;

  end;

  procedure DoTests;
  begin
    if (TestOrder >= 4) and (UseIncAllowPastEOL or UseIncAdjustToNextChar) then
      exit;

    ReCreateEdit;
    SynEdit.TabWidth := 6;
    SetLines(['x',
              StringOfChar('x', 40),
              ' äääbc', // EOL = 7, 10
              'X嗚呼あ嗚呼嗚呼あ嗚呼嗚呼あ嗚呼嗚呼あ',
              '嗚呼嗚呼あ嗚呼嗚呼あ嗚呼嗚呼あ嗚呼嗚呼あ',
              ' '#9#9#9'mn',
              '',
              'X ab',
              '']);
    try
      PushBaseName(Format('UseLock=%s, AdjustToNextChar=%s, IncAdjustToNextChar=%s,'+
                          ' AllowPastEOL=%s, IncAllowPastEOL=%s, KeepCaretX=%s,' +
                          ' ChangeOnTouch=%s, IncAutoMoveOnEdit=%s, SkipTabs=%s' +
                          ' MaxLeft=%s',
                          [dbgs(UseLock), dbgs(UseAdjustToNextChar), dbgs(UseIncAdjustToNextChar),
                           dbgs(UseAllowPastEOL), dbgs(UseIncAllowPastEOL),
                           dbgs(UseKeepCaretX), dbgs(UseChangeOnTouch),
                           dbgs(UseIncAutoMoveOnEdit), dbgs(UseSkipTabs), dbgs(UseMaxLeft)
                          ]));

//debugln(BaseTestName);

      TestCaret := TSynEditCaret.Create;
      TestCaret.Lines := SynEdit.ViewedTextBuffer;

      TestCaret.AdjustToNextChar := UseAdjustToNextChar;
      if UseIncAdjustToNextChar then TestCaret.IncForceAdjustToNextChar;

      TestCaret.AllowPastEOL := UseAllowPastEOL;
      if UseIncAllowPastEOL then TestCaret.IncForcePastEOL;

      TestCaret.KeepCaretX := UseKeepCaretX;
      if UseIncAutoMoveOnEdit then TestCaret.IncAutoMoveOnEdit;
      TestCaret.SkipTabs := UseSkipTabs;
      if UseMaxLeft then
        TestCaret.MaxLeftChar := @TestMaxLeftProc; // 6000

      TestCaret2 := TSynEditCaret.Create;
      TestCaret2.Lines := SynEdit.ViewedTextBuffer;



      if UseAdjustToNextChar or UseIncAdjustToNextChar
      then DoOneTest('Basic',     2, 3, 3, 0,    2, 3, 3, 0,
                     [4,4,0,  5,5,0], [2,2,0,  1,1,0], False,   // MoveHoriz
                     4, 4, 5  // KeepX
                    )
      else DoOneTest('Basic',     2, 3, 3, 0,    2, 3, 3, 0,
                     [4,4,0,  5,5,0], [2,2,0,  1,1,0], False,    // MoveHoriz
                     4, 2, 2  // KeepX
                    );

      // past EOL
      if UseAllowPastEOL or UseIncAllowPastEOL
      then DoOneTest('past EOL',     8, 9, 9, 0,    8, 9, 9, 0,
                     [10,10,0], [8,8,0])
      else DoOneTest('past EOL',     8, 9, 9, 0,    8, 5, 5, 0,
                     [5,5,0,  5,5,0],[], True);
      // BOL
      DoOneTest('at BOL',     8, 1, 1, 0,    8, 1, 1, 0,
                [],[1,1,0,  1,1,0], True);

      // one past EOL
      if UseAllowPastEOL or UseIncAllowPastEOL
      then DoOneTest('one past EOL',     3, 8,11, 0,    3, 8,11, 0,
                     [ 9,12,0,  10,13,0],  [7,10,0,  6,9,0])
      else DoOneTest('one past EOL',     3, 8,11, 0,    3, 7,10, 0,
                     [7,10,0,  7,10,0], [], True);

      // MaxLeftChar 6000 (5999 char / EOL = 6000)
      if UseAllowPastEOL or UseIncAllowPastEOL
      then if UseMaxLeft
           then DoOneTest('past EOL',     3, 6001, 6004, 0,    3, 6000, 6003, 0,
                          [6000, 6003, 0], [], True)
           else DoOneTest('past EOL',     3, 6001, 6004, 0,    3, 6001, 6004, 0,
                          [], [])
      else DoOneTest('past EOL',     3, 6001, 6004, 0,    3, 7,10, 0,
                     [],[]
                    );


      // ' äääbc'
      if UseAdjustToNextChar or UseIncAdjustToNextChar
      then DoOneTest('LogPhys',   3, 4, 6, 0,    3, 4, 6, 0,
                     [5,8,0,  6,9,0], [3,4,0,  2,2,0], False,
                     5, 5, 7
                    )
      else DoOneTest('LogPhys',   3, 4, 6, 0,    3, 4, 6, 0,
                     [5,8,0,  6,9,0], [3,4,0,  2,2,0], False,
                     5, 3, 4
                    );

      // 'X嗚呼あ'  // skip "from byte"
      if UseAdjustToNextChar or UseIncAdjustToNextChar
      then DoOneTest('Mid Dbl-Width',   4, 3, -3, 0,    4, 4, 5, 0,
                     [6,8,0], [2,2,0], False,
                     5, 5, 7
                    )
      else DoOneTest('Mid Dbl-Width',   4, 3, -3, 0,    4, 2, 2, 0,
                     [4,5,0], [1,1,0], False,
                     5, 1, 1
                    );

      // ' '#9#9#9'mn'   // skip "from byte" TODO
      if UseSkipTabs
      then if UseAdjustToNextChar or UseIncAdjustToNextChar
           then DoOneTest('Mid Tab',   6, 8, -3, 1,    6,13, 4, 0,
                          [19,5,0,  20,6,0],  [7,3,0,  2,2,0], False
                          //7,1,1
                         )
           else if UseAllowPastEOL or UseIncAllowPastEOL
                then DoOneTest('Mid Tab',   6, 8, -3, 1,    6, 7, 3, 0,
                               [13,4,0,  19,5,0],  [2,2,0,  1,1,0], False
                              )
                else DoOneTest('Mid Tab',   6, 8, -3, 1,    6, 7, 3, 0,
                               [13,4,0,  19,5,0],  [2,2,0,  1,1,0], False,
                               7,1,1
                              )
      else if UseAllowPastEOL or UseIncAllowPastEOL
           then DoOneTest('Mid Tab',   6, 8, -3, 1,    6, 8, 3, 1,
                          [9,3,2,  10,3,3],  [7,3,0,  6,2,4],  False
                         )
           else DoOneTest('Mid Tab',   6, 8, -3, 1,    6, 8, 3, 1,
                          [9,3,2,  10,3,3],  [7,3,0,  6,2,4], False,
                          7,1,1
                         );



    finally
      PopBaseName;
      FreeAndNil(TestCaret);
      FreeAndNil(TestCaret2);
    end;
  end;

begin
  FPersistSel := False;
  for TestOrder := 0 to 11 do // CheckAtPos (8) only runs first 4
    for UseLock := low(Boolean) to high(Boolean) do
      for UseAdjustToNextChar := low(Boolean) to high(Boolean) do
      for UseIncAdjustToNextChar := low(Boolean) to high(Boolean) do
        for UseAllowPastEOL := low(Boolean) to high(Boolean) do
        for UseIncAllowPastEOL := low(Boolean) to high(Boolean) do
          for UseKeepCaretX := low(Boolean) to high(Boolean) do
            for UseChangeOnTouch := low(Boolean) to high(Boolean) do
              for UseIncAutoMoveOnEdit := low(Boolean) to high(Boolean) do
                for UseSkipTabs := low(Boolean) to high(Boolean) do
                  for UseMaxLeft := low(Boolean) to high(Boolean) do
// OldPos, MoveHoriz
// IsAtPos /
    DoTests;

end;

procedure TTestBasicSynEdit.TestCaretAutoMove;

  procedure DoTest(name: string; y, x, insertY, insertX, InsertY2, InsertX2: integer;
                   txt: string; expY, expX: Integer);
  begin
    name := name + ' y='+inttostr(y)+' x='+inttostr(x);
    if y > 0 then begin
      ReCreateEdit;
      SynEdit.TabWidth := 6;
      SetLines(['x', 'abc', ' ääX', #9'mn', 'abc'#9'de', #9'Xää.']);
      SynEdit.CaretXY := Point(x, y);
    end;

    SynEdit.TextBetweenPointsEx[Point(insertX, insertY), point(insertX2, InsertY2), scamAdjust]
      := txt;
debugln(dbgstr(SynEdit.Text));
    TestIsCaretPhys(name, expX, expY);
  end;

const
  cr = LineEnding;
begin
  FPersistSel := False;

  DoTest('simple insert',              2,2,   2,1,  2,1, 'X',      2,3);
  DoTest('simple insert CR',           2,2,   2,1,  2,1, 'X'+cr,   3,2);
  DoTest('simple insert CR+',          2,2,   2,1,  2,1, cr+'X',   3,3);
  DoTest('simple delete',              2,2,   2,1,  2,2, '',       2,1);
  DoTest('simple delete CR',           2,2,   1,2,  2,1, '',       1,3);
  DoTest('+simple delete CR',          2,2,   1,1,  2,1, '',       1,2);

  DoTest('simple insert (eol)',        2,4,   2,1,  2,1, 'X',   2,5);
  DoTest('simple insert (past eol)',   2,7,   2,1,  2,1, 'X',   2,8);

  DoTest('insert with tab',            4,8,   4,1,  4,1, 'X',      4,8);
  DoTest('insert with tab (cont)',    -4,8,   4,2,  4,2, 'Y',      4,8);
  DoTest('insert with tab (cont)',    -4,8,   4,3,  4,3, 'abc',    4,8);
  DoTest('insert with tab (cont)',    -4,8,   4,6,  4,6, 'Z',      4,14);
  DoTest('insert with tab (cont)',    -4,8,   4,7,  4,7, '.',      4,14);
  DoTest('delete with tab (cont)',    -4,8,   4,1,  4,2, '',       4,14);
  DoTest('delete with tab (cont)',    -4,8,   4,1,  4,2, '',       4,8);
  DoTest('delete with tab (cont)',    -4,8,   4,1,  4,2, '',       4,8);
  DoTest('delete with tab (cont)',    -4,8,   4,1,  4,2, '',       4,8);


  SynEdit.CaretObj.IncAutoMoveOnEdit;
  DoTest('insert with tab (am-block)',            4,8,   4,1,  4,1, 'X',      4,8);
  DoTest('insert with tab (am-block) (cont)',    -4,8,   4,2,  4,2, 'Y',      4,8);
  DoTest('insert with tab (am-block) (cont)',    -4,8,   4,3,  4,3, 'abc',    4,8);
  DoTest('insert with tab (am-block) (cont)',    -4,8,   4,6,  4,6, 'Z',      4,14);
  DoTest('insert with tab (am-block) (cont)',    -4,8,   4,7,  4,7, '.',      4,14);
  DoTest('delete with tab (cont)',    -4,8,   4,1,  4,2, '',       4,14);
  DoTest('delete with tab (cont)',    -4,8,   4,1,  4,2, '',       4,8);
  DoTest('delete with tab (cont)',    -4,8,   4,1,  4,2, '',       4,8);
  DoTest('delete with tab (cont)',    -4,8,   4,1,  4,2, '',       4,8);
  SynEdit.CaretObj.DecAutoMoveOnEdit;

end;

procedure TTestBasicSynEdit.TestCaretEcDeleteWord;
begin
  FPersistSel := False;
  FDoInit := @DoInit1;
  FTestLines := @TestLines1;
  FTestCommand := ecDeleteWord;
  AllowPastEOL := True;
  PushBaseName('ecDeleteWord');

  // if in middle of word, keep spaces after
  TestCommand('simple "te|xt"',                    8, 1,    8, 1, [1,'Some te to test'],
                                                              8, 1, [1,'Some teto test']);
  // if at end of word, just del spaces after
  TestCommand('simple EOW "text|"',               10, 1,   10, 1, [1,'Some textto test'],
                                                             10, 1, [1,'Some text test']);
  // if at start of word, do NOT keep spaces after
  TestCommand('simple BOW "|text"',                6, 1,    6, 1, [1,'Some to test'],
                                                              6, 1, [1,'Some test']);
  TestCommand('simple EOT "li|ne"',               10,15,   10,15, [15,'normal li'],
                                                             10,15, [15,'normal li']);
  TestCommand('simple > EOL, next line "te|st"',  16, 1,   16, 1, [1,'Some text to te'],
                                                             16, 1, [1, 1,'Some text to teFoo bar abc']);

  TestCommand('tab "li|ne"',                      16, 3,   16, 3, [3, #9'Other li with tab'],
                                                             16, 3, [3, #9'Other liwith tab']);
  TestCommand('tab EOW "line|"',                  18, 3,   18, 3, [3, #9'Other linewith tab'],
                                                             18, 3, [3, #9'Other line tab']);
  TestCommand('tab BOW "|line"',                  14, 3,   14, 3, [3, #9'Other with tab'],
                                                             14, 3, [3, #9'Other tab']);
  TestCommand('tab > EOL, next-line',             25, 3,   25, 3, [3, #9'Other line with t'],
                                                             25, 3, [3, 3, #9'Other line with ttab'#9'in the middle']);

  TestCommand('M-tab "t|ab"',                      2, 4,    2, 4, [4, 't'#9'in the middle'],
                                                              2, 4, [4, 'tin the middle']);
  TestCommand('M-tab "tab|"',                      4, 4,    4, 4, [4, 'tabin the middle'],
                                                              4, 4, [4, 'tab the middle']);
  TestCommand('M-tab > EOL, next-line-tab',       17, 4,   17, 4, [4, 'tab'#9'in the mi'],
                                                             17, 4, [4, 4, 'tab'#9'in the mitab'#9' in the middle with space']);

  //TestCommand('M-S-tab BOW "t|ab"',                2, 5,    9, 5,
  //                                                           12, 5);
  //TestCommand('M-S-tab EOW "tab|"',                4, 5,    9, 5,
  //                                                           12, 5);
  //TestCommand('M-S-tab BOW "|tab"',                1, 5,    9, 5,
  //                                                           12, 5);
  //TestCommand('M-S-tab "tab#9| "',                 5, 5,    9, 5,
  //                                                           12, 5);
  //
  TestCommand('Umlaut "ää|ä"',                    11, 6,   11, 6, [6, 'umlaute ää in text'],
                                                             11, 6, [6, 'umlaute ääin text']);
  TestCommand('Umlaut EOW "äää|"',                12, 6,   12, 6, [6, 'umlaute äääin text'],
                                                             12, 6, [6, 'umlaute äää text']);
  TestCommand('Umlaut BOW "|äää"',                 9, 6,    9, 6, [6, 'umlaute in text'],
                                                              9, 6, [6, 'umlaute text']);
  TestCommand('Umlaut "umlaute|"',                 8, 6,    8, 6, [6, 'umlauteäää in text'],
                                                              8, 6, [6, 'umlaute in text']);
  TestCommand('After Umlaut > EOL, next line',    18, 6,   18, 6, [6, 'umlaute äää in te'],
                                                             18, 6, [6, 6, 'umlaute äää in te'+FTestLines()[6]]);
  // past eol
  TestCommand('Umlaut EOW "äää in text |"',                21, 6,   21, 6, [6, 6, 'umlaute äää in text normal line']);

  //TestCommand('Before untrimmed > next',           12, 7,   4, 8,
  //                                                           14, 8);
  //TestCommand('untrimmed > EOL, next',             30, 8,  35, 8,
  //                                                            1, 9);
  //TestCommand('Before untrimmed tab > next',       12, 9,   8,10,
  //                                                           15,10);
  //TestCommand('untrimmed tab > EOL, next',         24,10,  29,10,
  //                                                            1,11);
  //
  //TestCommand('Before empty > next',                12,11,  1,12);
  //TestCommand('Before space empty > next',          12,13,  1,14,
  //                                                            6, 14);
end;

procedure TTestBasicSynEdit.TestCaretEcDeleteLastWord;
begin
  FPersistSel := False;
  FDoInit := @DoInit1;
  FTestLines := @TestLines1;
  FTestCommand := ecDeleteLastWord;
  AllowPastEOL := True;
  PushBaseName('ecDeleteLastWord');

  TestCommand('simple "te|st"',                   16, 1,   14, 1, [1,'Some text to st'],
                                                            11, 1, [1,'Some text st']);
  TestCommand('simple EOW "test|"',               18, 1,   14, 1, [1,'Some text to '],
                                                            11, 1, [1,'Some text ']);
  TestCommand('simple BOW "|test"',               14, 1,   11, 1, [1,'Some text test'],
                                                             6, 1, [1,'Some test']);
  TestCommand('simple > BOT "So|me"',              3, 1,    1, 1, [1,'me text to test'],
                                                             1, 1, [1,'me text to test']);
  TestCommand('simple > prev-line "F|oo"',         2, 2,    1, 2, [2,'oo bar abc'],
                                                            18, 1, [1, 1, 'Some text to testoo bar abc']);
  TestCommand('simple > prev-line "|Foo"',         1, 2,   18, 1, [1, 1, 'Some text to testFoo bar abc'],
                                                            14, 1, [1, 1, 'Some text to Foo bar abc']);

  TestCommand('tab "wi|th"',                      21, 3,   19, 3, [3, #9'Other line th tab'],
                                                            14, 3, [3, #9'Other th tab']);
  TestCommand('tab EOW "with|"',                  23, 3,   19, 3, [3, #9'Other line  tab'],
                                                            14, 3, [3, #9'Other  tab']);
  TestCommand('tab BOW "|with"',                  19, 3,   14, 3, [3, #9'Other with tab'],
                                                             8, 3, [3, #9'with tab']);
  TestCommand('tab > prev-line "O|ther"',          9, 3,    8, 3, [3, #9'ther line with tab'],
                                                            12, 2, [2, 2, 'Foo bar abcther line with tab']);

  TestCommand('M-tab "i|n"',                       9, 4,    8, 4, [4, 'tab'#9'n the middle'],
                                                             1, 4, [4, 'n the middle']);
  TestCommand('M-tab > prev-line-tab "ta|b"',      3, 4,    1, 4, [4, 'b'#9'in the middle'],
                                                            27, 3, [3, 3, #9'Other line with tabb'#9'in the middle']);

  //TestCommand('M-S-tab "i|n"',                    10, 5,    9, 5,
  //                                                           1, 5);
  //TestCommand('M-S-EOW tab "in|"',                11, 5,    9, 5,
  //                                                           1, 5);
  //TestCommand('M-S-BOW tab "|in"',                 9, 5,    1, 5,
  //                                                          21, 4);
  //TestCommand('M-S-tab "#9| in"',                  8, 5,    1, 5);
  //
  TestCommand('Umlaut "ää|ä"',                    11, 6,    9, 6, [6, 'umlaute ä in text'],
                                                             1, 6, [6, 'ä in text']);
  TestCommand('Umlaut EOW "äää|"',                12, 6,    9, 6, [6, 'umlaute  in text'],
                                                             1, 6, [6, ' in text']);
  TestCommand('Umlaut BOW "|äää"',                 9, 6,    1, 6, [6, 'äää in text'],
                                                            33, 5, [5, 5, FTestLines()[4]+'äää in text']);
  TestCommand('Umlaut "i|n"',                     14, 6,   13, 6, [6, 'umlaute äää n text'],
                                                             9, 6, [6, 'umlaute n text']);
  TestCommand('After Umlaut > prev line',          1, 7,   20, 6, [6, 6, 'umlaute äää in text'+FTestLines()[6]],
                                                            16, 6, [6, 6, 'umlaute äää in '+FTestLines()[6]]);

  //TestCommand('untrimmed "un|trimmed"',            6, 8,    4, 8,
  //                                                          12, 7);
  //TestCommand('After untrimmed > prev',            1, 9,   35, 8,
  //                                                          28, 8);
  //TestCommand('untrimmed tab "t|ab"',              9,10,    8,10,
  //                                                          12, 9);
  //TestCommand('After untrimmed tab > prev',        1,11,   29,10,
  //                                                          22,10);
  //
  //TestCommand('After empty > prev',                1,13,    1,12);
  //TestCommand('After space empty > prev',          1,15,    6,14);
end;

procedure TTestBasicSynEdit.TestCaretEcDeleteEOL;
begin
  FPersistSel := False;
  FDoInit := @DoInit1;
  FTestLines := @TestLines1;
  FTestCommand := ecDeleteEOL;
  AllowPastEOL := True;
  PushBaseName('ecDeleteEOL');

  TestCommand('simple "te|xt"',                    8, 1,    8, 1, [1,'Some te']);
  // if at end of line
  TestCommand('simple EOL "text|"',               18, 1,   18, 1, []);
  // if at start of line
  TestCommand('simple BOL "|text"',                1, 1,    1, 1, [1,'']);

  // utf8
  TestCommand('utf8 "te|xt"',                   10, 6,   10, 6, [6,'umlaute ä']);
  // if at end of line
  TestCommand('utf8 EOL "text|"',               20, 6,   20, 6, []);
  // if at start of line
  TestCommand('utf8 BOL "|text"',                1, 6,    1, 6, [6,'']);

  //tab
  TestCommand('tab "te|#9xt"',                 11, 10,   11, 10, [10,#9'tab']);
  TestCommand('tab "te#9|xt"',                 15, 10,   15, 10, [10,#9'tab'#9]);
  // if at end of line
  TestCommand('tab EOL "text|"',               29, 10,   29, 10, []);
  // if at start of line
  TestCommand('tab BOL "|text"',                1, 10,    1, 10, [10,'']);

end;

procedure TTestBasicSynEdit.TestWordBreaker;
var
  WBrker: TSynWordBreaker;
begin
  FPersistSel := False;
  WBrker := TSynWordBreaker.Create;
  WBrker.IdentChars := ['a'..'z', 'A'..'Z', '0'..'9']; // not used
  WBrker.WhiteChars := [' ', #9];
  WBrker.WordBreakChars := [',', '.', '-', ';' ,':'];

  AssertEquals('', True,  WBrker.IsInWord('abc  123  ... aa', 1));
  AssertEquals('', True,  WBrker.IsInWord('abc  123  ... aa', 2));
  AssertEquals('', True,  WBrker.IsInWord('abc  123  ... aa', 3));
  AssertEquals('', True,  WBrker.IsInWord('abc  123  ... aa', 4));
  AssertEquals('', False, WBrker.IsInWord('abc  123  ... aa', 5));
  AssertEquals('', True,  WBrker.IsInWord('abc  123  ... aa', 6));
  AssertEquals('', True,  WBrker.IsInWord('abc  123  ... aa', 7));
  AssertEquals('', True,  WBrker.IsInWord('abc  123  ... aa', 9));
  AssertEquals('', False, WBrker.IsInWord('abc  123  ... aa', 10));
  AssertEquals('', False, WBrker.IsInWord('abc  123  ... aa', 11)); // before ...
  AssertEquals('', True,  WBrker.IsInWord('abc  123  ... aa', 15)); // |aa
  AssertEquals('', True,  WBrker.IsInWord('abc  123  ... aa', 17)); // at eol
  AssertEquals('', False, WBrker.IsInWord('abc  123  ... aa', 18));
  AssertEquals('', False, WBrker.IsInWord('abc  123  ... aa', 19));
  AssertEquals('', False, WBrker.IsInWord('abc  123  ... aa', 0));
  AssertEquals('', False, WBrker.IsInWord('abc  123  ... aa', -1));
  AssertEquals('', False, WBrker.IsInWord(' ', 1));
  AssertEquals('', False, WBrker.IsInWord(' ', 2));
  AssertEquals('', False, WBrker.IsInWord('', 1));
  AssertEquals('', False, WBrker.IsInWord('...', 1));
  AssertEquals('', False, WBrker.IsInWord('...', 2));
  AssertEquals('', False, WBrker.IsInWord('...', 4));
  AssertEquals('', False, WBrker.IsInWord('..aa', 2));
  AssertEquals('', True,  WBrker.IsInWord('..aa', 3));
  AssertEquals('', True,  WBrker.IsInWord('..aa', 4));
  AssertEquals('', True,  WBrker.IsInWord('..aa', 5));


  AssertEquals('', True,  WBrker.IsAtWordStart('abc  123  ... aa', 1));
  AssertEquals('', False, WBrker.IsAtWordStart('abc  123  ... aa', 2));
  AssertEquals('', False, WBrker.IsAtWordStart('abc  123  ... aa', 3));
  AssertEquals('', False, WBrker.IsAtWordStart('abc  123  ... aa', 4));
  AssertEquals('', False, WBrker.IsAtWordStart('abc  123  ... aa', 5));
  AssertEquals('', True,  WBrker.IsAtWordStart('abc  123  ... aa', 6));
  AssertEquals('', False, WBrker.IsAtWordStart('abc  123  ... aa', 7));
  AssertEquals('', False, WBrker.IsAtWordStart('abc  123  ... aa', 9));
  AssertEquals('', False, WBrker.IsAtWordStart('abc  123  ... aa', 10));
  AssertEquals('', False, WBrker.IsAtWordStart('abc  123  ... aa', 11)); // before ...
  AssertEquals('', True,  WBrker.IsAtWordStart('abc  123  ... aa', 15)); // |aa
  AssertEquals('', False, WBrker.IsAtWordStart('abc  123  ... aa', 17)); // at eol
  AssertEquals('', False, WBrker.IsAtWordStart('abc  123  ... aa', 18));
  AssertEquals('', False, WBrker.IsAtWordStart('abc  123  ... aa', 19));
  AssertEquals('', False, WBrker.IsAtWordStart('abc  123  ... aa', 0));
  AssertEquals('', False, WBrker.IsAtWordStart('abc  123  ... aa', -1));
  AssertEquals('', False, WBrker.IsAtWordStart(' ', 1));
  AssertEquals('', False, WBrker.IsAtWordStart('', 1));
  AssertEquals('', False, WBrker.IsAtWordStart(' ', 2));
  AssertEquals('', False, WBrker.IsAtWordStart('...', 1));
  AssertEquals('', True,  WBrker.IsAtWordStart('..aa', 3));

  AssertEquals('', False, WBrker.IsAtWordEnd('abc  123  ... aa', 1));
  AssertEquals('', False, WBrker.IsAtWordEnd('abc  123  ... aa', 2));
  AssertEquals('', False, WBrker.IsAtWordEnd('abc  123  ... aa', 3));
  AssertEquals('', True,  WBrker.IsAtWordEnd('abc  123  ... aa', 4));
  AssertEquals('', False, WBrker.IsAtWordEnd('abc  123  ... aa', 5));
  AssertEquals('', False, WBrker.IsAtWordEnd('abc  123  ... aa', 6));
  AssertEquals('', False, WBrker.IsAtWordEnd('abc  123  ... aa', 7));
  AssertEquals('', True,  WBrker.IsAtWordEnd('abc  123  ... aa', 9));
  AssertEquals('', False, WBrker.IsAtWordEnd('abc  123  ... aa', 10));
  AssertEquals('', False, WBrker.IsAtWordEnd('abc  123  ... aa', 11)); // before ...
  AssertEquals('', False, WBrker.IsAtWordEnd('abc  123  ... aa', 15)); // |aa
  AssertEquals('', True,  WBrker.IsAtWordEnd('abc  123  ... aa', 17)); // at eol
  AssertEquals('', False, WBrker.IsAtWordEnd('abc  123  ... aa', 18));
  AssertEquals('', False, WBrker.IsAtWordEnd('abc  123  ... aa', 19));
  AssertEquals('', False, WBrker.IsAtWordEnd('abc  123  ... aa', 0));
  AssertEquals('', False, WBrker.IsAtWordEnd('abc  123  ... aa', -1));
  AssertEquals('', False, WBrker.IsAtWordEnd(' ', 1));
  AssertEquals('', False, WBrker.IsAtWordEnd(' ', 2));
  AssertEquals('', False, WBrker.IsAtWordEnd('', 1));
  AssertEquals('', False, WBrker.IsAtWordEnd('...', 1));
  AssertEquals('', False, WBrker.IsAtWordEnd('..aa', 3));

  AssertEquals('', 6, WBrker.NextWordStart('abc  123  ... aa', 1, False));
  AssertEquals('', 6, WBrker.NextWordStart('abc  123  ... aa', 2, False));
  AssertEquals('', 6, WBrker.NextWordStart('abc  123  ... aa', 3, False));
  AssertEquals('', 6, WBrker.NextWordStart('abc  123  ... aa', 4, False)); // abc|
  AssertEquals('', 6, WBrker.NextWordStart('abc  123  ... aa', 5, False));
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 6, False)); // |123
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 7, False)); // 1|23
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 8, False));
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 9, False)); // 123|
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 10, False));
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 11, False)); // |...
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 12, False));
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 13, False));
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 14, False)); // ...|
  AssertEquals('',-1, WBrker.NextWordStart('abc  123  ... aa', 15, False));
  AssertEquals('',-1, WBrker.NextWordStart('abc  123  ... aa', 16, False));
  AssertEquals('',-1, WBrker.NextWordStart('abc  123  ... aa', 17, False)); // at eol
  AssertEquals('',-1, WBrker.NextWordStart('abc  123  ... aa', 18, False));
  AssertEquals('',-1, WBrker.NextWordStart('abc  123  ... aa', 19, False));
  AssertEquals('',-1, WBrker.NextWordStart('abc  123  ... aa', 0, False));
  AssertEquals('',-1, WBrker.NextWordStart('abc  123  ... aa', -1, False));
  AssertEquals('',-1, WBrker.NextWordStart('', 1, False));
  AssertEquals('',-1, WBrker.NextWordStart(' ', 1, False));
  AssertEquals('',-1, WBrker.NextWordStart(' ', 2, False));
  AssertEquals('',3,  WBrker.NextWordStart('..aa', 1, False));
  AssertEquals('',3,  WBrker.NextWordStart('..aa', 2, False));
  AssertEquals('',-1, WBrker.NextWordStart('..aa', 3, False));
  AssertEquals('',-1, WBrker.NextWordStart('..aa', 4, False));
  AssertEquals('', 3, WBrker.NextWordStart('a a 1', 1, False));
  AssertEquals('', 3, WBrker.NextWordStart('a a 1', 2, False));
  AssertEquals('', 5, WBrker.NextWordStart('a a 1', 3, False));
  AssertEquals('', 5, WBrker.NextWordStart('a a 1', 4, False));
  AssertEquals('',-1, WBrker.NextWordStart('a a 1', 5, False));
  AssertEquals('',-1, WBrker.NextWordStart('a a 1', 6, False));
  AssertEquals('', 2, WBrker.NextWordStart(' a ', 1, False));
  AssertEquals('',-1, WBrker.NextWordStart(' a ', 2, False));
  AssertEquals('',-1, WBrker.NextWordStart(' a ', 3, False));
  AssertEquals('',-1, WBrker.NextWordStart(' a ', 4, False));

  AssertEquals('', 1, WBrker.NextWordStart('abc  123  ... aa', 1, True));
  AssertEquals('', 6, WBrker.NextWordStart('abc  123  ... aa', 2, True));
  AssertEquals('', 6, WBrker.NextWordStart('abc  123  ... aa', 3, True));
  AssertEquals('', 6, WBrker.NextWordStart('abc  123  ... aa', 4, True)); // abc|
  AssertEquals('', 6, WBrker.NextWordStart('abc  123  ... aa', 5, True));
  AssertEquals('', 6, WBrker.NextWordStart('abc  123  ... aa', 6, True)); // |123
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 7, True)); // 1|23
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 8, True));
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 9, True)); // 123|
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 10, True));
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 11, True)); // |...
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 12, True));
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 13, True));
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 14, True)); // ...|
  AssertEquals('',15, WBrker.NextWordStart('abc  123  ... aa', 15, True));
  AssertEquals('',-1, WBrker.NextWordStart('abc  123  ... aa', 16, True));
  AssertEquals('',-1, WBrker.NextWordStart('abc  123  ... aa', 17, True)); // at eol
  AssertEquals('',-1, WBrker.NextWordStart('abc  123  ... aa', 18, True));
  AssertEquals('',-1, WBrker.NextWordStart('abc  123  ... aa', 19, True));
  AssertEquals('',-1, WBrker.NextWordStart('abc  123  ... aa', 0, True));
  AssertEquals('',-1, WBrker.NextWordStart('abc  123  ... aa', -1, True));
  AssertEquals('',-1, WBrker.NextWordStart('', 1, True));
  AssertEquals('',-1, WBrker.NextWordStart(' ', 1, True));
  AssertEquals('',-1, WBrker.NextWordStart(' ', 2, True));
  AssertEquals('',3,  WBrker.NextWordStart('..aa', 1, True));
  AssertEquals('',3,  WBrker.NextWordStart('..aa', 2, True));
  AssertEquals('',3,  WBrker.NextWordStart('..aa', 3, True));
  AssertEquals('',-1, WBrker.NextWordStart('..aa', 4, True));
  AssertEquals('', 1, WBrker.NextWordStart('a a 1', 1, True));
  AssertEquals('', 3, WBrker.NextWordStart('a a 1', 2, True));
  AssertEquals('', 3, WBrker.NextWordStart('a a 1', 3, True));
  AssertEquals('', 5, WBrker.NextWordStart('a a 1', 4, True));
  AssertEquals('', 5, WBrker.NextWordStart('a a 1', 5, True));
  AssertEquals('',-1, WBrker.NextWordStart('a a 1', 6, True));
  AssertEquals('', 2, WBrker.NextWordStart(' a ', 1, True));
  AssertEquals('', 2, WBrker.NextWordStart(' a ', 2, True));
  AssertEquals('',-1, WBrker.NextWordStart(' a ', 3, True));
  AssertEquals('',-1, WBrker.NextWordStart(' a ', 4, True));

  AssertEquals('', 4, WBrker.NextWordEnd('abc  123  ... aa', 1, False));
  AssertEquals('', 4, WBrker.NextWordEnd('abc  123  ... aa', 2, False));
  AssertEquals('', 4, WBrker.NextWordEnd('abc  123  ... aa', 3, False));
  AssertEquals('', 9, WBrker.NextWordEnd('abc  123  ... aa', 4, False)); // abc|
  AssertEquals('', 9, WBrker.NextWordEnd('abc  123  ... aa', 5, False));
  AssertEquals('', 9, WBrker.NextWordEnd('abc  123  ... aa', 6, False)); // |123
  AssertEquals('', 9, WBrker.NextWordEnd('abc  123  ... aa', 7, False)); // 1|23
  AssertEquals('', 9, WBrker.NextWordEnd('abc  123  ... aa', 8, False));
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 9, False)); // 123|
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 10, False));
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 11, False)); // |...
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 12, False));
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 13, False));
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 14, False)); // ...|
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 15, False));
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 16, False));
  AssertEquals('',-1, WBrker.NextWordEnd('abc  123  ... aa', 17, False)); // at eol
  AssertEquals('',-1, WBrker.NextWordEnd('abc  123  ... aa', 18, False));
  AssertEquals('',-1, WBrker.NextWordEnd('abc  123  ... aa', 19, False));
  AssertEquals('',-1, WBrker.NextWordEnd('abc  123  ... aa', 0, False));
  AssertEquals('',-1, WBrker.NextWordEnd('abc  123  ... aa', -1, False));
  AssertEquals('',-1, WBrker.NextWordEnd('', 1, False));
  AssertEquals('',-1, WBrker.NextWordEnd(' ', 1, False));
  AssertEquals('',-1, WBrker.NextWordEnd(' ', 2, False));
  AssertEquals('',5,  WBrker.NextWordEnd('..aa', 1, False));
  AssertEquals('',5,  WBrker.NextWordEnd('..aa', 2, False));
  AssertEquals('',5,  WBrker.NextWordEnd('..aa', 3, False));
  AssertEquals('',5,  WBrker.NextWordEnd('..aa', 4, False));
  AssertEquals('',-1, WBrker.NextWordEnd('..aa', 5, False));
  AssertEquals('', 2, WBrker.NextWordEnd('a a 1', 1, False));
  AssertEquals('', 4, WBrker.NextWordEnd('a a 1', 2, False));
  AssertEquals('', 4, WBrker.NextWordEnd('a a 1', 3, False));
  AssertEquals('', 6, WBrker.NextWordEnd('a a 1', 4, False));
  AssertEquals('', 6, WBrker.NextWordEnd('a a 1', 5, False));
  AssertEquals('',-1, WBrker.NextWordEnd('a a 1', 6, False));
  AssertEquals('', 3, WBrker.NextWordEnd(' a ', 1, False));
  AssertEquals('', 3, WBrker.NextWordEnd(' a ', 2, False));
  AssertEquals('',-1, WBrker.NextWordEnd(' a ', 3, False));
  AssertEquals('',-1, WBrker.NextWordEnd(' a ', 4, False));

  AssertEquals('', 4, WBrker.NextWordEnd('abc  123  ... aa', 1, True));
  AssertEquals('', 4, WBrker.NextWordEnd('abc  123  ... aa', 2, True));
  AssertEquals('', 4, WBrker.NextWordEnd('abc  123  ... aa', 3, True));
  AssertEquals('', 4, WBrker.NextWordEnd('abc  123  ... aa', 4, True)); // abc|
  AssertEquals('', 9, WBrker.NextWordEnd('abc  123  ... aa', 5, True));
  AssertEquals('', 9, WBrker.NextWordEnd('abc  123  ... aa', 6, True)); // |123
  AssertEquals('', 9, WBrker.NextWordEnd('abc  123  ... aa', 7, True)); // 1|23
  AssertEquals('', 9, WBrker.NextWordEnd('abc  123  ... aa', 8, True));
  AssertEquals('', 9, WBrker.NextWordEnd('abc  123  ... aa', 9, True)); // 123|
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 10, True));
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 11, True)); // |...
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 12, True));
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 13, True));
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 14, True)); // ...|
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 15, True));
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 16, True));
  AssertEquals('',17, WBrker.NextWordEnd('abc  123  ... aa', 17, True)); // at eol
  AssertEquals('',-1, WBrker.NextWordEnd('abc  123  ... aa', 18, True));
  AssertEquals('',-1, WBrker.NextWordEnd('abc  123  ... aa', 19, True));
  AssertEquals('',-1, WBrker.NextWordEnd('abc  123  ... aa', 0, True));
  AssertEquals('',-1, WBrker.NextWordEnd('abc  123  ... aa', -1, True));
  AssertEquals('',-1, WBrker.NextWordEnd('', 1, True));
  AssertEquals('',-1, WBrker.NextWordEnd(' ', 1, True));
  AssertEquals('',-1, WBrker.NextWordEnd(' ', 2, True));
  AssertEquals('',5,  WBrker.NextWordEnd('..aa', 1, True));
  AssertEquals('',5,  WBrker.NextWordEnd('..aa', 2, True));
  AssertEquals('',5,  WBrker.NextWordEnd('..aa', 3, True));
  AssertEquals('',5,  WBrker.NextWordEnd('..aa', 4, True));
  AssertEquals('',5, WBrker.NextWordEnd('..aa', 5, True));
  AssertEquals('', 2, WBrker.NextWordEnd('a a 1', 1, True));
  AssertEquals('', 2, WBrker.NextWordEnd('a a 1', 2, True));
  AssertEquals('', 4, WBrker.NextWordEnd('a a 1', 3, True));
  AssertEquals('', 4, WBrker.NextWordEnd('a a 1', 4, True));
  AssertEquals('', 6, WBrker.NextWordEnd('a a 1', 5, True));
  AssertEquals('', 6, WBrker.NextWordEnd('a a 1', 6, True));
  AssertEquals('', 3, WBrker.NextWordEnd(' a ', 1, True));
  AssertEquals('', 3, WBrker.NextWordEnd(' a ', 2, True));
  AssertEquals('', 3, WBrker.NextWordEnd(' a ', 3, True));
  AssertEquals('',-1, WBrker.NextWordEnd(' a ', 4, True));

  AssertEquals('',-1, WBrker.PrevWordStart('abc  123  ... aa', 1, False));
  AssertEquals('', 1, WBrker.PrevWordStart('abc  123  ... aa', 2, False));
  AssertEquals('', 1, WBrker.PrevWordStart('abc  123  ... aa', 3, False));
  AssertEquals('', 1, WBrker.PrevWordStart('abc  123  ... aa', 4, False)); // abc|
  AssertEquals('', 1, WBrker.PrevWordStart('abc  123  ... aa', 5, False));
  AssertEquals('', 1, WBrker.PrevWordStart('abc  123  ... aa', 6, False)); // |123
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 7, False)); // 1|23
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 8, False));
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 9, False)); // 123|
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 10, False));
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 11, False)); // |...
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 12, False));
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 13, False));
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 14, False)); // ...|
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 15, False));
  AssertEquals('',15, WBrker.PrevWordStart('abc  123  ... aa', 16, False));
  AssertEquals('',15, WBrker.PrevWordStart('abc  123  ... aa', 17, False)); // at eol
  AssertEquals('',-1, WBrker.PrevWordStart('abc  123  ... aa', 18, False));
  AssertEquals('',-1, WBrker.PrevWordStart('abc  123  ... aa', 19, False));
  AssertEquals('',-1, WBrker.PrevWordStart('abc  123  ... aa', 0, False));
  AssertEquals('',-1, WBrker.PrevWordStart('abc  123  ... aa', -1, False));
  AssertEquals('',-1, WBrker.PrevWordStart('', 1, False));
  AssertEquals('',-1, WBrker.PrevWordStart(' ', 1, False));
  AssertEquals('',-1, WBrker.PrevWordStart(' ', 2, False));
  AssertEquals('',-1, WBrker.PrevWordStart('..aa', 1, False));
  AssertEquals('',-1, WBrker.PrevWordStart('..aa', 2, False));
  AssertEquals('',-1, WBrker.PrevWordStart('..aa', 3, False));
  AssertEquals('', 3, WBrker.PrevWordStart('..aa', 4, False));
  AssertEquals('', 3, WBrker.PrevWordStart('..aa', 5, False));
  AssertEquals('',-1, WBrker.PrevWordStart('a a 1', 1, False));
  AssertEquals('', 1, WBrker.PrevWordStart('a a 1', 2, False));
  AssertEquals('', 1, WBrker.PrevWordStart('a a 1', 3, False));
  AssertEquals('', 3, WBrker.PrevWordStart('a a 1', 4, False));
  AssertEquals('', 3, WBrker.PrevWordStart('a a 1', 5, False));
  AssertEquals('', 5, WBrker.PrevWordStart('a a 1', 6, False));
  AssertEquals('',-1, WBrker.PrevWordStart(' a ', 1, False));
  AssertEquals('',-1, WBrker.PrevWordStart(' a ', 2, False));
  AssertEquals('', 2, WBrker.PrevWordStart(' a ', 3, False));
  AssertEquals('', 2, WBrker.PrevWordStart(' a ', 4, False));

  AssertEquals('', 1, WBrker.PrevWordStart('abc  123  ... aa', 1, True));
  AssertEquals('', 1, WBrker.PrevWordStart('abc  123  ... aa', 2, True));
  AssertEquals('', 1, WBrker.PrevWordStart('abc  123  ... aa', 3, True));
  AssertEquals('', 1, WBrker.PrevWordStart('abc  123  ... aa', 4, True)); // abc|
  AssertEquals('', 1, WBrker.PrevWordStart('abc  123  ... aa', 5, True));
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 6, True)); // |123
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 7, True)); // 1|23
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 8, True));
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 9, True)); // 123|
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 10, True));
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 11, True)); // |...
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 12, True));
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 13, True));
  AssertEquals('', 6, WBrker.PrevWordStart('abc  123  ... aa', 14, True)); // ...|
  AssertEquals('',15, WBrker.PrevWordStart('abc  123  ... aa', 15, True));
  AssertEquals('',15, WBrker.PrevWordStart('abc  123  ... aa', 16, True));
  AssertEquals('',15, WBrker.PrevWordStart('abc  123  ... aa', 17, True)); // at eol
  AssertEquals('',-1, WBrker.PrevWordStart('abc  123  ... aa', 18, True));
  AssertEquals('',-1, WBrker.PrevWordStart('abc  123  ... aa', 19, True));
  AssertEquals('',-1, WBrker.PrevWordStart('abc  123  ... aa', 0, True));
  AssertEquals('',-1, WBrker.PrevWordStart('abc  123  ... aa', -1, True));
  AssertEquals('',-1, WBrker.PrevWordStart('', 1, True));
  AssertEquals('',-1, WBrker.PrevWordStart(' ', 1, True));
  AssertEquals('',-1, WBrker.PrevWordStart(' ', 2, True));
  AssertEquals('',-1, WBrker.PrevWordStart('..aa', 1, True));
  AssertEquals('',-1, WBrker.PrevWordStart('..aa', 2, True));
  AssertEquals('', 3, WBrker.PrevWordStart('..aa', 3, True));
  AssertEquals('', 3, WBrker.PrevWordStart('..aa', 4, True));
  AssertEquals('', 3, WBrker.PrevWordStart('..aa', 5, True));
  AssertEquals('', 1, WBrker.PrevWordStart('a a 1', 1, True));
  AssertEquals('', 1, WBrker.PrevWordStart('a a 1', 2, True));
  AssertEquals('', 3, WBrker.PrevWordStart('a a 1', 3, True));
  AssertEquals('', 3, WBrker.PrevWordStart('a a 1', 4, True));
  AssertEquals('', 5, WBrker.PrevWordStart('a a 1', 5, True));
  AssertEquals('', 5, WBrker.PrevWordStart('a a 1', 6, True));
  AssertEquals('',-1, WBrker.PrevWordStart(' a ', 1, True));
  AssertEquals('', 2, WBrker.PrevWordStart(' a ', 2, True));
  AssertEquals('', 2, WBrker.PrevWordStart(' a ', 3, True));
  AssertEquals('', 2, WBrker.PrevWordStart(' a ', 4, True));

  AssertEquals('', -1, WBrker.PrevWordEnd('abc  123  ... aa', 1, False));
  AssertEquals('', -1, WBrker.PrevWordEnd('abc  123  ... aa', 2, False));
  AssertEquals('', -1, WBrker.PrevWordEnd('abc  123  ... aa', 3, False));
  AssertEquals('', -1, WBrker.PrevWordEnd('abc  123  ... aa', 4, False)); // abc|
  AssertEquals('', 4, WBrker.PrevWordEnd('abc  123  ... aa', 5, False));
  AssertEquals('',4, WBrker.PrevWordEnd('abc  123  ... aa', 6, False)); // |123
  AssertEquals('',4, WBrker.PrevWordEnd('abc  123  ... aa', 7, False)); // 1|23
  AssertEquals('',4, WBrker.PrevWordEnd('abc  123  ... aa', 8, False));
  AssertEquals('',4, WBrker.PrevWordEnd('abc  123  ... aa', 9, False)); // 123|
  AssertEquals('',9, WBrker.PrevWordEnd('abc  123  ... aa', 10, False));
  AssertEquals('',9, WBrker.PrevWordEnd('abc  123  ... aa', 11, False)); // |...
  AssertEquals('',9, WBrker.PrevWordEnd('abc  123  ... aa', 12, False));
  AssertEquals('',9, WBrker.PrevWordEnd('abc  123  ... aa', 13, False));
  AssertEquals('',9, WBrker.PrevWordEnd('abc  123  ... aa', 14, False)); // ...|
  AssertEquals('',9, WBrker.PrevWordEnd('abc  123  ... aa', 15, False));
  AssertEquals('',9, WBrker.PrevWordEnd('abc  123  ... aa', 16, False));
  AssertEquals('',9, WBrker.PrevWordEnd('abc  123  ... aa', 17, False)); // at eol
  AssertEquals('',-1, WBrker.PrevWordEnd('abc  123  ... aa', 18, False));
  AssertEquals('',-1, WBrker.PrevWordEnd('abc  123  ... aa', 19, False));
  AssertEquals('',-1, WBrker.PrevWordEnd('abc  123  ... aa', 0, False));
  AssertEquals('',-1, WBrker.PrevWordEnd('abc  123  ... aa', -1, False));
  AssertEquals('',-1, WBrker.PrevWordEnd('', 1, False));
  AssertEquals('',-1, WBrker.PrevWordEnd(' ', 1, False));
  AssertEquals('',-1, WBrker.PrevWordEnd(' ', 2, False));
  AssertEquals('',-1,  WBrker.PrevWordEnd('..aa', 1, False));
  AssertEquals('',-1,  WBrker.PrevWordEnd('..aa', 2, False));
  AssertEquals('',-1, WBrker.PrevWordEnd('..aa', 3, False));
  AssertEquals('',-1, WBrker.PrevWordEnd('..aa', 4, False));
  AssertEquals('',-1, WBrker.PrevWordEnd('..aa', 5, False));
  AssertEquals('',-1, WBrker.PrevWordEnd('a a 1', 1, False));
  AssertEquals('',-1, WBrker.PrevWordEnd('a a 1', 2, False));
  AssertEquals('', 2, WBrker.PrevWordEnd('a a 1', 3, False));
  AssertEquals('', 2, WBrker.PrevWordEnd('a a 1', 4, False));
  AssertEquals('', 4, WBrker.PrevWordEnd('a a 1', 5, False));
  AssertEquals('', 4, WBrker.PrevWordEnd('a a 1', 6, False));
  AssertEquals('',-1, WBrker.PrevWordEnd(' a ', 1, False));
  AssertEquals('',-1, WBrker.PrevWordEnd(' a ', 2, False));
  AssertEquals('',-1, WBrker.PrevWordEnd(' a ', 3, False));
  AssertEquals('', 3, WBrker.PrevWordEnd(' a ', 4, False));

  AssertEquals('', -1, WBrker.PrevWordEnd('abc  123  ... aa', 1, True));
  AssertEquals('', -1, WBrker.PrevWordEnd('abc  123  ... aa', 2, True));
  AssertEquals('', -1, WBrker.PrevWordEnd('abc  123  ... aa', 3, True));
  AssertEquals('', 4, WBrker.PrevWordEnd('abc  123  ... aa', 4, True)); // abc|
  AssertEquals('', 4, WBrker.PrevWordEnd('abc  123  ... aa', 5, True));
  AssertEquals('', 4, WBrker.PrevWordEnd('abc  123  ... aa', 6, True)); // |123
  AssertEquals('', 4, WBrker.PrevWordEnd('abc  123  ... aa', 7, True)); // 1|23
  AssertEquals('', 4, WBrker.PrevWordEnd('abc  123  ... aa', 8, True));
  AssertEquals('', 9, WBrker.PrevWordEnd('abc  123  ... aa', 9, True)); // 123|
  AssertEquals('', 9, WBrker.PrevWordEnd('abc  123  ... aa', 10, True));
  AssertEquals('', 9, WBrker.PrevWordEnd('abc  123  ... aa', 11, True)); // |...
  AssertEquals('', 9, WBrker.PrevWordEnd('abc  123  ... aa', 12, True));
  AssertEquals('', 9, WBrker.PrevWordEnd('abc  123  ... aa', 13, True));
  AssertEquals('', 9, WBrker.PrevWordEnd('abc  123  ... aa', 14, True)); // ...|
  AssertEquals('', 9, WBrker.PrevWordEnd('abc  123  ... aa', 15, True));
  AssertEquals('', 9, WBrker.PrevWordEnd('abc  123  ... aa', 16, True));
  AssertEquals('',17, WBrker.PrevWordEnd('abc  123  ... aa', 17, True)); // at eol
  AssertEquals('',-1, WBrker.PrevWordEnd('abc  123  ... aa', 18, True));
  AssertEquals('',-1, WBrker.PrevWordEnd('abc  123  ... aa', 19, True));
  AssertEquals('',-1, WBrker.PrevWordEnd('abc  123  ... aa', 0, True));
  AssertEquals('',-1, WBrker.PrevWordEnd('abc  123  ... aa', -1, True));
  AssertEquals('',-1, WBrker.PrevWordEnd('', 1, True));
  AssertEquals('',-1, WBrker.PrevWordEnd(' ', 1, True));
  AssertEquals('',-1, WBrker.PrevWordEnd(' ', 2, True));
  AssertEquals('',-1,  WBrker.PrevWordEnd('..aa', 1, True));
  AssertEquals('',-1,  WBrker.PrevWordEnd('..aa', 2, True));
  AssertEquals('',-1,  WBrker.PrevWordEnd('..aa', 3, True));
  AssertEquals('',-1, WBrker.PrevWordEnd('..aa', 4, True));
  AssertEquals('', 5, WBrker.PrevWordEnd('..aa', 5, True));
  AssertEquals('',-1, WBrker.PrevWordEnd('a a 1', 1, True));
  AssertEquals('', 2, WBrker.PrevWordEnd('a a 1', 2, True));
  AssertEquals('', 2, WBrker.PrevWordEnd('a a 1', 3, True));
  AssertEquals('', 4, WBrker.PrevWordEnd('a a 1', 4, True));
  AssertEquals('', 4, WBrker.PrevWordEnd('a a 1', 5, True));
  AssertEquals('', 6, WBrker.PrevWordEnd('a a 1', 6, True));
  AssertEquals('',-1, WBrker.PrevWordEnd(' a ', 1, True));
  AssertEquals('',-1, WBrker.PrevWordEnd(' a ', 2, True));
  AssertEquals('', 3, WBrker.PrevWordEnd(' a ', 3, True));
  AssertEquals('', 3, WBrker.PrevWordEnd(' a ', 4, True));


  AssertEquals('', 4, WBrker.NextBoundary('abc  123  ... aa', 1, False));
  AssertEquals('', 4, WBrker.NextBoundary('abc  123  ... aa', 2, False));
  AssertEquals('', 4, WBrker.NextBoundary('abc  123  ... aa', 3, False));
  AssertEquals('', 6, WBrker.NextBoundary('abc  123  ... aa', 4, False)); // abc|
  AssertEquals('', 6, WBrker.NextBoundary('abc  123  ... aa', 5, False));
  AssertEquals('', 9, WBrker.NextBoundary('abc  123  ... aa', 6, False)); // |123
  AssertEquals('', 9, WBrker.NextBoundary('abc  123  ... aa', 7, False)); // 1|23
  AssertEquals('', 9, WBrker.NextBoundary('abc  123  ... aa', 8, False));
  AssertEquals('',11, WBrker.NextBoundary('abc  123  ... aa', 9, False)); // 123|
  AssertEquals('',11, WBrker.NextBoundary('abc  123  ... aa', 10, False));
  AssertEquals('',14, WBrker.NextBoundary('abc  123  ... aa', 11, False)); // |...
  AssertEquals('',14, WBrker.NextBoundary('abc  123  ... aa', 12, False));
  AssertEquals('',14, WBrker.NextBoundary('abc  123  ... aa', 13, False));
  AssertEquals('',15, WBrker.NextBoundary('abc  123  ... aa', 14, False)); // ...|
  AssertEquals('',17, WBrker.NextBoundary('abc  123  ... aa', 15, False));
  AssertEquals('',17, WBrker.NextBoundary('abc  123  ... aa', 16, False));
  AssertEquals('',-1, WBrker.NextBoundary('abc  123  ... aa', 17, False)); // at eol
  AssertEquals('',-1, WBrker.NextBoundary('abc  123  ... aa', 18, False));
  AssertEquals('',-1, WBrker.NextBoundary('abc  123  ... aa', 19, False));
  AssertEquals('',-1, WBrker.NextBoundary('abc  123  ... aa', 0, False));
  AssertEquals('',-1, WBrker.NextBoundary('abc  123  ... aa', -1, False));
  AssertEquals('',-1, WBrker.NextBoundary('', 1, False));
  AssertEquals('',-1, WBrker.NextBoundary(' ', 1, False));
  AssertEquals('',-1, WBrker.NextBoundary(' ', 2, False));
  AssertEquals('',3,  WBrker.NextBoundary('..aa', 1, False));
  AssertEquals('',3,  WBrker.NextBoundary('..aa', 2, False));
  AssertEquals('',5, WBrker.NextBoundary('..aa', 3, False));
  AssertEquals('',5, WBrker.NextBoundary('..aa', 4, False));
  AssertEquals('',-1, WBrker.NextBoundary('..aa', 5, False));
  AssertEquals('', 2, WBrker.NextBoundary('a a 1', 1, False));
  AssertEquals('', 3, WBrker.NextBoundary('a a 1', 2, False));
  AssertEquals('', 4, WBrker.NextBoundary('a a 1', 3, False));
  AssertEquals('', 5, WBrker.NextBoundary('a a 1', 4, False));
  AssertEquals('', 6, WBrker.NextBoundary('a a 1', 5, False));
  AssertEquals('',-1, WBrker.NextBoundary('a a 1', 6, False));
  AssertEquals('', 2, WBrker.NextBoundary(' a ', 1, False));
  AssertEquals('', 3, WBrker.NextBoundary(' a ', 2, False));
  AssertEquals('',-1, WBrker.NextBoundary(' a ', 3, False));
  AssertEquals('',-1, WBrker.NextBoundary(' a ', 4, False));

  AssertEquals('', 1, WBrker.NextBoundary('abc  123  ... aa', 1, True));
  AssertEquals('', 4, WBrker.NextBoundary('abc  123  ... aa', 2, True));
  AssertEquals('', 4, WBrker.NextBoundary('abc  123  ... aa', 3, True));
  AssertEquals('', 4, WBrker.NextBoundary('abc  123  ... aa', 4, True)); // abc|
  AssertEquals('', 6, WBrker.NextBoundary('abc  123  ... aa', 5, True));
  AssertEquals('', 6, WBrker.NextBoundary('abc  123  ... aa', 6, True)); // |123
  AssertEquals('', 9, WBrker.NextBoundary('abc  123  ... aa', 7, True)); // 1|23
  AssertEquals('', 9, WBrker.NextBoundary('abc  123  ... aa', 8, True));
  AssertEquals('', 9, WBrker.NextBoundary('abc  123  ... aa', 9, True)); // 123|
  AssertEquals('',11, WBrker.NextBoundary('abc  123  ... aa', 10, True));
  AssertEquals('',11, WBrker.NextBoundary('abc  123  ... aa', 11, True)); // |...
  AssertEquals('',14, WBrker.NextBoundary('abc  123  ... aa', 12, True));
  AssertEquals('',14, WBrker.NextBoundary('abc  123  ... aa', 13, True));
  AssertEquals('',14, WBrker.NextBoundary('abc  123  ... aa', 14, True)); // ...|
  AssertEquals('',15, WBrker.NextBoundary('abc  123  ... aa', 15, True));
  AssertEquals('',17, WBrker.NextBoundary('abc  123  ... aa', 16, True));
  AssertEquals('',17, WBrker.NextBoundary('abc  123  ... aa', 17, True)); // at eol
  AssertEquals('',-1, WBrker.NextBoundary('abc  123  ... aa', 18, True));
  AssertEquals('',-1, WBrker.NextBoundary('abc  123  ... aa', 19, True));
  AssertEquals('',-1, WBrker.NextBoundary('abc  123  ... aa', 0, True));
  AssertEquals('',-1, WBrker.NextBoundary('abc  123  ... aa', -1, True));
  AssertEquals('',-1, WBrker.NextBoundary('', 1, True));
  AssertEquals('',-1, WBrker.NextBoundary(' ', 1, True));
  AssertEquals('',-1, WBrker.NextBoundary(' ', 2, True));
  AssertEquals('',1,  WBrker.NextBoundary('..aa', 1, True));
  AssertEquals('',3,  WBrker.NextBoundary('..aa', 2, True));
  AssertEquals('',3, WBrker.NextBoundary('..aa', 3, True));
  AssertEquals('',5, WBrker.NextBoundary('..aa', 4, True));
  AssertEquals('',5, WBrker.NextBoundary('..aa', 5, True));
  AssertEquals('', 1, WBrker.NextBoundary('a a 1', 1, True));
  AssertEquals('', 2, WBrker.NextBoundary('a a 1', 2, True));
  AssertEquals('', 3, WBrker.NextBoundary('a a 1', 3, True));
  AssertEquals('', 4, WBrker.NextBoundary('a a 1', 4, True));
  AssertEquals('', 5, WBrker.NextBoundary('a a 1', 5, True));
  AssertEquals('', 6, WBrker.NextBoundary('a a 1', 6, True));
  AssertEquals('', 2, WBrker.NextBoundary(' a ', 1, True));
  AssertEquals('', 2, WBrker.NextBoundary(' a ', 2, True));
  AssertEquals('', 3, WBrker.NextBoundary(' a ', 3, True));
  AssertEquals('',-1, WBrker.NextBoundary(' a ', 4, True));

  AssertEquals('',-1, WBrker.PrevBoundary('abc  123  ... aa', 1, False));
  AssertEquals('', 1, WBrker.PrevBoundary('abc  123  ... aa', 2, False));
  AssertEquals('', 1, WBrker.PrevBoundary('abc  123  ... aa', 3, False));
  AssertEquals('', 1, WBrker.PrevBoundary('abc  123  ... aa', 4, False)); // abc|
  AssertEquals('', 4, WBrker.PrevBoundary('abc  123  ... aa', 5, False));
  AssertEquals('', 4, WBrker.PrevBoundary('abc  123  ... aa', 6, False)); // |123
  AssertEquals('', 6, WBrker.PrevBoundary('abc  123  ... aa', 7, False)); // 1|23
  AssertEquals('', 6, WBrker.PrevBoundary('abc  123  ... aa', 8, False));
  AssertEquals('', 6, WBrker.PrevBoundary('abc  123  ... aa', 9, False)); // 123|
  AssertEquals('', 9, WBrker.PrevBoundary('abc  123  ... aa', 10, False));
  AssertEquals('', 9, WBrker.PrevBoundary('abc  123  ... aa', 11, False)); // |...
  AssertEquals('',11, WBrker.PrevBoundary('abc  123  ... aa', 12, False));
  AssertEquals('',11, WBrker.PrevBoundary('abc  123  ... aa', 13, False));
  AssertEquals('',11, WBrker.PrevBoundary('abc  123  ... aa', 14, False)); // ...|
  AssertEquals('',14, WBrker.PrevBoundary('abc  123  ... aa', 15, False));
  AssertEquals('',15, WBrker.PrevBoundary('abc  123  ... aa', 16, False));
  AssertEquals('',15, WBrker.PrevBoundary('abc  123  ... aa', 17, False)); // at eol
  AssertEquals('',-1, WBrker.PrevBoundary('abc  123  ... aa', 18, False));
  AssertEquals('',-1, WBrker.PrevBoundary('abc  123  ... aa', 19, False));
  AssertEquals('',-1, WBrker.PrevBoundary('abc  123  ... aa', 0, False));
  AssertEquals('',-1, WBrker.PrevBoundary('abc  123  ... aa', -1, False));
  AssertEquals('',-1, WBrker.PrevBoundary('', 1, False));
  AssertEquals('',-1, WBrker.PrevBoundary(' ', 1, False));
  AssertEquals('',-1, WBrker.PrevBoundary(' ', 2, False));
  AssertEquals('',-1,  WBrker.PrevBoundary('..aa', 1, False));
  AssertEquals('',1,  WBrker.PrevBoundary('..aa', 2, False));
  AssertEquals('',1, WBrker.PrevBoundary('..aa', 3, False));
  AssertEquals('',3, WBrker.PrevBoundary('..aa', 4, False));
  AssertEquals('',3, WBrker.PrevBoundary('..aa', 5, False));
  AssertEquals('',-1, WBrker.PrevBoundary('a a 1', 1, False));
  AssertEquals('', 1, WBrker.PrevBoundary('a a 1', 2, False));
  AssertEquals('', 2, WBrker.PrevBoundary('a a 1', 3, False));
  AssertEquals('', 3, WBrker.PrevBoundary('a a 1', 4, False));
  AssertEquals('', 4, WBrker.PrevBoundary('a a 1', 5, False));
  AssertEquals('', 5, WBrker.PrevBoundary('a a 1', 6, False));
  AssertEquals('',-1, WBrker.PrevBoundary(' a ', 1, False));
  AssertEquals('',-1, WBrker.PrevBoundary(' a ', 2, False));
  AssertEquals('', 2, WBrker.PrevBoundary(' a ', 3, False));
  AssertEquals('', 3, WBrker.PrevBoundary(' a ', 4, False));

  AssertEquals('', 1, WBrker.PrevBoundary('abc  123  ... aa', 1, True));
  AssertEquals('', 1, WBrker.PrevBoundary('abc  123  ... aa', 2, True));
  AssertEquals('', 1, WBrker.PrevBoundary('abc  123  ... aa', 3, True));
  AssertEquals('', 4, WBrker.PrevBoundary('abc  123  ... aa', 4, True)); // abc|
  AssertEquals('', 4, WBrker.PrevBoundary('abc  123  ... aa', 5, True));
  AssertEquals('', 6, WBrker.PrevBoundary('abc  123  ... aa', 6, True)); // |123
  AssertEquals('', 6, WBrker.PrevBoundary('abc  123  ... aa', 7, True)); // 1|23
  AssertEquals('', 6, WBrker.PrevBoundary('abc  123  ... aa', 8, True));
  AssertEquals('', 9, WBrker.PrevBoundary('abc  123  ... aa', 9, True)); // 123|
  AssertEquals('', 9, WBrker.PrevBoundary('abc  123  ... aa', 10, True));
  AssertEquals('',11, WBrker.PrevBoundary('abc  123  ... aa', 11, True)); // |...
  AssertEquals('',11, WBrker.PrevBoundary('abc  123  ... aa', 12, True));
  AssertEquals('',11, WBrker.PrevBoundary('abc  123  ... aa', 13, True));
  AssertEquals('',14, WBrker.PrevBoundary('abc  123  ... aa', 14, True)); // ...|
  AssertEquals('',15, WBrker.PrevBoundary('abc  123  ... aa', 15, True));
  AssertEquals('',15, WBrker.PrevBoundary('abc  123  ... aa', 16, True));
  AssertEquals('',17, WBrker.PrevBoundary('abc  123  ... aa', 17, True)); // at eol
  AssertEquals('',-1, WBrker.PrevBoundary('abc  123  ... aa', 18, True));
  AssertEquals('',-1, WBrker.PrevBoundary('abc  123  ... aa', 19, True));
  AssertEquals('',-1, WBrker.PrevBoundary('abc  123  ... aa', 0, True));
  AssertEquals('',-1, WBrker.PrevBoundary('abc  123  ... aa', -1, True));
  AssertEquals('',-1, WBrker.PrevBoundary('', 1, True));
  AssertEquals('',-1, WBrker.PrevBoundary(' ', 1, True));
  AssertEquals('',-1, WBrker.PrevBoundary(' ', 2, True));
  AssertEquals('',1,  WBrker.PrevBoundary('..aa', 1, True));
  AssertEquals('',1,  WBrker.PrevBoundary('..aa', 2, True));
  AssertEquals('',3, WBrker.PrevBoundary('..aa', 3, True));
  AssertEquals('',3, WBrker.PrevBoundary('..aa', 4, True));
  AssertEquals('',5, WBrker.PrevBoundary('..aa', 5, True));
  AssertEquals('', 1, WBrker.PrevBoundary('a a 1', 1, True));
  AssertEquals('', 2, WBrker.PrevBoundary('a a 1', 2, True));
  AssertEquals('', 3, WBrker.PrevBoundary('a a 1', 3, True));
  AssertEquals('', 4, WBrker.PrevBoundary('a a 1', 4, True));
  AssertEquals('', 5, WBrker.PrevBoundary('a a 1', 5, True));
  AssertEquals('', 6, WBrker.PrevBoundary('a a 1', 6, True));
  AssertEquals('',-1, WBrker.PrevBoundary(' a ', 1, True));
  AssertEquals('', 2, WBrker.PrevBoundary(' a ', 2, True));
  AssertEquals('', 3, WBrker.PrevBoundary(' a ', 3, True));
  AssertEquals('', 3, WBrker.PrevBoundary(' a ', 4, True));


  FreeAndNil(WBrker);
end;

procedure TTestBasicSynEdit.TestSearchReplace;
  function TestText1: TStringArray;
  begin
    SetLength(Result, 9);
    Result[0] := 'aaaa';
    Result[1] := 'xx11';
    Result[2] := 'cccc';
    Result[3] := '1 1 x';
    Result[4] := '9';
    Result[5] := '1 1 x';
    Result[6] := '9';
    Result[7] := '';
    Result[8] := '';
  end;

  type
    TCaretBlockCoordinates = record x,y, BBx, BBy, BEx, BEy: Integer end;

  function expC(ExpCaretX, ExpCaretY, ExpBBX, ExpBBY, ExpBEX, ExpBEY : Integer): TCaretBlockCoordinates;
  begin
    with result do begin
      x   := ExpCaretX;      y   := ExpCaretY;
      BBx := ExpBBX;         BBy := ExpBBY;
      BEx := ExpBEX;         BEy := ExpBEY;
    end;
  end;
  function expC(ExpCaretX, ExpCaretY: Integer; ExpBBX : Integer = -1): TCaretBlockCoordinates;
  begin
    Result := ExpC(ExpCaretX, ExpCaretY, ExpBBX, -1, -1, -1);
  end;
  function expC(ExpBBX, ExpBBY, ExpBEX, ExpBEY: Integer): TCaretBlockCoordinates;
  begin
    with result do begin
      x   := -1;          y   := -1;
      BBx := ExpBBX;      BBy := ExpBBY;
      BEx := ExpBEX;      BEy := ExpBEY;
    end;
  end;
  function expCNo(ExpCaretX, ExpCaretY: Integer): TCaretBlockCoordinates; // no solection
  begin
    Result := expC(ExpCaretX, ExpCaretY, -2);
  end;
  function expCFw(ExpBBX, ExpBBY, ExpBEX, ExpBEY: Integer): TCaretBlockCoordinates;
  begin // Forward selection
    with result do begin
      x   := ExpBEX;      y   := ExpBEY;
      BBx := ExpBBX;      BBy := ExpBBY;
      BEx := ExpBEX;      BEy := ExpBEY;
    end;
  end;
  function expCBw(ExpBBX, ExpBBY, ExpBEX, ExpBEY: Integer): TCaretBlockCoordinates;
  begin // Backward selection
    with result do begin
      x   := ExpBBX;      y   := ExpBBY;
      BBx := ExpBBX;      BBy := ExpBBY;
      BEx := ExpBEX;      BEy := ExpBEY;
    end;
  end;

  function nextSel(ExpBBX, ExpBBY, ExpBEX, ExpBEY: Integer): TCaretBlockCoordinates;
  begin
    Result := expC(ExpBBX, ExpBBY, ExpBEX, ExpBEY);
  end;

  var
    TheTestText: TStringArray;
    NextTestSetSelection: TCaretBlockCoordinates;

  procedure TestCoord(Name: String; ExpCoord: TCaretBlockCoordinates);
  begin
    if ExpCoord.x > 0 then
      TestIsCaret(Name+' Caret ', ExpCoord.x, ExpCoord.y);
    if ExpCoord.BBx > 0 then
      TestIsSelection(Name+' Selection ', ExpCoord.BBx, ExpCoord.BBy, ExpCoord.BEx, ExpCoord.BEy);
    if ExpCoord.BBx = -2 then
      TestIsSelection(Name+' NO Selection ', SynEdit.LogicalCaretXY.x, SynEdit.CaretY, SynEdit.LogicalCaretXY.x, SynEdit.CaretY);
  end;

  procedure TestSearch(Name, Find, Repl: String; SrcOpts: TSynSearchOptions;
    CaretX, CaretY: Integer;
    ExpCnt: Integer; ExpTxt: Array of const;
    ExpSearchCoord, ExpReplCoord: TCaretBlockCoordinates
  );
  var
    got: Integer;
  begin
    ReCreateEdit;
    SynEdit.Options := SynEdit.Options + [eoScrollPastEol];
    Name := ' - ' + Name;

    PushBaseName('Search');
      SetLines(TheTestText);
      if NextTestSetSelection.BBx > 0 then
        with NextTestSetSelection do SetCaretAndSel(BBx, BBy, BEx, BEy)
      else
        SetCaret(CaretX, CaretY);
      got := SynEdit.SearchReplace(Find, '', SrcOpts - [ssoReplace, ssoReplaceAll]);
      AssertEquals(BaseTestName + Name + 'Result Count', Min(ExpCnt,1), got);
      TestCoord(Name, ExpSearchCoord);
    PopBaseName;

    PushBaseName('Search ssoRegExprMultiLine');
      SetLines(TheTestText);
      if NextTestSetSelection.BBx > 0 then
        with NextTestSetSelection do SetCaretAndSel(BBx, BBy, BEx, BEy)
      else
        SetCaret(CaretX, CaretY);
      got := SynEdit.SearchReplace(Find, '', SrcOpts - [ssoReplace, ssoReplaceAll]+[ssoRegExprMultiLine]);
      AssertEquals(BaseTestName + Name + 'Result Count', Min(ExpCnt,1), got);
      TestCoord(Name, ExpSearchCoord);
    PopBaseName;

    if (SrcOpts * [ssoReplace, ssoReplaceAll]) = [] then exit;

    PushBaseName('Replace');
      SetLines(TheTestText);
      if NextTestSetSelection.BBx > 0 then
        with NextTestSetSelection do SetCaretAndSel(BBx, BBy, BEx, BEy)
      else
        SetCaret(CaretX, CaretY);
      got := SynEdit.SearchReplace(Find, Repl, SrcOpts);
      AssertEquals(BaseTestName + Name + 'Result Count', ExpCnt, got);
      TestIsText(Name + 'Result Text', TheTestText, ExpTxt);
      TestCoord(Name, ExpReplCoord);
    PopBaseName;

    PushBaseName('Replace ssoRegExprMultiLine');
      SetLines(TheTestText);
      if NextTestSetSelection.BBx > 0 then
        with NextTestSetSelection do SetCaretAndSel(BBx, BBy, BEx, BEy)
      else
        SetCaret(CaretX, CaretY);
      got := SynEdit.SearchReplace(Find, Repl, SrcOpts+[ssoRegExprMultiLine]);
      AssertEquals(BaseTestName + Name + 'Result Count', ExpCnt, got);
      TestIsText(Name + 'Result Text', TheTestText, ExpTxt);
      TestCoord(Name, ExpReplCoord);
    PopBaseName;

    NextTestSetSelection := expCNo(-1,-1);
  end;

  procedure TestSearch(Name, Find, Repl: String; SrcOpts: TSynSearchOptions;
    CaretX, CaretY: Integer; ExpCnt: Integer; ExpTxt: Array of const);
  begin
    TestSearch(Name, Find, Repl, SrcOpts, CaretX, CaretY, ExpCnt, ExpTxt, ExpC(-1,-1), ExpC(-1,-1));
  end;
  procedure TestSearch(Name, Find, Repl: String; Opts: TSynSearchOptions;
    ExpCnt: Integer; ExpTxt: Array of const);
  begin
    if ssoBackwards in Opts
    then TestSearch(Name, Find, Repl, Opts, 1,length(TheTestText)-1, ExpCnt, ExpTxt)
    else TestSearch(Name, Find, Repl, Opts, 1,1, ExpCnt, ExpTxt);
  end;
  procedure TestSearch(Name, Find, Repl: String; Opts: TSynSearchOptions;
    ExpCnt: Integer; ExpTxt: Array of const; ExpSearchCoord, ExpReplCoord: TCaretBlockCoordinates);
  begin
    if ssoBackwards in Opts
    then TestSearch(Name, Find, Repl, Opts, 1,length(TheTestText)-1, ExpCnt, ExpTxt, ExpSearchCoord, ExpReplCoord)
    else TestSearch(Name, Find, Repl, Opts, 1,1, ExpCnt, ExpTxt, ExpSearchCoord, ExpReplCoord);
  end;

const
  LE = LineEnding;
  optAllAll: TSynSearchOptions = [ssoEntireScope, ssoReplace, ssoReplaceAll];
  optAllAllB: TSynSearchOptions = [ssoEntireScope, ssoReplace, ssoReplaceAll, ssoBackwards];
  optSelAll: TSynSearchOptions = [ssoSelectedOnly, ssoReplace, ssoReplaceAll];
  optSelAllB: TSynSearchOptions = [ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoBackwards];
  var f, r: String;
  var txl: Integer;
begin
  FPersistSel := False;
  TheTestText := TestText1;
  txl := length(TheTestText)-1;
  NextTestSetSelection := nextSel(0,0, 0,0);

  PushBaseName('Find single line term ');
    PushBaseName('no match ');
      f := '11xx';
      r := '2222';
      TestSearch('',         f, r, optAllAll,  0, [], expCNo(1,1),   expCNo(1,1) );
      TestSearch('backward', f, r, optAllAllB, 0, [], expCNo(1,txl), expCNo(1,txl) );

    PopPushBaseName('match - full line');
      f := 'xx11';
      r := '2222';
      TestSearch('',         f, r, optAllAll,  1, [2, '2222'], expCFw(1,2, 5,2), expCFw(1,2, 5,2) );
      TestSearch('backward', f, r, optAllAllB, 1, [2, '2222'], expCBw(1,2, 5,2), expCBw(1,2, 5,2) );

      r := '';
      TestSearch('repl-empty ',         f, r, optAllAll,  1, [2, ''], expCFw(1,2, 5,2), expCNo(1,2) );
      TestSearch('repl-empty backward', f, r, optAllAllB, 1, [2, ''], expCBw(1,2, 5,2), expCNo(1,2) );

      r := LE;
      TestSearch('repl-CR ',         f, r, optAllAll,  1, [2, '', ''], expCFw(1,2, 5,2), expCFw(1,2, 1,3) );
      TestSearch('repl-CR backward', f, r, optAllAllB, 1, [2, '', ''], expCBw(1,2, 5,2), expCBw(1,2, 1,3) );

    PopPushBaseName('match - part line (end)');
      f := 'x11';
      r := '22';
      TestSearch('repl-shorter ',         f, r, optAllAll,  1, [2, 'x22'], expCFw(2,2, 5,2), expCFw(2,2, 4,2) );
      TestSearch('repl-shorter backward', f, r, optAllAllB, 1, [2, 'x22'], expCBw(2,2, 5,2), expCBw(2,2, 4,2) );

      r := '2222';
      TestSearch('repl-longer ',         f, r, optAllAll,  1, [2, 'x2222'], expCFw(2,2, 5,2), expCFw(2,2, 6,2) );
      TestSearch('repl-longer backward', f, r, optAllAllB, 1, [2, 'x2222'], expCBw(2,2, 5,2), expCBw(2,2, 6,2) );

      r := LE;
      TestSearch('repl-CR ',         f, r, optAllAll,  1, [2, 'x', ''], expCFw(2,2, 5,2), expCFw(2,2, 1,3) );
      TestSearch('repl-CR backward', f, r, optAllAllB, 1, [2, 'x', ''], expCBw(2,2, 5,2), expCBw(2,2, 1,3) );

    PopPushBaseName('match in selection');
      f := '1';
      r := 'N';
      NextTestSetSelection := nextSel(1,4, 2,6);
      TestSearch('repl-at-end ',         f, r, optSelAll,  3, [4,'N N x', 6,'N 1 x'], expCFw(1,4, 2,4), expCFw(1,6, 2,6) );
      NextTestSetSelection := nextSel(1,4, 2,6);
      TestSearch('repl-at-end backward', f, r, optSelAllB, 3, [4,'N N x', 6,'N 1 x'], expCBw(1,6, 2,6), expCBw(1,4, 2,4) );

      NextTestSetSelection := nextSel(1,4, 3,6);
      TestSearch('no-repl-after-end ',         f, r, optSelAll,  3, [4,'N N x', 6,'N 1 x'], expCFw(1,4, 2,4), expCFw(1,6, 2,6) );
      NextTestSetSelection := nextSel(1,4, 3,6);
      TestSearch('no-repl-after-end backward', f, r, optSelAllB, 3, [4,'N N x', 6,'N 1 x'], expCBw(1,6, 2,6), expCBw(1,4, 2,4) );

      NextTestSetSelection := nextSel(1,4, 1,6);
      TestSearch('no-repl-after-end(BOL) ',         f, r, optSelAll,  2, [4,'N N x'], expCFw(1,4, 2,4), expCFw(3,4, 4,4) );
      NextTestSetSelection := nextSel(1,4, 1,6);
      TestSearch('no-repl-after-end(BOL) backward', f, r, optSelAllB, 2, [4,'N N x'], expCBw(3,4, 4,4), expCBw(1,4, 2,4) );

    PopPushBaseName('match in selection, repl-SHORTER - to empty');
      r := '';
      NextTestSetSelection := nextSel(1,4, 4,6);
      TestSearch('repl-at-end ',         f, r, optSelAll,  4, [4,'  x', 6,'  x'], expCFw(1,4, 2,4), expCNo(2,6) );
      NextTestSetSelection := nextSel(1,4, 4,6);
      TestSearch('repl-at-end backward', f, r, optSelAllB, 4, [4,'  x', 6,'  x'], expCBw(3,6, 4,6), expCNo(1,4) );

      NextTestSetSelection := nextSel(1,4, 3,6);
      TestSearch('no-repl-after-end ',         f, r, optSelAll,  3, [4,'  x', 6,' 1 x'], expCFw(1,4, 2,4), expCNo(1,6) );
      NextTestSetSelection := nextSel(1,4, 3,6);
      TestSearch('no-repl-after-end backward', f, r, optSelAllB, 3, [4,'  x', 6,' 1 x'], expCBw(1,6, 2,6), expCNo(1,4) );

    PopPushBaseName('match in selection, repl-LONGER');
      r := 'NN';
      NextTestSetSelection := nextSel(1,4, 4,6);
      TestSearch('repl-at-end ',         f, r, optSelAll,  4, [4,'NN NN x', 6,'NN NN x'], expCFw(1,4, 2,4), expCFw(4,6, 6,6) );
      NextTestSetSelection := nextSel(1,4, 4,6);
      TestSearch('repl-at-end backward', f, r, optSelAllB, 4, [4,'NN NN x', 6,'NN NN x'], expCBw(3,6, 4,6), expCBw(1,4, 3,4) );

      NextTestSetSelection := nextSel(1,4, 3,6);
      TestSearch('no-repl-after-end ',         f, r, optSelAll,  3, [4,'NN NN x', 6,'NN 1 x'], expCFw(1,4, 2,4), expCFw(1,6, 3,6) );
      NextTestSetSelection := nextSel(1,4, 3,6);
      TestSearch('no-repl-after-end backward', f, r, optSelAllB, 3, [4,'NN NN x', 6,'NN 1 x'], expCBw(1,6, 2,6), expCBw(1,4, 3,4) );

    PopPushBaseName('match in selection, repl-CR');
      r := LE;
      NextTestSetSelection := nextSel(1,4, 4,6);
      TestSearch('repl-at-end ',         f, r, optSelAll,  4, [4,'',' ',' x', 8,'',' ',' x'], expCFw(1,4, 2,4), expCFw(2,9, 1,10) );
      NextTestSetSelection := nextSel(1,4, 4,6);
      TestSearch('repl-at-end backward', f, r, optSelAllB, 4, [4,'',' ',' x', 8,'',' ',' x'], expCBw(3,6, 4,6), expCBw(1,4, 1,5) );

      NextTestSetSelection := nextSel(1,4, 3,6);
      TestSearch('no-repl-after-end ',         f, r, optSelAll,  3, [4,'',' ',' x', 8,'',' 1 x'], expCFw(1,4, 2,4), expCFw(1,8, 1,9) );
      NextTestSetSelection := nextSel(1,4, 3,6);
      TestSearch('no-repl-after-end backward', f, r, optSelAllB, 3, [4,'',' ',' x', 8,'',' 1 x'], expCBw(1,6, 2,6), expCBw(1,4, 1,5) );

    PopBaseName;


  PopPushBaseName('Find with trailing CR ');
    PushBaseName('no match ');
      f := 'xx'+LE;
      r := '11'+LE;
      TestSearch('',         f, r, optSelAll,  0, [], expCNo(1,1),   expCNo(1,1) );
      TestSearch('backward', f, r, optSelAllB, 0, [], expCNo(1,txl), expCNo(1,txl) );

    PopPushBaseName('match ');
      f := '11'+LE;
      r := 'bb'+LE;
      TestSearch('',         f, r, optSelAll,  1, [2, 'xxbb'], expCFw(3,2, 1,3), expCFw(3,2, 1,3) );
      TestSearch('backward', f, r, optSelAllB, 1, [2, 'xxbb'], expCBw(3,2, 1,3), expCBw(3,2, 1,3) );
    PopBaseName;

  PopPushBaseName('Find with leading CR ');
    PushBaseName('no match ');
      f := LE+'11';
      r := LE+'xx';
      TestSearch('',         f, r, optSelAll,  0, [], expCNo(1,1),   expCNo(1,1) );
      TestSearch('backward', f, r, optSelAllB, 0, [], expCNo(1,txl), expCNo(1,txl) );

    PopPushBaseName('match ');
      f := LE+'xx';
      r := LE+'bb';
      TestSearch('',         f, r, optSelAll,  1, [2, 'bb11'], expCFw(5,1, 3,2), expCFw(5,1, 3,2) );
      TestSearch('backward', f, r, optSelAllB, 1, [2, 'bb11'], expCBw(5,1, 3,2), expCBw(5,1, 3,2) );
    PopBaseName;

  PopBaseName;
end;

procedure TTestBasicSynEdit.TestMoveLineUpDown;
begin
  FDoInit := @DoInit1;
  FPersistSel := False;
  FTestLines := @TestLines2;

  FTestCommand := ecMoveLineUp;
  TestCommand('ecMoveLineUp - no sel',           1,3,       1,2,       [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineUp - no sel',           4,3,       4,2,       [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineUp - 1L part sel',      2,3, 4,3,  2,2, 4,2,  [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineUp - 1L part sel',      2,3, 1,4,  2,2, 1,3,  [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineUp - 1L full sel',      1,3, 1,4,  1,2, 1,3,  [2, '123 456 789',  3, 'ABC DEF GHI']);

  TestCommand('ecMoveLineUp - 2L part sel',      2,3, 3,4,  2,2, 3,3,  [2, '123 456 789',  3, 'mno pqr stu', 4, 'ABC DEF GHI']);
  TestCommand('ecMoveLineUp - 2L part sel',      2,3, 1,5,  2,2, 1,4,  [2, '123 456 789',  3, 'mno pqr stu', 4, 'ABC DEF GHI']);
  TestCommand('ecMoveLineUp - 2L full sel',      1,3, 1,5,  1,2, 1,4,  [2, '123 456 789',  3, 'mno pqr stu', 4, 'ABC DEF GHI']);


  FTestCommand := ecMoveLineDown;
  TestCommand('ecMoveLineDown - no sel',           1,2,       1,3,       [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineDown - no sel',           4,2,       4,3,       [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineDown - 1L part sel',      2,2, 4,2,  2,3, 4,3,  [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineDown - 1L part sel',      2,2, 1,3,  2,3, 1,4,  [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineDown - 1L full sel',      1,2, 1,3,  1,3, 1,4,  [2, '123 456 789',  3, 'ABC DEF GHI']);

  TestCommand('ecMoveLineDown - 2L part sel',      2,2, 3,3,  2,3, 3,4,  [2, 'mno pqr stu', 3, 'ABC DEF GHI', 4, '123 456 789']);
  TestCommand('ecMoveLineDown - 2L part sel',      2,2, 1,4,  2,3, 1,5,  [2, 'mno pqr stu', 3, 'ABC DEF GHI', 4, '123 456 789']);
  TestCommand('ecMoveLineDown - 2L full sel',      1,2, 1,4,  1,3, 1,5,  [2, 'mno pqr stu', 3, 'ABC DEF GHI', 4, '123 456 789']);


  FPersistSel := true;

  FTestCommand := ecMoveLineUp;
  TestCommand('ecMoveLineUp - no sel',           1,3,       1,2,       [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineUp - no sel',           4,3,       4,2,       [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineUp - 1L part sel',      2,3, 4,3,  2,2, 4,2,  [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineUp - 1L part sel',      2,3, 1,4,  2,2, 1,3,  [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineUp - 1L full sel',      1,3, 1,4,  1,2, 1,3,  [2, '123 456 789',  3, 'ABC DEF GHI']);

  TestCommand('ecMoveLineUp - 2L part sel',      2,3, 3,4,  2,2, 3,3,  [2, '123 456 789',  3, 'mno pqr stu', 4, 'ABC DEF GHI']);
  TestCommand('ecMoveLineUp - 2L part sel',      2,3, 1,5,  2,2, 1,4,  [2, '123 456 789',  3, 'mno pqr stu', 4, 'ABC DEF GHI']);
  TestCommand('ecMoveLineUp - 2L full sel',      1,3, 1,5,  1,2, 1,4,  [2, '123 456 789',  3, 'mno pqr stu', 4, 'ABC DEF GHI']);


  FTestCommand := ecMoveLineDown;
  TestCommand('ecMoveLineDown - no sel',           1,2,       1,3,       [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineDown - no sel',           4,2,       4,3,       [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineDown - 1L part sel',      2,2, 4,2,  2,3, 4,3,  [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineDown - 1L part sel',      2,2, 1,3,  2,3, 1,4,  [2, '123 456 789',  3, 'ABC DEF GHI']);
  TestCommand('ecMoveLineDown - 1L full sel',      1,2, 1,3,  1,3, 1,4,  [2, '123 456 789',  3, 'ABC DEF GHI']);

  TestCommand('ecMoveLineDown - 2L part sel',      2,2, 3,3,  2,3, 3,4,  [2, 'mno pqr stu', 3, 'ABC DEF GHI', 4, '123 456 789']);
  TestCommand('ecMoveLineDown - 2L part sel',      2,2, 1,4,  2,3, 1,5,  [2, 'mno pqr stu', 3, 'ABC DEF GHI', 4, '123 456 789']);
  TestCommand('ecMoveLineDown - 2L full sel',      1,2, 1,4,  1,3, 1,5,  [2, 'mno pqr stu', 3, 'ABC DEF GHI', 4, '123 456 789']);

end;



initialization

  RegisterTest(TTestBasicSynEdit); 
end.

