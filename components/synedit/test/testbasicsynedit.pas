unit TestBasicSynEdit;

(* TODO:
   - TestEditEmpty:
     Test with different sets of VirtualViews (with/without trimming (enabled/module present at all)
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, LCLProc, LCLType, Forms, TestBase,
  SynEdit, SynEditTextTrimmer, SynEditKeyCmds, LazSynEditText;

type

  { TTestBasicSynEdit }

  TTestBasicSynEdit = class(TTestBase)
  private
    InsertFlag: Boolean;
    TrimType: TSynEditStringTrimmingType;
    TrimEnabled: Boolean;
  protected
    procedure ReCreateEdit; reintroduce;
  published
    procedure TestEditEmpty;
    procedure TestEditTabs;
    procedure TestEditEcChar;
    procedure TestPhysicalLogical;
    procedure TestCaretAutoMove;
    procedure TestCaretDeleteWord_LastWord;
  end;

implementation

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
    expDef, expColDef: Integer;
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

procedure TTestBasicSynEdit.TestCaretDeleteWord_LastWord;
var
  AllowPastEOL: Boolean;

  function TestLines: TStringArray;
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
    Result[15] := ''
  end;

  procedure DoInit;
  begin
    InsertFlag := False;
    TrimEnabled := False;;
    ReCreateEdit;

    if AllowPastEOL
    then SynEdit.Options := SynEdit.Options + [eoScrollPastEol]
    else SynEdit.Options := SynEdit.Options - [eoScrollPastEol];
    SynEdit.TabWidth := 7;
            // 1    6    11 14
    SetLines(TestLines);
  end;

  procedure TestWordLeft(Name:String; X, Y: Integer;
                         ExpX1, ExpY1: Integer; Repl: Array of const);
  begin
    DoInit;
    SetCaretPhys(X,Y);
    SynEdit.CommandProcessor(ecDeleteLastWord, '', nil);
    TestIsCaretPhys(Name + '(1st DeleteLastWord)', ExpX1, ExpY1);
    TestIsFullText(Name +  '(1st DeleteLastWord)', TestLines, Repl);
  end;
  procedure TestWordLeft(Name:String; X, Y: Integer;
                         ExpX1, ExpY1: Integer; Repl: Array of const;
                         ExpX2, ExpY2: Integer; Repl2: Array of const);
  begin
    TestWordLeft(Name, X, Y, ExpX1, ExpY1, Repl);
    SynEdit.CommandProcessor(ecDeleteLastWord, '', nil);
    TestIsCaretPhys(Name + '(2nd DeleteLastWord)', ExpX2, ExpY2);
    TestIsFullText(Name +  '(2nd DeleteLastWord)', TestLines, Repl2);
  end;

  procedure TestWordRight(Name:String; X, Y: Integer;
                         ExpX1, ExpY1: Integer; Repl: Array of const);
  begin
    DoInit;
    SetCaretPhys(X,Y);
    SynEdit.CommandProcessor(ecDeleteWord, '', nil);
    TestIsCaretPhys(Name + '(1st DeleteWord)', ExpX1, ExpY1);
    TestIsFullText(Name +  '(1st DeleteWord)', TestLines, Repl);
  end;
  procedure TestWordRight(Name:String; X, Y: Integer;
                         ExpX1, ExpY1: Integer; Repl: Array of const;
                         ExpX2, ExpY2: Integer; Repl2: Array of const);
  begin
    TestWordRight(Name, X, Y, ExpX1, ExpY1, Repl);
    SynEdit.CommandProcessor(ecDeleteWord, '', nil);
    TestIsCaretPhys(Name + '(2nd DeleteWord)', ExpX2, ExpY2);
    TestIsFullText(Name +  '(2nd DeleteWord)', TestLines, Repl2);
  end;

begin
  AllowPastEOL := True;
  {%region word left}
  TestWordLeft('simple "te|st"',                   16, 1,   14, 1, [1,'Some text to st'],
                                                            11, 1, [1,'Some text st']);
  TestWordLeft('simple EOW "test|"',               18, 1,   14, 1, [1,'Some text to '],
                                                            11, 1, [1,'Some text ']);
  TestWordLeft('simple BOW "|test"',               14, 1,   11, 1, [1,'Some text test'],
                                                             6, 1, [1,'Some test']);
  TestWordLeft('simple > BOT "So|me"',              3, 1,    1, 1, [1,'me text to test'],
                                                             1, 1, [1,'me text to test']);
  TestWordLeft('simple > prev-line "F|oo"',         2, 2,    1, 2, [2,'oo bar abc'],
                                                            18, 1, [1, 1, 'Some text to testoo bar abc']);
  TestWordLeft('simple > prev-line "|Foo"',         1, 2,   18, 1, [1, 1, 'Some text to testFoo bar abc'],
                                                            14, 1, [1, 1, 'Some text to Foo bar abc']);

  TestWordLeft('tab "wi|th"',                      21, 3,   19, 3, [3, #9'Other line th tab'],
                                                            14, 3, [3, #9'Other th tab']);
  TestWordLeft('tab EOW "with|"',                  23, 3,   19, 3, [3, #9'Other line  tab'],
                                                            14, 3, [3, #9'Other  tab']);
  TestWordLeft('tab BOW "|with"',                  19, 3,   14, 3, [3, #9'Other with tab'],
                                                             8, 3, [3, #9'with tab']);
  TestWordLeft('tab > prev-line "O|ther"',          9, 3,    8, 3, [3, #9'ther line with tab'],
                                                            12, 2, [2, 2, 'Foo bar abcther line with tab']);

  TestWordLeft('M-tab "i|n"',                       9, 4,    8, 4, [4, 'tab'#9'n the middle'],
                                                             1, 4, [4, 'n the middle']);
  TestWordLeft('M-tab > prev-line-tab "ta|b"',      3, 4,    1, 4, [4, 'b'#9'in the middle'],
                                                            27, 3, [3, 3, #9'Other line with tabb'#9'in the middle']);

  //TestWordLeft('M-S-tab "i|n"',                    10, 5,    9, 5,
  //                                                           1, 5);
  //TestWordLeft('M-S-EOW tab "in|"',                11, 5,    9, 5,
  //                                                           1, 5);
  //TestWordLeft('M-S-BOW tab "|in"',                 9, 5,    1, 5,
  //                                                          21, 4);
  //TestWordLeft('M-S-tab "#9| in"',                  8, 5,    1, 5);
  //
  TestWordLeft('Umlaut "ää|ä"',                    11, 6,    9, 6, [6, 'umlaute ä in text'],
                                                             1, 6, [6, 'ä in text']);
  TestWordLeft('Umlaut EOW "äää|"',                12, 6,    9, 6, [6, 'umlaute  in text'],
                                                             1, 6, [6, ' in text']);
  TestWordLeft('Umlaut BOW "|äää"',                 9, 6,    1, 6, [6, 'äää in text'],
                                                            33, 5, [5, 5, TestLines[4]+'äää in text']);
  TestWordLeft('Umlaut "i|n"',                     14, 6,   13, 6, [6, 'umlaute äää n text'],
                                                             9, 6, [6, 'umlaute n text']);
  TestWordLeft('After Umlaut > prev line',          1, 7,   20, 6, [6, 6, 'umlaute äää in text'+TestLines[6]],
                                                            16, 6, [6, 6, 'umlaute äää in '+TestLines[6]]);

  //TestWordLeft('untrimmed "un|trimmed"',            6, 8,    4, 8,
  //                                                          12, 7);
  //TestWordLeft('After untrimmed > prev',            1, 9,   35, 8,
  //                                                          28, 8);
  //TestWordLeft('untrimmed tab "t|ab"',              9,10,    8,10,
  //                                                          12, 9);
  //TestWordLeft('After untrimmed tab > prev',        1,11,   29,10,
  //                                                          22,10);
  //
  //TestWordLeft('After empty > prev',                1,13,    1,12);
  //TestWordLeft('After space empty > prev',          1,15,    6,14);
  {%endregion}

  {%region word right}
  // if in middle of word, keep spaces after
  TestWordRight('simple "te|xt"',                    8, 1,    8, 1, [1,'Some te to test'],
                                                              8, 1, [1,'Some teto test']);
  // if at end of word, just del spaces after
  TestWordRight('simple EOW "text|"',               10, 1,   10, 1, [1,'Some textto test'],
                                                             10, 1, [1,'Some text test']);
  // if at start of word, do NOT keep spaces after
  TestWordRight('simple BOW "|text"',                6, 1,    6, 1, [1,'Some to test'],
                                                              6, 1, [1,'Some test']);
  TestWordRight('simple EOT "li|ne"',               10,15,   10,15, [15,'normal li'],
                                                             10,15, [15,'normal li']);
  TestWordRight('simple > EOL, next line "te|st"',  16, 1,   16, 1, [1,'Some text to te'],
                                                             16, 1, [1, 1,'Some text to teFoo bar abc']);

  TestWordRight('tab "li|ne"',                      16, 3,   16, 3, [3, #9'Other li with tab'],
                                                             16, 3, [3, #9'Other liwith tab']);
  TestWordRight('tab EOW "line|"',                  18, 3,   18, 3, [3, #9'Other linewith tab'],
                                                             18, 3, [3, #9'Other line tab']);
  TestWordRight('tab BOW "|line"',                  14, 3,   14, 3, [3, #9'Other with tab'],
                                                             14, 3, [3, #9'Other tab']);
  TestWordRight('tab > EOL, next-line',             25, 3,   25, 3, [3, #9'Other line with t'],
                                                             25, 3, [3, 3, #9'Other line with ttab'#9'in the middle']);

  TestWordRight('M-tab "t|ab"',                      2, 4,    2, 4, [4, 't'#9'in the middle'],
                                                              2, 4, [4, 'tin the middle']);
  TestWordRight('M-tab "tab|"',                      4, 4,    4, 4, [4, 'tabin the middle'],
                                                              4, 4, [4, 'tab the middle']);
  TestWordRight('M-tab > EOL, next-line-tab',       17, 4,   17, 4, [4, 'tab'#9'in the mi'],
                                                             17, 4, [4, 4, 'tab'#9'in the mitab'#9' in the middle with space']);

  //TestWordRight('M-S-tab BOW "t|ab"',                2, 5,    9, 5,
  //                                                           12, 5);
  //TestWordRight('M-S-tab EOW "tab|"',                4, 5,    9, 5,
  //                                                           12, 5);
  //TestWordRight('M-S-tab BOW "|tab"',                1, 5,    9, 5,
  //                                                           12, 5);
  //TestWordRight('M-S-tab "tab#9| "',                 5, 5,    9, 5,
  //                                                           12, 5);
  //
  TestWordRight('Umlaut "ää|ä"',                    11, 6,   11, 6, [6, 'umlaute ää in text'],
                                                             11, 6, [6, 'umlaute ääin text']);
  TestWordRight('Umlaut EOW "äää|"',                12, 6,   12, 6, [6, 'umlaute äääin text'],
                                                             12, 6, [6, 'umlaute äää text']);
  TestWordRight('Umlaut BOW "|äää"',                 9, 6,    9, 6, [6, 'umlaute in text'],
                                                              9, 6, [6, 'umlaute text']);
  TestWordRight('Umlaut "umlaute|"',                 8, 6,    8, 6, [6, 'umlauteäää in text'],
                                                              8, 6, [6, 'umlaute in text']);
  TestWordRight('After Umlaut > EOL, next line',    18, 6,   18, 6, [6, 'umlaute äää in te'],
                                                             18, 6, [6, 6, 'umlaute äää in te'+TestLines[6]]);
  // past eol
  TestWordRight('Umlaut EOW "äää in text |"',                21, 6,   21, 6, [6, 6, 'umlaute äää in text normal line']);

  //TestWordRight('Before untrimmed > next',           12, 7,   4, 8,
  //                                                           14, 8);
  //TestWordRight('untrimmed > EOL, next',             30, 8,  35, 8,
  //                                                            1, 9);
  //TestWordRight('Before untrimmed tab > next',       12, 9,   8,10,
  //                                                           15,10);
  //TestWordRight('untrimmed tab > EOL, next',         24,10,  29,10,
  //                                                            1,11);
  //
  //TestWordRight('Before empty > next',                12,11,  1,12);
  //TestWordRight('Before space empty > next',          12,13,  1,14,
  //                                                            6, 14);
  {%endregion}

end;



initialization

  RegisterTest(TTestBasicSynEdit); 
end.

