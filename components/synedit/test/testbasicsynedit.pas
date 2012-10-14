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

  {$IFDEF WithSynBiDi }
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
  {$ENDIF}
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

