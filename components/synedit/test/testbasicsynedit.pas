unit TestBasicSynEdit;

(* TODO:
   - TestEditEmpty:
     Test with different sets of VirtualViews (with/without trimming (enabled/module present at all)
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, LCLProc, LCLType, Forms, TestBase,
  SynEdit, SynEditTextTrimmer, SynEditKeyCmds;

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
    procedure TestEditPhysicalLogical;
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

procedure TTestBasicSynEdit.TestEditPhysicalLogical;

  procedure TestPhysLog(name: string; y, x, expX: integer);
  var gotX: Integer;
  begin
    name := name + ' y='+inttostr(y)+' x='+inttostr(x);
    gotX := SynEdit.PhysicalToLogicalPos(Point(x, y)).x;
    AssertEquals(name+'  PhysicalToLogicalPos', expX, gotX);
    gotX := SynEdit.PhysicalToLogicalCol(SynEdit.Lines[y-1], y-1, x);
    AssertEquals(name+'  PhysicalToLogicalCol', expX, gotX);
  end;

  procedure TestLogPhys(name: string; y, x, expX: integer);
  var gotX: Integer;
  begin
    name := name + ' y='+inttostr(y)+' x='+inttostr(x);
    gotX := SynEdit.LogicalToPhysicalPos(Point(x, y)).x;
    AssertEquals(name+'  LogicalToPhysicalPos', expX, gotX);
    gotX := SynEdit.LogicalToPhysicalCol(SynEdit.Lines[y-1], y-1, x);
    AssertEquals(name+'  LogicalToPhysicalCol', expX, gotX);
  end;

begin
  ReCreateEdit;
  SynEdit.TabWidth := 6;

  SetLines(['abc', ' ääX', #9'mn', 'abc'#9'de', #9'Xää.']);

  TestLogPhys('simple line (abc)', 1, 1, 1);
  TestLogPhys('simple line (abc)', 1, 2, 2);
  TestLogPhys('simple line (abc)', 1, 4, 4);
  TestLogPhys('simple line (abc)', 1, 5, 5);
  TestLogPhys('simple line (abc)', 1, 6, 6);
  TestLogPhys('line with 2byte-char', 2, 1, 1);
  TestLogPhys('line with 2byte-char', 2, 2, 2);
  TestLogPhys('line with 2byte-char', 2, 4, 3); // after ae
  TestLogPhys('line with 2byte-char', 2, 6, 4);
  TestLogPhys('line with 2byte-char', 2, 7, 5);
  TestLogPhys('line with 2byte-char', 2, 8, 6);
  TestLogPhys('line with 2byte-char', 2, 11, 9);
  TestLogPhys('line with tab (start)', 3, 1, 1);
  TestLogPhys('line with tab (start)', 3, 2, 7);
  TestLogPhys('line with tab (middle)', 4, 3, 3);
  TestLogPhys('line with tab (middle)', 4, 4, 4); // before tab
  TestLogPhys('line with tab (middle)', 4, 5, 7); // after tab
  TestLogPhys('line with tab (middle)', 4, 6, 8);
  TestLogPhys('line with tab (middle)', 4, 9, 11);
  TestLogPhys('line with tab (start) + 2bc', 5, 1, 1);
  TestLogPhys('line with tab (start) + 2bc', 5, 2, 7);
  TestLogPhys('line with tab (start) + 2bc', 5, 3, 8);
  TestLogPhys('line with tab (start) + 2bc', 5, 5, 9);

  TestPhysLog('simple line (abc)', 1, 1, 1);
  TestPhysLog('simple line (abc)', 1, 2, 2);
  TestPhysLog('simple line (abc)', 1, 4, 4);
  TestPhysLog('simple line (abc)', 1, 5, 5);
  TestPhysLog('simple line (abc)', 1, 6, 6);
  TestPhysLog('line with 2byte-char', 2, 1, 1);
  TestPhysLog('line with 2byte-char', 2, 2, 2);
  TestPhysLog('line with 2byte-char', 2, 3, 4);
  TestPhysLog('line with 2byte-char', 2, 4, 6);
  TestPhysLog('line with 2byte-char', 2, 5, 7);
  TestPhysLog('line with 2byte-char', 2, 6, 8);
  TestPhysLog('line with 2byte-char', 2, 7, 9);
  TestPhysLog('line with tab (start)', 3, 1, 1);
  TestPhysLog('line with tab (start)', 3, 2, 1);
  TestPhysLog('line with tab (start)', 3, 5, 1);
  TestPhysLog('line with tab (start)', 3, 6, 1);
  TestPhysLog('line with tab (start)', 3, 7, 2);
  TestPhysLog('line with tab (start)', 3, 8, 3);
  TestPhysLog('line with tab (start)', 3, 9, 4);
  TestPhysLog('line with tab (start)', 3, 11, 6);

end;



initialization

  RegisterTest(TTestBasicSynEdit); 
end.

