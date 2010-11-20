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



initialization

  RegisterTest(TTestBasicSynEdit); 
end.

