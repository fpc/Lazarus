unit TestMultiCaret;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestBase, SynEditKeyCmds, SynPluginMultiCaret, SynEdit, Clipbrd,
  testregistry;

type

  TSynPluginMultiCaretTest = class(TSynPluginMultiCaret)
  public
    property Carets;
  end;

  { TTestMultiCaret }

  TTestMultiCaret = class(TTestBase)
  protected
    FMultiCaret: TSynPluginMultiCaretTest;
  public
    procedure ReCreateEdit; reintroduce;
    procedure RunCmdSeq(cmds: Array of TSynEditorCommand; chars: array of String);
  published
    procedure Edit;
    procedure ReplaceColSel;
    procedure TabKey;
    procedure Paste;
  end;

implementation

{ TTestMultiCaret }

procedure TTestMultiCaret.ReCreateEdit;
begin
  inherited;
  FMultiCaret := TSynPluginMultiCaretTest.Create(SynEdit);
  SynEdit.BlockIndent := 2;
  SynEdit.BlockTabIndent := 0;
  SynEdit.TabWidth := 4;
end;

procedure TTestMultiCaret.RunCmdSeq(cmds: array of TSynEditorCommand; chars: array of String);
var
  i, j: Integer;
  a: String;
begin
  j := 0;
  for i := 0 to high(cmds) do begin
    a := '';
    if (cmds[i] = ecChar) and (j <= high(chars)) then begin
      a := chars[j];
      inc(j);
    end;
    SynEdit.CommandProcessor(cmds[i], a, nil);
  end;
end;

procedure TTestMultiCaret.Edit;
  function TestText1: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1';
    Result[1] := '2';
    Result[2] := '3';
    Result[3] := '4';
    Result[4] := '5';
    Result[5] := '6';
    Result[6] := '7';
    Result[7] := '';
  end;
  function TestText1A: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1';
    Result[1] := 'A2';
    Result[2] := 'A3';
    Result[3] := 'A4';
    Result[4] := 'A5';
    Result[5] := 'A6';
    Result[6] := '7';
    Result[7] := '';
  end;
  function TestText1Del: TStringArray;
  begin
    SetLength(Result, 3);
    Result[0] := '123456';
    Result[1] := '7';
    Result[2] := '';
  end;

begin
  ReCreateEdit;
  SetLines(TestText1);
  SetCaret(1,2);

  RunCmdSeq([ecColSelDown, ecColSelDown, ecColSelDown, ecColSelDown], []);
  TestIsFullText('', TestText1);
  TestIsCaret('', 1,6);

  RunCmdSeq([ecChar], ['A']);
  TestIsFullText('', TestText1A);
  TestIsCaret('', 2,6);

  RunCmdSeq([ecDeleteLastChar], []);
  TestIsFullText('', TestText1);
  TestIsCaret('', 1,6);
  // 4 extra carets + main caret
  AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);

  RunCmdSeq([ecDeleteLastChar], []);
  TestIsFullText('', TestText1Del);
  TestIsCaret('', 6,1);
  // 4 extra carets + main caret
  AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);

  RunCmdSeq([ecDeleteLastChar], []);
  TestIsFullText('', TestText1Del, [1, '6']);
  TestIsCaret('', 1,1);
  // NO extra carets
  AssertEquals(BaseTestName+'', 0, FMultiCaret.Carets.Count);



end;

procedure TTestMultiCaret.ReplaceColSel;
  function TestText1: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1aA';
    Result[1] := '2bB';
    Result[2] := '3cC';
    Result[3] := '4dD';
    Result[4] := '5eE';
    Result[5] := '6fF';
    Result[6] := '7gG';
    Result[7] := '';
  end;
  function TestText1X: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1aA';
    Result[1] := '2XB';
    Result[2] := '3XC';
    Result[3] := '4XD';
    Result[4] := '5XE';
    Result[5] := '6XF';
    Result[6] := '7gG';
    Result[7] := '';
  end;
begin
  ReCreateEdit;
  SetLines(TestText1);
  SetCaret(2,2);

  RunCmdSeq([ecColSelDown, ecColSelDown, ecColSelDown, ecColSelDown, ecColSelRight], []);
  TestIsFullText('', TestText1);
  TestIsCaret('', 3,6);

  RunCmdSeq([ecChar], ['X']);
  TestIsFullText('', TestText1X);
  TestIsCaret('', 3,6);
  // 4 extra carets + main caret
  AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);

end;

procedure TTestMultiCaret.TabKey;
  function TestText1: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1a';
    Result[1] := '2b';
    Result[2] := '3c';
    Result[3] := '4d';
    Result[4] := '5e';
    Result[5] := '6f';
    Result[6] := '7g';
    Result[7] := '';
  end;
  function TestText1Tab: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1a';
    Result[1] := '2'#9'b';
    Result[2] := '3'#9'c';
    Result[3] := '4'#9'd';
    Result[4] := '5'#9'e';
    Result[5] := '6'#9'f';
    Result[6] := '7g';
    Result[7] := '';
  end;
  function TestText1Indent: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1a';
    Result[1] := '  2b';
    Result[2] := '  3c';
    Result[3] := '  4d';
    Result[4] := '  5e';
    Result[5] := '  6f';
    Result[6] := '7g';
    Result[7] := '';
  end;
  function TestText1IndentX: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1a';
    Result[1] := '  2b';
    Result[2] := '  3c';
    Result[3] := '  4d';
    Result[4] := '  5e';
    Result[5] := '  6f';
    Result[6] := '7g';
    Result[7] := '';
  end;
  function TestText1TabOver: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1a';
    Result[1] := '2'#9;
    Result[2] := '3'#9;
    Result[3] := '4'#9;
    Result[4] := '5'#9;
    Result[5] := '6'#9;
    Result[6] := '7g';
    Result[7] := '';
  end;
begin
  PushBaseName('ZERO width selection -- WITH eoTabIndent');
  ReCreateEdit;
  SynEdit.Options := SynEdit.Options + [eoTabIndent] - [eoTabsToSpaces, eoSmartTabs, eoTrimTrailingSpaces];
  SetLines(TestText1);
  SetCaret(2,2);

  RunCmdSeq([ecColSelDown, ecColSelDown, ecColSelDown, ecColSelDown], []);
  TestIsFullText('', TestText1);
  TestIsCaret('', 2,6);

  RunCmdSeq([ecTab], []);
  TestIsFullText('', TestText1Tab);
  TestIsCaret('', 3,6);
  // 4 extra carets + main caret
  AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);


  PopPushBaseName('ONE width selection -- WITH eoTabIndent');
  ReCreateEdit;
  SynEdit.Options := SynEdit.Options + [eoTabIndent] - [eoTabsToSpaces, eoSmartTabs, eoTrimTrailingSpaces];
  SetLines(TestText1);
  SetCaret(2,2);

  RunCmdSeq([ecColSelDown, ecColSelDown, ecColSelDown, ecColSelDown, ecColSelRight], []);
  TestIsFullText('', TestText1);
  TestIsCaret('', 3,6);

  RunCmdSeq([ecTab], []);
  TestIsFullText('', TestText1TabOver);
  TestIsCaret('', 3,6);
  // 4 extra carets + main caret
  AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);



  PopPushBaseName('ZERO width selection -- WITHOUT eoTabIndent');
  ReCreateEdit;
  SynEdit.Options := SynEdit.Options - [eoTabIndent] - [eoTabsToSpaces, eoSmartTabs, eoTrimTrailingSpaces];
  SetLines(TestText1);
  SetCaret(2,2);

  RunCmdSeq([ecColSelDown, ecColSelDown, ecColSelDown, ecColSelDown], []);
  TestIsFullText('', TestText1);
  TestIsCaret('', 2,6);

  RunCmdSeq([ecTab], []);
  TestIsFullText('', TestText1Tab);
  TestIsCaret('', 3,6);
  // 4 extra carets + main caret
  AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);


  PopPushBaseName('ONE width selection -- WITHOUT eoTabIndent');
  ReCreateEdit;
  SynEdit.Options := SynEdit.Options - [eoTabIndent] - [eoTabsToSpaces, eoSmartTabs, eoTrimTrailingSpaces];
  SetLines(TestText1);
  SetCaret(2,2);

  RunCmdSeq([ecColSelDown, ecColSelDown, ecColSelDown, ecColSelDown, ecColSelRight], []);
  TestIsFullText('', TestText1);
  TestIsCaret('', 3,6);

  RunCmdSeq([ecTab], []);
  TestIsFullText('', TestText1TabOver);
  TestIsCaret('', 3,6);
  // 4 extra carets + main caret
  AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);

  PopBaseName;
end;

procedure TTestMultiCaret.Paste;
  function TestText1: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1a';
    Result[1] := '2b';
    Result[2] := '3c';
    Result[3] := '4d';
    Result[4] := '5e';
    Result[5] := '6f';
    Result[6] := '7g';
    Result[7] := '';
  end;
  function TestText1PasteNorm: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1a';
    Result[1] := '21ab';
    Result[2] := '31ac';
    Result[3] := '41ad';
    Result[4] := '51ae';
    Result[5] := '61af';
    Result[6] := '7g';
    Result[7] := '';
  end;
  function TestText1PasteNormOver: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1a';
    Result[1] := '21a';
    Result[2] := '31a';
    Result[3] := '41a';
    Result[4] := '51a';
    Result[5] := '61a';
    Result[6] := '7g';
    Result[7] := '';
  end;
  function TestText1PasteCol: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1a';
    Result[1] := '2b';
    Result[2] := '3c';
    Result[3] := '4d';
    Result[4] := '5e';
    Result[5] := '61f';
    Result[6] := '72g';
    Result[7] := '';
  end;
  function TestText1PasteColOver: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1a';
    Result[1] := '21';
    Result[2] := '32';
    Result[3] := '4';
    Result[4] := '5';
    Result[5] := '6';
    Result[6] := '7g';
    Result[7] := '';
  end;
begin
  PushBaseName('ZERO width selection -- paste normal');
  ReCreateEdit;
  SynEdit.Options := SynEdit.Options + [eoTabIndent] - [eoTabsToSpaces, eoSmartTabs, eoTrimTrailingSpaces];
  SetLines(TestText1);

  SetCaret(1,1);
  RunCmdSeq([ecSelRight, ecSelRight, ecCopy], []); // copy

  SetCaret(2,2);
  RunCmdSeq([ecColSelDown, ecColSelDown, ecColSelDown, ecColSelDown], []);
  TestIsFullText('', TestText1);
  TestIsCaret('', 2,6);

  RunCmdSeq([ecPaste], []);
  TestIsFullText('', TestText1PasteNorm);
  TestIsCaret('', 4,6);
  // 4 extra carets + main caret
  AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);


  PopPushBaseName('ONE width selection -- paste normal');
  ReCreateEdit;
  SynEdit.Options := SynEdit.Options + [eoTabIndent] - [eoTabsToSpaces, eoSmartTabs, eoTrimTrailingSpaces];
  SetLines(TestText1);

  SetCaret(1,1);
  RunCmdSeq([ecSelRight, ecSelRight, ecCopy], []); // copy

  SetCaret(2,2);
  RunCmdSeq([ecColSelDown, ecColSelDown, ecColSelDown, ecColSelDown, ecColSelRight], []);
  TestIsFullText('', TestText1);
  TestIsCaret('', 3,6);

  RunCmdSeq([ecPaste], []);
  TestIsFullText('', TestText1PasteNormOver);
  TestIsCaret('', 4,6);
  // 4 extra carets + main caret
  AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);



  PushBaseName('ZERO width selection -- paste column');
  ReCreateEdit;
  SynEdit.Options := SynEdit.Options + [eoTabIndent] - [eoTabsToSpaces, eoSmartTabs, eoTrimTrailingSpaces];
  SetLines(TestText1);

  SetCaret(1,1);
  RunCmdSeq([ecColSelDown, ecColSelRight, ecCopy], []); // copy

  SetCaret(2,2);
  RunCmdSeq([ecColSelDown, ecColSelDown, ecColSelDown, ecColSelDown], []);
  TestIsFullText('', TestText1);
  TestIsCaret('', 2,6);

  RunCmdSeq([ecPaste], []);
  TestIsFullText('', TestText1PasteCol);
  TestIsCaret('', 3,7);
  AssertEquals(BaseTestName+'', 0, FMultiCaret.Carets.Count);


  PopPushBaseName('ONE width selection -- paste column');
  ReCreateEdit;
  SynEdit.Options := SynEdit.Options + [eoTabIndent] - [eoTabsToSpaces, eoSmartTabs, eoTrimTrailingSpaces];
  SetLines(TestText1);

  SetCaret(1,1);
  RunCmdSeq([ecColSelDown, ecColSelRight, ecCopy], []); // copy

  SetCaret(2,2);
  RunCmdSeq([ecColSelDown, ecColSelDown, ecColSelDown, ecColSelDown, ecColSelRight], []);
  TestIsFullText('', TestText1);
  TestIsCaret('', 3,6);

  RunCmdSeq([ecPaste], []);
  TestIsFullText('', TestText1PasteColOver);
  TestIsCaret('', 3,3);
  AssertEquals(BaseTestName+'', 0, FMultiCaret.Carets.Count);

end;

initialization
  RegisterTest(TTestMultiCaret);

end.

