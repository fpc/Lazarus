unit TestSynBeautifier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TestBase,
  SynEdit, SynEditTypes, SynEditTextTrimmer, SynEditKeyCmds, SynBeautifier,
  LCLType, LCLProc;

type

  { TTestSynSelection }

  { TTestSynBeautifier }

  TTestSynBeautifier = class(TTestBase)
  protected
    procedure TestRedoUndo(Name: String; Text: Array of String; X, Y: Integer; Data: Array of const);
  published
    (* Test includes:
       - no indent if switched off
       - indent after VK_RETURN
       - indent after CTRL-VK_N   (TODO)
       * handling split indent (return in the middle of the leading space; next line is already part indented)
       * VK_Return on pos=1, keeps indent. line without indent will *not* be indented
       * with/without trim spaces
       * undo/redo/group-undo
       * correct indent from tabs (include split indent)
       * correct indent from several lines above (skip empty lines to find indent) VK_RETURN only
       * indent modes: sbitSpace, sbitCopySpaceTab, sbitPositionCaret
    *)
    procedure DefaultIndent;
    (*
       - no unindent, if selection (none persistent)   (TODO)
    *)
    //procedure DefaultUnIndent;


  end;


implementation

{ TTestSynBeautifier }

procedure TTestSynBeautifier.TestRedoUndo(Name: String; Text: Array of String;
  X, Y: Integer; Data: array of const);

  function data2txt(n: Integer): String;
  begin
    if n < 0 then exit(LinesToText(Text));
    Result := '';
    case data[n].VType of
      vtString:     Result := data[n].VString^;
      vtAnsiString: Result := AnsiString(data[n].VAnsiString);
      vtChar:       Result := data[n].VChar;
    end;
  end;

var
  i: integer;
  st: TShiftState;
begin
  PushBaseName(Name + '(no group-undo)');
  SetLines(Text);
  SetCaret(X, Y);
  SynEdit.Options  := SynEdit.Options  - [eoGroupUndo];
  for i := 0 to length(data) div 4 - 1 do begin
    st := [];
    if data[i*4+0].vinteger = VK_N then st := [ssCtrl];
    DoKeyPress(data[i*4+0].vinteger, st);
    TestIsCaret('Key #'+IntToStr(i), data[i*4+1].vinteger, data[i*4+2].vinteger);
    TestIsFullText('Key #'+IntToStr(i), data2txt(i*4+3));
  end;

  for i := length(data) div 4 - 2 downto -1 do begin
    SynEdit.Undo;
    if i >= 0
    then TestIsCaret('Undo #'+IntToStr(i+1), data[i*4+1].vinteger, data[i*4+2].vinteger)
    else TestIsCaret('Undo #'+IntToStr(i+1), x, y);
    TestIsFullText('Undo #'+IntToStr(i+1), data2txt(i*4+3));
  end;

  for i := 0 to length(data) div 4 - 1 do begin
    SynEdit.Redo;
    TestIsCaret('Redo #'+IntToStr(i), data[i*4+1].vinteger, data[i*4+2].vinteger);
    TestIsFullText('Redo #'+IntToStr(i), data2txt(i*4+3));
  end;

  for i := length(data) div 4 - 2 downto -1 do begin
    SynEdit.Undo;
    if i >= 0
    then TestIsCaret('2nd Undo Key #'+IntToStr(i+1), data[i*4+1].vinteger, data[i*4+2].vinteger)
    else TestIsCaret('2nd Undo Key #'+IntToStr(i+1), x, y);
    TestIsFullText('2nd Undo #'+IntToStr(i+1), data2txt(i*4+3));
  end;

  PopPushBaseName(Name + '(group-undo)');
  SetLines(Text);
  SetCaret(X, Y);
  SynEdit.Options  := SynEdit.Options  + [eoGroupUndo];
  for i := 0 to length(data) div 4 - 1 do begin
    st := [];
    if data[i*4+0].vinteger = VK_N then st := [ssCtrl];
    DoKeyPress(data[i*4+0].vinteger, st);
    TestIsCaret('Key #'+IntToStr(i), data[i*4+1].vinteger, data[i*4+2].vinteger);
    TestIsFullText('Key #'+IntToStr(i), data2txt(i*4+3));
  end;

  SynEdit.Undo;
  TestIsCaret('Undo', x, y);
  TestIsFullText('Undo', Text);

  SynEdit.Redo;
  i := length(data) div 4 - 1;
  TestIsCaret('Redo', data[i*4+1].vinteger, data[i*4+2].vinteger);
  TestIsFullText('Redo', data2txt(i*4+3));

  SynEdit.Undo;
  TestIsCaret('2nd Undo', x, y);
  TestIsFullText('2nd Undo', Text);

  PopBaseName;
end;

procedure TTestSynBeautifier.DefaultIndent;
  function TestText: TStringArray;
  begin
    SetLength(Result, 11);
    Result[0] := '  a;';
    Result[1] := '    b';
    Result[2] := '';
    Result[3] := '   c';
    Result[5] := '';
    Result[6] := '';
    Result[7] := #9+'x';
    Result[8] := #32#9#32#32#9+'x'; // 8 indent ( 5 logical)
    Result[9] := '   c';
    Result[10] := '';
  end;
  function ExpText(rpl: array of const): String;
  begin
    Result := LinesToText(LinesReplace(TestText, rpl))
  end;

  function TestText2: TStringArray;
  begin
    SetLength(Result, 6);
    Result[0] := '  a;';
    Result[1] := 'b'; // not indent
    Result[2] := '  a;';
    Result[3] := '';
    Result[4] := 'b'; // not indent, after empty line
    Result[5] := '';
  end;
  function ExpText2(rpl: array of const): String;
  begin
    Result := LinesToText(LinesReplace(TestText2, rpl))
  end;

begin
  ReCreateEdit;
  SynEdit.TabWidth := 4;
  SynEdit.TrimSpaceType := settMoveCaret;
  {%region ***** Test WITHOUT indent (no trim) *****}
  BaseTestName := 'DefaultIndent (no indent, no trim)';
  SynEdit.Options  := SynEdit.Options  - [eoTrimTrailingSpaces, eoScrollPastEol, eoAutoIndent];

  TestRedoUndo('Return EOL',          TestText, 5,1, [ VK_RETURN, 1, 2, ExpText([ -2, '']) ]  );
  TestRedoUndo('Return empty line',   TestText, 1,3, [ VK_RETURN, 1, 4, ExpText([ -4, '']) ]  );
  TestRedoUndo('Return after space',  TestText, 5,2, [ VK_RETURN, 1, 3, ExpText([  2, '    ', 'b']) ]  ); // NOT trimmed
  TestRedoUndo('Return before space', TestText, 1,2, [ VK_RETURN, 1, 3, ExpText([ -2, '']) ]  );
  TestRedoUndo('Return mid space',    TestText, 4,2, [ VK_RETURN, 1, 3, ExpText([  2, '   ', ' b']) ]  ); // NOT trimmed
  // VK_N will be ctrl-n (special detection in TestRedoUndo
  TestRedoUndo('CTRL-N EOL',          TestText, 5,1, [ VK_N, 5, 1, ExpText([ -2, '']) ]  );
  {%endregion}

  {%region ***** Test WITHOUT indent (trim) *****}
  BaseTestName := 'DefaultIndent (trim, no indent)';
  SynEdit.Options  := SynEdit.Options  - [eoScrollPastEol, eoAutoIndent] + [eoTrimTrailingSpaces];

  TestRedoUndo('Return EOL',          TestText, 5,1, [ VK_RETURN, 1, 2, ExpText([ -2, '']) ]  );
  TestRedoUndo('Return empty line',   TestText, 1,3, [ VK_RETURN, 1, 4, ExpText([ -4, '']) ]  );
  TestRedoUndo('Return after space',  TestText, 5,2, [ VK_RETURN, 1, 3, ExpText([  2, '', 'b']) ]  ); // trimmed
  TestRedoUndo('Return before space', TestText, 1,2, [ VK_RETURN, 1, 3, ExpText([ -2, '']) ]  );
  TestRedoUndo('Return mid space',    TestText, 4,2, [ VK_RETURN, 1, 3, ExpText([  2, '', ' b']) ]  ); // trimmed
  {%endregion}

  {%region ***** Test WITHOUT trim *****}
    {%region sbitSpaces}
  BaseTestName := 'DefaultIndent (no trim)';
  TSynBeautifier(SynEdit.Beautifier).IndentType := sbitSpace;

  SynEdit.Options  := SynEdit.Options  - [eoTrimTrailingSpaces, eoScrollPastEol, eoTabsToSpaces] + [eoAutoIndent];
  TSynBeautifier(SynEdit.Beautifier).IndentType := sbitSpace;



  TestRedoUndo('Return EOL',          TestText, 5,1, [ VK_RETURN, 3, 2, ExpText([ -2, '  ']) ]  );
  TestRedoUndo('Return EOL+EOT',      TestText, 5,10,[ VK_RETURN, 4,11, ExpText([ -11,'   ']) ]  );
  TestRedoUndo('Return empty line',   TestText, 1,3, [ VK_RETURN, 5, 4, ExpText([ -4, '    ']) ]  );
  TestRedoUndo('Return after space',  TestText, 5,2, [ VK_RETURN, 5, 3, ExpText([ -2, '    ']) ]  ); // NOT trimmed
  TestRedoUndo('Return before space', TestText, 1,2, [ VK_RETURN, 1, 3, ExpText([ -2, '']) ]  );
  TestRedoUndo('Return mid space',    TestText, 4,2, [ VK_RETURN, 4, 3, ExpText([ -2, '   ']) ]  ); // NOT trimmed

  TestRedoUndo('Many Return EOL',          TestText, 5,1, [
                                           VK_RETURN, 3, 2, ExpText([ -2, '  ']),
                                           VK_RETURN, 3, 3, ExpText([ -2, '  ', '  ']),
                                           VK_RETURN, 3, 4, ExpText([ -2, '  ', '  ', '  '])
  ]);
  TestRedoUndo('Many Return EOL+EOT',      TestText, 5,10, [
                                           VK_RETURN, 4,11, ExpText([ -11, '   ']),
                                           VK_RETURN, 4,12, ExpText([ -11, '   ', '   ']),
                                           VK_RETURN, 4,13, ExpText([ -11, '   ', '   ', '   '])
  ]);
  TestRedoUndo('Many Return empty line',   TestText, 1,3, [
                                           VK_RETURN, 5, 4, ExpText([ -4, '    ']),
                                           VK_RETURN, 5, 5, ExpText([ -4, '    ', '    ']),
                                           VK_RETURN, 5, 6, ExpText([ -4, '    ', '    ', '    '])
  ]);
  TestRedoUndo('Many Return after space',  TestText, 5,2, [
                                           VK_RETURN, 5, 3, ExpText([ -2, '    ']),  // NOT trimmed
                                           VK_RETURN, 5, 4, ExpText([ -2, '    ', '    ']),  // NOT trimmed
                                           VK_RETURN, 5, 5, ExpText([ -2, '    ', '    ', '    '])  // NOT trimmed
  ]);
  TestRedoUndo('Many Return before space', TestText, 1,2, [
                                           VK_RETURN, 1, 3, ExpText([ -2, '']),
                                           VK_RETURN, 1, 4, ExpText([ -2, '', '']),
                                           VK_RETURN, 1, 5, ExpText([ -2, '', '', ''])
  ]);
  TestRedoUndo('Many Return mid space',    TestText, 4,2, [
                                           VK_RETURN, 4, 3, ExpText([ -2, '   ']),  // NOT trimmed
                                           VK_RETURN, 4, 4, ExpText([ -2, '   ', '   ']),  // NOT trimmed
                                           VK_RETURN, 4, 5, ExpText([ -2, '   ', '   ', '   '])  // NOT trimmed
  ]);

  TestRedoUndo('Return multi empty',             TestText,  1,7, [ VK_RETURN, 4, 8, ExpText([ -8, '   ']) ]  );
  TestRedoUndo('Return at pos1 of unindeted',    TestText2, 1,2, [ VK_RETURN, 1, 3, ExpText2([ -2, '']) ]  );
  TestRedoUndo('Return at pos1 of unindeted 2',  TestText2, 1,5, [ VK_RETURN, 1, 6, ExpText2([ -5, '']) ]  );

  // caret pos are logical
  TestRedoUndo('Return after tab',               TestText,  6,9, [ VK_RETURN, 9,10, ExpText([ 9, #32#9#32#32#9, '        x']) ]  );
  TestRedoUndo('Return before tab',              TestText,  1,9, [ VK_RETURN, 1,10, ExpText([-9, '']) ]  );
  TestRedoUndo('Return middle tab 1',            TestText,  3,9, [ VK_RETURN, 5,10, ExpText([ 9, #32#9, '    '+#32#32#9+'x']) ]  );
  TestRedoUndo('Return middle tab 2',            TestText,  4,9, [ VK_RETURN, 6,10, ExpText([ 9, #32#9#32, '     '+#32#9+'x']) ]  );
  TestRedoUndo('Return tab EOL',                 TestText,  7,9, [ VK_RETURN, 9,10, ExpText([-10, '        ']) ]  );
    {%endregion}

  (* *** *)
    {%region sbitCopySpaceTab}
  PushBaseName('CopySpaceTab');
  TSynBeautifier(SynEdit.Beautifier).IndentType := sbitCopySpaceTab;

  TestRedoUndo('Return after space',  TestText, 5,2, [ VK_RETURN, 5, 3, ExpText([ -2, '    ']) ]  ); // NOT trimmed
  TestRedoUndo('Return before space', TestText, 1,2, [ VK_RETURN, 1, 3, ExpText([ -2, '']) ]  );
  TestRedoUndo('Return mid space',    TestText, 4,2, [ VK_RETURN, 4, 3, ExpText([ -2, '   ']) ]  ); // NOT trimmed

  TestRedoUndo('Return after tab',               TestText,  6,9, [ VK_RETURN, 6,10, ExpText([ 9, #32#9#32#32#9, #32#9#32#32#9+'x']) ]  );
  TestRedoUndo('Return before tab',              TestText,  1,9, [ VK_RETURN, 1,10, ExpText([-9, '']) ]  );
  TestRedoUndo('Return middle tab 1',            TestText,  3,9, [ VK_RETURN, 3,10, ExpText([ 9, #32#9, #32#9#32#32#9+'x']) ]  );
  TestRedoUndo('Return middle tab 2',            TestText,  4,9, [ VK_RETURN, 4,10, ExpText([ 9, #32#9#32, #32#9#32#32#9+'x']) ]  );
  TestRedoUndo('Return tab EOL',                 TestText,  7,9, [ VK_RETURN, 6,10, ExpText([-10, #32#9#32#32#9]) ]  );
  TestRedoUndo('Many Return tab EOL',            TestText,  7,9, [
                                                 VK_RETURN, 6,10, ExpText([-10, #32#9#32#32#9]),
                                                 VK_RETURN, 6,11, ExpText([-10, #32#9#32#32#9, #32#9#32#32#9]),
                                                 VK_RETURN, 6,12, ExpText([-10, #32#9#32#32#9, #32#9#32#32#9, #32#9#32#32#9])
  ]);
    {%endregion}

  (* *** *)
    {%region sbitPosCaret}
  PopPushBaseName('PosCaret');
  TSynBeautifier(SynEdit.Beautifier).IndentType := sbitPositionCaret;

  TestRedoUndo('Return after space',  TestText, 5,2, [ VK_RETURN, 5, 3, ExpText([ -2, '    ']) ]  ); // NOT trimmed
  TestRedoUndo('Return before space', TestText, 1,2, [ VK_RETURN, 1, 3, ExpText([ -2, '']) ]  );
  TestRedoUndo('Return mid space',    TestText, 4,2, [ VK_RETURN, 4, 3, ExpText([ -2, '   ']) ]  ); // NOT trimmed
  TestRedoUndo('Return eol',          TestText, 6,2, [ VK_RETURN, 5, 3, ExpText([ -3, '']) ]  ); // position only
  TestRedoUndo('Many Return eol',     TestText, 6,2, [  // position only
                                      VK_RETURN, 5, 3, ExpText([ -3, '']),
                                      VK_RETURN, 5, 4, ExpText([ -3, '', '']),
                                      VK_RETURN, 5, 5, ExpText([ -3, '', '', ''])
  ]);

  TestRedoUndo('Return after tab',               TestText,  6,9, [ VK_RETURN, 9,10, ExpText([ 9, #32#9#32#32#9, '        '+'x']) ]  );
  TestRedoUndo('Return before tab',              TestText,  1,9, [ VK_RETURN, 1,10, ExpText([-9, '']) ]  );
  TestRedoUndo('Return middle tab 1',            TestText,  3,9, [ VK_RETURN, 5,10, ExpText([ 9, #32#9, '    '+#32#32#9+'x']) ]  );
  TestRedoUndo('Return middle tab 2',            TestText,  4,9, [ VK_RETURN, 6,10, ExpText([ 9, #32#9#32, '     '+#32#9+'x']) ]  );
  TestRedoUndo('Return tab EOL',                 TestText,  7,9, [ VK_RETURN, 9,10, ExpText([-10, '']) ]  ); // pos only
  TestRedoUndo('Many Return tab EOL',            TestText,  7,9, [
                                                 VK_RETURN, 9,10, ExpText([-10, '']), // pos only
                                                 VK_RETURN, 9,11, ExpText([-10, '', '']),
                                                 VK_RETURN, 9,12, ExpText([-10, '', '', ''])
  ]);
    {%endregion}

    {%region CTRL-N}
  PopPushBaseName('SpaceOnly');
  // VK_N will be ctrl-n (special detection in TestRedoUndo
  TSynBeautifier(SynEdit.Beautifier).IndentType := sbitSpace;
  TestRedoUndo('CTRL-N EOL',          TestText, 5,1, [ VK_N, 5, 1, ExpText([ -2, '  ']) ]  );
  TestRedoUndo('Many CTRL-N EOL',     TestText, 5,1, [
                                      VK_N, 5, 1, ExpText([ -2, '  ']),
                                      VK_N, 5, 1, ExpText([ -2, '  ', '  ']),
                                      VK_N, 5, 1, ExpText([ -2, '  ', '  ', '  '])
  ]);

  TestRedoUndo('CTRL-N empty line',   TestText, 1,3, [ VK_N, 1, 3, ExpText([ -4, '    ']) ]  ); // TODO: should it? 
  //TestRedoUndo('CTRL-N pos1 unindeted',TestText2, 1,2, [ VK_N, 1, 2, ExpText2([ -2, '']) ]  ); // TODO: must not

  TestRedoUndo('CTRL-N after space',  TestText, 5,2, [ VK_N, 5, 2, ExpText([ -2, '    ']) ]  ); // NOT trimmed
  //TestRedoUndo('CTRL-N before space', TestText, 1,2, [ VK_N, 1, 2, ExpText([ -2, '']) ]  ); // TODO: indents too much
  TestRedoUndo('CTRL-N mid space',    TestText, 4,2, [ VK_N, 4, 2, ExpText([ -2, '   ']) ]  ); // NOT trimmed


  PopPushBaseName('CopySpacetab');
  TSynBeautifier(SynEdit.Beautifier).IndentType := sbitCopySpaceTab;
  TestRedoUndo('CTRL-N EOL',          TestText, 5,1, [ VK_N, 5, 1, ExpText([ -2, '  ']) ]  );

  PopPushBaseName('PosOnly');
  TSynBeautifier(SynEdit.Beautifier).IndentType := sbitPositionCaret;
  TestRedoUndo('CTRL-N EOL',          TestText, 5,1, [ VK_N, 5, 1, ExpText([ -2, '']) ]  );

    {%endregion}

  {%endregion}

  {%region ***** Test WITH trim *****}
    {%region sbitSpaces}
  BaseTestName := 'DefaultIndent (trim)';
  TSynBeautifier(SynEdit.Beautifier).IndentType := sbitSpace;

  SynEdit.Options  := SynEdit.Options  - [eoScrollPastEol] + [eoTrimTrailingSpaces, eoAutoIndent];
  TSynBeautifier(SynEdit.Beautifier).IndentType := sbitSpace; //, sbitCopySpaceTab, sbitPositionCaret


  TestRedoUndo('Return EOL',          TestText, 5,1, [ VK_RETURN, 3, 2, ExpText([ -2, '  ']) ]  );
  TestRedoUndo('Return EOL+EOT',      TestText, 5,10,[ VK_RETURN, 4,11, ExpText([ -11,'   ']) ]  );
  TestRedoUndo('Return empty line',   TestText, 1,3, [ VK_RETURN, 5, 4, ExpText([ -4, '    ']) ]  );
  TestRedoUndo('Return after space',  TestText, 5,2, [ VK_RETURN, 5, 3, ExpText([ -2, '']) ]  ); // trimmed
  TestRedoUndo('Return before space', TestText, 1,2, [ VK_RETURN, 1, 3, ExpText([ -2, '']) ]  );
  TestRedoUndo('Return mid space',    TestText, 4,2, [ VK_RETURN, 4, 3, ExpText([ -2, '']) ]  ); // trimmed

  TestRedoUndo('Many Return EOL',          TestText, 5,1, [
                                           VK_RETURN, 3, 2, ExpText([ -2, '  ']),
                                           VK_RETURN, 3, 3, ExpText([ -2, '', '  ']), // trimmed
                                           VK_RETURN, 3, 4, ExpText([ -2, '', '', '  '])
  ]);
  TestRedoUndo('Many Return EOL+EOT',      TestText, 5,10, [
                                           VK_RETURN, 4,11, ExpText([ -11, '   ']),
                                           VK_RETURN, 4,12, ExpText([ -11, '', '   ']), // trimmed
                                           VK_RETURN, 4,13, ExpText([ -11, '', '', '   '])
  ]);
  TestRedoUndo('Many Return empty line',   TestText, 1,3, [
                                           VK_RETURN, 5, 4, ExpText([ -4, '    ']),
                                           VK_RETURN, 5, 5, ExpText([ -4, '', '    ']),  // trimmed
                                           VK_RETURN, 5, 6, ExpText([ -4, '', '', '    '])
  ]);
  TestRedoUndo('Many Return after space',  TestText, 5,2, [
                                           VK_RETURN, 5, 3, ExpText([ -2, '']),  // trimmed
                                           VK_RETURN, 5, 4, ExpText([ -2, '', '']),  // trimmed
                                           VK_RETURN, 5, 5, ExpText([ -2, '', '', ''])  // trimmed
  ]);
  TestRedoUndo('Many Return before space', TestText, 1,2, [
                                           VK_RETURN, 1, 3, ExpText([ -2, '']),
                                           VK_RETURN, 1, 4, ExpText([ -2, '', '']),
                                           VK_RETURN, 1, 5, ExpText([ -2, '', '', ''])
  ]);
  TestRedoUndo('Many Return mid space',    TestText, 4,2, [
                                           VK_RETURN, 4, 3, ExpText([ -2, '']),  // trimmed
                                           VK_RETURN, 4, 4, ExpText([ -2, '', '']),  // trimmed
                                           VK_RETURN, 4, 5, ExpText([ -2, '', '', ''])  // trimmed
  ]);

  TestRedoUndo('Return multi empty',             TestText,  1,7, [ VK_RETURN, 4, 8, ExpText([ -8, '   ']) ]  );
  TestRedoUndo('Return at pos1 of unindeted',    TestText2, 1,2, [ VK_RETURN, 1, 3, ExpText2([ -2, '']) ]  );
  TestRedoUndo('Return at pos1 of unindeted 2',  TestText2, 1,5, [ VK_RETURN, 1, 6, ExpText2([ -5, '']) ]  );

  // caret pos are logical
  TestRedoUndo('Return after tab',               TestText,  6,9, [ VK_RETURN, 9,10, ExpText([ 9, '', '        x']) ]  );
  TestRedoUndo('Return before tab',              TestText,  1,9, [ VK_RETURN, 1,10, ExpText([-9, '']) ]  );
  TestRedoUndo('Return middle tab 1',            TestText,  3,9, [ VK_RETURN, 5,10, ExpText([ 9, '', '    '+#32#32#9+'x']) ]  );
  TestRedoUndo('Return middle tab 2',            TestText,  4,9, [ VK_RETURN, 6,10, ExpText([ 9, '', '     '+#32#9+'x']) ]  );
  TestRedoUndo('Return tab EOL',                 TestText,  7,9, [ VK_RETURN, 9,10, ExpText([-10, '        ']) ]  );
    {%endregion}

  (* *** *)
    {%region sbitCopySpaceTab}
  PushBaseName('CopySpaceTab');
  TSynBeautifier(SynEdit.Beautifier).IndentType := sbitCopySpaceTab;

  TestRedoUndo('Return after space',  TestText, 5,2, [ VK_RETURN, 5, 3, ExpText([ -2, '']) ]  ); // trimmed
  TestRedoUndo('Return before space', TestText, 1,2, [ VK_RETURN, 1, 3, ExpText([ -2, '']) ]  );
  TestRedoUndo('Return mid space',    TestText, 4,2, [ VK_RETURN, 4, 3, ExpText([ -2, '']) ]  ); // trimmed

  TestRedoUndo('Return after tab',               TestText,  6,9, [ VK_RETURN, 6,10, ExpText([ 9, '', #32#9#32#32#9+'x']) ]  );
  TestRedoUndo('Return before tab',              TestText,  1,9, [ VK_RETURN, 1,10, ExpText([-9, '']) ]  );
  TestRedoUndo('Return middle tab 1',            TestText,  3,9, [ VK_RETURN, 3,10, ExpText([ 9, '', #32#9#32#32#9+'x']) ]  );
  TestRedoUndo('Return middle tab 2',            TestText,  4,9, [ VK_RETURN, 4,10, ExpText([ 9, '', #32#9#32#32#9+'x']) ]  );
  TestRedoUndo('Return tab EOL',                 TestText,  7,9, [ VK_RETURN, 6,10, ExpText([-10, #32#9#32#32#9]) ]  );
  TestRedoUndo('Many Return tab EOL',            TestText,  7,9, [
                                                 VK_RETURN, 6,10, ExpText([-10, #32#9#32#32#9]),
                                                 VK_RETURN, 6,11, ExpText([-10, '', #32#9#32#32#9]),
                                                 VK_RETURN, 6,12, ExpText([-10, '', '', #32#9#32#32#9])
  ]);
    {%endregion}

  (* *** *)
    {%region sbitPosCaret}
  PopPushBaseName('PosCaret');
  TSynBeautifier(SynEdit.Beautifier).IndentType := sbitPositionCaret;

  TestRedoUndo('Return after space',  TestText, 5,2, [ VK_RETURN, 5, 3, ExpText([ -2, '']) ]  ); // trimmed
  TestRedoUndo('Return before space', TestText, 1,2, [ VK_RETURN, 1, 3, ExpText([ -2, '']) ]  );
  TestRedoUndo('Return mid space',    TestText, 4,2, [ VK_RETURN, 4, 3, ExpText([ -2, '']) ]  ); //trimmed
  TestRedoUndo('Return eol',          TestText, 6,2, [ VK_RETURN, 5, 3, ExpText([ -3, '']) ]  ); // position only
  TestRedoUndo('Many Return eol',     TestText, 6,2, [  // position only
                                      VK_RETURN, 5, 3, ExpText([ -3, '']),
                                      VK_RETURN, 5, 4, ExpText([ -3, '', '']),
                                      VK_RETURN, 5, 5, ExpText([ -3, '', '', ''])
  ]);

  TestRedoUndo('Return after tab',               TestText,  6,9, [ VK_RETURN, 9,10, ExpText([ 9, '', '        '+'x']) ]  );
  TestRedoUndo('Return before tab',              TestText,  1,9, [ VK_RETURN, 1,10, ExpText([-9, '']) ]  );
  TestRedoUndo('Return middle tab 1',            TestText,  3,9, [ VK_RETURN, 5,10, ExpText([ 9, '', '    '+#32#32#9+'x']) ]  );
  TestRedoUndo('Return middle tab 2',            TestText,  4,9, [ VK_RETURN, 6,10, ExpText([ 9, '', '     '+#32#9+'x']) ]  );
  TestRedoUndo('Return tab EOL',                 TestText,  7,9, [ VK_RETURN, 9,10, ExpText([-10, '']) ]  ); // pos only
  TestRedoUndo('Many Return tab EOL',            TestText,  7,9, [
                                                 VK_RETURN, 9,10, ExpText([-10, '']), // pos only
                                                 VK_RETURN, 9,11, ExpText([-10, '', '']),
                                                 VK_RETURN, 9,12, ExpText([-10, '', '', ''])
  ]);
    {%endregion}

  {%endregion}

end;

initialization

  RegisterTest(TTestSynBeautifier);
end.

