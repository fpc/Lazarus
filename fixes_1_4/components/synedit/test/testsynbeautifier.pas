unit TestSynBeautifier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase, TestHighlightPas,
  SynEdit, SynEditTextTrimmer, SynEditKeyCmds, SynBeautifier,
  LCLType, LCLProc;

type

  TCallBackData = record
     LinePos: Integer;
     Indent: Integer;                   // Indent in spaces (Logical = Physical)
     RelativeToLinePos: Integer;        // Intend specifies +/- offset from intend on RTLine
                                            // 0: for absolute indent
     IndentChars: String ;              // use the following string to indent; maybe empty, single char, or string
     IndentCharsFromLinePos: Integer;   // Line for tab/space mix; set to -1 if unknown
  end;

  { TTestSynBeautifier }

  TTestSynBeautifier = class(TTestBaseHighlighterPas)
  protected
    procedure TestRedoUndo(Name: String; Text: Array of String; X, Y: Integer;
                           Data: Array of const;
                           SelX: Integer = -1; SelY: Integer = -1);
  protected
    FGetIndentCallBackResult: Boolean;
    FCallBackData: Array of TCallBackData;
    FGotLogCaret, FGotOldLogCaret: TPoint;
    FGotFirstLinePos, FGotLastLinePos: Integer;
    FGotReason: TSynEditorCommand;
    function GetIndentCallBack( Sender: TObject;                       // the beautifier
                                {%H-}Editor: TObject;                  // the synedit
                                LogCaret, OldLogCaret: TPoint;
                                FirstLinePos, LastLinePos: Integer;
                                Reason: TSynEditorCommand;             // what caused the evnt
                                SetIndentProc: TSynBeautifierSetIndentProc
                              ): boolean;

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
       - no un-indent if switched off
       - un-indent after VK_BACK (if at end of leading whitespace)
       - no un-indent after VK_BACK (if not at end of leading whitespace)
       - act on empty line (only leading whitespace; no chars follow)  (with/without trim space)
       - unindent after empty line (find indent from lines above)
       - handle tabs, in leading whitespace  (TODO)
       - no unindent, if selection (none persistent)
         Todo: decide on none-overwritable block
       - TODO: fix group undo
    *)
    procedure DefaultUnIndent;

    procedure IndentCallBack;

    procedure IndentPasComment;

  end;


implementation

{ TTestSynBeautifier }

var SkipGroupUndo: Boolean;
procedure TTestSynBeautifier.TestRedoUndo(Name: String; Text: array of String; X, Y: Integer;
  Data: array of const; SelX: Integer; SelY: Integer);

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
  if (SelX >= 0) then
    SetCaretAndSel(SelX, SelY, X, Y)
  else
    SetCaret(X, Y);
  SynEdit.Options  := SynEdit.Options  - [eoGroupUndo];
  for i := 0 to length(data) div 4 - 1 do begin
    st := [];
    if data[i*4+0].vinteger in [VK_A..VK_Z] then st := [ssCtrl];
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

  if SkipGroupUndo then begin
    PopBaseName;
    exit;
  end;

  PopPushBaseName(Name + '(group-undo)');
  SetLines(Text);
  if (SelX >= 0) then
    SetCaretAndSel(SelX, SelY, X, Y)
  else
    SetCaret(X, Y);
  SynEdit.Options  := SynEdit.Options  + [eoGroupUndo];
  for i := 0 to length(data) div 4 - 1 do begin
    st := [];
    if data[i*4+0].vinteger in [VK_A..VK_Z] then st := [ssCtrl];
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
  SkipGroupUndo := False;
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
  TestRedoUndo('CTRL-N pos1 unindeted',TestText2, 1,2, [ VK_N, 1, 2, ExpText2([ -2, '']) ]  ); // TODO: must not

  TestRedoUndo('CTRL-N after space',  TestText, 5,2, [ VK_N, 5, 2, ExpText([ -2, '    ']) ]  ); // NOT trimmed
  TestRedoUndo('CTRL-N before space', TestText, 1,2, [ VK_N, 1, 2, ExpText([ -2, '']) ]  ); // TODO: indents too much
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

    {%region CTRL-N}
  PopPushBaseName('SpaceOnly');
  // VK_N will be ctrl-n (special detection in TestRedoUndo
  TSynBeautifier(SynEdit.Beautifier).IndentType := sbitSpace;
  TestRedoUndo('CTRL-N EOL',          TestText, 5,1, [ VK_N, 5, 1, ExpText([ -2, '']) ]  ); // trimmed

  TestRedoUndo('CTRL-N pos1 unindeted',TestText2, 1,2, [ VK_N, 1, 2, ExpText2([ -2, '']) ]  ); // TODO: must not

  TestRedoUndo('CTRL-N after space',  TestText, 5,2, [ VK_N, 5, 2, ExpText([ -2, '    ']) ]  );
  TestRedoUndo('CTRL-N before space', TestText, 1,2, [ VK_N, 1, 2, ExpText([ -2, '']) ]  ); // TODO: indents too much
  TestRedoUndo('CTRL-N mid space',    TestText, 4,2, [ VK_N, 4, 2, ExpText([ -2, '   ']) ]  );


    {%endregion}

  {%endregion}

end;

procedure TTestSynBeautifier.DefaultUnIndent; 
  function TestText: TStringArray;
  begin
    SetLength(Result, 11);
    Result[0] := '  a;';
    Result[1] := '    b';
    Result[2] := '      c';
    Result[3] := '';
    Result[4] := '      d';
    Result[5] := '';
    Result[6] := '  x';
    Result[7] := '';
    Result[8] := '    y';
    Result[9] := '     z';
    Result[10]:= '';
  end;
  function ExpText(rpl: array of const): String;
  begin
    Result := LinesToText(LinesReplace(TestText, rpl))
  end;
  function TestText2: TStringArray;
  begin
    SetLength(Result, 4);
    Result[0] := '      a;';
    Result[1] := '    b';
    Result[2] := '      ';
    Result[3] := '';
  end;
  function ExpText2(rpl: array of const): String;
  begin
    Result := LinesToText(LinesReplace(TestText2, rpl))
  end;
begin
  SkipGroupUndo := False;
  ReCreateEdit;
  SynEdit.TabWidth := 4;
  SynEdit.TrimSpaceType := settMoveCaret;

  BaseTestName := 'UnIndent pff';
  SynEdit.Options  := SynEdit.Options - [eoTrimTrailingSpaces, eoScrollPastEol, eoAutoIndent];
  SynEdit.Options2  := SynEdit.Options2 - [eoPersistentBlock];
  TestRedoUndo('simple',             TestText,  5,2, [ VK_BACK, 4,2, ExpText([ 2, '   b']) ]  );
  TestRedoUndo('not at indent-end',  TestText,  4,2, [ VK_BACK, 3,2, ExpText([ 2, '   b']) ]  );

  BaseTestName := 'UnIndent';
  SynEdit.Options  := SynEdit.Options + [eoAutoIndent] - [eoTrimTrailingSpaces, eoScrollPastEol];

  TestRedoUndo('simple',             TestText,  5,2, [ VK_BACK, 3,2, ExpText([ 2, '  b']) ]  );
  TestRedoUndo('simple twice',       TestText,  5,2, [
                                     VK_BACK, 3,2, ExpText([ 2, '  b']),
                                     VK_BACK, 1,2, ExpText([ 2, 'b'])
  ]);
  TestRedoUndo('not at indent-end',  TestText,  4,2, [ VK_BACK, 3,2, ExpText([ 2, '   b']) ]  );

  TestRedoUndo('2 level',            TestText,  7,3, [
                                     VK_BACK, 5,3, ExpText([ 3, '    c']),
                                     VK_BACK, 3,3, ExpText([ 3, '  c']),
                                     VK_BACK, 1,3, ExpText([ 3, 'c'])
  ]);
  TestRedoUndo('2 lvl, not indent-end',   TestText,  6,3, [ VK_BACK, 5,3, ExpText([ 3, '     c']) ]  );
  TestRedoUndo('2 lvl, not indent-end 2', TestText,  5,3, [ VK_BACK, 4,3, ExpText([ 3, '     c']) ]  );

  TestRedoUndo('below empty line',        TestText,  7,5, [ VK_BACK, 5,5, ExpText([ 5, '    d']) ]  );
  TestRedoUndo('below empty, not I-End',  TestText,  6,5, [ VK_BACK, 5,5, ExpText([ 5, '     d']) ]  );
  TestRedoUndo('below empty, not I-End 2',TestText,  5,5, [ VK_BACK, 4,5, ExpText([ 5, '     d']) ]  );
  TestRedoUndo('below empty line, many',  TestText,  7,5, [
                                          VK_BACK, 5,5, ExpText([ 5, '    d']),
                                          VK_BACK, 3,5, ExpText([ 5, '  d']),
                                          VK_BACK, 1,5, ExpText([ 5, 'd'])
  ]);

  TestRedoUndo('unindent single space',   TestText,  6,10, [ VK_BACK, 5,10, ExpText([ 10, '    z']) ]  );
  TestRedoUndo('empty line in many',      TestText,  6,10, [
                                          VK_BACK, 5,10, ExpText([ 10, '    z']),
                                          VK_BACK, 3,10, ExpText([ 10, '  z']),
                                          VK_BACK, 1,10, ExpText([ 10, 'z'])
  ]);

  SkipGroupUndo := True; // The first VK_BACK is deletet, not  unindent, therefore it's 2 different undo
  SynEdit.Options  := SynEdit.Options - [eoTrimTrailingSpaces];
  TestRedoUndo('only indent, no text',  TestText,  8,3, [
                                     VK_BACK, 7,3, ExpText([ 3, '      ']),
                                     VK_BACK, 5,3, ExpText([ 3, '    ']),
                                     VK_BACK, 3,3, ExpText([ 3, '  ']),
                                     VK_BACK, 1,3, ExpText([ 3, ''])
  ]);

  SynEdit.Options  := SynEdit.Options + [eoTrimTrailingSpaces];
  TestRedoUndo('only indent, no text (trim)',  TestText,  8,3, [
                                     VK_BACK, 7,3, ExpText([ 3, '      ']),
                                     VK_BACK, 5,3, ExpText([ 3, '    ']),
                                     VK_BACK, 3,3, ExpText([ 3, '  ']),
                                     VK_BACK, 1,3, ExpText([ 3, ''])
  ]);
  SynEdit.Options  := SynEdit.Options - [eoTrimTrailingSpaces];
  SkipGroupUndo := False;

  TestRedoUndo('no unindent (selection)', TestText,  5,2, [ VK_BACK, 4,2, ExpText([ 2, '   b']) ],  4,2);
  TestRedoUndo('no unindent (selection)', TestText,  5,2, [ VK_BACK, 1,2, ExpText([ 2, 'b']) ],  1,2);
  SynEdit.Options2  := SynEdit.Options2 + [eoPersistentBlock];
  TestRedoUndo('unindent (persist selection)', TestText,  5,2, [ VK_BACK, 3,2, ExpText([ 2, '  b']) ],  4,2);
  TestRedoUndo('unindent (persist selection)', TestText,  5,2, [ VK_BACK, 3,2, ExpText([ 2, '  b']) ],  1,2);
  SynEdit.Options2  := SynEdit.Options2 - [eoPersistentBlock];


  BaseTestName := 'UnIndent, with 1st line indended most';
  SynEdit.Options  := SynEdit.Options - [eoTrimTrailingSpaces{, eoScrollPastEol}];
  SynEdit.Options2  := SynEdit.Options2 - [eoPersistentBlock];
  TestRedoUndo('extra indent on first', TestText2,  7,3,
               [ VK_BACK, 5,3, ExpText2([ 3, '    ']),
                 VK_BACK, 1,3, ExpText2([ 3, ''])
               ]  );

end;

function TTestSynBeautifier.GetIndentCallBack(Sender: TObject; Editor: TObject;
  LogCaret, OldLogCaret: TPoint; FirstLinePos, LastLinePos: Integer; Reason: TSynEditorCommand;
  SetIndentProc: TSynBeautifierSetIndentProc): boolean;
var
  i: integer;
begin
  FGotLogCaret := LogCaret;
  FGotOldLogCaret := OldLogCaret;
  FGotFirstLinePos := FirstLinePos;
  FGotLastLinePos := LastLinePos;
  FGotReason := Reason;

  for i := 0 to high(FCallBackData) do
    SetIndentProc( FCallBackData[i].LinePos,
                   FCallBackData[i].Indent,
                   FCallBackData[i].RelativeToLinePos,
                   FCallBackData[i].IndentChars,
                   FCallBackData[i].IndentCharsFromLinePos
    );

  Result := FGetIndentCallBackResult;
end;

procedure TTestSynBeautifier.IndentCallBack;
  function TestText: TStringArray;
  begin
    SetLength(Result, 13);
    Result[0] := '  a;';
    Result[1] := #9+'b';
    Result[2] := '      c';
    Result[3] := '';
    Result[4] := #32#9+'  d';
    Result[5] := '';
    Result[6] := '  x';
    Result[7] := '';
    Result[8] := '    y';
    Result[9] := '     z';
    Result[10]:= '  mn';
    Result[11]:= '  op';
    Result[12]:= '';
  end;
  function ExpText(rpl: array of const): String;
  begin
    Result := LinesToText(LinesReplace(TestText, rpl))
  end;

  procedure SetCB(Res: Boolean; Action: array of const);
  var i: integer;
  begin
    FGetIndentCallBackResult := Res;
    FGotLogCaret := Point(-3, -33);
    FGotOldLogCaret := Point(-3, -33);
    FGotFirstLinePos := -99;
    FGotLastLinePos := -99;
    FGotReason := 27997;

    SetLength(FCallBackData, length(Action) div 5);
    for i := 0 to length(Action) div 5 - 1 do begin
      FCallBackData[i].LinePos                := Action[i*5 + 0].VInteger;
      FCallBackData[i].Indent                 := Action[i*5 + 1].VInteger;
      FCallBackData[i].RelativeToLinePos      := Action[i*5 + 2].VInteger;
      case Action[i*5 + 3].VType of
        vtString:     FCallBackData[i].IndentChars := AnsiString(Action[i*5 + 3].VString);
        vtAnsiString: FCallBackData[i].IndentChars := AnsiString(Action[i*5 + 3].VAnsiString);
        vtChar:       FCallBackData[i].IndentChars := Action[i*5 + 3].VChar;
      end;
      FCallBackData[i].IndentCharsFromLinePos := Action[i*5 + 4].VInteger;
    end;
end;

  procedure TestCbArgs(Name: String; OldX, OldY, NewX, NewY, FirstLine, LastLine, Reason: integer);
  begin
    AssertEquals(Name + 'CB-Args: Got Caret.x Before', OldX, FGotOldLogCaret.x);
    AssertEquals(Name + 'CB-Args: Got Caret.y Before', OldY, FGotOldLogCaret.y);
    AssertEquals(Name + 'CB-Args: Got Caret.x After', NewX, FGotLogCaret.x); // x=1 => before auto indent
    AssertEquals(Name + 'CB-Args: Got Caret.y After', NewY, FGotLogCaret.y);
    AssertEquals(Name + 'CB-Args: FirstLine', FirstLine, FGotFirstLinePos);
    AssertEquals(Name + 'CB-Args: LastLine', LastLine, FGotLastLinePos);
    AssertEquals(Name + 'CB-Args: Reason', Reason, FGotReason);
  end;

begin
  ReCreateEdit;
  try
    SynEdit.Beautifier.OnGetDesiredIndent := @GetIndentCallBack;
    SynEdit.TabWidth := 4;
    SynEdit.TrimSpaceType := settMoveCaret;

    BaseTestName := 'Callback (no trim)';
    SynEdit.Options  := SynEdit.Options + [eoAutoIndent] - [eoTrimTrailingSpaces, eoScrollPastEol];
    TSynBeautifier(SynEdit.Beautifier).IndentType := sbitSpace; // sbitCopySpaceTab;


    SetCB(False, []);
    TestRedoUndo('cb result = false',   TestText,  3,2, [ VK_RETURN, 5,3, ExpText([ -3, '    ']) ]  );
    TestCbArgs('cb result = false', 3,2, 1,3, 3,3, ecLineBreak);

    SetCB(True, []);
    TestRedoUndo('cb result = true ',   TestText,  3,2, [ VK_RETURN, 1,3, ExpText([ -3, '']) ]  );
    TestCbArgs('cb result = true', 3,2, 1,3, 3,3, ecLineBreak);

    // LinPos, Indend, RelativeLinePos=0, Char(s), indentFromLine=-1,
    SetCB(True, [2, 2, 0, '', -1,
                 3, 3, 0, '', -1
    ]);
    TestRedoUndo('cb absolute',   TestText,  3,2, [ VK_RETURN, 1,3, ExpText([ 2, '  b', '   ']) ]  ); // caret is NOT adjusted
    TestCbArgs('cb absolute', 3,2, 1,3, 3,3, ecLineBreak);

    SetCB(True, [2, 3, 0, '', -1,
                 3, 3, 0, '',  2,
                 4, 2, 0, '', -1
    ]);
    TestRedoUndo('cb absolute 2', TestText,  3,2, [ VK_RETURN, 1,3, ExpText([ 2, 2, '   b', '   ', '  c']) ]  );
    TestCbArgs('cb absolute 2', 3,2, 1,3, 3,3, ecLineBreak);

    SetCB(True, [2, 7, 0, '',  2,
                 3, 6, 0, '',  2
    ]);
    TestRedoUndo('cb absolute, keep-cur',   TestText,  3,2, [ VK_RETURN, 1,3, ExpText([ 2, #9+'   b', #9+'  ']) ]  );
    TestCbArgs('cb absolute, keep-cur', 3,2, 1,3, 3,3, ecLineBreak);

    SetCB(True, [2, -2, 2, '', -1,
                 3,  3, 2, '', -1
    ]);
    TestRedoUndo('cb relative ',   TestText,  3,2, [ VK_RETURN, 1,3, ExpText([ 2, '  b', '     ']) ]  );
    TestCbArgs('cb relative ', 3,2, 1,3, 3,3, ecLineBreak);

    SetCB(True, [2,  3, 2, '', -1,
                 3, -1, 2, '', -1
    ]);
    TestRedoUndo('cb relative 2',  TestText,  3,2, [ VK_RETURN, 1,3, ExpText([ 2, '       b', '      ']) ]  );
    TestCbArgs('cb relative 2', 3,2, 1,3, 3,3, ecLineBreak);

    SetCB(True, [2,  3, 2, '', -1,
                 3,  1, 3, '', -1
    ]);
    TestRedoUndo('cb relative 3',  TestText,  3,2, [ VK_RETURN, 1,3, ExpText([ 2, '       b', ' ']) ]  );
    TestCbArgs('cb relative 3', 3,2, 1,3, 3,3, ecLineBreak);

    SetCB(True, [2,  3, 2, '',  2,
                 3,  0, 2, '',  2
    ]);
    TestRedoUndo('cb relative, keep',  TestText,  3,2, [ VK_RETURN, 1,3, ExpText([ 2, #9+'   b', #9+'   ']) ]  );
    TestCbArgs('cb relative, keep', 3,2, 1,3, 3,3, ecLineBreak);

    SetCB(True, [1, 9, 0, #9, -1]);
    TestRedoUndo('cb abs; char=tab',   TestText,  5,1, [ VK_RETURN, 1,2, ExpText([ 1, #9#9+' a;', '']) ]  );
    TestCbArgs('cb abs; char=tab', 5,1, 1,2, 2,2, ecLineBreak);

    (* Paste *)
    ClipBoardText := '1';
    SetCB(True, []);
    TestRedoUndo('paste 1 lines, noaction',  TestText,  5,11, [ VK_V, 6,11, ExpText([ 11, '  mn1']) ]  );
    TestCbArgs('paste 1 lines, noaction', 5,11, 6,11, 11,11, ecPaste);

    ClipBoardText := '1'+LineEnding+'2';
    SetCB(True, []);
    TestRedoUndo('paste 2 lines, noaction',  TestText,  5,11, [ VK_V, 2,12, ExpText([ 11, '  mn1', '2']) ]  );
    TestCbArgs('paste 2 lines, noaction', 5,11, 2,12, 11,12, ecPaste);

    ClipBoardText := '1'+LineEnding+'2'+LineEnding+'3';
    SetCB(True, []);
    TestRedoUndo('paste 3 lines, noaction',  TestText,  5,11, [ VK_V, 2,13, ExpText([ 11, '  mn1', '2', '3']) ]  );
    TestCbArgs('paste 3 lines, noaction', 5,11, 2,13, 11,13, ecPaste);

    ClipBoardText := '1'+LineEnding+'2'+LineEnding+'3'+LineEnding+'4';
    SetCB(True, []);
    TestRedoUndo('paste 4 lines, noaction',  TestText,  5,11, [ VK_V, 2,14, ExpText([ 11, '  mn1', '2', '3', '4']) ]  );
    TestCbArgs('paste 4 lines, noaction', 5,11, 2,14, 11,14, ecPaste);

    ClipBoardText := '1'+LineEnding+'2'+LineEnding+'3'+LineEnding+'4'+LineEnding;
    SetCB(True, []);
    TestRedoUndo('paste 4+ lines, noaction',  TestText,  5,11, [ VK_V, 1,15, ExpText([ 11, '  mn1', '2', '3', '4', '']) ]  );
    TestCbArgs('paste 4+ lines, noaction', 5,11, 1,15, 11,15, ecPaste);

    ClipBoardText := '1'+LineEnding+'2'+LineEnding+'3'+LineEnding+'4';
    SetCB(True, []);
    TestRedoUndo('paste 4 lines on empty, noaction',  TestText,  1,8, [ VK_V, 2,11, ExpText([ 8, '1', '2', '3', '4']) ]  );
    TestCbArgs('paste 4 lines on empty, noaction', 1,8, 2,11, 8,11, ecPaste);

    ClipBoardText := '1'+LineEnding+'2'+LineEnding+'3'+LineEnding+'4'+LineEnding +'5';
    SetCB(True, []);
    TestRedoUndo('paste 5 lines, noaction',  TestText,  5,11, [ VK_V, 2,15, ExpText([ 11, '  mn1', '2', '3', '4', '5']) ]  );
    TestCbArgs('paste 5 lines, noaction', 5,11, 2,15, 11,15, ecPaste);

    ClipBoardText := '1'+LineEnding+'2';
    SetCB(True, []);
    TestRedoUndo('paste 2 lines middle, noaction',  TestText,  4,11, [ VK_V, 2,12, ExpText([ 11, '  m1', '2n']) ]  );
    TestCbArgs('paste 2 lines middle, noaction', 4,11, 2,12, 11,12, ecPaste);

    ClipBoardText := '1'+LineEnding+'2'+LineEnding+'3';
    SetCB(True, []);
    TestRedoUndo('paste 3 lines middle, noaction',  TestText,  4,11, [ VK_V, 2,13, ExpText([ 11, '  m1', '2', '3n']) ]  );
    TestCbArgs('paste 3 lines middle, noaction', 4,11, 2,13, 11,13, ecPaste);


    BaseTestName := 'Callback (trim)';
    SynEdit.Options  := SynEdit.Options + [eoTrimTrailingSpaces, eoAutoIndent] - [eoScrollPastEol];

    (* Paste *)
    ClipBoardText := '1';
    SetCB(True, []);
    TestRedoUndo('paste 1 lines, noaction',  TestText,  5,11, [ VK_V, 6,11, ExpText([ 11, '  mn1']) ]  );
    TestCbArgs('paste 1 lines, noaction', 5,11, 6,11, 11,11, ecPaste);

    ClipBoardText := '1'+LineEnding+'2';
    SetCB(True, []);
    TestRedoUndo('paste 2 lines, noaction',  TestText,  5,11, [ VK_V, 2,12, ExpText([ 11, '  mn1', '2']) ]  );
    TestCbArgs('paste 2 lines, noaction', 5,11, 2,12, 11,12, ecPaste);

    ClipBoardText := '1'+LineEnding+'2'+LineEnding+'3';
    SetCB(True, []);
    TestRedoUndo('paste 3 lines, noaction',  TestText,  5,11, [ VK_V, 2,13, ExpText([ 11, '  mn1', '2', '3']) ]  );
    TestCbArgs('paste 3 lines, noaction', 5,11, 2,13, 11,13, ecPaste);

    ClipBoardText := '1'+LineEnding+'2'+LineEnding+'3'+LineEnding+'4';
    SetCB(True, []);
    TestRedoUndo('paste 4 lines, noaction',  TestText,  5,11, [ VK_V, 2,14, ExpText([ 11, '  mn1', '2', '3', '4']) ]  );
    TestCbArgs('paste 4 lines, noaction', 5,11, 2,14, 11,14, ecPaste);

    ClipBoardText := '1'+LineEnding+'2'+LineEnding+'3'+LineEnding+'4'+LineEnding;
    SetCB(True, []);
    TestRedoUndo('paste 4+ lines, noaction',  TestText,  5,11, [ VK_V, 1,15, ExpText([ 11, '  mn1', '2', '3', '4', '']) ]  );
    TestCbArgs('paste 4+ lines, noaction', 5,11, 1,15, 11,15, ecPaste);

    ClipBoardText := '1'+LineEnding+'2'+LineEnding+'3'+LineEnding+'4';
    SetCB(True, []);
    TestRedoUndo('paste 4 lines on empty, noaction',  TestText,  1,8, [ VK_V, 2,11, ExpText([ 8, '1', '2', '3', '4']) ]  );
    TestCbArgs('paste 4 lines on empty, noaction', 1,8, 2,11, 8,11, ecPaste);

    ClipBoardText := '1'+LineEnding+'2'+LineEnding+'3'+LineEnding+'4'+LineEnding +'5';
    SetCB(True, []);
    TestRedoUndo('paste 5 lines, noaction',  TestText,  5,11, [ VK_V, 2,15, ExpText([ 11, '  mn1', '2', '3', '4', '5']) ]  );
    TestCbArgs('paste 5 lines, noaction', 5,11, 2,15, 11,15, ecPaste);

    ClipBoardText := '1'+LineEnding+'2';
    SetCB(True, []);
    TestRedoUndo('paste 2 lines middle, noaction',  TestText,  4,11, [ VK_V, 2,12, ExpText([ 11, '  m1', '2n']) ]  );
    TestCbArgs('paste 2 lines middle, noaction', 4,11, 2,12, 11,12, ecPaste);

    ClipBoardText := '1'+LineEnding+'2'+LineEnding+'3';
    SetCB(True, []);
    TestRedoUndo('paste 3 lines middle, noaction',  TestText,  4,11, [ VK_V, 2,13, ExpText([ 11, '  m1', '2', '3n']) ]  );
    TestCbArgs('paste 3 lines middle, noaction', 4,11, 2,13, 11,13, ecPaste);


  finally
    SynEdit.Beautifier.OnGetDesiredIndent := nil;
  end;
end;

procedure TTestSynBeautifier.IndentPasComment;
var
  Beautifier: TSynBeautifierPascal;
  BaseName, BaseConf: String;
  BaseText: TStringArray;
  BaseStep: Integer;
  CurrentText: TStringArray;
  ParentIndentType: TSynBeautifierIndentType;
  PIName: string;
  LoopMatchFirst, LoopIndentApplyNoMatch, LoopIndentAdds, LoopIndentMax: Integer;
  ExtraIndentFlags: TSynCommentIndentFlags;
  MatchLine: TSynCommentMatchLine;
type
  TTestFlags = set of (tfExpLineDiffUseBase);

  procedure ConfigBeautifier(AType: TSynCommentType;
    AIndentMode: TSynCommentIndentFlags;
    AIndentFirstLineMax:   Integer; AIndentFirstLineExtra: String;
    ACommentMode: TSynCommentContineMode; AMatchMode: TSynCommentMatchMode;
    AMatchLine: TSynCommentMatchLine; ACommentIndent: TSynBeautifierIndentType;
    AMatch: String;  APrefix: String;
    AExtenbSlash: TSynCommentExtendMode = sceNever
  );
  begin
    writestr(BaseConf, AType,':',
             ' IMode=', dbgs(AIndentMode), ' IMax=', AIndentFirstLineMax, ' IExtra=', AIndentFirstLineExtra,
             ' CMode=', ACommentMode, ' CMatch=', AMatchMode, ' CLine=', AMatchLine,
             ' M=''', AMatch, ''' R=''', APrefix, ''' CIndent=', ACommentIndent
            );
    case AType of
      sctAnsi:
        with Beautifier do begin
          AnsiIndentMode           := AIndentMode;
          AnsiIndentFirstLineMax   := AIndentFirstLineMax;
          AnsiIndentFirstLineExtra := AIndentFirstLineExtra;

          AnsiCommentMode          := ACommentMode;
          AnsiMatchMode            := AMatchMode;
          AnsiMatchLine            := AMatchLine;
          AnsiCommentIndent        := ACommentIndent;
          AnsiMatch                := AMatch;
          AnsiPrefix               := APrefix;
        end;
      sctBor:
        with Beautifier do begin
          BorIndentMode           := AIndentMode;
          BorIndentFirstLineMax   := AIndentFirstLineMax;
          BorIndentFirstLineExtra := AIndentFirstLineExtra;

          BorCommentMode          := ACommentMode;
          BorMatchMode            := AMatchMode;
          BorMatchLine            := AMatchLine;
          BorCommentIndent        := ACommentIndent;
          BorMatch                := AMatch;
          BorPrefix               := APrefix;
        end;
      sctSlash:
        with Beautifier do begin
          ExtendSlashCommentMode := AExtenbSlash;

          SlashIndentMode           := AIndentMode;
          SlashIndentFirstLineMax   := AIndentFirstLineMax;
          SlashIndentFirstLineExtra := AIndentFirstLineExtra;

          SlashCommentMode          := ACommentMode;
          SlashMatchMode            := AMatchMode;
          SlashMatchLine            := AMatchLine;
          SlashCommentIndent        := ACommentIndent;
          SlashMatch                := AMatch;
          SlashPrefix               := APrefix;
        end;
    end;
  end;

  function BaseLinesToText: String;
  var
    i, j, k: Integer;
  begin
    i := 0;
    j := 30;
    k := length(BaseText);
    Result := ' ';
    while k > 0 do
      if BaseText[k - 1] = ''
      then dec(k)
      else break;
    while i < k do
      if BaseText[i] = ''
      then inc(i)
      else break;
    Result := Result + ' '+ IntToStr(i)+':';
    while (i < k) and (j>0) do begin
      Result := Result + copy(BaseText[i], 1, j);
      if Length(BaseText[i]) > j then
        Result := Result + '...'
      else
        Result := Result + '#13';
      j := j - Length(BaseText[i]);
      inc(i);
    end;
  end;

  Procedure DoSetText(ABaseName: String; ALines: TStringArray);
  begin
    BaseName := ABaseName;
    BaseStep := 0;
    BaseText := ALines;
    CurrentText := BaseText;
    SetLines(ALines);

    BaseName := BaseName + BaseLinesToText;

DebugLn(BaseConf);
  end;

  Procedure DoSetText(ABaseName: String; ALines: array of const);
  begin
    BaseName := ABaseName;
    BaseStep := 0;
    BaseText := nil;
    SetLength(BaseText, 10);
    BaseText := LinesReplace(BaseText, ALines);
    CurrentText := BaseText;
    SetLines(BaseText);

    BaseName := BaseName + BaseLinesToText;

DebugLn(BaseConf);
  end;

  // Caret is logical
  Procedure DoNewLine(AName: string; AStartX, AStartY: Integer;
    ExpX, ExpY: Integer; ExpLineDiff: array of const; AFlags: TTestFlags = []);
  begin
    SynEdit.TestTypeText(AStartX, AStartY, #13);

    if tfExpLineDiffUseBase in AFlags
    then CurrentText := LinesReplace(BaseText, ExpLineDiff)
    else CurrentText := LinesReplace(CurrentText, ExpLineDiff);
    inc(BaseStep);

debugln([Format('[%s (%d)] %s -- Enter at (%d, %d)', [BaseName, BaseStep, AName, AStartX, AStartY])]);
debugln([dbgs(SynEdit.LogicalCaretXY), ': ', DbgStr(SynEdit.TestFullText)]);
    TestIsCaret(Format('[%s (%d)] %s -- Enter at (%d, %d) -- Exp Caret (%d, %d) :: %s', [BaseName, BaseStep, AName, AStartX, AStartY, ExpX, ExpY, BaseConf]),
                ExpX, ExpY);
    TestIsText(Format('[%s (%d)] %s -- Enter at (%d, %d) -> (%d, %d) - Exp Text (Changes: %d) :: %s', [BaseName, BaseStep, AName, AStartX, AStartY, ExpX, ExpY, length(ExpLineDiff), BaseConf]),
               CurrentText);
  end;

begin
  Beautifier := TSynBeautifierPascal.Create(nil);
  try
  ReCreateEdit;
  SynEdit.Beautifier := Beautifier;
  SynEdit.Options := SynEdit.Options - [eoTrimTrailingSpaces];// + [eoScrollPastEol];
  SynEdit.TabWidth := 4;
  UseFullText := True;
  Beautifier.IndentType := sbitCopySpaceTab;


  {%region Bor (Curly) }

    ConfigBeautifier(sctBor, [sciAddTokenLen, sciAddPastTokenIndent], 0, '',
                     sccPrefixMatch, scmMatchAfterOpening, sclMatchPrev,
                     sbitSpace,
                     '^ ?([\*#])', '$1');

    DoSetText('Curly simple 1',  [2, '  {* abc']);
    DoNewLine('',   7, 2,   6, 3,  [2, '  {* a', '   * bc']);  // 2:"  {* a|bc"
    DoNewLine('',   7, 3,   6, 4,  [3, '   * b', '   * c' ]);  // 3:"   * b|c" => 4:"   * |c"
    DoNewLine('',   7, 4,   6, 5,  [4, '   * c', '   * '  ]);  // 4:"   * c|"
    DoNewLine('',   5, 3,   5, 4,  [3, '   *',   '   * b' ]);  // 3:"   *| b"

    DoSetText('Curly simple 2',  [2, '    {* abc', '  * ']);
    DoNewLine('',   5, 3,   5, 4,  [3, '  * ', '  * ']);

    DoSetText('Curly simple 3',  [2, '    {*', '  *']);
    DoNewLine('',   4, 3,   4, 4,  [3, '  *', '  *']);


    DoSetText('Curly, not matching',  [2, '  {+ abc']);
    DoNewLine('',   7, 2,   3, 3,  [2, '  {+ a', '  bc']);  // 2:"  {* a|bc"

    DoSetText('Curly Nested 1',  [2, '  {* abc {', '     # def']);
    DoNewLine('',   9, 3,   8, 4,  [3, '     # d', '     # ef']);  // 2:"    {# d|ef"

// Todo: Nested that matches the "{", and uses smartSpace.
    ConfigBeautifier(sctBor, [sciAddTokenLen, sciAddPastTokenIndent], 0, '',
                     sccPrefixMatch, scmMatchOpening, sclMatchPrev,
                     sbitSpace,
                     '^\{ ?([\*#])', '$1');
    DoSetText('Bor Nested 1',  [2, '  { * abc ', '    { # def']);
// Todo: smart, must detect, that prevline = nest open
//    DoNewLine('',  10, 3,   9, 4,  [3, '    { # d', '      # ef']);  // 2:"    {# d|ef"


    {%region Bor (Curly) --- TSynCommentIndentFlag (Pre-PerfixIndent) }
      PushBaseName('Curly - TSynCommentIndentFlag');

      // sbitSpace, sbitCopySpaceTab, sbitPositionCaret
      for ParentIndentType := low(TSynBeautifierIndentType) to sbitPositionCaret // high(TSynBeautifierIndentType)
      do begin
        Beautifier.IndentType := ParentIndentType;
        WriteStr(PIName, ParentIndentType);
        PushBaseName(PIName);

        // sclMatchFirst;
        for LoopMatchFirst := 0 to 1 do
        begin
          if LoopMatchFirst = 0
          then MatchLine := sclMatchPrev
          else MatchLine := sclMatchFirst;
          PushBaseName(dbgs(MatchLine));

          // sciApplyIndentForNoMatch
          for LoopIndentApplyNoMatch := 0 to 1 do
          begin

            // sciAddTokenLen, sciAddPastTokenIndent;
            for LoopIndentAdds := 0 to 3 do
            begin


              ExtraIndentFlags := [];
              if LoopIndentApplyNoMatch = 1 then begin
                ExtraIndentFlags := ExtraIndentFlags + [sciApplyIndentForNoMatch];
                PushBaseName('ApplyIndNoMatch');
              end
              else PushBaseName('');

              if (LoopIndentAdds and 1) = 1 then begin
                ExtraIndentFlags := ExtraIndentFlags + [sciAddTokenLen];
                PushBaseName('AddTokLen');
              end
              else PushBaseName('');

              if (LoopIndentAdds and 2) = 2 then begin
                ExtraIndentFlags := ExtraIndentFlags + [sciAddPastTokenIndent];
                PushBaseName('AddPastTokInd');
              end
              else PushBaseName('');


              {%region [] use default indent}
                PushBaseName('IndType=[]');

                // AnsiIndentFirstLineMax, indent is >= 2, so it is not affeceted by 1 or 2
                for LoopIndentMax := 0 to 2 do
                begin
                  PushBaseName('Max='+IntToStr(LoopIndentMax));
                  ConfigBeautifier(sctBor, [] + ExtraIndentFlags, LoopIndentMax, '',
                                   sccPrefixMatch, scmMatchAfterOpening, MatchLine,
                                   sbitSpace,
                                   '^\s*\*', '*');

                  if not (sciAddTokenLen in ExtraIndentFlags) then begin

                    // Indent / matching
                    DoSetText('matching',  [2, '  {* abc']);
                    DoNewLine('after 1st',   7, 2,   5, 3,  [2, '  {* a', '  * bc']);  // 2:"  {* a|bc"
                    DoNewLine('any line',    6, 3,   5, 4,  [3, '  * b', '  * c']);    // 3:"  * b|c"

                    // Indent in comment "{  *" / matching
                    DoSetText('"{  *" matching',  [2, '  {  * abc']);
                    if not (sciAddPastTokenIndent in ExtraIndentFlags) then begin
                      DoNewLine('after 1st',   9, 2,   5, 3,  [2, '  {  * a', '  * bc']);  // 2:"  {  * a|bc"
                      DoNewLine('any line',    6, 3,   5, 4,  [3, '  * b', '  * c']);    // 3:"  * b|c"
                    end
                    else begin
                      DoNewLine('after 1st',   9, 2,   7, 3,  [2, '  {  * a', '    * bc']);  // 2:"  {  * a|bc"
                      DoNewLine('any line',    8, 3,   7, 4,  [3, '    * b', '    * c']);    // 3:"    * b|c"
                    end;

                    // Indent tabs / matching
                    if ParentIndentType in [sbitCopySpaceTab] then begin
                      DoSetText('tabs matching',  [2, #9' {*'#9' abc']);
                      DoNewLine('after 1st',   8, 2,   6, 3,  [2, #9' {*'#9' a', #9' *'#9' bc']);  // 2:"_ {*_ a|bc"
                      DoNewLine('any line',    7, 3,   6, 4,  [3, #9' *'#9' b', #9' *'#9' c']);    // 3:"  * b|c"
                    end;

                    // Indent, not BOL / matching
                    DoSetText('not BOL matching',  [2, '  ;{* abc']);
                    DoNewLine('after 1st',   8, 2,   5, 3,  [2, '  ;{* a', '  * bc']);  // 2:"  ;{* a|bc"
                    DoNewLine('any line',    6, 3,   5, 4,  [3, '  * b', '  * c']);    // 3:"  * b|c"

                    // Indent / NOT matching
                    DoSetText('not matching',  [2, '  {+ abc']);
                    DoNewLine('after 1st',   7, 2,   3, 3,  [2, '  {+ a', '  bc']);  // 2:"  {+ a|bc"
                    DoNewLine('any line',    4, 3,   3, 4,  [3, '  b', '  c']);      // 3:"  b|c"

                  end
                  else begin // [sciAddTokenLen]

                    // Indent / matching
                    DoSetText('matching',  [2, '  {* abc']);
                    DoNewLine('after 1st',   7, 2,   6, 3,  [2, '  {* a', '   * bc']);  // 2:"  {* a|bc"
                    DoNewLine('any line',    7, 3,   6, 4,  [3, '   * b', '   * c']);   // 3:"   * b|c"

                    // Indent in comment "{  *" / matching
                    DoSetText('"{  *" matching',  [2, '  {  * abc']);
                    if not (sciAddPastTokenIndent in ExtraIndentFlags) then begin
                      DoNewLine('after 1st',   9, 2,   6, 3,  [2, '  {  * a', '   * bc']);  // 2:"  {  * a|bc"
                      DoNewLine('any line',    7, 3,   6, 4,  [3, '   * b', '   * c']);     // 3:"   * b|c"
                    end
                    else begin
                      DoNewLine('after 1st',   9, 2,   8, 3,  [2, '  {  * a', '     * bc']);  // 2:"  {  * a|bc"
                      DoNewLine('any line',    9, 3,   8, 4,  [3, '     * b', '     * c']);   // 3:"     * b|c"
                    end;

                    // Indent tabs / matching
                    if ParentIndentType in [sbitCopySpaceTab] then begin
                      DoSetText('tabs matching',  [2, #9' {*'#9' abc']);
                      DoNewLine('after 1st',   8, 2,   7, 3,  [2, #9' {*'#9' a', #9'  *'#9' bc']);  // 2:"_  {*_ a|bc"
                      DoNewLine('any line',    8, 3,   7, 4,  [3, #9'  *'#9' b', #9'  *'#9' c']);   // 3:"   * b|c"
                    end;

                    // Indent, not BOL / matching
                    DoSetText('not BOL matching',  [2, '  ;{* abc']);
                    DoNewLine('after 1st',   8, 2,   6, 3,  [2, '  ;{* a', '   * bc']);  // 2:"  ;{* a|bc"
                    DoNewLine('any line',    7, 3,   6, 4,  [3, '   * b', '   * c']);    // 3:"  * b|c"

                    // Indent / NOT matching
                    DoSetText('not matching',  [2, '  {+ abc']);
                    if sciApplyIndentForNoMatch in ExtraIndentFlags then begin
                      DoNewLine('after 1st',   7, 2,   4, 3,  [2, '  {+ a', '   bc']);  // 2:"   {+ a|bc"
                      DoNewLine('any line',    5, 3,   4, 4,  [3, '   b', '   c']);     // 3:"   b|c"
                    end
                    else begin
                      DoNewLine('after 1st',   7, 2,   3, 3,  [2, '  {+ a', '  bc']);  // 2:"  {+ a|bc"
                      DoNewLine('any line',    4, 3,   3, 4,  [3, '  b', '  c']);      // 3:"  b|c"
                    end;

                  end;

                  PopBaseName;
                end; // LoopIndentMax;

                PushBaseName('Max='+IntToStr(10));
                ConfigBeautifier(sctBor, [] + ExtraIndentFlags, 10, '',
                                 sccPrefixMatch, scmMatchAfterOpening, MatchLine,
                                 sbitSpace,
                                 '^\s*\*', '*');
                if not (sciAddTokenLen in ExtraIndentFlags) then begin
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   5, 3,  [2, '  {* a', '  * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    6, 3,   5, 4,  [3, '  * b', '  * c']);    // 3:"  * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,   8, 3,  [2, '  ;;;{* a', '     * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',    9, 3,   8, 4,  [3, '     * b', '     * c']);    // 3:"  * b|c"

                  // Check_that_Indent_is_NOT_restored // SEE Check_that_Indent_is_restored
                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('NOT restore Align',  [2, '  ;;;{* abc', '  *']);
                  if ParentIndentType = sbitPositionCaret
                  then DoNewLine('after 2nd',   4, 3,   5, 4,  [3, '  *', '  *'])   // NOT added post indent of 1
                  else DoNewLine('after 2nd',   4, 3,   5, 4,  [3, '  *', '  * ']); // added post indent of 1
                end
                else begin // [sciAddTokenLen]
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   6, 3,  [2, '  {* a', '   * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    7, 3,   6, 4,  [3, '   * b', '   * c']);   // 3:"   * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,   9, 3,  [2, '  ;;;{* a', '      * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',   10, 3,   9, 4,  [3, '      * b', '      * c']);    // 3:"  * b|c"

                  // Check_that_Indent_is_NOT_restored // SEE Check_that_Indent_is_restored
                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('NOT restore Align',  [2, '  ;;;{* abc', '  *']);
                  if ParentIndentType = sbitPositionCaret
                  then DoNewLine('after 2nd',   4, 3,   5, 4,  [3, '  *', '  *'])
                  else DoNewLine('after 2nd',   4, 3,   5, 4,  [3, '  *', '  * ']);
                end;
                PopBaseName;


                PushBaseName('Etra="     " + Max=10');
                ConfigBeautifier(sctBor, [] + ExtraIndentFlags, 10, '     ',
                                 sccPrefixMatch, scmMatchAfterOpening, MatchLine,
                                 sbitSpace,
                                 '^\s*\*', '*');
                if not (sciAddTokenLen in ExtraIndentFlags) then begin
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   5, 3,  [2, '  {* a', '  * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    6, 3,   5, 4,  [3, '  * b', '  * c']);    // 3:"  * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,   8, 3,  [2, '  ;;;{* a', '     * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',    9, 3,   8, 4,  [3, '     * b', '     * c']);    // 3:"  * b|c"
                end
                else begin // [sciAddTokenLen]
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   6, 3,  [2, '  {* a', '   * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    7, 3,   6, 4,  [3, '   * b', '   * c']);   // 3:"   * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,   9, 3,  [2, '  ;;;{* a', '      * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',   10, 3,   9, 4,  [3, '      * b', '      * c']);    // 3:"  * b|c"
                end;
                PopBaseName;

                PushBaseName('Etra="     " + Max=0');
                ConfigBeautifier(sctBor, [] + ExtraIndentFlags, 0, '     ',
                                 sccPrefixMatch, scmMatchAfterOpening, MatchLine,
                                 sbitSpace,
                                 '^\s*\*', '*');
                if not (sciAddTokenLen in ExtraIndentFlags) then begin
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   5, 3,  [2, '  {* a', '  * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    6, 3,   5, 4,  [3, '  * b', '  * c']);    // 3:"  * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,  10, 3,  [2, '  ;;;{* a', '       * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',   11, 3,  10, 4,  [3, '       * b', '       * c']);  // 3:"  * b|c"
                end
                else begin // [sciAddTokenLen]
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   6, 3,  [2, '  {* a', '   * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    7, 3,   6, 4,  [3, '   * b', '   * c']);   // 3:"   * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,  11, 3,  [2, '  ;;;{* a', '        * bc']);   // 2:"  ;{* a|bc"
                  DoNewLine('any line',   12, 3,  11, 4,  [3, '        * b', '        * c']);  // 3:"  * b|c"
                end;
                PopBaseName;

                PopBaseName;
              {%endregion [] use default indent}


              {%region [sciNone] }
                PushBaseName('IndType=[]');

                // AnsiIndentFirstLineMax, indent is >= 2, so it is not affeceted by 1 or 2
                for LoopIndentMax := 0 to 2 do
                begin
                  PushBaseName('Max='+IntToStr(LoopIndentMax));
                  ConfigBeautifier(sctBor, [sciNone] + ExtraIndentFlags, LoopIndentMax, '',
                                   sccPrefixMatch, scmMatchAfterOpening, MatchLine,
                                   sbitSpace,
                                   '^\s*\*', '*');

                  if not (sciAddTokenLen in ExtraIndentFlags) then begin

                    // Indent / matching
                    DoSetText('matching',  [2, '  {* abc']);
                    DoNewLine('after 1st',   7, 2,   3, 3,  [2, '  {* a', '* bc']);  // 2:"  {* a|bc"
                    DoNewLine('any line',    4, 3,   3, 4,  [3, '* b', '* c']);      // 3:"* b|c"

                    // Indent in comment "{  *" / matching
                    DoSetText('"{  *" matching',  [2, '  {  * abc']);
                    if not (sciAddPastTokenIndent in ExtraIndentFlags) then begin
                      DoNewLine('after 1st',   9, 2,   3, 3,  [2, '  {  * a', '* bc']);  // 2:"  {  * a|bc"
                      DoNewLine('any line',    4, 3,   3, 4,  [3, '* b', '* c']);        // 3:"* b|c"
                    end
                    else begin
                      DoNewLine('after 1st',   9, 2,   5, 3,  [2, '  {  * a', '  * bc']);  // 2:"  {  * a|bc"
                      DoNewLine('any line',    6, 3,   3, 4,  [3, '  * b', '* c']);      // 3:"    * b|c"
                    end;

                    // Indent tabs / matching
                    if ParentIndentType in [sbitCopySpaceTab] then begin
                      DoSetText('tabs matching',  [2, #9' {*'#9' abc']);
                      DoNewLine('after 1st',   8, 2,   4, 3,  [2, #9' {*'#9' a', '*'#9' bc']);  // 2:"_ {*_ a|bc"
                      DoNewLine('any line',    5, 3,   4, 4,  [3, '*'#9' b', '*'#9' c']);       // 3:"  * b|c"
                    end;

                    // Indent, not BOL / matching
                    DoSetText('not BOL matching',  [2, '  ;{* abc']);
                    DoNewLine('after 1st',   8, 2,   3, 3,  [2, '  ;{* a', '* bc']);  // 2:"  ;{* a|bc"
                    DoNewLine('any line',    4, 3,   3, 4,  [3, '* b', '* c']);       // 3:"  * b|c"

                    // Indent / NOT matching
                    DoSetText('not matching',  [2, '  {+ abc']);
                    if sciApplyIndentForNoMatch in ExtraIndentFlags then begin
                      DoNewLine('after 1st',   7, 2,   1, 3,  [2, '  {+ a', 'bc']);  // 2:"  {+ a|bc"
                      DoNewLine('any line',    2, 3,   1, 4,  [3, 'b', 'c']);      // 3:"  b|c"
                    end
                    else begin
                      DoNewLine('after 1st',   7, 2,   3, 3,  [2, '  {+ a', '  bc']);  // 2:"  {+ a|bc"
                      DoNewLine('any line',    4, 3,   3, 4,  [3, '  b', '  c']);      // 3:"  b|c"
                    end;

                  end
                  else begin // [sciAddTokenLen]

                    // Indent / matching
                    DoSetText('matching',  [2, '  {* abc']);
                    DoNewLine('after 1st',   7, 2,   4, 3,  [2, '  {* a', ' * bc']);  // 2:"  {* a|bc"
                    DoNewLine('any line',    5, 3,   3, 4,  [3, ' * b', '* c']);   // 3:" * b|c"

                    // Indent in comment "{  *" / matching
                    DoSetText('"{  *" matching',  [2, '  {  * abc']);
                    if not (sciAddPastTokenIndent in ExtraIndentFlags) then begin
                      DoNewLine('after 1st',   9, 2,   4, 3,  [2, '  {  * a', ' * bc']);  // 2:"  {  * a|bc"
                      DoNewLine('any line',    5, 3,   3, 4,  [3, ' * b', '* c']);       // 3:" * b|c"
                    end
                    else begin
                      DoNewLine('after 1st',   9, 2,   6, 3,  [2, '  {  * a', '   * bc']);  // 2:"  {  * a|bc"
                      DoNewLine('any line',    7, 3,   3, 4,  [3, '   * b', '* c']);     // 3:"     * b|c"
                    end;

                    // Indent tabs / matching
                    if ParentIndentType in [sbitCopySpaceTab] then begin
                      DoSetText('tabs matching',  [2, #9' {*'#9' abc']);
                      DoNewLine('after 1st',   8, 2,   5, 3,  [2, #9' {*'#9' a', ' *'#9' bc']);  // 2:"_  {*_ a|bc"
                      DoNewLine('any line',    6, 3,   4, 4,  [3, ' *'#9' b', '*'#9' c']);      // 3:"   * b|c"
                    end;

                    // Indent, not BOL / matching
                    DoSetText('not BOL matching',  [2, '  ;{* abc']);
                    DoNewLine('after 1st',   8, 2,   4, 3,  [2, '  ;{* a', ' * bc']);  // 2:"  ;{* a|bc"
                    DoNewLine('any line',    5, 3,   3, 4,  [3, ' * b', '* c']);      // 3:"  * b|c"

                    // Indent / NOT matching
                    DoSetText('not matching',  [2, '  {+ abc']);
                    if sciApplyIndentForNoMatch in ExtraIndentFlags then begin
                      DoNewLine('after 1st',   7, 2,   2, 3,  [2, '  {+ a', ' bc']);  // 2:"   {+ a|bc"
                      DoNewLine('any line',    3, 3,   1, 4,  [3, ' b', 'c']);     // 3:"   b|c"
                    end
                    else begin
                      DoNewLine('after 1st',   7, 2,   3, 3,  [2, '  {+ a', '  bc']);  // 2:"  {+ a|bc"
                      DoNewLine('any line',    4, 3,   3, 4,  [3, '  b', '  c']);      // 3:"  b|c"
                    end;

                  end;

                  PopBaseName;
                end; // LoopIndentMax;

                PushBaseName('Max='+IntToStr(10));
                ConfigBeautifier(sctBor, [sciNone] + ExtraIndentFlags, 10, '',
                                 sccPrefixMatch, scmMatchAfterOpening, MatchLine,
                                 sbitSpace,
                                 '^\s*\*', '*');

                if not (sciAddTokenLen in ExtraIndentFlags) then begin
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   3, 3,  [2, '  {* a', '* bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    4, 3,   3, 4,  [3, '* b', '* c']);      // 3:"* b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,   8, 3,  [2, '  ;;;{* a', '     * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',    9, 3,   3, 4,  [3, '     * b', '* c']);    // 3:"  * b|c"
                end
                else begin // [sciAddTokenLen]
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   4, 3,  [2, '  {* a', ' * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    5, 3,   3, 4,  [3, ' * b', '* c']);   // 3:" * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,   9, 3,  [2, '  ;;;{* a', '      * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',   10, 3,   3, 4,  [3, '      * b', '* c']);    // 3:"  * b|c"
                end;
                PopBaseName;

(* ****
                PushBaseName('Etra="     " + Max=10');
                ConfigBeautifier(sctBor, [sciNone] + ExtraIndentFlags, 10, '     ',
                                 sccPrefixMatch, scmMatchAfterOpening, MatchLine,
                                 sbitSpace,
                                 '^\s*\*', '*');
                if not (sciAddTokenLen in ExtraIndentFlags) then begin
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   5, 3,  [2, '  {* a', '  * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    6, 3,   5, 4,  [3, '  * b', '  * c']);    // 3:"  * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,   8, 3,  [2, '  ;;;{* a', '     * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',    9, 3,   8, 4,  [3, '     * b', '     * c']);    // 3:"  * b|c"
                end
                else begin // [sciAddTokenLen]
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   6, 3,  [2, '  {* a', '   * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    7, 3,   6, 4,  [3, '   * b', '   * c']);   // 3:"   * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,   9, 3,  [2, '  ;;;{* a', '      * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',   10, 3,   9, 4,  [3, '      * b', '      * c']);    // 3:"  * b|c"
                end;
                PopBaseName;

                PushBaseName('Etra="     " + Max=0');
                ConfigBeautifier(sctBor, [sciNone] + ExtraIndentFlags, 0, '     ',
                                 sccPrefixMatch, scmMatchAfterOpening, MatchLine,
                                 sbitSpace,
                                 '^\s*\*', '*');
                if not (sciAddTokenLen in ExtraIndentFlags) then begin
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   5, 3,  [2, '  {* a', '  * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    6, 3,   5, 4,  [3, '  * b', '  * c']);    // 3:"  * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,  10, 3,  [2, '  ;;;{* a', '       * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',   11, 3,  10, 4,  [3, '       * b', '       * c']);  // 3:"  * b|c"
                end
                else begin // [sciAddTokenLen]
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   6, 3,  [2, '  {* a', '   * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    7, 3,   6, 4,  [3, '   * b', '   * c']);   // 3:"   * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,  11, 3,  [2, '  ;;;{* a', '        * bc']);   // 2:"  ;{* a|bc"
                  DoNewLine('any line',   12, 3,  11, 4,  [3, '        * b', '        * c']);  // 3:"  * b|c"
                end;
                PopBaseName;

**** *)

                PopBaseName;
              {%endregion [sciNone] }


              {%region [sciAlignOpen] }
                PushBaseName('IndType=[sciAlignOpen]');

                PushBaseName('Max='+IntToStr(LoopIndentMax));
                ConfigBeautifier(sctBor, [sciAlignOpen] + ExtraIndentFlags, 0, '',
                                 sccPrefixMatch, scmMatchAfterOpening, MatchLine,
                                 sbitSpace,
                                 '^\s*\*', '*');

                // most are default, because the opening is at start
                if not (sciAddTokenLen in ExtraIndentFlags) then begin

                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   5, 3,  [2, '  {* a', '  * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    6, 3,   5, 4,  [3, '  * b', '  * c']);    // 3:"  * b|c"

                  // Indent in comment "{  *" / matching
                  DoSetText('"{  *" matching',  [2, '  {  * abc']);
                  if not (sciAddPastTokenIndent in ExtraIndentFlags) then begin
                    DoNewLine('after 1st',   9, 2,   5, 3,  [2, '  {  * a', '  * bc']);  // 2:"  {  * a|bc"
                    DoNewLine('any line',    6, 3,   5, 4,  [3, '  * b', '  * c']);    // 3:"  * b|c"
                  end
                  else begin
                    DoNewLine('after 1st',   9, 2,   7, 3,  [2, '  {  * a', '    * bc']);  // 2:"  {  * a|bc"
                    DoNewLine('any line',    8, 3,   7, 4,  [3, '    * b', '    * c']);    // 3:"    * b|c"
                  end;

                  // Indent tabs / matching
                  if ParentIndentType in [sbitCopySpaceTab] then begin
                    DoSetText('tabs matching',  [2, #9' {*'#9' abc']);
                    DoNewLine('after 1st',   8, 2,   6, 3,  [2, #9' {*'#9' a', #9' *'#9' bc']);  // 2:"_ {*_ a|bc"
                    DoNewLine('any line',    7, 3,   6, 4,  [3, #9' *'#9' b', #9' *'#9' c']);    // 3:"  * b|c"
                  end;

                  // Indent, not BOL / matching
                  DoSetText('not BOL matching',  [2, '  ;{* abc']);
                  DoNewLine('after 1st',   8, 2,   6, 3,  [2, '  ;{* a', '   * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',    7, 3,   6, 4,  [3, '   * b', '   * c']);    // 3:"  * b|c"

                  // Indent / NOT matching
                  DoSetText('not matching',  [2, '  {+ abc']);
                  DoNewLine('after 1st',   7, 2,   3, 3,  [2, '  {+ a', '  bc']);  // 2:"  {+ a|bc"
                  DoNewLine('any line',    4, 3,   3, 4,  [3, '  b', '  c']);      // 3:"  b|c"

                end
                else begin // [sciAddTokenLen]

                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   6, 3,  [2, '  {* a', '   * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    7, 3,   6, 4,  [3, '   * b', '   * c']);   // 3:"   * b|c"

                  // Indent in comment "{  *" / matching
                  DoSetText('"{  *" matching',  [2, '  {  * abc']);
                  if not (sciAddPastTokenIndent in ExtraIndentFlags) then begin
                    DoNewLine('after 1st',   9, 2,   6, 3,  [2, '  {  * a', '   * bc']);  // 2:"  {  * a|bc"
                    DoNewLine('any line',    7, 3,   6, 4,  [3, '   * b', '   * c']);     // 3:"   * b|c"
                  end
                  else begin
                    DoNewLine('after 1st',   9, 2,   8, 3,  [2, '  {  * a', '     * bc']);  // 2:"  {  * a|bc"
                    DoNewLine('any line',    9, 3,   8, 4,  [3, '     * b', '     * c']);   // 3:"     * b|c"
                  end;

                  // Indent tabs / matching
                  if ParentIndentType in [sbitCopySpaceTab] then begin
                    DoSetText('tabs matching',  [2, #9' {*'#9' abc']);
                    DoNewLine('after 1st',   8, 2,   7, 3,  [2, #9' {*'#9' a', #9'  *'#9' bc']);  // 2:"_  {*_ a|bc"
                    DoNewLine('any line',    8, 3,   7, 4,  [3, #9'  *'#9' b', #9'  *'#9' c']);   // 3:"   * b|c"
                  end;

                  // Indent, not BOL / matching
                  DoSetText('not BOL matching',  [2, '  ;{* abc']);
                  DoNewLine('after 1st',   8, 2,   7, 3,  [2, '  ;{* a', '    * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',    8, 3,   7, 4,  [3, '    * b', '    * c']);    // 3:"  * b|c"

                  // Indent / NOT matching
                  DoSetText('not matching',  [2, '  {+ abc']);
                  if sciApplyIndentForNoMatch in ExtraIndentFlags then begin
                    DoNewLine('after 1st',   7, 2,   4, 3,  [2, '  {+ a', '   bc']);  // 2:"   {+ a|bc"
                    DoNewLine('any line',    5, 3,   4, 4,  [3, '   b', '   c']);     // 3:"   b|c"
                  end
                  else begin
                    DoNewLine('after 1st',   7, 2,   3, 3,  [2, '  {+ a', '  bc']);  // 2:"  {+ a|bc"
                    DoNewLine('any line',    4, 3,   3, 4,  [3, '  b', '  c']);      // 3:"  b|c"
                  end;

                end;
                PopBaseName;

                PushBaseName('Max='+IntToStr(10));
                ConfigBeautifier(sctBor, [sciAlignOpen] + ExtraIndentFlags, 10, '',
                                 sccPrefixMatch, scmMatchAfterOpening, MatchLine,
                                 sbitSpace,
                                 '^\s*\*', '*');
                if not (sciAddTokenLen in ExtraIndentFlags) then begin
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   5, 3,  [2, '  {* a', '  * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    6, 3,   5, 4,  [3, '  * b', '  * c']);    // 3:"  * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,   8, 3,  [2, '  ;;;{* a', '     * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',    9, 3,   8, 4,  [3, '     * b', '     * c']);    // 3:"  * b|c"
                end
                else begin // [sciAddTokenLen]
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   6, 3,  [2, '  {* a', '   * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    7, 3,   6, 4,  [3, '   * b', '   * c']);   // 3:"   * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,   9, 3,  [2, '  ;;;{* a', '      * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',   10, 3,   9, 4,  [3, '      * b', '      * c']);    // 3:"  * b|c"
                end;
                PopBaseName;

                PushBaseName('Max='+IntToStr(4)); // actually cut off at 4
                ConfigBeautifier(sctBor, [sciAlignOpen] + ExtraIndentFlags, 4, '',
                                 sccPrefixMatch, scmMatchAfterOpening, MatchLine,
                                 sbitSpace,
                                 '^\s*\*', '*');
                if not (sciAddTokenLen in ExtraIndentFlags) then begin
                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,   7, 3,  [2, '  ;;;{* a', '    * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',    8, 3,   7, 4,  [3, '    * b', '    * c']);     // 3:"  * b|c"
                end
                else begin // [sciAddTokenLen]
                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,   8, 3,  [2, '  ;;;{* a', '     * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',    9, 3,   8, 4,  [3, '     * b', '     * c']);    // 3:"  * b|c"
                end;
                PopBaseName;


(* ****
                PushBaseName('Etra="     " + Max=10');
                ConfigBeautifier(sctBor, [sciAlignOpen] + ExtraIndentFlags, 10, '     ',
                                 sccPrefixMatch, scmMatchAfterOpening, MatchLine,
                                 sbitSpace,
                                 '^\s*\*', '*');
                if not (sciAddTokenLen in ExtraIndentFlags) then begin
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   5, 3,  [2, '  {* a', '  * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    6, 3,   5, 4,  [3, '  * b', '  * c']);    // 3:"  * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,   8, 3,  [2, '  ;;;{* a', '     * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',    9, 3,   8, 4,  [3, '     * b', '     * c']);    // 3:"  * b|c"
                end
                else begin // [sciAddTokenLen]
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   6, 3,  [2, '  {* a', '   * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    7, 3,   6, 4,  [3, '   * b', '   * c']);   // 3:"   * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,   9, 3,  [2, '  ;;;{* a', '      * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',   10, 3,   9, 4,  [3, '      * b', '      * c']);    // 3:"  * b|c"
                end;
                PopBaseName;

                PushBaseName('Etra="     " + Max=0');
                ConfigBeautifier(sctBor, [sciAlignOpen] + ExtraIndentFlags, 0, '     ',
                                 sccPrefixMatch, scmMatchAfterOpening, MatchLine,
                                 sbitSpace,
                                 '^\s*\*', '*');
                if not (sciAddTokenLen in ExtraIndentFlags) then begin
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   5, 3,  [2, '  {* a', '  * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    6, 3,   5, 4,  [3, '  * b', '  * c']);    // 3:"  * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,  10, 3,  [2, '  ;;;{* a', '       * bc']);  // 2:"  ;{* a|bc"
                  DoNewLine('any line',   11, 3,  10, 4,  [3, '       * b', '       * c']);  // 3:"  * b|c"
                end
                else begin // [sciAddTokenLen]
                  // Indent / matching
                  DoSetText('matching',  [2, '  {* abc']);
                  DoNewLine('after 1st',   7, 2,   6, 3,  [2, '  {* a', '   * bc']);  // 2:"  {* a|bc"
                  DoNewLine('any line',    7, 3,   6, 4,  [3, '   * b', '   * c']);   // 3:"   * b|c"

                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('not BOL matching',  [2, '  ;;;{* abc']);
                  DoNewLine('after 1st',  10, 2,  11, 3,  [2, '  ;;;{* a', '        * bc']);   // 2:"  ;{* a|bc"
                  DoNewLine('any line',   12, 3,  11, 4,  [3, '        * b', '        * c']);  // 3:"  * b|c"
                end;
                PopBaseName;
**** *)

                // Check_that_Indent_is_restored  SEE Check_that_Indent_is_NOT_restored
                PushBaseName('Max='+IntToStr(10));
                ConfigBeautifier(sctBor, [sciAlignOpen] + ExtraIndentFlags, 0, '',
                                 sccPrefixMatch, scmMatchAfterOpening, MatchLine,
                                 sbitSpace,
                                 '^\s*\*', '*');
                if not (sciAddTokenLen in ExtraIndentFlags) then begin
                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('restore Align',  [2, '  ;;;{* abc', '  *']);
                  if ParentIndentType = sbitPositionCaret
                  then DoNewLine('after 2nd',   4, 3,   8, 4,  [3, '  *', '     *'])
                  else DoNewLine('after 2nd',   4, 3,   8, 4,  [3, '  *', '     * ']);

                  DoSetText('restore Align',  [2, '  ;;;{* abc', '           *', '  *']);
                  if ParentIndentType = sbitPositionCaret
                  then DoNewLine('any line',   4, 4,    8, 5,  [4, '  *', '     *'])
                  else DoNewLine('any line',   4, 4,    8, 5,  [4, '  *', '     * ']);
                end
                else begin // [sciAddTokenLen]
                  // Indent, not BOL / matching // AnsiIndentFirstLineMax applied
                  DoSetText('restore Align',  [2, '  ;;;{* abc', '  *']);
                  if ParentIndentType = sbitPositionCaret
                  then DoNewLine('after 2nd',   4, 3,   9, 4,  [3, '  *', '      *'])
                  else DoNewLine('after 2nd',   4, 3,   9, 4,  [3, '  *', '      * ']);

                  DoSetText('restore Align',  [2, '  ;;;{* abc', '           *', '  *']);
                  if ParentIndentType = sbitPositionCaret
                  then DoNewLine('any line',   4, 4,    9, 5,  [4, '  *', '      *'])
                  else DoNewLine('any line',   4, 4,    9, 5,  [4, '  *', '      * ']);
                end;
                PopBaseName;


                PopBaseName;
              {%endregion [sciAlignOpen] }


              PopBaseName;

            end; //LoopIndentAdds
            PopBaseName;
            PopBaseName;
          end; // LoopIndentApplyNoMatch
          PopBaseName;
        end; // LoopMatchFirst

        PopBaseName;
      end;
      Beautifier.IndentType := sbitCopySpaceTab;
      PopBaseName;
    {%endregion Bor (Curly) --- Pre-PerfixIndent }


    // sccNoPrefix;
    ConfigBeautifier(sctBor, [sciAddTokenLen], 0, '',
                     sccNoPrefix, scmMatchAfterOpening, sclMatchPrev,
                     sbitSpace,
                     '^\s*\*', '*');
    DoSetText('sccNoPrefix; matching',       [2, '  {* abc']);
    DoNewLine('after 1st',   7, 2,   4, 3,  [2, '  {* a', '   bc']);  // 2:"  {* a|bc"
    DoSetText('sccNoPrefix; NOT matching',   [2, '  {+ abc']);
    DoNewLine('after 1st',   7, 2,   3, 3,  [2, '  {+ a', '  bc']);  // 2:"  {* a|bc"
    Beautifier.BorIndentMode := [sciAddTokenLen, sciApplyIndentForNoMatch];
    DoSetText('sccNoPrefix; NOT matching, apply',   [2, '  {+ abc']);
    DoNewLine('after 1st',   7, 2,   4, 3,  [2, '  {+ a', '   bc']);  // 2:"  {* a|bc"

    // sccPrefixMatch;
    ConfigBeautifier(sctBor, [sciAddTokenLen], 0, '',
                     sccPrefixMatch, scmMatchAfterOpening, sclMatchPrev,
                     sbitSpace,
                     '^\s*\*', '*');
    DoSetText('sccPrefixMatch; matching',       [2, '  {* abc']);
    DoNewLine('after 1st',   7, 2,   6, 3,  [2, '  {* a', '   * bc']);  // 2:"  {* a|bc"
    DoSetText('sccPrefixMatch; NOT matching',   [2, '  {+ abc']);
    DoNewLine('after 1st',   7, 2,   3, 3,  [2, '  {+ a', '  bc']);  // 2:"  {* a|bc"
    Beautifier.BorIndentMode := [sciAddTokenLen, sciApplyIndentForNoMatch];
    DoSetText('sccPrefixMatch; NOT matching, apply',   [2, '  {+ abc']);
    DoNewLine('after 1st',   7, 2,   4, 3,  [2, '  {+ a', '   bc']);  // 2:"  {* a|bc"

    // sccPrefixAlways;
    ConfigBeautifier(sctBor, [sciAddTokenLen], 0, '',
                     sccPrefixAlways, scmMatchAfterOpening, sclMatchPrev,
                     sbitSpace,
                     '^\s*\*', '*');
    DoSetText('sccPrefixAlways; matching',       [2, '  {* abc']);
    DoNewLine('after 1st',   7, 2,   6, 3,  [2, '  {* a', '   * bc']);  // 2:"  {* a|bc"
    DoSetText('sccPrefixAlways; NOT matching',   [2, '  {+ abc']);
    DoNewLine('after 1st',   7, 2,   5, 3,  [2, '  {+ a', '   *bc']);  // 2:"  {* a|bc"
    Beautifier.BorIndentMode := [sciAddTokenLen, sciApplyIndentForNoMatch];
    DoSetText('sccPrefixAlways; NOT matching, apply',   [2, '  {+ abc']);
    DoNewLine('after 1st',   7, 2,   5, 3,  [2, '  {+ a', '   *bc']);  // 2:"  {* a|bc"
  {%endregion Bor (Curly) }


  {%region Ansi ( * }

    ConfigBeautifier(sctAnsi, [sciAddTokenLen, sciAddPastTokenIndent], 0, '',
                     sccPrefixMatch, scmMatchAfterOpening, sclMatchPrev,
                     sbitSpace,
                     '^ ?\*', '*');

    DoSetText('Ansi 2',  [2, '  (* * ']);
    DoNewLine('',   8, 2,   8, 3,  [2, '  (* * ', '     * ']);  // 2:"  (* * |"

    DoSetText('Ansi 3',  [2, '  (* *']);
    DoNewLine('',   7, 2,   7, 3,  [2, '  (* *', '     *']);  // 2:"  (* *|"

    ConfigBeautifier(sctAnsi, [sciAddTokenLen], 0, '',
                     sccPrefixMatch, scmMatchAtAsterisk, sclMatchPrev,
                     sbitSpace,
                     '^\*', '*');

    DoSetText('Ansi 1',  [2, '  (* abc']);
    DoNewLine('',   7, 2,   6, 3,  [2, '  (* a', '   * bc']);  // 2:"  (* a|bc"


    // scmMatchAfterOpening
    ConfigBeautifier(sctAnsi, [sciAddTokenLen, sciAddPastTokenIndent], 0, '',
                     sccPrefixMatch, scmMatchAfterOpening, sclMatchPrev,
                     sbitSpace,
                     '^\+', '+');
    DoSetText('scmMatchAfterOpening match Ansi',  [2, '  (*+ ab']);
    DoNewLine('',   8, 2,   7, 3,  [2, '  (*+ a', '    + b']);  // 2:"  (*+ a|b"
    DoSetText('scmMatchAfterOpening NO match Ansi',  [2, '  (*- ab']);
    DoNewLine('',   8, 2,   3, 3,  [2, '  (*- a', '  b']);      // 2:"  (*+ a|b"

    // scmMatchOpening
    ConfigBeautifier(sctAnsi, [sciAddTokenLen, sciAddPastTokenIndent], 0, '',
                     sccPrefixMatch, scmMatchOpening, sclMatchPrev,
                     sbitSpace,
                     '^\(\*\+', '+');
    DoSetText('scmMatchOpening match Ansi',  [2, '  (*+ ab']);
    DoNewLine('',   8, 2,   7, 3,  [2, '  (*+ a', '    + b']);  // 2:"  (*+ a|b"
    DoSetText('scmMatchOpening NO match Ansi',  [2, '  (*- ab']);
    DoNewLine('',   8, 2,   3, 3,  [2, '  (*- a', '  b']);      // 2:"  (*+ a|b"

    // scmMatchWholeLine;
    ConfigBeautifier(sctAnsi, [sciAddTokenLen, sciAddPastTokenIndent], 0, '',
                     sccPrefixMatch, scmMatchWholeLine, sclMatchPrev,
                     sbitSpace,
                     '^  \(\*\+', '+');
    DoSetText('scmMatchWholeLine; match Ansi',  [2, '  (*+ ab']);
    DoNewLine('',   8, 2,   7, 3,  [2, '  (*+ a', '    + b']);  // 2:"  (*+ a|b"
    DoSetText('scmMatchWholeLine; NO match Ansi',  [2, '  (*- ab']);
    DoNewLine('',   8, 2,   3, 3,  [2, '  (*- a', '  b']);      // 2:"  (*+ a|b"

    // scmMatchAtAsterisk;
    ConfigBeautifier(sctAnsi, [sciAddTokenLen, sciAddPastTokenIndent], 0, '',
                     sccPrefixMatch, scmMatchAtAsterisk, sclMatchPrev,
                     sbitSpace,
                     '^\*\+', '*+');
    DoSetText('scmMatchAtAsterisk; match Ansi',  [2, '  (*+ ab']);
    DoNewLine('',   8, 2,   7, 3,  [2, '  (*+ a', '   *+ b']);  // 2:"  (*+ a|b"
    DoSetText('scmMatchAtAsterisk; NO match Ansi',  [2, '  (*- ab']);
    DoNewLine('',   8, 2,   3, 3,  [2, '  (*- a', '  b']);      // 2:"  (*+ a|b"
  {%endregion Ansi ( * }


  {%region Slash // }
    ConfigBeautifier(sctSlash, [sciAddTokenLen, sciAddPastTokenIndent], 0, '',
                     sccPrefixMatch, scmMatchAfterOpening, sclMatchPrev,
                     sbitSpace,
                     '^ ?\*', '*',
                     sceMatching);

    DoSetText('Slash No match',  [2, '  // abc']);
    DoNewLine('',   7, 2,   3, 3,  [2, '  // a', '  bc']);  // 2:"  // a|bc"

    DoSetText('Slash ',  [2, '  // * abc']);
    DoNewLine('',   9, 2,   8, 3,  [2, '  // * a', '  // * bc']);  // 2:"  // * a|bc"


    ConfigBeautifier(sctSlash, [sciAddTokenLen, sciAddPastTokenIndent], 0, '',
                     sccPrefixMatch, scmMatchAfterOpening, sclMatchPrev,
                     sbitSpace,
                     '^ ?\*', '*',
                     sceSplitLine);

    DoSetText('Slash No match, EOL',  [2, '  // abc']);
    DoNewLine('',   9, 2,   3, 3,  [2, '  // abc', '  ']);  // 2:"  // abc|"

    DoSetText('Slash No match, past EOL',  [2, '  // abc']);
    DoNewLine('',  10, 2,   3, 3,  [2, '  // abc', '  ']);  // 2:"  // abc |"

    DoSetText('Slash No match, split',  [2, '  // abc']);
    DoNewLine('',   7, 2,   6, 3,  [2, '  // a', '  // bc']);  // 2:"  // a|bc"


    // aligOpen (single and multiline)
    ConfigBeautifier(sctSlash, [sciAlignOpen, sciAddTokenLen, sciAddPastTokenIndent], 0, '',
                     sccPrefixAlways, scmMatchAfterOpening, sclMatchPrev,
                     sbitSpace,
                     '^.?', '',
                     sceAlways);
    DoSetText('Slash sciAlignOpen',  [2, '  ;;; // abc']);
    DoNewLine('first',  11, 2,  10, 3,  [2, '  ;;; // a', '      // bc']);      // 2:"      // a|bc"
    DoNewLine('any',    11, 3,  10, 4,  [3,  '      // b', '      // c']);     // 2:"      // b|c"

    //DoSetText('Slash sciAlignOpen realign',  [2, '  ;;; // abc', '    // de']);
    //DoNewLine('2nd',   9, 3,  10, 4,  [3, '    // d', '      // e']);    // 3:"    // d|e"
    //
    //DoSetText('Slash sciAlignOpen realign',  [2, '  ;;; // abc', '                //', '    // de']);
    //DoNewLine('3rd',   9, 4,  10, 5,  [4, '    // d', '      // e']);    // 3:"    // d|e"
  {%endregion Slash // }


  finally
    SynEdit.Beautifier := nil;
    FreeAndNil(Beautifier);
  end;
// TODO: extra/smart  indent only allowed, if MATCHED

end;

initialization

  RegisterTest(TTestSynBeautifier);
end.

