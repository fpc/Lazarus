unit TestSynBeautifier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase,
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

  TTestSynBeautifier = class(TTestBase)
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
                                Editor: TObject;                       // the synedit
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
  end;


implementation

{ TTestSynBeautifier }

var SkipGroupUndo: Boolean;
procedure TTestSynBeautifier.TestRedoUndo(Name: String; Text: Array of String;
  X, Y: Integer; Data: array of const; SelX: Integer = -1; SelY: Integer = -1);

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

initialization

  RegisterTest(TTestSynBeautifier);
end.

