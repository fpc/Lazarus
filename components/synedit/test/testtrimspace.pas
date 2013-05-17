unit TestTrimSpace;

{$mode objfpc}{$H+}

interface

(* TODO

trim if the caret changes between identical lines
__abc|___
__abc

or between two empty lines
|___

*)

uses
  Classes, SysUtils, Forms, testregistry, TestBase, LCLProc, LCLType,
  SynEdit, SynEditKeyCmds, SynEditTextTrimmer;

type

  TTestTrimUndoEntry = record
    Command:  TSynEditorCommand;
    CaretWas: TPoint;
    VTextWas: String;
  end;

  { TTestTrimSpace }

  TTestTrimSpace = class(TTestBase)
  private
    FTName: String;
    FUseEc: Boolean;
    FUndoList: Array of TTestTrimUndoEntry;
    FRedoList: Array of TTestTrimUndoEntry;
    FUndoIndex: Integer;
  protected
    procedure test_assert(ASubName: String; AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1; AppendCr: Boolean = True);
    procedure test_assert(ASubName: String; AExp: Array of String; AExpX: Integer = -1; AExpY: Integer = -1; AppendCr: Boolean = True);
    procedure test_add_undo(Cmd: TSynEditorCommand; IsRedo: Boolean = False);

    procedure test_start(AName, Txt: String);
    procedure test_start(AName: String; Txt: Array of String);
    procedure test_start(AName: String; Txt: Array of String; X, Y: Integer);

    Procedure test_cmd(ASubName: String; X, Y: Integer; Cmd: TSynEditorCommand; AChar: TUTF8Char;
                       AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1);
    Procedure test_cmd(ASubName: String; Cmd: TSynEditorCommand;
                       AExp: Array of String; AExpX: Integer = -1; AExpY: Integer = -1;
                       SkipAddUndo: Boolean = False);
    Procedure test_caret(ASubName: String; X, Y: Integer;
                       AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1);

    Procedure test_type  (ASubName: String; AText: String;
                       AExp: Array of String; AExpX: Integer = -1; AExpY: Integer = -1);
    Procedure test_type  (ASubName: String; AText: String; X, Y: Integer;
                       AExp: Array of String; AExpX: Integer = -1; AExpY: Integer = -1);
    Procedure test_bspace(ASubName: String;
                       AExp: Array of String; AExpX: Integer = -1; AExpY: Integer = -1);
    Procedure test_bspace(ASubName: String; X, Y: Integer;
                       AExp: Array of String; AExpX: Integer = -1; AExpY: Integer = -1);
    Procedure test_del   (ASubName: String;
                       AExp: Array of String; AExpX: Integer = -1; AExpY: Integer = -1);
    Procedure test_del   (ASubName: String; X, Y: Integer;
                       AExp: Array of String; AExpX: Integer = -1; AExpY: Integer = -1);

    Procedure test_undo(ASubName: String; SkipCaretTest: Boolean = False);
    Procedure test_redo(ASubName: String; SkipCaretTest: Boolean = False);
    Procedure test_undo_exp(ASubName: String;
                       AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1);
    Procedure test_undo_exp(ASubName: String;
                       AExp: Array of String; AExpX: Integer = -1; AExpY: Integer = -1);
    Procedure test_redo_exp(ASubName: String;
                       AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1);
    Procedure test_redo_exp(ASubName: String;
                       AExp: Array of String; AExpX: Integer = -1; AExpY: Integer = -1);
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TrimUndoRedo;
    procedure TrimUndoRedoEc;
    procedure NoTrimUndoRedo;
    procedure TestInUndoBlock;
    procedure TimmEdit;
  end;

implementation
const
  cr: String = LineEnding;

procedure TTestTrimSpace.SetUp;
begin
  inherited;
  FUseEc := False;
end;

procedure TTestTrimSpace.TearDown;
begin
  SetLength(FUndoList, 0);
  inherited TearDown;
end;

procedure TTestTrimSpace.test_assert(ASubName: String; AExp: String = '';
  AExpX: Integer = -1; AExpY: Integer = -1; AppendCr: Boolean = True);
begin
  if AppendCr then AExp := AExp + cr;
  if AExp <> '' then
    TestCompareString(FTName+' ('+ASubName+') Text', AExp, SynEdit.ViewedTextBuffer.Text);
  if AExpX > 0 then begin
    AssertEquals(FTName+' ('+ASubName+') Caret X', AExpX, SynEdit.CaretX);
    AssertEquals(FTName+' ('+ASubName+') Caret Y', AExpY, SynEdit.CaretY);
  end;
//debugln(['done ',ASubName]);
end;

procedure TTestTrimSpace.test_assert(ASubName: String; AExp: array of String; AExpX: Integer;
  AExpY: Integer; AppendCr: Boolean = True);
begin
  test_assert(ASubName, LinesToText(AExp), AExpX, AExpY, AppendCr);
end;

procedure TTestTrimSpace.test_add_undo(Cmd: TSynEditorCommand; IsRedo: Boolean);
begin
  if not IsRedo then begin
    inc(FUndoIndex);
    SetLength(FUndoList, FUndoIndex);
    FUndoList[FUndoIndex-1].Command := Cmd;
    FUndoList[FUndoIndex-1].CaretWas := SynEdit.CaretXY;
    FUndoList[FUndoIndex-1].VTextWas := SynEdit.ViewedTextBuffer.Text;
  end
  else begin
    SetLength(FRedoList, FUndoIndex);
    FRedoList[FUndoIndex-1].Command := Cmd;
    FRedoList[FUndoIndex-1].CaretWas := SynEdit.CaretXY;
    FRedoList[FUndoIndex-1].VTextWas := SynEdit.ViewedTextBuffer.Text;
  end;
end;

procedure TTestTrimSpace.test_start(AName, Txt: String);
begin
  FTName := AName;
  SynEdit.Text := Txt;
  SetLength(FUndoList, 0);
  FUndoIndex := 0;
//debugln(['----- START ',AName]);
end;

procedure TTestTrimSpace.test_start(AName: String; Txt: array of String);
begin
  test_start(AName, LinesToText(Txt));
end;

procedure TTestTrimSpace.test_start(AName: String; Txt: array of String; X, Y: Integer);
begin
  test_start(AName, LinesToText(Txt));
  SynEdit.CaretXY := Point(x, y);
end;

procedure TTestTrimSpace.test_cmd(ASubName: String; X, Y: Integer; Cmd: TSynEditorCommand;
  AChar: TUTF8Char; AExp: String; AExpX: Integer; AExpY: Integer);
begin
  test_add_undo(Cmd);

  SynEdit.CaretXY := Point(x, y);
  SynEdit.CommandProcessor(Cmd, AChar, nil);

  test_add_undo(Cmd, True);
  test_assert('cmd: '+ASubName, AExp, AExpX, AExpY);
end;

procedure TTestTrimSpace.test_cmd(ASubName: String; Cmd: TSynEditorCommand;
  AExp: array of String; AExpX: Integer; AExpY: Integer; SkipAddUndo: Boolean);
begin
  if not SkipAddUndo then
    test_add_undo(Cmd);

  SynEdit.CommandProcessor(Cmd, '', nil);

  if not SkipAddUndo then
    test_add_undo(Cmd, True);
  test_assert('cmd: '+ASubName, AExp, AExpX, AExpY);
end;

procedure TTestTrimSpace.test_caret(ASubName: String; X, Y: Integer; AExp: String;
  AExpX: Integer; AExpY: Integer);
begin
  SynEdit.CaretXY := Point(x, y);
  test_assert('caret: '+ASubName, AExp, AExpX, AExpY);
end;

procedure TTestTrimSpace.test_type(ASubName: String; AText: String; AExp: array of String;
  AExpX: Integer; AExpY: Integer);
begin
  test_add_undo(ecChar);
  SynEdit.TestTypeText(AText);
  test_add_undo(ecNone, True);
  test_assert('Typed: '+ASubName, AExp, AExpX, AExpY);
end;

procedure TTestTrimSpace.test_type(ASubName: String; AText: String; X, Y: Integer;
  AExp: array of String; AExpX: Integer; AExpY: Integer);
begin
  test_add_undo(ecChar);
  SynEdit.CaretXY := Point(x, y);
  SynEdit.TestTypeText(AText);
  test_add_undo(ecNone, True);
  test_assert('Typed: '+ASubName, AExp, AExpX, AExpY);
end;

procedure TTestTrimSpace.test_bspace(ASubName: String; AExp: array of String; AExpX: Integer;
  AExpY: Integer);
begin
  test_add_undo(ecDeleteLastChar);
  SynEdit.CommandProcessor(ecDeleteLastChar, '', nil);
  test_add_undo(ecNone, True);
  test_assert('BSpace: '+ASubName, AExp, AExpX, AExpY);
end;

procedure TTestTrimSpace.test_bspace(ASubName: String; X, Y: Integer; AExp: array of String;
  AExpX: Integer; AExpY: Integer);
begin
  test_add_undo(ecDeleteLastChar);
  SynEdit.CaretXY := Point(x, y);
  SynEdit.CommandProcessor(ecDeleteLastChar, '', nil);
  test_add_undo(ecNone, True);
  test_assert('BSpace: '+ASubName, AExp, AExpX, AExpY);
end;

procedure TTestTrimSpace.test_del(ASubName: String; AExp: array of String; AExpX: Integer;
  AExpY: Integer);
begin
  test_add_undo(ecDeleteChar);
  SynEdit.CommandProcessor(ecDeleteChar, '', nil);
  test_add_undo(ecNone, True);
  test_assert('Del: '+ASubName, AExp, AExpX, AExpY);
end;

procedure TTestTrimSpace.test_del(ASubName: String; X, Y: Integer; AExp: array of String;
  AExpX: Integer; AExpY: Integer);
begin
  test_add_undo(ecDeleteChar);
  SynEdit.CaretXY := Point(x, y);
  SynEdit.CommandProcessor(ecDeleteChar, '', nil);
  test_add_undo(ecNone, True);
  test_assert('Del: '+ASubName, AExp, AExpX, AExpY);
end;

procedure TTestTrimSpace.test_undo(ASubName: String; SkipCaretTest: Boolean);
begin
  if FUseEc then
    SynEdit.CommandProcessor(ecUndo, ' ', nil)
  else
    SynEdit.Undo;
  AssertTrue('Undo selftest', FUndoIndex > 0);
  dec(FUndoIndex);
  if SkipCaretTest then
    test_assert('undo: '+ASubName, FUndoList[FUndoIndex].VTextWas,
                -1, -1,
                False)
  else
    test_assert('undo: '+ASubName, FUndoList[FUndoIndex].VTextWas,
                FUndoList[FUndoIndex].CaretWas.X, FUndoList[FUndoIndex].CaretWas.Y,
                False);
end;

procedure TTestTrimSpace.test_redo(ASubName: String; SkipCaretTest: Boolean);
begin
  if FUseEc then
    SynEdit.CommandProcessor(ecRedo, ' ', nil)
  else
    SynEdit.Redo;
  assert(FUndoIndex < length(FRedoList));
  if SkipCaretTest then
    test_assert('undo: '+ASubName, FRedoList[FUndoIndex].VTextWas,
                -1, -1,
                False)
  else
    test_assert('undo: '+ASubName, FRedoList[FUndoIndex].VTextWas,
                FRedoList[FUndoIndex].CaretWas.X, FRedoList[FUndoIndex].CaretWas.Y,
                False);
  inc(FUndoIndex);
end;

procedure TTestTrimSpace.test_undo_exp(ASubName: String; AExp: String; AExpX: Integer;
  AExpY: Integer);
begin
  if FUseEc then
    SynEdit.CommandProcessor(ecUndo, ' ', nil)
  else
    SynEdit.Undo;
  test_assert('undo: '+ASubName, AExp, AExpX, AExpY);
end;

procedure TTestTrimSpace.test_undo_exp(ASubName: String; AExp: array of String;
  AExpX: Integer; AExpY: Integer);
begin
  test_undo_exp(ASubName, LinesToText(AExp), AExpX, AExpY);
end;

procedure TTestTrimSpace.test_redo_exp(ASubName: String; AExp: String; AExpX: Integer;
  AExpY: Integer);
begin
  if FUseEc then
    SynEdit.CommandProcessor(ecRedo, ' ', nil)
  else
    SynEdit.Redo;
  test_assert('redo: '+ASubName, AExp, AExpX, AExpY);
end;

procedure TTestTrimSpace.test_redo_exp(ASubName: String; AExp: array of String;
  AExpX: Integer; AExpY: Integer);
begin
  test_redo_exp(ASubName, LinesToText(AExp), AExpX, AExpY);
end;

procedure TTestTrimSpace.TrimUndoRedo;
  procedure UndoRedo(AMax: Integer; AName: String = ''; SkipCaretTest: Boolean = False);
  var
    i, j: Integer;
  begin
    case AMax of
      1: begin
          test_undo(AName, SkipCaretTest);
          test_redo(AName, SkipCaretTest);
        end;
      2: begin
        j := FUndoIndex;
          for i := 1 to j do test_undo(Format('%s %d/%d', [AName, i, j]), SkipCaretTest);
          for i := 1 to j do test_redo(Format('%s %d/%d', [AName, i, j]), SkipCaretTest);
        end;
    end;
  end;
var
  MaxUndo: Integer;
begin
// must have eoScrollPastEol
// because ForcePastEOL does not work on longest line
  SynEdit.Options := [eoTrimTrailingSpaces, eoAutoIndent, eoScrollPastEol]; // eoScrollPastEol, eoGroupUndo
  SynEdit.TabWidth := 6;
  for MaxUndo := 0 to 2 do begin
    {%region    settLeaveLine }
      SynEdit.TrimSpaceType := settLeaveLine;

      // no trim
      test_start ('Append Space and text',    ['', 'abc'],        4,2);
      test_type  ('Space 1',   ' ',           ['', 'abc '],       5,2); UndoRedo(MaxUndo);
      test_type  ('Space 2',   ' ',           ['', 'abc  '],      6,2); UndoRedo(MaxUndo);
      test_type  ('Char',      'A',           ['', 'abc  A'],     7,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                ['', 'abc  '],      6,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                ['', 'abc '],       5,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                ['', 'abc'],        4,2); UndoRedo(MaxUndo);

      // no trim
      test_start ('Append tab + Space and text', ['', 'abc'],        4,2);
      test_type  ('tab 1',      #9,              ['', 'abc'#9],         7,2); UndoRedo(MaxUndo);
      test_type  ('Space 2',   ' ',              ['', 'abc'#9' '],      8,2); UndoRedo(MaxUndo);
      test_type  ('Char',      'A',              ['', 'abc'#9' A'],     9,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                   ['', 'abc'#9' '],      8,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                   ['', 'abc'#9],         7,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                   ['', 'abc'],           4,2); UndoRedo(MaxUndo);

      // trim on leave line
      test_start ('Append Space, move left, text, trim', ['', 'abc'],        4,2);
      test_type  ('Space 1',   ' ',                   ['', 'abc '],       5,2); UndoRedo(MaxUndo);
      test_type  ('Space 2',   ' ',                   ['', 'abc  '],      6,2); UndoRedo(MaxUndo);
      test_cmd   ('caret left',ecLeft,                ['', 'abc  '],      5,2,  True);
      test_type  ('Char',      'A',                   ['', 'abc A '],     6,2); UndoRedo(MaxUndo);
      test_cmd   ('caret up',  ecUp,                  ['', 'abc A'],      6,1,  True);
      test_undo  ('');

      test_start ('Insert CR, and trim space before CR ', ['', 'abc def'],        5,2);
      test_type  ('CR',        #13,                       ['', 'abc', 'def'],     1,3);
      UndoRedo(MaxUndo);

      test_start ('Replace select by CR, and trim before sel', ['', 'abc def,123'],     5,2);
      SetCaretAndSel(5,2, 8,2); // 'def'
      test_type  ('CR',        #13,                            ['', 'abc', ',123'],     1,3);
      UndoRedo(MaxUndo);

    {%endregion settLeaveLine }

    {%region    settEditLine }
      SynEdit.TrimSpaceType := settEditLine;

      // no trim
      test_start ('Append Space and text',    ['', 'abc'],        4,2);
      test_type  ('Space 1',   ' ',           ['', 'abc '],       5,2); UndoRedo(MaxUndo);
      test_type  ('Space 2',   ' ',           ['', 'abc  '],      6,2); UndoRedo(MaxUndo);
      test_type  ('Char',      'A',           ['', 'abc  A'],     7,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                ['', 'abc  '],      6,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                ['', 'abc '],       5,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                ['', 'abc'],        4,2); UndoRedo(MaxUndo);

      // no trim
      test_start ('Append tab + Space and text', ['', 'abc'],        4,2);
      test_type  ('tab 1',      #9,              ['', 'abc'#9],         7,2); UndoRedo(MaxUndo);
      test_type  ('Space 2',   ' ',              ['', 'abc'#9' '],      8,2); UndoRedo(MaxUndo);
      test_type  ('Char',      'A',              ['', 'abc'#9' A'],     9,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                   ['', 'abc'#9' '],      8,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                   ['', 'abc'#9],         7,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                   ['', 'abc'],           4,2); UndoRedo(MaxUndo);

      // trim on edit
      test_start ('Append Space, move left, text',    ['', 'abc'],        4,2);
      test_type  ('Space 1',   ' ',                   ['', 'abc '],       5,2); UndoRedo(MaxUndo);
      test_type  ('Space 2',   ' ',                   ['', 'abc  '],      6,2); UndoRedo(MaxUndo);
      test_cmd   ('caret left',ecLeft,                ['', 'abc  '],      5,2,  True);
      test_type  ('Char',      'A',                   ['', 'abc A'],      6,2); UndoRedo(MaxUndo); // Trimmed on Edit
      test_cmd   ('caret up',  ecUp,                  ['', 'abc A'],      6,1,  True);
      test_undo  ('');

      test_start ('Insert CR, and trim space before CR ', ['', 'abc def'],        5,2);
      test_type  ('CR',        #13,                       ['', 'abc', 'def'],     1,3);
      UndoRedo(MaxUndo);

      test_start ('Replace select by CR, and trim before sel', ['', 'abc def,123'],     5,2);
      SetCaretAndSel(5,2, 8,2); // 'def'
      test_type  ('CR',        #13,                            ['', 'abc', ',123'],     1,3);
      UndoRedo(MaxUndo);

    {%endregion settEditLine }

    {%region    settMoveCaret }
      SynEdit.TrimSpaceType := settMoveCaret;

      // no trim
      test_start ('Append Space and text',    ['', 'abc'],        4,2);
      test_type  ('Space 1',   ' ',           ['', 'abc '],       5,2); UndoRedo(MaxUndo);
      test_type  ('Space 2',   ' ',           ['', 'abc  '],      6,2); UndoRedo(MaxUndo);
      test_type  ('Char',      'A',           ['', 'abc  A'],     7,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                ['', 'abc  '],      6,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                ['', 'abc '],       5,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                ['', 'abc'],        4,2); UndoRedo(MaxUndo);

      // no trim
      test_start ('Append tab + Space and text', ['', 'abc'],        4,2);
      test_type  ('tab 1',      #9,              ['', 'abc'#9],         7,2); UndoRedo(MaxUndo);
      test_type  ('Space 2',   ' ',              ['', 'abc'#9' '],      8,2); UndoRedo(MaxUndo);
      test_type  ('Char',      'A',              ['', 'abc'#9' A'],     9,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                   ['', 'abc'#9' '],      8,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                   ['', 'abc'#9],         7,2); UndoRedo(MaxUndo);
      test_bspace('BackSpace',                   ['', 'abc'],           4,2); UndoRedo(MaxUndo);

      // trim on move caret
      test_start ('Append Space, move left, text',    ['', 'abc'],        4,2);
      test_type  ('Space 1',   ' ',                   ['', 'abc '],       5,2); UndoRedo(MaxUndo);
      test_type  ('Space 2',   ' ',                   ['', 'abc  '],      6,2); UndoRedo(MaxUndo);
      test_cmd   ('caret left',ecLeft,                ['', 'abc '],       5,2,  True);
// TODO: Review: The spacethat was just trimmed, can be undone (leading to an undo that changes nothing)
      test_type  ('Char',      'A',                   ['', 'abc A'],      6,2); UndoRedo(MaxUndo); // Trimmed on Edit
      test_cmd   ('caret up',  ecUp,                  ['', 'abc A'],      6,1,  True);
//TODO: FAIL: This fails with MaxUndo = 2
//      test_undo  ('');

      test_start ('Insert CR, and trim space before CR ', ['', 'abc def'],        5,2);
      test_type  ('CR',        #13,                       ['', 'abc', 'def'],     1,3);
      UndoRedo(MaxUndo);

      test_start ('Replace select by CR, and trim before sel', ['', 'abc def,123'],     5,2);
      SetCaretAndSel(5,2, 8,2); // 'def'
      test_type  ('CR',        #13,                            ['', 'abc', ',123'],     1,3);
      UndoRedo(MaxUndo);

    {%endregion settMoveCaret }

    {%region    settIgnoreAll }
      SynEdit.TrimSpaceType := settIgnoreAll;

      // no trim
      test_start ('Append Space and text',    ['', 'abc'],        4,2);
      test_type  ('Space 1',   ' ',           ['', 'abc'],        5,2);
      test_type  ('Space 2',   ' ',           ['', 'abc'],        6,2);
      test_type  ('Char',      'A',           ['', 'abc  A'],     7,2);
      if MaxUndo > 0 then begin
        test_undo_exp('undo',                   ['', 'abc'],        6,2); // only undo the "A"
        test_redo_exp('redo',                   ['', 'abc  A'],     7,2);
      end;
      test_bspace('BackSpace',                ['', 'abc'],        6,2);
      test_bspace('BackSpace',                ['', 'abc'],        5,2);
      test_bspace('BackSpace',                ['', 'abc'],        4,2);

      // no trim
      test_start ('Append tab + Space and text', ['', 'abc'],           4,2);
// TODO: currently tab acts as space, because Log/Phys can not work, if the tab is not present
      test_type  ('tab 1',      #9,              ['', 'abc'],           7,2);
      test_type  ('Space 2',   ' ',              ['', 'abc'],           8,2);
      test_type  ('Char',      'A',              ['', 'abc    A'],     9,2);
      if MaxUndo > 0 then begin
        test_undo_exp('undo',                   ['', 'abc'],        8,2); // only undo the "A"
        test_redo_exp('redo',                   ['', 'abc    A'],     9,2);
      end;
      test_bspace('BackSpace',                   ['', 'abc'],           8,2);
      test_bspace('BackSpace',                   ['', 'abc'],           7,2);
      test_bspace('BackSpace',                   ['', 'abc'],           6,2);

      //
      test_start ('Append Space, move left, text',    ['', 'abc'],        4,2);
      test_type  ('Space 1',   ' ',                   ['', 'abc'],        5,2);
      test_type  ('Space 2',   ' ',                   ['', 'abc'],        6,2);
      test_cmd   ('caret left',ecLeft,                ['', 'abc'],        5,2,  True);
      test_type  ('Char',      'A',                   ['', 'abc A'],      6,2);
      test_cmd   ('caret up',  ecUp,                  ['', 'abc A'],      6,1,  True);

      test_start ('Insert CR, and trim space before CR ', ['', 'abc def'],        5,2);
      test_type  ('CR',        #13,                       ['', 'abc', 'def'],     1,3);
      UndoRedo(MaxUndo);

      test_start ('Replace select by CR, and trim before sel', ['', 'abc def,123'],     5,2);
      SetCaretAndSel(5,2, 8,2); // 'def'
      test_type  ('CR',        #13,                            ['', 'abc', ',123'],     1,3);
      UndoRedo(MaxUndo);

    {%endregion settIgnoreAll }
  end;

  SynEdit.Options := [eoTrimTrailingSpaces, eoAutoIndent, eoScrollPastEol]; // eoGroupUndo
  SynEdit.TrimSpaceType := settLeaveLine;

  { Edit Lines }
  (* *)
  test_start('Delete leaves trimable space', cr+'abc d');
  test_cmd  ('Backspace 6,2', 6,2, ecDeleteLastChar, '', cr+'abc ', 5,2);
  test_caret('trim', 1,1, cr+'abc', 1,1);
  // undo
//#  test_undo_exp('undo trim', cr+'abc ', 5,2);
  test_undo_exp('undo bspace', cr+'abc d', 6,2);
  test_redo_exp('redo bspace', cr+'abc ', 5,2);
  test_caret('trim', 1,1, cr+'abc', 1,1);
  // undo again
//#  test_undo_exp('undo trim(2)', cr+'abc ', 5,2);
  test_undo_exp('undo bspace(2)', cr+'abc d', 6,2);
  test_redo_exp('redo bspace(2)', cr+'abc ', 5,2);
  test_redo_exp('no redo trim(2)', cr+'abc ', 5,2);
  // undo again
  test_caret('trim(3)', 1,1, cr+'abc', 1,1);
//#  test_undo_exp('undo trim(3)', cr+'abc ', 5,2);
//##  test_caret('trim(3)', 1,1, cr+'abc', 1,1);

  (* *)
  test_start('insert past real space', cr+'abc ');
  test_cmd  ('insert 5,2', 5,2, ecChar, 'x', cr+'abc x', 6,2);
  // undo
  test_undo_exp('undo insert', cr+'abc ', 5,2);
  test_caret('no trim', 1,1, cr+'abc ', 1,1);
  // rdeo/undo again
  test_redo_exp('redo insert', cr+'abc x', 6,2);
  test_undo_exp('undo insert(2)', cr+'abc ', 5,2);
  test_caret('no trim(2)', 1,1, cr+'abc ', 1,1);

  (* *)
  test_start('insert spaces past real space', cr+'abc ');
  test_cmd  ('insert 5,2', 5,2, ecChar, ' ', cr+'abc  ', 6,2);
  test_cmd  ('insert 6,2', 6,2, ecChar, ' ', cr+'abc   ', 7,2);
  // undo
  test_undo_exp('undo insert2', cr+'abc  ', 6,2);
  test_undo_exp('undo insert1', cr+'abc ',  5,2);
  test_caret('no trim', 1,1, cr+'abc ', 1,1);
  // rdeo/undo again
  test_redo_exp('redo insert1', cr+'abc  ',  6,2);
  test_redo_exp('redo insert2', cr+'abc   ', 7,2);
  test_undo_exp('undo inserts2(2)', cr+'abc  ', 6,2);
  test_undo_exp('undo inserts1(2)', cr+'abc ',  5,2);
  test_caret('no trim(2)', 1,1, cr+'abc ', 1,1);


  (* *)
  test_start('insert spaces,char', cr+'abc');
  test_cmd  ('insert 4,2', 4,2, ecChar, ' ', cr+'abc ',  5,2);
  test_cmd  ('insert 5,2', 5,2, ecChar, 'x', cr+'abc x', 6,2);
  // undo
  test_undo_exp('undo ins char', cr+'abc ', 5,2);
  test_caret('trim', 1,1, cr+'abc', 1,1); /// xxx this undo, remose the ability to redo....

  (* *)
  test_start('insert spaces,char after space', cr+'ab ');
  test_cmd  ('insert 4,2', 4,2, ecChar, ' ', cr+'ab  ',  5,2);
  test_cmd  ('insert 5,2', 5,2, ecChar, 'x', cr+'ab  x', 6,2);
  // undo
  test_undo_exp('undo ins char', cr+'ab  ', 5,2);
  test_caret('trim', 1,1, cr+'ab', 1,1); /// xxx this undo, remose the ability to redo.... xxxxxxxxxxxxxxxx
  // We can no longer undo back to the real space

  (* *)
  test_start('insert spaces,char after space (2)', cr+'ab ');
  test_cmd  ('insert 4,2', 4,2, ecChar, ' ', cr+'ab  ',  5,2);
  test_cmd  ('insert 5,2', 5,2, ecChar, 'x', cr+'ab  x', 6,2);
  // undo
  test_undo_exp('undo ins char', cr+'ab  ', 5,2);
  test_undo_exp('undo ins space', cr+'ab ', 4,2);
  test_caret('no trim', 1,1, cr+'ab ', 1,1); /// xxx this undo, remose the ability to redo....

  (* *)
  test_start('insert past eol', cr+'abc ');
  test_cmd  ('insert 8,2', 8,2, ecChar, 'x', cr+'abc    x', 9,2);
  test_undo_exp('undo insert', cr+'abc ', 8,2);
  test_caret('trim', 1,1, cr+'abc ', 1,1);
  // TODO: redo

  { Insert LineBreaks }
  (* *)
  test_start('insert newline', cr+'ab 1234 ');
  test_cmd  ('insert 4,2', 4,2, ecLineBreak, ' ', cr+'ab'+cr+'1234 ',  1,3);
  test_caret('trim', 1,1, cr+'ab'+cr+'1234', 1,1);
//#  test_undo_exp('-trim', cr+'ab'+cr+'1234 ', 6,3);  // TODO caret at:   1,3);
  test_undo_exp('-newline', cr+'ab 1234 ', 4,2);
  test_caret('no trim', 1,1, cr+'ab 1234 ', 1,1);

  (* *)
  test_start('insert newline+indent', cr+'  ab ');
  test_cmd  ('insert 6,2', 6,2, ecLineBreak, ' ', cr+'  ab'+cr+'  ',              3,3);
  test_cmd  ('insert 3,3', 3,3, ecLineBreak, ' ', cr+'  ab'+cr+''+cr+'  ',        3,4);
  test_cmd  ('insert 3,4', 3,4, ecLineBreak, ' ', cr+'  ab'+cr+''+cr+''+cr+'  ',  3,5);
  test_caret('trim', 1,1, cr+'  ab'+cr+''+cr+''+cr+'', 1,1);
//#  test_undo_exp('-trim',      cr+'  ab'+cr+''+cr+''+cr+'  ', 3,5);
  test_undo_exp('-newlin 1', cr+'  ab'+cr+''+cr+'  ',       3,4);
  test_undo_exp('-newlin 2', cr+'  ab'+cr+'  ',             3,3);
  test_undo_exp('-newlin 3', cr+'  ab ',                    6,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);
  test_redo_exp('redo 3',    cr+'  ab'+cr+'  ', 3,3);
  test_redo_exp('redo 2',    cr+'  ab'+cr+''+cr+'  ', 3,4);
  test_redo_exp('redo 1',    cr+'  ab'+cr+''+cr+''+cr+'  ', 3,5);
  test_undo_exp('-newlin 1(r)',  cr+'  ab'+cr+''+cr+'  ',       3,4);
  test_undo_exp('-newlin 2(r)',  cr+'  ab'+cr+'  ',             3,3);
  test_undo_exp('-newlin 3(r)',  cr+'  ab ',                    6,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);

  (* *)
  test_start('insert newline+indent before space', cr+'  ab ');
  test_cmd  ('insert 5,2', 5,2, ecLineBreak, ' ', cr+'  ab'+cr+'   ',              3,3);
  test_cmd  ('insert 3,3', 3,3, ecLineBreak, ' ', cr+'  ab'+cr+''+cr+'   ',        3,4);
  test_cmd  ('insert 3,4', 3,4, ecLineBreak, ' ', cr+'  ab'+cr+''+cr+''+cr+'   ',  3,5);
  test_caret('trim', 1,1, cr+'  ab'+cr+''+cr+''+cr+'', 1,1);
//#  test_undo_exp('-trim',      cr+'  ab'+cr+''+cr+''+cr+'   ', 4,5); // TODO caret at:   3,5);
  test_undo_exp('-newlin 1', cr+'  ab'+cr+''+cr+'   ',        3,4);
  test_undo_exp('-newlin 2', cr+'  ab'+cr+'   ',              3,3);
  test_undo_exp('-newlin 3', cr+'  ab ',                      5,2);
  test_caret('no trim', 1,1, cr+'  ab ',           1,1);
  test_redo_exp('redo 3',    cr+'  ab'+cr+'   ',       3,3);
  test_redo_exp('redo 2',    cr+'  ab'+cr+''+cr+'   ', 3,4);
  test_redo_exp('redo 1',    cr+'  ab'+cr+''+cr+''+cr+'   ', 3,5);
  test_undo_exp('-newlin 1(r)',  cr+'  ab'+cr+''+cr+'   ',       3,4);
  test_undo_exp('-newlin 2(r)',  cr+'  ab'+cr+'   ',             3,3);
  test_undo_exp('-newlin 3(r)',  cr+'  ab ',                     5,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);

  (* *)
  test_start('insert linebreak', cr+'ab 1234 ');
  test_cmd  ('insert 4,2', 4,2, ecInsertLine, ' ', cr+'ab '+cr+'1234',  4,2);
  test_caret('trim', 1,1, cr+'ab'+cr+'1234', 1,1);
//#  test_undo_exp('-trim', cr+'ab '+cr+'1234', 4,2);
  test_undo_exp('-linebreak', cr+'ab 1234 ', 4,2);
  test_caret('no trim', 1,1, cr+'ab 1234 ', 1,1);

  (* *)
  test_start('insert linebreak+indent', cr+'  ab ');
  test_cmd  ('insert1 6,2', 6,2, ecInsertLine, ' ', cr+'  ab '+cr+'',  6,2);
  test_cmd  ('insert2 6,2', 6,2, ecInsertLine, ' ', cr+'  ab '+cr+''+cr+'',  6,2);
  test_caret('trim', 1,1, cr+'  ab'+cr+''+cr+'', 1,1);
//#  test_undo_exp('-trim',      cr+'  ab '+cr+''+cr+'', 6,2);
  test_undo_exp('-linebreak 1', cr+'  ab '+cr+'',     6,2);
  test_undo_exp('-linebreak 2', cr+'  ab ',           6,2);
  test_caret('no trim', 1,1, cr+'  ab ',          1,1);
(*
  test_redo_exp('redo 2',    cr+'  ab '+cr+'',        6,2);
  test_redo_exp('redo 1',    cr+'  ab '+cr+''+cr+'',  6,2);
  test_undo_exp('-linebreak 1(r)',  cr+'  ab '+cr+'', 6,2);
  test_undo_exp('-linebreak 2(r)',  cr+'  ab ',        6,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);
*)

  (* *)
  test_start('insert linebreak+indent before space', cr+'  ab ');
  test_cmd  ('insert1 5,2', 5,2, ecInsertLine, ' ', cr+'  ab'+cr+'',  5,2);
  test_cmd  ('insert2 5,2', 5,2, ecInsertLine, ' ', cr+'  ab'+cr+''+cr+'',  5,2);
  test_caret('no trim (0 spaces)', 1,1, cr+'  ab'+cr+''+cr+'', 1,1);
  test_undo_exp('-linebreak 1', cr+'  ab'+cr+'',     5,2);
  test_undo_exp('-linebreak 2', cr+'  ab ',          5,2);
  test_caret('no trim', 1,1, cr+'  ab ',         1,1);
(*
  test_redo_exp('redo 2',    cr+'  ab'+cr+'',       5,2);
  test_redo_exp('redo 1',    cr+'  ab'+cr+''+cr+'', 5,2);
  test_undo_exp('-linebreak 1(r)',  cr+'  ab'+cr+'', 5,2);
  test_undo_exp('-linebreak 2(r)',  cr+'  ab ',      5,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);
*)

  { Join Lines }
  (* *)
  test_start('del',       cr+'ab '+cr+'12');
  test_cmd  ('del 4,2', 4,2, ecDeleteChar, ' ', cr+'ab 12',  4,2);
  test_undo_exp('-del',       cr+'ab '+cr+'12',     4,2);
  test_caret('trim', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo_exp('+del',       cr+'ab 12',     4,2);
  test_undo_exp('-del(2)',    cr+'ab '+cr+'12',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);

  (* *)
  test_start('del (both sp)',       cr+'ab '+cr+'12 ');
  test_cmd  ('del 4,2', 4,2, ecDeleteChar, ' ', cr+'ab 12 ',  4,2);
  test_undo_exp('-del',       cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim', 1,1, cr+'ab '+cr+'12 ', 1,1);
  test_redo_exp('+del',       cr+'ab 12 ',     4,2);
  test_undo_exp('-del(2)',    cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12 ', 1,1);

  (* *)
  test_start('del past eol',       cr+'ab '+cr+'12');
  test_cmd  ('del 6,2', 6,2, ecDeleteChar, ' ', cr+'ab   12',  6,2);
  test_undo_exp('-del',        cr+'ab '+cr+'12',  6,2);
  test_caret('no trim', 1,1,  cr+'ab '+cr+'12',  1,1);
  test_caret('-no trim', 6,2, cr+'ab '+cr+'12',  6,2);
  test_redo_exp('+del(2)',       cr+'ab   12',     6,2);
  test_undo_exp('-del(2)',    cr+'ab '+cr+'12', 6,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo_exp('+del(3)',       cr+'ab   12',     6,2);  // redo with mis-place caret

  (* *)
  test_start('backspace', cr+'ab '+cr+'12');
  test_cmd  ('bs 1,3', 1,3, ecDeleteLastChar, ' ', cr+'ab 12',  4,2);
  test_undo_exp('-bs',    cr+'ab '+cr+'12',     1,3);
  test_caret('trim', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo_exp('+bs',    cr+'ab 12',     4,2);
  test_undo_exp('-bs(2)', cr+'ab '+cr+'12',     1,3);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);

  (* *)
  test_start('del-word',       cr+'ab '+cr+'12');
  test_cmd  ('del 4,2', 4,2, ecDeleteWord, ' ', cr+'ab 12',  4,2);
  test_undo_exp('-del',       cr+'ab '+cr+'12',     4,2);
  test_caret('no trim', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo_exp('+del',       cr+'ab 12',     4,2);
  test_undo_exp('-del(2)',    cr+'ab '+cr+'12',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);

  (* *)
  test_start('del-word (both sp)',       cr+'ab '+cr+'12 ');
  test_cmd  ('del 4,2', 4,2, ecDeleteWord, ' ', cr+'ab 12 ',  4,2);
(* xxxxxxxxxxxxxxx
  test_undo_exp('-del',       cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim', 1,1, cr+'ab '+cr+'12 ', 1,1);
  test_redo_exp('+del',       cr+'ab 12 ',     4,2);
  test_undo_exp('-del(2)',    cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12 ', 1,1);
*)

  (* *)
  test_start('del-word past eol',       cr+'ab '+cr+'12');
  test_cmd  ('del 6,2', 6,2, ecDeleteWord, ' ', cr+'ab   12',  6,2);
  test_undo_exp('-del',        cr+'ab '+cr+'12',  6,2);
  test_caret('trim', 1,1,  cr+'ab '+cr+'12',  1,1);
  test_caret('-trim', 6,2, cr+'ab '+cr+'12',  6,2);
  test_redo_exp('+del(2)',       cr+'ab   12',     6,2);
  test_undo_exp('-del(2)',    cr+'ab '+cr+'12', 6,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo_exp('+del(3)',       cr+'ab   12',     6,2);  // redo with mis-place caret

end;

procedure TTestTrimSpace.TrimUndoRedoEc;
begin
  FUseEc := True;
  TrimUndoRedo;
end;

procedure TTestTrimSpace.NoTrimUndoRedo;
begin
  SynEdit.Options := [eoAutoIndent, eoScrollPastEol]; // eoGroupUndo


  { Insert LineBreaks }
  (* *)
  test_start('insert newline', cr+'ab 1234 ');
  test_cmd  ('insert 4,2', 4,2, ecLineBreak, ' ', cr+'ab '+cr+'1234 ',  1,3);
  test_caret('trim', 1,1, cr+'ab '+cr+'1234 ', 1,1);
  test_undo_exp('-newline', cr+'ab 1234 ', 4,2);
  test_caret('no trim', 1,1, cr+'ab 1234 ', 1,1);

  (* *)
  test_start('insert newline+indent', cr+'  ab ');
  test_cmd  ('insert 6,2', 6,2, ecLineBreak, ' ', cr+'  ab '+cr+'  ',                  3,3);
  test_cmd  ('insert 3,3', 3,3, ecLineBreak, ' ', cr+'  ab '+cr+'  '+cr+'  ',          3,4);
  test_cmd  ('insert 3,4', 3,4, ecLineBreak, ' ', cr+'  ab '+cr+'  '+cr+'  '+cr+'  ',  3,5);
  test_caret('trim', 1,1, cr+'  ab '+cr+'  '+cr+'  '+cr+'  ', 1,1);
  test_undo_exp('-newlin 1', cr+'  ab '+cr+'  '+cr+'  ',       3,4);
  test_undo_exp('-newlin 2', cr+'  ab '+cr+'  ',               3,3);
  test_undo_exp('-newlin 3', cr+'  ab ',                      6,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);
  test_redo_exp('redo 3',    cr+'  ab '+cr+'  ',                 3,3);
  test_redo_exp('redo 2',    cr+'  ab '+cr+'  '+cr+'  ',         3,4);
  test_redo_exp('redo 1',    cr+'  ab '+cr+'  '+cr+'  '+cr+'  ', 3,5);
  test_undo_exp('-newlin 1(r)',  cr+'  ab '+cr+'  '+cr+'  ',     3,4);
  test_undo_exp('-newlin 2(r)',  cr+'  ab '+cr+'  ',             3,3);
  test_undo_exp('-newlin 3(r)',  cr+'  ab ',                    6,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);

(*
  (* *)
  test_start('insert newline+indent before space', cr+'  ab ');
  test_cmd  ('insert 5,2', 5,2, ecLineBreak, ' ', cr+'  ab'+cr+'   ',              3,3);
  test_cmd  ('insert 3,3', 3,3, ecLineBreak, ' ', cr+'  ab'+cr+''+cr+'   ',        3,4);
  test_cmd  ('insert 3,4', 3,4, ecLineBreak, ' ', cr+'  ab'+cr+''+cr+''+cr+'   ',  3,5);
  test_caret('trim', 1,1, cr+'  ab'+cr+''+cr+''+cr+'', 1,1);
  test_undo_exp('-trim',      cr+'  ab'+cr+''+cr+''+cr+'   ', 4,5); // TODO caret at:   3,5);
  test_undo_exp('-newlin 1', cr+'  ab'+cr+''+cr+'   ',        3,4);
  test_undo_exp('-newlin 2', cr+'  ab'+cr+'   ',              3,3);
  test_undo_exp('-newlin 3', cr+'  ab ',                      5,2);
  test_caret('no trim', 1,1, cr+'  ab ',           1,1);
  test_redo_exp('redo 3',    cr+'  ab'+cr+'   ',       3,3);
  test_redo_exp('redo 2',    cr+'  ab'+cr+''+cr+'   ', 3,4);
  test_redo_exp('redo 1',    cr+'  ab'+cr+''+cr+''+cr+'   ', 3,5);
  test_undo_exp('-newlin 1(r)',  cr+'  ab'+cr+''+cr+'   ',       3,4);
  test_undo_exp('-newlin 2(r)',  cr+'  ab'+cr+'   ',             3,3);
  test_undo_exp('-newlin 3(r)',  cr+'  ab ',                     5,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);


  (* *)
  test_start('insert linebreak', cr+'ab 1234 ');
  test_cmd  ('insert 4,2', 4,2, ecInsertLine, ' ', cr+'ab '+cr+'1234',  4,2);
  test_caret('trim', 1,1, cr+'ab'+cr+'1234', 1,1);
  test_undo_exp('-trim', cr+'ab '+cr+'1234', 4,2);
  test_undo_exp('-linebreak', cr+'ab 1234 ', 4,2);
  test_caret('no trim', 1,1, cr+'ab 1234 ', 1,1);

  (* *)
  test_start('insert linebreak+indent', cr+'  ab ');
  test_cmd  ('insert1 6,2', 6,2, ecInsertLine, ' ', cr+'  ab '+cr+'',  6,2);
  test_cmd  ('insert2 6,2', 6,2, ecInsertLine, ' ', cr+'  ab '+cr+''+cr+'',  6,2);
  test_caret('trim', 1,1, cr+'  ab'+cr+''+cr+'', 1,1);
  test_undo_exp('-trim',      cr+'  ab '+cr+''+cr+'', 6,2);
  test_undo_exp('-linebreak 1', cr+'  ab '+cr+'',     6,2);
  test_undo_exp('-linebreak 2', cr+'  ab ',           6,2);
  test_caret('no trim', 1,1, cr+'  ab ',          1,1);
(*
  test_redo_exp('redo 2',    cr+'  ab '+cr+'',        6,2);
  test_redo_exp('redo 1',    cr+'  ab '+cr+''+cr+'',  6,2);
  test_undo_exp('-linebreak 1(r)',  cr+'  ab '+cr+'', 6,2);
  test_undo_exp('-linebreak 2(r)',  cr+'  ab ',        6,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);
*)

  (* *)
  test_start('insert linebreak+indent before space', cr+'  ab ');
  test_cmd  ('insert1 5,2', 5,2, ecInsertLine, ' ', cr+'  ab'+cr+'',  5,2);
  test_cmd  ('insert2 5,2', 5,2, ecInsertLine, ' ', cr+'  ab'+cr+''+cr+'',  5,2);
  test_caret('no trim (0 spaces)', 1,1, cr+'  ab'+cr+''+cr+'', 1,1);
  test_undo_exp('-linebreak 1', cr+'  ab'+cr+'',     5,2);
  test_undo_exp('-linebreak 2', cr+'  ab ',          5,2);
  test_caret('no trim', 1,1, cr+'  ab ',         1,1);
(*
  test_redo_exp('redo 2',    cr+'  ab'+cr+'',       5,2);
  test_redo_exp('redo 1',    cr+'  ab'+cr+''+cr+'', 5,2);
  test_undo_exp('-linebreak 1(r)',  cr+'  ab'+cr+'', 5,2);
  test_undo_exp('-linebreak 2(r)',  cr+'  ab ',      5,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);
*)
*)

  { Join Lines }
  (* *)
  test_start('del',       cr+'ab '+cr+'12');
  test_cmd  ('del 4,2', 4,2, ecDeleteChar, ' ', cr+'ab 12',  4,2);
  test_undo_exp('-del',       cr+'ab '+cr+'12',     4,2);
  test_caret('trim', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo_exp('+del',       cr+'ab 12',     4,2);
  test_undo_exp('-del(2)',    cr+'ab '+cr+'12',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);

  (* *)
  test_start('del (both sp)',       cr+'ab '+cr+'12 ');
  test_cmd  ('del 4,2', 4,2, ecDeleteChar, ' ', cr+'ab 12 ',  4,2);
  test_undo_exp('-del',       cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim', 1,1, cr+'ab '+cr+'12 ', 1,1);
  test_redo_exp('+del',       cr+'ab 12 ',     4,2);
  test_undo_exp('-del(2)',    cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12 ', 1,1);

  (* *)
  test_start('del past eol',       cr+'ab '+cr+'12');
  test_cmd  ('del 6,2', 6,2, ecDeleteChar, ' ', cr+'ab   12',  6,2);
  test_undo_exp('-del',        cr+'ab '+cr+'12',  6,2);
  test_caret('trim', 1,1,  cr+'ab '+cr+'12',  1,1);
  test_caret('-trim', 6,2, cr+'ab '+cr+'12',  6,2);
(*
  test_redo_exp('+del(2)',       cr+'ab   12',     6,2);
  test_undo_exp('-del(2)',    cr+'ab '+cr+'12', 6,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo_exp('+del(3)',       cr+'ab   12',     6,2);  // redo with mis-place caret
*)

  (* *)
  test_start('backspace', cr+'ab '+cr+'12');
  test_cmd  ('bs 1,3', 1,3, ecDeleteLastChar, ' ', cr+'ab 12',  4,2);
  test_undo_exp('-bs',    cr+'ab '+cr+'12',     1,3);
  test_caret('trim', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo_exp('+bs',    cr+'ab 12',     4,2);
  test_undo_exp('-bs(2)', cr+'ab '+cr+'12',     1,3);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);

  (* *)
  test_start('del-word',       cr+'ab '+cr+'12');
  test_cmd  ('del 4,2', 4,2, ecDeleteWord, ' ', cr+'ab 12',  4,2);
  test_undo_exp('-del',       cr+'ab '+cr+'12',     4,2);
  test_caret('trim', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo_exp('+del',       cr+'ab 12',     4,2);
  test_undo_exp('-del(2)',    cr+'ab '+cr+'12',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);

  (* *)
  test_start('del-word (both sp)',       cr+'ab '+cr+'12 ');
  test_cmd  ('del 4,2', 4,2, ecDeleteWord, ' ', cr+'ab 12 ',  4,2);
  test_undo_exp('-del',       cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim', 1,1, cr+'ab '+cr+'12 ', 1,1);
  test_redo_exp('+del',       cr+'ab 12 ',     4,2);
  test_undo_exp('-del(2)',    cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12 ', 1,1);

  (* *)
  test_start('del-word past eol',       cr+'ab '+cr+'12');
  test_cmd  ('del 6,2', 6,2, ecDeleteWord, ' ', cr+'ab   12',  6,2);
  test_undo_exp('-del',        cr+'ab '+cr+'12',  6,2);
  test_caret('trim', 1,1,  cr+'ab '+cr+'12',  1,1);
  test_caret('-trim', 6,2, cr+'ab '+cr+'12',  6,2);
  test_redo_exp('+del(2)',       cr+'ab   12',     6,2);
  test_undo_exp('-del(2)',    cr+'ab '+cr+'12', 6,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo_exp('+del(3)',       cr+'ab   12',     6,2);  // redo with mis-place caret

end;

procedure TTestTrimSpace.TestInUndoBlock;
type TUpdateMode = (umNone, umOuter, umInner);
var UpdateMode: TUpdateMode;

  procedure BeginUndoBlock;
  begin
    if UpdateMode = umOuter then SynEdit.BeginUpdate;
    SynEdit.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('test'){$ENDIF};
    if UpdateMode = umInner then SynEdit.BeginUpdate;
  end;

  procedure EndUndoBlock;
  begin
    if UpdateMode = umInner then SynEdit.EndUpdate;
    SynEdit.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('test'){$ENDIF};
    if UpdateMode = umOuter then SynEdit.EndUpdate;
  end;

  procedure DoTestInUndoBlock;
  begin
    ReCreateEdit;
    SynEdit.Options := [eoTrimTrailingSpaces, eoAutoIndent, eoScrollPastEol]; // eoGroupUndo
    SynEdit.TrimSpaceType := settLeaveLine;
    SetLines(['abc d', 'mno', 'xyz', '']);
    SetCaret(1,1);
    // need to add space later, so it is regocnized as trailing

    BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('test'){$ENDIF};
      SynEdit.TextBetweenPointsEx[point(5,1), point(6,1), scamEnd] := ''; // delete d
      SynEdit.TextBetweenPointsEx[point(4,2), point(4,2), scamEnd] := ' '; // add space
    EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('test'){$ENDIF};
    TestIsFullText ('modified after block', ['abc', 'mno ', 'xyz', '']);
    TestIsCaret('modified after block', 5,2);

    SynEdit.Undo;
    TestIsFullText ('Undone', ['abc d', 'mno', 'xyz', '']);
    TestIsCaret('UnDone', 1,1);

    SynEdit.Redo;
    TestIsFullText ('Redone', ['abc', 'mno ', 'xyz', '']);
    TestIsCaret('Redone', 5,2);

    SynEdit.Undo;
    TestIsFullText ('Undone 2', ['abc d', 'mno', 'xyz', '']);
    TestIsCaret('UnDone 2', 1,1);

    SynEdit.Redo;
    TestIsFullText ('Redone 2', ['abc', 'mno ', 'xyz', '']);
    TestIsCaret('Redone 2', 5,2);

  end;

begin
  UpdateMode := umNone;
  PushBaseName('Without BeginUpdate');
  DoTestInUndoBlock;

  UpdateMode := umInner;
  PushBaseName('With BeginUpdate inside');
  DoTestInUndoBlock;

  UpdateMode := umOuter;
  PushBaseName('With BeginUpdate outside');
  DoTestInUndoBlock;

  PopBaseName;
end;

procedure TTestTrimSpace.TimmEdit;
var
  i: Integer;
begin
  ReCreateEdit;
  SynEdit.Options := [eoTrimTrailingSpaces, eoAutoIndent, eoScrollPastEol]; // eoGroupUndo
  SynEdit.TrimSpaceType := settLeaveLine;
  SetLines(['', '', '', 'xyz', '']);
  SetCaret(1,2);
  for i := 1 to 7 do
    SynEdit.CommandProcessor(ecChar, ' ', nil);
  SetCaret(3,2);
  SynEdit.CommandProcessor(ecChar, '/', nil);
  TestIsFullText ('spaces after edit', ['', '  /     ', '', 'xyz', '']);

end;



initialization

  RegisterTest(TTestTrimSpace);
end.

