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

  { TTestTrimSpace }

  TTestTrimSpace = class(TTestBase)
  private
    FTName: String;
    FUseEc: Boolean;
  protected
    procedure test_assert(ASubName: String; AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1);
    procedure test_start(AName, Txt: String);
    Procedure test_cmd(ASubName: String; X, Y: Integer; Cmd: TSynEditorCommand; AChar: TUTF8Char;
                       AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1);
    Procedure test_caret(ASubName: String; X, Y: Integer;
                       AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1);
    Procedure test_undo(ASubName: String;
                       AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1);
    Procedure test_redo(ASubName: String;
                       AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1);
    procedure SetUp; override;
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

procedure TTestTrimSpace.test_assert(ASubName: String; AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1);
begin
  if AExp <> '' then
    AssertEquals(FTName+' ('+ASubName+') Text', DbgStr(AExp+cr), DbgStr(SynEdit.ViewedTextBuffer.Text));
  if AExpX > 0 then begin
    AssertEquals(FTName+' ('+ASubName+') Caret X', AExpX, SynEdit.CaretX);
    AssertEquals(FTName+' ('+ASubName+') Caret Y', AExpY, SynEdit.CaretY);
  end;
//debugln(['done ',ASubName]);
end;

procedure TTestTrimSpace.test_start(AName, Txt: String);
begin
  FTName := AName;
  SynEdit.Text := Txt;
//debugln(['----- START ',AName]);
end;


Procedure TTestTrimSpace.test_cmd(ASubName: String; X, Y: Integer; Cmd: TSynEditorCommand; AChar: TUTF8Char;
                   AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1);
begin
  SynEdit.CaretXY := Point(x, y);
  SynEdit.CommandProcessor(Cmd, AChar, nil);
  test_assert('cmd: '+ASubName, AExp, AExpX, AExpY);
end;

Procedure TTestTrimSpace.test_caret(ASubName: String; X, Y: Integer;
                   AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1);
begin
  SynEdit.CaretXY := Point(x, y);
  test_assert('caret: '+ASubName, AExp, AExpX, AExpY);
end;

Procedure TTestTrimSpace.test_undo(ASubName: String;
                   AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1);
begin
  if FUseEc then
    SynEdit.CommandProcessor(ecUndo, ' ', nil)
  else
    SynEdit.Undo;
  test_assert('undo: '+ASubName, AExp, AExpX, AExpY);
end;

Procedure TTestTrimSpace.test_redo(ASubName: String;
                   AExp: String = ''; AExpX: Integer = -1; AExpY: Integer = -1);
begin
  if FUseEc then
    SynEdit.CommandProcessor(ecRedo, ' ', nil)
  else
    SynEdit.Redo;
  test_assert('redo: '+ASubName, AExp, AExpX, AExpY);
end;

procedure TTestTrimSpace.TrimUndoRedo;
begin
  SynEdit.Options := [eoTrimTrailingSpaces, eoAutoIndent, eoScrollPastEol]; // eoGroupUndo

  { Edit Lines }
  (* *)
  test_start('Delete leaves trimable space', cr+'abc d');
  test_cmd  ('Backspace 6,2', 6,2, ecDeleteLastChar, '', cr+'abc ', 5,2);
  test_caret('trim', 1,1, cr+'abc', 1,1);
  // undo
//#  test_undo('undo trim', cr+'abc ', 5,2);
  test_undo('undo bspace', cr+'abc d', 6,2);
  test_redo('redo bspace', cr+'abc ', 5,2);
  test_caret('trim', 1,1, cr+'abc', 1,1);
  // undo again
//#  test_undo('undo trim(2)', cr+'abc ', 5,2);
  test_undo('undo bspace(2)', cr+'abc d', 6,2);
  test_redo('redo bspace(2)', cr+'abc ', 5,2);
  test_redo('no redo trim(2)', cr+'abc ', 5,2);
  // undo again
  test_caret('trim(3)', 1,1, cr+'abc', 1,1);
//#  test_undo('undo trim(3)', cr+'abc ', 5,2);
//##  test_caret('trim(3)', 1,1, cr+'abc', 1,1);

  (* *)
  test_start('insert past real space', cr+'abc ');
  test_cmd  ('insert 5,2', 5,2, ecChar, 'x', cr+'abc x', 6,2);
  // undo
  test_undo('undo insert', cr+'abc ', 5,2);
  test_caret('no trim', 1,1, cr+'abc ', 1,1);
  // rdeo/undo again
  test_redo('redo insert', cr+'abc x', 6,2);
  test_undo('undo insert(2)', cr+'abc ', 5,2);
  test_caret('no trim(2)', 1,1, cr+'abc ', 1,1);

  (* *)
  test_start('insert spaces past real space', cr+'abc ');
  test_cmd  ('insert 5,2', 5,2, ecChar, ' ', cr+'abc  ', 6,2);
  test_cmd  ('insert 6,2', 6,2, ecChar, ' ', cr+'abc   ', 7,2);
  // undo
  test_undo('undo insert2', cr+'abc  ', 6,2);
  test_undo('undo insert1', cr+'abc ',  5,2);
  test_caret('no trim', 1,1, cr+'abc ', 1,1);
  // rdeo/undo again
  test_redo('redo insert1', cr+'abc  ',  6,2);
  test_redo('redo insert2', cr+'abc   ', 7,2);
  test_undo('undo inserts2(2)', cr+'abc  ', 6,2);
  test_undo('undo inserts1(2)', cr+'abc ',  5,2);
  test_caret('no trim(2)', 1,1, cr+'abc ', 1,1);


  (* *)
  test_start('insert spaces,char', cr+'abc');
  test_cmd  ('insert 4,2', 4,2, ecChar, ' ', cr+'abc ',  5,2);
  test_cmd  ('insert 5,2', 5,2, ecChar, 'x', cr+'abc x', 6,2);
  // undo
  test_undo('undo ins char', cr+'abc ', 5,2);
  test_caret('trim', 1,1, cr+'abc', 1,1); /// xxx this undo, remose the ability to redo....

  (* *)
  test_start('insert spaces,char after space', cr+'ab ');
  test_cmd  ('insert 4,2', 4,2, ecChar, ' ', cr+'ab  ',  5,2);
  test_cmd  ('insert 5,2', 5,2, ecChar, 'x', cr+'ab  x', 6,2);
  // undo
  test_undo('undo ins char', cr+'ab  ', 5,2);
  test_caret('trim', 1,1, cr+'ab', 1,1); /// xxx this undo, remose the ability to redo.... xxxxxxxxxxxxxxxx
  // We can no longer undo back to the real space

  (* *)
  test_start('insert spaces,char after space (2)', cr+'ab ');
  test_cmd  ('insert 4,2', 4,2, ecChar, ' ', cr+'ab  ',  5,2);
  test_cmd  ('insert 5,2', 5,2, ecChar, 'x', cr+'ab  x', 6,2);
  // undo
  test_undo('undo ins char', cr+'ab  ', 5,2);
  test_undo('undo ins space', cr+'ab ', 4,2);
  test_caret('no trim', 1,1, cr+'ab ', 1,1); /// xxx this undo, remose the ability to redo....

  (* *)
  test_start('insert past eol', cr+'abc ');
  test_cmd  ('insert 8,2', 8,2, ecChar, 'x', cr+'abc    x', 9,2);
  test_undo('undo insert', cr+'abc ', 8,2);
  test_caret('trim', 1,1, cr+'abc ', 1,1);
  // TODO: redo

  { Insert LineBreaks }
  (* *)
  test_start('insert newline', cr+'ab 1234 ');
  test_cmd  ('insert 4,2', 4,2, ecLineBreak, ' ', cr+'ab'+cr+'1234 ',  1,3);
  test_caret('trim', 1,1, cr+'ab'+cr+'1234', 1,1);
//#  test_undo('-trim', cr+'ab'+cr+'1234 ', 6,3);  // TODO caret at:   1,3);
  test_undo('-newline', cr+'ab 1234 ', 4,2);
  test_caret('no trim', 1,1, cr+'ab 1234 ', 1,1);

  (* *)
  test_start('insert newline+indent', cr+'  ab ');
  test_cmd  ('insert 6,2', 6,2, ecLineBreak, ' ', cr+'  ab'+cr+'  ',              3,3);
  test_cmd  ('insert 3,3', 3,3, ecLineBreak, ' ', cr+'  ab'+cr+''+cr+'  ',        3,4);
  test_cmd  ('insert 3,4', 3,4, ecLineBreak, ' ', cr+'  ab'+cr+''+cr+''+cr+'  ',  3,5);
  test_caret('trim', 1,1, cr+'  ab'+cr+''+cr+''+cr+'', 1,1);
//#  test_undo('-trim',      cr+'  ab'+cr+''+cr+''+cr+'  ', 3,5);
  test_undo('-newlin 1', cr+'  ab'+cr+''+cr+'  ',       3,4);
  test_undo('-newlin 2', cr+'  ab'+cr+'  ',             3,3);
  test_undo('-newlin 3', cr+'  ab ',                    6,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);
  test_redo('redo 3',    cr+'  ab'+cr+'  ', 3,3);
  test_redo('redo 2',    cr+'  ab'+cr+''+cr+'  ', 3,4);
  test_redo('redo 1',    cr+'  ab'+cr+''+cr+''+cr+'  ', 3,5);
  test_undo('-newlin 1(r)',  cr+'  ab'+cr+''+cr+'  ',       3,4);
  test_undo('-newlin 2(r)',  cr+'  ab'+cr+'  ',             3,3);
  test_undo('-newlin 3(r)',  cr+'  ab ',                    6,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);

  (* *)
  test_start('insert newline+indent before space', cr+'  ab ');
  test_cmd  ('insert 5,2', 5,2, ecLineBreak, ' ', cr+'  ab'+cr+'   ',              3,3);
  test_cmd  ('insert 3,3', 3,3, ecLineBreak, ' ', cr+'  ab'+cr+''+cr+'   ',        3,4);
  test_cmd  ('insert 3,4', 3,4, ecLineBreak, ' ', cr+'  ab'+cr+''+cr+''+cr+'   ',  3,5);
  test_caret('trim', 1,1, cr+'  ab'+cr+''+cr+''+cr+'', 1,1);
//#  test_undo('-trim',      cr+'  ab'+cr+''+cr+''+cr+'   ', 4,5); // TODO caret at:   3,5);
  test_undo('-newlin 1', cr+'  ab'+cr+''+cr+'   ',        3,4);
  test_undo('-newlin 2', cr+'  ab'+cr+'   ',              3,3);
  test_undo('-newlin 3', cr+'  ab ',                      5,2);
  test_caret('no trim', 1,1, cr+'  ab ',           1,1);
  test_redo('redo 3',    cr+'  ab'+cr+'   ',       3,3);
  test_redo('redo 2',    cr+'  ab'+cr+''+cr+'   ', 3,4);
  test_redo('redo 1',    cr+'  ab'+cr+''+cr+''+cr+'   ', 3,5);
  test_undo('-newlin 1(r)',  cr+'  ab'+cr+''+cr+'   ',       3,4);
  test_undo('-newlin 2(r)',  cr+'  ab'+cr+'   ',             3,3);
  test_undo('-newlin 3(r)',  cr+'  ab ',                     5,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);

  (* *)
  test_start('insert linebreak', cr+'ab 1234 ');
  test_cmd  ('insert 4,2', 4,2, ecInsertLine, ' ', cr+'ab '+cr+'1234',  4,2);
  test_caret('trim', 1,1, cr+'ab'+cr+'1234', 1,1);
//#  test_undo('-trim', cr+'ab '+cr+'1234', 4,2);
  test_undo('-linebreak', cr+'ab 1234 ', 4,2);
  test_caret('no trim', 1,1, cr+'ab 1234 ', 1,1);

  (* *)
  test_start('insert linebreak+indent', cr+'  ab ');
  test_cmd  ('insert1 6,2', 6,2, ecInsertLine, ' ', cr+'  ab '+cr+'',  6,2);
  test_cmd  ('insert2 6,2', 6,2, ecInsertLine, ' ', cr+'  ab '+cr+''+cr+'',  6,2);
  test_caret('trim', 1,1, cr+'  ab'+cr+''+cr+'', 1,1);
//#  test_undo('-trim',      cr+'  ab '+cr+''+cr+'', 6,2);
  test_undo('-linebreak 1', cr+'  ab '+cr+'',     6,2);
  test_undo('-linebreak 2', cr+'  ab ',           6,2);
  test_caret('no trim', 1,1, cr+'  ab ',          1,1);
(*
  test_redo('redo 2',    cr+'  ab '+cr+'',        6,2);
  test_redo('redo 1',    cr+'  ab '+cr+''+cr+'',  6,2);
  test_undo('-linebreak 1(r)',  cr+'  ab '+cr+'', 6,2);
  test_undo('-linebreak 2(r)',  cr+'  ab ',        6,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);
*)

  (* *)
  test_start('insert linebreak+indent before space', cr+'  ab ');
  test_cmd  ('insert1 5,2', 5,2, ecInsertLine, ' ', cr+'  ab'+cr+'',  5,2);
  test_cmd  ('insert2 5,2', 5,2, ecInsertLine, ' ', cr+'  ab'+cr+''+cr+'',  5,2);
  test_caret('no trim (0 spaces)', 1,1, cr+'  ab'+cr+''+cr+'', 1,1);
  test_undo('-linebreak 1', cr+'  ab'+cr+'',     5,2);
  test_undo('-linebreak 2', cr+'  ab ',          5,2);
  test_caret('no trim', 1,1, cr+'  ab ',         1,1);
(*
  test_redo('redo 2',    cr+'  ab'+cr+'',       5,2);
  test_redo('redo 1',    cr+'  ab'+cr+''+cr+'', 5,2);
  test_undo('-linebreak 1(r)',  cr+'  ab'+cr+'', 5,2);
  test_undo('-linebreak 2(r)',  cr+'  ab ',      5,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);
*)

  { Join Lines }
  (* *)
  test_start('del',       cr+'ab '+cr+'12');
  test_cmd  ('del 4,2', 4,2, ecDeleteChar, ' ', cr+'ab 12',  4,2);
  test_undo('-del',       cr+'ab '+cr+'12',     4,2);
  test_caret('trim', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo('+del',       cr+'ab 12',     4,2);
  test_undo('-del(2)',    cr+'ab '+cr+'12',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);

  (* *)
  test_start('del (both sp)',       cr+'ab '+cr+'12 ');
  test_cmd  ('del 4,2', 4,2, ecDeleteChar, ' ', cr+'ab 12 ',  4,2);
  test_undo('-del',       cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim', 1,1, cr+'ab '+cr+'12 ', 1,1);
  test_redo('+del',       cr+'ab 12 ',     4,2);
  test_undo('-del(2)',    cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12 ', 1,1);

  (* *)
  test_start('del past eol',       cr+'ab '+cr+'12');
  test_cmd  ('del 6,2', 6,2, ecDeleteChar, ' ', cr+'ab   12',  6,2);
  test_undo('-del',        cr+'ab '+cr+'12',  6,2);
  test_caret('no trim', 1,1,  cr+'ab '+cr+'12',  1,1);
  test_caret('-no trim', 6,2, cr+'ab '+cr+'12',  6,2);
  test_redo('+del(2)',       cr+'ab   12',     6,2);
  test_undo('-del(2)',    cr+'ab '+cr+'12', 6,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo('+del(3)',       cr+'ab   12',     6,2);  // redo with mis-place caret

  (* *)
  test_start('backspace', cr+'ab '+cr+'12');
  test_cmd  ('bs 1,3', 1,3, ecDeleteLastChar, ' ', cr+'ab 12',  4,2);
  test_undo('-bs',    cr+'ab '+cr+'12',     1,3);
  test_caret('trim', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo('+bs',    cr+'ab 12',     4,2);
  test_undo('-bs(2)', cr+'ab '+cr+'12',     1,3);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);

  (* *)
  test_start('del-word',       cr+'ab '+cr+'12');
  test_cmd  ('del 4,2', 4,2, ecDeleteWord, ' ', cr+'ab 12',  4,2);
  test_undo('-del',       cr+'ab '+cr+'12',     4,2);
  test_caret('no trim', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo('+del',       cr+'ab 12',     4,2);
  test_undo('-del(2)',    cr+'ab '+cr+'12',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);

  (* *)
  test_start('del-word (both sp)',       cr+'ab '+cr+'12 ');
  test_cmd  ('del 4,2', 4,2, ecDeleteWord, ' ', cr+'ab 12 ',  4,2);
(* xxxxxxxxxxxxxxx
  test_undo('-del',       cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim', 1,1, cr+'ab '+cr+'12 ', 1,1);
  test_redo('+del',       cr+'ab 12 ',     4,2);
  test_undo('-del(2)',    cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12 ', 1,1);
*)

  (* *)
  test_start('del-word past eol',       cr+'ab '+cr+'12');
  test_cmd  ('del 6,2', 6,2, ecDeleteWord, ' ', cr+'ab   12',  6,2);
  test_undo('-del',        cr+'ab '+cr+'12',  6,2);
  test_caret('trim', 1,1,  cr+'ab '+cr+'12',  1,1);
  test_caret('-trim', 6,2, cr+'ab '+cr+'12',  6,2);
  test_redo('+del(2)',       cr+'ab   12',     6,2);
  test_undo('-del(2)',    cr+'ab '+cr+'12', 6,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo('+del(3)',       cr+'ab   12',     6,2);  // redo with mis-place caret

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
  test_undo('-newline', cr+'ab 1234 ', 4,2);
  test_caret('no trim', 1,1, cr+'ab 1234 ', 1,1);

  (* *)
  test_start('insert newline+indent', cr+'  ab ');
  test_cmd  ('insert 6,2', 6,2, ecLineBreak, ' ', cr+'  ab '+cr+'  ',                  3,3);
  test_cmd  ('insert 3,3', 3,3, ecLineBreak, ' ', cr+'  ab '+cr+'  '+cr+'  ',          3,4);
  test_cmd  ('insert 3,4', 3,4, ecLineBreak, ' ', cr+'  ab '+cr+'  '+cr+'  '+cr+'  ',  3,5);
  test_caret('trim', 1,1, cr+'  ab '+cr+'  '+cr+'  '+cr+'  ', 1,1);
  test_undo('-newlin 1', cr+'  ab '+cr+'  '+cr+'  ',       3,4);
  test_undo('-newlin 2', cr+'  ab '+cr+'  ',               3,3);
  test_undo('-newlin 3', cr+'  ab ',                      6,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);
  test_redo('redo 3',    cr+'  ab '+cr+'  ',                 3,3);
  test_redo('redo 2',    cr+'  ab '+cr+'  '+cr+'  ',         3,4);
  test_redo('redo 1',    cr+'  ab '+cr+'  '+cr+'  '+cr+'  ', 3,5);
  test_undo('-newlin 1(r)',  cr+'  ab '+cr+'  '+cr+'  ',     3,4);
  test_undo('-newlin 2(r)',  cr+'  ab '+cr+'  ',             3,3);
  test_undo('-newlin 3(r)',  cr+'  ab ',                    6,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);

(*
  (* *)
  test_start('insert newline+indent before space', cr+'  ab ');
  test_cmd  ('insert 5,2', 5,2, ecLineBreak, ' ', cr+'  ab'+cr+'   ',              3,3);
  test_cmd  ('insert 3,3', 3,3, ecLineBreak, ' ', cr+'  ab'+cr+''+cr+'   ',        3,4);
  test_cmd  ('insert 3,4', 3,4, ecLineBreak, ' ', cr+'  ab'+cr+''+cr+''+cr+'   ',  3,5);
  test_caret('trim', 1,1, cr+'  ab'+cr+''+cr+''+cr+'', 1,1);
  test_undo('-trim',      cr+'  ab'+cr+''+cr+''+cr+'   ', 4,5); // TODO caret at:   3,5);
  test_undo('-newlin 1', cr+'  ab'+cr+''+cr+'   ',        3,4);
  test_undo('-newlin 2', cr+'  ab'+cr+'   ',              3,3);
  test_undo('-newlin 3', cr+'  ab ',                      5,2);
  test_caret('no trim', 1,1, cr+'  ab ',           1,1);
  test_redo('redo 3',    cr+'  ab'+cr+'   ',       3,3);
  test_redo('redo 2',    cr+'  ab'+cr+''+cr+'   ', 3,4);
  test_redo('redo 1',    cr+'  ab'+cr+''+cr+''+cr+'   ', 3,5);
  test_undo('-newlin 1(r)',  cr+'  ab'+cr+''+cr+'   ',       3,4);
  test_undo('-newlin 2(r)',  cr+'  ab'+cr+'   ',             3,3);
  test_undo('-newlin 3(r)',  cr+'  ab ',                     5,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);


  (* *)
  test_start('insert linebreak', cr+'ab 1234 ');
  test_cmd  ('insert 4,2', 4,2, ecInsertLine, ' ', cr+'ab '+cr+'1234',  4,2);
  test_caret('trim', 1,1, cr+'ab'+cr+'1234', 1,1);
  test_undo('-trim', cr+'ab '+cr+'1234', 4,2);
  test_undo('-linebreak', cr+'ab 1234 ', 4,2);
  test_caret('no trim', 1,1, cr+'ab 1234 ', 1,1);

  (* *)
  test_start('insert linebreak+indent', cr+'  ab ');
  test_cmd  ('insert1 6,2', 6,2, ecInsertLine, ' ', cr+'  ab '+cr+'',  6,2);
  test_cmd  ('insert2 6,2', 6,2, ecInsertLine, ' ', cr+'  ab '+cr+''+cr+'',  6,2);
  test_caret('trim', 1,1, cr+'  ab'+cr+''+cr+'', 1,1);
  test_undo('-trim',      cr+'  ab '+cr+''+cr+'', 6,2);
  test_undo('-linebreak 1', cr+'  ab '+cr+'',     6,2);
  test_undo('-linebreak 2', cr+'  ab ',           6,2);
  test_caret('no trim', 1,1, cr+'  ab ',          1,1);
(*
  test_redo('redo 2',    cr+'  ab '+cr+'',        6,2);
  test_redo('redo 1',    cr+'  ab '+cr+''+cr+'',  6,2);
  test_undo('-linebreak 1(r)',  cr+'  ab '+cr+'', 6,2);
  test_undo('-linebreak 2(r)',  cr+'  ab ',        6,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);
*)

  (* *)
  test_start('insert linebreak+indent before space', cr+'  ab ');
  test_cmd  ('insert1 5,2', 5,2, ecInsertLine, ' ', cr+'  ab'+cr+'',  5,2);
  test_cmd  ('insert2 5,2', 5,2, ecInsertLine, ' ', cr+'  ab'+cr+''+cr+'',  5,2);
  test_caret('no trim (0 spaces)', 1,1, cr+'  ab'+cr+''+cr+'', 1,1);
  test_undo('-linebreak 1', cr+'  ab'+cr+'',     5,2);
  test_undo('-linebreak 2', cr+'  ab ',          5,2);
  test_caret('no trim', 1,1, cr+'  ab ',         1,1);
(*
  test_redo('redo 2',    cr+'  ab'+cr+'',       5,2);
  test_redo('redo 1',    cr+'  ab'+cr+''+cr+'', 5,2);
  test_undo('-linebreak 1(r)',  cr+'  ab'+cr+'', 5,2);
  test_undo('-linebreak 2(r)',  cr+'  ab ',      5,2);
  test_caret('no trim', 1,1, cr+'  ab ', 1,1);
*)
*)

  { Join Lines }
  (* *)
  test_start('del',       cr+'ab '+cr+'12');
  test_cmd  ('del 4,2', 4,2, ecDeleteChar, ' ', cr+'ab 12',  4,2);
  test_undo('-del',       cr+'ab '+cr+'12',     4,2);
  test_caret('trim', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo('+del',       cr+'ab 12',     4,2);
  test_undo('-del(2)',    cr+'ab '+cr+'12',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);

  (* *)
  test_start('del (both sp)',       cr+'ab '+cr+'12 ');
  test_cmd  ('del 4,2', 4,2, ecDeleteChar, ' ', cr+'ab 12 ',  4,2);
  test_undo('-del',       cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim', 1,1, cr+'ab '+cr+'12 ', 1,1);
  test_redo('+del',       cr+'ab 12 ',     4,2);
  test_undo('-del(2)',    cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12 ', 1,1);

  (* *)
  test_start('del past eol',       cr+'ab '+cr+'12');
  test_cmd  ('del 6,2', 6,2, ecDeleteChar, ' ', cr+'ab   12',  6,2);
  test_undo('-del',        cr+'ab '+cr+'12',  6,2);
  test_caret('trim', 1,1,  cr+'ab '+cr+'12',  1,1);
  test_caret('-trim', 6,2, cr+'ab '+cr+'12',  6,2);
(*
  test_redo('+del(2)',       cr+'ab   12',     6,2);
  test_undo('-del(2)',    cr+'ab '+cr+'12', 6,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo('+del(3)',       cr+'ab   12',     6,2);  // redo with mis-place caret
*)

  (* *)
  test_start('backspace', cr+'ab '+cr+'12');
  test_cmd  ('bs 1,3', 1,3, ecDeleteLastChar, ' ', cr+'ab 12',  4,2);
  test_undo('-bs',    cr+'ab '+cr+'12',     1,3);
  test_caret('trim', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo('+bs',    cr+'ab 12',     4,2);
  test_undo('-bs(2)', cr+'ab '+cr+'12',     1,3);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);

  (* *)
  test_start('del-word',       cr+'ab '+cr+'12');
  test_cmd  ('del 4,2', 4,2, ecDeleteWord, ' ', cr+'ab 12',  4,2);
  test_undo('-del',       cr+'ab '+cr+'12',     4,2);
  test_caret('trim', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo('+del',       cr+'ab 12',     4,2);
  test_undo('-del(2)',    cr+'ab '+cr+'12',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);

  (* *)
  test_start('del-word (both sp)',       cr+'ab '+cr+'12 ');
  test_cmd  ('del 4,2', 4,2, ecDeleteWord, ' ', cr+'ab 12 ',  4,2);
  test_undo('-del',       cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim', 1,1, cr+'ab '+cr+'12 ', 1,1);
  test_redo('+del',       cr+'ab 12 ',     4,2);
  test_undo('-del(2)',    cr+'ab '+cr+'12 ',     4,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12 ', 1,1);

  (* *)
  test_start('del-word past eol',       cr+'ab '+cr+'12');
  test_cmd  ('del 6,2', 6,2, ecDeleteWord, ' ', cr+'ab   12',  6,2);
  test_undo('-del',        cr+'ab '+cr+'12',  6,2);
  test_caret('trim', 1,1,  cr+'ab '+cr+'12',  1,1);
  test_caret('-trim', 6,2, cr+'ab '+cr+'12',  6,2);
  test_redo('+del(2)',       cr+'ab   12',     6,2);
  test_undo('-del(2)',    cr+'ab '+cr+'12', 6,2);
  test_caret('trim(2)', 1,1, cr+'ab '+cr+'12', 1,1);
  test_redo('+del(3)',       cr+'ab   12',     6,2);  // redo with mis-place caret

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

