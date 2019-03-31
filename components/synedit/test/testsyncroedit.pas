unit TestSyncroEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, testregistry, LCLProc, LCLType, Forms, TestBase,
  SynEdit, SynPluginSyncroEdit, SynEditKeyCmds, SynPluginTemplateEdit,
  SynPluginSyncronizedEditBase;

type

  PSynPluginCustomSyncroEdit = ^TSynPluginCustomSyncroEdit;

  TTestSynPluginSyncroEdit = class(TSynPluginSyncroEdit)
  public
    property Cells;
    property CurrentCell;
  end;
  TTestSynPluginTemplateEdit = class(TSynPluginTemplateEdit)
  public
    property Cells;
    property CurrentCell;
  end;

  TCellData = record
    PStart, PEnd: TPoint;
    Group: Integer;
  end;
  TCellsData = array of TCellData;

  function c(x1,y1, x2,y2: Integer; g: integer = -99): TCellData;
  function cs(const AData: array of TCellData): TCellsData;

type

  { TTestSyncroEdit }

  TTestSyncroEdit = class(TTestBase)
  protected
    FCurModule: PSynPluginCustomSyncroEdit;
    FSyncroModule: TTestSynPluginSyncroEdit;
    FTemplateModule: TTestSynPluginTemplateEdit;

    FDoUndo: Integer; // repeated undo/redo cycles
    FMaxUndoSteps: Integer; // steps of undo/redo in each cycle

    FExpectUndoneList: array of record
      SyncActive: Boolean;
      FullText: String;
      Caret: TPoint;
      CellData: TCellsData;
      CurrentCell: Integer;
      ExpCellData: TCellsData;
      ExpCurrentCell: Integer;
    end;

    FName: String;
    procedure Dump(hsh: TSynPluginSyncroEditWordsHash);
    procedure ReCreateEdit; reintroduce;
    procedure StartSyncroMode;

    function CurrentCell: Integer;
    function CopyCells: TCellsData;
    procedure CheckCells(AName: String; AExp: array of TCellData);
    procedure SetCarretAndCell(X, Y, C: Integer);


    procedure TestCmdAt(AName: String; X, Y: Integer; Cmd: TSynEditorCommand;
                         Exp: Array of String; SyncRunning: Boolean = True;
                         ExpCurCell: Integer = -99; ExpCells: TCellsData = nil);
    procedure TestKeyAt(AName: String; X, Y: Integer; Key: Word; Shift: TShiftState;
                         Exp: Array of String; SyncRunning: Boolean = True;
                         ExpCurCell: Integer = -99; ExpCells: TCellsData = nil);
    procedure TestKeyAtSel(AName: String; X, Y, X2, Y2: Integer; Key: Word; Shift: TShiftState;
                         Exp: Array of String; SyncRunning: Boolean = True;
                         ExpCurCell: Integer = -99; ExpCells: TCellsData = nil);

    procedure TestUndoRedo(const AName: String; const Exp: Array of String; SyncRunning: Boolean);

  published
    procedure WordsHash;
    (* SyncroEdit
       must NOT add any undo entries (class TSynPluginSyncronizedEditUndoCurrentCell)
       TODO: add checks...
       Until then, run test with breakpoint in TSynPluginSyncronizedEditUndoCurrentCell.create
    *)
    procedure SyncroEdit;
    procedure SyncroTemplateEdit;
  end;

implementation

function c(x1, y1, x2, y2: Integer; g: integer): TCellData;
begin
  Result.PStart := Point(X1, Y1);
  Result.PEnd := Point(X2, Y2);
  Result.Group := g;
end;

function cs(const AData: array of TCellData): TCellsData;
var
  i: Integer;
begin
  SetLength(Result, Length(AData));
  for i := 0 to Length(AData) - 1 do
    Result[i] := AData[i];
end;

procedure TTestSyncroEdit.Dump(hsh: TSynPluginSyncroEditWordsHash);
var
  he: TSynPluginSyncroEditWordsHashEntry;
  i, j: Integer;
begin
  debugln(['Dump ', hsh.HashSize]);
  for i := 0 to hsh.HashSize - 1 do begin
    he := hsh.HashEntry[i, 0];
    if he.Count > 0 then begin
      debugln(['hash for ', i,', ', 0, '   Cnt=', he.Count, ' hsh=', he.Hash, ' nxt=', he.Next, ' y=', he.LineIdx, ' x=', he.BytePos, ' ln=', he.Len]);
      j := 1;
      he := hsh.HashEntry[i, j];
      while he.Count > 0 do begin
        debugln(['  hash for ', i,', ', j, ' Cnt=', he.Count, ' hsh=', he.Hash, ' nxt=', he.Next, ' y=', he.LineIdx, ' x=', he.BytePos, ' ln=', he.Len]);
        inc(j);
        he := hsh.HashEntry[i, j];
      end;
    end;
  end;
end;

procedure TTestSyncroEdit.ReCreateEdit;
begin
  inherited;
if FCurModule = @FSyncroModule then
  FSyncroModule := TTestSynPluginSyncroEdit.Create(SynEdit) //;
else
  FTemplateModule := TTestSynPluginTemplateEdit.Create(SynEdit);
  FExpectUndoneList := nil;
end;

procedure TTestSyncroEdit.StartSyncroMode;
var
  i: Integer;
begin
  //DoKeyPress(VK_J, [ssCtrl]);
  SynEdit.CommandProcessor(ecSynPSyncroEdStart, '', nil);
  i := 5;
  while (i > 0) and (not FSyncroModule.Active) do begin
    dec(i);
    Application.ProcessMessages;
  end;
  AssertTrue('SyncroMode started', FSyncroModule.Active );
  Application.ProcessMessages;
end;

function TTestSyncroEdit.CurrentCell: Integer;
begin
  if not FCurModule^.Active then
    Result := -1
  else
  if FCurModule^ = FSyncroModule then
    Result := FSyncroModule.CurrentCell
  else
    Result := FTemplateModule.CurrentCell;
end;

function TTestSyncroEdit.CopyCells: TCellsData;
var
  cells: TSynPluginSyncronizedEditList;
  i, l: Integer;
begin
  Result := nil;
  if not FCurModule^.Active then
    exit;
  if FCurModule^ = FSyncroModule then
    cells := FSyncroModule.Cells
  else
    cells := FTemplateModule.Cells;

  l := cells.Count;
  if cells[l-1].Group = -2 then // ignore final caret
    dec(l);

  SetLength(Result, l);
  for i := 0 to l-1 do begin
    Result[i].PStart := cells[i].LogStart;
    Result[i].PEnd := cells[i].LogEnd;
    Result[i].Group := cells[i].Group;
  end;
end;

procedure TTestSyncroEdit.CheckCells(AName: String; AExp: array of TCellData);
var
  cells: TSynPluginSyncronizedEditList;
  i, l: Integer;
begin
  if FCurModule^ = FSyncroModule then
    cells := FSyncroModule.Cells
  else
    cells := FTemplateModule.Cells;

  l := cells.Count;
  if cells[l-1].Group = -2 then // ignore final caret
    dec(l);

  AssertEquals(AName+' Len', Length(AExp), l);
  for i := 0 to l-1 do begin
    AssertEquals(AName+' Cell:'+inttostr(i)+' S.x', AExp[i].PStart.x, cells[i].LogStart.x);
    AssertEquals(AName+' Cell:'+inttostr(i)+' S.y', AExp[i].PStart.y, cells[i].LogStart.y);
    AssertEquals(AName+' Cell:'+inttostr(i)+' E.x', AExp[i].PEnd.x, cells[i].LogEnd.x);
    AssertEquals(AName+' Cell:'+inttostr(i)+' E.y', AExp[i].PEnd.y, cells[i].LogEnd.y);
    if AExp[i].Group <> -99 then
      AssertEquals(AName+' Cell:'+inttostr(i)+' Grp', AExp[i].Group, cells[i].Group);
  end;
end;

procedure TTestSyncroEdit.SetCarretAndCell(X, Y, C: Integer);
begin
  SetCaret(x, y);
  if FCurModule^ = FSyncroModule then
    FSyncroModule.CurrentCell := c
  else
    FTemplateModule.CurrentCell := c;
end;

procedure TTestSyncroEdit.TestCmdAt(AName: String; X, Y: Integer;
  Cmd: TSynEditorCommand; Exp: array of String; SyncRunning: Boolean;
  ExpCurCell: Integer; ExpCells: TCellsData);
var
  t: String;
  l: Integer;
begin
  t := SynEdit.Text;
  SetCaret(x, y);

  l := Length(FExpectUndoneList);
  SetLength(FExpectUndoneList, l+1);
  FExpectUndoneList[l].SyncActive := FCurModule^.Active;
  FExpectUndoneList[l].FullText := t;
  FExpectUndoneList[l].Caret := SynEdit.CaretObj.LineBytePos;
  FExpectUndoneList[l].CellData := CopyCells;
  FExpectUndoneList[l].CurrentCell := CurrentCell;
  FExpectUndoneList[l].ExpCellData := ExpCells;
  FExpectUndoneList[l].ExpCurrentCell := ExpCurCell;

  SynEdit.CommandProcessor(Cmd, '', nil);
  TestIsFullText(Format('Cmd at (%d,%d) %s / %s', [X, Y, FName, AName]), Exp);
  AssertEquals(Format('Cmd at (%d,%d) %s / %s - Sync Running', [X, Y, FName, AName]), SyncRunning, FCurModule^.Active );

  if ExpCurCell <> -99 then
    AssertEquals(Format('Cmd at (%d,%d) %s / %s CurrentCell', [X, Y, FName, AName]), ExpCurCell, CurrentCell);
  if ExpCells <> nil then
    CheckCells(Format('Cmd at (%d,%d) %s / %s', [X, Y, FName, AName]), ExpCells);

  TestUndoRedo(Format('Cmd at (%d,%d) %s / %s ', [X, Y, FName, AName]), Exp, SyncRunning);
end;

procedure TTestSyncroEdit.TestKeyAt(AName: String; X, Y: Integer; Key: Word;
  Shift: TShiftState; Exp: array of String; SyncRunning: Boolean;
  ExpCurCell: Integer; ExpCells: TCellsData);
var
  t: String;
  i, UCnt, j, l: Integer;
begin
  t := SynEdit.TestFullText;
  SetCaret(x, y);

  l := Length(FExpectUndoneList);
  SetLength(FExpectUndoneList, l+1);
  FExpectUndoneList[l].SyncActive := FCurModule^.Active;
  FExpectUndoneList[l].FullText := t;
  FExpectUndoneList[l].Caret := SynEdit.CaretObj.LineBytePos;
  FExpectUndoneList[l].CellData := CopyCells;
  FExpectUndoneList[l].CurrentCell := CurrentCell;
  FExpectUndoneList[l].ExpCellData := ExpCells;
  FExpectUndoneList[l].ExpCurrentCell := ExpCurCell;

debugln(['!!!! key']);
  DoKeyPress(Key, Shift);
  TestIsFullText(Format('KeyPress at (%d,%d) %s / %s', [X, Y, FName, AName]), Exp);
  AssertEquals(Format('KeyPress at (%d,%d) %s / %s - Sync Running', [X, Y, FName, AName]), SyncRunning, FCurModule^.Active );

  if ExpCurCell <> -99 then
    AssertEquals(Format('KeyPress at (%d,%d) %s / %s CurrentCell', [X, Y, FName, AName]), ExpCurCell, CurrentCell);
  if ExpCells <> nil then
    CheckCells(Format('KeyPress at (%d,%d) %s / %s', [X, Y, FName, AName]), ExpCells);

  TestUndoRedo(Format('KeyPress at (%d,%d) %s / %s ', [X, Y, FName, AName]), Exp, SyncRunning);
debugln(['!!!! end']);
end;

procedure TTestSyncroEdit.TestKeyAtSel(AName: String; X, Y, X2, Y2: Integer;
  Key: Word; Shift: TShiftState; Exp: array of String; SyncRunning: Boolean;
  ExpCurCell: Integer; ExpCells: TCellsData);
var
  t: String;
  l: Integer;
begin
  t := SynEdit.Text;
  SetCaretAndSel(x, y, X2, Y2);

  l := Length(FExpectUndoneList);
  SetLength(FExpectUndoneList, l+1);
  FExpectUndoneList[l].SyncActive := FCurModule^.Active;
  FExpectUndoneList[l].FullText := t;
  FExpectUndoneList[l].Caret := SynEdit.CaretObj.LineBytePos;
  FExpectUndoneList[l].CellData := CopyCells;
  FExpectUndoneList[l].CurrentCell := CurrentCell;
  FExpectUndoneList[l].ExpCellData := ExpCells;
  FExpectUndoneList[l].ExpCurrentCell := ExpCurCell;

  DoKeyPress(Key, Shift);
  TestIsFullText(Format('KeyPress+Sel at (%d,%d) %s / %s', [X, Y, FName, AName]), Exp);
  AssertEquals(Format('KeyPress+Sel at (%d,%d) %s / %s - Sync Running', [X, Y, FName, AName]), SyncRunning, FCurModule^.Active );

  if ExpCurCell <> -99 then
    AssertEquals(Format('KeyPress+Sel at (%d,%d) %s / %s CurrentCell', [X, Y, FName, AName]), ExpCurCell, CurrentCell);
  if ExpCells <> nil then
    CheckCells(Format('KeyPress+Sel at (%d,%d) %s / %s', [X, Y, FName, AName]), ExpCells);

  TestUndoRedo(Format('KeyPress+Sel at (%d,%d) %s / %s ', [X, Y, FName, AName]), Exp, SyncRunning);
end;

procedure TTestSyncroEdit.TestUndoRedo(const AName: String; const Exp: array of String; SyncRunning: Boolean);
var
  i, j: Integer;
  UCnt, l: Integer;
begin
  l := Length(FExpectUndoneList);
  UCnt := min(l, Max(1, FMaxUndoSteps));

  for i := 1 to FDoUndo do begin
    for j := 1 to UCnt do begin
debugln(['!!!! undo',j]);
      SynEdit.Undo;
      if not FExpectUndoneList[l-j].SyncActive then
        SyncRunning := False; // undone gone before template activation, template will be off, even in redo

      TestIsFullText(Format('%s UNDONE %d', [AName, j]), FExpectUndoneList[l-j].FullText);
      TestIsCaret(Format('%s UNDONE %d', [AName, j]), FExpectUndoneList[l-j].Caret.x, FExpectUndoneList[l-j].Caret.y);
      if SyncRunning then begin // no cells, if expected to have gone inactive
        CheckCells(Format('%s UNDONE %d Cells', [AName, j]), FExpectUndoneList[l-j].CellData);
        AssertEquals(Format('%s UNDONE %d CurCell', [AName, j]), FExpectUndoneList[l-j].CurrentCell, CurrentCell);
      end;
    end;

    for j := UCnt downto 1 do begin
debugln(['!!!! redo',j]);
      SynEdit.Redo;
      if j > 1 then begin
        TestIsFullText(Format('%s UNDONE %d', [AName, j]), FExpectUndoneList[l+1-j].FullText);
        // caret may on other location / stored loc is set by next test
        if SyncRunning then
          CheckCells(Format('%s UNDONE %d Cells', [AName, j]), FExpectUndoneList[l+1-j].CellData);
      end;

      if SyncRunning then begin
        if FExpectUndoneList[l-j].ExpCurrentCell <> -99 then
          AssertEquals(Format('%s UNDONE %d CurrentCell', [AName, j]), FExpectUndoneList[l-j].ExpCurrentCell, CurrentCell);
        if FExpectUndoneList[l-j].ExpCellData <> nil then
          CheckCells(Format('%s UNDONE %d Cells', [AName, j]), FExpectUndoneList[l-j].ExpCellData);
      end;

    end;
    TestIsFullText(Format('%s REDONE', [AName]), Exp);

    AssertEquals(Format('%s - REDONE Sync Running', [AName]), SyncRunning, FCurModule^.Active );
  end;
end;

procedure TTestSyncroEdit.WordsHash;
var
  hsh: TSynPluginSyncroEditWordsHash;

  function Check(Msg, Wrd: String; Cnt: Integer): TSynPluginSyncroEditWordsHashEntry;
  begin
    Result:= hsh.GetWord(@Wrd[1], length(Wrd));
    AssertEquals(Msg + ' ' + Wrd + ' Cnt', Cnt, Result.Count);
    if Cnt > 0 then
      AssertEquals(Msg + ' ' + Wrd + ' Len', length(Wrd), Result.Len);
  end;
  procedure Add(Wrd: String; Y, X: Integer);
  begin
    hsh.AddWord(y, x, length(Wrd), @Wrd[1]);
    //Dump(hsh);
  end;
  procedure Del(Wrd: String);
  begin
    hsh.RemoveWord(length(Wrd), @Wrd[1]);
    //Dump(hsh);
  end;

var
  lwl: TSynPluginSyncroEditLowerLineCache;
  s: String;
  i: Integer;
begin
  lwl := TSynPluginSyncroEditLowerLineCache.Create;
  lwl.Lines := SynEdit.ViewedTextBuffer;
  try
    hsh := TSynPluginSyncroEditWordsHash.Create;
    hsh.LowerLines := lwl;

    SynEdit.Lines.Add('Test abc');
    SynEdit.Lines.Add('atdesktop before2 252'); // supposed to have the same hash on a 4096 table
    SynEdit.Lines.Add('mk_equal ecpageup'); // same big hash

    Add('test', 0, 1);  Check('one word', 'test', 1);
    Add('test', 0, 1);  Check('one word twice', 'test', 2);
    Del('test');        Check('one word one down again', 'test', 1);
    Del('test');        Check('one word gone', 'test', 0);

    s:= 'atdesktop before2 252';
    AssertEquals('clash for atdesktop before2', hsh.GetWordModHash(@s[1], 9), hsh.GetWordModHash(@s[11], 7));
    AssertEquals('clash for atdesktop 252',     hsh.GetWordModHash(@s[1], 9), hsh.GetWordModHash(@s[19], 3));
    (* repeat test, but with double entry*)
    // add word with bad pointer, so we can create a clash

    Add('atdesktop', 1, 1); s:='2W  1'; Check(s, 'atdesktop', 1); Check(s, 'before2', 0); Check(s, '252', 0);
    Add('before2', 1, 11);  s:='2W  2'; Check(s, 'atdesktop', 1); Check(s, 'before2', 1); Check(s, '252', 0);
    Del('atdesktop');       s:='2W  3'; Check(s, 'atdesktop', 0); Check(s, 'before2', 1); Check(s, '252', 0);
    Add('atdesktop', 1, 1); s:='2W  4'; Check(s, 'atdesktop', 1); Check(s, 'before2', 1); Check(s, '252', 0);
    Add('atdesktop', 1, 1); s:='2W  5'; Check(s, 'atdesktop', 2); Check(s, 'before2', 1); Check(s, '252', 0);
    Del('atdesktop');       s:='2W  6'; Check(s, 'atdesktop', 1); Check(s, 'before2', 1); Check(s, '252', 0);
    Add('before2', 1, 11);  s:='2W  7'; Check(s, 'atdesktop', 1); Check(s, 'before2', 2); Check(s, '252', 0);
    Del('before2');         s:='2W  8'; Check(s, 'atdesktop', 1); Check(s, 'before2', 1); Check(s, '252', 0);
    Add('before2', 1, 11);  s:='2W  9'; Check(s, 'atdesktop', 1); Check(s, 'before2', 2); Check(s, '252', 0);
    Add('atdesktop', 1, 1); s:='2W 10'; Check(s, 'atdesktop', 2); Check(s, 'before2', 2); Check(s, '252', 0);
    Add('atdesktop', 1, 1); s:='2W 11'; Check(s, 'atdesktop', 3); Check(s, 'before2', 2); Check(s, '252', 0);
    Add('252', 1, 19);      s:='2W 12'; Check(s, 'atdesktop', 3); Check(s, 'before2', 2); Check(s, '252', 1);
    Del('before2');         s:='2W 13'; Check(s, 'atdesktop', 3); Check(s, 'before2', 1); Check(s, '252', 1);
    Del('before2');         s:='2W 14'; Check(s, 'atdesktop', 3); Check(s, 'before2', 0); Check(s, '252', 1);
    Del('before2');         s:='2W 15'; Check(s, 'atdesktop', 3); Check(s, 'before2', 0); Check(s, '252', 1); // none to del
    Del('252');             s:='2W 16'; Check(s, 'atdesktop', 3); Check(s, 'before2', 0); Check(s, '252', 0);
    Del('252');             s:='2W 17'; Check(s, 'atdesktop', 3); Check(s, 'before2', 0); Check(s, '252', 0); // none to del
    Del('atdesktop');       s:='2W 18'; Check(s, 'atdesktop', 2); Check(s, 'before2', 0); Check(s, '252', 0);
    hsh.Clear;              s:='2W 19'; Check(s, 'atdesktop', 0); Check(s, 'before2', 0); Check(s, '252', 0);

    Add('mk_equal', 2, 1);  s:='3W  1'; Check(s, 'mk_equal', 1); Check(s, 'ecpageup', 0);
    Add('ecpageup', 2, 10); s:='3W  2';
    AssertEquals('same hash', Check(s, 'mk_equal', 1).Hash, Check(s, 'ecpageup', 1).Hash);

    Add('ecpageup', 2, 1);  s:='3W  3'; Check(s, 'mk_equal', 1); Check(s, 'ecpageup', 2);
    Add('mk_equal', 2, 1);  s:='3W  4'; Check(s, 'mk_equal', 2); Check(s, 'ecpageup', 2);
    Del('mk_equal');        s:='3W  5'; Check(s, 'mk_equal', 1); Check(s, 'ecpageup', 2);
    Del('mk_equal');        s:='3W  6'; Check(s, 'mk_equal', 0); Check(s, 'ecpageup', 2);
    Del('mk_equal');        s:='3W  7'; Check(s, 'mk_equal', 0); Check(s, 'ecpageup', 2);
    hsh.Clear;

    // resize test
    for i := 0 to 5000 do begin
  //if i = 2200 then  Dump(hsh);
      SynEdit.Lines.Add(IntToStr(i));
      Add(inttostr(i), 3+i, 1);
      if i mod 7 = 0 then
        Add(inttostr(i), 3+i, 1);
    end;
    for i := 0 to 5000 do
      if i mod 7 = 0 then
        Check(inttostr(i), inttostr(i), 2)
      else
        Check(inttostr(i), inttostr(i), 1);

  finally
    hsh.free;
    lwl.Free;
  end;
end;

procedure TTestSyncroEdit.SyncroEdit;

  procedure SetTestText(TextIdx: Integer = 0);
  begin
    case TextIdx of
      0: SetLines(['abc foo',     // 1
                   'foo abc xy',  // 2
                   '  abc foo',   // 3
                   '',            // 4
                   'foo'          // 5
                  ]);
      1: SetLines(['abc foo',     // 1
                   'foo abc xy',  // 2
                   '  foo foo abc',//4
                   '',            // 5
                   'foo'          // 6
                  ]);
    end;
  end;
  procedure InitTestText(X1, Y1, X2, Y2: Integer; TextIdx: Integer = 0);
  begin
    ReCreateEdit;
    SynEdit.Options := SynEdit.Options - [eoGroupUndo, eoSmartTabDelete, eoSmartTabs] + [eoAutoIndent];
    SetTestText(TextIdx);
    SetCaretAndSel(X1, Y1, X2, Y2);
    StartSyncroMode;
  end;

  procedure TestSyncro;
  begin
    (* Edit at start/end of cell (insert/delete [backspace/del] )
       Edit cell, delete all, type again / Cut,Paste / newline
       Edit selection at start/end/complete of cell (delete/replace)
       Edit Cells, with 2 synced cells on same line

       Insert, delete linebreak in cell (with/without auto-indent)
         - includes some trim space
         - include 2 cells on one line

       Edit just before/after cell, while caret in cell (backspace at start / del at end)
         - same with emptied cell
       Edit just before/after cell, while caret in cell (selection outside cell / replace, delete)
         - part in/ part out of cell

       External Edit outside cell
TODO   External Edit inside current/other cell (or while caret outside any cell)

TODO   Lines[x] := ''; Lines.Add(); should deactivate the syncroMode

       TODO: more trim-space
       TODO: Delete word / last word => restrain to cell
       TODO: Change trim spaces, so it does keep spaces after undo
             see note in tests
    *)


    {%region Edit at start/end of cell (insert/delete [backspace/del] ) }
      {%region 'Insert/KeyPress inside cell' }
        Fname := 'Insert/KeyPress inside cell';

    // Tests that add only one undo-able step / no need to run again
    if FMaxUndoSteps<=1 then begin
        InitTestText(1,2, 1,4);
        TestKeyAt('Insert at Cell FOO (1 of 2), pos=1', 1, 2, VK_M, [],
          ['abc foo',
           'mfoo abc xy',
           '  abc mfoo',
           '',
           'foo',
           '']);

        InitTestText(1,2, 1,4);
        TestKeyAt('insert at cell FOO (1 of 2), pos=end', 4, 2, VK_M, [],
          ['abc foo',
           'foom abc xy',
           '  abc foom',
           '',
           'foo',
           '']);

        InitTestText(1,2, 1,4);
        TestKeyAt('insert at cell FOO (2 of 2), pos=1', 7, 3, VK_M, [],
          ['abc foo',
           'mfoo abc xy',
           '  abc mfoo',
           '',
           'foo',
           '']);

        InitTestText(1,2, 1,4);
        TestKeyAt('insert at cell FOO (2 of 2), pos=end', 10, 3, VK_M, [],
          ['abc foo',
           'foom abc xy',
           '  abc foom',
           '',
           'foo',
           '']);

        InitTestText(1,1, 1,4);
        TestKeyAt('insert at cell FOO (2 of 3), pos=1', 1, 2, VK_M, [],
          ['abc mfoo',
           'mfoo abc xy',
           '  abc mfoo',
           '',
           'foo',
           '']);
    end; // Tests that add only one undo-able step / no need to run again



        InitTestText(1,1, 1,4);
        TestKeyAt('insert at cell FOO (2 of 3), pos=end', 4, 2, VK_M, [],
          ['abc foom',
           'foom abc xy',
           '  abc foom',
           '',
           'foo',
           '']);
        TestKeyAt('insert(continue) at cell FOO (2 of 3), pos=new-end', 5, 2, VK_X, [],
          ['abc foomx',
           'foomx abc xy',
           '  abc foomx',
           '',
           'foo',
           '']);
        TestKeyAt('insert(continue) at cell FOO (2 of 3), pos=1', 1, 2, VK_N, [],
          ['abc nfoomx',
           'nfoomx abc xy',
           '  abc nfoomx',
           '',
           'foo',
           '']);
        TestKeyAt('insert(continue) at cell ABC(was FOO) (2 of 3), pos=1', 8, 2, VK_D, [],
          ['dabc nfoomx',
           'nfoomx dabc xy',
           '  dabc nfoomx',
           '',
           'foo',
           '']);
        TestKeyAt('insert(continue) at cell ABC(was FOO) (2 of 3), pos=end', 12, 2, VK_E, [],
          ['dabce nfoomx',
           'nfoomx dabce xy',
           '  dabce nfoomx',
           '',
           'foo',
           '']);
        TestKeyAt('insert(continue) at cell ABC(was FOO) (1 of 3), pos=mid', 3, 1, VK_F, [],
          ['dafbce nfoomx',
           'nfoomx dafbce xy',
           '  dafbce nfoomx',
           '',
           'foo',
           '']);
      {%endregion}
      {%region 'Delete/KeyPress inside cell' }
        Fname := 'Delete/KeyPress inside cell';

    // Tests that add only one undo-able step / no need to run again
    if FMaxUndoSteps<=1 then begin
        InitTestText(1,2, 1,4);
        TestKeyAt('Delete at Cell FOO (1 of 2), pos=1', 1, 2, VK_DELETE, [],
          ['abc foo',
           'oo abc xy',
           '  abc oo',
           '',
           'foo',
           '']);

        InitTestText(1,2, 1,4);
        TestKeyAt('Backspace at Cell FOO (1 of 2), pos=1', 2, 2, VK_BACK, [],
          ['abc foo',
           'oo abc xy',
           '  abc oo',
           '',
           'foo',
           '']);

        InitTestText(1,2, 1,4);
        TestKeyAt('Delete at cell FOO (1 of 2), pos=end', 3, 2, VK_DELETE, [],
          ['abc foo',
           'fo abc xy',
           '  abc fo',
           '',
           'foo',
           '']);

        InitTestText(1,2, 1,4);
        TestKeyAt('Backspace at cell FOO (1 of 2), pos=end', 4, 2, VK_BACK, [],
          ['abc foo',
           'fo abc xy',
           '  abc fo',
           '',
           'foo',
           '']);


        InitTestText(1,1, 1,4);
        TestKeyAt('Delete at Cell FOO (3 of 3), pos=1', 7, 3, VK_DELETE, [],
          ['abc oo',
           'oo abc xy',
           '  abc oo',
           '',
           'foo',
           '']);
    end; // Tests that add only one undo-able step / no need to run again

        InitTestText(1,1, 1,4);
        TestKeyAt('Backspace at cell FOO (3 of 3), pos=end', 10, 3, VK_BACK, [],
          ['abc fo',
           'fo abc xy',
           '  abc fo',
           '',
           'foo',
           '']);
        TestKeyAt('Delete(continue) at cell FOO (1 of 3), pos=end', 5, 1, VK_DELETE, [],
          ['abc o',
           'o abc xy',
           '  abc o',
           '',
           'foo',
           '']);
        TestKeyAt('Delete(continue) at cell ABC(was FOO) (2 of 3), pos=1', 3, 2, VK_DELETE, [],
          ['bc o',
           'o bc xy',
           '  bc o',
           '',
           'foo',
           '']);
        TestKeyAt('Backspace(continue) at cell ABC(was FOO) (2 of 3), pos=end', 5, 2, VK_BACK, [],
          ['b o',
           'o b xy',
           '  b o',
           '',
           'foo',
           '']);
      {%endregion}
    {%endregion}

    {%region Edit cell, delete all, type again }
      Fname := 'DeleteAll-Retype/KeyPress inside cell';

      InitTestText(1,2, 1,4);
      TestKeyAt('Delete at Cell FOO (1 of 2), pos=1', 1, 2, VK_DELETE, [],
        ['abc foo',
         'oo abc xy',
         '  abc oo',
         '',
         'foo',
         '']);
      TestKeyAt('Delete at Cell OO (1 of 2), pos=1', 1, 2, VK_DELETE, [],
        ['abc foo',
         'o abc xy',
         '  abc o',
         '',
         'foo',
         '']);
      TestKeyAt('Delete at Cell O (1 of 2), pos=1', 1, 2, VK_DELETE, [],
        ['abc foo',
         ' abc xy',
         '  abc ',
         '',
         'foo',
         '']);
      TestKeyAt('Retype(del) at Cell FOO (1 of 2), pos=1', 1, 2, VK_X, [],
        ['abc foo',
         'x abc xy',
         '  abc x',
         '',
         'foo',
         '']);
      TestKeyAt('Delete at Cell X (1 of 2), pos=1', 1, 2, VK_DELETE, [],
        ['abc foo',
         ' abc xy',
         '  abc ',
         '',
         'foo',
         '']);
if (FDoUndo = 0) then begin // todo: fails with undo
(* loosing trailing space on undo
   This is due to "FTrimmedLinesView.ForceTrim;" in SynEdit.Undo
   Which in turn is there, because the trimming in undo, must be done while the UndoList is still locked....
*)
      TestKeyAt('Retype / Insert new line', 1, 2, VK_RETURN, [],
        ['abc foo',
         '',' abc xy',
         '  abc ','',
         '',
         'foo',
         '']);
end;

      InitTestText(1,2, 1,4);
      TestCmdAt('DeleteLastWord at Cell FOO (1 of 2), pos=end', 4, 2, ecDeleteLastWord,
        ['abc foo',
         ' abc xy',
         '  abc ',
         '',
         'foo',
         '']);
      TestKeyAt('Retype(bck-sp) at Cell FOO (1 of 2)', 1, 2, VK_X, [],
        ['abc foo',
         'x abc xy',
         '  abc x',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4);
      TestCmdAt('DeleteLastWord at Cell FOO (1 of 2), pos=end', 4, 2, ecDeleteLastWord,
        ['abc foo',
         ' abc xy',
         '  abc ',
         '',
         'foo',
         '']);
      TestKeyAt('Retype(bck-sp) at other Cell FOO (2 of 2)', 7, 3, VK_X, [],
        ['abc foo',
         'x abc xy',
         '  abc x',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4);
      TestKeyAtSel('Cut at Cell FOO (1 of 2), pos=1', 4,2, 1,2, VK_X, [ssCtrl],
        ['abc foo',
         ' abc xy',
         '  abc ',
         '',
         'foo',
         '']);
      TestKeyAt('paste at Cell FOO (1 of 2), pos=1', 1, 2, VK_V, [ssCtrl],
        ['abc foo',
         'foo abc xy',
         '  abc foo',
         '',
         'foo',
         '']);
    {%endregion}

    {%region Edit selection at start/end/complete of cell (delete/replace) }
      Fname := 'Delete/Selection';

    // Tests that add only one undo-able step / no need to run again
    if FMaxUndoSteps<=1 then begin
      InitTestText(1,2, 1,4);
      TestKeyAtSel('at start of cell', 3,2, 1,2, VK_DELETE, [],
        ['abc foo',
         'o abc xy',
         '  abc o',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4);
      TestKeyAtSel('at end of cell', 2,2, 4,2, VK_DELETE, [],
        ['abc foo',
         'f abc xy',
         '  abc f',
         '',
         'foo',
         '']);
    end; // Tests that add only one undo-able step / no need to run again

      InitTestText(1,2, 1,4);
      TestKeyAtSel('full cell', 4,2, 1,2, VK_DELETE, [],
        ['abc foo',
         ' abc xy',
         '  abc ',
         '',
         'foo',
         '']);
      TestKeyAt('full cell(continue, reinsert)', 1,2, VK_A, [],
        ['abc foo',
         'a abc xy',
         '  abc a',
         '',
         'foo',
         '']);

      Fname := 'Replace/Selection';
    // Tests that add only one undo-able step / no need to run again
    if FMaxUndoSteps<=1 then begin
      InitTestText(1,2, 1,4);
      TestKeyAtSel('at start of cell', 3,2, 1,2, VK_L, [],
        ['abc foo',
         'lo abc xy',
         '  abc lo',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4);
      TestKeyAtSel('at end of cell', 2,2, 4,2, VK_L, [],
        ['abc foo',
         'fl abc xy',
         '  abc fl',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4);
      TestKeyAtSel('full cell', 4,2, 1,2, VK_L, [],
        ['abc foo',
         'l abc xy',
         '  abc l',
         '',
         'foo',
         '']);
    end; // Tests that add only one undo-able step / no need to run again
    {%endregion}

    {%region Edit Edit Cells, with 2 synced cells on same line }
      Fname := 'Two cells on one line';

    // Tests that add only one undo-able step / no need to run again
    if FMaxUndoSteps<=1 then begin
      InitTestText(1,2, 1,4, 1);
      TestKeyAt('insert in 1st cell', 3, 3, VK_X, [],
        ['abc foo',
         'xfoo abc xy',
         '  xfoo xfoo abc',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4, 1);
      TestKeyAt('insert in 2nd cell', 7, 3, VK_Y, [],
        ['abc foo',
         'yfoo abc xy',
         '  yfoo yfoo abc',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4, 1);
      TestKeyAt('insert in 2nd cell (2)', 10, 3, VK_Y, [],
        ['abc foo',
         'fooy abc xy',
         '  fooy fooy abc',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4, 1);
      TestKeyAt('delete in 1st cell', 6, 3, VK_BACK, [],
        ['abc foo',
         'fo abc xy',
         '  fo fo abc',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4, 1);
      TestKeyAt('delete in 2nd cell', 7, 3, VK_DELETE, [],
        ['abc foo',
         'oo abc xy',
         '  oo oo abc',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4, 1);
      TestKeyAtSel('Sel-Replace in 1st cell', 4,3, 6,3, VK_M, [],
        ['abc foo',
         'fm abc xy',
         '  fm fm abc',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4, 1);
      TestKeyAtSel('Sel-Replace in 2nd cell', 7,3, 9,3, VK_N, [],
        ['abc foo',
         'no abc xy',
         '  no no abc',
         '',
         'foo',
         '']);
    end; // Tests that add only one undo-able step / no need to run again
    {%endregion}

    {%region Insert, delete linebreak in cell (with/without auto-indent) }
      FName := 'LineBreaks';
      InitTestText(1,2, 1,4);
      TestKeyAt('Return middle of FOO, no indent', 2, 2, VK_RETURN, [],
        ['abc foo',
         'f','oo abc xy',
         '  abc f','oo',
         '',
         'foo',
         '']);
      TestKeyAt('Return middle of FOO, no indent - continue 1', 2, 2, VK_A, [],
        ['abc foo',
         'fa','oo abc xy',
         '  abc fa','oo',
         '',
         'foo',
         '']);
      TestKeyAt('Return middle of FOO, no indent - continue 2', 1, 3, VK_B, [],
        ['abc foo',
         'fa','boo abc xy',
         '  abc fa','boo',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4);
      TestKeyAt('Return begin of FOO, no indent', 1, 2, VK_RETURN, [],
        ['abc foo',
         '','foo abc xy',
         '  abc ','foo',
         '',
         'foo',
         '']);
      TestKeyAt('Return begin of FOO, no indent - cotinue 1', 1, 3, VK_C, [],
        ['abc foo',
         '','cfoo abc xy',
         '  abc ','cfoo',
         '',
         'foo',
         '']);
//if FDoUndo = 0 then begin
// TODO: fails due to trim space
      TestKeyAt('Return begin of FOO, no indent - cotinue 2', 1, 2, VK_D, [],
        ['abc foo',
         'd','cfoo abc xy',
         '  abc d','cfoo',
         '',
         'foo',
         '']);
//end;

      InitTestText(1,2, 1,4);
      TestKeyAt('Return end of FOO, no indent', 4, 2, VK_RETURN, [],
        ['abc foo',
         'foo',' abc xy',
         '  abc foo','',
         '',
         'foo',
         '']);
      TestKeyAt('Return end of FOO, no indent - continue 1', 1, 5, VK_E, [],
        ['abc foo',
         'foo','e abc xy',
         '  abc foo','e',
         '',
         'foo',
         '']);
      TestKeyAt('Return end of FOO, no indent - continue 2', 7, 4, VK_A, [],
        ['abc foo',
         'afoo','e abc xy',
         '  abc afoo','e',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4);
      TestKeyAt('Return middle of ABC, with indent', 4, 3, VK_RETURN, [],
        ['abc foo',
         'foo a','  bc xy',
         '  a','  bc foo',
         '',
         'foo',
         '']);
      TestKeyAt('Return middle of ABC, with indent - continue 1', 5, 3, VK_A, [],
        ['abc foo',
         'foo a','  bca xy',
         '  a','  bca foo',
         '',
         'foo',
         '']);
      TestKeyAt('Return middle of ABC, with indent - continue 2', 6, 2, VK_B, [],
        ['abc foo',
         'foo ab','  bca xy',
         '  ab','  bca foo',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4);
      TestKeyAt('Return begin of ABC, with indent', 3, 3, VK_RETURN, [],
        ['abc foo',
         'foo ','  abc xy',
         '  ','  abc foo',
         '',
         'foo',
         '']);
      TestKeyAt('Return begin of ABC, with indent - continue 1', 5, 2, VK_C, [],
        ['abc foo',
         'foo c','  abc xy',
         '  c','  abc foo',
         '',
         'foo',
         '']);
      TestKeyAt('Return begin of ABC, with indent - continue 2', 6, 3, VK_D, [],
        ['abc foo',
         'foo c','  abcd xy',
         '  c','  abcd foo',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4);
      TestKeyAt('Return end of ABC, with indent', 6, 3, VK_RETURN, [],
        ['abc foo',
         'foo abc','   xy',
         '  abc','   foo',
         '',
         'foo',
         '']);
      TestKeyAt('Return end of ABC, with indent - continue 1', 6, 4, VK_E, [],
        ['abc foo',
         'foo abce','   xy',
         '  abce','   foo',
         '',
         'foo',
         '']);
      TestKeyAt('Return end of ABC, with indent - continue 2', 3, 5, VK_F, [],
        ['abc foo',
         'foo abce','  f xy',
         '  abce','  f foo',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4, 1);
      TestKeyAt('2 cells in one line: insert in 1st cell', 4, 3, VK_RETURN, [],
        ['abc foo',
         'f','  oo abc xy',
         '  f','  oo f','  oo abc',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4, 1);
      TestKeyAt('2 cells in one line: insert in 2nd cell', 8, 3, VK_RETURN, [],
        ['abc foo',
         'f','  oo abc xy',
         '  f','  oo f','  oo abc',
         '',
         'foo',
         '']);
    {%endregion}

    {%region Edit just before/after cell, while caret in cell (backspace at start / del at end) }
      Fname := 'Delete/KeyPress caret in cell, text out of cell';
    // Tests that add only one undo-able step / no need to run again
    if FMaxUndoSteps<=1 then begin
      InitTestText(1,2, 1,4);
      TestKeyAt('Delete after  Cell ABC', 8, 2, VK_DELETE, [],
        ['abc foo',
         'foo abcxy',
         '  abc foo',
         '',
         'foo',
         ''], FALSE);

      InitTestText(1,2, 1,4);
      TestKeyAt('Delete before Cell ABC', 5, 2, VK_BACK, [],
        ['abc foo',
         'fooabc xy',
         '  abc foo',
         '',
         'foo',
         ''], FALSE);

      InitTestText(1,2, 1,4);
      TestKeyAtSel('prepare empty cell', 5,2, 8,2, VK_BACK, [],
        ['abc foo',
         'foo  xy',
         '   foo',
         '',
         'foo',
         ''], TRUE);
      TestKeyAt('Backspace, start of empty cell', 5,2, VK_BACK, [],
        ['abc foo',
         'foo xy',
         '   foo',
         '',
         'foo',
         ''], FALSE);
    end; // Tests that add only one undo-able step / no need to run again

      InitTestText(1,2, 1,4);
      TestKeyAtSel('prepare empty cell', 5,2, 8,2, VK_BACK, [],
        ['abc foo',
         'foo  xy',
         '   foo',
         '',
         'foo',
         ''], TRUE);
      TestKeyAt('Delete, end of empty cell', 5,2, VK_DELETE, [],
        ['abc foo',
         'foo xy',
         '   foo',
         '',
         'foo',
         ''], FALSE);
    {%endregion}

    {%region Edit just before/after cell, while caret in cell (selection outside cell / replace, delete) }
      Fname := 'Delete/Selection: caret in cell, text out of cell';
    // Tests that add only one undo-able step / no need to run again
    if FMaxUndoSteps<=1 then begin
      InitTestText(1,2, 1,4);
      TestKeyAtSel('Delete-Sel, after cell', 9,2, 8,2, VK_DELETE, [],
        ['abc foo',
         'foo abcxy',
         '  abc foo',
         '',
         'foo',
         ''], FALSE);

      InitTestText(1,2, 1,4);
      TestKeyAtSel('Replace-Sel, after cell', 9,2, 8,2, VK_M, [],
        ['abc foo',
         'foo abcmxy',
         '  abc foo',
         '',
         'foo',
         ''], FALSE);

      InitTestText(1,2, 1,4);
      TestKeyAtSel('Delete-Sel, part-in,part-after cell', 9,2, 7,2, VK_DELETE, [],
        ['abc foo',
         'foo abxy',
         '  abc foo',
         '',
         'foo',
         ''], FALSE);

      InitTestText(1,2, 1,4);
      TestKeyAtSel('Delete-Sel, part-in,part-after cell (caret out of cell)', 7,2, 9,2, VK_DELETE, [],
        ['abc foo',
         'foo abxy',
         '  abc foo',
         '',
         'foo',
         ''], FALSE);

      InitTestText(1,2, 1,4);
      TestKeyAtSel('Delete-Sel, before cell', 4,2, 5,2, VK_DELETE, [],
        ['abc foo',
         'fooabc xy',
         '  abc foo',
         '',
         'foo',
         ''], FALSE);

      InitTestText(1,2, 1,4);
      TestKeyAtSel('Replace-Sel, before cell', 4,2, 5,2, VK_M, [],
        ['abc foo',
         'foomabc xy',
         '  abc foo',
         '',
         'foo',
         ''], FALSE);

      InitTestText(1,2, 1,4);
      TestKeyAtSel('Delete-Sel, part-in,part-before cell', 4,2, 6,2, VK_DELETE, [],
        ['abc foo',
         'foobc xy',
         '  abc foo',
         '',
         'foo',
         ''], FALSE);

      InitTestText(1,2, 1,4);
      TestKeyAtSel('Delete-Sel, part-in,part-before cell (caret out of cell)', 6,2, 4,2, VK_DELETE, [],
        ['abc foo',
         'foobc xy',
         '  abc foo',
         '',
         'foo',
         ''], FALSE);
    end; // Tests that add only one undo-able step / no need to run again
    {%endregion}


    //Edit just before/after cell, while caret in cell (selection outside cell / replace, delete)
    //  - same with emptied cell
    //  - part in/ part out of cell

    {%region External Edit outside cell }
      Fname := 'External Edit outside cell';
      InitTestText(1,2, 1,4);
      SetCaret(4,3); // into cell def
      FSyncroModule.IncExternalEditLock;
      SynEdit.TextBetweenPoints[Point(1,3), Point(2,3)] := 'a';
      FSyncroModule.DecExternalEditLock;
      TestKeyAt('with cell active', 3, 3, VK_B, [],
        ['abc foo',
         'foo babc xy',
         'a babc foo',
         '',
         'foo',
         '']);

      InitTestText(1,2, 1,4);
      SetCaret(1,3); // out of cell abc
      FSyncroModule.IncExternalEditLock;
      SynEdit.TextBetweenPoints[Point(1,3), Point(2,3)] := 'a';
      FSyncroModule.DecExternalEditLock;
      TestKeyAt('with cell not active', 3, 3, VK_B, [],
        ['abc foo',
         'foo babc xy',
         'a babc foo',
         '',
         'foo',
         '']);

(*
      InitTestText(1,2, 1,4);
      SetCaret(1,3); // out of cell abc
      FSyncroModule.IncExternalEditLock;
      SynEdit.TextBetweenPoints[Point(3,3), Point(3,3)] := 'a';
      FSyncroModule.DecExternalEditLock;
      TestKeyAt('with cell not active, extern in cell', 3, 3, VK_C, [],
        ['abc foo',
         'foo caabc xy',
         '  caabc foo',
         '',
         'foo',
         '']);
*)

    {%endregion}

    // Test UNDO, going back before template started
    ReCreateEdit;
    SynEdit.Options := SynEdit.Options - [eoGroupUndo, eoSmartTabDelete, eoSmartTabs] + [eoAutoIndent];
    SetTestText(0);
    TestKeyAt('key before active', 1, 1, VK_1, [],
      ['1abc foo',
       'foo abc xy',
       '  abc foo',
       '',
       'foo',
       ''],
      False);
    SetCaretAndSel(1,2, 1,4);
    StartSyncroMode;
    TestKeyAt('Insert at Cell FOO (1 of 2), pos=1', 1, 2, VK_M, [],
      ['1abc foo',
       'mfoo abc xy',
       '  abc mfoo',
       '',
       'foo',
       '']);


    //External Edit inside current/other cell (or while caret outside any cell)

    //Application.ProcessMessages;
    //Application.ProcessMessages; sleep(2500);

  end;

var
  m: Integer;
begin
  FCurModule := @FSyncroModule;
  FMaxUndoSteps := 1;

  FDoUndo := 0;
  PushBaseName('simple');
  TestSyncro;

  for m := 1 to 5 do begin
    FMaxUndoSteps := m;

    FDoUndo := 1;
    PopPushBaseName('undo/redo');
    TestSyncro;

    FDoUndo := 2;
    PopPushBaseName('undo/redo');
    TestSyncro;

    FDoUndo := 5;
    PopPushBaseName('undo/redo');
    TestSyncro;
  end;
end;

procedure TTestSyncroEdit.SyncroTemplateEdit;

  procedure SetTestText(TextIdx: Integer = 0);
  begin
    case TextIdx of
      0: SetLines(['abc foo',     // 1
                   'foo property Name: TType read GetName write SetName; xy',  // 2
                   'foo'          // 3
                  ]);
    end;
  end;
  procedure InitTestText(ACells: array of TCellData; TextIdx: Integer = 0);
  var
    Cells: TSynPluginSyncronizedEditList;
    i: Integer;
  begin
    if TextIdx >= 0 then begin
      ReCreateEdit;
      SynEdit.Options := SynEdit.Options - [eoGroupUndo, eoSmartTabDelete, eoSmartTabs] + [eoAutoIndent];
      SetTestText(TextIdx);
    end;

    Cells := TSynPluginSyncronizedEditList.Create;
    for i := 0 to Length(ACells) - 1 do begin
      with Cells.AddNew do begin
        Group    := ACells[i].Group;
        LogStart := ACells[i].PStart;
        LogEnd   := ACells[i].PEnd;
      end;
    end;

    FTemplateModule.CellParserEnabled := False;
    SynEdit.BlockBegin := Point(1, 1);
    FTemplateModule.SetTemplate('', Point(1,1));
    FTemplateModule.AddEditCells(Cells);
    Cells.Free;
  end;

  procedure TestTemplate;
  begin
// TODO: replace selection with new text (2 undo items at once)
    FName := 'Template';
    InitTestText([c(14,2, 18,2, 1),  c(20,2, 25,2, 2),  c(31,2, 34,2, 3),  c(34,2, 38,2, 1),  c(45,2, 48,2, 4),  c(48,2, 52,2, 1)]);
    TestKeyAt('Start of Cell 0  "Name"', 14,2, VK_1, [],
     ['abc foo',
      'foo property 1Name: TType read Get1Name write Set1Name; xy',
      'foo',
      ''], True,
      0,
      cs([c(14,2, 19,2, 1),  c(21,2, 26,2, 2),  c(32,2, 35,2, 3),  c(35,2, 40,2, 1),  c(47,2, 50,2, 4),  c(50,2, 55,2, 1)])
      );
    SetCarretAndCell(19,2, 0);
    TestKeyAt('End of Cell 0 "Name"', 19,2, VK_2, [],
     ['abc foo',
      'foo property 1Name2: TType read Get1Name2 write Set1Name2; xy',
      'foo',
      ''], True,
      0,
      cs([c(14,2, 20,2, 1),  c(22,2, 27,2, 2),  c(33,2, 36,2, 3),  c(36,2, 42,2, 1),  c(49,2, 52,2, 4),  c(52,2, 58,2, 1)])
      );
    SetCarretAndCell(36,2, 2);
    TestKeyAt('End of Cell 2 "Get"', 36,2, VK_3, [],
     ['abc foo',
      'foo property 1Name2: TType read Get31Name2 write Set1Name2; xy',
      'foo',
      ''], True,
      2,
      cs([c(14,2, 20,2, 1),  c(22,2, 27,2, 2),  c(33,2, 37,2, 3),  c(37,2, 43,2, 1),  c(50,2, 53,2, 4),  c(53,2, 59,2, 1)])
      );
    SetCarretAndCell(37,2, 3);
    TestKeyAt('Start of Cell 3 "Name" after "Get"', 37,2, VK_4, [],
     ['abc foo',
      'foo property 41Name2: TType read Get341Name2 write Set41Name2; xy',
      'foo',
      ''], True,
      3,
      cs([c(14,2, 21,2, 1),  c(23,2, 28,2, 2),  c(34,2, 38,2, 3),  c(38,2, 45,2, 1),  c(52,2, 55,2, 4),  c(55,2, 62,2, 1)])
      );
    SetCarretAndCell(55,2, 4);
    TestKeyAt('End of Cell 4 "Set"', 55,2, VK_5, [],
     ['abc foo',
      'foo property 41Name2: TType read Get341Name2 write Set541Name2; xy',
      'foo',
      ''], True,
      4
      );
    // replace selectino / add multiple undo steps (del + ins)
    SetCarretAndCell(56,2, 5);
    TestKeyAtSel('Cell 5 "Name"', 56,2, 63, 2, VK_X, [],
     ['abc foo',
      'foo property x: TType read Get3x write Set5x; xy',
      'foo',
      ''], True,
      5
      );
    SetCarretAndCell(44,2, 4);
    TestKeyAtSel('Cell 4 "Set"', 40,2, 44, 2, VK_Y, [],
     ['abc foo',
      'foo property x: TType read Get3x write yx; xy',
      'foo',
      ''], True,
      4
      );
    SetCarretAndCell(41,2, 5);
    TestKeyAtSel('Cell 5 "Name"', 41,2, 42, 2, VK_Z, [],
     ['abc foo',
      'foo property z: TType read Get3z write yz; xy',
      'foo',
      ''], True,
      5
      );
    // deal with empty cells
    SetCarretAndCell(40,2, 4);
    TestCmdAt('empty Cell 4 "Set" ', 40,2, ecDeleteChar,
     ['abc foo',
      'foo property z: TType read Get3z write z; xy',
      'foo',
      ''], True,
      4
      );
    SetCarretAndCell(40,2, 4);
    TestKeyAt('re-add Cell 4 "Set" ', 40,2, VK_A, [],
     ['abc foo',
      'foo property z: TType read Get3z write az; xy',
      'foo',
      ''], True,
      4
      );
    SetCarretAndCell(41,2, 5);
    TestCmdAt('empty Cell 5 "Name" ', 41,2, ecDeleteChar,
     ['abc foo',
      'foo property : TType read Get3 write a; xy',
      'foo',
      ''], True,
      5
      );
    SetCarretAndCell(38,2, 4);
    TestCmdAt('empty Cell 4 "Set" ', 38,2, ecDeleteChar,
     ['abc foo',
      'foo property : TType read Get3 write ; xy',
      'foo',
      ''], True,
      4
      );
    SetCarretAndCell(38,2, 5);
    TestKeyAt('re-add Cell 5 "Name"', 38,2, VK_C, [],
     ['abc foo',
      'foo property c: TType read Get3c write c; xy',
      'foo',
      ''], True,
      5
      );
    SetCarretAndCell(40,2, 4);
    TestKeyAt('re-add Cell 4 "Set"', 40,2, VK_D, [],
     ['abc foo',
      'foo property c: TType read Get3c write dc; xy',
      'foo',
      ''], True,
      4
      );


    InitTestText([c(14,2, 18,2, 1),  c(20,2, 25,2, 2),  c(31,2, 34,2, 3),  c(34,2, 38,2, 1),  c(45,2, 48,2, 4),  c(48,2, 52,2, 1)]);
    TestKeyAt('', 18,2, VK_1, [],
     ['abc foo',
      'foo property Name1: TType read GetName1 write SetName1; xy',
      'foo',
      '']);
    SetCarretAndCell(35,2, 2);
    TestCmdAt('del Cell 2 "Get" ', 35,2, ecDeleteLastChar,
     ['abc foo',
      'foo property Name1: TType read GeName1 write SetName1; xy',
      'foo',
      ''], True,
      2
      );
    SetCarretAndCell(34,2, 3);
    TestCmdAt('del Cell 3 "Name" ', 34,2, ecDeleteChar,
     ['abc foo',
      'foo property ame1: TType read Geame1 write Setame1; xy',
      'foo',
      ''], True,
      3
      );


    InitTestText([c(14,2, 18,2, 1),  c(20,2, 25,2, 2),  c(31,2, 34,2, 3),  c(34,2, 38,2, 1),  c(45,2, 48,2, 4),  c(48,2, 52,2, 1)]);
    TestKeyAt('', 34,2, VK_1, [],
     ['abc foo',
      'foo property Name: TType read Get1Name write SetName; xy',
      'foo',
      '']);

    // Test UNDO, going back before template started
    ReCreateEdit;
    SynEdit.Options := SynEdit.Options - [eoGroupUndo, eoSmartTabDelete, eoSmartTabs] + [eoAutoIndent];
    SetTestText(0);
    TestKeyAt('Key before template', 2,1, VK_9, [],
     ['a9bc foo',
      'foo property Name: TType read GetName write SetName; xy',
      'foo',
      ''], False);
    InitTestText([c(14,2, 18,2, 1),  c(20,2, 25,2, 2),  c(31,2, 34,2, 3),  c(34,2, 38,2, 1),  c(45,2, 48,2, 4),  c(48,2, 52,2, 1)], -1);
    TestKeyAt('Key in template', 34,2, VK_1, [],
     ['a9bc foo',
      'foo property Name: TType read Get1Name write SetName; xy',
      'foo',
      '']);


  end;

var
  m: Integer;
begin
  FCurModule := @FTemplateModule;
  FMaxUndoSteps := 1;

  FDoUndo := 0;
  PushBaseName('simple');
  TestTemplate;

  for m := 1 to 5 do begin
    FMaxUndoSteps := m;

    FDoUndo := 1;
    PopPushBaseName('undo/redo');
    TestTemplate;

    FDoUndo := 2;
    PopPushBaseName('undo/redo');
    TestTemplate;

    FDoUndo := 5;
    PopPushBaseName('undo/redo');
    TestTemplate;
  end;
end;

initialization

  RegisterTest(TTestSyncroEdit); 
end.

