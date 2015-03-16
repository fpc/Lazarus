unit TestMultiCaret;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestBase, SynEditKeyCmds, SynPluginMultiCaret, SynEdit, Clipbrd, Forms,
  testregistry;

type

  TSynPluginMultiCaretTest = class(TSynPluginMultiCaret)
  public
    property Carets;
  end;

  { TTestMultiCaret }

  TTestMultiCaret = class(TTestBase)
  protected
    FMultiCaret, FMultiCaret2: TSynPluginMultiCaretTest;
    FOptAdd,  FOptRemove:  TSynEditorOptions;
    FOpt2Add, FOpt2Remove: TSynEditorOptions2;
    FEnableWithColumnSelection: Boolean;
    FDefaultMode: TSynPluginMultiCaretDefaultMode;
    FDefaultColumnSelectMode: TSynPluginMultiCaretDefaultMode;
    FSynEdit2: TTestSynEdit;

    procedure SetUp; override;
    procedure TearDown; override;


    procedure SetCaretAndColumnSelect(X, Y, Down, Right: Integer);
    procedure SetCaretsByKey(X, Y: Integer; CaretMoves: Array of Integer; EndMode: TSynPluginMultiCaretMode = mcmAddingCarets); //  [ {SET}, Right,DOwn, {SET}, Right,DOwn,..]
    procedure SetCaretsByKey(CaretMoves: Array of Integer; EndMode: TSynPluginMultiCaretMode = mcmAddingCarets); //  [ {SET}, Right,DOwn, {SET}, Right,DOwn,..]

    procedure TestExtraCaretCount(AName: String; ExpCount: Integer);
    procedure TestExtraCaretPos(AName: String; ExpCount: Integer; ExpPos: array of integer); // x,y
    procedure TestExtraCaretPosAndOffs(AName: String; ExpCount: Integer; ExpPos: array of integer); // x,y,offs

    procedure TestExtraCaretPos(AName: String; X, Y: Integer; ExpCount: Integer; ExpPos: array of integer); // x,y
    procedure TestExtraCaretPosAndOffs(AName: String; X, Y, Offs: Integer; ExpCount: Integer; ExpPos: array of integer); // x,y,offs

    procedure RunAndTest(AName: String;
        cmds: Array of TSynEditorCommand; chars: array of String;
        X, Y: Integer; ExpLines: Array of String
      );
    procedure RunAndTest(AName: String;
        cmds: Array of TSynEditorCommand; // no chars
        X, Y: Integer; ExpLines: Array of String
      );
    procedure RunAndTest(AName: String;
        cmds: Array of TSynEditorCommand; chars: array of String;
        X, Y, Offs: Integer; ExpLines: Array of String
      );
    procedure RunAndTest(AName: String;
        cmds: Array of TSynEditorCommand; // no chars
        X, Y, Offs: Integer; ExpLines: Array of String
      );


    procedure RunAndTest(AName: String;
        cmds: Array of TSynEditorCommand; chars: array of String;
        X, Y: Integer; ExpLines: Array of String;
        ExpCount: Integer; ExpPos: array of integer   // x, [offs,] y
      );
    procedure RunAndTest(AName: String;
        cmds: Array of TSynEditorCommand; // no chars
        X, Y: Integer; ExpLines: Array of String;
        ExpCount: Integer; ExpPos: array of integer   // x, [offs,] y
      );
    procedure RunAndTest(AName: String;
        cmds: Array of TSynEditorCommand; chars: array of String;
        X, Y, Offs: Integer; ExpLines: Array of String;
        ExpCount: Integer; ExpPos: array of integer   // x, [offs,] y
      );
    procedure RunAndTest(AName: String;
        cmds: Array of TSynEditorCommand; // no chars
        X, Y, Offs: Integer; ExpLines: Array of String;
        ExpCount: Integer; ExpPos: array of integer   // x, [offs,] y
      );

    // y1/2 1-based
    function DelCol(Lines: Array of String; y1,y2,X: Integer; Cnt: Integer = 1): TStringArray;
    // ADel: y,x,cnt,   y,x,cnt, .... 1-based
    function DelCol(Lines: Array of String; ADel: Array of Integer): TStringArray;

    function TestText1: TStringArray;
    function TestText1(DelY1, DelY2, DelX: Integer; DelCnt: Integer = 1): TStringArray;
    function TestText1(ADel: Array of Integer): TStringArray;
    function TestText2: TStringArray;
    function TestText2(DelY1, DelY2, DelX: Integer; DelCnt: Integer = 1): TStringArray;
    function TestText2(ADel: Array of Integer): TStringArray;
  public
    procedure ReCreateEdit; reintroduce;
    procedure ReCreateEdit(ALines: TStringArray);
    procedure ReCreateSharedEdit(Reverse: boolean = False);
    procedure SwapEdit; reintroduce;

    procedure RunCmdSeq(cmds: Array of TSynEditorCommand; chars: array of String);
  published
    procedure CaretList;
    procedure ColumnSelect;
    procedure CursorMove;
    procedure Edit;
    procedure Delete;
    procedure ReplaceColSel;
    procedure TabKey;
    procedure Paste;
    procedure Undo;
  end;

implementation

{ TTestMultiCaret }

procedure TTestMultiCaret.SetUp;
begin
  FOptAdd := [];
  FOptRemove := [];
  FOpt2Add := [];
  FOpt2Remove := [];
  FEnableWithColumnSelection := True;
  FDefaultMode := mcmMoveAllCarets;
  FDefaultColumnSelectMode := mcmCancelOnCaretMove;

  inherited SetUp;
end;

procedure TTestMultiCaret.TearDown;
begin
  FreeAndNil(FSynEdit2);
  inherited TearDown;
end;

procedure TTestMultiCaret.SetCaretAndColumnSelect(X, Y, Down, Right: Integer);
var
  i: Integer;
begin
  SetCaret(X, Y);
  if Down > 0 then
    for i := 1 to Down do
      RunCmdSeq([ecColSelDown], [])
  else
  if Down < 0 then
    for i := 1 to -Down do
      RunCmdSeq([ecColSelUp], []);

  if Right > 0 then
    for i := 1 to Right do
      RunCmdSeq([ecColSelRight], [])
  else
  if Right < 0 then
    for i := 1 to -Right do
      RunCmdSeq([ecColSelLeft], []);
end;

procedure TTestMultiCaret.SetCaretsByKey(X, Y: Integer; CaretMoves: array of Integer;
  EndMode: TSynPluginMultiCaretMode);
begin
  SetCaret(X, Y);
  SetCaretsByKey(CaretMoves, EndMode);
end;

procedure TTestMultiCaret.SetCaretsByKey(CaretMoves: array of Integer;
  EndMode: TSynPluginMultiCaretMode);
var
  i, j: Integer;
begin
  for i := 0 to (Length(CaretMoves) div 2) - 1 do begin
    RunCmdSeq([ecPluginMultiCaretSetCaret], []);
    if CaretMoves[i*2+1] > 0 then
      for j := 1 to CaretMoves[i*2+1] do
        RunCmdSeq([ecDown], [])
    else
    if CaretMoves[i*2+1] < 0 then
      for j := 1 to -CaretMoves[i*2+1] do
        RunCmdSeq([ecUp], []);

    if CaretMoves[i*2+0] > 0 then
      for j := 1 to CaretMoves[i*2+0] do
        RunCmdSeq([ecRight], [])
    else
    if CaretMoves[i*2+0] < 0 then
      for j := 1 to -CaretMoves[i*2+0] do
        RunCmdSeq([ecLeft], []);
  end;
  FMultiCaret.ActiveMode := EndMode;
end;

procedure TTestMultiCaret.TestExtraCaretCount(AName: String; ExpCount: Integer);
begin
  AssertEquals(BaseTestName+' '+AName + ' extra count', ExpCount, FMultiCaret.Carets.Count);
end;

procedure TTestMultiCaret.TestExtraCaretPos(AName: String; ExpCount: Integer;
  ExpPos: array of integer);
var
  i: Integer;
begin
  AssertEquals(BaseTestName+' '+AName + ' extra count', ExpCount, FMultiCaret.Carets.Count);
  AssertEquals(BaseTestName+' '+AName + 'selftest',length(ExpPos), ExpCount*2);
  for i := 0 to ExpCount - 1 do begin
    AssertEquals(BaseTestName+' '+AName + ' extra pos x', ExpPos[i*2+0], FMultiCaret.Carets.CaretX[i]);
    AssertEquals(BaseTestName+' '+AName + ' extra pos y', ExpPos[i*2+1], FMultiCaret.Carets.CaretY[i]);
  end
end;

procedure TTestMultiCaret.TestExtraCaretPosAndOffs(AName: String; ExpCount: Integer;
  ExpPos: array of integer);
var
  i: Integer;
begin
  AssertEquals(BaseTestName+' '+AName + ' extra count', ExpCount, FMultiCaret.Carets.Count);
  AssertEquals(BaseTestName+' '+AName + 'selftest',length(ExpPos), ExpCount*3);
  for i := 0 to ExpCount - 1 do begin
    AssertEquals(BaseTestName+' '+AName + ' extra pos x', ExpPos[i*3+0], FMultiCaret.Carets.CaretX[i]);
    AssertEquals(BaseTestName+' '+AName + ' extra pos y', ExpPos[i*3+1], FMultiCaret.Carets.CaretY[i]);
    AssertEquals(BaseTestName+' '+AName + ' extra pos O', ExpPos[i*3+2], FMultiCaret.Carets.CaretOffs[i]);
  end
end;

procedure TTestMultiCaret.TestExtraCaretPos(AName: String; X, Y: Integer; ExpCount: Integer;
  ExpPos: array of integer);
begin
  TestIsCaret(AName, X, Y);
  TestExtraCaretPos(AName, ExpCount, ExpPos);
end;

procedure TTestMultiCaret.TestExtraCaretPosAndOffs(AName: String; X, Y, Offs: Integer;
  ExpCount: Integer; ExpPos: array of integer);
begin
  TestIsCaret(AName, X, Y, Offs);
  TestExtraCaretPosAndOffs(AName, ExpCount, ExpPos);
end;

procedure TTestMultiCaret.RunAndTest(AName: String; cmds: array of TSynEditorCommand;
  chars: array of String; X, Y: Integer; ExpLines: array of String);
begin
  RunCmdSeq(cmds, chars);
  TestIsCaretLogAndFullText(AName, X, Y, ExpLines);
end;

procedure TTestMultiCaret.RunAndTest(AName: String; cmds: array of TSynEditorCommand; X,
  Y: Integer; ExpLines: array of String);
begin
  RunAndTest(AName, cmds, [], X, Y, ExpLines);
end;

procedure TTestMultiCaret.RunAndTest(AName: String; cmds: array of TSynEditorCommand;
  chars: array of String; X, Y, Offs: Integer; ExpLines: array of String);
begin
  RunCmdSeq(cmds, chars);
  TestIsCaretLogAndFullText(AName, X, Y, Offs, ExpLines);
end;

procedure TTestMultiCaret.RunAndTest(AName: String; cmds: array of TSynEditorCommand; X, Y,
  Offs: Integer; ExpLines: array of String);
begin
  RunAndTest(AName, cmds, [], X, Y, Offs, ExpLines);
end;

procedure TTestMultiCaret.RunAndTest(AName: String; cmds: array of TSynEditorCommand;
  chars: array of String; X, Y: Integer; ExpLines: array of String; ExpCount: Integer;
  ExpPos: array of integer);
begin
  RunAndTest(AName, cmds, chars, X, Y, 0, ExpLines, ExpCount, ExpPos);
end;

procedure TTestMultiCaret.RunAndTest(AName: String; cmds: array of TSynEditorCommand; X,
  Y: Integer; ExpLines: array of String; ExpCount: Integer; ExpPos: array of integer);
begin
  RunAndTest(AName, cmds, [], X, Y, ExpLines, ExpCount, ExpPos);
end;

procedure TTestMultiCaret.RunAndTest(AName: String; cmds: array of TSynEditorCommand;
  chars: array of String; X, Y, Offs: Integer; ExpLines: array of String; ExpCount: Integer;
  ExpPos: array of integer);
begin
  RunAndTest(AName, cmds, chars, X, Y, Offs, ExpLines);
  if length(ExpPos) = 0 then
    TestExtraCaretCount(AName, ExpCount)
  else
  if length(ExpPos) = ExpCount * 2 then
    TestExtraCaretPos(AName, ExpCount, ExpPos)
  else
  if length(ExpPos) = ExpCount * 3 then
    TestExtraCaretPosAndOffs(AName, ExpCount, ExpPos)
  else
    AssertTrue(BaseTestName+' '+AName + 'selftest CaretCOUNT <> pos-array-len', false);
end;

procedure TTestMultiCaret.RunAndTest(AName: String; cmds: array of TSynEditorCommand; X, Y,
  Offs: Integer; ExpLines: array of String; ExpCount: Integer; ExpPos: array of integer);
begin
  RunAndTest(AName, cmds, [], X, Y, Offs, ExpLines, ExpCount, ExpPos);
end;

function TTestMultiCaret.DelCol(Lines: array of String; y1, y2, X: Integer;
  Cnt: Integer): TStringArray;
var
  i: Integer;
begin
  SetLength(Result, length(Lines));
  for i := 0 to high(Lines) do
    Result[i] := Lines[i];
  for i := y1-1 to y2-1 do
    system.Delete(Result[i], X, Cnt);
end;

function TTestMultiCaret.DelCol(Lines: array of String; ADel: array of Integer): TStringArray;
var
  i: Integer;
begin
  SetLength(Result, length(Lines));
  for i := 0 to high(Lines) do
    Result[i] := Lines[i];
  for i := 0 to (length(ADel) div 3) - 1 do
    system.Delete(Result[ADel[i*3+0]-1], ADel[i*3+1], ADel[i*3+2]);
end;

function TTestMultiCaret.TestText1: TStringArray;
begin
  SetLength(Result, 8);
  Result[0] := '1abc def gh';
  Result[1] := '2mno pqr st';
  Result[2] := '3ABC DEF GH';
  Result[3] := '4MNO PQR ST';
  Result[4] := '5xyz klm op';
  Result[5] := '6aA bB cC dD';
  Result[6] := '7mM nN oO pP';
  Result[7] := '';
end;

function TTestMultiCaret.TestText1(DelY1, DelY2, DelX: Integer; DelCnt: Integer): TStringArray;
begin
  Result := DelCol(TestText1(), DelY1, DelY2, DelX, DelCnt);
end;

function TTestMultiCaret.TestText1(ADel: array of Integer): TStringArray;
begin
  Result := DelCol(TestText1(), ADel);
end;

function TTestMultiCaret.TestText2: TStringArray;
begin
  SetLength(Result, 18);
  Result[0] := '1abc def gh';
  Result[1] := '2mno pqr st';
  Result[2] := '1abc def gh Oo xx 99';
  Result[3] := '2äöü pqr st Oo xx 99';
  Result[4] := '3ÄÖÜ DEF GH Oo xx 99';
  Result[5] := '4MNä PQR ST Oo xx 99';
  Result[6] := '5xyÜ klm op';
  Result[7] := '6aA b'#9#9'B cC dD';
  Result[8] := '7mM n'#9#9'N oO pP';
  Result[9] := '6aA b'#9#9'B cC dD';
  Result[10] := '1abc アアウ gh Oo xx 99';
  Result[11] := '2mno pqr アウ Oo xx 99';
  Result[12] := '3アアウアアウ GH Oo xx 99';
  Result[13] := '4Mアアウアアウ ST Oo xx 99';
  Result[14] := '5xyz klm op bB cC dD';
  Result[15] := '6a'#9#9#9#9'A';
  Result[16] := '7mM nN oO pP';
  Result[17] := '';
end;

function TTestMultiCaret.TestText2(DelY1, DelY2, DelX: Integer; DelCnt: Integer): TStringArray;
begin
  Result := DelCol(TestText2(), DelY1, DelY2, DelX, DelCnt);
end;

function TTestMultiCaret.TestText2(ADel: array of Integer): TStringArray;
begin
  Result := DelCol(TestText2(), ADel);
end;

procedure TTestMultiCaret.ReCreateEdit;
begin
  inherited;
  FMultiCaret := TSynPluginMultiCaretTest.Create(SynEdit);

  SynEdit.Options  := SynEdit.Options  - FOptRemove  + FOptAdd;
  SynEdit.Options2 := SynEdit.Options2 - FOpt2Remove + FOpt2Add;

  FMultiCaret.EnableWithColumnSelection := FEnableWithColumnSelection;
  FMultiCaret.DefaultMode := FDefaultMode;
  FMultiCaret.DefaultColumnSelectMode := FDefaultColumnSelectMode;

  SynEdit.BlockIndent := 2;
  SynEdit.BlockTabIndent := 0;
  SynEdit.TabWidth := 4;
end;

procedure TTestMultiCaret.ReCreateEdit(ALines: TStringArray);
begin
  ReCreateEdit;
  SetLines(ALines);
end;

procedure TTestMultiCaret.ReCreateSharedEdit(Reverse: boolean);
begin
  FSynEdit2.Free;
  Form.Height := 600;
  Form.Width :=  500;

  FSynEdit2 := TTestSynEdit.Create(Form);
  FSynEdit2.Parent := Form;
  FSynEdit2.Top := 250;
  FSynEdit2.Left := 0;
  FSynEdit2.Width:= 500;
  FSynEdit2.Height := 250;

  FMultiCaret2 := TSynPluginMultiCaretTest.Create(FSynEdit2);

  if Reverse then begin
    FSynEdit2.Lines.Assign(FSynEdit.Lines);
    FSynEdit.ShareTextBufferFrom(FSynEdit2)
  end
  else
    FSynEdit2.ShareTextBufferFrom(FSynEdit);
end;

procedure TTestMultiCaret.SwapEdit;
var
  m: TSynPluginMultiCaretTest;
  e: TTestSynEdit;
begin
  m := FMultiCaret;
  e := FSynEdit;

  FMultiCaret := FMultiCaret2;
  FSynEdit := FSynEdit2;

  FMultiCaret2 := m;
  FSynEdit2 := e;
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
    Application.ProcessMessages;
  end;
end;

procedure TTestMultiCaret.CaretList;
  procedure TestSequence(name: string; a: Array of Integer);
  var
    c: TSynPluginMultiCaretList;
    i, j, k, n, m: Integer;
  begin

    c := TSynPluginMultiCaretList.Create;
    for i := 0 to high(a) do begin
      c.AddCaret(1,a[i],0);
      for j := 1 to c.Count-1 do
        AssertTrue(Format(name+' Test %d %d', [i, j]), c.Caret[j].y > c.Caret[j-1].y);
    end;

    c.Clear;
    for i := 0 to high(a) do begin
      k := c.AddCaret(1,a[i],0);
      AssertEquals(Format(name+' Test %d %d', [i, j]),a[i], c.Caret[k].y);
      for j := 1 to c.Count-1 do
        AssertTrue(Format(name+' Test %d %d', [i, j]), c.Caret[j].y > c.Caret[j-1].y);
    end;

    c.Clear;
    for i := 0 to high(a) do begin
      c.AddCaret(1,a[i],0);
    end;
    for j := 1 to c.Count-1 do
      AssertTrue(Format(name+' Test %d %d', [i, j]), c.Caret[j].y > c.Caret[j-1].y);

    c.Clear;
    for i := high(a) downto 0 do begin
      k := c.AddCaret(1,a[i],0);
      AssertEquals(Format(name+' Test %d %d', [i, j]),a[i], c.Caret[k].y);
      for j := 1 to c.Count-1 do
        AssertTrue(Format(name+' Test %d %d', [i, j]), c.Caret[j].y > c.Caret[j-1].y);
    end;


    for m := 0 to length(a)-1 do begin
      for n := 0 to m do begin
        c.Clear;
        for i := 0 to m do begin
          k := c.AddCaret(1,a[i],0);
          AssertEquals(Format(name+' Test %d %d', [i, j]),a[i], c.Caret[k].y);
        end;
        for j := 1 to c.Count-1 do
          AssertTrue(Format(name+' Test %d %d', [i, j]), c.Caret[j].y > c.Caret[j-1].y);
        k := c.Caret[n].y;
        c.RemoveCaret(n);
        for j := 1 to c.Count-1 do begin
          AssertTrue(Format(name+' Test %d %d', [i, j]), c.Caret[j].y > c.Caret[j-1].y);
          AssertTrue(Format(name+' Test %d %d', [i, j]), c.Caret[j].y <> k);
        end;


      end;
    end;

    c.Free;
  end;
  procedure TestSequenceEx(n: string; a: Array of Integer);
  var
    i, j: Integer;
    b: Array of Integer;
  begin
    for i := 1 to length(a) do begin
      TestSequence(n+IntToStr(i),a);
      j := a[0];
      if Length(a) > 1 then
        move(a[1],a[0],(Length(a)-1)*SizeOf(a[0]));
        a[high(a)] := j;
    end;

    SetLength(b, Length(a));
    for i := 0 to length(a)-1 do
      b[i] := a[high(a)-i];

    for i := 1 to length(b) do begin
      TestSequence(n+IntToStr(i),b);
      j := b[0];
      if Length(b) > 1 then
        move(b[1],b[0],(Length(b)-1)*SizeOf(b[0]));
        b[high(b)] := j;
    end;
  end;
begin
  TestSequence('XXX', [3,2,1,12,11,10,9,8,7,6,5,4]);
  TestSequence('XXX', [4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3]);

  TestSequenceEx('1', [1,2]);
  TestSequenceEx('1', [1,2,3,4,5,6,7,8,9,10,11,12]);
  TestSequenceEx('1', [1,99,2,98,3,97,4,96,5,95,6,94]);
  TestSequenceEx('1', [1,2,99,98,3,4,97,96,5,6,95,94,7,8,93,92,9,10]);
  TestSequenceEx('1', [1,2,3,4,5,6,7,8,9,10,11,12,-1]);
  TestSequenceEx('1', [1,2,3,4,5,6,7,8,9,10,-1]);
  TestSequenceEx('1', [1,2,3,4,5,6,7,8,9,-1]);
  TestSequenceEx('1', [1,2,3,4,5,6,7,8,-1]);
  TestSequenceEx('1', [1,2,3,4,5,6,7,-1]);
  TestSequenceEx('1', [1,2,3,4,5,6,-1]);
  TestSequenceEx('1', [1,2,3,4,5,-1]);
  TestSequenceEx('1', [1,2,3,4,-1]);
end;

procedure TTestMultiCaret.ColumnSelect;
begin
  PushBaseName('Simple  0 width col select ep/down');
  ReCreateEdit(TestText1);
  SetCaret(3,3);
  RunAndTest('', [ecColSelDown],   3,4, TestText1,   1, [3,3,0]);
  RunAndTest('', [ecColSelDown],   3,5, TestText1,   2, [3,3,0,  3,4,0]);
  RunAndTest('', [ecColSelUp],     3,4, TestText1,   1, [3,3,0]);
  RunAndTest('', [ecColSelUp],     3,3, TestText1,   0, []);
  RunAndTest('', [ecColSelUp],     3,2, TestText1,   1, [3,3,0]);
  RunAndTest('', [ecColSelDown],   3,3, TestText1,   0, []);
  RunAndTest('', [ecColSelDown],   3,4, TestText1,   1, [3,3,0]);
  RunAndTest('', [ecColSelDown],   3,5, TestText1,   2, [3,3,0,  3,4,0]);
  PopPushBaseName('column sel left/right');
  RunAndTest('', [ecColSelLeft],    2,5, TestText1,   2, [2,3,0,  2,4,0]);
  RunAndTest('', [ecColSelRight],   3,5, TestText1,   2, [3,3,0,  3,4,0]);
  RunAndTest('', [ecColSelRight],   4,5, TestText1,   2, [4,3,0,  4,4,0]);
  RunAndTest('', [ecColSelRight],   5,5, TestText1,   2, [5,3,0,  5,4,0]);
  PopPushBaseName('column sel, 2 width up/down');
  RunAndTest('', [ecColSelDown],    5,6, TestText1,   3, [5,3,0,  5,4,0,  5,5,0]);
  RunAndTest('', [ecColSelUp],      5,5, TestText1,   2, [5,3,0,  5,4,0]);
  RunAndTest('', [ecColSelUp],      5,4, TestText1,   1, [5,3,0]);
  PopBaseName;

  PushBaseName('double width char');
  FOptAdd := [eoKeepCaretX];
  ReCreateEdit(TestText2);
  // X at      log pos 4 / phys 4 / 3 chars before
  SetCaret(4,12);
  // X goes to log pos 5 / phys 4 / 2 chars => 1 double width char
  RunAndTest('', [ecColSelDown],   5,13, TestText2,   1, [4,12,0]);
  // X goes to log pos 3 / phys 3 / 2 chars => pushed forward by following dbl-w char
  RunAndTest('', [ecColSelDown],   3,14, TestText2,   2, [3,12,0,  2,13,0]); // 2,13 is pushed forward
  // X goes to log pos 4 / phys 4 => keepcaretX   // 1,14 is oout of line
  RunAndTest('', [ecColSelDown],   4,15, TestText2,   3, [4,12,0,  5,13,0,  3,14,0]);

  // X goes to log pos 3 / phys 3 / 2 chars => pushed forward by following dbl-w char
  RunAndTest('', [ecColSelUp],   3,14, TestText2,   2, [3,12,0,  2,13,0]);
  // X goes to log pos 5 / phys 4 / 2 chars => 1 double width char
  RunAndTest('', [ecColSelUp],   5,13, TestText2,   1, [4,12,0]);

  // X goes to log pos 3 / phys 3 / 2 chars => pushed forward by following dbl-w char
  RunAndTest('', [ecColSelDown],   3,14, TestText2,   2, [3,12,0,  2,13,0]); // 2,13 is pushed forward

end;

procedure TTestMultiCaret.CursorMove;
  function LocalText1: TStringArray;
  begin
    SetLength(Result, 4);
    Result[0] := '    123   ';
    Result[1] := '      abc ';
    Result[2] := '      abc ';
    Result[3] := '';
  end;
begin
  PushBaseName('eoScrollPastEol, eoCaretSkipTab');
    FOptAdd := [eoScrollPastEol];
    FOptRemove := [];
    FOpt2Add := [eoCaretSkipTab];
    FOpt2Remove := [];
    FDefaultColumnSelectMode := mcmMoveAllCarets;


    PushBaseName('ecUp');
      ReCreateEdit(TestText1);
      SetCaretAndColumnSelect(3,3, 1,0);
      RunAndTest('Height 2', [ecUp],   3,3, TestText1,   1, [3,2,0]);

      ReCreateEdit(TestText1);
      SetCaretAndColumnSelect(3,3, 2,0);
      RunAndTest('Height 3', [ecUp],   3,4, TestText1,   2, [3,2,0,  3,3,0]);

      ReCreateEdit(TestText1);
      SetCaretsByKey(3,3, [1,0, 1,0], mcmMoveAllCarets);
      RunAndTest('Width 3', [ecUp],   5,2, TestText1,   2, [3,2,0,  4,2,0]);
    PopBaseName;

    PushBaseName('ecUp');
      ReCreateEdit(TestText1);
      SetCaretAndColumnSelect(3,3, 1,0);
      RunAndTest('Height 2', [ecDown],   3,5, TestText1,   1, [3,4,0]);

      ReCreateEdit(TestText1);
      SetCaretAndColumnSelect(3,3, 2,0);
      RunAndTest('Height 3', [ecDown],   3,6, TestText1,   2, [3,4,0,  3,5,0]);

      ReCreateEdit(TestText1);
      SetCaretsByKey(3,3, [1,0, 1,0], mcmMoveAllCarets);
      RunAndTest('Width 3', [ecDown],   5,4, TestText1,   2, [3,4,0,  4,4,0]);
    PopBaseName;

    PushBaseName('ecLeft');
      ReCreateEdit(TestText1);
      SetCaretAndColumnSelect(3,3, 1,0);
      RunAndTest('Height 2', [ecLeft],   2,4, TestText1,   1, [2,3,0]);

      ReCreateEdit(TestText1);
      SetCaretAndColumnSelect(3,3, 2,0);
      RunAndTest('Height 3', [ecLeft],   2,5, TestText1,   2, [2,3,0,  2,4,0]);

      ReCreateEdit(TestText1);
      SetCaretsByKey(3,3, [1,0, 1,0], mcmMoveAllCarets);
      RunAndTest('Width 3', [ecLeft],   4,3, TestText1,   2, [2,3,0,  3,3,0]);
    PopBaseName;

    PushBaseName('ecRight');
      ReCreateEdit(TestText1);
      SetCaretAndColumnSelect(3,3, 1,0);
      RunAndTest('Height 2', [ecRight],   4,4, TestText1,   1, [4,3,0]);

      ReCreateEdit(TestText1);
      SetCaretAndColumnSelect(3,3, 2,0);
      RunAndTest('Height 3', [ecRight],   4,5, TestText1,   2, [4,3,0,  4,4,0]);

      ReCreateEdit(TestText1);
      SetCaretsByKey(3,3, [1,0, 1,0], mcmMoveAllCarets);
      RunAndTest('Width 3', [ecRight],   6,3, TestText1,   2, [4,3,0,  5,3,0]);
    PopBaseName;
  PopBaseName;

  PushBaseName('eoScrollPastEol, NO eoCaretSkipTab - move through tab');
    FOptAdd := [eoScrollPastEol];
    FOptRemove := [];
    FOpt2Add := [];
    FOpt2Remove := [eoCaretSkipTab];
    FDefaultColumnSelectMode := mcmMoveAllCarets;

    PushBaseName('ecRight');
      ReCreateEdit(TestText2); // tabw=4
      SetCaretAndColumnSelect(6,8, 2,0); // before both tabs
      RunAndTest('Height 3', [ecRight],   6,10,1, TestText2,   2, [6,8,1, 6,9,1]);
      RunAndTest('Height 3', [ecRight],   6,10,2, TestText2,   2, [6,8,2, 6,9,2]);
      RunAndTest('Height 3', [ecRight],   7,10,0, TestText2,   2, [7,8,0, 7,9,0]);
      RunAndTest('Height 3', [ecRight],   7,10,1, TestText2,   2, [7,8,1, 7,9,1]);
    PopBaseName;

    PushBaseName('ecLeft');
      ReCreateEdit(TestText2); // tabw=4
      SetCaretAndColumnSelect(8,8, 2,0); // after both tabs
      RunAndTest('Height 3', [ecLeft],   7,10,3, TestText2,   2, [7,8,3, 7,9,3]);
      RunAndTest('Height 3', [ecLeft],   7,10,2, TestText2,   2, [7,8,2, 7,9,2]);
      RunAndTest('Height 3', [ecLeft],   7,10,1, TestText2,   2, [7,8,1, 7,9,1]);
      RunAndTest('Height 3', [ecLeft],   7,10,0, TestText2,   2, [7,8,0, 7,9,0]);
      RunAndTest('Height 3', [ecLeft],   6,10,2, TestText2,   2, [6,8,2, 6,9,2]);
    PopBaseName;
  PopBaseName;

  // move through tab, but not double-widths

  PushBaseName('ecLineStart swap to carets');
    FOptAdd := [eoScrollPastEol, eoEnhanceHomeKey];
    FOptRemove := [eoTrimTrailingSpaces];
    FOpt2Add := [eoEnhanceEndKey];
    FOpt2Remove := [];

    ReCreateEdit(LocalText1);
    SetCaretsByKey(1,1, [2,0,  0,1], mcmMoveAllCarets);
    TestExtraCaretPosAndOffs('', 3,2,0,  2, [1,1,0,  3,1,0]);
    RunAndTest('3 carets',       [ecLineStart],   1,2,0, LocalText1,   2, [1,1,0,  5,1,0]);
    RunAndTest('3 carets Right', [ecRight],       2,2,0, LocalText1,   2, [2,1,0,  6,1,0]);
    RunAndTest('3 carets',       [ecLineStart],   1,2,0, LocalText1,   2, [1,1,0,  5,1,0]);
    RunAndTest('3 carets',       [ecLineStart],   7,2,0, LocalText1,   2, [1,1,0,  5,1,0]);

  PopBaseName;
end;

procedure TTestMultiCaret.Edit;
  function LocalText1: TStringArray;
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
  function LocalText1A: TStringArray;
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
  function LocalText1Del: TStringArray;
  begin
    SetLength(Result, 3);
    Result[0] := '123456';
    Result[1] := '7';
    Result[2] := '';
  end;

begin
  ReCreateEdit;
  SetLines(LocalText1);

  SetCaretAndColumnSelect(1,2, 4,0);
  TestIsCaretLogAndFullText('', 1, 6, LocalText1);

  RunCmdSeq([ecChar], ['A']);
  TestIsCaretLogAndFullText('', 2, 6, LocalText1A);
  TestExtraCaretPos('', 4, [2,2, 2,3, 2,4, 2,5]);

  RunCmdSeq([ecDeleteLastChar], []);
  TestIsCaretLogAndFullText('', 1, 6, LocalText1);
  TestExtraCaretPos('', 4, [1,2, 1,3, 1,4, 1,5]);

  RunCmdSeq([ecDeleteLastChar], []);
  TestIsCaretLogAndFullText('', 6, 1, LocalText1Del);
  TestExtraCaretPos('', 4, [2,1, 3,1, 4,1, 5,1]);

  RunCmdSeq([ecDeleteLastChar], []);
  TestIsCaretLogAndFullText('', 1, 1, LocalText1Del, [1, '6']);
  // NO extra carets
  AssertEquals(BaseTestName+'', 0, FMultiCaret.Carets.Count);



end;

procedure TTestMultiCaret.Delete;
  function LocalText1: TStringArray;
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
  function LocalText1Del: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1aA';
    Result[1] := '2B';
    Result[2] := '3C';
    Result[3] := '4D';
    Result[4] := '5E';
    Result[5] := '6F';
    Result[6] := '7gG';
    Result[7] := '';
  end;
  function LocalText1DelAndBS: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1aA';
    Result[1] := 'B';
    Result[2] := 'C';
    Result[3] := 'D';
    Result[4] := 'E';
    Result[5] := 'F';
    Result[6] := '7gG';
    Result[7] := '';
  end;
  function LocalText1Del2: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1aA';
    Result[1] := 'B';
    Result[2] := 'C';
    Result[3] := 'D';
    Result[4] := 'E';
    Result[5] := 'F';
    Result[6] := '7gG';
    Result[7] := '';
  end;
  function LocalText1DelExtra: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1aA';
    Result[1] := '2B';
    Result[2] := '3';
    Result[3] := 'D';
    Result[4] := '5E';
    Result[5] := '6F';
    Result[6] := '7gG';
    Result[7] := '';
  end;
begin
  PushBaseName('NO eoPersistentBlock, HAS eoOverwriteBlock');
  FOpt2Add := [eoOverwriteBlock];
  FOpt2Remove := [eoPersistentBlock];

    PushBaseName('ecDeleteLastChar');

      PushBaseName('ecDeleteLastChar - zero width sel');
        ReCreateEdit(LocalText1);
        SetCaretAndColumnSelect(3,2, 4,0);
        RunCmdSeq([ecDeleteLastChar], []);
        TestIsCaretLogAndFullText('', 2, 6, LocalText1Del);

      PopPushBaseName('ecDeleteLastChar - ONE width backward sel');
        ReCreateEdit(LocalText1);
        SetCaretAndColumnSelect(3,2, 4,-1);
        RunCmdSeq([ecDeleteLastChar], []);
        TestIsCaretLogAndFullText('', 2, 6, LocalText1Del);

        RunCmdSeq([ecDeleteLastChar], []);
        TestIsCaretLogAndFullText('BS again', 1, 6, LocalText1DelAndBS);

      PopPushBaseName('ecDeleteLastChar - ONE width sel');
        ReCreateEdit(LocalText1);
        SetCaretAndColumnSelect(2,2, 4,1);
        RunCmdSeq([ecDeleteLastChar], []);
        TestIsCaretLogAndFullText('', 2, 6, LocalText1Del);

      PopPushBaseName('ecDeleteLastChar - Two width sel');
        ReCreateEdit(LocalText1);
        SetCaretAndColumnSelect(1,2, 4,2);
        RunCmdSeq([ecDeleteLastChar], []);
        TestIsCaretLogAndFullText('', 1, 6, LocalText1Del2);

      PopPushBaseName('ecDeleteLastChar - ONE width sel / extra caret');
        ReCreateEdit(LocalText1);
        SetCaretAndColumnSelect(2,2, 4,1);
        FMultiCaret.AddCaretAtLogPos(4,3,0);
        FMultiCaret.AddCaretAtLogPos(2,4,0);
        RunCmdSeq([ecDeleteLastChar], []);
        TestIsCaretLogAndFullText('', 2, 6, LocalText1DelExtra);

    PopPushBaseName('ecDeleteChar');

      PushBaseName('ecDeleteChar - zero width sel');
        ReCreateEdit(TestText1);
        SetCaretAndColumnSelect(2,2, 1,0);
        RunAndTest('Height 2', [ecDeleteChar],   2, 3, TestText1(2,3, 2,1),   1, []);
        RunAndTest('Height 2', [ecRight],        3, 3, TestText1(2,3, 2,1),   0, []);
        RunCmdSeq([ecRight, ecUndo],[]);

        ReCreateEdit(TestText1);
        SetCaretAndColumnSelect(2,2, 2,0);
        RunAndTest('Height 3', [ecDeleteChar],   2, 4, TestText1(2,4, 2,1),   2, []);
        RunAndTest('Height 3', [ecRight],        3, 4, TestText1(2,4, 2,1),   0, []);
        RunCmdSeq([ecRight, ecUndo],[]);

        ReCreateEdit(TestText1);
        SetCaretAndColumnSelect(2,2, 3,0);
        RunAndTest('Height 4', [ecDeleteChar],   2, 5, TestText1(2,5, 2,1),   3, []);
        RunAndTest('Height 4', [ecRight],        3, 5, TestText1(2,5, 2,1),   0, []);
        RunCmdSeq([ecRight, ecUndo],[]);

        ReCreateEdit(TestText1);
        SetCaretAndColumnSelect(2,2, 4,0);
        RunAndTest('Height 5', [ecDeleteChar],   2, 6, TestText1(2,6, 2,1),   4, []);
        RunAndTest('Height 5', [ecRight],        3, 6, TestText1(2,6, 2,1),   0, []);
        RunCmdSeq([ecRight, ecUndo],[]);

        ReCreateEdit(TestText1);
        SetCaretAndColumnSelect(2,2, 5,0);
        RunAndTest('Height 5', [ecDeleteChar],   2, 7, TestText1(2,7, 2,1),   5, []);
        RunAndTest('Height 5', [ecRight],        3, 7, TestText1(2,7, 2,1),   0, []);
        RunCmdSeq([ecRight, ecUndo],[]);


        ReCreateEdit(TestText1);
        SetCaretAndColumnSelect(2,2, 4,0);
        RunAndTest('', [ecDeleteChar],   2, 6, TestText1(2,6, 2,1),   4, []);
        RunAndTest('', [ecRight],        3, 6, TestText1(2,6, 2,1),   0, []);


        ReCreateEdit(TestText1);
        SetCaretAndColumnSelect(1,2, 1,0);
        RunAndTest('X=0, Height 2', [ecDeleteChar],   1, 3, TestText1(2,3, 1,1),   1, []);
        RunAndTest('X=0, Height 2', [ecRight],        2, 3, TestText1(2,3, 1,1),   0, []);
        RunCmdSeq([ecRight, ecUndo],[]);

        ReCreateEdit(TestText1);
        SetCaretAndColumnSelect(1,2, 2,0);
        RunAndTest('X=0, Height 3', [ecDeleteChar],   1, 4, TestText1(2,4, 1,1),   2, []);
        RunAndTest('X=0, Height 3', [ecRight],        2, 4, TestText1(2,4, 1,1),   0, []);
        RunCmdSeq([ecRight, ecUndo],[]);

        ReCreateEdit(TestText1);
FMultiCaret.DefaultColumnSelectMode := mcmMoveAllCarets;
        SetCaretAndColumnSelect(1,2, 2,0);
        RunAndTest('X=0, Height 3', [ecDeleteChar],   1, 4, TestText1(2,4, 1,1),   2, [1,2, 1,3]);
        RunAndTest('X=0, Height 3', [ecRight],        2, 4, TestText1(2,4, 1,1),   2, [2,2, 2,3]);
        RunCmdSeq([ecUndo],[]);

      PopPushBaseName('ecDeleteChar - ONE width backward sel');
        ReCreateEdit(LocalText1);
        SetCaretAndColumnSelect(3,2, 4,-1);
        RunCmdSeq([ecDeleteChar], []);
        TestIsCaretLogAndFullText('', 2, 6, LocalText1Del);

      PopPushBaseName('ecDeleteChar - ONE width sel');
        ReCreateEdit(LocalText1);
        SetCaretAndColumnSelect(2,2, 4,1);
        RunCmdSeq([ecDeleteChar], []);
        TestIsCaretLogAndFullText('', 2, 6, LocalText1Del);

      PopPushBaseName('ecDeleteChar - Two width sel');
        ReCreateEdit(LocalText1);
        SetCaretAndColumnSelect(1,2, 4,2);
        RunCmdSeq([ecDeleteChar], []);
        TestIsCaretLogAndFullText('', 1, 6, LocalText1Del2);

    PopBaseName;
  PopBaseName;

  PopPushBaseName('NO eoPersistentBlock, NO eoOverwriteBlock');
  FOpt2Add := [];
  FOpt2Remove := [eoOverwriteBlock, eoPersistentBlock];

    PushBaseName('ecDeleteLastChar');
      PopPushBaseName('ecDeleteLastChar - Two width sel');
        ReCreateEdit(LocalText1);
        SetCaretAndColumnSelect(1,2, 4,2);
        RunCmdSeq([ecDeleteLastChar], []);
        TestIsCaretLogAndFullText('', 2, 6, LocalText1Del);

    PopPushBaseName('ecDeleteChar');
      PopPushBaseName('ecDeleteChar - Two width backward sel');
        ReCreateEdit(LocalText1);
        SetCaretAndColumnSelect(4,2, 4,-2);
        RunCmdSeq([ecDeleteChar], []);
        TestIsCaretLogAndFullText('', 2, 6, LocalText1Del);
    PopBaseName;
  PopBaseName;

  PopPushBaseName('NO eoPersistentBlock, NO eoOverwriteBlock');
  FOpt2Add := [eoPersistentBlock];
  FOpt2Remove := [eoOverwriteBlock];

    PopPushBaseName('ecDeleteLastChar - Two width sel');
      ReCreateEdit(LocalText1);
      SetCaretAndColumnSelect(1,2, 4,2);
      RunCmdSeq([ecDeleteLastChar], []);
      TestIsCaretLogAndFullText('', 2, 6, LocalText1Del);

  PopBaseName;

  // Delete and merge caret
  ReCreateEdit(TestText1);
  SetCaretAndColumnSelect(4,2, 0,0);
  SetCaretsByKey([2,0, 2,0, 2,0]); // 4carets / main caret at end
  RunAndTest('', [ecDeleteLastChar],  6,2, TestText1([2,9,1, 2,7,1, 2,5,1, 2,3,1]), 3, [3,2,0, 4,2,0, 5,2,0]);
  RunAndTest('', [ecDeleteLastChar],  2,2, TestText1([2,2,8]), 0, []);

  ReCreateEdit(TestText1);
  SetCaretAndColumnSelect(10,2, 0,0);
  SetCaretsByKey([-2,0, -2,0, -2,0]); // 4carets / main caret at start
  RunAndTest('', [ecDeleteLastChar],  3,2, TestText1([2,9,1, 2,7,1, 2,5,1, 2,3,1]), 3, [4,2,0, 5,2,0, 6,2,0]);
  RunAndTest('', [ecDeleteLastChar],  2,2, TestText1([2,2,8]), 0, []);

  ReCreateEdit(TestText1);
  SetCaretAndColumnSelect(10,2, 0,0);
  SetCaretsByKey([-2,0, -4,0, +2,0]); // 4carets / main caret in middle
  RunAndTest('', [ecDeleteLastChar],  4,2, TestText1([2,9,1, 2,7,1, 2,5,1, 2,3,1]), 3, [3,2,0, 5,2,0, 6,2,0]);
  RunAndTest('', [ecDeleteLastChar],  2,2, TestText1([2,2,8]), 0, []);

  ReCreateEdit(TestText1);
  SetCaretAndColumnSelect(3,2, 0,0);
  SetCaretsByKey([2,0, 2,0, 2,0]); // 4carets / main caret at end
  RunAndTest('', [ecDeleteChar],  6,2, TestText1([2,9,1, 2,7,1, 2,5,1, 2,3,1]), 3, [3,2,0, 4,2,0, 5,2,0]);
  RunAndTest('', [ecDeleteChar],  3,2, TestText1([2,3,8]), 0, []);


end;

procedure TTestMultiCaret.ReplaceColSel;
  function LocalText1: TStringArray;
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
  function LocalText1X: TStringArray;
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
  SetLines(LocalText1);

  SetCaretAndColumnSelect(2,2, 4,1);
  TestIsCaretLogAndFullText('', 3, 6, LocalText1);

  RunCmdSeq([ecChar], ['X']);
  TestIsCaretLogAndFullText('', 3, 6, LocalText1X);
  // 4 extra carets + main caret
  AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);

end;

procedure TTestMultiCaret.TabKey;
  function LocalText1: TStringArray;
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
  function LocalText1Tab: TStringArray;
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
  function LocalText1Indent: TStringArray;
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
  function LocalText1IndentX: TStringArray;
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
  function LocalText1TabOver: TStringArray;
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
  function LocalText1AfterIndent: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := '1a';
    Result[1] := '  2b';
    Result[2] := '  3c';
    Result[3] := '  4d';
    Result[4] := '5e';
    Result[5] := '6f';
    Result[6] := '7g';
    Result[7] := '';
  end;
begin
  PushBaseName('WITH eoTabIndent');
    FOptAdd := [eoTabIndent];
    FOptRemove := [eoTabsToSpaces, eoSmartTabs, eoTrimTrailingSpaces];

    PushBaseName('ZERO width selection');
      ReCreateEdit(LocalText1);
      SetCaretAndColumnSelect(2,2, 4,0);
      TestIsCaretLogAndFullText('', 2, 6, LocalText1);

      RunCmdSeq([ecTab], []);
      TestIsCaretLogAndFullText('', 3, 6, LocalText1Tab);
      // 4 extra carets + main caret
      AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);

    PopPushBaseName('ONE width selection');
      ReCreateEdit(LocalText1);
      SetCaretAndColumnSelect(2,2, 4,1);
      TestIsCaretLogAndFullText('', 3, 6, LocalText1);

      RunCmdSeq([ecTab], []);
      TestIsCaretLogAndFullText('', 3, 6, LocalText1TabOver);
      // 4 extra carets + main caret
      AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);
    PopBaseName;

    PopPushBaseName('indent selection');
      ReCreateEdit(LocalText1);
      SetCaretAndSel(2,2, 2,4);
      FMultiCaret.AddCaretAtLogPos(3,4,0);
      FMultiCaret.ActiveMode := mcmMoveAllCarets;

      RunCmdSeq([ecTab], []);
      TestIsCaretLogAndFullText('', 4, 4, LocalText1AfterIndent);
      TestExtraCaretPos('', 1, [5,4]);
    PopBaseName;

  PopBaseName;

  PushBaseName('WITHOUT eoTabIndent');
    FOptAdd := [];
    FOptRemove := [eoTabIndent, eoTabsToSpaces, eoSmartTabs, eoTrimTrailingSpaces];

    PushBaseName('ZERO width selection');
      ReCreateEdit(LocalText1);
      SetCaretAndColumnSelect(2,2, 4,0);
      TestIsCaretLogAndFullText('', 2, 6, LocalText1);

      RunCmdSeq([ecTab], []);
      TestIsCaretLogAndFullText('', 3, 6, LocalText1Tab);
      // 4 extra carets + main caret
      AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);


    PopPushBaseName('ONE width selection');
      ReCreateEdit(LocalText1);
      SetCaretAndColumnSelect(2,2, 4,1);
      TestIsCaretLogAndFullText('', 3, 6, LocalText1);

      RunCmdSeq([ecTab], []);
      TestIsCaretLogAndFullText('', 3, 6, LocalText1TabOver);
      // 4 extra carets + main caret
      AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);
    PopBaseName;

  PopBaseName;
end;

procedure TTestMultiCaret.Paste;
  function LocalText1: TStringArray;
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
  function LocalText1PasteNorm: TStringArray;
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
  function LocalText1PasteNormOver: TStringArray;
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
  function LocalText1PasteCol: TStringArray;
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
  function LocalText1PasteColOver: TStringArray;
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
  SetLines(LocalText1);

  SetCaret(1,1);
  RunCmdSeq([ecSelRight, ecSelRight, ecCopy], []); // copy

  SetCaretAndColumnSelect(2,2, 4,0);
  TestIsCaretLogAndFullText('', 2, 6, LocalText1);

  RunCmdSeq([ecPaste], []);
  TestIsCaretLogAndFullText('', 4, 6, LocalText1PasteNorm);
  // 4 extra carets + main caret
  AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);


  PopPushBaseName('ONE width selection -- paste normal');
  ReCreateEdit;
  SynEdit.Options := SynEdit.Options + [eoTabIndent] - [eoTabsToSpaces, eoSmartTabs, eoTrimTrailingSpaces];
  SetLines(LocalText1);

  SetCaret(1,1);
  RunCmdSeq([ecSelRight, ecSelRight, ecCopy], []); // copy

  SetCaretAndColumnSelect(2,2, 4,1);
  TestIsCaretLogAndFullText('', 3, 6, LocalText1);

  RunCmdSeq([ecPaste], []);
  TestIsCaretLogAndFullText('', 4, 6, LocalText1PasteNormOver);
  // 4 extra carets + main caret
  AssertEquals(BaseTestName+'', 4, FMultiCaret.Carets.Count);



  PushBaseName('ZERO width selection -- paste column');
  ReCreateEdit;
  SynEdit.Options := SynEdit.Options + [eoTabIndent] - [eoTabsToSpaces, eoSmartTabs, eoTrimTrailingSpaces];
  SetLines(LocalText1);

  SetCaretAndColumnSelect(1,1, 1,1);
  RunCmdSeq([ecCopy], []); // copy

  SetCaretAndColumnSelect(2,2, 4,0);
  TestIsCaretLogAndFullText('', 2, 6, LocalText1);

  RunCmdSeq([ecPaste], []);
  TestIsCaretLogAndFullText('', 3, 7, LocalText1PasteCol);
  AssertEquals(BaseTestName+'', 0, FMultiCaret.Carets.Count);


  PopPushBaseName('ONE width selection -- paste column');
  ReCreateEdit;
  SynEdit.Options := SynEdit.Options + [eoTabIndent] - [eoTabsToSpaces, eoSmartTabs, eoTrimTrailingSpaces];
  SetLines(LocalText1);

  SetCaretAndColumnSelect(1,1, 1,1);
  RunCmdSeq([ecCopy], []); // copy

  SetCaretAndColumnSelect(2,2, 4,1);
  TestIsCaretLogAndFullText('', 3, 6, LocalText1);

  RunCmdSeq([ecPaste], []);
  TestIsCaretLogAndFullText('', 3, 3, LocalText1PasteColOver);
  AssertEquals(BaseTestName+'', 0, FMultiCaret.Carets.Count);

end;

procedure TTestMultiCaret.Undo;
  function LocalText1: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := 'abcde';
    Result[1] := 'ABCDE';
    Result[2] := 'mnopq';
    Result[3] := 'MNOPQ';
    Result[4] := 'KLMXYZ';
    Result[5] := 'klmxyz';
    Result[6] := 'rsthi';
    Result[7] := '';
  end;
  function LocalText1X: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := 'acde'; // b missing
    Result[1] := 'ABCDE';
    Result[2] := 'mnopq';
    Result[3] := 'MNOPQ';
    Result[4] := 'KLMXYZ';
    Result[5] := 'klmxyz';
    Result[6] := 'rsthi';
    Result[7] := '';
  end;
  function LocalText1A: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := 'abcde';
    Result[1] := 'AB123CDE';
    Result[2] := 'mn123opq';
    Result[3] := 'MN123OPQ';
    Result[4] := 'KL123MXYZ';
    Result[5] := 'kl123mxyz';
    Result[6] := 'rsthi';
    Result[7] := '';
  end;
  function LocalText1B: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := 'abcde';
    Result[1] := 'AB145623CDE';
    Result[2] := 'mn145623opq';
    Result[3] := 'MN145623OPQ';
    Result[4] := 'KL145623MXYZ';
    Result[5] := 'kl145623mxyz';
    Result[6] := 'rsthi';
    Result[7] := '';
  end;
  function LocalText1C: TStringArray;
  begin
    SetLength(Result, 8);
    Result[0] := 'abcde';
    Result[1] := 'AB14578623CDE';
    Result[2] := 'mn14578623opq';
    Result[3] := 'MN14578623OPQ';
    Result[4] := 'KL14578623MXYZ';
    Result[5] := 'kl14578623mxyz';
    Result[6] := 'rsthi';
    Result[7] := '';
  end;
begin
  FOptAdd := [eoGroupUndo];
  PushBaseName('undo mcmCancelOnCaretMove');
    FDefaultColumnSelectMode := mcmCancelOnCaretMove;
    ReCreateEdit(TestText1);
    SetCaret(3,3);
    RunAndTest('', [ecColSelDown],       3,4, TestText1,            1, [3,3,0]);
    RunAndTest('', [ecColSelDown],       3,5, TestText1,            2, [3,3,0,  3,4,0]);
    RunAndTest('', [ecColSelDown],       3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteLastChar],   2,6, TestText1(3,6,2,1),   3, [2,3,0,  2,4,0,  2,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteChar],       3,6, TestText1(3,6,3,1),   3, [3,3,0,  3,4,0,  3,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

  PopPushBaseName('undo mcmMoveAllCarets');
    FDefaultColumnSelectMode := mcmMoveAllCarets;
    ReCreateEdit(TestText1);
    SetCaret(3,3);
    RunAndTest('', [ecColSelDown],       3,4, TestText1,            1, [3,3,0]);
    RunAndTest('', [ecColSelDown],       3,5, TestText1,            2, [3,3,0,  3,4,0]);
    RunAndTest('', [ecColSelDown],       3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteLastChar],   2,6, TestText1(3,6,2,1),   3, [2,3,0,  2,4,0,  2,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteChar],       3,6, TestText1(3,6,3,1),   3, [3,3,0,  3,4,0,  3,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteChar],       3,6, TestText1(3,6,3,1),   3, [3,3,0,  3,4,0,  3,5,0]);
    RunAndTest('', [ecLeft],             2,6, TestText1(3,6,3,1),   3, [2,3,0,  2,4,0,  2,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

  PopBaseName();


PopPushBaseName('SHARED');
  PushBaseName('undo mcmCancelOnCaretMove');
    FDefaultColumnSelectMode := mcmCancelOnCaretMove;
    ReCreateEdit(TestText1);
    ReCreateSharedEdit;
    SetCaret(3,3);
    RunAndTest('', [ecColSelDown],       3,4, TestText1,            1, [3,3,0]);
    RunAndTest('', [ecColSelDown],       3,5, TestText1,            2, [3,3,0,  3,4,0]);
    RunAndTest('', [ecColSelDown],       3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteLastChar],   2,6, TestText1(3,6,2,1),   3, [2,3,0,  2,4,0,  2,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteChar],       3,6, TestText1(3,6,3,1),   3, [3,3,0,  3,4,0,  3,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteLastChar],   2,6, TestText1(3,6,2,1),   3, [2,3,0,  2,4,0,  2,5,0]);
    SwapEdit;
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);


  PopPushBaseName('undo mcmMoveAllCarets');
    FDefaultColumnSelectMode := mcmMoveAllCarets;
    ReCreateEdit(TestText1);
    ReCreateSharedEdit;
    SetCaret(3,3);
    RunAndTest('', [ecColSelDown],       3,4, TestText1,            1, [3,3,0]);
    RunAndTest('', [ecColSelDown],       3,5, TestText1,            2, [3,3,0,  3,4,0]);
    RunAndTest('', [ecColSelDown],       3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteLastChar],   2,6, TestText1(3,6,2,1),   3, [2,3,0,  2,4,0,  2,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteChar],       3,6, TestText1(3,6,3,1),   3, [3,3,0,  3,4,0,  3,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteChar],       3,6, TestText1(3,6,3,1),   3, [3,3,0,  3,4,0,  3,5,0]);
    RunAndTest('', [ecLeft],             2,6, TestText1(3,6,3,1),   3, [2,3,0,  2,4,0,  2,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteLastChar],   2,6, TestText1(3,6,2,1),   3, [2,3,0,  2,4,0,  2,5,0]);
    SwapEdit;
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);
  PopBaseName();
PopBaseName();

PopPushBaseName('SHARED');
  PushBaseName('undo mcmCancelOnCaretMove');
    FDefaultColumnSelectMode := mcmCancelOnCaretMove;
    ReCreateEdit(TestText1);
    ReCreateSharedEdit(True);
    SetCaret(3,3);
    RunAndTest('', [ecColSelDown],       3,4, TestText1,            1, [3,3,0]);
    RunAndTest('', [ecColSelDown],       3,5, TestText1,            2, [3,3,0,  3,4,0]);
    RunAndTest('', [ecColSelDown],       3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteLastChar],   2,6, TestText1(3,6,2,1),   3, [2,3,0,  2,4,0,  2,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteChar],       3,6, TestText1(3,6,3,1),   3, [3,3,0,  3,4,0,  3,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteLastChar],   2,6, TestText1(3,6,2,1),   3, [2,3,0,  2,4,0,  2,5,0]);
    SwapEdit;
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);


  PopPushBaseName('undo mcmMoveAllCarets');
    FDefaultColumnSelectMode := mcmMoveAllCarets;
    ReCreateEdit(TestText1);
    ReCreateSharedEdit(True);
    SetCaret(3,3);
    RunAndTest('', [ecColSelDown],       3,4, TestText1,            1, [3,3,0]);
    RunAndTest('', [ecColSelDown],       3,5, TestText1,            2, [3,3,0,  3,4,0]);
    RunAndTest('', [ecColSelDown],       3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteLastChar],   2,6, TestText1(3,6,2,1),   3, [2,3,0,  2,4,0,  2,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteChar],       3,6, TestText1(3,6,3,1),   3, [3,3,0,  3,4,0,  3,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteChar],       3,6, TestText1(3,6,3,1),   3, [3,3,0,  3,4,0,  3,5,0]);
    RunAndTest('', [ecLeft],             2,6, TestText1(3,6,3,1),   3, [2,3,0,  2,4,0,  2,5,0]);
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);

    RunAndTest('', [ecDeleteLastChar],   2,6, TestText1(3,6,2,1),   3, [2,3,0,  2,4,0,  2,5,0]);
    SwapEdit;
    RunAndTest('', [ecUndo],             3,6, TestText1,            3, [3,3,0,  3,4,0,  3,5,0]);
  PopBaseName();
PopBaseName();

  PushBaseName('undo redo mcmMoveAllCarets');
    FDefaultColumnSelectMode := mcmMoveAllCarets;
    ReCreateEdit(LocalText1);
    SetCaretAndColumnSelect(3,2, 4,0);
    RunAndTest('', [ecChar,ecChar,ecChar], ['1','2','3'],     6,6, LocalText1A,  4, [6,2, 6,3, 6,4, 6,5]);
    RunAndTest('', [ecLeft,ecLeft],                           4,6, LocalText1A,  4, [4,2, 4,3, 4,4, 4,5]);
    RunAndTest('', [ecChar,ecChar,ecChar], ['4','5','6'],     7,6, LocalText1B,  4, [7,2, 7,3, 7,4, 7,5]);
    RunAndTest('', [ecLeft],                                  6,6, LocalText1B,  4, [6,2, 6,3, 6,4, 6,5]);
    RunAndTest('', [ecChar,ecChar], ['7','8'],                8,6, LocalText1C,  4, [8,2, 8,3, 8,4, 8,5]);
    RunAndTest('', [ecUndo],                                  6,6, LocalText1B,  4, [6,2, 6,3, 6,4, 6,5]);
    RunAndTest('', [ecUndo],                                  4,6, LocalText1A,  4, [4,2, 4,3, 4,4, 4,5]);
    RunAndTest('', [ecUndo],                                  3,6, LocalText1,   4, [3,2, 3,3, 3,4, 3,5]);

    RunAndTest('', [ecRedo],                                  6,6, LocalText1A,  4, [6,2, 6,3, 6,4, 6,5]);
    RunAndTest('', [ecRedo],                                  7,6, LocalText1B,  4, [7,2, 7,3, 7,4, 7,5]);
    RunAndTest('', [ecRedo], ['7','8'],                       8,6, LocalText1C,  4, [8,2, 8,3, 8,4, 8,5]);

    RunAndTest('', [ecUndo],                                  6,6, LocalText1B,  4, [6,2, 6,3, 6,4, 6,5]);
    RunAndTest('', [ecUndo],                                  4,6, LocalText1A,  4, [4,2, 4,3, 4,4, 4,5]);
    RunAndTest('', [ecUndo],                                  3,6, LocalText1,   4, [3,2, 3,3, 3,4, 3,5]);

    RunAndTest('', [ecRedo],                                  6,6, LocalText1A,  4, [6,2, 6,3, 6,4, 6,5]);
    RunAndTest('', [ecRedo],                                  7,6, LocalText1B,  4, [7,2, 7,3, 7,4, 7,5]);


    ReCreateEdit(LocalText1X);
    SetCaret(2,1);
    RunAndTest('', [ecChar], ['b'],                3,1, LocalText1,  0, []); // Undo to just ONE caret
    RunCmdSeq([ecDown], []);
    SetCaretsByKey([0,1, 0,1, 0,1, 0,1], mcmMoveAllCarets);
    RunAndTest('', [ecChar,ecChar,ecChar], ['1','2','3'],     6,6, LocalText1A,  4, [6,2, 6,3, 6,4, 6,5]);
    RunAndTest('', [ecLeft,ecLeft],                           4,6, LocalText1A,  4, [4,2, 4,3, 4,4, 4,5]);
    RunAndTest('', [ecChar,ecChar,ecChar], ['4','5','6'],     7,6, LocalText1B,  4, [7,2, 7,3, 7,4, 7,5]);
    RunAndTest('', [ecUndo],                                  4,6, LocalText1A,  4, [4,2, 4,3, 4,4, 4,5]);
    RunAndTest('', [ecUndo],                                  3,6, LocalText1,   4, [3,2, 3,3, 3,4, 3,5]);
    RunAndTest('', [ecUndo],                                  2,1, LocalText1X,  0, []);

    RunAndTest('', [ecRedo],                                  3,1, LocalText1,  0, []);
    RunAndTest('', [ecRedo],                                  6,6, LocalText1A,  4, [6,2, 6,3, 6,4, 6,5]);
    RunAndTest('', [ecRedo],                                  7,6, LocalText1B,  4, [7,2, 7,3, 7,4, 7,5]);


//    RunAndTest('', [ecColSelDown],       3,4, LocalText1,            1, [3,3,0]);

  PopBaseName;
end;

initialization
  RegisterTest(TTestMultiCaret);

end.

