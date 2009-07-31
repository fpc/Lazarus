unit TestSynSelection;

{$mode objfpc}{$H+}

interface

(* TODO: Known Issues in SynEdit
      - smLine Blocks: Begin/End Points return X values inside the line
      - Go/select to matcing brace, have inconsistent rules which side of the ")" the caret must be
        They should follow the same as bracket highlight?

   TODO: Missing tests
      - Select block, move caret away and back, continue selection
      - Select by Mouse (incl scrolling while selecting)
      - Drag Block (incl scrolling)
      - Replace text
*)
uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TestBase,
  SynEdit, SynEditTypes,
  LCLType, LCLProc;

type

  { TTestSynSelection }

  TTestSynSelection = class(TTestBase)
  protected
    procedure TestIsCaret(Name: String; X, Y: Integer);
    procedure TestIsBlock(Name: String; X1, Y1, X2, Y2: Integer);
    procedure TestIsBlock(Name: String; X1, Y1, X2, Y2: Integer; Text: String);
    procedure TestIsBlock(Name: String; X1, Y1, X2, Y2: Integer; Text: Array of String);
    procedure TestIsNoBlock(Name: String);
  published
    procedure SelectByKey;
    procedure SelectByMethod;
  end;

implementation

procedure TTestSynSelection.TestIsCaret(Name: String; X, Y: Integer);
begin
  if (SynEdit.LogicalCaretXY.X <> X) or (SynEdit.LogicalCaretXY.Y <> Y) then
    TestFail(Name, 'IsCaret',
             Format('X/Y=(%d, %d)', [X, Y]),
             Format('X/Y=(%d, %d)', [SynEdit.LogicalCaretXY.X, SynEdit.LogicalCaretXY.Y]));
end;

procedure TTestSynSelection.TestIsBlock(Name: String; X1, Y1, X2, Y2: Integer);
begin
  if (not SynEdit.SelAvail)
  then
    TestFail(Name, 'IsBlock',
             Format('X/Y=(%d, %d) - (%d, %d)', [X1, Y1, X2, Y2]),
             Format('No Sel Avail (X/Y=(%d, %d) - (%d, %d))', [SynEdit.BlockBegin.X, SynEdit.BlockBegin.Y,
                                                               SynEdit.BlockEnd.X, SynEdit.BlockEnd.Y])
            );
  if (SynEdit.BlockBegin.X <> X1) or (SynEdit.BlockBegin.Y <> Y1) or
     (SynEdit.BlockEnd.X <> X2) or (SynEdit.BlockEnd.Y <> Y2)
  then
    TestFail(Name, 'IsBlock',
             Format('X/Y=(%d, %d) - (%d, %d)', [X1, Y1, X2, Y2]),
             Format('X/Y=(%d, %d) - (%d, %d)', [SynEdit.BlockBegin.X, SynEdit.BlockBegin.Y,
                                                SynEdit.BlockEnd.X, SynEdit.BlockEnd.Y])
            );
end;

procedure TTestSynSelection.TestIsBlock(Name: String; X1, Y1, X2, Y2: Integer; Text: String);
begin
  TestIsBlock(Name, X1, Y1, X2, Y2);
  if (SynEdit.SelText <> Text) then
    TestFail(Name, 'IsBlockText', '"'+DbgStr(Text)+'"', '"'+DbgStr(SynEdit.SelText)+'"');
end;

procedure TTestSynSelection.TestIsBlock(Name: String; X1, Y1, X2, Y2: Integer;
  Text: array of String);
begin
  TestIsBlock(Name, X1, Y1, X2, Y2, LinesToText(Text));
end;

procedure TTestSynSelection.TestIsNoBlock(Name: String);
begin
  if (SynEdit.SelAvail) then
    TestFail(Name, 'IsNoBlock', 'No Selection',
             Format('X/Y=(%d, %d) - (%d, %d) "%s"',
                    [SynEdit.BlockBegin.X, SynEdit.BlockBegin.Y,
                     SynEdit.BlockEnd.X, SynEdit.BlockEnd.Y,
                     '"'+DbgStr(SynEdit.SelText)+'"'])
            );
end;

procedure TTestSynSelection.SelectByKey;
var
  SkipBlockOtherEndBack: Boolean;

  procedure TestKey(Name: String;
                    StartX, StartY: Integer;  Key: Word; Shift: TShiftState;
                    EndX, EndY: Integer;      ExpText: String;
                    ExitKey: Word; ExitShift: TShiftState;
                    ExitX, ExitY, ExitSkippedX, ExitSkippedY: Integer;
                    Persist: Boolean = False );
  var
    IsBackward: Boolean;
    X1, Y1, X2, Y2: Integer;
    SkipKey, SkipBackKey: Word;

    procedure TestOnlyPersitBlock(XName: String = '');
    begin
      if Persist
      then TestIsBlock(XName, X1, Y1, X2, Y2, ExpText)
      else TestIsNoBlock(XName);
    end;
    procedure TestKeySelect(Catch: Boolean = False);      // Simulate Key - Select
    begin
      SetCaret(StartX, StartY);
      TestIsNoBlock('Sanity, after set caret');
      DoKeyPress(Key, Shift);
      if (ExpText = '') and Catch then ExpText := SynEdit.SelText;    // Capture the text, to cmpare with later selections
      TestIsCaret('After Select Key', EndX, EndY);
      TestIsBlock('After Select Key', X1, Y1, X2, Y2, ExpText);
    end;

  begin
    PushBaseName(Name);
    if Persist then
      PushBaseName('Persistent');

    IsBackward := (EndY < StartY) or ( (EndY = StartY) and (EndX < StartX) );
    if IsBackward then begin
      X1 := EndX;      Y1 := EndY;
      X2 := StartX;    Y2 := StartY;
      SkipKey     := VK_RIGHT;
      SkipBackKey := VK_LEFT;
    end else begin
      X1 := StartX;    Y1 := StartY;
      X2 := EndX;      Y2 := EndY;
      SkipKey     := VK_LEFT;
      SkipBackKey := VK_RIGHT;
    end;
    if (SynEdit.SelectionMode = smLine) then begin
      // Todo: Block x are incorect returned in smLine
      //X1 := 1;
      //X2 := length(Synedit.Lines[Y2]);
    end;

    if Persist
    then SynEdit.Options2 := SynEdit.Options2 + [eoPersistentBlock]
    else SynEdit.Options2 := SynEdit.Options2 - [eoPersistentBlock];

    SynEdit.Options  := SynEdit.Options  - [eoNoSelection];
    SynEdit.Options2 := SynEdit.Options2 - [eoCaretSkipsSelection];

    // Select, and Unselect
    PushBaseName       ('Plain Select');
    TestKeySelect      (True);
    DoKeyPress         (ExitKey, ExitShift);
    TestIsCaret        ('After Exit Key', ExitX, ExitY);
    TestOnlyPersitBlock('After Exit Key');

    // if eoCaretSkipsSelection= OFF, then block is unselected, not Skipped
    PopPushBaseName    ('Skip=Off (unselect)');
    TestKeySelect      ();
    DoKeyPress         (SkipKey, []);
    TestOnlyPersitBlock('After Skip Key');

    SynEdit.Options2 := SynEdit.Options2 + [eoCaretSkipsSelection];

    // Block is kept, if caret skips to other end (eoCaretSkipsSelection)
    // Next caret move unselects as normal
    if (ExitKey <> SkipBackKey) or (ExitShift <> []) then begin
      PopPushBaseName    ('Skip=On (keep select)');
      TestKeySelect      ();
      DoKeyPress         (SkipKey, []);
      TestIsBlock        ('After Skip Key', X1, Y1, X2, Y2, ExpText);
      DoKeyPress         (ExitKey, ExitShift);
      TestIsCaret        ('After Exit Key', ExitSkippedX, ExitSkippedY);
      TestOnlyPersitBlock('After Exit Key');
    end;

    // Block is kept, if caret skips to other end and back (eoCaretSkipsSelection)
    // Next caret move unselects as normal
    if ((ExitKey <> SkipKey) or (ExitShift <> [])) and (not SkipBlockOtherEndBack) then begin
      PopPushBaseName    ('Skip=On (keep select, 2nd)');
      TestKeySelect      ();
      DoKeyPress         (SkipKey, []);
      TestIsBlock        ('After Skip Key', X1, Y1, X2, Y2, ExpText);
      DoKeyPress         (SkipBackKey, []);
      TestIsBlock        ('After SkipBack Key', X1, Y1, X2, Y2, ExpText);
      DoKeyPress         (ExitKey, ExitShift);
      TestIsCaret        ('After Exit Key', ExitX, ExitY);
      TestOnlyPersitBlock('After Exit Key');
    end;

    // Switching eoCaretSkipsSelection= OFF while selected is taken ito account on next move
    PopPushBaseName    ('Skip=On/Off');
    TestKeySelect      ();
    DoKeyPress         (SkipKey, []);
    TestIsBlock        ('After Skip Key', X1, Y1, X2, Y2, ExpText);
    SynEdit.Options2 := SynEdit.Options2 - [eoCaretSkipsSelection];
    DoKeyPress         (SkipBackKey, []);
    TestOnlyPersitBlock('After SkipBack Key (Skip=Off)');

    // Switching eoNoSelection=On while selected, clears the block
    PopPushBaseName    ('change eoNoSelection');
    TestKeySelect      ();
    SynEdit.Options  := SynEdit.Options  + [eoNoSelection];
    TestIsNoBlock      ('After setting eoNoSelection');

    // No selection, if eoNoSelection=On
    PopPushBaseName    ('eoNoSelection');
    SetCaret(StartX, StartY);
    TestIsNoBlock('Sanity, after set caret');
    DoKeyPress(Key, Shift);
    TestIsNoBlock('After Select Key');

    SynEdit.Options  := SynEdit.Options  - [eoNoSelection];

    PopBaseName;
    if Persist then
      PopBaseName;
    PopBaseName;

    // Repeat for persistent block
    if not Persist then
      TestKey(Name, StartX, StartY, Key, Shift, EndX, EndY, ExpText,
              ExitKey, ExitShift, ExitX, ExitY, ExitSkippedX, ExitSkippedY,
              True
             );
  end;

  procedure TestKey(Name: String;
                    StartX, StartY: Integer;  Key: Word; Shift: TShiftState;
                    EndX, EndY: Integer;      ExpText: Array of String;
                    ExitKey: Word; ExitShift: TShiftState;
                    ExitX, ExitY, ExitSkippedX, ExitSkippedY: Integer
                   );
  begin
    TestKey(Name, StartX, StartY, Key, Shift, EndX, EndY, LinesToText(ExpText),
            ExitKey, ExitShift, ExitX, ExitY, ExitSkippedX, ExitSkippedY
           );
  end;

begin
  ReCreateEdit;
  BaseTestName := 'SelectByKey';

  (* tests include:
       eoPersistentBlock
       eoCaretSkipsSelection
       eoNoSelection
       eoScrollPastEol
  *)
  // CaretPos are Logical, to save params

  SynEdit.TabWidth := 4;
  SkipBlockOtherEndBack := False;

  SetLines(['begin',
            '  Foo(bar);',
            '  // äüöäabc', // Utf8 2 bytes per char
            #9#9+'test',    // Tab  1 byte / several display cells
            'end;'
           ]);


  PushBaseName('Default:smNormal');
    SynEdit.DefaultSelectionMode := smNormal;
    //      Name                                   Start  Select-Key          Block-End    UnSel-Key     End-Caret/Skipped
    TestKey('S-VK_RIGHT, Exit=VK_RIGHT',            3,2, VK_RIGHT,[ssShift],  4,2, 'F',    VK_RIGHT,[],  5,2, 4,2);
    TestKey('S-VK_RIGHT, Exit=VK_LEFT',             3,2, VK_RIGHT,[ssShift],  4,2, 'F',    VK_LEFT,[],   3,2, 2,2); // VK_LEFT goes back to block begin;
    TestKey('S-VK_RIGHT, Exit=VK_UP',               3,2, VK_RIGHT,[ssShift],  4,2, 'F',    VK_UP,[],     4,1, 3,1);
    TestKey('S-VK_RIGHT, Exit=VK_UP(no move)',      3,1, VK_RIGHT,[ssShift],  4,1, 'g',    VK_UP,[],     4,1, 3,1);

    TestKey('S-VK_RIGHT, Exit=VK_RIGHT utf8',       8,3, VK_RIGHT,[ssShift], 10,3, 'ü',    VK_RIGHT,[], 12,3,10,2);
    TestKey('S-VK_RIGHT, Exit=VK_LEFT utf8',        8,3, VK_RIGHT,[ssShift], 10,3, 'ü',    VK_LEFT,[],   8,3, 6,3);
    // Todo: tab (currently shift-left, from behind a tab, will create blockbegin <> caret

    TestKey('S-VK_LEFT,  Exit=VK_LEFT',             3,2, VK_LEFT,[ssShift],   2,2, ' ',    VK_LEFT,[],   1,2, 2,2);
    TestKey('S-VK_LEFT,  Exit=VK_RIGHT',            3,2, VK_LEFT,[ssShift],   2,2, ' ',    VK_RIGHT,[],  3,2, 4,2);
    SynEdit.Options  := SynEdit.Options  + [eoScrollPastEol];
    TestKey('S-VK_LEFT,  Exit=VK_LEFT(no move)',    2,2, VK_LEFT,[ssShift],   1,2, ' ',    VK_LEFT,[],   1,2, 1,2);
    SynEdit.Options  := SynEdit.Options  - [eoScrollPastEol];

    TestKey('S-VK_UP, Exit=VK_DOWN',                3,2, VK_UP,[ssShift],     3,1, ['gin','  '],  VK_DOWN,[],   3,2, 3,3);

    TestKey('S-VK_END, Exit=VK_END',                7,2, VK_END,[ssShift],   12,2, 'bar);',       VK_END,[],   12,2, 12,2);
    TestKey('S-VK_HOME, Exit=VK_HOME',              3,1, VK_HOME,[ssShift],   1,1, 'be',          VK_HOME,[],   1,1, 1,1);

    TestKey('SC-VK_RIGHT, Exit=C-VK_LEFT (word)',      3,2, VK_RIGHT,[ssShift,ssCtrl], 7,2, 'Foo(',  VK_LEFT,[ssCtrl],   3,2, 6,1);
    TestKey('SC-VK_RIGHT, Exit=C-VK_LEFT (half word)', 4,2, VK_RIGHT,[ssShift,ssCtrl], 7,2, 'oo(',   VK_LEFT,[ssCtrl],   3,2, 3,2);

    SynEdit.Options := SynEdit.Options  + [eoScrollPastEol];
    TestKey('S-VK_LEFT,  Exit=VK_LEFT',             8,1, VK_LEFT,[ssShift],   7,1, '',    VK_LEFT,[],   6,1, 7,1);
    TestKey('S-VK_LEFT,  Exit=VK_RIGHT',            8,1, VK_LEFT,[ssShift],   7,1, '',    VK_RIGHT,[],  8,1, 9,1);

    // Column selection (force horiz move, by going to end of shorter line)
    SynEdit.Options := SynEdit.Options  - [eoScrollPastEol, eoKeepCaretX];
    TestKey('SA-VK_UP (Col), Exit=VK_DOWN (PastEOL=Off KeepX=Off)', 10,2, VK_UP,[ssShift, ssAlt],   6,1, ['    ','(bar'],  VK_DOWN,[],   6,2, 14,3); // caret-after-skip + 4 utf8 2 byte
    SkipBlockOtherEndBack := True; // KeepCaretX wouldn't work
    SynEdit.Options := SynEdit.Options  - [eoScrollPastEol] + [eoKeepCaretX];
    TestKey('SA-VK_UP (Col), Exit=VK_DOWN (PastEOL=Off KeepX=On)',  10,2, VK_UP,[ssShift, ssAlt],   6,1, ['    ','(bar'],  VK_DOWN,[],  10,2, 14,3); // caret-after-skip + 4 utf8 2 byte
    SkipBlockOtherEndBack := False;


  PopPushBaseName('Default:smColumn');
    SynEdit.DefaultSelectionMode := smColumn;
    //      Name                                      Start  Select-Key          Block-End    UnSel-Key     End-Caret/Skipped
    TestKey('S-VK_RIGHT, Exit=VK_RIGHT',               3,2, VK_RIGHT,[ssShift],  4,2, 'F',    VK_RIGHT,[],  5,2, 4,2);
    TestKey('S-VK_RIGHT, Exit=VK_LEFT',                3,2, VK_RIGHT,[ssShift],  4,2, 'F',    VK_LEFT,[],   3,2, 2,2); // VK_LEFT goes back to block begin;
    SynEdit.Options  := SynEdit.Options  + [eoScrollPastEol];
    TestKey('S-VK_LEFT,  Exit=VK_LEFT(no move)',       2,2, VK_LEFT,[ssShift],   1,2, ' ',    VK_LEFT,[],   1,2, 1,2);
    SynEdit.Options  := SynEdit.Options  - [eoScrollPastEol];

    TestKey('S-VK_END, Exit=VK_END',                   7,2, VK_END,[ssShift],   12,2, 'bar);',       VK_END,[],   12,2, 12,2);
    TestKey('S-VK_HOME, Exit=VK_HOME',                 3,1, VK_HOME,[ssShift],   1,1, 'be',          VK_HOME,[],   1,1, 1,1);
    TestKey('SC-VK_RIGHT, Exit=C-VK_LEFT (word)',      3,2, VK_RIGHT,[ssShift,ssCtrl], 7,2, 'Foo(',  VK_LEFT,[ssCtrl],   3,2, 6,1);
    TestKey('SC-VK_RIGHT, Exit=C-VK_LEFT (half word)', 4,2, VK_RIGHT,[ssShift,ssCtrl], 7,2, 'oo(',   VK_LEFT,[ssCtrl],   3,2, 3,2);

    SynEdit.Options := SynEdit.Options  + [eoScrollPastEol];
    TestKey('S-VK_LEFT,  Exit=VK_LEFT',             8,1, VK_LEFT,[ssShift],   7,1, ' ',    VK_LEFT,[],   6,1, 7,1);
    TestKey('S-VK_LEFT,  Exit=VK_RIGHT',            8,1, VK_LEFT,[ssShift],   7,1, ' ',    VK_RIGHT,[],  8,1, 9,1);

    // Column selection (force horiz move, by going to end of shorter line)
    SynEdit.Options := SynEdit.Options  - [eoScrollPastEol, eoKeepCaretX];
    TestKey('SA-VK_UP (Col), Exit=VK_DOWN (PastEOL=Off KeepX=Off)', 10,2, VK_UP,[ssShift, ssAlt],   6,1, ['    ','(bar'],  VK_DOWN,[],   6,2, 14,3); // caret-after-skip + 4 utf8 2 byte
    SkipBlockOtherEndBack := True; // KeepCaretX wouldn't work
    SynEdit.Options := SynEdit.Options  - [eoScrollPastEol] + [eoKeepCaretX];
    TestKey('SA-VK_UP (Col), Exit=VK_DOWN (PastEOL=Off KeepX=On)',  10,2, VK_UP,[ssShift, ssAlt],   6,1, ['    ','(bar'],  VK_DOWN,[],  10,2, 14,3); // caret-after-skip + 4 utf8 2 byte
    SkipBlockOtherEndBack := False;


  PopPushBaseName('Default:smLine');
    SynEdit.DefaultSelectionMode := smLine;
    //      Name                                   Start  Select-Key          Block-End              UnSel-Key     End-Caret/Skipped
    TestKey('S-VK_RIGHT, Exit=VK_RIGHT',            3,2, VK_RIGHT,[ssShift],  4,2, ['  Foo(bar);', ''],    VK_RIGHT,[],  5,2, 4,2);
    TestKey('S-VK_RIGHT, Exit=VK_LEFT',             3,2, VK_RIGHT,[ssShift],  4,2, ['  Foo(bar);', ''],    VK_LEFT,[],   3,2, 2,2); // VK_LEFT goes back to block begin;
    TestKey('S-VK_RIGHT, Exit=VK_UP',               3,2, VK_RIGHT,[ssShift],  4,2, ['  Foo(bar);', ''],    VK_UP,[],     4,1, 3,1);
    TestKey('S-VK_RIGHT, Exit=VK_UP(no move)',      3,1, VK_RIGHT,[ssShift],  4,1, ['begin', ''],          VK_UP,[],     4,1, 3,1);

    SynEdit.Options  := SynEdit.Options  + [eoScrollPastEol];
    TestKey('S-VK_LEFT,  Exit=VK_LEFT(no move)',    2,2, VK_LEFT,[ssShift],   1,2, ['  Foo(bar);', ''],    VK_LEFT,[],   1,2, 1,2);
    SynEdit.Options  := SynEdit.Options  - [eoScrollPastEol];

    TestKey('S-VK_UP, Exit=VK_DOWN',                3,2, VK_UP,[ssShift],     3,1, ['begin','  Foo(bar);', ''],  VK_DOWN,[],   3,2, 3,3);
    TestKey('S-VK_END, Exit=VK_END',                7,2, VK_END,[ssShift],   12,2, ['  Foo(bar);', ''],          VK_END,[],   12,2, 12,2);
    TestKey('S-VK_HOME, Exit=VK_HOME',              3,1, VK_HOME,[ssShift],   1,1, ['begin', ''],                VK_HOME,[],   1,1, 1,1);

    TestKey('SC-VK_RIGHT, Exit=C-VK_LEFT (word)',      3,2, VK_RIGHT,[ssShift,ssCtrl], 7,2, ['  Foo(bar);', ''],  VK_LEFT,[ssCtrl],   3,2, 6,1);
    TestKey('SC-VK_RIGHT, Exit=C-VK_LEFT (half word)', 4,2, VK_RIGHT,[ssShift,ssCtrl], 7,2, ['  Foo(bar);', ''],  VK_LEFT,[ssCtrl],   3,2, 3,2);

    SynEdit.Options := SynEdit.Options  + [eoScrollPastEol];
    TestKey('S-VK_LEFT,  Exit=VK_LEFT',             8,1, VK_LEFT,[ssShift],   7,1, ['begin', ''],    VK_LEFT,[],   6,1, 7,1);
    TestKey('S-VK_LEFT,  Exit=VK_RIGHT',            8,1, VK_LEFT,[ssShift],   7,1, ['begin', ''],    VK_RIGHT,[],  8,1, 9,1);

    // Column selection (force horiz move, by going to end of shorter line)
    SynEdit.Options := SynEdit.Options  - [eoScrollPastEol, eoKeepCaretX];
    TestKey('SA-VK_UP (Col), Exit=VK_DOWN (PastEOL=Off KeepX=Off)', 10,2, VK_UP,[ssShift, ssAlt],   6,1, ['    ','(bar'],  VK_DOWN,[],   6,2, 14,3); // caret-after-skip + 4 utf8 2 byte
    SkipBlockOtherEndBack := True; // KeepCaretX wouldn't work
    SynEdit.Options := SynEdit.Options  - [eoScrollPastEol] + [eoKeepCaretX];
    TestKey('SA-VK_UP (Col), Exit=VK_DOWN (PastEOL=Off KeepX=On)',  10,2, VK_UP,[ssShift, ssAlt],   6,1, ['    ','(bar'],  VK_DOWN,[],  10,2, 14,3); // caret-after-skip + 4 utf8 2 byte
    SkipBlockOtherEndBack := False;


  PopBaseName; // Default:smLine

  // ColumnSelection, Continue after zero width
  SynEdit.DefaultSelectionMode := smNormal;

  PushBaseName('BlockSel through zero width');
  SynEdit.Options2 := SynEdit.Options2 - [eoPersistentBlock];
  SetCaret(3, 1);
  DoKeyPress(VK_RIGHT, [ssShift, ssAlt]);
  TestIsBlock('after VK_RIGHT', 3, 1, 4, 1, 'g');
  DoKeyPress(VK_DOWN, [ssShift, ssAlt]);
  TestIsBlock('after VK_DOWN', 3, 1, 4, 2, ['g', 'F']);
  DoKeyPress(VK_LEFT, [ssShift, ssAlt]);
  TestIsNoBlock('after VK_LEFT (empty)');
  DoKeyPress(VK_LEFT, [ssShift, ssAlt]);
  TestIsBlock('after VK_LEFT (continue)', 3, 1, 2, 2, ['e', ' ']);

  PushBaseName('BlockSel through zero width (persistent)');
  SynEdit.Options2 := SynEdit.Options2 + [eoPersistentBlock];
  SetCaret(3, 1);
  DoKeyPress(VK_RIGHT, [ssShift, ssAlt]);
  TestIsBlock('after VK_RIGHT', 3, 1, 4, 1, 'g');
  DoKeyPress(VK_DOWN, [ssShift, ssAlt]);
  TestIsBlock('after VK_DOWN', 3, 1, 4, 2, ['g', 'F']);
  DoKeyPress(VK_LEFT, [ssShift, ssAlt]);
  TestIsNoBlock('after VK_LEFT (empty)');
  DoKeyPress(VK_LEFT, [ssShift, ssAlt]);
  TestIsBlock('after VK_LEFT (continue)', 3, 1, 2, 2, ['e', ' ']);

end;

procedure TTestSynSelection.SelectByMethod;
var
  NoSelection : Boolean;
  UseBeginUpdate: Boolean;

  procedure DoTestIsBlock(Name: String; X1, Y1, X2, Y2: Integer; ExpText: Array of String);
  begin
    if NoSelection
    then TestIsNoBlock(Name)
    else begin
      TestIsBlock(Name, X1, Y1, X2, Y2, ExpText);
      TestIsCaret(Name, X2, Y2);
    end;
  end;
  procedure DoTestIsBlockBackward(Name: String; X1, Y1, X2, Y2: Integer; ExpText: Array of String);
  begin
    if NoSelection
    then TestIsNoBlock(Name)
    else begin
      TestIsBlock(Name, X1, Y1, X2, Y2, ExpText);
      TestIsCaret(Name, X1, Y1); // caret at start
    end;
  end;
  procedure DoLock;
  begin
    if UseBeginUpdate then SynEdit.BeginUpdate;
  end;
  procedure DoUnLock;
  begin
    if UseBeginUpdate then SynEdit.EndUpdate;
  end;

  procedure DoSelectByMethod;
  var
    i: Integer;
  Begin
    (* *** smNormal *** *)
    PushBaseName('Default:smNormal');
    SynEdit.DefaultSelectionMode := smNormal;

    // word
    for i := 7 to 11 do begin // includes 11: select previous word, if no current
      SetCaret(i, 3);
      DoLock;
      SynEdit.SelectWord;
      DoUnLock;
      DoTestIsBlock('Select Word x='+IntToStr(i), 7,3, 10,3, ['bar']);
    end;
    SetCaret(1, 3);          // Select next word, if neitheer current nor previous
    DoLock;
    SynEdit.SelectWord;
    DoUnLock;
    DoTestIsBlock('Select Word x=1', 3,3, 6,3, ['Foo']);

    if UseBeginUpdate then begin
      SetCaret(8, 3);
      DoLock;
      SynEdit.SelectWord;
      SynEdit.LogicalCaretXY := Point(7, 3);
      DoUnLock;
      DoTestIsBlockBackward('Select Word, caret to start', 7,3, 10,3, ['bar']);
    end;

    // Line
    SetCaret(4, 3);
    DoLock;
    SynEdit.SelectLine(True);
    DoUnLock;
    DoTestIsBlock('Select Line (inc space)', 1,3, 12,3, ['  Foo(bar);']);

    SetCaret(4, 3);
    DoLock;
    SynEdit.SelectLine(False);
    DoUnLock;
    DoTestIsBlock('Select Line (excl space)', 3,3, 12,3, ['Foo(bar);']);

    // paragraph (includes 1 line before)
    SetCaret(1, 3);
    DoLock;
    SynEdit.SelectParagraph;
    DoUnLock;
    DoTestIsBlock('Select ParagraphLine Y=3', 1,2, 1,7, ['', '  Foo(bar);',
                                   '  abc;', '  // äüöäabc', #9#9+'test', '']);

    SetCaret(1, 2);
    DoLock;
    SynEdit.SelectParagraph;
    DoUnLock;
    DoTestIsBlock('Select ParagraphLine Y=2', 1,2, 1,7, ['', '  Foo(bar);',
                                   '  abc;', '  // äüöäabc', #9#9+'test', '']);

    SetCaret(1, 1);
    DoLock;
    SynEdit.SelectParagraph;
    DoUnLock;
    DoTestIsBlock('Select ParagraphLine Y=2 (begin of file)', 1,1, 1,2, ['begin', '']);

    SetCaret(1, 9);
    DoLock;
    SynEdit.SelectParagraph;
    DoUnLock;
    DoTestIsBlock('Select ParagraphLine Y=9 (end of file)', 1,8, 4,10, ['', 'end;(', '//)']);

    SetCaret(1, 7);
    DoLock;
    SynEdit.SelectParagraph;
    DoUnLock;
    DoTestIsBlock('Select ParagraphLine Y=7 (empty para)', 1,7, 1,8, ['', '']);

    // select all
    SetCaret(3, 3);
    DoLock;
    SynEdit.SelectAll;
    DoUnLock;
    DoTestIsBlock('Select All', 1,1, 4,10, ['begin', '', '  Foo(bar);', '  abc;',
                        '  // äüöäabc', #9#9+'test', '', '', 'end;(', '//)']);

    // select to brace
    SetCaret(8, 3);
    DoLock;
    SynEdit.SelectToBrace;
    DoUnLock;
    TestIsNoBlock('SelectToBrace (not at brace)');

    SetCaret(6, 3);
    DoLock;
    SynEdit.SelectToBrace;
    DoUnLock;
    DoTestIsBlockBackward('SelectToBrace X=6', 6,3, 11,3, ['(bar)']);

    SetCaret(11, 3);
    DoLock;
    SynEdit.SelectToBrace;
    DoUnLock;
    DoTestIsBlock('SelectToBrace X=11', 6,3, 11,3, ['(bar)']);

    SetCaret(5, 9);
    DoLock;
    SynEdit.SelectToBrace;
    DoUnLock;
    DoTestIsBlockBackward('SelectToBrace multi line', 5,9, 4,10, ['(', '//)']);

    // BlockBegin/end
    SetCaret(1, 1);
    DoLock;
    SetCaret(4, 3);
    SynEdit.BlockBegin := Point(2,3);
    SynEdit.BlockEnd   := Point(4,3);
    DoUnLock;
    DoTestIsBlock('Select Begin/End', 2,3, 4,3, [' F']);

    SetCaret(1, 1);
    DoLock;
    SetCaret(1, 4);
    SynEdit.BlockBegin := Point(12,3); // just lineend
    SynEdit.BlockEnd   := Point( 1,4);
    DoUnLock;
    DoTestIsBlock('Select Begin/End CrLf', 12,3, 1,4, ['', '']);

    SetCaret(1, 1);
    DoLock;
    SetCaret(11, 3);
    SynEdit.BlockBegin := Point(11,3);
    SynEdit.BlockEnd   := Point(13,3);
    DoUnLock;
    DoTestIsBlockBackward('Select Begin/End Eol', 11,3, 13,3, ['; ']);


    (* *** smColumn *** *)
    PopPushBaseName('Default:smColumn');
    SynEdit.DefaultSelectionMode := smColumn;

    // word
    for i := 7 to 11 do begin // includes 11: select previous word, if no current
      SetCaret(i, 3);
      DoLock;
      SynEdit.SelectWord;
      DoUnLock;
      DoTestIsBlock('Select Word x='+IntToStr(i), 7,3, 10,3, ['bar']);
    end;
    SetCaret(1, 3);          // Select next word, if neitheer current nor previous
    DoLock;
    SynEdit.SelectWord;
    DoUnLock;
    DoTestIsBlock('Select Word x=1', 3,3, 6,3, ['Foo']);

    if UseBeginUpdate then begin
      SetCaret(8, 3);
      DoLock;
      SynEdit.SelectWord;
      SynEdit.LogicalCaretXY := Point(7, 3);
      DoUnLock;
      DoTestIsBlockBackward('Select Word, caret to start', 7,3, 10,3, ['bar']);
    end;

    // Line
    SetCaret(4, 3);
    DoLock;
    SynEdit.SelectLine(True);
    DoUnLock;
    DoTestIsBlock('Select Line (inc space)', 1,3, 12,3, ['  Foo(bar);']);

    SetCaret(4, 3);
    DoLock;
    SynEdit.SelectLine(False);
    DoUnLock;
    DoTestIsBlock('Select Line (excl space)', 3,3, 12,3, ['Foo(bar);']);

    // paragraph (includes 1 line before)
    SetCaret(1, 3);
    DoLock;
    SynEdit.SelectParagraph;
    DoUnLock;
    DoTestIsBlock('Select ParagraphLine Y=3', 1,2, 1,7, ['', '  Foo(bar);',
                                   '  abc;', '  // äüöäabc', #9#9+'test', '']);

    SetCaret(1, 9);
    DoLock;
    SynEdit.SelectParagraph;
    DoUnLock;
    DoTestIsBlock('Select ParagraphLine Y=9 (end of file)', 1,8, 4,10, ['', 'end;(', '//)']);

    SetCaret(1, 7);
    DoLock;
    SynEdit.SelectParagraph;
    DoUnLock;
    DoTestIsBlock('Select ParagraphLine Y=7 (empty para)', 1,7, 1,8, ['', '']);

    // select all
    SetCaret(3, 3);
    DoLock;
    SynEdit.SelectAll;
    DoUnLock;
    DoTestIsBlock('Select All', 1,1, 4,10, ['begin', '', '  Foo(bar);', '  abc;',
                        '  // äüöäabc', #9#9+'test', '', '', 'end;(', '//)']);

    // select to brace
    SetCaret(6, 3);
    DoLock;
    SynEdit.SelectToBrace;
    DoUnLock;
    DoTestIsBlockBackward('SelectToBrace X=6', 6,3, 11,3, ['(bar)']);

    SetCaret(5, 9);
    DoLock;
    SynEdit.SelectToBrace;
    DoUnLock;
    DoTestIsBlockBackward('SelectToBrace multi line', 5,9, 4,10, [';', ' ']);

    // BlockBegin/end
    SetCaret(1, 1);
    DoLock;
    SetCaret(4, 3);
    SynEdit.BlockBegin := Point(2,3);
    SynEdit.BlockEnd   := Point(4,3);
    DoUnLock;
    DoTestIsBlock('Select Begin/End 1 line', 2,3, 4,3, [' F']);

    SetCaret(1, 1);
    DoLock;
    SetCaret(13, 3);
    SynEdit.BlockBegin := Point(11,3);
    SynEdit.BlockEnd   := Point(13,3);
    DoUnLock;
    DoTestIsBlock('Select Begin/End 1 line Eol', 11,3, 13,3, ['; ']);

    SetCaret(1, 1);
    DoLock;
    SetCaret(6, 5);
    SynEdit.BlockBegin := Point(10,3);
    SynEdit.BlockEnd   := Point(6,5);
    DoUnLock;
    DoTestIsBlock('Select Begin/End utf8 4 column', 10,3, 6,5, ['(bar', ';   ', 'äüöä']); // 3 utf8

    SetCaret(1, 1);
    DoLock;
    SetCaret(10, 3);
    SynEdit.BlockBegin := Point(10,3);
    SynEdit.BlockEnd   := Point(9+3,5);
    DoUnLock;
    DoTestIsBlockBackward('Select Begin/End utf8  1 column', 10,3, 9+3,5, ['r', ' ', 'ä']); // 3 utf8

    SetCaret(1, 1);
    DoLock;
    SetCaret(5, 9);
    SynEdit.BlockBegin := Point(1, 5);
    SynEdit.BlockEnd   := Point(5, 9);
    DoUnLock;
    DoTestIsBlock('Select Begin/End tabs', 1,5, 5,9, ['  //', #9, '    ', '    ', 'end;']); // 3 utf8



    (* *** smLine *** *)
    PopPushBaseName('Default:smLine');
    SynEdit.DefaultSelectionMode := smLine;

    // word
    for i := 7 to 11 do begin // includes 11: select previous word, if no current
      SetCaret(i, 3);
      DoLock;
      SynEdit.SelectWord;
      DoUnLock;
      DoTestIsBlock('Select Word x='+IntToStr(i), 7,3, 10,3, ['bar']);
    end;
    SetCaret(1, 3);          // Select next word, if neitheer current nor previous
    DoLock;
    SynEdit.SelectWord;
    DoUnLock;
    DoTestIsBlock('Select Word x=1', 3,3, 6,3, ['Foo']);

    if UseBeginUpdate then begin
      SetCaret(8, 3);
      DoLock;
      SynEdit.SelectWord;
      SynEdit.LogicalCaretXY := Point(7, 3);
      DoUnLock;
      DoTestIsBlockBackward('Select Word, caret to start', 7,3, 10,3, ['bar']);
    end;

    // Line
    SetCaret(4, 3);
    DoLock;
    SynEdit.SelectLine(True);
    DoUnLock;
    DoTestIsBlock('Select Line (inc space)', 1,3, 12,3, ['  Foo(bar);']);

    SetCaret(4, 3);
    DoLock;
    SynEdit.SelectLine(False);
    DoUnLock;
    DoTestIsBlock('Select Line (excl space)', 3,3, 12,3, ['Foo(bar);']); // TODO wrong X pos

    // paragraph (includes 1 line before)
    SetCaret(1, 3);
    DoLock;
    SynEdit.SelectParagraph;
    DoUnLock;
    DoTestIsBlock('Select ParagraphLine Y=3', 1,2, 1,7, ['', '  Foo(bar);',
                                   '  abc;', '  // äüöäabc', #9#9+'test', '']);

    SetCaret(1, 2);
    DoLock;
    SynEdit.SelectParagraph;
    DoUnLock;
    DoTestIsBlock('Select ParagraphLine Y=2', 1,2, 1,7, ['', '  Foo(bar);',
                                   '  abc;', '  // äüöäabc', #9#9+'test', '']);

    SetCaret(1, 1);
    DoLock;
    SynEdit.SelectParagraph;
    DoUnLock;
    DoTestIsBlock('Select ParagraphLine Y=2 (begin of file)', 1,1, 1,2, ['begin', '']);

    SetCaret(1, 9);
    DoLock;
    SynEdit.SelectParagraph;
    DoUnLock;
    DoTestIsBlock('Select ParagraphLine Y=9 (end of file)', 1,8, 4,10, ['', 'end;(', '//)']);

    SetCaret(1, 7);
    DoLock;
    SynEdit.SelectParagraph;
    DoUnLock;
    DoTestIsBlock('Select ParagraphLine Y=7 (empty para)', 1,7, 1,8, ['', '']);

    // select all
    SetCaret(3, 3);
    DoLock;
    SynEdit.SelectAll;
    DoUnLock;
    DoTestIsBlock('Select All', 1,1, 4,10, ['begin', '', '  Foo(bar);', '  abc;',
                        '  // äüöäabc', #9#9+'test', '', '', 'end;(', '//)']);

    // select to brace
    // TODO

    // BlockBegin/end
    SetCaret(1, 1);
    DoLock;
    SetCaret(4, 3);
    SynEdit.BlockBegin := Point(2,3);
    SynEdit.BlockEnd   := Point(4,3);
    DoUnLock;
    DoTestIsBlock('Select Begin/End', 2,3, 4,3, ['  Foo(bar);', '']); // TODO wrong X


    PopBaseName;
  End;

begin
  ReCreateEdit;
  BaseTestName := 'SelectByMethod';

  (* tests include:
       eoPersistentBlock
       eoNoSelection
       Embed into BeginUpdate for locks
  *)
  // CaretPos are Logical, to save params

  SynEdit.TabWidth := 4;
  SynEdit.Options := SynEdit.Options  + [eoScrollPastEol];

  SetLines(['begin',
            '',
            '  Foo(bar);',
            '  abc;',
            '  // äüöäabc', // Utf8 2 bytes per char
            #9#9+'test',    // Tab  1 byte / several display cells
            '',
            '',
            'end;(',
            '//)'
           ]);

  NoSelection := False;
  SynEdit.Options  := SynEdit.Options  - [eoNoSelection];
  SynEdit.Options2 := SynEdit.Options2 + [eoPersistentBlock];

  PushBaseName('NonePersist');
  UseBeginUpdate := False;
  DoSelectByMethod;

  PopPushBaseName('NonePersist Locked');
  UseBeginUpdate := True;
  DoSelectByMethod;

  NoSelection := True;
  SynEdit.Options  := SynEdit.Options  + [eoNoSelection];

  PopPushBaseName('NonePersist eoNoSelection');
  UseBeginUpdate := False;
  DoSelectByMethod;


  NoSelection := False;
  SynEdit.Options  := SynEdit.Options  - [eoNoSelection];
  SynEdit.Options2 := SynEdit.Options2 - [eoPersistentBlock];

  PopPushBaseName('Persisent');
  UseBeginUpdate := False;
  DoSelectByMethod;

  PopPushBaseName('Persisent Locked');
  UseBeginUpdate := True;
  DoSelectByMethod;

  NoSelection := True;
  SynEdit.Options  := SynEdit.Options  + [eoNoSelection];
  PopPushBaseName('Persisent eoNoSelection Locked');
  DoSelectByMethod;
end;



initialization

  RegisterTest(TTestSynSelection); 
end.

