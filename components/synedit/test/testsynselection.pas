unit TestSynSelection;

{$mode objfpc}{$H+}

interface

 (* TODO:
      - SelectetText for selections Past-EOL, current behaviour is not consistent:
        smNormal: SelText = empty  => should be spaces?
        smColumn: SelText = space(s)
        smLine:   SelText = TheLine (correct)
      - smLine Blocks: Begin/End Points return X values inside the line
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



initialization

  RegisterTest(TTestSynSelection); 
end.

