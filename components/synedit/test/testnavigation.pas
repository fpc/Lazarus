unit TestNavigation;

(* TODO:
     Word Left/Right: Need tests with fold, trim, selection
*)

{$mode objfpc}{$H+}

interface

uses
  SysUtils, testregistry, LCLProc, Forms, TestBase,
  SynEdit, SynEditKeyCmds;

type

  { TTestSynNavigation }

  TTestSynNavigation = class(TTestBase)
  published
    procedure TestCaretHomeEnd;
    procedure TestCaretLeftRight;
    procedure TestCaretMoveLeftRightWord;
    //procedure TestCaretMoveLeftRightPartWord;
  end;

implementation

{ TTestSynNavigation }

procedure TTestSynNavigation.TestCaretHomeEnd;
  procedure DoInit1;
  begin
    ReCreateEdit;
    SynEdit.TabWidth := 6;
    SyneDit.Options := [];
    SyneDit.Options2 := [];
    SetLines(['',           //1
              '',

              'test',       // 3
              '',
              '',
              '                                                      ',
              'test',       // 3
              #9#9#9#9#9,

              '   spaced',  // 9
              '',
              '',
              '                                                      ',
              '   spaced',
              #9#9#9#9#9,

              #9'tabbed',    // 15
              '',
              '',
              '                                                      ',
              #9'tabbed',
              #9#9#9#9#9,

              #9'   tabbed spaced', // 21
              '',
              '',
              '                                                      ',
              #9'   tabbed spaced',
              #9#9#9#9#9,

              '   ', // 27  (space only)
              '',
              '',
              '                                                      ',
              #9'   ',
              #9#9#9#9#9,

              '        spaced 9 for tab',  // 33
              #9#9#9#9#9,

              ' X   ', // 35
              ' X'#9, // 36

              ''
              ]);

  end;

  procedure TestHome(Name:String; X, Y, ExpX1, ExpX2, ExpX3: Integer; ExpLTxTStartX: Integer = -1);
  begin
    SetCaretPhys(X, Y);
    synedit.CommandProcessor(ecLineStart, '', nil);
    TestIsCaretPhys(Name + '(1st home)', ExpX1,Y);
    synedit.CommandProcessor(ecLineStart, '', nil);
    TestIsCaretPhys(Name + '(2nd home)', ExpX2,Y);
    synedit.CommandProcessor(ecLineStart, '', nil);
    TestIsCaretPhys(Name + '(3rd home)', ExpX3,Y);
    if ExpLTxTStartX > 0 then begin
      SetCaretPhys(X, Y);
      synedit.CommandProcessor(ecLineTextStart, '', nil);
      TestIsCaretPhys(Name + '(1st line-text-start)', ExpLTxTStartX,Y);
      synedit.CommandProcessor(ecLineTextStart, '', nil);
      TestIsCaretPhys(Name + '(2nd line-text-start)', ExpLTxTStartX,Y);
      synedit.CommandProcessor(ecLineTextStart, '', nil);
      TestIsCaretPhys(Name + '(3rd line-text-start)', ExpLTxTStartX,Y);
    end;
  end;

  procedure TestEnd(Name:String; X, Y, ExpX1, ExpX2, ExpX3: Integer);
  begin
    SetCaretPhys(X, Y);
    synedit.CommandProcessor(ecLineEnd, '', nil);
    TestIsCaretPhys(Name + '(1st end)', ExpX1,Y);
    synedit.CommandProcessor(ecLineEnd, '', nil);
    TestIsCaretPhys(Name + '(2nd end)', ExpX2,Y);
    synedit.CommandProcessor(ecLineEnd, '', nil);
    TestIsCaretPhys(Name + '(3rd end)', ExpX3,Y);
  end;

begin
  // None Smart-Home:
  //   Caret goes x=1, then x=indend
  // Smart-Home:
  //   Caret goes x=indent, then 1,    IF AFTER indent, or at x=1
  //   Caret goes x=1, then x=indend,  IF at x=indent, or inside indent
  // Both
  //   Caret does not go past eol, unless explicitly enabled
  //   if spaces/tab exist at start of otherwise empty line, caret will go to prev-line indent

  {%region}
  DoInit1;
  PushBaseName('no smart / no past-eol');

  TestHome('empty 1st line',                      1, 1,  1,1,1,   1);
  TestHome('empty 2nd line',                      1, 2,  1,1,1,   1);

  TestHome('unindented line',                     1, 3,  1,1,1,   1);
  TestHome('unindented line x-in-line',           4, 3,  1,1,1,   1);
  TestHome('1st line after unindendet',           1, 4,  1,1,1,   1);
  TestHome('2nd line after unindendet',           1, 5,  1,1,1,   1);
  TestHome('3rd #32 line after unindendet',       2, 6,  1,1,1,   1);
  TestHome('4th #9  line after unindendet',       2, 8,  1,1,1,   1);

  TestHome('space indented line',                 1, 9,  4,1,4,   4);
  TestHome('space indented line x-after-indent',  4, 9,  1,4,1,   4); // go to indent, after absolute home
  TestHome('space indented line x-in-indent',     3, 9,  1,4,1,   4);
  TestHome('space indented line x-in-line',       6, 9,  1,4,1,   4);
  TestHome('1st after space indented line',       1,10,  1,1,1,   1);
  TestHome('2nd after space indented line',       1,11,  1,1,1,   1);
  TestHome('3rd #32 after space indented line',   2,12,  1,4,1,   1);
  TestHome('4th #9  after space indented line',   2,14,  1,4,1,   1);
  TestHome('#9  after long space indented line',  2,34,  1,9,1,   1);
  SyneDit.Options2 := SyneDit.Options2 + [eoCaretSkipTab];
  TestHome('4th #9  after space indented line  [skipT]',   7,14,  1,1,1,   1);
  TestHome('#9  after long space indented line [skipT]',   1,34,  7,1,7,   1);
  TestHome('#9  after long space indented line [skipT]',   7,34,  1,7,1,   1);
  TestHome('#9  after long space indented line [skipT]',  13,34,  1,7,1,   1);
  SyneDit.Options2 := SyneDit.Options2 - [eoCaretSkipTab];

  TestHome('tab indented line',                   1,15,  7,1,7,   7);
  TestHome('tab indented line x-after-indent',    7,15,  1,7,1,   7);
  TestHome('tab indented line x-in-indent',       5,15,  1,7,1,   7);
  TestHome('tab indented line x-in-line',         9,15,  1,7,1,   7);
  TestHome('1st after tab indented line',         1,16,  1,1,1,   1);
  TestHome('2nd after tab indented line',         1,17,  1,1,1,   1);
  TestHome('3rd #32 after tab indented line',     2,18,  1,7,1,   1);
  TestHome('4th #9  after tab indented line',     2,20,  1,7,1,   1);
  {%endregion}

  {%region}
  DoInit1;
  SyneDit.Options := [eoScrollPastEol];
  PopPushBaseName('no smart / past-eol');

  TestHome('empty 1st line',                      1, 1,  1,1,1,   1);
  TestHome('empty 2nd line',                      1, 2,  1,1,1,   1);

  TestHome('unindented line',                     1, 3,  1,1,1,   1);
  TestHome('unindented line x-in-line',           4, 3,  1,1,1,   1);
  TestHome('1st line after unindendet',           1, 4,  1,1,1,   1);
  TestHome('2nd line after unindendet',           1, 5,  1,1,1,   1);
  TestHome('3rd #32 line after unindendet',       2, 6,  1,1,1,   1);
  TestHome('4th #9  line after unindendet',       2, 8,  1,1,1,   1);

  TestHome('space indented line',                 1, 9,  4,1,4,   4);
  TestHome('space indented line x-after-indent',  4, 9,  1,4,1,   4); // go to indent, after absolute home
  TestHome('space indented line x-in-indent',     3, 9,  1,4,1,   4);
  TestHome('space indented line x-in-line',       6, 9,  1,4,1,   4);
  TestHome('1st after space indented line',       1,10,  4,1,4,   1);
  TestHome('1st after space indented line (x)',   2,10,  1,4,1,   1);
  TestHome('2nd after space indented line',       1,11,  4,1,4,   1);
  TestHome('3rd #32 after space indented line',   2,12,  1,4,1,   1);
  TestHome('4th #9  after space indented line',   2,14,  1,4,1,   1);
  TestHome('#9  after long space indented line',  2,34,  1,9,1,   1);
  SyneDit.Options2 := SyneDit.Options2 + [eoCaretSkipTab];
  TestHome('4th #9  after space indented line  [skipT]',   7,14,  1,1,1,   1);
  TestHome('#9  after long space indented line [skipT]',   1,34,  7,1,7,   1);
  TestHome('#9  after long space indented line [skipT]',   7,34,  1,7,1,   1);
  TestHome('#9  after long space indented line [skipT]',  13,34,  1,7,1,   1);
  SyneDit.Options2 := SyneDit.Options2 - [eoCaretSkipTab];

  TestHome('tab indented line',                   1,15,  7,1,7,   7);
  TestHome('tab indented line x-after-indent',    7,15,  1,7,1,   7);
  TestHome('tab indented line x-in-indent',       5,15,  1,7,1,   7);
  TestHome('tab indented line x-in-line',         9,15,  1,7,1,   7);
  TestHome('1st after tab indented line',         1,16,  7,1,7,   1);
  TestHome('1st after tab indented line(x)',      2,16,  1,7,1,   1);
  TestHome('2nd after tab indented line',         1,17,  7,1,7,   1);
  TestHome('3rd #32 after tab indented line',     2,18,  1,7,1,   1);
  TestHome('4th #9  after tab indented line',     2,20,  1,7,1,   1);
  {%endregion}


  {%region}
  DoInit1;
  SyneDit.Options := [eoEnhanceHomeKey];

  PopPushBaseName('smart home / no past-eol');

  TestHome('empty 1st line',                      1, 1,  1,1,1,   1);
  TestHome('empty 2nd line',                      1, 2,  1,1,1,   1);

  TestHome('unindented line',                     1, 3,  1,1,1,   1);
  TestHome('unindented line x-in-line',           4, 3,  1,1,1,   1);
  TestHome('1st line after unindendet',           1, 4,  1,1,1,   1);
  TestHome('2nd line after unindendet',           1, 5,  1,1,1,   1);
  TestHome('3rd #32 line after unindendet',       2, 6,  1,1,1,   1);
  TestHome('4th #9  line after unindendet',       2, 8,  1,1,1,   1);

  TestHome('space indented line',                 1, 9,  4,1,4,   4); // go to absolut home (x-=1), after indent
  TestHome('space indented line x-after-indent',  4, 9,  1,4,1,   4);
  TestHome('space indented line x-in-indent',     3, 9,  1,4,1,   4);
  TestHome('space indented line x-in-line',       6, 9,  4,1,4,   4);
  TestHome('1st after space indented line',       1,10,  1,1,1,   1);
  TestHome('2nd after space indented line',       1,11,  1,1,1,   1);
  TestHome('3rd #32 after space indented line',   2,12,  1,4,1,   1);
  TestHome('4th #9  after space indented line',   2,14,  1,4,1,   1);
  TestHome('#9  after long space indented line',  2,34,  1,9,1,   1);
  SyneDit.Options2 := SyneDit.Options2 + [eoCaretSkipTab];
  TestHome('4th #9  after space indented line  [skipT]',   7,14,  1,1,1,   1);
  TestHome('#9  after long space indented line [skipT]',   1,34,  7,1,7,   1);
  TestHome('#9  after long space indented line [skipT]',   7,34,  1,7,1,   1);
  TestHome('#9  after long space indented line [skipT]',  13,34,  7,1,7,   1);
  SyneDit.Options2 := SyneDit.Options2 - [eoCaretSkipTab];

  TestHome('tab indented line',                   1,15,  7,1,7,   7);
  TestHome('tab indented line x-after-indent',    7,15,  1,7,1,   7);
  TestHome('tab indented line x-in-indent',       5,15,  1,7,1,   7);
  TestHome('tab indented line x-in-line',         9,15,  7,1,7,   7);
  TestHome('1st after tab indented line',         1,16,  1,1,1,   1);
  TestHome('2nd after tab indented line',         1,17,  1,1,1,   1);
  TestHome('3rd #32 after tab indented line',     2,18,  1,7,1,   1);
  TestHome('3rd #32 after tab indented line',    11,18,  7,1,7,   1);
  TestHome('4th #9  after tab indented line',     2,20,  1,7,1,   1);
  {%endregion}


  {%region}
  DoInit1;
  SyneDit.Options := [eoEnhanceHomeKey, eoScrollPastEol];

  PopPushBaseName('smart home / past-eol');

  TestHome('empty 1st line',                      1, 1,  1,1,1,   1);
  TestHome('empty 2nd line',                      1, 2,  1,1,1,   1);

  TestHome('unindented line',                     1, 3,  1,1,1,   1);
  TestHome('unindented line x-in-line',           4, 3,  1,1,1,   1);
  TestHome('1st line after unindendet',           1, 4,  1,1,1,   1);
  TestHome('2nd line after unindendet',           1, 5,  1,1,1,   1);
  TestHome('3rd #32 line after unindendet',       2, 6,  1,1,1,   1);
  TestHome('4th #9  line after unindendet',       2, 8,  1,1,1,   1);

  TestHome('space indented line',                 1, 9,  4,1,4,   4); // go to absolut home (x-=1), after indent
  TestHome('space indented line x-after-indent',  4, 9,  1,4,1,   4);
  TestHome('space indented line x-in-indent',     3, 9,  1,4,1,   4);
  TestHome('space indented line x-in-line',       6, 9,  4,1,4,   4);
  TestHome('1st after space indented line',       1,10,  4,1,4,   1);
  TestHome('2nd after space indented line',       1,11,  4,1,4,   1);
  TestHome('3rd #32 after space indented line',   2,12,  1,4,1,   1);
  TestHome('4th #9  after space indented line',   2,14,  1,4,1,   1);
  TestHome('#9  after long space indented line',  2,34,  1,9,1,   1);
  SyneDit.Options2 := SyneDit.Options2 + [eoCaretSkipTab];
  TestHome('4th #9  after space indented line [skipT]',   7,14,  1,1,1,   1);
  TestHome('#9  after long space indented line [skipT]',  7,34,  1,7,1,   1);
  SyneDit.Options2 := SyneDit.Options2 - [eoCaretSkipTab];

  TestHome('tab indented line',                   1,15,  7,1,7,   7);
  TestHome('tab indented line x-after-indent',    7,15,  1,7,1,   7);
  TestHome('tab indented line x-in-indent',       5,15,  1,7,1,   7);
  TestHome('tab indented line x-in-line',         9,15,  7,1,7,   7);
  TestHome('1st after tab indented line',         1,16,  7,1,7,   1);
  TestHome('1st after tab indented line (x)',     2,16,  1,7,1,   1);
  TestHome('2nd after tab indented line',         1,17,  7,1,7,   1);
  TestHome('3rd #32 after tab indented line',     2,18,  1,7,1,   1);
  TestHome('3rd #32 after tab indented line',    11,18,  7,1,7,   1);
  TestHome('4th #9  after tab indented line',     2,20,  1,7,1,   1);
  {%endregion}

  PopPushBaseName('NO smart end/ NO past-eol');
  DoInit1;
  TestEnd ('empty 1st line',  1,1,  1,1,1);
  TestEnd ('end',       1,35,  6,3,6);
  TestEnd ('end tab',   1,36,  7,3,7);

  PopPushBaseName('smart end/ NO past-eol');
  DoInit1;
  SyneDit.Options2 := [eoEnhanceEndKey];
  TestEnd ('end',       1,35,  3,6,3);
  TestEnd ('end tab',   1,36,  3,7,3);

  //SyneDit.Options := [eoScrollPastEol, eoEnhanceHomeKey];
  //SyneDit.Options2 := [eoEnhanceEndKey];

end;

procedure TTestSynNavigation.TestCaretLeftRight;

  procedure SetTextLeftRight;
  begin
    ReCreateEdit;
    SetLines([ 'ああああああああああああああああああああああ',   //  1: Double width at odd pos (start at 1, then every 2)
               ' ああああああああああああああああああああああ',  //  2: Double width at even pos
               '',                                               //  3: empty
               #9#9#9#9#9#9#9#9,                                 //  4: tabs
               'abc'#9'def',                                   //  5: text+tabs
               'abc def üüü xyz',                              //  6: utf8 multibyte
               'abc def ü'#9'üü xyz',                          //  7: utf8 multibyte + tabs
               'abc あdefあ123',                               //  8: Double withs
               'abc あdef'#9'あ'#9'123',                       //  9: Double withs + tabs
               'Aa'#$CC#$81'B',                                // 10: a-accent, in 2 (combining) codepoints)
               ''
           ]);
  end;

  procedure TestLeftRight(AName: String; ALine: Integer; AStartPhysX: Integer; ACommand:TSynEditorCommand;
    ExpPhysX, ExpLogX: Integer; ExpRepeatPhysX: Integer = -1; ExpRepeatLogX: Integer = -1;
    DoReverse: Boolean = True; DoKeepXDWidth: Boolean = True;
    ASelectXEnd: Integer = -1; ExpCaretY: integer = -1);
  var
    Name2: String;
    TestLogFirst: Boolean;

    procedure DoCmd;
    begin
      if ASelectXEnd < 0
      then SetCaretPhys(AStartPhysX, ALine)
      else SetCaretAndSelPhys(ASelectXEnd, ALine, AStartPhysX, ALine);
      SynEdit.ExecuteCommand(ACommand, '', nil);
    end;
    procedure TestCmd;
    begin
      DoCmd;
      if not TestLogFirst then
        TestIsCaretPhys(Name2+'[1]', ExpPhysX, ExpCaretY);
      TestIsCaret(Name2+'[1]', ExpLogX, ExpCaretY);
      TestIsCaretPhys(Name2+'[1]', ExpPhysX, ExpCaretY);
    end;

    procedure DoCmdRepeat;
    begin
      if ExpRepeatPhysX < 0 then exit;
      SynEdit.ExecuteCommand(ACommand, '', nil);
    end;
    procedure TestCmdRepeat;
    begin
      if ExpRepeatPhysX < 0 then exit;
      DoCmdRepeat;
      if not TestLogFirst then
        TestIsCaretPhys(Name2+'[2]', ExpRepeatPhysX, ExpCaretY);
      if ExpRepeatLogX > 0 then
        TestIsCaret(Name2+'[2]', ExpRepeatLogX, ExpCaretY);
      TestIsCaretPhys(Name2+'[2]', ExpRepeatPhysX, ExpCaretY);
    end;

    procedure DoCmdReverse(AfterRepeat: Boolean = false);
    begin
      if not DoReverse then exit;
      if AfterRepeat and (ExpRepeatPhysX < 0) then exit; // was never repeated

      if ACommand = ecLeft
      then SynEdit.ExecuteCommand(ecRight, '', nil)
      else SynEdit.ExecuteCommand(ecLeft, '', nil);
    end;
    procedure TestCmdReverse(AfterRepeat: Boolean = false);
    begin
      if not DoReverse then exit;
      if AfterRepeat and (ExpRepeatPhysX < 0) then exit; // was never repeated

      DoCmdReverse(AfterRepeat);
      if AfterRepeat then begin
        if not TestLogFirst then
          TestIsCaretPhys(Name2+'[R,'+dbgs(AfterRepeat)+']', ExpPhysX, ExpCaretY);
        TestIsCaret(Name2, ExpLogX, ExpCaretY);
        TestIsCaretPhys(Name2+'[R,'+dbgs(AfterRepeat)+']', ExpPhysX, ExpCaretY);
      end else begin
        TestIsCaretPhys(Name2+'[R,'+dbgs(AfterRepeat)+']', AStartPhysX, ALine);
      end;
    end;

    procedure DoKeepXOnEmpty;
    begin
      if (ExpPhysX > 1) and not(eoScrollPastEol in SynEdit.Options) then begin
        SynEdit.CaretObj.LinePos := 3; // empty line
        SynEdit.CaretObj.LinePos := ExpCaretY;
      end;
    end;
    procedure TestKeepXOnEmpty(TestBetween: Boolean = False);
    begin
      if (ExpPhysX > 1) and not(eoScrollPastEol in SynEdit.Options) then begin
        SynEdit.CaretObj.LinePos := 3; // empty line
        if TestBetween then
          AssertEquals(Name2+'empty line at pos 1', 1, SynEdit.CaretObj.CharPos);
        SynEdit.CaretObj.LinePos := ExpCaretY;
        if eoKeepCaretX in SynEdit.Options
        then AssertEquals(Name2+'after empty line at pos 1', ExpPhysX, SynEdit.CaretObj.CharPos)
        else AssertEquals(Name2+'after empty line at pos 1', 1, SynEdit.CaretObj.CharPos);
      end;
    end;

    procedure DoKeepXOnDWidth;
    begin
      if (ExpPhysX > 1) and DoKeepXDWidth then begin
        if (ExpPhysX and 1) = 0
        then SynEdit.CaretObj.LinePos := 1  // double width odd
        else SynEdit.CaretObj.LinePos := 2; // double width even
        SynEdit.CaretObj.LinePos := ExpCaretY;
      end;
    end;
    procedure TestKeepXOnDWidth(TestBetween: Boolean = False);
    begin
      if (ExpPhysX > 1) and DoKeepXDWidth then begin
        if (ExpPhysX and 1) = 0
        then SynEdit.CaretObj.LinePos := 1  // double width odd
        else SynEdit.CaretObj.LinePos := 2; // double width even
        if TestBetween then
          AssertFalse(Name2+'after dwidth line at pos 1', ExpPhysX = SynEdit.CaretObj.CharPos);
        SynEdit.CaretObj.LinePos := ExpCaretY;
        AssertTrue(Name2+'after dwidth line at pos 1',
                   (ExpPhysX = SynEdit.CaretObj.CharPos) = (eoKeepCaretX in SynEdit.Options));
      end;
    end;

  var
    s: string;
  begin
    if ExpCaretY < 0 then ExpCaretY := ALine;

    EditorCommandToIdent(ACommand, s{%H-});
    AName := Format('%s (%s Y=%d, X=%d) ', [AName, s, ALine, AStartPhysX]);

    {%region NO eoKeepCaretX}
      SynEdit.Options := SynEdit.Options - [eoKeepCaretX];
      Name2 := AName + 'NO eoKeepCaretX: ';

      TestLogFirst := False;

      TestCmd;
      TestCmdReverse;

      TestCmd;
      TestCmdRepeat;
      TestCmdReverse(True);
      TestCmdReverse;

      DoCmd;
      TestCmdReverse;

      DoCmd;
      DoCmdRepeat;
      TestCmdReverse(True);
      TestCmdReverse;

      TestLogFirst := True;

      TestCmd;
      TestCmdReverse;

      TestCmd;
      TestCmdRepeat;
      TestCmdReverse(True);
      TestCmdReverse;

      TestCmd;
      TestKeepXOnEmpty(False);

      TestCmd;
      TestKeepXOnEmpty(True);

      TestCmd;
      TestKeepXOnDWidth(False);

      TestCmd;
      TestKeepXOnDWidth(True);

    {%endregion NO eoKeepCaretX}


    {%region WITH eoKeepCaretX}
      SynEdit.Options := SynEdit.Options + [eoKeepCaretX];
      Name2 := Name2 + 'WITH eoKeepCaretX: ';

      TestLogFirst := False;

      TestCmd;
      TestCmdReverse;

      TestCmd;
      TestCmdRepeat;
      TestCmdReverse(True);
      TestCmdReverse;

      DoCmd;
      TestCmdReverse;

      DoCmd;
      DoCmdRepeat;
      TestCmdReverse(True);
      TestCmdReverse;

      TestLogFirst := True;

      TestCmd;
      TestCmdReverse;

      TestCmd;
      TestCmdRepeat;
      TestCmdReverse(True);
      TestCmdReverse;

      TestCmd;
      TestKeepXOnEmpty(False);
      if ASelectXEnd < 0 then
        TestCmdReverse;

      TestCmd;
      TestKeepXOnEmpty(True);
      if ASelectXEnd < 0 then
        TestCmdReverse;

      TestCmd;
      TestKeepXOnEmpty(False);
      TestCmdRepeat;

      TestCmd;
      TestKeepXOnDWidth(False);
      if ASelectXEnd < 0 then
        TestCmdReverse;

      TestCmd;
      TestKeepXOnDWidth(True);
      if ASelectXEnd < 0 then
        TestCmdReverse;

      TestCmd;
      TestKeepXOnDWidth(False);
      TestCmdRepeat;

    {%endregion WITH eoKeepCaretX}

  end;

begin
  // TODO:
  //  EOL/BOL at first/last (visible) line (with folding)
  //  Multybyte char at EOL
  SetTextLeftRight;

  {%region ecRight}
  PushBaseName('No opts');

  SynEdit.TabWidth := 6;
  SynEdit.MaxLeftChar := 200;
  SynEdit.Options := [];
  SynEdit.Options2 := [];

  TestLeftRight('Simple right',        5,  2, ecRight,  3, 3,    4, 4);

  TestLeftRight('After tab right',     5,  7, ecRight,  8, 6,    9, 7);
  TestLeftRight('Before tab right',    5,  4, ecRight,  5, 4,    6, 4);
  TestLeftRight('Mid tab right',       5,  5, ecRight,  6, 4,    7, 5);
  TestLeftRight('tab to tab right',    4,  6, ecRight,  7, 2,    8, 2);

  TestLeftRight('Umlaut right',        6,  9, ecRight, 10,11,   11,13);
  TestLeftRight('Double W right',      8,  5, ecRight,  7, 8,    8, 9);
  TestLeftRight('combining right',    10,  2, ecRight,  3, 5,    4, 6);

  TestLeftRight('at EOL right',        5, 10, ecRight,  1, 1,    2, 2, True, True, -1 , 6);

  PopPushBaseName('eoScrollPastEol');
  SynEdit.Options := [eoScrollPastEol];

  TestLeftRight('at EOL right',        5, 10, ecRight,  11, 9,   12,10);
  TestLeftRight('at Max-left right',   5,199, ecRight, 200,198,   200,198,   False, False);


  PopPushBaseName('eoCaretSkipTab');
  SynEdit.Options := [];
  SynEdit.Options2 := [eoCaretSkipTab];

  TestLeftRight('Simple right',        5,  2, ecRight,  3, 3,    4, 4);

  TestLeftRight('After tab right',     5,  7, ecRight,  8, 6,    9, 7);
  TestLeftRight('Before tab right',    5,  4, ecRight,  7, 5,    8, 6);
  TestLeftRight('tab to tab right',    4,  7, ecRight, 13, 3,   19, 4);


  PopPushBaseName('eoCaretSkipsSelection');
  SynEdit.Options2 := [eoCaretSkipsSelection];

  TestLeftRight('Selected right',      5,  2, ecRight,  8, 6,   -1, -1,   True, False, 8);

  {%region ecRight}

  {%region ecLeft}
  SynEdit.Options := [];
  SynEdit.Options2 := [];

  TestLeftRight('Simple left',        5,  3, ecLeft,  2, 2,    1, 1);

  TestLeftRight('After tab left',     5,  7, ecLeft,  6, 4,    5, 4);
  TestLeftRight('Before tab left',    5,  4, ecLeft,  3, 3,    2, 2);
  TestLeftRight('Mid tab left',       5,  5, ecLeft,  4, 4,    3, 3);
  TestLeftRight('tab to tab left',    4,  8, ecLeft,  7, 2,    6, 1);

  TestLeftRight('Umlaut left',        6, 10, ecLeft,  9, 9,    8, 8);
  TestLeftRight('Double W left',      8,  7, ecLeft,  5, 5,    4, 4);
  TestLeftRight('combining left',    10,  3, ecLeft,  2, 2,    1, 1);

  TestLeftRight('at BOL left',       6,  1, ecLeft, 10, 8,    9, 7, True, True, -1 , 5);

  PopPushBaseName('eoScrollPastEol');
  SynEdit.Options := [eoScrollPastEol];

  TestLeftRight('at BOL left',        6,  1, ecLeft,  1, 1,   1, 1,   False, False);


  PopPushBaseName('eoCaretSkipTab');
  SynEdit.Options := [];
  SynEdit.Options2 := [eoCaretSkipTab];

  TestLeftRight('Simple left',        5,  3, ecLeft,  2, 2,    1, 1);

  TestLeftRight('After tab left',     5,  7, ecLeft,  4, 4,    3, 3);
  TestLeftRight('tab to tab left',    4, 13, ecLeft,  7, 2,    1, 1);


  PopPushBaseName('eoCaretSkipsSelection');
  SynEdit.Options2 := [eoCaretSkipsSelection];

  TestLeftRight('Selected left',      5,  8, ecLeft,  3, 3,   -1, -1,   True, False, 3);

  {%endregion ecLeft}


  PopBaseName;
end;

procedure TTestSynNavigation.TestCaretMoveLeftRightWord;

  procedure DoInit;
  begin
    ReCreateEdit;

    SynEdit.TabWidth := 7;
            // 1    6    11 14
    SetLines(['Some text to test',                 //1
            // 1   5   9
              'Foo bar abc',                       // 2
            //   8     14   19   24
              #9'Other line with tab',             // 3
            // 1      8  11  15
              'tab'#9'in the middle',              // 4
            // 1       9  12  16     23   28
              'tab'#9' in the middle with space',  // 5
            // 1       9   13 16
              'umlaute äää in text',               // 6
              'normal line',
            //    4         14     21     28  32
              '   untrimmed spaces around line   ', // 8
              'normal line',
            //   8      15      22  26
              #9'tab'#9'only'#9'line'#9,            // 10
            // 1      8
              'normal line',
              '',                                    // 12 (empty)
              'normal line',
              '     ', // space only empty line      // 14
              'normal line',
              '',
              'A B',                 // 17
              'normal line',         // 18
              ''
             ]);
  end;

  procedure TestWordLeft(Name:String; X, Y, ExpX1, ExpY1: Integer;
                         ExpX2: Integer = -1; ExpY2: Integer = -1);
  begin
    SetCaretPhys(X,Y);
    SynEdit.CommandProcessor(ecWordLeft, '', nil);
    TestIsCaretPhys(Name + '(1st WordLeft)', ExpX1, ExpY1);
    if ExpY2 > 0 then begin
      SynEdit.CommandProcessor(ecWordLeft, '', nil);
      TestIsCaretPhys(Name + '(2nd WordLeft)', ExpX2, ExpY2);
    end;
  end;

  procedure TestWordRight(Name:String; X, Y, ExpX1, ExpY1: Integer;
                         ExpX2: Integer = -1; ExpY2: Integer = -1);
  begin
    SetCaretPhys(X,Y);
    SynEdit.CommandProcessor(ecWordRight, '', nil);
    TestIsCaretPhys(Name + '(1st WordRight)', ExpX1, ExpY1);
    if ExpY2 > 0 then begin
      SynEdit.CommandProcessor(ecWordRight, '', nil);
      TestIsCaretPhys(Name + '(2nd WordRight)', ExpX2, ExpY2);
    end;
  end;

begin
  DoInit;
  {%region word left}
  TestWordLeft('simple "te|st"',                   16, 1,   14, 1,  11, 1);
  TestWordLeft('simple EOW "test|"',               18, 1,   14, 1,  11, 1);
  TestWordLeft('simple BOW "|test"',               14, 1,   11, 1,  6, 1);
  TestWordLeft('simple > BOT "So|me"',              3, 1,    1, 1,   1, 1);
  TestWordLeft('simple > prev-line "F|oo"',         2, 2,    1, 2,  18, 1);
  TestWordLeft('simple > prev-line "|Foo"',         1, 2,   18, 1,  14, 1);

  TestWordLeft('tab "wi|th"',                      21, 3,   19, 3,  14, 3);
  TestWordLeft('tab EOW "with|"',                  23, 3,   19, 3,  14, 3);
  TestWordLeft('tab BOW "|with"',                  19, 3,   14, 3,   8, 3);
  TestWordLeft('tab > prev-line "O|ther"',          9, 3,    8, 3,  12, 2);

  TestWordLeft('M-tab "i|n"',                       9, 4,    8, 4,   1, 4);
  TestWordLeft('M-tab > prev-line-tab "ta|b"',      3, 4,    1, 4,  27, 3);

  TestWordLeft('M-S-tab "i|n"',                    10, 5,    9, 5,   1, 5);
  TestWordLeft('M-S-EOW tab "in|"',                11, 5,    9, 5,   1, 5);
  TestWordLeft('M-S-BOW tab "|in"',                 9, 5,    1, 5,  21, 4);
  TestWordLeft('M-S-tab "#9| in"',                  8, 5,    1, 5);

  TestWordLeft('Umlaut "ää|ä"',                    11, 6,    9, 6,   1, 6);
  TestWordLeft('Umlaut EOW "äää|"',                12, 6,    9, 6,   1, 6);
  TestWordLeft('Umlaut BOW "|äää"',                 9, 6,    1, 6,  33, 5);
  TestWordLeft('Umlaut "i|n"',                     14, 6,   13, 6,   9, 6);
  TestWordLeft('After Umlaut > prev line',          1, 7,   20, 6,  16, 6);

  TestWordLeft('untrimmed "un|trimmed"',            6, 8,    4, 8,  12, 7);
  TestWordLeft('After untrimmed > prev',            1, 9,   35, 8,  28, 8);
  TestWordLeft('untrimmed tab "t|ab"',              9,10,    8,10,  12, 9);
  TestWordLeft('After untrimmed tab > prev',        1,11,   29,10,  22,10);

  TestWordLeft('After empty > prev',                1,13,    1,12);
  TestWordLeft('After space empty > prev',          1,15,    6,14);

  TestWordLeft('single char at eol "A B|"',         4,17,    3,17,   1,17);
  {%endregion}

  {%region word right}
  TestWordRight('simple "te|xt"',                    8, 1,   11, 1,  14, 1);
  TestWordRight('simple EOW "text|"',               10, 1,   11, 1,  14, 1);
  TestWordRight('simple BOW "|text"',                6, 1,   11, 1,  14, 1);
  TestWordRight('simple EOT "li|ne"',               10,18,   12,18,  12,18);
  TestWordRight('simple > EOL, next line "te|st"',  16, 1,   18, 1,   1, 2);

  TestWordRight('tab "li|ne"',                      16, 3,   19, 3,  24, 3);
  TestWordRight('tab EOW "line|"',                  18, 3,   19, 3,  24, 3);
  TestWordRight('tab BOW "|line"',                  14, 3,   19, 3,  24, 3);
  TestWordRight('tab > EOL, next-line',             25, 3,   27, 3,   1, 4);

  TestWordRight('M-tab "t|ab"',                      2, 4,    8, 4,  11, 4);
  TestWordRight('M-tab "tab|"',                      4, 4,    8, 4,  11, 4);
  TestWordRight('M-tab > EOL, next-line-tab',       17, 4,   21, 4,   1, 5);

  TestWordRight('M-S-tab BOW "t|ab"',                2, 5,    9, 5,  12, 5);
  TestWordRight('M-S-tab EOW "tab|"',                4, 5,    9, 5,  12, 5);
  TestWordRight('M-S-tab BOW "|tab"',                1, 5,    9, 5,  12, 5);
  TestWordRight('M-S-tab "tab#9| "',                 5, 5,    9, 5,  12, 5);

  TestWordRight('Umlaut "ää|ä"',                    11, 6,   13, 6,  16, 6);
  TestWordRight('Umlaut EOW "äää|"',                12, 6,   13, 6,  16, 6);
  TestWordRight('Umlaut BOW "|äää"',                 9, 6,   13, 6,  16, 6);
  TestWordRight('Umlaut "um|laute"',                 2, 6,    9, 6,  13, 6);
  TestWordRight('After Umlaut > EOL, next line',    18, 6,   20, 6,   1, 7);

  TestWordRight('Before untrimmed > next',           12, 7,    4, 8,  14, 8);
  TestWordRight('untrimmed > EOL, next',             30, 8,   35, 8,   1, 9);
  TestWordRight('Before untrimmed tab > next',       12, 9,    8,10,  15,10);
  TestWordRight('untrimmed tab > EOL, next',         24,10,   29,10,   1,11);

  TestWordRight('Before empty > next',                12,11,    1,12);
  TestWordRight('Before space empty > next',          12,13,    1,14,   6, 14);
  {%endregion}
end;

initialization

  RegisterTest(TTestSynNavigation);
end.

