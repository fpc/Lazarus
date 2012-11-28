unit TestScriptProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, EMScriptMacro, fpcunit, testutils, testregistry;

type

  { TTestCase1 }

  TTestCase1 = class(TTestCase)
  private
    FTestSyn: TSynEdit;
    FTestMacro: TEMSEditorMacro;

    procedure DoTestSimple(AName, AStartText, AMacroText, AExpect: String;
      Ax: Integer = 1; Ay: Integer = 1;  ExpX: Integer = -1; ExpY: Integer = -1;
      AExpIsPart: Boolean = True);
  published
    procedure TestProcs;
    procedure TestInteractiv;
  end;

implementation

function t(lines: array of string): String;
var
  i: Integer;
begin
  Result := '';
  for i := low(lines) to high (lines) do Result := Result + lines[i] + LineEnding;
end;

procedure TTestCase1.DoTestSimple(AName, AStartText, AMacroText, AExpect: String; Ax: Integer;
  Ay: Integer; ExpX: Integer; ExpY: Integer; AExpIsPart: Boolean);
begin
  if pos ('end.', AMacroText) < 1 then
    AMacroText := 'begin' + LineEnding + AMacroText + LineEnding + 'end.';
  FTestSyn.Text := AStartText;
  FTestSyn.CaretXY := Point(aX, AY);
  FTestMacro.SetFromSource(AMacroText);
  AssertTrue(AName+' Macro is valid: ' +FTestMacro.ErrorMsg, not FTestMacro.IsInvalid);
  FTestMacro.PlaybackMacro(FTestSyn);
  if AExpIsPart
  then AssertTrue(AName+' contains: ' + AExpect + ' IN ' + FTestSyn.Text, pos(AExpect, FTestSyn.Text) > 0)
  else AssertEquals(AName+' equals: ' + AExpect + ' IN ' + FTestSyn.Text, AExpect, FTestSyn.Text);
  if ExpX > 0 then begin
    AssertEquals(AName+ ' Carety: ', Expy, FTestSyn.CaretXY.y);
    AssertEquals(AName+ ' CaretX: ', ExpX, FTestSyn.CaretXY.x);
  end;
end;

procedure TTestCase1.TestProcs;
begin
  FTestSyn := TSynEdit.Create(nil);
  FTestMacro := TEMSEditorMacro.Create(nil);
  try
    {%region Text / point / ecXXX *}

    // ecChar
    DoTestSimple('ecChar',              '', 'ecChar(''C'');', 'C');

    // InsertTextAtCaret
    DoTestSimple('InsertTextAtCaret',   '',
                 'Caller.InsertTextAtCaret(''Foo'', scamEnd);',
                 'Foo');
    DoTestSimple('InsertTextAtCaret 2', 'SomeBar',
                 'Caller.InsertTextAtCaret(''Foo'', scamEnd);',
                 'SomeFooBar',  5,1,  8,1);
    DoTestSimple('InsertTextAtCaret 2', 'SomeBar',
                 'Caller.InsertTextAtCaret(''Foo'', scamBegin);',
                 'SomeFooBar',  5,1,  5,1);

    // point
    DoTestSimple('point()', '',
                 'var p: TPoint; begin'+LineEnding+
                 '  p := point(4,2);'+LineEnding+
                 '  Caller.InsertTextAtCaret(inttostr(p.x)+'',''+inttostr(p.y), scamBegin);'+LineEnding+
                 'end.',
                 '4,2');

    // TextBetweenPoints
    DoTestSimple('TextBetweenPoints',   'SomeBar',
                 'Caller.TextBetweenPoints[Point(3,1), point(5,1)] :=  ''ng'';',
                 'SongBar');
    DoTestSimple('TextBetweenPoints 2', t(['Bar', 'aXY']),
                 'var s: string; begin'+LineEnding+
                 '  s := Caller.TextBetweenPoints[Point(2,2), point(4,2)];'+LineEnding+
                 '  Caller.TextBetweenPoints[Point(5,1), point(5,1)] :=  s;'+LineEnding+
                 'end.',
                 'Bar XY');

    DoTestSimple('SetTextBetweenPoints',   'SomeBar',
                 'Caller.SetTextBetweenPoints(Point(3,1), point(5,1), ''ng'', [], scamEnd, smaKeep, smNormal);',
                 'SongBar',  1,1,  5,1);
    DoTestSimple('SetTextBetweenPoints scamBegin',   'SomeBar',
                 'Caller.SetTextBetweenPoints(Point(3,1), point(5,1), ''ng'', [], scamBegin, smaKeep, smNormal);',
                 'SongBar',  1,1,  3,1);
    DoTestSimple('SetTextBetweenPoints setSelect',   'SomeBar',
                 'Caller.SetTextBetweenPoints(Point(3,1), point(5,1), ''ng'', [setSelect], scamEnd, smaKeep, smNormal);',
                 'SongBar');
    AssertEquals('SetTextBetweenPoints setSelect', 'ng', FTestSyn.SelText);

    // LineAtCaret
    DoTestSimple('LineAtCaret',   t(['1', 'Bar', 'abc 123']),
                 'Caller.TextBetweenPoints[Point(1,2), point(1,2)] := Caller.LineAtCaret',
                 'abc 123Bar',  2,3);

    // Lines[]
    DoTestSimple('Lines[2]',   t(['1', 'Bar', 'abc 123']),
                 'Caller.TextBetweenPoints[Point(1,1), point(1,1)] := Caller.Lines[2]',
                 'abc 1231');
    DoTestSimple('Lines[1]',   t(['1', 'Bar', 'abc 123']),
                 'Caller.TextBetweenPoints[Point(1,1), point(1,1)] := Caller.Lines[1]',
                 'Bar1');

    {%endregion Text / point / ecXXX *}


    {%region Caret *}
    // CaretXY
    DoTestSimple('CaretXY read',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'var p: TPoint; begin ' + LineEnding +
                 'p := caller.CaretXY;' + LineEnding +
                 'Caller.InsertTextAtCaret(IntToStr(p.x)+'',''+IntToStr(p.y), scamEnd);' + LineEnding +
                 'end.',
                 '3,2',
                 3,2
                 );

    DoTestSimple('CaretXY write',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'begin ' + LineEnding +
                 'caller.CaretXY := Point(5,2);' + LineEnding +
                 'Caller.InsertTextAtCaret(''XX'', scamEnd);' + LineEnding +
                 'end.',
                 'XXFoo',
                 1,1
                 );

    // CaretX, CaretY
    DoTestSimple('CaretX read',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'begin ' + LineEnding +
                 'Caller.InsertTextAtCaret(IntToStr(caller.CaretX)+'',''+IntToStr(caller.CaretY), scamEnd);' + LineEnding +
                 'end.',
                 '3,2',
                 3,2
                 );

    DoTestSimple('CaretX write',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'begin ' + LineEnding +
                 'caller.CaretX := 5;' + LineEnding +
                 'caller.CaretY := 2;' + LineEnding +
                 'Caller.InsertTextAtCaret(''XX'', scamEnd);' + LineEnding +
                 'end.',
                 'XXFoo',
                 1,1
                 );

    // LogicalCaretXY
    DoTestSimple('LogicalCaretXY read',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'var p: TPoint; begin ' + LineEnding +
                 'p := caller.LogicalCaretXY;' + LineEnding +
                 'Caller.InsertTextAtCaret(IntToStr(p.x)+'',''+IntToStr(p.y), scamEnd);' + LineEnding +
                 'end.',
                 '4,2',
                 3,2
                 );

    DoTestSimple('LogicalCaretXY write',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'begin ' + LineEnding +
                 'caller.LogicalCaretXY := Point(6,2);' + LineEnding +
                 'Caller.InsertTextAtCaret(''XX'', scamEnd);' + LineEnding +
                 'end.',
                 'XXFoo',
                 1,1
                 );

    // LogicalCaretX
    DoTestSimple('LogicalCaretX read',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'begin ' + LineEnding +
                 'Caller.InsertTextAtCaret(IntToStr(caller.LogicalCaretX)+'',''+IntToStr(caller.CaretY), scamEnd);' + LineEnding +
                 'end.',
                 '4,2',
                 3,2
                 );

    DoTestSimple('LogicalCaretX write',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'begin ' + LineEnding +
                 'caller.CaretY := 2;' + LineEnding +
                 'caller.LogicalCaretX := 6;' + LineEnding +
                 'Caller.InsertTextAtCaret(''XX'', scamEnd);' + LineEnding +
                 'end.',
                 'XXFoo',
                 1,1
                 );

    // MoveCaretIgnoreEOL
    DoTestSimple('MoveCaretIgnoreEOL',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'begin ' + LineEnding +
                 'Caller.MoveCaretIgnoreEOL(Point(9,2));' + LineEnding +
                 'Caller.InsertTextAtCaret(''XX'', scamEnd);' + LineEnding +
                 'end.',
                 'Foo XX',
                 1,1
                 );

    // MoveLogicalCaretIgnoreEOL
    DoTestSimple('MoveLogicalCaretIgnoreEOL',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'begin ' + LineEnding +
                 'Caller.MoveLogicalCaretIgnoreEOL(Point(10,2));' + LineEnding +
                 'Caller.InsertTextAtCaret(''XX'', scamEnd);' + LineEnding +
                 'end.',
                 'Foo XX',
                 1,1
                 );

    {%endregion Caret*}


    {%region Selection *}
    {%endregion Selection *}


    {%region Logical / Physical *}
    {%endregion Logical / Physical *}


    {%region Search *}
    {%endregion Search *}


    {%region Clipboard *}
    {%endregion Clipboard *}


  finally
    FTestMacro.Free;
    FTestSyn.Free;
  end;
end;

procedure TTestCase1.TestInteractiv;
begin
  FTestSyn := TSynEdit.Create(nil);
  FTestMacro := TEMSEditorMacro.Create(nil);
  try

    DoTestSimple('ShowMessage',   'Y',
                 'ShowMessage(''Hello World'')',
                 'Y');

    DoTestSimple('ShowMessagePos',   'Y',
                 'ShowMessagePos(''I am at 10,10'', 10, 10)',
                 'Y');

    DoTestSimple('MessageDlg',   '',
                 'if MessageDlg(''Please press ok'', mtConfirmation, [mbOK, mbCancel], 0) = mrOk' + LineEnding +
                 'then Caller.InsertTextAtCaret(''Y'', scamEnd)' + LineEnding +
                 'else Caller.InsertTextAtCaret(''N'', scamEnd);',
                 'Y');

    DoTestSimple('MessageDlgPos',   '',
                 'if MessageDlgPos(''Am I at x=10,y=10? Please press CANCEL if I am'', mtConfirmation, [mbOK, mbCancel], 0, 10,10) = mrCancel' + LineEnding +
                 'then Caller.InsertTextAtCaret(''Y'', scamEnd)' + LineEnding +
                 'else Caller.InsertTextAtCaret(''N'', scamEnd);',
                 'Y');

    DoTestSimple('MessageDlgPosHelp',   '',
                 'if MessageDlgPosHelp(''Am I at x=50,y=50? Please press YES if I am'', mtConfirmation, [mbOK, mbCancel, mbYes], 0, 50,50,'''') = mrYes' + LineEnding +
                 'then Caller.InsertTextAtCaret(''Y'', scamEnd)' + LineEnding +
                 'else Caller.InsertTextAtCaret(''N'', scamEnd);',
                 'Y');

    DoTestSimple('InputBox',   '',
                 'var s: string; begin '+ LineEnding +
                 's := ''replace me'';' + LineEnding +
                 's := InputBox(''Need Input'', ''Please enter 123'', s);' + LineEnding +
                 'Caller.InsertTextAtCaret(s, scamEnd);'+ LineEnding +
                 'end.',
                 '123');

    DoTestSimple('InputQuery Ok',   '',
                 'var s: string; begin '+ LineEnding +
                 's := ''replace me AGAIN'';' + LineEnding +
                 'if InputQuery(''Need Input'', ''Plese enter 123 and press ok'', s)' + LineEnding +
                 'then Caller.InsertTextAtCaret(s, scamEnd);' + LineEnding +
                 'end.',
                 '123');

    DoTestSimple('InputQuery Cancel',   '',
                 'var s: string; begin '+ LineEnding +
                 's := ''do NOT replace me'';' + LineEnding +
                 'if not InputQuery(''Need Input'', ''Please press CANCEL'', s)' + LineEnding +
                 'then Caller.InsertTextAtCaret(s, scamEnd);' + LineEnding +
                 'end.',
                 'do NOT replace me');


  finally
    FTestMacro.Free;
    FTestSyn.Free;
  end;
end;



initialization

  RegisterTest(TTestCase1);
end.

