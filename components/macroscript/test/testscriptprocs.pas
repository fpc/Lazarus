unit TestScriptProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, EMScriptMacro, fpcunit, testutils, testregistry;

type

  { TTestCase1 }

  TTestCase1 = class(TTestCase)
  published
    procedure TestProcs;
  end;

implementation

procedure TTestCase1.TestProcs;
var
  TestSyn: TSynEdit;
  TestMacro: TEMSEditorMacro;

  function t(lines: array of string): String;
  var
    i: Integer;
  begin
    Result := '';
    for i := low(lines) to high (lines) do Result := Result + lines[i] + LineEnding;
  end;

  procedure DoTestSimple(AName, AStartText, AMacroText, AExpect: String;
    Ax: Integer = 1; Ay: Integer = 1;  ExpX: Integer = -1; ExpY: Integer = -1;
    AExpIsPart: Boolean = True);
  begin
    if pos ('end.', AMacroText) < 1 then
      AMacroText := 'begin' + LineEnding + AMacroText + LineEnding + 'end.';
    TestSyn.Text := AStartText;
    TestSyn.CaretXY := Point(aX, AY);
    TestMacro.SetFromSource(AMacroText);
    AssertTrue(AName+' Macro is valid: ' +TestMacro.ErrorMsg, not TestMacro.IsInvalid);
    TestMacro.PlaybackMacro(TestSyn);
    if AExpIsPart
    then AssertTrue(AName+' contains: ' + AExpect + ' IN ' + TestSyn.Text, pos(AExpect, TestSyn.Text) > 0)
    else AssertEquals(AName+' equals: ' + AExpect + ' IN ' + TestSyn.Text, AExpect, TestSyn.Text);
    if ExpX > 0 then begin
      AssertEquals(AName+ ' Carety: ', Expy, TestSyn.CaretXY.y);
      AssertEquals(AName+ ' CaretX: ', ExpX, TestSyn.CaretXY.x);
    end;
  end;
begin
  TestSyn := TSynEdit.Create(nil);
  TestMacro := TEMSEditorMacro.Create(nil);
  try
    DoTestSimple('ecChar',              '', 'ecChar(''C'');', 'C');

    DoTestSimple('InsertTextAtCaret',   '',
                 'Caller.InsertTextAtCaret(''Foo'', scamEnd);',
                 'Foo');
    DoTestSimple('InsertTextAtCaret 2', 'SomeBar',
                 'Caller.InsertTextAtCaret(''Foo'', scamEnd);',
                 'SomeFooBar',  5,1,  8,1);
    DoTestSimple('InsertTextAtCaret 2', 'SomeBar',
                 'Caller.InsertTextAtCaret(''Foo'', scamBegin);',
                 'SomeFooBar',  5,1,  5,1);

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
    AssertEquals('SetTextBetweenPoints setSelect', 'ng', TestSyn.SelText);

    DoTestSimple('LineAtCaret',   t(['1', 'Bar', 'abc 123']),
                 'Caller.TextBetweenPoints[Point(1,2), point(1,2)] := Caller.LineAtCaret',
                 'abc 123Bar',  2,3);

    DoTestSimple('Lines[2]',   t(['1', 'Bar', 'abc 123']),
                 'Caller.TextBetweenPoints[Point(1,1), point(1,1)] := Caller.Lines[2]',
                 'abc 1231');
    DoTestSimple('Lines[1]',   t(['1', 'Bar', 'abc 123']),
                 'Caller.TextBetweenPoints[Point(1,1), point(1,1)] := Caller.Lines[1]',
                 'Bar1');


  finally
    TestMacro.Free;
    TestSyn.Free;
  end;
end;



initialization

  RegisterTest(TTestCase1);
end.

