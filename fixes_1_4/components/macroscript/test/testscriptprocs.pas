unit TestScriptProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, EMScriptMacro, EMSSelfTest, Controls, Dialogs,
  Clipbrd, fpcunit, testutils, testregistry;

type

  { TTestCase1 }

  TTestCase1 = class(TTestCase)
  private
    FTestSyn: TSynEdit;
    FTestMacro: TEMSEditorMacro;

    procedure DoTestSimple(AName, AStartText, AMacroText, AExpect: String;
      Ax: Integer = 1; Ay: Integer = 1;  ExpX: Integer = -1; ExpY: Integer = -1;
      AExpIsPart: Boolean = True; ABlockEndX: Integer = -1; ABlockEndY: Integer = -1);
  published
    procedure TestBasics;
    procedure TestSynProcs;
    procedure TestSelfTest;
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
  Ay: Integer; ExpX: Integer; ExpY: Integer; AExpIsPart: Boolean; ABlockEndX: Integer;
  ABlockEndY: Integer);
begin
  if pos ('end.', AMacroText) < 1 then
    AMacroText := 'begin' + LineEnding + AMacroText + LineEnding + 'end.';
  FTestSyn.Text := AStartText;
  FTestSyn.CaretXY := Point(aX, AY);
  if ABlockEndY > 0 then begin
    FTestSyn.BlockBegin := FTestSyn.LogicalCaretXY;
    FTestSyn.BlockEnd := Point(ABlockEndX, ABlockEndY);
  end;
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

procedure TTestCase1.TestBasics;
begin
  FTestSyn := TSynEdit.Create(nil);
  FTestMacro := TEMSelfTestEditorMacro.Create(nil);
  try
    DoTestSimple('SizeOf(TPoint)',   '',
                 'var p: TPoint; begin if SizeOf(p) = ' +IntToStr(SizeOf(TPoint)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd); end.',
                 'Y'
                 );

    DoTestSimple('mrNone',   '',
                 'if mrNone = ' +IntToStr(mrNone) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );

    DoTestSimple('mrOk',   '',
                 'if mrOk = ' +IntToStr(mrOk) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );


    DoTestSimple('mtWarning',   '',
                 'if test_ord_mt(mtWarning) = ' +IntToStr(ord(mtWarning)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );
    DoTestSimple('mtError',   '',
                 'if test_ord_mt(mtError) = ' +IntToStr(ord(mtError)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );
    DoTestSimple('mtInformation',   '',
                 'if test_ord_mt(mtInformation) = ' +IntToStr(ord(mtInformation)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );
    DoTestSimple('mtConfirmation',   '',
                 'if test_ord_mt(mtConfirmation) = ' +IntToStr(ord(mtConfirmation)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );
    DoTestSimple('mtCustom',   '',
                 'if test_ord_mt(mtCustom) = ' +IntToStr(ord(mtCustom)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );


    DoTestSimple('mbYes',   '',
                 'if test_ord_mb(mbYes) = ' +IntToStr(ord(mbYes)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );
    DoTestSimple('mbNo',   '',
                 'if test_ord_mb(mbNo) = ' +IntToStr(ord(mbNo)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );
    DoTestSimple('mbOk',   '',
                 'if test_ord_mb(mbOk) = ' +IntToStr(ord(mbOk)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );
    DoTestSimple('mbCancel',   '',
                 'if test_ord_mb(mbCancel) = ' +IntToStr(ord(mbCancel)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );
    DoTestSimple('mbAbort',   '',
                 'if test_ord_mb(mbAbort) = ' +IntToStr(ord(mbAbort)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );
    DoTestSimple('mbRetry',   '',
                 'if test_ord_mb(mbRetry) = ' +IntToStr(ord(mbRetry)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );
    DoTestSimple('mbIgnore',   '',
                 'if test_ord_mb(mbIgnore) = ' +IntToStr(ord(mbIgnore)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );
    DoTestSimple('mbAll',   '',
                 'if test_ord_mb(mbAll) = ' +IntToStr(ord(mbAll)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );
    DoTestSimple('mbNoToAll',   '',
                 'if test_ord_mb(mbNoToAll) = ' +IntToStr(ord(mbNoToAll)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );
    DoTestSimple('mbYesToAll',   '',
                 'if test_ord_mb(mbYesToAll) = ' +IntToStr(ord(mbYesToAll)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );
    DoTestSimple('mbHelp',   '',
                 'if test_ord_mb(mbHelp) = ' +IntToStr(ord(mbHelp)) + ' then Caller.InsertTextAtCaret(''Y'', scamEnd);',
                 'Y'
                 );


  finally
    FTestMacro.Free;
    FTestSyn.Free;
  end;
end;

procedure TTestCase1.TestSynProcs;
begin
  FTestSyn := TSynEdit.Create(nil);
  FTestMacro := TEMSelfTestEditorMacro.Create(nil);
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
    // BlockBegin, BlockEnd
    DoTestSimple('BlockBegin read',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'var p: TPoint; begin ' + LineEnding +
                 'p := caller.BlockBegin;' + LineEnding +
                 'Caller.InsertTextAtCaret(IntToStr(p.x)+'',''+IntToStr(p.y), scamEnd);' + LineEnding +
                 'end.',
                 '6,2',
                 5, 2,   -1, -1, True,   8, 2 // select "Fo"
                 );

    DoTestSimple('BlockEnd read',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'var p: TPoint; begin ' + LineEnding +
                 'p := caller.BlockEnd;' + LineEnding +
                 'Caller.InsertTextAtCaret(IntToStr(p.x)+'',''+IntToStr(p.y), scamEnd);' + LineEnding +
                 'end.',
                 '8,2',
                 5, 2,   -1, -1, True,   8, 2 // select "Fo"
                 );

    DoTestSimple('BlockBegin/End write',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'begin ' + LineEnding +
                 'caller.BlockBegin := point(2,1);' + LineEnding +
                 'caller.BlockEnd := point(5,1);' + LineEnding +
                 'ecChar(''X'');' + LineEnding + // Replace "est"
                 'end.',
                 'TX',
                 5, 2,   -1, -1, True,   8, 2 // select "Fo"
                 );

    // SelAvail
    DoTestSimple('SelAvail',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'if caller.SelAvail then begin' + LineEnding +
                 '  Caller.CaretXY := point(2,1);' + LineEnding +
                 '  Caller.InsertTextAtCaret(''Y'', scamEnd);' + LineEnding +
                 'end;',
                 'TYest',
                 5, 2,   -1, -1, True,   8, 2 // select "Fo"
                 );
    DoTestSimple('SelAvail (NO)',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'if caller.SelAvail then begin' + LineEnding +
                 '  Caller.CaretXY := point(2,1);' + LineEnding +
                 '  Caller.InsertTextAtCaret(''Y'', scamEnd);' + LineEnding +
                 'end;',
                 'Test',
                 5, 2
                 );

    // SelText
    DoTestSimple('SelText (read)',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'var s: String; begin ' + LineEnding +
                 's := caller.SelText;' + LineEnding +
                 'Caller.CaretXY := point(2,1);' + LineEnding +
                 'Caller.InsertTextAtCaret(s, scamEnd);' + LineEnding +
                 'end.',
                 'TFoest',
                 5, 2,   -1, -1, True,   8, 2 // select "Fo"
                 );
    DoTestSimple('SelText(empty)',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'var s: String; begin ' + LineEnding +
                 's := caller.SelText;' + LineEnding +
                 'Caller.CaretXY := point(2,1);' + LineEnding +
                 'Caller.InsertTextAtCaret(s, scamEnd);' + LineEnding +
                 'end.',
                 'Test',
                 5, 2
                 );
    DoTestSimple('SelText (write)',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'caller.SelText := ''X'';',
                 '   Xo',
                 5, 2,   -1, -1, True,   8, 2 // select "Fo"
                 );

    // ClearSelection (does delete the text)
    DoTestSimple('ClearSelection',   'Test123',
                 'caller.ClearSelection;',
                 'T123',
                 2, 1,   -1, -1, True,   5, 1 // select "est"
                 );

    // SelectAll
    DoTestSimple('SelectAll',   'Test123',
                 'caller.SelectAll;' + LineEnding +
                 'caller.SelText := '''';',
                 ''+LineEnding,
                 2, 1,   -1, -1, False,   5, 1 // select "est"
                 );

    // SelectToBrace
    DoTestSimple('SelectToBrace',   'Test (123);',
                 'caller.SelectToBrace;' + LineEnding +
                 'caller.SelText := '''';',
                 'Test ;'+LineEnding,
                 6, 1
                 );

    // SelectWord
    DoTestSimple('SelectWord',   'Test abc def',
                 'caller.SelectWord;' + LineEnding +
                 'caller.SelText := '''';',
                 'Test  def'+LineEnding,
                 7, 1
                 );

    // SelectLine
    DoTestSimple('SelectLine false',   '  Test abc ',
                 'caller.SelectLine(False);' + LineEnding +
                 'caller.SelText := ''X'';',
                 '  X'+LineEnding,
                 7, 1
                 );
    DoTestSimple('SelectLine true',   '  Test abc ',
                 'caller.SelectLine(True);' + LineEnding +
                 'caller.SelText := ''X'';',
                 'X'+LineEnding,
                 7, 1,  -1,-1, False
                 );

    DoTestSimple('SelectParagraph',   'Test'+ LineEnding + 'Foo' + LineEnding + LineEnding +'abc'+ LineEnding,
                 'caller.SelectParagraph;' + LineEnding +
                 'caller.SelText := '''';',
                 'abc',
                 1, 1
                 );


    // TODO: SelectionMode
    {%endregion Selection *}


    {%region Logical / Physical *}
    // LogicalToPhysicalPos
    DoTestSimple('LogicalToPhysicalPos', ' öabc',
                 'var p: TPoint; begin'+LineEnding+
                 '  p := Caller.LogicalToPhysicalPos(point(4,1));'+LineEnding+ // "a|b"
                 '  Caller.InsertTextAtCaret(inttostr(p.x)+'',''+inttostr(p.y), scamBegin);'+LineEnding+
                 'end.',
                 '3,1');

    // LogicalToPhysicalCol
    DoTestSimple('LogicalToPhysicalCol', ' öabc'+LineEnding+'x',
                 'var i: Integer; begin'+LineEnding+
                 '  i := Caller.LogicalToPhysicalCol(''ööabc'', 2, 6);'+LineEnding+ // "a|b"
                 '  Caller.InsertTextAtCaret(inttostr(i), scamBegin);'+LineEnding+
                 'end.',
                 '4');

    // PhysicalToLogicalPos
    DoTestSimple('PhysicalToLogicalPos', ' öabc',
                 'var p: TPoint; begin'+LineEnding+
                 '  p := Caller.PhysicalToLogicalPos(point(3,1));'+LineEnding+ // "a|b"
                 '  Caller.InsertTextAtCaret(inttostr(p.x)+'',''+inttostr(p.y), scamBegin);'+LineEnding+
                 'end.',
                 '4,1');

    // PhysicalToLogicalCol
    DoTestSimple('PhysicalToLogicalCol', ' öabc'+LineEnding+'x',
                 'var i: Integer; begin'+LineEnding+
                 '  i := Caller.PhysicalToLogicalCol(''ööabc'', 2, 4);'+LineEnding+ // "a|b"
                 '  Caller.InsertTextAtCaret(inttostr(i), scamBegin);'+LineEnding+
                 'end.',
                 '6');

    // PhysicalLineLength
    DoTestSimple('PhysicalLineLength', ' öabc'+LineEnding+'x',
                 'var i: Integer; begin'+LineEnding+
                 '  i := Caller.PhysicalLineLength(''ööabc'', 2);'+LineEnding+
                 '  Caller.InsertTextAtCaret(inttostr(i), scamBegin);'+LineEnding+
                 'end.',
                 '5');

    {%endregion Logical / Physical *}


    {%region Search *}
    DoTestSimple('Find',   'Test abc abcde 123',
                 'Caller.SearchReplace(''abc'', '''', []);',
                 'Test abc abcde 123',
                 1,1,  9,1 // caret at end of "abc"
                 );
    DoTestSimple('Find result',   'Test abc abcde 123',
                 'if Caller.SearchReplace(''abc'', '''', []) > 0 then ecChar(''X'');',
                 'Test X abcde 123',
                 1,1,  7,1
                 );
    DoTestSimple('Find NO result',   'Test abc abcde 123',
                 'if Caller.SearchReplace(''aXbc'', '''', []) > 0 then ecChar(''X'');',
                 'Test abc abcde 123' + LineEnding,
                 1,1,  -1,-1, False
                 );

    DoTestSimple('Replace',   'Test abc abcde 123',
                 'Caller.SearchReplace(''abc'', ''XYZ'', [ssoReplace]);',
                 'Test XYZ abcde 123'
                 );
    DoTestSimple('Replace All',   'Test abc abcde 123',
                 'Caller.SearchReplace(''abc'', ''XYZ'', [ssoReplaceAll]);',
                 'Test XYZ XYZde 123'
                 );
    DoTestSimple('Replace All whole word',   'Test abc abcde 123',
                 'Caller.SearchReplace(''abc'', ''XYZ'', [ssoReplaceAll, ssoWholeWord]);',
                 'Test XYZ abcde 123'
                 );

    DoTestSimple('Replace All EX',   'Test abc abcde 123',
                 'Caller.SearchReplaceEx(''abc'', ''XYZ'', [ssoReplaceAll], point(8,1));',
                 'Test abc XYZde 123'
                 );

    {%endregion Search *}


    {%region Clipboard *}
    DoTestSimple('Clipboard write AsText',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'Clipboard.AsText:= ''X'';',
                 'Foo',
                 5, 2,   -1, -1, True,   8, 2 // select "Fo"
                 );
    AssertEquals('Clipboard write AsText', 'X', Clipboard.AsText);


    Clipboard.AsText := 'MM';
    DoTestSimple('Clipboard read AsText',   ' ',
                 'Caller.InsertTextAtCaret(Clipboard.AsText, scamEnd);',
                 'MM'
                 );

    DoTestSimple('CopyToClipboard',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'Caller.CopyToClipboard;',
                 'Foo',
                 5, 2,   -1, -1, True,   8, 2 // select "Fo"
                 );
    AssertEquals('CopyToClipboard', 'Fo', Clipboard.AsText);

    DoTestSimple('CutToClipboard',   'Test'+ LineEnding + 'Ö   Foo' + LineEnding,
                 'Caller.CutToClipboard;',
                 ' o',
                 5, 2,   -1, -1, True,   8, 2 // select "Fo"
                 );
    AssertEquals('CutToClipboard', 'Fo', Clipboard.AsText);

    Clipboard.AsText := 'OO';
    DoTestSimple('PasteFromClipboard',   ' ',
                 'Caller.PasteFromClipboard;',
                 'OO'
                 );

    // CanPaste
    {%endregion Clipboard *}


  finally
    FTestMacro.Free;
    FTestSyn.Free;
  end;
end;

procedure TTestCase1.TestSelfTest;
begin
  AssertTrue(DoSelfTest);
end;

procedure TTestCase1.TestInteractiv;
begin
  FTestSyn := TSynEdit.Create(nil);
  FTestMacro := TEMSelfTestEditorMacro.Create(nil);
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

    DoTestSimple('InputQuery Cancel',   '',
                 'var s: string; begin '+ LineEnding +
                 's := '''';' + LineEnding +
                 'if InputQuery(''Need Input'', ''enter 123 / press OK'', s)' + LineEnding +
                 'then Caller.InsertTextAtCaret(s, scamEnd);' + LineEnding +
                 'end.',
                 '123');


  finally
    FTestMacro.Free;
    FTestSyn.Free;
  end;
end;



initialization

  RegisterTest(TTestCase1);
end.

