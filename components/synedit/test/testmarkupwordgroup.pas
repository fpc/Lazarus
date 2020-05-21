unit TestMarkupwordGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase, TestHighlightPas, Forms, LCLProc,
  SynEdit, SynHighlighterPas, SynEditMarkupWordGroup;

type

  TMarkupWordGroupAccess = class(TSynEditMarkupWordGroup)
  end;

  { TTestMarkupWordGroup }

  TTestMarkupWordGroup = class(TTestBaseHighlighterPas)
  private
    Markup: TMarkupWordGroupAccess;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
    procedure ReCreateEdit; reintroduce;
    function TestText1: TStringArray;
    function TestText2: TStringArray;
  published
    procedure TestWordGroup;
  end; 

implementation

{ TTestMarkupWordGroup }

procedure TTestMarkupWordGroup.SetUp;
begin
  Markup := nil;
  inherited SetUp;
end;

procedure TTestMarkupWordGroup.TearDown;
begin
  FreeAndNil(Markup);
  inherited TearDown;
end;

procedure TTestMarkupWordGroup.ReCreateEdit;
begin
  inherited ReCreateEdit;
  Markup := TMarkupWordGroupAccess.Create(SynEdit);
  Markup.Lines := SynEdit.ViewedTextBuffer;
  Markup.Highlighter := SynEdit.Highlighter;
end;

function TTestMarkupWordGroup.TestText1: TStringArray;
begin
  SetLength(Result, 10);
  Result[0] := 'program Foo;';
  Result[1] := 'procedure a;';
  Result[2] := 'begin';
  Result[3] := '  if a then begin';
  Result[4] := '    writeln()';
  Result[5] := '  end;';
  Result[6] := 'end;';
  Result[7] := 'begin';
  Result[8] := 'end.';
  Result[9] := '';
end;

function TTestMarkupWordGroup.TestText2: TStringArray;
begin
  SetLength(Result, 10);
  Result[0] := 'Unit Foo;';
  Result[1] := 'implementation';
  Result[2] := 'procedure a; begin'; // same line
  Result[3] := '  try';
  Result[4] := '    try writeln(3); except end;'; // all one line / nested into
  Result[5] := '  finally';
  Result[6] := '    writeln(2)';
  Result[7] := '  end;';
  Result[8] := 'end;';
  Result[9] := '';

end;

procedure TTestMarkupWordGroup.TestWordGroup;
  procedure CheckWord(Name: String; X,Y, w1X,w1E,w1Y, w2X,w2E,w2Y, w3X,w3E,w3Y: Integer);
    Procedure SortWordPoint(var w1,w2: TWordPoint);
    var w3: TWordPoint;
    begin
      if (w1.Y > w2.Y) or ((w1.Y = w2.Y) and (w1.X > w2.X)) then begin
        w3 := w1;
        w1 := w2;
        w2 := w3;
      end;
    end;
  var
    w1,w2,w3: TWordPoint;
  begin
    Name := Name + ' At '+IntToStr(x)+ ','+IntToStr(y)+' ';
    Markup.FindMatchingWords(Point(X,Y), w1, w2, w3);
    SortWordPoint(w1, w2);
    SortWordPoint(w2, w3);
    SortWordPoint(w1, w2);
    if w1Y = -1 then
      AssertEquals(Name+'Y', -1, w1.Y)
    else begin
      AssertEquals(Name+'Y',  w1Y, w1.Y);
      AssertEquals(Name+'X1', w1X, w1.X);
      AssertEquals(Name+'X2', w1E, w1.X2);
    end;
    if w2Y = -1 then
      AssertEquals(Name+'Y', -1, w2.Y)
    else begin
      AssertEquals(Name+'Y',  w2Y, w2.Y);
      AssertEquals(Name+'X1', w2X, w2.X);
      AssertEquals(Name+'X2', w2E, w2.X2);
    end;
    if w3Y = -1 then
      AssertEquals(Name+'Y', -1, w3.Y)
    else begin
      AssertEquals(Name+'Y',  w3Y, w3.Y);
      AssertEquals(Name+'X1', w3X, w3.X);
      AssertEquals(Name+'X2', w3E, w3.X2);
    end;
  end;
begin
  ReCreateEdit;

  PopPushBaseName('All folds');

  PushBaseName('Text 1');
  SetLines(TestText1);
  EnableFolds([cfbtBeginEnd.. cfbtNone], [cfbtSlashComment]);

  CheckWord('Procedure 1',  1, 2,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('Procedure 2',  2, 2,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('Procedure 3', 10, 2,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('P-begin 1',    1, 3,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('P-begin 2',    2, 3,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('P-begin 3',    6, 3,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('P-end 1',      1, 7,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('P-end 2',      2, 7,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('P-end 3',      4, 7,   1,10,2,  1,6,3,  1,4,7);

  PopPushBaseName('Text 2');
  SetLines(TestText2);
  EnableFolds([cfbtBeginEnd.. cfbtNone], [cfbtSlashComment]);

  CheckWord('Procedure 1',  1, 3,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('Procedure 2',  2, 3,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('Procedure 3', 10, 3,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('P-Begin 1',   14, 3,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('P-Begin 2',   15, 3,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('P-Begin 3',   19, 3,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('P-End 1',      1, 9,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('P-End 2',      2, 9,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('P-End 3',      4, 9,   1,10,3,  14,19,3,  1,4,9);

  CheckWord('Try 1',      3, 4,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('Try 2',      4, 4,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('Try 3',      6, 4,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('finally 1',  3, 6,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('finally 2',  4, 6,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('finally 3', 10, 6,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('T-End 1',    3, 8,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('T-End 2',    4, 8,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('T-End 3',    6, 8,   3,6,4,  3,10,6,  3,6,8);

  CheckWord('Try Nest 1',      5, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('Try Nest 2',      6, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('Try Nest 3',      8, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('except Nest 1',  21, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('except Nest 2',  22, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('except Nest 3',  27, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('T-End Nest 1',   28, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('T-End Nest 2',   29, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('T-End Nest 3',   31, 5,   5,8,5,  21,27,5,  28,31,5);

  PopBaseName;

  PopPushBaseName('No folds');

  PushBaseName('Text 1');
  SetLines(TestText1);
  EnableFolds([cfbtBeginEnd.. cfbtNone], [], [cfbtBeginEnd.. cfbtNone]);

  CheckWord('Procedure 1',  1, 2,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('Procedure 2',  2, 2,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('Procedure 3', 10, 2,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('P-begin 1',    1, 3,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('P-begin 2',    2, 3,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('P-begin 3',    6, 3,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('P-end 1',      1, 7,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('P-end 2',      2, 7,   1,10,2,  1,6,3,  1,4,7);
  CheckWord('P-end 3',      4, 7,   1,10,2,  1,6,3,  1,4,7);

  PopPushBaseName('Text 2');
  SetLines(TestText2);
  EnableFolds([cfbtBeginEnd.. cfbtNone], [cfbtSlashComment]);

  CheckWord('Procedure 1',  1, 3,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('Procedure 2',  2, 3,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('Procedure 3', 10, 3,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('P-Begin 1',   14, 3,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('P-Begin 2',   15, 3,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('P-Begin 3',   19, 3,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('P-End 1',      1, 9,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('P-End 2',      2, 9,   1,10,3,  14,19,3,  1,4,9);
  CheckWord('P-End 3',      4, 9,   1,10,3,  14,19,3,  1,4,9);

  CheckWord('Try 1',      3, 4,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('Try 2',      4, 4,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('Try 3',      6, 4,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('finally 1',  3, 6,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('finally 2',  4, 6,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('finally 3', 10, 6,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('T-End 1',    3, 8,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('T-End 2',    4, 8,   3,6,4,  3,10,6,  3,6,8);
  CheckWord('T-End 3',    6, 8,   3,6,4,  3,10,6,  3,6,8);

  CheckWord('Try Nest 1',      5, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('Try Nest 2',      6, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('Try Nest 3',      8, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('except Nest 1',  21, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('except Nest 2',  22, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('except Nest 3',  27, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('T-End Nest 1',   28, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('T-End Nest 2',   29, 5,   5,8,5,  21,27,5,  28,31,5);
  CheckWord('T-End Nest 3',   31, 5,   5,8,5,  21,27,5,  28,31,5);

  PopBaseName;
end;

initialization

  RegisterTest(TTestMarkupWordGroup); 
end.

