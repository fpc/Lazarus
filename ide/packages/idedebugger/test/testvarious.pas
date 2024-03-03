unit TestVarious;

{$mode objfpc}{$H+}
{$INLINE off}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  IdeDebuggerUtils,
  LazLogger;

type

  { TTestIdeDebuggerVarious }

  TTestIdeDebuggerVarious = class(TTestCase)
  published
    procedure TestExpressionForArrayElement;
  end;

implementation


{ TTestIdeDebuggerVarious }

procedure TTestIdeDebuggerVarious.TestExpressionForArrayElement;
  procedure Test(ABefore: string; AIdx: Integer; AnAfter: string);
  begin
    AssertEquals(ABefore+' @ ' + IntToStr(AIdx) + ' => '+AnAfter,
                 AnAfter, GetExpressionForArrayElement(ABefore, AIdx));
  end;
begin
  AssertEquals('a',    'a[11]',    GetExpressionForArrayElement('a', 11));
  AssertEquals('(a)',  '(a)[11]',  GetExpressionForArrayElement('(a)', 11));
  AssertEquals('a[2]', 'a[2][11]', GetExpressionForArrayElement('a[2]', 11));

  AssertEquals('(a * [1..22]) * b[3..33]',     '(a * [1..22]) * b[11]',     GetExpressionForArrayElement('(a * [1..22]) * b[3..33]', 11));
  AssertEquals('(a in [1..22]) * b[3..33]',    '(a in [1..22]) * b[11]',    GetExpressionForArrayElement('(a in [1..22]) * b[3..33]', 11));
  AssertEquals('( an [1..22]) * b[3..33]',     '( an [11]) * b[3..33]',      GetExpressionForArrayElement('( an [1..22]) * b[3..33]', 11));

  AssertEquals('a[2..22]',          'a[11]',          GetExpressionForArrayElement('a[2..22]', 11));
  AssertEquals('a[-2..22]',         'a[11]',          GetExpressionForArrayElement('a[-2..22]', 11));
  AssertEquals('a[-22..-2]',        'a[-11]',         GetExpressionForArrayElement('a[-22..-2]', -11));
  AssertEquals('a[2..22][3..33]',   'a[11][3..33]',   GetExpressionForArrayElement('a[2..22][3..33]', 11));
  AssertEquals('a[2..22]*b[3..33]', 'a[11]*b[3..33]', GetExpressionForArrayElement('a[2..22]*b[3..33]', 11));
  AssertEquals('Foo(a[2..22])',     'Foo(a[11])',     GetExpressionForArrayElement('Foo(a[2..22])', 11));

  AssertEquals('PCellProps((TFpList((StringGrid1.FGrid.FCellArr.FCols.FList)^[0..3]).FList)^[0..3])^',
    'PCellProps((TFpList((StringGrid1.FGrid.FCellArr.FCols.FList)^[2]).FList)^[0..3])^',
    GetExpressionForArrayElement('PCellProps((TFpList((StringGrid1.FGrid.FCellArr.FCols.FList)^[0..3]).FList)^[0..3])^', 2)
  );

  // nested goes first
  Test('Foo(a[x[2..14]..99])',      11, 'Foo(a[x[11]..99])');
  Test('Foo(a[1+x[2..14]..99])',    11, 'Foo(a[1+x[11]..99])');
  Test('Foo(a[(1+x[2..14])..99])',  11, 'Foo(a[(1+x[11])..99])');
  Test('Foo(a[0..x[2..14]])',       11, 'Foo(a[0..x[11]])');
  Test('Foo(a[0..1+x[2..14]])',     11, 'Foo(a[0..1+x[11]])');
  Test('Foo(a[0..(1+x[2..14])])',   11, 'Foo(a[0..(1+x[11])])');

  // not in set
  Test('Foo(a[x in [1..3]])',  11, 'Foo(a[x in [1..3]])[11]'); // no ..
  Test('Foo(a[x in [1..3]..True])',  11, 'Foo(a[11])');

  Test('Foo(a[b[2..22]])',     11, 'Foo(a[b[11]])');
  Test('Foo(a[(1+b[2..22])])', 11, 'Foo(a[(1+b[11])])');

  // ","
  Test('Foo(a[2..22,55..66])', 11, 'Foo(a[11,55..66])');
  Test('Foo(a[99,2..22])',     11, 'Foo(a[99,11])');

  // set from array
  Test('e1 in [x[1..15]]',     11, 'e1 in [x[11]]');
  Test('e1 in [x[1..15]..x[33..35]]',     11, 'e1 in [x[11]..x[33..35]]');

end;

initialization

  RegisterTest(TTestIdeDebuggerVarious);
end.

