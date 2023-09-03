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
end;

initialization

  RegisterTest(TTestIdeDebuggerVarious);
end.

