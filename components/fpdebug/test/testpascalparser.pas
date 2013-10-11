unit TestPascalParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, FpPascalParser;

type

  { TTestPascalParser }

  TTestPascalParser = class(TTestCase)
  published
    procedure TestParser;
  end;

implementation


type

  { TTestFpPascalExpression }

  TTestFpPascalExpression=class(TFpPascalExpression)
  public
    property ExpressionPart;
  end;

{ TTestFpPascalExpression }

procedure TTestPascalParser.TestParser;
var
  CurrentTestExprText: String;
  CurrentTestExprObj: TTestFpPascalExpression;

  Procedure TestExpr(APart: TFpPascalExpressionPart; AClass: TFpPascalExpressionPartClass;
    AText: String; AChildCount: Integer = -1);
  begin
    AssertNotNull(CurrentTestExprText+ ': IsAssigned', APart);
    AssertTrue(CurrentTestExprText+': APart IS Class exp: '+AClass.ClassName+' was: '+APart.ClassName,
               APart is AClass);
    AssertEquals(CurrentTestExprText+': Text', AText, APart.GetText);
    if AChildCount >=0 then begin
      AssertTrue(CurrentTestExprText+': Is container ', APart is TFpPascalExpressionPartContainer);
      AssertEquals(CurrentTestExprText+': childcount ', AChildCount, (APart as TFpPascalExpressionPartContainer).Count);
    end;
  end;

  procedure CreateExpr(t: string; ExpValid: Boolean);
  begin
    FreeAndNil(CurrentTestExprObj);
    CurrentTestExprText := t;
    CurrentTestExprObj := TTestFpPascalExpression.Create(CurrentTestExprText);
    AssertEquals('Valid '+CurrentTestExprObj.Error+ ' # '+CurrentTestExprText, ExpValid, CurrentTestExprObj.Valid);
  end;

  function GetChild(p: TFpPascalExpressionPart; i: array of integer): TFpPascalExpressionPart;
  var
    j: Integer;
  begin
    Result := p;
    for j := low(i) to high(i) do
      Result := (Result as TFpPascalExpressionPartContainer).Items[i[j]];
  end;

begin
  CurrentTestExprObj := nil;
  try
    CreateExpr('a', True);
    TestExpr(CurrentTestExprObj.ExpressionPart, TFpPascalExpressionPartIdentifer, 'a', 0);

    CreateExpr('a b', False);

    CreateExpr('@a', True);
    TestExpr(CurrentTestExprObj.ExpressionPart, TFpPascalExpressionPartOperatorAddressOf, '@', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0]), TFpPascalExpressionPartIdentifer, 'a', 0);

    CreateExpr('a@', False);

    CreateExpr('-a', True);
    TestExpr(CurrentTestExprObj.ExpressionPart, TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0]), TFpPascalExpressionPartIdentifer, 'a', 0);

    CreateExpr('+-a', True);
    TestExpr(CurrentTestExprObj.ExpressionPart, TFpPascalExpressionPartOperatorUnaryPlusMinus, '+', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0]), TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,0]), TFpPascalExpressionPartIdentifer, 'a', 0);

    CreateExpr('a+b', True);
    TestExpr(CurrentTestExprObj.ExpressionPart, TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0]), TFpPascalExpressionPartIdentifer, 'a', 0);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [1]), TFpPascalExpressionPartIdentifer, 'b', 0);

    CreateExpr('a+', False);
    CreateExpr('a*', False);
    CreateExpr('a+b-', False);
    CreateExpr('a@+b', False);

    CreateExpr('a+-b', True);
    TestExpr(CurrentTestExprObj.ExpressionPart, TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0]), TFpPascalExpressionPartIdentifer, 'a', 0);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [1]), TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [1,0]), TFpPascalExpressionPartIdentifer, 'b', 0);

    CreateExpr('+a + -@b  -  @+c', True);
    TestExpr(CurrentTestExprObj.ExpressionPart, TFpPascalExpressionPartOperatorPlusMinus, '-', 2);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,       [0]),       TFpPascalExpressionPartOperatorPlusMinus,    '+', 2);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,     [0,0]),     TFpPascalExpressionPartOperatorUnaryPlusMinus,'+', 1);
        TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,   [0,0,0]),   TFpPascalExpressionPartIdentifer,              'a', 0);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,     [0,1]),     TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
        TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,   [0,1,0]),   TFpPascalExpressionPartOperatorAddressOf,       '@', 1);
          TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,1,0,0]), TFpPascalExpressionPartIdentifer,                'b', 0);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,       [1]),       TFpPascalExpressionPartOperatorAddressOf,     '@', 1);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,     [1,0]),     TFpPascalExpressionPartOperatorUnaryPlusMinus, '+', 1);
        TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,   [1,0,0]),   TFpPascalExpressionPartIdentifer,               'c', 0);

    CreateExpr('a+b*c', True);
    TestExpr(CurrentTestExprObj.ExpressionPart, TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0]), TFpPascalExpressionPartIdentifer, 'a', 0);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [1]), TFpPascalExpressionPartOperatorMulDiv, '*', 2);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [1,0]), TFpPascalExpressionPartIdentifer, 'b', 0);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [1,1]), TFpPascalExpressionPartIdentifer, 'c', 0);

    CreateExpr('a*b+c', True);
    TestExpr(CurrentTestExprObj.ExpressionPart, TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,   [0]), TFpPascalExpressionPartOperatorMulDiv, '*', 2);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,0]), TFpPascalExpressionPartIdentifer, 'a', 0);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,1]), TFpPascalExpressionPartIdentifer, 'b', 0);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,   [1]), TFpPascalExpressionPartIdentifer, 'c', 0);

    CreateExpr('a*b+c*d', True);
    TestExpr(CurrentTestExprObj.ExpressionPart, TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,   [0]), TFpPascalExpressionPartOperatorMulDiv, '*', 2);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,0]), TFpPascalExpressionPartIdentifer, 'a', 0);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,1]), TFpPascalExpressionPartIdentifer, 'b', 0);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,   [1]), TFpPascalExpressionPartOperatorMulDiv, '*', 2);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [1,0]), TFpPascalExpressionPartIdentifer, 'c', 0);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [1,1]), TFpPascalExpressionPartIdentifer, 'd', 0);

    CreateExpr('@a*@b+@c', True);
    TestExpr(CurrentTestExprObj.ExpressionPart, TFpPascalExpressionPartOperatorPlusMinus,                       '+', 2);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,     [0]),     TFpPascalExpressionPartOperatorMulDiv,    '*', 2);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,   [0,0]),   TFpPascalExpressionPartOperatorAddressOf,  '@', 1);
        TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,0,0]), TFpPascalExpressionPartIdentifer,           'a', 0);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,   [0,1]),   TFpPascalExpressionPartOperatorAddressOf,  '@', 1);
        TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,1,0]), TFpPascalExpressionPartIdentifer,           'b', 0);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,     [1]),     TFpPascalExpressionPartOperatorAddressOf, '@', 1);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,   [1,0]),   TFpPascalExpressionPartIdentifer,          'c', 0);

    CreateExpr('@a*@b+@c*@d', True);
    TestExpr(CurrentTestExprObj.ExpressionPart, TFpPascalExpressionPartOperatorPlusMinus,                       '+', 2);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,     [0]),     TFpPascalExpressionPartOperatorMulDiv,    '*', 2);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,   [0,0]),   TFpPascalExpressionPartOperatorAddressOf,  '@', 1);
        TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,0,0]), TFpPascalExpressionPartIdentifer,           'a', 0);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,   [0,1]),   TFpPascalExpressionPartOperatorAddressOf,  '@', 1);
        TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,1,0]), TFpPascalExpressionPartIdentifer,           'b', 0);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,     [1]),     TFpPascalExpressionPartOperatorMulDiv,    '*', 2);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,   [1,0]),     TFpPascalExpressionPartOperatorAddressOf, '@', 1);
        TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [1,0,0]),   TFpPascalExpressionPartIdentifer,          'c', 0);
      TestExpr(GetChild(CurrentTestExprObj.ExpressionPart,   [1,1]),     TFpPascalExpressionPartOperatorAddressOf, '@', 1);
        TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [1,1,0]),   TFpPascalExpressionPartIdentifer,          'd', 0);


    CreateExpr('(a)', True);
    TestExpr(CurrentTestExprObj.ExpressionPart, TFpPascalExpressionPartRoundBracket, '(', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0]), TFpPascalExpressionPartIdentifer, 'a', 0);

    CreateExpr('a)', False);
    CreateExpr('(a', False);
    CreateExpr(')', False);
    CreateExpr('(', False);

    CreateExpr('(-a)', True);
    TestExpr(CurrentTestExprObj.ExpressionPart, TFpPascalExpressionPartRoundBracket, '(', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0]), TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,0]), TFpPascalExpressionPartIdentifer, 'a', 0);

    CreateExpr('-(-a)', True);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, []), TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0]), TFpPascalExpressionPartRoundBracket, '(', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,0]), TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,0,0]), TFpPascalExpressionPartIdentifer, 'a', 0);

    CreateExpr('(a*b)', True);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, []), TFpPascalExpressionPartRoundBracket, '(', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0]), TFpPascalExpressionPartOperatorMulDiv, '*', 2);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,0]), TFpPascalExpressionPartIdentifer, 'a', 0);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,1]), TFpPascalExpressionPartIdentifer, 'b', 0);

    CreateExpr('(-a*b)', True);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, []), TFpPascalExpressionPartRoundBracket, '(', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0]), TFpPascalExpressionPartOperatorMulDiv, '*', 2);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,0]), TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,0,0]), TFpPascalExpressionPartIdentifer, 'a', 0);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,1]), TFpPascalExpressionPartIdentifer, 'b', 0);

    CreateExpr('(a)*b', True);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, []), TFpPascalExpressionPartOperatorMulDiv, '*', 2);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0]), TFpPascalExpressionPartRoundBracket, '(', 1);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [0,0]), TFpPascalExpressionPartIdentifer, 'a', 0);
    TestExpr(GetChild(CurrentTestExprObj.ExpressionPart, [1]), TFpPascalExpressionPartIdentifer, 'b', 0);

    CreateExpr('(a+b)*c', True);
    CreateExpr('(@a)*@c', True);
    CreateExpr('(@a+@b)*@c', True);


  finally
    CurrentTestExprObj.Free;
  end;
end;



initialization

  RegisterTest(TTestPascalParser);
end.

