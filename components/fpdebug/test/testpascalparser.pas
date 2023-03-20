unit TestPascalParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, FpPascalParser,
  FpErrorMessages, FpDbgInfo,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif};

type

  { TTestFpPascalExpression }

  TTestFpPascalExpression=class(TFpPascalExpression)
  public
    property ExpressionPart;
  end;

  { TTestPascalParser }

  TTestPascalParser = class(TTestCase)
  private
    CurrentTestExprText: String;
    CurrentTestExprObj: TTestFpPascalExpression;
    procedure CreateExpr(t: string; ExpValid: Boolean; SkipExpValid: Boolean = False);
  published
    procedure TestParser;
    procedure TestParserError;
  end;

implementation

{ TTestFpPascalExpression }

procedure TTestPascalParser.CreateExpr(t: string; ExpValid: Boolean;
  SkipExpValid: Boolean);
var
  s: String;
  ctx: TFpDbgSimpleLocationContext;
  sc: TFpDbgSymbolScope;
begin
  ctx := TFpDbgSimpleLocationContext.Create(nil, 0, 4, 0, 0);
  sc := TFpDbgSymbolScope.Create(ctx);
  FreeAndNil(CurrentTestExprObj);
  CurrentTestExprText := t;
  CurrentTestExprObj := TTestFpPascalExpression.Create(CurrentTestExprText, sc);
DebugLn(CurrentTestExprObj.DebugDump);
  if not SkipExpValid then begin
    s := ErrorHandler.ErrorAsString(CurrentTestExprObj.Error);
    AssertEquals('Valid '+s+ ' # '+CurrentTestExprText, ExpValid, CurrentTestExprObj.Valid);
  end;
  ctx.ReleaseReference;
  sc.ReleaseReference;
end;

procedure TTestPascalParser.TestParser;

  function GetChild(p: TFpPascalExpressionPart; i: array of integer): TFpPascalExpressionPart;
  var
    j: Integer;
  begin
    Result := p;
    for j := low(i) to high(i) do
      Result := (Result as TFpPascalExpressionPartContainer).Items[i[j]];
  end;

  function GetChild(i: array of integer): TFpPascalExpressionPart;
  begin
    Result := GetChild(CurrentTestExprObj.ExpressionPart, i);
  end;

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

  Procedure TestExpr(i: array of integer; AClass: TFpPascalExpressionPartClass;
    AText: String; AChildCount: Integer = -1);
  begin
    TestExpr(GetChild(i), AClass, AText, AChildCount);
  end;

begin
  CurrentTestExprObj := nil;
  try
    CreateExpr('a', True);
    TestExpr([], TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('a b', False);
    CreateExpr('a |', False);
    CreateExpr('| b', False);
    CreateExpr('|', False);

    CreateExpr('@a', True);
    TestExpr([], TFpPascalExpressionPartOperatorAddressOf, '@', 1);
    TestExpr([0], TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('a@', False);
    CreateExpr('@', False);

    CreateExpr('-a', True);
    TestExpr([], TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr([0], TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('+-a', True);
    TestExpr([], TFpPascalExpressionPartOperatorUnaryPlusMinus, '+', 1);
    TestExpr([0], TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr([0,0], TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('a+b', True);
    TestExpr([], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
    TestExpr([0], TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([1], TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('a+', False);
    CreateExpr('a*', False);
    CreateExpr('a+b-', False);
    CreateExpr('a@+b', False);

    CreateExpr('a+-b', True);
    TestExpr([], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
    TestExpr([0], TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([1], TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr([1,0], TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('+a + -@b  -  @+c', True);
    TestExpr([], TFpPascalExpressionPartOperatorPlusMinus, '-', 2);
    TestExpr(       [0],       TFpPascalExpressionPartOperatorPlusMinus,    '+', 2);
      TestExpr(     [0,0],     TFpPascalExpressionPartOperatorUnaryPlusMinus,'+', 1);
        TestExpr(   [0,0,0],   TFpPascalExpressionPartIdentifier,              'a', 0);
      TestExpr(     [0,1],     TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
        TestExpr(   [0,1,0],   TFpPascalExpressionPartOperatorAddressOf,       '@', 1);
          TestExpr([0,1,0,0], TFpPascalExpressionPartIdentifier,                'b', 0);
    TestExpr(       [1],       TFpPascalExpressionPartOperatorAddressOf,     '@', 1);
      TestExpr(     [1,0],     TFpPascalExpressionPartOperatorUnaryPlusMinus, '+', 1);
        TestExpr(   [1,0,0],   TFpPascalExpressionPartIdentifier,               'c', 0);

    CreateExpr('a+b*c', True);
    TestExpr([], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
    TestExpr([0], TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([1], TFpPascalExpressionPartOperatorMulDiv, '*', 2);
    TestExpr([1,0], TFpPascalExpressionPartIdentifier, 'b', 0);
    TestExpr([1,1], TFpPascalExpressionPartIdentifier, 'c', 0);

    CreateExpr('a*b+c', True);
    TestExpr([], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
    TestExpr(   [0], TFpPascalExpressionPartOperatorMulDiv, '*', 2);
      TestExpr([0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,1], TFpPascalExpressionPartIdentifier, 'b', 0);
    TestExpr(   [1], TFpPascalExpressionPartIdentifier, 'c', 0);

    CreateExpr('a*b+c*d', True);
    TestExpr([], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
    TestExpr(   [0], TFpPascalExpressionPartOperatorMulDiv, '*', 2);
      TestExpr([0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,1], TFpPascalExpressionPartIdentifier, 'b', 0);
    TestExpr(   [1], TFpPascalExpressionPartOperatorMulDiv, '*', 2);
      TestExpr([1,0], TFpPascalExpressionPartIdentifier, 'c', 0);
      TestExpr([1,1], TFpPascalExpressionPartIdentifier, 'd', 0);

    CreateExpr('@a*@b+@c', True);
    TestExpr([], TFpPascalExpressionPartOperatorPlusMinus,                       '+', 2);
    TestExpr(     [0],     TFpPascalExpressionPartOperatorMulDiv,    '*', 2);
      TestExpr(   [0,0],   TFpPascalExpressionPartOperatorAddressOf,  '@', 1);
        TestExpr([0,0,0], TFpPascalExpressionPartIdentifier,           'a', 0);
      TestExpr(   [0,1],   TFpPascalExpressionPartOperatorAddressOf,  '@', 1);
        TestExpr([0,1,0], TFpPascalExpressionPartIdentifier,           'b', 0);
    TestExpr(     [1],     TFpPascalExpressionPartOperatorAddressOf, '@', 1);
      TestExpr(   [1,0],   TFpPascalExpressionPartIdentifier,          'c', 0);

    CreateExpr('@a*@b+@c*@d', True);
    TestExpr([], TFpPascalExpressionPartOperatorPlusMinus,                       '+', 2);
    TestExpr(     [0],     TFpPascalExpressionPartOperatorMulDiv,    '*', 2);
      TestExpr(   [0,0],   TFpPascalExpressionPartOperatorAddressOf,  '@', 1);
        TestExpr([0,0,0], TFpPascalExpressionPartIdentifier,           'a', 0);
      TestExpr(   [0,1],   TFpPascalExpressionPartOperatorAddressOf,  '@', 1);
        TestExpr([0,1,0], TFpPascalExpressionPartIdentifier,           'b', 0);
    TestExpr(     [1],     TFpPascalExpressionPartOperatorMulDiv,    '*', 2);
      TestExpr(   [1,0],     TFpPascalExpressionPartOperatorAddressOf, '@', 1);
        TestExpr([1,0,0],   TFpPascalExpressionPartIdentifier,          'c', 0);
      TestExpr(   [1,1],     TFpPascalExpressionPartOperatorAddressOf, '@', 1);
        TestExpr([1,1,0],   TFpPascalExpressionPartIdentifier,          'd', 0);


    CreateExpr('a.b', True);
    TestExpr([], TFpPascalExpressionPartOperatorMemberOf,  '.', 2);
    TestExpr([0], TFpPascalExpressionPartIdentifier,        'a', 0);
    TestExpr([1], TFpPascalExpressionPartIdentifier,        'b', 0);

    CreateExpr('a.b^', True);
    TestExpr([], TFpPascalExpressionPartOperatorDeRef,  '^', 1);
    TestExpr([0], TFpPascalExpressionPartOperatorMemberOf,  '.', 2);
    TestExpr([0,0], TFpPascalExpressionPartIdentifier,        'a', 0);
    TestExpr([0,1], TFpPascalExpressionPartIdentifier,        'b', 0);

    CreateExpr('a^.b', True);
    TestExpr([], TFpPascalExpressionPartOperatorMemberOf,  '.', 2);
    TestExpr([0], TFpPascalExpressionPartOperatorDeRef,  '^', 1);
    TestExpr([0,0], TFpPascalExpressionPartIdentifier,        'a', 0);
    TestExpr([1], TFpPascalExpressionPartIdentifier,        'b', 0);

    CreateExpr('a.b.c', True);
    TestExpr([], TFpPascalExpressionPartOperatorMemberOf,  '.', 2);
    TestExpr([0], TFpPascalExpressionPartOperatorMemberOf,  '.', 2);
    TestExpr([0,0], TFpPascalExpressionPartIdentifier,        'a', 0);
    TestExpr([0,1], TFpPascalExpressionPartIdentifier,        'b', 0);
    TestExpr([1], TFpPascalExpressionPartIdentifier,        'c', 0);

    CreateExpr('(a)', True);
    TestExpr([], TFpPascalExpressionPartBracketSubExpression, '(', 1);
    TestExpr([0], TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('a)', False);
    CreateExpr('(a', False);
    CreateExpr(')', False);
    CreateExpr('(', False);
    CreateExpr('(*a)', False);

    CreateExpr('(-a)', True);
    TestExpr([], TFpPascalExpressionPartBracketSubExpression, '(', 1);
    TestExpr([0], TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr([0,0], TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('-(-a)', True);
    TestExpr([], TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr([0], TFpPascalExpressionPartBracketSubExpression, '(', 1);
    TestExpr([0,0], TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr([0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('(a*b)', True);
    TestExpr([], TFpPascalExpressionPartBracketSubExpression, '(', 1);
    TestExpr([0], TFpPascalExpressionPartOperatorMulDiv, '*', 2);
    TestExpr([0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([0,1], TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('(-a*b)', True);
    TestExpr([], TFpPascalExpressionPartBracketSubExpression, '(', 1);
    TestExpr([0], TFpPascalExpressionPartOperatorMulDiv, '*', 2);
    TestExpr([0,0], TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr([0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([0,1], TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('(a)*b', True);
    TestExpr([], TFpPascalExpressionPartOperatorMulDiv, '*', 2);
    TestExpr([0], TFpPascalExpressionPartBracketSubExpression, '(', 1);
    TestExpr([0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([1], TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('(a+b)*c', True);
    CreateExpr('(@a)*@c', True);
    CreateExpr('(@a+@b)*@c', True);

    CreateExpr('f(a+b)*c', True);
    TestExpr(     [],      TFpPascalExpressionPartOperatorMulDiv, '*', 2);
    TestExpr(     [0],     TFpPascalExpressionPartBracketArgumentList, '(', 2);
      TestExpr(   [0,0],   TFpPascalExpressionPartIdentifier, 'f', 0);
      TestExpr(   [0,1],   TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
        TestExpr([0,1,0], TFpPascalExpressionPartIdentifier, 'a', 0);
        TestExpr([0,1,1], TFpPascalExpressionPartIdentifier, 'b', 0);
    TestExpr(     [1],     TFpPascalExpressionPartIdentifier, 'c', 0);

    CreateExpr('f(a)', True);
    TestExpr([],    TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([1],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('f(a)(b)', True);
    TestExpr([],    TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0],     TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0,0],     TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([0,1],     TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([1],     TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('f()', True);
    TestExpr([],    TFpPascalExpressionPartBracketArgumentList, '(', 1);
    TestExpr([0],     TFpPascalExpressionPartIdentifier, 'f', 0);

    CreateExpr('f(a,b)', True);
    TestExpr([],    TFpPascalExpressionPartBracketArgumentList, '(', 3);
    TestExpr([0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([1],   TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([2],   TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('f(-a, -b)', True);
    TestExpr([],    TFpPascalExpressionPartBracketArgumentList, '(', 3);
    TestExpr([0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([1],   TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr([1, 0],   TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([2],   TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr([2, 0],   TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('f(a,b, c)', True);
    TestExpr([],    TFpPascalExpressionPartBracketArgumentList, '(', 4);
    TestExpr([0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([1],   TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([2],   TFpPascalExpressionPartIdentifier, 'b', 0);
    TestExpr([3],   TFpPascalExpressionPartIdentifier, 'c', 0);

    CreateExpr('f(x(a),b)', True);
    TestExpr([],    TFpPascalExpressionPartBracketArgumentList, '(', 3);
      TestExpr([0],   TFpPascalExpressionPartIdentifier, 'f', 0);
      TestExpr([1],    TFpPascalExpressionPartBracketArgumentList, '(', 2);
        TestExpr([1,0],   TFpPascalExpressionPartIdentifier, 'x', 0);
        TestExpr([1,1],   TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([2],   TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('f(,)', False);
    CreateExpr('f(,,)', False);
    CreateExpr('f(a,)', False);
    CreateExpr('f(,a)', False);
    CreateExpr('f(a,,b)', False);

    CreateExpr('f(a)+b', True);
    TestExpr([],  TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
    TestExpr([0],   TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0,0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([0,1],   TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([1],   TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('c+f(a)', True);
    TestExpr([],  TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
    TestExpr([0],   TFpPascalExpressionPartIdentifier, 'c', 0);
    TestExpr([1],   TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([1,0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([1,1],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('c.f(a)', True);  // (c.f) (a)
    TestExpr([],   TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0],   TFpPascalExpressionPartOperatorMemberOf, '.', 2);
    TestExpr([0,0],   TFpPascalExpressionPartIdentifier, 'c', 0);
    TestExpr([0,1],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([1],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('f(a).c', True);  // (c.f) (a)
    TestExpr([],   TFpPascalExpressionPartOperatorMemberOf, '.', 2);
    TestExpr([0],   TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0,0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([0,1],   TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([1],   TFpPascalExpressionPartIdentifier, 'c', 0);

    CreateExpr('@f(a)', True);   // @( f(a) )
    TestExpr([],  TFpPascalExpressionPartOperatorAddressOf, '@', 1);
    TestExpr([0],   TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0,0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([0,1],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('-f(a)', True);   // -( f(a) )
    TestExpr([],  TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr([0],   TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0,0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([0,1],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('f^(a)', True);   // (f^) (a)
    TestExpr([],   TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0],  TFpPascalExpressionPartOperatorDeRef, '^', 1);
    TestExpr([0,0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([1],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('f(a)^', True);   // ( f(a) )^
    TestExpr([],  TFpPascalExpressionPartOperatorDeRef, '^', 1);
    TestExpr([0],   TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0,0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([0,1],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('f(a)(b)^', True);   // ( f(a)(b) )^
    TestExpr([],  TFpPascalExpressionPartOperatorDeRef, '^', 1);
    TestExpr([0],   TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0,0],   TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0,0,0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([0,0,1],   TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([0,1],   TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('f.()', False);
    CreateExpr('f(*a)', False);

    CreateExpr('f[a]', True);
    CreateExpr('f * [a]', True);

    CreateExpr('a^', True);
    TestExpr([],  TFpPascalExpressionPartOperatorDeRef, '^', 1);
    TestExpr([0],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('-a^', True);
    TestExpr([],  TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr([0],  TFpPascalExpressionPartOperatorDeRef, '^', 1);
    TestExpr([0,0],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('@a^', True);
    TestExpr([],  TFpPascalExpressionPartOperatorAddressOf, '@', 1);
    TestExpr([0],  TFpPascalExpressionPartOperatorDeRef, '^', 1);
    TestExpr([0,0],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('-@a', True);
    TestExpr([],  TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr([0],  TFpPascalExpressionPartOperatorAddressOf, '@', 1);
    TestExpr([0,0],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('-@a^', True);
    TestExpr([],  TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
    TestExpr([0],  TFpPascalExpressionPartOperatorAddressOf, '@', 1);
    TestExpr([0,0],  TFpPascalExpressionPartOperatorDeRef, '^', 1);
    TestExpr([0,0,0],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('^f(a)', True);
    TestExpr([], TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0],  TFpPascalExpressionPartOperatorMakeRef, '^', 1);
    TestExpr([0,0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([1],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('^f(a)^', True);
    TestExpr([],  TFpPascalExpressionPartOperatorDeRef, '^', 1);
    TestExpr([0], TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0,0],  TFpPascalExpressionPartOperatorMakeRef, '^', 1);
    TestExpr([0,0,0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([0,1],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('@f(a)(b)', True);
    TestExpr([],  TFpPascalExpressionPartOperatorAddressOf, '@', 1);
    TestExpr([0], TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0,0], TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0,0,0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([0,0,1],   TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([0,1],   TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('f(a)(b)^', True);
    TestExpr([],  TFpPascalExpressionPartOperatorDeRef, '^', 1);
    TestExpr([0], TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0,0], TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0,0,0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([0,0,1],   TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([0,1],   TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('f[a]', True);
    TestExpr([], TFpPascalExpressionPartBracketIndex, '[', 2);
    TestExpr([0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([1],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('f[a,b]', True);
    TestExpr([], TFpPascalExpressionPartBracketIndex, '[', 3);
    TestExpr([0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([1],   TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([2],   TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('f[]', False);
    CreateExpr('f[,]', False);
    CreateExpr('f[,a]', False);
    CreateExpr('f[a,]', False);
    CreateExpr('f[a,,b]', False);

    CreateExpr('TFoo(f^[0]).a', True);

    CreateExpr('^^int(1)', True);

    CreateExpr('x * [a]', True);
    TestExpr([], TFpPascalExpressionPartOperatorMulDiv, '*', 2);
    TestExpr([0],   TFpPascalExpressionPartIdentifier, 'x', 0);
    TestExpr([1],   TFpPascalExpressionPartBracketSet, '[', 1);
    TestExpr([1,0],   TFpPascalExpressionPartIdentifier, 'a', 0);

    CreateExpr('x * []', True);
    TestExpr([], TFpPascalExpressionPartOperatorMulDiv, '*', 2);
    TestExpr([0],   TFpPascalExpressionPartIdentifier, 'x', 0);
    TestExpr([1],   TFpPascalExpressionPartBracketSet, '[', 0);

    CreateExpr('x * [a,b]', True);
    TestExpr([], TFpPascalExpressionPartOperatorMulDiv, '*', 2);
    TestExpr([0],   TFpPascalExpressionPartIdentifier, 'x', 0);
    TestExpr([1],   TFpPascalExpressionPartBracketSet, '[', 2);
    TestExpr([1,0],   TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([1,1],   TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('x * [,]', False);
    CreateExpr('x * [,a]', False);
    CreateExpr('x * [a,]', False);


    CreateExpr('a[1..2]', True);
    TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
    TestExpr([0], TFpPascalExpressionPartBracketIndex, '[', 2);
    TestExpr([0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
    TestExpr([0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
    TestExpr([0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);

    CreateExpr('a[1..2]+9', True);
    TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
    TestExpr([0], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
    TestExpr([0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
    TestExpr([0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
    TestExpr([0,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
    TestExpr([0,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
    TestExpr([0,1], TFpPascalExpressionPartConstantNumber, '9', 0);

    CreateExpr('a[1..2][3..4]', True);
    TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
    TestExpr([0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
    TestExpr([0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
    //
    TestExpr([0,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
    TestExpr([0,0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([0,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
    TestExpr([0,0,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
    TestExpr([0,0,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
    //
    TestExpr([0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
    TestExpr([0,0,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
    TestExpr([0,0,1,1], TFpPascalExpressionPartConstantNumber, '4', 0);


    CreateExpr('a[1..2][3..4]+9', True);
    TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
    TestExpr([0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
    TestExpr([0,0], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
    TestExpr([0,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
    //
    TestExpr([0,0,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
    TestExpr([0,0,0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([0,0,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
    TestExpr([0,0,0,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
    TestExpr([0,0,0,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
    //
    TestExpr([0,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
    TestExpr([0,0,0,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
    TestExpr([0,0,0,1,1], TFpPascalExpressionPartConstantNumber, '4', 0);
    //
    TestExpr([0,0,1], TFpPascalExpressionPartConstantNumber, '9', 0);


  finally
    CurrentTestExprObj.Free;
  end;
end;

procedure TTestPascalParser.TestParserError;

  procedure AssertPrintError;
  var
    s: String;
  begin
    CurrentTestExprObj.ResultValue;
    AssertTrue('Got an error', IsError(CurrentTestExprObj.Error));

    s := ErrorHandler.ErrorAsString(CurrentTestExprObj.Error);
    AssertTrue('format error msg', pos('Internal Error:', s) < 1);
  end;

  procedure AssertError(const AnErrCode: array of TFpErrorCode);
  var
    GotErrCode: TFpErrorCode;
    Err: TFpError;
    i: Integer;
  begin
    CurrentTestExprObj.ResultValue;
    Err := CurrentTestExprObj.Error;
    AssertTrue('correct err code', IsError(Err));

    for i := low(AnErrCode) to high(AnErrCode) do begin
      AssertTrue('has sub error', Length(Err) >= i);
      GotErrCode := CurrentTestExprObj.Error[i].ErrorCode;
      AssertEquals('correct err code', AnErrCode[i], GotErrCode);
    end;

    AssertPrintError;
  end;

  procedure TestExpr(Expr: String; AnErrCode: TFpErrorCode);
  begin
    CreateExpr(Expr, False, True);
    AssertError([AnErrCode]);
  end;

  procedure TestExpr(Expr: String; const AnErrCode: array of TFpErrorCode);
  begin
    CreateExpr(Expr, False, True);
    AssertError(AnErrCode);
  end;

var
  s: String;
begin
  // self test
  s := ErrorHandler.ErrorAsString(CreateError(-1));
  AssertFalse('self test format error msg', pos('Internal Error:', s) < 1);
  s := ErrorHandler.ErrorAsString(CreateError(fpErrPasParserUnexpectedToken_p, []));
  AssertFalse('self test format error msg', pos('Internal Error:', s) < 1);


  TestExpr('£',        fpErrPasParserUnexpectedToken_p);
  TestExpr(':foobar',  fpErrPasParserUnknownIntrinsic_p);

  TestExpr('1..2',     fpErrPasParserUnexpectedToken_p);
  TestExpr('[1...2]',  fpErrPasParserUnexpectedToken_p);

  TestExpr('1)',       fpErrPasParserMissingOpenBracket_p);
  TestExpr('[1)',      fpErrPasParserWrongOpenBracket_p);

  TestExpr('1a ',      fpErrPasParserExpectedNumber_p);
  TestExpr('$ ',       fpErrPasParserExpectedNumber_p);
  TestExpr('$x',       fpErrPasParserExpectedNumber_p);
  TestExpr('$1x',      fpErrPasParserExpectedNumber_p);
  TestExpr('0x',       fpErrPasParserExpectedNumber_p);
  TestExpr('0x1z',     fpErrPasParserExpectedNumber_p);
  TestExpr('& ',       fpErrPasParserExpectedNumber_p);
  TestExpr('&1z ',     fpErrPasParserExpectedNumber_p);
  TestExpr('%9 ',      fpErrPasParserExpectedNumber_p);
  TestExpr('%1z ',     fpErrPasParserExpectedNumber_p);

  TestExpr('''a',      fpErrPasParserUnterminatedString_p);
  TestExpr('#',        fpErrPasParserUnexpectedEndOfExpression);
  TestExpr('# ',       fpErrPasParserExpectedNumber_p);
  TestExpr('#.',       fpErrPasParserExpectedNumber_p);
  TestExpr('#12a',     fpErrPasParserExpectedNumber_p);
  TestExpr('#af',      fpErrPasParserExpectedNumber_p);
  TestExpr('#$az',     fpErrPasParserExpectedNumber_p);
  TestExpr('#$z',     fpErrPasParserExpectedNumber_p);
  TestExpr('#&79',      fpErrPasParserExpectedNumber_p);
  TestExpr('#&9',      fpErrPasParserExpectedNumber_p);
  TestExpr('#%13',      fpErrPasParserExpectedNumber_p);
  TestExpr('#%3',      fpErrPasParserExpectedNumber_p);

  TestExpr('''abc''[]',    fpErrPasParserMissingIndexExpression);
  TestExpr('''abc''[#1]',  [fpErrPasParserIndexError_Wrapper, fpErrExpectedOrdinalVal_p]);
  TestExpr('''abc''[99]',  [fpErrPasParserIndexError_Wrapper, fpErrIndexOutOfRange]);
  TestExpr('1[99]',        [fpErrPasParserIndexError_Wrapper, fpErrTypeNotIndexable]);

  //TestExpr('@''ab''',      fpErrCannotCastToPointer_p);
  ///TestExpr('^T(''ab'')',      fpErrCannotCastToPointer_p);

end;



initialization

  RegisterTest(TTestPascalParser);
end.

