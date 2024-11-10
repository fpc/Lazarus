unit TestPascalParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, FpPascalParser,
  FpErrorMessages, FpDbgInfo, FpdMemoryTools, LazLogger;

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
    FPrefix: TFpIntrinsicPrefix;
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
  mm: TFpDbgMemManager;
begin
  mm := TFpDbgMemManager.Create(nil, nil, nil, nil);
  ctx := TFpDbgSimpleLocationContext.Create(mm, 0, 8, 0, 0);
  sc := TFpDbgSymbolScope.Create(ctx);
  FreeAndNil(CurrentTestExprObj);
  CurrentTestExprText := t;
  CurrentTestExprObj := TTestFpPascalExpression.Create(CurrentTestExprText, sc, True);
  CurrentTestExprObj.IntrinsicPrefix := FPrefix;
  CurrentTestExprObj.Parse;
DebugLn(CurrentTestExprObj.DebugDump);
  if not SkipExpValid then begin
    s := ErrorHandler.ErrorAsString(CurrentTestExprObj.Error);
    AssertEquals('Valid '+s+ ' # '+CurrentTestExprText, ExpValid, CurrentTestExprObj.Valid);
  end;
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
    AText: String; AChildCount: Integer = -1; AFullText: String = '');
  begin
    AssertNotNull(CurrentTestExprText+ ': IsAssigned', APart);
    AssertTrue(CurrentTestExprText+': APart IS Class exp: '+AClass.ClassName+' was: '+APart.ClassName,
               APart is AClass);
    AssertEquals(CurrentTestExprText+': Text', AText, APart.GetText);
    if AFullText <> '' then
      AssertEquals(CurrentTestExprText+': FullText', AFullText, APart.GetFullText);
    if AChildCount >=0 then begin
      AssertTrue(CurrentTestExprText+': Is container ', APart is TFpPascalExpressionPartContainer);
      AssertEquals(CurrentTestExprText+': childcount ', AChildCount, (APart as TFpPascalExpressionPartContainer).Count);
    end;
  end;

  Procedure TestExpr(i: array of integer; AClass: TFpPascalExpressionPartClass;
    AText: String; AChildCount: Integer = -1; AFullText: String = '');
  begin
    TestExpr(GetChild(i), AClass, AText, AChildCount, AFullText);
  end;

  Procedure TestSlOp(i: array of integer; AText: String);
  var
    APart: TFpPascalExpressionPart;
  begin
    APart := GetChild(i);

    AssertNotNull(CurrentTestExprText+ ': IsAssigned', APart);
    AssertTrue(CurrentTestExprText+': APart IS Class exp: TFpPascalExpressionPartOperatorArraySliceController was: '+APart.ClassName,
               APart is TFpPascalExpressionPartOperatorArraySliceController);

    AssertTrue(CurrentTestExprText+' text='+AText,  strlcomp(APart.StartChar, PChar(AText), Length(AText) )=0);
    AssertEquals(CurrentTestExprText+': childcount ', 1, (APart as TFpPascalExpressionPartContainer).Count);
  end;

var
  i, ip1: Integer;
  s: String;
  ip: Char;
begin
  FPrefix := ipColon;
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

    CreateExpr('f((a))', True);
    TestExpr([],    TFpPascalExpressionPartBracketArgumentList, '(', 2);
    TestExpr([0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([1], TFpPascalExpressionPartBracketSubExpression, '(', 1);
    TestExpr([1,0],   TFpPascalExpressionPartIdentifier, 'a', 0);

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

    CreateExpr('f((a),b)', True);
    TestExpr([],    TFpPascalExpressionPartBracketArgumentList, '(', 3);
    TestExpr([0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([1], TFpPascalExpressionPartBracketSubExpression, '(', 1);
    TestExpr([1,0],   TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([2],   TFpPascalExpressionPartIdentifier, 'b', 0);

    CreateExpr('f(a,(b))', True);
    TestExpr([],    TFpPascalExpressionPartBracketArgumentList, '(', 3);
    TestExpr([0],   TFpPascalExpressionPartIdentifier, 'f', 0);
    TestExpr([1],   TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([2], TFpPascalExpressionPartBracketSubExpression, '(', 1);
    TestExpr([2,0],   TFpPascalExpressionPartIdentifier, 'b', 0);

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


    for i := 0 to 3 do begin
      s := StringOfChar('!', i);
      CreateExpr('a[1..2]'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([], '..2');
      TestExpr([0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '1..2');
      TestExpr([0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);

      CreateExpr('a[1..2]+9'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestExpr([0], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
      TestExpr([0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '1..2');
      TestExpr([0,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      TestExpr([0,1], TFpPascalExpressionPartConstantNumber, '9', 0);


      CreateExpr('a[1..2,3..4]'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('1..2',  strlcomp(GetChild([]).StartChar, PChar('..2'), 3 )=0);
      TestExpr([0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('3..4',  strlcomp(GetChild([0]).StartChar, PChar('..4'), 3 )=0);
      //
      TestExpr([0,0], TFpPascalExpressionPartBracketIndex, '[', 3);
      TestExpr([0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '1..2');
      TestExpr([0,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      //
      TestExpr([0,0,2], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '3..4');
      TestExpr([0,0,2,0], TFpPascalExpressionPartConstantNumber, '3', 0);
      TestExpr([0,0,2,1], TFpPascalExpressionPartConstantNumber, '4', 0);


      CreateExpr('a[1..2][3..4]'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('1..2',  strlcomp(GetChild([]).StartChar, PChar('..2'), 3 )=0);
      TestExpr([0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('3..4',  strlcomp(GetChild([0]).StartChar, PChar('..4'), 3 )=0);
      //
      TestExpr([0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '1..2');
      TestExpr([0,0,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,0,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      //
      TestExpr([0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '3..4');
      TestExpr([0,0,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
      TestExpr([0,0,1,1], TFpPascalExpressionPartConstantNumber, '4', 0);


      CreateExpr('a[1..2]![3..4]'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('3..4',  strlcomp(GetChild([]).StartChar, PChar('..4'), 3 )=0);
      TestExpr([0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('1..2',  strlcomp(GetChild([0,0]).StartChar, PChar('..2'), 3 )=0);
      //
      TestExpr([0,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '1..2');
      TestExpr([0,0,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,0,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      //
      TestExpr([0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '3..4');
      TestExpr([0,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
      TestExpr([0,1,1], TFpPascalExpressionPartConstantNumber, '4', 0);


      CreateExpr('a[1..2][3..4]+9'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('1..2',  strlcomp(GetChild([]).StartChar, PChar('..2'), 3 )=0);
      TestExpr([0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('3..4',  strlcomp(GetChild([0]).StartChar, PChar('..4'), 3 )=0);
      TestExpr([0,0], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
      TestExpr([0,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      //
      TestExpr([0,0,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,0,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '1..2');
      TestExpr([0,0,0,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,0,0,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      //
      TestExpr([0,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '3..4');
      TestExpr([0,0,0,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
      TestExpr([0,0,0,1,1], TFpPascalExpressionPartConstantNumber, '4', 0);
      //
      TestExpr([0,0,1], TFpPascalExpressionPartConstantNumber, '9', 0);


      CreateExpr('a[1..2][3..4]!+9'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('1..2',  strlcomp(GetChild([]).StartChar, PChar('..2'), 3 )=0);
      TestExpr([0], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
      TestExpr([0,0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('3..4',  strlcomp(GetChild([0,0]).StartChar, PChar('..4'), 3 )=0);
      //
      TestExpr([0,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      //
      TestExpr([0,0,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,0,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '1..2');
      TestExpr([0,0,0,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,0,0,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      //
      TestExpr([0,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '3..4');
      TestExpr([0,0,0,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
      TestExpr([0,0,0,1,1], TFpPascalExpressionPartConstantNumber, '4', 0);
      //
      TestExpr([0,1], TFpPascalExpressionPartConstantNumber, '9', 0);


      CreateExpr('a[1..2][3..4]!!+9'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
      TestExpr([0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('1..2',  strlcomp(GetChild([0]).StartChar, PChar('..2'), 3 )=0);
      TestExpr([0,0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('3..4',  strlcomp(GetChild([0,0]).StartChar, PChar('..4'), 3 )=0);
      //
      TestExpr([0,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      //
      TestExpr([0,0,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,0,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '1..2');
      TestExpr([0,0,0,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,0,0,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      //
      TestExpr([0,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '3..4');
      TestExpr([0,0,0,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
      TestExpr([0,0,0,1,1], TFpPascalExpressionPartConstantNumber, '4', 0);
      //
      TestExpr([1], TFpPascalExpressionPartConstantNumber, '9', 0);


      CreateExpr('a[1..2]+b[3..4]'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('1..2',  strlcomp(GetChild([]).StartChar, PChar('..2'), 3 )=0);
      TestExpr([0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('3..4',  strlcomp(GetChild([0]).StartChar, PChar('..4'), 3 )=0);
      TestExpr([0,0], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
      //
      TestExpr([0,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '1..2');
      TestExpr([0,0,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,0,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      //
      TestExpr([0,0,1], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,1,0], TFpPascalExpressionPartIdentifier, 'b', 0);
      TestExpr([0,0,1,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '3..4');
      TestExpr([0,0,1,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
      TestExpr([0,0,1,1,1], TFpPascalExpressionPartConstantNumber, '4', 0);


      CreateExpr('a[b[1..2]..3]'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([], '..2');
      TestExpr([0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([0], '..3');
      //
      TestExpr([0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, 'b[1..2]..3');
        TestExpr([0,0,1,0], TFpPascalExpressionPartBracketIndex, '[', 2);
        TestExpr([0,0,1,0,0], TFpPascalExpressionPartIdentifier, 'b', 0);
        TestExpr([0,0,1,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '1..2');
          TestExpr([0,0,1,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
          TestExpr([0,0,1,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
        TestExpr([0,0,1,1], TFpPascalExpressionPartConstantNumber, '3', 0);


      CreateExpr('a[b[1..2]!..3]'+s, True);  // Error when executing, but ...
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('..3',  strlcomp(GetChild([]).StartChar, PChar('..3'), 3 )=0);
      //
      TestExpr([0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, 'b[1..2]!..3');
      TestExpr([0,1,0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('1..2',  strlcomp(GetChild([0,1,0]).StartChar, PChar('..2'), 3 )=0);
      TestExpr([0,1,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,1,0,0,0], TFpPascalExpressionPartIdentifier, 'b', 0);
      TestExpr([0,1,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '1..2');
      TestExpr([0,1,0,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,1,0,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      TestExpr([0,1,1], TFpPascalExpressionPartConstantNumber, '3', 0);


      CreateExpr('a[3..b[1..2]]'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([], '..2');
      TestExpr([0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([0], '..b');
      TestExpr([0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '3..b[1..2]');
        TestExpr([0,0,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
        TestExpr([0,0,1,1], TFpPascalExpressionPartBracketIndex, '[', 2);
        TestExpr([0,0,1,1,0], TFpPascalExpressionPartIdentifier, 'b', 0);
        TestExpr([0,0,1,1,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '1..2');
          TestExpr([0,0,1,1,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
          TestExpr([0,0,1,1,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);


      CreateExpr('a[3..b[1..2]!]'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('3..',  strlcomp(GetChild([]).StartChar-1, PChar('3..'), 3 )=0);
      TestExpr([0], TFpPascalExpressionPartBracketIndex, '[', 2, 'a[3..b[1..2]!]');
      TestExpr([0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '3..b[1..2]');
      TestExpr([0,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
      TestExpr([0,1,1], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      AssertTrue('1..2',  strlcomp(GetChild([0,1,1]).StartChar, PChar('..2'), 3 )=0);
      TestExpr([0,1,1,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,1,1,0,0], TFpPascalExpressionPartIdentifier, 'b', 0);
      TestExpr([0,1,1,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '1..2');
      TestExpr([0,1,1,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,1,1,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);


      for ip1 := 0 to 1 do begin
        ip := ':';
        FPrefix := ipColon;
        if ip1 = 1 then begin
          ip := '!';
            FPrefix := ipExclamation;
        end;


      CreateExpr(ip+'length(a[1..2])'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([], '..2');
      TestExpr([0], TFpPascalExpressionPartBracketArgumentList, '(', 2);
      TestExpr([0,0], TFpPascalExpressionPartIntrinsic, 'length(', 0);
      TestExpr([0,1], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,1,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,1,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2, '1..2');
      TestExpr([0,1,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,1,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);


      CreateExpr(ip+'length(a[1..2]!)'+s, True);
      TestExpr([], TFpPascalExpressionPartBracketArgumentList, '(', 2);
      TestExpr([0], TFpPascalExpressionPartIntrinsic, 'length(', 0);
      TestExpr([1], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([1], '..2');
      TestExpr([1,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([1,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([1,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([1,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([1,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);


      CreateExpr(ip+'length(8+a[1..2])'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([], '..2');
      TestExpr([0], TFpPascalExpressionPartBracketArgumentList, '(', 2);
      TestExpr([0,0], TFpPascalExpressionPartIntrinsic, 'length(', 0);
      TestExpr([0,1], TFpPascalExpressionPartOperator, '+', 2);
      TestExpr([0,1,0], TFpPascalExpressionPartConstantNumber, '8', 0);
      TestExpr([0,1,1], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,1,1,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,1,1,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,1,1,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,1,1,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);


      CreateExpr(ip+'length(8+a[1..2]!)'+s, True);
      TestExpr([], TFpPascalExpressionPartBracketArgumentList, '(', 2);
      TestExpr([0], TFpPascalExpressionPartIntrinsic, 'length(', 0);
      TestExpr([1], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([1], '..2');
      TestExpr([1,0], TFpPascalExpressionPartOperator, '+', 2);
      TestExpr([1,0,0], TFpPascalExpressionPartConstantNumber, '8', 0);
      TestExpr([1,0,1], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([1,0,1,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([1,0,1,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([1,0,1,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([1,0,1,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);


      CreateExpr(ip+'length(a[1..2])[3..4]'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([], '..2');
      TestExpr([0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([0], '..4');
      TestExpr([0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      //
      TestExpr([0,0,0], TFpPascalExpressionPartBracketArgumentList, '(', 2);
      TestExpr([0,0,0,0], TFpPascalExpressionPartIntrinsic, 'length(', 0);
      TestExpr([0,0,0,1], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,0,1,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,0,0,1,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,0,0,1,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,0,0,1,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      //
      TestExpr([0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,0,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
      TestExpr([0,0,1,1], TFpPascalExpressionPartConstantNumber, '4', 0);


      CreateExpr(ip+'length(a[1..2]!)[3..4]'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([], '..4');
      TestExpr([0], TFpPascalExpressionPartBracketIndex, '[', 2);
      //
      TestExpr([0,0], TFpPascalExpressionPartBracketArgumentList, '(', 2);
      TestExpr([0,0,0], TFpPascalExpressionPartIntrinsic, 'length(', 0);
      TestExpr([0,0,1], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([0,0,1], '..2');
      TestExpr([0,0,1,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,1,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,0,1,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,0,1,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,0,1,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      //
      TestExpr([0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
      TestExpr([0,1,1], TFpPascalExpressionPartConstantNumber, '4', 0);


      CreateExpr(ip+'length(a[1..2])![3..4]'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([], '..4');
      TestExpr([0], TFpPascalExpressionPartBracketIndex, '[', 2);
      //
      TestExpr([0,0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([0,0], '..2');
      TestExpr([0,0,0], TFpPascalExpressionPartBracketArgumentList, '(', 2);
      TestExpr([0,0,0,0], TFpPascalExpressionPartIntrinsic, 'length(', 0);
      TestExpr([0,0,0,1], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,0,1,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,0,0,1,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,0,0,1,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,0,0,1,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      //
      TestExpr([0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
      TestExpr([0,1,1], TFpPascalExpressionPartConstantNumber, '4', 0);


      CreateExpr(ip+'f_(a,b[1..2]:c[3..4])'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([], '..2');
      TestExpr([0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([0], '..4');
      TestExpr([0,0], TFpPascalExpressionPartBracketArgumentList, '(', 3);
      TestExpr([0,0,0], TFpPascalExpressionPartIntrinsic, 'f_(', 0);
      TestExpr([0,0,1], TFpPascalExpressionPartIdentifier, 'a', 0);
      //TestExpr([0,0,2], TFpPascalExpressionPartOperatorColonAsSeparator, 'a', 0);
      TestExpr([0,0,2,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,2,0,0], TFpPascalExpressionPartIdentifier, 'b', 0);
      TestExpr([0,0,2,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,0,2,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,0,2,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      TestExpr([0,0,2,1], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,0,2,1,0], TFpPascalExpressionPartIdentifier, 'c', 0);
      TestExpr([0,0,2,1,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,0,2,1,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
      TestExpr([0,0,2,1,1,1], TFpPascalExpressionPartConstantNumber, '4', 0);


      CreateExpr(ip+'f_(a,b[1..2]!:c[3..4])'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([], '..4');
      TestExpr([0], TFpPascalExpressionPartBracketArgumentList, '(', 3);
      TestExpr([0,0], TFpPascalExpressionPartIntrinsic, 'f_(', 0);
      TestExpr([0,1], TFpPascalExpressionPartIdentifier, 'a', 0);
      //TestExpr([0,2], TFpPascalExpressionPartOperatorColonAsSeparator, 'a', 0);
      TestExpr([0,2,0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([0,2,0], '..2');
      TestExpr([0,2,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,2,0,0,0], TFpPascalExpressionPartIdentifier, 'b', 0);
      TestExpr([0,2,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,2,0,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,2,0,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      TestExpr([0,2,1], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,2,1,0], TFpPascalExpressionPartIdentifier, 'c', 0);
      TestExpr([0,2,1,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,2,1,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
      TestExpr([0,2,1,1,1], TFpPascalExpressionPartConstantNumber, '4', 0);


      CreateExpr(ip+'f_(a,b[1..2]:c[3..4]!)'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([], '..2');
      TestExpr([0], TFpPascalExpressionPartBracketArgumentList, '(', 3);
      TestExpr([0,0], TFpPascalExpressionPartIntrinsic, 'f_(', 0);
      TestExpr([0,1], TFpPascalExpressionPartIdentifier, 'a', 0);
      //TestExpr([0,2], TFpPascalExpressionPartOperatorColonAsSeparator, 'a', 0);
      TestExpr([0,2,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,2,0,0], TFpPascalExpressionPartIdentifier, 'b', 0);
      TestExpr([0,2,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,2,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,2,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      TestExpr([0,2,1], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestSlOp([0,2,1], '..4');
      TestExpr([0,2,1,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,2,1,0,0], TFpPascalExpressionPartIdentifier, 'c', 0);
      TestExpr([0,2,1,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,2,1,0,1,0], TFpPascalExpressionPartConstantNumber, '3', 0);
      TestExpr([0,2,1,0,1,1], TFpPascalExpressionPartConstantNumber, '4', 0);


      CreateExpr('a ? b[1..2] : c'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestExpr([0], TFpPascalExpressionPartOperatorQuestionMark, '?', 2);
      TestExpr([0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,1], TFpPascalExpressionPartOperatorColon, ':', 2);
      TestExpr([0,1,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,1,0,0], TFpPascalExpressionPartIdentifier, 'b', 0);
      TestExpr([0,1,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,1,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,1,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      TestExpr([0,1,1], TFpPascalExpressionPartIdentifier, 'c', 0);

      CreateExpr('a ? b[1..2]! : c'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorQuestionMark, '?', 2);
      TestExpr([0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([1], TFpPascalExpressionPartOperatorColon, ':', 2);
      TestExpr([1,0], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestExpr([1,0,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([1,0,0,0], TFpPascalExpressionPartIdentifier, 'b', 0);
      TestExpr([1,0,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([1,0,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([1,0,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      TestExpr([1,1], TFpPascalExpressionPartIdentifier, 'c', 0);


      if s = '' then begin
      CreateExpr('a ? b : c[1..2]'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestExpr([0], TFpPascalExpressionPartOperatorQuestionMark, '?', 2);
      TestExpr([0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,1], TFpPascalExpressionPartOperatorColon, ':', 2);
      TestExpr([0,1,0], TFpPascalExpressionPartIdentifier, 'b', 0);
      TestExpr([0,1,1], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([0,1,1,0], TFpPascalExpressionPartIdentifier, 'c', 0);
      TestExpr([0,1,1,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([0,1,1,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([0,1,1,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);
      end;

      CreateExpr('a ? b : c[1..2]!'+s, True);
      TestExpr([], TFpPascalExpressionPartOperatorQuestionMark, '?', 2);
      TestExpr([0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([1], TFpPascalExpressionPartOperatorColon, ':', 2);
      TestExpr([1,0], TFpPascalExpressionPartIdentifier, 'b', 0);
      TestExpr([1,1], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
      TestExpr([1,1,0], TFpPascalExpressionPartBracketIndex, '[', 2);
      TestExpr([1,1,0,0], TFpPascalExpressionPartIdentifier, 'c', 0);
      TestExpr([1,1,0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);
      TestExpr([1,1,0,1,0], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([1,1,0,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);

    end;
    end;
    FPrefix := ipColon;

    CreateExpr('a ? b : c', True);
    TestExpr([], TFpPascalExpressionPartOperatorQuestionMark, '?', 2);
    TestExpr([0], TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([1], TFpPascalExpressionPartOperatorColon, ':', 2);
    TestExpr([1,0], TFpPascalExpressionPartIdentifier, 'b', 0);
    TestExpr([1,1], TFpPascalExpressionPartIdentifier, 'c', 0);


    CreateExpr('(a ? d : e)      ?      b*1 ? x+1 : y?r:s+2      :      c+9  ?  -m?+k:+j  :  not n?o:p ', True);
    TestExpr([], TFpPascalExpressionPartOperatorQuestionMark, '?', 2);
    TestExpr([0], TFpPascalExpressionPartBracketSubExpression, '(', 1);
      TestExpr([0,0], TFpPascalExpressionPartOperatorQuestionMark, '?', 2);
      TestExpr([0,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
      TestExpr([0,0,1], TFpPascalExpressionPartOperatorColon, ':', 2);
      TestExpr([0,0,1,0], TFpPascalExpressionPartIdentifier, 'd', 0);
      TestExpr([0,0,1,1], TFpPascalExpressionPartIdentifier, 'e', 0);

    TestExpr([1], TFpPascalExpressionPartOperatorColon, ':', 2);
      TestExpr([1,0], TFpPascalExpressionPartOperatorQuestionMark, '?', 2);  // b*1 ?
      TestExpr([1,0,0], TFpPascalExpressionPartOperatorMulDiv, '*', 2);
      TestExpr([1,0,0,0], TFpPascalExpressionPartIdentifier, 'b', 0);
      TestExpr([1,0,0,1], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([1,0,1], TFpPascalExpressionPartOperatorColon, ':', 2);
      TestExpr([1,0,1,0], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);  // x+1
      TestExpr([1,0,1,0,0], TFpPascalExpressionPartIdentifier, 'x', 0);
      TestExpr([1,0,1,0,1], TFpPascalExpressionPartConstantNumber, '1', 0);
      TestExpr([1,0,1,1], TFpPascalExpressionPartOperatorQuestionMark, '?', 2);  // y?r:s+2
      TestExpr([1,0,1,1,0], TFpPascalExpressionPartIdentifier, 'y', 0);
      TestExpr([1,0,1,1,1], TFpPascalExpressionPartOperatorColon, ':', 2);
      TestExpr([1,0,1,1,1,0], TFpPascalExpressionPartIdentifier, 'r', 0);
      TestExpr([1,0,1,1,1,1], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
      TestExpr([1,0,1,1,1,1,0], TFpPascalExpressionPartIdentifier, 's', 0);
      TestExpr([1,0,1,1,1,1,1], TFpPascalExpressionPartConstantNumber, '2', 0);

      // c+9  ?
      TestExpr([1,1], TFpPascalExpressionPartOperatorQuestionMark, '?', 2); // c+9  ?
      TestExpr([1,1,0], TFpPascalExpressionPartOperatorPlusMinus, '+', 2);
      TestExpr([1,1,0,0], TFpPascalExpressionPartIdentifier, 'c', 0);
      TestExpr([1,1,0,1], TFpPascalExpressionPartConstantNumber, '9', 0);
      TestExpr([1,1,1], TFpPascalExpressionPartOperatorColon, ':', 2);
      //  -m?+k:+j
      TestExpr([1,1,1,0], TFpPascalExpressionPartOperatorQuestionMark, '?', 2); // -m ?
      TestExpr([1,1,1,0,0], TFpPascalExpressionPartOperatorUnaryPlusMinus, '-', 1);
      TestExpr([1,1,1,0,0,0], TFpPascalExpressionPartIdentifier, 'm', 0);
      TestExpr([1,1,1,0,1], TFpPascalExpressionPartOperatorColon, ':', 2);
      TestExpr([1,1,1,0,1,0], TFpPascalExpressionPartOperatorUnaryPlusMinus, '+', 1);
      TestExpr([1,1,1,0,1,0,0], TFpPascalExpressionPartIdentifier, 'k', 0);
      TestExpr([1,1,1,0,1,1], TFpPascalExpressionPartOperatorUnaryPlusMinus, '+', 1);
      TestExpr([1,1,1,0,1,1,0], TFpPascalExpressionPartIdentifier, 'j', 0);

      //  not n?o:p
      TestExpr([1,1,1,1], TFpPascalExpressionPartOperatorQuestionMark, '?', 2); // not n ?
      TestExpr([1,1,1,1,0], TFpPascalExpressionPartOperatorUnaryNot, 'not', 1);
      TestExpr([1,1,1,1,0,0], TFpPascalExpressionPartIdentifier, 'n', 0);
      TestExpr([1,1,1,1,1], TFpPascalExpressionPartOperatorColon, ':', 2);
      TestExpr([1,1,1,1,1,0], TFpPascalExpressionPartIdentifier, 'o', 0);
      TestExpr([1,1,1,1,1,1], TFpPascalExpressionPartIdentifier, 'p', 0);



    CreateExpr('x[a ? b : c .. d ? e : f]', True);
    TestExpr([], TFpPascalExpressionPartOperatorArraySliceController, '..', 1);
    TestExpr([0], TFpPascalExpressionPartBracketIndex, '[', 2);
    TestExpr([0,0], TFpPascalExpressionPartIdentifier, 'x', 0);
    TestExpr([0,1], TFpPascalExpressionPartOperatorArraySlice, '..', 2);

    TestExpr([0,1,0], TFpPascalExpressionPartOperatorQuestionMark, '?', 2);
    TestExpr([0,1,0,0], TFpPascalExpressionPartIdentifier, 'a', 0);
    TestExpr([0,1,0,1], TFpPascalExpressionPartOperatorColon, ':', 2);
    TestExpr([0,1,0,1,0], TFpPascalExpressionPartIdentifier, 'b', 0);
    TestExpr([0,1,0,1,1], TFpPascalExpressionPartIdentifier, 'c', 0);

    TestExpr([0,1,1], TFpPascalExpressionPartOperatorQuestionMark, '?', 2);
    TestExpr([0,1,1,0], TFpPascalExpressionPartIdentifier, 'd', 0);
    TestExpr([0,1,1,1], TFpPascalExpressionPartOperatorColon, ':', 2);
    TestExpr([0,1,1,1,0], TFpPascalExpressionPartIdentifier, 'e', 0);
    TestExpr([0,1,1,1,1], TFpPascalExpressionPartIdentifier, 'f', 0);


    CreateExpr(':obj(b:3)', True);
    CreateExpr(':obj(b:3,c:-4)', True);
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
  FPrefix := ipColon;
  // self test
  s := ErrorHandler.ErrorAsString(CreateError(-1));
  AssertFalse('self test format error msg', pos('Internal Error:', s) < 1);
  s := ErrorHandler.ErrorAsString(CreateError(fpErrPasParserUnexpectedToken_p, []));
  AssertFalse('self test format error msg', pos('Internal Error:', s) < 1);


  //TestExpr('(a+2*)',     fpErrPasParser);
  CreateExpr('a+', False);
  CreateExpr('a*', False);
  CreateExpr('3 a*', False);
  CreateExpr('3 * 3 a*', False);
  CreateExpr('*a', False);
  CreateExpr('*a 3', False);
  CreateExpr('*a 3 * 3', False);
  CreateExpr('*a 3 3', False);
  CreateExpr('a+2*', False);
  CreateExpr('a*2+', False);
  CreateExpr('(a+2*)', False);
  CreateExpr('(a+2*)', False);
  CreateExpr('a+(* 3 3)', False);
  CreateExpr('a+(* 3 * 3)', False);
  CreateExpr('a+(3 3 *)', False);
  CreateExpr('a+(3 * 3 *)', False);
  CreateExpr('()', False);
  CreateExpr('-()', False);
  CreateExpr('()+1', False);
  CreateExpr('1+()', False);
  CreateExpr('f(a+2*)', False);
  CreateExpr('f(1,a+2*)', False);
  CreateExpr('f(1,a+2*)', False);
  CreateExpr('f(a+2*)', False);
  CreateExpr('f(a+2*,1)', False);
  CreateExpr('f(a+2*,1)', False);
  CreateExpr('f(* 3)', False);
  CreateExpr('f(* 3 3)', False);

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
  //TestExpr('''abc''[#1]',  [fpErrPasParserIndexError_Wrapper, fpErrExpectedOrdinalVal_p]);
  TestExpr('''abc''[99]',  [fpErrPasParserIndexError_Wrapper, fpErrIndexOutOfRange]);
  TestExpr('1[99]',        [fpErrPasParserIndexError_Wrapper, fpErrTypeNotIndexable]);

  CreateExpr('a((a,b)', False, False);
  CreateExpr('a((a,b,c)', False, False);

  //TestExpr('@''ab''',      fpErrCannotCastToPointer_p);
  ///TestExpr('^T(''ab'')',      fpErrCannotCastToPointer_p);


  CreateExpr('a ? b ', False);
  CreateExpr('a ? b :', False);
  CreateExpr('a ? : c', False);
  CreateExpr('a ? ', False);
  CreateExpr(' ? b : c', False);
  CreateExpr('b : c', False);
  CreateExpr('a ? b ? d : c', False);
  CreateExpr('a ? b : c ? d', False);
  CreateExpr('a ? b : c : d', False);

  CreateExpr('(a ? b )+1', False);
  CreateExpr('(a ? b :)+1', False);
  CreateExpr('(a ? : c)+1', False);
  CreateExpr('(a ? )+1', False);
  CreateExpr('( ? b : c)+1', False);
  CreateExpr('(b : c)+1', False);
  CreateExpr('(a ? b ? d : c)+1', False);
  CreateExpr('(a ? b : c ? d)+1', False);
  CreateExpr('(a ? b : c : d)+1', False);

  CreateExpr(':obj(1)', False);
  CreateExpr(':obj(b)', False);
  CreateExpr(':obj(b+1:3)', False);

  CreateExpr('..', False);
  CreateExpr('1..', False);
  CreateExpr('..2', False);
  CreateExpr('..]', False);
  CreateExpr('1..]', False);
  CreateExpr('..2]', False);
  CreateExpr('a[..', False);
  CreateExpr('a[1..', False);
  CreateExpr('a[..2', False);
  CreateExpr('a[..]', False);
  CreateExpr('a[1..]', False);
  CreateExpr('a[..2]', False);

  CreateExpr('b[a[..]..1', False);
  CreateExpr('b[a[1..]..1', False);
  CreateExpr('b[a[..2]..1', False);
  CreateExpr('a[..]..1', False);
  CreateExpr('a[1..]..1', False);
  CreateExpr('a[..2]..1', False);
  CreateExpr('b[1..a[..]', False);
  CreateExpr('b[1..a[1..]', False);
  CreateExpr('b[1..a[..2]', False);

  CreateExpr('!', False);
  CreateExpr('-!', False);
  CreateExpr('(!)', False);
  CreateExpr('b[1..2]+!', False);
  CreateExpr('b[1..2]+(!)', False);
end;



initialization

  RegisterTest(TTestPascalParser);
end.

