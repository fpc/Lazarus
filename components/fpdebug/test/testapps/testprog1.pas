program Foo;
{$mode objfpc}{$H+}

type
  TTestClass = class
  public
    FWord: Word;
    FBool: Boolean;
    FTest: TTestClass;
  end;

procedure Bar;
var
  int1, int2: Integer;
  pint1, pint2: ^Integer;
  uint1, uint2: Cardinal;
  puint1, puint2: ^Cardinal;
  b1,b2: Byte;
  bool1,bool2: Boolean;
  test: TTestClass;

begin
  int1 := int2;
  pint1 := pint2;
  uint1 := uint2;
  puint1 := puint2;
  b1:=b2;
  bool1 := bool2;
  writeln(int1,uint1,b1,bool1, test.FWord);
end;

begin
  Bar;
end.
