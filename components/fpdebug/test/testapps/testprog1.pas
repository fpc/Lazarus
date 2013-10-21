program Foo;
{$mode objfpc}{$H+}

type
  TTestClass = class
  public
    FWord: Word;
    FBool: Boolean;
    FTest: TTestClass;
  end;

  Pint = ^ integer;
  PPInt = ^Pint;
  PPPInt = ^PPint;
  //shortstring = record end;

procedure Bar;
var
  int1, int2: Integer;
  pint1, pint2: ^Integer;
  uint1, uint2: Cardinal;
  puint1, puint2: ^Cardinal;
  b1,b2: Byte;
  bool1,bool2: Boolean;
  test: TTestClass;

  s1: string[5];
  s2: string[15];
  s3: string[255];
  st1, st2: ansistring;
  pc1, pc2: pchar;
  pi: Pint;
  ppi: PPint;
  pppi: PPPint;

begin
  int1 := int2;
  pint1 := pint2;
  uint1 := uint2;
  puint1 := puint2;
  b1:=b2;
  bool1 := bool2;
  s1:= 'aa';
  st1:= 'bb';
  pc1:= @st1[1];
  writeln(int1,uint1,b1,bool1, test.FWord);
  WriteLn(s1,s2,s3,st1,st2,pc1,pc2, pi^,ppi^^,pppi^^^);
end;

begin
  Bar;
end.
