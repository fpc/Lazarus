program Foo;
{$mode objfpc}{$H+}

type

  TStatArray = Array[3..7] of boolean;
  TStatArray2 = Array[3..7, 2..4] of boolean;
  TDynArray = Array of boolean;
  TDynArray2 = Array of Array of boolean;
  PDynArray = ^TDynArray;

  TString25 = String[25];

  TEnum1 = (enum1a,enum1b,enum1c,enum1d);
  PTEnum1 = ^TEnum1;
  TSet1 = set of TEnum1;
  PTSet1 = ^TSet1;

  TTestClass = class;

  { TTestRecord }

  TTestRecord = record
    FWord: Word;
    FBool: Boolean;
    FTest: TTestClass;
  end;
  PTestRecord = ^TTestRecord;

  { TTestClass }

  TTestClass = class
  public
    FWord: Word;
    FBool: Boolean;
    FTest: TTestClass;
    procedure f0(a:integer); virtual;
  end;

  { TTestClass2 }

  TTestClass2 = class(TTestClass)
  public
    FWord2: Word;
    FBool2: Boolean;
    FTest2: TTestClass;
    a1: TStatArray;
    a2: TDynArray;
    a3: Array[3..7, 2..4] of boolean;
    a4: Array of Array of boolean;
    a5: array [1..2] of record x1:boolean; x2:integer; xr: record x1:boolean; x2:integer; end; end;
    a6: array of record x1:boolean; x2:integer; xr: record x1:boolean; x2:integer; end; end;
    a7: array [(a7e1,a7e2,a7e3)] of set of (a7s1,a7s2,a7s3);
    r1: record x1:boolean; x2:integer; xr: record x1:boolean; x2:integer; end; end;
    r2: TTestRecord;
    s1: string[25];
    s2: TString25;
    enum1: (ee1,ee2,ee3);
    set1: set of (se1,se2,se3);
    enum4: ^TEnum1;
    enum5: PTEnum1;
    set4: TSet1;
    set5: ^TSet1;
    procedure f0(a:integer); override;
    procedure f1(a:integer);
    procedure f2(a:integer); virtual;
    procedure f2a(a:integer); virtual; abstract;
    procedure f3(a:integer); dynamic;
  end;
  PTestClass2 = ^TTestClass2;

  TTestObject = object
  public
    FWord: Word;
    FBool: Boolean;
    FTest: TTestClass;
  end;

  TTestObject2 = object(TTestObject)
  public
    FWord2: Word;
    FBool2: Boolean;
    FTest2: TTestClass;
  end;
  PTestObject2 = ^TTestObject2;


  Pint = ^ integer;
  PPInt = ^Pint;
  PPPInt = ^PPint;
  //shortstring = record end;

procedure Bar(ArgClass: TTestClass; var VArgClass: TTestClass; pdarg: PDynArray);
var
  int1, int2: Integer;
  pint1, pint2: ^Integer;
  uint1, uint2: Cardinal;
  puint1, puint2: ^Cardinal;
  b1,b2: Byte;
  bool1,bool2: Boolean;

  TestC: TTestClass;      testC2: TTestClass2;
  PtestC2: PTestClass2;   PtestC2a: ^TTestClass2;
  testO: TTestObject;     testO2: TTestObject2;
  PtestO2: PTestObject2;  PtestO2a: ^TTestObject2;
  TestR: TTestRecord;
  PTestR: PTestRecord;    PTestRa: ^TTestRecord;
  ITestR: record    FWord: Word;    FBool: Boolean;  end;

  s1: string[5];
  s2: string[15];
  s3: string[255];
  st1, st2: ansistring;
  pc1, pc2: pchar;
  pi: Pint;
  ppi: PPint;
  pppi: PPPint;

  a1: TStatArray;
  a2: TDynArray;
  a1p: ^TStatArray;
  a2p: ^TDynArray;
  a1b: TStatArray2;
  a2b: TDynArray2;
  a3: Array[3..7, 2..4] of boolean;
  a4: Array of Array of boolean;

  enum1: TEnum1;
  enum2: enum1b..enum1d;
  enum3: enum1a..enum1c;
  enum4: ^TEnum1;
  enum5: PTEnum1;
  set1: set of byte;
  set2: set of (enum1a,enum1d);
  set3: set of 1..5;
  set4: TSet1;
  set5: ^TSet1;
  set6: PTSet1;
  subr: 1..9;
  subr2: -11..-9;
  subr3: #9..'m';

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
  SetLength(a2,9);
  SetLength(a2b,9,3);
  SetLength(a4,9,3);
  testC2 := TTestClass2.Create;
  a1[3]:= a1b[3,3];
  a1[3]:= a3[3,3];
  a1p := @a1;
  a2p := @a2;
  SetLength(testC2.a2,9);
  SetLength(testC2.a4,9,3);
  testC2.f1(1);
  testC2.f2(1);
  testC2.f3(1);
  testC2.f0(1);
  enum1 := enum1b;
  enum2 := enum1b;
  enum3 := enum1b;
  enum4 := @enum1;
  enum5 := @enum1;
  set1 := [];
  set2 := [];
  set3 := [];
  set4 := [];
  set5 := @set4;
  set6 := @set4;
  subr := 1;
  subr2 := -11;
  subr3 := 'm';
  writeln(int1,uint1,b1,bool1,
          testC.FWord, testC2.FWord, PtestC2^.FWord, PtestC2a^.FWord,
          testO.FWord, testO2.FWord, PtestO2^.FWord, PtestO2a^.FWord,
          testR.FWord, PtestR^.FWord, PtestRa^.FWord
         );
  WriteLn(s1,s2,s3,st1,st2,pc1,pc2, pi^,ppi^^,pppi^^^);
end;

var
  GlobClass: TTestClass;

procedure TTestClass.f0(a: integer);
begin
  //
end;

{ TTestClass2 }

procedure TTestClass2.f0(a: integer);
begin
  inherited f0(a);
end;

procedure TTestClass2.f1(a: integer);
begin
  //
end;

procedure TTestClass2.f2(a: integer);
begin
//
end;

procedure TTestClass2.f3(a: integer);
begin

end;

begin
  Bar(GlobClass, GlobClass, nil);
end.
