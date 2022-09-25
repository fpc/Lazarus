program WatchesFuncPrg;
{$mode objfpc}{$H+}

uses SysUtils;

var
  CurMemUsed: ptruint;

function UsedMem: ptruint;
var
  mm: TMemoryManager;
  hs: TFPCHeapStatus;
  i: integer;
begin
  // ensure global vars are in mem
  GetMemoryManager(mm);
  hs := mm.GetFPCHeapStatus();
  Result := hs.CurrHeapUsed;
end;

var
  LastRes: String;


function FuncResByte(a: Integer): Byte;
begin WriteStr(LastRes, a);  result := Length(LastRes); end;

function FuncResWord(a: Integer): Word;
begin WriteStr(LastRes, a);  result := Length(LastRes); end;

function FuncResInt(a: Integer): Integer;
begin WriteStr(LastRes, a);  result := -Length(LastRes); end;

function FuncResInt64(a: Integer): Int64;
begin WriteStr(LastRes, a);  result := -Length(LastRes); end;




function FuncByte1(a: byte): Integer;
begin WriteStr(LastRes, a);  result := Length(LastRes); end;

function FuncByte2(a,b: byte): Integer;
begin WriteStr(LastRes, a,b);  result := Length(LastRes); end;

function FuncByte12(a,b,c,d,e,f,g,h,i,j,k,l: byte): Integer;
begin WriteStr(LastRes, a,b,c,d,e,f,g,h,i,j,k,l);  result := Length(LastRes); end;


function FuncWord1(a: word): Integer;
begin WriteStr(LastRes, a);  result := Length(LastRes); end;

function FuncWord2(a,b: word): Integer;
begin WriteStr(LastRes, a,b);  result := Length(LastRes); end;

function FuncWord12(a,b,c,d,e,f,g,h,i,j,k,l: word): Integer;
begin WriteStr(LastRes, a,b,c,d,e,f,g,h,i,j,k,l);  result := Length(LastRes); end;


function FuncInt1(a: integer): Integer;
begin WriteStr(LastRes, a);  result := Length(LastRes); end;

function FuncInt2(a,b: integer): Integer;
begin WriteStr(LastRes, a,b);  result := Length(LastRes); end;

function FuncInt12(a,b,c,d,e,f,g,h,i,j,k,l: integer): Integer;
begin WriteStr(LastRes, a,b,c,d,e,f,g,h,i,j,k,l);  result := Length(LastRes); end;


function FuncQWord1(a: QWord): Integer;
begin WriteStr(LastRes, a);  result := Length(LastRes); end;

function FuncQWord2(a,b: QWord): Integer;
begin WriteStr(LastRes, a,b);  result := Length(LastRes); end;

function FuncQWord12(a,b,c,d,e,f,g,h,i,j,k,l: QWord): Integer;
begin WriteStr(LastRes, a,b,c,d,e,f,g,h,i,j,k,l);  result := Length(LastRes); end;



type

  { TFoo }

  TFoo = class
    fb1, fb2: Byte;
    fw1, fw2: Word;
    fc1, fc2: Cardinal;
    fi1, fi2: Integer;
    fx1, fx2: Int64;
    fq1, fq2: QWord;

    function FuncInt12(a,b,c,d,e,f,g,h,i,j,k,l: Integer): Integer;
  end;


function TFoo.FuncInt12(a, b, c, d, e, f, g, h, i, j, k, l: Integer): Integer;
begin WriteStr(LastRes, a,b,c,d,e,f,g,h,i,j,k,l,fb1);  result := Length(LastRes); end;


var
  foo: TFoo;
  b1, b2: Byte;
  w1, w2: Word;
  c1, c2: Cardinal;
  i1, i2: Integer;
  x1, x2: Int64;
  q1, q2: QWord;


begin
  // Call all functions, so they are used in the exe
  foo := TFoo.Create;
  FuncResByte(1);
  FuncResWord(1);
  FuncResInt(1);
  FuncResInt64(1);
  FuncByte1(1);
  FuncByte2(1,2);
  FuncByte12(1,2,3,4,5,6,7,8,9,10,11,12);
  FuncWord1(1);
  FuncWord2(1,2);
  FuncWord12(1,2,3,4,5,6,7,8,9,10,11,12);
  FuncInt1(1);
  FuncInt2(1,2);
  FuncInt12(1,2,3,4,5,6,7,8,9,10,11,12);
  FuncQWord1(1);
  FuncQWord2(1,2);
  FuncQWord12(1,2,3,4,5,6,7,8,9,10,11,12);

  foo.FuncInt12(1,2,3,4,5,6,7,8,9,10,11,12);

  b1 :=  1;
  b2 :=  2;
  w1 := 10;
  w2 := 11;
  c1 := 21;
  c2 := 22;
  i1 := 31;
  i2 := 32;
  x1 := 41;
  x2 := 42;
  q1 := 51;
  q2 := 52;

  foo.fb1 := 201;
  foo.fb2 := 202;
  foo.fw1 := 210;
  foo.fw2 := 211;
  foo.fc1 := 221;
  foo.fc2 := 222;
  foo.fi1 := 231;
  foo.fi2 := 232;
  foo.fx1 := 241;
  foo.fx2 := 242;
  foo.fq1 := 251;
  foo.fq2 := 252;

  FuncQWord12(b1, b2, w1, w2, c1, c2, i1, i2, x1, x2, q1, q2);




  // After each test the debugger can check the memusage
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem; // TEST_BREAKPOINT=main
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;

  FuncQWord12(b1, b2, w1, w2, c1, c2, i1, i2, x1, x2, q1, q2);


end.
