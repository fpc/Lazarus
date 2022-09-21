program WatchesFuncRecordPrg;
{$mode objfpc}{$H+}

uses SysUtils;

type
  TNibble = 0..15;

  {$IFDEF RECPAD2}
  TPad = word;
  {$ELSE}
  TPad = byte;
  {$ENDIF}
  TRecN2 = {$IFDEF PCKREC} packed {$ENDIF} record {$IFDEF RECPAD1} xxx: TPad; {$ENDIF} a, b: TNibble; end;
  TRecB2 = {$IFDEF PCKREC} packed {$ENDIF} record {$IFDEF RECPAD1} xxx: TPad; {$ENDIF} a, b: Byte; end;
  TRecW2 = {$IFDEF PCKREC} packed {$ENDIF} record {$IFDEF RECPAD1} xxx: TPad; {$ENDIF} a, b: Word; end;
  TRecC2 = {$IFDEF PCKREC} packed {$ENDIF} record {$IFDEF RECPAD1} xxx: TPad; {$ENDIF} a, b: Cardinal; end;
  TRecQ2 = {$IFDEF PCKREC} packed {$ENDIF} record {$IFDEF RECPAD1} xxx: TPad; {$ENDIF} a, b: QWord; end;
  TRecB3 = {$IFDEF PCKREC} packed {$ENDIF} record {$IFDEF RECPAD1} xxx: TPad; {$ENDIF} a, b, c: Byte; end;
  TRecW3 = {$IFDEF PCKREC} packed {$ENDIF} record {$IFDEF RECPAD1} xxx: TPad; {$ENDIF} a, b, c: Word; end;
  TRecC3 = {$IFDEF PCKREC} packed {$ENDIF} record {$IFDEF RECPAD1} xxx: TPad; {$ENDIF} a, b, c: Cardinal; end;
  TRecQ3 = {$IFDEF PCKREC} packed {$ENDIF} record {$IFDEF RECPAD1} xxx: TPad; {$ENDIF} a, b, c: QWord; end;
  TRecQ4 = {$IFDEF PCKREC} packed {$ENDIF} record {$IFDEF RECPAD1} xxx: TPad; {$ENDIF} a, b, c, d: QWord; end;
  TRecQ5 = {$IFDEF PCKREC} packed {$ENDIF} record {$IFDEF RECPAD1} xxx: TPad; {$ENDIF} a, b, c, d ,e: QWord; end;
  TRecQ6 = {$IFDEF PCKREC} packed {$ENDIF} record {$IFDEF RECPAD1} xxx: TPad; {$ENDIF} a, b, c, d ,e, f: QWord; end;
  TRecQ7 = {$IFDEF PCKREC} packed {$ENDIF} record {$IFDEF RECPAD1} xxx: TPad; {$ENDIF} a, b, c, d ,e, f, g: QWord; end;

var
  CurMemUsed: ptruint;
  SomeInt: Integer;

  aRecN2, bRecN2: TRecN2;
  aRecB2, bRecB2: TRecB2;
  aRecW2, bRecW2: TRecW2;
  aRecC2, bRecC2: TRecC2;
  aRecQ2, bRecQ2: TRecQ2;
  aRecB3, bRecB3: TRecB3;
  aRecW3, bRecW3: TRecW3;
  aRecC3, bRecC3: TRecC3;
  aRecQ3, bRecQ3: TRecQ3;

  aRecQ4, bRecQ4: TRecQ4;
  aRecQ5, bRecQ5: TRecQ5;
  aRecQ6, bRecQ6: TRecQ6;
  aRecQ7, bRecQ7: TRecQ7;


function TestRecN2_a(AVal: TRecN2): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecN2_b(AVal: TRecN2): Cardinal;  begin  Result := AVal.b; AVal.a:=0;  end;

function TestRecB2_a(AVal: TRecB2): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecB2_b(AVal: TRecB2): Cardinal;  begin  Result := AVal.b; AVal.a:=0;  end;
function TestRecB3_a(AVal: TRecB3): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecB3_b(AVal: TRecB3): Cardinal;  begin  Result := AVal.b; AVal.a:=0;  end;
function TestRecB3_c(AVal: TRecB3): Cardinal;  begin  Result := AVal.c; AVal.a:=0;  end;

function TestRecW2_a(AVal: TRecW2): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecW2_b(AVal: TRecW2): Cardinal;  begin  Result := AVal.b; AVal.a:=0;  end;
function TestRecW3_a(AVal: TRecW3): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecW3_b(AVal: TRecW3): Cardinal;  begin  Result := AVal.b; AVal.a:=0;  end;
function TestRecW3_c(AVal: TRecW3): Cardinal;  begin  Result := AVal.c; AVal.a:=0;  end;

function TestRecC2_a(AVal: TRecC2): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecC2_b(AVal: TRecC2): Cardinal;  begin  Result := AVal.b; AVal.a:=0;  end;
function TestRecC3_a(AVal: TRecC3): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecC3_b(AVal: TRecC3): Cardinal;  begin  Result := AVal.b; AVal.a:=0;  end;
function TestRecC3_c(AVal: TRecC3): Cardinal;  begin  Result := AVal.c; AVal.a:=0;  end;

function TestRecQ2_a(AVal: TRecQ2): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecQ2_b(AVal: TRecQ2): Cardinal;  begin  Result := AVal.b; AVal.a:=0;  end;
function TestRecQ3_a(AVal: TRecQ3): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecQ3_b(AVal: TRecQ3): Cardinal;  begin  Result := AVal.b; AVal.a:=0;  end;
function TestRecQ3_c(AVal: TRecQ3): Cardinal;  begin  Result := AVal.c; AVal.a:=0;  end;

function Test1RecB2(AVal: TRecB2; i: byte): Cardinal;  begin if i=0 then Result := AVal.a else Result := AVal.b; AVal.a:=0;  end;
function Test1RecW2(AVal: TRecW2; i: byte): Cardinal;  begin if i=0 then Result := AVal.a else Result := AVal.b; AVal.a:=0;  end;
function Test1RecC2(AVal: TRecC2; i: byte): Cardinal;  begin if i=0 then Result := AVal.a else Result := AVal.b; AVal.a:=0;  end;
function Test1RecQ2(AVal: TRecQ2; i: byte): Cardinal;  begin if i=0 then Result := AVal.a else Result := AVal.b; AVal.a:=0;  end;
function Test2RecB2(i: word; AVal: TRecB2): Cardinal;  begin if i=0 then Result := AVal.a else Result := AVal.b; AVal.a:=0;  end;
function Test2RecW2(i: word; AVal: TRecW2): Cardinal;  begin if i=0 then Result := AVal.a else Result := AVal.b; AVal.a:=0;  end;
function Test2RecC2(i: word; AVal: TRecC2): Cardinal;  begin if i=0 then Result := AVal.a else Result := AVal.b; AVal.a:=0;  end;
function Test2RecQ2(i: word; AVal: TRecQ2): Cardinal;  begin if i=0 then Result := AVal.a else Result := AVal.b; AVal.a:=0;  end;

function TestRecN2N2_1(AVal, BVal: TRecN2): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecN2N2_2(AVal, BVal: TRecN2): Cardinal;  begin  Result := BVal.a; AVal.a:=0;  end;

function TestRecB2B2_1(AVal, BVal: TRecB2): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecB2B2_2(AVal, BVal: TRecB2): Cardinal;  begin  Result := BVal.a; AVal.a:=0;  end;
function TestRecW2W2_1(AVal, BVal: TRecW2): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecW2W2_2(AVal, BVal: TRecW2): Cardinal;  begin  Result := BVal.a; AVal.a:=0;  end;
function TestRecC2C2_1(AVal, BVal: TRecC2): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecC2C2_2(AVal, BVal: TRecC2): Cardinal;  begin  Result := BVal.a; AVal.a:=0;  end;
function TestRecQ2Q2_1(AVal, BVal: TRecQ2): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecQ2Q2_2(AVal, BVal: TRecQ2): Cardinal;  begin  Result := BVal.a; AVal.a:=0;  end;

function Test1RecB2B2(AVal, BVal: TRecB2; i: byte): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test1RecW2W2(AVal, BVal: TRecW2; i: byte): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test1RecC2C2(AVal, BVal: TRecC2; i: byte): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test1RecQ2Q2(AVal, BVal: TRecQ2; i: byte): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;

function Test2RecB2B2(i: word; AVal, BVal: TRecB2): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test2RecW2W2(i: word; AVal, BVal: TRecW2): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test2RecC2C2(i: word; AVal, BVal: TRecC2): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test2RecQ2Q2(i: word; AVal, BVal: TRecQ2): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;


function TestRecB2B3_1(AVal: TRecB2; BVal: TRecB3): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecB2Q2_1(AVal: TRecB2; BVal: TRecQ2): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecB3B2_1(AVal: TRecB3; BVal: TRecB2): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecB3Q2_1(AVal: TRecB3; BVal: TRecQ2): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecQ2B2_1(AVal: TRecQ2; BVal: TRecB2): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;
function TestRecQ2B3_1(AVal: TRecQ2; BVal: TRecB3): Cardinal;  begin  Result := AVal.a; AVal.a:=0;  end;

function TestRecB2B3_2(AVal: TRecB2; BVal: TRecB3): Cardinal;  begin  Result := BVal.a; AVal.a:=0;  end;
function TestRecB2Q2_2(AVal: TRecB2; BVal: TRecQ2): Cardinal;  begin  Result := BVal.a; AVal.a:=0;  end;
function TestRecB3B2_2(AVal: TRecB3; BVal: TRecB2): Cardinal;  begin  Result := BVal.a; AVal.a:=0;  end;
function TestRecB3Q2_2(AVal: TRecB3; BVal: TRecQ2): Cardinal;  begin  Result := BVal.a; AVal.a:=0;  end;
function TestRecQ2B2_2(AVal: TRecQ2; BVal: TRecB2): Cardinal;  begin  Result := BVal.a; AVal.a:=0;  end;
function TestRecQ2B3_2(AVal: TRecQ2; BVal: TRecB3): Cardinal;  begin  Result := BVal.a; AVal.a:=0;  end;

function Test1RecB2B3(AVal: TRecB2; BVal: TRecB3; i: byte): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test1RecB2Q2(AVal: TRecB2; BVal: TRecQ2; i: byte): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test1RecB3B2(AVal: TRecB3; BVal: TRecB2; i: byte): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test1RecB3Q2(AVal: TRecB3; BVal: TRecQ2; i: byte): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test1RecQ2B2(AVal: TRecQ2; BVal: TRecB2; i: byte): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test1RecQ2B3(AVal: TRecQ2; BVal: TRecB3; i: byte): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;

function Test2RecB2B3(i: byte; AVal: TRecB2; BVal: TRecB3): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test2RecB2Q2(i: byte; AVal: TRecB2; BVal: TRecQ2): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test2RecB3B2(i: byte; AVal: TRecB3; BVal: TRecB2): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test2RecB3Q2(i: byte; AVal: TRecB3; BVal: TRecQ2): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test2RecQ2B2(i: byte; AVal: TRecQ2; BVal: TRecB2): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;
function Test2RecQ2B3(i: byte; AVal: TRecQ2; BVal: TRecB3): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0;  end;

function Test2RecB2QQQB3(i: byte; AVal: TRecB2; m,n,o,p,q,r,s,t: Integer; BVal: TRecB3): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0; if (m<>1) or (t<>8) then result := t;  end;
function Test2RecB2QQQQ2(i: byte; AVal: TRecB2; m,n,o,p,q,r,s,t: Integer; BVal: TRecQ2): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0; if (m<>1) or (t<>8) then result := t;  end;
function Test2RecB3QQQB2(i: byte; AVal: TRecB3; m,n,o,p,q,r,s,t: Integer; BVal: TRecB2): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0; if (m<>1) or (t<>8) then result := t;  end;
function Test2RecB3QQQQ2(i: byte; AVal: TRecB3; m,n,o,p,q,r,s,t: Integer; BVal: TRecQ2): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0; if (m<>1) or (t<>8) then result := t;  end;
function Test2RecQ2QQQB2(i: byte; AVal: TRecQ2; m,n,o,p,q,r,s,t: Integer; BVal: TRecB2): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0; if (m<>1) or (t<>8) then result := t;  end;
function Test2RecQ2QQQB3(i: byte; AVal: TRecQ2; m,n,o,p,q,r,s,t: Integer; BVal: TRecB3): Cardinal;  begin  if i=0 then Result := AVal.a else Result := BVal.a; AVal.a:=0; if (m<>1) or (t<>8) then result := t;  end;


function Test2RecQ3Q3(i: byte; AVal, BVal: TRecQ3): Cardinal;
begin
  case i of
    0: Result := AVal.a;    1: Result := AVal.c;
    2: Result := BVal.a;    3: Result := BVal.c;
  end;
  AVal.a:=0;  BVal.a:=0;
end;
function Test2RecQ4Q4(i: byte; AVal, BVal: TRecQ4): Cardinal;
begin
  case i of
    0: Result := AVal.a;    1: Result := AVal.d;
    2: Result := BVal.a;    3: Result := BVal.d;
  end;
  AVal.a:=0;  BVal.a:=0;
end;
function Test2RecQ5Q5(i: byte; AVal, BVal: TRecQ5): Cardinal;
begin
  case i of
    0: Result := AVal.a;    1: Result := AVal.e;
    2: Result := BVal.a;    3: Result := BVal.e;
  end;
  AVal.a:=0;  BVal.a:=0;
end;
function Test2RecQ6Q6(i: byte; AVal, BVal: TRecQ6): Cardinal;
begin
  case i of
    0: Result := AVal.a;    1: Result := AVal.f;
    2: Result := BVal.a;    3: Result := BVal.f;
  end;
  AVal.a:=0;  BVal.a:=0;
end;
function Test2RecQ7Q7(i: byte; AVal, BVal: TRecQ7): Cardinal;
begin
  case i of
    0: Result := AVal.a;    1: Result := AVal.g;
    2: Result := BVal.a;    3: Result := BVal.g;
  end;
  AVal.a:=0;  BVal.a:=0;
end;


function UsedMem: ptruint;
var
  mm: TMemoryManager;
  hs: TFPCHeapStatus;
begin
  // ensure global vars are in mem
  TestRecB2_a(aRecB2);  TestRecB2_a(bRecB2);
  TestRecB3_a(aRecB3);  TestRecB3_a(bRecB3);
  TestRecW2_a(aRecW2);  TestRecW2_a(bRecW2);
  TestRecW3_c(aRecW3);  TestRecW3_c(bRecW3);
  TestRecC2_a(aRecC2);  TestRecC2_a(bRecC2);
  TestRecC3_c(aRecC3);  TestRecC3_c(bRecC3);
  TestRecQ2_a(aRecQ2);  TestRecQ2_a(bRecQ2);
  TestRecQ3_c(aRecQ3);  TestRecQ3_c(bRecQ3);

  GetMemoryManager(mm);
  hs := mm.GetFPCHeapStatus();
  Result := hs.CurrHeapUsed;
end;





begin
  aRecN2.a := 11;  aRecN2.b := 12;
  bRecN2.a :=  8;  bRecN2.b :=  9;

  aRecB2.a := 11;  aRecB2.b := 21;
  aRecW2.a := 12;  aRecW2.b := 22;
  aRecC2.a := 13;  aRecC2.b := 23;
  aRecQ2.a := 14;  aRecQ2.b := 24;

  bRecB2.a := 51;  bRecB2.b := 61;
  bRecW2.a := 52;  bRecW2.b := 62;
  bRecC2.a := 53;  bRecC2.b := 63;
  bRecQ2.a := 54;  bRecQ2.b := 64;

  aRecB3.a := 15;  aRecB3.b := 25;  aRecB3.c := 35;
  aRecW3.a := 16;  aRecW3.b := 26;  aRecW3.c := 36;
  aRecC3.a := 17;  aRecC3.b := 27;  aRecC3.c := 37;
  aRecQ3.a := 18;  aRecQ3.b := 28;  aRecQ3.c := 38;

  bRecB3.a := 55;  bRecB3.b := 65;  bRecB3.c := 75;
  bRecW3.a := 56;  bRecW3.b := 66;  bRecW3.c := 76;
  bRecC3.a := 57;  bRecC3.b := 67;  bRecC3.c := 77;
  bRecQ3.a := 58;  bRecQ3.b := 68;  bRecQ3.c := 78;

  aRecQ4.a := 58;  aRecQ4.b := 68;  aRecQ4.c := 78;  aRecQ4.d := 2;
  aRecQ5.a := 58;  aRecQ5.b := 68;  aRecQ5.c := 78;  aRecQ5.d := 2;  aRecQ5.e := 3;
  aRecQ6.a := 58;  aRecQ6.b := 68;  aRecQ6.c := 78;  aRecQ6.d := 2;  aRecQ6.e := 3;  aRecQ6.f := 4;
  aRecQ7.a := 58;  aRecQ7.b := 68;  aRecQ7.c := 78;  aRecQ7.d := 2;  aRecQ7.e := 3;  aRecQ7.f := 4;  aRecQ7.g := 5;

  bRecQ4.a := 59;  bRecQ4.b := 69;  bRecQ4.c := 79;  bRecQ4.d := 92;
  bRecQ5.a := 59;  bRecQ5.b := 69;  bRecQ5.c := 79;  bRecQ5.d := 92;  bRecQ5.e := 93;
  bRecQ6.a := 59;  bRecQ6.b := 69;  bRecQ6.c := 79;  bRecQ6.d := 92;  bRecQ6.e := 93;  bRecQ6.f := 94;
  bRecQ7.a := 59;  bRecQ7.b := 69;  bRecQ7.c := 79;  bRecQ7.d := 92;  bRecQ7.e := 93;  bRecQ7.f := 94;  bRecQ7.g := 95;


  // After each test the debugger can check the memusage
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem; // TEST_BREAKPOINT=main
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;
  CurMemUsed := UsedMem;


TestRecN2_a(aRecN2);
TestRecN2_b(bRecN2);

TestRecB2_a(aRecB2);
TestRecB2_b(bRecB2);
TestRecB3_a(aRecB3);
TestRecB3_b(aRecB3);
TestRecB3_c(bRecB3);

TestRecW2_a(aRecW2);
TestRecW2_b(bRecW2);
TestRecW3_a(aRecW3);
TestRecW3_b(aRecW3);
TestRecW3_c(bRecW3);

TestRecC2_a(aRecC2);
TestRecC2_b(bRecC2);
TestRecC3_a(aRecC3);
TestRecC3_b(aRecC3);
TestRecC3_c(bRecC3);

TestRecQ2_a(aRecQ2);
TestRecQ2_b(bRecQ2);
TestRecQ3_a(aRecQ3);
TestRecQ3_b(aRecQ3);
TestRecQ3_c(bRecQ3);


Test1RecB2(aRecB2,1);
Test1RecW2(bRecW2,1);
Test1RecC2(aRecC2,1);
Test1RecQ2(aRecQ2,1);

Test2RecB2(1, aRecB2);
Test2RecW2(1, bRecW2);
Test2RecC2(1, aRecC2);
Test2RecQ2(1, aRecQ2);

TestRecN2N2_1(aRecN2, bRecN2);  TestRecN2N2_2(aRecN2, bRecN2);

TestRecB2B2_1(aRecB2, bRecB2);  TestRecB2B2_2(aRecB2, bRecB2);
TestRecW2W2_1(aRecW2, bRecW2);  TestRecW2W2_2(aRecW2, bRecW2);
TestRecC2C2_1(aRecC2, bRecC2);  TestRecC2C2_2(aRecC2, bRecC2);
TestRecQ2Q2_1(aRecQ2, bRecQ2);  TestRecQ2Q2_2(aRecQ2, bRecQ2);

Test1RecB2B2(aRecB2, bRecB2,1);
Test1RecW2W2(aRecW2, bRecW2,1);
Test1RecC2C2(aRecC2, bRecC2,1);
Test1RecQ2Q2(aRecQ2, bRecQ2,1);

Test2RecB2B2(1,aRecB2, bRecB2);
Test2RecW2W2(1,aRecW2, bRecW2);
Test2RecC2C2(1,aRecC2, bRecC2);
Test2RecQ2Q2(1,aRecQ2, bRecQ2);


TestRecB2B3_1(aRecB2, bRecB3);  TestRecB2B3_2(aRecB2, bRecB3);
TestRecB2Q2_1(aRecB2, bRecQ2);  TestRecB2Q2_2(aRecB2, bRecQ2);
TestRecB3B2_1(aRecB3, bRecB2);  TestRecB3B2_2(aRecB3, bRecB2);
TestRecB3Q2_1(aRecB3, bRecQ2);  TestRecB3Q2_2(aRecB3, bRecQ2);
TestRecQ2B2_1(aRecQ2, bRecB2);  TestRecQ2B2_2(aRecQ2, bRecB2);
TestRecQ2B3_1(aRecQ2, bRecB3);  TestRecQ2B3_2(aRecQ2, bRecB3);

Test1RecB2B3(aRecB2, bRecB3, 1);
Test1RecB2Q2(aRecB2, bRecQ2, 1);
Test1RecB3B2(aRecB3, bRecB2, 1);
Test1RecB3Q2(aRecB3, bRecQ2, 1);
Test1RecQ2B2(aRecQ2, bRecB2, 1);
Test1RecQ2B3(aRecQ2, bRecB3, 1);

Test2RecB2B3(1, aRecB2, bRecB3);
Test2RecB2Q2(1, aRecB2, bRecQ2);
Test2RecB3B2(1, aRecB3, bRecB2);
Test2RecB3Q2(1, aRecB3, bRecQ2);
Test2RecQ2B2(1, aRecQ2, bRecB2);
Test2RecQ2B3(1, aRecQ2, bRecB3);

Test2RecQ3Q3(1, aRecQ3, bRecQ3);
Test2RecQ4Q4(1, aRecQ4, bRecQ4);
Test2RecQ5Q5(1, aRecQ5, bRecQ5);
Test2RecQ6Q6(1, aRecQ6, bRecQ6);
Test2RecQ7Q7(1, aRecQ7, bRecQ7);

Test2RecB2QQQB3(1, aRecB2, 1,2,3,4,5,6,7,8, bRecB3);
Test2RecB2QQQQ2(1, aRecB2, 1,2,3,4,5,6,7,8, bRecQ2);
Test2RecB3QQQB2(1, aRecB3, 1,2,3,4,5,6,7,8, bRecB2);
Test2RecB3QQQQ2(1, aRecB3, 1,2,3,4,5,6,7,8, bRecQ2);
Test2RecQ2QQQB2(1, aRecQ2, 1,2,3,4,5,6,7,8, bRecB2);
Test2RecQ2QQQB3(1, aRecQ2, 1,2,3,4,5,6,7,8, bRecB3);


end.
