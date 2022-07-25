program WatchesFuncStringPrg;
{$mode objfpc}{$H+}

uses SysUtils;

var
  CurMemUsed: ptruint;
  SomeInt: Integer;
  s1, s2, s3, s4, x: String;
  ws1, ws2, ws3, ws4, wx: WideString;


function UsedMem: ptruint;
var
  mm: TMemoryManager;
  hs: TFPCHeapStatus;
  i: integer;
begin
  // ensure global vars are in mem
  i := SomeInt + Length(s1) + Length(s2) + Length(s3) + Length(s4);
  x := IntToStr(i);
  GetMemoryManager(mm);
  hs := mm.GetFPCHeapStatus();
  Result := hs.CurrHeapUsed;
end;

var
  Cnt: Integer;

function TestStrRes: String;
begin
  Result := '#'+IntToStr(Cnt);
  inc(Cnt);
end;

function TestIntToStrRes(AVal: Integer): String;
begin
  Result := '$'+IntToHex(AVal, 8);
end;

function TestIntSumToStrRes(AVal, AVal2: Integer): String;
begin
  Result := '$'+IntToHex(AVal+AVal2, 8);
end;

function TestStrToIntRes(AVal: String): Integer;
begin
  if Length(AVal) = 1 then
    AVal := AVal + 'abc'
  else
  if AVal <> '' then
    AVal[1] := 'X';

  Result := Length(AVal);
end;

function TestStrToStrRes(AVal: String): String;
begin
  if Length(AVal) = 1 then
    AVal := AVal + 'abc'
  else
  if AVal <> '' then
    AVal[1] := 'X';

  Result := '"' + IntToStr(Length(AVal)) + '"';
end;

function conc(AVal, BVal: String): String;
begin
  Result := AVal + BVal;
end;

// Widestring

function WTestStrRes: WideString;
begin
  Result := '#'+IntToStr(Cnt);
  inc(Cnt);
end;

function WTestIntToStrRes(AVal: Integer): WideString;
begin
  Result := '$'+IntToHex(AVal, 8);
end;

function WTestIntSumToStrRes(AVal, AVal2: Integer): WideString;
begin
  Result := '$'+IntToHex(AVal+AVal2, 8);
end;

function WTestStrToIntRes(AVal: WideString): Integer;
begin
  if Length(AVal) = 1 then
    AVal := AVal + 'abc'
  else
  if AVal <> '' then
    AVal[1] := 'X';

  Result := Length(AVal);
end;

function WTestStrToStrRes(AVal: WideString): WideString;
begin
  if Length(AVal) = 1 then
    AVal := AVal + 'abc'
  else
  if AVal <> '' then
    AVal[1] := 'X';

  Result := '"' + IntToStr(Length(AVal)) + '"';
end;

function Wconc(AVal, BVal: WideString): WideString;
begin
  Result := AVal + BVal;
end;


begin
  // Call all functions, so they are used in the exe
  TestStrRes;
  TestIntToStrRes(1);
  TestIntSumToStrRes(1,2);
  TestStrToIntRes('a');
  TestStrToStrRes('a');
  conc('a', 'b');

  wTestStrRes;
  wTestIntToStrRes(1);
  wTestIntSumToStrRes(1,2);
  wTestStrToIntRes('a');
  wTestStrToStrRes('a');
  wconc('a', 'b');

  Cnt := 0;
  SomeInt := 126;
  s1 := '';
  s2 := 'A';
  s3 := 'abc';
  s4 := 'def';

  ws1 := '';
  ws2 := 'A';
  ws3 := 'abc';
  ws4 := 'def';

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

  // access variables again
  TestIntToStrRes(SomeInt);
  TestStrToIntRes(s1);
  TestStrToIntRes(s2);
  TestStrToIntRes(s3);
  TestStrToIntRes(s4);

  TestStrToIntRes(ws1);
  TestStrToIntRes(ws2);
  TestStrToIntRes(ws3);
  TestStrToIntRes(ws4);


end.
