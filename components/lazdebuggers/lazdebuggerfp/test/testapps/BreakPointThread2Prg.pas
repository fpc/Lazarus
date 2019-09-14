program BreakPointThread2Prg;
{$asmMode intel}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  sysutils, Classes;

type

  { TTestThread }

  TTestThread = class(TThread)
    procedure Execute; override;
  end;

  { TTestThread2 }

  TTestThread2 = class(TThread)
    procedure Execute; override;
  end;


var
  T1, x, BreakDummy: Integer;

label
testasmlbl1;

{ TTestThread }

procedure TTestThread.Execute;
var
  i: Integer;
  t: array [0..5] of TTestThread2;
begin
  for i := 0 to high(t) do
    t[i] := TTestThread2.Create(False);
  InterLockedIncrement(T1);
  while not Terminated do
    for i := 0 to high(t) do begin
      t[i].WaitFor;
      t[i].Free;
      t[i] := TTestThread2.Create(False);
    end;
end;

{ TTestThread2 }

procedure TTestThread2.Execute;
begin
  //
end;

begin
  TTestThread.Create(False);
  TTestThread.Create(False);
  TTestThread.Create(False);
  TTestThread.Create(False);
  TTestThread.Create(False);
  while InterLockedExchangeAdd(T1, 0) < 4 do
    sleep(10);

  BreakDummy := 1;

  asm
    nop  // TEST_BREAKPOINT=BrkMainBegin
    xor eax, eax
    xor ebx, ebx
    add eax, 20
testasmlbl1:
    sub eax, 20
    add eax, 1 // TEST_BREAKPOINT=BrkMain0
    add eax, 1
    add eax, 1 // TEST_BREAKPOINT=BrkMain1
    add eax, 1
    add eax, 1 // TEST_BREAKPOINT=BrkMain2
    add eax, 1
    add eax, 1 // TEST_BREAKPOINT=BrkMain3
    add eax, 1
    add eax, 1 // TEST_BREAKPOINT=BrkMain4
    add eax, 1
    add eax, 1 // TEST_BREAKPOINT=BrkMain5
    add eax, 1
    add eax, 1 // TEST_BREAKPOINT=BrkMain6
    add eax, 1
    add eax, 1 // TEST_BREAKPOINT=BrkMain7
    add eax, 1
    add eax, 1 // TEST_BREAKPOINT=BrkMain8
    add eax, 1
    add eax, 1 // TEST_BREAKPOINT=BrkMain9
    add eax, 1

    add ebx, 1
    jmp testasmlbl1

    nop
    nop  // TEST_BREAKPOINT=BrkMainEnd
  end;

end.

