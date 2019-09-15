program StepOverPrg;
{$asmMode intel}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  sysutils, Classes;

type

  { TTestThread1 }

  TTestThread1 = class(TThread)
    procedure Execute; override;
  end;


var
  x, BreakDummy: Integer;
  T1, T1Interfere: Integer;
  T1Stop: Boolean;


label
testasmlbl1, testasmlbl2;

Procedure MyNorm;
begin
  x := 1;
end;

Procedure MyNested(ALvl: Integer = 0);
begin
  if ALvl > 3 then exit;
  if ALvl = 0 then
    x := 1;  // TEST_BREAKPOINT=BrkNested
  x := 3; MyNested(ALvl + 1); x := 4; if ALvl = 0 then // only reach "AfterNested" in most outer recurse
    x := 2; // TEST_BREAKPOINT=AfterNested
  x := 1;
end;

Procedure MySleep;
begin
  Sleep(50);
end;

Procedure MyBrkDis;
begin
  x := 1;  // TEST_BREAKPOINT=BrkDisabled
end;

Procedure MyBrkHitCnt;
begin
  x := 1;  // TEST_BREAKPOINT=BrkHitCnt
end;

(* Try to step over a line, while another thread is also going through the line *)
Procedure MyInterfereByThread(a: Boolean = false);
  Procedure MyInterfereSleep;
  begin
    if a then Sleep(10); // threads do not stop / while the test waits here, other threads should hit the calling line
  end;
begin
  if a then
    x := 1; // TEST_BREAKPOINT=BrkInterfereByThread
  if a then InterLockedExchange(T1Interfere, 0); while (InterLockedExchangeAdd(T1Interfere,0)=0) do begin MyInterfereSleep; if not a then break; end;
  x := 1; // TEST_BREAKPOINT=AfterInterfereByThread

  InterLockedIncrement(T1Interfere); // Other threads will increment
end;

{ TTestThread1 }

procedure TTestThread1.Execute;
begin
  InterLockedIncrement(T1);
  while not (Terminated or T1Stop) do MyInterfereByThread;
  InterLockedDecrement(T1);
end;

begin

  x := 1;
  x := x + 1;     // TEST_BREAKPOINT=BrkStart
  x := x + 1; x := x + 2; x := x + 3;    // TEST_BREAKPOINT=AfterStep
  MyNorm();       // TEST_BREAKPOINT=AfterStepLongLine
  x := 1; MyNorm(); x := x + 1; MyNorm(); x := x + 1; MyNorm(); x := x + 1;      // TEST_BREAKPOINT=AfterStepProc
  MySleep();      // TEST_BREAKPOINT=AfterStepProcLong
  sleep(50);      // TEST_BREAKPOINT=AfterStepSleepProc
  MyBrkDis;       // TEST_BREAKPOINT=AfterStepSleep
  MyBrkHitCnt;    // TEST_BREAKPOINT=AfterStepBrkDis
  MyBrkDis;       // TEST_BREAKPOINT=AfterStepBrkHitCnt
  x := 1;         // TEST_BREAKPOINT=AfterStepBrkDisAgain
  x := 1;
  MyNested; // TEST_BREAKPOINT=CallNested


  T1 := 0;
  T1Stop := False;
  TTestThread1.Create(False); while not (InterLockedExchangeAdd(T1,0)=1) do x:=1;  // TEST_BREAKPOINT=BrkThreadCreateInStep
  x := 1;         // TEST_BREAKPOINT=AfterThreadCreateInStep

  (* Prepare for threads to interfare with the hidden breakpoint *)
  // create a few threads to interfer
  TTestThread1.Create(False); TTestThread1.Create(False); TTestThread1.Create(False);
  TTestThread1.Create(False); TTestThread1.Create(False); TTestThread1.Create(False);
  while not (InterLockedExchangeAdd(T1,0)>=5) do sleep(10);  // at least 5 running

  MyInterfereByThread(True);
  T1Stop := True;

  while not (InterLockedExchangeAdd(T1,0)<=1) do x:=1;  // TEST_BREAKPOINT=BrkThreadExitInStep
  x := 1;         // TEST_BREAKPOINT=AfterThreadExitInStep

  //sleep(500);
  BreakDummy := 1;


end.

