unit TestBreakPoint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, TestDbgControl, TestDbgTestSuites,
  TTestWatchUtilities, TestCommonSources, TestDbgConfig, TTestDebuggerClasses,
  LazDebuggerIntf, LazDebuggerIntfBaseTypes, DbgIntfDebuggerBase,
  DbgIntfBaseTypes, LazLoggerBase, Forms;

type

  // Info used by tests based on TestBreakPointThreadPrg
  TBreakThreadPrgInfoEntry = record
    ID: Integer;
    Address: TDBGPtr;
    Line, PreviousLine: Integer;
    Val, LastVal: int64;           // AX register
    Loop, PreviousLoop: int64;         // BX register
    IsCurrent: Boolean;
    LastBrkLine, LastBrkLoop: Integer;
    PreviousLastBrkLine, PreviousLastBrkLoop: Integer;
  end;
  TBreakThreadPrgInfo = record
    ThrLoopFirst, ThrLoopLast, ThrLoopLine0, ThrLoopInc: Integer; // Linenumbers of the breakpoints
    // -1 => Main thread
    Threads: array[-1..10] of TBreakThreadPrgInfoEntry;
  end;

  { TTestBreakPoint }

  TTestBreakPoint = class(TDBGTestCase)
  protected
    // Info/Methods used by tests based on TestBreakPointThreadPrg
    FThrPrgInfo: TBreakThreadPrgInfo;
    procedure ThrPrgInitializeThreads(ATestName: String);
    procedure ThrPrgUpdateThreads(ATestName: String);

    (* ThrPrgCheckNoSkip
       Check that AX is correct for the line.
       Ensure the instruction restored from "int3" was executed
    *)
    procedure ThrPrgCheckNoSkip(ATestName: String='');
    (* ThrPrgInfoHasGoneThroughLine
       Has gone over the line WITHOUT stopping at the breakpoint
     - If the thread was NOT on the line, it has gone over it at least once.
     - if the thread was AT the line,
       ~ and had NOT reorted yet for the line => it as gone away from the line anyway
       ~ and had reported for the line => it has gone OVER the line in the NEXT loop already
    *)
    function  ThrPrgInfoHasGoneThroughLine(AIndex, ALine: Integer): boolean;
  protected
    Src: TCommonSource;
    Dbg: TDebuggerIntf;
    procedure TestLocation(ATestName, ABrkName: String; ABreakHitCount: Integer = 1);
    procedure TestHitCnt(ATestName, ABrkName: String; ABreakHitCount: Integer);
  published
    (* Ensure the debugger can correctly run/step after hidding a breakpoit
       - the original instruction is executed
       - the breakpoint can be hit again
       - stepping continues correctly to next line
       - breakpoints are "hit" (recognized) when stepping onto them (not actually triggering them)
    *)
    procedure TestBreakPoints;

    (* Ensure, that while one thread steps over a breakpoint (int3 removed),
       no other thread ignores the breakpoint
    *)
    procedure TestBreakThreadsNoSkip;

    (* Remove Breakpoints, while other threads have a pending event for the Brk.
       Make sure the other thread, still executeds the original instruction,
       hidden by the int3
       *** The test can actually NOT force multiple breakpoints to be hit.
           It only creates a high likelihood for this to happen.
    *)
    procedure TestBreakThreadsMoveBreak1;
    procedure TestBreakThreadsMoveBreak2;

    // check that each brk point hit was reported (one per thread)
    procedure TestBreakThreadsHitBreak;

    (* Only ONE thread running the breakpoints. Plenty of events from other
       threads (including thread-exit).
       Hit each breakpoint in order, WITHOUT ever re-hitting the last brk.
       *HOPE* is that some events will happen while the main thread single steps
         over a temp-removed break, and the single step has NOT yet moved.
         So the single step will still need the brk to be tmp-removed when it
         continues.
    *)
    procedure TestBreakThreadsIgnoreOther;
  end;

implementation

var
  ControlTest, ControlTestBreak, ControlTestThreadNoSkip,
  ControlTestThreadMove1, ControlTestThreadMove2, ControlTestThreadHit,
  ControlTestThreadIgnoreOther: Pointer;

procedure TTestBreakPoint.TestLocation(ATestName, ABrkName: String;
  ABreakHitCount: Integer);
var
  lc: TDBGLocationRec;
begin
  AssertDebuggerState(dsPause);
  lc := Debugger.LazDebugger.GetLocation;
  TestEquals(ATestName+' '+ABrkName+' Loc', Src.BreakPoints[ABrkName], lc.SrcLine);
  if ABreakHitCount >= 0 then
    TestEquals(ATestName+' '+ABrkName+' HitCnt', Debugger.BreakPointByName(ABrkName).HitCount, ABreakHitCount);
end;

procedure TTestBreakPoint.TestHitCnt(ATestName, ABrkName: String;
  ABreakHitCount: Integer);
begin
  TestEquals(ATestName+' '+ABrkName+' HitCnt', Debugger.BreakPointByName(ABrkName).HitCount, ABreakHitCount);
end;

procedure TTestBreakPoint.TestBreakPoints;
var
  ExeName: String;
  loc: TDBGLocationRec;
  b1, b2: TDBGBreakPoint;
  MainThreadId: Integer;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestBreak) then exit;
  Src := GetCommonSourceFor(AppDir + 'BreakPointPrg.pas');
  TestCompile(Src, ExeName);

  AssertTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  dbg := Debugger.LazDebugger;

  try
    Debugger.SetBreakPoint(Src, 'PrgStep1');
    Debugger.SetBreakPoint(Src, 'PrgStep2');
    Debugger.SetBreakPoint(Src, 'PrgRun1');
    Debugger.SetBreakPoint(Src, 'PrgRun2');

    Debugger.SetBreakPoint(Src, 'AsmStep1');
    Debugger.SetBreakPoint(Src, 'AsmStep2');
    Debugger.SetBreakPoint(Src, 'AsmRun1');
    Debugger.SetBreakPoint(Src, 'AsmRun2');

    Debugger.SetBreakPoint(Src, 'AsmBranch1');
    Debugger.SetBreakPoint(Src, 'AsmNever1');
    Debugger.SetBreakPoint(Src, 'AsmBranchEnd1');
    Debugger.SetBreakPoint(Src, 'AsmBranch2');
    Debugger.SetBreakPoint(Src, 'AsmNever2');
    Debugger.SetBreakPoint(Src, 'AsmBranchEnd2');

    Debugger.SetBreakPoint(Src, 'Foo1');
    Debugger.SetBreakPoint(Src, 'Foo2');

    Debugger.SetBreakPoint(Src, 'PrgAfterFoo2');

    Debugger.SetBreakPoint(Src, 'Thread1');
    AssertDebuggerNotInErrorState;

    // Step to break next line
    Debugger.RunToNextPause(dcRun);
    TestLocation('Init', 'PrgStep1');

    Debugger.RunToNextPause(dcStepOver);
    TestLocation('Stepped to break at next line', 'PrgStep2');
    TestHitCnt('Stepped to break at next line', 'PrgStep1', 1);

    // Run to break next line
    Debugger.RunToNextPause(dcRun);
    TestLocation('Run to break', 'PrgRun1');

    Debugger.RunToNextPause(dcRun);
    TestLocation('Run to break at next line', 'PrgRun2');
    TestHitCnt('Run to break at next line', 'PrgRun1', 1);


    // Run/Step with one byte asm steps
    Debugger.RunToNextPause(dcRun);
    TestLocation('Init NOP', 'AsmStep1');

    Debugger.RunToNextPause(dcStepOver);
    TestLocation('Stepped to NOP at next line', 'AsmStep2');
    TestHitCnt('Stepped to NOP at next line', 'AsmStep1', 1);

    Debugger.RunToNextPause(dcRun);
    TestLocation('Run to NOP', 'AsmRun1');

    Debugger.RunToNextPause(dcRun);
    TestLocation('Run to NOP at next line', 'AsmRun2');
    TestHitCnt('Run to NOP at next line', 'AsmRun1', 1);

    // check that the command hidden by the int3 breakpoint is executed
    Debugger.RunToNextPause(dcRun);
    TestLocation('Run to NOP at next line', 'AsmBranch1');
    Debugger.RunToNextPause(dcStepOver);
    TestLocation('Run to NOP at next line', 'AsmBranchEnd1');
    TestHitCnt('Run to NOP at next line', 'AsmBranch1', 1);
    TestHitCnt('Run to NOP at next line', 'AsmNever1', 0);

    Debugger.RunToNextPause(dcRun);
    TestLocation('Run to NOP at next line', 'AsmBranch2');
    Debugger.RunToNextPause(dcRun);
    TestLocation('Run to NOP at next line', 'AsmBranchEnd2');
    TestHitCnt('Run to NOP at next line', 'AsmBranch2', 1);
    TestHitCnt('Run to NOP at next line', 'AsmNever2', 0);


    // Run to same line (repeat proc call)
    Debugger.RunToNextPause(dcRun);
    TestLocation('Init Foo1', 'Foo1');

    Debugger.RunToNextPause(dcRun);
    TestLocation('Run Foo1', 'Foo1', 2);

    Debugger.RunToNextPause(dcStepOver);
    Debugger.RunToNextPause(dcRun);
    TestLocation('Step/Run Foo1', 'Foo1', 3);

    // Run to same line (loop)
    Debugger.RunToNextPause(dcRun);
    TestLocation('Run Foo2', 'Foo2');

    Debugger.RunToNextPause(dcRun);
    TestLocation('Run Foo2', 'Foo2', 2);

    Debugger.RunToNextPause(dcRun);
    TestLocation('Run Foo2 ended in prg', 'PrgAfterFoo2');
    TestHitCnt('Run Foo2', 'Foo2', 2);

    // new breakpoint
    Debugger.RunToNextPause(dcStepOver);
    TestLocation('Before insernt', 'New1', -1);

    Debugger.SetBreakPoint(Src, 'New1');
    Debugger.RunToNextPause(dcStepOver);
    TestLocation('After insernt', 'New2', -1);
    TestHitCnt('After insernt', 'New1', 0);

    Debugger.SetBreakPoint(Src, 'New2');
    Debugger.RunToNextPause(dcRun);
    TestHitCnt('After insernt', 'New1', 0);
    TestHitCnt('After insernt', 'New2', 0);

    // in threads
    (* In each thread set a breakpoint at the next instruction.
       So when all threads are started, both should run/step OVER the breakpoint
       without hitting it
    *)
    TestLocation('After insernt', 'Thread1');


    MainThreadId := dbg.Threads.CurrentThreads.CurrentThreadId;

    Debugger.RunToNextPause(dcStepOver);
    TestLocation('After insernt', 'Thread2', -1); // not set

    loc := dbg.GetLocation;
    TestEquals('loc in thread', loc.SrcLine, Src.BreakPoints['Thread2']);
    b1 := dbg.BreakPoints.Add(loc.Address);
    b1.InitialEnabled := True;
    b1.Enabled := True;

    dbg.Threads.ChangeCurrentThread(MainThreadId);
    Debugger.RunToNextPause(dcStepOver); // continue stepping in main thread
//TODO: not yet implemented

    loc := dbg.GetLocation;
//    TestTrue('loc thread main', loc.SrcLine > Src.BreakPoints['New2']);
    b2 := dbg.BreakPoints.Add(loc.Address);
    b2.InitialEnabled := True;
    b2.Enabled := True;


    If loc.SrcLine >= Src.BreakPoints['Main2']-2 then
      Debugger.SetBreakPoint(Src, 'Main1')
    else
      Debugger.SetBreakPoint(Src, 'Main2');

    Debugger.RunToNextPause(dcRun);

(* // TODO: breakpoints in diff threads may need to be hit, after the other break continued
    If loc.SrcLine >= Src.BreakPoints['Main2']-2 then begin
      TestLocation('main1', 'Main1');
      Debugger.BreakPointByName('Main1').Enabled := False;
    end else begin
      TestLocation('main2', 'Main2');
      Debugger.BreakPointByName('Main2').Enabled := False;
    end;

    TestEquals('b1 hits', 0, b1.HitCount);
    TestEquals('b2 hits', 0, b2.HitCount);

    Debugger.RunToNextPause(dcRun);
    TestEquals('b1 hits after', 0, b1.HitCount);
    TestEquals('b2 hits after', 0, b2.HitCount);
*)

    dbg.Stop;
  finally
    Debugger.ClearDebuggerMonitors;
    Debugger.FreeDebugger;

    AssertTestErrors;
  end;
end;

procedure TTestBreakPoint.ThrPrgInitializeThreads(ATestName: String);
var
  i, j: Integer;
  t: TThreadEntry;
begin
  FThrPrgInfo.ThrLoopFirst := Src.BreakPoints['BrkThreadBegin'];
  FThrPrgInfo.ThrLoopLast  := Src.BreakPoints['BrkThreadEnd'];
  FThrPrgInfo.ThrLoopLine0 := Src.BreakPoints['BrkThread1'];
  FThrPrgInfo.ThrLoopInc := Src.BreakPoints['BrkThreadIncLoop'];
  FThrPrgInfo.Threads[-1].ID := 0;
    (* Initialize
       Find all threads
    *)
  dbg.Threads.CurrentThreads.Count;
  j := 0;
  while TTestThreads(dbg.Threads.CurrentThreads).DataValidity <> ddsValid do begin
    sleep(50);
    Application.ProcessMessages;
    inc(j);
    if j > 100 then break;
  end;

  j := 0;
  for i := 0 to dbg.Threads.CurrentThreads.Count-1 do begin
    t := dbg.Threads.CurrentThreads.Entries[i];
    if t.TopFrame.Line < FThrPrgInfo.ThrLoopFirst then begin
      debugln(['Ignoring Thread ', t.ThreadId, ' at ',t.TopFrame.Address]);
      Continue;
    end;
    if t.TopFrame.Line > FThrPrgInfo.ThrLoopLast then begin
      debugln(['MAIN Thread ', t.ThreadId, ' at ',t.TopFrame.Address, ' line ', t.TopFrame.Line]);
      TestTrue('Only one main thread', FThrPrgInfo.Threads[-1].ID = 0);
      if FThrPrgInfo.Threads[-1].ID = 0 then
        FThrPrgInfo.Threads[-1].ID := t.ThreadId;
      Continue;
    end;

    FThrPrgInfo.Threads[j].ID := t.ThreadId;
    FThrPrgInfo.Threads[j].Line := -1;
    FThrPrgInfo.Threads[j].Loop := -5;
    FThrPrgInfo.Threads[j].LastBrkLine := -1;
    FThrPrgInfo.Threads[j].PreviousLastBrkLine := -1;
    debugln(['++ ADDED tid ',t.ThreadId]);
    inc(j);
    if j >= 11 then
      break;
  end;
  AssertEquals('Found 10 threads', 10, j);
      TestTrue('Found main thread', FThrPrgInfo.Threads[-1].ID <> 0);
end;

procedure TTestBreakPoint.ThrPrgUpdateThreads(ATestName: String);
var
  i, j: Integer;
  t: TThreadEntry;
  r: TRegisters;
  ax, bx: TRegisterValue;
begin
  dbg.Threads.CurrentThreads.Count;
  j := 0;
  while TTestThreads(dbg.Threads.CurrentThreads).DataValidity <> ddsValid do begin
    sleep(50);
    Application.ProcessMessages;
    inc(j);
    if j > 100 then break;
  end;

  for i := -1 to Min(9, dbg.Threads.CurrentThreads.Count-1) do begin
    t := dbg.Threads.CurrentThreads.EntryById[FThrPrgInfo.Threads[i].ID];
    TestTrue(ATestName+' thread for '+inttostr(FThrPrgInfo.Threads[i].ID), t<> nil);
    if t=nil then
      continue;
    r := dbg.Registers.CurrentRegistersList.Entries[FThrPrgInfo.Threads[i].ID, 0];
    ax := nil;
    for j := 0 to r.Count-1 do
      if (lowercase(r.Entries[j].Name) = 'eax') or (lowercase(r.Entries[j].Name) = 'rax')
      then
        ax := r.Entries[j];
    bx := nil;
    for j := 0 to r.Count-1 do
      if (lowercase(r.Entries[j].Name) = 'ebx') or (lowercase(r.Entries[j].Name) = 'rbx')
      then
        bx := r.Entries[j];

    FThrPrgInfo.Threads[i].IsCurrent := FThrPrgInfo.Threads[i].ID = dbg.Threads.CurrentThreads.CurrentThreadId;

    FThrPrgInfo.Threads[i].PreviousLine := FThrPrgInfo.Threads[i].Line;
    FThrPrgInfo.Threads[i].LastVal  := FThrPrgInfo.Threads[i].Val;
    FThrPrgInfo.Threads[i].PreviousLoop  := FThrPrgInfo.Threads[i].Loop;
    FThrPrgInfo.Threads[i].Address := t.TopFrame.Address;
    FThrPrgInfo.Threads[i].Line    := t.TopFrame.Line;
    if ax <> nil then
      FThrPrgInfo.Threads[i].Val   := StrToInt64Def(ax.Value,-1) and $7FFFFFFF;
    if bx <> nil then
      FThrPrgInfo.Threads[i].Loop   := StrToInt64Def(bx.Value,-1) and $7FFFFFFF;

    if FThrPrgInfo.Threads[i].ID = dbg.Threads.CurrentThreads.CurrentThreadId then begin
      FThrPrgInfo.Threads[i].PreviousLastBrkLine := FThrPrgInfo.Threads[i].LastBrkLine;
      FThrPrgInfo.Threads[i].PreviousLastBrkLoop := FThrPrgInfo.Threads[i].LastBrkLoop;

      FThrPrgInfo.Threads[i].LastBrkLine := FThrPrgInfo.Threads[i].Line;
      FThrPrgInfo.Threads[i].LastBrkLoop := FThrPrgInfo.Threads[i].Loop;
    end;

    debugln('Thread %d: ID=%d Cur=%s (%x):   Line: %d (%d) (was %d)   Val: %d (was %d)  LOOP: %d (was %d)  Brk: %d %d (%d %d)', [
      i,
      FThrPrgInfo.Threads[i].ID, dbgs(FThrPrgInfo.Threads[i].IsCurrent), FThrPrgInfo.Threads[i].Address,
      FThrPrgInfo.Threads[i].Line-FThrPrgInfo.ThrLoopLine0, FThrPrgInfo.Threads[i].Line,
        FThrPrgInfo.Threads[i].PreviousLine-FThrPrgInfo.ThrLoopLine0,
      FThrPrgInfo.Threads[i].Val, FThrPrgInfo.Threads[i].LastVal,
      FThrPrgInfo.Threads[i].Loop, FThrPrgInfo.Threads[i].PreviousLoop,
      FThrPrgInfo.Threads[i].LastBrkLine, FThrPrgInfo.Threads[i].LastBrkLoop,
      FThrPrgInfo.Threads[i].PreviousLastBrkLine, FThrPrgInfo.Threads[i].PreviousLastBrkLoop
      ]);
  end;
end;

procedure TTestBreakPoint.ThrPrgCheckNoSkip(ATestName: String);
// Make sure no thread skipped any add. All EAX values must be correct
var
  i, l: Integer;
const
  ExpVal: array[0..10] of integer = (
      0,   1,   3,   7,  15,
     31,  63, 127, 255, 511,
   1023
  );
begin
  for i := 0 to 9 do begin
    if FThrPrgInfo.Threads[i].Line = -1 then
      continue;
    l := FThrPrgInfo.Threads[i].Line - FThrPrgInfo.ThrLoopLine0;
    TestTrue(ATestName+' line in range tid: '+inttostr(FThrPrgInfo.Threads[i].ID), (l>=-1) and (l<FThrPrgInfo.ThrLoopLast-FThrPrgInfo.ThrLoopLine0));
    if l > 9 then l := 10;
    if l < 0 then l := 10;

    TestEquals(ATestName+' Reg val for '+inttostr(FThrPrgInfo.Threads[i].ID)+ ' / '+inttostr(FThrPrgInfo.Threads[i].Line - FThrPrgInfo.ThrLoopLine0),
      ExpVal[l],
      FThrPrgInfo.Threads[i].Val
    );
  end;
end;

function TTestBreakPoint.ThrPrgInfoHasGoneThroughLine(AIndex, ALine: Integer): boolean;
var
  LoopAdjust, LastLoopAdjust: Integer;
  LoopDiff: Integer;
  Entry: TBreakThreadPrgInfoEntry;
begin
  Result := True;
  LoopAdjust := 0;
  Entry := FThrPrgInfo.Threads[AIndex];

  if Entry.Line > FThrPrgInfo.ThrLoopInc then LoopAdjust := 1;
  LastLoopAdjust := 0;
  if Entry.PreviousLine > FThrPrgInfo.ThrLoopInc then LastLoopAdjust := 1;
  LoopDiff :=
      (Entry.Loop-LoopAdjust)
    - (Entry.PreviousLoop-LastLoopAdjust);

  // Was in front of line,   and now after (or even in next loop)?
  if (Entry.PreviousLine < ALine) and
     (  (Entry.Line > ALine) or
        (LoopDiff > 0)
     )
  then
    exit;
  // Was after line, and now after AND in next loop-LoopAdjust?
  if (Entry.PreviousLine > ALine) and
     (Entry.Line > ALine) and
     (LoopDiff > 0)
  then
    exit;

  // Was exactly at line,    and now after AND in next loop-LoopAdjust?
  if (Entry.PreviousLine = ALine) and
     (Entry.Line > ALine) and
     (LoopDiff > 1)
  then
    exit;

  // More than one loop-LoopAdjust ...
  if (LoopDiff > 1)
  then
    exit;
  Result := False;
end;

procedure TTestBreakPoint.TestBreakThreadsNoSkip;
  procedure HasManyAtLine(ALine: Integer; var AManyAt, AManyAfter: Integer);
  var
    AtLine, AfterLine, i: Integer;
  begin
    AtLine := 0;
    AfterLine := 0;
    for i := 0 to 9 do
      if FThrPrgInfo.Threads[i].Line = ALine then
        inc(AtLine)
      else
      if FThrPrgInfo.Threads[i].PreviousLine = ALine then // Current line moved on, stepped over break
        inc(AfterLine);
    if AtLine    > 3 then Inc(AManyAt);
    if AfterLine > 2 then Inc(AManyAfter);
  end;
var
  ExeName: String;
  i, j: Integer;
  MainBrk, Brk1, Brk2, Brk3, Brk4, Brk5: TDBGBreakPoint;
  ManyAtBrk1, ManyAfterBrk1: Integer;
  Entry: TBreakThreadPrgInfoEntry;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestThreadNoSkip) then exit;
  Src := GetCommonSourceFor(AppDir + 'BreakPointThreadPrg.pas');
  TestCompile(Src, ExeName);

  TestTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  dbg := Debugger.LazDebugger;

  try
    MainBrk := Debugger.SetBreakPoint(Src, 'BrkMain1');
    AssertDebuggerNotInErrorState;
    Debugger.RunToNextPause(dcRun);
    AssertDebuggerState(dsPause);

    ThrPrgInitializeThreads('');
    ThrPrgUpdateThreads('Init');
    ThrPrgCheckNoSkip('Init');

    (* Stopped in the main thread.
       Set fixed breakpoints in the thread loop.
       On each run, no thread must step over any of the breakpoints.

       Since threads do reach the breakpoints, they will have to temp remove them
    *)
    MainBrk.Enabled := False;

    Brk1 := Debugger.SetBreakPoint(Src, 'BrkThread1');

    (* ManyAtBrk1 / ManyAfterBrk1
       Cumulative count accross all "j" loop iterations.
       ManyAtBrk1:    Count "j"-iterations with at least 3 threads have been at "Brk1"
       ManyAfterBrk1: Count "j"-iterations with at least 3 threads just stepped/run away from "Brk1"
    *)
    ManyAtBrk1 := 0;
    ManyAfterBrk1 := 0;

    (* Each iteration the test checks that all "add eax, n" have been executed.
    *)
    for j := 0 to 300 do begin
      AssertDebuggerNotInErrorState;
      Debugger.RunToNextPause(dcRun);
      AssertDebuggerState(dsPause);

      ThrPrgUpdateThreads('loop fixed brk '+IntToStr(j));
      ThrPrgCheckNoSkip('loop, fixed brk '+IntToStr(j));  // Compare AX with line

      for i := 0 to 9 do begin
        Entry := FThrPrgInfo.Threads[i];
        if Entry.PreviousLine < 0 then
          continue;

        TestTrue('THread not gone over break 1 at line '+IntToStr(Brk1.Line)+' '+IntToStr(i),
                 not ThrPrgInfoHasGoneThroughLine(i, Brk1.Line)
          );
      end;

      HasManyAtLine(Brk1.Line, ManyAtBrk1, ManyAfterBrk1);
      if (j > 50) and (ManyAtBrk1 > 20) and (ManyAfterBrk1 > 15) then begin
        DebugLn('~~~~~~~~~~~~~ End loop early j=%d  at=%d after=%d', [j, ManyAtBrk1, ManyAfterBrk1]);
        break;
      end;
    end;
    TestTrue('Had Many at brk1 (loop1)',    ManyAtBrk1 > 0,    0, 'Ignore / not enforcable');
    TestTrue('Had Many after brk1 (loop1)', ManyAfterBrk1 > 0, 0, 'Ignore / not enforcable');

    // Add more breaks
    Brk3 := Debugger.SetBreakPoint(Src, 'BrkThread5');
    Brk5 := Debugger.SetBreakPoint(Src, 'BrkThread9');
    // clear values from last loop
    for i := 0 to 9 do begin
      FThrPrgInfo.Threads[i].LastBrkLine := -1;
      FThrPrgInfo.Threads[i].PreviousLastBrkLine := -1;
    end;

    for j := 0 to 100 do begin
      AssertDebuggerNotInErrorState;
      Debugger.RunToNextPause(dcRun);
      AssertDebuggerState(dsPause);

      ThrPrgUpdateThreads('loop fixed brk '+IntToStr(j));
      ThrPrgCheckNoSkip('loop, fixed brk '+IntToStr(j));

      for i := 0 to 9 do begin
        Entry := FThrPrgInfo.Threads[i];
        if Entry.PreviousLine < 0 then
          continue;

        TestTrue('THread not gone over break(n/3) 1 at line '+IntToStr(Brk1.Line)+' '+IntToStr(i),
                 not ThrPrgInfoHasGoneThroughLine(i, Brk1.Line)
          );
        TestTrue('THread not gone over break(n/3) 3 at line '+IntToStr(Brk3.Line)+' '+IntToStr(i),
                 not ThrPrgInfoHasGoneThroughLine(i, Brk3.Line)
          );
        TestTrue('THread not gone over break(n/3) 5 at line '+IntToStr(Brk5.Line)+' '+IntToStr(i),
                 not ThrPrgInfoHasGoneThroughLine(i, Brk5.Line)
          );

        if Entry.PreviousLastBrkLine > 0 then begin
          if Entry.LastBrkLine = Brk1.Line then begin
            TestEquals('Previous break(n/3) before Brk1', Brk5.Line, Entry.PreviousLastBrkLine);
            TestTrue  ('Previous break-loop(n/3) before Brk1', Entry.LastBrkLoop = Entry.PreviousLastBrkLoop + 1);
          end
          else
          if Entry.LastBrkLine = Brk3.Line then begin
            TestEquals('Previous break(n/3) before Brk3', Brk1.Line, Entry.PreviousLastBrkLine);
            TestTrue  ('Previous break-loop(n/3) before Brk3', Entry.LastBrkLoop = Entry.PreviousLastBrkLoop);
          end
          else
          if Entry.LastBrkLine = Brk5.Line then begin
            TestEquals('Previous break(n/3) before Brk5', Brk3.Line, Entry.PreviousLastBrkLine);
            TestTrue  ('Previous break-loop(n/3) before Brk5', Entry.LastBrkLoop = Entry.PreviousLastBrkLoop);
          end;
        end;
      end;
    end;

    // Add more breaks
    Brk2 := Debugger.SetBreakPoint(Src, 'BrkThread3');
    Brk4 := Debugger.SetBreakPoint(Src, 'BrkThread7');
    // clear values from last loop
    for i := 0 to 9 do begin
      FThrPrgInfo.Threads[i].LastBrkLine := -1;
      FThrPrgInfo.Threads[i].PreviousLastBrkLine := -1;
    end;

    for j := 0 to 100 do begin
      AssertDebuggerNotInErrorState;
      Debugger.RunToNextPause(dcRun);
      AssertDebuggerState(dsPause);

      ThrPrgUpdateThreads('loop fixed brk '+IntToStr(j));
      ThrPrgCheckNoSkip('loop, fixed brk '+IntToStr(j));

      for i := 0 to 9 do begin
        Entry := FThrPrgInfo.Threads[i];
        if Entry.PreviousLine < 0 then
          continue;

        TestTrue('THread not gone over break(n/5) 1 at line '+IntToStr(Brk1.Line)+' '+IntToStr(i),
                 not ThrPrgInfoHasGoneThroughLine(i, Brk1.Line)
          );
        TestTrue('THread not gone over break(n/5) 2 at line '+IntToStr(Brk2.Line)+' '+IntToStr(i),
                 not ThrPrgInfoHasGoneThroughLine(i, Brk2.Line)
          );
        TestTrue('THread not gone over break(n/5) 3 at line '+IntToStr(Brk3.Line)+' '+IntToStr(i),
                 not ThrPrgInfoHasGoneThroughLine(i, Brk3.Line)
          );
        TestTrue('THread not gone over break(n/5) 4 at line '+IntToStr(Brk4.Line)+' '+IntToStr(i),
                 not ThrPrgInfoHasGoneThroughLine(i, Brk4.Line)
          );
        TestTrue('THread not gone over break(n/5) 5 at line '+IntToStr(Brk5.Line)+' '+IntToStr(i),
                 not ThrPrgInfoHasGoneThroughLine(i, Brk5.Line)
          );

      end;
    end;


    dbg.Stop;
  finally
    Debugger.ClearDebuggerMonitors;
    Debugger.FreeDebugger;

    AssertTestErrors;
  end;
end;

procedure TTestBreakPoint.TestBreakThreadsMoveBreak1;
var
  ExeName: String;
  i, j: Integer;
  MainBrk, Brk1: TDBGBreakPoint;
  Entry: TBreakThreadPrgInfoEntry;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestThreadMove1) then exit;
  Src := GetCommonSourceFor(AppDir + 'BreakPointThreadPrg.pas');
  TestCompile(Src, ExeName);

  TestTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  dbg := Debugger.LazDebugger;

  try
    MainBrk := Debugger.SetBreakPoint(Src, 'BrkMain1');
    AssertDebuggerNotInErrorState;
    Debugger.RunToNextPause(dcRun);
    AssertDebuggerState(dsPause);

    ThrPrgInitializeThreads('');
    ThrPrgUpdateThreads('Init');
    ThrPrgCheckNoSkip('Init');

    (* Stopped in the main thread.
       Set a new breakpoint at the current address of one of the subthreads.
       It should be skipped until next loop.
       Other sub-thread must not accidentally go over the breakpoint for sub-threads
    *)

    for j := 0 to 100 do begin
      Brk1 := Debugger.SetBreakPoint(Src.FileName, FThrPrgInfo.Threads[0].Line);
      AssertDebuggerNotInErrorState;
      Debugger.RunToNextPause(dcRun);
      AssertDebuggerState(dsPause);

      ThrPrgUpdateThreads('loop one brk '+IntToStr(j));
      ThrPrgCheckNoSkip('loop, one brk '+IntToStr(j));

      for i := 0 to 9 do begin
        Entry := FThrPrgInfo.Threads[i];
        if Entry.PreviousLine < 0 then
          continue;
        TestTrue('THread not gone over break at line '+IntToStr(Brk1.Line)+' '+IntToStr(i),
                 not ThrPrgInfoHasGoneThroughLine(i, Brk1.Line)
          );
      end;

      Brk1.ReleaseReference;
      if j = 0 then
        MainBrk.Enabled := False;
    end;

    dbg.Stop;
  finally
    Debugger.ClearDebuggerMonitors;
    Debugger.FreeDebugger;

    AssertTestErrors;
  end;
end;

procedure TTestBreakPoint.TestBreakThreadsMoveBreak2;
var
  ExeName: String;
  i, j: Integer;
  MainBrk, Brk1, Brk2, Brk3, Brk4, Brk5: TDBGBreakPoint;
  Entry: TBreakThreadPrgInfoEntry;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestThreadMove2) then exit;
  Src := GetCommonSourceFor(AppDir + 'BreakPointThreadPrg.pas');
  TestCompile(Src, ExeName);

  TestTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  dbg := Debugger.LazDebugger;

  try
    MainBrk := Debugger.SetBreakPoint(Src, 'BrkMain1');
    AssertDebuggerNotInErrorState;
    Debugger.RunToNextPause(dcRun);
    AssertDebuggerState(dsPause);

    ThrPrgInitializeThreads('');
    ThrPrgUpdateThreads('Init');
    ThrPrgCheckNoSkip('Init');

    (* Try more breakpoints => so there is a likelihood that a several threads
       hit breakpoints at the same time.
       Then remove the breakpoints, while the FThrPrgInfo.ThrLoopFirst thread reports the hit
    *)
    for j := 0 to 150 do begin
      if (j and 1) = 0 then begin
        Brk1 := Debugger.SetBreakPoint(Src, 'BrkThread1');
        Brk2 := Debugger.SetBreakPoint(Src, 'BrkThread3');
        Brk3 := Debugger.SetBreakPoint(Src, 'BrkThread5');
        Brk4 := Debugger.SetBreakPoint(Src, 'BrkThread7');
        Brk5 := Debugger.SetBreakPoint(Src, 'BrkThread9');
      end else begin
        Brk1 := Debugger.SetBreakPoint(Src, 'BrkThread2');
        Brk2 := Debugger.SetBreakPoint(Src, 'BrkThread4');
        Brk3 := Debugger.SetBreakPoint(Src, 'BrkThread6');
        Brk4 := Debugger.SetBreakPoint(Src, 'BrkThread8');
        Brk5 := Debugger.SetBreakPoint(Src, 'BrkThread10');
      end;
      AssertDebuggerNotInErrorState;
      Debugger.RunToNextPause(dcRun);
      AssertDebuggerState(dsPause);

      ThrPrgUpdateThreads('loop, changing brk '+IntToStr(j));
      ThrPrgCheckNoSkip('loop, changing brk '+IntToStr(j));

      for i := 0 to 9 do begin
        Entry := FThrPrgInfo.Threads[i];
        if Entry.PreviousLine < 0 then
          continue;
        TestTrue('THread not gone over break '+IntToStr(i), not ThrPrgInfoHasGoneThroughLine(i, Brk1.Line) );
        TestTrue('THread not gone over break '+IntToStr(i), not ThrPrgInfoHasGoneThroughLine(i, Brk2.Line) );
        TestTrue('THread not gone over break '+IntToStr(i), not ThrPrgInfoHasGoneThroughLine(i, Brk3.Line) );
        TestTrue('THread not gone over break '+IntToStr(i), not ThrPrgInfoHasGoneThroughLine(i, Brk4.Line) );
        TestTrue('THread not gone over break '+IntToStr(i), not ThrPrgInfoHasGoneThroughLine(i, Brk5.Line) );
      end;

      Brk1.ReleaseReference; Brk2.ReleaseReference; Brk3.ReleaseReference;  Brk4.ReleaseReference; Brk5.ReleaseReference;
    end;


    dbg.Stop;
  finally
    Debugger.ClearDebuggerMonitors;
    Debugger.FreeDebugger;

    AssertTestErrors;
  end;
end;

procedure TTestBreakPoint.TestBreakThreadsHitBreak;
  function CheckLastBrk(AnIdx, BrkLine, NextBrkLine: Integer): Boolean;
  begin
    Result := True; // defaults to ok, if Line is not in this range.
    if FThrPrgInfo.Threads[AnIdx].LastBrkLine = -1 then exit;
    if FThrPrgInfo.Threads[AnIdx].PreviousLastBrkLine = -1 then exit;

    if FThrPrgInfo.Threads[AnIdx].Line = BrkLine then begin
      if FThrPrgInfo.Threads[AnIdx].IsCurrent then
        Result := FThrPrgInfo.Threads[AnIdx].LastBrkLine = BrkLine;
      // otherwise...
    end;

    if FThrPrgInfo.Threads[AnIdx].Line = NextBrkLine then begin
      if (FThrPrgInfo.Threads[AnIdx].LastBrkLine = NextBrkLine) then
        Result := (FThrPrgInfo.Threads[AnIdx].LastBrkLoop = FThrPrgInfo.Threads[AnIdx].Loop) and
                  (FThrPrgInfo.Threads[AnIdx].PreviousLastBrkLine = BrkLine)
                  // PreviousLastBrkLoop can be equal or 1 less
      else
        Result := (FThrPrgInfo.Threads[AnIdx].LastBrkLine = BrkLine)
      ;
    end;

    if BrkLine < NextBrkLine then begin
      if (FThrPrgInfo.Threads[AnIdx].Line > BrkLine) and
         (FThrPrgInfo.Threads[AnIdx].Line < NextBrkLine)
      then begin
        Result := (FThrPrgInfo.Threads[AnIdx].LastBrkLine = BrkLine) and
                  (FThrPrgInfo.Threads[AnIdx].LastBrkLoop = FThrPrgInfo.Threads[AnIdx].Loop);
      end;
    end
    else begin  // going over loop-end
      if (FThrPrgInfo.Threads[AnIdx].Line > BrkLine) or
         (FThrPrgInfo.Threads[AnIdx].Line < NextBrkLine)
      then begin
        Result := (FThrPrgInfo.Threads[AnIdx].LastBrkLine = BrkLine) and
                  ( (FThrPrgInfo.Threads[AnIdx].LastBrkLoop = FThrPrgInfo.Threads[AnIdx].Loop) or
                    (FThrPrgInfo.Threads[AnIdx].LastBrkLoop = FThrPrgInfo.Threads[AnIdx].Loop - 1)
                  );
      end;
    end;
  end;
var
  ExeName: String;
  i, j: Integer;
  MainBrk, Brk1, Brk2, Brk3, Brk4, Brk5: TDBGBreakPoint;
  Entry: TBreakThreadPrgInfoEntry;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestThreadHit) then exit;
  Src := GetCommonSourceFor(AppDir + 'BreakPointThreadPrg.pas');
  TestCompile(Src, ExeName);

  TestTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  dbg := Debugger.LazDebugger;

  try
    MainBrk := Debugger.SetBreakPoint(Src, 'BrkMain1');
    AssertDebuggerNotInErrorState;
    Debugger.RunToNextPause(dcRun);
    AssertDebuggerState(dsPause);

    ThrPrgInitializeThreads('');
    ThrPrgUpdateThreads('Init');
    ThrPrgCheckNoSkip('Init');

    Brk1 := Debugger.SetBreakPoint(Src, 'BrkThread1');
    Brk2 := Debugger.SetBreakPoint(Src, 'BrkThread3');
    Brk3 := Debugger.SetBreakPoint(Src, 'BrkThread5');
    Brk4 := Debugger.SetBreakPoint(Src, 'BrkThread7');
    Brk5 := Debugger.SetBreakPoint(Src, 'BrkThread9');

    for j := 0 to 150 do begin
      AssertDebuggerNotInErrorState;
      Debugger.RunToNextPause(dcRun);
      AssertDebuggerState(dsPause);

      ThrPrgUpdateThreads('loop, changing brk '+IntToStr(j));
      ThrPrgCheckNoSkip('loop, changing brk '+IntToStr(j));

      for i := 0 to 9 do begin
        Entry := FThrPrgInfo.Threads[i];
        if Entry.PreviousLine < 0 then
          continue;
        TestTrue('THread not gone over break '+IntToStr(i), not ThrPrgInfoHasGoneThroughLine(i, Brk1.Line) );
        TestTrue('THread not gone over break '+IntToStr(i), not ThrPrgInfoHasGoneThroughLine(i, Brk2.Line) );
        TestTrue('THread not gone over break '+IntToStr(i), not ThrPrgInfoHasGoneThroughLine(i, Brk3.Line) );
        TestTrue('THread not gone over break '+IntToStr(i), not ThrPrgInfoHasGoneThroughLine(i, Brk4.Line) );
        TestTrue('THread not gone over break '+IntToStr(i), not ThrPrgInfoHasGoneThroughLine(i, Brk5.Line) );

        // The thread must be between any 2 of thebreakpoint, and it must have hit
        // the breakpoint at the start of the range.
        TestTrue('Has hit last brk between 1 - 2  / thr= '+IntToStr(i), CheckLastBrk(i, Brk1.Line, Brk2.Line));
        TestTrue('Has hit last brk between 2 - 3  / thr= '+IntToStr(i), CheckLastBrk(i, Brk2.Line, Brk3.Line));
        TestTrue('Has hit last brk between 3 - 4  / thr= '+IntToStr(i), CheckLastBrk(i, Brk3.Line, Brk4.Line));
        TestTrue('Has hit last brk between 4 - 5  / thr= '+IntToStr(i), CheckLastBrk(i, Brk4.Line, Brk5.Line));
        TestTrue('Has hit last brk between 5 - 1  / thr= '+IntToStr(i), CheckLastBrk(i, Brk5.Line, Brk1.Line));
      end;
    end;


    dbg.Stop;
  finally
    Debugger.ClearDebuggerMonitors;
    Debugger.FreeDebugger;

    AssertTestErrors;
  end;
end;

procedure TTestBreakPoint.TestBreakThreadsIgnoreOther;
var
  ExeName: String;
  i, j, ThreadIdMain: Integer;
  Brk: array [0..9] of TDBGBreakPoint;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestThreadIgnoreOther) then exit;
  Src := GetCommonSourceFor(AppDir + 'BreakPointThread2Prg.pas');
  TestCompile(Src, ExeName);

  TestTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  dbg := Debugger.LazDebugger;

  try
    Debugger.SetBreakPoint(Src, 'BrkMainBegin');
    for i := 0 to 9 do
      Brk[i] := Debugger.SetBreakPoint(Src, 'BrkMain'+IntToStr(i));
    AssertDebuggerNotInErrorState;
    Debugger.RunToNextPause(dcRun);
    AssertDebuggerState(dsPause, 'main init');

    ThreadIdMain := dbg.Threads.CurrentThreads.CurrentThreadId;


    for j := 1 to 70 do begin
      for i := 0 to 9 do begin

        Debugger.RunToNextPause(dcRun, 15000);
        AssertDebuggerState(dsPause, 'in loop');
        TestEquals('ThreadId', ThreadIdMain, dbg.Threads.CurrentThreads.CurrentThreadId);
        TestLocation('loop '+IntToStr(j)+', '+IntToStr(i), 'BrkMain'+IntToStr(i), j);

        if i > 0 then
          TestEquals('prev brk hitcnt '+IntToStr(j)+', '+IntToStr(i),
                     j, Debugger.BreakPointByName('BrkMain'+IntToStr(i)).HitCount)
          else
          TestEquals('prev brk hitcnt '+IntToStr(j)+', '+IntToStr(i),
                     j-1, Debugger.BreakPointByName('BrkMain9').HitCount);
      end;
    end;


    dbg.Stop;
  finally
    Debugger.ClearDebuggerMonitors;
    Debugger.FreeDebugger;

    AssertTestErrors;
  end;
end;


initialization

  RegisterDbgTest(TTestBreakPoint);
  ControlTest                  := TestControlRegisterTest('TTestBreak');
  ControlTestBreak             := TestControlRegisterTest('TTestBreakPoint', ControlTest);
  ControlTestThreadNoSkip      := TestControlRegisterTest('TTestBreakThreadNoSkip', ControlTest);
  ControlTestThreadMove1       := TestControlRegisterTest('TTestBreakThreadMove1', ControlTest);
  ControlTestThreadMove2       := TestControlRegisterTest('TTestBreakThreadMove2', ControlTest);
  ControlTestThreadHit         := TestControlRegisterTest('TTestBreakThreadHit', ControlTest);
  ControlTestThreadIgnoreOther := TestControlRegisterTest('TTestBreakThreadIgnoreOther', ControlTest);
end.

