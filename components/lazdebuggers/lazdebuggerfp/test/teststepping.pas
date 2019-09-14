unit TestStepping;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, TestDbgControl, TestDbgTestSuites,
  TTestWatchUtilities, TestCommonSources, TestDbgConfig, TestOutputLogger,
  DbgIntfDebuggerBase, DbgIntfBaseTypes, LazLoggerBase, Forms;

type

  { TTestStepping }

  TTestStepping = class(TDBGTestCase)
  protected
    Src: TCommonSource;
    Dbg: TDebuggerIntf;
    procedure TestLocation(ATestName, ABrkName: String; ABreakHitCount: Integer = 1); // only line-number
    function IsAtLocation(ABrkName: String): Boolean; // only line-number
  published
    (* Step over to work with various events happening during the step
       - creation/exit of threads
       - ignored breakpoints (same thread / other thread)
       - TODO: ignored exception: caught inside the step
       - TODO: ignored exception: caught caught outside the step (step to except/finally)
    *)
    procedure TestStepOver;
  end;

implementation

var
  ControlTest, ControlTestStepOver: Pointer;

procedure TTestStepping.TestLocation(ATestName, ABrkName: String;
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

function TTestStepping.IsAtLocation(ABrkName: String): Boolean;
var
  lc: TDBGLocationRec;
begin
  lc := Debugger.LazDebugger.GetLocation;
  Result := Src.BreakPoints[ABrkName] = lc.SrcLine;
end;

procedure TTestStepping.TestStepOver;
var
  ExeName: String;
  MainBrk, BrkDis, BrkHitCnt: TDBGBreakPoint;
  ThreadIdMain: Integer;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestStepOver) then exit;
  Src := GetCommonSourceFor(AppDir + 'StepOverPrg.pas');
  TestCompile(Src, ExeName);

  TestTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  dbg := Debugger.LazDebugger;

  try
    MainBrk := Debugger.SetBreakPoint(Src, 'BrkStart');
    Debugger.SetBreakPoint(Src, 'BrkThreadCreateInStep');
    Debugger.SetBreakPoint(Src, 'BrkInterfereByThread');

    BrkDis    := Debugger.SetBreakPoint(Src, 'BrkDisabled');
    BrkHitCnt := Debugger.SetBreakPoint(Src, 'BrkHitCnt');
    BrkDis.Enabled := False;
BrkHitCnt.Enabled := False;
    BrkHitCnt.BreakHitCount := 999;
    AssertDebuggerNotInErrorState;

    Debugger.RunToNextPause(dcRun);
    AssertDebuggerState(dsPause);
    TestLocation('Init', 'BrkStart');
    ThreadIdMain := dbg.Threads.CurrentThreads.CurrentThreadId;

    Debugger.RunToNextPause(dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStep', 'AfterStep', -1);

    Debugger.RunToNextPause(dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepLongLine', 'AfterStepLongLine', -1);

    Debugger.RunToNextPause(dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepProc', 'AfterStepProc', -1);

    Debugger.RunToNextPause(dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepProcLong', 'AfterStepProcLong', -1);

    Debugger.RunToNextPause(dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepSleepProc', 'AfterStepSleepProc', -1);

    Debugger.RunToNextPause(dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepSleep', 'AfterStepSleep', -1);

    Debugger.RunToNextPause(dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepBrkDis', 'AfterStepBrkDis', -1);

    Debugger.RunToNextPause(dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepBrkHitCnt', 'AfterStepBrkHitCnt', -1);

    TestEquals('No Hit for disabled break', 0, BrkDis.HitCount);
    //TestEquals('No Hit for skipped break',  1, BrkHitCnt.HitCount);



    (* The debugger will encounter a thread create event, during the stepping
       This will mean the main-loop's FCurrentThread is the new thread
    *)
    Debugger.RunToNextPause(dcRun);
    AssertDebuggerState(dsPause);
    TestLocation('At BrkThreadCreateInStep', 'BrkThreadCreateInStep', -1);

    Debugger.RunToNextPause(dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterThreadCreateInStep', 'AfterThreadCreateInStep', -1);
    TestEquals('ThreadId AfterThreadCreateInStep', ThreadIdMain, dbg.Threads.CurrentThreads.CurrentThreadId);

    (* The debugger will step over a call.
       Other threads will hit the FHiddenBreakpoint
    *)
   Debugger.RunToNextPause(dcRun);
    AssertDebuggerState(dsPause);
    TestLocation('At BrkInterfereByThread', 'BrkInterfereByThread', -1);

    Debugger.RunToNextPause(dcStepOver);
    Debugger.RunToNextPause(dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterInterfereByThread', 'AfterInterfereByThread', -1);
    TestEquals('ThreadId AfterInterfereByThread', ThreadIdMain, dbg.Threads.CurrentThreads.CurrentThreadId);


    dbg.Stop;
  finally
    Debugger.ClearDebuggerMonitors;
    Debugger.FreeDebugger;

    AssertTestErrors;
  end;
end;


initialization

  RegisterDbgTest(TTestStepping);
  ControlTest              := TestControlRegisterTest('TTestStepping');
  ControlTestStepOver      := TestControlRegisterTest('TTestStepOver', ControlTest);
end.

