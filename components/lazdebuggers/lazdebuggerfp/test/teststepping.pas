unit TestStepping;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, TestDbgControl, TestDbgTestSuites, TestBase,
  TTestWatchUtilities, TestCommonSources, TestDbgConfig, TestOutputLogger,
  FpDebugDebugger, FpDebugDebuggerUtils, DbgIntfDebuggerBase, DbgIntfBaseTypes,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif}, Forms;

type

  { TTestStepping }

  TTestStepping = class(TDBGTestCase)
  protected
    Src: TCommonSource;
    Dbg: TDebuggerIntf;

    FGotExceptCount: Integer;
    FGotExceptClass: String;
    FGotExceptMsg: String;
    FGotExceptType: TDBGExceptionType;
    FGotExceptionLocation: TDBGLocationRec;
    FContinue: Boolean;

    function TestLocation(ATestName, ABrkName: String; ABreakHitCount: Integer = 1; AnAcceptLinesBefore: integer = 0): Boolean; // only line-number
    procedure TestLoopCount(ATestName: String);
    function IsAtLocation(ABrkName: String; ATrueOnNoLine: Boolean = False): Boolean; // only line-number
    procedure DoDebuggerException(Sender: TObject;
                                  const AExceptionType: TDBGExceptionType;
                                  const AExceptionClass: String;
                                  const AExceptionLocation: TDBGLocationRec;
                                  const AExceptionText: String;
                                  out AContinue: Boolean);
    function StepOverToLine(ATestName, ABrkName: String; AnExitIfNoLineInfo: Boolean = False;
      ACmd: TDBGCommand = dcStepOver; AMaxSteps: Integer = 100; AForbiddenLine: String = ''): Boolean;
    function StepIfAtLine(ATestName, ABrkName: String; ACmd: TDBGCommand = dcStepOver; AnExpAtBrkName: String = ''): Boolean;
    procedure DoTestStepOver(ANextOnlyStopOnStartLine: Boolean);
    procedure DoTestStepOverInstr(ANextOnlyStopOnStartLine: Boolean);
    procedure DoTestExceptionStepOutEx(ANextOnlyStopOnStartLine: Boolean);
    procedure DoTestExceptionStepOverEx(ANextOnlyStopOnStartLine: Boolean);
    procedure DoTestStepTryBlocks(ANextOnlyStopOnStartLine: Boolean);
  public
    function RunToNextPauseNoLoopBreak(AName: String; ACmd: TDBGCommand; ATimeOut: Integer;
      AnExpAtBrkName: String = ''; AnExpAcceptLinesBefore: integer = 0): Boolean;
    function RunToNextPauseNoLoopBreak(AName: String; ACmd: TDBGCommand;
      AnExpAtBrkName: String = ''; AnExpAcceptLinesBefore: integer = 0): Boolean;
    function RunToNextPauseEx(AName: String; ACmd: TDBGCommand;
      AnExpAtBrkName: String = ''; AnExpAcceptLinesBefore: integer = 0): Boolean;
  published
    (* Step over to work with various events happening during the step
       - creation/exit of threads
       - ignored breakpoints (same thread / other thread)
       - TODO: ignored exception: caught inside the step
       - TODO: ignored exception: caught caught outside the step (step to except/finally)
    *)
    procedure TestStepOver;
    procedure TestStepOverInstr;
    procedure TestExceptionStepOutEx;
    procedure TestExceptionStepOverEx;
    procedure TestStepTryBlocks;

    procedure TestStepOver_NextOnlyFalse;
    procedure TestStepOverInstr_NextOnlyFalse;
    procedure TestExceptionStepOutEx_NextOnlyFalse;
    procedure TestExceptionStepOverEx_NextOnlyFalse;
    procedure TestStepTryBlocks_NextOnlyFalse;
  end;

implementation

var
  ControlTest,
  ControlTest_NextOnlyTrue, ControlTestStepOver, ControlTestStepOverInstr,
  ControlTestExceptionStepOutEx, ControlTestExceptionStepOverEx, ControlTestStepTryBlocks: Pointer;

  ControlTest_NextOnly, ControlTestStepOver_NextOnly, ControlTestStepOverInstr_NextOnly,
  ControlTestExceptionStepOutEx_NextOnly, ControlTestExceptionStepOverEx_NextOnly, ControlTestStepTryBlocks_NextOnly: Pointer;

function TTestStepping.TestLocation(ATestName, ABrkName: String;
  ABreakHitCount: Integer; AnAcceptLinesBefore: integer): Boolean;
var
  lc: TDBGLocationRec;
  brk: LongInt;
begin
  Result := True;
  AssertDebuggerState(dsPause);
  lc := Debugger.LazDebugger.GetLocation;
  brk := Src.BreakPoints[ABrkName];
  if (AnAcceptLinesBefore > 0) and (lc.SrcLine < brk) and (lc.SrcLine >= brk - AnAcceptLinesBefore)
  then
    Result := TestEquals(ATestName+' '+ABrkName+' Loc', brk, lc.SrcLine, 0, 'Ignored - In AcceptLinesBefore range')
  else
    Result := TestEquals(ATestName+' '+ABrkName+' Loc', brk, lc.SrcLine);
  if ABreakHitCount >= 0 then
    if not TestEquals(ATestName+' '+ABrkName+' HitCnt', Debugger.BreakPointByName(ABrkName).HitCount, ABreakHitCount)
    then
      Result := False;
end;

procedure TTestStepping.TestLoopCount(ATestName: String);
begin
  TestEquals(ATestName+' - No unexpected breaks of debugloop', 1, THookedFpDebugDebugger(dbg).LockRelCount);
  THookedFpDebugDebugger(dbg).LockRelCount := 0;
end;

function TTestStepping.IsAtLocation(ABrkName: String; ATrueOnNoLine: Boolean
  ): Boolean;
var
  lc: TDBGLocationRec;
begin
  lc := Debugger.LazDebugger.GetLocation;
  if (lc.SrcFile = '') then
    exit(ATrueOnNoLine);
  Result := Src.BreakPoints[ABrkName] = lc.SrcLine;
end;

procedure TTestStepping.DoDebuggerException(Sender: TObject;
  const AExceptionType: TDBGExceptionType; const AExceptionClass: String;
  const AExceptionLocation: TDBGLocationRec; const AExceptionText: String; out
  AContinue: Boolean);
begin
  inc(FGotExceptCount);
  FGotExceptClass := AExceptionClass;
  FGotExceptMsg   := AExceptionText;
  FGotExceptType  := AExceptionType;
  FGotExceptionLocation := AExceptionLocation;
  AContinue := FContinue;
end;

function TTestStepping.StepOverToLine(ATestName, ABrkName: String;
  AnExitIfNoLineInfo: Boolean; ACmd: TDBGCommand; AMaxSteps: Integer;
  AForbiddenLine: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  ATestName := ATestName + dbgs(ACmd) + 'TO: ' + ABrkName + ' (StepOverToLine) ';
  i := 1;
  while (i <= AMaxSteps) and (not IsAtLocation(ABrkName, AnExitIfNoLineInfo)) do begin
    RunToNextPauseNoLoopBreak(ATestName+' '+IntToStr(i), ACmd);
    AssertDebuggerState(dsPause, ATestName);
    if (AForbiddenLine <> '') and IsAtLocation(AForbiddenLine, False) then
      TestFalse(ATestName+' FORBIDDEN LINE '+AForbiddenLine, True);
    inc(i);
  end;
  Result := IsAtLocation(ABrkName, AnExitIfNoLineInfo);
  if not AnExitIfNoLineInfo then
    TestLocation(ATestName, ABrkName, -1);
  debugln(['XXXXXXXXXXXXXXXXXXXXXXXX to ', ABrkName, '  ',ATestName]);
end;

function TTestStepping.StepIfAtLine(ATestName, ABrkName: String;
  ACmd: TDBGCommand; AnExpAtBrkName: String): Boolean;
begin
  Result := True;
  ATestName := ATestName + dbgs(ACmd) + ' (StepIfAtLine) ';
  if not IsAtLocation(ABrkName) then
    exit;
  RunToNextPauseNoLoopBreak(ATestName, ACmd);
  TestTrue(ATestName+' finally entered at begin (not end) / '+ ABrkName, False, 0, 'ignore');
  AssertDebuggerState(dsPause, ATestName);
  debugln(['XXXXXXXXXXXXXXXXXXXXXXXX STEPPED from END LINE to begin??? ', ABrkName, '  ',ATestName]);

  if AnExpAtBrkName <> '' then
    TestLocation(ATestName+': Finished at Line ', AnExpAtBrkName, -1);
end;

procedure TTestStepping.DoTestStepOver(ANextOnlyStopOnStartLine: Boolean);
var
  ExeName: String;
  MainBrk, BrkDis, BrkHitCnt: TDBGBreakPoint;
  ThreadIdMain: Integer;
begin
  Src := GetCommonSourceFor(AppDir + 'StepOverPrg.pas');
  TestCompile(Src, ExeName);

  TestTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  dbg := Debugger.LazDebugger;
  TFpDebugDebuggerProperties(dbg.GetProperties).NextOnlyStopOnStartLine := ANextOnlyStopOnStartLine;

  try
    MainBrk := Debugger.SetBreakPoint(Src, 'BrkStart');
    Debugger.SetBreakPoint(Src, 'BrkThreadCreateInStep');
    Debugger.SetBreakPoint(Src, 'BrkInterfereByThread');
    Debugger.SetBreakPoint(Src, 'BrkNested');
    Debugger.SetBreakPoint(Src, 'CallStepOverEnd');

    BrkDis    := Debugger.SetBreakPoint(Src, 'BrkDisabled');
    BrkHitCnt := Debugger.SetBreakPoint(Src, 'BrkHitCnt');
    BrkDis.Enabled := False;
    BrkHitCnt.BreakHitCount := 999;
    AssertDebuggerNotInErrorState;

    RunToNextPauseNoInternal('', dcRun);
    AssertDebuggerState(dsPause);
    TestLocation('Init', 'BrkStart');
    ThreadIdMain := dbg.Threads.CurrentThreads.CurrentThreadId;

    // Step over a line
    RunToNextPauseNoLoopBreak('', dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStep', 'AfterStep', -1);

    // Step over a longer line
    RunToNextPauseNoLoopBreak('', dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepLongLine', 'AfterStepLongLine', -1);

    // Step over a subroutine call
    RunToNextPauseNoLoopBreak('', dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepProc', 'AfterStepProc', -1);

    // Step over a several subroutine calls
    RunToNextPauseNoLoopBreak('', dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepProcLong', 'AfterStepProcLong', -1);

    // Step over a subroutine call, with sleep
    RunToNextPauseNoLoopBreak('', dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepSleepProc', 'AfterStepSleepProc', -1);

    // Step over a call to sleep
    RunToNextPauseNoLoopBreak('', dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepSleep', 'AfterStepSleep', -1);

    // Step over a subroutine call, with a disabled breakpoint
    RunToNextPauseNoLoopBreak('', dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepBrkDis', 'AfterStepBrkDis', -1);

    // Step over a subroutine call, with a breakpoint that continues
    RunToNextPauseTestInternal('', 1, dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepBrkHitCnt', 'AfterStepBrkHitCnt', -1);

    TestEquals('No Hit for disabled break', 0, BrkDis.HitCount);
    TestEquals('No Hit for skipped break',  1, BrkHitCnt.HitCount);

    BrkDis.Enabled := True;
    // Step over a subroutine call, BUT STOP at the breakpoint within it
    RunToNextPauseNoLoopBreak('', dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At BrkDisabled', 'BrkDisabled', -1);
    // And do another step
    RunToNextPauseNoLoopBreak('', dcStepOver);
    AssertDebuggerState(dsPause);

    TestEquals('No Hit for disabled break', 1, BrkDis.HitCount);
    TestEquals('No Hit for skipped break',  1, BrkHitCnt.HitCount);


    // Step over a RECURSIVE subroutine call
    RunToNextPauseNoLoopBreak('', dcRun);
    AssertDebuggerState(dsPause);
    TestLocation('At BrkNested', 'BrkNested', -1);

    RunToNextPauseNoLoopBreak('', dcStepOver);
    AssertDebuggerState(dsPause);
    RunToNextPauseNoLoopBreak('', dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterNested', 'AfterNested', -1);


    (* The debugger will encounter a thread create event, during the stepping
       This will mean the main-loop's FCurrentThread is the new thread
    *)
    RunToNextPauseNoLoopBreak('', dcRun);
    AssertDebuggerState(dsPause);
    TestLocation('At BrkThreadCreateInStep', 'BrkThreadCreateInStep', -1);

    // This test can take longer, as the new thread gets very little scheduler time
    // during the single stepping of the main thread.
    RunToNextPauseNoLoopBreak('', dcStepOver, 25000);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterThreadCreateInStep', 'AfterThreadCreateInStep', -1);
    TestEquals('ThreadId AfterThreadCreateInStep', ThreadIdMain, dbg.Threads.CurrentThreads.CurrentThreadId);

    (* The debugger will step over a call.
       Other threads will hit the FHiddenBreakpoint
    *)
    RunToNextPauseNoLoopBreak('', dcRun);
    AssertDebuggerState(dsPause);
    TestLocation('At BrkInterfereByThread', 'BrkInterfereByThread', -1);

    RunToNextPauseNoLoopBreak('', dcStepOver);
    RunToNextPauseNoLoopBreak('', dcStepOver);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterInterfereByThread', 'AfterInterfereByThread', -1);
    TestEquals('ThreadId AfterInterfereByThread', ThreadIdMain, dbg.Threads.CurrentThreads.CurrentThreadId);




    (* go to CallMyNested
       Step into, and step to endline
       => ensure "end" takes ONE step to leave
    *)
    RunToNextPauseNoLoopBreak('', dcRun);
    TestLocation('At CallStepOverEnd', 'CallStepOverEnd', -1);
    RunToNextPauseNoLoopBreak('', dcStepInto);
    RunToNextPauseNoLoopBreak('', dcStepOver);
    if not IsAtLocation('StepOverEnd') then  // depends on "begin" was code or not
      RunToNextPauseNoLoopBreak('', dcStepOver);

    RunToNextPauseNoLoopBreak('', dcStepInto);
    RunToNextPauseNoLoopBreak('', dcStepOver);
    if not IsAtLocation('StepOverEnd') then  // depends on "begin" was code or not
      RunToNextPauseNoLoopBreak('', dcStepOver);

    TestLocation('At StepOverEnd', 'StepOverEnd', -1);
    RunToNextPauseNoLoopBreak('', dcStepOver);
    RunToNextPauseNoLoopBreak('', dcStepOver);
    TestLocation('At AfterCallStepOverEnd', 'AfterCallStepOverEnd', -1);


    dbg.Stop;
  finally
    Debugger.FreeDebugger;

    AssertTestErrors;
  end;
end;

procedure TTestStepping.TestStepOver;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestStepOver) then exit;
  DoTestStepOver(True);
end;

procedure TTestStepping.TestStepOverInstr;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestStepOverInstr) then exit;
  DoTestStepOverInstr(True);
end;

procedure TTestStepping.TestExceptionStepOutEx;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestExceptionStepOutEx) then exit;
  DoTestExceptionStepOutEx(True);
end;

procedure TTestStepping.TestExceptionStepOverEx;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestExceptionStepOverEx) then exit;
  DoTestExceptionStepOverEx(True);
end;

procedure TTestStepping.TestStepTryBlocks;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestStepTryBlocks) then exit;
  DoTestStepTryBlocks(True);
end;

procedure TTestStepping.TestStepOver_NextOnlyFalse;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestStepOver_NextOnly) then exit;
  DoTestStepOver(False);
end;

procedure TTestStepping.TestStepOverInstr_NextOnlyFalse;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestStepOverInstr_NextOnly) then exit;
  DoTestStepOverInstr(False);
end;

procedure TTestStepping.TestExceptionStepOutEx_NextOnlyFalse;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestExceptionStepOutEx_NextOnly) then exit;
  DoTestExceptionStepOutEx(False);
end;

procedure TTestStepping.TestExceptionStepOverEx_NextOnlyFalse;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestExceptionStepOverEx_NextOnly) then exit;
  DoTestExceptionStepOverEx(False);
end;

procedure TTestStepping.TestStepTryBlocks_NextOnlyFalse;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestStepTryBlocks_NextOnly) then exit;
  DoTestStepTryBlocks(False);
end;

procedure TTestStepping.DoTestStepOverInstr(ANextOnlyStopOnStartLine: Boolean);
  procedure StepInstrToNextLine(AName: String; AnExpIntPauseCnt: integer = 0);
  var
    lc: TDBGLocationRec;
  begin
    lc := Debugger.LazDebugger.GetLocation;
    repeat
      RunToNextPauseTestInternal('', AnExpIntPauseCnt, dcStepOverInstr);
      AssertDebuggerState(dsPause, 'step instr '+AName);
    until (lc.SrcLine <> Debugger.LazDebugger.GetLocation.SrcLine);
  end;

var
  ExeName: String;
  MainBrk, BrkDis, BrkHitCnt: TDBGBreakPoint;
  ThreadIdMain: Integer;
begin
  Src := GetCommonSourceFor(AppDir + 'StepOverPrg.pas');
  TestCompile(Src, ExeName);

  TestTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  dbg := Debugger.LazDebugger;
  TFpDebugDebuggerProperties(dbg.GetProperties).NextOnlyStopOnStartLine := ANextOnlyStopOnStartLine;

  try
    MainBrk := Debugger.SetBreakPoint(Src, 'BrkStart');
    Debugger.SetBreakPoint(Src, 'BrkThreadCreateInStep');
    Debugger.SetBreakPoint(Src, 'BrkInterfereByThread');
    Debugger.SetBreakPoint(Src, 'BrkNested');

    BrkDis    := Debugger.SetBreakPoint(Src, 'BrkDisabled');
    BrkHitCnt := Debugger.SetBreakPoint(Src, 'BrkHitCnt');
    BrkDis.Enabled := False;
    BrkHitCnt.BreakHitCount := 999;
    AssertDebuggerNotInErrorState;

    RunToNextPauseNoInternal('', dcRun);
    AssertDebuggerState(dsPause);
    TestLocation('Init', 'BrkStart');
    ThreadIdMain := dbg.Threads.CurrentThreads.CurrentThreadId;

    StepInstrToNextLine('Go to AfterStep');
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStep', 'AfterStep', -1);

    StepInstrToNextLine('Go to AfterStepLongLine');
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepLongLine', 'AfterStepLongLine', -1);

    StepInstrToNextLine('Go to AfterStepProc');
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepProc', 'AfterStepProc', -1);

    StepInstrToNextLine('Go to AfterStepProcLong');
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepProcLong', 'AfterStepProcLong', -1);

    StepInstrToNextLine('Go to AfterStepSleepProc');
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepSleepProc', 'AfterStepSleepProc', -1);

    StepInstrToNextLine('Go to AfterStepSleep');
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepSleep', 'AfterStepSleep', -1);

    StepInstrToNextLine('Go to AfterStepBrkDis');
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepBrkDis', 'AfterStepBrkDis', -1);

    StepInstrToNextLine('Go to AfterStepBrkHitCnt', 1);
    AssertDebuggerState(dsPause);
    TestLocation('At AfterStepBrkHitCnt', 'AfterStepBrkHitCnt', -1);

    TestEquals('No Hit for disabled break', 0, BrkDis.HitCount);
    TestEquals('No Hit for skipped break',  1, BrkHitCnt.HitCount);


    RunToNextPauseNoLoopBreak('', dcRun);
    AssertDebuggerState(dsPause);
    TestLocation('At BrkNested', 'BrkNested', -1);

    StepInstrToNextLine('Go to AfterNested 1');
    AssertDebuggerState(dsPause);
    StepInstrToNextLine('Go to AfterNested 2');
    AssertDebuggerState(dsPause);
    TestLocation('At AfterNested', 'AfterNested', -1);



    dbg.Stop;
  finally
    Debugger.FreeDebugger;

    AssertTestErrors;
  end;
end;

procedure TTestStepping.DoTestExceptionStepOutEx(
  ANextOnlyStopOnStartLine: Boolean);
var
  ExeName, TstName: String;
begin
  ClearTestErrors;
  FGotExceptCount := 0;

  Src := GetCommonSourceFor(AppDir + 'ExceptTestPrg.pas');
  TestCompile(Src, ExeName);

  TestTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  dbg := Debugger.LazDebugger;
  TFpDebugDebuggerProperties(dbg.GetProperties).NextOnlyStopOnStartLine := ANextOnlyStopOnStartLine;
  try
    dbg.Exceptions.Add('MyExceptionIgnore').Enabled := False;
    dbg.OnException      := @DoDebuggerException;

    TstName := ' Run to Except';
    FContinue := False;
    RunToNextPauseNoInternal(TestName, dcRun);
    TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
//    TestLocation(TstName+': CurLine ', 'BrkMyRaise', -1);
    FContinue := True;

    THookedFpDebugDebugger(dbg).LockRelCount := 0;
    TstName := ' Step';
    RunToNextPauseNoLoopBreak(TestName, dcStepOver);
    TestLocation(TstName+': CurLine ', 'BrkStep3Fin_A', -1);
    //TestLoopCount(TstName+': BrkStep3Fin_A');

    RunToNextPauseNoLoopBreak(TestName, dcStepOut);
    TestLocation(TstName+': CurLine ', 'BrkStep3FinOuter_A', -1);
    //TestLoopCount(TstName+': BrkStep3FinOuter_A');

    RunToNextPauseNoLoopBreak(TestName, dcStepOut);
    TestLocation(TstName+': CurLine ', 'BrkStepMainExcept1', -1, 1);
    //TestLoopCount(TstName+': BrkStepMainExcept1');


    dbg.Stop;
  finally
    Debugger.FreeDebugger;
  end;

  AssertTestErrors;
end;

procedure TTestStepping.DoTestExceptionStepOverEx(
  ANextOnlyStopOnStartLine: Boolean);
  procedure ExpectEnterFinally(AName: String; ATestAppRecStep: Integer;
    ATestIgnoreRaise, ATestRaiseSkipped, ATestStepOverNested: Boolean;
    ATestIgnoreRaise_2, ATestRaiseSkipped_2, ATestStepOverNested_2: Boolean);
  var
    TstName: String;
    MyRaiseBrk: TDBGBreakPoint;
  begin
    TstName := AName + ' Run to raise';
    FGotExceptCount := 0;
    FContinue := ATestIgnoreRaise or ATestStepOverNested;

    if ATestStepOverNested then begin
      // step to call line, starting on:  Nop => TestVal => If ... then => call
      RunToNextPauseNoInternal(TstName, dcStepOver);
      RunToNextPauseNoInternal(TstName, dcStepOver);
      RunToNextPauseNoInternal(TstName, dcStepOver);
// step is done by caller
//      RunToNextPauseNoInternal(TstName, dcStepOver);  // Step over recurse
      //TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
      exit;
    end
    else
    if ATestIgnoreRaise then begin
      MyRaiseBrk := Debugger.SetBreakPoint(Src, 'BrkMyRaise');
      RunToNextPauseNoInternal(TstName, dcRun);
      MyRaiseBrk.ReleaseReference;
      RunToNextPauseNoInternal(TstName, dcStepOver);  // exception will be ignored => step to finally
      TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
    end
    else begin
      RunToNextPauseNoInternal(TstName, dcRun);
      TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
// TODO: currently reports in except.inc
//      TestLocation(TstName+': CurLine ', 'BrkMyRaise', -1);
      RunToNextPauseNoInternal(TstName, dcStepOver);
    end;

    TstName := AName + ' Run to Finally A';
    TestLocation(TstName+': CurLine ', 'BrkStep3Fin_A', -1);

    if (ATestAppRecStep = 1) and (not ATestRaiseSkipped) then
      ExpectEnterFinally(TstName+' INNER ', 0, ATestIgnoreRaise_2, ATestRaiseSkipped_2, ATestStepOverNested_2, False, False, False)
    else
      StepOverToLine(TstName, 'BrkStep3Fin_A_END', True);

    TstName := AName + ' Run to Finally B';
    RunToNextPauseNoInternal(TstName, dcStepOver);  // Step to next finally
    TestLocation(TstName+': CurLine ', 'BrkStep3Fin_B', -1);

    //if (ATestAppRecStep = 2) and (not ATestRaiseSkipped) then
    //  ExpectEnterFinally(TstName+' INNER ', 0, ATestIgnoreRaise_2, ATestRaiseSkipped_2, ATestStepOverNested_2, False, False, False)
    //else
      StepOverToLine(TstName, 'BrkStep3Fin_B_END', True);

    TstName := AName + ' Run to Finally C';
    RunToNextPauseNoLoopBreak(TstName, dcStepOver);  // Step to next finally
    TestLocation(TstName+': CurLine ', 'BrkStep3Fin_C', -1);

    if (ATestAppRecStep = 2) and (not ATestRaiseSkipped) then
      ExpectEnterFinally(TstName+' INNER ', 0, ATestIgnoreRaise_2, ATestRaiseSkipped_2, ATestStepOverNested_2, False, False, False)
    else
      StepOverToLine(TstName, 'BrkStep3Fin_C_END', True);

    TstName := AName + ' Run to Finally A(outer)';
    RunToNextPauseNoLoopBreak(TstName, dcStepOver);  // Step to next finally
    //StepIfAtLine(TstName, 'BrkStep3Fin_IMPLICIT'); // 32 bit
    //StepIfAtLine(TstName, 'BrkStep3Fin_IMPLICIT_1');
    TestLocation(TstName+': CurLine ', 'BrkStep3FinOuter_A', -1);

    if (ATestAppRecStep = 3) and (not ATestRaiseSkipped) then
      ExpectEnterFinally(TstName+' INNER ', 0, ATestIgnoreRaise_2, ATestRaiseSkipped_2, ATestStepOverNested_2, False, False, False)
    else
      StepOverToLine(TstName, 'BrkStep3FinOuter_A_END', True);

    TstName := AName + ' Run to Finally B(outer)';
    RunToNextPauseNoLoopBreak(TstName, dcStepOver);  // Step to next finally
    TestLocation(TstName+': CurLine ', 'BrkStep3FinOuter_B', -1);

    //if (ATestAppRecStep = 5) and (not ATestRaiseSkipped) then
    //  ExpectEnterFinally(TstName+' INNER ', 0, ATestIgnoreRaise_2, ATestRaiseSkipped_2, ATestStepOverNested_2, False, False, False)
    //else
      StepOverToLine(TstName, 'BrkStep3FinOuter_B_END', True);

    TstName := AName + ' Run to Finally C(outer)';
    RunToNextPauseNoLoopBreak(TstName, dcStepOver);  // Step to next finally
    TestLocation(TstName+': CurLine ', 'BrkStep3FinOuter_C', -1);

    if (ATestAppRecStep = 4) and (not ATestRaiseSkipped) then
      ExpectEnterFinally(TstName+' INNER ', 0, ATestIgnoreRaise_2, ATestRaiseSkipped_2, ATestStepOverNested_2, False, False, False)
    else
      StepOverToLine(TstName,'BrkStep3FinOuter_C_END', True);
  end;

  procedure ExpectNestedExcept(AName: String);
  var
    TstName: String;
  begin
    TstName := AName + ' Run to raise';
    FGotExceptCount := 0;
    FContinue := False;
    RunToNextPauseNoLoopBreak(TstName, dcRun);
    TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
//    TestLocation(TstName+': CurLine ', 'BrkMyRaise', -1);

    TstName := AName + ' Run to except fin';
    RunToNextPauseNoLoopBreak(TstName, dcStepOver);  // Step to fin
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept_Finally', -1);

    // NESTED
    TstName := AName + ' Run to raise nested';
    FGotExceptCount := 0;
    FContinue := False;
    RunToNextPauseNoLoopBreak(TstName, dcRun);
    TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
//    TestLocation(TstName+': CurLine ', 'BrkMyRaise', -1);

    TstName := AName + ' Run to except fin nested';
    RunToNextPauseNoLoopBreak(TstName, dcStepOver);  // Step to fin
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept_Finally', -1);

    StepOverToLine(TstName,'BrkStepNestedExcept_Finally_END', True);

    TstName := AName + ' Run to except nested';
    RunToNextPauseNoLoopBreak(TstName, dcStepOver);  // Step to next finally
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept', -1, 1);

    StepOverToLine(TstName,'BrkStepNestedExcept_END', True);
    // END NESTED

    TstName := AName + ' Run back except fin';
    RunToNextPauseNoLoopBreak(TstName, dcStepOver);  // Step back to end
    StepIfAtLine(TstName, 'BrkStepNestedExcept_TRY'); // may step to "try"
    RunToNextPauseNoLoopBreak(TstName, dcStepOver);  // Step back to finaly
    //if not ANextOnlyStopOnStartLine then
    //  StepIfAtLine(TstName, 'BrkStepNestedExcept_Finally_BEFORE'); // TODO: XXXXX StepOver may stop at the step out line.
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept_Finally_AFTER', -1);

    StepOverToLine(TstName,'BrkStepNestedExcept_Finally_END', True);

    TstName := AName + ' Run to except';
    RunToNextPauseNoLoopBreak(TstName, dcStepOver);  // Step to next finally
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept', -1, 1);

    RunToNextPauseNoLoopBreak(TstName, dcStepOut);  // Step out
  end;

  procedure ExpectNestedExcept_Ignore(AName: String);
  var
    TstName: String;
  begin
    TstName := AName + ' Run to raise';
    FGotExceptCount := 0;
    FContinue := False;
    RunToNextPauseNoLoopBreak(TstName, dcRun);
    TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
//    TestLocation(TstName+': CurLine ', 'BrkMyRaise', -1);

    TstName := AName + ' Run to except fin';
    RunToNextPauseNoLoopBreak(TstName, dcStepOver);  // Step to fin
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept_Finally', -1);

    // NESTED
    TstName := AName + ' Step over raise nested';
    FGotExceptCount := 0;
    FContinue := True;
    StepOverToLine(TstName,'BrkStepNestedExcept_Finally_BEFORE', True);
    RunToNextPauseNoInternal(TstName, dcStepOver);
    TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept_Finally_AFTER', -1);

    StepOverToLine(TstName,'BrkStepNestedExcept_Finally_END', True);

    TstName := AName + ' Run to except';
    RunToNextPauseNoLoopBreak(TstName, dcStepOver);  // Step to next finally
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept', -1, 1);

    //StepOverToLine(TstName,'BrkStepNestedExcept_END', True);
    RunToNextPauseNoLoopBreak(TstName, dcStepOut);  // Step out

  end;

var
  ExeName, TstName, LName: String;
  TestAppRecRaise, TestAppRecStep: Integer;
begin
  ClearTestErrors;

  Src := GetCommonSourceFor(AppDir + 'ExceptTestPrg.pas');
  TestCompile(Src, ExeName);

  TestTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  dbg := Debugger.LazDebugger;
  TFpDebugDebuggerProperties(dbg.GetProperties).NextOnlyStopOnStartLine := ANextOnlyStopOnStartLine;
  try
    dbg.Exceptions.Add('MyExceptionIgnore').Enabled := False;
    dbg.OnException      := @DoDebuggerException;

    (* RecRaise
       0 : Step in => Raise nested except and debugger breaks at except, step to finally
       1 : Step in => Raise nested except but debugger continues at except, step ends in finally
       2 : Step in => do NOT raise nested except. Step through all the finally
       3 : Step over => Raise nested except but debugger continues at except => nested finally NOT paused by debugger
    *)
    for TestAppRecRaise := 0 to 3 do
    for TestAppRecStep := 0 to 4 do
    if (TestAppRecRaise = 0) or (TestAppRecStep in [0,1,4]) then
    begin
      LName := Format('[RecRaise=%d / RecStep=%d] ', [TestAppRecRaise, TestAppRecStep]);
      ExpectEnterFinally(LName, TestAppRecStep,
         False, False, False,
         TestAppRecRaise = 1, TestAppRecRaise = 2, TestAppRecRaise = 3);

      TstName := LName + ' Run to Except (Main)';
      RunToNextPauseNoInternal(TstName, dcStepOver);  // Step to next finally
      //StepIfAtLine(TstName, 'BrkStep3FinOuter_IMPLICIT'); // 32 bit
      //StepIfAtLine(TstName, 'BrkStep3FinOuter_IMPLICIT_1');
      TestLocation(TstName+': CurLine ', 'BrkStepMainExcept1', -1, 1);

      TstName := LName + ' Step to After Except (Main)';
      RunToNextPauseNoInternal(TstName, dcStepOver);
      StepOverToLine(TstName,'BrkStepMainAfterExcept1', True);
      TestLocation(TstName+': CurLine ', 'BrkStepMainAfterExcept1', -1);
    end;

    ExpectNestedExcept('Nested Except 1');

    ExpectNestedExcept_Ignore('Nested Except Ignore');

    dbg.Stop;
  finally
    Debugger.FreeDebugger;
  end;

  AssertTestErrors;
end;

procedure TTestStepping.DoTestStepTryBlocks(ANextOnlyStopOnStartLine: Boolean);
  var NextCallAtName, NextTestAtName: String;
      BrkAtCall, BrkInTest: TDBGBreakPoint;
  procedure SetNextCallTo(ABrkName: String);
  begin
    NextCallAtName := ABrkName;
    if BrkAtCall <> nil then begin
      BrkAtCall.Enabled := False;
      BrkAtCall.ReleaseReference;
    end;
    BrkAtCall := Debugger.SetBreakPoint(Src, ABrkName);
  end;
  function IsAtNextCallTo: Boolean;
  begin
    Result := IsAtLocation(NextCallAtName);
  end;
  procedure RunToNextCallTo(ATestName: String);
  begin
    if not IsAtNextCallTo then
      RunToNextPauseEx(ATestName + ' (RunToNextCallTo) ', dcRun, NextCallAtName);
  end;

  procedure SetNextBreak(ABrkName: String);
  begin
    NextTestAtName := ABrkName;
    if BrkInTest <> nil then begin
      BrkInTest.Enabled := False;
      BrkInTest.ReleaseReference;
    end;
    BrkInTest := Debugger.SetBreakPoint(Src, ABrkName);
  end;
  function IsAtNextBreak: Boolean;
  begin
    Result := IsAtLocation(NextTestAtName);
  end;
  function RunToNextBreak(ATestName: String): boolean;
  begin
    Result := IsAtNextBreak;
    if not Result then
      Result := RunToNextPauseEx(ATestName + ' (RunToNextBreakTo) ', dcRun, NextTestAtName);
  end;


  function MaybeStepMissingLine(TstName, AFirstBrkName, AFinalBrkName, IgnReason: String; AnUnwantedBrk: String = ''; ACmd1: TDBGCommand = dcStepOver; ACmd2: TDBGCommand = dcStepOver): boolean;
  begin
    FIgnoreReason := IgnReason + ' (Should go first to '+AFinalBrkName+' - But may directly go to '+AFinalBrkName+')';
    Result := RunToNextPauseNoLoopBreak(TstName+'', ACmd1, AFirstBrkName);
    FIgnoreReason := '';
    if Result then begin
      Result := RunToNextPauseNoLoopBreak(TstName+'', ACmd2, AFinalBrkName);
    end
    else begin
      if AnUnwantedBrk <> '' then
        FIgnoreReason := IgnReason + ' (Skipped desired first step to '+AFinalBrkName+' - May have hit undesired '+AnUnwantedBrk+')';
      Result := TestLocation(TstName, AFinalBrkName, -1); // went directly to brk-2
      FIgnoreReason := '';

      if (AnUnwantedBrk <> '') and (not Result) and IsAtLocation(AnUnwantedBrk) then begin
        FIgnoreReason := IgnReason + ' (Went to unwanted '+AnUnwantedBrk+' - May have skipped first hit at '+AFirstBrkName+')';
        Result := RunToNextPauseNoLoopBreak(TstName+'', ACmd2, AFirstBrkName);
        FIgnoreReason := '';
        if (result) then
          Result := RunToNextPauseNoLoopBreak(TstName+'', ACmd2, AFinalBrkName)
        else
          Result := TestLocation(TstName, AFinalBrkName, -1); // went directly to brk-2
      end;
    end;
  end;

  function MaybeStepUnWantedLine(TstName, AUnwantedBrkName, AFinalBrkName, IgnReason: String; ACmd: TDBGCommand = dcStepOver): boolean;
  begin
    FIgnoreReason := IgnReason + ' (Should go to '+AFinalBrkName+' - But may first hit '+AUnwantedBrkName+')';
    Result := RunToNextPauseNoLoopBreak(TstName+'', ACmd, AFinalBrkName);
    FIgnoreReason := '';
    if Result then
      exit;

    TestLocation(TstName, AUnwantedBrkName, -1);
    Result := RunToNextPauseNoLoopBreak(TstName+'', ACmd, AFinalBrkName); // try again
  end;


  function StepEnterTry_F(TstName, ATryBrkName, ANextBrk: String; ACmd1: TDBGCommand = dcStepOver; ACmd2: TDBGCommand = dcStepOver): boolean;
  begin
    Result := MaybeStepMissingLine(TstName, ATryBrkName, ANextBrk, '[Step-enter-try (try-finally)]', '', ACmd1, ACmd2);
// 3.2.0 and above may be fine
    //Result := RunToNextPauseNoLoopBreak(TstName+'', dcStepOver, ATryBrkName);
    //if Result then
    //Result := RunToNextPauseNoLoopBreak(TstName+'', dcStepOver, ANextBrk);
  end;

  function StepEnterFinally(TstName, AFinallyBrkName, ANextBrk, ATryBrkName: String; ACmd1: TDBGCommand = dcStepOver; ACmd2: TDBGCommand = dcStepOver): boolean;
  begin
    Result := MaybeStepMissingLine(TstName, AFinallyBrkName, ANextBrk, '[Step-enter-finally]', {Unwanted:} ATryBrkName, ACmd1, ACmd2);
  end;

  function StepLeaveFinally(TstName, ANextBrk, ATryBrkName: String; ACmd: TDBGCommand = dcStepOver): boolean;
  begin
    Result := MaybeStepUnWantedLine(TstName, ATryBrkName, ANextBrk, '[Step-leave-finally]', ACmd);
    //Result := RunToNextPauseNoLoopBreak(TstName, ACmd, ANextBrk);
  end;

  function StepEnterTry_E(TstName, ATryBrkName, ANextBrk: String; ACmd1: TDBGCommand = dcStepOver; ACmd2: TDBGCommand = dcStepOver): boolean;
  begin
    Result := MaybeStepMissingLine(TstName, ATryBrkName, ANextBrk, '[Step-enter-try (try-except)]', '', ACmd1, ACmd2);
  end;

  function StepLeaveTryNoExcept(TstName, ANextBrk, ATryBrkName: String; ACmd: TDBGCommand = dcStepOver): boolean;
  begin
    Result := MaybeStepUnWantedLine(TstName, ATryBrkName, ANextBrk, '[Step-leave-try (try-except)]', ACmd);
    //Result := RunToNextPauseNoLoopBreak(TstName, ACmd, ANextBrk);
  end;


  procedure TestSimpleStepOver(TstName: String; ACmd: TDBGCommand = dcStepOver);
  var
    IsDone: Boolean;
  begin
    IsDone := False;
    try
      FIgnoreReason := '';
      SetNextBreak('BEGIN');
      if not RunToNextBreak(TstName) then exit;

      // Enter TRY_A
      if not RunToNextPauseNoLoopBreak(TstName+'', dcStepOver, 'TRY_A_1')             then exit;
      if not StepOverToLine(TstName+'', 'TRY_A_1_BEFORE_TRY_B', False, dcStepOver, 3)  then exit;

      // Enter TRY_B
      if not StepEnterTry_F(TstName+'', 'TRY_B_1_EARLY', 'TRY_B_1', dcStepOver, ACmd) then exit;
      if not RunToNextPauseNoLoopBreak(TstName+'', dcStepOver, 'TRY_B_1_BEFORE_TRY_C')     then exit;

      // Enter TRY_C
      if not StepEnterTry_E(TstName+'', 'TRY_C_EARLY', 'TRY_C_1', dcStepOver, ACmd) then exit;
      if not StepOverToLine(TstName+'', 'TRY_C_AFTER_RAISE_1', False, dcStepOver, 4)  then exit;
      if not RunToNextPauseNoLoopBreak(TstName+'', dcStepOver, 'TRY_C_BEFORE_EXCEPT_1')     then exit;

      // Leave TRY_C => back to TRY_B
      if not StepLeaveTryNoExcept(TstName, 'TRY_B_AFTER_EXCEPT_1', 'TRY_C_EARLY') then exit;

      if not StepOverToLine(TstName+'', 'TRY_B_BEFORE_FINALLY', False, dcStepOver, 5)  then exit;
      // Enter TRY_B_FINALLY
      if not StepEnterFinally(TstName+'', 'TRY_B_FINALLY', 'TRY_B_FINALLY_1', 'TRY_B_1_EARLY', dcStepOver, ACmd) then exit;
      if not StepOverToLine(TstName+'', 'TRY_B_FINALLY_END', False, dcStepOver, 2)  then exit;

      // Leave TRY_B => back to TRY_A
      if not StepLeaveFinally(TstName, 'TRY_A_AFTER_FIN_B', 'TRY_B_1_EARLY') then exit;
      if not RunToNextPauseNoLoopBreak(TstName+'', dcStepOver, 'TRY_A_BEFORE_FINALLY')      then exit;
      // Enter finally A
      if not StepEnterFinally(TstName+'', 'TRY_A_FINALLY_EARLY', 'TRY_A_FINALLY_1', 'TRY_A_1', dcStepOver, ACmd) then exit;

      // Enter TRY_D
      if not StepOverToLine(TstName+'', 'TRY_D_1', False, dcStepOver, 3)  then exit;
      // Enter TRY_E
      if not StepOverToLine(TstName+'', 'TRY_E_1', False, dcStepOver, 4)  then exit;
      if not StepOverToLine(TstName+'', 'TRY_E_BEFORE_FINALLY', False, dcStepOver, 3)  then exit;
      if not MaybeStepUnWantedLine(TstName, 'TRY_E_1', 'TRY_E_FINALLY_1', '[step-enter-finally-nop]') then exit;

      // Enter TRY_F
      if not StepOverToLine(TstName+'', 'TRY_F_1', False, dcStepOver, 4)  then exit;
      if not StepOverToLine(TstName+'', 'TRY_F_BEFORE_FINALLY', False, dcStepOver, 8)  then exit;
      // Enter finally_F
      if not StepEnterFinally(TstName+'', 'TRY_F_FINALLY', 'TRY_F_FINALLY_1', 'TRY_F_1', dcStepOver, ACmd) then exit;
      if not StepOverToLine(TstName+'', 'TRY_F_BEFORE_TRY_G', False, dcStepOver, 6)  then exit;

      // Enter TRY_G
      if not RunToNextPauseNoLoopBreak(TstName+'', dcStepOver, 'TRY_G_1')      then exit;
      if not StepOverToLine(TstName+'', 'TRY_G_2', False, dcStepOver, 6)  then exit;
      if not StepEnterFinally(TstName+'', 'TRY_G_FINALLY', 'TRY_G_FINALLY_1', 'TRY_G_1', dcStepOver, ACmd) then exit;
      if not StepOverToLine(TstName+'', 'TRY_G_FINALLY_END', False, dcStepOver, 4)  then exit;

      // LEAVE TRY_G => back to TRY F
      if not StepLeaveFinally(TstName, 'TRY_F_FINALLY_AFTER_FIN_G', 'TRY_G_1') then exit;
      if not StepOverToLine(TstName+'', 'TRY_F_FINALLY_END', False, dcStepOver, 3)  then exit;

      // LEAVE TRY_F => back to TRY E
      if not StepLeaveFinally(TstName, 'TRY_E_FINALLY_AFTER_FIN_F', 'TRY_F_1') then exit;
      if not StepOverToLine(TstName+'', 'TRY_E_FINALLY_END', False, dcStepOver, 3)  then exit;

      // LEAVE TRY_E => back to TRY A (skip except _D)
      //if not StepLeaveFinally(TstName, 'TRY_A_FINALLY_AFTER_EXCEPT_D', 'TRY_E_1') then exit; // we can also hit the try for the EXCEPT block....
      if not StepOverToLine(TstName, 'TRY_A_FINALLY_AFTER_EXCEPT_D', False, dcStepOver, 4) then exit;
      if not StepOverToLine(TstName+'', 'TRY_A_FINALLY_END', False, dcStepOver, 3)  then exit;

      if not StepOverToLine(TstName+'', 'TRY_X_1', False, dcStepOver, 2)  then exit;
      if not StepEnterFinally(TstName+'', 'TRY_X_FINALLY', 'TRY_X_FINALLY_1', 'TRY_X_EARLY', dcStepOver, ACmd) then exit;
      if not StepOverToLine(TstName+'', 'BEFORE_END', False, dcStepOver, 4)  then exit;
      if not RunToNextPauseNoLoopBreak(TstName+'', dcStepOver, 'END')      then exit;


      IsDone := True;
    finally
      TestTrue(TstName+' - DID NOT FINISH ALL TESTS', IsDone);
    end;
  end;

  procedure TestSimpleRunAndStepOver(TstName: String; ACmd: TDBGCommand = dcStepOver);
  (* Run into finally blocks, and see that step over behaves
  *)
  var
    IsDone: Boolean;
  begin
    IsDone := False;
    try
      SetNextBreak('TRY_C_BEFORE_EXCEPT_1');
      if not RunToNextBreak(TstName) then exit;
      // Leave TRY_C => back to TRY_B
      if not StepLeaveTryNoExcept(TstName, 'TRY_B_AFTER_EXCEPT_1', 'TRY_C_EARLY') then exit;

      if not StepOverToLine(TstName+'', 'TRY_B_BEFORE_FINALLY', False, dcStepOver, 5)  then exit;
      // Enter TRY_B_FINALLY
      if not StepEnterFinally(TstName+'', 'TRY_B_FINALLY', 'TRY_B_FINALLY_1', 'TRY_B_1_EARLY', dcStepOver, ACmd) then exit;
      if not StepOverToLine(TstName+'', 'TRY_B_FINALLY_END', False, dcStepOver, 2)  then exit;


      SetNextBreak('TRY_F_BEFORE_TRY_G');
      RunToNextBreak(TstName);
      // Enter TRY_G
      if not RunToNextPauseNoLoopBreak(TstName+'', dcStepOver, 'TRY_G_1')      then exit;
      if not StepOverToLine(TstName+'', 'TRY_G_2', False, dcStepOver, 6)  then exit;
      if not StepEnterFinally(TstName+'', 'TRY_G_FINALLY', 'TRY_G_FINALLY_1', 'TRY_G_1', dcStepOver, ACmd) then exit;
      if not StepOverToLine(TstName+'', 'TRY_G_FINALLY_END', False, dcStepOver, 4)  then exit;

      // LEAVE TRY_G => back to TRY F
      if not StepLeaveFinally(TstName, 'TRY_F_FINALLY_AFTER_FIN_G', 'TRY_G_1') then exit;
      if not StepOverToLine(TstName+'', 'TRY_F_FINALLY_END', False, dcStepOver, 3)  then exit;

      // LEAVE TRY_F => back to TRY E
      if not StepLeaveFinally(TstName, 'TRY_E_FINALLY_AFTER_FIN_F', 'TRY_F_1') then exit;
      if not StepOverToLine(TstName+'', 'TRY_E_FINALLY_END', False, dcStepOver, 3)  then exit;

      // LEAVE TRY_E => back to TRY A (skip except _D)
      //if not StepLeaveFinally(TstName, 'TRY_A_FINALLY_AFTER_EXCEPT_D', 'TRY_E_1') then exit; // we can also hit the try for the EXCEPT block....
      if not StepOverToLine(TstName, 'TRY_A_FINALLY_AFTER_EXCEPT_D', False, dcStepOver, 4) then exit;
      if not StepOverToLine(TstName+'', 'TRY_A_FINALLY_END', False, dcStepOver, 3)  then exit;



      IsDone := True;
    finally
      TestTrue(TstName+' - DID NOT FINISH ALL TESTS', IsDone);
    end;
  end;

  procedure TestSimpleRunAndStepOverExit(TstName: String);
  (* Run into finally blocks, and see that step over behaves
  *)
  var
    IsDone: Boolean;
    CurTstName: String;
  begin
    if (Compiler.Version < 030000) or
       ( (Compiler.CpuBitType = cpu32) and (Compiler.Version < 030200) )
    then
      exit;

    IsDone := False;
    try
      CurTstName := TstName + ' exit 1 ';
      SetNextBreak('TRY_F_3');
      if not RunToNextBreak(CurTstName) then exit;
      if not StepOverToLine(CurTstName+'', 'TRY_F_FINALLY', False, dcStepOver, 4, 'TRY_F_BEFORE_FINALLY')  then exit;
      if not RunToNextPauseNoLoopBreak(CurTstName+'', dcStepOver, 'TRY_F_FINALLY_1')      then exit;
      if not StepOverToLine(CurTstName+'', 'TRY_F_BEFORE_TRY_G', False, dcStepOver, 5)  then exit;
      if not StepOverToLine(CurTstName+'', 'TRY_G_1', False, dcStepOver, 2)  then exit;
      if not StepOverToLine(CurTstName+'', 'TRY_G_FINALLY', False, dcStepOver, 4)  then exit;
      if not StepOverToLine(CurTstName+'', 'TRY_F_FINALLY_END', False, dcStepOver, 8)  then exit;
      if not StepOverToLine(CurTstName+'', 'BAR1_RET', False, dcStepOver, 25, 'TRY_E_FINALLY_AFTER_FIN_F_2')  then exit;


(*
      CurTstName := TstName + ' exit 2 ';
      SetNextBreak('TRY_F_FINALLY_1');
      if not RunToNextBreak(CurTstName) then exit;
      if not StepOverToLine(CurTstName+'', 'BAR1_RET', False, dcStepOver, 35, 'TRY_E_FINALLY_AFTER_FIN_F_2')  then exit;
*)

      IsDone := True;
    finally
      TestTrue(CurTstName+' - DID NOT FINISH ALL TESTS', IsDone);
    end;
  end;

  procedure TestSimpleRunAndStepOut(TstName: String);
  (* Run into finally blocks, and see that step over behaves
  *)
  var
    IsDone: Boolean;
  begin
    IsDone := False;
    try
      SetNextBreak('TRY_C_BEFORE_EXCEPT_1');
      if not RunToNextBreak(TstName) then exit;
      RunToNextPauseNoLoopBreak(TstName+'', dcStepOut); // may end at BEGIN OR END
      if not StepOverToLine(TstName+'', 'BAR2_RET', False, dcStepOver, 4, 'BEFORE_END')  then exit;
      if not StepOverToLine(TstName+'', 'BAR1_RET_FIN', False, dcStepOver, 4)  then exit;
      RunToNextPauseNoLoopBreak(TstName+'', dcStepOut); // may end at BEGIN OR END
      if not StepOverToLine(TstName+'', 'FOO_RET', False, dcStepOver, 4)  then exit;


      SetNextBreak('TRY_F_BEFORE_TRY_G');
      RunToNextBreak(TstName);
      RunToNextPauseNoLoopBreak(TstName+'', dcStepOut); // may end at BEGIN OR END
      if not StepOverToLine(TstName+'', 'BAR2_RET', False, dcStepOver, 4, 'BEFORE_END')  then exit;
      RunToNextPauseNoLoopBreak(TstName+'', dcStepOut); // may end at BEGIN OR END
      if not StepOverToLine(TstName+'', 'FOO_RET', False, dcStepOver, 4)  then exit;

      IsDone := True;
    finally
      TestTrue(TstName+' - DID NOT FINISH ALL TESTS', IsDone);
    end;
  end;


var
  ExeName: String;
begin
  ClearTestErrors;
  FGotExceptCount := 0;
  FContinue := False;
  BrkAtCall := nil;
  BrkInTest := nil;

  Src := GetCommonSourceFor(AppDir + 'StepTryBlocksPrg.pas');
  TestCompile(Src, ExeName);

  TestTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  dbg := Debugger.LazDebugger;
  TFpDebugDebuggerProperties(dbg.GetProperties).NextOnlyStopOnStartLine := ANextOnlyStopOnStartLine;
  try
    dbg.OnException      := @DoDebuggerException;

    SetNextCallTo('CALL_TestSimpleStepOver');
    RunToNextCallTo('');

    SetNextCallTo('CALL_TestSimpleRunAndStepOver'); // catch any run-away - stop here
    TestSimpleStepOver('Simple Step-Over: ');
    if BrkInTest <> nil then BrkInTest.Enabled := False;
    RunToNextCallTo('');

(* ONLY works if the compiler/RTL has no debug info / otherwise we correctly ?? step into the rtl *)
    //SetNextCallTo('CALL_a');
    //TestSimpleStepOver('Simple Step-Into: ', dcStepInto);
    //RunToNextCallTo('');

    SetNextCallTo('CALL_TestSimpleRunAndStepOverExit'); // catch any run-away - stop here
    TestSimpleRunAndStepOver('TestSimpleRunAndStepOver: ');
    if BrkInTest <> nil then BrkInTest.Enabled := False;
    RunToNextCallTo('');

    SetNextCallTo('CALL_TestSimpleRunAndStepOut'); // catch any run-away - stop here
    TestSimpleRunAndStepOverExit('TestSimpleRunAndStepOverExit');
    if BrkInTest <> nil then BrkInTest.Enabled := False;
    RunToNextCallTo('');

    SetNextCallTo('CALL_xx'); // catch any run-away - stop here
    TestSimpleRunAndStepOut('TestSimpleRunAndStepOut: ');
    if BrkInTest <> nil then BrkInTest.Enabled := False;
    RunToNextCallTo('');




    dbg.Stop;
  finally
    FIgnoreReason := '';
    Debugger.FreeDebugger;
  end;

  AssertTestErrors;
end;

function TTestStepping.RunToNextPauseNoLoopBreak(AName: String;
  ACmd: TDBGCommand; ATimeOut: Integer; AnExpAtBrkName: String;
  AnExpAcceptLinesBefore: integer): Boolean;
begin
  if AnExpAtBrkName <> '' then
    AName := AName + ' (TO: '+AnExpAtBrkName+')';
  THookedFpDebugDebugger(dbg).LockRelCount := 0;
  Result := RunToNextPauseNoInternal(AName, ACmd);

  // LockRelease called in 2 * DoState / 1 * DebugLoopFinished
  if not TestEquals(AName + ' ' + dbgs(ACmd)+' - lock cnt', 3, THookedFpDebugDebugger(dbg).LockRelCount)
  then
    Result := False;

  if AnExpAtBrkName <> '' then
    if not TestLocation(aName + ' ' + dbgs(ACmd)+': Finished at Line ', AnExpAtBrkName, -1, AnExpAcceptLinesBefore)
    then
      Result := False;
end;

function TTestStepping.RunToNextPauseNoLoopBreak(AName: String;
  ACmd: TDBGCommand; AnExpAtBrkName: String; AnExpAcceptLinesBefore: integer
  ): Boolean;
begin
  Result := RunToNextPauseNoLoopBreak(AName, ACmd, 5000, AnExpAtBrkName, AnExpAcceptLinesBefore);
end;

function TTestStepping.RunToNextPauseEx(AName: String; ACmd: TDBGCommand;
  AnExpAtBrkName: String; AnExpAcceptLinesBefore: integer): Boolean;
begin
  if AnExpAtBrkName <> '' then
    AName := AName + ' (TO: '+AnExpAtBrkName+')';
  Result := Debugger.RunToNextPause(ACmd);

  if AnExpAtBrkName <> '' then
    if not TestLocation(aName + ' ' + dbgs(ACmd)+': Finished at Line ', AnExpAtBrkName, -1, AnExpAcceptLinesBefore)
    then
      Result := False;
end;


initialization

  RegisterDbgTest(TTestStepping);

  ControlTest              := TestControlRegisterTest('TTestStepping');
  ControlTest_NextOnlyTrue := TestControlRegisterTest('TTestStepping_NextOnly=True', ControlTest);
  ControlTestStepOver      := TestControlRegisterTest('TTestStepOver', ControlTest_NextOnlyTrue);
  ControlTestStepOverInstr := TestControlRegisterTest('TTestStepOverInstr', ControlTest_NextOnlyTrue);
  ControlTestExceptionStepOutEx  := TestControlRegisterTest('TTestExceptionStepOutEx', ControlTest_NextOnlyTrue);
  ControlTestExceptionStepOverEx := TestControlRegisterTest('TTestExceptionStepOverEx', ControlTest_NextOnlyTrue);
  ControlTestStepTryBlocks := TestControlRegisterTest('TTestStepTryBlocks', ControlTest_NextOnlyTrue);

  ControlTest_NextOnly              := TestControlRegisterTest('TTestStepping_NextOnly=False', ControlTest);
  ControlTestStepOver_NextOnly      := TestControlRegisterTest('TTestStepOver_NextOnly=False', ControlTest_NextOnly);
  ControlTestStepOverInstr_NextOnly := TestControlRegisterTest('TTestStepOverInstr_NextOnly=False', ControlTest_NextOnly);
  ControlTestExceptionStepOutEx_NextOnly  := TestControlRegisterTest('TTestExceptionStepOutEx_NextOnly=False', ControlTest_NextOnly);
  ControlTestExceptionStepOverEx_NextOnly := TestControlRegisterTest('TTestExceptionStepOverEx_NextOnly=False', ControlTest_NextOnly);
  ControlTestStepTryBlocks_NextOnly := TestControlRegisterTest('TTestStepTryBlocks_NextOnly=False', ControlTest_NextOnly);

end.

