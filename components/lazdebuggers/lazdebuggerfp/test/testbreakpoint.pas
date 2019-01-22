unit TestBreakPoint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TestBase, TestDbgControl,
  TestDbgTestSuites, TTestDebuggerClasses, TestOutputLogger,
  TTestWatchUtilities, TestCommonSources, TestDbgConfig, DbgIntfDebuggerBase,
  DbgIntfBaseTypes, Forms;

type

  { TTestBreakPoint }

  TTestBreakPoint = class(TDBGTestCase)
  published
    (* Ensure the debugger can correctly run/step after hidding a breakpoit
       - the original instruction is executed
       - the breakpoint can be hit again
       - stepping continues correctly to next line
       - breakpoints are "hit" (recognized) when stepping onto them (not actually triggering them)
    *)
    procedure TestBreakPoints;
  end;

implementation

var
  ControlTestBreak: Pointer;

procedure TTestBreakPoint.TestBreakPoints;
var
  Src: TCommonSource;
  dbg: TDebuggerIntf;
  ExeName: String;

  procedure TestLocation(ATestName, ABrkName: String; ABreakHitCount: Integer = 1);
  var
    lc: TDBGLocationRec;
  begin
    AssertDebuggerState(dsPause);
    lc := dbg.GetLocation;
    TestEquals(ATestName+' '+ABrkName+' Loc', Src.BreakPoints[ABrkName], lc.SrcLine);
    TestEquals(ATestName+' '+ABrkName+' HitCnt', Debugger.BreakPointByName(ABrkName).HitCount, ABreakHitCount);
  end;

  procedure TestHitCnt(ATestName, ABrkName: String; ABreakHitCount: Integer);
  var
    lc: TDBGLocationRec;
  begin
    TestEquals(ATestName+' '+ABrkName+' HitCnt', Debugger.BreakPointByName(ABrkName).HitCount, ABreakHitCount);
  end;

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
    Debugger.SetBreakPoint(Src, 'PrgAfterFoo2B');
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

    // Disable breakpoint
    Debugger.BreakPointByName('Foo2').Enabled := False;

  finally
    Debugger.ClearDebuggerMonitors;
    Debugger.FreeDebugger;

    AssertTestErrors;
  end;
end;



initialization

  RegisterDbgTest(TTestBreakPoint);
  ControlTestBreak         := TestControlRegisterTest('TTestBreakPoint');
end.

