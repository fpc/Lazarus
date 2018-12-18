unit TestException;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, fpcunit, testutils, testregistry, TestBase, GDBMIDebugger,
  LCLProc, DbgIntfDebuggerBase, TestDbgControl, TestDbgTestSuites;

type

  { TTestExceptionOne }

  TTestExceptionOne = class(TGDBTestCase)
  private
    FCurLine: Integer;
    FCurFile: string;

    FGotExceptCount: Integer;
    FGotExceptClass: String;
    FGotExceptMsg: String;
    FGotExceptType: TDBGExceptionType;
    FGotExceptionLocation: TDBGLocationRec;
    FContinue: Boolean;

    procedure DoDebuggerException(Sender: TObject;
                                  const AExceptionType: TDBGExceptionType;
                                  const AExceptionClass: String;
                                  const AExceptionLocation: TDBGLocationRec;
                                  const AExceptionText: String;
                                  out AContinue: Boolean);
  protected
    procedure DoCurrent(Sender: TObject; const ALocation: TDBGLocationRec);
  published
    procedure TestException;
    procedure TestExceptionStepOut;
    procedure TestExceptionStepOver;
  end;


const
  (* Stepping out of the exception may currently stop one line before the "except statemet.
     The lines below are the first line in the statement. (so 2 later)
  *)
  BREAK_LINE_EXCEPT_1 = 20;  // first except blog // may be 18 = at "except" keyword
  BREAK_LINE_EXCEPT_2 = 31;  // 2nd except
  BREAK_LINE_EXCEPT_3 = 45;  // 3rd except not handled
  BREAK_LINE_EXCEPT_4 = 50;  // 3rd except
  BREAK_LINE_EXCEPT_END = 54; // line for break at end

  STEP_OVER_RAISE_1 = 67;  // first except / step over test
  STEP_OVER_CATCH_1 = 68;  // first except / step over test
  STEP_OVER_RAISE_2 = 70;  // first except / step over test
  STEP_OVER_CATCH_2 = 71;  // first except / step over test
  STEP_OVER_RAISE_3 = 49;  // first except / step over test
  STEP_OVER_CATCH_3 = 53;  // first except / step over test
  STEP_OVER_RAISE_4 = 60;  // first except / step over test
  STEP_OVER_CATCH_4 = 81;  // first except / step over test


implementation
var
  ControlTestExceptionOne, ControlTestExceptionOneException, ControlTestExceptionOneExceptionStepOut, ControlTestExceptionOneExceptionStepOver: Pointer;


    //dbg.OnBreakPointHit := @DebuggerBreakPointHit;
    //dbg.OnState         := @DebuggerChangeState;
    //dbg.OnCurrent       := @DebuggerCurrentLine;
    //dbg.OnDbgOutput     := @DebuggerOutput;
    //dbg.OnDbgEvent      := @DebuggerEvent;

procedure TTestExceptionOne.DoDebuggerException(Sender: TObject;
  const AExceptionType: TDBGExceptionType; const AExceptionClass: String;
  const AExceptionLocation: TDBGLocationRec; const AExceptionText: String;
  out AContinue: Boolean);
begin
  inc(FGotExceptCount);
  FGotExceptClass := AExceptionClass;
  FGotExceptMsg   := AExceptionText;
  FGotExceptType  := AExceptionType;
  FGotExceptionLocation := AExceptionLocation;
  AContinue := FContinue;
end;

procedure TTestExceptionOne.DoCurrent(Sender: TObject; const ALocation: TDBGLocationRec);
begin
  FCurFile := ALocation.SrcFile;
  FCurLine := ALocation.SrcLine;
end;

procedure TTestExceptionOne.TestException;
var
  TestExeName, TstName: string;
  dbg: TGDBMIDebugger;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestExceptionOneException) then exit;
  ClearTestErrors;
  FContinue := False;

  TstName := 'All';
  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, '_raise_at', '-gt -gh -dTEST_EXCEPTION_AT');
  FGotExceptCount := 0;
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    TstName := 'All - raise';
    TestEquals(TstName+' Got 1 exception',   1, FGotExceptCount);
    TestEquals(TstName+' Got class',         'Exception', FGotExceptClass);
    TestEquals(TstName+' Got msg',           'foo', FGotExceptMsg, 060000);
    TestEquals(TstName+' Got location Line',  113, FGotExceptionLocation.SrcLine);
    TestMatches(TstName+' Got location File', 'ExceptPrg\.pas$', FGotExceptionLocation.SrcFile);
    TestMatches(TstName+' Got location Proc', '^main$', FGotExceptionLocation.FuncName);
    TestTrue(TstName+' Got type', FGotExceptType = deInternal);

    dbg.Run;
    TstName := 'All - raise at 2 down';
    TestEquals(TstName+' Got exception 2', 2, FGotExceptCount);
    TestEquals(TstName+' Got class', 'Exception', FGotExceptClass);
    TestEquals(TstName+' Got msg',   'at1', FGotExceptMsg, 060000);
    TestEquals(TstName+' Got location Line',  53, FGotExceptionLocation.SrcLine);
    TestMatches(TstName+' Got location File', 'ExceptPrg\.pas$', FGotExceptionLocation.SrcFile);
    TestMatches(TstName+' Got location Proc', '^Bar2$', FGotExceptionLocation.FuncName);
    TestTrue(TstName+' Got type', FGotExceptType = deInternal);

    dbg.Run;
    TstName := 'All - raise at 1 down';
    TestEquals(TstName+' Got exception 3', 3, FGotExceptCount);
    TestEquals(TstName+' Got class', 'Exception', FGotExceptClass);
    TestEquals(TstName+' Got msg',   'at2', FGotExceptMsg, 060000);
    TestEquals(TstName+' Got location Line',  65, FGotExceptionLocation.SrcLine);
    TestMatches(TstName+' Got location File', 'ExceptPrg\.pas$', FGotExceptionLocation.SrcFile);
    TestMatches(TstName+' Got location Proc', '^BarBar1$', FGotExceptionLocation.FuncName);
    TestTrue(TstName+' Got type', FGotExceptType = deInternal);

    dbg.Run;
    TstName := 'All - raise subclass';
    TestEquals(TstName+'Got exception 4', 4, FGotExceptCount);
    TestEquals(TstName+' Got class', 'MyESome', FGotExceptClass);
    // not yet MakePrintable
    //TestEquals(TstName+' Got msg',   'abc üü {[''''[{ \n\t''#13#9''#', FGotExceptMsg, 050300);
    TestEquals(TstName+' Got msg',   'abc üü {[''[{ \n\t'#13#9'#', FGotExceptMsg, 050300);
    TestEquals(TstName+' Got location Line',  34, FGotExceptionLocation.SrcLine);
    TestMatches(TstName+' Got location File', 'ExceptPrg\.pas$', FGotExceptionLocation.SrcFile);
    TestMatches(TstName+' Got location Proc', '^foo$', FGotExceptionLocation.FuncName);
    TestTrue(TstName+' Got type', FGotExceptType = deInternal);

    dbg.Run;
    TestEquals(TstName+' Got no more exception', 4, FGotExceptCount);
    dbg.Stop;
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;

  TstName := 'RunError';
  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, '_runerr', '-gt -gh -dTEST_SKIP_EXCEPTION_1 -dTEST_RUNERR');
  FGotExceptCount := 0;
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    TestEquals(TstName+' Got run err', 1, FGotExceptCount);
    TestMatches(TstName+' Got class', 'RunError', FGotExceptClass);
    //TestEquals(TstName+' Got msg',   'at2', FGotExceptMsg, 060000);
    TestEquals(TstName+' Got location Line',  81, FGotExceptionLocation.SrcLine);
    TestMatches(TstName+' Got location File', 'ExceptPrg\.pas$', FGotExceptionLocation.SrcFile);
    TestMatches(TstName+' Got location Proc', '^Run$', FGotExceptionLocation.FuncName);
    TestTrue(TstName+' Got type', FGotExceptType = deRunError);

    dbg.Stop;
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;

  TstName := 'Assert';
  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, '_assert', '-gt -gh -dTEST_SKIP_EXCEPTION_1 -dTEST_ASSERT');
  FGotExceptCount := 0;
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    TestEquals(TstName+' Got Assert', 1, FGotExceptCount);
    TestMatches(TstName+' Got class', 'EAssertionFailed', FGotExceptClass);
    TestMatches(TstName+' Got msg',   'denied', FGotExceptMsg, 060000);
//    TestEquals(TstName+' Got location Line',  94, FGotExceptionLocation.SrcLine);
    TestMatches(TstName+' Got location File', 'ExceptPrg\.pas$', FGotExceptionLocation.SrcFile);
//    TestMatches(TstName+' Got location Proc', '^check$', FGotExceptionLocation.FuncName);
    TestTrue(TstName+' Got type', FGotExceptType = deInternal);

    dbg.Stop;
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;

  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, 'no_etype',
              '-dTEST_NO_EXCEPTION_TYPE -gt -gh');
  FGotExceptCount := 0; TstName := 'no_exp_type';
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    TestEquals(TstName+' Got 1 exception', 1, FGotExceptCount);
    TestEquals(TstName+' Got class', 'Exception', FGotExceptClass);
    TestEquals(TstName+' Got msg',   'foo', FGotExceptMsg, 050300);
    dbg.Run;
    TestEquals(TstName+' Got no more exception', 1, FGotExceptCount);
    dbg.Stop;
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;

  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, 'no_etype_ptr',
              '-dTEST_NO_EXCEPTION_TYPE -dTEST_NO_POINTER_VAR -gt -gh');
  FGotExceptCount := 0; TstName := 'no_exp_type_ptr';
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    TestEquals(TstName+' Got 1 exception', 1, FGotExceptCount);
    TestEquals(TstName+' Got class', 'Exception', FGotExceptClass);
    TestEquals(TstName+' Got msg',   'foo', FGotExceptMsg, 050300);
    dbg.Run;
    TestEquals(TstName+' Got no more exception', 1, FGotExceptCount);
    dbg.Stop;
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;

  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, 'no_etype_str',
              '-dTEST_NO_EXCEPTION_TYPE -dTEST_NO_STRING_VAR -gt -gh');
  FGotExceptCount := 0; TstName := 'no_exp_type_str';
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    TestEquals(TstName+' Got 1 exception', 1, FGotExceptCount);
    TestEquals(TstName+' Got class', 'Exception', FGotExceptClass);
    TestEquals(TstName+' Got msg',   'foo', FGotExceptMsg, 050300);
    dbg.Run;
    TestEquals(TstName+' Got no more exception', 1, FGotExceptCount);
    dbg.Stop;
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;

  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, 'no_etype_ptr_str',
               '-dTEST_NO_EXCEPTION_TYPE -dTEST_NO_POINTER_VAR -gt -gh');
  FGotExceptCount := 0; TstName := 'no_exp_type_ptr_str';
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    TestEquals(TstName+' Got 1 exception', 1, FGotExceptCount);
    TestEquals(TstName+' Got class', 'Exception', FGotExceptClass);
    TestEquals(TstName+' Got msg',   'foo', FGotExceptMsg, 050300);
    dbg.Run;
    TestEquals(TstName+' Got no more exception', 1, FGotExceptCount);
    dbg.Stop;
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;

  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, 'no_etype_ptr_str_var',
               '-dTEST_NO_EXCEPTION_TYPE -dTEST_NO_POINTER_VAR -dTEST_NO_EXCEPTION_VAR -gt -gh');
  FGotExceptCount := 0; TstName := 'no_exp_type_ptr_str_var';
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    TestEquals(TstName+' Got 1 exception', 1, FGotExceptCount);
    TestEquals(TstName+' Got class', 'Exception', FGotExceptClass);
    TestEquals(TstName+' Got msg',   'foo', FGotExceptMsg, 050300);
    dbg.Run;
    TestEquals(TstName+' Got no more exception', 1, FGotExceptCount);
    dbg.Stop;
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;



  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, 'with_hplus', '-dTEST_WITH_HPLUS -gt -gh');
  FGotExceptCount := 0; TstName := 'with_hplus';
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    TestEquals(TstName+' Got 1 exception', 1, FGotExceptCount);
    TestEquals(TstName+' Got class', 'Exception', FGotExceptClass);
    TestEquals(TstName+' Got msg',   'foo', FGotExceptMsg, 050300);
    dbg.Run;
    TestEquals(TstName+' Got 2nd exception', 2, FGotExceptCount);
    dbg.Run;
    TestEquals(TstName+' Got no more exception', 2, FGotExceptCount);
    TestEquals(TstName+' Got class', 'MyESome', FGotExceptClass);
    //TestEquals(TstName+' Got msg',   'abc üü {[''''[{ \n\t''#13#9''#', FGotExceptMsg, 050300);
    TestEquals(TstName+' Got msg',   'abc üü {[''[{ \n\t'#13#9'#', FGotExceptMsg, 050300);

    dbg.Stop;
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;



  AssertTestErrors;
end;

procedure TTestExceptionOne.TestExceptionStepOut;
var
  TestExeName, TstName: string;
  dbg: TGDBMIDebugger;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestExceptionOneExceptionStepOut) then exit;
  ClearTestErrors;
  FContinue := False;

  TestCompile(AppDir + 'ExceptPrgStep.pas', TestExeName, '', '');
  FGotExceptCount := 0; TstName := 'STEP';
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;
    dbg.OnCurrent        := @DoCurrent;

    with dbg.BreakPoints.Add('ExceptPrgStep.pas', BREAK_LINE_EXCEPT_END) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    dbg.Run;
    TestEquals(TstName+' Got 1 exception', 1, FGotExceptCount);

    dbg.StepOver;
    TestTrue(TstName+' (Stepped) at break '+IntToStr(FCurLine),
             (FCurLine <= BREAK_LINE_EXCEPT_1) and (FCurLine >= BREAK_LINE_EXCEPT_1 - 2));
    TestEquals(TstName+' (Stepped) Still Got 1 exception', 1, FGotExceptCount);


    dbg.Run;
    TestEquals(TstName+' Got 2 exception', 2, FGotExceptCount);

    dbg.StepOver;
    TestTrue(TstName+' (Stepped 2) at break '+IntToStr(FCurLine),
             (FCurLine <= BREAK_LINE_EXCEPT_2) and (FCurLine >= BREAK_LINE_EXCEPT_2 - 2));
    TestEquals(TstName+' (Stepped 2) Still Got 2 exception', 2, FGotExceptCount);


    dbg.Run;
    TestEquals(TstName+' Got 3 exception', 3, FGotExceptCount);

    dbg.StepOver;
    TestTrue(TstName+' (Stepped 2) at break '+IntToStr(FCurLine),
             (FCurLine <= BREAK_LINE_EXCEPT_3) and (FCurLine >= BREAK_LINE_EXCEPT_3 - 2));
    TestEquals(TstName+' (Stepped 3) Still Got 3 exception', 3, FGotExceptCount);

    dbg.StepOver;
    TestTrue(TstName+' (Stepped 4) at break '+IntToStr(FCurLine),
             (FCurLine <= BREAK_LINE_EXCEPT_4) and (FCurLine >= BREAK_LINE_EXCEPT_4 - 2));
    TestEquals(TstName+' (Stepped 4) Still Got 3 exception', 3, FGotExceptCount);


    dbg.Run; // run to break (tmp break cleared)
    TestEquals(TstName+' at break', BREAK_LINE_EXCEPT_END, FCurLine);
    TestEquals(TstName+' Still Got 3 exception', 3, FGotExceptCount);

    dbg.Stop;
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;

  AssertTestErrors;
end;

procedure TTestExceptionOne.TestExceptionStepOver;
var
  TestExeName, TstName: string;
  dbg: TGDBMIDebugger;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestExceptionOneExceptionStepOver) then exit;
  ClearTestErrors;
  FContinue := True;

  TestCompile(AppDir + 'ExceptPrgStepOver.pas', TestExeName, '', '');
  FGotExceptCount := 0; TstName := 'STEPOVER ';
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;
    dbg.OnCurrent        := @DoCurrent;

    with dbg.BreakPoints.Add('ExceptPrgStepOver.pas', STEP_OVER_RAISE_1) do begin
      InitialEnabled := True;
      Enabled := True;
    end;
    with dbg.BreakPoints.Add('ExceptPrgStepOver.pas', STEP_OVER_RAISE_2) do begin
      InitialEnabled := True;
      Enabled := True;
    end;
    with dbg.BreakPoints.Add('ExceptPrgStepOver.pas', STEP_OVER_RAISE_3) do begin
      InitialEnabled := True;
      Enabled := True;
    end;
    with dbg.BreakPoints.Add('ExceptPrgStepOver.pas', STEP_OVER_RAISE_4) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    dbg.Run;
    TestEquals(TstName+' Before 1: Got 0 exceptions: ', 0, FGotExceptCount);
    TestEquals(TstName+' Before 1: CurLine', STEP_OVER_RAISE_1, FCurLine);

    dbg.StepOver;
    TestTrue(TstName+' (Stepped 1) at break '+IntToStr(FCurLine),
             (FCurLine <= STEP_OVER_CATCH_1) and (FCurLine >= STEP_OVER_CATCH_1 - 2));
    TestEquals(TstName+' (Stepped 1) Got 1 exception', 1, FGotExceptCount);



    dbg.Run;
    TestEquals(TstName+' Before 2: Got 1 exceptions: ', 1, FGotExceptCount);
    TestEquals(TstName+' Before 2: CurLine', STEP_OVER_RAISE_2, FCurLine);

    dbg.StepOver;
    TestTrue(TstName+' (Stepped 2) at break '+IntToStr(FCurLine),
             (FCurLine <= STEP_OVER_CATCH_2) and (FCurLine >= STEP_OVER_CATCH_2 - 2));
    TestEquals(TstName+' (Stepped 2) Got 2 exception', 2, FGotExceptCount);



    dbg.Run;
    TestEquals(TstName+' Before 3: Got 2 exceptions: ', 2, FGotExceptCount);
    TestEquals(TstName+' Before 3: CurLine', STEP_OVER_RAISE_3, FCurLine);

    dbg.StepOver;
    TestTrue(TstName+' (Stepped 3) at break '+IntToStr(FCurLine),
             (FCurLine <= STEP_OVER_CATCH_3) and (FCurLine >= STEP_OVER_CATCH_3 - 2));
    TestEquals(TstName+' (Stepped 3) Got 3 exception', 3, FGotExceptCount);



    dbg.Run;
    TestEquals(TstName+' Before 4: Got 3 exceptions: ', 3, FGotExceptCount);
    TestEquals(TstName+' Before 4: CurLine', STEP_OVER_RAISE_4, FCurLine);

    dbg.StepOver;
    TestTrue(TstName+' (Stepped 4) at break '+IntToStr(FCurLine),
             (FCurLine <= STEP_OVER_CATCH_4) and (FCurLine >= STEP_OVER_CATCH_4 - 2));
    TestEquals(TstName+' (Stepped 4) Got 4 exception', 4, FGotExceptCount);



    dbg.Stop;
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;

  AssertTestErrors;
end;

initialization
  RegisterDbgTest(TTestExceptionOne);
  ControlTestExceptionOne                  := TestControlRegisterTest('TTestExceptionOne');
  ControlTestExceptionOneException         := TestControlRegisterTest('Exception', ControlTestExceptionOne);
  ControlTestExceptionOneExceptionStepOut  := TestControlRegisterTest('ExceptionStepOut', ControlTestExceptionOne);
  ControlTestExceptionOneExceptionStepOver := TestControlRegisterTest('ExceptionStepOver', ControlTestExceptionOne);
end.

