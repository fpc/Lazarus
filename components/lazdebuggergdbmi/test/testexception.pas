unit TestException;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, fpcunit, testutils, testregistry, TestBase, GDBMIDebugger,
  LCLProc, DbgIntfDebuggerBase, TestDbgControl, TestDbgTestSuites,
  TestDbgConfig, TestCommonSources, TestOutputLogger;

type

  { TTestExceptionOne }

  TTestExceptionOne = class(TGDBTestCase)
  private
    Src: TCommonSource;

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
    FInternalExceptionBrkSetMethod: TInternBrkSetMethod;

    function GetLogFileName: String; override;
    procedure DoCurrent(Sender: TObject; const ALocation: TDBGLocationRec);
    procedure TestLocation(ATestName, ABrkName: String; AnAllowLineBefore: Integer = 0; AnAltBrkName: String = '');
    function  IsAtLine(ABrkName: String; ATrueOnNoLine: Boolean = False): Boolean;
    function StepOverToLine(ATestName, ABrkName: String; AnExitIfNoLineInfo: Boolean = False): Boolean;
    function StepOverLeaveFinally(ATestName: String): Boolean;
  published
    procedure TestException;
    procedure TestExceptionStepOut;
    procedure TestExceptionStepOver;
    procedure TestExceptionStepOutEx; // Extended for SEH
    procedure TestExceptionStepOverEx; // Extended for SEH
  end;

  { TTestExceptionAddrDirect }

  TTestExceptionAddrDirect = class(TTestExceptionOne)
  public
    constructor Create; override;
  end;

  { TTestExceptionAddrInDirect }

  TTestExceptionAddrInDirect = class(TTestExceptionOne)
  public
    constructor Create; override;
  end;

  { TTestExceptionForceName }

  TTestExceptionForceName = class(TTestExceptionOne)
  public
    constructor Create; override;
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
  ControlTestExceptionOne, ControlTestExceptionOneException,
  ControlTestExceptionOneExceptionStepOut, ControlTestExceptionOneExceptionStepOver,
  ControlTestExceptionOneExceptionStepOutEx, ControlTestExceptionOneExceptionStepOverEx: Pointer;

{ TTestExceptionForceName }

constructor TTestExceptionForceName.Create;
begin
  FInternalExceptionBrkSetMethod := ibmName;
  inherited Create;
end;

{ TTestExceptionAddrInDirect }

constructor TTestExceptionAddrInDirect.Create;
begin
  FInternalExceptionBrkSetMethod := ibmAddrIndirect;
  inherited Create;
end;

{ TTestExceptionAddrDirect }

constructor TTestExceptionAddrDirect.Create;
begin
  FInternalExceptionBrkSetMethod := ibmAddrDirect;
  inherited Create;
end;


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

function TTestExceptionOne.GetLogFileName: String;
begin
  Result := inherited GetLogFileName;
  case FInternalExceptionBrkSetMethod of
    ibmAddrIndirect:  Result := StringReplace(Result, '_', '_AddrIndirect_', []);
    ibmAddrDirect:  Result := StringReplace(Result, '_', '_AddrDirect_', []);
    ibmName:  Result := StringReplace(Result, '_', '_Name_', []);
  end;
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
  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, '_raise_at', '-gt -dTEST_EXCEPTION_AT');
  FGotExceptCount := 0;
  dbg := StartGDB(AppDir, TestExeName);
  TGDBMIDebuggerPropertiesBase(dbg.GetProperties).InternalExceptionBrkSetMethod := FInternalExceptionBrkSetMethod;
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    TstName := 'All - raise';
    LogText('### '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);
    TestEquals(TstName+' Got 1 exception',   1, FGotExceptCount);
    TestEquals(TstName+' Got class',         'Exception', FGotExceptClass);
    TestEquals(TstName+' Got msg',           'foo', FGotExceptMsg, 060000);
    TestEquals(TstName+' Got location Line',  114, FGotExceptionLocation.SrcLine);
    TestMatches(TstName+' Got location File', 'ExceptPrg\.pas$', FGotExceptionLocation.SrcFile);
    TestMatches(TstName+' Got location Proc', '^\$?main$', FGotExceptionLocation.FuncName);
    TestTrue(TstName+' Got type', FGotExceptType = deInternal);

    dbg.Run;
    TstName := 'All - raise at 2 down';
    LogText('### '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);
    TestEquals(TstName+' Got exception 2', 2, FGotExceptCount);
    TestEquals(TstName+' Got class', 'Exception', FGotExceptClass);
    TestEquals(TstName+' Got msg',   'at1', FGotExceptMsg, 060000);
    TestEquals(TstName+' Got location Line',  53, FGotExceptionLocation.SrcLine);
    TestMatches(TstName+' Got location File', 'ExceptPrg\.pas$', FGotExceptionLocation.SrcFile);
    TestMatches(TstName+' Got location Proc', '^Bar2$', FGotExceptionLocation.FuncName);
    TestTrue(TstName+' Got type', FGotExceptType = deInternal);

    dbg.Run;
    TstName := 'All - raise at 1 down';
    LogText('### '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);
    TestEquals(TstName+' Got exception 3', 3, FGotExceptCount);
    TestEquals(TstName+' Got class', 'Exception', FGotExceptClass);
    TestEquals(TstName+' Got msg',   'at2', FGotExceptMsg, 060000);
    TestEquals(TstName+' Got location Line',  65, FGotExceptionLocation.SrcLine);
    TestMatches(TstName+' Got location File', 'ExceptPrg\.pas$', FGotExceptionLocation.SrcFile);
    TestMatches(TstName+' Got location Proc', '^BarBar1$', FGotExceptionLocation.FuncName);
    TestTrue(TstName+' Got type', FGotExceptType = deInternal);

    dbg.Run;
    TstName := 'All - raise subclass';
    LogText('### '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);
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
  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, '_runerr', '-gt -dTEST_SKIP_EXCEPTION_1 -dTEST_RUNERR');
  FGotExceptCount := 0;
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    LogText('### '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);
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
  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, '_assert', '-gt -dTEST_SKIP_EXCEPTION_1 -dTEST_ASSERT');
  FGotExceptCount := 0;
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    LogText('### '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);

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
              '-dTEST_NO_EXCEPTION_TYPE -gt ');
  FGotExceptCount := 0; TstName := 'no_exp_type';
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    LogText('### '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);
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
              '-dTEST_NO_EXCEPTION_TYPE -dTEST_NO_POINTER_VAR -gt ');
  FGotExceptCount := 0; TstName := 'no_exp_type_ptr';
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    LogText('### '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);
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
              '-dTEST_NO_EXCEPTION_TYPE -dTEST_NO_STRING_VAR -gt ');
  FGotExceptCount := 0; TstName := 'no_exp_type_str';
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    LogText('### '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);
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
               '-dTEST_NO_EXCEPTION_TYPE -dTEST_NO_POINTER_VAR -gt ');
  FGotExceptCount := 0; TstName := 'no_exp_type_ptr_str';
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    LogText('### '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);
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
               '-dTEST_NO_EXCEPTION_TYPE -dTEST_NO_POINTER_VAR -dTEST_NO_EXCEPTION_VAR -gt ');
  FGotExceptCount := 0; TstName := 'no_exp_type_ptr_str_var';
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    LogText('### '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);
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



  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, 'with_hplus', '-dTEST_WITH_HPLUS -gt ');
  FGotExceptCount := 0; TstName := 'with_hplus';
  dbg := StartGDB(AppDir, TestExeName);
  try
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    LogText('### '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);
    TestEquals(TstName+' Got 1 exception', 1, FGotExceptCount);
    TestEquals(TstName+' Got class', 'Exception', FGotExceptClass);
    TestEquals(TstName+' Got msg',   'foo', FGotExceptMsg, 050300);
    dbg.Run;
    TestEquals(TstName+' Got 2nd exception', 2, FGotExceptCount);
    TestEquals(TstName+' Got class', 'MyESome', FGotExceptClass);
    //TestEquals(TstName+' Got msg',   'abc üü {[''''[{ \n\t''#13#9''#', FGotExceptMsg, 050300);
    TestEquals(TstName+' Got msg',   'abc üü {[''[{ \n\t'#13#9'#', FGotExceptMsg, 050300);

    dbg.Run;
    TestEquals(TstName+' Got no more exception', 2, FGotExceptCount);

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

  Src := GetCommonSourceFor(AppDir + 'ExceptPrgStep.pas');
  TestCompile(Src, TestExeName);

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
    LogText('### 1 '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);
    TestEquals(TstName+' Got 1 exception', 1, FGotExceptCount);

    dbg.StepOver;
    TestTrue(TstName+' (Stepped) at break '+IntToStr(FCurLine),
             (FCurLine <= BREAK_LINE_EXCEPT_1) and (FCurLine >= BREAK_LINE_EXCEPT_1 - 2));
    TestEquals(TstName+' (Stepped) Still Got 1 exception', 1, FGotExceptCount);


    dbg.Run;
    LogText('### 2 '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);
    TestEquals(TstName+' Got 2 exception', 2, FGotExceptCount);

    dbg.StepOver;
    TestTrue(TstName+' (Stepped 2) at break '+IntToStr(FCurLine),
             (FCurLine <= BREAK_LINE_EXCEPT_2) and (FCurLine >= BREAK_LINE_EXCEPT_2 - 2));
    TestEquals(TstName+' (Stepped 2) Still Got 2 exception', 2, FGotExceptCount);


    dbg.Run;
    LogText('### 3 '+TstName+'   '+TestExeName);
    TestTrue(TstName+'State=Pause', dbg.State = dsPause);
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

  Src := GetCommonSourceFor(AppDir + 'ExceptPrgStepOver.pas');
  TestCompile(Src, TestExeName, '', '');

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


procedure TTestExceptionOne.TestLocation(ATestName, ABrkName: String;
  AnAllowLineBefore: Integer; AnAltBrkName: String);
var
  l, b: Integer;
  ok: Boolean;
  lc: TDBGLocationRec;
begin
  AssertDebuggerState(dsPause, ATestName);
  lc := Debugger.LazDebugger.GetLocation;
  l := lc.SrcLine;
  b := Src.BreakPoints[ABrkName];
  ATestName := ATestName + ' Loc at Line=' + IntToStr(l) + ' ' + lc.SrcFile + ' Expected='+IntToStr(b)+' '+ABrkName;
  ok := (l >= b - AnAllowLineBefore) and (l <= b);
  if (AnAltBrkName = '') or ok then begin
    TestTrue(ATestName+' '+ABrkName+' Loc', ok);
  end
  else begin
    ATestName := ATestName + ' ALTERNATE-Exp='+IntToStr(b)+ ' '+AnAltBrkName;
    b := Src.BreakPoints[AnAltBrkName];
    if b=l then
      TestTrue(ATestName+' '+ABrkName+' Loc', false, 0, ' // Ignored for alternate name');
    TestTrue(ATestName+' '+ABrkName+' Loc', b = l);
  end;
end;

function TTestExceptionOne.IsAtLine(ABrkName: String; ATrueOnNoLine: Boolean
  ): Boolean;
var
  l, b: Integer;
  lc: TDBGLocationRec;
begin
  lc := Debugger.LazDebugger.GetLocation;
  if (lc.SrcFile = '') then
    exit(ATrueOnNoLine);

  l := Debugger.LazDebugger.GetLocation.SrcLine;
  b := Src.BreakPoints[ABrkName];
  Result := b = l;
end;

function TTestExceptionOne.StepOverToLine(ATestName, ABrkName: String;
  AnExitIfNoLineInfo: Boolean): Boolean;
var
  mx: Integer;
begin
  mx := 100; // max steps
  Result := True;
  while not IsAtLine(ABrkName, AnExitIfNoLineInfo) do begin
    Debugger.LazDebugger.StepOver;
    AssertDebuggerState(dsPause, ATestName);
    dec(mx);
    if mx = 0 then begin
      TestTrue(ATestName+'reached step target '+ ABrkName, False);
      Result := False;
      break;
    end;
  end;
  debugln(['XXXXXXXXXXXXXXXXXXXXXXXX to ', ABrkName, '  ',ATestName]);
end;

function TTestExceptionOne.StepOverLeaveFinally(ATestName: String): Boolean;
var
  mx: Integer;
  lc1, lc2: TDBGLocationRec;
begin
  Result := True;
  //somehow gdb does not always a full step, but does asm stepping on the "end" line
  lc1 := Debugger.LazDebugger.GetLocation;
  Debugger.LazDebugger.StepOver;
  AssertDebuggerState(dsPause, ATestName);

  // some lines in the "end" keyword may be without line number (for gdb), and not step correctly
  lc2 := Debugger.LazDebugger.GetLocation;
  if (lc2.SrcLine > 0) or (lc1.FuncName <> lc2.FuncName) then
    exit;

  if lc1.FuncName = lc2.FuncName then TestTrue('BAD FIN STEP ' + ATestName, false, 0, ' ignore');

  mx := 5; // max steps
  while (lc1.FuncName = lc2.FuncName) and (lc2.SrcLine < 1) do begin
    Debugger.LazDebugger.StepOver;
    AssertDebuggerState(dsPause, ATestName);
    dec(mx);
    if mx = 0 then begin
      TestTrue(ATestName+'reached step target FIN', False);
      Result := False;
      break;
    end;
  lc2 := Debugger.LazDebugger.GetLocation;
  end;
  debugln(['XXXXXXXXXXXXXXXXXXXXXXXX to FIN  ',ATestName]);
end;

procedure TTestExceptionOne.TestExceptionStepOutEx;
  function StepIfAtLine(ATestName, ABrkName: String): Boolean;
  begin
    Result := True;
    if not IsAtLine(ABrkName) then
      exit;
    Debugger.LazDebugger.StepOver;
    TestTrue(ATestName+' finally entered at begin (not end) / '+ ABrkName, False, 0, 'ignore');
    AssertDebuggerState(dsPause, ATestName);
    debugln(['XXXXXXXXXXXXXXXXXXXXXXXX STEPPED from END LINE to begin??? ', ABrkName, '  ',ATestName]);
  end;

var
  ExeName, TstName: String;
  dbg: TGDBMIDebugger;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestExceptionOneExceptionStepOutEx) then exit;
  ClearTestErrors;

  Src := GetCommonSourceFor(AppDir + 'ExceptTestPrg.pas');
  TestCompile(Src, ExeName);

  dbg := StartGDB(AppDir, ExeName);
  try
    dbg.Exceptions.Add('MyExceptionIgnore').Enabled := False;
    dbg.OnException      := @DoDebuggerException;
    dbg.OnCurrent        := @DoCurrent;

    TstName := ' Run to Except';
    FContinue := False;
    dbg.Run;
    TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
    TestLocation(TstName+': CurLine ', 'BrkMyRaise');
    FContinue := True;

    dbg.StepOver;
    StepIfAtLine(TstName, 'BrkStep3Fin_A_END');
    TestLocation(TstName+': CurLine ', 'BrkStep3Fin_A', 1);

    dbg.StepOut;
    StepIfAtLine(TstName, 'BrkStep3FinOuter_A_END');
    TestLocation(TstName+': CurLine ', 'BrkStep3FinOuter_A', 1);

    dbg.StepOut;
    TestLocation(TstName+': CurLine ', 'BrkStepMainExcept1', 1 );


    dbg.Stop;
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;

  AssertTestErrors;
end;

procedure TTestExceptionOne.TestExceptionStepOverEx;
var
  dbg: TGDBMIDebugger;

  function StepIfAtLine(ATestName, ABrkName: String): Boolean;
  begin
    Result := True;
    if not IsAtLine(ABrkName) then
      exit;
    Debugger.LazDebugger.StepOver;
    TestTrue(ATestName+' finally entered at begin (not end) / '+ ABrkName, False, 0, 'ignore');
    AssertDebuggerState(dsPause, ATestName);
    debugln(['XXXXXXXXXXXXXXXXXXXXXXXX STEPPED from END LINE to begin??? ', ABrkName, '  ',ATestName]);
  end;

  procedure ExpectEnterFinally(AName: String; ATestAppRecStep: Integer;
    ATestIgnoreRaise, ATestRaiseSkipped: Boolean;
    ATestIgnoreRaise_2, ATestRaiseSkipped_2: Boolean);
  var
    TstName: String;
    MyRaiseBrk: TDBGBreakPoint;
  begin
    TstName := AName + ' Run to raise';
    FGotExceptCount := 0;
    FContinue := ATestIgnoreRaise;

    if ATestIgnoreRaise then begin
      MyRaiseBrk := Debugger.SetBreakPoint(Src, 'BrkMyRaise');
      dbg.Run;
      MyRaiseBrk.ReleaseReference;
      dbg.StepOver;  // exception will be ignored => step to finally
      TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
    end
    else begin
      dbg.Run;
      TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
      TestLocation(TstName+': CurLine ', 'BrkMyRaise');
      dbg.StepOver;
    end;

    TstName := AName + ' Run to Finally A';
    StepIfAtLine(TstName, 'BrkStep3Fin_A_END');
    TestLocation(TstName+': CurLine ', 'BrkStep3Fin_A', 1);

    if (ATestAppRecStep = 1) and (not ATestRaiseSkipped) then
      ExpectEnterFinally(TstName+' INNER ', 0, ATestIgnoreRaise_2, ATestRaiseSkipped_2, False, False)
    else
      StepOverToLine(TstName, 'BrkStep3Fin_A_END', True);

    TstName := AName + ' Run to Finally B';
    StepOverLeaveFinally(TstName);  // Step to next finally
    StepIfAtLine(TstName, 'BrkStep3Fin_B_END');
    TestLocation(TstName+': CurLine ', 'BrkStep3Fin_B', 1);

    //if (ATestAppRecStep = 2) and (not ATestRaiseSkipped) then
    //  ExpectEnterFinally(TstName+' INNER ', 0, ATestIgnoreRaise_2, ATestRaiseSkipped_2, False, False)
    //else
      StepOverToLine(TstName, 'BrkStep3Fin_B_END', True);

    TstName := AName + ' Run to Finally C';
    StepOverLeaveFinally(TstName);  // Step to next finally
    StepIfAtLine(TstName, 'BrkStep3Fin_C_END');
    TestLocation(TstName+': CurLine ', 'BrkStep3Fin_C', 1);

    if (ATestAppRecStep = 2) and (not ATestRaiseSkipped) then
      ExpectEnterFinally(TstName+' INNER ', 0, ATestIgnoreRaise_2, ATestRaiseSkipped_2, False, False)
    else
      StepOverToLine(TstName, 'BrkStep3Fin_C_END', True);

    TstName := AName + ' Run to Finally A(outer)';
    StepOverLeaveFinally(TstName);  // Step to next finally
    StepIfAtLine(TstName, 'BrkStep3FinOuter_A_END');
    TestLocation(TstName+': CurLine ', 'BrkStep3FinOuter_A', 1);

    if (ATestAppRecStep = 3) and (not ATestRaiseSkipped) then
      ExpectEnterFinally(TstName+' INNER ', 0, ATestIgnoreRaise_2, ATestRaiseSkipped_2, False, False)
    else
      StepOverToLine(TstName, 'BrkStep3FinOuter_A_END', True);

    TstName := AName + ' Run to Finally B(outer)';
    StepOverLeaveFinally(TstName);  // Step to next finally
    StepIfAtLine(TstName, 'BrkStep3FinOuter_B_END');
    TestLocation(TstName+': CurLine ', 'BrkStep3FinOuter_B', 1);

    //if (ATestAppRecStep = 5) and (not ATestRaiseSkipped) then
    //  ExpectEnterFinally(TstName+' INNER ', 0, ATestIgnoreRaise_2, ATestRaiseSkipped_2, False, False)
    //else
      StepOverToLine(TstName, 'BrkStep3FinOuter_B_END', True);

    TstName := AName + ' Run to Finally C(outer)';
    StepOverLeaveFinally(TstName);  // Step to next finally
    StepIfAtLine(TstName, 'BrkStep3FinOuter_C_END');
    TestLocation(TstName+': CurLine ', 'BrkStep3FinOuter_C', 1);

    if (ATestAppRecStep = 4) and (not ATestRaiseSkipped) then
      ExpectEnterFinally(TstName+' INNER ', 0, ATestIgnoreRaise_2, ATestRaiseSkipped_2, False, False)
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
    dbg.Run;
    TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
    TestLocation(TstName+': CurLine ', 'BrkMyRaise');

    TstName := AName + ' Run to except fin';
    dbg.StepOver;  // Step to fin
    StepIfAtLine(TstName, 'BrkStepNestedExcept_Finally_END');
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept_Finally', 1);

    // NESTED
    TstName := AName + ' Run to raise nested';
    FGotExceptCount := 0;
    FContinue := False;
    dbg.Run;
    TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
    TestLocation(TstName+': CurLine ', 'BrkMyRaise');

    TstName := AName + ' Run to except fin nested';
    dbg.StepOver;  // Step to fin
    StepIfAtLine(TstName, 'BrkStepNestedExcept_Finally_END');
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept_Finally', 1);

    StepOverToLine(TstName,'BrkStepNestedExcept_Finally_END', True);

    TstName := AName + ' Run to except nested';
    StepOverLeaveFinally(TstName);
    StepIfAtLine(TstName, 'BrkStepNestedExcept_END');
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept', 1);

    StepOverToLine(TstName,'BrkStepNestedExcept_END', True);
    // END NESTED

    TstName := AName + ' Run back except fin';
    dbg.StepOver;  // Step back to end
    dbg.StepOver;  // Step back to finaly
    if not IsAtLine('BrkStepNestedExcept_Finally_AFTER') then dbg.StepOver;  // sometimes need extra steps at "end"
    if not IsAtLine('BrkStepNestedExcept_Finally_AFTER') then dbg.StepOver;  // sometimes need extra steps at "end"
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept_Finally_AFTER', 1);

    StepOverToLine(TstName,'BrkStepNestedExcept_Finally_END', True);

    TstName := AName + ' Run to except';
    StepOverLeaveFinally(TstName);
    StepIfAtLine(TstName, 'BrkStepNestedExcept_END');
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept', 1);

    //StepOverToLine(TstName,'BrkStepNestedExcept_END', True);
    dbg.StepOut;  // Step out
  end;

  procedure ExpectNestedExcept_Ignore(AName: String);
  var
    TstName: String;
  begin
    TstName := AName + ' Run to raise';
    FGotExceptCount := 0;
    FContinue := False;
    dbg.Run;
    TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
    TestLocation(TstName+': CurLine ', 'BrkMyRaise');

    TstName := AName + ' Run to except fin';
    dbg.StepOver;  // Step to fin
    StepIfAtLine(TstName, 'BrkStepNestedExcept_Finally_END');
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept_Finally', 1);

    // NESTED
    TstName := AName + ' Step over raise nested';
    FGotExceptCount := 0;
    FContinue := True;
    StepOverToLine(TstName,'BrkStepNestedExcept_Finally_BEFORE', True);
    dbg.StepOver;
    TestEquals(TstName+': Got 1 exceptions: ', 1, FGotExceptCount);
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept_Finally_AFTER', 1);

    StepOverToLine(TstName,'BrkStepNestedExcept_Finally_END', True);

    TstName := AName + ' Run to except';
    StepOverLeaveFinally(TstName);
    StepIfAtLine(TstName, 'BrkStepNestedExcept_END');
    TestLocation(TstName+': CurLine ', 'BrkStepNestedExcept', 1);

    //StepOverToLine(TstName,'BrkStepNestedExcept_END', True);
    dbg.StepOut;  // Step out

  end;

var
  ExeName, TstName, LName: String;
  TestAppRecRaise, TestAppRecStep: Integer;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestExceptionOneExceptionStepOverEx) then exit;
  ClearTestErrors;

  Src := GetCommonSourceFor(AppDir + 'ExceptTestPrg.pas');
  TestCompile(Src, ExeName);

  dbg := StartGDB(AppDir, ExeName);
  try
    dbg.Exceptions.Add('MyExceptionIgnore').Enabled := False;
    dbg.OnException      := @DoDebuggerException;
    dbg.OnCurrent        := @DoCurrent;

    for TestAppRecRaise := 0 to 2 do
    for TestAppRecStep := 0 to 4 do
    if (TestAppRecRaise = 0) or (TestAppRecStep in [0,1,4]) then
    begin
      LName := Format('[RecRaise=%d / RecStep=%d] ', [TestAppRecRaise, TestAppRecStep]);
      ExpectEnterFinally(LName, TestAppRecStep,
         False, False,
         TestAppRecRaise = 1, TestAppRecRaise = 2);

      TstName := LName + ' Run to Except (Main)';
      StepOverLeaveFinally(TstName);
      TestLocation(TstName+': CurLine ', 'BrkStepMainExcept1', 1);

      TstName := LName + ' Step to After Except (Main)';
      dbg.StepOver;
      StepOverToLine(TstName,'BrkStepMainAfterExcept1', True);
      TestLocation(TstName+': CurLine ', 'BrkStepMainAfterExcept1', 1);
    end;

    ExpectNestedExcept('Nested Except 1');

    ExpectNestedExcept_Ignore('Nested Except Ignore');

    dbg.Stop;
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;

  AssertTestErrors;
end;

initialization
  RegisterDbgTest(TTestExceptionAddrDirect);
  RegisterDbgTest(TTestExceptionAddrInDirect);
  RegisterDbgTest(TTestExceptionForceName);

  ControlTestExceptionOne                  := TestControlRegisterTest('TTestExceptionOne');
  ControlTestExceptionOneException         := TestControlRegisterTest('Exception', ControlTestExceptionOne);
  ControlTestExceptionOneExceptionStepOut  := TestControlRegisterTest('ExceptionStepOut', ControlTestExceptionOne);
  ControlTestExceptionOneExceptionStepOver := TestControlRegisterTest('ExceptionStepOver', ControlTestExceptionOne);
  ControlTestExceptionOneExceptionStepOutEx  := TestControlRegisterTest('ExceptionStepOutEx', ControlTestExceptionOne);
  ControlTestExceptionOneExceptionStepOverEx := TestControlRegisterTest('ExceptionStepOverEx', ControlTestExceptionOne);
end.

