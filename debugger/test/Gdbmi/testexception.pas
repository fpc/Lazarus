unit TestException;

{$mode objfpc}{$H+}

interface

uses
  Classes, fpcunit, testutils, testregistry, TestGDBMIControl,
  TestBase, Debugger, GDBMIDebugger, LCLProc;

type

  { TTestExceptionOne }

  TTestExceptionOne = class(TGDBTestCase)
  private
    FGotExceptCount: Integer;
    FGotExceptClass: String;
    FGotExceptMsg: String;
    FGotExceptType: TDBGExceptionType;

    procedure DoDebuggerException(Sender: TObject;
                                  const AExceptionType: TDBGExceptionType;
                                  const AExceptionClass: String;
                                  const AExceptionLocation: TDBGLocationRec;
                                  const AExceptionText: String;
                                  out AContinue: Boolean);
  published
    procedure TestException;
  end; 




implementation

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
  AContinue := False;
end;

procedure TTestExceptionOne.TestException;
var
  TestExeName, TstName: string;
  dbg: TGDBMIDebugger;
begin
  if SkipTest then exit;
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestExceptionOne')] then exit;
  ClearTestErrors;

  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, '', '-gt -gh');
  try
    FGotExceptCount := 0; TstName := 'all';
    dbg := StartGDB(AppDir, TestExeName);
    dbg.OnException      := @DoDebuggerException;

    dbg.Run;
    TestEquals(TstName+' Got 1 exception', 1, FGotExceptCount);
    TestEquals(TstName+' Got class', 'Exception', FGotExceptClass);
    TestEquals(TstName+' Got msg',   'foo', FGotExceptMsg, 060000);
    dbg.Run;
    TestEquals(TstName+' Got 2nd exception', 2, FGotExceptCount);
    dbg.Run;
    TestEquals(TstName+' Got no more exception', 2, FGotExceptCount);
    TestEquals(TstName+' Got class', 'MyESome', FGotExceptClass);
    TestEquals(TstName+' Got msg',   'abc', FGotExceptMsg, 050300);
    dbg.Stop;
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;

  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName, 'no_etype',
              '-dTEST_NO_EXCEPTION_TYPE -gt -gh');
  try
    FGotExceptCount := 0; TstName := 'no_exp_type';
    dbg := StartGDB(AppDir, TestExeName);
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
  try
    FGotExceptCount := 0; TstName := 'no_exp_type_ptr';
    dbg := StartGDB(AppDir, TestExeName);
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
  try
    FGotExceptCount := 0; TstName := 'no_exp_type_str';
    dbg := StartGDB(AppDir, TestExeName);
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
  try
    FGotExceptCount := 0; TstName := 'no_exp_type_ptr_str';
    dbg := StartGDB(AppDir, TestExeName);
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
  try
    FGotExceptCount := 0; TstName := 'no_exp_type_ptr_str_var';
    dbg := StartGDB(AppDir, TestExeName);
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
  try
    FGotExceptCount := 0; TstName := 'with_hplus';
    dbg := StartGDB(AppDir, TestExeName);
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
    TestEquals(TstName+' Got msg',   'abc', FGotExceptMsg, 050300);
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

end.

