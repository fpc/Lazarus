unit TestEnvironment;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testutils, testregistry, TestGDBMIControl,
  TestBase, GDBMIDebugger, LCLProc, DbgIntfDebuggerBase, TestWatches;

const
  BREAK_LINE_ENV1 = 10;
  BREAK_LINE_ENV2 = 12;

type

  { TTestEnvironment }

  TTestEnvironment = class(TGDBTestCase)
  private
    FCurLine: Integer;
  protected
    procedure DoCurrent(Sender: TObject; const ALocation: TDBGLocationRec);
  published
    procedure TestEnv;
  end;

implementation

{ TTestEnvironment }

procedure TTestEnvironment.DoCurrent(Sender: TObject; const ALocation: TDBGLocationRec);
begin
  FCurLine := ALocation.SrcLine;
end;

procedure TTestEnvironment.TestEnv;
var
  dbg: TGDBMIDebugger;
  TestExeName, s: string;
  IgnoreRes: String;
begin
  if SkipTest then exit;
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestEnvironment')] then exit;

  ClearTestErrors;
  TestCompile(AppDir + 'EnvPrg.pas', TestExeName);

  IgnoreRes := '';
  {$IFDEF Windows}
  if (DebuggerInfo.Version > 060600) and
     (DebuggerInfo.Version < 070400)
  then
    IgnoreRes := 'broken gdb';
  {$ENDIF}

  s := 'env value 1';
  try
    dbg := StartGDB(AppDir, TestExeName);
    dbg.OnCurrent  := @DoCurrent;
    dbg.Environment.Add('ETEST1=ab123c');
    with dbg.BreakPoints.Add('EnvPrg.pas', BREAK_LINE_ENV1) do begin
      InitialEnabled := True;
      Enabled := True;
    end;
    with dbg.BreakPoints.Add('EnvPrg.pas', BREAK_LINE_ENV2) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    dbg.Run;

    TestTrue(s+' not in error state', dbg.State <> dsError, 0);
	TestTrue(s+' at break', FCurLine = BREAK_LINE_ENV1, 0, IgnoreRes);
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;

  s := 'env value 2';
  try
    dbg := StartGDB(AppDir, TestExeName);
    dbg.OnCurrent  := @DoCurrent;
    dbg.Environment.Add('ETEST1=xxx');
    with dbg.BreakPoints.Add('EnvPrg.pas', BREAK_LINE_ENV1) do begin
      InitialEnabled := True;
      Enabled := True;
    end;
    with dbg.BreakPoints.Add('EnvPrg.pas', BREAK_LINE_ENV2) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    dbg.Run;

    TestTrue(s+' not in error state', dbg.State <> dsError, 0);
	TestTrue(s+' at break', FCurLine = BREAK_LINE_ENV2, 0);
  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;


  AssertTestErrors;
end;

initialization
  RegisterDbgTest(TTestEnvironment);
  RegisterTestSelectors(['TTestEnvironment'
                        ]);

end.

