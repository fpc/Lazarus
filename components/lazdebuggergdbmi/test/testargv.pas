unit TestArgV;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testutils, testregistry, TestBase,
  GDBMIDebugger, LCLProc, FileUtil, LazUTF8, DbgIntfDebuggerBase,
  TestDbgControl, TestDbgTestSuites, TestDbgConfig, TestWatches;

const
  BREAK_LINE_ARGV = 26;

type

  { TTestEnvironment }

  { TTestArgV }

  TTestArgV = class(TGDBTestCase)
  published
    procedure TestArgv;
  end;

implementation

var
  ControlTestArgV: Pointer;

{ TTestArgV }

procedure TTestArgV.TestArgv;
var
  dbg: TGDBMIDebugger;
  TestExeName, s, s2, s3: string;
  t: TDBGType;
  i: Integer;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestArgV) then exit;

  ClearTestErrors;
  TestCompile(AppDir + 'ArgVPrg.pas', TestExeName);

  s := 'env value 1';
  try
    dbg := StartGDB(AppDir, TestExeName);
    dbg.Arguments := 'a b c ä ö ';

    with dbg.BreakPoints.Add('ArgVPrg.pas', BREAK_LINE_ARGV) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    dbg.Run;

    TestTrue(s+' not in error state', dbg.State <> dsError, 0);
    t := nil;
    TestTrue('Can eval', dbg.EvaluateWait('s', s, t));

    TestTrue('a b c in '+s, pos('61 62 63', s) > 0);

    s2 := UTF8ToSys('ä');
    s3 := ' ';
    for i := 1 to length(s2) do
      s3 := s3 + IntToHex(ord(s2[i]), 2);
    s3 := s3 + ' ';

    TestTrue(' ä in '+s, pos(s3, s) > 0);


  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;

  AssertTestErrors;
end;

initialization
  RegisterDbgTest(TTestArgV);
  ControlTestArgV         := TestControlRegisterTest('TTestArgV');

end.

