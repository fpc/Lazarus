unit TestEnvironment;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testutils, testregistry, TestBase, GDBMIDebugger, LCLProc,
  DbgIntfDebuggerBase, TestDbgControl, TestDbgTestSuites, TestDbgConfig,
  TestWatches, TestArgV;


type

  { TTestEnvironment }

  TTestEnvironment = class(TTestArgBase)
  private
    FCurLine: Integer;
  protected
    //function TestHex(const s: array of string): String; override;

    function TestSourceName: String; override;
    function TestBreakLine: integer; override;
  published
    procedure TestEnvBasic;
    procedure TestEnvBasicTab;
    procedure TestEnvBasicQuote;
    procedure TestEnvUtf1;
    procedure TestEnvUtf2;
  end;

implementation
var
  ControlTestEnvironment, ControlTestEnvironmentBasic, ControlTestEnvironmentTab,
  ControlTestEnvironmentUtf1, ControlTestEnvironmentUtf2: Pointer;

{ TTestEnvironment }

//function TTestEnvironment.TestHex(const s: array of string): String;
//var
//  w: WideString;
//  i: Integer;
//begin
//  w := '';
//  for i := 0 to Length(s) - 1 do begin
//    if w <> '' then w := w + ' ';
//    w := w + UTF8Decode(s[i]);
//  end;
//  Result := TestHex64(w);
//end;

function TTestEnvironment.TestSourceName: String;
begin
  Result := 'EnvPrg.pas';
end;

function TTestEnvironment.TestBreakLine: integer;
begin
  Result := 25;
end;

procedure TTestEnvironment.TestEnvBasic;
var
  dbg: TGDBMIDebugger;
begin
  if not StartTest(ControlTestEnvironmentUtf2, dbg) then
    exit;
  if Debugger.HasFlag('no_env') then
    FIgnoreReason := 'no_env flag';

  try
    dbg.Environment.Add('ETEST1=ab123c');
    RunAndCheckVal(dbg, 'ab123c', ['ab123c']);
  finally
    EndTest(dbg);
  end;
end;

procedure TTestEnvironment.TestEnvBasicTab;
var
  dbg: TGDBMIDebugger;
begin
  if not StartTest(ControlTestEnvironmentTab, dbg) then
    exit;
  if Debugger.HasFlag('no_env') then
    FIgnoreReason := 'no_env flag';

  try
    dbg.Environment.Add('ETEST1=a'#9'b');
    RunAndCheckVal(dbg, 'a'#9'b', ['a'#9'b']);
  finally
    EndTest(dbg);
  end;
end;

procedure TTestEnvironment.TestEnvBasicQuote;
var
  dbg: TGDBMIDebugger;
begin
  if not StartTest(ControlTestEnvironmentBasic, dbg) then
    exit;
  if Debugger.HasFlag('no_env') then
    FIgnoreReason := 'no_env flag';

  try
    dbg.Environment.Add('ETEST1=ab123c"'' \" a\b$^!)\''x');
    RunAndCheckVal(dbg, 'ab123c"'' \" a\b$^!)\''x', ['ab123c"'' \" a\b$^!)\''x']);
  finally
    EndTest(dbg);
  end;
end;

procedure TTestEnvironment.TestEnvUtf1;
var
  dbg: TGDBMIDebugger;
begin
  if not StartTest(ControlTestEnvironmentUtf1, dbg) then
    exit;
  if Debugger.HasFlag('no_env') then
    FIgnoreReason := 'no_env flag';
  if Compiler.Version < 030000 then
    FIgnoreReason := FIgnoreReason + 'fpc to old';

  try
    dbg.Environment.Add('ETEST1=aäöx');
    RunAndCheckVal(dbg, 'aäöx', ['aäöx']);
  finally
    EndTest(dbg);
  end;
end;

procedure TTestEnvironment.TestEnvUtf2;
var
  dbg: TGDBMIDebugger;
begin
  if not StartTest(ControlTestEnvironment, dbg) then
    exit;
  if Debugger.HasFlag('no_env') then
    FIgnoreReason := 'no_env flag';
  if Debugger.HasFlag('no_env_u2') then
    FIgnoreReason := 'no_env_u2 flag';
  if Compiler.Version < 030000 then
    FIgnoreReason := FIgnoreReason + 'fpc to old';

  try
    dbg.Environment.Add('ETEST1=a b c ä ö 😁 X あｓｆ');
    RunAndCheckVal(dbg, 'a b c ä ö 😁 X あｓｆ', ['a b c ä ö 😁 X あｓｆ']);
  finally
    EndTest(dbg);
  end;
end;


initialization
  RegisterDbgTest(TTestEnvironment);
  ControlTestEnvironment        := TestControlRegisterTest('TTestEnvironment');
  ControlTestEnvironmentBasic   := TestControlRegisterTest('TTestEnvironment Basic', ControlTestEnvironment);
  ControlTestEnvironmentTab     := TestControlRegisterTest('TTestEnvironment Tab', ControlTestEnvironment);
  ControlTestEnvironmentUtf1    := TestControlRegisterTest('TTestEnvironment Utf1', ControlTestEnvironment);
  ControlTestEnvironmentUtf2    := TestControlRegisterTest('TTestEnvironment Utf2', ControlTestEnvironment);

end.

