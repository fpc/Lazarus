unit TestArgV;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testutils, testregistry, TestBase, GDBMIDebugger, LCLProc,
  FileUtil, LazUTF8, DbgIntfDebuggerBase, TestDbgControl, TestDbgTestSuites,
  TestDbgConfig, TestDbgCompilerProcess, TestWatches;

const
  BREAK_LINE_ARGV = 40;

type

  { TTestArgV }

  { TTestArgBase }

  TTestArgBase = class(TGDBTestCase)
  protected
    function TestHex(const s: array of string): String; virtual;
    function TestHex64(const w: WideString): String;
    function TestSourceName: String; virtual;
    function TestBreakLine: integer; virtual;

    function StartTest(ConrolVar: Pointer; var dbg: TGDBMIDebugger; NamePostFix: String=''): Boolean;
    procedure RunAndCheckVal(dbg: TGDBMIDebugger; AName: String; const AExp: Array of string);
    procedure EndTest(dbg: TGDBMIDebugger);
  end;

  { TTestArgWideBase }

  TTestArgWinWideBase = class(TTestArgBase)
  protected
    function TestHex(const s: array of string): String; override;
  end;

  TTestArgV = class(TTestArgWinWideBase)
  published
    procedure TestArgvBasic;
    procedure TestArgvBasicTab;
    procedure TestArgvBasicQuote;
    procedure TestArgvUtf1;
    procedure TestArgvUtf2;
  end;

  { TTestExeName }

  TTestExeName = class(TTestArgWinWideBase)
  published
    procedure TestExeNameUtf1;
    procedure TestExeNameUtf2;
  end;

implementation

var
  ControlTestArgV, ControlTestArgVBasic, ControlTestArgVBasicTab, ControlTestArgVBasicQuote,
  ControlTestArgVUtf1, ControlTestArgVUtf2: Pointer;
  ControlTestExeName: Pointer;

{ TTestArgWideBase }

function TTestArgWinWideBase.TestHex(const s: array of string): String;
var
  w: WideString;
  i: Integer;
begin
  {$IFDEF WINDOWS}
  w := '';
  for i := 0 to Length(s) - 1 do begin
    if w <> '' then w := w + ' ';
    w := w + UTF8Decode(s[i]);
  end;
  Result := TestHex64(w);
  exit;
  {$ENDIF}
  Result := inherited TestHex(s);
end;

{ TTestExeName }

procedure TTestExeName.TestExeNameUtf1;
var
  dbg: TGDBMIDebugger;
begin
  if not StartTest(ControlTestExeName, dbg, 'äÖ') then
    exit;

  try
    dbg.Arguments := '';
    RunAndCheckVal(dbg, 'äÖ', ['äÖ']);
  finally
    EndTest(dbg);
  end;
end;

procedure TTestExeName.TestExeNameUtf2;
var
  dbg: TGDBMIDebugger;
begin
  if not StartTest(ControlTestExeName, dbg, 'あｓ') then
    exit;
  if Debugger.HasFlag('no_exe_u2') then
    FIgnoreReason := 'no_exe_u2 flag';

  try
    dbg.Arguments := '';
    RunAndCheckVal(dbg, 'あｓ', ['あｓ']);
  finally
    EndTest(dbg);
  end;
end;

{ TTestArgV }

function TTestArgBase.StartTest(ConrolVar: Pointer; var dbg: TGDBMIDebugger;
  NamePostFix: String): Boolean;
var
  TestExeName, s: String;
begin
  Result := False;
  if SkipTest then exit;
  if not TestControlCanTest(ConrolVar) then exit;

  Result := True;
  ClearTestErrors;
  TestCompile(AppDir + TestSourceName, TestExeName);

  if NamePostFix <> '' then begin
    s := TestExeName;
    TestExeName := UTF8StringReplace(s, 'ArgVPrg', 'ArgVPrg'+NamePostFix, []);
    RenameFile(s, TestExeName);
    CreatedExecutableList.AddExe(TestExeName, '');
  end;

  dbg := StartGDB(AppDir, TestExeName);
end;

function TTestArgBase.TestHex(const s: array of string): String;
var
  i, j: Integer;
begin
  Result := '';
  for i := 0 to length(s)-1 do begin
    for j := 1 to length(s[i]) do
      Result := Result + IntToHex(ord(s[i][j]), 2);
    Result := Result + ' ';
  end;
  delete(Result, Length(Result), 1);
end;

function TTestArgBase.TestHex64(const w: WideString): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(w) do
    Result := Result + IntToHex(ord(w[i]), 4);
end;

function TTestArgBase.TestSourceName: String;
begin
  Result := 'ArgVPrg.pas';
end;

function TTestArgBase.TestBreakLine: integer;
begin
  Result := BREAK_LINE_ARGV;
end;

procedure TTestArgBase.RunAndCheckVal(dbg: TGDBMIDebugger; AName: String;
  const AExp: array of string);
var
  s, s2: string;
  t: TDBGType;
begin
    dbg.AddTestBreakPoint(TestSourceName, TestBreakLine);
    dbg.Run;

    if not TestTrue(s+' not in error state', dbg.State <> dsError, 0) then
      exit;
    if not TestTrue(s+' not in stop state', dbg.State <> dsStop, 0) then
      exit;

    t := nil;
    TestTrue('Can eval', dbg.EvaluateWait('S', s, t, [], 15000));
    FreeAndNil(t);

    s2 := TestHex(AExp);
    TestTrue(AName + '[['+s2+']] in '+s, pos(s2, s) > 0);
end;

procedure TTestArgBase.EndTest(dbg: TGDBMIDebugger);
begin
  dbg.Done;
  CleanGdb;
  dbg.Free;

  AssertTestErrors;
end;

procedure TTestArgV.TestArgvBasic;
var
  dbg: TGDBMIDebugger;
begin
  if not StartTest(ControlTestArgVBasic, dbg) then
    exit;

  try
    dbg.Arguments := 'a b c -d=e1 ab';
    {$IFDEF WINDOWS}
    RunAndCheckVal(dbg, 'a b c -d=e1 a\b$^!)x', ['a b c -d=e1 ab']);
    {$ELSE}
    RunAndCheckVal(dbg, 'a b c -d=e1 ab', ['a', 'b', 'c', '-d=e1', 'ab']);
    {$ENDIF}
  finally
    EndTest(dbg);
  end;
end;

procedure TTestArgV.TestArgvBasicTab;
var
  dbg: TGDBMIDebugger;
begin
  if not StartTest(ControlTestArgVBasicTab, dbg) then
    exit;

  try
    dbg.Arguments := 'd=e1 "A'#9'x"';
    {$IFDEF WINDOWS}
    RunAndCheckVal(dbg, 'd=e1 "A'#9'x"', ['d=e1 "A'#9'x"']);
    {$ELSE}
    RunAndCheckVal(dbg, 'd=e1"A'#9'x"', ['d=e1', 'A'#9'x']);
    {$ENDIF}
  finally
    EndTest(dbg);
  end;
end;

procedure TTestArgV.TestArgvBasicQuote;
var
  dbg: TGDBMIDebugger;
begin
  if not StartTest(ControlTestArgVBasicQuote, dbg) then
    exit;

  try
    {$IFDEF WINDOWS}
    //dbg.Arguments := '"A "" B"';
    //RunAndCheckVal(dbg, '', ['A " B']);
    {$ENDIF}
    {$IFDEF UNIX}
    dbg.Arguments := '"A B" ''a b''';
    RunAndCheckVal(dbg, '', ['A B', 'a b']);
    {$ENDIF}
  finally
    EndTest(dbg);
  end;
end;

procedure TTestArgV.TestArgvUtf1;
var
  dbg: TGDBMIDebugger;
begin
  if not StartTest(ControlTestArgVUtf1, dbg) then
    exit;

  try
    dbg.Arguments := 'a b c ä ö ';
    RunAndCheckVal(dbg, 'a b c ä ö ', ['a', 'b', 'c', 'ä', 'ö']);
  finally
    EndTest(dbg);
  end;
end;

procedure TTestArgV.TestArgvUtf2;
var
  dbg: TGDBMIDebugger;
begin
  if not StartTest(ControlTestArgVUtf2, dbg) then
    exit;
  if Debugger.HasFlag('no_arg_u2') then
    FIgnoreReason := 'no_arg_u2 flag';

  try
    dbg.Arguments := 'a b c ä ö 😁 X あｓｆ';
    RunAndCheckVal(dbg, 'a b c ä ö 😁 X あｓｆ', ['a', 'b', 'c', 'ä', 'ö', '😁', 'X', 'あｓｆ']);
  finally
    EndTest(dbg);
  end;
end;

initialization
  RegisterDbgTest(TTestArgV);
  RegisterDbgTest(TTestExeName);
  ControlTestArgV           := TestControlRegisterTest('TTestArgV');
  ControlTestArgVBasic      := TestControlRegisterTest('ArgV Basic',   ControlTestArgV);
  ControlTestArgVBasicTab   := TestControlRegisterTest('ArgV Basic Tab',   ControlTestArgV);
  ControlTestArgVBasicQuote := TestControlRegisterTest('ArgV Basic Quote',   ControlTestArgV);
  ControlTestArgVUtf1       := TestControlRegisterTest('ArgV Utf1',    ControlTestArgV);
  ControlTestArgVUtf2       := TestControlRegisterTest('ArgV Utf2',    ControlTestArgV);
  ControlTestExeName        := TestControlRegisterTest('TTestExeName');

end.

