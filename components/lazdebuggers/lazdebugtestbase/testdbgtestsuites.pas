unit TestDbgTestSuites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TTestDbgExecuteables, TestDbgControl, TestDbgConfig,
  TestOutputLogger, LazFileUtils, LazLogger, fpcunit, testregistry, RegExpr;

const
  EqIgnoreCase = False; // for TestEquals(..., CaseSense, ...);
  EqMatchCase = True;

type
  TDBGTestsuite = class;

  { TDBGTestCase }

  TDBGTestCase = class(TTestCase)
  private
    FParent: TDBGTestsuite;
    // TestResults
    FTestBaseName: String;
    FTestErrors, FIgnoredErrors, FUnexpectedSuccess: String;
    FTestCnt, FTestErrorCnt, FIgnoredErrorCnt, FUnexpectedSuccessCnt, FSucessCnt: Integer;
    FTotalErrorCnt, FTotalIgnoredErrorCnt, FTotalUnexpectedSuccessCnt: Integer;
    FRegX: TRegExpr;

    // Logging
    FLogFile: TextFile;
    FLogFileCreated: Boolean;
    FLogFileName, FLogBufferText: String;
    procedure InitLog;
    procedure FinishLog;

    function GetCompiler: TTestDbgCompiler;
    function GetDebugger: TTestDbgDebugger;
  protected
    // TestResults
    procedure AddTestError  (s: string; MinDbgVers: Integer = 0; AIgnoreReason: String = '');
    procedure AddTestError  (s: string; MinDbgVers: Integer; MinFpcVers: Integer;AIgnoreReason: String = '');
    procedure AddTestSuccess(s: string; MinDbgVers: Integer = 0; AIgnoreReason: String = '');
    procedure AddTestSuccess(s: string; MinDbgVers: Integer; MinFpcVers: Integer;AIgnoreReason: String = '');
    procedure ClearTestErrors;
    procedure AssertTestErrors;
    property TestErrors: string read FTestErrors;

    // Logging
    function GetLogActive: Boolean;
    function GetLogFileName: String; virtual;
    function GetFinalLogFileName: String; virtual;
    procedure CreateLog;
    // Debugln
    procedure DoDbgOut(Sender: TObject; S: string; var Handled: Boolean); virtual;
    procedure DoDebugln(Sender: TObject; S: string; var Handled: Boolean); virtual;

    procedure SetUp; override;
    procedure TearDown; override;
    procedure RunTest; override;
  public
    function SkipTest: Boolean; virtual;
    Procedure TestCompile(const PrgName: string; out ExeName: string; NamePostFix: String=''; ExtraArgs: String=''); overload;
    Procedure TestCompile(const PrgName: string; out ExeName: string; UsesDirs: array of TUsesDir;
                          NamePostFix: String=''; ExtraArgs: String=''); overload;
    // Logging
    procedure LogText(const s: string; CopyToTestLogger: Boolean = False);
    procedure LogError(const s: string; CopyToTestLogger: Boolean = False);

    function Matches(RegEx, Val: string; ACaseSense: Boolean = False): Boolean;
    // TestAsserts
    function TestMatches(Expected, Got: string; ACaseSense: Boolean = False): Boolean;
    function TestMatches(Name: string; Expected, Got: string; MinDbgVers: Integer = 0; AIgnoreReason: String = ''): Boolean;
    function TestMatches(Name: string; Expected, Got: string; ACaseSense: Boolean; MinDbgVers: Integer = 0; AIgnoreReason: String = ''): Boolean;
    function TestMatches(Name: string; Expected, Got: string; MinDbgVers: Integer; MinFpcVers: Integer; AIgnoreReason: String = ''): Boolean;
    function TestMatches(Name: string; Expected, Got: string; ACaseSense: Boolean; MinDbgVers: Integer; MinFpcVers: Integer; AIgnoreReason: String = ''): Boolean;

    function TestEquals(Expected, Got: string; ACaseSense: Boolean = False): Boolean;
    function TestEquals(Name: string; Expected, Got: string; MinDbgVers: Integer = 0; AIgnoreReason: String = ''): Boolean;
    function TestEquals(Name: string; Expected, Got: string; MinDbgVers: Integer; MinFpcVers: Integer; AIgnoreReason: String = ''): Boolean;

    function TestEquals(Name: string; Expected, Got: string; ACaseSense: Boolean; MinDbgVers: Integer = 0; AIgnoreReason: String = ''): Boolean;
    function TestEquals(Name: string; Expected, Got: string; ACaseSense: Boolean; MinDbgVers: Integer; MinFpcVers: Integer; AIgnoreReason: String = ''): Boolean;

    function TestEquals(Expected, Got: integer): Boolean;
    function TestEquals(Name: string; Expected, Got: integer; MinDbgVers: Integer = 0; AIgnoreReason: String = ''): Boolean;
    function TestEquals(Name: string; Expected, Got: integer; MinDbgVers: Integer; MinFpcVers: Integer; AIgnoreReason: String = ''): Boolean;

    function TestTrue(Name: string; Got: Boolean; MinDbgVers: Integer = 0; AIgnoreReason: String = ''): Boolean;
    function TestTrue(Name: string; Got: Boolean; MinDbgVers: Integer; MinFpcVers: Integer; AIgnoreReason: String = ''): Boolean;
    function TestFalse(Name: string; Got: Boolean; MinDbgVers: Integer = 0; AIgnoreReason: String = ''): Boolean;
    function TestFalse(Name: string; Got: Boolean; MinDbgVers: Integer; MinFpcVers: Integer; AIgnoreReason: String = ''): Boolean;

    property Parent: TDBGTestsuite read FParent;
    property Compiler: TTestDbgCompiler read GetCompiler;
    property Debugger: TTestDbgDebugger read GetDebugger;
    // TestResults
    property TestBaseName: String read FTestBaseName write FTestBaseName;
  end;

  TTestCaseClass = class of TDBGTestCase;

  { TDBGTestWrapper }

  TDBGTestWrapper = class(TTestSuite)
  private
    FParent: TDBGTestsuite;
  public
    constructor CreateTest(AParent: TDBGTestsuite; AClass: TClass); overload;
    procedure AddTest(ATest: TTest); overload; override;
  end;

  { TDBGTestsuite }

  TDBGTestsuite = class(TTestSuite)
  private
    FCompiler: TTestDbgCompiler;
    FDebugger: TTestDbgDebugger;
    FInRun: Boolean;
  protected
    procedure Clear; virtual;
  public
    constructor Create(ACompiler: TTestDbgCompiler; ADebugger: TTestDbgDebugger); overload;
    procedure RegisterDbgTest(ATestClass: TTestCaseClass);
    procedure Run(AResult: TTestResult); override;
    procedure RunTest(ATest: TTest; AResult: TTestResult); override;

    property Compiler: TTestDbgCompiler read FCompiler;
    property Debugger: TTestDbgDebugger read FDebugger;
  end;

  TDBGTestsuiteClass = class of TDBGTestsuite;

procedure RegisterDbgTest(ATestClass: TTestCaseClass);

procedure CreateTestSuites(ACompilerList: TTestDbgCompilerList;
  ADebuggerList: TTestDbgDebuggerList; ATestSuiteClass: TDBGTestsuiteClass);

implementation

{ TDBGTestCase }

function TDBGTestCase.GetCompiler: TTestDbgCompiler;
begin
  Result := Parent.Compiler;
end;

function TDBGTestCase.GetDebugger: TTestDbgDebugger;
begin
  Result := Parent.Debugger;
end;

procedure TDBGTestCase.AddTestError(s: string; MinDbgVers: Integer;
  AIgnoreReason: String);
begin
  AddTestError(s, MinDbgVers, 0, AIgnoreReason);
end;

procedure TDBGTestCase.AddTestError(s: string; MinDbgVers: Integer;
  MinFpcVers: Integer; AIgnoreReason: String);
var
  IgnoreReason: String;
  i: Integer;
begin
  inc(FTestCnt);
  IgnoreReason := '';
  s := FTestBaseName + s;
  if MinDbgVers > 0 then begin
    i := Debugger.Version;
    if (i > 0) and (i < MinDbgVers) then
      IgnoreReason := 'GDB ('+IntToStr(i)+') to old, required:'+IntToStr(MinDbgVers);
  end;
  if MinFpcVers > 0 then begin
    i := Compiler.Version;
    if (i > 0) and (i < MinFpcVers) then
      IgnoreReason := 'FPC ('+IntToStr(i)+') to old, required:'+IntToStr(MinFpcVers);
  end;
  IgnoreReason := IgnoreReason + AIgnoreReason;

  if IgnoreReason <> '' then begin
    FIgnoredErrors := FIgnoredErrors + IntToStr(FTestCnt) + ': ' + '### '+IgnoreReason +' >>> '+s+LineEnding;
    inc(FIgnoredErrorCnt);
  end else begin
    FTestErrors := FTestErrors + IntToStr(FTestCnt) + ': ' + s + LineEnding;
    inc(FTestErrorCnt);
  end;
end;

procedure TDBGTestCase.AddTestSuccess(s: string; MinDbgVers: Integer;
  AIgnoreReason: String);
begin
  AddTestSuccess(s, MinDbgVers, 0, AIgnoreReason);
end;

procedure TDBGTestCase.AddTestSuccess(s: string; MinDbgVers: Integer;
  MinFpcVers: Integer; AIgnoreReason: String);
var
  i: Integer;
begin
  s := FTestBaseName + s;
  inc(FTestCnt);
  if (MinDbgVers > 0) then begin
    i := Debugger.Version;
    if (i > 0) and (i < MinDbgVers) then
      AIgnoreReason := AIgnoreReason
        + 'GDB ('+IntToStr(i)+') to old, required:'+IntToStr(MinDbgVers);
  end;
  if (MinFpcVers > 0) then begin
    i := Compiler.Version;
    if (i > 0) and (i < MinFpcVers) then
      AIgnoreReason := AIgnoreReason
        + 'FPC ('+IntToStr(i)+') to old, required:'+IntToStr(MinFpcVers);
  end;

  if AIgnoreReason <> '' then begin
    s := '[OK] ' + s;
    FUnexpectedSuccess:= FUnexpectedSuccess + IntToStr(FTestCnt) + ': ' + '### '+AIgnoreReason +' >>> '+s+LineEnding;
    inc(FUnexpectedSuccessCnt);
  end
  else
    inc(FSucessCnt);
end;

procedure TDBGTestCase.ClearTestErrors;
begin
  FTotalErrorCnt := FTotalErrorCnt + FTestErrorCnt;
  FTotalIgnoredErrorCnt := FTotalIgnoredErrorCnt + FIgnoredErrorCnt;
  FTotalUnexpectedSuccessCnt := FTotalUnexpectedSuccessCnt + FUnexpectedSuccessCnt;

  FTestErrors := '';
  FIgnoredErrors := '';
  FUnexpectedSuccess := '';
  FTestErrorCnt := 0;
  FIgnoredErrorCnt := 0;
  FUnexpectedSuccessCnt := 0;
  FSucessCnt := 0;
  FTestCnt := 0;
  FTestBaseName := '';
end;

procedure TDBGTestCase.AssertTestErrors;
var
  s, s1: String;
begin
  s := FTestErrors;
  s1 := Format('Failed: %d of %d - Ignored: %d Unexpected: %d - Success: %d',
               [FTestErrorCnt, FTestCnt, FIgnoredErrorCnt, FUnexpectedSuccessCnt, FSucessCnt ]);
  FTestErrors := '';
  if GetLogActive or (FTestErrorCnt > 0) or (s <> '') then begin
    LogError('***' + s1 + '***' +LineEnding);
    LogError('================= Failed:'+LineEnding);
    LogError(s);
    LogError('================= Ignored'+LineEnding);
    LogError(FIgnoredErrors);
    LogError('================= Unexpected Success'+LineEnding);
    LogError(FUnexpectedSuccess);
    LogError('================='+LineEnding);
  end;
  if s <> '' then begin
    Fail(s1+ LineEnding + s);
  end;
end;

function TDBGTestCase.TestMatches(Expected, Got: string; ACaseSense: Boolean
  ): Boolean;
begin
  TestMatches('', Expected, Got, ACaseSense, 0, 0);
end;

function TDBGTestCase.TestMatches(Name: string; Expected, Got: string;
  MinDbgVers: Integer; AIgnoreReason: String): Boolean;
begin
  Result := TestMatches(Name, Expected, Got, MinDbgVers, 0, AIgnoreReason);
end;

function TDBGTestCase.TestMatches(Name: string; Expected, Got: string;
  ACaseSense: Boolean; MinDbgVers: Integer; AIgnoreReason: String): Boolean;
begin
  TestMatches(Name, Expected, Got, ACaseSense, MinDbgVers);
end;

function TDBGTestCase.TestMatches(Name: string; Expected, Got: string;
  MinDbgVers: Integer; MinFpcVers: Integer; AIgnoreReason: String): Boolean;
begin
  TestMatches(Name, Expected, Got, False, MinDbgVers, MinFpcVers, AIgnoreReason);
end;

function TDBGTestCase.TestMatches(Name: string; Expected, Got: string;
  ACaseSense: Boolean; MinDbgVers: Integer; MinFpcVers: Integer;
  AIgnoreReason: String): Boolean;
begin
  if FRegX = nil then
    FRegX := TRegExpr.Create;
  FRegX.ModifierI := not ACaseSense;
  FRegX.Expression := Expected;
  Result :=  FRegX.Exec(Got);
  Name := Name + ': Expected (regex) "'+Expected+'", Got "'+Got+'"';
  if Result
  then AddTestSuccess(Name, MinDbgVers, MinFpcVers, AIgnoreReason)
  else AddTestError(Name, MinDbgVers, MinFpcVers, AIgnoreReason);
end;

function TDBGTestCase.TestEquals(Expected, Got: string; ACaseSense: Boolean
  ): Boolean;
begin
  Result := TestEquals('', Expected, Got, ACaseSense);
end;

function TDBGTestCase.TestEquals(Name: string; Expected, Got: string;
  MinDbgVers: Integer; AIgnoreReason: String): Boolean;
begin
  Result := TestEquals(Name, Expected, Got, MinDbgVers, 0, AIgnoreReason);
end;

function TDBGTestCase.TestEquals(Name: string; Expected, Got: string;
  MinDbgVers: Integer; MinFpcVers: Integer; AIgnoreReason: String): Boolean;
begin
  Result :=  Got = Expected;
  Name := Name + ': Expected "'+Expected+'", Got "'+Got+'"';
  if Result
  then AddTestSuccess(Name, MinDbgVers, MinFpcVers, AIgnoreReason)
  else AddTestError(Name, MinDbgVers, MinFpcVers, AIgnoreReason);
end;

function TDBGTestCase.TestEquals(Name: string; Expected, Got: string;
  ACaseSense: Boolean; MinDbgVers: Integer; AIgnoreReason: String): Boolean;
begin
  Result := TestEquals(Name, Expected, Got, ACaseSense, MinDbgVers, 0, AIgnoreReason);
end;

function TDBGTestCase.TestEquals(Name: string; Expected, Got: string;
  ACaseSense: Boolean; MinDbgVers: Integer; MinFpcVers: Integer;
  AIgnoreReason: String): Boolean;
begin
  if ACaseSense then
    Result :=  Got = Expected
  else
    Result := UpperCase(Got) = UpperCase(Expected);
  Name := Name + ': Expected "'+Expected+'", Got "'+Got+'"';
  if Result
  then AddTestSuccess(Name, MinDbgVers, MinFpcVers, AIgnoreReason)
  else AddTestError(Name, MinDbgVers, MinFpcVers, AIgnoreReason);
end;

function TDBGTestCase.TestEquals(Expected, Got: integer): Boolean;
begin
  Result := TestEquals('', Expected, Got);
end;

function TDBGTestCase.TestEquals(Name: string; Expected, Got: integer;
  MinDbgVers: Integer; AIgnoreReason: String): Boolean;
begin
  Result := TestEquals(Name, Expected, Got, MinDbgVers, 0, AIgnoreReason);
end;

function TDBGTestCase.TestEquals(Name: string; Expected, Got: integer;
  MinDbgVers: Integer; MinFpcVers: Integer; AIgnoreReason: String): Boolean;
begin
  Result :=  Got = Expected;
  Name := Name + ': Expected "'+IntToStr(Expected)+'", Got "'+IntToStr(Got)+'"';
  if Result
  then AddTestSuccess(Name, MinDbgVers, MinFpcVers, AIgnoreReason)
  else AddTestError(Name, MinDbgVers, MinFpcVers, AIgnoreReason);
end;

function TDBGTestCase.TestTrue(Name: string; Got: Boolean; MinDbgVers: Integer;
  AIgnoreReason: String): Boolean;
begin
  Result := TestTrue(Name, Got, MinDbgVers, 0, AIgnoreReason);
end;

function TDBGTestCase.TestTrue(Name: string; Got: Boolean; MinDbgVers: Integer;
  MinFpcVers: Integer; AIgnoreReason: String): Boolean;
begin
  Result := Got;
  if Result
  then AddTestSuccess(Name + ': Got "True"', MinDbgVers, MinFpcVers, AIgnoreReason)
  else AddTestError(Name + ': Expected "True", Got "False"', MinDbgVers, MinFpcVers, AIgnoreReason);
end;

function TDBGTestCase.TestFalse(Name: string; Got: Boolean;
  MinDbgVers: Integer; AIgnoreReason: String): Boolean;
begin
  Result := TestFalse(Name, Got, MinDbgVers, 0, AIgnoreReason);
end;

function TDBGTestCase.TestFalse(Name: string; Got: Boolean;
  MinDbgVers: Integer; MinFpcVers: Integer; AIgnoreReason: String): Boolean;
begin
  Result := not Got;
  if Result
  then AddTestSuccess(Name + ': Got "False"', MinDbgVers, MinFpcVers, AIgnoreReason)
  else AddTestError(Name + ': Expected "False", Got "True"', MinDbgVers, MinFpcVers, AIgnoreReason);
end;

function TDBGTestCase.GetLogActive: Boolean;
begin
  Result := (TestControlGetWriteLog = wlAlways) or FLogFileCreated;
end;

function TDBGTestCase.GetLogFileName: String;
begin
  Result := TestName
    + '_' + NameToFileName(Compiler.Name, False)
    + '_' + SymbolTypeNames[Compiler.SymbolType]
    + '_' + CpuBitNames[Compiler.CpuBitType]
    + '_' + NameToFileName(Debugger.Name, False)
    ; // .log extension will be added
end;

function TDBGTestCase.GetFinalLogFileName: String;
begin
  Result := FLogFileName;

  if (FTotalIgnoredErrorCnt + FIgnoredErrorCnt > 0)
  then Result := Result + '.ignor_'+IntToStr(FTotalIgnoredErrorCnt + FIgnoredErrorCnt);
  if (FTotalUnexpectedSuccessCnt + FUnexpectedSuccessCnt > 0)
  then Result := Result + '.unexp_'+IntToStr(FTotalUnexpectedSuccessCnt + FUnexpectedSuccessCnt);
  if (FTotalErrorCnt  + FTestErrorCnt > 0)
  then Result := Result + '.fail_'+IntToStr(FTotalErrorCnt  + FTestErrorCnt);
end;

procedure TDBGTestCase.InitLog;
begin
  FLogFileCreated := False;
  FLogBufferText := '';
end;

procedure TDBGTestCase.CreateLog;
var
  name: String;
  i: Integer;
  dir: String;
begin
  if FLogFileCreated then exit;

  name := GetLogFileName;
  for i := 1 to length(name) do
    if name[i] in ['/', '\', '*', '?', ':'] then
      name[i] := '_';

  if DirectoryExistsUTF8(TestControlGetLogPath) then
    dir := TestControlGetLogPath
  else
    dir := GetCurrentDirUTF8;

  FLogFileName := dir + name;

  AssignFile(FLogFile, FLogFileName + '.log.running');
  Rewrite(FLogFile);
  FLogFileCreated := True;

  writeln(FLogFile, FLogBufferText);
  FLogBufferText := '';
end;

procedure TDBGTestCase.FinishLog;
var
  NewName: String;
begin
  if FLogFileCreated then begin
    CloseFile(FLogFile);
    NewName := GetFinalLogFileName;
    RenameFileUTF8(FLogFileName + '.log.running', NewName + '.log');
  end;
  FLogBufferText := '';
end;

procedure TDBGTestCase.LogText(const s: string; CopyToTestLogger: Boolean);
begin
  if GetLogActive then begin
    CreateLog;
    writeln(FLogFile, s);
  end
  else begin
    if length(FLogBufferText) > 50000000 then
      Delete(FLogBufferText, 1 , Length(s + LineEnding));
    FLogBufferText := FLogBufferText + s + LineEnding;
  end;
  if CopyToTestLogger then
    TestLogger.DebugLn(s);
end;

procedure TDBGTestCase.LogError(const s: string; CopyToTestLogger: Boolean);
begin
  if GetLogActive or (TestControlGetWriteLog = wlOnError) then
    CreateLog;
  writeln(FLogFile, s);
  if CopyToTestLogger then
    TestLogger.DebugLn(s);
end;

function TDBGTestCase.Matches(RegEx, Val: string; ACaseSense: Boolean): Boolean;
begin
  if FRegX = nil then
    FRegX := TRegExpr.Create;
  FRegX.ModifierI := not ACaseSense;
  FRegX.Expression := RegEx;
  Result :=  FRegX.Exec(Val);
end;

procedure TDBGTestCase.DoDbgOut(Sender: TObject; S: string; var Handled: Boolean
  );
begin
  LogText(': ' + S);
  Handled := True;
end;

procedure TDBGTestCase.DoDebugln(Sender: TObject; S: string;
  var Handled: Boolean);
begin
  LogText(S);
  Handled := True;
end;

procedure TDBGTestCase.SetUp;
begin
  ClearTestErrors;
  FTotalErrorCnt := 0;
  FTotalIgnoredErrorCnt := 0;
  FTotalUnexpectedSuccessCnt := 0;

  InitLog;
  DebugLogger.OnDbgOut  := @DoDbgOut;
  DebugLogger.OnDebugLn := @DoDebugln;
  inherited SetUp;
end;

procedure TDBGTestCase.TearDown;
begin
  inherited TearDown;
  DebugLogger.OnDbgOut  := nil;
  DebugLogger.OnDebugLn := nil;
  FinishLog;
  FRegX.Free;
end;

procedure TDBGTestCase.RunTest;
begin
  TestLogger.DebugLn(['Running ', Parent.TestSuiteName, ' ', Parent.TestName, ' ', TestSuiteName, ' ', TestName]);
  try
    ClearTestErrors;
    inherited RunTest;
  finally
    Debugger.CleanAfterTestDone;
  end;
end;

function TDBGTestCase.SkipTest: Boolean;
begin
  Result := not(
    TestControlCanCompiler(Parent.Compiler.Name) and
    TestControlCanDebugger(Parent.Debugger.Name) and
    TestControlCanSymType(Parent.Compiler.SymbolType) and
    TestControlCanCpuBits(Parent.Compiler.CpuBitType)
  );
end;

procedure TDBGTestCase.TestCompile(const PrgName: string; out ExeName: string;
  NamePostFix: String; ExtraArgs: String);
begin
  TestCompile(PrgName, ExeName, [], NamePostFix, ExtraArgs);
end;

procedure TDBGTestCase.TestCompile(const PrgName: string; out ExeName: string;
  UsesDirs: array of TUsesDir; NamePostFix: String; ExtraArgs: String);
begin
  LogText(LineEnding+LineEnding + '******************* compile '+PrgName + ' ' + ExtraArgs +LineEnding );
  Compiler.TestCompile(PrgName, ExeName, UsesDirs, NamePostFix, ExtraArgs);
  LogText(Compiler.LastCompileCommandLine+LineEnding + '*******************' +LineEnding+LineEnding );
end;

{ TDBGTestWrapper }

constructor TDBGTestWrapper.CreateTest(AParent: TDBGTestsuite; AClass: TClass);
begin
  FParent := AParent;
  Create(AClass);
end;

procedure TDBGTestWrapper.AddTest(ATest: TTest);
begin
  if ATest is TDBGTestCase then
    TDBGTestCase(ATest).FParent := FParent;
  inherited AddTest(ATest);
end;

{ TDBGTestsuite }

procedure TDBGTestsuite.Clear;
begin
  //
end;

constructor TDBGTestsuite.Create(ACompiler: TTestDbgCompiler;
  ADebugger: TTestDbgDebugger);
begin
  FInRun := False;
  FCompiler := ACompiler;
  FDebugger := ADebugger;
  inherited Create(ACompiler.FullName + ', ' + ADebugger.FullName);
end;

procedure TDBGTestsuite.RegisterDbgTest(ATestClass: TTestCaseClass);
var
  NewSuite: TDBGTestWrapper;
begin
  NewSuite := TDBGTestWrapper.CreateTest(Self, ATestClass);
  AddTest(NewSuite);
end;

procedure TDBGTestsuite.Run(AResult: TTestResult);
begin
  FInRun := True;
  try
    inherited Run(AResult);
  finally
    FInRun := False;
    Clear;
  end;
end;

procedure TDBGTestsuite.RunTest(ATest: TTest; AResult: TTestResult);
begin
  try
    inherited RunTest(ATest, AResult);
  finally
    if not FInRun then Clear;
  end;
end;

procedure RegisterDbgTest(ATestClass: TTestCaseClass);
var
  Suite: TTestSuite;
  i: Integer;
begin
  Suite := GetTestRegistry;
  for i := 0 to Suite.ChildTestCount - 1 do
    if Suite.Test[i] is TDBGTestsuite then
      TDBGTestsuite(Suite.Test[i]).RegisterDbgTest(ATestClass);
end;

procedure CreateTestSuites(ACompilerList: TTestDbgCompilerList;
  ADebuggerList: TTestDbgDebuggerList; ATestSuiteClass: TDBGTestsuiteClass);
var
  i, j: Integer;
  r: TTestSuite;
begin
  r := GetTestRegistry;
  for i := 0 to ACompilerList.Count - 1 do
  for j := 0 to ADebuggerList.Count - 1 do begin
    if ADebuggerList[j].MatchesCompiler(ACompilerList[i]) then begin
      r.AddTest(ATestSuiteClass.Create(ACompilerList[i], ADebuggerList[j]));
    end;
  end;
end;

end.

