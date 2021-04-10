unit TestDbgTestSuites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TTestDbgExecuteables, TestDbgControl, TestDbgConfig,
  TestOutputLogger, TestCommonSources, LazFileUtils, LazLogger,
  DbgIntfDebuggerBase, StrUtils, fpcunit, testregistry, RegExpr;

const
  EqIgnoreCase = False; // for TestEquals(..., CaseSense, ...);
  EqMatchCase = True;

type
  TDBGTestsuite = class;
  TDBGStates = set of TDBGState;

  { TDbgBaseTestsuite }

  TDbgBaseTestsuite = class(TTestSuite)
  private
    FInRun: Integer;
    FDirectParent: TDbgBaseTestsuite;
    FOverviewReport: String;
    procedure LogOverviewReport;
  protected
    procedure Clear; virtual;
  public
    procedure Run(AResult: TTestResult); override;
    procedure RunTest(ATest: TTest; AResult: TTestResult); override;
    procedure AddTest(ATest: TTest); overload; override;

    procedure AddOverviewLog(Const AText: String);
  end;

  { TDBGTestCase }

  TDBGTestCase = class(TTestCase)
  private
    FParent: TDBGTestsuite;
    FDirectParent: TDbgBaseTestsuite;
    // TestResults
    FTestBaseName: String;
    FTestErrors, FIgnoredErrors, FUnexpectedSuccess: String;
    FTestCnt, FTestErrorCnt, FIgnoredErrorCnt, FUnexpectedSuccessCnt, FSucessCnt: Integer;
    FInTestBlock: integer;
    FInTestBlockTxt: String;
    FInTestBlockRes: (tbOk, tbErr, tbIgnore, tbUnexpected);
    FTotalErrorCnt, FTotalIgnoredErrorCnt, FTotalUnexpectedSuccessCnt: Integer;
    FRegX: TRegExpr;

    // Logging
    FLogLock: TRTLCriticalSection;
    FLogFile, FReportFile: TLazLoggerFileHandle;
    FLogFileCreated, FReportFileCreated: Boolean;
    FLogFileName, FReportFileName: String;
    FLogBufferText: TStringList;
    procedure InitLog;
    procedure FinishLog;

    function GetCompiler: TTestDbgCompiler;
    function GetDebugger: TTestDbgDebugger;
  protected
    FIgnoreReason: String;
    // TestResults
    procedure StartTestBlock;
    procedure EndTestBlock;
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
    procedure CreateReport;
    // Debugln
    procedure DoDbgOut(Sender: TObject; S: string; var Handled: Boolean); virtual;
    procedure DoDebugln(Sender: TObject; S: string; var Handled: Boolean); virtual;

    procedure SetUp; override;
    procedure TearDown; override;
    procedure RunTest; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function SkipTest: Boolean; virtual;
    Procedure TestCompile(const PrgName: string; out ExeName: string; NamePostFix: String=''; ExtraArgs: String=''); overload;
    Procedure TestCompile(const PrgName: string; out ExeName: string; const UsesDirs: array of TUsesDir;
                          NamePostFix: String=''; ExtraArgs: String=''); overload;
    Procedure TestCompile(const Prg: TCommonSource; out ExeName: string; NamePostFix: String=''; ExtraArgs: String=''); overload;
    Procedure TestCompile(const Prg: TCommonSource; out ExeName: string; const UsesDirs: array of TUsesDir;
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

    procedure AssertDebuggerState(AState: TDBGState; AName: String = '');
    procedure AssertDebuggerState(AStates: TDBGStates; AName: String = '');
    procedure AssertDebuggerNotInErrorState;

    property Parent: TDBGTestsuite read FParent;
    property Compiler: TTestDbgCompiler read GetCompiler;
    property Debugger: TTestDbgDebugger read GetDebugger;
    // TestResults
    property TestBaseName: String read FTestBaseName write FTestBaseName;
  end;

  TTestCaseClass = class of TDBGTestCase;

  { TDBGTestWrapper }

  TDBGTestWrapper = class(TDbgBaseTestsuite)
  private
    FParent: TDBGTestsuite;
  public
    constructor CreateTest(AParent: TDBGTestsuite; AClass: TClass); overload;
    procedure AddTest(ATest: TTest); overload; override;
  end;

  { TDBGTestsuite }

  TDBGTestsuite = class(TDbgBaseTestsuite)
  private
    FCompiler: TTestDbgCompiler;
    FDebugger: TTestDbgDebugger;
  public
    constructor Create(ACompiler: TTestDbgCompiler; ADebugger: TTestDbgDebugger); overload;
    procedure RegisterDbgTest(ATestClass: TTestCaseClass);

    property Compiler: TTestDbgCompiler read FCompiler;
    property Debugger: TTestDbgDebugger read FDebugger;
  end;

  TDBGTestsuiteClass = class of TDBGTestsuite;

procedure RegisterDbgTest(ATestClass: TTestCaseClass; ASymTypes: TSymbolTypes = []);

procedure CreateTestSuites(ACompilerList: TTestDbgCompilerList;
  ADebuggerList: TTestDbgDebuggerList; ATestSuiteClass: TDBGTestsuiteClass);

implementation

{ TDbgBaseTestsuite }

procedure TDbgBaseTestsuite.LogOverviewReport;
var
  oname: String;
  FOview: TextFile;
begin
  if FOverviewReport = '' then
    exit;
  if TestControlGetWriteOverView = wlAlways then begin
    if DirectoryExistsUTF8(TestControlGetLogPath) then
      oname := TestControlGetLogPath
    else
      oname := GetCurrentDirUTF8;

    oname := oname + 'overview_' +
      NameToFileName(DateTimeToStr(Now), False) +
      '.txt';
    AssignFile(FOView, oname);
    Rewrite(FOView);
    writeln(FOView, FOverviewReport);
    CloseFile(FOView);
  end;
  FOverviewReport := '';
end;

procedure TDbgBaseTestsuite.Clear;
begin
  //
end;

procedure TDbgBaseTestsuite.Run(AResult: TTestResult);
begin
  inc(FInRun);
  try
    inherited Run(AResult);
  finally
    dec(FInRun);
    if FInRun = 0 then begin
      LogOverviewReport;
    end;
    Clear;
  end;
end;

procedure TDbgBaseTestsuite.RunTest(ATest: TTest; AResult: TTestResult);
begin
  inc(FInRun);
  try
    inherited RunTest(ATest, AResult);
  finally
    dec(FInRun);
    if FInRun = 0 then begin
      LogOverviewReport;
      Clear;
    end;
  end;
end;

procedure TDbgBaseTestsuite.AddTest(ATest: TTest);
begin
  inherited AddTest(ATest);
  if ATest is TDbgBaseTestsuite then
    TDbgBaseTestsuite(ATest).FDirectParent := Self
  else
  if ATest is TDBGTestCase then
    TDBGTestCase(ATest).FDirectParent := Self;
end;

procedure TDbgBaseTestsuite.AddOverviewLog(const AText: String);
begin
  if (FDirectParent <> nil) and (FDirectParent.FInRun > 0) then begin
    FDirectParent.AddOverviewLog(AText);
    exit;
  end;
  FOverviewReport := FOverviewReport + AText;
  if (FInRun = 0) then
    LogOverviewReport;
end;

{ TDBGTestCase }

function TDBGTestCase.GetCompiler: TTestDbgCompiler;
begin
  Result := Parent.Compiler;
end;

function TDBGTestCase.GetDebugger: TTestDbgDebugger;
begin
  Result := Parent.Debugger;
end;

procedure TDBGTestCase.StartTestBlock;
begin
  if FInTestBlock = 0 then begin
    inc(FTestCnt);
    FInTestBlockTxt := '';
    FInTestBlockRes := tbOk;
  end;
  inc(FInTestBlock);
end;

procedure TDBGTestCase.EndTestBlock;
begin
  dec(FInTestBlock);
  if FInTestBlock = 0 then begin
    case FInTestBlockRes of
      tbErr: begin
          FTestErrors := FTestErrors + FInTestBlockTxt;
          inc(FTestErrorCnt);
        end;
      tbIgnore: begin
          FIgnoredErrors := FIgnoredErrors + FInTestBlockTxt;
          inc(FIgnoredErrorCnt);
        end;
      tbUnexpected: begin
          FUnexpectedSuccess:= FUnexpectedSuccess + FInTestBlockTxt;
          inc(FUnexpectedSuccessCnt);
        end;
    end;
    FInTestBlockTxt := '';
    FInTestBlockRes := tbOk;
  end;
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
  if FInTestBlock = 0 then
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
  if IgnoreReason = '' then
    IgnoreReason := FIgnoreReason;

  if FInTestBlock > 0 then begin
    if IgnoreReason <> '' then begin
      FInTestBlockTxt := FInTestBlockTxt + IntToStr(FTestCnt) + ': ' + '### '+IgnoreReason +' >>> '+s+LineEnding;
      if FInTestBlockRes in [tbOk, tbUnexpected] then
        FInTestBlockRes := tbIgnore;
    end else begin
      FInTestBlockTxt := FInTestBlockTxt + IntToStr(FTestCnt) + ': ' + s + LineEnding;
      FInTestBlockRes := tbErr;
      DebugLn(['!!!!! ERROR: ' + IntToStr(FTestCnt) + ': ' + s]);
    end;
  end
  else begin
    if IgnoreReason <> '' then begin
      FIgnoredErrors := FIgnoredErrors + IntToStr(FTestCnt) + ': ' + '### '+IgnoreReason +' >>> '+s+LineEnding;
      inc(FIgnoredErrorCnt);
    end else begin
      FTestErrors := FTestErrors + IntToStr(FTestCnt) + ': ' + s + LineEnding;
      DebugLn(['!!!!! ERROR: ' + IntToStr(FTestCnt) + ': ' + s]);
      inc(FTestErrorCnt);
    end;
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
  if FInTestBlock = 0 then
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
    if FInTestBlock > 0 then begin
      FInTestBlockTxt := FInTestBlockTxt + IntToStr(FTestCnt) + ': ' + '### '+AIgnoreReason +' >>> '+s+LineEnding;
      if FInTestBlockRes in [tbOk] then
        FInTestBlockRes := tbUnexpected;
    end
    else begin
      FUnexpectedSuccess:= FUnexpectedSuccess + IntToStr(FTestCnt) + ': ' + '### '+AIgnoreReason +' >>> '+s+LineEnding;
      inc(FUnexpectedSuccessCnt);
    end;
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

  function RemoveHexNumbers(txt: String): String;
  var
    i, j, n: Integer;
    p, p2: SizeInt;
    s: String;
  begin
    Result := txt;
    i := 1;
    j := 1;
    n := 0;
    p := PosEx('$', Result, i);
    while p > 0 do begin
      if p > n then j := 1;
      n := PosSetEx([#10,#13], Result, p);
      i := p+2;

      p2 := p + 2;
      while (p2 <= Length(Result)) and (Result[p2] in ['0'..'9', 'a'..'f', 'A'..'F']) do
        inc(p2);
      if p2 - p > 6 then begin
        s := copy(Result, p, p2-p);
        Result := StringReplace(Result, s, '$##HEX'+IntToStr(j)+'##', [rfReplaceAll, rfIgnoreCase]);
        inc(j);
      end;

      p := PosEx('$', Result, i);
    end;
  end;

var
  s, s1: String;
begin
  s := FTestErrors;
  s1 := Format('Failed: %4d of %5d - Ignored: %5d Unexpected: %4d - Success: %5d',
               [FTestErrorCnt, FTestCnt, FIgnoredErrorCnt, FUnexpectedSuccessCnt, FSucessCnt ]);
  FDirectParent.AddOverviewLog(Format('%-30s  %14s %12s %7s  %18s   %s',
    [TestName, Compiler.Name, SymbolTypeNames[Compiler.SymbolType],
     CpuBitNames[Compiler.CpuBitType], Debugger.Name,
     s1 + LineEnding]));
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
  if (TestControlGetWriteReport = wlAlways) or
     ( (TestControlGetWriteReport = wlOnError) and (
       (FTestErrorCnt > 0) or (FIgnoredErrorCnt > 0) or (FUnexpectedSuccessCnt > 0)
     ))
  then begin
    CreateReport;
    FReportFile.WriteLnToFile('***' + s1 + '***' +LineEnding);
    FReportFile.WriteLnToFile('================= Failed:'+LineEnding);
    FReportFile.WriteLnToFile(RemoveHexNumbers(s));
    FReportFile.WriteLnToFile('================= Ignored'+LineEnding);
    FReportFile.WriteLnToFile(RemoveHexNumbers(FIgnoredErrors));
    FReportFile.WriteLnToFile('================= Unexpected Success'+LineEnding);
    FReportFile.WriteLnToFile(RemoveHexNumbers(FUnexpectedSuccess));
    FReportFile.WriteLnToFile('================='+LineEnding);
  end;

  FIgnoredErrors := '';
  FUnexpectedSuccess := '';
  if s <> '' then
    Fail(s1+ LineEnding + s);
end;

function TDBGTestCase.TestMatches(Expected, Got: string; ACaseSense: Boolean
  ): Boolean;
begin
  Result := TestMatches('', Expected, Got, ACaseSense, 0, 0);
end;

function TDBGTestCase.TestMatches(Name: string; Expected, Got: string;
  MinDbgVers: Integer; AIgnoreReason: String): Boolean;
begin
  Result := TestMatches(Name, Expected, Got, MinDbgVers, 0, AIgnoreReason);
end;

function TDBGTestCase.TestMatches(Name: string; Expected, Got: string;
  ACaseSense: Boolean; MinDbgVers: Integer; AIgnoreReason: String): Boolean;
begin
  Result := TestMatches(Name, Expected, Got, ACaseSense, MinDbgVers);
end;

function TDBGTestCase.TestMatches(Name: string; Expected, Got: string;
  MinDbgVers: Integer; MinFpcVers: Integer; AIgnoreReason: String): Boolean;
begin
  Result := TestMatches(Name, Expected, Got, False, MinDbgVers, MinFpcVers, AIgnoreReason);
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

procedure TDBGTestCase.AssertDebuggerState(AState: TDBGState; AName: String);
begin
  if not TestEquals('Debugger State '+AName, dbgs(AState), dbgs(Debugger.LazDebugger.State)) then
    AssertTestErrors;
end;

procedure TDBGTestCase.AssertDebuggerState(AStates: TDBGStates; AName: String);
begin
  If not (Debugger.LazDebugger.State in AStates) then begin
    TestTrue('Debugger State not in expected, got: ' + dbgs(Debugger.LazDebugger.State) + ' ' +AName, False);
    AssertTestErrors;
  end;
end;

procedure TDBGTestCase.AssertDebuggerNotInErrorState;
begin
  If (Debugger.LazDebugger.State = dsError) then begin
    TestTrue('Debugger State should not be dsError', False);
    AssertTestErrors;
  end;
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
  then Result := Result + '___ignor.'+IntToStr(FTotalIgnoredErrorCnt + FIgnoredErrorCnt);
  if (FTotalUnexpectedSuccessCnt + FUnexpectedSuccessCnt > 0)
  then Result := Result + '___unexp.'+IntToStr(FTotalUnexpectedSuccessCnt + FUnexpectedSuccessCnt);
  if (FTotalErrorCnt  + FTestErrorCnt > 0)
  then Result := Result + '___fail.'+IntToStr(FTotalErrorCnt  + FTestErrorCnt);
end;

procedure TDBGTestCase.InitLog;
begin
  FLogFileCreated := False;
  FLogBufferText.Clear;
end;

procedure TDBGTestCase.CreateLog;
var
  name: String;
  i: Integer;
  dir: String;
begin
  if FLogFileCreated then exit;
  EnterCriticalsection(FLogLock);
  try
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

    {$IFDEF Windows}
    FLogFile := TLazLoggerFileHandleThreadSave.Create;
    {$ELSE}
    FLogFile := TLazLoggerFileHandleMainThread.Create;
    {$ENDIF}
    FLogFile.LogName := FLogFileName + '.log.running';
    //AssignFile(FLogFile, FLogFileName + '.log.running');
    //Rewrite(FLogFile);
    FLogFileCreated := True;

    FLogFile.WriteLnToFile(FLogBufferText.Text);
    //writeln(FLogFile, FLogBufferText);
    FLogBufferText.Clear;
  finally
    LeaveCriticalsection(FLogLock);
  end;
end;

procedure TDBGTestCase.CreateReport;
var
  name: String;
  i: Integer;
  dir: String;
begin
  if FReportFileCreated then exit;
  EnterCriticalsection(FLogLock);
  try
    if FReportFileCreated then exit;

    name := GetLogFileName;
    for i := 1 to length(name) do
      if name[i] in ['/', '\', '*', '?', ':'] then
        name[i] := '_';

    if DirectoryExistsUTF8(TestControlGetLogPath) then
      dir := TestControlGetLogPath
    else
      dir := GetCurrentDirUTF8;

    FReportFileName := dir + name;

    {$IFDEF Windows}
    FReportFile := TLazLoggerFileHandleThreadSave.Create;
    {$ELSE}
    FReportFile := TLazLoggerFileHandleMainThread.Create;
    {$ENDIF}
    FReportFile.LogName := FReportFileName + '___fail.' + IntToStr(FTestErrorCnt) + '.report';
    //AssignFile(FReportFile, FReportFileName + '.log.running');
    //Rewrite(FReportFile);
    FReportFileCreated := True;

  finally
    LeaveCriticalsection(FLogLock);
  end;
end;

procedure TDBGTestCase.FinishLog;
var
  NewName: String;
begin
  if FLogFileCreated then begin
    CheckSynchronize(1);
    FreeAndNil(FLogFile);
    //CloseFile(FLogFile);
    NewName := GetFinalLogFileName;
    sleep(5);
    RenameFileUTF8(FLogFileName + '.log.running', NewName + '.log');
  end;
  if FReportFileCreated then begin
    CheckSynchronize(1);
    FreeAndNil(FReportFile);
    //CloseFile(FReportFile);
    FReportFileCreated := False;
  end;
  FLogBufferText.Clear;
end;

function EscapeText(s: String): String;
begin
  Result := s;
  Result := StringReplace(Result, #0, '\\x00', [rfReplaceAll]);
end;

procedure TDBGTestCase.LogText(const s: string; CopyToTestLogger: Boolean);
begin
  if GetLogActive then begin
    CreateLog;
    FLogFile.WriteLnToFile(EscapeText(s));
    //writeln(FLogFile, EscapeText(s));
  end
  else begin
    EnterCriticalsection(FLogLock);
    try
      if FLogBufferText.Count > 500000 then
        FLogBufferText.Delete(1);
      FLogBufferText.Add(EscapeText(s));
    finally
      LeaveCriticalsection(FLogLock);
    end;
  end;
  if CopyToTestLogger then
    TestLogger.DebugLn(s);
end;

procedure TDBGTestCase.LogError(const s: string; CopyToTestLogger: Boolean);
begin
  if GetLogActive or (TestControlGetWriteLog = wlOnError) then begin
    CreateLog;
    FLogFile.WriteLnToFile(EscapeText(s));
    //writeln(FLogFile, EscapeText(s));
  end;
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
var
  i: Integer;
begin
  InitCriticalSection(FLogLock);
  ClearTestErrors;
  FTotalErrorCnt := 0;
  FTotalIgnoredErrorCnt := 0;
  FTotalUnexpectedSuccessCnt := 0;
  FIgnoreReason := '';

  for i := 0 to DebugLogger.LogGroupList.Count - 1 do
    DebugLogger.LogGroupList[i]^.Enabled := True;

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
  FreeAndNil(FRegX);
  DoneCriticalsection(FLogLock);
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

constructor TDBGTestCase.Create;
begin
  inherited Create;
  FLogBufferText := TStringList.Create;
end;

destructor TDBGTestCase.Destroy;
begin
  FreeAndNil(FLogBufferText);
  inherited Destroy;
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
  const UsesDirs: array of TUsesDir; NamePostFix: String; ExtraArgs: String);
begin
  try
    LogText(LineEnding+LineEnding + '******************* compile '+PrgName + ' ' + ExtraArgs +LineEnding );
    Compiler.TestCompile(PrgName, ExeName, UsesDirs, NamePostFix, ExtraArgs);
    LogText(Compiler.LastCompileCommandLine+LineEnding + '*******************' +LineEnding+LineEnding );
  except
    On E: Exception do begin
      TestTrue('Compile '+PrgName + ' GOT: '+ E.Message+ LineEnding + Compiler.LastCompileOutput, False);
      AssertTestErrors;
    end;
  end;
end;

procedure TDBGTestCase.TestCompile(const Prg: TCommonSource; out
  ExeName: string; NamePostFix: String; ExtraArgs: String);
begin
  TestCompile(Prg, ExeName, [], NamePostFix, ExtraArgs);
end;

procedure TDBGTestCase.TestCompile(const Prg: TCommonSource; out
  ExeName: string; const UsesDirs: array of TUsesDir; NamePostFix: String;
  ExtraArgs: String);
begin
  Prg.Save(AppDir);
  TestCompile(Prg.FullFileName, ExeName, UsesDirs, NamePostFix, ExtraArgs);
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

constructor TDBGTestsuite.Create(ACompiler: TTestDbgCompiler;
  ADebugger: TTestDbgDebugger);
begin
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

var
  MainTestSuite: TDbgBaseTestsuite;

procedure RegisterDbgTest(ATestClass: TTestCaseClass; ASymTypes: TSymbolTypes);
var
  Suite: TTestSuite;
  i: Integer;
begin
  //Suite := GetTestRegistry;
  Suite := MainTestSuite;
  for i := 0 to Suite.ChildTestCount - 1 do
    if Suite.Test[i] is TDBGTestsuite then
      if (ASymTypes = []) or (TDBGTestsuite(Suite.Test[i]).Compiler.SymbolType in ASymTypes) then
        TDBGTestsuite(Suite.Test[i]).RegisterDbgTest(ATestClass);
end;

procedure CreateTestSuites(ACompilerList: TTestDbgCompilerList;
  ADebuggerList: TTestDbgDebuggerList; ATestSuiteClass: TDBGTestsuiteClass);
var
  i, j: Integer;
begin
  MainTestSuite := TDbgBaseTestsuite.Create;
  GetTestRegistry.AddTest(MainTestSuite);
  for i := 0 to ACompilerList.Count - 1 do
  for j := 0 to ADebuggerList.Count - 1 do begin
    if ADebuggerList[j].MatchesCompiler(ACompilerList[i]) then begin
      MainTestSuite.AddTest(ATestSuiteClass.Create(ACompilerList[i], ADebuggerList[j]));
    end;
  end;
end;

end.

