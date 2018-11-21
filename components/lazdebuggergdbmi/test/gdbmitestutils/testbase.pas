unit TestBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fpcunit, testregistry, LCLProc,
  LazLogger, LazFileUtils, DbgIntfDebuggerBase, Dialogs, Forms, RegExpr,
  GDBMIDebugger, TestDbgConfig, TestDbgTestSuites, TTestDbgExecuteables,
  TTestDebuggerClasses, TestDbgCompilerProcess, TestDbgControl, TestOutputLogger; // , FpGdbmiDebugger;
  // EnvironmentOpts, ExtToolDialog, TransferMacros,


const

  stDwarf2All = [stDwarf, stDwarfSet];
  stDwarfAll  = [stDwarf, stDwarfSet, stDwarf3];
  stSymAll = [stStabs, stDwarf, stDwarfSet, stDwarf3];

  TWatchDisplayFormatNames: array [TWatchDisplayFormat] of string =
    ('wdfDefault',
     'wdfStructure',
     'wdfChar', 'wdfString',
     'wdfDecimal', 'wdfUnsigned', 'wdfFloat', 'wdfHex',
     'wdfPointer',
     'wdfMemDump', 'wdfBinary'
    );

type

  TGDBMIDebuggerClass = class of TGDBMIDebugger;



  TDebuggerInfo = TExternalExeInfo;
  TCompilerInfo = TExternalExeInfo;
  { TCompilerList }

  TCompilerList = class(TBaseList)
  private
    function GetCompilerInfo(Index: Integer): TCompilerInfo;
  public
    property CompilerInfo[Index: Integer]: TCompilerInfo read GetCompilerInfo;
  end;

  { TDebuggerList }

  TDebuggerList = class(TBaseList)
  private
    function GetDebuggerInfo(Index: Integer): TDebuggerInfo;
  public
    property DebuggerInfo[Index: Integer]: TDebuggerInfo read GetDebuggerInfo;
  end;

  { TCompilerSuite }

  TCompilerSuite = class(TDBGTestsuite)
  private
    FSymbolSwitch: String;
    FFileNameExt: String;
    FCompileProcess: TCompilerProcess;
    function GetCompilerInfo: TExternalExeInfo;
    function GetDebuggerInfo: TExternalExeInfo;
    function GetSymbolType: TSymbolType;
  protected
  public
    constructor Create; reintroduce; overload; override;
  public
    property SymbolType: TSymbolType read GetSymbolType;
    property SymbolSwitch: String read FSymbolSwitch;

    property CompilerInfo: TExternalExeInfo read GetCompilerInfo;
    property DebuggerInfo: TExternalExeInfo read GetDebuggerInfo;
  end;

  { TGDBMIDebuggerForTest }
  var
    FEvalDone: Boolean;
    FEvalRes: String;
    FEvalResType: TDBGType;

  type

  TGDBMIDebuggerForTest = class helper for TGDBMIDebugger
  private
    procedure EvalCallBack(Sender: TObject; ASuccess: Boolean;
      ResultText: String; ResultDBGType: TDBGType);
  public
    function EvaluateWait(const AExpression: String; var ARes: String;
      var AResType: TDBGType; EvalFlags: TDBGEvaluateFlags = []): Boolean;
  end;


  { TGDBTestCase }

  TGDBTestCase = class(TDBGTestCase)
  private
    FTotalGDBInternalErrorCnt, FTotalDsErrorCrash: Integer;
    FTotalClassVsRecord: Integer;
    FStartTime: QWord;
    FLogDebuglnCount: Integer;
    function GetCompilerInfo: TCompilerInfo;
    function GetSymbolType: TSymbolType;
    procedure DoDbgOut(Sender: TObject; S: string; var Handled: Boolean); override;
    procedure DoDebugln(Sender: TObject; S: string; var Handled: Boolean); override;
    function GetWatches: TTestWatchesMonitor;
  protected
    procedure SetUp; override;
    function GetFinalLogFileName: String; override;
    procedure DoDbgOutPut(Sender: TObject; const AText: String); virtual;
    procedure InternalDbgOutPut(Sender: TObject; const AText: String);
    function InternalFeedBack(Sender: TObject; const AText, AInfo: String;
      AType: TDBGFeedbackType; AButtons: TDBGFeedbackResults): TDBGFeedbackResult;
    procedure InternalDbgEvent(Sender: TObject;
      const ACategory: TDBGEventCategory; const AEventType: TDBGEventType;
      const AText: String);
    function GdbClass: TGDBMIDebuggerClass; virtual;
    function StartGDB(AppDir, TestExeName: String): TGDBMIDebugger;
    procedure CleanGdb;

    function GetDebuggerInfo: TDebuggerInfo;

    property TotalClassVsRecord: Integer read FTotalClassVsRecord write FTotalClassVsRecord;
    property TotalDsErrorCrash: Integer read FTotalDsErrorCrash write FTotalDsErrorCrash;
  public
    procedure LogToFile(const s: string);
  public
    property DebuggerInfo: TDebuggerInfo read GetDebuggerInfo;
    property SymbolType: TSymbolType read GetSymbolType;
    property CompilerInfo: TCompilerInfo read GetCompilerInfo;
  public
    property Watches: TTestWatchesMonitor read GetWatches;
  end;


function GetCompilers: TCompilerList;
function GetDebuggers: TDebuggerList;

var
  AppDir: String;
  ConfDir: String;

  TestGdbClass: TGDBMIDebuggerClass = TGDBMIDebugger;
  // TestGdbClass: TGDBMIDebuggerClass = TFPGDBMIDebugger;


implementation

var
  Compilers: TCompilerList = nil;
  Debuggers: TDebuggerList = nil;


function GetCompilers: TCompilerList;
begin
  if Compilers <> nil then exit(Compilers);
  Compilers := TCompilerList(LoadConfig(ConfDir + 'fpclist.txt'));
  Result := Compilers;
end;

function GetDebuggers: TDebuggerList;
begin
  if Debuggers <> nil then exit(Debuggers);
  Debuggers := TDebuggerList(LoadConfig(ConfDir + 'gdblist.txt'));
  Result := Debuggers;
end;

{ TGDBMIDebuggerForTest }

procedure TGDBMIDebuggerForTest.EvalCallBack(Sender: TObject;
  ASuccess: Boolean; ResultText: String; ResultDBGType: TDBGType);
begin
  FEvalRes := ResultText;
  FEvalResType := ResultDBGType;
  FEvalDone := true;
end;

function TGDBMIDebuggerForTest.EvaluateWait(const AExpression: String;
  var ARes: String; var AResType: TDBGType; EvalFlags: TDBGEvaluateFlags
  ): Boolean;
begin
  FEvalDone := false;
  inherited Evaluate(AExpression, @EvalCallBack, EvalFlags);
  while not FEvalDone do begin
    Application.ProcessMessages;
    sleep(5);
  end;
  ARes := FEvalRes;
  AResType := FEvalResType;
end;

{ TGDBTestCase }

procedure TGDBTestCase.DoDbgOutPut(Sender: TObject; const AText: String);
begin
  //
end;

procedure TGDBTestCase.InternalDbgOutPut(Sender: TObject; const AText: String);
begin
  //LogToFile(AText);
  DoDbgOutPut(Sender, AText);
end;

function TGDBTestCase.GdbClass: TGDBMIDebuggerClass;
begin
  Result := TestGdbClass;
end;

procedure TGDBTestCase.DoDbgOut(Sender: TObject; S: string; var Handled: Boolean);
begin
  DoDebugln(Sender, '| '+S, Handled);
end;

procedure TGDBTestCase.DoDebugln(Sender: TObject; S: string; var Handled: Boolean);
begin
  inherited DoDebugln(Sender, S, Handled);

  if pos('(gdb)', s) > 0 then begin
    inc(FLogDebuglnCount);
    if FLogDebuglnCount mod 10 = 0 then begin
      TestLogger.DebugLn([FLogDebuglnCount]);
    end;
  end;
end;

function TGDBTestCase.GetWatches: TTestWatchesMonitor;
begin
  Result := Debugger.Watches;
end;

function TGDBTestCase.InternalFeedBack(Sender: TObject; const AText, AInfo: String;
  AType: TDBGFeedbackType; AButtons: TDBGFeedbackResults): TDBGFeedbackResult;
begin
  Result := frOk;
  DebugLn(['**** Feedback requested ****: ', AText]);
  DebugLn(['**** ', AInfo]);
end;

procedure TGDBTestCase.InternalDbgEvent(Sender: TObject;
  const ACategory: TDBGEventCategory; const AEventType: TDBGEventType;
  const AText: String);
begin
  case ACategory of
  	ecBreakpoint: ;
    ecProcess: ;
    ecThread: ;
    ecModule: ;
    ecOutput: ;
    ecWindows: ;
    ecDebugger: begin
      case AEventType of
      	etDefault: begin
          // maybe crash / internal error? Text from IDE not GDB (po file)
          if (Pos('internal error:', LowerCase(AText)) > 0) then
            inc(FTotalGDBInternalErrorCnt);
        end;
      end;
    end;
  end;
end;

function TGDBTestCase.GetCompilerInfo: TCompilerInfo;
begin
  Result := TCompilerSuite(Parent).CompilerInfo;
end;

function TGDBTestCase.GetDebuggerInfo: TDebuggerInfo;
begin
  Result := TCompilerSuite(Parent).DebuggerInfo;
end;

function TGDBTestCase.GetSymbolType: TSymbolType;
begin
  Result := TCompilerSuite(Parent).SymbolType;
end;

procedure TGDBTestCase.SetUp;
begin
  FLogDebuglnCount := 0;
  FTotalGDBInternalErrorCnt := 0;
  FTotalDsErrorCrash := 0;
  FTotalClassVsRecord := 0;
  FStartTime := GetTickCount64;
  inherited SetUp;
end;

function TGDBTestCase.GetFinalLogFileName: String;
var
  i: QWord;
begin
  Result := inherited GetFinalLogFileName;

  i := GetTickCount64;
  if i >= FStartTime then
    i := i - FStartTime
  else
    i := high(QWord) - FStartTime + 1 + i;

  if FTotalGDBInternalErrorCnt > 0
  then Result := Result + '.gdb_intern_'+IntToStr(FTotalGDBInternalErrorCnt);
  if FTotalDsErrorCrash > 0
  then Result := Result + '.gdb_crash_'+IntToStr(FTotalDsErrorCrash);
  if FTotalClassVsRecord > 0
  then Result := Result + '.class_rec_'+IntToStr(FTotalClassVsRecord);

  Result := Result + '.t_'+ IntToStr(i div 1000);
end;

function TGDBTestCase.StartGDB(AppDir, TestExeName: String): TGDBMIDebugger;
begin
  Result := GdbClass.Create(DebuggerInfo.ExeName);
  Result.OnDbgOutput  := @InternalDbgOutPut;
  Result.OnFeedback := @InternalFeedBack;
  Result.OnDbgEvent:=@InternalDbgEvent;

  Debugger.InitDebuggerMonitors(Result);

  Result.Init;
  if Result.State = dsError then
    Fail(' Failed Init');
  Result.WorkingDir := AppDir;
  Result.FileName   := TestExeName;
  Result.Arguments := '';
  Result.ShowConsole := True;

end;

procedure TGDBTestCase.CleanGdb;
begin
  Debugger.ClearDebuggerMonitors;
end;

procedure TGDBTestCase.LogToFile(const s: string);
begin
  LogText('## '+s);
end;

{ TCompilerList }

function TCompilerList.GetCompilerInfo(Index: Integer): TCompilerInfo;
begin
  Result := FullInfo[Index];
end;

{ TDebuggerList }

function TDebuggerList.GetDebuggerInfo(Index: Integer): TDebuggerInfo;
begin
  Result := FullInfo[Index];
end;

{ TCompilerSuite }

function TCompilerSuite.GetCompilerInfo: TExternalExeInfo;
begin
  Result := Compiler.FullInfo;
end;

function TCompilerSuite.GetDebuggerInfo: TExternalExeInfo;
begin
  Result := Debugger.FullInfo;
end;

function TCompilerSuite.GetSymbolType: TSymbolType;
begin
  Result := Compiler.SymbolType;
end;

constructor TCompilerSuite.Create;
begin
  inherited Create;
  FSymbolSwitch := SymbolTypeSwitches[SymbolType];
  FFileNameExt := SymbolTypeNames[SymbolType] + '_' + NameToFileName(CompilerInfo.Name);
end;


{ --- }

procedure BuildTestSuites;
var
  FpcList: TCompilerList;
  GdbList: TDebuggerList;
begin
  FpcList := GetCompilers;
  GdbList := GetDebuggers;

  CreateCompilerList(FpcList, TTestDbgCompiler);
  CreateDebuggerList(GdbList, TTestDbgDebugger);
  CreateTestSuites(TestDbgCompilerList, TestDbgDebuggerList, TCompilerSuite);

  TestControlRegisterCompilers(FpcList);
  TestControlRegisterDebuggers(GdbList);
end;

function CheckAppDir(var AppDir: string): Boolean;
begin
  Result := DirectoryExistsUTF8(AppDir + 'TestApps');
end;

function CheckAppDirLib(var AppDir: string): Boolean;
var
  s: string;
begin
  Result := False;
  if RightStr(AppDir, length('lib' + DirectorySeparator)) = 'lib' + DirectorySeparator
  then begin
    s := copy(AppDir, 1, length(AppDir) - length('lib' + DirectorySeparator));
    Result :=  DirectoryExistsUTF8(s + 'TestApps');
    if Result then
      AppDir := s;
  end;
end;

function AppDirStripAppBundle(AppDir: string): String;
var
  p: LongInt;
begin
  Result := AppDir;
  p := pos('.app' + DirectorySeparator, AppDir);
  while (p > 1) and (AppDir[p-1] <> DirectorySeparator) do
    dec(p);
  if p > 1 then
    Result := Copy(AppDir, 1, p - 1);
end;

initialization
  // GDBMIDebugger is un uses
  DebugLogger.FindOrRegisterLogGroup('DBG_CMD_ECHO' , True  )^.Enabled := True;
  DebugLogger.FindOrRegisterLogGroup('DBGMI_QUEUE_DEBUG' , True  )^.Enabled := True;
  DebugLogger.FindOrRegisterLogGroup('DBGMI_STRUCT_PARSER' , True  )^.Enabled := True;
  DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE'  , True  )^.Enabled := True;
  DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS', True )^.Enabled := True;
  DebugLogger.FindOrRegisterLogGroup('DBG_DISASSEMBLER', True  )^.Enabled := True;
  DebugLogger.FindOrRegisterLogGroup('DBGMI_TYPE_INFO', True  )^.Enabled := True;
  DebugLogger.FindOrRegisterLogGroup('DBGMI_TIMEOUT_DEBUG', True  )^.Enabled := True;

  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_ERRORS', True);
  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_SEARCH', True)^.Enabled := True;
  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_WARNINGS', True)^.Enabled := True;
  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_VERBOSE', True);
  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_DATA_WARNINGS', True);


  AppDir := AppendPathDelim(ExtractFilePath(Paramstr(0)));
  if  not(CheckAppDir(AppDir))
  and not(CheckAppDirLib(AppDir))
  then begin
    AppDir := AppDirStripAppBundle(AppDir);
    if  not(CheckAppDir(AppDir))
    and not(CheckAppDirLib(AppDir))
    then
      with TSelectDirectoryDialog.Create(nil) do begin
        if Execute then AppDir := AppendPathDelim(FileName);
        Free;
      end;
  end;
  ConfDir := AppDir;
  AppDir := AppendPathDelim(AppDir + 'TestApps');

  if DirectoryExistsUTF8(ConfDir+'logs') then
    TestControlSetLogPath(ConfDir+'logs'+DirectorySeparator)
  else if DirectoryExistsUTF8(ConfDir+'log') then
    TestControlSetLogPath(ConfDir+'log'+DirectorySeparator)
  else
    TestControlSetLogPath(ConfDir);


  //EnvironmentOptions := TEnvironmentOptions.Create;
  //with EnvironmentOptions do
  //begin
  //  CreateConfig;
  //  Load(false);
  //end;
  //GlobalMacroList:=TTransferMacroList.Create;

  BuildTestSuites;

finalization
  FreeAndNil(Compilers);
  FreeAndNil(Debuggers);
  //FreeAndNil(EnvironmentOptions);

end.

