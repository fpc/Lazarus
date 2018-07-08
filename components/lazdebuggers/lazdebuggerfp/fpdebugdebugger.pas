unit FpDebugDebugger;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Maps,
  process,
  LazLogger,
  Dialogs,
  FpDbgClasses,
  FpDbgInfo,
  contnrs,
  FpErrorMessages,
  FpPascalBuilder,
  DbgIntfBaseTypes,
  DbgIntfDebuggerBase,
  FpdMemoryTools,
  FpPascalParser,
  FPDbgController, FpDbgDwarfDataClasses;

type

  { TFpDebugThread }
  TFpDebugDebugger = class;
  TFpDbgAsyncMethod = procedure() of object;

  TFpDebugThread = class(TThread)
  private
    FAsyncMethod: TFpDbgAsyncMethod;
    FDebugLoopStoppedEvent: PRTLEvent;
    FFpDebugDebugger: TFpDebugDebugger;
    FStartDebugLoopEvent: PRTLEvent;
    FStartSuccessfull: boolean;
    FQueuedFinish: boolean;  // true = DoDebugLoopFinishedASync queud in main thread
    procedure DoDebugLoopFinishedASync({%H-}Data: PtrInt);
  public
    constructor Create(AFpDebugDebugger: TFpDebugDebugger);
    destructor Destroy; override;
    procedure Execute; override;
    property StartSuccesfull: boolean read FStartSuccessfull;
    property StartDebugLoopEvent: PRTLEvent read FStartDebugLoopEvent;
    property DebugLoopStoppedEvent: PRTLEvent read FDebugLoopStoppedEvent;
    property AsyncMethod: TFpDbgAsyncMethod read FAsyncMethod write FAsyncMethod;
  end;

  { TFpDbgLogMessage }

  TFpDbgLogMessage = class
  public
    SyncLogMessage: string;
    SyncLogLevel: TFPDLogLevel;
  end;

  { TFpDebugDebuggerProperties }

  TFpDebugDebuggerProperties = class(TDebuggerProperties)
  private
    FConsoleTty: string;
    FNextOnlyStopOnStartLine: boolean;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    {$ifdef unix}
  published
    {$endif unix}
    property ConsoleTty: string read FConsoleTty write FConsoleTty;
  published
    property NextOnlyStopOnStartLine: boolean read FNextOnlyStopOnStartLine write FNextOnlyStopOnStartLine;
  end;

  { TFpDebugDebugger }

  TFpDebugDebugger = class(TDebuggerIntf)
  private
    FWatchEvalList: TFPList; // Schedule
    FWatchAsyncQueued: Boolean;
    FPrettyPrinter: TFpPascalPrettyPrinter;
    FDbgController: TDbgController;
    FFpDebugThread: TFpDebugThread;
    FQuickPause: boolean;
    FRaiseExceptionBreakpoint: TFpInternalBreakpoint;
    FDbgLogMessageList: TFPObjectList;
    FLogCritSection: TRTLCriticalSection;
    FMemConverter: TFpDbgMemConvertorLittleEndian;
    FMemReader: TDbgMemReader;
    FMemManager: TFpDbgMemManager;
    FConsoleOutputThread: TThread;
    FReleaseLock: Integer;
    FReleaseNeeded: Boolean;
    {$ifdef linux}
    FCacheLine: cardinal;
    FCacheFileName: string;
    FCacheBreakpoint: TFpInternalBreakpoint;
    FCacheLocation: TDBGPtr;
    FCacheBoolean: boolean;
    FCachePointer: pointer;
    {$endif linux}
    function GetClassInstanceName(AnAddr: TDBGPtr): string;
    function ReadAnsiString(AnAddr: TDbgPtr): string;
    function SetSoftwareExceptionBreakpoint: boolean;
    procedure HandleSoftwareException(out AnExceptionLocation: TDBGLocationRec; var continue: boolean);
    procedure FreeDebugThread;
    procedure FDbgControllerHitBreakpointEvent(var continue: boolean; const Breakpoint: TFpInternalBreakpoint);
    procedure FDbgControllerCreateProcessEvent(var {%H-}continue: boolean);
    procedure FDbgControllerProcessExitEvent(AExitCode: DWord);
    procedure FDbgControllerExceptionEvent(var continue: boolean; const ExceptionClass, ExceptionMessage: string);
    procedure FDbgControllerDebugInfoLoaded(Sender: TObject);
    function GetDebugInfo: TDbgInfo;
    procedure DoWatchFreed(Sender: TObject);
    procedure ProcessASyncWatches({%H-}Data: PtrInt);
    procedure DoLog();
    procedure IncReleaseLock;
    procedure DecReleaseLock;
  protected
    procedure ScheduleWatchValueEval(AWatchValue: TWatchValue);
    function EvaluateExpression(AWatchValue: TWatchValue;
                                AExpression: String;
                                out AResText: String;
                                out ATypeInfo: TDBGType;
                                EvalFlags: TDBGEvaluateFlags = []): Boolean;

    function CreateLineInfo: TDBGLineInfo; override;
    function CreateWatches: TWatchesSupplier; override;
    function CreateThreads: TThreadsSupplier; override;
    function CreateLocals: TLocalsSupplier; override;
    function  CreateRegisters: TRegisterSupplier; override;
    function CreateCallStack: TCallStackSupplier; override;
    function CreateDisassembler: TDBGDisassembler; override;
    function CreateBreakPoints: TDBGBreakPoints; override;
    function  RequestCommand(const ACommand: TDBGCommand;
                             const AParams: array of const;
                             const ACallback: TMethod): Boolean; override;
    function ChangeFileName: Boolean; override;

    procedure OnLog(const AString: string; const ALogLevel: TFPDLogLevel);
    // On Linux, communication with the debuggee is only allowed from within
    // the thread that created the debuggee. So a method to execute functions
    // within the debug-thread is necessary.
    procedure ExecuteInDebugThread(AMethod: TFpDbgAsyncMethod);
    procedure StartDebugLoop;
    procedure DebugLoopFinished;
    procedure QuickPause;
    procedure DoRelease; override;
    procedure DoState(const OldState: TDBGState); override;
    {$ifdef linux}
    procedure DoAddBreakLine;
    procedure DoAddBreakLocation;
    procedure DoReadData;
    procedure DoPrepareCallStackEntryList;
    procedure DoFreeBreakpoint;
    {$endif linux}
    function AddBreak(const ALocation: TDbgPtr): TFpInternalBreakpoint; overload;
    function AddBreak(const AFileName: String; ALine: Cardinal): TFpInternalBreakpoint; overload;
    procedure FreeBreakpoint(const ABreakpoint: TFpInternalBreakpoint);
    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean;
    function ReadAddress(const AAdress: TDbgPtr; out AData: TDBGPtr): Boolean;
    procedure PrepareCallStackEntryList;

    property DebugInfo: TDbgInfo read GetDebugInfo;
  public
    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;
    function GetLocationRec(AnAddress: TDBGPtr=0): TDBGLocationRec;
    function GetLocation: TDBGLocationRec; override;
    class function Caption: String; override;
    class function NeedsExePath: boolean; override;
    class function CanExternalDebugSymbolsFile: boolean; override;
    class function CreateProperties: TDebuggerProperties; override;
    function  GetSupportedCommands: TDBGCommands; override;
  end;

  { TFpLineInfo }

  TFpLineInfo = class(TDBGLineInfo) //class(TGDBMILineInfo)
  private
    FRequestedSources: TStringList;
  protected
    function  FpDebugger: TFpDebugDebugger;
    procedure DoStateChange(const {%H-}AOldState: TDBGState); override;
    procedure ClearSources;
    procedure DebugInfoChanged;
  public
    constructor Create(const ADebugger: TDebuggerIntf);
    destructor Destroy; override;
    function Count: Integer; override;
    function HasAddress(const AIndex: Integer; const ALine: Integer): Boolean; override;
    function GetInfo({%H-}AAddress: TDbgPtr; out {%H-}ASource, {%H-}ALine, {%H-}AOffset: Integer): Boolean; override;
    function IndexOf(const ASource: String): integer; override;
    procedure Request(const ASource: String); override;
    procedure Cancel(const {%H-}ASource: String); override;
  end;

  { TFPWatches }

  TFPWatches = class(TWatchesSupplier)
  protected
    function  FpDebugger: TFpDebugDebugger;
    //procedure DoStateChange(const AOldState: TDBGState); override;
    procedure InternalRequestData(AWatchValue: TWatchValue); override;
  public
  end;

  { TFPCallStackSupplier }

  TFPCallStackSupplier = class(TCallStackSupplier)
  private
    FPrettyPrinter: TFpPascalPrettyPrinter;
  protected
    function  FpDebugger: TFpDebugDebugger;
    procedure DoStateLeavePause; override;
  public
    constructor Create(const ADebugger: TDebuggerIntf);
    destructor Destroy; override;
    procedure RequestCount(ACallstack: TCallStackBase); override;
    procedure RequestEntries(ACallstack: TCallStackBase); override;
    procedure RequestCurrent(ACallstack: TCallStackBase); override;
    procedure UpdateCurrentIndex; override;
  end;

  { TFPLocals }

  TFPLocals = class(TLocalsSupplier)
  private
    FPrettyPrinter: TFpPascalPrettyPrinter;
  protected
    function  FpDebugger: TFpDebugDebugger;
  public
    procedure RequestData(ALocals: TLocals); override;
    constructor Create(const ADebugger: TDebuggerIntf);
    destructor Destroy; override;
  end;

  { TFPRegisters }

  TFPRegisters = class(TRegisterSupplier)
  public
    procedure RequestData(ARegisters: TRegisters); override;
  end;

  { TFPThreads }

  TFPThreads = class(TThreadsSupplier)
  protected
    procedure DoStateEnterPause; override;
  public
    procedure RequestMasterData; override;
  end;

  { TFPDBGDisassembler }

  TFPDBGDisassembler = class(TDBGDisassembler)
  protected
    function PrepareEntries(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): boolean; override;
  end;

  { TFPBreakpoint }

  TFPBreakpoint = class(TDBGBreakPoint)
  private
    FSetBreakFlag: boolean;
    FResetBreakFlag: boolean;
    FInternalBreakpoint: FpDbgClasses.TFpInternalBreakpoint;
    FIsSet: boolean;
    procedure SetBreak;
    procedure ResetBreak;
  protected
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure DoEnableChange; override;
    procedure DoChanged; override;
  public
    destructor Destroy; override;
  end;

  { TFPBreakpoints }

  TFPBreakpoints = class(TDBGBreakPoints)
  private
    FDelayedRemoveBreakpointList: TObjectList;
  protected
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure AddBreakpointToDelayedRemoveList(ABreakpoint: FpDbgClasses.TFpInternalBreakpoint);
  public
    constructor Create(const ADebugger: TDebuggerIntf; const ABreakPointClass: TDBGBreakPointClass);
    destructor Destroy; override;
    function Find(AIntBReakpoint: FpDbgClasses.TFpInternalBreakpoint): TDBGBreakPoint;
  end;

procedure Register;

implementation

uses
  FpDbgUtil,
  FpDbgDisasX86;

type

  { TFpDbgMemReader }

  TFpDbgMemReader = class(TDbgMemReader)
  private
    FFpDebugDebugger: TFpDebugDebugger;
  protected
    function GetDbgProcess: TDbgProcess; override;
    function GetDbgThread(AContext: TFpDbgAddressContext): TDbgThread; override;
  public
    constructor create(AFpDebugDebuger: TFpDebugDebugger);
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
    function ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
  end;

  { TFpWaitForConsoleOutputThread }

  TFpWaitForConsoleOutputThread = class(TThread)
  private
    FFpDebugDebugger: TFpDebugDebugger;
    FHasConsoleOutputQueued: PRTLEvent;
    procedure DoHasConsoleOutput(Data: PtrInt);
  public
    constructor Create(AFpDebugDebugger: TFpDebugDebugger);
    destructor Destroy; override;
    procedure Execute; override;
  end;


procedure Register;
begin
  RegisterDebugger(TFpDebugDebugger);
end;

{ TFPThreads }

procedure TFPThreads.DoStateEnterPause;
begin
  inherited DoStateEnterPause;
  Changed;
end;

procedure TFPThreads.RequestMasterData;
var
  ThreadArray: TFPDThreadArray;
  ThreadEntry: TThreadEntry;
  CallStack: TDbgCallstackEntryList;
  i: Integer;
  FunctionName, SourceFile, State: String;
  AnAddress: TDBGPtr;
  Line: LongInt;
begin
  if Monitor = nil then exit;
  if CurrentThreads = nil then exit;

  if Debugger = nil then Exit;

  CurrentThreads.Clear;

  if not (Debugger.State in [dsPause, dsInternalPause, dsRun]) then Exit;

  ThreadArray := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.GetThreadArray;
  for i := 0 to high(ThreadArray) do
    begin
    CallStack := ThreadArray[i].CallStackEntryList;
    if ThreadArray[i].ID = TFpDebugDebugger(Debugger).FDbgController.CurrentThread.ID then
      State := 'stopped'
    else
      State := 'running';
    if Assigned(CallStack) and (CallStack.Count > 0) then
      begin
      AnAddress := CallStack.Items[0].AnAddress;
      FunctionName := CallStack.Items[0].FunctionName;
      SourceFile := CallStack.Items[0].SourceFile;
      Line := CallStack.Items[0].Line;
      end
    else
      begin
      AnAddress := 0;
      FunctionName := '';
      SourceFile := '';
      Line := 0;
      end;
    ThreadEntry := CurrentThreads.CreateEntry(
      AnAddress,
      nil,
      FunctionName,
      SourceFile,
      '',
      Line,
      ThreadArray[i].ID,
      'Thread ' + IntToStr(ThreadArray[i].ID),
      State);
    try
      CurrentThreads.Add(ThreadEntry);
    finally
      ThreadEntry.Free;
    end;
    end;

  CurrentThreads.CurrentThreadId := TFpDebugDebugger(Debugger).FDbgController.CurrentThread.ID;
  CurrentThreads.SetValidity(ddsValid);
end;

{ TFpDebugDebuggerProperties }

constructor TFpDebugDebuggerProperties.Create;
begin
  inherited Create;
  FNextOnlyStopOnStartLine:=true;
end;

procedure TFpDebugDebuggerProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TFpDebugDebuggerProperties then begin
    FNextOnlyStopOnStartLine := TFpDebugDebuggerProperties(Source).NextOnlyStopOnStartLine;
    FConsoleTty:=TFpDebugDebuggerProperties(Source).ConsoleTty;
  end;
end;

{ TFpWaitForConsoleOutputThread }

procedure TFpWaitForConsoleOutputThread.DoHasConsoleOutput(Data: PtrInt);
var
  s: string;
begin
  if (Data=0) or assigned(TFpDebugDebugger(Data).FConsoleOutputThread) then
  begin
    s := FFpDebugDebugger.FDbgController.CurrentProcess.GetConsoleOutput;
    RTLeventSetEvent(FHasConsoleOutputQueued);
    FFpDebugDebugger.OnConsoleOutput(self, s);
  end;
end;

constructor TFpWaitForConsoleOutputThread.Create(AFpDebugDebugger: TFpDebugDebugger);
begin
  Inherited create(false);
  FHasConsoleOutputQueued := RTLEventCreate;
  FFpDebugDebugger := AFpDebugDebugger;
end;

destructor TFpWaitForConsoleOutputThread.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  RTLeventdestroy(FHasConsoleOutputQueued);
  inherited Destroy;
end;

procedure TFpWaitForConsoleOutputThread.Execute;
var
  res: integer;
begin
  while not terminated do
  begin
    res := FFpDebugDebugger.FDbgController.CurrentProcess.CheckForConsoleOutput(100);
    if res<0 then
      Terminate
    else if res>0 then
    begin
      RTLeventResetEvent(FHasConsoleOutputQueued);
      Application.QueueAsyncCall(@DoHasConsoleOutput, PtrInt(FFpDebugDebugger));
      RTLeventWaitFor(FHasConsoleOutputQueued);
    end;
  end;
end;

{ TFpDbgMemReader }

function TFpDbgMemReader.GetDbgProcess: TDbgProcess;
begin
  result := FFpDebugDebugger.FDbgController.CurrentProcess;
end;

function TFpDbgMemReader.GetDbgThread(AContext: TFpDbgAddressContext): TDbgThread;
var
  Process: TDbgProcess;
begin
  Process := GetDbgProcess;
  if not Process.GetThread(AContext.ThreadId, Result) then
    Result := FFpDebugDebugger.FDbgController.CurrentThread;
end;

constructor TFpDbgMemReader.create(AFpDebugDebuger: TFpDebugDebugger);
begin
  FFpDebugDebugger := AFpDebugDebuger;
end;

function TFpDbgMemReader.ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean;
begin
  result := FFpDebugDebugger.ReadData(AnAddress, ASize, ADest^);
end;

function TFpDbgMemReader.ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean;
begin
  Assert(AnAddressSpace>0,'TFpDbgMemReader.ReadMemoryEx ignores AddressSpace');
  result := FFpDebugDebugger.ReadData(AnAddress, ASize, ADest^);
end;

{ TFPCallStackSupplier }

function TFPCallStackSupplier.FpDebugger: TFpDebugDebugger;
begin
  Result := TFpDebugDebugger(Debugger);
end;

procedure TFPCallStackSupplier.DoStateLeavePause;
begin
  if (TFpDebugDebugger(Debugger).FDbgController <> nil) and
     (TFpDebugDebugger(Debugger).FDbgController.CurrentThread <> nil)
  then
    TFpDebugDebugger(Debugger).FDbgController.CurrentThread.ClearCallStack;
  inherited DoStateLeavePause;
end;

constructor TFPCallStackSupplier.Create(const ADebugger: TDebuggerIntf);
begin
  inherited Create(ADebugger);
  FPrettyPrinter := TFpPascalPrettyPrinter.Create(sizeof(pointer));
end;

destructor TFPCallStackSupplier.Destroy;
begin
  inherited Destroy;
  FPrettyPrinter.Free;
end;

procedure TFPCallStackSupplier.RequestCount(ACallstack: TCallStackBase);
var
  ThreadCallStack: TDbgCallstackEntryList;
begin
  if (Debugger = nil) or not(Debugger.State = dsPause)
  then begin
    ACallstack.SetCountValidity(ddsInvalid);
    exit;
  end;
  TFpDebugDebugger(Debugger).PrepareCallStackEntryList;
  ThreadCallStack := TFpDebugDebugger(Debugger).FDbgController.CurrentThread.CallStackEntryList;
  if ThreadCallStack = nil then
    exit;

  if ThreadCallStack.Count = 0 then
  begin
    ACallstack.SetCountValidity(ddsInvalid);
    ACallstack.SetHasAtLeastCountInfo(ddsInvalid);
  end
  else
  begin
    ACallstack.Count := ThreadCallStack.Count;
    ACallstack.SetCountValidity(ddsValid);
  end;
end;

procedure TFPCallStackSupplier.RequestEntries(ACallstack: TCallStackBase);
var
  e: TCallStackEntry;
  It: TMapIterator;
  ThreadCallStack: TDbgCallstackEntryList;
  v, params: String;
  i: Integer;
  ProcVal, m: TFpDbgValue;
  RegList: TDbgRegisterValueList;
  Reg: TDbgRegisterValue;
  AController: TDbgController;
  CurThreadId: Integer;
  AContext: TFpDbgInfoContext;
  OldContext: TFpDbgAddressContext;
begin
  It := TMapIterator.Create(ACallstack.RawEntries);
  //TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.MainThread.PrepareCallStackEntryList;
  //CurThreadId := FpDebugger.Threads.CurrentThreads.CurrentThreadId;
  //ThreadCallStack := FpDebugger.Threads.CurrentThreads.Entries[CurThreadId].CallStackEntryList;

  CurThreadId := FpDebugger.FDbgController.CurrentThread.ID;
  ThreadCallStack := FpDebugger.FDbgController.CurrentThread.CallStackEntryList;

  if not It.Locate(ACallstack.LowestUnknown )
  then if not It.EOM
  then It.Next;

  AController := FpDebugger.FDbgController;
  OldContext := FpDebugger.FMemManager.DefaultContext;

  while (not IT.EOM) and (TCallStackEntry(It.DataPtr^).Index < ACallstack.HighestUnknown)
  do begin
    e := TCallStackEntry(It.DataPtr^);
    if e.Validity = ddsRequested then
    begin
      if ThreadCallStack[e.Index].ProcSymbol <> nil then
        ProcVal := ThreadCallStack[e.Index].ProcSymbol.Value;

      params := '';
      if (ProcVal <> nil) then begin
        if e.Index = 0 then
          RegList := AController.CurrentThread.RegisterValueList
        else
          RegList := ThreadCallStack[e.Index].RegisterValueList;
        if AController.CurrentProcess.Mode=dm32 then
          Reg := RegList.FindRegisterByDwarfIndex(8)
        else
          Reg := RegList.FindRegisterByDwarfIndex(16);
        if Reg <> nil then begin
          AContext := AController.CurrentProcess.DbgInfo.FindContext(CurThreadId, e.Index, Reg.NumValue);
          if AContext <> nil then begin
            AContext.MemManager.DefaultContext := AContext;
            FPrettyPrinter.AddressSize := AContext.SizeOfAddress;

            for i := 0 to ProcVal.MemberCount - 1 do begin
              m := ProcVal.Member[i];
              if (m <> nil) and (sfParameter in m.DbgSymbol.Flags) then begin
                FPrettyPrinter.PrintValue(v, m, wdfDefault, -1, [ppoStackParam]);
                if params <> '' then params := params + ', ';
                params := params + v;
              end;
            end;
            AContext.ReleaseReference;
          end;
        end;
      end;
      if params <> '' then
        params := '(' + params + ')';
      e.Init(ThreadCallStack[e.Index].AnAddress, nil,
        ThreadCallStack[e.Index].FunctionName+params, ThreadCallStack[e.Index].SourceFile,
        '', ThreadCallStack[e.Index].Line, ddsValid);
    end;
    It.Next;
  end;
  It.Free;
  FpDebugger.FMemManager.DefaultContext := OldContext;
end;

procedure TFPCallStackSupplier.RequestCurrent(ACallstack: TCallStackBase);
begin
  ACallstack.CurrentIndex := 0;
  ACallstack.SetCurrentValidity(ddsValid);
end;

procedure TFPCallStackSupplier.UpdateCurrentIndex;
var
  tid, idx: Integer;
  cs: TCallStackBase;
begin
  if (Debugger = nil) or not(Debugger.State = dsPause) then begin
    exit;
  end;

  tid := Debugger.Threads.CurrentThreads.CurrentThreadId;
  cs := TCallStackBase(CurrentCallStackList.EntriesForThreads[tid]);
  idx := cs.NewCurrentIndex;  // NEW-CURRENT

  if cs <> nil then begin
    cs.CurrentIndex := idx;
    cs.SetCurrentValidity(ddsValid);
  end;
end;

{ TFPLocals }

function TFPLocals.FpDebugger: TFpDebugDebugger;
begin
  Result := TFpDebugDebugger(Debugger);
end;

procedure TFPLocals.RequestData(ALocals: TLocals);
var
  AContext: TFpDbgInfoContext;
  AController: TDbgController;
  ProcVal: TFpDbgValue;
  i: Integer;
  m: TFpDbgValue;
  n, v: String;
  CurThreadId, CurStackFrame: Integer;
  AFrame: TDbgCallstackEntry;
  RegList: TDbgRegisterValueList;
  Reg: TDbgRegisterValue;
  CurStackList: TCallStackBase;
begin
  AController := FpDebugger.FDbgController;
  if (AController = nil) or (AController.CurrentProcess = nil) or
     (AController.CurrentProcess.DbgInfo = nil)
  then begin
    ALocals.SetDataValidity(ddsInvalid);
    exit;
  end;

  CurThreadId := Debugger.Threads.CurrentThreads.CurrentThreadId;
  CurStackList := Debugger.CallStack.CurrentCallStackList.EntriesForThreads[CurThreadId];
  if CurStackList <> nil then
    CurStackFrame := Debugger.CallStack.CurrentCallStackList.EntriesForThreads[CurThreadId].CurrentIndex
  else
    CurStackFrame := 0;

  if CurStackFrame > 0 then
    begin
    AController.CurrentThread.PrepareCallStackEntryList(CurStackFrame);
    AFrame := AController.CurrentThread.CallStackEntryList[CurStackFrame];
    if AFrame = nil then
      begin
      ALocals.SetDataValidity(ddsInvalid);
      exit;
      end;
    RegList := AFrame.RegisterValueList;
    end
  else
    RegList := AController.CurrentThread.RegisterValueList;
  if AController.CurrentProcess.Mode=dm32 then
    Reg := RegList.FindRegisterByDwarfIndex(8)
  else
    Reg := RegList.FindRegisterByDwarfIndex(16);
  if Reg <> nil then
    AContext := AController.CurrentProcess.DbgInfo.FindContext(CurThreadId, CurStackFrame, Reg.NumValue)
  else
    AContext := nil;

  if (AContext = nil) or (AContext.SymbolAtAddress = nil) then begin
    ALocals.SetDataValidity(ddsInvalid);
    AContext.ReleaseReference;
    exit;
  end;

  ProcVal := AContext.ProcedureAtAddress;

  if (ProcVal = nil) then begin
    ALocals.SetDataValidity(ddsInvalid);
    AContext.ReleaseReference;
    exit;
  end;
  FPrettyPrinter.AddressSize := AContext.SizeOfAddress;

  ALocals.Clear;
  for i := 0 to ProcVal.MemberCount - 1 do begin
    m := ProcVal.Member[i];
    if m <> nil then begin
      if m.DbgSymbol <> nil then
        n := m.DbgSymbol.Name
      else
        n := '';
      FPrettyPrinter.PrintValue(v, m);
      ALocals.Add(n, v);
    end;
  end;
  ALocals.SetDataValidity(ddsValid);
  AContext.ReleaseReference;
end;

constructor TFPLocals.Create(const ADebugger: TDebuggerIntf);
begin
  inherited Create(ADebugger);
  FPrettyPrinter := TFpPascalPrettyPrinter.Create(sizeof(pointer));
end;

destructor TFPLocals.Destroy;
begin
  inherited Destroy;
  FPrettyPrinter.Free;
end;

{ TFPBreakpoints }

procedure TFPBreakpoints.DoStateChange(const AOldState: TDBGState);
var
  ABrkPoint: FpDbgClasses.TFpInternalBreakpoint;
  i: Integer;
begin
  inherited DoStateChange(AOldState);
  if Debugger.State in [dsPause, dsInternalPause] then
  begin
    if FDelayedRemoveBreakpointList.Count>0 then
      for i := FDelayedRemoveBreakpointList.Count-1 downto 0 do
      begin
        ABrkPoint := FpDbgClasses.TFpInternalBreakpoint(FDelayedRemoveBreakpointList[i]);
        TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.RemoveBreak(ABrkPoint);
        TFpDebugDebugger(Debugger).FreeBreakpoint(ABrkPoint);
        ABrkPoint := nil;
        FDelayedRemoveBreakpointList.Delete(i);
      end;
  end;
end;

procedure TFPBreakpoints.AddBreakpointToDelayedRemoveList(ABreakpoint: FpDbgClasses.TFpInternalBreakpoint);
begin
  FDelayedRemoveBreakpointList.Add(ABreakpoint);
end;

constructor TFPBreakpoints.Create(const ADebugger: TDebuggerIntf; const ABreakPointClass: TDBGBreakPointClass);
begin
  inherited create(ADebugger, ABreakPointClass);
  FDelayedRemoveBreakpointList := TObjectList.Create(false);
end;

destructor TFPBreakpoints.Destroy;
begin
  FDelayedRemoveBreakpointList.Free;
  inherited Destroy;
end;

function TFPBreakpoints.Find(AIntBReakpoint: FpDbgClasses.TFpInternalBreakpoint): TDBGBreakPoint;
var
  i: integer;
begin
  for i := 0 to count-1 do
    if TFPBreakpoint(Items[i]).FInternalBreakpoint=AIntBReakpoint then
      begin
      result := TFPBreakpoint(Items[i]);
      Exit;
      end;
  result := nil;
end;

procedure TFPBreakpoint.SetBreak;
begin
  assert(FInternalBreakpoint=nil);
  case Kind of
    bpkAddress:   FInternalBreakpoint := TFpDebugDebugger(Debugger).AddBreak(Address);
    bpkSource:    FInternalBreakpoint := TFpDebugDebugger(Debugger).AddBreak(Source, cardinal(Line));
  else
    Raise Exception.Create('Breakpoints of this kind are not suported.');
  end;
  FIsSet:=true;
  if not assigned(FInternalBreakpoint) then
    FValid:=vsInvalid
  else
    FValid:=vsValid;
end;

procedure TFPBreakpoint.ResetBreak;
begin
  // If Debugger is not assigned, the Controller's currentprocess is already
  // freed. And so are the corresponding InternalBreakpoint's.
  if assigned(Debugger) and assigned(FInternalBreakpoint) then
    begin
    TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.RemoveBreak(FInternalBreakpoint);
    TFpDebugDebugger(Debugger).FreeBreakpoint(FInternalBreakpoint);
    FInternalBreakpoint := nil;
    end;
  FIsSet:=false;
end;

destructor TFPBreakpoint.Destroy;
begin
  if assigned(Debugger) and (Debugger.State = dsRun) and assigned(FInternalBreakpoint) then
    begin
    TFPBreakpoints(Collection).AddBreakpointToDelayedRemoveList(FInternalBreakpoint);
    FInternalBreakpoint:=nil;
    TFpDebugDebugger(Debugger).QuickPause;
    end
  else
    ResetBreak;
  inherited Destroy;
end;

procedure TFPBreakpoint.DoStateChange(const AOldState: TDBGState);
begin
  if (Debugger.State in [dsPause, dsInternalPause]) then
    begin
    if Enabled and not FIsSet then
      begin
      FSetBreakFlag:=true;
      Changed;
      end
    else if not enabled and FIsSet then
      begin
      FResetBreakFlag:=true;
      Changed;
      end;
    end
  else if Debugger.State = dsStop then
    begin
    TFpDebugDebugger(Debugger).FreeBreakpoint(FInternalBreakpoint);
    FInternalBreakpoint := nil;
    FIsSet:=false;
    end;
  inherited DoStateChange(AOldState);
end;

procedure TFPBreakpoint.DoEnableChange;
var
  ADebugger: TFpDebugDebugger;
begin
  ADebugger := TFpDebugDebugger(Debugger);
  if (ADebugger.State in [dsPause, dsInit]) then
    begin
    if Enabled and not FIsSet then
      FSetBreakFlag := True
    else if not Enabled and FIsSet then
      FResetBreakFlag := True;
    end
  else if (ADebugger.State = dsRun) and ((Enabled and not FIsSet) or (not Enabled and FIsSet)) then
    ADebugger.QuickPause;
  inherited;
end;

procedure TFPBreakpoint.DoChanged;
begin
  if FResetBreakFlag and not FSetBreakFlag then
    ResetBreak
  else if FSetBreakFlag then
    SetBreak;

  FSetBreakFlag := false;
  FResetBreakFlag := false;

  inherited DoChanged;
end;

{ TFPDBGDisassembler }

function TFPDBGDisassembler.PrepareEntries(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): boolean;
var
  ARange: TDBGDisassemblerEntryRange;
  AnEntry: TDisassemblerEntry;
  CodeBin: array[0..20] of byte;
  p: pointer;
  ADump,
  AStatement,
  ASrcFileName: string;
  ASrcFileLine: integer;
  i,j: Integer;
  Sym: TFpDbgSymbol;
  StatIndex: integer;
  FirstIndex: integer;
  ALastAddr: TDBGPtr;

begin
  Result := False;
  if (Debugger = nil) or not(Debugger.State = dsPause) then
    exit;

  Sym:=nil;
  ASrcFileLine:=0;
  ASrcFileName:='';
  StatIndex:=0;
  FirstIndex:=0;
  ARange := TDBGDisassemblerEntryRange.Create;
  ARange.RangeStartAddr:=AnAddr;
  ALastAddr:=0;

  Assert(ALinesBefore<>0,'TFPDBGDisassembler.PrepareEntries LinesBefore not supported');

  for i := 0 to ALinesAfter-1 do
    begin
    if not TFpDebugDebugger(Debugger).ReadData(AnAddr,sizeof(CodeBin),CodeBin) then
      begin
      DebugLn(Format('Disassemble: Failed to read memory at %s.', [FormatAddress(AnAddr)]));
      inc(AnAddr);
      end
    else
      begin
      p := @CodeBin;
      FpDbgDisasX86.Disassemble(p, TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.Mode=dm64, ADump, AStatement);

      Sym := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.FindSymbol(AnAddr);

      // If this is the last statement for this source-code-line, fill the
      // SrcStatementCount from the prior statements.
      if (assigned(sym) and ((ASrcFileName<>sym.FileName) or (ASrcFileLine<>sym.Line))) or
        (not assigned(sym) and ((ASrcFileLine<>0) or (ASrcFileName<>''))) then
        begin
        for j := 0 to StatIndex-1 do
          ARange.EntriesPtr[FirstIndex+j]^.SrcStatementCount:=StatIndex;
        StatIndex:=0;
        FirstIndex:=i;
        end;

      if assigned(sym) then
        begin
        ASrcFileName:=sym.FileName;
        ASrcFileLine:=sym.Line;
        sym.ReleaseReference;
        end
      else
        begin
        ASrcFileName:='';
        ASrcFileLine:=0;
        end;
      AnEntry.Addr := AnAddr;
      AnEntry.Dump := ADump;
      AnEntry.Statement := AStatement;
      AnEntry.SrcFileLine:=ASrcFileLine;
      AnEntry.SrcFileName:=ASrcFileName;
      AnEntry.SrcStatementIndex:=StatIndex;
      ARange.Append(@AnEntry);
      ALastAddr:=AnAddr;
      inc(StatIndex);
      Inc(AnAddr, {%H-}PtrUInt(p) - {%H-}PtrUInt(@CodeBin));
      end;
    end;

  if ARange.Count>0 then
    begin
    ARange.RangeEndAddr:=ALastAddr;
    ARange.LastEntryEndAddr:={%H-}TDBGPtr(p);
    EntryRanges.AddRange(ARange);
    result := true;
    end
  else
    begin
    result := false;
    ARange.Free;
    end;
end;

{ TFPRegisters }

procedure TFPRegisters.RequestData(ARegisters: TRegisters);
var
  ARegisterList: TDbgRegisterValueList;
  i: Integer;
  ARegisterValue: TRegisterValue;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsStop]) then
    exit;

  ARegisterList := TFpDebugDebugger(Debugger).FDbgController.CurrentThread.RegisterValueList;
  for i := 0 to ARegisterList.Count-1 do
    begin
    ARegisterValue := ARegisters.EntriesByName[ARegisterList[i].Name];
    ARegisterValue.ValueObj.SetAsNum(ARegisterList[i].NumValue, ARegisterList[i].Size);
    ARegisterValue.ValueObj.SetAsText(ARegisterList[i].StrValue);
    ARegisterValue.DataValidity:=ddsValid;
    end;
  ARegisters.DataValidity:=ddsValid;
end;

{ TFpLineInfo }

function TFpLineInfo.FpDebugger: TFpDebugDebugger;
begin
  Result := TFpDebugDebugger(Debugger);
end;

procedure TFpLineInfo.DoStateChange(const AOldState: TDBGState);
begin
  //inherited DoStateChange(AOldState);
  if not (Debugger.State in [dsPause, dsInternalPause, dsRun]) then
    ClearSources;
end;

procedure TFpLineInfo.ClearSources;
begin
  FRequestedSources.Clear;
end;

procedure TFpLineInfo.DebugInfoChanged;
var
  i: Integer;
  Src: String;
begin
  if (FpDebugger.DebugInfo = nil) or not(FpDebugger.DebugInfo is TFpDwarfInfo) then
    exit;

  for i := 0 to FRequestedSources.Count - 1 do begin
    if FRequestedSources.Objects[i] = nil then begin
      Src := FRequestedSources[i];
      FRequestedSources.Objects[i] := TObject(TFpDwarfInfo(FpDebugger.DebugInfo).GetLineAddressMap(Src));
      if FRequestedSources.Objects[i] <> nil then
        DoChange(Src);
    end;
  end;
end;

constructor TFpLineInfo.Create(const ADebugger: TDebuggerIntf);
begin
  FRequestedSources := TStringList.Create;
  inherited Create(ADebugger);
end;

destructor TFpLineInfo.Destroy;
begin
  FreeAndNil(FRequestedSources);
  inherited Destroy;
end;

function TFpLineInfo.Count: Integer;
begin
  Result := FRequestedSources.Count;
end;

function TFpLineInfo.HasAddress(const AIndex: Integer; const ALine: Integer
  ): Boolean;
var
  Map: PDWarfLineMap;
  dummy: TDBGPtrArray;
begin
  Result := False;
  if not((FpDebugger.DebugInfo <> nil) and (FpDebugger.DebugInfo is TFpDwarfInfo)) then
    exit;
  Map := PDWarfLineMap(FRequestedSources.Objects[AIndex]);
  if Map <> nil then
  begin
    dummy:=nil;
    Result := Map^.GetAddressesForLine(ALine, dummy, True);
  end;
end;

function TFpLineInfo.GetInfo(AAddress: TDbgPtr; out ASource, ALine,
  AOffset: Integer): Boolean;
begin
  Result := False;
end;

function TFpLineInfo.IndexOf(const ASource: String): integer;
begin
  Result := FRequestedSources.IndexOf(ASource);
end;

procedure TFpLineInfo.Request(const ASource: String);
var
  i: Integer;
begin
  if (FpDebugger.DebugInfo = nil) or not(FpDebugger.DebugInfo is TFpDwarfInfo) then begin
    FRequestedSources.AddObject(ASource, nil);
    exit;
  end;
  i := FRequestedSources.AddObject(ASource, TObject(TFpDwarfInfo(FpDebugger.DebugInfo).GetLineAddressMap(ASource)));
  if FRequestedSources.Objects[i] <> nil then
    DoChange(ASource);
end;

procedure TFpLineInfo.Cancel(const ASource: String);
begin
  //
end;

{ TFPWatches }

function TFPWatches.FpDebugger: TFpDebugDebugger;
begin
  Result := TFpDebugDebugger(Debugger);
end;

procedure TFPWatches.InternalRequestData(AWatchValue: TWatchValue);
//var
//  AVal: string;
//  AType: TDBGType;
begin
  FpDebugger.ScheduleWatchValueEval(AWatchValue);
  //FpDebugger.EvaluateExpression(AWatchValue, AWatchValue.Expression, AVal, AType);
end;

{ TFpDebugThread }

procedure TFpDebugThread.DoDebugLoopFinishedASync(Data: PtrInt);
begin
  FQueuedFinish:=false;
  FFpDebugDebugger.DebugLoopFinished;
end;

constructor TFpDebugThread.Create(AFpDebugDebugger: TFpDebugDebugger);
begin
  FDebugLoopStoppedEvent := RTLEventCreate;
  FStartDebugLoopEvent := RTLEventCreate;
  FFpDebugDebugger := AFpDebugDebugger;
  inherited Create(false);
end;

destructor TFpDebugThread.Destroy;
begin
  if FQueuedFinish then
    Application.RemoveAsyncCalls(Self);
  RTLeventdestroy(FStartDebugLoopEvent);
  RTLeventdestroy(FDebugLoopStoppedEvent);
  inherited Destroy;
end;

procedure TFpDebugThread.Execute;
begin
  if FFpDebugDebugger.FDbgController.Run then
    FStartSuccessfull:=true;

  RTLeventSetEvent(FDebugLoopStoppedEvent);

  if FStartSuccessfull then
    begin
    repeat
    RTLeventWaitFor(FStartDebugLoopEvent);
    RTLeventResetEvent(FStartDebugLoopEvent);
    if not terminated then
      begin
      if assigned(FAsyncMethod) then
        begin
        try
          FAsyncMethod();
        finally
          RTLeventSetEvent(FDebugLoopStoppedEvent);
        end;
        end
      else
        begin
        FFpDebugDebugger.FDbgController.ProcessLoop;
        if not FQueuedFinish then
          begin
          FQueuedFinish:=true;
          Application.QueueAsyncCall(@DoDebugLoopFinishedASync, 0);
          end;
        end;
      end;
    until Terminated;
    end
end;

{ TFpDebugDebugger }

procedure TFpDebugDebugger.FDbgControllerProcessExitEvent(AExitCode: DWord);
var
  AThread: TFpWaitForConsoleOutputThread;
begin
  if assigned(FConsoleOutputThread) then
    begin
    AThread := TFpWaitForConsoleOutputThread(FConsoleOutputThread);
    FConsoleOutputThread := nil;
    AThread.Terminate;
    AThread.DoHasConsoleOutput(0);
    AThread.WaitFor;
    AThread.Free;
    end;

  SetExitCode(Integer(AExitCode));
  {$PUSH}{$R-}
  DoDbgEvent(ecProcess, etProcessExit, Format('Process exited with exit-code %d',[AExitCode]));
  {$POP}
  IncReleaseLock;
  try
    SetState(dsStop);
    FreeAndNil(FRaiseExceptionBreakpoint);
    FreeDebugThread;
  finally
    DecReleaseLock;
  end;
end;

procedure TFpDebugDebugger.FDbgControllerExceptionEvent(var continue: boolean;
  const ExceptionClass, ExceptionMessage: string);
begin
  DoException(deExternal, ExceptionClass, GetLocation, ExceptionMessage, continue);
  if not continue then
    begin
    SetState(dsPause);
    DoCurrent(GetLocation);
    end;
end;

function TFpDebugDebugger.GetDebugInfo: TDbgInfo;
begin
  Result := nil;
  if (FDbgController <> nil) and (FDbgController.CurrentProcess<> nil) then
    Result := FDbgController.CurrentProcess.DbgInfo;
end;

procedure TFpDebugDebugger.ScheduleWatchValueEval(AWatchValue: TWatchValue);
begin
  AWatchValue.AddFreeNotification(@DoWatchFreed);
  FWatchEvalList.Add(pointer(AWatchValue));
  if not FWatchAsyncQueued then
    begin
    Application.QueueAsyncCall(@ProcessASyncWatches, 0);
    FWatchAsyncQueued := True;
    end;
end;

function TFpDebugDebugger.EvaluateExpression(AWatchValue: TWatchValue; AExpression: String;
  out AResText: String; out ATypeInfo: TDBGType; EvalFlags: TDBGEvaluateFlags): Boolean;
var
  AContext: TFpDbgInfoContext;
  AController: TDbgController;
  APasExpr: TFpPascalExpression;
  ADbgInfo: TDbgInfo;
  DispFormat: TWatchDisplayFormat;
  RepeatCnt: Integer;
  Res: Boolean;
  AFrame: TDbgCallstackEntry;
  StackFrame, ThreadId: Integer;
  RegList: TDbgRegisterValueList;
  Reg: TDbgRegisterValue;
  StackList: TCallStackBase;
begin
  Result := False;
  AResText := '';
  ATypeInfo := nil;

  AController := FDbgController;
  ADbgInfo := AController.CurrentProcess.DbgInfo;

  if AWatchValue <> nil then begin
    StackFrame := AWatchValue.StackFrame;
    ThreadId := AWatchValue.ThreadId;
    DispFormat := AWatchValue.DisplayFormat;
    RepeatCnt := AWatchValue.RepeatCount;
  end
  else begin
    ThreadId := Threads.CurrentThreads.CurrentThreadId;
    StackList := CallStack.CurrentCallStackList.EntriesForThreads[ThreadId];
    if StackList <> nil then
      StackFrame := CallStack.CurrentCallStackList.EntriesForThreads[ThreadId].CurrentIndex
    else
      StackFrame := 0;
    DispFormat := wdfDefault;
    RepeatCnt := -1;
  end;

  if StackFrame > 0 then
    begin
    FDbgController.CurrentThread.PrepareCallStackEntryList(StackFrame);
    AFrame := FDbgController.CurrentThread.CallStackEntryList[StackFrame];
    if AFrame = nil then
      begin
      if AWatchValue <> nil then
        AWatchValue.Validity := ddsInvalid;
      exit;
      end;
    RegList := AFrame.RegisterValueList;
    end
  else
    RegList := AController.CurrentThread.RegisterValueList;
  if AController.CurrentProcess.Mode = dm32 then
    Reg := RegList.FindRegisterByDwarfIndex(8)
  else
    Reg := RegList.FindRegisterByDwarfIndex(16);
  if Reg <> nil then
    AContext := ADbgInfo.FindContext(ThreadId, StackFrame, Reg.NumValue)
  else
    AContext := nil;

  if AContext = nil then
    begin
    if AWatchValue <> nil then
      AWatchValue.Validity := ddsInvalid;
    exit;
    end;

  Result := True;
  AContext.MemManager.DefaultContext := AContext;
  APasExpr := TFpPascalExpression.Create(AExpression, AContext);
  try
    APasExpr.ResultValue; // trigger full validation
    if not APasExpr.Valid then
      begin
      AResText := ErrorHandler.ErrorAsString(APasExpr.Error);
      if AWatchValue <> nil then
        begin
        AWatchValue.Value := AResText;
        AWatchValue.Validity := ddsError;
        end;
      end
    else
      begin
      FPrettyPrinter.AddressSize:=AContext.SizeOfAddress;
      FPrettyPrinter.MemManager := AContext.MemManager;
      if defNoTypeInfo in EvalFlags then
        Res := FPrettyPrinter.PrintValue(AResText, APasExpr.ResultValue, DispFormat, RepeatCnt)
      else
        Res := FPrettyPrinter.PrintValue(AResText, ATypeInfo, APasExpr.ResultValue, DispFormat, RepeatCnt);
      // TODO: PCHAR/String
      if Res then
        begin
        if AWatchValue <> nil then
          begin
          AWatchValue.Value := AResText; //IntToStr(APasExpr.ResultValue.AsInteger);
          AWatchValue.Validity := ddsValid;
          end;
        end
      else
        begin
        AResText := 'Error';
        if AWatchValue <> nil then
          AWatchValue.Validity := ddsInvalid;
        end;
      end;
  finally
    APasExpr.Free;
    AContext.ReleaseReference;
  end;
end;

function TFpDebugDebugger.CreateLineInfo: TDBGLineInfo;
begin
  Result := TFpLineInfo.Create(Self);
end;

function TFpDebugDebugger.CreateWatches: TWatchesSupplier;
begin
  Result := TFPWatches.Create(Self);
end;

function TFpDebugDebugger.CreateThreads: TThreadsSupplier;
begin
  Result := TFPThreads.Create(Self);
end;

function TFpDebugDebugger.CreateLocals: TLocalsSupplier;
begin
  Result := TFPLocals.Create(Self);
end;

function TFpDebugDebugger.CreateRegisters: TRegisterSupplier;
begin
  Result := TFPRegisters.Create(Self);
end;

function TFpDebugDebugger.CreateCallStack: TCallStackSupplier;
begin
  Result:=TFPCallStackSupplier.Create(Self);
end;

function TFpDebugDebugger.CreateDisassembler: TDBGDisassembler;
begin
  Result:=TFPDBGDisassembler.Create(Self);
end;

function TFpDebugDebugger.CreateBreakPoints: TDBGBreakPoints;
begin
  Result := TFPBreakPoints.Create(Self, TFPBreakpoint);
end;

procedure TFpDebugDebugger.FDbgControllerDebugInfoLoaded(Sender: TObject);
begin
  TFpDwarfInfo(FDbgController.CurrentProcess.DbgInfo).MemManager := FMemManager;
  if LineInfo <> nil then begin
    TFpLineInfo(LineInfo).DebugInfoChanged;
  end;
end;

procedure TFpDebugDebugger.DoWatchFreed(Sender: TObject);
begin
  FWatchEvalList.Remove(pointer(Sender));
end;

procedure TFpDebugDebugger.ProcessASyncWatches(Data: PtrInt);
var
  WatchValue: TWatchValue;
  AVal: String;
  AType: TDBGType;
begin
  FWatchAsyncQueued := False;
  if FWatchEvalList.Count = 0 then
    exit;
  WatchValue := TWatchValue(FWatchEvalList[0]);
  FWatchEvalList.Delete(0);
  WatchValue.RemoveFreeNotification(@DoWatchFreed);

  EvaluateExpression(WatchValue, WatchValue.Expression, AVal, AType);
  AType.Free;

  if (not FWatchAsyncQueued) and (FWatchEvalList.Count > 0) then
    begin
    Application.QueueAsyncCall(@ProcessASyncWatches, 0);
    FWatchAsyncQueued := True;
    end;
end;

procedure TFpDebugDebugger.DoLog();
var
  AMessage: TFpDbgLogMessage;
  AnObjList: TFPObjectList;
  i: Integer;
begin
  AnObjList:=TFPObjectList.Create(false);
  try
    EnterCriticalsection(FLogCritSection);
    try
      while FDbgLogMessageList.Count > 0 do
        begin
        AnObjList.Add(FDbgLogMessageList[0]);
        FDbgLogMessageList.Delete(0);
        end;
    finally
      LeaveCriticalsection(FLogCritSection);
    end;

    for i := 0 to AnObjList.Count-1 do
      begin
      AMessage := TFpDbgLogMessage(AnObjList[i]);
      case AMessage.SyncLogLevel of
        dllDebug: DebugLn(AMessage.SyncLogMessage);
        dllInfo:  ShowMessage(AMessage.SyncLogMessage);
        dllError: raise exception.Create(AMessage.SyncLogMessage);
      end; {case}
      AMessage.Free;
      end;
  finally
    AnObjList.Free;
  end;
end;

procedure TFpDebugDebugger.IncReleaseLock;
begin
  inc(FReleaseLock);
end;

procedure TFpDebugDebugger.DecReleaseLock;
begin
  dec(FReleaseLock);
  if FReleaseNeeded and (FReleaseLock = 0) then
    DoRelease;
end;

function TFpDebugDebugger.GetClassInstanceName(AnAddr: TDBGPtr): string;
var
  VMTAddr: TDBGPtr;
  ClassNameAddr: TDBGPtr;
  b: byte;
begin
  // Read address of the vmt
  ReadAddress(AnAddr, VMTAddr);
  ReadAddress(VMTAddr+3*DBGPTRSIZE[FDbgController.CurrentProcess.Mode], ClassNameAddr);
  // read classname (as shortstring)
  ReadData(ClassNameAddr, 1, b);
  setlength(result,b);
  ReadData(ClassNameAddr+1, b, result[1]);
end;

function TFpDebugDebugger.ReadAnsiString(AnAddr: TDbgPtr): string;
var
  StrAddr: TDBGPtr;
  len: TDBGPtr;
begin
  result := '';
  if not ReadAddress(AnAddr, StrAddr) then
    Exit;
  ReadAddress(StrAddr-DBGPTRSIZE[FDbgController.CurrentProcess.Mode], len);
  setlength(result, len);
  if not ReadData(StrAddr, len, result[1]) then
    result := '';
end;

function TFpDebugDebugger.SetSoftwareExceptionBreakpoint: boolean;
var
  AContext: TFpDbgInfoContext;
  AValue: TFpDbgValue;
  AnAddr: TDBGPtr;
begin
  result := false;
  if assigned(FDbgController.CurrentProcess.SymbolTableInfo) then
  begin
    AContext := FDbgController.CurrentProcess.SymbolTableInfo.FindContext(0);
    if Assigned(AContext) then
    begin
      AValue := AContext.FindSymbol('FPC_RAISEEXCEPTION');
      if assigned(AValue) then
      begin
        AnAddr:=AValue.Address.Address;
        AValue.ReleaseReference;
        FRaiseExceptionBreakpoint := AddBreak(AnAddr);
        if assigned(FRaiseExceptionBreakpoint) then
          result := True;
      end;
    end;
  end;
end;

procedure TFpDebugDebugger.HandleSoftwareException(out
  AnExceptionLocation: TDBGLocationRec; var continue: boolean);
var
  AnExceptionObjectLocation: TDBGPtr;
  ExceptionClass: string;
  ExceptionMessage: string;
  RegDxDwarfIndex: byte;
begin
  // Using regvar:
  // In all their wisdom, people decided to give the (r)dx register dwarf index
  // 1 on for x86_64 and index 2 for i386.
  if FDbgController.CurrentProcess.Mode=dm32 then
    RegDxDwarfIndex:=2
  else
    RegDxDwarfIndex:=1;

  AnExceptionLocation:=GetLocationRec(FDbgController.CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(RegDxDwarfIndex).NumValue);
  AnExceptionObjectLocation:=FDbgController.CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(0).NumValue;
  ExceptionClass := GetClassInstanceName(AnExceptionObjectLocation);
  ExceptionMessage := ReadAnsiString(AnExceptionObjectLocation+DBGPTRSIZE[FDbgController.CurrentProcess.Mode]);

  DoException(deInternal, ExceptionClass, AnExceptionLocation, ExceptionMessage, continue);
end;

procedure TFpDebugDebugger.FreeDebugThread;
begin
  if FFpDebugThread = nil then
    exit;
  FFpDebugThread.Terminate;
  RTLeventSetEvent(FFpDebugThread.StartDebugLoopEvent);
  FFpDebugThread.WaitFor;
  FFpDebugThread.Free;
  FFpDebugThread := nil;
end;

procedure TFpDebugDebugger.FDbgControllerHitBreakpointEvent(
  var continue: boolean; const Breakpoint: TFpInternalBreakpoint);
var
  ABreakPoint: TDBGBreakPoint;
  ALocationAddr: TDBGLocationRec;
begin
  if assigned(Breakpoint) then
    begin
    if BreakPoint=FRaiseExceptionBreakpoint then
      begin
        HandleSoftwareException(ALocationAddr, continue);
        if continue then
          exit;
      end
    else
      begin
        ALocationAddr := GetLocation;
        ABreakPoint := TFPBreakpoints(BreakPoints).Find(Breakpoint);

        if Assigned(EventLogHandler) then
          EventLogHandler.LogEventBreakPointHit(ABreakpoint, ALocationAddr);

        if assigned(ABreakPoint) then
          ABreakPoint.Hit(continue);
      end;
    end
  else if FQuickPause then
    begin
      SetState(dsPause);//dsInternalPause;
      continue:=true;
      exit;
    end
  else
    // Debugger returned after a step/next/step-out etc..
    ALocationAddr := GetLocation;

  SetState(dsPause);
  DoCurrent(ALocationAddr);
end;

procedure TFpDebugDebugger.FDbgControllerCreateProcessEvent(var continue: boolean);
begin
  // This will trigger setting the breakpoints, but won't trigger the evaluation
  // of the callstack or disassembler.
  SetState(dsInternalPause);

  if not SetSoftwareExceptionBreakpoint then
    debugln('Failed to set software-debug breakpoint');

  if assigned(OnConsoleOutput) then
    FConsoleOutputThread := TFpWaitForConsoleOutputThread.Create(self);
end;

function TFpDebugDebugger.RequestCommand(const ACommand: TDBGCommand;
  const AParams: array of const; const ACallback: TMethod): Boolean;
var
  EvalFlags: TDBGEvaluateFlags;
  AConsoleTty, ResText: string;
  addr: TDBGPtrArray;
  ResType: TDBGType;
begin
  result := False;
  if assigned(FDbgController) then
    FDbgController.NextOnlyStopOnStartLine := TFpDebugDebuggerProperties(GetProperties).NextOnlyStopOnStartLine;
  case ACommand of
    dcRun:
      begin
      if not assigned(FDbgController.MainProcess) then
        begin
        FDbgController.ExecutableFilename:=FileName;
        AConsoleTty:=TFpDebugDebuggerProperties(GetProperties).ConsoleTty;
        FDbgController.ConsoleTty:=AConsoleTty;
        FDbgController.RedirectConsoleOutput:=AConsoleTty='';
        FDbgController.Params.Clear;
        if Arguments<>'' then
          CommandToList(Arguments, FDbgController.Params);
        FDbgController.WorkingDirectory:=WorkingDir;
        FDbgController.Environment:=Environment;
        FFpDebugThread := TFpDebugThread.Create(Self);
        RTLeventWaitFor(FFpDebugThread.DebugLoopStoppedEvent);
        RTLeventResetEvent(FFpDebugThread.DebugLoopStoppedEvent);
        result := FFpDebugThread.StartSuccesfull;
        if not result then
          begin
          // TDebuggerIntf.SetFileName has set the state to dsStop, to make sure
          // that dcRun could be requested. Reset the filename so that the state
          // is set to dsIdle again and is set to dsStop on the next try
          // to run.
          FileName := '';
          FreeDebugThread;
          Exit;
          end;
        SetState(dsInit);
        end
      else
        begin
        Result := True;
        SetState(dsRun);
        end;
      StartDebugLoop;
      end;
    dcStop:
      begin
        FDbgController.Stop;
        if state=dsPause then
          begin
          SetState(dsRun);
          StartDebugLoop;
          end;
        result := true;
      end;
    dcStepIntoInstr:
      begin
        FDbgController.StepIntoInstr;
        SetState(dsRun);
        StartDebugLoop;
        result := true;
      end;
    dcStepOverInstr:
      begin
        FDbgController.StepOverInstr;
        SetState(dsRun);
        StartDebugLoop;
        result := true;
      end;
    dcPause:
      begin
        FDbgController.Pause;
      end;
    dcRunTo:
      begin
        result := false;
        if FDbgController.CurrentProcess.DbgInfo.HasInfo then
          begin
          addr:=nil;
          if FDbgController.CurrentProcess.DbgInfo.GetLineAddresses(AnsiString(AParams[0].VAnsiString), AParams[1].VInteger, addr)
          then begin
            result := true;
            FDbgController.InitializeCommand(TDbgControllerRunToCmd.Create(FDbgController, addr));
            SetState(dsRun);
            StartDebugLoop;
            end;
          end;
      end;
    dcStepOver:
      begin
        FDbgController.Next;
        SetState(dsRun);
        StartDebugLoop;
        result := true;
      end;
    dcStepInto:
      begin
        FDbgController.Step;
        SetState(dsRun);
        StartDebugLoop;
        result := true;
      end;
    dcStepOut:
      begin
        FDbgController.StepOut;
        SetState(dsRun);
        StartDebugLoop;
        result := true;
      end;
    dcEvaluate:
      begin
        EvalFlags := TDBGEvaluateFlags(AParams[1].VInteger);
        Result := EvaluateExpression(nil, String(AParams[0].VAnsiString),
          ResText, ResType, EvalFlags);
        if EvalFlags * [defNoTypeInfo, defSimpleTypeInfo, defFullTypeInfo] = [defNoTypeInfo]
        then FreeAndNil(ResType);
        TDBGEvaluateResultCallback(ACallback)(Self, Result, ResText, ResType);
        Result := True;
      end;
    dcSendConsoleInput:
      begin
        FDbgController.CurrentProcess.SendConsoleInput(String(AParams[0].VAnsiString));
      end;
  end; {case}
end;

function TFpDebugDebugger.ChangeFileName: Boolean;
begin
  result := true;
end;

procedure TFpDebugDebugger.OnLog(const AString: string; const ALogLevel: TFPDLogLevel);
var
  AMessage: TFpDbgLogMessage;
begin
  // This function could be running in a thread. Add the log-message to an
  // array and queue the processing in the main thread. Not an ideal
  // implementation. But good enough for now.
  AMessage := TFpDbgLogMessage.Create;
  AMessage.SyncLogLevel:=ALogLevel;
  AMessage.SyncLogMessage:=AString;
  EnterCriticalsection(FLogCritSection);
  try
    FDbgLogMessageList.Add(AMessage);
  finally
    LeaveCriticalsection(FLogCritSection);
  end;
  TThread.Queue(nil, @DoLog);
end;

procedure TFpDebugDebugger.ExecuteInDebugThread(AMethod: TFpDbgAsyncMethod);
begin
  assert(not assigned(FFpDebugThread.AsyncMethod));
  FFpDebugThread.AsyncMethod:=AMethod;
  RTLeventSetEvent(FFpDebugThread.StartDebugLoopEvent);
  RTLeventWaitFor(FFpDebugThread.DebugLoopStoppedEvent);
  RTLeventResetEvent(FFpDebugThread.DebugLoopStoppedEvent);
  FFpDebugThread.AsyncMethod:=nil;
end;

procedure TFpDebugDebugger.StartDebugLoop;
begin
  {$ifdef DBG_FPDEBUG_VERBOSE}
  DebugLn('StartDebugLoop');
  {$endif DBG_FPDEBUG_VERBOSE}
  RTLeventSetEvent(FFpDebugThread.StartDebugLoopEvent);
end;

procedure TFpDebugDebugger.DebugLoopFinished;
var
  Cont: boolean;
begin
  IncReleaseLock;
  try
    {$ifdef DBG_FPDEBUG_VERBOSE}
    DebugLn('DebugLoopFinished');
    {$endif DBG_FPDEBUG_VERBOSE}

    FDbgController.SendEvents(Cont); // This may free the TFpDebugDebugger (self)

    FQuickPause:=false; // TODO: there may be other events: deInternalContinue, deLoadLibrary...

    if Cont then
      begin
      SetState(dsRun);
      StartDebugLoop;
      end
  finally
    DecReleaseLock;
  end;
end;

procedure TFpDebugDebugger.QuickPause;
begin
  FQuickPause:=FDbgController.Pause;
end;

procedure TFpDebugDebugger.DoRelease;
begin
  if FReleaseLock > 0 then begin
    FReleaseNeeded := True;
    exit;
  end;
  inherited DoRelease;
end;

procedure TFpDebugDebugger.DoState(const OldState: TDBGState);
begin
  IncReleaseLock;
  try
    inherited DoState(OldState);
    if not (State in [dsPause, dsInternalPause]) then
      begin
      if Assigned(FWatchEvalList) then
        FWatchEvalList.Clear;
      FWatchAsyncQueued := False;
      end;
  finally
    DecReleaseLock;
  end;
end;

{$ifdef linux}
procedure TFpDebugDebugger.DoAddBreakLine;
begin
  FCacheBreakpoint := TDbgInstance(FDbgController.CurrentProcess).AddBreak(FCacheFileName, FCacheLine);
end;

procedure TFpDebugDebugger.DoAddBreakLocation;
begin
  FCacheBreakpoint := FDbgController.CurrentProcess.AddBreak(FCacheLocation);
end;

procedure TFpDebugDebugger.DoReadData;
begin
  FCacheBoolean:=FDbgController.CurrentProcess.ReadData(FCacheLocation, FCacheLine, FCachePointer^);
end;

procedure TFpDebugDebugger.DoPrepareCallStackEntryList;
begin
  FDbgController.CurrentThread.PrepareCallStackEntryList;
end;

procedure TFpDebugDebugger.DoFreeBreakpoint;
begin
  FCacheBreakpoint.Free;
end;

{$endif linux}

function TFpDebugDebugger.AddBreak(const ALocation: TDbgPtr
  ): TFpInternalBreakpoint;
begin
{$ifdef linux}
  FCacheLocation:=ALocation;
  ExecuteInDebugThread(@DoAddBreakLocation);
  result := FCacheBreakpoint;
{$else linux}
  result := FDbgController.CurrentProcess.AddBreak(ALocation);
{$endif linux}
end;

function TFpDebugDebugger.AddBreak(const AFileName: String; ALine: Cardinal
  ): TFpInternalBreakpoint;
begin
{$ifdef linux}
  FCacheFileName:=AFileName;
  FCacheLine:=ALine;
  ExecuteInDebugThread(@DoAddBreakLine);
  result := FCacheBreakpoint;
{$else linux}
  result := TDbgInstance(FDbgController.CurrentProcess).AddBreak(AFileName, ALine);
{$endif linux}
end;

procedure TFpDebugDebugger.FreeBreakpoint(
  const ABreakpoint: TFpInternalBreakpoint);
begin
{$ifdef linux}
  if ABreakpoint = nil then exit;
  FCacheBreakpoint:=ABreakpoint;
  ExecuteInDebugThread(@DoFreeBreakpoint);
{$else linux}
  ABreakpoint.Free;
{$endif linux}
end;

function TFpDebugDebugger.ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean;
begin
{$ifdef linux}
  FCacheLocation := AAdress;
  FCacheLine:=ASize;
  FCachePointer := @AData;
  ExecuteInDebugThread(@DoReadData);
  result := FCacheBoolean;
{$else linux}
  result:=FDbgController.CurrentProcess.ReadData(AAdress, ASize, AData);
{$endif linux}
end;

function TFpDebugDebugger.ReadAddress(const AAdress: TDbgPtr; out AData: TDBGPtr): Boolean;
var
  dw: DWord;
  qw: QWord;
begin
  case FDbgController.CurrentProcess.Mode of
    dm32:
      begin
        result := ReadData(AAdress, sizeof(dw), dw);
        AData:=dw;
      end;
    dm64:
      begin
        result := ReadData(AAdress, sizeof(qw), qw);
        AData:=qw;
      end;
  end;
end;

procedure TFpDebugDebugger.PrepareCallStackEntryList;
begin
{$ifdef linux}
  ExecuteInDebugThread(@DoPrepareCallStackEntryList);
{$else linux}
  FDbgController.CurrentThread.PrepareCallStackEntryList;
{$endif linux}
end;

constructor TFpDebugDebugger.Create(const AExternalDebugger: String);
begin
  inherited Create(AExternalDebugger);
  FWatchEvalList := TFPList.Create;
  FPrettyPrinter := TFpPascalPrettyPrinter.Create(sizeof(pointer));
  FDbgLogMessageList := TFPObjectList.Create(false);
  InitCriticalSection(FLogCritSection);
  FMemReader := TFpDbgMemReader.Create(self);
  FMemConverter := TFpDbgMemConvertorLittleEndian.Create;
  FMemManager := TFpDbgMemManager.Create(FMemReader, FMemConverter);
  FDbgController := TDbgController.Create;
  FDbgController.OnLog:=@OnLog;
  FDbgController.OnCreateProcessEvent:=@FDbgControllerCreateProcessEvent;
  FDbgController.OnHitBreakpointEvent:=@FDbgControllerHitBreakpointEvent;
  FDbgController.OnProcessExitEvent:=@FDbgControllerProcessExitEvent;
  FDbgController.OnExceptionEvent:=@FDbgControllerExceptionEvent;
  FDbgController.OnDebugInfoLoaded := @FDbgControllerDebugInfoLoaded;
  FDbgController.NextOnlyStopOnStartLine := TFpDebugDebuggerProperties(GetProperties).NextOnlyStopOnStartLine;
end;

destructor TFpDebugDebugger.Destroy;
begin
  if assigned(FFpDebugThread) then
    FreeDebugThread;
  FreeAndNil(FDbgController);
  FreeAndNil(FPrettyPrinter);
  FreeAndNil(FWatchEvalList);
  FreeAndNil(FMemManager);
  FreeAndNil(FMemConverter);
  FreeAndNil(FMemReader);
  FreeAndNil(FDbgLogMessageList);
  DoneCriticalsection(FLogCritSection);
  inherited Destroy;
end;

function TFpDebugDebugger.GetLocationRec(AnAddress: TDBGPtr): TDBGLocationRec;
var
  sym, symproc: TFpDbgSymbol;
begin
  if Assigned(FDbgController.CurrentProcess) then
    begin
    result.FuncName:='';
    result.SrcFile:='';
    result.SrcFullName:='';
    result.SrcLine:=0;

    if AnAddress=0 then
      result.Address := FDbgController.CurrentThread.GetInstructionPointerRegisterValue
    else
      result.Address := AnAddress;

    sym := FDbgController.CurrentProcess.FindSymbol(result.Address);
    if sym = nil then
      Exit;

    result.SrcFile := ExtractFileName(sym.FileName);
    result.SrcLine := sym.Line;
    result.SrcFullName := sym.FileName;

    symproc := sym;
    while not (symproc.kind in [skProcedure, skFunction]) do
      symproc := symproc.Parent;

    if assigned(symproc) then
      result.FuncName:=symproc.Name;
    sym.ReleaseReference;
    end
end;

function TFpDebugDebugger.GetLocation: TDBGLocationRec;
begin
  Result:=GetLocationRec;
end;

class function TFpDebugDebugger.Caption: String;
begin
  Result:='FpDebug internal Dwarf-debugger (beta)';
end;

class function TFpDebugDebugger.NeedsExePath: boolean;
begin
  Result:=False;
end;

class function TFpDebugDebugger.CanExternalDebugSymbolsFile: boolean;
begin
  {$ifdef CD_Cocoa}{$DEFINE MacOS}{$ENDIF}
  {$IFDEF Darwin}{$DEFINE MacOS}{$ENDIF}
  {$IFDEF MacOs}
  Result:=True;
  {$ELSE}
  Result:=False;
  {$ENDIF}
end;

class function TFpDebugDebugger.CreateProperties: TDebuggerProperties;
begin
  Result := TFpDebugDebuggerProperties.Create;
end;

function TFpDebugDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result:=[dcRun, dcStop, dcStepIntoInstr, dcStepOverInstr, dcStepOver,
           dcRunTo, dcPause, dcStepOut, dcStepInto, dcEvaluate, dcSendConsoleInput];
end;

end.

