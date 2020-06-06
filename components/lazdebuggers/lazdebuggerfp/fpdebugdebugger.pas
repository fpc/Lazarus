unit FpDebugDebugger;

{$mode objfpc}{$H+}
{$TYPEDADDRESS on}

interface

uses
  Classes,
  SysUtils, fgl, math,
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
  FPDbgController, FpDbgDwarfDataClasses, FpDbgDwarfFreePascal, FpDbgDwarf;

type

  { TFpDebugThread }
  TFpDebugDebugger = class;
  TFpDbgAsyncMethod = procedure() of object;

  TFpDebugThread = class(TThread)
  private
    FAsyncMethod: TFpDbgAsyncMethod;
    FDebugLoopStoppedEvent: PRTLEvent;
    FFpDebugDebugger: TFpDebugDebugger;
    FLoopIsRunnig: LongBool;
    FStartDebugLoopEvent: PRTLEvent;
    FStartSuccessfull: boolean;
    FQueuedFinish: boolean;  // true = DoDebugLoopFinishedASync queud in main thread
    procedure DoDebugLoopFinishedASync({%H-}Data: PtrInt);
    function GetLoopIsRunnig: LongBool;
  public
    constructor Create(AFpDebugDebugger: TFpDebugDebugger);
    destructor Destroy; override;
    procedure Execute; override;
    property StartSuccesfull: boolean read FStartSuccessfull;
    property StartDebugLoopEvent: PRTLEvent read FStartDebugLoopEvent;
    property DebugLoopStoppedEvent: PRTLEvent read FDebugLoopStoppedEvent;
    property AsyncMethod: TFpDbgAsyncMethod read FAsyncMethod write FAsyncMethod;
    property LoopIsRunnig: LongBool read GetLoopIsRunnig;
  end;

  { TFpDebugDebuggerProperties }

  TFpDebugDebuggerProperties = class(TDebuggerProperties)
  private
    FConsoleTty: string;
    {$ifdef windows}
    FForceNewConsole: boolean;
    {$endif windows}
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
    {$ifdef windows}
    property ForceNewConsole: boolean read FForceNewConsole write FForceNewConsole;
    {$endif windows}
  end;

  { TDbgControllerStepOverOrFinallyCmd
    Step over with detection for finally blocks
  }

  TDbgControllerStepOverOrFinallyCmd = class(TDbgControllerStepOverLineCmd)
  private
    FFinState: (fsNone, fsMov, fsCall, fsInFin);
  protected
    procedure InternalContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread;
      out Finished: boolean); override;
  end;

  { TDbgControllerStepOverFirstFinallyLineCmd }

  TDbgControllerStepOverFirstFinallyLineCmd = class(TDbgControllerStepOverLineCmd)
  protected
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread;
      out Finished: boolean); override;
  end;

  { TDbgControllerStepThroughFpcSpecialHandler }

  TDbgControllerStepThroughFpcSpecialHandler = class(TDbgControllerStepOverInstructionCmd)
  private
    FAfterFinCallAddr: TDbgPtr;
    FDone: Boolean;
  protected
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean); override;
    procedure InternalContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
    procedure Init; override;
  public
    constructor Create(AController: TDbgController; AnAfterFinCallAddr: TDbgPtr); reintroduce;
  end;

  { TFpDebugExceptionStepping }

  TFpDebugExceptionStepping = class
  (* Methods in this class are either called:
     - by the debug-thread / in the context of the debug-thread
     - by the main-thread / But ONLY if the debug-thread is paused
     Starting the debug-thread uses "RTLeventSetEvent" which triggers a
     memory barrier. So memberfields can be used savely between all methods.
  *)
  private
  type
    TBreakPointLoc = (
      bplRaise, bplReRaise, // bplBreakError, bplRunError,
      bplPopExcept, bplCatches,
      bplRtlUnwind, bplFpcSpecific,
      bplSehW64Finally, bplSehW64Except,
      bplStepOut  // Step out of Pop/Catches
    );
    TBreakPointLocs = set of TBreakPointLoc;
    TExceptStepState = (esNone,
      esStoppedAtRaise,  // Enter dsPause, next step is "stop to finally"
      esIgnoredRaise,    // Keep dsRun, stop at finally/except *IF* outside current stepping frame
      esStepToFinally,
      esStepSehFinallyProloque,
      esSteppingFpcSpecialHandler,
      esAtWSehExcept
    );
    TFrameList = class(specialize TFPGList<TDbgPtr>);

    { TAddressFrameList }

    TAddressFrameList = class(specialize TFPGMapObject<TDbgPtr, TFrameList>)
    public
      function Add(const AKey: TDbgPtr): TFrameList; inline;
    end;
  const
    DBGPTRSIZE: array[TFPDMode] of Integer = (4, 8);
  private
    FDebugger: TFpDebugDebugger;
    FBreakPoints: array[TBreakPointLoc] of TFpDbgBreakpoint;
    FBreakEnabled: TBreakPointLocs;
    FBreakNewEnabled: TBreakPointLocs;
    FAddressFrameList: TAddressFrameList;
    FState: TExceptStepState;
    function GetCurrentCommand: TDbgControllerCmd; inline;
    function GetCurrentProcess: TDbgProcess; inline;
    function GetCurrentThread: TDbgThread; inline;
    function GetDbgController: TDbgController; inline;
    function dbgs(st: TExceptStepState): string;
    function dbgs(loc: TBreakPointLoc): string;
    function dbgs(locs: TBreakPointLocs): string;
  protected
    property DbgController: TDbgController read GetDbgController;
    property CurrentProcess: TDbgProcess read GetCurrentProcess;
    property CurrentThread: TDbgThread read GetCurrentThread;
    property CurrentCommand: TDbgControllerCmd read GetCurrentCommand;

    procedure EnableBreaks(ALocs: TBreakPointLocs);
    procedure EnableBreaksDirect(ALocs: TBreakPointLocs); // only in dbg thread
    procedure DisableBreaks(ALocs: TBreakPointLocs);
    procedure DisableBreaksDirect(ALocs: TBreakPointLocs); // only in dbg thread
    procedure SetStepOutAddrDirect(AnAddr: TDBGPtr); // only in dbg thread

    procedure DoExceptionRaised(var &continue: boolean);
    //procedure DoPopExcptStack;
    procedure DoRtlUnwindEx;
  public
    constructor Create(ADebugger: TFpDebugDebugger);
    destructor Destroy; override;

    procedure DoProcessLoaded;
    procedure DoNtDllLoaded(ALib: TDbgLibrary);
    //procedure DoLibraryLoaded(ALib: TDbgLibrary);  // update breakpoints
    procedure DoDbgStopped;

    procedure ThreadBeforeLoop(Sender: TObject);
    procedure ThreadProcessLoopCycle(var AFinishLoopAndSendEvents: boolean;
      var AnEventType: TFPDEvent; var ACurCommand: TDbgControllerCmd; var AnIsFinished: boolean);
    function  BreakpointHit(var &continue: boolean; const Breakpoint: TFpDbgBreakpoint): boolean;
    procedure UserCommandRequested(var ACommand: TDBGCommand);

//    procedure ClearState;
  end;

  { TFpDebugDebugger }

  TFpDebugDebugger = class(TDebuggerIntf)
  private
    FIsIdle: Boolean;
    FWatchEvalList: TFPList; // Schedule
    FWatchAsyncQueued: Boolean;
    FPrettyPrinter: TFpPascalPrettyPrinter;
    FDbgController: TDbgController;
    FFpDebugThread: TFpDebugThread;
    FQuickPause: boolean;
    FMemConverter: TFpDbgMemConvertorLittleEndian;
    FMemReader: TDbgMemReader;
    FMemManager: TFpDbgMemManager;
    FExceptionStepper: TFpDebugExceptionStepping;
    FConsoleOutputThread: TThread;
    // Helper vars to run in debug-thread
    FCacheLine: cardinal;
    FCacheFileName: string;
    FCacheLib: TDbgLibrary;
    FCacheBreakpoint: TFpDbgBreakpoint;
    FCacheLocation: TDBGPtr;
    FCacheBoolean: boolean;
    FCachePointer: pointer;
    FCacheReadWrite: TDBGWatchPointKind;
    FCacheScope: TDBGWatchPointScope;
    FCacheThreadId, FCacheStackFrame: Integer;
    FCacheContext: TFpDbgInfoContext;
    //
    function GetClassInstanceName(AnAddr: TDBGPtr): string;
    function ReadAnsiString(AnAddr: TDbgPtr): string;
    procedure HandleSoftwareException(out AnExceptionLocation: TDBGLocationRec; var continue: boolean);
    procedure FreeDebugThread;
    procedure FDbgControllerHitBreakpointEvent(var continue: boolean; const Breakpoint: TFpDbgBreakpoint);
    procedure EnterPause(ALocationAddr: TDBGLocationRec; AnInternalPause: Boolean = False);
    procedure RunInternalPauseTasks;
    procedure FDbgControllerCreateProcessEvent(var {%H-}continue: boolean);
    procedure FDbgControllerProcessExitEvent(AExitCode: DWord);
    procedure FDbgControllerExceptionEvent(var continue: boolean; const ExceptionClass, ExceptionMessage: string);
    procedure FDbgControllerDebugInfoLoaded(Sender: TObject);
    procedure FDbgControllerLibraryLoaded(var continue: boolean; ALib: TDbgLibrary);
    procedure FDbgControllerLibraryUnloaded(var continue: boolean; ALib: TDbgLibrary);
    function GetDebugInfo: TDbgInfo;
    procedure DoWatchFreed(Sender: TObject);
    procedure ProcessASyncWatches({%H-}Data: PtrInt);
    procedure ClearWatchEvalList;
  protected
    procedure GetCurrentThreadAndStackFrame(out AThreadId, AStackFrame: Integer);
    function GetContextForEvaluate(const ThreadId, StackFrame: Integer): TFpDbgInfoContext;
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

    // On Linux, communication with the debuggee is only allowed from within
    // the thread that created the debuggee. So a method to execute functions
    // within the debug-thread is necessary.
    function  ExecuteInDebugThread(AMethod: TFpDbgAsyncMethod): boolean;
    procedure StartDebugLoop;
    procedure DebugLoopFinished;
    procedure QuickPause;
    procedure DoRelease; override;
    procedure DoOnIdle;
    procedure DoState(const OldState: TDBGState); override;
    function GetIsIdle: Boolean; override;
  protected
    // Helper vars to run in debug-thread
    FCallStackEntryListThread: TDbgThread;
    FCallStackEntryListFrameRequired, FNewThreadId: Integer;
    FParamAsString: String;
    FParamAsStringStackEntry: TDbgCallstackEntry;
    FParamAsStringPrettyPrinter: TFpPascalPrettyPrinter;
    FParamEnabled: Boolean;
    procedure DoAddBreakLine;
    procedure DoAddBreakFuncLib;
    procedure DoAddBreakLocation;
    procedure DoAddBWatch;
    procedure DoReadData;
    procedure DoPrepareCallStackEntryList;
    procedure DoFreeBreakpoint;
    procedure DoFindContext;
    procedure DoGetParamsAsString;
    procedure DoChangeCurrentThreadId;
    //
    function AddBreak(const ALocation: TDbgPtr; AnEnabled: Boolean = True): TFpDbgBreakpoint; overload;
    function AddBreak(const AFileName: String; ALine: Cardinal; AnEnabled: Boolean = True): TFpDbgBreakpoint; overload;
    function AddBreak(const AFuncName: String; ALib: TDbgLibrary = nil; AnEnabled: Boolean = True): TFpDbgBreakpoint; overload;
    function AddWatch(const ALocation: TDBGPtr; ASize: Cardinal; AReadWrite: TDBGWatchPointKind;
                      AScope: TDBGWatchPointScope): TFpDbgBreakpoint;
    procedure FreeBreakpoint(const ABreakpoint: TFpDbgBreakpoint);
    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; inline;
    function ReadAddress(const AAdress: TDbgPtr; out AData: TDBGPtr): Boolean;
    procedure PrepareCallStackEntryList(AFrameRequired: Integer = -1; AThread: TDbgThread = nil); inline;
    function  FindContext(AThreadId, AStackFrame: Integer): TFpDbgInfoContext; inline;
    function GetParamsAsString(AStackEntry: TDbgCallstackEntry; APrettyPrinter: TFpPascalPrettyPrinter): string; inline;

    property DebugInfo: TDbgInfo read GetDebugInfo;
  public
    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;
    function GetLocationRec(AnAddress: TDBGPtr=0): TDBGLocationRec;
    function GetLocation: TDBGLocationRec; override;
    class function Caption: String; override;
    class function NeedsExePath: boolean; override;
    class function RequiredCompilerOpts({%H-}ATargetCPU, {%H-}ATargetOS: String): TDebugCompilerRequirements; override;
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

  { TCallstackAsyncRequest }

  TCallstackAsyncRequest = class
  private
    FCallstack: TCallStackBase;
    FRequiredMinCount: Integer;
    FDebugger: TFpDebugDebugger;
    FInDestroy: Boolean;
    procedure FreeSelf;
    procedure CallStackFreed(Sender: TObject);
    procedure RequestAsync({%H-}Data: PtrInt);
  public
    constructor Create(ADebugger: TFpDebugDebugger; ACallstack: TCallStackBase;
      ARequiredMinCount: Integer);
    destructor Destroy; override;
  end;

  TCallstackAsyncRequestList = class(specialize TFPGObjectList<TCallstackAsyncRequest>);

  { TFPCallStackSupplier }

  TFPCallStackSupplier = class(TCallStackSupplier)
  private
    FPrettyPrinter: TFpPascalPrettyPrinter;
    FReqList: TCallstackAsyncRequestList;
  protected
    function  FpDebugger: TFpDebugDebugger;
    procedure DoStateLeavePause; override;
  public
    constructor Create(const ADebugger: TDebuggerIntf);
    destructor Destroy; override;
    procedure RequestCount(ACallstack: TCallStackBase); override;
    procedure RequestAtLeastCount(ACallstack: TCallStackBase;
      ARequiredMinCount: Integer); override;
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
    procedure ChangeCurrentThread(ANewId: Integer); override;
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
    FInternalBreakpoint: FpDbgClasses.TFpDbgBreakpoint;
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
    procedure AddBreakpointToDelayedRemoveList(ABreakpoint: FpDbgClasses.TFpDbgBreakpoint);
  public
    constructor Create(const ADebugger: TDebuggerIntf; const ABreakPointClass: TDBGBreakPointClass);
    destructor Destroy; override;
    function Find(AIntBReakpoint: FpDbgClasses.TFpDbgBreakpoint): TDBGBreakPoint;
  end;

procedure Register;

implementation

uses
  FpDbgUtil,
  FpDbgDisasX86,
  FpDbgCommon;

var
  DBG_VERBOSE, DBG_WARNINGS, DBG_BREAKPOINTS, FPDBG_COMMANDS: PLazLoggerLogGroup;

type

  { TFpDbgMemReader }

  TFpDbgMemReader = class(TDbgMemReader)
  private
    FFpDebugDebugger: TFpDebugDebugger;
    FRegNum: Cardinal;
    FRegValue: TDbgPtr;
    FRegContext: TFpDbgAddressContext;
    FRegResult: Boolean;
    procedure DoReadRegister;
  protected
    function GetDbgProcess: TDbgProcess; override;
    function GetDbgThread(AContext: TFpDbgAddressContext): TDbgThread; override;
  public
    constructor create(AFpDebugDebuger: TFpDebugDebugger);
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
    function ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
    function ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr;
      AContext: TFpDbgAddressContext): Boolean; override;
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

{ TDbgControllerStepOverFirstFinallyLineCmd }

procedure TDbgControllerStepOverFirstFinallyLineCmd.DoResolveEvent(
  var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean);
begin
  Finished := (FThread.CompareStepInfo(0, True) <> dcsiSameLine) or
              (NextInstruction.IsReturnInstruction) or IsSteppedOut;

  if Finished then
    AnEvent := deFinishedStep
  else
  if AnEvent in [deFinishedStep] then
    AnEvent:=deInternalContinue;
end;

{ TDbgControllerStepOverOrFinallyCmd }

procedure TDbgControllerStepOverOrFinallyCmd.InternalContinue(
  AProcess: TDbgProcess; AThread: TDbgThread);
var
  Instr: TDbgAsmInstruction;
begin
{
00000001000374AE 4889C1                   mov rcx,rax
00000001000374B1 488D15D3FFFFFF           lea rdx,[rip-$0000002D]
00000001000374B8 4989E8                   mov rax,rbp
00000001000374BB E89022FEFF               call -$0001DD70

}
  if (AThread = FThread) then begin
    Instr := NextInstruction;
    if Instr is TX86AsmInstruction then begin
      case TX86AsmInstruction(Instr).X86OpCode of
        OPmov:
          if UpperCase(TX86AsmInstruction(Instr).X86Instruction.Operand[2].Value) = 'RBP' then
            FFinState := fsMov;
        OPcall:
          if FFinState = fsMov then begin
            CheckForCallAndSetBreak;
            FProcess.Continue(FProcess, FThread, True); // Step into
            FFinState := fsCall;
            exit;
          end;
        else
          FFinState := fsNone;
      end;
    end;
  end;
  inherited InternalContinue(AProcess, AThread);
end;

procedure TDbgControllerStepOverOrFinallyCmd.DoResolveEvent(
  var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean);
var
  sym: TFpSymbol;
begin
  if FFinState = fsCall then begin
    sym := FProcess.FindProcSymbol(FThread.GetInstructionPointerRegisterValue);
    if pos('fin$', sym.Name) > 0 then
      FFinState := fsInFin
    else
      FFinState := fsNone;
    sym.ReleaseReference;

    if FFinState = fsInFin then begin
      FThread.StoreStepInfo;
      Finished := False;
      RemoveHiddenBreak;
      if AnEvent = deFinishedStep then
        AnEvent := deInternalContinue;
      exit;
    end;
  end;
  inherited DoResolveEvent(AnEvent, AnEventThread, Finished);
end;

{ TDbgControllerStepThroughFpcSpecialHandler }

procedure TDbgControllerStepThroughFpcSpecialHandler.DoResolveEvent(
  var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean);
begin
  if IsAtOrOutOfHiddenBreakFrame then
    RemoveHiddenBreak;

  Finished := IsSteppedOut or FDone or ((not HasHiddenBreak) and (NextInstruction.IsReturnInstruction));
  if Finished then
    AnEvent := deFinishedStep
  else
  if AnEvent = deFinishedStep then
    AnEvent := deInternalContinue;
end;

procedure TDbgControllerStepThroughFpcSpecialHandler.InternalContinue(
  AProcess: TDbgProcess; AThread: TDbgThread);
begin
  {$PUSH}{$Q-}{$R-}
  if (AThread = FThread) and
     (NextInstruction.IsCallInstruction) and
     (FThread.GetInstructionPointerRegisterValue + NextInstruction.InstructionLength = FAfterFinCallAddr)
  then begin
    RemoveHiddenBreak;
    FProcess.Continue(FProcess, FThread, True);
    FDone := True;
// TODO: last step => then single line step
    exit;
  end;
  {$POP}
  inherited InternalContinue(AProcess, AThread);
end;

procedure TDbgControllerStepThroughFpcSpecialHandler.Init;
begin
  InitStackFrameInfo;
  inherited Init;
end;

constructor TDbgControllerStepThroughFpcSpecialHandler.Create(
  AController: TDbgController; AnAfterFinCallAddr: TDbgPtr);
begin
  FAfterFinCallAddr := AnAfterFinCallAddr;
  inherited Create(AController);
end;

{ TFpDebugExceptionStepping.TAddressFrameList }

function TFpDebugExceptionStepping.TAddressFrameList.Add(const AKey: TDbgPtr): TFrameList;
begin
  Result := TFrameList.Create;
  inherited Add(AKey, Result);
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
    TFpDebugDebugger(Debugger).PrepareCallStackEntryList(1, ThreadArray[i]);
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

  if TFpDebugDebugger(Debugger).FDbgController.CurrentThread = nil then
    CurrentThreads.CurrentThreadId := 0 // TODO: only until controller is guranteed to have a currentthread
  else
    CurrentThreads.CurrentThreadId := TFpDebugDebugger(Debugger).FDbgController.CurrentThread.ID;
  CurrentThreads.SetValidity(ddsValid);
end;

procedure TFPThreads.ChangeCurrentThread(ANewId: Integer);
begin
  inherited ChangeCurrentThread(ANewId);
  if not(Debugger.State in [dsPause, dsInternalPause]) then exit;

  if TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.RequiresExecutionInDebuggerThread then
  begin
    TFpDebugDebugger(Debugger).FNewThreadId := ANewId;
    TFpDebugDebugger(Debugger).ExecuteInDebugThread(@TFpDebugDebugger(Debugger).DoChangeCurrentThreadId);
  end
  else begin
    TFpDebugDebugger(Debugger).FDbgController.CurrentThreadId := ANewId;
    if CurrentThreads <> nil
    then CurrentThreads.CurrentThreadId := ANewId;
  end;
  Changed;
end;

{ TFpDebugDebuggerProperties }

constructor TFpDebugDebuggerProperties.Create;
begin
  inherited Create;
  FNextOnlyStopOnStartLine:=False;
end;

procedure TFpDebugDebuggerProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TFpDebugDebuggerProperties then begin
    FNextOnlyStopOnStartLine := TFpDebugDebuggerProperties(Source).NextOnlyStopOnStartLine;
    FConsoleTty:=TFpDebugDebuggerProperties(Source).ConsoleTty;
    {$ifdef windows}
    FForceNewConsole:=TFpDebugDebuggerProperties(Source).FForceNewConsole;
    {$endif windows}
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
    if Assigned(FFpDebugDebugger.OnConsoleOutput) then
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

procedure TFpDbgMemReader.DoReadRegister;
begin
  FRegResult := inherited ReadRegister(FRegNum, FRegValue, FRegContext);
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

function TFpDbgMemReader.ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr;
  AContext: TFpDbgAddressContext): Boolean;
begin
  if FFpDebugDebugger.FDbgController.CurrentProcess.RequiresExecutionInDebuggerThread then
  begin
    FRegNum := ARegNum;
    FRegContext := AContext;
    FRegValue := 0; // TODO: error detection
    FFpDebugDebugger.ExecuteInDebugThread(@DoReadRegister);
    AValue := FRegValue;
    result := FRegResult;
  end
  else
    result := inherited ReadRegister(ARegNum, AValue, AContext);
end;

{ TCallstackAsyncRequest }

procedure TCallstackAsyncRequest.RequestAsync(Data: PtrInt);
var
  AThread: TDbgThread;
  CurCnt: LongInt;
  ThreadCallStack: TDbgCallstackEntryList;
  ReqCnt: Integer;
begin

  AThread := FDebugger.FDbgController.CurrentThread;
  if (AThread = nil) then begin
    FCallstack.SetCountValidity(ddsInvalid);
    FCallstack.SetHasAtLeastCountInfo(ddsInvalid);
    FRequiredMinCount := -1;
    FreeSelf;
    exit;
  end;

  ThreadCallStack := AThread.CallStackEntryList;
  if ThreadCallStack <> nil then
    CurCnt := ThreadCallStack.Count
  else
    CurCnt := 0;
  if (FRequiredMinCount > CurCnt) then begin
    ReqCnt := Min(CurCnt + 5, FRequiredMinCount);
    FDebugger.PrepareCallStackEntryList(ReqCnt);

    ThreadCallStack := AThread.CallStackEntryList;
    if ThreadCallStack <> nil then begin
      CurCnt := ThreadCallStack.Count;
      if (CurCnt < FRequiredMinCount) and (CurCnt >= ReqCnt) then begin
        Application.QueueAsyncCall(@RequestAsync, 0);
        exit;
      end;
    end;
  end;

  if (CurCnt = 0) then begin
    FCallstack.SetCountValidity(ddsInvalid);
    FCallstack.SetHasAtLeastCountInfo(ddsInvalid);
    FreeSelf;
    exit;
  end;

  if (FRequiredMinCount < 0) or (CurCnt < FRequiredMinCount) then
  begin
    FCallstack.Count := CurCnt;
    FCallstack.SetCountValidity(ddsValid);
  end
  else
  begin
    FCallstack.SetHasAtLeastCountInfo(ddsValid, CurCnt);
  end;

  // save whatever we have to history // limit to reduce time
  if (FRequiredMinCount < 1) then
    FCallstack.PrepareRange(0, Min(CurCnt, 10));

  FRequiredMinCount := -1;
  FreeSelf;
end;

procedure TCallstackAsyncRequest.FreeSelf;
begin
  if not FInDestroy then
    TFPCallStackSupplier(FDebugger.CallStack).FReqList.Remove(Self); // calls  Destroy;
end;

procedure TCallstackAsyncRequest.CallStackFreed(Sender: TObject);
begin
  FCallstack := nil;
  FRequiredMinCount := -1;
  FreeSelf;
end;

constructor TCallstackAsyncRequest.Create(ADebugger: TFpDebugDebugger;
  ACallstack: TCallStackBase; ARequiredMinCount: Integer);
begin
  FDebugger := ADebugger;
  FCallstack := ACallstack;
  FCallstack.AddFreeNotification(@CallStackFreed);
  FRequiredMinCount := ARequiredMinCount;
end;

destructor TCallstackAsyncRequest.Destroy;
begin
  assert(not FInDestroy, 'TCallstackAsyncRequest.Destroy: not FInDestroy');
  FInDestroy := True;
  if FRequiredMinCount >= 0 then begin
    FRequiredMinCount := -1;
    RequestAsync(0);
  end;
  Application.RemoveAsyncCalls(Self);
  if FCallstack <> nil then
    FCallstack.RemoveFreeNotification(@CallStackFreed);
  inherited Destroy;
end;

{ TFPCallStackSupplier }

function TFPCallStackSupplier.FpDebugger: TFpDebugDebugger;
begin
  Result := TFpDebugDebugger(Debugger);
end;

procedure TFPCallStackSupplier.DoStateLeavePause;
begin
  FReqList.Clear;
  if (TFpDebugDebugger(Debugger).FDbgController <> nil) and
     (TFpDebugDebugger(Debugger).FDbgController.CurrentProcess <> nil)
  then
    TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.ThreadsClearCallStack;
  inherited DoStateLeavePause;
end;

constructor TFPCallStackSupplier.Create(const ADebugger: TDebuggerIntf);
begin
  FReqList := TCallstackAsyncRequestList.Create;
  inherited Create(ADebugger);
  FPrettyPrinter := TFpPascalPrettyPrinter.Create(sizeof(pointer));
end;

destructor TFPCallStackSupplier.Destroy;
begin
  FReqList.Free;
  inherited Destroy;
  FPrettyPrinter.Free;
end;

procedure TFPCallStackSupplier.RequestCount(ACallstack: TCallStackBase);
begin
  RequestAtLeastCount(ACallstack, -1);
end;

procedure TFPCallStackSupplier.RequestAtLeastCount(ACallstack: TCallStackBase;
  ARequiredMinCount: Integer);
var
  r: TCallstackAsyncRequest;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause])
  then begin
    ACallstack.SetCountValidity(ddsInvalid);
    exit;
  end;
  r := TCallstackAsyncRequest.Create(FpDebugger, ACallstack, ARequiredMinCount);
  FReqList.add(r);
  r.RequestAsync(0);
end;

procedure TFPCallStackSupplier.RequestEntries(ACallstack: TCallStackBase);
var
  e: TCallStackEntry;
  It: TMapIterator;
  ThreadCallStack: TDbgCallstackEntryList;
  cs: TDbgCallstackEntry;
begin
  It := TMapIterator.Create(ACallstack.RawEntries);
  ThreadCallStack := FpDebugger.FDbgController.CurrentThread.CallStackEntryList;

  if not It.Locate(ACallstack.LowestUnknown )
  then if not It.EOM
  then It.Next;

  while (not IT.EOM) and (TCallStackEntry(It.DataPtr^).Index <= ACallstack.HighestUnknown)
  do begin
    e := TCallStackEntry(It.DataPtr^);
    if e.Validity = ddsRequested then
    begin
      cs := ThreadCallStack[e.Index];
      e.Init(cs.AnAddress, nil,
        cs.FunctionName + FpDebugger.GetParamsAsString(cs, FPrettyPrinter),
        cs.SourceFile, '', cs.Line, ddsValid);
    end;
    It.Next;
  end;
  It.Free;
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
  if (Debugger = nil) or not(Debugger.State = dsPause) then begin // dsInternalPause ?
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
  ProcVal: TFpValue;
  i: Integer;
  m: TFpValue;
  n, v: String;
  CurThreadId, CurStackFrame: Integer;
begin
  AController := FpDebugger.FDbgController;
  if (AController = nil) or (AController.CurrentProcess = nil) or
     (AController.CurrentProcess.DbgInfo = nil)
  then begin
    ALocals.SetDataValidity(ddsInvalid);
    exit;
  end;

  TFpDebugDebugger(Debugger).GetCurrentThreadAndStackFrame(CurThreadId, CurStackFrame);
  AContext := FpDebugger.FindContext(CurThreadId, CurStackFrame);
  if AContext = nil then begin
    ALocals.SetDataValidity(ddsInvalid);
    exit;
  end;

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
  FPrettyPrinter.MemManager := AContext.MemManager;
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
      m.ReleaseReference;
      ALocals.Add(n, v);
    end;
  end;
  ALocals.SetDataValidity(ddsValid);
  ProcVal.ReleaseReference;
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
  ABrkPoint: FpDbgClasses.TFpDbgBreakpoint;
  i: Integer;
begin
  inherited DoStateChange(AOldState);
  if Debugger.State in [dsPause, dsInternalPause, dsStop] then
  begin
    if FDelayedRemoveBreakpointList.Count>0 then begin
      debuglnEnter(DBG_BREAKPOINTS, ['TFPBreakpoints.DoStateChange  REMOVE DELAYED']);
      for i := FDelayedRemoveBreakpointList.Count-1 downto 0 do
      begin
        ABrkPoint := FpDbgClasses.TFpDbgBreakpoint(FDelayedRemoveBreakpointList[i]);
        TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.RemoveBreak(ABrkPoint);
        TFpDebugDebugger(Debugger).FreeBreakpoint(ABrkPoint);
        ABrkPoint := nil;
        FDelayedRemoveBreakpointList.Delete(i);
      end;
      debuglnExit(DBG_BREAKPOINTS, ['<< TFPBreakpoints.DoStateChange  REMOVE DELAYED ' ]);
    end;
  end;
end;

procedure TFPBreakpoints.AddBreakpointToDelayedRemoveList(ABreakpoint: FpDbgClasses.TFpDbgBreakpoint);
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

function TFPBreakpoints.Find(AIntBReakpoint: FpDbgClasses.TFpDbgBreakpoint): TDBGBreakPoint;
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
var
  CurThreadId, CurStackFrame: Integer;
  CurContext: TFpDbgInfoContext;
  WatchPasExpr: TFpPascalExpression;
  R: TFpValue;
  s: TFpDbgValueSize;
begin
  assert(FInternalBreakpoint=nil);
  debuglnEnter(DBG_BREAKPOINTS, ['>> TFPBreakpoint.SetBreak  ADD ',FSource,':',FLine,'/',dbghex(Address),' ' ]);
  case Kind of
    bpkAddress:   FInternalBreakpoint := TFpDebugDebugger(Debugger).AddBreak(Address);
    bpkSource:    FInternalBreakpoint := TFpDebugDebugger(Debugger).AddBreak(Source, cardinal(Line));
    bpkData: begin
        TFpDebugDebugger(Debugger).GetCurrentThreadAndStackFrame(CurThreadId, CurStackFrame);
        CurContext := TFpDebugDebugger(Debugger).GetContextForEvaluate(CurThreadId, CurStackFrame);
        if CurContext <> nil then begin
          WatchPasExpr := TFpPascalExpression.Create(WatchData, CurContext);
          R := WatchPasExpr.ResultValue; // Address and Size
// TODO: Cache current value
          if WatchPasExpr.Valid and IsTargetNotNil(R.Address) and R.GetSize(s) then begin
// pass context
            FInternalBreakpoint := TFpDebugDebugger(Debugger).AddWatch(R.Address.Address, SizeToFullBytes(s), WatchKind, WatchScope);
          end;
          WatchPasExpr.Free;
          CurContext.ReleaseReference;
        end;
      end;
  end;
  debuglnExit(DBG_BREAKPOINTS, ['<< TFPBreakpoint.SetBreak ' ]);
  FIsSet:=true;
  if not assigned(FInternalBreakpoint) then
    FValid:=vsInvalid // pending?
  else
    FValid:=vsValid;
end;

procedure TFPBreakpoint.ResetBreak;
begin
  // If Debugger is not assigned, the Controller's currentprocess is already
  // freed. And so are the corresponding InternalBreakpoint's.
  if assigned(Debugger) and assigned(FInternalBreakpoint) then
    begin
    debuglnEnter(DBG_BREAKPOINTS, ['>> TFPBreakpoint.ResetBreak  REMOVE ',FSource,':',FLine,'/',dbghex(Address),' ' ]);
    TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.RemoveBreak(FInternalBreakpoint);
    TFpDebugDebugger(Debugger).FreeBreakpoint(FInternalBreakpoint);
    FInternalBreakpoint := nil;
    debuglnExit(DBG_BREAKPOINTS, ['<< TFPBreakpoint.ResetBreak ' ]);
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
    debuglnEnter(DBG_BREAKPOINTS, ['>> TFPBreakpoint.DoStateChange  REMOVE ',FSource,':',FLine,'/',dbghex(Address),' ' ]);
    TFpDebugDebugger(Debugger).FreeBreakpoint(FInternalBreakpoint);
    debuglnExit(DBG_BREAKPOINTS, ['<< TFPBreakpoint.DoStateChange ' ]);
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
  ARange, AReversedRange: TDBGDisassemblerEntryRange;
  AnEntry: TDisassemblerEntry;
  CodeBin: TBytes;
  p: pointer;
  ADump,
  AStatement,
  ASrcFileName: string;
  ASrcFileLine: integer;
  i,j, sz, bytesDisassembled, bufOffset: Integer;
  Sym: TFpSymbol;
  StatIndex: integer;
  FirstIndex: integer;
  ALastAddr, tmpAddr, tmpPointer, prevInstructionSize: TDBGPtr;
  ADisassembler: TDbgAsmDecoder;

begin
  Result := False;
  if (Debugger = nil) or not(Debugger.State = dsPause) then
    exit;

  ADisassembler := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.Disassembler;
  Sym:=nil;
  ASrcFileLine:=0;
  ASrcFileName:='';
  StatIndex:=0;
  FirstIndex:=0;
  ARange := TDBGDisassemblerEntryRange.Create;
  ARange.RangeStartAddr:=AnAddr;
  ALastAddr:=0;

  if (ALinesBefore > 0) and
    ADisassembler.CanReverseDisassemble then
   begin
     AReversedRange := TDBGDisassemblerEntryRange.Create;
     tmpAddr := AnAddr;  // do not modify AnAddr in this loop
     // Large enough block of memory for whole loop
     sz := ADisassembler.MaxInstructionSize * ALinesBefore;
     SetLength(CodeBin, sz);

     // TODO: Check if AnAddr is at lower address than length(CodeBin)
     //       and ensure ReadData size doesn't exceed available target memory.
     //       Fill out of bounds memory in buffer with "safe" value e.g. 0
     if sz + ADisassembler.MaxInstructionSize > AnAddr then
     begin
       FillByte(CodeBin[0], sz, 0);
       // offset into buffer where active memory should start
       bufOffset := sz - AnAddr;
       // size of active memory to read
       sz := integer(AnAddr);
     end
     else
     begin
       bufOffset := 0;
     end;

     // Everything now counts back from starting address...
     bytesDisassembled := 0;
     // Only read up to byte before this address
     if not TFpDebugDebugger(Debugger).ReadData(tmpAddr-sz, sz, CodeBin[bufOffset]) then
       DebugLn(Format('Reverse disassemble: Failed to read memory at %s.', [FormatAddress(tmpAddr)]))
     else
       for i := 0 to ALinesBefore-1 do
       begin
         if bytesDisassembled >= sz then
           break;

         tmpPointer := TDBGPtr(@CodeBin[bufOffset]) + TDBGPtr(sz) - TDBGPtr(bytesDisassembled);
         p := pointer(tmpPointer);
         ADisassembler.ReverseDisassemble(p, ADump, AStatement); // give statement before pointer p, pointer p points to decoded instruction on return
         prevInstructionSize := tmpPointer - PtrUInt(p);
         bytesDisassembled := bytesDisassembled + prevInstructionSize;
         DebugLn(DBG_VERBOSE, format('Disassembled: [%.8X:  %s] %s',[tmpAddr, ADump, Astatement]));

         Dec(tmpAddr, prevInstructionSize);
         Sym := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.FindProcSymbol(tmpAddr);
         // If this is the last statement for this source-code-line, fill the
         // SrcStatementCount from the prior statements.
         if (assigned(sym) and ((ASrcFileName<>sym.FileName) or (ASrcFileLine<>sym.Line))) or
           (not assigned(sym) and ((ASrcFileLine<>0) or (ASrcFileName<>''))) then
         begin
           for j := 0 to StatIndex-1 do
           begin
             with AReversedRange.EntriesPtr[FirstIndex+j]^ do
               SrcStatementCount := StatIndex;
           end;
           StatIndex := 0;
           FirstIndex := i;
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
         AnEntry.Addr := tmpAddr;
         AnEntry.Dump := ADump;
         AnEntry.Statement := AStatement;
         AnEntry.SrcFileLine:=ASrcFileLine;
         AnEntry.SrcFileName:=ASrcFileName;
         AnEntry.SrcStatementIndex:=StatIndex;  // should be inverted for reverse parsing
         AReversedRange.Append(@AnEntry);
         inc(StatIndex);
       end;

     if AReversedRange.Count>0 then
     begin
       // Update start of range
       ARange.RangeStartAddr := tmpAddr;
       // Copy range in revese order of entries
       for i := 0 to AReversedRange.Count-1 do
       begin
         // Reverse order of statements
         with AReversedRange.Entries[AReversedRange.Count-1 - i] do
         begin
           for j := 0 to SrcStatementCount-1 do
             SrcStatementIndex := SrcStatementCount - 1 - j;
         end;

         ARange.Append(AReversedRange.EntriesPtr[AReversedRange.Count-1 - i]);
       end;
     end;
     // Entries are all pointers, don't free entries
     FreeAndNil(AReversedRange);
   end;

  if ALinesAfter > 0 then
  begin
  sz := ALinesAfter * ADisassembler.MaxInstructionSize;
  SetLength(CodeBin, sz);
  bytesDisassembled := 0;
  if not TFpDebugDebugger(Debugger).ReadData(AnAddr, sz, CodeBin[0]) then
    begin
    DebugLn(Format('Disassemble: Failed to read memory at %s.', [FormatAddress(AnAddr)]));
    inc(AnAddr);
    end
  else
    for i := 0 to ALinesAfter-1 do
      begin
      p := @CodeBin[bytesDisassembled];
      ADisassembler.Disassemble(p, ADump, AStatement);

      prevInstructionSize := p - @CodeBin[bytesDisassembled];
      bytesDisassembled := bytesDisassembled + prevInstructionSize;
      Sym := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.FindProcSymbol(AnAddr);
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
      Inc(AnAddr, prevInstructionSize);
      end;
  end
  else
    ALastAddr := AnAddr;

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
  thr: TDbgThread;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause, dsStop]) then
    exit;

  if not TFpDebugDebugger(Debugger).FDbgController.MainProcess.GetThread(ARegisters.ThreadId, thr) then begin
    ARegisters.DataValidity:=ddsError;
    exit;
  end;
  ARegisterList :=  thr.RegisterValueList;
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

function TFpDebugThread.GetLoopIsRunnig: LongBool;
begin
  Result := longbool(InterLockedExchangeAdd(longint(FLoopIsRunnig), 0));
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
    InterLockedExchange(longint(FLoopIsRunnig), ord(LongBool(False)));
    RTLeventWaitFor(FStartDebugLoopEvent);
    InterLockedExchange(longint(FLoopIsRunnig), ord(LongBool(True)));
    RTLeventResetEvent(FStartDebugLoopEvent);
    if not terminated then
      begin
      if assigned(FAsyncMethod) then
        begin
        try
          FAsyncMethod();
        except
          on E: Exception do
            debugln(['FATAL: ',e.Message]);
        end;
        InterLockedExchange(longint(FLoopIsRunnig), ord(LongBool(False)));
        RTLeventSetEvent(FDebugLoopStoppedEvent);
        end
      else
        begin
        FFpDebugDebugger.FDbgController.ProcessLoop;
        InterLockedExchange(longint(FLoopIsRunnig), ord(LongBool(False))); // The main thread can set the start event.
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

{ TFpDebugExceptionStepping }

function TFpDebugExceptionStepping.GetDbgController: TDbgController;
begin
  Result := FDebugger.FDbgController;
end;

function TFpDebugExceptionStepping.dbgs(st: TExceptStepState): string;
begin
  writestr(Result, st);
end;

function TFpDebugExceptionStepping.dbgs(loc: TBreakPointLoc): string;
begin
  writestr(Result, loc);
end;

function TFpDebugExceptionStepping.dbgs(locs: TBreakPointLocs): string;
var
  a: TBreakPointLoc;
begin
  Result := '';
  for a in locs do Result := Result + dbgs(a) +',';
end;

function TFpDebugExceptionStepping.GetCurrentProcess: TDbgProcess;
begin
  Result := FDebugger.FDbgController.CurrentProcess;
end;

function TFpDebugExceptionStepping.GetCurrentCommand: TDbgControllerCmd;
begin
  Result := FDebugger.FDbgController.CurrentCommand;
end;

function TFpDebugExceptionStepping.GetCurrentThread: TDbgThread;
begin
  Result := FDebugger.FDbgController.CurrentThread;
end;

procedure TFpDebugExceptionStepping.EnableBreaks(ALocs: TBreakPointLocs);
var
  a: TBreakPointLoc;
begin
  // Not in thread => only flag desired changes
  for a in ALocs do
    Include(FBreakNewEnabled, a);
end;

procedure TFpDebugExceptionStepping.EnableBreaksDirect(ALocs: TBreakPointLocs);
var
  a: TBreakPointLoc;
begin
  // Running in debug thread
  //debugln(['EnableBreaksDirect ', dbgs(ALocs)]);
  for a in ALocs do
    if FBreakPoints[a] <> nil then begin
      if not(a in FBreakEnabled) then
        FBreakPoints[a].SetBreak;
      Include(FBreakEnabled, a);
      Include(FBreakNewEnabled, a);
    end;
end;

procedure TFpDebugExceptionStepping.DisableBreaks(ALocs: TBreakPointLocs);
var
  a: TBreakPointLoc;
begin
  // Not in thread => only flag desired changes
  //debugln(['DisableBreaks ', dbgs(ALocs)]);
  for a in ALocs do
    Exclude(FBreakNewEnabled, a);
end;

procedure TFpDebugExceptionStepping.DisableBreaksDirect(ALocs: TBreakPointLocs);
var
  a: TBreakPointLoc;
begin
  // Running in debug thread
  //debugln(['DisableBreaksDirect ', dbgs(ALocs)]);
  for a in ALocs do
    if FBreakPoints[a] <> nil then begin
      if (a in FBreakEnabled) then
        FBreakPoints[a].ResetBreak;
      Exclude(FBreakEnabled, a);
      Exclude(FBreakNewEnabled, a);
    end;
end;

procedure TFpDebugExceptionStepping.SetStepOutAddrDirect(AnAddr: TDBGPtr);
begin
  FreeAndNil(FBreakPoints[bplStepOut]);
  FBreakPoints[bplStepOut] := CurrentProcess.AddBreak(AnAddr);
end;

procedure TFpDebugExceptionStepping.DoExceptionRaised(var &continue: boolean);
var
  AnExceptionLocation: TDBGLocationRec;
begin
  FDebugger.HandleSoftwareException(AnExceptionLocation, &continue);

  case &continue of
    True: begin
        if (CurrentCommand <> nil) and not(CurrentCommand is TDbgControllerContinueCmd) and
           (CurrentCommand.Thread = CurrentThread)
        then begin
          EnableBreaks([bplPopExcept, bplCatches, bplFpcSpecific]);
          FState := esIgnoredRaise; // currently stepping
        end;
      end;
    False:
      begin
        FDebugger.EnterPause(AnExceptionLocation);
        FState := esStoppedAtRaise;
      end;
  end;
end;

//procedure TFpDebugExceptionStepping.DoPopExcptStack;
//begin
//  // check if step over??
//  // clear breaks
//  DbgController.AbortCurrentCommand;
//  DbgController.StepOut;
//  FState := esNone;
//
//  DisableBreaks([bplPopExcept, bplCatches, bplFpcSpecific]);
//end;

procedure TFpDebugExceptionStepping.DoRtlUnwindEx;
begin

end;

constructor TFpDebugExceptionStepping.Create(ADebugger: TFpDebugDebugger);
begin
  FDebugger := ADebugger;
  FAddressFrameList := TAddressFrameList.Create(True);
end;

destructor TFpDebugExceptionStepping.Destroy;
begin
  DoDbgStopped;
  inherited Destroy;
  FAddressFrameList.Destroy;
end;

procedure TFpDebugExceptionStepping.DoProcessLoaded;
begin
  debuglnEnter(DBG_BREAKPOINTS, ['>> TFpDebugDebugger.SetSoftwareExceptionBreakpoint FPC_RAISEEXCEPTION' ]);
  FBreakPoints[bplRaise]         := FDebugger.AddBreak('FPC_RAISEEXCEPTION');
  //FBreakPoints[bplBreakError]    := FDebugger.AddBreak('FPC_BREAK_ERROR');
  //FBreakPoints[bplRunError]      := FDebugger.AddBreak('FPC_RUNERROR');
  FBreakPoints[bplReRaise]       := FDebugger.AddBreak('FPC_RERAISE', nil,            False);
  FBreakPoints[bplPopExcept]     := FDebugger.AddBreak('FPC_POPADDRSTACK', nil,       False);
  FBreakPoints[bplCatches]       := FDebugger.AddBreak('FPC_CATCHES', nil,            False);
  FBreakPoints[bplFpcSpecific]   := FDebugger.AddBreak('__FPC_specific_handler', nil, False);
  FBreakPoints[bplSehW64Except]  := FDebugger.AddBreak(0, False);
  FBreakPoints[bplSehW64Finally] := FDebugger.AddBreak(0, False);
  debuglnExit(DBG_BREAKPOINTS, ['<< TFpDebugDebugger.SetSoftwareExceptionBreakpoint ' ]);
end;

procedure TFpDebugExceptionStepping.DoNtDllLoaded(ALib: TDbgLibrary);
begin
  debugln(DBG_BREAKPOINTS, ['SetSoftwareExceptionBreakpoint RtlUnwind']);
  FBreakPoints[bplRtlUnwind].Free;
  FBreakPoints[bplRtlUnwind] := FDebugger.AddBreak('RtlUnwindEx', ALib, False);
end;

procedure TFpDebugExceptionStepping.DoDbgStopped;
var
  a: TBreakPointLoc;
begin
  debuglnEnter(DBG_BREAKPOINTS, ['>> TFpDebugDebugger.FDbgControllerProcessExitEvent fpc_Raiseexception' ]);
  for a in TBreakPointLoc do
    FreeAndNil(FBreakPoints[a]);
  debuglnExit(DBG_BREAKPOINTS, ['<< TFpDebugDebugger.FDbgControllerProcessExitEvent ' ]);
end;

procedure TFpDebugExceptionStepping.ThreadBeforeLoop(Sender: TObject);
begin
  // Running in debug thread
  EnableBreaksDirect(FBreakNewEnabled - FBreakEnabled);
  DisableBreaksDirect(FBreakEnabled - FBreakNewEnabled);
end;

procedure TFpDebugExceptionStepping.ThreadProcessLoopCycle(
  var AFinishLoopAndSendEvents: boolean; var AnEventType: TFPDEvent;
  var ACurCommand: TDbgControllerCmd; var AnIsFinished: boolean);

  function CheckCommandFinishesInFrame(AFrameAddr: TDBGPtr): Boolean;
  begin
    Result := (ACurCommand is TDbgControllerHiddenBreakStepBaseCmd) and
              (TDbgControllerHiddenBreakStepBaseCmd(CurrentCommand).StoredStackFrameInfo <> nil);
    if not Result then
      exit; // none stepping command, does not stop
    if ACurCommand is TDbgControllerStepOutCmd then
      Result := TDbgControllerHiddenBreakStepBaseCmd(CurrentCommand).StoredStackFrameInfo.StoredStackFrame < AFrameAddr
    else
      Result := TDbgControllerHiddenBreakStepBaseCmd(CurrentCommand).StoredStackFrameInfo.StoredStackFrame <= AFrameAddr;
  end;

  procedure CheckSteppedOutFromW64SehFinally;
  var
    sym: TFpSymbol;
    r: Boolean;
  begin
    if (FState <> esNone) or (not(ACurCommand is TDbgControllerLineStepBaseCmd)) or
       (ACurCommand.Thread <> CurrentThread)
    then
      exit;

    if (pos('fin$', TDbgControllerLineStepBaseCmd(ACurCommand).StartedInFuncName) < 1) then
      exit;

    if (not TDbgControllerLineStepBaseCmd(ACurCommand).IsSteppedOut) then begin
      EnableBreaksDirect([bplFpcSpecific]);
      exit;
    end;

    sym := CurrentProcess.FindProcSymbol(CurrentThread.GetInstructionPointerRegisterValue);
    r := (sym <> nil) and (UpperCase(sym.Name) <> '__FPC_SPECIFIC_HANDLER') and
         (sym.FileName <> '');
    sym.ReleaseReference;
    if r then
      exit;

    FState := esSteppingFpcSpecialHandler;
    AFinishLoopAndSendEvents := False;
    ACurCommand := TDbgControllerStepThroughFpcSpecialHandler.Create(DbgController, CurrentThread.GetInstructionPointerRegisterValue);
  end;

  procedure StepOutFromPopCatches;
  begin
    ACurCommand := TDbgControllerStepOutCmd.Create(DbgController);
    TDbgControllerStepOutCmd(ACurCommand).SetReturnAdressBreakpoint(CurrentProcess, True);
  end;

const
  MaxFinallyHandlerCnt = 256; // more finally in a single proc is not probable....
var
  StepOutStackPos, ReturnAddress, Base, HData, ImgBase, Addr: TDBGPtr;
  Rdx, Rcx, R8, R9, PC: TDBGPtr;
  o, i: Integer;
  EFlags, Cnt: Cardinal;
  Frames: TFrameList;
  FinallyData: Array of array [0..3] of DWORD;
begin
  if CurrentThread = nil then
    exit;
  if (FState = esSteppingFpcSpecialHandler) and AnIsFinished and
     (ACurCommand is TDbgControllerStepThroughFpcSpecialHandler)
  then begin
    if TDbgControllerStepThroughFpcSpecialHandler(ACurCommand).FDone then begin
      FState := esNone;
      if ACurCommand.Thread = CurrentThread then
        ACurCommand := TDbgControllerStepOverFirstFinallyLineCmd.Create(DbgController);
      // else thread has gone => finish old command
    end
    else begin
      FState := esStepToFinally;
      ACurCommand := nil; // run
      EnableBreaksDirect([bplFpcSpecific]);
    end;
    AFinishLoopAndSendEvents := False;
    exit;
  end
  else
  if CurrentProcess.CurrentBreakpoint = nil then begin
    CheckSteppedOutFromW64SehFinally;
    exit;
  end;
  DisableBreaksDirect([bplRtlUnwind, bplSehW64Finally]); // bplRtlUnwind must always be unset;

  PC := CurrentThread.GetInstructionPointerRegisterValue;
  // bplPopExcept / bplCatches
  if (assigned(FBreakPoints[bplPopExcept]) and FBreakPoints[bplPopExcept].HasLocation(PC)) or
     (assigned(FBreakPoints[bplCatches]) and FBreakPoints[bplCatches].HasLocation(PC))
  then begin
    debugln(FPDBG_COMMANDS, ['@ bplPop/bplCatches ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    // TODO: esStepToFinally has "CurrentCommand = nil" and is Running, not stepping => thread not avail
    if (CurrentCommand <> nil) and (CurrentCommand.Thread <> CurrentThread) then
      exit;

    //DebugLn(['THreadProcLoop ', dbgs(FState), ' ', DbgSName(CurrentCommand)]);
    DisableBreaksDirect([bplPopExcept, bplCatches, bplFpcSpecific]); // FpcSpecific was not needed -> not SEH based code
    case FState of
      esIgnoredRaise: begin
          // bplReRaise may set them again
          if not (CurrentCommand is TDbgControllerHiddenBreakStepBaseCmd) then
            exit; // wrong command type // should not happen

          if AnIsFinished then begin
// FORCE the breakpoint WITHoUT FRAME => known to be without frame // optimized fpc may not have expected asm
            StepOutFromPopCatches;
          end
          else begin
            o := 0;
            if (CurrentCommand is TDbgControllerStepOutCmd) then
              o := 1; // frame must be less, not equal

            {$PUSH}{$Q-}{$R-}
            // GetStackBasePointerRegisterValue is still on parent frame
            if CheckCommandFinishesInFrame(CurrentThread.GetStackBasePointerRegisterValue - o)
            then begin
              // Insert a "step out" breakpoint, but leave control to the running command.
              StepOutStackPos := CurrentThread.GetStackPointerRegisterValue;
              if CurrentProcess.ReadAddress(StepOutStackPos, ReturnAddress) then
                SetStepOutAddrDirect(ReturnAddress)
              else
                StepOutFromPopCatches; // error reading mem
            end;
            {$POP}
          end;
        end;
      esStepToFinally: begin
          StepOutFromPopCatches;
        end;
    end;
    FState := esNone;
  end
  else
  // bplStepOut => part of esIgnoredRaise
  if assigned(FBreakPoints[bplStepOut]) and FBreakPoints[bplStepOut].HasLocation(PC) then begin
    debugln(FPDBG_COMMANDS, ['@ bplStepOut ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    if (CurrentCommand = nil) or (CurrentCommand.Thread <> CurrentThread) then
      exit;
    AFinishLoopAndSendEvents := AnIsFinished;
    CurrentProcess.RemoveBreak(FBreakPoints[bplStepOut]);
    FreeAndNil(FBreakPoints[bplStepOut]);
  end
  else
  // bplReRaise
  if assigned(FBreakPoints[bplReRaise]) and FBreakPoints[bplReRaise].HasLocation(PC) then begin
    debugln(FPDBG_COMMANDS, ['@ bplReRaise ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    if (CurrentCommand = nil) or (CurrentCommand.Thread <> CurrentThread) then
      exit;
    EnableBreaksDirect([bplPopExcept, bplCatches, bplFpcSpecific]);
    // if not(FState = esStepToFinally) then
    FState := esIgnoredRaise;
  end
  else
  (* ***** Win64 SEH ***** *)
  // bplFpcSpecific
  if assigned(FBreakPoints[bplFpcSpecific]) and FBreakPoints[bplFpcSpecific].HasLocation(PC) then begin
    debugln(FPDBG_COMMANDS, ['@ bplFpcSpecific ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    // TODO: esStepToFinally has "CurrentCommand = nil" and is Running, not stepping => thread not avail
    if (CurrentCommand <> nil) and (CurrentCommand.Thread <> CurrentThread) then
      exit;
    EnableBreaksDirect([bplRtlUnwind]);
    FBreakPoints[bplSehW64Finally].RemoveAllAddresses;

    if (FState = esIgnoredRaise) and not(CurrentCommand is TDbgControllerHiddenBreakStepBaseCmd) then
      exit; // wrong command type // should not happen

    (* TODO: Look at using DW_TAG_try_block https://bugs.freepascal.org/view.php?id=34881 *)

    {$PUSH}{$Q-}{$R-}
    Rcx := CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(2).NumValue; // rec: TExceptionRecord
    {$PUSH}{$Q-}{$R-}
    if (not CurrentProcess.ReadData(Rcx + 4, 4, EFlags)) or
       ((EFlags and 66) = 0) // rec.ExceptionFlags and EXCEPTION_UNWIND)=0
    then
      exit;
    {$POP}

    R8  := CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(8).NumValue;
    if (not CurrentProcess.ReadAddress(R8 + 160, Base)) or (Base = 0) then // RPB at finally
      exit;
    if (FState = esIgnoredRaise) and
       not CheckCommandFinishesInFrame(Base)
    then
      exit;

    R9  := CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(9).NumValue;
    //dispatch.HandlerData
    if (not CurrentProcess.ReadAddress(R9 + 56, HData)) or (HData = 0) then
      exit;
    if (not CurrentProcess.ReadData(HData, 4, Cnt)) or (Cnt = 0) or (Cnt > MaxFinallyHandlerCnt) then
      exit;

    if (not CurrentProcess.ReadAddress(R9 + 8, ImgBase)) or (ImgBase = 0) then
      exit;

    SetLength(FinallyData, Cnt);
    if (not CurrentProcess.ReadData(HData + 4, 16 * Cnt, FinallyData[0])) then
      exit;
    for i := 0 to Cnt - 1 do begin
      Addr := FinallyData[i][3];
      if (FinallyData[i][0] <> 0) or // scope^.Typ=SCOPE_FINALLY
         (Addr = 0)
      then
        Continue;
      FBreakPoints[bplSehW64Finally].AddAddress(ImgBase + Addr);
    end;
    {$POP}
    FBreakPoints[bplSehW64Finally].SetBreak;
  end
  else
  // bplRtlUnwind
  if assigned(FBreakPoints[bplRtlUnwind]) and FBreakPoints[bplRtlUnwind].HasLocation(PC) then begin
    debugln(FPDBG_COMMANDS, ['@ bplRtlUnwind ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    // This is Win64 bit only
    // Must run for any thread => the thread may stop at a break in a finally block, and then attempt to step to except
    // maybe store the thread-id with each breakpoint // though SP register values should be unique
    Rcx := CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(2).NumValue; // rsp at target
    Rdx := CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(1).NumValue;
    if (Rcx <> 0) and (Rdx <> 0) then begin
      o := FAddressFrameList.IndexOf(Rdx);
      if o >= 0 then
        Frames := FAddressFrameList.Data[o]
      else
        Frames := FAddressFrameList.Add(Rdx);
      if Frames.IndexOf(Rcx) >= 0 then
        exit;
      Frames.Add(Rcx);
      FBreakPoints[bplSehW64Except].AddAddress(Rdx);
      FBreakPoints[bplSehW64Except].SetBreak;
    end;
  end
  else
  // bplSehW64Except
  if assigned(FBreakPoints[bplSehW64Except]) and FBreakPoints[bplSehW64Except].HasLocation(PC) then begin // always assigned
    debugln(FPDBG_COMMANDS, ['@ bplSehW64Except ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    FBreakPoints[bplSehW64Finally].RemoveAllAddresses;
    o := FAddressFrameList.IndexOf(PC);
    if o >= 0 then begin
      Frames := FAddressFrameList.Data[o];
      Frames.Remove(CurrentThread.GetStackPointerRegisterValue);
      if Frames.Count = 0 then begin
        FBreakPoints[bplSehW64Except].RemoveAddress(Rdx);
        FAddressFrameList.Delete(o);
      end;
    end
    else
      FBreakPoints[bplSehW64Except].RemoveAddress(Rdx);

    // TODO: esStepToFinally has "CurrentCommand = nil" and is Running, not stepping => thread not avail
    if (CurrentCommand <> nil) and (CurrentCommand.Thread <> CurrentThread) then
      exit;

    if (not (FState in [esStepToFinally, esSteppingFpcSpecialHandler])) and
       not(CurrentCommand is TDbgControllerHiddenBreakStepBaseCmd)
    then
      exit; // wrong command type / should not happen
    if (FState = esIgnoredRaise) and
       (not CheckCommandFinishesInFrame(CurrentThread.GetStackBasePointerRegisterValue))
    then
      exit;

    AFinishLoopAndSendEvents := True; // Stop at this address
    FState := esAtWSehExcept;
    AnIsFinished := True;
    AnEventType := deFinishedStep;
  end
  else
  // bplSehW64Finally
  if assigned(FBreakPoints[bplSehW64Finally]) and FBreakPoints[bplSehW64Finally].HasLocation(PC) then begin // always assigned
    debugln(FPDBG_COMMANDS, ['@ bplSehW64Finally ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    // TODO: esStepToFinally has "CurrentCommand = nil" and is Running, not stepping => thread not avail
    if (CurrentCommand <> nil) and (CurrentCommand.Thread <> CurrentThread) then
      exit;
    FBreakPoints[bplSehW64Finally].RemoveAllAddresses;
    // step over proloque
    ACurCommand := TDbgControllerStepOverFirstFinallyLineCmd.Create(DbgController);
    FState := esStepSehFinallyProloque;
  end
  else
    CheckSteppedOutFromW64SehFinally;

end;

function TFpDebugExceptionStepping.BreakpointHit(var &continue: boolean;
  const Breakpoint: TFpDbgBreakpoint): boolean;
begin
  if FState in [esAtWSehExcept] then begin
    FDebugger.EnterPause(FDebugger.GetLocation);
    FState := esNone;
    exit;
  end;

  Result := Assigned(Breakpoint);
  if not Result then begin
    exit;
  end;

  if BreakPoint = FBreakPoints[bplRaise] then begin
    debugln(FPDBG_COMMANDS, ['@ bplRaise']);
    DoExceptionRaised(&continue);
  end
  else
    Result := False;
end;

procedure TFpDebugExceptionStepping.UserCommandRequested(
  var ACommand: TDBGCommand);
var
  st: TExceptStepState;
begin
  // This only runs if the debugloop is paused
  st := FState;
  FState := esNone;
  DisableBreaks([bplPopExcept, bplCatches, bplFpcSpecific,
    bplReRaise,
    bplRtlUnwind, bplStepOut]);

  if ACommand in [dcStepInto, dcStepOver, dcStepOut, dcStepTo, dcStepOverInstr{, dcStepIntoInstr}] then
    EnableBreaks([bplReRaise]);
  if ACommand in [dcStepOut] then
    EnableBreaks([bplFpcSpecific]);

  case st of
    esStoppedAtRaise: begin
      if ACommand in [dcStepInto, dcStepOver, dcStepOut, dcStepTo] then begin
        FState := esStepToFinally;
        ACommand := dcRun;
        EnableBreaks([bplPopExcept, bplCatches, bplFpcSpecific]);
      end
    end;
  end;
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
  LockRelease;
  try
    SetState(dsStop);
    FExceptionStepper.DoDbgStopped;
    FreeDebugThread;
  finally
    UnlockRelease;
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
  APasExpr, PasExpr2: TFpPascalExpression;
  DispFormat: TWatchDisplayFormat;
  RepeatCnt: Integer;
  Res: Boolean;
  StackFrame, ThreadId: Integer;
  ResValue: TFpValue;
  CastName, ResText2: String;
  ClassAddr, CNameAddr: TFpDbgMemLocation;
  NameLen: QWord;
begin
  Result := False;
  AResText := '';
  ATypeInfo := nil;

  if AWatchValue <> nil then begin
    StackFrame := AWatchValue.StackFrame;
    ThreadId := AWatchValue.ThreadId;
    DispFormat := AWatchValue.DisplayFormat;
    RepeatCnt := AWatchValue.RepeatCount;
    EvalFlags := AWatchValue.EvaluateFlags;
  end
  else begin
    GetCurrentThreadAndStackFrame(ThreadId, StackFrame);
    DispFormat := wdfDefault;
    RepeatCnt := -1;
  end;

  AContext := GetContextForEvaluate(ThreadId, StackFrame);

  if AContext = nil then
    begin
    if AWatchValue <> nil then
      AWatchValue.Validity := ddsInvalid;
    exit;
    end;

  Result := True;
  APasExpr := nil;
  try
    APasExpr := TFpPascalExpression.Create(AExpression, AContext);
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

      ResValue := APasExpr.ResultValue;
      if ResValue = nil then begin
        AResText := 'Error';
        if AWatchValue <> nil then
          AWatchValue.Validity := ddsInvalid;
        exit;
      end;

      if (ResValue.Kind = skClass) and (ResValue.AsCardinal <> 0) and (defClassAutoCast in EvalFlags)
      then begin
        CastName := '';
        if FMemManager.ReadAddress(ResValue.DataAddress, SizeVal(AContext.SizeOfAddress), ClassAddr) then begin
          {$PUSH}{$Q-}
          ClassAddr.Address := ClassAddr.Address + TDBGPtr(3 * AContext.SizeOfAddress);
          {$POP}
          if FMemManager.ReadAddress(ClassAddr, SizeVal(AContext.SizeOfAddress), CNameAddr) then begin
            if (FMemManager.ReadUnsignedInt(CNameAddr, SizeVal(1), NameLen)) then
              if NameLen > 0 then begin
                SetLength(CastName, NameLen);
                CNameAddr.Address := CNameAddr.Address + 1;
                FMemManager.ReadMemory(CNameAddr, SizeVal(NameLen), @CastName[1]);
                PasExpr2 := TFpPascalExpression.Create(CastName+'('+AExpression+')', AContext);
                PasExpr2.ResultValue;
                if PasExpr2.Valid then begin
                  APasExpr.Free;
                  APasExpr := PasExpr2;
                  ResValue := APasExpr.ResultValue;
                end
                else
                  PasExpr2.Free;
              end;
          end;
        end;
      end;


      if defNoTypeInfo in EvalFlags then
        Res := FPrettyPrinter.PrintValue(AResText, ResValue, DispFormat, RepeatCnt)
      else
        Res := FPrettyPrinter.PrintValue(AResText, ATypeInfo, ResValue, DispFormat, RepeatCnt);

      // PCHAR/String
      if APasExpr.HasPCharIndexAccess then begin
      // TODO: Only dwarf 2
        APasExpr.FixPCharIndexAccess := True;
        APasExpr.ResetEvaluation;
        ResValue := APasExpr.ResultValue;
        if (ResValue=nil) or (not FPrettyPrinter.PrintValue(ResText2, ResValue, DispFormat, RepeatCnt)) then
          ResText2 := 'Failed';
        AResText := 'PChar: '+AResText+ LineEnding + 'String: '+ResText2;
      end;

      if Res then
        begin
        if AWatchValue <> nil then
          begin
          AWatchValue.Value := AResText; //IntToStr(APasExpr.ResultValue.AsInteger);
          AWatchValue.TypeInfo := ATypeInfo;
          if IsError(ResValue.LastError) then
            AWatchValue.Validity := ddsError
          else
            AWatchValue.Validity := ddsValid;
          end;
        end
      else
        begin
        AResText := 'Error';
        if AWatchValue <> nil then
          AWatchValue.Validity := ddsInvalid;
        FreeAndNil(ATypeInfo);
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

procedure TFpDebugDebugger.FDbgControllerLibraryLoaded(var continue: boolean;
  ALib: TDbgLibrary);
var
  n: String;
begin
  n := ExtractFileName(ALib.Name);
  DoDbgEvent(ecModule, etModuleLoad, 'Loaded: ' + n + ' (' + ALib.Name +')');

  if n = 'ntdll.dll' then
    FExceptionStepper.DoNtDllLoaded(ALib);
end;

procedure TFpDebugDebugger.FDbgControllerLibraryUnloaded(var continue: boolean;
  ALib: TDbgLibrary);
var
  n: String;
begin
  n := ExtractFileName(ALib.Name);
  DoDbgEvent(ecModule, etModuleUnload, 'Unloaded: ' + n + ' (' + ALib.Name +')');
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
  t: QWord;
  i: Integer;
begin
  FWatchAsyncQueued := False;
  t := GetTickCount64;
  i := 0;
  // Do the stack first.
  // TODO: have ONE proper queue for all async stuff
  if TFPCallStackSupplier(CallStack).FReqList.Count = 0 then begin
    repeat
      if FWatchEvalList.Count = 0 then
        exit;
      WatchValue := TWatchValue(FWatchEvalList[0]);
      FWatchEvalList.Delete(0);
      WatchValue.RemoveFreeNotification(@DoWatchFreed);

      EvaluateExpression(WatchValue, WatchValue.Expression, AVal, AType);
      inc(i);
    {$PUSH}{$Q-}
    until (GetTickCount64 - t > 60) or (i > 30);
    {$POP}
  end;

  if (not FWatchAsyncQueued) and (FWatchEvalList.Count > 0) then
    begin
    Application.QueueAsyncCall(@ProcessASyncWatches, 0);
    FWatchAsyncQueued := True;
    end
  else
    DoOnIdle;
end;

procedure TFpDebugDebugger.ClearWatchEvalList;
begin
  if Assigned(FWatchEvalList) then
    while FWatchEvalList.Count > 0 do begin
      TWatchValue(FWatchEvalList[0]).RemoveFreeNotification(@DoWatchFreed);
      FWatchEvalList.Delete(0);
    end;
end;

procedure TFpDebugDebugger.GetCurrentThreadAndStackFrame(out AThreadId,
  AStackFrame: Integer);
var
  CurStackList: TCallStackBase;
begin
  AThreadId := Threads.CurrentThreads.CurrentThreadId;
  CurStackList := CallStack.CurrentCallStackList.EntriesForThreads[AThreadId];
  if CurStackList <> nil then begin
    AStackFrame := CurStackList.CurrentIndex;
    if AStackFrame < 0 then
      AStackFrame := 0;
  end
  else
    AStackFrame := 0;
end;

function TFpDebugDebugger.GetContextForEvaluate(const ThreadId,
  StackFrame: Integer): TFpDbgInfoContext;
begin
  Result := FindContext(ThreadId, StackFrame);
  if Result <> nil then
    Result.MemManager.DefaultContext := Result;
end;

function TFpDebugDebugger.GetClassInstanceName(AnAddr: TDBGPtr): string;
var
  VMTAddr: TDBGPtr;
  ClassNameAddr: TDBGPtr;
  b: byte;
begin
  Result := '';
  // Read address of the vmt
  ReadAddress(AnAddr, VMTAddr);
  if VMTAddr = 0 then
    exit;
  ReadAddress(VMTAddr+3*DBGPTRSIZE[FDbgController.CurrentProcess.Mode], ClassNameAddr);
  if ClassNameAddr = 0 then
    exit;
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
  if StrAddr = 0 then
    exit;
  ReadAddress(StrAddr-DBGPTRSIZE[FDbgController.CurrentProcess.Mode], len);
  setlength(result, len);
  if not ReadData(StrAddr, len, result[1]) then
    result := '';
end;

procedure TFpDebugDebugger.HandleSoftwareException(out
  AnExceptionLocation: TDBGLocationRec; var continue: boolean);
var
  AnExceptionObjectLocation: TDBGPtr;
  ExceptionClass: string;
  ExceptionMessage: string;
  RegDxDwarfIndex, RegFirstArg: Cardinal;
  ExceptItem: TBaseException;
begin
  // Using regvar:
  // In all their wisdom, people decided to give the (r)dx register dwarf index
  // 1 on for x86_64 and index 2 for i386.
  if FDbgController.CurrentProcess.Mode=dm32 then begin
    RegDxDwarfIndex:=2;
    RegFirstArg := 0; // AX
  end else begin
    RegDxDwarfIndex:=1;
    {$IFDEF windows}
    // Must be Win64
    RegFirstArg := 2; // RCX
    {$ELSE}
    RegFirstArg := 5; // RDI
    {$ENDIF}
  end;

  AnExceptionLocation:=GetLocationRec(FDbgController.CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(RegDxDwarfIndex).NumValue);
  AnExceptionObjectLocation:=FDbgController.CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(RegFirstArg).NumValue;
  ExceptionClass := '';
  ExceptionMessage := '';
  if AnExceptionObjectLocation <> 0 then begin
    ExceptionClass := GetClassInstanceName(AnExceptionObjectLocation);
    ExceptionMessage := ReadAnsiString(AnExceptionObjectLocation+DBGPTRSIZE[FDbgController.CurrentProcess.Mode]);
  end;

  ExceptItem := Exceptions.Find(ExceptionClass);
  if (ExceptItem <> nil) and (ExceptItem.Enabled)
  then begin
    continue := True;
    exit;
  end;


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
  var continue: boolean; const Breakpoint: TFpDbgBreakpoint);
var
  ABreakPoint: TDBGBreakPoint;
  ALocationAddr: TDBGLocationRec;
  Context: TFpDbgInfoContext;
  PasExpr: TFpPascalExpression;
begin
  if FExceptionStepper.BreakpointHit(&continue, Breakpoint) then
    exit;

  if assigned(Breakpoint) then
    begin
      ABreakPoint := TFPBreakpoints(BreakPoints).Find(Breakpoint);
      if ABreakPoint <> nil then begin

        // TODO: parse expression when breakpoin is created / so invalid expressions do not need to be handled here
        if ABreakPoint.Expression <> '' then begin
          Context := GetContextForEvaluate(FDbgController.CurrentThreadId, 0);
          if Context <> nil then begin
            PasExpr := nil;
            try
              PasExpr := TFpPascalExpression.Create(ABreakPoint.Expression, Context);
              PasExpr.ResultValue; // trigger full validation
              if PasExpr.Valid and (svfBoolean in PasExpr.ResultValue.FieldFlags) and
                 (not PasExpr.ResultValue.AsBool) // false => do not pause
              then
                &continue := True;
            finally
              PasExpr.Free;
              Context.ReleaseReference;
            end;

            if &continue then
              exit;
          end;
        end;

        ALocationAddr := GetLocation;
        if Assigned(EventLogHandler) then
          EventLogHandler.LogEventBreakPointHit(ABreakpoint, ALocationAddr);

        if assigned(ABreakPoint) then
          ABreakPoint.Hit(&continue);

        if (not &continue) and (ABreakPoint.Kind = bpkData) and (OnFeedback <> nil) then begin
          // For message use location(Address - 1)
          OnFeedback(self,
              Format('The Watchpoint for "%1:s" was triggered.%0:s%0:s', // 'Old value: %2:s%0:sNew value: %3:s',
                     [LineEnding, ABreakPoint.WatchData{, AOldVal, ANewVal}]),
              '', ftInformation, [frOk]);
        end;
      end
    end
  else if FQuickPause then
    begin
      SetState(dsInternalPause);
      &continue:=true;
      exit;
    end
  else
    // Debugger returned after a step/next/step-out etc..
    ALocationAddr := GetLocation;

  EnterPause(ALocationAddr, &continue);

  if &continue then
    RunInternalPauseTasks;
end;

procedure TFpDebugDebugger.EnterPause(ALocationAddr: TDBGLocationRec;
  AnInternalPause: Boolean);
begin
  if State <> dsPause then begin
    SetState(dsPause);
    DoCurrent(ALocationAddr);
  end;
end;

procedure TFpDebugDebugger.RunInternalPauseTasks;
begin
  // wait for any watches for Snapshots
  while FWatchAsyncQueued or (TFPCallStackSupplier(CallStack).FReqList.Count > 0) do begin
    if TFPCallStackSupplier(CallStack).FReqList.Count > 0 then
      Application.Idle(False);
    ProcessASyncWatches(0);
  end;
end;

procedure TFpDebugDebugger.FDbgControllerCreateProcessEvent(var continue: boolean);
begin
  // This will trigger setting the breakpoints,
  // may also trigger the evaluation of the callstack or disassembler.
  SetState(dsInternalPause);

  FExceptionStepper.DoProcessLoaded;

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
  Cmd: TDBGCommand;
begin
  result := False;
  if assigned(FDbgController) then
    FDbgController.NextOnlyStopOnStartLine := TFpDebugDebuggerProperties(GetProperties).NextOnlyStopOnStartLine;

  if (ACommand in [dcRun, dcStepOver, dcStepInto, dcStepOut, dcStepTo, dcJumpto,
      dcStepOverInstr, dcStepIntoInstr, dcAttach]) and
     not assigned(FDbgController.MainProcess)
  then
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
    {$ifdef windows}
    FDbgController.ForceNewConsoleWin:=TFpDebugDebuggerProperties(GetProperties).ForceNewConsole;
    {$endif windows}
    //FDbgController.AttachToPid := 0;
    if ACommand = dcAttach then begin
      FDbgController.AttachToPid := StrToIntDef(String(AParams[0].VAnsiString), 0);
      Result := FDbgController.AttachToPid <> 0;
      if not Result then begin
        FileName := '';
        Exit;
      end;
    end;
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
    // TODO: any step commond should run to "main" or "pascalmain"
    // Currently disabled in TFpDebugDebugger.GetSupportedCommands
    StartDebugLoop;
    exit;
  end;

  Cmd := ACommand;
  FExceptionStepper.UserCommandRequested(Cmd);
  case Cmd of
    dcRun:
      begin
        Result := True;
        SetState(dsRun);
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
        Result := FDbgController.Pause;
      end;
    dcStepTo:
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
        FDbgController.InitializeCommand(TDbgControllerStepOverOrFinallyCmd.Create(FDbgController));
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
        FDbgController.StepOut(True);
        SetState(dsRun);
        StartDebugLoop;
        result := true;
      end;
    dcDetach:
      begin
        Result := FDbgController.Detach;
        if Result and (State in [dsPause, dsInternalPause]) then
          StartDebugLoop;
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

function TFpDebugDebugger.ExecuteInDebugThread(AMethod: TFpDbgAsyncMethod
  ): boolean;
begin
  Result := True;
  if ThreadID = FFpDebugThread.ThreadID then begin
    AMethod();
    exit;
  end;

  Result := False;
  assert(not assigned(FFpDebugThread.AsyncMethod));
  if FFpDebugThread.LoopIsRunnig then begin
    DebugLn(DBG_WARNINGS, ['ExecuteInDebugThread while thread busy']);
    exit;
  end;

  Result := True;
  FFpDebugThread.AsyncMethod:=AMethod;
  RTLeventSetEvent(FFpDebugThread.StartDebugLoopEvent);
  RTLeventWaitFor(FFpDebugThread.DebugLoopStoppedEvent);
  RTLeventResetEvent(FFpDebugThread.DebugLoopStoppedEvent);
  FFpDebugThread.AsyncMethod:=nil;
end;

procedure TFpDebugDebugger.StartDebugLoop;
begin
  {$ifdef DBG_FPDEBUG_VERBOSE}
  DebugLn(DBG_VERBOSE, 'StartDebugLoop');
  {$endif DBG_FPDEBUG_VERBOSE}
  RTLeventSetEvent(FFpDebugThread.StartDebugLoopEvent);
end;

procedure TFpDebugDebugger.DebugLoopFinished;
var
  Cont: boolean;
begin
  LockRelease;
  try
    {$ifdef DBG_FPDEBUG_VERBOSE}
    DebugLn(DBG_VERBOSE, 'DebugLoopFinished');
    {$endif DBG_FPDEBUG_VERBOSE}

    (* Need to ensure CurrentThreadId is correct,
       because any callstack (never mind which to which IDE-thread object it belongs
       will always get the data for the current thread only
     TODO: callstacks need a field with the thread-id to which they belong *)
    if (Threads <> nil) and (Threads.CurrentThreads <> nil) and
       (FDbgController.CurrentThread <> nil)
    then
      Threads.CurrentThreads.CurrentThreadId := FDbgController.CurrentThreadId;

    FDbgController.SendEvents(Cont); // This may free the TFpDebugDebugger (self)

    FQuickPause:=false; // TODO: there may be other events: deInternalContinue, deLoadLibrary...

    if Cont then
      begin
      SetState(dsRun);
      StartDebugLoop;
      end
  finally
    UnlockRelease;
  end;
end;

procedure TFpDebugDebugger.QuickPause;
begin
  FQuickPause:=FDbgController.Pause;
end;

procedure TFpDebugDebugger.DoRelease;
begin
  DebugLn(DBG_VERBOSE, ['++++ dorelase  ', Dbgs(ptrint(FDbgController)), dbgs(state)]);
//  SetState(dsDestroying);
  if (State <> dsDestroying) and //assigned(FFpDebugThread) and //???
     (FDbgController <> nil) and (FDbgController.MainProcess <> nil)
  then begin
    FDbgController.Stop;
    FDbgControllerProcessExitEvent(0); // Force exit;
  end;

  inherited DoRelease;
end;

procedure TFpDebugDebugger.DoOnIdle;
begin
  if not Assigned(OnIdle) then
    exit;
  FIsIdle := True;
  try
    OnIdle(Self);
  except
    on E: Exception do
      DebugLn(['exception during idle ', E.ClassName, ': ', E.Message]);
  end;
  FIsIdle := False;
end;

procedure TFpDebugDebugger.DoState(const OldState: TDBGState);
begin
  LockRelease;
  try
    if (State in [dsPause{, dsInternalPause}]) then // Make sure we have threads first // this can be removed, once threads are KEPT between pauses
      Threads.RequestMasterData;
    inherited DoState(OldState);
    if not (State in [dsPause, dsInternalPause]) then
      begin
      ClearWatchEvalList;
      FWatchAsyncQueued := False;
      end
    else
    if (State in [dsPause, dsInternalPause]) and
      not(OldState in [dsPause, dsInternalPause{, dsInit}]) and
      (not Assigned(FWatchEvalList) or (FWatchEvalList.Count = 0))
    then
      DoOnIdle;
  finally
    UnlockRelease;
  end;
end;

function TFpDebugDebugger.GetIsIdle: Boolean;
begin
  Result := FIsIdle;
end;

procedure TFpDebugDebugger.DoAddBreakLine;
begin
  FCacheBreakpoint := TDbgInstance(FDbgController.CurrentProcess).AddBreak(FCacheFileName, FCacheLine, FCacheBoolean);
end;

procedure TFpDebugDebugger.DoAddBreakFuncLib;
begin
  if FCacheLib <> nil then
    FCacheBreakpoint := FCacheLib.AddBreak(FCacheFileName, FCacheBoolean)
  else
    FCacheBreakpoint := TDbgInstance(FDbgController.CurrentProcess).AddBreak(FCacheFileName, FCacheBoolean);
end;

procedure TFpDebugDebugger.DoAddBreakLocation;
begin
  if FCacheLocation = 0 then
    FCacheBreakpoint := FDbgController.CurrentProcess.AddBreak(nil, FCacheBoolean)
  else
    FCacheBreakpoint := FDbgController.CurrentProcess.AddBreak(FCacheLocation, FCacheBoolean);
end;

procedure TFpDebugDebugger.DoAddBWatch;
begin
  FCacheBreakpoint := FDbgController.CurrentProcess.AddWatch(FCacheLocation, FCacheLine, FCacheReadWrite, FCacheScope);
end;

procedure TFpDebugDebugger.DoReadData;
begin
  FCacheBoolean:=FDbgController.CurrentProcess.ReadData(FCacheLocation, FCacheLine, FCachePointer^);
end;

procedure TFpDebugDebugger.DoPrepareCallStackEntryList;
begin
  FCallStackEntryListThread.PrepareCallStackEntryList(FCallStackEntryListFrameRequired);
end;

procedure TFpDebugDebugger.DoFreeBreakpoint;
begin
  FCacheBreakpoint.Free;
end;

procedure TFpDebugDebugger.DoFindContext;
begin
  FCacheContext := FDbgController.CurrentProcess.FindContext(FCacheThreadId, FCacheStackFrame);
end;

procedure TFpDebugDebugger.DoGetParamsAsString;
begin
  FParamAsString := FParamAsStringStackEntry.GetParamsAsString(FParamAsStringPrettyPrinter);
end;

procedure TFpDebugDebugger.DoChangeCurrentThreadId;
begin
  FDbgController.CurrentThreadId := FNewThreadId;
  if Threads.CurrentThreads <> nil
  then Threads.CurrentThreads.CurrentThreadId := FNewThreadId;
end;

function TFpDebugDebugger.AddBreak(const ALocation: TDbgPtr; AnEnabled: Boolean
  ): TFpDbgBreakpoint;
begin
  if FDbgController.CurrentProcess.RequiresExecutionInDebuggerThread then
  begin
    FCacheLocation:=ALocation;
    FCacheBoolean:=AnEnabled;
    FCacheBreakpoint := nil;
    ExecuteInDebugThread(@DoAddBreakLocation);
    result := FCacheBreakpoint;
  end
  else
    if ALocation = 0 then
      result := FDbgController.CurrentProcess.AddBreak(nil, AnEnabled)
    else
      result := FDbgController.CurrentProcess.AddBreak(ALocation, AnEnabled);
end;

function TFpDebugDebugger.AddBreak(const AFileName: String; ALine: Cardinal;
  AnEnabled: Boolean): TFpDbgBreakpoint;
begin
  if FDbgController.CurrentProcess.RequiresExecutionInDebuggerThread then
  begin
    FCacheFileName:=AFileName;
    FCacheLine:=ALine;
    FCacheBoolean:=AnEnabled;
    FCacheBreakpoint := nil;
    ExecuteInDebugThread(@DoAddBreakLine);
    result := FCacheBreakpoint;
  end
  else
    result := TDbgInstance(FDbgController.CurrentProcess).AddBreak(AFileName, ALine, AnEnabled);
end;

function TFpDebugDebugger.AddBreak(const AFuncName: String; ALib: TDbgLibrary;
  AnEnabled: Boolean): TFpDbgBreakpoint;
begin
  if FDbgController.CurrentProcess.RequiresExecutionInDebuggerThread then
  begin
    FCacheFileName:=AFuncName;
    FCacheLib:=ALib;
    FCacheBoolean:=AnEnabled;
    FCacheBreakpoint := nil;
    ExecuteInDebugThread(@DoAddBreakFuncLib);
    result := FCacheBreakpoint;
  end
  else
    if ALib <> nil then
      result := ALib.AddBreak(AFuncName, AnEnabled)
    else
      result := TDbgInstance(FDbgController.CurrentProcess).AddBreak(AFuncName, AnEnabled);
end;

function TFpDebugDebugger.AddWatch(const ALocation: TDBGPtr; ASize: Cardinal;
  AReadWrite: TDBGWatchPointKind; AScope: TDBGWatchPointScope
  ): TFpDbgBreakpoint;
begin
  if FDbgController.CurrentProcess.RequiresExecutionInDebuggerThread then
  begin
    FCacheLocation:=ALocation;
    FCacheLine:=ASize;
    FCacheReadWrite:=AReadWrite;
    FCacheScope:=AScope;
    FCacheBreakpoint := nil;
    ExecuteInDebugThread(@DoAddBWatch);
    result := FCacheBreakpoint;
  end
  else
    result := FDbgController.CurrentProcess.AddWatch(ALocation, ASize, AReadWrite, AScope);
end;

procedure TFpDebugDebugger.FreeBreakpoint(
  const ABreakpoint: TFpDbgBreakpoint);
begin
  if ABreakpoint = nil then exit;
  if FDbgController.CurrentProcess.RequiresExecutionInDebuggerThread then
  begin
    FCacheBreakpoint:=ABreakpoint;
    ExecuteInDebugThread(@DoFreeBreakpoint);
  end
  else
    ABreakpoint.Free;
end;

function TFpDebugDebugger.ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean;
begin
  if FDbgController.CurrentProcess.RequiresExecutionInDebuggerThread then
  begin
    FCacheLocation := AAdress;
    FCacheLine:=ASize;
    FCachePointer := @AData;
    FCacheBoolean := False;
    ExecuteInDebugThread(@DoReadData);
    result := FCacheBoolean;
  end
  else
    result:=FDbgController.CurrentProcess.ReadData(AAdress, ASize, AData);
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

procedure TFpDebugDebugger.PrepareCallStackEntryList(AFrameRequired: Integer;
  AThread: TDbgThread);
begin
  if AThread = nil then
    AThread := FDbgController.CurrentThread;
  // In case of linux, check if required, before handind to other thread
  if (AFrameRequired >= 0) and
     (AThread.CallStackEntryList <> nil) and
     (AFrameRequired < AThread.CallStackEntryList.Count) then
    exit;
  if FDbgController.CurrentProcess.RequiresExecutionInDebuggerThread then
  begin
    FCallStackEntryListThread := AThread;
    FCallStackEntryListFrameRequired := AFrameRequired;
    ExecuteInDebugThread(@DoPrepareCallStackEntryList);
  end
  else
    AThread.PrepareCallStackEntryList(AFrameRequired);
end;

function TFpDebugDebugger.FindContext(AThreadId, AStackFrame: Integer): TFpDbgInfoContext;
begin
  if FDbgController.CurrentProcess.RequiresExecutionInDebuggerThread then
  begin
    FCacheThreadId := AThreadId;
    FCacheStackFrame := AStackFrame;
    FCacheContext := nil;
    ExecuteInDebugThread(@DoFindContext);
    Result := FCacheContext;
  end
  else
    Result := FDbgController.CurrentProcess.FindContext(AThreadId, AStackFrame);
end;

function TFpDebugDebugger.GetParamsAsString(AStackEntry: TDbgCallstackEntry;
  APrettyPrinter: TFpPascalPrettyPrinter): string;
begin
  if FDbgController.CurrentProcess.RequiresExecutionInDebuggerThread then
  begin
    FParamAsStringStackEntry := AStackEntry;
    FParamAsStringPrettyPrinter := APrettyPrinter;
    FParamAsString:='';
    ExecuteInDebugThread(@DoGetParamsAsString);
    Result := FParamAsString;
  end
  else
    Result := AStackEntry.GetParamsAsString(APrettyPrinter);
end;

constructor TFpDebugDebugger.Create(const AExternalDebugger: String);
begin
  inherited Create(AExternalDebugger);
  FExceptionStepper := TFpDebugExceptionStepping.Create(Self);
  FWatchEvalList := TFPList.Create;
  FPrettyPrinter := TFpPascalPrettyPrinter.Create(sizeof(pointer));
  FMemReader := TFpDbgMemReader.Create(self);
  FMemConverter := TFpDbgMemConvertorLittleEndian.Create;
  FMemManager := TFpDbgMemManager.Create(FMemReader, FMemConverter);
  FDbgController := TDbgController.Create;
  FDbgController.OnCreateProcessEvent:=@FDbgControllerCreateProcessEvent;
  FDbgController.OnHitBreakpointEvent:=@FDbgControllerHitBreakpointEvent;
  FDbgController.OnProcessExitEvent:=@FDbgControllerProcessExitEvent;
  FDbgController.OnExceptionEvent:=@FDbgControllerExceptionEvent;
  FDbgController.OnDebugInfoLoaded := @FDbgControllerDebugInfoLoaded;
  FDbgController.OnLibraryLoadedEvent := @FDbgControllerLibraryLoaded;
  FDbgController.OnLibraryUnloadedEvent := @FDbgControllerLibraryUnloaded;
  FDbgController.NextOnlyStopOnStartLine := TFpDebugDebuggerProperties(GetProperties).NextOnlyStopOnStartLine;

  FDbgController.OnThreadProcessLoopCycleEvent:=@FExceptionStepper.ThreadProcessLoopCycle;
  FDbgController.OnThreadBeforeProcessLoop:=@FExceptionStepper.ThreadBeforeLoop;
end;

destructor TFpDebugDebugger.Destroy;
begin
  if state in [dsPause, dsInternalPause] then
    try
      SetState(dsStop);
    except
    end;
  if assigned(FFpDebugThread) then
    FreeDebugThread;
  ClearWatchEvalList;
  Application.RemoveAsyncCalls(Self);
  FreeAndNil(FDbgController);
  FreeAndNil(FPrettyPrinter);
  FreeAndNil(FWatchEvalList);
  FreeAndNil(FMemManager);
  FreeAndNil(FMemConverter);
  FreeAndNil(FMemReader);
  FreeAndNil(FExceptionStepper);
  inherited Destroy;
end;

function TFpDebugDebugger.GetLocationRec(AnAddress: TDBGPtr): TDBGLocationRec;
var
  sym, symproc: TFpSymbol;
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

    sym := FDbgController.CurrentProcess.FindProcSymbol(result.Address);
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

class function TFpDebugDebugger.RequiredCompilerOpts(ATargetCPU, ATargetOS: String): TDebugCompilerRequirements;
begin
  {$ifdef CD_Cocoa}{$DEFINE MacOS}
  if ATargetCPU = '' then ATargetCPU := 'x86_64';
  {$ENDIF}
  {$IFDEF Darwin}{$DEFINE MacOS}
  if ATargetCPU = '' then ATargetCPU := 'i386';
  {$ENDIF}
  {$IFDEF MacOs}
  if LowerCase(ATargetCPU) = 'i386' then
    Result:=[dcrDwarfOnly] // carbon
  else
    Result:=[dcrExternalDbgInfoOnly, dcrDwarfOnly]; // cocoa
  {$ELSE}
  Result:=[dcrNoExternalDbgInfo, dcrDwarfOnly];
  {$ENDIF}
end;

class function TFpDebugDebugger.CreateProperties: TDebuggerProperties;
begin
  Result := TFpDebugDebuggerProperties.Create;
end;

function TFpDebugDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result:=[dcRun, dcStop, dcStepIntoInstr, dcStepOverInstr, dcStepOver,
           dcStepTo, dcPause, dcStepOut, dcStepInto, dcEvaluate, dcSendConsoleInput
           {$IFDEF windows} , dcAttach, dcDetach {$ENDIF}
           {$IFDEF linux} , dcAttach, dcDetach {$ENDIF}
          ];
  if State = dsStop then
    Result := Result - [dcStepInto, dcStepOver, dcStepOut, dcStepIntoInstr, dcStepOverInstr];
end;

initialization
  DBG_VERBOSE     := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS    := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  DBG_BREAKPOINTS := DebugLogger.FindOrRegisterLogGroup('DBG_BREAKPOINTS' {$IFDEF DBG_BREAKPOINTS} , True {$ENDIF} );
  FPDBG_COMMANDS  := DebugLogger.FindOrRegisterLogGroup('FPDBG_COMMANDS' {$IFDEF FPDBG_COMMANDS} , True {$ENDIF} );

end.

