unit FPDbgController;

{$mode objfpc}{$H+}
{$TYPEDADDRESS on}

interface

uses
  Classes,
  SysUtils,
  Maps,
  LazLoggerBase,
  DbgIntfBaseTypes, DbgIntfDebuggerBase,
  FpDbgDisasX86,
  FpDbgClasses, FpDbgInfo;

type

  TOnCreateProcessEvent = procedure(var continue: boolean) of object;
  TOnHitBreakpointEvent = procedure(var continue: boolean; const Breakpoint: TFpDbgBreakpoint) of object;
  TOnExceptionEvent = procedure(var continue: boolean; const ExceptionClass, ExceptionMessage: string) of object;
  TOnProcessExitEvent = procedure(ExitCode: DWord) of object;
  TOnLibraryLoadedEvent = procedure(var continue: boolean; ALib: TDbgLibrary) of object;
  TOnLibraryUnloadedEvent = procedure(var continue: boolean; ALib: TDbgLibrary) of object;

  TDbgController = class;

  { TDbgControllerCmd }

  TDbgControllerCmd = class
  protected
    FController: TDbgController;
    FThread: TDbgThread;
    FProcess: TDbgProcess;
    FIsInitialized: Boolean;
    procedure Init; virtual;
    function IsAtCallInstruction: Integer;
    function GetAsmInstruction(var AnInstr: TInstruction): Integer;
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean); virtual; abstract;
  public
    constructor Create(AController: TDbgController); virtual;
    procedure DoBeforeLoopStart;
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); virtual; abstract;
    procedure ResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean); virtual;
  end;

  { TDbgControllerContinueCmd }

  TDbgControllerContinueCmd = class(TDbgControllerCmd)
  protected
    procedure Init; override;
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean); override;
  public
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
  end;

  { TDbgControllerStepIntoInstructionCmd }

  TDbgControllerStepIntoInstructionCmd = class(TDbgControllerCmd)
  protected
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean); override;
  public
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
  end;

  { TDbgControllerHiddenBreakStepBaseCmd }

  TDbgControllerHiddenBreakStepBaseCmd = class(TDbgControllerCmd)
  private
    FStoredStackFrame, FStoredStackPointer: TDBGPtr; // In case of IsSteppedOut, those are kept to the original values
    FIsSteppedOut: Boolean;
    FHiddenBreakpoint: TFpInternalBreakpoint;
    FHiddenBreakAddr, FHiddenBreakInstrPtr, FHiddenBreakFrameAddr, FHiddenBreakStackPtrAddr: TDBGPtr;
    FNextInstruction: TInstruction;
    FNextInstructionLen: Integer;
    function GetIsSteppedOut: Boolean;
    function GetNextInstructionLen: integer; inline;
    function GetNextOpCode: TOpCode; inline;
  protected
    function IsAtHiddenBreak: Boolean; inline;
    function HasHiddenBreak: Boolean; inline;
    function IsAtLastHiddenBreakAddr: Boolean; inline;
    function IsAtOrOutOfHiddenBreakFrame: Boolean;  inline; // Stopped in/out-of the origin frame, maybe by a breakpoint after an exception
    procedure SetHiddenBreak(AnAddr: TDBGPtr);
    procedure RemoveHiddenBreak;

    function CheckForCallAndSetBreak: boolean; // True, if break is newly set

    procedure Init; override;
    procedure InternalContinue(AProcess: TDbgProcess; AThread: TDbgThread); virtual; abstract;
  public
    destructor Destroy; override;
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;

    property StoredStackFrame: TDBGPtr read FStoredStackFrame;
    property IsSteppedOut: Boolean read GetIsSteppedOut;
    property NextInstructionLen: integer read GetNextInstructionLen;
    property NextOpCode: TOpCode read GetNextOpCode;
  end;

  { TDbgControllerStepOverInstructionCmd }

  TDbgControllerStepOverInstructionCmd = class(TDbgControllerHiddenBreakStepBaseCmd)
  protected
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean); override;
    procedure InternalContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
  end;

  { TDbgControllerLineStepBaseCmd }

  TDbgControllerLineStepBaseCmd = class(TDbgControllerHiddenBreakStepBaseCmd)
  private
    FStepInfoUpdatedForStepOut, FStepInfoUnavailAfterStepOut: Boolean;
  protected
    procedure UpdateThreadStepInfoAfterStepOut;
    function HasSteppedAwayFromOriginLine: boolean; // Call only, if in original frame (or updated frame)
  end;

  { TDbgControllerStepIntoLineCmd }

  TDbgControllerStepIntoLineCmd = class(TDbgControllerLineStepBaseCmd)
  private
    FState: (siSteppingCurrent, siSteppingIn, siSteppingNested, siRunningStepOut);
    FStepCount, FNestDepth: Integer;
  protected
    procedure Init; override;
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean); override;
    procedure InternalContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
  end;

  { TDbgControllerStepOverLineCmd }

  TDbgControllerStepOverLineCmd = class(TDbgControllerLineStepBaseCmd)
  protected
    procedure Init; override;
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean); override;
    procedure InternalContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
  end;

  { TDbgControllerStepOutCmd }

  TDbgControllerStepOutCmd = class(TDbgControllerLineStepBaseCmd)
// TODO: do not store the initial line info
  private
    FStepCount: Integer;
    FWasOutsideFrame: boolean;
  protected
    function  GetOutsideFrame(var AnOutside: Boolean): Boolean;
    procedure SetReturnAdressBreakpoint(AProcess: TDbgProcess; AnOutsideFrame: Boolean);
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean); override;
    procedure InternalContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
  public
  end;

  { TDbgControllerRunToCmd }

  TDbgControllerRunToCmd = class(TDbgControllerHiddenBreakStepBaseCmd)
  private
    FLocation: TDBGPtrArray;
  protected
    procedure Init; override;
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean); override;
    procedure InternalContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
  public
    constructor Create(AController: TDbgController; ALocation: TDBGPtrArray);
  end;

  { TDbgController }

  TDbgController = class
  private
    FOnLibraryLoadedEvent: TOnLibraryLoadedEvent;
    FOnLibraryUnloadedEvent: TOnLibraryUnloadedEvent;
    FRunning, FPauseRequest: cardinal;
    FAttachToPid: Integer;
    FDetaching: cardinal;
    FEnvironment: TStrings;
    FExecutableFilename: string;
    FForceNewConsoleWin: boolean;
    FNextOnlyStopOnStartLine: boolean;
    FOnCreateProcessEvent: TOnCreateProcessEvent;
    FOnDebugInfoLoaded: TNotifyEvent;
    FOnExceptionEvent: TOnExceptionEvent;
    FOnHitBreakpointEvent: TOnHitBreakpointEvent;
    FOnProcessExitEvent: TOnProcessExitEvent;
    FProcessMap: TMap;
    FPDEvent: TFPDEvent;
    FParams: TStringList;
    FConsoleTty: string;
    FRedirectConsoleOutput: boolean;
    FWorkingDirectory: string;
    function GetCurrentThreadId: Integer;
    procedure SetCurrentThreadId(AValue: Integer);
    procedure SetEnvironment(AValue: TStrings);
    procedure SetExecutableFilename(AValue: string);
    procedure DoOnDebugInfoLoaded(Sender: TObject);
    procedure SetParams(AValue: TStringList);
  protected
    FMainProcess: TDbgProcess;
    FCurrentProcess: TDbgProcess;
    FCurrentThread: TDbgThread;
    FCommand, FCommandToBeFreed: TDbgControllerCmd;
    function GetProcess(const AProcessIdentifier: THandle; out AProcess: TDbgProcess): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure InitializeCommand(ACommand: TDbgControllerCmd);
    function Run: boolean;
    procedure Stop;
    procedure StepIntoInstr;
    procedure StepOverInstr;
    procedure Next;
    procedure Step;
    procedure StepOut;
    function Pause: boolean;
    function Detach: boolean;
    procedure ProcessLoop;
    procedure SendEvents(out continue: boolean);

    property ExecutableFilename: string read FExecutableFilename write SetExecutableFilename;
    property AttachToPid: Integer read FAttachToPid write FAttachToPid;
    property CurrentProcess: TDbgProcess read FCurrentProcess;
    property CurrentThread: TDbgThread read FCurrentThread;
    property CurrentThreadId: Integer read GetCurrentThreadId write SetCurrentThreadId;
    property MainProcess: TDbgProcess read FMainProcess;
    property Params: TStringList read FParams write SetParams;
    property Environment: TStrings read FEnvironment write SetEnvironment;
    property WorkingDirectory: string read FWorkingDirectory write FWorkingDirectory;
    property RedirectConsoleOutput: boolean read FRedirectConsoleOutput write FRedirectConsoleOutput;
    property ForceNewConsoleWin: boolean read FForceNewConsoleWin write FForceNewConsoleWin; // windows only
    property ConsoleTty: string read FConsoleTty write FConsoleTty;
    // With this parameter set a 'next' will only stop if the current
    // instruction is the first instruction of a line according to the
    // debuginfo.
    // Due to a bug in fpc's debug-info, the line info for the first instruction
    // of a line, sometimes points the the prior line. This setting hides the
    // results of that bug. It seems like it that GDB does something similar.
    property NextOnlyStopOnStartLine: boolean read FNextOnlyStopOnStartLine write FNextOnlyStopOnStartLine;

    property OnCreateProcessEvent: TOnCreateProcessEvent read FOnCreateProcessEvent write FOnCreateProcessEvent;
    property OnHitBreakpointEvent: TOnHitBreakpointEvent read FOnHitBreakpointEvent write FOnHitBreakpointEvent;
    property OnProcessExitEvent: TOnProcessExitEvent read FOnProcessExitEvent write FOnProcessExitEvent;
    property OnExceptionEvent: TOnExceptionEvent read FOnExceptionEvent write FOnExceptionEvent;
    property OnDebugInfoLoaded: TNotifyEvent read FOnDebugInfoLoaded write FOnDebugInfoLoaded;
    property OnLibraryLoadedEvent: TOnLibraryLoadedEvent read FOnLibraryLoadedEvent write FOnLibraryLoadedEvent;
    property OnLibraryUnloadedEvent: TOnLibraryUnloadedEvent read FOnLibraryUnloadedEvent write FOnLibraryUnloadedEvent;
  end;

implementation

var
  DBG_VERBOSE, DBG_WARNINGS, FPDBG_COMMANDS: PLazLoggerLogGroup;

{ TDbgControllerCmd }

procedure TDbgControllerCmd.Init;
begin
  //
end;

function TDbgControllerCmd.IsAtCallInstruction: Integer;
var
  CodeBin: array[0..20] of byte;
begin
  Result := 0;
  if FProcess.ReadData(FThread.GetInstructionPointerRegisterValue, sizeof(CodeBin), CodeBin) then
    Result := IsCallInstruction(@CodeBin, FProcess.Mode=dm64);
end;

function TDbgControllerCmd.GetAsmInstruction(var AnInstr: TInstruction
  ): Integer;
var
  CodeBin: array[0..20] of byte;
  p: Pointer;
begin
  Result := 0;
  AnInstr.OpCode := OPX_Invalid;
  if FProcess.ReadData(FThread.GetInstructionPointerRegisterValue, sizeof(CodeBin), CodeBin) then begin
    p := @CodeBin;
    Disassemble(p, FProcess.Mode=dm64, AnInstr);
    Result := p - @CodeBin[0];
  end;
end;

constructor TDbgControllerCmd.Create(AController: TDbgController);
begin
  FController := AController;
  FProcess := FController.CurrentProcess;
  FThread := FController.CurrentThread;
end;

procedure TDbgControllerCmd.DoBeforeLoopStart;
begin
  if not FIsInitialized then
    Init;
  FIsInitialized := True;
end;

procedure TDbgControllerCmd.ResolveEvent(var AnEvent: TFPDEvent;
  AnEventThread: TDbgThread; out Finished: boolean);
var
  dummy: TDbgThread;
begin
  Finished := False;
  if AnEventThread = nil then
    exit;
  if FThread <> nil then begin
    // ResolveDebugEvent will have removed the thread, but not yet destroyed it
    // Finish, if the thread has gone.
    Finished := not FProcess.GetThread(FThread.ID, dummy);
    if Finished then
      exit;
    // Only react to events for the correct thread. (Otherwise return Finished = False)
    if FThread <> AnEventThread then
      exit;
  end;
  DoResolveEvent(AnEvent, AnEventThread, Finished);
end;

{ TDbgControllerContinueCmd }

procedure TDbgControllerContinueCmd.Init;
begin
  inherited Init;
  FThread := nil; // run until any thread has an event
end;

procedure TDbgControllerContinueCmd.DoContinue(AProcess: TDbgProcess; AThread: TDbgThread);
begin
  assert(FProcess=AProcess, 'TDbgControllerContinueCmd.DoContinue: FProcess=AProcess');
  AProcess.Continue(AProcess, AThread, False);
end;

procedure TDbgControllerContinueCmd.DoResolveEvent(var AnEvent: TFPDEvent;
  AnEventThread: TDbgThread; out Finished: boolean);
begin
  Finished := (AnEvent<>deInternalContinue); // TODO: always False? will be aborted, if another event terminates the ProcessLoop
end;

{ TDbgControllerStepIntoInstructionCmd }

procedure TDbgControllerStepIntoInstructionCmd.DoContinue(
  AProcess: TDbgProcess; AThread: TDbgThread);
begin
  assert(FProcess=AProcess, 'TDbgControllerStepIntoInstructionCmd.DoContinue: FProcess=AProcess');
  FProcess.Continue(FProcess, FThread, True);
end;

procedure TDbgControllerStepIntoInstructionCmd.DoResolveEvent(
  var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean);
begin
  Finished := (AnEvent<>deInternalContinue);
  if Finished then
    AnEvent := deFinishedStep;
end;

{ TDbgControllerHiddenBreakStepBaseCmd }

function TDbgControllerHiddenBreakStepBaseCmd.GetNextInstructionLen: integer;
begin
  if FNextInstruction.OpCode = OPX_InternalUnknown then
    FNextInstructionLen := GetAsmInstruction(FNextInstruction);
  Result := FNextInstructionLen;
end;

function TDbgControllerHiddenBreakStepBaseCmd.GetNextOpCode: TOpCode;
begin
  if FNextInstruction.OpCode = OPX_InternalUnknown then
    FNextInstructionLen := GetAsmInstruction(FNextInstruction);
  Result := FNextInstruction.OpCode;
end;

function TDbgControllerHiddenBreakStepBaseCmd.GetIsSteppedOut: Boolean;
var
  CurBp, CurSp: TDBGPtr;
begin
  Result := FIsSteppedOut;
  if Result then
    exit;

  CurBp := FController.CurrentThread.GetStackBasePointerRegisterValue;
  if FStoredStackFrame < CurBp then begin
    CurSp := FController.CurrentThread.GetStackPointerRegisterValue;
    if FStoredStackPointer >= CurSp then // this happens, if current was recorded before the BP frame was set up // a finally handle may then fake an outer frame
      exit;
    {$PUSH}{$Q-}{$R-}
    if CurSp = FStoredStackPointer + FProcess.PointerSize then
      exit; // Still in proc, but passed asm "leave" (BP has been popped, but IP not yet)
    {$POP}
    FIsSteppedOut := True;
    debugln(FPDBG_COMMANDS, ['BreakStepBaseCmd.GetIsSteppedOut: Has stepped out Stored-BP=', FStoredStackFrame, ' < BP=', CurBp, ' / SP', CurSp]);
  end;

  Result := FIsSteppedOut;
end;

function TDbgControllerHiddenBreakStepBaseCmd.IsAtHiddenBreak: Boolean;
begin
  Result := (FHiddenBreakpoint <> nil) and
            (FThread.GetInstructionPointerRegisterValue = FHiddenBreakAddr) and // FHiddenBreakpoint.HasLocation()
            (FThread.GetStackPointerRegisterValue >= FHiddenBreakStackPtrAddr);
            // if SP > FStackPtrRegVal >> then the brk was hit stepped out (should not happen)
  debugln(FPDBG_COMMANDS and Result, ['BreakStepBaseCmd.IsAtHiddenBreak: At Hidden break = true']);
end;

function TDbgControllerHiddenBreakStepBaseCmd.HasHiddenBreak: Boolean;
begin
  Result := FHiddenBreakpoint <> nil;
end;

function TDbgControllerHiddenBreakStepBaseCmd.IsAtLastHiddenBreakAddr: Boolean;
begin
  Result := (FThread.GetInstructionPointerRegisterValue = FHiddenBreakAddr);
  debugln(FPDBG_COMMANDS and Result, ['BreakStepBaseCmd.IsAtLastHiddenBreakAddr : At LAST Hidden break ADDR = true']);
end;

function TDbgControllerHiddenBreakStepBaseCmd.IsAtOrOutOfHiddenBreakFrame: Boolean;
begin
  Result := HasHiddenBreak;
  if not Result then
    exit;
  if (FHiddenBreakInstrPtr = FThread.GetInstructionPointerRegisterValue) then
    Result := ((FHiddenBreakStackPtrAddr < FThread.GetStackPointerRegisterValue) or
               (FHiddenBreakFrameAddr < FThread.GetStackBasePointerRegisterValue))
  else
    // InstructPtr moved, so SP can be equal
    Result := ((FHiddenBreakStackPtrAddr <= FThread.GetStackPointerRegisterValue) or
               (FHiddenBreakFrameAddr < FThread.GetStackBasePointerRegisterValue));
  debugln(FPDBG_COMMANDS and Result and (FHiddenBreakpoint <> nil), ['BreakStepBaseCmd.IsAtOrOutOfHiddenBreakFrame: Gone past hidden break = true']);
end;

procedure TDbgControllerHiddenBreakStepBaseCmd.SetHiddenBreak(AnAddr: TDBGPtr);
begin
  // The callee may not setup a stackfram (StackBasePtr unchanged). So we use SP to detect recursive hits
  FHiddenBreakStackPtrAddr := FThread.GetStackPointerRegisterValue;
  FHiddenBreakFrameAddr := FThread.GetStackBasePointerRegisterValue;
  FHiddenBreakInstrPtr := FThread.GetInstructionPointerRegisterValue;
  FHiddenBreakAddr := AnAddr;
  FHiddenBreakpoint := FProcess.AddInternalBreak(AnAddr);
end;

procedure TDbgControllerHiddenBreakStepBaseCmd.RemoveHiddenBreak;
begin
  if assigned(FHiddenBreakpoint) then
    FreeAndNil(FHiddenBreakpoint);
end;

function TDbgControllerHiddenBreakStepBaseCmd.CheckForCallAndSetBreak: boolean;
begin
  Result := FHiddenBreakpoint = nil;
  if not Result then
    exit;
  Result := NextOpCode = OPcall;
  if Result then
    {$PUSH}{$Q-}{$R-}
    SetHiddenBreak(FThread.GetInstructionPointerRegisterValue + NextInstructionLen);
    {$POP}
end;

procedure TDbgControllerHiddenBreakStepBaseCmd.Init;
begin
  FStoredStackPointer := FThread.GetStackPointerRegisterValue;
  FStoredStackFrame   := FThread.GetStackBasePointerRegisterValue;
  inherited Init;
end;

destructor TDbgControllerHiddenBreakStepBaseCmd.Destroy;
begin
  RemoveHiddenBreak;
  inherited Destroy;
end;

procedure TDbgControllerHiddenBreakStepBaseCmd.DoContinue(AProcess: TDbgProcess;
  AThread: TDbgThread);
begin
  InternalContinue(AProcess, AThread);
  if ((FNextInstruction.OpCode = OPret) or (FNextInstruction.OpCode = OPretf)) and
     (FHiddenBreakpoint = nil)
  then
    FIsSteppedOut := True;
  FNextInstruction.OpCode := OPX_InternalUnknown;
  FNextInstructionLen := 0;
end;

{ TDbgControllerStepOverInstructionCmd }

procedure TDbgControllerStepOverInstructionCmd.InternalContinue(
  AProcess: TDbgProcess; AThread: TDbgThread);
begin
  assert(FProcess=AProcess, 'TDbgControllerStepOverInstructionCmd.DoContinue: FProcess=AProcess');
  CheckForCallAndSetBreak;
  FProcess.Continue(FProcess, FThread, FHiddenBreakpoint = nil);
end;

procedure TDbgControllerStepOverInstructionCmd.DoResolveEvent(
  var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean);
begin
  if FHiddenBreakpoint <> nil then
    Finished := IsAtOrOutOfHiddenBreakFrame
  else
    Finished := not (AnEvent in [deInternalContinue, deLoadLibrary]);
  if Finished then
  begin
    AnEvent := deFinishedStep;
    RemoveHiddenBreak;
  end
  else
  if AnEvent = deFinishedStep then
    AnEvent := deInternalContinue;
end;

{ TDbgControllerLineStepBaseCmd }

procedure TDbgControllerLineStepBaseCmd.UpdateThreadStepInfoAfterStepOut;
begin
  if FStepInfoUpdatedForStepOut or not IsSteppedOut then
    exit;
  if not FController.NextOnlyStopOnStartLine then
    exit;

  if IsAtLastHiddenBreakAddr then begin
    {$PUSH}{$Q-}{$R-}
    FThread.StoreStepInfo(FThread.GetInstructionPointerRegisterValue +
      FThread.GetInstructionPointerRegisterValue - 1);
    {$POP}
  end;
  FStepInfoUnavailAfterStepOut := not IsAtLastHiddenBreakAddr;
  FStepInfoUpdatedForStepOut := True;
end;

function TDbgControllerLineStepBaseCmd.HasSteppedAwayFromOriginLine: boolean;
var
  CompRes: TFPDCompareStepInfo;
begin
  Result := IsSteppedOut and (not FController.NextOnlyStopOnStartLine);
  if Result then
    exit;

// LIMIT steps ? // avoid further stepping out ? // stop at leave/ret ?
  if IsSteppedOut and FStepInfoUnavailAfterStepOut then begin
    Result := FController.FCurrentThread.IsAtStartOfLine;
    exit;
  end;

  CompRes := FThread.CompareStepInfo;

  if CompRes in [dcsiSameLine, dcsiZeroLine] then begin
    DebugLn((DBG_VERBOSE or FPDBG_COMMANDS) and (CompRes=dcsiZeroLine), ['LineInfo with number=0']);
    Result := False;
    exit;
  end;

  if CompRes = dcsiNoLineInfo then begin
    // only get here, if the original did have line info, so no line info should not happen
    // check if the next asm is on the same line, otherwise treat as new line
    {$PUSH}{$Q-}{$R-}
    CompRes := FThread.CompareStepInfo(FThread.GetInstructionPointerRegisterValue +
      FThread.GetInstructionPointerRegisterValue - 1);
    {$POP}
    Result := not(CompRes in [dcsiNewLine, dcsiSameLine]); // Step once more, maybe we do a jmp....
    DebugLn(DBG_VERBOSE or FPDBG_COMMANDS, ['UNEXPECTED absence of debug info @',FThread.GetInstructionPointerRegisterValue, ' Out:', FIsSteppedOut, ' Res:', Result]);
    exit;
  end;

  Result := True;
end;

{ TDbgControllerStepIntoLineCmd }

procedure TDbgControllerStepIntoLineCmd.InternalContinue(AProcess: TDbgProcess;
  AThread: TDbgThread);
begin
  assert(FProcess=AProcess, 'TDbgControllerStepIntoLineCmd.DoContinue: FProcess=AProcess');
  if FState = siSteppingCurrent then
  begin
    if CheckForCallAndSetBreak then begin
      FState := siSteppingIn;
      FProcess.Continue(FProcess, FThread, true);
      exit;
    end;
  end;

  FProcess.Continue(FProcess, FThread, FState <> siRunningStepOut);
end;

procedure TDbgControllerStepIntoLineCmd.Init;
begin
  FThread.StoreStepInfo;
  inherited Init;
end;

procedure TDbgControllerStepIntoLineCmd.DoResolveEvent(var AnEvent: TFPDEvent;
  AnEventThread: TDbgThread; out Finished: boolean);
var
  CompRes: TFPDCompareStepInfo;
begin
  UpdateThreadStepInfoAfterStepOut;

  if IsAtOrOutOfHiddenBreakFrame then begin
    RemoveHiddenBreak;
    FState := siSteppingCurrent;
  end;
  assert((FHiddenBreakpoint<>nil) xor (FState=siSteppingCurrent), 'TDbgControllerStepIntoLineCmd.DoResolveEvent: (FHiddenBreakpoint<>nil) xor (FState=siSteppingCurrent)');

  if (FState = siSteppingCurrent) then begin
    Finished := HasSteppedAwayFromOriginLine;
  end
  else begin
    // we stepped into
    CompRes := FThread.CompareStepInfo;
    Finished := CompRes = dcsiNewLine;
  end;

  if Finished then
    AnEvent := deFinishedStep
  else
  if AnEvent in [deFinishedStep] then
    AnEvent:=deInternalContinue;

  If (FState = siSteppingCurrent) or Finished then
    exit;

  // Currently stepped into some method
  assert(FHiddenBreakpoint <> nil, 'TDbgControllerStepIntoLineCmd.DoResolveEvent: Stepping: FHiddenBreakpoint <> nil');

  if FState = siSteppingIn then begin
    FState := siSteppingNested;
    FStepCount := 0;
    FNestDepth := 0;
  end;

  inc(FStepCount);
  if IsAtCallInstruction > 0 then
    inc(FNestDepth);

  // FNestDepth = 2  => About to step into 3rd level nested
  if (FStepCount > 5) or (FNestDepth > 1) then begin
    FState := siRunningStepOut; // run to breakpoint
    exit;
  end;
  // Just step and see if we find line info
end;

{ TDbgControllerStepOverLineCmd }

procedure TDbgControllerStepOverLineCmd.InternalContinue(AProcess: TDbgProcess;
  AThread: TDbgThread);
begin
  assert(FProcess=AProcess, 'TDbgControllerStepOverLineCmd.DoContinue: FProcess=AProcess');
  CheckForCallAndSetBreak;
  FProcess.Continue(FProcess, FThread, FHiddenBreakpoint = nil);
end;

procedure TDbgControllerStepOverLineCmd.Init;
begin
  FThread.StoreStepInfo;
  inherited Init;
end;

procedure TDbgControllerStepOverLineCmd.DoResolveEvent(var AnEvent: TFPDEvent;
  AnEventThread: TDbgThread; out Finished: boolean);
begin
  UpdateThreadStepInfoAfterStepOut;
  if IsAtOrOutOfHiddenBreakFrame then
      RemoveHiddenBreak;

  if FHiddenBreakpoint <> nil then
    Finished := False
  else
    Finished := HasSteppedAwayFromOriginLine;

  if Finished then
    AnEvent := deFinishedStep
  else
  if AnEvent in [deFinishedStep] then
    AnEvent:=deInternalContinue;
end;

{ TDbgControllerStepOutCmd }

function TDbgControllerStepOutCmd.GetOutsideFrame(var AnOutside: Boolean
  ): Boolean;
var
  CodeBin: array[0..30] of byte;
begin
  Result := (FProcess.ReadData(FThread.GetInstructionPointerRegisterValue, sizeof(CodeBin), CodeBin) and
             GetFunctionFrameInfo(@CodeBin[0], sizeof(CodeBin), FProcess.Mode=dm64, AnOutside)
            );
end;

procedure TDbgControllerStepOutCmd.SetReturnAdressBreakpoint(
  AProcess: TDbgProcess; AnOutsideFrame: Boolean);
var
  AStackPointerValue, StepOutStackPos, ReturnAddress: TDBGPtr;
begin
  FWasOutsideFrame := AnOutsideFrame;
  {$PUSH}{$Q-}{$R-}
  if AnOutsideFrame then begin
    StepOutStackPos:=FController.CurrentThread.GetStackPointerRegisterValue;
  end
  else begin
    AStackPointerValue:=FController.CurrentThread.GetStackBasePointerRegisterValue;
    StepOutStackPos:=AStackPointerValue+DBGPTRSIZE[FController.FCurrentProcess.Mode];
  end;
  {$POP}
    debugln(FPDBG_COMMANDS, ['StepOutCmd.SetReturnAdressBreakpoint NoFrame=',AnOutsideFrame,  ' ^RetIP=',dbghex(StepOutStackPos),' SP=',dbghex(FController.CurrentThread.GetStackPointerRegisterValue),' BP=',dbghex(FController.CurrentThread.GetStackBasePointerRegisterValue)]);

  if AProcess.ReadAddress(StepOutStackPos, ReturnAddress) then
    SetHiddenBreak(ReturnAddress)
  else
    debugln(DBG_WARNINGS or FPDBG_COMMANDS, ['Failed to read return-address from stack', ReturnAddress]);
end;

procedure TDbgControllerStepOutCmd.InternalContinue(AProcess: TDbgProcess;
  AThread: TDbgThread);
var
  Opc: TOpCode;
  Outside: Boolean;
begin
  assert(FProcess=AProcess, 'TDbgControllerStepOutCmd.DoContinue: FProcess=AProcess');

  if IsSteppedOut then begin
    CheckForCallAndSetBreak;
  end
  else
  if not assigned(FHiddenBreakpoint) then begin
    if GetOutsideFrame(Outside) then begin
      SetReturnAdressBreakpoint(AProcess, Outside);
    end
    else
    if FStepCount < 12 then
    begin
      // During the prologue and epiloge of a procedure the call-stack might not been
      // setup already. To avoid problems in these cases, start with a few (max
      // 12) single steps.
      Inc(FStepCount);
      Opc := NextOpCode;
      if (Opc = OPcall) or (Opc = OPleave)  then  // asm "call" // set break before "leave" or the frame becomes unavail
      begin
        SetReturnAdressBreakpoint(AProcess, False);
      end
      else
      if (Opc = OPret) or (Opc = OPretf) then  // asm "ret"
      begin
        FStepCount := MaxInt; // Do one more single-step, and we're finished.
        FProcess.Continue(FProcess, FThread, True);
        exit;
      end;
    end
    else
    begin
      // Enough with the single-stepping
      SetReturnAdressBreakpoint(AProcess, False);
    end;
  end;

  FProcess.Continue(FProcess, FThread, FHiddenBreakpoint = nil);
end;

procedure TDbgControllerStepOutCmd.DoResolveEvent(var AnEvent: TFPDEvent;
  AnEventThread: TDbgThread; out Finished: boolean);
begin
  Finished := False;

  // If we stepped out, without a frame, then IsSteppedOut will not detect it
  // The Stack will be popped for the return address.
  if FWasOutsideFrame and (not IsSteppedOut) and
     (FHiddenBreakStackPtrAddr < FThread.GetStackPointerRegisterValue)
  then
    FIsSteppedOut := True;

  if IsSteppedOut then begin
    UpdateThreadStepInfoAfterStepOut;

    if IsAtOrOutOfHiddenBreakFrame then
        RemoveHiddenBreak;

    if FHiddenBreakpoint <> nil then
      Finished := False
    else
      Finished := HasSteppedAwayFromOriginLine;
  end;

  if Finished then
    AnEvent := deFinishedStep
  else
  if AnEvent in [deFinishedStep] then
    AnEvent:=deInternalContinue;
end;

{ TDbgControllerRunToCmd }

constructor TDbgControllerRunToCmd.Create(AController: TDbgController; ALocation: TDBGPtrArray);
begin
  FLocation:=ALocation;
  inherited create(AController);
end;

procedure TDbgControllerRunToCmd.InternalContinue(AProcess: TDbgProcess;
  AThread: TDbgThread);
begin
  assert(FProcess=AProcess, 'TDbgControllerRunToCmd.DoContinue: FProcess=AProcess');
  FProcess.Continue(FProcess, FThread, False);
end;

procedure TDbgControllerRunToCmd.Init;
begin
  inherited Init;
  FHiddenBreakpoint := FProcess.AddInternalBreak(FLocation);
end;

procedure TDbgControllerRunToCmd.DoResolveEvent(var AnEvent: TFPDEvent;
  AnEventThread: TDbgThread; out Finished: boolean);
begin
  Finished := (AnEvent<>deInternalContinue);
  if Finished then begin
    RemoveHiddenBreak;
    AnEvent := deFinishedStep;
  end;
end;


{ TDbgController }

procedure TDbgController.DoOnDebugInfoLoaded(Sender: TObject);
begin
  if Assigned(FOnDebugInfoLoaded) then
    FOnDebugInfoLoaded(Self);
end;

procedure TDbgController.SetParams(AValue: TStringList);
begin
  if FParams=AValue then Exit;
  FParams.Assign(AValue);
end;

procedure TDbgController.SetExecutableFilename(AValue: string);
begin
  if FExecutableFilename=AValue then Exit;
  FExecutableFilename:=AValue;
end;

procedure TDbgController.SetEnvironment(AValue: TStrings);
begin
  if FEnvironment=AValue then Exit;
  FEnvironment.Assign(AValue);
end;

function TDbgController.GetCurrentThreadId: Integer;
begin
  Result := FCurrentThread.ID;
end;

procedure TDbgController.SetCurrentThreadId(AValue: Integer);
var
  ExistingThread: TDbgThread;
begin
  if FCurrentThread.ID = AValue then Exit;

  if not FCurrentProcess.GetThread(AValue, ExistingThread) then begin
    debugln(DBG_WARNINGS, ['SetCurrentThread() unknown thread id: ', AValue]);
    // raise ...
    exit;
  end;
  FCurrentThread := ExistingThread;
end;

destructor TDbgController.Destroy;
var
  it: TMapIterator;
  p: TDbgProcess;
begin
  if FCommand <> nil then begin
    FCommand.FProcess := nil;
    FCommand.FThread := nil;
    FCommand.Free;
  end;
  if FCommandToBeFreed <> nil then begin
    FCommandToBeFreed.FProcess := nil;
    FCommandToBeFreed.FThread := nil;
    FCommandToBeFreed.Free;
  end;

  if Assigned(FMainProcess) then begin
    FProcessMap.Delete(FMainProcess.ProcessID);
    FMainProcess.Free;
  end;

  it := TMapIterator.Create(FProcessMap);
  while not it.EOM do begin
    it.GetData(p);
    p.Free;
    it.Next;
  end;
  it.Free;
  FProcessMap.Free;

  FParams.Free;
  FEnvironment.Free;
  inherited Destroy;
end;

procedure TDbgController.InitializeCommand(ACommand: TDbgControllerCmd);
begin
  if assigned(FCommand) then
    raise exception.create('Prior command not finished yet.');
  DebugLn(FPDBG_COMMANDS, 'Initialized command '+ACommand.ClassName);
  FCommand := ACommand;
end;

function TDbgController.Run: boolean;
var
  Flags: TStartInstanceFlags;
begin
  result := False;
  if assigned(FMainProcess) then
    begin
    DebugLn(DBG_WARNINGS, 'The debuggee is already running');
    Exit;
    end;

  if FExecutableFilename = '' then
    begin
    DebugLn(DBG_WARNINGS, 'No filename given to execute.');
    Exit;
    end;

  if not FileExists(FExecutableFilename) then
    begin
    DebugLn(DBG_WARNINGS, 'File %s does not exist.',[FExecutableFilename]);
    Exit;
    end;

  Flags := [];
  if RedirectConsoleOutput then Include(Flags, siRediretOutput);
  if ForceNewConsoleWin then Include(Flags, siForceNewConsole);
  if AttachToPid <> 0 then
    FCurrentProcess := OSDbgClasses.DbgProcessClass.AttachToInstance(FExecutableFilename, AttachToPid)
  else
    FCurrentProcess := OSDbgClasses.DbgProcessClass.StartInstance(FExecutableFilename, Params, Environment, WorkingDirectory, FConsoleTty, Flags);
  if assigned(FCurrentProcess) then
    begin
    FProcessMap.Add(FCurrentProcess.ProcessID, FCurrentProcess);
    DebugLn(DBG_VERBOSE, 'Got PID: %d, TID: %d', [FCurrentProcess.ProcessID, FCurrentProcess.ThreadID]);
    result := true;
    end;
end;

procedure TDbgController.Stop;
begin
  if assigned(FMainProcess) then
    FMainProcess.TerminateProcess
  else
    raise Exception.Create('Failed to stop debugging. No main process.');
end;

procedure TDbgController.StepIntoInstr;
begin
  InitializeCommand(TDbgControllerStepIntoInstructionCmd.Create(self));
end;

procedure TDbgController.StepOverInstr;
begin
  InitializeCommand(TDbgControllerStepOverInstructionCmd.Create(self));
end;

procedure TDbgController.Next;
begin
  InitializeCommand(TDbgControllerStepOverLineCmd.Create(self));
end;

procedure TDbgController.Step;
begin
  InitializeCommand(TDbgControllerStepIntoLineCmd.Create(self));
end;

procedure TDbgController.StepOut;
begin
  InitializeCommand(TDbgControllerStepOutCmd.Create(self));
end;

function TDbgController.Pause: boolean;
begin
  InterLockedExchange(FPauseRequest, 1);
  Result := InterLockedExchangeAdd(FRunning, 0) = 0; // not running
  if not Result then
    Result := FCurrentProcess.Pause;
end;

function TDbgController.Detach: boolean;
begin
  InterLockedExchange(FDetaching, 1);
  Result := Pause;
end;

procedure TDbgController.ProcessLoop;

  function MaybeDetach: boolean;
  begin
    Result := InterLockedExchange(FDetaching, 0) <> 0;
    if not Result then
      exit;

    if Assigned(FCommand) then
      FreeAndNil(FCommand);
    FPDEvent := deFinishedStep; // go to pause, if detach fails
    if FCurrentProcess.Detach(FCurrentProcess, FCurrentThread) then
      FPDEvent := deExitProcess;
  end;
var
  AProcessIdentifier: THandle;
  AThreadIdentifier: THandle;
  AExit: boolean;
  IsFinished, b: boolean;
  EventProcess: TDbgProcess;
  DummyThread: TDbgThread;

begin
  AExit:=false;
  if FCurrentProcess = nil then begin
    DebugLn(DBG_WARNINGS, 'Error: Processloop has no process');
    exit;
  end;

  FreeAndNil(FCommandToBeFreed);
  if FCommand <> nil then
    FCommand.DoBeforeLoopStart;

  if MaybeDetach then
    exit;

  FCurrentProcess.ThreadsClearCallStack;

  repeat
    if assigned(FCurrentProcess) and not assigned(FMainProcess) then begin
      // IF there is a pause-request, we will hit a deCreateProcess.
      // No need to indicate FRunning
      FMainProcess:=FCurrentProcess;
    end
    else
    begin
      InterLockedExchange(FRunning, 1);
      // if Pause() is called right here, an Interrupt-Event is scheduled, even though we do not run (yet)
      if InterLockedExchangeAdd(FPauseRequest, 0) = 1 then begin
        FPDEvent := deBreakpoint;
        InterLockedExchange(FRunning, 0);
        break; // no event handling. Keep Process/Thread from last run
      end
      else begin
      if not assigned(FCommand) then
        begin
        DebugLn(FPDBG_COMMANDS, 'Continue process without command.');
        FCurrentProcess.Continue(FCurrentProcess, FCurrentThread, False)
        end
      else
        begin
        DebugLn(FPDBG_COMMANDS, 'Continue process with command '+FCommand.ClassName);
        FCommand.DoContinue(FCurrentProcess, FCurrentThread);
        end;

        // TODO: replace the dangling pointer with the next best value....
        // There is still a race condition, for another thread to access it...
        if (FCurrentThread <> nil) and not FCurrentProcess.GetThread(FCurrentThread.ID, DummyThread) then
          FreeAndNil(FCurrentThread);
    end;
    end;
    if not FCurrentProcess.WaitForDebugEvent(AProcessIdentifier, AThreadIdentifier) then
      Continue;
    InterLockedExchange(FRunning, 0);

    (* Do not change CurrentProcess/Thread,
       unless the debugger can actually controll/debug those processes
       - If FCurrentProcess is not set to FMainProcess then Pause will fail
          (because a process that is not debugged, can not be paused,
           and if it were debugged, *all* debugged processes may need to be paused)
       - The LazFpDebugger may try to access FCurrentThread. If that is nil, it may crash.
         e.g. TFPThreads.RequestMasterData

       This may need 3 threads: main, user-selected (thread win), current-event

       deExitProcess relies on only the main process receiving this.

    *)
    //FCurrentProcess := nil;
    //FCurrentThread := nil;
    EventProcess := nil;
//    if not GetProcess(AProcessIdentifier, FCurrentProcess) then
    if not GetProcess(AProcessIdentifier, EventProcess) then
      begin
      // A second/third etc process has been started.
      (* A process was created/forked
         However the debugger currently does not attach to it on all platforms
           so maybe other processes should be ignored?
           It seems on windows/linux it does NOT attach.
           On Mac, it may attempt to attach.
         If the process is not debugged, it may not receive an deExitProcess
      *)
      (* As above, currently do not change those variables,
         just continue the process-loop (as "FCurrentProcess<>FMainProcess" would do)
      *)
      //FCurrentProcess := OSDbgClasses.DbgProcessClass.Create('', AProcessIdentifier, AThreadIdentifier, OnLog);
      //FProcessMap.Add(AProcessIdentifier, FCurrentProcess);

      Continue; // ***** This will swallow all FPDEvent for unknow processes *****
      end;

    if EventProcess<>FMainProcess then
    //if FCurrentProcess<>FMainProcess then
      // Just continue the process. Only the main-process is being debugged.
      Continue;

    if not FCurrentProcess.GetThread(AThreadIdentifier, FCurrentThread) then
      FCurrentThread := FCurrentProcess.AddThread(AThreadIdentifier);

    (* TODO: ExitThread **********
       at least the winprocess handles exitthread in the next line.
       this will remove CurrentThread form the list of threads
       CurrentThread is then destroyed in the next call to continue....
    *)

    FPDEvent:=FCurrentProcess.ResolveDebugEvent(FCurrentThread);
    if FCurrentThread <> nil then DebugLn(DBG_VERBOSE, 'Process stopped with event %s. IP=%s, SP=%s, BSP=%s. HasBreak: %s',
                         [FPDEventNames[FPDEvent],
                         FCurrentProcess.FormatAddress(FCurrentThread.GetInstructionPointerRegisterValue),
                         FCurrentProcess.FormatAddress(FCurrentThread.GetStackPointerRegisterValue),
                         FCurrentProcess.FormatAddress(FCurrentThread.GetStackBasePointerRegisterValue),
                         dbgs(CurrentProcess.CurrentBreakpoint<>nil)]);

    if MaybeDetach then
      break;

    IsFinished:=false;
    if FPDEvent=deExitProcess then
      FreeAndNil(FCommand)
    else
    if assigned(FCommand) then
    begin
      FCommand.ResolveEvent(FPDEvent, FCurrentThread, IsFinished);
      DebugLn(FPDBG_COMMANDS, 'Command %s: IsFinished=%s', [FCommand.ClassName, dbgs(IsFinished)])
    end;

    AExit:=true;
    if not IsFinished then
    begin
     case FPDEvent of
       deInternalContinue: AExit := False;
       deBreakpoint: begin
           b := FCurrentProcess.GetAndClearPauseRequested;
           AExit := (FCurrentProcess.CurrentBreakpoint <> nil) or
                    ( (FCurrentProcess.CurrentWatchpoint <> nil) and (FCurrentProcess.CurrentWatchpoint <> Pointer(-1)) ) or
                    (b and (InterLockedExchangeAdd(FPauseRequest, 0) = 1))
         end;
{        deLoadLibrary :
          begin
            if FCurrentProcess.GetLib(FCurrentProcess.LastEventProcessIdentifier, ALib)
            and (GImageInfo <> iiNone)
            then begin
              WriteLN('Name: ', ALib.Name);
              //if GImageInfo = iiDetail
              //then DumpPEImage(Proc.Handle, Lib.BaseAddr);
            end;
            if GBreakOnLibraryLoad
            then GState := dsPause;

          end; }
      end; {case}
    end;
    if IsFinished then
      FreeAndNil(FCommand);
  until AExit or (InterLockedExchangeAdd(FPauseRequest, 0) = 1);
end;

procedure TDbgController.SendEvents(out continue: boolean);
var
  HasPauseRequest: Boolean;
  CurWatch: TFpInternalWatchpoint;
begin
  // reset pause request. If Pause() is called after this, it will be seen in the next loop
  HasPauseRequest := InterLockedExchange(FPauseRequest, 0) = 1;
  CurWatch := nil;
  if (FCurrentProcess.CurrentWatchpoint <> nil) and (FCurrentProcess.CurrentWatchpoint <> Pointer(-1)) then
    CurWatch := TFpInternalWatchpoint(FCurrentProcess.CurrentWatchpoint);

  case FPDEvent of
    deCreateProcess:
      begin
      (* Only events for the main process get here / See ProcessLoop *)
        FCurrentProcess.LoadInfo;
        if not FCurrentProcess.DbgInfo.HasInfo then
          DebugLn(DBG_WARNINGS, 'No Dwarf-debug information available. The debugger will not function properly. [CurrentProcess='+dbgsname(FCurrentProcess)+',DbgInfo='+dbgsname(FCurrentProcess.DbgInfo)+']');

        DoOnDebugInfoLoaded(self);

        continue:=true;
        if assigned(OnCreateProcessEvent) then
          OnCreateProcessEvent(continue);
      end;
    deFinishedStep:
      begin
        if assigned(OnHitBreakpointEvent) then begin
          // if there is a breakpoint at the stepping end, execute its actions
          continue:=false;
          if (CurWatch <> nil) and assigned(OnHitBreakpointEvent) then
            OnHitBreakpointEvent(continue, CurWatch);
          continue:=false;
          if assigned(FCurrentProcess.CurrentBreakpoint) then
            OnHitBreakpointEvent(continue, FCurrentProcess.CurrentBreakpoint);

          // TODO: dedicated event to set pause and location
          // ensure state = dsPause and location is set
          continue:=false;
          OnHitBreakpointEvent(continue, nil);
          HasPauseRequest := False;
        end;
        continue:=false;
      end;
    deBreakpoint:
      begin
        // If there is no breakpoint AND no pause-request then this is a deferred, allready handled pause request
        continue := (FCurrentProcess.CurrentBreakpoint = nil) and (CurWatch = nil) and (not HasPauseRequest);
        if (not continue) and assigned(OnHitBreakpointEvent) then begin
          if (CurWatch <> nil) then
            OnHitBreakpointEvent(continue, CurWatch);
          if assigned(FCurrentProcess.CurrentBreakpoint) then
            OnHitBreakpointEvent(continue, FCurrentProcess.CurrentBreakpoint);
          HasPauseRequest := False;
        end;
      end;
    deExitProcess:
      begin
      (* Only events for the main process get here / See ProcessLoop *)
        if FCurrentProcess = FMainProcess then FMainProcess := nil;
        FCurrentProcess.GotExitProcess := True;

        if assigned(OnProcessExitEvent) then
          OnProcessExitEvent(FCurrentProcess.ExitCode);

        FProcessMap.Delete(FCurrentProcess.ProcessID);
        FCurrentProcess.Free;
        FCurrentProcess := nil;
        HasPauseRequest := False;
        continue := false;
      end;
    deException:
      begin
        continue:=false;
        if assigned(OnExceptionEvent) then
          OnExceptionEvent(continue, FCurrentProcess.ExceptionClass, FCurrentProcess.ExceptionMessage );
        if not continue then
          HasPauseRequest := False;
      end;
    deLoadLibrary:
      begin
        continue:=true;
        if assigned(OnLibraryLoadedEvent) and Assigned(FCurrentProcess.LastLibraryLoaded) then
          OnLibraryLoadedEvent(continue, FCurrentProcess.LastLibraryLoaded);
      end;
    deUnloadLibrary:
      begin
        continue:=true;
        if assigned(OnLibraryUnloadedEvent) and Assigned(FCurrentProcess.LastLibraryUnloaded) then
          OnLibraryUnloadedEvent(continue, FCurrentProcess.LastLibraryUnloaded);
        FCurrentProcess.LastLibraryUnloaded := nil;
      end;
    deInternalContinue:
      begin
        continue := true;
      end;
    else
      raise exception.create('Unknown debug controler state');
  end;
  if HasPauseRequest then begin
    continue := False;
    if assigned(OnHitBreakpointEvent) then
      OnHitBreakpointEvent(continue, nil);
  end;

  if (not &continue) and (FCommand <> nil) then begin
    assert(FCommandToBeFreed=nil, 'TDbgController.SendEvents: FCommandToBeFreed=nil');
    FCommandToBeFreed := FCommand;
    FCommand := nil;
  end;
end;

function TDbgController.GetProcess(const AProcessIdentifier: THandle; out AProcess: TDbgProcess): Boolean;
begin
  Result := FProcessMap.GetData(AProcessIdentifier, AProcess) and (AProcess <> nil);
end;

constructor TDbgController.Create;
begin
  FParams := TStringList.Create;
  FEnvironment := TStringList.Create;
  FProcessMap := TMap.Create(itu4, SizeOf(TDbgProcess));
  FNextOnlyStopOnStartLine := true;
end;

initialization
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  FPDBG_COMMANDS := DebugLogger.FindOrRegisterLogGroup('FPDBG_COMMANDS' {$IFDEF FPDBG_COMMANDS} , True {$ENDIF} );

end.

