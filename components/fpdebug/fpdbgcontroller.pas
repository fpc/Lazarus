unit FPDbgController;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Maps,
  LazLoggerBase,
  DbgIntfBaseTypes,
  FpDbgDisasX86,
  FpDbgClasses;

type

  TOnCreateProcessEvent = procedure(var continue: boolean) of object;
  TOnHitBreakpointEvent = procedure(var continue: boolean; const Breakpoint: TFpInternalBreakpoint) of object;
  TOnExceptionEvent = procedure(var continue: boolean; const ExceptionClass, ExceptionMessage: string) of object;
  TOnProcessExitEvent = procedure(ExitCode: DWord) of object;

  TDbgController = class;

  { TDbgControllerCmd }

  TDbgControllerCmd = class
  protected
    FController: TDbgController;
    FThread: TDbgThread;
    FProcess: TDbgProcess;
    procedure Init; virtual;
    function IsAtCallInstruction: Integer;
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Handled, Finished: boolean); virtual; abstract;
  public
    constructor Create(AController: TDbgController); virtual;
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); virtual; abstract;
    procedure ResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Handled, Finished: boolean); virtual;
  end;

  { TDbgControllerContinueCmd }

  TDbgControllerContinueCmd = class(TDbgControllerCmd)
  protected
    procedure Init; override;
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Handled, Finished: boolean); override;
  public
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
  end;

  { TDbgControllerStepIntoInstructionCmd }

  TDbgControllerStepIntoInstructionCmd = class(TDbgControllerCmd)
  protected
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Handled, Finished: boolean); override;
  public
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
  end;

  { TDbgControllerHiddenBreakStepCmd }

  TDbgControllerHiddenBreakStepCmd = class(TDbgControllerCmd)
  protected
    FHiddenBreakpoint: TFpInternalBreakpoint;
    procedure RemoveHiddenBreak;
  public
    destructor Destroy; override;
  end;

  { TDbgControllerStepOverInstructionCmd }

  TDbgControllerStepOverInstructionCmd = class(TDbgControllerHiddenBreakStepCmd)
  protected
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Handled, Finished: boolean); override;
  public
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
  end;

  { TDbgControllerStepIntoLineCmd }

  TDbgControllerStepIntoLineCmd = class(TDbgControllerHiddenBreakStepCmd)
  private
    FStoredStackFrame: TDBGPtr;
    FInto: boolean;
    FHiddenWatchpointInto: integer;
    FHiddenWatchpointOut: integer;
    FHiddenWatchpointOutStackbase: integer;
    FLastStackPointerValue: TDBGPtr;
    FLastStackBaseValue: TDBGPtr;
    FAssumedProcStartStackPointer: TDBGPtr;
    FInstCount: integer;
  protected
    procedure Init; override;
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Handled, Finished: boolean); override;
  public
    constructor Create(AController: TDbgController); override;
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
  end;

  { TDbgControllerStepOverLineCmd }

  TDbgControllerStepOverLineCmd = class(TDbgControllerStepOverInstructionCmd)
  private
    FStoredStackFrame: TDBGPtr;
  protected
    procedure Init; override;
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Handled, Finished: boolean); override;
  public
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
  end;

  { TDbgControllerStepOutCmd }

  TDbgControllerStepOutCmd = class(TDbgControllerHiddenBreakStepCmd)
  private
    FIsSet: boolean;
    FStepCount: Integer;
    FStepOut: Boolean;
  protected
    procedure SetReturnAdressBreakpoint(AProcess: TDbgProcess);
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Handled, Finished: boolean); override;
  public
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
  end;

  { TDbgControllerRunToCmd }

  TDbgControllerRunToCmd = class(TDbgControllerHiddenBreakStepCmd)
  private
    FLocation: TDBGPtrArray;
  protected
    procedure Init; override;
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Handled, Finished: boolean); override;
  public
    constructor Create(AController: TDbgController; ALocation: TDBGPtrArray);
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
  end;

  { TDbgController }

  TDbgController = class
  private
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
    FCommand: TDbgControllerCmd;
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
    procedure ProcessLoop;
    procedure SendEvents(out continue: boolean);

    property ExecutableFilename: string read FExecutableFilename write SetExecutableFilename;
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

constructor TDbgControllerCmd.Create(AController: TDbgController);
begin
  FController := AController;
  FProcess := FController.CurrentProcess;
  FThread := FController.CurrentThread;
  Init;
end;

procedure TDbgControllerCmd.ResolveEvent(var AnEvent: TFPDEvent;
  AnEventThread: TDbgThread; out Handled, Finished: boolean);
var
  dummy: TDbgThread;
begin
  if FThread <> nil then begin
    // ResolveDebugEvent will have removed the thread, but not yet destroyed it
    // Finish, if the thread has gone.
    Handled := False;
    Finished := not FProcess.GetThread(FThread.ID, dummy);
    if Finished then
      exit;
    // Only react to events for the correct thread. (Otherwise return Finished = False)
    if FThread <> AnEventThread then
      exit;
  end;
  DoResolveEvent(AnEvent, AnEventThread, Handled, Finished);
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
  AnEventThread: TDbgThread; out Handled, Finished: boolean);
begin
  Handled := false;
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
  var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Handled,
  Finished: boolean);
begin
  Finished := (AnEvent<>deInternalContinue);
  Handled := Finished;
  if Finished then
    AnEvent := deFinishedStep;
end;

{ TDbgControllerHiddenBreakStepCmd }

procedure TDbgControllerHiddenBreakStepCmd.RemoveHiddenBreak;
begin
  if assigned(FHiddenBreakpoint) then
    FreeAndNil(FHiddenBreakpoint);
end;

destructor TDbgControllerHiddenBreakStepCmd.Destroy;
begin
  RemoveHiddenBreak;
  inherited Destroy;
end;

{ TDbgControllerStepOverInstructionCmd }

procedure TDbgControllerStepOverInstructionCmd.DoContinue(AProcess: TDbgProcess; AThread: TDbgThread);
var
  l: Integer;
begin
  assert(FProcess=AProcess, 'TDbgControllerStepOverInstructionCmd.DoContinue: FProcess=AProcess');
  if FHiddenBreakpoint = nil then begin
    l := IsAtCallInstruction;
    if l > 0 then
      FHiddenBreakpoint := AProcess.AddInternalBreak(FThread.GetInstructionPointerRegisterValue + l);
  end;
  FProcess.Continue(FProcess, FThread, FHiddenBreakpoint = nil);
end;

procedure TDbgControllerStepOverInstructionCmd.DoResolveEvent(
  var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Handled,
  Finished: boolean);
begin
  Finished := not (AnEvent in [deInternalContinue, deLoadLibrary]);
  Handled := Finished;
  if Finished then
  begin
    AnEvent := deFinishedStep;
    RemoveHiddenBreak;
  end;
end;

{ TDbgControllerStepIntoLineCmd }

procedure TDbgControllerStepIntoLineCmd.Init;
begin
  inherited Init;
  FHiddenWatchpointInto:=-1;
  FHiddenWatchpointOut:=-1;
  FHiddenWatchpointOutStackbase:=-1;
  FStoredStackFrame:=FThread.GetStackBasePointerRegisterValue;
  FThread.StoreStepInfo;
end;

constructor TDbgControllerStepIntoLineCmd.Create(AController: TDbgController);
begin
  inherited Create(AController);
end;

procedure TDbgControllerStepIntoLineCmd.DoContinue(AProcess: TDbgProcess; AThread: TDbgThread);
var
  l: Integer;
begin
  assert(FProcess=AProcess, 'TDbgControllerStepIntoLineCmd.DoContinue: FProcess=AProcess');
  if not FInto then
  begin
    l := IsAtCallInstruction;
    if l > 0 then begin
      FInto := true;
      FInstCount := 0;
      FHiddenBreakpoint := AProcess.AddInternalBreak(FThread.GetInstructionPointerRegisterValue + l);
      FProcess.Continue(FProcess, FThread, true);
      exit;
    end;
  end;

  FProcess.Continue(FProcess, FThread, (FHiddenWatchpointInto=-1) and (FHiddenWatchpointOut=-1));
end;

procedure TDbgControllerStepIntoLineCmd.DoResolveEvent(var AnEvent: TFPDEvent;
  AnEventThread: TDbgThread; out Handled, Finished: boolean);

var
  AStackPointerValue: TDBGPtr;
  AStackBasePointerValue: TDBGPtr;

  procedure SetHWBreakpoints;
  var
    OutStackPos: TDBGPtr;
    StackBasePos: TDBGPtr;
    IntoStackPos: TDBGPtr;
  begin
    IntoStackPos:=AStackPointerValue-DBGPTRSIZE[FController.FCurrentProcess.Mode];
    OutStackPos:=FAssumedProcStartStackPointer;
    StackBasePos:=AStackBasePointerValue;

    FHiddenWatchpointInto := FController.FCurrentThread.AddWatchpoint(IntoStackPos);
    FHiddenWatchpointOut := FController.FCurrentThread.AddWatchpoint(OutStackPos);
    FHiddenWatchpointOutStackbase := FController.FCurrentThread.AddWatchpoint(StackBasePos);
  end;

begin
  if (FHiddenWatchpointOut<>-1) and FController.FCurrentThread.RemoveWatchpoint(FHiddenWatchpointOut) then
    FHiddenWatchpointOut:=-1;
  if (FHiddenWatchpointInto<>-1) and FController.FCurrentThread.RemoveWatchpoint(FHiddenWatchpointInto) then
    FHiddenWatchpointInto:=-1;
  if (FHiddenWatchpointOutStackbase<>-1) and FController.FCurrentThread.RemoveWatchpoint(FHiddenWatchpointOutStackbase) then
    FHiddenWatchpointOutStackbase:=-1;

  AStackPointerValue:=FController.CurrentThread.GetStackPointerRegisterValue;
  AStackBasePointerValue:=FController.CurrentThread.GetStackBasePointerRegisterValue;

  Finished := not (AnEvent in [deInternalContinue, deLoadLibrary]);
  if (AnEvent=deBreakpoint) and not assigned(FController.FCurrentProcess.CurrentBreakpoint) then
  begin
    if FController.FCurrentThread.CompareStepInfo<>dcsiNewLine then
    begin
      AnEvent:=deInternalContinue;
      Finished:=false;
      inc(FInstCount);

      if FInto then
      begin
        if FHiddenBreakpoint.HasLocation(FThread.GetInstructionPointerRegisterValue) then
        begin
          FInto:=false;
          FInstCount:=0;
          RemoveHiddenBreak;
        end
        else
        begin
          if FInstCount=1 then
            FAssumedProcStartStackPointer:=AStackPointerValue;
          if (AStackBasePointerValue<>FLastStackBaseValue) or (AStackPointerValue<>FLastStackPointerValue) then
            FInstCount:=1;

          if FInstCount>4 then
            SetHWBreakpoints;
        end;
      end
      else
        FInstCount := 0;
    end
    else
    begin
      // Also check if the current instruction is at the start of a new
      // sourceline. (Dwarf only)
      // Don't do this while stepping into a procedure, only when stepping out.
      // This because when stepping out of a procedure, the first asm-instruction
      // could still be part of the instruction-line that made the call to the
      // procedure in the first place.
      if ((FStoredStackFrame<AStackBasePointerValue) or (FController.NextOnlyStopOnStartLine))
        and not FController.FCurrentThread.IsAtStartOfLine then
      begin
        Finished:=false;
        AnEvent:=deInternalContinue;
      end;
    end;
  end;
  Handled := Finished;
  if Finished then
    AnEvent := deFinishedStep;
  FLastStackPointerValue:=AStackPointerValue;
  FLastStackBaseValue:=AStackBasePointerValue;
end;

{ TDbgControllerStepOverLineCmd }

procedure TDbgControllerStepOverLineCmd.Init;
begin
  inherited Init;
  FThread.StoreStepInfo;
  FStoredStackFrame:=FThread.GetStackBasePointerRegisterValue;
end;

procedure TDbgControllerStepOverLineCmd.DoContinue(AProcess: TDbgProcess; AThread: TDbgThread);
begin
  assert(FProcess=AProcess, 'TDbgControllerStepOverLineCmd.DoContinue: FProcess=AProcess');
  inherited DoContinue(AProcess, AThread);
end;

procedure TDbgControllerStepOverLineCmd.DoResolveEvent(var AnEvent: TFPDEvent;
  AnEventThread: TDbgThread; out Handled, Finished: boolean);
var
  EventCopy: TFPDEvent;
begin
  EventCopy := AnEvent;
  inherited DoResolveEvent(EventCopy, AnEventThread, Handled, Finished);
  // todo: check FHiddenBreakpoint.HasLocation();
  if (AnEvent=deBreakpoint) and not assigned(FController.CurrentProcess.CurrentBreakpoint)
  then
  begin
    if (FController.FCurrentThread.CompareStepInfo<>dcsiNewLine) or
      (not FController.FCurrentThread.IsAtStartOfLine and
       (FController.NextOnlyStopOnStartLine or (FStoredStackFrame < FController.CurrentThread.GetStackBasePointerRegisterValue))) then
    begin
      AnEvent:=deInternalContinue;
      RemoveHiddenBreak;
      Finished:=false;
    end;
  end;
  Handled := Finished;
  if Finished then
    AnEvent := deFinishedStep;
end;

{ TDbgControllerStepOutCmd }

procedure TDbgControllerStepOutCmd.SetReturnAdressBreakpoint(AProcess: TDbgProcess);
var
  AStackPointerValue, StepOutStackPos, ReturnAddress: TDBGPtr;
begin
  AStackPointerValue:=FController.CurrentThread.GetStackBasePointerRegisterValue;
  StepOutStackPos:=AStackPointerValue+DBGPTRSIZE[FController.FCurrentProcess.Mode];

  if AProcess.ReadAddress(StepOutStackPos, ReturnAddress) then
  begin
    FHiddenBreakpoint := AProcess.AddInternalBreak(ReturnAddress)
  end
  else
  begin
    debugln(DBG_WARNINGS, 'Failed to read return-address from stack');
  end;

  FIsSet:=true;
end;

procedure TDbgControllerStepOutCmd.DoContinue(AProcess: TDbgProcess; AThread: TDbgThread);
var
  CodeBin: array[0..20] of byte;
  p: pointer;
  ADump,
  AStatement: string;
begin
  assert(FProcess=AProcess, 'TDbgControllerStepOutCmd.DoContinue: FProcess=AProcess');
  if FIsSet then
    // When a breanpoint has already been set on the return-adress, just continue
    FProcess.Continue(FProcess, FThread, false)
  else if FStepCount < 12 then
  begin
    // During the prologue and epiloge of a procedure the call-stack might not been
    // setup already. To avoid problems in these cases, start with a few (max
    // 12) single steps.
    Inc(FStepCount);
    if AProcess.ReadData(AThread.GetInstructionPointerRegisterValue,sizeof(CodeBin),CodeBin) then
    begin
      p := @CodeBin;
      Disassemble(p, AProcess.Mode=dm64, ADump, AStatement);
      if (copy(AStatement,1,4)='call') then
      begin
        // Stop with the single-steps, set an hidden breakpoint at the return
        // address and continue.
        SetReturnAdressBreakpoint(AProcess);
        FProcess.Continue(FProcess, FThread, False);
      end
      else if (copy(AStatement,1,3)='ret') then
      begin
        // Do one more single-step, and we're finished.
        FStepOut := True;
        FProcess.Continue(FProcess, FThread, True);
      end
      else
        FProcess.Continue(FProcess, FThread, True);
    end
    else
      FProcess.Continue(FProcess, FThread, True);
  end
  else
  begin
    // Enough with the single-stepping, set an hidden breakpoint at the return
    // address, and continue.
    SetReturnAdressBreakpoint(AProcess);
    FProcess.Continue(FProcess, FThread, False);
  end;
end;

procedure TDbgControllerStepOutCmd.DoResolveEvent(var AnEvent: TFPDEvent;
  AnEventThread: TDbgThread; out Handled, Finished: boolean);
begin
  Finished := false;

  if FStepOut then
    // During single-stepping a 'ret' instruction was encountered. So we're just
    // finished.
    Finished := true
  else if FIsSet then
    Finished := (not (AnEvent in [deInternalContinue, deLoadLibrary])) or (FHiddenBreakpoint = nil)
  else if (AnEvent in [deBreakpoint]) and not FProcess.HasBreak(FThread.GetInstructionPointerRegisterValue) then
    // Single-stepping, so continue silently.
    AnEvent := deInternalContinue;

  if Finished then
  begin
    AnEvent := deFinishedStep;
    RemoveHiddenBreak;
  end;
  Handled := Finished;
end;

{ TDbgControllerRunToCmd }

constructor TDbgControllerRunToCmd.Create(AController: TDbgController; ALocation: TDBGPtrArray);
begin
  FLocation:=ALocation;
  inherited create(AController);
end;

procedure TDbgControllerRunToCmd.DoContinue(AProcess: TDbgProcess; AThread: TDbgThread);
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
  AnEventThread: TDbgThread; out Handled, Finished: boolean);
begin
  Finished := (AnEvent<>deInternalContinue);
  Handled := Finished;
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
  result := FCurrentProcess.Pause;
end;

procedure TDbgController.ProcessLoop;

var
  AProcessIdentifier: THandle;
  AThreadIdentifier: THandle;
  AExit: boolean;
  IsHandled: boolean;
  IsFinished: boolean;
  EventProcess: TDbgProcess;
  DummyThread: TDbgThread;
  ctid: Integer;

begin
  AExit:=false;
  if FCurrentProcess = nil then begin
    DebugLn(DBG_WARNINGS, 'Error: Processloop has no process');
    exit;
  end;

  repeat
    if assigned(FCurrentProcess) and not assigned(FMainProcess) then
      FMainProcess:=FCurrentProcess
    else
    begin
      ctid := 0;
      if FCurrentThread <> nil then
        ctid := FCurrentThread.ID;
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
        if (ctid <> 0) and not FCurrentProcess.GetThread(ctid, DummyThread) then
          FCurrentThread := nil;
    end;
    if not FCurrentProcess.WaitForDebugEvent(AProcessIdentifier, AThreadIdentifier) then Continue;

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
    DebugLn(DBG_VERBOSE, 'Process stopped with event %s. IP=%s, SP=%s, BSP=%s. HasBreak: %s',
                         [FPDEventNames[FPDEvent],
                         FCurrentProcess.FormatAddress(FCurrentThread.GetInstructionPointerRegisterValue),
                         FCurrentProcess.FormatAddress(FCurrentThread.GetStackPointerRegisterValue),
                         FCurrentProcess.FormatAddress(FCurrentThread.GetStackBasePointerRegisterValue),
                         dbgs(CurrentProcess.CurrentBreakpoint<>nil)]);
    IsHandled:=false;
    IsFinished:=false;
    if FPDEvent=deExitProcess then
      FreeAndNil(FCommand)
    else
    if assigned(FCommand) then
    begin
      FCommand.ResolveEvent(FPDEvent, FCurrentThread, IsHandled, IsFinished);
      DebugLn(FPDBG_COMMANDS, 'Command %s: IsFinished=%s, IsHandled=%s', [FCommand.ClassName, dbgs(IsFinished), dbgs(IsHandled)])
    end;

    AExit:=true;
    if not IsHandled then
    begin
     case FPDEvent of
       deInternalContinue: AExit := False;
       deBreakpoint: AExit := (FCurrentProcess.CurrentBreakpoint <> nil) or // no breakpoint? continue
                              (FCurrentProcess.GetAndClearPauseRequested);
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
  until AExit;
end;

procedure TDbgController.SendEvents(out continue: boolean);
begin
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
        continue:=false;
        // if there is a breakpoint at the stepping end, execute its actions
        if assigned(OnHitBreakpointEvent) and assigned(FCurrentProcess.CurrentBreakpoint) then
          OnHitBreakpointEvent(continue, FCurrentProcess.CurrentBreakpoint);
        if continue or not assigned(FCurrentProcess.CurrentBreakpoint) then begin
          // TODO: dedicated event to set pause and location
          // ensure state = dsPause and location is set
          continue:=false;
          OnHitBreakpointEvent(continue, nil);
        end;
        // but do not continue
        continue:=false;
      end;
    deBreakpoint:
      begin
        continue:=false;
        if assigned(OnHitBreakpointEvent) then
          OnHitBreakpointEvent(continue, FCurrentProcess.CurrentBreakpoint);
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
        continue := false;
      end;
    deException:
      begin
        continue:=false;
        if assigned(OnExceptionEvent) then
          OnExceptionEvent(continue, FCurrentProcess.ExceptionClass, FCurrentProcess.ExceptionMessage );
      end;
    deLoadLibrary:
      begin
        continue:=true;
      end;
    deInternalContinue:
      begin
        continue := true;
      end;
    else
      raise exception.create('Unknown debug controler state');
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

