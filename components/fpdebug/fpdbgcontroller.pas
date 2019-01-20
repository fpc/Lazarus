unit FPDbgController;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Maps,
  LazLogger,
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
  public
    constructor Create(AController: TDbgController); virtual;
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); virtual; abstract;
    procedure ResolveEvent(var AnEvent: TFPDEvent; out Handled, Finished: boolean); virtual; abstract;
  end;

  { TDbgControllerContinueCmd }

  TDbgControllerContinueCmd = class(TDbgControllerCmd)
  public
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
    procedure ResolveEvent(var AnEvent: TFPDEvent; out Handled, Finished: boolean); override;
  end;

  { TDbgControllerStepIntoInstructionCmd }

  TDbgControllerStepIntoInstructionCmd = class(TDbgControllerCmd)
  public
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
    procedure ResolveEvent(var AnEvent: TFPDEvent; out Handled, Finished: boolean); override;
  end;

  { TDbgControllerStepOverInstructionCmd }

  TDbgControllerStepOverInstructionCmd = class(TDbgControllerCmd)
  private
    FHiddenBreakpoint: TFpInternalBreakpoint;
    FIsSet: boolean;
  public
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
    procedure ResolveEvent(var AnEvent: TFPDEvent; out Handled, Finished: boolean); override;
  end;

  { TDbgControllerStepOutInstructionCmd }

  TDbgControllerStepOutInstructionCmd = class(TDbgControllerCmd)
  private
    FHiddenBreakpoint: TFpInternalBreakpoint;
    FIsSet: boolean;
    FProcess: TDbgProcess;
    FThread: TDbgThread;
    FStepCount: Integer;
    FStepOut: Boolean;
  protected
    procedure SetReturnAdressBreakpoint(AProcess: TDbgProcess);
  public
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
    procedure ResolveEvent(var AnEvent: TFPDEvent; out Handled, Finished: boolean); override;
  end;

  { TDbgControllerStepOverLineCmd }

  TDbgControllerStepOverLineCmd = class(TDbgControllerStepOverInstructionCmd)
  private
    FInfoStored: boolean;
    FStoredStackFrame: TDBGPtr;
  public
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
    procedure ResolveEvent(var AnEvent: TFPDEvent; out Handled, Finished: boolean); override;
  end;

  { TDbgControllerStepIntoLineCmd }

  TDbgControllerStepIntoLineCmd = class(TDbgControllerCmd)
  private
    FInfoStored: boolean;
    FStoredStackFrame: TDBGPtr;
    FInto: boolean;
    FHiddenWatchpointInto: integer;
    FHiddenWatchpointOut: integer;
    FHiddenWatchpointOutStackbase: integer;
    FLastStackPointerValue: TDBGPtr;
    FLastStackBaseValue: TDBGPtr;
    FAssumedProcStartStackPointer: TDBGPtr;
    FHiddenBreakpoint: TFpInternalBreakpoint;
    FInstCount: integer;
  public
    constructor Create(AController: TDbgController); override;
    destructor Destroy; override;
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
    procedure ResolveEvent(var AnEvent: TFPDEvent; out Handled, Finished: boolean); override;
  end;

  { TDbgControllerRunToCmd }

  TDbgControllerRunToCmd = class(TDbgControllerCmd)
  private
    FHiddenBreakpoint: TFpInternalBreakpoint;
    FLocation: TDBGPtrArray;
    FProcess: TDbgProcess;
  public
    constructor Create(AController: TDbgController; ALocation: TDBGPtrArray);
    procedure DoContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
    procedure ResolveEvent(var AnEvent: TFPDEvent; out Handled, Finished: boolean); override;
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
    FOnLog: TOnLog;
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
    procedure SetOnLog(AValue: TOnLog);
    procedure DoOnDebugInfoLoaded(Sender: TObject);
    procedure SetParams(AValue: TStringList);
  protected
    FMainProcess: TDbgProcess;
    FCurrentProcess: TDbgProcess;
    FCurrentThread: TDbgThread;
    FCommand: TDbgControllerCmd;
    procedure Log(const AString: string; const ALogLevel: TFPDLogLevel = dllDebug);
    procedure Log(const AString: string; const Options: array of const; const ALogLevel: TFPDLogLevel = dllDebug);
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
    property OnLog: TOnLog read FOnLog write SetOnLog;
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

{ TDbgControllerStepOutInstructionCmd }

procedure TDbgControllerStepOutInstructionCmd.SetReturnAdressBreakpoint(AProcess: TDbgProcess);
var
  AStackPointerValue, StepOutStackPos, ReturnAddress: TDBGPtr;
begin
  AStackPointerValue:=FController.CurrentThread.GetStackBasePointerRegisterValue;
  StepOutStackPos:=AStackPointerValue+DBGPTRSIZE[FController.FCurrentProcess.Mode];

  if AProcess.ReadAddress(StepOutStackPos, ReturnAddress) then
  begin
    FProcess := AProcess;
    if not AProcess.HasBreak(ReturnAddress) then
      FHiddenBreakpoint := AProcess.AddBreak(ReturnAddress)
  end
  else
  begin
    AProcess.Log('Failed to read return-address from stack');
  end;

  FIsSet:=true;
end;

procedure TDbgControllerStepOutInstructionCmd.DoContinue(AProcess: TDbgProcess; AThread: TDbgThread);
var
  CodeBin: array[0..20] of byte;
  p: pointer;
  ADump,
  AStatement: string;
begin
  FThread := AThread;
  FProcess := AProcess;
  if FIsSet then
    // When a breanpoint has already been set on the return-adress, just continue
    AProcess.Continue(AProcess, AThread, false)
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
        AProcess.Continue(AProcess, AThread, False);
      end
      else if (copy(AStatement,1,3)='ret') then
      begin
        // Do one more single-step, and we're finished.
        FStepOut := True;
        AProcess.Continue(AProcess, AThread, True);
      end
      else
        AProcess.Continue(AProcess, AThread, True);
    end
    else
      AProcess.Continue(AProcess, AThread, True);
  end
  else
  begin
    // Enough with the single-stepping, set an hidden breakpoint at the return
    // address, and continue.
    SetReturnAdressBreakpoint(AProcess);
    AProcess.Continue(AProcess, AThread, False);
  end;
end;

procedure TDbgControllerStepOutInstructionCmd.ResolveEvent(var AnEvent: TFPDEvent; out Handled, Finished: boolean);
begin
  Handled := false;
  Finished := false;

  if FStepOut then
    // During single-stepping a 'ret' instruction was encountered. So we're just
    // finished.
    Finished := true
  else if FIsSet then
    Finished := not (AnEvent in [deInternalContinue, deLoadLibrary])
  else if (AnEvent in [deBreakpoint]) and not FProcess.HasBreak(FThread.GetInstructionPointerRegisterValue) then
    // Single-stepping, so continue silently.
    AnEvent := deInternalContinue;

  if Finished then
  begin
    AnEvent := deFinishedStep;
    if Assigned(FHiddenBreakpoint) then begin
      FProcess.RemoveBreak(FHiddenBreakpoint);
      FHiddenBreakpoint.Free;
    end;
  end;
end;

{ TDbgControllerRunToCmd }

constructor TDbgControllerRunToCmd.Create(AController: TDbgController; ALocation: TDBGPtrArray);
begin
  inherited create(AController);
  FLocation:=ALocation;
end;

procedure TDbgControllerRunToCmd.DoContinue(AProcess: TDbgProcess; AThread: TDbgThread);
begin
  FProcess := AProcess;
  if not assigned(FHiddenBreakpoint) then // and not AProcess.HasBreak(FLocation)
    FHiddenBreakpoint := AProcess.AddBreak(FLocation)
  else
    FProcess.Log('TDbgControllerRunToCmd.DoContinue: Breakpoint already used');

  AProcess.Continue(AProcess, AThread, False);
end;

procedure TDbgControllerRunToCmd.ResolveEvent(var AnEvent: TFPDEvent; out
  Handled, Finished: boolean);
begin
  Handled := false;
  Finished := (AnEvent<>deInternalContinue);
  if Finished and assigned(FHiddenBreakpoint) then
    begin
    FProcess.RemoveBreak(FHiddenBreakpoint);
    FHiddenBreakpoint.Free;
    end;
end;

{ TDbgControllerStepIntoLineCmd }

constructor TDbgControllerStepIntoLineCmd.Create(AController: TDbgController);
begin
  inherited Create(AController);
  FHiddenWatchpointInto:=-1;
  FHiddenWatchpointOut:=-1;
  FHiddenWatchpointOutStackbase:=-1;
end;

destructor TDbgControllerStepIntoLineCmd.Destroy;
begin
  if assigned(FHiddenBreakpoint) then
    begin
    FController.CurrentProcess.RemoveBreak(FHiddenBreakpoint);
    FreeAndNil(FHiddenBreakpoint);
    end;
  inherited Destroy;
end;

procedure TDbgControllerStepIntoLineCmd.DoContinue(AProcess: TDbgProcess; AThread: TDbgThread);

var
  CodeBin: array[0..20] of byte;
  p: pointer;
  ADump,
  AStatement: string;
  ALocation: TDBGPtr;

begin
  if not FInfoStored then
  begin
    FInfoStored:=true;
    FStoredStackFrame:=AThread.GetStackBasePointerRegisterValue;
    AThread.StoreStepInfo;
  end;

  if not FInto then
  begin
    if AProcess.ReadData(AThread.GetInstructionPointerRegisterValue,sizeof(CodeBin),CodeBin) then
    begin
      p := @CodeBin;
      Disassemble(p, AProcess.Mode=dm64, ADump, AStatement);
      if (copy(AStatement,1,4)='call') then
        begin
        FInto := true;
        FInstCount := 0;

        ALocation := AThread.GetInstructionPointerRegisterValue+(PtrUInt(p)-PtrUInt(@codebin));
        if not AProcess.HasBreak(ALocation) then
          FHiddenBreakpoint := AProcess.AddBreak(ALocation);

        AProcess.Continue(AProcess, AThread, true);
        exit;
        end;
    end;
  end;

  AProcess.Continue(AProcess, AThread, (FHiddenWatchpointInto=-1) and (FHiddenWatchpointOut=-1));
end;

procedure TDbgControllerStepIntoLineCmd.ResolveEvent(var AnEvent: TFPDEvent; out Handled, Finished: boolean);

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

  Handled := false;
  Finished := not (AnEvent in [deInternalContinue, deLoadLibrary]);
  if (AnEvent=deBreakpoint) and (not assigned(FController.FCurrentProcess.CurrentBreakpoint) or (FController.FCurrentProcess.CurrentBreakpoint=FHiddenBreakpoint)) then
  begin
    if FController.FCurrentThread.CompareStepInfo<>dcsiNewLine then
    begin
      AnEvent:=deInternalContinue;
      Finished:=false;
      inc(FInstCount);

      if FInto then
      begin
        if (FController.CurrentProcess.CurrentBreakpoint=FHiddenBreakpoint) then
        begin
          FInto:=false;
          FInstCount:=0;
          FController.CurrentProcess.RemoveBreak(FHiddenBreakpoint);
          FreeAndNil(FHiddenBreakpoint);
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
  FLastStackPointerValue:=AStackPointerValue;
  FLastStackBaseValue:=AStackBasePointerValue;
end;

{ TDbgControllerStepOverLineCmd }

procedure TDbgControllerStepOverLineCmd.DoContinue(AProcess: TDbgProcess; AThread: TDbgThread);
begin
  if not FInfoStored then
  begin
    FInfoStored:=true;
    AThread.StoreStepInfo;
    FStoredStackFrame:=AThread.GetStackBasePointerRegisterValue;
  end;

  inherited DoContinue(AProcess, AThread);
end;

procedure TDbgControllerStepOverLineCmd.ResolveEvent(var AnEvent: TFPDEvent; out Handled, Finished: boolean);
var
  EventCopy: TFPDEvent;
begin
  EventCopy := AnEvent;
  inherited ResolveEvent(EventCopy, Handled, Finished);
  // todo: check FHiddenBreakpoint.HasLocation();
  if (AnEvent=deBreakpoint) and not assigned(FController.CurrentProcess.CurrentBreakpoint)
  then
  begin
    if (FController.FCurrentThread.CompareStepInfo<>dcsiNewLine) or
      (not FController.FCurrentThread.IsAtStartOfLine and
       (FController.NextOnlyStopOnStartLine or (FStoredStackFrame < FController.CurrentThread.GetStackBasePointerRegisterValue))) then
    begin
      AnEvent:=deInternalContinue;
      FHiddenBreakpoint:=nil;
      FIsSet:=false;
      Finished:=false;
    end;
  end;
  if Finished then
    AnEvent := deFinishedStep;
end;


{ TDbgControllerStepOverInstructionCmd }

procedure TDbgControllerStepOverInstructionCmd.DoContinue(AProcess: TDbgProcess; AThread: TDbgThread);

var
  CodeBin: array[0..20] of byte;
  p: pointer;
  ADump,
  AStatement: string;
  CallInstr: boolean;
  ALocation: TDbgPtr;

begin
  if FIsSet then
    AProcess.Continue(AProcess, AThread, false)
  else
  begin
    CallInstr:=false;
    if AProcess.ReadData(AThread.GetInstructionPointerRegisterValue,sizeof(CodeBin),CodeBin) then
    begin
      p := @CodeBin;
      Disassemble(p, AProcess.Mode=dm64, ADump, AStatement);
      if copy(AStatement,1,4)='call' then
        CallInstr:=true;
    end;

    if CallInstr then
    begin
      ALocation := AThread.GetInstructionPointerRegisterValue+(PtrUInt(p)-PtrUInt(@codebin));
      if not AProcess.HasBreak(ALocation) then
        FHiddenBreakpoint := AProcess.AddBreak(ALocation);
    end;
    FIsSet:=true;
    AProcess.Continue(AProcess, AThread, not CallInstr);
  end;
end;

procedure TDbgControllerStepOverInstructionCmd.ResolveEvent(
  var AnEvent: TFPDEvent; out Handled, Finished: boolean);
begin
  Handled := false;
  Finished := not (AnEvent in [deInternalContinue, deLoadLibrary]);
  if Finished then
  begin
    AnEvent := deFinishedStep;
    if assigned(FHiddenBreakpoint) then
    begin
      FController.FCurrentProcess.RemoveBreak(FHiddenBreakpoint);
      FHiddenBreakpoint.Free;
    end;
  end;
end;

{ TDbgControllerStepIntoInstructionCmd }

procedure TDbgControllerStepIntoInstructionCmd.DoContinue(
  AProcess: TDbgProcess; AThread: TDbgThread);
begin
  AProcess.Continue(AProcess, AThread, True);
end;

procedure TDbgControllerStepIntoInstructionCmd.ResolveEvent(
  var AnEvent: TFPDEvent; out Handled, Finished: boolean);
begin
  Handled := false;
  Finished := (AnEvent<>deInternalContinue);
end;

{ TDbgControllerContinueCmd }

procedure TDbgControllerContinueCmd.DoContinue(AProcess: TDbgProcess; AThread: TDbgThread);
begin
  AProcess.Continue(AProcess, AThread, False);
end;

procedure TDbgControllerContinueCmd.ResolveEvent(var AnEvent: TFPDEvent; out
  Handled, Finished: boolean);
begin
  Handled := false;
  Finished := (AnEvent<>deInternalContinue);
end;

{ TDbgControllerCmd }

constructor TDbgControllerCmd.Create(AController: TDbgController);
begin
  FController := AController;
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
    debugln(['SetCurrentThread() unknown thread id: ', AValue]);
    // raise ...
    exit;
  end;
  FCurrentThread := ExistingThread;
end;

procedure TDbgController.SetOnLog(AValue: TOnLog);
begin
  if FOnLog=AValue then Exit;
  FOnLog:=AValue;
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
  {$ifdef DBG_FPDEBUG_VERBOSE}
  log('Initialized command '+ACommand.ClassName, dllDebug);
  {$endif DBG_FPDEBUG_VERBOSE}
  FCommand := ACommand;
end;

function TDbgController.Run: boolean;
var
  Flags: TStartInstanceFlags;
begin
  result := False;
  if assigned(FMainProcess) then
    begin
    Log('The debuggee is already running', dllInfo);
    Exit;
    end;

  if FExecutableFilename = '' then
    begin
    Log('No filename given to execute.', dllInfo);
    Exit;
    end;

  if not FileExists(FExecutableFilename) then
    begin
    Log('File %s does not exist.',[FExecutableFilename], dllInfo);
    Exit;
    end;

  Flags := [];
  if RedirectConsoleOutput then Include(Flags, siRediretOutput);
  if ForceNewConsoleWin then Include(Flags, siForceNewConsole);
  FCurrentProcess := OSDbgClasses.DbgProcessClass.StartInstance(FExecutableFilename, Params, Environment, WorkingDirectory, FConsoleTty , @Log, Flags);
  if assigned(FCurrentProcess) then
    begin
    FProcessMap.Add(FCurrentProcess.ProcessID, FCurrentProcess);
    Log('Got PID: %d, TID: %d', [FCurrentProcess.ProcessID, FCurrentProcess.ThreadID]);
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
  InitializeCommand(TDbgControllerStepOutInstructionCmd.Create(self));
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
        {$ifdef DBG_FPDEBUG_VERBOSE}
        log('Continue process without command.', dllDebug);
        {$endif DBG_FPDEBUG_VERBOSE}
        FCurrentProcess.Continue(FCurrentProcess, FCurrentThread, False)
        end
      else
        begin
        {$ifdef DBG_FPDEBUG_VERBOSE}
        log('Continue process with command '+FCommand.ClassName, dllDebug);
        {$endif DBG_FPDEBUG_VERBOSE}
        FCommand.DoContinue(FCurrentProcess, FCurrentThread);
        end;

        // TODO: replace the dangling pointer with the next best value....
        // There is still a race condition, for another thread to access it...
        if (ctid <> 0) and not FCurrentProcess.GetThread(ctid, DummyThread) then
          FCurrentThread := nil;
    end;
    if not FCurrentProcess.WaitForDebugEvent(AProcessIdentifier, AThreadIdentifier) then Continue;

    FCurrentProcess.RestoreTempBreakInstructionCodes;

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
    {$ifdef DBG_FPDEBUG_VERBOSE}
    log('Process stopped with event %s. IP=%s, SP=%s, BSP=%s.', [FPDEventNames[FPDEvent],
                                                                FCurrentProcess.FormatAddress(FCurrentThread.GetInstructionPointerRegisterValue),
                                                                FCurrentProcess.FormatAddress(FCurrentThread.GetStackPointerRegisterValue),
                                                                FCurrentProcess.FormatAddress(FCurrentThread.GetStackBasePointerRegisterValue)], dllDebug);
    {$endif DBG_FPDEBUG_VERBOSE}
    if assigned(FCommand) then
      begin
      FCommand.ResolveEvent(FPDEvent, IsHandled, IsFinished);
      {$ifdef DBG_FPDEBUG_VERBOSE}
      if IsFinished then
        log('Command %s is finished. (IsHandled=%s)', [FCommand.ClassName, BoolToStr(IsHandled)], dllDebug)
      else
        log('Command %s is not finished. (IsHandled=%s)', [FCommand.ClassName, BoolToStr(IsHandled)], dllDebug);
      {$endif DBG_FPDEBUG_VERBOSE}
      end
    else
    begin
      IsHandled:=false;
      IsFinished:=false;
    end;

    AExit:=true;
    if not IsHandled then
    begin
     case FPDEvent of
       deInternalContinue: AExit := False;
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
          Log('No Dwarf-debug information available. The debugger will not function properly. [CurrentProcess='+dbgsname(FCurrentProcess)+',DbgInfo='+dbgsname(FCurrentProcess.DbgInfo)+']',dllInfo);

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

procedure TDbgController.Log(const AString: string; const ALogLevel: TFPDLogLevel);
begin
  if assigned(FOnLog) then
    FOnLog(AString, ALogLevel)
  else
    DebugLn(AString);
end;

procedure TDbgController.Log(const AString: string;
  const Options: array of const; const ALogLevel: TFPDLogLevel);
begin
  Log(Format(AString, Options), ALogLevel);
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

end.

