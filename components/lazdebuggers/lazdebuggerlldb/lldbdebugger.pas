(*
  settings set target.output-path /tmp/out.txt
*)
unit LldbDebugger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, DbgIntfDebuggerBase, DbgIntfBaseTypes, LazLoggerBase,
  LazClasses, LazFileUtils, Maps, strutils, DebugProcess, LldbInstructions,
  LldbHelper;

type

  (*
   *  Commands
   *)

  TLldbDebugger = class;
  TLldbDebuggerCommand = class;

  { TLldbDebuggerCommandQueue }

  TLldbDebuggerCommandQueue = class(TRefCntObjList)
  private
    FDebugger: TLldbDebugger;
    FLockQueueRun: Integer;
    function Get(Index: Integer): TLldbDebuggerCommand;
    procedure Put(Index: Integer; const AValue: TLldbDebuggerCommand);
  private
    FRunningCommand: TLldbDebuggerCommand;
    procedure Run; // Call Debugger.OnIdle // set IsIdle
  protected
    procedure CommandFinished(ACommand: TLldbDebuggerCommand);
  public
    constructor Create(ADebugger: TLldbDebugger);
    destructor Destroy; override;
    procedure LockQueueRun;
    procedure UnLockQueueRun;
    property Items[Index: Integer]: TLldbDebuggerCommand read Get write Put; default;
    procedure QueueCommand(AValue: TLldbDebuggerCommand);
  end;

  { TLldbDebuggerCommand }

  TLldbDebuggerCommand = class(TRefCountedObject)
  private
    FOwner: TLldbDebugger;
    function GetDebuggerState: TDBGState;
    function GetCommandQueue: TLldbDebuggerCommandQueue;
    function GetInstructionQueue: TLldbInstructionQueue;
  protected
    procedure DoExecute; virtual; abstract;
    procedure Finished;

    procedure InstructionSucceeded(AnInstruction: TObject);
    procedure InstructionFailed(AnInstruction: TObject);

    procedure QueueInstruction(AnInstruction: TLldbInstruction);
    procedure SetDebuggerState(const AValue: TDBGState);
    property Debugger: TLldbDebugger read FOwner;
    property CommandQueue: TLldbDebuggerCommandQueue read GetCommandQueue;
    property InstructionQueue: TLldbInstructionQueue read GetInstructionQueue;
    property DebuggerState: TDBGState read GetDebuggerState;
  public
    constructor Create(AOwner: TLldbDebugger);
    procedure Execute;
  end;

  { TLldbDebuggerCommandInit }

  TLldbDebuggerCommandInit = class(TLldbDebuggerCommand)
  protected
    procedure DoExecute; override;
  end;

  { TLldbDebuggerCommandRun }

  TLldbDebuggerCommandRun = class(TLldbDebuggerCommand)
  private
    FRunInstr: TLldbInstruction;
    procedure TargetCreated(Sender: TObject);
  protected
    procedure DoExecute; override;
  end;

  { TLldbDebuggerCommandStop }

  TLldbDebuggerCommandStop = class(TLldbDebuggerCommand)
  private
    procedure StopInstructionSucceeded(Sender: TObject);
  protected
    procedure DoExecute; override;
  end;

  { TLldbDebuggerCommandEvaluate }

  TLldbDebuggerCommandEvaluate = class(TLldbDebuggerCommand)
  private
    FInstr: TLldbInstructionExpression;
    FWatchValue: TWatchValue;
    FExpr: String;
    FFlags: TDBGEvaluateFlags;
    FCallback: TDBGEvaluateResultCallback;
    procedure EvalInstructionFailed(Sender: TObject);
    procedure EvalInstructionSucceeded(Sender: TObject);
  protected
    procedure DoExecute; override;
  public
    // TODO: Pass FCurrentStackFrame to create
    constructor Create(AOwner: TLldbDebugger; AWatchValue: TWatchValue);
    constructor Create(AOwner: TLldbDebugger; AnExpr: String; AFlags: TDBGEvaluateFlags;
                       ACallback: TDBGEvaluateResultCallback);
  end;

  (*
   *  Debugger
   *)
  { TLldbDebugger }

  TLldbDebugger = class(TDebuggerIntf)
  private
    FDebugProcess: TDebugProcess;
    FDebugInstructionQueue: TLldbInstructionQueue;
    FCommandQueue: TLldbDebuggerCommandQueue;
    FCurrentLocation: TDBGLocationRec;
    FCurrentStackFrame: Integer;
    FCurrentThreadId: Integer;

    procedure DoAfterLineReceived(var ALine: String);
    procedure DoBeforeLineReceived(var ALine: String);
    procedure DoCmdLineDebuggerTerminated(Sender: TObject);
    procedure DoLineSentToDbg(Sender: TObject; ALine: String);
    function  LldbRun: Boolean;
    function  LldbStep(AStepAction: TLldbInstructionProcessStepAction): Boolean;
    function  LldbStop: Boolean;
    function  LldbEvaluate(const AExpression: String; EvalFlags: TDBGEvaluateFlags; ACallback: TDBGEvaluateResultCallback): Boolean;
  protected
    procedure DoBeginReceivingLines(Sender: TObject);
    procedure DoEndReceivingLines(Sender: TObject);
    procedure LockRelease; override;
    procedure UnlockRelease; override;
    procedure QueueCommand(const ACommand: TLldbDebuggerCommand);
    procedure SetState(const AValue: TDBGState);
    //procedure DoState(const OldState: TDBGState); override;
    //procedure DoBeforeState(const OldState: TDBGState); override;
    property CurrentThreadId: Integer read FCurrentThreadId;
    property CurrentStackFrame: Integer read FCurrentStackFrame;
    property CurrentLocation: TDBGLocationRec read FCurrentLocation;
    property DebugInstructionQueue: TLldbInstructionQueue read FDebugInstructionQueue;
    property CommandQueue: TLldbDebuggerCommandQueue read FCommandQueue;
  protected
    function  CreateBreakPoints: TDBGBreakPoints; override;
    //function  CreateLocals: TLocalsSupplier; override;
    //function  CreateLineInfo: TDBGLineInfo; override;
    function  CreateRegisters: TRegisterSupplier; override;
    function  CreateCallStack: TCallStackSupplier; override;
    //function  CreateDisassembler: TDBGDisassembler; override;
    function  CreateWatches: TWatchesSupplier; override;
    function  CreateThreads: TThreadsSupplier; override;

    function  GetSupportedCommands: TDBGCommands; override;
    //function  GetCommands: TDBGCommands; override;
    function  RequestCommand(const ACommand: TDBGCommand;
              const AParams: array of const;
              const ACallback: TMethod): Boolean; override;
  public
//    class function CreateProperties: TDebuggerProperties; override; // Creates debuggerproperties
    class function Caption: String; override;
//    class function ExePaths: String; override;

    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;
    procedure Init; override;         // Initializes external debugger
    procedure Done; override;         // Kills external debugger

    function GetLocation: TDBGLocationRec; override;
//    function GetProcessList({%H-}AList: TRunningProcessInfoList): boolean; override;
//    function NeedReset: Boolean; override;
    procedure TestCmd(const ACommand: String); override;
  end;


procedure Register;

implementation

type

  {%region
    *****
    *****     Threads
    ***** }

  { TLldbDebuggerCommandThreads }

  TLldbDebuggerCommandThreads = class(TLldbDebuggerCommand)
  private
    FCurrentThreads: TThreads;
    procedure ThreadInstructionSucceeded(Sender: TObject);
    //procedure StopInstructionSucceeded(Sender: TObject);
  protected
    procedure DoExecute; override;
  public
    property  CurrentThreads: TThreads read FCurrentThreads write FCurrentThreads;
  end;

  { TLldbThreads }

  TLldbThreads = class(TThreadsSupplier)
  private
  protected
    procedure DoStateEnterPause; override;
  public
    procedure RequestMasterData; override;
    procedure ChangeCurrentThread(ANewId: Integer); override;
  end;

  {%endregion   ^^^^^  Threads  ^^^^^   }

  {%region
    *****
    *****     CallStack
    ***** }

  { TLldbDebuggerCommandCallStack }

  TLldbDebuggerCommandCallStack = class(TLldbDebuggerCommand)
  private
    FCurrentCallStack: TCallStackBase;
    procedure DoCallstackFreed(Sender: TObject);
    procedure StackInstructionFinished(Sender: TObject);
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TLldbDebugger; ACurrentCallStack: TCallStackBase);
    destructor Destroy; override;
    property  CurrentCallStack: TCallStackBase read FCurrentCallStack;
  end;

  { TLldbCallStack }

  TLldbCallStack = class(TCallStackSupplier)
  protected
    //procedure Clear;
    //procedure DoThreadChanged;
    procedure ParentRequestEntries(ACallstack: TCallStackBase);
  public
    procedure RequestAtLeastCount(ACallstack: TCallStackBase;
      ARequiredMinCount: Integer); override;
    procedure UpdateCurrentIndex; override;
    procedure RequestCurrent(ACallstack: TCallStackBase); override;
    procedure RequestEntries(ACallstack: TCallStackBase); override;
  end;

  {%endregion   ^^^^^  CallStack  ^^^^^   }

  {%region
    *****
    *****     Watches
    ***** }

  { TLldbWatches }

  TLldbWatches = class(TWatchesSupplier)
  private
  protected
    procedure InternalRequestData(AWatchValue: TWatchValue); override;
  public
  end;

  {%endregion   ^^^^^  Watches  ^^^^^   }

  {%region
    *****
    *****     BreakPoint
    ***** }

  { TLldbBreakPoint }

  TLldbBreakPoint = class(TDBGBreakPoint)
  private
    FBreakID: Integer;
    procedure BreakInstructionFinished(Sender: TObject);
    procedure SetBreakPoint;
    procedure ReleaseBreakPoint;
  protected
    procedure DoStateChange(const AOldState: TDBGState); override;
  public
//    constructor Create(ACollection: TCollection); override;
//    destructor Destroy; override;
//    procedure DoLogExpression(const AnExpression: String); override;
    procedure SetLocation(const ASource: String; const ALine: Integer); override;
//    procedure SetWatch(const AData: String; const AScope: TDBGWatchPointScope;
//                       const AKind: TDBGWatchPointKind); override;
  end;

  TLldbBreakPoints = class(TDBGBreakPoints)
  protected
//    function FindById(AnId: Integer): TGDBMIBreakPoint;
  end;

  {%endregion   ^^^^^  BreakPoint  ^^^^^   }

  {%region
    *****
    *****     Register
    ***** }

  { TLldbDebuggerCommandRegister }

  TLldbDebuggerCommandRegister = class(TLldbDebuggerCommand)
  private
    FRegisters: TRegisters;
    procedure RegisterInstructionFinished(Sender: TObject);
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TLldbDebugger; ARegisters: TRegisters);
    destructor Destroy; override;
    property Registers: TRegisters read FRegisters;
  end;

  { TLldbRegisterSupplier }

  TLldbRegisterSupplier = class(TRegisterSupplier)
  public
    procedure Changed;
    procedure RequestData(ARegisters: TRegisters); override;
  end;

  {%endregion   ^^^^^  Register  ^^^^^   }

{%region
  *****
  *****     Threads
  ***** }

{ TLldbDebuggerCommandThreads }

procedure TLldbDebuggerCommandThreads.ThreadInstructionSucceeded(Sender: TObject
  );
var
  Instr: TLldbInstructionThreadList absolute Sender;
  i, j, line: Integer;
  s, func, filename, name, d: String;
  found, foundFunc, foundArg: TStringArray;
  TId, CurThrId: LongInt;
  CurThr: Boolean;
  Arguments: TStringList;
  addr: TDBGPtr;
  te: TThreadEntry;
begin
  CurrentThreads.Clear;
  for i := 0 to Length(Instr.Res) - 1 do begin
    s := Instr.Res[i];
    ParseThreadLocation(s, TId, CurThr, name, addr, func, Arguments, filename, line, d);
    if CurThr then
      CurThrId := TId;

    te := CurrentThreads.CreateEntry(
      addr,
      Arguments,
      func,
      filename, '',
      line,
      TId, name, ''
    );
    CurrentThreads.Add(te);
    te.Free;

    Arguments.Free;
  end;

  CurrentThreads.CurrentThreadId := CurThrId;
  CurrentThreads.SetValidity(ddsValid);

  Finished;
end;

procedure TLldbDebuggerCommandThreads.DoExecute;
var
  Instr: TLldbInstructionThreadList;
begin
  Instr := TLldbInstructionThreadList.Create();
  Instr.OnFinish := @ThreadInstructionSucceeded;
  InstructionQueue.QueueInstruction(Instr);
  Instr.ReleaseReference;
end;

{ TLldbThreads }

procedure TLldbThreads.DoStateEnterPause;
begin
  inherited DoStateEnterPause;
  Changed;
end;

procedure TLldbThreads.RequestMasterData;
var
  Cmd: TLldbDebuggerCommandThreads;
begin
  if not (Debugger.State in [dsPause, dsInternalPause]) then
    exit;

  Cmd := TLldbDebuggerCommandThreads.Create(TLldbDebugger(Debugger));
  Cmd.CurrentThreads := CurrentThreads;
  TLldbDebugger(Debugger).QueueCommand(Cmd);
  Cmd.ReleaseReference;
end;

procedure TLldbThreads.ChangeCurrentThread(ANewId: Integer);
begin
  if Debugger = nil then Exit;
  if not(Debugger.State in [dsPause, dsInternalPause]) then exit;

  TLldbDebugger(Debugger).FCurrentThreadId := ANewId;

  if CurrentThreads <> nil
  then CurrentThreads.CurrentThreadId := ANewId;
end;

{%endregion   ^^^^^  Threads  ^^^^^   }

{%region
  *****
  *****     CallStack
  ***** }

{ TLldbDebuggerCommandCallStack }

procedure TLldbDebuggerCommandCallStack.StackInstructionFinished(Sender: TObject
  );
var
  Instr: TLldbInstructionStackTrace absolute Sender;
  i, FId, line: Integer;
  e: TCallStackEntry;
  found, foundArg: TStringArray;
  Arguments: TStringList;
  It: TMapIterator;
  s, func, filename, d, fullfile: String;
  frame: LongInt;
  IsCur: Boolean;
  addr: TDBGPtr;
begin
  if FCurrentCallStack = nil then begin
    Finished;
    exit;
  end;

  It := TMapIterator.Create(FCurrentCallStack.RawEntries);

  for i := 0 to Length(Instr.Res) - 1 do begin
    s := Instr.Res[i];
    ParseNewFrameLocation(s, FId, IsCur, addr, func, Arguments, filename, fullfile, line, d);
    if It.Locate(FId) then begin
      e := TCallStackEntry(It.DataPtr^);
      e.Init(addr, Arguments, func, filename, fullfile, line);
    end;
    Arguments.Free;
  end;
  It.Free;

  TLldbCallStack(Debugger.CallStack).ParentRequestEntries(FCurrentCallStack);

  Finished;
end;

procedure TLldbDebuggerCommandCallStack.DoCallstackFreed(Sender: TObject);
begin
  FCurrentCallStack := nil;
  //cancel
end;

procedure TLldbDebuggerCommandCallStack.DoExecute;
var
  StartIdx, EndIdx: Integer;
  Instr: TLldbInstructionStackTrace;
begin
  if FCurrentCallStack = nil then begin
    Finished;
    exit;
  end;

  StartIdx := Max(FCurrentCallStack.LowestUnknown, 0);
  EndIdx   := FCurrentCallStack.HighestUnknown;
  if EndIdx < StartIdx then begin
    Finished;
    exit;
  end;

  Instr := TLldbInstructionStackTrace.Create(EndIdx+1, FCurrentCallStack.ThreadId);
  Instr.OnFinish := @StackInstructionFinished;
  QueueInstruction(Instr);
  Instr.ReleaseReference;
end;

constructor TLldbDebuggerCommandCallStack.Create(AOwner: TLldbDebugger;
  ACurrentCallStack: TCallStackBase);
begin
  inherited Create(AOwner);
  FCurrentCallStack := ACurrentCallStack;
  FCurrentCallStack.AddFreeNotification(@DoCallstackFreed);
end;

destructor TLldbDebuggerCommandCallStack.Destroy;
begin
  FCurrentCallStack.RemoveFreeeNotification(@DoCallstackFreed);
  inherited Destroy;
end;

{ TLldbCallStack }

procedure TLldbCallStack.ParentRequestEntries(ACallstack: TCallStackBase);
begin
  inherited RequestEntries(ACallstack);
end;

procedure TLldbCallStack.RequestAtLeastCount(ACallstack: TCallStackBase;
  ARequiredMinCount: Integer);
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then begin
    ACallstack.SetCurrentValidity(ddsInvalid);
    Exit;
  end;

  ACallstack.Count := ARequiredMinCount + 1; // TODO: get data, and return correct result
  ACallstack.SetCountValidity(ddsValid);
end;

procedure TLldbCallStack.UpdateCurrentIndex;
var
  tid, idx: Integer;
  cs: TCallStackBase;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then begin
    exit;
  end;

  tid := Debugger.Threads.CurrentThreads.CurrentThreadId; // FCurrentThreadId ?
  cs := TCallStackBase(CurrentCallStackList.EntriesForThreads[tid]);
  idx := cs.NewCurrentIndex;  // NEW-CURRENT

  if TLldbDebugger(Debugger).FCurrentStackFrame = idx then Exit;

  TLldbDebugger(Debugger).FCurrentStackFrame := idx;

  if cs <> nil then begin
    cs.CurrentIndex := idx;
    cs.SetCurrentValidity(ddsValid);
  end;
end;

procedure TLldbCallStack.RequestCurrent(ACallstack: TCallStackBase);
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then begin
    ACallstack.SetCurrentValidity(ddsInvalid);
    Exit;
  end;

  ACallstack.CurrentIndex := 0; // will be used, if thread is changed
  ACallstack.SetCurrentValidity(ddsValid);
end;

procedure TLldbCallStack.RequestEntries(ACallstack: TCallStackBase);
var
  Cmd: TLldbDebuggerCommandCallStack;
begin
  if not (Debugger.State in [dsPause, dsInternalPause]) then
    exit;

  Cmd := TLldbDebuggerCommandCallStack.Create(TLldbDebugger(Debugger), ACallstack);
  TLldbDebugger(Debugger).QueueCommand(Cmd);
  Cmd.ReleaseReference;
end;

{%endregion   ^^^^^  CallStack  ^^^^^   }

{ TLldbWatches }

procedure TLldbWatches.InternalRequestData(AWatchValue: TWatchValue);
var
  Cmd: TLldbDebuggerCommandEvaluate;
begin
  Cmd := TLldbDebuggerCommandEvaluate.Create(TLldbDebugger(Debugger), AWatchValue);
  TLldbDebugger(Debugger).QueueCommand(Cmd);
  Cmd.ReleaseReference;
end;

{ TLldbBreakPoint }

procedure TLldbBreakPoint.SetBreakPoint;
var
  i: Integer;
  s: String;
  Instr: TLldbInstructionBreakSet;
begin
debugln(['TLldbBreakPoint.SetBreakPoint ']);
  i := LastPos(PathDelim, Source);
  if i > 0 then
    s := Copy(Source, i+1, Length(Source))
  else
    s := Source;
  Instr := TLldbInstructionBreakSet.Create(s, Line);
  Instr.OnFinish := @BreakInstructionFinished;
//  TLldbDebugger(Debugger).QueueCommand();
  TLldbDebugger(Debugger).FDebugInstructionQueue.QueueInstruction(Instr);
  Instr.ReleaseReference;
end;

procedure TLldbBreakPoint.BreakInstructionFinished(Sender: TObject);
begin
  if TLldbInstructionBreakSet(Sender).IsSuccess then begin
    FBreakID := TLldbInstructionBreakSet(Sender).BreakId;
    SetValid(TLldbInstructionBreakSet(Sender).State);
  end
  else
    SetValid(vsInvalid);
end;

procedure TLldbBreakPoint.ReleaseBreakPoint;
var
  Instr: TLldbInstructionBreakDelete;
begin
  SetHitCount(0);
  if FBreakID <= 0 then exit;

  Instr := TLldbInstructionBreakDelete.Create(FBreakID);
//  Instr.OnFinish := @BreakInstructionFinished;
  TLldbDebugger(Debugger).FDebugInstructionQueue.QueueInstruction(Instr);
  Instr.ReleaseReference;
end;

procedure TLldbBreakPoint.DoStateChange(const AOldState: TDBGState);
begin
  inherited DoStateChange(AOldState);
  case Debugger.State of
    dsRun: if AOldState = dsInit then begin
      // Disabled data breakpoints: wait until enabled
      // Disabled other breakpoints: Give to LLDB to see if they are valid
      if (Kind <> bpkData) or Enabled then
        SetBreakpoint;
    end;
    dsStop: begin
      if FBreakID > 0
      then ReleaseBreakpoint;
    end;
  end;
end;

procedure TLldbBreakPoint.SetLocation(const ASource: String;
  const ALine: Integer);
begin
  inherited SetLocation(ASource, ALine);
  if Debugger.State in [dsPause, dsInternalPause, dsRun] then
    SetBreakPoint;
end;

  {%region
    *****
    *****     Register
    ***** }

{ TLldbDebuggerCommandRegister }

procedure TLldbDebuggerCommandRegister.RegisterInstructionFinished(
  Sender: TObject);
var
  Instr: TLldbInstructionRegister absolute Sender;
  RegVal: TRegisterValue;
  n: String;
  i: Integer;
begin
  if not Instr.IsSuccess then begin
    if FRegisters.DataValidity in [ddsRequested, ddsEvaluating] then
      FRegisters.DataValidity := ddsInvalid;
    exit;
  end;

  FRegisters.DataValidity := ddsEvaluating;

  for i := 0 to Instr.Res.Count - 1 do begin
    n := Instr.Res.Names[i];
    RegVal := FRegisters.EntriesByName[n];
    RegVal.Value := Instr.Res.Values[n];
    RegVal.DataValidity := ddsValid;
  end;

  FRegisters.DataValidity := ddsValid;
  Finished;
end;

procedure TLldbDebuggerCommandRegister.DoExecute;
var
  Instr: TLldbInstructionRegister;
begin
  // TODO: store thread/frame when command is created
  Instr := TLldbInstructionRegister.Create(Debugger.FCurrentThreadId, Debugger.FCurrentStackFrame);
  Instr.OnFinish := @RegisterInstructionFinished;
  QueueInstruction(Instr);
  Instr.ReleaseReference;
end;

constructor TLldbDebuggerCommandRegister.Create(AOwner: TLldbDebugger;
  ARegisters: TRegisters);
begin
  FRegisters := ARegisters;
  FRegisters.AddReference;
  inherited Create(AOwner);
end;

destructor TLldbDebuggerCommandRegister.Destroy;
begin
  ReleaseRefAndNil(FRegisters);
  inherited Destroy;
end;

{ TLldbRegisterSupplier }

procedure TLldbRegisterSupplier.Changed;
begin
  if CurrentRegistersList <> nil
  then CurrentRegistersList.Clear;
end;

procedure TLldbRegisterSupplier.RequestData(ARegisters: TRegisters);
var
  Cmd: TLldbDebuggerCommandRegister;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsStop]) then
    exit;

  Cmd := TLldbDebuggerCommandRegister.Create(TLldbDebugger(Debugger), ARegisters);
  TLldbDebugger(Debugger).QueueCommand(Cmd);
  Cmd.ReleaseReference;
end;

  {%endregion   ^^^^^  Register  ^^^^^   }

{ TLldbDebuggerCommandQueue }

function TLldbDebuggerCommandQueue.Get(Index: Integer): TLldbDebuggerCommand;
begin
  Result := TLldbDebuggerCommand(inherited Get(Index));
end;

procedure TLldbDebuggerCommandQueue.Put(Index: Integer;
  const AValue: TLldbDebuggerCommand);
begin
  inherited Put(Index, AValue);
end;

procedure TLldbDebuggerCommandQueue.QueueCommand(AValue: TLldbDebuggerCommand);
begin
debugln(['CommandQueue.QueueCommand ', AValue.ClassName]);
  Insert(Count, AValue);
  Run;
end;

procedure TLldbDebuggerCommandQueue.Run;
begin
  if (FRunningCommand <> nil) or (FLockQueueRun > 0) then
    exit;
  if Count = 0 then
    exit;

  FRunningCommand := Items[0];
  FRunningCommand.AddReference;
  Delete(0);
DebugLnEnter(['||||||||>>> CommandQueue.Run ', FRunningCommand.ClassName, ', ', dbgs(fDebugger.State)]);
  FRunningCommand.Execute;
  // debugger and queue may get destroyed at the end of execute
end;

procedure TLldbDebuggerCommandQueue.CommandFinished(
  ACommand: TLldbDebuggerCommand);
begin
  if FRunningCommand = ACommand then begin
DebugLnExit(['||||||||<<< CommandQueue.Run ', FRunningCommand.ClassName, ', ', dbgs(fDebugger.State)]);
    ReleaseRefAndNil(FRunningCommand);
  end//;
else DebugLn('|||||||| TLldbDebuggerCommandQueue.CommandFinished >> unknown ???');
  if not(FDebugger.State in [dsError, dsDestroying, dsNone]) then
    Run;
end;

constructor TLldbDebuggerCommandQueue.Create(ADebugger: TLldbDebugger);
begin
  FDebugger := ADebugger;
  inherited Create;
end;

destructor TLldbDebuggerCommandQueue.Destroy;
begin
  while Count > 0 do
    Delete(0);
  if FRunningCommand <> nil then begin
DebugLnExit(['<<< CommandQueue.Run (Destroy)', FRunningCommand.ClassName, ', ', fDebugger.State]);
    ReleaseRefAndNil(FRunningCommand);
  end;
  inherited Destroy;
end;

procedure TLldbDebuggerCommandQueue.LockQueueRun;
begin
  inc(FLockQueueRun);
  debugln(['TLldbDebuggerCommandQueue.LockQueueRun ',FLockQueueRun]);
end;

procedure TLldbDebuggerCommandQueue.UnLockQueueRun;
begin
  debugln(['TLldbDebuggerCommandQueue.UnLockQueueRun ',FLockQueueRun]);
  dec(FLockQueueRun);
  if FLockQueueRun = 0 then Run;
end;

{ TLldbDebuggerCommand }

function TLldbDebuggerCommand.GetDebuggerState: TDBGState;
begin
  Result := Debugger.State;
end;

procedure TLldbDebuggerCommand.InstructionSucceeded(AnInstruction: TObject);
begin
  Finished;
end;

procedure TLldbDebuggerCommand.InstructionFailed(AnInstruction: TObject);
begin
  SetDebuggerState(dsError);
  Finished;
end;

procedure TLldbDebuggerCommand.Finished;
begin
  InstructionQueue.CancelAllForCommand(Self); // in case there still are any
  CommandQueue.CommandFinished(Self);
end;

function TLldbDebuggerCommand.GetCommandQueue: TLldbDebuggerCommandQueue;
begin
  Result := Debugger.FCommandQueue;
end;

function TLldbDebuggerCommand.GetInstructionQueue: TLldbInstructionQueue;
begin
  Result := Debugger.FDebugInstructionQueue;
end;

procedure TLldbDebuggerCommand.QueueInstruction(AnInstruction: TLldbInstruction);
begin
  AnInstruction.OwningCommand := Self;
  InstructionQueue.QueueInstruction(AnInstruction);
end;

procedure TLldbDebuggerCommand.SetDebuggerState(const AValue: TDBGState);
begin
  Debugger.SetState(AValue);
end;

constructor TLldbDebuggerCommand.Create(AOwner: TLldbDebugger);
begin
  FOwner := AOwner;
  inherited Create;
  AddReference;
end;

procedure TLldbDebuggerCommand.Execute;
var
  d: TLldbDebugger;
begin
  d := Debugger;
  try
    d.LockRelease;
    DoExecute;  // may call Finished and Destroy Self
  finally
    d.UnlockRelease;
  end;
end;

{ TLldbDebuggerCommandInit }

procedure TLldbDebuggerCommandInit.DoExecute;
var
  Instr: TLldbInstruction;
begin
  Instr := TLldbInstructionSettingSet.Create('frame-format',
    '"frame #${frame.index}: ${frame.pc}' +
    ' &&//FULL: {${line.file.fullpath}} &&//SHORT: {${line.file.basename}} &&//LINE: {${line.number}}' +
    ' &&//MOD: {${module.file.basename}} &&//FUNC: {${function.name-with-args}}' +
    ' <<&&//FRAME\n"'
//    ' {  ${frame.fp} }  \n"'
  );
  QueueInstruction(Instr);
  Instr.ReleaseReference;

  Instr := TLldbInstructionSettingSet.Create('stop-line-count-after', '0');
  QueueInstruction(Instr);
  Instr.ReleaseReference;

  Instr := TLldbInstructionSettingSet.Create('stop-line-count-before', '0');
  QueueInstruction(Instr);
  Instr.ReleaseReference;

  Instr := TLldbInstructionSettingSet.Create('stop-disassembly-count', '0');
  //Instr.OnFinish := @InstructionSucceeded;
  QueueInstruction(Instr);
  Instr.ReleaseReference;

  Instr := TLldbInstructionBreakSet.Create('fpc_raiseexception');
  Instr.OnFinish := @InstructionSucceeded;
  QueueInstruction(Instr);
  Instr.ReleaseReference;
end;

{ TLldbDebuggerCommandRun }

procedure TLldbDebuggerCommandRun.TargetCreated(Sender: TObject);
begin
  SetDebuggerState(dsRun);
  // the state change allows breakpoints to be set, before the run command is issued.

  FRunInstr := TLldbInstructionProcessLaunch.Create();
  FRunInstr.OnSuccess := @InstructionSucceeded;
  FRunInstr.OnFailure := @InstructionFailed;
  QueueInstruction(FRunInstr);
  FRunInstr.ReleaseReference;
end;

procedure TLldbDebuggerCommandRun.DoExecute;
var
  Instr: TLldbInstruction;
begin
  Instr := TLldbInstructionTargetCreate.Create(Debugger.FileName);
  Instr.OnSuccess := @TargetCreated;
  Instr.OnFailure := @InstructionFailed;
  QueueInstruction(Instr);
  Instr.ReleaseReference;
end;

{ TLldbDebuggerCommandStop }

procedure TLldbDebuggerCommandStop.StopInstructionSucceeded(Sender: TObject);
begin
  if DebuggerState <> dsIdle then
    SetDebuggerState(dsStop);
end;

procedure TLldbDebuggerCommandStop.DoExecute;
var
  Instr: TLldbInstruction;
begin
  Instr := TLldbInstructionProcessKill.Create();
  Instr.OnSuccess := @StopInstructionSucceeded;
  Instr.OnFailure := @InstructionFailed;
  QueueInstruction(Instr);
  Instr.ReleaseReference;

  Instr := TLldbInstructionTargetDelete.Create();
  Instr.OnFailure := @InstructionFailed;
  QueueInstruction(Instr);
  Instr.ReleaseReference;

  Instr := TLldbInstructionTargetDelete.Create();
  Instr.OnSuccess := @InstructionSucceeded;
  Instr.OnFailure := @InstructionFailed;
  QueueInstruction(Instr);
  Instr.ReleaseReference;
end;

{ TLldbDebuggerCommandEvaluate }

procedure TLldbDebuggerCommandEvaluate.EvalInstructionSucceeded(Sender: TObject
  );
begin
  if FWatchValue <> nil then begin
    FWatchValue.Value := FInstr.Res;
    //FWatchValue.TypeInfo := TypeInfo;
    FWatchValue.Validity := ddsValid;
  end
  else
  if FCallback <> nil then
    FCallback(Debugger, True, FInstr.Res, nil);

  FInstr.ReleaseReference;
  Finished;
end;

procedure TLldbDebuggerCommandEvaluate.EvalInstructionFailed(Sender: TObject);
begin
  if FWatchValue <> nil then
    FWatchValue.Validity := ddsError
  else
  if FCallback <> nil then
    FCallback(Debugger, False, '', nil);
  FInstr.ReleaseReference;
  Finished;
end;

procedure TLldbDebuggerCommandEvaluate.DoExecute;
begin
  if FWatchValue <> nil then
    FInstr := TLldbInstructionExpression.Create(FWatchValue.Expression, Debugger.FCurrentThreadId, Debugger.FCurrentStackFrame)
  else
    FInstr := TLldbInstructionExpression.Create(FExpr, Debugger.FCurrentThreadId, Debugger.FCurrentStackFrame);
  FInstr.OnSuccess := @EvalInstructionSucceeded;
  FInstr.OnFailure := @EvalInstructionFailed;
  QueueInstruction(FInstr);
end;

constructor TLldbDebuggerCommandEvaluate.Create(AOwner: TLldbDebugger;
  AWatchValue: TWatchValue);
begin
  FWatchValue := AWatchValue;
  inherited Create(AOwner);
end;

constructor TLldbDebuggerCommandEvaluate.Create(AOwner: TLldbDebugger;
  AnExpr: String; AFlags: TDBGEvaluateFlags;
  ACallback: TDBGEvaluateResultCallback);
begin
  FExpr := AnExpr;
  FFlags := AFlags;
  FCallback := ACallback;
  inherited Create(AOwner);
end;

{ TLldbDebugger }

function TLldbDebugger.LldbRun: Boolean;
var
  Cmd: TLldbDebuggerCommandRun;
begin
  DebugLn('*** Run');
  Result := True;

  if State in [dsPause, dsInternalPause] then begin
    LldbStep(saContinue);
    exit;
  end;

  if State in [dsNone, dsIdle, dsStop] then
    SetState(dsInit);

  Cmd := TLldbDebuggerCommandRun.Create(Self);
  QueueCommand(Cmd);
  Cmd.ReleaseReference;
end;

procedure TLldbDebugger.DoAfterLineReceived(var ALine: String);
var
  Instr: TLldbInstructionTargetDelete;
  found: TStringArray;
  AnId, SrcLine: Integer;
  AnIsCurrent: Boolean;
  AnAddr: TDBGPtr;
  AFuncName, AFile, AReminder, AFullFile: String;
  AnArgs: TStringList;
begin
  if ALine = '' then
    exit;

  if StrMatches(ALine, ['* thread #', ', stop reason = ', ''], found) then begin
    FCurrentThreadId := StrToIntDef(found[0], 0);
    FCurrentStackFrame := 0;
    FDebugInstructionQueue.SetKnownThreadAndFrame(FCurrentThreadId, 0);
    Threads.CurrentThreads.CurrentThreadId := FCurrentThreadId;
    SetState(dsPause);
    ALine := '';
  end;

  // Process 8888 exited with status = 0 (0x00000000)
  if (LeftStr(ALine, 8) = 'Process ') and (pos('exited with status = ', ALine) > 0) then begin
// todo: target delete
    if State <> dsIdle then
      SetState(dsStop);
    ALine := '';

    Instr := TLldbInstructionTargetDelete.Create();
    FDebugInstructionQueue.QueueInstruction(Instr);
    Instr.ReleaseReference;
  end;

  if ParseNewFrameLocation(ALine, AnId, AnIsCurrent, AnAddr, AFuncName, AnArgs,
    AFile, AFullFile, SrcLine, AReminder)
  then begin
    AnArgs.Free;
    FCurrentLocation.Address := AnAddr;
    FCurrentLocation.FuncName := AFuncName;
    FCurrentLocation.SrcFile := AFile;
    FCurrentLocation.SrcFullName := AFullFile;
    FCurrentLocation.SrcLine := SrcLine;
    DoCurrent(FCurrentLocation);
    ALine := '';
  end;
end;

procedure TLldbDebugger.DoBeforeLineReceived(var ALine: String);
begin
  DoDbgOutput(ALine);
end;

procedure TLldbDebugger.DoBeginReceivingLines(Sender: TObject);
begin
  LockRelease;
end;

procedure TLldbDebugger.DoCmdLineDebuggerTerminated(Sender: TObject);
begin
  SetState(dsError);
end;

procedure TLldbDebugger.DoLineSentToDbg(Sender: TObject; ALine: String);
begin
  DoDbgOutput('>> '+ALine);
end;

procedure TLldbDebugger.DoEndReceivingLines(Sender: TObject);
begin
  UnlockRelease;
end;

function TLldbDebugger.LldbStep(AStepAction: TLldbInstructionProcessStepAction
  ): Boolean;
var
  Instr: TLldbInstructionProcessStep;
begin
  // TODO
  Result := True;
  Instr := TLldbInstructionProcessStep.Create(AStepAction);
  FDebugInstructionQueue.QueueInstruction(Instr);
  Instr.ReleaseReference;
  SetState(dsRun);
end;

function TLldbDebugger.LldbStop: Boolean;
var
  Cmd: TLldbDebuggerCommandStop;
begin
  DebugLn('*** Stop');
  Result := True;

  Cmd := TLldbDebuggerCommandStop.Create(Self);
  QueueCommand(Cmd);
  Cmd.ReleaseReference;
end;

function TLldbDebugger.LldbEvaluate(const AExpression: String;
  EvalFlags: TDBGEvaluateFlags; ACallback: TDBGEvaluateResultCallback): Boolean;
var
  Cmd: TLldbDebuggerCommandEvaluate;
begin
  Cmd := TLldbDebuggerCommandEvaluate.Create(Self, AExpression, EvalFlags, ACallback);
  QueueCommand(Cmd);
  Cmd.ReleaseReference;
  Result := True;
end;

procedure TLldbDebugger.LockRelease;
begin
  inherited LockRelease;
end;

procedure TLldbDebugger.UnlockRelease;
begin
  inherited UnlockRelease;
end;

procedure TLldbDebugger.QueueCommand(const ACommand: TLldbDebuggerCommand);
begin
  FCommandQueue.QueueCommand(ACommand);
end;

procedure TLldbDebugger.SetState(const AValue: TDBGState);
begin
  inherited;
end;

function TLldbDebugger.CreateBreakPoints: TDBGBreakPoints;
begin
  Result := TLldbBreakPoints.Create(Self, TLldbBreakPoint);
end;

function TLldbDebugger.CreateRegisters: TRegisterSupplier;
begin
  Result := TLldbRegisterSupplier.Create(Self);
end;

function TLldbDebugger.CreateCallStack: TCallStackSupplier;
begin
  Result := TLldbCallStack.Create(Self);
end;

function TLldbDebugger.CreateWatches: TWatchesSupplier;
begin
  Result := TLldbWatches.Create(Self);
end;

function TLldbDebugger.CreateThreads: TThreadsSupplier;
begin
  Result := TLldbThreads.Create(Self);
end;

function TLldbDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result := [dcRun, dcStop, dcStepOver, dcStepInto, dcStepOut, dcEvaluate];
//  Result := [dcPause, dcStepOverInstr, dcStepIntoInstr, dcRunTo, dcAttach, dcDetach, dcJumpto,
//             dcBreak, dcWatch, dcLocal, dcEvaluate, dcModify, dcEnvironment,
//             dcSetStackFrame, dcDisassemble
//            ];
end;

function TLldbDebugger.RequestCommand(const ACommand: TDBGCommand;
  const AParams: array of const; const ACallback: TMethod): Boolean;
var
  EvalFlags: TDBGEvaluateFlags;
begin
  LockRelease;
  try
    case ACommand of
      dcRun:         Result := LldbRun;
      //dcPause:       Result := ;
      dcStop:        Result := LldbStop;
      dcStepOver:    Result := LldbStep(saOver);
      dcStepInto:    Result := LldbStep(saInto);
      dcStepOut:     Result := LldbStep(saOut);
      dcEvaluate:    begin
                       EvalFlags := [];
                       if high(AParams) >= 1 then
                         EvalFlags := TDBGEvaluateFlags(AParams[1].VInteger);
                       Result := LldbEvaluate(String(AParams[0].VAnsiString),
                         EvalFlags, TDBGEvaluateResultCallback(ACallback));
                     end;
//      dcRunTo:       Result := GDBRunTo(String(AParams[0].VAnsiString), AParams[1].VInteger);
//      dcJumpto:      Result := GDBJumpTo(String(AParams[0].VAnsiString), AParams[1].VInteger);
//      dcAttach:      Result := GDBAttach(String(AParams[0].VAnsiString));
//      dcDetach:      Result := GDBDetach;
//      dcModify:      Result := GDBModify(String(AParams[0].VAnsiString), String(AParams[1].VAnsiString));
//      dcEnvironment: Result := GDBEnvironment(String(AParams[0].VAnsiString), AParams[1].VBoolean);
//      dcDisassemble: Result := GDBDisassemble(AParams[0].VQWord^, AParams[1].VBoolean, TDbgPtr(AParams[2].VPointer^),
//                                              String(AParams[3].VPointer^), String(AParams[4].VPointer^),
//                                              String(AParams[5].VPointer^), Integer(AParams[6].VPointer^))
//                                              {%H-};
//      dcStepOverInstr: Result := GDBStepOverInstr;
//      dcStepIntoInstr: Result := GDBStepIntoInstr;
    end;
  finally
    UnlockRelease;
  end;
end;

class function TLldbDebugger.Caption: String;
begin
  Result := 'LLDB Debugger (Alpha)';
end;

constructor TLldbDebugger.Create(const AExternalDebugger: String);
begin
  inherited Create(AExternalDebugger);
  FDebugProcess := TDebugProcess.Create(AExternalDebugger);
  FDebugProcess.OnLineSent := @DoLineSentToDbg;

  FDebugInstructionQueue := TLldbInstructionQueue.Create(FDebugProcess);
  FDebugInstructionQueue.OnBeginLinesReceived := @DoBeginReceivingLines;
  FDebugInstructionQueue.OnEndLinesReceived := @DoEndReceivingLines;
  FDebugInstructionQueue.OnBeforeHandleLineReceived := @DoBeforeLineReceived;
  FDebugInstructionQueue.OnAfterHandleLineReceived := @DoAfterLineReceived;
  FDebugInstructionQueue.OnDebuggerTerminated := @DoCmdLineDebuggerTerminated;

  FCommandQueue := TLldbDebuggerCommandQueue.Create(Self);
end;

destructor TLldbDebugger.Destroy;
begin
debugln(['!!!!!!!!!!!!!!! TLldbDebugger.Destroy ']);
  FDebugInstructionQueue.LockQueueRun;
  inherited Destroy;
  FCommandQueue.Destroy;
  FDebugInstructionQueue.Destroy;
  FDebugProcess.Destroy;
end;

procedure TLldbDebugger.Init;
var
  Cmd: TLldbDebuggerCommandInit;
begin
  DebugLnEnter('*** Init');
  FDebugProcess.CreateDebugProcess('', Environment);
  inherited Init;

  Cmd := TLldbDebuggerCommandInit.Create(Self);
  QueueCommand(Cmd);
  Cmd.ReleaseReference;
  DebugLnExit('*** Init');
end;

procedure TLldbDebugger.Done;
begin
  DebugLnEnter('!!! TLldbDebugger.Done;');
  // TODO: cancel all commands
  if FDebugProcess.DebugProcessRunning then begin
    FDebugProcess.SendCmdLn('process kill');
    FDebugProcess.SendCmdLn('quit');
  end;

  FDebugInstructionQueue.OnDebuggerTerminated := nil;  // TODO: use a flag to prevent this
  FDebugProcess.StopDebugProcess;
  FDebugInstructionQueue.OnDebuggerTerminated := @DoCmdLineDebuggerTerminated;
  inherited Done;
  DebugLnExit('!!! TLldbDebugger.Done;');
end;

function TLldbDebugger.GetLocation: TDBGLocationRec;
begin
  Result := FCurrentLocation;
end;

procedure TLldbDebugger.TestCmd(const ACommand: String);
begin
  FDebugProcess.SendCmdLn(ACommand);
end;

procedure Register;
begin
  RegisterDebugger(TLldbDebugger);
end;

end.

