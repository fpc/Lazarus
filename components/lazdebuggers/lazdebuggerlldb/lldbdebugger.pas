(*
  settings set prompt ((lldb)) \r\n
  settings set target.output-path /tmp/out.txt
*)
unit LldbDebugger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, DbgIntfDebuggerBase, LazLoggerBase, LazClasses,
  LazFileUtils, Maps, strutils, DebugProcess, LldbInstructions, LldbHelper;

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
    procedure Run;
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
    //function  CreateRegisters: TRegisterSupplier; override;
    function  CreateCallStack: TCallStackSupplier; override;
    //function  CreateDisassembler: TDBGDisassembler; override;
    function  CreateWatches: TWatchesSupplier; override;
    //function  CreateThreads: TThreadsSupplier; override;

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
  end;


procedure Register;

implementation

type

  { TLldbCallStack }

  TLldbCallStack = class(TCallStackSupplier)
  private
    procedure StackInstructionFinished(Sender: TObject);
  protected
    //procedure Clear;
    //procedure DoThreadChanged;
  public
    procedure RequestAtLeastCount(ACallstack: TCallStackBase;
      ARequiredMinCount: Integer); override;
    procedure UpdateCurrentIndex; override;
    procedure RequestCurrent(ACallstack: TCallStackBase); override;
    procedure RequestEntries(ACallstack: TCallStackBase); override;
  end;

  { TLldbWatches }

  TLldbWatches = class(TWatchesSupplier)
  private
  protected
    procedure InternalRequestData(AWatchValue: TWatchValue); override;
  public
  end;

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

{ TLldbCallStack }

procedure TLldbCallStack.StackInstructionFinished(Sender: TObject);
var
  Instr: TLldbInstructionStackTrace absolute Sender;
  i: Integer;
  e: TCallStackEntry;
  found, foundArg: TStringArray;
  Arguments: TStringList;
  It: TMapIterator;
  s: String;
  frame: LongInt;
begin
  It := TMapIterator.Create(Instr.Callstack.RawEntries);

  for i := 0 to Length(Instr.Res) - 1 do begin
    s := Instr.Res[i];
    if (Length(s) > 3) and (s[3] = '*') then s[3] := ' ';
    if StrMatches(s, ['    frame #'{id}, ': '{addr}, ' '{exe}, '`'{func}, '',' at '{file}, ':'{line}, ''], found) then begin
      frame := StrToIntDef(found[0], -1);
      if It.Locate(frame) then begin
        Arguments := TStringList.Create;
        if StrMatches(found[3], ['', '(', '',')'], foundArg) then begin
          Arguments.CommaText := foundArg[1];
          found[3] := foundArg[0];
        end;

        e := TCallStackEntry(It.DataPtr^);
        e.Init(StrToInt64Def(found[1],0), Arguments, found[3], found[4], '', StrToIntDef(found[5], -1));
        Arguments.Free;
      end;
    end;
  end;
  It.Free;


{
<< << TCmdLineDebugger.ReadLn "  * frame #0: 0x00429258 project1.exe`
FORMCREATE(this=0x04a91060, SENDER=0x04a91060) at unit1.pas:39"
<< << TCmdLineDebugger.ReadLn "    frame #1: 0x0041ab6f project1.exe`DOCREATE(this=0x04a91060) at customform.inc:939"
<< << TCmdLineDebugger.ReadLn "    frame #2: 0x00418bd8 project1.exe`AFTERCONSTRUCTION(this=0x04a91060) at customform.inc:149"
<< << TCmdLineDebugger.ReadLn "    frame #3: 0x0042023a project1.exe`CREATE(this=0x000000c7, vmt=0x04a91060, THEOWNER=0x04a91060) at customform.inc:3184"
<< << TCmdLineDebugger.ReadLn "    frame #4: 0x0042746e project1.exe`CREATEFORM(this=0x000000c7, INSTANCECLASS=0x04a91060, REFERENCE=<unavailable>) at application.inc:2241"
<< << TCmdLineDebugger.ReadLn "    frame #5: 0x00402a42 project1.exe`main at project1.lpr:19"
    procedure Init(const AnAddress: TDbgPtr;
                   const AnArguments: TStrings; const AFunctionName: String;
                   const {%H-}FileName, {%H-}FullName: String;
                   const ALine: Integer; AState: TDebuggerDataState = ddsValid); virtual;
}

  inherited RequestEntries(Instr.Callstack);
end;

procedure TLldbCallStack.RequestAtLeastCount(ACallstack: TCallStackBase;
  ARequiredMinCount: Integer);
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then begin
    ACallstack.SetCurrentValidity(ddsInvalid);
    Exit;
  end;

  ACallstack.Count := ARequiredMinCount + 1;
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

  tid := 0; // Debugger.Threads.CurrentThreads.CurrentThreadId; // FCurrentThreadId ?
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
  StartIdx, EndIdx: Integer;
  Instr: TLldbInstructionStackTrace;
begin
  StartIdx := Max(ACallstack.LowestUnknown, 0);
  EndIdx   := ACallstack.HighestUnknown;

  Instr := TLldbInstructionStackTrace.Create(EndIdx, ACallstack);
  Instr.OnFinish := @StackInstructionFinished;
  TLldbDebugger(Debugger).FDebugInstructionQueue.QueueInstruction(Instr);
  Instr.ReleaseReference;
end;

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
DebugLnEnter(['>>> CommandQueue.Run ', FRunningCommand.ClassName, ', ', fDebugger.State]);
  FRunningCommand.Execute;
  // debugger and queue may get destroyed at the end of execute
end;

procedure TLldbDebuggerCommandQueue.CommandFinished(
  ACommand: TLldbDebuggerCommand);
begin
  if FRunningCommand = ACommand then begin
DebugLnExit(['<<< CommandQueue.Run ', FRunningCommand.ClassName, ', ', fDebugger.State]);
    ReleaseRefAndNil(FRunningCommand);
  end//;
else DebugLn('TLldbDebuggerCommandQueue.CommandFinished >> unknown ???');
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
begin
  try
    Debugger.LockRelease;
    DoExecute;
  finally
    Debugger.UnlockRelease;
  end;
end;

{ TLldbDebuggerCommandInit }

procedure TLldbDebuggerCommandInit.DoExecute;
var
  Instr: TLldbInstructionSettingSet;
begin
  Instr := TLldbInstructionSettingSet.Create('stop-line-count-after', '0');
  QueueInstruction(Instr);
  Instr.ReleaseReference;

  Instr := TLldbInstructionSettingSet.Create('stop-line-count-before', '0');
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
begin
  if ALine = '' then
    exit;

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

  if StrMatches(ALine, ['    frame #0: ' {addr}, ' ' {}, '`' {fname}, '(', '',' at ', ':', ''], found) then begin
    FCurrentLocation.Address := StrToInt64Def(found[0], 0);
    FCurrentLocation.FuncName := found[2];
    FCurrentLocation.SrcFile := found[4];
    FCurrentLocation.SrcLine := StrToIntDef(found[5], -1);
    DoCurrent(FCurrentLocation);
    ALine := '';
  end;
end;

procedure TLldbDebugger.DoBeforeLineReceived(var ALine: String);
var
  found: TStringArray;
begin
  if StrMatches(ALine, ['Process ', ' stopped']) then begin
    ALine := '';
  end;

  if StrMatches(ALine, ['* thread #', ', stop reason = ', ''], found) then begin
    FCurrentThreadId := StrToIntDef(found[0], 0);
    FCurrentStackFrame := 0;
    FDebugInstructionQueue.SetKnownThreadAndFrame(FCurrentThreadId, 0);
    SetState(dsPause);
    ALine := '';
  end;
end;

procedure TLldbDebugger.DoBeginReceivingLines(Sender: TObject);
begin
  LockRelease;
end;

procedure TLldbDebugger.DoCmdLineDebuggerTerminated(Sender: TObject);
begin
  SetState(dsError);
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

function TLldbDebugger.CreateCallStack: TCallStackSupplier;
begin
  Result := TLldbCallStack.Create(Self);
end;

function TLldbDebugger.CreateWatches: TWatchesSupplier;
begin
  Result := TLldbWatches.Create(Self);
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
  FDebugInstructionQueue.OnDebuggerTerminated := nil;
  FDebugProcess.StopDebugProcess;
  inherited Done;
  DebugLnExit('!!! TLldbDebugger.Done;');
end;

function TLldbDebugger.GetLocation: TDBGLocationRec;
begin
  Result := FCurrentLocation;
end;

procedure Register;
begin
  RegisterDebugger(TLldbDebugger);
end;

end.

