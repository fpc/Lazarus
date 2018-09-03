unit DebugInstructions;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazClasses, LazLoggerBase, debugprocess;

type

  { TDBGInstruction }

  TDBGInstructionFlag = (
    ifRequiresThread,
    ifRequiresStackFrame
  );
  TDBGInstructionFlags = set of TDBGInstructionFlag;

  TDBGInstructionState = (
    disNew,
    disQueued,
    disDataSent,
    disContentReceived, //may still wait for closing token
    disComleted,
    disFailed
  );

  TDBGInstructionErrorFlag = (
    ifeContentError,  // the imput from debugger was not in the expected format
    ifeWriteError,    // writing to debugger (pipe) failed
    ifeReadError,
    ifeDbgNotRunning,
    ifeTimedOut,
    ifeRecoveredTimedOut, // not an error
    ifeInvalidStackFrame,
    ifeInvalidThreadId,
    ifeQueueContextError,  // The thread or stack command went ok, but something else interfered with setting th econtext
    ifeCancelled
  );
  TDBGInstructionErrorFlags = set of TDBGInstructionErrorFlag;

  TDBGInstructionQueue = class;

  { TDBGInstruction }

  TDBGInstruction = class(TRefCountedObject)
  private
    FCommand: String;
    FOnFailure: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FOnSuccess: TNotifyEvent;
    FStackFrame: Integer;
    FThreadId: Integer;
    FFlags: TDBGInstructionFlags;
    FState: TDBGInstructionState;
    FErrorFlags: TDBGInstructionErrorFlags;
    FTimeOut: Integer;
    FQueue: TDBGInstructionQueue;
  protected
    FNextQueuedInstruction, FPrevQueuedInstruction: TDBGInstruction;
  protected
    function GetCommandAsString(): String; virtual;
    procedure SendCommandDataToDbg(); virtual;
    procedure SetStateRunning;
    function  ProcessInputFromDbg(const AData: String): Boolean; virtual; abstract; // True if data was handled

  //  function  GetTimeOutVerifier: TDBGInstruction; virtual;
    procedure Init; virtual;
    procedure SetQueued(AQueue: TDBGInstructionQueue);
    procedure SetContentReceieved;
    procedure InternalCreate(ACommand: String;
                             AThread, AFrame: Integer; // ifRequiresThread, ifRequiresStackFrame will always be included
                             AFlags: TDBGInstructionFlags;
                             ATimeOut: Integer
                            );
    property Queue: TDBGInstructionQueue read FQueue;
    property NextInstruction: TDBGInstruction read FNextQueuedInstruction;
  public
    constructor Create(ACommand: String;
                       AFlags: TDBGInstructionFlags = [];
                       ATimeOut: Integer = 0
                      );
    constructor Create(ACommand: String;
                       AThread: Integer;         // ifRequiresThread will always be included
                       AOtherFlags: TDBGInstructionFlags = [];
                       ATimeOut: Integer = 0
                      );
    constructor Create(ACommand: String;
                       AThread, AFrame: Integer; // ifRequiresThread, ifRequiresStackFrame will always be included
                       AOtherFlags: TDBGInstructionFlags = [];
                       ATimeOut: Integer = 0
                      );
    procedure Cancel;
    function  IsSuccess: Boolean;
    function  IsCompleted: boolean;
    function  IsRunning: boolean;
    procedure MarkAsSuccess; // calls DoInstructionFinished // releases the instruction
    procedure MarkAsFailed;  // calls DoInstructionFinished // releases the instruction

    procedure HandleWriteError; virtual;
    procedure HandleReadError; virtual;
    procedure HandleTimeOut; virtual;
    procedure HandleRecoveredTimeOut; virtual;
    procedure HandleNoDbgRunning; virtual;
    procedure HandleContentError; virtual;
    procedure HandleError(AnError: TDBGInstructionErrorFlag; AMarkAsFailed: Boolean = True); virtual;
    function  DebugText: String;

    property Command: String read GetCommandAsString;
    property ThreadId: Integer read FThreadId;
    property StackFrame: Integer read FStackFrame;
    property Flags: TDBGInstructionFlags read FFlags;
    property State: TDBGInstructionState read FState;
    property ErrorFlags: TDBGInstructionErrorFlags read FErrorFlags;
    property TimeOut: Integer read FTimeOut;

    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnSuccess: TNotifyEvent read FOnSuccess write FOnSuccess;
    property OnFailure: TNotifyEvent read FOnFailure write FOnFailure;
  end;

  { TDBGInstructionQueue }

  TDBGInstructionQueueFlag = (
    iqfValidThread,
    iqfValidStackFrame
  );
  TDBGInstructionQueueFlags = set of TDBGInstructionQueueFlag;

  TLineReceivedNotification = procedure(var ALine: String) of object;


  TDBGInstructionQueue = class
  private
    FDebugProcess: TDebugProcess;
    FFirstQueuedInstruction, FLastQueuedInstruction: TDBGInstruction;
    FCurrentInstruction, FRunningInstruction: TDBGInstruction;
    FOnBeginLinesReceived: TNotifyEvent;
    FOnEndLinesReceived: TNotifyEvent;
    FOnDebuggerTerminated: TNotifyEvent;
    FOnAfterHandleLineReceived: TLineReceivedNotification;
    FOnBeforeHandleLineReceived: TLineReceivedNotification;

    FFlags: TDBGInstructionQueueFlags;
    FLockQueueRun: Integer;
    FCurrentStackFrame: Integer;
    FCurrentThreadId: Integer;

    procedure DoDbgLineReceived(Sender: TObject; ALine: String);
    procedure DoBeginLinesReceived(Sender: TObject);
    procedure DoEndLinesReceived(Sender: TObject);
    procedure DoDbgSendError(Sender: TObject; ALine: String);
    procedure DoDbgTerminated(Sender: TObject);

    procedure RunInstruction(AnInstruction: TDBGInstruction);
  protected
    procedure RunQueue;
    function  GetNextInstructionToRun: TDBGInstruction; virtual;
    function  GetChangeContextInstruction(AnInstruction: TDBGInstruction): TDBGInstruction; virtual;

    function  GetSelectThreadInstruction(AThreadId: Integer): TDBGInstruction; virtual; abstract;
    function  GetSelectFrameInstruction(AFrame: Integer): TDBGInstruction; virtual; abstract;

    procedure DoBeforeHandleLineReceived(var ALine: String); virtual;
    procedure DoAfterHandleLineReceived(var ALine: String); virtual;
    procedure DoDebuggerTerminated; virtual;
    procedure DoInstructionFinished(Sender: TDBGInstruction); virtual;

    procedure SendDataToDBG(ASender: TDBGInstruction; AData: String);
    procedure SendDataToDBG(ASender: TDBGInstruction; AData: String; const AValues: array of const);

    procedure RemoveInstruction(AnInstruction: TDBGInstruction);
    property  FirstInstruction: TDBGInstruction read FFirstQueuedInstruction;
  public
    constructor Create(ADebugProcess: TDebugProcess);
    destructor Destroy; override;
    procedure LockQueueRun; // prevent next instruction from running, until unLockQueueRun
    procedure UnLockQueueRun;

    procedure InvalidateThredAndFrame(AStackFrameOnly: Boolean = False);
    procedure SetKnownThread(AThread: Integer);
    procedure SetKnownThreadAndFrame(AThread, AFrame: Integer);
    procedure QueueInstruction(AnInstruction: TDBGInstruction); // Wait for instruction to be finished, not queuing
    property CurrentThreadId: Integer read FCurrentThreadId;
    property CurrentStackFrame: Integer read FCurrentStackFrame;
    property Debugger: TDebugProcess read FDebugProcess;
    property CurrentInstruction: TDBGInstruction read FCurrentInstruction;
    property RunningInstruction: TDBGInstruction read FRunningInstruction;
  public
    property OnBeforeHandleLineReceived: TLineReceivedNotification
      read FOnBeforeHandleLineReceived write FOnBeforeHandleLineReceived;
    property OnAfterHandleLineReceived: TLineReceivedNotification
      read FOnAfterHandleLineReceived write FOnAfterHandleLineReceived;
    property OnDebuggerTerminated: TNotifyEvent read FOnDebuggerTerminated write FOnDebuggerTerminated;
    property OnBeginLinesReceived: TNotifyEvent read FOnBeginLinesReceived write FOnBeginLinesReceived;
    property OnEndLinesReceived: TNotifyEvent read FOnEndLinesReceived write FOnEndLinesReceived;
  end;

function dbgs(AState: TDBGInstructionState): String; overload;
function dbgs(AFlag: TDBGInstructionQueueFlag): String; overload;
function dbgs(AFlags: TDBGInstructionQueueFlags): String; overload;
function dbgs(AFlag: TDBGInstructionFlag): String; overload;
function dbgs(AFlags: TDBGInstructionFlags): String; overload;
function dbgsInstr(AnInstr: TDBGInstruction): String;

implementation

var
  DBGMI_TIMEOUT_DEBUG, DBG_THREAD_AND_FRAME, DBG_VERBOSE: PLazLoggerLogGroup;

function dbgs(AState: TDBGInstructionState): String;
begin
  writestr(Result{%H-}, AState);
end;

function dbgs(AFlag: TDBGInstructionQueueFlag): String;
begin
  writestr(Result{%H-}, AFlag);
end;

function dbgs(AFlags: TDBGInstructionQueueFlags): String;
var
  i: TDBGInstructionQueueFlag;
begin
  Result := '';
  for i := low(TDBGInstructionQueueFlags) to high(TDBGInstructionQueueFlags) do
    if i in AFlags then
      if Result = '' then
        Result := Result + dbgs(i)
      else
        Result := Result + ', ' +dbgs(i);
  if Result <> '' then
    Result := '[' + Result + ']';
end;

function dbgs(AFlag: TDBGInstructionFlag): String;
begin
  writestr(Result{%H-}, AFlag);
end;

function dbgs(AFlags: TDBGInstructionFlags): String;
var
  i: TDBGInstructionFlag;
begin
  Result := '';
  for i := low(TDBGInstructionFlags) to high(TDBGInstructionFlags) do
    if i in AFlags then
      if Result = '' then
        Result := Result + dbgs(i)
      else
        Result := Result + ', ' +dbgs(i);
  if Result <> '' then
    Result := '[' + Result + ']';
end;

function dbgsInstr(AnInstr: TDBGInstruction): String;
begin
  if AnInstr = nil then
    Result := 'nil'
  else
    Result := AnInstr.DebugText;
end;

{ TDBGInstruction }

function TDBGInstruction.GetCommandAsString(): String;
begin
  Result := FCommand;
end;

procedure TDBGInstruction.SendCommandDataToDbg();
begin
  FQueue.SendDataToDBG(Self, GetCommandAsString);
  FState := disDataSent;
end;

procedure TDBGInstruction.SetStateRunning;
begin
  Assert(FState=disQueued, 'FState=disQueued');
  FState := disDataSent;
end;

procedure TDBGInstruction.Init;
begin
  //
end;

procedure TDBGInstruction.SetQueued(AQueue: TDBGInstructionQueue);
begin
  FState := disQueued;
  FQueue := AQueue;
end;

procedure TDBGInstruction.SetContentReceieved;
begin
  FState := disContentReceived;
debugln('disContentReceived');
end;

procedure TDBGInstruction.InternalCreate(ACommand: String; AThread,
  AFrame: Integer; AFlags: TDBGInstructionFlags; ATimeOut: Integer);
begin
  inherited Create;
  AddReference;
  FState := disNew;
  FCommand := ACommand;
  FThreadId   := AThread;
  FStackFrame := AFrame;
  FFlags := AFlags;
  FTimeOut := ATimeOut;
  Init;
end;

constructor TDBGInstruction.Create(ACommand: String;
  AFlags: TDBGInstructionFlags; ATimeOut: Integer);
begin
  InternalCreate(ACommand, -1, -1, AFlags, ATimeOut);
end;

constructor TDBGInstruction.Create(ACommand: String; AThread: Integer;
  AOtherFlags: TDBGInstructionFlags; ATimeOut: Integer);
begin
  InternalCreate(ACommand, AThread, -1,
                 AOtherFlags + [ifRequiresThread], ATimeOut);
end;

constructor TDBGInstruction.Create(ACommand: String; AThread, AFrame: Integer;
  AOtherFlags: TDBGInstructionFlags; ATimeOut: Integer);
begin
  InternalCreate(ACommand, AThread, AFrame,
                 AOtherFlags + [ifRequiresThread, ifRequiresStackFrame], ATimeOut);
end;

procedure TDBGInstruction.Cancel;
begin
debugln(['TDBGInstruction.Cancel ', Command]);
  if FState = disQueued then
    FQueue.RemoveInstruction(Self)
  else
    HandleError(ifeCancelled);
end;

function TDBGInstruction.IsSuccess: Boolean;
begin
  Result := FState = disComleted;
end;

function TDBGInstruction.IsCompleted: boolean;
begin
  Result := (FState = disComleted) or (FState = disFailed);
end;

function TDBGInstruction.IsRunning: boolean;
begin
  Result := (FState = disDataSent) or (FState = disContentReceived);
end;

procedure TDBGInstruction.MarkAsSuccess;
begin
debugln(['TDBGInstruction.MarkAsSuccess SUCCESS ', Command]);
  FState := disComleted;
  if FOnSuccess <> nil then FOnSuccess(Self);
  if FOnFinish <> nil then FOnFinish(Self);

  If FQueue <> nil then
    FQueue.DoInstructionFinished(Self);
end;

procedure TDBGInstruction.MarkAsFailed;
begin
debugln(['TDBGInstruction.MarkAsFailed FAILED ',Command]);
  FState := disFailed;
  if FOnFailure <> nil then FOnFailure(Self);
  if FOnFinish <> nil then FOnFinish(Self);

  If FQueue <> nil then
    FQueue.DoInstructionFinished(Self);
end;

procedure TDBGInstruction.HandleWriteError;
begin
  HandleError(ifeWriteError, False);
//  if (FTimeOut = 0) or (FTimeOut > TIMEOUT_AFTER_WRITE_ERROR) then
//    FTimeOut := TIMEOUT_AFTER_WRITE_ERROR;
end;

procedure TDBGInstruction.HandleReadError;
begin
  HandleError(ifeReadError);
end;

procedure TDBGInstruction.HandleTimeOut;
begin
  HandleError(ifeTimedOut);
end;

procedure TDBGInstruction.HandleRecoveredTimeOut;
begin
  Include(FErrorFlags, ifeRecoveredTimedOut);
end;

procedure TDBGInstruction.HandleNoDbgRunning;
begin
  HandleError(ifeDbgNotRunning);
end;

procedure TDBGInstruction.HandleContentError;
begin
  HandleError(ifeContentError);
end;

procedure TDBGInstruction.HandleError(AnError: TDBGInstructionErrorFlag;
  AMarkAsFailed: Boolean);
begin
  Include(FErrorFlags,  AnError);
  if AMarkAsFailed then
    MarkAsFailed;
end;

function TDBGInstruction.DebugText: String;
begin
  Result := ClassName + ': "' + FCommand + '", ' + dbgs(FFlags) + ', ' + dbgs(FState)+' # refcnt '+dbgs(RefCount);
  if ifRequiresThread in FFlags then
    Result := Result + ' Thr=' + IntToStr(FThreadId);
  if ifRequiresStackFrame in FFlags then
    Result := Result + ' Frm=' + IntToStr(FStackFrame);
end;

{ TDBGInstructionQueue }

procedure TDBGInstructionQueue.RunQueue;
var
  ContextInstr: TDBGInstruction;
begin
  if FLockQueueRun > 0 then
    exit;
  if FRunningInstruction <> nil then
    exit;
debugln(['TDBGInstructionQueue.RunQueue ', dbgsInstr(FCurrentInstruction), ' / ', dbgsInstr(FRunningInstruction)]);

  if FCurrentInstruction = nil then begin
    FCurrentInstruction := GetNextInstructionToRun;
    if FCurrentInstruction = nil then
      exit;
    FCurrentInstruction.AddReference;
    RemoveInstruction(FCurrentInstruction);
DebugLnEnter(['>> Current Instruction: ', FCurrentInstruction.Command]);
  end;

  // set FCurrentInstruction to a pre running state, while changing stack....

  ContextInstr := GetChangeContextInstruction(FCurrentInstruction);
  if ContextInstr <> nil then begin
    ContextInstr.SetQueued(Self);
    RunInstruction(ContextInstr);
    ContextInstr.ReleaseReference;
    exit; // run will be called again
  end;

  RunInstruction(FCurrentInstruction);
  ReleaseRefAndNil(FCurrentInstruction);
end;

procedure TDBGInstructionQueue.DoDbgLineReceived(Sender: TObject; ALine: String
  );
begin
  // Lock DoInstructionFinished
  DoBeforeHandleLineReceived(ALine);
  if (FRunningInstruction <> nil) and (ALine <> '') then begin
    if FRunningInstruction.ProcessInputFromDbg(ALine) then
      ALine := '';
  end;
  if ALine <> '' then
    DoAfterHandleLineReceived(ALine);

  if (FRunningInstruction = nil) and (ALine <> '') then
    DebugLn(DBG_VERBOSE, ['TDBGInstructionQueue: Got Data, but no command running: ', ALine]);
  // unlock DoInstructionFinished
end;

procedure TDBGInstructionQueue.DoBeginLinesReceived(Sender: TObject);
begin
  if FOnBeginLinesReceived <> nil then
    FOnBeginLinesReceived(Self);
end;

procedure TDBGInstructionQueue.DoEndLinesReceived(Sender: TObject);
begin
  if FOnEndLinesReceived <> nil then
    FOnEndLinesReceived(Self);
end;

procedure TDBGInstructionQueue.DoDbgSendError(Sender: TObject; ALine: String);
begin
  if FRunningInstruction <> nil then
    FRunningInstruction.HandleWriteError;
end;

procedure TDBGInstructionQueue.DoDbgTerminated(Sender: TObject);
begin
  if FRunningInstruction <> nil then
    FRunningInstruction.HandleNoDbgRunning;
  DoDebuggerTerminated;
end;

function TDBGInstructionQueue.GetNextInstructionToRun: TDBGInstruction;
begin
  Result := FFirstQueuedInstruction;
end;

function TDBGInstructionQueue.GetChangeContextInstruction(
  AnInstruction: TDBGInstruction): TDBGInstruction;
begin
  Result := nil;
  if (ifRequiresThread in AnInstruction.Flags) and
  ( (CurrentThreadId <> AnInstruction.ThreadId) or not (iqfValidThread in FFlags) )
  then begin
    Result := GetSelectThreadInstruction(AnInstruction.ThreadId);
    exit;
  end;

  if (ifRequiresStackFrame in AnInstruction.Flags) and
  ( (CurrentStackFrame <> AnInstruction.StackFrame) or not (iqfValidStackFrame in FFlags) )
  then begin
    Result := GetSelectFrameInstruction(AnInstruction.StackFrame);
    exit;
  end;
end;

procedure TDBGInstructionQueue.DoBeforeHandleLineReceived(var ALine: String);
begin
  if FOnBeforeHandleLineReceived <> nil then
    FOnBeforeHandleLineReceived(ALine);
end;

procedure TDBGInstructionQueue.DoAfterHandleLineReceived(var ALine: String);
begin
  if FOnAfterHandleLineReceived <> nil then
    FOnAfterHandleLineReceived(ALine);
end;

procedure TDBGInstructionQueue.DoDebuggerTerminated;
begin
  if FOnDebuggerTerminated <> nil then
    FOnDebuggerTerminated(self);
end;

procedure TDBGInstructionQueue.DoInstructionFinished(Sender: TDBGInstruction);
begin
DebugLnExit(['<< Finished Instruction: ', FRunningInstruction.Command, ' // ', Sender=FRunningInstruction]);
if nil = FCurrentInstruction then DebugLnExit(['<< Current Instruction: ']);
  ReleaseRefAndNil(FRunningInstruction);
  RunQueue;
end;

procedure TDBGInstructionQueue.RunInstruction(AnInstruction: TDBGInstruction);
begin
DebugLnEnter(['>> Running Instruction: ', AnInstruction.Command]);
  FRunningInstruction := AnInstruction;
  FRunningInstruction.AddReference;
  FRunningInstruction.SendCommandDataToDBG;
end;

procedure TDBGInstructionQueue.RemoveInstruction(AnInstruction: TDBGInstruction
  );
begin
  If AnInstruction.FPrevQueuedInstruction <> nil then
    AnInstruction.FPrevQueuedInstruction.FNextQueuedInstruction := AnInstruction.FNextQueuedInstruction
  else begin
  assert(FFirstQueuedInstruction = AnInstruction, 'not on queue');
    FFirstQueuedInstruction := AnInstruction.FNextQueuedInstruction;
  end;

  If AnInstruction.FNextQueuedInstruction <> nil then
    AnInstruction.FNextQueuedInstruction.FPrevQueuedInstruction := AnInstruction.FPrevQueuedInstruction
  else begin
  assert(FLastQueuedInstruction = AnInstruction, 'not on queue');
    FLastQueuedInstruction := AnInstruction.FPrevQueuedInstruction;
  end;

  AnInstruction.FPrevQueuedInstruction := nil;
  AnInstruction.FNextQueuedInstruction := nil;
  AnInstruction.ReleaseReference;
end;

procedure TDBGInstructionQueue.SendDataToDBG(ASender: TDBGInstruction;
  AData: String);
begin
  FDebugProcess.SendCmdLn(AData);
end;

procedure TDBGInstructionQueue.SendDataToDBG(ASender: TDBGInstruction;
  AData: String; const AValues: array of const);
begin
  SendDataToDBG(ASender, Format(AData, AValues));
end;

constructor TDBGInstructionQueue.Create(ADebugProcess: TDebugProcess);
begin
  FDebugProcess := ADebugProcess;
  FDebugProcess.OnLineReceived := @DoDbgLineReceived;
  FDebugProcess.OnBeginLinesReceived := @DoBeginLinesReceived;
  FDebugProcess.OnEndLinesReceived := @DoEndLinesReceived;
  FDebugProcess.OnSendError := @DoDbgSendError;
  FDebugProcess.OnTerminate := @DoDbgTerminated;
end;

destructor TDBGInstructionQueue.Destroy;
begin
  while FFirstQueuedInstruction <> nil do
    RemoveInstruction(FFirstQueuedInstruction);
  if FRunningInstruction <> nil then
    DoInstructionFinished(FRunningInstruction); // TODO: maybe cancel?
  inherited Destroy;
end;

procedure TDBGInstructionQueue.LockQueueRun;
begin
  inc(FLockQueueRun);
end;

procedure TDBGInstructionQueue.UnLockQueueRun;
begin
  dec(FLockQueueRun);
  if FLockQueueRun = 0 then
    RunQueue;
end;

procedure TDBGInstructionQueue.InvalidateThredAndFrame(AStackFrameOnly: Boolean);
begin
  if AStackFrameOnly then begin
    DebugLn(DBG_THREAD_AND_FRAME, ['TGDB_IQ: Invalidating queue''s stack only. Was: ', dbgs(FFlags), ' Thr=', FCurrentThreadId, ' Frm=', FCurrentStackFrame]);
    FFlags := FFlags - [iqfValidStackFrame];
  end
  else begin
    DebugLn(DBG_THREAD_AND_FRAME, ['TGDB_IQ: Invalidating queue''s current thread and stack. Was: ', dbgs(FFlags), ' Thr=', FCurrentThreadId, ' Frm=', FCurrentStackFrame]);
    FFlags := FFlags - [iqfValidThread, iqfValidStackFrame];
  end;
end;

procedure TDBGInstructionQueue.SetKnownThread(AThread: Integer);
begin
  DebugLn(DBG_THREAD_AND_FRAME, ['TGDB_IQ: Setting queue''s current thread and stack. New: Thr=', AThread, ' Was: ', dbgs(FFlags), ' Thr=', FCurrentThreadId, ' Frm=', FCurrentStackFrame]);
  FCurrentThreadId := AThread;
  FFlags := FFlags + [iqfValidThread] - [iqfValidStackFrame];
end;

procedure TDBGInstructionQueue.SetKnownThreadAndFrame(AThread, AFrame: Integer);
begin
  DebugLn(DBG_THREAD_AND_FRAME, ['TGDB_IQ: Setting queue''s current thread and stack. New: Thr=', AThread, ' Frm=', AFrame,' Was: ', dbgs(FFlags), ' Thr=', FCurrentThreadId, ' Frm=', FCurrentStackFrame]);
  FCurrentThreadId := AThread;
  FCurrentStackFrame := AFrame;
  FFlags := FFlags + [iqfValidThread, iqfValidStackFrame];
end;

procedure TDBGInstructionQueue.QueueInstruction(AnInstruction: TDBGInstruction);
begin
debugln(['TDBGInstructionQueue.QueueInstruction ', AnInstruction.DebugText]);
  Assert(AnInstruction.State = disNew, 'queue only new instr');
  AnInstruction.AddReference;
  AnInstruction.FNextQueuedInstruction := nil;
  AnInstruction.FPrevQueuedInstruction := FLastQueuedInstruction;
  if FLastQueuedInstruction <> nil then
    FLastQueuedInstruction.FNextQueuedInstruction := AnInstruction
  else
    FFirstQueuedInstruction := AnInstruction;
  FLastQueuedInstruction := AnInstruction;
  AnInstruction.SetQueued(Self);
  RunQueue;
end;

initialization
  DBGMI_TIMEOUT_DEBUG := DebugLogger.FindOrRegisterLogGroup('DBGMI_TIMEOUT_DEBUG' {$IFDEF DBGMI_TIMEOUT_DEBUG} , True {$ENDIF} );
  DBG_THREAD_AND_FRAME := DebugLogger.FindOrRegisterLogGroup('DBG_THREAD_AND_FRAME' {$IFDEF DBG_THREAD_AND_FRAME} , True {$ENDIF} );
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );

end.

