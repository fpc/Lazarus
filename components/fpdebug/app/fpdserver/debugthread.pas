unit debugthread;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils, fgl,
  FPDbgController,
  FpDbgDwarfDataClasses,
  FpdMemoryTools,
  DbgIntfBaseTypes,
  DbgIntfDebuggerBase,
  lazCollections,
  syncobjs,
  lazfglhash,
  fpjson,
  FpDbgClasses;

type
  // The debug-thread sends three different kind of messages to it's listeners
  TFpDebugEventType = (
    etEvent,             // Messages that are send by the debugger. (debuggee has been started, pauzed, stopped, etc.)
    etLog,               // Log-messages send by the debugger. (Fpdebug also uses log-messages to inform users of some
                         // events (the dllInfo-log messages)
    etNotification       // Messages from the debug-thread itself. Including new or lost connections and commands that
                         // are queued or executed.
  );

  // The different kinds of etNotifications
  TFpDebugNotificationType = (
    ntNewConnection,
    ntLostConnection,
    ntInvalidCommand,
    ntConnectionProblem,
    ntListenerMessage,
    ntReceivedCommand,
    ntExecutedCommand,
    ntFailedCommand
  );

  TFpDebugEventCallStackEntry = record
    AnAddress: TDBGPtr;
    FrameAdress: TDBGPtr;
    SourceFile: string;
    FunctionName: string;
    Line: integer;
  end;
  TFpDebugEventWatchEntry = record
    TextValue: string;
    Expression: string;
    NumValue: TDBGPtr;
    Size: byte;
  end;

  TFpDebugEventCallStackEntryArray = array of TFpDebugEventCallStackEntry;
  TFpDebugEventDisassemblerEntryArray = array of TDisassemblerEntry;
  TFpDebugEventWatchEntryArray = array of TFpDebugEventWatchEntry;

  TFPDLogLevel = (dllDebug, dllInfo, dllError);
  TOnLog = procedure(const AString: string; const ALogLevel: TFPDLogLevel) of object;

  // This record is used to pass debugging-events. Not every field is applicable for each type of event.
  TFpDebugEvent = record
    SendByConnectionIdentifier: integer;
    EventType: TFpDebugEventType;
    NotificationType: TFpDebugNotificationType;
    Message: string;
    EventName: string;
    LogLevel: TFPDLogLevel;
    InstructionPointerRegValue: TDBGPtr;
    AnUID: variant;
    BreakpointServerIdr: Integer;
    LocationRec: TDBGLocationRec;
    Validity: TDebuggerDataState;
    Addr1: TDBGPtr;
    Addr2: TDBGPtr;
    Addr3: TDBGPtr;
    StackEntryArray: TFpDebugEventCallStackEntryArray;
    DisassemblerEntryArray: TFpDebugEventDisassemblerEntryArray;
    WatchEntryArray: TFpDebugEventWatchEntryArray;
  end;

  // Each listener should implement this interface.
  IFpDebugListener = interface ['{2230763A-672E-4EC1-941D-6B8814D789C8}']
     // This procedure is called by the debugthread when there is a message for the listener.
     // Not that this procedure will be called from within the debug-thread, and should not take too much
     // resources, or ot will slow down the debugging.
     procedure SendEvent(AnEvent: TFpDebugEvent);
     // Gives more information about the origin of the listener.
     function GetOrigin: string;
  end;

  TFpDebugThread = class;

  { TFpServerDbgController }

  TFpServerDbgController = class(TDbgController)
  private type
    TBreakPointIdMap = specialize TFPGMap<Integer, TFpDbgBreakpoint>;
  function DoBreakPointCompare(Key1, Key2: Pointer): Integer;
  private
    FBreakPointIdCnt: Integer;
    FBreakPointIdMap: TBreakPointIdMap;
  public
    constructor Create; override;
    destructor Destroy; override;
    function AddInternalBreakPointToId(ABrkPoint: TFpInternalBreakpoint): Integer;
    function GetInternalBreakPointFromId(AnId: Integer): TFpDbgBreakpoint;
    function GetIdFromInternalBreakPoint(ABrkPoint: TFpDbgBreakpoint): Integer;
    procedure RemoveInternalBreakPoint(AnId: Integer);
    //procedure RemoveInternalBreakPoint(ABrkPoint: TFpDbgBreakpoint);
    procedure ClearInternalBreakPoint;
  end;

  { TFpDebugThreadCommand }

  // The base class for all commands that can be send to the debug-thread.

  TFpDebugThreadCommand = class
  private
    FOnLog: TOnLog;
  protected
    FListenerIdentifier: integer;
    FUID: variant;
    procedure Log(const AString: string; const ALogLevel: TFPDLogLevel);
  public
    constructor Create(AListenerIdentifier: integer; AnUID: variant; AOnLog: TOnLog); virtual;
    // Descendents may override this procedure to add additionol information to the event that will
    // be send to all listeners when a command has been received succesfully
    procedure ComposeReceiveEvent(var AnEvent: TFpDebugEvent); virtual;
    // As above, for commands that has been executed successfully
    procedure ComposeSuccessEvent(var AnEvent: TFpDebugEvent); virtual;
    // As above, for commands that has failed to execute.
    procedure ComposeFailureEvent(var AnEvent: TFpDebugEvent); virtual;
    // Descendents have to override this function to implement the actual command. This function is called from within
    // the controller's debug loop. (This means it is only executed when the debuggee is paused or stopped)
    // Should return true on success, false on a failure. Set DoProcessLoop to true when the debuggee should continue,
    // make it false if the debuggee should stay in a paused state.
    function Execute(AController: TFpServerDbgController; out DoProcessLoop: boolean): boolean; virtual; abstract;
    // This method is called before the command is queued for execution in the controller's debug loop. This
    // can happen in any thread. If DoQueueCommand is true, the result is ignored or else a success-event is
    // send if the result is true, a failure if the result is false.
    function PreExecute(AController: TFpServerDbgController; out DoQueueCommand: boolean): boolean; virtual;
    // The name that is used to identify the command
    class function TextName: string; virtual; abstract;
    // The identifier of the Listener that has send this command
    property ListenerIdentifier: integer read FListenerIdentifier;
  end;
  TFpDebugThreadCommandClass = class of TFpDebugThreadCommand;
  TFpDebugThreadCommandQueue = specialize TLazThreadedQueue<TFpDebugThreadCommand>;

  { TFpDebugThread }

  TFpDebugThread = class(TThread)
  private
    FCommandQueue: TFpDebugThreadCommandQueue;
    FController: TFpServerDbgController;
    FListenerList: TThreadList;
    FMemConverter: TFpDbgMemConvertorLittleEndian;
    FMemReader: TDbgMemReader;
    FMemManager: TFpDbgMemManager;
    FConsoleOutputThread: TThread;
    procedure FreeConsoleOutputThread;
  protected
    // Handlers for the FController-events
    procedure FControllerHitBreakpointEvent(var continue: boolean; const Breakpoint: TFpDbgBreakpoint;
      AnEventType: TFPDEvent; AMoreHitEventsPending: Boolean);
    procedure FControllerProcessExitEvent(ExitCode: DWord);
    procedure FControllerCreateProcessEvent(var continue: boolean);
    procedure FControllerDebugInfoLoaded(Sender: TObject);
    // Main debug thread-loop
    procedure Execute; override;
    // Send an event to all listeners
    procedure SendEvent(ADebugEvent: TFpDebugEvent);
    procedure ClearEvent(var AnEvent: TFpDebugEvent);
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TFpDebugThread;
    // Sends a command to the command-queue, the command-queue takes ownership of the command.
    procedure QueueCommand(ACommand: TFpDebugThreadCommand);
    // Procedures to send notifications and log-messages to the listeners
    procedure SendNotification(AConnectionIdentifier: integer; ANotificationType: TFpDebugNotificationType; AnUID: variant; AMessage, ACommand: string);
    procedure SendNotification(AConnectionIdentifier: integer; ANotificationType: TFpDebugNotificationType; AnUID: variant; AMessage, ACommand: string; Arg: array of const); overload;
    procedure SendLogMessage(const AString: string; const ALogLevel: TFPDLogLevel);
    // Methods to add and remove listeners
    function AddListener(AFpDebugListener: IFpDebugListener): integer;
    procedure RemoveListener(AFpDebugListener: IFpDebugListener);
  end;

const
  FpEventTypeNames: array[TFpDebugEventType] of string = (
    'event',
    'log',
    'notification');
  FpDebugNotificationTypeNames: array[TFpDebugNotificationType] of string = (
    'NewConnection',
    'LostConnection',
    'InvalidCommand',
    'ConnectionProblem',
    'ListenerMessage',
    'ReceivedCommand',
    'ExecutedCommand',
    'FailedCommand');


implementation

var
  FFpDebugThread: TFpDebugThread;

type

  { TFpDbgMemReader }

  TFpDbgMemReader = class(TDbgMemReader)
    private
      FDebugThread: TFpDebugThread;
    protected
      function GetDbgProcess: TDbgProcess; override;
    public
      constructor create(ADebugThread: TFpDebugThread);
  end;

  { TFpWaitForConsoleOutputThread }

  TFpWaitForConsoleOutputThread = class(TThread)
  private
    FDebugThread: TFpDebugThread;
  public
    constructor Create(ADebugThread: TFpDebugThread);
    procedure Execute; override;
  end;

{ TFpServerDbgController }

function TFpServerDbgController.DoBreakPointCompare(Key1, Key2: Pointer
  ): Integer;
begin
  Result := PPointer(Key1)^ - PPointer(Key1)^;
end;

constructor TFpServerDbgController.Create;
begin
  FBreakPointIdMap := TBreakPointIdMap.Create;
  FBreakPointIdMap.OnDataPtrCompare := @DoBreakPointCompare;
  inherited Create;
end;

destructor TFpServerDbgController.Destroy;
begin
  inherited Destroy;
  FBreakPointIdMap.Free;
end;

function TFpServerDbgController.AddInternalBreakPointToId(
  ABrkPoint: TFpInternalBreakpoint): Integer;
begin
  inc(FBreakPointIdCnt);
  Result := FBreakPointIdCnt;
  FBreakPointIdMap.Add(Result, ABrkPoint);
end;

function TFpServerDbgController.GetInternalBreakPointFromId(AnId: Integer
  ): TFpDbgBreakpoint;
begin
  if not FBreakPointIdMap.TryGetData(AnId, Result) then
    Result := nil;
end;

function TFpServerDbgController.GetIdFromInternalBreakPoint(
  ABrkPoint: TFpDbgBreakpoint): Integer;
begin
  Result := FBreakPointIdMap.IndexOfData(ABrkPoint);
end;

procedure TFpServerDbgController.RemoveInternalBreakPoint(AnId: Integer);
begin
  FBreakPointIdMap.Remove(AnId);
end;

procedure TFpServerDbgController.ClearInternalBreakPoint;
begin
  FBreakPointIdMap.Clear;
end;

constructor TFpWaitForConsoleOutputThread.Create(ADebugThread: TFpDebugThread);
begin
  Inherited create(false);
  FDebugThread := ADebugThread;
end;

procedure TFpWaitForConsoleOutputThread.Execute;
var
  res: integer;
  AnEvent: TFpDebugEvent;
  s: string;
begin
  while not terminated do
  begin
    res := FDebugThread.FController.CurrentProcess.CheckForConsoleOutput(100);
    if res<0 then
      Terminate
    else if res>0 then
    begin
      FDebugThread.ClearEvent(AnEvent);
      AnEvent.EventType:=etEvent;
      AnEvent.EventName:='ConsoleOutput';
      s := FDebugThread.FController.CurrentProcess.GetConsoleOutput;
      if s <> '' then
      begin
        AnEvent.Message:=s;
        FDebugThread.SendEvent(AnEvent);
      end;
    end;
  end;
end;

{ TFpDbgMemReader }

function TFpDbgMemReader.GetDbgProcess: TDbgProcess;
begin
  result := FDebugThread.FController.CurrentProcess;
end;

constructor TFpDbgMemReader.create(ADebugThread: TFpDebugThread);
begin
  Inherited Create;
  FDebugThread:=ADebugThread;
end;

{ TFpDebugThreadCommand }

procedure TFpDebugThreadCommand.Log(const AString: string; const ALogLevel: TFPDLogLevel);
begin
  if assigned(FOnLog) then
    FOnLog(AString, ALogLevel);
end;

constructor TFpDebugThreadCommand.Create(AListenerIdentifier: integer;
  AnUID: variant; AOnLog: TOnLog);
begin
  FListenerIdentifier:=AListenerIdentifier;
  FUID:=AnUID;
  FOnLog:=AOnLog;
end;

procedure TFpDebugThreadCommand.ComposeReceiveEvent(var AnEvent: TFpDebugEvent);
begin
  AnEvent.EventType:=etNotification;
  AnEvent.NotificationType:=ntReceivedCommand;
  AnEvent.SendByConnectionIdentifier:=ListenerIdentifier;
  AnEvent.AnUID:=FUID;
  AnEvent.EventName:=TextName;
  AnEvent.Message:=Format('Received %s-command.',[TextName]);
end;

procedure TFpDebugThreadCommand.ComposeSuccessEvent(var AnEvent: TFpDebugEvent);
begin
  AnEvent.EventType:=etNotification;
  AnEvent.NotificationType:=ntExecutedCommand;
  AnEvent.SendByConnectionIdentifier:=ListenerIdentifier;
  AnEvent.AnUID:=FUID;
  AnEvent.EventName:=TextName;
  AnEvent.Message:=Format('%s-command executed succesfully.',[TextName]);
end;

procedure TFpDebugThreadCommand.ComposeFailureEvent(var AnEvent: TFpDebugEvent);
begin
  AnEvent.EventType:=etNotification;
  AnEvent.NotificationType:=ntFailedCommand;
  AnEvent.SendByConnectionIdentifier:=ListenerIdentifier;
  AnEvent.AnUID:=FUID;
  AnEvent.EventName:=TextName;
  AnEvent.Message:=Format('%s-command failed.',[TextName]);
end;

function TFpDebugThreadCommand.PreExecute(AController: TFpServerDbgController; out DoQueueCommand: boolean): boolean;
begin
  DoQueueCommand:=true;
  result:=true;
end;

{ TFpDebugThread }

procedure TFpDebugThread.SendLogMessage(const AString: string; const ALogLevel: TFPDLogLevel);
var
  ADebugEvent: TFpDebugEvent;
begin
  ClearEvent(ADebugEvent);
  ADebugEvent.EventType:=etLog;
  ADebugEvent.Message:=AString;
  ADebugEvent.LogLevel:=ALogLevel;

  SendEvent(ADebugEvent);
end;

procedure TFpDebugThread.ClearEvent(var AnEvent: TFpDebugEvent);
begin
  AnEvent.AnUID:=null;
  AnEvent.SendByConnectionIdentifier:=-1;
  AnEvent.InstructionPointerRegValue:=0;
  AnEvent.BreakpointServerIdr:=0;
  AnEvent.LocationRec.Address:=0;
  AnEvent.Validity:=ddsUnknown;
  SetLength(AnEvent.StackEntryArray,0);
  SetLength(AnEvent.DisassemblerEntryArray,0);
  SetLength(AnEvent.WatchEntryArray,0);
  AnEvent.Addr1:=0;
  AnEvent.Addr2:=0;
  AnEvent.Addr3:=0;
end;

procedure TFpDebugThread.FControllerDebugInfoLoaded(Sender: TObject);
begin
  TFpDwarfInfo(FController.CurrentProcess.DbgInfo).MemManager := FMemManager;
end;

procedure TFpDebugThread.FreeConsoleOutputThread;
var
  ADebugEvent: TFpDebugEvent;
  AThread: TFpWaitForConsoleOutputThread;
begin
  if assigned(FConsoleOutputThread) then
    begin
    AThread := TFpWaitForConsoleOutputThread(FConsoleOutputThread);
    FConsoleOutputThread := nil;
    AThread.Terminate;
    AThread.WaitFor;
    AThread.Free;
    end;
end;

procedure TFpDebugThread.FControllerHitBreakpointEvent(var continue: boolean;
  const Breakpoint: TFpDbgBreakpoint; AnEventType: TFPDEvent; AMoreHitEventsPending: Boolean);
var
  ADebugEvent: TFpDebugEvent;
  AnId: Integer;
begin
  ClearEvent(ADebugEvent);
  ADebugEvent.EventType:=etEvent;
  ADebugEvent.EventName:='BreakPoint';
  ADebugEvent.InstructionPointerRegValue:=FController.CurrentThread.GetInstructionPointerRegisterValue;
  if assigned(Breakpoint) then begin
    (* There may be several breakpoints at this address.
       For now sending the IP address allows the IDE to find the same breakpoint(s) as the fpdserver app.
    *)
    AnId := FController.GetIdFromInternalBreakPoint(Breakpoint);
    ADebugEvent.BreakpointServerIdr := AnId;
  end;


  SendEvent(ADebugEvent);
  continue:=false;
end;

procedure TFpDebugThread.FControllerProcessExitEvent(ExitCode: DWord);
var
  ADebugEvent: TFpDebugEvent;
begin
  FreeConsoleOutputThread;

  ClearEvent(ADebugEvent);
  ADebugEvent.EventType:=etEvent;
  ADebugEvent.EventName:='ExitProcess';
  ADebugEvent.InstructionPointerRegValue:=FController.CurrentThread.GetInstructionPointerRegisterValue;

  SendEvent(ADebugEvent);
end;

procedure TFpDebugThread.FControllerCreateProcessEvent(var continue: boolean);
var
  ADebugEvent: TFpDebugEvent;
begin
  ClearEvent(ADebugEvent);
  ADebugEvent.EventType:=etEvent;
  ADebugEvent.EventName:='CreateProcess';
  ADebugEvent.InstructionPointerRegValue:=FController.CurrentThread.GetInstructionPointerRegisterValue;

  SendEvent(ADebugEvent);

  if FController.RedirectConsoleOutput then
    FConsoleOutputThread := TFpWaitForConsoleOutputThread.Create(self);
  continue:=false;
end;

procedure TFpDebugThread.Execute;
var
  ACommand: TFpDebugThreadCommand;
  ARunLoop: boolean;
  AnEvent: TFpDebugEvent;
begin
  FController := TFpServerDbgController.Create;
  FController.RedirectConsoleOutput:=true;
  FController.OnCreateProcessEvent:=@FControllerCreateProcessEvent;
  FController.OnProcessExitEvent:=@FControllerProcessExitEvent;
  FController.OnHitBreakpointEvent:=@FControllerHitBreakpointEvent;
  FController.OnDebugInfoLoaded:=@FControllerDebugInfoLoaded;
  //TODO: DebugLogger.OnLog ....
  //FController.OnLog:=@SendLogMessage;

  try
    repeat
    try
      if FCommandQueue.PopItem(ACommand)<>wrSignaled then
        ACommand:=nil;

      if assigned(ACommand) then
        begin
        try
          ClearEvent(AnEvent);
          ACommand.ComposeReceiveEvent(AnEvent);
          SendEvent(AnEvent);
          if ACommand.Execute(FController, ARunLoop) then
            begin
            ClearEvent(AnEvent);
            ACommand.ComposeSuccessEvent(AnEvent);
            SendEvent(AnEvent);
            end
          else
            begin
            ClearEvent(AnEvent);
            ACommand.ComposeFailureEvent(AnEvent);
            SendEvent(AnEvent);
            end;
        finally
          ACommand.Free;
        end;

        while ARunLoop do
          begin
          FController.ProcessLoop;
          FController.SendEvents(ARunLoop);
          end;
        end;
    except
      on E: Exception do
        writeln('Exception in debug-thread: '+e.Message); // just continue
    end;
    until terminated;

  finally
    FController.Free;
  end;
end;

procedure TFpDebugThread.SendEvent(ADebugEvent: TFpDebugEvent);
var
  i: integer;
  AList: TList;
begin
  AList:=FListenerList.LockList;
  try
    for i := 0 to AList.Count-1 do
      begin
      IFpDebugListener(AList[i]).SendEvent(ADebugEvent);
      end;
  finally
    FListenerList.UnlockList;
  end;
end;

constructor TFpDebugThread.Create;
begin
  inherited create(false);
  FCommandQueue := TFpDebugThreadCommandQueue.create(100, INFINITE, 100);
  FListenerList:=TThreadList.Create;

  FMemReader := TFpDbgMemReader.Create(self);
  FMemConverter := TFpDbgMemConvertorLittleEndian.Create;
  FMemManager := TFpDbgMemManager.Create(FMemReader, FMemConverter);
end;

destructor TFpDebugThread.Destroy;
begin
  FreeConsoleOutputThread;
  FListenerList.Free;
  FCommandQueue.Free;
  inherited Destroy;
  FMemManager.Free;
  FMemConverter.Free;
  FMemReader.Free;
end;

class function TFpDebugThread.Instance: TFpDebugThread;
begin
  if not assigned(FFpDebugThread) then
    FFpDebugThread:=TFpDebugThread.Create;
  result := FFpDebugThread;
end;

procedure TFpDebugThread.QueueCommand(ACommand: TFpDebugThreadCommand);
var
  DoQueueCommand: boolean;
  Success: boolean;
  AnEvent: TFpDebugEvent;
begin
  try
    Success := ACommand.PreExecute(FController, DoQueueCommand);
  except
    on E: Exception do
      begin
      SendLogMessage('Exception while executing command :'+e.Message, dllError);
      DoQueueCommand:=false;
      Success:=false;
      end;
  end;
  if DoQueueCommand then
    begin
    FCommandQueue.PushItem(ACommand);
    end
  else
    begin
    try
      if Success then
        begin
        ClearEvent(AnEvent);
        ACommand.ComposeSuccessEvent(AnEvent);
        SendEvent(AnEvent);
        end
      else
        begin
        ClearEvent(AnEvent);
        ACommand.ComposeFailureEvent(AnEvent);
        SendEvent(AnEvent);
        end;
    finally
      ACommand.Free;
    end;
    end;
end;

procedure TFpDebugThread.SendNotification(AConnectionIdentifier: integer; ANotificationType: TFpDebugNotificationType; AnUID: variant; AMessage, ACommand: string);
var
  AnEvent: TFpDebugEvent;
begin
  ClearEvent(AnEvent);
  AnEvent.SendByConnectionIdentifier:=AConnectionIdentifier;
  AnEvent.EventType:=etNotification;
  AnEvent.NotificationType:=ANotificationType;
  anEvent.EventName:=ACommand;
  AnEvent.Message:=AMessage;
  AnEvent.AnUID:=AnUID;
  SendEvent(AnEvent);
end;

procedure TFpDebugThread.SendNotification(AConnectionIdentifier: integer; ANotificationType: TFpDebugNotificationType; AnUID: variant; AMessage, ACommand: string;
  Arg: array of const);
begin
  SendNotification(AConnectionIdentifier, ANotificationType, AnUID, format(AMessage, Arg), ACommand);
end;

var
  GIdentifierCount: integer = 0;

function TFpDebugThread.AddListener(AFpDebugListener: IFpDebugListener): integer;
begin
  inc(GIdentifierCount);
  result := GIdentifierCount;
  SendNotification(result, ntNewConnection, null, 'New connection from %s', '',[AFpDebugListener.GetOrigin]);
  FListenerList.Add(AFpDebugListener);
end;

procedure TFpDebugThread.RemoveListener(AFpDebugListener: IFpDebugListener);
begin
  FListenerList.Remove(AFpDebugListener);
end;

initialization
  FFpDebugThread := nil;
finalization
  FFpDebugThread.Free;
end.

