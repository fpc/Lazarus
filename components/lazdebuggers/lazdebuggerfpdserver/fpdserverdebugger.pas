unit FPDServerDebugger;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  ssockets,
  fgl,
  forms,
  DbgIntfDebuggerBase,
  DbgIntfBaseTypes,
  fpjson,
  jsonparser,
  BaseUnix,
  LazLogger,
  dialogs,
  syncobjs,
  lazCollections,
  strutils,
  SysUtils;

type
  TThreadedQueueString = specialize TLazThreadedQueue<string>;
  TFPDServerDebugger = class;

  { TFPDSendCommand }

  TFPDSendCommand = class
  protected
    FCommandUID: integer;
    FServerDebugger: TFPDServerDebugger;
    function GetAsString: string; virtual;
    procedure ComposeJSon(AJsonObject: TJSONObject); virtual;
  public
    constructor create; virtual;
    procedure DoOnCommandSuccesfull(ACommandResponse: TJSonObject); virtual;
    procedure DoOnCommandReceived(ACommandResponse: TJSonObject); virtual;
    procedure DoOnCommandFailed(ACommandResponse: TJSonObject); virtual;
    property CommandUID: integer read FCommandUID;
    property AsString: string read GetAsString;
  end;

  { TFPDSendCommandList }

  TFPDCustomSendCommandList = specialize TFPGObjectList<TFPDSendCommand>;
  TFPDSendCommandList = class(TFPDCustomSendCommandList)
  public
    function SearchByUID(const ACommandUID: integer): TFPDSendCommand;
  end;

  { TFPDSendRunCommand }

  TFPDSendRunCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    procedure DoOnCommandFailed(ACommandResponse: TJSonObject); override;
  end;

  { TFPDSendContinueCommand }

  TFPDSendContinueCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  end;

  { TFPDSendNextCommand }

  TFPDSendNextCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  end;

  { TFPDSendStepCommand }

  TFPDSendStepCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  end;

  { TFPDSendStepIntoInstrCommand }

  TFPDSendStepIntoInstrCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  end;

  { TFPDSendStepOverInstrCommand }

  TFPDSendStepOverInstrCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  end;

  { TFPDSendStopCommand }

  TFPDSendStopCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  end;

  { TFPDSendStepOutCommand }

  TFPDSendStepOutCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  end;

  { TFPDSendFilenameCommand }

  TFPDSendFilenameCommand = class(TFPDSendCommand)
  private
    FFileName: string;
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    constructor create(AFileName: string); virtual;
  end;

  { TFPDSendAddBreakpointCommand }

  TFPDSendAddBreakpointCommand = class(TFPDSendCommand)
  private
    FFileName: string;
    FLineNr: integer;
    FLocation: TDBGPtr;
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    procedure DoOnCommandFailed(ACommandResponse: TJSonObject); override;
    procedure DoOnCommandSuccesfull(ACommandResponse: TJSonObject); override;
    constructor create(AFileName: string; ALineNr: integer); virtual;
    constructor create(ALocation: TDBGPtr); virtual;
  end;

  { TFPDSendRemoveBreakpointCommand }

  TFPDSendRemoveBreakpointCommand = class(TFPDSendCommand)
  private
    FLocation: TDBGPtr;
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    constructor create(ALocation: TDBGPtr); virtual;
  end;

  { TFPDSendDoCurrentCommand }

  TFPDSendDoCurrentCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    procedure DoOnCommandSuccesfull(ACommandResponse: TJSonObject); override;
  end;

  { TInterruptableInetSocket }

  TInterruptableInetSocket = class(TInetSocket)
  private
    FIntHandler: TSocketHandler;
  public
    constructor Create(const AHost: String; APort: Word; AHandler: TSocketHandler=nil); Overload;
    procedure ShutDown;
  end;

  { TFPDSocketThread }

  TFPDSocketThread = class(TThread)
  private
    FConnectionIdentifier: integer;
    FSocket: TInetSocket;
    FDebugger: TFPDServerDebugger;
    FSendQueue: TThreadedQueueString;
  protected
    procedure ReceivedCommand(Data: PtrInt);
    procedure Execute; override;
  public
    constructor Create(ADebugger: TFPDServerDebugger);
    procedure SendString(AString: string);
    destructor Destroy; override;
    procedure CloseConnection;
    property ConnectionIdentifier: integer read FConnectionIdentifier;
  end;

  { TFPDServerDebugger }

  TFPDServerDebugger = class(TDebuggerIntf)
  private
    FSocketThread: TFPDSocketThread;
    FCommandList: TFPDSendCommandList;
  protected
    // Overrides of TDebuggerIntf methods.
    function GetSupportedCommands: TDBGCommands; override;
    // Handle Notifications received from the FPDebug-server
    procedure HandleNotification(ANotification: TJSONObject);
    // Handle log-messages received from the FPDebug-server
    procedure HandleLog(ALog: TJSONObject);
    // Handle Events received from the FPDebug-server
    procedure HandleEvent(ANotification: TJSONObject);
    // Event-handlers for events received from the FPDebug-server
    procedure DoHandleCreateProcessEvent(AnEvent: TJSONObject);
    procedure DoHandleExitProcessEvent(AnEvent: TJSONObject);
    procedure DoHandleBreakpointEvent(AnEvent: TJSONObject);
  public
    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;
    // This function is called (in the main thread) by the TFPDSocketThread on each JSon-object received
    // from the FPD-Server.
    procedure ReceivedCommand(ACommand: TJSONObject);
    // Queue a command for sending to the FPDebug-server.
    procedure QueueCommand(ACommand: TFPDSendCommand);
    // Overrides of TDebuggerIntf methods.
    class function Caption: String; override;
    function CreateBreakPoints: TDBGBreakPoints; override;
    function RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean; override;
    // These methods are called by several TFPDSendCommands after success or failure of a command. (Most common
    // because the TFPDSendCommands do not have access to TFPDServerDebugger's protected methods theirself)
    procedure DoOnRunFailed;
    procedure DoOnDoCurrentSuccessfull(ALocRec: TDBGLocationRec);
  end;

procedure Register;

implementation

type

  { TFPBreakpoint }

  TFPBreakpoint = class(TDBGBreakPoint)
  private
    FSetBreakFlag: boolean;
    FResetBreakFlag: boolean;
    FIsSet: boolean;
    FUID: integer;
    procedure SetBreak;
    procedure ResetBreak;
  protected
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure DoEnableChange; override;
    procedure DoChanged; override;
    // Used in the succes or failure callbacks of the TFPDSendAddBreakpointCommand command to set the
    // validity of the underlying breakpoint.
    procedure SetValid;
    procedure SetInvalid;
  public
    destructor Destroy; override;
    property UID: integer read FUID;
  end;

  { TFPBreakpoints }

  TFPBreakpoints = class(TDBGBreakPoints)
  public
    function FindByUID(AnUID: integer): TFPBreakpoint;
  end;

procedure Register;
begin
  RegisterDebugger(TFPDServerDebugger);
end;

{ TFPDSendCommand }

var GCommandUID: integer = 0;

{ TFPDSendRemoveBreakpointCommand }

procedure TFPDSendRemoveBreakpointCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','removebreakpoint');
  AJsonObject.Add('location', Dec2Numb(FLocation, 8, 16));
end;

constructor TFPDSendRemoveBreakpointCommand.create(ALocation: TDBGPtr);
begin
  inherited create;
  FLocation:=ALocation;
end;

{ TFPDSendResetBreakpointCommand }

function TFPDSendCommand.GetAsString: string;
var
  AJsonObject: TJSONObject;
begin
  AJsonObject := TJSONObject.Create;
  try
    ComposeJSon(AJsonObject);
    result := AJsonObject.AsJSON;
  finally
    AJsonObject.Free;
  end;
end;

procedure TFPDSendCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  AJsonObject.Add('uid', FCommandUID);
end;

constructor TFPDSendCommand.create;
begin
  inc(GCommandUID);
  FCommandUID := GCommandUID;
end;

procedure TFPDSendCommand.DoOnCommandSuccesfull(ACommandResponse: TJSonObject);
begin
  // Do nothing
end;

procedure TFPDSendCommand.DoOnCommandReceived(ACommandResponse: TJSonObject);
begin
  // Do nothing
end;

procedure TFPDSendCommand.DoOnCommandFailed(ACommandResponse: TJSonObject);
begin
  // Do nothing;
end;

{ TFPDSendCommandList }

function TFPDSendCommandList.SearchByUID(const ACommandUID: integer): TFPDSendCommand;
var
  i: Integer;
begin
  for i := 0 to count -1 do
  begin
    if Items[i].CommandUID = ACommandUID then
      begin
      result := items[i];
      exit;
      end;
  end;
  result := nil;
end;

{$I fpdserverdebuggercommands.inc}

{ TFPBreakpoint }

procedure TFPBreakpoint.SetBreak;
var
  ASendCommand: TFPDSendCommand;
begin
  ASendCommand := nil;
  case Kind of
    bpkAddress:  ASendCommand := TFPDSendAddBreakpointCommand.create(Address);
    bpkSource:   ASendCommand := TFPDSendAddBreakpointCommand.create(Source, Line);
  else
    Raise Exception.Create('Breakpoints of this kind are not suported.');
  end;
  if assigned(ASendCommand) then
    begin
    FUID:=ASendCommand.CommandUID;
    TFPDServerDebugger(Debugger).QueueCommand(ASendCommand);
    end;
  FIsSet:=true;
end;

procedure TFPBreakpoint.ResetBreak;
begin
  TFPDServerDebugger(Debugger).QueueCommand(TFPDSendRemoveBreakpointCommand.create(Address));
  FIsSet:=false;
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
    FIsSet:=false;
    end;
  inherited DoStateChange(AOldState);
end;

procedure TFPBreakpoint.DoEnableChange;
begin
  if (Debugger.State in [dsPause, dsInit, dsRun]) then
    begin
    if FEnabled and not FIsSet then
      FSetBreakFlag := True
    else if not FEnabled and FIsSet then
      FResetBreakFlag := True;
    end;
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

procedure TFPBreakpoint.SetValid;
begin
  FValid:=vsValid;
end;

procedure TFPBreakpoint.SetInvalid;
begin
  FValid:=vsInvalid;
end;

destructor TFPBreakpoint.Destroy;
begin
  if FIsSet then
    ResetBreak;
  inherited Destroy;
end;

function TFPBreakpoints.FindByUID(AnUID: integer): TFPBreakpoint;
var
  i: integer;
begin
  for i := 0 to Count-1 do
    if TFPBreakpoint(Items[i]).UID=AnUID then
      begin
      result := TFPBreakpoint(Items[i]);
      exit;
      end;
  result := nil;
end;

{ TInterruptableInetSocket }

constructor TInterruptableInetSocket.Create(const AHost: String; APort: Word; AHandler: TSocketHandler);
begin
  if not assigned(AHandler) then
    FIntHandler := TSocketHandler.Create
  else
    FIntHandler := AHandler;
  inherited create(AHost, APort, AHandler)
end;

procedure TInterruptableInetSocket.ShutDown;
begin
  FIntHandler.Shutdown(true);
end;

{ TFPDSocketThread }

procedure TFPDSocketThread.ReceivedCommand(Data: PtrInt);
var
  ACommand: TJSONObject;
begin
  ACommand := TObject(data) as TJSONObject;;
  FDebugger.ReceivedCommand(ACommand);
  ACommand.Free;
end;

procedure TFPDSocketThread.Execute;
const
  InputBufferSize = 1024;
var
  SendStr: string;
  s: string;
  i: integer;
  InputStr: string;
  JSonData: TJSONData;

  function ReadString: string;
  var
    InputBuffer: array[0..InputBufferSize-1] of char;
    s: string;
  begin
    // First see if there is a string left in the input-buffer.
    i := pos(#10, InputStr);
    if i > 0 then
      begin
      s := copy(InputStr, 1, i-1);
      delete(InputStr,1,i);
      result := s;
      exit;
      end;

    result := '';
    i := FSocket.Read(InputBuffer[0], InputBufferSize-1);
    if i=0 then
      begin
      // Connection closed
      DebugLn('Lost Connection with FPDebug-server.');
      Terminate;
      end
    else if i<0 then
      begin
      if FSocket.LastError<>35 {EAGAIN} then
        begin
        debugln('Error during write. Socket-error: '+inttostr(FSocket.LastError));
        Terminate;
        end;
      end
    else if i > 0 then
      begin
      setlength(s,i);
      move(InputBuffer[0],s[1],i);
      InputStr:=InputStr+s;
      i := pos(#10, InputStr);
      if i > 0 then
        begin
        s := copy(InputStr, 1, i-1);
        delete(InputStr,1,i);
        result := s;
        end;
      end;
  end;

  function ReadSTringTimeout(ATimeout: integer): string;
  var
    tc: int64;
  begin
    tc := GetTickCount64;
    result := ReadString;
    while not terminated and (result='') and ((GetTickCount64-tc)<ATimeout) do
      result := ReadString;
  end;

begin
  try
    FSocket := TInterruptableInetSocket.Create('127.0.0.1',9001);

    // Set non-blocking
    fpfcntl(FSocket.Handle,F_SETFL,O_NONBLOCK);

    // Read and check FPDebug Server greeting
    s := ReadSTringTimeout(100);
    if s<>'Welcome to FPDebug-server.' then
      Exit;
    // Read connection-identifier
    s := ReadSTringTimeout(100);
    delete(s,length(s),1);
    s := copy(s, rpos(' ',s)+1, 5);
    FConnectionIdentifier:=StrToIntDef(s,-1);
    if FConnectionIdentifier=-1 then
      raise Exception.Create('Failed to retreive connection-identifier');

    // Skip help-message
    s := ReadSTringTimeout(100);

    while not terminated do
      begin
      s:=ReadString;
      if s<>'' then
        begin
        JSonData := GetJSON(s);
        if JSonData is TJSONObject then
          Application.QueueAsyncCall(@ReceivedCommand, ptrint(JSonData))
        else
          raise exception.CreateFmt('JSon-command %s is not a JSON-Object.',[s]);
        end;

      if not terminated and (FSendQueue.PopItem(SendStr) = wrSignaled) then
        begin
        SendStr := SendStr + #10;
        i := FSocket.Write(SendStr[1], length(SendStr));

        if i < 0 then
          begin
          if FSocket.LastError=32 then
            begin
            // Lost connection
            end
          else
            DebugLn(Format('Error during write. Socket-error: %d',[FSocket.LastError]));
          Terminate;
          end
        else if i < length(SendStr) then
          raise exception.create('Message has not been send to client entirely');
        end;
      end;
  except
    on E: Exception do
      DebugLn('Exception in  SocketThread: '+E.Message);
  end;
end;

constructor TFPDSocketThread.Create(ADebugger: TFPDServerDebugger);
begin
  FDebugger := ADebugger;
  FSendQueue:=TThreadedQueueString.create(100, INFINITE, 100);
  inherited create(false);
end;

procedure TFPDSocketThread.SendString(AString: string);
begin
  if Assigned(FDebugger.OnDbgOutput) then
    FDebugger.OnDbgOutput(Self, 'send: '+AString);
  FSendQueue.PushItem(AString);
end;

destructor TFPDSocketThread.Destroy;
begin
  FSendQueue.Free;
  FSocket.Free;
  inherited destroy;
end;

procedure TFPDSocketThread.CloseConnection;
begin
  Terminate;
  (FSocket as TInterruptableInetSocket).ShutDown;
end;

{ TFPDServerDebugger }

function TFPDServerDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result:=[dcRun, dcStepOver, dcStepInto, dcStepOut, dcStepOverInstr, dcStepIntoInstr, dcStop];
end;

procedure TFPDServerDebugger.DoHandleCreateProcessEvent(AnEvent: TJSONObject);
begin
  SetState(dsInternalPause);
  QueueCommand(TFPDSendContinueCommand.create);
end;

procedure TFPDServerDebugger.DoHandleExitProcessEvent(AnEvent: TJSONObject);
begin
  SetState(dsStop);
end;

procedure TFPDServerDebugger.DoHandleBreakpointEvent(AnEvent: TJSONObject);
var
  BrkLocation: string;
  Brk: TDBGBreakPoint;
  Continue: boolean;
begin
  BrkLocation:=AnEvent.Get('breakpointLocation','');
  if BrkLocation<>'' then
    begin
    Brk :=  BreakPoints.Find(Hex2Dec(BrkLocation));
    if not assigned(brk) then
      debugln('Break on unknown breakpoint')
    else
      begin
      brk.Hit(Continue);
      if Continue then
        begin
        QueueCommand(TFPDSendContinueCommand.create);
        Exit;
        end;
      end;
    end;

  SetState(dsPause);
  QueueCommand(TFPDSendDoCurrentCommand.create);
end;

procedure TFPDServerDebugger.HandleNotification(ANotification: TJSONObject);
var
  NotificationType: string;
  UID: integer;
  SendCommand: TFPDSendCommand;
begin
  // Ignore notifications from other connections
  if ANotification.get('connIdentifier',-1)=FSocketThread.ConnectionIdentifier then
    begin
    NotificationType:=ANotification.Get('notificationType','');
    case NotificationType of
      'InvalidCommand':
        raise exception.CreateFmt('The FPD-Server complains about an invalid command: %s',[ANotification.get('message', '-')]);
      'ExecutedCommand', 'FailedCommand', 'ReceivedCommand':
        begin
        uid := ANotification.get('uid',-1);
        if uid > -1 then
          begin
          SendCommand := FCommandList.SearchByUID(uid);
          if assigned(SendCommand) then
            begin
            case NotificationType of
              'ExecutedCommand':
                begin
                SendCommand.DoOnCommandSuccesfull(ANotification);
                FCommandList.Remove(SendCommand);
                end;
              'FailedCommand'  :
                begin
                SendCommand.DoOnCommandFailed(ANotification);
                FCommandList.Remove(SendCommand);
                end;
              'ReceivedCommand':
                SendCommand.DoOnCommandReceived(ANotification);
            end; {case}
            end
          else
            debugln('Received command-notification for unknown command-uid '+inttostr(UID));
          end
        else
          debugln('Received command notification without UID');
        end;
    end; {case}
    end;
end;

procedure TFPDServerDebugger.HandleLog(ALog: TJSONObject);
var
  LogType: string;
  Message: string;
begin
  LogType:=ALog.get('logType','');
  Message:=ALog.Get('message','');
  case LogType of
    'debug'  : DebugLn(Message);
    'info'   : ShowMessage(Message);
    'error'  : raise Exception.Create(Message);
  else
    raise Exception.CreateFmt('Received unknown log-type from FPDebug-server. (%s)', [LogType]);
  end; {case}
end;

procedure TFPDServerDebugger.HandleEvent(ANotification: TJSONObject);
var
  EventName: string;
begin
  EventName:=ANotification.get('eventName','');
  case EventName of
    'CreateProcess' : DoHandleCreateProcessEvent(ANotification);
    'ExitProcess'   : DoHandleExitProcessEvent(ANotification);
    'BreakPoint'    : DoHandleBreakPointEvent(ANotification)
  else
    debugln('Received unknown event: '+EventName);
  end;
end;

procedure TFPDServerDebugger.QueueCommand(ACommand: TFPDSendCommand);
begin
  ACommand.FServerDebugger := self;
  FCommandList.Add(ACommand);
  FSocketThread.SendString(ACommand.AsString);
end;

constructor TFPDServerDebugger.Create(const AExternalDebugger: String);
begin
  inherited Create(AExternalDebugger);
  FSocketThread := TFPDSocketThread.Create(Self);
  FCommandList := TFPDSendCommandList.Create(true);
end;

destructor TFPDServerDebugger.Destroy;
begin
  FSocketThread.CloseConnection;
  FSocketThread.WaitFor;
  FSocketThread.Free;
  FCommandList.Free;
  inherited Destroy;
end;

procedure TFPDServerDebugger.ReceivedCommand(ACommand: TJSONObject);
var
  TypeStr: string;
begin
  DoDbgOutput('recv: '+ACommand.AsJSON);

  TypeStr := ACommand.Get('type','');
  case TypeStr of
    'event'        : HandleEvent(ACommand);
    'log'          : HandleLog(ACommand);
    'notification' : HandleNotification(ACommand);
  else
    raise Exception.CreateFmt('Received unknown event-type. (%s)',[TypeStr]);
  end;
end;

class function TFPDServerDebugger.Caption: String;
begin
  Result:='FpDebug external Dwarf-debugger (fpdserver, alpha)';
end;

function TFPDServerDebugger.CreateBreakPoints: TDBGBreakPoints;
begin
  Result := TFPBreakPoints.Create(Self, TFPBreakpoint);
end;

function TFPDServerDebugger.RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean;
begin
  result := true;
  case ACommand of
    dcRun:
      begin
      if State in [dsPause, dsInternalPause] then
        begin
        QueueCommand(TFPDSendContinueCommand.create);
        SetState(dsRun);
        end
      else
        begin
        QueueCommand(TFPDSendFilenameCommand.create(FileName));
        QueueCommand(TFPDSendRunCommand.create);
        SetState(dsInit);
        end;
      end;
    dcStepOver:
      begin
      QueueCommand(TFPDSendNextCommand.create);
      SetState(dsRun);
      end;
    dcStepInto:
      begin
      QueueCommand(TFPDSendStepCommand.create);
      SetState(dsRun);
      end;
    dcStepIntoInstr:
      begin
      QueueCommand(TFPDSendStepIntoInstrCommand.create);
      SetState(dsRun);
      end;
    dcStepOverInstr:
      begin
      QueueCommand(TFPDSendStepOverInstrCommand.create);
      SetState(dsRun);
      end;
    dcStepOut:
      begin
      QueueCommand(TFPDSendStepOutCommand.create);
      SetState(dsRun);
      end;
    dcStop:
      begin
      QueueCommand(TFPDSendStopCommand.create);
      if state=dsPause then
        SetState(dsRun);
      end
    else
      result := false;
  end;
end;

procedure TFPDServerDebugger.DoOnRunFailed;
begin
  // TDebuggerIntf.SetFileName has set the state to dsStop, to make sure
  // that dcRun could be requested. Reset the filename so that the state
  // is set to dsIdle again and is set to dsStop on the next try
  // to run.
  FileName := ''
end;

procedure TFPDServerDebugger.DoOnDoCurrentSuccessfull(ALocRec: TDBGLocationRec);
begin
  DoCurrent(ALocRec);
end;

end.

