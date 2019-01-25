unit DebugTCPServer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ssockets,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  debugthread,
  sockets,
  syncobjs,
  FpDbgClasses,
  lazfglhash,
  lazCollections,
  fpjson,
  fgl,
  DebugInOutputProcessor,
  DebugThreadCommand;

type

  { TFpDebugTcpServer }

  TFpDebugTcpConnectionThread = class;
  TThreadedQueueString = specialize TLazThreadedQueue<string>;
  TConnectionList = specialize TFPGObjectList<TFpDebugTcpConnectionThread>;

  TFpDebugTcpServer = class(TThread)
  private
    FPort: integer;
    FSensePorts: integer;
    FTCPConnection: TInetServer;
    FConnectionList: TConnectionList;
    FDebugThread: TFpDebugThread;
    FInitializationFinished: PRTLEvent;
    function CreateInetServer: TInetServer;
    procedure FTCPConnectionConnect(Sender: TObject; Data: TSocketStream);
    procedure FTCPConnectionAcceptError(Sender: TObject; ASocket: Longint; E: Exception; var ErrorAction: TAcceptErrorAction);
    procedure FTCPConnectionConnectQuery(Sender: TObject; ASocket: Longint; var Allow: Boolean);
  protected
    procedure Execute; override;
  public
    procedure WaitForInitialization(out Port: integer);
    procedure StopListening;
    constructor create(ADebugThread: TFpDebugThread; APort, ASensePorts: integer);
    procedure RemoveConnection(ADebugTcpConnectionThread: TFpDebugTcpConnectionThread);
    destructor Destroy; override;
  end;

  { TFpDebugTcpConnectionThread }

  TFpDebugTcpConnectionThread = class(tthread, IFpDebugListener)
  private
    FData: TSocketStream;
    FDebugThread: TFpDebugThread;
    FResponseQueue: TThreadedQueueString;
    FDebugTcpServer: TFpDebugTcpServer;
    FConnectionIdentifier: integer;
    FInOutputProcessor: TCustomInOutputProcessor;
  protected
    procedure Execute; override;
    procedure SendCommand(ACommandStr: string);
  public
    procedure SendEvent(AnEvent: TFpDebugEvent);
    function GetOrigin: string;
    constructor create(ADebugThread: TFpDebugThread; ADebugTcpServer: TFpDebugTcpServer; Data: TSocketStream);
    destructor Destroy; override;
  end;

implementation

{ TFpDebugTcpConnectionThread }

function STrToStr(AStr: string): string;
var i : integer;
begin
  result := '';
  for i := 1 to length(AStr) do
    if ord(AStr[i])<20 then
      result := result +'#'+inttostr(ord(AStr[i]))
    else
      result := result + Astr[i];
end;

procedure TFpDebugTcpConnectionThread.Execute;

  procedure WriteString(AStr: string);
  var
    i: integer;
  begin
    AStr := AStr + #10;
    i := FData.Write(AStr[1], length(AStr));

    if i < 0 then
      begin
      if FData.LastError=32 then
        begin
        // Lost connection
        end
      else
        FDebugThread.SendNotification(FConnectionIdentifier, ntConnectionProblem, null, 'Error during write. Socket-error: %d', '', [FData.LastError]);
      Terminate;
      end
    else if i < length(AStr) then
      raise exception.create('Message has not been send to client entirely');
  end;

const
  InputBufferSize = 1024;
var
  s: string;
  i: integer;
  InputBuffer: array[0..InputBufferSize-1] of char;
  InputStr: string;
begin
  WriteString('Welcome to FPDebug-server.');
  if not Terminated then
    WriteString('Your connection-idenfifier is '+IntToStr(FConnectionIdentifier)+'.');
  if not Terminated then
    WriteString('Send "help<enter>" for more information.');
  while not terminated do
    begin
    i := FData.Read(InputBuffer[0], InputBufferSize);
    if i > 0 then
      begin
      setlength(s,i);
      move(InputBuffer[0],s[1],i);
      s := StringReplace(s,#13#10,#10,[rfReplaceAll]);
      InputStr:=InputStr+s;
      i := pos(#10, InputStr);
      while i > 0 do
        begin
        s := copy(InputStr, 1, i-1);
        delete(InputStr,1,i);
        SendCommand(S);
        i := pos(#10, InputStr);
        end;
      end
    else if i < 0 then
      begin
      if FData.LastError<>35 {EAGAIN} then
        begin
        writeln('Error during write. Socket-error: '+inttostr(FData.LastError));
        Terminate;
        end;
      end
    else if i = 0 then
      begin
      // Zero-count -> Connection closed
      Terminate;
      end;

    if not terminated and (FResponseQueue.PopItem(s) = wrSignaled) then
      begin
      WriteString(s);
      end;
    end;
  FDebugTcpServer.RemoveConnection(self);
end;

procedure TFpDebugTcpConnectionThread.SendCommand(ACommandStr: string);
var
  ACommand: TFpDebugThreadCommand;
begin
  ACommand := FInOutputProcessor.TextToCommand(ACommandStr);
  if assigned(ACommand) then
    FDebugThread.QueueCommand(ACommand);
end;

procedure TFpDebugTcpConnectionThread.SendEvent(AnEvent: TFpDebugEvent);
var
  s: string;
begin
  s := FInOutputProcessor.EventToText(AnEvent);
  FResponseQueue.PushItem(s);
end;

function TFpDebugTcpConnectionThread.GetOrigin: string;
begin
  result :=  format('%d.%d.%d.%d:%d', [FData.RemoteAddress.sin_addr.s_bytes[1], FData.RemoteAddress.sin_addr.s_bytes[2],FData.RemoteAddress.sin_addr.s_bytes[3], FData.RemoteAddress.sin_addr.s_bytes[4], FData.RemoteAddress.sin_port])
end;

constructor TFpDebugTcpConnectionThread.create(ADebugThread: TFpDebugThread;
  ADebugTcpServer: TFpDebugTcpServer; Data: TSocketStream);
begin
  FData := data;

  // Set non-blocking
  {$IFDEF UNIX}
  fpfcntl(FData.Handle,F_SETFL,O_NONBLOCK);
  {$ENDIF}

  FDebugThread := ADebugThread;
  FDebugTcpServer := ADebugTcpServer;
  FResponseQueue:=TThreadedQueueString.create(100, INFINITE, 100);
  FConnectionIdentifier := FDebugThread.AddListener(self);
  FInOutputProcessor := TJSonInOutputProcessor.create(FConnectionIdentifier, @ADebugThread.SendLogMessage);
  inherited create(false);
end;

destructor TFpDebugTcpConnectionThread.Destroy;
begin
  FInOutputProcessor.Free;
  FDebugThread.RemoveListener(self);
  FResponseQueue.Free;
  FData.Free;
  inherited Destroy;
end;

{ TFpDebugTcpServer }

procedure TFpDebugTcpServer.FTCPConnectionAcceptError(Sender: TObject;
  ASocket: Longint; E: Exception; var ErrorAction: TAcceptErrorAction);
begin
  if E is ESocketError and (ESocketError(E).Code=seAcceptFailed) and (socketerror=53) {ECONNABORTED} then
    begin
    // The socket has stopped listening. The TCP-server is shutting down...
    ErrorAction:=aeaStop;
    end
  else
    writeln('ErrorAction a: '+e.ClassName + ' -- ',ErrorAction, '::',socketerror);
end;

procedure TFpDebugTcpServer.FTCPConnectionConnectQuery(Sender: TObject;
  ASocket: Longint; var Allow: Boolean);
begin
  Allow:=true;
end;

function TFpDebugTcpServer.CreateInetServer: TInetServer;
var
  Conn: boolean;
  InetServer: TInetServer;
  FFirstError: string;
  i: Integer;
begin
  result := nil;
  for i := 0 to FSensePorts-1 do
    begin
    conn := false;
    InetServer := TInetServer.Create(FPort+i);
    try
      InetServer.Listen;
      Conn:=true;
      Break;
    except
      on E: Exception do
        begin
        InetServer.Free;
        if (E is ESocketError) and (ESocketError(E).Code=seBindFailed) then
          begin
          // Ignore, try next port
          if FFirstError='' then
            FFirstError:=e.Message;
          end
        else
          Raise;
        end;
    end;
    end;
  if conn then
    begin
    result := InetServer;
    FPort:=result.Port;
    FDebugThread.SendNotification(-1, ntListenerMessage, null, 'Listening for incoming TCP-connections on port %d', '', [FPort])
    end
  else
    begin
    FPort:=-1;
    FDebugThread.SendNotification(-1, ntConnectionProblem, null, 'Failed to start listening for incoming TCP-connections: %s', '', [FFirstError])
    end;
end;

procedure TFpDebugTcpServer.FTCPConnectionConnect(Sender: TObject; Data: TSocketStream);
var
  AConnectionThread: TFpDebugTcpConnectionThread;
begin
  AConnectionThread:=TFpDebugTcpConnectionThread.create(FDebugThread, Self, data);
  AConnectionThread.FreeOnTerminate:=true;
  FConnectionList.Add(AConnectionThread);
end;

procedure TFpDebugTcpServer.Execute;
var
  AConnection: TInetServer;
begin
  try
    FTCPConnection := CreateInetServer;
    RTLeventSetEvent(FInitializationFinished);
    if assigned(FTCPConnection) then
      begin
      try
        FTCPConnection.OnConnect:=@FTCPConnectionConnect;
        FTCPConnection.OnConnectQuery:=@FTCPConnectionConnectQuery;
        FTCPConnection.OnAcceptError:=@FTCPConnectionAcceptError;
        FTCPConnection.StartAccepting;
      finally
        AConnection:=FTCPConnection;
        FTCPConnection := nil;
        AConnection.Free;
      end;
      end
  Except
    on E: Exception do
      begin
      WriteLn('Exception: '+e.Message);
      end;
  end;
end;

procedure TFpDebugTcpServer.WaitForInitialization(out Port: integer);
begin
  RTLeventWaitFor(FInitializationFinished);
  Port := FPort;
end;

procedure TFpDebugTcpServer.StopListening;
begin
  Terminate;
  if assigned(FTCPConnection) then
    FTCPConnection.StopAccepting(true);
end;

constructor TFpDebugTcpServer.create(ADebugThread: TFpDebugThread; APort, ASensePorts: integer);
begin
  FPort:=APort;
  if ASensePorts<1 then
    ASensePorts:=1;
  FSensePorts:=ASensePorts;
  FDebugThread:=ADebugThread;
  FConnectionList:=TConnectionList.Create(false);
  FInitializationFinished:=RTLEventCreate;
  inherited Create(false);
end;

procedure TFpDebugTcpServer.RemoveConnection(ADebugTcpConnectionThread: TFpDebugTcpConnectionThread);
begin
  FConnectionList.Remove(ADebugTcpConnectionThread);
end;

destructor TFpDebugTcpServer.Destroy;
var
  i: integer;
begin
  RTLeventdestroy(FInitializationFinished);
  for i := 0 to FConnectionList.Count-1 do
    FConnectionList[i].Terminate;
  for i := 0 to FConnectionList.Count-1 do
    FConnectionList[i].WaitFor;
  if FConnectionList.Count<>0 then
    raise exception.create('Not all connections are cleared.');
  FConnectionList.Free;
  inherited Destroy;
end;

end.

