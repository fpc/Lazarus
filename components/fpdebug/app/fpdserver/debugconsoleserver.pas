unit DebugConsoleServer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  debugthread,
  DebugInOutputProcessor,
  lazCollections,
  syncobjs,
  pipes;

type

  TFpDebugEventQueue = specialize TLazThreadedQueue<TFpDebugEvent>;

  { TFpDebugConsoleServer }

  TFpDebugConsoleServer = class(TThread, IFpDebugListener)
  private
    FDebugThread: TFpDebugThread;
    FConnectionIdentifier: integer;
    FInOutputProcessor: TJSonInOutputProcessor;
    FEventQueue: TFpDebugEventQueue;
  protected
    procedure Execute; override;
  public
    constructor create(ADebugThread: TFpDebugThread);
    function GetOrigin: string;
    procedure SendEvent(AnEvent: TFpDebugEvent);
    destructor Destroy; override;
  end;

implementation

{ TFpDebugConsoleServer }

procedure TFpDebugConsoleServer.Execute;
var
  InputStream: TInputPipeStream;
  DebugEvent: TFpDebugEvent;
  CommandStr: string;
  ACommand: TFpDebugThreadCommand;
  b: char;
begin
  InputStream:=TInputPipeStream.Create(StdInputHandle);
  FInOutputProcessor := TJSonInOutputProcessor.create(FConnectionIdentifier, @FDebugThread.SendLogMessage);
  FConnectionIdentifier := FDebugThread.AddListener(self);
  try
    while not terminated do
      begin
      if FEventQueue.PopItem(DebugEvent) = wrSignaled then
        begin
        writeln(FInOutputProcessor.EventToText(DebugEvent));
        end;
      while InputStream.NumBytesAvailable>0 do
        begin
        InputStream.Read(b,sizeof(b));
        if b <> #10 then
          CommandStr:=CommandStr+b
        else
          begin
          ACommand := FInOutputProcessor.TextToCommand(CommandStr);
          if assigned(ACommand) then
            FDebugThread.QueueCommand(ACommand);
          CommandStr:='';
          end;
        end;
      end;
  finally
    FDebugThread.RemoveListener(self);
    FInOutputProcessor.Free;
    InputStream.Free;
  end;
end;

constructor TFpDebugConsoleServer.create(ADebugThread: TFpDebugThread);
begin
  FEventQueue:=TFpDebugEventQueue.Create(100, INFINITE, 100);
  FDebugThread:=ADebugThread;
  inherited Create(false);
end;

function TFpDebugConsoleServer.GetOrigin: string;
begin
  result := 'console'
end;

procedure TFpDebugConsoleServer.SendEvent(AnEvent: TFpDebugEvent);
begin
  FEventQueue.PushItem(AnEvent);
end;

destructor TFpDebugConsoleServer.Destroy;
begin
  FEventQueue.Free;
  inherited Destroy;
end;

end.

