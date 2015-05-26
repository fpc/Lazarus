unit DebugScriptServer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  debugthread,
  DebugInOutputProcessor;

type

  { TFpDebugScriptServer }

  TFpDebugScriptServer = class(TThread, IFpDebugListener)
  private
    FDebugThread: TFpDebugThread;
    FConnectionIdentifier: integer;
    FFileContents: TStringList;
    FInOutputProcessor: TJSonInOutputProcessor;
  protected
    procedure Execute; override;
  public
    constructor create(ADebugThread: TFpDebugThread; AFileName: string);
    function GetOrigin: string;
    procedure SendEvent(AnEvent: TFpDebugEvent);
    destructor Destroy; override;
  end;

implementation

{ TFpDebugScriptServer }

procedure TFpDebugScriptServer.Execute;
var
  ACommand: TFpDebugThreadCommand;
  i: Integer;
begin
  FInOutputProcessor := TJSonInOutputProcessor.create(FConnectionIdentifier, @FDebugThread.SendLogMessage);
  try
    for i := 0 to FFileContents.Count-1 do
      begin
      ACommand := FInOutputProcessor.TextToCommand(FFileContents.Strings[i]);
      if assigned(ACommand) then
        FDebugThread.QueueCommand(ACommand);
      if Terminated then
        Break;
      end;
  finally
    FInOutputProcessor.Free;
  end;
  Terminate;
end;

constructor TFpDebugScriptServer.create(ADebugThread: TFpDebugThread; AFileName: string);
begin
  inherited Create(false);
  FDebugThread:=ADebugThread;
  FConnectionIdentifier := FDebugThread.AddListener(self);
  FFileContents := TStringList.Create;
  FFileContents.LoadFromFile(AFileName);
end;

function TFpDebugScriptServer.GetOrigin: string;
begin
  result := 'File input';
end;

procedure TFpDebugScriptServer.SendEvent(AnEvent: TFpDebugEvent);
begin
  // Ignore
end;

destructor TFpDebugScriptServer.Destroy;
begin
  FFileContents.Free;
  inherited Destroy;
end;

end.

