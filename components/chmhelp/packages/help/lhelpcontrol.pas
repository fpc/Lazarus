unit LHelpControl;

{
Starts, stops and controls external help viewer via IPC.
This is used to display context-sensitive help in Lazarus, and could be used in applications to do the same.

This unit serves as reference implementation and documentation of the protocol used to communicate with help viewers.

Currently, the only help viewer that supports this protocol is the lhelp CHM help viewer.
}

{$mode objfpc}{$H+}

{$IFDEF UNIX}
  {$if FPC_FULLVERSION <= 20700}
       {$DEFINE STALE_PIPE_WORKAROUND}
  {$ENDIF}
{$ENDIF}

interface

uses
  {$IFDEF STALE_PIPE_WORKAROUND}
  BaseUnix,
  {$ENDIF}
  Classes, SysUtils, FileUtil, SimpleIPC, UTF8Process;

type
  TRequestType = (rtFile, rtUrl, rtContext);

  TLHelpResponse = (srNoAnswer, srUnknown, srSuccess, srInvalidFile, srInvalidURL, srInvalidContext);

  TFileRequest = record
    RequestType: TRequestType;
    FileName: array[0..512] of char;
  end;
  TUrlRequest = record
    FileRequest: TFileRequest;
    Url: array[0..512] of char;
  end;
  TContextRequest = record
    FileRequest: TFileRequest;
    HelpContext: THelpContext;
  end;

  TProcedureOfObject = procedure of object;
  
  { TLHelpConnection }

  TLHelpConnection = class(TObject)
  private
    FProcessWhileWaiting: TProcedureOfObject;
    fServerOut: TSimpleIPCClient; // sends messages to lhelp
    fServerIn:  TSimpleIPCServer; // recieves messages from lhelp
    function  WaitForMsgResponse: TLHelpResponse;
    function  SendMessage(Stream: TStream): TLHelpResponse;
  public
    constructor Create;
    destructor Destroy; override;
    // Checks whether the server is running using SimpleIPC
    function ServerRunning: Boolean;
    // Starts server
    // Server must support a switch --ipcname that accepts the NameForServer argument to identify it for SimpleIPC
    function StartHelpServer(NameForServer: String; ServerEXE: String = ''): Boolean;
    // Shows URL in the HelpFileName file by sending a TUrlRequest
    function OpenURL(HelpFileName: String; Url: String): TLHelpResponse;
    // Shows help for Context in the HelpFileName file by sending a TContextRequest request
    function OpenContext(HelpFileName: String; Context: THelpContext): TLHelpResponse;
    // Opens HelpFileName by sending a TContextRequest
    function OpenFile(HelpFileName: String): TLHelpResponse;
    property ProcessWhileWaiting: TProcedureOfObject read FProcessWhileWaiting write FProcessWhileWaiting;
  end;

  {$IFDEF STALE_PIPE_WORKAROUND}
  function IPCPipeIsStale(AIPC: TSimpleIPC): Boolean;
  {$ENDIF}

implementation

{ TLHelpConnection }

function TLHelpConnection.WaitForMsgResponse: TLHelpResponse;
var
  Stream: TStream;
  WaitTime: Integer = 5000;
begin
  Result := srNoAnswer;
  while WaitTime >= 0 do
  begin
    Dec(WaitTime, 50);
    if fServerIn.PeekMessage(50, True) then
    begin
      Stream := fServerIn.MsgData;
      Stream.Position:=0;
      Result := TLHelpResponse(Stream.ReadDWord);
      Exit;
    end;
    if Assigned(FProcessWhileWaiting) then FProcessWhileWaiting();
  end;
end;

function TLHelpConnection.SendMessage(Stream: TStream): TLHelpResponse;
begin
  //try
    fServerOut.SendMessage(mtUnknown, Stream);
    Result := WaitForMsgResponse;
  //except
  //  on EIPCError do Result := srNoAnswer;
  //end;
end;

constructor TLHelpConnection.Create;
begin
  fServerOut := TSimpleIPCClient.Create(nil);
  fServerIn  := TSimpleIPCServer.Create(nil);
end;

destructor TLHelpConnection.Destroy;
begin
  if fServerOut.Active then
    fServerOut.Active:=False;
  if fServerIn.Active then
    fServerIn.Active:=False;
  fServerOut.Free;
  fServerIn.Free;
  inherited Destroy;
end;

{$IFDEF STALE_PIPE_WORKAROUND}
function IPCPipeIsStale(AIPC: TSimpleIPC): Boolean;
var
  PipeName: String;
  fd: cint;
begin
  Result := False;
  PipeName:='/tmp/'+AIPC.ServerID;
  if (AIPC is TSimpleIPCServer) and (not TSimpleIPCServer(AIPC).Global) and (TSimpleIPCServer(AIPC).InstanceID <> '') then
    PipeName := PipeName +'-'+TSimpleIPCServer(AIPC).InstanceID;

  // it's possible to have a stale file that is not open for reading which will
  // cause fpOpen to hang/block later when .Active is set to true while it
  // wait's for the pipe to be opened on the other end

  // O_WRONLY | O_NONBLOCK causes fpOpen to return -1 if the file is not open for reading
  // so in fact the 'server' is not running
  fd := FpOpen(PipeName, O_WRONLY or O_NONBLOCK);
  if fd = -1 then
  begin
    Result := True;
    // delete the named pipe since it's orphaned
    FpUnlink(PipeName);
  end
  else
  FpClose(fd);

end;

{$ENDIF}
function TLHelpConnection.ServerRunning: Boolean;
{$IFDEF STALE_PIPE_WORKAROUND}
{$ENDIF}
begin
  Result := (fServerOut<>nil) and (fServerOut.ServerRunning);
  {$IFDEF STALE_PIPE_WORKAROUND}
  if not Result then
    Exit; // ==>
  Result := not IPCPipeIsStale(fServerOut);
  {$ENDIF}
end;

function TLHelpConnection.StartHelpServer(NameForServer: String;
  ServerEXE: String): Boolean;
var
  X: Integer;
begin
  Result := False;

  fServerIn.Active := False;
  fServerIn.ServerID := NameForServer+'client';
  fServerIn.Global := True;
  fServerIn.Active := True;

  fServerOut.Active := False;
  fServerOut.ServerID := NameForServer;
  if not ServerRunning then begin
    with TProcessUTF8.Create(nil) do begin
      CommandLine := ServerExe + ' --ipcname ' + NameForServer;
      Execute;
      Free;
    end;
    // give the server some time to get started
    for X := 0 to 40 do begin
      // use fServerOut.ServerRunning here instead of Self.ServerRunning to avoid a race condition
      if not fServerOut.ServerRunning then Sleep(200);
    end;
  end;
  if fServerOut.ServerRunning then begin
    fServerOut.Active := True;
    Result := True;
  end;
end;

function TLHelpConnection.OpenURL(HelpFileName: String; Url: String): TLHelpResponse;
var
  UrlRequest: TUrlRequest;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    UrlRequest.FileRequest.FileName := HelpFileName+#0;
    UrlRequest.FileRequest.RequestType := rtURL;
    UrlRequest.Url := Url+#0;
    Stream.Write(UrlRequest,SizeOf(UrlRequest));
    Result := SendMessage(Stream);
  finally
    Stream.Free;
  end;
end;

function TLHelpConnection.OpenContext(HelpFileName: String;
  Context: THelpContext) : TLHelpResponse;
var
  ContextRequest: TContextRequest;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    ContextRequest.FileRequest.FileName := HelpFileName+#0;
    ContextRequest.FileRequest.RequestType := rtContext;
    ContextRequest.HelpContext := Context;
    Stream.Write(ContextRequest, SizeOf(ContextRequest));
    Result := SendMessage(Stream);
  finally
    Stream.Free;
  end;
end;

function TLHelpConnection.OpenFile(HelpFileName: String): TLHelpResponse;
var
  FileRequest : TFileRequest;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    FileRequest.RequestType := rtFile;
    FileRequest.FileName := HelpFileName+#0;
    Stream.Write(FileRequest, SizeOf(FileRequest));
    Result := SendMessage(Stream);
  finally
    Stream.Free;
  end;
end;

end.

