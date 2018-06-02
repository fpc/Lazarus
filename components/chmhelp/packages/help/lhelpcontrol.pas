unit LHelpControl;

{
Starts, stops and controls external help viewer via IPC.
This is used to display context-sensitive help in Lazarus, and could be used in applications to do the same.

Also contains definitions used by both Lazarus IDE and help viewers.
Currently, the only help viewer that supports this protocol is the lhelp CHM help viewer.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazLoggerBase, SimpleIPC, process, UTF8Process;

const
  PROTOCOL_VERSION='2'; //IDE<>LHelp communication protocol version. Please update when breaking compatibility
  // Version 1: original version
  // Version 2:
  // - support for Proposed extensions in bug 24743:
  // - openurl: if applicable return error instead of unknown
  // - openurl: if applicable return invalid url instead of invalid file for openurl
  // Version 2.1: ipcname string constant part may only contain A..Z, a..z, _
type
  TRequestType = (rtFile, rtUrl, rtContext, rtMisc {window handling etc});
  TMiscRequests = (mrShow, mrVersion, mrClose, mrBeginUpdate, mrEndUpdate);

  TLHelpResponse = (srError, srNoAnswer, srUnknown, srSuccess, srInvalidFile, srInvalidURL, srInvalidContext);

  TFileRequest = record
    // Opening files
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
  TMiscRequest = record
    // In this record, the FileName array may have a meaning specific to the request ID.
    FileRequest: TFileRequest;
    RequestID: TMiscRequests;
  end;

  TProcedureOfObject = procedure of object;
  
  { TLHelpConnection }

  TLHelpConnection = class(TObject)
  private
    FProcessWhileWaiting: TProcedureOfObject;
    fServerOut: TSimpleIPCClient; // sends messages to lhelp
    fServerIn:  TSimpleIPCServer; // recieves messages from lhelp
    // Wait for help viewer to respond in a reasonable timeframe and return the response
    function  WaitForMsgResponse: TLHelpResponse;
    // Send a message to the help viewer
    function  SendMessage(Stream: TStream): TLHelpResponse;
  public
    constructor Create;
    destructor Destroy; override;
    // Checks whether the server is running using SimpleIPC
    function ServerRunning: Boolean;
    // Starts remote server (help viewer); if Hide specified, asks the help server to hide itself/run minimized while starting
    // Server must support a switch --ipcname that accepts the NameForServer argument to identify it for SimpleIPC
    function StartHelpServer(NameForServer: String;
      ServerEXE: String = ''; Hide: boolean=false): Boolean;
    // Shows URL in the HelpFileName file by sending a TUrlRequest
    function OpenURL(HelpFileName: String; Url: String): TLHelpResponse;
    // Shows help for Context in the HelpFileName file by sending a TContextRequest request
    function OpenContext(HelpFileName: String; Context: THelpContext): TLHelpResponse;
    // Opens HelpFileName by sending a TContextRequest
    function OpenFile(HelpFileName: String): TLHelpResponse;
    // Send BeginUpdate through miscCommand
    function BeginUpdate: TLHelpResponse;
    // Send EndUpdate through miscCommand
    function EndUpdate: TLHelpResponse;
    // Requests to run command on viewer by sending a TMiscrequest
    function RunMiscCommand(CommandID: TMiscRequests): TLHelpResponse;
    // Calling code can set this to process e.g. GUI handling while waiting for help to show
    property ProcessWhileWaiting: TProcedureOfObject read FProcessWhileWaiting write FProcessWhileWaiting;
  end;

implementation

{ TLHelpConnection }

function TLHelpConnection.WaitForMsgResponse: TLHelpResponse;
const
  // This value should be big enough to give any reasonable help system time to
  // respond.
  // If it does respond but takes longer, the delay should be irritating for the
  // typical user so it then isn't fit for purpose.
  TimeoutSecs=10;
var
  Stream: TStream;
  StartTime: TDateTime;
  TimeOut: TDateTime;
begin
  Result := srNoAnswer;
  TimeOut := EncodeTime(0,0,TimeOutSecs,0);
  StartTime := Now;
  while (Now-StartTime)<TimeOut do
  begin
    if fServerIn.PeekMessage(50, True) then
    begin
      Stream := fServerIn.MsgData;
      Stream.Position := 0;
      Result := TLHelpResponse(Stream.ReadDWord);
      Exit;
    end;
    if Assigned(FProcessWhileWaiting) then FProcessWhileWaiting();
  end;
  debugln('LHelpControl: WaitForMsgResponse: hit timeout ('+inttostr(TimeoutSecs)+' seconds)');
end;

function TLHelpConnection.SendMessage(Stream: TStream): TLHelpResponse;
begin
  Result := srNoAnswer;
  if fServerOut.Active then
  begin
    fServerOut.SendMessage(mtUnknown, Stream);
    Result := WaitForMsgResponse;
  end;
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

function TLHelpConnection.ServerRunning: Boolean;
begin
  Result := (fServerOut<>nil) and (fServerOut.ServerID <> '') and (fServerOut.ServerRunning);
end;

function TLHelpConnection.StartHelpServer(NameForServer: String;
  ServerEXE: String; Hide: boolean=false): Boolean;
var
  X: Integer;
  Cmd: String;
begin
  Result := False;

  fServerIn.Active := False;
  fServerIn.ServerID := NameForServer+'client';
  fServerIn.Global := True;
  fServerIn.Active := True;

  fServerOut.Active := False;
  fServerOut.ServerID := NameForServer;
  if not ServerRunning then
  begin
    Cmd := ServerExe + ' --ipcname ' + NameForServer;
    if Hide then Cmd := Cmd + ' --hide';
    {$IFDEF darwin}
    if DirectoryExistsUTF8(ServerEXE+'.app') then
      ServerEXE+='.app';
    debugln(['TLHelpConnection.StartHelpServer ',ServerEXE]);
    if DirectoryExistsUTF8(ServerEXE) then
    begin
      // application bundle
      // to put lhelp into the foreground, use "open -n"
      Cmd:='/usr/bin/open -n '+ServerEXE+' --args --ipcname ' + NameForServer;
    end;
    {$ENDIF}
    with TProcessUTF8.Create(nil) do
    begin
      InheritHandles := false;
      ShowWindow:=swoShowNormal;
      ParseCmdLine(Cmd);
      debugln('TLHelpConnection.StartHelpServer: going to start help server by executing '+Cmd);
      Execute;
      Free;
    end;
    // give the server some time to get started
    for X := 0 to 40 do
    begin
      // use fServerOut.ServerRunning here instead of Self.ServerRunning to avoid a race condition
      if not fServerOut.ServerRunning then
        Sleep(200)
      else
        break;
    end;
  end;
  if fServerOut.ServerRunning then
  begin
    fServerOut.Active := True;
    Result := True;
  end
  else
  begin
    debugln('Could not get lhelp running with command '+Cmd);
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
    Result := srNoAnswer;
    try
      Stream.Write(UrlRequest,SizeOf(UrlRequest));
      Result := SendMessage(Stream);
    except
      // Catch stream read errors etc
      on E: Exception do
      begin
        debugln('Help connection: error '+E.Message+' running UrlRequest command');
      end;
    end;
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
  Result := srNoAnswer;
  try
    ContextRequest.FileRequest.FileName := HelpFileName+#0;
    ContextRequest.FileRequest.RequestType := rtContext;
    ContextRequest.HelpContext := Context;
    Result := srNoAnswer;
    try
      Stream.Write(ContextRequest, SizeOf(ContextRequest));
      Result := SendMessage(Stream);
    except
      // Catch stream read errors etc
      on E: Exception do
      begin
        debugln('Help connection: error '+E.Message+' running ContextRequest command');
      end;
    end;
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
    Result := srNoAnswer;
    try
      Stream.Write(FileRequest, SizeOf(FileRequest));
      Result := SendMessage(Stream);
    except
      // Catch stream read errors etc
      on E: Exception do
      begin
        debugln('Help connection: error '+E.Message+' running FileRequest command');
      end;
    end;
  finally
    Stream.Free;
  end;
end;

function TLHelpConnection.BeginUpdate: TLHelpResponse;
begin
  Result := RunMiscCommand(mrBeginUpdate);
end;

function TLHelpConnection.EndUpdate: TLHelpResponse;
begin
  Result := RunMiscCommand(mrEndUpdate);
end;

function TLHelpConnection.RunMiscCommand(CommandID: TMiscRequests): TLHelpResponse;
var
  MiscRequest : TMiscRequest;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    MiscRequest.FileRequest.RequestType := rtMisc;
    MiscRequest.FileRequest.FileName := ''+#0;
    //CommandID is ord(TMiscRequests)
    MiscRequest.RequestID:=CommandID;
    case CommandID of
      mrClose: ; //no arguments required
      mrShow: ;  //no arguments required
      mrVersion:
        MiscRequest.FileRequest.FileName := PROTOCOL_VERSION+#0;
    end;
    try
      Stream.Write(MiscRequest, SizeOf(MiscRequest));
      Result := SendMessage(Stream);
    except
      // Catch stream read errors etc
      on E: Exception do
      begin
        // When closing, the viewer may not respond in time, which is expected.
        if CommandID<>mrClose then
          debugln('Help connection: error '+E.Message+' running MiscRequest command');
      end;
    end;
  finally
    Stream.Free;
  end;
end;

end.

