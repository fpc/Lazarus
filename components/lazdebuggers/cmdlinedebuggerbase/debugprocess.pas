{
 This unit contains the Commandline debugger class for external commandline
 debuggers.

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit DebugProcess;

{$mode objfpc}{$H+}

{$DEFINE DBG_VERBOSE}
{$DEFINE DBG_VERBOSE_FULL_DATA}

{$IFDEF MSWindows} // optional gtk
  {$DEFINE NATIVE_ASYNC_PROCESS}
{$ELSE}
  {$UNDEF NATIVE_ASYNC_PROCESS}
{$ENDIF}

interface

uses
  Classes, sysutils, AsyncProcess, LCLIntf, InterfaceBase, process,
  Pipes, LazLoggerBase, UTF8Process;

type

  TDebugProcessNotification = procedure(Sender: TObject; ALine: String) of object;

  { TDebugProcessReadThread }

  {$IFnDEF NATIVE_ASYNC_PROCESS}
  TDebugProcessReadThread = class(TThread)
  private
    FAsyncLoopWaitEvent: PRTLEvent;
  protected
    FStream: TInputPipeStream;
    FOnDataAvail: TThreadMethod;
    FOnPipeError: TThreadMethod;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt =
      DefaultStackSize);
    destructor Destroy; override;
    property AsyncLoopWaitEvent: PRTLEvent read FAsyncLoopWaitEvent;
  end;
  {$ENDIF}

  { TDebugAsyncProcess }

  TDebugAsyncProcess = class(TProcessUTF8) // TAsyncProcess
  private
    {$IFdef NATIVE_ASYNC_PROCESS}
    FPipeHandler: PPipeEventHandler;
    FProcessHandler: PProcessEventHandler;
    {$ELSE}
    FReadThread: TDebugProcessReadThread;
    {$ENDIF}
    FOnReadData: TNotifyEvent;
    FOnTerminate: TNotifyEvent;
    {$ifNdef NATIVE_ASYNC_PROCESS}
    procedure ThreadDataAvail;
    procedure ThreadPipeError;
    {$ENDIF}
    procedure FinishedReadingOutput;
  protected
    procedure HandlePipeInput(AData: PtrInt; AReasons: TPipeReasons);
    procedure HandleProcessTermination(AData: PtrInt; AReason: TChildExitReason; AInfo: dword);
    procedure UnhookPipeHandle;
    procedure UnhookProcessHandle;
  public
    procedure Execute; override;
    destructor Destroy; override;
    function Terminate(AExitCode: Integer): Boolean; override;
  published
    property OnReadData: TNotifyEvent read FOnReadData write FOnReadData;// You must read all the data in this event. Otherwise it is called again.
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

  { TDebugProcess }

  TDebugProcess = class
  private const
    DBG_STREAM_READ_LEN = 8192;
  private
    FExternalDebugger: String;
    FDbgProcess: TDebugAsyncProcess;
    FOnLineReceived: TDebugProcessNotification;
    FOnBeginLinesReceived: TNotifyEvent;
    FOnEndLinesReceived: TNotifyEvent;
    FOnLineSent: TDebugProcessNotification;
    FOnSendError: TDebugProcessNotification;
    FOnTerminate: TNotifyEvent;
    FTmpBuffer: String;
    FOutputBuf: String;
    FLockReadData: Boolean;
    procedure DoReadData(Sender: TObject);
    procedure DoTerminate(Sender: TObject);
    function  GetDbgProcess: TProcessUTF8;
    function HandleHasData(const AHandle: Integer): Boolean;
  protected
    function GetDebugProcessRunning: Boolean;
  public
    constructor Create(const AExternalDebugger: String);
    destructor Destroy; override;
    function  CreateDebugProcess(const AOptions: String; AnEnvironment: TStrings): Boolean;
    procedure StopDebugProcess;
    procedure SendCmdLn(const ACommand: String); overload;
    procedure SendCmdLn(const ACommand: String; Values: array of const); overload;
  public
    property DebugProcess: TProcessUTF8 read GetDbgProcess;
    property DebugProcessRunning: Boolean read GetDebugProcessRunning;
    property OnLineReceived: TDebugProcessNotification read FOnLineReceived write FOnLineReceived;
    property OnBeginLinesReceived: TNotifyEvent read FOnBeginLinesReceived write FOnBeginLinesReceived;
    property OnEndLinesReceived: TNotifyEvent read FOnEndLinesReceived write FOnEndLinesReceived;
    property OnLineSent: TDebugProcessNotification read FOnLineSent write FOnLineSent;
    property OnSendError: TDebugProcessNotification read FOnSendError write FOnSendError;
//    property OnTimeOut: TDebugProcessNotification read FOnTimeOut write FOnTimeOut;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

implementation

uses
  {$IFdef MSWindows} Windows {$ENDIF}
  {$IFDEF UNIX} Unix, BaseUnix {$ENDIF}
  ;

var
  DBG_CMD_ECHO, DBG_CMD_ECHO_FULL, DBG_VERBOSE, DBG_WARNINGS: PLazLoggerLogGroup;

{ TDebugProcessReadThread }

{$IFnDEF NATIVE_ASYNC_PROCESS}
procedure TDebugProcessReadThread.Execute;
var
  R: Integer;
  FDS: TFDSet;
begin
  while (not Terminated) and (FStream.Handle >= 0) do begin
    FpFD_ZERO(FDS);
    FpFD_Set(FStream.Handle, FDS);
    // R = -1 on error, 0 on timeout, >0 on success and is number of handles
    // FDS is changed, and indicates what descriptors have changed
    R := FpSelect(FStream.Handle + 1, @FDS, nil, nil, 50);

    if Terminated then
      break;

    if r < 0 then begin
DebugLn('TTTTT pipe err');
      Queue(FOnPipeError);
      exit;
    end;

    if (R > 0) and (FpFD_ISSET(FStream.Handle,FDS)=1) then begin
DebugLn('TTTTT data avail');
      Queue(FOnDataAvail);
      RTLeventWaitFor(FAsyncLoopWaitEvent);
DebugLn('TTTTT data avail continue');
    end;

  end;
DebugLn(['TTTTT loop end ', Terminated]);
end;

constructor TDebugProcessReadThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  FAsyncLoopWaitEvent := RTLEventCreate;
  inherited;
end;

destructor TDebugProcessReadThread.Destroy;
begin
  inherited Destroy;
  RTLeventdestroy(FAsyncLoopWaitEvent);
end;

{$ENDIF}

{ TDebugAsyncProcess }

procedure TDebugAsyncProcess.FinishedReadingOutput;
begin
{$ifNdef NATIVE_ASYNC_PROCESS}
  // Either finished reading, or TThread.Terminate was called
  if FReadThread <> nil then
    RTLeventSetEvent(FReadThread.AsyncLoopWaitEvent);
{$ENDIF}
end;

{$ifNdef NATIVE_ASYNC_PROCESS}
procedure TDebugAsyncProcess.ThreadDataAvail;
begin
  if not Running then begin
    //HandlePipeInput(0, [prBroken]);
    HandleProcessTermination(0, cerExit, 0);
    exit;
  end;

  HandlePipeInput(0, [prDataAvailable]);
  // SELF may have been destroyed, during read or handle-termination
end;

procedure TDebugAsyncProcess.ThreadPipeError;
begin
DebugLn(['got pipe err / is running ', Running]);
  Terminate(0);
  HandleProcessTermination(0, cerExit, 0);
end;
{$ENDIF}

procedure TDebugAsyncProcess.HandlePipeInput(AData: PtrInt;
  AReasons: TPipeReasons);
begin
  if prBroken in AReasons then
    UnhookPipeHandle;
  if prDataAvailable in AReasons then
    if FOnReadData <> nil then
      FOnReadData(Self);
end;

procedure TDebugAsyncProcess.HandleProcessTermination(AData: PtrInt;
  AReason: TChildExitReason; AInfo: dword);
begin
DebugLn('HandleProcessTermination');
  UnhookProcessHandle;
  UnhookPipeHandle;
  if FOnTerminate <> nil then
    FOnTerminate(Self);
end;

procedure TDebugAsyncProcess.UnhookPipeHandle;
begin
  {$IFdef NATIVE_ASYNC_PROCESS}
  if FPipeHandler <> nil then
    RemovePipeEventHandler(FPipeHandler);
  {$ELSE}
  if FReadThread <> nil then begin
    FReadThread.Terminate;
    FinishedReadingOutput;
  end;
  {$ENDIF}
end;

procedure TDebugAsyncProcess.UnhookProcessHandle;
begin
  {$IFdef NATIVE_ASYNC_PROCESS}
  if FProcessHandler <> nil then
    RemoveProcessEventHandler(FProcessHandler);
  {$ELSE} // should be enough in UnhookPipeHandle;
  if FReadThread <> nil then begin
    FReadThread.Terminate;
    FinishedReadingOutput;
  end;
  {$ENDIF}
end;

procedure TDebugAsyncProcess.Execute;
begin
  inherited Execute;

  {$IFdef NATIVE_ASYNC_PROCESS}
  if poUsePipes in Options then
    FPipeHandler := AddPipeEventHandler(Output.Handle, @HandlePipeInput, 0);
  FProcessHandler := AddProcessEventHandler(ProcessHandle, @HandleProcessTermination, 0);
  {$ELSE}
  if FReadThread = nil then
    FReadThread := TDebugProcessReadThread.Create(false);
  FReadThread.FStream := Output;
  FReadThread.FOnDataAvail := @ThreadDataAvail;
  FReadThread.FOnPipeError := @ThreadPipeError;
  FReadThread.Start;
  {$ENDIF}
end;

destructor TDebugAsyncProcess.Destroy;
begin
  {$IFdef NATIVE_ASYNC_PROCESS}
  UnhookProcessHandle;
  UnhookPipeHandle;
  {$ELSE}
  if FReadThread <> nil then begin
    FReadThread.Terminate;
    FinishedReadingOutput; // make sure destroy will not wait forever
debugln(['DESTROY thread destroying']);
    FreeAndNil(FReadThread);
debugln(['DESTROY thread destroyed']);
  end;
  {$ENDIF}
  inherited;
end;

function TDebugAsyncProcess.Terminate(AExitCode: Integer): Boolean;
begin
  {$ifdef NATIVE_ASYNC_PROCESS}
  UnhookProcessHandle;
  UnhookPipeHandle;
  {$ELSE}
  if FReadThread <> nil then begin
    FReadThread.Terminate;
    FinishedReadingOutput;
  end;
  {$ENDIF}
  Result := inherited Terminate(AExitCode);
end;

{ TDebugProcess }

procedure TDebugProcess.DoReadData(Sender: TObject);
  function ReadData(const AStream: TInputPipeStream; var ABuffer: String): Integer;
  var
    c: LongInt;
  begin
    Result := 0;
    c := AStream.Read(FTmpBuffer[1], DBG_STREAM_READ_LEN);
    while c > 0 do begin
      SetLength(ABuffer, Length(ABuffer)+c);
      Move(FTmpBuffer[1], ABuffer[1 + Result], c);
      Result := Result + c;
      if (c = DBG_STREAM_READ_LEN) and HandleHasData(AStream.Handle) then begin
        c := AStream.Read(FTmpBuffer[1], DBG_STREAM_READ_LEN);
      end
      else
        c := 0;
    end;
  end;
  function LineEndPos(const s: string; out LineEndLen: integer): integer;
  var
    n, idx: Integer;
  begin
    LineEndLen := 0;
    Result := pos(#10, s);
    n := pos(#13, s);
    if (n > 0) and (n < Result) then
      Result := n;

    if Result = 0 then exit;
    LineEndLen := 1;
    if Result < Length(s) then begin
      if (s[Result+1] in [#10,#13]) and (s[Result+1] <> s[Result]) then
        LineEndLen := 2;
    end;
  end;

var
  LineEndIdx, LineEndLen: Integer;
  Line: String;
begin
  if not DebugProcessRunning then begin
    StopDebugProcess;
    exit;
  end;

  if (FDbgProcess.Output <> nil) then
    ReadData(FDbgProcess.Output, Line);
  FOutputBuf := FOutputBuf + Line;

  FDbgProcess.FinishedReadingOutput; // Allow new reads, while we are processing

  if FLockReadData or (FOutputBuf = '') then
    exit;

  try
    FLockReadData := True;
    if FOnBeginLinesReceived <> nil then  // use to UnlockRelease
      FOnBeginLinesReceived(Self);

    LineEndIdx := LineEndPos(FOutputBuf, LineEndLen);
    while (LineEndIdx > 0) do begin
      Dec(LineEndIdx);
      Line := Copy(FOutputBuf, 1, LineEndIdx);
      Delete(FOutputBuf, 1, LineEndIdx + LineEndLen);

      if ((DBG_CMD_ECHO_FULL <> nil) and (DBG_CMD_ECHO_FULL^. Enabled))
      then debugln(DBG_CMD_ECHO_FULL, '<< << TCmdLineDebugger.ReadLn "',Line,'"')
      else if (length(Line) < 300)
      then debugln(DBG_CMD_ECHO, '<< << TCmdLineDebugger.ReadLn "',Line,'"')
      else debugln(DBG_CMD_ECHO, ['<< << TCmdLineDebugger.ReadLn "',copy(Line, 1, 200), '" ..(',length(Line)-300,').. "',copy(Line, length(Line)-99, 100),'"']);

      if FOnLineReceived <> nil then
        FOnLineReceived(Self, Line);

      LineEndIdx := LineEndPos(FOutputBuf, LineEndLen);
    end;

  finally
    FLockReadData := False;
    if FOnEndLinesReceived <> nil then  // use to LockRelease
      FOnEndLinesReceived(Self);
    // Debugger and Self may get destroyed at this point
  end;
end;

procedure TDebugProcess.DoTerminate(Sender: TObject);
begin
  if FOnTerminate <> nil then
    FOnTerminate(Self);
end;

function TDebugProcess.GetDbgProcess: TProcessUTF8;
begin
  Result := FDbgProcess;
end;

function TDebugProcess.HandleHasData(const AHandle: Integer): Boolean;
{$IFDEF UNIX}
var
  R: Integer;
  FDS: TFDSet;
begin
  Result := False;
  if AHandle < 0 then
    exit;

  FpFD_ZERO(FDS);
  FpFD_Set(AHandle, FDS);
  // R = -1 on error, 0 on timeout, >0 on success and is number of handles
  // FDS is changed, and indicates what descriptors have changed
  R := FpSelect(AHandle + 1, @FDS, nil, nil, 1);

  Result := (R > 0) and (FpFD_ISSET(AHandle,FDS)=1);
end;
{$ELSE linux}
{$IFdef MSWindows}
var
  TotalBytesAvailable: dword;
  R: LongBool;
begin
  R := Windows.PeekNamedPipe(AHandle, nil, 0, nil, @TotalBytesAvailable, nil);
  if not R then begin
    // PeekNamedPipe failed
    DebugLn(DBG_WARNINGS, ['PeekNamedPipe failed, GetLastError is ', GetLastError]);
    Exit;
  end;
  Result := TotalBytesAvailable > 0;
end;
{$ELSE win32}
begin
  DebugLn('ToDo: implement WaitForHandles for this OS');
  Result := 0;
end;
{$ENDIF win32}
{$ENDIF linux}


function TDebugProcess.GetDebugProcessRunning: Boolean;
begin
  Result := (FDbgProcess <> nil) and FDbgProcess.Running;
end;

constructor TDebugProcess.Create(const AExternalDebugger: String);
begin
  FDbgProcess := nil;
  FExternalDebugger := AExternalDebugger;
  SetLength(FTmpBuffer, DBG_STREAM_READ_LEN);
  inherited Create;
end;

destructor TDebugProcess.Destroy;
begin
  if DebugProcessRunning then
    StopDebugProcess;  // calls  FDbgProcess.Release;
  inherited Destroy;
end;

function TDebugProcess.CreateDebugProcess(const AOptions: String;
  AnEnvironment: TStrings): Boolean;
begin
  Result := False;
  if FDbgProcess = nil
  then begin
    FDbgProcess := TDebugAsyncProcess.Create(nil);
    try
      FDbgProcess.ParseCmdLine(FExternalDebugger + ' ' + AOptions);
      FDbgProcess.Options:= [poUsePipes, poNoConsole, poStdErrToOutPut, poNewProcessGroup];
      {$if defined(windows) and not defined(wince)}
      // under win9x and winMe should be created with console,
      // otherwise no break can be send.
      if Win32MajorVersion <= 4 then
        FDbgProcess.Options:= [poUsePipes, poNewConsole, poStdErrToOutPut, poNewProcessGroup];
      {$endif windows}
      FDbgProcess.ShowWindow := swoNone;
      FDbgProcess.Environment:= AnEnvironment;
    except
      FreeAndNil(FDbgProcess);
    end;
  end;
  if FDbgProcess = nil then exit;

  FDbgProcess.OnReadData := @DoReadData;
  FDbgProcess.OnTerminate := @DoTerminate;

  if not FDbgProcess.Running
  then begin
    try
      FDbgProcess.Execute;
      DebugLn(DBG_VERBOSE, '[TDebugProcess] Debug PID: ', IntToStr(FDbgProcess.Handle));
      Result := FDbgProcess.Running;
    except
      on E: Exception do begin
        FOutputBuf := E.Message;
        DebugLn(DBG_WARNINGS, 'Exception while executing debugger: ', FOutputBuf);
      end;
    end;
  end;

end;

procedure TDebugProcess.StopDebugProcess;
begin
debugln(['TDebugProcess.StopDebugProcess FDbgProcess = nil ',FDbgProcess = nil]);
  if FDbgProcess = nil then exit;

  FDbgProcess.Terminate(0);
  try
    FDbgProcess.Destroy;
  except
    on E: Exception do DebugLn(DBG_WARNINGS, 'Exception while freeing debugger: ', E.Message);
  end;
  FDbgProcess := nil;
end;

procedure TDebugProcess.SendCmdLn(const ACommand: String);
const
  LE: string = LineEnding;
begin
  if (DBG_CMD_ECHO_FULL <> nil) and (DBG_CMD_ECHO_FULL^.Enabled)
  then debugln(DBG_CMD_ECHO_FULL, '>> >> TDebugProcess.SendCmdLn "',ACommand,'"')
  else debugln(DBG_CMD_ECHO,      '>> >> TDebugProcess.SendCmdLn "',ACommand,'"');

  if DebugProcessRunning
  then begin
    if FOnLineSent <> nil then
      FOnLineSent(Self, ACommand);

    if ACommand <> ''
    then FDbgProcess.Input.Write(ACommand[1], Length(ACommand));
    FDbgProcess.Input.Write(LE[1], Length(LE));
  end
  else begin
    DebugLn(DBG_WARNINGS, '[TDebugProcess.SendCmdLn] Unable to send <', ACommand, '>. No process running.');
    if FOnSendError <> nil then
      FOnSendError(Self, ACommand);
  end;
end;

procedure TDebugProcess.SendCmdLn(const ACommand: String;
  Values: array of const);
begin
  SendCmdLn(Format(ACommand, Values));
end;

initialization
  DBG_CMD_ECHO      := DebugLogger.FindOrRegisterLogGroup('DBG_CMD_ECHO' {$IF defined(DBG_VERBOSE) or defined(DBG_CMD_ECHO)} , True {$ENDIF} );
  DBG_CMD_ECHO_FULL := DebugLogger.FindOrRegisterLogGroup('DBG_CMD_ECHO_FULL' {$IF defined(DBG_VERBOSE_FULL_DATA) or defined(DBG_CMD_ECHO_FULL)} , True {$ENDIF} );
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );

end.

