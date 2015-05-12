program fpdserver;

{ FPDebug server

  Copyright (C) 2015 Joost van der Sluis joost@cnoc.nl

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  syncobjs,
  pipes,
  lazfglhash,
  debugthread,
  DebugThreadCommand,
  lazCollections,
  DebugInOutputProcessor,
  DebugTCPServer;

type

  TFpDebugEventQueue = specialize TLazThreadedQueue<TFpDebugEvent>;

  { TFPDServerApplication }

  TFPDServerApplication = class(TCustomApplication, IFpDebugListener)
  private
    FEventQueue: TFpDebugEventQueue;
    FInOutputProcessor: TCustomInOutputProcessor;
    FConnectionIdentifier: integer;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    // IFpDebugListener
    procedure SendEvent(AnEvent: TFpDebugEvent);
    function GetOrigin: string;
  end;

{ TFPDServerApplication }

procedure TFPDServerApplication.DoRun;
var
  ErrorMsg: String;
  DebugThread: TFpDebugThread;
  DebugEvent: TFpDebugEvent;
  InputStream: TInputPipeStream;
  CommandStr: string;
  TCPServerThread: TFpDebugTcpServer;
  ACommand: TFpDebugThreadCommand;
  b: char;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hf', ['help']);
  if ErrorMsg<>'' then
    begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
    end;

  // parse parameters
  if HasOption('h', 'help') then
    begin
    WriteHelp;
    Terminate;
    Exit;
    end;

  DebugThread := TFpDebugThread.Instance;
  TCPServerThread := TFpDebugTcpServer.Create(DebugThread);

  if HasOption('f') then
    begin
    CommandStr := GetOptionValue('f');
    if CommandStr<>'' then
      begin
      ACommand := TFpDebugThreadSetFilenameCommand.create(-1, null, @DebugThread.SendLogMessage);
      TFpDebugThreadSetFilenameCommand(ACommand).Filename:=CommandStr;
      DebugThread.QueueCommand(ACommand);
      end
    else
      begin
      WriteHelp;
      Terminate;
      end;
    CommandStr:='';
    end;

  InputStream:=TInputPipeStream.Create(StdInputHandle);

  FConnectionIdentifier := DebugThread.AddListener(self);
  FInOutputProcessor := TJSonInOutputProcessor.create(FConnectionIdentifier, @DebugThread.SendLogMessage);
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
          if CommandStr='q' then
            Terminate
          else
            begin
            ACommand := FInOutputProcessor.TextToCommand(CommandStr);
            if assigned(ACommand) then
              DebugThread.QueueCommand(ACommand);
            end;
          CommandStr:='';
          end;
        end;
      CheckSynchronize;
      end;

    DebugThread.RemoveListener(self);

  finally
    FInOutputProcessor.Free;
  end;


  TCPServerThread.StopListening;
  TCPServerThread.WaitFor;
  TCPServerThread.Free;
  DebugThread.Terminate;
  DebugThread.WaitFor;
  InputStream.Free;

  terminate;
end;

constructor TFPDServerApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEventQueue:=TFpDebugEventQueue.Create(100, INFINITE, 100);
  StopOnException:=True;
end;

destructor TFPDServerApplication.Destroy;
begin
  FEventQueue.Free;
  inherited Destroy;
end;

procedure TFPDServerApplication.SendEvent(AnEvent: TFpDebugEvent);
begin
  FEventQueue.PushItem(AnEvent);
end;

function TFPDServerApplication.GetOrigin: string;
begin
  result := 'console';
end;

procedure TFPDServerApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' --help -h -f <executable name>');
end;

var
  Application: TFPDServerApplication;
begin
  Application:=TFPDServerApplication.Create(nil);
  Application.Title:='FPD Server';
  Application.Run;
  Application.Free;
end.

