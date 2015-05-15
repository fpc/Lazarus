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
  debugthread,
  DebugThreadCommand,
  DebugInOutputProcessor,
  DebugTCPServer,
  DebugConsoleServer;

type

  { TFPDServerApplication }

  TFPDServerApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure WriteHelp; virtual;
  end;

{ TFPDServerApplication }

procedure TFPDServerApplication.DoRun;
var
  ErrorMsg: String;
  DebugThread: TFpDebugThread;
  DebugEvent: TFpDebugEvent;
  TCPServerThread: TFpDebugTcpServer;
  ConsoleServerThread: TFpDebugConsoleServer;
  ACommand: TFpDebugThreadCommand;
  CommandStr: string;
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
  ConsoleServerThread := TFpDebugConsoleServer.Create(DebugThread);

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

  while not Terminated do
    begin
    try
      CheckSynchronize(100);
    except
      on e: exception do
        writeln(StdErr, 'Exception: '+e.Message);
    end;
    end;

  ConsoleServerThread.Terminate;
  TCPServerThread.StopListening;

  ConsoleServerThread.WaitFor;
  TCPServerThread.WaitFor;

  TCPServerThread.Free;
  ConsoleServerThread.Free;

  DebugThread.Terminate;
  DebugThread.WaitFor;
end;

constructor TFPDServerApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
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

