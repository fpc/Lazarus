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
  ErrorMsg:=CheckOptions('hf:td', ['help','filename:','tcp','daemon'], True);
  if ErrorMsg<>'' then
    begin
    writeln('FPDebug server');
    writeln(ErrorMsg);
    Terminate;
    Exit;
    end;

  if HasOption('h', 'help') then
    begin
    WriteHelp;
    Terminate;
    Exit;
    end;

  DebugThread := TFpDebugThread.Instance;

  if HasOption('t','tcp') then
    TCPServerThread := TFpDebugTcpServer.Create(DebugThread)
  else
    TCPServerThread := nil;

  if not HasOption('d','daemon') then
    ConsoleServerThread := TFpDebugConsoleServer.Create(DebugThread)
  else
    ConsoleServerThread := nil;

  CommandStr := GetOptionValue('f', 'filename');
  if CommandStr<>'' then
    begin
    ACommand := TFpDebugThreadSetFilenameCommand.create(-1, null, @DebugThread.SendLogMessage);
    TFpDebugThreadSetFilenameCommand(ACommand).Filename:=CommandStr;
    DebugThread.QueueCommand(ACommand);
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

  if assigned(ConsoleServerThread) then
    ConsoleServerThread.Terminate;
  if assigned(TCPServerThread) then
    TCPServerThread.StopListening;

  if assigned(ConsoleServerThread) then
    ConsoleServerThread.WaitFor;
  if assigned(TCPServerThread) then
    TCPServerThread.WaitFor;

  if assigned(TCPServerThread) then
    TCPServerThread.Free;
  if assigned(ConsoleServerThread) then
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
  writeln('FPDebug server');
  writeln('Copyright (c) 2015 by Joost van der Sluis');
  writeln('fpdserver [options]');
  writeln(' List of options without argument:');
  writeln('  -h --help      Show this help message');
  writeln('  -t --tcp       Start listening to incoming tcp-connections');
  writeln('  -d --daemon    Do not use the console in- or output');
  writeln(' List of options with argument:');
  writeln('  -f --filename  Set the filename of the executable to debug');
end;

var
  Application: TFPDServerApplication;
begin
  Application:=TFPDServerApplication.Create(nil);
  Application.Title:='FPDebug Server';
  Application.Run;
  Application.Free;
end.

