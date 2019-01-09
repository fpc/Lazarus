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
  DebugConsoleServer,
  debugscriptserver;

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
  TCPServerThread: TFpDebugTcpServer;
  ScriptServerThread: TFpDebugScriptServer;
  ConsoleServerThread: TFpDebugConsoleServer;
  Port: integer;
  SensePorts: integer;
  ACommand: TFpDebugThreadCommand;
  ScriptFile: string;
  CommandStr: string;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hf:tdp:a::is:', ['help','filename:','tcp','daemon','port:','autoport::','interactive','script:'], True);

  if not HasOption('i','interactive') then
    begin
    writeln('FPDebug Server');
    writeln('Copyright (c) 2015 by Joost van der Sluis');
    end;

  if ErrorMsg<>'' then
    begin
    writeln(ErrorMsg);
    writeln('For more help, try: '+ExtractFileName(ExeName)+' -h');
    Terminate;
    Exit;
    end;

  if HasOption('h', 'help') then
    begin
    WriteHelp;
    Terminate;
    Exit;
    end;

  CommandStr := GetOptionValue('p','port');
  if CommandStr<>'' then
    begin
    if not TryStrToInt(CommandStr, Port) then
      begin
      writeln('Invalid port number '''+CommandStr+'''');
      Terminate;
      Exit;
      end;
    end
  else
    Port := 9159;

  if HasOption('a','autoport') then
    begin
    CommandStr := GetOptionValue('a','autoport');
    if CommandStr<>'' then
      begin
      if not TryStrToInt(CommandStr, SensePorts) then
        begin
        writeln('Autoport should be an integer number. Invalid autoport value '''+CommandStr+'''');
        Terminate;
        Exit;
        end;
      end
    else
      SensePorts:=5
    end
  else
    SensePorts:=1;

  DebugThread := TFpDebugThread.Instance;

  if not HasOption('d','daemon') then
    ConsoleServerThread := TFpDebugConsoleServer.Create(DebugThread)
  else
    ConsoleServerThread := nil;

  if HasOption('t','tcp') then
    TCPServerThread := TFpDebugTcpServer.Create(DebugThread, Port, SensePorts)
  else
    TCPServerThread := nil;

  if HasOption('i','interactive') then
    begin
    if assigned(TCPServerThread) then
      begin
      TCPServerThread.WaitForInitialization(Port);
      end
    else
      Port := -1;
    writeln(TJSonInOutputProcessor.InteractiveInitializationMessage(Port));
    FlushThread;
    end;

  ScriptFile := GetOptionValue('s','script');
  if ScriptFile<>'' then
    ScriptServerThread := TFpDebugScriptServer.create(DebugThread, ScriptFile)
  else
    ScriptServerThread := nil;

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
  if assigned(ScriptServerThread) then
    ScriptServerThread.Terminate;

  if assigned(ConsoleServerThread) then
    ConsoleServerThread.WaitFor;
  if assigned(TCPServerThread) then
    TCPServerThread.WaitFor;
  if assigned(ScriptServerThread) then
    ScriptServerThread.WaitFor;

  if assigned(TCPServerThread) then
    TCPServerThread.Free;
  if assigned(ConsoleServerThread) then
    ConsoleServerThread.Free;
  if assigned(ScriptServerThread) then
    ScriptServerThread.Free;

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
  writeln('fpdserver [options]');
  writeln(' List of options without argument:');
  writeln('  -h --help        Show this help message');
  writeln('  -t --tcp         Start listening to incoming tcp-connections');
  writeln('  -d --daemon      Do not use the console in- or output');
  writeln('  -i --interactive Run in interactive mode for automatic parsing');
  writeln(' List of options with argument:');
  writeln('  -f --filename    Set the filename of the executable to debug');
  writeln('  -p --port        Set the port (9159) to listen for incoming tcp-connections');
  writeln('  -a --autoport    Try to bind to n (5) sequential ports when a port is in use');
  writeln('  -s --script      Load script with debug-commands');
end;

var
  Application: TFPDServerApplication;
begin
  Application:=TFPDServerApplication.Create(nil);
  CustomApplication:=Application;
  Application.Title := 'FPD Server';
  Application.Run;
  Application.Free;
end.

