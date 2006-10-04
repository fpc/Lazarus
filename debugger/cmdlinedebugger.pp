{ $Id$ }
{                        ----------------------------------------------  
                         CMDLineDebugger.pp  -  Debugger class for 
                                                commandline debuggers
                         ---------------------------------------------- 
 
 @created(Wed Feb 28st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit CmdLineDebugger;

{$mode objfpc}
{$H+}

interface

uses
  Classes, Process, Debugger, LCLProc, Forms, LazConf, DBGUtils;

type
  TCmdLineDebugger = class(TDebugger)
  private
    FDbgProcess: TProcess;   // The process used to call the debugger
    FLineEnds: TStringList;  // List of strings considered as lineends
    FOutputBuf: String;
    FReading: Boolean;       // Set if we are in the ReadLine loop
    FFlushAfterRead: Boolean;// Set if we should flush after finished reading
    FPeekOffset: Integer;    // Count the number of lines we have peeked
    function GetDebugProcessRunning: Boolean;
  protected
    function  CreateDebugProcess(const AOptions: String): Boolean;
    procedure Flush;                                   // Flushes output buffer
    function  ReadLine: String; overload;
    function  ReadLine(const APeek: Boolean): String; overload;
    procedure SendCmdLn(const ACommand: String); overload;
    procedure SendCmdLn(const ACommand: String; Values: array of const); overload;
  public
    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;
    procedure TestCmd(const ACommand: String); virtual;// For internal debugging purposes
  public
    property DebugProcess: TProcess read FDbgProcess;
    property DebugProcessRunning: Boolean read GetDebugProcessRunning;
    property LineEnds: TStringList read FLineEnds;
  end;


implementation

//////////////////////////////////////////////////
//       Needs to go to proper include
//          Platform dependent
//////////////////////////////////////////////////

uses
{$IFdef MSWindows}
  Windows,
{$ENDIF}
{$IFDEF UNIX}
   Unix,BaseUnix,
{$ENDIF}
  SysUtils;
  
{------------------------------------------------------------------------------
  Function: WaitForHandles
  Params:  AHandles:              A set of handles to wait for (max 32)
  Returns: BitArray of handles set, 0 when an error occoured
 ------------------------------------------------------------------------------}
function WaitForHandles(const AHandles: array of Integer): Integer;
{$IFDEF UNIX}
var
  n, R, Max, Count: Integer;
  TimeOut: Integer;
  FDSWait, FDS: TFDSet;
  Step: Integer;
begin      
  Result := 0;
  Max := 0;
  Count := High(AHandles);
  if Count < 0 then Exit;
  if Count > 31 then Count := 31;
  
  // zero the whole bit set of handles
  FpFD_ZERO(FDS);

  // set bits for all waiting handles
  for n := 0 to Count do   
  begin
    if Max < AHandles[n] then Max := AHandles[n];
    if AHandles[n] <> 0 then
      FpFD_Set(AHandles[n], FDS);
  end;
  if Max=0 then begin
    // no valid handle, so no change possible
    DebugLn('WaitForHandles: Error: no handles');
    exit;
  end;

  // wait for all handles
  Step:=0;
  repeat
    FDSWait := FDS;
    TimeOut := 10;
    // Select:
    // R = -1 on error, 0 on timeout, >0 on success and is number of handles
    // FDSWait is changed, and indicates what descriptors have changed
    R := FpSelect(Max + 1, @FDSWait, nil, nil, TimeOut);
    inc(Step);
    if Step=50 then begin
      Step:=0;
      Application.Idle(false);
    end;
    Application.ProcessMessages;
    if Application.Terminated then Break;
  until R <> 0;

  // set bits for all changed handles
  if R > 0 
  then begin
    for n := 0 to Count do   
      if  (AHandles[n] <> 0) 
      and (FpFD_ISSET(AHandles[n],FDSWait)=1)
      then begin
        Result := Result or 1 shl n;
        Dec(R);
        if R=0 then Break;
      end;
  end;
{$ELSE linux}
{$IFdef MSWindows}
var
  PipeHandle: Integer;
  TotalBytesAvailable: dword;
  R: LongBool;
  n: integer;
begin
  Result := 0;

  while Result=0 do
  begin
    for n:= 0 to High(AHandles) do
    begin
      PipeHandle := AHandles[n];
      R := Windows.PeekNamedPipe(PipeHandle, nil, 0, nil, @TotalBytesAvailable, nil);
      if not R then begin
        // PeekNamedPipe failed
        DebugLn('PeekNamedPipe failed, GetLastError is ', IntToStr(GetLastError));
        Exit;
      end;
      if R then begin
        // PeekNamedPipe successfull
        if (TotalBytesAvailable>0) then begin
          Result := 1 shl n;
          Break;
        end;
      end;
    end;
    // process messages
    Application.ProcessMessages;
    if Application.Terminated then Break;
    // sleep a bit
    Sleep(10);
  end;
{$ELSE win32}
begin
  DebugLn('ToDo: implement WaitForHandles for this OS');
  Result := 0;
{$ENDIF win32}
{$ENDIF linux}
end;

//////////////////////////////////////////////////

{ TCmdLineDebugger }

constructor TCmdLineDebugger.Create(const AExternalDebugger: String);
begin
  FDbgProcess := nil;
  FLineEnds := TStringList.Create;
  FLineEnds.Add(LineEnding);
  FReading := False;
  FFlushAfterRead := False;
  FPeekOffset := 0;
  inherited;
end;

function TCmdLineDebugger.CreateDebugProcess(const AOptions: String): Boolean;
begin
  if FDbgProcess = nil
  then begin
    FDbgProcess := TProcess.Create(nil);
    FDbgProcess.CommandLine := ExternalDebugger + ' ' + AOptions;
    // TODO: under win9x and winMe should be created with console,
    // otherwise no break can be sent.
    FDbgProcess.Options:= [poUsePipes, poNoConsole, poStdErrToOutPut, poNewProcessGroup];
    FDbgProcess.ShowWindow := swoNone;
    FDbgProcess.Environment := DebuggerEnvironment;
  end;
  if not FDbgProcess.Running 
  then begin
    FDbgProcess.Execute;
    DebugLn('[TCmdLineDebugger] Debug PID: ', IntToStr(FDbgProcess.Handle));
  end;
  Result := FDbgProcess.Running;
end;

(*
function TCmdLineDebugger.CreateTargetProcess(const AName:String): Boolean;
begin
  // TODO: Better cleanup
  FTargetProcess.Free;
  FTargetProcess := TProcess.Create(nil);
  FTargetProcess.CommandLine := AName;
  FTargetProcess.Options:= [poUsePipes, poNoConsole, poRunSuspended, poStdErrToOutPut];
  FTargetProcess.ShowWindow := swoNone;
  FTargetProcess.Execute;
  WriteLN('[TCmdLineDebugger] Target PID = ', FTargetProcess.Handle);
  Result := FTargetProcess.Running;  
end;
*)

destructor TCmdLineDebugger.Destroy;
begin
  if (FDbgProcess <> nil) and (FDbgProcess.Running)
  then FDbgProcess.Terminate(0);
  
  inherited;
  
  try
    FreeAndNil(FDbgProcess);
  except
    on E: Exception do DebugLn('Exeption while freeing debugger: ', E.Message);
  end;
  FreeAndNil(FLineEnds);
end;

procedure TCmdLineDebugger.Flush;
begin
  if FReading
  then FFlushAfterRead := True
  else FOutputBuf := '';
end;

function TCmdLineDebugger.GetDebugProcessRunning: Boolean;
begin
  Result := (FDbgProcess <> nil) and FDbgProcess.Running;
end;

function TCmdLineDebugger.ReadLine: String;
begin
  Result := ReadLine(False);
end;

function TCmdLineDebugger.ReadLine(const APeek: Boolean): String;

  function ReadData(const AStream: TStream; var ABuffer: String): Integer;
  var
    S: String;
  begin
    SetLength(S, 1024);
    Result := AStream.Read(S[1], 1024);
    if Result > 0
    then begin
      SetLength(S, Result);
      ABuffer := ABuffer + S;
    end;
  end;

var   
  WaitSet: Integer;
  LineEndMatch: String;
  n, Idx, MinIdx, PeekCount: Integer;
begin                
//  WriteLN('[TCmdLineDebugger.GetOutput] Enter');

// TODO: get extra handles to wait for
// TODO: Fix multiple peeks
  if not APeek 
  then FPeekOffset := 0;
  FReading := True;
  PeekCount := 0;
  repeat                       
    if FOutputBuf <> ''
    then begin
      MinIdx := MaxInt;
      for n := 0 to FLineEnds.Count - 1 do
      begin
        LineEndMatch := FLineEnds[n];
        Idx := Pos(LineEndMatch, FOutputBuf);
        if (idx > 0) and (idx < MinIdx) 
        then MinIdx := idx;
      end;
    
      if MinIdx < MaxInt 
      then begin
        n := MinIdx + Length(LineEndMatch) - 1;
        Result := Copy(FOutputBuf, 1, n);
        if APeek 
        then begin
          if PeekCount = FPeekOffset
          then Inc(FPeekOffset)
          else begin
            Inc(PeekCount);
            Continue;
          end;
        end
        else Delete(FOutputBuf, 1, n);
      
        DoDbgOutput(Result);
        Break;
      end;
    end;

    WaitSet := WaitForHandles([FDbgProcess.Output.Handle]);
    if WaitSet = 0
    then begin
      SmartWriteln('[TCmdLineDebugger.Getoutput] Error waiting ');
      SetState(dsError);
      Break;
    end;
    
    if  ((WaitSet and 1) <> 0) 
    and (FDbgProcess <> nil)
    and (ReadData(FDbgProcess.Output, FOutputBuf) > 0) 
    then Continue; // start lineend search

(*
    if ((WaitSet and 2) <> 0) and (FTargetProcess <> nil)
    then begin
      Count := ReadData(FTargetProcess.Output, FTargetOutputBuf);
      if Count > 0
      then while True do
      begin
        Line := StripLN(GetLine(FTargetOutputBuf));
        if Line = '' then Break;
        DoOutput(Line); 
      end;
    end;
*)
    {$message warning condition should also check end-of-file reached for process output stream}
  until not DebugProcessRunning and (Length(FOutputBuf) = 0); 

  FReading := False;
  if FFlushAfterRead 
  then FOutputBuf := '';
  FFlushAfterRead := False;
  //writeln('TCmdLineDebugger.ReadLine returns ', result);
end;

procedure TCmdLineDebugger.SendCmdLn(const ACommand: String); overload;
begin
  //writeln('TCmdLineDebugger.SendCmdLn "',ACommand,'"');
  if DebugProcessRunning
  then begin
    DoDbgOutput('<' + ACommand + '>');
    if ACommand <> ''
    then FDbgProcess.Input.Write(ACommand[1], Length(ACommand));
{$ifdef MSWindows}
    FDbgProcess.Input.Write(LineEnding[1], Length(LineEnding));
{$else}
    FDbgProcess.Input.Write(LineEnding, Length(LineEnding));
{$endif}
  end
  else begin
    DebugLn('[TCmdLineDebugger.SendCmdLn] Unable to send <', ACommand, '>. No process running.');
    SetState(dsError);
  end;
end;

procedure TCmdLineDebugger.SendCmdLn(const ACommand: String; Values: array of const);
begin
  SendCmdLn(Format(ACommand, Values));
end;

procedure TCmdLineDebugger.TestCmd(const ACommand: String);
begin
  SendCmdLn(ACommand);
end;

initialization
end.
