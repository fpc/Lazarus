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
  Classes, Types, Process, FileUtil, Debugger, LCLProc, Forms, LazConf, DebugUtils;

type

  { TCmdLineDebugger }

  TCmdLineDebugger = class(TDebugger)
  private
    FDbgProcess: TProcess;   // The process used to call the debugger
    FLineEnds: TStringDynArray;  // List of strings considered as lineends
    FOutputBuf: String;
    FReading: Boolean;       // Set if we are in the ReadLine loop
    FFlushAfterRead: Boolean;// Set if we should flush after finished reading
    FPeekOffset: Integer;    // Count the number of lines we have peeked
    FReadLineTimedOut: Boolean;
    function WaitForHandles(const AHandles: array of Integer; var ATimeOut: Integer): Integer; overload;
    function WaitForHandles(const AHandles: array of Integer): Integer; overload;
  protected
    procedure DoReadError; virtual;
    procedure DoWriteError; virtual;
    function GetDebugProcessRunning: Boolean; virtual;
    procedure ProcessWhileWaitForHandles; virtual;
    function  CreateDebugProcess(const AOptions: String): Boolean; virtual;
    procedure Flush;                                   // Flushes output buffer
    function  GetWaiting: Boolean; override;
    function  ReadLine(ATimeOut: Integer = -1): String; overload;
    function  ReadLine(const APeek: Boolean; ATimeOut: Integer = -1): String; virtual; overload;
    procedure SendCmdLn(const ACommand: String); virtual; overload;
    procedure SendCmdLn(const ACommand: String; Values: array of const); overload;
    procedure SetLineEnds(ALineEnds: TStringDynArray);
    property ReadLineTimedOut: Boolean read FReadLineTimedOut;
  public
    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;
    procedure TestCmd(const ACommand: String); virtual;// For internal debugging purposes
  public
    property DebugProcess: TProcess read FDbgProcess;
    property DebugProcessRunning: Boolean read GetDebugProcessRunning;
  end;


implementation

//////////////////////////////////////////////////
//       Needs to go to proper include
//          Platform dependent
//////////////////////////////////////////////////

uses
  LCLIntf,
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
  TimeOut: Max Time in milli-secs => set to 0 if timeout occured
  Returns: BitArray of handles set, 0 when an error occoured
 ------------------------------------------------------------------------------}
function TCmdLineDebugger.WaitForHandles(const AHandles: array of Integer; var ATimeOut: Integer): Integer;
{$IFDEF UNIX}
var
  n, R, Max, Count: Integer;
  TimeOut: Integer;
  FDSWait, FDS: TFDSet;
  Step: Integer;
  t, t2, t3: DWord;
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

  if ATimeOut > 0
  then t := GetTickCount;

  // wait for all handles
  Step:=0;
  repeat
    FDSWait := FDS;
    TimeOut := 10;
    // Select:
    // R = -1 on error, 0 on timeout, >0 on success and is number of handles
    // FDSWait is changed, and indicates what descriptors have changed
    R := FpSelect(Max + 1, @FDSWait, nil, nil, TimeOut);

    if (ATimeOut > 0) then begin
      t2 := GetTickCount;
      if t2 < t
      then t3 := t2 + (High(t) - t)
      else t3 := t2 - t;
      if (t3 >= ATimeOut)
      then begin
        ATimeOut := 0;
        break;
      end
      else begin
        ATimeOut := ATimeOut - t3;
        t := t2;
      end;
    end;

    ProcessWhileWaitForHandles;
    inc(Step);
    if Step=50 then begin
      Step:=0;
      Application.Idle(false);
    end;
    try
      Application.ProcessMessages;
    except
      Application.HandleException(Application);
    end;
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
end;
{$ELSE linux}
{$IFdef MSWindows}
var
  PipeHandle: Integer;
  TotalBytesAvailable: dword;
  R: LongBool;
  n: integer;
  Step: Integer;
  t, t2, t3: DWord;
begin
  Result := 0;
  Step:=0;
  if ATimeOut > 0
  then t := GetTickCount;

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

    if (ATimeOut > 0) then begin
      t2 := GetTickCount;
      if t2 < t
      then t3 := t2 + (High(t) - t)
      else t3 := t2 - t;
      if (t3 >= ATimeOut)
      then begin
        ATimeOut := 0;
        break;
      end
      else begin
        ATimeOut := ATimeOut - t3;
        t := t2;
      end;
    end;

    ProcessWhileWaitForHandles;
    // process messages
    inc(Step);
    if Step=20 then begin
      Step:=0;
      Application.Idle(false);
    end;
    try
      Application.ProcessMessages;
    except
      Application.HandleException(Application);
    end;
    if Application.Terminated then Break;
    // sleep a bit
    Sleep(10);
  end;
end;
{$ELSE win32}
begin
  DebugLn('ToDo: implement WaitForHandles for this OS');
  Result := 0;
end;
{$ENDIF win32}
{$ENDIF linux}

function TCmdLineDebugger.WaitForHandles(const AHandles: array of Integer): Integer; overload;
var
  t: Integer;
begin
  t := -1;
  Result := WaitForHandles(AHandles, t);
end;

procedure TCmdLineDebugger.DoReadError;
begin
  SetState(dsError);
end;

procedure TCmdLineDebugger.DoWriteError;
begin
  SetState(dsError);
end;

procedure TCmdLineDebugger.ProcessWhileWaitForHandles;
begin
  // nothing
end;

//////////////////////////////////////////////////

{ TCmdLineDebugger }

constructor TCmdLineDebugger.Create(const AExternalDebugger: String);
begin
  FDbgProcess := nil;
  SetLength(FLineEnds, 1);
  FLineEnds[0] := LineEnding;
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
    FDbgProcess.CommandLine := UTF8ToSys(ExternalDebugger + ' ' + AOptions);
    // TODO: under win9x and winMe should be created with console,
    // otherwise no break can be sent.
    FDbgProcess.Options:= [poUsePipes, poNoConsole, poStdErrToOutPut, poNewProcessGroup];
    FDbgProcess.ShowWindow := swoNone;
    AssignUTF8ListToAnsi(DebuggerEnvironment,FDbgProcess.Environment);
  end;
  if not FDbgProcess.Running 
  then begin
    FDbgProcess.Execute;
    DebugLn('[TCmdLineDebugger] Debug PID: ', IntToStr(FDbgProcess.Handle));
  end;
  Result := FDbgProcess.Running;
end;

destructor TCmdLineDebugger.Destroy;
begin
  if (FDbgProcess <> nil) and (FDbgProcess.Running)
  then FDbgProcess.Terminate(0); //TODO: set state ?
  
  inherited;
  
  try
    FreeAndNil(FDbgProcess);
  except
    on E: Exception do DebugLn('Exeption while freeing debugger: ', E.Message);
  end;
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

function TCmdLineDebugger.GetWaiting: Boolean;
begin
  Result := FReading;
end;

function TCmdLineDebugger.ReadLine(ATimeOut: Integer = -1): String;
begin
  Result := ReadLine(False, ATimeOut);
end;

function TCmdLineDebugger.ReadLine(const APeek: Boolean; ATimeOut: Integer = -1): String;

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
  Result := '';
  FReadLineTimedOut := False;

  if not APeek
  then FPeekOffset := 0;
  FReading := True;
  PeekCount := 0;
  repeat                       
    if FOutputBuf <> ''
    then begin
      MinIdx := MaxInt;
      for n := Low(FLineEnds) to High(FLineEnds) do
      begin
        idx := Pos(FLineEnds[n], FOutputBuf);
        if (idx > 0) and (idx < MinIdx) 
        then begin
          MinIdx := idx;
          LineEndMatch := FLineEnds[n];
        end;
      end;
    
      if MinIdx < MaxInt 
      then begin
        Dec(MinIdx);
        Result := Copy(FOutputBuf, 1, MinIdx);
        if APeek 
        then begin
          if PeekCount = FPeekOffset
          then Inc(FPeekOffset)
          else begin
            Inc(PeekCount);
            Continue;
          end;
        end
        else Delete(FOutputBuf, 1, MinIdx + Length(LineEndMatch));
      
        DoDbgOutput(Result);
        Break;
      end;
    end;

    if FReadLineTimedOut
    then break;

    WaitSet := WaitForHandles([FDbgProcess.Output.Handle], ATimeOut);

    if (ATimeOut = 0)
    then FReadLineTimedOut := True;


    if (WaitSet = 0) and not FReadLineTimedOut
    then begin
      SmartWriteln('[TCmdLineDebugger.Getoutput] Error waiting ');
      DoReadError;
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
  {$IFDEF VerboseIDEToDo}{$message warning condition should also check end-of-file reached for process output stream}{$ENDIF}
  until not DebugProcessRunning and (Length(FOutputBuf) = 0); 

  FReading := False;
  if FFlushAfterRead 
  then FOutputBuf := '';
  FFlushAfterRead := False;
  //writeln('TCmdLineDebugger.ReadLine returns ', result);
  {$IFDEF DBG_VERBOSE}
  {$IFnDEF DBG_VERBOSE_FULL_DATA} if length(Result) < 300 then  {$ENDIF}
  debugln('<< TCmdLineDebugger.ReadLn "',Result,'"')
  {$IFnDEF DBG_VERBOSE_FULL_DATA}
  else  debugln(['<< TCmdLineDebugger.ReadLn "',copy(Result, 1, 200), '" ..(',length(Result)-250,').. "',copy(Result, length(Result)-99, 100),'"'])
  {$ENDIF}
  ;
  {$ENDIF}
end;

procedure TCmdLineDebugger.SendCmdLn(const ACommand: String); overload;
var
  LE: string[2];
begin
  //writeln('TCmdLineDebugger.SendCmdLn "',ACommand,'"');
  {$IFDEF DBG_VERBOSE}
  debugln('>> TCmdLineDebugger.SendCmdLn "',ACommand,'"');
  {$ENDIF}
  if DebugProcessRunning
  then begin
    DoDbgOutput('<' + ACommand + '>');
    if ACommand <> ''
    then FDbgProcess.Input.Write(ACommand[1], Length(ACommand));
    // store LineEnding in local variable, so the same statement can be used
    // for windows and *nix (1 or 2 character line ending)
    LE := LineEnding;
    FDbgProcess.Input.Write(LE[1], Length(LE));
  end
  else begin
    DebugLn('[TCmdLineDebugger.SendCmdLn] Unable to send <', ACommand, '>. No process running.');
    DoWriteError;
  end;
end;

procedure TCmdLineDebugger.SendCmdLn(const ACommand: String; Values: array of const);
begin
  SendCmdLn(Format(ACommand, Values));
end;

procedure TCmdLineDebugger.SetLineEnds(ALineEnds: TStringDynArray);
begin
  if Length(ALineEnds) = 0
  then begin
    SetLength(FLineEnds, 1);
    FLineEnds[0] := LineEnding;
  end
  else FLineEnds := ALineEnds;
end;

procedure TCmdLineDebugger.TestCmd(const ACommand: String);
begin
  SendCmdLn(ACommand);
end;

initialization
end.
