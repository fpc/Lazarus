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
{$IFDEF WIN32}
  Windows,
{$ENDIF}
{$IFDEF UNIX}
 {$IFDEF Ver1_0}
   Linux,
 {$ELSE}
   Unix,BaseUnix,
 {$ENDIF}     
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
begin      
  Result := 0;
  Max := 0;
  Count := High(AHandles);
  if Count < 0 then Exit;
  if Count > 31 then Count := 31;
  
  // zero the whole bit set of handles
  {$IFDEF Ver1_0}FD_ZERO{$ELSE}FpFD_ZERO{$ENDIF}(FDS);

  // set bits for all waiting handles
  for n := 0 to Count do   
  begin
    if Max < AHandles[n] then Max := AHandles[n];
    if AHandles[n] <> 0 then
      {$IFDEF Ver1_0}FD_Set{$ELSE}FpFD_Set{$ENDIF}(AHandles[n], FDS);
  end;
  if Max=0 then begin
    // no valid handle, so no change possible
    DebugLn('WaitForHandles: Error: no handles');
    exit;
  end;

  // wait for all handles
  repeat
    FDSWait := FDS;
    TimeOut := 10;
    // Select:
    // R = -1 on error, 0 on timeout, >0 on success and is number of handles
    // FDSWait is changed, and indicates what descriptors have changed
    R := {$IFDEF Ver1_0}Select{$ELSE}FpSelect{$ENDIF}(Max + 1, @FDSWait,
                                                      nil, nil, TimeOut);
    Application.ProcessMessages;
    if Application.Terminated then Break;
  until R <> 0;

  // set bits for all changed handles
  if R > 0 
  then begin
    for n := 0 to Count do   
      if  (AHandles[n] <> 0) 
      and {$IFDEF Ver1_0}
          FD_ISSET(AHandles[n],FDSWait)
          {$ELSE}
          (FpFD_ISSET(AHandles[n],FDSWait)=1)
          {$ENDIF}
      then begin
        Result := Result or 1 shl n;
        Dec(R);
        if R=0 then Break;
      end;
  end;
{$ELSE linux}
{$IFDEF WIN32}
var
  PipeHandle: Integer;
  TotalBytesAvailable: integer;
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
    FDbgProcess.Options:= [poUsePipes, {poNoConsole,} poStdErrToOutPut, poNewProcessGroup];
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
  inherited;
  try
    FDbgProcess.Free;
    FDbgProcess:=nil;
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
  until not DebugProcessRunning; 

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
{$ifdef win32}
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
{ =============================================================================
  $Log$
  Revision 1.37  2004/11/02 23:25:02  marc
  * Introduced another method of interrupting gdb on win32

  Revision 1.36  2004/10/25 17:59:29  vincents
  fpc 1.9.5 has no saveregisters calling convention anymore.

  Revision 1.35  2004/09/14 21:30:36  vincents
  replaced writeln by DebugLn

  Revision 1.34  2004/09/04 21:54:08  marc
  + Added option to skip compiler step on compile, build or run
  * Fixed adding of runtime watches
  * Fixed runnerror reporting (correct number and location is shown)

  Revision 1.33  2004/03/13 00:01:53  marc
  * fixed debugtarget PID parsing (for win32)

  Revision 1.32  2004/03/12 22:12:53  vincents
  fixed non-win32 compilation

  Revision 1.31  2004/03/12 21:39:29  vincents
  Lazarus can communicate with debugger on win32

  Revision 1.30  2004/03/08 09:55:41  marc
  * Fixed length on writing LineEnding

  Revision 1.29  2004/03/07 21:05:29  vincents
  WaitForHandles rewritten using PeekNamedPipe

  Revision 1.28  2004/02/12 01:09:42  marc
  + added the first conceptual code for WaitForHandles on Win32

  Revision 1.27  2004/01/17 13:29:04  mattias
  using now fpc constant LineEnding   from Vincent

  Revision 1.26  2004/01/05 15:22:42  mattias
  improved debugger: saved log, error handling in initialization, better reinitialize

  Revision 1.25  2003/12/08 14:27:16  mattias
  fixed WaitForHandles

  Revision 1.24  2003/10/31 15:14:43  mazen
  + added some paranthesis to avoid operators precedence problems

  Revision 1.23  2003/10/31 14:25:59  mazen
  * Fixing VER1_1 compile problem to allow using 1.1 compiler
  * Most of oldlinux unit calls are now in BaseUnix unit with prefix Fp

  Revision 1.22  2003/10/16 23:54:27  marc
  Implemented new gtk keyevent handling

  Revision 1.21  2003/08/15 14:28:48  mattias
  clean up win32 ifdefs

  Revision 1.20  2003/08/08 07:49:56  mattias
  fixed mem leaks in debugger

  Revision 1.19  2003/08/02 00:23:08  marc
  - removed accidently committed testcode

  Revision 1.18  2003/08/02 00:20:20  marc
  * fixed environment handling to debuggee

  Revision 1.17  2003/07/24 08:47:37  marc
  + Added SSHGDB debugger

  Revision 1.16  2003/06/17 23:13:06  marc
  * Canged Linux derective to unit, so it will work on xxxbsd?

  Revision 1.15  2003/05/27 20:58:12  mattias
  implemented enable and deleting breakpoint in breakpoint dlg

  Revision 1.14  2003/05/23 14:12:51  mattias
  implemented restoring breakpoints

  Revision 1.13  2002/08/28 11:41:52  lazarus
  MG: activated environment opts in debugger

  Revision 1.12  2002/08/28 10:44:44  lazarus
  MG: implemented run param environment variables

  Revision 1.11  2002/05/10 06:57:47  lazarus
  MG: updated licenses

  Revision 1.10  2002/04/30 15:57:39  lazarus
  MWE:
    + Added callstack object and dialog
    + Added checks to see if debugger = nil
    + Added dbgutils

  Revision 1.9  2002/04/24 20:42:29  lazarus
  MWE:
    + Added watches
    * Updated watches and watchproperty dialog to load as resource
    = renamed debugger resource files from *.lrc to *.lrs
    * Temporary fixed language problems on GDB (bug #508)
    * Made Debugmanager dialog handling more generic

  Revision 1.8  2002/03/23 15:54:30  lazarus
  MWE:
    + Added locals dialog
    * Modified breakpoints dialog (load as resource)
    + Added generic debuggerdlg class
    = Reorganized main.pp, all debbugger relater routines are moved
      to include/ide_debugger.inc

  Revision 1.7  2002/03/09 02:03:58  lazarus
  MWE:
    * Upgraded gdb debugger to gdb/mi debugger
    * Set default value for autpopoup
    * Added Clear popup to debugger output window

  Revision 1.6  2002/02/20 23:33:23  lazarus
  MWE:
    + Published OnClick for TMenuItem
    + Published PopupMenu property for TEdit and TMemo (Doesn't work yet)
    * Fixed debugger running twice
    + Added Debugger output form
    * Enabled breakpoints

  Revision 1.5  2002/02/06 08:58:29  lazarus
  MG: fixed compiler warnings and asking to create non existing files

  Revision 1.4  2002/02/05 23:16:48  lazarus
  MWE: * Updated tebugger
       + Added debugger to IDE

  Revision 1.3  2001/11/07 00:17:33  lazarus
  MWE: Added IFDEFs so non linux targetswill compile

  Revision 1.2  2001/11/06 23:59:12  lazarus
  MWE: + Initial breakpoint support
       + Added exeption handling on process.free

  Revision 1.1  2001/11/05 00:12:51  lazarus
  MWE: First steps of a debugger.


}
