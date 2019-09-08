{ $Id: fpdbgwinclasses.pp 43410 2013-11-09 20:34:31Z martin $ }
{
 ---------------------------------------------------------------------------
 fpdbgwinclasses.pp  -  Native freepascal debugger
 ---------------------------------------------------------------------------

 This unit contains debugger classes for a native freepascal debugger

 ---------------------------------------------------------------------------

 @created(Sun Feb 9th WET 2014)
 @lastmod($Date: 2013-11-09 21:34:31 +0100 (za, 09 nov 2013) $)
 @author(Joost van der Sluis <joost@@cnoc.nl>)

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

(* About Windows debug events and breakpoints

  In a multi-threaded app, several threads can all reach breakpoints (the same
  or different breakpoints) at the same time.
  Windows will report each such breakpoint in an event on its own.

  When the first Breakpoint event is received, it is not possible to tell which
  other threads have also hit breakpoints.
  - A thread that has hit a breakpoint will have its Instruction-Pointer exactly
    one after the int3 break instruction.
  - But a thread could also be in that location as a result of a jump. (If the
    int3 replaced another 1 byte instruction)
  As a consequence: While all threads are stopped due to the first thread having
  hit a breakpoint, the Instruction pointer for the other threads may be
  wrong/unusable. It may need correction by -1, if that other thread also already
  hit a breakpoint. [1]

  If the debugger resumes after a breakpoint, it must temporarily remove the
  breakpoint, so the original instruction can be executed. (There is an option
  to do "out of place execution", but that is not implemented, and may not always
  be available)
  In order to execute the original instruction (while the int3 is removed):
  - The thread must do a single-step. This ensures it can not loop back and
    execute the instruction again, when it should hit the breakpoint again (after
    looping back)
  - Other threads must be suspended, so they can not run to/through the location
    of the breakpoint. Otherwise they would miss the breakpoint, as the int3 is
    removed,
    Other threads may/should execute, if they previously started a single step.

  The debugger may also skip a breakpoint (for the current thread) that is next
  to be hit, even if it had no event yet.
  The controller should have seen that the thread was at the breakpoint location,
  and should have triggered the actions for the breakpoint.

  If several events (such a breakpoints) have been raised at the same time (e.g.
  several breakpoints hit), then those events will be reported.
  => They will be reported, even if their thread got suspended in the meantime.
     (Since the event had already happened, no code execution happens in such a
     suspended thread.)
  However that means, if the debugger want thread A to do a single step over a
  (temp removed) breakpoint, then the next event for the debugger could be an
  already pending signal (other breakpoint or other event).
  In that case, the single step, may not yet have been executed, and will only
  happen if the debugger calls ContinueDebugEvent for the current event.
  But the debugger is not allowed to run the current thread, because the int3
  for thread A is still temporary removed.
  The debugger can run the thread, if it single steps it. Otherwise it can
  suspend it before calling ContinueDebugEvent (TODO if that does not work, it
  must revert to single step).

  The pending single step thread will remember its single step flag. So it just
  needs to be kept un-suspended for the next ContinueDebugEvent.

  [1] TODO (may or may not work):
  It may be possible to get the other events using Win10 DBG_REPLY_LATER
  (or setting the IP back to the breakpoint, and hit it again).
  Then while *all* threads are suspended, events can be collected.
  If no more events are coming in, the original thread can be resumed, triggering
  its breakpoint event again.
  All the event, would need to be collected, and each would need to be answered
  with a ContinueDebugEvent to windows.
  And only when all events are known AND the debugger has not yet called
  ContinueDebugEvent for the last event (so the target app is paused), then they
  would be reported (one by one) to the user.

*)
unit FpDbgWinClasses;

{$mode objfpc}{$H+}
{$DEFINE DebuglnWinDebugEvents}

interface

uses
  Classes,
  SysUtils,
  Windows,
  FpDbgUtil,
  FpDbgClasses,
  process,
  FpDbgWinExtra,
  strutils,
  FpDbgInfo,
  FpDbgLoader,
  DbgIntfBaseTypes,
  LazLoggerBase, UTF8Process;

type

  { TDbgWinThread }

  TDbgWinThread = class(TDbgThread)
  private
    FIsSuspended: Boolean;
    FIsSkippingBreakPoint: Boolean;
    FIsSkippingBreakPointAddress: TDBGPtr;
  protected
    FThreadContextChanged: boolean;
    FCurrentContext: PContext; // FCurrentContext := Pointer((PtrUInt(@_UnAligendContext) + 15) and not PtrUInt($F));
    _UnAligendContext: record
      C: TContext;
      dummy: array[1..16] of byte;
    end;
    procedure LoadRegisterValues; override;
  public
    procedure Suspend;
    procedure SuspendForStepOverBreakPoint;
    procedure Resume;
    procedure SetSingleStepOverBreakPoint;
    procedure EndSingleStepOverBreakPoint;
    procedure SetSingleStep;
    function AddWatchpoint(AnAddr: TDBGPtr): integer; override;
    function RemoveWatchpoint(AnId: integer): boolean; override;
    procedure BeforeContinue; override;
    function ResetInstructionPointerAfterBreakpoint: boolean; override;
    function ReadThreadState: boolean;

    function GetInstructionPointerRegisterValue: TDbgPtr; override;
    function GetStackBasePointerRegisterValue: TDbgPtr; override;
    function GetStackPointerRegisterValue: TDbgPtr; override;
    property Process;
  end;

  { TDbgWinProcess }

  TDbgWinProcess = class(TDbgProcess)
  private
    FInfo: TCreateProcessDebugInfo;
    FPauseRequested: boolean;
    FProcProcess: TProcessUTF8;
    FJustStarted: boolean;
    function GetFullProcessImageName(AProcessHandle: THandle): string;
    function GetModuleFileName(AModuleHandle: THandle): string;
    function GetProcFilename(AProcess: TDbgProcess; lpImageName: LPVOID; fUnicode: word; hFile: handle): string;
    procedure LogLastError;
  protected
    procedure AfterChangingInstructionCode(const ALocation: TDBGPtr); override;
    function GetHandle: THandle; override;
    function GetLastEventProcessIdentifier: THandle; override;
    procedure InitializeLoaders; override;
  public
    destructor Destroy; override;

    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; override;
    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; override;
    function ReadString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: String): Boolean; override;
    function ReadWString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: WideString): Boolean; override;

    procedure Interrupt;
    function  HandleDebugEvent(const ADebugEvent: TDebugEvent): Boolean;

    class function StartInstance(AFileName: string; AParams, AnEnvironment: TStrings; AWorkingDirectory, AConsoleTty: string; AFlags: TStartInstanceFlags): TDbgProcess; override;
    function Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean; override;
    function WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean; override;
    function AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent; override;
    function CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread; override;

    procedure StartProcess(const AThreadID: DWORD; const AInfo: TCreateProcessDebugInfo);

    function Pause: boolean; override;

    procedure TerminateProcess; override;

    function AddrOffset: Int64; override;
    function  AddLib(const AInfo: TLoadDLLDebugInfo): TDbgLibrary;
    procedure RemoveLib(const AInfo: TUnloadDLLDebugInfo);
  end;

  { tDbgWinLibrary }

  tDbgWinLibrary = class(TDbgLibrary)
  private
    FInfo: TLoadDLLDebugInfo;
  protected
    procedure InitializeLoaders; override;
  public
    constructor Create(const AProcess: TDbgProcess; const ADefaultName: String;
      const AModuleHandle: THandle; const ABaseAddr: TDbgPtr; AInfo: TLoadDLLDebugInfo);
  end;


procedure RegisterDbgClasses;

implementation

var
  DBG_VERBOSE, DBG_WARNINGS: PLazLoggerLogGroup;

{$ifdef cpux86_64}
const
  FLAG_TRACE_BIT = $100;
{$endif}

function dbgs(AnDbgEvent: DEBUG_EVENT): String; overload;
begin
  case AnDbgEvent.dwDebugEventCode of
    CREATE_PROCESS_DEBUG_EVENT: result := '>> CREATE_PROCESS_DEBUG_EVENT'
      + ' htproc:' + IntToStr(AnDbgEvent.CreateProcessInfo.hProcess);
    CREATE_THREAD_DEBUG_EVENT:  result := '>> CREATE_THREAD_DEBUG_EVENT'
      + ' hthread:' + IntToStr(AnDbgEvent.CreateThread.hThread)
      + ' start:' + dbghex(PtrUInt(AnDbgEvent.CreateThread.lpStartAddress));
    EXCEPTION_DEBUG_EVENT: begin
                                result := 'EXCEPTION_DEBUG_EVENT'
        + ' Code:' + dbghex(AnDbgEvent.Exception.ExceptionRecord.ExceptionCode)
        + ' Flags:' + dbghex(AnDbgEvent.Exception.ExceptionRecord.ExceptionFlags)
        + ' NumParam:' + IntToStr(AnDbgEvent.Exception.ExceptionRecord.NumberParameters);
      case AnDbgEvent.Exception.ExceptionRecord.ExceptionCode of
         EXCEPTION_ACCESS_VIOLATION:         Result := Result + ' EXCEPTION_ACCESS_VIOLATION';
         EXCEPTION_BREAKPOINT:               Result := Result + ' EXCEPTION_BREAKPOINT';
         EXCEPTION_DATATYPE_MISALIGNMENT:    Result := Result + ' EXCEPTION_DATATYPE_MISALIGNMENT';
         EXCEPTION_SINGLE_STEP:              Result := Result + ' EXCEPTION_SINGLE_STEP';
         EXCEPTION_ARRAY_BOUNDS_EXCEEDED:    Result := Result + ' EXCEPTION_ARRAY_BOUNDS_EXCEEDED';
         EXCEPTION_FLT_DENORMAL_OPERAND:     Result := Result + ' EXCEPTION_FLT_DENORMAL_OPERAND';
         EXCEPTION_FLT_DIVIDE_BY_ZERO:       Result := Result + ' EXCEPTION_FLT_DIVIDE_BY_ZERO';
         EXCEPTION_FLT_INEXACT_RESULT:       Result := Result + ' EXCEPTION_FLT_INEXACT_RESULT';
         EXCEPTION_FLT_INVALID_OPERATION:    Result := Result + ' EXCEPTION_FLT_INVALID_OPERATION';
         EXCEPTION_FLT_OVERFLOW:             Result := Result + ' EXCEPTION_FLT_OVERFLOW';
         EXCEPTION_FLT_STACK_CHECK:          Result := Result + ' EXCEPTION_FLT_STACK_CHECK';
         EXCEPTION_FLT_UNDERFLOW:            Result := Result + ' EXCEPTION_FLT_UNDERFLOW';
         EXCEPTION_INT_DIVIDE_BY_ZERO:       Result := Result + ' EXCEPTION_INT_DIVIDE_BY_ZERO';
         EXCEPTION_INT_OVERFLOW:             Result := Result + ' EXCEPTION_INT_OVERFLOW';
         EXCEPTION_INVALID_HANDLE:           Result := Result + ' EXCEPTION_INVALID_HANDLE';
         EXCEPTION_PRIV_INSTRUCTION:         Result := Result + ' EXCEPTION_PRIV_INSTRUCTION';
         EXCEPTION_NONCONTINUABLE_EXCEPTION: Result := Result + ' EXCEPTION_NONCONTINUABLE_EXCEPTION';
         EXCEPTION_NONCONTINUABLE:           Result := Result + ' EXCEPTION_NONCONTINUABLE';
         EXCEPTION_STACK_OVERFLOW:           Result := Result + ' EXCEPTION_STACK_OVERFLOW';
         EXCEPTION_INVALID_DISPOSITION:      Result := Result + ' EXCEPTION_INVALID_DISPOSITION';
         EXCEPTION_IN_PAGE_ERROR:            Result := Result + ' EXCEPTION_IN_PAGE_ERROR';
         EXCEPTION_ILLEGAL_INSTRUCTION:      Result := Result + ' EXCEPTION_ILLEGAL_INSTRUCTION';
         EXCEPTION_POSSIBLE_DEADLOCK:        Result := Result + ' EXCEPTION_POSSIBLE_DEADLOCK';
      end;
    end;
    EXIT_PROCESS_DEBUG_EVENT:   result := '<< EXIT_PROCESS_DEBUG_EVENT'
      + ' exitcode:' + IntToStr(AnDbgEvent.ExitProcess.dwExitCode);
    EXIT_THREAD_DEBUG_EVENT:    result := '<< EXIT_THREAD_DEBUG_EVENT'
      + ' exitcode:' + IntToStr(AnDbgEvent.ExitThread.dwExitCode);
    LOAD_DLL_DEBUG_EVENT:       result := '> LOAD_DLL_DEBUG_EVENT';
    OUTPUT_DEBUG_STRING_EVENT:  result := 'OUTPUT_DEBUG_STRING_EVENT';
    UNLOAD_DLL_DEBUG_EVENT:     result := '< UNLOAD_DLL_DEBUG_EVENT';
    RIP_EVENT:                  result := 'RIP_EVENT'
      + ' type:' + IntToStr(AnDbgEvent.RipInfo.dwType)
      + ' err:' + IntToStr(AnDbgEvent.RipInfo.dwError);
    else                        result := 'Code='+inttostr(AnDbgEvent.dwDebugEventCode);
  end;
  Result := format('EVENT for Process %d Thread %d: %s', [AnDbgEvent.dwProcessId, AnDbgEvent.dwThreadId, Result]);
end;

procedure RegisterDbgClasses;
begin
  OSDbgClasses.DbgThreadClass:=TDbgWinThread;
  OSDbgClasses.DbgBreakpointClass:=TFpInternalBreakpoint;
  OSDbgClasses.DbgProcessClass:=TDbgWinProcess;
end;

procedure TDbgWinProcess.LogLastError;
begin
  if not GotExitProcess then
    DebugLn(DBG_WARNINGS, 'FpDbg-ERROR: %s', [GetLastErrorText]);
end;

procedure TDbgWinProcess.AfterChangingInstructionCode(const ALocation: TDBGPtr);
begin
  inherited AfterChangingInstructionCode(ALocation);
  FlushInstructionCache(Handle, Pointer(PtrUInt(ALocation)), 1);
end;

function QueryFullProcessImageName(hProcess:HANDLE; dwFlags: DWord; lpExeName:LPWSTR; var lpdwSize:DWORD):BOOL; stdcall; external 'kernel32' name 'QueryFullProcessImageNameW';

function TDbgWinProcess.GetFullProcessImageName(AProcessHandle: THandle): string;
var
  u: UnicodeString;
  len: DWORD;
begin
  len := MAX_PATH;
  SetLength(u, len);
  if QueryFullProcessImageName(AProcessHandle, 0, @u[1], len)
  then begin
    SetLength(u, len);
    Result:=UTF8Encode(u);
  end else begin
    Result := '';
    LogLastError;
  end;
end;

function TDbgWinProcess.GetModuleFileName(AModuleHandle: THandle): string;
var
  u: UnicodeString;
  s: string;
  len: Integer;
  hMod: THandle;
  _GetFinalPathNameByHandle: function(hFile: HANDLE; lpFilename:LPWSTR; cchFilePath, dwFlags: DWORD):DWORD; stdcall;
begin
  result := '';

  // normally you would load a lib, but since kernel32 is
  // always loaded we can use this (and we don't have to free it
  hMod := GetModuleHandle(kernel32);
  if hMod = 0 then Exit; //????

  // GetFinalPathNameByHandle is only available on Windows Vista / Server 2008
  _GetFinalPathNameByHandle := nil;
  pointer(_GetFinalPathNameByHandle) := GetProcAddress(hMod, 'GetFinalPathNameByHandleW');
  if assigned(_GetFinalPathNameByHandle) then begin
    SetLength(u, MAX_PATH);

    len := _GetFinalPathNameByHandle(AModuleHandle, @u[1], MAX_PATH, 0);
    s:='';
    if len > 0
    then begin
      SetLength(u, len - 1);
      if (u<>'') and (u[length(u)]=#0) then
      begin
        // On some older Windows versions there's a bug in GetFinalPathNameByHandleW,
        // which leads to a trailing #0.
        Delete(u,length(u),1);
      end;
      s:=UTF8Encode(u);
    end else begin
      u := '';
      LogLastError;
    end;
    // Remove the \\?\ prefix
    Delete(S,1,4);
    result := S;
  end;
end;

function TDbgWinProcess.GetProcFilename(AProcess: TDbgProcess; lpImageName: LPVOID; fUnicode: word; hFile: handle): string;
var
  NamePtr: TDbgPtr;
  S: String;
  W: WideString;
begin
  S := '';
  if (lpImageName<>nil) and AProcess.ReadOrdinal(TDbgPtr(lpImageName), NamePtr)
  then begin
    if fUnicode <> 0
    then begin
      if AProcess.ReadWString(NamePtr, MAX_PATH, W)
      then S := W;
    end
    else begin
      AProcess.ReadString(NamePtr, MAX_PATH, S);
    end;
  end;

  if S = ''
  then begin
    if hFile=0 then
      S := GetFullProcessImageName(AProcess.Handle)
    else
      S := GetModuleFileName(hFile);
  end;
  result := S;
end;

{ tDbgWinLibrary }

procedure tDbgWinLibrary.InitializeLoaders;
begin
  TDbgImageLoader.Create(FInfo.hFile).AddToLoaderList(LoaderList);
end;

constructor tDbgWinLibrary.Create(const AProcess: TDbgProcess;
  const ADefaultName: String; const AModuleHandle: THandle;
  const ABaseAddr: TDbgPtr; AInfo: TLoadDLLDebugInfo);
var
  S: String;
begin
  inherited Create(AProcess, ADefaultName, AModuleHandle, ABaseAddr);
  FInfo := AInfo;

  s := TDbgWinProcess(AProcess).GetProcFilename(AProcess, AInfo.lpImageName, AInfo.fUnicode, AInfo.hFile);
  if s <> ''
  then SetFileName(s);

  LoadInfo;
end;

{ TDbgWinProcess }

function TDbgWinProcess.GetHandle: THandle;
begin
  Result:=FInfo.hProcess;
end;

function TDbgWinProcess.GetLastEventProcessIdentifier: THandle;
begin
  Result:= MDebugEvent.LoadDll.hFile;
end;

procedure TDbgWinProcess.InitializeLoaders;
begin
  TDbgImageLoader.Create(FInfo.hFile).AddToLoaderList(LoaderList);
end;

destructor TDbgWinProcess.Destroy;
begin
  FInfo.hProcess:=0;
  FProcProcess.Free;
  inherited Destroy;
end;

function TDbgWinProcess.ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean;
var
  BytesRead: PtrUInt;
begin
  Result := ReadProcessMemory(Handle, Pointer(PtrUInt(AAdress)), @AData, ASize, BytesRead) and (BytesRead = ASize);

  if not Result then LogLastError;
  MaskBreakpointsInReadData(AAdress, ASize, AData);
end;

function TDbgWinProcess.WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean;
var
  BytesWritten: PtrUInt;
begin
  Result := WriteProcessMemory(Handle, Pointer(PtrUInt(AAdress)), @AData, ASize, BytesWritten) and (BytesWritten = ASize);

  if not Result then LogLastError;
end;

function TDbgWinProcess.ReadString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: String): Boolean;
var
  BytesRead: PtrUInt;
  buf: array of Char;
begin
  AData := '';
  SetLength(buf, AMaxSize + 1);
  Result := ReadProcessMemory(Handle, Pointer(PtrUInt(AAdress)), @Buf[0], AMaxSize, BytesRead);
  if not Result then Exit;
  if BytesRead < AMaxSize
  then Buf[BytesRead] := #0
  else Buf[AMaxSize] := #0;
  AData := PChar(@Buf[0]);
end;

function TDbgWinProcess.ReadWString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: WideString): Boolean;
var
  BytesRead: PtrUInt;
  buf: array of WChar;
begin
  AData := '';
  SetLength(buf, AMaxSize + 1);
  Result := ReadProcessMemory(Handle, Pointer(PtrUInt(AAdress)), @Buf[0], SizeOf(WChar) * AMaxSize, BytesRead);
  if not Result then Exit;
  BytesRead := BytesRead div SizeOf(WChar);
  if BytesRead < AMaxSize
  then Buf[BytesRead] := #0
  else Buf[AMaxSize] := #0;
  AData := PWChar(@Buf[0]);
end;

procedure TDbgWinProcess.Interrupt;
var
  _UC: record
    C: TContext;
    D: array[1..16] of Byte;
  end;
  Context: PContext;
begin
  // Interrupting is implemented by suspending the thread and set DB0 to the
  // (to be) executed EIP. When the thread is resumed, it will generate a break
  // Single stepping doesn't work in all cases.

  // A context needs to be aligned to 16 bytes. Unfortunately, the compiler has
  // no directive for this, so align it somewhere in our "reserved" memory
  Context := AlignPtr(@_UC, $10);
  SuspendThread(FInfo.hThread);
  try
    Context^.ContextFlags := CONTEXT_CONTROL or CONTEXT_DEBUG_REGISTERS;
    if not GetThreadContext(FInfo.hThread, Context^)
    then begin
      DebugLn(DBG_WARNINGS, 'Proces %u interrupt: Unable to get context', [ProcessID]);
      Exit;
    end;

    Context^.ContextFlags := CONTEXT_DEBUG_REGISTERS;
    {$ifdef cpui386}
    Context^.Dr0 := Context^.Eip;
    {$else}
    Context^.Dr0 := Context^.Rip;
    {$endif}
    Context^.Dr7 := (Context^.Dr7 and $FFF0FFFF) or $1;

    if not SetThreadContext(FInfo.hThread, Context^)
    then begin
      DebugLn(DBG_WARNINGS, 'Proces %u interrupt: Unable to set context', [ProcessID]);
      Exit;
    end;
  finally
    ResumeTHread(FInfo.hThread);
  end;
end;

{ ------------------------------------------------------------------
  HandleDebugEvent

  Result: True if the event was triggered internally
          The callee should continue the process
  ------------------------------------------------------------------ }
function TDbgWinProcess.HandleDebugEvent(const ADebugEvent: TDebugEvent): Boolean;

  //function DoSingleStep: Boolean;
  //var
  //  _UC: record
  //    C: TContext;
  //    D: array[1..16] of Byte;
  //  end;
  //  Context: PContext;
  //begin
  //  Result := False;
  //  // check if we are interupting
  //  Context := AlignPtr(@_UC, $10);
  //  Context^.ContextFlags := CONTEXT_DEBUG_REGISTERS;
  //  if GetThreadContext(FInfo.hThread, Context^)
  //  then begin
  //    if Context^.Dr6 and 1 <> 0
  //    then begin
  //      // interrupt !
  //      // disable break.
  //      Context^.Dr7 := Context^.Dr7 and not $1;
  //      Context^.Dr0 := 0;
  //      if not SetThreadContext(FInfo.hThread, Context^)
  //      then begin
  //        // Heeellppp!!
  //        DebugLn(DBG_WARNINGS, 'Thread %u: Unable to reset BR0', [ADebugEvent.dwThreadId]);
  //      end;
  //      // check if we are also singlestepping
  //      // if not, then exit, else proceed to next check
  //      if Context^.Dr6 and $40 = 0
  //      then Exit;
  //    end;
  //  end
  //  else begin
  //    // if we can not get the context, we probable weren't able to set it either
  //    DebugLn(DBG_WARNINGS, 'Thread %u: Unable to get context', [ADebugEvent.dwThreadId]);
  //  end;
  //
  //  // check if we are single stepping ourself
  //  if FCurrentBreakpoint = nil then Exit;
  //
  //  FCurrentBreakpoint.SetBreak;
  //  FCurrentBreakpoint := nil;
  //  Result := FReEnableBreakStep;
  //  FReEnableBreakStep := False;
  //end;

begin
  Result := False;
  case ADebugEvent.dwDebugEventCode of
    EXIT_THREAD_DEBUG_EVENT: begin
      // The thread event will be freed later, may still be used
      // will be freed, in "TDbgWinProcess.Continue"
      // This relies on the thread being removed, to be the same as FCurrentThread in FPDbgController
      RemoveThread(ADebugEvent.dwThreadId);
    end;
    LOAD_DLL_DEBUG_EVENT: begin
      AddLib(ADebugEvent.LoadDll);
    end;
    UNLOAD_DLL_DEBUG_EVENT: begin
      RemoveLib(ADebugEvent.UnloadDll);
    end;
  end;
end;

class function TDbgWinProcess.StartInstance(AFileName: string; AParams,
  AnEnvironment: TStrings; AWorkingDirectory, AConsoleTty: string;
  AFlags: TStartInstanceFlags): TDbgProcess;
var
  AProcess: TProcessUTF8;
begin
  result := nil;
  AProcess := TProcessUTF8.Create(nil);
  try
    AProcess.Options:=[poDebugProcess, poNewProcessGroup];
    if siForceNewConsole in AFlags then
      AProcess.Options:=AProcess.Options+[poNewConsole];
    AProcess.Executable:=AFilename;
    AProcess.Parameters:=AParams;
    AProcess.Environment:=AnEnvironment;
    AProcess.CurrentDirectory:=AWorkingDirectory;
    AProcess.Execute;

    result := TDbgWinProcess.Create(AFileName, AProcess.ProcessID, AProcess.ThreadID);
    TDbgWinProcess(result).FProcProcess := AProcess;
  except
    on E: Exception do
    begin
      {$ifdef cpui386}
      if (E is EProcess) and (GetLastError=50) then
      begin
        DebugLn(DBG_WARNINGS, 'Failed to start process "%s". Note that on Windows it is not possible to debug a 64-bit application with a 32-bit debugger.'+sLineBreak+'Errormessage: "%s".',[AFileName, E.Message]);
      end
      else
      {$endif i386}
        DebugLn(DBG_WARNINGS, 'Failed to start process "%s". Errormessage: "%s".',[AFileName, E.Message]);
      AProcess.Free;
    end;
  end;
end;


function TDbgWinProcess.Continue(AProcess: TDbgProcess; AThread: TDbgThread;
  SingleStep: boolean): boolean;
var
  EventThread, t: TDbgThread;
  b: Boolean;
begin
debugln(['TDbgWinProcess.Continue ',SingleStep]);
  if assigned(AThread) and not FThreadMap.HasId(AThread.ID) then begin
    AThread.Free;
    AThread := nil;
  end;

  (* In case a thread needs to single-step over a (temp-removed) breakpoint,
     other events (from suspended threads, if the event is already triggered)
     can be received. THe single step must be continued until finished.
     This may mean suspending the current thread.
  *)

  if AProcess.GetThread(MDebugEvent.dwThreadId, EventThread) then begin
    if EventThread = AThread then
      EventThread.NextIsSingleStep := SingleStep;

    if HasInsertedBreakInstructionAtLocation(EventThread.GetInstructionPointerRegisterValue) then begin
debugln(['## skip brkpoint ',AThread= EventThread, '  iss ',EventThread.NextIsSingleStep]);
      TDbgWinThread(EventThread).SetSingleStepOverBreakPoint;
      for t in FThreadMap do
        TDbgWinThread(t).SuspendForStepOverBreakPoint;
    end
    else begin
      // EventThread does not need to skip a breakpoint;
      b := False;
      for t in FThreadMap do
        if TDbgWinThread(t).FIsSkippingBreakPoint then begin
          b := True;
          break;
        end;

      if b then begin
debugln(['## skip brkpoint (others only) ',AThread= EventThread, '  iss ',EventThread.NextIsSingleStep]);
        // But other threads are still skipping
        for t in FThreadMap do
          if not (SingleStep and (t = AThread)) then   // allow athread to single-step
            TDbgWinThread(t).SuspendForStepOverBreakPoint;
      end;
    end;

    AThread := nil; // Already handled, might be suspended
  end;

  if assigned(AThread) then
  begin
    AThread.NextIsSingleStep:=SingleStep;
    if SingleStep then
      TDbgWinThread(AThread).SetSingleStep;
  end;
  AProcess.ThreadsBeforeContinue;
if AThread<>nil then debugln(['## ath.iss ',AThread.NextIsSingleStep]);

  if MDebugEvent.dwDebugEventCode = EXCEPTION_DEBUG_EVENT then
    case MDebugEvent.Exception.ExceptionRecord.ExceptionCode of
     EXCEPTION_BREAKPOINT,
     EXCEPTION_SINGLE_STEP: begin
       Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE);
     end
    else
      Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_EXCEPTION_NOT_HANDLED);
    end
  else
    Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE);
  result := true;
end;

function TDbgWinProcess.WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean;
var
  t: TDbgWinThread;
begin
  result := Windows.WaitForDebugEvent(MDebugEvent, INFINITE);
  ProcessIdentifier:=MDebugEvent.dwProcessId;
  ThreadIdentifier:=MDebugEvent.dwThreadId;
  {$IFDEF DebuglnWinDebugEvents}
  DebugLn([dbgs(MDebugEvent), ' ', Result]);
  for TDbgThread(t) in FThreadMap do begin
  if t.ReadThreadState then
    DebugLn('Thr.Id:%d  SSTep %s EF %s     DR6:%x  WP:%x  RegAcc: %d,  SStep: %d  Task: %d, ExcBrk: %d', [t.ID, dbgs(t.FCurrentContext^.EFlags and FLAG_TRACE_BIT), dbghex(t.FCurrentContext^.EFlags), t.FCurrentContext^.Dr6, t.FCurrentContext^.Dr6 and 15, t.FCurrentContext^.Dr6 and (1<< 13), t.FCurrentContext^.Dr6 and (1<< 14), t.FCurrentContext^.Dr6 and (1<< 15), t.FCurrentContext^.Dr6 and (1<< 16)]);
  end;
  {$ENDIF}

  RestoreTempBreakInstructionCodes;
  for TDbgThread(t) in FThreadMap do
    t.Resume;

  // Should be done in AnalyseDebugEvent, but that is not called for forked processes
  if (MDebugEvent.dwDebugEventCode = CREATE_PROCESS_DEBUG_EVENT) and
     (MDebugEvent.dwProcessId <> ProcessID) and
     (MDebugEvent.CreateProcessInfo.hFile <> 0)
  then begin
    CloseHandle(MDebugEvent.CreateProcessInfo.hFile);
    MDebugEvent.CreateProcessInfo.hFile := 0;
  end;
end;

function TDbgWinProcess.AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent;

  procedure HandleException(const AEvent: TDebugEvent; out InterceptAtFirstChance: Boolean);
  const
    PARAMCOLS = 12 - SizeOf(Pointer);
  var
    Info0: QWORD;
    Info1: QWORD;
    Info1Str: String;
    ExInfo32: TExceptionDebugInfo32 absolute AEvent.Exception;
    ExInfo64: TExceptionDebugInfo64 absolute AEvent.Exception;
  begin
    InterceptAtFirstChance := True;
    // Kept the debug-output as comments, since they provide deeper information
    // on how to interprete the exception-information.
    {
    if AEvent.Exception.dwFirstChance = 0
    then DebugLn(DBG_VERBOSE, 'Exception: ')
    else DebugLn(DBG_VERBOSE, 'First chance exception: ');
    }
    // in both 32 and 64 case is the exceptioncode the first, so no difference
    case AEvent.Exception.ExceptionRecord.ExceptionCode of
      EXCEPTION_ACCESS_VIOLATION         : ExceptionClass:='ACCESS VIOLATION';
      EXCEPTION_ARRAY_BOUNDS_EXCEEDED    : ExceptionClass:='ARRAY BOUNDS EXCEEDED';
      EXCEPTION_BREAKPOINT               : ExceptionClass:='BREAKPOINT';  // should never be here
      EXCEPTION_DATATYPE_MISALIGNMENT    : ExceptionClass:='DATATYPE MISALIGNMENT';
      EXCEPTION_FLT_DENORMAL_OPERAND     : ExceptionClass:='FLT DENORMAL OPERAND';
      EXCEPTION_FLT_DIVIDE_BY_ZERO       : ExceptionClass:='FLT DIVIDE BY ZERO';
      EXCEPTION_FLT_INEXACT_RESULT       : ExceptionClass:='FLT INEXACT RESULT';
      EXCEPTION_FLT_INVALID_OPERATION    : ExceptionClass:='FLT INVALID OPERATION';
      EXCEPTION_FLT_OVERFLOW             : ExceptionClass:='FLT OVERFLOW';
      EXCEPTION_FLT_STACK_CHECK          : ExceptionClass:='FLT STACK CHECK';
      EXCEPTION_FLT_UNDERFLOW            : ExceptionClass:='FLT UNDERFLOW';
      EXCEPTION_ILLEGAL_INSTRUCTION      : ExceptionClass:='ILLEGAL INSTRUCTION';
      EXCEPTION_IN_PAGE_ERROR            : ExceptionClass:='IN PAGE ERROR';
      EXCEPTION_INT_DIVIDE_BY_ZERO       : ExceptionClass:='INT DIVIDE BY ZERO';
      EXCEPTION_INT_OVERFLOW             : ExceptionClass:='INT OVERFLOW';
      EXCEPTION_INVALID_DISPOSITION      : ExceptionClass:='INVALID DISPOSITION';
      EXCEPTION_INVALID_HANDLE           : ExceptionClass:='INVALID HANDLE';
      EXCEPTION_NONCONTINUABLE_EXCEPTION : ExceptionClass:='NONCONTINUABLE EXCEPTION';
      EXCEPTION_POSSIBLE_DEADLOCK        : ExceptionClass:='POSSIBLE DEADLOCK';
      EXCEPTION_PRIV_INSTRUCTION         : ExceptionClass:='PRIV INSTRUCTION';
      EXCEPTION_SINGLE_STEP              : ExceptionClass:='SINGLE STEP';    // should never be here
      EXCEPTION_STACK_OVERFLOW           : ExceptionClass:='STACK OVERFLOW';

      // add some status - don't know if we can get them here
      {
      DBG_EXCEPTION_NOT_HANDLED          : DebugLn(DBG_VERBOSE, 'DBG_EXCEPTION_NOT_HANDLED');
      STATUS_GUARD_PAGE_VIOLATION        : DebugLn(DBG_VERBOSE, 'STATUS_GUARD_PAGE_VIOLATION');
      STATUS_NO_MEMORY                   : DebugLn(DBG_VERBOSE, 'STATUS_NO_MEMORY');
      STATUS_CONTROL_C_EXIT              : DebugLn(DBG_VERBOSE, 'STATUS_CONTROL_C_EXIT');
      STATUS_FLOAT_MULTIPLE_FAULTS       : DebugLn(DBG_VERBOSE, 'STATUS_FLOAT_MULTIPLE_FAULTS');
      STATUS_FLOAT_MULTIPLE_TRAPS        : DebugLn(DBG_VERBOSE, 'STATUS_FLOAT_MULTIPLE_TRAPS');
      STATUS_REG_NAT_CONSUMPTION         : DebugLn(DBG_VERBOSE, 'STATUS_REG_NAT_CONSUMPTION');
      STATUS_SXS_EARLY_DEACTIVATION      : DebugLn(DBG_VERBOSE, 'STATUS_SXS_EARLY_DEACTIVATION');
      STATUS_SXS_INVALID_DEACTIVATION    : DebugLn(DBG_VERBOSE, 'STATUS_SXS_INVALID_DEACTIVATION');
      }
    else
      InterceptAtFirstChance := False;
      ExceptionClass := 'Unknown exception code $' + IntToHex(ExInfo32.ExceptionRecord.ExceptionCode, 8);
      {
      DebugLn(DBG_VERBOSE, ' [');
      case ExInfo32.ExceptionRecord.ExceptionCode and $C0000000 of
        STATUS_SEVERITY_SUCCESS       : DebugLn(DBG_VERBOSE, 'SEVERITY_ERROR');
        STATUS_SEVERITY_INFORMATIONAL : DebugLn(DBG_VERBOSE, 'SEVERITY_ERROR');
        STATUS_SEVERITY_WARNING       : DebugLn(DBG_VERBOSE, 'SEVERITY_WARNING');
        STATUS_SEVERITY_ERROR         : DebugLn(DBG_VERBOSE, 'SEVERITY_ERROR');
      end;
      if ExInfo32.ExceptionRecord.ExceptionCode and $20000000 <> 0
      then DebugLn (DBG_VERBOSE, ' Customer');
      if ExInfo32.ExceptionRecord.ExceptionCode and $10000000 <> 0
      then DebugLn (DBG_VERBOSE, ' Reserved');
      case (ExInfo32.ExceptionRecord.ExceptionCode and $0FFF0000) shr 16 of
        FACILITY_DEBUGGER            : DebugLn(DBG_VERBOSE, 'FACILITY_DEBUGGER');
        FACILITY_RPC_RUNTIME         : DebugLn(DBG_VERBOSE, 'FACILITY_RPC_RUNTIME');
        FACILITY_RPC_STUBS           : DebugLn(DBG_VERBOSE, 'FACILITY_RPC_STUBS');
        FACILITY_IO_ERROR_CODE       : DebugLn(DBG_VERBOSE, 'FACILITY_IO_ERROR_CODE');
        FACILITY_TERMINAL_SERVER     : DebugLn(DBG_VERBOSE, 'FACILITY_TERMINAL_SERVER');
        FACILITY_USB_ERROR_CODE      : DebugLn(DBG_VERBOSE, 'FACILITY_USB_ERROR_CODE');
        FACILITY_HID_ERROR_CODE      : DebugLn(DBG_VERBOSE, 'FACILITY_HID_ERROR_CODE');
        FACILITY_FIREWIRE_ERROR_CODE : DebugLn(DBG_VERBOSE, 'FACILITY_FIREWIRE_ERROR_CODE');
        FACILITY_CLUSTER_ERROR_CODE  : DebugLn(DBG_VERBOSE, 'FACILITY_CLUSTER_ERROR_CODE');
        FACILITY_ACPI_ERROR_CODE     : DebugLn(DBG_VERBOSE, 'FACILITY_ACPI_ERROR_CODE');
        FACILITY_SXS_ERROR_CODE      : DebugLn(DBG_VERBOSE, 'FACILITY_SXS_ERROR_CODE');
      else
        DebugLn(DBG_VERBOSE, ' Facility: $', IntToHex((ExInfo32.ExceptionRecord.ExceptionCode and $0FFF0000) shr 16, 3));
      end;
      DebugLn(DBG_VERBOSE, ' Code: $', IntToHex((ExInfo32.ExceptionRecord.ExceptionCode and $0000FFFF), 4));
      }
    end;
    ExceptionClass:='External: '+ExceptionClass;
    ExceptionMessage:='';
    {
    if GMode = dm32
    then Info0 := PtrUInt(ExInfo32.ExceptionRecord.ExceptionAddress)
    else Info0 := PtrUInt(ExInfo64.ExceptionRecord.ExceptionAddress);
    DebugLn(DBG_VERBOSE, ' at: ', FormatAddress(Info0));
    DebugLn(DBG_VERBOSE, ' Flags:', Format('%x', [AEvent.Exception.ExceptionRecord.ExceptionFlags]), ' [');

    if AEvent.Exception.ExceptionRecord.ExceptionFlags = 0
    then DebugLn(DBG_VERBOSE, 'Continuable')
    else DebugLn(DBG_VERBOSE, 'Not continuable');
    DebugLn(DBG_VERBOSE, ']');
    if GMode = dm32
    then DebugLn(DBG_VERBOSE, ' ParamCount:', IntToStr(ExInfo32.ExceptionRecord.NumberParameters))
    else DebugLn(DBG_VERBOSE, ' ParamCount:', IntToStr(ExInfo64.ExceptionRecord.NumberParameters));
    }
    case AEvent.Exception.ExceptionRecord.ExceptionCode of
      EXCEPTION_ACCESS_VIOLATION: begin
        if GMode = dm32
        then begin
          Info0 := ExInfo32.ExceptionRecord.ExceptionInformation[0];
          Info1 := ExInfo32.ExceptionRecord.ExceptionInformation[1];
        end
        else begin
          Info0 := ExInfo64.ExceptionRecord.ExceptionInformation[0];
          Info1 := ExInfo64.ExceptionRecord.ExceptionInformation[1];
        end;
        Info1Str := FormatAddress(Info1);

        case Info0 of
          EXCEPTION_READ_FAULT:    ExceptionMessage := 'Access violation reading from address ' + Info1Str +'.';
          EXCEPTION_WRITE_FAULT:   ExceptionMessage := 'Access violation writing to address ' + Info1Str +'.';
          EXCEPTION_EXECUTE_FAULT: ExceptionMessage := 'Access violation executing address ' + Info1Str +'.';
        end;
      end;
    end;
    {
    DebugLn(DBG_VERBOSE, ' Info: ');
    for n := 0 to EXCEPTION_MAXIMUM_PARAMETERS - 1 do
    begin
      if GMode = dm32
      then Info0 := ExInfo32.ExceptionRecord.ExceptionInformation[n]
      else Info0 := ExInfo64.ExceptionRecord.ExceptionInformation[n];
      DebugLn(DBG_VERBOSE, IntToHex(Info0, DBGPTRSIZE[GMode] * 2), ' ');
      if n and (PARAMCOLS - 1) = (PARAMCOLS - 1)
      then begin
        DebugLn(DBG_VERBOSE, '');
        DebugLn(DBG_VERBOSE, '       ');
      end;
    end;
    DebugLn(DBG_VERBOSE, '');
    }
  end;

  procedure DumpEvent(const AEvent: String);
  var
    f: Cardinal;
    n: integer;
  begin
    if (DBG_VERBOSE = nil) or (not DBG_VERBOSE^.Enabled) then
      exit;
    DebugLn('===');
    DebugLn(AEvent);
    DebugLn('---');
    DebugLn('Process ID: '+ IntToSTr(MDebugEvent.dwProcessId));
    DebugLn('Thread ID: '+ IntToStr(MDebugEvent.dwThreadId));

    if AThread = nil then Exit;
    if TDbgWinThread(AThread).FCurrentContext = nil then Exit;

{$PUSH}{$R-}
    {$ifdef cpui386}
    with TDbgWinThread(AThread).FCurrentContext^ do DebugLn(Format('DS: 0x%x, ES: 0x%x, FS: 0x%x, GS: 0x%x', [SegDs, SegEs, SegFs, SegGs]));
    with TDbgWinThread(AThread).FCurrentContext^ do DebugLn(Format('EAX: 0x%x, EBX: 0x%x, ECX: 0x%x, EDX: 0x%x, EDI: 0x%x, ESI: 0x%x', [Eax, Ebx, Ecx, Edx, Edi, Esi]));
    with TDbgWinThread(AThread).FCurrentContext^ do DebugLn(Format('CS: 0x%x, SS: 0x%x, EBP: 0x%x, EIP: 0x%x, ESP: 0x%x, EFlags: 0x%x [', [SegCs, SegSs, Ebp, Eip, Esp, EFlags]));
    {$else}
    with TDbgWinThread(AThread).FCurrentContext^ do DebugLn(Format('SegDS: 0x%4.4x, SegES: 0x%4.4x, SegFS: 0x%4.4x, SegGS: 0x%4.4x', [SegDs, SegEs, SegFs, SegGs]));
    with TDbgWinThread(AThread).FCurrentContext^ do DebugLn(Format('RAX: 0x%16.16x, RBX: 0x%16.16x, RCX: 0x%16.16x, RDX: 0x%16.16x, RDI: 0x%16.16x, RSI: 0x%16.16x, R9: 0x%16.16x, R10: 0x%16.16x, R11: 0x%16.16x, R12: 0x%16.16x, R13: 0x%16.16x, R14: 0x%16.16x, R15: 0x%16.16x', [Rax, Rbx, Rcx, Rdx, Rdi, Rsi, R9, R10, R11, R12, R13, R14, R15]));
    with TDbgWinThread(AThread).FCurrentContext^ do DebugLn(Format('SegCS: 0x%4.4x, SegSS: 0x%4.4x, RBP: 0x%16.16x, RIP: 0x%16.16x, RSP: 0x%16.16x, EFlags: 0x%8.8x [', [SegCs, SegSs, Rbp, Rip, Rsp, EFlags]));
    {$endif}
    // luckely flag and debug registers are named the same
    with TDbgWinThread(AThread).FCurrentContext^ do
    begin
      if EFlags and (1 shl 0) <> 0 then DebugLn('CF ');
      if EFlags and (1 shl 2) <> 0 then DebugLn('PF ');
      if EFlags and (1 shl 4) <> 0 then DebugLn('AF ');
      if EFlags and (1 shl 6) <> 0 then DebugLn('ZF ');
      if EFlags and (1 shl 7) <> 0 then DebugLn('SF ');
      if EFlags and (1 shl 8) <> 0 then DebugLn('TF ');
      if EFlags and (1 shl 9) <> 0 then DebugLn('IF ');
      if EFlags and (1 shl 10) <> 0 then DebugLn('DF ');
      if EFlags and (1 shl 11) <> 0 then DebugLn('OF ');
      if (EFlags shr 12) and 3 <> 0 then DebugLn('IOPL=', IntToSTr((EFlags shr 12) and 3));
      if EFlags and (1 shl 14) <> 0 then DebugLn('NT ');
      if EFlags and (1 shl 16) <> 0 then DebugLn('RF ');
      if EFlags and (1 shl 17) <> 0 then DebugLn('VM ');
      if EFlags and (1 shl 18) <> 0 then DebugLn('AC ');
      if EFlags and (1 shl 19) <> 0 then DebugLn('VIF ');
      if EFlags and (1 shl 20) <> 0 then DebugLn('VIP ');
      if EFlags and (1 shl 21) <> 0 then DebugLn('ID ');
      DebugLn(']');

      DebugLn(Format('DR0: 0x%x, DR1: 0x%x, DR2: 0x%x, DR3: 0x%x', [Dr0, Dr1, Dr2, Dr3]));
      DebugLn(' DR6: 0x', IntToHex(Dr6, SizeOf(Pointer) * 2), ' [');
      if Dr6 and $0001 <> 0 then DebugLn('B0 ');
      if Dr6 and $0002 <> 0 then DebugLn('B1 ');
      if Dr6 and $0004 <> 0 then DebugLn('B2 ');
      if Dr6 and $0008 <> 0 then DebugLn('B3 ');
      if Dr6 and $2000 <> 0 then DebugLn('BD ');
      if Dr6 and $4000 <> 0 then DebugLn('BS ');
      if Dr6 and $8000 <> 0 then DebugLn('BT ');
      DebugLn('] DR7: 0x', IntToHex(Dr7, SizeOf(Pointer) * 2), ' [');
      if Dr7 and $01 <> 0 then DebugLn('L0 ');
      if Dr7 and $02 <> 0 then DebugLn('G0 ');
      if Dr7 and $04 <> 0 then DebugLn('L1 ');
      if Dr7 and $08 <> 0 then DebugLn('G1 ');
      if Dr7 and $10 <> 0 then DebugLn('L2 ');
      if Dr7 and $20 <> 0 then DebugLn('G2 ');
      if Dr7 and $40 <> 0 then DebugLn('L3 ');
      if Dr7 and $80 <> 0 then DebugLn('G3 ');
      if Dr7 and $100 <> 0 then DebugLn('LE ');
      if Dr7 and $200 <> 0 then DebugLn('GE ');
      if Dr7 and $2000 <> 0 then DebugLn('GD ');
      f := Dr7 shr 16;
      for n := 0 to 3 do
      begin
        DebugLn('R/W', IntToSTr(n),':');
        case f and 3 of
          0: DebugLn('ex');
          1: DebugLn('wo');
          2: DebugLn('IO');
          3: DebugLn('rw');
        end;
        f := f shr 2;
        DebugLn(' LEN', IntToSTr(n),':', IntToSTr(f and 3 + 1), ' ');
        f := f shr 2;
      end;
      DebugLn(']');
    end;
    DebugLn('---');
  {$POP}
  end;

  procedure HandleOutputDebug(const AEvent: TDebugEvent);
  var
    S: String;
    W: WideString;
  begin
    if AEvent.DebugString.fUnicode <> 0
    then begin
      if not ReadWString(TDbgPtr(AEvent.DebugString.lpDebugStringData), AEvent.DebugString.nDebugStringLength, W)
      then Exit;
      S := W;
    end
    else begin
      if not ReadString(TDbgPtr(AEvent.DebugString.lpDebugStringData), AEvent.DebugString.nDebugStringLength, S)
      then Exit;
    end;
    DebugLn(DBG_VERBOSE, '[%d:%d]: %s', [AEvent.dwProcessId, AEvent.dwThreadId, S]);
  end;

var
  InterceptAtFirst: Boolean;
begin
  TDbgWinThread(AThread).EndSingleStepOverBreakPoint;
  if HandleDebugEvent(MDebugEvent)
  then result := deBreakpoint // unreachable
  else begin
    if AThread <> nil
    then begin
      // TODO: move to TDbgThread
      DebugLn(DBG_WARNINGS and (not TDbgWinThread(AThread).ReadThreadState), 'LOOP: Unable to retrieve thread context');
    end;

    case MDebugEvent.dwDebugEventCode of
      EXCEPTION_DEBUG_EVENT: begin
        //DumpEvent('EXCEPTION_DEBUG_EVENT');
        case MDebugEvent.Exception.ExceptionRecord.ExceptionCode of
          EXCEPTION_BREAKPOINT: begin
            if FJustStarted and (MDebugEvent.Exception.dwFirstChance <> 0) and (MDebugEvent.Exception.ExceptionRecord.ExceptionFlags = 0) then
            begin
              FJustStarted:=false;
              result := deInternalContinue;
            end
            else begin
              result := deBreakpoint;
              AThread.CheckAndResetInstructionPointerAfterBreakpoint;
            end;
          end;
          EXCEPTION_SINGLE_STEP: begin
            result := deBreakpoint;
          end
        else begin
          HandleException(MDebugEvent, InterceptAtFirst);
          if (MDebugEvent.Exception.dwFirstChance = 1) and (not InterceptAtFirst) then
            result := deInternalContinue // might be an SEH exception
          else
            result := deException;
        end;
        end;
      end;
      CREATE_THREAD_DEBUG_EVENT: begin
        //DumpEvent('CREATE_THREAD_DEBUG_EVENT');
        result := deInternalContinue;
      end;
      CREATE_PROCESS_DEBUG_EVENT: begin
        //DumpEvent('CREATE_PROCESS_DEBUG_EVENT');
        if MDebugEvent.dwProcessId = TDbgWinThread(AThread).Process.ProcessID then begin
          //main process
          StartProcess(MDebugEvent.dwThreadId, MDebugEvent.CreateProcessInfo); // hfile will be closed by TDbgImageLoader
          FJustStarted := true;
          result := deCreateProcess;
        end
        else begin
          //child process: ignore
          // we currently do not use the file handle => close it
          if MDebugEvent.CreateProcessInfo.hFile <> 0 then
            CloseHandle(MDebugEvent.CreateProcessInfo.hFile);
          result := deInternalContinue;
        end;
      end;
      EXIT_THREAD_DEBUG_EVENT: begin
        //DumpEvent('EXIT_THREAD_DEBUG_EVENT');
        result := deInternalContinue;
      end;
      EXIT_PROCESS_DEBUG_EVENT: begin
        //DumpEvent('EXIT_PROCESS_DEBUG_EVENT');
        SetExitCode(MDebugEvent.ExitProcess.dwExitCode);
        // Let the kernel close all debug-handles and close-up the
        // debuggee.
        Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE);
        result := deExitProcess;
      end;
      LOAD_DLL_DEBUG_EVENT: begin
        //DumpEvent('LOAD_DLL_DEBUG_EVENT');
        result := deLoadLibrary;
      end;
      UNLOAD_DLL_DEBUG_EVENT: begin
        //DumpEvent('UNLOAD_DLL_DEBUG_EVENT');
        result := deInternalContinue;
      end;
      OUTPUT_DEBUG_STRING_EVENT: begin
        //DumpEvent('OUTPUT_DEBUG_STRING_EVENT');
        HandleOutputDebug(MDebugEvent);
        result := deInternalContinue;
      end;
      RIP_EVENT: begin
        //DumpEvent('RIP_EVENT');
        result := deInternalContinue;
      end
      else begin
        raise Exception.CreateFmt('Unknown dwDebugEventCode value %d',[MDebugEvent.dwDebugEventCode]);
      end;
    end;

  end;
end;

function TDbgWinProcess.CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread;
begin
  case MDebugEvent.dwDebugEventCode of
    CREATE_THREAD_DEBUG_EVENT :
      begin
      result := OSDbgClasses.DbgThreadClass.Create(Self, AThreadIdentifier, MDebugEvent.CreateThread.hThread);
      IsMainThread := false;
      end;
    CREATE_PROCESS_DEBUG_EVENT :
      begin
      result := OSDbgClasses.DbgThreadClass.Create(Self, AThreadIdentifier, MDebugEvent.CreateProcessInfo.hThread);
      IsMainThread := true;
      end
  else
    result := nil;
  end; {case}
end;

procedure TDbgWinProcess.StartProcess(const AThreadID: DWORD;const AInfo: TCreateProcessDebugInfo);
var
  s: string;
begin
  FInfo := AInfo;

  s := GetProcFilename(Self, AInfo.lpImageName, AInfo.fUnicode, 0);
  if s <> ''
  then SetFileName(s);
end;

function DebugBreakProcess(Process:HANDLE): WINBOOL; external 'kernel32' name 'DebugBreakProcess';
var
  DebugBreakAddr: Pointer = nil;
  _CreateRemoteThread: function(hProcess: THandle; lpThreadAttributes: Pointer; dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer; dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall = nil;

procedure InitWin32;
var
  hMod: THandle;
begin
  // Check if we already are initialized
  if DebugBreakAddr <> nil then Exit;

  // normally you would load a lib, but since kernel32 is
  // always loaded we can use this (and we don't have to free it
  hMod := GetModuleHandle(kernel32);
  if hMod = 0 then Exit; //????

  DebugBreakAddr := GetProcAddress(hMod, 'DebugBreak');
  Pointer(_CreateRemoteThread) := GetProcAddress(hMod, 'CreateRemoteThread');
end;

function TDbgWinProcess.Pause: boolean;
var
  hndl: Handle;
  hThread: THandle;
  NewThreadId: Cardinal;
begin
  //hndl := OpenProcess(PROCESS_CREATE_THREAD or PROCESS_QUERY_INFORMATION or PROCESS_VM_OPERATION or PROCESS_VM_WRITE or PROCESS_VM_READ, False, TargetPID);
  hndl := OpenProcess(PROCESS_ALL_ACCESS, false, ProcessID);
  FPauseRequested:=true;
  result := DebugBreakProcess(hndl);
  if not Result then begin
    DebugLn(DBG_WARNINGS, ['pause failed(1) ', GetLastError]);
    InitWin32;
    hThread := _CreateRemoteThread(hndl, nil, 0, DebugBreakAddr, nil, 0, NewThreadId);
    if hThread = 0 then begin
      DebugLn(DBG_WARNINGS, ['pause failed(2) ', GetLastError]);
    end
    else begin
      Result := True;
      CloseHandle(hThread);
    end;
  end;
  CloseHandle(hndl);
end;

procedure TDbgWinProcess.TerminateProcess;
begin
  Windows.TerminateProcess(Handle, 0);
end;

function TDbgWinProcess.AddrOffset: Int64;
begin
  Result:=0;//inherited AddrOffset - TDbgPtr(FInfo.lpBaseOfImage);
end;

function TDbgWinProcess.AddLib(const AInfo: TLoadDLLDebugInfo): TDbgLibrary;
var
  ID: TDbgPtr;
begin
  Result := TDbgWinLibrary.Create(Self, HexValue(AInfo.lpBaseOfDll, SizeOf(Pointer), [hvfIncludeHexchar]), AInfo.hFile, TDbgPtr(AInfo.lpBaseOfDll), AInfo);
  ID := TDbgPtr(AInfo.lpBaseOfDll);
  FLibMap.Add(ID, Result);
  if Result.DbgInfo.HasInfo
  then FSymInstances.Add(Result);
end;

procedure TDbgWinProcess.RemoveLib(const AInfo: TUnloadDLLDebugInfo);
var
  Lib: TDbgLibrary;
  ID: TDbgPtr;
begin
  if FLibMap = nil then Exit;
  ID := TDbgPtr(AInfo.lpBaseOfDll);
  if not FLibMap.GetData(ID, Lib) then Exit;
  if Lib.DbgInfo.HasInfo
  then FSymInstances.Remove(Lib);
  FLibMap.Delete(ID);
  Lib.Free;
end;

{ TDbgWinThread }

procedure TDbgWinThread.LoadRegisterValues;
begin
  if FCurrentContext = nil then
    ReadThreadState;
  {$ifdef cpui386}
  with FCurrentContext^ do
  begin
    FRegisterValueList.DbgRegisterAutoCreate['eax'].SetValue(Eax, IntToStr(Eax),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['ecx'].SetValue(Ecx, IntToStr(Ecx),4,1);
    FRegisterValueList.DbgRegisterAutoCreate['edx'].SetValue(Edx, IntToStr(Edx),4,2);
    FRegisterValueList.DbgRegisterAutoCreate['ebx'].SetValue(Ebx, IntToStr(Ebx),4,3);
    FRegisterValueList.DbgRegisterAutoCreate['esp'].SetValue(Esp, IntToStr(Esp),4,4);
    FRegisterValueList.DbgRegisterAutoCreate['ebp'].SetValue(Ebp, IntToStr(Ebp),4,5);
    FRegisterValueList.DbgRegisterAutoCreate['esi'].SetValue(Esi, IntToStr(Esi),4,6);
    FRegisterValueList.DbgRegisterAutoCreate['edi'].SetValue(Edi, IntToStr(Edi),4,7);
    FRegisterValueList.DbgRegisterAutoCreate['eip'].SetValue(Eip, IntToStr(Eip),4,8);

    FRegisterValueList.DbgRegisterAutoCreate['eflags'].Setx86EFlagsValue(EFlags);

    FRegisterValueList.DbgRegisterAutoCreate['cs'].SetValue(SegCs, IntToStr(SegCs),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['ss'].SetValue(SegSs, IntToStr(SegSs),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['ds'].SetValue(SegDs, IntToStr(SegDs),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['es'].SetValue(SegEs, IntToStr(SegEs),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['fs'].SetValue(SegFs, IntToStr(SegFs),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['gs'].SetValue(SegGs, IntToStr(SegGs),4,0);
  end;
{$else}
  with FCurrentContext^ do
  begin
    FRegisterValueList.DbgRegisterAutoCreate['rax'].SetValue(rax, IntToStr(rax),8,0);
    FRegisterValueList.DbgRegisterAutoCreate['rbx'].SetValue(rbx, IntToStr(rbx),8,3);
    FRegisterValueList.DbgRegisterAutoCreate['rcx'].SetValue(rcx, IntToStr(rcx),8,2);
    FRegisterValueList.DbgRegisterAutoCreate['rdx'].SetValue(rdx, IntToStr(rdx),8,1);
    FRegisterValueList.DbgRegisterAutoCreate['rsi'].SetValue(rsi, IntToStr(rsi),8,4);
    FRegisterValueList.DbgRegisterAutoCreate['rdi'].SetValue(rdi, IntToStr(rdi),8,5);
    FRegisterValueList.DbgRegisterAutoCreate['rbp'].SetValue(rbp, IntToStr(rbp),8,6);
    FRegisterValueList.DbgRegisterAutoCreate['rsp'].SetValue(rsp, IntToStr(rsp),8,7);

    FRegisterValueList.DbgRegisterAutoCreate['r8'].SetValue(r8, IntToStr(r8),8,8);
    FRegisterValueList.DbgRegisterAutoCreate['r9'].SetValue(r9, IntToStr(r9),8,9);
    FRegisterValueList.DbgRegisterAutoCreate['r10'].SetValue(r10, IntToStr(r10),8,10);
    FRegisterValueList.DbgRegisterAutoCreate['r11'].SetValue(r11, IntToStr(r11),8,11);
    FRegisterValueList.DbgRegisterAutoCreate['r12'].SetValue(r12, IntToStr(r12),8,12);
    FRegisterValueList.DbgRegisterAutoCreate['r13'].SetValue(r13, IntToStr(r13),8,13);
    FRegisterValueList.DbgRegisterAutoCreate['r14'].SetValue(r14, IntToStr(r14),8,14);
    FRegisterValueList.DbgRegisterAutoCreate['r15'].SetValue(r15, IntToStr(r15),8,15);

    FRegisterValueList.DbgRegisterAutoCreate['rip'].SetValue(rip, IntToStr(rip),8,16);
    FRegisterValueList.DbgRegisterAutoCreate['eflags'].Setx86EFlagsValue(EFlags);

    FRegisterValueList.DbgRegisterAutoCreate['cs'].SetValue(SegCs, IntToStr(SegCs),8,43);
    FRegisterValueList.DbgRegisterAutoCreate['ss'].SetValue(SegSs, IntToStr(SegSs),8,44);
    FRegisterValueList.DbgRegisterAutoCreate['ds'].SetValue(SegDs, IntToStr(SegDs),8,45);
    FRegisterValueList.DbgRegisterAutoCreate['es'].SetValue(SegEs, IntToStr(SegEs),8,42);
    FRegisterValueList.DbgRegisterAutoCreate['fs'].SetValue(SegFs, IntToStr(SegFs),8,46);
    FRegisterValueList.DbgRegisterAutoCreate['gs'].SetValue(SegGs, IntToStr(SegGs),8,47);
  end;
{$endif}
  FRegisterValueListValid:=true;
end;

procedure TDbgWinThread.Suspend;
var
  r: DWORD;
begin
  if FIsSuspended then
    exit;
  r := SuspendThread(Handle);
  FIsSuspended := r <> DWORD(-1);
  debugln(DBG_WARNINGS and (r = DWORD(-1)), 'Failed to resume Thread %d (handle: %d). Error: %s', [Id, Handle, GetLastErrorText]);
end;

procedure TDbgWinThread.SuspendForStepOverBreakPoint;
begin
  if FIsSkippingBreakPoint then begin
    if GetInstructionPointerRegisterValue = FIsSkippingBreakPointAddress then
      Process.TempRemoveBreakInstructionCode(FIsSkippingBreakPointAddress);
    // else the single step should be done, and the event should be received next
  end
  else
    Suspend;
end;

procedure TDbgWinThread.Resume;
var
  r: DWORD;
begin
  if not FIsSuspended then
    exit;
  r := ResumeThread(Handle);
  FIsSuspended := not(r <> DWORD(-1));
  debugln(DBG_WARNINGS and (r = DWORD(-1)), 'Failed to resume Thread %d (handle: %d). Error: %s', [Id, Handle, GetLastErrorText]);
end;

procedure TDbgWinThread.SetSingleStepOverBreakPoint;
begin
  SetSingleStep;
  FIsSkippingBreakPoint := True;
  FIsSkippingBreakPointAddress := GetInstructionPointerRegisterValue;
end;

procedure TDbgWinThread.EndSingleStepOverBreakPoint;
begin
  FIsSkippingBreakPoint := False;
end;

procedure TDbgWinThread.SetSingleStep;
begin
  if FCurrentContext = nil then
    ReadThreadState;
  FCurrentContext^.EFlags := FCurrentContext^.EFlags or FLAG_TRACE_BIT;
  FThreadContextChanged:=true;
end;

function TDbgWinThread.AddWatchpoint(AnAddr: TDBGPtr): integer;
  function SetBreakpoint(var dr: {$ifdef cpui386}DWORD{$else}DWORD64{$endif}; ind: byte): boolean;
  begin
    if (Dr=0) and ((FCurrentContext^.Dr7 and (1 shl ind))=0) then
    begin
      FCurrentContext^.Dr7 := FCurrentContext^.Dr7 or (1 shl (ind*2));
      FCurrentContext^.Dr7 := FCurrentContext^.Dr7 or ($30000 shl (ind*4));
      Dr:=AnAddr;
      FThreadContextChanged:=true;
      Result := True;
    end
    else
      result := False;
  end;

begin
  if FCurrentContext = nil then
    ReadThreadState;
  result := -1;
  if SetBreakpoint(FCurrentContext^.Dr0, 0) then
    result := 0
  else if SetBreakpoint(FCurrentContext^.Dr1, 1) then
    result := 1
  else if SetBreakpoint(FCurrentContext^.Dr2, 2) then
    result := 2
  else if SetBreakpoint(FCurrentContext^.Dr3, 3) then
    result := 3
  else
    DebugLn(DBG_WARNINGS ,'No hardware breakpoint available.');
end;

function TDbgWinThread.RemoveWatchpoint(AnId: integer): boolean;

  function RemoveBreakpoint(var dr: {$ifdef cpui386}DWORD{$else}DWORD64{$endif}; ind: byte): boolean;
  begin
    if (Dr<>0) and ((FCurrentContext^.Dr7 and (1 shl (ind*2)))<>0) then
    begin
      FCurrentContext^.Dr7 := FCurrentContext^.Dr7 xor (1 shl (ind*2));
      FCurrentContext^.Dr7 := FCurrentContext^.Dr7 xor ($30000 shl (ind*4));
      Dr:=0;
      FThreadContextChanged:=true;
      Result := True;
    end
    else
    begin
      result := False;
      DebugLn(DBG_WARNINGS ,'HW watchpoint is not set.');
    end;
  end;

begin
  if FCurrentContext = nil then
    ReadThreadState;
  case AnId of
    0: result := RemoveBreakpoint(FCurrentContext^.Dr0, 0);
    1: result := RemoveBreakpoint(FCurrentContext^.Dr1, 1);
    2: result := RemoveBreakpoint(FCurrentContext^.Dr2, 2);
    3: result := RemoveBreakpoint(FCurrentContext^.Dr3, 3);
  end
end;

procedure TDbgWinThread.BeforeContinue;
begin
  if ID <> MDebugEvent.dwThreadId then
    exit;


  inherited;

  if (FCurrentContext <> nil) and
     (FCurrentContext^.Dr6 <> $ffff0ff0) then
  begin
    FCurrentContext^.Dr6:=$ffff0ff0;
    FThreadContextChanged:=true;
  end;

  if FThreadContextChanged then
  begin
    Assert(FCurrentContext <> nil, 'TDbgWinThread.BeforeContinue: none existing context was changed');
    if SetThreadContext(Handle, FCurrentContext^) then
      FThreadContextChanged:=false
    else
      DebugLn(DBG_WARNINGS ,'Thread %u: Unable to set context', [ID])
  end;
  FThreadContextChanged := False;
  FCurrentContext := nil;
end;

function TDbgWinThread.ResetInstructionPointerAfterBreakpoint: boolean;
var
  _UC: record
    C: TContext;
    D: array[1..16] of Byte;
  end;
  Context: PContext;
begin
  Result := False;
  assert(MDebugEvent.Exception.ExceptionRecord.ExceptionCode <> EXCEPTION_SINGLE_STEP, 'dec(IP) EXCEPTION_SINGLE_STEP');

  Context := AlignPtr(@_UC, $10);
  Context^.ContextFlags := CONTEXT_CONTROL;
  if not GetThreadContext(Handle, Context^)
  then begin
    DebugLn(DBG_WARNINGS ,'Unable to get context');
    Exit;
  end;

  if FCurrentContext = nil then
    ReadThreadState;

  Context^.ContextFlags := CONTEXT_CONTROL;
  {$ifdef cpui386}
  Dec(Context^.Eip);
  dec(FCurrentContext^.Eip);
  {$else}
  Dec(Context^.Rip);
  dec(FCurrentContext^.Rip);
debugln(['TDbgWinThread.ResetInstructionPointerAfterBreakpoint ',ID, ' before ', dbghex(FCurrentContext^.Rip), ' / ',Context^.Rip]);
  {$endif}

  if not SetThreadContext(Handle, Context^)
  then begin
    DebugLn(DBG_WARNINGS ,'Unable to set context');
    Exit;
  end;
  // TODO: only changed FCurrentContext, and write back in BeforeContinue;
  FThreadContextChanged:=false;
  Result := True;
end;

function TDbgWinThread.ReadThreadState: boolean;
begin
  if Process.ProcessID <> MDebugEvent.dwProcessId then begin
    DebugLn(DBG_WARNINGS, 'ERROR: attempt to read threadstate, for wrong process. Thread: %u Thread-Process: %u Event-Process %u', [Id, Process.ProcessID, MDebugEvent.dwProcessId]);
    exit(False);
  end;

  FCurrentContext := Pointer((PtrUInt(@_UnAligendContext) + 15) and not PtrUInt($F));
  FCurrentContext^.ContextFlags := CONTEXT_SEGMENTS or CONTEXT_INTEGER or CONTEXT_CONTROL or CONTEXT_DEBUG_REGISTERS;
  SetLastError(0);
  result := GetThreadContext(Handle, FCurrentContext^);
  FRegisterValueListValid:=False;
end;

function TDbgWinThread.GetInstructionPointerRegisterValue: TDbgPtr;
begin
  if FCurrentContext = nil then
    ReadThreadState;
{$ifdef cpui386}
  Result := FCurrentContext^.Eip;
{$else}
  Result := FCurrentContext^.Rip;
{$endif}
end;

function TDbgWinThread.GetStackBasePointerRegisterValue: TDbgPtr;
begin
  if FCurrentContext = nil then
    ReadThreadState;
{$ifdef cpui386}
  Result := FCurrentContext^.Ebp;
{$else}
  Result := FCurrentContext^.Rbp;
{$endif}
end;

function TDbgWinThread.GetStackPointerRegisterValue: TDbgPtr;
begin
  if FCurrentContext = nil then
    ReadThreadState;
{$ifdef cpui386}
  Result := FCurrentContext^.Esp;
{$else}
  Result := FCurrentContext^.Rsp;
{$endif}
end;

initialization
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
end.

