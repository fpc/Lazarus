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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit FpDbgWinClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Windows,
  FpDbgUtil,
  FpDbgClasses,
  FpDbgWinExtra,
  FpDbgInfo,
  FpDbgLoader, FpdMemoryTools,
  DbgIntfBaseTypes,
  LazLoggerBase;

type

  { TDbgWinThread }

  TDbgWinThread = class(TDbgThread)
  public
    function SingleStep: Boolean; virtual;
  end;


  { TDbgWinBreakpoint }

  TDbgWinBreakpointEvent = procedure(const ASender: TDbgBreakpoint; const AContext: TContext) of object;
  TDbgWinBreakpoint = class(TDbgBreakpoint)
  protected
    procedure SetBreak; override;
    procedure ResetBreak; override;
  public
    function Hit(const AThreadID: Integer): Boolean; override;
  end;

  { TDbgWinProcess }

  TDbgWinProcess = class(TDbgProcess)
  private
    FInfo: TCreateProcessDebugInfo;
  protected
    function GetModuleFileName(AModuleHandle: THandle): string; override;
    function GetHandle: THandle; override;
    function InitializeLoader: TDbgImageLoader; override;
  public
    constructor Create(const ADefaultName: String; const AProcessID, AThreadID: Integer; const AInfo: TCreateProcessDebugInfo);
    destructor Destroy; override;

    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; override;
    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; override;
    function ReadString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: String): Boolean; override;
    function ReadWString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: WideString): Boolean; override;

    procedure Interrupt;
    procedure ContinueDebugEvent(const AThread: TDbgThread; const ADebugEvent: TDebugEvent);
    function  HandleDebugEvent(const ADebugEvent: TDebugEvent): Boolean;

    function  AddLib(const AInfo: TLoadDLLDebugInfo): TDbgLibrary;
    procedure AddThread(const AID: Integer; const AInfo: TCreateThreadDebugInfo);
    procedure RemoveLib(const AInfo: TUnloadDLLDebugInfo);
  end;


procedure RegisterDbgClasses;

implementation

procedure RegisterDbgClasses;
begin
  OSDbgClasses.DbgThreadClass:=TDbgWinThread;
  OSDbgClasses.DbgBreakpointClass:=TDbgWinBreakpoint;
  OSDbgClasses.DbgProcessClass:=TDbgWinProcess;
end;

procedure LogLastError;
begin
  DebugLn('FpDbg-ERROR: ', GetLastErrorText);
end;

{ TDbgWinProcess }

function TDbgWinProcess.GetModuleFileName(AModuleHandle: THandle): string;
var
  s: string;
  len: Integer;
begin
  SetLength(S, MAX_PATH);
  len := windows.GetModuleFileName(AModuleHandle, @S[1], MAX_PATH);
  if len > 0
  then SetLength(S, len - 1)
  else begin
    S := '';
    LogLastError;
  end;
  result := s;
end;

function TDbgWinProcess.GetHandle: THandle;
begin
  Result:=FInfo.hProcess;
end;

function TDbgWinProcess.InitializeLoader: TDbgImageLoader;
begin
  result := TDbgImageLoader.Create(ModuleHandle);
end;

constructor TDbgWinProcess.Create(const ADefaultName: String; const AProcessID, AThreadID: Integer; const AInfo: TCreateProcessDebugInfo);
begin
  FInfo := AInfo;
  inherited Create(ADefaultName, AProcessID, AThreadID, AInfo.hFile, TDbgPtr(AInfo.lpBaseOfImage), TDbgPtr(AInfo.lpImageName), AInfo.fUnicode <> 0);
  FMainThread := OSDbgClasses.DbgThreadClass.Create(Self, AThreadID, AInfo.hThread, AInfo.lpThreadLocalBase, AInfo.lpStartAddress);
end;

destructor TDbgWinProcess.Destroy;
begin
  CloseHandle(FInfo.hProcess);
  inherited Destroy;
end;

function TDbgWinProcess.ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean;
var
  BytesRead: Cardinal;
begin
  Result := ReadProcessMemory(Handle, Pointer(PtrUInt(AAdress)), @AData, ASize, BytesRead) and (BytesRead = ASize);

  if not Result then LogLastError;
end;

function TDbgWinProcess.WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean;
var
  BytesWritten: Cardinal;
begin
  Result := WriteProcessMemory(Handle, Pointer(PtrUInt(AAdress)), @AData, ASize, BytesWritten) and (BytesWritten = ASize);

  if not Result then LogLastError;
end;

function TDbgWinProcess.ReadString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: String): Boolean;
var
  BytesRead: Cardinal;
  buf: array of Char;
begin
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
  BytesRead: Cardinal;
  buf: array of WChar;
begin
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
      Log('Proces %u interrupt: Unable to get context', [ProcessID]);
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
      Log('Proces %u interrupt: Unable to set context', [ProcessID]);
      Exit;
    end;
  finally
    ResumeTHread(FInfo.hThread);
  end;
end;

procedure TDbgWinProcess.ContinueDebugEvent(const AThread: TDbgThread; const ADebugEvent: TDebugEvent);
begin
  case ADebugEvent.dwDebugEventCode of
    EXCEPTION_DEBUG_EVENT: begin
      case ADebugEvent.Exception.ExceptionRecord.ExceptionCode of
        EXCEPTION_BREAKPOINT: begin
          if AThread = nil then Exit;
          if FCurrentBreakpoint = nil then Exit;
          if AThread.SingleStepping then Exit;
          AThread.SingleStep;
          FReEnableBreakStep := True;
        end;
      end;
    end;
  end;
end;

{ ------------------------------------------------------------------
  HandleDebugEvent

  Result: True if the event was triggered internally
          The callee should continue the process
  ------------------------------------------------------------------ }
function TDbgWinProcess.HandleDebugEvent(const ADebugEvent: TDebugEvent): Boolean;

  function DoBreak: Boolean;
  var
    ID: TDbgPtr;
  begin
    Result := False;
    ID := TDbgPtr(ADebugEvent.Exception.ExceptionRecord.ExceptionAddress);
    if not FBreakMap.GetData(ID, FCurrentBreakpoint) then Exit;
    if FCurrentBreakpoint = nil then Exit;

    Result := True;
    if not FCurrentBreakpoint.Hit(ADebugEvent.dwThreadId)
    then FCurrentBreakpoint := nil; // no need for a singlestep if we continue
  end;

  function DoSingleStep: Boolean;
  var
    _UC: record
      C: TContext;
      D: array[1..16] of Byte;
    end;
    Context: PContext;
  begin
    Result := False;
    // check if we are interupting
    Context := AlignPtr(@_UC, $10);
    Context^.ContextFlags := CONTEXT_DEBUG_REGISTERS;
    if GetThreadContext(FInfo.hThread, Context^)
    then begin
      if Context^.Dr6 and 1 <> 0
      then begin
        // interrupt !
        // disable break.
        Context^.Dr7 := Context^.Dr7 and not $1;
        Context^.Dr0 := 0;
        if not SetThreadContext(FInfo.hThread, Context^)
        then begin
          // Heeellppp!!
          Log('Thread %u: Unable to reset BR0', [ADebugEvent.dwThreadId]);
        end;
        // check if we are also singlestepping
        // if not, then exit, else proceed to next check
        if Context^.Dr6 and $40 = 0
        then Exit;
      end;
    end
    else begin
      // if we can not get the context, we probable weren't able to set it either
      Log('Thread %u: Unable to get context', [ADebugEvent.dwThreadId]);
    end;

    // check if we are single stepping ourself
    if FCurrentBreakpoint = nil then Exit;

    FCurrentBreakpoint.SetBreak;
    FCurrentBreakpoint := nil;
    Result := FReEnableBreakStep;
    FReEnableBreakStep := False;
  end;

begin
  Result := False;
  case ADebugEvent.dwDebugEventCode of
    EXCEPTION_DEBUG_EVENT: begin
      case ADebugEvent.Exception.ExceptionRecord.ExceptionCode of
        EXCEPTION_BREAKPOINT:  {Result :=} DoBreak; // we never set a break ourself, let the callee pause!
        EXCEPTION_SINGLE_STEP: Result := DoSingleStep;
      end;
    end;
    CREATE_THREAD_DEBUG_EVENT: begin
      AddThread(ADebugEvent.dwThreadId, ADebugEvent.CreateThread)
    end;
    EXIT_THREAD_DEBUG_EVENT: begin
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

function TDbgWinProcess.AddLib(const AInfo: TLoadDLLDebugInfo): TDbgLibrary;
var
  ID: TDbgPtr;
begin
  Result := TDbgLibrary.Create(Self, HexValue(AInfo.lpBaseOfDll, SizeOf(Pointer), [hvfIncludeHexchar]), AInfo.hFile, TDbgPtr(AInfo.lpBaseOfDll), TDbgPtr(AInfo.lpImageName), AInfo.fUnicode <> 0);
  ID := TDbgPtr(AInfo.lpBaseOfDll);
  FLibMap.Add(ID, Result);
  if Result.DbgInfo.HasInfo
  then FSymInstances.Add(Result);
end;

procedure TDbgWinProcess.AddThread(const AID: Integer; const AInfo: TCreateThreadDebugInfo);
var
  Thread: TDbgThread;
begin
  Thread := OSDbgClasses.DbgThreadClass.Create(Self, AID, AInfo.hThread, AInfo.lpThreadLocalBase, AInfo.lpStartAddress);
  FThreadMap.Add(AID, Thread);
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
  // TODO: Free lib ???
end;

{ TDbgWinBreakpoint }

procedure TDbgWinBreakpoint.SetBreak;
begin
  inherited;
  FlushInstructionCache(Process.Handle, Pointer(PtrUInt(Location)), 1);
end;

procedure TDbgWinBreakpoint.ResetBreak;
begin
  inherited;
  FlushInstructionCache(Process.Handle, Pointer(PtrUInt(Location)), 1);
end;

function TDbgWinBreakpoint.Hit(const AThreadID: Integer): Boolean;
var
  Thread: TDbgThread;
  _UC: record
    C: TContext;
    D: array[1..16] of Byte;
  end;
  Context: PContext;
begin
  Result := False;
  if FOrgValue = $CC then Exit; // breakpoint on a hardcoded breakpoint
                                // no need to jum back and restore instruction
  ResetBreak;

  if not Process.GetThread(AThreadId, Thread) then Exit;

  Context := AlignPtr(@_UC, $10);

  Context^.ContextFlags := CONTEXT_CONTROL;
  if not GetThreadContext(Thread.Handle, Context^)
  then begin
    Log('Break $s: Unable to get context', [HexValue(Location, SizeOf(Pointer), [hvfIncludeHexchar])]);
    Exit;
  end;

  Context^.ContextFlags := CONTEXT_CONTROL;
  {$ifdef cpui386}
  Dec(Context^.Eip);
  {$else}
  Dec(Context^.Rip);
  {$endif}

  if not SetThreadContext(Thread.Handle, Context^)
  then begin
    Log('Break %s: Unable to set context', [HexValue(Location, SizeOf(Pointer), [hvfIncludeHexchar])]);
    Exit;
  end;
  Result := True;
end;

{ TDbgWinThread }

function TDbgWinThread.SingleStep: Boolean;
var
  _UC: record
    C: TContext;
    D: array[1..16] of Byte;
  end;
  Context: PContext;
begin
  Context := AlignPtr(@_UC, $10);
  Context^.ContextFlags := CONTEXT_CONTROL;
  if not GetThreadContext(Handle, Context^)
  then begin
    Log('Thread %u: Unable to get context', [ID]);
    Exit;
  end;

  Context^.ContextFlags := CONTEXT_CONTROL;
  Context^.EFlags := Context^.EFlags or FLAG_TRACE_BIT;

  if not SetThreadContext(Handle, Context^)
  then begin
    Log('Thread %u: Unable to set context', [ID]);
    Exit;
  end;

  Inherited;
end;

end.

