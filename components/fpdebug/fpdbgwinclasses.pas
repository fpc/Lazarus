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
  process,
  FpDbgWinExtra,
  FpDbgInfo,
  FpDbgLoader, FpdMemoryTools,
  DbgIntfBaseTypes,
  LazLoggerBase;

type

  { TDbgWinThread }

  TDbgWinThread = class(TDbgThread)
  protected
    FThreadContextChanged: boolean;
    procedure LoadRegisterValues; override;
  public
    procedure SetSingleStep;
    function AddWatchpoint(AnAddr: TDBGPtr): integer; override;
    function RemoveWatchpoint(AnId: integer): boolean; override;
    procedure BeforeContinue; override;
    function ResetInstructionPointerAfterBreakpoint: boolean; override;
    function ReadThreadState: boolean;
  end;


  { TDbgWinBreakpoint }

  TDbgWinBreakpointEvent = procedure(const ASender: TDbgBreakpoint; const AContext: TContext) of object;
  TDbgWinBreakpoint = class(TDbgBreakpoint)
  protected
    procedure SetBreak; override;
    procedure ResetBreak; override;
  end;

  { TDbgWinProcess }

  TDbgWinProcess = class(TDbgProcess)
  private
    FInfo: TCreateProcessDebugInfo;
    FPauseRequested: boolean;
    FProcProcess: TProcess;
  protected
    function GetHandle: THandle; override;
    function GetLastEventProcessIdentifier: THandle; override;
    function InitializeLoader: TDbgImageLoader; override;
  public
    destructor Destroy; override;

    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; override;
    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; override;
    function ReadString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: String): Boolean; override;
    function ReadWString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: WideString): Boolean; override;

    procedure Interrupt;
    function  HandleDebugEvent(const ADebugEvent: TDebugEvent): Boolean;

    class function StartInstance(AFileName: string; AParams, AnEnvironment: TStrings; AWorkingDirectory: string): TDbgProcess; override;
    function Continue(AProcess: TDbgProcess; AThread: TDbgThread): boolean; override;
    function WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean; override;
    function ResolveDebugEvent(AThread: TDbgThread): TFPDEvent; override;
    procedure StartProcess(const AThreadID: DWORD; const AInfo: TCreateProcessDebugInfo);

    function GetInstructionPointerRegisterValue: TDbgPtr; override;
    function GetStackBasePointerRegisterValue: TDbgPtr; override;
    function GetStackPointerRegisterValue: TDbgPtr; override;
    function Pause: boolean; override;

    procedure TerminateProcess; override;

    function AddrOffset: Int64; override;
    function  AddLib(const AInfo: TLoadDLLDebugInfo): TDbgLibrary;
    procedure AddThread(const AID: Integer; const AInfo: TCreateThreadDebugInfo);
    procedure RemoveLib(const AInfo: TUnloadDLLDebugInfo);
  end;

  { tDbgWinLibrary }

  tDbgWinLibrary = class(TDbgLibrary)
  private
    FInfo: TLoadDLLDebugInfo;
  protected
    function InitializeLoader: TDbgImageLoader; override;
  public
    constructor Create(const AProcess: TDbgProcess; const ADefaultName: String;
      const AModuleHandle: THandle; const ABaseAddr: TDbgPtr; AInfo: TLoadDLLDebugInfo);
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

function GetModuleFileName(AModuleHandle: THandle): string;
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

function GetProcFilename(AProcess: TDbgProcess; lpImageName: LPVOID; fUnicode: word; hFile: handle): string;
var
  NamePtr: TDbgPtr;
  S: String;
  W: WideString;
begin
  W := '';
  if (lpImageName<>nil) and AProcess.ReadOrdinal(TDbgPtr(lpImageName), NamePtr)
  then begin
    if fUnicode <> 0
    then begin
      AProcess.ReadWString(NamePtr, MAX_PATH, W);
    end
    else begin
      if AProcess.ReadString(NamePtr, MAX_PATH, S)
      then W := S;
    end;
  end;

  if W = ''
  then begin
    W := GetModuleFileName(hFile);
  end;
  result := W;
end;

{ tDbgWinLibrary }

function tDbgWinLibrary.InitializeLoader: TDbgImageLoader;
begin
  result := TDbgImageLoader.Create(FInfo.hFile);
end;

constructor tDbgWinLibrary.Create(const AProcess: TDbgProcess;
  const ADefaultName: String; const AModuleHandle: THandle;
  const ABaseAddr: TDbgPtr; AInfo: TLoadDLLDebugInfo);
var
  S: String;
begin
  inherited Create(AProcess, ADefaultName, AModuleHandle, ABaseAddr);
  FInfo := AInfo;

  s := GetProcFilename(AProcess, AInfo.lpImageName, AInfo.fUnicode, AInfo.hFile);
  if s <> ''
  then SetName(s);

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

function TDbgWinProcess.InitializeLoader: TDbgImageLoader;
begin
  result := TDbgImageLoader.Create(FInfo.hFile);
end;

destructor TDbgWinProcess.Destroy;
begin
  CloseHandle(FInfo.hProcess);
  FInfo.hProcess:=0;
  FProcProcess.Free;
  inherited Destroy;
end;

function TDbgWinProcess.ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean;
var
  BytesRead: Cardinal;
begin
  Result := ReadProcessMemory(Handle, Pointer(PtrUInt(AAdress)), @AData, ASize, BytesRead) and (BytesRead = ASize);

  if not Result then LogLastError;
  MaskBreakpointsInReadData(AAdress, ASize, AData);
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

{ ------------------------------------------------------------------
  HandleDebugEvent

  Result: True if the event was triggered internally
          The callee should continue the process
  ------------------------------------------------------------------ }
function TDbgWinProcess.HandleDebugEvent(const ADebugEvent: TDebugEvent): Boolean;

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

class function TDbgWinProcess.StartInstance(AFileName: string; AParams, AnEnvironment: TStrings; AWorkingDirectory: string): TDbgProcess;
var
  AProcess: TProcess;
begin
  AProcess := TProcess.Create(nil);
  try
    AProcess.Options:=[poDebugProcess, poNewProcessGroup];
    AProcess.Executable:=AFilename;
    AProcess.Parameters:=AParams;
    AProcess.Environment:=AnEnvironment;
    AProcess.CurrentDirectory:=AWorkingDirectory;
    AProcess.Execute;

    result := TDbgWinProcess.Create(AFileName, AProcess.ProcessID, AProcess.ThreadID);
  except
    AProcess.Free;
    raise;
  end;

  TDbgWinProcess(result).FProcProcess := AProcess;
end;


function TDbgWinProcess.Continue(AProcess: TDbgProcess; AThread: TDbgThread): boolean;
begin
  case MDebugEvent.Exception.ExceptionRecord.ExceptionCode of
   EXCEPTION_BREAKPOINT,
   EXCEPTION_SINGLE_STEP: begin
     if (AThread.SingleStepping) or assigned(FCurrentBreakpoint) then
       TDbgWinThread(AThread).SetSingleStep;
     AThread.BeforeContinue;
     Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE);
   end
  else begin
     if assigned(AThread) then
       AThread.BeforeContinue;
     Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_EXCEPTION_NOT_HANDLED);
   end;
  end;
  result := true;
end;

function TDbgWinProcess.WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean;
begin
  result := Windows.WaitForDebugEvent(MDebugEvent, INFINITE);
  ProcessIdentifier:=MDebugEvent.dwProcessId;
  ThreadIdentifier:=MDebugEvent.dwThreadId;
end;

function TDbgWinProcess.ResolveDebugEvent(AThread: TDbgThread): TFPDEvent;

  procedure HandleCreateThread(const AEvent: TDebugEvent);
  begin
    DebugLn(Format('Start adress: 0x%p', [AEvent.CreateThread.lpStartAddress]));
  end;

  procedure HandleException(const AEvent: TDebugEvent);
  const
    PARAMCOLS = 12 - SizeOf(Pointer);
  var
    N: Integer;
    Info0: QWORD;
    Info1: QWORD;
    Info1Str: String;
    P: PByte;
    ExInfo32: TExceptionDebugInfo32 absolute AEvent.Exception;
    ExInfo64: TExceptionDebugInfo64 absolute AEvent.Exception;
  begin
    if AEvent.Exception.dwFirstChance = 0
    then DebugLn('Exception: ')
    else DebugLn('First chance exception: ');

    // in both 32 and 64 case is the exceptioncode the first, so no difference
    case AEvent.Exception.ExceptionRecord.ExceptionCode of
      EXCEPTION_ACCESS_VIOLATION         : ExceptionClass:='ACCESS VIOLATION';
      EXCEPTION_ARRAY_BOUNDS_EXCEEDED    : ExceptionClass:='ARRAY BOUNDS EXCEEDED';
      EXCEPTION_BREAKPOINT               : ExceptionClass:='BREAKPOINT';
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
      EXCEPTION_SINGLE_STEP              : ExceptionClass:='SINGLE STEP';
      EXCEPTION_STACK_OVERFLOW           : ExceptionClass:='STACK OVERFLOW';

      // add some status - don't know if we can get them here
      {
      DBG_EXCEPTION_NOT_HANDLED          : DebugLn('DBG_EXCEPTION_NOT_HANDLED');
      STATUS_GUARD_PAGE_VIOLATION        : DebugLn('STATUS_GUARD_PAGE_VIOLATION');
      STATUS_NO_MEMORY                   : DebugLn('STATUS_NO_MEMORY');
      STATUS_CONTROL_C_EXIT              : DebugLn('STATUS_CONTROL_C_EXIT');
      STATUS_FLOAT_MULTIPLE_FAULTS       : DebugLn('STATUS_FLOAT_MULTIPLE_FAULTS');
      STATUS_FLOAT_MULTIPLE_TRAPS        : DebugLn('STATUS_FLOAT_MULTIPLE_TRAPS');
      STATUS_REG_NAT_CONSUMPTION         : DebugLn('STATUS_REG_NAT_CONSUMPTION');
      STATUS_SXS_EARLY_DEACTIVATION      : DebugLn('STATUS_SXS_EARLY_DEACTIVATION');
      STATUS_SXS_INVALID_DEACTIVATION    : DebugLn('STATUS_SXS_INVALID_DEACTIVATION');
      }
    else
      ExceptionClass := 'Unknown exception code $' + IntToHex(ExInfo32.ExceptionRecord.ExceptionCode, 8);
      DebugLn(' [');
      case ExInfo32.ExceptionRecord.ExceptionCode and $C0000000 of
        STATUS_SEVERITY_SUCCESS       : DebugLn('SEVERITY_ERROR');
        STATUS_SEVERITY_INFORMATIONAL : DebugLn('SEVERITY_ERROR');
        STATUS_SEVERITY_WARNING       : DebugLn('SEVERITY_WARNING');
        STATUS_SEVERITY_ERROR         : DebugLn('SEVERITY_ERROR');
      end;
      if ExInfo32.ExceptionRecord.ExceptionCode and $20000000 <> 0
      then DebugLn (' Customer');
      if ExInfo32.ExceptionRecord.ExceptionCode and $10000000 <> 0
      then DebugLn (' Reserved');
      case (ExInfo32.ExceptionRecord.ExceptionCode and $0FFF0000) shr 16 of
        FACILITY_DEBUGGER            : DebugLn('FACILITY_DEBUGGER');
        FACILITY_RPC_RUNTIME         : DebugLn('FACILITY_RPC_RUNTIME');
        FACILITY_RPC_STUBS           : DebugLn('FACILITY_RPC_STUBS');
        FACILITY_IO_ERROR_CODE       : DebugLn('FACILITY_IO_ERROR_CODE');
        FACILITY_TERMINAL_SERVER     : DebugLn('FACILITY_TERMINAL_SERVER');
        FACILITY_USB_ERROR_CODE      : DebugLn('FACILITY_USB_ERROR_CODE');
        FACILITY_HID_ERROR_CODE      : DebugLn('FACILITY_HID_ERROR_CODE');
        FACILITY_FIREWIRE_ERROR_CODE : DebugLn('FACILITY_FIREWIRE_ERROR_CODE');
        FACILITY_CLUSTER_ERROR_CODE  : DebugLn('FACILITY_CLUSTER_ERROR_CODE');
        FACILITY_ACPI_ERROR_CODE     : DebugLn('FACILITY_ACPI_ERROR_CODE');
        FACILITY_SXS_ERROR_CODE      : DebugLn('FACILITY_SXS_ERROR_CODE');
      else
        DebugLn(' Facility: $', IntToHex((ExInfo32.ExceptionRecord.ExceptionCode and $0FFF0000) shr 16, 3));
      end;
      DebugLn(' Code: $', IntToHex((ExInfo32.ExceptionRecord.ExceptionCode and $0000FFFF), 4));

    end;
    ExceptionClass:='External: '+ExceptionClass;
    DebugLn(ExceptionClass);
    ExceptionMessage:='';
    if GMode = dm32
    then Info0 := PtrUInt(ExInfo32.ExceptionRecord.ExceptionAddress)
    else Info0 := PtrUInt(ExInfo64.ExceptionRecord.ExceptionAddress);
    DebugLn(' at: ', FormatAddress(Info0));
    DebugLn(' Flags:', Format('%x', [AEvent.Exception.ExceptionRecord.ExceptionFlags]), ' [');

    if AEvent.Exception.ExceptionRecord.ExceptionFlags = 0
    then DebugLn('Continuable')
    else DebugLn('Not continuable');
    DebugLn(']');
    if GMode = dm32
    then DebugLn(' ParamCount:', IntToStr(ExInfo32.ExceptionRecord.NumberParameters))
    else DebugLn(' ParamCount:', IntToStr(ExInfo64.ExceptionRecord.NumberParameters));

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

    DebugLn(' Info: ');
    for n := 0 to EXCEPTION_MAXIMUM_PARAMETERS - 1 do
    begin
      if GMode = dm32
      then Info0 := ExInfo32.ExceptionRecord.ExceptionInformation[n]
      else Info0 := ExInfo64.ExceptionRecord.ExceptionInformation[n];
      DebugLn(IntToHex(Info0, DBGPTRSIZE[GMode] * 2), ' ');
      if n and (PARAMCOLS - 1) = (PARAMCOLS - 1)
      then begin
        DebugLn;
        DebugLn('       ');
      end;
    end;
    DebugLn('');
  end;

  procedure HandleCreateProcess(const AEvent: TDebugEvent);
  var
    S: String;
  begin
    DebugLn(Format('hFile: 0x%x', [AEvent.CreateProcessInfo.hFile]));
    DebugLn(Format('hProcess: 0x%x', [AEvent.CreateProcessInfo.hProcess]));
    DebugLn(Format('hThread: 0x%x', [AEvent.CreateProcessInfo.hThread]));
    DebugLn('Base adress: ', FormatAddress(AEvent.CreateProcessInfo.lpBaseOfImage));
    DebugLn(Format('Debugsize: %d', [AEvent.CreateProcessInfo.nDebugInfoSize]));
    DebugLn(Format('Debugoffset: %d', [AEvent.CreateProcessInfo.dwDebugInfoFileOffset]));

    StartProcess(AEvent.dwThreadId, AEvent.CreateProcessInfo);
  end;

  procedure HandleExitThread(const AEvent: TDebugEvent);
  begin
    DebugLn('Exitcode: ' + IntToStr(AEvent.ExitThread.dwExitCode));
  end;

  procedure DumpEvent(const AEvent: String);
  var
    f: Cardinal;
    n: integer;
  begin
    DebugLn('===');
    DebugLn(AEvent);
    DebugLn('---');
    DebugLn('Process ID: '+ IntToSTr(MDebugEvent.dwProcessId));
    DebugLn('Thread ID: '+ IntToStr(MDebugEvent.dwThreadId));

    if AThread = nil then Exit;

{$PUSH}{$R-}
    {$ifdef cpui386}
    with GCurrentContext^ do DebugLn(Format('DS: 0x%x, ES: 0x%x, FS: 0x%x, GS: 0x%x', [SegDs, SegEs, SegFs, SegGs]));
    with GCurrentContext^ do DebugLn(Format('EAX: 0x%x, EBX: 0x%x, ECX: 0x%x, EDX: 0x%x, EDI: 0x%x, ESI: 0x%x', [Eax, Ebx, Ecx, Edx, Edi, Esi]));
    with GCurrentContext^ do DebugLn(Format('CS: 0x%x, SS: 0x%x, EBP: 0x%x, EIP: 0x%x, ESP: 0x%x, EFlags: 0x%x [', [SegCs, SegSs, Ebp, Eip, Esp, EFlags]));
    {$else}
    with GCurrentContext^ do DebugLn(Format('SegDS: 0x%4.4x, SegES: 0x%4.4x, SegFS: 0x%4.4x, SegGS: 0x%4.4x', [SegDs, SegEs, SegFs, SegGs]));
    with GCurrentContext^ do DebugLn(Format('RAX: 0x%16.16x, RBX: 0x%16.16x, RCX: 0x%16.16x, RDX: 0x%16.16x, RDI: 0x%16.16x, RSI: 0x%16.16x, R9: 0x%16.16x, R10: 0x%16.16x, R11: 0x%16.16x, R12: 0x%16.16x, R13: 0x%16.16x, R14: 0x%16.16x, R15: 0x%16.16x', [Rax, Rbx, Rcx, Rdx, Rdi, Rsi, R9, R10, R11, R12, R13, R14, R15]));
    with GCurrentContext^ do DebugLn(Format('SegCS: 0x%4.4x, SegSS: 0x%4.4x, RBP: 0x%16.16x, RIP: 0x%16.16x, RSP: 0x%16.16x, EFlags: 0x%8.8x [', [SegCs, SegSs, Rbp, Rip, Rsp, EFlags]));
    {$endif}
    // luckely flag and debug registers are named the same
    with GCurrentContext^ do
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

  procedure HandleLoadDll(const AEvent: TDebugEvent);
  var
    Proc: TDbgProcess;
    Lib: TDbgLibrary;
  begin
    DebugLn('Base adress: ', FormatAddress(AEvent.LoadDll.lpBaseOfDll));
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
    DebugLn('[', IntToStr(AEvent.dwProcessId), ':', IntToSTr(AEvent.dwThreadId), '] ', S);
  end;

  procedure HandleRipEvent(const AEvent: TDebugEvent);
  begin
    DebugLn('Error: ', IntToStr(AEvent.RipInfo.dwError));
    DebugLn('Type: ', IntToStr(AEvent.RipInfo.dwType));
  end;

  procedure HandleUnloadDll(const AEvent: TDebugEvent);
  begin
    DebugLn('Base adress: ', FormatAddress(AEvent.UnloadDll.lpBaseOfDll));
  end;

begin
  if HandleDebugEvent(MDebugEvent)
  then result := deBreakpoint
  else begin
    FillChar(GCurrentContext^, SizeOf(GCurrentContext^), $EE);

    if AThread <> nil
    then begin
      // TODO: move to TDbgThread

      if not TDbgWinThread(AThread).ReadThreadState
      then DebugLn('LOOP: Unable to retrieve thread context');
    end;

    case MDebugEvent.dwDebugEventCode of
      EXCEPTION_DEBUG_EVENT: begin
        DumpEvent('EXCEPTION_DEBUG_EVENT');
        case MDebugEvent.Exception.ExceptionRecord.ExceptionCode of
          EXCEPTION_BREAKPOINT: begin
            if DoBreak(TDbgPtr(MDebugEvent.Exception.ExceptionRecord.ExceptionAddress), MDebugEvent.dwThreadId)
            then
              result := deBreakpoint
            else if assigned(AThread) and assigned(AThread.HiddenBreakpoint) then begin
              AThread.HiddenBreakpoint.Hit(AThread.ID);
              if AThread.Stepping and AThread.CompareStepInfo then
                result := deInternalContinue
              else
                result := deBreakpoint;
            end else if FPauseRequested
            then begin
              result := deBreakpoint;
              FPauseRequested:=false;
            end
            else begin
              // Unknown breakpoint.
              if (MDebugEvent.Exception.dwFirstChance <> 0) and (MDebugEvent.Exception.ExceptionRecord.ExceptionFlags = 0)
              then begin
                // First chance and breakpoint is continuable -> silently ignore.
                result := deInternalContinue
              end else begin
                // Or else, show an exception
                result := deException;
              end;
            end;
          end;
          EXCEPTION_SINGLE_STEP: begin
            if assigned(FCurrentBreakpoint) then
            begin
              FCurrentBreakpoint.SetBreak;
              FCurrentBreakpoint:=nil;
              if FMainThread.SingleStepping then
                result := deBreakpoint
              else
                result := deInternalContinue;
            end
            else
              result := deBreakpoint;

            if AThread.Stepping then
            begin
              if AThread.CompareStepInfo then
                result := deInternalContinue
              else
                result := deBreakpoint;
            end;

            // If there is a breakpoint on this location, handle the breakpoint.
            // Or else the int3-interrupt instruction won't be cleared and the
            // breakpoint will be triggered again. (Notice that the location of
            // the eip-register does not have to be decremented in this case,
            // see TDbgWinThread.ResetInstructionPointerAfterBreakpoint)
            if DoBreak(TDbgPtr(MDebugEvent.Exception.ExceptionRecord.ExceptionAddress), MDebugEvent.dwThreadId)
            then
              result := deBreakpoint;
          end
        else begin
          HandleException(MDebugEvent);
          if MDebugEvent.Exception.dwFirstChance = 1 then
            result := deInternalContinue
          else
            result := deException;
        end;
        end;
      end;
      CREATE_THREAD_DEBUG_EVENT: begin
        DumpEvent('CREATE_THREAD_DEBUG_EVENT');
        HandleCreateThread(MDebugEvent);
        result := deInternalContinue;
      end;
      CREATE_PROCESS_DEBUG_EVENT: begin
        DumpEvent('CREATE_PROCESS_DEBUG_EVENT');
        HandleCreateProcess(MDebugEvent);
        result := deCreateProcess;
      end;
      EXIT_THREAD_DEBUG_EVENT: begin
        DumpEvent('EXIT_THREAD_DEBUG_EVENT');
        HandleExitThread(MDebugEvent);
        result := deInternalContinue;
      end;
      EXIT_PROCESS_DEBUG_EVENT: begin
        DumpEvent('EXIT_PROCESS_DEBUG_EVENT');
        SetExitCode(MDebugEvent.ExitProcess.dwExitCode);
        result := deExitProcess;
      end;
      LOAD_DLL_DEBUG_EVENT: begin
        DumpEvent('LOAD_DLL_DEBUG_EVENT');
        HandleLoadDll(MDebugEvent);
        result := deLoadLibrary;
      end;
      UNLOAD_DLL_DEBUG_EVENT: begin
        DumpEvent('UNLOAD_DLL_DEBUG_EVENT');
        HandleUnloadDll(MDebugEvent);
        result := deInternalContinue;
      end;
      OUTPUT_DEBUG_STRING_EVENT: begin
        DumpEvent('OUTPUT_DEBUG_STRING_EVENT');
        HandleOutputDebug(MDebugEvent);
        result := deInternalContinue;
      end;
      RIP_EVENT: begin
        DumpEvent('RIP_EVENT');
        HandleRipEvent(MDebugEvent);
        result := deInternalContinue;
      end
      else begin
        raise Exception.CreateFmt('Unknown dwDebugEventCode value %d',[MDebugEvent.dwDebugEventCode]);
      end;
    end;

  end;
end;

procedure TDbgWinProcess.StartProcess(const AThreadID: DWORD;const AInfo: TCreateProcessDebugInfo);
var
  s: string;
begin
  FInfo := AInfo;

  s := GetProcFilename(Self, AInfo.lpImageName, AInfo.fUnicode, AInfo.hFile);
  if s <> ''
  then SetName(s);

  LoadInfo;

  if DbgInfo.HasInfo
  then FSymInstances.Add(Self);

  FMainThread := OSDbgClasses.DbgThreadClass.Create(Self, AThreadID, AInfo.hThread);
  FThreadMap.Add(FMainThread.ID, FMainThread);
end;

function TDbgWinProcess.GetInstructionPointerRegisterValue: TDbgPtr;
begin
{$ifdef cpui386}
  Result := GCurrentContext^.Eip;
{$else}
  Result := GCurrentContext^.Rip;
{$endif}
end;

function TDbgWinProcess.GetStackBasePointerRegisterValue: TDbgPtr;
begin
{$ifdef cpui386}
  Result := GCurrentContext^.Ebp;
{$else}
  Result := GCurrentContext^.Rdi;
{$endif}
end;

function TDbgWinProcess.GetStackPointerRegisterValue: TDbgPtr;
begin
{$ifdef cpui386}
  Result := GCurrentContext^.Esp;
{$else}
//  Result := GCurrentContext^.Rdi;
{$endif}
end;

function DebugBreakProcess(Process:HANDLE): WINBOOL; external 'kernel32' name 'DebugBreakProcess';

function TDbgWinProcess.Pause: boolean;
var
  hndl: Handle;
begin
  hndl := OpenProcess(PROCESS_ALL_ACCESS, false, ProcessID);
  FPauseRequested:=true;
  result := DebugBreakProcess(hndl);
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

procedure TDbgWinProcess.AddThread(const AID: Integer; const AInfo: TCreateThreadDebugInfo);
var
  Thread: TDbgThread;
begin
  Thread := OSDbgClasses.DbgThreadClass.Create(Self, AID, AInfo.hThread);
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

{ TDbgWinThread }

procedure TDbgWinThread.LoadRegisterValues;
begin
  {$ifdef cpui386}
  with GCurrentContext^ do
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
  FRegisterValueListValid:=true;
{$else}
  FRegisterValueListValid := False;
  {$warning register not ready for 64 bit}
{$endif}
end;

procedure TDbgWinThread.SetSingleStep;
begin
{$ifdef cpui386}
  GCurrentContext^.EFlags := GCurrentContext^.EFlags or FLAG_TRACE_BIT;
{$else}
  {$warning singlestep not ready for 64 bit}
{$endif}
  FThreadContextChanged:=true;
end;

function TDbgWinThread.AddWatchpoint(AnAddr: TDBGPtr): integer;
var
  i: integer;

  function SetBreakpoint(var dr: DWORD; ind: byte): boolean;
  begin
    if (Dr=0) and ((GCurrentContext^.Dr7 and (1 shl ind))=0) then
    begin
      GCurrentContext^.Dr7 := GCurrentContext^.Dr7 or (1 shl (ind*2));
      GCurrentContext^.Dr7 := GCurrentContext^.Dr7 or ($30000 shl (ind*4));
      Dr:=AnAddr;
      FThreadContextChanged:=true;
      Result := True;
    end
    else
      result := False;
  end;

begin
  result := -1;
{$ifdef cpui386}
  if SetBreakpoint(GCurrentContext^.Dr0, 0) then
    result := 0
  else if SetBreakpoint(GCurrentContext^.Dr1, 1) then
    result := 1
  else if SetBreakpoint(GCurrentContext^.Dr2, 2) then
    result := 2
  else if SetBreakpoint(GCurrentContext^.Dr3, 3) then
    result := 3
  else
    Process.Log('No hardware breakpoint available.');
{$else}
  FRegisterValueListValid := False;
  {$warning watchpoint not ready for 64 bit}
{$endif}
end;

function TDbgWinThread.RemoveWatchpoint(AnId: integer): boolean;

  function RemoveBreakpoint(var dr: DWORD; ind: byte): boolean;
  begin
    if (Dr<>0) and ((GCurrentContext^.Dr7 and (1 shl (ind*2)))<>0) then
    begin
      GCurrentContext^.Dr7 := GCurrentContext^.Dr7 xor (1 shl (ind*2));
      GCurrentContext^.Dr7 := GCurrentContext^.Dr7 xor ($30000 shl (ind*4));
      Dr:=0;
      FThreadContextChanged:=true;
      Result := True;
    end
    else
    begin
      result := False;
      Process.Log('HW watchpoint is not set.');
    end;
  end;

begin
{$ifdef cpui386}
  case AnId of
    0: result := RemoveBreakpoint(GCurrentContext^.Dr0, 0);
    1: result := RemoveBreakpoint(GCurrentContext^.Dr1, 1);
    2: result := RemoveBreakpoint(GCurrentContext^.Dr2, 2);
    3: result := RemoveBreakpoint(GCurrentContext^.Dr3, 3);
  end
{$else}
  FRegisterValueListValid := False;
  {$warning watchpoint not ready for 64 bit}
{$endif}
end;

procedure TDbgWinThread.BeforeContinue;
begin
  if GCurrentContext^.Dr6 <> $ffff0ff0 then
  begin
    GCurrentContext^.Dr6:=$ffff0ff0;
    FThreadContextChanged:=true;
  end;

  if FThreadContextChanged then
  begin
    if SetThreadContext(Handle, GCurrentContext^) then
      FThreadContextChanged:=false
    else
      Log('Thread %u: Unable to set context', [ID])
  end;
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

  // If the location of the breakpoint is reached by single-stepping, there is
  // no need to decrement the instruction pointer.
  if MDebugEvent.Exception.ExceptionRecord.ExceptionCode = EXCEPTION_SINGLE_STEP
  then begin
    result := true;
    Exit;
  end;

  Context := AlignPtr(@_UC, $10);

  Context^.ContextFlags := CONTEXT_CONTROL;
  if not GetThreadContext(Handle, Context^)
  then begin
    Log('Unable to get context');
    Exit;
  end;

  Context^.ContextFlags := CONTEXT_CONTROL;
  {$ifdef cpui386}
  Dec(Context^.Eip);
  dec(GCurrentContext^.Eip);
  {$else}
  Dec(Context^.Rip);
  {$endif}

  if not SetThreadContext(Handle, Context^)
  then begin
    Log('Unable to set context');
    Exit;
  end;
  FThreadContextChanged:=false;
  Result := True;
end;

function TDbgWinThread.ReadThreadState: boolean;
begin
  GCurrentContext^.ContextFlags := CONTEXT_SEGMENTS or CONTEXT_INTEGER or CONTEXT_CONTROL or CONTEXT_DEBUG_REGISTERS;
  SetLastError(0);
  result := GetThreadContext(Handle, GCurrentContext^);
  FRegisterValueListValid:=False;
end;

end.

