{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpwdloop.pas  -  FP standalone windows debugger - Debugger main loop
 ---------------------------------------------------------------------------

 This unit contains the main loop of the debugger. It waits for a debug
 event and handles it

 ---------------------------------------------------------------------------

 @created(Mon Apr 10th WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)

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
unit FPWDLoop;
{$mode objfpc}{$H+}
interface

uses
  Windows, SysUtils, WinDebugger, WinDExtra, WinDisas;

procedure DebugLoop;


implementation

uses
  FPWDGlobal, FPWDPEImage, FPWDType;

var
  MDebugEvent: TDebugEvent;

procedure HandleCreateProcess(const AEvent: TDebugEvent);
var
  Proc: TDbgProcess;
  S: String;
begin
  WriteLN(Format('hFile: 0x%x', [AEvent.CreateProcessInfo.hFile]));
  WriteLN(Format('hProcess: 0x%x', [AEvent.CreateProcessInfo.hProcess]));
  WriteLN(Format('hThread: 0x%x', [AEvent.CreateProcessInfo.hThread]));
  WriteLN('Base adress: ', FormatAddress(AEvent.CreateProcessInfo.lpBaseOfImage));
  WriteLN(Format('Debugsize: %d', [AEvent.CreateProcessInfo.nDebugInfoSize]));
  WriteLN(Format('Debugoffset: %d', [AEvent.CreateProcessInfo.dwDebugInfoFileOffset]));

  if AEvent.CreateProcessInfo.lpBaseOfImage <> nil
  then DumpPEImage(AEvent.CreateProcessInfo.hProcess, TDbgPtr(AEvent.CreateProcessInfo.lpBaseOfImage));

  if GMainProcess = nil
  then S := GFileName;
  Proc := TDbgProcess.Create(S, AEvent.dwProcessId, AEvent.dwThreadId, AEvent.CreateProcessInfo);
  if GMainProcess = nil
  then GMainProcess := Proc;
  GProcessMap.Add(AEvent.dwProcessId, Proc);
  if GBreakOnLibraryLoad
  then GState := dsPause;
  GCurrentProcess := proc;
end;

procedure HandleCreateThread(const AEvent: TDebugEvent);
begin
  WriteLN(Format('Start adress: 0x%p', [AEvent.CreateThread.lpStartAddress]));
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
begin
  if AEvent.Exception.dwFirstChance = 0
  then Write('Exception: ')
  else Write('First chance exception: ');

  case AEvent.Exception.ExceptionRecord.ExceptionCode of
    EXCEPTION_ACCESS_VIOLATION         : Write('ACCESS_VIOLATION');
    EXCEPTION_ARRAY_BOUNDS_EXCEEDED    : Write('ARRAY_BOUNDS_EXCEEDED');
    EXCEPTION_BREAKPOINT               : Write('BREAKPOINT');
    EXCEPTION_DATATYPE_MISALIGNMENT    : Write('DATATYPE_MISALIGNMENT');
    EXCEPTION_FLT_DENORMAL_OPERAND     : Write('FLT_DENORMAL_OPERAND');
    EXCEPTION_FLT_DIVIDE_BY_ZERO       : Write('FLT_DIVIDE_BY_ZERO');
    EXCEPTION_FLT_INEXACT_RESULT       : Write('FLT_INEXACT_RESULT');
    EXCEPTION_FLT_INVALID_OPERATION    : Write('FLT_INVALID_OPERATION');
    EXCEPTION_FLT_OVERFLOW             : Write('FLT_OVERFLOW');
    EXCEPTION_FLT_STACK_CHECK          : Write('FLT_STACK_CHECK');
    EXCEPTION_FLT_UNDERFLOW            : Write('FLT_UNDERFLOW');
    EXCEPTION_ILLEGAL_INSTRUCTION      : Write('ILLEGAL_INSTRUCTION');
    EXCEPTION_IN_PAGE_ERROR            : Write('IN_PAGE_ERROR');
    EXCEPTION_INT_DIVIDE_BY_ZERO       : Write('INT_DIVIDE_BY_ZERO');
    EXCEPTION_INT_OVERFLOW             : Write('INT_OVERFLOW');
    EXCEPTION_INVALID_DISPOSITION      : Write('INVALID_DISPOSITION');
    EXCEPTION_NONCONTINUABLE_EXCEPTION : Write('NONCONTINUABLE_EXCEPTION');
    EXCEPTION_PRIV_INSTRUCTION         : Write('PRIV_INSTRUCTION');
    EXCEPTION_SINGLE_STEP              : Write('SINGLE_STEP');
    EXCEPTION_STACK_OVERFLOW           : Write('STACK_OVERFLOW');
  else
    Write(' Unknown code: ', AEvent.Exception.ExceptionRecord.ExceptionCode);
  end;
  Info0 := PtrUInt(AEvent.Exception.ExceptionRecord.ExceptionAddress);
  Write(' at: ', FormatAddress(Info0));
  Write(' Flags:', Format('%x', [AEvent.Exception.ExceptionRecord.ExceptionFlags]), ' [');
  if AEvent.Exception.ExceptionRecord.ExceptionFlags = 0
  then Write('Continuable')
  else Write('Not continuable');
  Write(']');
  Write(' ParamCount:', AEvent.Exception.ExceptionRecord.NumberParameters);

  case AEvent.Exception.ExceptionRecord.ExceptionCode of
    EXCEPTION_ACCESS_VIOLATION: begin
      Info0 := AEvent.Exception.ExceptionRecord.ExceptionInformation[0];
      Info1Str := FormatAddress(AEvent.Exception.ExceptionRecord.ExceptionInformation[1]);

      case Info0 of
        0: begin
          Write(' Read of address: ', Info1Str);
        end;
        1: begin
          Write(' Write of address: ', Info1Str);
        end;
      end;
    end;
  end;
  WriteLN;

  Write(' Info: ');
  with AEvent.Exception.ExceptionRecord do
    for n := Low(ExceptionInformation) to high(ExceptionInformation) do
    begin
      Write(IntToHex(ExceptionInformation[n], SizeOf(Pointer) * 2), ' ');
      if n and (PARAMCOLS - 1) = (PARAMCOLS - 1)
      then begin
        WriteLN;
        Write('       ');
      end;
    end;
  WriteLn;
  GState := dsPause;
end;

procedure HandleExitProcess(const AEvent: TDebugEvent);
var
  Proc: TDbgProcess;
begin
  if not GetProcess(AEvent.dwProcessId, Proc) then Exit;

  if Proc = GMainProcess then GMainProcess := nil;
  GProcessMap.Delete(AEvent.dwProcessId);

  GState := dsStop;
  WriteLN('Process stopped with exitcode: ', AEvent.ExitProcess.dwExitCode);
end;

procedure HandleExitThread(const AEvent: TDebugEvent);
begin
  WriteLN('Exitcode: ', AEvent.ExitThread.dwExitCode);
end;

procedure HandleLoadDll(const AEvent: TDebugEvent);
var
  Proc: TDbgProcess;
  Lib: TDbgLibrary;
begin
  WriteLN('Base adress: ', FormatAddress(AEvent.LoadDll.lpBaseOfDll));

  if GetProcess(AEvent.dwProcessId, Proc)
  and Proc.GetLib(AEvent.LoadDll.hFile, Lib)
  then begin
    WriteLN('Name: ', Lib.Name);
    DumpPEImage(Proc.Handle, Lib.BaseAddr);
  end;
  if GBreakOnLibraryLoad
  then GState := dsPause;
end;

procedure HandleOutputDebug(const AEvent: TDebugEvent);
var
  Proc: TDbgProcess;
  S: String;
  W: WideString;
begin
  if not GetProcess(AEvent.dwProcessId, Proc) then Exit;

  if AEvent.DebugString.fUnicode <> 0
  then begin
    if not Proc.ReadWString(TDbgPtr(AEvent.DebugString.lpDebugStringData), AEvent.DebugString.nDebugStringLength, W)
    then Exit;
    S := W;
  end
  else begin
    if not Proc.ReadString(TDbgPtr(AEvent.DebugString.lpDebugStringData), AEvent.DebugString.nDebugStringLength, S)
    then Exit;
  end;
  WriteLN('[', AEvent.dwProcessId, ':', AEvent.dwThreadId, '] ', S);
end;

procedure HandleRipEvent(const AEvent: TDebugEvent);
begin
  WriteLN('Error: ', AEvent.RipInfo.dwError);
  WriteLN('Type: ', AEvent.RipInfo.dwType);
end;

procedure HandleUnloadDll(const AEvent: TDebugEvent);
begin
  WriteLN('Base adress: ', FormatAddress(AEvent.UnloadDll.lpBaseOfDll));
end;

procedure DebugLoop;
  procedure DumpEvent(const AEvent: String);
  var
    f: Cardinal;
    n: integer;
  begin
    WriteLN('===');
    WriteLN(AEvent);
    WriteLN('---');
    WriteLN('Process ID: ', MDebugEvent.dwProcessId);
    WriteLN('Thread ID: ', MDebugEvent.dwThreadId);

    if GCurrentThread = nil then Exit;

    {$ifdef cpui386}
    with GCurrentContext^ do WriteLN(Format('DS: 0x%x, ES: 0x%x, FS: 0x%x, GS: 0x%x', [SegDs, SegEs, SegFs, SegGs]));
    with GCurrentContext^ do WriteLN(Format('EAX: 0x%x, EBX: 0x%x, ECX: 0x%x, EDX: 0x%x, EDI: 0x%x, ESI: 0x%x', [Eax, Ebx, Ecx, Edx, Edi, Esi]));
    with GCurrentContext^ do Write(Format('CS: 0x%x, SS: 0x%x, EBP: 0x%x, EIP: 0x%x, ESP: 0x%x, EFlags: 0x%x [', [SegCs, SegSs, Ebp, Eip, Esp, EFlags]));
    {$else}
    with GCurrentContext^ do WriteLN(Format('SegDS: 0x%4.4x, SegES: 0x%4.4x, SegFS: 0x%4.4x, SegGS: 0x%4.4x', [SegDs, SegEs, SegFs, SegGs]));
    with GCurrentContext^ do WriteLN(Format('RAX: 0x%16.16x, RBX: 0x%16.16x, RCX: 0x%16.16x, RDX: 0x%16.16x, RDI: 0x%16.16x, RSI: 0x%16.16x, R9: 0x%16.16x, R10: 0x%16.16x, R11: 0x%16.16x, R12: 0x%16.16x, R13: 0x%16.16x, R14: 0x%16.16x, R15: 0x%16.16x', [Rax, Rbx, Rcx, Rdx, Rdi, Rsi, R9, R10, R11, R12, R13, R14, R15]));
    with GCurrentContext^ do Write(Format('SegCS: 0x%4.4x, SegSS: 0x%4.4x, RBP: 0x%16.16x, RIP: 0x%16.16x, RSP: 0x%16.16x, EFlags: 0x%8.8x [', [SegCs, SegSs, Rbp, Rip, Rsp, EFlags]));
    {$endif}
    // luckely flag and debug registers are named the same
    with GCurrentContext^ do
    begin
      if EFlags and (1 shl 0) <> 0 then Write('CF ');
      if EFlags and (1 shl 2) <> 0 then Write('PF ');
      if EFlags and (1 shl 4) <> 0 then Write('AF ');
      if EFlags and (1 shl 6) <> 0 then Write('ZF ');
      if EFlags and (1 shl 7) <> 0 then Write('SF ');
      if EFlags and (1 shl 8) <> 0 then Write('TF ');
      if EFlags and (1 shl 9) <> 0 then Write('IF ');
      if EFlags and (1 shl 10) <> 0 then Write('DF ');
      if EFlags and (1 shl 11) <> 0 then Write('OF ');
      if (EFlags shr 12) and 3 <> 0 then Write('IOPL=', (EFlags shr 12) and 3);
      if EFlags and (1 shl 14) <> 0 then Write('NT ');
      if EFlags and (1 shl 16) <> 0 then Write('RF ');
      if EFlags and (1 shl 17) <> 0 then Write('VM ');
      if EFlags and (1 shl 18) <> 0 then Write('AC ');
      if EFlags and (1 shl 19) <> 0 then Write('VIF ');
      if EFlags and (1 shl 20) <> 0 then Write('VIP ');
      if EFlags and (1 shl 21) <> 0 then Write('ID ');
      WriteLn(']');

      Write(Format('DR0: 0x%x, DR1: 0x%x, DR2: 0x%x, DR3: 0x%x', [Dr0, Dr1, Dr2, Dr3]));
      Write(' DR6: 0x', IntToHex(Dr6, SizeOf(Pointer) * 2), ' [');
      if Dr6 and $0001 <> 0 then Write('B0 ');
      if Dr6 and $0002 <> 0 then Write('B1 ');
      if Dr6 and $0004 <> 0 then Write('B2 ');
      if Dr6 and $0008 <> 0 then Write('B3 ');
      if Dr6 and $2000 <> 0 then Write('BD ');
      if Dr6 and $4000 <> 0 then Write('BS ');
      if Dr6 and $8000 <> 0 then Write('BT ');
      Write('] DR7: 0x', IntToHex(Dr7, SizeOf(Pointer) * 2), ' [');
      if Dr7 and $01 <> 0 then Write('L0 ');
      if Dr7 and $02 <> 0 then Write('G0 ');
      if Dr7 and $04 <> 0 then Write('L1 ');
      if Dr7 and $08 <> 0 then Write('G1 ');
      if Dr7 and $10 <> 0 then Write('L2 ');
      if Dr7 and $20 <> 0 then Write('G2 ');
      if Dr7 and $40 <> 0 then Write('L3 ');
      if Dr7 and $80 <> 0 then Write('G3 ');
      if Dr7 and $100 <> 0 then Write('LE ');
      if Dr7 and $200 <> 0 then Write('GE ');
      if Dr7 and $2000 <> 0 then Write('GD ');
      f := Dr7 shr 16;
      for n := 0 to 3 do
      begin
        Write('R/W', n,':');
        case f and 3 of
          0: Write('ex');
          1: Write('wo');
          2: Write('IO');
          3: Write('rw');
        end;
        f := f shr 2;
        Write(' LEN', n,':', f and 3 + 1, ' ');
        f := f shr 2;
      end;
      WriteLN(']');
    end;
    WriteLN('---');
  end;
  
  procedure ShowDisas;
  var
    a: PtrUInt;
    Code, CodeBytes: String;
  begin
    WriteLN('===');
    {$ifdef cpui386}
      a := GCurrentContext^.EIP;
      Write('  [', FormatAddress(a), ']');
      Disassemble(GCurrentProcess.Handle, False, a, CodeBytes, Code);
    {$else}
      a := GCurrentContext^.RIP;
      Write('  [', FormatAddress(a), ']');
      Disassemble(GCurrentProcess.Handle, True, a, CodeBytes, Code);
    {$endif}
    WriteLN(' ', CodeBytes, '    ', Code);
  end;
  
  procedure ShowCode;
  var
    a: TDbgPtr;
    sym, symproc: TDbgSymbol;
  begin
    WriteLN('===');
    {$ifdef cpui386}
      a := GCurrentContext^.EIP;
    {$else}
      a := GCurrentContext^.RIP;
    {$endif}
    sym := GCurrentProcess.FindSymbol(a);
    if sym = nil
    then begin
      WriteLn('  [', FormatAddress(a), '] ???');
      Exit;
    end;
    
    symproc := sym;
    while not (symproc.kind in [skProcedure, skFunction]) do
      symproc := symproc.Parent;

    if symproc = nil
    then WriteLn('???')
    else WriteLn(symproc.FileName, ':', symproc.Line, ' ', symproc.Name);

    WriteLn('  [', FormatAddress(a), '] ', sym.FileName, ':', sym.Line, ' ', sym.Name);
  end;

begin
  repeat
    if (GCurrentProcess <> nil) and (GState = dsPause)
    then begin
      GCurrentProcess.ContinueDebugEvent(GCurrentThread, MDebugEvent);
    end;

    if GState in [dsStop, dsPause, dsEvent]
    then begin
      case MDebugEvent.Exception.ExceptionRecord.ExceptionCode of
       EXCEPTION_BREAKPOINT,
       EXCEPTION_SINGLE_STEP: ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE);
      else
        ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_EXCEPTION_NOT_HANDLED);
      end;
      GState := dsRun;
    end;

    if not WaitForDebugEvent(MDebugEvent, 10) then Continue;

    GCurrentProcess := nil;
    GCurrentThread := nil;
    if not GetProcess(MDebugEvent.dwProcessId, GCurrentPRocess) and (GMainProcess <> nil) then Continue;

    GState := dsEvent;
    if GCurrentProcess <> nil
    then begin
      if GCurrentProcess.HandleDebugEvent(MDebugEvent) then Continue;
      if not GCurrentProcess.GetThread(MDebugEvent.dwTHreadID, GCurrentThread)
      then WriteLN('LOOP: Unable to retrieve current thread');
    end;

    FillChar(GCurrentContext^, SizeOf(GCurrentContext^), $EE);

    if GCurrentThread <> nil
    then begin
      // TODO: move to TDbgThread
      GCurrentContext^.ContextFlags := CONTEXT_SEGMENTS or CONTEXT_INTEGER or CONTEXT_CONTROL or CONTEXT_DEBUG_REGISTERS;
      SetLastError(0);
      if not GetThreadContext(GCurrentThread.Handle, GCurrentContext^)
      then WriteLN('LOOP: Unable to retrieve thread context');
    end;

    case MDebugEvent.dwDebugEventCode of
      EXCEPTION_DEBUG_EVENT: begin
        DumpEvent('EXCEPTION_DEBUG_EVENT');
        HandleException(MDebugEvent);
      end;
      CREATE_THREAD_DEBUG_EVENT: begin
        DumpEvent('CREATE_THREAD_DEBUG_EVENT');
        HandleCreateThread(MDebugEvent);
      end;
      CREATE_PROCESS_DEBUG_EVENT: begin
        DumpEvent('CREATE_PROCESS_DEBUG_EVENT');
        HandleCreateProcess(MDebugEvent);
      end;
      EXIT_THREAD_DEBUG_EVENT: begin
        DumpEvent('EXIT_THREAD_DEBUG_EVENT');
        HandleExitThread(MDebugEvent);
      end;
      EXIT_PROCESS_DEBUG_EVENT: begin
        DumpEvent('EXIT_PROCESS_DEBUG_EVENT');
        HandleExitProcess(MDebugEvent);
      end;
      LOAD_DLL_DEBUG_EVENT: begin
        DumpEvent('LOAD_DLL_DEBUG_EVENT');
        HandleLoadDll(MDebugEvent);
      end;
      UNLOAD_DLL_DEBUG_EVENT: begin
        DumpEvent('UNLOAD_DLL_DEBUG_EVENT');
        HandleUnloadDll(MDebugEvent);
      end;
      OUTPUT_DEBUG_STRING_EVENT: begin
        DumpEvent('OUTPUT_DEBUG_STRING_EVENT');
        HandleOutputDebug(MDebugEvent);
      end;
      RIP_EVENT: begin
        DumpEvent('RIP_EVENT');
        HandleRipEvent(MDebugEvent);
      end;
    end;
  until (GState in [dsStop, dsPause, dsQuit]);

  if GState = dsPause
  then begin
    ShowDisas;
    ShowCode;
  end;
end;

end.
