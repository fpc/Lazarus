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
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}
{off $DEFINE DebuglnWinDebugEvents}

interface

uses
  Classes,
  SysUtils,
  Windows,
  {$IF FPC_Fullversion>30202}
  {$ifNdef cpui386} ufloatx80, sfpux80, {$endif}
  {$ENDIF}
  Math,
  LazLinkedList,
  FpDbgUtil,
  FpDbgClasses,
  DbgIntfProcess,
  FpDbgWinExtra,
  strutils,
  FpDbgInfo,
  FpDbgLoader, FpDbgDisasX86,
  DbgIntfBaseTypes, DbgIntfDebuggerBase,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif}, UTF8Process,
  FpDbgCommon, FpdMemoryTools, FpErrorMessages, FpDbgCpuX86;

type

  TFpWinCtxFlags = (cfSkip, cfControl, cfFull);
  TFpContextChangeFlag = (ccfControl, ccfInteger);
  TFpContextChangeFlags = set of TFpContextChangeFlag;
  PPWSTR = ^PWSTR;

  { TDbgWinThread }

  TDbgWinThread = class(TDbgx86Thread)
  private type
    TBreakPointState = (bsNone, bsInSingleStep);
  private
    FIsSuspended: Boolean;
    FBreakPointState: TBreakPointState;
    FDoNotPollName: Boolean;
    FIgnoreNextInt3: Boolean;
    FName: String;
    FUnwinder: TDbgStackUnwinderX86MultiMethod;
  protected
    FHasExceptionCleared: boolean;
    FAtHardCodeBreakpoint: boolean;
    FLastHardcodedSize: integer;
  protected
    function GetName: String; override;
    function HasContext: Boolean; virtual; abstract;
    procedure DumpContext; virtual;
  public
    destructor Destroy; override;
    function GetStackUnwinder: TDbgStackUnwinder; override;
    procedure Suspend;
    procedure SuspendForStepOverBreakPoint;
    procedure Resume;
    procedure EndSingleStepOverBreakPoint;
    procedure SetSingleStep; virtual; abstract;
    procedure BeforeContinue; override;
    function ReadThreadState: boolean; virtual; abstract;
    procedure ClearExceptionSignal; override;
    property HasExceptionCleared: boolean read FHasExceptionCleared;

    property Process;
    property HitExternalWatchPoint: boolean read FHitExternalWatchPoint;
  end;

  TDbgWinThreadNameInternal = class(TLinkListItem)
   Fid: TThreadID;
   Fthreadname: shortstring;
   procedure  SetInfo(id:TThreadID;const threadname:string);
  end;

  TDbgWinThreadNameList = class(TLinkList)
  private
   const
    FMaxCounter = 10000;
   var
    FNumCounter: integer;
  protected
    function CreateItem: TLinkListItem; override;
  public
    procedure  ClearThread;
    function   FindById(id:TThreadID):TDbgWinThreadNameInternal;
    function   FetchThread(id:TThreadID):string;
    procedure  AddThread(id:TThreadID;const threadname:string);
  end;

  { TDbgWinProcess }

  TDbgWinProcess = class(TDbgx86Process)
  private
    FInfo: TCreateProcessDebugInfo;
    FProcProcess: TProcessWithRedirect;
    FJustStarted, FTerminated: boolean;
    FDbgUiRemoteBreakin: TDBGPtr;
    FBitness: TBitness;
    FThreadNameList: TDbgWinThreadNameList;
    FGetConsoleBuffer: char;
    FGetConsoleBufferCnt: LongInt;
    FGetConsoleBufferNeedSleep: boolean;
    function GetFullProcessImageName(AProcessHandle: THandle): string;
    function GetModuleFileName(AModuleHandle: THandle): string;
    function GetProcFilename(AProcess: TDbgProcess; lpImageName: LPVOID; fUnicode: word; hFile: handle): string;
    procedure LogLastError(AMsg: String = '');
  protected
    function GetImageBase: QWord; override;
    procedure AfterChangingInstructionCode(const ALocation: TDBGPtr; ACount: Integer); override;
    function GetHandle: THandle; override;
    function GetLastEventProcessIdentifier: THandle; override;
    procedure InitializeLoaders; override;
    function CreateWatchPointData: TFpWatchPointData; override;
    property Info: TCreateProcessDebugInfo read FInfo;
  public
    constructor Create(const AFileName: string; AnOsClasses: TOSDbgClasses;
      AMemManager: TFpDbgMemManager; AMemModel: TFpDbgMemModel; AProcessConfig: TDbgProcessConfig = nil); override;
    destructor Destroy; override;

    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; override;
    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; override;
    function ReadString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: String): Boolean; override;
    function ReadWString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: WideString): Boolean; override;
    function CallParamDefaultLocation(AParamIdx: Integer): TFpDbgMemLocation; override;

    procedure Interrupt; virtual; // required by app/fpd
    function  HandleDebugEvent(const ADebugEvent: TDebugEvent): Boolean;

    function StartInstance(AParams, AnEnvironment: TStrings; AWorkingDirectory, AConsoleTty: string;
                      AFlags: TStartInstanceFlags; out AnError: TFpError): boolean; override;
    function AttachToInstance(APid: Integer; out AnError: TFpError): boolean; override;
    { Program-I/O capture (RedirectConsoleOutput). Served from the launch
      TProcess's pipes when siRediretOutput was requested at StartInstance;
      the base no-ops apply otherwise. }
    function CheckForConsoleOutput(ATimeOutMs: integer): integer; override;
    procedure StopCheckingForConsoleOutput; override;
    function GetConsoleOutput: string; override;
    procedure SendConsoleInput(AString: string); override;

    function CanContinueForWatchEval(ACurrentThread: TDbgThread): boolean; override;
    function Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean; override;
    function Detach(AProcess: TDbgProcess; AThread: TDbgThread): boolean; override;
    function WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean; override;
    function AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent; override;
    function CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread; override;

    procedure StartProcess(const AThreadID: DWORD; const AInfo: TCreateProcessDebugInfo);

    function Pause: boolean; override;

    procedure TerminateProcess; override;

    function  AddLib(const AInfo: TLoadDLLDebugInfo): TDbgLibrary;
    procedure RemoveLib(const AInfo: TUnloadDLLDebugInfo);
    property Bitness: TBitness read FBitness;
  end;
  TDbgWinProcessClass = class of TDbgWinProcess;

  { tDbgWinLibrary }

  tDbgWinLibrary = class(TDbgLibrary)
  private
    FInfo: TLoadDLLDebugInfo;
  protected
    procedure InitializeLoaders; override;
    function GetImageBase: QWord; override;
  public
    constructor Create(const AProcess: TDbgProcess; const ADefaultName: String;
      const AModuleHandle: THandle; AInfo: TLoadDLLDebugInfo);
  end;

type
  PPCONTEXT = ^PCONTEXT;

var
  DebugBreakAddr: Pointer = nil;
  _CreateRemoteThread: function(hProcess: THandle; lpThreadAttributes: Pointer; dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer; dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall = nil;
  _GetFinalPathNameByHandle: function(hFile: HANDLE; lpFilename:LPWSTR; cchFilePath, dwFlags: DWORD):DWORD; stdcall = nil;
  _QueryFullProcessImageName: function (hProcess:HANDLE; dwFlags: DWord; lpExeName:LPWSTR; var lpdwSize:DWORD):BOOL; stdcall = nil;
  _DebugActiveProcessStop: function (ProcessId:DWORD):BOOL; stdcall = nil;
  _DebugActiveProcess: function (ProcessId:DWORD):BOOL; stdcall = nil;
  _IsWow64Process: function (hProcess:HANDLE; WoW64Process: PBOOL):BOOL; stdcall = nil;
  _Wow64GetThreadContext: function (hThread: THandle; var   lpContext: WOW64_CONTEXT): BOOL; stdcall = nil;
  _Wow64SetThreadContext: function (hThread: THandle; const lpContext: WOW64_CONTEXT): BOOL; stdcall = nil;
  _Wow64SuspendThread: function (hThread:HANDLE):DWORD; stdcall = nil;
  _DebugBreakProcess: function(Process:HANDLE): WINBOOL; stdcall = nil;
  _GetThreadDescription: function(hThread: THandle; ppszThreadDescription: PPWSTR): HResult; stdcall = nil;
  _WaitForDebugEventEx: function(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL; stdcall = nil;
  _CancelSynchronousIo: function(hThread: HANDLE): BOOL; stdcall = nil; // requires Vista

{$ifdef cpux86_64}
const
  FLAG_TRACE_BIT = $100;
{$endif}

implementation

var
  DBG_VERBOSE, DBG_WARNINGS, FPDBG_WINDOWS: PLazLoggerLogGroup;

function dbgs(ABrkPointState: TDbgWinThread.TBreakPointState): String;
begin
  WriteStr(Result, ABrkPointState);
end;

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
         STATUS_WX86_BREAKPOINT:             Result := Result + ' STATUS_WX86_BREAKPOINT';
         EXCEPTION_DATATYPE_MISALIGNMENT:    Result := Result + ' EXCEPTION_DATATYPE_MISALIGNMENT';
         EXCEPTION_SINGLE_STEP:              Result := Result + ' EXCEPTION_SINGLE_STEP';
         STATUS_WX86_SINGLE_STEP:            Result := Result + ' STATUS_WX86_SINGLE_STEP';
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

procedure LoadKernelEntryPoints;
var
  hMod: THandle;
begin
  hMod := GetModuleHandle(kernel32);
  DebugLn(DBG_WARNINGS and (hMod = 0), ['ERROR: Failed to get kernel32 handle']);
  if hMod = 0 then
    exit; //????

  DebugBreakAddr := GetProcAddress(hMod, 'DebugBreak');
  Pointer(_CreateRemoteThread) := GetProcAddress(hMod, 'CreateRemoteThread');
  Pointer(_QueryFullProcessImageName) := GetProcAddress(hMod, 'QueryFullProcessImageNameW'); // requires Vista
  Pointer(_DebugActiveProcessStop) := GetProcAddress(hMod, 'DebugActiveProcessStop');
  Pointer(_DebugActiveProcess) := GetProcAddress(hMod, 'DebugActiveProcess');
  Pointer(_GetFinalPathNameByHandle) := GetProcAddress(hMod, 'GetFinalPathNameByHandleW');
  Pointer(_DebugBreakProcess) := GetProcAddress(hMod, 'DebugBreakProcess');
  Pointer(_GetThreadDescription) := GetProcAddress(hMod, 'GetThreadDescription');
  {$ifdef cpux86_64}
  Pointer(_IsWow64Process) := GetProcAddress(hMod, 'IsWow64Process');
  Pointer(_Wow64GetThreadContext) := GetProcAddress(hMod, 'Wow64GetThreadContext');
  Pointer(_Wow64SetThreadContext) := GetProcAddress(hMod, 'Wow64SetThreadContext');
  Pointer(_Wow64SuspendThread) := GetProcAddress(hMod, 'Wow64SuspendThread');
  {$endif}
  Pointer(_WaitForDebugEventEx) := GetProcAddress(hMod, 'WaitForDebugEventEx');
  Pointer(_CancelSynchronousIo) := GetProcAddress(hMod, 'CancelSynchronousIo'); // requires Vista

  DebugLn(DBG_WARNINGS and (DebugBreakAddr = nil), ['WARNING: Failed to get DebugBreakAddr']);
  DebugLn(DBG_WARNINGS and (_CreateRemoteThread = nil), ['WARNING: Failed to get CreateRemoteThread']);
  DebugLn(DBG_WARNINGS and (_QueryFullProcessImageName = nil), ['WARNING: Failed to get QueryFullProcessImageName']);
  DebugLn(DBG_WARNINGS and (_DebugActiveProcessStop = nil), ['WARNING: Failed to get DebugActiveProcessStop']);
  DebugLn(DBG_WARNINGS and (_DebugActiveProcess = nil), ['WARNING: Failed to get DebugActiveProcess']);
  DebugLn(DBG_WARNINGS and (_GetFinalPathNameByHandle = nil), ['WARNING: Failed to get GetFinalPathNameByHandle']);
  DebugLn(DBG_WARNINGS and (_DebugBreakProcess = nil), ['WARNING: Failed to get DebugBreakProcess']);
  DebugLn(DBG_WARNINGS and (_GetThreadDescription = nil), ['WARNING: Failed to get GetThreadDescription']);
  DebugLn(DBG_WARNINGS and (_CancelSynchronousIo = nil), ['WARNING: Failed to get CancelSynchronousIo']);
  {$ifdef cpux86_64}
  DebugLn(DBG_WARNINGS and (_IsWow64Process = nil), ['WARNING: Failed to get IsWow64Process']);
  DebugLn(DBG_WARNINGS and (_Wow64GetThreadContext = nil), ['WARNING: Failed to get Wow64GetThreadContext']);
  DebugLn(DBG_WARNINGS and (_Wow64SetThreadContext = nil), ['WARNING: Failed to get Wow64SetThreadContext']);
  DebugLn(DBG_WARNINGS and (_Wow64SuspendThread = nil), ['WARNING: Failed to get _Wow64SuspendThread']);
  {$endif}
end;

//TDbgWinThreadName

procedure TDbgWinThreadNameInternal.SetInfo(id:TThreadID;const threadname:string);
begin
 Fid:=id;
 Fthreadname:=threadname;
end;

//

function TDbgWinThreadNameList.CreateItem: TLinkListItem;
begin
 Result:=TLinkListItem(TDbgWinThreadNameInternal.Create);
end;

procedure TDbgWinThreadNameList.ClearThread;
begin
 Clear;
 FNumCounter:=0;
end;

function TDbgWinThreadNameList.FindById(id:TThreadID):TDbgWinThreadNameInternal;
var
 node:TDbgWinThreadNameInternal;
begin
 Result:=nil;
 node:=TDbgWinThreadNameInternal(First);
 while (node<>nil) do
 begin
  if (node.Fid=id) then
  begin
   Exit(node);
  end;
  node:=TDbgWinThreadNameInternal(node.Next);
 end;
end;

function TDbgWinThreadNameList.FetchThread(id:TThreadID):string;
var
 node:TDbgWinThreadNameInternal;
begin
 Result:='';
 node:=FindById(id);
 if (node<>nil) then
 begin
  Result:=node.Fthreadname;
  Delete(TLinkListItem(node));
  Dec(FNumCounter);
 end;
end;

procedure TDbgWinThreadNameList.AddThread(id:TThreadID;const threadname:string);
var
 node:TDbgWinThreadNameInternal;
begin
 node:=FindById(id);
 if (node<>nil) then
 begin
  node.SetInfo(id,threadname);
 end else
 begin

  if (FNumCounter>=FMaxCounter) then
  begin
   //limit
   node:=TDbgWinThreadNameInternal(First);
   if (node=nil) then Exit;
   node.SetInfo(id,threadname);
   MoveToLast(TLinkListItem(node));
  end else
  begin
   node:=TDbgWinThreadNameInternal(GetNewItem);
   node.SetInfo(id,threadname);
   AddAsLast(TLinkListItem(node));
   Inc(FNumCounter);
  end;

 end;
end;

//TThreadNameMap

procedure TDbgWinProcess.LogLastError(AMsg: String);
begin
  if not GotExitProcess then
    DebugLn(DBG_WARNINGS, 'FpDbg-ERROR: %s -> %s', [AMsg, GetLastErrorText]);
end;

function TDbgWinProcess.GetImageBase: QWord;
begin
  if ThreadID <> 0 then
    Result := QWord(FInfo.lpBaseOfImage)
  else
    Result := inherited GetImageBase;
end;

procedure TDbgWinProcess.AfterChangingInstructionCode(const ALocation: TDBGPtr;
  ACount: Integer);
begin
  inherited AfterChangingInstructionCode(ALocation, ACount);
  FlushInstructionCache(Handle, Pointer(PtrUInt(ALocation)), 1);
  //FlushInstructionCache(Handle, nil, 0);
end;

function TDbgWinProcess.GetFullProcessImageName(AProcessHandle: THandle): string;
var
  u: UnicodeString;
  len: DWORD;
begin
  Result := '';
  if _QueryFullProcessImageName = nil then
    exit;
  len := MAX_PATH;
  SetLength(u, len);
  if _QueryFullProcessImageName(AProcessHandle, 0, @u[1], len)
  then begin
    SetLength(u, len);
    Result:=UTF8Encode(u);
  end
  else begin
    LogLastError;
  end;
end;

function TDbgWinProcess.GetModuleFileName(AModuleHandle: THandle): string;
var
  u: UnicodeString;
  s: string;
  len: Integer;
begin
  result := '';

  // GetFinalPathNameByHandle is only available on Windows Vista / Server 2008
  if assigned(_GetFinalPathNameByHandle) then begin
    SetLength(u, MAX_PATH+1);

    len := _GetFinalPathNameByHandle(AModuleHandle, @u[1], MAX_PATH, 0);
    s:='';
    if len > 0
    then begin
      // On some older Windows versions there's a bug in GetFinalPathNameByHandleW,
      // which leads to a trailing #0.
      if (u[len]=#0) then
        dec(len);
      SetLength(u, len);
      s:=UTF8Encode(u);
    end else begin
      u := '';
      LogLastError;
    end;
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
var
  FileInformation: TByHandleFileInformation;
  Loader: TDbgImageLoader;
begin
  Loader := nil;
  if GetFileInformationByHandle(FInfo.hFile, FileInformation) then
    Loader := TDbgImageLoaderLibrary.Create(FInfo.hFile, nil, TDBGPtr(FInfo.lpBaseOfDll))
  else if Name <> '' then
    begin
    // There are situations in which the provided handle is not a file-handle. In
    // those cases, use the filename as fallback.
    // (Happened in a Windows-docker (Azure, AKS) on the kernel32.dll. No idea
    // why, though)
    if FileExists(Name) then
      Loader := TDbgImageLoaderLibrary.Create(Name, nil, TDBGPtr(FInfo.lpBaseOfDll))
    else
      DebugLn(DBG_WARNINGS, 'File [%s] related to library does not exist', [Name]);
    end;
  if Assigned(Loader) and Loader.IsValid then
    Loader.AddToLoaderList(LoaderList)
  else
    Loader.Free;
end;

function tDbgWinLibrary.GetImageBase: QWord;
begin
  Result := QWord(FInfo.lpBaseOfDll);
end;

constructor tDbgWinLibrary.Create(const AProcess: TDbgProcess;
  const ADefaultName: String; const AModuleHandle: THandle;
  AInfo: TLoadDLLDebugInfo);
var
  S: String;
begin
  inherited Create(AProcess, ADefaultName, AModuleHandle);
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
var
  FileInformation: TByHandleFileInformation;
  Loader: TDbgImageLoader;
begin
  Loader := nil;
  if GetFileInformationByHandle(FInfo.hFile, FileInformation) then
    Loader := TDbgImageLoader.Create(FInfo.hFile, nil, TDbgPtr(FInfo.lpBaseOfImage))
  else if Name <> '' then
    begin
    // There are situations in which the provided handle is not a file-handle. In
    // those cases, use the filename as fallback.
    // (Happened in a Windows-docker (Azure, AKS) on the kernel32.dll. No idea
    // why, though)
    if FileExists(Name) then
      Loader := TDbgImageLoader.Create(Name, nil, TDBGPtr(FInfo.lpBaseOfImage))
    else
      DebugLn(DBG_WARNINGS, 'File [%s] related to the process does not exist', [Name]);
    end;
  if Assigned(Loader) and Loader.IsValid then
    Loader.AddToLoaderList(LoaderList)
  else
    Loader.Free;
end;

function TDbgWinProcess.CreateWatchPointData: TFpWatchPointData;
begin
  Result := TFpIntelWatchPointData.Create;
end;

constructor TDbgWinProcess.Create(const AFileName: string;
  AnOsClasses: TOSDbgClasses; AMemManager: TFpDbgMemManager;
  AMemModel: TFpDbgMemModel; AProcessConfig: TDbgProcessConfig);
begin
  {$ifdef cpui386}
  FBitness := b32;
  {$else}
  FBitness := b64;
  {$endif}
  FThreadNameList := TDbgWinThreadNameList.Create;
  inherited Create(AFileName, AnOsClasses, AMemManager, AMemModel, AProcessConfig);
end;

destructor TDbgWinProcess.Destroy;
begin
  FInfo.hProcess:=0;
  FProcProcess.Free;
  FThreadNameList.Free;
  inherited Destroy;
end;

function TDbgWinProcess.ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean;
var
  BytesRead: PtrUInt;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgWinProcess.ReadData');{$ENDIF}
  assert(MDebugEvent.dwProcessId <> 0, 'TDbgWinProcess.ReadData: MDebugEvent.dwProcessId <> 0');
  Result := ReadProcessMemory(Handle, Pointer(PtrUInt(AAdress)), @AData, ASize, BytesRead) and (BytesRead = ASize);

  if Result then
    MaskBreakpointsInReadData(AAdress, ASize, AData)
  else
    LogLastError('ReadData '+dbghex(int64(AAdress))+' / '+dbgs(ASize) + '(done: '+dbgs(BytesRead)+' )');
end;

function TDbgWinProcess.WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean;
var
  BytesWritten: PtrUInt;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgWinProcess.WriteData');{$ENDIF}
  assert(MDebugEvent.dwProcessId <> 0, 'TDbgWinProcess.WriteData: MDebugEvent.dwProcessId <> 0');
  Result := WriteProcessMemory(Handle, Pointer(PtrUInt(AAdress)), @AData, ASize, BytesWritten) and (BytesWritten = ASize);

  if not Result then
    LogLastError('WriteData '+dbghex(int64(AAdress))+' / '+dbgs(ASize) + '(done: '+dbgs(BytesWritten)+' )');
end;

function TDbgWinProcess.ReadString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: String): Boolean;
var
  BytesRead: PtrUInt;
  buf: array of Char;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgWinProcess.ReadString');{$ENDIF}
  assert(MDebugEvent.dwProcessId <> 0, 'TDbgWinProcess.ReadString: MDebugEvent.dwProcessId <> 0');
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
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgWinProcess.ReadWString');{$ENDIF}
  assert(MDebugEvent.dwProcessId <> 0, 'TDbgWinProcess.ReadWString: MDebugEvent.dwProcessId <> 0');
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

function TDbgWinProcess.CallParamDefaultLocation(AParamIdx: Integer
  ): TFpDbgMemLocation;
begin
  Result := InvalidLoc;
  case Mode of
    dm32: case AParamIdx of
       -1: Result := RegisterLoc(0); // EAX
        0: Result := RegisterLoc(0); // EAX
        1: Result := RegisterLoc(2); // EDX
        2: Result := RegisterLoc(1); // ECX
      else
        Result := UnInitializedLoc;
      end;
    dm64: case AParamIdx of
       -1: Result := RegisterLoc(0); // RAX
        0: Result := RegisterLoc(2); // RCX
        1: Result := RegisterLoc(1); // RDX
        2: Result := RegisterLoc(8); // R8
        3: Result := RegisterLoc(9); // R9
      else
        Result := UnInitializedLoc;
      end;
  end;
end;

procedure TDbgWinProcess.Interrupt;
begin
  //
end;

{ ------------------------------------------------------------------
  HandleDebugEvent

  Result: True if the event was triggered internally
          The callee should continue the process
  ------------------------------------------------------------------ }
function TDbgWinProcess.HandleDebugEvent(const ADebugEvent: TDebugEvent): Boolean;
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

function TDbgWinProcess.StartInstance(AParams, AnEnvironment: TStrings;
  AWorkingDirectory, AConsoleTty: string; AFlags: TStartInstanceFlags; out
  AnError: TFpError): boolean;
var
  LastErr: Integer;
begin
  result := false;
  FProcProcess := TProcessWithRedirect.Create(nil);
  try
    // To debug sub-processes, this needs to be poDebugProcess
    FProcProcess.Options:=[poDebugProcess, poDebugOnlyThisProcess, poNewProcessGroup];
    if siForceNewConsole in AFlags then
      FProcProcess.Options:=FProcProcess.Options+[poNewConsole];
    { Capture the debuggee's stdio into pipes so the debugger can read/write it
      (RedirectConsoleOutput / siRediretOutput) -- this is what fills
      GetConsoleOutput / SendConsoleInput below.
        - Skip when the caller asked for a SEPARATE console window
          (siForceNewConsole): its stdio must stay visible in that console, not
          be captured -- so Force New Console effectively turns capture off.
        - Skip when explicit file redirection is configured.
      stderr is merged into stdout for a single stream. poDetached
      (DETACHED_PROCESS) gives the captured child no console at all, so no empty
      console window appears; STARTF_USESTDHANDLES still routes its stdio to the
      inherited pipe handles (InheritHandles stays True). NOTE: do NOT use
      poNoConsole here -- CREATE_NO_WINDOW from a console-less (GUI) parent such
      as the IDE allocates a hidden console and defeats the pipe capture (stdio
      ends up on that fresh console instead of the inherited pipe handles). }
    if (siRediretOutput in AFlags) and not (siForceNewConsole in AFlags) and
       (Config.StdOutRedirFile = '') and (Config.StdErrRedirFile = '') and
       (Config.StdInRedirFile = '') then
      FProcProcess.Options := FProcProcess.Options + [poUsePipes, poStderrToOutPut, poDetached];
    FProcProcess.Executable:=Name;
    FProcProcess.Parameters:=AParams;
    FProcProcess.Environment:=AnEnvironment;
    FProcProcess.CurrentDirectory:=AWorkingDirectory;
    if Config.UseConsoleWinPos then begin
      FProcProcess.StartupOptions := FProcProcess.StartupOptions + [suoUsePosition];
      FProcProcess.WindowLeft   := Cardinal(Config.ConsoleWinPos.X);
      FProcProcess.WindowTop    := Cardinal(Config.ConsoleWinPos.Y);
    end;
    if Config.UseConsoleWinSize then begin
      FProcProcess.StartupOptions := FProcProcess.StartupOptions + [suoUseSize];
      FProcProcess.WindowWidth    := Cardinal(Config.ConsoleWinSize.X);
      FProcProcess.WindowHeight   := Cardinal(Config.ConsoleWinSize.Y);
    end;
    if Config.UseConsoleWinBuffer then begin
      FProcProcess.StartupOptions := FProcProcess.StartupOptions + [suoUseCountChars];
      FProcProcess.WindowColumns := Cardinal(Config.ConsoleWinBuffer.X);
      FProcProcess.WindowRows    := Cardinal(Config.ConsoleWinBuffer.Y);
    end;

    if DBG_PROCESS_HAS_REDIRECT then begin
      FProcProcess.SetRedirection(dtStdIn,  Config.StdInRedirFile,  Config.FileOverwriteStdIn);
      if (Config.StdOutRedirFile = Config.StdErrRedirFile) then begin
        if Config.StdOutRedirFile <> '' then begin
          FProcProcess.SetRedirection(dtStdOut, Config.StdOutRedirFile, Config.FileOverwriteStdOut or Config.FileOverwriteStdErr);
          FProcProcess.Options := FProcProcess.Options + [poStdErrToOutPut];
        end;
      end
      else begin
        FProcProcess.SetRedirection(dtStdOut, Config.StdOutRedirFile, Config.FileOverwriteStdOut);
        FProcProcess.SetRedirection(dtStdErr, Config.StdErrRedirFile, Config.FileOverwriteStdErr);
      end;

      if (Win32MajorVersion < 6) or
         ( (Win32MajorVersion = 6) and (Win32MinorVersion <= 1) )
      then
        FProcProcess.ApplyWin7Fix;
    end;

    FProcProcess.Execute;

    Init(FProcProcess.ProcessID, 0);

    FThreadNameList.ClearThread;

    Result:=true;
  except
    on E: Exception do
    begin
      LastErr := Integer(GetLastError);
      DebugLn(DBG_WARNINGS, 'Failed to start process "%s". Errormessage: "%s %d".',[Name, E.Message, LastErr]);
      {$ifdef cpui386}
      if (E is EProcess) and (GetLastError=50) then
      begin
        AnError := CreateError(fpErrCreateProcess, [Name, LastErr, E.Message, 'Note that on Windows it is not possible to debug a 64-bit application with a 32-bit debugger.'])
      end
      else
      {$endif i386}
      AnError := CreateError(fpErrCreateProcess, [Name, LastErr, E.Message, '']);
      FreeAndNil(FProcProcess);
    end;
  end;
end;

function TDbgWinProcess.AttachToInstance(APid: Integer; out AnError: TFpError
  ): boolean;
var
  LastErr: Integer;
begin
  Result := false;
  if _DebugActiveProcess = nil then begin
    AnError := CreateError(fpErrAttachProcess, [Name, 0, 'API unavailable', '']);
    exit;
  end;
  if not _DebugActiveProcess(APid) then begin
    LastErr := Integer(GetLastError);
    AnError := CreateError(fpErrAttachProcess, [Name, LastErr, GetLastErrorText(LastErr), '']);
    exit;
  end;

  Init(APid, 0);

  FThreadNameList.ClearThread;

  Result := true;
  // TODO: change the filename to the actual exe-filename. Load the correct dwarf info
end;

function TDbgWinProcess.CheckForConsoleOutput(ATimeOutMs: integer): integer;
var
  Avail, e: DWord;
  Deadline: QWord;
begin
  if (FProcProcess = nil) or (FProcProcess.Output = nil) then
    Exit(-1);

  if (_CancelSynchronousIo <> nil) and (CheckingForConsoleOutputThread <> nil) then begin
    if FGetConsoleBufferNeedSleep then sleep(10);
    try
      FGetConsoleBufferCnt := FProcProcess.Output.Read(FGetConsoleBuffer, 1);
      if FGetConsoleBufferCnt < 0 then FGetConsoleBufferCnt := 0; // Should never happen, but otherwise we should get the error on the next line
      Result := FProcProcess.Output.NumBytesAvailable + FGetConsoleBufferCnt;
    except
      Result := -1;
    end;
    FGetConsoleBufferNeedSleep := Result = 0;
    if (Result = 0) then begin
      e := GetLastError;
      if (e = ERROR_BROKEN_PIPE) or (e = ERROR_PIPE_NOT_CONNECTED) or
         (e = ERROR_INVALID_HANDLE)
      then
        Result := -1;
    end;
    exit;
  end;

  // Launched without pipe capture -> report "no console" (< 0 stops the IDE
  // reader thread; fpdmcp's pull just returns empty).
  Deadline := SysUtils.GetTickCount64 + QWord(ATimeOutMs);
  repeat
    try
      Avail := FProcProcess.Output.NumBytesAvailable;
    except
      Exit(-1);  // pipe broken / process gone
    end;
    if Avail > 0 then
      Exit(Integer(Avail));
    if ATimeOutMs <= 0 then
      Exit(0);
    Sleep(10);
  until StopCheckingForConsoleOutputRequested or (SysUtils.GetTickCount64 >= Deadline);
  Result := 0;
end;

procedure TDbgWinProcess.StopCheckingForConsoleOutput;
begin
  inherited StopCheckingForConsoleOutput;
  { The supported way to abort the read: it returns with
    ERROR_OPERATION_ABORTED and the loop then sees Terminated. }
  if (_CancelSynchronousIo <> nil) and (CheckingForConsoleOutputThread <> nil) then
    _CancelSynchronousIo(THandle(CheckingForConsoleOutputThread.Handle));
end;

function TDbgWinProcess.GetConsoleOutput: string;
var
  Avail: DWord;
  Buf: array of Byte;
  Got: LongInt;
begin
  Result := '';
  if FGetConsoleBufferCnt = 1 then
    Result := FGetConsoleBuffer;
  if (FProcProcess = nil) or (FProcProcess.Output = nil) then
    Exit;
  try
    Avail := FProcProcess.Output.NumBytesAvailable;
  except
    Exit;
  end;
  if Avail = 0 then
    Exit;
  FGetConsoleBufferNeedSleep := False; // there was something to be read after all
  SetLength(Buf, Avail+FGetConsoleBufferCnt);
  Buf[0] := ord(FGetConsoleBuffer);
  Got := FProcProcess.Output.Read(Buf[FGetConsoleBufferCnt], Avail);
  if Got > 0 then
    SetString(Result, PAnsiChar(@Buf[0]), Got);
end;

procedure TDbgWinProcess.SendConsoleInput(AString: string);
begin
  if (FProcProcess <> nil) and (FProcProcess.Input <> nil) and (AString <> '') then
    FProcProcess.Input.Write(AString[1], Length(AString));
end;

function TDbgWinProcess.CanContinueForWatchEval(ACurrentThread: TDbgThread
  ): boolean;
begin
  Result := inherited CanContinueForWatchEval(ACurrentThread);
  Result := Result and
    ( (TDbgWinThread(ACurrentThread).FHasExceptionCleared) or
      (MDebugEvent.dwDebugEventCode <> EXCEPTION_DEBUG_EVENT) or
      (MDebugEvent.Exception.ExceptionRecord.ExceptionCode = EXCEPTION_BREAKPOINT) or
      (MDebugEvent.Exception.ExceptionRecord.ExceptionCode = STATUS_WX86_BREAKPOINT) or
      (MDebugEvent.Exception.ExceptionRecord.ExceptionCode = EXCEPTION_SINGLE_STEP) or
      (MDebugEvent.Exception.ExceptionRecord.ExceptionCode = STATUS_WX86_SINGLE_STEP)
    );
end;

function TDbgWinProcess.Continue(AProcess: TDbgProcess; AThread: TDbgThread;
  SingleStep: boolean): boolean;

  function HasThreadInSkippingBreak: Boolean;
  var
    t: TDbgThread;
  begin
    Result := False;
    for t in FThreadMap do
      if TDbgWinThread(t).FBreakPointState = bsInSingleStep then begin
        Result := True;
        break;
      end;
  end;

var
  EventThread, t: TDbgThread;
  WinEventThread: TDbgWinThread absolute EventThread;
  WinAThread: TDbgWinThread absolute AThread;
  HasExceptionCleared, EventThreadNeedsTempBrkRemove, RaiseSingleStep: Boolean;
begin
  debugln(FPDBG_WINDOWS, ['TDbgWinProcess.Continue ',SingleStep, ' # ', ' # ',DbgSTime]);
  HasExceptionCleared := (WinAThread <> nil) and WinAThread.FHasExceptionCleared;
  RaiseSingleStep := (udeReRaiseExternalWatchPoint in HandleUserDebugEvents) and
    (WinAThread <> nil) and WinAThread.HitExternalWatchPoint;

  if assigned(AThread) and not FThreadMap.HasId(AThread.ID) then begin
    AThread := nil;
  end;

  (* In case a thread needs to single-step over a (temp-removed) breakpoint,
     other events (from suspended threads, if the event is already triggered)
     can be received. THe single step must be continued until finished.
     This may mean suspending the current thread.
  *)

  (* AThread  versus  EventThread

   * AThread:
     - AThread is ONLY passed for the "SingleStep" parameter.

     - If AThread is at breakpoint, and AThread is *not* the event-thread, then
       AThread must still hit that breakpoint.
       Only the event-thread has been checked for being at a breakpoint.

   * EventThread
     - The event-thread will have been checked for being at a breakpoint.
       It therefore must always step-over, if it is at a breakpoint

     - Except, if the event-thread is at a hardcoded breakpoint.
       In that case:
       ~ The controller has handled, the hardcoded breakpoint.
       ~ The IP was *not* reset.
         So the event-thread may already be at the *next* breakpoint.
  *)

  EventThreadNeedsTempBrkRemove := False;
  if AProcess.GetThread(MDebugEvent.dwThreadId, EventThread) then begin
    EventThreadNeedsTempBrkRemove :=
      (not EventThread.PausedAtHardcodeBreakPoint) and
      Process.HasInsertedBreakInstructionAtLocation(EventThread.GetInstructionPointerRegisterValue);

    if EventThreadNeedsTempBrkRemove then
      WinEventThread.FBreakPointState := bsInSingleStep;

    if ( (EventThread = AThread) and SingleStep ) or
       ( EventThreadNeedsTempBrkRemove )
    then
      WinEventThread.SetSingleStep;
    assert((WinEventThread.FBreakPointState=bsNone) or WinEventThread.NextIsSingleStep, 'TDbgWinProcess.Continue: (WinEventThread.FBreakPointState=bsNone) or WinEventThread.NextIsSingleStep');
  end;

  if (AThread <> nil) and (AThread <> EventThread) and SingleStep then
    WinAThread.SetSingleStep;

  if EventThreadNeedsTempBrkRemove or HasThreadInSkippingBreak then begin
    debugln(FPDBG_WINDOWS or DBG_VERBOSE, '## Skip BrkPoint: EvntThread Nil=%s ISS=%s TmpRmBreak=%s / Thread Nil=%s ISS=%s ',
      [ dbgs(EventThread <> nil), dbgs((EventThread<>nil) and EventThread.NextIsSingleStep), dbgs(EventThreadNeedsTempBrkRemove),
        dbgs(AThread <> nil), dbgs((AThread<>nil) and AThread.NextIsSingleStep)  ]);
    for t in FThreadMap do
      TDbgWinThread(t).SuspendForStepOverBreakPoint;
  end;

  for t in FThreadMap do
    if (t <> AThread) and (t.SuspendCount > 0) then
      TDbgWinThread(t).Suspend;

  AProcess.ThreadsBeforeContinue;
  if AThread<>nil then debugln(FPDBG_WINDOWS, ['## ath.iss ',AThread.NextIsSingleStep]);

  if HasExceptionCleared then
    result := Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE)
  else
  if MDebugEvent.dwDebugEventCode = EXCEPTION_DEBUG_EVENT then
    case MDebugEvent.Exception.ExceptionRecord.ExceptionCode of
      EXCEPTION_SINGLE_STEP, STATUS_WX86_SINGLE_STEP: begin
        if RaiseSingleStep then
          result := Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_EXCEPTION_NOT_HANDLED)
        else
          result := Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE);
      end;
      EXCEPTION_BREAKPOINT, STATUS_WX86_BREAKPOINT: begin
        result := Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE);
      end;
    otherwise
      result := Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_EXCEPTION_NOT_HANDLED);
    end
  else
    result := Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE);
  DebugLn((FPDBG_WINDOWS or DBG_WARNINGS) and (not Result), 'ContinueDebugEvent failed: %d', [Windows.GetLastError]);
  result := true;
  MDebugEvent.dwProcessId := 0; // Flag as running // for assert in ReadThreadState
end;

function TDbgWinProcess.Detach(AProcess: TDbgProcess; AThread: TDbgThread
  ): boolean;
var
  t: TDbgWinThread;
  PendingDebugEvent: TDebugEvent;
begin
  Result := _DebugActiveProcessStop <> nil;
  if not Result then
    exit;

  RemoveAllBreakPoints;

  // Collect all pending events // Deal with any breakpoint/int3 hit
  if not GetThread(MDebugEvent.dwThreadId, TDbgThread(AThread)) then begin
    assert(False, 'TDbgWinProcess.Detach: Missing thread');
    TDbgThread(AThread) := AddThread(MDebugEvent.dwThreadId);
  end;

  for TDbgThread(t) in FThreadMap do
    if not t.ID = MDebugEvent.dwThreadId then
      t.Suspend;

  TDbgWinThread(AThread).SetSingleStep;
  Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE);
  while Windows.WaitForDebugEvent(PendingDebugEvent, 1) do begin
    if PendingDebugEvent.dwThreadId = MDebugEvent.dwThreadId then
      break;
    case PendingDebugEvent.dwDebugEventCode of
      CREATE_PROCESS_DEBUG_EVENT: begin
          if PendingDebugEvent.CreateProcessInfo.hFile <> 0 then
            CloseHandle(PendingDebugEvent.CreateProcessInfo.hFile);
          _DebugActiveProcessStop(PendingDebugEvent.dwProcessId);
        end;
      EXCEPTION_DEBUG_EVENT:
        case PendingDebugEvent.Exception.ExceptionRecord.ExceptionCode of
          EXCEPTION_BREAKPOINT, STATUS_WX86_BREAKPOINT: begin
            if not GetThread(PendingDebugEvent.dwThreadId, TDbgThread(t)) then
              TDbgThread(t) := AddThread(PendingDebugEvent.dwThreadId);
            t.CheckAndResetInstructionPointerAfterBreakpoint;
          end;
        end;
    end;
    Windows.ContinueDebugEvent(PendingDebugEvent.dwProcessId, PendingDebugEvent.dwThreadId, DBG_CONTINUE);
  end;

  for TDbgThread(t) in FThreadMap do
    t.Resume;

  Result := _DebugActiveProcessStop(ProcessID);
//  Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE);
end;

function TDbgWinProcess.WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean;
const
  DBG_POLL_WAIT_TIMEOUT = 500;
var
  t: TDbgWinThread;
  Done: Boolean;
  LastErr: DWORD;
  h: THandle;
begin
  repeat
    Done := True;
    repeat
      MDebugEvent:=Default(TDebugEvent);
      Result := False;
      if _WaitForDebugEventEx <> nil then
        result := _WaitForDebugEventEx(MDebugEvent, DBG_POLL_WAIT_TIMEOUT)
      else
        result := Windows.WaitForDebugEvent(MDebugEvent, DBG_POLL_WAIT_TIMEOUT);

      if not Result then begin
        LastErr := Windows.GetLastError;
        if (LastErr = ERROR_SEM_TIMEOUT) then begin
          h := Handle;
          if (h<>0) and (WaitForSingleObject (Handle, 0) = 0) then begin
            MDebugEvent.dwDebugEventCode := EXIT_PROCESS_DEBUG_EVENT;
            MDebugEvent.ExitProcess.dwExitCode := 0;
            MDebugEvent.dwProcessId := ProcessID;
            ProcessIdentifier:=0;
            GotExitProcess := True;
            exit(True);
          end;
          system.continue;
        end;
        DebugLn( 'WaitForDebugEvent failed: %d', [LastErr]);
      end;
      DebugLn(FPDBG_WINDOWS and (not Result), 'WaitForDebugEvent failed: %d', [Windows.GetLastError]);

      break;
    until False;

    if Result and FTerminated and (MDebugEvent.dwDebugEventCode <> EXIT_PROCESS_DEBUG_EVENT)
       and (MDebugEvent.dwDebugEventCode <> EXIT_THREAD_DEBUG_EVENT)
    then begin
      // Wait for the terminate event // Do not report any queued breakpoints
      DebugLn(FPDBG_WINDOWS, ['Terminating... Skipping event: ', dbgs(MDebugEvent)]);
      for TDbgThread(t) in FThreadMap do
        t.Suspend;
      Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE);
      Done := False;
    end

    else
    if Result and (MDebugEvent.dwProcessId <> Self.ProcessID) then begin
      (* Some events are not processed yet anyway.
         They never reach AnalyseDebugEvent, so deal with them here
      *)
      case MDebugEvent.dwDebugEventCode of
        CREATE_PROCESS_DEBUG_EVENT: begin
            //child process: ignore
            // we currently do not use the file handle => close it
            if MDebugEvent.CreateProcessInfo.hFile <> 0 then
              if not CloseHandle(MDebugEvent.CreateProcessInfo.hFile) then
                debugln(DBG_WARNINGS, ['Failed to close new process file handle: ',GetLastErrorText]);
            if _DebugActiveProcessStop <> nil then
              if not _DebugActiveProcessStop(MDebugEvent.dwProcessId) then
                debugln(DBG_WARNINGS, ['Failed to detach: ',GetLastErrorText]);

            Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE);
            Done := False;
          end;
        EXIT_PROCESS_DEBUG_EVENT: begin
            // Should never be here, since it detached
            FThreadNameList.ClearThread;
            Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE);
            Done := False;
          end;
      end;
    end;
  until Done;

  ProcessIdentifier:=MDebugEvent.dwProcessId;
  ThreadIdentifier:=MDebugEvent.dwThreadId;
  {$IFDEF DebuglnWinDebugEvents}
  DebugLn(FPDBG_WINDOWS, [dbgs(MDebugEvent), ' ', Result, ' # ',DbgSTime]);
  for TDbgThread(t) in FThreadMap do begin
  if t.ReadThreadState then
    DebugLn(FPDBG_WINDOWS,
      'Thr.Id:%d %x  SSTep %s EF %s     DR6:%x  DR7:%x  WP:%x  RegAcc: %d,  SStep: %d  Task: %d, ExcBrk: %d Susp: %s, ISS: %s BS:%s',
      [t.ID, t.GetInstructionPointerRegisterValue, dbgs(t.FCurrentContext^.def.EFlags and FLAG_TRACE_BIT), dbghex(t.FCurrentContext^.def.EFlags), t.FCurrentContext^.def.Dr6, t.FCurrentContext^.def.Dr7, t.FCurrentContext^.def.Dr6 and 15, t.FCurrentContext^.def.Dr6 and (1<< 13), t.FCurrentContext^.def.Dr6 and (1<< 14), t.FCurrentContext^.def.Dr6 and (1<< 15), t.FCurrentContext^.def.Dr6 and (1<< 16), dbgs(t.FIsSuspended), dbgs(t.NextIsSingleStep), dbgs(t.FBreakPointState) ]);
  end;
  {$ENDIF}

  RestoreTempBreakInstructionCodes;
  if not FTerminated then
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
  begin
    if (DBG_VERBOSE = nil) or (not DBG_VERBOSE^.Enabled) then
      exit;
    DebugLn('===');
    DebugLn(AEvent);
    DebugLn('---');
    DebugLn('Process ID: '+ IntToSTr(MDebugEvent.dwProcessId));
    DebugLn('Thread ID: '+ IntToStr(MDebugEvent.dwThreadId));

    if AThread = nil then Exit;
    if not TDbgWinThread(AThread).HasContext then Exit;

    TDbgWinThread(AThread).DumpContext;
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
    if OnDebugOutputEvent <> nil then
      OnDebugOutputEvent(Self, AEvent.dwProcessId, AEvent.dwThreadId, S);
  end;

const
   EXCEPTION_SET_THREADNAME = $406D1388;
var
  InterceptAtFirst: Boolean;
  threadname: String;
  t: TDbgThread;
  Lib: TDbgLibrary;
  FpProc: TFpSymbol;
begin
  if AThread <> nil then
    TDbgWinThread(AThread).EndSingleStepOverBreakPoint;

  if HandleDebugEvent(MDebugEvent)
  then result := deBreakpoint // unreachable
  else begin

    case MDebugEvent.dwDebugEventCode of
      EXCEPTION_DEBUG_EVENT: begin
        //DumpEvent('EXCEPTION_DEBUG_EVENT');
        case MDebugEvent.Exception.ExceptionRecord.ExceptionCode of
          EXCEPTION_BREAKPOINT, STATUS_WX86_BREAKPOINT: begin
            if FJustStarted and (MDebugEvent.Exception.dwFirstChance <> 0) and (MDebugEvent.Exception.ExceptionRecord.ExceptionFlags = 0) then
            begin
              FJustStarted:=false;
              result := deInternalContinue;
            end
            else
            if (AThread <> nil) and (TDbgWinThread(AThread).FIgnoreNextInt3) then begin
              result := deInternalContinue; // pause request
              TDbgWinThread(AThread).FIgnoreNextInt3 := False;
            end
            else begin
              result := deBreakpoint;
              if AThread <> nil then
                TDbgWinThread(AThread).ResetInstructionPointerAfterBreakpoint; // This is always an int3 breakpoint
            end;
          end;
          EXCEPTION_SINGLE_STEP, STATUS_WX86_SINGLE_STEP: begin
            // includes WatchPoints
            result := deBreakpoint;
          end;
          EXCEPTION_SET_THREADNAME: begin
            if AThread <> nil then begin
              if not ReadString(TDbgPtr(MDebugEvent.Exception.ExceptionRecord.ExceptionInformation[1]), 200, threadname) then
                threadname := 'error getting threadname';
              t := AThread;
              with MDebugEvent.Exception.ExceptionRecord do begin
                if (NumberParameters >= 3) and
                   ((ExceptionInformation[0] and $ffffffff) = $1000)
                then begin
                  if not(udeReRaiseWin32ThreadNameException in HandleUserDebugEvents) then
                    AThread.ClearExceptionSignal;
                  if (TThreadID(ExceptionInformation[2]) <> 0) and
                     (TThreadID(ExceptionInformation[2]) <> TThreadID(-1))
                  then begin
                    if not GetThread(Integer(ExceptionInformation[2]), t) then
                      t := nil;
                  end;
                end;
              end;
              if t <> nil then begin
                with TDbgWinThread(t) do begin
                  FName := threadname;
                  FDoNotPollName := True;
                end;
              end else
              with MDebugEvent.Exception.ExceptionRecord do
              begin
               FThreadNameList.AddThread(TThreadID(ExceptionInformation[2]),threadname);
              end;
            end;
            result := deInternalContinue;
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
        if PauseRequested then begin
          if FDbgUiRemoteBreakin = 0 then begin
            FDbgUiRemoteBreakin := TDBGPtr(-1);
            for Lib in LibMap do
              if (lowercase(Lib.Name) = 'ntdll.dll') or
                 (lowercase(copy(Lib.Name, length(Lib.Name)-9, 10)) = '\ntdll.dll')
              then begin
                FpProc := Lib.SymbolTableInfo.FindProcSymbol('DbgUiRemoteBreakin', True);
                if (FpProc <> nil) and (FpProc.Address.Address <> 0) then begin
                  FDbgUiRemoteBreakin := FpProc.Address.Address;
                  FpProc.ReleaseReference;
                end;
                break;
              end;
          end;
          if (FDbgUiRemoteBreakin <> TDBGPtr(-1)) and (TDBGPtr(MDebugEvent.CreateThread.lpStartAddress) = FDbgUiRemoteBreakin) and (AThread <> nil) then
            TDbgWinThread(AThread).FIgnoreNextInt3 := True;
        end;
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
        FThreadNameList.ClearThread;
        Windows.ContinueDebugEvent(MDebugEvent.dwProcessId, MDebugEvent.dwThreadId, DBG_CONTINUE);
        result := deExitProcess;
      end;
      LOAD_DLL_DEBUG_EVENT: begin
        //DumpEvent('LOAD_DLL_DEBUG_EVENT');
        result := deLoadLibrary;
      end;
      UNLOAD_DLL_DEBUG_EVENT: begin
        //DumpEvent('UNLOAD_DLL_DEBUG_EVENT');
        result := deUnloadLibrary;
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
var
  threadname: string;
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

  if (result<>nil) then
  begin
   threadname:=FThreadNameList.FetchThread(result.ID);
   if (threadname<>'') then
    with TDbgWinThread(result) do
    begin
      FName := threadname;
      FDoNotPollName := True;
    end;
  end;
end;

procedure TDbgWinProcess.StartProcess(const AThreadID: DWORD;const AInfo: TCreateProcessDebugInfo);
var
  s: string;
  {$ifNdef cpui386}
  b: BOOL;
  {$endif}
begin
  FInfo := AInfo;
  if ThreadID = 0 then
    SetThreadId(AThreadID);
  {$ifdef cpui386}
  FBitness := b32; // only 32 bit supported
  {$else}
  if (_IsWow64Process <> nil) and _IsWow64Process(GetHandle, @b) then begin
    if b then
      FBitness := b32
    else
      FBitness := b64;
  end
  else
    FBitness := b64;
  {$endif}

  s := GetProcFilename(Self, AInfo.lpImageName, AInfo.fUnicode, 0);
  if s <> ''
  then SetFileName(s);
end;

function TDbgWinProcess.Pause: boolean;
var
  hndl: Handle;
  hThread: THandle;
  NewThreadId: Cardinal;
begin
  //hndl := OpenProcess(PROCESS_CREATE_THREAD or PROCESS_QUERY_INFORMATION or PROCESS_VM_OPERATION or PROCESS_VM_WRITE or PROCESS_VM_READ, False, TargetPID);
  hndl := OpenProcess(PROCESS_ALL_ACCESS, false, ProcessID);
  PauseRequested:=true;
  Result := False;
  if _DebugBreakProcess <> nil then
    Result := _DebugBreakProcess(hndl);
  if not Result then begin
    DebugLn(DBG_WARNINGS, ['pause failed(1) ', GetLastError]);
    if (_CreateRemoteThread <> nil) and (DebugBreakAddr <> nil) then begin
      hThread := _CreateRemoteThread(hndl, nil, 0, DebugBreakAddr, nil, 0, NewThreadId);
      if hThread = 0 then begin
        DebugLn(DBG_WARNINGS, ['pause failed(2) ', GetLastError]);
      end
      else begin
        Result := True;
        CloseHandle(hThread);
      end;
    end;
  end;
  CloseHandle(hndl);
end;

procedure TDbgWinProcess.TerminateProcess;
begin
  Windows.TerminateProcess(Handle, 0);
  FTerminated := True;
  FThreadNameList.ClearThread;
end;

function TDbgWinProcess.AddLib(const AInfo: TLoadDLLDebugInfo): TDbgLibrary;
begin
  Result := TDbgWinLibrary.Create(Self, HexValue(AInfo.lpBaseOfDll, SizeOf(Pointer), [hvfIncludeHexchar]), AInfo.hFile, AInfo);
  AddLibrary(Result, TDbgPtr(AInfo.lpBaseOfDll));
end;

procedure TDbgWinProcess.RemoveLib(const AInfo: TUnloadDLLDebugInfo);
var
  Lib: TDbgLibrary;
  ID: TDbgPtr;
begin
  if FLibMap = nil then Exit;
  ID := TDbgPtr(AInfo.lpBaseOfDll);
  if not FLibMap.GetData(ID, Lib) then Exit;
  FSymInstances.Remove(Lib);
  FLibMap.Delete(ID);
end;

{ TDbgWinThread }

function TDbgWinThread.GetName: String;
var
  n: PWSTR;
begin
  Result := '';
  if FDoNotPollName then begin
    Result := FName;
  end else begin
    if _GetThreadDescription <> nil then
      if Succeeded(_GetThreadDescription(Handle, @n)) then begin
        Result := WideCharToString(n);
        LocalFree(HLOCAL(n));
      end;
  end;
  if Result = '' then
    Result := inherited GetName;
end;

procedure TDbgWinThread.DumpContext;
begin
  //
end;

function TDbgWinThread.GetStackUnwinder: TDbgStackUnwinder;
begin
  if FUnwinder = nil then
    FUnwinder := TDbgStackUnwinderX86MultiMethod.Create(Process);
  Result := FUnwinder;
end;

destructor TDbgWinThread.Destroy;
begin
  FUnwinder.Free;
  inherited Destroy;
end;

procedure TDbgWinThread.Suspend;
var
  r: DWORD;
begin
  if FIsSuspended then
    exit;
  {$ifdef cpux86_64}
  if (Process.Mode = dm32) and (_Wow64SuspendThread <> nil) then
    r := _Wow64SuspendThread(Handle)
  else
  {$endif}
  r := SuspendThread(Handle);
  FIsSuspended := r <> DWORD(-1);
  debugln(DBG_WARNINGS and (r = DWORD(-1)), 'Failed to suspend Thread %d (handle: %d). Error: %s', [Id, Handle, GetLastErrorText]);
end;

procedure TDbgWinThread.SuspendForStepOverBreakPoint;
var
  t: TDBGPtr;
begin
  t := GetInstructionPointerRegisterValue;
  if (FBreakPointState = bsInSingleStep)
//     or  (NextIsSingleStep)
  then begin
    Process.TempRemoveBreakInstructionCode(t);
  end
  else
  if NextIsSingleStep and (not Process.HasInsertedBreakInstructionAtLocation(t)) then begin
    // nothing / do the single step
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

procedure TDbgWinThread.EndSingleStepOverBreakPoint;
begin
  FBreakPointState := bsNone;
end;

procedure TDbgWinThread.BeforeContinue;
begin
  inherited;
  FHasResetInstructionPointerAfterBreakpoint := False;
  FAtHardCodeBreakpoint := False;
end;

procedure TDbgWinThread.ClearExceptionSignal;
begin
  inherited ClearExceptionSignal;
  FHasExceptionCleared := True;
end;

initialization
  LoadKernelEntryPoints;

  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  FPDBG_WINDOWS := DebugLogger.FindOrRegisterLogGroup('FPDBG_WINDOWS' {$IFDEF FPDBG_WINDOWS} , True {$ENDIF} );

end.

