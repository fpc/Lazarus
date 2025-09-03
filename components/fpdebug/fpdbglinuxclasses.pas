unit FpDbgLinuxClasses;

{$mode objfpc}{$H+}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}
{$packrecords c}
{$modeswitch advancedrecords}
{off $define DebuglnLinuxDebugEvents}

interface

uses
  Classes,
  SysUtils,
  BaseUnix,
  termio, fgl,
  Contnrs,
  StrUtils,
  Types,
  FpDbgClasses,
  FpDbgLoader, FpDbgDisasX86,
  DbgIntfBaseTypes, DbgIntfDebuggerBase, DbgIntfProcess,
  FpDbgLinuxExtra,
  FpDbgInfo,
  FpDbgUtil,
  UTF8Process,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif}, Maps,
  FpDbgCommon, FpdMemoryTools,
  FpErrorMessages,
  FpImgReaderBase,
  FpDbgCpuX86, LazDebuggerIntfFloatTypes;

type
  user_regs_struct64 = record
    r15: cuint64;
    r14: cuint64;
    r13: cuint64;
    r12: cuint64;
    rbp: cuint64;
    rbx: cuint64;
    r11: cuint64;
    r10: cuint64;
    r9 : cuint64;
    r8 : cuint64;
    rax: cuint64;
    rcx: cuint64;
    rdx: cuint64;
    rsi: cuint64;
    rdi: cuint64;
    orig_rax: cuint64;
    rip: cuint64;
    cs : cuint64;
    eflags: cuint64;
    rsp: cuint64;
    ss : cuint64;
    fs_base: cuint64;
    gs_base: cuint64;
    ds : cuint64;
    es : cuint64;
    fs : cuint64;
    gs : cuint64;
  end;

  user_fpregs_struct64 = record
    cwd : word;
    swd : word;
    ftw : word;
    fop : word;
    rip : qword;
    rdp : qword;
    mxcsr : dword;
    mxcr_mask : dword;
    st_space : array[0..31] of dword;
    xmm_space : array[0..63] of dword;
    padding : array[0..23] of dword;
  end;

  user64 = record
    regs : user_regs_struct64;
    u_fpvalid : longint;
    i387 : user_fpregs_struct64;
    u_tsize : qword;
    u_dsize : qword;
    u_ssize : qword;
    start_code : qword;
    start_stack : qword;
    signal : int64;
    reserved : longint;
//  case integer of
//    0: (
          u_ar0 : ^user_regs_struct32;
          __u_ar0_word : qword;
//       );
//      1: (u_fpstate : ^user_fpregs_struct32;
//          __u_fpstate_word : qword);
    magic : qword;
    u_comm : array[0..31] of char;
    u_debugreg : array[0..7] of qword;
  end;

  TUserRegs32 = array[0..26] of cuint32;
  TUserRegs64 = array[0..26] of cuint64;
  TUserRegs = record
    case integer of
      0: (regs32: TUserRegs32);
      1: (regs64: TUserRegs64);
  end;

  user_regs_struct32 = record
    ebx: cuint32;
    ecx: cuint32;
    edx: cuint32;
    esi: cuint32;
    edi: cuint32;
    ebp: cuint32;
    eax: cuint32;
    xds: cuint32;
    xes: cuint32;
    xfs: cuint32;
    xgs: cuint32;
    orig_eax: cuint32;
    eip: cuint32;
    xcs: cuint32;
    eflags: cuint32;
    esp: cuint32;
    xss: cuint32;
  end;

  user_fpxregs_struct32 = record
    cwd : word;
    swd : word;
    twd : word;
    fop : word;
    fip : longint;
    fcs : longint;
    foo : longint;
    fos : longint;
    mxcsr : longint;
    reserved : longint;
    st_space : array[0..31] of longint;
    xmm_space : array[0..31] of longint;
    padding : array[0..55] of longint;
  end;

  user_fpregs_struct32 = record
      cwd : longint;
      swd : longint;
      twd : longint;
      fip : longint;
      fcs : longint;
      foo : longint;
      fos : longint;
      st_space : array[0..19] of longint;
    end;

  user32 = record
    regs : user_regs_struct32;
    u_fpvalid : longint;
    i387 : user_fpregs_struct32;
    u_tsize : dword;
    u_dsize : dword;
    u_ssize : dword;
    start_code : dword;
    start_stack : dword;
    signal : longint;
    reserved : longint;
    u_ar0 : ^user_regs_struct32;
    u_fpstate : ^user_fpregs_struct32;
    magic : dword;
    u_comm : array[0..31] of char;
    u_debugreg : array[0..7] of longint;
  end;

function login_tty(__fd:longint):longint;cdecl;external 'c' name 'login_tty';
function openpty(__amaster:Plongint; __aslave:Plongint; __name:Pchar; __termp:pointer{Ptermios}; __winp:pointer{Pwinsize}):longint;cdecl;external 'util' name 'openpty';

const
  R15      = 0;
  R14      = 1;
  R13      = 2;
  R12      = 3;
  RBP      = 4;
  RBX      = 5;
  R11      = 6;
  R10      = 7;
  R9       = 8;
  R8       = 9;
  RAX      = 10;
  RCX      = 11;
  RDX      = 12;
  RSI      = 13;
  RDI      = 14;
  ORIG_RAX = 15;
  RIP      = 16;
  CS       = 17;
  EFLAGS   = 18;
  RSP      = 19;
  SS       = 20;
  FS_BASE  = 21;
  GS_BASE  = 22;
  DS       = 23;
  ES       = 24;
  FS       = 25;
  GS       = 26;

  EBX      = 0;
  ECX      = 1;
  EDX      = 2;
  ESI      = 3;
  EDI      = 4;
  EBP      = 5;
  EAX      = 6;
  XDS      = 7;
  XES      = 8;
  XFS      = 9;
  XGS      = 10;
  ORIG_EAX = 11;
  EIP      = 12;
  XCS      = 13;
  EFL      = 14;
  UESP     = 15;
  XSS      = 16;
  __WALL   = $40000000;

  NT_PRSTATUS    = 1;
  NT_PRFPREG     = 2;
  NT_PRPSINFO    = 3;
  NT_TASKSTRUCT  = 4;
  NT_AUXV        = 6;
  NT_X86_XSTATE  = $202;

type

  { TFpDbgLinuxSignal }

  TFpDbgLinuxSignal = record
    PID: THandle;
    WaitStatus: cint;
    class operator = (a, b: TFpDbgLinuxSignal): boolean;
  end;

  { TFpDbgLinuxSignalQueue }

  TFpDbgLinuxSignalQueue = class(specialize TFPGList<TFpDbgLinuxSignal>)
  public
    procedure AddSignal(APID: THandle; AWaitStatus: cint); overload;
    function GetNextSignal(out APID: THandle; out AWaitStatus: cint): Boolean;
  end;

  { TDbgLinuxMemoryMapping }

  // Corresponds to the structure in /proc/[pid]/maps, see the Linux documentation
  // 'man 5 proc' for more info.
  TDbgLinuxMemoryMapping = class(TObject)
    AddressFrom: PtrInt;
    AddressTill: PtrInt;
    Rights: string;
    Offset: PtrInt;
    DeviceId: string;
    Inode: QWord;
    FileName: string;
  end;
  TDbgLinuxMemoryMappingList = class(TObjectList);

  { TDbgLinuxThread }

  TDbgLinuxThread = class(TDbgx86Thread)
  private
    FFpRegs: user_fpxregs_struct32;
    FFpRegsAvail: Boolean;
    FUserRegs: TUserRegs;
    FStoredUserRegs: TUserRegs;
    FUserRegsChanged: boolean;
    FExceptionSignal: cint;
    FIsPaused, FInternalPauseRequested, FIsInInternalPause: boolean;
    FHasExited: Boolean;
    FIsSteppingBreakPoint: boolean;
    FDidResetInstructionPointer: Boolean;
    FHasThreadState: boolean;
    FUnwinder: TDbgStackUnwinderX86MultiMethod;
    function GetDebugRegOffset(ind: byte): pointer;
    function ReadDebugReg(ind: byte; out AVal: PtrUInt): boolean;
    function WriteDebugReg(ind: byte; AVal: PtrUInt): boolean;
    function GetName: String; override;
  protected
    function ReadThreadState: boolean;

    function RequestInternalPause: Boolean;
    function CheckSignalForPostponing(AWaitedStatus: cint): Boolean;
    procedure ResetPauseStates;
    function GetStackUnwinder: TDbgStackUnwinder; override;
  public
    destructor Destroy; override;
    function ResetInstructionPointerAfterBreakpoint: boolean; override;
    procedure ApplyWatchPoints(AWatchPointData: TFpWatchPointData); override;
    function DetectHardwareWatchpoint: Pointer; override;
    procedure BeforeContinue; override;
    procedure LoadRegisterValues; override;
    procedure SetRegisterValue(AName: string; AValue: QWord); override;
    procedure StoreRegisters; override;
    procedure RestoreRegisters; override;
    procedure ClearExceptionSignal; override;

    function GetInstructionPointerRegisterValue: TDbgPtr; override;
    function GetStackBasePointerRegisterValue: TDbgPtr; override;
    procedure SetInstructionPointerRegisterValue(AValue: TDbgPtr); override;
    procedure SetStackPointerRegisterValue(AValue: TDbgPtr); override;
    function GetStackPointerRegisterValue: TDbgPtr; override;
  end;

  tDbgLinuxLibrary = class;

  { TDbgLinuxProcess }

  TDbgLinuxProcess = class(TDbgx86Process)
  private
    FPostponedSignals: TFpDbgLinuxSignalQueue;
    FStatus: cint;
    FProcessStarted: boolean;
    FProcProcess: TProcessWithRedirect;
    FIsTerminating: boolean;
    FMasterPtyFd: cint;
    FCurrentThreadId: THandle;
    FSingleSteppingThreadID: THandle;
    // This breakpoint is triggered after dynamic libraries have been (un)loaded
    FSOLibEventBreakpoint: TFpDbgBreakpoint;
    {$ifndef VER2_6}
    procedure OnForkEvent(Sender : TObject);
    {$endif}
    function ReadWordSize(Adr: TDbgPtr; out AVal: TDBGPtr): boolean; inline;
    function WriteWordSize(Adr: TDbgPtr; AVal: TDBGPtr): boolean; inline;
  protected
    function GetRequiresExecutionInDebuggerThread: boolean; override;
    procedure InitializeLoaders; override;
    function CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread; override;
    function AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent; override;
    function CreateWatchPointData: TFpWatchPointData; override;
    // On Linux, dynamic loading of Libraries isn't done by the kernel, but by
    // a library. At startup a breakpoint is set within this library to provide
    // an hook for the event a library has been (un)loaded. This function
    // analyzes deBreakpoint events and checks if they are triggered by this
    // special breakpoint. And when this is the case, it evaluates the state
    // of the process to determine if a library has been (un)loaded or that
    // nothing has happened. And returns the corresponding TFPDEvent.
    // (deLoadLibrary, deUnloadLibrary, deInternalContinue or deBreakpoint when
    // the breakpoint is not this 'special' breakpoint).
    function CheckForSOLibDebugEvent(AThread: TDbgThread): TFPDEvent;
    // Searches in /proc/[pid]/maps for dynamically loaded libraries and
    // synchronizes these results with the list of loaded libraries.
    function SynchronizeProcMapsWithLibraryList: TFPDEvent;
    // Scan /proc/[pid]/maps and return the results
    function ObtainProcMaps: TDbgLinuxMemoryMappingList;
    procedure AddLib(const ALibrary: tDbgLinuxLibrary);
  public
    class function isSupported(ATargetInfo: TTargetDescriptor): boolean; override;
    constructor Create(const AFileName: string; AnOsClasses: TOSDbgClasses;
      AMemManager: TFpDbgMemManager; AMemModel: TFpDbgMemModel; AProcessConfig: TDbgProcessConfig = nil); override;
    destructor Destroy; override;

    function StartInstance(AParams, AnEnvironment: TStrings;
      AWorkingDirectory, AConsoleTty: string; AFlags: TStartInstanceFlags;
      out AnError: TFpError): boolean; override;

    function AttachToInstance(APid: Integer; out AnError: TFpError): boolean; override;

    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; override;
    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData; out APartSize: Cardinal): Boolean; override;
    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; override;
    function CallParamDefaultLocation(AParamIdx: Integer): TFpDbgMemLocation; override;

    function CheckForConsoleOutput(ATimeOutMs: integer): integer; override;
    function GetConsoleOutput: string; override;
    procedure SendConsoleInput(AString: string); override;

    procedure TerminateProcess; override;
    function Pause: boolean; override;
    function Detach(AProcess: TDbgProcess; AThread: TDbgThread): boolean; override;

    procedure LoadInfo; override;

    function CanContinueForWatchEval(ACurrentThread: TDbgThread): boolean; override;
    function Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean; override;
    function WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean; override;
  end;
  TDbgLinuxProcessClass = class of TDbgLinuxProcess;

  { tDbgLinuxLibrary }

  tDbgLinuxLibrary = class(TDbgLibrary)
  protected
    FLoadedTargetImageAddr: TDbgPtr;
    procedure InitializeLoaders; override;
  public
    constructor Create(const AProcess: TDbgProcess; const AFileName: string; const AModuleHandle: THandle; const ALoadedTargetImageAddr: TDbgPtr);
  end;


implementation

var
  DBG_VERBOSE, DBG_WARNINGS, FPDBG_LINUX: PLazLoggerLogGroup;
  GConsoleTty: string;
  GSlavePTyFd: cint;

type
  Tprocess_vm_readv = function(pid: pid_t; const local_iov: piovec;
    liovcnt: NativeUInt; const remote_iov: piovec; riovcnt: NativeUInt;
    flags: NativeUInt): ssize_t; cdecl;

var
  process_vm_lib: TLibHandle = 0;
  process_vm_readv: Tprocess_vm_readv = nil;

function dbgsThreadId(AThread: TDbgThread): string;
begin
  if AThread = nil then
    result := 'nil'
  else
    result := inttostr(AThread.ID);
end;

Function WIFSTOPPED(Status: Integer): Boolean;
begin
  WIFSTOPPED:=((Status and $FF)=$7F);
end;

{ tDbgLinuxLibrary }

procedure tDbgLinuxLibrary.InitializeLoaders;
var
  Loader: TDbgImageLoader;
begin
  Loader := TDbgImageLoader.Create(Name, nil, FLoadedTargetImageAddr);
  // The dynamic-loader (dl) on Linux also loads other stuff then ELF-
  // formatted libraries.
  // So it is reasonable likely that the loaded 'library' can not be handled
  // by the default readers from the loader.
  if Loader.IsValid then
    Loader.AddToLoaderList(LoaderList)
  else
    Loader.Free;
end;

constructor tDbgLinuxLibrary.Create(const AProcess: TDbgProcess; const AFileName: string; const AModuleHandle: THandle; const ALoadedTargetImageAddr: TDbgPtr);
begin
  FLoadedTargetImageAddr := ALoadedTargetImageAddr;
  Inherited Create(AProcess, AFileName, AModuleHandle);
  SetFileName(AFileName);

  LoadInfo;
end;

{ TFpDbgLinuxSignal }

class operator TFpDbgLinuxSignal.=(a, b: TFpDbgLinuxSignal): boolean;
begin
  result := a.Pid = b.Pid;
  assert(false);
end;

{ TFpDbgLinuxSignalQueue }

procedure TFpDbgLinuxSignalQueue.AddSignal(APID: THandle; AWaitStatus: cint);
var
  tmp: TFpDbgLinuxSignal;
begin
  tmp.PID := APid;
  tmp.WaitStatus := AWaitStatus;
  Add(tmp);
end;

function TFpDbgLinuxSignalQueue.GetNextSignal(out APID: THandle; out
  AWaitStatus: cint): Boolean;
var
  tmp: TFpDbgLinuxSignal;
begin
  Result := Count > 0;
  if not Result then
    exit;
  tmp := Items[0];
  APID := tmp.PID;
  AWaitStatus := tmp.WaitStatus;
  delete(0);
  DebugLn(DBG_VERBOSE, ['DEFERRED event for ',Apid]);
end;

{ TDbgLinuxThread }

{$ifndef VER2_6}
procedure TDbgLinuxProcess.OnForkEvent(Sender: TObject);
{$else}
procedure OnForkEvent;
{$endif VER2_6}
var
  ConsoleTtyFd: cint;
begin
  if fpPTrace(PTRACE_TRACEME, 0, nil, nil) <> 0 then
    writeln('Failed to start trace of process. Errcode: '+inttostr(fpgeterrno));

  ConsoleTtyFd := -1;
  if GConsoleTty<>'' then
    ConsoleTtyFd:=FpOpen(GConsoleTty,O_RDWR+O_NOCTTY)
  else if GSlavePTyFd>-1 then
    ConsoleTtyFd:=GSlavePTyFd;

  if ConsoleTtyFd>-1 then begin
    if FpSetsid <> -1 then
      FpIOCtl(ConsoleTtyFd, TIOCSCTTY, nil);

    if Config.StdInRedirFile = '' then begin
      //if DBG_PROCESS_HAS_REDIRECT then ????????????
      FpClose(0);
      FpDup2(ConsoleTtyFd,0);
    end;
    if Config.StdOutRedirFile = '' then begin
      FpClose(1);
      FpDup2(ConsoleTtyFd,1);
    end;
    if Config.StdErrRedirFile = '' then begin
      FpClose(2);
      FpDup2(ConsoleTtyFd,2);
    end;
    FpClose(ConsoleTtyFd);
  end
  else
    writeln('Failed to open tty '+GConsoleTty+'. Errno: '+inttostr(fpgeterrno));

end;

function TDbgLinuxThread.GetDebugRegOffset(ind: byte): pointer;
var
  user64ptr: ^user64;
  user32ptr: ^user32;
begin
  if Process.Mode=dm64 then
    begin
    user64ptr:=nil;
    result := @(user64ptr^.u_debugreg[ind])
    end
  else
    begin
    user32ptr:=nil;
    result := @(user32ptr^.u_debugreg[ind])
    end;
end;

function TDbgLinuxThread.ReadDebugReg(ind: byte; out AVal: PtrUInt): boolean;
var
  e: integer;
begin
  fpseterrno(0);
  AVal := PtrUInt(fpPTrace(PTRACE_PEEKUSR, ID, GetDebugRegOffset(ind), nil));
  e := fpgeterrno;
  if e <> 0 then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to read dr'+inttostr(ind)+'-debug register. Errcode: '+inttostr(e));
    result := false;
    end
  else
    result := true;
end;

function TDbgLinuxThread.WriteDebugReg(ind: byte; AVal: PtrUInt): boolean;
begin
  if fpPTrace(PTRACE_POKEUSR, ID, GetDebugRegOffset(ind), pointer(AVal)) = -1 then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to write dr'+inttostr(ind)+'-debug register. Errcode: '+inttostr(fpgeterrno));
    result := false;
    end
  else
    result := true;
end;

function TDbgLinuxThread.GetName: String;
var
  fh: THandle;
  n: array[0..30] of AnsiChar;
  c: LongInt;
begin
  Result := '';
  fh := FileOpen('/proc/' + IntToStr(Handle) + '/comm', fmOpenRead or fmShareDenyNone);
  if fh <> THandle(-1) then begin
    try
      c := FileRead(fh, n, 30);
      if c > 0 then begin
        n[c] := #0;
        Result := TrimRightSet(n, [' ', #10]);
      end;
    finally
      FileClose(fh);
    end;
  end;
  if Result = '' then
    Result := inherited GetName;
end;

function TDbgLinuxThread.ReadThreadState: boolean;
var
  io: iovec;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxThread.ReadThreadState');{$ENDIF}
  assert(FIsPaused, 'TDbgLinuxThread.ReadThreadState: FIsPaused');

  result := true;
  if FHasThreadState then
    exit;
  FFpRegsAvail:=False;
  io.iov_base:=@(FUserRegs.regs32[0]);
  io.iov_len:= sizeof(FUserRegs);
  if fpPTrace(PTRACE_GETREGSET, ID, pointer(PtrUInt(NT_PRSTATUS)), @io) <> 0 then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to read thread registers from threadid '+inttostr(ID)+'. Errcode: '+inttostr(fpgeterrno));
    result := false;
    end;
  FUserRegsChanged:=false;
  FRegisterValueListValid:=false;
  FHasThreadState := Result;
  FHasResetInstructionPointerAfterBreakpoint := False;

  io.iov_base:=@FFpRegs;
  io.iov_len:= sizeof(FFpRegs);
  if fpPTrace(PTRACE_GETREGSET, ID, pointer(PtrUInt(NT_PRFPREG)), @io) = 0 then
    FFpRegsAvail:=True
  else
    DebugLn(DBG_WARNINGS, 'Failed to read thread registers from threadid '+inttostr(ID)+'. Errcode: '+inttostr(fpgeterrno));
end;

function TDbgLinuxThread.RequestInternalPause: Boolean;
begin
  Result := False;
  if FHasExited then begin
    DebugLn(DBG_VERBOSE, ['PauseRequest for exited Thread ', ID]);
    exit;
  end;
  if FInternalPauseRequested or FIsPaused then
    exit;

  result := fpkill(ID, SIGSTOP)=0;
  {$IFDEF DebuglnLinuxDebugEvents}
  debugln(FPDBG_LINUX, 'TDbgLinuxThread.RequestInternalPause fpkill(%d, SIGSTOP) => %s', [ID, dbgs(Result)]);
  {$ENDIF}
  if not result then
    begin
    // TODO: errChld -> remove thread
    DebugLn(DBG_WARNINGS, 'Failed to send SIGTSTOP to process %d. Errno: %d',[ID, errno]);
    exit;
    end;

  FInternalPauseRequested := True;
end;

function TDbgLinuxThread.CheckSignalForPostponing(AWaitedStatus: cint): Boolean;
begin
  //Assert(not FIsPaused, 'Got WaitStatus while already paused');
  //assert(FExceptionSignal = 0, 'TDbgLinuxThread.CheckSignalForPostponing: FExceptionSignal = 0');
  if FHasExited then begin
    DebugLn(DBG_VERBOSE, ['Received double exit for Thread ', ID]);
    exit(False);
  end;
  Result := FIsPaused;
  DebugLn(DBG_VERBOSE and (Result), ['Warning: Thread already paused', ID]);
  if Result then
    exit;

  FIsPaused := True;
  FIsInInternalPause := False;

  if {FInternalPauseRequested and} (wstopsig(AWaitedStatus) = SIGSTOP) then begin
    DebugLn(DBG_VERBOSE and not FInternalPauseRequested, 'Received SigStop, but had not (yet) requested it. TId=', [Id]);
    FInternalPauseRequested := False;
    FIsInInternalPause := True;
    // no postpone
  end

  else
  if wstopsig(AWaitedStatus) = SIGTRAP then begin
    if ReadThreadState then
      CheckAndResetInstructionPointerAfterBreakpoint;
    Result := True;
    // TODO: main loop should search all threads for breakpoints
  end

  else
  if wifexited(AWaitedStatus) and (ID <> Process.ProcessID) then begin
    FHasExited := True;
  end

  else
  begin
    // Handle later
    Result := True;
  end;

  //TODO: Handle all signals/exceptions/...
end;

procedure TDbgLinuxThread.ResetPauseStates;
begin
  FIsInInternalPause := False;
  FIsPaused := False;
  ClearExceptionSignal;
  FHasThreadState := False;
  FDidResetInstructionPointer := False;
end;

function TDbgLinuxThread.GetStackUnwinder: TDbgStackUnwinder;
begin
  if FUnwinder = nil then
    FUnwinder := TDbgStackUnwinderX86MultiMethod.Create(Process);
  Result := FUnwinder;
end;

destructor TDbgLinuxThread.Destroy;
begin
  FUnwinder.Free;
  inherited Destroy;
end;

function TDbgLinuxThread.ResetInstructionPointerAfterBreakpoint: boolean;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxThread.ResetInstructionPointerAfterBreakpoint');{$ENDIF}
  assert(FIsPaused, 'TDbgLinuxThread.ResetInstructionPointerAfterBreakpoint: FIsPaused');

  if not ReadThreadState then
    exit(False);
  result := true;
  if FDidResetInstructionPointer then
    exit;
  FDidResetInstructionPointer := True;

  if Process.Mode=dm32 then
    Dec(FUserRegs.regs32[eip])
  else
    Dec(FUserRegs.regs64[rip]);
  FUserRegsChanged:=true;
  FHasResetInstructionPointerAfterBreakpoint := True;
end;

procedure TDbgLinuxThread.ApplyWatchPoints(AWatchPointData: TFpWatchPointData);
var
  i: integer;
  r: boolean;
  dr7: PtrUInt;
  addr: PtrUInt;
begin
  if not ReadDebugReg(7, dr7) then
    Exit;

  r := True;
  for i := 0 to 3 do begin
    addr := PtrUInt(TFpIntelWatchPointData(AWatchPointData).Dr03[i]);
    r := r and WriteDebugReg(i, addr);
  end;
  Dr7 := (Dr7 and $0000FF00);
  if r then
    Dr7 := Dr7 or PtrUInt(TFpIntelWatchPointData(AWatchPointData).Dr7);
  WriteDebugReg(7, dr7);
end;

function TDbgLinuxThread.DetectHardwareWatchpoint: Pointer;
var
  dr6: PtrUInt;
  wd: TFpIntelWatchPointData;
begin
  result := nil;
  if ReadDebugReg(6, dr6) then
  begin
    wd := TFpIntelWatchPointData(Process.WatchPointData);
    if dr6 and 1 = 1 then result := wd.Owner[0]
    else if dr6 and 2 = 2 then result := wd.Owner[1]
    else if dr6 and 4 = 4 then result := wd.Owner[2]
    else if dr6 and 8 = 8 then result := wd.Owner[3];
    if (Result = nil) and ((dr6 and 15) <> 0) then
      Result := Pointer(-1); // not owned watchpoint
  end;
end;

procedure TDbgLinuxThread.BeforeContinue;
var
  io: iovec;
begin
  if not FIsPaused then
    exit;

  inherited;
  if Process.CurrentWatchpoint <> nil then
    WriteDebugReg(6, 0);

  if FUserRegsChanged then
    begin
    io.iov_base:=@(FUserRegs.regs64[0]);
    io.iov_len:= sizeof(FUserRegs);

    if fpPTrace(PTRACE_SETREGSET, ID, pointer(PtrUInt(NT_PRSTATUS)), @io) <> 0 then
      begin
      DebugLn(DBG_WARNINGS, 'Failed to set thread registers. Errcode: '+inttostr(fpgeterrno));
      end;
    FUserRegsChanged:=false;
    end;
  FHasResetInstructionPointerAfterBreakpoint := False;
end;

procedure TDbgLinuxThread.LoadRegisterValues;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxThread.LoadRegisterValues');{$ENDIF}
  assert(FIsPaused, 'TDbgLinuxThread.LoadRegisterValues: FIsPaused');

  if not ReadThreadState then
    exit;
  if Process.Mode=dm32 then
  begin
    FRegisterValueList.DbgRegisterAutoCreate['eax'].SetValue(FUserRegs.regs32[eax], IntToStr(FUserRegs.regs32[eax]),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['ecx'].SetValue(FUserRegs.regs32[ecx], IntToStr(FUserRegs.regs32[ecx]),4,1);
    FRegisterValueList.DbgRegisterAutoCreate['edx'].SetValue(FUserRegs.regs32[edx], IntToStr(FUserRegs.regs32[edx]),4,2);
    FRegisterValueList.DbgRegisterAutoCreate['ebx'].SetValue(FUserRegs.regs32[ebx], IntToStr(FUserRegs.regs32[ebx]),4,3);
    FRegisterValueList.DbgRegisterAutoCreate['esp'].SetValue(FUserRegs.regs32[uesp], IntToStr(FUserRegs.regs32[uesp]),4,4);
    FRegisterValueList.DbgRegisterAutoCreate['ebp'].SetValue(FUserRegs.regs32[ebp], IntToStr(FUserRegs.regs32[ebp]),4,5);
    FRegisterValueList.DbgRegisterAutoCreate['esi'].SetValue(FUserRegs.regs32[esi], IntToStr(FUserRegs.regs32[esi]),4,6);
    FRegisterValueList.DbgRegisterAutoCreate['edi'].SetValue(FUserRegs.regs32[edi], IntToStr(FUserRegs.regs32[edi]),4,7);
    FRegisterValueList.DbgRegisterAutoCreate['eip'].SetValue(FUserRegs.regs32[eip], IntToStr(FUserRegs.regs32[EIP]),4,8);

    FRegisterValueList.DbgRegisterAutoCreate['eflags'].Setx86EFlagsValue(FUserRegs.regs32[eflags]);

    FRegisterValueList.DbgRegisterAutoCreate['cs'].SetValue(FUserRegs.regs32[xcs], IntToStr(FUserRegs.regs32[xcs]),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['ss'].SetValue(FUserRegs.regs32[xss], IntToStr(FUserRegs.regs32[xss]),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['ds'].SetValue(FUserRegs.regs32[xds], IntToStr(FUserRegs.regs32[xds]),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['es'].SetValue(FUserRegs.regs32[xes], IntToStr(FUserRegs.regs32[xes]),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['fs'].SetValue(FUserRegs.regs32[xfs], IntToStr(FUserRegs.regs32[xfs]),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['gs'].SetValue(FUserRegs.regs32[xgs], IntToStr(FUserRegs.regs32[xgs]),4,0);
  end else
    begin
    FRegisterValueList.DbgRegisterAutoCreate['rax'].SetValue(FUserRegs.regs64[rax], IntToStr(FUserRegs.regs64[rax]),8,0);
    FRegisterValueList.DbgRegisterAutoCreate['rbx'].SetValue(FUserRegs.regs64[rbx], IntToStr(FUserRegs.regs64[rbx]),8,3);
    FRegisterValueList.DbgRegisterAutoCreate['rcx'].SetValue(FUserRegs.regs64[rcx], IntToStr(FUserRegs.regs64[rcx]),8,2);
    FRegisterValueList.DbgRegisterAutoCreate['rdx'].SetValue(FUserRegs.regs64[rdx], IntToStr(FUserRegs.regs64[rdx]),8,1);
    FRegisterValueList.DbgRegisterAutoCreate['rsi'].SetValue(FUserRegs.regs64[rsi], IntToStr(FUserRegs.regs64[rsi]),8,4);
    FRegisterValueList.DbgRegisterAutoCreate['rdi'].SetValue(FUserRegs.regs64[rdi], IntToStr(FUserRegs.regs64[rdi]),8,5);
    FRegisterValueList.DbgRegisterAutoCreate['rbp'].SetValue(FUserRegs.regs64[rbp], IntToStr(FUserRegs.regs64[rbp]),8,6);
    FRegisterValueList.DbgRegisterAutoCreate['rsp'].SetValue(FUserRegs.regs64[rsp], IntToStr(FUserRegs.regs64[rsp]),8,7);

    FRegisterValueList.DbgRegisterAutoCreate['r8'].SetValue(FUserRegs.regs64[r8], IntToStr(FUserRegs.regs64[r8]),8,8);
    FRegisterValueList.DbgRegisterAutoCreate['r9'].SetValue(FUserRegs.regs64[r9], IntToStr(FUserRegs.regs64[r9]),8,9);
    FRegisterValueList.DbgRegisterAutoCreate['r10'].SetValue(FUserRegs.regs64[r10], IntToStr(FUserRegs.regs64[r10]),8,10);
    FRegisterValueList.DbgRegisterAutoCreate['r11'].SetValue(FUserRegs.regs64[r11], IntToStr(FUserRegs.regs64[r11]),8,11);
    FRegisterValueList.DbgRegisterAutoCreate['r12'].SetValue(FUserRegs.regs64[r12], IntToStr(FUserRegs.regs64[r12]),8,12);
    FRegisterValueList.DbgRegisterAutoCreate['r13'].SetValue(FUserRegs.regs64[r13], IntToStr(FUserRegs.regs64[r13]),8,13);
    FRegisterValueList.DbgRegisterAutoCreate['r14'].SetValue(FUserRegs.regs64[r14], IntToStr(FUserRegs.regs64[r14]),8,14);
    FRegisterValueList.DbgRegisterAutoCreate['r15'].SetValue(FUserRegs.regs64[r15], IntToStr(FUserRegs.regs64[r15]),8,15);

    FRegisterValueList.DbgRegisterAutoCreate['rip'].SetValue(FUserRegs.regs64[rip], IntToStr(FUserRegs.regs64[rip]),8,16);
    FRegisterValueList.DbgRegisterAutoCreate['eflags'].Setx86EFlagsValue(FUserRegs.regs64[eflags]);

    FRegisterValueList.DbgRegisterAutoCreate['cs'].SetValue(FUserRegs.regs64[cs], IntToStr(FUserRegs.regs64[cs]),8,43);
    FRegisterValueList.DbgRegisterAutoCreate['fs'].SetValue(FUserRegs.regs64[fs], IntToStr(FUserRegs.regs64[fs]),8,46);
    FRegisterValueList.DbgRegisterAutoCreate['gs'].SetValue(FUserRegs.regs64[gs], IntToStr(FUserRegs.regs64[gs]),8,47);

    if FFpRegsAvail then begin
      DisableFloatExceptions;
      FRegisterValueList.DbgRegisterAutoCreate['st0'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 0*4])^),10,500);
      FRegisterValueList.DbgRegisterAutoCreate['st1'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 1*4])^),10,501);
      FRegisterValueList.DbgRegisterAutoCreate['st2'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 2*4])^),10,502);
      FRegisterValueList.DbgRegisterAutoCreate['st3'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 3*4])^),10,503);
      FRegisterValueList.DbgRegisterAutoCreate['st4'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 4*4])^),10,504);
      FRegisterValueList.DbgRegisterAutoCreate['st5'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 5*4])^),10,505);
      FRegisterValueList.DbgRegisterAutoCreate['st6'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 6*4])^),10,506);
      FRegisterValueList.DbgRegisterAutoCreate['st7'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 7*4])^),10,507);

      FRegisterValueList.DbgRegisterAutoCreate['fctrl'].SetValue(FFpRegs.cwd, IntToStr(FFpRegs.cwd),2,510);
      FRegisterValueList.DbgRegisterAutoCreate['fstat'].SetValue(FFpRegs.swd, IntToStr(FFpRegs.swd),2,511);
      FRegisterValueList.DbgRegisterAutoCreate['ftwd'].SetValue(FFpRegs.twd, IntToStr(FFpRegs.twd),2,512);
      FRegisterValueList.DbgRegisterAutoCreate['fop'].SetValue(FFpRegs.fop, IntToStr(FFpRegs.fop),2,513);
      FRegisterValueList.DbgRegisterAutoCreate['fcs'].SetValue(FFpRegs.fcs, IntToStr(FFpRegs.fcs),4,514);
      FRegisterValueList.DbgRegisterAutoCreate['fip'].SetValue(FFpRegs.fip, IntToStr(FFpRegs.fip),4,515);
      FRegisterValueList.DbgRegisterAutoCreate['foo'].SetValue(FFpRegs.foo, IntToStr(FFpRegs.foo),4,516);
      FRegisterValueList.DbgRegisterAutoCreate['fos'].SetValue(FFpRegs.fos, IntToStr(FFpRegs.fos),4,517);

      FRegisterValueList.DbgRegisterAutoCreate['Xmm0' ].SetValue(@FFpRegs.xmm_space[0*4],16,600, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm1' ].SetValue(@FFpRegs.xmm_space[1*4],16,601, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm2' ].SetValue(@FFpRegs.xmm_space[2*4],16,602, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm3' ].SetValue(@FFpRegs.xmm_space[3*4],16,603, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm4' ].SetValue(@FFpRegs.xmm_space[4*4],16,604, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm5' ].SetValue(@FFpRegs.xmm_space[5*4],16,605, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm6' ].SetValue(@FFpRegs.xmm_space[6*4],16,606, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm7' ].SetValue(@FFpRegs.xmm_space[7*4],16,607, @XmmToFormat);

      FRegisterValueList.DbgRegisterAutoCreate['mxcsr'].SetValue(FFpRegs.fos, IntToStr(FFpRegs.mxcsr),4,620);

      EnableFloatExceptions;
    end;
  end;
  FRegisterValueListValid:=true;
end;

function TDbgLinuxThread.GetInstructionPointerRegisterValue: TDbgPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxThread.GetInstructionPointerRegisterValue');{$ENDIF}
  assert(FIsPaused, 'TDbgLinuxThread.GetInstructionPointerRegisterValue: FIsPaused');

  Result := 0;
  if not ReadThreadState then
    exit;
  if Process.Mode=dm32 then
    result := FUserRegs.regs32[eip]
  else
    result := FUserRegs.regs64[rip];
end;

function TDbgLinuxThread.GetStackBasePointerRegisterValue: TDbgPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxThread.GetStackBasePointerRegisterValue');{$ENDIF}
  assert(FIsPaused, 'TDbgLinuxThread.GetStackBasePointerRegisterValue: FIsPaused');

  Result := 0;
  if not ReadThreadState then
    exit;
  if Process.Mode=dm32 then
    result := FUserRegs.regs32[ebp]
  else
    result := FUserRegs.regs64[rbp];
end;

procedure TDbgLinuxThread.SetInstructionPointerRegisterValue(AValue: TDbgPtr);
begin
  if not FHasThreadState then
    exit;
  if Process.Mode=dm32 then
    FUserRegs.regs32[eip] := AValue
  else
    FUserRegs.regs64[rip] := AValue;
end;

procedure TDbgLinuxThread.SetStackPointerRegisterValue(AValue: TDbgPtr);
begin
  if not FHasThreadState then
    exit;
  if Process.Mode=dm32 then
    FUserRegs.regs32[UESP] := AValue
  else
    FUserRegs.regs64[rsp] := AValue;
end;

function TDbgLinuxThread.GetStackPointerRegisterValue: TDbgPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxThread.GetStackPointerRegisterValue');{$ENDIF}
  assert(FIsPaused, 'TDbgLinuxThread.GetStackPointerRegisterValue: FIsPaused');

  Result := 0;
  if not ReadThreadState then
    exit;
  if Process.Mode=dm32 then
    result := FUserRegs.regs32[UESP]
  else
    result := FUserRegs.regs64[rsp];
end;

procedure TDbgLinuxThread.SetRegisterValue(AName: string; AValue: QWord);
begin
  if Process.Mode=dm32 then
  begin
    assert((AValue and QWord($ffffffff00000000) = 0) or (AValue and QWord($ffffffff00000000) = QWord($ffffffff00000000)), 'TDbgLinuxThread.SetRegisterValue: (AValue and QWord($ffffffff00000000) = 0) or (AValue and QWord($ffffffff00000000) = QWord($ffffffff00000000))');
    case AName of
      'eip': FUserRegs.regs32[eip] := cuint32(AValue);
      'eax': FUserRegs.regs32[eax] := cuint32(AValue);
      'ecx': FUserRegs.regs32[ecx] := cuint32(AValue);
      'edx': FUserRegs.regs32[edx] := cuint32(AValue);
    else
      raise Exception.CreateFmt('Setting the [%s] register is not supported', [AName]);
    end;
    FUserRegsChanged:=true;
  end else
    begin
    case AName of
      'rax': FUserRegs.regs64[rax] := AValue;
      'rbx': FUserRegs.regs64[rbx] := AValue;
      'rcx': FUserRegs.regs64[rcx] := AValue;
      'rdx': FUserRegs.regs64[rdx] := AValue;
      'rsi': FUserRegs.regs64[rsi] := AValue;
      'rdi': FUserRegs.regs64[rdi] := AValue;
      'rbp': FUserRegs.regs64[rbp] := AValue;
      'rsp': FUserRegs.regs64[rsp] := AValue;

      'r8': FUserRegs.regs64[r8] := AValue;
      'r9': FUserRegs.regs64[r9] := AValue;
      'r10': FUserRegs.regs64[r10] := AValue;
      'r11': FUserRegs.regs64[r11] := AValue;
      'r12': FUserRegs.regs64[r12] := AValue;
      'r13': FUserRegs.regs64[r13] := AValue;
      'r14': FUserRegs.regs64[r14] := AValue;
      'r15': FUserRegs.regs64[r15] := AValue;

      'rip': FUserRegs.regs64[rip] := AValue;

      'cs': FUserRegs.regs64[cs] := AValue;
      'fs': FUserRegs.regs64[fs] := AValue;
      'gs': FUserRegs.regs64[gs] := AValue;
    else
      raise Exception.CreateFmt('Setting the [%s] register is not supported', [AName]);
    end;
    FUserRegsChanged:=true;
  end;
end;

procedure TDbgLinuxThread.RestoreRegisters;
begin
  FUserRegs:=FStoredUserRegs;
  FUserRegsChanged := true;
  FRegisterValueListValid := False;
end;

procedure TDbgLinuxThread.StoreRegisters;
begin
  Assert(FHasThreadState);
  FStoredUserRegs := FUserRegs;
end;

procedure TDbgLinuxThread.ClearExceptionSignal;
begin
  FExceptionSignal := 0;
end;

{ TDbgLinuxProcess }

function TDbgLinuxProcess.GetRequiresExecutionInDebuggerThread: boolean;
begin
  Result := True;
end;

procedure TDbgLinuxProcess.InitializeLoaders;
var
  Loader: TDbgImageLoader;
begin
  Loader := TDbgImageLoader.Create(Name);
  if Loader.IsValid then
    Loader.AddToLoaderList(LoaderList)
  else
    Loader.Free;
end;

function TDbgLinuxProcess.CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread;
begin
  IsMainThread:=False;
  if AthreadIdentifier>-1 then
    begin
    IsMainThread := AthreadIdentifier=ProcessID;
    result := TDbgLinuxThread.Create(Self, AthreadIdentifier, AthreadIdentifier)
    end
  else
    result := nil;
end;

function TDbgLinuxProcess.CreateWatchPointData: TFpWatchPointData;
begin
  Result := TFpIntelWatchPointData.Create;
end;

function TDbgLinuxProcess.CheckForSOLibDebugEvent(AThread: TDbgThread): TFPDEvent;
var
  CurrentAddr: TDBGPtr;
  BList: TFpInternalBreakpointArray;
  SOLibBreakpointFound: Boolean;
  RegularBreakpointFound: Boolean;
  i: Integer;
  ProcMaps: TDbgLinuxMemoryMappingList;
  ProcMap: TDbgLinuxMemoryMapping;
begin
  // When we are not dealing with an SOLibDebugEvent, we have a 'normal'
  // Breakpoint.
  Result := deBreakpoint;

  // When the SOLib-breakpoint has not been set, this is not a SOLibDebugEvent.
  if not Assigned(FSOLibEventBreakpoint) then
    Exit;

  // Loop through all defined breakpoints at the
  // instruction-pointer location, and see if the SOLib or any other breakpoint
  // has been set on the current location,
  RegularBreakpointFound:=False;
  SOLibBreakpointFound:=False;
  CurrentAddr:=AThread.GetInstructionPointerRegisterValue;
  BList := FBreakMap.GetInternalBreaksAtLocation(CurrentAddr);
  if BList <> nil then
    begin
    for i := 0 to Length(BList) -1 do
      begin
      if BList[i] = FSOLibEventBreakpoint then
        SOLibBreakpointFound := True
      else
        RegularBreakpointFound := True;
      end;
    end;

  if RegularBreakpointFound then
    begin
    // Regular breakpoints have precedense. In case there is a library-change,
    // handle that one later.
    //if SOLibBreakpointFound then
    //  FAwaitingLibSOEventsPresent:=True;
    end
  else if SOLibBreakpointFound then
    Result := SynchronizeProcMapsWithLibraryList();
end;

function TDbgLinuxProcess.SynchronizeProcMapsWithLibraryList: TFPDEvent;
var
  ProcMaps: TDbgLinuxMemoryMappingList;
  i: Integer;
  ProcMap: TDbgLinuxMemoryMapping;
  AnId: TDbgPtr;
begin
  Result := deInternalContinue;
  FLibMap.ClearAddedAndRemovedLibraries;
  ProcMaps := ObtainProcMaps;
  try
    // The first entry is the application itself, which we skip, so start with
    // 1.
    for i := 1 to ProcMaps.Count -1 do
      begin
      ProcMap := ProcMaps.Items[i] as TDbgLinuxMemoryMapping;
      // Check if the ProcMap is the first entry of a valid library
      // If the Offset <> 0, it is probably not the 'main' entry we are looking
      // for. When there is no filename, we can not handle it, and when the
      // filename starts with '[', it is a general placeholder.
      if (ProcMap.Offset = 0) and (ProcMap.FileName <> '') and (Copy(ProcMap.FileName,1,1) <> '[') then
        begin
        // Check if this library is already known.
        AnId := TDBGPtr(ProcMap.AddressFrom);
        if not (FLibMap.HasId(AnId)) then
          begin
          // Add the library and trigger a deLoadLibrary event
          AddLib(tDbgLinuxLibrary.Create(Self, ProcMap.FileName, THandle(ProcMap.Inode), TDBGPtr(ProcMap.AddressFrom)));
          Result := deLoadLibrary;
          end
        end;
      end;
  finally
    ProcMaps.Free;
  end;
end;

function TDbgLinuxProcess.ObtainProcMaps: TDbgLinuxMemoryMappingList;

  procedure ParseMapsLine(const Line: string);
  var
    Mapping: TDbgLinuxMemoryMapping;
    Parts: TStringDynArray;
    Addresses: TStringDynArray;
  begin
    Mapping := TDbgLinuxMemoryMapping.Create;
    try
      Parts := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);
      Addresses := Parts[0].Split(['-']);
      Mapping.AddressFrom:=PtrInt(Hex2Dec64(Addresses[0]));
      Mapping.AddressTill:=PtrInt(Hex2Dec64(Addresses[1]));

      Mapping.Rights:=Parts[1];
      Mapping.Offset:=Hex2Dec64(Parts[2]);
      Mapping.DeviceId:=Parts[3];
      Mapping.Inode:=StrToInt64(Parts[4]);
      if Length(Parts) > 5 then
        Mapping.FileName:=Parts[5]
      else
        Mapping.FileName:='';

      Result.Add(Mapping);
      Mapping := nil;
    finally
      Mapping.Free;
    end;
  end;

var
  FN: string;
  Buf: string;
  FS: TFileStream;
  BytesRead: Int64;
  TotalBytesRead: Int64;
  BlockLength: Int64;
  MapsStrings: TStringList;
  i: Integer;
begin
  Result := TDbgLinuxMemoryMappingList.Create(True);
  // All memory-mappings are retrieved from /proc/<ps>/maps and stored in
  // FMemoryMappingList
  FN := '/proc/'+IntToStr(ProcessID)+'/maps';

  // First read the contents of /proc/<ps>/maps and place it in a buffer

  // In principle the file should be read in one read-operation, or the file
  // might be changed while reading it. So we use a relatively large buffer.
  // (In principle it is still possible that the file isn't
  // being read in one operation. But let's ignore this for now.)
  FS := TFileStream.Create(FN, fmOpenRead);
  try
    BlockLength:=64*1024;
    TotalBytesRead := 0;
    repeat
      SetLength(Buf, TotalBytesRead + BlockLength);
      BytesRead := FS.Read(Buf[TotalBytesRead+1], BlockLength);
      TotalBytesRead:=TotalBytesRead+BytesRead;
    until BytesRead <= 0;
    SetLength(Buf, TotalBytesRead);
  finally
    FS.Free;
  end;

  // Now use a TStringList to parse the contents of the buffer.
  MapsStrings := TStringList.Create;
  try
    MapsStrings.Text := Buf;
    // Parse the file line-by-line and add the mappings to FMemoryMappingList
    for i := 0 to MapsStrings.Count -1 do
      ParseMapsLine(MapsStrings[i]);
  finally
    MapsStrings.Free;
  end;
end;

procedure TDbgLinuxProcess.AddLib(const ALibrary: tDbgLinuxLibrary);
begin
  AddLibrary(ALibrary, ALibrary.FLoadedTargetImageAddr);
end;

constructor TDbgLinuxProcess.Create(const AFileName: string;
  AnOsClasses: TOSDbgClasses; AMemManager: TFpDbgMemManager;
  AMemModel: TFpDbgMemModel; AProcessConfig: TDbgProcessConfig);
begin
  FMasterPtyFd:=-1;
  FSingleSteppingThreadID := -1;
  FPostponedSignals := TFpDbgLinuxSignalQueue.Create;
  inherited Create(AFileName, AnOsClasses, AMemManager, AMemModel, AProcessConfig);
end;

destructor TDbgLinuxProcess.Destroy;
begin
  FProcProcess.Free;
  FPostponedSignals.Free;

  if FMasterPtyFd>-1 then
    FpClose(FMasterPtyFd);
  FMasterPtyFd:=-1;

  inherited Destroy;
end;

function TDbgLinuxProcess.StartInstance(AParams, AnEnvironment: TStrings;
  AWorkingDirectory, AConsoleTty: string; AFlags: TStartInstanceFlags; out
  AnError: TFpError): boolean;
var
  AProcess: TProcessWithRedirect;
  AMasterPtyFd: cint;
  AnExecutabeFilename: string;
begin
  Result := false;
  if FMasterPtyFd>-1 then
    FpClose(FMasterPtyFd);
  FMasterPtyFd:=-1;

  AnExecutabeFilename:=ExcludeTrailingPathDelimiter(Name);
  if DirectoryExists(AnExecutabeFilename) then
  begin
    DebugLn(DBG_WARNINGS, 'Can not debug %s, because it''s a directory',[AnExecutabeFilename]);
    Exit;
  end;

  if not FileExists(Name) then
  begin
    DebugLn(DBG_WARNINGS, 'Can not find  %s.',[AnExecutabeFilename]);
    Exit;
  end;

  AMasterPtyFd:=-1;
  if siRediretOutput in AFlags then
    begin
    if AConsoleTty<>'' then
      DebugLn(DBG_VERBOSE, 'It is of no use to provide a console-tty when the console output is being redirected.');
    GConsoleTty:='';
    if openpty(@AMasterPtyFd, @GSlavePTyFd, nil, nil, nil) <> 0 then
      DebugLn(DBG_WARNINGS, 'Failed to open pseudo-tty. Errcode: '+inttostr(fpgeterrno));
    end
  else
    begin
    GSlavePTyFd:=-1;
    GConsoleTty:=AConsoleTty;
    end;

  AProcess := TProcessWithRedirect.Create(nil);
  try
    AProcess.OnForkEvent:=@OnForkEvent;
    AProcess.Executable:=AnExecutabeFilename;
    AProcess.Parameters:=AParams;
    AProcess.Environment:=AnEnvironment;
    AProcess.CurrentDirectory:=AWorkingDirectory;
    if DBG_PROCESS_HAS_REDIRECT then begin
      AProcess.SetRedirection(dtStdIn,  Config.StdInRedirFile,  Config.FileOverwriteStdIn);
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
    end;

    AProcess.Execute;
    Init(AProcess.ProcessID, 0);
    FMasterPtyFd := AMasterPtyFd;
    FProcProcess := AProcess;
    sleep(100);
    Result := ProcessID > 0;
  except
    on E: Exception do
    begin
      DebugLn(DBG_WARNINGS, Format('Failed to start process "%s". Errormessage: "%s".',[Name, E.Message]));
      AProcess.Free;

    if GSlavePTyFd>-1 then
      FpClose(GSlavePTyFd);
    if AMasterPtyFd>-1 then
      FpClose(AMasterPtyFd);
    FMasterPtyFd:=-1;
    end;
  end;
end;

function TDbgLinuxProcess.AttachToInstance(APid: Integer; out AnError: TFpError
  ): boolean;
begin
  Result := fpPTrace(PTRACE_ATTACH, APid, nil, Pointer(PTRACE_O_TRACECLONE)) = 0;
  Init(APid, 0);
  // TODO: change the filename to the actual exe-filename. Load the correct dwarf info
end;

class function TDbgLinuxProcess.isSupported(ATargetInfo: TTargetDescriptor
  ): boolean;
begin
  result := (ATargetInfo.OS = osLinux) and
            (ATargetInfo.machineType in [mt386, mtX86_64]);
end;

function TDbgLinuxProcess.ReadWordSize(Adr: TDbgPtr; out AVal: TDBGPtr
  ): boolean;
var
  e: integer;
begin
  AVal := TDbgPtr(fpPTrace(PTRACE_PEEKDATA, FCurrentThreadId, pointer(Adr), nil));
  e := fpgeterrno;
  Result := e = 0;
  if not Result then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to read data at address '+FormatAddress(Adr)+' from processid '+inttostr(FCurrentThreadId)+'. Errcode: '+inttostr(e));
    result := false;
    end;
end;

function TDbgLinuxProcess.WriteWordSize(Adr: TDbgPtr; AVal: TDBGPtr): boolean;
var
  e: LongInt;
begin
  fpPTrace(PTRACE_POKEDATA, FCurrentThreadId, pointer(Adr), pointer(AVal));
  e := fpgeterrno;
  Result := e = 0;
  if not Result then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to write data at address '+FormatAddress(Adr)+' from processid '+inttostr(FCurrentThreadId)+'. Errcode: '+inttostr(e));
    result := false;
    end;
end;

function TDbgLinuxProcess.ReadData(const AAdress: TDbgPtr;
  const ASize: Cardinal; out AData): Boolean;
var
  APartSize: Cardinal;
begin
  Result := ReadData(AAdress, ASize, AData, APartSize);
  Result := Result and (APartSize = ASize);
end;

function TDbgLinuxProcess.ReadData(const AAdress: TDbgPtr;
  const ASize: Cardinal; out AData; out APartSize: Cardinal): Boolean;
var
  WordSize, WordAlignOffset, BytesDone: integer;
  TmpBuffer: TDbgPtr;
  DataWritePtr: pbyte;
  RemainingDataSize: int64;
  TargetReadAddress: TDBGPtr;

  localiov, remoteiov: iovec;
  readcnt: ssize_t;
begin
  result := false;
  if ASize = 0 then
    exit;
  APartSize := 0;
  BytesDone := 0;
  try
    // since kernel >= 3.2
    // https://kernelnewbies.org/Linux_3.2#Cross_memory_attach
    if Assigned(process_vm_readv) then
    begin
      localiov.iov_base := @AData;
      localiov.iov_len := ASize;
      remoteiov.iov_base := Pointer(AAdress);
      remoteiov.iov_len := ASize;
      readcnt := process_vm_readv(ProcessID, @localiov, 1, @remoteiov, 1, 0);
      if readcnt > 0 then
        APartSize := Cardinal(readcnt);
      if ASize = APartSize then
        Exit;
    end;

    fpseterrno(0);
    TargetReadAddress := AAdress + APartSize;
    DataWritePtr      := @AData  + APartSize;
    RemainingDataSize := ASize   - APartSize;
    WordSize:=DBGPTRSIZE[Mode];
    WordAlignOffset := TargetReadAddress and TDBGPtr(WordSize - 1);

    {$ifNdef LINUX_NO_PTRACE_ALIGN}  // according to man, only peek/poke_user need align
    if WordAlignOffset <> 0 then begin
      if not ReadWordSize(TargetReadAddress-WordAlignOffset, TmpBuffer) then
        Exit;  // APartSize is still correct

      BytesDone := WordSize - WordAlignOffset;
      if BytesDone > RemainingDataSize then
        BytesDone := RemainingDataSize;
      move(PByte(@TmpBuffer)[WordAlignOffset], DataWritePtr^, BytesDone);
      inc(DataWritePtr, BytesDone);
      inc(TargetReadAddress, BytesDone);
    end;
    {$endif}

    dec(RemainingDataSize, WordSize - 1); // full words only

    while BytesDone < RemainingDataSize do begin
      if not ReadWordSize(TargetReadAddress, TmpBuffer) then
        Exit;
      move(TmpBuffer, DataWritePtr^, WordSize);
      inc(DataWritePtr, WordSize);
      inc(BytesDone, WordSize);
      inc(TargetReadAddress, WordSize);
    end;

    RemainingDataSize := ASize - APartSize - BytesDone;
    assert((RemainingDataSize>=0) and (RemainingDataSize<WordSize));

    if RemainingDataSize > 0 then begin
      if not ReadWordSize(TargetReadAddress, TmpBuffer) then
        Exit;
      move(TmpBuffer, DataWritePtr^, RemainingDataSize);
      inc(BytesDone, RemainingDataSize);
    end;

  finally
    APartSize := APartSize + BytesDone;
    Result := APartSize > 0;
    if Result then
      MaskBreakpointsInReadData(AAdress, APartSize, AData);
  end;
end;

function TDbgLinuxProcess.WriteData(const AAdress: TDbgPtr;
  const ASize: Cardinal; const AData): Boolean;
var
  WordSize, BytesDone: integer;
  BufSize: int64;
  AVal: TDBGPtr;
  buf: PByte;
  AAdressAlign: TDBGPtr;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxProcess.WriteData');{$ENDIF}
  result := false;
  fpseterrno(0);
  BytesDone := 0;
  buf := @AData;
  BufSize := ASize;
  WordSize:=DBGPTRSIZE[Mode];

  {$ifNdef LINUX_NO_PTRACE_ALIGN}  // according to man, only peek/poke_user need align
  AAdressAlign := AAdress and (not TDBGPtr(WordSize - 1));
  if AAdressAlign <> AAdress then begin
    if not ReadWordSize(AAdressAlign, AVal) then
      Exit;
    BytesDone := WordSize - (AAdress-AAdressAlign);
    if BytesDone > ASize then
      BytesDone := ASize;
    move(buf[0], PByte(@AVal)[AAdress-AAdressAlign], BytesDone);
    if not WriteWordSize(AAdressAlign, AVal) then
      Exit;
    inc(AAdressAlign, WordSize);
  end;
  {$else}
  AAdressAlign := AAdress;
  {$endif}

  dec(BufSize, WordSize - 1); // full words only

  while BytesDone < BufSize do begin
    move(buf[BytesDone], AVal, WordSize);
    if not WriteWordSize(AAdressAlign, AVal) then
      Exit;
    inc(BytesDone, WordSize);
    inc(AAdressAlign, WordSize);
  end;

  BufSize := ASize - BytesDone;
  assert((BufSize>=0) and (BufSize<WordSize));

  if BufSize > 0 then begin
    if not ReadWordSize(AAdressAlign, AVal) then
      Exit;
    move(buf[BytesDone], AVal, BufSize);
    if not WriteWordSize(AAdressAlign, AVal) then
      Exit;
  end;

  result := true;
end;

function TDbgLinuxProcess.CallParamDefaultLocation(AParamIdx: Integer
  ): TFpDbgMemLocation;
begin
  case Mode of
    dm32: case AParamIdx of
       -1: Result := RegisterLoc(0); // EAX  // result
        0: Result := RegisterLoc(0); // EAX
        1: Result := RegisterLoc(2); // EDX
        2: Result := RegisterLoc(1); // ECX
      else
        Result := UnInitializedLoc;
      end;
    dm64: case AParamIdx of
       -1: Result := RegisterLoc(0); // RAX  // result
        0: Result := RegisterLoc(5); // RDI
        1: Result := RegisterLoc(4); // RSI
        2: Result := RegisterLoc(1); // RDX
        3: Result := RegisterLoc(2); // RCX
        4: Result := RegisterLoc(8); // R8
        5: Result := RegisterLoc(9); // R9
      else
        Result := UnInitializedLoc;
      end;
  end;
end;

function TDbgLinuxProcess.CheckForConsoleOutput(ATimeOutMs: integer): integer;
Var
  f: TfdSet;
  sleepytime: ttimeval;
begin
  sleepytime.tv_sec := ATimeOutMs div 1000;
  sleepytime.tv_usec := (ATimeOutMs mod 1000)*1000;
  FpFD_ZERO(f);
  fpFD_SET(FMasterPtyFd,f);
  result := fpselect(FMasterPtyFd+1,@f,nil,nil,@sleepytime);
end;

function TDbgLinuxProcess.GetConsoleOutput: string;
var
  ABytesAvailable: DWord;
  ABytesRead: cint;
  s: string;
begin
  if fpioctl(FMasterPtyFd, FIONREAD, @ABytesAvailable)<0 then
    ABytesAvailable := 0;

  result := '';
  while ABytesAvailable>0 do
  begin
    setlength(s, ABytesAvailable);
    ABytesRead := fpRead(FMasterPtyFd, s[1], ABytesAvailable);
    SetLength(s, ABytesRead);
    Result := Result + s;

    if fpioctl(FMasterPtyFd, FIONREAD, @ABytesAvailable)<0 then
      ABytesAvailable := 0;
  end;
end;

procedure TDbgLinuxProcess.SendConsoleInput(AString: string);
begin
  if FpWrite(FMasterPtyFd, AString[1], length(AString)) <> Length(AString) then
    DebugLn(DBG_WARNINGS, 'Failed to send input to console.');
end;

procedure TDbgLinuxProcess.TerminateProcess;
begin
  FIsTerminating:=true;
  if fpkill(ProcessID,SIGKILL)<>0 then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to send SIGKILL to process %d. Errno: %d',[ProcessID, errno]);
    FIsTerminating:=false;
    end;
end;

function TDbgLinuxProcess.Pause: boolean;
begin
  result := fpkill(ProcessID, SIGTRAP)=0;
  PauseRequested:=true;
  if not result then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to send SIGTRAP to process %d. Errno: %d',[ProcessID, errno]);
    end;
end;

function TDbgLinuxProcess.Detach(AProcess: TDbgProcess; AThread: TDbgThread): boolean;
begin
  RemoveAllBreakPoints;
  fpPTrace(PTRACE_DETACH, AThread.ID, nil, pointer(TDbgLinuxThread(AThread).FExceptionSignal));
  Result := True;
end;

procedure TDbgLinuxProcess.LoadInfo;
var
  i: Integer;
  InterpSection: PDbgImageSection;
  Astat: Stat;
  ALib: TDbgLibrary;

begin
  inherited LoadInfo;

  // This would be strange, but you never know.
  if Assigned(FSOLibEventBreakpoint) then
    Raise Exception.Create('SOLib event-breakpoint already exists.');

  // Check if the library supports dynamic loading by searching for the
  // .interp section.
  for i := 0 to LoaderList.Count -1 do
    begin
    InterpSection := LoaderList.Items[i].Section['.interp'];
    if assigned(InterpSection) then
      begin
      // Try to retrieve the inode of the file (library) in the .interp section.
      // This is the filename of the library that handles the dynamic loading.
      if FpStat(PChar(InterpSection^.RawData), AStat) = 0 then
        begin
        // Syncronize all loaded libraries, and obtain the library for the
        // dynamic-loading, based on the .interp section.
        SynchronizeProcMapsWithLibraryList();
        if FLibMap.GetLib(Astat.st_ino, ALib) then
          begin
          // Set a breakpoint at _dl_debug_state. This procedure is called after
          // one or more libraries have been loaded. This breakpoint is used to
          // detect the (un)loading of libraries.
          FSOLibEventBreakpoint := AddBreak('_dl_debug_state', False, ALib);
          TFpDbgBreakpoint(FSOLibEventBreakpoint).FreeByDbgProcess := True;
          end
        end;
      end;
    end;
end;

function TDbgLinuxProcess.CanContinueForWatchEval(ACurrentThread: TDbgThread
  ): boolean;
begin
  Result := inherited CanContinueForWatchEval(ACurrentThread);
  Result := Result and (TDbgLinuxThread(ACurrentThread).FExceptionSignal = 0)
end;

function TDbgLinuxProcess.Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean;
  function CheckNoError: Boolean;
  var
    e: integer;
  begin
    e := fpgeterrno;
    Result := e = 0;
    if not Result then
      DebugLn(DBG_WARNINGS, 'Failed to continue process. Errcode: '+inttostr(e));
  end;

var
  ThreadToContinue: TDbgLinuxThread;
  WaitStatus: cint;
  PID: THandle;
  IP: TDBGPtr;
begin
  if (FSOLibEventBreakpoint <> nil) and not FSOLibEventBreakpoint.Enabled then
    FSOLibEventBreakpoint.Enabled:=True;
  {$IFDEF DebuglnLinuxDebugEvents}
  debuglnEnter(['>>>>> TDbgLinuxProcess.Continue TID:', dbgsThreadId(AThread), ' SingleStep:', SingleStep ]); try
  {$ENDIF}
  FSingleSteppingThreadID := -1;

  // Terminating process and all threads
  if FIsTerminating and (AThread <> nil) then begin
    fpseterrno(0);
    AThread.BeforeContinue;
    fpPTrace(PTRACE_KILL, AThread.ID, pointer(1), nil);
    TDbgLinuxThread(AThread).ResetPauseStates;
    Result := CheckNoError;
    exit;
  end;

  if (AThread <> nil) and TDbgLinuxThread(AThread).FIsPaused then  // in case of deInternal, it may not be paused and can be ignored
    AThread.NextIsSingleStep:=SingleStep;

  // check for pending events in other threads
  if FPostponedSignals.Count > 0 then begin
    {$IFDEF DebuglnLinuxDebugEvents}
    debugln(FPDBG_LINUX, ['Exit for DEFERRED event TID']);
    {$ENDIF}
    exit;
  end;

  // check other threads if they need a singlestep
  for TDbgThread(ThreadToContinue) in FThreadMap do
    if (ThreadToContinue <> AThread) and ThreadToContinue.FIsPaused and
       (ThreadToContinue.SuspendCount <= 0)
    then begin
      IP := ThreadToContinue.GetInstructionPointerRegisterValue;
      if HasInsertedBreakInstructionAtLocation(IP) or ThreadToContinue.NextIsSingleStep then begin
        TempRemoveBreakInstructionCode(IP);
        ThreadToContinue.BeforeContinue;

        while (ThreadToContinue.GetInstructionPointerRegisterValue = IP) do begin
          {$IFDEF DebuglnLinuxDebugEvents}
          Debugln(FPDBG_LINUX, ['Single-stepping other TID: ', ThreadToContinue.ID]);
          {$ENDIF}
          fpseterrno(0);
          fpPTrace(PTRACE_SINGLESTEP, ThreadToContinue.ID, pointer(1), pointer(TDbgLinuxThread(ThreadToContinue).FExceptionSignal));

          TDbgLinuxThread(ThreadToContinue).ResetPauseStates;
          ThreadToContinue.FIsPaused := True;
          if CheckNoError then begin
            PID := fpWaitPid(ThreadToContinue.ID, WaitStatus, __WALL);
            if PID <> ThreadToContinue.ID then begin
              DebugLn(DBG_WARNINGS, ['XXXXX Error single stepping other thread ', ThreadToContinue.ID, ' waitpid got ', PID, ', ',WaitStatus, ' err ', Errno]);
              break;
            end;
            if ThreadToContinue.NextIsSingleStep then begin
              FPostponedSignals.AddSignal(PID, WaitStatus);
              break;
            end;
            if (wstopsig(WaitStatus) = SIGTRAP) then
              break; // if the command jumps back an itself....
          end
          else begin
            DebugLn(DBG_WARNINGS, ['Error single stepping other thread ', ThreadToContinue.ID]);
            break;
          end;
        end;

      end;
    end;

  if FPostponedSignals.Count > 0 then begin
    {$IFDEF DebuglnLinuxDebugEvents}
    debugln(FPDBG_LINUX, ['Exit for DEFERRED SingleSteps event TID']);
    {$ENDIF}
    exit;
  end;

  if (AThread <> nil) and TDbgLinuxThread(AThread).FIsPaused then  // in case of deInternal, it may not be paused and can be ignored
  if HasInsertedBreakInstructionAtLocation(AThread.GetInstructionPointerRegisterValue) then begin
    TempRemoveBreakInstructionCode(AThread.GetInstructionPointerRegisterValue);
    TDbgLinuxThread(AThread).FIsSteppingBreakPoint := True;
    fpseterrno(0);
    AThread.BeforeContinue;
    {$IFDEF DebuglnLinuxDebugEvents}
    Debugln(FPDBG_LINUX, ['Single-stepping current']);
    {$ENDIF}
    FSingleSteppingThreadID := AThread.ID;
    fpPTrace(PTRACE_SINGLESTEP, AThread.ID, pointer(1), pointer(TDbgLinuxThread(AThread).FExceptionSignal));
    TDbgLinuxThread(AThread).ResetPauseStates;
    Result := CheckNoError;
    exit;
  end;

  RestoreTempBreakInstructionCodes;

  ThreadsBeforeContinue;

  // start all other threads
  for TDbgThread(ThreadToContinue) in FThreadMap do begin
    if (ThreadToContinue <> AThread) and (ThreadToContinue.FIsPaused) and
       (ThreadToContinue.SuspendCount <= 0)
    then begin
      fpseterrno(0);
      {$IFDEF DebuglnLinuxDebugEvents}
      Debugln(FPDBG_LINUX, ['RUN other TID: ', ThreadToContinue.ID]);
      {$ENDIF}
      fpPTrace(PTRACE_CONT, ThreadToContinue.ID, pointer(1), pointer(ThreadToContinue.FExceptionSignal));
      CheckNoError; // only log
      ThreadToContinue.ResetPauseStates;
    end;
  end;

  if (AThread <> nil) and TDbgLinuxThread(AThread).FIsPaused then  // in case of deInternal, it may not be paused and can be ignored
  if not FIsTerminating then begin
    fpseterrno(0);
    //AThread.BeforeContinue;
    {$IFDEF DebuglnLinuxDebugEvents}
    Debugln(FPDBG_LINUX, ['RUN ']);
    {$ENDIF}
    if AThread.NextIsSingleStep then
      fpPTrace(PTRACE_SINGLESTEP, AThread.ID, pointer(1), pointer(TDbgLinuxThread(AThread).FExceptionSignal))
    else
      fpPTrace(PTRACE_CONT, AThread.ID, pointer(1), pointer((TDbgLinuxThread(AThread).FExceptionSignal)));
    TDbgLinuxThread(AThread).ResetPauseStates;
    Result := CheckNoError;
  end;

  {$IFDEF DebuglnLinuxDebugEvents}
  finally debuglnExit(['<<<<< TDbgLinuxProcess.Continue ' ]); end;
  {$ENDIF}
end;

function TDbgLinuxProcess.WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean;
var
  PID: THandle;
begin
  ThreadIdentifier:=-1;
  ProcessIdentifier:=-1;

  if FSingleSteppingThreadID <> -1 then begin
    PID:=FpWaitPid(FSingleSteppingThreadID, FStatus, __WALL);
    if PID <> FSingleSteppingThreadID then
      DebugLn(DBG_WARNINGS, ['XXXXX Error: single stepping current thread ', FSingleSteppingThreadID, ' waitpid got ', PID, ', ',FStatus, ' err ', Errno]);
  end
  else
  If not FPostponedSignals.GetNextSignal(PID, FStatus) then
    PID:=FpWaitPid(-1, FStatus, __WALL);

  RestoreTempBreakInstructionCodes; // should only happen after single step, so all threads should be paused

  result := PID<>-1;
  if not result then
    DebugLn(DBG_WARNINGS, 'Failed to wait for debug event. Errcode: %d', [fpgeterrno])
  else
    begin
    ThreadIdentifier := PID;
    FCurrentThreadId := PID;

    if not FProcessStarted and (PID <> ProcessID) then
      DebugLn(DBG_WARNINGS, 'ThreadID of main thread does not match the ProcessID');

    ProcessIdentifier := ProcessID;
    {$IFDEF DebuglnLinuxDebugEvents}
    debugln(FPDBG_LINUX, ['##### GOT EVENT FOR ',pid, ' st ', FStatus]);
    {$ENDIF}
    end;
end;

function TDbgLinuxProcess.AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent;

  function ExistsPendingSignal(out PID: THandle; out WaitStatus: cint;
    out AThread: TDbgLinuxThread; ANoHang: Boolean): Boolean;
  var
    Opts: cint;
  begin
    AThread := nil;
    Opts := __WALL;
    if ANoHang then
      Opts := Opts or WNOHANG;

    PID:=FpWaitPid(-1, WaitStatus, Opts);
    Result := (PID <> 0) and (PID <> -1);
    if not Result then
      exit;

    if not FThreadMap.GetData(PID, AThread) then
      AThread := nil;
    DebugLn(DBG_VERBOSE, ['Got SIGNAL for thread: ', pid, ' Status: ',WaitStatus, ' Found thread:', AThread <> nil]);
  end;

//var
//  NewThreadID: culong;
var
  ThreadToPause, ThreadSignaled: TDbgLinuxThread;
  Pid: THandle;
  WaitStatus: cint;
  it: TThreadMapUnLockedEnumerator;
begin
  if AThread = nil then begin // should not happen... / just assume the most likely safe failbacks
    if FIsTerminating then
      result := deExitProcess
    else
      result := deInternalContinue;
  end;

  TDbgLinuxThread(AThread).FExceptionSignal:=0;
  TDbgLinuxThread(AThread).FIsPaused := True;
  if wifexited(FStatus) or wifsignaled(FStatus) then
    begin
    if AThread.ID=ProcessID then
      begin
      // Main thread stop -> application exited
      SetExitCode(wexitStatus(FStatus));
      result := deExitProcess
      end
    else
      begin
      // Thread stopped, just continue
      RemoveThread(AThread.Id);
      result := deInternalContinue;
      end;
    end
  else if WIFSTOPPED(FStatus) then
    begin
    //DebugLn(DBG_WARNINGS, 'Stopped ',FStatus, ' signal: ',wstopsig(FStatus));
    TDbgLinuxThread(AThread).ReadThreadState;

    if (FStatus >> 8) = (SIGTRAP or (PTRACE_EVENT_CLONE << 8)) then
      begin
      // New thread started (stopped in 'parent' thread)
      Result := deInternalContinue;

      // Usefull in case of debugging:
      //if fpPTrace(PTRACE_GETEVENTMSG, AThread.ID, nil, @NewThreadID) = -1 then
      //  DebugLn(DBG_WARNINGS, 'Failed to retrieve ThreadId of new thread. Errcode: %d', [fpgeterrno]);
      Exit;
      end;

    if (not FProcessStarted) and (wstopsig(FStatus) <> SIGTRAP) then begin
      // attached, should be SigStop, but may be out of order
      debugln(DBG_VERBOSE, ['Attached ', wstopsig(FStatus)]);
      result := deCreateProcess;
      FProcessStarted:=true;
      if not wstopsig(FStatus) = SIGSTOP then
        FPostponedSignals.AddSignal(AThread.Id, FStatus);
    end

    else
    case wstopsig(FStatus) of
      SIGTRAP:
        begin
        if not FProcessStarted then
          begin
          result := deCreateProcess;
          FProcessStarted:=true;
          if fpPTrace(PTRACE_SETOPTIONS, ProcessID, nil,  Pointer( PTRACE_O_TRACECLONE) ) <> 0 then
            writeln('Failed to set set trace options. Errcode: '+inttostr(fpgeterrno));
          end
        else
// TODO: check it is not a real breakpoint
// or end of single step
//          if TDbgLinuxThread(AThread).FInternalPauseRequested then begin
//            DebugLn(DBG_VERBOSE, ['Received late SigTrag for thread ', AThread.ID]);
//            result := deInternalContinue; // left over signal
//          end
//          else
//            begin
            result := deBreakpoint; // or pause requested
            if not TDbgLinuxThread(AThread).FIsSteppingBreakPoint then
              AThread.CheckAndResetInstructionPointerAfterBreakpoint;
//            end;
        end;
      SIGBUS:
        begin
        ExceptionClass:='SIGBUS';
        TDbgLinuxThread(AThread).FExceptionSignal:=SIGBUS;
        result := deException;
        end;
      SIGINT:
        begin
        ExceptionClass:='SIGINT';
        TDbgLinuxThread(AThread).FExceptionSignal:=SIGINT;
        result := deException;
        end;
      SIGSEGV:
        begin
        ExceptionClass:='SIGSEGV';
        TDbgLinuxThread(AThread).FExceptionSignal:=SIGSEGV;
        result := deException;
        end;
      SIGCHLD:
        begin
        TDbgLinuxThread(AThread).FExceptionSignal:=SIGCHLD;
        result := deInternalContinue;
        end;
      SIGKILL:
        begin
        if FIsTerminating then
          result := deInternalContinue
        else
          begin
          ExceptionClass:='SIGKILL';
          TDbgLinuxThread(AThread).FExceptionSignal:=SIGKILL;
          result := deException;
          end;
        end;
      SIGSTOP:
        begin
          // New thread (stopped within the new thread)
          result := deInternalContinue;
        end
      else
        begin
        ExceptionClass:='Unknown exception code '+inttostr(wstopsig(FStatus));
        TDbgLinuxThread(AThread).FExceptionSignal:=wstopsig(FStatus);
        result := deException;
        end;
    end; {case}
    if result=deException then
      ExceptionClass:='External: '+ExceptionClass;
    end
  else
    raise exception.CreateFmt('Received unknown status %d from process with pid=%d',[FStatus, ProcessID]);

  TDbgLinuxThread(AThread).FIsSteppingBreakPoint := False;


  if (result = deBreakpoint) and (AThread <> nil) then
    // Check if the breakpoint is the special breakpoint that is inserted to
    // detect the (un)loading of libraries.
    Result := CheckForSOLibDebugEvent(AThread);

  if Result in [deException, deBreakpoint, deFinishedStep] then begin // deFinishedStep will not be set here
    {$IFDEF DebuglnLinuxDebugEvents}
    debuglnenter('STOP ALL THREADS');
    {$ENDIF}
    // Signal all other threads to pause
    for TDbgThread(ThreadToPause) in FThreadMap do begin
      if (ThreadToPause <> AThread) then begin
        while  (not ThreadToPause.FIsPaused) do begin

          // Check if any thread is already interrupted
          while ExistsPendingSignal(Pid, WaitStatus, ThreadSignaled, True) do begin
            if (ThreadSignaled = nil) or
               (ThreadSignaled.CheckSignalForPostponing(WaitStatus))
            then
              FPostponedSignals.AddSignal(PID, WaitStatus);
          end;
          if ThreadToPause.FIsPaused or ThreadToPause.FHasExited then
            break;

          DebugLn(DBG_VERBOSE and (ThreadToPause.FInternalPauseRequested), ['Re-Request Internal pause for ', ThreadToPause.ID]);
          ThreadToPause.FInternalPauseRequested:=false;
          if not ThreadToPause.RequestInternalPause then // will fail, if already paused
             break;

          if ExistsPendingSignal(Pid, WaitStatus, ThreadSignaled, False) then begin
            if (ThreadSignaled = nil) or
               (ThreadSignaled.CheckSignalForPostponing(WaitStatus))
            then
              FPostponedSignals.AddSignal(PID, WaitStatus);
          end;

        end;
      end;
    end;
    {$IFDEF DebuglnLinuxDebugEvents}
    debuglnexit('<<');
    {$ENDIF}
  end;

  it := TThreadMapUnLockedEnumerator.Create(FThreadMap); // At this point no other thread (ide-main, ...) can add an iterator to the map
  it.First;
  while not it.EOM do begin
    TDbgThread(ThreadToPause) := it.Current;
    if ThreadToPause.FHasExited then begin
      Process.RemoveThread(ThreadToPause.ID); // TODO: postpone ?
      if ThreadToPause <> AThread then
        ThreadToPause.Free;
    end;
    it.Next;
  end;
  it.Free;

  {$IFDEF DebuglnLinuxDebugEvents}
  for TDbgThread(ThreadToPause) in FThreadMap do
  debugln(FPDBG_LINUX, [ThreadToPause.id, ' =athrd:', ThreadToPause = AThread, ' psd:', ThreadToPause.FIsPaused,ThreadToPause.FIsInInternalPause, ' exs:', ThreadToPause.FExceptionSignal, '  sstep:',ThreadToPause.NextIsSingleStep]);
  debugln(FPDBG_LINUX, '<<<<<<<<<<<<<<<<<<<<<<<<');
  {$ENDIF}

end;

initialization
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  FPDBG_LINUX := DebugLogger.FindOrRegisterLogGroup('FPDBG_LINUX' {$IFDEF DebuglnLinuxDebugEvents} , True {$ENDIF} );

  process_vm_lib := LoadLibrary('libc.' + SharedSuffix);
  if process_vm_lib = 0 then
    process_vm_lib := LoadLibrary('libc.' + SharedSuffix + '.6');
  if process_vm_lib <> 0 then
    process_vm_readv := Tprocess_vm_readv(GetProcAddress(process_vm_lib, 'process_vm_readv'));

  RegisterDbgOsClasses(TOSDbgClasses.Create(
    TDbgLinuxProcess,
    TDbgLinuxThread,
    TX86AsmDecoder
  ));

finalization

  if process_vm_lib <> 0 then
    FreeLibrary(process_vm_lib);

end.
