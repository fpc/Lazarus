unit FpDbgLinuxClasses;

{$mode objfpc}{$H+}
{$packrecords c}
{off $define DebuglnLinuxDebugEvents}

interface

uses
  Classes,
  SysUtils,
  BaseUnix,
  termio,
  process,
  FpDbgClasses,
  FpDbgLoader,
  DbgIntfBaseTypes,
  FpDbgLinuxExtra,
  FpDbgInfo,
  FpDbgUtil,
  UTF8Process,
  LazLoggerBase, Maps;

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

  { TDbgLinuxThread }

  TDbgLinuxThread = class(TDbgThread)
  private
    FUserRegs: TUserRegs;
    FUserRegsChanged: boolean;
    // FExceptionSignal
    // For the current thread this is wstopsig(FStatus)
    // For other threads this is the full status (stored to execute event later.)
    FExceptionSignal: cint;
    FHasExceptionSignal: Boolean;
    FIsPaused, FInternalPauseRequested, FIsInInternalPause: boolean;
    FIsSteppingBreakPoint: boolean;
    FHasThreadState: boolean;
    function GetDebugRegOffset(ind: byte): pointer;
    function ReadDebugReg(ind: byte; out AVal: PtrUInt): boolean;
    function WriteDebugReg(ind: byte; AVal: PtrUInt): boolean;
  protected
    function ReadThreadState: boolean;

    function RequestInternalPause: Boolean;
    procedure CheckStatusReceived(AWaitedStatus: cint);
    procedure ResetPauseStates;
  public
    function ResetInstructionPointerAfterBreakpoint: boolean; override;
    function AddWatchpoint(AnAddr: TDBGPtr): integer; override;
    function RemoveWatchpoint(AnId: integer): boolean; override;
    function DetectHardwareWatchpoint: integer; override;
    procedure BeforeContinue; override;
    procedure LoadRegisterValues; override;

    function GetInstructionPointerRegisterValue: TDbgPtr; override;
    function GetStackBasePointerRegisterValue: TDbgPtr; override;
    function GetStackPointerRegisterValue: TDbgPtr; override;
  end;

  { TDbgLinuxProcess }

  TDbgLinuxProcess = class(TDbgProcess)
  private
    FStatus: cint;
    FProcessStarted: boolean;
    FProcProcess: TProcessUTF8;
    FIsTerminating: boolean;
    FMasterPtyFd: cint;
    FCurrentThreadId: THandle;
    {$ifndef VER2_6}
    procedure OnForkEvent(Sender : TObject);
    {$endif}
  protected
    procedure InitializeLoaders; override;
    function CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread; override;
    function AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent; override;
  public
    class function StartInstance(AFileName: string; AParams, AnEnvironment: TStrings;
      AWorkingDirectory, AConsoleTty: string; AFlags: TStartInstanceFlags): TDbgProcess; override;
    constructor Create(const AName: string; const AProcessID, AThreadID: Integer); override;
    destructor Destroy; override;

    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; override;
    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; override;

    function CheckForConsoleOutput(ATimeOutMs: integer): integer; override;
    function GetConsoleOutput: string; override;
    procedure SendConsoleInput(AString: string); override;

    procedure TerminateProcess; override;
    function Pause: boolean; override;

    function Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean; override;
    function WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean; override;
  end;

procedure RegisterDbgClasses;

implementation

var
  DBG_VERBOSE, DBG_WARNINGS: PLazLoggerLogGroup;
  GConsoleTty: string;
  GSlavePTyFd: cint;

procedure RegisterDbgClasses;
begin
  OSDbgClasses.DbgProcessClass:=TDbgLinuxProcess;
  OSDbgClasses.DbgThreadClass:=TDbgLinuxThread;
end;

Function WIFSTOPPED(Status: Integer): Boolean;
begin
  WIFSTOPPED:=((Status and $FF)=$7F);
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

  if GConsoleTty<>'' then
    begin
    ConsoleTtyFd:=FpOpen(GConsoleTty,O_RDWR+O_NOCTTY);
    if ConsoleTtyFd>-1 then
      begin
      if (FpIOCtl(ConsoleTtyFd, TIOCSCTTY, nil) = -1) then
        begin
        // This call always fails for some reason. That's also why login_tty can not be used. (login_tty
        // also calls TIOCSCTTY, but when it fails it aborts) The failure is ignored.
        // writeln('Failed to set tty '+inttostr(fpgeterrno));
        end;

      FpDup2(ConsoleTtyFd,0);
      FpDup2(ConsoleTtyFd,1);
      FpDup2(ConsoleTtyFd,2);
      end
    else
      writeln('Failed to open tty '+GConsoleTty+'. Errno: '+inttostr(fpgeterrno));
    end
  else if GSlavePTyFd>-1 then
    begin
    if login_tty(GSlavePTyFd) <> 0 then
      writeln('Failed to login to tty. Errcode: '+inttostr(fpgeterrno)+' - '+inttostr(GSlavePTyFd));
    end;

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


function TDbgLinuxThread.ReadThreadState: boolean;
var
  io: iovec;
begin
  assert(FIsPaused, 'TDbgLinuxThread.ReadThreadState: FIsPaused');
  result := true;
  if FHasThreadState then
    exit;
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
end;

function TDbgLinuxThread.RequestInternalPause: Boolean;
begin
  Result := False;
  if FInternalPauseRequested or FIsPaused then
    exit;

  result := fpkill(ID, SIGSTOP)=0;
  {$IFDEF DebuglnLinuxDebugEvents}
  debugln('TDbgLinuxThread.RequestInternalPause fpkill(%d, SIGSTOP) => %s', [ID, dbgs(Result)]);
  {$ENDIF}
  if not result then
    begin
    // TODO: errChld -> remove thread
    DebugLn(DBG_WARNINGS, 'Failed to send SIGTSTOP to process %d. Errno: %d',[ID, errno]);
    exit;
    end;

  FInternalPauseRequested := True;
end;

procedure TDbgLinuxThread.CheckStatusReceived(AWaitedStatus: cint);
begin
  Assert(not FIsPaused, 'Got WaitStatus while already paused');
  Assert(not FHasExceptionSignal, 'Got WaitStatus while already having defered status');
  FIsPaused := True;
  FIsInInternalPause := False;
  FExceptionSignal := 0;
  FHasExceptionSignal := True;

  if FInternalPauseRequested and (wstopsig(AWaitedStatus) = SIGSTOP) then begin
    FInternalPauseRequested := False;
    FIsInInternalPause := True;
    FHasExceptionSignal := False;
  end

  else
  if wstopsig(AWaitedStatus) = SIGTRAP then begin
    ReadThreadState;
    CheckAndResetInstructionPointerAfterBreakpoint;
    FHasExceptionSignal := False; // TODO: main loop should search all threads for breakpoints
  end

  else
  if wifexited(AWaitedStatus) and (ID <> Process.ProcessID) then begin
    Process.RemoveThread(ID);
    FHasExceptionSignal := False;
  end;

  //TODO: Handle all signals/exceptions/...

  // Keep FExceptionSignal = zero, unless it really is pending
  if FHasExceptionSignal then
    FExceptionSignal := AWaitedStatus;
end;

procedure TDbgLinuxThread.ResetPauseStates;
begin
  FIsInInternalPause := False;
  FIsPaused := False;
  FExceptionSignal := 0;
  FHasThreadState := False;
end;

function TDbgLinuxThread.ResetInstructionPointerAfterBreakpoint: boolean;
begin
  ReadThreadState;
  result := true;

  if Process.Mode=dm32 then
    Dec(FUserRegs.regs32[eip])
  else
    Dec(FUserRegs.regs64[rip]);
  FUserRegsChanged:=true;
end;

function TDbgLinuxThread.AddWatchpoint(AnAddr: TDBGPtr): integer;
var
  dr7: PtrUInt;

  function SetHWBreakpoint(ind: byte): boolean;
  var
    Addr: PtrUInt;
  begin
    result := false;
    if ((dr7 and (1 shl ind))=0) then
    begin
      if not ReadDebugReg(ind, Addr) or (Addr<>0) then
        Exit;

      dr7 := dr7 or (1 shl (ind*2));
      dr7 := dr7 or ($30000 shl (ind*4));
      if WriteDebugReg(7, dr7) and WriteDebugReg(ind, AnAddr) then
        result := true;
    end;
  end;

var
  i: integer;
begin
  result := -1;
  if not ReadDebugReg(7, dr7) then
    Exit;

  i := 0;
  while (i<4) and not SetHWBreakpoint(i) do
    inc(i);
  if i=4 then
    DebugLn(DBG_WARNINGS, 'No hardware breakpoint available.')
  else
    result := i;
end;

function TDbgLinuxThread.RemoveWatchpoint(AnId: integer): boolean;
var
  dr7: PtrUInt;
  addr: PtrUInt;
begin
  result := false;
  if not ReadDebugReg(7, dr7) or not ReadDebugReg(AnId, addr) then
    Exit;

  if (addr<>0) and ((dr7 and (1 shl (AnId*2)))<>0) then
  begin
    dr7 := dr7 xor (1 shl (AnId*2));
    dr7 := dr7 xor ($30000 shl (AnId*4));
    if WriteDebugReg(AnId, 0) and WriteDebugReg(7, dr7) then
      Result := True;
  end
  else
  begin
    DebugLn(DBG_WARNINGS, 'HW watchpoint %d is not set.',[AnId]);
  end;
end;

function TDbgLinuxThread.DetectHardwareWatchpoint: integer;
var
  dr6: PtrUInt;
begin
  result := -1;
  if ReadDebugReg(6, dr6) then
  begin
    if dr6 and 1 = 1 then result := 0
    else if dr6 and 2 = 2 then result := 1
    else if dr6 and 4 = 4 then result := 2
    else if dr6 and 8 = 8 then result := 3;
  end;
end;

procedure TDbgLinuxThread.BeforeContinue;
var
  io: iovec;
begin
  if not FIsPaused then
    exit;

  if Process.CurrentWatchpoint>-1 then
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
end;

procedure TDbgLinuxThread.LoadRegisterValues;
begin
  ReadThreadState;
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
  end;
  FRegisterValueListValid:=true;
end;

function TDbgLinuxThread.GetInstructionPointerRegisterValue: TDbgPtr;
begin
  ReadThreadState;
  if Process.Mode=dm32 then
    result := FUserRegs.regs32[eip]
  else
    result := FUserRegs.regs64[rip];
end;

function TDbgLinuxThread.GetStackBasePointerRegisterValue: TDbgPtr;
begin
  ReadThreadState;
  if Process.Mode=dm32 then
    result := FUserRegs.regs32[ebp]
  else
    result := FUserRegs.regs64[rbp];
end;

function TDbgLinuxThread.GetStackPointerRegisterValue: TDbgPtr;
begin
  ReadThreadState;
  if Process.Mode=dm32 then
    result := FUserRegs.regs32[UESP]
  else
    result := FUserRegs.regs64[rsp];
end;

{ TDbgLinuxProcess }

procedure TDbgLinuxProcess.InitializeLoaders;
begin
  TDbgImageLoader.Create(Name).AddToLoaderList(LoaderList);
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

constructor TDbgLinuxProcess.Create(const AName: string; const AProcessID,
  AThreadID: Integer);
begin
  FMasterPtyFd:=-1;
  inherited Create(AName, AProcessID, AThreadID);
end;

destructor TDbgLinuxProcess.Destroy;
begin
  FProcProcess.Free;
  inherited Destroy;
end;

class function TDbgLinuxProcess.StartInstance(AFileName: string; AParams,
  AnEnvironment: TStrings; AWorkingDirectory, AConsoleTty: string;
  AFlags: TStartInstanceFlags): TDbgProcess;
var
  PID: TPid;
  AProcess: TProcessUTF8;
  AMasterPtyFd: cint;
  AnExecutabeFilename: string;
begin
  result := nil;

  AnExecutabeFilename:=ExcludeTrailingPathDelimiter(AFileName);
  if DirectoryExists(AnExecutabeFilename) then
  begin
    DebugLn(DBG_WARNINGS, 'Can not debug %s, because it''s a directory',[AnExecutabeFilename]);
    Exit;
  end;

  if not FileExists(AFileName) then
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

  AProcess := TProcessUTF8.Create(nil);
  try
    AProcess.OnForkEvent:=@OnForkEvent;
    AProcess.Executable:=AnExecutabeFilename;
    AProcess.Parameters:=AParams;
    AProcess.Environment:=AnEnvironment;
    AProcess.CurrentDirectory:=AWorkingDirectory;

    AProcess.Execute;
    PID:=AProcess.ProcessID;

    sleep(100);
    result := TDbgLinuxProcess.Create(AFileName, Pid, -1);
    TDbgLinuxProcess(result).FMasterPtyFd := AMasterPtyFd;
    TDbgLinuxProcess(result).FProcProcess := AProcess;
  except
    on E: Exception do
    begin
      DebugLn(DBG_WARNINGS, Format('Failed to start process "%s". Errormessage: "%s".',[AFileName, E.Message]));
      AProcess.Free;

    if GSlavePTyFd>-1 then
      FpClose(GSlavePTyFd);
    if AMasterPtyFd>-1 then
      FpClose(AMasterPtyFd);
    end;
  end;
end;

function TDbgLinuxProcess.ReadData(const AAdress: TDbgPtr;
  const ASize: Cardinal; out AData): Boolean;

var
  WordSize: byte;

  function ReadWordSize(Adr: TDbgPtr; out AVal: TDBGPtr): boolean;
  var
    e: integer;
  begin
    errno := 0;
    AVal := TDbgPtr(fpPTrace(PTRACE_PEEKDATA, FCurrentThreadId, pointer(Adr), nil));
    e := fpgeterrno;
    if e <> 0 then
      begin
      DebugLn(DBG_WARNINGS, 'Failed to read data at address '+FormatAddress(Adr)+' from processid '+inttostr(FCurrentThreadId)+'. Errcode: '+inttostr(e));
      result := false;
      end
    else
      result := true;
  end;

var
  AVal: TDbgPtr;
  AAdressAlign: TDBGPtr;
  BytesRead: integer;
  ReadBytes: integer;
  PB: PByte;
  buf: pbyte;
begin
  BytesRead := 0;
  result := false;
  getmem(buf, ASize);
  try
    WordSize:=DBGPTRSIZE[Mode];
    if AAdress mod WordSize <> 0 then
      begin
      AAdressAlign := ((PtrUInt(AAdress)) and not PtrUInt(WordSize - 1));
      if not ReadWordSize(AAdressAlign, AVal) then
        Exit;
      pb := @AVal;
      BytesRead:=WordSize-(AAdress-AAdressAlign);
      if BytesRead>=ASize then
        BytesRead:=ASize;
      move(pb[AAdress-AAdressAlign], buf[0], BytesRead);
      inc(AAdressAlign, WordSize);
      end
    else
      AAdressAlign:=AAdress;

    while BytesRead<ASize do
      begin
      if not ReadWordSize(AAdressAlign, AVal) then
        exit;
      if WordSize<(ASize-BytesRead) then
        ReadBytes:=WordSize
      else
        ReadBytes:=(ASize-BytesRead);
      move(AVal, buf[BytesRead], ReadBytes);
      inc(BytesRead, ReadBytes);
      inc(AAdressAlign, WordSize);

      end;
    System.Move(buf^, AData, BytesRead);
  finally
    freemem(buf);
  end;
  MaskBreakpointsInReadData(AAdress, ASize, AData);
  result := true;
end;

function TDbgLinuxProcess.WriteData(const AAdress: TDbgPtr;
  const ASize: Cardinal; const AData): Boolean;
var
  e: integer;
  pi: TDBGPtr;
  WordSize: integer;
begin
  result := false;
  WordSize:=DBGPTRSIZE[Mode];

  if ASize>WordSize then
    DebugLn(DBG_WARNINGS, 'Can not write more then '+IntToStr(WordSize)+' bytes.')
  else
    begin
    if ASize<WordSize then
      begin
      fpseterrno(0);
      pi := TDbgPtr(fpPTrace(PTRACE_PEEKDATA, FCurrentThreadId, pointer(AAdress), nil));
      e := fpgeterrno;
      if e <> 0 then
        begin
        DebugLn(DBG_WARNINGS, 'Failed to read data. Errcode: '+inttostr(e));
        result := false;
        exit;
        end;
      end;
    move(AData, pi, ASize);

    fpPTrace(PTRACE_POKEDATA, FCurrentThreadId, pointer(AAdress), pointer(pi));
    e := fpgeterrno;
    if e <> 0 then
      begin
      DebugLn(DBG_WARNINGS, 'Failed to write data. Errcode: '+inttostr(e));
      result := false;
      end;
    end;

  result := true;
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
begin
  if fpioctl(FMasterPtyFd, FIONREAD, @ABytesAvailable)<0 then
    ABytesAvailable := 0;

  if ABytesAvailable>0 then
  begin
    setlength(result, ABytesAvailable);
    ABytesRead := fpRead(FMasterPtyFd, result[1], ABytesAvailable);
    SetLength(result, ABytesRead);
  end
  else
    result := '';
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
  if not result then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to send SIGTRAP to process %d. Errno: %d',[ProcessID, errno]);
    end;
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
  {$IFDEF DebuglnLinuxDebugEvents}
  debuglnEnter(['>>>>> TDbgLinuxProcess.Continue TID:', AThread.ID, ' SingleStep:', SingleStep ]); try
  {$ENDIF}

  // Terminating process and all threads
  if FIsTerminating then begin
    fpseterrno(0);
    AThread.BeforeContinue;
    fpPTrace(PTRACE_KILL, AThread.ID, pointer(1), nil);
    TDbgLinuxThread(AThread).ResetPauseStates;
    Result := CheckNoError;
    if not FThreadMap.HasId(AThread.ID) then
      AThread.Free;
    exit;
  end;

  // check for pending events in other threads
  assert(not TDbgLinuxThread(AThread).FHasExceptionSignal, 'current thread must not have deferred sig');
  for TDbgThread(ThreadToContinue) in FThreadMap do
    if (ThreadToContinue.FHasExceptionSignal) then begin
      Assert(not ThreadToContinue.FIsInInternalPause, 'internal pause should not have deferred sig');
      AThread.NextIsSingleStep:=False; // UNDO
      {$IFDEF DebuglnLinuxDebugEvents}
      debugln(['Exit for DEFERRED event TID', ThreadToContinue.Id]);
      {$ENDIF}
      exit; // WaitForDebugEvent will report the event // AThread will now be treaded as paused.
    end;


  AThread.NextIsSingleStep:=SingleStep;

  // check other threads if they need a singlestep
  for TDbgThread(ThreadToContinue) in FThreadMap do
    if (ThreadToContinue <> AThread) and ThreadToContinue.FIsPaused then begin
      IP := ThreadToContinue.GetInstructionPointerRegisterValue;
      if HasInsertedBreakInstructionAtLocation(IP) then begin
        TempRemoveBreakInstructionCode(IP);
        ThreadToContinue.BeforeContinue;

        while (ThreadToContinue.GetInstructionPointerRegisterValue = IP) do begin
          fpseterrno(0);
          {$IFDEF DebuglnLinuxDebugEvents}
          Debugln(['Single-stepping other TID: ', ThreadToContinue.ID]);
          {$ENDIF}
          fpPTrace(PTRACE_SINGLESTEP, ThreadToContinue.ID, pointer(1), pointer(wstopsig(TDbgLinuxThread(ThreadToContinue).FExceptionSignal)));
          TDbgLinuxThread(ThreadToContinue).ResetPauseStates; // So BeforeContinue will not run again

          ThreadToContinue.FIsPaused := True;
          if CheckNoError then begin
            PID := fpWaitPid(ThreadToContinue.ID, WaitStatus, __WALL);
            if PID <> ThreadToContinue.ID then begin
              DebugLn(DBG_WARNINGS, ['Error single stepping other thread ', ThreadToContinue.ID, ' waitpid got ', PID, ', ',WaitStatus, ' err ', Errno]);
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

  if HasInsertedBreakInstructionAtLocation(AThread.GetInstructionPointerRegisterValue) then begin
    TempRemoveBreakInstructionCode(AThread.GetInstructionPointerRegisterValue);
    TDbgLinuxThread(AThread).FIsSteppingBreakPoint := True;
    fpseterrno(0);
    AThread.BeforeContinue;
    {$IFDEF DebuglnLinuxDebugEvents}
    Debugln(['Single-stepping current']);
    {$ENDIF}
    fpPTrace(PTRACE_SINGLESTEP, AThread.ID, pointer(1), pointer(wstopsig(TDbgLinuxThread(AThread).FExceptionSignal)));
    TDbgLinuxThread(AThread).ResetPauseStates;
    Result := CheckNoError;
    exit;
  end;

  RestoreTempBreakInstructionCodes;

  ThreadsBeforeContinue;

  // start all other threads
  for TDbgThread(ThreadToContinue) in FThreadMap do begin
    if (ThreadToContinue <> AThread) and (ThreadToContinue.FIsPaused) then begin
      fpseterrno(0);
      {$IFDEF DebuglnLinuxDebugEvents}
      Debugln(['RUN other TID: ', ThreadToContinue.ID]);
      {$ENDIF}
      fpPTrace(PTRACE_CONT, ThreadToContinue.ID, pointer(1), pointer(wstopsig(ThreadToContinue.FExceptionSignal)));
      CheckNoError; // only log
      ThreadToContinue.ResetPauseStates;
    end;
  end;

  if not FIsTerminating then begin
    fpseterrno(0);
    //AThread.BeforeContinue;
    {$IFDEF DebuglnLinuxDebugEvents}
    Debugln(['RUN ']);
    {$ENDIF}
    if SingleStep then
      fpPTrace(PTRACE_SINGLESTEP, AThread.ID, pointer(1), pointer(wstopsig(TDbgLinuxThread(AThread).FExceptionSignal)))
    else
      fpPTrace(PTRACE_CONT, AThread.ID, pointer(1), pointer(wstopsig(TDbgLinuxThread(AThread).FExceptionSignal)));
    TDbgLinuxThread(AThread).ResetPauseStates;
    Result := CheckNoError;
  end;

  if not FThreadMap.HasId(AThread.ID) then
    AThread.Free;
  {$IFDEF DebuglnLinuxDebugEvents}
  finally debuglnExit(['<<<<< TDbgLinuxProcess.Continue ' ]); end;
  {$ENDIF}
end;

function TDbgLinuxProcess.WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean;
var
  PID: THandle;
  ThreadWithEvent: TDbgLinuxThread;
begin
  ThreadIdentifier:=-1;
  ProcessIdentifier:=-1;

  PID := 0;
  for TDbgThread(ThreadWithEvent) in FThreadMap do
    if (TDbgLinuxThread(ThreadWithEvent).FHasExceptionSignal) then begin
      Assert(TDbgLinuxThread(ThreadWithEvent).FIsPaused, 'TDbgLinuxThread(ThreadWithEvent).FIsPaused');
      PID := ThreadWithEvent.ID;
      FStatus := TDbgLinuxThread(ThreadWithEvent).FExceptionSignal;
      DebugLn(DBG_VERBOSE, ['DEFERRED event for ',pid]);
      break;
    end;


  if PID = 0 then
    PID:=FpWaitPid(-1, FStatus, __WALL);
  RestoreTempBreakInstructionCodes;

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
    debugln(['##### GOT EVENT FOR ',pid, ' st ', FStatus]);
    {$ENDIF}
    end;
end;

function TDbgLinuxProcess.AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent;

  function WaitForThread(out WaitStatus: cint; ANoHang: Boolean): TDbgLinuxThread;
  var
    Opts: cint;
    PID: THandle;
  begin
    Result := nil;
    Opts := __WALL;
    if ANoHang then
      Opts := Opts or WNOHANG;

    PID:=FpWaitPid(-1, WaitStatus, Opts);
    if (PID = 0) and (ANoHang) then
      exit;

    DebugLn(DBG_VERBOSE, ['Got SIGNAL for thread: ', pid, ' Status: ',WaitStatus]);
    if not FThreadMap.GetData(PID, Result) then
        Result := nil;
    if Result = nil then DebugLn(DBG_VERBOSE, 'NO THREAD')
    else if Result.FIsPaused then DebugLn(DBG_VERBOSE, 'PAUSED THREAD');
  end;

//var
//  NewThreadID: culong;
var
  ThreadToPause, ThreadSignaled: TDbgLinuxThread;
  WaitStatus: cint;
begin
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
          if TDbgLinuxThread(AThread).FInternalPauseRequested then begin
            DebugLn(DBG_VERBOSE, ['Received late SigTrag for thread ', AThread.ID]);
            result := deInternalContinue; // left over signal
          end
          else
            begin
            result := deBreakpoint; // or pause requested
            if not TDbgLinuxThread(AThread).FIsSteppingBreakPoint then
              AThread.CheckAndResetInstructionPointerAfterBreakpoint;
            end;
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

  if Result in [deException, deBreakpoint, deFinishedStep] then begin // deFinishedStep will not be set here
    {$IFDEF DebuglnLinuxDebugEvents}
    debuglnenter('STOP ALL THREADS');
    {$ENDIF}
    // Signal all other threads to pause
    for TDbgThread(ThreadToPause) in FThreadMap do begin
      if (ThreadToPause <> AThread) and (not ThreadToPause.FIsPaused) then begin

        // Check if any thread is already interrupted
        ThreadSignaled := WaitForThread(WaitStatus, True);

        while ThreadSignaled <> nil do begin
          if not ThreadSignaled.FIsPaused then
            ThreadSignaled.CheckStatusReceived(WaitStatus);
          ThreadSignaled := WaitForThread(WaitStatus, True);
        end;

        while not ThreadToPause.FIsPaused do begin
          ThreadToPause.FInternalPauseRequested:=false;
          if not ThreadToPause.RequestInternalPause then
             break;

          ThreadSignaled := WaitForThread(WaitStatus, False);
          if (ThreadSignaled <> nil) and (not ThreadSignaled.FIsPaused) then
            ThreadSignaled.CheckStatusReceived(WaitStatus);

          DebugLn(DBG_VERBOSE and (not ThreadToPause.FIsPaused), ['Re-Request Internal pause for ', ThreadToPause.ID]);
        end;

      end;
    end;
    {$IFDEF DebuglnLinuxDebugEvents}
    debuglnexit('<<');
    {$ENDIF}
  end;

  {$IFDEF DebuglnLinuxDebugEvents}
  for TDbgThread(ThreadToPause) in FThreadMap do
  debugln([ThreadToPause.id, ' =athrd:', ThreadToPause = AThread, ' psd:', ThreadToPause.FIsPaused,ThreadToPause.FIsInInternalPause,' has:',ThreadToPause.FHasExceptionSignal, ' exs:', ThreadToPause.FExceptionSignal]);
  debugln('<<<<<<<<<<<<<<<<<<<<<<<<');
  {$ENDIF}

end;

initialization
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
end.
