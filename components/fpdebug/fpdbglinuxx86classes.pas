unit FpDbgLinuxX86Classes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif},
  DbgIntfBaseTypes, FpDbgLinuxExtra, FpDbgClasses, FpDbgLinuxClasses, FpDbgCpuX86, FpDbgCommon,
  FpDbgUtil, FpDbgDisasX86, LazDebuggerIntfFloatTypes;

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

  NT_PRSTATUS    = 1;
  NT_PRFPREG     = 2;
  NT_PRPSINFO    = 3;
  NT_TASKSTRUCT  = 4;
  NT_AUXV        = 6;
  NT_X86_XSTATE  = $202;

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

  { TDbgX86LinuxThread }

  TDbgX86LinuxThread = class(TDbgLinuxThread)
  private
    FUserRegs, FStoredUserRegs: TUserRegs;
    FFpRegs: user_fpxregs_struct32;
    FHasThreadState: boolean;
    FFpRegsAvail: Boolean;
    FUserRegsChanged: boolean;
    FHasResetInstructionPointerAfterBreakpoint: boolean;

    FUnwinder: TDbgStackUnwinderX86MultiMethod;

    function GetDebugRegOffset(ind: byte): pointer;
    function ReadThreadState: boolean;
    function ReadDebugReg(ind: byte; out AVal: PtrUInt): boolean;
    function WriteDebugReg(ind: byte; AVal: PtrUInt): boolean;
  protected
    procedure ResetPauseStates; override;
    function GetInstructionPointerForHasBreakpointInfoForAddress: TDBGPtr; override;
    function GetCurrentStackFrameInfo: TDbgStackFrameInfo; override;
  public
    destructor Destroy; override;
    function GetStackUnwinder: TDbgStackUnwinder; override;

    procedure BeforeContinue; override;

    procedure ApplyWatchPoints(AWatchPointData: TFpWatchPointData); override;
    function DetectHardwareWatchpoint: TFpInternalWatchpoint; override;

    procedure LoadRegisterValues; override;
    procedure SetRegisterValue(AName: string; AValue: QWord); override;
    procedure StoreRegisters; override;
    procedure RestoreRegisters; override;

    function ResetInstructionPointerAfterBreakpoint: boolean; override;
    function GetInstructionPointerRegisterValue: TDbgPtr; override;
    function GetStackBasePointerRegisterValue: TDbgPtr; override;
    function GetStackPointerRegisterValue: TDbgPtr; override;
    procedure SetInstructionPointerRegisterValue(AValue: TDbgPtr); override;
    procedure SetStackPointerRegisterValue(AValue: TDbgPtr); override;
  end;

  { TDbgX86LinuxProcess }

  TDbgX86LinuxProcess = class(TDbgLinuxProcess)
  protected
    function DbgThreadClass: TDbgThreadClass; override;
  public
    class function isSupported(ATargetInfo: TTargetDescriptor): boolean; override;
  end;


implementation

var
  DBG_VERBOSE, DBG_WARNINGS, FPDBG_LINUX: PLazLoggerLogGroup;

{ TDbgX86LinuxThread }

function TDbgX86LinuxThread.GetDebugRegOffset(ind: byte): pointer;
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

function TDbgX86LinuxThread.ReadThreadState: boolean;
var
  io: iovec;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxThread.ReadThreadState');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxThread.ReadThreadState: IsPaused');

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

function TDbgX86LinuxThread.ReadDebugReg(ind: byte; out AVal: PtrUInt): boolean;
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

function TDbgX86LinuxThread.WriteDebugReg(ind: byte; AVal: PtrUInt): boolean;
begin
  if fpPTrace(PTRACE_POKEUSR, ID, GetDebugRegOffset(ind), pointer(AVal)) = -1 then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to write dr'+inttostr(ind)+'-debug register. Errcode: '+inttostr(fpgeterrno));
    result := false;
    end
  else
    result := true;
end;

procedure TDbgX86LinuxThread.ResetPauseStates;
begin
  inherited ResetPauseStates;
  FHasThreadState := False;
  FHasResetInstructionPointerAfterBreakpoint := False;
end;

function TDbgX86LinuxThread.GetInstructionPointerForHasBreakpointInfoForAddress: TDBGPtr;
begin
  Result := GetInstructionPointerRegisterValue;
  if (Result <> 0) and not FHasResetInstructionPointerAfterBreakpoint then
    Result := Result - 1;
end;

function TDbgX86LinuxThread.GetCurrentStackFrameInfo: TDbgStackFrameInfo;
begin
  Result := TDbgStackFrameSteppingInfoX86.Create(Self);
end;

function TDbgX86LinuxThread.GetStackUnwinder: TDbgStackUnwinder;
begin
  if FUnwinder = nil then
    FUnwinder := TDbgStackUnwinderX86MultiMethod.Create(Process);
  Result := FUnwinder;
end;

destructor TDbgX86LinuxThread.Destroy;
begin
  inherited Destroy;
  FUnwinder.Free;
end;

procedure TDbgX86LinuxThread.BeforeContinue;
var
  io: iovec;
begin
  if not IsPaused then
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

procedure TDbgX86LinuxThread.ApplyWatchPoints(AWatchPointData: TFpWatchPointData);
var
  i: integer;
  r: boolean;
  dr7: PtrUInt;
  addr: PtrUInt;
begin
  if (udeKeepExternalWatchPointData in Process.HandleUserDebugEvents) and
     (TFpIntelWatchPointData(AWatchPointData).Dr7 = 0)
  then
    exit;
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

function TDbgX86LinuxThread.DetectHardwareWatchpoint: TFpInternalWatchpoint;
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
      FHitExternalWatchPoint := True; // not set by the debugger
  end;
end;

procedure TDbgX86LinuxThread.LoadRegisterValues;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgX86LinuxThread.LoadRegisterValues');{$ENDIF}
  assert(IsPaused, 'TDbgX86LinuxThread.LoadRegisterValues: IsPaused');

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

    FRegisterValueList.DbgRegisterAutoCreate['cs'].SetValue(FUserRegs.regs32[xcs], IntToStr(FUserRegs.regs32[xcs]),4,51);
    FRegisterValueList.DbgRegisterAutoCreate['ss'].SetValue(FUserRegs.regs32[xss], IntToStr(FUserRegs.regs32[xss]),4,52);
    FRegisterValueList.DbgRegisterAutoCreate['ds'].SetValue(FUserRegs.regs32[xds], IntToStr(FUserRegs.regs32[xds]),4,53);
    FRegisterValueList.DbgRegisterAutoCreate['es'].SetValue(FUserRegs.regs32[xes], IntToStr(FUserRegs.regs32[xes]),4,50);
    FRegisterValueList.DbgRegisterAutoCreate['fs'].SetValue(FUserRegs.regs32[xfs], IntToStr(FUserRegs.regs32[xfs]),4,54);
    FRegisterValueList.DbgRegisterAutoCreate['gs'].SetValue(FUserRegs.regs32[xgs], IntToStr(FUserRegs.regs32[xgs]),4,55);
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
      FRegisterValueList.DbgRegisterAutoCreate['st0'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 0*4])^),10,33);
      FRegisterValueList.DbgRegisterAutoCreate['st1'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 1*4])^),10,34);
      FRegisterValueList.DbgRegisterAutoCreate['st2'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 2*4])^),10,35);
      FRegisterValueList.DbgRegisterAutoCreate['st3'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 3*4])^),10,36);
      FRegisterValueList.DbgRegisterAutoCreate['st4'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 4*4])^),10,37);
      FRegisterValueList.DbgRegisterAutoCreate['st5'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 5*4])^),10,38);
      FRegisterValueList.DbgRegisterAutoCreate['st6'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 6*4])^),10,39);
      FRegisterValueList.DbgRegisterAutoCreate['st7'].SetValue(0, FloatToStr(PExtended(@FFpRegs.st_space[ 7*4])^),10,40);

      FRegisterValueList.DbgRegisterAutoCreate['fctrl'].SetValue(FFpRegs.cwd, IntToStr(FFpRegs.cwd),2,510);
      FRegisterValueList.DbgRegisterAutoCreate['fstat'].SetValue(FFpRegs.swd, IntToStr(FFpRegs.swd),2,511);
      FRegisterValueList.DbgRegisterAutoCreate['ftwd'].SetValue(FFpRegs.twd, IntToStr(FFpRegs.twd),2,512);
      FRegisterValueList.DbgRegisterAutoCreate['fop'].SetValue(FFpRegs.fop, IntToStr(FFpRegs.fop),2,513);
      FRegisterValueList.DbgRegisterAutoCreate['fcs'].SetValue(FFpRegs.fcs, IntToStr(FFpRegs.fcs),4,514);
      FRegisterValueList.DbgRegisterAutoCreate['fip'].SetValue(FFpRegs.fip, IntToStr(FFpRegs.fip),4,515);
      FRegisterValueList.DbgRegisterAutoCreate['foo'].SetValue(FFpRegs.foo, IntToStr(FFpRegs.foo),4,516);
      FRegisterValueList.DbgRegisterAutoCreate['fos'].SetValue(FFpRegs.fos, IntToStr(FFpRegs.fos),4,517);

      FRegisterValueList.DbgRegisterAutoCreate['Xmm0' ].SetValue(@FFpRegs.xmm_space[0*4],16,17, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm1' ].SetValue(@FFpRegs.xmm_space[1*4],16,18, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm2' ].SetValue(@FFpRegs.xmm_space[2*4],16,19, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm3' ].SetValue(@FFpRegs.xmm_space[3*4],16,20, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm4' ].SetValue(@FFpRegs.xmm_space[4*4],16,21, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm5' ].SetValue(@FFpRegs.xmm_space[5*4],16,22, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm6' ].SetValue(@FFpRegs.xmm_space[6*4],16,23, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm7' ].SetValue(@FFpRegs.xmm_space[7*4],16,24, @XmmToFormat);

      FRegisterValueList.DbgRegisterAutoCreate['mxcsr'].SetValue(FFpRegs.fos, IntToStr(FFpRegs.mxcsr),4,620);

      EnableFloatExceptions;
    end;
  end;
  FRegisterValueListValid:=true;
end;

procedure TDbgX86LinuxThread.SetRegisterValue(AName: string; AValue: QWord);
begin
  if Process.Mode=dm32 then
  begin
    assert((AValue and QWord($ffffffff00000000) = 0) or (AValue and QWord($ffffffff00000000) = QWord($ffffffff00000000)), 'TDbgX86LinuxThread.SetRegisterValue: (AValue and QWord($ffffffff00000000) = 0) or (AValue and QWord($ffffffff00000000) = QWord($ffffffff00000000))');
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

procedure TDbgX86LinuxThread.StoreRegisters;
begin
  Assert(FHasThreadState);
  FStoredUserRegs := FUserRegs;
end;

procedure TDbgX86LinuxThread.RestoreRegisters;
begin
  FUserRegs:=FStoredUserRegs;
  FUserRegsChanged := true;
  FRegisterValueListValid := False;
end;

function TDbgX86LinuxThread.ResetInstructionPointerAfterBreakpoint: boolean;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgX86LinuxThread.ResetInstructionPointerAfterBreakpoint');{$ENDIF}
  assert(IsPaused, 'TDbgX86LinuxThread.ResetInstructionPointerAfterBreakpoint: IsPaused');

  if not ReadThreadState then
    exit(False);
  result := true;
  if FHasResetInstructionPointerAfterBreakpoint then
    exit;

  if Process.Mode=dm32 then
    Dec(FUserRegs.regs32[eip])
  else
    Dec(FUserRegs.regs64[rip]);
  FUserRegsChanged:=true;
  FHasResetInstructionPointerAfterBreakpoint := True;
end;

function TDbgX86LinuxThread.GetInstructionPointerRegisterValue: TDbgPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxThread.GetInstructionPointerRegisterValue');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxThread.GetInstructionPointerRegisterValue: IsPaused');

  Result := 0;
  if not ReadThreadState then
    exit;
  if Process.Mode=dm32 then
    result := FUserRegs.regs32[eip]
  else
    result := FUserRegs.regs64[rip];
end;

function TDbgX86LinuxThread.GetStackBasePointerRegisterValue: TDbgPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxThread.GetStackBasePointerRegisterValue');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxThread.GetStackBasePointerRegisterValue: IsPaused');

  Result := 0;
  if not ReadThreadState then
    exit;
  if Process.Mode=dm32 then
    result := FUserRegs.regs32[ebp]
  else
    result := FUserRegs.regs64[rbp];
end;

function TDbgX86LinuxThread.GetStackPointerRegisterValue: TDbgPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxThread.GetStackPointerRegisterValue');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxThread.GetStackPointerRegisterValue: IsPaused');

  Result := 0;
  if not ReadThreadState then
    exit;
  if Process.Mode=dm32 then
    result := FUserRegs.regs32[UESP]
  else
    result := FUserRegs.regs64[rsp];
end;

procedure TDbgX86LinuxThread.SetInstructionPointerRegisterValue(AValue: TDbgPtr);
begin
  if not FHasThreadState then
    exit;
  if Process.Mode=dm32 then
    FUserRegs.regs32[eip] := AValue
  else
    FUserRegs.regs64[rip] := AValue;
end;

procedure TDbgX86LinuxThread.SetStackPointerRegisterValue(AValue: TDbgPtr);
begin
  if not FHasThreadState then
    exit;
  if Process.Mode=dm32 then
    FUserRegs.regs32[UESP] := AValue
  else
    FUserRegs.regs64[rsp] := AValue;
end;

{ TDbgX86LinuxProcess }

function TDbgX86LinuxProcess.DbgThreadClass: TDbgThreadClass;
begin
  Result := TDbgX86LinuxThread;
end;

class function TDbgX86LinuxProcess.isSupported(ATargetInfo: TTargetDescriptor): boolean;
begin
  result := (ATargetInfo.OS = osLinux) and
            (ATargetInfo.machineType in [mt386, mtX86_64]);
end;

initialization
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  FPDBG_LINUX := DebugLogger.FindOrRegisterLogGroup('FPDBG_LINUX' {$IFDEF DebuglnLinuxDebugEvents} , True {$ENDIF} );

  RegisterDbgOsClasses(TOSDbgClasses.Create(
    TDbgX86LinuxProcess,
    TDbgX86LinuxThread,
    TX86AsmDecoder
  ));

end.

