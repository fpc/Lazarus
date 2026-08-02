unit FpDbgWinAarch64Classes;

{$mode objfpc}{$H+}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}

interface

uses
  Classes, windows, SysUtils, FpDbgWinClasses, FpDbgCpuAarch64, FpDbgClasses, FpDbgUtil,
  FpDbgWinExtra, FpDbgCommon, FpdMemoryTools, LazLoggerBase, LazDebuggerIntfBaseTypes;

const
  ARM64_MAX_BREAKPOINTS = 8;
  ARM64_MAX_WATCHPOINTS = 2;
  CONTEXT_ARM64           = $00400000;
  CONTEXT_ARM64_CONTROL         = CONTEXT_ARM64 or $1;
  CONTEXT_ARM64_INTEGER         = CONTEXT_ARM64 or $2;
  CONTEXT_ARM64_FLOATING_POINT  = CONTEXT_ARM64 or $4;
  CONTEXT_ARM64_DEBUG_REGISTERS = CONTEXT_ARM64 or $8;
  CONTEXT_ARM64_X18             = CONTEXT_ARM64 or $10;

  CONTEXT_ARM64_FULL = CONTEXT_ARM64_CONTROL or CONTEXT_ARM64_INTEGER or CONTEXT_ARM64_FLOATING_POINT;
  CONTEXT_ARM64_ALL  = CONTEXT_ARM64_FULL or CONTEXT_ARM64_DEBUG_REGISTERS or CONTEXT_ARM64_X18;

  CPSR_SS_BIT = $00200000; // bit 21 single step


type
  T_ARM64_NT_NEON128 = record
    case Integer of
      0: (Low: QWord; High: Int64);
      1: (D: array[0..1] of Double);
      2: (S: array[0..3] of Single);
      3: (H: array[0..7] of Word);
      4: (B: array[0..15] of Byte);
  end;

  T_ARM64_NT_REGS = record
    case Integer of
      0: ( X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15,
           X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28,
           Fp, Lr: QWord;
         );
      1: ( X: array[0..30] of QWord;
         );
  end;

  T_ARM64_NT_CONTEXT = record
    ContextFlags: DWord;
    Cpsr: DWord;
    //X: array[0..30] of QWord;
    Regs: T_ARM64_NT_REGS;      // X0..X28, Fp, Lr  (or X[0..30])
    SP: QWord;
    PC: QWord;
    V: array[0..31] of T_ARM64_NT_NEON128;
    Fpcr: DWord;
    Fpsr: DWord;
    Bcr: array[0..ARM64_MAX_BREAKPOINTS - 1] of DWord;
    Bvr: array[0..ARM64_MAX_BREAKPOINTS - 1] of QWord;
    Wcr: array[0..ARM64_MAX_WATCHPOINTS - 1] of DWord;
    Wvr: array[0..ARM64_MAX_WATCHPOINTS - 1] of QWord;
  end;
  P_ARM64_NT_CONTEXT = ^T_ARM64_NT_CONTEXT;

  T_ARM64_NT_CONTEXT_DUMMY = record
    ctx: T_ARM64_NT_CONTEXT;
    AlignDummy: array [0..16]of byte;
  end;

  { TDbgWinAarch64Thread }

  TDbgWinAarch64Thread = class(TDbgWinThread)
  private
    _UnAligendContext, FStoredContext: T_ARM64_NT_CONTEXT_DUMMY;
    FCurrentContext: P_ARM64_NT_CONTEXT;
    FThreadContextChanged: boolean;

    //FUserRegs, FStoredUserRegs: TAarch64LinuxGeneralRegisters;
    //FHasThreadState: boolean;
    //FUserRegsChanged: boolean;

    FUnwinder: TDbgStackUnwinder;

    function GetFpThreadContext(var AStorage: T_ARM64_NT_CONTEXT_DUMMY; out ACtxPtr: P_ARM64_NT_CONTEXT; ACtxFlags: TFpWinCtxFlags): Boolean;
    function SetFpThreadContext(ACtxPtr: P_ARM64_NT_CONTEXT; ACtxFlags: TFpWinCtxFlags = cfSkip): Boolean;
  protected
//    //procedure ResetPauseStates; override;
    function GetInstructionPointerForHasBreakpointInfoForAddress: TDBGPtr; override;
    function HasContext: Boolean; override;

  public
    destructor Destroy; override;
    function GetStackUnwinder: TDbgStackUnwinder; override;

    procedure SetSingleStep; override;
    procedure BeforeContinue; override;
    function ReadThreadState: boolean; override;

    procedure ApplyWatchPoints(AWatchPointData: TFpWatchPointData); override;
    function DetectHardwareWatchpoint: TFpInternalWatchpoint; override;

    procedure LoadRegisterValues; override;
    procedure SetRegisterValue(AName: string; AValue: QWord); override;
    procedure StoreRegisters; override;
    procedure RestoreRegisters; override;

    function ResetInstructionPointerAfterBreakpoint: boolean; override;
    function GetAdjustedInstructionPointerRegisterValue: TDbgPtr; override;
    function GetInstructionPointerRegisterValue: TDBGPtr; override;
    function GetStackBasePointerRegisterValue: TDbgPtr; override;
    function GetStackPointerRegisterValue: TDbgPtr; override;
    procedure SetInstructionPointerRegisterValue(AValue: TDbgPtr); override;
    procedure SetStackPointerRegisterValue(AValue: TDbgPtr); override;
  end;

  { TDbgWinAarch64Process }

  TDbgWinAarch64Process = class(TDbgWinProcess)
  protected
    function CreateBreakPointTargetHandler: TFpBreakPointTargetHandler; override;
  public
    class function isSupported(ATargetInfo: TTargetDescriptor): boolean; override;
    function CallParamDefaultLocation(AParamIdx: Integer): TFpDbgMemLocation; override;
  end;


implementation

function SetThreadContext(hThread: THandle; var lpContext: T_ARM64_NT_CONTEXT): BOOL; external 'kernel32' name 'SetThreadContext';
function GetThreadContext(hThread: THandle; var lpContext: T_ARM64_NT_CONTEXT): BOOL; external 'kernel32' name 'GetThreadContext';

var
  DBG_VERBOSE, DBG_WARNINGS, FPDBG_WINDOWS: PLazLoggerLogGroup;

{ TDbgWinAarch64Thread }

function TDbgWinAarch64Thread.GetFpThreadContext(var AStorage: T_ARM64_NT_CONTEXT_DUMMY; out
  ACtxPtr: P_ARM64_NT_CONTEXT; ACtxFlags: TFpWinCtxFlags): Boolean;
begin
  ACtxPtr := AlignPtr(@AStorage, $10);
  FillByte(ACtxPtr^, SizeOf(T_ARM64_NT_CONTEXT), 0);

  SetLastError(0);
  case ACtxFlags of
    cfControl: ACtxPtr^.ContextFlags := CONTEXT_ARM64_CONTROL;
    cfFull:    ACtxPtr^.ContextFlags := CONTEXT_ARM64_FULL;
  end;
  Result := GetThreadContext(Handle, ACtxPtr^);
  DebugLn(DBG_WARNINGS and (not Result), ['Unable to get Context for ', ID, ': ', GetLastErrorText]);
end;

function TDbgWinAarch64Thread.SetFpThreadContext(ACtxPtr: P_ARM64_NT_CONTEXT;
  ACtxFlags: TFpWinCtxFlags): Boolean;
begin
  SetLastError(0);
  case ACtxFlags of
    cfControl: ACtxPtr^.ContextFlags := CONTEXT_ARM64_CONTROL;
    cfFull:    ACtxPtr^.ContextFlags := CONTEXT_ARM64_FULL;
  end;
  //if ccfControl in FThreadContextChangeFlags then
  //  ACtxPtr^.ContextFlags := ACtxPtr^.ContextFlags + CONTEXT_ARM64_CONTROL;
  //if ccfInteger in FThreadContextChangeFlags then
  //  ACtxPtr^.ContextFlags := ACtxPtr^.ContextFlags + CONTEXT_ARM64_INTEGER;

  Result := SetThreadContext(Handle, ACtxPtr^);
  DebugLn(DBG_WARNINGS and (not Result), ['Unable to set Context for ', ID, ': ', GetLastErrorText]);
end;

function TDbgWinAarch64Thread.ReadThreadState: boolean;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgWinThread.ReadThreadState');{$ENDIF}
  assert(MDebugEvent.dwProcessId <> 0, 'TDbgWinThread.ReadThreadState: MDebugEvent.dwProcessId <> 0');

  if Process.ProcessID <> MDebugEvent.dwProcessId then begin
    DebugLn(DBG_WARNINGS, 'ERROR: attempt to read threadstate, for wrong process. Thread: %u Thread-Process: %u Event-Process %u', [Id, Process.ProcessID, MDebugEvent.dwProcessId]);
    exit(False);
  end;

  Result := True;
  if FCurrentContext <> nil then
    exit;

  Result := GetFpThreadContext(_UnAligendContext, FCurrentContext, cfFull);
  DebugLn((DBG_WARNINGS or DBG_VERBOSE) and (not Result), ['Failed to read thread-state for ', ID]);
  //FThreadContextChanged := False; TODO: why was that not here?
  //FThreadContextChangeFlags := [];
  FRegisterValueListValid:=False;
  FHasResetInstructionPointerAfterBreakpoint := False;
  FAtHardCodeBreakpoint := False;
end;

function TDbgWinAarch64Thread.GetInstructionPointerForHasBreakpointInfoForAddress: TDBGPtr;
begin
  Result := GetInstructionPointerRegisterValue;
end;

function TDbgWinAarch64Thread.HasContext: Boolean;
begin
  Result := FCurrentContext <> nil;
end;

destructor TDbgWinAarch64Thread.Destroy;
begin
  inherited Destroy;
  FUnwinder.Free;
end;

function TDbgWinAarch64Thread.GetStackUnwinder: TDbgStackUnwinder;
begin
  if FUnwinder = nil then
    FUnwinder := TDbgAarch64StackUnwinder.Create(Process);
  Result := FUnwinder;
end;

procedure TDbgWinAarch64Thread.SetSingleStep;
begin
  NextIsSingleStep := True;

  if FCurrentContext = nil then
    if not ReadThreadState then
      exit;

  FCurrentContext^.Cpsr := FCurrentContext^.Cpsr or CPSR_SS_BIT;

  FThreadContextChanged:=true;
end;

procedure TDbgWinAarch64Thread.BeforeContinue;
begin
  inherited;
  // TODO: exception

  if FThreadContextChanged then
  begin
    Assert(FCurrentContext <> nil, 'TDbgWinThread.BeforeContinue: none existing context was changed');
    if not SetFpThreadContext(FCurrentContext) then
      debugln(FPDBG_WINDOWS or DBG_WARNINGS, ['Failed to SetFpThreadContext()']);
  end;
  FThreadContextChanged := False;
  //FThreadContextChangeFlags := [];
  FCurrentContext := nil;
end;

procedure TDbgWinAarch64Thread.ApplyWatchPoints(AWatchPointData: TFpWatchPointData);
begin
  //
end;

function TDbgWinAarch64Thread.DetectHardwareWatchpoint: TFpInternalWatchpoint;
begin
  Result := nil;
end;

procedure TDbgWinAarch64Thread.LoadRegisterValues;
var
  i: Integer;
begin
  if FCurrentContext = nil then
    if not ReadThreadState then
      exit;

  with FCurrentContext^ do
    for i := 0 to 30 do
      FRegisterValueList.DbgRegisterAutoCreate['X'+inttostr(i)].SetValue(Regs.X[i], IntToStr(Regs.X[i]), 8, i);

  FRegisterValueList.DbgRegisterAutoCreate['SP'].SetValue(FCurrentContext^.SP, IntToHex(FCurrentContext^.SP, 16), 8, 31);
  FRegisterValueList.DbgRegisterAutoCreate['PC'].SetValue(FCurrentContext^.PC, IntToHex(FCurrentContext^.PC, 16), 8, 32); // no dwarf idx


  FRegisterValueListValid:=true;
end;

procedure TDbgWinAarch64Thread.SetRegisterValue(AName: string; AValue: QWord);
var
  i: integer;
begin
  assert(MDebugEvent.dwProcessId <> 0, 'TDbgWinThread.SetRegisterValue: MDebugEvent.dwProcessId <> 0');

  if not ReadThreadState then
    exit;

  AName := LowerCase(AName);
  if (AName <> '') and (AName[1] = 'x') then begin
    delete(AName, 1, 1);
    i := StrToIntDef(AName, -1);
    if (i >= 0) and (i <= 30) then
      FCurrentContext^.Regs.X[i] := AValue
    else
      raise Exception.CreateFmt('Setting the [%s] register is not supported', [AName]);
  end
  else
  case AName of
    'sp': FCurrentContext^.SP := AValue;
    'pc': FCurrentContext^.PC := AValue;
    else raise Exception.CreateFmt('Setting the [%s] register is not supported', [AName]);
  end;
  FThreadContextChanged:=true;
end;

procedure TDbgWinAarch64Thread.StoreRegisters;
begin
  _UnAligendContext := FStoredContext;
  FThreadContextChanged := True;
  FRegisterValueListValid := False;
end;

procedure TDbgWinAarch64Thread.RestoreRegisters;
begin
  FStoredContext := _UnAligendContext;
end;

function TDbgWinAarch64Thread.ResetInstructionPointerAfterBreakpoint: boolean;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgWinThread.ResetInstructionPointerAfterBreakpoint');{$ENDIF}
  assert(MDebugEvent.dwProcessId <> 0, 'TDbgWinThread.ResetInstructionPointerAfterBreakpoint: MDebugEvent.dwProcessId <> 0');

  Result := ReadThreadState;
  if not Result then exit;

  FCurrentContext^.PC := FCurrentContext^.PC - 4;
  FThreadContextChanged := True;

  FHasResetInstructionPointerAfterBreakpoint := True;
  FLastHardcodedSize := 4;
end;

function TDbgWinAarch64Thread.GetAdjustedInstructionPointerRegisterValue: TDbgPtr;
begin
  Result := GetInstructionPointerRegisterValue;
end;

function TDbgWinAarch64Thread.GetInstructionPointerRegisterValue: TDBGPtr;
begin
  Result := 0;
  if not ReadThreadState then
    exit;
  result := FCurrentContext^.PC;
end;

function TDbgWinAarch64Thread.GetStackBasePointerRegisterValue: TDbgPtr;
begin
  Result := 0;
  if not ReadThreadState then
    exit;
  result := FCurrentContext^.Regs.Fp;
end;

function TDbgWinAarch64Thread.GetStackPointerRegisterValue: TDbgPtr;
begin
  Result := 0;
  if not ReadThreadState then
    exit;
  result := FCurrentContext^.SP;
end;

procedure TDbgWinAarch64Thread.SetInstructionPointerRegisterValue(AValue: TDbgPtr);
begin
  if FCurrentContext = nil then
    exit;
  FCurrentContext^.PC := AValue;
  FThreadContextChanged := True;
end;

procedure TDbgWinAarch64Thread.SetStackPointerRegisterValue(AValue: TDbgPtr);
begin
  if FCurrentContext = nil then
    exit;
  FCurrentContext^.SP := AValue;
  FThreadContextChanged := True;
end;

{ TDbgWinAarch64Process }

function TDbgWinAarch64Process.CreateBreakPointTargetHandler: TFpBreakPointTargetHandler;
begin
  Result := TBreakPointAarch64Handler.Create(Self);
end;

class function TDbgWinAarch64Process.isSupported(ATargetInfo: TTargetDescriptor): boolean;
begin
  Result := (ATargetInfo.OS = osWindows) and
            (ATargetInfo.machineType in [mtARM64]);
end;

function TDbgWinAarch64Process.CallParamDefaultLocation(AParamIdx: Integer): TFpDbgMemLocation;
begin
  Result := InvalidLoc;
  if (AParamIdx >= 0) and (AParamIdx <= 28) then
  begin
    Result.MType := mlfTargetRegister;
    Result.Address := AParamIdx;
  end;
end;

initialization

  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  FPDBG_WINDOWS := DebugLogger.FindOrRegisterLogGroup('FPDBG_WINDOWS' {$IFDEF FPDBG_WINDOWS} , True {$ENDIF} );

  RegisterDbgOsClasses(TOSDbgClasses.Create(
    TDbgWinAarch64Process,
    TDbgWinAarch64Thread,
    TAarch64AsmDecoder
  ));
end.

