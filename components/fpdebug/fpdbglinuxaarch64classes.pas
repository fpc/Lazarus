unit FpDbgLinuxAarch64Classes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif},
  LazDebuggerIntfBaseTypes, FpDbgClasses, FpDbgCommon, FpDbgLinuxClasses, FpDbgLinuxExtra,
  FpdMemoryTools, LazClasses, FpDbgCpuAarch64;

type

  TAarch64LinuxGeneralRegisters = record
    Regs: array [0..30] of QWord; // x0 to x30
    SP: QWord;
    PC: QWord;
    State: QWord;
  end;

  { TDbgLinuxAarch64Thread }

  TDbgLinuxAarch64Thread = class(TDbgLinuxThread)
  private
    FUserRegs, FStoredUserRegs: TAarch64LinuxGeneralRegisters;
    FHasThreadState: boolean;
    FUserRegsChanged: boolean;

    FUnwinder: TDbgStackUnwinder;

    function ReadThreadState: boolean;
  protected
    procedure ResetPauseStates; override;
    function GetInstructionPointerForHasBreakpointInfoForAddress: TDBGPtr; override;

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
    function GetInstructionPointerRegisterValue: TDBGPtr; override;
    function GetStackBasePointerRegisterValue: TDbgPtr; override;
    function GetStackPointerRegisterValue: TDbgPtr; override;
    procedure SetInstructionPointerRegisterValue(AValue: TDbgPtr); override;
    procedure SetStackPointerRegisterValue(AValue: TDbgPtr); override;
  end;

  { TDbgLinuxAarch64Process }
  TDbgLinuxAarch64Process = class(TDbgLinuxProcess)
  protected
    function CreateBreakPointTargetHandler: TFpBreakPointTargetHandler; override;
    function DbgThreadClass: TDbgThreadClass; override;
  public
    class function isSupported(ATargetInfo: TTargetDescriptor): boolean; override;
    function CallParamDefaultLocation(AParamIdx: Integer): TFpDbgMemLocation; override;
  end;

implementation
var
  DBG_VERBOSE, DBG_WARNINGS, FPDBG_LINUX: PLazLoggerLogGroup;

const
  NT_PRSTATUS = 1;

{ TDbgLinuxAarch64Thread }

function TDbgLinuxAarch64Thread.ReadThreadState: boolean;
var
  io: iovec;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxAarch64Thread.ReadThreadState');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxAarch64Thread.ReadThreadState: FIsPaused');

  result := true;
  if FHasThreadState then
    exit;
//  FFpRegsAvail:=False;
  io.iov_base:=@(FUserRegs.Regs[0]);
  io.iov_len:= sizeof(FUserRegs);
  if fpPTrace(PTRACE_GETREGSET, ID, pointer(PtrUInt(NT_PRSTATUS)), @io) <> 0 then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to read thread registers from threadid '+inttostr(ID)+'. Errcode: '+inttostr(fpgeterrno));
    result := false;
    end;
  FHasThreadState := Result;
end;

procedure TDbgLinuxAarch64Thread.ResetPauseStates;
begin
  inherited ResetPauseStates;
  FHasThreadState := false;
end;

function TDbgLinuxAarch64Thread.GetInstructionPointerForHasBreakpointInfoForAddress: TDBGPtr;
begin
  Result := GetInstructionPointerRegisterValue;
end;

function TDbgLinuxAarch64Thread.GetStackUnwinder: TDbgStackUnwinder;
begin
  if FUnwinder = nil then
    FUnwinder := TDbgAarch64StackUnwinder.Create(Process);
  Result := FUnwinder;
end;

destructor TDbgLinuxAarch64Thread.Destroy;
begin
  inherited Destroy;
  FUnwinder.Free;
end;

procedure TDbgLinuxAarch64Thread.ApplyWatchPoints(AWatchPointData: TFpWatchPointData);
begin
  //
end;

function TDbgLinuxAarch64Thread.DetectHardwareWatchpoint: TFpInternalWatchpoint;
begin
  Result := nil;
end;

procedure TDbgLinuxAarch64Thread.LoadRegisterValues;
var
  i: integer;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxAarch64Thread.LoadRegisterValues');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxAarch64Thread.LoadRegisterValues: FIsPaused');

  if not ReadThreadState then
    exit;

  for i := 0 to 30 do
    FRegisterValueList.DbgRegisterAutoCreate['r'+inttostr(i)].SetValue(FUserRegs.Regs[i], IntToStr(FUserRegs.Regs[i]), 8, i);

  FRegisterValueList.DbgRegisterAutoCreate['SP'].SetValue(FUserRegs.SP, IntToHex(FUserRegs.SP, 16), 8, 31);
  FRegisterValueList.DbgRegisterAutoCreate['PC'].SetValue(FUserRegs.PC, IntToHex(FUserRegs.PC, 16), 8, 32); // no dwarf idx

  FRegisterValueListValid:=true;
end;

procedure TDbgLinuxAarch64Thread.SetRegisterValue(AName: string; AValue: QWord);
var
  i: integer;
begin
  AName := LowerCase(AName);
  if (AName <> '') and (AName[1] = 'x') then begin
    delete(AName, 1, 1);
    i := StrToIntDef(AName, -1);
    if (i >= 0) and (i <= 30) then
      FUserRegs.Regs[i] := AValue
    else
      raise Exception.CreateFmt('Setting the [%s] register is not supported', [AName]);
  end
  else
  case AName of
    'sp': FUserRegs.SP := AValue;
    'pc': FUserRegs.PC := AValue;
    else raise Exception.CreateFmt('Setting the [%s] register is not supported', [AName]);
  end;
  FUserRegsChanged:=true;
end;

procedure TDbgLinuxAarch64Thread.StoreRegisters;
begin
  Assert(FHasThreadState);
  FStoredUserRegs := FUserRegs;
end;

procedure TDbgLinuxAarch64Thread.RestoreRegisters;
begin
  FUserRegs:=FStoredUserRegs;
  FUserRegsChanged := true;
  FRegisterValueListValid := False;
end;

procedure TDbgLinuxAarch64Thread.BeforeContinue;
var
  io: iovec;
begin
  if not IsPaused then
    exit;

  inherited;
  //if Process.CurrentWatchpoint <> nil then
  //  WriteDebugReg(6, 0);

  if FUserRegsChanged then
    begin
    io.iov_base:=@(FUserRegs.regs[0]);
    io.iov_len:= sizeof(FUserRegs);

    if fpPTrace(PTRACE_SETREGSET, ID, pointer(PtrUInt(NT_PRSTATUS)), @io) <> 0 then
      begin
      DebugLn(DBG_WARNINGS, 'Failed to set thread registers. Errcode: '+inttostr(fpgeterrno));
      end;
    FUserRegsChanged:=false;
    end;
end;

function TDbgLinuxAarch64Thread.ResetInstructionPointerAfterBreakpoint: boolean;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxAarch64Thread.ResetInstructionPointerAfterBreakpoint');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxAarch64Thread.ResetInstructionPointerAfterBreakpoint: FIsPaused');

  if not ReadThreadState then
    exit(False);
  result := true;
end;

function TDbgLinuxAarch64Thread.GetInstructionPointerRegisterValue: TDBGPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxAarch64Thread.GetInstructionPointerRegisterValue');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxAarch64Thread.GetInstructionPointerRegisterValue: FIsPaused');

  Result := 0;
  if not ReadThreadState then
    exit;
  result := FUserRegs.PC;
end;

function TDbgLinuxAarch64Thread.GetStackBasePointerRegisterValue: TDbgPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxAarch64Thread.GetInstructionPointerRegisterValue');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxAarch64Thread.GetInstructionPointerRegisterValue: FIsPaused');

  Result := 0;
  if not ReadThreadState then
    exit;
  result := FUserRegs.Regs[29];
end;

function TDbgLinuxAarch64Thread.GetStackPointerRegisterValue: TDbgPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxAarch64Thread.GetInstructionPointerRegisterValue');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxAarch64Thread.GetInstructionPointerRegisterValue: FIsPaused');

  Result := 0;
  if not ReadThreadState then
    exit;
  result := FUserRegs.SP;
end;

procedure TDbgLinuxAarch64Thread.SetInstructionPointerRegisterValue(AValue: TDbgPtr);
begin
  if not FHasThreadState then
    exit;
  FUserRegs.PC := AValue
end;

procedure TDbgLinuxAarch64Thread.SetStackPointerRegisterValue(AValue: TDbgPtr);
begin
  if not FHasThreadState then
    exit;
  FUserRegs.SP := AValue
end;


{ TDbgLinuxAarch64Process }

function TDbgLinuxAarch64Process.CreateBreakPointTargetHandler: TFpBreakPointTargetHandler;
begin
    Result := TBreakPointAarch64Handler.Create(Self);
end;

function TDbgLinuxAarch64Process.DbgThreadClass: TDbgThreadClass;
begin
  Result := TDbgLinuxAarch64Thread;
end;

class function TDbgLinuxAarch64Process.isSupported(ATargetInfo: TTargetDescriptor): boolean;
begin
  result := (ATargetInfo.OS = osLinux) and
            (ATargetInfo.machineType in [mtARM64]);
end;

function TDbgLinuxAarch64Process.CallParamDefaultLocation(AParamIdx: Integer): TFpDbgMemLocation;
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
  FPDBG_LINUX := DebugLogger.FindOrRegisterLogGroup('FPDBG_LINUX' {$IFDEF DebuglnLinuxDebugEvents} , True {$ENDIF} );

  RegisterDbgOsClasses(TOSDbgClasses.Create(
    TDbgLinuxAarch64Process,
    TDbgLinuxAarch64Thread,
    TAarch64AsmDecoder
  ));

end.
