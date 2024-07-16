unit FpDbgRiscvClasses;

{$mode objfpc}{$H+}
{$packrecords c}
{$modeswitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  FpDbgClasses,
  DbgIntfBaseTypes, DbgIntfDebuggerBase,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif},
  FpDbgRsp, FpDbgRspClasses, FpDbgCommon, FpdMemoryTools,
  FpErrorMessages;

const
  SPindex            = 2;
  SPindexDwarf       = 2;    // Also known as a1, Index refers to active window
  ReturnAddressIndex = 1;
  ReturnAddressIndexDwarf = 1;
  PCIndexDwarf       = 32;   // Dwarf index
  nPC                = 'pc';
  nReturnPC          = 'ra';
  nSP                = 'sp';

type
  { TDbgRiscvThread }

  TDbgRiscvThread = class(TDbgRspThread)
  private
  const
    lastCPURegIndex = 32; // 32 registers + PC
    // Offsets to load specific registers from register data
    // These are byte offsets, to be used when reading from the raw byte register data
    RegArrayByteLength = 33*4;  // Depends on qemu options, but this seems to be the smallest size to handle.  Only show basic registers, so rest can be ignored for now.
    PCindex = 32;
  protected
    procedure LoadRegisterCache; override;
    procedure SaveRegisterCache; override;
    function GetReturnPC: TDbgPtr;
    function GetStackUnwinder: TDbgStackUnwinder; override;
  public
    destructor Destroy; override;
    procedure LoadRegisterValues; override;
    procedure SetRegisterValue(AName: string; AValue: QWord); override;
    function GetInstructionPointerRegisterValue: TDbgPtr; override;
    function GetStackBasePointerRegisterValue: TDbgPtr; override;
    // procedure SetInstructionPointerRegisterValue(AValue: TDbgPtr); override;
    function GetStackPointerRegisterValue: TDbgPtr; override;
    // procedure SetStackPointerRegisterValue(AValue: TDbgPtr); override;
  end;

  { TDbgRiscvProcess }

  TDbgRiscvProcess = class(TDbgRspProcess)
  private const
    FNumRegisters = 33;  // x0..x31,pc
  protected
    function CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread; override;
    function CreateBreakPointTargetHandler: TFpBreakPointTargetHandler; override;
  public
    class function isSupported(target: TTargetDescriptor): boolean; override;
    constructor Create(const AFileName: string; AnOsClasses: TOSDbgClasses;
                      AMemManager: TFpDbgMemManager; AMemModel: TFpDbgMemModel;
                      AProcessConfig: TDbgProcessConfig = nil); override;
    destructor Destroy; override;
  end;

  TRiscvBreakInfo = object
  const
    _CODE: Word = $9002; // c.ebreak -> this is a 16 bit instruction
  end;

  TRiscvBreakPointTargetHandler = specialize TRspBreakPointTargetHandler<Word, TRiscvBreakInfo>;

implementation

uses
  FpDbgDisasRiscv, FpDbgDwarfDataClasses;

var
  DBG_VERBOSE, DBG_WARNINGS: PLazLoggerLogGroup;

{ TDbgRiscvThread }

procedure TDbgRiscvThread.LoadRegisterCache;
var
  regs: TBytes;
  i: integer;
begin
  if not FRegs.Initialized then
  begin
    SetLength(regs, RegArrayByteLength);
    FRegs.Initialized := TDbgRiscvProcess(Process).RspConnection.ReadRegisters(regs[0], length(regs));
    // 32 bit LE registers
    for i := 0 to lastCPURegIndex do  // x0..x31, PC
      FRegs.regs[i] := regs[4*i] + (regs[4*i + 1] shl 8) + (regs[4*i + 2] shl 16) + (regs[4*i + 3] shl 24);
  end;
end;

procedure TDbgRiscvThread.SaveRegisterCache;
  procedure CopyDWordToByteArray( const val: DWord; regs: PByte);
  begin
    regs[0] := val;
    regs[1] := val shr 8;
    regs[2] := val shr 16;
    regs[3] := val shr 24;
  end;

var
  regs: TBytes;
  i: Integer;
begin
  exit; // TODO: Need to determine which other registers will also change in case of a subroutine call on the debugger side.

  if FRegsChanged then
  begin
    SetLength(regs, RegArrayByteLength);
    for i := 0 to lastCPURegIndex do
      CopyDWordToByteArray(FRegs.regs[i], @regs[4*i]);
  end;
end;

function TDbgRiscvThread.GetReturnPC: TDbgPtr;
begin
  Result := 0;
  if TDbgRiscvProcess(Process).FIsTerminating then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRiscvProcess.GetStackPointerRegisterValue called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  DebugLn(DBG_VERBOSE, 'TDbgRiscvProcess.GetStackPointerRegisterValue requesting stack registers.');
  ReadDebugReg(PCindex, result);
end;

function TDbgRiscvThread.GetStackUnwinder: TDbgStackUnwinder;
begin
  if FUnwinder = nil then
    FUnwinder := TDbgStackUnwinderRiscv.Create(Process);
  Result := FUnwinder;
end;

destructor TDbgRiscvThread.Destroy;
begin
  if Assigned(FUnwinder) then
    FreeAndNil(FUnwinder);
  inherited Destroy;
end;

procedure TDbgRiscvThread.LoadRegisterValues;
var
  i: integer;
begin
  if TDbgRiscvProcess(Process).FIsTerminating or (TDbgRiscvProcess(Process).FStatus = SIGHUP) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRiscvProcess.LoadRegisterValues called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  LoadRegisterCache;

  if FRegs.Initialized then
  begin
    { FRegs.regs starts with 32 core registers, then PC
      Todo: Where does floating point and vector registers fit in? }

    for i := 0 to 32 do
      FRegisterValueList.DbgRegisterAutoCreate[RiscvABIRegisterNames[i]].SetValue(FRegs.regs[i], IntToStr(FRegs.regs[i]), 4, i);

    FRegisterValueListValid := true;
  end
  else
    DebugLn(DBG_WARNINGS, 'Warning: Could not update registers');
end;

procedure TDbgRiscvThread.SetRegisterValue(AName: string; AValue: QWord);
var
  i: integer;
  res: boolean;
begin
  res := false;
  for i := low(RiscvABIRegisterNames) to high(RiscvABIRegisterNames) do
    if AName = RiscvABIRegisterNames[i] then
    begin
      res := TDbgRspProcess(Process).RspConnection.WriteDebugReg(i, byte(AValue));
      break;
    end;

  if not res then
    DebugLn(DBG_WARNINGS, 'Error setting register %s to %u', [AName, AValue]);
end;

function TDbgRiscvThread.GetInstructionPointerRegisterValue: TDbgPtr;
begin
  Result := 0;
  if TDbgRiscvProcess(Process).FIsTerminating then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRiscvProcess.GetInstructionPointerRegisterValue called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  DebugLn(DBG_VERBOSE, 'TDbgRiscvProcess.GetInstructionPointerRegisterValue requesting PC.');
  ReadDebugReg(PCindex, Result);
end;

function TDbgRiscvThread.GetStackBasePointerRegisterValue: TDbgPtr;
begin
  Result := 0;
  if TDbgRiscvProcess(Process).FIsTerminating then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRiscvProcess.GetStackBasePointerRegisterValue called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  DebugLn(DBG_VERBOSE, 'TDbgRiscvProcess.GetStackBasePointerRegisterValue requesting base registers.');
  // Todo: check FPC implementation of stack frame for riscv
  Result := GetStackPointerRegisterValue;
end;

function TDbgRiscvThread.GetStackPointerRegisterValue: TDbgPtr;
begin
  Result := 0;
  if TDbgRiscvProcess(Process).FIsTerminating then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRiscvProcess.GetStackPointerRegisterValue called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  DebugLn(DBG_VERBOSE, 'TDbgRiscvProcess.GetStackPointerRegisterValue requesting stack registers.');
  if not ReadDebugReg(SPindex, result) then
    Result := 0;
end;

{ TDbgRiscvProcess }

function TDbgRiscvProcess.CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread;
begin
  IsMainThread:=False;
  if AthreadIdentifier<>feInvalidHandle then
  begin
    IsMainThread := AthreadIdentifier=ProcessID;
    result := TDbgRiscvThread.Create(Self, AthreadIdentifier, AthreadIdentifier);
  end
  else
    result := nil;
end;

function
  TDbgRiscvProcess.CreateBreakPointTargetHandler: TFpBreakPointTargetHandler;
begin
    Result := TRiscvBreakPointTargetHandler.Create(Self);
end;

constructor TDbgRiscvProcess.Create(const AFileName: string;
  AnOsClasses: TOSDbgClasses; AMemManager: TFpDbgMemManager;
  AMemModel: TFpDbgMemModel; AProcessConfig: TDbgProcessConfig);
begin
  FRegArrayLength := FNumRegisters;
  inherited Create(AFileName, AnOsClasses, AMemManager, AMemModel, AProcessConfig);
end;

destructor TDbgRiscvProcess.Destroy;
begin
  inherited Destroy;
end;

class function TDbgRiscvProcess.isSupported(target: TTargetDescriptor): boolean;
begin
  result := (target.OS = osEmbedded) and  (target.machineType = mtRISCV);
end;

initialization
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  RegisterDbgOsClasses(TOSDbgClasses.Create(
    TDbgRiscvProcess,
    TDbgRiscvThread,
    TRiscvAsmDecoder));
end.
