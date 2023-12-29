unit FpDbgAvrClasses;

{ This unit supports AVR specific code for fpdebug.
  It communicates with a remote target via remote serial protocol }

{$mode objfpc}{$H+}
{$packrecords c}
{$modeswitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  FpDbgClasses,
  FpDbgLoader,
  DbgIntfBaseTypes, DbgIntfDebuggerBase,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif}, Maps,
  FpDbgRsp, FpDbgRspClasses, FpDbgCommon, FpdMemoryTools,
  FpErrorMessages;

const
  // Use as dwarf register indexes
  SREGindex = 32;  // 1 byte
  SPindex = 33;    // 2 bytes
  PCindex = 34;    // 4 bytes
  // Special register names
  nSP = 'SP';
  nPC = 'PC';
  nSREG = 'SReg';

type
  { TAvrMemManager }

  TAvrMemManager = class(TFpDbgMemManager)
    function ReadRegisterAsAddress(ARegNum: Cardinal; out AValue: TDbgPtr; AContext: TFpDbgLocationContext): Boolean; override;
  end;

  { TDbgAvrThread }

  TDbgAvrThread = class(TDbgRspThread)
  private
  const
    lastCPURegIndex = 31; // After this are SREG, SP and PC
    nSREGF = 'SRegFlags';

    // Byte level register indexes
    SPLindex = 33;
    SPHindex = 34;
    PC0 = 35;
    PC1 = 36;
    PC2 = 37;
    PC3 = 38;
    RegArrayByteLength = 39;
  protected
    procedure LoadRegisterCache; override;
    procedure SaveRegisterCache; override;
    function FormatStatusFlags(sreg: byte): string;
    function GetStackUnwinder: TDbgStackUnwinder; override;
  public
    procedure LoadRegisterValues; override;
    procedure SetRegisterValue(AName: string; AValue: QWord); override;
    function GetInstructionPointerRegisterValue: TDbgPtr; override;
    function GetStackBasePointerRegisterValue: TDbgPtr; override;
    procedure SetStackPointerRegisterValue(AValue: TDbgPtr); override;
    function GetStackPointerRegisterValue: TDbgPtr; override;
  end;

  { TDbgAvrProcess }

  TDbgAvrProcess = class(TDbgRspProcess)
  private const
    FNumRegisters = 35;  // r0..r31, SREG, SP, PC
  protected
    function CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread; override;
  public
    class function isSupported(target: TTargetDescriptor): boolean; override;
    constructor Create(const AFileName: string; AnOsClasses: TOSDbgClasses;
      AMemManager: TFpDbgMemManager; AProcessConfig: TDbgProcessConfig); override;
    destructor Destroy; override;
  end;

implementation

uses
  FpDbgDisasAvr, FpDbgDwarfDataClasses, FpDbgInfo;

var
  DBG_VERBOSE, DBG_WARNINGS: PLazLoggerLogGroup;

{ TAvrMemManager }

function TAvrMemManager.ReadRegisterAsAddress(ARegNum: Cardinal; out
  AValue: TDbgPtr; AContext: TFpDbgLocationContext): Boolean;
const
  AvrDataOffset = $800000;
var
  tmpVal: TDBGPtr;
begin
  Result := ReadRegister(ARegNum, AValue, AContext);
  { Assume the pointer points to data space.
    This will be the case for stack based parameters.
    But if parameters are stored in registers this could lead to confusion.
    E.g. when passing a procedure pointer the pointer points to code space.
    Todo: consider adding DW_AT_address_class or DW_AT_segment information on compiler side.
    There will be more potential for confusion when section support is added to the compiler }
  if Result then
  begin
    Result := ReadRegister(ARegNum+1, tmpVal, AContext);
    if Result then
      AValue := AvrDataOffset or (AValue + (word(tmpVal) shl 8));
  end;
end;

{ TDbgAvrThread }

procedure TDbgAvrThread.LoadRegisterCache;
var
  regs: TBytes;
  i: integer;
begin
  if not FRegs.Initialized then
  begin
    SetLength(regs, RegArrayByteLength);
    FRegs.Initialized := TDbgRspProcess(Process).RspConnection.ReadRegisters(regs[0], length(regs));
    for i := 0 to lastCPURegIndex do
      FRegs.regs[i] := regs[i];

    FRegs.regs[SREGindex] := regs[SREGindex];
    // repack according to target endianness
    FRegs.regs[SPindex] := regs[SPLindex] + (regs[SPHindex] shl 8);
    FRegs.regs[PCindex] := regs[PC0] + (regs[PC1] shl 8) + (regs[PC2] shl 16) + (regs[PC3] shl 24);
  end;
end;

procedure TDbgAvrThread.SaveRegisterCache;
var
  regs: TBytes;
  i: integer;
begin
  if FRegsChanged then
  begin
    SetLength(regs, RegArrayByteLength);
    for i := 0 to lastCPURegIndex do
      regs[i] := FRegs.regs[i];

    // SREG
    regs[SREGindex] := FRegs.regs[SREGindex];
    // SP
    regs[SPLindex] := byte(FRegs.regs[SPindex]);
    regs[SPHindex] := byte(FRegs.regs[SPindex] shr 8);
    // PC
    regs[PC0] := byte(FRegs.regs[PCindex]);
    regs[PC1] := byte(FRegs.regs[PCindex] shr 8);
    regs[PC2] := byte(FRegs.regs[PCindex] shr 16);
    regs[PC3] := byte(FRegs.regs[PCindex] shr 24);

    if not TDbgRspProcess(Process).RspConnection.WriteRegisters(regs[0], Length(regs)) then
      DebugLn(DBG_WARNINGS, 'Failed to set thread registers.');
   FRegsChanged := false;
  end;
end;

function TDbgAvrThread.FormatStatusFlags(sreg: byte): string;
const
  SREG_FLAGS = 'ITHSVNZC';
var
  i: integer;
  flag: char;
begin
  Result := '               ';
  for i := 0 to 7 do
  begin
    if sreg and $80 = $80 then
      flag := SREG_FLAGS[i+1]
    else
      flag := '.';

    Result[2*i+1] := flag;
    sreg := byte(sreg shl 1);
  end;
end;

function TDbgAvrThread.GetStackUnwinder: TDbgStackUnwinder;
begin
  if FUnwinder = nil then
    FUnwinder := TDbgStackUnwinderAVR.Create(Process);
  Result := FUnwinder;
end;

procedure TDbgAvrThread.LoadRegisterValues;
var
  i: integer;
begin
  if TDbgRspProcess(Process).IsTerminating or (TDbgRspProcess(Process).Status = SIGHUP) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspThread.LoadRegisterValues called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  LoadRegisterCache;

  if FRegs.Initialized then
  begin
    for i := 0 to lastCPURegIndex do
      FRegisterValueList.DbgRegisterAutoCreate['r'+IntToStr(i)].SetValue(FRegs.regs[i], IntToStr(FRegs.regs[i]),1, i); // confirm dwarf index

    FRegisterValueList.DbgRegisterAutoCreate[nSREG].SetValue(FRegs.regs[SREGindex], IntToStr(FRegs.regs[SREGindex]),1,SREGindex);
    FRegisterValueList.DbgRegisterAutoCreate[nSREGF].SetValue(FRegs.regs[SREGindex], FormatStatusFlags(FRegs.regs[SREGindex]),1,0);
    FRegisterValueList.DbgRegisterAutoCreate[nSP].SetValue(FRegs.regs[SPindex], IntToStr(FRegs.regs[SPindex]),2,SPindex);
    FRegisterValueList.DbgRegisterAutoCreate[nPC].SetValue(FRegs.regs[PCindex], IntToStr(FRegs.regs[PCindex]),4,PCindex);
    FRegisterValueListValid := true;
  end
  else
    DebugLn(DBG_WARNINGS, 'Warning: Could not update registers');
end;

procedure TDbgAvrThread.SetRegisterValue(AName: string; AValue: QWord);
var
  i, err: integer;
  res: boolean;
begin
  if AName[1] = 'r' then
  begin
    val(copy(AName, 2, length(Aname)), i, err);
    res := (err = 0) and (i <= 31);
    if res then
      res := TDbgRspProcess(Process).RspConnection.WriteDebugReg(i, byte(AValue));
  end
  else if AName = nSREG then
    res := TDbgRspProcess(Process).RspConnection.WriteDebugReg(SREGindex, byte(AValue))
  else if AName = nSP then
    res := TDbgRspProcess(Process).RspConnection.WriteDebugReg(SPindex, word(AValue))
  else if AName = nPC then
    res := TDbgRspProcess(Process).RspConnection.WriteDebugReg(PCindex, dword(AValue));

  if not res then
    DebugLn(DBG_WARNINGS, 'Error setting register %s to %u', [AName, AValue]);
end;

function TDbgAvrThread.GetInstructionPointerRegisterValue: TDbgPtr;
begin
  Result := 0;
  if TDbgRspProcess(Process).IsTerminating then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspThread.GetInstructionPointerRegisterValue called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  DebugLn(DBG_VERBOSE, 'TDbgRspThread.GetInstructionPointerRegisterValue requesting PC.');
  ReadDebugReg(PCindex, result);
end;

function TDbgAvrThread.GetStackBasePointerRegisterValue: TDbgPtr;
var
  lval, hval: QWord;
begin
  Result := 0;
  if TDbgRspProcess(Process).IsTerminating then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgAvrThread.GetStackBasePointerRegisterValue called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  DebugLn(DBG_VERBOSE, 'TDbgAvrThread.GetStackBasePointerRegisterValue requesting base registers.');
  // Y-pointer (r28..r29)
  ReadDebugReg(28, lval);
  ReadDebugReg(29, hval);
  result := byte(lval) + (byte(hval) shl 8);
end;

procedure TDbgAvrThread.SetStackPointerRegisterValue(AValue: TDbgPtr);
begin
end;

function TDbgAvrThread.GetStackPointerRegisterValue: TDbgPtr;
begin
  Result := 0;
  if TDbgRspProcess(Process).IsTerminating then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspThread.GetStackPointerRegisterValue called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  DebugLn(DBG_VERBOSE, 'TDbgRspThread.GetStackPointerRegisterValue requesting stack registers.');
  ReadDebugReg(SPindex, result);
end;

{ TDbgAvrProcess }

function TDbgAvrProcess.CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread;
begin
  IsMainThread:=False;
  if AthreadIdentifier<>feInvalidHandle then
  begin
    IsMainThread := AthreadIdentifier=ProcessID;
    result := TDbgAvrThread.Create(Self, AthreadIdentifier, AthreadIdentifier)
  end
  else
    result := nil;
end;

constructor TDbgAvrProcess.Create(const AFileName: string; AnOsClasses: TOSDbgClasses;
  AMemManager: TFpDbgMemManager; AProcessConfig: TDbgProcessConfig);
begin
  FRegArrayLength := FNumRegisters;
  inherited Create(AFileName, AnOsClasses, AMemManager, AProcessConfig);
end;

destructor TDbgAvrProcess.Destroy;
begin
  inherited Destroy;
end;

class function TDbgAvrProcess.isSupported(target: TTargetDescriptor): boolean;
begin
  result := (target.OS = osEmbedded) and
            (target.machineType = mtAVR8);
end;

initialization
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  RegisterDbgOsClasses(TOSDbgClasses.Create(
    TDbgAvrProcess,
    TDbgAvrThread,
    TAvrAsmDecoder));
end.
