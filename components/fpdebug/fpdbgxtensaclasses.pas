unit FpDbgXtensaClasses;

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
  SPindexDwarf       = 1;    // Also known as a1, Index refers to active window
  PCIndexDwarf       = 100;  // Dwarf index (assumed)
  nPC                = 'PC';
  nReturnPC          = 'a0';
  nSP                = 'a1';
  ReturnPCIndexDwarf = 0;    // Also known as a0, Index refers to active window
  WindowBaseIndex    = 65;
  WindowStartIndex   = 66;
  PSIndex            = 67;
  nWindowBase        = 'windowbase';
  nWindowStart       = 'windowstart';
  nPS                = 'ps';

type
  { TDbgXtensaThread }

  TDbgXtensaThread = class(TDbgRspThread)
  private
  const
    lastCPURegIndex    = 64; // PC + 64 registers
    // Offsets to load specific registers from register data
    // These are byte offsets, to be used when reading from the raw byte register data
    WindowBaseOffset   = 276;  // Dependent on Windowed option...
    WindowStartOffset  = 280;  // "
    PSOffset           = 292;  // "
    RegArrayByteLength = 420;  // Depends on qemu options, but this seems to be the smallest size to handle.  Only show basic registers, so rest can be ignored for now.
    // Ordered registers index
    // PC, ar0..ar63, wb, ws, ps
    PCindex            = 0;    // Offset into raw register array

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

  { TDbgXtensaProcess }

  TDbgXtensaProcess = class(TDbgRspProcess)
  private const
    FNumRegisters = 68;  // pc, ar0..ar63, WindowBase, WindowStart, PS
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

  TXtensaBreakInfo = object
  const
    _CODE: DWord = $F06D; // ILL.N -> this is a 16 bit narrow instruction
  end;

  TXtensaBreakPointTargetHandler = specialize TRspBreakPointTargetHandler<Word, TXtensaBreakInfo>;


  { TDbgStackUnwinderXtensa }

  TDbgStackUnwinderXtensa = class(TDbgStackUnwinder)
  private
    FThread: TDbgThread;
    FProcess: TDbgProcess;
    FAddressSize: Integer;
    FLastFrameBaseIncreased: Boolean;
    FCodeReadErrCnt: integer;

    FWindowBase: uint32;
    FWindowStart: uint32;
  protected
    property Process: TDbgProcess read FProcess;
    property Thread: TDbgThread read FThread;
    property AddressSize: Integer read FAddressSize;
  public
    constructor Create(AProcess: TDbgProcess);
    procedure InitForThread(AThread: TDbgThread); override;
    procedure InitForFrame(ACurrentFrame: TDbgCallstackEntry; out CodePointer,
      StackPointer, FrameBasePointer: TDBGPtr); override;
    procedure GetTopFrame(out CodePointer, StackPointer, FrameBasePointer: TDBGPtr;
      out ANewFrame: TDbgCallstackEntry); override;
    function Unwind(AFrameIndex: integer; var CodePointer, StackPointer,
      FrameBasePointer: TDBGPtr; ACurrentFrame: TDbgCallstackEntry; out
      ANewFrame: TDbgCallstackEntry): TTDbgStackUnwindResult; override;
  end;


implementation

uses
  FpDbgDisasXtensa, FpDbgDwarfDataClasses;

var
  DBG_VERBOSE, DBG_WARNINGS: PLazLoggerLogGroup;

{ TDbgXtensaThread }

procedure TDbgXtensaThread.LoadRegisterCache;
var
  regs: TBytes;
  i: integer;
begin
  if not FRegs.Initialized then
  begin
    SetLength(regs, RegArrayByteLength);
    FRegs.Initialized := TDbgXtensaProcess(Process).RspConnection.ReadRegisters(regs[0], length(regs));
    // 32 bit LE registers
    for i := 0 to lastCPURegIndex do  // PC, ar0..ar63
      FRegs.regs[i] := regs[4*i] + (regs[4*i + 1] shl 8) + (regs[4*i + 2] shl 16) + (regs[4*i + 3] shl 24);

    i := WindowBaseOffset;
    FRegs.regs[WindowBaseIndex]  := regs[i] + (regs[i + 1] shl 8) + (regs[i + 2] shl 16) + (regs[i + 3] shl 24);
    i := WindowStartOffset;
    FRegs.regs[WindowStartIndex] := regs[i] + (regs[i + 1] shl 8) + (regs[i + 2] shl 16) + (regs[i + 3] shl 24);
    i := PSOffset;
    FRegs.regs[PSIndex]          := regs[i] + (regs[i + 1] shl 8) + (regs[i + 2] shl 16) + (regs[i + 3] shl 24);
  end;
end;

procedure TDbgXtensaThread.SaveRegisterCache;
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

    CopyDWordToByteArray(FRegs.regs[WindowBaseIndex], @regs[WindowBaseOffset]);
    CopyDWordToByteArray(FRegs.regs[WindowStartIndex], @regs[WindowStartOffset]);
    CopyDWordToByteArray(FRegs.regs[PSIndex], @regs[PSOffset]);
  end;
end;

function TDbgXtensaThread.GetReturnPC: TDbgPtr;
var
  wb: TDBGPtr;
begin
  Result := 0;
  if TDbgXtensaProcess(Process).FIsTerminating then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgXtensaProcess.GetStackPointerRegisterValue called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  DebugLn(DBG_VERBOSE, 'TDbgXtensaProcess.GetStackPointerRegisterValue requesting stack registers.');
  // Windowed ABI: retPC in a0
  ReadDebugReg(WindowBaseIndex, wb);
  wb := wb * 4;
  ReadDebugReg(wb+1, result);
end;

function TDbgXtensaThread.GetStackUnwinder: TDbgStackUnwinder;
begin
  if FUnwinder = nil then
    FUnwinder := TDbgStackUnwinderXtensa.Create(Process);
  Result := FUnwinder;
end;

destructor TDbgXtensaThread.Destroy;
begin
  if Assigned(FUnwinder) then
    FreeAndNil(FUnwinder);
  inherited Destroy;
end;

procedure TDbgXtensaThread.LoadRegisterValues;
var
  i, j, wb: integer;
begin
  if TDbgXtensaProcess(Process).FIsTerminating or (TDbgXtensaProcess(Process).FStatus = SIGHUP) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgXtensaProcess.LoadRegisterValues called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  LoadRegisterCache;

  if FRegs.Initialized then
  begin
    { FRegs.regs start with PC, then the 64 core registers,
      then a bunch of other system and optionally user registers.
      Only load currently visible registers, PC and WindowBaseOffset for now.
      The start of currently visible core registers is given by WindowBaseOffset }

    // WindowBaseOffset is a 4 bit value
    wb := 4 * (FRegs.regs[WindowBaseIndex] and $0F);
    // Returned registers start with PC, followed by core register file, so offset index by 1
    for i := 0 to 15 do
    begin
      // Index should wrap around to start of register file
      j := ((wb+i) and 63) + 1;
      FRegisterValueList.DbgRegisterAutoCreate['a'+IntToStr(i)].SetValue(FRegs.regs[j], IntToStr(FRegs.regs[j]), 4, i);
    end;

    FRegisterValueList.DbgRegisterAutoCreate[nPC].SetValue(FRegs.regs[0], IntToStr(FRegs.regs[0]), 4, 0);
    FRegisterValueList.DbgRegisterAutoCreate[nWindowBase].SetValue(byte(FRegs.regs[WindowBaseIndex]), IntToStr(byte(FRegs.regs[WindowBaseIndex])),1 , WindowBaseIndex);
    FRegisterValueList.DbgRegisterAutoCreate[nWindowStart].SetValue(word(FRegs.regs[WindowStartIndex]), IntToStr(word(FRegs.regs[WindowStartIndex])),2 , WindowStartIndex);
    FRegisterValueList.DbgRegisterAutoCreate[nPS].SetValue(byte(FRegs.regs[PSIndex]), IntToStr(byte(FRegs.regs[PSIndex])),1 , 0);
    FRegisterValueList.DbgRegisterAutoCreate[nPS].SetValue(byte(FRegs.regs[PSIndex]), IntToStr(byte(FRegs.regs[PSIndex])),1 , 0);
    FRegisterValueListValid := true;
  end
  else
    DebugLn(DBG_WARNINGS, 'Warning: Could not update registers');
end;

procedure TDbgXtensaThread.SetRegisterValue(AName: string; AValue: QWord);
var
  i, err: integer;
  res: boolean;
begin
  if AName[1] = 'a' then
  begin
    val(copy(AName, 2, length(Aname)), i, err);
    res := (err = 0) and (i <= 15);
    if res then
      res := TDbgRspProcess(Process).RspConnection.WriteDebugReg(i, byte(AValue));
  end
  else if AName = nPC then
    res := TDbgRspProcess(Process).RspConnection.WriteDebugReg(PCindex, dword(AValue))
  else
    res := False;

  if not res then
    DebugLn(DBG_WARNINGS, 'Error setting register %s to %u', [AName, AValue]);
end;

function TDbgXtensaThread.GetInstructionPointerRegisterValue: TDbgPtr;
begin
  Result := 0;
  if TDbgXtensaProcess(Process).FIsTerminating then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgXtensaProcess.GetInstructionPointerRegisterValue called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  DebugLn(DBG_VERBOSE, 'TDbgXtensaProcess.GetInstructionPointerRegisterValue requesting PC.');
  ReadDebugReg(PCindex, Result);
end;

function TDbgXtensaThread.GetStackBasePointerRegisterValue: TDbgPtr;
begin
  Result := 0;
  if TDbgXtensaProcess(Process).FIsTerminating then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgXtensaProcess.GetStackBasePointerRegisterValue called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  DebugLn(DBG_VERBOSE, 'TDbgXtensaProcess.GetStackBasePointerRegisterValue requesting base registers.');
  // Todo: check FPC implementation of stack frame for ESP32
  Result := GetStackPointerRegisterValue;
end;

function TDbgXtensaThread.GetStackPointerRegisterValue: TDbgPtr;
var
  wb: TDBGPtr;
begin
  Result := 0;
  if TDbgXtensaProcess(Process).FIsTerminating then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgXtensaProcess.GetStackPointerRegisterValue called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  DebugLn(DBG_VERBOSE, 'TDbgXtensaProcess.GetStackPointerRegisterValue requesting stack registers.');
  // Windowed ABI: SP in a1
  if ReadDebugReg(WindowBaseIndex, wb) then
  begin
    wb := wb * 4;
    ReadDebugReg(wb+2, result);
  end
  else
    Result := 0;
end;

{ TDbgXtensaProcess }

function TDbgXtensaProcess.CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread;
begin
  IsMainThread:=False;
  if AthreadIdentifier<>feInvalidHandle then
  begin
    IsMainThread := AthreadIdentifier=ProcessID;
    result := TDbgXtensaThread.Create(Self, AthreadIdentifier, AthreadIdentifier);
  end
  else
    result := nil;
end;

function
  TDbgXtensaProcess.CreateBreakPointTargetHandler: TFpBreakPointTargetHandler;
begin
    Result := TXtensaBreakPointTargetHandler.Create(Self);
end;

constructor TDbgXtensaProcess.Create(const AFileName: string;
  AnOsClasses: TOSDbgClasses; AMemManager: TFpDbgMemManager;
  AMemModel: TFpDbgMemModel; AProcessConfig: TDbgProcessConfig);
begin
  FRegArrayLength := FNumRegisters;
  inherited Create(AFileName, AnOsClasses, AMemManager, AMemModel, AProcessConfig);
end;

destructor TDbgXtensaProcess.Destroy;
begin
  inherited Destroy;
end;

class function TDbgXtensaProcess.isSupported(target: TTargetDescriptor): boolean;
begin
  result := (target.OS = osEmbedded) and  (target.machineType = mtXTENSA);
end;

{ TDbgStackUnwinderXtensa }

constructor TDbgStackUnwinderXtensa.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
  FAddressSize := 4;
  FCodeReadErrCnt := 0;
end;

procedure TDbgStackUnwinderXtensa.InitForThread(AThread: TDbgThread);
begin
  FThread := AThread;
end;

procedure TDbgStackUnwinderXtensa.InitForFrame(
  ACurrentFrame: TDbgCallstackEntry; out CodePointer, StackPointer,
  FrameBasePointer: TDBGPtr);
var
  R: TDbgRegisterValue;
begin
  CodePointer := ACurrentFrame.AnAddress;

  // Frame pointer is a7 (if used)
  FrameBasePointer := ACurrentFrame.FrameAdress;

  StackPointer := 0;
  R := ACurrentFrame.RegisterValueList.FindRegisterByDwarfIndex(SPindexDwarf);
  if R = nil then exit;
  StackPointer := R.NumValue;
end;

procedure TDbgStackUnwinderXtensa.GetTopFrame(out CodePointer, StackPointer,
  FrameBasePointer: TDBGPtr; out ANewFrame: TDbgCallstackEntry);
var
  i: Integer;
  R: TDbgRegisterValue;
begin
  FLastFrameBaseIncreased := True;
  CodePointer      := Thread.GetInstructionPointerRegisterValue;
  StackPointer     := Thread.GetStackPointerRegisterValue;
  FrameBasePointer := Thread.GetStackBasePointerRegisterValue;
  ANewFrame        := TDbgCallstackEntry.create(Thread, 0, FrameBasePointer, CodePointer);

  // Frame pointer may not have been updated yet
  if FrameBasePointer > StackPointer then
    FrameBasePointer := StackPointer;

  i := Thread.RegisterValueList.Count;
  while i > 0 do begin
    dec(i);
    R := Thread.RegisterValueList[i];
    ANewFrame.RegisterValueList.DbgRegisterAutoCreate[R.Name].SetValue(R.NumValue, R.StrValue, R.Size, R.DwarfIdx);
  end;

  FWindowBase := ANewFrame.RegisterValueList.FindRegisterByDwarfIndex(WindowBaseIndex).NumValue and $F;
  FWindowStart := ANewFrame.RegisterValueList.FindRegisterByDwarfIndex(WindowStartIndex).NumValue and $FFFF;
end;

function TDbgStackUnwinderXtensa.Unwind(AFrameIndex: integer; var CodePointer,
  StackPointer, FrameBasePointer: TDBGPtr; ACurrentFrame: TDbgCallstackEntry;
  out ANewFrame: TDbgCallstackEntry): TTDbgStackUnwindResult;
const
  MAX_FRAMES = 500; // safety net
  Size = 4;
var
  Address, returnAddress: TDBGPtr;
  callSize: byte;
  j, k, stackRegCount: integer;
  spilled: boolean;
  stackRegs: array of TDBGPtr;
  tmpReg: uint32;
  LastFrameBase: TDBGPtr;
begin
  ANewFrame := nil;
  Result := suFailed;

  if (StackPointer > $40000000) or (StackPointer < $3F000000) or
     (CodePointer < $40000001) or not FLastFrameBaseIncreased then
    exit;

  LastFrameBase := FrameBasePointer;

  ReturnAddress := ACurrentFrame.RegisterValueList.FindRegisterByName(nReturnPC).NumValue;
  spilled := FWindowStart and (1 shl FWindowBase) = 0;

  FCodeReadErrCnt := 0;

  callSize := uint32(returnAddress) shr 30;
  if callsize = 0 then
    exit;

  Address := (returnAddress and $3FFFFFFF) or $40000000;
  // Check if start of this window frame is live or spilled
  if not spilled then
  begin
    // Move window base to previous window frame
    // Wraparound subtraction
    FWindowBase := (FWindowBase + 16 - callsize) and $F;
    spilled := FWindowStart and (1 shl FWindowBase) = 0;
  end;

  stackRegCount := 4*callSize;
  SetLength(stackRegs, stackRegCount);
  if not spilled then
  begin
    // Live registers, read from register file
    j := ((4*FWindowBase) and 63);
    for k := 0 to stackRegCount-1 do
      stackRegs[k] := TDbgRspThread(Thread).InternalRegs.regs[((j+k) and 63)+1];  // wraparound indexing
  end
  else
  begin
    // Registers spilled to stack, read from memory
    for j := 0 to callSize-1 do
    begin
      for k := 0 to 3 do
      begin
        if j = 0 then
        begin
          if not Process.ReadData(StackPointer-16+4*k, 4, tmpReg) then Break;
        end
        else
        begin
          if (stackRegs[1] < $3F000000) or
             not Process.ReadData(stackRegs[1]-16+4*k, 4, tmpReg) then
            Break;
        end;
        stackRegs[4*j + k] := NtoLE(tmpReg);
      end;
    end;
  end;

  returnAddress := stackRegs[0];
  StackPointer := stackRegs[1];
  FrameBasePointer := StackPointer;

  ANewFrame:= TDbgCallstackEntry.create(Thread, AFrameIndex, FrameBasePointer, Address);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate[nPC].SetValue(Address, IntToStr(Address),Size, PCIndexDwarf);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate[nReturnPC].SetValue(returnAddress, IntToStr(returnAddress),Size, ReturnPCIndexDwarf);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate[nSP].SetValue(StackPointer, IntToStr(StackPointer),Size, SPindexDwarf);
  // In case a7 is used as frame pointer
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate['a7'].SetValue(byte(FrameBasePointer), IntToStr(byte(FrameBasePointer)),Size, 7);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate[nWindowBase].SetValue(FWindowBase, IntToStr(FWindowBase), 1, WindowBaseIndex);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate[nWindowStart].SetValue(FWindowStart, IntToStr(FWindowStart), 2 , WindowStartIndex);

  FLastFrameBaseIncreased := (FrameBasePointer <> 0) and (FrameBasePointer > LastFrameBase);
  Result := suSuccess;
end;

initialization
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  RegisterDbgOsClasses(TOSDbgClasses.Create(
    TDbgXtensaProcess,
    TDbgXtensaThread,
    TXtensaAsmDecoder));
end.
