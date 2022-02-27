unit FpDbgAvrClasses;

// Connects to gdbserver instance and communicate over gdb's remote serial protocol (RSP)
// in principle possible to connect over any serial text capabile interface such as
// tcp/ip, RS-232, pipes etc.
// Support only tcp/ip connection for now.

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
  FpDbgRsp, FpDbgCommon, FpdMemoryTools,
  FpErrorMessages;

const
  // RSP commands
  Rsp_Status = '?';     // Request break reason - returns either S or T
  lastCPURegIndex = 31; // After this are SREG, SP and PC
  // Use as dwarf register indexes
  SREGindex = 32;  // 1 byte
  SPindex = 33;    // 2 bytes
  PCindex = 34;    // 4 bytes
  RegArrayLength = 35;
  // Special register names
  nSREG = 'SReg';
  nSP = 'SP';
  nPC = 'PC';

  // Byte level register indexes
  SPLindex = 33;
  SPHindex = 34;
  PC0 = 35;
  PC1 = 36;
  PC2 = 37;
  PC3 = 38;
  RegArrayByteLength = 39;

type

  { TDbgAvrThread }

  TDbgAvrThread = class(TDbgThread)
  private
    FRegs: TInitializedRegisters;
    FRegsUpdated: boolean;   // regs read from target
    //FRegsChanged: boolean;   // write regs to target
    FExceptionSignal: integer;
    FIsPaused, FInternalPauseRequested, FIsInInternalPause: boolean;
    FIsSteppingBreakPoint: boolean;
    FDidResetInstructionPointer: Boolean;
    FHasThreadState: boolean;
    function ReadDebugReg(ind: byte; out AVal: TDbgPtr): boolean;
    function WriteDebugReg(ind: byte; AVal: PtrUInt): boolean;

    // Cache registers if reported in event
    // Only cache if all reqisters are reported
    // if not, request registers from target
    procedure UpdateStatusFromEvent(event: TStatusEvent);
    procedure InvalidateRegisters;
    procedure RefreshRegisterCache;
  protected
    function ReadThreadState: boolean;

    function RequestInternalPause: Boolean;
    function CheckSignalForPostponing(AWaitedStatus: integer): Boolean;
    procedure ResetPauseStates;
  public
    constructor Create(const AProcess: TDbgProcess; const AID: Integer; const AHandle: THandle); override;
    function ResetInstructionPointerAfterBreakpoint: boolean; override;
    procedure ApplyWatchPoints(AWatchPointData: TFpWatchPointData); override;
    function DetectHardwareWatchpoint: Pointer; override;
    procedure BeforeContinue; override;
    procedure LoadRegisterValues; override;

    function GetInstructionPointerRegisterValue: TDbgPtr; override;
    function GetStackBasePointerRegisterValue: TDbgPtr; override;
    function GetStackPointerRegisterValue: TDbgPtr; override;

    procedure PrepareCallStackEntryList(AFrameRequired: Integer = -1); override;
  end;

  { TDbgAvrProcess }

  TDbgAvrProcess = class(TDbgProcess)
  private
    FStatus: integer;
    FProcessStarted: boolean;
    FIsTerminating: boolean;
    // RSP communication
    FConnection: TRspConnection;
    FRemoteConfig: TRemoteConfig;

    procedure OnForkEvent(Sender : TObject);
  protected
    procedure InitializeLoaders; override;
    function CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread; override;
    function AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent; override;
    function CreateWatchPointData: TFpWatchPointData; override;
  public
    class function isSupported(target: TTargetDescriptor): boolean; override;
    constructor Create(const AFileName: string; AnOsClasses: TOSDbgClasses;
      AMemManager: TFpDbgMemManager; AProcessConfig: TDbgProcessConfig); override;
    destructor Destroy; override;
    function StartInstance(AParams, AnEnvironment: TStrings;
      AWorkingDirectory, AConsoleTty: string; AFlags: TStartInstanceFlags;
      out AnError: TFpError): boolean; override;

    // FOR AVR target AAddress could be program or data (SRAM) memory (or EEPROM)
    // Gnu tools masks data memory with $800000
    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; override;
    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; override;

    procedure TerminateProcess; override;
    function Pause: boolean; override;
    function Detach(AProcess: TDbgProcess; AThread: TDbgThread): boolean; override;

    function Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean; override;
    // Wait for -S or -T response from target, or if connection to target is lost
    function WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean; override;

    // Insert/Delete break points on target
    // TODO: if target doesn't support break points or have limited break points
    // then debugger needs to manage insertion/deletion of break points in target memory
    function InsertBreakInstructionCode(const ALocation: TDBGPtr; out OrigValue: Byte): Boolean; override;
    function RemoveBreakInstructionCode(const ALocation: TDBGPtr; const OrigValue: Byte): Boolean; override;

    property RspConfig: TRemoteConfig read FRemoteConfig;
  end;

  // Lets stick with points 4 for now

  { TFpRspWatchPointData }

  TRspBreakWatchPoint = record
    Owner: Pointer;
    Address: TDBGPtr;
    Size: Cardinal;
    Kind: TDBGWatchPointKind;
  end;

  TFpRspWatchPointData = class(TFpWatchPointData)
  private
    FData: array of TRspBreakWatchPoint;
    function BreakWatchPoint(AnIndex: Integer): TRspBreakWatchPoint;
    function DataCount: integer;
    function FindOwner(AnAddr: TDBGPtr): Pointer;
  public
    function AddOwnedWatchpoint(AnOwner: Pointer; AnAddr: TDBGPtr; ASize: Cardinal; AReadWrite: TDBGWatchPointKind): boolean; override;
    function RemoveOwnedWatchpoint(AnOwner: Pointer): boolean; override;
    property Data[AnIndex: Integer]: TRspBreakWatchPoint read BreakWatchPoint;
    property Count: integer read DataCount;
  end;

implementation

uses
  FpDbgDisasAvr, FpDbgDwarfDataClasses, FpDbgInfo;

var
  DBG_VERBOSE, DBG_WARNINGS: PLazLoggerLogGroup;

{ TFpRspWatchPointData }

function TFpRspWatchPointData.BreakWatchPoint(AnIndex: Integer
  ): TRspBreakWatchPoint;
begin
  if AnIndex < length(FData) then
    result := FData[AnIndex];
end;

function TFpRspWatchPointData.DataCount: integer;
begin
  result := length(FData);
end;

function TFpRspWatchPointData.FindOwner(AnAddr: TDBGPtr): Pointer;
var
  i: integer;
begin
  i := 0;
  while (i < Count) and not ((AnAddr >= Data[i].Address) and (AnAddr < Data[i].Address + Data[i].Size)) do
  begin
    inc(i);
  end;
  if i < Count then
    Result := Data[i].Owner
  else
    Result := nil;
end;

function TFpRspWatchPointData.AddOwnedWatchpoint(AnOwner: Pointer;
  AnAddr: TDBGPtr; ASize: Cardinal; AReadWrite: TDBGWatchPointKind): boolean;
var
  idx: integer;
begin
  Result := false;
  idx := length(FData);
  SetLength(FData, idx+1);
  FData[idx].Address := AnAddr;
  FData[idx].Size := ASize;
  FData[idx].Kind := AReadWrite;
  FData[idx].Owner := AnOwner;
  Changed := true;
  Result := true;
end;

function TFpRspWatchPointData.RemoveOwnedWatchpoint(AnOwner: Pointer): boolean;
var
  i, j: integer;
begin
  Result := False;
  i := 0;
  while (i < length(FData)) and (FData[i].Owner <> AnOwner) do
    inc(i);

  if i < length(FData) then begin
    for j := i+1 to length(FData)-1 do begin
      FData[j-1] := FData[j];
      Changed := True;
      Result := True;
    end;

    SetLength(FData, length(FData)-1);
    Changed := True;
    Result := True;
  end;
end;

{ TDbgAvrThread }

procedure TDbgAvrProcess.OnForkEvent(Sender: TObject);
begin
end;

function TDbgAvrThread.ReadDebugReg(ind: byte; out AVal: TDbgPtr): boolean;
begin
  Result := false;
  if TDbgAvrProcess(Process).FIsTerminating or (TDbgAvrProcess(Process).FStatus = SIGHUP) then
    DebugLn(DBG_WARNINGS, 'TDbgRspThread.GetDebugReg called while FIsTerminating is set.')
  else
  begin
    DebugLn(DBG_VERBOSE, ['TDbgRspThread.GetDebugReg requesting register: ',ind]);
    RefreshRegisterCache;
    if ind < length(FRegs) then
    begin
      AVal := FRegs[ind].Value;
      Result := true;
    end;
  end;
end;

function TDbgAvrThread.WriteDebugReg(ind: byte; AVal: PtrUInt): boolean;
begin
  if TDbgAvrProcess(Process).FIsTerminating or (TDbgAvrProcess(Process).FStatus = SIGHUP) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspThread.WriteDebugReg called while FIsTerminating is set.');
    Result := false;
  end
  else
    result := TDbgAvrProcess(Process).FConnection.WriteDebugReg(ind, AVal);
end;

procedure TDbgAvrThread.UpdateStatusFromEvent(event: TStatusEvent);
var
  i: integer;
begin
  for i := 0 to high(FRegs) do
  begin
    FRegs[i].Initialized := event.registers[i].Initialized;
    if event.registers[i].Initialized then
      FRegs[i].Value := event.registers[i].Value;
  end;
end;

procedure TDbgAvrThread.InvalidateRegisters;
var
  i: integer;
begin
  FRegsUpdated := false;
  for i := 0 to high(FRegs) do
    FRegs[i].Initialized := false;
end;

procedure TDbgAvrThread.RefreshRegisterCache;
var
  regs: TBytes;
  i: integer;
begin
  if not FRegsUpdated then
  begin
    SetLength(regs, RegArrayByteLength);
    FRegsUpdated := TDbgAvrProcess(Process).FConnection.ReadRegisters(regs[0], length(regs));
    for i := 0 to lastCPURegIndex do
    begin
      FRegs[i].Initialized := true;
      FRegs[i].Value := regs[i];
    end;
    // repack according to target endianness
    FRegs[SPindex].Value := regs[SPLindex] + (regs[SPHindex] shl 8);
    FRegs[SPHindex].Initialized := true;
    FRegs[PCindex].Value := regs[PC0] + (regs[PC1] shl 8) + (regs[PC2] shl 16) + (regs[PC3] shl 24);
    FRegs[PCindex].Initialized := true;
  end;
end;

function TDbgAvrThread.ReadThreadState: boolean;
begin
//  assert(FIsPaused, 'TDbgRspThread.ReadThreadState: FIsPaused');
  result := true;
  if FHasThreadState then
    exit;
  FRegisterValueListValid := false;
end;

function TDbgAvrThread.RequestInternalPause: Boolean;
begin
  if TDbgAvrProcess(Process).FIsTerminating then
    DebugLn(DBG_WARNINGS, 'TDbgRspThread.RequestInternalPause called while FIsTerminating is set.');

  Result := False;
  if FInternalPauseRequested or FIsPaused or (TDbgAvrProcess(Process).FStatus = SIGHUP) then
    exit;

  DebugLn(DBG_VERBOSE, 'TDbgRspThread.RequestInternalPause requesting Ctrl-C.');

  FInternalPauseRequested := true;
  // Send SIGSTOP/break
  TDbgAvrProcess(Process).FConnection.Break();
end;

function TDbgAvrThread.CheckSignalForPostponing(AWaitedStatus: integer): Boolean;
begin
  Assert(not FIsPaused, 'Got WaitStatus while already paused');
  assert(FExceptionSignal = 0, 'TDbgLinuxThread.CheckSignalForPostponing: FExceptionSignal = 0');
  Result := FIsPaused;
  DebugLn(DBG_VERBOSE and (Result), ['Warning: Thread already paused', ID]);

  DebugLn(DBG_VERBOSE, ['TDbgRspThread.CheckSignalForPostponing called with ', AWaitedStatus]);

  if Result then
    exit;

  FIsPaused := True;
  FIsInInternalPause := False;
end;

procedure TDbgAvrThread.ResetPauseStates;
begin
  FIsInInternalPause := False;
  FIsPaused := False;
  FExceptionSignal := 0;
  FHasThreadState := False;
  FDidResetInstructionPointer := False;
end;

constructor TDbgAvrThread.Create(const AProcess: TDbgProcess;
  const AID: Integer; const AHandle: THandle);
begin
  inherited;
  SetLength(FRegs, RegArrayLength);
end;

function TDbgAvrThread.ResetInstructionPointerAfterBreakpoint: boolean;
begin
  if not ReadThreadState then
    exit(False);
  result := true;
  if FDidResetInstructionPointer then
    exit;
  FDidResetInstructionPointer := True;

  // This is not required for gdbserver
  // since remote stub should ensure PC points to break address
  //Dec(FRegs.cpuRegs[PCindex]);
  //FRegsChanged:=true;
end;

procedure TDbgAvrThread.ApplyWatchPoints(AWatchPointData: TFpWatchPointData);
var
  i: integer;
  addr: PtrUInt;
  watchData: TRspBreakWatchPoint;
  tmpData: TBytes;
begin
  for i := 0 to TFpRspWatchPointData(AWatchPointData).Count-1 do
  begin
    watchData := TFpRspWatchPointData(AWatchPointData).Data[i];
    addr := watchData.Address;
    SetLength(tmpData, watchData.Size);
    if Process.ReadData(addr, watchData.Size, tmpData[0]) then
    begin
      if not TDbgAvrProcess(Process).FConnection.SetBreakWatchPoint(addr, watchData.Kind) then
        DebugLn(DBG_WARNINGS, 'Failed to set watch point.', []);
    end
    else
      DebugLn(DBG_WARNINGS, 'Failed to read memory.', []);
  end;
end;

function TDbgAvrThread.DetectHardwareWatchpoint: Pointer;
begin
  if TDbgAvrProcess(Process).FConnection.LastStatusEvent.stopReason in [srAnyWatchPoint, srReadWatchPoint, srWriteWatchPoint] then
  begin
    Result := TFpRspWatchPointData(TDbgAvrProcess(Process).WatchPointData).FindOwner(TDbgAvrProcess(Process).FConnection.LastStatusEvent.watchPointAddress);
    TDbgAvrProcess(Process).FConnection.ResetStatusEvent;
  end
  else
    result := nil;
end;

procedure TDbgAvrThread.BeforeContinue;
//var
//  regs: TBytes;
begin
  if not FIsPaused then
    exit;

  inherited;
  InvalidateRegisters;

  // TODO: currently nothing changes registers locally?

  // Update registers if changed locally
  //if FRegsChanged then
  //begin
  //  SetLength(regs, RegArrayByteLength);
  //  for i := 0 to lastCPURegIndex do
  //    regs[i] :=
  //  FRegsChanged:=false;
  //end;
end;

procedure TDbgAvrThread.LoadRegisterValues;
var
  i: integer;
begin
  if TDbgAvrProcess(Process).FIsTerminating or (TDbgAvrProcess(Process).FStatus = SIGHUP) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspThread.LoadRegisterValues called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  RefreshRegisterCache;

  if FRegsUpdated then
  begin
    for i := 0 to lastCPURegIndex do
      FRegisterValueList.DbgRegisterAutoCreate['r'+IntToStr(i)].SetValue(FRegs[i].Value, IntToStr(FRegs[i].Value),1, i); // confirm dwarf index

    FRegisterValueList.DbgRegisterAutoCreate[nSREG].SetValue(FRegs[SREGindex].Value, IntToStr(FRegs[SREGindex].Value),1,SREGindex);
    FRegisterValueList.DbgRegisterAutoCreate[nSP].SetValue(FRegs[SPindex].Value, IntToStr(FRegs[SPindex].Value),2,SPindex);
    FRegisterValueList.DbgRegisterAutoCreate[nPC].SetValue(FRegs[PCindex].Value, IntToStr(FRegs[PCindex].Value),4,PCindex);
    FRegisterValueListValid := true;
  end
  else
    DebugLn(DBG_WARNINGS, 'Warning: Could not update registers');
end;

function TDbgAvrThread.GetInstructionPointerRegisterValue: TDbgPtr;
begin
  Result := 0;
  if TDbgAvrProcess(Process).FIsTerminating then
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
  if TDbgAvrProcess(Process).FIsTerminating then
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

function TDbgAvrThread.GetStackPointerRegisterValue: TDbgPtr;
begin
  Result := 0;
  if TDbgAvrProcess(Process).FIsTerminating then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspThread.GetStackPointerRegisterValue called while FIsTerminating is set.');
    exit;
  end;

  if not ReadThreadState then
    exit;

  DebugLn(DBG_VERBOSE, 'TDbgRspThread.GetStackPointerRegisterValue requesting stack registers.');
  ReadDebugReg(SPindex, result);
end;

procedure TDbgAvrThread.PrepareCallStackEntryList(AFrameRequired: Integer);
const
  MAX_FRAMES = 50000; // safety net
  // To read RAM, add data space offset to address
  DataOffset = $800000;
  Size = 2;
var
  Address, FrameBase, LastFrameBase: TDBGPtr;
  CountNeeded, CodeReadErrCnt: integer;
  AnEntry: TDbgCallstackEntry;
  R: TDbgRegisterValue;
  NextIdx: LongInt;
  OutSideFrame: Boolean;
  StackPtr: TDBGPtr;
  startPC, endPC: TDBGPtr;
  returnAddrStackOffset: word;
  b: byte;
begin
  // TODO: use AFrameRequired // check if already partly done
  if FCallStackEntryList = nil then
    FCallStackEntryList := TDbgCallstackEntryList.Create;
  if AFrameRequired = -2 then
    exit;

  if (AFrameRequired >= 0) and (AFrameRequired < FCallStackEntryList.Count) then
    exit;

  FCallStackEntryList.FreeObjects:=true;
  if FCallStackEntryList.Count > 0 then begin
    AnEntry := FCallStackEntryList[FCallStackEntryList.Count - 1];
    R := AnEntry.RegisterValueList.FindRegisterByDwarfIndex(PCindex);
    if R = nil then exit;
    Address := R.NumValue;
    R := AnEntry.RegisterValueList.FindRegisterByDwarfIndex(28);
    if R = nil then exit;
    FrameBase := R.NumValue;
    R := AnEntry.RegisterValueList.FindRegisterByDwarfIndex(29);
    if R = nil then exit;
    FrameBase := FrameBase + (byte(R.NumValue) shl 8);

    R := AnEntry.RegisterValueList.FindRegisterByDwarfIndex(SPindex);
    if R = nil then exit;
    StackPtr := R.NumValue;
  end
  else begin
    Address := GetInstructionPointerRegisterValue;
    FrameBase := GetStackBasePointerRegisterValue;
    StackPtr := GetStackPointerRegisterValue;
    AnEntry := TDbgCallstackEntry.create(Self, 0, FrameBase, Address);
    // Top level could be without entry in registerlist / same as GetRegisterValueList / but some code tries to find it here ....
    AnEntry.RegisterValueList.DbgRegisterAutoCreate[nPC].SetValue(Address, IntToStr(Address),Size, PCindex);
    AnEntry.RegisterValueList.DbgRegisterAutoCreate[nSP].SetValue(StackPtr, IntToStr(StackPtr),Size, SPindex);

    // Y pointer register [r28:r29] is used as frame pointer for AVR
    // Store these to enable stack based parameters to be read correctly
    // as they are frame pointer relative and dwarf stores the frame pointer under r28
    //AnEntry.RegisterValueList.DbgRegisterAutoCreate[nFP].SetValue(FrameBase, IntToStr(FrameBase),Size, FPindex);
    b := byte(FrameBase);
    AnEntry.RegisterValueList.DbgRegisterAutoCreate['r28'].SetValue(b, IntToStr(b),Size, 28);
    b := (FrameBase and $FF00) shr 8;
    AnEntry.RegisterValueList.DbgRegisterAutoCreate['r29'].SetValue(b, IntToStr(b),Size, 29);

    FCallStackEntryList.Add(AnEntry);
  end;

  NextIdx := FCallStackEntryList.Count;
  if AFrameRequired < 0 then
    AFrameRequired := MaxInt;
  CountNeeded := AFrameRequired - FCallStackEntryList.Count;
  LastFrameBase := 0;
  CodeReadErrCnt := 0;
  while (CountNeeded > 0) and (FrameBase <> 0) and (FrameBase > LastFrameBase) do
  begin
    // Get start/end PC of proc from debug info
    if not Self.Process.FindProcStartEndPC(Address, startPC, endPC) then
    begin
      // Give up for now, it is complicated to locate prologue/epilogue in general without proc limits
      // ToDo: Perhaps interpret .debug_frame info if available,
      //       or scan forward from address until an epilogue is found.
      break;
    end;

    if not TAvrAsmDecoder(Process.Disassembler).GetFunctionFrameReturnAddress(Address, startPC, endPC, returnAddrStackOffset, OutSideFrame) then
    begin
      OutSideFrame := False;
    end;
    LastFrameBase := FrameBase;

    if OutSideFrame then begin
      // Before adjustment of frame pointer, or after restoration of frame pointer,
      // return PC should be located by offset from SP
      if not Process.ReadData(DataOffset or (StackPtr + returnAddrStackOffset), Size, Address) or (Address = 0) then Break;
    end
    else begin
      // Inside stack frame, return PC should be located by offset from FP
      if not Process.ReadData(DataOffset or (FrameBase + returnAddrStackOffset), Size, Address) or (Address = 0) then Break;
    end;
    // Convert return address from BE to LE, shl 1 to get byte address
    Address := BEtoN(word(Address)) shl 1;
    {$PUSH}{$R-}{$Q-}
    StackPtr := FrameBase + returnAddrStackOffset + Size - 1; // After popping return-addr from "StackPtr"
    FrameBase := StackPtr;  // Estimate of previous FP
    LastFrameBase := LastFrameBase - 1; // Make the loop think that LastFrameBase was smaller
    {$POP}

    AnEntry := TDbgCallstackEntry.create(Self, NextIdx, FrameBase, Address);
    AnEntry.RegisterValueList.DbgRegisterAutoCreate[nPC].SetValue(Address, IntToStr(Address),Size, PCindex);
    AnEntry.RegisterValueList.DbgRegisterAutoCreate[nSP].SetValue(StackPtr, IntToStr(StackPtr),Size, SPindex);
    AnEntry.RegisterValueList.DbgRegisterAutoCreate['r28'].SetValue(byte(FrameBase), IntToStr(b),Size, 28);
    AnEntry.RegisterValueList.DbgRegisterAutoCreate['r29'].SetValue((FrameBase and $FF00) shr 8, IntToStr(b),Size, 29);

    FCallStackEntryList.Add(AnEntry);
    Dec(CountNeeded);
    inc(NextIdx);
    CodeReadErrCnt := 0;
    if (NextIdx > MAX_FRAMES) then
      break;
  end;
  if CountNeeded > 0 then // there was an error / not possible to read more frames
    FCallStackEntryList.SetHasReadAllAvailableFrames;
end;

{ TDbgAvrProcess }

procedure TDbgAvrProcess.InitializeLoaders;
begin
  if LoaderList.Count = 0 then
    TDbgImageLoader.Create(Name).AddToLoaderList(LoaderList);
end;

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

function TDbgAvrProcess.CreateWatchPointData: TFpWatchPointData;
begin
  DebugLn(DBG_VERBOSE, 'TDbgRspProcess.CreateWatchPointData called.');
  Result := TFpRspWatchPointData.Create;
end;

constructor TDbgAvrProcess.Create(const AFileName: string; AnOsClasses: TOSDbgClasses;
  AMemManager: TFpDbgMemManager; AProcessConfig: TDbgProcessConfig);
begin
  if Assigned(AProcessConfig) and (AProcessConfig is TRemoteConfig) then
  begin
    FRemoteConfig := TRemoteConfig.Create;
    FRemoteConfig.Assign(AProcessConfig);
  end;

  inherited Create(AFileName, AnOsClasses, AMemManager, AProcessConfig);
end;

destructor TDbgAvrProcess.Destroy;
begin
  if Assigned(FConnection) then
    FreeAndNil(FConnection);
  if Assigned(FRemoteConfig) then
    FreeAndNil(FRemoteConfig);
  inherited Destroy;
end;

function TDbgAvrProcess.StartInstance(AParams, AnEnvironment: TStrings;
  AWorkingDirectory, AConsoleTty: string; AFlags: TStartInstanceFlags; out
  AnError: TFpError): boolean;
var
  AnExecutabeFilename: string;
begin
  Result := false;
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

  if not Assigned(FRemoteConfig) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgAvrProcess only supports remote debugging and requires a valid TRemoteConfig class');
    Exit;
  end;

  try
    FConnection := TRspConnection.Create(Name, self, self.FRemoteConfig);
    FConnection.Connect;
    try
      FConnection.RegisterCacheSize := RegArrayLength;
      FStatus := FConnection.Init;
      Result := true;
    except
      on E: Exception do
      begin
        DebugLn(DBG_WARNINGS, Format('Failed to init remote connection. Errormessage: "%s".', [E.Message]));
      end;
    end;
  except
    on E: Exception do
    begin
      DebugLn(DBG_WARNINGS, Format('Failed to start remote connection. Errormessage: "%s".', [E.Message]));
    end;
  end;
end;

class function TDbgAvrProcess.isSupported(target: TTargetDescriptor): boolean;
begin
  result := (target.OS = osEmbedded) and
            (target.machineType = mtAVR8);
end;

function TDbgAvrProcess.ReadData(const AAdress: TDbgPtr;
  const ASize: Cardinal; out AData): Boolean;
begin
  if FIsTerminating or (TDbgAvrProcess(Process).FStatus = SIGHUP) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspProcess.ReadData called while FIsTerminating is set.');
    Result := false;
    exit;
  end;

  result := FConnection.ReadData(AAdress, ASize, AData);
  if Result then
    MaskBreakpointsInReadData(AAdress, ASize, AData);
end;

function TDbgAvrProcess.WriteData(const AAdress: TDbgPtr;
  const ASize: Cardinal; const AData): Boolean;
begin
  if FIsTerminating or (TDbgAvrProcess(Process).FStatus = SIGHUP) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspProcess.WriteData called while FIsTerminating is set.');
    Result := false;
    exit;
  end;

  result := FConnection.WriteData(AAdress,AAdress, AData);
end;

procedure TDbgAvrProcess.TerminateProcess;
begin
  // Try to prevent access to the RSP socket after it has been closed
  if not (FIsTerminating or (TDbgAvrProcess(Process).FStatus = SIGHUP)) then
  begin
    DebugLn(DBG_VERBOSE, 'Removing all break points');
    RemoveAllBreakPoints;
    DebugLn(DBG_VERBOSE, 'Sending kill command from TDbgRspProcess.TerminateProcess');
    FConnection.Kill();
    FIsTerminating:=true;
  end;
end;

function TDbgAvrProcess.Pause: boolean;
begin
  if FIsTerminating or (TDbgAvrProcess(Process).FStatus = SIGHUP) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspProcess.Pause called while FIsTerminating is set.');
    Result := false;
    exit;
  end;

  // Target should automatically respond with T or S reply after processing the break
  result := true;
  if not PauseRequested then
  begin
    FConnection.Break();
    PauseRequested := true;
    DebugLn(DBG_VERBOSE, 'TDbgRspProcess.Pause called.');
  end
  else
  begin
    result := true;
    DebugLn(DBG_WARNINGS, 'TDbgRspProcess.Pause called while PauseRequested is set.');
  end;
end;

function TDbgAvrProcess.Detach(AProcess: TDbgProcess; AThread: TDbgThread): boolean;
begin
  RemoveAllBreakPoints;
  DebugLn(DBG_VERBOSE, 'Sending detach command from TDbgRspProcess.Detach');
  Result := FConnection.Detach();
end;

function TDbgAvrProcess.Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean;
var
  ThreadToContinue: TDbgAvrThread;
  PC: word;
  s: string;
  tempState: integer;
  initRegs: TInitializedRegisters;
begin
  // Terminating process and all threads
  if FIsTerminating or (FStatus = SIGHUP) then
  begin
    AThread.BeforeContinue;
    TDbgAvrThread(AThread).InvalidateRegisters;
    DebugLn(DBG_VERBOSE, 'TDbgRspProcess.Continue called while terminating.');

    // The kill command should have been issued earlier (if using fpd), calling SendKill again will lead to an exception since the connection should be terminated already.
    // FConnection.Kill();

    TDbgAvrThread(AThread).ResetPauseStates;
    if not FThreadMap.HasId(AThread.ID) then
      AThread.Free;
    exit;
  end;

  if TDbgAvrThread(AThread).FIsPaused then  // in case of deInternal, it may not be paused and can be ignored
    AThread.NextIsSingleStep:=SingleStep;

  // check other threads if they need a singlestep
  for TDbgThread(ThreadToContinue) in FThreadMap do
    if (ThreadToContinue <> AThread) and ThreadToContinue.FIsPaused then
    begin
      PC := ThreadToContinue.GetInstructionPointerRegisterValue;
      if HasInsertedBreakInstructionAtLocation(PC) then
      begin
        TempRemoveBreakInstructionCode(PC);
        ThreadToContinue.BeforeContinue;

        while (ThreadToContinue.GetInstructionPointerRegisterValue = PC) do
        begin
          result := FConnection.SingleStep();
          TDbgAvrThread(ThreadToContinue).ResetPauseStates; // So BeforeContinue will not run again
          ThreadToContinue.FIsPaused := True;
          if result then
          begin
            tempState := FConnection.WaitForSignal(s, initRegs);  // TODO: Update registers cache for this thread
            if (tempState = SIGTRAP) then
              break; // if the command jumps back an itself....
          end
          else
          begin
            DebugLn(DBG_WARNINGS, ['Error single stepping other thread ', ThreadToContinue.ID]);
            break;
          end;
        end;
      end;
    end;

  if TDbgAvrThread(AThread).FIsPaused and SingleStep then  // in case of deInternal, it may not be paused and can be ignored
  if HasInsertedBreakInstructionAtLocation(AThread.GetInstructionPointerRegisterValue) then
  begin
    TempRemoveBreakInstructionCode(AThread.GetInstructionPointerRegisterValue);
    TDbgAvrThread(AThread).FIsSteppingBreakPoint := True;
    AThread.BeforeContinue;
    result := FConnection.SingleStep(); // TODO: pass thread ID once it is supported in FConnection - also signals not yet passed through
    TDbgAvrThread(AThread).ResetPauseStates;
    FStatus := 0; // need to call WaitForSignal to read state after single step
    exit;
  end;

  RestoreTempBreakInstructionCodes;

  ThreadsBeforeContinue;

  // start all other threads
  for TDbgThread(ThreadToContinue) in FThreadMap do
  begin
    if (ThreadToContinue <> AThread) and (ThreadToContinue.FIsPaused) then
    begin
      FConnection.Continue();
      ThreadToContinue.ResetPauseStates;
    end;
  end;

  if TDbgAvrThread(AThread).FIsPaused then  // in case of deInternal, it may not be paused and can be ignored
    if not FIsTerminating then
    begin
      AThread.BeforeContinue;
      if SingleStep then
        result := FConnection.SingleStep()
      else
        result := FConnection.Continue();
      TDbgAvrThread(AThread).ResetPauseStates;
      FStatus := 0;  // should update status by calling WaitForSignal
    end;

  if not FThreadMap.HasId(AThread.ID) then
    AThread.Free;
end;

function TDbgAvrProcess.WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean;
var
  s: string;
  initRegs: TInitializedRegisters;
begin
  debugln(DBG_VERBOSE, ['Entering WaitForDebugEvent, FStatus = ', FStatus]);
  // Currently only single process/thread
  // TODO: Query and handle process/thread states of target
  ThreadIdentifier  := self.ThreadID;
  ProcessIdentifier := Self.ProcessID;

  if FIsTerminating then
  begin
    DebugLn(DBG_VERBOSE, 'TDbgRspProcess.WaitForDebugEvent called while FIsTerminating is set.');
    FStatus := SIGKILL;
  end
  else
  // Wait for S or T response from target, or if connection to target is lost
  if FStatus = 0 then
    repeat
      try
        FStatus := FConnection.WaitForSignal(s, initRegs); // TODO: Update registers cache
        sleep(1);
      except
        FStatus := 0;
      end;
    until FStatus <> 0;

  if FStatus in [SIGINT, SIGTRAP] then
    RestoreTempBreakInstructionCodes;

  result := FStatus <> 0;
end;

function TDbgAvrProcess.InsertBreakInstructionCode(const ALocation: TDBGPtr;
  out OrigValue: Byte): Boolean;
begin
  if FIsTerminating or (FStatus = SIGHUP) then
    DebugLn(DBG_WARNINGS, 'TDbgRspProcess.InsertBreakInstruction called while FIsTerminating is set.');

  result := ReadData(ALocation, SizeOf(OrigValue), OrigValue);
  if result then
  begin
  // HW break...
    result := FConnection.SetBreakWatchPoint(ALocation, wkpExec);
    if not result then
      DebugLn(DBG_WARNINGS, 'Failed to set break point.', []);
  end
  else
    DebugLn(DBG_WARNINGS, 'Failed to read memory.', []);
end;

function TDbgAvrProcess.RemoveBreakInstructionCode(const ALocation: TDBGPtr;
  const OrigValue: Byte): Boolean;
begin
  if FIsTerminating or (FStatus = SIGHUP) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspProcess.RemoveBreakInstructionCode called while FIsTerminating is set');
    result := false;
  end
  else
    result := FConnection.DeleteBreakWatchPoint(ALocation, wkpExec);
end;

function TDbgAvrProcess.AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent;
var
  ThreadToPause: TDbgAvrThread;
begin
  debugln(DBG_VERBOSE, ['Entering TDbgRspProcess.AnalyseDebugEvent, FStatus = ', FStatus, ' PauseRequested = ', PauseRequested]);
  if FIsTerminating then begin
    result := deExitProcess;
    exit;
  end;

  if AThread = nil then begin // should not happen... / just assume the most likely safe failbacks
    result := deInternalContinue;
    exit;
  end;

  TDbgAvrThread(AThread).FExceptionSignal:=0;
  TDbgAvrThread(AThread).FIsPaused := True;
  TDbgAvrThread(AThread).UpdateStatusFromEvent(FConnection.lastStatusEvent);

  if FStatus in [SIGHUP, SIGKILL] then  // not sure which signals is relevant here
  begin
    if AThread.ID=ProcessID then
    begin
      // Main thread stop -> application exited
      SetExitCode(FStatus);
      result := deExitProcess
    end
    else
    begin
      // Thread stopped, just continue
      RemoveThread(AThread.Id);
      result := deInternalContinue;
    end;
  end
  else if FStatus <> 0 then
  begin
    TDbgAvrThread(AThread).ReadThreadState;

    if (not FProcessStarted) and (FStatus <> SIGTRAP) then
    begin
      // attached, should be SigStop, but may be out of order
      debugln(DBG_VERBOSE, ['Attached ', FStatus]);
      result := deCreateProcess;
      FProcessStarted:=true;
    end
    else
    case FStatus of
      SIGTRAP:
      begin
        if not FProcessStarted then
        begin
          result := deCreateProcess;
          FProcessStarted:=true;
          DebugLn(DBG_VERBOSE, ['Creating process - SIGTRAP received for thread: ', AThread.ID]);
        end
        else if TDbgAvrThread(AThread).FInternalPauseRequested then
        begin
          DebugLn(DBG_VERBOSE, ['???Received late SigTrap for thread ', AThread.ID]);
          result := deBreakpoint;
        end
        else
        begin
          DebugLn(DBG_VERBOSE, ['Received SigTrap for thread ', AThread.ID,
             ' PauseRequest=', PauseRequested]);
          result := deBreakpoint;

          if not TDbgAvrThread(AThread).FIsSteppingBreakPoint then
            AThread.CheckAndResetInstructionPointerAfterBreakpoint;
        end;
      end;
      SIGINT:
        begin
          if PauseRequested then
            result := deBreakpoint
          else
          begin
            ExceptionClass:='SIGINT';
            TDbgAvrThread(AThread).FExceptionSignal:=SIGINT;
            result := deException;
          end;
        end;
      SIGKILL:
        begin
          if FIsTerminating then
            result := deInternalContinue
          else
            begin
            ExceptionClass:='SIGKILL';
            TDbgAvrThread(AThread).FExceptionSignal:=SIGKILL;
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
        ExceptionClass:='Unknown exception code ' + inttostr(FStatus);
        TDbgAvrThread(AThread).FExceptionSignal := FStatus;
        result := deException;
      end;
    end; {case}
    if result=deException then
      ExceptionClass:='External: '+ExceptionClass;
  end;

  debugln(DBG_VERBOSE, ['Leaving AnalyseDebugEvent, result = ', result]);

  TDbgAvrThread(AThread).FIsSteppingBreakPoint := False;

  if Result in [deException, deBreakpoint, deFinishedStep] then // deFinishedStep will not be set here
  begin
    // Signal all other threads to pause
    for TDbgThread(ThreadToPause) in FThreadMap do
    begin
      if (ThreadToPause <> AThread) then
      begin
          DebugLn(DBG_VERBOSE and (ThreadToPause.FInternalPauseRequested), ['Re-Request Internal pause for ', ThreadToPause.ID]);
          ThreadToPause.FInternalPauseRequested:=false;
          if not ThreadToPause.RequestInternalPause then // will fail, if already paused
            break;
      end;
    end;
  end;
end;

initialization
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  RegisterDbgOsClasses(TOSDbgClasses.Create(
    TDbgAvrProcess,
    TDbgAvrThread,
    TAvrAsmDecoder));
end.
