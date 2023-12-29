unit FpDbgRspClasses;

{ Connects to gdbserver instance and communicate over gdb's remote serial protocol (RSP).
  Can in principle possible to connect over any serial text capabile interface
  such as tcp/ip, RS-232, pipes etc. Currently only tcp/ip connections are supported. }

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

type
  TInitializedRegisters = record
    Initialized: boolean;
    regs: array of qword; // sized to handle largest register, should truncate as required to smaller registers
  end;

  { TDbgRspThread }

  TDbgRspThread = class(TDbgThread)
  private
  protected
    FRegs: TInitializedRegisters;
    FRegsChanged: boolean;
    FStoredRegs: TInitializedRegisters;
    FExceptionSignal: integer;
    FIsPaused, FInternalPauseRequested, FIsInInternalPause: boolean;
    FIsSteppingBreakPoint: boolean;
    FDidResetInstructionPointer: Boolean;
    FHasThreadState: boolean;
    FUnwinder: TDbgStackUnwinder;

    procedure LoadRegisterCache; virtual;
    procedure SaveRegisterCache; virtual;
    procedure InvalidateRegisters;
    function ReadDebugReg(ind: byte; out AVal: TDbgPtr): boolean;
    function WriteDebugReg(ind: byte; AVal: PtrUInt): boolean;
    function ReadThreadState: boolean;
    function RequestInternalPause: Boolean;
    procedure ResetPauseStates;
  public
    constructor Create(const AProcess: TDbgProcess; const AID: Integer; const AHandle: THandle); override;
    function ResetInstructionPointerAfterBreakpoint: boolean; override;
    procedure ApplyWatchPoints(AWatchPointData: TFpWatchPointData); override;
    function DetectHardwareWatchpoint: Pointer; override;
    procedure BeforeContinue; override;
    procedure SetRegisterValue(AName: string; AValue: QWord); override;
    procedure StoreRegisters; override;
    procedure RestoreRegisters; override;
  end;

  { TDbgRspProcess }

  TDbgRspProcess = class(TDbgProcess)
  private
  protected
    FStatus: integer;
    FProcessStarted: boolean;
    FIsTerminating: boolean;
    FConnection: TRspConnection;
    FRemoteConfig: TRemoteConfig;
    // Initialize in target specific class
    FRegArrayLength: integer;

    function AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent; override;
    procedure InitializeLoaders; override;
    // Insert/Delete break points on target
    // TODO: if target doesn't support break points or have limited break points
    // then debugger needs to manage insertion/deletion of break points in target memory
    function InsertBreakInstructionCode(const ALocation: TDBGPtr; out OrigValue: Byte; AMakeTempRemoved: Boolean): Boolean; override;
    function RemoveBreakInstructionCode(const ALocation: TDBGPtr; const OrigValue: Byte): Boolean; override;
  public
    constructor Create(const AFileName: string; AnOsClasses: TOSDbgClasses;
                      AMemManager: TFpDbgMemManager; AProcessConfig: TDbgProcessConfig = nil); override;
    destructor Destroy; override;

    function StartInstance(AParams, AnEnvironment: TStrings; AWorkingDirectory, AConsoleTty: string;
                      AFlags: TStartInstanceFlags; out AnError: TFpError): boolean; override;
    function AttachToInstance(APid: Integer; out AnError: TFpError): boolean; override;

    procedure CreateRspConnection(AFileName: string);

    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; override;
    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; override;

    procedure TerminateProcess; override;
    function Pause: boolean; override;
    function Detach(AProcess: TDbgProcess; AThread: TDbgThread): boolean; override;

    function Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean; override;
    // Wait for -S or -T response from target, or if connection to target is lost
    function WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean; override;

    property RspConnection: TRspConnection read FConnection;
    // Target specific length of register array
    property RegArrayLength: integer read FRegArrayLength;
    property IsTerminating: boolean read FIsTerminating;
    property Status: integer read FStatus;
  end;

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
  public
    function AddOwnedWatchpoint(AnOwner: Pointer; AnAddr: TDBGPtr; ASize: Cardinal; AReadWrite: TDBGWatchPointKind): boolean; override;
    function RemoveOwnedWatchpoint(AnOwner: Pointer): boolean; override;
    function FindOwner(AnAddr: TDBGPtr): Pointer;
    property Data[AnIndex: Integer]: TRspBreakWatchPoint read BreakWatchPoint;
    property Count: integer read DataCount;
  end;

implementation

uses
  FpDbgDwarfDataClasses;

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

{ TDbgRspThread }

function TDbgRspThread.ReadDebugReg(ind: byte; out AVal: TDbgPtr): boolean;
begin
  Result := false;
  if TDbgRspProcess(Process).FIsTerminating or (TDbgRspProcess(Process).FStatus = SIGHUP) then
    DebugLn(DBG_WARNINGS, 'TDbgRspThread.GetDebugReg called while FIsTerminating is set.')
  else
  begin
    DebugLn(DBG_VERBOSE, ['TDbgRspThread.GetDebugReg requesting register: ',ind]);
    LoadRegisterCache;
    if ind < length(FRegs.regs) then
    begin
      AVal := FRegs.regs[ind];
      Result := true;
    end;
  end;
end;

function TDbgRspThread.WriteDebugReg(ind: byte; AVal: PtrUInt): boolean;
begin
  if TDbgRspProcess(Process).FIsTerminating or (TDbgRspProcess(Process).FStatus = SIGHUP) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspThread.WriteDebugReg called while FIsTerminating is set.');
    Result := false;
  end
  else
    result := TDbgRspProcess(Process).RspConnection.WriteDebugReg(ind, AVal);
end;

procedure TDbgRspThread.InvalidateRegisters;
begin
  FRegs.Initialized := false;
end;

procedure TDbgRspThread.LoadRegisterCache;
begin
  // Target specific
end;

procedure TDbgRspThread.SaveRegisterCache;
begin
  // Target specific
end;

function TDbgRspThread.ReadThreadState: boolean;
begin
//  assert(FIsPaused, 'TDbgRspThread.ReadThreadState: FIsPaused');
  result := true;
  if FHasThreadState then
    exit;
  FRegisterValueListValid := false;
end;

function TDbgRspThread.RequestInternalPause: Boolean;
begin
  if TDbgRspProcess(Process).FIsTerminating then
    DebugLn(DBG_WARNINGS, 'TDbgRspThread.RequestInternalPause called while FIsTerminating is set.');

  Result := False;
  if FInternalPauseRequested or FIsPaused or (TDbgRspProcess(Process).FStatus = SIGHUP) then
    exit;

  DebugLn(DBG_VERBOSE, 'TDbgRspThread.RequestInternalPause requesting Ctrl-C.');

  FInternalPauseRequested := true;
  // Send SIGSTOP/break
  TDbgRspProcess(Process).RspConnection.Break();
end;

procedure TDbgRspThread.ResetPauseStates;
begin
  FIsInInternalPause := False;
  FIsPaused := False;
  FExceptionSignal := 0;
  FHasThreadState := False;
  FDidResetInstructionPointer := False;
end;

constructor TDbgRspThread.Create(const AProcess: TDbgProcess;
  const AID: Integer; const AHandle: THandle);
begin
  inherited;
  FRegs.Initialized := false;
  SetLength(FRegs.regs, TDbgRspProcess(AProcess).RegArrayLength);
end;

function TDbgRspThread.ResetInstructionPointerAfterBreakpoint: boolean;
begin
  if not ReadThreadState then
    exit(False);
  result := true;
  if FDidResetInstructionPointer then
    exit;
  FDidResetInstructionPointer := True;
end;

procedure TDbgRspThread.ApplyWatchPoints(AWatchPointData: TFpWatchPointData);
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
      if not TDbgRspProcess(Process).RspConnection.SetBreakWatchPoint(addr, watchData.Kind) then
        DebugLn(DBG_WARNINGS, 'Failed to set watch point.', []);
    end
    else
      DebugLn(DBG_WARNINGS, 'Failed to read memory.', []);
  end;
end;

function TDbgRspThread.DetectHardwareWatchpoint: Pointer;
begin
  if TDbgRspProcess(Process).RspConnection.LastStatusEvent.stopReason in [srAnyWatchPoint, srReadWatchPoint, srWriteWatchPoint] then
  begin
    Result := TFpRspWatchPointData(TDbgRspProcess(Process).WatchPointData).FindOwner(TDbgRspProcess(Process).RspConnection.LastStatusEvent.watchPointAddress);
    TDbgRspProcess(Process).RspConnection.ResetStatusEvent;
  end
  else
    result := nil;
end;

procedure TDbgRspThread.BeforeContinue;
begin
  if not FIsPaused then
    exit;

  inherited;
  if FRegsChanged then
    SaveRegisterCache;
  InvalidateRegisters;
end;

procedure TDbgRspThread.SetRegisterValue(AName: string; AValue: QWord);
begin
  assert(true, 'TDbgRspThread.SetRegisterValue not implemented');
end;

procedure TDbgRspThread.StoreRegisters;
begin
  FStoredRegs.Initialized := FRegs.Initialized;
  FStoredRegs.regs := copy(FRegs.regs);
end;

procedure TDbgRspThread.RestoreRegisters;
begin
  FRegs.Initialized := FStoredRegs.Initialized;
  FRegs.regs := copy(FStoredRegs.regs);
  FRegsChanged := true;
end;

{ TDbgRspProcess }

procedure TDbgRspProcess.InitializeLoaders;
begin
  if LoaderList.Count = 0 then
    TDbgImageLoader.Create(Name).AddToLoaderList(LoaderList);
end;

procedure TDbgRspProcess.CreateRspConnection(AFileName: string);
begin
  self.FConnection := TRspConnection.Create(AFileName, self, FRemoteConfig);
end;

constructor TDbgRspProcess.Create(const AFileName: string;
  AnOsClasses: TOSDbgClasses; AMemManager: TFpDbgMemManager;
  AProcessConfig: TDbgProcessConfig);
begin
  if Assigned(AProcessConfig) and (AProcessConfig is TRemoteConfig) then
  begin
    FRemoteConfig := TRemoteConfig.Create;
    FRemoteConfig.Assign(AProcessConfig);
  end;

  inherited Create(AFileName, AnOsClasses, AMemManager);
end;

destructor TDbgRspProcess.Destroy;
begin
  if Assigned(FRemoteConfig) then
    FreeAndNil(FRemoteConfig);
  if Assigned(FConnection) then
    FreeAndNil(FConnection);
  inherited Destroy;
end;

function TDbgRspProcess.AttachToInstance(APid: Integer; out AnError: TFpError
  ): boolean;
begin
  result := false;
end;

function TDbgRspProcess.StartInstance(AParams, AnEnvironment: TStrings;
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

function TDbgRspProcess.ReadData(const AAdress: TDbgPtr;
  const ASize: Cardinal; out AData): Boolean;
begin
  if FIsTerminating or (TDbgRspProcess(Process).FStatus = SIGHUP) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspProcess.ReadData called while FIsTerminating is set.');
    Result := false;
    exit;
  end;

  result := RspConnection.ReadData(AAdress, ASize, AData);
  if Result then
    MaskBreakpointsInReadData(AAdress, ASize, AData);
end;

function TDbgRspProcess.WriteData(const AAdress: TDbgPtr;
  const ASize: Cardinal; const AData): Boolean;
begin
  if FIsTerminating or (TDbgRspProcess(Process).FStatus = SIGHUP) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspProcess.WriteData called while FIsTerminating is set.');
    Result := false;
    exit;
  end;

  result := RspConnection.WriteData(AAdress, ASize, AData);
end;

procedure TDbgRspProcess.TerminateProcess;
begin
  // Try to prevent access to the RSP socket after it has been closed
  if not (FIsTerminating or (TDbgRspProcess(Process).FStatus = SIGHUP)) then
  begin
    DebugLn(DBG_VERBOSE, 'Removing all break points');
    RemoveAllBreakPoints;
    DebugLn(DBG_VERBOSE, 'Sending kill command from TDbgRspProcess.TerminateProcess');
    RspConnection.Kill();
    FIsTerminating:=true;
  end;
end;

function TDbgRspProcess.Pause: boolean;
begin
  if FIsTerminating or (TDbgRspProcess(Process).FStatus = SIGHUP) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspProcess.Pause called while FIsTerminating is set.');
    Result := false;
    exit;
  end;

  // Target should automatically respond with T or S reply after processing the break
  result := true;
  if not PauseRequested then
  begin
    RspConnection.Break();
    PauseRequested := true;
    DebugLn(DBG_VERBOSE, 'TDbgRspProcess.Pause called.');
  end
  else
  begin
    result := true;
    DebugLn(DBG_WARNINGS, 'TDbgRspProcess.Pause called while PauseRequested is set.');
  end;
end;

function TDbgRspProcess.Detach(AProcess: TDbgProcess; AThread: TDbgThread): boolean;
begin
  RemoveAllBreakPoints;
  DebugLn(DBG_VERBOSE, 'Sending detach command from TDbgRspProcess.Detach');
  Result := RspConnection.Detach();
end;

function TDbgRspProcess.Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean;
var
  ThreadToContinue: TDbgRspThread;
  PC: word;
  s: string;
  tempState: integer;
  initRegs: TInitializedRegisters;
begin
  // Terminating process and all threads
  if FIsTerminating or (FStatus = SIGHUP) then
  begin
    AThread.BeforeContinue;
    TDbgRspThread(AThread).InvalidateRegisters;
    DebugLn(DBG_VERBOSE, 'TDbgRspProcess.Continue called while terminating.');

    TDbgRspThread(AThread).ResetPauseStates;
    if not FThreadMap.HasId(AThread.ID) then
      AThread.Free;
    exit;
  end;

  if TDbgRspThread(AThread).FIsPaused then  // in case of deInternal, it may not be paused and can be ignored
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
          result := RspConnection.SingleStep();
          TDbgRspThread(ThreadToContinue).ResetPauseStates; // So BeforeContinue will not run again
          ThreadToContinue.FIsPaused := True;
          if result then
          begin
            tempState := RspConnection.WaitForSignal(s);  // TODO: Update registers cache for this thread
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

  if TDbgRspThread(AThread).FIsPaused and SingleStep then  // in case of deInternal, it may not be paused and can be ignored
  if HasInsertedBreakInstructionAtLocation(AThread.GetInstructionPointerRegisterValue) then
  begin
    TempRemoveBreakInstructionCode(AThread.GetInstructionPointerRegisterValue);
    TDbgRspThread(AThread).FIsSteppingBreakPoint := True;
    AThread.BeforeContinue;
    result := RspConnection.SingleStep(); // TODO: pass thread ID once it is supported in RspConnection - also signals not yet passed through
    TDbgRspThread(AThread).ResetPauseStates;
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
      RspConnection.Continue();
      ThreadToContinue.ResetPauseStates;
    end;
  end;

  if TDbgRspThread(AThread).FIsPaused then  // in case of deInternal, it may not be paused and can be ignored
    if not FIsTerminating then
    begin
      AThread.BeforeContinue;
      if SingleStep then
        result := RspConnection.SingleStep()
      else
        result := RspConnection.Continue();
      TDbgRspThread(AThread).ResetPauseStates;
      FStatus := 0;  // should update status by calling WaitForSignal
    end;

  if not FThreadMap.HasId(AThread.ID) then
    AThread.Free;
end;

function TDbgRspProcess.WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean;
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
        FStatus := RspConnection.WaitForSignal(s); // TODO: Update registers cache
        sleep(1);
      except
        FStatus := 0;
      end;
    until FStatus <> 0;

  if FStatus in [SIGINT, SIGTRAP] then
    RestoreTempBreakInstructionCodes;

  result := FStatus <> 0;
end;

function TDbgRspProcess.InsertBreakInstructionCode(const ALocation: TDBGPtr;
  out OrigValue: Byte; AMakeTempRemoved: Boolean): Boolean;
begin
  if FIsTerminating or (FStatus = SIGHUP) then
    DebugLn(DBG_WARNINGS, 'TDbgRspProcess.InsertBreakInstruction called while FIsTerminating is set.');

  // Todo: This does not respect a break instruction larger than 1 byte.
  // Fix: Use target specific break information in parent class.
  result := ReadData(ALocation, SizeOf(OrigValue), OrigValue);
  if AMakeTempRemoved then
    exit;

  // Insert HW break...
  result := RspConnection.SetBreakWatchPoint(ALocation, wkpExec);
  if not result then
    DebugLn(DBG_WARNINGS, 'Failed to set break point.', []);
end;

function TDbgRspProcess.RemoveBreakInstructionCode(const ALocation: TDBGPtr;
  const OrigValue: Byte): Boolean;
begin
  if FIsTerminating or (FStatus = SIGHUP) then
  begin
    DebugLn(DBG_WARNINGS, 'TDbgRspProcess.RemoveBreakInstructionCode called while FIsTerminating is set');
    result := false;
  end
  else
    result := RspConnection.DeleteBreakWatchPoint(ALocation, wkpExec);
end;

function TDbgRspProcess.AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent;
var
  ThreadToPause: TDbgRspThread;
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

  TDbgRspThread(AThread).FExceptionSignal:=0;
  TDbgRspThread(AThread).FIsPaused := True;

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
    TDbgRspThread(AThread).ReadThreadState;

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
        else if TDbgRspThread(AThread).FInternalPauseRequested then
        begin
          DebugLn(DBG_VERBOSE, ['???Received late SigTrap for thread ', AThread.ID]);
          result := deBreakpoint;
        end
        else
        begin
          DebugLn(DBG_VERBOSE, ['Received SigTrap for thread ', AThread.ID,
             ' PauseRequest=', PauseRequested]);
          result := deBreakpoint;

          if not TDbgRspThread(AThread).FIsSteppingBreakPoint then
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
            TDbgRspThread(AThread).FExceptionSignal:=SIGINT;
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
            TDbgRspThread(AThread).FExceptionSignal:=SIGKILL;
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
        TDbgRspThread(AThread).FExceptionSignal := FStatus;
        result := deException;
      end;
    end; {case}
    if result=deException then
      ExceptionClass:='External: '+ExceptionClass;
  end;

  debugln(DBG_VERBOSE, ['Leaving AnalyseDebugEvent, result = ', result]);

  TDbgRspThread(AThread).FIsSteppingBreakPoint := False;

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

end.
