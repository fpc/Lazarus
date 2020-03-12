(*  This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2, 3 or any later version
    of the License (at your option).

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)
unit FpLldbDebugger;

{$mode objfpc}{$H+}

{$IFdef MSWindows}
{//$DEFINE  WithWinMemReader}
{$ENDIF}

interface

uses
  {$IFdef WithWinMemReader}
  windows,
  {$ENDIF}
  Classes, sysutils, math, FpdMemoryTools, FpDbgInfo, LldbDebugger,
  LldbInstructions, LldbHelper, DbgIntfBaseTypes, DbgIntfDebuggerBase, LCLProc,
  Forms, FpDbgLoader, FpDbgDwarf, LazLoggerBase, LazClasses, FpPascalParser,
  FpPascalBuilder, FpErrorMessages, FpDbgDwarfDataClasses, FpDbgDwarfFreePascal,
  FpDbgCommon;

type

  TFpLldbDebugger = class;

  //TLldbDebuggerCommandMemReader = class(TLldbDebuggerCommand)
  //end;

  { TFpLldbDbgMemReader }

  TFpLldbDbgMemReader = class(TFpDbgMemReaderBase)
  private
// TODO
    //FThreadId: Integer;
    //FStackFrame: Integer;
    FDebugger: TFpLldbDebugger;
    FEnabled: Boolean;
    //FCmd: TLldbDebuggerCommandMemReader;
  protected
    // TODO: needs to be handled by memory manager
    //FThreadId, FStackFrame: Integer;
    property Enabled: Boolean read FEnabled write FEnabled;
  public
    constructor Create(ADebugger: TFpLldbDebugger);
    destructor Destroy; override;
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
    function ReadMemoryEx({%H-}AnAddress, {%H-}AnAddressSpace:{%H-} TDbgPtr; ASize: {%H-}Cardinal; ADest: Pointer): Boolean; override;
    function ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr; AContext: TFpDbgAddressContext): Boolean; override;
    function RegisterSize({%H-}ARegNum: Cardinal): Integer; override;
  end;

  {$IFdef WithWinMemReader}
  { TFpLldbAndWin32DbgMemReader }

  TFpLldbAndWin32DbgMemReader = class(TFpLldbDbgMemReader)
  private
    hProcess: THandle;
  public
    destructor Destroy; override;
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
    //function ReadRegister(ARegNum: Integer; out AValue: TDbgPtr): Boolean; override;
    procedure OpenProcess(APid: Cardinal);
    procedure CloseProcess;
  end;
  {$EndIf}

  { TFpLldbDbgMemCacheManagerSimple }

  TFpLldbDbgMemCacheManagerSimple = class(TFpDbgMemCacheManagerSimple)
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer
      ): Boolean; override;
    procedure Clear;
  end;

  { TDwarfLoaderThread }

  TDwarfLoaderThread = class(TThread)
  private
    FFileName: String;
    FDebugger: TFpLldbDebugger;
    FImageLoaderList: TDbgImageLoaderList;
    FDwarfInfo: TFpDwarfInfo;
    FIsSuccess: Boolean;
    FMemReader: TFpLldbDbgMemReader;
    FMemManager: TFpDbgMemManager;
    FReaderErrors: String;
  public
    procedure Execute; override;
    constructor Create(AFileName: String; ADebugger: TFpLldbDebugger);
    procedure FreeDwarf;

    property ImageLoaderList: TDbgImageLoaderList read FImageLoaderList;
    property DwarfInfo: TFpDwarfInfo read FDwarfInfo;
    property MemReader: TFpLldbDbgMemReader read FMemReader;
    property MemManager: TFpDbgMemManager read FMemManager;
    property ReaderErrors: String read FReaderErrors;
    property IsSuccess: Boolean read FIsSuccess;
  end;

const
  MAX_CTX_CACHE = 30;

type
  { TFpLldbDebugger }

  TFpLldbDebugger = class(TLldbDebugger)
  private
    FWatchEvalList: TList;
    FImageLoaderList: TDbgImageLoaderList;
    FDwarfInfo: TFpDwarfInfo;
    FPrettyPrinter: TFpPascalPrettyPrinter;
    FMemReader: TFpLldbDbgMemReader;
    FMemManager: TFpDbgMemManager;
    FDwarfLoaderThread: TDwarfLoaderThread;
    // cache last context
    FLastContext: array [0..MAX_CTX_CACHE-1] of TFpDbgInfoContext;
    procedure DoBeginReceivingLines(Sender: TObject);
    procedure DoEndReceivingLines(Sender: TObject);
  protected
    procedure DoBeforeLaunch; override;
    procedure DoAfterLaunch(var LaunchWarnings: string); override;
    function CreateLineInfo: TDBGLineInfo; override;
    function  CreateWatches: TWatchesSupplier; override;
    function  CreateLocals: TLocalsSupplier; override;
    function CreateDisassembler: TDBGDisassembler; override;
    procedure DoState(const OldState: TDBGState); override;
    function  HasDwarf: Boolean;
    function LoadDwarf: String;
    procedure UnLoadDwarf;
    function  RequestCommand(const ACommand: TDBGCommand;
              const AParams: array of const;
              const ACallback: TMethod): Boolean; override;
    procedure QueueCommand(const ACommand: TLldbDebuggerCommand; ForceQueue: Boolean = False);

    procedure GetCurrentContext(out AThreadId, AStackFrame: Integer);
    function  GetLocationForContext(AThreadId, AStackFrame: Integer): TDBGPtr;
    function  GetInfoContextForContext(AThreadId, AStackFrame: Integer): TFpDbgInfoContext;
    {$IFdef WithWinMemReader}
    property TargetPID;
    {$EndIf}
    property DebugInstructionQueue;
    property MemReader: TFpLldbDbgMemReader read FMemReader;
  protected
    procedure DoWatchFreed(Sender: TObject);
    function EvaluateExpression(AWatchValue: TWatchValue;
                                AExpression: String;
                                out AResText: String;
                                out ATypeInfo: TDBGType;
                                EvalFlags: TDBGEvaluateFlags = []): Boolean;
    property CurrentThreadId;
    property CurrentStackFrame;
    property CommandQueue;
  public
    class function Caption: String; override;
    class function RequiredCompilerOpts(ATargetCPU, ATargetOS: String): TDebugCompilerRequirements; override;
  public
    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

var
  DBG_VERBOSE, DBG_ERRORS: PLazLoggerLogGroup;

type
  TFPLldbWatches = class;

  { TFpLldbDebuggerCommandEvaluate }

  TFpLldbDebuggerCommandEvaluate = class(TLldbDebuggerCommand)
  private
    FOwner: TFPLldbWatches;
  protected
    procedure DoExecute; override;
    procedure DoFree; override;
    procedure DoCancel; override;
  public
    constructor Create(AOwner: TFPLldbWatches);
  end;

  { TFPLldbWatches }

  TFPLldbWatches = class(TWatchesSupplier)
  private
    FWatchEvalLock: Integer;
    FEvaluationCmdObj: TFpLldbDebuggerCommandEvaluate;
    FWatchEvalCancel: Boolean;
  protected
    function  FpDebugger: TFpLldbDebugger;
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure ProcessEvalList;
    procedure QueueCommand;
    procedure InternalRequestData(AWatchValue: TWatchValue); override;
  public
  end;

  TFPLldbLocals = class;

  { TFpLldbDebuggerCommandLocals }

  TFpLldbDebuggerCommandLocals = class(TLldbDebuggerCommand)
  private
    FOwner: TFPLldbLocals;
    FLocals: TLocals;
    procedure DoLocalsFreed(Sender: TObject);
  protected
    procedure DoExecute; override;
    procedure DoCancel; override;
  public
    constructor Create(AOwner: TFPLldbLocals; ALocals: TLocals);
  end;

  { TFPLldbLocals }

  TFPLldbLocals = class(TLocalsSupplier)
  private
    FLocalsEvalCancel: Boolean;
    procedure ProcessLocals(ALocals: TLocals);
  protected
    function  FpDebugger: TFpLldbDebugger;
    procedure DoStateChange(const AOldState: TDBGState); override;
  public
    procedure RequestData(ALocals: TLocals); override;
  end;

  { TFpLldbLineInfo }

  TFpLldbLineInfo = class(TDBGLineInfo)
  private
    FRequestedSources: TStringList;
  protected
    function  FpDebugger: TFpLldbDebugger;
    procedure DoStateChange(const {%H-}AOldState: TDBGState); override;
    procedure ClearSources;
  public
    constructor Create(const ADebugger: TDebuggerIntf);
    destructor Destroy; override;
    function Count: Integer; override;
    function HasAddress(const AIndex: Integer; const ALine: Integer): Boolean; override;
    function GetInfo(AAdress: TDbgPtr; out ASource, ALine, AOffset: Integer): Boolean; override;
    function IndexOf(const ASource: String): integer; override;
    procedure Request(const ASource: String); override;
    procedure Cancel(const ASource: String); override;
  end;

  TFpLldbDBGDisassembler = class;

  TFpLldbDebuggerCommandDisassemble = class(TLldbDebuggerCommand)
  private
    FOwner: TFpLldbDBGDisassembler;
    FAddr: TDBGPtr;
    FBeforeAddr: TDBGPtr;
    FLinesAfter: Cardinal;
  protected
    procedure InstrFinished(Sender: TObject);
    procedure CmdFinished(Sender: TObject);
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TFpLldbDBGDisassembler; AnAddr: TDBGPtr; ALinesBefore, ALinesAfter: Cardinal);
  end;


  { TLldbDBGDisassembler }

  TFpLldbDBGDisassembler = class(TDBGDisassembler)
  private
    FIsDisassembling: Boolean;
  protected
    function FpDebugger: TFpLldbDebugger;
    function PrepareEntries(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): boolean; override;
  public
    constructor Create(const ADebugger: TDebuggerIntf);
    procedure EntriesFinished;
    property EntryRanges;
  end;

{ TDwarfLoaderThread }

procedure TDwarfLoaderThread.Execute;
var
  AnImageLoader: TDbgImageLoader;
begin
  debugln(DBG_VERBOSE, ['THREAD TFpLldbDebugger.LoadDwarf ']);
  try
    AnImageLoader := TDbgImageLoader.Create(FFileName);
    if not AnImageLoader.IsValid then begin
      FreeAndNil(AnImageLoader);
      exit;
    end;
    if Terminated then
      exit;

    FImageLoaderList := TDbgImageLoaderList.Create(True);
    AnImageLoader.AddToLoaderList(FImageLoaderList);
    FReaderErrors := AnImageLoader.ReaderErrors;
    if Terminated then
      exit;

  {$IFdef WithWinMemReader}
    FMemReader := TFpLldbAndWin32DbgMemReader.Create(FDebugger);
  {$Else}
    FMemReader := TFpLldbDbgMemReader.Create(FDebugger);
  {$ENDIF}
    FMemManager := TFpDbgMemManager.Create(FMemReader, TFpDbgMemConvertorLittleEndian.Create);
    FMemManager.SetCacheManager(TFpLldbDbgMemCacheManagerSimple.Create);
    if Terminated then
      exit;


    FDwarfInfo := TFpDwarfInfo.Create(FImageLoaderList);
    FDwarfInfo.MemManager := FMemManager;
    if Terminated then
      exit;

    FDwarfInfo.LoadCompilationUnits;
    debugln(DBG_VERBOSE, ['finish THREAD TFpLldbDebugger.LoadDwarf ']);

    FIsSuccess := True;
  except
    on E: Exception do begin
      FReaderErrors := FReaderErrors + E.Message;
      debugln(DBG_ERRORS, ['LoaderThread failed ', E.ClassName, ', ', E.Message]);
    end;
  end;
end;

constructor TDwarfLoaderThread.Create(AFileName: String; ADebugger: TFpLldbDebugger);
begin
  FFileName := AFileName;
  FDebugger := ADebugger;
  inherited Create(False);
end;

procedure TDwarfLoaderThread.FreeDwarf;
begin
  FreeAndNil(FDwarfInfo);
  FreeAndNil(FImageLoaderList);
  FreeAndNil(FMemReader);
  if FMemManager <> nil then
    FMemManager.TargetMemConvertor.Free;
  FreeAndNil(FMemManager);
end;


{ TFpLldbDebuggerCommandLocals }

procedure TFpLldbDebuggerCommandLocals.DoLocalsFreed(Sender: TObject);
begin
  FLocals := nil;
end;

procedure TFpLldbDebuggerCommandLocals.DoExecute;
begin
  if FLocals <> nil then begin
    FOwner.ProcessLocals(FLocals);
    FLocals.RemoveFreeNotification(@DoLocalsFreed);
  end;
  if TFpLldbDebugger(Debugger).MemReader <> nil then
    TFpLldbDebugger(Debugger).MemReader.Enabled := True;
  Finished;
end;

procedure TFpLldbDebuggerCommandLocals.DoCancel;
begin
  inherited DoCancel;
  FOwner.FLocalsEvalCancel := True;
  if TFpLldbDebugger(Debugger).MemReader <> nil then
    TFpLldbDebugger(Debugger).MemReader.Enabled := False;
end;

constructor TFpLldbDebuggerCommandLocals.Create(AOwner: TFPLldbLocals; ALocals: TLocals);
begin
  inherited Create(AOwner.FpDebugger);
  FOwner := AOwner;
  FLocals := ALocals;
  FLocals.AddFreeNotification(@DoLocalsFreed);
  CancelableForRun := True;
//////  Priority := 1; // before watches
end;

{ TFPLldbLocals }

procedure TFPLldbLocals.ProcessLocals(ALocals: TLocals);
var
  Ctx: TFpDbgInfoContext;
  ProcVal: TFpValue;
  i: Integer;
  m: TFpValue;
  n, v: String;
begin
  if FLocalsEvalCancel then begin
    ALocals.SetDataValidity(ddsInvalid);
    exit;
  end;

  Ctx := FpDebugger.GetInfoContextForContext(ALocals.ThreadId, ALocals.StackFrame);
  try
    if (Ctx = nil) or (Ctx.SymbolAtAddress = nil) then begin
      ALocals.SetDataValidity(ddsInvalid);
      exit;
    end;

    ProcVal := Ctx.ProcedureAtAddress;

    if (ProcVal = nil) then begin
      ALocals.SetDataValidity(ddsInvalid);
      exit;
    end;
    FpDebugger.FPrettyPrinter.AddressSize := ctx.SizeOfAddress;
    FpDebugger.FPrettyPrinter.MemManager := ctx.MemManager;

    ALocals.Clear;
    for i := 0 to ProcVal.MemberCount - 1 do begin
      if FLocalsEvalCancel then begin
        ALocals.SetDataValidity(ddsInvalid);
        exit;
      end;
      m := ProcVal.Member[i];
      if m <> nil then begin
        if m.DbgSymbol <> nil then
          n := m.DbgSymbol.Name
        else
          n := '';
        FpDebugger.FPrettyPrinter.PrintValue(v, m);
        m.ReleaseReference;
        ALocals.Add(n, v);
      end;
    end;
    ALocals.SetDataValidity(ddsValid);
  finally
    Ctx.ReleaseReference;
    ProcVal.ReleaseReference;
  end;
end;

function TFPLldbLocals.FpDebugger: TFpLldbDebugger;
begin
  Result := TFpLldbDebugger(Debugger);
end;

procedure TFPLldbLocals.DoStateChange(const AOldState: TDBGState);
begin
  inherited DoStateChange(AOldState);
  FLocalsEvalCancel := False;
end;

procedure TFPLldbLocals.RequestData(ALocals: TLocals);
var
  LocalsCmdObj: TFpLldbDebuggerCommandLocals;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then begin
    Exit;
  end;

  FpDebugger.Threads.CurrentThreads.Count; // trigger threads, in case
  FpDebugger.Registers.CurrentRegistersList[FpDebugger.CurrentThreadId, FpDebugger.CurrentStackFrame].Count;

  // Join the queue, registers and threads are needed first
  LocalsCmdObj := TFpLldbDebuggerCommandLocals.Create(Self, ALocals);
//  LocalsCmdObj.Properties := [dcpCancelOnRun];
  // If a ExecCmd is running, then defer exec until the exec cmd is done
//  FpDebugger.QueueCommand(LocalsCmdObj, ForceQueuing);
  FpDebugger.QueueCommand(LocalsCmdObj);
  LocalsCmdObj.ReleaseReference;
end;

{ TFpLldbDebuggerCommandEvaluate }

procedure TFpLldbDebuggerCommandEvaluate.DoExecute;
begin
  FOwner.FEvaluationCmdObj := nil;
  FOwner.ProcessEvalList;
  Finished;
  if TFpLldbDebugger(Debugger).FMemReader <> nil then
    TFpLldbDebugger(Debugger).FMemReader.Enabled := True;
end;

procedure TFpLldbDebuggerCommandEvaluate.DoFree;
begin
  FOwner.FEvaluationCmdObj := nil;
  inherited DoFree;
end;

procedure TFpLldbDebuggerCommandEvaluate.DoCancel;
begin
  inherited DoCancel;
  FOwner.FWatchEvalCancel := True;
  if TFpLldbDebugger(Debugger).FMemReader <> nil then
    TFpLldbDebugger(Debugger).FMemReader.Enabled := False;
end;

//procedure TFpLldbDebuggerCommandEvaluate.DoCancel;
//begin
//  FOwner.FpDebugger.FWatchEvalList.Clear;
//  FOwner.FEvaluationCmdObj := nil;
//  inherited DoCancel;
//end;

constructor TFpLldbDebuggerCommandEvaluate.Create(AOwner: TFPLldbWatches);
begin
  inherited Create(AOwner.FpDebugger);
  FOwner := AOwner;
  CancelableForRun := True;
  //Priority := 0;
end;

{$IFdef WithWinMemReader}
{ TFpLldbAndWin32DbgMemReader }

destructor TFpLldbAndWin32DbgMemReader.Destroy;
begin
  CloseProcess;
  inherited Destroy;
end;

function TFpLldbAndWin32DbgMemReader.ReadMemory(AnAddress: TDbgPtr;
  ASize: Cardinal; ADest: Pointer): Boolean;
var
  BytesRead: SizeUInt;
begin
  {$IFdef MSWindows}
  Result := ReadProcessMemory(
    hProcess,
    Pointer(AnAddress),
    ADest, ASize,
    BytesRead) and
  (BytesRead = ASize);
//DebugLn(['*&*&*&*& ReadMem ', dbgs(Result), '  at ', AnAddress, ' Size ',ASize, ' br=',BytesRead, ' b1',PBYTE(ADest)^]);
  {$ELSE}
  Result := inherited ReadMemory(AnAddress, ASize, ADest);
  {$ENDIF}
end;

procedure TFpLldbAndWin32DbgMemReader.OpenProcess(APid: Cardinal);
begin
  {$IFdef MSWindows}
  debugln(DBG_VERBOSE, ['OPEN process ',APid]);
  if APid <> 0 then
    hProcess := windows.OpenProcess(PROCESS_CREATE_THREAD or PROCESS_QUERY_INFORMATION or PROCESS_VM_OPERATION or PROCESS_VM_WRITE or PROCESS_VM_READ, False, APid);
  {$ENDIF}
end;

procedure TFpLldbAndWin32DbgMemReader.CloseProcess;
begin
  {$IFdef MSWindows}
  if hProcess <> 0 then
    CloseHandle(hProcess);
  {$ENDIF}
end;
{$ENDIF}

{ TFpLldbDbgMemReader }

constructor TFpLldbDbgMemReader.Create(ADebugger: TFpLldbDebugger);
begin
  FDebugger := ADebugger;
  FEnabled := True;
  //FCmd := TLldbDebuggerCommandMemReader.Create(ADebugger);
end;

destructor TFpLldbDbgMemReader.Destroy;
begin
  //FCmd.ReleaseReference;
  inherited Destroy;
end;

function TFpLldbDbgMemReader.ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal;
  ADest: Pointer): Boolean;
var
  i: Integer;
  InStr: TLldbInstructionMemory;
begin
  Result := False;
  if not Enabled then
    Exit;
  InStr := TLldbInstructionMemory.Create(AnAddress, ASize);
  try
    FDebugger.DebugInstructionQueue.QueueInstruction(InStr);
    while not InStr.IsCompleted do begin
      Application.ProcessMessages;
      sleep(30);
    end;

    debugln(['TFpLldbDbgMemReader.ReadMemory  got mem ', AnAddress, ' ', ASize, ' ', Length(InStr.Res)]);

    if Length(InStr.Res) <> ASize then
      exit;

    for i := 0 to ASize - 1 do begin
      PByte(ADest + i)^ := InStr.Res[i];
    end;

  finally
    InStr.ReleaseReference;
  end;
  Result := True;
end;

function TFpLldbDbgMemReader.ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr;
  ASize: Cardinal; ADest: Pointer): Boolean;
begin
  Result := False;
end;

function TFpLldbDbgMemReader.ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr;
  AContext: TFpDbgAddressContext): Boolean;
var
  rname: String;
  v: String;
  i: Integer;
  Reg: TRegisters;
  RegVObj: TRegisterDisplayValue;
  CmdQueue: TLldbDebuggerCommandQueue;
  QItem: TLldbDebuggerCommand;
begin
  Result := False;
  if not Enabled then
    Exit;


  // WINDOWS gdb dwarf names
  if FDebugger.FDwarfInfo.TargetInfo.bitness = b64 then begin
    case ARegNum of
       0:  rname := 'RAX'; // RAX
       1:  rname := 'RDX'; // RDX
       2:  rname := 'RCX'; // RCX
       3:  rname := 'RBX'; // RBX
       4:  rname := 'RSI';
       5:  rname := 'RDI';
       6:  rname := 'RBP';
       7:  rname := 'RSP';
       8:  rname := 'R8'; // R8D , but gdb uses R8
       9:  rname := 'R9';
      10:  rname := 'R10';
      11:  rname := 'R11';
      12:  rname := 'R12';
      13:  rname := 'R13';
      14:  rname := 'R14';
      15:  rname := 'R15';
      16:  rname := 'RIP';
      else
        exit;
    end;
  end
  else begin
    case ARegNum of
       0:  rname := 'EAX'; // RAX
       1:  rname := 'ECX'; // RDX
       2:  rname := 'EDX'; // RCX
       3:  rname := 'EBX'; // RBX
       4:  rname := 'ESP';
       5:  rname := 'EBP';
       6:  rname := 'ESI';
       7:  rname := 'EDI';
       8:  rname := 'EIP';
      else
        exit;
    end;
  end;
  assert(AContext <> nil, 'TFpLldbDbgMemReader.ReadRegister: AContext <> nil');

  Reg := FDebugger.Registers.CurrentRegistersList[AContext.ThreadId, AContext.StackFrame];
  Reg.Count; // trigger
  if (reg.DataValidity = ddsRequested) then begin
    CmdQueue := FDebugger.CommandQueue;
    if CmdQueue.Count > 0 then begin;
      QItem := CmdQueue.Items[CmdQueue.Count - 1];
      if (QItem is TLldbDebuggerCommandRegister) and (TLldbDebuggerCommandRegister(QItem).Registers = Reg) then begin
        QItem.AddReference;
        CmdQueue.Delete(CmdQueue.Count - 1);
        QItem.Execute;
        while Reg.DataValidity = ddsRequested do begin
          Application.ProcessMessages;
          CheckSynchronize(25);
        end;
        QItem.ReleaseReference;
      end;
    end;
  end;

  if (reg.Count = 0) or (reg.DataValidity <> ddsValid) then begin
    DebugLn(DBG_VERBOSE, ['Cant get Registers for context ', AContext.ThreadId, ', ', AContext.StackFrame, ', ', dbgs(Reg.DataValidity), ' Reg:', rname]);
    exit;
  end;

  for i := 0 to Reg.Count - 1 do
    if UpperCase(Reg[i].Name) = rname then
      begin
        RegVObj := Reg[i].ValueObjFormat[rdDefault];
        if RegVObj <> nil then
          v := RegVObj.Value[rdDefault]
        else
          v := '';
        if pos(' ', v) > 1 then v := copy(v, 1, pos(' ', v)-1);
        Result := true;
        try
          AValue := StrToQWord(v);
        except
          Result := False;
        end;
        exit;
      end;
end;

function TFpLldbDbgMemReader.RegisterSize(ARegNum: Cardinal): Integer;
begin
  if FDebugger.FDwarfInfo.TargetInfo.bitness = b64 then
    Result := 8 // for the very few supported...
  else
    Result := 4; // for the very few supported...
end;

{ TFpLldbDbgMemCacheManagerSimple }

constructor TFpLldbDbgMemCacheManagerSimple.Create;
begin
  FList := TList.Create;
  inherited Create;
end;

destructor TFpLldbDbgMemCacheManagerSimple.Destroy;
begin
  Clear;
  inherited Destroy;
  FList.Free;
end;

function TFpLldbDbgMemCacheManagerSimple.ReadMemory(AnAddress: TDbgPtr;
  ASize: Cardinal; ADest: Pointer): Boolean;
var
  i: Integer;
begin
  i := -1;
  if not HasMemory(AnAddress, ASize) then
    i := FList.Add(AddCache(AnAddress, ASize));
  Result := inherited ReadMemory(AnAddress, ASize, ADest);

  // Only auto add caches, if success. May get a request for a subset later (pchar)
  if (not Result) and (i >= 0) then begin
    RemoveCache(TFpDbgMemCacheBase(FList[i]));
    FList.Delete(i);
  end;
end;

procedure TFpLldbDbgMemCacheManagerSimple.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    RemoveCache(TFpDbgMemCacheBase(FList[i]));
  FList.Clear;
end;

{ TFPLldbWatches }

function TFPLldbWatches.FpDebugger: TFpLldbDebugger;
begin
  Result := TFpLldbDebugger(Debugger);
end;

procedure TFPLldbWatches.DoStateChange(const AOldState: TDBGState);
begin
  inherited DoStateChange(AOldState);
  FWatchEvalCancel := False;
end;

procedure TFPLldbWatches.ProcessEvalList;
var
  WatchValue: TWatchValue;
  ResTypeInfo: TDBGType;
  ResText: String;

  function IsWatchValueAlive: Boolean;
  begin
    Result := (FpDebugger.FWatchEvalList.Count > 0) and (FpDebugger.FWatchEvalList[0] = Pointer(WatchValue));
  end;
begin
  if (FpDebugger.FWatchEvalList.Count = 0) or (FWatchEvalLock > 0) or FWatchEvalCancel then
    exit;

debugln(['ProcessEvalList ']);
  inc(FWatchEvalLock);
  try // TODO: if the stack/thread is changed, registers will be wrong
    while (FpDebugger.FWatchEvalList.Count > 0) and (FEvaluationCmdObj = nil) and (not FWatchEvalCancel)
    do begin
      WatchValue := TWatchValue(FpDebugger.FWatchEvalList[0]);
    if FpDebugger.Registers.CurrentRegistersList[WatchValue.ThreadId, WatchValue.StackFrame].Count = 0 then begin
      // trigger register
      FpDebugger.Registers.CurrentRegistersList[FpDebugger.CurrentThreadId, FpDebugger.CurrentStackFrame].Count;
      QueueCommand;
      exit;
    end;

      try
        ResTypeInfo := nil;
        if not FpDebugger.EvaluateExpression(WatchValue, WatchValue.Expression, ResText, ResTypeInfo)
        then begin
          if IsWatchValueAlive then    debugln(['TFPLldbWatches.InternalRequestData FAILED ', WatchValue.Expression]);
          if IsWatchValueAlive then
            inherited InternalRequestData(WatchValue);
        end;
      finally
        if IsWatchValueAlive then begin
          WatchValue.RemoveFreeNotification(@FpDebugger.DoWatchFreed);
          FpDebugger.FWatchEvalList.Remove(pointer(WatchValue));
        end;
        Application.ProcessMessages;
      end;
    end;
  finally
    dec(FWatchEvalLock);
  end;
end;

procedure TFPLldbWatches.QueueCommand;
begin
  FEvaluationCmdObj := TFpLldbDebuggerCommandEvaluate.Create(Self);
//  FEvaluationCmdObj.Properties := [dcpCancelOnRun];
  // If a ExecCmd is running, then defer exec until the exec cmd is done
  FpDebugger.QueueCommand(FEvaluationCmdObj);
  FEvaluationCmdObj.ReleaseReference;
end;

procedure TFPLldbWatches.InternalRequestData(AWatchValue: TWatchValue);
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then begin
    AWatchValue.Validity := ddsInvalid;
    Exit;
  end;

  AWatchValue.AddFreeNotification(@FpDebugger.DoWatchFreed); // we may call gdb
  FpDebugger.FWatchEvalList.Add(pointer(AWatchValue));

  if FEvaluationCmdObj <> nil then exit;

  FpDebugger.Threads.CurrentThreads.Count; // trigger threads, in case
  // TODO currentframe should not be needed?
  FpDebugger.Registers.CurrentRegistersList[FpDebugger.CurrentThreadId, FpDebugger.CurrentStackFrame].Count;  // trigger register, in case
  FpDebugger.Registers.CurrentRegistersList[AWatchValue.ThreadId, AWatchValue.StackFrame].Count; // trigger register, in case

  // Join the queue, registers and threads are needed first
  QueueCommand;
end;

{ TFpLldbLineInfo }

function TFpLldbLineInfo.FpDebugger: TFpLldbDebugger;
begin
  Result := TFpLldbDebugger(Debugger);
end;

procedure TFpLldbLineInfo.DoStateChange(const AOldState: TDBGState);
begin
  //inherited DoStateChange(AOldState);
  if not (Debugger.State in [dsPause, dsInternalPause, dsRun]) then
    ClearSources;
end;

procedure TFpLldbLineInfo.ClearSources;
begin
  FRequestedSources.Clear;
end;

constructor TFpLldbLineInfo.Create(const ADebugger: TDebuggerIntf);
begin
  FRequestedSources := TStringList.Create;
  inherited Create(ADebugger);
end;

destructor TFpLldbLineInfo.Destroy;
begin
  FreeAndNil(FRequestedSources);
  inherited Destroy;
end;

function TFpLldbLineInfo.Count: Integer;
begin
  Result := FRequestedSources.Count;
end;

function TFpLldbLineInfo.HasAddress(const AIndex: Integer; const ALine: Integer
  ): Boolean;
var
  Map: PDWarfLineMap;
  dummy: TDBGPtrArray;
begin
  Result := False;
  if not FpDebugger.HasDwarf then
    exit;
  //Result := FpDebugger.FDwarfInfo.GetLineAddress(FRequestedSources[AIndex], ALine);
  Map := PDWarfLineMap(FRequestedSources.Objects[AIndex]);
  if Map <> nil then
    Result := Map^.GetAddressesForLine(ALine, dummy, True);
end;

function TFpLldbLineInfo.GetInfo(AAdress: TDbgPtr; out ASource, ALine,
  AOffset: Integer): Boolean;
begin
  Result := False;
  //ASource := '';
  //ALine := 0;
  //if not FpDebugger.HasDwarf then
  //  exit(nil);
  //FpDebugger.FDwarfInfo.
end;

function TFpLldbLineInfo.IndexOf(const ASource: String): integer;
begin
  Result := FRequestedSources.IndexOf(ASource);
end;

procedure TFpLldbLineInfo.Request(const ASource: String);
begin
  if not FpDebugger.HasDwarf then
    exit;
  FRequestedSources.AddObject(ASource, TObject(FpDebugger.FDwarfInfo.GetLineAddressMap(ASource)));
  DoChange(ASource);
end;

procedure TFpLldbLineInfo.Cancel(const ASource: String);
begin
  //
end;

{ TFpLldbDebuggerCommandDissassemble }

constructor TFpLldbDebuggerCommandDisassemble.Create(AOwner: TFpLldbDBGDisassembler; AnAddr: TDBGPtr; ALinesBefore, ALinesAfter: Cardinal);
begin
  inherited Create(AOwner.FpDebugger);
  FOwner := AOwner;
  FAddr := AnAddr;
  FBeforeAddr := FAddr - Min(ALinesBefore * DAssBytesPerCommandAvg, DAssMaxRangeSize);
  FLinesAfter := ALinesAfter;
  CancelableForRun := True;
end;

procedure TFpLldbDebuggerCommandDisassemble.CmdFinished(Sender: TObject);
begin
  InstrFinished(Sender);
  Fowner.EntriesFinished;
  Finished;
end;


procedure TFpLldbDebuggerCommandDisassemble.InstrFinished(Sender: TObject);
var
  Instr: TLldbInstructionDisassem absolute Sender;
  Range: TDBGDisassemblerEntryRange;
  AnEntry: TDisassemblerEntry;
  SrcFileName, LineAddrStr: String;
  i,j, StatIndex, FirstIndex, SrcFileLine: Integer;
  Sym: TFpSymbol;
  ALastAddr, LineAddr: TDBGPtr;
begin
  StatIndex := 0;
  FirstIndex := 0;
  SrcFileLine := 0;
  SrcFileName := '';
  Sym:=nil;
  ALastAddr:=0;
  Range := TDBGDisassemblerEntryRange.Create;

  i := 0;
  while (i < Instr.Res.Count) do begin
    if AnsiStrPos(PAnsiChar(Instr.Res[i]), '0x') = nil then begin
      Instr.Res.Delete(i)
      end
    else
      Inc(i)
    end;

  For i := 0 to Instr.Res.Count - 1 do begin

    SScanf(Instr.Res[i], '%s0xsx %s', [@LineAddrStr]);
    if (LineAddrStr <>  '') and (LineAddrStr[Length(LineAddrStr)] = ':') then
      Delete(LineAddrStr, Length(LineAddrStr), 1);
    LineAddr := StrToInt64(LineAddrStr);
    if i = 0 then
      Range.RangeStartAddr :=  LineAddr;
    Sym :=  FOwner.FpDebugger.FDwarfInfo.FindProcSymbol(LineAddr);

    // If this is the last statement for this source-code-line, fill the
    // SrcStatementCount from the prior statements.
    if (assigned(Sym) and ((SrcFileName<>Sym.FileName) or (SrcFileLine<>Sym.Line))) or
       (not assigned(Sym) and ((SrcFileLine<>0) or (SrcFileName<>''))) then begin
      for j := 0 to StatIndex-1 do
        Range.EntriesPtr[FirstIndex+j]^.SrcStatementCount := StatIndex;
      StatIndex := 0;
      FirstIndex := i;
      end;

    if assigned(Sym) then  begin
      SrcFileName := Sym.FileName;
      SrcFileLine := Sym.Line;
      Sym.ReleaseReference;
      end
    else begin
      SrcFileName:='';
      SrcFileLine:=0;
      end;

    AnEntry.Addr := LineAddr;
    AnEntry.Dump := '';
    AnEntry.Statement := AnsiStrPos(PAnsiChar(Instr.Res[i]), ':');
    AnEntry.SrcFileLine := SrcFileLine;
    AnEntry.SrcFileName := SrcFileName;
    AnEntry.SrcStatementIndex := StatIndex;
    Range.Append(@AnEntry);

    Inc(StatIndex);
    ALastAddr:=LineAddr;
  end;

  if Range.Count>0 then begin
    Range.RangeEndAddr := ALastAddr;
    Range.LastEntryEndAddr := ALastAddr;
    FOwner.EntryRanges.AddRange(Range);
    end
  else
    Range.Free;
end;


procedure TFpLldbDebuggerCommandDisassemble.DoExecute;
var
  DInstr: TLldbInstructionDisassem;
  Sym: TFpSymbol;
  StartRange, EndRange: TDBGPtr;
begin

  Sym := FOwner.FpDebugger.FDwarfInfo.FindProcSymbol(FBeforeAddr);
  if Sym <> nil then
    StartRange := Sym.Address.Address
  else
    StartRange := FBeforeAddr;

  EndRange := FAddr + $20; // Make sure ranges overlap;

  DInstr := TLldbInstructionDisassem.CreateRange(StartRange, EndRange);
  DInstr.OnFinish := @InstrFinished;
  QueueInstruction(DInstr);
  DInstr.ReleaseReference;

  // Make sure FlinesAfter returned is slightly bigger than requested
  // so that FindRange() will return true
  DInstr := TLldbInstructionDisassem.Create(FAddr, FLinesAfter + 5);
  DInstr.OnFinish := @CmdFinished;
  QueueInstruction(DInstr);
  DInstr.ReleaseReference;
end;



{ TFpLldbDBGDisassembler }

constructor TFpLldbDBGDisassembler.Create(const ADebugger: TDebuggerIntf);
begin
  inherited Create(ADebugger);
  FIsDisassembling := False;
end;

procedure TFpLldbDBGDisassembler.EntriesFinished;
begin
  OnChange(self);
  FIsDisassembling := False;
end;

function TFpLldbDBGDisassembler.FpDebugger: TFpLldbDebugger;
begin
  Result := TFpLldbDebugger(Debugger);
end;

function TFpLldbDBGDisassembler.PrepareEntries(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): boolean;
var
  cmd: TFpLldbDebuggerCommandDisassemble;
begin
  Result := False;
  if (Debugger = nil) or not(Debugger.State = dsPause) or FIsDisassembling or
     (TFpLldbDebugger(Debugger).FDwarfInfo = nil)
  then
    exit;
  FIsDisassembling := True;
  cmd := TFpLldbDebuggerCommandDisassemble.Create(self, AnAddr, ALinesBefore, ALinesAfter);
  TFpLldbDebugger(Debugger).QueueCommand(cmd);
  cmd.ReleaseReference;
  Result := True;
end;

{ TFpLldbDebugger }

procedure TFpLldbDebugger.DoState(const OldState: TDBGState);
var
  i: Integer;
begin
  if FMemReader <> nil then
    FMemReader.Enabled := True;
  inherited DoState(OldState);
  if State in [dsStop, dsError, dsNone] then
    UnLoadDwarf;

  if OldState in [dsPause, dsInternalPause] then begin
    for i := 0 to MAX_CTX_CACHE-1 do
      ReleaseRefAndNil(FLastContext[i]);
    if not(State in [dsPause, dsInternalPause]) then begin
      for i := 0 to FWatchEvalList.Count - 1 do begin
        TWatchValue(FWatchEvalList[i]).RemoveFreeNotification(@DoWatchFreed);
        //TWatchValueBase(FWatchEvalList[i]).Validity := ddsInvalid;
      end;
      FWatchEvalList.Clear;
    end;
  end;
  if (State = dsRun) and (FMemManager <> nil) then
    TFpLldbDbgMemCacheManagerSimple(FMemManager.CacheManager).Clear;
end;

function TFpLldbDebugger.HasDwarf: Boolean;
begin
  Result := FDwarfInfo <> nil;
end;

function TFpLldbDebugger.LoadDwarf: String;
var
  AnImageLoader: TDbgImageLoader;
  Loader: TDwarfLoaderThread;
begin
  Result := ''; // no errors/warnings
  Loader := FDwarfLoaderThread;
  FDwarfLoaderThread := nil;

  UnLoadDwarf;

  if Loader <> nil then begin
    debugln(DBG_VERBOSE, ['Getting dwarf from FDwarfLoaderThread ']);
    Loader.WaitFor;
    Result := Loader.ReaderErrors;
    if not Loader.IsSuccess then begin
      Result := 'Failed loading Dwarf debug info';
      Loader.FreeDwarf;
      Loader.Free;
      exit;
    end;

    FImageLoaderList := Loader.ImageLoaderList;
    FMemReader := Loader.MemReader;
    FMemManager := Loader.MemManager;
    FDwarfInfo := Loader.DwarfInfo;
    Loader.Free;

    if FDwarfInfo.TargetInfo.bitness = b64 then
      FPrettyPrinter := TFpPascalPrettyPrinter.Create(8)
    else
      FPrettyPrinter := TFpPascalPrettyPrinter.Create(4);
    exit;
  end;

  debugln(DBG_VERBOSE, ['TFpLldbDebugger.LoadDwarf ']);
  try
    AnImageLoader := TDbgImageLoader.Create(FileName);
    if not AnImageLoader.IsValid then begin
      FreeAndNil(AnImageLoader);
      exit;
    end;
    FImageLoaderList := TDbgImageLoaderList.Create(True);
    AnImageLoader.AddToLoaderList(FImageLoaderList);
    Result := AnImageLoader.ReaderErrors;
  {$IFdef WithWinMemReader}
    FMemReader := TFpLldbAndWin32DbgMemReader.Create(Self);
  {$Else}
    FMemReader := TFpLldbDbgMemReader.Create(Self);
  {$ENDIF}
    FMemManager := TFpDbgMemManager.Create(FMemReader, TFpDbgMemConvertorLittleEndian.Create);
    FMemManager.SetCacheManager(TFpLldbDbgMemCacheManagerSimple.Create);

    FDwarfInfo := TFpDwarfInfo.Create(FImageLoaderList);
    FDwarfInfo.MemManager := FMemManager;
    FDwarfInfo.LoadCompilationUnits;

    if FDwarfInfo.TargetInfo.bitness = b64 then
      FPrettyPrinter := TFpPascalPrettyPrinter.Create(8)
    else
      FPrettyPrinter := TFpPascalPrettyPrinter.Create(4);
  except
    on E: Exception do begin
      Result := Result + E.Message;
      debugln(DBG_ERRORS, ['LoaderThread failed ', E.ClassName, ', ', E.Message]);
      UnLoadDwarf;
    end;
  end;
end;

procedure TFpLldbDebugger.UnLoadDwarf;
begin
  debugln(DBG_VERBOSE, ['TFpLldbDebugger.UnLoadDwarf ']);
  FreeAndNil(FDwarfInfo);
  FreeAndNil(FImageLoaderList);
  FreeAndNil(FMemReader);
  if FMemManager <> nil then
    FMemManager.TargetMemConvertor.Free;
  FreeAndNil(FMemManager);
  FreeAndNil(FPrettyPrinter);

  if FDwarfLoaderThread <> nil then begin
    debugln(DBG_VERBOSE, ['Terminate FDwarfLoaderThread ']);
    FDwarfLoaderThread.Terminate;
    FDwarfLoaderThread.WaitFor; // This may take a while, but normally the thread should never exist in UnLoadDwarf
    FDwarfLoaderThread.FreeDwarf;
    FreeAndNil(FDwarfLoaderThread);
  end;
end;

function TFpLldbDebugger.RequestCommand(const ACommand: TDBGCommand;
  const AParams: array of const; const ACallback: TMethod): Boolean;
var
  EvalFlags: TDBGEvaluateFlags;
  ResText: String;
  ResType: TDBGType;
begin
  if (ACommand = dcEvaluate) then begin
    EvalFlags := [];
    EvalFlags := TDBGEvaluateFlags(AParams[1].VInteger);
    Result := False;
    if (HasDwarf) then begin
      Threads.CurrentThreads.Count; // trigger threads, in case
      if Registers.CurrentRegistersList[CurrentThreadId, CurrentStackFrame].Count = 0 then begin  // trigger register, in case
        Registers.CurrentRegistersList[CurrentThreadId, CurrentStackFrame].Count;
        while DebugInstructionQueue.RunningInstruction <> nil do begin // TODO: use a callback
          Application.ProcessMessages;
          sleep(30);
        end;
      end;
      Result := EvaluateExpression(nil, String(AParams[0].VAnsiString),
        ResText, ResType, EvalFlags);
      if EvalFlags * [defNoTypeInfo, defSimpleTypeInfo, defFullTypeInfo] = [defNoTypeInfo]
      then FreeAndNil(ResType);
      TDBGEvaluateResultCallback(ACallback)(Self, Result, ResText, ResType);
      Result := True;
    end;
  end
  else
    Result := inherited RequestCommand(ACommand, AParams, ACallback);
end;

procedure TFpLldbDebugger.QueueCommand(const ACommand: TLldbDebuggerCommand;
  ForceQueue: Boolean);
begin
  inherited QueueCommand(ACommand); //, ForceQueue);
end;

procedure TFpLldbDebugger.GetCurrentContext(out AThreadId, AStackFrame: Integer);
begin
  AThreadId := CurrentThreadId;
  AStackFrame := CurrentStackFrame
//  if CurrentThreadIdValid then begin
//    AThreadId := CurrentThreadId;
//
//    if CurrentStackFrameValid then
//      AStackFrame := CurrentStackFrame
//    else
//      AStackFrame := 0;
//  end
//  else begin
//    AThreadId := 1;
//    AStackFrame := 0;
//  end;
end;

function TFpLldbDebugger.GetLocationForContext(AThreadId, AStackFrame: Integer): TDBGPtr;
var
  s: TCallStackBase;
  f: TCallStackEntry;
  r: TRegisters;
  v: string;
  //Instr: TLldbDebuggerInstruction;
begin
(*
  Instr := TLldbDebuggerInstruction.Create(Format('-stack-list-frames %d %d', [AStackFrame, AStackFrame]), AThreadId, [], 0);
  Instr.AddReference;
  Instr.Cmd := TLldbDebuggerCommand.Create(Self);
  FTheDebugger.FInstructionQueue.RunInstruction(Instr);
  ok := Instr.IsSuccess and Instr.FHasResult;
  AResult := Instr.ResultData;
  Instr.Cmd.ReleaseReference;
  Instr.Cmd := nil;
  Instr.ReleaseReference;

  if ok then begin
    List := TLldbNameValueList.Create(R, ['stack']);
    Result := List.Values['frame'];
    List.Free;
  end;
*)


  Result := 0;
  if (AThreadId <= 0) then begin
    GetCurrentContext(AThreadId, AStackFrame);
  end
  else
  if (AStackFrame < 0) then begin
    AStackFrame := 0;
  end;

//  t := Threads.CurrentThreads.EntryById[AThreadId];
//  if t = nil then begin
//    DebugLn(DBG_ERRORS, ['NO Threads']);
//    exit;
//  end;

  r := Registers.CurrentRegistersList[AThreadId, AStackFrame];
  if (r <> nil) and (r.DataValidity = ddsValid) then begin
    try
      if TargetWidth = 64 then
        v := r.EntriesByName['RIP'].ValueObjFormat[rdDefault].Value[rdDefault]
      else
        v := r.EntriesByName['EIP'].ValueObjFormat[rdDefault].Value[rdDefault];
      if pos(' ', v) > 1 then v := copy(v, 1, pos(' ', v)-1);
      Result := StrToQWord(v);
      exit;
    except
    end;
  end;

  s := CallStack.CurrentCallStackList.EntriesForThreads[AThreadId];
  if (s = nil) or (AStackFrame >= s.Count) then begin
    DebugLn(DBG_ERRORS, ['NO Stackframe list for thread']);
    exit;
  end;
  f := s.Entries[AStackFrame];
  if f = nil then begin
    DebugLn(DBG_ERRORS, ['NO Stackframe']);
    exit;
  end;

  Result := f.Address;
  //DebugLn(['Returning addr from frame', dbgs(Result)]);

end;

function TFpLldbDebugger.GetInfoContextForContext(AThreadId,
  AStackFrame: Integer): TFpDbgInfoContext;
var
  Addr: TDBGPtr;
  i: Integer;
begin
  Result := nil;
  if FDwarfInfo = nil then
    exit;

  if (AThreadId <= 0) then begin
    GetCurrentContext(AThreadId, AStackFrame);
  end;

  Addr := GetLocationForContext(AThreadId, AStackFrame);

  if Addr = 0 then begin
    debugln(DBG_VERBOSE, ['GetInfoContextForContext ADDR NOT FOUND for ', AThreadId, ', ', AStackFrame]);
    Result := nil;
    exit;
  end;

  i := MAX_CTX_CACHE - 1;
  while (i >= 0) and
    ( (FLastContext[i] = nil) or
      (FLastContext[i].ThreadId <> AThreadId) or (FLastContext[i].StackFrame <> AStackFrame)
    )
  do
    dec(i);

  if i >= 0 then begin
    Result := FLastContext[i];
    Result.AddReference;
    exit;
  end;

  DebugLn(DBG_VERBOSE, ['* FDwarfInfo.FindContext ', dbgs(Addr)]);
  Result := FDwarfInfo.FindContext(AThreadId, AStackFrame, Addr);

  if Result = nil then begin
    debugln(DBG_VERBOSE, ['GetInfoContextForContext CTX NOT FOUND for ', AThreadId, ', ', AStackFrame]);
    exit;
  end;

  ReleaseRefAndNil(FLastContext[MAX_CTX_CACHE-1]);
  move(FLastContext[0], FLastContext[1], (MAX_CTX_CACHE-1) + SizeOf(FLastContext[0]));
  FLastContext[0] := Result;
  Result.AddReference;
end;

procedure TFpLldbDebugger.DoWatchFreed(Sender: TObject);
begin
  FWatchEvalList.Remove(pointer(Sender));
end;

function TFpLldbDebugger.EvaluateExpression(AWatchValue: TWatchValue; AExpression: String;
  out AResText: String; out ATypeInfo: TDBGType; EvalFlags: TDBGEvaluateFlags): Boolean;
var
  Ctx: TFpDbgInfoContext;
  PasExpr, PasExpr2: TFpPascalExpression;
  ResValue: TFpValue;
  s: String;
  DispFormat: TWatchDisplayFormat;
  RepeatCnt: Integer;
  TiSym: TFpSymbol;

  function IsWatchValueAlive: Boolean;
  begin
    Result := (State in [dsPause, dsInternalPause]) and
              (  (AWatchValue = nil) or
                 ( (FWatchEvalList.Count > 0) and (FWatchEvalList[0] = Pointer(AWatchValue)) )
              );
  end;

var
  CastName: String;
  ClassAddr, CNameAddr: TFpDbgMemLocation;
  NameLen: QWord;
begin
  Result := False;

  ATypeInfo := nil;
  AResText := '';
  if AWatchValue <> nil then begin
    EvalFlags := AWatchValue.EvaluateFlags;
    AExpression := AWatchValue.Expression;
    //FMemReader.FThreadId := AWatchValue.ThreadId;
    //FMemReader.FStackFrame := AWatchValue.StackFrame;
  //end
  //else begin
  //  FMemReader.FThreadId := CurrentThreadId;
  //  FMemReader.FStackFrame := CurrentStackFrame;
  end;

  if AWatchValue <> nil then begin
    Ctx := GetInfoContextForContext(AWatchValue.ThreadId, AWatchValue.StackFrame);
    DispFormat := AWatchValue.DisplayFormat;
    RepeatCnt := AWatchValue.RepeatCount;
  end
  else begin
    Ctx := GetInfoContextForContext(CurrentThreadId, CurrentStackFrame);
    DispFormat := wdfDefault;
    RepeatCnt := -1;
  end;
  if Ctx = nil then exit;
debugln(['TFpLldbDebugger.EvaluateExpression ctx ', Ctx.Address]);

  FMemManager.DefaultContext := Ctx;
  FPrettyPrinter.AddressSize := ctx.SizeOfAddress;
  FPrettyPrinter.MemManager := ctx.MemManager;

  PasExpr := TFpPascalExpression.Create(AExpression, Ctx);
  try
    if not IsWatchValueAlive then exit;
    PasExpr.ResultValue; // trigger evaluate // and check errors
    if not IsWatchValueAlive then exit;

    if not PasExpr.Valid then begin
DebugLn(DBG_VERBOSE, [ErrorHandler.ErrorAsString(PasExpr.Error)]);
      if ErrorCode(PasExpr.Error) <> fpErrAnyError then begin
        Result := True;
        AResText := ErrorHandler.ErrorAsString(PasExpr.Error);;
        if AWatchValue <> nil then begin;
          AWatchValue.Value    := AResText;
          AWatchValue.Validity := ddsError;
        end;
        exit;
      end;
    end;

    if not (PasExpr.Valid and (PasExpr.ResultValue <> nil)) then
      exit; // TODO handle error
    if not IsWatchValueAlive then exit;

    ResValue := PasExpr.ResultValue;

    if (ResValue.Kind = skClass) and (ResValue.AsCardinal <> 0) and (defClassAutoCast in EvalFlags)
    then begin
      CastName := '';
      if FMemManager.ReadAddress(ResValue.DataAddress, SizeVal(Ctx.SizeOfAddress), ClassAddr) then begin
        ClassAddr.Address := ClassAddr.Address + 3 * Ctx.SizeOfAddress;
        if FMemManager.ReadAddress(ClassAddr, SizeVal(Ctx.SizeOfAddress), CNameAddr) then begin
          if (FMemManager.ReadUnsignedInt(CNameAddr, SizeVal(1), NameLen)) then
            if NameLen > 0 then begin
              SetLength(CastName, NameLen);
              CNameAddr.Address := CNameAddr.Address + 1;
              FMemManager.ReadMemory(CNameAddr, SizeVal(NameLen), @CastName[1]);
              PasExpr2 := TFpPascalExpression.Create(CastName+'('+AExpression+')', Ctx);
              PasExpr2.ResultValue;
              if PasExpr2.Valid then begin
                PasExpr.Free;
                PasExpr := PasExpr2;
                ResValue := PasExpr.ResultValue;
              end
              else
                PasExpr2.Free;
            end;
        end;
      end;
    end;

    case ResValue.Kind of
      skNone: begin
          // maybe type
          TiSym := ResValue.DbgSymbol;
          if (TiSym <> nil) and (TiSym.SymbolType = stType) then begin
            if GetTypeAsDeclaration(AResText, TiSym) then
              AResText := Format('{Type=} %1s', [AResText])
            else
            if GetTypeName(AResText, TiSym) then
              AResText := Format('{Type=} %1s', [AResText])
            else
              AResText := '{Type=} unknown';
            Result := True;
            if AWatchValue <> nil then begin
              if not IsWatchValueAlive then exit;
              AWatchValue.Value    := AResText;
              AWatchValue.Validity := ddsValid; // TODO ddsError ?
            end;
            exit;
          end;
        end;
      else
      begin
        if defNoTypeInfo in EvalFlags then
          FPrettyPrinter.PrintValue(AResText, ResValue, DispFormat, RepeatCnt)
        else
          FPrettyPrinter.PrintValue(AResText, ATypeInfo, ResValue, DispFormat, RepeatCnt);
      end;
    end;
    if not IsWatchValueAlive then exit;

    if PasExpr.HasPCharIndexAccess and not IsError(ResValue.LastError) then begin
      // TODO: Only dwarf 2
      PasExpr.FixPCharIndexAccess := True;
      PasExpr.ResetEvaluation;
      ResValue := PasExpr.ResultValue;
      if (ResValue=nil) or (not FPrettyPrinter.PrintValue(s, ResValue, DispFormat, RepeatCnt)) then
        s := 'Failed';
      AResText := 'PChar: '+AResText+ LineEnding + 'String: '+s;
    end
    else
    if CastName <> '' then
      AResText := CastName + AResText;

    if (ATypeInfo <> nil) or (AResText <> '') then begin
      Result := True;
      debugln(DBG_VERBOSE, ['TFPLldbWatches.InternalRequestData   GOOOOOOD ', AExpression]);
      if AWatchValue <> nil then begin
        AWatchValue.Value    := AResText;
        AWatchValue.TypeInfo := ATypeInfo;
        if IsError(ResValue.LastError) then
          AWatchValue.Validity := ddsError
        else
          AWatchValue.Validity := ddsValid;
      end;
    end;

  finally
    PasExpr.Free;
    FMemManager.DefaultContext := nil;
    Ctx.ReleaseReference;
  end;
end;

procedure TFpLldbDebugger.DoBeginReceivingLines(Sender: TObject);
begin
  inherited DoBeginReceivingLines(Sender);
  CommandQueue.LockQueueRun;
end;

procedure TFpLldbDebugger.DoEndReceivingLines(Sender: TObject);
begin
  CommandQueue.UnLockQueueRun;
  inherited DoEndReceivingLines(Sender);
end;

procedure TFpLldbDebugger.DoBeforeLaunch;
begin
  inherited DoBeforeLaunch;
  if FDwarfInfo = nil then begin
    FDwarfLoaderThread := TDwarfLoaderThread.Create(FileName, Self);
  end;
end;

procedure TFpLldbDebugger.DoAfterLaunch(var LaunchWarnings: string);
begin
  inherited DoAfterLaunch(LaunchWarnings);
  LaunchWarnings := LaunchWarnings + LoadDwarf;
  {$IFdef WithWinMemReader}
  TFpLldbAndWin32DbgMemReader(FMemReader).OpenProcess(TargetPid);
  {$ENDIF}
end;

function TFpLldbDebugger.CreateLineInfo: TDBGLineInfo;
begin
  Result := TFpLldbLineInfo.Create(Self);
end;

function TFpLldbDebugger.CreateWatches: TWatchesSupplier;
begin
  Result := TFPLldbWatches.Create(Self);
end;

function TFpLldbDebugger.CreateLocals: TLocalsSupplier;
begin
  Result := TFPLldbLocals.Create(Self);
end;

function TFpLldbDebugger.CreateDisassembler: TDBGDisassembler;
begin
  Result:=TFpLldbDBGDisassembler.Create(Self);
end;

class function TFpLldbDebugger.Caption: String;
begin
  Result := 'LLDB debugger (with fpdebug) (Beta)';
end;

class function TFpLldbDebugger.RequiredCompilerOpts(ATargetCPU, ATargetOS: String): TDebugCompilerRequirements;
begin
  {$ifdef CD_Cocoa}
  Result:=[dcrDwarfOnly];
  {$ELSE}
  {$IFDEF DARWIN}
  Result:=[dcrDwarfOnly];
  {$ELSE}
  Result:=[dcrNoExternalDbgInfo, dcrDwarfOnly];
  {$ENDIF}
  {$ENDIF}
end;

constructor TFpLldbDebugger.Create(const AExternalDebugger: String);
begin
  FWatchEvalList := TList.Create;
  inherited Create(AExternalDebugger);
  DebugInstructionQueue.OnBeginLinesReceived := @DoBeginReceivingLines;
  DebugInstructionQueue.OnEndLinesReceived := @DoEndReceivingLines;
end;

destructor TFpLldbDebugger.Destroy;
begin
  UnLoadDwarf;
  inherited Destroy;
  FWatchEvalList.Free;
end;

procedure Register;
begin
  RegisterDebugger(TFpLldbDebugger);
end;

initialization
  DBG_VERBOSE       := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_ERRORS         := DebugLogger.FindOrRegisterLogGroup('DBG_ERRORS' {$IFDEF DBG_ERRORS} , True {$ENDIF} );
end.

