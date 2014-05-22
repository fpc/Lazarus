unit FpDebugDebugger;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils, fgl,
  Forms,
  Maps,
  process,
  LazLogger,
  FpDbgClasses,
  FpDbgInfo,
  FpErrorMessages,
  FpPascalBuilder,
  DbgIntfBaseTypes,
  DbgIntfDebuggerBase,
  FpPascalParser,
  FPDbgController, FpDbgDwarfDataClasses;

type

  { TFpDebugThread }
  TFpDebugDebugger = class;
  TFpDebugThread = class(TThread)
  private
    FDebugLoopStoppedEvent: PRTLEvent;
    FFpDebugDebugger: TFpDebugDebugger;
    FStartDebugLoopEvent: PRTLEvent;
    FStartSuccesfull: boolean;
    procedure DoDebugLoopFinishedASync({%H-}Data: PtrInt);
  public
    constructor Create(AFpDebugDebugger: TFpDebugDebugger);
    destructor Destroy; override;
    procedure Execute; override;
    property StartSuccesfull: boolean read FStartSuccesfull;
    property StartDebugLoopEvent: PRTLEvent read FStartDebugLoopEvent;
    property DebugLoopStoppedEvent: PRTLEvent read FDebugLoopStoppedEvent;
  end;

  { TFpDebugDebugger }

  TFpDebugDebugger = class(TDebuggerIntf)
  private
    FWatchEvalList: TList; // Schedule
    FWatchAsyncQueued: Boolean;
    FPrettyPrinter: TFpPascalPrettyPrinter;
    FDbgController: TDbgController;
    FFpDebugThread: TFpDebugThread;
    FQuickPause: boolean;
    FRaiseExceptionBreakpoint: FpDbgClasses.TDBGBreakPoint;
    function GetClassInstanceName(AnAddr: TDBGPtr): string;
    function ReadAnsiString(AnAddr: TDbgPtr): string;
    function SetSoftwareExceptionBreakpoint: boolean;
    procedure HandleSoftwareException(var AnExceptionLocation: TDBGLocationRec; var continue: boolean);
    procedure FreeDebugThread;
    procedure FDbgControllerHitBreakpointEvent(var continue: boolean; const Breakpoint: FpDbgClasses.TDbgBreakpoint);
    procedure FDbgControllerCreateProcessEvent(var continue: boolean);
    procedure FDbgControllerProcessExitEvent(AExitCode: DWord);
    procedure FDbgControllerExceptionEvent(var continue: boolean; const ExceptionClass, ExceptionMessage: string);
    procedure FDbgControllerDebugInfoLoaded(Sender: TObject);
    function GetDebugInfo: TDbgInfo;
    procedure DoWatchFreed(Sender: TObject);
    procedure ProcessASyncWatches(Data: PtrInt);
  protected
    procedure ScheduleWatchValueEval(AWatchValue: TWatchValue);
    function EvaluateExpression(AWatchValue: TWatchValue;
                                AExpression: String;
                                out AResText: String;
                                out ATypeInfo: TDBGType;
                                EvalFlags: TDBGEvaluateFlags = []): Boolean;

    function CreateLineInfo: TDBGLineInfo; override;
    function CreateWatches: TWatchesSupplier; override;
    function CreateLocals: TLocalsSupplier; override;
    function  CreateRegisters: TRegisterSupplier; override;
    function CreateCallStack: TCallStackSupplier; override;
    function CreateDisassembler: TDBGDisassembler; override;
    function CreateBreakPoints: TDBGBreakPoints; override;
    function  RequestCommand(const ACommand: TDBGCommand;
                             const AParams: array of const): Boolean; override;
    function ChangeFileName: Boolean; override;

    procedure OnLog(AString: String);
    procedure StartDebugLoop;
    procedure DebugLoopFinished;
    procedure QuickPause;
    procedure DoState(const OldState: TDBGState); override;

    property DebugInfo: TDbgInfo read GetDebugInfo;
  public
    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;
    function GetLocationRec(AnAddress: TDBGPtr=0): TDBGLocationRec;
    function GetLocation: TDBGLocationRec; override;
    class function Caption: String; override;
    class function HasExePath: boolean; override;
    function  GetSupportedCommands: TDBGCommands; override;
  end;

  { TFpLineInfo }

  TFpLineInfo = class(TDBGLineInfo) //class(TGDBMILineInfo)
  private
    FRequestedSources: TStringList;
  protected
    function  FpDebugger: TFpDebugDebugger;
    procedure DoStateChange(const {%H-}AOldState: TDBGState); override;
    procedure ClearSources;
    procedure DebugInfoChanged;
  public
    constructor Create(const ADebugger: TDebuggerIntf);
    destructor Destroy; override;
    function Count: Integer; override;
    function GetAddress(const AIndex: Integer; const ALine: Integer): TDbgPtr; override;
    function GetInfo(AAdress: TDbgPtr; out ASource, ALine, AOffset: Integer): Boolean; override;
    function IndexOf(const ASource: String): integer; override;
    procedure Request(const ASource: String); override;
    procedure Cancel(const ASource: String); override;
  end;

  { TFPWatches }

  TFPWatches = class(TWatchesSupplier)
  protected
    function  FpDebugger: TFpDebugDebugger;
    //procedure DoStateChange(const AOldState: TDBGState); override;
    procedure InternalRequestData(AWatchValue: TWatchValue); override;
  public
  end;

  { TFPCallStackSupplier }

  TFPCallStackSupplier = class(TCallStackSupplier)
  protected
    procedure DoStateLeavePause; override;
  public
    procedure RequestCount(ACallstack: TCallStackBase); override;
    procedure RequestEntries(ACallstack: TCallStackBase); override;
    procedure RequestCurrent(ACallstack: TCallStackBase); override;
    procedure UpdateCurrentIndex; override;
  end;

  { TFPLocals }

  TFPLocals = class(TLocalsSupplier)
  private
    FPrettyPrinter: TFpPascalPrettyPrinter;
  protected
    function  FpDebugger: TFpDebugDebugger;
  public
    procedure RequestData(ALocals: TLocals); override;
    constructor Create(const ADebugger: TDebuggerIntf);
    destructor Destroy; override;
  end;

  { TFPRegisters }

  TFPRegisters = class(TRegisterSupplier)
  public
    procedure RequestData(ARegisters: TRegisters); override;
  end;

  { TFPDBGDisassembler }

  TFPDBGDisassembler = class(TDBGDisassembler)
  protected
    function PrepareEntries(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): boolean; override;
  end;

  { TFPBreakpoint }

  TFPBreakpoint = class(TDBGBreakPoint)
  private
    FSetBreakFlag: boolean;
    FResetBreakFlag: boolean;
    FInternalBreakpoint: FpDbgClasses.TDbgBreakpoint;
    FIsSet: boolean;
    procedure SetBreak;
    procedure ResetBreak;
  protected
    destructor Destroy; override;
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure DoEnableChange; override;
    procedure DoChanged; override;
  public
  end;

  { TFPBreakpoints }

  TFPBreakpoints = class(TDBGBreakPoints)
  public
    function Find(AIntBReakpoint: FpDbgClasses.TDbgBreakpoint): TDBGBreakPoint;
  end;

procedure Register;

implementation

uses
  FpDbgUtil,
  FpDbgDisasX86;

procedure Register;
begin
  RegisterDebugger(TFpDebugDebugger);
end;

{ TFPCallStackSupplier }

procedure TFPCallStackSupplier.DoStateLeavePause;
begin
  if (TFpDebugDebugger(Debugger).FDbgController <> nil) and
     (TFpDebugDebugger(Debugger).FDbgController.CurrentProcess <> nil) and
     (TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.MainThread <> nil)
  then
    TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.MainThread.ClearCallStack;
  inherited DoStateLeavePause;
end;

procedure TFPCallStackSupplier.RequestCount(ACallstack: TCallStackBase);
var
  Address, Frame, LastFrame: QWord;
  Size, Count: integer;
  ThreadCallStack: TDbgCallstackEntryList;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause])
  then begin
    ACallstack.SetCountValidity(ddsInvalid);
    exit;
  end;
  TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.MainThread.PrepareCallStackEntryList;
  ThreadCallStack := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.MainThread.CallStackEntryList;

  if ThreadCallStack.Count = 0 then
  begin
    ACallstack.SetCountValidity(ddsInvalid);
    ACallstack.SetHasAtLeastCountInfo(ddsInvalid);
  end
  else
  begin
    ACallstack.Count := ThreadCallStack.Count;
    ACallstack.SetCountValidity(ddsValid);
  end;
end;

procedure TFPCallStackSupplier.RequestEntries(ACallstack: TCallStackBase);
var
  e: TCallStackEntry;
  It: TMapIterator;
  ThreadCallStack: TDbgCallstackEntryList;
begin
  It := TMapIterator.Create(ACallstack.RawEntries);
  //TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.MainThread.PrepareCallStackEntryList;
  ThreadCallStack := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.MainThread.CallStackEntryList;

  if not It.Locate(ACallstack.LowestUnknown )
  then if not It.EOM
  then It.Next;

  while (not IT.EOM) and (TCallStackEntry(It.DataPtr^).Index < ACallstack.HighestUnknown)
  do begin
    e := TCallStackEntry(It.DataPtr^);
    if e.Validity = ddsRequested then
    begin
      e.Init(ThreadCallStack[e.Index].AnAddress, nil,
        ThreadCallStack[e.Index].FunctionName, ThreadCallStack[e.Index].SourceFile,
        '', ThreadCallStack[e.Index].Line, ddsValid);
    end;
    It.Next;
  end;
  It.Free;
end;

procedure TFPCallStackSupplier.RequestCurrent(ACallstack: TCallStackBase);
begin
  ACallstack.CurrentIndex := 0;
  ACallstack.SetCurrentValidity(ddsValid);
end;

procedure TFPCallStackSupplier.UpdateCurrentIndex;
var
  tid, idx: Integer;
  cs: TCallStackBase;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then begin
    exit;
  end;

  tid := Debugger.Threads.CurrentThreads.CurrentThreadId;
  cs := TCallStackBase(CurrentCallStackList.EntriesForThreads[tid]);
  idx := cs.NewCurrentIndex;  // NEW-CURRENT

  if cs <> nil then begin
    cs.CurrentIndex := idx;
    cs.SetCurrentValidity(ddsValid);
  end;
end;

{ TFPLocals }

function TFPLocals.FpDebugger: TFpDebugDebugger;
begin
  Result := TFpDebugDebugger(Debugger);
end;

procedure TFPLocals.RequestData(ALocals: TLocals);
var
  AContext: TFpDbgInfoContext;
  AController: TDbgController;
  ProcVal: TFpDbgValue;
  i: Integer;
  m: TFpDbgValue;
  n, v: String;
  CurThreadId, CurStackFrame: Integer;
  AFrame: TDbgCallstackEntry;
  RegList: TDbgRegisterValueList;
  Reg: TDbgRegisterValue;
  CurStackList: TCallStackBase;
begin
  AController := FpDebugger.FDbgController;
  if (AController = nil) or (AController.CurrentProcess = nil) or
     (AController.CurrentProcess.DbgInfo = nil)
  then begin
    ALocals.SetDataValidity(ddsInvalid);
    exit;
  end;

  CurThreadId := Debugger.Threads.CurrentThreads.CurrentThreadId;
  CurStackList := Debugger.CallStack.CurrentCallStackList.EntriesForThreads[CurThreadId];
  if CurStackList <> nil then
    CurStackFrame := Debugger.CallStack.CurrentCallStackList.EntriesForThreads[CurThreadId].CurrentIndex
  else
    CurStackFrame := 0;

  if CurStackFrame > 0 then
    begin
    AController.CurrentProcess.MainThread.PrepareCallStackEntryList(CurStackFrame);
    AFrame := AController.CurrentProcess.MainThread.CallStackEntryList[CurStackFrame];
    if AFrame = nil then
      begin
      ALocals.SetDataValidity(ddsInvalid);
      exit;
      end;
    RegList := AFrame.RegisterValueList;
    end
  else
    RegList := AController.CurrentProcess.MainThread.RegisterValueList;
  Reg := RegList.FindRegisterByDwarfIndex(8);
  if Reg <> nil then
    AContext := AController.CurrentProcess.DbgInfo.FindContext(CurThreadId, CurStackFrame, Reg.NumValue)
  else
    AContext := nil;

  if (AContext = nil) or (AContext.SymbolAtAddress = nil) then begin
    ALocals.SetDataValidity(ddsInvalid);
    AContext.ReleaseReference;
    exit;
  end;

  ProcVal := AContext.ProcedureAtAddress;

  if (ProcVal = nil) then begin
    ALocals.SetDataValidity(ddsInvalid);
    AContext.ReleaseReference;
    exit;
  end;
  FPrettyPrinter.AddressSize := AContext.SizeOfAddress;

  ALocals.Clear;
  for i := 0 to ProcVal.MemberCount - 1 do begin
    m := ProcVal.Member[i];
    if m <> nil then begin
      if m.DbgSymbol <> nil then
        n := m.DbgSymbol.Name
      else
        n := '';
      FPrettyPrinter.PrintValue(v, m);
      ALocals.Add(n, v);
    end;
  end;
  ALocals.SetDataValidity(ddsValid);
  AContext.ReleaseReference;
end;

constructor TFPLocals.Create(const ADebugger: TDebuggerIntf);
begin
  inherited Create(ADebugger);
  FPrettyPrinter := TFpPascalPrettyPrinter.Create(sizeof(pointer));
end;

destructor TFPLocals.Destroy;
begin
  inherited Destroy;
  FPrettyPrinter.Free;
end;

{ TFPBreakpoints }

function TFPBreakpoints.Find(AIntBReakpoint: FpDbgClasses.TDbgBreakpoint): TDBGBreakPoint;
var
  i: integer;
begin
  for i := 0 to count-1 do
    if TFPBreakpoint(Items[i]).FInternalBreakpoint=AIntBReakpoint then
      begin
      result := TFPBreakpoint(Items[i]);
      Exit;
      end;
  result := nil;
end;

{ TFPBreakpoint }

procedure TFPBreakpoint.SetBreak;
begin
  assert(FInternalBreakpoint=nil);
  case Kind of
    bpkAddress:   FInternalBreakpoint := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.AddBreak(Address);
    bpkSource:    FInternalBreakpoint := TDbgInstance(TFpDebugDebugger(Debugger).FDbgController.CurrentProcess).AddBreak(Source, cardinal(Line));
  else
    Raise Exception.Create('Breakpoints of this kind are not suported.');
  end;
  FIsSet:=true;
  if not assigned(FInternalBreakpoint) then
    FValid:=vsInvalid
  else
    FValid:=vsValid;
end;

procedure TFPBreakpoint.ResetBreak;
begin
  // If Debugger is not assigned, the Controller's currentprocess is already
  // freed. And so are the corresponding InternalBreakpoint's.
  if assigned(Debugger) and assigned(FInternalBreakpoint) then
    begin
    TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.RemoveBreak(FInternalBreakpoint.Location);
    FreeAndNil(FInternalBreakpoint);
    end;
  FIsSet:=false;
end;

destructor TFPBreakpoint.Destroy;
begin
  ResetBreak;
  inherited Destroy;
end;

procedure TFPBreakpoint.DoStateChange(const AOldState: TDBGState);
begin
  if (Debugger.State = dsPause) then
    begin
    if Enabled and not FIsSet then
      begin
      FSetBreakFlag:=true;
      Changed;
      end
    else if not enabled and FIsSet then
      begin
      FResetBreakFlag:=true;
      Changed;
      end;
    end
  else if Debugger.State = dsStop then
    begin
    FInternalBreakpoint := nil;
    FIsSet:=false;
    end;
  inherited DoStateChange(AOldState);
end;

procedure TFPBreakpoint.DoEnableChange;
var
  ADebugger: TFpDebugDebugger;
begin
  ADebugger := TFpDebugDebugger(Debugger);
  if (ADebugger.State in [dsPause, dsInit]) then
    begin
    if FEnabled and not FIsSet then
      FSetBreakFlag := True
    else if not FEnabled and FIsSet then
      FResetBreakFlag := True;
    end
  else if (ADebugger.State = dsRun) and ((FEnabled and not FIsSet) or (not FEnabled and FIsSet)) then
    ADebugger.QuickPause;
  inherited;
end;

procedure TFPBreakpoint.DoChanged;
begin
  if FResetBreakFlag and not FSetBreakFlag then
    ResetBreak
  else if FSetBreakFlag then
    SetBreak;

  FSetBreakFlag := false;
  FResetBreakFlag := false;

  inherited DoChanged;
end;

{ TFPDBGDisassembler }

function TFPDBGDisassembler.PrepareEntries(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): boolean;
var
  ARange: TDBGDisassemblerEntryRange;
  AnEntry: PDisassemblerEntry;
  CodeBin: array[0..20] of byte;
  p: pointer;
  ADump,
  AStatement,
  ASrcFileName,
  APriorFileName: string;
  ASrcFileLine,
  APriorFileLine: integer;
  i,j: Integer;
  NextSym,Sym: TFpDbgSymbol;
  StatIndex: integer;
  FirstIndex: integer;

begin
  Result := False;
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then
    exit;

  AnEntry:=nil;
  Sym:=nil;
  ASrcFileLine:=0;
  ASrcFileName:='';
  StatIndex:=0;
  FirstIndex:=0;
  ARange := TDBGDisassemblerEntryRange.Create;
  ARange.RangeStartAddr:=AnAddr;

  for i := 0 to ALinesAfter-1 do
    begin
    if not TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.ReadData(AnAddr,sizeof(CodeBin),CodeBin) then
      begin
      DebugLn(Format('Disassemble: Failed to read memory at %s.', [FormatAddress(AnAddr)]));
      inc(AnAddr);
      end
    else
      begin
      p := @CodeBin;
      FpDbgDisasX86.Disassemble(p, GMode=dm64, ADump, AStatement);

      Sym := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.FindSymbol(AnAddr);

      // If this is the last statement for this source-code-line, fill the
      // SrcStatementCount from the prior statements.
      if (assigned(sym) and ((ASrcFileName<>sym.FileName) or (ASrcFileLine<>sym.Line))) or
        (not assigned(sym) and ((ASrcFileLine<>0) or (ASrcFileName<>''))) then
        begin
        for j := 0 to StatIndex-1 do
          ARange.EntriesPtr[FirstIndex+j]^.SrcStatementCount:=StatIndex;
        StatIndex:=0;
        FirstIndex:=i;
        end;

      if assigned(sym) then
        begin
        ASrcFileName:=sym.FileName;
        ASrcFileLine:=sym.Line;
        end
      else
        begin
        ASrcFileName:='';
        ASrcFileLine:=0;
        end;
      new(AnEntry);
      AnEntry^.Addr := AnAddr;
      AnEntry^.Dump := ADump;
      AnEntry^.Statement := AStatement;
      AnEntry^.SrcFileLine:=ASrcFileLine;
      AnEntry^.SrcFileName:=ASrcFileName;
      AnEntry^.SrcStatementIndex:=StatIndex;
      ARange.Append(AnEntry);
      inc(StatIndex);
      Inc(AnAddr, PtrUInt(p) - PtrUInt(@CodeBin));
      end;
    end;

  if assigned(AnEntry) then
    begin
    ARange.RangeEndAddr:=AnEntry^.Addr;
    ARange.LastEntryEndAddr:=TDBGPtr(p);
    EntryRanges.AddRange(ARange);
    result := true;
    end
  else
    begin
    result := false;
    ARange.Free;
    end;
end;

{ TFPRegisters }

procedure TFPRegisters.RequestData(ARegisters: TRegisters);
var
  ARegisterList: TDbgRegisterValueList;
  i: Integer;
  ARegisterValue: TRegisterValue;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsStop]) then
    exit;

  ARegisterList := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.MainThread.RegisterValueList;
  for i := 0 to ARegisterList.Count-1 do
    begin
    ARegisterValue := ARegisters.EntriesByName[ARegisterList[i].Name];
    ARegisterValue.ValueObj.SetAsNum(ARegisterList[i].NumValue, ARegisterList[i].Size);
    ARegisterValue.ValueObj.SetAsText(ARegisterList[i].StrValue);
    ARegisterValue.DataValidity:=ddsValid;
    end;
  ARegisters.DataValidity:=ddsValid;
end;

{ TFpLineInfo }

function TFpLineInfo.FpDebugger: TFpDebugDebugger;
begin
  Result := TFpDebugDebugger(Debugger);
end;

procedure TFpLineInfo.DoStateChange(const AOldState: TDBGState);
begin
  //inherited DoStateChange(AOldState);
  if not (Debugger.State in [dsPause, dsInternalPause, dsRun]) then
    ClearSources;
end;

procedure TFpLineInfo.ClearSources;
begin
  FRequestedSources.Clear;
end;

procedure TFpLineInfo.DebugInfoChanged;
var
  i: Integer;
  Src: String;
begin
  if (FpDebugger.DebugInfo = nil) or not(FpDebugger.DebugInfo is TFpDwarfInfo) then
    exit;

  for i := 0 to FRequestedSources.Count - 1 do begin
    if FRequestedSources.Objects[i] = nil then begin
      Src := FRequestedSources[i];
      FRequestedSources.Objects[i] := TObject(TFpDwarfInfo(FpDebugger.DebugInfo).GetLineAddressMap(Src));
      if FRequestedSources.Objects[i] <> nil then
        DoChange(Src);
    end;
  end;
end;

constructor TFpLineInfo.Create(const ADebugger: TDebuggerIntf);
begin
  FRequestedSources := TStringList.Create;
  inherited Create(ADebugger);
end;

destructor TFpLineInfo.Destroy;
begin
  FreeAndNil(FRequestedSources);
  inherited Destroy;
end;

function TFpLineInfo.Count: Integer;
begin
  Result := FRequestedSources.Count;
end;

function TFpLineInfo.GetAddress(const AIndex: Integer; const ALine: Integer): TDbgPtr;
var
  Map: PDWarfLineMap;
begin
  Result := 0;
  if not((FpDebugger.DebugInfo <> nil) and (FpDebugger.DebugInfo is TFpDwarfInfo)) then
    exit;
  Map := PDWarfLineMap(FRequestedSources.Objects[AIndex]);
  if Map <> nil then
    Result := Map^.GetAddressForLine(ALine);
end;

function TFpLineInfo.GetInfo(AAdress: TDbgPtr; out ASource, ALine,
  AOffset: Integer): Boolean;
begin
  Result := False;
end;

function TFpLineInfo.IndexOf(const ASource: String): integer;
begin
  Result := FRequestedSources.IndexOf(ASource);
end;

procedure TFpLineInfo.Request(const ASource: String);
var
  i: Integer;
begin
  if (FpDebugger.DebugInfo = nil) or not(FpDebugger.DebugInfo is TFpDwarfInfo) then begin
    FRequestedSources.AddObject(ASource, nil);
    exit;
  end;
  i := FRequestedSources.AddObject(ASource, TObject(TFpDwarfInfo(FpDebugger.DebugInfo).GetLineAddressMap(ASource)));
  if FRequestedSources.Objects[i] <> nil then
    DoChange(ASource);
end;

procedure TFpLineInfo.Cancel(const ASource: String);
begin
  //
end;

{ TFPWatches }

function TFPWatches.FpDebugger: TFpDebugDebugger;
begin
  Result := TFpDebugDebugger(Debugger);
end;

procedure TFPWatches.InternalRequestData(AWatchValue: TWatchValue);
//var
//  AVal: string;
//  AType: TDBGType;
begin
  FpDebugger.ScheduleWatchValueEval(AWatchValue);
  //FpDebugger.EvaluateExpression(AWatchValue, AWatchValue.Expression, AVal, AType);
end;

{ TFpDebugThread }

procedure TFpDebugThread.DoDebugLoopFinishedASync(Data: PtrInt);
begin
  FFpDebugDebugger.DebugLoopFinished;
end;

constructor TFpDebugThread.Create(AFpDebugDebugger: TFpDebugDebugger);
begin
  FDebugLoopStoppedEvent := RTLEventCreate;
  FStartDebugLoopEvent := RTLEventCreate;
  FFpDebugDebugger := AFpDebugDebugger;
  inherited Create(false);
end;

destructor TFpDebugThread.Destroy;
begin
  RTLeventdestroy(FStartDebugLoopEvent);
  RTLeventdestroy(FDebugLoopStoppedEvent);
  inherited Destroy;
end;

procedure TFpDebugThread.Execute;
begin
  if FFpDebugDebugger.FDbgController.Run then
    FStartSuccesfull:=true;

  RTLeventSetEvent(FDebugLoopStoppedEvent);

  if FStartSuccesfull then
    begin
    repeat
    RTLeventWaitFor(FStartDebugLoopEvent);
    RTLeventResetEvent(FStartDebugLoopEvent);
    if not terminated then
      begin
      FFpDebugDebugger.FDbgController.ProcessLoop;
      Application.QueueAsyncCall(@DoDebugLoopFinishedASync, 0);
      end;
    until Terminated;
    end
end;

{ TFpDebugDebugger }

procedure TFpDebugDebugger.FDbgControllerProcessExitEvent(AExitCode: DWord);
begin
  SetExitCode(Integer(AExitCode));
  {$PUSH}{$R-}
  DoDbgEvent(ecProcess, etProcessExit, Format('Process exited with exit-code %d',[AExitCode]));
  {$POP}
  SetState(dsStop);
  FreeDebugThread;
end;

procedure TFpDebugDebugger.FDbgControllerExceptionEvent(var continue: boolean;
  const ExceptionClass, ExceptionMessage: string);
begin
  DoException(deExternal, ExceptionClass, GetLocation, ExceptionMessage, continue);
  if not continue then
    begin
    SetState(dsPause);
    DoCurrent(GetLocation);
    end;
end;

function TFpDebugDebugger.GetDebugInfo: TDbgInfo;
begin
  Result := nil;
  if (FDbgController <> nil) and (FDbgController.CurrentProcess<> nil) then
    Result := FDbgController.CurrentProcess.DbgInfo;
end;

procedure TFpDebugDebugger.ScheduleWatchValueEval(AWatchValue: TWatchValue);
begin
  AWatchValue.AddFreeNotification(@DoWatchFreed);
  FWatchEvalList.Add(pointer(AWatchValue));
  if not FWatchAsyncQueued then
    Application.QueueAsyncCall(@ProcessASyncWatches, 0);
  FWatchAsyncQueued := True;
end;

function TFpDebugDebugger.EvaluateExpression(AWatchValue: TWatchValue; AExpression: String;
  out AResText: String; out ATypeInfo: TDBGType; EvalFlags: TDBGEvaluateFlags): Boolean;
var
  AContext: TFpDbgInfoContext;
  AController: TDbgController;
  APasExpr: TFpPascalExpression;
  ADbgInfo: TDbgInfo;
  DispFormat: TWatchDisplayFormat;
  RepeatCnt: Integer;
  Res: Boolean;
  AFrame: TDbgCallstackEntry;
  StackFrame, ThreadId: Integer;
  RegList: TDbgRegisterValueList;
  Reg: TDbgRegisterValue;
  StackList: TCallStackBase;
begin
  Result := False;
  AResText := '';
  ATypeInfo := nil;

  AController := FDbgController;
  ADbgInfo := AController.CurrentProcess.DbgInfo;

  if AWatchValue <> nil then begin
    StackFrame := AWatchValue.StackFrame;
    ThreadId := AWatchValue.ThreadId;
    DispFormat := AWatchValue.DisplayFormat;
    RepeatCnt := AWatchValue.RepeatCount;
  end
  else begin
    ThreadId := Threads.CurrentThreads.CurrentThreadId;
    StackList := CallStack.CurrentCallStackList.EntriesForThreads[ThreadId];
    if StackList <> nil then
      StackFrame := CallStack.CurrentCallStackList.EntriesForThreads[ThreadId].CurrentIndex
    else
      StackFrame := 0;
    DispFormat := wdfDefault;
    RepeatCnt := -1;
  end;

  if StackFrame > 0 then
    begin
    FDbgController.CurrentProcess.MainThread.PrepareCallStackEntryList(StackFrame);
    AFrame := FDbgController.CurrentProcess.MainThread.CallStackEntryList[StackFrame];
    if AFrame = nil then
      begin
      if AWatchValue <> nil then
        AWatchValue.Validity := ddsInvalid;
      exit;
      end;
    RegList := AFrame.RegisterValueList;
    end
  else
    RegList := AController.CurrentProcess.MainThread.RegisterValueList;
  Reg := RegList.FindRegisterByDwarfIndex(8);
  if Reg <> nil then
    AContext := ADbgInfo.FindContext(ThreadId, StackFrame, Reg.NumValue)
  else
    AContext := nil;

  if AContext = nil then
    begin
    if AWatchValue <> nil then
      AWatchValue.Validity := ddsInvalid;
    exit;
    end;

  Result := True;
  AContext.MemManager.DefaultContext := AContext;
  APasExpr := TFpPascalExpression.Create(AExpression, AContext);
  try
    APasExpr.ResultValue; // trigger full validation
    if not APasExpr.Valid then
      begin
      AResText := ErrorHandler.ErrorAsString(APasExpr.Error);
      if AWatchValue <> nil then
        begin
        AWatchValue.Value := AResText;
        AWatchValue.Validity := ddsError;
        end;
      end
    else
      begin
      FPrettyPrinter.AddressSize:=AContext.SizeOfAddress;
      FPrettyPrinter.MemManager := AContext.MemManager;
      if defNoTypeInfo in EvalFlags then
        Res := FPrettyPrinter.PrintValue(AResText, APasExpr.ResultValue, DispFormat, RepeatCnt)
      else
        Res := FPrettyPrinter.PrintValue(AResText, ATypeInfo, APasExpr.ResultValue, DispFormat, RepeatCnt);
      // TODO: PCHAR/String
      if Res then
        begin
        if AWatchValue <> nil then
          begin
          AWatchValue.Value := AResText; //IntToStr(APasExpr.ResultValue.AsInteger);
          AWatchValue.Validity := ddsValid;
          end;
        end
      else
        begin
        AResText := 'Error';
        if AWatchValue <> nil then
          AWatchValue.Validity := ddsInvalid;
        end;
      end;
  finally
    APasExpr.Free;
    AContext.ReleaseReference;
  end;
end;

function TFpDebugDebugger.CreateLineInfo: TDBGLineInfo;
begin
  Result := TFpLineInfo.Create(Self);
end;

function TFpDebugDebugger.CreateWatches: TWatchesSupplier;
begin
  Result := TFPWatches.Create(Self);
end;

function TFpDebugDebugger.CreateLocals: TLocalsSupplier;
begin
  Result := TFPLocals.Create(Self);
end;

function TFpDebugDebugger.CreateRegisters: TRegisterSupplier;
begin
  Result := TFPRegisters.Create(Self);
end;

function TFpDebugDebugger.CreateCallStack: TCallStackSupplier;
begin
  Result:=TFPCallStackSupplier.Create(Self);
end;

function TFpDebugDebugger.CreateDisassembler: TDBGDisassembler;
begin
  Result:=TFPDBGDisassembler.Create(Self);
end;

function TFpDebugDebugger.CreateBreakPoints: TDBGBreakPoints;
begin
  Result := TFPBreakPoints.Create(Self, TFPBreakpoint);
end;

procedure TFpDebugDebugger.FDbgControllerDebugInfoLoaded(Sender: TObject);
begin
  if LineInfo <> nil then begin
    TFpLineInfo(LineInfo).DebugInfoChanged;
  end;
end;

procedure TFpDebugDebugger.DoWatchFreed(Sender: TObject);
begin
  FWatchEvalList.Remove(pointer(Sender));
end;

procedure TFpDebugDebugger.ProcessASyncWatches(Data: PtrInt);
var
  WatchValue: TWatchValue;
  AVal: String;
  AType: TDBGType;
begin
  FWatchAsyncQueued := False;
  if FWatchEvalList.Count = 0 then
    exit;
  WatchValue := TWatchValue(FWatchEvalList[0]);
  FWatchEvalList.Delete(0);
  WatchValue.RemoveFreeNotification(@DoWatchFreed);

  EvaluateExpression(WatchValue, WatchValue.Expression, AVal, AType);

  if (not FWatchAsyncQueued) and (FWatchEvalList.Count > 0) then
    begin
    Application.QueueAsyncCall(@ProcessASyncWatches, 0);
    FWatchAsyncQueued := True;
    end;
end;

function TFpDebugDebugger.GetClassInstanceName(AnAddr: TDBGPtr): string;
var
  VMTAddr: TDBGPtr;
  ClassNameAddr: TDBGPtr;
  b: byte;
begin
  // Read address of the vmt
  FDbgController.CurrentProcess.ReadAddress(AnAddr, VMTAddr);
  FDbgController.CurrentProcess.ReadAddress(VMTAddr+3*4, ClassNameAddr);
  // read classname (as shortstring)
  FDbgController.CurrentProcess.ReadData(ClassNameAddr, 1, b);
  setlength(result,b);
  FDbgController.CurrentProcess.ReadData(ClassNameAddr+1, b, result[1]);
end;

function TFpDebugDebugger.ReadAnsiString(AnAddr: TDbgPtr): string;
var
  StrAddr: TDBGPtr;
  len: integer;
begin
  result := '';
  if not FDbgController.CurrentProcess.ReadAddress(AnAddr, StrAddr) then
    Exit;
  FDbgController.CurrentProcess.ReadOrdinal(StrAddr-sizeof(len), len);
  setlength(result, len);
  if not FDbgController.CurrentProcess.ReadData(StrAddr, len, result[1]) then
    result := '';
end;

function TFpDebugDebugger.SetSoftwareExceptionBreakpoint: boolean;
var
  AContext: TFpDbgInfoContext;
  AValue: TFpDbgValue;
begin
  result := false;
  if assigned(FDbgController.CurrentProcess.SymbolTableInfo) then
  begin
    AContext := FDbgController.CurrentProcess.SymbolTableInfo.FindContext(0);
    if Assigned(AContext) then
    begin
      AValue := AContext.FindSymbol('FPC_RAISEEXCEPTION');
      if assigned(AValue) then
      begin
        FRaiseExceptionBreakpoint := FDbgController.CurrentProcess.AddBreak(AValue.Address.Address);
        if assigned(FRaiseExceptionBreakpoint) then
          result := True;
      end;
    end;
  end;
end;

procedure TFpDebugDebugger.HandleSoftwareException(var AnExceptionLocation: TDBGLocationRec;var continue: boolean);
var
  AnExceptionObjectLocation: TDBGPtr;
  ExceptionClass: string;
  ExceptionMessage: string;
begin
  // Using regvar:
  AnExceptionLocation:=GetLocationRec(FDbgController.CurrentProcess.MainThread.RegisterValueList.FindRegisterByDwarfIndex(2).NumValue);
  AnExceptionObjectLocation:=FDbgController.CurrentProcess.MainThread.RegisterValueList.FindRegisterByDwarfIndex(0).NumValue;
  ExceptionClass := GetClassInstanceName(AnExceptionObjectLocation);
  ExceptionMessage := ReadAnsiString(AnExceptionObjectLocation+4);

  DoException(deInternal, ExceptionClass, AnExceptionLocation, ExceptionMessage, continue);
end;

procedure TFpDebugDebugger.FreeDebugThread;
begin
  if FFpDebugThread = nil then
    exit;
  FFpDebugThread.Terminate;
  RTLeventSetEvent(FFpDebugThread.StartDebugLoopEvent);
  FFpDebugThread.WaitFor;
  FFpDebugThread.Free;
  FFpDebugThread := nil;
end;

procedure TFpDebugDebugger.FDbgControllerHitBreakpointEvent(var continue: boolean; const Breakpoint: FpDbgClasses.TDbgBreakpoint);
var
  ABreakPoint: TDBGBreakPoint;
  ALocationAddr: TDBGLocationRec;
begin
  if assigned(Breakpoint) then
    begin
    if BreakPoint=FRaiseExceptionBreakpoint then
      begin
        HandleSoftwareException(ALocationAddr, continue);
        if continue then
          exit;
      end
    else
      begin
        ABreakPoint := TFPBreakpoints(BreakPoints).Find(Breakpoint);
        if assigned(ABreakPoint) then
          ABreakPoint.Hit(continue);
        ALocationAddr := GetLocation;
      end;
    end
  else if FQuickPause then
    begin
      SetState(dsPause);
      continue:=true;
      exit;
    end;
  SetState(dsPause);
  DoCurrent(ALocationAddr);
end;

procedure TFpDebugDebugger.FDbgControllerCreateProcessEvent(var continue: boolean);
begin
  // This will trigger setting the breakpoints
  SetState(dsPause);

  if not SetSoftwareExceptionBreakpoint then
    debugln('Failed to set software-debug breakpoint');
end;

function TFpDebugDebugger.RequestCommand(const ACommand: TDBGCommand;
  const AParams: array of const): Boolean;
var
  EvalFlags: TDBGEvaluateFlags;
begin
  result := False;
  case ACommand of
    dcRun:
      begin
      if not assigned(FDbgController.MainProcess) then
        begin
        FDbgController.ExecutableFilename:=FileName;
        FDbgController.Params.Clear;
        if Arguments<>'' then
          CommandToList(Arguments, FDbgController.Params);
        FDbgController.WorkingDirectory:=WorkingDir;
        FDbgController.Environment:=Environment;
        FFpDebugThread := TFpDebugThread.Create(Self);
        RTLeventWaitFor(FFpDebugThread.DebugLoopStoppedEvent);
        RTLeventResetEvent(FFpDebugThread.DebugLoopStoppedEvent);
        result := FFpDebugThread.StartSuccesfull;
        if not result then
          begin
          FreeDebugThread;
          Exit;
          end;
        SetState(dsInit);
        end
      else
        begin
        Result := True;
        SetState(dsRun);
        end;
      StartDebugLoop;
      end;
    dcStop:
      begin
        FDbgController.Stop;
        if state=dsPause then
          begin
          SetState(dsRun);
          StartDebugLoop;
          end;
        result := true;
      end;
    dcStepIntoInstr:
      begin
        FDbgController.StepIntoInstr;
        SetState(dsRun);
        StartDebugLoop;
        result := true;
      end;
    dcStepOverInstr:
      begin
        FDbgController.StepOverInstr;
        SetState(dsRun);
        StartDebugLoop;
        result := true;
      end;
    dcPause:
      begin
        FDbgController.Pause;
      end;
    dcRunTo:
      begin
        result := FDbgController.CurrentProcess.RunTo(AnsiString(AParams[0].VAnsiString), AParams[1].VInteger);
        if result then
          begin
          SetState(dsRun);
          StartDebugLoop;
          end;
      end;
    dcStepOver:
      begin
        FDbgController.Next;
        SetState(dsRun);
        StartDebugLoop;
        result := true;
      end;
    dcStepInto:
      begin
        FDbgController.Step;
        SetState(dsRun);
        StartDebugLoop;
        result := true;
      end;
    dcStepOut:
      begin
        FDbgController.StepOut;
        SetState(dsRun);
        StartDebugLoop;
        result := true;
      end;
    dcEvaluate:
      begin
        EvalFlags := TDBGEvaluateFlags(AParams[3].VInteger);
        Result := False;
        Result := EvaluateExpression(nil, String(AParams[0].VAnsiString),
          String(AParams[1].VPointer^), TDBGType(AParams[2].VPointer^),
          EvalFlags);
      end;
  end; {case}
end;

function TFpDebugDebugger.ChangeFileName: Boolean;
begin
  result := true;
end;

procedure TFpDebugDebugger.OnLog(AString: String);
begin
  DebugLn(AString);
end;

procedure TFpDebugDebugger.StartDebugLoop;
begin
  DebugLn('StartDebugLoop');
  RTLeventSetEvent(FFpDebugThread.StartDebugLoopEvent);
end;

procedure TFpDebugDebugger.DebugLoopFinished;
var
  Cont: boolean;
begin
  DebugLn('DebugLoopFinished');

  FDbgController.SendEvents(Cont);

  if Cont then
    begin
    SetState(dsRun);
    StartDebugLoop;
    end
end;

procedure TFpDebugDebugger.QuickPause;
begin
  FQuickPause:=true;
  FDbgController.Pause;
end;

procedure TFpDebugDebugger.DoState(const OldState: TDBGState);
begin
  inherited DoState(OldState);
  if not (State in [dsPause, dsInternalPause]) then
    begin
    FWatchEvalList.Clear;
    FWatchAsyncQueued := False;
    end;
end;

constructor TFpDebugDebugger.Create(const AExternalDebugger: String);
begin
  inherited Create(AExternalDebugger);
  FWatchEvalList := TList.Create;
  FPrettyPrinter := TFpPascalPrettyPrinter.Create(sizeof(pointer));
  FDbgController := TDbgController.Create;
  FDbgController.OnLog:=@OnLog;
  FDbgController.OnCreateProcessEvent:=@FDbgControllerCreateProcessEvent;
  FDbgController.OnHitBreakpointEvent:=@FDbgControllerHitBreakpointEvent;
  FDbgController.OnProcessExitEvent:=@FDbgControllerProcessExitEvent;
  FDbgController.OnExceptionEvent:=@FDbgControllerExceptionEvent;
  FDbgController.OnDebugInfoLoaded := @FDbgControllerDebugInfoLoaded;
end;

destructor TFpDebugDebugger.Destroy;
begin
  if assigned(FFpDebugThread) then
    FreeDebugThread;
  FDbgController.Free;
  FPrettyPrinter.Free;
  FWatchEvalList.Free;
  inherited Destroy;
end;

function TFpDebugDebugger.GetLocationRec(AnAddress: TDBGPtr): TDBGLocationRec;
var
  sym, symproc: TFpDbgSymbol;
begin
  if Assigned(FDbgController.CurrentProcess) then
    begin
    result.FuncName:='';
    result.SrcFile:='';
    result.SrcFullName:='';
    result.SrcLine:=0;

    if AnAddress=0 then
      result.Address := FDbgController.CurrentProcess.GetInstructionPointerRegisterValue
    else
      result.Address := AnAddress;

    sym := FDbgController.CurrentProcess.FindSymbol(result.Address);
    if sym = nil then
      Exit;

    result.SrcFile := sym.FileName;
    result.SrcLine := sym.Line;
    result.SrcFullName := sym.FileName;

    symproc := sym;
    while not (symproc.kind in [skProcedure, skFunction]) do
      symproc := symproc.Parent;

    if assigned(symproc) then
      result.FuncName:=symproc.Name;
    end
end;

function TFpDebugDebugger.GetLocation: TDBGLocationRec;
begin
  Result:=GetLocationRec;
end;

class function TFpDebugDebugger.Caption: String;
begin
  Result:='FpDebug internal Dwarf-debugger (alfa)';
end;

class function TFpDebugDebugger.HasExePath: boolean;
begin
  Result:=False;
end;

function TFpDebugDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result:=[dcRun, dcStop, dcStepIntoInstr, dcStepOverInstr, dcStepOver,
           dcRunTo, dcPause, dcStepOut, dcStepInto, dcEvaluate];
end;

end.

