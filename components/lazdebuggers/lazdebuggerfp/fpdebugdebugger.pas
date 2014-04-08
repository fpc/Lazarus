unit FpDebugDebugger;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  LazLogger,
  FpDbgClasses,
  FpDbgInfo,
  FpErrorMessages,
  FpPascalBuilder,
  DbgIntfBaseTypes,
  DbgIntfDebuggerBase,
  FpPascalParser,
  FPDbgController;

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
    FDbgController: TDbgController;
    FFpDebugThread: TFpDebugThread;
    procedure FreeDebugThread;
    procedure FDbgControllerHitBreakpointEvent(var continue: boolean);
    procedure FDbgControllerCreateProcessEvent(var continue: boolean);
    procedure FDbgControllerProcessExitEvent(AExitCode: DWord);
    procedure FDbgControllerExceptionEvent(var continue: boolean);
  protected
    function CreateWatches: TWatchesSupplier; override;
    function  CreateRegisters: TRegisterSupplier; override;
    function  RequestCommand(const ACommand: TDBGCommand;
                             const AParams: array of const): Boolean; override;
    function ChangeFileName: Boolean; override;

    procedure OnLog(AString: String);
    procedure StartDebugLoop;
    procedure DebugLoopFinished;
  public
    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;
    function GetLocation: TDBGLocationRec; override;
    class function Caption: String; override;
    class function HasExePath: boolean; override;
    function  GetSupportedCommands: TDBGCommands; override;
  end;

  { TFPWatches }

  TFPWatches = class(TWatchesSupplier)
  private
    FPrettyPrinter: TFpPascalPrettyPrinter;
  protected
    function  FpDebugger: TFpDebugDebugger;
    //procedure DoStateChange(const AOldState: TDBGState); override;
    procedure InternalRequestData(AWatchValue: TWatchValue); override;
  public
    constructor Create(const ADebugger: TDebuggerIntf);
    destructor Destroy; override;
  end;

  { TFPRegisters }

  TFPRegisters = class(TRegisterSupplier)
  public
    procedure RequestData(ARegisters: TRegisters); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterDebugger(TFpDebugDebugger);
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
    ARegisterValue.ValueObj.SetAsNum(ARegisterList[i].NumValue, SizeOf(ARegisterList[i].NumValue));
    ARegisterValue.ValueObj.SetAsText(ARegisterList[i].StrValue);
    ARegisterValue.DataValidity:=ddsValid;
    end;
  ARegisters.DataValidity:=ddsValid;
end;

{ TFPWatches }

function TFPWatches.FpDebugger: TFpDebugDebugger;
begin
  Result := TFpDebugDebugger(Debugger);
end;

procedure TFPWatches.InternalRequestData(AWatchValue: TWatchValue);
var
  AContext: TFpDbgInfoContext;
  AController: TDbgController;
  APasExpr: TFpPascalExpression;
  AVal: string;
begin
  AController := FpDebugger.FDbgController;

  AContext := AController.CurrentProcess.DbgInfo.FindContext(AController.CurrentProcess.GetInstructionPointerRegisterValue);

  APasExpr := TFpPascalExpression.Create(AWatchValue.Expression, AContext);
  try
    if not APasExpr.Valid then
      begin
      AWatchValue.Value := ErrorHandler.ErrorAsString(APasExpr.Error);
      AWatchValue.Validity := ddsError;
      end
    else
      begin
      FPrettyPrinter.AddressSize:=AContext.SizeOfAddress;
      if FPrettyPrinter.PrintValue(AVal, APasExpr.ResultValue, []) then
        begin
        AWatchValue.Value := IntToStr(APasExpr.ResultValue.AsInteger);
        AWatchValue.Validity := ddsValid;
        end
      else
        AWatchValue.Validity := ddsInvalid;
      end;
  finally
    APasExpr.Free;
  end;
end;

constructor TFPWatches.Create(const ADebugger: TDebuggerIntf);
begin
  inherited Create(ADebugger);
  FPrettyPrinter := TFpPascalPrettyPrinter.Create(sizeof(pointer));
end;

destructor TFPWatches.Destroy;
begin
  FPrettyPrinter.Free;
  inherited Destroy;
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
  FreeDebugThread;
  SetState(dsStop);
end;

procedure TFpDebugDebugger.FDbgControllerExceptionEvent(var continue: boolean);
begin
  DoException(deInternal, 'unknown', GetLocation, 'Unknown exception', continue);
  if not continue then
    begin
    SetState(dsPause);
    DoCurrent(GetLocation);
    end;
end;

function TFpDebugDebugger.CreateWatches: TWatchesSupplier;
begin
  Result := TFPWatches.Create(Self);
end;

function TFpDebugDebugger.CreateRegisters: TRegisterSupplier;
begin
  Result := TFPRegisters.Create(Self);
end;

procedure TFpDebugDebugger.FreeDebugThread;
begin
  FFpDebugThread.Terminate;
  RTLeventSetEvent(FFpDebugThread.StartDebugLoopEvent);
  FFpDebugThread.WaitFor;
  FFpDebugThread.Free;
  FFpDebugThread := nil;
end;

procedure TFpDebugDebugger.FDbgControllerHitBreakpointEvent(var continue: boolean);
begin
  BreakPoints[0].Hit(continue);
  SetState(dsPause);
  DoCurrent(GetLocation);
end;

procedure TFpDebugDebugger.FDbgControllerCreateProcessEvent(var continue: boolean);
var
  i: integer;
  bp: TDBGBreakPoint;
  ibp: FpDbgClasses.TDbgBreakpoint;
begin
  SetState(dsInit);
  for i := 0 to BreakPoints.Count-1 do
    begin
    bp := BreakPoints.Items[i];
    if bp.Enabled then
      begin
      case bp.Kind of
        bpkAddress:   ibp := FDbgController.CurrentProcess.AddBreak(bp.Address);
        bpkSource:    ibp := TDbgInstance(FDbgController.CurrentProcess).AddBreak(bp.Source, cardinal(bp.Line));
      else
        Raise Exception.Create('Breakpoints of this kind are not suported.');
      end;
      if not assigned(ibp) then
        begin
        DoDbgOutput('Failed to set breakpoint '+inttostr(bp.ID));
        DoOutput('Failed to set breakpoint '+inttostr(bp.ID));
        //bp.Valid:=vsInvalid;
        end
      //else
        //bp.Valid:=vsValid;
      end;
    end;
end;

function TFpDebugDebugger.RequestCommand(const ACommand: TDBGCommand;
  const AParams: array of const): Boolean;
begin
  result := False;
  case ACommand of
    dcRun:
      begin
      if not assigned(FDbgController.MainProcess) then
        begin
        FDbgController.ExecutableFilename:=FileName;
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
        SetState(dsRun);
        end;
      StartDebugLoop;
      end;
    dcStop:
      begin
        FDbgController.Stop;
        result := true;
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

constructor TFpDebugDebugger.Create(const AExternalDebugger: String);
begin
  inherited Create(AExternalDebugger);
  FDbgController := TDbgController.Create;
  FDbgController.OnLog:=@OnLog;
  FDbgController.OnCreateProcessEvent:=@FDbgControllerCreateProcessEvent;
  FDbgController.OnHitBreakpointEvent:=@FDbgControllerHitBreakpointEvent;
  FDbgController.OnProcessExitEvent:=@FDbgControllerProcessExitEvent;
  FDbgController.OnExceptionEvent:=@FDbgControllerExceptionEvent;
end;

destructor TFpDebugDebugger.Destroy;
begin
  if assigned(FFpDebugThread) then
    FreeDebugThread;
  FDbgController.Free;
  inherited Destroy;
end;

function TFpDebugDebugger.GetLocation: TDBGLocationRec;
var
  sym, symproc: TFpDbgSymbol;
begin
  if Assigned(FDbgController.CurrentProcess) then
    begin
    result.FuncName:='';
    result.SrcFile:='';
    result.SrcFullName:='';
    result.SrcLine:=0;

    result.Address := FDbgController.CurrentProcess.GetInstructionPointerRegisterValue;

    sym := FDbgController.CurrentProcess.FindSymbol(result.Address);
    if sym = nil then
      Exit;

    result.SrcFile := sym.FileName;
    result.SrcLine := sym.Line;
    result.SrcFullName := sym.FileName;

    debugln('Locatie: '+sym.FileName+':'+sym.Name+':'+inttostr(sym.Line));

    symproc := sym;
    while not (symproc.kind in [skProcedure, skFunction]) do
      symproc := symproc.Parent;

    if assigned(symproc) then
      result.FuncName:=symproc.Name;
    end
  else
    result := inherited;
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
  Result:=[dcRun, dcStop];
end;

end.

