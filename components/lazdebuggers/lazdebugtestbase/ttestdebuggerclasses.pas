unit TTestDebuggerClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DbgIntfDebuggerBase;

type
  { TTestCallStackList }

  TTestCallStackList = class(TCallStackList)
  protected
    function NewEntryForThread(const AThreadId: Integer): TCallStackBase; override;
  end;

  { TTestCallStackMonitor }

  TTestCallStackMonitor = class(TCallStackMonitor)
  protected
    function CreateCallStackList: TCallStackList; override;
  end;

  TTestThreadsMonitor = class;
  { TTestThreads }

  TTestThreads = class(TThreads)
  private
    FMonitor: TTestThreadsMonitor;
    FDataValidity: TDebuggerDataState;
  public
    constructor Create;
    function  Count: Integer; override;
    procedure Clear; override;
    procedure SetValidity(AValidity: TDebuggerDataState); override;
  end;

  { TTestThreadsMonitor }

  TTestThreadsMonitor = class(TThreadsMonitor)
  protected
    procedure DoStateEnterPause; override;
    function CreateThreads: TThreads; override;
    procedure RequestData;
  end;

  { TTestWatchValue }

  TTestWatchValue = class(TWatchValue)
  protected
    procedure RequestData;
    function GetTypeInfo: TDBGType; override;
    function GetValue: String; override;
  public
    constructor Create(AOwnerWatch: TWatch;
                       const AThreadId: Integer;
                       const AStackFrame: Integer
                      );
    constructor Create(AOwnerWatch: TWatch);
  end;

  { TTestWatchValueList }

  TTestWatchValueList = class(TWatchValueList)
  protected
    function CopyEntry(AnEntry: TWatchValue): TWatchValue; override;
    function CreateEntry(const {%H-}AThreadId: Integer; const {%H-}AStackFrame: Integer): TWatchValue; override;
  end;

  { TTestWatch }

  TTestWatch = class(TWatch)
    function CreateValueList: TWatchValueList; override;
    procedure RequestData(AWatchValue: TTestWatchValue);
  public
  end;

  TTestWatchesMonitor = class;
  { TTestWatches }

  TTestWatches = class(TWatches)
  protected
    FMonitor: TTestWatchesMonitor;
    function WatchClass: TWatchClass; override;
    procedure RequestData(AWatchValue: TWatchValue);
  end;

  { TTestWatchesMonitor }

  TTestWatchesMonitor = class(TWatchesMonitor)
  protected
    procedure DoStateChangeEx(const AOldState, ANewState: TDBGState); override;
    procedure RequestData(AWatchValue: TWatchValue);
    function CreateWatches: TWatches; override;
  end;

  TTestRegistersMonitor = class;
  { TTestRegisters }

  TTestRegisters = class(TRegisters)
  private
    FMonitor: TTestRegistersMonitor;
  protected
    procedure DoDataValidityChanged(AnOldValidity: TDebuggerDataState); override;
  public
    function Count: Integer; reintroduce; override;
  end;

  { TTEstRegistersList }

  TTestRegistersList = class(TRegistersList)
  private
    FMonitor: TTestRegistersMonitor;
  protected
    function CreateEntry(AThreadId, AStackFrame: Integer): TRegisters; override;
  end;

  { TTestRegistersMonitor }

  TTestRegistersMonitor = class(TRegistersMonitor)
  protected
    function CreateRegistersList: TRegistersList; override;
    procedure RequestData(ARegisters: TRegisters);
    procedure DoStateEnterPause; override;
    procedure DoStateLeavePause; override;
  end;

implementation

{ TTestThreads }

constructor TTestThreads.Create;
begin
  inherited Create;
  FDataValidity := ddsUnknown;
end;

function TTestThreads.Count: Integer;
begin
  if (FDataValidity = ddsUnknown) then begin
    FDataValidity := ddsRequested;
    FMonitor.RequestData;
  end;

  Result := inherited Count;
end;

procedure TTestThreads.Clear;
begin
  FDataValidity := ddsUnknown;
  inherited Clear;
end;

procedure TTestThreads.SetValidity(AValidity: TDebuggerDataState);
begin
  if FDataValidity = AValidity then exit;
  FDataValidity := AValidity;
  if FDataValidity = ddsUnknown then Clear;
end;

{ TTestThreadsMonitor }

procedure TTestThreadsMonitor.DoStateEnterPause;
begin
  inherited DoStateEnterPause;
  TTestThreads(Threads).SetValidity(ddsUnknown);
end;

function TTestThreadsMonitor.CreateThreads: TThreads;
begin
  Result := TTestThreads.Create;
  TTestThreads(Result).FMonitor := Self;
end;

procedure TTestThreadsMonitor.RequestData;
begin
  if Supplier <> nil
  then Supplier.RequestMasterData;
end;

{ TTestRegistersMonitor }

function TTestRegistersMonitor.CreateRegistersList: TRegistersList;
begin
  Result := TTestRegistersList.Create;
  TTestRegistersList(Result).FMonitor := Self;
end;

procedure TTestRegistersMonitor.RequestData(ARegisters: TRegisters);
begin
  if Supplier <> nil
  then Supplier.RequestData(ARegisters)
  else ARegisters.DataValidity := ddsInvalid;
end;

procedure TTestRegistersMonitor.DoStateEnterPause;
begin
  inherited DoStateEnterPause;
  RegistersList.Clear;
end;

procedure TTestRegistersMonitor.DoStateLeavePause;
begin
  inherited DoStateLeavePause;
  RegistersList.Clear;
end;

{ TTEstRegistersList }

function TTestRegistersList.CreateEntry(AThreadId, AStackFrame: Integer): TRegisters;
begin
  Result := TTestRegisters.Create(AThreadId, AStackFrame);
  TTestRegisters(Result).FMonitor := FMonitor;
end;

{ TTestRegisters }

procedure TTestRegisters.DoDataValidityChanged(AnOldValidity: TDebuggerDataState);
begin
  inherited DoDataValidityChanged(AnOldValidity);
end;

function TTestRegisters.Count: Integer;
begin
  case DataValidity of
    ddsUnknown:   begin
        AddReference;
        try
          Result := 0;
          DataValidity := ddsRequested;
          FMonitor.RequestData(Self);  // Locals can be cleared, if debugger is "run" again
          if DataValidity = ddsValid then Result := inherited Count();
        finally
          ReleaseReference;
        end;
      end;
    ddsRequested, ddsEvaluating: Result := 0;
    ddsValid:                    Result := inherited Count;
    ddsInvalid, ddsError:        Result := 0;
  end;
end;

{ TTestWatches }

function TTestWatches.WatchClass: TWatchClass;
begin
  Result := TTestWatch;
end;

procedure TTestWatches.RequestData(AWatchValue: TWatchValue);
begin
  TTestWatchesMonitor(FMonitor).RequestData(AWatchValue);
end;

{ TTestWatchesMonitor }

procedure TTestWatchesMonitor.DoStateChangeEx(const AOldState, ANewState: TDBGState);
begin
  inherited DoStateChangeEx(AOldState, ANewState);
  Watches.ClearValues;
end;

procedure TTestWatchesMonitor.RequestData(AWatchValue: TWatchValue);
begin
  if Supplier <> nil
  then Supplier.RequestData(AWatchValue)
  else AWatchValue.Validity := ddsInvalid;
end;

function TTestWatchesMonitor.CreateWatches: TWatches;
begin
  Result := TTestWatches.Create;
  TTestWatches(Result).FMonitor := Self;
end;

{ TTestWatchValue }

procedure TTestWatchValue.RequestData;
begin
  TTestWatch(Watch).RequestData(self);
end;

function TTestWatchValue.GetTypeInfo: TDBGType;
var
  i: Integer;
begin
  Result := nil;
  if not Watch.Enabled then
    exit;
  i := DbgStateChangeCounter;  // workaround for state changes during TWatchValue.GetValue
  if Validity = ddsUnknown then begin
    Validity := ddsRequested;
    RequestData;
    if i <> DbgStateChangeCounter then exit;
  end;
  case Validity of
    ddsRequested,
    ddsEvaluating: Result := nil;
    ddsValid:      Result := inherited GetTypeInfo;
    ddsInvalid,
    ddsError:      Result := nil;
  end;
end;

function TTestWatchValue.GetValue: String;
var
  i: Integer;
begin
  if not Watch.Enabled then begin
    Result := '<disabled>';
    exit;
  end;
  i := DbgStateChangeCounter;  // workaround for state changes during TWatchValue.GetValue
  if Validity = ddsUnknown then begin
    Result := '<evaluating>';
    Validity := ddsRequested;
    RequestData;
    if i <> DbgStateChangeCounter then exit; // in case the debugger did run.
    // TODO: The watch can also be deleted by the user
  end;
  case Validity of
    ddsRequested, ddsEvaluating: Result := '<evaluating>';
    ddsValid:                    Result := inherited GetValue;
    ddsInvalid:                  Result := '<invalid>';
    ddsError:                    Result := '<Error: '+ (inherited GetValue) +'>';
  end;
end;

constructor TTestWatchValue.Create(AOwnerWatch: TWatch; const AThreadId: Integer;
  const AStackFrame: Integer);
begin
  inherited Create(AOwnerWatch);
  Validity := ddsUnknown;
  FDisplayFormat := Watch.DisplayFormat;
  FEvaluateFlags := Watch.EvaluateFlags;
  FRepeatCount   := Watch.RepeatCount;
  FThreadId := AThreadId;
  FStackFrame := AStackFrame;
end;

constructor TTestWatchValue.Create(AOwnerWatch: TWatch);
begin
  inherited Create(AOwnerWatch);
  Validity := ddsUnknown;
  FDisplayFormat := Watch.DisplayFormat;
  FEvaluateFlags := Watch.EvaluateFlags;
  FRepeatCount   := Watch.RepeatCount;
end;

{ TTestWatchValueList }

function TTestWatchValueList.CopyEntry(AnEntry: TWatchValue): TWatchValue;
begin
  Result := TTestWatchValue.Create(Watch);
  Result.Assign(AnEntry);
end;

function TTestWatchValueList.CreateEntry(const AThreadId: Integer;
  const AStackFrame: Integer): TWatchValue;
begin
  Result := TTestWatchValue.Create(Watch, AThreadId, AStackFrame);
  Add(Result);
end;

{ TTestWatch }

function TTestWatch.CreateValueList: TWatchValueList;
begin
  Result := TTestWatchValueList.Create(Self);
end;

procedure TTestWatch.RequestData(AWatchValue: TTestWatchValue);
begin
  if Collection <> nil
  then TTestWatches(Collection).RequestData(AWatchValue)
  else AWatchValue.Validity := ddsInvalid;
end;

{ TTestCallStackMonitor }

function TTestCallStackMonitor.CreateCallStackList: TCallStackList;
begin
  Result := TTestCallStackList.Create;
end;

{ TTestCallStackList }

function TTestCallStackList.NewEntryForThread(const AThreadId: Integer): TCallStackBase;
begin
  Result := TCallStackBase.Create;
  Result.ThreadId := AThreadId;
  add(Result);
end;

end.

