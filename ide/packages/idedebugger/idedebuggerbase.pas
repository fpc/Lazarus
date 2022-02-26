unit IdeDebuggerBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazClasses, LazLoggerBase, IdeDebuggerWatchResult,
  DbgIntfDebuggerBase, DbgIntfMiscClasses,
  LazDebuggerIntf, LazDebuggerTemplate, LazDebuggerIntfBaseTypes;

type

  TWatch = class;

  { TWatchValue }

  TWatchValue = class(TRefCountedObject)
  protected
    FWatch: TWatch;
    FTypeInfo: TDBGType;
    FValidity: TDebuggerDataState;
    FResultData: TWatchResultData;

    procedure SetWatch(AValue: TWatch); virtual;
    function GetDisplayFormat: TWatchDisplayFormat;
    function GetEvaluateFlags: TWatcheEvaluateFlags;
    function GetRepeatCount: Integer;
    function GetStackFrame: Integer;
    function GetThreadId: Integer;
    function GetValidity: TDebuggerDataState; virtual;
    procedure SetValidity(AValue: TDebuggerDataState); virtual;
    procedure SetValue(AValue: String); virtual;
    procedure SetTypeInfo(AValue: TDBGType);
    procedure SetTypeInfo(AValue: TDBGTypeBase);

    function GetResultData: TWatchResultData; virtual;
    procedure SetResultData(AResultData: TWatchResultData);
  protected
    FDisplayFormat: TWatchDisplayFormat;
    FEvaluateFlags: TWatcheEvaluateFlags;
    FRepeatCount: Integer;
    FStackFrame: Integer;
    FThreadId: Integer;
    procedure DoDataValidityChanged({%H-}AnOldValidity: TDebuggerDataState); virtual;

    function GetExpression: String; virtual;
    function GetTypeInfo: TDBGType; virtual;
    function GetValue: String; virtual;
  public
    constructor Create(AOwnerWatch: TWatch);
    destructor Destroy; override;
    procedure Assign(AnOther: TWatchValue); virtual;
    property DisplayFormat: TWatchDisplayFormat read GetDisplayFormat;
    property EvaluateFlags: TWatcheEvaluateFlags read GetEvaluateFlags;
    property RepeatCount: Integer read GetRepeatCount;
    property ThreadId: Integer read GetThreadId;
    property StackFrame: Integer read GetStackFrame;
    property Expression: String read GetExpression;
  public
    property Watch: TWatch read FWatch write SetWatch;
    property Validity: TDebuggerDataState read GetValidity write SetValidity;
    property Value: String read GetValue write SetValue;
    property TypeInfo: TDBGType read GetTypeInfo write SetTypeInfo;
    property ResultData: TWatchResultData read GetResultData;
  end;

  { TWatchValueList }

  TWatchValueList = class
  private
    FList: TList;
    FWatch: TWatch;
    function GetEntry(const AThreadId: Integer; const AStackFrame: Integer): TWatchValue;
    function GetEntryByIdx(AnIndex: integer): TWatchValue;
  protected
    function CreateEntry(const {%H-}AThreadId: Integer; const {%H-}AStackFrame: Integer): TWatchValue; virtual;
    function CopyEntry(AnEntry: TWatchValue): TWatchValue; virtual;
  public
    procedure Assign(AnOther: TWatchValueList);
    constructor Create(AOwnerWatch: TWatch);
    destructor Destroy; override;
    procedure Add(AnEntry: TWatchValue);
    procedure Clear;
    function Count: Integer;
    property EntriesByIdx[AnIndex: integer]: TWatchValue read GetEntryByIdx;
    property Entries[const AThreadId: Integer; const AStackFrame: Integer]: TWatchValue
             read GetEntry; default;
    property Watch: TWatch read FWatch;
  end;

  { TWatch }

  TWatch = class(TDelayedUdateItem)
  private

    procedure SetDisplayFormat(AValue: TWatchDisplayFormat);
    procedure SetEnabled(AValue: Boolean);
    procedure SetEvaluateFlags(AValue: TWatcheEvaluateFlags);
    procedure SetExpression(AValue: String);
    procedure SetRepeatCount(AValue: Integer);
    function GetValue(const AThreadId: Integer; const AStackFrame: Integer): TWatchValue;
  protected
    FEnabled: Boolean;
    FEvaluateFlags: TWatcheEvaluateFlags;
    FExpression: String;
    FDisplayFormat: TWatchDisplayFormat;
    FRepeatCount: Integer;
    FValueList: TWatchValueList;

    procedure DoModified; virtual;  // user-storable data: expression, enabled, display-format
    procedure DoEnableChange; virtual;
    procedure DoExpressionChange; virtual;
    procedure DoDisplayFormatChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    function CreateValueList: TWatchValueList; virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure ClearValues; virtual;
  public
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Expression: String read FExpression write SetExpression;
    property DisplayFormat: TWatchDisplayFormat read FDisplayFormat write SetDisplayFormat;
    property EvaluateFlags: TWatcheEvaluateFlags read FEvaluateFlags write SetEvaluateFlags;
    property RepeatCount: Integer read FRepeatCount write SetRepeatCount;
    property Values[const AThreadId: Integer; const AStackFrame: Integer]: TWatchValue
             read GetValue;
  end;
  TWatchClass = class of TWatch;

  { TWatches }

  TWatches = class(TCollection)
  protected
    function GetItemBase(const AnIndex: Integer): TWatch;
    procedure SetItemBase(const AnIndex: Integer; const AValue: TWatch);
    function WatchClass: TWatchClass; virtual;
  public
    constructor Create;
    procedure ClearValues;
    function Find(const AExpression: String): TWatch;
    property Items[const AnIndex: Integer]: TWatch read GetItemBase write SetItemBase; default;
  end;

  { TWatchesMonitor }

  TWatchesMonitor = class(specialize TWatchesMonitorClassTemplate<TDebuggerDataHandler>, TWatchesMonitorIntf)
  protected
    procedure DoStateChange(const AOldState, ANewState: TDBGState); reintroduce;

    // from TDebuggerDataMonitor
    procedure DoModified; virtual;                                              // user-modified / xml-storable data modified
  public
    destructor Destroy; override;
  end;

implementation

var
  DBG_DATA_MONITORS: PLazLoggerLogGroup;

{ TWatchValue }

procedure TWatchValue.SetValidity(AValue: TDebuggerDataState);
var
  OldValidity: TDebuggerDataState;
begin
  if FValidity = AValue then exit;
  //DebugLn(DBG_DATA_MONITORS, ['DebugDataMonitor: TWatchValue.SetValidity: FThreadId=', FThreadId, '  FStackFrame=',FStackFrame, ' Expr=', Expression, ' AValidity=',dbgs(AValue)]);
  DebugLn(DBG_DATA_MONITORS, ['DebugDataMonitor: TWatchValue.SetValidity:  Expr=', Expression, ' AValidity=',dbgs(AValue)]);
  OldValidity := FValidity;
  FValidity := AValue;
  DoDataValidityChanged(OldValidity);
end;

function TWatchValue.GetValidity: TDebuggerDataState;
begin
  Result := FValidity;
end;

function TWatchValue.GetStackFrame: Integer;
begin
  Result := FStackFrame;
end;

function TWatchValue.GetEvaluateFlags: TWatcheEvaluateFlags;
begin
  Result := FEvaluateFlags;
end;

function TWatchValue.GetResultData: TWatchResultData;
begin
  Result := FResultData;
end;

procedure TWatchValue.SetResultData(AResultData: TWatchResultData);
begin
  assert(FResultData=nil, 'TWatchValue.SetResultData: FResultData=nil');
  FResultData := AResultData;
end;

procedure TWatchValue.SetWatch(AValue: TWatch);
begin
  if FWatch = AValue then Exit;
  FWatch := AValue;
end;

function TWatchValue.GetDisplayFormat: TWatchDisplayFormat;
begin
  if (FWatch <> nil) and
     (FWatch.FDisplayFormat <> wdfMemDump) and
     (FDisplayFormat <> wdfMemDump) and
     (FResultData <> nil) and
     (FResultData.ValueKind <> rdkPrePrinted)
  then
    Result := FWatch.DisplayFormat
  else
    Result := FDisplayFormat;
end;

function TWatchValue.GetRepeatCount: Integer;
begin
  Result := FRepeatCount;
end;

function TWatchValue.GetThreadId: Integer;
begin
  Result := FThreadId;
end;

procedure TWatchValue.SetValue(AValue: String);
begin
  assert(False, 'TWatchValue.SetValue: False');
end;

procedure TWatchValue.SetTypeInfo(AValue: TDBGType);
begin
  //assert(Self is TCurrentWatchValue, 'TWatchValue.SetTypeInfo');
  FreeAndNil(FTypeInfo);
  FTypeInfo := AValue;
end;

procedure TWatchValue.SetTypeInfo(AValue: TDBGTypeBase);
begin
  SetTypeInfo(TDBGType(AValue));
end;

procedure TWatchValue.DoDataValidityChanged(AnOldValidity: TDebuggerDataState);
begin
  //
end;

function TWatchValue.GetExpression: String;
begin
  if FWatch <> nil then
    Result := FWatch.Expression;
end;

function TWatchValue.GetTypeInfo: TDBGType;
begin
  Result := FTypeInfo;
end;

function TWatchValue.GetValue: String;
begin
  if FResultData <> nil then
    Result := FResultData.AsString
  else
    Result := '';
end;

constructor TWatchValue.Create(AOwnerWatch: TWatch);
begin
  FWatch := AOwnerWatch;
  inherited Create;
  AddReference;
end;

destructor TWatchValue.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FTypeInfo);
  FreeAndNil(FResultData);
end;

procedure TWatchValue.Assign(AnOther: TWatchValue);
var
  i: Integer;
begin
  FreeAndNil(FTypeInfo);
  if AnOther.FTypeInfo <> nil then begin
    // partial assign
    FTypeInfo      := TDBGType.Create(AnOther.FTypeInfo.Kind, AnOther.FTypeInfo.TypeName);
    for i := 0 to AnOther.FTypeInfo.Fields.Count - 1 do
      FTypeInfo.Fields.Add(AnOther.FTypeInfo.Fields.Items[i]);
  end;
  FValidity      := AnOther.FValidity;
  FResultData.Free;
  FResultData := AnOther.FResultData.CreateCopy;
end;

{ TWatch }

procedure TWatch.SetDisplayFormat(AValue: TWatchDisplayFormat);
begin
  if AValue = FDisplayFormat then exit;
  FDisplayFormat := AValue;
  DoDisplayFormatChanged;
end;

procedure TWatch.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue
  then begin
    FEnabled := AValue;
    DoEnableChange;
  end;
end;

procedure TWatch.SetEvaluateFlags(AValue: TWatcheEvaluateFlags);
begin
  if FEvaluateFlags = AValue then Exit;
  FEvaluateFlags := AValue;
  Changed;
  DoModified;
end;

procedure TWatch.SetExpression(AValue: String);
begin
  if AValue <> FExpression
  then begin
    FExpression := AValue;
    FValueList.Clear;
    DoExpressionChange;
  end;
end;

procedure TWatch.SetRepeatCount(AValue: Integer);
begin
  if FRepeatCount = AValue then Exit;
  FRepeatCount := AValue;
  Changed;
  DoModified;
end;

function TWatch.GetValue(const AThreadId: Integer;
  const AStackFrame: Integer): TWatchValue;
begin
  Result := FValueList[AThreadId, AStackFrame];
end;

procedure TWatch.DoModified;
begin
  //
end;

procedure TWatch.DoEnableChange;
begin
  //
end;

procedure TWatch.DoExpressionChange;
begin
  //
end;

procedure TWatch.DoDisplayFormatChanged;
begin
  //
end;

procedure TWatch.AssignTo(Dest: TPersistent);
begin
  if Dest is TWatch
  then begin
    TWatch(Dest).FExpression    := FExpression;
    TWatch(Dest).FEnabled       := FEnabled;
    TWatch(Dest).FDisplayFormat := FDisplayFormat;
    TWatch(Dest).FRepeatCount   := FRepeatCount;
    TWatch(Dest).FEvaluateFlags := FEvaluateFlags;
    TWatch(Dest).FValueList.Assign(FValueList);
  end
  else inherited;
end;

function TWatch.CreateValueList: TWatchValueList;
begin
  Result := TWatchValueList.Create(Self);
end;

constructor TWatch.Create(ACollection: TCollection);
begin
  FEnabled := False;
  FValueList := CreateValueList;
  inherited Create(ACollection);
end;

destructor TWatch.Destroy;
begin
  FValueList.Clear;
  inherited Destroy;
  FreeAndNil(FValueList);
end;

procedure TWatch.ClearValues;
begin
  FValueList.Clear;
end;

{ TWatches }

function TWatches.GetItemBase(const AnIndex: Integer): TWatch;
begin
  Result := TWatch(inherited Items[AnIndex]);
end;

procedure TWatches.SetItemBase(const AnIndex: Integer; const AValue: TWatch);
begin
  inherited Items[AnIndex] := AValue;
end;

function TWatches.WatchClass: TWatchClass;
begin
  Result := TWatch;
end;

constructor TWatches.Create;
begin
  inherited Create(WatchClass);
end;

procedure TWatches.ClearValues;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    Items[n].ClearValues;
end;

function TWatches.Find(const AExpression: String): TWatch;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
  begin
    Result := TWatch(GetItem(n));
    if CompareText(Result.Expression, AExpression) = 0 then Exit;
  end;
  Result := nil;
end;

{ TWatchValueList }

function TWatchValueList.GetEntry(const AThreadId: Integer;
  const AStackFrame: Integer): TWatchValue;
var
  i: Integer;
begin
  i := FList.Count - 1;
  while i >= 0 do begin
    Result := TWatchValue(FList[i]);
    if (Result.ThreadId = AThreadId) and (Result.StackFrame = AStackFrame) and
       (Result.DisplayFormat = FWatch.DisplayFormat) and
       (Result.RepeatCount = FWatch.RepeatCount) and
       (Result.EvaluateFlags = FWatch.EvaluateFlags)
    then
      exit;
    dec(i);
  end;
  Result := CreateEntry(AThreadId, AStackFrame);
end;

function TWatchValueList.GetEntryByIdx(AnIndex: integer): TWatchValue;
begin
  Result := TWatchValue(FList[AnIndex]);
end;

function TWatchValueList.CreateEntry(const AThreadId: Integer;
  const AStackFrame: Integer): TWatchValue;
begin
  Result := nil;
end;

function TWatchValueList.CopyEntry(AnEntry: TWatchValue): TWatchValue;
begin
  Result := TWatchValue.Create(FWatch);
  Result.Assign(AnEntry);
end;

procedure TWatchValueList.Assign(AnOther: TWatchValueList);
var
  i: Integer;
begin
  Clear;
  for i := 0 to AnOther.FList.Count - 1 do begin
    FList.Add(CopyEntry(TWatchValue(AnOther.FList[i])));
  end;
end;

constructor TWatchValueList.Create(AOwnerWatch: TWatch);
begin
  assert(AOwnerWatch <> nil, 'TWatchValueList.Create without owner');
  FList := TList.Create;
  FWatch := AOwnerWatch;
  inherited Create;
end;

destructor TWatchValueList.Destroy;
begin
  Clear;
  inherited Destroy;
  FreeAndNil(FList);
end;

procedure TWatchValueList.Add(AnEntry: TWatchValue);
begin
  Flist.Add(AnEntry);
end;

procedure TWatchValueList.Clear;
begin
  while FList.Count > 0 do begin
    TWatchValue(FList[0]).Watch := nil;
    TWatchValue(FList[0]).ReleaseReference;
    FList.Delete(0);
  end;
end;

function TWatchValueList.Count: Integer;
begin
  Result := FList.Count;
end;

{ TWatchesMonitor }

procedure TWatchesMonitor.DoStateChange(const AOldState, ANewState: TDBGState);
begin
  DoStateChangeEx(AOldState, ANewState);
end;

procedure TWatchesMonitor.DoModified;
begin
  //
end;

destructor TWatchesMonitor.Destroy;
begin
  DoDestroy;
  inherited Destroy;
end;

initialization
  DBG_DATA_MONITORS := DebugLogger.FindOrRegisterLogGroup('DBG_DATA_MONITORS' {$IFDEF DBG_DATA_MONITORS} , True {$ENDIF} );

end.

