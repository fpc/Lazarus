unit IdeDebuggerBase;

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils, LazClasses, LazLoggerBase, LazMethodList,
  IdeDebuggerWatchResult, IdeDebuggerBackendValueConv,
  IdeDebuggerWatchResultJSon, DbgIntfDebuggerBase, DbgIntfMiscClasses,
  LazDebuggerIntf, LazDebuggerTemplate, LazDebuggerIntfBaseTypes,
  LazDebuggerValueConverter, FpDebugConvDebugForJson;

type

  IFreeNotifyingIntf = interface ['fni']
    procedure AddFreeNotification(ANotification: TNotifyEvent);
    procedure RemoveFreeNotification(ANotification: TNotifyEvent);
  end;


  IWatchAbleDataIntf = interface(IFreeNotifyingIntf) ['wdi']
    procedure LimitChildWatchCount(AMaxCnt: Integer; AKeepIndexEntriesBelow: Int64 = low(Int64)); virtual;
    procedure ClearDisplayData;  // Clear any cached display-data / keep only what's needed for the snapshot

    function GetEnabled: Boolean;
    function GetExpression: String;
    function GetDisplayName: String;

    property Enabled: Boolean read GetEnabled;
    property Expression: String read GetExpression; // thread save / non-changing in begin/end-uptdate
    property DisplayName: String read GetDisplayName;
  end;

  IWatchAbleResultIntf = interface ['wdr']
    function GetChildrenByNameAsArrayEntry(AName: Int64): TObject;
    function GetChildrenByNameAsField(AName, AClassName: String): TObject;

    function GetEnabled: Boolean;
    function GetValidity: TDebuggerDataState;
    function GetDisplayFormat: TWatchDisplayFormat;
    function GetTypeInfo: TDBGType; deprecated;
    function GetValue: string;
    function GetResultData: TWatchResultData;

    property ChildrenByNameAsField[AName, AClassName: String]: TObject read GetChildrenByNameAsField;
    property ChildrenByNameAsArrayEntry[AName: Int64]: TObject read GetChildrenByNameAsArrayEntry;

    property Enabled: Boolean read GetEnabled;
    property Validity: TDebuggerDataState read GetValidity;
    property DisplayFormat: TWatchDisplayFormat read GetDisplayFormat;
    property TypeInfo: TDBGType read GetTypeInfo;
    property Value: string read GetValue; // for <Error> etc
    property ResultData: TWatchResultData read GetResultData;
  end;

  TWatch = class;

  { TWatchValue }

  TWatchValue = class(TRefCountedObject)
  private
  protected
    FWatch: TWatch;
    FTypeInfo: TDBGType;
    FValidity: TDebuggerDataState;
    FResultData: TWatchResultData;
    FResultDataSpecialised: TWatchResultData;
    FResultDataContent: (rdcNotAnalysed, rdcNotSpecial, rdcJSon);

    procedure SetWatch(AValue: TWatch); virtual;
    function GetDisplayFormat: TWatchDisplayFormat;
    function GetEvaluateFlags: TWatcheEvaluateFlags;
    function GetFirstIndexOffs: Int64;
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
    FFirstIndexOffs: Int64;
    FStackFrame: Integer;
    FThreadId: Integer;
    procedure DoDataValidityChanged({%H-}AnOldValidity: TDebuggerDataState); virtual;

    function GetExpression: String; virtual;
    function GetBackendExpression: String;
    function GetFrontendExpressionSuffix: String;
    function GetTypeInfo: TDBGType; virtual;
    function GetValue: String; virtual;
  public
    constructor Create(AOwnerWatch: TWatch); virtual;
    destructor Destroy; override;
    procedure Assign(AnOther: TWatchValue); virtual;
    procedure ClearDisplayData; // keep only what's needed for the snapshot
    property DisplayFormat: TWatchDisplayFormat read GetDisplayFormat;
    property EvaluateFlags: TWatcheEvaluateFlags read GetEvaluateFlags;
    property FirstIndexOffs: Int64 read GetFirstIndexOffs;
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
  TWatchValueClass = class of TWatchValue;

  { TWatchValueList }

  TWatchValueList = class
  private
    FList: TList;
    FWatch: TWatch;
    function GetEntry(const AThreadId: Integer; const AStackFrame: Integer): TWatchValue;
    function GetEntryByIdx(AnIndex: integer): TWatchValue;
    function GetExistingEntry(const AThreadId: Integer;
      const AStackFrame: Integer): TWatchValue;
  protected
    function CreateEntry(const {%H-}AThreadId: Integer; const {%H-}AStackFrame: Integer): TWatchValue; virtual;
    function CopyEntry(AnEntry: TWatchValue): TWatchValue;
  public
    procedure Assign(AnOther: TWatchValueList);
    constructor Create(AOwnerWatch: TWatch);
    destructor Destroy; override;
    procedure Add(AnEntry: TWatchValue);
    function GetEntriesForRange(const AThreadId: Integer; const AStackFrame: Integer;
                                const AFirstIndexOffs: Int64; const ARepeatCount: Integer): TWatchValue;
    procedure ClearRangeEntries(AKeepMostRecentCount: integer = 0);
    procedure Clear; virtual;
    procedure ClearDisplayData; // keep only what's needed for the snapshot
    function Count: Integer;
    property EntriesByIdx[AnIndex: integer]: TWatchValue read GetEntryByIdx;
    property Entries[const AThreadId: Integer; const AStackFrame: Integer]: TWatchValue
             read GetEntry; default;
    property ExistingEntries[const AThreadId: Integer; const AStackFrame: Integer]: TWatchValue
             read GetExistingEntry;
    property Watch: TWatch read FWatch;
  end;

  { TWatch }

  TWatch = class(TDelayedUdateItem)
  private
    FFreeNotificationList: TMethodList;
    FFirstIndexOffs: Int64;
    FDbgBackendConverter: TIdeDbgValueConvertSelector;

    procedure FDbgBackendConverterFreed(Sender: TObject);
    procedure SetDisplayFormat(AValue: TWatchDisplayFormat);
    procedure SetEnabled(AValue: Boolean);
    procedure SetEvaluateFlags(AValue: TWatcheEvaluateFlags);
    procedure SetExpression(AValue: String);
    procedure SetFirstIndexOffs(AValue: Int64);
    procedure SetDbgBackendConverter(AValue: TIdeDbgValueConvertSelector);
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

    function GetBackendExpression: String;
    function GetFrontendExpressionSuffix: String;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure ClearValues; virtual;
    procedure ClearDisplayData; // keep only what's needed for the snapshot

    procedure AddFreeNotification(ANotification: TNotifyEvent);
    procedure RemoveFreeNotification(ANotification: TNotifyEvent);
  public
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Expression: String read FExpression write SetExpression;
    property DisplayFormat: TWatchDisplayFormat read FDisplayFormat write SetDisplayFormat;
    property EvaluateFlags: TWatcheEvaluateFlags read FEvaluateFlags write SetEvaluateFlags;
    property FirstIndexOffs: Int64 read FFirstIndexOffs write SetFirstIndexOffs;
    property RepeatCount: Integer read FRepeatCount write SetRepeatCount;
    property DbgBackendConverter: TIdeDbgValueConvertSelector read FDbgBackendConverter write SetDbgBackendConverter;
    property Values[const AThreadId: Integer; const AStackFrame: Integer]: TWatchValue
             read GetValue;
    property ValueList: TWatchValueList read FValueList;
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

  TWatchesMonitor = class(specialize TWatchesMonitorClassTemplate<TDebuggerDataHandler>, IDbgWatchesMonitorIntf)
  protected
    procedure DoStateChange(const AOldState, ANewState: TDBGState); reintroduce;

    // from TDebuggerDataMonitor
    procedure DoModified; virtual;                                              // user-modified / xml-storable data modified
  public
    destructor Destroy; override;
  end;

   { TLocalsValue }

  TLocalsValue = class(TDbgEntityValue)
  protected
    FName: String;
    FValue: TWatchResultData;
    function GetValue: String; virtual;
    function GetResultData: TWatchResultData; virtual;
    procedure DoAssign(AnOther: TDbgEntityValue); override;
  public
    destructor Destroy; override;
    procedure Init(AName: String; AValue: TWatchResultData);
    property Name: String read FName;
    property Value: String read GetValue;
    property ResultData: TWatchResultData read GetResultData;
  end;

 { TLocals }

  TLocals = class(TDbgEntityValuesList)
  private
    function GetEntry(AnIndex: Integer): TLocalsValue;
    function GetName(const AnIndex: Integer): String;
    function GetResultData(const AnIndex: Integer): TWatchResultData;
  protected
    function CreateEntry: TDbgEntityValue; override;
  public
    function Add(const AName: String; AValue: TWatchResultData): TLocalsValue; virtual; overload;
    procedure SetDataValidity({%H-}AValidity: TDebuggerDataState); virtual;
  public
    function Count: Integer;reintroduce; virtual;
    function Find(AName: String): TLocalsValue;
    property Entries[AnIndex: Integer]: TLocalsValue read GetEntry;
    property Names[const AnIndex: Integer]: String read GetName;
    property Values[const AnIndex: Integer]: TWatchResultData read GetResultData;
  end;

  { TLocalsList }

  TLocalsList = class(TDbgEntitiesThreadStackList)
  private
    function GetEntry(AThreadId, AStackFrame: Integer): TLocals;
    function GetEntryByIdx(AnIndex: Integer): TLocals;
  protected
    //function CreateEntry(AThreadId, AStackFrame: Integer): TDbgEntityValuesList; override;
  public
    property EntriesByIdx[AnIndex: Integer]: TLocals read GetEntryByIdx;
    property Entries[AThreadId, AStackFrame: Integer]: TLocals read GetEntry; default;
  end;

  { TLocalsMonitor }

  TLocalsMonitor = class(specialize TLocalsMonitorClassTemplate<TDebuggerDataHandler>, IDbgLocalsMonitorIntf)
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
  if Watch <> nil then
    Result := FValidity
  else
    Result := ddsUnknown;
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
  function IsMaybeJson(const s: String): boolean;
  var
    l: SizeInt;
    i: Integer;
  begin
    Result := False;
    l := Length(s);
    if l = 0 then
      exit;

    while (l > 1) and (s[l] in [' ', #9, #10, #13]) do
      dec(l);
    if not(s[l] in [']', '}']) then
      exit;

    i := 1;
    while (i < l) and (s[i] in [' ', #9, #10, #13]) do
      inc(i);
    if not(s[i] in ['[', '{']) then
      exit;

    Result := True;
  end;

var
  UsedConv: ILazDbgValueConverterIntf;
  SrcData: TWatchResultData;

  function CreateJson: TWatchResultDataJSon;
  begin
    Result := TWatchResultDataJSon.Create(SrcData.AsString);
    Result.SetTypeName(SrcData.TypeName);
    if SrcData.HasDataAddress then
      Result.SetDataAddress(SrcData.DataAddress);
    if (Result.Count > 0) or (Result.FieldCount > 0) then
      FResultDataContent := rdcJSon;
    if (UsedConv <> nil) and (UsedConv.GetObject is TFpDbgValueConverterJsonForDebug)
    then begin
      Result.JsonAddressKey  := TFpDbgValueConverterJsonForDebug(UsedConv.GetObject).JsonAddressKey;
      Result.JsonTypenameKey := TFpDbgValueConverterJsonForDebug(UsedConv.GetObject).JsonTypenameKey;
    end;
  end;

begin
  if FResultDataSpecialised <> nil then begin
    Result := FResultDataSpecialised;
    exit;
  end;

  Result := FResultData;
  if (FResultDataContent = rdcNotSpecial) or (Result = nil) then
    exit;

  SrcData := FResultData;
  UsedConv := nil;
  if (SrcData.ValueKind = rdkConvertRes) and (SrcData.FieldCount > 0) and
     (SrcData.Fields[0].Field <> nil) and
     (SrcData.Fields[0].Field.ValueKind <> rdkError)
  then begin
    SrcData := SrcData.Fields[0].Field;
    UsedConv := FResultData.BackendValueHandler;
  end;

  case FResultDataContent of
    rdcJSon: begin
        FResultDataSpecialised := CreateJson;
      end;

    else begin
      FResultDataContent := rdcNotSpecial;

      if (SrcData.ValueKind in [rdkString, rdkPrePrinted]) and (IsMaybeJson(SrcData.AsString)) then begin
        FResultDataSpecialised := CreateJson;
      end;

      if FResultDataContent = rdcNotSpecial then
        FResultDataSpecialised := nil;
    end;
  end;

  Result := FResultData;
  if FResultDataSpecialised <> nil then
    Result := FResultDataSpecialised;

  Result := Result.HandleExpressionSuffix(GetFrontendExpressionSuffix);
  if (Result <> FResultDataSpecialised) and (Result <> FResultData) then begin
    FResultDataSpecialised.Free;
    FResultDataSpecialised := Result;
  end;
end;

procedure TWatchValue.SetResultData(AResultData: TWatchResultData);
begin
  ResultData.Free;
  FResultData := AResultData;
end;

function TWatchValue.GetFirstIndexOffs: Int64;
begin
  Result := FFirstIndexOffs;
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
  Result := '';
  if FWatch <> nil then
    Result := FWatch.Expression;
end;

function TWatchValue.GetBackendExpression: String;
begin
  Result := '';
  if FWatch <> nil then
    Result := FWatch.GetBackendExpression;
end;

function TWatchValue.GetFrontendExpressionSuffix: String;
begin
  Result := '';
  if FWatch <> nil then
    Result := FWatch.GetFrontendExpressionSuffix;
end;

function TWatchValue.GetTypeInfo: TDBGType;
begin
  Result := FTypeInfo;
end;

function TWatchValue.GetValue: String;
var
  rd: TWatchResultData;
begin
  rd := ResultData;
  if rd <> nil then
    Result := rd.AsString
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
  FreeAndNil(FResultDataSpecialised);
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

procedure TWatchValue.ClearDisplayData;
begin
  FreeAndNil(FResultDataSpecialised);
end;

{ TWatch }

procedure TWatch.SetDisplayFormat(AValue: TWatchDisplayFormat);
begin
  if AValue = FDisplayFormat then exit;
  FDisplayFormat := AValue;
  DoDisplayFormatChanged;
end;

procedure TWatch.FDbgBackendConverterFreed(Sender: TObject);
begin
  FDbgBackendConverter := nil;

  Changed;
  DoModified;
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

procedure TWatch.SetFirstIndexOffs(AValue: Int64);
begin
  if FFirstIndexOffs = AValue then Exit;
  FFirstIndexOffs := AValue;
  //FValueList.Clear;
  Changed;
  DoModified;
end;

procedure TWatch.SetDbgBackendConverter(AValue: TIdeDbgValueConvertSelector);
begin
  if FDbgBackendConverter = AValue then Exit;
  FValueList.Clear;

  if FDbgBackendConverter <> nil then
    FDbgBackendConverter.RemoveFreeNotification(@FDbgBackendConverterFreed);

  FDbgBackendConverter := AValue;

  if FDbgBackendConverter <> nil then
    FDbgBackendConverter.AddFreeNotification(@FDbgBackendConverterFreed);

  Changed;
  DoModified;
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
    TWatch(Dest).FFirstIndexOffs    := FFirstIndexOffs;
    TWatch(Dest).FRepeatCount   := FRepeatCount;
    TWatch(Dest).FEvaluateFlags := FEvaluateFlags;
    TWatch(Dest).DbgBackendConverter := DbgBackendConverter;
    TWatch(Dest).FValueList.Assign(FValueList);
  end
  else inherited;
end;

function TWatch.CreateValueList: TWatchValueList;
begin
  Result := TWatchValueList.Create(Self);
end;

function TWatch.GetBackendExpression: String;
var
  i, l: Integer;
  InQuote: Boolean;
begin
  Result := Expression;
  l := Length(Result);
  if l = 0 then
    exit;

  while (l > 0) and (Result[l] in [' ', #9])
    do dec(l);
  if l <= 0 then
    exit;

  if Result[l] <> '}' then
    exit;

  i := 1;
  InQuote := False;
  while i < l do begin
    if Result[i] = '''' then
      InQuote := not InQuote;
      if (not InQuote) and (Result[i] = '{') then
        exit(copy(Result, 1 , i-1));
    inc(i);
  end;
end;

function TWatch.GetFrontendExpressionSuffix: String;
var
  l: SizeInt;
begin
  Result := Expression;
  l := Length(GetBackendExpression);
  Result := copy(Result, 1 + l, Length(Result));
end;

constructor TWatch.Create(ACollection: TCollection);
begin
  FEnabled := False;
  FValueList := CreateValueList;
  inherited Create(ACollection);
end;

destructor TWatch.Destroy;
begin
  if FDbgBackendConverter <> nil then
    FDbgBackendConverter.RemoveFreeNotification(@FDbgBackendConverterFreed);

  FValueList.Clear;

  if FFreeNotificationList <> nil then
    FFreeNotificationList.CallNotifyEvents(Self);

  inherited Destroy;
  FreeAndNil(FFreeNotificationList);
  FreeAndNil(FValueList);
end;

procedure TWatch.ClearValues;
begin
  FValueList.Clear;
end;

procedure TWatch.ClearDisplayData;
begin
  FValueList.ClearDisplayData;
end;

procedure TWatch.AddFreeNotification(ANotification: TNotifyEvent);
begin
  if FFreeNotificationList = nil then
    FFreeNotificationList := TMethodList.Create;
  FFreeNotificationList.Add(TMethod(ANotification));
end;

procedure TWatch.RemoveFreeNotification(ANotification: TNotifyEvent);
begin
  if FFreeNotificationList = nil then
    exit;
  FFreeNotificationList.Remove(TMethod(ANotification));
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
begin
  Result := GetExistingEntry(AThreadId, AStackFrame);
  if Result = nil then
    Result := CreateEntry(AThreadId, AStackFrame);
end;

function TWatchValueList.GetEntriesForRange(const AThreadId: Integer;
  const AStackFrame: Integer; const AFirstIndexOffs: Int64;
  const ARepeatCount: Integer): TWatchValue;
var
  i: Integer;
begin
  i := FList.Count - 1;
  while i >= 0 do begin
    Result := TWatchValue(FList[i]);
    if (Result.ThreadId = AThreadId) and (Result.StackFrame = AStackFrame) and
       (Result.DisplayFormat = FWatch.DisplayFormat) and
       (Result.EvaluateFlags = FWatch.EvaluateFlags) and
       (Result.FFirstIndexOffs <= AFirstIndexOffs) and
       (Result.FFirstIndexOffs + Result.FRepeatCount  > AFirstIndexOffs) and
       (Result.FRepeatCount >= ARepeatCount + (AFirstIndexOffs - Result.FFirstIndexOffs))
    then
      exit;
    dec(i);
  end;
  Result := CreateEntry(AThreadId, AStackFrame); // XXXXXXXXXXXXX No Snapshot
  Result.FFirstIndexOffs  := AFirstIndexOffs;
  Result.FRepeatCount := ARepeatCount;
end;

procedure TWatchValueList.ClearRangeEntries(AKeepMostRecentCount: integer);
var
  Val: TWatchValue;
  i, FirstIdx, RCnt: Integer;
begin
  i := FList.Count - 1;
  FirstIdx := Watch.FirstIndexOffs;
  RCnt := Watch.RepeatCount;
  while i >= 0 do begin
    Val := TWatchValue(FList[i]);
    if (Val.FFirstIndexOffs <>  FirstIdx) or (Val.FRepeatCount <> RCnt) then begin
      if AKeepMostRecentCount > 0 then
        dec(AKeepMostRecentCount)
      else
        FList.Delete(i);
    end;
    dec(i);
  end;
end;

function TWatchValueList.GetEntryByIdx(AnIndex: integer): TWatchValue;
begin
  Result := TWatchValue(FList[AnIndex]);
end;

function TWatchValueList.GetExistingEntry(const AThreadId: Integer;
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
       (Result.FirstIndexOffs = FWatch.FirstIndexOffs) and
       (Result.EvaluateFlags = FWatch.EvaluateFlags)
    then
      exit;
    dec(i);
  end;
  Result := nil;
end;

function TWatchValueList.CreateEntry(const AThreadId: Integer;
  const AStackFrame: Integer): TWatchValue;
begin
  Result := nil;
end;

function TWatchValueList.CopyEntry(AnEntry: TWatchValue): TWatchValue;
begin
  Result := TWatchValueClass(AnEntry.ClassType).Create(FWatch);
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

procedure TWatchValueList.ClearDisplayData;
var
  i: Integer;
begin
  i := FList.Count - 1;
  while i >= 0 do begin
    TWatchValue(FList[i]).ClearDisplayData;
    dec(i);
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

{ TLocalsValue }

function TLocalsValue.GetValue: String;
begin
  Result := '';
  if FValue <> nil then
    Result := FValue.AsString;
end;

function TLocalsValue.GetResultData: TWatchResultData;
begin
  Result := FValue;
end;

procedure TLocalsValue.DoAssign(AnOther: TDbgEntityValue);
begin
  inherited DoAssign(AnOther);
  FName := TLocalsValue(AnOther).FName;
  FValue.Free;
  FValue := TLocalsValue(AnOther).FValue.CreateCopy();
end;

destructor TLocalsValue.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FValue);
end;

procedure TLocalsValue.Init(AName: String; AValue: TWatchResultData);
begin
  FName := AName;
  FValue := AValue;
end;

{ TLocalsList }

function TLocalsList.GetEntry(AThreadId, AStackFrame: Integer): TLocals;
begin
  Result := TLocals(inherited Entries[AThreadId, AStackFrame]);
end;

function TLocalsList.GetEntryByIdx(AnIndex: Integer): TLocals;
begin
  Result := TLocals(inherited EntriesByIdx[AnIndex]);
end;

{ TLocals }

function TLocals.GetEntry(AnIndex: Integer): TLocalsValue;
begin
  Result := TLocalsValue(inherited Entries[AnIndex]);
end;

function TLocals.GetName(const AnIndex: Integer): String;
begin
  Result := Entries[AnIndex].Name;
end;

function TLocals.GetResultData(const AnIndex: Integer): TWatchResultData;
begin
  Result := Entries[AnIndex].ResultData;
end;

function TLocals.CreateEntry: TDbgEntityValue;
begin
  Result := TLocalsValue.Create;
end;

function TLocals.Add(const AName: String; AValue: TWatchResultData
  ): TLocalsValue;
begin
  assert(not Immutable, 'TLocalsBase.Add Immutable');
  Result := TLocalsValue(CreateEntry);
  Result.FName := AName;
  Result.FValue := AValue;
  inherited Add(Result);
end;

procedure TLocals.SetDataValidity(AValidity: TDebuggerDataState);
begin
  //
end;

function TLocals.Count: Integer;
begin
  Result := inherited Count;
end;

function TLocals.Find(AName: String): TLocalsValue;
var
  i: Integer;
begin
  i := Count - 1;
  while i >= 0 do begin
    Result := Entries[i];
    if Result.Name = AName then
      exit;
    dec(i);
  end;
  Result := nil;
end;

{ TLocalsMonitor }

procedure TLocalsMonitor.DoStateChange(const AOldState, ANewState: TDBGState);
begin
  DoStateChangeEx(AOldState, ANewState);
end;

procedure TLocalsMonitor.DoModified;
begin
  //
end;

destructor TLocalsMonitor.Destroy;
begin
  inherited Destroy;
  DoDestroy;
end;

initialization
  DBG_DATA_MONITORS := DebugLogger.FindOrRegisterLogGroup('DBG_DATA_MONITORS' {$IFDEF DBG_DATA_MONITORS} , True {$ENDIF} );

end.

