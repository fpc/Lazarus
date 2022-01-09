unit IdeDebuggerBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  DbgIntfDebuggerBase, DbgIntfMiscClasses, LazDebuggerIntf;

type

  TWatch = class;

  { TGuiWatchValue }

  TGuiWatchValue = class(TWatchValue)
  private
    FWatch: TWatch;
  protected
    function GetExpression: String; override;
  public
    constructor Create(AOwnerWatch: TWatch);
    property Watch: TWatch read FWatch;
  end;

  { TWatchValueList }

  TWatchValueList = class
  private
    FList: TList;
    FWatch: TWatch;
    function GetEntry(const AThreadId: Integer; const AStackFrame: Integer): TGuiWatchValue;
    function GetEntryByIdx(AnIndex: integer): TGuiWatchValue;
  protected
    function CreateEntry(const {%H-}AThreadId: Integer; const {%H-}AStackFrame: Integer): TGuiWatchValue; virtual;
    function CopyEntry(AnEntry: TGuiWatchValue): TGuiWatchValue; virtual;
  public
    procedure Assign(AnOther: TWatchValueList);
    constructor Create(AOwnerWatch: TWatch);
    destructor Destroy; override;
    procedure Add(AnEntry: TGuiWatchValue);
    procedure Clear;
    function Count: Integer;
    property EntriesByIdx[AnIndex: integer]: TGuiWatchValue read GetEntryByIdx;
    property Entries[const AThreadId: Integer; const AStackFrame: Integer]: TGuiWatchValue
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
    function GetValue(const AThreadId: Integer; const AStackFrame: Integer): TGuiWatchValue;
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
    property Values[const AThreadId: Integer; const AStackFrame: Integer]: TGuiWatchValue
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


implementation

{ TGuiWatchValue }

function TGuiWatchValue.GetExpression: String;
begin
  Result := FWatch.Expression;
end;

constructor TGuiWatchValue.Create(AOwnerWatch: TWatch);
begin
  FWatch := AOwnerWatch;
  inherited Create;
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
  const AStackFrame: Integer): TGuiWatchValue;
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
  const AStackFrame: Integer): TGuiWatchValue;
var
  i: Integer;
begin
  i := FList.Count - 1;
  while i >= 0 do begin
    Result := TGuiWatchValue(FList[i]);
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

function TWatchValueList.GetEntryByIdx(AnIndex: integer): TGuiWatchValue;
begin
  Result := TGuiWatchValue(FList[AnIndex]);
end;

function TWatchValueList.CreateEntry(const AThreadId: Integer;
  const AStackFrame: Integer): TGuiWatchValue;
begin
  Result := nil;
end;

function TWatchValueList.CopyEntry(AnEntry: TGuiWatchValue): TGuiWatchValue;
begin
  Result := TGuiWatchValue.Create(FWatch);
  Result.Assign(AnEntry);
end;

procedure TWatchValueList.Assign(AnOther: TWatchValueList);
var
  i: Integer;
begin
  Clear;
  for i := 0 to AnOther.FList.Count - 1 do begin
    FList.Add(CopyEntry(TGuiWatchValue(AnOther.FList[i])));
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

procedure TWatchValueList.Add(AnEntry: TGuiWatchValue);
begin
  Flist.Add(AnEntry);
end;

procedure TWatchValueList.Clear;
begin
  while FList.Count > 0 do begin
    TObject(FList[0]).Free;
    FList.Delete(0);
  end;
end;

function TWatchValueList.Count: Integer;
begin
  Result := FList.Count;
end;

end.

