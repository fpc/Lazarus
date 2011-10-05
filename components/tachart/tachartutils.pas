{
 /***************************************************************************
                               TAChartUtils.pas
                               ----------------
              Component Library Standard Graph Utiliity Functions


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

Authors: Luнs Rodrigues, Philippe Martinole, Alexander Klenin

}

unit TAChartUtils;

{$H+}

interface

uses
  Classes, Math, Types, SysUtils;

const
  CHART_COMPONENT_IDE_PAGE = 'Chart';
  PERCENT = 0.01;
  clTAColor = $20000000; // = clDefault, but avoiding dependency on Graphics

type
  EChartError = class(Exception);
  EChartIntervalError = class(EChartError);
  EListenerError = class(EChartError);
  EDrawDataError = class(EChartError);

  // Like TColor, but avoiding dependency on Graphics.
  TChartColor = -$7FFFFFFF-1..$7FFFFFFF;

  TDoublePoint = record
    X, Y: Double;
  end;

  TDoubleRect = record
  case Integer of
    0: (
      a, b: TDoublePoint;
    );
    1: (
      coords: array [1..4] of Double;
    );
  end;

  TPointArray = array of TPoint;

  TChartDistance = 0..MaxInt;

  TPointDistFunc = function (const A, B: TPoint): Integer;

  TTransformFunc = function (A: Double): Double of object;

  TAxisScale = (asIncreasing, asDecreasing, asLogIncreasing, asLogDecreasing);

  TSeriesMarksStyle = (
    smsCustom,         { user-defined }
    smsNone,           { no labels }
    smsValue,          { 1234 }
    smsPercent,        { 12 % }
    smsLabel,          { Cars }
    smsLabelPercent,   { Cars 12 % }
    smsLabelValue,     { Cars 1234 }
    smsLegend,         { ? }
    smsPercentTotal,   { 12 % of 1234 }
    smsLabelPercentTotal, { Cars 12 % of 1234 }
    smsXValue);        { 21/6/1996 }

  TDoubleInterval = record
    FStart, FEnd: Double;
  end;

  TPointBoolArr = array [Boolean] of Integer;
  TDoublePointBoolArr = array [Boolean] of Double;

  { TIntervalList }

  TIntervalList = class
  private
    FEpsilon: Double;
    FIntervals: array of TDoubleInterval;
    FOnChange: TNotifyEvent;
    procedure Changed;
    function GetInterval(AIndex: Integer): TDoubleInterval;
    function GetIntervalCount: Integer;
    procedure SetEpsilon(AValue: Double);
    procedure SetOnChange(AValue: TNotifyEvent);
  public
    procedure Assign(ASource: TIntervalList);
    constructor Create;
  public
    procedure AddPoint(APoint: Double); inline;
    procedure AddRange(AStart, AEnd: Double);
    procedure Clear;
    function Intersect(
      var ALeft, ARight: Double; var AHint: Integer): Boolean;
  public
    property Epsilon: Double read FEpsilon write SetEpsilon;
    property Interval[AIndex: Integer]: TDoubleInterval read GetInterval;
    property IntervalCount: Integer read GetIntervalCount;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TCaseOfTwo = (cotNone, cotFirst, cotSecond, cotBoth);

  { TIndexedComponent }

  TIndexedComponent = class(TComponent)
  strict protected
    function GetIndex: Integer; virtual; abstract;
    procedure SetIndex(AValue: Integer); virtual; abstract;
  public
    procedure ChangeNamePrefix(const AOld, ANew: String; var AFailed: String);

    property Index: Integer read GetIndex write SetIndex;
  end;

  TShowMessageProc = procedure (const AMsg: String);

  {$IFNDEF fpdoc} // Workaround for issue #18549.
  generic TTypedFPListEnumerator<T> = class(TFPListEnumerator)
  {$ELSE}
  TTypedFPListEnumerator = class(TFPListEnumerator)
  {$ENDIF}
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

  { TIndexedComponentList }

  TIndexedComponentList = class(TFPList)
  public
    procedure ChangeNamePrefix(const AOld, ANew: String);
  end;

  TBroadcaster = class;

  { TListener }

  TListener = class
  private
    FBroadcaster: TBroadcaster;
    FOnNotify: TNotifyEvent;
    FRef: PPointer;
    function GetIsListening: Boolean;
  public
    constructor Create(ARef: PPointer; AOnNotify: TNotifyEvent);
    destructor Destroy; override;
    procedure Forget; virtual;
    procedure Notify(ASender: TObject);
    property IsListening: Boolean read GetIsListening;
    property OnNotify: TNotifyEvent read FOnNotify write FOnNotify;
  end;

  { TBroadcaster }

  TBroadcaster = class(TFPList)
  private
    FLocked: Boolean;
  public
    destructor Destroy; override;
  public
    procedure Broadcast(ASender: TObject);
    procedure Subscribe(AListener: TListener);
    procedure Unsubscribe(AListener: TListener);
  public
    property Locked: Boolean read FLocked write FLocked;
  end;

  { TDrawDataItem }

  TDrawDataItem = class
  private
    FChart: TObject;
    FOwner: TObject;
  public
    constructor Create(AChart, AOwner: TObject);
    property Chart: TObject read FChart;
    property Owner: TObject read FOwner;
  end;

  TDrawDataItemClass = class of TDrawDataItem;

  { TDrawDataRegistry }

  TDrawDataRegistry = class
  private
    // Probably should be replaced by more efficiend data structure.
    FItems: TFPList;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure Add(AItem: TDrawDataItem);
    procedure DeleteByChart(AChart: TObject);
    procedure DeleteByOwner(AOwner: TObject);
    function Find(AChart, AOwner: TObject): TDrawDataItem;
  end;

  // An ordered set of integers represented as a comma-separated string
  // for publushing as a single property.
  TPublishedIntegerSet = object
  strict private
    FAllSet: Boolean;
    FData: TIntegerDynArray;
    function GetAsString: String;
    function GetIsSet(AIndex: Integer): Boolean;
    procedure SetAllSet(AValue: Boolean);
    procedure SetAsString(AValue: String);
    procedure SetIsSet(AIndex: Integer; AValue: Boolean);
  public
    constructor Init;
  public
    property AllSet: Boolean read FAllSet write SetAllSet;
    function AsBooleans(AMax: Integer): TBooleanDynArray;
    property AsString: String read GetAsString write SetAsString;
    property IsSet[AIndex: Integer]: Boolean read GetIsSet write SetIsSet;
  end;

const
  PUB_INT_SET_ALL = '';
  PUB_INT_SET_EMPTY = '-';
  // 0-value, 1-percent, 2-label, 3-total, 4-xvalue
  SERIES_MARK_FORMATS: array [TSeriesMarksStyle] of String = (
    '', '',
    '%0:.9g', // smsValue
    '%1:.2f%%', // smsPercent
    '%2:s', // smsLabel
    '%2:s %1:.2f%%', // smsLabelPercent
    '%2:s %0:.9g', // smsLabelValue
    '%2:s', // smsLegend: not sure what it means, left for Delphi compatibility
    '%1:.2f%% of %3:g', // smsPercentTotal
    '%1:.2f%% of %3:g', // smsLabelPercentTotal
    '%4:.9g' // smsXValue
  );
  ZeroDoublePoint: TDoublePoint = (X: 0; Y: 0);
  EmptyDoubleRect: TDoubleRect = (coords: (0, 0, 0, 0));
  EmptyExtent: TDoubleRect =
    (coords: (Infinity, Infinity, NegInfinity, NegInfinity));
  CASE_OF_TWO: array [Boolean, Boolean] of TCaseOfTwo =
    ((cotNone, cotSecond), (cotFirst, cotBoth));

function BoundsSize(ALeft, ATop: Integer; ASize: TSize): TRect; inline;

function DoubleInterval(AStart, AEnd: Double): TDoubleInterval; inline;

procedure Exchange(var A, B: Integer); overload; inline;
procedure Exchange(var A, B: Double); overload; inline;
procedure Exchange(var A, B: TDoublePoint); overload; inline;
procedure Exchange(var A, B: String); overload; inline;

procedure ExpandRange(var ALo, AHi: Double; ACoeff: Double); inline;

function FormatIfNotEmpty(AFormat, AStr: String): String; inline;

function InterpolateRGB(AColor1, AColor2: Integer; ACoeff: Double): Integer;
function IntToColorHex(AColor: Integer): String; inline;
function IsNan(const APoint: TDoublePoint): Boolean; overload; inline;
function NumberOr(ANum: Double; ADefault: Double = 0.0): Double; inline;

function OrientToRad(AOrient: Integer): Double; inline;

function RadToDeg16(ARad: Double): Integer; inline;
function RadToOrient(ARad: Double): Integer; inline;

function RoundChecked(A: Double): Integer; inline;

procedure SetPropDefaults(AObject: TPersistent; APropNames: array of String);

// Accept both locale-specific and default decimal separators.
function StrToFloatDefSep(const AStr: String): Double;

// Call this to silence 'parameter is unused' hint
procedure Unused(const A1);
procedure Unused(const A1, A2);

procedure UpdateMinMax(AValue: Double; var AMin, AMax: Double); overload;
procedure UpdateMinMax(AValue: Integer; var AMin, AMax: Integer); overload;

function WeightedAverage(AX1, AX2, ACoeff: Double): Double; inline;

operator =(const A, B: TMethod): Boolean; overload; inline;

var
  DrawData: TDrawDataRegistry;
  ShowMessageProc: TShowMessageProc;

resourcestring
  tasFailedSubcomponentRename = 'Failed to rename components: %s';

implementation

uses
  StrUtils, TypInfo;

const
  ORIENTATION_UNITS_PER_DEG = 10;

function BoundsSize(ALeft, ATop: Integer; ASize: TSize): TRect; inline;
begin
  Result := Bounds(ALeft, ATop, ASize.cx, ASize.cy);
end;

function DoubleInterval(AStart, AEnd: Double): TDoubleInterval;
begin
  Result.FStart := AStart;
  Result.FEnd := AEnd;
end;

procedure Exchange(var A, B: Integer);
var
  t: Integer;
begin
  t := A;
  A := B;
  B := t;
end;

procedure Exchange(var A, B: Double);
var
  t: Double;
begin
  t := A;
  A := B;
  B := t;
end;

procedure Exchange(var A, B: TDoublePoint);
var
  t: TDoublePoint;
begin
  t := A;
  A := B;
  B := t;
end;

procedure Exchange(var A, B: String);
var
  t: String;
begin
  t := A;
  A := B;
  B := t;
end;

procedure ExpandRange(var ALo, AHi: Double; ACoeff: Double);
var
  d: Double;
begin
  d := AHi - ALo;
  ALo -= d * ACoeff;
  AHi += d * ACoeff;
end;

function FormatIfNotEmpty(AFormat, AStr: String): String;
begin
  if AStr = '' then
    Result := ''
  else
    Result := Format(AFormat, [AStr]);
end;

function InterpolateRGB(AColor1, AColor2: Integer; ACoeff: Double): Integer;
type
  TBytes = packed array [1..4] of Byte;
var
  c1: TBytes absolute AColor1;
  c2: TBytes absolute AColor2;
  r: TBytes absolute Result;
  i: Integer;
begin
  ACoeff := EnsureRange(ACoeff, 0.0, 1.0);
  for i := 1 to 4 do
    r[i] := Round(c1[i]  + (c2[i] - c1[i]) * ACoeff);
end;

function IntToColorHex(AColor: Integer): String;
begin
  if AColor = clTAColor then
    Result := '?'
  else
    Result := '$' + IntToHex(AColor, 6);
end;

function IsNan(const APoint: TDoublePoint): Boolean;
begin
  Result := IsNan(APoint.X) or IsNan(APoint.Y);
end;

function NumberOr(ANum: Double; ADefault: Double): Double;
begin
  Result := IfThen(IsNan(ANum), ADefault, ANum);
end;

function OrientToRad(AOrient: Integer): Double;
begin
  Result := DegToRad(AOrient / ORIENTATION_UNITS_PER_DEG);
end;

function RadToDeg16(ARad: Double): Integer;
begin
  Result := Round(RadToDeg(ARad) * 16);
end;

function RadToOrient(ARad: Double): Integer;
begin
  Result := Round(RadToDeg(ARad)) * ORIENTATION_UNITS_PER_DEG;
end;

function RoundChecked(A: Double): Integer;
begin
  Result := Round(EnsureRange(A, -MaxInt, MaxInt));
end;

procedure SetPropDefaults(AObject: TPersistent; APropNames: array of String);
var
  n: String;
  p: PPropInfo;
begin
  for n in APropNames do begin
    p := GetPropInfo(AObject, n);
    SetOrdProp(AObject, p, p^.Default);
  end;
end;

var
  DefSeparatorSettings: TFormatSettings;

function StrToFloatDefSep(const AStr: String): Double;
begin
  if
    not TryStrToFloat(AStr, Result, DefSeparatorSettings) and
    not TryStrToFloat(AStr, Result)
  then
    Result := 0.0;
end;

{$HINTS OFF}
procedure Unused(const A1);
begin
end;

procedure Unused(const A1, A2);
begin
end;
{$HINTS ON}

procedure UpdateMinMax(AValue: Double; var AMin, AMax: Double);
begin
  if IsNan(AValue) then exit;
  if AValue < AMin then
    AMin := AValue;
  if AValue > AMax then
    AMax := AValue;
end;

procedure UpdateMinMax(AValue: Integer; var AMin, AMax: Integer);
begin
  if AValue < AMin then
    AMin := AValue;
  if AValue > AMax then
    AMax := AValue;
end;

function WeightedAverage(AX1, AX2, ACoeff: Double): Double;
begin
  Result := AX1 * (1 - ACoeff) + AX2 * ACoeff;
end;

operator = (const A, B: TMethod): Boolean;
begin
  Result := (A.Code = B.Code) and (A.Data = B.Data);
end;

{ TTypedFPListEnumerator }

function TTypedFPListEnumerator.GetCurrent: T;
begin
  Result := T(inherited GetCurrent);
end;

{ TIndexedComponentList }

procedure TIndexedComponentList.ChangeNamePrefix(
  const AOld, ANew: String);
var
  failed: String;
  i: Integer;
begin
  failed := '';
  for i := 0 to Count - 1 do
    TIndexedComponent(Items[i]).ChangeNamePrefix(AOld, ANew, failed);
  if (failed <> '') and Assigned(ShowMessageProc) then
    ShowMessageProc(Format(tasFailedSubcomponentRename, [failed]));
end;

{ TIndexedComponent }

procedure TIndexedComponent.ChangeNamePrefix(
  const AOld, ANew: String; var AFailed: String);
begin
  if AnsiStartsStr(AOld, Name) then
    try
      Name := ANew + Copy(Name, Length(AOld) + 1, Length(Name));
    except on EComponentError do
      AFailed += IfThen(AFailed = '', '', ', ') + Name;
    end;
end;

{ TIntervalList }

procedure TIntervalList.AddPoint(APoint: Double); inline;
begin
  AddRange(APoint, APoint);
end;

procedure TIntervalList.AddRange(AStart, AEnd: Double);
var
  i: Integer;
  j: Integer;
  k: Integer;
begin
  i := 0;
  while (i <= High(FIntervals)) and (FIntervals[i].FEnd < AStart) do
    i += 1;
  if i <= High(FIntervals) then
    AStart := Min(AStart, FIntervals[i].FStart);
  j := High(FIntervals);
  while (j >= 0) and (FIntervals[j].FStart > AEnd) do
    j -= 1;
  if j >= 0 then
    AEnd := Max(AEnd, FIntervals[j].FEnd);
  if i < j then begin
    for k := j + 1 to High(FIntervals) do
      FIntervals[i + k - j] := FIntervals[j];
    SetLength(FIntervals, Length(FIntervals) - j + i);
  end
  else if i > j then begin
    SetLength(FIntervals, Length(FIntervals) + 1);
    for k := High(FIntervals) downto i + 1 do
      FIntervals[k] := FIntervals[k - 1];
  end;
  FIntervals[i] := DoubleInterval(AStart, AEnd);
  Changed;
end;

procedure TIntervalList.Assign(ASource: TIntervalList);
begin
  FEpsilon := ASource.FEpsilon;
  FIntervals := Copy(ASource.FIntervals);
end;

procedure TIntervalList.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TIntervalList.Clear;
begin
  FIntervals := nil;
  Changed;
end;

constructor TIntervalList.Create;
const
  DEFAULT_EPSILON = 1e-6;
begin
  FEpsilon := DEFAULT_EPSILON;
end;

function TIntervalList.GetInterval(AIndex: Integer): TDoubleInterval;
begin
  Result := FIntervals[AIndex];
end;

function TIntervalList.GetIntervalCount: Integer;
begin
  Result := Length(FIntervals);
end;

function TIntervalList.Intersect(
  var ALeft, ARight: Double; var AHint: Integer): Boolean;
var
  fi, li: Integer;
begin
  Result := false;
  if Length(FIntervals) = 0 then exit;

  AHint := Min(High(FIntervals), AHint);
  while (AHint > 0) and (FIntervals[AHint].FStart > ARight) do
    Dec(AHint);

  while
    (AHint <= High(FIntervals)) and (FIntervals[AHint].FStart <= ARight)
  do begin
    if FIntervals[AHint].FEnd >= ALeft then begin
      if not Result then fi := AHint;
      li := AHint;
      Result := true;
    end;
    Inc(AHint);
  end;

  if Result then begin
    ALeft := FIntervals[fi].FStart - Epsilon;
    ARight := FIntervals[li].FEnd + Epsilon;
  end;
end;

procedure TIntervalList.SetEpsilon(AValue: Double);
begin
  if FEpsilon = AValue then exit;
  if AValue <= 0 then
    raise EChartIntervalError.Create('Epsilon <= 0');
  FEpsilon := AValue;
  Changed;
end;

procedure TIntervalList.SetOnChange(AValue: TNotifyEvent);
begin
  if TMethod(FOnChange) = TMethod(AValue) then exit;
  FOnChange := AValue;
end;

{ TListener }

constructor TListener.Create(ARef: PPointer; AOnNotify: TNotifyEvent);
begin
  FOnNotify := AOnNotify;
  FRef := ARef;
end;

destructor TListener.Destroy;
begin
  if IsListening then
    FBroadcaster.Unsubscribe(Self);
  inherited;
end;

procedure TListener.Forget;
begin
  FBroadcaster := nil;
  if FRef <> nil then
    FRef^ := nil;
end;

function TListener.GetIsListening: Boolean;
begin
  Result := FBroadcaster <> nil;
end;

procedure TListener.Notify(ASender: TObject);
begin
  if Assigned(FOnNotify) then
    FOnNotify(ASender)
end;

{ TBroadcaster }

procedure TBroadcaster.Broadcast(ASender: TObject);
var
  p: Pointer;
begin
  if Locked then exit;
  for p in Self do
    TListener(p).Notify(ASender);
end;

destructor TBroadcaster.Destroy;
var
  p: Pointer;
begin
  for p in Self do
    TListener(p).Forget;
  inherited;
end;

procedure TBroadcaster.Subscribe(AListener: TListener);
begin
  if AListener.IsListening then
    raise EListenerError.Create('Listener subscribed twice');
  if IndexOf(AListener) >= 0 then
    raise EListenerError.Create('Duplicate listener');
  AListener.FBroadcaster := Self;
  Add(AListener);
end;

procedure TBroadcaster.Unsubscribe(AListener: TListener);
var
  i: Integer;
begin
  if not AListener.IsListening then
    raise EListenerError.Create('Listener not subscribed');
  AListener.Forget;
  i := IndexOf(AListener);
  if i < 0 then
    raise EListenerError.Create('Listener not found');
  Delete(i);
end;

{ TDrawDataItem }

constructor TDrawDataItem.Create(AChart, AOwner: TObject);
begin
  FChart := AChart;
  FOwner := AOwner;
end;

{ TDrawDataRegistry }

procedure TDrawDataRegistry.Add(AItem: TDrawDataItem);
begin
  if Find(AItem.Chart, AItem.Owner) <> nil then
    raise EDrawDataError.Create('Duplicate DrawData');
  FItems.Add(AItem);
end;

constructor TDrawDataRegistry.Create;
begin
  FItems := TFPList.Create;
end;

procedure TDrawDataRegistry.DeleteByChart(AChart: TObject);
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    with TDrawDataItem(FItems[i]) do
      if Chart = AChart then begin
        Free;
        FItems[i] := nil;
      end;
  FItems.Pack;
end;

procedure TDrawDataRegistry.DeleteByOwner(AOwner: TObject);
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    with TDrawDataItem(FItems[i]) do
      if Owner = AOwner then begin
        Free;
        FItems[i] := nil;
      end;
  FItems.Pack;
end;

destructor TDrawDataRegistry.Destroy;
begin
  if FItems.Count > 0 then
    raise EDrawDataError.Create('DrawData leak');
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TDrawDataRegistry.Find(AChart, AOwner: TObject): TDrawDataItem;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do begin
    Result := TDrawDataItem(FItems[i]);
    if (Result.Chart = AChart) and (Result.Owner = AOwner) then exit;
  end;
  Result := nil;
end;

{ TPublishedIntegerSet }

function TPublishedIntegerSet.AsBooleans(AMax: Integer): TBooleanDynArray;
var
  i: Integer;
begin
  SetLength(Result, AMax);
  if AllSet then begin
    FillChar(Result[0], Length(Result), true);
    exit;
  end;
  for i in FData do
    if InRange(i, 0, High(Result)) then
      Result[i] := true;
end;

function TPublishedIntegerSet.GetAsString: String;
var
  i: Integer;
begin
  if AllSet then
    Result := PUB_INT_SET_ALL
  else if Length(FData) = 0 then
    Result := PUB_INT_SET_EMPTY
  else begin
    Result := IntToStr(FData[0]);
    for i := 1 to High(FData) do
      Result += ',' + IntToStr(FData[i]);
  end;
end;

function TPublishedIntegerSet.GetIsSet(AIndex: Integer): Boolean;
var
  i: Integer;
begin
  Result := true;
  if AllSet then exit;
  for i in FData do
    if i = AIndex then exit;
  Result := false;
end;

constructor TPublishedIntegerSet.Init;
begin
  FAllSet := true;
end;

procedure TPublishedIntegerSet.SetAllSet(AValue: Boolean);
begin
  if FAllSet = AValue then exit;
  FAllSet := AValue;
  if FAllSet then
    SetLength(FData, 0);
end;

procedure TPublishedIntegerSet.SetAsString(AValue: String);
var
  sl: TStringList;
  i, p: Integer;
  s: String;
begin
  AllSet := AValue = PUB_INT_SET_ALL;
  if AllSet then exit;
  sl := TStringList.Create;
  try
    sl.CommaText := AValue;
    SetLength(FData, sl.Count);
    i := 0;
    for s in sl do
      if TryStrToInt(s, p) then begin
        FData[i] := p;
        i += 1;
      end;
  finally
    sl.Free;
  end;
  SetLength(FData, i);
end;

procedure TPublishedIntegerSet.SetIsSet(AIndex: Integer; AValue: Boolean);
var
  i, j: Integer;
begin
  if AllSet or (IsSet[AIndex] = AValue) then exit;
  if AValue then begin
    SetLength(FData, Length(FData) + 1);
    FData[High(FData)] := AIndex;
  end
  else begin
    j := 0;
    for i := 0 to High(FData) do
      if FData[i] <> AIndex then begin
        FData[j] := FData[i];
        j += 1;
      end;
    SetLength(FData, j);
  end;
end;

initialization

  DrawData := TDrawDataRegistry.Create;
  DefSeparatorSettings := DefaultFormatSettings;
  DefSeparatorSettings.DecimalSeparator := '.';

finalization

  FreeAndNil(DrawData);

end.
