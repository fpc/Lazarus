{
 /***************************************************************************
                               TAChartUtils.pas
                               ----------------
              Component Library Standard Graph Utiliity Functions


 ***************************************************************************/

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Luнs Rodrigues, Philippe Martinole, Alexander Klenin

}

unit TAChartUtils;

{$H+}
{$IF FPC_FULLVERSION<20700}
{$OPTIMIZATION NOSTACKFRAME}
{$ENDIF}

interface

uses
  Classes, Math, Types, SysUtils;

const
  CHART_COMPONENT_IDE_PAGE = 'Chart';
  PERCENT = 0.01;
  clTAColor = $20000000; // = clDefault, but avoiding dependency on Graphics
  DEFAULT_FONT_SIZE = 10;
  DEFAULT_EPSILON = 1e-6;
  RANGE_EPSILON = 1e-12;

  // Replacement for +INF, Canvas does not work correctly when MaxInt is used.
  // Any screen coordinates are clipped to range -MAX_COORD ... MAX_COORD.
  MAX_COORD = 100*1000*1000;

type
  EChartError = class(Exception);
  EChartIntervalError = class(EChartError);
  EBroadcasterError = class(EChartError);
  EListenerError = class(EChartError);
  EDrawDataError = class(EChartError);

  // Like TColor, but avoiding dependency on Graphics.
  TChartColor = -$7FFFFFFF-1..$7FFFFFFF;

  // dto with TFontStyle
  TChartFontStyle = (cfsBold, cfsItalic, cfsUnderline, cfsStrikeout);
  TChartFontStyles = set of TChartFontStyle;

  TChartTextFormat = (tfNormal, tfHTML);

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
  TDoublePointArray = array of TDoublepoint;

  TChartDistance = 0..MaxInt;

  TPercent = 0..100;

  TPointDistFunc = function (const A, B: TPoint): Integer;

  TTransformFunc = function (A: Double): Double of object;
  TImageToGraphFunc = function (AX: Integer): Double of object;
  TGraphToImageFunc = function (AX: Double): Integer of object;

  TChartUnits = (cuPercent, cuAxis, cuGraph, cuPixel);

  TOverrideColor = (ocBrush, ocPen);
  TOverrideColors = set of TOverrideColor;

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

  TIntervalOption = (ioOpenStart, ioOpenEnd);
  TIntervalOptions = set of TIntervalOption;

  TDoubleInterval = record
    FStart, FEnd: Double;
  end;

  TPointBoolArr = array [Boolean] of Integer;
  TDoublePointBoolArr = array [Boolean] of Double;

  TNearestPointTarget = (
    nptPoint,   // Look for the nearest point at (x, y)
    nptXList,   // Check additional x values in XList
    nptYList,   // Check additional y values in YList
    nptCustom   // Depends on series type (e.g., TBarSeries --> click inside bar.)
  );

  TNearestPointTargets = set of TNearestPointTarget;

  { TIntervalList }

  TIntervalList = class
  private
    FEpsilon: Double;
    FIntervals: array of TDoubleInterval;
    FOnChange: TNotifyEvent;
    procedure Changed;
    function GetInterval(AIndex: Integer): TDoubleInterval;
    function GetIntervalCount: Integer;
    procedure SetOnChange(AValue: TNotifyEvent);
  public
    procedure Assign(ASource: TIntervalList);
    constructor Create;
  public
    procedure AddPoint(APoint: Double); inline;
    procedure AddRange(AStart, AEnd: Double; ALimits: TIntervalOptions = []);
    procedure Clear;
    function Intersect(
      var ALeft, ARight: Double; var AHint: Integer): Boolean;
  public
    property Epsilon: Double read FEpsilon write FEpsilon;
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
  // for publishing as a single property.
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
    function AsBooleans(ACount: Integer): TBooleanDynArray;
    property AsString: String read GetAsString write SetAsString;
    property IsSet[AIndex: Integer]: Boolean read GetIsSet write SetIsSet;
  end;

  // A limited capacity stack to store 'undo'-like history.
  generic THistory<TElem> = class
  strict private
    FCount: Cardinal;
    FData: array of TElem;

    function GetCapacity: Cardinal; inline;
    function GetItem(AIndex: Integer): TElem;
    procedure SetCapacity(AValue: Cardinal);
    procedure DeleteOld(ACount: Integer);
  public
    procedure Add(const AItem: TElem);
    function Pop: TElem; inline;

    property Capacity: Cardinal read GetCapacity write SetCapacity;
    property Count: Cardinal read FCount;
    property Item[AIndex: Integer]: TElem read GetItem; default;
  end;

  PStr = ^String;  // PString is declared in system and in objpas!

  TClassRegistryItem = class
    FClass: TClass;
    FCaption: String;
    FCaptionPtr: PStr;
    constructor Create(AClass: TClass; const ACaption: String);
    constructor CreateRes(AClass: TClass; ACaptionPtr: PStr);
  end;

  TClassRegistry = class(TFPList)
  public
    destructor Destroy; override;
    procedure Clear;
    function GetCaption(AIndex: Integer): String;
    function GetClass(AIndex: Integer): TClass;
    function IndexOfClass(AClass: TClass): Integer;
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
    '%2:s %1:.2f%% of %3:g', // smsLabelPercentTotal
    '%4:.9g' // smsXValue
  );
  ZeroDoublePoint: TDoublePoint = (X: 0; Y: 0);
  ZeroRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
  EmptyDoubleRect: TDoubleRect = (coords: (0, 0, 0, 0));
  EmptyExtent: TDoubleRect =
    (coords: (Infinity, Infinity, NegInfinity, NegInfinity));
  CASE_OF_TWO: array [Boolean, Boolean] of TCaseOfTwo =
    ((cotNone, cotSecond), (cotFirst, cotBoth));
  ORIENTATION_UNITS_PER_DEG = 10;

var
  DefSeparatorSettings: TFormatSettings;

function BoundsSize(ALeft, ATop: Integer; ASize: TSize): TRect; inline;

function ChopString(const AString: String; const AMaxLen: Integer): String;

function Deg16ToRad(ADeg16: Integer): Double; inline;
function DoubleInterval(AStart, AEnd: Double): TDoubleInterval; inline;

procedure Exchange(var A, B: Integer); overload; inline;
procedure Exchange(var A, B: Double); overload; inline;
procedure Exchange(var A, B: TDoublePoint); overload; inline;
procedure Exchange(var A, B: String); overload; inline;

function FormatIfNotEmpty(AFormat, AStr: String): String; inline;

function IfThen(ACond: Boolean; ATrue, AFalse: TObject): TObject; overload;
function ImgRoundChecked(A: Double): Integer; inline;
function InterpolateRGB(AColor1, AColor2: Integer; ACoeff: Double): Integer;
function IntToColorHex(AColor: Integer): String; inline;
function IsEquivalent(const A1, A2: Double): Boolean; inline;
function IsNan(const APoint: TDoublePoint): Boolean; overload; inline;
function NameOrClassName(AComponent: TComponent): String; inline;
function NumberOr(ANum: Double; ADefault: Double = 0.0): Double; inline;

function OrientToRad(AOrient: Integer): Double; inline;

function RadToDeg16(ARad: Double): Integer; inline;
function RadToOrient(ARad: Double): Integer; inline;

function RoundChecked(A: Double): Integer; inline;

procedure SetPropDefaults(AObject: TPersistent; APropNames: array of String);

function Split(
  AString: String; ADest: TStrings = nil; ADelimiter: Char = '|'): TStrings;

// Accept both locale-specific and default decimal separators.
function StrToFloatDefSep(const AStr: String; ADefault: Double = 0.0): Double;
// .. or date/time values
function StrToFloatOrDateTimeDef(const AStr: String): Double;

// Call this to silence 'parameter is unused' hint
procedure Unused(const A1);
procedure Unused(const A1, A2);
procedure Unused(const A1, A2, A3);

procedure UpdateMinMax(AValue: Double; var AMin, AMax: Double); overload;
procedure UpdateMinMax(AValue: Integer; var AMin, AMax: Integer); overload;

function WeightedAverage(AX1, AX2, ACoeff: Double): Double; inline;

operator =(const A, B: TMethod): Boolean; overload; inline;

var
  DrawData: TDrawDataRegistry;
  ShowMessageProc: TShowMessageProc;


implementation

uses
  StrUtils, TypInfo, TAChartStrConsts;

function BoundsSize(ALeft, ATop: Integer; ASize: TSize): TRect; inline;
begin
  Result := Bounds(ALeft, ATop, ASize.cx, ASize.cy);
end;

function ChopString(const AString: String; const AMaxLen: Integer): String;
begin
  if (Length(AString) > AMaxlen) and (AMaxLen > 3) then
    Result := copy(AString, 1, AMaxLen - 3) + '...'
  else
    Result := AString;
end;

function Deg16ToRad(ADeg16: Integer): Double;
begin
  Result := DegToRad(ADeg16 / 16);
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

function FormatIfNotEmpty(AFormat, AStr: String): String;
begin
  if AStr = '' then
    Result := ''
  else
    Result := Format(AFormat, [AStr]);
end;

function IfThen(ACond: Boolean; ATrue, AFalse: TObject): TObject;
begin
  if ACond then
    Result := ATrue
  else
    Result := AFalse;
end;

function ImgRoundChecked(A: Double): Integer;
begin
  Result := Round(EnsureRange(A, -MAX_COORD, MAX_COORD));
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

function IsEquivalent(const A1, A2: Double): Boolean;
begin
  Result := CompareDWord(A1, A2, SizeOf(A1) div SizeOf(DWord)) = 0;
end;

function IsNan(const APoint: TDoublePoint): Boolean;
begin
  Result := IsNan(APoint.X) or IsNan(APoint.Y);
end;

function NameOrClassName(AComponent: TComponent): String;
begin
  if AComponent = nil then
    Result := '<nil>'
  else
  if AComponent.Name = '' then
    Result := AComponent.ClassName
  else
    Result := AComponent.Name;
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

function Split(AString: String; ADest: TStrings; ADelimiter: Char): TStrings;
begin
  Result := ADest;
  if Result = nil then
    Result := TStringList.Create;
  Result.Delimiter := ADelimiter;
  Result.StrictDelimiter := true;
  Result.DelimitedText := AString;
end;

function StrToFloatDefSep(const AStr: String; ADefault: Double = 0.0): Double;
begin
  if
    not TryStrToFloat(AStr, Result, DefSeparatorSettings) and
    not TryStrToFloat(AStr, Result)
  then
    Result := ADefault;
end;

function StrToFloatOrDateTimeDef(const AStr: String): Double;
begin
  if
    not TryStrToFloat(AStr, Result, DefSeparatorSettings) and
    not TryStrToFloat(AStr, Result) and
    not TryStrToDateTime(AStr, Result)
  then
    Result := 0.0;
end;

{$PUSH}{$HINTS OFF}
procedure Unused(const A1);
begin
end;

procedure Unused(const A1, A2);
begin
end;

procedure Unused(const A1, A2, A3);
begin
end;
{$POP}

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

{ THistory }

procedure THistory.Add(const AItem: TElem);
begin
  if Capacity = 0 then exit;
  if FCount = Capacity then
    DeleteOld(1);
  FData[FCount] := AItem;
  FCount += 1;
end;

procedure THistory.DeleteOld(ACount: Integer);
begin
  FCount -= ACount;
  Move(FData[ACount], FData[0], SizeInt(FCount) * SizeOf(FData[0]));
end;

function THistory.GetCapacity: Cardinal;
begin
  Result := Length(FData);
end;

function THistory.GetItem(AIndex: Integer): TElem;
begin
  if AIndex < 0 then
    AIndex += Integer(FCount);
  Result := FData[AIndex];
end;

function THistory.Pop: TElem;
begin
  Result := GetItem(-1);
  FCount -= 1;
end;

procedure THistory.SetCapacity(AValue: Cardinal);
begin
  if Capacity = AValue then exit;
  if AValue < FCount then
    DeleteOld(FCount - AValue);
  SetLength(FData, AValue);
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

procedure TIntervalList.AddRange(AStart, AEnd: Double; ALimits: TIntervalOptions = []);
var
  i: Integer;
  j: Integer;
  k: Integer;
begin
  if not (ioOpenStart in ALimits) then AStart -= FEpsilon;
  if not (ioOpenEnd in ALimits) then AEnd += FEpsilon;
  if AStart > AEnd then exit;

  // In most cases we will be adding ranges in the ascending order,
  // so the code here is optimized for this case

  // Find index of the first interval, having its FEnd >= AStart
  i := High(FIntervals) + 1;
  while (i > 0) and (FIntervals[i-1].FEnd >= AStart) do
    i -= 1;
  if i <= High(FIntervals) then
    AStart := Min(AStart, FIntervals[i].FStart);

  // Find index of the last interval, having its FStart <= AEnd
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
begin
  Result := false;
  if (Length(FIntervals) = 0) or (ALeft > ARight) then exit;

  AHint := EnsureRange(AHint, 0, High(FIntervals));
  while (AHint > 0) and (FIntervals[AHint].FStart > ALeft) do
    Dec(AHint);

  while
    (AHint <= High(FIntervals)) and (FIntervals[AHint].FStart < ARight)
  do begin
    if FIntervals[AHint].FEnd > ALeft then begin
      ALeft := FIntervals[AHint].FStart;
      ARight := FIntervals[AHint].FEnd;
      exit(true);
    end;
    Inc(AHint);
  end;
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
  ListCopy: array of Pointer;
  Exceptions: TStringList;
  Aborted: Boolean;
  i: Integer;
begin
  if Locked then exit;
  if Count = 0 then exit;

  // Listeners can remove themselves when being notified, which
  // changes the list - so we must use a copy of the list when
  // notifying, to avoid omissions in notifying
  SetLength(ListCopy, Count);
  for i := 0 to High(ListCopy) do
    ListCopy[i] := List^[i];

  Exceptions := nil;
  Aborted := False;
  try
    for i := 0 to High(ListCopy) do
    try
      TListener(ListCopy[i]).Notify(ASender);
    except
      on E: Exception do
        if E is EAbort then
          Aborted := true
        else begin
          if not Assigned(Exceptions) then begin
            Exceptions := TStringList.Create;
            Exceptions.Duplicates := dupIgnore;
            Exceptions.Sorted := true; // required by dupIgnore
          end;
          Exceptions.Add(E.Message);
        end;
    end;
    if Assigned(Exceptions) then
      raise EBroadcasterError.Create(Trim(Exceptions.Text));
    if Aborted then
      Abort;
  finally
    Exceptions.Free;
  end;
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

function TPublishedIntegerSet.AsBooleans(ACount: Integer): TBooleanDynArray;
var
  i: Integer;
begin
  SetLength(Result, ACount);
  if ACount = 0 then exit;
  if AllSet then
    FillChar(Result[0], Length(Result), true)
  else
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
  i, p, pc, ps, pp: Integer;
  s: String;
begin
  AllSet := AValue = PUB_INT_SET_ALL;
  if AllSet then exit;
  sl := TStringList.Create;
  try
    pc := pos(',', AValue);
    ps := pos(';', AValue);
    pp := pos('|', AValue);
    if (pc = 0) and (ps = 0) and (pp = 0) then
      if TryStrToInt(AValue, i) then begin
        SetLength(FData, 1);
        FData[0] := i;
        exit;
      end;
    if pc > 0 then
      sl.CommaText := AValue
    else if ps > 0 then begin
      sl.Delimiter := ';';
      sl.DelimitedText := AValue;
    end else if pp > 0 then begin
      sl.Delimiter := '|';
      sl.DelimitedText := AValue;
    end;
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


{ TClassRegistryItem }

constructor TClassRegistryItem.Create(AClass: TClass; const ACaption: String);
begin
  FClass := AClass;
  FCaption := ACaption;
end;

constructor TClassRegistryItem.CreateRes(AClass: TClass; ACaptionPtr: PStr);
begin
  FClass := AClass;
  FCaptionPtr := ACaptionPtr;
  if FCaptionPtr <> nil then FCaption := ACaptionPtr^;
end;


{ TClassRegistry }

destructor TClassRegistry.Destroy;
begin
  Clear;
  inherited;
end;

procedure TClassRegistry.Clear;
var
  i: Integer;
begin
  for i:= Count-1 downto 0 do
    TObject(Items[i]).Free;
  inherited;
end;

function TClassRegistry.GetCaption(AIndex: Integer): String;
var
  item: TClassRegistryItem;
begin
  item := TClassRegistryItem(Items[AIndex]);
  if item.FCaptionPtr <> nil then
    Result := item.FCaptionPtr^ else
    Result := item.FCaption;
end;

function TClassRegistry.GetClass(AIndex: Integer): TClass;
begin
  Result := TClassRegistryItem(Items[AIndex]).FClass;
end;

function TClassRegistry.IndexOfClass(AClass: TClass): Integer;
begin
  for Result := 0 to Count-1 do
    if TClassRegistryItem(Items[Result]).FClass = AClass then
      exit;
  Result := -1;
end;


initialization

  DrawData := TDrawDataRegistry.Create;
  DefSeparatorSettings := DefaultFormatSettings;
  DefSeparatorSettings.DecimalSeparator := '.';

finalization

  FreeAndNil(DrawData);

end.
