{

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}

unit TACustomSource;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, Types, TAChartUtils;

type
  TAxisIntervalParamOption = (
    aipGraphCoords,
    aipUseCount, aipUseMaxLength, aipUseMinLength, aipUseNiceSteps);

const
  DEF_INTERVAL_STEPS = '0.2|0.5|1.0';
  DEF_INTERVAL_OPTIONS = [aipUseMaxLength, aipUseMinLength, aipUseNiceSteps];

type
  TAxisIntervalParamOptions = set of TAxisIntervalParamOption;

  TChartAxisIntervalParams = class(TPersistent)
  strict private
    FCount: Integer;
    FMaxLength: Integer;
    FMinLength: Integer;
    FNiceSteps: String;
    FOptions: TAxisIntervalParamOptions;
    FOwner: TPersistent;
    FStepValues: TDoubleDynArray;
    FTolerance: Cardinal;
    function NiceStepsIsStored: Boolean;
    procedure ParseNiceSteps;
    procedure SetCount(AValue: Integer);
    procedure SetMaxLength(AValue: Integer);
    procedure SetMinLength(AValue: Integer);
    procedure SetNiceSteps(const AValue: String);
    procedure SetOptions(AValue: TAxisIntervalParamOptions);
    procedure SetTolerance(AValue: Cardinal);
  strict protected
    procedure Changed; virtual;
  protected
    function GetOwner: TPersistent; override;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TPersistent);
    property StepValues: TDoubleDynArray read FStepValues;
  published
    property Count: Integer read FCount write SetCount default 5;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 50;
    property MinLength: Integer read FMinLength write SetMinLength default 10;
    property NiceSteps: String
      read FNiceSteps write SetNiceSteps stored NiceStepsIsStored;
    property Options: TAxisIntervalParamOptions
      read FOptions write SetOptions default DEF_INTERVAL_OPTIONS;
    property Tolerance: Cardinal read FTolerance write SetTolerance default 0;
  end;

type
  EBufferError = class(EChartError);
  EEditableSourceRequired = class(EChartError);
  EXCountError = class(EChartError);
  EYCountError = class(EChartError);

  TChartValueText = record
    FText: String;
    FValue: Double;
  end;
  PChartValueText = ^TChartValueText;

  TChartValueTextArray = array of TChartValueText;

  TChartDataItem = packed record
  public
    X, Y: Double;
    Color: TChartColor;
    Text: String;
    XList: TDoubleDynArray;
    YList: TDoubleDynArray;
    function GetX(AIndex: Integer): Double;
    function GetY(AIndex: Integer): Double;
    procedure SetX(AIndex: Integer; AValue: Double);
    procedure SetX(AValue: Double);
    procedure SetY(AIndex: Integer; AValue: Double);
    procedure SetY(AValue: Double);
    procedure MultiplyY(ACoeff: Double);
    function Point: TDoublePoint; inline;
  end;
  PChartDataItem = ^TChartDataItem;

  TGraphToImageFunc = function (AX: Double): Integer of object;
  TIntegerTransformFunc = function (AX: Integer): Integer of object;

  TValuesInRangeParams = object
    FAxisToGraph: TTransformFunc;
    FFormat: String;
    FGraphToAxis: TTransformFunc;
    FGraphToImage: TGraphToImageFunc;
    FIntervals: TChartAxisIntervalParams;
    FMin, FMax: Double;
    FMinStep: Double;
    FScale: TIntegerTransformFunc;
    FUseY: Boolean;

    function CountToStep(ACount: Integer): Double; inline;
    function IsAcceptableStep(AStep: Int64): Boolean; inline;
    procedure RoundToImage(var AValue: Double);
    function ToImage(AX: Double): Integer; inline;
  end;

  TBasicChartSource = class(TComponent)
  strict private
    FBroadcaster: TBroadcaster;
  strict protected
    FUpdateCount: Integer;
    procedure Notify;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function IsUpdating: Boolean; inline;
    property Broadcaster: TBroadcaster read FBroadcaster;
  end;

  TCustomChartSource = class;

  TCustomChartSourceEnumerator = class
  strict private
    FSource: TCustomChartSource;
    FIndex: Integer;
  public
    constructor Create(ASource: TCustomChartSource);
    function GetCurrent: PChartDataItem;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: PChartDataItem read GetCurrent;
  end;

  TChartErrorBarKind = (ebkNone, ebkConst, ebkPercent, ebkChartSource);

  TChartErrorBarData = class(TPersistent)
  private
    FKind: TChartErrorBarKind;
    FValue: array[0..1] of Double;  // 0 = positive, 1 = negative
    FIndex: array[0..1] of Integer;
    FOnChange: TNotifyEvent;
    procedure Changed;
    function GetIndex(AIndex: Integer): Integer;
    function GetValue(AIndex: Integer): Double;
    function IsErrorBarValueStored(AIndex: Integer): Boolean;
    procedure SetKind(AValue: TChartErrorbarKind);
    procedure SetIndex(AIndex, AValue: Integer);
    procedure SetValue(AIndex: Integer; AValue: Double);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Kind: TChartErrorBarKind read FKind write SetKind default ebkNone;
    property IndexMinus: Integer index 1 read GetIndex write SetIndex default -1;
    property IndexPlus: Integer index 0 read GetIndex write SetIndex default -1;
    property ValueMinus: Double index 1 read GetValue write SetValue stored IsErrorBarValueStored;
    property ValuePlus: Double index 0 read GetValue write SetValue stored IsErrorBarValueStored;

  end;

  TCustomChartSource = class(TBasicChartSource)
  strict private
    FErrorBarData: array[0..1] of TChartErrorBarData;
    function GetErrorBarData(AIndex: Integer): TChartErrorBarData;
    function IsErrorBarDataStored(AIndex: Integer): Boolean;
    procedure SetErrorBarData(AIndex: Integer; AValue: TChartErrorBarData);
    procedure SortValuesInRange(
      var AValues: TChartValueTextArray; AStart, AEnd: Integer);
  strict protected
    FExtent: TDoubleRect;
    FExtentIsValid: Boolean;
    FValuesTotal: Double;
    FValuesTotalIsValid: Boolean;
    FXCount: Cardinal;
    FYCount: Cardinal;
    procedure ChangeErrorBars(Sender: TObject); virtual;
    function GetCount: Integer; virtual; abstract;
    function GetErrorBarValues(APointIndex: Integer; Which: Integer;
      out AUpperDelta, ALowerDelta: Double): Boolean;
    function GetHasErrorBars(Which: Integer): Boolean;
    function GetItem(AIndex: Integer): PChartDataItem; virtual; abstract;
    procedure InvalidateCaches;
    procedure SetXCount(AValue: Cardinal); virtual; abstract;
    procedure SetYCount(AValue: Cardinal); virtual; abstract;
    property XErrorBarData: TChartErrorBarData index 0 read GetErrorBarData
      write SetErrorBarData stored IsErrorBarDataStored;
    property YErrorBarData: TChartErrorBarData index 1 read GetErrorBarData
      write SetErrorBarData stored IsErrorBarDataStored;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure AfterDraw; virtual;
    procedure BeforeDraw; virtual;
    procedure EndUpdate; override;
  public
    class procedure CheckFormat(const AFormat: String);
    function Extent: TDoubleRect; virtual;
    function ExtentCumulative: TDoubleRect; virtual;
    function ExtentList: TDoubleRect; virtual;
    procedure FindBounds(AXMin, AXMax: Double; out ALB, AUB: Integer);
    function FormatItem(
      const AFormat: String; AIndex, AYIndex: Integer): String; inline;
    function FormatItemXYText(
      const AFormat: String; AX, AY: Double; AText: String): String;
    function GetEnumerator: TCustomChartSourceEnumerator;
    function GetXErrorBarLimits(APointIndex: Integer;
      out AUpperLimit, ALowerLimit: Double): Boolean;
    function GetYErrorBarLimits(APointIndex: Integer;
      out AUpperLimit, ALowerLimit: Double): Boolean;
    function GetXErrorBarValues(APointIndex: Integer;
      out AUpperDelta, ALowerDelta: Double): Boolean;
    function GetYErrorBarValues(APointIndex: Integer;
      out AUpperDelta, ALowerDelta: Double): Boolean;
    function HasXErrorBars: Boolean;
    function HasYErrorBars: Boolean;
    function IsXErrorIndex(AXIndex: Integer): Boolean;
    function IsYErrorIndex(AYIndex: Integer): Boolean;
    function IsSorted: Boolean; virtual;
    procedure ValuesInRange(
      AParams: TValuesInRangeParams; var AValues: TChartValueTextArray); virtual;
    function ValuesTotal: Double; virtual;
    function XOfMax(AIndex: Integer = 0): Double;
    function XOfMin(AIndex: Integer = 0): Double;

    property Count: Integer read GetCount;
    property Item[AIndex: Integer]: PChartDataItem read GetItem; default;
    property XCount: Cardinal read FXCount write SetXCount default 1;
    property YCount: Cardinal read FYCount write SetYCount default 1;
  end;

  { TChartSourceBuffer }

  TChartSourceBuffer = class
  strict private
    FBuf: array of TChartDataItem;
    FCount: Cardinal;
    FStart: Cardinal;
    FSum: TChartDataItem;
    procedure AddValue(const AItem: TChartDataItem);
    function EndIndex: Cardinal; inline;
    function GetCapacity: Cardinal; inline;
    procedure SetCapacity(AValue: Cardinal); inline;
  public
    procedure AddFirst(const AItem: TChartDataItem);
    procedure AddLast(const AItem: TChartDataItem);
    procedure Clear; inline;
    function GetPtr(AOffset: Cardinal): PChartDataItem; overload;
    procedure GetSum(var AItem: TChartDataItem);
    procedure RemoveFirst;
    procedure RemoveLast;
    procedure RemoveValue(const AItem: TChartDataItem);
    property Capacity: Cardinal read GetCapacity write SetCapacity;
  end;

procedure SetDataItemDefaults(var AItem: TChartDataItem);

implementation

uses
  Math, StrUtils, SysUtils, TAMath;

function CompareChartValueTextPtr(AItem1, AItem2: Pointer): Integer;
begin
  Result := CompareValue(
    PChartValueText(AItem1)^.FValue,
    PChartValueText(AItem2)^.FValue);
end;

function IsValueTextsSorted(
  const AValues: TChartValueTextArray; AStart, AEnd: Integer): Boolean;
var
  i: Integer;
begin
  for i := AStart to AEnd - 1 do
    if AValues[i].FValue > AValues[i + 1].FValue then exit(false);
  Result := true;
end;

procedure SetDataItemDefaults(var AItem: TChartDataItem);
var
  i: Integer;
begin
  AItem.X := 0;
  AItem.Y := 0;
  AItem.Color := clTAColor;
  AItem.Text := '';
  for i := 0 to High(AItem.YList) do
    AItem.YList[i] := 0;
end;

{ TValuesInRangeParams }

function TValuesInRangeParams.CountToStep(ACount: Integer): Double;
begin
  Result := Power(10, Floor(Log10((FMax - FMin) / ACount)));
end;

function TValuesInRangeParams.IsAcceptableStep(AStep: Int64): Boolean;
begin
  with FIntervals do
    Result := not (
      (aipUseMinLength in Options) and (AStep < FScale(MinLength)) or
      (aipUseMaxLength in Options) and (AStep > FScale(MaxLength)));
end;

procedure TValuesInRangeParams.RoundToImage(var AValue: Double);

  function A2I(AX: Double): Integer; inline;
  begin
    Result := FGraphToImage(FAxisToGraph(AX));
  end;

var
  p, rv: Double;
  x: Int64;
begin
  if
    (FIntervals.Tolerance = 0) or (AValue = 0) or IsInfinite(AValue) or IsNan(AValue)
  then
    exit;
  x := A2I(AValue);
  p := Power(10, Floor(Log10(Abs(AValue)) - Log10(High(Int64)) + 1));
  while AValue <> 0 do begin
    rv := Round(AValue / p) * p;
    if Abs(A2I(rv) - x) >= FIntervals.Tolerance then break;
    AValue := rv;
    p *= 10;
  end;
end;

function TValuesInRangeParams.ToImage(AX: Double): Integer;
begin
  if not (aipGraphCoords in FIntervals.Options) then
    AX := FAxisToGraph(AX);
  Result := FGraphToImage(AX);
end;

{ TChartAxisIntervalParams }

procedure TChartAxisIntervalParams.Assign(ASource: TPersistent);
begin
  if ASource is TChartAxisIntervalParams then
    with TChartAxisIntervalParams(ASource) do begin
      Self.FCount := Count;
      Self.FMaxLength := MaxLength;
      Self.FMinLength := MinLength;
      Self.FNiceSteps := NiceSteps;
      Self.FOptions := Options;
    end
  else
    inherited Assign(ASource);
end;

procedure TChartAxisIntervalParams.Changed;
begin
  if not (FOwner is TCustomChartSource) then exit;
  with FOwner as TCustomChartSource do begin
    BeginUpdate;
    EndUpdate;
  end;
end;

constructor TChartAxisIntervalParams.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  SetPropDefaults(Self, ['Count', 'MaxLength', 'MinLength', 'Options']);
  FNiceSteps := DEF_INTERVAL_STEPS;
  ParseNiceSteps;
end;

function TChartAxisIntervalParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TChartAxisIntervalParams.NiceStepsIsStored: Boolean;
begin
  Result := NiceSteps <> DEF_INTERVAL_STEPS;
end;

procedure TChartAxisIntervalParams.ParseNiceSteps;
var
  parts: TStrings;
  i: Integer;
begin
  parts := Split(IfThen(NiceSteps = '', DEF_INTERVAL_STEPS, NiceSteps));
  try
    SetLength(FStepValues, parts.Count);
    for i := 0 to parts.Count - 1 do
      FStepValues[i] := StrToFloatDefSep(parts[i]);
  finally
    parts.Free;
  end;
end;

procedure TChartAxisIntervalParams.SetCount(AValue: Integer);
begin
  if FCount = AValue then exit;
  FCount := AValue;
  Changed;
end;

procedure TChartAxisIntervalParams.SetMaxLength(AValue: Integer);
begin
  if FMaxLength = AValue then exit;
  FMaxLength := AValue;
  Changed;
end;

procedure TChartAxisIntervalParams.SetMinLength(AValue: Integer);
begin
  if FMinLength = AValue then exit;
  FMinLength := AValue;
  Changed;
end;

procedure TChartAxisIntervalParams.SetNiceSteps(const AValue: String);
begin
  if FNiceSteps = AValue then exit;
  FNiceSteps := AValue;
  ParseNiceSteps;
  Changed;
end;

procedure TChartAxisIntervalParams.SetOptions(
  AValue: TAxisIntervalParamOptions);
begin
  if FOptions = AValue then exit;
  FOptions := AValue;
  Changed;
end;

procedure TChartAxisIntervalParams.SetTolerance(AValue: Cardinal);
begin
  if FTolerance = AValue then exit;
  FTolerance := AValue;
  Changed;
end;

{ TChartDataItem }

function TChartDataItem.GetX(AIndex: Integer): Double;
begin
  AIndex := EnsureRange(AIndex, 0, Length(XList));
  if AIndex = 0 then
    Result := X
  else
    Result := XList[AIndex - 1];
end;

function TChartDataItem.GetY(AIndex: Integer): Double;
begin
  AIndex := EnsureRange(AIndex, 0, Length(YList));
  if AIndex = 0 then
    Result := Y
  else
    Result := YList[AIndex - 1];
end;

procedure TChartDataItem.MultiplyY(ACoeff: Double);
var
  i: Integer;
begin
  Y *= ACoeff;
  for i := 0 to High(YList) do
    YList[i] *= ACoeff;
end;

function TChartDataItem.Point: TDoublePoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure TChartDataItem.SetX(AValue: Double);
var
  i: Integer;
begin
  X := AValue;
  for i := 0 to High(XList) do
    XList[i] := AValue;
end;

procedure TChartDataItem.SetX(AIndex: Integer; AValue: Double);
begin
  if AIndex = 0 then
    X := AValue
  else
    XList[AIndex - 1] := AValue;
end;

procedure TChartDataItem.SetY(AValue: Double);
var
  i: Integer;
begin
  Y := AValue;
  for i := 0 to High(YList) do
    YList[i] := AValue;
end;

procedure TChartDataItem.SetY(AIndex: Integer; AValue: Double);
begin
  if AIndex = 0 then
    Y := AValue
  else
    YList[AIndex - 1] := AValue;
end;


{ TBasicChartSource }

constructor TBasicChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBroadcaster := TBroadcaster.Create;
end;

destructor TBasicChartSource.Destroy;
begin
  FreeAndNil(FBroadcaster);
  inherited Destroy;
end;

procedure TBasicChartSource.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TBasicChartSource.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount > 0 then exit;
  Notify;
end;

function TBasicChartSource.IsUpdating: Boolean; inline;
begin
  Result := FUpdateCount > 0;
end;

procedure TBasicChartSource.Notify;
begin
  if not IsUpdating then
    FBroadcaster.Broadcast(Self);
end;


{ TChartSourceBuffer }

procedure TChartSourceBuffer.AddFirst(const AItem: TChartDataItem);
begin
  if Capacity = 0 then
    raise EBufferError.Create('');
  FStart := (FStart + Cardinal(High(FBuf))) mod Capacity;
  if FCount = Capacity then
    RemoveValue(FBuf[FStart])
  else
    FCount += 1;
  FBuf[FStart] := AItem;
  AddValue(AItem);
end;

procedure TChartSourceBuffer.AddLast(const AItem: TChartDataItem);
begin
  if Capacity > 0 then
    if FCount = Capacity then begin
      RemoveValue(FBuf[FStart]);
      FBuf[FStart] := AItem;
      FStart := (FStart + 1) mod Capacity;
    end
    else begin
      FCount += 1;
      FBuf[EndIndex] := AItem;
    end;
  AddValue(AItem);
end;

procedure TChartSourceBuffer.AddValue(const AItem: TChartDataItem);
var
  i, oldLen: Integer;
begin
  with FSum do begin
    Y += AItem.Y;
    oldLen := Length(YList);
    SetLength(YList, Max(Length(AItem.YList), oldLen));
    for i := oldLen to High(YList) do
      YList[i] := 0;
    for i := 0 to Min(High(YList), High(AItem.YList)) do
      YList[i] += AItem.YList[i];
  end;
end;

procedure TChartSourceBuffer.Clear;
begin
  FCount := 0;
  FStart := 0;
  FSum.Y := 0;
  FSum.YList := nil;
end;

function TChartSourceBuffer.EndIndex: Cardinal;
begin
  Result := (FStart + Cardinal(FCount - 1)) mod Capacity;
end;

function TChartSourceBuffer.GetCapacity: Cardinal;
begin
  Result := Length(FBuf);
end;

function TChartSourceBuffer.GetPtr(AOffset: Cardinal): PChartDataItem;
begin
  if AOffset >= FCount then
    raise EBufferError.Create('AOffset');
  Result := @FBuf[(FStart + AOffset + Capacity) mod Capacity];
end;

procedure TChartSourceBuffer.GetSum(var AItem: TChartDataItem);
begin
  AItem.Y := FSum.Y;
  AItem.YList := Copy(FSum.YList);
end;

procedure TChartSourceBuffer.RemoveFirst;
begin
  if FCount = 0 then
    raise EBufferError.Create('Empty');
  RemoveValue(FBuf[FStart]);
  FCount -= 1;
  FStart := (FStart + 1) mod Capacity;
end;

procedure TChartSourceBuffer.RemoveLast;
begin
  if FCount = 0 then
    raise EBufferError.Create('Empty');
  RemoveValue(FBuf[EndIndex]);
  FCount -= 1;
end;

procedure TChartSourceBuffer.RemoveValue(const AItem: TChartDataItem);
var
  i: Integer;
begin
  with AItem do begin
    FSum.Y -= Y;
    for i := 0 to Min(High(FSum.YList), High(YList)) do
      FSum.YList[i] -= YList[i];
  end;
end;

procedure TChartSourceBuffer.SetCapacity(AValue: Cardinal);
begin
  if AValue = Capacity then exit;
  SetLength(FBuf, AValue);
  Clear;
end;

{ TCustomChartSourceEnumerator }

constructor TCustomChartSourceEnumerator.Create(ASource: TCustomChartSource);
begin
  FSource := ASource;
  FIndex := -1;
end;

function TCustomChartSourceEnumerator.GetCurrent: PChartDataItem;
begin
  Result := FSource[FIndex];
end;

function TCustomChartSourceEnumerator.MoveNext: Boolean;
begin
  FIndex += 1;
  Result := FIndex < FSource.Count;
end;

procedure TCustomChartSourceEnumerator.Reset;
begin
  FIndex := 0;
end;


{ TChartErrorBarData }

constructor TChartErrorBarData.Create;
begin
  inherited;
  FIndex[0] := -1;
  FIndex[1] := -1;
  FValue[0] := 0;
  FValue[1] := -1;
  FKind := ebkNone;
end;

procedure TChartErrorBarData.Assign(ASource: TPersistent);
begin
  if ASource is TChartErrorBarData then begin
    FValue := TChartErrorBarData(ASource).FValue;
    FIndex := TChartErrorBarData(ASource).FIndex;
    FKind := TChartErrorBarData(ASource).Kind;
  end else
    inherited;
end;

procedure TChartErrorBarData.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TChartErrorBarData.GetIndex(AIndex: Integer): Integer;
begin
  Result := FIndex[AIndex];
end;

function TChartErrorBarData.GetValue(AIndex: Integer): Double;
begin
  Result := FValue[AIndex];
end;

function TChartErrorBarData.IsErrorbarValueStored(AIndex: Integer): Boolean;
begin
  if AIndex = 0 then
    Result := FValue[0] <> 0
  else
    Result := FValue[1] <> -1;
end;

procedure TChartErrorBarData.SetIndex(AIndex, AValue: Integer);
begin
  if FIndex[AIndex] = AValue then exit;
  FIndex[AIndex] := AValue;
  Changed;
end;

procedure TChartErrorBarData.SetKind(AValue: TChartErrorBarKind);
begin
  if FKind = AValue then exit;
  FKind := AValue;
  Changed;
end;

procedure TChartErrorBarData.SetValue(AIndex: Integer; AValue: Double);
begin
  if FValue[AIndex] = AValue then exit;
  FValue[AIndex] := AValue;
  Changed;
end;


{ TCustomChartSource }

procedure TCustomChartSource.AfterDraw;
begin
  // empty
end;

procedure TCustomChartSource.BeforeDraw;
begin
  // empty
end;

class procedure TCustomChartSource.CheckFormat(const AFormat: String);
begin
  Format(AFormat, [0.0, 0.0, '', 0.0, 0.0]);
end;

constructor TCustomChartSource.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  FXCount := 1;
  FYCount := 1;
  for i:=0 to 1 do begin
    FErrorBarData[i] := TChartErrorBarData.Create;
    FErrorBarData[i].OnChange := @ChangeErrorBars;
  end;
end;

destructor TCustomChartSource.Destroy;
begin
  FErrorBarData[0].Free;
  FErrorBarData[1].Free;
  inherited;
end;

procedure TCustomChartSource.ChangeErrorBars(Sender: TObject);
begin
  Unused(Sender);
  InvalidateCaches;
  Notify;
end;

procedure TCustomChartSource.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount > 0 then exit;
  // Values can be set directly between BeginUpdate and EndUpdate.
  InvalidateCaches;
  Notify;
end;

function TCustomChartSource.Extent: TDoubleRect;
var
  i: Integer;
  vhi, vlo: Double;
begin
  if FExtentIsValid then exit(FExtent);
  FExtent := EmptyExtent;

  if HasXErrorBars then
    for i := 0 to Count - 1 do begin
      GetXErrorBarLimits(i, vhi, vlo);
      UpdateMinMax(vhi, FExtent.a.X, FExtent.b.X);
      UpdateMinMax(vlo, FExtent.a.X, FExtent.b.X);
    end
  else
    for i:=0 to Count - 1 do
      UpdateMinMax(Item[i]^.X, FExtent.a.X, FExtent.b.X);

  if HasYErrorBars then
    for i := 0 to Count - 1 do begin
      GetYErrorBarLimits(i, vhi, vlo);
      UpdateMinMax(vhi, FExtent.a.Y, FExtent.b.Y);
      UpdateMinMax(vlo, FExtent.a.Y, FExtent.b.Y);
    end
  else
    for i:=0 to Count - 1 do
      UpdateMinMax(Item[i]^.Y, FExtent.a.Y, FExtent.b.Y);

  FExtentIsValid := true;
  Result := FExtent;
end;

{ Calculates the extent of multiple y values stacked onto each other. }
function TCustomChartSource.ExtentCumulative: TDoubleRect;
var
  h: Double;
  i, j: Integer;
  jyp: Integer = -1;
  jyn: Integer = -1;
begin
  Result := Extent;
  if YCount < 2 then exit;

  // Skip the y values used for error bars in calculating the cumulative sum.
  if YErrorBarData.Kind = ebkChartSource then begin
    jyp := YErrorBarData.IndexPlus - 1;  // -1 because YList index is offset by 1
    jyn := YErrorBarData.IndexMinus - 1;
  end;

  for i := 0 to Count - 1 do begin
    h := NumberOr(Item[i]^.Y);
    for j := 0 to YCount - 2 do
      if (j <> jyp) and (j <> jyn) then begin
        h += NumberOr(Item[i]^.YList[j]);
        // If some of the Y values are negative, h may be non-monotonic.
        UpdateMinMax(h, Result.a.Y, Result.b.Y);
      end;
  end;
end;

{ Calculates the extent including multiple x and y values (non-stacked) }
function TCustomChartSource.ExtentList: TDoubleRect;
var
  i, j: Integer;
  jxp, jxn: Integer;
  jyp, jyn: Integer;
begin
  Result := Extent;
  if (YCount < 2) and (XCount < 2) then exit;

  // Skip then x and y values used for error bars when calculating the list extent.
  if XErrorBarData.Kind = ebkChartSource then begin
    jxp := XErrorBarData.IndexPlus - 1;  // -1 because XList is offset by 1
    jxn := XErrorBarData.IndexMinus - 1;
  end else begin
    jxp := -1;
    jxn := -1;
  end;
  if YErrorBarData.Kind = ebkChartSource then begin
    jyp := YErrorbarData.IndexPlus - 1;  // -1 because YList is offset by 1
    jyn := YErrorBarData.IndexMinus - 1;
  end else begin
    jyp := -1;
    jyn := -1;
  end;

  for i := 0 to Count - 1 do
    with Item[i]^ do begin
      for j := 0 to High(XList) do
        if (j <> jxp) and (j <> jxn) then
          UpdateMinMax(XList[j], Result.a.X, Result.b.X);
      for j := 0 to High(YList) do
        if (j <> jyp) and (j <> jyn) then
          UpdateMinMax(YList[j], Result.a.Y, Result.b.Y);
    end;
end;

// ALB -> leftmost item where X >= AXMin, or Count if no such item
// ALB -> rightmost item where X <= AXMax, or -1 if no such item
// If the source is sorted, performs binary search. Otherwise, skips NaNs.
procedure TCustomChartSource.FindBounds(
  AXMin, AXMax: Double; out ALB, AUB: Integer);

  function FindLB(X: Double; L, R: Integer): Integer;
  begin
    while L <= R do begin
      Result := (R - L) div 2 + L;
      if Item[Result]^.X < X then
        L := Result + 1
      else
        R := Result - 1;
    end;
    Result := L;
  end;

  function FindUB(X: Double; L, R: Integer): Integer;
  begin
    while L <= R do begin
      Result := (R - L) div 2 + L;
      if Item[Result]^.X <= X then
        L := Result + 1
      else
        R := Result - 1;
    end;
    Result := R;
  end;

begin
  EnsureOrder(AXMin, AXMax);
  if IsSorted then begin
    ALB := FindLB(AXMin, 0, Count - 1);
    AUB := FindUB(AXMax, 0, Count - 1);
  end
  else begin
    ALB := 0;
    while ALB < Count do begin
      with Item[ALB]^ do
        if not IsNan(X) and (X >= AXMin) then break;
      ALB += 1;
    end;
    AUB := Count - 1;
    while AUB >= 0 do begin
      with Item[AUB]^ do
        if not IsNan(X) and (X <= AXMax) then break;
      AUB -= 1;
    end;
  end;
end;

function TCustomChartSource.FormatItem(
  const AFormat: String; AIndex, AYIndex: Integer): String;
begin
  with Item[AIndex]^ do
    Result := FormatItemXYText(AFormat, X, GetY(AYIndex), Text);
end;

function TCustomChartSource.FormatItemXYText(
  const AFormat: String; AX, AY: Double; AText: String): String;
const
  TO_PERCENT = 100;
var
  total, percent: Double;
begin
  total := ValuesTotal;
  if total = 0 then
    percent := 0
  else
    percent := TO_PERCENT / total;
  Result := Format(AFormat, [AY, AY * percent, AText, total, AX]);
end;

function TCustomChartSource.GetEnumerator: TCustomChartSourceEnumerator;
begin
  Result := TCustomChartSourceEnumerator.Create(Self);
end;

function TCustomChartSource.GetErrorbarData(AIndex: Integer): TChartErrorBarData;
begin
  Result := FErrorBarData[AIndex];
end;

{ Returns the error bar values in positive and negative direction for the
  x (which = 0) or y (which = 1) coordinates of the data point at the specified
  index. The result is false if there is no error bar. }
function TCustomChartSource.GetErrorBarValues(APointIndex: Integer;
  Which: Integer; out AUpperDelta, ALowerDelta: Double): Boolean;
var
  v: Double;
  pidx, nidx: Integer;
begin
  Result := false;
  AUpperDelta := 0;
  ALowerDelta := 0;

  if Which = 0 then
    v := Item[APointIndex]^.X
  else
    v := Item[APointIndex]^.Y;

  if IsNaN(v) then
    exit;

  if Assigned(FErrorBarData[Which]) then begin
    case FErrorBarData[Which].Kind of
      ebkNone:
        exit;
      ebkConst:
        begin
          AUpperDelta := FErrorBarData[Which].ValuePlus;
          if FErrorBarData[Which].ValueMinus = -1 then
            ALowerDelta := AUpperDelta
          else
            ALowerDelta := FErrorBarData[Which].ValueMinus;
        end;
      ebkPercent:
        begin
          AUpperDelta := v * FErrorBarData[Which].ValuePlus * PERCENT;
          if FErrorBarData[Which].ValueMinus = -1 then
            ALowerDelta := AUpperDelta
          else
            ALowerDelta := v * FErrorBarData[Which].ValueMinus * PERCENT;
        end;
      ebkChartSource:
        if Which = 0 then begin
          pidx := FErrorBarData[0].IndexPlus;
          nidx := FErrorBarData[0].IndexMinus;
          if not InRange(pidx, 0, XCount) then exit;
          if (nidx <> -1) and not InRange(nidx, 0, XCount-1) then exit;
          AUpperDelta := Item[APointIndex]^.GetX(pidx);
          if nidx = -1 then
            ALowerDelta := AUpperDelta
          else
            ALowerDelta := Item[APointIndex]^.GetX(nidx);
        end else begin
          pidx := FErrorBarData[1].IndexPlus;
          nidx := FErrorBarData[1].IndexMinus;
          if not InRange(pidx, 0, YCount-1) then exit;
          if (nidx <> -1) and not InRange(nidx, 0, YCount-1) then exit;
          AUpperDelta := Item[APointIndex]^.GetY(pidx);
          if nidx = -1 then
            ALowerDelta := AUpperDelta
          else
            ALowerDelta := Item[APointIndex]^.GetY(nidx);
        end;
    end;
    AUpperDelta := abs(AUpperDelta);
    ALowerDelta := abs(ALowerDelta);
    Result := (AUpperDelta <> 0) and (ALowerDelta <> 0);
  end;
end;

function TCustomChartSource.GetXErrorBarLimits(APointIndex: Integer;
  out AUpperLimit, ALowerLimit: Double): Boolean;
var
  v, dxp, dxn: Double;
begin
  Result := GetErrorBarValues(APointIndex, 0, dxp, dxn);
  v := Item[APointIndex]^.X;
  if Result and not IsNaN(v) then begin
    AUpperLimit := v + dxp;
    ALowerLimit := v - dxn;
  end else begin
    AUpperLimit := v;
    ALowerLimit := v;
  end;
end;

function TCustomChartSource.GetYErrorBarLimits(APointIndex: Integer;
  out AUpperLimit, ALowerLimit: Double): Boolean;
var
  v, dyp, dyn: Double;
begin
  Result := GetErrorBarValues(APointIndex, 1, dyp, dyn);
  v := Item[APointIndex]^.Y;
  if Result and not IsNaN(v) then begin
    AUpperLimit := v + dyp;
    ALowerLimit := v - dyn;
  end else begin
    AUpperLimit := v;
    ALowerLimit := v;
  end;
end;

function TCustomChartSource.GetXErrorBarValues(APointIndex: Integer;
  out AUpperDelta, ALowerDelta: Double): Boolean;
begin
  Result := GetErrorBarValues(APointIndex, 0, AUpperDelta, ALowerDelta);
end;

function TCustomChartSource.GetYErrorBarValues(APointIndex: Integer;
  out AUpperDelta, ALowerDelta: Double): Boolean;
begin
  Result := GetErrorBarValues(APointIndex, 1, AUpperDelta, ALowerDelta);
end;

function TCustomChartSource.GetHasErrorBars(Which: Integer): Boolean;
var
  errbar: TChartErrorbarData;
begin
  Result := false;
  errbar := FErrorBarData[Which];
  if Assigned(errbar) then
    case errbar.Kind of
      ebkNone:
        ;
      ebkConst, ebkPercent:
        Result := (errbar.ValuePlus > 0) and
                  ((errbar.ValueMinus = -1) or (errbar.ValueMinus > 0));
      ebkChartSource:
        Result := (errbar.IndexPlus > -1) and (errbar.IndexMinus >= -1);
    end;
end;

function TCustomChartSource.HasXErrorBars: Boolean;
begin
  Result := GetHasErrorBars(0);
end;

function TCustomChartSource.HasYErrorBars: Boolean;
begin
  Result := GetHasErrorBars(1);
end;

procedure TCustomChartSource.InvalidateCaches;
begin
  FExtentIsValid := false;
  FValuesTotalIsValid := false;
end;

function TCustomChartSource.IsErrorBarDataStored(AIndex: Integer): Boolean;
begin
  with FErrorBarData[AIndex] do
    Result := (FIndex[AIndex] <> -1) or (FValue[AIndex] <> -1) or (FKind <> ebkNone);
end;

function TCustomChartSource.IsXErrorIndex(AXIndex: Integer): Boolean;
begin
  Result :=
    (XErrorBarData.Kind = ebkChartSource) and
    ((XErrorBarData.IndexPlus = AXIndex) or (XErrorBarData.IndexMinus = AXIndex) and
    (AXIndex > -1)
  );
end;

function TCustomChartSource.IsYErrorIndex(AYIndex: Integer): Boolean;
begin
  Result :=
    (YErrorBarData.Kind = ebkChartSource) and
    ((YErrorBarData.IndexPlus = AYIndex) or (YErrorBarData.IndexMinus = AYIndex)) and
    (AYIndex > -1);
end;

function TCustomChartSource.IsSorted: Boolean;
begin
  Result := false;
end;

procedure TCustomChartSource.SetErrorbarData(AIndex: Integer;
  AValue: TChartErrorBarData);
begin
  FErrorbarData[AIndex] := AValue;
  Notify;
end;

procedure TCustomChartSource.SortValuesInRange(
  var AValues: TChartValueTextArray; AStart, AEnd: Integer);
var
  i, j, next: Integer;
  lst: TFPList;
  p: PChartValueText;
  tmp: TChartValueText;
begin
  lst := TFPList.Create;
  try
    lst.Count := AEnd - AStart + 1;
    for i := AStart to AEnd do
      lst[i - AStart] := @AValues[i];
    lst.Sort(@CompareChartValueTextPtr);
    for i := AStart to AEnd do begin
      if lst[i - AStart] = nil then continue;
      j := i;
      tmp := AValues[j];
      while true do begin
        p := PChartValueText(lst[j - AStart]);
        lst[j - AStart] := nil;
        {$PUSH}
        {$HINTS OFF} // Work around the fpc bug #19582.
        next := (PtrUInt(p) - PtrUInt(@AValues[0])) div SizeOf(p^);
        {$POP}
        if next = i then break;
        AValues[j] := p^;
        j := next;
      end;
      AValues[j] := tmp;
    end;
  finally
    lst.Free;
  end;
end;

procedure TCustomChartSource.ValuesInRange(
  AParams: TValuesInRangeParams; var AValues: TChartValueTextArray);

  procedure Put(
    out ADest: TChartValueText; AValue: Double; AIndex: Integer); inline;
  var
    nx, ny: Double;
  begin
    AParams.RoundToImage(AValue);
    ADest.FValue := AValue;
    with Item[AIndex]^ do begin
      if AParams.FUseY then begin
        nx := X;
        ny := AValue;
      end
      else begin
        nx := AValue;
        ny := Y;
      end;
      ADest.FText := FormatItemXYText(AParams.FFormat, nx, ny, Text);
    end;
  end;

var
  prevImagePos: Integer = MaxInt;

  function IsTooClose(AValue: Double): Boolean;
  var
    imagePos: Integer;
  begin
    with AParams do
      if aipUseMinLength in FIntervals.Options then begin
        imagePos := ToImage(AValue);
        Result := Abs(imagePos - prevImagePos) < FScale(FIntervals.MinLength);
      end;
    if not Result then
      prevImagePos := imagePos;
  end;

  function EnsureMinLength(AStart, AEnd: Integer): Integer;
  var
    i: Integer;
    v: Double;
  begin
    prevImagePos := MaxInt;
    Result := AStart;
    for i := AStart to AEnd do begin
      v := AValues[i].FValue;
      if InRange(v, AParams.FMin, AParams.FMax) and IsTooClose(v) then continue;
      AValues[Result] := AValues[i];
      Result += 1;
    end;
  end;

var
  i, cnt, start: Integer;
  v: Double;
  lo, hi: TChartValueText;
begin
  // Select all values in a given range, plus lower and upper bound values.
  // Proceed through the (possibly unsorted) data source in a single pass.
  start := Length(AValues);
  SetLength(AValues, start + Count + 2);
  cnt := start;
  lo.FValue := NegInfinity;
  hi.FValue := SafeInfinity;
  AValues[start].FValue := SafeNan;
  for i := 0 to Count - 1 do begin
    with Item[I]^ do
      v := IfThen(AParams.FUseY, Y, X);
    if IsNan(v) then continue;
    if v < AParams.FMin then begin
      if v > lo.FValue then
        Put(lo, v, i);
    end
    else if v > AParams.FMax then begin
      if v < hi.FValue then
        Put(hi, v, i);
    end
    else begin
      if (aipUseMinLength in AParams.FIntervals.Options) and IsTooClose(v) then
        continue;
      if not IsInfinite(lo.FValue) and (cnt = start) then
        cnt += 1;
      Put(AValues[cnt], v, i);
      cnt += 1;
    end;
  end;

  if not IsInfinite(lo.FValue) then begin
    if not IsNan(AValues[start].FValue) then begin
      // The lower bound value occured after the first in-range value,
      // so we did not reserve space for it. Hopefully rare case.
      for i := cnt downto start + 1 do
        AValues[i] := AValues[i - 1];
      cnt += 1;
    end;
    AValues[start] := lo;
    if cnt = start then
      cnt += 1;
  end;
  if not IsInfinite(hi.FValue) then begin
    AValues[cnt] := hi;
    cnt += 1;
  end;

  if not IsSorted and not IsValueTextsSorted(AValues, start, cnt - 1) then begin
    SortValuesInRange(AValues, start, cnt - 1);
    if aipUseMinLength in AParams.FIntervals.Options then
      cnt := EnsureMinLength(start, cnt - 1);
  end;
  SetLength(AValues, cnt);
end;

function TCustomChartSource.ValuesTotal: Double;
var
  i: Integer;
begin
  if FValuesTotalIsValid then exit(FValuesTotal);
  FValuesTotal := 0;
  for i := 0 to Count - 1 do
    with Item[i]^ do
      FValuesTotal += NumberOr(Y);
  FValuesTotalIsValid := true;
  Result := FValuesTotal;
end;

function TCustomChartSource.XOfMax(AIndex: Integer = 0): Double;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Item[i]^ do
      if Y = Extent.b.Y then exit(GetX(AIndex));
  Result := 0.0;
end;

function TCustomChartSource.XOfMin(AIndex: Integer = 0): Double;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Item[i]^ do
      if Y = Extent.a.Y then exit(GetX(AIndex));
  Result := 0.0;
end;

end.

