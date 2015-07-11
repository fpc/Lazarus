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
    YList: TDoubleDynArray;
    function GetY(AIndex: Integer): Double;
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

  TCustomChartSource = class(TComponent)
  strict private
    FBroadcaster: TBroadcaster;
    FUpdateCount: Integer;

    procedure SortValuesInRange(
      var AValues: TChartValueTextArray; AStart, AEnd: Integer);
  strict protected
    FExtent: TDoubleRect;
    FExtentIsValid: Boolean;
    FValuesTotal: Double;
    FValuesTotalIsValid: Boolean;
    FYCount: Cardinal;

    function GetCount: Integer; virtual; abstract;
    function GetItem(AIndex: Integer): PChartDataItem; virtual; abstract;
    procedure InvalidateCaches;
    procedure Notify;
    procedure SetYCount(AValue: Cardinal); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure AfterDraw; virtual;
    procedure BeforeDraw; virtual;
    procedure BeginUpdate;
    procedure EndUpdate; virtual;
    function IsUpdating: Boolean; inline;
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
    function IsSorted: Boolean; virtual;
    procedure ValuesInRange(
      AParams: TValuesInRangeParams; var AValues: TChartValueTextArray); virtual;
    function ValuesTotal: Double; virtual;
    function XOfMax: Double;
    function XOfMin: Double;

    property Broadcaster: TBroadcaster read FBroadcaster;
    property Count: Integer read GetCount;
    property Item[AIndex: Integer]: PChartDataItem read GetItem; default;
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

{ TCustomChartSource }

procedure TCustomChartSource.AfterDraw;
begin
  // empty
end;

procedure TCustomChartSource.BeforeDraw;
begin
  // empty
end;

procedure TCustomChartSource.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

class procedure TCustomChartSource.CheckFormat(const AFormat: String);
begin
  Format(AFormat, [0.0, 0.0, '', 0.0, 0.0]);
end;

constructor TCustomChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBroadcaster := TBroadcaster.Create;
  FYCount := 1;
end;

destructor TCustomChartSource.Destroy;
begin
  FreeAndNil(FBroadcaster);
  inherited;
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
begin
  if FExtentIsValid then exit(FExtent);
  FExtent := EmptyExtent;
  for i := 0 to Count - 1 do
    with Item[i]^ do begin
      UpdateMinMax(X, FExtent.a.X, FExtent.b.X);
      UpdateMinMax(Y, FExtent.a.Y, FExtent.b.Y);
    end;
  FExtentIsValid := true;
  Result := FExtent;
end;

function TCustomChartSource.ExtentCumulative: TDoubleRect;
var
  h: Double;
  i, j: Integer;
begin
  Result := Extent;
  if YCount < 2 then exit;
  for i := 0 to Count - 1 do begin
    h := NumberOr(Item[i]^.Y);
    for j := 0 to YCount - 2 do begin
      h += NumberOr(Item[i]^.YList[j]);
      // If some of Y values are negative, h may be non-monotonic.
      UpdateMinMax(h, Result.a.Y, Result.b.Y);
    end;
  end;
end;

function TCustomChartSource.ExtentList: TDoubleRect;
var
  i, j: Integer;
begin
  Result := Extent;
  for i := 0 to Count - 1 do
    with Item[i]^ do
      for j := 0 to High(YList) do
        UpdateMinMax(YList[j], Result.a.Y, Result.b.Y);
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

procedure TCustomChartSource.InvalidateCaches;
begin
  FExtentIsValid := false;
  FValuesTotalIsValid := false;
end;

function TCustomChartSource.IsSorted: Boolean;
begin
  Result := false;
end;

function TCustomChartSource.IsUpdating: Boolean; inline;
begin
  Result := FUpdateCount > 0;
end;

procedure TCustomChartSource.Notify;
begin
  if not IsUpdating then
    FBroadcaster.Broadcast(Self);
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
        {$HINTS OFF} // Work around the fpc bug #19582.
        next := (PtrUInt(p) - PtrUInt(@AValues[0])) div SizeOf(p^);
        {$HINTS ON}
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

function TCustomChartSource.XOfMax: Double;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Item[i]^ do
      if Y = Extent.b.Y then exit(X);
  Result := 0.0;
end;

function TCustomChartSource.XOfMin: Double;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Item[i]^ do
      if Y = Extent.a.Y then exit(X);
  Result := 0.0;
end;

end.

