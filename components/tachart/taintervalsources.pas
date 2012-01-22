{

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

 Authors: Alexander Klenin

}

unit TAIntervalSources;

{$H+}

interface

uses
  Classes, TAChartUtils, TACustomSource;

type

  { TIntervalChartSource }

  TIntervalChartSource = class(TCustomChartSource)
  strict private
    FParams: TChartAxisIntervalParams;
    procedure RoundToImage(
      const AParams: TValuesInRangeParams; var AValues: TChartValueTextArray);
    procedure SetParams(AValue: TChartAxisIntervalParams);
  strict protected
    procedure CalculateIntervals(
      AParams: TValuesInRangeParams; out ABestStart, ABestStep: Double);
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
    procedure SetYCount(AValue: Cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ValuesInRange(
      AParams: TValuesInRangeParams; var AValues: TChartValueTextArray); override;
  published
    property Params: TChartAxisIntervalParams read FParams write SetParams;
  end;

  TDateTimeStep = (
    dtsYear, dtsQuarter, dtsMonth, dtsWeek, dtsDay,
    dtsHour, dtsMinute, dtsSecond, dtsMillisecond
  );
  TDateTimeSteps = set of TDateTimeStep;

const
  DATE_TIME_STEPS_ALL = [Low(TDateTimeStep) .. High(TDateTimeStep)];

type

  { TDateTimeIntervalChartSource }

  TDateTimeIntervalChartSource = class(TIntervalChartSource)
  strict private
    FDateTimeFormat: String;
    FSteps: TDateTimeSteps;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ValuesInRange(
      AParams: TValuesInRangeParams; var AValues: TChartValueTextArray); override;
  published
    property DateTimeFormat: String read FDateTimeFormat write FDateTimeFormat;
    property Steps: TDateTimeSteps
      read FSteps write FSteps default DATE_TIME_STEPS_ALL;
  end;


procedure Register;

implementation

uses
  DateUtils, Math, StrUtils, SysUtils, TAMath;

const
  YEAR = 365.25;
  DATE_STEP_INTERVALS: array [TDateTimeStep] of Double = (
    YEAR, YEAR / 4, YEAR / 12, 7, 1,
    OneHour, OneMinute, OneSecond, OneMillisecond
  );

type
  TSourceIntervalParams = class(TChartAxisIntervalParams)
  strict protected
    procedure Changed; override;
  end;

  TDateTimeIntervalsHelper = object
    FBestStep: TDateTimeStep;
    FBestStepCoeff: Double;
    FOrigParams: TValuesInRangeParams;
    FStep: TDateTimeStep;
    FStepLen: Double;

    function AxisToGraph(AX: Double): Double;
    procedure CheckStep(AStepCoeff: Double);
    function GraphToAxis(AX: Double): Double;
    function NextValue(AValue: TDateTime): Double;
  end;

procedure Register;
begin
  RegisterComponents(
    CHART_COMPONENT_IDE_PAGE, [
      TIntervalChartSource, TDateTimeIntervalChartSource
    ]);
end;

function SafeRound(AValue: Double): Double; inline;
begin
  Result := Int(AValue * 1e9) / 1e9;
end;

{ TDateTimeIntervalsHelper }

function TDateTimeIntervalsHelper.AxisToGraph(AX: Double): Double;
begin
  Result := FOrigParams.FAxisToGraph(AX) * DATE_STEP_INTERVALS[FStep];
end;

procedure TDateTimeIntervalsHelper.CheckStep(AStepCoeff: Double);
begin
  // Strict inequaltity is importatnt to avoid steps like "ten quarters".
  if (1.0 <= AStepCoeff) and (AStepCoeff < FBestStepCoeff) then begin
    FBestStepCoeff := AStepCoeff;
    FBestStep := FStep;
    FStepLen := DATE_STEP_INTERVALS[FBestStep] * FBestStepCoeff;
  end;
end;

function TDateTimeIntervalsHelper.GraphToAxis(AX: Double): Double;
begin
  Result := FOrigParams.FGraphToAxis(AX / DATE_STEP_INTERVALS[FStep]);
end;

function TDateTimeIntervalsHelper.NextValue(AValue: TDateTime): Double;
begin
  case FBestStep of
    dtsYear:
      if FBestStepCoeff > 10 then
        // DateTime arithmetics fails on large year numbers.
        Result := AValue + FStepLen
      else
        Result := IncYear(AValue, Round(FBestStepCoeff));
    dtsMonth: Result := IncMonth(AValue, Round(FBestStepCoeff));
    otherwise Result := AValue + FStepLen;
  end;
end;

{ TSourceIntervalParams }

procedure TSourceIntervalParams.Changed;
begin
  with GetOwner as TCustomChartSource do begin
    BeginUpdate;
    EndUpdate;
  end;
end;

{ TIntervalChartSource }

procedure TIntervalChartSource.CalculateIntervals(
  AParams: TValuesInRangeParams; out ABestStart, ABestStep: Double);

  procedure CalcMinMaxCount(out AMinCount, AMaxCount: Integer);
  var
    imageWidth, len: Integer;
  begin
    // If the axis transformation is non-linear, steps may not be equidistant.
    // However, both minimax and maximin will be achieved on equal steps.
    with AParams do
      imageWidth := Abs(ToImage(FMax) - ToImage(FMin));
    if aipUseMinLength in Params.Options then
      len := AParams.FScale(Max(Params.MinLength, 2))
    else
      len := 2;
    AMaxCount := Max(imageWidth div len, 2);
    if aipUseMaxLength in Params.Options then begin
      len := AParams.FScale(Max(Params.MaxLength, 2));
      AMinCount := Max((imageWidth + 1) div len, 2);
    end
    else
      AMinCount := 2;
  end;

  procedure TryStep(AStep: Double; var ABestCount: Integer);
  var
    m, start: Double;
    mi, prev, cnt: Integer;
  begin
    if AStep <= 0 then exit;
    start := Int(AParams.FMin / AStep) * AStep;
    m := start;
    prev := AParams.ToImage(m);
    cnt := 0;
    while m <= AParams.FMax do begin
      mi := AParams.ToImage(m + AStep);
      if not AParams.IsAcceptableStep(Abs(prev - mi)) then exit;
      m += AStep;
      prev := mi;
      cnt += 1;
    end;
    if
      not (aipUseCount in Params.Options) or (ABestCount <= 0) or
      (Abs(cnt - Params.Count) < Abs(ABestCount - Params.Count))
    then begin
      ABestStart := start - AStep;
      ABestStep := AStep;
      ABestCount := cnt;
    end;
  end;

var
  minCount, maxCount, bestCount: Integer;
  s, sv: Double;
begin
  CalcMinMaxCount(minCount, maxCount);
  bestCount := 0;
  if aipUseNiceSteps in Params.Options then begin
    s := AParams.CountToStep(minCount)  * 10;
    while s >= Max(AParams.CountToStep(maxCount), AParams.FMinStep) do begin
      for sv in Params.StepValues do
        TryStep(s * sv, bestCount);
      // We are not required to pick the best count, so any one will do.
      if not (aipUseCount in Params.Options) and (bestCount > 0) then break;
      s *= 0.1;
    end;
  end;
  if bestCount > 0 then exit;
  // Either nice steps were not required, or we failed to find one.
  if aipUseCount in Params.Options then
    bestCount := EnsureRange(Params.Count, minCount, maxCount)
  else
    bestCount := minCount;
  ABestStep := (AParams.FMax - AParams.FMin) / bestCount;
  ABestStart := AParams.FMin - ABestStep;
end;

constructor TIntervalChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TChartAxisIntervalParams.Create(Self);
end;

destructor TIntervalChartSource.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

function TIntervalChartSource.GetCount: Integer;
begin
  Result := 0;
end;

function TIntervalChartSource.GetItem(AIndex: Integer): PChartDataItem;
begin
  Unused(AIndex);
  Result := nil;
end;

procedure TIntervalChartSource.RoundToImage(
  const AParams: TValuesInRangeParams; var AValues: TChartValueTextArray);

  function A2I(AX: Double): Integer; inline;
  begin
    Result := AParams.FGraphToImage(AParams.FAxisToGraph(AX));
  end;

var
  i: Integer;
  v, p, rv: Double;
  x: Int64;
begin
  if AParams.FIntervals.Tolerance = 0 then exit;
  for i := 0 to High(AValues) do begin
    v := AValues[i].FValue;
    if (v = 0) or IsInfinite(v) or IsNan(v) then continue;
    x := A2I(v);
    p := Power(10, Floor(Log10(Abs(v)) - Log10(High(Int64)) + 1));
    while v <> 0 do begin
      rv := Round(v / p) * p;
      if Abs(A2I(rv) - x) >= AParams.FIntervals.Tolerance then break;
      v := rv;
      p *= 10;
    end;
    AValues[i].FValue := v;
  end;
end;

procedure TIntervalChartSource.SetParams(AValue: TChartAxisIntervalParams);
begin
  if FParams = AValue then exit;
  FParams.Assign(AValue);
end;

procedure TIntervalChartSource.SetYCount(AValue: Cardinal);
begin
  Unused(AValue);
  raise EYCountError.Create('Can not set YCount');
end;

procedure TIntervalChartSource.ValuesInRange(
  AParams: TValuesInRangeParams; var AValues: TChartValueTextArray);
const
  // Arbitrary limit to prevent hangup/OOM in case of bug in CalculateIntervals.
  MAX_COUNT = 10000;
var
  start, step, m: Double;
  i: Integer;
begin
  if AParams.FMin >= AParams.FMax then exit;
  AParams.FIntervals := Params;

  if aipGraphCoords in Params.Options then begin
    AParams.FMin := AParams.FAxisToGraph(AParams.FMin);
    AParams.FMax := AParams.FAxisToGraph(AParams.FMax);
  end;
  CalculateIntervals(AParams, start, step);
  if step <= 0 then exit;
  m := start;
  SetLength(AValues, Trunc(Min((AParams.FMax - m) / step + 2, MAX_COUNT)));
  for i := 0 to High(AValues) do begin
    if IsZero(m) then
      m := 0;
    AValues[i].FValue := m;
    if m > AParams.FMax then begin
      SetLength(AValues, i + 1);
      break;
    end;
    m += step;
  end;
  if aipGraphCoords in Params.Options then
    for i := 0 to High(AValues) do
      AValues[i].FValue := AParams.FGraphToAxis(AValues[i].FValue);
  RoundToImage(AParams, AValues);
  for i := 0 to High(AValues) do
    // Extra format arguments for compatibility with FormatItem.
    AValues[i].FText := Format(
      AParams.FFormat, [AValues[i].FValue, 0.0, '', 0.0, 0.0]);
end;

{ TDateTimeIntervalChartSource }

constructor TDateTimeIntervalChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSteps := DATE_TIME_STEPS_ALL;
end;

procedure TDateTimeIntervalChartSource.ValuesInRange(
  AParams: TValuesInRangeParams; var AValues: TChartValueTextArray);
var
  helper: TDateTimeIntervalsHelper;
  prevSt: TSystemTime;

  function FormatLabel(AValue: TDateTime): String;
  var
    st: TSystemTime;
  begin
    if DateTimeFormat <> '' then
      exit(FormatDateTime(DateTimeFormat, AValue));
    DateTimeToSystemTime(AValue, st);
    case helper.FBestStep of
      dtsYear:
        Result := FormatDateTime('yyyy', AValue);
      dtsQuarter:
        Result :=
          IntToRoman(Floor(AValue / helper.FStepLen) mod 4 + 1) +
          FormatDateTime('/yyyy', AValue);
      dtsMonth:
        Result := FormatDateTime(
          IfThen(st.Year = prevSt.Year, 'mm', 'mm/yyyy'), AValue);
      dtsWeek:
        Result := FormatDateTime('dd/mm', AValue);
      dtsDay:
        Result := FormatDateTime(
          IfThen(st.Month = prevSt.Month, 'dd', 'dd/mm'), AValue);
      dtsHour:
        Result := FormatDateTime(
          IfThen(st.Day = prevSt.Day, 'hh:00', 'dd hh:00'), AValue);
      dtsMinute:
        Result := FormatDateTime(
          IfThen(st.Hour = prevSt.Hour, 'nn', 'hh:nn'), AValue);
      dtsSecond:
        Result := FormatDateTime(
          IfThen(st.Minute = prevSt.Minute, 'ss', 'nn:ss'), AValue);
      dtsMillisecond:
        Result :=
          IfThen(st.Second = prevSt.Second, '', IntToStr(st.Second) + '.') +
          IntToStr(st.Millisecond) + 'ms';
    end;
    if InRange(AValue, helper.FOrigParams.FMin, helper.FOrigParams.FMax) then
      prevSt := st;
  end;

  procedure AddValue(AIndex: Integer; AValue: Double);
  begin
    with AValues[AIndex] do begin
      FValue := AValue;
      FText := Format(
        AParams.FFormat, [AValue, 0.0, FormatLabel(AValue), 0.0, 0.0]);
    end;
  end;

const
  MAX_COUNT = 1000; // Arbitraty limit to prevent OOM in case of a bug.
var
  i, cnt: Integer;
  x, start, stepLen: Double;
begin
  if
    (AParams.FMin >= AParams.FMax) or (aipGraphCoords in Params.options)
  then
    exit;
  AParams.FIntervals := Params;

  helper.FOrigParams := AParams;
  AParams.FAxisToGraph := @helper.AxisToGraph;
  AParams.FGraphToAxis := @helper.GraphToAxis;
  AParams.FMinStep := 1.0;
  helper.FBestStepCoeff := SafeInfinity;
  for helper.FStep in Steps do begin
    AParams.FMin := helper.FOrigParams.FMin / DATE_STEP_INTERVALS[helper.FStep];
    AParams.FMax := helper.FOrigParams.FMax / DATE_STEP_INTERVALS[helper.FStep];
    CalculateIntervals(AParams, start, stepLen);
    helper.CheckStep(stepLen);
  end;

  if IsInfinite(helper.FBestStepCoeff) then exit;

  with helper do
    start := Int(FOrigParams.FMin / FStepLen - 1) * FStepLen;
  cnt := 1;
  x := start;
  while (x <= helper.FOrigParams.FMax) and (cnt < MAX_COUNT) do begin
    cnt += 1;
    x := helper.NextValue(x);
  end;
  i := Length(AValues);
  SetLength(AValues, i + cnt);

  FillChar(prevSt, SizeOf(prevSt), $FF);
  x := start;
  while (x <= helper.FOrigParams.FMax) and (i < cnt - 1) do begin
    AddValue(i, x);
    i += 1;
    x := helper.NextValue(x);
  end;
  AddValue(i, x);
end;

end.

