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
    procedure CalculateIntervals(
      AParams: TValuesInRangeParams; out ABestStart, ABestStep: Double);
    procedure SetParams(AValue: TChartAxisIntervalParams);
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
    dtsCentury, dtsDecade, dtsYear, dtsQuarter, dtsMonth, dtsWeek, dtsDay,
    dtsHour, dtsTenMinutes, dtsMinute, dtsTenSeconds, dtsSecond, dtsMillisecond
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
  DateUtils, Math, StrUtils, SysUtils;

type
  TSourceIntervalParams = class(TChartAxisIntervalParams)
  strict protected
    procedure Changed; override;
  end;

procedure Register;
begin
  RegisterComponents(
    CHART_COMPONENT_IDE_PAGE, [
      TIntervalChartSource, TDateTimeIntervalChartSource
    ]);
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

  function A2I(AX: Double): Integer; inline;
  begin
    if not (aipGraphCoords in Params.Options) then
      AX := AParams.FAxisToGraph(AX);
    Result := AParams.FGraphToImage(AX);
  end;

  procedure CalcMinMaxCount(out AMinCount, AMaxCount: Integer);
  var
    imageWidth, d: Integer;
  begin
    // If the axis transformation is non-linear, steps may not be equidistant.
    // However, both minimax and maximin will be achieved on equal steps.
    imageWidth := Abs(A2I(AParams.FMax) - A2I(AParams.FMin));
    d := IfThen(aipUseMinLength in Params.Options, Max(Params.MinLength, 2), 2);
    AMaxCount := Max(imageWidth div d, 2);
    if aipUseMaxLength in Params.Options then
      AMinCount := Max((imageWidth + 1) div Max(Params.MaxLength, 2), 2)
    else
      AMinCount := 2;
  end;

  function CountToStep(ACount: Integer): Double;
  begin
    Result := (AParams.FMax - AParams.FMin) / ACount;
    Result := Power(10, Floor(Log10(Result)));
  end;

  procedure TryStep(AStep: Double; var ABestCount: Integer);
  var
    m, start: Double;
    mi, prev, cnt: Integer;
  begin
    start := Floor(AParams.FMin / AStep) * AStep;
    m := start;
    prev := A2I(m);
    cnt := 0;
    while m <= AParams.FMax do begin
      mi := A2I(m + AStep);
      prev := Abs(prev - mi);
      if
        (aipUseMinLength in Params.Options) and (prev < Params.MinLength) or
        (aipUseMaxLength in Params.Options) and (prev > Params.MaxLength)
      then
        exit;
      m += AStep;
      prev := mi;
      cnt += 1;
    end;
    if
      not (aipUseCount in Params.Options) or
      (Abs(cnt - Params.Count) < Abs(ABestCount - Params.Count))
    then begin
      ABestStart := start - AStep;
      ABestStep := AStep;
      ABestCount := cnt;
    end;
  end;

var
  minCount, maxCount, cnt: Integer;
  s, sv: Double;
begin
  CalcMinMaxCount(minCount, maxCount);
  cnt := MaxInt;
  if aipUseNiceSteps in Params.Options then begin
    s := CountToStep(minCount)  * 10;
    while s >= CountToStep(maxCount) do begin
      for sv in Params.StepValues do
        TryStep(s * sv, cnt);
      // We are not required to pick best count, so any one will do.
      if not (aipUseCount in Params.Options) and (cnt < MaxInt) then break;
      s *= 0.1;
    end;
  end;
  if cnt < MaxInt then exit;
  // Either nice steps were not required, or we failed to find one.
  if aipUseCount in Params.Options then
    cnt := EnsureRange(Params.Count, minCount, maxCount)
  else
    cnt := minCount;
  ABestStep := (AParams.FMax - AParams.FMin) / cnt;
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
const
  YEAR = 365.25;
  STEP_INTERVALS: array [TDateTimeStep] of Double = (
    100 * YEAR, 10 * YEAR, YEAR, YEAR / 4, YEAR / 12, 7, 1,
    OneHour, 10 * OneMinute, OneMinute, 10 * OneSecond, OneSecond, OneMillisecond
  );
  MIN_STEPS = 4;
  MAX_STEPS = 20;
var
  s: TDateTimeStep;
  si, x, start: TDateTime;
  prevSt: TSystemTime;

  function FormatLabel: String;
  var
    st: TSystemTime;
  begin
    if DateTimeFormat <> '' then
      exit(FormatDateTime(DateTimeFormat, x));
    DateTimeToSystemTime(x, st);
    case s of
      dtsCentury, dtsDecade, dtsYear:
        Result := FormatDateTime('yyyy', x);
      dtsQuarter:
        Result := FormatDateTime('yyyy/', x) + IntToStr(Floor(x / si) mod 4 + 1);
      dtsMonth:
        Result := FormatDateTime(
          IfThen(st.Year = prevSt.Year, 'mm', 'mm/yyyy'), x);
      dtsWeek:
        Result := FormatDateTime('dd/mm', x);
      dtsDay:
        Result := FormatDateTime(
          IfThen(st.Month = prevSt.Month, 'dd', 'dd/mm'), x);
      dtsHour:
        Result := FormatDateTime(
          IfThen(st.Day = prevSt.Day, 'hh:00', 'dd hh:00'), x);
      dtsTenMinutes, dtsMinute:
        Result := FormatDateTime(
          IfThen(st.Hour = prevSt.Hour, 'nn', 'hh:nn'), x);
      dtsTenSeconds, dtsSecond:
        Result := FormatDateTime(
          IfThen(st.Minute = prevSt.Minute, 'ss', 'nn:ss'), x);
      dtsMillisecond:
        Result := IntToStr(st.Millisecond) + 'ms';
    end;
    prevSt := st;
  end;

var
  i, cnt: Integer;
  r: Double;
begin
  with AParams do begin
    r := FMax - FMin;
    if r / STEP_INTERVALS[dtsCentury] > MAX_STEPS then begin
      FMin /= STEP_INTERVALS[dtsYear];
      FMax /= STEP_INTERVALS[dtsYear];
      inherited ValuesInRange(AParams, AValues);
      exit;
    end;
  end;
  s := Low(s);
  while s < High(s) do begin
    si := STEP_INTERVALS[s];
    if (s in Steps) and (r / si > MIN_STEPS) then
      break;
    Inc(s);
  end;
  start := Int(AParams.FMin / si - 1) * si;
  x := start;
  cnt := 1;
  while x <= AParams.FMax do begin
    cnt += 1;
    x += si;
  end;
  i := Length(AValues);
  SetLength(AValues, i + cnt);

  FillChar(prevSt, SizeOf(prevSt), $FF);
  x := start;
  while x <= AParams.FMax do begin
    AValues[i].FValue := x;
    AValues[i].FText := Format(AParams.FFormat, [x, 0.0, FormatLabel, 0.0, 0.0]);
    i += 1;
    case s of
      dtsCentury: x := IncYear(x, 100);
      dtsDecade: x := IncYear(x, 10);
      dtsYear: x := IncYear(x);
      dtsMonth: x := IncMonth(x);
      otherwise x += si;
    end;
  end;
  AValues[i].FValue := x;
  AValues[i].FText := Format(AParams.FFormat, [x, 0.0, FormatLabel, 0.0, 0.0]);
end;

end.

