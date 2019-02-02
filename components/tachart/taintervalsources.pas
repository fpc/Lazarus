{

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
    procedure SetParams(AValue: TChartAxisIntervalParams);
  strict protected
    procedure CalculateIntervals(
      AParams: TValuesInRangeParams; out ABestStart, ABestStep: Double);
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
    procedure SetXCount(AValue: Cardinal); override;
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

  { TDateTimeStepFormat }

  TDateTimeStepFormat = class(TPersistent)
  private
    FSource: TBasicChartSource;
    FYearFmt: String;
    FMonthFmt: String;
    FWeekFmt: String;
    FDayFmt: String;
    FHourFmt: String;
    FMinuteFmt: String;
    FSecondFmt: String;
    FMillisecondFmt: String;
    function IsStoredYearFmt: Boolean;
    function IsStoredMonthFmt: Boolean;
    function IsStoredWeekFmt: Boolean;
    function IsStoredDayFmt: Boolean;
    function IsStoredHourFmt: Boolean;
    function IsStoredMinuteFmt: Boolean;
    function IsStoredSecondFmt: Boolean;
    function IsStoredMillisecondFmt: Boolean;
    procedure SetYearFmt(const AValue: String);
    procedure SetMonthFmt(const AValue: String);
    procedure SetWeekFmt(const AValue: String);
    procedure SetDayFmt(const AValue: String);
    procedure SetHourFmt(const AValue: String);
    procedure SetMinuteFmt(const AValue: String);
    procedure SetSecondFmt(const AValue: String);
    procedure SetMillisecondFmt(const AValue: String);
  public
    constructor Create(ASource: TBasicChartSource);
  published
    property YearFormat: String
      read FYearFmt write SetYearFmt stored IsStoredYearFmt;
    property MonthFormat: String
      read FMonthFmt write SetMonthFmt stored IsStoredMonthFmt;
    property WeekFormat: String
      read FWeekFmt write SetWeekFmt stored IsStoredWeekFmt;
    property DayFormat: String
      read FDayFmt write SetDayFmt stored IsStoredDayFmt;
    property HourFormat: String
      read FHourFmt write SetHourFmt stored IsStoredHourFmt;
    property MinuteFormat: String
      read FMinuteFmt write SetMinuteFmt stored IsStoredMinuteFmt;
    property SecondFormat: String
      read FSecondFmt write SetSecondFmt stored IsStoredSecondFmt;
    property MillisecondFormat: String
      read FMillisecondFmt write SetMillisecondFmt stored IsStoredMillisecondFmt;
  end;

  { TDateTimeIntervalChartSource }

  TDateTimeStepChangeEvent = procedure (Sender: TObject; ASteps: TDateTimeStep) of object;

  TDateTimeIntervalChartSource = class(TIntervalChartSource)
  strict private
    FDateTimeFormat: String;
    FDateTimeStepFormat: TDateTimeStepFormat;
    FSteps: TDateTimeSteps;
    FSuppressPrevUnit: Boolean;
    FOnDateTimeStepChange: TDateTimeStepChangeEvent;
    procedure SetDateTimeFormat(AValue: String);
    procedure SetSteps(AValue: TDateTimeSteps);
    procedure SetSuppressPrevUnit(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ValuesInRange(
      AParams: TValuesInRangeParams; var AValues: TChartValueTextArray); override;
  published
    property DateTimeFormat: String
      read FDateTimeFormat write SetDateTimeFormat;
    property DateTimeStepFormat: TDateTimeStepFormat
      read FDateTimeStepFormat write FDateTimeStepFormat;
    property Steps: TDateTimeSteps
      read FSteps write SetSteps default DATE_TIME_STEPS_ALL;
    property SuppressPrevUnit: Boolean
      read FSuppressPrevUnit write SetSuppressPrevUnit default true;
    property OnDateTimeStepChange: TDateTimeStepChangeEvent
      read FOnDateTimeStepChange write FOnDateTimeStepChange;
  end;

const
  DEFAULT_YEAR_FORMAT = 'yyyy';
//  DEFAULT_QUARTER_FORMAT = 'Q/yyyy';
  DEFAULT_MONTH_FORMAT = 'mm/yyyy';
  DEFAULT_WEEK_FORMAT = 'dd/mm';
  DEFAULT_DAY_FORMAT = 'dd/mm';
  DEFAULT_HOUR_FORMAT = 'dd hh:nn';
  DEFAULT_MINUTE_FORMAT = 'hh:nn';
  DEFAULT_SECOND_FORMAT = 'nn:ss';
  DEFAULT_MILLISECOND_FORMAT = 'szzz"ms"';

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
    function StartValue(AValue: TDateTime): TDateTime;
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

function TDateTimeIntervalsHelper.StartValue(AValue: TDateTime): TDateTime;
begin
  Result := Int(AValue / FStepLen - 1) * FStepLen;
  case FBestStep of
    dtsYear:
      // DateTime arithmetics fails on large year numbers.
      if FBestStepCoeff <= 10 then
        Result := StartOfTheYear(AValue);
    dtsMonth: Result := StartOfTheMonth(AValue);
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
    mi, prev, cnt: Int64;
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

procedure TIntervalChartSource.SetParams(AValue: TChartAxisIntervalParams);
begin
  if FParams = AValue then exit;
  FParams.Assign(AValue);
  InvalidateCaches;
  Notify;
end;

procedure TIntervalChartSource.SetXCount(AValue: Cardinal);
begin
  Unused(AValue);
  raise EXCountError.Create('Cannot set XCount');
end;

procedure TIntervalChartSource.SetYCount(AValue: Cardinal);
begin
  Unused(AValue);
  raise EYCountError.Create('Cannot set YCount');
end;

procedure TIntervalChartSource.ValuesInRange(
  AParams: TValuesInRangeParams; var AValues: TChartValueTextArray);
const
  // Arbitrary limit to prevent hangup/OOM in case of bug in CalculateIntervals.
  MAX_COUNT = 10000;
var
  start, step, m, eps: Double;
  i: Integer;
begin
  if AParams.FMin >= AParams.FMax then exit;
  AParams.FIntervals := Params;

  if aipGraphCoords in Params.Options then begin
    AParams.FMin := AParams.FAxisToGraph(AParams.FMin);
    AParams.FMax := AParams.FAxisToGraph(AParams.FMax);
  end;
  EnsureOrder(AParams.FMin, AParams.FMax);
  CalculateIntervals(AParams, start, step);
  if step <= 0 then exit;
  eps := (AParams.FMax - AParams.FMin) * RANGE_EPSILON;
  m := start;
  SetLength(AValues, Trunc(Min((AParams.FMax - m) / step + 2, MAX_COUNT)));
  for i := 0 to High(AValues) do begin
    if IsZero(m, eps) then
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
  for i := 0 to High(AValues) do begin
    AParams.RoundToImage(AValues[i].FValue);
    // Extra format arguments for compatibility with FormatItem.
    AValues[i].FText := Format(
      AParams.FFormat, [AValues[i].FValue, 0.0, '', 0.0, 0.0]);
  end;
end;

{ TDateTimeStepFormat }

constructor TDateTimeStepFormat.Create(ASource: TBasicChartSource);
begin
  inherited Create;
  FSource := ASource;
  FYearFmt := DEFAULT_YEAR_FORMAT;
  FMonthFmt := DEFAULT_MONTH_FORMAT;
  FWeekFmt := DEFAULT_WEEK_FORMAT;
  FDayFmt := DEFAULT_DAY_FORMAT;
  FHourFmt := DEFAULT_HOUR_FORMAT;
  FMinuteFmt := DEFAULT_MINUTE_FORMAT;
  FSecondFmt := DEFAULT_SECOND_FORMAT;
  FMillisecondFmt := DEFAULT_MILLISECOND_FORMAT;
end;

function TDateTimeStepFormat.IsStoredYearFmt: Boolean;
begin
  Result := FYearFmt <> DEFAULT_YEAR_FORMAT;
end;

function TDateTimeStepFormat.IsStoredMonthFmt: Boolean;
begin
  Result := FMonthFmt <> DEFAULT_MONTH_FORMAT;
end;

function TDateTimeStepFormat.IsStoredWeekFmt: Boolean;
begin
  Result := FWeekFmt <> DEFAULT_WEEK_FORMAT;
end;

function TDateTimeStepFormat.IsStoredDayFmt: Boolean;
begin
  Result := FDayFmt <> DEFAULT_DAY_FORMAT;
end;

function TDateTimeStepFormat.IsStoredHourFmt: Boolean;
begin
  Result := FHourFmt <> DEFAULT_HOUR_FORMAT;
end;

function TDateTimeStepFormat.IsStoredMinuteFmt: Boolean;
begin
  Result := FMinuteFmt <> DEFAULT_MINUTE_FORMAT;
end;

function TDateTimeStepFormat.IsStoredSecondFmt: Boolean;
begin
  Result := FSecondFmt <> DEFAULT_SECOND_FORMAT;
end;

function TDateTimeStepFormat.IsStoredMillisecondFmt: Boolean;
begin
  Result := FMillisecondFmt <> DEFAULT_MILLISECOND_FORMAT;
end;

procedure TDateTimeStepFormat.SetYearFmt(const AValue: String);
begin
  if (AValue <> '') and (AValue <> FYearFmt) then begin
    FSource.BeginUpdate;
    FYearFmt := AValue;
    FSource.EndUpdate;
  end;
end;

procedure TDateTimeStepFormat.SetMonthFmt(const AValue: String);
begin
  if (AValue <> '') and (AValue <> FMonthFmt) then begin
    FSource.BeginUpdate;
    FMonthFmt := AValue;
    FSource.EndUpdate;
  end;
end;

procedure TDateTimeStepFormat.SetWeekFmt(const AValue: String);
begin
  if (AValue <> '') and (AValue <> FWeekFmt) then begin
    FSource.BeginUpdate;
    FWeekFmt := AValue;
    FSource.EndUpdate;
  end;
end;

procedure TDateTimeStepFormat.SetDayFmt(const AValue: String);
begin
  if (AValue <> '') and (AValue <> FDayFmt) then begin
    FSource.BeginUpdate;
    FDayFmt := AValue;
    FSource.EndUpdate;
  end;
end;

procedure TDateTimeStepFormat.SetHourFmt(const AValue: String);
begin
  if (AValue <> '') and (AValue <> FHourFmt) then begin
    FSource.BeginUpdate;
    FHourFmt := AValue;
    FSource.EndUpdate;
  end;
end;

procedure TDateTimeStepFormat.SetMinuteFmt(const AValue: String);
begin
  if (AValue <> '') and (AValue <> FMinuteFmt) then begin
    FSource.BeginUpdate;
    FMinuteFmt := AValue;
    FSource.EndUpdate;
  end;
end;

procedure TDateTimeStepFormat.SetSecondFmt(const AValue: String);
begin
  if (AValue <> '') and (AValue <> FSecondFmt) then begin
    FSource.BeginUpdate;
    FSecondFmt := AValue;
    FSource.EndUpdate;
  end;
end;

procedure TDateTimeStepFormat.SetMillisecondFmt(const AValue: String);
begin
  if (AValue <> '') and (AValue <> FMillisecondFmt) then begin
    FSource.BeginUpdate;
    FMillisecondFmt := AValue;
    FSource.EndUpdate;
  end;
end;


{ TDateTimeIntervalChartSource }

constructor TDateTimeIntervalChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSteps := DATE_TIME_STEPS_ALL;
  FSuppressPrevUnit := true;
  FDateTimeStepFormat := TDateTimeStepFormat.Create(self);
end;

destructor TDateTimeIntervalChartSource.Destroy;
begin
  FDateTimeStepFormat.Free;
  inherited;
end;

procedure TDateTimeIntervalChartSource.SetDateTimeFormat(AValue: String);
begin
  if FDateTimeFormat = AValue then exit;
  FDateTimeFormat := AValue;
  InvalidateCaches;
  Notify;
end;

procedure TDateTimeIntervalChartSource.SetSteps(AValue: TDateTimeSteps);
begin
  if FSteps = AValue then exit;
  FSteps := AValue;
  InvalidateCaches;
  Notify;
end;

procedure TDateTimeIntervalChartSource.SetSuppressPrevUnit(AValue: Boolean);
begin
  if FSuppressPrevUnit = AValue then exit;
  BeginUpdate;
  FSuppressPrevUnit := AValue;
  EndUpdate;
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
        Result := FormatDateTime(DateTimeStepFormat.YearFormat, AValue);
      dtsQuarter:
        Result := IntToRoman(Floor(AValue / helper.FStepLen) mod 4 + 1) + '/' +
          FormatDateTime(DateTimeStepFormat.YearFormat, AValue);
      dtsMonth:
        if FSuppressPrevUnit and (st.Year = prevSt.Year) then
          Result := FormatDateTime('mm', AValue) else
          Result := FormatDateTime(DateTimeStepFormat.MonthFormat, AValue);
      dtsWeek:
        Result := FormatDateTime(DateTimeStepFormat.WeekFormat, AValue);
      dtsDay:
        if FSuppressPrevUnit and (st.Month = prevSt.Month) then
          Result := FormatDateTime('dd', AValue) else
          Result := FormatDateTime(DateTimeStepFormat.DayFormat, AValue);
      dtsHour:
        if FSuppressPrevUnit and (st.Day = prevSt.Day) then
          Result := FormatDateTime('hh:00', AValue) else
          Result := FormatDateTime(DateTimeStepFormat.HourFormat, AValue);
      dtsMinute:
        if FSuppressPrevUnit and (st.Hour = prevSt.Hour) then
          Result := FormatDateTime('nn', AValue) else
          Result := FormatDateTime(DateTimeStepFormat.MinuteFormat, AValue);
      dtsSecond:
        if FSuppressPrevUnit and (st.Minute = prevSt.Minute) then
          Result := FormatDateTime('ss', AValue) else
          Result := FormatDateTime(DateTimeStepFormat.SecondFormat, AValue);
      dtsMillisecond:
        if FSuppressPrevUnit and (st.Second = prevSt.Second) then
          Result := IntToStr(st.Millisecond) + 'ms' else
          Result := FormatDateTime(DateTimeStepFormat.MillisecondFormat, AValue);
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

  start := helper.StartValue(helper.FOrigParams.FMin);
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

  if Assigned(FOnDateTimeStepChange) then
    FOnDateTimeStepChange(self, helper.FBestStep);
end;

end.

