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

unit TASources;

{$H+}

interface

uses
  Classes, Types, TAChartUtils, TACustomSource;

type
  { TListChartSource }

  TListChartSource = class(TCustomChartSource)
  private
    FData: TFPList;
    FDataPoints: TStrings;
    FSorted: Boolean;

    procedure AddAt(
      APos: Integer; AX, AY: Double; const ALabel: String; AColor: TChartColor);
    procedure ClearCaches;
    function NewItem: PChartDataItem;
    procedure SetDataPoints(AValue: TStrings);
    procedure SetSorted(AValue: Boolean);
    procedure UpdateCachesAfterAdd(AX, AY: Double);
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
    procedure SetYCount(AValue: Cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function Add(
      AX, AY: Double; const ALabel: String = '';
      AColor: TChartColor = clTAColor): Integer;
    procedure Clear;
    procedure CopyForm(ASource: TCustomChartSource);
    procedure Delete(AIndex: Integer); inline;
    function IsSorted: Boolean; override;
    function SetXValue(AIndex: Integer; AValue: Double): Integer;
    procedure SetYList(AIndex: Integer; const AYList: array of Double);
    procedure SetYValue(AIndex: Integer; AValue: Double);
    procedure Sort;
  published
    property DataPoints: TStrings read FDataPoints write SetDataPoints;
    property Sorted: Boolean read FSorted write SetSorted default false;
    property YCount;
  end;

  { TMWCRandomGenerator }

  // Mutliply-with-carry random number generator.
  // Algorithm by George Marsaglia.
  // A generator is incapsulated in a class to allow using many simultaneous
  // random sequences, each determined by its own seed.
  TMWCRandomGenerator = class
  private
    FHistory: array [0..4] of LongWord;
    procedure SetSeed(const AValue: Integer);
  public
    function Get: LongWord;
    function GetInRange(AMin, AMax: Integer): Integer;
    property Seed: Integer write SetSeed;
  end;

  { TRandomChartSource }

  TRandomChartSource = class(TCustomChartSource)
  private
    FPointsNumber: Integer;
    FRandomX: Boolean;
    FRandSeed: Integer;
    FXMax: Double;
    FXMin: Double;
    FYMax: Double;
    FYMin: Double;
  private
    FCurIndex: Integer;
    FCurItem: TChartDataItem;
    FRNG: TMWCRandomGenerator;

    procedure SetPointsNumber(const AValue: Integer);
    procedure SetRandomX(const AValue: Boolean);
    procedure SetRandSeed(const AValue: Integer);
    procedure SetXMax(const AValue: Double);
    procedure SetXMin(const AValue: Double);
    procedure SetYMax(const AValue: Double);
    procedure SetYMin(const AValue: Double);
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
    procedure SetYCount(AValue: Cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function IsSorted: Boolean; override;
  published
    property PointsNumber: Integer
      read FPointsNumber write SetPointsNumber default 0;
    property RandomX: Boolean read FRandomX write SetRandomX default false;
    property RandSeed: Integer read FRandSeed write SetRandSeed;
    property XMax: Double read FXMax write SetXMax;
    property XMin: Double read FXMin write SetXMin;
    property YCount;
    property YMax: Double read FYMax write SetYMax;
    property YMin: Double read FYMin write SetYMin;
  end;

  { TIntervalChartSource }

  TIntervalChartSource = class(TCustomChartSource)
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
    procedure SetYCount(AValue: Cardinal); override;
  public
    procedure ValuesInRange(
      AMin, AMax: Double; const AFormat: String; AUseY: Boolean;
      var AValues: TDoubleDynArray; var ATexts: TStringDynArray); override;
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
  private
    FDateTimeFormat: String;
    FSteps: TDateTimeSteps;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ValuesInRange(
      AMin, AMax: Double; const AFormat: String; AUseY: Boolean;
      var AValues: TDoubleDynArray; var ATexts: TStringDynArray); override;
  published
    property DateTimeFormat: String read FDateTimeFormat write FDateTimeFormat;
    property Steps: TDateTimeSteps
      read FSteps write FSteps default DATE_TIME_STEPS_ALL;
  end;

  TUserDefinedChartSource = class;

  TGetChartDataItemEvent = procedure (
    ASource: TUserDefinedChartSource; AIndex: Integer;
    var AItem: TChartDataItem) of object;

  { TUserDefinedChartSource }

  TUserDefinedChartSource = class(TCustomChartSource)
  private
    FItem: TChartDataItem;
    FOnGetChartDataItem: TGetChartDataItemEvent;
    FPointsNumber: Integer;
    FSorted: Boolean;
    procedure SetOnGetChartDataItem(const AValue: TGetChartDataItemEvent);
    procedure SetPointsNumber(const AValue: Integer);
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
    procedure SetYCount(AValue: Cardinal); override;
  public
    procedure EndUpdate; override;
    function IsSorted: Boolean; override;
    procedure Reset; inline;
  published
    property OnGetChartDataItem: TGetChartDataItemEvent
      read FOnGetChartDataItem write SetOnGetChartDataItem;
    property PointsNumber: Integer
      read FPointsNumber write SetPointsNumber default 0;
    property Sorted: Boolean read FSorted write FSorted default false;
  end;

  TChartAccumulationMethod = (camNone, camSum, camAverage);

  { TCalculatedChartSource }

  TCalculatedChartSource = class(TCustomChartSource)
  private
    FAccumulationMethod: TChartAccumulationMethod;
    FAccumulationRange: Integer;
    FHistory: TChartSourceBuffer;
    FIndex: Integer;
    FItem: TChartDataItem;
    FListener: TListener;
    FOrigin: TCustomChartSource;
    FOriginYCount: Cardinal;
    FPercentage: Boolean;
    FReorderYList: String;
    FYOrder: array of Integer;

    procedure CalcAccumulation(AIndex: Integer);
    procedure CalcPercentage;
    procedure Changed(ASender: TObject); inline;
    procedure ExtractItem(out AItem: TChartDataItem; AIndex: Integer);
    procedure SetAccumulationMethod(AValue: TChartAccumulationMethod);
    procedure SetAccumulationRange(AValue: Integer);
    procedure SetOrigin(AValue: TCustomChartSource);
    procedure SetPercentage(AValue: Boolean);
    procedure SetReorderYList(const AValue: String);
    procedure UpdateYOrder;
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
    procedure SetYCount(AValue: Cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IsSorted: Boolean; override;
  published
    property AccumulationMethod: TChartAccumulationMethod
      read FAccumulationMethod write SetAccumulationMethod default camNone;
    property AccumulationRange: Integer
      read FAccumulationRange write SetAccumulationRange default 1;

    property Origin: TCustomChartSource read FOrigin write SetOrigin;
    property Percentage: Boolean
      read FPercentage write SetPercentage default false;
    property ReorderYList: String read FReorderYList write SetReorderYList;
  end;

procedure Register;

implementation

uses
  DateUtils, Math, StrUtils, SysUtils;

type

  { TListChartSourceStrings }

  TListChartSourceStrings = class(TStrings)
  private
    FSource: TListChartSource;
    procedure Parse(const AString: String; ADataItem: PChartDataItem);
  protected
    function Get(Index: Integer): String; override;
    function GetCount: Integer; override;
    procedure Put(Index: Integer; const S: String); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: String); override;
  end;

procedure Register;
begin
  RegisterComponents(
    CHART_COMPONENT_IDE_PAGE, [
      TListChartSource, TRandomChartSource, TDateTimeIntervalChartSource,
      TUserDefinedChartSource, TCalculatedChartSource
    ]);
end;

{ TListChartSourceStrings }

procedure TListChartSourceStrings.Clear;
begin
  FSource.Clear;
end;

procedure TListChartSourceStrings.Delete(Index: Integer);
begin
  FSource.Delete(Index);
end;

function TListChartSourceStrings.Get(Index: Integer): String;
var
  i: Integer;
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  with FSource[Index]^ do begin
    Result := Format('%g', [X], fs);
    if FSource.YCount > 0 then
      Result += Format('|%g', [Y], fs);
    for i := 0 to High(YList) do
      Result += Format('|%g', [YList[i]], fs);
    Result += Format('|%s|%s',
      [IfThen(Color = clTAColor, '?', '$' + IntToHex(Color, 6)), Text]);
  end;
end;

function TListChartSourceStrings.GetCount: Integer;
begin
  Result := FSource.Count;
end;

procedure TListChartSourceStrings.Insert(Index: Integer; const S: String);
var
  item: PChartDataItem;
begin
  item := FSource.NewItem;
  FSource.FData.Insert(Index, item);
  Parse(S, item);
  FSource.UpdateCachesAfterAdd(item^.X, item^.Y);
end;

procedure TListChartSourceStrings.Parse(
  const AString: String; ADataItem: PChartDataItem);
var
  p: Integer = 0;
  parts: TStringList;
  fs: TFormatSettings;

  function NextPart: String;
  begin
    if p < parts.Count then
      Result := parts[p]
    else
      Result := '';
    p += 1;
  end;

  function S2F(const AStr: String): Double;
  begin
    // Accept both locale-specific and default decimal separators.
    if
      not TryStrToFloat(AStr, Result, fs) and
      not TryStrToFloat(AStr, Result)
    then
      Result := 0.0;
  end;

var
  i: Integer;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  parts := TStringList.Create;
  try
    parts.Delimiter := '|';
    parts.StrictDelimiter := true;
    parts.DelimitedText := AString;
    if FSource.YCount + 3 < Cardinal(parts.Count) then
      FSource.YCount := parts.Count - 3;
    with ADataItem^ do begin
      X := S2F(NextPart);
      if FSource.YCount > 0 then begin
        Y := S2F(NextPart);
        for i := 0 to High(YList) do
          YList[i] := S2F(NextPart);
      end;
      Color := StrToIntDef(NextPart, clTAColor);
      Text := NextPart;
    end;
  finally
    parts.Free;
  end;
end;

procedure TListChartSourceStrings.Put(Index: Integer; const S: String);
begin
  Parse(S, FSource[Index]);
end;

{ TListChartSource }

function TListChartSource.Add(
  AX, AY: Double; const ALabel: String; AColor: TChartColor): Integer;
begin
  Result := FData.Count;
  if Sorted then
    // Keep data points ordered by X coordinate.
    // Note that this leads to O(N^2) time except
    // for the case of adding already ordered points.
    // So, is the user wants to add many (>10000) points to a graph,
    // he should pre-sort them to avoid performance penalty.
    while (Result > 0) and (Item[Result - 1]^.X > AX) do
      Dec(Result);
  AddAt(Result, AX, AY, ALabel, AColor);
end;

procedure TListChartSource.AddAt(
  APos: Integer; AX, AY: Double; const ALabel: String; AColor: TChartColor);
var
  pcd: PChartDataItem;
begin
  pcd := NewItem;
  pcd^.X := AX;
  pcd^.Y := AY;
  pcd^.Color := AColor;
  pcd^.Text := ALabel;
  FData.Insert(APos, pcd);
  UpdateCachesAfterAdd(AX, AY);
  Notify;
end;

procedure TListChartSource.Clear; inline;
var
  i: Integer;
begin
  for i := 0 to FData.Count - 1 do
    Dispose(Item[i]);
  FData.Clear;
  ClearCaches;
  Notify;
end;

procedure TListChartSource.ClearCaches;
begin
  FExtent := EmptyExtent;
  FExtentIsValid := true;
  FValuesTotal := 0;
  FValuesTotalIsValid := true;
end;

procedure TListChartSource.CopyForm(ASource: TCustomChartSource);
var
  i: Integer;
begin
  BeginUpdate;
  try
    Clear;
    YCount := ASource.YCount;
    for i := 0 to ASource.Count - 1 do
      with ASource[i]^ do begin
        AddAt(FData.Count, X, Y, Text, Color);
        SetYList(FData.Count - 1, YList);
      end;
    if Sorted and not ASource.IsSorted then Sort;
  finally
    EndUpdate;
  end;
end;

constructor TListChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := TFPList.Create;
  FDataPoints := TListChartSourceStrings.Create;
  TListChartSourceStrings(FDataPoints).FSource := Self;
  FYCount := 1;
  ClearCaches;
end;

procedure TListChartSource.Delete(AIndex: Integer);
begin
  with Item[AIndex]^ do begin
    FExtentIsValid :=
      (FExtent.a.X < X) and (X < FExtent.b.X) and
      (FExtent.a.Y < Y) and (Y < FExtent.b.Y);
    if FValuesTotalIsValid then
      FValuesTotal -= Y;
  end;
  Dispose(Item[AIndex]);
  FData.Delete(AIndex);
  Notify;
end;

destructor TListChartSource.Destroy;
begin
  Clear;
  FreeAndNil(FDataPoints);
  FreeAndNil(FData);
  inherited;
end;

function TListChartSource.GetCount: Integer;
begin
  Result := FData.Count;
end;

function TListChartSource.GetItem(AIndex: Integer): PChartDataItem;
begin
  Result := PChartDataItem(FData.Items[AIndex]);
end;

function TListChartSource.IsSorted: Boolean;
begin
  Result := Sorted;
end;

function TListChartSource.NewItem: PChartDataItem;
begin
  New(Result);
  SetLength(Result^.YList, Max(YCount - 1, 0));
end;

procedure TListChartSource.SetDataPoints(AValue: TStrings);
begin
  if FDataPoints = AValue then exit;
  BeginUpdate;
  try
    FDataPoints.Assign(AValue);
    if Sorted then Sort;
  finally
    EndUpdate;
  end;
end;

procedure TListChartSource.SetSorted(AValue: Boolean);
begin
  if FSorted = AValue then exit;
  FSorted := AValue;
  if Sorted then begin
    Sort;
    Notify;
  end;
end;

function TListChartSource.SetXValue(AIndex: Integer; AValue: Double): Integer;
var
  i: Integer;
  oldX: Double;
begin
  oldX := Item[AIndex]^.X;
  Item[AIndex]^.X := AValue;

  if FExtentIsValid then begin
    if AValue <= FExtent.a.X then FExtent.a.X := AValue
    else if AValue >= FExtent.b.X then FExtent.b.X := AValue;
    if oldX = FExtent.b.X then begin
      FExtent.b.X := NegInfinity;
      for i := 0 to Count - 1 do
        FExtent.b.X := Max(FExtent.b.X, Item[i]^.X);
    end;
    if oldX = FExtent.a.X then begin
      FExtent.a.X := SafeInfinity;
      for i := 0 to Count - 1 do
        FExtent.a.X := Min(FExtent.a.X, Item[i]^.X);
    end;
  end;

  Result := AIndex;
  if Sorted then begin
    if AValue > oldX then
      while (Result < Count - 1) and (Item[Result + 1]^.X < AValue) do
        Inc(Result)
    else
      while (Result > 0) and (Item[Result - 1]^.X > AValue) do
        Dec(Result);
    if Result <> AIndex then
      FData.Move(AIndex, Result);
  end;
  Notify;
end;

procedure TListChartSource.SetYCount(AValue: Cardinal);
var
  i: Integer;
begin
  if AValue = FYCount then exit;
  FYCount := AValue;
  for i := 0 to Count - 1 do
    SetLength(Item[i]^.YList, Max(FYCount - 1, 0));
end;

procedure TListChartSource.SetYList(
  AIndex: Integer; const AYList: array of Double);
var
  i: Integer;
begin
  with Item[AIndex]^ do
    for i := 0 to Min(High(AYList), High(YList)) do
      YList[i] := AYList[i];
end;

procedure TListChartSource.SetYValue(AIndex: Integer; AValue: Double);
var
  i: Integer;
  oldY: Double;
begin
  oldY := Item[AIndex]^.Y;
  Item[AIndex]^.Y := AValue;
  if FValuesTotalIsValid then
    FValuesTotal += AValue - oldY;

  if FExtentIsValid then begin
    if AValue <= FExtent.a.Y then FExtent.a.Y := AValue
    else if AValue >= FExtent.b.Y then FExtent.b.Y := AValue;
    if oldY = FExtent.b.Y then begin
      FExtent.b.Y := NegInfinity;
      for i := 0 to Count - 1 do
        FExtent.b.Y := Max(FExtent.b.Y, Item[i]^.Y);
    end;
    if oldY = FExtent.a.Y then begin
      FExtent.a.Y := SafeInfinity;
      for i := 0 to Count - 1 do
        FExtent.a.Y := Min(FExtent.a.Y, Item[i]^.Y);
    end;
  end;
  Notify;
end;

function CompareDataItemX(AItem1, AItem2: Pointer): Integer;
begin
  Result := Sign(PChartDataItem(AItem1)^.X - PChartDataItem(AItem2)^.X);
end;

procedure TListChartSource.Sort;
begin
  FData.Sort(@CompareDataItemX);
end;

procedure TListChartSource.UpdateCachesAfterAdd(AX, AY: Double);
begin
  if FExtentIsValid then begin
    UpdateMinMax(AX, FExtent.a.X, FExtent.b.X);
    UpdateMinMax(AY, FExtent.a.Y, FExtent.b.Y);
  end;
  if FValuesTotalIsValid then
    FValuesTotal += AY;
end;

{ TMWCRandomGenerator }

function TMWCRandomGenerator.Get: LongWord;
const
  MULT: array [0..4] of UInt64 = (5115, 1776, 1492, 2111111111, 1);
var
  i: Integer;
  s: UInt64;
begin
  s := 0;
  for i := 0 to High(FHistory) do
    s += MULT[i] * FHistory[i];
  FHistory[3] := FHistory[2];
  FHistory[2] := FHistory[1];
  FHistory[1] := FHistory[0];
  FHistory[4] := Hi(s);
  FHistory[0] := Lo(s);
  Result := FHistory[0];
end;

function TMWCRandomGenerator.GetInRange(AMin, AMax: Integer): Integer;
var
  m: UInt64;
begin
  m := AMax - AMin + 1;
  m *= Get;
  // m is now equidistributed on [0, (2^32-1) * range],
  // so its upper double word is equidistributed on [0, range].
  Result := Integer(Hi(m)) + AMin;
end;

procedure TMWCRandomGenerator.SetSeed(const AValue: Integer);
var
  i: Integer;
begin
  FHistory[0] := AValue;
  // Use trivial LCG for seeding
  for i := 1 to High(FHistory) do
    FHistory[i] := Lo(Int64(FHistory[i - 1]) * 29943829 - 1);
  // Skip some initial values to increase randomness.
  for i := 1 to 20 do
    Get;
end;

{ TRandomChartSource }

constructor TRandomChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCurItem.Color := clTAColor;
  FRNG := TMWCRandomGenerator.Create;
  FYCount := 1;
  RandSeed := Trunc(Frac(Now) * MaxInt);
end;

destructor TRandomChartSource.Destroy;
begin
  FreeAndNil(FRNG);
  inherited;
end;

function TRandomChartSource.GetCount: Integer;
begin
  Result := FPointsNumber;
end;

function TRandomChartSource.GetItem(AIndex: Integer): PChartDataItem;

  function GetRandomY: Double;
  begin
    if YMax <= YMin then
      Result := YMin
    else
      Result := FRNG.Get / High(LongWord) * (YMax - YMin) + YMin;
  end;

var
  i: Integer;
begin
  if FCurIndex > AIndex then begin
    FRNG.Seed := FRandSeed;
    FCurIndex := -1;
  end;
  while FCurIndex < AIndex do begin
    Inc(FCurIndex);
    if XMax <= XMin then
      FCurItem.X := XMin
    else begin
      if FRandomX then
        FCurItem.X := FRNG.Get / High(LongWord)
      else
        FCurItem.X := FCurIndex / (Count - 1);
      FCurItem.X := FCurItem.X * (XMax - XMin) + XMin;
    end;
    if YCount > 0 then begin
      FCurItem.Y := GetRandomY;
      SetLength(FCurItem.YList, Max(YCount - 1, 0));
      for i := 0 to YCount - 2 do
        FCurItem.YList[i] := GetRandomY;
    end;
  end;
  Result := @FCurItem;
end;

function TRandomChartSource.IsSorted: Boolean;
begin
  Result := not RandomX;
end;

procedure TRandomChartSource.SetPointsNumber(const AValue: Integer);
begin
  if FPointsNumber = AValue then exit;
  FPointsNumber := AValue;
  InvalidateCaches;
  Notify;
end;

procedure TRandomChartSource.SetRandomX(const AValue: Boolean);
begin
  if FRandomX = AValue then exit;
  FRandomX := AValue;
  InvalidateCaches;
  Notify;
end;

procedure TRandomChartSource.SetRandSeed(const AValue: Integer);
begin
  if FRandSeed = AValue then exit;
  FRandSeed := AValue;
  FRNG.Seed := AValue;
  FCurIndex := -1;
  InvalidateCaches;
  Notify;
end;

procedure TRandomChartSource.SetXMax(const AValue: Double);
begin
  if FXMax = AValue then exit;
  FXMax := AValue;
  InvalidateCaches;
  Notify;
end;

procedure TRandomChartSource.SetXMin(const AValue: Double);
begin
  if FXMin = AValue then exit;
  FXMin := AValue;
  InvalidateCaches;
  Notify;
end;

procedure TRandomChartSource.SetYCount(AValue: Cardinal);
begin
  if YCount = AValue then exit;
  FYCount := AValue;
  InvalidateCaches;
  Notify;
end;

procedure TRandomChartSource.SetYMax(const AValue: Double);
begin
  if FYMax = AValue then exit;
  FYMax := AValue;
  InvalidateCaches;
  Notify;
end;

procedure TRandomChartSource.SetYMin(const AValue: Double);
begin
  if FYMin = AValue then exit;
  FYMin := AValue;
  InvalidateCaches;
  Notify;
end;

{ TIntervalChartSource }

function TIntervalChartSource.GetCount: Integer;
begin
  Result := 0;
end;

function TIntervalChartSource.GetItem(AIndex: Integer): PChartDataItem;
begin
  Unused(AIndex);
  Result := nil;
end;

procedure TIntervalChartSource.SetYCount(AValue: Cardinal);
begin
  Unused(AValue);
  raise EYCountError.Create('Can not set YCount');
end;

procedure TIntervalChartSource.ValuesInRange(
  AMin, AMax: Double; const AFormat: String; AUseY: Boolean;
  var AValues: TDoubleDynArray; var ATexts: TStringDynArray);
var
  i: Integer;
begin
  Unused(AUseY);
  if AMin > AMax then exit;
  AValues := GetIntervals(AMin, AMax, false);
  SetLength(ATexts, Length(AValues));
  for i := 0 to High(AValues) do
    // Extra format arguments for compatibility with FormatItem.
    ATexts[i] := Format(AFormat, [AValues[i], 0.0, '', 0.0, 0.0]);
end;

{ TDateTimeIntervalChartSource }

constructor TDateTimeIntervalChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSteps := DATE_TIME_STEPS_ALL;
end;

procedure TDateTimeIntervalChartSource.ValuesInRange(AMin, AMax: Double;
  const AFormat: String; AUseY: Boolean; var AValues: TDoubleDynArray;
  var ATexts: TStringDynArray);
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
begin
  if (AMax - AMin) / STEP_INTERVALS[dtsCentury] > MAX_STEPS then begin
    inherited ValuesInRange(
      AMin / STEP_INTERVALS[dtsYear], AMax / STEP_INTERVALS[dtsYear],
      AFormat, AUseY, AValues, ATexts);
    exit;
  end;
  s := Low(s);
  while s < High(s) do begin
    si := STEP_INTERVALS[s];
    if (s in Steps) and ((AMax - AMin) / si > MIN_STEPS) then
      break;
    Inc(s);
  end;
  start := Int(AMin / si - 1) * si;
  x := start;
  cnt := 0;
  while x <= AMax do begin
    if x >= AMin then
      cnt += 1;
    x += si;
  end;
  i := Length(AValues);
  SetLength(AValues, i + cnt);
  SetLength(ATexts, i + cnt);

  FillChar(prevSt, SizeOf(prevSt), $FF);
  x := start;
  while x <= AMax do begin
   if x >= AMin then begin
      AValues[i] := x;
      ATexts[i] := Format(AFormat, [x, 0.0, FormatLabel, 0.0, 0.0]);
      i += 1;
    end;
    case s of
      dtsCentury: x := IncYear(x, 100);
      dtsDecade: x := IncYear(x, 10);
      dtsYear: x := IncYear(x);
      dtsMonth: x := IncMonth(x);
      otherwise x += si;
    end;
  end;
end;

{ TUserDefinedChartSource }

procedure TUserDefinedChartSource.EndUpdate;
begin
  // There is no way to detect if the extent changed --
  // so call Reset to be a bit safer, but a bit slower.
  Reset;
  inherited EndUpdate;
end;

function TUserDefinedChartSource.GetCount: Integer;
begin
  Result := FPointsNumber;
end;

function TUserDefinedChartSource.GetItem(AIndex: Integer): PChartDataItem;
begin
  SetDataItemDefaults(FItem);
  if Assigned(FOnGetChartDataItem) then
    FOnGetChartDataItem(Self, AIndex, FItem);
  Result := @FItem;
end;

function TUserDefinedChartSource.IsSorted: Boolean;
begin
  Result := Sorted;
end;

procedure TUserDefinedChartSource.Reset;
begin
  InvalidateCaches;
  Notify;
end;

procedure TUserDefinedChartSource.SetOnGetChartDataItem(
  const AValue: TGetChartDataItemEvent);
begin
  if TMethod(FOnGetChartDataItem) = TMethod(AValue) then exit;
  FOnGetChartDataItem := AValue;
  Reset;
end;

procedure TUserDefinedChartSource.SetPointsNumber(const AValue: Integer);
begin
  if FPointsNumber = AValue then exit;
  FPointsNumber := AValue;
  Reset;
end;

procedure TUserDefinedChartSource.SetYCount(AValue: Cardinal);
begin
  if FYCount = AValue then exit;
  FYCount := AValue;
  SetLength(FItem.YList, Max(YCount - 1, 0));
  Reset;
end;

{ TCalculatedChartSource }

procedure TCalculatedChartSource.CalcAccumulation(AIndex: Integer);
var
  i, ar: Integer;
begin
  FHistory.Capacity := AccumulationRange;
  ar := IfThen(AccumulationRange = 0, MaxInt, AccumulationRange);
  if FIndex = AIndex - 1 then begin
    ExtractItem(FItem, AIndex);
    FHistory.AddLast(FItem);
  end
  else if FIndex = AIndex + 1 then begin
    if AccumulationRange = 0 then begin
      ExtractItem(FItem, FIndex);
      FHistory.RemoveValue(FItem);
      ExtractItem(FItem, AIndex);
    end
    else begin
      i := AIndex - AccumulationRange + 1;
      if i < 0 then
        FHistory.RemoveLast
      else begin
        ExtractItem(FItem, i);
        FHistory.AddFirst(FItem);
      end;
      FItem := FHistory.GetPLast^;
    end;
  end
  else begin
    FHistory.Clear;
    for i := Max(AIndex - ar + 1, 0) to AIndex do begin
      ExtractItem(FItem, i);
      FHistory.AddLast(FItem);
    end;
  end;
  FHistory.GetSum(FItem);
  if AccumulationMethod = camAverage then begin
    FItem.Y /= Min(ar, AIndex + 1);
    for i := 0 to High(FItem.YList) do
      FItem.YList[i] /= Min(ar, AIndex + 1);
  end;
end;

procedure TCalculatedChartSource.CalcPercentage;
var
  s: Double;
  i: Integer;
begin
  if not Percentage then exit;
  s := (FItem.Y + Sum(FItem.YList)) * PERCENT;
  FItem.Y /= s;
  for i := 0 to High(FItem.YList) do
    FItem.YList[i] /= s;
end;

procedure TCalculatedChartSource.Changed(ASender: TObject);
begin
  if
    (FOrigin <> nil) and (ASender = FOrigin) and
    (FOrigin.YCount <> FOriginYCount)
  then begin
    UpdateYOrder;
    exit;
  end;
  FIndex := -1;
  InvalidateCaches;
  Notify;
end;

constructor TCalculatedChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAccumulationRange := 1;
  FIndex := -1;
  FHistory := TChartSourceBuffer.Create;
  FListener := TListener.Create(@FOrigin, @Changed);
end;

destructor TCalculatedChartSource.Destroy;
begin
  FreeAndNil(FHistory);
  FreeAndNil(FListener);
  inherited Destroy;
end;

procedure TCalculatedChartSource.ExtractItem(
  out AItem: TChartDataItem; AIndex: Integer);
var
  t: TDoubleDynArray;
  i: Integer;
begin
  AItem := Origin[AIndex]^;
  SetLength(t, Length(FYOrder));
  for i := 0 to High(FYOrder) do
    t[i] := AItem.YList[FYOrder[i]];
  AItem.YList := t;
end;

function TCalculatedChartSource.GetCount: Integer;
begin
  if Origin <> nil then
    Result := Origin.Count
  else
    Result := 0;
end;

function TCalculatedChartSource.GetItem(AIndex: Integer): PChartDataItem;
begin
  if Origin = nil then exit(nil);
  Result := @FItem;
  if FIndex = AIndex then exit;
  if (AccumulationMethod = camNone) or (AccumulationRange = 1) then
    ExtractItem(FItem, AIndex)
  else
    CalcAccumulation(AIndex);
  CalcPercentage;
  FIndex := AIndex;
end;

function TCalculatedChartSource.IsSorted: Boolean;
begin
  if Origin <> nil then
    Result := Origin.IsSorted
  else
    Result := false;
end;

procedure TCalculatedChartSource.SetAccumulationMethod(
  AValue: TChartAccumulationMethod);
begin
  if FAccumulationMethod = AValue then exit;
  FAccumulationMethod := AValue;
  Changed(nil);
end;

procedure TCalculatedChartSource.SetAccumulationRange(AValue: Integer);
begin
  if FAccumulationRange = AValue then exit;
  FAccumulationRange := AValue;
  Changed(nil);
end;

procedure TCalculatedChartSource.SetOrigin(AValue: TCustomChartSource);
begin
  if AValue = Self then
      AValue := nil;
  if FOrigin = AValue then exit;
  if FOrigin <> nil then
    FOrigin.Broadcaster.Unsubscribe(FListener);
  FOrigin := AValue;
  if FOrigin <> nil then
    FOrigin.Broadcaster.Subscribe(FListener);
  UpdateYOrder;
end;

procedure TCalculatedChartSource.SetPercentage(AValue: Boolean);
begin
  if FPercentage = AValue then exit;
  FPercentage := AValue;
  Changed(nil);
end;

procedure TCalculatedChartSource.SetReorderYList(const AValue: String);
begin
  if FReorderYList = AValue then exit;
  FReorderYList := AValue;
  UpdateYOrder;
end;

procedure TCalculatedChartSource.SetYCount(AValue: Cardinal);
begin
  Unused(AValue);
  raise EYCountError.Create('Can not set YCount');
end;

procedure TCalculatedChartSource.UpdateYOrder;
var
  order: TStringList;
  i: Integer;
begin
  if FOrigin = nil then begin
    FOriginYCount := 0;
    FYCount := 0;
    FYOrder := nil;
    FItem.YList := nil;
    Changed(nil);
    exit;
  end;

  FOriginYCount := FOrigin.YCount;
  if ReorderYList = '' then begin
    FYCount := FOrigin.YCount;
    SetLength(FYOrder,  Max(FYCount - 1, 0));
    for i := 0 to High(FYOrder) do
      FYOrder[i] := i;
  end
  else begin
    order := TStringList.Create;
    try
      order.CommaText := ReorderYList;
      SetLength(FYOrder, order.Count);
      for i := 0 to High(FYOrder) do
        FYOrder[i] :=
          EnsureRange(StrToIntDef(order[i], 0), 0, FOrigin.YCount - 2);
      FYCount := Length(FYOrder) + 1;
    finally
      order.Free;
    end;
  end;

  SetLength(FItem.YList, Length(FYOrder));
  Changed(nil);
end;

end.

