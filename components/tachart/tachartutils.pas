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

  TIndexedComponent = class (TComponent)
  protected
    function GetIndex: Integer; virtual; abstract;
    procedure SetIndex(AValue: Integer); virtual; abstract;
  public
    property Index: Integer read GetIndex write SetIndex;
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
    procedure Notify(ASender: TObject); virtual;
    property IsListening: Boolean read GetIsListening;
  end;

  { TBroadcaster }

  TBroadcaster = class(TFPList)
  public
    destructor Destroy; override;
  public
    procedure Broadcast(ASender: TObject);
    procedure Subscribe(AListener: TListener);
    procedure Unsubscribe(AListener: TListener);
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

const
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

function DoublePoint(AX, AY: Double): TDoublePoint; inline;
function DoubleRect(AX1, AY1, AX2, AY2: Double): TDoubleRect; inline;
function DoubleInterval(AStart, AEnd: Double): TDoubleInterval; inline;

procedure EnsureOrder(var A, B: Integer); overload; inline;
procedure EnsureOrder(var A, B: Double); overload; inline;

procedure Exchange(var A, B: Integer); overload; inline;
procedure Exchange(var A, B: Double); overload; inline;
procedure Exchange(var A, B: TDoublePoint); overload; inline;
procedure Exchange(var A, B: String); overload; inline;

procedure ExpandRange(var ALo, AHi: Double; ACoeff: Double); inline;
procedure ExpandRect(var ARect: TDoubleRect; const APoint: TDoublePoint); inline;
procedure ExpandRect(var ARect: TRect; const APoint: TPoint); inline;
procedure ExpandRect(
  var ARect: TRect; const ACenter: TPoint; ARadius: Integer;
  AAngle1, AAngle2: Double); inline;

function GetIntervals(AMin, AMax: Double; AInverted: Boolean): TDoubleDynArray;

function InterpolateRGB(AColor1, AColor2: Integer; ACoeff: Double): Integer;

function IsPointOnLine(const AP, A1, A2: TPoint): Boolean; inline;
function IsPointInPolygon(
  const AP: TPoint; const APolygon: array of TPoint): Boolean;
function IsPointInRect(const AP, A1, A2: TPoint): Boolean; inline; overload;
function IsPointInRect(const AP: TPoint; const AR: TRect): Boolean; inline; overload;
function IsRectInRect(const AInner, AOuter: TRect): Boolean; inline;
function IsLineIntersectsLine(const AA, AB, AC, AD: TPoint): Boolean;
function IsPolygonIntersectsPolygon(const AP1, AP2: array of TPoint): Boolean;
function LineIntersectsRect(
  var AA, AB: TDoublePoint; const ARect: TDoubleRect): Boolean;

function MeasureRotatedRect(const ASize: TPoint; AAngle: Double): TSize;

procedure NormalizeRect(var ARect: TRect); overload;
procedure NormalizeRect(var ARect: TDoubleRect); overload;

function OrientToRad(AOrient: Integer): Double; inline;

function PointDist(const A, B: TPoint): Integer; inline;
function PointDistX(const A, B: TPoint): Integer; inline;
function PointDistY(const A, B: TPoint): Integer; inline;

function ProjToRect(
  const APt: TDoublePoint; const ARect: TDoubleRect): TDoublePoint;

function RadToDeg16(ARad: Double): Integer; inline;
function RadToOrient(ARad: Double): Integer; inline;

function RectIntersectsRect(
  var ARect: TDoubleRect; const AFixed: TDoubleRect): Boolean;

function RotatePoint(const APoint: TDoublePoint; AAngle: Double): TDoublePoint; overload;
function RotatePoint(const APoint: TPoint; AAngle: Double): TPoint; overload;
function RoundChecked(A: Double): Integer; inline;

function SafeInfinity: Double; inline;
function SafeInRange(AValue, ABound1, ABound2: Double): Boolean;

// Call this to silence 'parameter is unused' hint
procedure Unused(const A1);
procedure Unused(const A1, A2);

procedure UpdateMinMax(AValue: Double; var AMin, AMax: Double); overload;
procedure UpdateMinMax(AValue: Integer; var AMin, AMax: Integer); overload;

operator +(const A: TPoint; B: TSize): TPoint; overload; inline;
operator +(const A, B: TPoint): TPoint; overload; inline;
operator +(const A, B: TDoublePoint): TDoublePoint; overload; inline;
operator -(const A: TPoint): TPoint; overload; inline;
operator -(const A, B: TPoint): TPoint; overload; inline;
operator -(const A, B: TDoublePoint): TDoublePoint; overload; inline;
operator div(const A: TPoint; ADivisor: Integer): TPoint; inline;
operator *(const A: TPoint; AMultiplier: Integer): TPoint; inline;
operator *(const A, B: TPoint): TPoint; inline;
operator *(const A, B: TDoublePoint): TDoublePoint; overload; inline;
operator /(const A, B: TDoublePoint): TDoublePoint; overload; inline;
operator =(const A, B: TMethod): Boolean; overload; inline;

operator :=(const APoint: TPoint): TSize; inline;
operator :=(const ASize: TSize): TPoint; inline;

var
  DrawData: TDrawDataRegistry;

implementation

const
  ORIENTATION_UNITS_PER_DEG = 10;

function PointLineSide(AP, A1, A2: TPoint): TValueSign; forward;

function BoundsSize(ALeft, ATop: Integer; ASize: TSize): TRect; inline;
begin
  Result := Bounds(ALeft, ATop, ASize.cx, ASize.cy);
end;

procedure CalculateIntervals(
  AMin, AMax: Double; AxisScale: TAxisScale; out AStart, AStep: Double);
var
  extent, extentTmp, stepCount, scale, maxStepCount, m: Double;
  i: Integer;
const
  GOOD_STEPS: array [1..3] of Double = (0.2, 0.5, 1.0);
  BASE = 10;
begin
  extent := AMax - AMin;
  AStep := 1;
  AStart := AMin;
  if extent <= 0 then exit;

  maxStepCount := 0;
  scale := 1.0;
  for i := Low(GOOD_STEPS) to High(GOOD_STEPS) do begin
    extentTmp := extent / GOOD_STEPS[i];
    m := IntPower(BASE, Round(logn(BASE, extentTmp)));
    while extentTmp * m > BASE do
      m /= BASE;
    while extentTmp * m <= 1 do
      m *= BASE;
    stepCount := extentTmp * m;
    if stepCount > maxStepCount then begin
      maxStepCount := stepCount;
      scale := m;
      AStep := GOOD_STEPS[i] / m;
    end;
  end;
  case AxisScale of
    asIncreasing: begin
      // If 0 is in the interval, set it as a mark.
      if InRange(0, AMin, AMax) then
        AStart := 0
      else
        AStart := Round((AMin - AStep) * scale) / scale;
      while AStart > AMin do AStart -= AStep;
    end;
    asDecreasing: begin
      // If 0 is in the interval, set it as a mark.
      if InRange(0, AMin, AMax) then
        AStart := 0
      else
        AStart := Round((AMax + AStep) * scale) / scale;
      while AStart < AMax do AStart += AStep;
    end;
    asLogIncreasing: begin
      // FIXME: asLogIncreasing is still not implemented.
      // The following is the same code for asIncreasing;
      // If 0 is in the interval, set it as a mark.
      if InRange(0, AMin, AMax) then
        AStart := 0
      else
        AStart := Round((AMin - AStep) * scale) / scale;
      while AStart > AMin do AStart -= AStep;
    end;
    asLogDecreasing: begin
      // FIXME: asLogDecreasing is still not implemented.
      // The following is the same code for asIncreasing;
      // If 0 is in the interval, set it as a mark.
      if InRange(0, AMin, AMax) then
        AStart := 0
      else
        AStart := Round((AMax + AStep) * scale) / scale;
      while AStart < AMax do AStart += AStep;
    end;
  end; {case AxisScale}
end;

function DoublePoint(AX, AY: Double): TDoublePoint; inline;
begin
  Result.X := AX;
  Result.Y := AY;
end;

function DoubleRect(AX1, AY1, AX2, AY2: Double): TDoubleRect; inline;
begin
  Result.a.X := AX1;
  Result.a.Y := AY1;
  Result.b.X := AX2;
  Result.b.Y := AY2;
end;

function DoubleInterval(AStart, AEnd: Double): TDoubleInterval;
begin
  Result.FStart := AStart;
  Result.FEnd := AEnd;
end;

procedure EnsureOrder(var A, B: Integer); overload; inline;
begin
  if A > B then
    Exchange(A, B);
end;

procedure EnsureOrder(var A, B: Double); overload; inline;
begin
  if A > B then
    Exchange(A, B);
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

procedure ExpandRect(var ARect: TDoubleRect; const APoint: TDoublePoint);
begin
  NormalizeRect(ARect);
  UpdateMinMax(APoint.X, ARect.a.X, ARect.b.X);
  UpdateMinMax(APoint.Y, ARect.a.Y, ARect.b.Y);
end;

procedure ExpandRect(var ARect: TRect; const APoint: TPoint);
begin
  NormalizeRect(ARect);
  UpdateMinMax(APoint.X, ARect.Left, ARect.Right);
  UpdateMinMax(APoint.Y, ARect.Top, ARect.Bottom);
end;

procedure ExpandRect(
  var ARect: TRect; const ACenter: TPoint; ARadius: Integer;
  AAngle1, AAngle2: Double);
var
  p: TPoint;
  i, j: Integer;
begin
  p := Point(ARadius, 0);
  EnsureOrder(AAngle1, AAngle2);
  ExpandRect(ARect, RotatePoint(p, AAngle1) + ACenter);
  ExpandRect(ARect, RotatePoint(p, AAngle2) + ACenter);
  j := Floor(AAngle1 / Pi * 2);
  for i := j to j + 4 do
    if InRange(Pi / 2 * i, AAngle1, AAngle2) then
      ExpandRect(ARect, RotatePoint(p, Pi / 2 * i) + ACenter);
end;

function GetIntervals(AMin, AMax: Double; AInverted: Boolean): TDoubleDynArray;
const
  INV_TO_SCALE: array [Boolean] of TAxisScale = (asIncreasing, asDecreasing);
  K = 1e-10;
var
  start, step, m, m1: Double;
  markCount: Integer;
begin
  CalculateIntervals(AMin, AMax, INV_TO_SCALE[AInverted], start, step);
  AMin -= step * K;
  AMax += step * K;
  if AInverted then
    step := - step;
  m := start;
  markCount := 0;
  while true do begin
    if InRange(m, AMin, AMax) then
      Inc(markCount)
    else if markCount > 0 then
      break;
    m1 := m + step;
    if m1 = m then break;
    m := m1;
  end;
  SetLength(Result, markCount);
  m := start;
  markCount := 0;
  while true do begin
    if Abs(m / step) < K then
      m := 0;
    if InRange(m, AMin, AMax) then begin
      Result[markCount] := m;
      Inc(markCount);
    end
    else if markCount > 0 then
      break;
    m1 := m + step;
    if m1 = m then break;
    m := m1;
  end;
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

function IsPointOnLine(const AP, A1, A2: TPoint): Boolean;
begin
  Result := IsPointInRect(AP, A1, A2) and (PointLineSide(AP, A1, A2) = 0);
end;

function IsPointInPolygon(
  const AP: TPoint; const APolygon: array of TPoint): Boolean;
var
  i, count: Integer;
  p1, p2: TPoint;
  s1, s2: TValueSign;
begin
  if Length(APolygon) = 0 then exit(false);
  p1 := APolygon[High(APolygon)];
  for i := 0 to High(APolygon) do begin
    p2 := APolygon[i];
    if IsPointOnLine(AP, p1, p2) then exit(true);
    p1 := p2;
  end;
  count := 0;
  p1 := APolygon[High(APolygon)];
  for i := 0 to High(APolygon) do begin
    p2 := APolygon[i];
    s1 := Sign(p1.Y - AP.Y);
    s2 := Sign(p2.Y - AP.Y);
    case s1 * s2 of
      -1: count += Ord(PointLineSide(AP, p1, p2) = Sign(p1.Y - p2.Y));
      0: if s1 + s2 = 1 then begin
        if s1 = 0 then
          count += Ord(p1.X >= AP.X)
        else
          count += Ord(p2.X >= AP.X)
      end;
    end;
    p1 := p2;
  end;
  Result := count mod 2 = 1;
end;

function IsPointInRect(const AP, A1, A2: TPoint): Boolean;
begin
  Result := SafeInRange(AP.X, A1.X, A2.X) and SafeInRange(AP.Y, A1.Y, A2.Y);
end;

function IsPointInRect(const AP: TPoint; const AR: TRect): Boolean;
begin
  Result :=
    SafeInRange(AP.X, AR.Left, AR.Right) and
    SafeInRange(AP.Y, AR.Top, AR.Bottom);
end;

function IsRectInRect(const AInner, AOuter: TRect): Boolean;
begin
  Result :=
    IsPointInRect(AInner.TopLeft, AOuter) and
    IsPointInRect(AInner.BottomRight, AOuter);
end;

function IsLineIntersectsLine(const AA, AB, AC, AD: TPoint): Boolean;
var
  sa, sb, sc, sd: TValueSign;
begin
  sa := PointLineSide(AA, AC, AD);
  sb := PointLineSide(AB, AC, AD);
  if (sa = 0) and (sb = 0) then
    // All points are on the same infinite line.
    Result :=
      IsPointInRect(AA, AC, AD) or IsPointInRect(AB, AC, AD) or
      IsPointInRect(AC, AA, AB) or IsPointInRect(AD, AA, AB)
  else begin
    sc := PointLineSide(AC, AA, AB);
    sd := PointLineSide(AD, AA, AB);
    Result := (sa * sb <= 0) and (sc * sd <= 0);
  end;
end;

function IsPolygonIntersectsPolygon(const AP1, AP2: array of TPoint): Boolean;
var
  i, j: Integer;
  p1, p2: TPoint;
begin
  if (Length(AP1) = 0) or (Length(AP2) = 0) then exit(false);
  if IsPointInPolygon(AP1[0], AP2) or IsPointInPolygon(AP2[0], AP1) then
    exit(true);
  for i := 0 to High(AP1) do begin
    p1 := AP1[i];
    p2 := AP1[(i + 1) mod Length(AP1)];
    for j := 0 to High(AP2) do
      if IsLineIntersectsLine(p1, p2, AP2[j], AP2[(j + 1) mod Length(AP2)]) then
        exit(true);
  end;
  Result := false;
end;

function LineIntersectsRect(
  var AA, AB: TDoublePoint; const ARect: TDoubleRect): Boolean;
var
  dx, dy: Double;

  procedure AdjustX(var AP: TDoublePoint; ANewX: Double); inline;
  begin
    AP.Y += dy / dx * (ANewX - AP.X);
    AP.X := ANewX;
  end;

  procedure AdjustY(var AP: TDoublePoint; ANewY: Double); inline;
  begin
    AP.X += dx / dy * (ANewY - AP.Y);
    AP.Y := ANewY;
  end;

begin
  dx := AB.X - AA.X;
  dy := AB.Y - AA.Y;
  case CASE_OF_TWO[AA.X < ARect.a.X, AB.X < ARect.a.X] of
    cotFirst: AdjustX(AA, ARect.a.X);
    cotSecond: AdjustX(AB, ARect.a.X);
    cotBoth: exit(false);
  end;
  case CASE_OF_TWO[AA.X > ARect.b.X, AB.X > ARect.b.X] of
    cotFirst: AdjustX(AA, ARect.b.X);
    cotSecond: AdjustX(AB, ARect.b.X);
    cotBoth: exit(false);
  end;
  case CASE_OF_TWO[AA.Y < ARect.a.Y, AB.Y < ARect.a.Y] of
    cotFirst: AdjustY(AA, ARect.a.Y);
    cotSecond: AdjustY(AB, ARect.a.Y);
    cotBoth: exit(false);
  end;
  case CASE_OF_TWO[AA.Y > ARect.b.Y, AB.Y > ARect.b.Y] of
    cotFirst: AdjustY(AA, ARect.b.Y);
    cotSecond: AdjustY(AB, ARect.b.Y);
    cotBoth: exit(false);
  end;
  Result := true;
end;

function MeasureRotatedRect(const ASize: TPoint; AAngle: Double): TSize;
var
  pt1, pt2: TPoint;
begin
  pt1 := RotatePoint(ASize, AAngle);
  pt2 := RotatePoint(Point(ASize.X, -ASize.Y), AAngle);
  Result.cx := Max(Abs(pt1.X), Abs(pt2.X));
  Result.cy := Max(Abs(pt1.Y), Abs(pt2.Y));
end;

procedure NormalizeRect(var ARect: TRect);
begin
  with ARect do begin
    EnsureOrder(Left, Right);
    EnsureOrder(Top, Bottom);
  end;
end;

procedure NormalizeRect(var ARect: TDoubleRect); overload;
begin
  with ARect do begin
    EnsureOrder(a.X, b.X);
    EnsureOrder(a.Y, b.Y);
  end;
end;

function OrientToRad(AOrient: Integer): Double;
begin
  Result := DegToRad(AOrient / ORIENTATION_UNITS_PER_DEG);
end;

function PointDist(const A, B: TPoint): Integer;
begin
  Result := Min(Sqr(Int64(A.X) - B.X) + Sqr(Int64(A.Y) - B.Y), MaxInt);
end;

function PointDistX(const A, B: TPoint): Integer;
begin
  Result := Min(Abs(Int64(A.X) - B.X), MaxInt);
end;

function PointDistY(const A, B: TPoint): Integer; inline;
begin
  Result := Min(Abs(Int64(A.Y) - B.Y), MaxInt);
end;

function PointLineSide(AP, A1, A2: TPoint): TValueSign;
var
  a1x, a1y: Int64;
begin
  a1x := A1.X;
  a1y := A1.Y;
  Result := Sign((AP.X - a1x) * (A2.Y - a1y) - (AP.Y - a1y) * (A2.X - a1x));
end;

function ProjToRect(
  const APt: TDoublePoint; const ARect: TDoubleRect): TDoublePoint;
begin
  Result.X := EnsureRange(APt.X, ARect.a.X, ARect.b.X);
  Result.Y := EnsureRange(APt.Y, ARect.a.Y, ARect.b.Y);
end;

function RadToDeg16(ARad: Double): Integer;
begin
  Result := Round(RadToDeg(ARad) * 16);
end;

function RadToOrient(ARad: Double): Integer;
begin
  Result := Round(RadToDeg(ARad)) * ORIENTATION_UNITS_PER_DEG;
end;

function RectIntersectsRect(
  var ARect: TDoubleRect; const AFixed: TDoubleRect): Boolean;

  function RangesIntersect(L1, R1, L2, R2: Double; out L, R: Double): Boolean;
  begin
    EnsureOrder(L1, R1);
    EnsureOrder(L2, R2);
    L := Max(L1, L2);
    R := Min(R1, R2);
    Result := L <= R;
  end;

begin
  with ARect do
    Result :=
      RangesIntersect(a.X, b.X, AFixed.a.X, AFixed.b.X, a.X, b.X) and
      RangesIntersect(a.Y, b.Y, AFixed.a.Y, AFixed.b.Y, a.Y, b.Y);
end;

function RotatePoint(const APoint: TDoublePoint; AAngle: Double): TDoublePoint;
var
  sa, ca: Extended;
begin
  SinCos(AAngle, sa, ca);
  Result.X := ca * APoint.X - sa * APoint.Y;
  Result.Y := sa * APoint.X + ca * APoint.Y;
end;

function RotatePoint(const APoint: TPoint; AAngle: Double): TPoint;
var
  sa, ca: Extended;
begin
  SinCos(AAngle, sa, ca);
  Result.X := Round(ca * APoint.X - sa * APoint.Y);
  Result.Y := Round(sa * APoint.X + ca * APoint.Y);
end;

function RoundChecked(A: Double): Integer;
begin
  Result := Round(EnsureRange(A, -MaxInt, MaxInt));
end;

function SafeInfinity: Double;
begin
  {$IFOPT R+}{$DEFINE RangeChecking}{$ELSE}{$UNDEF RangeChecking}{$ENDIF}
  {$IFOPT Q+}{$DEFINE OverflowChecking}{$ELSE}{$UNDEF OverflowChecking}{$ENDIF}
  {$R-}{$Q-}
  Result := Infinity;
  {$IFDEF OverflowChecking}{$Q+}{$ENDIF}{$IFDEF RangeChecking}{$R+}{$ENDIF}
end;

function SafeInRange(AValue, ABound1, ABound2: Double): Boolean;
begin
  EnsureOrder(ABound1, ABound2);
  Result := InRange(AValue, ABound1, ABound2);
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

operator + (const A: TPoint; B: TSize): TPoint;
begin
  Result.X := A.X + B.cx;
  Result.Y := A.Y + B.cy;
end;

operator + (const A, B: TPoint): TPoint;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

operator + (const A, B: TDoublePoint): TDoublePoint;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

operator - (const A: TPoint): TPoint;
begin
  Result.X := - A.X;
  Result.Y := - A.Y;
end;

operator - (const A, B: TPoint): TPoint;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

operator - (const A, B: TDoublePoint): TDoublePoint;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

operator div(const A: TPoint; ADivisor: Integer): TPoint;
begin
  Result.X := A.X div ADivisor;
  Result.Y := A.Y div ADivisor;
end;

operator * (const A: TPoint; AMultiplier: Integer): TPoint;
begin
  Result.X := A.X * AMultiplier;
  Result.Y := A.Y * AMultiplier;
end;

operator * (const A, B: TPoint): TPoint;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
end;

operator * (const A, B: TDoublePoint): TDoublePoint;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
end;

operator / (const A, B: TDoublePoint): TDoublePoint;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
end;

operator = (const A, B: TMethod): Boolean;
begin
  Result := (A.Code = B.Code) and (A.Data = B.Data);
end;

operator := (const APoint: TPoint): TSize;
begin
  Result.cx := APoint.X;
  Result.cy := APoint.Y;
end;

operator := (const ASize: TSize): TPoint;
begin
  Result.X := ASize.cx;
  Result.Y := ASize.cy;
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
  FRef^ := nil;
end;

function TListener.GetIsListening: Boolean;
begin
  Result := FBroadcaster <> nil;
end;

procedure TListener.Notify(ASender: TObject);
begin
  FOnNotify(ASender)
end;

{ TBroadcaster }

procedure TBroadcaster.Broadcast(ASender: TObject);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TListener(Items[i]).Notify(ASender);
end;

destructor TBroadcaster.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TListener(Items[i]).Forget;
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

initialization

  DrawData := TDrawDataRegistry.Create;

finalization

  FreeAndNil(DrawData);

end.
