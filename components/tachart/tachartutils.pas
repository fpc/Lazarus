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
  Graphics, Math, Types, SysUtils;

const
  MaxColor = 15;
  Colors: array [1..MaxColor] of TColor = (
    clRed, clGreen, clYellow, clBlue, clWhite, clGray, clFuchsia,
    clTeal, clNavy, clMaroon, clLime, clOlive, clPurple, clSilver, clAqua);
  clTAColor = clScrollBar;
  CHART_COMPONENT_IDE_PAGE = 'Chart';
  PERCENT = 0.01;

type
  EChartError = class(Exception);

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

  TChartDistance = 0..MaxInt;

  TPointDistFunc = function (const A, B: TPoint): Integer;

  TAxisScale = (asIncreasing, asDecreasing, asLogIncreasing, asLogDecreasing);

  TPenBrushFont = set of (pbfPen, pbfBrush, pbfFont);

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

  TSeriesPointerStyle = (
    psNone, psRectangle, psCircle, psCross, psDiagCross, psStar,
    psLowBracket, psHighBracket, psLeftBracket, psRightBracket, psDiamond);

  { TPenBrushFontRecall }

  TPenBrushFontRecall = class
  private
    FBrush: TBrush;
    FCanvas: TCanvas;
    FFont: TFont;
    FPen: TPen;
  public
    constructor Create(ACanvas: TCanvas; AParams: TPenBrushFont);
    destructor Destroy; override;
    procedure Recall;
  end;

  TCaseOfTwo = (cotNone, cotFirst, cotSecond, cotBoth);

const
  // 0-value, 1-percent, 2-label, 3-total, 4-xvalue
  SERIES_MARK_FORMATS: array [TSeriesMarksStyle] of String = (
    '', '',
    '%0:g', // smsValue
    '%1:.2f%%', // smsPercent
    '%2:s', // smsLabel
    '%2:s %1:.2f%%', // smsLabelPercent
    '%2:s %0:g', // smsLabelValue
    '%2:s', // smsLegend: not sure what it means, left for Delphi compatibility
    '%1:.2f%% of %3:g', // smsPercentTotal
    '%1:.2f%% of %3:g', // smsLabelPercentTotal
    '%4:g' // smsXValue
  );
  EmptyDoubleRect: TDoubleRect = (coords: (0, 0, 0, 0));
  EmptyExtent: TDoubleRect =
    (coords: (Infinity, Infinity, NegInfinity, NegInfinity));
  CASE_OF_TWO: array [Boolean, Boolean] of TCaseOfTwo =
    ((cotNone, cotSecond), (cotFirst, cotBoth));

procedure CalculateIntervals(
  AMin, AMax: Double; AxisScale: TAxisScale; out AStart, AStep: Double);

function DoubleRect(AX1, AY1, AX2, AY2: Double): TDoubleRect; inline;

procedure DrawLineDepth(ACanvas: TCanvas; AX1, AY1, AX2, AY2, ADepth: Integer);
procedure DrawLineDepth(ACanvas: TCanvas; const AP1, AP2: TPoint; ADepth: Integer);

procedure Exchange(var A, B: Integer); overload;
procedure Exchange(var A, B: Double); overload;
procedure Exchange(var A, B: TDoublePoint); overload;

procedure ExpandRange(var ALo, AHi: Double; ACoeff: Double); inline;

function GetIntervals(AMin, AMax: Double; AInverted: Boolean): TDoubleDynArray;

function LineIntersectsRect(
  var AA, AB: TDoublePoint; const ARect: TDoubleRect): Boolean;

procedure NormalizeRect(var ARect: TRect);

function PointDist(const A, B: TPoint): Integer; inline;
function PointDistX(const A, B: TPoint): Integer; inline;
function PointDistY(const A, B: TPoint): Integer; inline;

function RectIntersectsRect(
  var ARect: TDoubleRect; const AFixed: TDoubleRect): Boolean;

function RoundChecked(A: Double): Integer; inline;

// Call this to silence 'parameter is unused' hint
procedure Unused(const A1);
procedure Unused(const A1, A2);

procedure UpdateMinMax(AValue: Double; var AMin, AMax: Double);

operator +(const A: TPoint; B: TSize): TPoint;

implementation

uses
  LCLIntf, LCLType;

procedure CalculateIntervals(
  AMin, AMax: Double; AxisScale: TAxisScale; out AStart, AStep: Double);
var
  extent, extentTmp, stepCount, scale, maxStepCount, m: Double;
  i: Integer;
const
  GOOD_STEPS: array [1..3] of Double = (0.2, 0.5, 1.0);
begin
  extent := AMax - AMin;
  AStep := 1;
  AStart := AMin;
  if extent <= 0 then exit;

  maxStepCount := 0;
  scale := 1.0;
  for i := Low(GOOD_STEPS) to High(GOOD_STEPS) do begin
    extentTmp := extent / GOOD_STEPS[i];
    m := power(10, Round(log10(extentTmp)));
    while extentTmp * m > 10 do
      m *= 0.1;
    while extentTmp * m <= 1 do
      m *= 10;
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

function DoubleRect(AX1, AY1, AX2, AY2: Double): TDoubleRect; inline;
begin
  Result.a.X := AX1;
  Result.a.Y := AY1;
  Result.b.X := AX2;
  Result.b.Y := AY2;
end;

procedure DrawLineDepth(ACanvas: TCanvas; AX1, AY1, AX2, AY2, ADepth: Integer);
begin
  DrawLineDepth(ACanvas, Point(AX1, AY1), Point(AX2, AY2), ADepth);
end;

procedure DrawLineDepth(
  ACanvas: TCanvas; const AP1, AP2: TPoint; ADepth: Integer);
var
  d: TSize;
begin
  d := Size(ADepth, -ADepth);
  ACanvas.Polygon([AP1, AP1 + d, AP2 + d, AP2]);
end;

procedure Exchange(var A, B: Integer); overload;
var
  t: Integer;
begin
  t := A;
  A := B;
  B := t;
end;

procedure Exchange(var A, B: Double); overload;
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

procedure ExpandRange(var ALo, AHi: Double; ACoeff: Double); inline;
var
  d: Double;
begin
  d := AHi - ALo;
  ALo -= d * ACoeff;
  AHi += d * ACoeff;
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

procedure NormalizeRect(var ARect: TRect);
begin
  with ARect do begin
    if Left > Right then
      Exchange(Left, Right);
    if Top > Bottom then
      Exchange(Top, Bottom);
  end;
end;

function PointDist(const A, B: TPoint): Integer;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;

function PointDistX(const A, B: TPoint): Integer;
begin
  Result := Abs(A.X - B.X);
end;

function PointDistY(const A, B: TPoint): Integer; inline;
begin
  Result := Abs(A.Y - B.Y);
end;

{$HINTS OFF}

function RectIntersectsRect(
  var ARect: TDoubleRect; const AFixed: TDoubleRect): Boolean;

  function RangesIntersect(L1, R1, L2, R2: Double; out L, R: Double): Boolean;
  begin
    if L1 > R1 then Exchange(L1, R1);
    if L2 > R2 then Exchange(L2, R2);
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

function RoundChecked(A: Double): Integer;
begin
  Result := Round(EnsureRange(A, -MaxInt, MaxInt));
end;

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

operator + (const A: TPoint; B: TSize): TPoint;
begin
  Result.X := A.X + B.cx;
  Result.Y := A.Y + B.cy;
end;

{ TPenBrushFontRecall }

constructor TPenBrushFontRecall.Create(ACanvas: TCanvas; AParams: TPenBrushFont);
begin
  inherited Create;
  FCanvas := ACanvas;
  if pbfPen in AParams then begin
    FPen := TPen.Create;
    FPen.Assign(FCanvas.Pen);
  end;
  if pbfBrush in AParams then begin
    FBrush := TBrush.Create;
    FBrush.Assign(FCanvas.Brush);
  end;
  if pbfFont in AParams then begin
    FFont := TFont.Create;
    FFont.Assign(FCanvas.Font);
  end;
end;

destructor TPenBrushFontRecall.Destroy;
begin
  Recall;
  inherited;
end;

procedure TPenBrushFontRecall.Recall;
begin
  if FPen <> nil then begin
    FCanvas.Pen.Assign(FPen);
    FreeAndNil(FPen);
  end;
  if FBrush <> nil then begin
    FCanvas.Brush.Assign(FBrush);
    FreeAndNil(FBrush);
  end;
  if FFont <> nil then begin
    FCanvas.Font.Assign(FFont);
    FreeAndNil(FFont);
  end;
end;

end.
