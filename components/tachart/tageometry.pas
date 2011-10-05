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
unit TAGeometry;

{$H+}

interface

uses
  TAChartUtils, Types;

procedure BoundingBoxToCenterAndHalfRadius(
  AX1, AY1, AX2, AY2: Integer; out ACX, ACY, ARX, ARY: Integer);
function CopyPoints(
  APoints: array of TPoint; AStartIndex, ANumPts: Integer): TPointArray;
function DoublePoint(AX, AY: Double): TDoublePoint; inline;
function DoubleRect(AX1, AY1, AX2, AY2: Double): TDoubleRect; inline;
procedure ExpandRect(var ARect: TDoubleRect; const APoint: TDoublePoint); inline;
procedure ExpandRect(var ARect: TRect; const APoint: TPoint); inline;
procedure ExpandRect(
  var ARect: TRect; const ACenter: TPoint; ARadius: Integer;
  AAngle1, AAngle2: Double); inline;
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
procedure NormalizeRect(var ARect: TRect); overload;
procedure NormalizeRect(var ARect: TDoubleRect); overload;
function MakeSquare(const ARect: TRect): TRect;
function MaxPoint(const A, B: TPoint): TPoint; inline;
function MeasureRotatedRect(const ASize: TPoint; AAngle: Double): TSize;
function PointDist(const A, B: TPoint): Integer; inline;
function PointDistX(const A, B: TPoint): Integer; inline;
function PointDistY(const A, B: TPoint): Integer; inline;
function ProjToRect(
  const APt: TDoublePoint; const ARect: TDoubleRect): TDoublePoint;
function RectIntersectsRect(
  var ARect: TDoubleRect; const AFixed: TDoubleRect): Boolean;
function RotatePoint(const APoint: TDoublePoint; AAngle: Double): TDoublePoint; overload;
function RotatePoint(const APoint: TPoint; AAngle: Double): TPoint; overload;
function RotateRect(const ASize: TPoint; AAngle: Double): TPointArray;

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
operator = (const A, B: TDoublePoint): Boolean; overload; inline;
operator = (const A, B: TDoubleRect): Boolean; overload; inline;
operator <= (const A, B: TDoublePoint): Boolean; overload; inline;
operator :=(const APoint: TPoint): TSize; inline;
operator :=(const ASize: TSize): TPoint; inline;

implementation

uses
  Math, TAMath;

function PointLineSide(AP, A1, A2: TPoint): TValueSign; forward;

procedure BoundingBoxToCenterAndHalfRadius(
  AX1, AY1, AX2, AY2: Integer; out ACX, ACY, ARX, ARY: Integer);
begin
  ACX := (AX1 + AX2) div 2;
  ACY := (AY1 + AY2) div 2;
  ARX := Abs(AX1 - AX2) div 2;
  ARY := Abs(AY1 - AY2) div 2;
end;

function CopyPoints(
  APoints: array of TPoint; AStartIndex, ANumPts: Integer): TPointArray;
var
  i: Integer;
begin
  Assert(ANumPts >= 0);
  SetLength(Result, ANumPts);
  for i := 0 to ANumPts - 1 do
    Result[i] := APoints[i + AStartIndex];
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

procedure ExpandRect(var ARect: TDoubleRect; const APoint: TDoublePoint);
begin
  UpdateMinMax(APoint.X, ARect.a.X, ARect.b.X);
  UpdateMinMax(APoint.Y, ARect.a.Y, ARect.b.Y);
end;

procedure ExpandRect(var ARect: TRect; const APoint: TPoint);
begin
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

function MakeSquare(const ARect: TRect): TRect;
var
  c: TPoint;
  w, h: Integer;
begin
  c := CenterPoint(ARect);
  Result := ARect;
  w := Abs(Result.Right - Result.Left);
  h := Abs(Result.Bottom - Result.Top);
  if w > h then begin
    Result.Left := c.X - h div 2;
    Result.Right := c.X + h div 2;
  end
  else begin
    Result.Top := c.Y - w div 2;
    Result.Bottom := c.Y + w div 2;
  end;
end;

function MaxPoint(const A, B: TPoint): TPoint;
begin
  Result.X := Max(A.X, B.X);
  Result.Y := Max(A.Y, B.Y);
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

function PointLineSide(AP, A1, A2: TPoint): TValueSign;
var
  a1x, a1y: Int64;
begin
  a1x := A1.X;
  a1y := A1.Y;
  Result := Sign((AP.X - a1x) * (A2.Y - a1y) - (AP.Y - a1y) * (A2.X - a1x));
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

function ProjToRect(
  const APt: TDoublePoint; const ARect: TDoubleRect): TDoublePoint;
begin
  Result.X := EnsureRange(APt.X, ARect.a.X, ARect.b.X);
  Result.Y := EnsureRange(APt.Y, ARect.a.Y, ARect.b.Y);
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

function RotateRect(const ASize: TPoint; AAngle: Double): TPointArray;
var
  i: Integer;
begin
  SetLength(Result, 4);
  Result[0] := -ASize div 2;
  Result[2] := Result[0] + ASize;
  Result[1] := Point(Result[2].X, Result[0].Y);
  Result[3] := Point(Result[0].X, Result[2].Y);
  for i := 0 to High(Result) do
    Result[i] := RotatePoint(Result[i], AAngle);
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

operator = (const A, B: TDoublePoint): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

operator = (const A, B: TDoubleRect): Boolean;
begin
  Result := (A.a = B.a) and (A.b = B.b);
end;

operator <= (const A, B: TDoublePoint): Boolean;
begin
  Result := (A.X <= B.X) and (A.Y <= B.Y);
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

end.

