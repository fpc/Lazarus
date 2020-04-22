{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}
unit TAGeometry;

{$H+}

interface

uses
  TAChartUtils, Types;

type
  TPolygon = object
  public
    FPoints: TPointArray;
    FCount: Integer;
  public
    constructor Init;
    procedure Add(const APoint: TPoint);
    procedure AddNoDup(const APoint: TPoint); inline;
    function LastPoint: TPoint; inline;
    function Purge: TPointArray; inline;
  end;

  TEllipse = object
  public
    FC: TDoublePoint;
    FR: TDoublePoint;
    constructor InitBoundingBox(AX1, AY1, AX2, AY2: Integer);
  public
    function GetPoint(AParametricAngle: Double): TDoublePoint;
    procedure SliceToPolygon(
      AAngleStart, AAngleLength: Double; AStep: Integer; var APoly: TPolygon);
    function TesselateRadialPie(
      AAngleStart, AAngleLength: Double; AStep: Integer): TPointArray;
  end;

function CopyPoints(
  APoints: array of TPoint; AStartIndex, ANumPts: Integer): TPointArray;
function DotProduct(A, B: TDoublePoint): Double;
function DoublePoint(AX, AY: Double): TDoublePoint; inline; overload;
function DoublePoint(const AP: TPoint): TDoublePoint; inline; overload;
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
function NormalizeAngle(Angle: Double): Double;
procedure NormalizeRect(var ARect: TRect); overload;
procedure NormalizeRect(var ARect: TDoubleRect); overload;
function MakeSquare(const ARect: TRect): TRect;
function MakeCallout(
  const AShape: TPointArray; const ACenter, ATarget: TPoint;
  AAngle: Double): TPointArray;
function MaxPoint(const A, B: TPoint): TPoint; inline;
function MeasureRotatedRect(const ASize: TPoint; AAngle: Double): TSize;
function NextNumberSeq(
  const APoints: array of TDoublePoint; var AStart, AEnd: Integer): Boolean;
function PointDist(const A, B: TPoint): Integer; inline;
function PointDistX(const A, B: TPoint): Integer; inline;
function PointDistY(const A, B: TPoint): Integer; inline;
function PointLineDist(const P, A, B: TPoint): Integer; overload;
function PointLineDist(const P, A,B: TPoint; out Q: TPoint; out Inside: Boolean): Integer; overload;
function ProjToLine(const P, A, B: TDoublePoint): TDoublePoint; overload;
function ProjToLine(const P, A, B: TPoint): TPoint; overload;
function ProjToRect(
  const APt: TDoublePoint; const ARect: TDoubleRect): TDoublePoint;
function RectIntersectsRect(
  var ARect: TDoubleRect; const AFixed: TDoubleRect): Boolean;
function RotatePoint(const APoint: TDoublePoint; AAngle: Double): TDoublePoint; overload;
function RotatePoint(const APoint: TPoint; AAngle: Double): TPoint; overload;
function RotatePointX(AX, AAngle: Double): TPoint;
function RoundPoint(APoint: TDoublePoint): TPoint;
function TesselateRect(const ARect: TRect): TPointArray;
function TesselateEllipse(const ABounds: TRect; AStep: Integer): TPointArray;
function TesselateRoundRect(const ARect: TRect; ARadius, AStep: Integer): TPointArray;

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
operator * (const A: TDoublePoint; B: Double): TDoublePoint; overload; inline;
operator /(const A, B: TDoublePoint): TDoublePoint; overload; inline;
operator = (const A, B: TDoublePoint): Boolean; overload; inline;
operator = (const A, B: TDoubleRect): Boolean; overload; inline;
operator <= (const A, B: TDoublePoint): Boolean; overload; inline;
operator :=(const APoint: TPoint): TSize; inline;
operator :=(const ASize: TSize): TPoint; inline;

implementation

uses
  GraphMath, Math, TAMath;

function PointLineSide(AP, A1, A2: TPoint): TValueSign; forward;

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

function DotProduct(A, B: TDoublePoint): Double;
begin
  Result := A.X * B.X + A.Y * B.Y;
end;

function DoublePoint(AX, AY: Double): TDoublePoint; inline;
begin
  Result.X := AX;
  Result.Y := AY;
end;

function DoublePoint(const AP: TPoint): TDoublePoint;
begin
  Result.X := AP.X;
  Result.Y := AP.Y;
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
  i, j: Integer;
begin
  EnsureOrder(AAngle1, AAngle2);
  ExpandRect(ARect, RotatePointX(ARadius, AAngle1) + ACenter);
  ExpandRect(ARect, RotatePointX(ARadius, AAngle2) + ACenter);
  j := Floor(AAngle1 / Pi * 2);
  for i := j to j + 4 do
    if InRange(Pi / 2 * i, AAngle1, AAngle2) then
      ExpandRect(ARect, RotatePointX(ARadius, Pi / 2 * i) + ACenter);
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

  procedure AdjustX(var AP: TDoublePoint; ANewX: Double); inline;
  var
    dx: Double;
  begin
    dx := AB.X - AA.X;
    if not IsInfinite(dx) and not IsInfinite(AP.Y) then
      AP.Y += (AB.Y - AA.Y) / dx * (ANewX - AP.X);
    AP.X := ANewX;
  end;

  procedure AdjustY(var AP: TDoublePoint; ANewY: Double); inline;
  var
    dy: Double;
  begin
    dy := AB.Y - AA.Y;
    if not IsInfinite(dy) and not IsInfinite(AP.X) then
      AP.X += (AB.X - AA.X) / dy * (ANewY - AP.Y);
    AP.Y := ANewY;
  end;

begin
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

function MakeCallout(
  const AShape: TPointArray; const ACenter, ATarget: TPoint;
  AAngle: Double): TPointArray;
var
  AVector: TPoint;

  function Next(AIndex, ADir: Integer): Integer; inline;
  begin
    Result := (AIndex + Length(AShape) + ADir) mod Length(AShape);
  end;

  function NearestSide: Integer;
  begin
    for Result := 0 to High(AShape) do
      if
        IsLineIntersectsLine(
          ACenter, ATarget, AShape[Result], AShape[Next(Result, 1)])
      then
        exit;
    Result := -1;
  end;

  function ScalarProduct(const AP1, AP2: TPoint): Double; inline;
  begin
    Result := Double(AP1.X) * AP2.X + Double(AP1.Y) * AP2.Y;
  end;

  function CrossProductSign(const AP1, AP2: TPoint): Integer; inline;
  begin
    Result := Sign(Double(AP1.X) * AP2.Y - Double(AP1.Y) * AP2.X);
  end;

  function CrossProductSignByIndex(AIndex: Integer): Integer; inline;
  begin
    Result := CrossProductSign(AVector, AShape[AIndex] - ATarget);
  end;

  function CosVector(AIndex: Integer): Double;
  begin
    Result :=
      ScalarProduct(AShape[AIndex] - ATarget, AVector) /
      Sqrt(Double(PointDist(AShape[AIndex], ATarget)) * PointDist(ACenter, ATarget));
  end;

  function LineIntersectsRay(
    const AFrom: TPoint; const ARay: TDoublePoint; const AA, AB: TPoint): TPoint;
  var
    line: TDoublePoint;
    det, t: Double;
  begin
    line := DoublePoint(AB - AA);
    // x = t * ARay.X + AFrom.X; y = t * ARay.Y + AFrom.Y;
    // (x - AA.X) * line.Y = (y - AA.Y) * line.X
    // t * ARay.X * line.Y + (AFrom.X - AA.X) * line.Y =
    // t * ARay.Y * line.X + (AFrom.Y - AA.Y) * line.X
    det := ARay.X * line.Y - ARay.Y * line.X;
    if det = 0 then exit(AB);
    with (AFrom - AA) do // Workaround for issue #17005.
      t := (Y * line.X - X * line.Y) / det;
    if t <= 0 then exit(AB);
    Result := RoundPoint(DoublePoint(t, t) * ARay) + AFrom;
  end;

  procedure PointOnAngle(ADir: Integer; var AIndex: Integer; out APt: TPoint);
  var
    targetCos, c, maxCos: Double;
    this, prev: TPoint;
    ray: TDoublePoint;
    s, n: Integer;
  begin
    targetCos := Cos(AAngle / 2);
    maxCos := 2.0;
    while true do begin
      // Central vector of the callout passes exactly through the shape vertex.
      s := CrossProductSignByIndex(AIndex);
      if s <> 0 then break;
      AIndex := Next(AIndex, ADir);
    end;
    prev := AShape[Next(AIndex, -ADir)];
    while true do begin
      this := AShape[AIndex];
      c := CosVector(AIndex);
      n := Next(AIndex, ADir);
      if
        (CrossProductSignByIndex(AIndex) <> s) or (c > maxCos) and
        // Imprecision of integer grid may result in short concave segments on
        // a convex figure. Skip them by a single-point lookahead.
        ((CrossProductSignByIndex(n) <> s) or (CosVector(n) > maxCos))
      then begin
        APt := prev;
        AIndex := Next(AIndex, -ADir);
        exit;
      end;
      if c <= targetCos then begin
        ray := RotatePoint(DoublePoint(AVector), s * AAngle / 2);
        APt := LineIntersectsRay(ATarget, ray, prev, this);
        exit;
      end;
      AIndex := Next(AIndex, ADir);
      maxCos := c;
      prev := this;
    end;
  end;

var
  cnt: Integer = 0;

  procedure Add(const APoint: TPoint);
  begin
    if (cnt = 0) or (Result[cnt - 1] <> APoint) then begin
      Result[cnt] := APoint;
      cnt += 1;
    end;
  end;

var
  ni, li, ri, i: Integer;
  lp, rp: TPoint;
begin
  if
    (Length(AShape) < 3) or
    IsPointInPolygon(ATarget, AShape) or not IsPointInPolygon(ACenter, AShape)
  then
    exit(AShape);
  ni := NearestSide;
  if ni < 0 then exit(AShape);
  AVector := ACenter - ATarget;
  li := ni;
  PointOnAngle(-1, li, lp);
  ri := Next(ni, 1);
  PointOnAngle(+1, ri, rp);
  SetLength(Result, Length(AShape) + 3);
  i := ri;
  while i <> li do begin
    Add(AShape[i]);
    i := Next(i, 1);
  end;
  Add(AShape[li]);
  Add(lp);
  Add(ATarget);
  Add(rp);
  SetLength(Result, cnt);
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

// Normalizes an angle to be in the interval 0 .. 2 pi
function NormalizeAngle(Angle: Double): Double;
const
  TWO_PI = 2.0 * pi;
begin
  Result := Angle;
  while Result > TWO_PI do Result -= TWO_PI;
  while Result < 0 do Result += TWO_PI;
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

function NextNumberSeq(
  const APoints: array of TDoublePoint; var AStart, AEnd: Integer): Boolean;
begin
  AStart := AEnd + 2;
  while (AStart <= High(APoints)) and IsNan(APoints[AStart]) do
    AStart += 1;
  AEnd := AStart;
  while (AEnd + 1 <= High(APoints)) and not IsNan(APoints[AEnd + 1]) do
    AEnd += 1;
  Result := AStart <= High(APoints);
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

function PointLineDist(const P, A,B: TPoint): Integer;
var
  v, w, Q: TPoint;
  dot: Int64;
  lv: Integer;
begin
  if A = B then
    Result := PointDist(A, P)
  else begin
    v := B - A;                // Vector pointing along line from A to B
    w := P - A;                // Vector pointing from A to P
    dot := Int64(v.x) * w.x + Int64(v.y) * w.y;  // dot product v . w
    lv := PointDist(A, B);     // Length of vector AB
    Q := (v * dot) div lv;     // Projection of P onto line A-B, seen from A
    Result := PointDist(Q, w); // Length from A to Q
  end;
end;

function PointLineDist(const P, A,B: TPoint; out Q: TPoint;
  out Inside: Boolean): Integer;
var
  v, w: TPoint;
  dot: Int64;
  lv: Integer;

  aq, bq: Integer;
begin
  if A = B then begin
    Result := PointDist(A, P);
    Inside := false;
    Q := A;
  end else begin
    v := B - A;
    w := P - A;
    dot := Int64(v.x) * w.x + Int64(v.y) * w.y;
    lv := PointDist(A, B);
    Q := (v * dot) div lv;
    Result := PointDist(Q, w);

    // Check whether the projection point Q is inside the A-B line.
    // In this case the lengths AQ and BQ are shorter than AB.
    aq := sqr(Q.x) + sqr(Q.y);     // note: Q is seen from A, not from origin.
    bq := PointDist(v, Q);
    Inside := (aq <= lv) and (bq <= lv);
    Q := Q + A;
  end;
end;

function ProjToLine(const P, A,B: TDoublePoint): TDoublePoint;
var
  v, s: TDoublePoint;
begin
  if P = A then
    Result := A
  else if P = B then
    Result := B
  else begin
    s := B - A;
    v := P - A;
    Result := A + s * (DotProduct(v, s) / DotProduct(s, s));
  end;
end;

function ProjToLine(const P, A, B: TPoint): TPoint;
begin
  Result := RoundPoint(ProjToLine(DoublePoint(P), DoublePoint(A), DoublePoint(B)));
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

function RotatePointX(AX, AAngle: Double): TPoint;
var
  sa, ca: Extended;
begin
  SinCos(AAngle, sa, ca);
  Result.X := Round(ca * AX);
  Result.Y := Round(sa * AX);
end;

function RoundPoint(APoint: TDoublePoint): TPoint;
begin
  Result.X := Round(APoint.X);
  Result.Y := Round(APoint.Y);
end;

function TesselateRect(const ARect: TRect): TPointArray;
begin
  SetLength(Result, 4);
  with ARect do begin
    Result[0] := TopLeft;
    Result[1] := Point(Left, Bottom);
    Result[2] := BottomRight;
    Result[3] := Point(Right, Top);
  end;
end;

function TesselateEllipse(const ABounds: TRect; AStep: Integer): TPointArray;
var
  e: TEllipse;
  p: TPolygon;
begin
  with ABounds do
    e.InitBoundingBox(Left, Top, Right, Bottom);
  p.Init;
  e.SliceToPolygon(0, 2 * Pi, AStep, p);
  Result := p.Purge;
end;

function TesselateRoundRect(
  const ARect: TRect; ARadius, AStep: Integer): TPointArray;
var
  e: TEllipse;
  p: TPolygon;
begin
  with ARect do begin
    if Min(Right - Left, Bottom - Top) < 2 * ARadius then exit(nil);

    p.Init;
    e.FR := DoublePoint(ARadius, ARadius);

    p.AddNoDup(Point(Right, Bottom - ARadius));
    p.AddNoDup(Point(Right, Top + ARadius));
    e.FC := DoublePoint(Right - ARadius, Top + ARadius);
    e.SliceToPolygon(0, Pi / 2, AStep, p);

    p.AddNoDup(Point(Right - ARadius, Top));
    p.AddNoDup(Point(Left + ARadius, Top));
    e.FC := DoublePoint(Left + ARadius, Top + ARadius);
    e.SliceToPolygon(Pi / 2, Pi / 2, AStep, p);

    p.AddNoDup(Point(Left, Top + ARadius));
    p.AddNoDup(Point(Left, Bottom - ARadius));
    e.FC := DoublePoint(Left + ARadius, Bottom - ARadius);
    e.SliceToPolygon(Pi, Pi / 2, AStep, p);

    p.AddNoDup(Point(Left + ARadius, Bottom));
    p.AddNoDup(Point(Right - ARadius, Bottom));
    e.FC := DoublePoint(Right - ARadius, Bottom - ARadius);
    e.SliceToPolygon(Pi * 3/2, Pi / 2, AStep, p);
  end;

  Result := p.Purge;
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

operator * (const A: TDoublePoint; B: Double): TDoublePoint;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
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

{ TPolygon }

procedure TPolygon.Add(const APoint: TPoint);
begin
  if FCount > High(FPoints) then
    SetLength(FPoints, Max(2 * FCount, 16));
  FPoints[FCount] := APoint;
  FCount += 1;
end;

procedure TPolygon.AddNoDup(const APoint: TPoint);
begin
  if (FCount = 0) or (LastPoint <> APoint) then
    Add(APoint);
end;

constructor TPolygon.Init;
begin
  FCount := 0;
  FPoints := nil;
end;

function TPolygon.LastPoint: TPoint;
begin
  Result := FPoints[FCount - 1];
end;

function TPolygon.Purge: TPointArray;
begin
  SetLength(FPoints, FCount);
  Result := FPoints;
end;

{ TEllipse }

function TEllipse.GetPoint(AParametricAngle: Double): TDoublePoint;
var
  s, c: Extended;
begin
  SinCos(AParametricAngle, s, c);
  Result := DoublePoint(c, -s) * FR + FC;
end;

constructor TEllipse.InitBoundingBox(AX1, AY1, AX2, AY2: Integer);
begin
  FC.X := (AX1 + AX2) / 2;
  FC.Y := (AY1 + AY2) / 2;
  FR.X := Abs(AX1 - AX2) / 2;
  FR.Y := Abs(AY1 - AY2) / 2;
end;

procedure TEllipse.SliceToPolygon(
  AAngleStart, AAngleLength: Double; AStep: Integer; var APoly: TPolygon);
var
  lastAngle: Double;

  procedure SafeAddPoint(APoint: TPoint; AAngle: Double);
  begin
    if APoly.LastPoint <> APoint then begin
      APoly.Add(APoint);
      lastAngle := AAngle;
    end;
  end;

  procedure Rec(ALo, AHi: Double);
  var
    pt: TPoint;
  begin
    pt := RoundPoint(GetPoint(AHi));
    if PointDist(APoly.LastPoint, pt) <= Sqr(AStep) then
      SafeAddPoint(pt, AHi)
    else begin
      Rec(ALo, (ALo + AHi) / 2);
      Rec(lastAngle, AHi)
    end;
  end;

  procedure Add(AAngle: Double);
  begin
    SafeAddPoint(RoundPoint(GetPoint(AAngle)), AAngle)
  end;

const
  HalfPi = Pi / 2;
var
  t, tprev, tlast: Double;
begin
  tprev := AAngleStart;
  tlast := AAngleStart + AAngleLength;
  APoly.Add(RoundPoint(GetPoint(tprev)));
  if (FR.X < 1) or (FR.Y < 1) then begin
    // Ellipse has degenerated into a line.
    Add(tlast);
    exit;
  end;
  APoly.Add(RoundPoint(GetPoint(tprev)));
  lastAngle := tprev;
  t := Ceil(tprev / HalfPi) * HalfPi;
  while t < tlast do begin
    Add(tprev);
    Rec(tprev, t);
    tprev := t;
    t += HalfPi;
  end;
  Rec(tprev, tlast);
  Add(tlast);
end;

// Represent the ellipse sector with a polygon on an integer grid.
// Polygon vertices are no more then AStep pixels apart.
function TEllipse.TesselateRadialPie(
  AAngleStart, AAngleLength: Double; AStep: Integer): TPointArray;
var
  resultPoly: TPolygon;
begin
  resultPoly.Init;
  SliceToPolygon(AAngleStart, AAngleLength, AStep, resultPoly);
  resultPoly.AddNoDup(RoundPoint(FC));
  Result := resultPoly.Purge;
end;

end.

