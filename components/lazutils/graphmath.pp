{
/***************************************************************************
                             GraphMath.pp
                             ------------
         Math helper routines for use within Graphics/Drawing & related
                   Initial Revision  : Wed Aug 07 2002


***************************************************************************/

*****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
*****************************************************************************
}
{
@abstract(A Set of Math Helper routines to simplify Cross-Platfrom Canvas,
etc)
@author(Andrew Johnson <AJ_Genius@Hotmail.com>)
@created(2002)
@lastmod(2002)
}
unit GraphMath;

{$Mode OBJFPC} {$H+}
{$inline on}

interface

Uses
  Types, Classes, SysUtils, Math,
  // LazUtils
  GraphType, LazUtilities;

Type
  TFloatPoint = Record
    X, Y : Extended;
  end;

  TBezier = Array[0..3] of TFloatPoint;

  PPoint = ^TPoint;

procedure Angles2Coords(X,Y, Width, Height : Integer;
  Angle1, Angle2 : Extended; var SX, SY, EX, EY : Integer);

procedure Arc2Bezier(X, Y, Width, Height : Longint; Angle1, Angle2,
  Rotation : Extended; var Points : TBezier);

function Bezier(const C1,C2,C3,C4 : TFloatPoint): TBezier; Overload; inline;
function Bezier(const C1,C2,C3,C4 : TPoint): TBezier; Overload; inline;

procedure Bezier2Polyline(const Bezier : TBezier; var Points : PPoint;
  var Count : Longint);

procedure BezierArcPoints(X, Y, Width, Height : Longint; Angle1, Angle2,
  Rotation : Extended; var Points : PPoint; var Count : Longint);

function BezierMidPoint(const Bezier : TBezier) : TFloatPoint; inline;

procedure CalculateLeftTopWidthHeight(X1,Y1,X2,Y2: integer;
  out Left,Top,Width,Height: integer);

procedure Coords2Angles(X, Y, Width, Height : Integer; SX, SY,
  EX, EY : Integer; var Angle1, Angle2 : Extended);

function Distance(const PT1,Pt2 : TPoint) : Extended; overload; inline;
function Distance(const PT1,Pt2 : TFloatPoint) : Extended; overload; inline;
function Distance(const Pt, SP, EP : TFloatPoint) : Extended; overload;

function EccentricAngle(const PT : TPoint; const Rect : TRect) : Extended;

function EllipseRadialLength(const Rect : TRect; EccentricAngle : Extended) : Longint;
function EllipsePolygon(const aRect: TRect): TPointArray;

function FloatPoint(AX,AY : Extended): TFloatPoint; inline;

function LineEndPoint(const StartPoint : TPoint; Angle, Length : Extended) : TPoint;

procedure MakeMinMax(var i1, i2: integer);

procedure MoveRect(var ARect: TRect; x, y: Integer);
procedure MoveRectToFit(var ARect: TRect; const MaxRect: TRect);
function SameRect(R1, R2: PRect): Boolean;

procedure PolyBezier2Polyline(Beziers: Array of TBezier;
  var Points : PPoint; var Count : Longint); Overload;
procedure PolyBezier2Polyline(Beziers : Array of TPoint;
  var Points : PPoint; var Count : Longint;
  Continuous : Boolean); Overload; inline;
procedure PolyBezier2Polyline(Beziers : PPoint; BCount : Longint;
  var Points : PPoint; var Count : Longint;
  Continuous : Boolean); Overload;

procedure PolyBezierArcPoints(X, Y, Width, Height : Longint; Angle1,
  Angle2, Rotation : Extended; var Points : PPoint; var Count : Longint);

function Quadrant(const PT, Center : TPoint) : Integer;

function RadialPoint(EccentricAngle : Extended; const Rect : TRect) : TPoint;

function RotatePoint(const APoint: TPoint; AAngle: Double): TPoint;
function RotateRect(AWidth, AHeight: Integer; AAngle: Double): TRect;

procedure SplitBezier(const Bezier : TBezier; var Left, Right : TBezier);

Operator + (const Addend1, Addend2 : TFloatPoint) : TFloatPoint; inline;
Operator + (const Addend1 : TFloatPoint; Addend2 : Extended) : TFloatPoint; inline;
Operator + (Addend1 : Extended; const Addend2 : TFloatPoint) : TFloatPoint; inline;
Operator + (const Addend1 : TFloatPoint; const Addend2 : TPoint) : TFloatPoint; inline;
Operator + (const Addend1 : TPoint; const Addend2 : TFloatPoint) : TFloatPoint; inline;

Operator - (const Minuend : TFloatPoint; Subtrahend : Extended) : TFloatPoint; inline;
Operator - (const Minuend, Subtrahend : TFloatPoint) : TFloatPoint; inline;
Operator - (const Minuend : TFloatPoint; const Subtrahend : TPoint) : TFloatPoint; inline;
Operator - (const Minuend : TPoint; const Subtrahend : TFloatPoint) : TFloatPoint; inline;

Operator * (const Multiplicand, Multiplier : TFloatPoint) : TFloatPoint; inline;
Operator * (const Multiplicand : TFloatPoint; Multiplier : Extended) : TFloatPoint; inline;
Operator * (Multiplicand : Extended; const Multiplier : TFloatPoint) : TFloatPoint; inline;
Operator * (const Multiplicand : TFloatPoint; const Multiplier : TPoint) : TFloatPoint; inline;
Operator * (const Multiplicand : TPoint; const Multiplier : TFloatPoint) : TFloatPoint; inline;

Operator / (const Dividend, Divisor : TFloatPoint) : TFloatPoint; inline;
Operator / (const Dividend : TFloatPoint; Divisor : Extended) : TFloatPoint; inline;
Operator / (const Dividend : TFloatPoint; const Divisor : TPoint) : TFloatPoint; inline;
Operator / (const Dividend : TPoint; const Divisor : TFloatPoint) : TFloatPoint; inline;

Operator = (const Compare1, Compare2 : TPoint) : Boolean; inline;
Operator = (const Compare1, Compare2 : TFloatPoint) : Boolean; inline;

Operator := (const Value : TFloatPoint) : TPoint; inline;

Operator := (const Value : TPoint) : TFloatPoint; inline;

Operator = (const Compare1, Compare2  : TRect) : Boolean;


implementation


operator+(const Addend1, Addend2: TFloatPoint): TFloatPoint;
Begin
  With Result do begin
    X := Addend1.X + Addend2.X;
    Y := Addend1.Y + Addend2.Y;
  end;
end;

operator+(const Addend1: TFloatPoint; Addend2: Extended): TFloatPoint;
Begin
  With Result do begin
    X := Addend1.X + Addend2;
    Y := Addend1.Y + Addend2;
  end;
end;

operator+(Addend1: Extended; const Addend2: TFloatPoint): TFloatPoint;
begin
  Result := Addend2 + Addend1;
end;

operator+(const Addend1: TFloatPoint; const Addend2: TPoint): TFloatPoint;
Begin
  With Result do begin
    X := Addend1.X + Addend2.X;
    Y := Addend1.Y + Addend2.Y;
  end;
end;

operator+(const Addend1: TPoint; const Addend2: TFloatPoint): TFloatPoint;
begin
  Result := Addend2 + Addend1;
end;

operator-(const Minuend, Subtrahend: TFloatPoint): TFloatPoint;
Begin
  With Result do begin
    X := Minuend.X - Subtrahend.X;
    Y := Minuend.Y - Subtrahend.Y;
  end;
end;

operator-(const Minuend: TFloatPoint; Subtrahend: Extended): TFloatPoint;
Begin
  With Result do begin
    X := Minuend.X - Subtrahend;
    Y := Minuend.Y - Subtrahend;
  end;
end;

operator-(const Minuend: TFloatPoint; const Subtrahend: TPoint): TFloatPoint;
begin
  With Result do begin
    X := Minuend.X - Subtrahend.X;
    Y := Minuend.Y - Subtrahend.Y;
  end;
end;

operator-(const Minuend: TPoint; const Subtrahend: TFloatPoint): TFloatPoint;
begin
  With Result do begin
    X := Minuend.X - Subtrahend.X;
    Y := Minuend.Y - Subtrahend.Y;
  end;
end;

operator*(const Multiplicand, Multiplier: TFloatPoint): TFloatPoint;
Begin
  With Result do begin
    X := Multiplicand.X * Multiplier.X;
    Y := Multiplicand.Y * Multiplier.Y;
  end;
end;

operator*(const Multiplicand: TFloatPoint; Multiplier: Extended): TFloatPoint;
Begin
  With Result do begin
    X := Multiplicand.X * Multiplier;
    Y := Multiplicand.Y * Multiplier;
  end;
end;

operator*(Multiplicand: Extended; const Multiplier: TFloatPoint): TFloatPoint;
Begin
  Result := Multiplier*Multiplicand;
end;

operator*(const Multiplicand: TFloatPoint; const Multiplier: TPoint
  ): TFloatPoint;
begin
  With Result do begin
    X := Multiplicand.X * Multiplier.X;
    Y := Multiplicand.Y * Multiplier.Y;
  end;
end;

operator*(const Multiplicand: TPoint; const Multiplier: TFloatPoint
  ): TFloatPoint;
begin
  Result := Multiplier*Multiplicand;
end;

operator/(const Dividend, Divisor: TFloatPoint): TFloatPoint;
Begin
  With Result do begin
    X := Dividend.X / Divisor.X;
    Y := Dividend.Y / Divisor.Y;
  end;
end;

operator/(const Dividend: TFloatPoint; Divisor: Extended): TFloatPoint;
begin
  With Result do begin
    X := Dividend.X / Divisor;
    Y := Dividend.Y / Divisor;
  end;
end;

operator/(const Dividend: TFloatPoint; const Divisor: TPoint): TFloatPoint;
begin
  With Result do begin
    X := Dividend.X / Divisor.X;
    Y := Dividend.Y / Divisor.Y;
  end;
end;

operator/(const Dividend: TPoint; const Divisor: TFloatPoint): TFloatPoint;
begin
  With Result do begin
    X := Dividend.X / Divisor.X;
    Y := Dividend.Y / Divisor.Y;
  end;
end;

operator=(const Compare1, Compare2: TPoint): Boolean;
begin
  Result := (Compare1.X = Compare2.X) and (Compare1.Y = Compare2.Y);
end;

operator=(const Compare1, Compare2: TFloatPoint): Boolean;
begin
  Result := (Compare1.X = Compare2.X) and (Compare1.Y = Compare2.Y);
end;

operator:=(const Value: TFloatPoint): TPoint;
begin
  Result.X := Trunc(SimpleRoundTo(Value.X, 0));
  Result.Y := Trunc(SimpleRoundTo(Value.Y, 0));
end;

operator:=(const Value: TPoint): TFloatPoint;
begin
  With Result do begin
    X := Value.X;
    Y := Value.Y;
  end;
end;

operator=(const Compare1, Compare2: TRect): Boolean;
begin
  Result := (Compare1.Left = Compare2.Left) and
            (Compare1.Top = Compare2.Top) and
            (Compare1.Right = Compare2.Right) and
            (Compare1.Bottom = Compare2.Bottom);
end;

{------------------------------------------------------------------------------
  Method:   Angles2Coords
  Params:   x,y,width,height,angle1,angle2, sx, sy, ex, ey
  Returns:  Nothing

  Use Angles2Coords to convert an Eccentric(aka Radial) Angle and an
  Angle-Length, such as are used in X-Windows and GTK, into the coords,
  for Start and End Radial-Points, such as are used in the Windows API Arc
  Pie and Chord routines. The angles are 1/16th of a degree. For example, a
  full circle equals 5760 (16*360). Positive values of Angle and AngleLength
  mean counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position.

------------------------------------------------------------------------------}
procedure Angles2Coords(X, Y, Width, Height : Integer;
  Angle1, Angle2 : Extended; var SX, SY, EX, EY : Integer);
var
  aRect : TRect;
  SP, EP : TPoint;
begin
  aRect := Rect(X,Y,X + Width,Y + Height);
  SP := RadialPoint(Angle1 , aRect);
  If Angle2 + Angle1 > 360*16 then
    Angle2 := (Angle2 + Angle1) - 360*16
  else
    Angle2 := Angle2 + Angle1;
  EP := RadialPoint(Angle2, aRect);
  SX := SP.X;
  SY := SP.Y;
  EX := EP.X;
  EY := EP.Y;
end;

{------------------------------------------------------------------------------
  Method:   Arc2Bezier
  Params:   X, Y, Width, Height, Angle1, Angle2, Rotation, Points, Count
  Returns:  Nothing

  Use Arc2Bezier to convert an elliptical arc into a Bezier approximation
  of the arc.

  The arc is defined by a rectangle which bounds the full ellipse. The
  rectangle has the specified Width and Height, its top/left corner is at the
  point X,Y. Start of the arc is given by the eccentric Angle1, and its length
  is given by eccentric Angle2. Both angles are expressed in 1/16th of a degree,
  a full circle, for example, equals to 5760 (16*360).
  A positive value of Angle2 means counter-clockwise while a negative value
  means clockwise direction. Zero degrees is at the 3 o'clock position.

  The Rotation parameter accepts an angle for a rotated ellipse - for a
  non-rotated ellipse this value would be 0, or 360*16.

  If the angle length is greater than 90 degrees or is equal to 0, the procedure
  automatically exits, as Bezier cannot accurately approximate any angle greater
  then 90 degrees, and in fact for best results no angle greater than 45 should
  be converted, instead an array of Beziers should be created, each Bezier
  describing a portion of the total arc no greater than 45 degrees.

------------------------------------------------------------------------------}
procedure Arc2Bezier(X, Y, Width, Height : Longint; Angle1, Angle2,
  Rotation : Extended; var Points : TBezier);

  function Rotate(Point : TFloatPoint; Rotation : Extended) : TFloatPoint;
  var
    SinA,CosA : Extended;
  begin
    SinCos(Rotation, SinA, CosA);
    Result.X := Point.X*CosA + Point.Y*SinA;
    Result.Y := Point.X*SinA - Point.Y*CosA;
  end;

  function Scale(Point : TFloatPoint; ScaleX, ScaleY : Extended) : TFloatPoint;
  begin
    Result := Point*FloatPoint(ScaleX,ScaleY);
  end;

var
  Beta : Extended;
  P : array[0..3] of TFLoatPoint;
  SinA,CosA : Extended;
  SinA2o2,CosA2o2: Extended;
  A,B : Extended;
  I : Longint;
  PT : TFloatPoint;
  ScaleX, ScaleY : Extended;
begin
  If ABS(Angle2) > 90*16 then
    exit;
  If Angle2 = 0 then
    exit;

  B := Extended(Height) / 2;
  A := Extended(Width) / 2;

  If (A <> B) and (A <> 0) and (B <> 0) then begin
    If A > B then begin
      ScaleX := Extended(Width) / Height;
      ScaleY := 1;
      A := B;
    end
    else begin
      ScaleX := 1;
      ScaleY := Extended(Height) / Width;
      B := A;
    end;
  end
  else begin
    ScaleX := 1;
    ScaleY := 1;
  end;

  Angle1 := DegToRad(Angle1/16);
  Angle2 := DegToRad(Angle2/16);
  Rotation := -DegToRad(Rotation/16);
  SinCos(Angle2/2, SinA2o2, CosA2o2);
  Beta := (4/3)*(1 - CosA2o2)/SinA2o2;
  PT.X := X + Width / 2;
  PT.Y := Y + Height / 2;

  SinCos(Angle1, SinA, CosA);

  P[0].X := A *CosA;
  P[0].Y := B *SinA;
  P[1].X := P[0].X - Beta * A * SinA;
  P[1].Y := P[0].Y + Beta * B * CosA;

  SinCos(Angle1 + Angle2, SinA, CosA);

  P[3].X := A *CosA;
  P[3].Y := B *SinA;
  P[2].X := P[3].X + Beta * A * SinA;
  P[2].Y := P[3].Y - Beta * B * CosA;

  For I := 0 to 3 do
  begin
    Points[I] := Scale(P[I],ScaleX, ScaleY); //Scale to proper size
    Points[I] := Rotate(Points[I], Rotation); //Rotate Counter-Clockwise
    Points[I] := Points[I] + PT; //Translate to Center
  end;
end;

{------------------------------------------------------------------------------
  Method:   Bezier
  Params:   C1,C2,C3,C4
  Returns:  TBezier

  Use Bezier to get a TBezier. It is Primarily for use with and in Bezier
  routines.

------------------------------------------------------------------------------}
function Bezier(const C1,C2,C3,C4 : TFloatPoint): TBezier;
begin
  Result[0] := C1;
  Result[1] := C2;
  Result[2] := C3;
  Result[3] := C4;
end;

{------------------------------------------------------------------------------
  Method:   Bezier
  Params:   C1,C2,C3,C4
  Returns:  TBezier

  Use Bezier to get a TBezier. It is Primarily for use with and in Bezier
  routines.

------------------------------------------------------------------------------}
function Bezier(const C1,C2,C3,C4 : TPoint): TBezier;
begin
  Result[0] := FloatPoint(C1.X,C1.Y);
  Result[1] := FloatPoint(C2.X,C2.Y);
  Result[2] := FloatPoint(C3.X,C3.Y);
  Result[3] := FloatPoint(C4.X,C4.Y);
end;

{------------------------------------------------------------------------------
  Method:   Bezier2Polyline
  Params:   Bezier, Points, Count
  Returns:  Nothing

  Use BezierToPolyline to convert a 4-Point Bezier into a Pointer Array of
  TPoint and a Count variable which can then be used within either a Polyline,
  or Polygon routine. It is primarily for use within PolyBezier2Polyline. If
  Points is not initialized or Count is less then 0, it is set to nil and
  the array starts at 0, otherwise it tries to append points
  to the array starting at Count. Points should ALWAYS be Freed when done
  by calling to ReallocMem(Points, 0) or FreeMem.

------------------------------------------------------------------------------}
procedure Bezier2Polyline(const Bezier : TBezier; var Points : PPoint;
  var Count : Longint);
var
  Pt : TPoint;

  procedure AddPoint(const Point : TFloatPoint);
  var
    P : TPoint;
  begin
    P := Point;
    if (Pt <> P) then
    begin
      Inc(Count);
      ReallocMem(Points, SizeOf(TPoint) * Count);
      Points[Count - 1] := P;
      Pt := P;
    end;
  end;

  function Colinear(BP : TBezier; Tolerance : Extended) : Boolean;
  var
    D : Extended;
  begin
    D := SQR(Distance(BP[1], BP[0], BP[3]));
    Result := D < Tolerance;
    D := SQR(Distance(BP[2], BP[0], BP[3]));
    If Result then
      Result := Result and (D < Tolerance);
  end;

  procedure SplitRecursive(B : TBezier);
  var
    Left,
    Right : TBezier;
  begin
    If Colinear(B, 1) then begin
      AddPoint(B[0]);
      AddPoint(B[3]);
    end
    else begin
      SplitBezier(B,left{%H-},right{%H-});
      SplitRecursive(left);
      SplitRecursive(right);
    end;
  end;

begin
  Pt := Point(-1,-1);
  If (not Assigned(Points)) or (Count <= 0) then
  begin
    Count := 0;
    if Assigned(Points) then
      ReallocMem(Points, 0);
  end;
  SplitRecursive(Bezier);
end;

{------------------------------------------------------------------------------
  Method:   BezierArcPoints
  Params:   X, Y, Width, Height, Angle1, Angle2, Rotation, Points, Count
  Returns:  Nothing

  Use BezierArcPoints to convert an elliptical arc to a pointer array of
  TPoints for use with Polyline or Polygon.

  The arc is defined by a rectangle which bounds the full ellipse. The
  rectangle has the specified Width and Height, its top/left corner is at the
  point X,Y. Start of the arc is given by the eccentric Angle1, and its length
  is given by eccentric Angle2. Both angles are expressed in 1/16th of a degree,
  a full circle, for example, equals to 5760 (16*360)
  A positive value of Angle2 means counter-clockwise while a negative value
  means clockwise direction. Zero degrees is at the 3 o'clock position.

  The Rotation parameter accepts a rotation angle for a rotated ellipse - for
  a non-rotated ellipse this value would be 0, or 360*16.

  The result is an approximation based on one or more Beziers. If the angle length
  is greater than 90 degrees, the procedure calls PolyBezierArcPoints, otherwise
  it converts the angles into a Bezier by calling Arc2Bezier which then is
  converted to an array of points by calling Bezier2PolyLine.

  If Points is not initialized or Count is less then 0, it is set to nil and
  the array starts at 0, otherwise it tries to append points to the array
  starting at Count. Points should ALWAYS be free'd when done by calling
  ReallocMem(Points, 0) or FreeMem.

------------------------------------------------------------------------------}
procedure BezierArcPoints(X, Y, Width, Height : Longint; Angle1, Angle2,
  Rotation : Extended; var Points : PPoint; var Count : Longint);
var
  B : TBezier;
begin
  If ABS(Angle2) > 90*16 then begin
    PolyBezierArcPoints(X, Y, Width, Height, Angle1, Angle2, Rotation, Points,
                        Count);
    Exit;
  end;
  If Angle2 = 0 then
    exit;

  If (not Assigned(Points)) or (Count <= 0) then
  begin
    Count := 0;
    if Assigned(Points) then
      ReallocMem(Points, 0);
  end;

  Arc2Bezier(X, Y, Width, Height, Angle1, Angle2, Rotation, B{%H-});
  Bezier2Polyline(B,Points,Count);
end;

{------------------------------------------------------------------------------
  Method:   BezierMidPoint
  Params:   Bezier
  Returns:  TFloatPoint

  Use BezierMidPoint to get the Mid-Point of any 4-Point Bezier. It is
  primarily for use in SplitBezier.

------------------------------------------------------------------------------}
function BezierMidPoint(const Bezier : TBezier) : TFloatPoint;
begin
  Result := (Bezier[0] + 3*Bezier[1] + 3*Bezier[2] + Bezier[3]) / 8;
end;

procedure CalculateLeftTopWidthHeight(X1, Y1, X2, Y2: integer;
  out Left, Top, Width, Height: integer);
begin
  if X1 <= X2 then
   begin
    Left := X1;
    Width := X2 - X1;
  end
  else
  begin
    Left := X2;
    Width := X1 - X2;
  end;
  if Y1 <= Y2 then
  begin
    Top := Y1;
    Height := Y2 - Y1;
  end
  else
  begin
    Top := Y2;
    Height := Y1 - Y2;
  end;
end;

{------------------------------------------------------------------------------
  Method:   Coords2Angles
  Params:   x,y,width,height,sx,sy,ex,ey, angle1,angle2
  Returns:  Nothing

  Use Coords2Angles to convert the coords for Start and End Radial-Points, such
  as are used in the Windows API Arc Pie and Chord routines, into an Eccentric
  (aka Radial) counter clockwise Angle and an Angle-Length, such as are used in
  X-Windows and GTK. The angles angle1 and angle2 are returned in 1/16th of a
  degree. For example, a full circle equals 5760 (16*360). Zero degrees is at
  the 3'o clock position.

------------------------------------------------------------------------------}
procedure Coords2Angles(X, Y, Width, Height : Integer; SX, SY,
  EX, EY : Integer; var Angle1, Angle2 : Extended);
var
  aRect : TRect;
  SP,EP : TPoint;
begin
  aRect := Rect(X,Y,X + Width,Y + Height);
  SP := Point(SX,SY);
  EP := Point(EX,EY);
  Angle1 := EccentricAngle(SP, aRect);
  Angle2 := EccentricAngle(EP, aRect);
  If Angle2 < Angle1 then
    Angle2 := 360*16 - (Angle1 - Angle2)
  else
    Angle2 := Angle2 - Angle1;
end;

{------------------------------------------------------------------------------
  Method:   Distance
  Params:   PT1, PT2: TPoint
  Returns:  Extended

  Use Distance to get the distance between any two Points. It is primarily
  for use in other routines such as EccentricAngle.

------------------------------------------------------------------------------}
function Distance(const PT1, Pt2: TPoint): Extended;
begin
  Result := Sqrt(Sqr(Pt2.X - Pt1.X) + Sqr(Pt2.Y - Pt1.Y));
end;

{------------------------------------------------------------------------------
  Method:   Distance
  Params:   PT1, PT2: TFloatPoint
  Returns:  Extended

  Use Distance to get the distance between any two Points. It is primarily
  for use in other routines such as EccentricAngle.

------------------------------------------------------------------------------}
function Distance(const PT1, Pt2: TFloatPoint): Extended;
begin
  Result := Sqrt(Sqr(Pt2.X - Pt1.X) + Sqr(Pt2.Y - Pt1.Y));
end;

{------------------------------------------------------------------------------
  Method:   Distance
  Params:   PT, SP,EP
  Returns:  Extended

  Use Distance to get the distance between any point(PT) and a line defined
  by any two points(SP, EP). Intended for use in Bezier2Polyline, so params
  are TFloatPoint's, NOT TPoint's.

------------------------------------------------------------------------------}
function Distance(const Pt, SP, EP : TFloatPoint) : Extended;
var
  A, B, C : Extended;

  function Slope(PT1,Pt2 : TFloatPoint) : Extended;
  begin
    // The case Pt1.X = Pt2.X has already been handled.
    Result := (Pt2.Y - Pt1.Y) / (Pt2.X - Pt1.X);
  end;

  function YIntercept(Pt1: TFloatPoint; ASlope: Extended) : Extended;
  begin
    Result := Pt1.Y - ASlope*Pt1.X;
  end;

begin
  if SP.X = EP.X then
  begin
    if SP.Y <> EP.Y then
      Result := abs(Pt.X - SP.X)
    else
      Result := Distance(Pt, SP);
    exit;
  end;

  A := -Slope(SP,EP);
  B := 1;
  C := -YIntercept(SP, -A);
  Result := ABS(A*Pt.X + B*Pt.Y + C)/Sqrt(Sqr(A) + Sqr(B));
end;

{------------------------------------------------------------------------------
  Method:   EccentricAngle
  Params:   Pt, Rect
  Returns:  Extended

  Use EccentricAngle to get the Eccentric( aka Radial ) Angle of a given
  point on any non-rotated ellipse. It is primarily for use in Coords2Angles.
  The result is in 1/16th of a degree. For example, a full circle equals
  5760 (16*360).  Zero degrees is at the 3'o clock position.

------------------------------------------------------------------------------}
function EccentricAngle(const PT : TPoint; const Rect : TRect) : Extended;
var
  CenterPt : TPoint;
  Quad : Integer;
  Theta : Extended;
begin
  CenterPt := CenterPoint(Rect);
  Quad := Quadrant(Pt,CenterPt);
  Theta := -1;
  Case Quad of
    1..4:
      begin
        Theta := Distance(CenterPt,Pt);
        If Theta > 0 then
          Theta := RadToDeg(ArcSin(ABS(PT.Y - CenterPt.Y) / Theta));
      end;
  end;
  Case Quad of
    0:{ 0, 0}
      Theta := -1;
    1:{ X, Y}
      Theta := Theta;
    2:{-X, Y}
      Theta := 180 - Theta;
    3:{-X,-Y}
      Theta := 180 + Theta;
    4:{ X,-Y}
      Theta := 360 - Theta;
    5:{ 0, Y}
      Theta := 90;
    6:{ X, 0}
      Theta := 0;
    7:{ 0,-Y}
      Theta := 270;
    8:{-X, 0}
      Theta := 180;
  end;
  Result := Theta*16;
end;

{------------------------------------------------------------------------------
  Method:   EllipseRadialLength
  Params:   Rect, EccentricAngle
  Returns:  Longint

  Use EllipseRadialLength to get the Radial-Length of non-rotated ellipse at
  any given Eccentric( aka Radial ) Angle. It is primarily for use in other
  routines such as RadialPoint. The Eccentric angle is in 1/16th of a degree.
  For example, a full circle equals 5760 (16*360).  Zero degrees is at the
  3'o clock position.

------------------------------------------------------------------------------}
function EllipseRadialLength(const Rect : TRect; EccentricAngle : Extended) : Longint;
var
  a, b, R : Extended;
  sinAngle, cosAngle: Extended;
begin
  a := (Rect.Right - Rect.Left) div 2;
  b := (Rect.Bottom - Rect.Top) div 2;
  R := Sqr(a)*Sqr(b);
  if R <> 0 then
  begin
    SinCos(DegToRad(EccentricAngle/16), sinAngle, cosAngle);
    R := Sqrt(R / (sqr(b*cosAngle) + sqr(a*sinAngle)));
  end;
  Result := TruncToInt(R);
end;

function EllipsePolygon(const aRect: TRect): TPointArray;
{ Our Ellipse is axis-aligned, so it's parametrization is:

  X(t) = Xc + a * cos(t)
  Y(t) = Yc + b * sin(t)

  (Xc,Yc) is the center of the ellipse }
var
  n_points, i, n4, aLeft, aRight, aTop, aBottom: Integer;
  Xc, Yc, a, b, MaxR, t: single;
  SinT, CosT, AX, AY, BX, BY, CX, CY, Deviation: single;
begin
  Xc:=single(aRect.Left+aRect.Right)/2;
  Yc:=single(aRect.Top+aRect.Bottom)/2;
  a:=single(aRect.Width)/2;
  b:=single(aRect.Height)/2;
  MaxR:=Max(a,b);

  // Choose the minimum number of points, so that the error - the distance
  // between the edges and the mathematical circle is less than the rounding
  // error (0.5), for a smoother step 0.4.
  n_points:=0;
  repeat
    inc(n_points,4);
    t := single(1) / single(n_points) * 2 * Pi;
    SinCos(t,SinT,CosT);
    AX := MaxR * CosT;
    AY := MaxR * SinT;
    SinCos(t/2,SinT,CosT);
    BX := MaxR * CosT;
    BY := MaxR * SinT;
    CX := (AX + MaxR) /2;
    CY := (AY + 0) /2;
    Deviation := sqrt(sqr(BX-CX)+sqr(BY-CY));
  until Deviation<0.4;
  SetLength(Result{%H-}, n_points);

  // And fill them iterating through the ellipse
  // by computing the upper right quarter and mirror the other three quartes.
  // This way a perfectly symmetrical ellipse is created.
  n4 := n_points div 4;
  for i := 0 to n4 do
  begin
    t := single(i) / single(n_points) * 2 * Pi;
    SinCos(t,SinT,CosT);
    aRight := Round(Xc + a * CosT);
    aLeft := Trunc(2*Xc - aRight);
    aTop := Round(Yc + b * SinT);
    aBottom := Trunc(2*Yc - aTop);
    Result[i].X := aRight;
    Result[i].Y := aTop;
    if i>0 then
    begin
      Result[n_points - i].X := aRight;
      Result[n_points - i].Y := aBottom;
    end;
    if i<n4 then
    begin
      Result[2*n4 - i].X := aLeft;
      Result[2*n4 - i].Y := aTop;
      if i>0 then
      begin
        Result[2*n4 + i].X := aLeft;
        Result[2*n4 + i].Y := aBottom;
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:   FloatPoint
  Params:   AX, AY
  Returns:  TFloatPoint

  Use FloatPoint to get a TFloatPoint. It is essentialy like Classes. Point in
  use, except that it excepts Extended Parameters. It is Primarily for use with
  and in Bezier routines.

------------------------------------------------------------------------------}
function FloatPoint(AX,AY : Extended): TFloatPoint;
begin
  With Result do begin
    X := AX;
    Y := AY;
  end;
end;

{------------------------------------------------------------------------------
  Method:   LineEndPoint
  Params:   StartPoint, Angle, Length
  Returns:  TPoint

  Use LineEndPoint to get the End-Point of a line of any given Length at
  any given angle with any given Start-Point. It is primarily for use in
  other routines such as RadialPoint. The angle is in 1/16th of a degree.
  For example, a full circle equals 5760 (16*360).  Zero degrees is at the
  3'o clock position.

------------------------------------------------------------------------------}
function LineEndPoint(const StartPoint : TPoint; Angle, Length : Extended) : TPoint;
var
  sinAngle, cosAngle: Extended;
begin
  SinCos(DegToRad(Angle/16), sinAngle, cosAngle);
  Result.Y := StartPoint.Y - Round(Length*sinAngle);
  Result.X := StartPoint.X + Round(Length*cosAngle);
end;

procedure MakeMinMax(var i1, i2: integer);
var
  h: Integer;
begin
  if i1>i2 then begin
    h:=i1;
    i1:=i2;
    i2:=h;
  end;
end;

procedure MoveRect(var ARect: TRect; x, y: Integer);
begin
  inc(ARect.Right,x-ARect.Left);
  inc(ARect.Bottom,y-ARect.Top);
  ARect.Left:=x;
  ARect.Top:=y;
end;

procedure MoveRectToFit(var ARect: TRect; const MaxRect: TRect);
// move ARect, so it fits into MaxRect
// if MaxRect is too small, ARect is resized.
begin
  if ARect.Left<MaxRect.Left then begin
    // move rectangle right
    ARect.Right:=Min(ARect.Right+MaxRect.Left-ARect.Left,MaxRect.Right);
    ARect.Left:=MaxRect.Left;
  end;
  if ARect.Top<MaxRect.Top then begin
    // move rectangle down
    ARect.Bottom:=Min(ARect.Bottom+MaxRect.Top-ARect.Top,MaxRect.Bottom);
    ARect.Top:=MaxRect.Top;
  end;
  if ARect.Right>MaxRect.Right then begin
    // move rectangle left
    ARect.Left:=Max(ARect.Left-ARect.Right+MaxRect.Right,MaxRect.Left);
    ARect.Right:=MaxRect.Right;
  end;
  if ARect.Bottom>MaxRect.Bottom then begin
    // move rectangle left
    ARect.Top:=Max(ARect.Top-ARect.Bottom+MaxRect.Bottom,MaxRect.Top);
    ARect.Bottom:=MaxRect.Bottom;
  end;
end;

function SameRect(R1, R2: PRect): Boolean;
begin
  Result:=(R1^.Left=R2^.Left) and (R1^.Top=R2^.Top) and
          (R1^.Bottom=R2^.Bottom) and (R1^.Right=R2^.Right);
  {if not Result then begin
    DebugLn(' DIFFER: ',R1^.Left,',',R1^.Top,',',R1^.Right,',',R1^.Bottom
      ,' <> ',R2^.Left,',',R2^.Top,',',R2^.Right,',',R2^.Bottom);
  end;}
end;

{------------------------------------------------------------------------------
  Method:   PolyBezier2Polyline
  Params:   Beziers, Points, Count
  Returns:  Nothing

  Use BezierToPolyline to convert an array of 4-Point Bezier into a Pointer
  Array of TPoint and a Count variable which can then be used within either a
  Polyline, or Polygon routine. Points is automatically initialized, so any
  existing information is lost, and the array starts at 0. Points should ALWAYS
  be Freed when done by calling to ReallocMem(Points, 0).

------------------------------------------------------------------------------}
procedure PolyBezier2Polyline(Beziers: array of TBezier; var Points: PPoint;
  var Count: Longint);
var
  I : Integer;
begin
  If (High(Beziers) < 1) then
    exit;
  Count := 0;
  If Assigned(Points) then
    Try
      ReallocMem(Points, 0)
    Finally
      Points := nil;
    end;
  For I := 0 to High(Beziers) - 1 do
    Bezier2PolyLine(Beziers[I], Points, Count);
end;

{------------------------------------------------------------------------------
  Method:   PolyBezier2Polyline
  Params:   Beziers, Points, Count, Continuous
  Returns:  Nothing

  Use BezierToPolyline to convert an array of TPoints descibing 1 or more
  4-Point Beziers into a Pointer Array of TPoint and a Count variable which
  can then be used within either a Polyline, or Polygon routine. If Continuous
  is set to true then the first point of each Bezier is the last point of
  the preceding Bezier, so every bezier must have 3 described points, in
  addition to the initial Starting Point; otherwise each Bezier must have 4
  points. If there are an uneven number of points then the last set of points
  is ignored. Points is automatically initialized, so any existing information
  is lost, and the array starts at 0. Points should ALWAYS be Freed when done
  by calling to ReallocMem(Points, 0).

------------------------------------------------------------------------------}
procedure PolyBezier2Polyline(Beziers: array of TPoint; var Points: PPoint;
  var Count: Longint; Continuous: Boolean);
begin
  PolyBezier2Polyline(@Beziers[0],High(Beziers) + 1, Points, Count, Continuous);
end;

procedure PolyBezier2Polyline(Beziers : PPoint; BCount : Longint;
  var Points : PPoint; var Count : Longint; Continuous : Boolean);
var
  I : Integer;
  NB : Longint;
begin
  If BCount < 4 then
    exit;
  Count := 0;
  If Assigned(Points) then
    Try
      ReallocMem(Points, 0)
    Finally
      Points := nil;
    end;
  If Not Continuous then begin
    NB := BCount;
    NB := NB div 4;
    For I := 0 to NB - 1 do
      Bezier2PolyLine(Bezier(Beziers[I*4],Beziers[I*4+1],
        Beziers[I*4+2],Beziers[I*4+3]), Points, Count);
  end
  else begin
    NB := BCount - 1;
    NB := NB div 3;
    For I := 0 to NB-1 do
      Bezier2PolyLine(Bezier(Beziers[(I - 1)*3 + 3],Beziers[I*3 + 1],
        Beziers[I*3+2],Beziers[I*3+3]), Points, Count);
  end;
end;

{------------------------------------------------------------------------------
  Method:   PolyBezierArcPoints
  Params:   X, Y, Width, Height, Angle1, Angle2, Rotation, Points, Count
  Returns:  Nothing

  Use PolyBezierArcPoints to convert an elliptical arc to a pointer array of
  TPoints for use with Polyline or Polygon.

  The arc is defined by a rectangle which bounds the full ellipse. The
  rectangle has the specified Width and Height, its top/left corner is at the
  point X,Y. Start of the arc is given by the eccentric angle Angle1, and its
  length is given by Angle2. Both angles are expressed in 1/16th of a degree,
  a full circle, for example, equals to 5760 (16*360)
  A positive value of Angle2 means counter-clockwise while a negative value
  means clockwise direction. Zero degrees is at the 3 o'clock position.

  The Rotation parameter accepts a angle for a rotated ellipse - for a
  non-rotated ellipse this value would be 0, or 360*16.

  The result is an approximation based on 1 or more Beziers. If the angle length
  is greater than 45*16 (45 degrees), it recursively breaks the arc into arcs of
  45 degrees or less, and converts them into beziers with BezierArcPoints.

  Points is automatically initialized, so that any existing information is lost,
  and the array starts at 0. Points should ALWAYS be freed when done by calling
  ReallocMem(Points, 0).

------------------------------------------------------------------------------}
procedure PolyBezierArcPoints(X, Y, Width, Height : Longint; Angle1, Angle2,
  Rotation : Extended; var Points : PPoint; var Count : Longint);
var
  I,K : Integer;
  FullAngle : Extended;
  TST : Boolean;
begin
  If Abs(Angle2) > 360*16 then begin
    Angle2 := 360*16;
    Angle1 := 0;
  end;
  FullAngle := Angle1 + Angle2;
  K := Ceil(ABS(Angle2/16) / 45);
  Count := 0;
  If Assigned(Points) then
    Try
      ReallocMem(Points, 0)
    Finally
      Points := nil;
    end;
  If Angle2 > 45*16 then
    Angle2 := 45*16
  else
    If Angle2 < -45*16 then
      Angle2 := -45*16;
  For I := 0 to K - 1 do begin
    BezierArcPoints(X, Y, Width,Height,Angle1,Angle2,Rotation,Points,Count);
    Angle1 := Angle1 + Angle2;
    If Angle2 > 0 then
      TST := (FullAngle - Angle1) > 45*16
    else
      TST := ABS(FullAngle - Angle1) > 45*16;
    If TST then begin
      If Angle2 > 0 then
        Angle2 := 45*16
      else
        Angle2 := -45*16;
    end
    else begin
      {If Angle2 > 0 then}
        Angle2 := FullAngle - Angle1
      {else
        Angle2 := -(FullAngle - Angle1);
        - Wrong: This gives the wrong sign to Angle2 - G. Colla
        }
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:   Quadrant
  Params:   PT, Center
  Returns:  Integer

  Use Quadrant to determine the Quadrant of any point, given the Center.
  It is primarily for use in other routines such as EccentricAngle. A result
  of 1-4 represents the primary 4 quardants. A result of 5-8 means the point
  lies on one of the Axis', 5 = -Y Axis, 6 = +X Axis, 7 = +Y Axis, and
  8 = -X Axis. A result of -1 means that it does not fall in any quadrant,
  that is, it is the Center.

------------------------------------------------------------------------------}
function Quadrant(const PT, Center: TPoint): Integer;
var
  X,Y,CX,CY : Longint;
begin
  X  := Pt.X;
  Y  := Pt.Y;
  CX := Center.X;
  CY := Center.Y;
  Result := -1;
  If (Y < CY) then begin
    If (X > CX) then begin
      Result := 1;
    end
    else
      If (X < CX) then begin
        Result := 2;
      end
    else begin
      Result := 5;
    end;
  end
  else
    If (Y > CY) then begin
      If (X < CX) then begin
        Result := 3;
      end
      else
        If (X > CX) then begin
          Result := 4;
        end
      else begin
        Result := 7;
      end;
    end
  else
    If (Y = CY) then begin
      If (X > CX) then begin
        Result := 6;
      end
      else
        If (X < CX) then begin
          Result := 8;
        end;
    end;
end;

{------------------------------------------------------------------------------
  Method:   RadialPointAngle
  Params:   EccentricAngle, Rect
  Returns:  TPoint

  Use RadialPoint to get the Radial-Point at any given Eccentric( aka Radial )
  angle on any non-rotated ellipse. It is primarily for use in Angles2Coords.
  The EccentricAngle is in 1/16th of a degree. For example, a full circle
  equals 5760 (16*360).  Zero degrees is at the 3'o clock position.

------------------------------------------------------------------------------}
function RadialPoint(EccentricAngle : Extended; const Rect : TRect) : TPoint;
var
  R : Longint;
Begin
  R := EllipseRadialLength(Rect,EccentricAngle);
  Result := LineEndPoint(CenterPoint(Rect), EccentricAngle, R);
end;

{-------------------------------------------------------------------------------
  Method:  RotatePoint
  Params:  APoint, AAngle
  Returns: TPoint after rotation
  
  Rotates a point around the origin (0,0) by the angle AAngle. The angle is
  in radians and positive for counter-clockwise rotation.
  Note that y points downwards.
-------------------------------------------------------------------------------}
function RotatePoint(const APoint: TPoint; AAngle: Double): TPoint;
var
  sa, ca: Double;
begin
  SinCos(AAngle, sa, ca);
  Result.X := Round( ca * APoint.X + sa * APoint.Y);
  Result.Y := Round(-sa * APoint.X + ca * APoint.Y);
end;

procedure GetMinMax(x: Integer; var min, max: Integer);
begin
  if x < min then min := x;
  if x > max then max := x;
end;

{-------------------------------------------------------------------------------
  Method:   RotateRect
  Params:   AWidth, AHeight, AAngle
  Returns:  smallest TRect containing the rotated rectangle.
  
  Rotates the rectangle (0, 0, AWidth, AHeight) around its top-left corner (0,0)
  by the angle AAngle (in radians).
  Note that y points downwards.
-------------------------------------------------------------------------------}
function RotateRect(AWidth, AHeight: Integer; AAngle: Double): TRect;
var
  P1, P2, P3: TPoint;
begin
  if AAngle = 0 then
    Result := Rect(0, 0, AWidth, AHeight)
  else
  begin
    P1 := RotatePoint(Point(AWidth, 0), AAngle);
    P2 := RotatePoint(Point(0, AHeight), AAngle);
    P3 := P1 + P2;

    Result := Rect(0, 0, 0, 0);
    GetMinMax(P1.X, Result.Left, Result.Right);
    GetMinMax(P2.X, Result.Left, Result.Right);
    GetMinMax(P3.X, Result.Left, Result.Right);
    GetMinMax(P1.Y, Result.Top, Result.Bottom);
    GetMinMax(P2.Y, Result.Top, Result.Bottom);
    GetMinMax(P3.Y, Result.Top, Result.Bottom);
  end;
end;

{------------------------------------------------------------------------------
  Method:   SplitBezier
  Params:   Bezier, Left, Right
  Returns:  Nothing

  Use SplitBezier to split any 4-Point Bezier into two 4-Point Bezier's :
  a 'Left' and a 'Right'. It is primarily for use in Bezier2Polyline.

------------------------------------------------------------------------------}
procedure SplitBezier(const Bezier : TBezier; var Left, Right : TBezier);
var
  Tmp : TFloatPoint;
begin
  Tmp := (Bezier[1] + Bezier[2]) / 2;

  left[0]  := Bezier[0];
  Left[1]  := (Bezier[0] + Bezier[1]) / 2;
  left[2]  := (Left[1] + Tmp) / 2;
  Left[3]  := BezierMidPoint(Bezier);

  right[3] := Bezier[3];
  right[2] := (Bezier[2] + Bezier[3]) / 2;
  Right[1] := (Right[2] + Tmp) / 2;
  right[0] := BezierMidPoint(Bezier);
end;

end.
