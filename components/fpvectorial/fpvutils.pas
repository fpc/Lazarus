{
fpvutils.pas

Vector graphics document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
         Pedro Sol Pegorini L de Lima
}
unit fpvutils;

{$define USE_LCL_CANVAS}
{.$define FPVECTORIAL_BEZIERTOPOINTS_DEBUG}
{.$define FPVECTORIAL_DEFLATE_DEBUG}

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, Math, Types,
  {$ifdef USE_LCL_CANVAS}
  Graphics, LCLIntf, LCLType,
  {$endif}
  base64,
  fpvectorial, fpimage, zstream;

type
  T10Strings = array[0..9] of shortstring;
//  TPointsArray = array of TPoint;
  TFPVUByteArray = array of Byte;

  TNumericalEquation = function (AParameter: Double): Double of object; // return the error

  TFPVUDebugOutCallback = procedure (AStr: string) of object;

// Color Conversion routines
function FPColorToRGBHexString(AColor: TFPColor): string;
function RGBToFPColor(AR, AG, AB: byte): TFPColor; inline;
function MixColors(AColor1, AColor2: TFPColor; APos, AMax: Double): TFPColor;
function GradientColor(AColors: TvGradientColors; AValue: Double): TFPColor;
function AlphaBlendColor(AColorBase, AColor: TFPColor): TFPColor;
// Coordinate Conversion routines
function CanvasCoordsToFPVectorial(AY: Integer; AHeight: Integer): Integer; inline;
function CanvasTextPosToFPVectorial(AY: Integer; ACanvasHeight, ATextHeight: Integer): Integer;
function CoordToCanvasX(ACoord: Double; ADestX: Integer; AMulX: Double): Integer; inline;
function CoordToCanvasY(ACoord: Double; ADestY: Integer; AMulY: Double): Integer; inline;
// Other routines
function SeparateString(AString: string; ASeparator: char): T10Strings;
procedure SeparateStringInTwo(AString: string; ASeparator: char; out AStart, AEnd: string);
function Make3DPoint(AX, AY, AZ: Double): T3DPoint; overload; inline;
function Make3DPoint(AX, AY: Double): T3DPoint; overload; inline;
function Point2D(AX, AY: Double): T2DPoint; inline;
function IsGradientBrush(ABrush: TvBrush): Boolean;
// Mathematical routines
function LineEquation_GetPointAndTangentForLength(AStart, AEnd: T3DPoint; ADistance: Double; out AX, AY, ATangentAngle: Double): Boolean;
procedure EllipticalArcToBezier(Xc, Yc, Rx, Ry, startAngle, endAngle: Double; var P1, P2, P3, P4: T3DPoint);
procedure CircularArcToBezier(Xc, Yc, R, startAngle, endAngle: Double; var P1, P2, P3, P4: T3DPoint);
procedure AddBezierToPoints(P1, P2, P3, P4: T3DPoint; var Points: TPointsArray);
function BezierEquation_GetPoint(t: Double; P1, P2, P3, P4: T3DPoint): T3DPoint;
function BezierEquation_GetTangent(t: Double; P1, P2, P3, P4: T3DPoint): Double;
function BezierEquation_GetLength(P1, P2, P3, P4: T3DPoint; AMaxT: Double = 1; ASteps: Integer = 30): Double;
function BezierEquation_GetT_ForLength(P1, P2, P3, P4: T3DPoint; ALength: Double; ASteps: Integer = 30): Double;
function BezierEquation_GetPointAndTangentForLength(P1, P2, P3, P4: T3DPoint;
  ADistance: Double; out AX, AY, ATangentAngle: Double; ASteps: Integer = 30): Boolean;
function CalcEllipseCenter(x1,y1, x2,y2, rx,ry, phi: Double; fa, fs: Boolean;
  out cx,cy, lambda: Double): Boolean;
function CalcEllipsePointAngle(x,y, rx,ry, cx,cy, phi: Double): Double;
procedure CalcEllipsePoint(t, rx,ry, cx,cy, phi: Double; out x,y: Double);
procedure ConvertPathToPolygons(APath: TPath; ADestX, ADestY: Integer; AMulX, AMulY: Double;
  var PolygonPoints: TPointsArray; var PolygonStartIndexes: TIntegerDynArray);
procedure ConvertPathToPoints(APath: TPath; ADestX, ADestY: Integer; AMulX, AMulY: Double; var Points: TPointsArray);
function GetLinePolygonIntersectionPoints(ACoord: Double;
  const APoints: T2DPointsArray; const APolyStarts: TIntegerDynArray;
  ACoordIsX: Boolean): T2DPointsArray; overload;
function GetLinePolygonIntersectionPoints(ACoord: Double;
  const APoints: T2DPointsArray; ACoordIsX: Boolean): T2DPointsArray; overload;
function Rotate2DPoint(P, RotCenter: TPoint; alpha:double): TPoint;
function Rotate3DPointInXY(P, RotCenter: T3DPoint; alpha:double): T3DPoint;
function SamePoint(P1, P2: T3DPoint; Epsilon: Double = 0.0): Boolean; overload;
function SamePoint(P1, P2: TPoint): Boolean; overload;
procedure NormalizeRect(var ARect: TRect);
// Transformation matrix operations
// See http://www.useragentman.com/blog/2011/01/07/css3-matrix-transform-for-the-mathematically-challenged/
procedure ConvertTransformationMatrixToOperations(AA, AB, AC, AD, AE, AF: Double; out ATranslateX, ATranslateY, AScaleX, AScaleY, ASkewX, ASkewY, ARotate: Double);
procedure InvertMatrixOperations(var ATranslateX, ATranslateY, AScaleX, AScaleY, ASkewX, ASkewY, ARotate: Double);
// Numerical Calculus
function SolveNumericallyAngle(ANumericalEquation: TNumericalEquation;
  ADesiredMaxError: Double; ADesiredMaxIterations: Integer = 10): Double;
// Compression/Decompression
procedure DeflateBytes(var ASource, ADest: TFPVUByteArray);
procedure DeflateStream(ASource, ADest: TStream);
// Binary to Text encodings
procedure DecodeASCII85(ASource: string; var ADest: TFPVUByteArray);
procedure DecodeBase64(ASource: string; ADest: TStream);
// Byte array to stream conversion
procedure ByteArrayToStream(ASource: TFPVUByteArray; ADest: TStream);
// Debug
procedure FPVUDebug(AStr: string);
procedure FPVUDebugLn(AStr: string);
// LCL-related routines
{$ifdef USE_LCL_CANVAS}
function ConvertPathToRegion(APath: TPath; ADestX, ADestY: Integer; AMulX, AMulY: Double): HRGN;
{$endif}

procedure AddStringToArray(var A: TStringArray; const B: String);
procedure AddStringsToArray(var A: TStringArray; const B: TStringArray);

var
  FPVUDebugOutCallback: TFPVUDebugOutCallback; // executes DebugLn
  FPVDebugBuffer: string;
  ScreenDpiX: Integer = 96;
  ScreenDpiY: Integer = 96;

implementation

{@@ This function is utilized by the SVG writer and some other places, so
    it shouldn't be changed.
}
function FPColorToRGBHexString(AColor: TFPColor): string;
begin
  Result := Format('%.2x%.2x%.2x', [AColor.Red shr 8, AColor.Green shr 8, AColor.Blue shr 8]);
end;

function RGBToFPColor(AR, AG, AB: byte): TFPColor; inline;
begin
  Result.Red := (AR shl 8) + AR;
  Result.Green := (AG shl 8) + AG;
  Result.Blue := (AB shl 8) + AB;
  Result.Alpha := $FFFF;
end;

{@@ Returns AColor1 if APos = 0, AColor2 if APos = AMax, or interpolates between }
function MixColors(AColor1, AColor2: TFPColor; APos, AMax: Double): TFPColor;
var
  f1, f2: Double;
begin
  f1 := (AMax - APos) / AMax;
  f2 := APos / AMax;
  Result.Alpha := Round(AColor1.Alpha * f1 + AColor2.Alpha * f2);
  Result.Red := Round(AColor1.Red * f1 + AColor2.Red * f2);
  Result.Green := Round(AColor1.Green * f1 + AColor2.Green * f2);
  Result.Blue := Round(AColor1.Blue * f1 + AColor2.Blue * f2);
end;

{@@ Assigns a color to the specified value. The color is interpolated between
    the colors defined in AColors.
}
function GradientColor(AColors: TvGradientColors; AValue: Double): TFPColor;
var
  i: Integer;
  c1, c2: TFPColor;
  p1, p2: Double;
begin
  // Return first color if AValue is below the first color position
  if AValue <= AColors[0].Position then
    Result := AColors[0].Color
  else
  // Return last color if AValue is above the last color position
  if AValue >= AColors[High(AColors)].Position then
    Result := AColors[High(AColors)].Color
  else
    // Find pair of colors positions which bracket the specified value and
    // interpolate color
    for i:= High(AColors)-1 downto 0 do
      if AValue >= AColors[i].Position then
      begin
        c1 := AColors[i].Color;
        c2 := AColors[i+1].Color;
        p1 := AColors[i].Position;
        p2 := AColors[i+1].Position;
        Result := MixColors(c1, c2, AValue - p1, p2 - p1);
        exit;
      end;
end;

function AlphaBlendColor(AColorBase, AColor: TFPColor): TFPColor;
var
  f1, f2: Double;
begin
  f1 := 1 - f2;
  f2 := AColor.Alpha / alphaOpaque;
  Result.Alpha := Round(AColorBase.Alpha * f1 + AColor.Alpha * f2);
  Result.Red := Round(AColorBase.Red * f1 + AColor.Red * f2);
  Result.Green := Round(AColorBase.Green * f1 + AColor.Green * f2);
  Result.Blue := Round(AColorBase.Blue * f1 + AColor.Blue * f2);
end;

{@@ Converts the coordinate system from a TCanvas to FPVectorial
    The basic difference is that the Y axis is positioned differently and
    points upwards in FPVectorial and downwards in TCanvas.
    The X axis doesn't change. The fix is trivial and requires only the Height of
    the Canvas as extra info.

    @param AHeight Should receive TCanvas.Height
}
function CanvasCoordsToFPVectorial(AY: Integer; AHeight: Integer): Integer; inline;
begin
  Result := AHeight - AY;
end;

{@@
  LCL Text is positioned based on the top-left corner of the text.
  Besides that, one also needs to take the general coordinate change into account too.

  @param ACanvasHeight Should receive TCanvas.Height
  @param ATextHeight   Should receive TFont.Size
}
function CanvasTextPosToFPVectorial(AY: Integer; ACanvasHeight, ATextHeight: Integer): Integer;
begin
  Result := CanvasCoordsToFPVectorial(AY, ACanvasHeight) - ATextHeight;
end;

function CoordToCanvasX(ACoord: Double; ADestX: Integer; AMulX: Double): Integer;
begin
  Result := Round(ADestX + AmulX * ACoord);
end;

function CoordToCanvasY(ACoord: Double; ADestY: Integer; AMulY: Double): Integer;
begin
  Result := Round(ADestY + AmulY * ACoord);
end;

{@@
  Reads a string and separates it in substring
  using ASeparator to delimite them.

  Limits:

  Number of substrings: 10 (indexed 0 to 9)
  Length of each substring: 255 (they are shortstrings)
}
function SeparateString(AString: string; ASeparator: char): T10Strings;
var
  i, CurrentPart: integer;
begin
  CurrentPart := 0;

  { Clears the result }
  for i := 0 to 9 do
    Result[i] := '';

  { Iterates througth the string, filling strings }
  for i := 1 to Length(AString) do
  begin
    if Copy(AString, i, 1) = ASeparator then
    begin
      Inc(CurrentPart);

      { Verifies if the string capacity wasn't exceeded }
      if CurrentPart > 9 then
        Exit;
    end
    else
      Result[CurrentPart] := Result[CurrentPart] + Copy(AString, i, 1);
  end;
end;

function Point2D(AX, AY: Double): T2DPoint;
begin
  Result.X := AX;
  Result.Y := AY;
end;

procedure SeparateStringInTwo(AString: string; ASeparator: char; out AStart,
  AEnd: string);
var
  lPosSep: SizeInt;
begin
  lPosSep := Pos(ASeparator, AString);
  AStart := Copy(AString, 0, lPosSep-1);
  AEnd := Copy(AString, lPosSep+1, Length(AString));
end;

function Make3DPoint(AX, AY, AZ: Double): T3DPoint;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
end;

function Make3DPoint(AX, AY: Double): T3DPoint;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := 0;
end;

function IsGradientBrush(ABrush: TvBrush): Boolean;
begin
  Result := ABrush.Kind in [bkHorizontalGradient, bkVerticalGradient,
    bkOtherLinearGradient, bkRadialGradient];
end;

{ Considering a counter-clockwise arc, elliptical and alligned to the axises

  An elliptical Arc can be converted to
  the following Cubic Bezier control points:

  P1 = E(startAngle)            <- start point
  P2 = P1+alfa * dE(startAngle) <- control point
  P3 = P4−alfa * dE(endAngle)   <- control point
  P4 = E(endAngle)              <- end point

  source: http://www.spaceroots.org/documents/ellipse/elliptical-arc.pdf

  The equation of an elliptical arc is:

  X(t) = Xc + Rx * cos(t)
  Y(t) = Yc + Ry * sin(t)

  dX(t)/dt = - Rx * sin(t)
  dY(t)/dt = + Ry * cos(t)
}
procedure EllipticalArcToBezier(Xc, Yc, Rx, Ry, startAngle, endAngle: Double;
  var P1, P2, P3, P4: T3DPoint);
var
  halfLength, arcLength, alfa: Double;
begin
  arcLength := endAngle - startAngle;
  halfLength := (endAngle - startAngle) / 2;
  alfa := sin(arcLength) * (Sqrt(4 + 3*sqr(tan(halfLength))) - 1) / 3;

  // Start point
  P1.X := Xc + Rx * cos(startAngle);
  P1.Y := Yc + Ry * sin(startAngle);

  // End point
  P4.X := Xc + Rx * cos(endAngle);
  P4.Y := Yc + Ry * sin(endAngle);

  // Control points
  P2.X := P1.X + alfa * -1 * Rx * sin(startAngle);
  P2.Y := P1.Y + alfa * Ry * cos(startAngle);

  P3.X := P4.X - alfa * -1 * Rx * sin(endAngle);
  P3.Y := P4.Y - alfa * Ry * cos(endAngle);
end;

// (x2,y2)=(x1+L⋅cos(a),y1+L⋅sin(a)).
// ATangentAngle - in Radians
function LineEquation_GetPointAndTangentForLength(AStart, AEnd: T3DPoint; ADistance: Double; out AX, AY, ATangentAngle: Double): Boolean;
var
  lLineAngle: Double; // to X axis
begin
  Result := False;
//  lLineAngle := arctan((AEnd.Y-AStart.Y) / (AEnd.X - AStart.X));
  lLineAngle := arctan2(AEnd.Y - AStart.Y, AEnd.X - AStart.X);
  AX := AStart.X + ADistance * Cos(lLineAngle);
  AY := AStart.Y + ADistance * Sin(lLineAngle);
end;

procedure CircularArcToBezier(Xc, Yc, R, startAngle, endAngle: Double; var P1,
  P2, P3, P4: T3DPoint);
begin
  EllipticalArcToBezier(Xc, Yc, R, R, startAngle, endAngle, P1, P2, P3, P4);
end;

{ This routine converts a Bezier to a Polygon and adds the points of this polygon
  to the end of the provided Points output variables }
procedure AddBezierToPoints(P1, P2, P3, P4: T3DPoint; var Points: TPointsArray);
var
  CurveLength, k, LastPoint: Integer;
  CurPoint: T3DPoint;
  t: Double;
begin
  {$ifdef FPVECTORIAL_BEZIERTOPOINTS_DEBUG}
  Write(Format('[AddBezierToPoints] P1=%f,%f P2=%f,%f P3=%f,%f P4=%f,%f =>', [P1.X, P1.Y, P2.X, P2.Y, P3.X, P3.Y, P4.X, P4.Y]));
  {$endif}

  // there is no problem here using a number larger than the real one
  // so to be fast, just connect the 4 points...
  CurveLength :=
    Round(sqrt(sqr(P2.X - P1.X) + sqr(P2.Y - P1.Y))) +
    Round(sqrt(sqr(P3.X - P2.X) + sqr(P3.Y - P2.Y))) +
    Round(sqrt(sqr(P4.X - P4.X) + sqr(P4.Y - P3.Y)));

  LastPoint := Length(Points)-1;
  SetLength(Points, Length(Points)+CurveLength);
  for k := 1 to CurveLength do
  begin
    t := k / CurveLength;
    CurPoint := BezierEquation_GetPoint(t, P1, P2, P3, P4);
    Points[LastPoint+k].X := Round(CurPoint.X);
    Points[LastPoint+k].Y := Round(CurPoint.Y);
    {$ifdef FPVECTORIAL_BEZIERTOPOINTS_DEBUG}
    Write(Format(' P=%d,%d', [CurPoint.X, CurPoint.Y]));
    {$endif}
  end;
  {$ifdef FPVECTORIAL_BEZIERTOPOINTS_DEBUG}
  WriteLn(Format(' CurveLength=%d', [CurveLength]));
  {$endif}
end;

// B(t) = (1-t)³ [Prev.X, Prev.Y] + 3 (1-t)² t [X2, Y2] + 3 (1-t) t² [X3, Y3] + t³ [X,Y], 0<=t<=1
function BezierEquation_GetPoint(t: Double; P1, P2, P3, P4: T3DPoint): T3DPoint;
begin
  Result.X := sqr(1 - t) * (1 - t) * P1.X + 3 * t * sqr(1 - t) * P2.X + 3 * t * t * (1 - t) * P3.X + t * t * t * P4.X;
  Result.Y := sqr(1 - t) * (1 - t) * P1.Y + 3 * t * sqr(1 - t) * P2.Y + 3 * t * t * (1 - t) * P3.Y + t * t * t * P4.Y;
end;

// B'(t) = 3 (1-t)² [X2-Prev.X, Y2-Prev.Y] + 6 (1-t) t [X3-X2, Y3-Y2] + 3 t² [X-X3,Y-Y3]
function BezierEquation_GetTangent(t: Double; P1, P2, P3, P4: T3DPoint): Double;
var
  lDerivateVector: T3DPoint;
begin
  lDerivateVector.X := 3 * sqr(1 - t) * (P2.X-P1.X) + 6 * t * (1 - t) * (P3.X-P2.X) + 3 * t * t * (P4.X - P3.X);
  lDerivateVector.Y := 3 * sqr(1 - t) * (P2.Y-P1.Y) + 6 * t * (1 - t) * (P3.Y-P2.Y) + 3 * t * t * (P4.Y - P3.Y);
  Result := arctan(lDerivateVector.Y / lDerivateVector.X)
end;

// See http://www.lemoda.net/maths/bezier-length/index.html
// See http://steve.hollasch.net/cgindex/curves/cbezarclen.html for a more complex method
function BezierEquation_GetLength(P1, P2, P3, P4: T3DPoint; AMaxT: Double; ASteps: Integer): Double;
var
  lCurT, x_diff, y_diff: Double;
  i, lCurStep: Integer;
  lCurPoint, lPrevPoint: T3DPoint;
begin
  Result := 0.0;

  for i := 0 to ASteps do
  begin
    lCurT := i / ASteps;
    if lCurT > AMaxT then Exit;
    lCurPoint := BezierEquation_GetPoint(lCurT, P1, P2, P3, P4);
    if i = 0 then
    begin
      lPrevPoint := lCurPoint;
      Continue;
    end;

    x_diff := lCurPoint.x - lPrevPoint.x;
    y_diff := lCurPoint.y - lPrevPoint.y;
    Result := Result + sqrt(sqr(x_diff) + sqr(y_diff));
    lPrevPoint := lCurPoint;
  end;
end;

function BezierEquation_GetT_ForLength(P1, P2, P3, P4: T3DPoint; ALength: Double; ASteps: Integer): Double;
var
  i: Integer;
  LeftT, RightT: Double;

  function IsLeftBetter(): Boolean;
  var
    lLeftLen, lRightLen: Double;
  begin
    lLeftLen := BezierEquation_GetLength(P1, P2, P3, P4, LeftT, ASteps);
    lRightLen := BezierEquation_GetLength(P1, P2, P3, P4, RightT, ASteps);
    Result := Abs(lLeftLen - ALength) < Abs(lRightLen - ALength);
  end;

begin
  LeftT := 0;
  RightT := 1;

  for i := 0 to ASteps do
  begin
    if IsLeftBetter() then
      RightT := (RightT + LeftT) / 2
    else
      LeftT := (RightT + LeftT) / 2;
  end;

  if IsLeftBetter() then
    Result := RightT
  else
    Result := LeftT;
end;

function BezierEquation_GetPointAndTangentForLength(P1, P2, P3, P4: T3DPoint;
  ADistance: Double; out AX, AY, ATangentAngle: Double; ASteps: Integer): Boolean;
var
  lCurT: Double;
  lCurPoint: T3DPoint;
begin
  Result := False;
  lCurT := BezierEquation_GetT_ForLength(P1, P2, P3, P4, ADistance, ASteps);
  lCurPoint := BezierEquation_GetPoint(lCurT, P1, P2, P3, P4);
  AX := lCurPoint.X;
  AY := lCurPoint.Y;
  ATangentAngle := BezierEquation_GetTangent(lCurT, P1, P2, P3, P4);
  Result := True;
end;

// Calculate center of ellipse defined by two points on its perimeter, the
// major and minor axes, and the "sweep" and "large-angle" flags.
// Calculation follows the SVG implementation notes
// see: http://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes
// - (x1, y1) absolute coordinates of start point of arc
// - (x2, y2) absolute coordinates of end point of arc
// - rx, ry: radii of major and minor ellipse axes. Must be > 0. Use abs() if necessary.
// - phi: rotation angle of ellipse
// - fa: large arc flag (false = small arc, true = large arc)
// - fs: sweep flag (false = counterclockwise, true = clockwise)
// - cx, cy: Center coordinates of ellipse
// - Function result is false if the center cannot be calculated
function CalcEllipseCenter(x1,y1, x2,y2, rx,ry, phi: Double; fa, fs: Boolean;
  out cx,cy, lambda: Double): Boolean;
const
  EPS = 1E-9;
var
  sinphi, cosphi: Extended;
  x1p, x2p, y1p, y2p: Double;  // x1', x2', y1', y2'
  cxp, cyp: Double;            // cx', cy'
  m: Double;
begin
  Result := false;
  if (rx = 0) or (ry = 0) then
    exit;

  rx := abs(rx);  // only positive radii!
  ry := abs(ry);
  SinCos(phi, sinphi, cosphi);

  // (F.6.5.1) in above document
  x1p := ( cosphi*(x1-x2) + sinphi*(y1-y2)) / 2;
  y1p := (-sinphi*(x1-x2) + cosphi*(y1-y2)) / 2;

  lambda := sqr(x1p/rx) + sqr(y1p/ry);
  if lambda > 1 then
  begin
    // If the distance of the points is too large in relation to the ellipse
    // size there is no solution. SVG Implemantation Notes request in this case
    // that the ellipse is magnified so much that a solution exists.
    lambda := sqrt(lambda);
    rx := rx * lambda;
    ry := ry * lambda;
  end else
    lambda := 1.0;

  // (F.6.5.2)
  m := (sqr(rx*ry) - sqr(rx*y1p) - sqr(ry*x1p)) / (sqr(rx*y1p) + sqr(ry*x1p));
  if SameValue(m, 0.0, EPS) then
    // Prevent a crash caused by a tiny negative sqrt argument due to rounding error.
    m := 0
  else if m < 0 then
    exit;
    // Exit if point distance is too large and return "false" - but this
    // should no happen after having applied lambda!
  m := sqrt(m);                  // Positive root for fa <> fs
  if fa = fs then m := -m;       // Negative root for fa = fs.
  cxp :=  m * rx / ry * y1p;
  cyp := -m * ry / rx * x1p;

  // (F.6.5.3)
  cx := cosphi*cxp - sinphi*cyp + (x1 + x2) / 2;
  cy := sinphi*cxp + cosphi*cyp + (y1 + y2) / 2;

  // If the function gets here we have a valid ellipse center in cx,cy
  Result := true;
end;

{ Calculates the arc angle (in radians) of the point (x,y) on the perimeter of
  an ellipse with radii rx,ry and center cx,cy. phi is the rotation angle of
  the ellipse major axis with the x axis.
  The result is in the range 0 .. 2pi}
function CalcEllipsePointAngle(x,y, rx,ry, cx,cy, phi: Double): Double;
var
  p: T3DPoint;
begin
  // Rotate ellipse back to align its major axis with the x axis
  P := Rotate3dPointInXY(Make3dPoint(x-cx, y-cy, 0), Make3dPoint(0, 0, 0), phi);
  // Correctly speaking, above line should use -phi, instead of phi. But
  // Rotate3DPointInXY seems to define the angle in the opposite way.
  Result := arctan2(P.Y/ry, P.X/rx);
  if Result < 0 then Result := TWO_PI + Result;
end;

{ Calculates the x,y coordinates of a point on an ellipse defined by these
  parameters:
  - rx, ry: major and minor radius
  - phi: rotation angle of the ellipse (angle between major axis and x axis)
  - t: angle between x axis and line from ellipse center to point

   parameterized:
     x = cx + rx*cos(t)*cos(phi) - ry*sin(t)*sin(phi)  [1]
     y = cy + ry*sin(t)*cos(phi) + rx*cos(t)*sin(phi)  [2]        }
procedure CalcEllipsePoint(t, rx,ry, cx,cy, phi: Double; out x,y: Double);
var
  P: T3dPoint;
  cost, sint: Extended;
  cosphi, sinphi: Extended;
begin
  SinCos(t, sint, cost);
  SinCos(phi, sinphi, cosphi);
  x := cx + rx*cost*cosphi - ry*sint*sinphi;
  y := cy + ry*sint*cosphi + rx*cost*sinphi;
end;

{ Converts a path to one or more polygons. The polygon vertices are returned
  in "PolygonPoints"; they are given in canvas units (pixels).
  Since the path can contain several polygons the start index of each polygon
  is returned in "PolygonStartIndexes". }
procedure ConvertPathToPolygons(APath: TPath;
  ADestX, ADestY: Integer; AMulX, AMulY: Double;
  var PolygonPoints: TPointsArray;
  var PolygonStartIndexes: TIntegerDynArray);
const
  POINT_BUFFER = 100;
var
  i, j: Integer;
  numPoints: Integer;
  numPolygons: Integer;
  coordX, coordY: Integer;
  coordX2, coordY2, coordX3, coordY3, coordX4, coordY4: Integer;
  // temporary point arrays
  pts: array of TPoint;
  pts3D: T3dPointsArray;
  // Segments
  curSegment: TPathSegment;
  cur2DSegment: T2DSegment absolute curSegment;
  cur2DBSegment: T2DBezierSegment absolute curSegment;
  cur2DArcSegment: T2DEllipticalArcSegment absolute curSegment;
begin
  if (APath = nil) then
  begin
    SetLength(PolygonPoints, 0);
    SetLength(PolygonStartIndexes, 0);
    exit;
  end;

  SetLength(PolygonPoints, POINT_BUFFER);
  SetLength(PolygonStartIndexes, POINT_BUFFER);
  numPoints := 0;
  numPolygons := 0;

  APath.PrepareForSequentialReading;
  for i := 0 to APath.Len - 1 do
  begin
    curSegment := TPathSegment(APath.Next);

    if (i = 0) and (curSegment.SegmentType <> stMoveTo) then
      raise Exception.Create('Path must start with a "MoveTo" command');

    case curSegment.SegmentType of
      stMoveTo:
      begin
        // Store current length of points array as polygon start index
        if numPolygons >= Length(PolygonStartIndexes) then
          SetLength(PolygonstartIndexes, Length(PolygonStartIndexes) + POINT_BUFFER);
        PolygonStartIndexes[numPolygons] := numPoints;
        inc(numPolygons);

        // Store current point as first point of a new polygon
        coordX := CoordToCanvasX(cur2DSegment.X, ADestX, AMulX);
        coordY := CoordToCanvasY(cur2DSegment.Y, ADestY, AMulY);
        if numPoints >= Length(PolygonPoints) then
          SetLength(PolygonPoints, Length(PolygonPoints) + POINT_BUFFER);
        PolygonPoints[numPoints] := Point(coordX, coordY);
        inc(numPoints);
      end;

      st2DLine, st3DLine, st2DLineWithPen:
      begin
        // Add current point to current polygon
        coordX := CoordToCanvasX(cur2DSegment.X, ADestX, AMulX);
        coordY := CoordToCanvasY(cur2DSegment.Y, ADestY, AMulY);
        if numPoints >= Length(PolygonPoints) then
          SetLength(PolygonPoints, Length(PolygonPoints) + POINT_BUFFER);
        PolygonPoints[numPoints] := Point(coordX, coordY);
        inc(numPoints);
      end;

      st2DBezier, st3DBezier, st2DEllipticalArc:
      begin
        SetLength(PolygonPoints, numPoints);
        curSegment.AddToPoints(ADestX, ADestY, AMulX, AMulY, PolygonPoints);
        numPoints := Length(PolygonPoints);
      end;
    end;
  end;
  SetLength(PolygonPoints, numPoints);
  SetLength(PolygonStartIndexes, numPolygons);
end;

procedure ConvertPathToPoints(APath: TPath; ADestX, ADestY: Integer; AMulX, AMulY: Double; var Points: TPointsArray);
var
  i, LastPoint: Integer;
  CoordX, CoordY: Integer;
  CoordX2, CoordY2, CoordX3, CoordY3, CoordX4, CoordY4: Integer;
  // Segments
  CurSegment: TPathSegment;
  Cur2DSegment: T2DSegment absolute CurSegment;
  Cur2DBSegment: T2DBezierSegment absolute CurSegment;
begin
  APath.PrepareForSequentialReading;

  SetLength(Points, 0);

  for i := 0 to APath.Len - 1 do
  begin
    CurSegment := TPathSegment(APath.Next());

    CoordX := CoordToCanvasX(Cur2DSegment.X, ADestX, AMulX);
    CoordY := CoordToCanvasY(Cur2DSegment.Y, ADestY, AMulY);

    case CurSegment.SegmentType of
    st2DBezier, st3DBezier:
    begin
      LastPoint := Length(Points)-1;
      CoordX4 := CoordX;
      CoordY4 := CoordY;
      CoordX := Points[LastPoint].X;
      CoordY := Points[LastPoint].Y;
      CoordX2 := CoordToCanvasX(Cur2DBSegment.X2, ADestX, AMulX);
      CoordY2 := CoordToCanvasY(Cur2DBSegment.Y2, ADestY, AMulY);
      CoordX3 := CoordToCanvasX(Cur2DBSegment.X3, ADestX, AMulX);
      CoordY3 := CoordToCanvasY(Cur2DBSegment.Y3, ADestY, AMulY);
      AddBezierToPoints(
        Make3DPoint(CoordX, CoordY, 0),
        Make3DPoint(CoordX2, CoordY2, 0),
        Make3DPoint(CoordX3, CoordY3, 0),
        Make3DPoint(CoordX4, CoordY4, 0),
        Points);
    end;
    else
      LastPoint := Length(Points);
      SetLength(Points, Length(Points)+1);
      Points[LastPoint].X := CoordX;
      Points[LastPoint].Y := CoordY;
    end;
  end;
end;

function CompareDbl(P1, P2: Pointer): Integer;
var
  val1, val2: ^Double;
begin
  val1 := P1;
  val2 := P2;
  Result := CompareValue(val1^, val2^);
end;

function GetLinePolygonIntersectionPoints(ACoord: Double;
  const APoints: T2DPointsArray; ACoordIsX: Boolean): T2DPointsArray;
var
  polystarts: TIntegerDynArray;
begin
  SetLength(polystarts, 1);
  polystarts[0] := 0;
  Result := GetLinePolygonIntersectionPoints(ACoord, APoints, ACoordIsX);
end;

{@@ Calculates the intersection points of a vertical (ACoordIsX = true) or
    horizontal (ACoordIsX = false) line with border of the polygon specified
    by APoints. Returns the coordinates of the intersection points }
function GetLinePolygonIntersectionPoints(ACoord: Double;
  const APoints: T2DPointsArray; const APolyStarts: TIntegerDynArray;
  ACoordIsX: Boolean): T2DPointsArray;
const
  EPS = 1e-9;
var
  j, p: Integer;
  firstj,lastj: Integer;
  dx, dy: Double;
  xval, yval: Double;
  val: ^Double;
  list: TFPList;
begin
  list := TFPList.Create;
  if ACoordIsX then
  begin
    for p := 0 to High(APolyStarts) do
    begin
      firstj := APolyStarts[p];
      lastj := IfThen(p = High(APolyStarts), High(APoints), APolyStarts[p+1]-1);
      // Skip non-closed polygons
      if (APoints[firstj].X <> APoints[lastj].x) or (APoints[lastj].Y <> APoints[lastj].Y) then
        continue;
      for j := firstj to lastj-1 do
        if ((APoints[j].X <= ACoord) and (ACoord < APoints[j+1].X)) or
           ((APoints[j+1].X <= ACoord) and (ACoord < APoints[j].X)) then
        begin
          dx := APoints[j+1].X - APoints[j].X;   // can't be zero here
          dy := APoints[j+1].Y - APoints[j].Y;
          New(val);
          val^ := APoints[j].Y + (ACoord - APoints[j].X) * dy / dx;
          list.Add(val);
        end;
      end;
  end else
  begin
    for p := 0 to High(APolyStarts) do begin
      firstj := APolyStarts[p];
      lastj := IfThen(p = High(APolyStarts), High(APoints), APolyStarts[p+1]-1);
      // Skip non-closed polygons
      if (APoints[firstj].X <> APoints[lastj].x) or (APoints[lastj].Y <> APoints[lastj].Y) then
        continue;
      for j := firstj to lastj-1 do
        if ((APoints[j].Y <= ACoord) and (ACoord < APoints[j+1].Y)) or
           ((APoints[j+1].Y <= ACoord) and (ACoord < APoints[j].Y)) then
        begin
          dy := APoints[j+1].Y - APoints[j].Y;     // can't be zero here
          dx := APoints[j+1].X - APoints[j].X;
          New(val);
          val^ := APoints[j].X + (ACoord - APoints[j].Y) * dx / dy;
          list.Add(val);
        end;
    end;
  end;

  // Sort intersection coordinates in ascending order
  list.Sort(@CompareDbl);

  // When scanning across an non-contiguous polygon the scan may produce an
  // odd number of points where the scan finds irregular points due to interaction
  // with the other polygon curves. I don't have a general solution, only for
  // the case of 3 points.
  (*
  if list.Count = 3 then begin // this can't be --> use ony outer points
    SetLength(Result, 2);
    if ACoordIsX then begin
      Result[0] := Point2D(ACoord, Double(list[0]^));
      Result[1] := Point2D(ACoord, Double(list[2]^));
    end else begin
      Result[0] := Point2D(Double(list[0]^), ACoord);
      Result[1] := Point2D(Double(list[2]^), ACoord);
    end;
  end else
  *)
  begin    // regular case
    SetLength(Result, list.Count);
    if ACoordIsX then
      for j:=0 to list.Count-1 do
        Result[j] := Point2D(ACoord, Double(list[j]^))
    else
      for j:=0 to list.Count-1 do
        Result[j] := Point2D(Double(list[j]^), ACoord);
  end;

  // Clean-up
  for j:=list.Count-1 downto 0 do
  begin
    val := List[j];
    Dispose(val);
  end;
  list.Free;
end;

// Rotates a point P around RotCenter
function Rotate2DPoint(P, RotCenter: TPoint; alpha:double): TPoint;
var
  sinus, cosinus : Extended;
begin
  SinCos(alpha, sinus, cosinus);
  P.x := P.x - RotCenter.x;
  P.y := P.y - RotCenter.y;
  result.x := Round(p.x*cosinus + p.y*sinus)  +  RotCenter.x ;
  result.y := Round(-p.x*sinus + p.y*cosinus) +  RotCenter.y;
end;

// Rotates a point P around RotCenter
// alpha angle in radians
// Be CAREFUL: the angle used here grows in clockwise direction. This is
// against mathematical convention!
function Rotate3DPointInXY(P, RotCenter: T3DPoint; alpha:double): T3DPoint;
var
  sinus, cosinus : Extended;
begin
  SinCos(alpha, sinus, cosinus);
  P.x := P.x - RotCenter.x;
  P.y := P.y - RotCenter.y;
  result.x := Round( p.x*cosinus + p.y*sinus)   +  RotCenter.x;
  result.y := Round(-p.x*sinus   + p.y*cosinus) +  RotCenter.y;
  result.z := P.z;
end;

function SamePoint(P1, P2: TPoint): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;

function SamePoint(P1, P2: T3DPoint; Epsilon: Double = 0.0): Boolean;
begin
  Result := SameValue(P1.X, P2.X, Epsilon) and
            SameValue(P1.Y, P2.Y, Epsilon) and
            SameValue(P1.Z, P2.Z, Epsilon);
end;

procedure NormalizeRect(var ARect: TRect);
var
  tmp: Integer;
begin
  if ARect.Left > ARect.Right then
  begin
    tmp := ARect.Left;
    ARect.left := ARect.Right;
    ARect.Right := tmp;
  end;
  if ARect.Top > ARect.Bottom then
  begin
    tmp := ARect.Top;
    ARect.Top := ARect.Bottom;
    ARect.Bottom := tmp;
  end;
end;

// Current Transformation Matrix
// This has 6 numbers, which means this:
//                      (a  c  e)
// [a, b, c, d, e, f] = (b  d  f)
//                      (0  0  1)
// scale(Num)  => a,d=Num  rest=0
// scaleX(Num) => a=Num  d=1 rest=0
// scaleY(Num) => a=1  d=Num rest=0
// TranslateX(Num) => a,d=1 e=Num rest=0
// TranslateY(Num) => a,d=1 f=Num rest=0
// Translate(NumX,NumY)  => a,d=1 e=NumX f=NumY rest=0
// skewX(TX) => a=1 b=0 c=tan(TX) d=1 rest=0
// skewY(TY) => a=1 b=tan(TY) c=0 d=1 rest=0
// skew(TX,TY) => a=1 b=tan(TY) c=tan(TX) d=1 rest=0
// rotate(T) => a=cos(T) b=sin(T) c=-sin(T) d=cos(T) rest=0
//
// Example:
// 0.860815 0 -0 1.07602 339.302 489.171
// Which has a Scale and Translate
//
procedure ConvertTransformationMatrixToOperations(AA, AB, AC, AD, AE,
  AF: Double; out ATranslateX, ATranslateY, AScaleX, AScaleY, ASkewX, ASkewY,
  ARotate: Double);
begin
  ATranslateX := 0;
  ATranslateY := 0;
  AScaleX := 1;
  AScaleY := 1;
  ASkewX := 0;
  ASkewY := 0;
  ARotate := 0;

  // This is valid if AB=AC=0
  ATranslateX := AE;
  ATranslateY := AF;
  AScaleX := AA;
  AScaleY := AD;
end;

{$ifdef USE_LCL_CANVAS}

procedure InvertMatrixOperations(var ATranslateX, ATranslateY, AScaleX,
  AScaleY, ASkewX, ASkewY, ARotate: Double);
begin
  ATranslateX := -1 * ATranslateX;
  ATranslateY := -1 * ATranslateY;
  AScaleX := 1 / AScaleX;
  AScaleY := 1 / AScaleY;
  ASkewX := -1 * ATranslateX;
  ASkewY := -1 * ATranslateX;
  ARotate := -1 * ATranslateX;
end;

function SolveNumericallyAngle(ANumericalEquation: TNumericalEquation;
  ADesiredMaxError: Double; ADesiredMaxIterations: Integer = 10): Double;
var
  lError, lErr1, lErr2, lErr3, lErr4: Double;
  lParam1, lParam2: Double;
  lCount: Integer;
begin
  lErr1 := ANumericalEquation(0);
  lErr2 := ANumericalEquation(Pi/2);
  lErr3 := ANumericalEquation(Pi);
  lErr4 := ANumericalEquation(3*Pi/2);
  // Choose the place to start
  if (lErr1 < lErr2) and (lErr1 < lErr3) and (lErr1 < lErr4) then
  begin
    lParam1 := -Pi/2;
    lParam2 := Pi/2;
  end
  else if (lErr2 < lErr3) and (lErr2 < lErr4) then
  begin
    lParam1 := 0;
    lParam2 := Pi;
  end
  else if (lErr2 < lErr3) and (lErr2 < lErr4) then      // wp: same as above!
  begin
    lParam1 := Pi/2;
    lParam2 := 3*Pi/2;
  end
  else
  begin
    lParam1 := Pi;
    lParam2 := TWO_PI;
  end;

  // Iterate as many times necessary to get the best answer!
  lCount := 0;
  lError := $FFFFFFFF;
  while ((ADesiredMaxError < 0 ) or (lError > ADesiredMaxError))
    and (lParam1 <> lParam2)
    and ((ADesiredMaxIterations < 0) or (lCount < ADesiredMaxIterations)) do
  begin
    lErr1 := ANumericalEquation(lParam1);
    lErr2 := ANumericalEquation(lParam2);

    if lErr1 < lErr2 then
      lParam2 := (lParam1+lParam2)/2
    else
      lParam1 := (lParam1+lParam2)/2;

    lError := Min(lErr1, lErr2);
    Inc(lCount);
  end;

  // Choose the best of the last two
  if lErr1 < lErr2 then
    Result := lParam1
  else
    Result := lParam2
end;

procedure DeflateBytes(var ASource, ADest: TFPVUByteArray);
var
  SourceMem, DestMem: TMemoryStream;
  i: Integer;
begin
  SourceMem := TMemoryStream.Create;
  DestMem := TMemoryStream.Create;
  try
    // copy the source to the stream
    {$ifdef FPVECTORIAL_DEFLATE_DEBUG}
    FPVUDebug('[DeflateBytes] ASource= ');
    {$endif}
    for i := 0 to Length(ASource)-1 do
    begin
      SourceMem.WriteByte(ASource[i]);
      {$ifdef FPVECTORIAL_DEFLATE_DEBUG}
      FPVUDebug(Format('%.2x ', [ASource[i]]));
      {$endif}
    end;
    {$ifdef FPVECTORIAL_DEFLATE_DEBUG}
    FPVUDebugLn('');
    {$endif}
    SourceMem.Position := 0;

    DeflateStream(SourceMem, DestMem);

    // copy the dest from the stream
    DestMem.Position := 0;
    SetLength(ADest, DestMem.Size);
    for i := 0 to DestMem.Size-1 do
      ADest[i] := DestMem.ReadByte();
  finally
    SourceMem.Free;
    DestMem.Free;
  end;
end;

procedure DeflateStream(ASource, ADest: TStream);
var
  DeCompressionStream: TDecompressionStream;
  readCount: Integer;
  Buf: array[0..1023]of Byte;
  FirstChar: Char;
begin
  ASource.Read(FirstChar, 1);

  if FirstChar <> #120 then
    raise Exception.Create('File is not a zLib archive');

  ASource.Position := 0;
  DecompressionStream := TDecompressionStream.Create(ASource);
  repeat
    readCount := DecompressionStream.Read(Buf, SizeOf(Buf));
    if readCount <> 0 then ADest.Write(Buf, readCount);
  until readCount < SizeOf(Buf);

  DecompressionStream.Free;
end;

procedure DecodeASCII85(ASource: string; var ADest: TFPVUByteArray);
var
  CurSrcPos, CurDestPos: Integer;
  lDataDWordPtr: PCardinal;
  lDataCurChar: Char;
begin
  SetLength(ADest, 0);
  CurDestPos := 0;

  CurSrcPos := 1;
  while CurSrcPos <= Length(ASource) do
  begin
    lDataCurChar := ASource[CurSrcPos];

    // Compressed block of zeroes
    if lDataCurChar = 'z' then
    begin
      SetLength(ADest, Length(ADest)+4);
      ADest[CurDestPos] := 0;
      ADest[CurDestPos+1] := 0;
      ADest[CurDestPos+2] := 0;
      ADest[CurDestPos+3] := 0;
      Inc(CurDestPos, 4);
      Inc(CurSrcPos, 1);
      Continue;
    end;

    // Common block of data: 5 input bytes generate 4 output bytes
    SetLength(ADest, Length(ADest)+4);
    lDataDWordPtr := @(ADest[CurDestPos]);
    if CurSrcPos+4 <= Length(ASource) then
    begin
      lDataDWordPtr^ := (Byte(ASource[CurSrcPos])-33)*85*85*85*85
       + (Byte(ASource[CurSrcPos+1])-33)*85*85*85 + (Byte(ASource[CurSrcPos+2])-33)*85*85
       + (Byte(ASource[CurSrcPos+3])-33)*85       + (Byte(ASource[CurSrcPos+4])-33);
      lDataDWordPtr^ := NToBE(lDataDWordPtr^);
    end
    else if CurSrcPos+3 <= Length(ASource) then
    begin
      lDataDWordPtr^ := (Byte(ASource[CurSrcPos])-33)*85*85*85*85
       + (Byte(ASource[CurSrcPos+1])-33)*85*85*85 + (Byte(ASource[CurSrcPos+2])-33)*85*85
       + (Byte(ASource[CurSrcPos+3])-33)*85       + (Byte('u')-33);
      lDataDWordPtr^ := NToBE(lDataDWordPtr^);
      SetLength(ADest, Length(ADest)-1);
    end
    else if CurSrcPos+2 <= Length(ASource) then
    begin
      lDataDWordPtr^ := (Byte(ASource[CurSrcPos])-33)*85*85*85*85
       + (Byte(ASource[CurSrcPos+1])-33)*85*85*85 + (Byte(ASource[CurSrcPos+2])-33)*85*85
       + (Byte('u')-33)*85       + (Byte('u')-33);
      lDataDWordPtr^ := NToBE(lDataDWordPtr^);
      SetLength(ADest, Length(ADest)-2);
    end
    else if CurSrcPos+1 <= Length(ASource) then
    begin
      lDataDWordPtr^ := (Byte(ASource[CurSrcPos])-33)*85*85*85*85
       + (Byte(ASource[CurSrcPos+1])-33)*85*85*85 + (Byte('u')-33)*85*85
       + (Byte('u')-33)*85       + (Byte('u')-33);
      lDataDWordPtr^ := NToBE(lDataDWordPtr^);
      SetLength(ADest, Length(ADest)-3);
    end
    else
    begin
      raise Exception.Create('[DecodeASCII85] Too few bytes remaining to decode!');
    end;

    Inc(CurDestPos, 4);
    Inc(CurSrcPos, 5);
  end;
end;

procedure DecodeBase64(ASource: string; ADest: TStream);
var
  lSourceStream: TStringStream;
  lDecoder: TBase64DecodingStream;
begin
  lSourceStream := TStringStream.Create(ASource);
  lDecoder := TBase64DecodingStream.Create(lSourceStream);
  try
    ADest.CopyFrom(lDecoder, lDecoder.Size);
  finally
    lDecoder.Free;
    lSourceStream.Free;
  end;
end;

procedure ByteArrayToStream(ASource: TFPVUByteArray; ADest: TStream);
var
  i: Integer;
begin
  for i := 0 to Length(ASource)-1 do
    ADest.WriteByte(ASource[i]);
end;

procedure FPVUDebug(AStr: string);
begin
  FPVDebugBuffer := FPVDebugBuffer + AStr;
end;

procedure FPVUDebugLn(AStr: string);
begin
  if Assigned(FPVUDebugOutCallback) then
    FPVUDebugOutCallback(FPVDebugBuffer + AStr);
  FPVDebugBuffer := '';
end;

function ConvertPathToRegion(APath: TPath; ADestX, ADestY: Integer; AMulX, AMulY: Double): HRGN;
var
  WindingMode: Integer;
  Points: array of TPoint;
begin
  APath.PrepareForSequentialReading;

  SetLength(Points, 0);
  ConvertPathToPoints(APath, ADestX, ADestY, AMulX, AMulY, Points);

  if APath.ClipMode = vcmEvenOddRule then WindingMode := LCLType.ALTERNATE
  else WindingMode := LCLType.WINDING;

  Result := LCLIntf.CreatePolygonRgn(@Points[0], Length(Points), WindingMode);
end;
{$endif}

procedure AddStringToArray(var A: TStringArray; const B: String);
begin
  SetLength(A, Length(A) + 1);
  A[High(A)] := B;
end;

procedure AddStringsToArray(var A: TStringArray; const B: TStringArray);
var
  n, i: Integer;
begin
  n := Length(A);
  SetLength(A, n + Length(B));
  for i:=0 to High(B) do
    A[i + n] := B[i];
end;

end.

