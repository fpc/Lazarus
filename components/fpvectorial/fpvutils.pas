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
  Classes, SysUtils, Math,
  {$ifdef USE_LCL_CANVAS}
  Graphics, LCLIntf, LCLType,
  {$endif}
  base64,
  fpvectorial, fpimage, zstream;

type
  T10Strings = array[0..9] of shortstring;
  TPointsArray = array of TPoint;
  TFPVUByteArray = array of Byte;

  TNumericalEquation = function (AParameter: Double): Double of object; // return the error

  TFPVUDebugOutCallback = procedure (AStr: string) of object;

// Color Conversion routines
function FPColorToRGBHexString(AColor: TFPColor): string;
function RGBToFPColor(AR, AG, AB: byte): TFPColor; inline;
// Coordinate Conversion routines
function CanvasCoordsToFPVectorial(AY: Integer; AHeight: Integer): Integer; inline;
function CanvasTextPosToFPVectorial(AY: Integer; ACanvasHeight, ATextHeight: Integer): Integer;
function CoordToCanvasX(ACoord: Double; ADestX: Integer; AMulX: Double): Integer; inline;
function CoordToCanvasY(ACoord: Double; ADestY: Integer; AMulY: Double): Integer; inline;
// Other routines
function SeparateString(AString: string; ASeparator: char): T10Strings;
function Make3DPoint(AX, AY, AZ: Double): T3DPoint;
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
procedure ConvertPathToPoints(APath: TPath; ADestX, ADestY: Integer; AMulX, AMulY: Double; var Points: TPointsArray);
function Rotate2DPoint(P, RotCenter: TPoint; alpha:double): TPoint;
function Rotate3DPointInXY(P, RotCenter: T3DPoint; alpha:double): T3DPoint;
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

var
  FPVUDebugOutCallback: TFPVUDebugOutCallback; // executes DebugLn
  FPVDebugBuffer: string;

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

function Make3DPoint(AX, AY, AZ: Double): T3DPoint;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
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
  lLineAngle := arctan((AEnd.Y-AStart.Y) / (AEnd.X - AStart.X));
  AX := AStart.X + ADistance * Cos(lLineAngle);
  AY := AStart.Y + ADistance * Sin(lLineAngle);
end;

procedure CircularArcToBezier(Xc, Yc, R, startAngle, endAngle: Double; var P1,
  P2, P3, P4: T3DPoint);
begin
  EllipticalArcToBezier(Xc, Yc, R, R, startAngle, endAngle, P1, P2, P3, P4);
end;

{ This routine converts a Bezier to a Polygon and adds the points of this poligon
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
        Make2DPoint(CoordX, CoordY),
        Make2DPoint(CoordX2, CoordY2),
        Make2DPoint(CoordX3, CoordY3),
        Make2DPoint(CoordX4, CoordY4),
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
function Rotate3DPointInXY(P, RotCenter: T3DPoint; alpha:double): T3DPoint;
var
  sinus, cosinus : Extended;
begin
  SinCos(alpha, sinus, cosinus);
  P.x := P.x - RotCenter.x;
  P.y := P.y - RotCenter.y;
  result.x := Round(p.x*cosinus + p.y*sinus)  +  RotCenter.x;
  result.y := Round(-p.x*sinus + p.y*cosinus) +  RotCenter.y;
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
  lIterations: Integer;
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
  else if (lErr2 < lErr3) and (lErr2 < lErr4) then
  begin
    lParam1 := Pi/2;
    lParam2 := 3*Pi/2;
  end
  else
  begin
    lParam1 := Pi;
    lParam2 := 2*Pi;
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

end.

