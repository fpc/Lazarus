{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    The path data grammar, arc conversion and the basic shapes.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.path;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Math, fpsvg.types;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, math, fpsvg.types;
{$ENDIF FPC_DOTTEDUNITS}

type
  { Builds paths: the d grammar, elliptical arcs and the basic shapes. }
  TSVGPathHelper = class helper for TSVGPath
  public
    // Replaces the path with the d grammar in aText. False on a malformed
    // command; the segments read before it are kept.
    function TryParse(const aText: TSVGString): Boolean;
    // Appends an elliptical arc, given by its end point, as cubic
    // segments.
    procedure ArcTo(aRX, aRY, aXRotation: Double;
      aLargeArc, aSweep: Boolean; aX, aY: Double);
    // Appends a rectangle. It is rounded when a radius is positive.
    procedure AddRect(aX, aY, aWidth, aHeight, aRX, aRY: Double);
    // Appends a circle as four arcs.
    procedure AddCircle(aCX, aCY, aR: Double);
    // Appends an axis-aligned ellipse as four arcs.
    procedure AddEllipse(aCX, aCY, aRX, aRY: Double);
    // Appends a single open segment.
    procedure AddLine(aX1, aY1, aX2, aY2: Double);
    // Appends a run of points: closed for a polygon, open for a polyline.
    procedure AddPolygon(const aPoints: TSVGPointArray; aClose: Boolean);
    // The segments as text, one per line, for diagnostics.
    function AsText: TSVGString;
  end;

implementation

const
  ArcEpsilon = 1e-12;

type
  { The cursor and pen state while one run of path data is read. }
  TPathDataParser = class(TObject)
  private
    FPath: TSVGPath;
    FScanner: TSVGScanner;
    FCubicControl: TSVGPoint;
    FQuadControl: TSVGPoint;
    FCommand: AnsiChar;
    FPrevious: AnsiChar;
    FStarted: Boolean;
    function Current: TSVGPoint;
    function NextCommand(out aCommand: AnsiChar): Boolean;
    function ScanCoordinate(out aValue: Double): Boolean;
    function ScanPoint(aRelative: Boolean; out aPoint: TSVGPoint): Boolean;
    function CubicReflection: TSVGPoint;
    function QuadReflection: TSVGPoint;
    function RunMoveTo(aRelative: Boolean): Boolean;
    function RunLineTo(aRelative: Boolean): Boolean;
    function RunHorizontal(aRelative: Boolean): Boolean;
    function RunVertical(aRelative: Boolean): Boolean;
    function RunCubic(aRelative: Boolean): Boolean;
    function RunSmoothCubic(aRelative: Boolean): Boolean;
    function RunQuad(aRelative: Boolean): Boolean;
    function RunSmoothQuad(aRelative: Boolean): Boolean;
    function RunArc(aRelative: Boolean): Boolean;
    function RunClose: Boolean;
    function AtCoordinate: Boolean;
  public
    constructor Create(aPath: TSVGPath; const aText: String);
    function Run: Boolean;
  end;

{ TPathDataParser }

constructor TPathDataParser.Create(aPath: TSVGPath; const aText: String);

begin
  inherited Create;
  FPath := aPath;
  FScanner := TSVGScanner.Create(aText);
end;


function TPathDataParser.Current: TSVGPoint;

begin
  Result := FPath.CurrentPoint;
end;


function TPathDataParser.AtCoordinate: Boolean;

begin
  Result := not FScanner.AtEnd
        and (FScanner.Current in ['0'..'9', '+', '-', '.']);
end;


function TPathDataParser.NextCommand(out aCommand: AnsiChar): Boolean;

begin
  FScanner.SkipWSPComma;
  aCommand := #0;
  Result := not FScanner.AtEnd;
  if not Result then
    Exit;
  aCommand := FScanner.Current;
  Result := aCommand in ['M', 'm', 'L', 'l', 'H', 'h', 'V', 'v',
                         'C', 'c', 'S', 's', 'Q', 'q', 'T', 't',
                         'A', 'a', 'Z', 'z'];
  if Result then
    FScanner.Pos := FScanner.Pos + 1;
end;


function TPathDataParser.ScanCoordinate(out aValue: Double): Boolean;

begin
  FScanner.SkipWSPComma;
  Result := FScanner.ScanNumber(aValue);
end;


function TPathDataParser.ScanPoint(aRelative: Boolean;
  out aPoint: TSVGPoint): Boolean;

var
  lX, lY: Double;

begin
  aPoint := TSVGPoint.Create(0, 0);
  Result := ScanCoordinate(lX) and ScanCoordinate(lY);
  if not Result then
    Exit;
  if aRelative then
    aPoint := TSVGPoint.Create(Current.X + lX, Current.Y + lY)
  else
    aPoint := TSVGPoint.Create(lX, lY);
end;


function TPathDataParser.CubicReflection: TSVGPoint;

begin
  if FPrevious in ['C', 'c', 'S', 's'] then
    Result := TSVGPoint.Create(2 * Current.X - FCubicControl.X,
      2 * Current.Y - FCubicControl.Y)
  else
    Result := Current;
end;


function TPathDataParser.QuadReflection: TSVGPoint;

begin
  if FPrevious in ['Q', 'q', 'T', 't'] then
    Result := TSVGPoint.Create(2 * Current.X - FQuadControl.X,
      2 * Current.Y - FQuadControl.Y)
  else
    Result := Current;
end;


function TPathDataParser.RunMoveTo(aRelative: Boolean): Boolean;

var
  lPoint: TSVGPoint;
  lFirst: Boolean;

begin
  lFirst := True;
  repeat
    if not ScanPoint(aRelative and FStarted, lPoint) then
      Exit(False);
    if lFirst then
      begin
      FPath.MoveTo(lPoint.X, lPoint.Y);
      FStarted := True;
      end
    else
      FPath.LineTo(lPoint.X, lPoint.Y);
    lFirst := False;
    FScanner.SkipWSPComma;
  until not AtCoordinate;
  Result := True;
end;


function TPathDataParser.RunLineTo(aRelative: Boolean): Boolean;

var
  lPoint: TSVGPoint;

begin
  repeat
    if not ScanPoint(aRelative, lPoint) then
      Exit(False);
    FPath.LineTo(lPoint.X, lPoint.Y);
    FScanner.SkipWSPComma;
  until not AtCoordinate;
  Result := True;
end;


function TPathDataParser.RunHorizontal(aRelative: Boolean): Boolean;

var
  lX: Double;

begin
  repeat
    if not ScanCoordinate(lX) then
      Exit(False);
    if aRelative then
      lX := Current.X + lX;
    FPath.LineTo(lX, Current.Y);
    FScanner.SkipWSPComma;
  until not AtCoordinate;
  Result := True;
end;


function TPathDataParser.RunVertical(aRelative: Boolean): Boolean;

var
  lY: Double;

begin
  repeat
    if not ScanCoordinate(lY) then
      Exit(False);
    if aRelative then
      lY := Current.Y + lY;
    FPath.LineTo(Current.X, lY);
    FScanner.SkipWSPComma;
  until not AtCoordinate;
  Result := True;
end;


function TPathDataParser.RunCubic(aRelative: Boolean): Boolean;

var
  lC1, lC2, lEnd: TSVGPoint;

begin
  repeat
    if not (ScanPoint(aRelative, lC1) and ScanPoint(aRelative, lC2)
            and ScanPoint(aRelative, lEnd)) then
      Exit(False);
    FPath.CubicTo(lC1.X, lC1.Y, lC2.X, lC2.Y, lEnd.X, lEnd.Y);
    FCubicControl := lC2;
    FScanner.SkipWSPComma;
  until not AtCoordinate;
  Result := True;
end;


function TPathDataParser.RunSmoothCubic(aRelative: Boolean): Boolean;

var
  lC1, lC2, lEnd: TSVGPoint;

begin
  repeat
    lC1 := CubicReflection;
    if not (ScanPoint(aRelative, lC2) and ScanPoint(aRelative, lEnd)) then
      Exit(False);
    FPath.CubicTo(lC1.X, lC1.Y, lC2.X, lC2.Y, lEnd.X, lEnd.Y);
    FCubicControl := lC2;
    FPrevious := FCommand;
    FScanner.SkipWSPComma;
  until not AtCoordinate;
  Result := True;
end;


function TPathDataParser.RunQuad(aRelative: Boolean): Boolean;

var
  lControl, lEnd: TSVGPoint;

begin
  repeat
    if not (ScanPoint(aRelative, lControl) and ScanPoint(aRelative, lEnd)) then
      Exit(False);
    FPath.QuadTo(lControl.X, lControl.Y, lEnd.X, lEnd.Y);
    FQuadControl := lControl;
    FScanner.SkipWSPComma;
  until not AtCoordinate;
  Result := True;
end;


function TPathDataParser.RunSmoothQuad(aRelative: Boolean): Boolean;

var
  lControl, lEnd: TSVGPoint;

begin
  repeat
    lControl := QuadReflection;
    if not ScanPoint(aRelative, lEnd) then
      Exit(False);
    FPath.QuadTo(lControl.X, lControl.Y, lEnd.X, lEnd.Y);
    FQuadControl := lControl;
    FPrevious := FCommand;
    FScanner.SkipWSPComma;
  until not AtCoordinate;
  Result := True;
end;


function TPathDataParser.RunArc(aRelative: Boolean): Boolean;

var
  lRX, lRY, lRotation: Double;
  lLargeArc, lSweep: Boolean;
  lEnd: TSVGPoint;

begin
  repeat
    if not (ScanCoordinate(lRX) and ScanCoordinate(lRY)
            and ScanCoordinate(lRotation)) then
      Exit(False);
    FScanner.SkipWSPComma;
    if not FScanner.ScanFlag(lLargeArc) then
      Exit(False);
    FScanner.SkipWSPComma;
    if not FScanner.ScanFlag(lSweep) then
      Exit(False);
    if not ScanPoint(aRelative, lEnd) then
      Exit(False);
    FPath.ArcTo(lRX, lRY, lRotation, lLargeArc, lSweep, lEnd.X, lEnd.Y);
    FScanner.SkipWSPComma;
  until not AtCoordinate;
  Result := True;
end;


function TPathDataParser.RunClose: Boolean;

begin
  FPath.Close;
  Result := True;
end;


function TPathDataParser.Run: Boolean;

var
  lRelative: Boolean;

begin
  FPath.Clear;
  FStarted := False;
  FPrevious := #0;
  Result := True;
  FScanner.SkipWSPComma;
  if FScanner.AtEnd then
    Exit;
  while not FScanner.AtEnd do
    begin
    if not NextCommand(FCommand) then
      Exit(False);
    if not FStarted and not (FCommand in ['M', 'm']) then
      Exit(False);
    lRelative := FCommand in ['a'..'z'];
    case UpCase(FCommand) of
      'M': Result := RunMoveTo(lRelative);
      'L': Result := RunLineTo(lRelative);
      'H': Result := RunHorizontal(lRelative);
      'V': Result := RunVertical(lRelative);
      'C': Result := RunCubic(lRelative);
      'S': Result := RunSmoothCubic(lRelative);
      'Q': Result := RunQuad(lRelative);
      'T': Result := RunSmoothQuad(lRelative);
      'A': Result := RunArc(lRelative);
      'Z': Result := RunClose;
    end;
    if not Result then
      Exit;
    FPrevious := FCommand;
    FScanner.SkipWSPComma;
    end;
end;


{ TSVGPathHelper }

function TSVGPathHelper.TryParse(const aText: TSVGString): Boolean;

var
  lParser: TPathDataParser;

begin
  lParser := TPathDataParser.Create(Self, aText);
  try
    Result := lParser.Run;
  finally
    lParser.Free;
  end;
end;


// The angle from the first vector to the second, in radians.
function VectorAngle(aUX, aUY, aVX, aVY: Double): Double;

var
  lDot, lLength: Double;

begin
  lLength := Sqrt(aUX * aUX + aUY * aUY) * Sqrt(aVX * aVX + aVY * aVY);
  if lLength < ArcEpsilon then
    Exit(0);
  lDot := (aUX * aVX + aUY * aVY) / lLength;
  lDot := SVGClamp(lDot, -1, 1);
  Result := ArcCos(lDot);
  if (aUX * aVY - aUY * aVX) < 0 then
    Result := -Result;
end;


procedure TSVGPathHelper.ArcTo(aRX, aRY, aXRotation: Double;
  aLargeArc, aSweep: Boolean; aX, aY: Double);

var
  lStart: TSVGPoint;
  lSin, lCos, lDX2, lDY2, lX1, lY1, lLambda: Double;
  lNumerator, lDenominator, lCoefficient, lCX1, lCY1, lCX, lCY: Double;
  lTheta, lDelta, lStep, lAlpha, lAngle, lNext: Double;
  lSinA, lCosA, lSinB, lCosB: Double;
  lP0, lP1, lP2, lP3: TSVGPoint;
  lSegments, I: Integer;

  function ToEllipse(aPX, aPY: Double): TSVGPoint;

  begin
    Result := TSVGPoint.Create(
      lCX + aRX * aPX * lCos - aRY * aPY * lSin,
      lCY + aRX * aPX * lSin + aRY * aPY * lCos);
  end;

begin
  lStart := CurrentPoint;
  if (Abs(lStart.X - aX) < ArcEpsilon) and (Abs(lStart.Y - aY) < ArcEpsilon) then
    Exit;
  aRX := Abs(aRX);
  aRY := Abs(aRY);
  if (aRX < ArcEpsilon) or (aRY < ArcEpsilon) then
    begin
    LineTo(aX, aY);
    Exit;
    end;
  SinCos(DegToRad(aXRotation), lSin, lCos);
  lDX2 := (lStart.X - aX) / 2;
  lDY2 := (lStart.Y - aY) / 2;
  lX1 := lCos * lDX2 + lSin * lDY2;
  lY1 := -lSin * lDX2 + lCos * lDY2;
  lLambda := (lX1 * lX1) / (aRX * aRX) + (lY1 * lY1) / (aRY * aRY);
  if lLambda > 1 then
    begin
    aRX := aRX * Sqrt(lLambda);
    aRY := aRY * Sqrt(lLambda);
    end;
  lNumerator := aRX * aRX * aRY * aRY - aRX * aRX * lY1 * lY1
    - aRY * aRY * lX1 * lX1;
  lDenominator := aRX * aRX * lY1 * lY1 + aRY * aRY * lX1 * lX1;
  if lDenominator < ArcEpsilon then
    begin
    LineTo(aX, aY);
    Exit;
    end;
  lCoefficient := Sqrt(SVGClamp(lNumerator, 0, MaxDouble) / lDenominator);
  if aLargeArc = aSweep then
    lCoefficient := -lCoefficient;
  lCX1 := lCoefficient * aRX * lY1 / aRY;
  lCY1 := -lCoefficient * aRY * lX1 / aRX;
  lCX := lCos * lCX1 - lSin * lCY1 + (lStart.X + aX) / 2;
  lCY := lSin * lCX1 + lCos * lCY1 + (lStart.Y + aY) / 2;
  lTheta := VectorAngle(1, 0, (lX1 - lCX1) / aRX, (lY1 - lCY1) / aRY);
  lDelta := VectorAngle((lX1 - lCX1) / aRX, (lY1 - lCY1) / aRY,
    (-lX1 - lCX1) / aRX, (-lY1 - lCY1) / aRY);
  if not aSweep and (lDelta > 0) then
    lDelta := lDelta - 2 * Pi
  else if aSweep and (lDelta < 0) then
    lDelta := lDelta + 2 * Pi;
  lSegments := Ceil(Abs(lDelta) / (Pi / 2) - ArcEpsilon);
  if lSegments < 1 then
    lSegments := 1;
  lStep := lDelta / lSegments;
  lAlpha := 4 / 3 * Tan(lStep / 4);
  lAngle := lTheta;
  for I := 1 to lSegments do
    begin
    lNext := lAngle + lStep;
    SinCos(lAngle, lSinA, lCosA);
    SinCos(lNext, lSinB, lCosB);
    lP0 := ToEllipse(lCosA, lSinA);
    lP3 := ToEllipse(lCosB, lSinB);
    lP1 := ToEllipse(lCosA - lAlpha * lSinA, lSinA + lAlpha * lCosA);
    lP2 := ToEllipse(lCosB + lAlpha * lSinB, lSinB - lAlpha * lCosB);
    CubicTo(lP1.X, lP1.Y, lP2.X, lP2.Y, lP3.X, lP3.Y);
    lAngle := lNext;
    end;
end;


procedure TSVGPathHelper.AddRect(aX, aY, aWidth, aHeight, aRX, aRY: Double);

begin
  if (aWidth <= 0) or (aHeight <= 0) then
    Exit;
  if (aRX < 0) and (aRY < 0) then
    begin
    aRX := 0;
    aRY := 0;
    end
  else if aRX < 0 then
    aRX := aRY
  else if aRY < 0 then
    aRY := aRX;
  aRX := Min(aRX, aWidth / 2);
  aRY := Min(aRY, aHeight / 2);
  if (aRX <= 0) or (aRY <= 0) then
    begin
    MoveTo(aX, aY);
    LineTo(aX + aWidth, aY);
    LineTo(aX + aWidth, aY + aHeight);
    LineTo(aX, aY + aHeight);
    Close;
    Exit;
    end;
  MoveTo(aX + aRX, aY);
  LineTo(aX + aWidth - aRX, aY);
  ArcTo(aRX, aRY, 0, False, True, aX + aWidth, aY + aRY);
  LineTo(aX + aWidth, aY + aHeight - aRY);
  ArcTo(aRX, aRY, 0, False, True, aX + aWidth - aRX, aY + aHeight);
  LineTo(aX + aRX, aY + aHeight);
  ArcTo(aRX, aRY, 0, False, True, aX, aY + aHeight - aRY);
  LineTo(aX, aY + aRY);
  ArcTo(aRX, aRY, 0, False, True, aX + aRX, aY);
  Close;
end;


procedure TSVGPathHelper.AddCircle(aCX, aCY, aR: Double);

begin
  AddEllipse(aCX, aCY, aR, aR);
end;


procedure TSVGPathHelper.AddEllipse(aCX, aCY, aRX, aRY: Double);

begin
  if (aRX <= 0) or (aRY <= 0) then
    Exit;
  MoveTo(aCX + aRX, aCY);
  ArcTo(aRX, aRY, 0, False, True, aCX, aCY + aRY);
  ArcTo(aRX, aRY, 0, False, True, aCX - aRX, aCY);
  ArcTo(aRX, aRY, 0, False, True, aCX, aCY - aRY);
  ArcTo(aRX, aRY, 0, False, True, aCX + aRX, aCY);
  Close;
end;


procedure TSVGPathHelper.AddLine(aX1, aY1, aX2, aY2: Double);

begin
  MoveTo(aX1, aY1);
  LineTo(aX2, aY2);
end;


procedure TSVGPathHelper.AddPolygon(const aPoints: TSVGPointArray;
  aClose: Boolean);

var
  I: Integer;

begin
  if Length(aPoints) = 0 then
    Exit;
  MoveTo(aPoints[0].X, aPoints[0].Y);
  for I := 1 to High(aPoints) do
    LineTo(aPoints[I].X, aPoints[I].Y);
  if aClose then
    Close;
end;


function TSVGPathHelper.AsText: TSVGString;

var
  I: Integer;

begin
  Result := '';
  for I := 0 to SegmentCount - 1 do
    Result := Result + Segments[I].ToString + LineEnding;
end;


end.
