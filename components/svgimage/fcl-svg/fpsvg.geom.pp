{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Flattening and the polygon form a backend rasterizes.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.geom;

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

const
  SVGDefaultFlatness = 0.25;
  SVGMaxSubdivision = 16;

type
  TSVGSubPath = record
    Points : TSVGPointArray;
    Count  : Integer;
    Closed : Boolean;
  end;
  TSVGSubPathArray = array of TSVGSubPath;

  { Where a marker goes on a path: the first vertex, the last, and every
    turn between them. }
  TSVGVertexKind = (vkStart, vkMid, vkEnd);

  { A vertex of a path, with the direction the path arrives by and the direction it leaves by.
    A direction is zero when there is none:
    nothing arrives at the first vertex, and nothing leaves the last. }
  TSVGPathVertex = record
    Point  : TSVGPoint;
    InDir  : TSVGPoint;
    OutDir : TSVGPoint;
    Kind   : TSVGVertexKind;
    // The angle in degrees that orient="auto" turns a marker to.
    // With two directions it is the angle between them.
    function AutoAngle: Double;
  end;
  TSVGPathVertexArray = array of TSVGPathVertex;

  { A path measured along its length:
    the flattened points, and the distance reached at each one.
    The jump between two subpaths counts as no distance,
    so all subpaths count as one continuous run. }
  TSVGPathMetrics = record
  private
    FPoints: TSVGPointArray;
    FLengths: TSVGDoubleArray;
    FCount: Integer;
  public
    // Measures a path by flattening it in its own space.
    constructor Create(aPath: TSVGPath; aTolerance: Double);
    // The point and the unit tangent at a distance from the start.
    // Returns False when the distance lies outside the path, or nothing was measured.
    function PlaceAt(aDistance: Double; out aPoint,
      aTangent: TSVGPoint): Boolean;
    // Distance from the start of the path to its end.
    function TotalLength: Double;
  end;

  { A path reduced to straight segments, in the space the CTM maps to. }
  TSVGPolyPath = class(TObject)
  private
    FSubPaths: TSVGSubPathArray;
    FCount: Integer;
    function GetClosed(aIndex: Integer): Boolean;
    function GetPoint(aSubPath, aIndex: Integer): TSVGPoint;
    function GetPointCount(aIndex: Integer): Integer;
    procedure AddPoint(const aPoint: TSVGPoint);
    procedure FlattenCubic(const aP0, aP1, aP2, aP3: TSVGPoint;
      aTolerance: Double; aDepth: Integer);
    procedure AppendPiece(const aPoints: TSVGPointArray; aCount: Integer);
    procedure AppendDisc(const aCentre: TSVGPoint; aRadius, aTolerance: Double);
    procedure AppendSegment(const aFrom, aTo: TSVGPoint; aHalfWidth: Double);
    procedure AppendJoin(const aVertex, aInDir, aOutDir: TSVGPoint;
      aHalfWidth: Double; const aPen: TSVGPen; aTolerance: Double);
    procedure AppendCap(const aEnd, aDirection: TSVGPoint; aHalfWidth: Double;
      aCap: TSVGLineCap; aTolerance: Double);
    procedure StrokeSubPath(aSource: TSVGPolyPath; aIndex: Integer;
      const aPen: TSVGPen; aTolerance: Double);
    procedure DashSubPath(aSource: TSVGPolyPath; aIndex: Integer;
      const aPattern: TSVGDoubleArray; aOffset: Double);
  public
    // Discards every subpath.
    procedure Clear;
    // Starts a subpath at the given point.
    procedure MoveTo(const aPoint: TSVGPoint);
    // Appends a straight segment to the current subpath.
    procedure LineTo(const aPoint: TSVGPoint);
    // Marks the current subpath as closed.
    procedure Close;
    // Replaces the contents with aPath flattened through aCTM.
    procedure Flatten(aPath: TSVGPath; const aCTM: TSVGMatrix;
      aTolerance: Double);
    // Maps every point through the matrix, in place.
    procedure Transform(const aMatrix: TSVGMatrix);
    // Replaces the contents with a copy of aSource.
    procedure Assign(aSource: TSVGPolyPath);
    // Replaces the contents with aSource cut into the dash pattern.
    // A source with no pattern is copied unchanged. aSource may not be Self.
    procedure BuildDashes(aSource: TSVGPolyPath;
      const aPattern: TSVGDoubleArray; aOffset: Double);
    // Replaces the contents with the outline of aSource stroked by the
    // pen. The pieces run in the same direction, so filling them with the
    // nonzero rule joins them into one shape. aSource may not be Self, and
    // the dashes of the pen are not applied here.
    procedure BuildStroke(aSource: TSVGPolyPath; const aPen: TSVGPen;
      aTolerance: Double);
    // Bounding box of every point, empty when there are none.
    function Bounds: TSVGRect;
    // Total number of points across all subpaths.
    function TotalPointCount: Integer;
    // True when no subpath holds a segment.
    function IsEmpty: Boolean;
    // The points as text, one subpath per line, for diagnostics.
    function AsText: String;
    // Number of subpaths.
    property SubPathCount: Integer read FCount;
    // Number of points in a subpath.
    property PointCount[aIndex: Integer]: Integer read GetPointCount;
    // True when the source path closed this subpath.
    property Closed[aIndex: Integer]: Boolean read GetClosed;
    // A point of a subpath.
    property Points[aSubPath, aIndex: Integer]: TSVGPoint read GetPoint;
  end;

// The vertices of a path, in order. Markers are placed on these.
function SVGPathVertices(aPath: TSVGPath): TSVGPathVertexArray;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ELSE FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ENDIF FPC_DOTTEDUNITS}

{ TSVGPolyPath }

procedure TSVGPolyPath.Clear;

begin
  FCount := 0;
end;


function TSVGPolyPath.GetPointCount(aIndex: Integer): Integer;

begin
  if (aIndex < 0) or (aIndex >= FCount) then
    raise ESVGError.CreateFmt(SErrSubPathIndexOutOfRange, [aIndex]);
  Result := FSubPaths[aIndex].Count;
end;


function TSVGPolyPath.GetClosed(aIndex: Integer): Boolean;

begin
  if (aIndex < 0) or (aIndex >= FCount) then
    raise ESVGError.CreateFmt(SErrSubPathIndexOutOfRange, [aIndex]);
  Result := FSubPaths[aIndex].Closed;
end;


function TSVGPolyPath.GetPoint(aSubPath, aIndex: Integer): TSVGPoint;

begin
  if (aIndex < 0) or (aIndex >= GetPointCount(aSubPath)) then
    raise ESVGError.CreateFmt(SErrPointIndexOutOfRange, [aIndex]);
  Result := FSubPaths[aSubPath].Points[aIndex];
end;


procedure TSVGPolyPath.MoveTo(const aPoint: TSVGPoint);

begin
  if FCount = Length(FSubPaths) then
    SetLength(FSubPaths, Max(4, FCount * 2));
  FSubPaths[FCount].Count := 0;
  FSubPaths[FCount].Closed := False;
  Inc(FCount);
  AddPoint(aPoint);
end;


procedure TSVGPolyPath.AddPoint(const aPoint: TSVGPoint);

var
  lIndex: Integer;

begin
  lIndex := FCount - 1;
  if FSubPaths[lIndex].Count = Length(FSubPaths[lIndex].Points) then
    SetLength(FSubPaths[lIndex].Points,
      Max(8, FSubPaths[lIndex].Count * 2));
  FSubPaths[lIndex].Points[FSubPaths[lIndex].Count] := aPoint;
  Inc(FSubPaths[lIndex].Count);
end;


procedure TSVGPolyPath.LineTo(const aPoint: TSVGPoint);

var
  lLast: TSVGPoint;

begin
  if FCount = 0 then
    begin
    MoveTo(aPoint);
    Exit;
    end;
  lLast := FSubPaths[FCount - 1].Points[FSubPaths[FCount - 1].Count - 1];
  if (lLast.X = aPoint.X) and (lLast.Y = aPoint.Y) then
    Exit;
  AddPoint(aPoint);
end;


procedure TSVGPolyPath.Close;

begin
  if FCount > 0 then
    FSubPaths[FCount - 1].Closed := True;
end;


procedure TSVGPolyPath.FlattenCubic(const aP0, aP1, aP2, aP3: TSVGPoint;
  aTolerance: Double; aDepth: Integer);

var
  lUX, lUY, lVX, lVY: Double;
  lP01, lP12, lP23, lP012, lP123, lMid: TSVGPoint;

begin
  lUX := 3 * aP1.X - 2 * aP0.X - aP3.X;
  lUY := 3 * aP1.Y - 2 * aP0.Y - aP3.Y;
  lVX := 3 * aP2.X - aP0.X - 2 * aP3.X;
  lVY := 3 * aP2.Y - aP0.Y - 2 * aP3.Y;
  lUX := Max(lUX * lUX, lVX * lVX);
  lUY := Max(lUY * lUY, lVY * lVY);
  if (aDepth >= SVGMaxSubdivision)
     or (lUX + lUY <= 16 * aTolerance * aTolerance) then
    begin
    LineTo(aP3);
    Exit;
    end;
  lP01 := TSVGPoint.Create((aP0.X + aP1.X) / 2, (aP0.Y + aP1.Y) / 2);
  lP12 := TSVGPoint.Create((aP1.X + aP2.X) / 2, (aP1.Y + aP2.Y) / 2);
  lP23 := TSVGPoint.Create((aP2.X + aP3.X) / 2, (aP2.Y + aP3.Y) / 2);
  lP012 := TSVGPoint.Create((lP01.X + lP12.X) / 2, (lP01.Y + lP12.Y) / 2);
  lP123 := TSVGPoint.Create((lP12.X + lP23.X) / 2, (lP12.Y + lP23.Y) / 2);
  lMid := TSVGPoint.Create((lP012.X + lP123.X) / 2, (lP012.Y + lP123.Y) / 2);
  FlattenCubic(aP0, lP01, lP012, lMid, aTolerance, aDepth + 1);
  FlattenCubic(lMid, lP123, lP23, aP3, aTolerance, aDepth + 1);
end;


procedure TSVGPolyPath.Flatten(aPath: TSVGPath; const aCTM: TSVGMatrix;
  aTolerance: Double);

var
  I: Integer;
  lSegment: TSVGPathSegment;
  lCurrent, lStart, lC1, lC2, lEnd: TSVGPoint;

begin
  Clear;
  if aPath = nil then
    Exit;
  if aTolerance <= 0 then
    aTolerance := SVGDefaultFlatness;
  lCurrent := TSVGPoint.Create(0, 0);
  lStart := lCurrent;
  for I := 0 to aPath.SegmentCount - 1 do
    begin
    lSegment := aPath[I];
    case lSegment.Kind of
      skMoveTo:
        begin
        lCurrent := aCTM.Transform(lSegment.Points[0]);
        lStart := lCurrent;
        MoveTo(lCurrent);
        end;
      skLineTo:
        begin
        lCurrent := aCTM.Transform(lSegment.Points[0]);
        LineTo(lCurrent);
        end;
      skCubicTo:
        begin
        lC1 := aCTM.Transform(lSegment.Points[0]);
        lC2 := aCTM.Transform(lSegment.Points[1]);
        lEnd := aCTM.Transform(lSegment.Points[2]);
        FlattenCubic(lCurrent, lC1, lC2, lEnd, aTolerance, 0);
        lCurrent := lEnd;
        end;
      skClose:
        begin
        Close;
        lCurrent := lStart;
        end;
    end;
    end;
end;


procedure TSVGPolyPath.Transform(const aMatrix: TSVGMatrix);

var
  I, J: Integer;

begin
  for I := 0 to FCount - 1 do
    for J := 0 to FSubPaths[I].Count - 1 do
      FSubPaths[I].Points[J] := aMatrix.Transform(FSubPaths[I].Points[J]);
end;


procedure TSVGPolyPath.Assign(aSource: TSVGPolyPath);

var
  I, J: Integer;

begin
  if aSource = Self then
    Exit;
  Clear;
  if aSource = nil then
    Exit;
  for I := 0 to aSource.SubPathCount - 1 do
    begin
    if aSource.PointCount[I] = 0 then
      Continue;
    MoveTo(aSource.Points[I, 0]);
    for J := 1 to aSource.PointCount[I] - 1 do
      AddPoint(aSource.Points[I, J]);
    if aSource.Closed[I] then
      Close;
    end;
end;


// The unit direction from aFrom to aTo, and the distance between them.
// False when the two points are the same.
function UnitDirection(const aFrom, aTo: TSVGPoint;
  out aDirection: TSVGPoint; out aLength: Double): Boolean;

begin
  aDirection := TSVGPoint.Create(aTo.X - aFrom.X, aTo.Y - aFrom.Y);
  aLength := Sqrt(aDirection.X * aDirection.X + aDirection.Y * aDirection.Y);
  Result := aLength > 0;
  if Result then
    aDirection := TSVGPoint.Create(aDirection.X / aLength,
      aDirection.Y / aLength);
end;


// The direction rotated by a quarter turn.
function LeftNormal(const aDirection: TSVGPoint): TSVGPoint;

begin
  Result := TSVGPoint.Create(-aDirection.Y, aDirection.X);
end;


// Number of straight segments an arc needs to stay within the tolerance.
function ArcSteps(aRadius, aSweep, aTolerance: Double): Integer;

var
  lStep: Double;

begin
  if (aRadius <= aTolerance) or (aSweep <= 0) then
    Exit(1);
  lStep := 2 * ArcCos(1 - aTolerance / aRadius);
  if lStep <= 0 then
    Exit(1);
  Result := Max(1, Ceil(aSweep / lStep));
end;


procedure TSVGPolyPath.AppendPiece(const aPoints: TSVGPointArray;
  aCount: Integer);

var
  I: Integer;
  lArea: Double;

begin
  if aCount < 3 then
    Exit;
  lArea := 0;
  for I := 0 to aCount - 1 do
    lArea := lArea + aPoints[I].X * aPoints[(I + 1) mod aCount].Y
      - aPoints[(I + 1) mod aCount].X * aPoints[I].Y;
  if lArea = 0 then
    Exit;
  if lArea > 0 then
    begin
    MoveTo(aPoints[0]);
    for I := 1 to aCount - 1 do
      LineTo(aPoints[I]);
    end
  else
    begin
    MoveTo(aPoints[aCount - 1]);
    for I := aCount - 2 downto 0 do
      LineTo(aPoints[I]);
    end;
  Close;
end;


procedure TSVGPolyPath.AppendDisc(const aCentre: TSVGPoint;
  aRadius, aTolerance: Double);

var
  lSteps, I: Integer;
  lAngle: Double;
  lPoints: TSVGPointArray;

begin
  if aRadius <= 0 then
    Exit;
  lSteps := Max(4, ArcSteps(aRadius, 2 * Pi, aTolerance));
  SetLength(lPoints, lSteps);
  for I := 0 to lSteps - 1 do
    begin
    lAngle := 2 * Pi * I / lSteps;
    lPoints[I] := TSVGPoint.Create(aCentre.X + aRadius * Cos(lAngle),
      aCentre.Y + aRadius * Sin(lAngle));
    end;
  AppendPiece(lPoints, lSteps);
end;


procedure TSVGPolyPath.AppendSegment(const aFrom, aTo: TSVGPoint;
  aHalfWidth: Double);

var
  lDirection, lNormal: TSVGPoint;
  lLength: Double;
  lPoints: TSVGPointArray;

begin
  if not UnitDirection(aFrom, aTo, lDirection, lLength) then
    Exit;
  lNormal := LeftNormal(lDirection);
  SetLength(lPoints, 4);
  lPoints[0] := TSVGPoint.Create(aFrom.X + lNormal.X * aHalfWidth,
    aFrom.Y + lNormal.Y * aHalfWidth);
  lPoints[1] := TSVGPoint.Create(aTo.X + lNormal.X * aHalfWidth,
    aTo.Y + lNormal.Y * aHalfWidth);
  lPoints[2] := TSVGPoint.Create(aTo.X - lNormal.X * aHalfWidth,
    aTo.Y - lNormal.Y * aHalfWidth);
  lPoints[3] := TSVGPoint.Create(aFrom.X - lNormal.X * aHalfWidth,
    aFrom.Y - lNormal.Y * aHalfWidth);
  AppendPiece(lPoints, 4);
end;


procedure TSVGPolyPath.AppendJoin(const aVertex, aInDir, aOutDir: TSVGPoint;
  aHalfWidth: Double; const aPen: TSVGPen; aTolerance: Double);

var
  lCross, lMiterX, lMiterY, lLength, lRatio: Double;
  lN1, lN2: TSVGPoint;
  lSide: Double;
  lPoints: TSVGPointArray;

begin
  lCross := aInDir.X * aOutDir.Y - aInDir.Y * aOutDir.X;
  if lCross = 0 then
    Exit;
  if aPen.Join = ljRound then
    begin
    AppendDisc(aVertex, aHalfWidth, aTolerance);
    Exit;
    end;
  // The join has to fill the outer side of the turn.
  if lCross > 0 then
    lSide := -1
  else
    lSide := 1;
  lN1 := LeftNormal(aInDir);
  lN2 := LeftNormal(aOutDir);
  lN1 := TSVGPoint.Create(lN1.X * lSide * aHalfWidth, lN1.Y * lSide * aHalfWidth);
  lN2 := TSVGPoint.Create(lN2.X * lSide * aHalfWidth, lN2.Y * lSide * aHalfWidth);
  SetLength(lPoints, 4);
  lPoints[0] := aVertex;
  lPoints[1] := TSVGPoint.Create(aVertex.X + lN1.X, aVertex.Y + lN1.Y);
  lPoints[2] := lPoints[1];
  lPoints[3] := TSVGPoint.Create(aVertex.X + lN2.X, aVertex.Y + lN2.Y);
  if aPen.Join = ljMiter then
    begin
    lMiterX := lN1.X + lN2.X;
    lMiterY := lN1.Y + lN2.Y;
    lLength := Sqrt(lMiterX * lMiterX + lMiterY * lMiterY);
    if lLength > 0 then
      begin
      lRatio := 2 * aHalfWidth / lLength;
      if lRatio <= aPen.MiterLimit then
        begin
        lRatio := 2 * aHalfWidth * aHalfWidth / (lLength * lLength);
        lPoints[2] := TSVGPoint.Create(aVertex.X + lMiterX * lRatio,
          aVertex.Y + lMiterY * lRatio);
        end;
      end;
    end;
  AppendPiece(lPoints, 4);
end;


procedure TSVGPolyPath.AppendCap(const aEnd, aDirection: TSVGPoint;
  aHalfWidth: Double; aCap: TSVGLineCap; aTolerance: Double);

var
  lNormal: TSVGPoint;
  lPoints: TSVGPointArray;

begin
  case aCap of
    lcButt: Exit;
    lcRound: AppendDisc(aEnd, aHalfWidth, aTolerance);
    lcSquare:
      begin
      lNormal := LeftNormal(aDirection);
      SetLength(lPoints, 4);
      lPoints[0] := TSVGPoint.Create(aEnd.X + lNormal.X * aHalfWidth,
        aEnd.Y + lNormal.Y * aHalfWidth);
      lPoints[1] := TSVGPoint.Create(
        lPoints[0].X + aDirection.X * aHalfWidth,
        lPoints[0].Y + aDirection.Y * aHalfWidth);
      lPoints[3] := TSVGPoint.Create(aEnd.X - lNormal.X * aHalfWidth,
        aEnd.Y - lNormal.Y * aHalfWidth);
      lPoints[2] := TSVGPoint.Create(
        lPoints[3].X + aDirection.X * aHalfWidth,
        lPoints[3].Y + aDirection.Y * aHalfWidth);
      AppendPiece(lPoints, 4);
      end;
  end;
end;


procedure TSVGPolyPath.StrokeSubPath(aSource: TSVGPolyPath; aIndex: Integer;
  const aPen: TSVGPen; aTolerance: Double);

var
  I, lCount, lLast: Integer;
  lHalfWidth, lLength: Double;
  lFrom, lTo, lDirection, lPrevious: TSVGPoint;
  lClosed, lHaveDirection: Boolean;
  lFirstDirection: TSVGPoint;

begin
  lCount := aSource.PointCount[aIndex];
  lHalfWidth := aPen.Width / 2;
  lClosed := aSource.Closed[aIndex];
  if lCount = 0 then
    Exit;
  if lCount = 1 then
    begin
    if aPen.Cap = lcRound then
      AppendDisc(aSource.Points[aIndex, 0], lHalfWidth, aTolerance)
    else if aPen.Cap = lcSquare then
      AppendCap(aSource.Points[aIndex, 0], TSVGPoint.Create(1, 0),
        lHalfWidth, lcSquare, aTolerance);
    Exit;
    end;
  if lClosed then
    lLast := lCount - 1
  else
    lLast := lCount - 2;
  lHaveDirection := False;
  lPrevious := TSVGPoint.Create(0, 0);
  lFirstDirection := lPrevious;
  for I := 0 to lLast do
    begin
    lFrom := aSource.Points[aIndex, I];
    lTo := aSource.Points[aIndex, (I + 1) mod lCount];
    if not UnitDirection(lFrom, lTo, lDirection, lLength) then
      Continue;
    AppendSegment(lFrom, lTo, lHalfWidth);
    if lHaveDirection then
      AppendJoin(lFrom, lPrevious, lDirection, lHalfWidth, aPen, aTolerance)
    else
      lFirstDirection := lDirection;
    lPrevious := lDirection;
    lHaveDirection := True;
    end;
  if not lHaveDirection then
    Exit;
  if lClosed then
    AppendJoin(aSource.Points[aIndex, 0], lPrevious, lFirstDirection,
      lHalfWidth, aPen, aTolerance)
  else
    begin
    AppendCap(aSource.Points[aIndex, lCount - 1], lPrevious, lHalfWidth,
      aPen.Cap, aTolerance);
    AppendCap(aSource.Points[aIndex, 0],
      TSVGPoint.Create(-lFirstDirection.X, -lFirstDirection.Y),
      lHalfWidth, aPen.Cap, aTolerance);
    end;
end;


procedure TSVGPolyPath.BuildStroke(aSource: TSVGPolyPath; const aPen: TSVGPen;
  aTolerance: Double);

var
  I: Integer;

begin
  Clear;
  if (aSource = nil) or (aSource = Self) or (aPen.Width <= 0) then
    Exit;
  if aTolerance <= 0 then
    aTolerance := SVGDefaultFlatness;
  for I := 0 to aSource.SubPathCount - 1 do
    StrokeSubPath(aSource, I, aPen, aTolerance);
end;


procedure TSVGPolyPath.DashSubPath(aSource: TSVGPolyPath; aIndex: Integer;
  const aPattern: TSVGDoubleArray; aOffset: Double);

var
  I, lCount, lLast, lPhase: Integer;
  lRemaining, lLength, lTaken, lTotal: Double;
  lFrom, lTo, lDirection, lCursor, lNext: TSVGPoint;
  lOn, lStarted: Boolean;

begin
  lCount := aSource.PointCount[aIndex];
  if lCount < 2 then
    Exit;
  lTotal := 0;
  for I := 0 to High(aPattern) do
    lTotal := lTotal + aPattern[I];
  if lTotal <= 0 then
    Exit;
  // Walks the pattern forward by the offset, so a dash may start partly
  // used.
  aOffset := aOffset - Floor(aOffset / lTotal) * lTotal;
  lPhase := 0;
  while aOffset >= aPattern[lPhase] do
    begin
    aOffset := aOffset - aPattern[lPhase];
    lPhase := (lPhase + 1) mod Length(aPattern);
    end;
  lRemaining := aPattern[lPhase] - aOffset;
  lOn := not Odd(lPhase);
  lStarted := False;
  if aSource.Closed[aIndex] then
    lLast := lCount - 1
  else
    lLast := lCount - 2;
  for I := 0 to lLast do
    begin
    lFrom := aSource.Points[aIndex, I];
    lTo := aSource.Points[aIndex, (I + 1) mod lCount];
    if not UnitDirection(lFrom, lTo, lDirection, lLength) then
      Continue;
    lCursor := lFrom;
    while lLength > 0 do
      begin
      lTaken := Min(lLength, lRemaining);
      lNext := TSVGPoint.Create(lCursor.X + lDirection.X * lTaken,
        lCursor.Y + lDirection.Y * lTaken);
      if lOn then
        begin
        if not lStarted then
          begin
          MoveTo(lCursor);
          lStarted := True;
          end;
        LineTo(lNext);
        end;
      lCursor := lNext;
      lLength := lLength - lTaken;
      lRemaining := lRemaining - lTaken;
      if lRemaining <= 0 then
        begin
        lPhase := (lPhase + 1) mod Length(aPattern);
        lRemaining := aPattern[lPhase];
        lOn := not lOn;
        lStarted := False;
        end;
      end;
    end;
end;


procedure TSVGPolyPath.BuildDashes(aSource: TSVGPolyPath;
  const aPattern: TSVGDoubleArray; aOffset: Double);

var
  I: Integer;
  lPattern: TSVGDoubleArray;

begin
  if (aSource = nil) or (aSource = Self) then
    Exit;
  if Length(aPattern) = 0 then
    begin
    Assign(aSource);
    Exit;
    end;
  // A pattern with an odd count is repeated once, so on and off lengths
  // alternate evenly.
  if Odd(Length(aPattern)) then
    begin
    SetLength(lPattern, Length(aPattern) * 2);
    for I := 0 to High(lPattern) do
      lPattern[I] := aPattern[I mod Length(aPattern)];
    end
  else
    lPattern := aPattern;
  Clear;
  for I := 0 to aSource.SubPathCount - 1 do
    DashSubPath(aSource, I, lPattern, aOffset);
end;


function TSVGPolyPath.Bounds: TSVGRect;

var
  I, J: Integer;

begin
  Result := TSVGRect.Empty;
  for I := 0 to FCount - 1 do
    for J := 0 to FSubPaths[I].Count - 1 do
      begin
      Result.Left := Min(Result.Left, FSubPaths[I].Points[J].X);
      Result.Top := Min(Result.Top, FSubPaths[I].Points[J].Y);
      Result.Right := Max(Result.Right, FSubPaths[I].Points[J].X);
      Result.Bottom := Max(Result.Bottom, FSubPaths[I].Points[J].Y);
      end;
end;


function TSVGPolyPath.TotalPointCount: Integer;

var
  I: Integer;

begin
  Result := 0;
  for I := 0 to FCount - 1 do
    Inc(Result, FSubPaths[I].Count);
end;


function TSVGPolyPath.IsEmpty: Boolean;

var
  I: Integer;

begin
  for I := 0 to FCount - 1 do
    if FSubPaths[I].Count > 1 then
      Exit(False);
  Result := True;
end;


function TSVGPolyPath.AsText: String;

var
  I, J: Integer;
  lLine: String;

begin
  Result := '';
  for I := 0 to FCount - 1 do
    begin
    lLine := '';
    for J := 0 to FSubPaths[I].Count - 1 do
      begin
      if J > 0 then
        lLine := lLine + ' ';
      lLine := lLine + FSubPaths[I].Points[J].ToString;
      end;
    if FSubPaths[I].Closed then
      lLine := lLine + ' close';
    Result := Result + lLine + LineEnding;
    end;
end;


{ TSVGPathVertex }

function TSVGPathVertex.AutoAngle: Double;

var
  lX, lY: Double;

begin
  lX := InDir.X + OutDir.X;
  lY := InDir.Y + OutDir.Y;
  // When the two directions cancel out, the outgoing direction is used.
  if (Abs(lX) < 1E-12) and (Abs(lY) < 1E-12) then
    begin
    lX := OutDir.X;
    lY := OutDir.Y;
    if (Abs(lX) < 1E-12) and (Abs(lY) < 1E-12) then
      begin
      lX := InDir.X;
      lY := InDir.Y;
      end;
    end;
  if (Abs(lX) < 1E-12) and (Abs(lY) < 1E-12) then
    Result := 0
  else
    Result := RadToDeg(ArcTan2(lY, lX));
end;


{ TSVGPathMetrics }

constructor TSVGPathMetrics.Create(aPath: TSVGPath; aTolerance: Double);

var
  lPoly: TSVGPolyPath;
  S, I: Integer;
  lPrevious, lPoint: TSVGPoint;
  lRun: Double;

begin
  FPoints := nil;
  FLengths := nil;
  FCount := 0;
  if aPath = nil then
    Exit;
  lPoly := TSVGPolyPath.Create;
  try
    lPoly.Flatten(aPath, TSVGMatrix.Identity, aTolerance);
    if lPoly.TotalPointCount = 0 then
      Exit;
    SetLength(FPoints, lPoly.TotalPointCount + lPoly.SubPathCount);
    SetLength(FLengths, Length(FPoints));
    lRun := 0;
    lPrevious := TSVGPoint.Create(0, 0);
    for S := 0 to lPoly.SubPathCount - 1 do
      begin
      for I := 0 to lPoly.PointCount[S] - 1 do
        begin
        lPoint := lPoly.Points[S, I];
        if I > 0 then
          lRun := lRun + Sqrt(Sqr(lPoint.X - lPrevious.X)
                            + Sqr(lPoint.Y - lPrevious.Y));
        FPoints[FCount] := lPoint;
        FLengths[FCount] := lRun;
        Inc(FCount);
        lPrevious := lPoint;
        end;
      if lPoly.Closed[S] and (lPoly.PointCount[S] > 1) then
        begin
        lPoint := lPoly.Points[S, 0];
        lRun := lRun + Sqrt(Sqr(lPoint.X - lPrevious.X)
                          + Sqr(lPoint.Y - lPrevious.Y));
        FPoints[FCount] := lPoint;
        FLengths[FCount] := lRun;
        Inc(FCount);
        lPrevious := lPoint;
        end;
      end;
    SetLength(FPoints, FCount);
    SetLength(FLengths, FCount);
  finally
    lPoly.Free;
  end;
end;


function TSVGPathMetrics.TotalLength: Double;

begin
  Result := 0;
  if FCount > 0 then
    Result := FLengths[FCount - 1];
end;


function TSVGPathMetrics.PlaceAt(aDistance: Double; out aPoint,
  aTangent: TSVGPoint): Boolean;

var
  lLow, lHigh, lMid, I: Integer;
  lSpan, lRatio: Double;

begin
  Result := False;
  aPoint := TSVGPoint.Create(0, 0);
  aTangent := TSVGPoint.Create(1, 0);
  if FCount < 2 then
    Exit;
  if (aDistance < 0) or (aDistance > FLengths[FCount - 1]) then
    Exit;
  lLow := 0;
  lHigh := FCount - 1;
  while lLow < lHigh do
    begin
    lMid := (lLow + lHigh) div 2;
    if FLengths[lMid] < aDistance then
      lLow := lMid + 1
    else
      lHigh := lMid;
    end;
  I := lLow;
  if I > 0 then
    Dec(I);
  // The jump from one subpath to the next covers no distance. Step over
  // the spans of zero length to reach the one holding the point.
  while (I < FCount - 2) and (FLengths[I + 1] <= FLengths[I]) do
    Inc(I);
  lSpan := FLengths[I + 1] - FLengths[I];
  if lSpan <= 0 then
    Exit;
  lRatio := (aDistance - FLengths[I]) / lSpan;
  aPoint := TSVGPoint.Create(
    FPoints[I].X + (FPoints[I + 1].X - FPoints[I].X) * lRatio,
    FPoints[I].Y + (FPoints[I + 1].Y - FPoints[I].Y) * lRatio);
  Result := UnitDirection(FPoints[I], FPoints[I + 1], aTangent, lSpan);
end;


// The direction from one point to another, zero when they are the same.
function DirectionBetween(const aFrom, aTo: TSVGPoint): TSVGPoint;

var
  lLength: Double;

begin
  Result := TSVGPoint.Create(0, 0);
  UnitDirection(aFrom, aTo, Result, lLength);
end;


function SVGPathVertices(aPath: TSVGPath): TSVGPathVertexArray;

var
  I, lCount: Integer;
  lSegment: TSVGPathSegment;
  lPoint, lStart, lPrevious, lIn: TSVGPoint;
  lHave: Boolean;

  procedure Add(const aPoint, aIn: TSVGPoint);
  begin
    if lCount = Length(Result) then
      SetLength(Result, Max(8, lCount * 2));
    Result[lCount].Point := aPoint;
    Result[lCount].InDir := aIn;
    Result[lCount].OutDir := TSVGPoint.Create(0, 0);
    Result[lCount].Kind := vkMid;
    Inc(lCount);
  end;

begin
  SetLength(Result, 0);
  lCount := 0;
  if aPath = nil then
    Exit;
  lStart := TSVGPoint.Create(0, 0);
  lPrevious := lStart;
  lHave := False;
  for I := 0 to aPath.SegmentCount - 1 do
    begin
    lSegment := aPath[I];
    case lSegment.Kind of
      skMoveTo:
        begin
        lPoint := lSegment.Points[0];
        Add(lPoint, TSVGPoint.Create(0, 0));
        lStart := lPoint;
        lPrevious := lPoint;
        lHave := True;
        end;
      skLineTo:
        if lHave then
          begin
          lPoint := lSegment.Points[0];
          Add(lPoint, DirectionBetween(lPrevious, lPoint));
          lPrevious := lPoint;
          end;
      skCubicTo:
        if lHave then
          begin
          lPoint := lSegment.Points[2];
          // A curve arrives along its last control point. When that
          // control sits on the end point, the chord is used instead.
          lIn := DirectionBetween(lSegment.Points[1], lPoint);
          if (lIn.X = 0) and (lIn.Y = 0) then
            lIn := DirectionBetween(lPrevious, lPoint);
          Add(lPoint, lIn);
          lPrevious := lPoint;
          end;
      skClose:
        if lHave then
          begin
          // Closing draws back to the start of the subpath. That is a
          // separate vertex only when the pen actually moved.
          if (lPrevious.X <> lStart.X) or (lPrevious.Y <> lStart.Y) then
            Add(lStart, DirectionBetween(lPrevious, lStart));
          lPrevious := lStart;
          end;
    end;
    end;
  SetLength(Result, lCount);
  if lCount = 0 then
    Exit;
  // A vertex leaves in the direction the next one arrives with. The two
  // ends of the path take the start and the end marker.
  for I := 0 to lCount - 2 do
    Result[I].OutDir := Result[I + 1].InDir;
  Result[0].Kind := vkStart;
  Result[lCount - 1].Kind := vkEnd;
end;


end.
