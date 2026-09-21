{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Scanline rasterizer: coverage accumulation with anti-aliasing.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.raster;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Math, fpsvg.types, fpsvg.geom;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, math, fpsvg.types, fpsvg.geom;
{$ENDIF FPC_DOTTEDUNITS}

const
  { How many lines across a row of pixels a shape is sampled on }
  SVGSubSamples = 16;
  SVGMinSubSamples = 1;
  SVGMaxSubSamples = 256;

type
  TSVGCoverageArray = array of Byte;

  { Reports one run of covered pixels on row aY, starting at column aX. }
  TSVGSpanEvent = procedure(aY, aX, aCount: Integer;
    aCoverage: PByte) of object;

  TSVGRasterEdge = record
    X0, Y0, X1, Y1 : Double;
    Slope          : Double;
    Direction      : Integer;
  end;
  TSVGRasterEdgeArray = array of TSVGRasterEdge;

  TSVGCrossing = record
    X         : Double;
    Direction : Integer;
  end;
  TSVGCrossingArray = array of TSVGCrossing;

  { Turns a flattened path into runs of pixel coverage. }
  TSVGRasterizer = class(TObject)
  private
    FEdges: TSVGRasterEdgeArray;
    FEdgeCount: Integer;
    FActive: array of Integer;
    FActiveCount: Integer;
    FNextEdge: Integer;
    FCrossings: TSVGCrossingArray;
    FCrossingCount: Integer;
    FAccumulator: array of Double;
    FCoverage: TSVGCoverageArray;
    // The stretch of FAccumulator the current row wrote to, inclusive.
    // Everything outside it is zero.
    FRowFirst, FRowLast: Integer;
    FClipLeft, FClipTop, FClipRight, FClipBottom: Integer;
    FOnSpan: TSVGSpanEvent;
    FSubSamples: Integer;
    procedure SetSubSamples(aValue: Integer);
    procedure AddEdge(const aFrom, aTo: TSVGPoint);
    procedure BuildEdges(aPoly: TSVGPolyPath);
    procedure SortEdges;
    procedure UpdateActive(aRow: Integer);
    procedure CollectCrossings(aSampleY: Double);
    procedure SortCrossings;
    procedure AccumulateSpan(aLeft, aRight, aWeight: Double);
    procedure AccumulateRow(aRow: Integer; aRule: TSVGFillRule);
    procedure EmitRow(aRow: Integer);
  public
    constructor Create;
    // Limits the output to the given half-open pixel rectangle.
    procedure SetClip(aLeft, aTop, aRight, aBottom: Integer);
    // Rasterizes a flattened path.
    procedure Rasterize(aPoly: TSVGPolyPath; aRule: TSVGFillRule);
    // Called once for each run of covered pixels.
    property OnSpan: TSVGSpanEvent read FOnSpan write FOnSpan;
    // Number of lines across a row of pixels that a shape is sampled on.
    // Bounded by SVGMinSubSamples and SVGMaxSubSamples.
    property SubSamples: Integer read FSubSamples write SetSubSamples;
  end;

implementation

{ TSVGRasterizer }

constructor TSVGRasterizer.Create;

begin
  inherited Create;
  FSubSamples := SVGSubSamples;
  SetClip(0, 0, 0, 0);
end;


// Bounds the count.
procedure TSVGRasterizer.SetSubSamples(aValue: Integer);

begin
  if aValue < SVGMinSubSamples then
    aValue := SVGMinSubSamples
  else if aValue > SVGMaxSubSamples then
    aValue := SVGMaxSubSamples;
  FSubSamples := aValue;
end;


procedure TSVGRasterizer.SetClip(aLeft, aTop, aRight, aBottom: Integer);

var
  I: Integer;

begin
  FClipLeft := aLeft;
  FClipTop := aTop;
  FClipRight := Max(aLeft, aRight);
  FClipBottom := Max(aTop, aBottom);
  SetLength(FAccumulator, FClipRight - FClipLeft);
  SetLength(FCoverage, FClipRight - FClipLeft);
  for I := 0 to High(FAccumulator) do
    FAccumulator[I] := 0;
  FRowFirst := 0;
  FRowLast := -1;
end;


procedure TSVGRasterizer.AddEdge(const aFrom, aTo: TSVGPoint);

var
  lEdge: TSVGRasterEdge;

begin
  if aFrom.Y = aTo.Y then
    Exit;
  if aFrom.Y < aTo.Y then
    begin
    lEdge.X0 := aFrom.X;
    lEdge.Y0 := aFrom.Y;
    lEdge.X1 := aTo.X;
    lEdge.Y1 := aTo.Y;
    lEdge.Direction := 1;
    end
  else
    begin
    lEdge.X0 := aTo.X;
    lEdge.Y0 := aTo.Y;
    lEdge.X1 := aFrom.X;
    lEdge.Y1 := aFrom.Y;
    lEdge.Direction := -1;
    end;
  lEdge.Slope := (lEdge.X1 - lEdge.X0) / (lEdge.Y1 - lEdge.Y0);
  if FEdgeCount = Length(FEdges) then
    SetLength(FEdges, Max(16, FEdgeCount * 2));
  FEdges[FEdgeCount] := lEdge;
  Inc(FEdgeCount);
end;


procedure TSVGRasterizer.BuildEdges(aPoly: TSVGPolyPath);

var
  I, J, lCount: Integer;

begin
  FEdgeCount := 0;
  for I := 0 to aPoly.SubPathCount - 1 do
    begin
    lCount := aPoly.PointCount[I];
    if lCount < 2 then
      Continue;
    for J := 0 to lCount - 2 do
      AddEdge(aPoly.Points[I, J], aPoly.Points[I, J + 1]);
    AddEdge(aPoly.Points[I, lCount - 1], aPoly.Points[I, 0]);
    end;
end;


procedure TSVGRasterizer.SortEdges;

var
  I, J: Integer;
  lEdge: TSVGRasterEdge;

begin
  for I := 1 to FEdgeCount - 1 do
    begin
    lEdge := FEdges[I];
    J := I - 1;
    while (J >= 0) and (FEdges[J].Y0 > lEdge.Y0) do
      begin
      FEdges[J + 1] := FEdges[J];
      Dec(J);
      end;
    FEdges[J + 1] := lEdge;
    end;
end;


procedure TSVGRasterizer.UpdateActive(aRow: Integer);

var
  I, lKept: Integer;
  lBottom: Double;

begin
  lBottom := aRow + 1;
  while (FNextEdge < FEdgeCount) and (FEdges[FNextEdge].Y0 < lBottom) do
    begin
    if FActiveCount = Length(FActive) then
      SetLength(FActive, Max(16, FActiveCount * 2));
    FActive[FActiveCount] := FNextEdge;
    Inc(FActiveCount);
    Inc(FNextEdge);
    end;
  lKept := 0;
  for I := 0 to FActiveCount - 1 do
    if FEdges[FActive[I]].Y1 > aRow then
      begin
      FActive[lKept] := FActive[I];
      Inc(lKept);
      end;
  FActiveCount := lKept;
end;


procedure TSVGRasterizer.CollectCrossings(aSampleY: Double);

var
  I: Integer;
  lEdge: TSVGRasterEdge;

begin
  FCrossingCount := 0;
  for I := 0 to FActiveCount - 1 do
    begin
    lEdge := FEdges[FActive[I]];
    if (aSampleY < lEdge.Y0) or (aSampleY >= lEdge.Y1) then
      Continue;
    if FCrossingCount = Length(FCrossings) then
      SetLength(FCrossings, Max(16, FCrossingCount * 2));
    FCrossings[FCrossingCount].X :=
      lEdge.X0 + (aSampleY - lEdge.Y0) * lEdge.Slope;
    FCrossings[FCrossingCount].Direction := lEdge.Direction;
    Inc(FCrossingCount);
    end;
end;


procedure TSVGRasterizer.SortCrossings;

var
  I, J: Integer;
  lCrossing: TSVGCrossing;

begin
  for I := 1 to FCrossingCount - 1 do
    begin
    lCrossing := FCrossings[I];
    J := I - 1;
    while (J >= 0) and (FCrossings[J].X > lCrossing.X) do
      begin
      FCrossings[J + 1] := FCrossings[J];
      Dec(J);
      end;
    FCrossings[J + 1] := lCrossing;
    end;
end;


procedure TSVGRasterizer.AccumulateSpan(aLeft, aRight, aWeight: Double);

var
  lFirst, lLast, lTouched, I: Integer;

begin
  if aLeft < FClipLeft then
    aLeft := FClipLeft;
  if aRight > FClipRight then
    aRight := FClipRight;
  if aRight <= aLeft then
    Exit;
  lFirst := Floor(aLeft);
  lLast := Floor(aRight);
  lTouched := Min(lLast, FClipRight - 1) - FClipLeft;
  if lFirst - FClipLeft < FRowFirst then
    FRowFirst := lFirst - FClipLeft;
  if lTouched > FRowLast then
    FRowLast := lTouched;
  if lFirst = lLast then
    begin
    FAccumulator[lFirst - FClipLeft] :=
      FAccumulator[lFirst - FClipLeft] + (aRight - aLeft) * aWeight;
    Exit;
    end;
  FAccumulator[lFirst - FClipLeft] :=
    FAccumulator[lFirst - FClipLeft] + (lFirst + 1 - aLeft) * aWeight;
  for I := lFirst + 1 to lLast - 1 do
    FAccumulator[I - FClipLeft] := FAccumulator[I - FClipLeft] + aWeight;
  if lLast < FClipRight then
    FAccumulator[lLast - FClipLeft] :=
      FAccumulator[lLast - FClipLeft] + (aRight - lLast) * aWeight;
end;


procedure TSVGRasterizer.AccumulateRow(aRow: Integer; aRule: TSVGFillRule);

// True when the winding number counts as inside under the given rule.
  function IsInside(aWinding: Integer): Boolean;

  begin
    if aRule = frNonZero then
      Result := aWinding <> 0
    else
      Result := Odd(aWinding);
  end;

var
  I, lSample, lWinding: Integer;
  lSampleY, lWeight, lLeft: Double;
  lWasInside, lNowInside: Boolean;

begin
  for I := FRowFirst to FRowLast do
    FAccumulator[I] := 0;
  FRowFirst := Length(FAccumulator);
  FRowLast := -1;
  lWeight := 1 / FSubSamples;
  for lSample := 0 to FSubSamples - 1 do
    begin
    lSampleY := aRow + (lSample + 0.5) / FSubSamples;
    CollectCrossings(lSampleY);
    if FCrossingCount < 2 then
      Continue;
    SortCrossings;
    lWinding := 0;
    lLeft := 0;
    for I := 0 to FCrossingCount - 1 do
      begin
      lWasInside := IsInside(lWinding);
      if aRule = frNonZero then
        Inc(lWinding, FCrossings[I].Direction)
      else
        Inc(lWinding);
      lNowInside := IsInside(lWinding);
      if not lWasInside and lNowInside then
        lLeft := FCrossings[I].X
      else if lWasInside and not lNowInside then
        AccumulateSpan(lLeft, FCrossings[I].X, lWeight);
      end;
    end;
end;


procedure TSVGRasterizer.EmitRow(aRow: Integer);

var
  I, lStart, lValue: Integer;

begin
  I := FRowFirst;
  while I <= FRowLast do
    begin
    if FAccumulator[I] <= 0 then
      begin
      Inc(I);
      Continue;
      end;
    lStart := I;
    while (I <= FRowLast) and (FAccumulator[I] > 0) do
      begin
      lValue := Round(FAccumulator[I] * 255);
      if lValue < 0 then
        lValue := 0
      else if lValue > 255 then
        lValue := 255;
      FCoverage[I] := lValue;
      Inc(I);
      end;
    if Assigned(FOnSpan) then
      FOnSpan(aRow, FClipLeft + lStart, I - lStart, @FCoverage[lStart]);
    end;
end;


procedure TSVGRasterizer.Rasterize(aPoly: TSVGPolyPath; aRule: TSVGFillRule);

var
  lBounds: TSVGRect;
  lTop, lBottom, lRow: Integer;

begin
  if (aPoly = nil) or aPoly.IsEmpty or (FClipRight <= FClipLeft)
     or (FClipBottom <= FClipTop) then
    Exit;
  BuildEdges(aPoly);
  if FEdgeCount = 0 then
    Exit;
  SortEdges;
  lBounds := aPoly.Bounds;
  lTop := Max(FClipTop, Floor(lBounds.Top));
  lBottom := Min(FClipBottom, Ceil(lBounds.Bottom));
  FActiveCount := 0;
  FNextEdge := 0;
  while (FNextEdge < FEdgeCount) and (FEdges[FNextEdge].Y1 <= lTop) do
    Inc(FNextEdge);
  for lRow := lTop to lBottom - 1 do
    begin
    UpdateActive(lRow);
    if FActiveCount = 0 then
      Continue;
    AccumulateRow(lRow, aRule);
    EmitRow(lRow);
    end;
end;


end.
