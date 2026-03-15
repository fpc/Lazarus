{

 Basic code for function series of TAChart.

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Alexander Klenin

}
unit TACustomFuncSeries;

{$MODE ObjFPC}{$H+}

interface

uses
  Classes,
  TAChartUtils, TACustomSeries, TADrawUtils, TAGraph, TATypes;

type
  { TBasicFuncSeries }

  TBasicFuncSeries = class(TCustomChartSeries)
  strict private
    FExtent: TChartExtent;
    procedure SetExtent(AValue: TChartExtent);
  protected
    procedure AfterAdd; override;
    procedure GetBounds(var ABounds: TDoubleRect); override;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active default true;
    property Extent: TChartExtent read FExtent write SetExtent;
    property ShowInLegend;
    property Title;
    property ZPosition;
  end;

  TMakeDoublePoint = function (AX, AY: Double): TDoublePoint;

  TCustomDrawFuncHelper = class
  private
    FCalc: TTransformFunc;
    FChart: TChart;
    FDrawer: IChartDrawer;
    FEnhancedPen: Boolean;
    FExtentYMax: PDouble;
    FExtentYMin: PDouble;
    FImageToGraph: TImageToGraphFunc;
    FNearestPointParams: ^TNearestPointParams;
    FNearestPointResults: ^TNearestPointResults;
    FMakeDP: TMakeDoublePoint;
    FPrev: TDoublePoint;
    FPrevInExtent: Boolean;
    procedure CalcAt(AXg, AXa: Double; out APt: TDoublePoint; out AIn: Boolean);
    procedure CheckForNearestPoint(AXg, AXa: Double);
    procedure LineTo(AXg, AXa: Double);
    procedure MoveTo(AXg, AXa: Double);
    procedure UpdateExtent(AXg, AXa: Double);
    function XRange: TDoubleInterval;
  protected
    type
      TOnPoint = procedure (AXg, AXa: Double) of object;
  protected
    FAdaptivePts: Boolean;
    FAxisToGraphXr, FAxisToGraphYr, FGraphToAxisXr: TTransformFunc;
    FExtent: TDoubleRect;
    FGraphStep: Double;
    FSeries: TCustomChartSeries;
    procedure ForEachAdaptivePoint(AXg, AXMax: Double; AOnMoveTo, AOnLineTo: TOnPoint); virtual;
    procedure ForEachPoint(AXg, AXMax: Double; AOnMoveTo, AOnLineTo: TOnPoint); virtual; abstract;
  public
    constructor Create(ASeries: TCustomChartSeries; ACalc: TTransformFunc;
      AStep: Integer; AEnhancedPen: Boolean = false);
    procedure CalcAxisExtentY(AMinX, AMaxX: Double; var AMinY, AMaxY: Double);
    procedure DrawFunction(ADrawer: IChartDrawer);
    function GetNearestPoint(
      const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean;
  end;

  TDrawFuncHelper = class(TCustomDrawFuncHelper)
  private
    FDomainExclusions: TIntervalList;
  protected
    FEpsilon: Integer;
    FPixelOversampling: Integer;
    procedure ForEachAdaptivePoint(AXg, AXMax: Double; AOnMoveTo, AOnLineTo: TOnPoint); override;
    procedure ForEachPoint(AXg, AXMax: Double; AOnMoveTo, AOnLineTo: TOnPoint); override;
  public
    constructor Create(ASeries: TCustomChartSeries; ADomainExclusions: TIntervalList;
      ACalc: TTransformFunc; AStep: Integer; AEnhancedPen: Boolean = false);
    procedure SetAdaptiveMode(APixelOversampling, AEpsilon: Integer);
  end;

  TPointsDrawFuncHelper = class(TCustomDrawFuncHelper)
  private
    FStartIndex: Integer;
  protected
    procedure ForEachPoint(AXg, AXMax: Double; AOnMoveTo, AOnLineTo: TOnPoint); override;
  public
    constructor Create(ASeries: TBasicPointSeries; AMinX, AMaxX: Double;
      AStartIndex: Integer; ACalc: TTransformFunc; AStep: Integer;
      AEnhancedPen: Boolean = false);
  end;

implementation

uses
  Math, SysUtils, FPCanvas,
  TAGeometry, TAMath;

function DoublePointRotated(AX, AY: Double): TDoublePoint;
begin
  Result.X := AY;
  Result.Y := AX;
end;


{ TCustomDrawFuncHelper }

procedure TCustomDrawFuncHelper.CalcAt(
  AXg, AXa: Double; out APt: TDoublePoint; out AIn: Boolean);
begin
  APt := FMakeDP(AXg, FAxisToGraphYr(FCalc(AXa)));
  if IsNaN(APt) then
    AIn := false
  else
    AIn := (FExtent.a <= APt) and (APt <= FExtent.b);
end;

procedure TCustomDrawFuncHelper.CalcAxisExtentY(
  AMinX, AMaxX: Double; var AMinY, AMaxY: Double);
begin
  FExtentYMin := @AMinY;
  FExtentYMax := @AMaxY;
  with XRange do
    if FAdaptivePts then
      ForEachAdaptivePoint(FStart, FEnd, @UpdateExtent, @UpdateExtent)
    else
      ForEachPoint(AMinX, AMaxX, @UpdateExtent, @UpdateExtent);
end;

procedure TCustomDrawFuncHelper.CheckForNearestPoint(AXg, AXa: Double);
var
  inExtent: Boolean;
  gp: TDoublePoint;
  ip: TPoint;
  d: Integer;
begin
  CalcAt(AXg, AXa, gp, inExtent);
  if not inExtent then exit;
  ip := FChart.GraphToImage(gp);
  d := FNearestPointParams^.FDistFunc(FNearestPointParams^.FPoint, ip);
  if d >= FNearestPointResults^.FDist then exit;
  FNearestPointResults^.FDist := d;
  FNearestPointResults^.FImg := ip;
  FNearestPointResults^.FValue.X := AXa;
end;

constructor TCustomDrawFuncHelper.Create( ASeries: TCustomChartSeries;
  ACalc: TTransformFunc; AStep: Integer; AEnhancedPen: Boolean = false);
begin
  FChart := ASeries.ParentChart;
  FExtent := FChart.CurrentExtent;
  FSeries := ASeries;
  FCalc := ACalc;

  with FSeries do
    if IsRotated then begin
      FAxisToGraphXr := @AxisToGraphY;
      FAxisToGraphYr := @AxisToGraphX;
      FGraphToAxisXr := @GraphToAxisY;
      FMakeDP := @DoublePointRotated;
      FImageToGraph := @FChart.YImageToGraph;
      AStep := -AStep;
    end
    else begin
      FAxisToGraphXr := @AxisToGraphX;
      FAxisToGraphYr := @AxisToGraphY;
      FGraphToAxisXr := @GraphToAxisX;
      FMakeDP := @DoublePoint;
      FImageToGraph := @FChart.XImageToGraph;
    end;
  FGraphStep := FImageToGraph(AStep) - FImageToGraph(0);

  FEnhancedPen := AEnhancedPen;
end;

procedure TCustomDrawFuncHelper.DrawFunction(ADrawer: IChartDrawer);
var
  savedPenStyle: TFPPenStyle;
begin
  FDrawer := ADrawer;

  ADrawer.SetEnhancedBrokenLines(FEnhancedPen);
  savedPenStyle := ADrawer.GetPenStyle;

  with XRange do
    if FAdaptivePts then
      ForEachAdaptivePoint(FStart, FEnd, @MoveTo, @LineTo)
    else
      ForEachPoint(FStart, FEnd, @MoveTo, @LineTo);

  ADrawer.SetEnhancedBrokenLines(false);
  ADrawer.SetPenStyle(savedPenStyle);
end;

procedure TCustomDrawFuncHelper.ForEachAdaptivePoint(AXg, AXMax: Double;
  AOnMoveTo, AOnLineTo: TOnPoint);
begin
  Unused(AXg, AXMax);
  Unused(AOnMoveTo, AOnLineTo);
  raise EChartError.Create('Adaptive point spacing not implemented for this type of series.');
end;

function TCustomDrawFuncHelper.GetNearestPoint(
  const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  x: Integer;
  r: TDoubleInterval;
begin
  AResults.FIndex := -1;
  AResults.FDist := Sqr(AParams.FRadius) + 1;
  FNearestPointParams := @AParams;
  FNearestPointResults := @AResults;

  with AParams do
    if FOptimizeX then begin
      x := TPointBoolArr(FPoint)[FSeries.IsRotated];
      r := DoubleInterval(FImageToGraph(x - FRadius), FImageToGraph(x + FRadius));
      EnsureOrder(r.FStart, r.FEnd);
    end
    else
      r := DoubleInterval(NegInfinity, SafeInfinity);
  with XRange do
    if FAdaptivePts then
      ForEachAdaptivePoint(
        Max(r.FStart, FStart), Min(r.FEnd, FEnd),
        @CheckForNearestPoint, @CheckForNearestPoint)
    else
      ForEachPoint(
        Max(r.FStart, FStart), Min(r.FEnd, FEnd),
        @CheckForNearestPoint, @CheckForNearestPoint);

  Result := AResults.FDist < Sqr(AParams.FRadius) + 1;
end;

procedure TCustomDrawFuncHelper.LineTo(AXg, AXa: Double);
var
  p, t: TDoublePoint;
  inExtent: Boolean;
begin
  if IsNaN(AXg) then
    exit;
  CalcAt(AXg, AXa, p, inExtent);
  if IsNaN(p) then
    exit;
  t := p;
  if inExtent and FPrevInExtent then
    FDrawer.LineTo(FChart.GraphToImage(p))
  else if LineIntersectsRect(FPrev, t, FExtent) then begin
    FDrawer.MoveTo(FChart.GraphToImage(FPrev));
    FDrawer.LineTo(FChart.GraphToImage(t));
  end;
  FPrevInExtent := inExtent;
  FPrev := p;
end;

procedure TCustomDrawFuncHelper.MoveTo(AXg, AXa: Double);
begin
  CalcAt(AXg, AXa, FPrev, FPrevInExtent);
  if FPrevInExtent then
    FDrawer.MoveTo(FChart.GraphToImage(FPrev));
end;

procedure TCustomDrawFuncHelper.UpdateExtent(AXg, AXa: Double);
begin
  Unused(AXg);
  UpdateMinMax(FCalc(AXa), FExtentYMin^, FExtentYMax^);
end;

function TCustomDrawFuncHelper.XRange: TDoubleInterval;
begin
  if FSeries.IsRotated then
    Result := DoubleInterval(FExtent.a.Y, FExtent.b.Y)
  else
    Result := DoubleInterval(FExtent.a.X, FExtent.b.X);
end;


{ TDrawFuncHelper }

constructor TDrawFuncHelper.Create(ASeries: TCustomChartSeries;
  ADomainExclusions: TIntervalList; ACalc: TTransformFunc; AStep: Integer;
  AEnhancedPen: Boolean = false);
begin
  inherited Create(ASeries, ACalc, AStep, AEnhancedPen);
  FDomainExclusions := ADomainExclusions;
end;

procedure TDrawFuncHelper.ForEachPoint(
  AXg, AXMax: Double; AOnMoveTo, AOnLineTo: TOnPoint);
var
  hint: Integer;
  xa, xg1, xa1, dx: Double;
begin
  if FGraphStep = 0 then exit;

  dx := abs(FGraphStep);
  hint := 0;
  xa := FGraphToAxisXr(AXg);
  if Assigned(FDomainExclusions) and FDomainExclusions.Intersect(xa, xa, hint) then
    AXg := FAxisToGraphXr(xa);

  if AXg < AXMax then
    AOnMoveTo(AXg, xa);

  while AXg < AXMax do begin
    xg1 := AXg + dx;
    xa1 := FGraphToAxisXr(xg1);
    if Assigned(FDomainExclusions) and FDomainExclusions.Intersect(xa, xa1, hint) then
    begin
      AOnLineTo(FAxisToGraphXr(xa), xa);
      xg1 := FAxisToGraphXr(xa1);
      if xg1 < AXMax then
        AOnMoveTo(xg1, xa1);
    end
    else
      AOnLineTo(xg1, xa1);
    AXg := xg1;
    xa := xa1;
  end;
end;

// "Swing-Door-Trend Algorithm" (SDTA) to reduce amount of data points. Samples
// differing in y by less than FEpsilon pixels are dropped. This allows
// massive oversampling to better detect sharp maxima and minima of the function.
// https://softwaredocs.weatherford.com/cygnet/94/Content/Topics/History/CygNet%20Swinging%20Door%20Compression.htm
//
// Adapted from code by forum user "hedgehog":
// https://forum.lazarus.freepascal.org/index.php/topic,73628.msg578118.html#msg578118
procedure TDrawFuncHelper.ForEachAdaptivePoint(AXg, AXMax: Double;
  AOnMoveTo, AOnLineTo: TOnPoint);
var
  dx: Double;    // Step size to advance along curve
  complete: Boolean;

  procedure DoLineTo(AGraphPt, AAxisPt: TDoublePoint);
  begin
    if FSeries.IsRotated then
      AOnLineTo(AGraphPt.Y, AAxisPt.Y)
    else
      AOnLineTo(AGraphPt.X, AAxisPt.X);
  end;

  procedure DoMoveTo(AGraphPt, AAxisPt: TDoublePoint);
  begin
    if FSeries.IsRotated then
      AOnMoveTo(AGraphPt.Y, AAxisPt.Y)
    else
      AOnMoveTo(AGraphPt.X, AAxisPt.X);
  end;

  // Function returns false when drawing operations already have been done
  // due to DomainExclusion. Otherwise the calling procedure must continue to
  // find the next drawing point by SDTA and draw the line segment.
  function GetNextPointOfLine(var xg, xa: Double; var hint: Integer;
    out AGraphPt, AAxisPt: TDoublePoint): Boolean;
  var
    xg1, xa1, ya: Double;
  begin
    Result := true;

    xg1 := xg + dx;
    if xg1 >= AXMax then
    begin
      xg1 := AXMax;
      complete := true;
    end;
    xa1 := FGraphToAxisXr(xg1);

    // Check for domain exclusions between xa and xa1
    if Assigned(FDomainExclusions) and FDomainExclusions.Intersect(xa, xa1, hint) then
    begin
      AOnLineTo(xg, xa);
      xa := xa1;
      xg := FAxisToGraphXr(xa);
      Result := false;
      if xg > AXMax then
      begin
        // Domain exclusion is at the end of the curve
        complete := true;
        AGraphPt := FMakeDP(NaN, NaN);   // Prevent final DoLineTo
        AAxisPt := FMakeDP(NaN, NaN);
        exit;
      end else
        AOnMoveTo(xg, xa);
    end else
    begin
      xg := xg1;
      xa := FGraphToAxisXr(xg);
    end;

    ya := FCalc(xa);
    AGraphPt := FMakeDP(xg, FAxisToGraphYr(ya));
    AAxisPt := FMakeDP(xa, ya);
  end;

  procedure CalcSlopes(ABasePoint, APoint: TDoublePoint;
    out LowSlope, HighSlope: Double);
  var
    imgY: Integer;
    delta: Double;
  begin
    if FSeries.IsRotated then
    begin
      delta := APoint.Y - ABasePoint.Y;
      imgY := FChart.XGraphToImage(APoint.X);
      LowSlope := (FChart.XImageToGraph(imgY - FEpsilon) - ABasePoint.X) / delta;
      HighSlope := (FChart.XImageToGraph(imgY + FEpsilon) - ABasePoint.X) / delta;
    end else
    begin
      delta := APoint.X - ABasePoint.X;
      imgY := FChart.YGraphToImage(APoint.Y);
      LowSlope := (FChart.YImageToGraph(imgY + FEpsilon) - ABasePoint.Y) / delta;
      HighSlope := (FChart.YImageToGraph(imgY - FEpsilon) - ABasePoint.Y) / delta;
    end;
  end;

var
  archPg: TDoublePoint;         // "archived point", in graph coordinates
  heldPa: TDoublePoint;         // "held point", in axis coordinates
  heldPg: TDoublePoint;         // "held point", in graph coordinates
  testPa: TDoublePoint;         // "new (test) point", in axis coordinates
  testPg: TDoublePoint;         // "new (test) point", in graph coordinates;
  xa, ya: Double;               // x and y in axis coordinates
  upperSlope, lowerSlope, testSlope: Double;  // slopes for SDTA testing
  tmpUpperSlope, tmpLowerSlope: Double;
  hint: Integer;                // Index of found domain exclusion
  onePx: Double;                // Size of 1 pixel in graph units
begin
  // We'll calculate FPixelOverSampling samples per pixel differing in x by dx.
  if FSeries.IsRotated then
    onePx := FChart.YImageToGraph(1) - FChart.YImageToGraph(0)
  else
    onePx := FChart.XImageToGraph(1) - FChart.XImageToGraph(0);
  dx := abs(onePx) / FPixelOverSampling;

  xa := FGraphToAxisXr(AXg);
  hint := 0;
  if Assigned(FDomainExclusions) and FDomainExclusions.Intersect(xa, xa, hint) then
    AXg := FAxisToGraphXr(xa);

  if AXg < AXMax then
    AOnMoveTo(AXg, xa)
  else
    exit;

  // Get the first point - this is the "archived point"
  ya := FCalc(xa);
  archPg := FMakeDP(AXg, FAxisToGraphXr(ya));

  // Get next point - this will be the "held point"
  // Hopefully there is no DomainExclusion between archived and held point... To be fixed!
  GetNextPointOfLine(AXg, xa, hint, heldPg, heldPa);

  complete := false;
  while not complete do
  begin
    // Calculate the +/- epsilon slopes
    CalcSlopes(archPg, heldPg, lowerSlope, upperSlope);

    while (not complete) do begin
      // Get next point - this will be a "test point"
      if GetNextPointOfLine(AXg, xa, hint, testPg, testPa) then
      begin
        // slope of the line archived-to-test-point
        if FSeries.IsRotated then
          testSlope := (testPg.X - archPg.X)/(testPg.Y - archPg.Y)
        else
          testSlope := (testPg.Y - archPg.Y)/(testPg.X - archPg.X);

        if not InRange(testSlope, lowerSlope, upperSlope) or (testPg.X >= archPg.X + FGraphStep) then
        begin
          // new point is outside the critical aperture window
          // draw line from archived to held point
          DoLineTo(heldPg, heldPa);

          // held point becomes new archived point
          //archPa := heldPa;
          archPg := heldPg;

          // test point becomes new held point
          heldPa := testPa;
          heldPg := testPg;

          // Return to outer loop
          break;
        end;

        // Test point becomes the held point
        heldPg := testPg;
        heldPa := testPa;

        // Update the slopes of the critical aperture window for the new held point
        CalcSlopes(archPg, heldPg, tmpLowerSlope, tmpUpperSlope);
        if tmpLowerSlope > lowerSlope then lowerSlope := tmpLowerSlope;
        if tmpUpperSlope < upperSlope then upperSlope := tmpUpperSlope;
      end else
      if not complete then
      begin
        // This part is entered when the prev GetNextPointOfLine had detected
        // a DomainExclusion.
        archPg := testPg;
        GetNextPointOfLine(AXg, xa, hint, heldPg, heldPa);
        break;
      end;
    end;
  end;
  DoLineTo(testPg, testPa);
end;

procedure TDrawFuncHelper.SetAdaptiveMode(APixelOversampling, AEpsilon: Integer);
begin
  FAdaptivePts := true;
  FPixelOversampling := APixelOversampling;
  FEpsilon := AEpsilon;
end;


{ TPointsDrawFuncHelper }

type
  TBasicPointSeriesAccess = class(TBasicPointSeries);

constructor TPointsDrawFuncHelper.Create(
  ASeries: TBasicPointSeries; AMinX, AMaxX: Double; AStartIndex: Integer;
  ACalc: TTransformFunc; AStep: Integer; AEnhancedPen: Boolean = false);
begin
  inherited Create(ASeries, ACalc, AStep, AEnhancedPen);
  if ASeries.IsRotated then
  begin
    FExtent.a.Y := Min(AMinX, AMaxX);
    FExtent.b.Y := Max(AMaxX, AMinX);
  end else
  begin
    FExtent.a.X := Min(AMinX, AMaxX);
    FExtent.b.X := Max(AMaxX, AMinX);
  end;
  FStartIndex := AStartIndex;
end;

procedure TPointsDrawFuncHelper.ForEachPoint(
  AXg, AXMax: Double; AOnMoveTo, AOnLineTo: TOnPoint);
var
  xa, xg1, xa1, dx, xfg: Double;
  j, n: Integer;
  ser: TBasicPointSeriesAccess;
  isRotated: Boolean;
begin
  if FGraphStep = 0 then exit;

  if not (FSeries is TBasicPointSeries) then
    raise EChartError.CreateFmt(
      '[%s.ForEachPoint] Series %s must be a TBasicPointSeries',
      [ClassName, NameOrClassName(FSeries)]
    );

  ser := TBasicPointSeriesAccess(FSeries);
  n := Length(ser.FGraphPoints);
  dx := abs(FGraphStep);
  isRotated := ser.IsRotated;

  xa := FAxisToGraphXr(AXg);
  j := Max(0, FStartIndex - ser.FLoBound);

  while (j < n) and (xa > TDoublePointBoolArr(ser.FGraphPoints[j])[isRotated]) do inc(j);
  if j < n then xfg := TDoublePointBoolArr(ser.FGraphPoints[j])[isRotated] else exit;


 // while (j < n) and (xa > ser.FGraphPoints[j].X) do inc(j);
 // if j < n then xfg := ser.FGraphPoints[j].X else exit;
  AOnMoveTo(AXg, xa);

  while AXg < AXMax do begin
    xg1 := AXg + dx;
    xa1 := FGraphToAxisXr(xg1);
    if (j >= 0) and (xg1 > xfg) then begin
      xg1 := xfg;
      xa1 := ser.GetXValue(j + ser.FLoBound);
      inc(j);

      while (j < n) and (xfg > TDoublePointBoolArr(ser.FGraphPoints[j])[isRotated]) do inc(j);   // skip unordered points
      if j < n then xfg := TDoublePointBoolArr(ser.FGraphPoints[j])[isRotated] else xfg := Infinity;


      //while (j < n) and (xfg > ser.FGraphPoints[j].X) do inc(j);   // skip unordered points
      //if j < n then xfg := ser.FGraphPoints[j].X else xfg := Infinity;
    end;
    AOnLineTo(xg1, xa1);
    AXg := xg1;
    xa := xa1;
  end;
end;


{ TBasicFuncSeries }

procedure TBasicFuncSeries.AfterAdd;
begin
  inherited AfterAdd;
  FExtent.SetOwner(FChart);
end;

procedure TBasicFuncSeries.Assign(ASource: TPersistent);
begin
  if ASource is TBasicFuncSeries then
    with TBasicFuncSeries(ASource) do
      Self.Extent := FExtent;
  inherited Assign(ASource);
end;

constructor TBasicFuncSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExtent := TChartExtent.Create(FChart);
end;

destructor TBasicFuncSeries.Destroy;
begin
  FreeAndNil(FExtent);
  inherited Destroy;
end;

procedure TBasicFuncSeries.GetBounds(var ABounds: TDoubleRect);
begin
  with Extent do begin
    if UseXMin then ABounds.a.X := XMin;
    if UseYMin then ABounds.a.Y := YMin;
    if UseXMax then ABounds.b.X := XMax;
    if UseYMax then ABounds.b.Y := YMax;
  end;
end;

procedure TBasicFuncSeries.SetExtent(AValue: TChartExtent);
begin
  if FExtent = AValue then exit;
  FExtent.Assign(AValue);
  UpdateParentChart;
end;

end.

