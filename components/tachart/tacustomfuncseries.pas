{

 Basic code for function series of TAChart.

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Alexander Klenin

}
unit TACustomFuncSeries;

{$H+}

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
    FAxisToGraphXr, FAxisToGraphYr, FGraphToAxisXr: TTransformFunc;
    FExtent: TDoubleRect;
    FGraphStep: Double;
    FSeries: TCustomChartSeries;
    procedure ForEachPoint(AXg, AXMax: Double; AOnMoveTo, AOnLineTo: TOnPoint); virtual; abstract;
  public
    constructor Create(ASeries: TCustomChartSeries; ACalc: TTransformFunc; AStep: Integer);
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
    procedure ForEachPoint(AXg, AXMax: Double; AOnMoveTo, AOnLineTo: TOnPoint); override;
  public
    constructor Create(ASeries: TCustomChartSeries; ADomainExclusions: TIntervalList;
      ACalc: TTransformFunc; AStep: Integer);
  end;

  TPointsDrawFuncHelper = class(TCustomDrawFuncHelper)
  private
    FStartIndex: Integer;
  protected
    procedure ForEachPoint(AXg, AXMax: Double; AOnMoveTo, AOnLineTo: TOnPoint); override;
  public
    constructor Create(ASeries: TBasicPointSeries; AMinX, AMaxX: Double;
      AStartIndex: Integer; ACalc: TTransformFunc; AStep: Integer);
  end;

implementation

uses
  Math, SysUtils,
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
  AIn := (FExtent.a <= APt) and (APt <= FExtent.b);
end;

procedure TCustomDrawFuncHelper.CalcAxisExtentY(
  AMinX, AMaxX: Double; var AMinY, AMaxY: Double);
begin
  FExtentYMin := @AMinY;
  FExtentYMax := @AMaxY;
  with XRange do
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
  ACalc: TTransformFunc; AStep: Integer);
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
end;

procedure TCustomDrawFuncHelper.DrawFunction(ADrawer: IChartDrawer);
begin
  FDrawer := ADrawer;
  with XRange do
    ForEachPoint(FStart, FEnd, @MoveTo, @LineTo);
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
  CalcAt(AXg, AXa, p, inExtent);
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
  ADomainExclusions: TIntervalList; ACalc: TTransformFunc; AStep: Integer);
begin
  inherited Create(ASeries, ACalc, AStep);
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


{ TPointsDrawFuncHelper }

type
  TBasicPointSeriesAccess = class(TBasicPointSeries);

constructor TPointsDrawFuncHelper.Create(
  ASeries: TBasicPointSeries; AMinX, AMaxX: Double; AStartIndex: Integer;
  ACalc: TTransformFunc; AStep: Integer);
begin
  inherited Create(ASeries, ACalc, AStep);
  FExtent.a.X := Min(AMinX, AMaxX);
  FExtent.b.X := Max(AMaxX, AMinX);
  FStartIndex := AStartIndex;
end;

procedure TPointsDrawFuncHelper.ForEachPoint(
  AXg, AXMax: Double; AOnMoveTo, AOnLineTo: TOnPoint);
var
  xa, xg1, xa1, dx, xfg: Double;
  j, n: Integer;
  ser: TBasicPointSeriesAccess;
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

  xa := FAxisToGraphXr(AXg);
  j := FStartIndex - ser.FLoBound;
  while (j < n) and (xa > ser.FGraphPoints[j].X) do inc(j);
  if j < n then xfg := ser.FGraphPoints[j].X else exit;
  AOnMoveTo(AXg, xa);

  while AXg < AXMax do begin
    xg1 := AXg + dx;
    xa1 := FGraphToAxisXr(xg1);
    if (j >= 0) and (xg1 > xfg) then begin
      xg1 := xfg;
      xa1 := ser.GetXValue(j + ser.FLoBound);
      inc(j);
      while (j < n) and (xfg > ser.FGraphPoints[j].X) do inc(j);   // skip unordered points
      if j < n then xfg := ser.FGraphPoints[j].X else xfg := Infinity;
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

