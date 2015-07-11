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

  TDrawFuncHelper = class
  strict private
  type
    TOnPoint = procedure (AXg, AXa: Double) of object;
  var
    FAxisToGraphXr, FAxisToGraphYr, FGraphToAxisXr: TTransformFunc;
    FCalc: TTransformFunc;
    FChart: TChart;
    FDomainExclusions: TIntervalList;
    FDrawer: IChartDrawer;
    FExtent: TDoubleRect;
    FExtentYMax: PDouble;
    FExtentYMin: PDouble;
    FGraphStep: Double;
    FImageToGraph: TImageToGraphFunc;
    FNearestPointParams: ^TNearestPointParams;
    FNearestPointResults: ^TNearestPointResults;
    FMakeDP: TMakeDoublePoint;
    FPrev: TDoublePoint;
    FPrevInExtent: Boolean;
    FSeries: TCustomChartSeries;

    procedure CalcAt(AXg, AXa: Double; out APt: TDoublePoint; out AIn: Boolean);
    procedure CheckForNearestPoint(AXg, AXa: Double);
    procedure ForEachPoint(AXg, AXMax: Double; AOnMoveTo, AOnLineTo: TOnPoint);
    procedure LineTo(AXg, AXa: Double);
    procedure MoveTo(AXg, AXa: Double);
    procedure UpdateExtent(AXg, AXa: Double);
    function XRange: TDoubleInterval;
  public
    constructor Create(
      ASeries: TCustomChartSeries; ADomainExclusions:
      TIntervalList; ACalc: TTransformFunc; AStep: Integer);
    procedure CalcAxisExtentY(AMinX, AMaxX: Double; var AMinY, AMaxY: Double);
    procedure DrawFunction(ADrawer: IChartDrawer);
    function GetNearestPoint(
      const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean;
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

{ TDrawFuncHelper }

procedure TDrawFuncHelper.CalcAt(
  AXg, AXa: Double; out APt: TDoublePoint; out AIn: Boolean);
begin
  APt := FMakeDP(AXg, FAxisToGraphYr(FCalc(AXa)));
  AIn := (FExtent.a <= APt) and (APt <= FExtent.b);
end;

procedure TDrawFuncHelper.CalcAxisExtentY(
  AMinX, AMaxX: Double; var AMinY, AMaxY: Double);
begin
  FExtentYMin := @AMinY;
  FExtentYMax := @AMaxY;
  with XRange do
    ForEachPoint(AMinX, AMaxX, @UpdateExtent, @UpdateExtent);
end;

procedure TDrawFuncHelper.CheckForNearestPoint(AXg, AXa: Double);
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

constructor TDrawFuncHelper.Create(
  ASeries: TCustomChartSeries; ADomainExclusions: TIntervalList;
  ACalc: TTransformFunc; AStep: Integer);
begin
  FChart := ASeries.ParentChart;
  FExtent := FChart.CurrentExtent;
  FSeries := ASeries;
  FDomainExclusions := ADomainExclusions;
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

procedure TDrawFuncHelper.DrawFunction(ADrawer: IChartDrawer);
begin
  FDrawer := ADrawer;
  with XRange do
    ForEachPoint(FStart, FEnd, @MoveTo, @LineTo);
end;

procedure TDrawFuncHelper.ForEachPoint(
  AXg, AXMax: Double; AOnMoveTo, AOnLineTo: TOnPoint);
var
  hint: Integer;
  xa, xg1, xa1, dx: Double;
begin
  if FGraphStep = 0 then exit;

  hint := 0;
  xa := FGraphToAxisXr(AXg);
  if FDomainExclusions.Intersect(xa, xa, hint) then
    AXg := FAxisToGraphXr(xa);

  if AXg < AXMax then
    AOnMoveTo(AXg, xa);

  dx := abs(FGraphStep);
  while AXg < AXMax do begin
    xg1 := AXg + dx;
    xa1 := FGraphToAxisXr(xg1);
    if FDomainExclusions.Intersect(xa, xa1, hint) then begin
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

function TDrawFuncHelper.GetNearestPoint(
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

procedure TDrawFuncHelper.LineTo(AXg, AXa: Double);
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

procedure TDrawFuncHelper.MoveTo(AXg, AXa: Double);
begin
  CalcAt(AXg, AXa, FPrev, FPrevInExtent);
  if FPrevInExtent then
    FDrawer.MoveTo(FChart.GraphToImage(FPrev));
end;

procedure TDrawFuncHelper.UpdateExtent(AXg, AXa: Double);
begin
  Unused(AXg);
  UpdateMinMax(FCalc(AXa), FExtentYMin^, FExtentYMax^);
end;

function TDrawFuncHelper.XRange: TDoubleInterval;
begin
  if FSeries.IsRotated then
    Result := DoubleInterval(FExtent.a.Y, FExtent.b.Y)
  else
    Result := DoubleInterval(FExtent.a.X, FExtent.b.X);
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

