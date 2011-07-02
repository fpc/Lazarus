{

 Function series for TAChart.

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Authors: Alexander Klenin

}
unit TAFuncSeries;

{$H+}

interface

uses
  Classes, Graphics, typ,
  TAChartUtils, TACustomSeries, TACustomSource, TADrawUtils, TALegend, TATypes;

const
  DEF_FUNC_STEP = 2;
  DEF_SPLINE_DEGREE = 3;
  DEF_SPLINE_STEP = 4;
  DEF_COLORMAP_STEP = 4;

type
  TFuncCalculateEvent = procedure (const AX: Double; out AY: Double) of object;

  TFuncSeriesStep = 1..MaxInt;

  { TBasicFuncSeries }

  TBasicFuncSeries = class(TCustomChartSeries)
  private
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

  { TFuncSeries }

  TFuncSeries = class(TBasicFuncSeries)
  private
    FDomainExclusions: TIntervalList;
    FOnCalculate: TFuncCalculateEvent;
    FPen: TChartPen;
    FStep: TFuncSeriesStep;

    function DoCalcIdentity(AX: Double): Double;
    function DoCalculate(AX: Double): Double;
    procedure SetOnCalculate(const AValue: TFuncCalculateEvent);
    procedure SetPen(const AValue: TChartPen);
    procedure SetStep(AValue: TFuncSeriesStep);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;

  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Draw(ADrawer: IChartDrawer); override;
    function IsEmpty: Boolean; override;
  public
    property DomainExclusions: TIntervalList read FDomainExclusions;
  published
    property AxisIndexX;
    property AxisIndexY;
    property OnCalculate: TFuncCalculateEvent
      read FOnCalculate write SetOnCalculate;
    property Pen: TChartPen read FPen write SetPen;
    property Step: TFuncSeriesStep
      read FStep write SetStep default DEF_FUNC_STEP;
  end;

  TSplineDegree = 1..100;

  { TBSplineSeries }

  TBSplineSeries = class(TBasicPointSeries)
  private
    FDegree: TSplineDegree;
    FPen: TChartPen;
    FStep: TFuncSeriesStep;

    procedure SetDegree(AValue: TSplineDegree);
    procedure SetPen(AValue: TChartPen);
    procedure SetStep(AValue: TFuncSeriesStep);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;

  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Draw(ADrawer: IChartDrawer); override;
  published
    property Active default true;
    property AxisIndexX;
    property AxisIndexY;
    property ShowInLegend;
    property Source;
    property Title;
    property ZPosition;
  published
    property Degree: TSplineDegree
      read FDegree write SetDegree default DEF_SPLINE_DEGREE;
    property Pen: TChartPen read FPen write SetPen;
    property Pointer;
    property Step: TFuncSeriesStep
      read FStep write SetStep default DEF_SPLINE_STEP;
  end;

  TBadDataChartPen = class(TChartPen)
  published
    property Color default clRed;
  end;

  TCubicSplineOptions =
    set of (csoDrawFewPoints, csoDrawUnorderedX);

  { TCubicSplineSeries }

  TCubicSplineSeries = class(TBasicPointSeries)
  strict private
    FBadDataPen: TBadDataChartPen;
    FOptions: TCubicSplineOptions;
    FPen: TChartPen;
    FStep: TFuncSeriesStep;

    procedure SetPen(AValue: TChartPen);
    procedure SetStep(AValue: TFuncSeriesStep);
  strict private
    FUnorderedX: Boolean;
    FX, FY, FCoeff: array of ArbFloat;

    function Calculate(AX: Double): Double;
    procedure PrepareCoeffs;
    procedure SetBadDataPen(AValue: TBadDataChartPen);
    procedure SetOptions(AValue: TCubicSplineOptions);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    procedure SourceChanged(ASender: TObject); override;

  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Draw(ADrawer: IChartDrawer); override;
  published
    property Active default true;
    property AxisIndexX;
    property AxisIndexY;
    property Pointer;
    property ShowInLegend;
    property Source;
    property Title;
    property ZPosition;
  published
    // Used when data is not suitable for drawing cubic spline --
    // e.g. points are too few or not ordered by X value.
    property BadDataPen: TBadDataChartPen read FBadDataPen write SetBadDataPen;
    property Options: TCubicSplineOptions read FOptions write SetOptions;
    property Pen: TChartPen read FPen write SetPen;
    property Step: TFuncSeriesStep
      read FStep write SetStep default DEF_SPLINE_STEP;
  end;

  TFuncCalculate3DEvent =
    procedure (const AX, AY: Double; out AZ: Double) of object;

  { TColorMapSeries }

  TColorMapSeries = class(TBasicFuncSeries)
  private
    FBrush: TBrush;
    FColorSource: TCustomChartSource;
    FColorSourceListener: TListener;
    FInterpolate: Boolean;
    FOnCalculate: TFuncCalculate3DEvent;
    FStepX: TFuncSeriesStep;
    FStepY: TFuncSeriesStep;
    procedure SetBrush(AValue: TBrush);
    procedure SetColorSource(AValue: TCustomChartSource);
    procedure SetInterpolate(AValue: Boolean);
    procedure SetOnCalculate(AValue: TFuncCalculate3DEvent);
    procedure SetStepX(AValue: TFuncSeriesStep);
    procedure SetStepY(AValue: TFuncSeriesStep);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;

  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  public
    function ColorByValue(AValue: Double): TColor;
    procedure Draw(ADrawer: IChartDrawer); override;
    function IsEmpty: Boolean; override;
  published
    property AxisIndexX;
    property AxisIndexY;
    property Brush: TBrush read FBrush write SetBrush;
    property ColorSource: TCustomChartSource
      read FColorSource write SetColorSource;
    property Interpolate: Boolean
      read FInterpolate write SetInterpolate default false;
    property OnCalculate: TFuncCalculate3DEvent
      read FOnCalculate write SetOnCalculate;
    property StepX: TFuncSeriesStep
      read FStepX write SetStepX default DEF_COLORMAP_STEP;
    property StepY: TFuncSeriesStep
      read FStepY write SetStepY default DEF_COLORMAP_STEP;
  end;

implementation

uses
  ipf, Math, SysUtils, TAGeometry, TAGraph;

function DoublePointRotated(AX, AY: Double): TDoublePoint;
begin
  Result.X := AY;
  Result.Y := AX;
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

{ TFuncSeries }

procedure TFuncSeries.Assign(ASource: TPersistent);
begin
  if ASource is TFuncSeries then
    with TFuncSeries(ASource) do begin
      Self.FDomainExclusions.Assign(FDomainExclusions);
      Self.FOnCalculate := FOnCalculate;
      Self.Pen := FPen;
      Self.FStep := FStep;
    end;
  inherited Assign(ASource);
end;

constructor TFuncSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDomainExclusions := TIntervalList.Create;
  FDomainExclusions.OnChange := @StyleChanged;
  FPen := TChartPen.Create;
  FPen.OnChange := @StyleChanged;
  FStep := DEF_FUNC_STEP;
end;

destructor TFuncSeries.Destroy;
begin
  FreeAndNil(FDomainExclusions);
  FreeAndNil(FPen);
  inherited;
end;

function TFuncSeries.DoCalcIdentity(AX: Double): Double;
begin
  Result := AX;
end;

function TFuncSeries.DoCalculate(AX: Double): Double;
begin
  OnCalculate(AX, Result)
end;

type
  TCustomSeriesCrack = class(TCustomChartSeries);

procedure DrawFunction(
  ADrawer: IChartDrawer; ASeries: TCustomChartSeries;
  ADomainExclusions: TIntervalList; ACalc: TTransformFunc; AStep: Integer);
type
  TMakeDoublePoint = function (AX, AY: Double): TDoublePoint;

var
  axisToGraphXr, axisToGraphYr, graphToAxisXr: TTransformFunc;
  makeDP: TMakeDoublePoint;
  r: TDoubleRect = (coords:(NegInfinity, NegInfinity, Infinity, Infinity));
  prev: TDoublePoint;
  prevInExtent: Boolean;
  chart: TChart;

  procedure CalcAt(AXg, AXa: Double; out APt: TDoublePoint; out AIn: Boolean);
  begin
    APt := makeDP(AXg, axisToGraphYr(ACalc(AXa)));
    AIn := (r.a <= APt) and (APt <= r.b);
  end;

  procedure MoveTo(AXg, AXa: Double);
  begin
    CalcAt(AXg, AXa, prev, prevInExtent);
    if prevInExtent then
      ADrawer.MoveTo(chart.GraphToImage(prev));
  end;

  procedure LineTo(AXg, AXa: Double);
  var
    p, t: TDoublePoint;
    inExtent: Boolean;
  begin
    CalcAt(AXg, AXa, p, inExtent);
    t := p;
    if inExtent and prevInExtent then
      ADrawer.LineTo(chart.GraphToImage(p))
    else if LineIntersectsRect(prev, t, r) then begin
      ADrawer.MoveTo(chart.GraphToImage(prev));
      ADrawer.LineTo(chart.GraphToImage(t));
    end;
    prevInExtent := inExtent;
    prev := p;
  end;

var
  hint: Integer;
  xg, xa, xg1, xa1, xmax, graphStep: Double;
begin
  chart := TCustomSeriesCrack(ASeries).FChart;
  r := chart.CurrentExtent;

  with TCustomSeriesCrack(ASeries) do
    if IsRotated then begin
      axisToGraphXr := @AxisToGraphY;
      axisToGraphYr := @AxisToGraphX;
      graphToAxisXr := @GraphToAxisY;
      makeDP := @DoublePointRotated;
      graphStep := chart.YImageToGraph(-AStep) - chart.YImageToGraph(0);
      xg := r.a.Y;
      xmax := r.b.Y;
    end
    else begin
      axisToGraphXr := @AxisToGraphX;
      axisToGraphYr := @AxisToGraphY;
      graphToAxisXr := @GraphToAxisX;
      makeDP := @DoublePoint;
      graphStep := chart.XImageToGraph(AStep) - chart.XImageToGraph(0);
      xg := r.a.X;
      xmax := r.b.X;
    end;

  hint := 0;
  xa := graphToAxisXr(xg);
  if ADomainExclusions.Intersect(xa, xa, hint) then
    xg := axisToGraphXr(xa);

  MoveTo(xg, xa);

  while xg < xmax do begin
    xg1 := xg + graphStep;
    xa1 := graphToAxisXr(xg1);
    if ADomainExclusions.Intersect(xa, xa1, hint) then begin
      LineTo(axisToGraphXr(xa), xa);
      xg1 := axisToGraphXr(xa1);
      MoveTo(xg1, xa1);
    end
    else
      LineTo(xg1, xa1);
    xg := xg1;
    xa := xa1;
  end;
end;

procedure TFuncSeries.Draw(ADrawer: IChartDrawer);
var
  calc: TTransformFunc;
begin
  if Assigned(OnCalculate) then
    calc := @DoCalculate
  else if csDesigning in ComponentState then
    calc := @DoCalcIdentity
  else
    exit;
  ADrawer.Pen := Pen;
  DrawFunction(
    ADrawer, TCustomSeriesCrack(TCustomChartSeries(Self)),
    DomainExclusions, calc, Step);
end;

procedure TFuncSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemLine.Create(Pen, Title));
end;

function TFuncSeries.IsEmpty: Boolean;
begin
  Result := not Assigned(OnCalculate);
end;

procedure TFuncSeries.SetOnCalculate(const AValue: TFuncCalculateEvent);
begin
  if TMethod(FOnCalculate) = TMethod(AValue) then exit;
  FOnCalculate := AValue;
  UpdateParentChart;
end;

procedure TFuncSeries.SetPen(const AValue: TChartPen);
begin
  if FPen = AValue then exit;
  FPen.Assign(AValue);
  UpdateParentChart;
end;

procedure TFuncSeries.SetStep(AValue: TFuncSeriesStep);
begin
  if FStep = AValue then exit;
  FStep := AValue;
  UpdateParentChart;
end;

{ TBSplineSeries }

procedure TBSplineSeries.Assign(ASource: TPersistent);
begin
  if ASource is TBSplineSeries then
    with TBSplineSeries(ASource) do begin
      Self.FDegree := FDegree;
      Self.Pen := FPen;
      Self.FStep := FStep;
    end;
  inherited Assign(ASource);
end;

constructor TBSplineSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDegree := DEF_SPLINE_DEGREE;
  FPen := TChartPen.Create;
  FPen.OnChange := @StyleChanged;
  FPointer := TSeriesPointer.Create(ParentChart);
  FStep := DEF_SPLINE_STEP;
end;

destructor TBSplineSeries.Destroy;
begin
  FreeAndNil(FPen);
  inherited;
end;

procedure TBSplineSeries.Draw(ADrawer: IChartDrawer);
var
  p: array of TDoublePoint;
  startIndex: Integer;

  function SplinePoint(APos: Double): TPoint;
  var
    i, d: Integer;
    w, denom: Double;
  begin
    // Duplicate end points Degree times to fix spline to them.
    for i := 0 to Degree do
      p[i] := FGraphPoints[
        EnsureRange(startIndex - Degree + i, 0, High(FGraphPoints))];
    // De Boor's algorithm, source points used as control points.
    // Parametric coordinate is equal to point index.
    for d := 1 to Degree do begin
      denom := 1 / (Degree + 1 - d);
      for i := Degree downto d do begin
        w := (APos + Degree - i) * denom;
        p[i].X := WeightedAverage(p[i - 1].X, p[i].X, w);
        p[i].Y := WeightedAverage(p[i - 1].Y, p[i].Y, w);
      end;
    end;
    Result := ParentChart.GraphToImage(p[Degree]);
  end;

var
  level: Integer = 0;

  // Pass screen coordinates down to calculate them only once for each point.
  procedure SplineSegment(AL, AR: Double; const APL, APR: TPoint);
  const
    INF_SENTINEL = 15; // Arbitrary guard against infinite recursion.
  var
    m: Double;
    pm: TPoint;
  begin
    if (level > INF_SENTINEL) or (PointDist(APL, APR) <= Sqr(Step)) then
      // Left-then-right recursive call order guarantees that
      // the last drawn segment is the immediately preceding one.
      ADrawer.LineTo(APR)
    else begin
      m := (AL + AR) / 2;
      pm := SplinePoint(m);
      level += 1;
      SplineSegment(AL, m, APL, pm);
      SplineSegment(m, AR, pm, APR);
      level -= 1;
    end;
  end;

var
  ext: TDoubleRect;
begin
  if IsEmpty then exit;

  with Extent do begin
    ext.a := AxisToGraph(a);
    ext.b := AxisToGraph(b);
  end;
  NormalizeRect(ext);
  ExpandRange(ext.a.X, ext.b.X, 1.0);
  ExpandRange(ext.a.Y, ext.b.Y, 1.0);
  PrepareGraphPoints(ext, true);

  SetLength(p, Degree + 1);
  ADrawer.Pen := Pen;
  ADrawer.MoveTo(ParentChart.GraphToImage(FGraphPoints[0]));
  for startIndex := 0 to High(FGraphPoints) + Degree - 1 do
    SplineSegment(0.0, 1.0, SplinePoint(0.0), SplinePoint(1.0));
  DrawLabels(ADrawer);
  DrawPointers(ADrawer);
end;

procedure TBSplineSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemLine.Create(Pen, Title));
end;

procedure TBSplineSeries.SetDegree(AValue: TSplineDegree);
begin
  if FDegree = AValue then exit;
  FDegree := AValue;
  UpdateParentChart;
end;

procedure TBSplineSeries.SetPen(AValue: TChartPen);
begin
  if FPen = AValue then exit;
  FPen.Assign(AValue);
  UpdateParentChart;
end;

procedure TBSplineSeries.SetStep(AValue: TFuncSeriesStep);
begin
  if FStep = AValue then exit;
  FStep := AValue;
  UpdateParentChart;
end;

{ TCubicSplineSeries }

procedure TCubicSplineSeries.Assign(ASource: TPersistent);
begin
  if ASource is TCubicSplineSeries then
    with TCubicSplineSeries(ASource) do begin
      Self.Pen := FPen;
      Self.FStep := FStep;
    end;
  inherited Assign(ASource);
end;

function TCubicSplineSeries.Calculate(AX: Double): Double;
var
  ok: Integer = 0;
begin
  Result := ipfspn(High(FCoeff), FX[0], FY[0], FCoeff[0], AX, ok);
end;

constructor TCubicSplineSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBadDataPen := TBadDataChartPen.Create;
  FBadDataPen.Color := clRed;
  FBadDataPen.OnChange := @StyleChanged;
  FPen := TChartPen.Create;
  FPen.OnChange := @StyleChanged;
  FPointer := TSeriesPointer.Create(ParentChart);
  FStep := DEF_SPLINE_STEP;
end;

destructor TCubicSplineSeries.Destroy;
begin
  FreeAndNil(FBadDataPen);
  FreeAndNil(FPen);
  inherited;
end;

procedure TCubicSplineSeries.Draw(ADrawer: IChartDrawer);

  function DrawFewPoints: Boolean;
  const
    MIN_SPLINE_POINTS = 4;
  var
    pts: TPointArray;
    i: Integer;
  begin
    Result := Length(FX) < MIN_SPLINE_POINTS;
    if
      not Result or not (csoDrawFewPoints in Options) or not BadDataPen.Visible
    then
      exit;
    SetLength(pts, Length(FGraphPoints));
    for i := 0 to High(FGraphPoints) do
      pts[i] := ParentChart.GraphToImage(FGraphPoints[i]);
    ADrawer.Pen := BadDataPen;
    ADrawer.Polyline(pts, 0, Length(pts));
  end;

  procedure DrawSpline;
  var
    de: TIntervalList;
    p: TChartPen;
  begin
    if FCoeff = nil then exit;
    if FUnorderedX then begin
      if csoDrawUnorderedX in Options then
        p := BadDataPen
      else
        exit;
    end
    else
      p := Pen;
    if not p.Visible then exit;
    ADrawer.Pen := p;
    de := TIntervalList.Create;
    try
      DrawFunction(ADrawer, Self, de, @Calculate, Step);
    finally
      de.Free;
    end;
  end;

begin
  if FCoeff = nil then
    PrepareCoeffs;

  PrepareGraphPoints(FChart.CurrentExtent, true);
  if not DrawFewPoints then
    DrawSpline;

  DrawLabels(ADrawer);
  DrawPointers(ADrawer);
end;

procedure TCubicSplineSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemLine.Create(Pen, Title));
end;

procedure TCubicSplineSeries.PrepareCoeffs;
var
  i, n: Integer;
begin
  n := Source.Count;
  SetLength(FX, n);
  SetLength(FY, n);
  SetLength(FCoeff, n);
  FUnorderedX := false;
  n := 0;
  for i := 0 to Source.Count - 1 do
    with Source[i]^ do
      if (i > 0) and (FX[n - 1] >= X) then
        FUnorderedX := true
      else begin
        FX[n] := X;
        FY[n] := Y;
        n += 1;
      end;
  SetLength(FX, n);
  SetLength(FY, n);
  SetLength(FCoeff, n);
  ipfisn(n - 1, FX[0], FY[0], FCoeff[0], i);
  if i > 1 then
    FCoeff := nil;
end;

procedure TCubicSplineSeries.SetBadDataPen(AValue: TBadDataChartPen);
begin
  if FBadDataPen = AValue then exit;
  FBadDataPen.Assign(AValue);
  UpdateParentChart;
end;

procedure TCubicSplineSeries.SetOptions(AValue: TCubicSplineOptions);
begin
  if FOptions = AValue then exit;
  FOptions := AValue;
  FCoeff := nil;
  UpdateParentChart;
end;

procedure TCubicSplineSeries.SetPen(AValue: TChartPen);
begin
  if FPen = AValue then exit;
  FPen.Assign(AValue);
  UpdateParentChart;
end;

procedure TCubicSplineSeries.SetStep(AValue: TFuncSeriesStep);
begin
  if FStep = AValue then exit;
  FStep := AValue;
  UpdateParentChart;
end;

procedure TCubicSplineSeries.SourceChanged(ASender: TObject);
begin
  inherited SourceChanged(ASender);
  FCoeff := nil;
end;

{ TColorMapSeries }

procedure TColorMapSeries.Assign(ASource: TPersistent);
begin
  if ASource is TColorMapSeries then
    with TColorMapSeries(ASource) do begin
      Self.Brush := FBrush;
      Self.ColorSource := FColorSource;
      Self.FInterpolate := FInterpolate;
      Self.FOnCalculate := FOnCalculate;
      Self.FStepX := FStepX;
      Self.FStepY := FStepY;
    end;
  inherited Assign(ASource);
end;

function TColorMapSeries.ColorByValue(AValue: Double): TColor;
var
  lb, ub: Integer;
  c1, c2: TColor;
  v1, v2: Double;
begin
  if ColorSource = nil then exit(clTAColor);
  ColorSource.FindBounds(AValue, SafeInfinity, lb, ub);
  if Interpolate and InRange(lb, 1, ColorSource.Count - 1) then begin
    with ColorSource[lb - 1]^ do begin
      v1 := X;
      c1 := Color;
    end;
    with ColorSource[lb]^ do begin
      v2 := X;
      c2 := Color;
    end;
    if v2 <= v1 then
      Result := c1
    else
      Result := InterpolateRGB(c1, c2, (AValue - v1) / (v2 - v1));
  end
  else
    Result := ColorSource[EnsureRange(lb, 0, ColorSource.Count - 1)]^.Color;
end;

constructor TColorMapSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorSourceListener := TListener.Create(@FColorSource, @StyleChanged);
  FBrush := TBrush.Create;
  FBrush.OnChange := @StyleChanged;
  FStepX := DEF_COLORMAP_STEP;
  FStepY := DEF_COLORMAP_STEP;
end;

destructor TColorMapSeries.Destroy;
begin
  FreeAndNil(FColorSourceListener);
  FreeAndNil(FBrush);
  inherited Destroy;
end;

procedure TColorMapSeries.Draw(ADrawer: IChartDrawer);
var
  ext: TDoubleRect;
  bounds: TDoubleRect;
  r: TRect;
  pt, next, offset: TPoint;
  gp: TDoublePoint;
  v: Double;
begin
  if not (csDesigning in ComponentState) and IsEmpty then exit;

  ext := ParentChart.CurrentExtent;
  bounds := EmptyExtent;
  GetBounds(bounds);
  bounds.a := AxisToGraph(bounds.a);
  bounds.b := AxisToGraph(bounds.b);
  if not RectIntersectsRect(ext, bounds) then exit;

  r.TopLeft := ParentChart.GraphToImage(ext.a);
  r.BottomRight := ParentChart.GraphToImage(ext.b);
  NormalizeRect(r);
  offset := ParentChart.GraphToImage(ZeroDoublePoint);

  ADrawer.Brush := Brush;
  ADrawer.SetPenParams(psClear, clTAColor);
  pt.Y := (r.Top div StepY - 1) * StepY + offset.Y mod StepY;
  while pt.Y <= r.Bottom do begin
    next.Y := pt.Y + StepY;
    if next.Y <= r.Top then begin
      pt.Y := next.Y;
      continue;
    end;
    pt.X := (r.Left div StepX  - 1) * StepX + offset.X mod StepX;
    while pt.X <= r.Right do begin
      next.X := pt.X + StepX;
      if next.X <= r.Left then begin
        pt.X := next.X;
        continue;
      end;
      gp := GraphToAxis(ParentChart.ImageToGraph((pt + next) div 2));
      if not (csDesigning in ComponentState) then
        OnCalculate(gp.X, gp.Y, v);
      if ColorSource <> nil then
        ADrawer.BrushColor := ColorByValue(v);
      ADrawer.Rectangle(
        Max(pt.X, r.Left), Max(pt.Y, r.Top),
        Min(next.X, r.Right) + 1, Min(next.Y, r.Bottom) + 1);
      pt.X := next.X;
    end;
    pt.Y := next.Y;
  end;
end;

procedure TColorMapSeries.GetLegendItems(AItems: TChartLegendItems);
var
  i: Integer;
  prev: Double;

  function ItemTitle(const AText: String; AX: Double): String;
  const
    FORMATS: array [1..3] of String = ('z ≤ %1:g', '%g < z ≤ %g', '%g < z');
  var
    idx: Integer;
  begin
    if AText <> '' then exit(AText);
    if ColorSource.Count = 1 then exit('');
    if i = 0 then idx := 1
    else if i = ColorSource.Count - 1 then idx := 3
    else idx := 2;
    Result := Format(FORMATS[idx], [prev, AX]);
  end;

var
  li: TLegendItemBrushRect;
begin
  case Legend.Multiplicity of
    lmSingle: AItems.Add(TLegendItemBrushRect.Create(Brush, Title));
    lmPoint:
      if ColorSource <> nil then begin
        prev := 0.0;
        for i := 0 to ColorSource.Count - 1 do
          with ColorSource[i]^ do begin
            li := TLegendItemBrushRect.Create(Brush, ItemTitle(Text, X));
            li.Color := Color;
            AItems.Add(li);
            prev := X;
          end;
      end;
  end;
end;

function TColorMapSeries.IsEmpty: Boolean;
begin
  Result := not Assigned(OnCalculate);
end;

procedure TColorMapSeries.SetBrush(AValue: TBrush);
begin
  if FBrush = AValue then exit;
  FBrush := AValue;
  UpdateParentChart;
end;

procedure TColorMapSeries.SetColorSource(AValue: TCustomChartSource);
begin
  if FColorSource = AValue then exit;
  if FColorSourceListener.IsListening then
    ColorSource.Broadcaster.Unsubscribe(FColorSourceListener);
  FColorSource := AValue;
  ColorSource.Broadcaster.Subscribe(FColorSourceListener);
  UpdateParentChart;
end;

procedure TColorMapSeries.SetInterpolate(AValue: Boolean);
begin
  if FInterpolate = AValue then exit;
  FInterpolate := AValue;
  UpdateParentChart;
end;

procedure TColorMapSeries.SetOnCalculate(AValue: TFuncCalculate3DEvent);
begin
  if TMethod(FOnCalculate) = TMethod(AValue) then exit;
  FOnCalculate := AValue;
  UpdateParentChart;
end;

procedure TColorMapSeries.SetStepX(AValue: TFuncSeriesStep);
begin
  if FStepX = AValue then exit;
  FStepX := AValue;
  UpdateParentChart;
end;

procedure TColorMapSeries.SetStepY(AValue: TFuncSeriesStep);
begin
  if FStepY = AValue then exit;
  FStepY := AValue;
  UpdateParentChart;
end;

initialization
  RegisterSeriesClass(TFuncSeries, 'Function series');
  RegisterSeriesClass(TBSplineSeries, 'B-Spline series');
  RegisterSeriesClass(TCubicSplineSeries, 'Cubic spline series');
  RegisterSeriesClass(TColorMapSeries, 'Color map series');

end.

