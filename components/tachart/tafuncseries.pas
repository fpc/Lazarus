{

 Function series for TAChart.

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Alexander Klenin

}
unit TAFuncSeries;

{$H+}

interface

uses
  Classes, Graphics, typ, Types,
  TAChartUtils, TACustomFuncSeries, TACustomSeries, TACustomSource,
  TADrawUtils, TAFitUtils, TALegend, TATypes;

const
  DEF_FUNC_STEP = 2;
  DEF_SPLINE_DEGREE = 3;
  DEF_SPLINE_STEP = 4;
  DEF_FIT_STEP = 4;
  DEF_FIT_PARAM_COUNT = 3;
  DEF_COLORMAP_STEP = 4;

type
  TFuncCalculateEvent = procedure (const AX: Double; out AY: Double) of object;

  TFuncSeriesStep = 1..MaxInt;

  TFuncSeries = class(TBasicFuncSeries)
  strict private
    FDomainExclusions: TIntervalList;
    FExtentAutoY: Boolean;
    FOnCalculate: TFuncCalculateEvent;
    FPen: TChartPen;
    FStep: TFuncSeriesStep;

    function DoCalcIdentity(AX: Double): Double;
    function DoCalculate(AX: Double): Double;
    procedure SetExtentAutoY(AValue: Boolean);
    procedure SetOnCalculate(AValue: TFuncCalculateEvent);
    procedure SetPen(AValue: TChartPen);
    procedure SetStep(AValue: TFuncSeriesStep);
  protected
    procedure GetBounds(var ABounds: TDoubleRect); override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;

  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Draw(ADrawer: IChartDrawer); override;
    function GetNearestPoint(
      const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
    function IsEmpty: Boolean; override;
  public
    property DomainExclusions: TIntervalList read FDomainExclusions;
  published
    property AxisIndexX;
    property AxisIndexY;
    property OnCalculate: TFuncCalculateEvent
      read FOnCalculate write SetOnCalculate;
    property ExtentAutoY: Boolean
      read FExtentAutoY write SetExtentAutoY default false;
    property Pen: TChartPen read FPen write SetPen;
    property Step: TFuncSeriesStep
      read FStep write SetStep default DEF_FUNC_STEP;
  end;

  TParametricCurveCalculateEvent = procedure (
    const AT: Double; out AX, AY: Double) of object;

  TParametricCurveSeries = class(TBasicFuncSeries)
  strict private
    FOnCalculate: TParametricCurveCalculateEvent;
    FParamMax: Double;
    FParamMaxStep: Double;
    FParamMin: Double;
    FPen: TChartPen;
    FStep: TFuncSeriesStep;

    function DoCalcIdentity(AT: Double): TDoublePoint;
    function DoCalculate(AT: Double): TDoublePoint;
    function ParamMaxIsStored: Boolean;
    function ParamMaxStepIsStored: Boolean;
    function ParamMinIsStored: Boolean;
    procedure SetOnCalculate(AValue: TParametricCurveCalculateEvent);
    procedure SetParamMax(AValue: Double);
    procedure SetParamMaxStep(AValue: Double);
    procedure SetParamMin(AValue: Double);
    procedure SetPen(AValue: TChartPen);
    procedure SetStep(AValue: TFuncSeriesStep);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;

  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Draw(ADrawer: IChartDrawer); override;
    function IsEmpty: Boolean; override;
  published
    property AxisIndexX;
    property AxisIndexY;
    property OnCalculate: TParametricCurveCalculateEvent
      read FOnCalculate write SetOnCalculate;
    property ParamMax: Double read FParamMax write SetParamMax
      stored ParamMaxIsStored;
    property ParamMaxStep: Double
      read FParamMaxStep write SetParamMaxStep stored ParamMaxStepIsStored;
    property ParamMin: Double
      read FParamMin write SetParamMin stored ParamMinIsStored;
    property Pen: TChartPen read FPen write SetPen;
    property Step: TFuncSeriesStep
      read FStep write SetStep default DEF_FUNC_STEP;
  end;

  TSplineDegree = 1..100;

  { TBSplineSeries }

  TBSplineSeries = class(TBasicPointSeries)
  strict private
    FDegree: TSplineDegree;
    FPen: TChartPen;
    FStep: TFuncSeriesStep;

    procedure InternalPrepareGraphPoints;
    procedure SetDegree(AValue: TSplineDegree);
    procedure SetPen(AValue: TChartPen);
    procedure SetStep(AValue: TFuncSeriesStep);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;

  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Calculate(AX: Double): Double;
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

  TCubicSplineOptions = set of (
    csoDrawFewPoints, csoDrawUnorderedX, csoExtrapolateLeft,
    csoExtrapolateRight);

  TCubicSplineSeries = class(TBasicPointSeries)
  strict private
    FBadDataPen: TBadDataChartPen;
    FOptions: TCubicSplineOptions;
    FPen: TChartPen;
    FStep: TFuncSeriesStep;

    procedure SetPen(AValue: TChartPen);
    procedure SetStep(AValue: TFuncSeriesStep);
  strict private
  type
    TSpline = class
    public
      FCoeff, FX, FY: array of ArbFloat;
      FIntervals: TIntervalList;
      FIsUnorderedX: Boolean;
      FStartIndex: Integer;

      destructor Destroy; override;
      function Calculate(AX: Double): Double;
      function IsFewPoints: Boolean; inline;
      function PrepareCoeffs(
        ASource: TCustomChartSource; var AIndex: Integer): Boolean;
      procedure PrepareIntervals(AOptions: TCubicSplineOptions);
    end;

  var
    FSplines: array of TSpline;

    procedure FreeSplines;
    function IsUnorderedVisible: Boolean; inline;
    function IsFewPointsVisible: Boolean; inline;
    procedure PrepareCoeffs;
    procedure SetBadDataPen(AValue: TBadDataChartPen);
    procedure SetOptions(AValue: TCubicSplineOptions);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    procedure SourceChanged(ASender: TObject); override;

  public
    procedure Assign(ASource: TPersistent); override;
    function Calculate(AX: Double): Double;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Draw(ADrawer: IChartDrawer); override;
    function Extent: TDoubleRect; override;
    function GetNearestPoint(
      const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
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
    property Options: TCubicSplineOptions
      read FOptions write SetOptions default [];
    property Pen: TChartPen read FPen write SetPen;
    property Step: TFuncSeriesStep
      read FStep write SetStep default DEF_SPLINE_STEP;
  end;

  TFitParamsState = (fpsUnknown, fpsInvalid, fpsValid);

  TFitSeries = class(TBasicPointSeries)
  strict private
    FDrawFitRangeOnly: Boolean;
    FFitEquation: TFitEquation;
    FFitParams: TDoubleDynArray;
    FFitRange: TChartRange;
    FOnFitComplete: TNotifyEvent;
    FPen: TChartPen;
    FState: TFitParamsState;
    FStep: TFuncSeriesStep;
    function GetParam(AIndex: Integer): Double;
    function GetParamCount: Integer;
    function PrepareIntervals: TIntervalList;
    procedure SetDrawFitRangeOnly(AValue: Boolean);
    procedure SetFitEquation(AValue: TFitEquation);
    procedure SetFitRange(AValue: TChartRange);
    procedure SetParam(AIndex: Integer; AValue: Double);
    procedure SetParamCount(AValue: Integer);
    procedure SetPen(AValue: TChartPen);
    procedure SetStep(AValue: TFuncSeriesStep);
  strict protected
    procedure CalcXRange(out AXMin, AXMax: Double);
    procedure Transform(AX, AY: Double; out ANewX, ANewY: Extended);
  protected
    procedure AfterAdd; override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    procedure SourceChanged(ASender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function Calculate(AX: Double): Double; virtual;
    procedure Draw(ADrawer: IChartDrawer); override;
    procedure ExecFit; virtual;
    function EquationText: IFitEquationText;
    function GetFitEquationString(
      ANumFormat: String; AXText: String = 'x'; AYText: String = 'y'): String;
      deprecated 'Use EquationText';
    function GetNearestPoint(
      const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
    property Param[AIndex: Integer]: Double read GetParam write SetParam;
    property State: TFitParamsState read FState;
  published
    property AxisIndexX;
    property AxisIndexY;
    property DrawFitRangeOnly: Boolean
      read FDrawFitRangeOnly write SetDrawFitRangeOnly default true;
    property FitEquation: TFitEquation read FFitEquation write SetFitEquation default fePolynomial;
    property FitRange: TChartRange read FFitRange write SetFitRange;
    property OnFitComplete: TNotifyEvent read FOnFitComplete write FOnFitComplete;
    property ParamCount: Integer
      read GetParamCount write SetParamCount default DEF_FIT_PARAM_COUNT;
    property Pen: TChartPen read FPen write SetPen;
    property Source;
    property Step: TFuncSeriesStep read FStep write SetStep default DEF_FIT_STEP;
  end;

  TFuncCalculate3DEvent =
    procedure (const AX, AY: Double; out AZ: Double) of object;


  TColorMapSeries = class(TBasicFuncSeries)
  public
  type
    TUseImage = (cmuiAuto, cmuiAlways, cmuiNever);
  strict private
    FBrush: TBrush;
    FColorSource: TCustomChartSource;
    FColorSourceListener: TListener;
    FInterpolate: Boolean;
    FOnCalculate: TFuncCalculate3DEvent;
    FStepX: TFuncSeriesStep;
    FStepY: TFuncSeriesStep;
    FUseImage: TUseImage;
    procedure SetBrush(AValue: TBrush);
    procedure SetColorSource(AValue: TCustomChartSource);
    procedure SetInterpolate(AValue: Boolean);
    procedure SetOnCalculate(AValue: TFuncCalculate3DEvent);
    procedure SetStepX(AValue: TFuncSeriesStep);
    procedure SetStepY(AValue: TFuncSeriesStep);
    procedure SetUseImage(AValue: TUseImage);
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
    property UseImage: TUseImage
      read FUseImage write SetUseImage default cmuiAuto;
  end;

  // Builds an equation string based on the parameters and the type of equation.
  // AXText and AYText are placeholders for the x and y variables, respectively.
  // Parameters are formatted by passing ANumFormat to the "Format" function.
  function ParamsToEquation(
    AEquation: TFitEquation; const AParams: array of Double;
    ANumFormat: String; AXText: String = 'x'; AYText: String = 'y'): String;
    deprecated 'Use IFitEquationText';

implementation

uses
  ipf, GraphType, IntfGraphics, Math, StrUtils, SysUtils,
  TAGeometry, TAGraph, TAMath;

const
  DEF_PARAM_MIN = 0.0;
  DEF_PARAM_MAX = 1.0;

type
  TFitSeriesRange = class(TChartRange)
  strict private
    FSeries: TFitSeries;
  strict protected
    procedure StyleChanged(ASender: TObject); override;
  public
    constructor Create(ASeries: TFitSeries);
  end;

  TLegendItemColorMap = class(TLegendItem)
  strict private
    FColor2: TColor;
    FFramePen: TChartPen;
  public
    constructor Create(
      AColor1, AColor2: TColor; AFramePen: TChartPen; const AText: String);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
  end;

  TParametricFunc = function (A: Double): TDoublePoint of object;

function ParamsToEquation(
  AEquation: TFitEquation; const AParams: array of Double;
  ANumFormat, AXText, AYText: String): String;
begin
  Result :=
    TFitEquationText.Create.Equation(AEquation).
    X(AXText).Y(AYText).NumFormat(ANumFormat).Params(AParams);
end;

{ TColorMapLegendItem }

constructor TLegendItemColorMap.Create(
  AColor1, AColor2: TColor; AFramePen: TChartPen; const AText: String);
begin
  inherited Create(AText);
  Color := AColor1;
  FColor2 := AColor2;
  FFramePen := AFramePen;
end;

procedure TLegendItemColorMap.Draw(ADrawer: IChartDrawer; const ARect: TRect);
var
  x, w, pw: Integer;
  c: TColor;
begin
  inherited Draw(ADrawer, ARect);
  with FFramePen do
    pw := IfThen(EffVisible, (Width + 1) div 2, 0);
  w := ARect.Right - ARect.Left - 2 * pw;
  if w <= 0 then exit;
  for x := ARect.Left + pw to ARect.Right - pw do begin
    c := InterpolateRGB(Color, FColor2, (x - ARect.Left - pw) / w);
    ADrawer.SetPenParams(psSolid, c);
    ADrawer.Line(x, ARect.Top, x, ARect.Bottom - 1);
  end;
  if pw > 0 then begin
    ADrawer.Pen := FFramePen;
    ADrawer.SetBrushParams(bsClear, clTAColor);
    ADrawer.Rectangle(ARect);
  end;
end;

{ TFitSeriesRange }

constructor TFitSeriesRange.Create(ASeries: TFitSeries);
begin
  inherited Create(ASeries.ParentChart);
  FSeries := ASeries;
end;

procedure TFitSeriesRange.StyleChanged(ASender: TObject);
begin
  FSeries.SourceChanged(nil); // reset FState to fpsUnknown
  FSeries.ExecFit;
  inherited;
end;

{ TFuncSeries }

procedure TFuncSeries.Assign(ASource: TPersistent);
begin
  if ASource is TFuncSeries then
    with TFuncSeries(ASource) do begin
      Self.FDomainExclusions.Assign(FDomainExclusions);
      Self.FExtentAutoY := FExtentAutoY;
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
  with TDrawFuncHelper.Create(Self, DomainExclusions, calc, Step) do
    try
      DrawFunction(ADrawer);
    finally
      Free;
    end;
end;

procedure TFuncSeries.GetBounds(var ABounds: TDoubleRect);
var
  ymin, ymax: Double;
begin
  inherited GetBounds(ABounds);
  if
    not Extent.UseXMin or not Extent.UseXMax or not ExtentAutoY
    or not Assigned(OnCalculate)
  then
    exit;
  ymin := SafeInfinity;
  ymax := NegInfinity;
  with TDrawFuncHelper.Create(Self, DomainExclusions, @DoCalculate, Step) do
    try
      CalcAxisExtentY(ABounds.a.X, ABounds.b.X, ymin, ymax);
      if not Extent.UseYMin or (ymin > Extent.YMin) then
        ABounds.a.Y := ymin;
      if not Extent.UseYMax or (ymax < Extent.YMax) then
        ABounds.b.Y := ymax;
    finally
      Free;
    end;
end;

procedure TFuncSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemLine.Create(Pen, LegendTextSingle));
end;

function TFuncSeries.GetNearestPoint(
  const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
begin
  AResults.FIndex := -1;
  if not Assigned(OnCalculate) then exit(false);

  with TDrawFuncHelper.Create(Self, DomainExclusions, @DoCalculate, Step) do
    try
      Result := GetNearestPoint(AParams, AResults);
    finally
      Free;
    end;
end;

function TFuncSeries.IsEmpty: Boolean;
begin
  Result := not Assigned(OnCalculate);
end;

procedure TFuncSeries.SetExtentAutoY(AValue: Boolean);
begin
  if FExtentAutoY = AValue then exit;
  FExtentAutoY := AValue;
  UpdateParentChart;
end;

procedure TFuncSeries.SetOnCalculate(AValue: TFuncCalculateEvent);
begin
  if TMethod(FOnCalculate) = TMethod(AValue) then exit;
  FOnCalculate := AValue;
  UpdateParentChart;
end;

procedure TFuncSeries.SetPen(AValue: TChartPen);
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

{ TParametricCurveSeries }

procedure TParametricCurveSeries.Assign(ASource: TPersistent);
begin
  if ASource is TFuncSeries then
    with TFuncSeries(ASource) do begin
      Self.FOnCalculate := FOnCalculate;
      Self.FParamMax := FParamMax;
      Self.FParamMin := FParamMin;
      Self.Pen := FPen;
      Self.FStep := FStep;
    end;
  inherited Assign(ASource);
end;

constructor TParametricCurveSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParamMin := DEF_PARAM_MIN;
  FParamMax := DEF_PARAM_MAX;
  FPen := TChartPen.Create;
  FPen.OnChange := @StyleChanged;
  FStep := DEF_FUNC_STEP;
end;

destructor TParametricCurveSeries.Destroy;
begin
  FreeAndNil(FPen);
  inherited;
end;

function TParametricCurveSeries.DoCalcIdentity(AT: Double): TDoublePoint;
begin
  Result := DoublePoint(AT, AT);
end;

function TParametricCurveSeries.DoCalculate(AT: Double): TDoublePoint;
begin
  OnCalculate(AT, Result.X, Result.Y);
end;

procedure TParametricCurveSeries.Draw(ADrawer: IChartDrawer);
var
  calc: TParametricFunc;

  function PointAt(AT: Double): TPoint;
  begin
    Result := ParentChart.GraphToImage(AxisToGraph(calc(AT)))
  end;

var
  t, ts, ms: Double;
  p, pp: TPoint;
begin
  if Assigned(OnCalculate) then
    calc := @DoCalculate
  else if csDesigning in ComponentState then
    calc := @DoCalcIdentity
  else
    exit;
  ADrawer.Pen := Pen;

  t := ParamMin;
  pp := PointAt(ParamMin);
  ADrawer.MoveTo(pp);
  ms := IfThen(ParamMaxStep > 0, ParamMaxStep, (ParamMax - ParamMin) / 4);
  ts := ms;
  while t < ParamMax do begin
    p := PointAt(t + ts);
    if PointDist(p, pp) > Sqr(Step) then
      ts /= 2
    else begin
      ADrawer.LineTo(p);
      pp := p;
      t += ts;
      ts := MinValue([ts * 2, ms, ParamMax - t]);
    end;
  end;
end;

procedure TParametricCurveSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemLine.Create(Pen, LegendTextSingle));
end;

function TParametricCurveSeries.IsEmpty: Boolean;
begin
  Result := not Assigned(OnCalculate);
end;

function TParametricCurveSeries.ParamMaxIsStored: Boolean;
begin
  Result := ParamMax <> DEF_PARAM_MAX;
end;

function TParametricCurveSeries.ParamMaxStepIsStored: Boolean;
begin
  Result := ParamMaxStep > 0;
end;

function TParametricCurveSeries.ParamMinIsStored: Boolean;
begin
  Result := ParamMin <> DEF_PARAM_MIN;
end;

procedure TParametricCurveSeries.SetOnCalculate(
  AValue: TParametricCurveCalculateEvent);
begin
  if TMethod(FOnCalculate) = TMethod(AValue) then exit;
  FOnCalculate := AValue;
  UpdateParentChart;
end;

procedure TParametricCurveSeries.SetParamMax(AValue: Double);
begin
  if FParamMax = AValue then exit;
  FParamMax := AValue;
  UpdateParentChart;
end;

procedure TParametricCurveSeries.SetParamMaxStep(AValue: Double);
begin
  if FParamMaxStep = AValue then exit;
  FParamMaxStep := AValue;
  UpdateParentChart;
end;

procedure TParametricCurveSeries.SetParamMin(AValue: Double);
begin
  if FParamMin = AValue then exit;
  FParamMin := AValue;
  UpdateParentChart;
end;

procedure TParametricCurveSeries.SetPen(AValue: TChartPen);
begin
  if FPen = AValue then exit;
  FPen.Assign(AValue);
  UpdateParentChart;
end;

procedure TParametricCurveSeries.SetStep(AValue: TFuncSeriesStep);
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

function TBSplineSeries.Calculate(AX: Double): Double;
var
  p: array of TDoublePoint;
  startIndex: Integer;
  splineStart: Integer = 0;
  splineEnd: Integer = -2;
  level: Integer = 0;
  pStart, pEnd: TDoublePoint;

  function CalcSpline(APos: Double): TDoublePoint;
  var
    i, d: Integer;
    w, denom: Double;
  begin
    // Duplicate end points Degree times to fix spline to them.
    for i := 0 to Degree do
      p[i] := FGraphPoints[
        EnsureRange(startIndex - Degree + i, splineStart, splineEnd)];
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
    Result := p[Degree];
  end;

  function Interpolate(ATest: Double): TDoublePoint;
  // calculates the B-Spline at n pivot points of the parametric coordinate t=0..1
  // and seeks the t for the requested x value (ATest) by means of
  // interpolating a cubic spline
  var
    i,n: Integer;
    pp: TDoublePoint;
    xval, yval: array of ArbFloat;
    coeff: array of ArbFloat;
    ok: Integer;
    t: ArbFloat;
  begin
    n := 10;
    SetLength(xval, n+1);
    SetLength(yval, n+1);
    SetLength(coeff, n+1);
    // calculate pivots
    for i:=0 to n do begin
      pp := CalcSpline(i/n);
      xval[i] := pp.X;
      yval[i] := i/n;
    end;
    // calc interpolation spline coefficients
    ok := 0;
    ipfisn(N, xval[0], yval[0], coeff[0], ok);
    // calc interpolation spline value at ATest
    t := ipfspn(High(coeff), xval[0], yval[0], coeff[0], ATest, ok);
    // calc B-Spline value at t
    Result := CalcSpline(t);
  end;

begin
  Result := NaN;
  if IsEmpty then
    exit;

  if Length(FGraphPoints) = 0 then
    InternalPrepareGraphPoints;

  SetLength(p, Degree + 1);
  while NextNumberSeq(FGraphPoints, splineStart, splineEnd) do begin
    startIndex := splineStart;
    pStart := CalcSpline(0.0);
    while startIndex <= splineEnd + Degree - 1 do begin
      pEnd := CalcSpline(1.0);
      // find interval
      if (AX = pStart.X) and (pStart.X = pEnd.X) then
        Result := pStart.Y
      else
      if InRange(AX, pStart.X, pEnd.X) and (pStart.X <> pEnd.X) then begin
        // calculate B-spline y value by interpolation
        if SameValue(AX, 15.88, 0.01) then
          Result := 1;
        Result := Interpolate(AX).Y;
        exit;
      end;
      pStart := pEnd;
      inc(startIndex);
    end;
  end;
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
  splineStart: Integer = 0;
  splineEnd: Integer = -2;

  function SplinePoint(APos: Double): TPoint;
  var
    i, d: Integer;
    w, denom: Double;
  begin
    // Duplicate end points Degree times to fix spline to them.
    for i := 0 to Degree do
      p[i] := FGraphPoints[
        EnsureRange(startIndex - Degree + i, splineStart, splineEnd)];
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

begin
  if IsEmpty then exit;

  InternalPrepareGraphPoints;

  SetLength(p, Degree + 1);
  ADrawer.Pen := Pen;
  while NextNumberSeq(FGraphPoints, splineStart, splineEnd) do begin
    ADrawer.MoveTo(ParentChart.GraphToImage(FGraphPoints[splineStart]));
    for startIndex := splineStart to splineEnd + Degree - 1 do
      SplineSegment(0.0, 1.0, SplinePoint(0.0), SplinePoint(1.0));
  end;
  DrawLabels(ADrawer);
  DrawPointers(ADrawer);
end;

procedure TBSplineSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemLine.Create(Pen, LegendTextSingle));
end;

procedure TBSplineSeries.InternalPrepareGraphPoints;
var
  ext: TDoubleRect;
begin
  with Extent do begin
    ext.a := AxisToGraph(a);
    ext.b := AxisToGraph(b);
  end;
  NormalizeRect(ext);
  ExpandRange(ext.a.X, ext.b.X, 1.0);
  ExpandRange(ext.a.Y, ext.b.Y, 1.0);
  PrepareGraphPoints(ext, true);
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

{ TCubicSplineSeries.TSpline }

function TCubicSplineSeries.TSpline.Calculate(AX: Double): Double;
var
  ok: Integer = 0;
begin
  if IsFewPoints then exit(SafeNaN);
  Result := ipfspn(High(FCoeff), FX[0], FY[0], FCoeff[0], AX, ok);
  if ok > 1 then
    Result :=  SafeNaN;
end;

destructor TCubicSplineSeries.TSpline.Destroy;
begin
  FreeAndNil(FIntervals);
  inherited;
end;

function TCubicSplineSeries.TSpline.IsFewPoints: Boolean;
begin
  Result := Length(FX) < 4;
end;

function TCubicSplineSeries.TSpline.PrepareCoeffs(
  ASource: TCustomChartSource; var AIndex: Integer): Boolean;
var
  n, ok: Integer;
begin
  n := ASource.Count - AIndex;
  SetLength(FX, n);
  SetLength(FY, n);
  SetLength(FCoeff, n);
  FIsUnorderedX := false;
  while (AIndex < ASource.Count) and IsNan(ASource[AIndex]^.Point) do
    AIndex += 1;
  FStartIndex := AIndex;
  n := 0;
  while (AIndex < ASource.Count) and not IsNan(ASource[AIndex]^.Point) do begin
    with ASource[AIndex]^ do
      if (n > 0) and (FX[n - 1] >= X) then
        FIsUnorderedX := true
      else begin
        FX[n] := X;
        FY[n] := Y;
        n += 1;
      end;
    AIndex += 1;
  end;
  SetLength(FX, n);
  SetLength(FY, n);
  SetLength(FCoeff, n);
  if n = 0 then exit(false);
  if IsFewPoints then exit(true);
  ok := 0;
  ipfisn(n - 1, FX[0], FY[0], FCoeff[0], ok);
  Result := ok = 1;
end;

procedure TCubicSplineSeries.TSpline.PrepareIntervals(
  AOptions: TCubicSplineOptions);
begin
  FIntervals := TIntervalList.Create;
  try
    if not (csoExtrapolateLeft in AOptions) then
      FIntervals.AddRange(NegInfinity, FX[0]);
    if not (csoExtrapolateRight in AOptions) then
      FIntervals.AddRange(FX[High(FX)], SafeInfinity);
  except
    FreeAndNil(FIntervals);
    raise;
  end;
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
  hint: Integer;
  s: TSpline;
  x: Double;
begin
  for s in FSplines do begin
    hint := 0;
    x := AX;
    if not s.FIntervals.Intersect(x, x, hint) then
      exit(s.Calculate(AX));
  end;
  Result := SafeNaN;
end;

constructor TCubicSplineSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBadDataPen := TBadDataChartPen.Create;
  FBadDataPen.OnChange := @StyleChanged;
  FPen := TChartPen.Create;
  FPen.OnChange := @StyleChanged;
  FPointer := TSeriesPointer.Create(ParentChart);
  FStep := DEF_SPLINE_STEP;
  FUseReticule := true;
end;

destructor TCubicSplineSeries.Destroy;
begin
  FreeSplines;
  FreeAndNil(FBadDataPen);
  FreeAndNil(FPen);
  inherited;
end;

procedure TCubicSplineSeries.Draw(ADrawer: IChartDrawer);

  function DrawFewPoints(ASpline: TSpline): Boolean;
  var
    pts: TPointArray;
    i, lb, ub: Integer;
  begin
    Result := ASpline.IsFewPoints;
    if not Result or not IsFewPointsVisible then exit;
    lb := Max(ASpline.FStartIndex, FLoBound);
    ub := Min(ASpline.FStartIndex + High(ASpline.FX), FUpBound);
    if lb > ub then exit;
    SetLength(pts, ub - lb + 1);
    for i := lb to ub do
      pts[i - lb] := ParentChart.GraphToImage(FGraphPoints[i - FLoBound]);
    ADrawer.Pen := BadDataPen;
    ADrawer.Polyline(pts, 0, Length(pts));
  end;

  procedure DrawSpline(ASpline: TSpline);
  begin
    if ASpline.FIsUnorderedX then begin
      if not IsUnorderedVisible then exit;
      ADrawer.Pen := BadDataPen;
    end
    else begin
      if not Pen.EffVisible then exit;
      ADrawer.Pen := Pen;
    end;
    with TDrawFuncHelper.Create(Self, ASpline.FIntervals, @ASpline.Calculate, Step) do
      try
        DrawFunction(ADrawer);
      finally
        Free;
      end;
  end;

var
  s: TSpline;
begin
  if IsEmpty then exit;
  if FSplines = nil then
    PrepareCoeffs;

  PrepareGraphPoints(FChart.CurrentExtent, true);
  for s in FSplines do
    if not DrawFewPoints(s) then
      DrawSpline(s);

  DrawLabels(ADrawer);
  DrawPointers(ADrawer);
end;

function TCubicSplineSeries.Extent: TDoubleRect;
var
  r: Integer = 0;
  minv, maxv: ArbFloat;
  extY: TDoubleInterval = (FStart: Infinity; FEnd: NegInfinity);
  s: TSpline;
begin
  Result := inherited Extent;
  if FSplines = nil then
    PrepareCoeffs;
  if FSplines = nil then exit;
  for s in FSplines do begin
    if s.IsFewPoints then continue;
    minv := Result.a.Y;
    maxv := Result.b.Y;
    ipfsmm(High(s.FCoeff), s.FX[0], s.FY[0], s.FCoeff[0], minv, maxv, r);
    extY.FStart := Min(minv, extY.FStart);
    extY.FEnd := Max(maxv, extY.FEnd);
  end;
  Result.a.Y := extY.FStart;
  Result.b.Y := extY.FEnd;
end;

procedure TCubicSplineSeries.FreeSplines;
var
  s: TSpline;
begin
  for s in FSplines do
    s.Free;
  FSplines := nil;
end;

procedure TCubicSplineSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemLine.Create(Pen, LegendTextSingle));
end;

function TCubicSplineSeries.GetNearestPoint(
  const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  s: TSpline;
  r: TNearestPointResults;
begin
  Result := inherited GetNearestPoint(AParams, AResults);
  for s in FSplines do begin
    if s.IsFewPoints or (s.FIsUnorderedX and not IsUnorderedVisible) then
      continue;
    with TDrawFuncHelper.Create(Self, s.FIntervals, @s.Calculate, Step) do
      try
        if
          not GetNearestPoint(AParams, r) or
          Result and (AResults.FDist <= r.FDist)
        then
          continue;
        AResults := r;
        Result := true;
      finally
        Free;
      end;
  end;
end;

function TCubicSplineSeries.IsFewPointsVisible: Boolean;
begin
  Result := (csoDrawFewPoints in Options) and BadDataPen.EffVisible;
end;

function TCubicSplineSeries.IsUnorderedVisible: Boolean;
begin
  Result := (csoDrawUnorderedX in Options) and BadDataPen.EffVisible;
end;

procedure TCubicSplineSeries.PrepareCoeffs;
var
  i: Integer = 0;
  s: TSpline;
begin
  FreeSplines;
  while i < Source.Count do begin
    s := TSpline.Create;
    if s.PrepareCoeffs(Source, i) then begin
      s.PrepareIntervals(Options);
      SetLength(FSplines, Length(FSplines) + 1);
      FSplines[High(FSplines)] := s;
    end
    else
      s.Free;
  end;
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
  FreeSplines;
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
  FreeSplines;
end;

{ TFitSeries }

procedure TFitSeries.AfterAdd;
begin
  inherited AfterAdd;
  FFitRange.SetOwner(ParentChart);
end;

function TFitSeries.Calculate(AX: Double): Double;
var
  i: Integer;
begin
  if IsInfinite(AX) then exit(AX);
  Result := SafeNaN;
  if IsNaN(AX) or (State <> fpsValid) then exit;

  case FFitEquation of
    fePolynomial, feLinear:
      begin
        Result := 0;
        for i := High(FFitParams) downto 0 do
          Result := Result * AX + FFitParams[i];
      end;
    feExp:
      Result := FFitParams[0] * Exp(FFitParams[1] * AX);
    fePower:
      if AX < 0 then
        Result := SafeNaN
      else
        Result := FFitParams[0] * Power(AX, FFitParams[1]);
  end;
end;

procedure TFitSeries.CalcXRange(out AXMin, AXMax: Double);
var
  ext: TDoubleRect;
begin
  with Extent do begin
    ext.a := AxisToGraph(a);
    ext.b := AxisToGraph(b);
  end;
  NormalizeRect(ext);
  AXMin := GraphToAxisX(ext.a.X);
  AXMax := GraphToAxisX(ext.b.X);
  EnsureOrder(AXMin, AXMax);
  FFitRange.Intersect(AXMin, AXMax);
end;

constructor TFitSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFitEquation := fePolynomial;
  FFitRange := TFitSeriesRange.Create(Self);
  FDrawFitRangeOnly := true;
  FPen := TChartPen.Create;
  FPen.OnChange := @StyleChanged;
  FStep := DEF_FIT_STEP;
  ParamCount := DEF_FIT_PARAM_COUNT; // Parabolic fit as default.
end;

destructor TFitSeries.Destroy;
begin
  FreeAndNil(FPen);
  FreeAndNil(FFitRange);
  inherited;
end;

procedure TFitSeries.Draw(ADrawer: IChartDrawer);
var
  de : TIntervalList;
begin
  if IsEmpty then exit;
  ExecFit;
  if State <> fpsValid then exit;
  ADrawer.Pen := Pen;
  de := PrepareIntervals;
  try
    with TDrawFuncHelper.Create(Self, de, @Calculate, Step) do
      try
        DrawFunction(ADrawer);
      finally
        Free;
      end;
  finally
    de.Free;
  end;
end;

function TFitSeries.EquationText: IFitEquationText;
begin
  if State = fpsValid then
    Result := TFitEquationText.Create
  else
    Result := TFitEmptyEquationText.Create;
  Result.Equation(FitEquation).Params(FFitParams);
end;

procedure TFitSeries.ExecFit;
var
  xmin, xmax: Double;

  function IsValidPoint(AX, AY: Double): Boolean; inline;
  begin
    Result := not IsNaN(AX) and not IsNaN(AY) and InRange(AX, xmin, xmax);
  end;

  procedure TryFit;
  var
    i, j, term, ns, np, n: Integer;
    xv, yv, fp: array of ArbFloat;
  begin
    np := ParamCount;
    ns := Source.Count;
    if (np <= 0) or (ns = 0) or (ns < np) then exit;
    CalcXRange(xmin, xmax);

    n := 0;
    for i := 0 to ns - 1 do
      with Source.Item[i]^ do
        n += Ord(IsValidPoint(X, Y));
    if n < np then exit;

    // Copy data in fit range to temporary arrays.
    SetLength(xv, n);
    SetLength(yv, n);
    j := 0;
    for i := 0 to ns - 1 do
      with Source.Item[i]^ do
        if IsValidPoint(X, Y) then begin
          Transform(X, Y, xv[j], yv[j]);
          j += 1;
        end;

    // Execute the polynomial fit; the degree of the polynomial is np - 1.
    SetLength(fp, np);
    term := 0;
    ipfpol(n, np - 1, xv[0], yv[0], fp[0], term);
    if term <> 1 then exit;
    for i := 0 to High(FFitParams) do
      FFitParams[i] := fp[i];

    // See comment for "Transform": for exponential and power fit equations, the
    // first fitted parameter is the logarithm of the "real" parameter. It needs
    // to be transformed back to real units by exp function.
    if FFitEquation in [feExp, fePower] then
      FFitParams[0] := Exp(FFitParams[0]);
    FState := fpsValid;
  end;

begin
  if State <> fpsUnknown then exit;
  FState := fpsInvalid;
  try
    TryFit;
  finally
    if Assigned(FOnFitComplete) then
      FOnFitComplete(Self);
    UpdateParentChart;
  end;
end;

function TFitSeries.GetFitEquationString(ANumFormat: String; AXText: String;
  AYText: String): String;
begin
  Result := EquationText.NumFormat(ANumFormat).X(AXText).Y(AYText);
end;

procedure TFitSeries.GetLegendItems(AItems: TChartLegendItems);
var
  t: String;
begin
  if Legend.Format = '' then
    t := Title
  else
    t := Format(Legend.Format, [Title, Index, EquationText.NumFormat('%f').Get]);
  AItems.Add(TLegendItemLine.Create(Pen, t));
end;

function TFitSeries.GetNearestPoint(
  const AParams: TNearestPointParams; out AResults: TNearestPointResults): Boolean;
var
  de : TIntervalList;
begin
  Result := false;
  AResults.FIndex := -1;
  ExecFit;
  if State <> fpsValid then exit;
  de := PrepareIntervals;
  try
    with TDrawFuncHelper.Create(Self, de, @Calculate, Step) do
      try
        Result := GetNearestPoint(AParams, AResults);
      finally
        Free;
      end;
  finally
    de.Free;
  end;
end;

function TFitSeries.GetParam(AIndex: Integer): Double;
begin
  if not InRange(AIndex, 0, ParamCount - 1) then
    raise EChartError.Create('TFitSeries.GetParam index out of range');
  Result := FFitParams[AIndex]
end;

function TFitSeries.GetParamCount: Integer;
begin
  Result := Length(FFitParams);
end;

function TFitSeries.PrepareIntervals: TIntervalList;
var
  xmin, xmax: Double;
begin
  Result := TIntervalList.Create;
  try
    CalcXRange(xmin, xmax);
    if DrawFitRangeOnly then begin
      Result.AddRange(NegInfinity, xmin);
      Result.AddRange(xmax, SafeInfinity);
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TFitSeries.SetDrawFitRangeOnly(AValue: Boolean);
begin
  if FDrawFitRangeOnly = AValue then exit;
  FDrawFitRangeOnly := AValue;
  UpdateParentChart;
end;

procedure TFitSeries.SetFitEquation(AValue: TFitEquation);
begin
  if FFitEquation = AValue then exit;
  FFitEquation := AValue;
  SetLength(
    FFitParams, IfThen(FFitEquation = fePolynomial, DEF_FIT_PARAM_COUNT, 2));
  FState := fpsUnknown;
  UpdateParentChart;
end;

procedure TFitSeries.SetFitRange(AValue: TChartRange);
begin
  if FFitRange = AValue then exit;
  FFitRange := AValue;
  FState := fpsUnknown;
  UpdateParentChart;
end;

procedure TFitSeries.SetParam(AIndex: Integer; AValue: Double);
begin
  if not InRange(AIndex, 0, ParamCount - 1) then
    raise EChartError.Create('TFitSeries.SetParam index out of range');
  FFitParams[AIndex] := AValue;
  UpdateParentChart;
end;

procedure TFitSeries.SetParamCount(AValue: Integer);
begin
  if (AValue = ParamCount) or (FFitEquation <> fePolynomial) then exit;
  SetLength(FFitParams, AValue);
  FState := fpsUnknown;
  UpdateParentChart;
end;

procedure TFitSeries.SetPen(AValue: TChartPen);
begin
  if FPen = AValue then exit;
  FPen.Assign(AValue);
  UpdateParentChart;
end;

procedure TFitSeries.SetStep(AValue: TFuncSeriesStep);
begin
  if FStep = AValue then exit;
  FStep := AValue;
  UpdateParentChart;
end;

procedure TFitSeries.SourceChanged(ASender: TObject);
begin
  inherited;
  FState := fpsUnknown;
end;

procedure TFitSeries.Transform(AX, AY: Double; out ANewX, ANewY: Extended);
begin
  // The exponential and power fitting equations can be transformed to a
  // polynomial by taking the logarithm:
  // feExp:   y = a exp(b*x) ==> ln(y) = ln(a) + b*x
  // fePower: y = a*x^b      ==> ln(y) = ln(a) + b*ln(x)
  // In each case, the first parameter (a) needs to be transformed back
  // after the fitting -- see "ExecFit".
  if FitEquation in [fePower] then
    ANewX := Ln(AX)
  else
    ANewX := AX;
  if FitEquation in [feExp, fePower] then
    ANewY := Ln(AY)
  else
    ANewY := AY;
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
  if (ColorSource = nil) or (ColorSource.Count = 0) then exit(clTAColor);
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
  r, cell: TRect;
  pt, next, offset: TPoint;
  gp: TDoublePoint;
  v: Double;
  img: TLazIntfImage = nil;
  rawImage: TRawImage;
  optimize: Boolean;
  x, y: Integer;
  cellColor: TChartColor;
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
  pt.Y := (r.Top div StepY - 1) * StepY + offset.Y mod StepY;

  case UseImage of
    cmuiAuto: optimize := (StepX <= 2) and (StepY <= 2);
    cmuiAlways: optimize := true;
    cmuiNever: optimize := false;
  end;
  if optimize then
    img := CreateLazIntfImage(rawImage, r.BottomRight - r.TopLeft)
  else begin
    ADrawer.Brush := Brush;
    ADrawer.SetPenParams(psClear, clTAColor);
  end;

  try
    while pt.Y <= r.Bottom do begin
      next.Y := pt.Y + StepY;
      if next.Y <= r.Top then begin
        pt.Y := next.Y;
        continue;
      end;
      pt.X := (r.Left div StepX - 1) * StepX + offset.X mod StepX;
      while pt.X <= r.Right do begin
        next.X := pt.X + StepX;
        if next.X <= r.Left then begin
          pt.X := next.X;
          continue;
        end;
        gp := GraphToAxis(ParentChart.ImageToGraph((pt + next) div 2));
        if not (csDesigning in ComponentState) then
          OnCalculate(gp.X, gp.Y, v);
        cell := Rect(
          Max(pt.X, r.Left), Max(pt.Y, r.Top),
          Min(next.X, r.Right) + 1, Min(next.Y, r.Bottom) + 1);
        if optimize then begin
          if ColorSource = nil then
            cellColor := Brush.Color
          else
            cellColor := ColorByValue(v);
          for y := cell.Top - r.Top to cell.Bottom - r.Top - 2 do
            for x := cell.Left - r.Left to cell.Right - r.Left - 2 do
              img.TColors[x, y] := cellColor;
        end
        else begin
          if ColorSource <> nil then
            ADrawer.BrushColor := ColorByValue(v);
          ADrawer.Rectangle(cell);
        end;
        pt.X := next.X;
      end;
      pt.Y := next.Y;
    end;
    if optimize then
      ADrawer.PutImage(r.Left, r.Top, img);
  finally
    FreeAndNil(img);
  end;
end;

procedure TColorMapSeries.GetLegendItems(AItems: TChartLegendItems);

  function PrepareFormats: TStrings;
  const
    FORMAT_DEF = 'z ≤ %1:g|%g < z ≤ %g|%g < z';
  begin
    Result := Split(IfThen(Legend.Format = '', FORMAT_DEF, Legend.Format));
    with Result do
      try
        while Count < 3 do
          Add(Strings[Count - 1]);
      except
        Result.Free;
        raise;
      end;
  end;

var
  prev: Double;
  formats: TStrings;

  function ItemTitle(AIndex: Integer; const AText: String; AX: Double): String;
  var
    idx: Integer;
  begin
    if AText <> '' then exit(AText);
    if ColorSource.Count = 1 then exit('');
    if AIndex = 0 then idx := 0
    else if AIndex = ColorSource.Count - 1 then idx := 2
    else idx := 1;
    Result := Format(formats[idx], [prev, AX]);
  end;

  procedure MakePointItems;
  var
    t: String;
    prevColor: TColor;
    li: TLegendItem;
    i: Integer;
  begin
    prev := ColorSource[0]^.X;
    prevColor := clTAColor;
    formats := PrepareFormats;
    try
      for i := 0 to ColorSource.Count - 1 do
        with ColorSource[i]^ do begin
          t := ItemTitle(i, Text, X);
          if Interpolate then
            li := TLegendItemColorMap.Create(
              ColorDef(prevColor, Color), Color, ParentChart.Legend.SymbolFrame, t)
          else begin
            li := TLegendItemBrushRect.Create(Brush, t);
            li.Color := Color;
          end;
          AItems.Add(li);
          prev := X;
          prevColor := Color;
        end;
    finally
      formats.Free;
    end;
  end;

begin
  case Legend.Multiplicity of
    lmSingle:
      AItems.Add(TLegendItemBrushRect.Create(Brush, LegendTextSingle));
    lmPoint:
      if (ColorSource <> nil) and (ColorSource.Count > 0) then
        MakePointItems;
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
  if ColorSource <> nil then
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

procedure TColorMapSeries.SetUseImage(AValue: TUseImage);
begin
  if FUseImage = AValue then exit;
  FUseImage := AValue;
  UpdateParentChart;
end;

initialization
  RegisterSeriesClass(TFuncSeries, 'Function series');
  RegisterSeriesClass(TParametricCurveSeries, 'Parametric curve series');
  RegisterSeriesClass(TBSplineSeries, 'B-Spline series');
  RegisterSeriesClass(TCubicSplineSeries, 'Cubic spline series');
  RegisterSeriesClass(TFitSeries, 'Least-squares fit series');
  RegisterSeriesClass(TColorMapSeries, 'Color map series');

end.

