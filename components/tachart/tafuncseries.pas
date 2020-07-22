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
  TAChartUtils, TACustomFuncSeries, TACustomSeries, TACustomSource, TASources,
  TADrawUtils, TAFitUtils, TALegend, TATypes, TAFitLib, TAStyles;

const
  DEF_FUNC_STEP = 2;
  DEF_SPLINE_DEGREE = 3;
  DEF_SPLINE_STEP = 4;
  DEF_FIT_STEP = 4;
  DEF_FIT_PARAM_COUNT = 3;
  DEF_COLORMAP_STEP = 4;
  DEF_COLORMAP_LEGENDFORMAT = 'z ≤ %1:g|%g < z ≤ %g|%g < z';

type
  TFuncCalculateEvent = procedure (const AX: Double; out AY: Double) of object;

  TFuncSeriesStep = 1..MaxInt;

  TCustomFuncSeries = class(TBasicFuncSeries)
  strict private
    FDomainExclusions: TIntervalList;
    FExtentAutoY: Boolean;
    FPen: TChartPen;
    FStep: TFuncSeriesStep;

    procedure SetExtentAutoY(AValue: Boolean);
    procedure SetPen(AValue: TChartPen);
    procedure SetStep(AValue: TFuncSeriesStep);

  protected
    function DoCalculate(AX: Double): Double; virtual; abstract;
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
  public
    property DomainExclusions: TIntervalList read FDomainExclusions;
  published
    property AxisIndexX;
    property AxisIndexY;
    property ExtentAutoY: Boolean
      read FExtentAutoY write SetExtentAutoY default false;
    property Pen: TChartPen read FPen write SetPen;
    property Step: TFuncSeriesStep
      read FStep write SetStep default DEF_FUNC_STEP;
  end;

  TFuncSeries = class(TCustomFuncSeries)
  strict private
    FOnCalculate: TFuncCalculateEvent;
    procedure SetOnCalculate(AValue: TFuncCalculateEvent);
  protected
    function DoCalculate(AX: Double): Double; override;
    procedure GetBounds(var ABounds: TDoubleRect); override;
  public
    procedure Assign(ASource: TPersistent); override;
    procedure Draw(ADrawer: IChartDrawer); override;
    function IsEmpty: Boolean; override;
  published
    property OnCalculate: TFuncCalculateEvent
      read FOnCalculate write SetOnCalculate;
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
    function GetNearestPoint(const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
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
    property MarkPositions;
    property Marks;
    property Pen: TChartPen read FPen write SetPen;
    property Pointer;
    property Step: TFuncSeriesStep
      read FStep write SetStep default DEF_SPLINE_STEP;
    property Styles;
    property ToolTargets default [nptPoint, nptCustom];
    property XErrorBars;
    property YErrorBars;
    property OnCustomDrawPointer;
    property OnGetPointerStyle;
  end;

  TBadDataChartPen = class(TChartPen)
  published
    property Color default clRed;
  end;

  TCubicSplineOptions = set of (
    csoDrawUnorderedX, csoExtrapolateLeft, csoExtrapolateRight
  );

  TCubicSplineType = (cstNatural, cstHermiteMonotone);

  TCubicSplineSeries = class(TBasicPointSeries)
  strict private
    FBadDataPen: TBadDataChartPen;
    FCachedExtent: TDoubleRect;
    FOptions: TCubicSplineOptions;
    FSplineType: TCubicSplineType;
    FPen: TChartPen;
    FStep: TFuncSeriesStep;
    FX, FY: array of ArbFloat;
    procedure SetPen(AValue: TChartPen);
    procedure SetStep(AValue: TFuncSeriesStep);
  strict private
  type
    TSpline = class
    public
      FOwner: TCubicSplineSeries;
      FCoeff: array of ArbFloat;
      FIntervals: TIntervalList;
      FIsUnorderedX: Boolean;
      FSourceStartIndex: Integer;
      FFirstCacheIndex, FLastCacheIndex: Integer;
      constructor Create(AOwner: TCubicSplineSeries);
      destructor Destroy; override;
      function Calculate(AX: Double): Double;
      function IsFewPoints: Boolean; inline;
      function PrepareCoeffs(ASource: TCustomChartSource;
        var ASourceIndex, ACacheIndex: Integer): Boolean;
    end;

  var
    FSplines: array of TSpline;
    procedure FreeSplines;
    function GetSplineXRange(ASpline: TSpline; out AXMin, AXMax: Double): Boolean;
    function IsUnorderedVisible: Boolean; inline;
    procedure PrepareCoeffs;
    procedure SetBadDataPen(AValue: TBadDataChartPen);
    procedure SetOptions(AValue: TCubicSplineOptions);
    procedure SetSplineType(AValue: TCubicSplineType);
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
    property MarkPositions;
    property Marks;
    property Pointer;
    property ShowInLegend;
    property Source;
    property Title;
    property ToolTargets default [nptPoint, nptCustom];
    property ZPosition;
    property OnCustomDrawPointer;
    property OnGetPointerStyle;
  published
    // Used when data is not suitable for drawing cubic spline --
    // e.g. points are not ordered by X value.
    property BadDataPen: TBadDataChartPen read FBadDataPen write SetBadDataPen;
    property Options: TCubicSplineOptions
      read FOptions write SetOptions default [];
    property Pen: TChartPen read FPen write SetPen;
    property SplineType: TCubicSplineType
      read FSplineType write SetSplineType default cstNatural;
    property Step: TFuncSeriesStep
      read FStep write SetStep default DEF_SPLINE_STEP;
    property XErrorBars;
    property YErrorBars;
  end;

  TFitSeries = class;

  TFitParamsState = (fpsUnknown, fpsInvalid, fpsValid);
  TFitFuncIndex = 0..MaxInt;
  TFitFuncEvent = procedure(AIndex: TFitFuncIndex; AFitFunc: TFitFunc) of object;
  TFitEquationTextEvent = procedure (ASeries: TFitSeries; AEquationText: IFitEquationText) of object;

  TFitSeries = class(TBasicPointSeries)
  strict private
    FAutoFit: Boolean;
    FDrawFitRangeOnly: Boolean;
    FUseCombinedExtentY: Boolean;
    FFitEquation: TFitEquation;
    FFitParams: TFitParamArray; // raw values, not transformed!
    FFitRange: TChartRange;
    FFixedParams: String;
    FOnFitComplete: TNotifyEvent;
    FOnFitEquationText: TFitEquationTextEvent;
    FPen: TChartPen;
    FState: TFitParamsState;
    FStep: TFuncSeriesStep;
    FErrCode: TFitErrCode;
    FFitStatistics: TFitStatistics;
    FConfidenceLevel: Double;
    FLockFit: Integer;
    function GetParam(AIndex: Integer): Double;
    function GetParamCount: Integer;
    function GetParamError(AIndex: Integer): Double;
    function GetParam_RawError(AIndex: Integer): Double;
    function GetParam_RawValue(AIndex: Integer): Double;
    function GetParam_tValue(AIndex: Integer): Double;
    function IsFixedParamsStored: Boolean;
    procedure SetConfidenceLevel(AValue: Double);
    procedure SetDrawFitRangeOnly(AValue: Boolean);
    procedure SetFitEquation(AValue: TFitEquation);
    procedure SetFitRange(AValue: TChartRange);
    procedure SetFixedParams(AValue: String);
    procedure SetParamCount(AValue: Integer);
    procedure SetPen(AValue: TChartPen);
    procedure SetStep(AValue: TFuncSeriesStep);
    procedure SetUseCombinedExtentY(AValue: Boolean);
    {$IF FPC_FullVersion >= 30004}
    procedure GetInterval(const Ax: Double; out AY: Double; IsUpper, IsPrediction: Boolean);
    function GetParam_pValue(AIndex: Integer): Double;
    {$IFEND}
  strict protected
    procedure CalcXRange(out AXMin, AXMax: Double);
    function TransformX(AX: Double): Extended; inline;
    function TransformY(AY: Double): Extended; inline;
  protected
    procedure AfterAdd; override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    procedure InvalidateFitResults; virtual;
    procedure Loaded; override;
    function PrepareFitParams: boolean;
    function PrepareIntervals: TIntervalList; virtual;
    procedure SourceChanged(ASender: TObject); override;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure BeginUpdate;
    function Calculate(AX: Double): Double; virtual;
    procedure Clear; override;
    procedure Draw(ADrawer: IChartDrawer); override;
    procedure EndUpdate;
    function ErrorMsg: String;
    procedure ExecFit; virtual;
    function Extent: TDoubleRect; override;
    function EquationText: IFitEquationText;
    function FitParams: TDoubleDynArray;
    {$IF FPC_FullVersion >= 30004}
    procedure GetConfidenceLimits(AIndex: Integer; out ALower, AUpper: Double);
    procedure GetLowerConfidenceInterval(const Ax: Double; out AY: Double);
    procedure GetUpperConfidenceInterval(const Ax: Double; out AY: Double);
    procedure GetLowerPredictionInterval(const Ax: Double; out AY: Double);
    procedure GetUpperPredictionInterval(const Ax: Double; out AY: Double);
    {$IFEND}
    function GetNearestPoint(
      const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
    procedure SetFitBasisFunc(AIndex: TFitFuncIndex; AFitFunc: TFitFunc;
      AFitFuncName: String);
  public  // properties
    property Param[AIndex: Integer]: Double read GetParam;
    property ParamError[AIndex: Integer]: Double read GetParamError;
    {$IF FPC_FullVersion >= 30004}
    property Param_pValue[AIndex: Integer]: Double read GetParam_pValue;
    {$IFEND}
    property Param_tValue[AIndex: Integer]: Double read GetParam_tValue;
    property FitStatistics: TFitStatistics read FFitStatistics;
    property ConfidenceLevel: Double read FConfidenceLevel write SetConfidenceLevel;
    property ErrCode: TFitErrCode read FErrCode;
    property State: TFitParamsState read FState;
  published
    property AutoFit: Boolean read FAutoFit write FAutoFit default true;
    property AxisIndexX;
    property AxisIndexY;
    property DrawFitRangeOnly: Boolean
      read FDrawFitRangeOnly write SetDrawFitRangeOnly default true;
    property FitEquation: TFitEquation
      read FFitEquation write SetFitEquation default fePolynomial;
    property FitRange: TChartRange read FFitRange write SetFitRange;
    property FixedParams: String read FFixedParams write SetFixedParams
      stored IsFixedParamsStored;
    property MarkPositions;
    property Marks;
    property ParamCount: Integer
      read GetParamCount write SetParamCount default DEF_FIT_PARAM_COUNT;
    property Pen: TChartPen read FPen write SetPen;
    property Pointer;
    property Source;
    property Step: TFuncSeriesStep read FStep write SetStep default DEF_FIT_STEP;
    property ToolTargets default [nptPoint, nptCustom];
    property UseCombinedExtentY: Boolean
      read FUseCombinedExtentY write SetUseCombinedExtentY default false;
    property XErrorBars;
    property YErrorBars;
    property OnCustomDrawPointer;
    property OnFitComplete: TNotifyEvent
      read FOnFitComplete write FOnFitComplete;
    property OnFitEquationText: TFitEquationTextEvent
      read FOnFitEquationText write FOnFitEquationText;
    property OnGetPointerStyle;
  end;

  TColorMapPalette = (cmpHot, cmpCold, cmpRainbow, cmpMonochrome);

  TFuncCalculate3DEvent =
    procedure (const AX, AY: Double; out AZ: Double) of object;

  TCustomColorMapSeries = class(TBasicFuncSeries)
  public
  type
    TUseImage = (cmuiAuto, cmuiAlways, cmuiNever);
  strict private
    FBrush: TBrush;
    FColorSource: TCustomChartSource;
    FColorSourceListener: TListener;
    FInterpolate: Boolean;
    FStepX: TFuncSeriesStep;
    FStepY: TFuncSeriesStep;
    FUseImage: TUseImage;
    FColorExtentMin, FColorExtentMax: Double;
    FBuiltinColorSource: TListChartSource;
    FBuiltinPalette: TColormapPalette;
    FPaletteMax: Double;
    FPaletteMin: Double;
    function GetColorSource: TCustomChartSource;
    function IsColorSourceStored: boolean;
    function IsPaletteMaxStored: Boolean;
    function IsPaletteMinStored: Boolean;
    procedure SetBrush(AValue: TBrush);
    procedure SetBuiltinPalette(AValue: TColorMapPalette);
    procedure SetColorSource(AValue: TCustomChartSource);
    procedure SetInterpolate(AValue: Boolean);
    procedure SetPaletteMax(AValue: Double);
    procedure SetPaletteMin(AValue: Double);
    procedure SetStepX(AValue: TFuncSeriesStep);
    procedure SetStepY(AValue: TFuncSeriesStep);
    procedure SetUseImage(AValue: TUseImage);
  protected
    FMinZ, FMaxZ: Double;
    procedure BuildPalette(APalette: TColorMapPalette);
    procedure CheckColorSource(ASource: TCustomChartSource);
    procedure ColorSourceChanged(ASender: TObject); virtual;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    procedure GetZRange(ARect: TRect; dx, dy: Integer);
    procedure UpdateColorExtent;
    class procedure GetXYCountNeeded(out AXCount, AYCount: Cardinal); virtual;

  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  public
    function ColorByValue(AValue: Double): TColor;
    function FunctionValue(AX, AY: Double): Double; virtual;
    procedure Draw(ADrawer: IChartDrawer); override;
    function IsEmpty: Boolean; override;
  published
    property AxisIndexX;
    property AxisIndexY;
    property Brush: TBrush read FBrush write SetBrush;
    property BuiltInPalette: TColorMapPalette
      read FBuiltinPalette write SetBuiltinPalette default cmpHot;
    property BuiltInPaletteMax: Double
      read FPaletteMax write SetPaletteMax stored IsPaletteMaxStored;
    property BuiltInPaletteMin: Double
      read FPaletteMin write SetPaletteMin stored IsPaletteMinStored;
    property ColorSource: TCustomChartSource
      read GetColorSource write SetColorSource stored IsColorSourceStored;
    property Interpolate: Boolean
      read FInterpolate write SetInterpolate default false;
    property StepX: TFuncSeriesStep
      read FStepX write SetStepX default DEF_COLORMAP_STEP;
    property StepY: TFuncSeriesStep
      read FStepY write SetStepY default DEF_COLORMAP_STEP;
    property UseImage: TUseImage
      read FUseImage write SetUseImage default cmuiAuto;
  end;

  TColorMapSeries = class(TCustomColorMapSeries)
  private
    FOnCalculate: TFuncCalculate3DEvent;
    procedure SetOnCalculate(AValue: TFuncCalculate3DEvent);
  public
    procedure Assign(ASource: TPersistent); override;
    function FunctionValue(AX, AY: Double): Double; override;
    function IsEmpty: Boolean; override;
  published
    property OnCalculate: TFuncCalculate3DEvent
      read FOnCalculate write SetOnCalculate;
  end;

  // Builds an equation string based on the parameters and the type of equation.
  // AXText and AYText are placeholders for the x and y variables, respectively.
  // Parameters are formatted by passing ANumFormat to the "Format" function.
  function ParamsToEquation(
    AEquation: TFitEquation; const AParams: array of Double;
    ANumFormat: String; AXText: String = 'x'; AYText: String = 'y'): String;

implementation

uses
  {$IF FPC_FullVersion >= 30101}ipf{$ELSE}ipf_fix{$ENDIF},
  GraphType, GraphUtil, IntfGraphics, Math, spe, StrUtils, SysUtils,
  TAChartStrConsts, TAGeometry, TAGraph, TAMath;

const
  DEF_PARAM_MIN = 0.0;
  DEF_PARAM_MAX = 1.0;

  SIndexOutOfRange = '[%s.%s] Index out of range.';

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

function ParamsToEquation(
  AEquation: TFitEquation; const AParams: array of Double;
  ANumFormat, AXText, AYText: String): String;
begin
  Result :=
    TFitEquationText.Create.Equation(AEquation).
    X(AXText).Y(AYText).NumFormat(ANumFormat).Params(AParams);
end;

// Workaround for numlib issue with too-small arguments of exp()
// https://bugs.freepascal.org/view.php?id=34434
function exp(x: ArbFloat): ArbFloat;
begin
  Result := system.exp(x);
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
  FSeries.InvalidateFitResults;
//  if FSeries.AutoFit then FSeries.ExecFit;
  inherited;
end;


{ TCustomFuncSeries }

procedure TCustomFuncSeries.Assign(ASource: TPersistent);
begin
  if ASource is TCustomFuncSeries then
    with TCustomFuncSeries(ASource) do begin
      Self.FDomainExclusions.Assign(FDomainExclusions);
      Self.FExtentAutoY := FExtentAutoY;
      Self.Pen := FPen;
      Self.FStep := FStep;
    end;
  inherited Assign(ASource);
end;

constructor TCustomFuncSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDomainExclusions := TIntervalList.Create;
  FDomainExclusions.OnChange := @StyleChanged;
  FPen := TChartPen.Create;
  FPen.OnChange := @StyleChanged;
  FStep := DEF_FUNC_STEP;
end;

destructor TCustomFuncSeries.Destroy;
begin
  FreeAndNil(FDomainExclusions);
  FreeAndNil(FPen);
  inherited;
end;

procedure TCustomFuncSeries.Draw(ADrawer: IChartDrawer);
begin
  if IsEmpty or (not Active) then exit;
  if not RequestValidChartScaling then exit;

  ADrawer.SetBrushParams(bsClear, clTAColor);
  ADrawer.Pen := Pen;
  with TDrawFuncHelper.Create(Self, DomainExclusions, @DoCalculate, Step) do
    try
      DrawFunction(ADrawer);
    finally
      Free;
    end;
end;

procedure TCustomFuncSeries.GetBounds(var ABounds: TDoubleRect);
var
  ymin, ymax: Double;
begin
  inherited GetBounds(ABounds);
  if not Extent.UseXMin or not Extent.UseXMax or not ExtentAutoY then
    exit;
  if IsEmpty or (not Active) then exit;
  if not RequestValidChartScaling then exit;

  with TDrawFuncHelper.Create(Self, DomainExclusions, @DoCalculate, Step) do
    try
      ymin := SafeInfinity;
      ymax := NegInfinity;
      CalcAxisExtentY(ABounds.a.X, ABounds.b.X, ymin, ymax);
      if not Extent.UseYMin or (ymin > Extent.YMin) then
        ABounds.a.Y := ymin;
      if not Extent.UseYMax or (ymax < Extent.YMax) then
        ABounds.b.Y := ymax;
    finally
      Free;
    end;
end;

procedure TCustomFuncSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemLine.Create(Pen, LegendTextSingle));
end;

function TCustomFuncSeries.GetNearestPoint(
  const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
begin
  // As in TBasicPointSeries.GetNearestPoint()
  AResults.FDist := Sqr(AParams.FRadius) + 1;
  AResults.FIndex := -1;
  AResults.FXIndex := 0;
  AResults.FYIndex := 0;
  if IsEmpty then exit(false);
  if not RequestValidChartScaling then exit(false);

  with TDrawFuncHelper.Create(Self, DomainExclusions, @DoCalculate, Step) do
    try
      Result := GetNearestPoint(AParams, AResults);
    finally
      Free;
    end;
end;

procedure TCustomFuncSeries.SetExtentAutoY(AValue: Boolean);
begin
  if FExtentAutoY = AValue then exit;
  FExtentAutoY := AValue;
  UpdateParentChart;
end;

procedure TCustomFuncSeries.SetPen(AValue: TChartPen);
begin
  if FPen = AValue then exit;
  FPen.Assign(AValue);
  UpdateParentChart;
end;

procedure TCustomFuncSeries.SetStep(AValue: TFuncSeriesStep);
begin
  if FStep = AValue then exit;
  FStep := AValue;
  UpdateParentChart;
end;


{ TFuncSeries }

procedure TFuncSeries.Assign(ASource: TPersistent);
begin
  if ASource is TFuncSeries then
    with TFuncSeries(ASource) do begin
      Self.FOnCalculate := FOnCalculate;
    end;
  inherited Assign(ASource);
end;

function TFuncSeries.DoCalculate(AX: Double): Double;
begin
  OnCalculate(AX, Result);
end;

procedure TFuncSeries.Draw(ADrawer: IChartDrawer);
var
  R: TRect;
begin
  if (not Active) then exit;

  if csDesigning in ComponentState then begin
    with ParentChart do begin
      R.TopLeft := GraphToImage(CurrentExtent.a);
      R.BottomRight := GraphToImage(CurrentExtent.b);
      NormalizeRect(R);
    end;
    ADrawer.SetBrushParams(bsClear, clTAColor);
    ADrawer.Pen := Pen;
    ADrawer.Line(R.Left, R.Bottom, R.Right, R.Top);
    exit;
  end;

  inherited;
end;

procedure TFuncSeries.GetBounds(var ABounds: TDoubleRect);
begin
  inherited GetBounds(ABounds);
  if not (csDesigning in ComponentState) or
     not Extent.UseXMin or not Extent.UseXMax or not ExtentAutoY then exit;

  // When designing, an oblique line is drawn (see TFuncSeries.Draw),
  // so bounds should be adjusted when ExtentAutoY is True
  ABounds.a.Y := ABounds.a.X;
  ABounds.b.Y := ABounds.b.X;
end;

function TFuncSeries.IsEmpty: Boolean;
begin
  Result := not Assigned(OnCalculate);
end;

procedure TFuncSeries.SetOnCalculate(AValue: TFuncCalculateEvent);
begin
  if TMethod(FOnCalculate) = TMethod(AValue) then exit;
  FOnCalculate := AValue;
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

function TParametricCurveSeries.DoCalculate(AT: Double): TDoublePoint;
begin
  OnCalculate(AT, Result.X, Result.Y);
end;

procedure TParametricCurveSeries.Draw(ADrawer: IChartDrawer);

  function PointAt(AT: Double): TPoint;
  begin
    Result := ParentChart.GraphToImage(AxisToGraph(DoCalculate(AT)))
  end;

var
  R: TRect;
  t, ts, ms: Double;
  p, pp: TPoint;
begin
  if (not Active) then exit;

  ADrawer.SetBrushParams(bsClear, clTAColor);
  ADrawer.Pen := Pen;

  if csDesigning in ComponentState then begin
    with ParentChart do begin
      R.TopLeft := GraphToImage(LogicalExtent.a);
      R.BottomRight := GraphToImage(LogicalExtent.b);
      NormalizeRect(R);
    end;
    ADrawer.Ellipse(R.Left, R.Bottom, R.Right, R.Top);
    exit;
  end;

  if IsEmpty then exit;

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
  Result := not SameValue(ParamMax, DEF_PARAM_MAX);
end;

function TParametricCurveSeries.ParamMaxStepIsStored: Boolean;
begin
  Result := not SameValue(ParamMaxStep, 0.0) and (ParamMaxStep > 0);
end;

function TParametricCurveSeries.ParamMinIsStored: Boolean;
begin
  Result := not SameValue(ParamMin, DEF_PARAM_MIN);
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
  if SameValue(FParamMax, AValue) then exit;
  FParamMax := AValue;
  UpdateParentChart;
end;

procedure TParametricCurveSeries.SetParamMaxStep(AValue: Double);
begin
  if SameValue(FParamMaxStep, AValue) then exit;
  FParamMaxStep := AValue;
  UpdateParentChart;
end;

procedure TParametricCurveSeries.SetParamMin(AValue: Double);
begin
  if SameValue(FParamMin, AValue) then exit;
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
  p: array of TDoublePoint = nil;
  startIndex: Integer;
  splineStart: Integer = 0;
  splineEnd: Integer = -2;
  //level: Integer = 0;
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
    xval: array of ArbFloat = nil;
    yval: array of ArbFloat = nil;
    coeff: array of ArbFloat = nil;
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
  ToolTargets := [nptPoint, nptCustom];
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
  p: array of TDoublePoint = nil;
  startIndex: Integer;
  splineStart: Integer;
  splineEnd: Integer;

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

  procedure DrawSpline(AStyleIndex: Integer);
  var
    j: Integer;
  begin
    ADrawer.SetBrushParams(bsClear, clTAColor);
    ADrawer.Pen := Pen;
    if Styles <> nil then
      Styles.Apply(ADrawer, AStyleIndex, true);
      // "true" avoids painting the gaps of non-solid lines in brush color
    splineStart := 0;
    splineEnd := -2;
    while NextNumberSeq(FGraphPoints, splineStart, splineEnd) do begin
      ADrawer.MoveTo(ParentChart.GraphToImage(FGraphPoints[splineStart]));
      for j := splineStart to splineEnd + Degree - 1 do begin
        startIndex := j;
        SplineSegment(0.0, 1.0, SplinePoint(0.0), SplinePoint(1.0));
      end;
    end;
  end;

var
  i: Integer;
begin
  if IsEmpty or (not Active) then exit;

  SetLength(p, Degree + 1);

  InternalPrepareGraphPoints;
  DrawSpline(0);
  DrawErrorBars(ADrawer);
  DrawLabels(ADrawer, 0);
  DrawPointers(ADrawer, 0, true);

  for i := 1 to Source.YCount-1 do begin
    UpdateGraphPoints(i-1, false);
    DrawSpline(i);
    // error bars supported only for YLevel = 0 -- no DrawErrorBars here.
    DrawLabels(ADrawer, i);
    DrawPointers(ADrawer, i, true);
  end;
end;

procedure TBSplineSeries.GetLegendItems(AItems: TChartLegendItems);
var
  p: TSeriesPointer;
  li: TLegendItemLinePointer;
  s: TChartStyle;
  i: Integer;
  lBrush: TBrush;
  lPen: TPen;
begin
  if FPen.Visible and (FPen.Style <> psClear) then
    lPen := FPen
  else
    lPen := nil;

  if FPointer.Visible then
    p := FPointer
  else
    p := nil;

  case Legend.Multiplicity of
    lmSingle:
      AItems.Add(TLegendItemLinePointer.Create(lPen, p, LegendTextSingle));
    lmPoint:
      for i := 0 to Count - 1 do begin
        li := TLegendItemLinePointer.Create(lPen, p, LegendTextPoint(i));
        li.Color := GetColor(i);
        AItems.Add(li);
      end;
    lmStyle:
      if Styles <> nil then begin
        if Assigned(p) then lBrush := p.Brush else lBrush := nil;
        for s in Styles.Styles do
          AItems.Add(TLegendItemLinePointer.CreateWithBrush(
            IfThen((lPen <> nil) and s.UsePen, s.Pen, lPen) as TPen,
            IfThen(s.UseBrush, s.Brush, lBrush) as TBrush,
            p,
            LegendTextStyle(s)
          ));
        end;
  end;
end;

function TBSplineSeries.GetNearestPoint(
  const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  x, y: Double;
begin
  Result := inherited GetNearestPoint(AParams, AResults);

  if (not Result) and (nptCustom in ToolTargets) and (nptCustom in AParams.FTargets)
  then begin
    x := GraphToAxisX(ParentChart.XImageToGraph(AParams.FPoint.X));
    y := Calculate(x);
    AResults.FValue := DoublePoint(x, y);
    AResults.FImg := AParams.FPoint;
    AResults.FIndex := -1;
    AResults.FXIndex := -1;
    AResults.FYIndex := -1;

    AResults.FDist := 0;
    Result := not IsNaN(y);
  end;
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
  case FOwner.SplineType of
    cstNatural:
      Result := ipfspn(High(FCoeff), FOwner.FX[FFirstCacheIndex], FOwner.FY[FFirstCacheIndex], FCoeff[0], AX, ok);
    cstHermiteMonotone:
      Result := ipfsph(High(FCoeff), FOwner.FX[FFirstCacheIndex], FOwner.FY[FFirstCacheIndex], FCoeff[0], AX, ok);
  end;
  if ok > 1 then
    Result :=  SafeNaN;
end;

constructor TCubicSplineSeries.TSpline.Create(AOwner: TCubicSplineSeries);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TCubicSplineSeries.TSpline.Destroy;
begin
  FreeAndNil(FIntervals);
  inherited;
end;

function TCubicSplineSeries.TSpline.IsFewPoints: Boolean;
begin
  Result := (FLastCacheIndex <= FFirstCacheIndex);   // less than 2 points
end;

function TCubicSplineSeries.TSpline.PrepareCoeffs(ASource: TCustomChartSource;
  var ASourceIndex, ACacheIndex: Integer): Boolean;
var
  n, ok: Integer;
begin
  FIsUnorderedX := false;
  if ASource.XCount > 0 then
    while (ASourceIndex < ASource.Count) and IsNan(ASource[ASourceIndex]^.Point) do
      ASourceIndex += 1;
  FSourceStartIndex := ASourceIndex;
  FFirstCacheIndex := ACacheIndex;
  if ASource.XCount > 0 then
    while (ASourceIndex < ASource.Count) and not IsNan(ASource[ASourceIndex]^.Point) do begin
      with ASource[ASourceIndex]^ do
        if (ACacheIndex > FFirstCacheIndex) and (FOwner.FX[ACacheIndex - 1] >= X) then
          FIsUnorderedX := true
        else begin
          FOwner.FX[ACacheIndex] := X;
          FOwner.FY[ACacheIndex] := Y;
          ACacheIndex += 1;
        end;
      ASourceIndex += 1;
    end
  else
    while ASourceIndex < ASource.Count do begin
      with ASource[ASourceIndex]^ do begin
        FOwner.FX[ACacheIndex] := ASourceIndex;
        FOwner.FY[ACacheIndex] := Y;
        ACacheIndex += 1;
      end;
      ASourceIndex += 1;
    end;
  FLastCacheIndex := ACacheIndex - 1;
  if FLastCacheIndex < FFirstCacheIndex then exit(false);  // No points
  if IsFewPoints then exit(true);
  ok := 0;
  n := ACacheIndex - FFirstCacheIndex;
  SetLength(FCoeff, n);
  case FOwner.SplineType of
    cstNatural:
      ipfisn(n - 1, FOwner.FX[FFirstCacheIndex], FOwner.FY[FFirstCacheIndex], FCoeff[0], ok);
    cstHermiteMonotone:
      ipfish(hstMonotone, n - 1, FOwner.FX[FFirstCacheIndex], FOwner.FY[FFirstCacheIndex], FCoeff[0], ok);
  end;
  Result := (ok = 1);
end;


{ TCubicSplineSeries }

procedure TCubicSplineSeries.Assign(ASource: TPersistent);
begin
  if ASource is TCubicSplineSeries then
    with TCubicSplineSeries(ASource) do begin
      if (Self.FOptions <> FOptions) or (Self.FSplineType <> FSplineType) then
        Self.FreeSplines;
      Self.BadDataPen.Assign(FBadDataPen);
      Self.FOptions := FOptions;
      Self.FPen.Assign(FPen);
      Self.FSplineType := FSplineType;
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
  ToolTargets := [nptPoint, nptCustom];
  FBadDataPen := TBadDataChartPen.Create;
  FBadDataPen.OnChange := @StyleChanged;
  FPen := TChartPen.Create;
  FPen.OnChange := @StyleChanged;
  FPointer := TSeriesPointer.Create(ParentChart);
  FStep := DEF_SPLINE_STEP;
  FCachedExtent := EmptyExtent;
end;

destructor TCubicSplineSeries.Destroy;
begin
  FreeSplines;
  FreeAndNil(FBadDataPen);
  FreeAndNil(FPen);
  inherited;
end;

procedure TCubicSplineSeries.Draw(ADrawer: IChartDrawer);

  procedure DrawSpline(ASpline: TSpline);
  var
    xmin, xmax: Double;
  begin
    if not GetSplineXRange(ASpline, xmin, xmax) then
      exit;
    ADrawer.SetBrushParams(bsClear, clTAColor);
    if ASpline.FIsUnorderedX then begin
      if not IsUnorderedVisible then exit;
      ADrawer.Pen := BadDataPen;
    end
    else begin
      if not Pen.EffVisible then exit;
      ADrawer.Pen := Pen;
    end;
    with TPointsDrawFuncHelper.Create(Self, xmin, xmax, ASpline.FSourceStartIndex, @ASpline.Calculate, Step) do
      try
        DrawFunction(ADrawer);
      finally
        Free;
      end;
  end;

var
  s: TSpline;
begin
  if IsEmpty or (not Active) then exit;
  if not RequestValidChartScaling then exit;

  if FSplines = nil then
    PrepareCoeffs;

  PrepareGraphPoints(FChart.CurrentExtent, true);
  for s in FSplines do
    if not s.IsFewPoints then
      DrawSpline(s);

  DrawErrorBars(ADrawer);
  DrawLabels(ADrawer, 0);
  DrawPointers(ADrawer, 0, true);
end;

function TCubicSplineSeries.Extent: TDoubleRect;
var
  r: Integer = 0;
  minv, maxv: ArbFloat;
  extY: TDoubleInterval = (FStart: Infinity; FEnd: NegInfinity);
  extChg: Boolean = false;
  s: TSpline;
begin
  Result := Source.BasicExtent;
  if SplineType = cstHermiteMonotone then
    exit;

  if (FCachedExtent <> EmptyExtent) then begin
    Result := FCachedExtent;
    exit;
  end;

  if FSplines = nil then
    PrepareCoeffs;
  if FSplines = nil then
    exit;
  for s in FSplines do begin
    if s.IsFewPoints then continue;
    minv := Result.a.Y;
    maxv := Result.b.Y;
    ipfsmm(High(s.FCoeff), FX[s.FFirstCacheIndex], FY[s.FFirstCacheIndex], s.FCoeff[0], minv, maxv, r);
    extY.FStart := Min(minv, extY.FStart);
    extY.FEnd := Max(maxv, extY.FEnd);
    extChg := true;
  end;
  if extChg then begin
    Result.a.Y := extY.FStart;
    Result.b.Y := extY.FEnd;
  end;
  FCachedExtent := Result;
end;

procedure TCubicSplineSeries.FreeSplines;
var
  s: TSpline;
begin
  for s in FSplines do
    s.Free;
  FSplines := nil;
  FCachedExtent := EmptyExtent;
end;

procedure TCubicSplineSeries.GetLegendItems(AItems: TChartLegendItems);
var
  cp: TChartPen;
  p: TSeriesPointer;
begin
  if FPen.Visible and (FPen.Style <> psClear) then
    cp := FPen
  else
    cp := nil;

  if FPointer.Visible then
    p := FPointer
  else
    p := nil;

  AItems.Add(TLegendItemLinePointer.Create(cp, p, LegendTextSingle));
end;

function TCubicSplineSeries.GetNearestPoint(
  const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  s: TSpline;
  r: TNearestPointResults;
  xmin, xmax: Double;
begin
  Result := inherited GetNearestPoint(AParams, AResults);
  if (not Result) and (nptCustom in ToolTargets) and (nptCustom in AParams.FTargets)
  then begin
    if IsEmpty then exit;
    if not RequestValidChartScaling then exit;

    for s in FSplines do begin
      if s.IsFewPoints or (s.FIsUnorderedX and not IsUnorderedVisible) then
        continue;

      if not GetSplineXRange(s, xmin, xmax) then
        continue;
      with TPointsDrawFuncHelper.Create(Self, xmin, xmax, s.FSourceStartIndex, @s.Calculate, Step) do
        try
          if not GetNearestPoint(AParams, r) or
             Result and (AResults.FDist <= r.FDist)
          then
            continue;
          AResults := r;
          AResults.FYIndex := -1;
          Result := true;
        finally
          Free;
        end;
    end;
  end;
end;

function TCubicSplineSeries.GetSplineXRange(ASpline: TSpline;
  out AXMin, AXMax: Double): Boolean;
var
  ext: TDoubleRect;
begin
  ext := FChart.CurrentExtent;

  if (csoExtrapolateLeft in FOptions) and (ASpline = FSplines[0]) then
    AXmin := ext.a.x
  else
    AXmin := Max(ext.a.x, AxisToGraphX(FX[ASpline.FFirstCacheIndex]));

  if AXmin > ext.b.x then
    exit(false);

  if (csoExtrapolateRight in FOptions) and (ASpline = FSplines[High(FSplines)]) then
    AXmax := ext.b.x
  else
    AXmax := Min(ext.b.x, AxisToGraphX(FX[ASpline.FLastCacheIndex]));

  Result := AXMin <= AXMax;
end;

function TCubicSplineSeries.IsUnorderedVisible: Boolean;
begin
  Result := (csoDrawUnorderedX in Options) and BadDataPen.EffVisible;
end;

procedure TCubicSplineSeries.PrepareCoeffs;
var
  i: Integer = 0;
  j: Integer = 0;
  sCount: Integer = 0;
  s: TSpline;
begin
  FreeSplines;
  SetLength(FX, Source.Count);
  SetLength(FY, Source.Count);
  SetLength(FSplines, Source.Count);
  try
    while i < Source.Count do begin
      s := TSpline.Create(self);
      try
        if s.PrepareCoeffs(Source, i, j) then begin
          FSplines[sCount] := s;
          s := nil;
          sCount += 1;
        end;
      finally
        s.Free;
      end;
    end;
    SetLength(FX, j);
    SetLength(FY, j);
  finally
    SetLength(FSplines, sCount);
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

procedure TCubicSplineSeries.SetSplineType(AValue: TCubicSplineType);
begin
  if FSplineType = AValue then exit;
  FSplineType := AValue;
  FreeSplines;
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

procedure TFitSeries.BeginUpdate;
begin
  inherited BeginUpdate;
  inc(FLockFit);
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
        for i := ParamCount-1 downto 0 do
          Result := Result * AX + Param[i];
      end;
    feExp:
      Result := Param[0] * Exp(Param[1] * AX);
    fePower:
      begin
        if (abs(Param[1]) < 1) and (AX < 0) then
          exit;
        Result := Param[0] * Power(AX, Param[1]);
      end;
    feCustom:
      begin
        Result := 0;
        for i := 0 to ParamCount - 1 do
          Result := Result + Param[i] * FFitParams[i].Func(AX, i);
      end;
  end;
end;

procedure TFitSeries.CalcXRange(out AXMin, AXMax: Double);
var
  ext: TDoubleRect;
begin
  if Source.XCount > 0 then begin
    with Source.BasicExtent do begin
      ext.a := AxisToGraph(a);
      ext.b := AxisToGraph(b);
    end;
    NormalizeRect(ext);
    if IsRotated then begin
      AXMin := GraphToAxisY(ext.a.Y);
      AXMax := GraphToAxisY(ext.b.Y);
    end else begin
      AXMin := GraphToAxisX(ext.a.X);
      AXMax := GraphToAxisX(ext.b.X);
    end;
    EnsureOrder(AXMin, AXMax);
    FFitRange.Intersect(AXMin, AXMax);
  end else begin
    AXMin := 0;
    AXMax := Source.Count - 1;
  end;
end;

procedure TFitSeries.Clear;
begin
  inherited;
  InvalidateFitResults;
end;

procedure TFitSeries.Assign(ASource: TPersistent);
begin
  if ASource is TFitSeries then
    with TFitSeries(ASource) do begin
      Self.FAutoFit := FAutoFit;
      Self.FConfidenceLevel := FConfidenceLevel;
      Self.FDrawFitRangeOnly := FDrawFitRangeOnly;
      Self.FUseCombinedExtentY := FUseCombinedExtentY;
      Self.FFitEquation := FFitEquation;
      Self.FFitRange.Assign(FFitRange);
      Self.FFixedParams := FFixedParams;
      Self.ParamCount := GetParamCount;
      Self.Pen := FPen;
      Self.FStep := FStep;
    end;
  inherited Assign(ASource);
end;

constructor TFitSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ToolTargets := [nptPoint, nptCustom];
  FAutoFit := true;
  FUseCombinedExtentY := false;
  FFitEquation := fePolynomial;
  FFitRange := TFitSeriesRange.Create(Self);
  FDrawFitRangeOnly := true;
  FPointer := TSeriesPointer.Create(ParentChart);
  FPen := TChartPen.Create;
  FPen.OnChange := @StyleChanged;
  FStep := DEF_FIT_STEP;
  FConfidenceLevel := 0.95;
  SetParamCount(DEF_FIT_PARAM_COUNT); // Parabolic fit as default.
  InvalidateFitResults;
end;

destructor TFitSeries.Destroy;
begin
  FreeAndNil(FPen);
  FreeAndNil(FFitRange);
  FreeAndNil(FFitStatistics);
  inherited;
end;

procedure TFitSeries.Draw(ADrawer: IChartDrawer);
var
  de : TIntervalList;
begin
  if IsEmpty or (not Active) then exit;
  if not RequestValidChartScaling then exit;

  if FAutoFit then ExecFit;
  ADrawer.SetBrushParams(bsClear, clTAColor);
  ADrawer.Pen := Pen;
  de := PrepareIntervals;
  try
    PrepareGraphPoints(FChart.CurrentExtent, true);
    if (FState = fpsValid) and (FErrCode = fitOK) then
      with TDrawFuncHelper.Create(Self, de, @Calculate, Step) do
        try
          DrawFunction(ADrawer);
        finally
          Free;
        end;
    DrawErrorBars(ADrawer);
    DrawLabels(ADrawer, 0);
    DrawPointers(ADrawer, 0, true);
  finally
    de.Free;
  end;
end;

procedure TFitSeries.EndUpdate;
begin
  inherited EndUpdate;
  dec(FLockFit);
  if (FLockFit = 0) and FAutoFit then
    ExecFit;
end;

function TFitSeries.EquationText: IFitEquationText;
var
  basis: Array of string = nil;
  i: Integer;
begin
  if State = fpsValid then begin
    Result := TFitEquationText.Create;
    Result.TextFormat(Legend.TextFormat).
           NumFormat('%.2f').
           Equation(FitEquation).
           Params(FitParams);
    if FitEquation = feCustom then begin
      SetLength(basis, ParamCount);
      for i:=0 to High(FFitParams) do
        basis[i] := FFitParams[i].CustomFuncName;
      Result.BasisFuncs(basis);
    end;
    if Assigned(FOnFitEquationText) then
      FOnFitEquationText(Self, Result);
  end else
    Result := TFitEmptyEquationText.Create;
end;

function TFitSeries.ErrorMsg: String;
begin
  case ErrCode of
    fitOK                   : Result := '';
    fitDimError             : Result := rsErrFitDimError;
    fitMoreParamsThanValues : Result := rsErrFitMoreParamsThanValues;
    fitNoFitParams          : Result := rsErrFitNoFitParams;
    fitSingular             : Result := rsErrFitSingular;
    fitNoBaseFunctions      : Result := rsErrFitNoBaseFunctions;
    fitOverflow             : Result := rsErrNumericalOverflow;
  else
    raise EChartError.CreateFmt('[%s.ErrorMsg] No message text assigned to error code #%d.',
      [NameOrClassName(self), ord(ErrCode)]);
  end;
end;

procedure TFitSeries.ExecFit;
var
  xmin, xmax: Double;

  function IsValidPoint(AX, AY: Double): Boolean; inline;
  begin
    if Source.XCount > 0 then
      Result := not IsNaN(AX) and not IsNaN(AY) and InRange(AX, xmin, xmax)
    else
      Result := not IsNaN(AY);
  end;

  procedure TryFit;
  var
    i, j, ns, n: Integer;
    xv: array of ArbFloat = nil;
    yv: array of ArbFloat = nil;
    dy: array of ArbFloat = nil;
    yp, yn: Double;
    fitRes: TFitResults;
    hasErrorBars: Boolean;
  begin
    ns := Source.Count;

    CalcXRange(xmin, xmax);
    if xmin = xmax then exit;

    n := 0;
    for i := 0 to ns - 1 do
      with Source.Item[i]^ do
        n += Ord(IsValidPoint(X, Y));

    // Copy data in fit range to temporary arrays.
    SetLength(xv, n);
    SetLength(yv, n);
    hasErrorBars := Source.HasYErrorBars;
    SetLength(dy, IfThen(hasErrorBars, n, 0));
    j := 0;
    for i := 0 to ns - 1 do
      with Source.Item[i]^ do
        if IsValidPoint(X, Y) then begin
          if Source.XCount > 0 then
            xv[j] := TransformX(X)
          else
            xv[j] := TransformX(i);
          yv[j] := TransformY(Y);
          if hasErrorBars and Source.GetYErrorBarLimits(i, yp, yn) then
            dy[j] := abs(TransformY(yp) - TransformY(yn)) / 2;
          j += 1;
        end;

    // Prepare fit parameters
    if not PrepareFitParams then begin
      FErrCode := fitNoBaseFunctions;
      exit;
    end;

    // Execute the polynomial fit; the degree of the polynomial is np - 1.
    try
      fitRes := LinearFit(xv, yv, dy, FFitParams);

      FErrCode := fitRes.ErrCode;
      if fitRes.ErrCode <> fitOK then
        exit;

      // Store values of fit parameters.
      // Note: In case of exponential and power fit equations, the first fitted
      // parameter is the logarithm of the "real" parameter. It needs to be
      // transformed back to real units by exp function. This is done by the
      // getter of the property
      for i:= 0 to High(FFitParams) do
        FFitParams[i].Value := fitRes.ParamValues[i];

     // Analysis of variance, variance-covariance matrix
      FFitStatistics.Free;
      FFitStatistics := TFitStatistics.Create(fitRes, 1 - FConfidenceLevel);

      // State of the fit
      FState := fpsValid;
    except
      FErrCode := fitOverflow;
    end;
  end;

begin
  if (State <> fpsUnknown) or not Active or IsEmpty or (FChart = nil) or
     ([csLoading, csDestroying] * ComponentState <> []) or (FLockFit > 0)
  then
    exit;
  FState := fpsInvalid;
  try
    TryFit;
  finally
    if Assigned(FOnFitComplete) then
      FOnFitComplete(Self);
    UpdateParentChart;
  end;
end;

function TFitSeries.Extent: TDoubleRect;
var
  de : TIntervalList;
begin
  Result := Source.BasicExtent;
  if not FUseCombinedExtentY then exit;
  if IsEmpty or (not Active) then exit;
  if not RequestValidChartScaling then exit;

  if FAutoFit then ExecFit;
  if (FState = fpsValid) and (FErrCode = fitOK) then begin
    de := PrepareIntervals;
    try
      with TDrawFuncHelper.Create(Self, de, @Calculate, Step) do
        try
          CalcAxisExtentY(Result.a.X, Result.b.X, Result.a.Y, Result.b.Y);
        finally
          Free;
        end;
    finally
      de.Free;
    end;
  end;
end;

function TFitSeries.FitParams: TDoubleDynArray;
var
  i: Integer;
begin
  SetLength(Result{%H-}, ParamCount);
  for i := 0 to High(Result) do
    Result[i] := Param[i];
end;

{$IF FPC_FullVersion >= 30004}
procedure TFitSeries.GetConfidenceLimits(AIndex: Integer; out ALower, AUpper: Double);
var
  val, sig, t: Double;
begin
  if not InRange(AIndex, 0, ParamCount - 1) then
    raise EChartError.CreateFmt(SIndexOutOfRange, [NameOrClassName(self), 'GetConfidenceLimits']);

  if FState <> fpsValid then begin
    ALower := NaN;
    AUpper := NaN;
    exit;
  end;

  val := GetParam_RawValue(AIndex);
  sig := GetParam_RawError(AIndex);
  t := FitStatistics.tValue;
  ALower := val - sig*t;
  AUpper := val + sig*t;
  if (FFitEquation in [feExp, fePower]) and (AIndex = 0) then begin
    ALower := exp(ALower);
    AUpper := exp(AUpper);
  end;
end;

procedure TFitSeries.GetInterval(const aX: Double; out AY: Double;
  IsUpper, IsPrediction: Boolean);
var
  x,y: Double;
  dy: Double;
  Offs: Double;
begin
  if FState <> fpsValid then begin
    aY := NaN;
    exit;
  end;

  offs := IfThen(IsPrediction, 1, 0);
  with FitStatistics do begin
    x := TransformX(AX);
    if IsNaN(x) then exit;
    y := Calculate(AX);
    if IsNaN(y) then exit;
    y := TransformY(y);
    dy := tValue * ResidualStdError * sqrt(offs + 1/N + sqr(x - xBar) / SSx);
    if IsUpper then
      AY := y + dy
    else
      AY := y - dy;
    if (FFitEquation in [feExp, fePower]) then AY := exp(AY);
  end;
end;

procedure TFitSeries.GetLowerConfidenceInterval(const AX: Double; out AY: Double);
begin
  GetInterval(AX, AY, false, false);
end;

procedure TFitSeries.GetUpperConfidenceInterval(const AX: Double; out AY: Double);
begin
  GetInterval(AX, AY, true, false);
end;

procedure TFitSeries.GetLowerPredictionInterval(const AX: Double; out AY: Double);
begin
  GetInterval(AX, AY, false, true);
end;

procedure TFitSeries.GetUpperPredictionInterval(const AX: Double; out AY: Double);
begin
  GetInterval(AX, AY, true, true);
end;
{$IFEND}

{ Function removed, but left here commented to show useage of IEquationText.
function TFitSeries.GetFitEquationString(ANumFormat: String; AXText: String;
  AYText: String): String;
begin
  Result := EquationText.NumFormat(ANumFormat).X(AXText).Y(AYText);
end;
}

procedure TFitSeries.GetLegendItems(AItems: TChartLegendItems);
var
  cp: TChartPen;
  p: TSeriesPointer;
  t: String;
begin
  if FPen.Visible and (FPen.Style <> psClear) then
    cp := FPen
  else
    cp := nil;

  if FPointer.Visible then
    p := FPointer
  else
    p := nil;

  if Legend.Format = '' then
    t := Title
  else
    t := Format(Legend.Format, [Title, Index, EquationText.Get]);
  AItems.Add(TLegendItemLinePointer.Create(cp, p, t));
end;

function TFitSeries.GetNearestPoint(
  const AParams: TNearestPointParams; out AResults: TNearestPointResults): Boolean;
var
  de : TIntervalList;
begin
  Result := inherited GetNearestPoint(AParams, AResults);
  if (not Result) and (nptCustom in ToolTargets) and (nptCustom in AParams.FTargets)
  then begin
    if IsEmpty then exit;
    if not RequestValidChartScaling then exit;

    ExecFit;
    if State <> fpsValid then exit(false);
    de := PrepareIntervals;
    try
      with TDrawFuncHelper.Create(Self, de, @Calculate, Step) do
        try
          Result := GetNearestPoint(AParams, AResults);
          if Result then AResults.FYIndex := -1;
        finally
          Free;
        end;
    finally
      de.Free;
    end;
  end;
end;

function TFitSeries.GetParam(AIndex: Integer): Double;
begin
  if not InRange(AIndex, 0, ParamCount - 1) then
    raise EChartError.CreateFmt(SIndexOutOfRange, [NameOrClassName(Self), 'GetParam']);

  if FState <> fpsValid then begin
    Result := NaN;
    exit;
  end;

  if (FFitEquation in [feExp, fePower]) and (AIndex = 0) then
    Result := exp(FFitParams[AIndex].Value)
  else
    Result := FFitParams[AIndex].Value;
end;

function TFitSeries.GetParamCount: Integer;
begin
  Result := Length(FFitParams);
end;

function TFitSeries.GetParamError(AIndex: Integer): Double;
var
  val, sig: Double;
begin
  if not InRange(AIndex, 0, ParamCount - 1) then
    raise EChartError.CreateFmt(SIndexOutOfRange, [NameOrClassName(self), 'GetParamError']);

  Result := NaN;
  if FState <> fpsValid then
    exit;
  sig := GetParam_RawError(AIndex);
  Result := sig;
  if not IsNaN(sig) and (FFitEquation in [feExp, fePower]) and (AIndex = 0) then
  begin
    val := GetParam_RawValue(AIndex);
    Result := (exp(val + sig) - exp(val - sig)) / 2;
  end;
end;

{$IF FPC_FullVersion >= 30004}
function TFitSeries.GetParam_pValue(AIndex: Integer): Double;
var
  t: Double;
begin
  if not InRange(AIndex, 0, ParamCount - 1) then
    raise EChartError.CreateFmt(SIndexOutOfRange, [NameOrClassName(self), 'GetParam_pValue']);

  t := GetParam_tValue(AIndex);
  if IsNaN(t) then
    Result := NaN
  else
    Result := tDist(t, FFitStatistics.DOF, 2);
end;
{$IFEND}

function TFitSeries.GetParam_RawError(AIndex: Integer): Double;
var
  sig2: Double;
begin
  Result := NaN;
  if (FState = fpsValid) and Assigned(FFitStatistics) then begin
    sig2 := FFitStatistics.VarCovar[AIndex, AIndex];
    if not IsNaN(sig2) and (sig2 >= 0) then
      Result := sqrt(sig2);
  end;
end;

function TFitSeries.GetParam_RawValue(AIndex: Integer): Double;
begin
  Result := FFitParams[AIndex].Value;
end;

function TFitSeries.GetParam_tValue(AIndex: Integer): Double;
var
  sig: Double;
begin
  if not InRange(AIndex, 0, ParamCount - 1) then
    raise EChartError.CreateFmt(SIndexOutOfRange, [NameOrClassName(self), 'GetParam_tValue']);

  sig := GetParam_RawError(AIndex);
  if IsNaN(sig) then
    Result := NaN
  else
    Result := GetParam_RawValue(AIndex) / sig;
end;

procedure TFitSeries.InvalidateFitResults;
var
  i: Integer;
begin
  FState := fpsUnknown;
  FreeAndNil(FFitStatistics);
  for i:=0 to High(FFitParams) do FFitParams[i].Value := NaN;
end;

function TFitSeries.IsFixedParamsStored: Boolean;
begin
  Result := FFixedParams <> '';
end;

procedure TFitSeries.Loaded;
begin
  inherited;
  if FAutoFit and (FFitEquation <> feCustom) then ExecFit;
end;

{ FFixedParams contains several items separated by semicolon or bar ('|'). Any
  numerical item will be used as fixed fitting parameter at the given index.
  Non-numerial items are assumed to be variable fitting parameters.
  Examples:
    '0'     --> the first fitting parameter (index 0) is held fixed at 0
    '-;1.0' --> the first parameter is variable, the second one is fixed at 1.0.
                Another way to write this would be
                  ';1.0'   or   'free;1.0'   or   '-|1.0'
  For each fixed parameter the corresponding element of the output list has
  .Fixed = true. Variable parameters are stored in the outpust list as
  .Value = NaN, and .Fixed = false.

  By default, the fit base functions (.Func) are set to a polynomial because
  all built-in fitting types are of this kind.

  In case of custom fitting, the fit base functions become equal to the
  function .CustomFunc defined separately by the method SetFitBasisFunc().
}
function TFitSeries.PrepareFitParams: Boolean;
var
  sl: TStringList;
  i: Integer;
  sep: Char;
begin
  Result := false;

  for i := 0 to High(FFitParams) do begin
    FFitParams[i].Fixed := false;
    FFitParams[i].Value := NaN;
    if FFitEquation <> feCustom then
      FFitParams[i].Func := @FitBaseFunc_Poly
    else begin
      if FFitParams[i].CustomFunc = nil then
        exit;
      FFitParams[i].Func := FFitParams[i].CustomFunc;
    end;
  end;

  if FFixedParams <> '' then begin
    // Extract fixed parameters
    sl := TStringlist.Create;
    try
      sep := ';';
      if pos('|', FFixedParams) > 0 then sep := '|';
      Split(FFixedParams, sl, sep);
      for i := 0 to High(FFitParams) do begin
        if i < sl.Count then
          FFitParams[i].Value := StrToFloatDefSep(sl[i], NaN);
        FFitParams[i].Fixed := not IsNaN(FFitParams[i].Value);
      end;

      // Transform fixed parameters
      if (FFitEquation in [feExp, fePower]) and FFitparams[0].Fixed then
        FFitParams[0].Value := sign(FFitParams[0].Value) * ln(abs(FFitParams[0].Value));
    finally
      sl.Free;
    end;
  end;
  Result := true;
end;

function TFitSeries.PrepareIntervals: TIntervalList;
var
  xmin, xmax: Double;
begin
  Result := TIntervalList.Create;
  try
    CalcXRange(xmin, xmax);
    if DrawFitRangeOnly then begin
      Result.AddRange(NegInfinity, xmin, [ioOpenStart, ioOpenEnd]);
      Result.AddRange(xmax, SafeInfinity, [ioOpenStart, ioOpenEnd]);
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TFitSeries.SetConfidenceLevel(AValue: Double);
begin
  if FConfidenceLevel = AValue then exit;
  FConfidenceLevel := AValue;
  InvalidateFitResults;
  if FAutoFit then
    ExecFit;
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
  if not (FFitEquation in [fePolynomial, feCustom]) then
    SetLength(FFitParams, 2);
  InvalidateFitResults;
  UpdateParentChart;
end;

procedure TFitSeries.SetFitBasisFunc(AIndex: TFitFuncIndex; AFitFunc: TFitFunc;
  AFitFuncName: String);
begin
  if not InRange(AIndex, 0, ParamCount - 1) then
    raise EChartError.CreateFmt(SIndexOutOfRange, [NameOrClassName(self), 'SetFitBasisFunc']);

  FFitParams[AIndex].CustomFuncName := AFitFuncName;  // e.g. 'sin(x)';
  if FFitParams[AIndex].CustomFunc = AFitFunc then
    exit;

  FFitParams[AIndex].CustomFunc := AFitFunc;
  if FFitEquation = feCustom then begin
    InvalidateFitResults;
    UpdateParentChart;
  end;
end;

procedure TFitSeries.SetFitRange(AValue: TChartRange);
begin
  if FFitRange = AValue then exit;
  FFitRange := AValue;
  InvalidateFitResults;
  UpdateParentChart;
end;

procedure TFitSeries.SetFixedParams(AValue: String);
begin
  if FFixedParams = AValue then exit;
  FFixedParams := AValue;
  InvalidateFitResults;
  UpdateParentChart;
end;

procedure TFitSeries.SetParamCount(AValue: Integer);
begin
  if (AValue = ParamCount) or not (FFitEquation in [fePolynomial, feCustom]) then
    exit;
  if AValue <= 0 then
    raise EChartError.Create(rsErrIllegalFitParamCount);
  SetLength(FFitParams, AValue);
  InvalidateFitResults;
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

procedure TFitSeries.SetUseCombinedExtentY(AValue: Boolean);
begin
  if FUseCombinedExtentY = AValue then exit;
  FUseCombinedExtentY := AValue;
  UpdateParentChart;
end;

procedure TFitSeries.SourceChanged(ASender: TObject);
begin
  inherited;
  InvalidateFitResults;
  if FAutoFit then ExecFit;
end;

{ The exponential and power fitting equations can be transformed to a
  polynomial by taking the logarithm:
    feExp:   y = a exp(b*x) ==> ln(y) = ln(a) + b*x
    fePower: y = a*x^b      ==> ln(y) = ln(a) + b*ln(x)
  In each case, the first parameter (a) needs to be transformed back
  after the fitting -- see "ExecFit". }
function TFitSeries.TransformX(AX: Double): Extended;
begin
  if FitEquation in [fePower] then
  begin
    if AX > 0 then
      Result := ln(AX)
    else
      Result := SafeNaN;
  end else
    Result := AX;
end;

function TFitSeries.TransformY(AY: Double): Extended;
begin
  if FitEquation in [feExp, fePower] then
  begin
    if AY > 0 then
      Result := ln(AY)
    else
      Result := SafeNaN;
  end else
    Result := AY;
end;


{ TCustomColorMapSeries }

procedure TCustomColorMapSeries.Assign(ASource: TPersistent);
begin
  if ASource is TCustomColorMapSeries then
    with TCustomColorMapSeries(ASource) do begin
      Self.Brush := FBrush;
      Self.BuiltinPalette := FBuiltinPalette;
      Self.BuiltinPaletteMax := FPaletteMax;
      Self.BuiltinPaletteMin := FPaletteMin;
      Self.ColorSource := FColorSource;
      Self.FInterpolate := FInterpolate;
      Self.FStepX := FStepX;
      Self.FStepY := FStepY;
    end;
  inherited Assign(ASource);
end;

procedure TCustomColorMapSeries.BuildPalette(APalette: TColorMapPalette);
var
  i: Integer;
  h, s, l: Byte;
  cmax, cmin, factor: Double;
  ex: TDoubleRect;
begin
  with FBuiltinColorSource do begin
    BeginUpdate;
    try
      Clear;
      case APalette of
        cmpHot:
          begin
            Add(0, 0, '', clBlack);
            Add(1/3, 0, '', clRed);
            Add(2/3, 0, '', clYellow);
            Add(1, 0, '', clWhite);
          end;
        cmpCold:
          begin
            ColorToHLS(clBlue, h, l, s);
            i := 0;
            while i <= 255 do begin
              Add(i, 0, '', HLSToColor(h, i, s));
              inc(i, 32);
            end;
            Add(255, 0, '', clWhite);
          end;
        cmpRainbow:
          begin
            i := 0;
            while i <= 255 do begin      // i is hue
              Add(i, 0, '', HLSToColor(i, 128, 255));
              inc(i, 32);
            end;
            Add(255, 0, '', HLSToColor(255, 128, 255));
          end;
        cmpMonochrome:
          begin
            i := 0;
            while i <= 255 do begin
              Add(i, 0, '', RgbToColor(i, i, i));
              inc(i, 32);
            end;
            Add(255, 0, '', clWhite);
          end;
      else
        raise EChartError.CreateFmt('[%s.BuildPalette] Palette not supported', [NameOrClassName(Self)]);
      end;

      if FPaletteMin < FPaletteMax then begin
        cmin := FPaletteMin;
        cmax := FPaletteMax;
       end else
      if FPaletteMax < FPaletteMin then begin
        cmin := FPaletteMax;
        cmax := FPaletteMin;
      end else
        exit;

      ex := Extent;
      if (ex.a.x = ex.b.x) then
        exit;
      factor := (cmax - cmin) / (ex.b.x - ex.a.x);
      for i:=0 to Count-1 do
        Item[i]^.X := (Item[i]^.X - ex.a.x) * factor + cmin;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCustomColorMapSeries.CheckColorSource(ASource: TCustomChartSource);
var
  nx, ny: Cardinal;
begin
  if ASource = nil then
    exit;
  GetXYCountNeeded(nx, ny);
  if ASource.XCount < nx then
    raise EXCountError.CreateFmt(rsSourceCountError, [ClassName, nx, 'x']);
  if ASource.YCount < ny then
    raise EYCountError.CreateFmt(rsSourceCountError, [ClassName, ny, 'y']);
end;

function TCustomColorMapSeries.ColorByValue(AValue: Double): TColor;
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

constructor TCustomColorMapSeries.Create(AOwner: TComponent);
const
  BUILTIN_SOURCE_NAME = 'BuiltinColors';
var
  nx, ny: Cardinal;
begin
  inherited Create(AOwner);
  FColorSourceListener := TListener.Create(@FColorSource, @ColorSourceChanged);
  GetXYCountNeeded(nx, ny);
  FBuiltinColorSource := TBuiltinListChartSource.Create(self, nx, ny);
  FBuiltinColorSource.XCount := nx;
  FBuiltinColorSource.YCount := ny;
  FBuiltinColorSource.Name := BUILTIN_SOURCE_NAME;
  FBuiltinColorSource.Broadcaster.Subscribe(FColorSourceListener);
  FBrush := TBrush.Create;
  FBrush.OnChange := @StyleChanged;
  FStepX := DEF_COLORMAP_STEP;
  FStepY := DEF_COLORMAP_STEP;
  SetBuiltinPalette(cmpHot);
end;

destructor TCustomColorMapSeries.Destroy;
begin
  FreeAndNil(FColorSourceListener);
  FreeAndNil(FBuiltinColorSource);
  FreeAndNil(FBrush);
  inherited Destroy;
end;

procedure TCustomColorMapSeries.Draw(ADrawer: IChartDrawer);
var
  ext: TDoubleRect;
//  cext: TDoubleRect;
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
  scaled_stepX: Integer;
  scaled_stepY: Integer;
begin
  if (not (csDesigning in ComponentState) and IsEmpty) or (not Active) then exit;

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

  scaled_stepX := IfThen(StepX > 1, Max(1, ADrawer.Scale(StepX)), 1);
  scaled_stepY := IfThen(StepY > 1, Max(1, ADrawer.Scale(StepY)), 1);

  GetZRange(r, scaled_stepX, scaled_stepY);

  if FColorExtentMin = FColorExtentMax then begin
    ADrawer.FillRect(r.Left, r.Top, r.Right, r.Bottom);
    exit;
  end;

  try
    pt.Y := (r.Top div scaled_stepY - 1) * scaled_stepY + offset.Y mod scaled_stepY;
    while pt.Y <= r.Bottom do begin
      next.Y := pt.Y + scaled_stepY;
      if next.Y <= r.Top then begin
        pt.Y := next.Y;
        continue;
      end;
      pt.X := (r.Left div scaled_stepX - 1) * scaled_stepX + offset.X mod scaled_stepX;
      while pt.X <= r.Right do begin
        next.X := pt.X + scaled_stepX;
        if next.X <= r.Left then begin
          pt.X := next.X;
          continue;
        end;
        gp := GraphToAxis(ParentChart.ImageToGraph((pt + next) div 2));
//        if not (csDesigning in ComponentState) then
          v := FunctionValue(gp.X, gp.Y);
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

function TCustomColorMapSeries.FunctionValue(AX, AY: Double): Double;
begin
  Unused(AX, AY);
  Result := 0.0;
end;

function TCustomColorMapSeries.GetColorSource: TCustomChartSource;
begin
  if Assigned(FColorSource) then
    Result := FColorSource
  else
    Result := FBuiltinColorSource;
end;

procedure TCustomColorMapSeries.GetLegendItems(AItems: TChartLegendItems);

  function PrepareFormats: TStrings;
  begin
    Result := Split(IfThen(Legend.Format = '', DEF_COLORMAP_LEGENDFORMAT, Legend.Format));
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

class procedure TCustomColorMapSeries.GetXYCountNeeded(out AXCount, AYCount: Cardinal);
begin
  AXCount := 1;
  AYCount := 0;
end;

procedure TCustomColorMapSeries.GetZRange(ARect: TRect; dx, dy: Integer);
var
  gp: TDoublePoint;
  ix, iy: Integer;
  z: Double;
  dx2, dy2: Double;
begin
  if IsEmpty then begin
    FMinZ := 0.0;
    FMaxZ := 0.0;
    exit;
  end;

  dx2 := dx div 2;
  dy2 := dy div 2;

  FMinZ := 1E308;
  FMaxZ := -FMinZ;
  iy := ARect.Top;
  try
    while iy <= ARect.Bottom - dy2 do begin
      ix := ARect.Left;
      while ix < ARect.Right - dx2 do begin
        gp := ParentChart.ImageToGraph(Point(ix, iy));
        z := FunctionValue(gp.X + dx2, gp.Y + dy2);
        FMinZ := Min(FMinZ, z);
        FMaxZ := Max(FMaxZ, z);
        inc(ix, dx);
      end;
      inc(iy, dy);
    end;
  except
    FActive := false;
    raise;
  end;
end;

function TCustomColorMapSeries.IsEmpty: Boolean;
begin
  Result := true;
end;

function TCustomColorMapSeries.IsColorSourceStored: boolean;
begin
  Result := FColorSource <> nil;
end;

function TCustomColorMapSeries.IsPaletteMaxStored: Boolean;
begin
  Result := FPaletteMax <> 0;
end;

function TCustomColorMapSeries.IsPaletteMinStored: Boolean;
begin
  Result := FPaletteMin <> 0;
end;

procedure TCustomColorMapSeries.SetBrush(AValue: TBrush);
begin
  if FBrush = AValue then exit;
  FBrush := AValue;
  UpdateParentChart;
end;

procedure TCustomColorMapSeries.SetBuiltinPalette(AValue: TColorMapPalette);
begin
  FBuiltinPalette := AValue;
  BuildPalette(FBuiltinPalette);
end;

procedure TCustomColorMapSeries.SetColorSource(AValue: TCustomChartSource);
begin
  if AValue = FBuiltinColorSource then
    AValue := nil;
  if FColorSource = AValue then
    exit;
  CheckColorSource(AValue);
  if FColorSourceListener.IsListening then
    ColorSource.Broadcaster.Unsubscribe(FColorSourceListener);
  FColorSource := AValue;
  ColorSource.Broadcaster.Subscribe(FColorSourceListener);
  ColorSourceChanged(Self);
end;

procedure TCustomColorMapSeries.SetInterpolate(AValue: Boolean);
begin
  if FInterpolate = AValue then exit;
  FInterpolate := AValue;
  UpdateParentChart;
end;

procedure TCustomColorMapSeries.SetPaletteMax(AValue: Double);
begin
  if AValue = FPaletteMax then exit;
  FPaletteMax := AValue;
  BuildPalette(FBuiltinPalette);
end;

procedure TCustomColorMapSeries.SetPaletteMin(AValue: Double);
begin
  if AValue = FPaletteMin then exit;
  FPaletteMin := AValue;
  BuildPalette(FBuiltinPalette);
end;

procedure TCustomColorMapSeries.SetStepX(AValue: TFuncSeriesStep);
begin
  if FStepX = AValue then exit;
  FStepX := AValue;
  UpdateParentChart;
end;

procedure TCustomColorMapSeries.SetStepY(AValue: TFuncSeriesStep);
begin
  if FStepY = AValue then exit;
  FStepY := AValue;
  UpdateParentChart;
end;

procedure TCustomColorMapSeries.SetUseImage(AValue: TUseImage);
begin
  if FUseImage = AValue then exit;
  FUseImage := AValue;
  UpdateParentChart;
end;

procedure TCustomColorMapSeries.ColorSourceChanged(ASender: TObject);
begin
  if (ASender <> FBuiltinColorSource) and (ASender is TCustomChartSource) then
    try
      CheckColorSource(TCustomChartSource(ASender));
    except
      ColorSource := nil; // revert to built-in source
      raise;
    end;
  UpdateColorExtent;
  StyleChanged(ASender);
end;

procedure TCustomColorMapSeries.UpdateColorExtent;
var
  ext: TDoubleRect;
begin
  ext := ColorSource.Extent;
  FColorExtentMin := ext.a.x;
  FColorExtentMax := ext.b.x;
end;


{ TColorMapSeries }

procedure TColorMapSeries.Assign(ASource: TPersistent);
begin
  if ASource is TColorMapSeries then
    with TCustomColorMapSeries(ASource) do begin
      Self.FOnCalculate := FOnCalculate;
    end;
  inherited;
end;

function TColorMapSeries.FunctionValue(AX, AY: Double): Double;
begin
  if Assigned(OnCalculate) then
    OnCalculate(AX, AY, Result)
  else
    Result := 0;
end;

function TColorMapSeries.IsEmpty: Boolean;
begin
  Result := not Assigned(OnCalculate);
end;

procedure TColorMapSeries.SetOnCalculate(AValue: TFuncCalculate3DEvent);
begin
  if TMethod(FOnCalculate) = TMethod(AValue) then exit;
  FOnCalculate := AValue;
  UpdateParentChart;
end;


initialization
  RegisterSeriesClass(TParametricCurveSeries, @rsParametricCurveSeries);
  RegisterSeriesClass(TBSplineSeries, @rsBSplineSeries);
  RegisterSeriesClass(TCubicSplineSeries, @rsCubicSplineSeries);
  RegisterSeriesClass(TFitSeries, @rsLeastSquaresFitSeries);
  RegisterSeriesClass(TFuncSeries, @rsFunctionSeries);
  RegisterSeriesClass(TColorMapSeries, @rsColorMapSeries);

end.

