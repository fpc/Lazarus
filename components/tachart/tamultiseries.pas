{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}

unit TAMultiSeries;

{$MODE ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

interface

uses
  Classes, Graphics,
  TAChartUtils, TATypes, TACustomSource, TACustomSeries, TADrawUtils, TALegend;

const
  DEF_BOX_WIDTH = 50;
  DEF_WHISKERS_WIDTH = 25;
  DEF_OHLC_TICK_WIDTH = 25;
  
  DEF_YINDEX_OPEN = 1;
  DEF_YINDEX_HIGH = 3;
  DEF_YINDEX_LOW = 0;
  DEF_YINDEX_CLOSE = 2;
  
  DEF_YINDEX_WHISKERMIN = 0;
  DEF_YINDEX_BOXMIN = 1;
  DEF_YINDEX_CENTER = 2;
  DEF_YINDEX_BOXMAX = 3;
  DEF_YINDEX_WHISKERMAX = 4;

type

  // TBubbleRadiusTransform = (brtNone, brtX, brtY); not used
  TBubbleOverrideColor = (bocBrush, bocPen);
  TBubbleOverrideColors = set of TBubbleOverrideColor;
  TBubbleRadiusUnits = (
    bruX,                // Circle with radius given in x axis units
    bruY,                // Circle with radius given in y axis units
    bruXY,               // Ellipse
    bruPercentageRadius, // Bubble radius is percentage of the smallest dimension of plot area
    bruPercentageArea    // dto., but bubble area
  );

  { TBubbleSeries }

  TBubbleSeries = class(TBasicPointSeries)
  private
    FBubbleBrush: TBrush;
    FBubblePen: TPen;
    FOverrideColor: TBubbleOverrideColors;
    FBubbleRadiusPercentage: Integer;
    FBubbleRadiusUnits: TBubbleRadiusUnits;
    FBubbleScalingFactor: Double;
    procedure SetBubbleBrush(AValue: TBrush);
    procedure SetBubblePen(AValue: TPen);
    procedure SetBubbleRadiusPercentage(AValue: Integer);
    procedure SetBubbleRadiusUnits(AValue: TBubbleRadiusUnits);
    procedure SetOverrideColor(AValue: TBubbleOverrideColors);
  protected
    function CalcBubbleScalingFactor(const ARect: TRect): Double;
    function GetBubbleRect(AItem: PChartDataItem; AFactor: Double; out ARect: TRect): Boolean;
    function GetLabelDataPoint(AIndex, AYIndex: Integer): TDoublePoint; override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetSeriesColor: TColor; override;
    class procedure GetXYCountNeeded(out AXCount, AYCount: Cardinal); override;
    function ToolTargetDistance(const AParams: TNearestPointParams;
      AGraphPt: TDoublePoint; APointIdx, AXIdx, AYIdx: Integer): Integer; override;
    procedure UpdateLabelDirectionReferenceLevel(AIndex, AYIndex: Integer;
      var ALevel: Double); override;
    procedure UpdateMargins(ADrawer: IChartDrawer; var AMargins: TRect); override;
  public
    function AddXY(AX, AY, ARadius: Double; AXLabel: String = '';
      AColor: TColor = clTAColor): Integer; overload;
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(ADrawer: IChartDrawer); override;
    function Extent: TDoubleRect; override;
    function GetNearestPoint(const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
    procedure MovePointEx(var AIndex: Integer; AXIndex, AYIndex: Integer;
      const ANewPos: TDoublePoint); override;
  published
    property AxisIndexX;
    property AxisIndexY;
    property BubbleBrush: TBrush read FBubbleBrush write SetBubbleBrush;
    property BubblePen: TPen read FBubblePen write SetBubblePen;
    property BubbleRadiusPercentage: Integer read FBubbleRadiusPercentage
      write SetBubbleRadiusPercentage default 20;
    property BubbleRadiusUnits: TBubbleRadiusUnits read FBubbleRadiusUnits
      write SetBubbleRadiusUnits default bruXY;
    property MarkPositions;
    property Marks;
    property OverrideColor: TBubbleOverrideColors
      read FOverrideColor write SetOverrideColor default [];
    property Source;
    property Styles;
    property ToolTargets default [nptPoint, nptYList, nptCustom];
  end;

  TBoxAndWhiskerSeriesLegendDir = (bwlHorizontal, bwlVertical, bwlAuto);
  TBoxAndWhiskerSeriesWidthStyle = (bwsPercent, bwsPercentMin);
  TBoxAndWhiskerYDataLayout = (bwlNormal, bwlLegacy, bwlCustom);

  TBoxAndWhiskerSeries = class(TBasicPointSeries)
  strict private
    FBoxBrush: TBrush;
    FBoxPen: TPen;
    FBoxWidth: Integer;
    FLegendDirection: TBoxAndWhiskerSeriesLegendDir;
    FMedianPen: TPen;
    FWhiskersPen: TPen;
    FWhiskersWidth: Integer;
    FWidthStyle: TBoxAndWhiskerSeriesWidthStyle;
    FYDataLayout: TBoxAndWhiskerYDataLayout;
    FYIndexWhiskerMin: Integer;
    FYIndexBoxMin: Integer;
    FYIndexCenter: Integer;
    FYIndexBoxMax: Integer;
    FYIndexWhiskerMax: Integer;
    procedure SetBoxBrush(AValue: TBrush);
    procedure SetBoxPen(AValue: TPen);
    procedure SetBoxWidth(AValue: Integer);
    procedure SetLegendDirection(AValue: TBoxAndWhiskerSeriesLegendDir);
    procedure SetMedianPen(AValue: TPen);
    procedure SetWhiskersPen(AValue: TPen);
    procedure SetWhiskersWidth(AValue: Integer);
    procedure SetYDataLayout(AValue: TBoxAndWhiskerYDataLayout);
    procedure SetYIndexBoxMax(AValue: Integer);
    procedure SetYIndexBoxMin(AValue: Integer);
    procedure SetYIndexCenter(AValue: Integer);
    procedure SetYIndexWhiskerMax(AValue: Integer);
    procedure SetYIndexWhiskerMin(AValue: Integer);
    procedure UpdateYDataLayout;
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetSeriesColor: TColor; override;
    class procedure GetXYCountNeeded(out AXCount, AYCount: Cardinal); override;
    function SkipMissingValues(AIndex: Integer): Boolean; override;
    function ToolTargetDistance(const AParams: TNearestPointParams;
      AGraphPt: TDoublePoint; APointIdx, AXIdx, AYIdx: Integer): Integer; override;
    procedure UpdateLabelDirectionReferenceLevel(AIndex, AYIndex: Integer;
      var ALevel: Double); override;
  public
    function AddXY(
      AX, AYLoWhisker, AYLoBox, AY, AYHiBox, AYHiWhisker: Double;
      AXLabel: String = ''; AColor: TColor = clTAColor): Integer; overload;
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(ADrawer: IChartDrawer); override;
    function Extent: TDoubleRect; override;
    function GetNearestPoint(const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
  published
    property BoxBrush: TBrush read FBoxBrush write SetBoxBrush;
    property BoxPen: TPen read FBoxPen write SetBoxPen;
    property BoxWidth: Integer
      read FBoxWidth write SetBoxWidth default DEF_BOX_WIDTH;
    property LegendDirection: TBoxAndWhiskerSeriesLegendDir
      read FLegendDirection write SetLegendDirection default bwlHorizontal;
    property MedianPen: TPen read FMedianPen write SetMedianPen;
    property ToolTargets default [nptPoint, nptYList, nptCustom];
    property WidthStyle: TBoxAndWhiskerSeriesWidthStyle
      read FWidthStyle write FWidthStyle default bwsPercent;
    property WhiskersPen: TPen read FWhiskersPen write SetWhiskersPen;
    property WhiskersWidth: Integer
      read FWhiskersWidth write SetWhiskersWidth default DEF_WHISKERS_WIDTH;
    property YDataLayout: TBoxAndWhiskerYDataLayout
      read FYDataLayout write SetYDataLayout default bwlLegacy;
    property YIndexBoxMax: Integer
      read FYIndexBoxMax write SetYIndexBoxMax default DEF_YINDEX_BOXMAX;
    property YIndexBoxMin: Integer
      read FYIndexBoxMin write SetYIndexBoxMin default DEF_YINDEX_BOXMIN;
    property YIndexCenter: Integer
      read FYIndexCenter write SetYIndexCenter default DEF_YINDEX_CENTER;
    property YIndexWhiskerMax: Integer
      read FYIndexWhiskerMax write SetYIndexWhiskerMax default DEF_YINDEX_WHISKERMAX;
    property YIndexWhiskerMin: Integer
      read FYIndexWhiskerMin write SetYIndexWhiskerMin default DEF_YINDEX_WHISKERMIN;
  published
    property AxisIndexX;
    property AxisIndexY;
    property MarkPositions;
    property Marks;
    property Source;
  end;

  TOHLCBrushKind = (obkCandleUp, obkCandleDown);
  TOHLCPenKind = (opkCandleUp, opkCandleDown, opkCandleLine, opkLineUp, opkLineDown);
  TOHLCMode = (mOHLC, mCandleStick);
  TTickWidthStyle = (twsPercent, twsPercentMin);

  TOpenHighLowCloseSeries = class(TBasicPointSeries)
  private
    const
      OHLC_BRUSH_COLORS: array[TOHLCBrushKind] of TColor = (clLime, clRed);
      OHLC_PEN_COLORS: array[TOHLCPenKind] of TColor = (clGreen, clMaroon, clDefault, clLime, clRed);
  private
    FPen: array[TOHLCPenKind] of TPen;
    FBrush: array[TOHLCBrushKind] of TBrush;
    FTickWidth: Integer;
    FTickWidthStyle: TTickWidthStyle;
    FYIndexClose: Integer;
    FYIndexHigh: Integer;
    FYIndexLow: Integer;
    FYIndexOpen: Integer;
    FMode: TOHLCMode;
    function GetBrush(AIndex: TOHLCBrushKind): TBrush;
    function GetPen(AIndex: TOHLCPenKind): TPen;
    function IsBrushStored(AIndex: TOHLCBrushKind): Boolean;
    function IsPenStored(AIndex: TOHLCPenKind): Boolean;
    procedure SetBrush(AIndex: TOHLCBrushKind; AValue: TBrush);
    procedure SetPen(AIndex: TOHLCPenKind; AValue: TPen);
    procedure SetOHLCMode(AValue: TOHLCMode);
    procedure SetTickWidth(AValue: Integer);
    procedure SetTickWidthStyle(AValue: TTickWidthStyle);
    procedure SetYIndexClose(AValue: Integer);
    procedure SetYIndexHigh(AValue: Integer);
    procedure SetYIndexLow(AValue: Integer);
    procedure SetYIndexOpen(AValue: Integer);
  protected
    function CalcTickWidth(AX: Double; AIndex: Integer): Double;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetSeriesColor: TColor; override;
    class procedure GetXYCountNeeded(out AXCount, AYCount: Cardinal); override;
    function SkipMissingValues(AIndex: Integer): Boolean; override;
    function ToolTargetDistance(const AParams: TNearestPointParams;
      AGraphPt: TDoublePoint; APointIdx, AXIdx, AYIdx: Integer): Integer; override;
    procedure UpdateLabelDirectionReferenceLevel(AIndex, AYIndex: Integer;
      var ALevel: Double); override;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function AddXOHLC(
      AX, AOpen, AHigh, ALow, AClose: Double;
      ALabel: String = ''; AColor: TColor = clTAColor): Integer; inline;
    procedure Draw(ADrawer: IChartDrawer); override;
    function Extent: TDoubleRect; override;
    function GetNearestPoint(const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
  published
    property CandlestickDownBrush: TBrush index obkCandleDown
      read GetBrush write SetBrush stored IsBrushStored;
    property CandlestickDownPen: TPen index opkCandleDown
      read GetPen write SetPen stored IsPenStored;
    property CandlestickLinePen: TPen index opkCandleLine
      read GetPen write SetPen stored IsPenStored;
    property CandlestickUpBrush: TBrush index obkCandleUp
      read GetBrush write SetBrush stored IsBrushStored;
    property CandlestickUpPen: TPen index opkCandleUp
      read GetPen write SetPen stored IsPenStored;
    property DownLinePen: TPen index opkLineDown
      read GetPen write SetPen stored IsPenStored;
    property LinePen: TPen index opkLineUp
      read GetPen write SetPen stored IsPenStored;
    property Mode: TOHLCMode read FMode write SetOHLCMode default mOHLC;
    property TickWidth: integer
      read FTickWidth write SetTickWidth default DEF_OHLC_TICK_WIDTH;
    property TickWidthStyle: TTickWidthStyle
      read FTickWidthStyle write SetTickWidthStyle default twsPercent;
    property ToolTargets default [nptPoint, nptYList, nptCustom];
    property YIndexClose: integer
      read FYIndexClose write SetYIndexClose default DEF_YINDEX_CLOSE;
    property YIndexHigh: Integer
      read FYIndexHigh write SetYIndexHigh default DEF_YINDEX_HIGH;
    property YIndexLow: Integer
      read FYIndexLow write SetYIndexLow default DEF_YINDEX_LOW;
    property YIndexOpen: Integer
      read FYIndexOpen write SetYIndexOpen default DEF_YINDEX_OPEN;
  published
    property AxisIndexX;
    property AxisIndexY;
    property MarkPositions;
    property Marks;
    property Source;
  end;

  TVectorCoordKind = (vckCenterDir, vckStartEnd);

  TFieldSeries = class(TBasicPointSeries)
  private
    FArrow: TChartArrow;
    FPen: TPen;
    FCoordKind: TVectorCoordKind;
    procedure SetArrow(AValue: TChartArrow);
    procedure SetCoordKind(AValue: TVectorCoordKind);
    procedure SetPen(AValue: TPen);
  protected
    procedure AfterAdd; override;
    procedure DrawVector(ADrawer: IChartDrawer; AStartPt, AEndPt: TDoublePoint;
      APen: TPen);
    function GetColor(AIndex: Integer): TColor; inline;
    function GetVectorPoints(AIndex: Integer;
      out AStartPt, AEndPt: TDoublePoint): Boolean; inline;
    class procedure GetXYCountNeeded(out AXCount, AYCount: Cardinal); override;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddVector(AX, AY, AVectorX, AVectorY: Double; AXLabel: String = '';
      AColor: TColor = clTAColor): Integer; //inline;
    function GetVector(AIndex: Integer): TDoublePoint; inline;
    procedure SetVector(AIndex: Integer; const AValue: TDoublePoint); inline;

    procedure Draw(ADrawer: IChartDrawer); override;
    function Extent: TDoubleRect; override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetNearestPoint(const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
    procedure MovePointEx(var AIndex: Integer; AXIndex, AYIndex: Integer;
      const ANewPos: TDoublePoint); override;
    procedure NormalizeVectors(ALength: Double);

  published
    property Arrow: TChartArrow read FArrow write SetArrow;
    property AxisIndexX;
    property AxisIndexY;
    property MarkPositions;
    property Marks;
    property Pen: TPen read FPen write SetPen;
    property Source;
    property ToolTargets default [nptPoint, nptXList, nptYList, nptCustom];
    property VectorCoordKind: TVectorCoordKind read FCoordKind write SetCoordKind default vckCenterDir;
  end;

implementation

uses
  FPCanvas, Math, SysUtils, Types,
  TAChartStrConsts, TAGeometry, TAGraph, TAMath;

type

  TLegendItemOHLCLine = class(TLegendItemLine)
  strict private
    FMode: TOHLCMode;
    FCandleStickUpColor: TColor;
    FCandleStickDownColor: TColor;
  public
    constructor Create(ASeries: TOpenHighLowCloseSeries; const AText: String);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
  end;

  TLegendItemBoxAndWhiskers = class(TLegendItem)
  strict private
    FBoxBrush: TBrush;
    FBoxPen: TPen;
    FBoxWidth: Integer;
    FIsVertical: Boolean;
    FMedianPen: TPen;
    FWhiskersPen: TPen;
    FWhiskersWidth: Integer;
  public
    constructor Create(ASeries: TBoxAndWhiskerSeries; const AText: String);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
  end;

  TLegendItemField = class(TLegendItemLine)
  strict private
    FArrow: TChartArrow;
  public
    constructor Create(APen: TPen; AArrow: TChartArrow; const AText: String);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
  end;

{ TLegendItemOHLCLine }

constructor TLegendItemOHLCLine.Create(ASeries: TOpenHighLowCloseSeries; const AText: String);
var
  pen: TFPCustomPen;
begin
  case ASeries.Mode of
    mOHLC        : pen := ASeries.LinePen;
    mCandleStick : pen := ASeries.CandleStickLinePen;
  end;
  inherited Create(pen, AText);
  FMode := ASeries.Mode;
  FCandlestickUpColor := ASeries.CandlestickUpBrush.Color;
  FCandlestickDownColor := ASeries.CandlestickDownBrush.Color;
end;

procedure TLegendItemOHLCLine.Draw(ADrawer: IChartDrawer; const ARect: TRect);
const
  TICK_LENGTH = 3;
var
  dx, dy, x, y: Integer;
  pts: array[0..3] of TPoint;
begin
  inherited Draw(ADrawer, ARect);
  y := (ARect.Top + ARect.Bottom) div 2;
  dx := (ARect.Right - ARect.Left) div 3;
  x := ARect.Left + dx;
  case FMode of
    mOHLC:
      begin
        dy := ADrawer.Scale(TICK_LENGTH);
        ADrawer.Line(x, y, x, y + dy);
        x += dx;
        ADrawer.Line(x, y, x, y - dy);
      end;
    mCandlestick:
      begin
        dy := (ARect.Bottom - ARect.Top) div 4;
        pts[0] := Point(x, y-dy);
        pts[1] := Point(x, y+dy);
        pts[2] := Point(x+dx, y+dy);
        pts[3] := pts[0];
        ADrawer.SetBrushParams(bsSolid, FCandlestickUpColor);
        ADrawer.Polygon(pts, 0, 4);
        pts[0] := Point(x+dx, y+dy);
        pts[1] := Point(x+dx, y-dy);
        pts[2] := Point(x, y-dy);
        pts[3] := pts[0];
        ADrawer.SetBrushParams(bsSolid, FCandlestickDownColor);
        ADrawer.Polygon(pts, 0, 4);
      end;
  end;
end;

{ TLegendItemBoxAndWhiskers }

constructor TLegendItemBoxAndWhiskers.Create(
  ASeries: TBoxAndWhiskerSeries; const AText: String);
begin
  inherited Create(AText);
  with ASeries do begin
    FBoxBrush := BoxBrush;
    FBoxPen := BoxPen;
    FBoxWidth := BoxWidth;
    FIsVertical :=
      (LegendDirection = bwlVertical) or
      (LegendDirection = bwlAuto) and IsRotated;
    FMedianPen := MedianPen;
    FWhiskersPen := WhiskersPen;
    FWhiskersWidth := WhiskersWidth;
  end;
end;

procedure TLegendItemBoxAndWhiskers.Draw(
  ADrawer: IChartDrawer; const ARect: TRect);

  function FlipRect(const AR: TRect): TRect;
  begin
    Result := Rect(AR.Top, AR.Left, AR.Bottom, AR.Right);
  end;

var
  symbol: array [1..5] of TRect;
var
  center: TPoint;
  i, m, ww, bw: Integer;
  r: TRect;
begin
  inherited Draw(ADrawer, ARect);
  r := ARect;
  r.BottomRight -= Point(1, 1);
  if FIsVertical then
    r := FlipRect(r);

  center := (r.TopLeft + r.BottomRight) div 2;
  m := MaxValue([FWhiskersWidth, FBoxWidth, 1]) * 2;
  ww := (r.Bottom - r.Top) * FWhiskersWidth div m;
  symbol[1] := Rect(r.Left, center.y, r.Right, center.y);
  symbol[2] := Rect(r.Left, center.y - ww, r.Left, center.y + ww + 1);
  symbol[3] := Rect(r.Right, center.y - ww, r.Right, center.y + ww + 1);
  bw := (r.Bottom - r.Top) * FBoxWidth div m;
  symbol[4] := Rect(
    (r.Left * 2 + r.Right) div 3, center.y - bw,
    (r.Left + r.Right * 2) div 3, center.y + bw);
  bw -= Math.IfThen(FBoxPen.Style = psClear, 0, (FBoxPen.Width + 1) div 2);
  symbol[5] := Rect(center.x, center.y - bw, center.x, center.y + bw);

  if FIsVertical then
    for i := 1 to High(symbol) do
      symbol[i] := FlipRect(symbol[i]);

  // Whisker
  ADrawer.Pen := FWhiskersPen;
  ADrawer.SetPenColor(FWhiskersPen.Color);
  ADrawer.SetBrushParams(bsClear, clTAColor);
  for i := 1 to 3 do
    ADrawer.Line(symbol[i].TopLeft, symbol[i].BottomRight);

  // Box
  ADrawer.Pen := FBoxPen;
  ADrawer.SetPenColor(FBoxPen.Color);
  ADrawer.Brush:= FBoxBrush;
  ADrawer.SetBrushColor(FBoxBrush.Color);
  ADrawer.Rectangle(symbol[4]);

  // Median line
  ADrawer.Pen := FMedianPen;
  ADrawer.SetPenColor(FMedianPen.Color);
  ADrawer.Line(symbol[5].TopLeft, symbol[5].BottomRight);
end;

{ TLegendItemField }

constructor TLegendItemField.Create(APen: TPen; AArrow: TChartArrow;
  const AText: String);
begin
  inherited Create(APen, AText);
  FArrow := AArrow;
end;

procedure TLegendItemField.Draw(ADrawer: IChartDrawer; const ARect: TRect);
var
  y: Integer;
  len: Double;
  arr: TChartArrow;
begin
  inherited Draw(ADrawer, ARect);
  if (FPen = nil) or (FArrow = nil) or not FArrow.Visible then
    exit;
  len := (ARect.Right - ARect.Left) * 0.01;
  arr := TChartArrow.Create(nil);
  try
    arr.Assign(FArrow);
    arr.SetOwner(nil);
    arr.BaseLength := round(FArrow.BaseLength * len);
    arr.Length := round(FArrow.Length * len);
    arr.Width := round(FArrow.Width * len);
    y := (ARect.Top + ARect.Bottom) div 2;
    arr.Draw(ADrawer, Point(ARect.Right, y), 0, FPen);
  finally
    arr.Free;
  end;
end;


{ TBubbleSeries }

function TBubbleSeries.AddXY(AX, AY, ARadius: Double; AXLabel: String;
  AColor: TColor): Integer;
begin
  Result := AddXY(AX, AY, [ARadius], AXLabel, AColor);
end;

procedure TBubbleSeries.Assign(ASource: TPersistent);
begin
  if ASource is TBubbleSeries then
    with TBubbleSeries(ASource) do begin
      Self.BubbleBrush := FBubbleBrush;
      Self.BubblePen := FBubblePen;
      Self.BubbleRadiusUnits := FBubbleRadiusUnits;
      Self.BubbleRadiusPercentage := FBubbleRadiusPercentage;
      Self.OverrideColor := FOverrideColor;
    end;
  inherited Assign(ASource);
end;

{ Adjusts a scaling factor such that the largest bubble radius is the
  given percentage (FBubbleRadiusPercentage) of the smallest edge of
  the chart area (ARect). }
function TBubbleSeries.CalcBubbleScalingFactor(const ARect: TRect): Double;
var
  rMin: Double = 0.0;
  rMax: Double = 0.0;
begin
  if FBubbleRadiusUnits in [bruPercentageRadius, bruPercentageArea] then
  begin
    Source.YRange(1, rMin, rMax);
    if FBubbleRadiusUnits = bruPercentageArea then
      rMax := sqrt(abs(rMax));
    Result := Min(ARect.Width, ARect.Height) * FBubbleRadiusPercentage * PERCENT / abs(rMax);
  end else
    Result := 1.0;
end;

constructor TBubbleSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ToolTargets := [nptPoint, nptYList, nptCustom];
  FBubblePen := TPen.Create;
  FBubblePen.OnChange := @StyleChanged;
  FBubbleBrush := TBrush.Create;
  FBubbleBrush.OnChange := @StyleChanged;
  FBubbleRadiusPercentage := 20;
  FBubbleRadiusUnits := bruXY;
  FBubbleScalingFactor := 1.0;
end;

destructor TBubbleSeries.Destroy;
begin
  FreeAndNil(FBubbleBrush);
  FreeAndNil(FBubblePen);
  inherited Destroy;
end;

procedure TBubbleSeries.Draw(ADrawer: IChartDrawer);
var
  i: Integer;
  item: PChartDataItem;
  clipR: TRect;
  irect: TRect;
  dummyR: TRect = (Left:0; Top:0; Right:0; Bottom:0);
  ext: TDoubleRect;
  nx, ny: Cardinal;
begin
  if IsEmpty or (not Active) then exit;
  if not RequestValidChartScaling then exit;

  ADrawer.Pen := BubblePen;
  if BubblePen.Color = clDefault then
    ADrawer.SetPenColor(FChart.GetDefaultColor(dctFont))
  else
    ADrawer.SetPenColor(BubblePen.Color);
  ADrawer.Brush := BubbleBrush;
  if BubbleBrush.Color = clDefault then
    ADrawer.SetBrushColor(FChart.GetDefaultColor(dctBrush))
  else
    ADrawer.SetBrushColor(BubbleBrush.Color);

  ext := ParentChart.CurrentExtent;
  clipR.TopLeft := ParentChart.GraphToImage(ext.a);
  clipR.BottomRight := ParentChart.GraphToImage(ext.b);
  NormalizeRect(clipR);
  ADrawer.ClippingStart(clipR);

  FBubbleScalingFactor := CalcBubbleScalingFactor(clipR);

  for i := 0 to Count - 1 do begin
    item := Source[i];
    if not GetBubbleRect(item, FBubbleScalingFactor, irect) then
      continue;
    if not IntersectRect(dummyR, clipR, irect) then
      continue;
    if bocPen in OverrideColor then
      ADrawer.SetPenParams(BubblePen.Style, ColorDef(item^.Color, BubblePen.Color));
    if bocBrush in OverrideColor then
      ADrawer.SetBrushColor(ColorDef(item^.Color, BubbleBrush.Color));

    if Styles <> nil then
      Styles.Apply(ADrawer, i);

    ADrawer.Ellipse(irect.Left, irect.Top, irect.Right, irect.Bottom);
  end;

  GetXYCountNeeded(nx, ny);
  if Source.YCount >= ny then
    for i := 0 to ny - 1 do DrawLabels(ADrawer, i)
  else
    DrawLabels(ADrawer);

  ADrawer.ClippingStop;
end;

{ Calculates the extent of the series such that bubbles are not clipped.
  But note that this method is correct only for BubbleRadiusUnits bruXY, it
  would crash for bruX and bruY. Adjust Chart.Margins or Chart.ExpandPercentage
  in these cases. }
function TBubbleSeries.Extent: TDoubleRect;
var
  i: Integer;
  r: Double;
  sp, gp, gq, rp: TDoublePoint;
  item: PChartDataItem;
begin
  Result := EmptyExtent;
  if IsEmpty then exit;
  if not RequestValidChartScaling then exit;

  if FBubbleRadiusUnits = bruXY then
  begin
    for i := 0 to Count - 1 do begin
      item := Source[i];
      sp := item^.Point;
      if TAChartUtils.IsNaN(sp) then
        continue;
      r := item^.YList[0];
      if Math.IsNaN(r) then
        continue;
      rp := DoublePoint(r, r);
      gp := AxisToGraph(sp);
      gq := AxisToGraph(sp + rp);
      rp := gq - gp;

      Result.a.X := Min(Result.a.X, sp.x - rp.x);
      Result.b.X := Max(Result.b.X, sp.x + rp.x);
      Result.a.Y := Min(Result.a.Y, sp.y - rp.y);
      Result.b.Y := Max(Result.b.Y, sp.y + rp.y);
    end;
  end else
    Result := Source.BasicExtent;
end;

function TBubbleSeries.GetBubbleRect(AItem: PChartDataItem;
  AFactor: Double; out ARect: TRect): Boolean;
var
  sp: TDoublePoint;    // source point in axis units
  p: TPoint;           // bubble center in image units
  q: TPoint;           // bubble center offset by 1 radius, in image units
  r: Double;           // radius in axis units
  ri: Integer;         // radius in image units
begin
  Result := false;
  sp := AItem^.Point;
  if TAChartUtils.IsNaN(sp) then
    exit;
  r := AItem^.YList[0];
  if Math.IsNaN(r) then
    exit;

  case FBubbleRadiusUnits of
    bruX:
      begin
        p := ParentChart.GraphToImage(AxisToGraph(sp));
        q := ParentChart.GraphToImage(AxisToGraph(sp + DoublePoint(r, 0)));  // offset along x
        if IsRotated then ri := q.y - p.y else ri := q.x - p.x;
        ARect := Rect(p.x - ri, p.y - ri, p.x + ri, p.y + ri);
      end;
    bruY:
      begin
        p := ParentChart.GraphToImage(AxisToGraph(sp));
        q := ParentChart.GraphToImage(AxisToGraph(sp + DoublePoint(0, r)));  // offset along y
        if IsRotated then ri := q.x - p.x else ri := q.y - p.y;
        ARect := Rect(p.x - ri, p.y - ri, p.x + ri, p.y + ri);
      end;
    bruXY:
      begin
        ARect.TopLeft := ParentChart.GraphToImage(AxisToGraph(DoublePoint(sp.x - r, sp.y - r)));
        ARect.BottomRight := ParentChart.GraphToImage(AxisToGraph(DoublePoint(sp.x + r, sp.y + r)));
      end;
    bruPercentageRadius:
      begin
        p := ParentChart.GraphToImage(AxisToGraph(sp));
        ri := round(r * AFactor);
        ARect := Rect(p.x - ri, p.y - ri, p.x + ri, p.y + ri);
      end;
    bruPercentageArea:
      begin
        p := ParentChart.GraphToImage(AxisToGraph(sp));
        ri := round(sqrt(abs(r)) * AFactor);
        ARect := Rect(p.x - ri, p.y - ri, p.x + ri, p.y + ri);
      end;
  end;
  NormalizeRect(ARect);
  Result := true;
end;

function TBubbleSeries.GetLabelDataPoint(AIndex, AYIndex: Integer): TDoublePoint;
const
  DIRECTION: array [Boolean, Boolean] of TLabelDirection =
    ((ldTop, ldBottom), (ldRight, ldLeft));
  IS_NEGATIVE: array[TLinearMarkPositions] of boolean =
    (true,        false,       true,        false);
    //lmpOutside, lmpPositive, lmpNegative, lmpInside
var
  R: TRect;
  RArray: array[0..3] of Integer absolute R;
  isneg: Boolean;
  dir: TLabelDirection;
begin
  if (AYIndex = 1) and GetBubbleRect(Source.Item[AIndex + FLoBound], FBubbleScalingFactor, R) then begin
    isNeg := IS_NEGATIVE[MarkPositions];
    if Assigned(GetAxisY) then
      if (IsRotated and ParentChart.IsRightToLeft) xor GetAxisY.Inverted then
        isNeg := not isNeg;
    dir := DIRECTION[IsRotated, isNeg];
    if IsRotated then
      Result := ParentChart.ImageToGraph(Point(RArray[ord(dir)], (R.Top + R.Bottom) div 2))
    else
      Result := ParentChart.ImageToGraph(Point((R.Left + R.Right) div 2, RArray[ord(dir)]));
  end else
    Result := GetGraphPoint(AIndex, 0, 0);
end;

procedure TBubbleSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  GetLegendItemsRect(AItems, BubbleBrush, BubblePen);
end;

function TBubbleSeries.GetNearestPoint(const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  i: Integer;
  item: PChartDataItem;
  iRect: TRect;
  p: TPoint;
  dperim: Integer;      // Distance of perimeter point from center of bubble
  d, dist: Integer;
  phi: Double;
  rx, ry: Integer;
  cosphi, sinphi: Math.float;
begin
  Result := inherited;
  if IsEmpty or not RequestValidChartScaling then exit;
  if Result and (nptPoint in AParams.FTargets) and (nptPoint in ToolTargets) then
    if (AResults.FYIndex = 0) then
      exit;

  if Result and (nptYList in AParams.FTargets) and (nptYList in ToolTargets) then
    if (AResults.FYIndex = 1) then begin
      item := Source[AResults.FIndex];
      GetBubbleRect(item, FBubbleScalingFactor, iRect);
      rx := (iRect.Right - iRect.Left) div 2;
      ry := (iRect.Bottom - iRect.Top) div 2;
      p := ParentChart.GraphToImage(AxisToGraph(item^.Point));
      phi := arctan2(AParams.FPoint.Y - p.y, AParams.FPoint.X - p.x);
      SinCos(phi, sinphi, cosphi);
      AResults.FImg := p + Point(round(rx * cosPhi), round(ry * sinPhi));
      exit;
    end;

  if (nptCustom in AParams.FTargets) and (nptCustom in ToolTargets) then begin
    dist := MaxInt;
    for i := 0 to Count - 1 do begin
      item := Source[i];
      if not GetBubbleRect(item, FBubbleScalingFactor, irect) then
        continue;
      rx := (iRect.Right - iRect.Left) div 2;
      ry := (iRect.Bottom - iRect.Top) div 2;
      p := ParentChart.GraphToImage(AxisToGraph(item^.Point));
      phi := -arctan2(AParams.FPoint.Y - p.y, AParams.FPoint.X - p.x);
      SinCos(phi, sinphi, cosphi);
      dperim := round(sqrt(sqr(rx * cosPhi) + sqr(ry * sinPhi)));
      d := round(sqrt(PointDistSq(p, AParams.FPoint)));
      if (d < dist) and (d < dperim + AParams.FRadius) then begin  // not quite exact...
        dist := d;
        AResults.FDist := d;
        AResults.FIndex := i;
        AResults.FYIndex := -1;
        AResults.FValue := item^.Point;
        AResults.FImg := AParams.FPoint;
        if d = 0 then break;
      end;
    end;
    if AResults.FIndex <> -1 then begin
      AResults.FDist := sqr(AResults.FDist);  // we need sqr for comparison with other series
      Result := true;
    end;
  end;
end;

function TBubbleSeries.GetSeriesColor: TColor;
begin
  Result := FBubbleBrush.Color;
end;

class procedure TBubbleSeries.GetXYCountNeeded(out AXCount, AYCount: Cardinal);
begin
  AXCount := 1;
  AYCount := 2;
end;

procedure TBubbleSeries.MovePointEx(var AIndex: Integer;
  AXIndex, AYIndex: Integer; const ANewPos: TDoublePoint);
var
  np: TDoublePoint;   // ANewPos, in axis units
  sp: TDoublePoint;   // Orig data point (source point), in axis units
  gp: TDoublePoint;   // Orig data point, in graph units
  ip: TPoint;         // original data point, in image units
  r: Double;          // radius, in axis units
  inp: TPoint;        // NewPos in image units
  rvec: TDoublePoint; // Rotated radius vector
begin
  Unused(AXIndex);

  ParentChart.DisableRedrawing;
  ListSource.BeginUpdate;
  try
    case AYIndex of
      -1,
       0: begin
            np := GraphToAxis(ANewPos);
            ListSource.SetXValue(AIndex, np.X);
            ListSource.SetYValue(AIndex, np.Y);
          end;
      1:  begin
            sp := ListSource.Item[AIndex]^.Point;
            gp := AxisToGraph(sp);
            case FBubbleRadiusUnits of
              bruX:
                begin
                  inp := ParentChart.GraphToImage(ANewPos);
                  ip := ParentChart.GraphToImage(gp);
                  // Distance data pt to ANewPos, in image units
                  r := sqrt(sqr(ip.X - inp.X) + sqr(ip.Y - inp.Y));
                  // Vector from bubble center to right bubble perimeter, in axis units
                  rvec := GraphToAxis(ParentChart.ImageToGraph(Point(ip.x + round(r), ip.y))) - sp;
                  // Radius of the circle
                  r := abs(rvec.x);
                end;
              bruY:
                begin
                  // like bruX, but with y instead of x
                  inp := ParentChart.GraphToImage(ANewPos);
                  ip := ParentChart.GraphToImage(gp);
                  r := sqrt(sqr(ip.X - inp.X) + sqr(ip.Y - inp.Y));
                  rvec := GraphToAxis(ParentChart.ImageToGraph(Point(ip.x, ip.y + round(r)))) - sp;
                  r := abs(rvec.y);
                end;
              bruXY:
                begin
                  // Blubble radius is the distance between data pt and mouse pt, in axis units
                  np := GraphToAxis(ANewPos);
                  rvec := np - sp;
                  r := sqrt(sqr(rvec.x) + sqr(rvec.y));
                end;
            end;
            ListSource.SetYList(AIndex, [r]);
          end;
    end;
  finally
    ListSource.EndUpdate;
    ParentChart.EnableRedrawing;
    UpdateParentChart;
  end;
end;

procedure TBubbleSeries.SetBubbleBrush(AValue: TBrush);
begin
  if FBubbleBrush = AValue then exit;
  FBubbleBrush.Assign(AValue);
  UpdateParentChart;
end;

procedure TBubbleSeries.SetBubblePen(AValue: TPen);
begin
  if FBubblePen = AValue then exit;
  FBubblePen.Assign(AValue);
  UpdateParentChart;
end;

procedure TBubbleSeries.SetBubbleRadiusPercentage(AValue: Integer);
begin
  if FBubbleRadiusPercentage = AValue then exit;
  FBubbleRadiusPercentage := AValue;
  UpdateParentChart;
end;

procedure TBubbleSeries.SetBubbleRadiusUnits(AValue: TBubbleRadiusUnits);
begin
  if FBubbleRadiusUnits = AValue then exit;
  FBubbleRadiusUnits := AValue;
  UpdateParentChart;
end;

procedure TBubbleSeries.SetOverrideColor(AValue: TBubbleOverrideColors);
begin
  if FOverrideColor = AValue then exit;
  FOverrideColor := AValue;
  UpdateParentChart;
end;

function TBubbleSeries.ToolTargetDistance(const AParams: TNearestPointParams;
  AGraphPt: TDoublePoint; APointIdx, AXIdx, AYIdx: Integer): Integer;
var
  item: PChartDataItem;
  iRect: TRect;
  rx, ry: Integer;
  d, dPerim: Integer;
  p: TPoint;
  phi, sinPhi, cosPhi: Math.float;
begin
  if AYIdx = 0 then begin
    Result := inherited;
    exit;
  end;

  item := Source[APointIdx];
  GetBubbleRect(item, FBubbleScalingFactor, iRect);
  rx := (iRect.Right - iRect.Left) div 2;
  ry := (iRect.Bottom - iRect.Top) div 2;
  p := ParentChart.GraphToImage(AxisToGraph(item^.Point));
  d := round(sqrt(PointDistSq(p, AParams.FPoint)));  // dist between data pt and clicked pt
  phi := -arctan2(AParams.FPoint.Y - p.y, AParams.FPoint.X - p.x);
  SinCos(phi, sinphi, cosphi);
  dperim := round(sqrt((sqr(rx * cosPhi) + sqr(ry * sinPhi))));

  if AYIdx = 1 then
    Result := sqr(abs(d - dperim))
  else begin
    Result := PointDistSq(p, AParams.FPoint);
    if sqrt(Result) > dperim then
      Result := MaxInt;
  end;
end;

procedure TBubbleSeries.UpdateLabelDirectionReferenceLevel(AIndex, AYIndex: Integer;
  var ALevel: Double);
begin
  Unused(AIndex);
  case AYIndex of
    0: ALevel := -Infinity;
    1: ALevel := +Infinity;
  end;
end;

procedure TBubbleSeries.UpdateMargins(ADrawer: IChartDrawer;
  var AMargins: TRect);
var
  i, dist, j: Integer;
  labelText: String;
  dir: TLabelDirection;
  m: array [TLabelDirection] of Integer absolute AMargins;
  gp: TDoublePoint;
  scMarksDistance: Integer;
  center: Double;
begin
  if not Marks.IsMarkLabelsVisible or not Marks.AutoMargins then exit;
  if IsEmpty then exit;
  if not RequestValidChartScaling then exit;

  FindExtentInterval(ParentChart.CurrentExtent, Source.IsSortedByXAsc);
  with Extent do
    center := AxisToGraphY((a.y + b.y) * 0.5);
  UpdateLabelDirectionReferenceLevel(0, 0, center);
  scMarksDistance := ADrawer.Scale(Marks.Distance);

  for i := FLoBound to FUpBound do begin
    for j := 0 to Min(1, Source.YCount-1) do begin
      gp := GetLabelDataPoint(i, j);
      if not ParentChart.IsPointInViewPort(gp) then break;
      labelText := FormattedMark(i, '', j);
      if labelText = '' then break;
      UpdateLabelDirectionReferenceLevel(i, j, center);
      dir := GetLabelDirection(TDoublePointBoolArr(gp)[not IsRotated], center);
      with Marks.MeasureLabel(ADrawer, labelText) do
        dist := Math.IfThen(dir in [ldLeft, ldRight], cx, cy);
      if Marks.DistanceToCenter then
        dist := dist div 2;
      m[dir] := Max(m[dir], dist + scMarksDistance);
    end;
  end;
end;



{ TBoxAndWhiskerSeries }

function TBoxAndWhiskerSeries.AddXY(
  AX, AYLoWhisker, AYLoBox, AY, AYHiBox, AYHiWhisker: Double; AXLabel: String;
  AColor: TColor): Integer;
var
  y: Double;
begin
  if FYIndexWhiskerMin = 0 then
    y := AYLoWhisker
  else if FYIndexBoxMin = 0 then
    y := AYLoBox
  else if FYIndexCenter = 0 then
    y := AY
  else if FYIndexBoxMax = 0 then
    y := AYHiBox
  else if FYIndexWhiskerMax = 0 then
    y := AYHiWhisker
  else
    raise Exception.Create('[TBoxAndWhiskerSeries.AddXY] Ordinary y value missing');

  Result := ListSource.Add(AX, y, AXLabel, AColor);
  with ListSource.Item[Result]^ do begin
    SetY(FYIndexWhiskerMin, AYLoWhisker);
    SetY(FYIndexBoxMin, AYLoBox);
    SetY(FYIndexCenter, AY);
    SetY(FYIndexBoxMax, AYHiBox);
    SetY(FYIndexWhiskerMax, AYHiWhisker);
  end;
end;

procedure TBoxAndWhiskerSeries.Assign(ASource: TPersistent);
begin
  if ASource is TBoxAndWhiskerSeries then
    with TBoxAndWhiskerSeries(ASource) do begin
      Self.BoxBrush.Assign(FBoxBrush);
      Self.BoxPen.Assign(FBoxPen);
      Self.FBoxWidth := FBoxWidth;
      Self.MedianPen.Assign(FMedianPen);
      Self.WhiskersPen.Assign(FWhiskersPen);
      Self.FWhiskersWidth := FWhiskersWidth;
      Self.FYDataLayout := FYDataLayout;
      Self.FYIndexWhiskerMin := FYIndexWhiskerMin;
      Self.FYIndexBoxMin := FYIndexBoxMin;
      Self.FYIndexCenter := FYIndexCenter;
      Self.FYIndexBoxMax := FYIndexBoxMax;
      Self.FYIndexWhiskerMax := FYIndexWhiskerMax;
    end;
  inherited Assign(ASource);
end;

constructor TBoxAndWhiskerSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ToolTargets := [nptPoint, nptYList, nptCustom];
  FOptimizeX := false;
  FBoxBrush := TBrush.Create;
  FBoxBrush.OnChange := @StyleChanged;
  FBoxPen := TPen.Create;
  FBoxPen.OnChange := @StyleChanged;
  FBoxWidth := DEF_BOX_WIDTH;
  FMedianPen := TPen.Create;
  FMedianPen.OnChange := @StyleChanged;
  FWhiskersPen := TPen.Create;
  FWhiskersPen.OnChange := @StyleChanged;
  FWhiskersWidth := DEF_WHISKERS_WIDTH;
  FYDataLayout := bwlLegacy;
  FYIndexWhiskerMin := DEF_YINDEX_WHISKERMIN;
  FYIndexBoxMin := DEF_YINDEX_BOXMIN;
  FYIndexCenter := DEF_YINDEX_CENTER;
  FYIndexBoxMax := DEF_YINDEX_BOXMAX;
  FYIndexWhiskerMax := DEF_YINDEX_WHISKERMAX;
end;

destructor TBoxAndWhiskerSeries.Destroy;
begin
  FreeAndNil(FBoxBrush);
  FreeAndNil(FBoxPen);
  FreeAndNil(FMedianPen);
  FreeAndNil(FWhiskersPen);
  inherited Destroy;
end;

procedure TBoxAndWhiskerSeries.Draw(ADrawer: IChartDrawer);

  procedure GraphToImage_Whisker(X, XW, Y1, Y2: Double; 
    out iX1, iX, iX2, iY1, iY2: Integer);
  begin
    if IsRotated then
    begin
      iX1 := ParentChart.YGraphToImage(X - XW);
      iX2 := ParentChart.YGraphToImage(X + XW);
      if iX1 <> iX2 then
        iX := ParentChart.YGraphToImage(X)
      else
        iX := iX1;
      iY1 := ParentChart.XGraphToImage(Y1);
      iY2 := ParentChart.XGraphToImage(Y2);      
    end else
    begin
      iX1 := ParentChart.XGraphToImage(X - XW);
      iX2 := ParentChart.XGraphToImage(X + XW);
      if iX1 <> iX2 then
        iX := ParentChart.XGraphToImage(X)
      else
        iX := iX1;
      iY1 := ParentChart.YGraphToImage(Y1);
      iY2 := ParentChart.YGraphToImage(Y2);
    end;
  end;
  
  procedure GraphToImage_Bar(X1, X2, Y1, Y2: Double; 
    out iX1, iX2, iY1, iY2: Integer);
  begin
    if IsRotated then
    begin
      iX1 := ParentChart.YGraphToImage(X1);
      iX2 := ParentChart.YGraphToImage(X2);
      iY1 := ParentChart.XGraphToImage(Y1);
      iY2 := ParentChart.XGraphToImage(Y2);
    end else
    begin
      iX1 := ParentChart.XGraphToImage(X1);
      iX2 := ParentChart.XGraphToImage(X2);
      iY1 := ParentChart.YGraphToImage(Y1);
      iY2 := ParentChart.YGraphToImage(Y2);
    end;
  end;
  
  procedure GraphToImage_Median(Y: Double; out iY: Integer);
  begin
    if IsRotated then
      iY := ParentChart.XGraphToImage(Y)
    else
      iY := ParentChart.YGraphToImage(Y);
  end;

  procedure PrepareLine(i: Integer; APen: TPen);
  begin
    ADrawer.Pen := APen;
    if (Source[i]^.Color <> clTAColor) and (APen.Color = clTAColor) then
      ADrawer.SetPenColor(Source[i]^.Color);
    ADrawer.SetBrushParams(bsClear, clTAColor);
  end;
    
  procedure PrepareBox(i: Integer);
  begin
    ADrawer.Pen := BoxPen;
    if Source[i]^.Color <> clTAColor then
    begin
      if BoxPen.Color = clTAColor then
        ADrawer.SetPenColor(Source[i]^.Color);
      ADrawer.SetBrushParams(bsSolid, Source[i]^.Color);
    end
    else
      ADrawer.Brush := BoxBrush;
  end;
  
  {    X1   X   X2
       |----+----|       Y
            |
      +-----+-----+      YBox
      |           |
      |           |
  }
  procedure DrawWhisker(X1, X, X2, Y, YBox: Integer);
  begin
    if IsRotated then
    begin
      if X1 <> X2 then
        ADrawer.Line(Y, X1, Y, X2);
      ADrawer.Line(Y, X, YBox, X);
    end else
    begin
      if X1 <> X2 then
        ADrawer.Line(X1, Y, X2, Y);
      ADrawer.Line(X, Y, X, YBox);
    end;
  end;
  
  procedure DrawBox(XBox1, XBox2, YBox1, YBox2: Integer);
  begin
    if (XBox1 = XBox2) or (YBox1 = YBox2) then
    begin
      if IsRotated then
        ADrawer.Line(YBox1, XBox1, YBox2, XBox2)
      else
        ADrawer.Line(XBox1, YBox1, XBox2, YBox2);
    end else
    begin
      if IsRotated then
        ADrawer.Rectangle(YBox1, XBox1, YBox2, XBox2)
      else
        ADrawer.Rectangle(XBox1, YBox1, XBox2, YBox2);
    end;
  end;
  
  procedure DrawMedian(X1, X2, Y: Integer);
  begin
    if IsRotated then
      ADrawer.Line(Y, X1, Y, X2)
    else
      ADrawer.Line(X1, Y, X2, Y);
  end;

  function GetY(AItem: PChartDataItem; AYIndex: Integer; out AY: Double): Boolean;
  var
    y: Double;
  begin
    Result := false;
    if AYIndex = 0 then
      y := AItem^.Y
    else
      y := AItem^.YList[AYIndex-1];
    if IsNaN(y) then
      exit;

    AY := AxisToGraphY(y);
    Result := true;
  end;
    
var
  ext2: TDoubleRect;
  x, ymin, yqmin, ymed, yqmax, ymax, wb, ww, w: Double;
  i: Integer;
  ix, ixb1, ixb2, ixw1, ixw2: Integer;
  iymin, iyqmin, iymed, iyqmax, iymax: Integer;
  nx, ny: Cardinal;
  item: PChartDataItem;
begin
  if IsEmpty or (not Active) then exit;

  if FWidthStyle = bwsPercentMin then
    UpdateMinXRange;

  ext2 := ParentChart.CurrentExtent;
  ExpandRange(ext2.a.X, ext2.b.X, 1.0);
  ExpandRange(ext2.a.Y, ext2.b.Y, 1.0);

  PrepareGraphPoints(ext2, true);

  for i := FLoBound to FUpBound do begin
    // Get values from source and convert to graph units.
    item := Source[i];
    x := GetGraphPointX(i);
    if IsNaN(x) then continue;
    if not GetY(item, FYIndexWhiskerMin, ymin) then continue;
    if not GetY(item, FYIndexBoxMin, yqmin) then continue;
    if not GetY(item, FYIndexCenter, ymed) then continue;
    if not GetY(item, FYIndexBoxMax, yqmax) then continue;
    if not GetY(item, FYIndexWhiskerMax, ymax) then continue;
    
    case FWidthStyle of
      bwsPercent: w := GetXRange(x, i) * PERCENT / 2;
      bwsPercentMin: w := FMinXRange * PERCENT / 2;
    end;
    wb := w * BoxWidth;
    ww := w * WhiskersWidth;
    
    // Calculate image coordiantes
    GraphToImage_Whisker(x, ww, ymin, ymax, ixw1, ix, ixw2, iymin, iymax);
    GraphToImage_Bar(x-wb, x+wb, yqmin, yqmax, ixb1, ixb2, iyqmin, iyqmax);
    GraphToImage_Median(ymed, iymed);
     
    // Draw whisker
    PrepareLine(i, WhiskersPen);
    DrawWhisker(ixw1, ix, ixw2, iymin, iyqmin);
    DrawWhisker(ixw1, ix, ixw2, iymax, iyqmax);
    
    // Draw box
    PrepareBox(i);
    DrawBox(ixb1, ixb2, iyqmin, iyqmax);
    ADrawer.Pen := WhiskersPen;
    ADrawer.SetPenColor(WhiskersPen.Color);
    
    // Draw median line
    PrepareLine(i, MedianPen);
    DrawMedian(ixb1, ixb2, iymed);
  end;

  GetXYCountNeeded(nx, ny);
  if Source.YCount > ny then
    for i := 0 to ny-1 do DrawLabels(ADrawer, i)
  else
    DrawLabels(ADrawer);
end;

function TBoxAndWhiskerSeries.Extent: TDoubleRect;
var
  x: Double;
  j: Integer;

  function ExtraWidth(AIndex: Integer): Double;
  begin
    Result := GetXRange(x, AIndex) * Max(BoxWidth, WhiskersWidth) * PERCENT / 2;
  end;

begin
  Result := Source.ExtentList;
  if Source.Count = 0 then
    exit;
  
  // Show first and last boxes fully.
  j := -1;
  x := NaN;
  while IsNaN(x) and (j < Source.Count-1) do begin
    inc(j);
    x := GetGraphPointX(j);
  end;
  Result.a.X := Min(Result.a.X, GraphToAxisX(x - ExtraWidth(j)));
  j := Count;
  x := NaN;
  while IsNaN(x) and (j > 0) do begin
    dec(j);
    x := GetGraphPointX(j);
  end;
  Result.b.X := Max(Result.b.X, GraphToAxisX(x + ExtraWidth(j)));
end;

procedure TBoxAndWhiskerSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemBoxAndWhiskers.Create(Self, LegendTextSingle));
end;

function TBoxAndWhiskerSeries.GetNearestPoint(const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  i, j: Integer;
  graphClickPt, pt: TDoublePoint;
  x, w, wb: Double;
  y: Array[0..4] of Double;
  pImg: TPoint;
  R: TDoubleRect;
  xImg, dist: Integer;
begin
  Result := inherited;

  if Result then begin
    if (nptPoint in AParams.FTargets) and (nptPoint in ToolTargets) then
      exit;
    if (nptYList in AParams.FTargets) and (nptYList in ToolTargets) then
      exit;
  end;

  if not ((nptCustom in AParams.FTargets) and (nptCustom in ToolTargets))
  then
    exit;

  pImg := AParams.FPoint;
  graphClickPt := ParentChart.ImageToGraph(AParams.FPoint);
  if IsRotated then begin
    Exchange(graphclickpt.X, graphclickpt.Y);
    pImg := ParentChart.GraphToImage(graphclickPt);
  end;

  // Iterate through all points of the series
  for i := 0 to Count - 1 do begin
    x := GetGraphPointX(i);
    for j := 0 to High(y) do
      y[j] := GetGraphPointY(i, j);
    case FWidthStyle of
      bwsPercent    : w := GetXRange(x, i) * PERCENT / 2;
      bwsPercentMin : w := FMinXRange * PERCENT / 2;
    end;
    wb := w * BoxWidth;

    dist := MaxInt;

    // click inside box
    R.a := DoublePoint(x - wb, y[FYIndexBoxMin]);  
    R.b := DoublePoint(x + wb, y[FYIndexBoxMax]);  
    if InRange(graphClickPt.X, R.a.x, R.b.x) and InRange(graphClickPt.Y, R.a.Y, R.b.Y) then
    begin
      dist := 0;
      AResults.FYIndex := -1;
   end;

    // click on whisker line
    xImg := ParentChart.XGraphToImage(x);
    if InRange(graphClickPt.Y, y[FYIndexWhiskerMin], y[FYIndexBoxMin]) or 
       InRange(graphClickPt.Y, y[FYIndexWhiskerMax], y[FYIndexBoxMax])
    then begin
      dist := sqr(pImg.X - xImg);
      AResults.FYIndex := -1;
    end;

    // Sufficiently close?
    if dist < AResults.FDist then begin
      AResults.FDist := dist;
      AResults.FIndex := i;
      pt := DoublePoint(x, y[2]);   // Median
      AResults.FValue := pt;
      if IsRotated then Exchange(pt.X, pt.Y);
      AResults.FImg := ParentChart.GraphToImage(pt);
      if dist = 0 then break;
    end;
  end;
  Result := AResults.FIndex > -1;
end;

function TBoxAndWhiskerSeries.GetSeriesColor: TColor;
begin
  Result := BoxBrush.Color;
end;

class procedure TBoxAndWhiskerSeries.GetXYCountNeeded(out AXCount, AYCount: Cardinal);
begin
  AXCount := 0;
  AYCount := 5;
end;

procedure TBoxAndWhiskerSeries.SetBoxBrush(AValue: TBrush);
begin
  if FBoxBrush = AValue then exit;
  FBoxBrush.Assign(AValue);
  UpdateParentChart;
end;

procedure TBoxAndWhiskerSeries.SetBoxPen(AValue: TPen);
begin
  if FBoxPen = AValue then exit;
  FBoxPen.Assign(AValue);
  UpdateParentChart;
end;

procedure TBoxAndWhiskerSeries.SetBoxWidth(AValue: Integer);
begin
  if FBoxWidth = AValue then exit;
  FBoxWidth := AValue;
  UpdateParentChart;
end;

procedure TBoxAndWhiskerSeries.SetLegendDirection(
  AValue: TBoxAndWhiskerSeriesLegendDir);
begin
  if FLegendDirection = AValue then exit;
  FLegendDirection := AValue;
  UpdateParentChart;
end;

procedure TBoxAndWhiskerSeries.SetMedianPen(AValue: TPen);
begin
  if FMedianPen = AValue then exit;
  FMedianPen.Assign(AValue);
  UpdateParentChart;
end;

procedure TBoxAndWhiskerSeries.SetWhiskersPen(AValue: TPen);
begin
  if FWhiskersPen = AValue then exit;
  FWhiskersPen.Assign(AValue);
  UpdateParentChart;
end;

procedure TBoxAndWhiskerSeries.SetWhiskersWidth(AValue: Integer);
begin
  if FWhiskersWidth = AValue then exit;
  FWhiskersWidth := AValue;
  UpdateParentChart;
end;

procedure TBoxAndWhiskerSeries.SetYIndexBoxMax(AValue: Integer);
begin
  if FYIndexBoxMax = AValue then exit;
  FYIndexBoxMax := AValue;
  UpdateYDataLayout;
  UpdateParentChart;
end;

procedure TBoxAndWhiskerSeries.SetYIndexBoxMin(AValue: Integer);
begin
  if FYIndexBoxMin = AValue then exit;
  FYIndexBoxMin := AValue;
  UpdateYDataLayout;
  UpdateParentChart;
end;

procedure TBoxAndWhiskerSeries.SetYIndexCenter(AValue: Integer);
begin
  if FYIndexCenter = AValue then exit;
  FYIndexCenter := AValue;
  UpdateYDataLayout;
  UpdateParentChart;
end;

procedure TBoxAndWhiskerSeries.SetYIndexWhiskerMax(AValue: Integer);
begin
  if FYIndexWhiskerMax = AValue then exit;
  FYIndexWhiskerMax := AValue;
  UpdateYDataLayout;
  UpdateParentChart;
end;

procedure TBoxAndWhiskerSeries.SetYIndexWhiskerMin(AValue: Integer);
begin
  if FYIndexWhiskerMin = AValue then exit;
  FYIndexWhiskerMin := AValue;
  UpdateYDataLayout;
  UpdateParentChart;
end;

procedure TBoxAndWhiskerSeries.SetYDataLayout(AValue: TBoxAndWhiskerYDataLayout);

  procedure SetYIndices(AWhiskerMin, ABoxMin, ACenter, ABoxMax, AWhiskerMax: Integer);
  begin
    FYIndexWhiskerMin := AWhiskerMin;
    FYIndexBoxMin := ABoxMin;
    FYIndexCenter := ACenter;
    FYIndexBoxMax := ABoxMax;
    FYIndexWhiskerMax := AWhiskerMax;
  end;

begin
  if FYDataLayout = AValue then exit;
  FYDataLayout := AValue;
  case FYDataLayout of
    bwlNormal: SetYIndices(1, 2, 0, 3, 4);   
    bwlLegacy: SetYIndices(0, 1, 2, 3, 4);
    bwlCustom: ;
  end;
  UpdateParentChart;
end;

function TBoxAndWhiskerSeries.SkipMissingValues(AIndex: Integer): Boolean;
begin
  Result := IsNaN(Source[AIndex]^.Point);
  if not Result then
    Result := HasMissingYValue(AIndex, 5);
end;

function TBoxAndWhiskerSeries.ToolTargetDistance(
  const AParams: TNearestPointParams; AGraphPt: TDoublePoint;
  APointIdx, AXIdx, AYIdx: Integer): Integer;

  // All in image coordinates transformed to have a horizontal x axis
  function DistanceToLine(Pt: TPoint; x1, x2, y: Integer): Integer;
  begin
    if InRange(Pt.X, x1, x2) then
      Result := sqr(Pt.Y - y)   // FDistFunc does not calc sqrt
    else
      Result := Min(
        AParams.FDistFunc(Pt, Point(x1, y)),
        AParams.FDistFunc(Pt, Point(x2, y))
      );
  end;

var
  xw1, xw2, xb1, xb2, y: Integer;
  w, wb, ww: Double;
  clickPt: TPoint;
  gp: TDoublePoint;
begin
  Unused(AXIdx);

  if IsRotated then begin
    gp := ParentChart.ImageToGraph(AParams.FPoint);
    Exchange(gp.X, gp.Y);
    clickPt := ParentChart.GraphToImage(gp);
    Exchange(AGraphPt.X, AGraphPt.Y);
  end else
    clickPt := AParams.FPoint;

  case FWidthStyle of
    bwsPercent    : w := GetXRange(AGraphPt.X, APointIdx) * PERCENT / 2;
    bwsPercentMin : w := FMinXRange * PERCENT / 2;
  end;
  wb := w * BoxWidth;
  ww := w * WhiskersWidth;

  xw1 := ParentChart.XGraphToImage(AGraphPt.X - ww);
  xw2 := ParentChart.XGraphToImage(AGraphPt.X + ww);
  xb1 := ParentChart.XGraphToImage(AGraphPt.X - wb);
  xb2 := ParentChart.XGraphToImage(AGraphPt.X + wb);
  y := ParentChart.YGraphToImage(AGraphPt.Y);

  if AYIdx in [FYIndexWhiskerMax, FYIndexWhiskerMin] then   
    Result := DistanceToLine(clickPt, xw1, xw2, y)
  else if AYIdx in [FYIndexBoxMax, FYIndexCenter, FYIndexBoxMin] then
    Result := DistanceToLine(clickPt, xb1, xb2, y)
  else
    raise Exception.Create('[TBoxAndWhiskerSeries.ToolTargetDistance] Unknown y index.');
end;

procedure TBoxAndWhiskerSeries.UpdateLabelDirectionReferenceLevel(
  AIndex, AYIndex: Integer; var ALevel: Double);
var
  item: PChartDataItem;
  y0, y3: Double;
begin
  { ToDo: The version before introducing FYIndex* variables had used the values 
    0 and 3 here. 3 means: FYIndexBoxMax. Interpreted this as a typo, but there
    is some chance that it would be correct --> needs to be checked. }
  if (AYIndex = FYIndexWhiskerMin) then
    ALevel := +Infinity
  else if (AYIndex = FYIndexWhiskerMax) then
    ALevel := -Infinity
  else
  begin
    item := Source.Item[AIndex];
    y0 := AxisToGraphY(item^.GetY(FYIndexWhiskerMin));
    y3 := AxisToGraphY(item^.GetY(FYIndexWhiskerMax));
    ALevel := (y0 + y3) * 0.5;
  end;
end;

procedure TBoxAndWhiskerSeries.UpdateYDataLayout;
begin
  if (FYIndexWhiskerMin = 0) and (FYIndexBoxMin = 1) and (FYIndexCenter = 2) and
     (FYIndexBoxMax = 3) and (FYIndexWhiskerMax = 4) 
  then 
    FYDataLayout := bwlLegacy
  else
  if (FYIndexCenter = 0) and (FYIndexWhiskerMin = 1) and (FYIndexBoxMin = 2) and
     (FYIndexBoxMax = 3) and (FYIndexWhiskerMax = 4) 
  then 
    FYDataLayout := bwlNormal
  else
    FYDataLayout := bwlCustom;
end;

(*
{ TOHLCBrush }

function TOHLCBrush.IsColorStored: Boolean;
begin
  Result := (Color <> DEFAULT_COLORS[FBrushKind]);
end;

procedure TOHLCBrush.SetBrushKind(AValue: TOHLCBrushKind);
begin
  FBrushKind := AValue;
  //Color := DEFAULT_COLORS[FBrushKind];
end;

{ TOHLCPen }

function TOHLCPen.IsColorStored: Boolean;
begin
  Result := (Color <> DEFAULT_COLORS[FPenKind]);
end;

procedure TOHLCPen.SetPenKind(AValue: TOHLCPenKind);
begin
  FPenKind := AValue;
  //Color := DEFAULT_COLORS[FPenKind];
end;
       *)
{ TOpenHighLowCloseSeries }

function TOpenHighLowCloseSeries.AddXOHLC(
  AX, AOpen, AHigh, ALow, AClose: Double;
  ALabel: String; AColor: TColor): Integer;
var
  y: Double;
begin
  if YIndexOpen = 0 then
    y := AOpen
  else if YIndexHigh = 0 then
    y := AHigh
  else if YIndexLow = 0 then
    y := ALow
  else if YIndexClose = 0 then
    y := AClose
  else
    raise Exception.Create('TOpenHighLowCloseSeries: Ordinary y value missing');

  Result := ListSource.Add(AX, y, ALabel, AColor);
  with ListSource.Item[Result]^ do begin
    SetY(YIndexOpen, AOpen);
    SetY(YIndexHigh, AHigh);
    SetY(YIndexLow, ALow);
    SetY(YIndexClose, AClose);
  end;
end;

procedure TOpenHighLowCloseSeries.Assign(ASource: TPersistent);
var
  bk: TOHLCBrushKind;
  pk: TOHLCPenKind;
begin
  if ASource is TOpenHighLowCloseSeries then
    with TOpenHighLowCloseSeries(ASource) do begin
      for bk in TOHLCBrushKind do
        Self.FBrush[bk] := FBrush[bk];
      for pk in TOHLCPenKind do
        Self.FPen[pk] := FPen[pk];
      Self.FMode := FMode;
      Self.FTickWidth := FTickWidth;
      Self.FYIndexClose := FYIndexClose;
      Self.FYIndexHigh := FYIndexHigh;
      Self.FYIndexLow := FYIndexLow;
      Self.FYIndexOpen := FYIndexOpen;
    end;
  inherited Assign(ASource);
end;

constructor TOpenHighLowCloseSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ToolTargets := [nptPoint, nptYList, nptCustom];
  FOptimizeX := false;
  FStacked := false;
  FTickWidth := DEF_OHLC_TICK_WIDTH;
  FYIndexClose := DEF_YINDEX_CLOSE;
  FYIndexHigh := DEF_YINDEX_HIGH;
  FYIndexLow := DEF_YINDEX_LOW;
  FYIndexOpen := DEF_YINDEX_OPEN;

  // Candlestick up brush
  FBrush[obkCandleUp] := TBrush.Create;
  FBrush[obkCandleUp].Color := OHLC_BRUSH_COLORS[obkCandleUp];
  FBrush[obkCandleUp].OnChange := @StyleChanged;
  // Candlestick down brush
  FBrush[obkCandleDown] := TBrush.Create;
  FBrush[obkCandleDown].Color := OHLC_BRUSH_COLORS[obkCandleDown];
  FBrush[obkCandleDown].OnChange := @StyleChanged;
  // Candlestick up border pen
  FPen[opkCandleUp] := TPen.Create;
  FPen[opkCandleUp].Color := OHLC_PEN_COLORS[opkCandleUp];
  FPen[opkCandleUp].OnChange := @StyleChanged;
  // Candlestick down border pen
  FPen[opkCandleDown] := TPen.Create;
  FPen[opkCandleDown].Color := OHLC_PEN_COLORS[opkCandleDown];
  FPen[opkCandleDown].OnChange := @StyleChanged;
  // Candlestick range pen
  FPen[opkCandleLine] := TPen.Create;
  FPen[opkCandleLine].Color := OHLC_PEN_COLORS[opkCandleLine];
  FPen[opkCandleLine].OnChange := @StyleChanged;
  // OHLC up pen
  FPen[opkLineUp] := TPen.Create;
  FPen[opkLineUp].Color := OHLC_PEN_COLORS[opkLineUp];
  FPen[opkLineUp].OnChange := @StyleChanged;
  // OHLC down pen
  FPen[opkLineDown] := TPen.Create; //TOHLCPen.Create;
  FPen[opkLineDown].Color := OHLC_PEN_COLORS[opkLineDown];
  FPen[opkLineDown].OnChange := @StyleChanged;
end;

destructor TOpenHighLowCloseSeries.Destroy;
var
  bk: TOHLCBrushKind;
  pk: TOHLCPenKind;
begin
  for bk in TOHLCBrushKind do
    FreeAndNil(FBrush[bk]);
  for pk in TOHLCPenKind do
    FreeAndNil(FPen[pk]);
  inherited;
end;

function TOpenHighLowCloseSeries.CalcTickWidth(AX: Double; AIndex: Integer): Double;
begin
  case FTickWidthStyle of
    twsPercent:
      Result := GetXRange(AX, AIndex) * PERCENT * TickWidth;
    twsPercentMin:
      begin
        if FMinXRange = 0 then
          UpdateMinXRange;
        Result := FMinXRange * PERCENT * TickWidth;
      end;
  end;
end;

procedure TOpenHighLowCloseSeries.Draw(ADrawer: IChartDrawer);

  function MaybeRotate(AX, AY: Double): TPoint;
  begin
    if IsRotated then
      Exchange(AX, AY);
    Result := ParentChart.GraphToImage(DoublePoint(AX, AY));
  end;

  procedure DoLine(AX1, AY1, AX2, AY2: Double);
  begin
    ADrawer.Line(MaybeRotate(AX1, AY1), MaybeRotate(AX2, AY2));
  end;

  procedure NoZeroRect(var R: TRect);
  begin
    if IsRotated then
    begin
      if R.Left = R.Right then inc(R.Right);
    end else
    begin
      if R.Top = R.Bottom then inc(R.Bottom);
    end;
  end;

  procedure DoRect(AX1, AY1, AX2, AY2: Double);
  var
    r: TRect;
  begin
    r.TopLeft := MaybeRotate(AX1, AY1);
    r.BottomRight := MaybeRotate(AX2, AY2);
    NoZeroRect(r);
    ADrawer.FillRect(r.Left, r.Top, r.Right, r.Bottom);
    ADrawer.Rectangle(r);
  end;

  procedure DrawOHLC(x, yopen, yhigh, ylow, yclose, tw: Double);
  begin
    DoLine(x, yhigh, x, ylow);
    DoLine(x, yclose, x + tw, yclose);
    if not IsNaN(yopen) then
      DoLine(x - tw, yopen, x, yopen);
  end;

  procedure DrawCandleStick(x, yopen, yhigh, ylow, yclose, tw: Double; APenIdx: Integer);
  begin
    if CandleStickLinePen.Color = clDefault then
      // use linepen and linedown pen for range line
      ADrawer.Pen := FPen[TOHLCPenKind(APenIdx + 3)]
    else
      ADrawer.Pen := CandleStickLinePen;
    DoLine(x, yhigh, x, ylow);
    ADrawer.Pen := FPen[TOHLCPenKind(APenIdx)];
    DoRect(x - tw, yopen, x + tw, yclose);
  end;

const
  UP_INDEX = 0;
  DOWN_INDEX = 1;
var
  my: Cardinal;
  ext2: TDoubleRect;
  i: Integer;
  x, tw, yopen, yhigh, ylow, yclose, prevclose: Double;
  idx: Integer;
  nx, ny: Cardinal;
begin
  if IsEmpty or (not Active) then exit;
  my := MaxIntValue([YIndexOpen, YIndexHigh, YIndexLow, YIndexClose]);
  if my >= Source.YCount then exit;

  ext2 := ParentChart.CurrentExtent;
  ExpandRange(ext2.a.X, ext2.b.X, 1.0);
  ExpandRange(ext2.a.Y, ext2.b.Y, 1.0);

  PrepareGraphPoints(ext2, true);

  prevclose := -Infinity;
  for i := FLoBound to FUpBound do begin
    x := GetGraphPointX(i);
    if IsNaN(x) then Continue;
    yopen := GetGraphPointY(i, YIndexOpen);
    if IsNaN(yopen) and (FMode = mCandleStick) then Continue;
    yhigh := GetGraphPointY(i, YIndexHigh);
    if IsNaN(yhigh) then Continue;
    ylow := GetGraphPointY(i, YIndexLow);
    if IsNaN(ylow) then Continue;
    yclose := GetGraphPointY(i, YIndexClose);
    if IsNaN(yclose) then Continue;
    tw := CalcTickWidth(x, i);

    if IsNaN(yopen) then
    begin
      // HLC chart: compare with close value of previous data point
      if prevclose < yclose then
        idx := UP_INDEX
      else
        idx := DOWN_INDEX;
    end else
    if (yopen <= yclose) then
      idx := UP_INDEX
    else
      idx := DOWN_INDEX;
    ADrawer.Brush := FBrush[TOHLCBrushKind(idx)];
    case FMode of
      mOHLC: ADrawer.Pen := FPen[TOHLCPenKind(idx + 3)];
      mCandlestick: ADrawer.Pen := FPen[TOHLCPenKind(idx)];
    end;
    if Source[i]^.Color <> clTAColor then
    begin
      ADrawer.SetPenParams(FPen[TOHLCPenKind(idx)].Style, Source[i]^.Color, FPen[TOHLCPenKind(idx)].Width);
      ADrawer.SetBrushParams(FBrush[TOHLCBrushKind(idx)].Style, Source[i]^.Color);
    end;

    case FMode of
      mOHLC: DrawOHLC(x, yopen, yhigh, ylow, yclose, tw);
      mCandleStick: DrawCandleStick(x, yopen, yhigh, ylow, yclose, tw, idx);
    end;

    prevclose := yclose;
  end;

  GetXYCountNeeded(nx, ny);
  if Source.YCount > ny then
    for i := 0 to ny-1 do DrawLabels(ADrawer, i)
  else
    DrawLabels(ADrawer);
end;

function TOpenHighLowCloseSeries.Extent: TDoubleRect;
var
  x: Double;
  tw: Double;
  j: Integer;
begin
  Result := Source.ExtentList;                            // axis units

  // Enforce recalculation of tick/candlebox width
  FMinXRange := 0;

  // Show first and last open/close ticks and candle boxes fully.
  j := -1;
  x := NaN;
  while IsNaN(x) and (j < Source.Count-1) do begin
    inc(j);
    x := GetGraphPointX(j);                                 // graph units
  end;
  tw := CalcTickWidth(x, j);
  Result.a.X := Min(Result.a.X, GraphToAxisX(x - tw));    // axis units
//  Result.a.X := Min(Result.a.X, x - tw);
  j := Count;
  x := NaN;
  While IsNaN(x) and (j > 0) do begin
    dec(j);
    x := GetGraphPointX(j);
  end;
  tw := CalcTickWidth(x, j);
  Result.b.X := Max(Result.b.X, AxisToGraphX(x + tw));
//  Result.b.X := Max(Result.b.X, x + tw);
end;

function TOpenHighLowCloseSeries.GetBrush(AIndex: TOHLCBrushKind): TBrush;
begin
  Result := FBrush[AIndex];
end;

procedure TOpenHighLowCloseSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemOHLCLine.Create(Self, LegendTextSingle));
end;

function TOpenHighLowCloseSeries.GetNearestPoint(const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  i: Integer;
  graphClickPt, p: TDoublePoint;
  pImg: TPoint;
  x, yopen, yhigh, ylow, yclose, tw: Double;
  xImg, dist: Integer;
  R: TDoubleRect;
begin
  Result := inherited;

  if Result then begin
    if (nptPoint in AParams.FTargets) and (nptPoint in ToolTargets) then
      exit;
    if (nptYList in AParams.FTargets) and (nptYList in ToolTargets) then
      exit;
  end;
  if not ((nptCustom in AParams.FTargets) and (nptCustom in ToolTargets))
  then
    exit;

  graphClickPt := ParentChart.ImageToGraph(AParams.FPoint);
  pImg := AParams.FPoint;
  if IsRotated then begin
//    Exchange(pImg.X, pImg.Y);
    Exchange(graphclickpt.X, graphclickpt.Y);
    pImg := ParentChart.GraphToImage(graphClickPt);
  end;

  // Iterate through all points of the series
  for i := 0 to Count - 1 do begin
    x := GetGraphPointX(i);
    yopen := GetGraphPointY(i, YIndexOpen);
    yhigh := GetGraphPointY(i, YIndexHigh);
    ylow := GetGraphPointY(i, YIndexLow);
    yclose := GetGraphPointY(i, YIndexClose);
    tw := CalcTickWidth(x, i);

    dist := MaxInt;

    // click on vertical line
    if InRange(graphClickPt.Y, ylow, yhigh) then begin
      xImg := ParentChart.XGraphToImage(x);
      dist := sqr(pImg.X - xImg);
      AResults.FYIndex := -1;
    end;

    // click on candle box
    if FMode = mCandlestick then begin
      R.a := DoublePoint(x - tw, Min(yopen, yclose));
      R.b := DoublePoint(x + tw, Max(yopen, yclose));
      if InRange(graphClickPt.X, R.a.x, R.b.x) and InRange(graphClickPt.Y, R.a.Y, R.b.Y) then
      begin
        dist := 0;
        AResults.FYIndex := -1;
      end;
    end;

    // Sufficiently close?
    if dist < AResults.FDist then begin
      AResults.FDist := dist;
      AResults.FIndex := i;
      p := DoublePoint(x, yclose);   // "Close" value
      AResults.FValue := p;
      if IsRotated then Exchange(p.X, p.Y);
      AResults.FImg := ParentChart.GraphToImage(p);
      if dist = 0 then break;
    end;
  end;
  Result := AResults.FIndex > -1;
end;

function TOpenHighLowCloseSeries.GetPen(AIndex: TOHLCPenKind): TPen;
begin
  Result := FPen[AIndex];
end;

function TOpenHighLowCloseSeries.GetSeriesColor: TColor;
begin
  Result := LinePen.Color;
end;

class procedure TOpenHighLowCloseSeries.GetXYCountNeeded(out AXCount, AYCount: Cardinal);
begin
  AXCount := 0;
  AYCount := 4;
end;

function TOpenHighLowCloseSeries.IsBrushStored(AIndex: TOHLCBrushKind): Boolean;
begin
  Result := not (
    (FBrush[AIndex].Color = OHLC_BRUSH_COLORS[AIndex]) and
    (FBrush[AIndex].Style = bsSolid)
  );
end;

function TOpenHighLowCloseSeries.IsPenStored(AIndex: TOHLCPenKind): Boolean;
begin
  Result := not (
    (FPen[AIndex].Color = OHLC_PEN_COLORS[AIndex]) and
    FPen[AIndex].Cosmetic and
    (FPen[AIndex].EndCap = pecRound) and
    (FPen[AIndex].JoinStyle = pjsRound) and
    (FPen[AIndex].Mode = pmCopy) and
    (FPen[AIndex].Style = psSolid) and
    (FPen[AIndex].Width = 1)
  );
end;

procedure TOpenHighLowCloseSeries.SetBrush(AIndex: TOHLCBrushKind; AValue: TBrush);
begin
  if GetBrush(AIndex) = AValue then exit;
  FBrush[AIndex].Assign(AValue);
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetPen(AIndex: TOHLCPenKind; AValue: TPen);
begin
  if GetPen(AIndex) = AValue then exit;
  FPen[AIndex].Assign(AValue);
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetOHLCMode(AValue: TOHLCMode);
begin
  if FMode = AValue then exit;
  FMode := AValue;
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetTickWidth(AValue: Integer);
begin
  if FTickWidth = AValue then exit;
  FTickWidth := AValue;
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetTickWidthStyle(AValue: TTickWidthStyle);
begin
  if FTickWidthStyle = AValue then exit;
  FTickWidthStyle := AValue;
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetYIndexClose(AValue: Integer);
begin
  if FYIndexClose = AValue then exit;
  FYIndexClose := AValue;
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetYIndexHigh(AValue: Integer);
begin
  if FYIndexHigh = AValue then exit;
  FYIndexHigh := AValue;
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetYIndexLow(AValue: Integer);
begin
  if FYIndexLow = AValue then exit;
  FYIndexLow := AValue;
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetYIndexOpen(AValue: Integer);
begin
  if FYIndexOpen = AValue then exit;
  FYIndexOpen := AValue;
  UpdateParentChart;
end;

function TOpenHighLowCloseSeries.SkipMissingValues(AIndex: Integer): Boolean;
begin
  Result := IsNaN(Source[AIndex]^.Point);
  if not Result then
    Result := HasMissingYValue(AIndex, 4);
end;

function TOpenHighLowCloseSeries.ToolTargetDistance(
  const AParams: TNearestPointParams; AGraphPt: TDoublePoint;
  APointIdx, AXIdx, AYIdx: Integer): Integer;

  // All in image coordinates transformed to have a horizontal x axis
  function DistanceToLine(Pt: TPoint; x1, x2, y: Integer): Integer;
  begin
    if InRange(Pt.X, x1, x2) then     // FDistFunc does not calculate sqrt
      Result := sqr(Pt.Y - y)
    else
      Result := Min(
        AParams.FDistFunc(Pt, Point(x1, y)),
        AParams.FDistFunc(Pt, Point(x2, y))
      );
  end;

var
  x1, x2: Integer;
  w: Double;
  p, clickPt: TPoint;
  gp: TDoublePoint;
begin
  Unused(AXIdx);

  // Convert the "clicked" and "test" point to non-rotated axes
  if IsRotated then begin
    gp := ParentChart.ImageToGraph(AParams.FPoint);
    Exchange(gp.X, gp.Y);
    clickPt := ParentChart.GraphToImage(gp);
    Exchange(AGraphPt.X, AGraphPt.Y);
  end else
    clickPt := AParams.FPoint;

  w := CalcTickWidth(AGraphPt.X, APointIdx);
  x1 := ParentChart.XGraphToImage(AGraphPt.X - w);
  x2 := ParentChart.XGraphToImage(AGraphPt.X + w);
  p := ParentChart.GraphToImage(AGraphPt);

  case FMode of
    mOHLC:
      with ParentChart do
        if (AYIdx = YIndexOpen) then
          Result := DistanceToLine(clickPt, x1, p.x, p.y)
        else if (AYIdx = YIndexClose) then
          Result := DistanceToLine(clickPt, p.x, x2, p.y)
        else if (AYIdx = YIndexHigh) or (AYIdx = YIndexLow) then
          Result := AParams.FDistFunc(clickPt, p)
        else
          raise Exception.Create('TOpenHighLowCloseSeries.ToolTargetDistance: Illegal YIndex.');
    mCandleStick:
      with ParentChart do
        if (AYIdx = YIndexOpen) or (AYIdx = YIndexClose) then
          Result := DistanceToLine(clickPt, x1, x2, p.y)
        else if (AYIdx = YIndexHigh) or (AYIdx = YIndexLow) then
          Result := AParams.FDistFunc(clickPt, p)
        else
          raise Exception.Create('TOpenHighLowCloseSeries.ToolTargetDistance: Illegal YIndex.');
  end;
end;

procedure TOpenHighLowCloseSeries.UpdateLabelDirectionReferenceLevel(
  AIndex, AYIndex: Integer; var ALevel: Double);
var
  item: PChartDataItem;
begin
  if AYIndex = FYIndexLow then
    ALevel := +Infinity
  else if AYIndex = FYIndexHigh then
    ALevel := -Infinity
  else begin
    item := Source.Item[AIndex];
    ALevel := (AxisToGraphY(item^.GetY(FYIndexLow)) + AxisToGraphY(item^.GetY(FYIndexHigh)))*0.5;
  end;
end;


{ TFieldSeries }

constructor TFieldSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ToolTargets := [nptPoint, nptXList, nptYList, nptCustom];
  FArrow := TChartArrow.Create(ParentChart);
  FArrow.Length := 20;
  FArrow.Width := 10;
  FArrow.Visible := true;
  FPen := TPen.Create;
  FPen.OnChange := @StyleChanged;
end;

destructor TFieldSeries.Destroy;
begin
  FreeAndNil(FArrow);
  FreeAndNil(FPen);
  inherited;
end;

function TFieldSeries.AddVector(AX, AY, AVectorX, AVectorY: Double;
  AXLabel: String = ''; AColor: TColor = clTAColor): Integer;
begin
  Result := AddXY(AX, AY, AXLabel, AColor);
  SetVector(Result, DoublePoint(AVectorX, AVectorY));
end;

procedure TFieldSeries.AfterAdd;
begin
  inherited;
  FArrow.SetOwner(ParentChart);
end;

procedure TFieldSeries.Assign(ASource: TPersistent);
begin
  if ASource is TFieldSeries then
    with TFieldSeries(ASource) do begin
      Self.FArrow.Assign(FArrow);
      Self.FPen := FPen;
    end;
  inherited Assign(ASource);
end;

procedure TFieldSeries.Draw(ADrawer: IChartDrawer);
var
  ext: TDoubleRect;
  i: Integer;
  p1, p2: TDoublePoint;
  lPen: TPen;
begin
  if IsEmpty or (not Active) then exit;
  with Extent do begin
    ext.a := AxisToGraph(a);
    ext.b := AxisToGraph(b);
  end;
  NormalizeRect(ext);
  // Do not draw anything if the series extent does not intersect CurrentExtent.
  if not RectIntersectsRect(ext, ParentChart.CurrentExtent) then exit;

  lPen := TPen.Create;
  try
    lPen.Assign(FPen);
    if (AxisIndexX < 0) and (AxisIndexY < 0) then begin
      // Optimization: bypass transformations in the default case
      for i := 0 to Count - 1 do
        if GetVectorPoints(i, p1, p2) then begin
          lPen.Color := GetColor(i);
          DrawVector(ADrawer, p1, p2, lPen);
        end;
    end else begin
      for i := 0 to Count - 1 do
        if GetVectorPoints(i, p1, p2) then begin
          p1 := AxisToGraph(p1);
          p2 := AxisToGraph(p2);
          lPen.Color := GetColor(i);
          DrawVector(ADrawer, p1, p2, lPen);
        end;
    end;
    DrawLabels(ADrawer, 0);
  finally
    lPen.Free;
  end;
end;

procedure TFieldSeries.DrawVector(ADrawer: IChartDrawer;
  AStartPt, AEndPt: TDoublePoint; APen: TPen);
var
  p1, p2: TPoint;
  arr: TChartArrow;
  len: Double;
begin
  p1 := ParentChart.GraphToImage(AStartPt);
  p2 := ParentChart.GraphToImage(AEndPt);
  ADrawer.Pen := APen;
  if APen.Color = clDefault then
    ADrawer.SetPenColor(FChart.GetDefaultColor(dctFont))
  else
    ADrawer.SetPenColor(APen.Color);
  ADrawer.Line(p1.x, p1.y, p2.x, p2.y);
  if FArrow.Visible then begin
    len := sqrt(sqr(p2.x - p1.x) + sqr(p2.y - p1.y)) * 0.01 / ADrawer.Scale(1);
    // Be aware that the drawer scales pixels. But the arrow length here is
    // already at the correct size!
    arr := TChartArrow.Create(nil);
    arr.Assign(FArrow);
    arr.SetOwner(nil);  // avoid repainting due to next commands
    arr.BaseLength := round(FArrow.BaseLength * len);
    arr.Length := round(FArrow.Length * len);
    arr.Width := round(FArrow.Width * len);
    arr.Draw(ADrawer, p2, arctan2(p2.y-p1.y, p2.x-p1.x), APen);
    arr.Free;
  end;
end;

function TFieldSeries.Extent: TDoubleRect;
var
  p1, p2: TDoublePoint;
  i: Integer;
begin
  Result := Source.Extent;
  for i := 0 to Source.Count - 1 do
    if GetVectorPoints(i, p1, p2) then begin
      UpdateMinMax(p1.X, Result.a.X, Result.b.X);
      UpdateMinMax(p2.X, Result.a.X, Result.b.X);
      UpdateMinMax(p1.Y, Result.a.Y, Result.b.Y);
      UpdateMinMax(p2.Y, Result.a.Y, Result.b.Y);
    end;
end;

function TFieldSeries.GetColor(AIndex: Integer): TColor;
begin
  with Source.Item[AIndex]^ do
    Result := TColor(Math.IfThen(Color = clTAColor, FPen.Color, Color));
end;

procedure TFieldSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemField.Create(FPen, FArrow, LegendTextSingle));
end;

function TFieldSeries.GetNearestPoint(const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  dist, d, i, xidx, yidx: Integer;
  pt1, pt2: TPoint;
  sp1, sp2: TDoublePoint;
  R: TRect;
  img: TPoint;
begin
  AResults.FDist := Sqr(AParams.FRadius) + 1;
  AResults.FIndex := -1;
  AResults.FXIndex := 0;
  AResults.FYIndex := 0;
  if IsEmpty then exit(false);

  for i := 0 to Count - 1 do begin
    if not GetVectorPoints(i, sp1, sp2) then
      Continue;
    // End points of the vector arrow
    pt1 := ParentChart.GraphToImage(AxisToGraph(sp1));
    pt2 := ParentChart.GraphToImage(AxisToGraph(sp2));
    // At first we check if the point is in the rect spanned by the vector.
    R := Rect(pt1.x, pt1.y, pt2.x, pt2.y);
    NormalizeRect(R);
    R.TopLeft := R.TopLeft - Point(AParams.FRadius, AParams.FRadius);
    R.BottomRight := R.BottomRight + Point(AParams.FRadius, AParams.FRadius);
    if not IsPointInRect(AParams.FPoint, R) then continue;

    dist := MaxInt;
    xidx := -1;
    yidx := -1;
    if (nptPoint in AParams.FTargets) and (nptPoint in ToolTargets) then begin
      dist := AParams.FDistFunc(AParams.FPoint, pt1);
      xidx := 0;
      yidx := 0;
      img := pt1;
    end;

    if (AParams.FTargets * [nptXList, nptYList] <> []) and
       (ToolTargets * [nptXList, nptYList] <> [])
    then begin
      d := AParams.FDistFunc(AParams.FPoint, pt2);
      if d < dist then begin
        dist := d;
        xidx := 1;
        yidx := 1;
        img := pt2;
      end;
    end;
    // give priority to end points
    if (dist > AResults.FDist) and
       (nptCustom in AParams.FTargets) and
       (nptCustom in ToolTargets)
    then begin
      d := PointLineDistSq(AParams.FPoint, pt1, pt2);  // distance of point from line
      if d < dist then begin
        dist := d;
        xidx := -1;
        yidx := -1;
        img := ProjToLine(AParams.FPoint, pt1, pt2);
      end;
    end;
    if dist >= AResults.FDist then continue;

    AResults.FDist := dist;
    AResults.FIndex := i;
    AResults.FXIndex := xidx;
    AResults.FYIndex := yidx;
    AResults.FImg := img;
    AResults.FValue := Source[i]^.Point;
    break;
  end;
  Result := AResults.FIndex >= 0;
end;

function TFieldSeries.GetVector(AIndex: Integer): TDoublePoint;
begin
  with Source.Item[AIndex]^ do
    case FCoordKind of
      vckCenterDir: Result := DoublePoint(XList[0], YList[0]);
      vckStartEnd: Result := DoublePoint(XList[0]-X, YList[0]-Y);
    end;
end;

function TFieldSeries.GetVectorPoints(AIndex: Integer;
  out AStartPt, AEndPt: TDoublePoint): Boolean;
var
  dx, dy: Double;
begin
  with Source.Item[AIndex]^ do begin
    if isNaN(X) or IsNaN(Y) or IsNaN(XList[0]) or IsNaN(YList[0]) then
      exit(false)
    else begin
      case FCoordKind of
        vckCenterDir:
          begin
            dx := XList[0] * 0.5;
            dy := YList[0] * 0.5;
            AStartPt := DoublePoint(X - dx, Y - dy);
            AEndPt := DoublePoint(X + dx, Y + dy);
          end;
        vckStartEnd:
          begin
            AStartPt := DoublePoint(X, Y);
            AEndPt := DoublePoint(XList[0], YList[0]);
          end;
      end;
      Result := true;
    end;
  end;
end;

class procedure TFieldSeries.GetXYCountNeeded(out AXCount, AYCount: Cardinal);
begin
  AXCount := 2;
  AYCount := 2;
end;

procedure TFieldSeries.MovePointEx(var AIndex: Integer;
  AXIndex, AYIndex: Integer; const ANewPos: TDoublePoint);
var
  np, p: TDoublePoint;
begin
  Unused(AXIndex);

  if not InRange(AIndex, 0, Count - 1) then
    exit;

  p := DoublePoint(XValue[AIndex], YValue[AIndex]);
  np := GraphToAxis(ANewPos);

  ParentChart.DisableRedrawing;
  try
    case AYIndex of
     -1: begin
           ListSource.SetXValue(AIndex, np.X);
           ListSource.SetYValue(AIndex, np.Y);
         end;
      0: SetVector(AIndex, (p - np) * 2);
      1: SetVector(AIndex, (np - p) * 2);
    end;
  finally
    ParentChart.EnableRedrawing;
    UpdateParentChart;
  end;
end;

procedure TFieldSeries.NormalizeVectors(ALength: Double);
var
  factor, maxlen, len: Double;
  i: Integer;
  v: TDoublePoint;
begin
  maxLen := 0;
  for i := 0 to Count - 1 do begin
    v := GetVector(i);
    len := v.x * v.x + v.y * v.y;
    len := sqrt(v.x*v.x + v.y*v.y);
//    len := sqrt(sqr(v.x) + sqr(v.y));
    maxLen := Max(len, maxlen);
  end;
  if maxLen = 0 then
    exit;
  factor := ALength / maxLen;
  for i := 0 to Count - 1 do begin
    v := GetVector(i);
    SetVector(i, v*factor);
  end;
end;

procedure TFieldSeries.SetArrow(AValue: TChartArrow);
begin
  FArrow.Assign(AValue);
  UpdateParentChart;
end;

procedure TFieldSeries.SetCoordKind(AValue: TVectorCoordkind);
begin
  if AValue <> FCoordKind then
  begin
    FCoordKind := AValue;
    UpdateParentChart;
  end;
end;

procedure TFieldSeries.SetPen(AValue: TPen);
begin
  FPen.Assign(AValue);
end;

procedure TFieldSeries.SetVector(AIndex: Integer; const AValue: TDoublePoint);
begin
  with ListSource.Item[AIndex]^ do begin
    case FCoordKind of
      vckCenterDir:
        begin
          XList[0] := AValue.X;
          YList[0] := AValue.Y;
        end;
      vckStartEnd:
        begin
          XList[0] := X + AValue.X;
          YList[0] := Y + AValue.Y;
        end;
    end;
  end;
end;


initialization
  RegisterSeriesClass(TBubbleSeries, @rsBubbleSeries);
  RegisterSeriesClass(TBoxAndWhiskerSeries, @rsBoxAndWhiskerSeries);
  RegisterSeriesClass(TOpenHighLowCloseSeries, @rsOpenHighLowCloseSeries);
  RegisterSeriesClass(TFieldSeries, @rsFieldSeries);

end.
