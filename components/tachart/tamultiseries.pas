{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}

unit TAMultiSeries;

{$H+}

interface

uses
  Classes, Graphics,
  TAChartUtils, TATypes, TACustomSeries, TADrawUtils, TALegend;

const
  DEF_BOX_WIDTH = 50;
  DEF_WHISKERS_WIDTH = 25;
  DEF_OHLC_TICK_WIDTH = 25;
  DEF_YINDEX_OPEN = 1;
  DEF_YINDEX_HIGH = 3;
  DEF_YINDEX_LOW = 0;
  DEF_YINDEX_CLOSE = 2;

type

  TBubbleRadiusTransform = (brtNone, brtX, brtY);
  TBubbleOverrideColor = (bocBrush, bocPen);
  TBubbleOverrideColors = set of TBubbleOverrideColor;

  { TBubbleSeries }

  TBubbleSeries = class(TBasicPointSeries)
  private
    FBubbleBrush: TBrush;
    FBubblePen: TPen;
    FOverrideColor: TBubbleOverrideColors;
    procedure SetBubbleBrush(AValue: TBrush);
    procedure SetBubblePen(AValue: TPen);
    procedure SetOverrideColor(AValue: TBubbleOverrideColors);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetSeriesColor: TColor; override;
  public
    function AddXY(AX, AY, ARadius: Double; AXLabel: String = '';
      AColor: TColor = clTAColor): Integer; overload;
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Draw(ADrawer: IChartDrawer); override;
    function Extent: TDoubleRect; override;
  published
    property AxisIndexX;
    property AxisIndexY;
    property BubbleBrush: TBrush read FBubbleBrush write SetBubbleBrush;
    property BubblePen: TPen read FBubblePen write SetBubblePen;
    property OverrideColor: TBubbleOverrideColors
      read FOverrideColor write SetOverrideColor default [];
    property Source;
  end;

  TBoxAndWhiskerSeriesLegendDir = (bwlHorizontal, bwlVertical, bwlAuto);
  TBoxAndWhiskerSeriesWidthStyle = (bwsPercent, bwsPercentMin);

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
    procedure SetBoxBrush(AValue: TBrush);
    procedure SetBoxPen(AValue: TPen);
    procedure SetBoxWidth(AValue: Integer);
    procedure SetLegendDirection(AValue: TBoxAndWhiskerSeriesLegendDir);
    procedure SetMedianPen(AValue: TPen);
    procedure SetWhiskersPen(AValue: TPen);
    procedure SetWhiskersWidth(AValue: Integer);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetSeriesColor: TColor; override;
  public
    function AddXY(
      AX, AYLoWhisker, AYLoBox, AY, AYHiBox, AYHiWhisker: Double;
      AXLabel: String = ''; AColor: TColor = clTAColor): Integer; overload;
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Draw(ADrawer: IChartDrawer); override;
    function Extent: TDoubleRect; override;
  published
    property BoxBrush: TBrush read FBoxBrush write SetBoxBrush;
    property BoxPen: TPen read FBoxPen write SetBoxPen;
    property BoxWidth: Integer
      read FBoxWidth write SetBoxWidth default DEF_BOX_WIDTH;
    property LegendDirection: TBoxAndWhiskerSeriesLegendDir
      read FLegendDirection write SetLegendDirection default bwlHorizontal;
    property MedianPen: TPen read FMedianPen write SetMedianPen;
    property WidthStyle: TBoxAndWhiskerSeriesWidthStyle
      read FWidthStyle write FWidthStyle default bwsPercent;
    property WhiskersPen: TPen read FWhiskersPen write SetWhiskersPen;
    property WhiskersWidth: Integer
      read FWhiskersWidth write SetWhiskersWidth default DEF_WHISKERS_WIDTH;
  published
    property AxisIndexX;
    property AxisIndexY;
    property Source;
  end;

  TOHLCDownPen = class(TPen)
  published
    property Color default clTAColor;
  end;

  TOHLCMode = (mOHLC, mCandleStick);

  TOpenHighLowCloseSeries = class(TBasicPointSeries)
  private
    FCandlestickDownBrush: TBrush;
    FCandleStickLinePen: TPen;
    FCandlestickUpBrush: TBrush;
    FDownLinePen: TOHLCDownPen;
    FLinePen: TPen;
    FTickWidth: Cardinal;
    FYIndexClose: Cardinal;
    FYIndexHigh: Cardinal;
    FYIndexLow: Cardinal;
    FYIndexOpen: Cardinal;
    FMode: TOHLCMode;
    procedure SetCandlestickLinePen(AValue: TPen);
    procedure SetCandlestickDownBrush(AValue: TBrush);
    procedure SetCandlestickUpBrush(AValue: TBrush);
    procedure SetDownLinePen(AValue: TOHLCDownPen);
    procedure SetLinePen(AValue: TPen);
    procedure SetOHLCMode(AValue: TOHLCMode);
    procedure SetTickWidth(AValue: Cardinal);
    procedure SetYIndexClose(AValue: Cardinal);
    procedure SetYIndexHigh(AValue: Cardinal);
    procedure SetYIndexLow(AValue: Cardinal);
    procedure SetYIndexOpen(AValue: Cardinal);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetSeriesColor: TColor; override;
    function ToolTargetDistance(const AParams: TNearestPointParams;
      AGraphPt: TDoublePoint; APointIdx, AXIdx, AYIdx: Integer): Integer; override;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddXOHLC(
      AX, AOpen, AHigh, ALow, AClose: Double;
      ALabel: String = ''; AColor: TColor = clTAColor): Integer; inline;
    procedure Draw(ADrawer: IChartDrawer); override;
    function Extent: TDoubleRect; override;
    function GetNearestPoint(const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
  published
    property CandlestickDownBrush: TBrush
      read FCandlestickDownBrush write SetCandlestickDownBrush;
    property CandlestickLinePen: TPen
      read FCandlestickLinePen write FCandleStickLinePen;
    property CandlestickUpBrush: TBrush
      read FCandlestickUpBrush write SetCandlestickUpBrush;
    property DownLinePen: TOHLCDownPen read FDownLinePen write SetDownLinePen;
    property LinePen: TPen read FLinePen write SetLinePen;
    property Mode: TOHLCMode read FMode write SetOHLCMode;
    property TickWidth: Cardinal
      read FTickWidth write SetTickWidth default DEF_OHLC_TICK_WIDTH;
    property YIndexClose: Cardinal
      read FYIndexClose write SetYIndexClose default DEF_YINDEX_CLOSE;
    property YIndexHigh: Cardinal
      read FYIndexHigh write SetYIndexHigh default DEF_YINDEX_HIGH;
    property YIndexLow: Cardinal
      read FYIndexLow write SetYIndexLow default DEF_YINDEX_LOW;
    property YIndexOpen: Cardinal
      read FYIndexOpen write SetYIndexOpen default DEF_YINDEX_OPEN;
  published
    property AxisIndexX;
    property AxisIndexY;
    property Source;
  end;

  TFieldSeries = class(TBasicPointSeries)
  private
    FArrow: TChartArrow;
    FPen: TPen;
    procedure SetArrow(AValue: TChartArrow);
    procedure SetPen(AValue: TPen);
  protected
    procedure AfterAdd; override;
    procedure DrawVector(ADrawer: IChartDrawer; AStartPt, AEndPt: TDoublePoint;
      APen: TPen);
    function GetColor(AIndex: Integer): TColor; inline;
    function GetVectorPoints(AIndex: Integer;
      out AStartPt, AEndPt: TDoublePoint): Boolean; inline;
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
    procedure NormalizeVectors(ALength: Double);

  published
    property Arrow: TChartArrow read FArrow write SetArrow;
    property AxisIndexX;
    property AxisIndexY;
    property Pen: TPen read FPen write SetPen;
    property Source;
  end;

implementation

uses
  FPCanvas, Math, SysUtils,
  TAChartStrConsts, TACustomSource, TAGeometry, TAGraph, TAMath;

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
  bw -= IfThen(FBoxPen.Style = psClear, 0, (FBoxPen.Width + 1) div 2);
  symbol[5] := Rect(center.x, center.y - bw, center.x, center.y + bw);

  if FIsVertical then
    for i := 1 to High(symbol) do
      symbol[i] := FlipRect(symbol[i]);

  ADrawer.Pen := FWhiskersPen;
  ADrawer.SetBrushParams(bsClear, clTAColor);
  for i := 1 to 3 do
    ADrawer.Line(symbol[i].TopLeft, symbol[i].BottomRight);
  ADrawer.Pen := FBoxPen;
  ADrawer.Brush:= FBoxBrush;
  ADrawer.Rectangle(symbol[4]);
  ADrawer.Pen := FMedianPen;
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
  if ListSource.YCount < 2 then ListSource.YCount := 2;
  Result := AddXY(AX, AY, [ARadius], AXLabel, AColor);
end;

procedure TBubbleSeries.Assign(ASource: TPersistent);
begin
  if ASource is TBubbleSeries then
    with TBubbleSeries(ASource) do begin
      Self.BubbleBrush := FBubbleBrush;
      Self.BubblePen := FBubblePen;
    end;
  inherited Assign(ASource);
end;

constructor TBubbleSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBubblePen := TPen.Create;
  FBubblePen.OnChange := @StyleChanged;
  FBubbleBrush := TBrush.Create;
  FBubbleBrush.OnChange := @StyleChanged;
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
  pt, d: TPoint;
  r: Double;
  pi: PChartDataItem;
begin
  if Source.YCount < 2 then exit;
  r := 0;
  for i := 0 to Count - 1 do
    if IsNaN(Source[i]^.YList[0]) then
      continue
    else
      r := Max(Source[i]^.YList[0], r);
  with ParentChart.CurrentExtent do
    PrepareGraphPoints(DoubleRect(a.X - r, a.Y - r, b.X + r, b.Y + r), true);
  ADrawer.Pen := BubblePen;
  ADrawer.Brush := BubbleBrush;
  for i := 0 to High(FGraphPoints) do begin
    if IsNaN(FGraphPoints[i].X) or IsNaN(FGraphPoints[i].Y) then
      Continue;
    pt := ParentChart.GraphToImage(FGraphPoints[i]);
    pi := Source[i + FLoBound];
    r := pi^.YList[0];
    if IsNaN(r) then
      Continue;
    d.X := ParentChart.XGraphToImage(r) - ParentChart.XGraphToImage(0);
    d.Y := ParentChart.YGraphToImage(r) - ParentChart.YGraphToImage(0);
    if bocPen in OverrideColor then
      ADrawer.SetPenParams(BubblePen.Style, ColorDef(pi^.Color, BubblePen.Color));
    if bocBrush in OverrideColor then
      ADrawer.SetBrushColor(ColorDef(pi^.Color, BubbleBrush.Color));
    ADrawer.Ellipse(pt.X - d.X, pt.Y - d.Y, pt.X + d.X, pt.Y + d.Y);
  end;
  DrawLabels(ADrawer);
end;

function TBubbleSeries.Extent: TDoubleRect;
var
  i: Integer;
  r: Double;
begin
  Result := EmptyExtent;
  if Source.YCount < 2 then exit;
  for i := 0 to Count - 1 do
    with Source[i]^ do begin
      r := YList[0];
      if IsNaN(X) or IsNaN(Y) or IsNaN(r) then continue;
      Result.a.X := Min(Result.a.X, X - r);
      Result.b.X := Max(Result.b.X, X + r);
      Result.a.Y := Min(Result.a.Y, Y - r);
      Result.b.Y := Max(Result.b.Y, Y + r);
    end;
end;

procedure TBubbleSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  GetLegendItemsRect(AItems, BubbleBrush);
end;

function TBubbleSeries.GetSeriesColor: TColor;
begin
  Result := FBubbleBrush.Color;
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

procedure TBubbleSeries.SetOverrideColor(AValue: TBubbleOverrideColors);
begin
  if FOverrideColor = AValue then exit;
  FOverrideColor := AValue;
  UpdateParentChart;
end;

{ TBoxAndWhiskerSeries }

function TBoxAndWhiskerSeries.AddXY(
  AX, AYLoWhisker, AYLoBox, AY, AYHiBox, AYHiWhisker: Double; AXLabel: String;
  AColor: TColor): Integer;
begin
  if ListSource.YCount < 5 then ListSource.YCount := 5;
  Result := AddXY(
    AX, AYLoWhisker, [AYLoBox, AY, AYHiBox, AYHiWhisker], AXLabel, AColor);
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
    end;
  inherited Assign(ASource);
end;

constructor TBoxAndWhiskerSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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

  procedure DoRect(AX1, AY1, AX2, AY2: Double);
  var
    r: TRect;
  begin
    with ParentChart do begin
      r.TopLeft := MaybeRotate(AX1, AY1);
      r.BottomRight := MaybeRotate(AX2, AY2);
    end;
    ADrawer.Rectangle(r);
  end;

var
  ext2: TDoubleRect;
  x, ymin, yqmin, ymed, yqmax, ymax, wb, ww, w: Double;
  i: Integer;
begin
  if IsEmpty or (Source.YCount < 5) then
    exit;
  if FWidthStyle = bwsPercentMin then
    UpdateMinXRange;

  ext2 := ParentChart.CurrentExtent;
  ExpandRange(ext2.a.X, ext2.b.X, 1.0);
  ExpandRange(ext2.a.Y, ext2.b.Y, 1.0);

  PrepareGraphPoints(ext2, true);

  for i := FLoBound to FUpBound do begin
    x := GetGraphPointX(i);
    ymin := GetGraphPointY(i);
    if IsNaN(x) or IsNaN(ymin) then
      continue;
    with Source[i]^ do begin
      if IsNaN(YList[0]) then continue else yqmin := AxisToGraphY(YList[0]);
      if IsNaN(YList[1]) then continue else ymed := AxisToGraphY(YList[1]);
      if IsNaN(YList[2]) then continue else yqmax := AxisToGraphY(YList[2]);
      if IsNaN(YList[3]) then continue else ymax := AxisToGraphY(YList[3]);
    end;
    case FWidthStyle of
      bwsPercent: w := GetXRange(x, i) * PERCENT / 2;
      bwsPercentMin: w := FMinXRange * PERCENT / 2;
    end;
    wb := w * BoxWidth;
    ww := w * WhiskersWidth;

    ADrawer.Pen := WhiskersPen;
    ADrawer.SetBrushParams(bsClear, clTAColor);
    DoLine(x - ww, ymin, x + ww, ymin);
    DoLine(x, ymin, x, yqmin);
    DoLine(x - ww, ymax, x + ww, ymax);
    DoLine(x, ymax, x, yqmax);
    ADrawer.Pen := BoxPen;
    if Source[i]^.Color <> clTAColor then
      ADrawer.SetBrushParams(bsSolid, Source[i]^.Color)
    else
      ADrawer.Brush := BoxBrush;
    DoRect(x - wb, yqmin, x + wb, yqmax);
    ADrawer.Pen := MedianPen;
    ADrawer.SetBrushParams(bsClear, clTAColor);
    DoLine(x - wb, ymed, x + wb, ymed);
  end;
end;

function TBoxAndWhiskerSeries.Extent: TDoubleRect;
var
  x: Double;

  function ExtraWidth(AIndex: Integer): Double;
  begin
    Result := GetXRange(x, AIndex) * Max(BoxWidth, WhiskersWidth) * PERCENT / 2;
  end;

begin
  if Source.YCount < 5 then exit(EmptyExtent);
  Result := Source.ExtentList;
  // Show first and last boxes fully.
  x := GetGraphPointX(0);
  Result.a.X := Min(Result.a.X, GraphToAxisX(x - ExtraWidth(0)));
  x := GetGraphPointX(Count - 1);
  Result.b.X := Max(Result.b.X, GraphToAxisX(x + ExtraWidth(Count - 1)));
end;

procedure TBoxAndWhiskerSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemBoxAndWhiskers.Create(Self, LegendTextSingle));
end;

function TBoxAndWhiskerSeries.GetSeriesColor: TColor;
begin
  Result := BoxBrush.Color;
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

{ TOpenHighLowCloseSeries }

function TOpenHighLowCloseSeries.AddXOHLC(
  AX, AOpen, AHigh, ALow, AClose: Double;
  ALabel: String; AColor: TColor): Integer;
begin
  if ListSource.YCount < 4 then ListSource.YCount := 4;
  Result := ListSource.Add(AX, NaN, ALabel, AColor);
  with ListSource.Item[Result]^ do begin
    SetY(YIndexOpen, AOpen);
    SetY(YIndexHigh, AHigh);
    SetY(YIndexLow, ALow);
    SetY(YIndexClose, AClose);
  end;
end;

procedure TOpenHighLowCloseSeries.Assign(ASource: TPersistent);
begin
  if ASource is TOpenHighLowCloseSeries then
    with TOpenHighLowCloseSeries(ASource) do begin
      Self.FCandlestickDownBrush := FCandlestickDownBrush;
      Self.FCandlestickLinePen := FCandlestickLinePen;
      Self.FCandlestickUpBrush := FCandlestickUpBrush;
      Self.FDownLinePen := FDownLinePen;
      Self.FLinePen := FLinePen;
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
  FStacked := false;
  FCandlestickDownBrush := TBrush.Create;
  with FCandlestickDownBrush do begin
    Color := clRed;
    OnChange := @StyleChanged;
  end;
  FCandlestickLinePen := TPen.Create;
  with FCandlestickLinePen do begin
    Color := clBlack;
    OnChange := @StyleChanged;
  end;
  FCandlestickUpBrush := TBrush.Create;
  with FCandlestickUpBrush do begin
    Color := clLime;
    OnChange := @StyleChanged;
  end;
  FDownLinePen := TOHLCDownPen.Create;
  with FDownLinePen do begin
    Color := clTAColor;
    OnChange := @StyleChanged;
  end;
  FLinePen := TPen.Create;
  with FLinePen do
    OnChange := @StyleChanged;
  FTickWidth := DEF_OHLC_TICK_WIDTH;
  FYIndexClose := DEF_YINDEX_CLOSE;
  FYIndexHigh := DEF_YINDEX_HIGH;
  FYIndexLow := DEF_YINDEX_LOW;
  FYIndexOpen := DEF_YINDEX_OPEN;
end;

destructor TOpenHighLowCloseSeries.Destroy;
begin
  FreeAndNil(FCandlestickDownBrush);
  FreeAndNil(FCandlestickLinePen);
  FreeAndNil(FCandlestickUpBrush);
  FreeAndNil(FDownLinePen);
  FreeAndNil(FLinePen);
  inherited;
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

  procedure DoRect(AX1, AY1, AX2, AY2: Double);
  var
    r: TRect;
  begin
    with ParentChart do begin
      r.TopLeft := MaybeRotate(AX1, AY1);
      r.BottomRight := MaybeRotate(AX2, AY2);
    end;
    ADrawer.FillRect(r.Left, r.Top, r.Right, r.Bottom);
    ADrawer.Rectangle(r);
  end;

  procedure DrawOHLC(x, yopen, yhigh, ylow, yclose, tw: Double);
  begin
    DoLine(x, yhigh, x, ylow);
    DoLine(x - tw, yopen, x, yopen);
    DoLine(x, yclose, x + tw, yclose);
  end;

  procedure DrawCandleStick(x, yopen, yhigh, ylow, yclose, tw: Double);
  var
    clr: TColor;
  begin
    ADrawer.Pen := FCandlestickLinePen;
    if FCandleStickLinePen.Color = clDefault then begin
      if yopen <= yclose then
        clr := FCandleStickUpBrush.Color
      else
        clr := FCandleStickDownBrush.Color;
      ADrawer.SetPenParams(FCandleStickLinePen.Style, clr);
    end;
    DoLine(x, yhigh, x, ylow);
    DoRect(x - tw, yopen, x + tw, yclose);
  end;

var
  my: Cardinal;
  ext2: TDoubleRect;
  i: Integer;
  x, tw, yopen, yhigh, ylow, yclose: Double;
  p: TPen;
begin
  my := MaxIntValue([YIndexOpen, YIndexHigh, YIndexLow, YIndexClose]);
  if IsEmpty or (my >= Source.YCount) then exit;

  ext2 := ParentChart.CurrentExtent;
  ExpandRange(ext2.a.X, ext2.b.X, 1.0);
  ExpandRange(ext2.a.Y, ext2.b.Y, 1.0);

  PrepareGraphPoints(ext2, true);

  for i := FLoBound to FUpBound do begin
    x := GetGraphPointX(i);
    yopen := GetGraphPointY(i, YIndexOpen);
    yhigh := GetGraphPointY(i, YIndexHigh);
    ylow := GetGraphPointY(i, YIndexLow);
    yclose := GetGraphPointY(i, YIndexClose);
    tw := GetXRange(x, i) * PERCENT * TickWidth;

    if (yopen <= yclose) then begin
      p := LinePen;
      ADrawer.Brush := FCandleStickUpBrush;
    end
    else begin
      p := DownLinePen;
      ADrawer.Brush := FCandleStickDownBrush;
    end;
    ADrawer.Pen := p;
    with Source[i]^ do
      if Color <> clTAColor then
        ADrawer.SetPenParams(p.Style, Color);

    case FMode of
      mOHLC: DrawOHLC(x, yopen, yhigh, ylow, yclose, tw);
      mCandleStick: DrawCandleStick(x, yopen, yhigh, ylow, yclose, tw);
    end;
  end;
end;

function TOpenHighLowCloseSeries.Extent: TDoubleRect;
var
  x: Double;
  tw: Double;
begin
  if Source.YCount < 4 then exit(EmptyExtent);
  Result := Source.ExtentList;                            // axis units
  // Show first and last open/close ticks and candle boxes fully.
  x := GetGraphPointX(0);                                 // graph units
  tw := GetXRange(x, 0) * PERCENT * TickWidth;
  Result.a.X := Min(Result.a.X, GraphToAxisX(x - tw));    // axis units
//  Result.a.X := Min(Result.a.X, x - tw);
  x := GetGraphPointX(Count - 1);
  tw := GetXRange(x, Count - 1) * PERCENT * TickWidth;
  Result.b.X := Max(Result.b.X, AxisToGraphX(x + tw));
//  Result.b.X := Max(Result.b.X, x + tw);
end;

procedure TOpenHighLowCloseSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemOHLCLine.Create(Self, LegendTextSingle));
end;

function TOpenHighLowCloseSeries.GetNearestPoint(const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  i: Integer;
  graphClickPt: TDoublePoint;
  x, yopen, yhigh, ylow, yclose, tw: Double;
  R: TDoubleRect;
begin
  Result := false;
  AResults.FDist := Sqr(AParams.FRadius) + 1;
  AResults.FIndex := -1;
  AResults.FXIndex := 0;
  AResults.FYIndex := 0;

  if not (nptCustom in AParams.FTargets) then begin
    Result := inherited;
    exit;
  end;

  graphClickPt := ParentChart.ImageToGraph(AParams.FPoint);
  if IsRotated then
    Exchange(graphclickpt.X, graphclickpt.Y);

  // Iterate through all points of the series
  for i := 0 to Count - 1 do begin
    x := GetGraphPointX(i);
    yopen := GetGraphPointY(i, YIndexOpen);
    yhigh := GetGraphPointY(i, YIndexHigh);
    ylow := GetGraphPointY(i, YIndexLow);
    yclose := GetGraphPointY(i, YIndexClose);
    tw := GetXRange(x, i) * PERCENT * TickWidth;
    R.a := DoublePoint(x - tw, MinValue([yopen, yhigh, ylow, yclose]));
    R.b := DoublePoint(x + tw, MaxValue([yopen, yhigh, ylow, yclose]));
    if InRange(graphClickPt.X, R.a.x, R.b.x) and
       InRange(graphClickPt.Y, R.a.Y, R.b.Y) then
    begin
      AResults.FDist := 0;
      AResults.FIndex := i;
      AResults.FValue := DoublePoint(x, yopen);
      AResults.FImg := ParentChart.GraphToImage(AResults.FValue);
      Result := true;
      exit;
    end;
  end;
end;

function TOpenHighLowCloseSeries.GetSeriesColor: TColor;
begin
  Result := LinePen.Color;
end;

procedure TOpenHighLowCloseSeries.SetCandlestickLinePen(AValue: TPen);
begin
  if FCandleStickLinePen = AValue then exit;
  FCandleStickLinePen.Assign(AValue);
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetCandlestickDownBrush(AValue: TBrush);
begin
  if FCandlestickDownBrush = AValue then exit;
  FCandlestickDownBrush.Assign(AValue);
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetCandlestickUpBrush(AValue: TBrush);
begin
  if FCandlestickUpBrush = AValue then exit;
  FCandlestickUpBrush.Assign(AValue);
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetDownLinePen(AValue: TOHLCDownPen);
begin
  if FDownLinePen = AValue then exit;
  FDownLinePen.Assign(AValue);
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetLinePen(AValue: TPen);
begin
  if FLinePen = AValue then exit;
  FLinePen.Assign(AValue);
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetOHLCMode(AValue: TOHLCMode);
begin
  if FMode = AValue then exit;
  FMode := AValue;
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetTickWidth(AValue: Cardinal);
begin
  if FTickWidth = AValue then exit;
  FTickWidth := AValue;
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetYIndexClose(AValue: Cardinal);
begin
  if FYIndexClose = AValue then exit;
  FYIndexClose := AValue;
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetYIndexHigh(AValue: Cardinal);
begin
  if FYIndexHigh = AValue then exit;
  FYIndexHigh := AValue;
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetYIndexLow(AValue: Cardinal);
begin
  if FYIndexLow = AValue then exit;
  FYIndexLow := AValue;
  UpdateParentChart;
end;

procedure TOpenHighLowCloseSeries.SetYIndexOpen(AValue: Cardinal);
begin
  if FYIndexOpen = AValue then exit;
  FYIndexOpen := AValue;
  UpdateParentChart;
end;

function TOpenHighLowCloseSeries.ToolTargetDistance(
  const AParams: TNearestPointParams; AGraphPt: TDoublePoint;
  APointIdx, AXIdx, AYIdx: Integer): Integer;

  function DistanceToLine(x1, x2, y: Integer): Integer;
  begin
    if InRange(AParams.FPoint.X, x1, x2) then
      Result := sqr(AParams.FPoint.Y - y)   // FDistFunc does not calc sqrt
    else
      Result := Min(
        AParams.FDistFunc(AParams.FPoint, Point(x1, y)),
        AParams.FDistFunc(AParams.FPoint, Point(x2, y))
      );
  end;

var
  x1, x2, w: Double;
  p: TPoint;
begin
  Unused(AXIdx);

  p := ParentChart.GraphToImage(AGraphPt);
  w := GetXRange(AGraphPt.X, APointIdx) * PERCENT * TickWidth;
  x1 := AGraphPt.X - w;
  x2 := AGraphPt.X + w;

  case FMode of
    mOHLC:
      with ParentChart do
        if (AYIdx = YIndexOpen) or (AYIdx = YIndexClose) then
          Result := DistanceToLine(XGraphToImage(x1), XGraphToImage(x2), p.y)
        else if (AYIdx = YIndexHigh) or (AYIdx = YIndexLow) then
          Result := AParams.FDistFunc(AParams.FPoint, p)
        else
         raise Exception.Create('TOpenHighLowCloseSeries.ToolTargetDistance: Illegal YIndex.');
    mCandleStick:
      with ParentChart do
        if (AYIdx = YIndexOpen) then
          Result := DistanceToLine(XGraphToImage(x1), p.x, p.y)
        else if (AYIdx = YIndexClose) then
          Result := DistanceToLine(p.x, XGraphToImage(x2), p.y)
        else if (AYIdx = YIndexHigh) or (AYIdx = YIndexLow) then
          Result := AParams.FDistFunc(AParams.FPoint, p)
        else
         raise Exception.Create('TOpenHighLowCloseSeries.ToolTargetDistance: Illegal YIndex.');
  end;
end;


{ TFieldSeries }

constructor TFieldSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ListSource.XCount := 2;
  ListSource.YCount := 2;
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
  with Extent do begin
    ext.a := AxisToGraph(a);
    ext.b := AxisToGraph(b);
  end;
  NormalizeRect(ext);
  // Do not draw anything if the series extent does not intersect CurrentExtent.
  if not RectIntersectsRect(ext, ParentChart.CurrentExtent) then exit;

  lPen := TPen.Create;
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
        p1 := DoublePoint(AxisToGraphX(p1.X), AxisToGraphY(p1.Y));
        p2 := DoublePoint(AxisToGraphX(p2.X), AxisToGraphY(p2.Y));
        lPen.Color := GetColor(i);
        DrawVector(ADrawer, p1, p2, lPen);
      end;
  end;

  lPen.Free;
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
    Result := TColor(IfThen(Color = clTAColor, FPen.Color, Color));
end;

procedure TFieldSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemField.Create(FPen, FArrow, LegendTextSingle));
end;

function TFieldSeries.GetNearestPoint(const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  dist, i: Integer;
  pt1, pt2: TPoint;
  sp1, sp2: TDoublePoint;
  R: TRect;
begin
  AResults.FDist := Sqr(AParams.FRadius) + 1;
  AResults.FIndex := -1;
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
    // Calculate distance of point from line
    dist := PointLineDist(AParams.FPoint, pt1, pt2);
    if dist >= AParams.FRadius then continue;
    AResults.FDist := dist;
    AResults.FIndex := i;
    AResults.FImg := (pt1 + pt2) div 2;
    AResults.FValue := Source.Item[i]^.Point;
    break;
  end;
  Result := AResults.FIndex >= 0;
end;

function TFieldSeries.GetVector(AIndex: Integer): TDoublePoint;
begin
  with Source.Item[AIndex]^ do
    Result := DoublePoint(XList[0], YList[0]);
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
      dx := XList[0] * 0.5;
      dy := YList[0] * 0.5;
      AStartPt := DoublePoint(X - dx, Y - dy);
      AEndPt := DoublePoint(X + dx, Y + dy);
      Result := true;
    end;
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

procedure TFieldSeries.SetPen(AValue: TPen);
begin
  FPen.Assign(AValue);
end;

procedure TFieldSeries.SetVector(AIndex: Integer; const AValue: TDoublePoint);
begin
  with ListSource.Item[AIndex]^ do begin
    XList[0] := AValue.X;
    YList[0] := AValue.Y;
  end;
end;


initialization
  RegisterSeriesClass(TBubbleSeries, @rsBubbleSeries);
  RegisterSeriesClass(TBoxAndWhiskerSeries, @rsBoxAndWhiskerSeries);
  RegisterSeriesClass(TOpenHighLowCloseSeries, @rsOpenHighLowCloseSeries);
  RegisterSeriesClass(TFieldSeries, @rsFieldSeries);

end.
