unit TAStateSeries;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics,
  TAChartUtils, TADrawUtils, TAMath, TAGeometry, TALegend, TACustomSeries;

type
  EStateTimeSeriesError = class(EChartError);

  TBarHeightStyle = (bhsPercent, bhsPercentMin);

  TStateSeries = class(TBasicPointSeries)
  private
    const
      DEFAULT_BAR_HEIGHT_PERCENT = 70;
  private
    FBarHeightPercent: Integer;
    FBarHeightStyle: TBarHeightStyle;
    FBrush: TBrush;
    FPen: TPen;
    procedure SetBarHeightPercent(AValue: Integer);
    procedure SetBarHeightStyle(AValue: TBarHeightStyle);
    procedure SetBrush(AValue: TBrush);
    procedure SetPen(AValue: TPen);
  protected
    FMinYRange: Double;
    procedure CalcBarHeight(AY: Double; AIndex: Integer; out AHeight: Double);
    function GetLabelDataPoint(AIndex, AYIndex: Integer): TDoublePoint; override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function NearestYNumber(var AIndex: Integer; ADir: Integer): Double;
    procedure UpdateMinYRange;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;

    function AddXY(AStart, AEnd, Y: Double; ALabel: String;
      AColor: TColor = clTAColor): Integer;
    procedure Draw(ADrawer: IChartDrawer); override;
    function Extent: TDoubleRect; override;
    function GetBarHeight(AIndex: Integer): Integer;
    function GetNearestPoint(const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
    function GetYRange(AY: Double; AIndex: Integer): Double;
    procedure MovePointEx(var AIndex: Integer; AXIndex, AYIndex: Integer;
      const ANewPos: TDoublePoint); override;

    class procedure GetXYCountNeeded(out AXCount, AYCount: Cardinal); override;

  published
    property BarHeightPercent: Integer read FBarHeightPercent
      write SetBarHeightPercent default DEFAULT_BAR_HEIGHT_PERCENT;
    property BarHeightStyle: TBarHeightStyle read FBarHeightStyle
      write SetBarHeightStyle default bhsPercent;
    property Brush: TBrush read FBrush write SetBrush;
    property Pen: TPen read FPen write SetPen;
    property MarkPositions;
    property Marks;
    property ToolTargets default [nptPoint, nptXList, nptCustom];

  end;

implementation

uses
  TAChartStrConsts, TAGraph;

constructor TStateSeries.Create(AOwner: TComponent);
begin
  inherited;

  FPen := TPen.Create;
  FPen.OnChange := @StyleChanged;
  FPen.Color := clBlack;

  FBrush := TBrush.Create;
  FBrush.OnChange := @StyleChanged;
  FBrush.Color := clRed;

  FBarHeightPercent := DEFAULT_BAR_HEIGHT_PERCENT;
  FBarHeightStyle := bhsPercent;

  ToolTargets := [nptPoint, nptXList, nptCustom];
end;

destructor TStateSeries.Destroy;
begin
  FBrush.Free;
  FPen.Free;
  inherited;
end;

function TStateSeries.AddXY(AStart, AEnd, Y: Double;
  ALabel: String; AColor: TColor = clTAColor): Integer;
begin
  EnsureOrder(AStart, AEnd);
  Result := ListSource.AddXListY([AStart, AEnd], Y, ALabel, AColor);
end;

procedure TStateSeries.Assign(ASource: TPersistent);
begin
  if ASource is TStateSeries then
    with TStateSeries(ASource) do begin
      Self.BarHeightPercent := FBarHeightPercent;
      Self.BarHeightStyle := FBarHeightStyle;
      Self.Brush.Assign(FBrush);
      Self.Pen.Assign(FPen);
    end;
  inherited Assign(ASource);
end;

procedure TStateSeries.CalcBarHeight(AY: Double; AIndex: Integer;
  out AHeight: Double);
var
  r: Double;
begin
  case BarHeightStyle of
    bhsPercent    : r := GetYRange(AY, AIndex) * PERCENT;
    bhsPercentMin : r := FMinYRange * PERCENT;
    else
      raise EStateTimeSeriesError.Create('BarHeightStyle not implemented'){%H-};
  end;
  AHeight := r * BarHeightPercent / 2;
end;

procedure TStateSeries.Draw(ADrawer: IChartDrawer);
var
  pointIndex: Integer;
  scaledDepth: Integer;

  procedure DrawBar(const ARect: TRect);
  var
    sz: TSize;
    c: TColor;
   // ic: IChartTCanvasDrawer;   -- maybe later...
  begin
    ADrawer.Pen := FPen;
    if FPen.Color = clDefault then
      ADrawer.SetPenColor(FChart.GetDefaultColor(dctFont))
    else
      ADrawer.SetPenColor(FPen.Color);
    ADrawer.Brush := FBrush;
    if FBrush.Color = clDefault then
      ADrawer.SetBrushColor(FChart.GetDefaultColor(dctBrush))
    else
      ADrawer.SetPenColor(FPen.Color);

    c := Source[pointIndex]^.Color;
    if c <> clTAColor then
      ADrawer.BrushColor := c;

    sz := Size(ARect);
    if (sz.cx <= 2*FPen.Width) or (sz.cy <= 2*FPen.Width) then begin
      // Bars are too small to distinguish the border from the interior.
      ADrawer.SetPenParams(psSolid, ADrawer.BrushColor);
    end;
            (*  todo --- add me
    if Assigned(FOnCustomDrawBar) then begin
      FOnCustomDrawBar(Self, ADrawer, AR, pointIndex, stackIndex);
      exit;
    end;

    if Supports(ADrawer, IChartTCanvasDrawer, ic) and Assigned(OnBeforeDrawBar) then
      OnBeforeDrawBar(Self, ic.Canvas, AR, pointIndex, stackIndex, defaultDrawing);
    if not defaultDrawing then exit; *)

    ADrawer.Rectangle(ARect);
    if scaledDepth > 0 then begin
      c := ADrawer.BrushColor;
      ADrawer.BrushColor := GetDepthColor(c, true);
      ADrawer.DrawLineDepth(
        ARect.Left, ARect.Top, ARect.Right - 1, ARect.Top, scaledDepth);
      ADrawer.BrushColor := GetDepthColor(c, false);
      ADrawer.DrawLineDepth(
        ARect.Right - 1, ARect.Top, ARect.Right - 1, ARect.Bottom - 1, scaledDepth);
    end;
  end;

var
  ext2: TDoubleRect;
  h: Double;
  p: TDoublePoint;

  procedure BuildBar(x1, x2, y: Double);
  var
    graphBar: TDoubleRect;
    imageBar: TRect;
  begin
    graphBar := DoubleRect(x1, y - h, x2, y + h);
    if IsRotated then
      with graphBar do begin
        Exchange(a.X, a.Y);
        Exchange(b.X, b.Y);
      end;

    if not RectIntersectsRect(graphBar, ext2) then exit;

    with imageBar do begin
      TopLeft := ParentChart.GraphToImage(graphBar.a);
      BottomRight := ParentChart.GraphToImage(graphBar.b);
      TAGeometry.NormalizeRect(imageBar);
      if IsRotated then inc(imageBar.Right) else inc(imageBar.Bottom);

      // Draw a line instead of an empty rectangle.
      if (Left = Right) and IsRotated then Dec(Left);
      if (Bottom = Top) and not IsRotated then Inc(Bottom);
    end;
    DrawBar(imageBar);
  end;

var
  x1, x2: Double;
begin
  if IsEmpty or (not Active) then exit;

  if BarHeightStyle = bhsPercentMin then
    UpdateMinYRange;
  ext2 := ParentChart.CurrentExtent;
  ExpandRange(ext2.a.X, ext2.b.X, 1.0);
  ExpandRange(ext2.a.Y, ext2.b.Y, 1.0);

  scaledDepth := ADrawer.Scale(Depth);

  PrepareGraphPoints(ext2, true);
  for pointIndex := FLoBound to FUpBound do begin
    p := Source[pointIndex]^.Point;
    if SkipMissingValues(pointIndex) then
      continue;
    p.Y := AxisToGraphY(p.Y);
    CalcBarHeight(p.Y, pointIndex, h);

    with Source[pointIndex]^ do
    begin
      x1 := AxisToGraphX(GetX(0));
      x2 := AxisToGraphX(GetX(1));
    end;

    BuildBar(x1, x2, p.Y);
  end;

  DrawLabels(ADrawer);
end;

function TStateSeries.Extent: TDoubleRect;
var
  y, h: Double;
  i: Integer;
begin
//  Result := inherited Extent;
  Result := Source.ExtentXYList;

  if FChart = nil then
    raise EStateTimeSeriesError.Create('Calculation of TStateTimeSeries.Extent is not possible when the series is not added to a chart.');

  if IsEmpty then exit;
  if BarHeightStyle = bhsPercentMin then
    UpdateMinYRange;

  // Show lowest and highest bars fully.
  if Source.YCount = 0 then begin
    CalcBarHeight(0.0, 0, h);
    Result.a.Y -= h;
    Result.b.Y += h;
  end else begin
    i := 0;
    y := NearestYNumber(i, +1);   // --> y is in graph units
    if not IsNan(y) then begin
      CalcBarHeight(y, i, h);
      y := GraphToAxisY(y - h);   // y is in graph units, Extent in axis units!
      Result.a.Y := Min(Result.a.Y, y);
    end;
    i := Count - 1;
    y := NearestYNumber(i, -1);
    if not IsNan(y) then begin
      CalcBarHeight(y, i, h);
      y := GraphToAxisY(y + h);
      Result.b.Y := Max(Result.b.Y, y);
    end;
  end;
end;

function TStateSeries.GetBarHeight(AIndex: Integer): Integer;
var
  h: Double;
  f: TGraphToImageFunc;
begin
  CalcBarHeight(GetGraphPointX(AIndex), AIndex, h);
  if IsRotated then
    f := @FChart.YGraphToImage
  else
    f := @FChart.XGraphToImage;
  Result := Abs(f(2 * h) - f(0));
end;

function TStateSeries.GetLabelDataPoint(AIndex, AYIndex: Integer): TDoublePoint;
var
  P1, P2: TDoublePoint;
begin
  P1 := GetGraphPoint(AIndex, 0, AYIndex);
  P2 := GetGraphPoint(AIndex, 1, AYIndex);

  if IsRotated then
    Result := DoublePoint(P1.X, (P1.Y + P2.Y) / 2)
  else
    Result := DoublePoint((P1.X + P2.X) / 2, P1.Y);
end;

procedure TStateSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  GetLegendItemsRect(AItems, Brush, Pen);
end;

function TStateSeries.GetNearestPoint(const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  pointIndex: Integer;
  graphClickPt: TDoublePoint;
  sp: TDoublePoint;
  p1, p2, h: Double;
  img1, img2, imgClick: Integer;
begin
  Result := false;
  AResults.FDist := Sqr(AParams.FRadius) + 1;
  AResults.FIndex := -1;
  AResults.FXIndex := 0;
  AResults.FYIndex := 0;

  // clicked point in image units
  graphClickPt := ParentChart.ImageToGraph(AParams.FPoint);

  // Iterate through all points of the series
  for pointIndex := 0 to Count - 1 do begin
    sp := Source[pointIndex]^.Point;
    if IsNaN(sp) then
      Continue;
    if Source.YCount = 0 then
      sp.Y := pointIndex;
    sp := AxisToGraph(sp);

    if IsRotated then
    begin
      CalcBarHeight(sp.X, pointIndex, h);
      if not InRange(graphClickPt.X, sp.X - h, sp.X + h) then
        Continue;
      with Source[pointIndex]^ do
      begin
        p1 := AxisToGraphY(GetX(0));
        p2 := AxisToGraphY(GetX(1));
      end;
      img1 := ParentChart.YGraphToImage(p1);
      img2 := ParentChart.YGraphToImage(p2);
      imgClick := AParams.FPoint.Y;
    end else
    begin
      CalcBarHeight(sp.Y, pointIndex, h); // works with graph units
      if not InRange(graphClickPt.Y, sp.Y - h, sp.Y + h) then
        continue;
      with Source[pointIndex]^ do
      begin
        p1 := AxisToGraphX(GetX(0));
        p2 := AxisToGraphX(GetX(1));
      end;
      img1 := ParentChart.XGraphToImage(p1);
      img2 := ParentChart.XGraphToImage(p2);
      imgClick := AParams.FPoint.X;
    end;

    // Checking start point
    if (nptPoint in AParams.FTargets) and (nptPoint in ToolTargets) and
      InRange(imgClick, img1 - AParams.FRadius, img1 + AParams.FRadius) then
    begin
      AResults.FDist := abs(img1 - imgClick);
      AResults.FIndex := pointindex;
      AResults.FXIndex := 0;
      AResults.FValue := DoublePoint(p1, Source[pointIndex]^.Point.Y);
      Result := true;
      break;
    end;

    // Checking end point
    if (nptXList in AParams.FTargets) and (nptXList in ToolTargets) and
      InRange(imgClick, img2 - AParams.FRadius, img2 + AParams.FRadius) then
    begin
      AResults.FDist := abs(img2 - imgClick);
      AResults.FIndex := pointIndex;
      AResults.FXIndex := 1;
      AResults.FValue := DoublePoint(Source[pointindex]^.GetX(1), Source[pointindex]^.Y);
      Result := true;
      break;
    end;

    // Checking interior
    if IsRotated then
      Exchange(img1, img2);
    if (nptCustom in AParams.FTargets) and (nptCustom in ToolTargets) and
      InRange(imgClick, img1, img2) then
    begin
      AResults.FDist := abs((img1 + img2) div 2 - imgClick);
      AResults.FIndex := pointIndex;
      AResults.FXIndex := -1;
      AResults.FValue := DoublePoint((Source[pointIndex]^.GetX(0) + Source[pointIndex]^.GetX(1))/2, Source[pointIndex]^.Y);
      Result := true;
      break;
    end;
  end;

  if Result then
  begin
    AResults.FYIndex := 0;
    AResults.FImg := AParams.FPoint;
  end;
end;

class procedure TStateSeries.GetXYCountNeeded(out AXCount, AYCount: Cardinal);
begin
  AXCount := 2;
  AYCount := 1;
end;

function TStateSeries.GetYRange(AY: Double; AIndex: Integer): Double;
var
  hb, ht: Double;
  i: Integer;
begin
  if Source.YCount > 0 then begin
    i := AIndex - 1;
    hb := Abs(AY - NearestYNumber(i, -1));
    i := AIndex + 1;
    ht := Abs(AY - NearestYNumber(i, +1));
    Result := NumberOr(SafeMin(hb, ht), 1.0);
    if Result = 0.0 then
      Result := 1.0;
  end else
    Result := 1.0;
end;

procedure TStateSeries.MovePointEx(var AIndex: Integer;
  AXIndex, AYIndex: Integer; const ANewPos: TDoublePoint);
var
  np: TDoublePoint;
  x1, x2, dx: Double;
begin
  Unused(AYIndex);

  if not InRange(AIndex, 0, Count - 1) then
    exit;

  x1 := XValues[AIndex, 0];
  x2 := XValues[AIndex, 1];
  dx := (x2 - x1) / 2;
  np := GraphToAxis(ANewPos);

  ParentChart.DisableRedrawing;
  try
    case AXIndex of
     -1: begin
           x1 := np.X - dx;
           x2 := np.X + dx;
         end;
      0: x1 := np.X;
      1: x2 := np.X;
    end;
    EnsureOrder(x1, x2);
    with ListSource.Item[AIndex]^ do
    begin
      SetX(0, x1);
      SetX(1, x2);
    end;
  finally
    ParentChart.EnableRedrawing;
    UpdateParentChart;
  end;
end;

function TStateSeries.NearestYNumber(var AIndex: Integer; ADir: Integer): Double;
begin
  while InRange(AIndex, 0, Count - 1) do
    with Source[AIndex]^ do
      if IsNan(Y) then
        AIndex += ADir
      else
        exit(AxisToGraphY(Y));
  Result := SafeNan;
end;

procedure TStateSeries.SetBarHeightPercent(AValue: Integer);
begin
  if FBarHeightPercent = AValue then
    exit;
  if (csDesigning in ComponentState) and (AValue < 1) or (AValue > 100) then
    raise EStateTimeSeriesError.Create('Wrong BarHeight Percent');
  FBarHeightPercent := EnsureRange(AValue, 1, 100);
  UpdateParentChart;
end;

procedure TStateSeries.SetBarHeightStyle(AValue: TBarHeightStyle);
begin
  if FBarHeightStyle = AValue then
    exit;
  FBarHeightStyle := AValue;
  UpdateParentChart;
end;

procedure TStateSeries.SetBrush(AValue: TBrush);
begin
  FBrush.Assign(AValue);
end;

procedure TStateSeries.SetPen(AValue: TPen);
begin
  FPen.Assign(AValue);
end;

procedure TStateSeries.UpdateMinYRange;
var
  Y, prevY: Double;
  i: Integer;
begin
  if (Count < 2) or (Source.YCount = 0) then begin
    FMinYRange := 1.0;
    exit;
  end;
  Y := Source[0]^.Y;
  prevY := Source[1]^.Y;
  FMinYRange := Abs(Y - prevY);
  for i := 2 to Count - 1 do begin
    Y := Source[i]^.Y;
    FMinYRange := SafeMin(Abs(Y - prevY), FMinYRange);
    prevY := Y;
  end;
end;


initialization
  RegisterSeriesClass(TStateSeries, @rsStateSeries);

end.

