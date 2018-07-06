{

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}

unit TARadialSeries;

{$H+}

interface

uses
  Classes, Graphics, SysUtils, Types,
  TAChartUtils, TACustomSeries, TADrawUtils, TALegend;

type

  { TLegendItemPie }

  TLegendItemPie = class(TLegendItem)
  private
    FColors: array [0..2] of TChartColor;
    procedure SetColors(AIndex: Integer; AValue: TChartColor);
  public
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
    property Colors[AIndex: Integer]: TChartColor write SetColors;
  end;

  { TLegendItemPieSlice }

  TLegendItemPieSlice = class(TLegendItem)
  public
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
  end;

  TLabelParams = record
    FAttachment: TPoint;
    FCenter: TPoint;
    FText: String;
  end;

  TPieSlice = object
    FBase: TPoint;
    FLabel: TLabelParams;
    FOrigIndex: Integer;
    FPrevAngle, FNextAngle: Double;
    FVisible: Boolean;
    function Angle: Double; inline;
    function CenterAngle: Double; inline;
  end;

  TPieMarkPositions = (pmpAround, pmpInside, pmpLeftRight);

  { TCustomPieSeries }

  TCustomPieSeries = class(TChartSeries)
  private
    FCenter: TPoint;
    FMarkDistancePercent: Boolean;
    FMarkPositions: TPieMarkPositions;
    FRadius: Integer;
    FSlices: array of TPieSlice;
  private
    FEdgePen: TPen;
    FExploded: Boolean;
    FFixedRadius: TChartDistance;
    FRotateLabels: Boolean;
    procedure Measure(ADrawer: IChartDrawer);
    procedure SetEdgePen(AValue: TPen);
    procedure SetExploded(AValue: Boolean);
    procedure SetFixedRadius(AValue: TChartDistance);
    procedure SetMarkDistancePercent(AValue: Boolean);
    procedure SetMarkPositions(AValue: TPieMarkPositions);
    procedure SetRotateLabels(AValue: Boolean);
    function SliceColor(AIndex: Integer): TColor;
    function TryRadius(ADrawer: IChartDrawer): TRect;
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    property Radius: Integer read FRadius;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function AddPie(AValue: Double; AText: String; AColor: TColor): Integer;
    procedure Assign(ASource: TPersistent); override;
    procedure Draw(ADrawer: IChartDrawer); override;
    function FindContainingSlice(const APoint: TPoint): Integer;
    function GetNearestPoint(const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
    procedure MovePointEx(var AIndex: Integer; AXIndex, AYIndex: Integer;
      const ANewPos: TDoublePoint); override;

    property EdgePen: TPen read FEdgePen write SetEdgePen;
    // Offset slices away from center based on X value.
    property Exploded: Boolean read FExploded write SetExploded default false;
    property FixedRadius: TChartDistance
      read FFixedRadius write SetFixedRadius default 0;
    property MarkDistancePercent: Boolean
      read FMarkDistancePercent write SetMarkDistancePercent default false;
    property MarkPositions: TPieMarkPositions
      read FMarkPositions write SetMarkPositions default pmpAround;
    property RotateLabels: Boolean
      read FRotateLabels write SetRotateLabels default false;
  end;

  TSinCos = record
    FSin, FCos: Double;
  end;

  { TPolarSeries }

  TPolarSeries = class(TBasicPointSeries)
  strict private
    FBrush: TBrush;
    FCloseCircle: Boolean;
    FFilled: Boolean;
    FLinePen: TPen;
    FOriginX: Double;
    FOriginY: Double;
    FShowPoints: Boolean;
    function IsOriginXStored: Boolean;
    function IsOriginYStored: Boolean;
    procedure SetBrush(AValue: TBrush);
    procedure SetCloseCircle(AValue: Boolean);
    procedure SetFilled(AValue: Boolean);
    procedure SetLinePen(AValue: TPen);
    procedure SetOriginX(AValue: Double);
    procedure SetOriginY(AValue: Double);
    procedure SetShowPoints(AValue: Boolean);
  strict private
    FAngleCache: array of TSinCos;
    function GraphPoint(AIndex: Integer): TDoublePoint;
    procedure PrepareAngleCache;
    procedure PrepareGraphPoints;
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    procedure SourceChanged(ASender: TObject); override;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Draw(ADrawer: IChartDrawer); override;
    function Extent: TDoubleRect; override;
    function GetNearestPoint(
      const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
    procedure MovePoint(var AIndex: Integer; const ANewPos: TDoublePoint); override;
    procedure MovePointEx(var AIndex: Integer; AXIndex, AYIndex: Integer;
        const ANewPos: TDoublePoint); override;
  published
    property Brush: TBrush read FBrush write SetBrush;
    property CloseCircle: Boolean read FCloseCircle write SetCloseCircle default false;
    property Filled: Boolean read FFilled write SetFilled default false;
    property LinePen: TPen read FLinePen write SetLinePen;
    property OriginX: Double read FOriginX write SetOriginX stored IsOriginXStored;
    property OriginY: Double read FOriginY write SetOriginY stored IsOriginYStored;
    property Pointer;
    property ShowPoints: Boolean read FShowPoints write SetShowPoints;
    property Source;
    property OnCustomDrawPointer;
    property OnGetPointerStyle;
  end;

implementation

uses
  Math,
  TAChartStrConsts, TATypes, TACustomSource, TAGeometry, TAGraph;

{ TPieSlice }

function TPieSlice.Angle: Double;
begin
  Result := FNextAngle - FPrevAngle;
end;

function TPieSlice.CenterAngle: Double;
begin
  Result := (FNextAngle + FPrevAngle) / 2;
end;

{ TLegendItemPieS }

procedure TLegendItemPie.Draw(ADrawer: IChartDrawer; const ARect: TRect);
const
  INDEX_TO_ANGLE = 360 * 16 / Length(FColors);
var
  i: Integer;
begin
  inherited Draw(ADrawer, ARect);
  for i := 0 to High(FColors) do begin
    ADrawer.SetBrushColor(FColors[i]);
    with MakeSquare(ARect) do
      ADrawer.RadialPie(
        Left, Top, Right, Bottom,
        Round(i * INDEX_TO_ANGLE), Round(INDEX_TO_ANGLE));
  end;
end;

procedure TLegendItemPie.SetColors(AIndex: Integer; AValue: TChartColor);
begin
  FColors[AIndex] := AValue;
end;

{ TLegendItemPieSlice }

procedure TLegendItemPieSlice.Draw(ADrawer: IChartDrawer; const ARect: TRect);
const
  ANGLE = 30 * 16;
begin
  inherited Draw(ADrawer, ARect);
  ADrawer.SetBrushParams(bsSolid, ColorDef(Color, clRed));
  ADrawer.RadialPie(
    2 * ARect.Left - ARect.Right, ARect.Top, ARect.Right, ARect.Bottom,
    -ANGLE, 2 * ANGLE);
end;

{ TCustomPieSeries }

function TCustomPieSeries.AddPie(
  AValue: Double; AText: String; AColor: TColor): Integer;
begin
  Result := AddXY(GetXMaxVal + 1, AValue, AText, AColor);
end;

procedure TCustomPieSeries.Assign(ASource: TPersistent);
begin
  if ASource is TCustomPieSeries then
    with TCustomPieSeries(ASource) do begin
      Self.FExploded := FExploded;
      Self.FFixedRadius := FFixedRadius;
      Self.FRotateLabels := FRotateLabels;
    end;
  inherited Assign(ASource);
end;

constructor TCustomPieSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEdgePen := TPen.Create;
  FEdgePen.OnChange := @StyleChanged;
end;

destructor TCustomPieSeries.Destroy;
begin
  FreeAndNil(FEdgePen);
  inherited;
end;

procedure TCustomPieSeries.Draw(ADrawer: IChartDrawer);
const
  STEP = 4;
var
  prevLabelPoly: TPointArray = nil;
  ps: TPieSlice;
  i, numSteps: Integer;
  p: array of TPoint;
  a: Double;
  scaled_depth: Integer;
begin
  if IsEmpty then exit;

  Marks.SetAdditionalAngle(0);
  Measure(ADrawer);

  ADrawer.SetPen(EdgePen);
  if Depth > 0 then begin
    scaled_depth := ADrawer.Scale(Depth);
    for ps in FSlices do begin
      if not ps.FVisible then continue;
      ADrawer.SetBrushParams(bsSolid, SliceColor(ps.FOrigIndex));
      if not InRange(ps.FNextAngle, Pi / 4, 5 * Pi / 4) then
        ADrawer.DrawLineDepth(
          ps.FBase, ps.FBase + RotatePointX(FRadius, -ps.FNextAngle), scaled_depth);
      if InRange(ps.FPrevAngle, Pi / 4, 5 * Pi / 4) then
        ADrawer.DrawLineDepth(
          ps.FBase, ps.FBase + RotatePointX(FRadius, -ps.FPrevAngle), scaled_depth);
    end;
    for ps in FSlices do begin
      if not ps.FVisible then continue;
      ADrawer.SetBrushParams(bsSolid, SliceColor(ps.FOrigIndex));
      numSteps := Max(Round(2 * Pi * ps.Angle * FRadius / STEP), 2);
      SetLength(p, 2 * numSteps);
      for i := 0 to numSteps - 1 do begin
        a := WeightedAverage(ps.FPrevAngle, ps.FNextAngle, i / (numSteps - 1));
        p[i] := ps.FBase + RotatePointX(FRadius, -a);
        p[High(p) - i] := p[i] + Point(scaled_depth, -scaled_depth);
      end;
      ADrawer.Polygon(p, 0, Length(p));
    end;
  end;
  for ps in FSlices do begin
    if not ps.FVisible then continue;
    ADrawer.SetBrushParams(bsSolid, SliceColor(ps.FOrigIndex));
    ADrawer.RadialPie(
      ps.FBase.X - FRadius, ps.FBase.Y - FRadius,
      ps.FBase.X + FRadius, ps.FBase.Y + FRadius,
      RadToDeg16(ps.FPrevAngle), RadToDeg16(ps.Angle));
  end;
  if not Marks.IsMarkLabelsVisible then exit;
  for ps in FSlices do begin
    if not ps.FVisible then continue;
    with ps.FLabel do
      if FText <> '' then begin
        if RotateLabels then
          Marks.SetAdditionalAngle(ps.CenterAngle);
        Marks.DrawLabel(ADrawer, FAttachment, FCenter, FText, prevLabelPoly);
      end;
  end;
end;

function TCustomPieSeries.FindContainingSlice(const APoint: TPoint): Integer;
var
  c: TPoint;
  pointAngle: Double;
  ps: TPieSlice;
begin
  for ps in FSlices do begin
    if not ps.FVisible then continue;
    c := APoint - ps.FBase;
    pointAngle := ArcTan2(-c.Y, c.X);
    if pointAngle < 0 then
      pointAngle += 2 * Pi;
    if
      InRange(pointAngle, ps.FPrevAngle, ps.FNextAngle) and
      (Sqr(c.X) + Sqr(c.Y) <= Sqr(FRadius))
    then
      exit(ps.FOrigIndex);
  end;
  Result := -1;
end;

procedure TCustomPieSeries.GetLegendItems(AItems: TChartLegendItems);
var
  i: Integer;
  p: TLegendItemPie;
  ps: TLegendItemPieSlice;
begin
  case Legend.Multiplicity of
    lmSingle: begin
      p := TLegendItemPie.Create(LegendTextSingle);
      for i := 0 to 2 do
        p.Colors[i] := SliceColor(i);
      AItems.Add(p);
    end;
    lmPoint:
      for i := 0 to Count - 1 do begin
        ps := TLegendItemPieSlice.Create(LegendTextPoint(i));
        ps.Color := SliceColor(i);
        AItems.Add(ps);
      end;
  end;
end;

function TCustomPieSeries.GetNearestPoint(const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  idx: Integer;
begin
  Result := false;
  AResults.FDist := Sqr(AParams.FRadius) + 1;
  AResults.FIndex := -1;
  AResults.FXIndex := 0;
  AResults.FYIndex := 0;

  idx := FindContainingSlice(AParams.FPoint);
  if idx > -1 then begin
    AResults.FDist := 0;
    AResults.FIndex := idx;
    AResults.FImg := AParams.FPoint;
    Result := true;
  end;
end;

procedure TCustomPieSeries.Measure(ADrawer: IChartDrawer);
const
  MIN_RADIUS = 5;
var
  a, b: Integer;
begin
  FCenter := CenterPoint(ParentChart.ClipRect);
  if FixedRadius = 0 then begin
    // Use binary search to find maximum radius fitting into the parent chart.
    a := MIN_RADIUS;
    with Size(ParentChart.ClipRect) do
      b := Max(cx div 2, cy div 2);
    repeat
      FRadius := (a + b) div 2;
      if IsRectInRect(TryRadius(ADrawer), ParentChart.ClipRect) then
        a := FRadius
      else
        b := FRadius - 1;
    until a >= b - 1;
  end
  else begin
    FRadius := FixedRadius;
    TryRadius(ADrawer);
  end;
end;

procedure TCustomPieSeries.MovePointEx(var AIndex: Integer;
  AXIndex, AYIndex: Integer; const ANewPos: TDoublePoint);
const
  SENS = 25;  // empirical sensitivity factor for dragging
var
  idx: Integer;
  p: TPoint;
  vslice, vdrag: TDoublePoint;
  phi, dist: Double;
begin
  Unused(AIndex, AXIndex, AYIndex);

  if FExploded then begin
    p := ParentChart.GraphToImage(ANewPos);
    idx := FindContainingSlice(p);
    if idx > -1 then begin
      // Center angle of slice (runs in counter-clockwise direction)
      phi := FSlices[idx].CenterAngle;
      // Radial slice vector (unit length)
      vslice := DoublePoint(cos(phi), sin(phi));
      // Direction vector of dragging
      vdrag := ANewPos - ParentChart.ImageToGraph(FDragOrigin);
      // Projection of dragging vector onto slice vector (length)
      dist := DotProduct(vdrag, vslice);
      // Add to distance parameter in source (x)
      dist := Source.Item[idx]^.X + DotProduct(vdrag, vslice) / SENS;
      if dist < 0 then dist := 0;  // Don't let value go negative
      ListSource.BeginUpdate;
      try
        ListSource.SetXValue(idx, dist);
      finally
        ListSource.EndUpdate;
      end;
    end;
  end;
end;

procedure TCustomPieSeries.SetEdgePen(AValue: TPen);
begin
  if FEdgePen = AValue then exit;
  FEdgePen.Assign(AValue);
end;

procedure TCustomPieSeries.SetExploded(AValue: Boolean);
begin
  if FExploded = AValue then exit;
  FExploded := AValue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetFixedRadius(AValue: TChartDistance);
begin
  if FFixedRadius = AValue then exit;
  FFixedRadius := AValue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetMarkPositions(AValue: TPieMarkPositions);
begin
  if FMarkPositions = AValue then exit;
  FMarkPositions := AValue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetMarkDistancePercent(AValue: Boolean);
begin
  if FMarkDistancePercent = AValue then exit;
  FMarkDistancePercent := AVAlue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetRotateLabels(AValue: Boolean);
begin
  if FRotateLabels = AValue then exit;
  FRotateLabels := AValue;
  UpdateParentChart;
end;

function TCustomPieSeries.SliceColor(AIndex: Integer): TColor;
const
  SLICE_COLORS: array [0..14] of TColor = (
    clRed, clGreen, clYellow, clBlue, clWhite, clGray, clFuchsia,
    clTeal, clNavy, clMaroon, clLime, clOlive, clPurple, clSilver, clAqua);
begin
  if AIndex < Count then
    Result := Source[AIndex]^.Color
  else
    Result := clTAColor;
  Result := ColorDef(Result, SLICE_COLORS[AIndex mod Length(SLICE_COLORS)]);
end;

function TCustomPieSeries.TryRadius(ADrawer: IChartDrawer): TRect;

  function EndPoint(AAngle, ARadius: Double): TPoint;
  begin
    Result := RotatePointX(ARadius, -AAngle);
  end;

  function LabelExtraDist(APoly: TPointArray; AAngle: Double): Double;
  const
    ALMOST_INF = 1e100;
  var
    sa, ca: Extended;
    denom, t, tmin: Double;
    a, b, d: TPoint;
    i: Integer;
  begin
    // x = t * ca; y = t * sa
    // (t * ca - a.x) * dy = (t * sa - a.y) * dx
    // t * (ca * dy - sa * dx) = a.x * dy - a.y * dx
    SinCos(-Pi - AAngle, sa, ca);
    b := APoly[High(APoly)];
    tmin := ALMOST_INF;
    for i := 0 to High(APoly) do begin
      a := APoly[i];
      d := b - a;
      denom := ca * d.Y - sa * d.X;
      if denom <> 0 then begin
        t := (a.X * d.Y - a.Y * d.X) / denom;
        if t > 0 then
          tmin := Min(tmin, t);
      end;
      b := a;
    end;
    if tmin = ALMOST_INF then // Should never happen.
      Result := 0
    else
      Result := Norm([tmin * ca, tmin * sa]);
  end;

  procedure PrepareLabel(
    var ALabel: TLabelParams; AIndex: Integer; AAngle: Double);
  var
    i: Integer;
    p: TPointArray;

    function Ofs(AAngle: Double): TPoint;
    var
      d: Double;
    begin
      d := IfThen(FMarkDistancePercent, Marks.Distance * FRadius / 100, Marks.Distance);
      if not Marks.DistanceToCenter then
        d += LabelExtraDist(p, AAngle);
      Result := EndPoint(AAngle, d);
    end;

  begin
    with ALabel do begin
      FCenter := FAttachment;
      if not Marks.IsMarkLabelsVisible then exit;
        FText := FormattedMark(AIndex);
      if FText = '' then exit;
      if RotateLabels then
        Marks.SetAdditionalAngle(AAngle);
      p := Marks.GetLabelPolygon(ADrawer, ADrawer.TextExtent(FText));
      case MarkPositions of
        pmpAround:
          FCenter += Ofs(AAngle);
        pmpInside:
          FCenter -= Ofs(AAngle);
        pmpLeftRight:
          FCenter += Ofs(IfThen(InRange(AAngle, Pi / 2, 3 * Pi / 2), Pi, 0));
      end;
      for i := 0 to High(p) do
        ExpandRect(Result, p[i] + FCenter);
    end;
  end;

const
  MARGIN = 4;
var
  i, j: Integer;
  di: PChartDataItem;
  prevAngle: Double = 0;
  a, total: Double;
  scaled_depth: Integer;
begin
  Result.TopLeft := FCenter;
  Result.BottomRight := FCenter;
  scaled_depth := ADrawer.Scale(Depth);
  SetLength(FSlices, Count);
  j := 0;
  // This is a workaround for db source invalidating the cache due to
  // unnecessary "dataset changed" events.
  total := Source.ValuesTotal;
  if total = 0 then
    exit;
  for i := 0 to Count - 1 do begin
    di := Source[i];
    if IsNan(di^.Y) then continue;
    with FSlices[j] do begin
      FOrigIndex := i;
      FPrevAngle := prevAngle;
      FNextAngle := FPrevAngle + CycleToRad(di^.Y / total);
      FVisible := not IsNan(di^.X);
      if FVisible then begin
        FBase := FCenter;
        a := CenterAngle;
        if Exploded and (di^.X > 0) then
          FBase += EndPoint(a, FRadius * di^.X);
        ExpandRect(Result, FBase, FRadius, -FPrevAngle, -FNextAngle);
        if Depth > 0 then
          ExpandRect(
            Result, FBase + Point(scaled_depth, -scaled_depth),
            FRadius, -FPrevAngle, -FNextAngle);
        FLabel.FAttachment := EndPoint(a, FRadius) + FBase;
        PrepareLabel(FLabel, i, a);
      end;
      prevAngle := FNextAngle;
    end;
    j += 1;
  end;
  SetLength(FSlices, j);
  InflateRect(Result, MARGIN, MARGIN);
end;

{ TPolarSeries }

procedure TPolarSeries.Assign(ASource: TPersistent);
begin
  if ASource is TPolarSeries then
    with TPolarSeries(ASource) do begin
      Self.Brush := FBrush;
      Self.LinePen := FLinePen;
      Self.FOriginX := FOriginX;
      Self.FOriginY := FOriginY;
      Self.FShowPoints := FShowPoints;
    end;
  inherited Assign(ASource);
end;

constructor TPolarSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBrush := TBrush.Create;
  FBrush.OnChange := @StyleChanged;
  FLinePen := TPen.Create;
  FLinePen.OnChange := @StyleChanged;
  FPointer := TSeriesPointer.Create(FChart);
  FFilled := true;      // needed for SetFilled to execute its code
  FShowPoints := true;  // needed for SetShowPoints to execute its code
  SetFilled(false);
  SetShowPoints(false);
end;

destructor TPolarSeries.Destroy;
begin
  FreeAndNil(FBrush);
  FreeAndNil(FLinePen);
  inherited;
end;

procedure TPolarSeries.Draw(ADrawer: IChartDrawer);
var
  i: Integer;
  cnt: Integer = 0;
  pts: TPointArray;
  gp: TDoublePoint;
  firstPoint, lastPoint: TPoint;
  firstPointSet: Boolean = false;
  originPt: TPoint;
  fill: Boolean;

  procedure DoDraw;
  begin
    if cnt = 0 then
      exit;
    if fill then begin
      pts[cnt] := originPt;
      ADrawer.Brush := FBrush;
      ADrawer.SetPenParams(psClear, clBlack);
      ADrawer.Polygon(pts, 0, cnt + 1);
    end;
    ADrawer.Pen := LinePen;
    ADrawer.PolyLine(pts, 0, cnt);
  end;

begin
  PrepareGraphPoints;
  originPt := ParentChart.GraphToImage(DoublePoint(OriginX, OriginY));
  fill := FFilled and (FBrush.Style <> bsClear);
  SetLength(pts, Count + 1);  // +1 for origin
  for i := 0 to Count - 1 do begin
    gp := GraphPoint(i);
    if IsNaN(gp) then begin
      DoDraw;
      cnt := 0;
    end else begin
      lastPoint := FChart.GraphToImage(gp);
      pts[cnt] := lastPoint;
      cnt += 1;
      if not firstPointSet then begin
        firstPoint := lastPoint;
        firstPointSet := true;
      end;
    end;
  end;
  DoDraw;
  if firstPointSet and CloseCircle then begin
    SetLength(pts, 3);
    pts[0] := lastPoint;
    pts[1] := firstPoint;
    cnt := 2;
    DoDraw;
  end;

  DrawPointers(ADrawer);
end;

function TPolarSeries.Extent: TDoubleRect;
var
  i: Integer;
begin
  FindExtentInterval(DoubleRect(0, 0, 0, 0), false);
  PrepareAngleCache;
  Result := EmptyExtent;
  for i := 0 to Count - 1 do
    ExpandRect(Result, GraphPoint(i));
end;

procedure TPolarSeries.GetLegendItems(AItems: TChartLegendItems);
var
  p: TSeriesPointer;
begin
  if ShowPoints then
    p := Pointer
  else
    p := nil;
  AItems.Add(TLegendItemLinePointer.Create(LinePen, p, LegendTextSingle));
end;

function TPolarSeries.GetNearestPoint(const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  dist: Integer;
  gp: TDoublePoint;
  i: Integer;
begin
  Result := false;
  AResults.FDist := Sqr(AParams.FRadius) + 1;  // the dist func does not calc sqrt
  AResults.FIndex := -1;
  AResults.FXIndex := 0;
  AResults.FYIndex := 0;
  if Length(FAngleCache) = 0 then
    exit;

  dist := AResults.FDist;
  for i := 0 to Count - 1 do begin
    gp := GraphPoint(i);
    if IsNan(gp) then
      continue;
    // Find nearest point of datapoint at (x, y)
    if (nptPoint in AParams.FTargets) and (nptPoint in ToolTargets) then
    begin
      dist := Min(dist, ToolTargetDistance(AParams, gp, i, 0, 0));
    end;
    if dist >= AResults.FDist then
      continue;

    AResults.FDist := dist;
    AResults.FIndex := i;
    AResults.FValue := DoublePoint(gp.y*cos(gp.x), gp.y*sin(gp.x));
    AResults.FImg := ParentChart.GraphToImage(gp);
    if dist = 0 then break;
  end;
  Result := AResults.FIndex >= 0;
end;

function TPolarSeries.GraphPoint(AIndex: Integer): TDoublePoint;
begin
  with Source[AIndex]^, FAngleCache[AIndex] do
    Result := DoublePoint(Y * FCos + OriginX, Y * FSin + OriginY);
end;

function TPolarSeries.IsOriginXStored: Boolean;
begin
  Result := not SameValue(OriginX, 0.0);
end;

function TPolarSeries.IsOriginYStored: Boolean;
begin
  Result := not SameValue(OriginY, 0.0);
end;

{ ANewPos is in cartesioan coordinates. Convert to polar coordinates and store
 in ListSource }
procedure TPolarSeries.MovePoint(var AIndex: Integer;
  const ANewPos: TDoublePoint);
var
  p: TDoublePoint;
  r, phi: Double;
begin
  if not InRange(AIndex, 0, Count - 1) then exit;
  p := ANewPos - DoublePoint(OriginX, OriginY);
  r := Sqrt(sqr(p.x) + sqr(p.y));
  phi := arctan2(p.y, p.x);
  with ListSource do begin
    AIndex := SetXValue(AIndex, phi);
    SetYValue(AIndex, r);
  end;
end;

procedure TPolarSeries.MovePointEx(var AIndex: Integer;
  AXIndex, AYIndex: Integer; const ANewPos: TDoublePoint);
begin
  Unused(AXIndex, AYIndex);
  MovePoint(AIndex, ANewPos);
end;

procedure TPolarSeries.PrepareAngleCache;
var
  i: Integer;
  s, c: Extended;
begin
  if Length(FAngleCache) = Count then exit;
  SetLength(FAngleCache, Count);
  for i := 0 to Count - 1 do begin
    SinCos(Source[i]^.X, s, c);
    FAngleCache[i].FSin := s;
    FAngleCache[i].FCos := c;
  end;
end;

procedure TPolarSeries.PrepareGraphPoints;
var
  i: Integer;
begin
  SetLength(FGraphPoints, Count);
  for i := 0 to Count - 1 do
    FGraphPoints[i] := GraphPoint(i);
end;

procedure TPolarSeries.SetBrush(AValue: TBrush);
begin
  if FBrush = AValue then exit;
  FBrush.Assign(AValue);
  UpdateParentChart;
end;

procedure TPolarSeries.SetCloseCircle(AValue: Boolean);
begin
  if FCloseCircle = AValue then exit;
  FCloseCircle := AValue;
  UpdateParentChart;
end;

procedure TPolarSeries.SetFilled(AValue: Boolean);
begin
  if FFilled = AValue then exit;
  FFilled := AValue;
  UpdateParentChart;
end;

procedure TPolarSeries.SetLinePen(AValue: TPen);
begin
  if FLinePen = AValue then exit;
  FLinePen.Assign(AValue);
  UpdateParentChart;
end;

procedure TPolarSeries.SetOriginX(AValue: Double);
begin
  if SameValue(FOriginX, AValue) then exit;
  FOriginX := AValue;
  UpdateParentChart;
end;

procedure TPolarSeries.SetOriginY(AValue: Double);
begin
  if SameValue(FOriginY, AValue) then exit;
  FOriginY := AValue;
  UpdateParentChart;
end;

procedure TPolarSeries.SetShowPoints(AValue: Boolean);
begin
  if ShowPoints = AValue then exit;
  FShowPoints := AValue;
  Pointer.Visible := FShowPoints;;
  UpdateParentChart;
end;

procedure TPolarSeries.SourceChanged(ASender: TObject);
begin
  FAngleCache := nil;
  inherited;
end;


initialization

  RegisterSeriesClass(TPolarSeries, @rsPolarSeries);

end.

