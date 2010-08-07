{
 /***************************************************************************
                               TASeries.pas
                               ------------
                Component Library Standard Graph Series


 ***************************************************************************/

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

Authors: Luís Rodrigues, Philippe Martinole, Alexander Klenin

}

unit TASeries;

{$H+}

interface

uses
  Classes, Graphics,
  TAChartUtils, TADrawUtils, TACustomSeries, TAGraph, TALegend, TATypes;

const
  DEF_BAR_WIDTH_PERCENT = 70;

type
  EBarError = class(EChartError);

  TLabelDirection = (ldLeft, ldTop, ldRight, ldBottom);

  { TBasicPointSeries }

  TBasicPointSeries = class(TChartSeries)
  private
    procedure SetUseReticule(AValue: Boolean);

  protected
    FGraphPoints: array of TDoublePoint;
    FLoBound: Integer;
    FUpBound: Integer;
    FUseReticule: Boolean;

    procedure DrawLabels(ACanvas: TCanvas);
    function GetLabelDirection(AIndex: Integer): TLabelDirection; virtual;
    procedure PrepareGraphPoints(
      const AExtent: TDoubleRect; AFilterByExtent: Boolean);
    procedure UpdateMargins(ACanvas: TCanvas; var AMargins: TRect); override;
    property UseReticule: Boolean
      read FUseReticule write SetUseReticule default false;
  public
    function GetNearestPoint(
      ADistFunc: TPointDistFunc; const APoint: TPoint;
      out AIndex: Integer; out AImg: TPoint; out AValue: TDoublePoint): Boolean;
      override;
    procedure MovePoint(var AIndex: Integer; const ANewPos: TPoint); override;
  end;

  { TBarSeries }

  TBarSeries = class(TBasicPointSeries)
  private
    FBarBrush: TBrush;
    FBarPen: TPen;
    FBarWidthPercent: Integer;

    function CalcBarWidth(AX: Double; AIndex: Integer): Double;
    procedure SetBarBrush(Value: TBrush);
    procedure SetBarPen(Value: TPen);
    procedure SetBarWidthPercent(Value: Integer);
    procedure SetSeriesColor(AValue: TColor);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetSeriesColor: TColor; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Draw(ACanvas: TCanvas); override;
    function Extent: TDoubleRect; override;
  published
    property AxisIndexX;
    property AxisIndexY;
    property BarBrush: TBrush read FBarBrush write SetBarBrush;
    property BarPen: TPen read FBarPen write SetBarPen;
    property BarWidthPercent: Integer
      read FBarWidthPercent write SetBarWidthPercent default DEF_BAR_WIDTH_PERCENT;
    property Depth;
    property SeriesColor: TColor
      read GetSeriesColor write SetSeriesColor default clTAColor;
    property Source;
    property UseReticule;
  end;

  { TPieSeries }

  TPieSeries = class(TChartSeries)
  private
    FExploded: Boolean;
    procedure SetExploded(const AValue: Boolean);
    function SliceColor(AIndex: Integer): TColor;
  protected
    procedure AfterAdd; override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
  public
    function AddPie(Value: Double; Text: String; Color: TColor): Longint;
    procedure Draw(ACanvas: TCanvas); override;
  published
    // Offset slices away from center based on X value.
    property Exploded: Boolean read FExploded write SetExploded default false;
    property Source;
  end;

  { TAreaSeries }

  TAreaSeries = class(TBasicPointSeries)
  private
    FAreaBrush: TBrush;
    FAreaLinesPen: TPen;
    FInvertedStairs: Boolean;
    FStairs: Boolean;

    procedure SetAreaBrush(Value: TBrush);
    procedure SetInvertedStairs(Value: Boolean);
    procedure SetSeriesColor(AValue: TColor);
    procedure SetStairs(Value: Boolean);
  protected
    function GetLabelDirection(AIndex: Integer): TLabelDirection; override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetSeriesColor: TColor; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Draw(ACanvas: TCanvas); override;
  published
    property AxisIndexX;
    property AxisIndexY;
  published
    property AreaBrush: TBrush read FAreaBrush write SetAreaBrush;
    property AreaLinesPen: TPen read FAreaLinesPen write FAreaLinesPen;
    property Depth;
    property InvertedStairs: Boolean
      read FInvertedStairs write SetInvertedStairs default false;
    property SeriesColor: TColor
      read GetSeriesColor write SetSeriesColor default clWhite;
    property Source;
    property Stairs: Boolean read FStairs write SetStairs default false;
    property UseReticule;
  end;

  TSeriesPointerDrawEvent = procedure (
    ASender: TChartSeries; ACanvas: TCanvas; AIndex: Integer;
    ACenter: TPoint) of object;

  TLineType = (ltNone, ltFromPrevious, ltFromOrigin, ltStepXY, ltStepYX);

  { TLineSeries }

  TLineSeries = class(TBasicPointSeries)
  private
    FLinePen: TPen;
    FLineType: TLineType;
    FOnDrawPointer: TSeriesPointerDrawEvent;
    FPointer: TSeriesPointer;
    FShowPoints: Boolean;

    function GetShowLines: Boolean;
    procedure SetLinePen(AValue: TPen);
    procedure SetLineType(AValue: TLineType);
    procedure SetPointer(Value: TSeriesPointer);
    procedure SetSeriesColor(AValue: TColor);
    procedure SetShowLines(Value: Boolean);
    procedure SetShowPoints(Value: Boolean);
  protected
    procedure AfterAdd; override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetSeriesColor: TColor; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Draw(ACanvas: TCanvas); override;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property AxisIndexX;
    property AxisIndexY;
    property Depth;
    property LinePen: TPen read FLinePen write SetLinePen;
    property LineType: TLineType
      read FLineType write SetLineType default ltFromPrevious;
    property OnDrawPointer: TSeriesPointerDrawEvent
      read FOnDrawPointer write FOnDrawPointer;
    property Pointer: TSeriesPointer read FPointer write SetPointer;
    property SeriesColor: TColor
      read GetSeriesColor write SetSeriesColor default clTAColor;
    property ShowLines: Boolean
      read GetShowLines write SetShowLines stored false default true;
    property ShowPoints: Boolean
      read FShowPoints write SetShowPoints default false;
    property Source;
    property UseReticule default true;
  end;

  // 'TSerie' alias is for compatibility with older versions of TAChart.
  // Use TLineSeries instead.
  TSerie = TLineSeries deprecated;

  TLineStyle = (lsVertical, lsHorizontal);

  { TConstantLine }

  TConstantLine = class(TCustomChartSeries)
  private
    FLineStyle: TLineStyle;
    FPen: TPen;
    FPosGraph: Double; // Graph coordinate of line
    FUseBounds: Boolean;

    function GetSeriesColor: TColor;
    procedure SavePosToCoord(var APoint: TDoublePoint);
    procedure SetLineStyle(AValue: TLineStyle);
    procedure SetPen(AValue: TPen);
    procedure SetPos(AValue: Double);
    procedure SetSeriesColor(AValue: TColor);
    procedure SetUseBounds(AValue: Boolean);
  protected
    procedure GetBounds(var ABounds: TDoubleRect); override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Draw(ACanvas: TCanvas); override;
    function GetNearestPoint(
      ADistFunc: TPointDistFunc;
      const APoint: TPoint; out AIndex: Integer; out AImg: TPoint;
      out AValue: TDoublePoint): Boolean; override;
    procedure MovePoint(var AIndex: Integer; const ANewPos: TPoint); override;

  published
    property Active default true;
    property LineStyle: TLineStyle
      read FLineStyle write SetLineStyle default lsHorizontal;
    property Pen: TPen read FPen write SetPen;
    property Position: Double read FPosGraph write SetPos;
    property SeriesColor: TColor
      read GetSeriesColor write SetSeriesColor default clTAColor;
    property ShowInLegend;
    property Title;
    property UseBounds: Boolean read FUseBounds write SetUseBounds default true;
    property ZPosition;
  end;

  // 'TLine' alias is for compatibility with older versions of TAChart.
  // Use TConstantLine instead.
  TLine = class(TConstantLine) end deprecated;

  TFuncCalculateEvent = procedure (const AX: Double; out AY: Double) of object;

  TFuncSeriesStep = 1..MaxInt;

  { TFuncSeries }

  TFuncSeries = class(TCustomChartSeries)
  private
    FDomainExclusions: TIntervalList;
    FExtent: TChartExtent;
    FOnCalculate: TFuncCalculateEvent;
    FPen: TChartPen;
    FStep: TFuncSeriesStep;

    procedure SetExtent(const AValue: TChartExtent);
    procedure SetOnCalculate(const AValue: TFuncCalculateEvent);
    procedure SetPen(const AValue: TChartPen);
    procedure SetStep(AValue: TFuncSeriesStep);
  protected
    procedure GetBounds(var ABounds: TDoubleRect); override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Draw(ACanvas: TCanvas); override;
    function IsEmpty: Boolean; override;
  public
    property DomainExclusions: TIntervalList read FDomainExclusions;
  published
    property Active default true;
    property AxisIndexY;
    property Extent: TChartExtent read FExtent write SetExtent;
    property OnCalculate: TFuncCalculateEvent read FOnCalculate write SetOnCalculate;
    property Pen: TChartPen read FPen write SetPen;
    property ShowInLegend;
    property Step: TFuncSeriesStep read FStep write SetStep default 2;
    property Title;
    property ZPosition;
  end;

  TSeriesDrawEvent = procedure (ACanvas: TCanvas; const ARect: TRect) of object;
  TSeriesGetBoundsEvent = procedure (var ABounds: TDoubleRect) of object;

  { TUserDrawnSeries }

  TUserDrawnSeries = class(TCustomChartSeries)
  private
    FOnDraw: TSeriesDrawEvent;
    FOnGetBounds: TSeriesGetBoundsEvent;
    procedure SetOnDraw(AValue: TSeriesDrawEvent);
    procedure SetOnGetBounds(AValue: TSeriesGetBoundsEvent);
  protected
    procedure GetBounds(var ABounds: TDoubleRect); override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
  public
    procedure Draw(ACanvas: TCanvas); override;
    function IsEmpty: Boolean; override;
  published
    property Active default true;
    property ZPosition;
  published
    property OnDraw: TSeriesDrawEvent read FOnDraw write SetOnDraw;
    property OnGetBounds: TSeriesGetBoundsEvent
      read FOnGetBounds write SetOnGetBounds;
  end;

implementation

uses
  GraphMath, Math, PropEdits, SysUtils, Types;

{ TLineSeries }

procedure TLineSeries.AfterAdd;
begin
  inherited AfterAdd;
  FPointer.SetOwner(FChart);
end;

procedure TLineSeries.BeginUpdate;
begin
  ListSource.BeginUpdate;
end;

constructor TLineSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLinePen := TPen.Create;
  FLinePen.OnChange := @StyleChanged;
  FLineType := ltFromPrevious;
  FPointer := TSeriesPointer.Create(FChart);
  FUseReticule := true;
end;

destructor TLineSeries.Destroy;
begin
  FreeAndNil(FLinePen);
  FreeAndNil(FPointer);
  inherited;
end;

procedure TLineSeries.Draw(ACanvas: TCanvas);
var
  points: array of TPoint;
  pointCount: Integer = 0;
  breaks: TIntegerDynArray;
  breakCount: Integer = 0;

  procedure CacheLine(AA, AB: TDoublePoint);
  var
    ai, bi: TPoint;
  begin
    // This is not an optimization, but a safety check to avoid
    // integer overflow with extreme zoom-ins.
    if not LineIntersectsRect(AA, AB, ParentChart.CurrentExtent) then exit;
    ai := ParentChart.GraphToImage(AA);
    bi := ParentChart.GraphToImage(AB);
    if ai = bi then exit;
    if (pointCount = 0) or (points[pointCount - 1] <> ai) then begin
      breaks[breakCount] := pointCount;
      breakCount += 1;
      points[pointCount] := ai;
      pointCount += 1;
    end;
    points[pointCount] := bi;
    pointCount += 1;
  end;

  procedure DrawLines;
  var
    i, j: Integer;
    orig, m: TDoublePoint;
  begin
    if LineType = ltNone then exit;
    // For extremely long series (10000 points or more), the Canvas.Line
    // call becomes a bottleneck. So represent a serie as a sequence of polylines.
    // This achieves approximately 3x speedup for the typical case.
    SetLength(points, 2 * Length(FGraphPoints));
    SetLength(breaks, Length(FGraphPoints) + 1);
    case LineType of
      ltFromPrevious: begin
        for i := 0 to High(FGraphPoints) - 1 do
          CacheLine(FGraphPoints[i], FGraphPoints[i + 1]);
      end;
      ltFromOrigin: begin
        orig := AxisToGraph(ZeroDoublePoint);
        for i := 0 to High(FGraphPoints) do
          CacheLine(orig, FGraphPoints[i]);
      end;
      ltStepXY, ltStepYX: begin
        for i := 0 to High(FGraphPoints) - 1 do begin
          if (LineType = ltStepXY) xor IsRotated then
            m := DoublePoint(FGraphPoints[i + 1].X, FGraphPoints[i].Y)
          else
            m := DoublePoint(FGraphPoints[i].X, FGraphPoints[i + 1].Y);
          CacheLine(FGraphPoints[i], m);
          CacheLine(m, FGraphPoints[i + 1]);
        end;
      end;
    end;
    breaks[breakCount] := pointCount;
    breakCount += 1;
    SetLength(points, pointCount);
    SetLength(breaks, breakCount);

    ACanvas.Pen.Assign(LinePen);
    if Depth = 0 then
      for i := 0 to High(breaks) - 1 do
        ACanvas.Polyline(points, breaks[i], breaks[i + 1] - breaks[i])
    else begin
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := LinePen.Color;
      ACanvas.Pen.Color := clBlack;
      for i := 0 to High(breaks) - 1 do
        for j := breaks[i] to breaks[i + 1] - 2 do
          DrawLineDepth(ACanvas, points[j], points[j + 1], Depth);
    end;
  end;

var
  i: Integer;
  ai: TPoint;
  ext: TDoubleRect;
  p: TDoublePoint;
begin
  // Do not draw anything if the series extent does not intersect CurrentExtent.
  ext.a := AxisToGraph(Source.Extent.a);
  ext.b := AxisToGraph(Source.Extent.b);
  if LineType = ltFromOrigin then
    ExpandRect(ext, AxisToGraph(ZeroDoublePoint));
  if not RectIntersectsRect(ext, ParentChart.CurrentExtent) then exit;

  PrepareGraphPoints(ext, LineType <> ltFromOrigin);

  DrawLines;
  DrawLabels(ACanvas);

  if FShowPoints then
    for i := FLoBound to FUpBound do begin
      p := FGraphPoints[i - FLoBound];
      if not ParentChart.IsPointInViewPort(p) then continue;
      ai := ParentChart.GraphToImage(p);
      FPointer.Draw(ACanvas, ai, GetColor(i));
      if Assigned(FOnDrawPointer) then
        FOnDrawPointer(Self, ACanvas, i, ai);
    end;
end;

procedure TLineSeries.EndUpdate;
begin
  ListSource.EndUpdate;
  UpdateParentChart;
end;

procedure TLineSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemLine.Create(LinePen, Title));
end;

function TLineSeries.GetSeriesColor: TColor;
begin
  Result := FLinePen.Color;
end;

function TLineSeries.GetShowLines: Boolean;
begin
  Result := FLineType <> ltNone;
end;

procedure TLineSeries.SetLinePen(AValue: TPen);
begin
  FLinePen.Assign(AValue);
end;

procedure TLineSeries.SetLineType(AValue: TLineType);
begin
  if FLineType = AValue then exit;
  FLineType := AValue;
  UpdateParentChart;
end;

procedure TLineSeries.SetPointer(Value: TSeriesPointer);
begin
  FPointer.Assign(Value);
  UpdateParentChart;
end;

procedure TLineSeries.SetSeriesColor(AValue: TColor);
begin
  FLinePen.Color := AValue;
end;

procedure TLineSeries.SetShowLines(Value: Boolean);
begin
  if ShowLines = Value then exit;
  if Value then
    FLineType := ltFromPrevious
  else
    FLineType := ltNone;
  UpdateParentChart;
end;

procedure TLineSeries.SetShowPoints(Value: Boolean);
begin
  FShowPoints := Value;
  UpdateParentChart;
end;

{ TConstantLine }

constructor TConstantLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLineStyle := lsHorizontal;
  FPen := TPen.Create;
  FPen.OnChange := @StyleChanged;
  FUseBounds := true;
end;

destructor TConstantLine.Destroy;
begin
  FreeAndNil(FPen);
  inherited;
end;

procedure TConstantLine.Draw(ACanvas: TCanvas);
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Assign(FPen);

  with ParentChart do
    case LineStyle of
      lsHorizontal:
        DrawLineHoriz(ACanvas, YGraphToImage(FPosGraph));
      lsVertical:
        DrawLineVert(ACanvas, XGraphToImage(FPosGraph));
    end;
end;

procedure TConstantLine.GetBounds(var ABounds: TDoubleRect);
begin
  if not UseBounds then exit;
  SavePosToCoord(ABounds.a);
  SavePosToCoord(ABounds.b);
end;

procedure TConstantLine.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemLine.Create(Pen, Title));
end;

function TConstantLine.GetNearestPoint(ADistFunc: TPointDistFunc;
  const APoint: TPoint; out AIndex: Integer; out AImg: TPoint; out
  AValue: TDoublePoint): Boolean;
begin
  Unused(ADistFunc);
  Result := true;
  AIndex := -1;
  AImg := APoint;
  // Return the actual nearest point of the line.
  if LineStyle = lsVertical then begin
    AValue.Y := FChart.YImageToGraph(APoint.Y);
    AImg.X := FChart.XGraphToImage(Position);
  end
  else begin
    AValue.X := FChart.XImageToGraph(APoint.X);
    AImg.Y := FChart.YGraphToImage(Position);
  end;
  SavePosToCoord(AValue);
end;

function TConstantLine.GetSeriesColor: TColor;
begin
  Result := FPen.Color;
end;

procedure TConstantLine.MovePoint(var AIndex: Integer; const ANewPos: TPoint);
begin
  Unused(AIndex);
  if LineStyle = lsVertical then
    Position := FChart.XImageToGraph(ANewPos.X)
  else
    Position := FChart.YImageToGraph(ANewPos.Y);
end;

procedure TConstantLine.SavePosToCoord(var APoint: TDoublePoint);
begin
  if LineStyle = lsVertical then
    APoint.X := Position
  else
    APoint.Y := Position;
end;

procedure TConstantLine.SetLineStyle(AValue: TLineStyle);
begin
  if FLineStyle = AValue then exit;
  FLineStyle := AValue;
  UpdateParentChart;
end;

procedure TConstantLine.SetPen(AValue: TPen);
begin
  FPen.Assign(AValue);
end;

procedure TConstantLine.SetPos(AValue: Double);
begin
  if FPosGraph = AValue then exit;
  FPosGraph := AValue;
  UpdateParentChart;
end;

procedure TConstantLine.SetSeriesColor(AValue: TColor);
begin
  if FPen.Color = AValue then exit;
  FPen.Color := AValue;
end;

procedure TConstantLine.SetUseBounds(AValue: Boolean);
begin
  if FUseBounds = AValue then exit;
  FUseBounds := AValue;
  UpdateParentChart;
end;

{ TBasicPointSeries }

procedure TBasicPointSeries.DrawLabels(ACanvas: TCanvas);
var
  prevLabelPoly: TPointArray;

  procedure DrawLabel(
    const AText: String; const ADataPoint: TPoint; ADir: TLabelDirection);
  const
    OFFSETS: array [TLabelDirection] of TPoint =
      ((X: -1; Y: 0), (X: 0; Y: -1), (X: 1; Y: 0), (X: 0; Y: 1));
  var
    center: TPoint;
    sz: TSize;
  begin
    if AText = '' then exit;

    sz := Marks.MeasureLabel(ACanvas, AText);
    center := ADataPoint;
    center.X += OFFSETS[ADir].X * (Marks.Distance + sz.cx div 2);
    center.Y += OFFSETS[ADir].Y * (Marks.Distance + sz.cy div 2);
    Marks.DrawLabel(ACanvas, ADataPoint, center, AText, prevLabelPoly);
  end;

var
  g: TDoublePoint;
  i: Integer;
begin
  if not Marks.IsMarkLabelsVisible then exit;
  for i := 0 to Count - 1 do begin
    g := GetGraphPoint(i);
    with ParentChart do
      if IsPointInViewPort(g) then
        DrawLabel(FormattedMark(i), GraphToImage(g), GetLabelDirection(i));
  end;
end;

function TBasicPointSeries.GetLabelDirection(AIndex: Integer): TLabelDirection;
const
  DIR: array [Boolean, Boolean] of TLabelDirection =
    ((ldTop, ldBottom), (ldRight, ldLeft));
begin
  Result := DIR[IsRotated, GetGraphPointY(AIndex) < 0];
end;

function TBasicPointSeries.GetNearestPoint(
  ADistFunc: TPointDistFunc; const APoint: TPoint;
  out AIndex: Integer; out AImg: TPoint; out AValue: TDoublePoint): Boolean;
var
  dist, minDist, i: Integer;
  pt: TPoint;
begin
  Result := UseReticule and (Count > 0);
  minDist := MaxInt;
  for i := 0 to Count - 1 do begin
    pt := Point(GetXImgValue(i), GetYImgValue(i));
    dist := ADistFunc(APoint, pt);
    if dist >= minDist then
      Continue;
    minDist := dist;
    AIndex := i;
    AImg := pt;
    AValue.X := GetXValue(i);
    AValue.Y := GetYValue(i);
  end;
end;

procedure TBasicPointSeries.MovePoint(
  var AIndex: Integer; const ANewPos: TPoint);
var
  p: TDoublePoint;
begin
  if not InRange(AIndex, 0, Count - 1) then exit;
  p := FChart.ImageToGraph(ANewPos);
  with ListSource do begin
    AIndex := SetXValue(AIndex, p.X);
    SetYValue(AIndex, p.Y);
  end;
end;

procedure TBasicPointSeries.PrepareGraphPoints(
  const AExtent: TDoubleRect; AFilterByExtent: Boolean);
var
  axisExtent: TDoubleInterval;
  i: Integer;
begin
  // Find an interval of x-values intersecting the extent.
  // Requires monotonic (but not necessarily increasing) axis transformation.
  FLoBound := 0;
  FUpBound := Count - 1;
  if AFilterByExtent then begin
    with AExtent do
      if IsRotated then
        axisExtent := DoubleInterval(GraphToAxisY(a.Y), GraphToAxisY(b.Y))
      else
        axisExtent := DoubleInterval(GraphToAxisX(a.X), GraphToAxisX(b.X));
    Source.FindBounds(axisExtent.FStart, axisExtent.FEnd, FLoBound, FUpBound);
    FLoBound := Max(FLoBound - 1, 0);
    FUpBound := Min(FUpBound + 1, Count - 1);
  end;

  SetLength(FGraphPoints, FUpBound - FLoBound + 1);
  if (AxisIndexX < 0) and (AxisIndexY < 0) then
    // Optimization: bypass transformations in the default case.
    for i := FLoBound to FUpBound do
      with Source[i]^ do
        FGraphPoints[i - FLoBound] := DoublePoint(X, Y)
  else
    for i := FLoBound to FUpBound do
      FGraphPoints[i - FLoBound] := GetGraphPoint(i);
end;

procedure TBasicPointSeries.SetUseReticule(AValue: Boolean);
begin
  if FUseReticule = AValue then exit;
  FUseReticule := AValue;
  UpdateParentChart;
end;

procedure TBasicPointSeries.UpdateMargins(ACanvas: TCanvas; var AMargins: TRect);
const
  LABEL_TO_BORDER = 4;
var
  i, d: Integer;
  labelText: String;
  dir: TLabelDirection;
  m: array [TLabelDirection] of Integer absolute AMargins;
begin
  if not Marks.IsMarkLabelsVisible then exit;

  for i := 0 to Count - 1 do begin
    if not ParentChart.IsPointInViewPort(GetGraphPoint(i)) then continue;
    labelText := FormattedMark(i);
    if labelText = '' then continue;

    dir := GetLabelDirection(i);
    with Marks.MeasureLabel(ACanvas, labelText) do
      d := IfThen(dir in [ldLeft, ldRight], cx, cy);
    m[dir] := Max(m[dir], d + Marks.Distance + LABEL_TO_BORDER);
  end;
end;

{ TBarSeries }

function TBarSeries.CalcBarWidth(AX: Double; AIndex: Integer): Double;
begin
  case CASE_OF_TWO[AIndex > 0, AIndex < Count - 1] of
    cotNone: Result := 1.0;
    cotFirst: Result := Abs(AX - GetGraphPointX(AIndex - 1));
    cotSecond: Result := Abs(AX - GetGraphPointX(AIndex + 1));
    cotBoth: Result := Min(
      Abs(AX - GetGraphPointX(AIndex - 1)),
      Abs(AX - GetGraphPointX(AIndex + 1)));
  end;
  Result *= FBarWidthPercent * PERCENT / 2;
end;

constructor TBarSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBarWidthPercent := DEF_BAR_WIDTH_PERCENT;

  FBarBrush := TBrush.Create;
  FBarBrush.OnChange := @StyleChanged;

  FBarPen := TPen.Create;
  FBarPen.OnChange := @StyleChanged;
  FBarPen.Color := clBlack;
  FBarBrush.Color := clRed;
end;

destructor TBarSeries.Destroy;
begin
  FreeAndNil(FBarPen);
  FreeAndNil(FBarBrush);
  inherited;
end;

procedure TBarSeries.Draw(ACanvas: TCanvas);

  procedure DrawBar(const AR: TRect);
  var
    sz: TSize;
  begin
    sz := Size(AR);
    if (sz.cx > 2) and (sz.cy > 2) then
      ACanvas.Pen.Assign(BarPen)
    else begin
      // Bars are too small to distinguish border from interior.
      ACanvas.Pen.Color := ACanvas.Brush.Color;
      ACanvas.Pen.Style := psSolid;
    end;

    ACanvas.Rectangle(AR);

    if Depth = 0 then exit;
    DrawLineDepth(ACanvas, AR.Left, AR.Top, AR.Right - 1, AR.Top, Depth);
    DrawLineDepth(
      ACanvas, AR.Right - 1, AR.Top, AR.Right - 1, AR.Bottom - 1, Depth);
  end;

var
  i: Integer;
  ext2, graphBar: TDoubleRect;
  imageBar: TRect;
  w: Double;
  p: TDoublePoint;
begin
  if IsEmpty then exit;

  ext2 := ParentChart.CurrentExtent;
  ExpandRange(ext2.a.X, ext2.b.X, 1.0);
  ExpandRange(ext2.a.Y, ext2.b.Y, 1.0);

  ACanvas.Brush.Assign(BarBrush);
  for i := 0 to Count - 1 do begin
    p := GetGraphPoint(i);
    w := CalcBarWidth(GetGraphPointX(i), i);
    if IsRotated then
      graphBar := DoubleRect(AxisToGraphX(0), p.Y - w, p.X, p.Y + w)
    else
      graphBar := DoubleRect(p.X - w, AxisToGraphY(0), p.X + w, p.Y);
    if not RectIntersectsRect(graphBar, ext2) then continue;

    with imageBar do begin
      TopLeft := ParentChart.GraphToImage(graphBar.a);
      BottomRight := ParentChart.GraphToImage(graphBar.b);
      NormalizeRect(imageBar);

      // Draw a line instead of an empty rectangle.
      if Bottom = Top then Dec(Top);
      if Left = Right then Inc(Right);
    end;
    ACanvas.Brush.Color := GetColor(i);
    DrawBar(imageBar);
  end;

  DrawLabels(ACanvas);
end;

function TBarSeries.Extent: TDoubleRect;
var
  x: Double;
begin
  Result := inherited Extent;
  if IsEmpty then exit;
  UpdateMinMax(0, Result.a.Y, Result.b.Y);
  // Show first and last bars fully.
  x := GetGraphPointX(0);
  Result.a.X := Min(Result.a.X, x - CalcBarWidth(x, 0));
  x := GetGraphPointX(Count - 1);
  Result.b.X := Max(Result.b.X, x + CalcBarWidth(x, Count - 1));
end;

procedure TBarSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemBrushRect.Create(BarBrush, Title));
end;

function TBarSeries.GetSeriesColor: TColor;
begin
  Result := FBarBrush.Color;
end;

procedure TBarSeries.SetBarBrush(Value: TBrush);
begin
  FBarBrush.Assign(Value);
end;

procedure TBarSeries.SetBarPen(Value:TPen);
begin
  FBarPen.Assign(Value);
end;

procedure TBarSeries.SetBarWidthPercent(Value: Integer);
begin
  if (Value < 1) or (Value > 100) then
    raise EBarError.Create('Wrong BarWidth Percent');
  FBarWidthPercent := Value;
end;

procedure TBarSeries.SetSeriesColor(AValue: TColor);
begin
  FBarBrush.Color := AValue;
end;

{ TPieSeries }

function TPieSeries.AddPie(Value: Double; Text: String; Color: TColor): Longint;
begin
  Result := AddXY(GetXMaxVal + 1, Value, Text, Color);
end;

procedure TPieSeries.AfterAdd;
begin
  // disable axis when we have TPie series
  ParentChart.LeftAxis.Visible := false;
  ParentChart.BottomAxis.Visible := false;
end;

procedure TPieSeries.Draw(ACanvas: TCanvas);
var
  labelWidths, labelHeights: TIntegerDynArray;
  labelTexts: TStringDynArray;

  procedure Measure(out ACenter: TPoint; out ARadius: Integer);
  const
    MARGIN = 8;
  var
    i: Integer;
  begin
    SetLength(labelWidths, Count);
    SetLength(labelHeights, Count);
    SetLength(labelTexts, Count);
    for i := 0 to Count - 1 do begin
      labelTexts[i] := FormattedMark(i);
      with Marks.MeasureLabel(ACanvas, labelTexts[i]) do begin
        labelWidths[i] := cx;
        labelHeights[i] := cy;
      end;
    end;

    with ParentChart do begin
      ACenter := CenterPoint(ClipRect);
      // Reserve space for labels.
      ARadius := Min(
        ClipRect.Right - ACenter.x - MaxIntValue(labelWidths),
        ClipRect.Bottom - ACenter.y - MaxIntValue(labelHeights));
    end;
    if Marks.IsMarkLabelsVisible then
      ARadius -= Marks.Distance;
    ARadius := Max(ARadius - MARGIN, 0);
    if Exploded then
      ARadius := Trunc(ARadius / (Max(Source.Extent.b.X, 0) + 1));
  end;

var
  i, radius: Integer;
  prevAngle: Double = 0;
  d, angleStep, sliceCenterAngle: Double;
  c, center: TPoint;
  sa, ca: Extended;
  prevLabelPoly: TPointArray = nil;
const
  RAD_TO_DEG16 = 360 * 16;
begin
  if IsEmpty then exit;

  Measure(center, radius);
  for i := 0 to Count - 1 do begin
    ACanvas.Pen.Color := clBlack;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := SliceColor(i);

    with Source[i]^ do begin
      angleStep := Y / Source.ValuesTotal * RAD_TO_DEG16;
      sliceCenterAngle := prevAngle + angleStep / 2;
      if Exploded and (X > 0) then
        c := LineEndPoint(center, sliceCenterAngle, radius * X)
      else
        c := center;
    end;
    ACanvas.RadialPie(
      c.x - radius, c.y - radius, c.x + radius, c.y + radius,
      round(prevAngle), round(angleStep));

    prevAngle += angleStep;

    if not Marks.IsMarkLabelsVisible then continue;

    // This is a crude approximation of label "radius", it may be improved.
    SinCos(DegToRad(sliceCenterAngle / 16), sa, ca);
    d := Max(Abs(labelWidths[i] * ca), Abs(labelHeights[i] * sa)) / 2;
    Marks.DrawLabel(
      ACanvas,
      LineEndPoint(c, sliceCenterAngle, radius),
      LineEndPoint(c, sliceCenterAngle, radius + Marks.Distance + d),
      labelTexts[i], prevLabelPoly);
  end;
end;

procedure TPieSeries.GetLegendItems(AItems: TChartLegendItems);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    AItems.Add(TLegendItemColorRect.Create(SliceColor(i), FormattedMark(i)));
end;

procedure TPieSeries.SetExploded(const AValue: Boolean);
begin
  if FExploded = AValue then exit;
  FExploded := AValue;
  UpdateParentChart;
end;

function TPieSeries.SliceColor(AIndex: Integer): TColor;
begin
  Result :=
    ColorOrDefault(Source[AIndex]^.Color, Colors[AIndex mod High(Colors) + 1]);
end;

{ TAreaSeries }

constructor TAreaSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAreaLinesPen := TPen.Create;
  FAreaLinesPen.OnChange := @StyleChanged;

  FAreaBrush := TBrush.Create;
  FAreaBrush.OnChange := @StyleChanged;
end;

destructor TAreaSeries.Destroy;
begin
  FreeAndNil(FAreaLinesPen);
  FreeAndNil(FAreaBrush);
  inherited;
end;

procedure TAreaSeries.Draw(ACanvas: TCanvas);
var
  pts: array [0..4] of TPoint;
  numPts: Integer;

  procedure PushPoint(const A: TPoint);
  begin
    pts[numPts] := A;
    Inc(numPts);
  end;

var
  i, ax, bx, ymin: Integer;
  a, b: TDoublePoint;
  ext, ext2: TDoubleRect;
  imageRect: TRect;
begin
  if Count = 0 then exit;

  ACanvas.Brush.Assign(AreaBrush);
  ACanvas.Pen.Assign(AreaLinesPen);

  ext := ParentChart.CurrentExtent;
  ext2 := ext;
  ExpandRange(ext2.a.X, ext2.b.X, 0.1);
  ExpandRange(ext2.a.Y, ext2.b.Y, 1.0);

  ymin := ParentChart.ClipRect.Bottom - 1;

  for i := 0 to Count - 2 do begin
    a := GetGraphPoint(i);
    b := GetGraphPoint(i + 1);
    if a.X > b.X then
      Exchange(a, b);
    if (a.X > ext.b.X) or (b.X < ext.a.X) then continue;
    if Stairs then begin
      if InvertedStairs then
        a.Y := b.Y
      else
        b.Y := a.Y;
    end;
    ax := ParentChart.XGraphToImage(Max(a.X, ext2.a.X));
    bx := ParentChart.XGraphToImage(Min(b.X, ext2.b.X));

    if LineIntersectsRect(a, b, ext2) then begin
      numPts := 0;
      PushPoint(Point(ax, ymin));
      if a.Y = ext2.b.Y then
        PushPoint(Point(ax, ParentChart.YGraphToImage(a.Y)));
      PushPoint(ParentChart.GraphToImage(a));
      PushPoint(ParentChart.GraphToImage(b));
      if b.Y = ext2.b.Y then
        PushPoint(Point(bx, ParentChart.YGraphToImage(b.Y)));
      PushPoint(Point(bx, ymin));
      ACanvas.Polygon(pts, false, 0, numPts);
    end
    else begin
      if a.Y > ext.b.Y then begin
        imageRect := Rect(ax, ParentChart.ClipRect.Top - 1, bx, ymin);
        NormalizeRect(imageRect);
        ACanvas.Rectangle(imageRect);
      end;
    end;
  end;
  DrawLabels(ACanvas);
end;

function TAreaSeries.GetLabelDirection(AIndex: Integer): TLabelDirection;
const
  DIR: array [Boolean] of TLabelDirection = (ldTop, ldRight);
begin
  Unused(AIndex);
  Result := DIR[IsRotated];
end;

procedure TAreaSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemBrushRect.Create(AreaBrush, Title));
end;

function TAreaSeries.GetSeriesColor: TColor;
begin
  Result := FAreaBrush.Color;
end;

procedure TAreaSeries.SetAreaBrush(Value: TBrush);
begin
  FAreaBrush.Assign(Value);
  UpdateParentChart;
end;

procedure TAreaSeries.SetInvertedStairs(Value: Boolean);
begin
  FInvertedStairs := Value;
  UpdateParentChart;
end;

procedure TAreaSeries.SetSeriesColor(AValue: TColor);
begin
  FAreaBrush.Color := AValue;
end;

procedure TAreaSeries.SetStairs(Value: Boolean);
begin
  FStairs := Value;
  UpdateParentChart;
end;

{ TFuncSeries }

constructor TFuncSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExtent := TChartExtent.Create(FChart);
  FDomainExclusions := TIntervalList.Create;
  FDomainExclusions.OnChange := @StyleChanged;
  FPen := TChartPen.Create;
  FPen.OnChange := @StyleChanged;
  FStep := 2;
end;

destructor TFuncSeries.Destroy;
begin
  FreeAndNil(FExtent);
  FreeAndNil(FDomainExclusions);
  FreeAndNil(FPen);
  inherited;
end;

procedure TFuncSeries.Draw(ACanvas: TCanvas);
var
  ygMin, ygMax: Double;

  function CalcY(AXg: Double): Integer;
  var
    yg: Double;
  begin
    OnCalculate(AXg, yg);
    Result := FChart.YGraphToImage(EnsureRange(AxisToGraphY(yg), ygMin, ygMax));
  end;

var
  x, xmax, hint: Integer;
  xg, xg1: Double;
begin
  if not Assigned(OnCalculate) then exit;

  x := FChart.ClipRect.Left;
  if Extent.UseXMin then
    x := Max(FChart.XGraphToImage(Extent.XMin), x);
  xmax := FChart.ClipRect.Right;
  if Extent.UseXMax then
    xmax := Min(FChart.XGraphToImage(Extent.XMax), xmax);

  ygMin := FChart.CurrentExtent.a.Y;
  if Extent.UseYMin and (ygMin < Extent.YMin) then
    ygMin := Extent.YMin;
  ygMax := FChart.CurrentExtent.b.Y;
  if Extent.UseYMax and (ygMax < Extent.YMax) then
    ygMax := Extent.YMax;
  ExpandRange(ygMin, ygMax, 1);

  hint := 0;
  xg := FChart.XImageToGraph(x);
  if DomainExclusions.Intersect(xg, xg, hint) then
    x := FChart.XGraphToImage(xg);
  ACanvas.MoveTo(x, CalcY(xg));

  ACanvas.Pen.Assign(Pen);
  while x < xmax do begin
    Inc(x, FStep);
    xg1 := FChart.XImageToGraph(x);
    if DomainExclusions.Intersect(xg, xg1, hint) then begin
      ACanvas.LineTo(FChart.XGraphToImage(xg), CalcY(xg));
      x := FChart.XGraphToImage(xg1);
      ACanvas.MoveTo(x, CalcY(xg1));
    end
    else
      ACanvas.LineTo(x, CalcY(xg1));
    xg := xg1;
  end;
end;

procedure TFuncSeries.GetBounds(var ABounds: TDoubleRect);
begin
  with Extent do begin
    if UseXMin then ABounds.a.X := XMin;
    if UseYMin then ABounds.a.Y := YMin;
    if UseXMax then ABounds.b.X := XMax;
    if UseYMax then ABounds.b.Y := YMax;
  end;
end;

procedure TFuncSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemLine.Create(Pen, Title));
end;

function TFuncSeries.IsEmpty: Boolean;
begin
  Result := not Assigned(OnCalculate);
end;

procedure TFuncSeries.SetExtent(const AValue: TChartExtent);
begin
  if FExtent = AValue then exit;
  FExtent.Assign(AValue);
  UpdateParentChart;
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

{ TUserDrawnSeries }

procedure TUserDrawnSeries.Draw(ACanvas: TCanvas);
begin
  if Assigned(FOnDraw) then
     FOnDraw(ACanvas, FChart.ClipRect);
end;

procedure TUserDrawnSeries.GetBounds(var ABounds: TDoubleRect);
begin
  if Assigned(FOnGetBounds) then
    FOnGetBounds(ABounds);
end;

procedure TUserDrawnSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  Unused(AItems);
end;

function TUserDrawnSeries.IsEmpty: Boolean;
begin
  Result := not Assigned(FOnDraw);
end;

procedure TUserDrawnSeries.SetOnDraw(AValue: TSeriesDrawEvent);
begin
  if TMethod(FOnDraw) = TMethod(AValue) then exit;
  FOnDraw := AValue;
  UpdateParentChart;
end;

procedure TUserDrawnSeries.SetOnGetBounds(AValue: TSeriesGetBoundsEvent);
begin
  if TMethod(FOnGetBounds) = TMethod(AValue) then exit;
  FOnGetBounds := AValue;
  UpdateParentChart;
end;

initialization
  RegisterSeriesClass(TLineSeries, 'Line series');
  RegisterSeriesClass(TAreaSeries, 'Area series');
  RegisterSeriesClass(TBarSeries, 'Bar series');
  RegisterSeriesClass(TPieSeries, 'Pie series');
  RegisterSeriesClass(TFuncSeries, 'Function series');
  RegisterSeriesClass(TUserDrawnSeries, 'User-drawn series');
  RegisterSeriesClass(TConstantLine, 'Constant line');
  {$WARNINGS OFF}RegisterSeriesClass(TLine, '');{$WARNINGS ON}
  RegisterPropertyEditor(
    TypeInfo(Boolean), TLineSeries, 'ShowLines', THiddenPropertyEditor);

end.
