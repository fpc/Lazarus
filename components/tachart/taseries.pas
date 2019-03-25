{
 /***************************************************************************
                               TASeries.pas
                               ------------
                Component Library Standard Graph Series


 ***************************************************************************/

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Luís Rodrigues, Philippe Martinole, Alexander Klenin

}

unit TASeries;

{$H+}

interface

uses
  Classes, Graphics, Types,
  TAChartUtils, TADrawUtils, TACustomSeries, TALegend, TARadialSeries, TATypes;

const
  DEF_BAR_WIDTH_PERCENT = 70;

type
  EBarError = class(EChartError);

  TBarShape = (bsRectangular, bsCylindrical, bsHexPrism, bsPyramid, bsConical);

  TBarWidthStyle = (bwPercent, bwPercentMin);

  TBarSeries = class;

  TBeforeDrawBarEvent = procedure (
    ASender: TBarSeries; ACanvas: TCanvas; const ARect: TRect;
    APointIndex, AStackIndex: Integer; var ADoDefaultDrawing: Boolean
  ) of object; deprecated;

  TCustomDrawBarEvent = procedure (
    ASeries: TBarSeries; ADrawer: IChartDrawer; const ARect: TRect;
    APointIndex, AStackIndex: Integer) of object;

  { TBarSeries }

  TBarSeries = class(TBasicPointSeries)
  private
    FBarBrush: TBrush;
    FBarOffsetPercent: Integer;
    FBarPen: TPen;
    FBarShape: TBarShape;
    FBarWidthPercent: Integer;
    FBarWidthStyle: TBarWidthStyle;
    FOnBeforeDrawBar: TBeforeDrawBarEvent;
    FOnCustomDrawBar: TCustomDrawBarEvent;
    FUseZeroLevel: Boolean;
    FZeroLevel: Double;

    function IsZeroLevelStored: boolean;
    procedure SetBarBrush(Value: TBrush);
    procedure SetBarOffsetPercent(AValue: Integer);
    procedure SetBarPen(Value: TPen);
    procedure SetBarShape(AValue: TBarShape);
    procedure SetBarWidthPercent(Value: Integer);
    procedure SetBarWidthStyle(AValue: TBarWidthStyle);
    procedure SetOnBeforeDrawBar(AValue: TBeforeDrawBarEvent);
    procedure SetOnCustomDrawBar(AValue: TCustomDrawBarEvent);
    procedure SetSeriesColor(AValue: TColor);
    procedure SetUseZeroLevel(AValue: Boolean);
    procedure SetZeroLevel(AValue: Double);
  strict protected
    function GetLabelDataPoint(AIndex, AYIndex: Integer): TDoublePoint; override;
    function ToolTargetDistance(const AParams: TNearestPointParams;
      AGraphPt: TDoublePoint; APointIdx, AXIdx, AYIdx: Integer): Integer; override;
  protected
    procedure BarOffsetWidth(
      AX: Double; AIndex: Integer; out AOffset, AWidth: Double);
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetSeriesColor: TColor; override;
    function GetZeroLevel: Double; override;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Draw(ADrawer: IChartDrawer); override;
    function Extent: TDoubleRect; override;
    function GetBarWidth(AIndex: Integer): Integer;
    function GetNearestPoint(const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
  published
    property AxisIndexX;
    property AxisIndexY;
    property BarBrush: TBrush read FBarBrush write SetBarBrush;
    property BarOffsetPercent: Integer
      read FBarOffsetPercent write SetBarOffsetPercent default 0;
    property BarPen: TPen read FBarPen write SetBarPen;
    property BarShape: TBarShape read FBarshape write SetBarShape default bsRectangular;
    property BarWidthPercent: Integer
      read FBarWidthPercent write SetBarWidthPercent default DEF_BAR_WIDTH_PERCENT;
    property BarWidthStyle: TBarWidthStyle
      read FBarWidthStyle write SetBarWidthStyle default bwPercent;
    property Depth;
    property DepthBrightnessDelta;
    property MarkPositionCentered;
    property MarkPositions;
    property Marks;
    property SeriesColor: TColor
      read GetSeriesColor write SetSeriesColor stored false default clRed;
    property Source;
    property Stacked default true;
    property StackedNaN;
    property Styles;
    property ToolTargets default [nptPoint, nptYList, nptCustom];
    property UseZeroLevel: Boolean
      read FUseZeroLevel write SetUseZeroLevel default true;
    property ZeroLevel: Double
      read FZeroLevel write SetZeroLevel stored IsZeroLevelStored;
  published
    property OnBeforeDrawBar: TBeforeDrawBarEvent
      read FOnBeforeDrawBar write SetOnBeforeDrawBar; deprecated 'Use OnCustomDrawBar instead';
    property OnCustomDrawBar: TCustomDrawBarEvent
      read FOnCustomDrawBar write SetOnCustomDrawBar;
  end;


  { TPieSeries }

  TPieSeries = class(TCustomPieSeries)
  public
    property Radius;
  published
    property EdgePen;
    property Depth;
    property DepthBrightnessDelta;
    property Exploded;
    property FixedRadius;
    property InnerRadiusPercent;
    property MarkDistancePercent;
    property MarkPositionCentered;
    property MarkPositions;
    property Marks;
    property Orientation;
    property RotateLabels;
    property StartAngle;
    property Source;
    property ViewAngle;
    property OnCustomDrawPie;
  end;

  TConnectType = (ctLine, ctStepXY, ctStepYX);

  { TAreaSeries }

  TAreaSeries = class(TBasicPointSeries)
  private
    FAreaBrush: TBrush;
    FAreaContourPen: TPen;
    FAreaLinesPen: TPen;
    FBanded: Boolean;
    FConnectType: TConnectType;
    FUseZeroLevel: Boolean;
    FZeroLevel: Double;

    function IsZeroLevelStored: boolean;
    procedure SetAreaBrush(AValue: TBrush);
    procedure SetAreaContourPen(AValue: TPen);
    procedure SetAreaLinesPen(AValue: TPen);
    procedure SetBanded(AValue: Boolean);
    procedure SetConnectType(AValue: TConnectType);
    procedure SetSeriesColor(AValue: TColor);
    procedure SetUseZeroLevel(AValue: Boolean);
    procedure SetZeroLevel(AValue: Double);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetSeriesColor: TColor; override;
    function GetZeroLevel: Double; override;
    function SkipMissingValues(AIndex: Integer): Boolean; override;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(ADrawer: IChartDrawer); override;
    function Extent: TDoubleRect; override;
  published
    property AxisIndexX;
    property AxisIndexY;
  published
    property AreaBrush: TBrush read FAreaBrush write SetAreaBrush;
    property AreaContourPen: TPen read FAreaContourPen write SetAreaContourPen;
    property AreaLinesPen: TPen read FAreaLinesPen write SetAreaLinesPen;
    property Banded: Boolean read FBanded write SetBanded default false;
    property ConnectType: TConnectType
      read FConnectType write SetConnectType default ctLine;
    property Depth;
    property DepthBrightnessDelta;
    property MarkPositionCentered;
    property MarkPositions;
    property Marks;
    property SeriesColor: TColor
      read GetSeriesColor write SetSeriesColor stored false default clWhite;
    property Source;
    property Stacked default true;
    property StackedNaN;
    property Styles;
    property ToolTargets;
    property UseZeroLevel: Boolean
      read FUseZeroLevel write SetUseZeroLevel default false;
    property ZeroLevel: Double
      read FZeroLevel write SetZeroLevel stored IsZeroLevelStored;
  end;

  TSeriesPointerDrawEvent = procedure (
    ASender: TChartSeries; ACanvas: TCanvas; AIndex: Integer;
    ACenter: TPoint) of object;

  TLineType = (ltNone, ltFromPrevious, ltFromOrigin, ltStepXY, ltStepYX);

  TColorEachMode = (ceNone, cePoint, ceLineBefore, ceLineAfter,
    cePointAndLineBefore, cePointAndLineAfter);

  { TLineSeries }

  TLineSeries = class(TBasicPointSeries)
  private
    FLinePen: TPen;
    FLineType: TLineType;
    FOnDrawPointer: TSeriesPointerDrawEvent;
    FColorEach: TColorEachMode;

    procedure DrawSingleLineInStack(ADrawer: IChartDrawer; AIndex: Integer);
    function GetShowLines: Boolean;
    function GetShowPoints: Boolean;
    procedure SetColorEach(AValue: TColorEachMode);
    procedure SetLinePen(AValue: TPen);
    procedure SetLineType(AValue: TLineType);
    procedure SetSeriesColor(AValue: TColor);
    procedure SetShowLines(Value: Boolean);
    procedure SetShowPoints(AValue: Boolean);
  protected
    procedure AfterDrawPointer(
      ADrawer: IChartDrawer; AIndex: Integer; const APos: TPoint); override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetSeriesColor: TColor; override;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(ADrawer: IChartDrawer); override;
  published
    property AxisIndexX;
    property AxisIndexY;
    property ColorEach: TColorEachMode
      read FColorEach write SetColorEach default cePoint;
    property Depth;
    property DepthBrightnessDelta;
    property LinePen: TPen read FLinePen write SetLinePen;
    property LineType: TLineType
      read FLineType write SetLineType default ltFromPrevious;
    property MarkPositions;
    property Marks;
    property Pointer;
    property SeriesColor: TColor
      read GetSeriesColor write SetSeriesColor stored false default clBlack;
    property ShowLines: Boolean
      read GetShowLines write SetShowLines stored false default true;
    property ShowPoints: Boolean
      read GetShowPoints write SetShowPoints default false;
    property Stacked default false;
    property StackedNaN;
    property Source;
    property Styles;
    property ToolTargets;
    property XErrorBars;
    property YErrorBars;
    // Events
    property OnCustomDrawPointer;
    property OnGetPointerStyle;
  end;

  // Scatter plot displaying a single pixel per data point.
  // Optimized to work efficiently for millions of points.
  // See http://en.wikipedia.org/wiki/Manhattan_plot
  TManhattanSeries = class(TBasicPointSeries)
  private
    FSeriesColor: TColor;

    procedure SetSeriesColor(AValue: TColor);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;
  public
    procedure Assign(ASource: TPersistent); override;
    procedure Draw(ADrawer: IChartDrawer); override;
  published
    property AxisIndexX;
    property AxisIndexY;
    property SeriesColor: TColor
      read FSeriesColor write SetSeriesColor default clBlack;
    property Source;
  end;

  TLineStyle = (lsVertical, lsHorizontal);

  { TConstantLine }

  TConstantLine = class(TCustomChartSeries)
  strict private
    FArrow: TChartArrow;
    FLineStyle: TLineStyle;
    FPen: TPen;
    FPosition: Double; // Graph coordinate of line
    FUseBounds: Boolean;

    function GetAxisIndex: TChartAxisIndex;
    function GetSeriesColor: TColor;
    procedure SavePosToCoord(var APoint: TDoublePoint);
    procedure SetArrow(AValue: TChartArrow);
    procedure SetAxisIndex(AValue: TChartAxisIndex);
    procedure SetLineStyle(AValue: TLineStyle);
    procedure SetPen(AValue: TPen);
    procedure SetPosition(AValue: Double);
    procedure SetSeriesColor(AValue: TColor);
    procedure SetUseBounds(AValue: Boolean);
  protected
    procedure AfterAdd; override;
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
    procedure MovePoint(var AIndex: Integer; const ANewPos: TDoublePoint); override;
    procedure UpdateBiDiMode; override;
  published
    property Active default true;
    property Arrow: TChartArrow read FArrow write SetArrow;
    property AxisIndex: TChartAxisIndex
      read GetAxisIndex write SetAxisIndex default DEF_AXIS_INDEX;
    property AxisIndexX stored false; deprecated 'Use "AxisIndex"';
    property LineStyle: TLineStyle
      read FLineStyle write SetLineStyle default lsHorizontal;
    property Pen: TPen read FPen write SetPen;
    property Position: Double read FPosition write SetPosition;
    property SeriesColor: TColor
      read GetSeriesColor write SetSeriesColor stored false default clBlack;
    property ShowInLegend;
    property Title;
    property UseBounds: Boolean read FUseBounds write SetUseBounds default true;
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
    procedure Assign(ASource: TPersistent); override;
    procedure Draw(ADrawer: IChartDrawer); override;
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
  GraphMath, GraphType, IntfGraphics, LResources, Math, PropEdits, SysUtils,
  TAChartStrConsts, TADrawerCanvas, TAGeometry, TACustomSource, TAGraph,
  TAMath, TAStyles;

{ TLineSeries }

procedure TLineSeries.AfterDrawPointer(
  ADrawer: IChartDrawer; AIndex: Integer; const APos: TPoint);
var
  ic: IChartTCanvasDrawer;
begin
  if Supports(ADrawer, IChartTCanvasDrawer, ic) and Assigned(FOnDrawPointer) then
    FOnDrawPointer(Self, ic.Canvas, AIndex, APos);
end;

procedure TLineSeries.Assign(ASource: TPersistent);
begin
  if ASource is TLineSeries then
    with TLineSeries(ASource) do begin
      Self.LinePen := FLinePen;
      Self.FLineType := FLineType;
      Self.FOnDrawPointer := FOnDrawPointer;
      Self.FColorEach := FColorEach;
    end;
  inherited Assign(ASource);
end;

constructor TLineSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorEach := cePoint;
  FLinePen := TPen.Create;
  FLinePen.OnChange := @StyleChanged;
  FPointer := TSeriesPointer.Create(FChart);
  SetPropDefaults(Self, ['LineType']);
end;

destructor TLineSeries.Destroy;
begin
  FreeAndNil(FLinePen);
  inherited;
end;

procedure TLineSeries.Draw(ADrawer: IChartDrawer);

  procedure RemoveStackedNaN;
  var
    i, j: Integer;
    item: PChartDataItem;
  begin
    if FStacked and (FStackedNaN = snDoNotDraw) then
      for i := 0 to High(FGraphPoints) do begin
        item := Source.Item[i + FLoBound];
        if not IsNaN(item^.X) then
          if IsNaN(item^.Y) then
            FGraphPoints[i].X := NaN
          else
            for j := 0 to Source.YCount-2 do
              if IsNaN(item^.YList[j]) then begin
                FGraphPoints[i].X := NaN;
                break;
              end;
      end;
  end;

var
  ext: TDoubleRect;
  i: Integer;
begin
  with Extent do begin
    ext.a := AxisToGraph(a);
    ext.b := AxisToGraph(b);
  end;
  NormalizeRect(ext);
  if LineType = ltFromOrigin then
    ExpandRect(ext, AxisToGraph(ZeroDoublePoint));
  // Do not draw anything if the series extent does not intersect CurrentExtent.
  if not RectIntersectsRect(ext, ParentChart.CurrentExtent) then exit;

  PrepareGraphPoints(ext, LineType <> ltFromOrigin);
  RemoveStackedNaN;
  DrawSingleLineInStack(ADrawer, 0);
  for i := 0 to Source.YCount - 2 do begin
    if Source.IsYErrorIndex(i+1) then Continue;
    UpdateGraphPoints(i, FStacked);
    RemoveStackedNaN;
    DrawSingleLineInStack(ADrawer, i + 1);
  end;
end;

procedure TLineSeries.DrawSingleLineInStack(
  ADrawer: IChartDrawer; AIndex: Integer);
var
  points: array of TPoint;
  pointCount: Integer = 0;
  breaks: TIntegerDynArray;
  breakCount: Integer = 0;

  // Drawing long polylines with wide pen is very inefficient on Windows and GTK.
  // On Windows it is so bad that trying to draw polyline with 50000 points
  // will cause hard freeze of entire OS. (!)
  // Also, Windows refuses to draw any polyline with number of points
  // above approximately one million.
  // So, split long polylines into segments.
  function PolylineIsTooLong: Boolean; inline;
  // There is a trade-off between the call overhead for short serment and
  // the above-mentioned inefficiency for long ones.
  // First value was selected by some experiments as "optimal enough" for
  // both affected platforms.
  {$IF defined(LCLWIN32) or defined(LCLGTK2)}
  const
    MAX_LENGTH: array [Boolean] of Integer = (50000, 1000000);
  {$ENDIF}
  begin
    {$IF defined(LCLWIN32)}
    Result :=
      (breakCount > 0) and
      (pointCount - breaks[breakCount - 1] > MAX_LENGTH[LinePen.Width = 1]);
    {$ELSEIF defined(LCLGTK2)}
    Result :=
      (LinePen.Width > 1) and (breakCount > 0) and
      (pointCount - breaks[breakCount - 1] > MAX_LENGTH[false]);
    {$ELSE}
    Result := false;
    {$ENDIF}
  end;

  procedure PushPoint(const APoint: TPoint); inline;
  begin
    if pointCount > High(points) then
      SetLength(points, Length(points) * 2);
    points[pointCount] := APoint;
    pointCount += 1;
  end;

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
    if
      (pointCount = 0) or (points[pointCount - 1] <> ai) or PolylineIsTooLong
    then begin
      breaks[breakCount] := pointCount;
      breakCount += 1;
      PushPoint(ai);
    end;
    PushPoint(bi);
  end;

  procedure DrawStep(const AP1, AP2: TDoublePoint);
  var
    m: TDoublePoint;
  begin
    if (LineType = ltStepXY) xor IsRotated then
      m := DoublePoint(AP2.X, AP1.Y)
    else
      m := DoublePoint(AP1.X, AP2.Y);
    CacheLine(AP1, m);
    CacheLine(m, AP2);
  end;

  procedure DrawDefaultLines;
  var
    i, j: Integer;
    p, pPrev: TDoublePoint;
    pNan, pPrevNan: Boolean;
    scaled_depth: Integer;
  begin
    if LineType = ltNone then exit;
    // For extremely long series (10000 points or more), the Canvas.Line call
    // becomes a bottleneck. So represent a serie as a sequence of polylines.
    // This achieves approximately 3x speedup for the typical case.
    SetLength(points, Length(FGraphPoints) + 1);
    SetLength(breaks, Length(FGraphPoints) + 1);
    pPrevNan := true;
    // Actually needed only for ltFromOrigin, but moved to silence a warning.
    pPrev := AxisToGraph(ZeroDoublePoint);
    case LineType of
      ltFromPrevious:
        for p in FGraphPoints do begin
          pNan := IsNan(p);
          if not (pNan or pPrevNan) then
            CacheLine(pPrev, p);
          pPrev := p;
          pPrevNan := pNan;
        end;
      ltFromOrigin:
        for p in FGraphPoints do
          if not IsNan(p) then
            CacheLine(pPrev, p);
      ltStepXY, ltStepYX:
        for p in FGraphPoints do begin
          pNan := IsNan(p);
          if not (pNan or pPrevNan) then
            DrawStep(pPrev, p);
          pPrev := p;
          pPrevNan := pNan;
        end;
    end;
    breaks[breakCount] := pointCount;
    breakCount += 1;
    SetLength(points, pointCount);
    SetLength(breaks, breakCount);

    ADrawer.SetBrushParams(bsClear, clTAColor);
    ADrawer.Pen := LinePen;
    if Styles <> nil then
      Styles.Apply(ADrawer, AIndex);
    if Depth = 0 then
      for i := 0 to High(breaks) - 1 do
        ADrawer.Polyline(points, breaks[i], breaks[i + 1] - breaks[i])
    else begin
      if Styles = nil then begin
        ADrawer.SetBrushParams(bsSolid, GetDepthColor(LinePen.Color));
        ADrawer.SetPenParams(LinePen.Style, clBlack);
      end;
      scaled_depth := ADrawer.Scale(Depth);
      for i := 0 to High(breaks) - 1 do
        for j := breaks[i] to breaks[i + 1] - 2 do
          ADrawer.DrawLineDepth(points[j], points[j + 1], scaled_depth);
    end;
  end;

  function GetPtColor(AIndex: Integer): TColor;
  begin
    Result := Source[AIndex]^.Color;
    if Result = clTAColor then Result := SeriesColor;
  end;

  procedure DrawColoredLines;
  var
    i, n: Integer;
    gp: TDoublepoint;
    col, col1, col2: TColor;
    imgPt1, imgPt2: TPoint;
    pt, origin: TPoint;
    hasBreak: Boolean;
  begin
    if LineType = ltNone then exit;

    n := Length(FGraphPoints);

    // Find first point
    i := 0;
    while (i < n) do begin
      gp := FGraphPoints[i];
      if not IsNaN(gp) then break;
      inc(i);
    end;
    if i = n then
      exit;

    ADrawer.Pen := LinePen;
    imgPt1 := ParentChart.GraphToImage(gp);
    col1 := GetPtColor(i);

    // First line for line type ltFromOrigin
    if LineType = ltFromOrigin then begin
      origin := ParentChart.GraphToImage(AxisToGraph(ZeroDoublePoint));
      ADrawer.SetPenParams(FLinePen.Style, col1);
      ADrawer.Line(origin, imgPt1);
    end;

    // iterate through all other points
    hasBreak := false;
    while (i < n) do begin
      gp := FGraphPoints[i];
      if IsNaN(gp) then begin
        hasBreak := true;
      end else begin
        if hasBreak then begin
          imgPt1 := ParentChart.GraphToImage(gp);
          hasBreak := false;
        end;
        imgPt2 := ParentChart.GraphToImage(gp);
        col2 := GetPtColor(i);
        if imgPt1 <> imgPt2 then begin
          case FColorEach of
            ceLineBefore, cePointAndLineBefore: col := col2;
            ceLineAfter, cePointAndLineAfter: col := col1;
            else raise Exception.Create('TLineSeries: ColorEach error');
          end;
          ADrawer.SetPenParams(FLinePen.Style, col);
          case LineType of
            ltFromPrevious:
              ADrawer.Line(imgPt1, imgPt2);
            ltStepXY:
              begin
                pt := Point(imgPt2.x, imgPt1.Y);
                ADrawer.Line(imgPt1, pt);
                ADrawer.Line(pt, imgPt2);
              end;
            ltStepYX:
              begin
                pt := Point(imgPt1.x, imgPt2.Y);
                ADrawer.Line(imgPt1, pt);
                ADrawer.Line(pt, imgPt2);
              end;
            ltFromOrigin:
              ADrawer.Line(origin, imgPt2);
          end;
        end;
        imgPt1 := imgPt2;
        col1 := col2;
      end;
      inc(i);
    end;
  end;

begin
  case FColorEach of
    ceNone, cePoint:
      DrawDefaultLines;
    else
      DrawColoredLines;
  end;
  if AIndex = 0 then
    DrawErrorBars(ADrawer);
  DrawLabels(ADrawer, AIndex);
  if ShowPoints then
    DrawPointers(ADrawer, AIndex, FColorEach in [cePoint, cePointAndLineBefore, cePointAndLineAfter]);
end;

procedure TLineSeries.GetLegendItems(AItems: TChartLegendItems);
var
  lb: TBrush;
  lp: TPen;
  p: TSeriesPointer;
  i: Integer;
  li: TLegendItemLinePointer;
  s: TChartStyle;
begin
  if LineType = ltNone then
    lp := nil
  else
    lp := LinePen;
  if ShowPoints then
    p := Pointer
  else
    p := nil;
  case Legend.Multiplicity of
    lmSingle:
      AItems.Add(TLegendItemLinePointer.Create(lp, p, LegendTextSingle));
    lmPoint:
      for i := 0 to Count - 1 do begin
        li := TLegendItemLinePointer.Create(lp, p, LegendTextPoint(i));
        li.Color := GetColor(i);
        AItems.Add(li);
      end;
    lmStyle:
      if Styles <> nil then begin
        if Assigned(p) then lb := p.Brush else lb := nil;
        for s in Styles.Styles do
          AItems.Add(TLegendItemLinePointer.CreateWithBrush(
            IfThen((lp <> nil) and s.UsePen, s.Pen, lp) as TPen,
            IfThen(s.UseBrush, s.Brush, lb) as TBrush,
            p,
            LegendTextStyle(s)
          ));
        end;
  end;
end;

function TLineSeries.GetSeriesColor: TColor;
begin
  Result := FLinePen.Color;
end;

function TLineSeries.GetShowLines: Boolean;
begin
  Result := FLineType <> ltNone;
end;

function TLineSeries.GetShowPoints: Boolean;
begin
  Result := FPointer.Visible;
end;

procedure TLineSeries.SetColorEach(AValue: TColorEachMode);
begin
  if FColorEach = AValue then exit;
  FColorEach := AValue;
  UpdateParentChart;
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

procedure TLineSeries.SetShowPoints(AValue: Boolean);
begin
  if ShowPoints = AValue then exit;
  FPointer.Visible := AValue;
  UpdateParentChart;
end;


{ TManhattanSeries }

procedure TManhattanSeries.Assign(ASource: TPersistent);
begin
  if ASource is TManhattanSeries then
    with TManhattanSeries(ASource) do
      Self.FSeriesColor := SeriesColor;
  inherited Assign(ASource);
end;

procedure TManhattanSeries.Draw(ADrawer: IChartDrawer);
var
  img: TLazIntfImage;
  topLeft, pt: TPoint;
  i, cnt: Integer;
  ext: TDoubleRect;
  rawImage: TRawImage;
  r: TRect;

  procedure PutPixel(const APoint: TPoint; AColor: TChartColor); inline;
  begin
    PCardinal(rawImage.Data)[APoint.Y * r.Right + APoint.X] :=
      Cardinal(ColorDef(AColor, SeriesColor)) or $FF000000; // Opacity.
    cnt += 1;
  end;

begin
  with Extent do begin
    ext.a := AxisToGraph(a);
    ext.b := AxisToGraph(b);
  end;
  NormalizeRect(ext);
  if not RectIntersectsRect(ext, ParentChart.CurrentExtent) then exit;

  // Do not cache graph points to reduce memory overhead.
  FindExtentInterval(ext, true);
  topLeft := ParentChart.ClipRect.TopLeft;
  r := BoundsSize(0, 0, ParentChart.ClipRect.BottomRight - topLeft);

  cnt := 0;
  img := CreateLazIntfImage(rawImage, r.BottomRight);
  try
    // AxisToGraph is slow, so split loop to optimize non-transformed case.
    if (AxisIndexX = -1) and (AxisIndexY = -1) then
      for i := FLoBound to FUpBound do
        with Source[i]^ do begin
          pt := ParentChart.GraphToImage(Point) - topLeft;
          if PtInRect(r, pt) then
            PutPixel(pt, Color);
        end
    else
      for i := FLoBound to FUpBound do
        with Source[i]^ do begin
          pt := ParentChart.GraphToImage(AxisToGraph(Point)) - topLeft;
          if PtInRect(r, pt) then
            PutPixel(pt, Color);
        end;
    if cnt > 0 then
      ADrawer.PutImage(topLeft.X, topLeft.Y, img);
  finally
    img.Free;
  end;
end;

procedure TManhattanSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  Unused(AItems); // TODO
end;

procedure TManhattanSeries.SetSeriesColor(AValue: TColor);
begin
  if FSeriesColor = AValue then exit;
  FSeriesColor := AValue;
  UpdateParentChart;
end;

{ TConstantLine }

procedure TConstantLine.AfterAdd;
begin
  inherited;
  Arrow.SetOwner(ParentChart);
end;

procedure TConstantLine.Assign(ASource: TPersistent);
begin
  if ASource is TConstantLine then
    with TConstantLine(ASource) do begin
      Self.FArrow.Assign(FArrow);
      Self.FLineStyle := FLineStyle;
      Self.Pen := FPen;
      Self.FPosition := FPosition;
      Self.FUseBounds := FUseBounds;
    end;
  inherited Assign(ASource);
end;

constructor TConstantLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArrow := TChartArrow.Create(ParentChart);
  FLineStyle := lsHorizontal;
  FPen := TPen.Create;
  FPen.OnChange := @StyleChanged;
  FUseBounds := true;
end;

destructor TConstantLine.Destroy;
begin
  FreeAndNil(FArrow);
  FreeAndNil(FPen);
  inherited;
end;

procedure TConstantLine.Draw(ADrawer: IChartDrawer);
var
  p: Integer;
begin
  if Pen.Style = psClear then exit;

  ADrawer.SetBrushParams(bsClear, clTAColor);
  ADrawer.Pen := FPen;

  with ParentChart do
    case LineStyle of
      lsHorizontal: begin
        p := YGraphToImage(AxisToGraphX(Position));
        // The "X" here is correct:
        // The constant line series needs only a single axis, which is its
        // "x axis" - the user will set the axis index to that of the y axis
        // for the case of a horizontal line. Therefore, AxisToGraph must get
        // the transformation from the line's x axis (even if it is the y axis
        // of the chart!).
        DrawLineHoriz(ADrawer, p);
        if Arrow.Inverted then
          Arrow.Draw(ADrawer, Point(ClipRect.Left, p), 0, Pen)
        else
          Arrow.Draw(ADrawer, Point(ClipRect.Right - 1, p), 0, Pen);
      end;
      lsVertical: begin
        p := XGraphToImage(AxisToGraphX(Position));
        DrawLineVert(ADrawer, p);
        if Arrow.Inverted then
          Arrow.Draw(ADrawer, Point(p, ClipRect.Bottom - 1), -Pi / 2, Pen)
        else
          Arrow.Draw(ADrawer, Point(p, ClipRect.Top), -Pi / 2, Pen);
      end;
    end;
end;

function TConstantLine.GetAxisIndex: TChartAxisIndex;
begin
  Result := inherited AxisIndexX;
end;

procedure TConstantLine.GetBounds(var ABounds: TDoubleRect);
begin
  if not UseBounds then exit;
  SavePosToCoord(ABounds.a);
  SavePosToCoord(ABounds.b);
end;

procedure TConstantLine.GetLegendItems(AItems: TChartLegendItems);
begin
  AItems.Add(TLegendItemLine.Create(Pen, LegendTextSingle));
end;

function TConstantLine.GetNearestPoint(
  const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
begin
  AResults.FIndex := -1;
  AResults.FImg := AParams.FPoint;
  // Return the actual nearest point of the line.
  if LineStyle = lsVertical then begin
    AResults.FValue.Y := FChart.YImageToGraph(AParams.FPoint.Y);
    AResults.FImg.X := FChart.XGraphToImage(AxisToGraphX(Position));
  end
  else begin
    AResults.FValue.X := FChart.XImageToGraph(AParams.FPoint.X);
    AResults.FImg.Y := FChart.YGraphToImage(AxisToGraphX(Position));
  end;
  AResults.FDist := AParams.FDistFunc(AParams.FPoint, AResults.FImg);
  Result := AResults.FDist <= Sqr(AParams.FRadius);
  SavePosToCoord(AResults.FValue);
end;

function TConstantLine.GetSeriesColor: TColor;
begin
  Result := FPen.Color;
end;

function TConstantLine.IsEmpty: Boolean;
begin
  Result := false;
end;

procedure TConstantLine.MovePoint(
  var AIndex: Integer; const ANewPos: TDoublePoint);
begin
  Unused(AIndex);
  Position :=
    GraphToAxisX(TDoublePointBoolArr(ANewPos)[LineStyle = lsHorizontal]);
end;

procedure TConstantLine.SavePosToCoord(var APoint: TDoublePoint);
begin
  TDoublePointBoolArr(APoint)[LineStyle = lsHorizontal] := Position;
end;

procedure TConstantLine.SetArrow(AValue: TChartArrow);
begin
  FArrow.Assign(AValue);
  UpdateParentChart;
end;

procedure TConstantLine.SetAxisIndex(AValue: TChartAxisIndex);
begin
  inherited AxisIndexX := AValue;
  AxisIndexY := AValue;
  // Make sure that both axis indexes have the same value. The ConstantLineSeries
  // does use only the x axis index, but transformations of the y axis outside
  // this unit may require tha y axis index - which would not be correct without
  // this here...
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

procedure TConstantLine.SetPosition(AValue: Double);
begin
  if FPosition = AValue then exit;
  FPosition := AValue;
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

procedure TConstantLine.UpdateBiDiMode;
begin
  if LineStyle = lsHorizontal then
    Arrow.Inverted := not Arrow.Inverted;
end;

{ TBarSeries }

procedure TBarSeries.Assign(ASource: TPersistent);
begin
  if ASource is TBarSeries then
    with TBarSeries(ASource) do begin
      Self.BarBrush := FBarBrush;
      Self.FBarOffsetPercent := FBarOffsetPercent;
      Self.BarPen := FBarPen;
      Self.FBarWidthPercent := FBarWidthPercent;
      Self.FBarWidthStyle := FBarWidthStyle;
      Self.FOnBeforeDrawBar := FOnBeforeDrawBar;
      Self.FUseZeroLevel := FUseZeroLevel;
      Self.FZeroLevel := FZeroLevel;
    end;
  inherited Assign(ASource);
end;

procedure TBarSeries.BarOffsetWidth(
  AX: Double; AIndex: Integer; out AOffset, AWidth: Double);
var
  r: Double;
begin
  case BarWidthStyle of
    bwPercent: r := GetXRange(AX, AIndex) * PERCENT;
    bwPercentMin: r := FMinXRange * PERCENT;
    else
      raise EBarError.Create('BarWidthStyle not implemented');
  end;
  AOffset := r * BarOffsetPercent;
  AWidth := r * BarWidthPercent / 2;
end;

constructor TBarSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ToolTargets := [nptPoint, nptYList, nptCustom];

  FBarWidthPercent := DEF_BAR_WIDTH_PERCENT;

  FBarBrush := TBrush.Create;
  FBarBrush.OnChange := @StyleChanged;

  FBarPen := TPen.Create;
  FBarPen.OnChange := @StyleChanged;
  FBarPen.Color := clBlack;
  FBarBrush.Color := clRed;

  FStacked := true;
  FOptimizeX := false;
  FSupportsZeroLevel := true;
  FUseZeroLevel := true;
end;

destructor TBarSeries.Destroy;
begin
  FreeAndNil(FBarPen);
  FreeAndNil(FBarBrush);
  inherited;
end;

procedure TBarSeries.Draw(ADrawer: IChartDrawer);
var
  pointIndex, stackIndex: Integer;
  scaled_depth: Integer;
  scaled_depth2: Integer;

  procedure DrawBar(const AR: TRect);
  var
    sz: TSize;
    defaultDrawing: Boolean = true;
    c: TColor;
    ic: IChartTCanvasDrawer;
    pts: TPointArray;
    a, b, cx, cy, factor: Double;
    h: Integer;
  begin
    ADrawer.Pen := BarPen;
    ADrawer.Brush := BarBrush;
    c := Source[pointIndex]^.Color;
    if c <> clTAColor then
      ADrawer.BrushColor := c;
    if Styles <> nil then
      Styles.Apply(ADrawer, stackIndex);
    sz := Size(AR);
    if (sz.cx <= 2) or (sz.cy <= 2) then begin
      // Bars are too small to distinguish the border from the interior.
      ADrawer.SetPenParams(psSolid, ADrawer.BrushColor);
    end;

    if Assigned(FOnCustomDrawBar) then begin
      FOnCustomDrawBar(Self, ADrawer, AR, pointIndex, stackIndex);
      exit;
    end;

    if Supports(ADrawer, IChartTCanvasDrawer, ic) and Assigned(OnBeforeDrawBar) then
      OnBeforeDrawBar(Self, ic.Canvas, AR, pointIndex, stackIndex, defaultDrawing);
    if not defaultDrawing then exit;

    case FBarShape of
      bsRectangular:
        begin
          ADrawer.Rectangle(AR);
          if Depth > 0 then begin
            c := ADrawer.BrushColor;
            ADrawer.BrushColor := GetDepthColor(c, true);
            ADrawer.DrawLineDepth(
              AR.Left, AR.Top, AR.Right - 1, AR.Top, scaled_depth);
            ADrawer.BrushColor := GetDepthColor(c, false);
            ADrawer.DrawLineDepth(
              AR.Right - 1, AR.Top, AR.Right - 1, AR.Bottom - 1, scaled_depth);
          end;
        end;
      bsPyramid:
        begin
          c := ADrawer.BrushColor;
          SetLength(pts, 3);
          if Depth = 0 then begin
            pts[0] := Point(AR.Left, AR.Bottom);
            if IsRotated then begin
              pts[1] := Point(AR.Left, AR.Top);
              pts[2] := Point(AR.Right, (AR.Top + AR.Bottom) div 2);
            end else begin
              pts[1] := Point(AR.Right, AR.Bottom);
              pts[2] := Point((AR.Left + AR.Right) div 2, AR.Top);
            end;
            ADrawer.Polygon(pts, 0, 3);
          end else
          if IsRotated then begin
            pts[0] := Point(AR.Left + scaled_depth, AR.Bottom - scaled_depth);
            pts[1] := Point(AR.Left + scaled_depth, AR.Top - scaled_depth);
            pts[2] := Point(AR.Right + scaled_depth2, (AR.Top + AR.Bottom - scaled_depth) div 2);
            ADrawer.BrushColor := GetDepthColor(c);
            ADrawer.Polygon(pts, 0, 3);
            pts[1] := Point(AR.Left, AR.Bottom);
            ADrawer.Polygon(pts, 0, 3);
            pts[0] := Point(AR.Left + scaled_depth, AR.Top - scaled_depth);
            pts[1] := Point(AR.Left, AR.Top);
            ADrawer.Polygon(pts, 0, 3);
            ADrawer.BrushColor := c;
            pts[0] := Point(AR.Left, AR.Bottom);
            ADrawer.Polygon(pts, 0, 3);
          end else begin
            // rear side
            pts[0] := Point(AR.Left + scaled_depth, AR.Bottom - scaled_depth);
            pts[1] := Point(AR.Right + scaled_depth, AR.Bottom - scaled_depth);
            pts[2] := Point((AR.Left + AR.Right + scaled_depth) div 2, AR.Top - scaled_depth2);
            ADrawer.BrushColor := GetDepthColor(c);
            ADrawer.Polygon(pts, 0, 3);
            // left side
            pts[1] := Point(AR.Left, AR.Bottom);
            ADrawer.Polygon(pts, 0, 3);
            // right side
            pts[0] := Point(AR.Right + scaled_depth, AR.Bottom - scaled_depth);
            pts[1] := Point(AR.Right, AR.Bottom);
            ADrawer.Polygon(pts, 0, 3);
            // front face
            ADrawer.BrushColor := c;
            pts[0] := Point(AR.Left, AR.Bottom);
            ADrawer.Polygon(pts, 0, 3);
          end;
        end;
      bsCylindrical:
        begin
          if Depth = 0 then
            ADrawer.Rectangle(AR)
          else
          if IsRotated then begin
            ADrawer.Ellipse(AR.Left, AR.Top, AR.Left + scaled_depth, AR.Bottom);
            ADrawer.FillRect(AR.Left + scaled_depth2, AR.Top, AR.Right + scaled_depth2, AR.Bottom);
            ADrawer.Line(AR.Left + scaled_depth2, AR.Top, AR.Right + scaled_depth2, AR.Top);
            ADrawer.Line(AR.Left + scaled_depth2, AR.Bottom, AR.Right + scaled_depth2, AR.Bottom);
            ADrawer.BrushColor := GetDepthColor(ADrawer.BrushColor, false);
            ADrawer.Ellipse(AR.Right, AR.Top, AR.Right + scaled_depth, AR.Bottom);
          end else begin
            ADrawer.Ellipse(AR.Left, AR.Bottom, AR.Right, AR.Bottom - scaled_depth);
            ADrawer.FillRect(AR.Left, AR.Bottom - scaled_depth2, AR.Right, AR.Top - scaled_depth2);
            ADrawer.Line(AR.Left, AR.Bottom - scaled_depth2, AR.Left, AR.Top - scaled_depth2);
            ADrawer.Line(AR.Right, AR.Bottom - scaled_depth2, AR.Right, AR.Top - scaled_depth2);
            ADrawer.BrushColor := GetDepthColor(ADrawer.BrushColor, true);
            ADrawer.Ellipse(AR.Left, AR.Top, AR.Right, AR.Top - scaled_depth);
          end;
        end;
      bsConical:
        begin
          SetLength(pts, 3);
          if Depth = 0 then begin
            pts[0] := Point(AR.Left, AR.Bottom);
            if IsRotated then begin
              pts[1] := Point(AR.Left, AR.Top);
              pts[2] := Point(AR.Right, (AR.Top + AR.Bottom) div 2);
            end else begin
              pts[1] := Point(AR.Right, AR.Bottom);
              pts[2] := Point((AR.Left + AR.Right) div 2, AR.Top);
            end;
            ADrawer.Polygon(pts, 0, 3);
          end else begin
            if IsRotated then begin
              ADrawer.Ellipse(AR.Left, AR.Top, AR.Left + scaled_depth, AR.Bottom);
              h := AR.Right - AR.Left;
              if h <= scaled_depth2 then
                exit;
              a := (AR.Bottom - AR.Top) * 0.5;
              b := scaled_depth2;
              cx := (AR.Top + AR.Bottom) * 0.5;
              cy := AR.Left + scaled_depth2;
              factor := sqrt(1 - sqr(b/h));
              pts[0] := Point(round(cy + sqr(b)/h), round(cx - a*factor));
              pts[1] := Point(AR.Right + scaled_depth2, round(cx));
              pts[2] := Point(round(cy + sqr(b)/h), round(cx + a*factor));
            end else begin
              ADrawer.Ellipse(AR.Left, AR.Bottom, AR.Right, AR.Bottom - scaled_depth);
              // https://www.emathzone.com/tutorials/geometry/equation-of-tangent-and-normal-to-ellipse.html
              // tangent to ellipse (x/a)² + (y/b)² = 1 at ellipse point (x1, y1):
              //    (x1 x) / a² + (y1 x) / b² = 1
              h := AR.Bottom - AR.Top;      // height of cone
              if h <= scaled_depth2 then
                exit;
              a := (AR.Right - AR.Left) * 0.5;
              b := scaled_depth2;
              cx := (AR.Left + AR.Right) * 0.5;    // center of cone ground area
              cy := AR.Bottom - scaled_depth2;
              factor := sqrt(1 - sqr(b/h));
              pts[0] := Point(round(cx - a*factor), round(cy - sqr(b) / h));
              pts[1] := Point(round(cx), AR.Top - scaled_depth2);
              pts[2] := Point(round(cx + a*factor), round(cy - sqr(b) / h));
            end;
            ADrawer.SetPenParams(psClear, clTAColor);
            ADrawer.Polygon(pts, 0, 3);
            ADrawer.Pen := BarPen;
            if Styles <> nil then
              Styles.Apply(ADrawer, stackIndex);
            ADrawer.PolyLine(pts, 0, 3);
          end;
        end;
      bsHexPrism:
        begin
          a := IfThen(IsRotated, AR.Bottom - AR.Top, AR.Right - AR.Left) * 0.5;
          cx := IfThen(IsRotated, AR.Top + AR.Bottom, AR.Left + AR.Right) * 0.5;
          cy := scaled_depth2;
          SetLength(pts, 4);
          factor := sin(pi * 30 / 180);
          if Depth = 0 then begin
            pts[0] := Point(AR.Left, AR.Bottom);
            if IsRotated then begin
              pts[1] := Point(AR.Left, round(cx + a*factor));
              pts[2] := Point(AR.Right, pts[1].Y);
              pts[3] := Point(AR.Right, AR.Bottom);
              ADrawer.Polygon(pts, 0, 4);
              pts[0] := pts[1];
              pts[3] := pts[2];
              pts[1] := Point(AR.Left, round(cx - a*factor));
              pts[2] := Point(AR.Right, pts[1].Y);
              ADrawer.Polygon(pts, 0, 4);
              pts[0] := pts[1];
              pts[3] := pts[2];
              pts[1] := Point(AR.Left, AR.Top);
              pts[2] := Point(AR.Right, AR.Top);
              ADrawer.Polygon(pts, 0, 4);
            end else begin
              pts[1] := Point(round(cx - a*factor), AR.Bottom);
              pts[2] := Point(pts[1].X, AR.Top);
              pts[3] := Point(AR.Left, AR.Top);
              ADrawer.Polygon(pts, 0, 4);
              pts[0] := pts[1];
              pts[3] := pts[2];
              pts[1] := Point(round(cx + a*factor), AR.Bottom);
              pts[2] := Point(pts[1].X, AR.Top);
              ADrawer.Polygon(pts, 0, 4);
              pts[0] := pts[1];
              pts[3] := pts[2];
              pts[1] := Point(AR.Right, AR.Bottom);
              pts[2] := Point(AR.Right, AR.Top);
              ADrawer.Polygon(pts, 0, 4);
            end;
          end else begin
            c := ADrawer.BrushColor;
            ADrawer.BrushColor := GetDepthColor(c);
            if IsRotated then begin
              pts[0] := Point(AR.Left + scaled_depth2, AR.Bottom);
              pts[1] := Point(AR.Left, round(cx + a*factor));
              pts[2] := Point(AR.Right, pts[1].Y);
              pts[3] := Point(AR.Right + scaled_depth2, AR.Bottom);
              ADrawer.Polygon(pts, 0, 4);
              pts[0] := pts[1];
              pts[3] := pts[2];
              pts[1] := Point(AR.Left, round(cx - a*factor));
              pts[2] := Point(AR.Right, pts[1].Y);
              ADrawer.BrushColor := c;
              ADrawer.Polygon(pts, 0, 4);
              pts[0] := pts[1];
              pts[3] := pts[2];
              pts[1] := Point(AR.Left + scaled_depth2, AR.Top);
              pts[2] := Point(AR.Right + scaled_depth2, pts[1].Y);
              ADrawer.BrushColor := GetDepthColor(c);
              ADrawer.Polygon(pts, 0, 4);
              SetLength(pts, 6);
              pts[0] := Point(AR.Right + scaled_depth2, AR.Bottom);
              pts[1] := Point(AR.Right, round(cx + a*factor));
              pts[2] := Point(AR.Right, round(cx - a*factor));
              pts[3] := Point(AR.Right + scaled_depth2, AR.Top);
              pts[4] := Point(AR.Right + scaled_depth, round(cx - a*factor));
              pts[5] := Point(AR.Right + scaled_depth, round(cx + a*factor));
            end else begin
              pts[0] := Point(AR.Left, AR.Bottom - scaled_depth2);
              pts[1] := Point(round(cx - a*factor), AR.Bottom);
              pts[2] := Point(pts[1].X, AR.Top);
              pts[3] := Point(AR.Left, AR.Top - scaled_depth2);
              ADrawer.Polygon(pts, 0, 4);
              pts[0] := pts[1];
              pts[3] := pts[2];
              pts[1] := Point(round(cx + a*factor), AR.Bottom);
              pts[2] := Point(pts[1].X, AR.Top);
              ADrawer.BrushColor := c;
              ADrawer.Polygon(pts, 0, 4);
              pts[0] := pts[1];
              pts[3] := pts[2];
              pts[1] := Point(AR.Right, AR.Bottom - scaled_depth2);
              pts[2] := Point(AR.Right, AR.Top - scaled_depth2);
              ADrawer.BrushColor := GetDepthColor(c);
              ADrawer.Polygon(pts, 0, 4);
              SetLength(pts, 6);
              pts[0] := Point(AR.Left, AR.Top - scaled_depth2);
              pts[1] := Point(round(cx - a*factor), AR.Top);
              pts[2] := Point(round(cx + a*factor), AR.Top);
              pts[3] := Point(AR.Right, AR.Top - scaled_depth2);
              pts[4] := Point(round(cx + a*factor), AR.Top - scaled_depth);
              pts[5] := Point(round(cx - a*factor), AR.Top - scaled_depth);
            end;
            ADrawer.BrushColor := GetDepthColor(c, true);
            ADrawer.Polygon(pts, 0, 6);
          end;
        end;
    end;
  end;

var
  ext2: TDoubleRect;
  w: Double;
  p: TDoublePoint;
  heights: TDoubleDynArray;

  procedure BuildBar(x, y1, y2: Double);
  var
    graphBar: TDoubleRect;
    imageBar: TRect;
  begin
    graphBar := DoubleRect(x - w, y1, x + w, y2);
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

      // Draw a line instead of an empty rectangle.
   //   if Bottom = Top then Dec(Top);
   //   if Left = Right then Inc(Right);
    end;
    DrawBar(imageBar);
  end;

var
  ofs, y: Double;
  zero: Double;
begin
  if IsEmpty then exit;

  if BarWidthStyle = bwPercentMin then
    UpdateMinXRange;
  ext2 := ParentChart.CurrentExtent;
  ExpandRange(ext2.a.X, ext2.b.X, 1.0);
  ExpandRange(ext2.a.Y, ext2.b.Y, 1.0);

  scaled_depth := ADrawer.Scale(Depth);
  scaled_depth2 := scaled_depth div 2;
  if UseZeroLevel then
    zero := ZeroLevel
  else
    zero := IfThen(IsRotated, ext2.a.X, ext2.a.Y);

  PrepareGraphPoints(ext2, true);
  SetLength(heights, Source.YCount + 1);
  for pointIndex := FLoBound to FUpBound do begin
    p := Source[pointIndex]^.Point;
    if SkipMissingValues(pointIndex) then
      continue;
    p.X := AxisToGraphX(p.X);
    BarOffsetWidth(p.X, pointIndex, ofs, w);
    p.X += ofs;
    heights[0] := zero;
    if FStacked then begin
      heights[1] := NumberOr(p.Y, zero);
      for stackIndex := 1 to Source.YCount - 1 do begin
        y := NumberOr(Source[pointIndex]^.YList[stackIndex - 1], 0);
        heights[stackIndex + 1] := heights[stackIndex] + y;
      end;
      for stackIndex := 0 to High(heights) do
        heights[stackindex] := AxisToGraphY(heights[stackindex]);
      for stackIndex := 0 to Source.YCount - 1 do
        BuildBar(p.X, heights[stackindex], heights[stackIndex+1]);
    end else begin
      for stackIndex := 0 to Source.YCount - 1 do begin
        y := Source[pointIndex]^.GetY(stackIndex);
        if not IsNaN(y) then
          heights[stackIndex + 1] := AxisToGraphY(y)
        else
          heights[stackIndex + 1] := zero;
      end;
      p.X -= w;
      w := w / High(heights);
      p.X += w;
      for stackIndex := 0 to Source.YCount - 1 do begin
        BuildBar(p.X, heights[0], heights[stackIndex+1]);
        p.X += 2*w;
      end;
    end;
  end;

  DrawLabels(ADrawer);
end;

function TBarSeries.Extent: TDoubleRect;
var
  x, ofs, w: Double;
  i: Integer;
begin
  Result := inherited Extent;
  if IsEmpty then exit;
  if BarWidthStyle = bwPercentMin then
    UpdateMinXRange;
  if UseZeroLevel then
    UpdateMinMax(GraphToAxisY(ZeroLevel), Result.a.Y, Result.b.Y);

  // Show first and last bars fully.
  i := 0;
  x := NearestXNumber(i, +1);       // --> x is in graph units
  if not IsNan(x) then begin
    BarOffsetWidth(x, i, ofs, w);
    x := GraphToAxisX(x + ofs - w); // x is in graph units, Extent in axis units!
    Result.a.X := Min(Result.a.X, x);
  end;
  i := Count - 1;
  x := NearestXNumber(i, -1);
  if not IsNan(x) then begin
    BarOffsetWidth(x, i, ofs, w);
    x := GraphToAxisX(x + ofs + w);
    Result.b.X := Max(Result.b.X, x);
  end;
end;

function TBarSeries.GetBarWidth(AIndex: Integer): Integer;
var
  ofs, w: Double;
  f: TGraphToImageFunc;
begin
  BarOffsetWidth(GetGraphPointX(AIndex), AIndex, ofs, w);
  if IsRotated then
    f := @FChart.YGraphToImage
  else
    f := @FChart.XGraphToImage;
  Result := Abs(f(2 * w) - f(0));
end;

function TBarSeries.GetLabelDataPoint(AIndex, AYIndex: Integer): TDoublePoint;
var
  ofs, w, wbar: Double;
begin
  Result := inherited GetLabelDataPoint(AIndex, AYIndex);
  BarOffsetWidth(TDoublePointBoolArr(Result)[IsRotated], AIndex, ofs, w);
  TDoublePointBoolArr(Result)[IsRotated] += ofs;

  // Find x centers of bars in non-stacked bar series with multiple y values.
  if (not FStacked) and (Source.YCount > 1) then begin
    wbar := 2 * w / Source.YCount;
    TDoublePointboolArr(Result)[IsRotated] += (wbar * (AYIndex + 0.5) - w);
  end;
end;

procedure TBarSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  GetLegendItemsRect(AItems, BarBrush);
end;

function TBarSeries.GetNearestPoint(const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  pointIndex: Integer;
  graphClickPt: TDoublePoint;
  sp: TDoublePoint;
  ofs, w: Double;
  heights: TDoubleDynArray;
  y: Double;
  stackindex: Integer;
begin
  Result := false;
  AResults.FDist := Sqr(AParams.FRadius) + 1;
  AResults.FIndex := -1;
  AResults.FXIndex := 0;
  AResults.FYIndex := 0;

  if not ((nptCustom in AParams.FTargets) and (nptCustom in ToolTargets))
  then begin
    Result := inherited;
    exit;
  end;

  SetLength(heights, Source.YCount + 1);

  // clicked point in image units
  graphClickPt := ParentChart.ImageToGraph(AParams.FPoint);
  if IsRotated then
    Exchange(graphclickpt.X, graphclickpt.Y);

  // Iterate through all points of the series
  for pointIndex := 0 to Count - 1 do begin
    sp := Source[pointindex]^.Point;
    if IsNan(sp) then
      continue;
    sp.X := AxisToGraphX(sp.X);
    BarOffsetWidth(sp.X, pointindex, ofs, w); // works with graph units
    sp.X := sp.X + ofs;
    if not InRange(graphClickPt.X, sp.X - w, sp.X + w) then
      continue;
    // Calculate stacked bar levels (in axis units)
    heights[0] := ZeroLevel;
    heights[1] := NumberOr(sp.Y, ZeroLevel);
    for stackIndex := 1 to Source.YCount-1 do begin
      y := NumberOr(Source[pointindex]^.YList[stackIndex - 1], 0);
      heights[stackIndex + 1] := heights[stackindex] + y;
    end;
    // Convert heights to graph units
    for stackIndex := 0 to High(heights) do
      heights[stackIndex] := AxisToGraphY(heights[stackIndex]);
    // Check if clicked pt is inside stacked bar
    for stackindex := 0 to High(heights)-1 do
      if ((heights[stackindex] < heights[stackindex + 1]) and
         InRange(graphClickPt.Y, heights[stackindex], heights[stackIndex + 1]))
      or
         ((heights[stackindex + 1] < heights[stackindex]) and
         InRange(graphClickPt.Y, heights[stackindex + 1], heights[stackIndex]))
      then  begin
        AResults.FDist := 0;
        AResults.FIndex := pointindex;
        AResults.FYIndex := stackIndex;
        AResults.FValue := DoublePoint(Source[pointindex]^.X, Source[pointindex]^.GetY(stackIndex));
        AResults.FValue := AxisToGraph(AResults.FValue);
        AResults.FImg := ParentChart.GraphToImage(AResults.FValue);
        Result := true;
        exit;
      end;
  end;
end;

function TBarSeries.GetSeriesColor: TColor;
begin
  Result := FBarBrush.Color;
end;

function TBarSeries.GetZeroLevel: Double;
begin
  Result := IfThen(UseZeroLevel, ZeroLevel, 0.0);
end;

function TBarSeries.IsZeroLevelStored: boolean;
begin
  Result := ZeroLevel <> 0.0;
end;

procedure TBarSeries.SetBarBrush(Value: TBrush);
begin
  FBarBrush.Assign(Value);
end;

procedure TBarSeries.SetBarOffsetPercent(AValue: Integer);
begin
  if FBarOffsetPercent = AValue then exit;
  FBarOffsetPercent := AValue;
  UpdateParentChart;
end;

procedure TBarSeries.SetBarPen(Value:TPen);
begin
  FBarPen.Assign(Value);
end;

procedure TBarSeries.SetBarShape(AValue: TBarshape);
begin
  if FBarshape = AValue then exit;
  FBarShape := AValue;
  UpdateParentChart;
end;

procedure TBarSeries.SetBarWidthPercent(Value: Integer);
begin
  if (Value < 1) or (Value > 100) then
    raise EBarError.Create('Wrong BarWidth Percent');
  FBarWidthPercent := Value;
  UpdateParentChart;
end;

procedure TBarSeries.SetBarWidthStyle(AValue: TBarWidthStyle);
begin
  if FBarWidthStyle = AValue then exit;
  FBarWidthStyle := AValue;
  UpdateParentChart;
end;

procedure TBarSeries.SetOnBeforeDrawBar(AValue: TBeforeDrawBarEvent);
begin
  if TMethod(FOnBeforeDrawBar) = TMethod(AValue) then exit;
  FOnBeforeDrawBar := AValue;
  UpdateParentChart;
end;

procedure TBarSeries.SetOnCustomDrawBar(AValue: TCustomDrawBarEvent);
begin
  if TMethod(FOnCustomDrawBar) = TMethod(AValue) then exit;
  FOnCustomDrawBar := AValue;
  UpdateParentChart;
end;

procedure TBarSeries.SetSeriesColor(AValue: TColor);
begin
  FBarBrush.Color := AValue;
end;

procedure TBarSeries.SetUseZeroLevel(AValue: Boolean);
begin
  if FUseZeroLevel = AValue then exit;
  FUseZeroLevel := AValue;
//  FSupportsZeroLevel := FUseZeroLevel;
  UpdateParentChart;
end;

procedure TBarSeries.SetZeroLevel(AValue: Double);
begin
  if FZeroLevel = AValue then exit;
  FZeroLevel := AValue;
  UpdateParentChart;
end;

function TBarSeries.ToolTargetDistance(const AParams: TNearestPointParams;
  AGraphPt: TDoublePoint; APointIdx, AXIdx, AYIdx: Integer): Integer;
var
  sp1, sp2: TDoublePoint;
  clickPt, pt1, pt2: TPoint;
  ofs, w: Double;
  dist1, dist2: Integer;
begin
  Unused(APointIdx);
  Unused(AXIdx, AYIdx);

  clickPt := AParams.FPoint;
  if IsRotated then begin
    Exchange(clickPt.X, clickPt.Y);
    Exchange(AGraphPt.X, AGraphPt.Y);
  end;

  BarOffsetWidth(AGraphPt.X, APointIdx, ofs, w);
  sp1 := DoublePoint(AGraphPt.X + ofs - w, AGraphPt.Y);
  sp2 := DoublePoint(AGraphPt.X + ofs + w, AGraphPt.Y);
  if IsRotated then begin
    Exchange(sp1.X, sp1.Y);
    Exchange(sp2.X, sp2.Y);
  end;
  pt1 := ParentChart.GraphToImage(sp1);
  pt2 := ParentChart.GraphToImage(sp2);
  if IsRotated then begin
    Exchange(pt1.X, pt1.Y);
    Exchange(pt2.X, pt2.Y);
    if pt1.X > pt2.X then Exchange(pt1.X, pt2.X);
  end;

  if InRange(clickPt.X, pt1.X, pt2.X) then
    Result := sqr(clickPt.Y - pt1.Y)
  else begin
    dist1 := AParams.FDistFunc(clickPt, pt1);
    dist2 := AParams.FDistFunc(clickPt, pt2);
    Result := Min(dist1, dist2);
  end;
end;


{ TAreaSeries }

procedure TAreaSeries.Assign(ASource: TPersistent);
begin
  if ASource is TAreaSeries then
    with TAreaSeries(ASource) do begin
      Self.AreaBrush := FAreaBrush;
      Self.AreaContourPen := FAreaContourPen;
      Self.AreaLinesPen := FAreaLinesPen;
      Self.FConnectType := FConnectType;
      Self.FUseZeroLevel := FUseZeroLevel;
      Self.FZeroLevel := FZeroLevel;
    end;
  inherited Assign(ASource);
end;

constructor TAreaSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAreaBrush := TBrush.Create;
  FAreaBrush.OnChange := @StyleChanged;
  FAreaContourPen := TPen.Create;
  FAreaContourPen.OnChange := @StyleChanged;
  FAreaLinesPen := TPen.Create;
  FAreaLinesPen.OnChange := @StyleChanged;
  FStacked := true;
  FSupportsZeroLevel := true; //FUseZeroLevel;
end;

destructor TAreaSeries.Destroy;
begin
  FreeAndNil(FAreaBrush);
  FreeAndNil(FAreaContourPen);
  FreeAndNil(FAreaLinesPen);
  inherited;
end;

procedure TAreaSeries.Draw(ADrawer: IChartDrawer);
var
  pts, basePts: TPointArray;
  numPts, numBasePts: Integer;
  scaled_depth: Integer;
  missing: array of Integer;
  numMissing: Integer;
  zero: Double;
  ext, ext2: TDoubleRect;

  procedure CollectMissingItem(AIndex: Integer);
  begin
    missing[numMissing] := AIndex;
    inc(numMissing);
  end;

  { Collects the indexes of data points having NaN as x or any of the y values }
  procedure CollectMissing;
  var
    i, j: Integer;
    item: PChartDataItem;
  begin
    SetLength(missing, Length(FGraphPoints));
    numMissing := 0;
    for i := 0 to High(FGraphPoints) do begin
      item := Source.Item[i + FLoBound];
      if IsNaN(item^.X) then
        CollectMissingItem(i)
      else
      if FBanded and IsNaN(item^.Y) then
        CollectMissingItem(i)
      else
      if FStacked and (FStackedNaN = snDoNotDraw) then
        if IsNaN(item^.Y) then
          CollectMissingItem(i)
        else
          for j := 0 to Source.YCount-2 do
            if IsNaN(item^.YList[j]) then CollectMissingItem(i);
    end;
    SetLength(missing, numMissing);
  end;

  procedure PushPoint(const AP: TPoint); overload;
  begin
    if (numPts > 0) and (AP = pts[numPts - 1]) then exit;
    pts[numPts] := AP;
    numPts += 1;
  end;

  procedure PushPoint(const AP: TDoublePoint); overload;
  begin
    PushPoint(ParentChart.GraphToImage(AP));
  end;

  procedure PushBasePoint(AP: TDoublePoint; AIndex: Integer);
  var
    p: TPoint;
  begin
    p := ParentChart.GraphToImage(AP);
    if IsRotated then
      p.X := basePts[IfThen(FBanded, AIndex, 1)].X
    else
      p.Y := basePts[IfThen(FBanded, AIndex, 1)].Y;
    PushPoint(p);
  end;

  function ProjToLine(const APt: TDoublePoint; ACoord: Double): TDoublePoint;
  begin
    Result := APt;
    if IsRotated then
      Result.X := ACoord
    else
      Result.Y := ACoord;
  end;

  // Widens zero-width area to see at least a narrow stripe.
  procedure FixZeroWidth;
  var
    p1, p2, p3: TPoint;
    delta: Integer;
  begin
    delta := ADrawer.Scale(1);
    if numPts = 1 then begin
      p1 := pts[0];
      if IsRotated then begin
        dec(pts[0].Y, delta);
        inc(p1.Y, delta);
      end else begin
        dec(pts[0].X, delta);
        inc(p1.X, delta);
      end;
      PushPoint(p1);
    end else
    if numPts = 2 then begin
      p1 := pts[numpts-1];
      p2 := pts[numpts-2];
      if IsRotated and (p1.Y = p2.Y) then begin
        pts[0] := p1;
        pts[1] := p2;
        inc(p1.Y, 2*delta);
        inc(p2.Y, 2*delta);
        PushPoint(p2);
        PushPoint(p1);
      end else
      if not IsRotated and (p1.X = p2.X) then begin
        pts[0] := p1;
        pts[1] := p2;
        inc(p1.X, 2*delta);
        inc(p2.X, 2*delta);
        PushPoint(p2);
        PushPoint(p1);
      end;
    end else
    if numPts > 2 then begin
      p1 := pts[numpts-1];
      p2 := pts[numpts-2];
      p3 := pts[numpts-3];
      if IsRotated and (p1.Y = p2.Y) and (p2.Y = p3.Y) then begin
        dec(pts[numpts-3].Y, delta);
        dec(pts[numpts-2].Y, delta);
        inc(pts[numpts-1].Y, delta);
        pts[numpts-1].X := p2.X;
        inc(p3.Y, delta);
        PushPoint(p3);
      end else
      if not IsRotated and (p1.X = p2.X) and (p2.X = p3.X) then begin
        dec(pts[numpts-3].X, delta);
        dec(pts[numpts-2].X, delta);
        inc(pts[numpts-1].X, delta);
        pts[numpts-1].Y := p2.Y;
        inc(p3.X, delta);
        PushPoint(p3);
      end;
    end;
  end;

  procedure CollectPoints(AStart, AEnd: Integer);
  var
    i: Integer;
    a, b: TDoublePoint;
    singlePoint: Boolean;
  begin
    singlepoint := AStart = AEnd;
    if singlepoint then inc(AEnd);
    for i := AStart to AEnd - 1 do begin
      a := FGraphPoints[i];
      if singlePoint then b := a else b := FGraphPoints[i + 1];

      case ConnectType of
        ctLine: ;
        ctStepXY:
          if IsRotated then
            b.X := a.X
          else
            b.Y := a.Y;
        ctStepYX:
          if IsRotated then
            a.X := b.X
          else
            a.Y := b.Y;
      end;

      if IsNaN(a) and IsNaN(b) then begin
        PushBasePoint(a, i);
        if i < AEnd then PushBasePoint(b, i+1) else PushBasePoint(b, i);
      end else
      if IsNaN(b) then begin
        PushPoint(a);
        PushBasePoint(a, i);
        FixZeroWidth;
        if i < AEnd then PushBasePoint(b, i+1) else PushBasePoint(b, i);
      end else
      if IsNaN(a) then begin
        PushBasepoint(a, i);
        FixZeroWidth;
        if i < AEnd then PushBasePoint(b, i+1) else PushBasePoint(b, i);
        PushPoint(b);
      end else begin
        PushPoint(a);
        PushPoint(b);
      end;
    end;
    FixZeroWidth;
  end;

  procedure CopyPoints(var ADest: TPointArray; ASource: TPointArray;
    ANumPts: Integer);
  var
    i: Integer;
  begin
    for i:=0 to ANumPts - 1 do
      ADest[i] := ASource[i];
  end;

  procedure DrawSegment(AStart, AEnd: Integer);
  var
    numDataPts: Integer;
    p: TDoublePoint;
    i, j, j0: Integer;
    zeroPt: TPoint;
    c: TColor;
  begin
    // Get baseline of area series: this is the curve of the 1st y value in case
    // of banded, or the zero level in case for normal area series.
    if FBanded then begin
      UpdateGraphPoints(-1, FLoBound, FUpBound, FStacked);
      numPts := 0;
      CollectPoints(AStart, AEnd);
      numBasePts := numPts;
    end else begin
      numPts := 0;
      p := ProjToRect(FGraphPoints[AStart], ext2);
      PushPoint(ProjToLine(p, zero));
      p := ProjToRect(FGraphPoints[AEnd], ext2);
      PushPoint(ProjToLine(p, zero));
      FixZeroWidth;
      numBasePts := numPts;
    end;
    SetLength(basePts, numBasePts);
    CopyPoints(basePts, pts, numBasePts);

    // Iterate through y values
    j0 := IfThen(FBanded and (Source.YCount > 1), 0, -1);
    for j := Source.YCount - 2 downto j0 do begin
      // Stack level points
      numPts := 0;
      UpdateGraphPoints(j, FLoBound, FUpBound, FStacked);
      CollectPoints(AStart, AEnd);
      numDataPts := numPts;

      // Base points
      for i:=numBasePts-1 downto 0 do
        PushPoint(basepts[i]);

      // Prepare painting
      ADrawer.Brush := AreaBrush;
      ADrawer.Pen := AreaContourPen;
      if Styles <> nil then
        Styles.Apply(ADrawer, j - j0);

      // Draw 3D sides
      // Note: Rendering is often incorrect, e.g. when values cross zero level
      // or when values are not stacked!
      if (Depth > 0) then begin
        c := ADrawer.BrushColor;
        ADrawer.BrushColor := GetDepthColor(c);
        // Top sides
        if (Source.YCount = 1) or (not FStacked) or (j = Source.YCount-2) then
          for i := 0 to numDataPts-2 do
            ADrawer.DrawLineDepth(pts[i], pts[i+1], scaled_depth);
        // Sides at the right
        ADrawer.DrawLineDepth(pts[numdataPts-1], pts[numDataPts], scaled_depth);
        ADrawer.BrushColor := c;
      end;

      // Fill polygon of current level
      ADrawer.Polygon(pts, 0, numPts);

      // Draw drop-lines
      if AreaLinesPen.Style <> psClear then begin
        if FBanded and (j > -1) then begin
          ADrawer.Pen := AreaLinesPen;
          for i := 1 to numDataPts-2 do
            ADrawer.Line(pts[i], pts[numpts - 1 - i]);
        end else
        if not FBanded then begin
          ADrawer.Pen := AreaLinesPen;
          zeroPt := pts[numDataPts];
          for i := 1 to numDataPts-2 do begin
            if IsRotated then zeroPt.Y := pts[i].Y else zeroPt.X := pts[i].X;
            ADrawer.Line(pts[i], zeroPt);
          end;
        end;
      end;
    end;
  end;

var
  j, k: Integer;
begin
  if IsEmpty then exit;

  ext := ParentChart.CurrentExtent;
  ext2 := ext;
  ExpandRange(ext2.a.X, ext2.b.X, 0.1);
  ExpandRange(ext2.a.Y, ext2.b.Y, 0.1);

  PrepareGraphPoints(ext, true);
  if Length(FGraphPoints) = 0 then
    exit;

  if UseZeroLevel then
    zero := AxisToGraphY(ZeroLevel)
  else
    zero := IfThen(IsRotated, ext2.a.X, ext2.a.Y);
  scaled_depth := ADrawer.Scale(Depth);
  SetLength(pts, Length(FGraphPoints) * 4 + 4);

  CollectMissing;
  if Length(missing) = 0 then
    DrawSegment(0, High(FGraphPoints))
  else begin
    j := 0;
    k := 0;
    while j < Length(missing) do begin
      while (missing[j] = k) do begin
        inc(k);
        inc(j);
        if j = Length(missing) then
          break;
      end;
      if j <= High(missing) then begin
        DrawSegment(k, missing[j]-1);
        k := missing[j]+1;
      end else
        DrawSegment(k, High(FGraphPoints));
      inc(j);
    end;
    if k <= High(FGraphPoints) then
      DrawSegment(k, High(FGraphPoints));
  end;

  DrawLabels(ADrawer);
end;

function TAreaSeries.Extent: TDoubleRect;
begin
  Result := inherited Extent;
  if not IsEmpty and UseZeroLevel then
    UpdateMinMax(GraphToAxisY(ZeroLevel), Result.a.Y, Result.b.Y);
end;

procedure TAreaSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  GetLegendItemsRect(AItems, AreaBrush);
end;

function TAreaSeries.GetSeriesColor: TColor;
begin
  Result := FAreaBrush.Color;
end;

function TAreaSeries.GetZeroLevel: Double;
begin
  Result := IfThen(UseZeroLevel, ZeroLevel, 0.0);
end;

function TAreaSeries.IsZeroLevelStored: boolean;
begin
  Result := ZeroLevel <> 0.0;
end;

procedure TAreaSeries.SetAreaBrush(AValue: TBrush);
begin
  FAreaBrush.Assign(AValue);
  UpdateParentChart;
end;

procedure TAreaSeries.SetAreaContourPen(AValue: TPen);
begin
  FAreaContourPen.Assign(AValue);
  UpdateParentChart;
end;

procedure TAreaSeries.SetAreaLinesPen(AValue: TPen);
begin
  FAreaLinesPen.Assign(AValue);
  UpdateParentChart;
end;

procedure TAreaSeries.SetBanded(AValue: Boolean);
begin
  if FBanded = AValue then exit;
  FBanded := AValue;
  UpdateParentChart;
end;

procedure TAreaSeries.SetConnectType(AValue: TConnectType);
begin
  if FConnectType = AValue then exit;
  FConnectType := AValue;
  UpdateParentChart;
end;

procedure TAreaSeries.SetSeriesColor(AValue: TColor);
begin
  FAreaBrush.Color := AValue;
end;

procedure TAreaSeries.SetUseZeroLevel(AValue: Boolean);
begin
  if FUseZeroLevel = AValue then exit;
  FUseZeroLevel := AValue;
//  FSupportsZeroLevel := FUseZeroLevel;
  UpdateParentChart;
end;

procedure TAreaSeries.SetZeroLevel(AValue: Double);
begin
  if FZeroLevel = AValue then exit;
  FZeroLevel := AValue;
  UpdateParentChart;
end;

function TAreaSeries.SkipMissingValues(AIndex: Integer): Boolean;
begin
  Result := inherited;
  if not Result then
    Result := FBanded and IsNaN(Source.Item[AIndex]^.Y);
end;


{ TUserDrawnSeries }

procedure TUserDrawnSeries.Assign(ASource: TPersistent);
begin
  if ASource is TUserDrawnSeries then
    with TUserDrawnSeries(ASource) do begin
      Self.FOnDraw := FOnDraw;
      Self.FOnGetBounds := FOnGetBounds;
    end;
  inherited Assign(ASource);
end;

procedure TUserDrawnSeries.Draw(ADrawer: IChartDrawer);
var
  ic: IChartTCanvasDrawer;
begin
  if Supports(ADrawer, IChartTCanvasDrawer, ic) and Assigned(FOnDraw) then
    FOnDraw(ic.Canvas, FChart.ClipRect);
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

procedure SkipObsoleteProperties;
const
  STAIRS_NOTE = 'Obsolete, use ConnectType instead';
  DRAWPOINTER_NOTE = 'Obsolete, use OnCustomDrawPointer instead';
begin
  RegisterPropertyEditor(
    TypeInfo(TChartAxisIndex), TConstantLine, 'AxisIndexX', THiddenPropertyEditor);
  RegisterPropertyToSkip(TAreaSeries, 'Stairs', STAIRS_NOTE, '');
  RegisterPropertyToSkip(TAreaSeries, 'InvertedStairs', STAIRS_NOTE, '');
  RegisterPropertyToSkip(TLineSeries, 'OnDrawPointer', DRAWPOINTER_NOTE, '');
end;

initialization
  RegisterSeriesClass(TLineSeries, @rsLineSeries);
  RegisterSeriesClass(TAreaSeries, @rsAreaSeries);
  RegisterSeriesClass(TBarSeries, @rsBarSeries);
  RegisterSeriesClass(TPieSeries, @rsPieSeries);
  RegisterSeriesClass(TUserDrawnSeries, @rsUserDrawnSeries);
  RegisterSeriesClass(TConstantLine, @rsConstantLine);
  RegisterSeriesClass(TManhattanSeries, @rsManhattanPlotSeries);
//  {$WARNINGS OFF}RegisterSeriesClass(TLine, nil);{$WARNINGS ON}
  SkipObsoleteProperties;

end.
