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

  TBarWidthStyle = (bwPercent, bwPercentMin);

  TBarSeries = class;

  TBeforeDrawBarEvent = procedure (
    ASender: TBarSeries; ACanvas: TCanvas; const ARect: TRect;
    APointIndex, AStackIndex: Integer; var ADoDefaultDrawing: Boolean
  ) of object;

  { TBarSeries }

  TBarSeries = class(TBasicPointSeries)
  private
    FBarBrush: TBrush;
    FBarOffsetPercent: Integer;
    FBarPen: TPen;
    FBarWidthPercent: Integer;
    FBarWidthStyle: TBarWidthStyle;
    FOnBeforeDrawBar: TBeforeDrawBarEvent;
    FZeroLevel: Double;

    function IsZeroLevelStored: boolean;
    procedure SetBarBrush(Value: TBrush);
    procedure SetBarOffsetPercent(AValue: Integer);
    procedure SetBarPen(Value: TPen);
    procedure SetBarWidthPercent(Value: Integer);
    procedure SetBarWidthStyle(AValue: TBarWidthStyle);
    procedure SetOnBeforeDrawBar(AValue: TBeforeDrawBarEvent);
    procedure SetSeriesColor(AValue: TColor);
    procedure SetZeroLevel(AValue: Double);
  strict protected
    function GetLabelDataPoint(AIndex: Integer): TDoublePoint; override;
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
    function GetBarWidth(AIndex: Integer): Integer;
    procedure Draw(ADrawer: IChartDrawer); override;
    function Extent: TDoubleRect; override;
    function GetNearestPoint(const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
  published
    property AxisIndexX;
    property AxisIndexY;
    property BarBrush: TBrush read FBarBrush write SetBarBrush;
    property BarOffsetPercent: Integer
      read FBarOffsetPercent write SetBarOffsetPercent default 0;
    property BarPen: TPen read FBarPen write SetBarPen;
    property BarWidthPercent: Integer
      read FBarWidthPercent write SetBarWidthPercent default DEF_BAR_WIDTH_PERCENT;
    property BarWidthStyle: TBarWidthStyle
      read FBarWidthStyle write SetBarWidthStyle default bwPercent;
    property Depth;
    property MarkPositions;
    property SeriesColor: TColor
      read GetSeriesColor write SetSeriesColor stored false default clRed;
    property Source;
    property Stacked default true;
    property Styles;
    property ToolTargets default [nptPoint, nptYList, nptCustom];
    property UseReticule;
    property ZeroLevel: Double
      read FZeroLevel write SetZeroLevel stored IsZeroLevelStored;
  published
    property OnBeforeDrawBar: TBeforeDrawBarEvent
      read FOnBeforeDrawBar write SetOnBeforeDrawBar;
  end;


  { TPieSeries }

  TPieSeries = class(TCustomPieSeries)
  public
    property Radius;
  published
    property EdgePen;
    property Depth;
    property Exploded;
    property FixedRadius;
    property MarkDistancePercent;
    property MarkPositions;
    property RotateLabels;
    property Source;
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
    property MarkPositions;
    property SeriesColor: TColor
      read GetSeriesColor write SetSeriesColor stored false default clWhite;
    property Source;
    property Stacked default true;
    property Styles;
    property ToolTargets;
    property UseReticule;
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
    property LinePen: TPen read FLinePen write SetLinePen;
    property LineType: TLineType
      read FLineType write SetLineType default ltFromPrevious;
    property MarkPositions;
    property Pointer;
    property SeriesColor: TColor
      read GetSeriesColor write SetSeriesColor stored false default clBlack;
    property ShowLines: Boolean
      read GetShowLines write SetShowLines stored false default true;
    property ShowPoints: Boolean
      read GetShowPoints write SetShowPoints default false;
    property Stacked default false;
    property Source;
    property Styles;
    property ToolTargets;
    property UseReticule default true;
    property XErrorBars;
    property YErrorBars;
    // Events
    property OnDrawPointer: TSeriesPointerDrawEvent
      read FOnDrawPointer write FOnDrawPointer; deprecated 'Use OnCustomDrawPointer';
    property OnCustomDrawPointer;
    property OnGetPointerStyle;
  end;

  // 'TSerie' alias is for compatibility with older versions of TAChart.
  // Use TLineSeries instead.
  TSerie = TLineSeries deprecated;

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
    property UseReticule;
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
    property AxisIndexX write SetAxisIndex; deprecated 'Use AxisIndex';
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

  // 'TLine' alias is for compatibility with older versions of TAChart.
  // Use TConstantLine instead.
  TLine = class(TConstantLine) end deprecated;

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
  TAChartStrConsts, TADrawerCanvas, TAGeometry, TAGraph, TAMath, TAStyles;

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
  SetPropDefaults(Self, ['LineType', 'UseReticule']);
end;

destructor TLineSeries.Destroy;
begin
  FreeAndNil(FLinePen);
  inherited;
end;

procedure TLineSeries.Draw(ADrawer: IChartDrawer);
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
  DrawSingleLineInStack(ADrawer, 0);
  for i := 0 to Source.YCount - 2 do begin
    if Source.IsYErrorIndex(i+1) then Continue;
    UpdateGraphPoints(i, FStacked);
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
        ADrawer.SetBrushParams(bsSolid, LinePen.Color);
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
  DrawErrorBars(ADrawer);
  DrawLabels(ADrawer);
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

  procedure DrawBar(const AR: TRect);
  var
    sz: TSize;
    defaultDrawing: Boolean = true;
    c: TColor;
    ic: IChartTCanvasDrawer;
  begin
    ADrawer.Brush := BarBrush;
    ADrawer.Pen := BarPen;
    if Styles <> nil then
      Styles.Apply(ADrawer, stackIndex);
    c := Source[pointIndex]^.Color;
    if c <> clTAColor then
      ADrawer.BrushColor := c;
    sz := Size(AR);
    if (sz.cx <= 2) or (sz.cy <= 2) then begin
      // Bars are too small to distinguish the border from the interior.
      ADrawer.SetPenParams(psSolid, ADrawer.BrushColor);
    end;

    if Supports(ADrawer, IChartTCanvasDrawer, ic) and Assigned(OnBeforeDrawBar) then
      OnBeforeDrawBar(Self, ic.Canvas, AR, pointIndex, stackIndex, defaultDrawing);
    if not defaultDrawing then exit;

    ADrawer.Rectangle(AR);

    if Depth = 0 then exit;

    ADrawer.DrawLineDepth(AR.Left, AR.Top, AR.Right - 1, AR.Top, scaled_depth);
    ADrawer.DrawLineDepth(
      AR.Right - 1, AR.Top, AR.Right - 1, AR.Bottom - 1, scaled_depth);
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
begin
  if IsEmpty then exit;

  if BarWidthStyle = bwPercentMin then
    UpdateMinXRange;
  ext2 := ParentChart.CurrentExtent;
  ExpandRange(ext2.a.X, ext2.b.X, 1.0);
  ExpandRange(ext2.a.Y, ext2.b.Y, 1.0);

  scaled_depth := ADrawer.Scale(Depth);

  PrepareGraphPoints(ext2, true);
  SetLength(heights, Source.YCount + 1);
  for pointIndex := FLoBound to FUpBound do begin
    p := Source[pointIndex]^.Point;
    if IsNan(p.X) then continue;
    p.X := AxisToGraphX(p.X);
    BarOffsetWidth(p.X, pointIndex, ofs, w);
    p.X += ofs;
    heights[0] := ZeroLevel;
    if FStacked then begin
      heights[1] := NumberOr(p.Y, ZeroLevel);
      for stackIndex := 1 to Source.YCount - 1 do begin
        y := Source[pointIndex]^.YList[stackIndex - 1];
        if not IsNan(y) then
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
          heights[stackIndex + 1] := y
        else
          heights[stackIndex + 1] := 0;
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
  UpdateMinMax(ZeroLevel, Result.a.Y, Result.b.Y);
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

function TBarSeries.GetLabelDataPoint(AIndex: Integer): TDoublePoint;
var
  ofs, w: Double;
begin
  Result := inherited GetLabelDataPoint(AIndex);
  BarOffsetWidth(TDoublePointBoolArr(Result)[IsRotated], AIndex, ofs, w);
  TDoublePointBoolArr(Result)[IsRotated] += ofs;
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
  Result := ZeroLevel;
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

procedure TBarSeries.SetSeriesColor(AValue: TColor);
begin
  FBarBrush.Color := AValue;
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
  pts: TPointArray;
  numPts: Integer;
  scaled_depth: Integer;

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

  function ProjToLine(const APt: TDoublePoint; ACoord: Double): TDoublePoint;
  begin
    Result := APt;
    if IsRotated then
      Result.X := ACoord
    else
      Result.Y := ACoord;
  end;

var
  ext, ext2: TDoubleRect;
  prevPts: TPointArray;

  procedure CollectPoints(AStart, AEnd: Integer);
  var
    i: Integer;
    a, b: TDoublePoint;
  begin
    for i := AStart to AEnd - 1 do begin
      a := FGraphPoints[i];
      b := FGraphPoints[i + 1];
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
      if not IsRotated then begin
        PushPoint(a);
        PushPoint(b);
      end else begin
        PushPoint(a);
        PushPoint(b);
      end
    end;
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
    i, j, j0, n2, numPrevPts, numSavedPts: Integer;
    a, b: TDoublePoint;
    a0, b0: TDoublePoint;
    z, z1, z2: Double;
  begin
    numPts := 0;

    if UseZeroLevel then
      z := AxisToGraphY(ZeroLevel)
    else
      z := IfThen(IsRotated, ext2.a.X, ext2.a.Y);
    z1 := z;
    z2 := z;

    a0 := FGraphPoints[AStart];
    b0 := FGraphPoints[AEnd];

    // Collect points of top-most curve
    UpdateGraphPoints(Source.YCount-2, AStart, AEnd, FStacked);
      // Index (1st parameter) refers to YList --> one less than usual!)
    numPts := 0;
    CollectPoints(AStart, AEnd);
    CopyPoints(prevPts, pts, numPts);
    numPrevPts := numPts;
    numSavedPts := numPts;

    // Collect points of lower end of each level
    j0 := IfThen(FBanded and (Source.YCount > 1), 0, -1);
    for j := Source.YCount - 2 downto j0 do begin
      numPts := 0;

      if (j > -1) then begin
        // Stack level points
        UpdateGraphPoints(j - 1, AStart, AEnd, FStacked);
        CollectPoints(AStart, AEnd);
        numSavedPts := numPts;
        for i := 0 to numPrevPts-1 do
          PushPoint(prevPts[numPrevPts - 1 - i]);
        CopyPoints(prevPts, pts, numSavedPts);
        numPrevPts := numSavedPts;
      end else
      begin
        // Zerolevel points
        a := ProjToRect(a0, ext2);
        PushPoint(ProjToLine(a, z1));
        z1 := IfThen(IsRotated, a.X, a.Y);

        for i:=0 to numPrevPts - 1 do
          PushPoint(prevPts[i]);

        b := ProjToRect(b0, ext2);
        PushPoint(ProjToLine(b, z2));
        a := ProjToRect(FGraphPoints[AEnd], ext2);
        z2 := IfThen(IsRotated, a.X, a.Y);
      end;
      n2 := numPts;

      // Prepare painting
      ADrawer.Brush := AreaBrush;
      ADrawer.Pen := AreaContourPen;
      if Styles <> nil then
        Styles.Apply(ADrawer, j - j0);

      // Draw 3D sides
      // Note: Rendering is often incorrect, e.g. when values cross zero level!
      if (Depth > 0) then begin
        if (Source.YCount = 1) or ((not FStacked) and (j = -1)) then
          for i := 1 to numSavedPts do
            ADrawer.DrawlineDepth(pts[i], pts[i+1], scaled_depth)
        else
        if ((not FStacked) or (j = Source.YCount-2)) then
          for i:= n2-2 downto n2 - numSavedPts - 1 do
            ADrawer.DrawLineDepth(pts[i], pts[i + 1], scaled_depth)
        else
        if FStacked then begin
          if (j > -1) then
            ADrawer.DrawLineDepth(pts[numSavedPts - 1], pts[numSavedPts], scaled_depth)
          else
          if (j = -1) and (FStacked or FBanded) then
            ADrawer.DrawLineDepth(pts[numSavedPts], pts[numSavedPts+1], scaled_depth);
        end;
      end;

      // Fill area
      ADrawer.Polygon(pts, 0, numPts);

      // Draw droplines
      if (AreaLinesPen.Style <> psClear) and (j > -1) then begin
        ADrawer.Pen := AreaLinesPen;
        for i := 1 to numPrevPts-2 do
          ADrawer.Line(pts[i], pts[numpts - 1 - i]);
      end;
    end;

    // Drop lines of lowest layer
    if (AreaLinesPen.Style <> psClear) and (not FBanded) then begin
      ADrawer.Pen := AreaLinesPen;
      for i := AStart + 1 to AEnd - 1 do begin
        a := ProjToRect(FGraphPoints[i], ext2);
        b := ProjToLine(a, z);
        ADrawer.Line(ParentChart.GraphToImage(a), ParentChart.GraphToImage(b));
      end;
    end;
  end;

var
  i, j: Integer;
begin
  if IsEmpty then exit;

  ext := ParentChart.CurrentExtent;
  ext2 := ext;
  ExpandRange(ext2.a.X, ext2.b.X, 0.1);
  ExpandRange(ext2.a.Y, ext2.b.Y, 0.1);

  PrepareGraphPoints(ext, true);
  if Length(FGraphPoints) = 0 then exit;

  scaled_depth := ADrawer.Scale(Depth);

  SetLength(pts, Length(FGraphPoints) * 4 + 4);
  SetLength(prevPts, Length(pts));

  j := -1;
  for i := 0 to High(FGraphPoints) do
    if IsNan(FGraphPoints[i]) = (j >= 0) then
      if j >= 0 then begin
        DrawSegment(j, i - 1);
        j := -1;
      end
      else
        j := i;
  if j >= 0 then
    DrawSegment(j, High(FGraphPoints));
  DrawLabels(ADrawer);
end;

function TAreaSeries.Extent: TDoubleRect;
begin
  Result := inherited Extent;
  if not IsEmpty and UseZeroLevel then
    UpdateMinMax(ZeroLevel, Result.a.Y, Result.b.Y);
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
  UpdateParentChart;
end;

procedure TAreaSeries.SetZeroLevel(AValue: Double);
begin
  if FZeroLevel = AValue then exit;
  FZeroLevel := AValue;
  UpdateParentChart;
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
begin
  RegisterPropertyEditor(
    TypeInfo(Boolean), TLineSeries, 'ShowLines', THiddenPropertyEditor);
  RegisterPropertyToSkip(TAreaSeries, 'Stairs', STAIRS_NOTE, '');
  RegisterPropertyToSkip(TAreaSeries, 'InvertedStairs', STAIRS_NOTE, '');
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
