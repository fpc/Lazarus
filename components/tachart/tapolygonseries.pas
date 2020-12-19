unit TAPolygonSeries;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  TAChartUtils, TADrawUtils, TACustomSeries;

type
  TPolygonSeries = class(TBasicPointSeries)
  private
    FBrush: TBrush;
    FPen: TPen;
    FPoints: array of TPoint;
    FStart: array of Integer;
    procedure SetBrush(AValue: TBrush);
    procedure SetPen(AValue: TPen);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(ADrawer: IChartDrawer); override;
    function GetNearestPoint(const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;

  published
    property Brush: TBrush read FBrush write SetBrush;
    property Pen: TPen read FPen write SetPen;
    property ToolTargets default [nptPoint, nptCustom];

    // inherited
    property AxisIndexX;
    property AxisIndexY;
    property Source;
  end;

implementation

uses
  TAChartStrConsts, TAGeometry, TAGraph;

constructor TPolygonSeries.Create(AOwner: TComponent);
begin
  inherited;
  ToolTargets := [nptPoint, nptCustom];

  FPen := TPen.Create;
  FPen.OnChange := @StyleChanged;
  FBrush := TBrush.Create;
  FBrush.OnChange := @StyleChanged;
end;

destructor TPolygonSeries.Destroy;
begin
  FPoints := nil;
  FStart := nil;
  FPen.Free;
  FBrush.Free;
  inherited;
end;

procedure TPolygonSeries.Draw(ADrawer: IChartDrawer);
const
  START_BLOCKSIZE = 100;
var
  ext: TDoubleRect;
  ap, apStart: TDoublePoint;  // axis units
  gp: TDoublePoint;           // graph units
  pt: TPoint;                 // image units
  nSource, nPoints: Integer;
  nStart: Integer;
  newPolygon: Boolean;
  i: Integer;

  procedure SavePolygonStartIndex(AIndex: Integer);
  begin
    if nStart mod START_BLOCKSIZE = 0 then
      SetLength(FStart, Length(FStart) + START_BLOCKSIZE);
    FStart[nStart] := AIndex;
    inc(nStart);
  end;

begin
  if IsEmpty or (not Active) then
    exit;

  // Do not draw anything if the series extent does not intersect CurrentExtent.
  with Extent do begin
    ext.a := AxisToGraph(a);
    ext.b := AxisToGraph(b);
  end;
  NormalizeRect(ext);
  if not RectIntersectsRect(ext, ParentChart.CurrentExtent) then
    exit;

  nSource := Source.Count;
  SetLength(FPoints, nSource);
  SetLength(FStart, START_BLOCKSIZE);
  nStart := 0;
  nPoints := 0;
  newPolygon := true;

  for i := 0 to nSource-1 do begin
    // a new polygon begins with the current index --> store index in array FStart[].
    if newPolygon then begin
      SavePolygonStartIndex(nPoints);

      // Since the polygon ends if the start point is met again we must store the start point.
      apStart := Source.Item[i]^.Point;
    end;

    // Get polygon point in image coordinates
    ap := Source.Item[i]^.Point;
    if IsRotated then
      gp := DoublePoint(AxisToGraphX(ap.Y), AxisToGraphY(ap.X))
    else
      gp := AxisToGraph(ap);
    pt := ParentChart.GraphToImage(gp);

    // Store image point in FPoints array
    FPoints[nPoints] := pt;
    inc(nPoints);

    // Current point coincides with the polygon start point
    // --> polygon is closed, set flag "newPolygon" to be processed in next iteration
    if (ap = apStart) then
      newPolygon := not newPolygon;
  end;

  if nPoints = 0 then
    exit;

  // Use length of FStart array as last array element for easier calculation
  SavePolygonStartIndex(nPoints);

  // Trim length of FStart array to occupied length
  SetLength(FStart, nStart);

  // Draw polygon(s)
  ADrawer.SetBrush(FBrush);
  ADrawer.SetPen(FPen);
  for i := 0 to High(FStart)-1 do
    ADrawer.Polygon(FPoints, FStart[i], FStart[i+1] - FStart[i]);

end;

{ Is overridden in order to detect tool events inside the polygon (nptCustom).
  Otherwise only events on the perimenter would be detected. }
function TPolygonSeries.GetNearestPoint(const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  ext: TDoubleRect;
begin
  Result := false;
  AResults.FDist := sqr(AParams.FRadius) + 1;
  AResults.FIndex := -1;
  AResults.FXIndex := 0;
  AResults.FYIndex := 0;

  Result := inherited;

  if Result or not ((nptCustom in AParams.FTargets) and (nptCustom in ToolTargets)) then
    exit;

  with Extent do begin
    ext.a := AxisToGraph(a);
    ext.b := AxisToGraph(b);
  end;
  NormalizeRect(ext);
  // Do not do anything if the series extent does not intersect CurrentExtent.
  if not RectIntersectsRect(ext, ParentChart.CurrentExtent) then
    exit;

  Result := IsPointInPolygon(AParams.FPoint, FPoints);
  if Result then
  begin
    AResults.FDist := 0;
    AResults.FIndex := 0;
    AResults.FYIndex := 0;
    AResults.FValue := ParentChart.ImageToGraph(AParams.FPoint);
    AResults.FImg := AParams.FPoint;
  end;
end;


procedure TPolygonSeries.SetBrush(AValue: TBrush);
begin
  FBrush.Assign(AValue);
  UpdateParentChart;
end;

procedure TPolygonSeries.SetPen(AValue: TPen);
begin
  FPen.Assign(AValue);
  UpdateParentChart;
end;


initialization
  RegisterSeriesClass(TPolygonSeries, @rsPolygonSeries);

end.

