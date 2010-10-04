{
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

Authors: Alexander Klenin

}

unit TAMultiSeries;

{$H+}

interface

uses
  Classes, Graphics,
  TAChartUtils, TACustomSeries, TALegend;

const
  DEF_BOX_WIDTH = 50;
  DEF_WHISKERS_WIDTH = 25;

type

  TBubbleRadiusTransform = (brtNone, brtX, brtY);

  { TBubbleSeries }

  TBubbleSeries = class(TBasicPointSeries)
  private
    FBubbleBrush: TBrush;
    FBubblePen: TPen;
    procedure SetBubbleBrush(AValue: TBrush);
    procedure SetBubblePen(AValue: TPen);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetSeriesColor: TColor; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Draw(ACanvas: TCanvas); override;
    function Extent: TDoubleRect; override;
  published
    property AxisIndexX;
    property AxisIndexY;
    property BubbleBrush: TBrush read FBubbleBrush write SetBubbleBrush;
    property BubblePen: TPen read FBubblePen write SetBubblePen;
    property Source;
  end;

  { TBoxAndWhiskerSeries }

  TBoxAndWhiskerSeries = class(TBasicPointSeries)
  private
    FBoxBrush: TBrush;
    FBoxPen: TPen;
    FBoxWidth: Integer;
    FMedianPen: TPen;
    FWhiskersPen: TPen;
    FWhiskersWidth: Integer;
    procedure SetBoxBrush(AValue: TBrush);
    procedure SetBoxPen(AValue: TPen);
    procedure SetBoxWidth(AValue: Integer);
    procedure SetMedianPen(AValue: TPen);
    procedure SetWhiskersPen(AValue: TPen);
    procedure SetWhiskersWidth(AValue: Integer);
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    function GetSeriesColor: TColor; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Draw(ACanvas: TCanvas); override;
    function Extent: TDoubleRect; override;
  published
    property BoxBrush: TBrush read FBoxBrush write SetBoxBrush;
    property BoxPen: TPen read FBoxPen write SetBoxPen;
    property BoxWidth: Integer
      read FBoxWidth write SetBoxWidth default DEF_BOX_WIDTH;
    property MedianPen: TPen read FMedianPen write SetMedianPen;
    property WhiskersPen: TPen read FWhiskersPen write SetWhiskersPen;
    property WhiskersWidth: Integer
      read FWhiskersWidth write SetWhiskersWidth default DEF_WHISKERS_WIDTH;
  published
    property AxisIndexX;
    property AxisIndexY;
    property Source;
  end;

implementation

uses
  Math, SysUtils, TAGraph;

{ TBubbleSeries }

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

procedure TBubbleSeries.Draw(ACanvas: TCanvas);
var
  i: Integer;
  pt, d: TPoint;
  r: Double;
begin
  if Source.YCount < 2 then exit;
  r := 0;
  for i := 0 to Count - 1 do
    r := Max(Source[i]^.YList[0], r);
  with ParentChart.CurrentExtent do
    PrepareGraphPoints(DoubleRect(a.X - r, a.Y - r, b.X + r, b.Y + r), true);
  ACanvas.Pen.Assign(BubblePen);
  ACanvas.Brush.Assign(BubbleBrush);
  for i := 0 to High(FGraphPoints) do begin
    pt := ParentChart.GraphToImage(FGraphPoints[i]);
    r := Source[i + FLoBound]^.YList[0];
    d.X := ParentChart.XGraphToImage(r) - ParentChart.XGraphToImage(0);
    d.Y := ParentChart.YGraphToImage(r) - ParentChart.YGraphToImage(0);
    ACanvas.EllipseC(pt.X, pt.Y, d.X, d.Y);
  end;
  DrawLabels(ACanvas);
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

{ TBoxAndWhiskerSeries }

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
  inherited Destroy;
  FreeAndNil(FBoxBrush);
  FreeAndNil(FBoxPen);
  FreeAndNil(FMedianPen);
  FreeAndNil(FWhiskersPen);
end;

procedure TBoxAndWhiskerSeries.Draw(ACanvas: TCanvas);

  function MaybeRotate(AX, AY: Double): TPoint;
  begin
    if IsRotated then
      Exchange(AX, AY);
    Result := ParentChart.GraphToImage(DoublePoint(AX, AY));
  end;

  procedure DoLine(AX1, AY1, AX2, AY2: Double);
  begin
    ACanvas.Line(MaybeRotate(AX1, AY1), MaybeRotate(AX2, AY2));
  end;

  procedure DoRect(AX1, AY1, AX2, AY2: Double);
  var
    r: TRect;
  begin
    with ParentChart do begin
      r.TopLeft := MaybeRotate(AX1, AY1);
      r.BottomRight := MaybeRotate(AX2, AY2);
    end;
    ACanvas.Rectangle(r);
  end;

var
  ext2: TDoubleRect;
  x, ymin, yqmin, ymed, yqmax, ymax, wb, ww, w: Double;
  i: Integer;
begin
  if IsEmpty or (Source.YCount < 5) then exit;

  ext2 := ParentChart.CurrentExtent;
  ExpandRange(ext2.a.X, ext2.b.X, 1.0);
  ExpandRange(ext2.a.Y, ext2.b.Y, 1.0);

  PrepareGraphPoints(ext2, true);

  for i := FLoBound to FUpBound do begin
    x := GetGraphPointX(i);
    ymin := GetGraphPointY(i);
    with Source[i]^ do begin
      yqmin := AxisToGraphY(YList[0]);
      ymed := AxisToGraphY(YList[1]);
      yqmax := AxisToGraphY(YList[2]);
      ymax := AxisToGraphY(YList[3]);
    end;
    w := GetXRange(x, i) * PERCENT / 2;
    wb := w * BoxWidth;
    ww := w * WhiskersWidth;

    ACanvas.Pen := WhiskersPen;
    ACanvas.Brush.Style := bsClear;
    DoLine(x - ww, ymin, x + ww, ymin);
    DoLine(x, ymin, x, yqmin);
    DoLine(x - ww, ymax, x + ww, ymax);
    DoLine(x, ymax, x, yqmax);
    ACanvas.Pen := BoxPen;
    ACanvas.Brush:= BoxBrush;
    DoRect(x - wb, yqmin, x + wb, yqmax);
    ACanvas.Pen := MedianPen;
    ACanvas.Brush.Style := bsClear;
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
  Result.a.X := Min(Result.a.X, x - ExtraWidth(0));
  x := GetGraphPointX(Count - 1);
  Result.b.X := Max(Result.b.X, x + ExtraWidth(Count - 1));
end;

procedure TBoxAndWhiskerSeries.GetLegendItems(AItems: TChartLegendItems);
begin
  GetLegendItemsRect(AItems, BoxBrush);
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

initialization
  RegisterSeriesClass(TBubbleSeries, 'Bubble series');
  RegisterSeriesClass(TBoxAndWhiskerSeries, 'Box-and-whiskers series');

end.
