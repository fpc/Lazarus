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

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics,
  TAChartUtils, TACustomSeries, TALegend;

type

  TBubbleRadiusTransform = (brtNone, brtX, brtY);

  { TBubbleSeries }

  TBubbleSeries = class(TBasicPointSeries)
  private
    FBubbleBrush: TBrush;
    FBubblePen: TPen;
    procedure SetBubbleBrush(const AValue: TBrush);
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
  AItems.Add(TLegendItemBrushRect.Create(BubbleBrush, Title));
end;

function TBubbleSeries.GetSeriesColor: TColor;
begin
  Result := FBubbleBrush.Color;
end;

procedure TBubbleSeries.SetBubbleBrush(const AValue: TBrush);
begin
  if FBubbleBrush = AValue then exit;
  FBubbleBrush := AValue;
  UpdateParentChart;
end;

procedure TBubbleSeries.SetBubblePen(AValue: TPen);
begin
  if FBubblePen = AValue then exit;
  FBubblePen := AValue;
  UpdateParentChart;
end;

initialization
  RegisterSeriesClass(TBubbleSeries, 'Bubble series');

end.
