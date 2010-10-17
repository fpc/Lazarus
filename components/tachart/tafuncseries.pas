{

 Function series for TAChart.

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
unit TAFuncSeries;

{$H+}

interface

uses
  Classes, Graphics,
  TAChartUtils, TACustomSeries, TALegend, TATypes;

type
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

implementation

uses
  Math, SysUtils, TAGraph;

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

initialization
  RegisterSeriesClass(TFuncSeries, 'Function series');

end.

