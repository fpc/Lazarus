{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}
unit TALegendPanel;

{$H+}

interface

uses
  Classes, Controls, TAChartUtils, TAGraph;

type

  { TChartLegendPanel }

  TChartLegendPanel = class(TCustomControl)
  private
    FChart: TChart;
    FListener: TListener;
    procedure ChartChanged(ASender: TObject);
    procedure SetChart(AValue: TChart);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Chart: TChart read FChart write SetChart;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Visible;
  end;

procedure Register;

implementation

uses
  SysUtils;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartLegendPanel]);
end;

{ TChartLegendPanel }

procedure TChartLegendPanel.ChartChanged(ASender: TObject);
begin
  // TODO: Do not auto-update on chart zooming/scrolling.
  Unused(ASender);
  Invalidate;
end;

constructor TChartLegendPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListener := TListener.Create(@FChart, @ChartChanged);
  Width := 40;
  Height := 20;
end;

destructor TChartLegendPanel.Destroy;
begin
  FreeAndNil(FListener);
  inherited Destroy;
end;

procedure TChartLegendPanel.Paint;
var
  r: TRect;
begin
  if Chart = nil then exit;
  r := Rect(0, 0, Width, Height);
  Chart.DrawLegendOn(Canvas, r);
end;

procedure TChartLegendPanel.SetChart(AValue: TChart);
begin
  if FChart = AValue then exit;
  if FListener.IsListening then
    FChart.Broadcaster.Unsubscribe(FListener);
  FChart := AValue;
  if FChart <> nil then
    FChart.Broadcaster.Subscribe(FListener);
  Invalidate;
end;

end.

