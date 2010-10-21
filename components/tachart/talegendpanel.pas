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
unit TALegendPanel;

{$H+}

interface

uses
  Classes, Controls, TAGraph;

type

  { TChartLegendPanel }

  TChartLegendPanel = class(TCustomControl)
  private
    FChart: TChart;
    procedure SetChart(const AValue: TChart);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property Chart: TChart read FChart write SetChart;
  published
    property Align;
  end;

procedure Register;

implementation

uses
  TAChartUtils;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartLegendPanel]);
end;

{ TChartLegendPanel }

constructor TChartLegendPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 40;
  Height := 20;
end;

procedure TChartLegendPanel.Paint;
var
  r: TRect;
begin
  if Chart = nil then exit;
  r := Canvas.ClipRect;
  Chart.DrawLegendOn(Canvas, r);
end;

procedure TChartLegendPanel.SetChart(const AValue: TChart);
begin
  if FChart = AValue then exit;
  FChart := AValue;
  Invalidate;
end;

end.

