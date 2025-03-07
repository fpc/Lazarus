unit frmLinear;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls,
  TAGraph, TASeries, TASources, TATransformations;

type
  TLinearFrame = class(TFrame)
    catT: TChartAxisTransformations;
    catTAuto: TChartAxisTransformations;
    catTAutoAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    catTAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    catTFahrToCel: TLinearAxisTransform;
    cbAuto: TCheckBox;
    Chart: TChart;
    ChartSummer: TLineSeries;
    ChartWinterBar: TBarSeries;
    ChartWinterLine: TLineSeries;
    pnlAutoControls: TPanel;
    rcsTSummer: TRandomChartSource;
    rcsTWinter: TRandomChartSource;
    procedure cbAutoChange(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TLinearFrame.cbAutoChange(Sender: TObject);
begin
  catTAutoAutoScaleAxisTransform1.Enabled := cbAuto.Checked;
  catTAutoScaleAxisTransform1.Enabled := cbAuto.Checked;
end;

end.

