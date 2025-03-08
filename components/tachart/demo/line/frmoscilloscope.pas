unit frmOscilloscope;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, ExtCtrls,
  TAGraph, TASources, TATransformations, TASeries;

type
  TOscilloscopeFrame = class(TFrame)
    AxisTransformations: TChartAxisTransformations;
    LinearAxisTransform: TLinearAxisTransform;
    Chart: TChart;
    ChartLineSeries1: TLineSeries;
    ListChartSource: TListChartSource;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TOscilloscopeFrame.TimerTimer(Sender: TObject);
var
  rp: TChartRenderingParams;
begin
  rp := Chart.RenderingParams;
  with ChartLineSeries1 do begin
    Add(Sin(GetXMax / 20) + Random - 0.5);
    if Count > 20 then
      ListSource.Delete(0);
    // Allow to zoom into various parts of the chart
    // while preserving "oscilloscope" behaviour.
    LinearAxisTransform.Offset := -GetXMin;
  end;
  // Transformation change resets logical extent.
  // We know the old extent is safe to keep, so restore it.
  Chart.RenderingParams := rp;
end;

end.

