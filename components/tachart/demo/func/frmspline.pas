unit frmSpline;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls, StdCtrls, ExtCtrls, RTTICtrls,
  TAGraph, TASources, TASeries, TAFuncSeries, TATransformations;

type
  TSplineFrame = class(TFrame)
    catSpline: TChartAxisTransformations;
    catSplineLogarithmAxisTransform: TLogarithmAxisTransform;
    cbBSpline: TTICheckBox;
    cbCubic: TTICheckBox;
    cbLogY: TTICheckBox;
    Chart: TChart;
    ChartBSplineSeries1: TBSplineSeries;
    ChartCubicSplineSeries1: TCubicSplineSeries;
    ChartLineSeries1: TLineSeries;
    icbSplineRandomX: TTICheckBox;
    iseSplineDegree: TTISpinEdit;
    lblSplineDegree: TLabel;
    pnSpline: TPanel;
    RandomChartSource: TRandomChartSource;
    procedure iseSplineDegreeChange(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TSplineFrame.iseSplineDegreeChange(Sender: TObject);
begin
  (Sender as TTISpinEdit).EditingDone;
end;

end.

