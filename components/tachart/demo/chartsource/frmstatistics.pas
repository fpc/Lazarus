unit frmStatistics;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls,
  TAGraph, TASeries, TASources;

type
  TStatisticsFrame = class(TFrame)
    cbAccDirStatistics: TComboBox;
    cbCumulative: TCheckBox;
    ccsAvg: TCalculatedChartSource;
    ccsSum: TCalculatedChartSource;
    chCalc: TChart;
    chCalcLineSeries1: TLineSeries;
    chCalcLineSeriesAvg: TLineSeries;
    chCalcLineSeriesSum: TLineSeries;
    Panel2: TPanel;
    RandomChartSource2: TRandomChartSource;
    procedure cbAccDirStatisticsChange(Sender: TObject);
    procedure cbCumulativeChange(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TStatisticsFrame.cbCumulativeChange(Sender: TObject);
begin
  chCalcLineSeriesSum.Active := cbCumulative.Checked;
end;

procedure TStatisticsFrame.cbAccDirStatisticsChange(Sender: TObject);
var
  dir: TChartAccumulationDirection;
begin
  dir := TChartAccumulationDirection(cbAccDirStatistics.ItemIndex);
  ccsAvg.AccumulationDirection := dir;
  ccsSum.AccumulationDirection := dir;
end;

end.

