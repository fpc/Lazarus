unit Main;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, ExtCtrls, StdCtrls, Forms, TAGraph, TASeries, TASources, Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbCumulative: TCheckBox;
    ccsAvg: TCalculatedChartSource;
    ccsSum: TCalculatedChartSource;
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    Chart2: TChart;
    Chart2AreaSeries1: TAreaSeries;
    Chart2LineSeries1: TLineSeries;
    chCalc: TChart;
    chCalcLineSeries1: TLineSeries;
    chCalcLineSeriesAvg: TLineSeries;
    chCalcLineSeriesSum: TLineSeries;
    ListChartSource1: TListChartSource;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel2: TPanel;
    RandomChartSource1: TRandomChartSource;
    RandomChartSource2: TRandomChartSource;
    Splitter1: TSplitter;
    tsStatistics: TTabSheet;
    tsBasic: TTabSheet;
    procedure cbCumulativeChange(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.cbCumulativeChange(Sender: TObject);
begin
  chCalcLineSeriesSum.Active := cbCumulative.Checked;
end;

end.

