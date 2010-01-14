unit main;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1BarSeries2: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    RandomChartSource1: TRandomChartSource;
    RandomChartSource2: TRandomChartSource;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

end.

