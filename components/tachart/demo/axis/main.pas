unit Main;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, Forms, TAGraph, TASeries, TASources, TATransformations;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    ChartT: TChart;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LinearAxisTransform2: TLinearAxisTransform;
    ChartAxisTransformations1LogarithmAxisTransform1: TLogarithmAxisTransform;
    catT: TChartAxisTransformations;
    catTCelToFahr: TLinearAxisTransform;
    ChartCustomMarks: TChart;
    ChartCustomMarksBarSeries1: TBarSeries;
    ChartTSummer: TLineSeries;
    ChartTWinter: TLineSeries;
    ListChartSource1: TListChartSource;
    PageControl1: TPageControl;
    RandomChartSource1: TRandomChartSource;
    rcsTSummer: TRandomChartSource;
    rcsTWinter: TRandomChartSource;
    lsLinear: TTabSheet;
    tsMultiAxis: TTabSheet;
    tsCustomMarks: TTabSheet;
    procedure TChartAxisList1MarkToText(var AText: String; AMark: Double);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.TChartAxisList1MarkToText(var AText: String; AMark: Double);
begin
  if AMark < 15 then
    AText := '*' + AText + '*';
end;

end.

