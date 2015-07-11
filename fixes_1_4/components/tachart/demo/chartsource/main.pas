unit Main;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, ExtCtrls, Spin, StdCtrls, Forms, TAGraph, TASeries, TASources,
  Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbAccDirDerivative: TComboBox;
    ccsDerivative: TCalculatedChartSource;
    cbCumulative: TCheckBox;
    ccsAvg: TCalculatedChartSource;
    ccsSum: TCalculatedChartSource;
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    chDerivativeLineOrig: TLineSeries;
    chDerivativeLineDeriv: TLineSeries;
    Chart1LineSeries4: TLineSeries;
    Chart1LineSeries5: TLineSeries;
    Chart2: TChart;
    Chart2AreaSeries1: TAreaSeries;
    Chart2LineSeries1: TLineSeries;
    chDerivative: TChart;
    chCalc: TChart;
    chCalcLineSeries1: TLineSeries;
    chCalcLineSeriesAvg: TLineSeries;
    chCalcLineSeriesSum: TLineSeries;
    cbAccDirStatistics: TComboBox;
    cbSmooth: TCheckBox;
    seAccumulationRange: TSpinEdit;
    lblAccumulationRange: TLabel;
    ListChartSource1: TListChartSource;
    lcsDerivative: TListChartSource;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    rgDataShape: TRadioGroup;
    RandomChartSource1: TRandomChartSource;
    RandomChartSource2: TRandomChartSource;
    Splitter1: TSplitter;
    tsDerivative: TTabSheet;
    tsStatistics: TTabSheet;
    tsBasic: TTabSheet;
    procedure cbAccDirDerivativeChange(Sender: TObject);
    procedure cbAccDirStatisticsChange(Sender: TObject);
    procedure cbCumulativeChange(Sender: TObject);
    procedure cbSmoothChange(Sender: TObject);
    procedure CreateData;
    procedure seAccumulationRangeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgDataShapeClick(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  Math;

{ TForm1 }

procedure TForm1.cbAccDirDerivativeChange(Sender: TObject);
begin
  ccsDerivative.AccumulationDirection :=
    TChartAccumulationDirection(cbAccDirDerivative.ItemIndex);
end;

procedure TForm1.cbAccDirStatisticsChange(Sender: TObject);
begin
  ccsAvg.AccumulationDirection :=
    TChartAccumulationDirection(cbAccDirStatistics.ItemIndex);
  ccsSum.AccumulationDirection := ccsAvg.AccumulationDirection;
end;

procedure TForm1.cbCumulativeChange(Sender: TObject);
begin
  chCalcLineSeriesSum.Active := cbCumulative.Checked;
end;

procedure TForm1.cbSmoothChange(Sender: TObject);
begin
  if cbSmooth.Checked then
    ccsDerivative.AccumulationMethod := camSmoothDerivative
  else
    ccsDerivative.AccumulationMethod := camDerivative;
end;

procedure TForm1.CreateData;
const
  N = 100;
  MIN_X = -10;
  MAX_X = 10;
  EPS = 1e-6;
var
  i: Integer;
  x, y: Double;
begin
  lcsDerivative.Clear;
  if rgDataShape.ItemIndex = 6 then
    for i := 0 to 9 do
      lcsDerivative.Add(i - IfThen(i > 6, 1, 0), i)
  else
    for i := 0 to N - 1 do begin
      x := MIN_X + (MAX_X - MIN_X) / (N - 1) * i;
      if SameValue(x, 0.0, EPS) then x := 0;
      case rgDataShape.ItemIndex of
        0: y := x;
        1: y := Sin(x);
        2: if x = 0 then y := 1 else y := Sin(x) / x;
        3: y := Exp(-x / 3);
        4: y := Exp(-Sqr((x - 2.5) / 2.5));
        5: y := Exp(-Sqr((x - 2.5) / 2.5)) + 0.05 * (Random - 0.5);
      end;
      lcsDerivative.Add(x, y);
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  CreateData;
end;

procedure TForm1.rgDataShapeClick(Sender: TObject);
begin
  CreateData;
end;

procedure TForm1.seAccumulationRangeChange(Sender: TObject);
begin
  ccsDerivative.AccumulationRange := seAccumulationRange.Value;
end;

end.

