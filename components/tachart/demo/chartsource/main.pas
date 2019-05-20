unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, ComCtrls, ExtCtrls, Spin, Forms,
  TAGraph, TASeries, TACustomSource, TASources,  TAMultiSeries, TAChartUtils;

type
  TDataForSorting = record
    Year: Integer;
    CityName: String;
    Visitors: Integer;
    Color: TChartColor;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    btnSort: TButton;
    BubbleSeries: TBubbleSeries;
    cbAccDirDerivative: TComboBox;
    ccsDerivative: TCalculatedChartSource;
    cbCumulative: TCheckBox;
    ccsAvg: TCalculatedChartSource;
    ccsSum: TCalculatedChartSource;
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    BubbleChart: TChart;
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
    lblSortInfo: TLabel;
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
    SortedChartSource: TSortedChartSource;
    Splitter1: TSplitter;
    tsSorted: TTabSheet;
    tsDerivative: TTabSheet;
    tsStatistics: TTabSheet;
    tsBasic: TTabSheet;
    UserDefinedChartSource: TUserDefinedChartSource;
    procedure btnSortClick(Sender: TObject);
    procedure BubbleChartAfterPaint(ASender: TChart);
    procedure cbAccDirDerivativeChange(Sender: TObject);
    procedure cbAccDirStatisticsChange(Sender: TObject);
    procedure cbCumulativeChange(Sender: TObject);
    procedure cbSmoothChange(Sender: TObject);
    procedure CreateDataForDerivative;
    procedure CreateDataForSorting;
    procedure seAccumulationRangeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgDataShapeClick(Sender: TObject);
    procedure UserDefinedChartSourceGetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
  private
    FDataForSorting: array of TDataForSorting;
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

procedure TForm1.btnSortClick(Sender: TObject);
begin
  SortedChartSource.Sorted := not SortedChartSource.Sorted;
end;

procedure TForm1.BubbleChartAfterPaint(ASender: TChart);
begin
  if SortedChartSource.IsSorted then
    lblSortInfo.Caption := 'Sorted by radius (descending)'
  else
    lblSortInfo.Caption := 'Not sorted';
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

procedure TForm1.CreateDataForDerivative;
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

procedure TForm1.CreateDataForSorting;

  procedure AddCity(AYear: Integer; const ACityName: string;
    ANumberOfVisitors: Integer; AColor: TChartColor);
  begin
    SetLength(FDataForSorting, Length(FDataForSorting) + 1);
    with FDataForSorting[High(FDataForSorting)] do begin
      Year := AYear;
      CityName := ACityName;
      Visitors := ANumberOfVisitors;
      Color := AColor;
    end;
  end;

begin
  SetLength(FDataForSorting, 0);
  AddCity(1967, 'City A', 155000, $00FFFF);  // yellow
  AddCity(1968, 'City B', 30000, $FF0000);   // blue
  AddCity(1969, 'City C', 450000, $FF00FF);  // fuchsia
  AddCity(1970, 'City D', 50000, $FFFF00);   // aqua
  AddCity(1971, 'City E', 80000, $00FF00);   // lime
  AddCity(1972, 'City F', 615000, $0000FF);  // red
  UserDefinedChartSource.PointsNumber := Length(FDataForSorting);

  { These settings are made in the Object Inspector for the SortedChartSource:
    - Origin = UserDefinedChartSource
    - SortBy = sbY
    - SortIndex = 1            // --> sort by radius
    - SortDir = sdDescending   // --> draw large bubbles first
  }
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  CreateDataForDerivative;
  CreateDataForSorting;
end;

procedure TForm1.rgDataShapeClick(Sender: TObject);
begin
  CreateDataForDerivative;
end;

procedure TForm1.UserDefinedChartSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := FDataForSorting[AIndex].Year;
  AItem.Y := 0;
  AItem.YList[0] := Sqrt(FDataForSorting[AIndex].Visitors/100000 / PI);
  AItem.Text := FDataForSorting[AIndex].CityName;
  AItem.Color := FDataForSorting[AIndex].Color;
end;

procedure TForm1.seAccumulationRangeChange(Sender: TObject);
begin
  ccsDerivative.AccumulationRange := seAccumulationRange.Value;
end;

end.

