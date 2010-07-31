unit Main;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, ExtCtrls, Forms, StdCtrls, TAGraph, TASeries, TASources,
  TATransformations, Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    catTAutoAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    catTAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    catTAuto: TChartAxisTransformations;
    cbAuto: TCheckBox;
    ChartAxisGroup: TChart;
    ChartLog: TChart;
    cfsLog: TFuncSeries;
    cbLog: TCheckBox;
    ChartTWinterBar: TBarSeries;
    clsLogPoints: TLineSeries;
    ChartT: TChart;
    catLog: TChartAxisTransformations;
    ChartAxisTransformations1LinearAxisTransform2: TLinearAxisTransform;
    ChartAxisTransformations1LogarithmAxisTransform1: TLogarithmAxisTransform;
    catT: TChartAxisTransformations;
    catTFahrToCel: TLinearAxisTransform;
    ChartCustomMarks: TChart;
    ChartCustomMarksBarSeries1: TBarSeries;
    ChartTSummer: TLineSeries;
    ChartTWinterLine: TLineSeries;
    lcsMarks: TListChartSource;
    PageControl1: TPageControl;
    pnlLogControls: TPanel;
    pnlAutoControls: TPanel;
    rcsTSummer: TRandomChartSource;
    rcsTWinter: TRandomChartSource;
    lsLinear: TTabSheet;
    tsAxisGroup: TTabSheet;
    tsLog: TTabSheet;
    tsCustomMarks: TTabSheet;
    procedure cbAutoChange(Sender: TObject);
    procedure cbLogChange(Sender: TObject);
    procedure ChartLogFuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure FormCreate(Sender: TObject);
    procedure TChartAxisList1MarkToText(var AText: String; AMark: Double);
  end;

var
  Form1: TForm1; 

implementation

uses
  Math, TAChartAxis;

{$R *.lfm}

function MyFunc(AX: Double): Double;
begin
  Result := Power(10, AX) + 3;
end;

{ TForm1 }

procedure TForm1.cbAutoChange(Sender: TObject);
begin
  catTAutoAutoScaleAxisTransform1.Enabled := cbAuto.Checked;
  catTAutoScaleAxisTransform1.Enabled := cbAuto.Checked;
end;

procedure TForm1.cbLogChange(Sender: TObject);
begin
  ChartAxisTransformations1LogarithmAxisTransform1.Enabled := cbLog.Checked;
end;

procedure TForm1.ChartLogFuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := MyFunc(AX);
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  COLORS: array [1..5] of Integer =
    ($0000A0, $002080, $004060, $006040, $008020);
var
  i, j: Integer;
  x: Double;
  ls: TLineSeries;
  tr: TChartAxisTransformations;
begin
  for i := 0 to 50 do begin
    with cfsLog.Extent do
      x := i / 50 * (XMax - XMin) + XMin;
    clsLogPoints.AddXY(x + Random - 0.5, MyFunc(x) + Random - 0.5);
  end;
  for i := 1 to 5 do begin
    ls := TLineSeries.Create(Self);
    ChartAxisGroup.AddSeries(ls);
    ls.SeriesColor := COLORS[i];
    ls.LinePen.Width := 2;
    for j := 1 to 20 do
      ls.AddXY(j, Random * 8);
    tr := TChartAxisTransformations.Create(Self);
    with TAutoScaleAxisTransform.Create(Self) do begin
      Transformations := tr;
      MinValue := i;
      MaxValue := i + 0.8;
    end;
    with TChartAxis.Create(ChartAxisGroup.AxisList) do begin
      Transformations := tr;
      Marks.AtDataOnly := true;
      Marks.LabelFont.Orientation := 900;
      Marks.LabelFont.Color := COLORS[i];
      TickColor := COLORS[i];
      Group := 1;
    end;
    ls.AxisIndexY := ChartAxisGroup.AxisList.Count - 1;
  end;
end;

procedure TForm1.TChartAxisList1MarkToText(var AText: String; AMark: Double);
begin
  if AMark < 15 then
    AText := '*' + AText + '*';
end;

end.

