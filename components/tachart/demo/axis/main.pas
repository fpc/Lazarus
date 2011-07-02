unit Main;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, ExtCtrls, Forms, StdCtrls, TACustomSource, TAFuncSeries, TAGraph,
  TASeries, TASources, TATools, TATransformations;

type

  { TForm1 }

  TForm1 = class(TForm)
    catTAutoAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    catTAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    catTAuto: TChartAxisTransformations;
    cbAuto: TCheckBox;
    ChartSubmarks: TChart;
    ChartDateTime: TChart;
    ChartAxisGroup: TChart;
    ChartDateTimeLineSeries1: TLineSeries;
    ChartLog: TChart;
    cfsLog: TFuncSeries;
    cbLog: TCheckBox;
    ChartSubmarksLineSeries1: TLineSeries;
    ChartToolsetDateTime: TChartToolset;
    ChartToolset1ZoomIn: TZoomClickTool;
    ChartToolset1ZoomOut: TZoomClickTool;
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
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    lcsMarks: TListChartSource;
    PageControl1: TPageControl;
    pnlLogControls: TPanel;
    pnlAutoControls: TPanel;
    rcsDates: TRandomChartSource;
    rcsTSummer: TRandomChartSource;
    rcsTWinter: TRandomChartSource;
    lsLinear: TTabSheet;
    tsSubmarks: TTabSheet;
    tsDateTime: TTabSheet;
    tsAxisGroup: TTabSheet;
    tsLog: TTabSheet;
    tsCustomMarks: TTabSheet;
    udcsMain: TUserDefinedChartSource;
    udcsGraph: TUserDefinedChartSource;
    udcsSub: TUserDefinedChartSource;
    procedure cbAutoChange(Sender: TObject);
    procedure cbLogChange(Sender: TObject);
    procedure ChartLogFuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure FormCreate(Sender: TObject);
    procedure TChartAxisList1MarkToText(var AText: String; AMark: Double);
    procedure udcsGraphGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure udcsMainGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure udcsSubGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
  end;

var
  Form1: TForm1; 

implementation

uses
  Math, SysUtils, TAChartAxis, TAChartUtils;

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
  with rcsDates do begin
    XMin := Now - 5 * 365;
    XMax := Now + 5 * 365;
    PointsNumber := 10 * 365 * 24;
  end;
end;

procedure TForm1.TChartAxisList1MarkToText(var AText: String; AMark: Double);
begin
  if AMark = 3 then
    AText := '*' + AText + '*';
end;

procedure TForm1.udcsGraphGetChartDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := (AIndex - 50) * 0.1;
  AItem.Y := Sin(AItem.X * 3) + AItem.X / 10;
end;

procedure TForm1.udcsMainGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := AIndex - 5;
  AItem.Y := AItem.X;
end;

procedure TForm1.udcsSubGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := (AIndex - 25) * 0.2;
  AItem.Y := AItem.X;
end;

end.

