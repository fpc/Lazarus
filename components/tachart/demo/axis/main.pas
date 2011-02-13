unit Main;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, ExtCtrls, Forms, TACustomSource, TAFuncSeries, TAGraph,
  TASeries, TASources, TAStyles, TATools, TATransformations, TAChartAxis;

type

  { TForm1 }

  TForm1 = class(TForm)
    catTAutoAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    catTAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    csStripes: TChartStyles;
    ChartSubmarks: TChart;
    ChartDateTime: TChart;
    ChartAxisGroup: TChart;
    ChartDateTimeLineSeries1: TLineSeries;
    ChartSubmarksLineSeries1: TLineSeries;
    ChartToolsetDateTime: TChartToolset;
    ChartToolset1ZoomIn: TZoomClickTool;
    ChartToolset1ZoomOut: TZoomClickTool;
    catT: TChartAxisTransformations;
    catTFahrToCel: TLinearAxisTransform;
    ChartCustomMarks: TChart;
    ChartCustomMarksBarSeries1: TBarSeries;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    lcsMarks: TListChartSource;
    PageControl1: TPageControl;
    rcsDates: TRandomChartSource;
    tsSubmarks: TTabSheet;
    tsDateTime: TTabSheet;
    tsAxisGroup: TTabSheet;
    tsCustomMarks: TTabSheet;
    udcsMain: TUserDefinedChartSource;
    udcsGraph: TUserDefinedChartSource;
    udcsSub: TUserDefinedChartSource;
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
  SysUtils, TAChartUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
const
  COLORS: array [1..5] of Integer =
    ($0000A0, $002080, $004060, $006040, $008020);
var
  i, j: Integer;
  ls: TLineSeries;
  tr: TChartAxisTransformations;
begin
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
  if AMark < 15 then
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

