unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAChartImageList, TASources,
  TAFuncSeries, Forms, Controls, Graphics, Dialogs, ComCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Chart1: TChart;
    Chart1FuncSeries1: TFuncSeries;
    Chart1FuncSeries2: TFuncSeries;
    ChartImageList: TChartImageList;
    ToolbarImages: TImageList;
    ListView1: TListView;
    RandomChartSource1: TRandomChartSource;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    tbAdd: TToolButton;
    tbQuit: TToolButton;
    tbSep1: TToolButton;
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure Chart1FuncSeries2Calculate(const AX: Double; out AY: Double);
    procedure ChartImageListPopulate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbAddClick(Sender: TObject);
    procedure tbQuitClick(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  LCLIntf, TATypes, TASeries;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := Sin(AX);
end;

procedure TfrmMain.Chart1FuncSeries2Calculate(const AX: Double; out AY: Double);
begin
  AY := Cos(AX);
end;

procedure TfrmMain.ChartImageListPopulate(Sender: TObject);
begin
  with ChartImageList do
    StatusBar1.SimpleText := Format(
      'Now %d images in image list. ' +
      'Among them %d series images starting at index %d',
      [Count, SeriesCount, FirstSeriesIndex]);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ChartImageList.Chart := nil;
  ChartImageList.AddImages(ToolbarImages);
  ChartImageList.Chart := Chart1;
end;

procedure TfrmMain.tbAddClick(Sender: TObject);
var
  series: TLineSeries;
  sty: Integer;
begin
  RandomChartSource1.RandSeed := Random(MaxInt);
  series := TLineSeries.Create(Self);
  series.SeriesColor := RGB(Random(256), Random(256), Random(256));
  series.Pointer.Brush.Color := series.SeriesColor;
  sty := Random(Ord(High(TSeriesPointerStyle)) + 5);
  series.ShowPoints := sty <= Ord(High(TSeriesPointerStyle));
  if series.ShowPoints then
    series.Pointer.Style := TSeriesPointerStyle(sty);
  series.ListSource.CopyFrom(RandomChartSource1);
  Chart1.AddSeries(series);
  with Listview1.Items.Add do begin
    Caption := Format('run item %d', [ListView1.Items.Count]);
    ImageIndex := ChartImageList.Count - 1;
  end;
end;

procedure TfrmMain.tbQuitClick(Sender: TObject);
begin
  Close;
end;

end.

