unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, TAGraph, TASeries, TASources, TATools,
  TATransformations, TACustomSeries;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAddSeries: TButton;
    btnRefresh: TButton;
    catOscillatorLinearAxisTransform1: TLinearAxisTransform;
    cb3D: TCheckBox;
    cbLineType: TComboBox;
    cbRotated: TCheckBox;
    cbSorted: TCheckBox;
    ccsAvg: TCalculatedChartSource;
    ccsDerivative: TCalculatedChartSource;
    ccsSum: TCalculatedChartSource;
    catOscillator: TChartAxisTransformations;
    chOscillator: TChart;
    chOscillatorLineSeries1: TLineSeries;
    chPointers: TChart;
    chFast: TChart;
    chFastConstantLine1: TConstantLine;
    chFastLineSeries1: TLineSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointDragTool1: TDataPointDragTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    edTime: TEdit;
    lblPointerSize: TLabel;
    lblPointsCount: TLabel;
    lcsOscillator: TListChartSource;
    PageControl1: TPageControl;
    Panel1: TPanel;
    pnlPointers: TPanel;
    RandomChartSource1: TRandomChartSource;
    sePointerSize: TSpinEdit;
    timOscilloscope: TTimer;
    tsOscilloscope: TTabSheet;
    tsPointers: TTabSheet;
    tsFast: TTabSheet;
    procedure btnAddSeriesClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure cb3DChange(Sender: TObject);
    procedure cbLineTypeChange(Sender: TObject);
    procedure cbRotatedChange(Sender: TObject);
    procedure cbSortedChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure sePointerSizeChange(Sender: TObject);
    procedure timOscilloscopeTimer(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  LCLIntf, TAChartUtils, TATypes, TAEnumerators;

type
  TLineSeriesEnum =
    specialize TFilteredChartSeriesEnumeratorFactory<TLineSeries>;

{ TForm1 }

procedure TForm1.btnAddSeriesClick(Sender: TObject);
const
  POINTS_PER_SERIE = 50000;
var
  s: TLineSeries;
  i, j: Integer;
begin
  for i := 1 to 10 do begin
    s := TLineSeries.Create(chFast);
    s.SeriesColor := clRed;
    for j := 1 to POINTS_PER_SERIE do
      s.AddXY(j, Random * 5 + chFast.SeriesCount * 10);
    chFast.AddSeries(s);
  end;
  lblPointsCount.Caption :=
    Format('Points: %.2e', [chFast.SeriesCount * POINTS_PER_SERIE * 1.0]);
end;

procedure TForm1.btnRefreshClick(Sender: TObject);
var
  t: TDateTime;
begin
  t := Now;
  chFast.Refresh;
  edTime.Text := FormatDateTime('s.zzz', Now - t) + ' s';
end;

procedure TForm1.cb3DChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(chFast) do
    ls.Depth := 15 - ls.Depth;
end;

procedure TForm1.cbLineTypeChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(chFast) do
    ls.LineType := TLineType(cbLineType.ItemIndex);
end;

procedure TForm1.cbRotatedChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(chFast) do begin
    ls.AxisIndexY := Ord(cbRotated.Checked);
    ls.AxisIndexX := 1 - ls.AxisIndexY;
  end;
end;

procedure TForm1.cbSortedChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(chFast) do
    if ls.Source is TListChartSource then
      ls.ListSource.Sorted := cbSorted.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  st: TSeriesPointerStyle;
  ls: TLineSeries;
  s: ShortString;
begin
  for st in TSeriesPointerStyle do begin
    ls := TLineSeries.Create(Self);
    ls.LinePen.Color := clGreen;
    ls.ShowPoints := true;
    ls.Pointer.Pen.Color := clRed;
    ls.Pointer.Style := st;
    ls.AddXY(1, Ord(st));
    Str(st, s);
    ls.AddXY(10, Ord(st), s, clGreen);
    ls.AddXY(19, Ord(st));
    ls.Marks.Visible := true;
    ls.Marks.Style := smsLabel;
    ls.Marks.Distance := 4;
    chPointers.AddSeries(ls);
  end;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  timOscilloscope.Enabled := PageControl1.ActivePage = tsOscilloscope;
end;

procedure TForm1.sePointerSizeChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(chPointers) do
    with ls.Pointer do begin
      HorizSize := sePointerSize.Value;
      VertSize := sePointerSize.Value;
    end;
end;

procedure TForm1.timOscilloscopeTimer(Sender: TObject);
var
  rp: TChartRenderingParams;
begin
  rp := chOscillator.RenderingParams;
  with chOscillatorLineSeries1 do begin
    Add(Sin(GetXMax / 20) + Random - 0.5);
    if Count > 20 then
      ListSource.Delete(0);
    // Allow to zoom into various parts of the chart
    // while preserving "oscilloscope" behaviour.
    catOscillatorLinearAxisTransform1.Offset := -GetXMin;
  end;
  // Transformation change resets logical extent.
  // We know the old extent is safe to keep, so restore it.
  chOscillator.RenderingParams := rp;
end;

end.

