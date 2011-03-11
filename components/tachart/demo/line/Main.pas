unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, TAGraph, TASeries, TASources, TATools;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAddSeries: TButton;
    btnRefresh: TButton;
    cb3D: TCheckBox;
    cbLineType: TComboBox;
    cbRotated: TCheckBox;
    cbSorted: TCheckBox;
    ccsAvg: TCalculatedChartSource;
    ccsSum: TCalculatedChartSource;
    chCalc: TChart;
    chCalcLineSeries1: TLineSeries;
    chCalcLineSeriesAvg: TLineSeries;
    chCalcLineSeriesSum: TLineSeries;
    chFast: TChart;
    chFastConstantLine1: TConstantLine;
    chFastLineSeries1: TLineSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointDragTool1: TDataPointDragTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    edTime: TEdit;
    lblPointsCount: TLabel;
    PageControl2: TPageControl;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    tsStats: TTabSheet;
    tsFast: TTabSheet;
    procedure btnAddSeriesClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure cb3DChange(Sender: TObject);
    procedure cbLineTypeChange(Sender: TObject);
    procedure cbRotatedChange(Sender: TObject);
    procedure cbSortedChange(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  LCLIntf;

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
  i: Integer;
begin
  for i := 0 to chFast.SeriesCount - 1 do
    if chFast.Series[i] is TLineSeries then
      with chFast.Series[i] as TLineSeries do
        Depth := 15 - Depth;
end;

procedure TForm1.cbLineTypeChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chFast.SeriesCount - 1 do
    if chFast.Series[i] is TLineSeries then
      with chFast.Series[i] as TLineSeries do
        LineType := TLineType(cbLineType.ItemIndex);
end;

procedure TForm1.cbRotatedChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chFast.SeriesCount - 1 do
    if chFast.Series[i] is TLineSeries then
      with chFast.Series[i] as TLineSeries do begin
        AxisIndexY := Ord(cbRotated.Checked);
        AxisIndexX := 1 - AxisIndexY;
      end;
end;

procedure TForm1.cbSortedChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chFast.SeriesCount - 1 do
    if chFast.Series[i] is TLineSeries then
      with chFast.Series[i] as TLineSeries do
        if Source is TListChartSource then
          ListSource.Sorted := cbSorted.Checked;
end;

end.

