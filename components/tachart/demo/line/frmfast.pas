unit frmFast;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Graphics, Controls, StdCtrls, ExtCtrls,
  TAGraph, TAEnumerators, TASources, TASeries, TATools;

type
  TFastDrawingFrame = class(TFrame)
    btnAddSeries: TButton;
    btnRefresh: TButton;
    cb3D: TCheckBox;
    ChartToolset: TChartToolset;
    PanDragTool: TPanDragTool;
    ZoomDragTool: TZoomDragTool;
    cmbLineType: TComboBox;
    cbRotated: TCheckBox;
    cbSorted: TCheckBox;
    Chart: TChart;
    ConstantLine: TConstantLine;
    LineSeries: TLineSeries;
    edTime: TEdit;
    lblPointsCount: TLabel;
    Panel1: TPanel;
    RandomChartSource: TRandomChartSource;
    procedure btnAddSeriesClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure cb3DChange(Sender: TObject);
    procedure cbRotatedChange(Sender: TObject);
    procedure cbSortedChange(Sender: TObject);
    procedure cmbLineTypeChange(Sender: TObject);
  private

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

type
  TLineSeriesEnum =
    specialize TFilteredChartSeriesEnumeratorFactory<TLineSeries>;

constructor TFastDrawingFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  lblPointsCount.Caption := '';
end;

procedure TFastDrawingFrame.btnAddSeriesClick(Sender: TObject);
const
  POINTS_PER_SERIES = 50000;
var
  s: TLineSeries;
  i, j: Integer;
begin
  for i := 1 to 10 do begin
    s := TLineSeries.Create(Chart);
    s.SeriesColor := clRed;
    if cb3D.Checked then
      s.Depth := 15 - s.Depth;
    s.AxisIndexY := Ord(cbRotated.Checked);
    s.AxisIndexX := 1 - s.AxisIndexY;
    s.ListSource.Sorted := cbSorted.Checked;
    for j := 1 to POINTS_PER_SERIES do
      s.AddXY(j, Random * 5 + Chart.SeriesCount * 10);
    Chart.AddSeries(s);
  end;
  lblPointsCount.Caption :=
    Format('Points: %.0n', [Chart.SeriesCount * POINTS_PER_SERIES * 1.0]);
end;

procedure TFastDrawingFrame.btnRefreshClick(Sender: TObject);
var
  t: TDateTime;
begin
  t := Now;
  Chart.Refresh;
  edTime.Text := FormatDateTime('s.zzz', Now - t) + ' s';
end;

procedure TFastDrawingFrame.cb3DChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(Chart) do
    ls.Depth := 15 - ls.Depth;
end;

procedure TFastDrawingFrame.cbRotatedChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(Chart) do begin
    ls.AxisIndexY := Ord(cbRotated.Checked);
    ls.AxisIndexX := 1 - ls.AxisIndexY;
  end;
  ConstantLine.LineStyle := TLineStyle(cbRotated.Checked);
  ConstantLine.AxisIndex := Ord(cbRotated.Checked);
end;

procedure TFastDrawingFrame.cbSortedChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(Chart) do
    if ls.Source is TListChartSource then
      ls.ListSource.Sorted := cbSorted.Checked;
end;

procedure TFastDrawingFrame.cmbLineTypeChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(Chart) do
    ls.LineType := TLineType(cmbLineType.ItemIndex);
end;


end.

