unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Grids, TAGraph, TASeries, TASources, TATools, TAFuncSeries,
  TARadialSeries, Types;

type

  { TMainForm }

  TMainForm = class(TForm)
    Chart: TChart;
    cbSortBy: TComboBox;
    cbXCount0: TCheckBox;
    ChartToolset: TChartToolset;
    DataPointClickTool: TDataPointClickTool;
    PanDragTool: TPanDragTool;
    Grid: TStringGrid;
    ZoomDragTool: TZoomDragTool;
    Label1: TLabel;
    ListChartSource: TListChartSource;
    SettingsPanel: TPanel;
    TabControl: TTabControl;
    procedure cbSortByChange(Sender: TObject);
    procedure cbXCount0Change(Sender: TObject);
    procedure DataPointClickToolPointClick(ATool: TChartTool; APoint: TPoint);
    procedure FormCreate(Sender: TObject);
    procedure GridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure TabControlChange(Sender: TObject);
  private
    PieSeries: TPieSeries;
    BarSeries: TBarSeries;
    LineSeries: TLineSeries;
    AreaSeries: TAreaSeries;
    CubicSplineSeries: TCubicSplineSeries;
    BSplineSeries: TBSplineSeries;
    FitSeries: TFitSeries;
    PolarSeries: TPolarSeries;
  private
    procedure CreateData;
    procedure UpdateGrid;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  TACustomSource, TACustomSeries, TAChartUtils, TATypes, TAFitUtils;

{ TMainForm }

procedure TMainForm.CreateData;
begin
  ListChartSource.BeginUpdate;
  try
    ListChartSource.Clear;
    ListChartSource.XCount := 0;
    ListChartSource.Add(2,  738, 'Europe', clBlue);
    ListChartSource.Add(0, 4436, 'Asia', clYellow);
    ListChartSource.Add(5,   39.9, 'Oceania', clAqua);
    ListChartSource.Add(1, 1216, 'Africa', clMaroon);
    ListChartSource.Add(3,  579, 'North' + LineEnding + 'America', clRed);
    ListChartSource.Add(6, 0.004, 'Antarctica', clWhite);
    ListChartSource.Add(4,  422, 'South' + LineEnding + 'America', clGreen);
  finally
    ListChartSource.EndUpdate;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
  ser: TChartSeries;
begin
  CreateData;
  Chart.BottomAxis.Marks.Source := ListChartSource;
  Chart.BottomAxis.Marks.Style := smsLabel;
  UpdateGrid;

  PieSeries := TPieSeries.Create(Chart);
  PieSeries.Title := 'Pies';
  PieSeries.Marks.Style := smsCustom;
  PieSeries.Marks.Format := '%2:s' + LineEnding + '%0:.9g';
  Chart.AddSeries(PieSeries);
  TabControl.Tabs.Add('Pie series');

  BarSeries := TBarSeries.Create(Chart);
  BarSeries.Title := 'Bars';
  BarSeries.BarWidthStyle := bwPercentMin;
  Chart.AddSeries(BarSeries);
  TabControl.Tabs.Add('Bar series');

  LineSeries := TLineSeries.Create(Chart);
  LineSeries.Title := 'Lines';
  LineSeries.SeriesColor := clRed;
  LineSeries.ShowPoints := true;
  LineSeries.Pointer.Style := psCircle;
  LineSeries.Pointer.HorizSize := 6;
  LineSeries.Pointer.VertSize := 6;
  Chart.AddSeries(LineSeries);
  TabControl.Tabs.Add('Line series');

  AreaSeries := TAreaSeries.Create(Chart);
  AreaSeries.Title := 'Areas';
  AreaSeries.AreaBrush.Color := clRed;
  AreaSeries.AreaLinesPen.Style := psClear;
  Chart.AddSeries(AreaSeries);
  TabControl.Tabs.Add('Area series');

  CubicSplineSeries := TCubicSplineSeries.Create(Chart);
  CubicSplineSeries.Title := 'Cubic spline';
  CubicSplineSeries.Pen.Color := clBlue;
  CubicSplineSeries.Pointer.Visible := true;
  CubicSplineSeries.Pointer.Style := psCircle;
  CubicSplineSeries.Pointer.HorizSize := 6;
  CubicSplineSeries.Pointer.VertSize := 6;
  Chart.AddSeries(CubicSplineSeries);
  TabControl.Tabs.Add('Cubic spline series');

  BSplineSeries := TBSplineSeries.Create(Chart);
  BSplineSeries.Title := 'B-Spline';
  BSplineSeries.Pen.Color := clBlue;
  BSplineSeries.Pointer.Visible := true;
  BSplineSeries.Pointer.Style := psCircle;
  BSplineSeries.Pointer.HorizSize := 6;
  BSplineSeries.Pointer.VertSize := 6;
  Chart.AddSeries(BSplineSeries);
  TabControl.Tabs.Add('B-Spline series');

  FitSeries := TFitSeries.Create(Chart);
  FitSeries.Title := 'Fit';
  FitSeries.Pen.Color := clBlue;
  FitSeries.Pointer.Visible := true;
  FitSeries.Pointer.Style := psCircle;
  FitSeries.Pointer.HorizSize := 6;
  FitSeries.Pointer.VertSize := 6;
  FitSeries.FitEquation := feLinear;
  Chart.AddSeries(FitSeries);
  TabControl.Tabs.Add('Fit series');

  PolarSeries := TPolarSeries.Create(Chart);
  PolarSeries.Title := 'Polar';
  PolarSeries.LinePen.Color := clBlue;
  PolarSeries.Pointer.Visible := true;
  PolarSeries.Pointer.Style := psCircle;
  PolarSeries.Pointer.HorizSize := 6;
  PolarSeries.Pointer.VertSize := 6;
  PolarSeries.Marks.Style := smsCustom;
  PolarSeries.Marks.Format := '%2:s' + LineEnding + '%0:.9g';
  Chart.AddSeries(PolarSeries);
  TabControl.Tabs.Add('Polar series');

  for i:=0 to Chart.SeriesCount-1 do begin
    ser := Chart.Series[i] as TChartSeries;
    if (ser <> PieSeries) and (ser <> PolarSeries) then
      ser.Marks.Style := smsValue;
    ser.Marks.LinkPen.Color := clGray;
    ser.Marks.Alignment := taCenter;
  end;

  TabControlChange(nil);
end;

procedure TMainForm.GridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  if aCol < Grid.ColCount - 1 then begin
    ts := Grid.Canvas.TextStyle;
    ts.Alignment := taCenter;
    Grid.Canvas.TextStyle := ts;
  end;
end;

procedure TMainForm.cbSortByChange(Sender: TObject);
begin
  case cbSortBy.ItemIndex of
    0: begin
         ListChartSource.SortBy := sbX;
         ListChartSource.SortDir := sdAscending;
       end;
    1: begin
         ListChartSource.SortBy := sbY;
         ListChartSource.SortDir := sdDescending;
       end;
    2: begin
         ListChartSource.SortBy := sbY;
         ListChartSource.SortDir := sdAscending;
       end;
    3: begin
         ListChartSource.SortBy := sbText;
         ListChartSource.SortDir := sdAscending;
       end;
  end;
  ListChartSource.Sorted := true;
  UpdateGrid;
end;

procedure TMainForm.cbXCount0Change(Sender: TObject);
begin
  if cbXCount0.Checked then
    ListChartSource.XCount := 0
  else
    ListChartSource.XCount := 1;
end;

procedure TMainForm.DataPointClickToolPointClick(ATool: TChartTool;
  APoint: TPoint);
var
  tool: TDataPointClickTool;
  ser: TChartSeries;
begin
  tool := ATool as TDataPointClickTool;
  ser := tool.Series as TChartSeries;
  ShowMessage(Format('Series "%s" clicked at data point #%d, x=%f, y=%f ("%s")', [
    ser.Title, tool.PointIndex,
    ser.GetXValue(tool.PointIndex), ser.GetYValue(tool.PointIndex),
    ser.Source.Item[tool.PointIndex]^.Text
  ]));
end;

procedure TMainForm.TabControlChange(Sender: TObject);
var
  ser: TChartSeries;
  i: Integer;
begin
  ser := Chart.Series[TabControl.TabIndex] as TChartSeries;
  ser.Source := ListChartSource;
  for i := 0 to Chart.SeriesCount-1 do
    Chart.Series[i].Active := Chart.Series[i] = ser;

  Chart.Frame.Visible := ser <> PieSeries;
  Chart.BottomAxis.Visible := ser <> PieSeries;
  Chart.LeftAxis.Visible := ser <> PieSeries;
  Chart.LeftAxis.Title.Visible := ser <> PolarSeries;
  Chart.BottomAxis.Title.Visible := ser <> PolarSeries;

  DataPointClickTool.AffectedSeries := IntToStr(ser.Index);

  if ser = PolarSeries then begin
    Chart.BottomAxis.Marks.Source := nil;
    Chart.BottomAxis.Marks.Style := smsValue;
    Chart.LeftAxis.Marks.Range.UseMin := false;
  end else begin
    Chart.BottomAxis.Marks.Source := ListChartSource;
    if ser = PieSeries then
      Chart.BottomAxis.Marks.Style := smsLabelValue
    else
      Chart.BottomAxis.Marks.Style := smsLabel;
    Chart.LeftAxis.Marks.Range.UseMin := true;
  end;
end;

procedure TMainForm.UpdateGrid;
var
  r, j: Integer;
begin
  Grid.RowCount := ListChartSource.Count + 1;
  j := 0;
  for r := 1 to Grid.RowCount - 1 do begin
    Grid.Cells[0, r] := IntToStr(j);
    Grid.Cells[1, r] := FormatFloat('0', ListChartSource[j]^.X);
    Grid.Cells[2, r] := Format('%.9g', [ListChartSource[j]^.Y]);
    Grid.Cells[3, r] := ListChartSource[j]^.Text;
    inc(j);
  end;
end;

end.

