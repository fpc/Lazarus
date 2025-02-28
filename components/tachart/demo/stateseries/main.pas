unit Main;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, SysUtils, Classes, Math,
  Graphics, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, LCLVersion,
  TAGraph, TAIntervalSources, TASources, TAChartUtils, TATextElements, TATools,
  TAChartAxisUtils, TACustomSeries, TASeries, TAMultiSeries;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Chart: TChart;
    cbAdjustMargin: TCheckBox;
    FlowPanel1: TFlowPanel;
    Label1: TLabel;
    MachineA_Series: TStateSeries;
    MachineB_Series: TStateSeries;
    MachineC_Series: TStateSeries;
    ChartToolset: TChartToolset;
    cbSeriesMarks: TCheckBox;
    cbShowPopupHints: TCheckBox;
    ChartToolsetDataPointDragTool1: TDataPointDragTool;
    ChartToolsetPanDragTool1: TPanDragTool;
    ChartToolsetZoomDragTool1: TZoomDragTool;
    cbRotated: TCheckBox;
    DataPointHintTool: TDataPointHintTool;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    MachineLabelsChartSource: TListChartSource;
    Panel1: TPanel;
    TrackBar1: TTrackBar;
    procedure cbRotatedChange(Sender: TObject);
    procedure cbSeriesMarksChange(Sender: TObject);
    procedure cbShowPopupHintsChange(Sender: TObject);
    procedure cbAdjustMarginChange(Sender: TObject);
    procedure Chart1AxisList1GetMarkText(Sender: TObject; var AText: String;
      AMark: Double);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    procedure GetMarkTextHandler(ASeries: TChartSeries;
      APointIndex, {%H-}AXIndex, {%H-}AYIndex: Integer; var AFormattedMark: String);
    procedure PrepareMarks(ASeries: TStateSeries);
    procedure SetupNormalAxes;
    procedure SetupRotatedAxes;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  clRepair = $4040FF;        // red
  clProduction = $00C800;    // green
  clDevelopment = $FC8B70;   // blue
  clMaintainance = clYellow; // yellow

  idxMachineA = 0;
  idxMachineB = 1;
  idxMachineC = 2;

procedure TMainForm.cbAdjustMarginChange(Sender: TObject);
var
  ext: TDoubleRect;
begin
  if cbAdjustMargin.Checked then
  begin
    ext := Chart.LogicalExtent;
    if MachineA_Series.IsRotated then
    begin
      Chart.Extent.XMin := Floor(ext.a.x) + 0.5;
      Chart.Extent.XMax := Ceil(ext.b.x) - 0.5;
      Chart.Extent.UseXMin := true;
      Chart.Extent.UseXMax := true;
    end else
    begin
      Chart.Extent.YMin := Floor(ext.a.y) + 0.5;
      Chart.Extent.YMax := Ceil(ext.b.y) - 0.5;
      Chart.Extent.UseYMin := true;
      Chart.Extent.UseYMax := true;
    end;
  end else
  begin
    Chart.Extent.UseXMin := false;
    Chart.Extent.UseXMax := false;
    Chart.Extent.UseYMin := false;
    Chart.Extent.UseYMax := false;
  end;
end;

// Toggles between "normal" and "rotated" state series (horizontal or
// vertical orientation)
procedure TMainForm.cbRotatedChange(Sender: TObject);
var
  w, h, i: Integer;
begin
  w := Width;
  h := Height;
  SetBounds(Left, Top, h, w);
  for i := 0 to Chart.SeriesCount-1 do
    if Chart.Series[i] is TStateSeries then
      with TStateSeries(Chart.Series[i]) do
        if cbRotated.Checked then
        begin
          AxisIndexX := 0;
          AxisIndexY := 1;
        end else
        begin
          AxisIndexX := 1;
          AxisIndexY := 0;
        end;

  if cbRotated.Checked then
    SetupRotatedAxes
  else
    SetupNormalAxes;
end;

// Shows/hides series marks
procedure TMainForm.cbSeriesMarksChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Chart.SeriesCount-1 do
    if Chart.Series[i] is TStateSeries then
      with TStateSeries(Chart.Series[i]) do
        if cbSeriesMarks.Checked then
          Marks.Style := smsLabel
        else
          Marks.Style := smsNone;
end;

// Shows/hides mouse-over popup hints
procedure TMainForm.cbShowPopupHintsChange(Sender: TObject);
begin
  DatapointHintTool.Enabled := cbShowPopupHints.Checked;
end;

// Displays the last time tick on the x axis as '24:00' rather than '0:00'
procedure TMainForm.Chart1AxisList1GetMarkText(Sender: TObject; var AText: String;
  AMark: Double);
begin
  if AMark = 1.0 then AText := '24:00';
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Provide y axis labels
  MachineLabelsChartSource.Add(idxMachineA, idxMachineA, 'Machine'+LineEnding+'A');
  MachineLabelsChartSource.Add(idxMachineB, idxMachineB, 'Machine'+LineEnding+'B');
  MachineLabelsChartSource.Add(idxMachineC, idxMachineC, 'Machine'+LineEnding+'C');
  SetupNormalAxes;

  DateTimeIntervalChartSource.DateTimeStepFormat.HourFormat := 'hh:nn';
  DateTimeIntervalChartSource.SuppressPrevUnit := false;

  // Create the series and add their values
  MachineA_Series.AddXY(EncodeTime( 5, 0, 0, 0), EncodeTime( 9, 0, 0, 0), idxMachineA, 'Production', clProduction);
  MachineA_Series.AddXY(EncodeTime(10,30, 0, 0), EncodeTime(12,10, 0, 0), idxMachineA, 'Repair', clRepair);
  MachineA_Series.AddXY(EncodeTime(12,30, 0, 0), EncodeTime(18, 0, 0, 0), idxMachineA, 'Development', clDevelopment);
  MachineA_Series.AddXY(EncodeTime(20, 0, 0, 0), EncodeTime(23, 0, 0, 0), idxMachineA, 'Maintainance', clMaintainance);
  PrepareMarks(MachineA_Series);

  MachineB_Series.AddXY(EncodeTime( 0, 0, 0, 0), EncodeTime( 8, 0, 0, 0), idxMachineB, 'Repair', clRepair);
  MachineB_Series.AddXY(EncodeTime( 9, 0, 0, 0), EncodeTime(12,55, 0, 0), idxMachineB, 'Production', clProduction);
  MachineB_Series.AddXY(EncodeTime(13, 0, 0, 0), EncodeTime(17,25, 0, 0), idxMachineB, 'Production', clProduction);
  MachineB_Series.AddXY(EncodeTime(19, 0, 0, 0), EncodeTime(21,42, 0, 0), idxMachineB, 'Development', clDevelopment);
  PrepareMarks(MachineB_Series);

  MachineC_Series.AddXY(EncodeTime( 0, 0, 0, 0), EncodeTime( 6, 0, 0, 0), idxMachineC, 'Production', clProduction);
  MachineC_Series.AddXY(EncodeTime( 6,10, 0, 0), EncodeTime( 8,45, 0, 0), idxMachineC, 'Production', clProduction);
  MachineC_Series.AddXY(EncodeTime( 9, 0, 0, 0), EncodeTime(12, 0, 0, 0), idxMachineC, 'Production', clProduction);
  MachineC_Series.AddXY(EncodeTime(13, 0, 0, 0), EncodeTime(18,55, 0, 0), idxMachineC, 'Production', clProduction);
  MachineC_Series.AddXY(EncodeTime(19, 0, 0, 0), EncodeTime(23,50, 0, 0), idxMachineC, 'Maintainance', clMaintainance);
  PrepareMarks(MachineC_Series);
end;

// Composes the label text from the label value and each data point's
// state duration.
procedure TMainForm.GetMarkTextHandler(ASeries: TChartSeries;
  APointIndex, AXIndex, AYIndex: Integer; var AFormattedMark: String);
var
  txt: String;
  t1, t2: TDateTime;
begin
  with ASeries.Source[APointIndex]^ do
  begin
    txt := Text;
    t1 := GetX(0);
    t2 := GetX(1);
  end;
  AFormattedMark := Format('%s'+LineEnding+'%s', [txt, FormatDateTime('[hh]:nn', t2-t1, [fdoInterval])]);
end;

// Prepares the marks for the series and for the popup hints:
// no border, no background, centered, user-defined text (see GetMarkTextHandler)
procedure TMainForm.PrepareMarks(ASeries: TStateSeries);
begin
  ASeries.Marks.Style := smsLabel;
  ASeries.Marks.Frame.Visible := false;
  ASeries.Marks.LabelBrush.Style := bsClear;
  ASeries.Marks.LinkPen.Visible := false;
  ASeries.Marks.Distance := 0;
  ASeries.Marks.Alignment := taCenter;
  ASeries.Marks.Attachment := maCenter;
  ASeries.MarkPositions := lmpInside;
  ASeries.OnGetMarkText := @GetMarkTextHandler;
end;

// Sets axis properties for "normal" (horizontally oriented) state series
procedure TMainForm.SetupNormalAxes;
begin
  // Left axis marks
  Chart.LeftAxis.Marks.Source := MachineLabelsChartSource;
  Chart.LeftAxis.Marks.Style := smsLabel;
  Chart.LeftAxis.OnGetMarkText := nil;
  Chart.LeftAxis.TickLength := 0;

  // Bottom axis marks
  Chart.BottomAxis.Marks.Source := DateTimeIntervalChartSource;
  Chart.BottomAxis.Marks.Style := smsLabel;
  Chart.BottomAxis.OnGetMarkText := @Chart1AxisList1GetMarkText;
  Chart.BottomAxis.TickLength := 4;

  // Nicer grid for the y axis
  Chart.LeftAxis.Grid.Visible := false;
  Chart.BottomAxis.Grid.Visible := true;
  if Chart.LeftAxis.Minors.Count = 0 then
    with Chart.LeftAxis.Minors.Add do
    begin
      Intervals.Count := 1;
      Grid.Color := clSilver;
      Grid.Style := psSolid;
    end;
  Chart.LeftAxis.Minors[0].Visible := true;
  if Chart.BottomAxis.Minors.Count > 0 then
    Chart.BottomAxis.Minors[0].Visible := false;

  // Show a full day on the x axis
  Chart.BottomAxis.Range.Max := 1.0;
  Chart.BottomAxis.Range.Min := 0.0;
  Chart.BottomAxis.Range.UseMax := true;
  Chart.BottomAxis.Range.UseMin := true;
  Chart.LeftAxis.Range.UseMin := false;
  Chart.LeftAxis.Range.UseMax := false;

  // For top-to-bottom order of the machines (or use negative idxMachineXXXX values)
  Chart.LeftAxis.Inverted := true;
  Chart.BottomAxis.Inverted := false;
end;

// Sets axis properties for the case of "rotated" (vertically oriented) state series
procedure TMainForm.SetupRotatedAxes;
begin
  // Bottom axis marks
  Chart.BottomAxis.Marks.Source := MachineLabelsChartSource;
  Chart.BottomAxis.Marks.Style := smsLabel;
  Chart.BottomAxis.OnGetMarkText := nil;
  Chart.BottomAxis.TickLength := 0;

  // Left axis marks
  Chart.LeftAxis.Marks.Source := DateTimeIntervalChartSource;
  Chart.LeftAxis.Marks.Style := smsLabel;
  Chart.LeftAxis.OnGetMarkText := @Chart1AxisList1GetMarkText;
  Chart.LeftAxis.TickLength := 4;

  // Nicer grid for the x axis
  Chart.BottomAxis.Grid.Visible := false;
  Chart.LeftAxis.Grid.Visible := true;
  if Chart.BottomAxis.Minors.Count = 0 then
    with Chart.BottomAxis.Minors.Add do
    begin
      Intervals.Count := 1;
      Grid.Color := clSilver;
      Grid.Style := psSolid;
    end;
  Chart.BottomAxis.Minors[0].Visible := true;
  if Chart.LeftAxis.Minors.Count > 0 then
    Chart.LeftAxis.Minors[0].Visible := false;

  // Show a full day on the y axis
  Chart.LeftAxis.Range.Max := 1.0;
  Chart.LeftAxis.Range.Min := 0.0;
  Chart.LeftAxis.Range.UseMax := true;
  Chart.LeftAxis.Range.UseMin := true;
  Chart.BottomAxis.Range.UseMin := false;
  Chart.BottomAxis.Range.UseMax := false;

  // Restore left axis direction
  Chart.LeftAxis.Inverted := false;
end;

// A change in the trackbar position should be applied as new series BarHeight value.
procedure TMainForm.TrackBar1Change(Sender: TObject);
begin
  MachineA_Series.BarHeight := Trackbar1.Position * 0.01;
  MachineB_Series.BarHeight := Trackbar1.Position * 0.01;
  MachineC_Series.BarHeight := Trackbar1.Position * 0.01;
end;

end.

