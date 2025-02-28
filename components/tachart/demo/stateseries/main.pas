unit Main;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, SysUtils, Classes, Math,
  Graphics, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, LCLVersion,
  TAGraph, TAIntervalSources, TASources, TAChartUtils, TATextElements, TATools,
  TAChartAxisUtils, TACustomSeries, TASeries, TAMultiSeries, TACustomSource, TADrawUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    GanttChartToolset: TChartToolset;
    GanttChart: TChart;
    GanttDataPointHintTool: TDataPointHintTool;
    GanttPanDragTool: TPanDragTool;
    GanttZoomDragTool: TZoomDragTool;
    GanttCompletedSeries: TStateSeries;
    TasksChartSource: TListChartSource;
    MilestoneSeries: TLineSeries;
    GanttSeries: TStateSeries;
    State_Chart: TChart;
    cbAdjustMargin: TCheckBox;
    FlowPanel1: TFlowPanel;
    Label1: TLabel;
    MachineA_Series: TStateSeries;
    MachineB_Series: TStateSeries;
    MachineC_Series: TStateSeries;
    StateChartToolset: TChartToolset;
    cbSeriesMarks: TCheckBox;
    cbShowPopupHints: TCheckBox;
    StateChartDataPointDragTool: TDataPointDragTool;
    StateChartPanDragTool: TPanDragTool;
    StateChartZoomDragTool: TZoomDragTool;
    cbRotated: TCheckBox;
    StateChartDataPointHintTool: TDataPointHintTool;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    MachineLabelsChartSource: TListChartSource;
    PageControl1: TPageControl;
    Panel1: TPanel;
    pgStatePlot: TTabSheet;
    pgGanttPlot: TTabSheet;
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
    // State chart
    procedure CreateStateChart;
    procedure GetMarkTextHandler(ASeries: TChartSeries;
      APointIndex, {%H-}AXIndex, {%H-}AYIndex: Integer; var AFormattedMark: String);
    procedure PrepareMarks(ASeries: TStateSeries);
    procedure SetupNormalAxes;
    procedure SetupRotatedAxes;

  private
    // Gantt chart
    procedure CreateGanttChart;
  published
    procedure GanttCompletedSeriesBarHeightChanged(Sender: TObject);
    procedure GanttSeriesGetMarkText(ASeries: TChartSeries;
      APointIndex, AXIndex, AYIndex: Integer; var AFormattedMark: String);

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
    ext := State_Chart.LogicalExtent;
    if MachineA_Series.IsRotated then
    begin
      State_Chart.Extent.XMin := Floor(ext.a.x) + 0.5;
      State_Chart.Extent.XMax := Ceil(ext.b.x) - 0.5;
      State_Chart.Extent.UseXMin := true;
      State_Chart.Extent.UseXMax := true;
    end else
    begin
      State_Chart.Extent.YMin := Floor(ext.a.y) + 0.5;
      State_Chart.Extent.YMax := Ceil(ext.b.y) - 0.5;
      State_Chart.Extent.UseYMin := true;
      State_Chart.Extent.UseYMax := true;
    end;
  end else
  begin
    State_Chart.Extent.UseXMin := false;
    State_Chart.Extent.UseXMax := false;
    State_Chart.Extent.UseYMin := false;
    State_Chart.Extent.UseYMax := false;
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
  for i := 0 to State_Chart.SeriesCount-1 do
    if State_Chart.Series[i] is TStateSeries then
      with TStateSeries(State_Chart.Series[i]) do
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
  for i := 0 to State_Chart.SeriesCount-1 do
    if State_Chart.Series[i] is TStateSeries then
      with TStateSeries(State_Chart.Series[i]) do
        if cbSeriesMarks.Checked then
          Marks.Style := smsLabel
        else
          Marks.Style := smsNone;
end;

// Shows/hides mouse-over popup hints
procedure TMainForm.cbShowPopupHintsChange(Sender: TObject);
begin
  StateChartDataPointHintTool.Enabled := cbShowPopupHints.Checked;
end;

// Displays the last time tick on the x axis as '24:00' rather than '0:00'
procedure TMainForm.Chart1AxisList1GetMarkText(Sender: TObject; var AText: String;
  AMark: Double);
begin
  if AMark = 1.0 then AText := '24:00';
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CreateStateChart;
  CreateGanttChart;
end;

procedure TMainForm.GanttSeriesGetMarkText(ASeries: TChartSeries;
  APointIndex, AXIndex, AYIndex: Integer; var AFormattedMark: String);
var
  tPlan1, tPlan2: Double;
  tComplete1, tComplete2: Double;
  txt: String;
begin
  txt := GanttSeries.Source.Item[APointIndex]^.Text;
  tPlan1 := GanttSeries.XValues[APointIndex, 0];
  tPlan2 := GanttSeries.XValues[APointIndex, 1];
  tComplete1 := GanttCompletedSeries.XValues[APointIndex, 0];
  tComplete2 := GanttCompletedSeries.XValues[APointIndex, 1];
  AFormattedMark := Format('Task "%s": %.0f %% complete', [txt, (tComplete2 - tComplete1)/(tPlan2 - tPlan1)*100]);
end;

procedure TMainForm.GanttCompletedSeriesBarHeightChanged(Sender: TObject);
var
  h: Integer;
begin
  if GanttChart.ScalingValid then
  begin
    h := round(GanttSeries.GetImgBarHeight * 0.66);
    MilestoneSeries.Pointer.HorizSize := h;
    MilestoneSeries.Pointer.VertSize := h;
  end;
end;

procedure TMainForm.CreateGanttChart;
begin
  TasksChartSource.Add(0, 0, 'Kick-off');
  TasksChartSource.Add(1, 1, 'Activity 1');
  TasksChartSource.Add(2, 2, 'Activity 2');
  TasksChartSource.Add(3, 3, 'Activity 3');
  TasksChartSource.Add(4, 4, 'Intermediate Milestone');
  TasksChartSource.Add(5, 5, 'Activity 4');
  TasksChartSource.Add(6, 6, 'Activity 5');
  TasksChartSource.Add(7, 7, 'Final Milestone');

  // The x/y values in the TasksChartSource are the y values for the
  // GanttSeries data.

  // GanttSeries represents the project plan
  GanttSeries.AddXY(EncodeDate(2024,2,3), EncodeDate(2024,2,4), 0, TasksChartSource.Item[0]^.Text);
  GanttSeries.AddXY(EncodeDate(2024,2,4), EncodeDate(2024,2,8), 1, TasksChartSource.Item[1]^.Text);
  GanttSeries.AddXY(EncodeDate(2024,2,8), EncodeDate(2024,2,11), 2, TasksChartSource.Item[2]^.Text);
  GanttSeries.AddXY(EncodeDate(2024,2,11), EncodeDate(2024,2,21), 3, TasksChartSource.Item[3]^.Text);
  GanttSeries.AddXY(EncodeDate(2024,2,21), EncodeDate(2024,2,28), 5, TasksChartSource.Item[5]^.Text);
  GanttSeries.AddXY(EncodeDate(2024,2,28), EncodeDate(2024,3,7), 6, TasksChartSource.Item[6]^.Text);

  // GanttCompletedSeries contains only the activities from GanttSeries which
  // are completed to some extent
  GanttCompletedSeries.AddXY(GanttSeries.XValues[0,0], GanttSeries.XValues[0,1], 0);     // Kick-off is 100% finished
  GanttCompletedSeries.AddXY(GanttSeries.XValues[1,0], GanttSeries.XValues[1,1], 1);     // Activity 1 is 100% finished
  GanttCompletedSeries.AddXY(GanttSeries.XValues[2,0], GanttSeries.XValues[2,1], 2);     // Activity 2 is 100% finished
  GanttCompletedSeries.AddXY(GanttSeries.XValues[3,0], GanttSeries.XValues[3,1], 3);     // Activity 3 is 100% finished
  GanttCompletedSeries.AddXY(GanttSeries.XValues[4,0], GanttSeries.XValues[4,0]+6, 5);   // Activity 4 is almost finished
  GanttCompletedSeries.AddXY(GanttSeries.XValues[5,0], GanttSeries.XValues[5,0]+1, 6);   // Activity 6 has a lot left to be done
  GanttCompletedSeries.ZPosition := 1;   // draw this series over the GanttSeries

  // Milestones
  Milestoneseries.AddXY(EncodeDate(2024,2,21), 4, '', clGreen);       // Milestone 1 reached
  Milestoneseries.AddXY(EncodeDate(2024,3,7), 7, '', clSilver);      // Milestone 2 open

  GanttChart.LeftAxis.Marks.Source := TasksChartSource;
  GanttChart.LeftAxis.Marks.Style := smsLabel;
  GanttChart.LeftAxis.Inverted := true;
end;

procedure TMainForm.CreateStateChart;
begin
  // Provide y axis labels
  MachineLabelsChartSource.Add(idxMachineA, idxMachineA, 'Machine'+LineEnding+'A');
  MachineLabelsChartSource.Add(idxMachineB, idxMachineB, 'Machine'+LineEnding+'B');
  MachineLabelsChartSource.Add(idxMachineC, idxMachineC, 'Machine'+LineEnding+'C');
  SetupNormalAxes;

  //DateTimeIntervalChartSource.DateTimeStepFormat.HourFormat := 'hh:nn';
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
  State_Chart.LeftAxis.Marks.Source := MachineLabelsChartSource;
  State_Chart.LeftAxis.Marks.Style := smsLabel;
  State_Chart.LeftAxis.OnGetMarkText := nil;
  State_Chart.LeftAxis.TickLength := 0;

  // Bottom axis marks
  State_Chart.BottomAxis.Marks.Source := DateTimeIntervalChartSource;
  State_Chart.BottomAxis.Marks.Style := smsLabel;
  State_Chart.BottomAxis.OnGetMarkText := @Chart1AxisList1GetMarkText;
  State_Chart.BottomAxis.TickLength := 4;

  // Nicer grid for the y axis
  State_Chart.LeftAxis.Grid.Visible := false;
  State_Chart.BottomAxis.Grid.Visible := true;
  if State_Chart.LeftAxis.Minors.Count = 0 then
    with State_Chart.LeftAxis.Minors.Add do
    begin
      Intervals.Count := 1;
      Grid.Color := clSilver;
      Grid.Style := psSolid;
    end;
  State_Chart.LeftAxis.Minors[0].Visible := true;
  if State_Chart.BottomAxis.Minors.Count > 0 then
    State_Chart.BottomAxis.Minors[0].Visible := false;

  // Show a full day on the x axis
  State_Chart.BottomAxis.Range.Max := 1.0;
  State_Chart.BottomAxis.Range.Min := 0.0;
  State_Chart.BottomAxis.Range.UseMax := true;
  State_Chart.BottomAxis.Range.UseMin := true;
  State_Chart.LeftAxis.Range.UseMin := false;
  State_Chart.LeftAxis.Range.UseMax := false;

  // For top-to-bottom order of the machines (or use negative idxMachineXXXX values)
  State_Chart.LeftAxis.Inverted := true;
  State_Chart.BottomAxis.Inverted := false;
end;

// Sets axis properties for the case of "rotated" (vertically oriented) state series
procedure TMainForm.SetupRotatedAxes;
begin
  // Bottom axis marks
  State_Chart.BottomAxis.Marks.Source := MachineLabelsChartSource;
  State_Chart.BottomAxis.Marks.Style := smsLabel;
  State_Chart.BottomAxis.OnGetMarkText := nil;
  State_Chart.BottomAxis.TickLength := 0;

  // Left axis marks
  State_Chart.LeftAxis.Marks.Source := DateTimeIntervalChartSource;
  State_Chart.LeftAxis.Marks.Style := smsLabel;
  State_Chart.LeftAxis.OnGetMarkText := @Chart1AxisList1GetMarkText;
  State_Chart.LeftAxis.TickLength := 4;

  // Nicer grid for the x axis
  State_Chart.BottomAxis.Grid.Visible := false;
  State_Chart.LeftAxis.Grid.Visible := true;
  if State_Chart.BottomAxis.Minors.Count = 0 then
    with State_Chart.BottomAxis.Minors.Add do
    begin
      Intervals.Count := 1;
      Grid.Color := clSilver;
      Grid.Style := psSolid;
    end;
  State_Chart.BottomAxis.Minors[0].Visible := true;
  if State_Chart.LeftAxis.Minors.Count > 0 then
    State_Chart.LeftAxis.Minors[0].Visible := false;

  // Show a full day on the y axis
  State_Chart.LeftAxis.Range.Max := 1.0;
  State_Chart.LeftAxis.Range.Min := 0.0;
  State_Chart.LeftAxis.Range.UseMax := true;
  State_Chart.LeftAxis.Range.UseMin := true;
  State_Chart.BottomAxis.Range.UseMin := false;
  State_Chart.BottomAxis.Range.UseMax := false;

  // Restore left axis direction
  State_Chart.LeftAxis.Inverted := false;
end;

// A change in the trackbar position should be applied as new series BarHeight value.
procedure TMainForm.TrackBar1Change(Sender: TObject);
begin
  MachineA_Series.BarHeight := Trackbar1.Position * 0.01;
  MachineB_Series.BarHeight := Trackbar1.Position * 0.01;
  MachineC_Series.BarHeight := Trackbar1.Position * 0.01;
end;

end.

