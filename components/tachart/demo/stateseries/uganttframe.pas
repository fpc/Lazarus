unit uGanttFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls,
  TAGraph, TAChartUtils, TAIntervalSources, TASources, TATools,
  TACustomSeries, TASeries, TAMultiSeries;

type
  TGanttFrame = class(TFrame)
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    GanttChart: TChart;
    GanttChartToolset: TChartToolset;
    GanttCompletedSeries: TStateSeries;
    GanttDataPointHintTool: TDataPointHintTool;
    GanttPanDragTool: TPanDragTool;
    GanttSeries: TStateSeries;
    GanttZoomDragTool: TZoomDragTool;
    MilestoneSeries: TLineSeries;
    TasksChartSource: TListChartSource;
    procedure GanttSeriesBarHeightChanged(Sender: TObject);
    procedure GanttSeriesGetMarkText(ASeries: TChartSeries; APointIndex, AXIndex
      , AYIndex: Integer; var AFormattedMark: String);
  private
    procedure PrepareData;

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TGanttFrame.Create(AOwner: TComponent);
begin
  inherited;
  PrepareData;
end;

procedure TGanttFrame.GanttSeriesGetMarkText(ASeries: TChartSeries;
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

procedure TGanttFrame.GanttSeriesBarHeightChanged(Sender: TObject);
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

procedure TGanttFrame.PrepareData;
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
  Milestoneseries.AddXY(EncodeDate(2024,2,21), 4, '', clGreen);      // Milestone 1 reached
  Milestoneseries.AddXY(EncodeDate(2024,3,7), 7, '', clSilver);      // Milestone 2 open

  GanttChart.LeftAxis.Marks.Source := TasksChartSource;
  GanttChart.LeftAxis.Marks.Style := smsLabel;
  GanttChart.LeftAxis.Inverted := true;
end;

end.

