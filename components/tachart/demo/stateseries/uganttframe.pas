unit uGanttFrame;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, Math,
  Graphics, Forms, Controls, StdCtrls, ExtCtrls,
  TAGraph, TAChartUtils, TACustomSource, TAIntervalSources, TASources,
  TACustomSeries, TASeries, TAMultiSeries, TATools,
  uGanttData;

type
  TGanttFrame = class(TFrame)
    cbShowCompleteness: TCheckBox;
    cbRotated: TCheckBox;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    GanttChart: TChart;
    GanttChartToolset: TChartToolset;
    GanttCompletedSeries: TStateSeries;
    GanttDataPointHintTool: TDataPointHintTool;
    GanttPanDragTool: TPanDragTool;
    GanttSeries: TStateSeries;
    GanttZoomDragTool: TZoomDragTool;
    Panel1: TPanel;
    Tasks_ChartSource: TUserDefinedChartSource;
    Completed_ChartSource: TUserDefinedChartSource;
    procedure cbRotatedChange(Sender: TObject);
    procedure cbShowCompletenessChange(Sender: TObject);
    procedure GanttSeriesGetMarkText({%H-}ASeries: TChartSeries;
      APointIndex, {%H-}AXIndex, {%H-}AYIndex: Integer; var AFormattedMark: String);
    procedure Tasks_ChartSourceGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
  private
    FTasks: TGanttTaskList;
    procedure PrepareData;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

constructor TGanttFrame.Create(AOwner: TComponent);
begin
  inherited;

  FTasks := TGanttTaskList.Create;
  FTasks.AddTask('Kick-off', EncodeDate(2024,2,3), 1, 100);
  FTasks.AddTask('Activity 1', EncodeDate(2024,2,4), 4, 100);
  FTasks.AddTask('Activity 2', EncodeDate(2024,2,8), 3, 100);
  FTasks.AddTask('Activity 3', EncodeDate(2024,2,11), 10, 100);
  FTasks.AddMilestone('Intermediate Milestone', EncodeDate(2024,2,21), true);
  FTasks.AddTask('Activity 4', EncodeDate(2024,2,21), 7, 80);
  FTasks.AddTask('Activity 5', EncodeDate(2024,2,28), 8, 10);
  FTasks.AddMilestone('Final Milestone', EncodeDate(2024,3,7), false);

  PrepareData;
end;

destructor TGanttFrame.Destroy;
begin
  FTasks.Free;
  inherited Destroy;
end;

// Checkbox clicked to toggle between "normal" and "rotated" state series
// (horizontal or vertical orientation)
procedure TGanttFrame.cbRotatedChange(Sender: TObject);
begin
  if cbRotated.Checked then
  begin
    GanttSeries.AxisIndexX := 0;
    GanttSeries.AxisIndexY := 1;
    GanttCompletedSeries.AxisIndexX := 0;
    GanttCompletedSeries.AxisIndexY := 1;

    // Bottom axis marks
    GanttChart.BottomAxis.Marks.Source := Tasks_ChartSource;
    GanttChart.BottomAxis.Marks.Style := smsLabel;
    GanttChart.BottomAxis.Marks.SourceExchangeXY := true;
    GanttChart.BottomAxis.Marks.LabelFont.Orientation := 900;
    GanttChart.BottomAxis.Grid.Visible := false;

    // Left axis marks
    GanttChart.LeftAxis.Marks.Source := DateTimeIntervalChartSource;
    GanttChart.LeftAxis.Marks.Style := smsLabel;
    GanttChart.LeftAxis.Grid.Visible := true;

    GanttChart.Margins.Top := 0;
    GanttChart.Margins.Left := 10;
  end else
  begin
    GanttSeries.AxisIndexX := 1;
    GanttSeries.AxisIndexY := 0;
    GanttCompletedSeries.AxisIndexX := 1;
    GanttCompletedSeries.AxisIndexY := 0;

    // Left axis marks
    GanttChart.LeftAxis.Marks.Source := Tasks_ChartSource;
    GanttChart.LeftAxis.Marks.Style := smsLabel;
    GanttChart.LeftAxis.Grid.Visible := false;

    // Bottom axis marks
    GanttChart.BottomAxis.Marks.Source := DateTimeIntervalChartSource;
    GanttChart.BottomAxis.Marks.Style := smsLabel;
    GanttChart.BottomAxis.Marks.SourceExchangeXY := false;
    GanttChart.BottomAxis.Marks.LabelFont.Orientation := 0;
    GanttChart.BottomAxis.Grid.Visible := true;

    GanttChart.Margins.Left := 0;
    GanttChart.Margins.Top := 10;
  end;
end;

procedure TGanttFrame.cbShowCompletenessChange(Sender: TObject);
begin
  GanttCompletedSeries.Active := cbShowCompleteness.Checked;
end;

{ Constructs the Mark text to be used in the popup hint. }
procedure TGanttFrame.GanttSeriesGetMarkText(ASeries: TChartSeries;
  APointIndex, AXIndex, AYIndex: Integer; var AFormattedMark: String);
const
  NO_Yes: array[boolean] of string = ('not ', '');
var
  t, tPlan1, tPlan2: Double;
  completeness: Double;
  txt: String;
  item: TBasicGanttItem;
begin
  item := FTasks[APointIndex];
  txt := item.Title;
  if item is TGanttTask then
  begin
    tPlan1 := TGanttTask(item).StartDate;
    tPlan2 := TGanttTask(item).EndDate;
    completeness := TGanttTask(item).PercentageComplete;
    AFormattedMark := Format(
      'Task "%s":' + LineEnding +
      '⯄ %s - %s' + LineEnding +
      '⯄ %.0f%% complete', [
      txt, DateToStr(tPlan1), DateToStr(tPlan2), completeness
    ]);
  end
  else
  if item is TGanttMilestone then
  begin
    t := TGanttMileStone(item).DateDue;
    AFormattedMark := Format(
      '"%s":' + LineEnding +
      '⯄ due %s' + LineEnding +
      '⯄ %scomplete', [
      txt, DateToStr(t), NO_YES[TGanttMilestone(item).Complete]
    ]);
  end;
end;

{ Extracts the data for the data point at index AIndex and transfers them
  to the chartdataitem AItem needed by the series. }
procedure TGanttFrame.Tasks_ChartSourceGetChartDataItem(ASource:
  TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
var
  dt1, dt2: TDateTime;
begin
  if FTasks[AIndex] is TGanttTask then
  begin
    dt1 := TGanttTask(FTasks[AIndex]).StartDate;
    dt2 := TGanttTask(FTasks[AIndex]).EndDate;
    if ASource = Completed_ChartSource then
      dt2 := dt1 + TGanttTask(FTasks[AIndex]).PercentageComplete/100 * (dt2 - dt1);
  end else
  if FTasks[AIndex] is TGanttMilestone then
  begin
    if TGanttMilestone(FTasks[AIndex]).Complete or (ASource = Tasks_ChartSource) then
      dt1 := TGanttMilestone(FTasks[AIndex]).DateDue
    else
      dt1 := NaN;
    dt2 := dt1;
  end;
  AItem.Text := FTasks[AIndex].Title;
  AItem.SetX(0, dt1);
  AItem.SetX(1, dt2);
  AItem.SetY(AIndex);
end;

procedure TGanttFrame.PrepareData;
begin
  // Series with planned tasks
  Tasks_ChartSource.XCount := 2;
  Tasks_ChartSource.PointsNumber := FTasks.Count;
  GanttSeries.Source := Tasks_ChartSource;

  // Series showing to which percentage the planned tasks are completed
  Completed_ChartSource.XCount := 2;
  Completed_ChartSource.PointsNumber := FTasks.Count;
  GanttCompletedSeries.Source := Completed_ChartSource;
  GanttCompletedSeries.ZPosition := 1;  // Draw it before GanttSeries

  // Show the task titles as y axis labels
  GanttChart.LeftAxis.Marks.Source := Tasks_ChartSource;
  GanttChart.LeftAxis.Marks.Style := smsLabel;
  GanttChart.LeftAxis.Inverted := true;
end;

end.

