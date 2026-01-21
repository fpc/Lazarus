{ A simple demonstration of a GanttChart created by means of a TStateSeries.

  Data are stored in the built-in ListChartSource of the series.
}

unit uSimpleGanttFrame;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Graphics, Forms, Controls, StdCtrls, ExtCtrls,
  TAGraph, TAChartUtils, TACustomSource, TAIntervalSources, TASources,
  TACustomSeries, TASeries, TAMultiSeries, TATools;

type
  TSimpleGanttFrame = class(TFrame)
    cbRotated: TCheckBox;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    GanttChart: TChart;
    GanttChartToolset: TChartToolset;
    GanttDataPointHintTool: TDataPointHintTool;
    GanttPanDragTool: TPanDragTool;
    GanttSeries: TStateSeries;
    GanttZoomDragTool: TZoomDragTool;
    Panel1: TPanel;
    procedure cbRotatedChange(Sender: TObject);
    procedure GanttSeriesGetMarkText({%H-}ASeries: TChartSeries;
      APointIndex, {%H-}AXIndex, {%H-}AYIndex: Integer; var AFormattedMark: String);
  private
    procedure PrepareData;

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TSimpleGanttFrame.Create(AOwner: TComponent);
begin
  inherited;
  PrepareData;
end;

// Checkbox clicked to toggle between "normal" and "rotated" state series
// (horizontal or vertical orientation)
procedure TSimpleGanttFrame.cbRotatedChange(Sender: TObject);
begin
  if cbRotated.Checked then
  begin
    GanttSeries.AxisIndexX := 0;
    GanttSeries.AxisIndexY := 1;

    // Bottom axis marks
    GanttChart.BottomAxis.Marks.Source := GanttSeries.ListSource;
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

    // Left axis marks
    GanttChart.LeftAxis.Marks.Source := GanttSeries.ListSource;
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

{ Constructs the Mark text to be used in the popup hint. }
procedure TSimpleGanttFrame.GanttSeriesGetMarkText(ASeries: TChartSeries;
  APointIndex, AXIndex, AYIndex: Integer; var AFormattedMark: String);
var
  tPlan1, tPlan2: Double;
  txt: String;
  item: PChartDataItem;
begin
  item := TStateSeries(ASeries).Source.Item[APointIndex];
  txt := item^.Text;
  tPlan1 := item^.GetX(0);
  tPlan2 := item^.GetX(1);
  if tPlan1 = tPlan2 then
    AFormattedMark := Format(
      '"%s"' + LineEnding +
      '● due %s', [
      txt, DateToStr(tPlan1)
    ])
  else
    AFormattedMark := Format(
      'Task "%s":' + LineEnding +
      '● %s - %s', [
      txt, DateToStr(tPlan1), DateToStr(tPlan2)
    ]);
end;

procedure TSimpleGanttFrame.PrepareData;
begin
  GanttSeries.ListSource.XCount := 2;
  GanttSeries.AddXY(EncodeDate(2024,2,3), EncodeDate(2024,2,4), 0, 'Kick-off', clGreen);
  GanttSeries.AddXY(EncodeDate(2024,2,4), EncodeDate(2024,2,8), 1, 'Activity 1', clRed);
  GanttSeries.AddXY(EncodeDate(2024,2,8), EncodeDate(2024,2,11), 2, 'Activity 2', clYellow);
  GanttSeries.AddXY(EncodeDate(2024,2,11), EncodeDate(2024,2,21), 3, 'Activity 3', clFuchsia);
  GanttSeries.AddXY(EncodeDate(2024,2,21), EncodeDate(2024,2,21), 4, 'Intermediate Milestone', clBlack);
  GanttSeries.AddXY(EncodeDate(2024,2,21), EncodeDate(2024,2,28), 5, 'Activity 4', clSkyBlue);
  GanttSeries.AddXY(EncodeDate(2024,2,28), EncodeDate(2024,3,7), 6, 'Activity 5', clOlive);
  GanttSeries.AddXY(EncodeDate(2024,3,7), EncodeDate(2024,3,7), 7, 'Final Milestone', clBlack);

  // Show the task titles as y axis labels
  GanttChart.LeftAxis.Marks.Source := GanttSeries.ListSource;
  GanttChart.LeftAxis.Marks.Style := smsLabel;
  GanttChart.LeftAxis.Inverted := true;
end;

end.

