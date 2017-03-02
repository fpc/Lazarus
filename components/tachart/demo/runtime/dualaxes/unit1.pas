unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, Forms,
  Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  TAChartAxis, TAChartAxisUtils, TALegend, TATransformations, TASeries;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
const
  LEFT_COLOR = clRed;
  RIGHT_COLOR = clBlue;
var
  leftAxis, rightAxis: TChartAxis;
  t: TAutoscaleAxisTransform;
  leftSeries, rightSeries: TLineSeries;
  i: Integer;
  axis: TChartAxis;
begin
  { *** Left axis *** }

  // It is already created, but we set some properties
  leftAxis := Chart1.LeftAxis;
  leftAxis.Marks.LabelFont.Color := LEFT_COLOR;
  leftAxis.Title.LabelFont.Color := LEFT_COLOR;
  leftAxis.Title.Caption := 'Left axis';
  leftAxis.Title.Visible := true;
  leftAxis.AxisPen.Color := LEFT_COLOR;
  leftAxis.AxisPen.Visible := true;
  leftAxis.TickColor := LEFT_COLOR;

  // Draw marks and axis title at the data part of the axis
  leftAxis.Marks.AtDataOnly := true;
  leftAxis.AtDataOnly := true;
  leftAxis.Title.PositionOnMarks := true;

  // AxisTransformation for left axis
  Chart1.LeftAxis.Transformations := TChartAxisTransformations.Create(self);
  t := TAutoscaleAxisTransform.Create(Chart1.LeftAxis.Transformations);
  t.Transformations := Chart1.LeftAxis.Transformations;
  t.MinValue := 0;
  t.MaxValue := 0.5;

  // Series for left axis
  leftSeries := TLineSeries.Create(Chart1);
  leftSeries.SeriesColor := LEFT_COLOR;
  leftSeries.Title := 'red (left)';
  leftSeries.AxisIndexY := Chart1.LeftAxis.Index;
  for i:=0 to 10 do
    leftSeries.AddXY(i, random);
  Chart1.AddSeries(leftSeries);


  { *** Right axis *** }

  rightAxis := Chart1.AxisList.Add;
  rightAxis.Alignment := calRight;
  rightAxis.Title.Caption  := 'Right axis';
  rightAxis.Title.Visible := true;
  rightAxis.Marks.LabelFont.Color := RIGHT_COLOR;
  rightAxis.Title.LabelFont.Color := RIGHT_COLOR;
  rightAxis.Title.LabelFont.Orientation := -900;  // in 1/10 degrees
  rightAxis.AxisPen.Color := RIGHT_COLOR;
  rightAxis.AxisPen.Visible := true;
  rightAxis.TickColor := RIGHT_COLOR;

  // Draw marks and axis title at the data part of the axis
  rightAxis.Marks.AtDataOnly := true;
  rightAxis.AtDataOnly := true;
  rightAxis.Title.PositionOnMarks := true;

  // Axis transformation for right axis
  rightAxis.Transformations := TChartAxisTransformations.Create(self);
  t := TAutoscaleAxisTransform.Create(rightAxis.Transformations);
  t.MinValue := 0.5;
  t.MaxValue := 1.0;
  t.Transformations := rightAxis.Transformations;

  // Series for right axis
  rightSeries := TLineSeries.Create(Chart1);
  rightSeries.SeriesColor := RIGHT_COLOR;
  rightSeries.Title := 'blue (right)';
  rightSeries.AxisIndexY := rightAxis.Index;
  for i:=5 to 20 do
    rightSeries.AddXY(i, random*10);
  Chart1.AddSeries(rightSeries);

  // Second series for right axis
  rightSeries := TLineSeries.Create(Chart1);
  rightSeries.SeriesColor := RIGHT_COLOR;
  rightSeries.LinePen.Style := psDot;
  rightSeries.Title := 'blue dotted (right)';
  rightSeries.AxisIndexY := rightAxis.Index;
  for i:=3 to 15 do
    rightSeries.AddXY(i, random*12 + 5);
  Chart1.AddSeries(rightSeries);


  { *** Misc *** }

  // Show legend
  Chart1.Legend.Visible := true;
  Chart1.Legend.Alignment := laBottomCenter;
  Chart1.Legend.ColumnCount := 3;

  // Axis grids
  for axis in Chart1.AxisList do begin
    axis.Grid.Style := psSolid;
    axis.Grid.Color := $E0E0E0;
  end;
end;

end.

