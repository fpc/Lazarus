unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, TAGraph, TASeries, TASources, TAChartUtils, TATools;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    RandomChartSource1: TRandomChartSource;
    sbChartHor: TScrollBar;
    sbChartVert: TScrollBar;
    procedure Chart1ExtentChanged(ASender: TChart);
    procedure sbChartHorChange(Sender: TObject);
    procedure sbChartVertChange(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Chart1ExtentChanged(ASender: TChart);
var
  fe, le: TDoubleRect;
  w: Double;
begin
  fe := ASender.GetFullExtent;
  le := ASender.LogicalExtent;
  w := fe.b.X - fe.a.X;
  with sbChartHor do begin
    if w <= 0 then
      Position := 0
    else
      Position := Round(WeightedAverage(Min, Max, (le.a.X - fe.a.X) / w));
  end;
  w := fe.b.Y - fe.a.Y;
  with sbChartVert do begin
    if w <= 0 then
      Position := 0
    else
      Position := Round(WeightedAverage(Max, Min, (le.a.Y - fe.a.Y) / w));
  end;
end;

procedure TForm1.sbChartHorChange(Sender: TObject);
var
  fe, le: TDoubleRect;
  d, w: Double;
begin
  w := sbChartHor.Max - sbChartHor.Min;
  if w = 0 then exit;
  fe := Chart1.GetFullExtent;
  le := Chart1.LogicalExtent;
  d := WeightedAverage(fe.a.X, fe.b.X, sbChartHor.Position / w);
  le.b.X += d - le.a.X;
  le.a.X := d;
  Chart1.LogicalExtent := le;
end;

procedure TForm1.sbChartVertChange(Sender: TObject);
var
  fe, le: TDoubleRect;
  d, w: Double;
begin
  w := sbChartVert.Max - sbChartVert.Min;
  if w = 0 then exit;
  fe := Chart1.GetFullExtent;
  le := Chart1.LogicalExtent;
  d := WeightedAverage(fe.b.Y, fe.a.Y, sbChartVert.Position / w);
  le.b.Y += d - le.a.Y;
  le.a.Y := d;
  Chart1.LogicalExtent := le;
end;

end.

