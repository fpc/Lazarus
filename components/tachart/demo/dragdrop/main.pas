unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, TAGraph, TASeries;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    procedure Chart1MouseDown(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Chart1MouseMove(
      Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Chart1MouseUp(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    FDragIndex: Integer;
  end;

var
  Form1: TForm1; 

implementation

uses
  TAChartUtils;

type
  TLineSeriesAccess = class(TLineSeries);

{ TForm1 }

procedure TForm1.Chart1MouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt, img: TPoint;
  val: TDoublePoint;
begin
  Unused(Button);
  pt := Point(X, Y);
  if
    (Shift = [ssShift, ssLeft]) and
      TLineSeriesAccess(Chart1LineSeries1).GetNearestPoint(
        @PointDist, pt, FDragIndex, img, val) and
    (PointDist(pt, img) <= Sqr(Chart1LineSeries1.Pointer.HorizSize))
  then
  else
    FDragIndex := -1;
end;

procedure TForm1.Chart1MouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Unused(Shift);
  Unused(X, Y);
  if FDragIndex < 0 then exit;
  Chart1LineSeries1.SetXValue(FDragIndex, Chart1.XImageToGraph(X));
  Chart1LineSeries1.SetYValue(FDragIndex, Chart1.YImageToGraph(Y));
end;

procedure TForm1.Chart1MouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Unused(Button, Shift);
  Unused(X, Y);
  FDragIndex := -1;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  RandSeed := 675402;
  for i := 1 to 10 do
    Chart1LineSeries1.AddXY(i, Random(20) - 10);
  FDragIndex := -1;
end;

initialization
  {$I main.lrs}

end.

