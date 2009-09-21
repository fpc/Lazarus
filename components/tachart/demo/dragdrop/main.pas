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
    procedure Chart1LineSeries1GetMark(out AFormattedMark: String;
      AIndex: Integer);
    procedure Chart1MouseDown(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Chart1MouseMove(
      Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Chart1MouseUp(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    FDragIndex: Integer;
    FNearestIndex: Integer;
  end;

var
  Form1: TForm1; 

implementation

uses
  TAChartUtils;

type
  TLineSeriesAccess = class(TLineSeries);

{ TForm1 }

procedure TForm1.Chart1LineSeries1GetMark(
  out AFormattedMark: String; AIndex: Integer);
begin
  if AIndex = FNearestIndex then
    AFormattedMark := Chart1LineSeries1.DefaultFormattedMark(AIndex)
  else
    AFormattedMark := '';
end;

procedure TForm1.Chart1MouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Unused(Button);
  Unused(X, Y);
  if Shift = [ssShift, ssLeft] then
    FDragIndex := FNearestIndex;
end;

procedure TForm1.Chart1MouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  newNearest: Integer;
  pt, img: TPoint;
  val: TDoublePoint;
begin
  Unused(Shift);
  pt := Point(X, Y);
  if
    TLineSeriesAccess(Chart1LineSeries1).
      GetNearestPoint(@PointDist, pt, newNearest, img, val)
  then begin
    if PointDist(pt, img) <= Sqr(Chart1LineSeries1.Pointer.HorizSize) then begin
      if newNearest <> FNearestIndex then begin
        FNearestIndex := newNearest;
        Chart1.Invalidate;
      end;
    end
    else
      FNearestIndex := -1;
  end;
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
  FNearestIndex := -1;
end;

initialization
  {$I main.lrs}

end.

