unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1FuncSeries1: TFuncSeries;
    Chart1UserDrawnSeries1: TUserDrawnSeries;
    Chart1YAxis: TLine;
    Chart1XAxis: TLine;
    UserDefinedChartSource1: TUserDefinedChartSource;
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure Chart1UserDrawnSeries1Draw(ACanvas: TCanvas; const ARect: TRect);
    procedure UserDefinedChartSource1GetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
  end;

var
  Form1: TForm1; 

implementation

uses
  TAChartUtils;

{ TForm1 }

procedure TForm1.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := Sin(AX);
end;

procedure TForm1.Chart1UserDrawnSeries1Draw(
  ACanvas: TCanvas; const ARect: TRect);
var
  a: TDoublePoint = (X: -1; Y: -1);
  b: TDoublePoint = (X: 1; Y: 1);
  r: TRect;
begin
  r.TopLeft := Chart1.GraphToImage(a);
  r.BottomRight := Chart1.GraphToImage(b);
  ACanvas.Pen.Mode := pmCopy;
  ACanvas.Pen.Color := clRed;
  ACanvas.Pen.Style := psDash;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Ellipse(r);
end;

procedure TForm1.UserDefinedChartSource1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := AIndex - ASource.PointsNumber / 2;
  AItem.Y := Cos(AItem.X);
end;

initialization
  {$I main.lrs}

end.

