unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbRotateBar: TCheckBox;
    cbNegative: TCheckBox;
    cbRotateArea: TCheckBox;
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    Chart1BarSeries1: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    cbRotateLine: TCheckBox;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    procedure cbNegativeChange(Sender: TObject);
    procedure cbRotateAreaChange(Sender: TObject);
    procedure cbRotateBarChange(Sender: TObject);
    procedure cbRotateLineChange(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  TACustomSeries;

procedure Rotate(ASeries: TBasicPointSeries);
var
  t: Integer;
begin
  with ASeries do begin
    t := AxisIndexX;
    AxisIndexX := AxisIndexY;
    AxisIndexY := t;
  end;
end;

{ TForm1 }

procedure TForm1.cbNegativeChange(Sender: TObject);
begin
  RandomChartSource1.YMin := -RandomChartSource1.YMin;
end;

procedure TForm1.cbRotateAreaChange(Sender: TObject);
begin
  Rotate(Chart1AreaSeries1)
end;

procedure TForm1.cbRotateBarChange(Sender: TObject);
begin
  Rotate(Chart1BarSeries1)
end;

procedure TForm1.cbRotateLineChange(Sender: TObject);
begin
  Rotate(Chart1LineSeries1)
end;

end.

