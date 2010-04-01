unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, TAGraph, TASeries, TASources, TATools;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1FuncSeries1: TFuncSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1PanAny: TPanDragTool;
    ChartToolset1PanHor: TPanDragTool;
    ChartToolset1PanVert: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ChartToolset1ZoomOut: TZoomClickTool;
    ChartToolset1ZoomIn: TZoomClickTool;
    Panel1: TPanel;
    rgZoom: TRadioGroup;
    RandomChartSource1: TRandomChartSource;
    rgPan: TRadioGroup;
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure rgPanClick(Sender: TObject);
    procedure rgZoomClick(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := Sin(AX * 10) + 0.7 * Cos(AX * 30) + 0.3 * Sin(AX * 80);
end;

procedure TForm1.rgPanClick(Sender: TObject);
var
  b: Boolean;
begin
  b := rgPan.ItemIndex = 0;
  ChartToolset1PanAny.Enabled := b;
  ChartToolset1PanHor.Enabled := not b;
  ChartToolset1PanVert.Enabled := not b;
end;

procedure TForm1.rgZoomClick(Sender: TObject);
var
  b: Boolean;
begin
  b := rgZoom.ItemIndex = 0;
  ChartToolset1ZoomDragTool1.Enabled := b;
  ChartToolset1ZoomIn.Enabled := not b;
  ChartToolset1ZoomOut.Enabled := not b;
end;

end.

