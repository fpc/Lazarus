unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, Types, TAGraph, TASeries, TASources, TATools;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    ChartLine2: TConstantLine;
    ChartLine1: TConstantLine;
    Chart1FuncSeries1: TFuncSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointDragTool1: TDataPointDragTool;
    ChartToolset1PanAny: TPanDragTool;
    ChartToolset1PanHor: TPanDragTool;
    ChartToolset1PanVert: TPanDragTool;
    ChartToolset1ReticuleTool1: TReticuleTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ChartToolset1ZoomOut: TZoomClickTool;
    ChartToolset1ZoomIn: TZoomClickTool;
    cbFixedPoint: TCheckBox;
    Panel1: TPanel;
    Panel2: TPanel;
    rgZoom: TRadioGroup;
    RandomChartSource1: TRandomChartSource;
    rgPan: TRadioGroup;
    procedure cbFixedPointChange(Sender: TObject);
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure ChartToolset1DataPointDragTool1BeforeMouseMove(ATool: TChartTool;
      APoint: TPoint);
    procedure rgPanClick(Sender: TObject);
    procedure rgZoomClick(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.cbFixedPointChange(Sender: TObject);
begin
  ChartToolset1ZoomIn.FixedPoint := cbFixedPoint.Checked;
  ChartToolset1ZoomOut.FixedPoint := cbFixedPoint.Checked;
end;

procedure TForm1.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := Sin(AX * 10) + 0.7 * Cos(AX * 30) + 0.3 * Sin(AX * 80);
end;

procedure TForm1.ChartToolset1DataPointDragTool1BeforeMouseMove(
  ATool: TChartTool; APoint: TPoint);
const
  D = 10;
begin
  with ATool as TDataPointDragTool do begin
    if
      (Series = ChartLine1) and
        (APoint.X > Chart1.XGraphToImage(ChartLine2.Position) - D) or
      (Series = ChartLine2) and
        (APoint.X < Chart1.XGraphToImage(ChartLine1.Position) + D)
    then
      Handled;
  end;
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
  b := rgZoom.ItemIndex <= 1;
  ChartToolset1ZoomDragTool1.Enabled := b;
  ChartToolset1ZoomDragTool1.Proportional := rgZoom.ItemIndex = 1;
  ChartToolset1ZoomIn.Enabled := not b;
  ChartToolset1ZoomOut.Enabled := not b;
end;

end.

