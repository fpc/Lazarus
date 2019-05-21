unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, Types,
  TAFuncSeries, TAGraph, TASeries, TASources, TATools, TADrawUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    ChartLine2: TConstantLine;
    ChartLine1: TConstantLine;
    Chart1FuncSeries1: TFuncSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointCrosshairTool1: TDataPointCrosshairTool;
    ChartToolset1DataPointDragTool1: TDataPointDragTool;
    ChartToolset1PanAny: TPanDragTool;
    ChartToolset1PanClickTool1: TPanClickTool;
    ChartToolset1PanHor: TPanDragTool;
    ChartToolset1PanMouseWheelTool1: TPanMouseWheelTool;
    ChartToolset1PanVert: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    ChartToolset1ZoomOut: TZoomClickTool;
    ChartToolset1ZoomIn: TZoomClickTool;
    cbFixedPoint: TCheckBox;
    cbAnimate: TCheckBox;
    Panel1: TPanel;
    Panel2: TPanel;
    rgZoom: TRadioGroup;
    RandomChartSource1: TRandomChartSource;
    rgPan: TRadioGroup;
    procedure cbAnimateClick(Sender: TObject);
    procedure cbFixedPointChange(Sender: TObject);
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure ChartToolset1DataPointCrosshairTool1AfterKeyUp(ATool: TChartTool;
      APoint: TPoint);
    procedure ChartToolset1DataPointCrosshairTool1CustomDraw(
      ASender: TDataPointCrosshairTool; ADrawer: IChartDrawer);
    procedure ChartToolset1DataPointDragTool1BeforeMouseMove(ATool: TChartTool;
      APoint: TPoint);
    procedure rgPanClick(Sender: TObject);
    procedure rgZoomClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses
  Math, TAChartUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.cbAnimateClick(Sender: TObject);
var
  t: TChartTool;
begin
  for t in ChartToolset1.Tools do
    if t is TBasicZoomTool then
      TBasicZoomTool(t).AnimationSteps := IfThen(cbAnimate.Checked, 3, 0);
end;

procedure TForm1.cbFixedPointChange(Sender: TObject);
var
  t: TChartTool;
begin
  for t in ChartToolset1.Tools do
    if t is TBasicZoomStepTool then
      TBasicZoomTool(t).FixedPoint := cbFixedPoint.Checked;
end;

procedure TForm1.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := Sin(AX * 10) + 0.7 * Cos(AX * 30) + 0.3 * Sin(AX * 80);
end;

procedure TForm1.ChartToolset1DataPointCrosshairTool1AfterKeyUp(
  ATool: TChartTool; APoint: TPoint);
begin
  Unused(ATool, APoint);
  ChartToolset1DataPointCrosshairTool1.Hide;
end;

procedure TForm1.ChartToolset1DataPointCrosshairTool1CustomDraw(
  ASender: TDataPointCrosshairTool; ADrawer: IChartDrawer);
const
  R = 20;
begin
  with Chart1.GraphToImage(ASender.Position) do
    Chart1.Drawer.Ellipse(X - R, Y - R, X + R, Y + R);
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
  i: Integer;
begin
  i := rgPan.ItemIndex;
  ChartToolset1PanAny.Enabled := i = 0;
  ChartToolset1PanHor.Enabled := i = 1;
  ChartToolset1PanVert.Enabled := i = 1;
  ChartToolset1PanClickTool1.Enabled := i in [2, 3];
  ChartToolset1PanClickTool1.Interval := IfThen(i = 2, 0, 200);
  with ChartToolset1PanMouseWheelTool1 do begin
    Enabled := i in [4, 5];
    if i = 4 then
      WheelUpDirection := pdUp
    else
      WheelUpDirection := pdRight;
  end;
end;

procedure TForm1.rgZoomClick(Sender: TObject);
var
  t: TChartTool;
begin
  for t in ChartToolset1.Tools do
    if Pos('Zoom', t.Name) > 0 then
      t.Enabled := false;
  case rgZoom.ItemIndex of
    0: begin
      ChartToolset1ZoomDragTool1.Enabled := true;
      ChartToolset1ZoomDragTool1.RatioLimit := zrlNone;
    end;
    1: begin
      ChartToolset1ZoomDragTool1.Enabled := true;
      ChartToolset1ZoomDragTool1.RatioLimit := zrlProportional
    end;
    2: begin
      ChartToolset1ZoomIn.Enabled := true;
      ChartToolset1ZoomOut.Enabled := true;
    end;
    3: begin
      ChartToolset1ZoomMouseWheelTool1.Enabled := true;
    end;
  end;
end;

end.

