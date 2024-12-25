unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, Forms, Controls, Graphics, Dialogs, Math,
  TAGraph, TATools, TASeries, TATransformations, TAChartUtils, TACustomSeries;

type
  { TMainForm }

  TMainForm = class(TForm)
    cbStretched: TCheckBox;
    Chart: TChart;
    BlueSeries: TLineSeries;
    cbAnimated: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    rbStretchedToBlue: TRadioButton;
    rbStretchedToRed: TRadioButton;
    rbStretchedToAll: TRadioButton;
    RedSeries: TLineSeries;
    ChartToolset: TChartToolset;
    PanClickTool: TPanClickTool;
    PanDragTool: TPanDragTool;
    PanMouseWheelTool: TPanMouseWheelTool;
    ZoomClickTool: TZoomClickTool;
    ZoomDragTool: TZoomDragTool;
    ZoomMouseWheelTool: TZoomMouseWheelTool;
    cbRotateAxes: TCheckBox;
    BottomPanel: TPanel;
    procedure cbAnimatedChange(Sender: TObject);
    procedure ZoomDragToolCalculateNewExtent(ATool: TChartTool;
      var ANewExtent: TDoubleRect);
    procedure cbRotateAxesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure RotateAxes;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  RedSeries.Clear;
  BlueSeries.Clear;
  RedSeries.AddXY(0, 50);
  BlueSeries.AddXY(0, 40);
  for i := 1 to 100 do
  begin
    RedSeries.AddXY(i, RedSeries.GetYValue(i - 1) + Random(9) - 4);
    BlueSeries.AddXY(i, BlueSeries.GetYValue(i - 1) + Random(9) - 5);
  end;

  RotateAxes;
end;

procedure TMainForm.ZoomDragToolCalculateNewExtent(ATool: TChartTool;
  var ANewExtent: TDoubleRect);

  procedure ResetRange(out min, max: Double);
  begin
    min := Infinity;
    max := -Infinity;
  end;

  function StretchToSeries(ASeries: TBasicChartSeries): Boolean;
  begin
    Result := rbStretchedToAll.Checked or
      (rbStretchedToRed.Checked and (ASeries = RedSeries)) or
      (rbStretchedToBlue.Checked and (ASeries = BlueSeries));
  end;

var
  newExt: TDoubleRect;
  ymin, ymax: Double;
  ser: TBasicPointSeries;
  i: Integer;
begin
  if not cbStretched.Checked then
    exit;

  ResetRange(newExt.a.X, newExt.b.X);
  ResetRange(newExt.a.Y, newExt.b.Y);
  for i := 0 to ATool.Chart.SeriesCount-1 do
    if StretchToSeries(ATool.Chart.Series[i]) and (ATool.Chart.Series[i] is TBasicPointSeries) then
    begin
      ser := TBasicPointSeries(ATool.Chart.Series[i]);
      ResetRange(ymin, ymax);
      if ser.IsRotated then
      begin
        ser.FindYRange(ANewExtent.a.Y, ANewExtent.b.Y, ymin, ymax);
        UpdateMinMax(ymin, newExt.a.X, newExt.b.X);
        UpdateMinmax(ymax, newExt.a.X, newExt.b.X);
      end else
      begin
        ser.FindYRange(ANewExtent.a.X, ANewExtent.b.X, ymin, ymax);
        UpdateMinMax(ymin, newExt.a.Y, newExt.b.Y);
        UpdateMinMax(ymax, newExt.a.Y, newExt.b.Y);
      end;
    end;

  if not IsInfinite(newExt.a.X) then ANewExtent.a.X := newExt.a.X;
  if not IsInfinite(newExt.a.Y) then ANewExtent.a.Y := newExt.a.Y;
  if not IsInfinite(newExt.b.X) then ANewExtent.b.X := newExt.b.X;
  if not IsInfinite(newExt.b.Y) then ANewExtent.b.Y := newExt.b.Y;
end;

procedure TMainForm.cbAnimatedChange(Sender: TObject);
var
  intvl: Integer;
begin
  if cbAnimated.Checked then intvl := 10 else intvl := 0;
  ZoomDragTool.AnimationInterval := intvl;
  ZoomClickTool.AnimationInterval := intvl;
  ZoomMouseWheelTool.AnimationInterval := intvl;
end;

procedure TMainForm.cbRotateAxesChange(Sender: TObject);
begin
  RotateAxes;
end;

procedure TMainForm.RotateAxes;
var
  i: Integer;
  ser: TBasicPointSeries;
begin
  Chart.ZoomFull;
  for i := 0 to Chart.SeriesCount-1 do
  begin
    if (Chart.Series[i] is TBasicPointSeries) then
    begin
      ser := TBasicPointSeries(Chart.Series[i]);
      if cbRotateAxes.Checked then
      begin
        ser.AxisIndexX := 0;
        ser.AxisIndexY := 1;
        ZoomDragTool.RatioLimit := zrlFixedX;
      end else
      begin
        ser.AxisIndexX := 1;
        ser.AxisIndexY := 0;
        ZoomDragTool.RatioLimit := zrlFixedY;
      end;
    end;
  end;
  Chart.Invalidate;
end;

end.

