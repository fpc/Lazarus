unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, ComCtrls,
  TAChartUtils, TAFuncSeries, TATransformations, TAGraph, TASources, TASeries,
  TATools, TADataTools, types;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbFlipLabel: TCheckBox;
    cbHide: TCheckBox;
    cbRotateLabel: TCheckBox;
    cbShowLabel: TCheckBox;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Chart1LineSeries3: TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LogarithmAxisTransform1: TLogarithmAxisTransform;
    ChartAxisTransformations3: TChartAxisTransformations;
    ChartAxisTransformations3AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    chFit: TChart;
    chFitFitSeries1: TFitSeries;
    chFitLineSeries1: TLineSeries;
    clrBackgroundColor: TColorButton;
    clrFontColor: TColorButton;
    clrPenColor: TColorButton;
    ctDist: TChartToolset;
    ctCrosshair: TDataPointCrosshairTool;
    ctDistance1: TDataPointDistanceTool;
    ctDistance2: TDataPointDistanceTool;
    ctDistPanMouseWheelTool1: TPanMouseWheelTool;
    ctFit: TChartToolset;
    ctFitDataPointDistanceTool1: TDataPointDistanceTool;
    ctFitZoomDragTool1: TZoomDragTool;
    edEndbarLength: TSpinEdit;
    lblFit: TLabel;
    lblEndBarLength: TLabel;
    mDistanceText: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlFit: TPanel;
    rgFitParamCount: TRadioGroup;
    RandomChartSource1: TRandomChartSource;
    RandomChartSource2: TRandomChartSource;
    RandomChartSource3: TRandomChartSource;
    rgDataPointMode: TRadioGroup;
    rgDrawingMode: TRadioGroup;
    rgMeasureMode: TRadioGroup;
    rgSnapMode: TRadioGroup;
    StatusBar1: TStatusBar;
    tsMain: TTabSheet;
    tsFit: TTabSheet;
    procedure cbFlipLabelClick(Sender: TObject);
    procedure cbHideClick(Sender: TObject);
    procedure cbRotateLabelClick(Sender: TObject);
    procedure cbShowLabelClick(Sender: TObject);
    procedure clrBackgroundColorColorChanged(Sender: TObject);
    procedure clrFontColorColorChanged(Sender: TObject);
    procedure clrPenColorColorChanged(Sender: TObject);
    procedure ctCrosshairDraw(ASender: TDataPointCrosshairTool);
    procedure ctDistance1BeforeKeyDown(ATool: TChartTool; APoint: TPoint);
    procedure ctDistance1BeforeKeyUp(ATool: TChartTool; APoint: TPoint);
    procedure ctDistance1Measure(
      ASender: TDataPointDistanceTool);
    procedure ctFitDataPointDistanceTool1GetDistanceText(
      ASender: TDataPointDistanceTool; var AText: String);
    procedure ctFitDataPointDistanceTool1Measure(
      ASender: TDataPointDistanceTool);
    procedure edEndbarLengthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mDistanceTextChange(Sender: TObject);
    procedure rgFitParamCountClick(Sender: TObject);
    procedure rgDataPointModeClick(Sender: TObject);
    procedure rgDrawingModeClick(Sender: TObject);
    procedure rgMeasureModeClick(Sender: TObject);
    procedure rgSnapModeClick(Sender: TObject);
  private
    procedure PrepareFitData;
    procedure SwitchOptions(AOptions: TDataPointDistanceTool.TOptions; AOn: Boolean);
    procedure UpdateButtons;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  TACustomSeries, TAMath;

{ TForm1 }

procedure TForm1.cbFlipLabelClick(Sender: TObject);
begin
  SwitchOptions([dpdoFlipLabel], cbFlipLabel.Checked);
end;

procedure TForm1.cbHideClick(Sender: TObject);
begin
  SwitchOptions([dpdoPermanent], not cbHide.Checked);
end;

procedure TForm1.cbRotateLabelClick(Sender: TObject);
begin
  SwitchOptions([dpdoRotateLabel], cbRotateLabel.Checked);
end;

procedure TForm1.cbShowLabelClick(Sender: TObject);
begin
  ctDistance1.Marks.Visible := cbShowLabel.Checked;
  ctDistance2.Marks.Visible := cbShowLabel.Checked;
  UpdateButtons;
end;

procedure TForm1.clrBackgroundColorColorChanged(Sender: TObject);
begin
  Chart1.BackColor := clrBackgroundColor.ButtonColor;
end;

procedure TForm1.clrFontColorColorChanged(Sender: TObject);
begin
  ctDistance1.Marks.LabelFont.Color := clrFontColor.ButtonColor;
  ctDistance2.Marks.LabelFont.Color := clrFontColor.ButtonColor;
end;

procedure TForm1.clrPenColorColorChanged(Sender: TObject);
begin
  ctDistance1.LinePen.Color := clrPenColor.ButtonColor;
  ctDistance1.PointerStart.Pen.Color := clrPenColor.ButtonColor;
  ctDistance1.PointerEnd.Pen.Color := clrPenColor.ButtonColor;
  ctDistance2.LinePen.Color := clrPenColor.ButtonColor;
  ctDistance2.PointerStart.Pen.Color := clrPenColor.ButtonColor;
  ctDistance2.PointerEnd.Pen.Color := clrPenColor.ButtonColor;
  ctCrosshair.CrosshairPen.Color := clrPenColor.ButtonColor;
end;

procedure TForm1.ctCrosshairDraw(
  ASender: TDataPointCrosshairTool);
var
  ser: TChartSeries;
begin
  ser := TChartSeries(ASender.Series);
  if ser <> nil then begin
    with ser.Source.Item[ASender.PointIndex]^ do
      Statusbar1.SimpleText := Format('Cursor at (%f; %f)', [X, Y]);
  end else
    Statusbar1.SimpleText := '';
end;

procedure TForm1.ctDistance1BeforeKeyDown(ATool: TChartTool; APoint: TPoint);
const
  ZOOM_FACTOR = 2;
var
  ext: TDoubleRect;
  x, sz, ratio: Double;
begin
  if not (ssShift in ATool.Toolset.DispatchedShiftState) then exit;
  ext := Chart1.LogicalExtent;
  if ext.b.x - ext.a.x >= 10 then begin
    x := Chart1.XImageToGraph(APoint.X);
    sz := ext.b.x - ext.a.x;
    ratio := (x - ext.a.x) / sz;
    ext.a.x := x - sz * ratio / ZOOM_FACTOR;
    ext.b.x := x + sz * (1 - ratio) / ZOOM_FACTOR;
    Chart1.LogicalExtent := ext;
  end;
  ATool.Handled;
end;

procedure TForm1.ctDistance1BeforeKeyUp(ATool: TChartTool; APoint: TPoint);
begin
  Unused(APoint);
  Chart1.ZoomFull;
  ATool.Handled;
end;

procedure TForm1.ctDistance1Measure(
  ASender: TDataPointDistanceTool);
const
  DIST_TEXT: array [TChartDistanceMode] of String = ('', 'x ', 'y ');
begin
  with ASender do
    Statusbar1.SimpleText := Format(
      'Measured %sdistance between (%f; %f) and (%f; %f): %f', [
      DIST_TEXT[MeasureMode],
      PointStart.GraphPos.X, PointStart.GraphPos.Y,
      PointEnd.GraphPos.X, PointEnd.GraphPos.Y,
      Distance(cuPixel)
    ]);
end;

procedure TForm1.ctFitDataPointDistanceTool1GetDistanceText(
  ASender: TDataPointDistanceTool; var AText: String);
var
  xmin, xmax: Double;
begin
  xmin := ASender.PointStart.AxisPos.X;
  xmax := ASender.PointEnd.AxisPos.X;
  EnsureOrder(xmin, xmax);
  with chFitFitSeries1.FitRange do begin
    Min := xmin;
    Max := xmax;
    if xmin < xmax then begin
      UseMax := true;
      UseMin := true;
    end else begin
      UseMin := true;
      UseMax := true;
    end;
  end;
  case rgFitParamCount.ItemIndex of
    0: AText := Format('Mean value: %f', [chFitFitSeries1.Param[0]]);
    1: AText := Format('Slope: %f', [chFitFitSeries1.Param[1]]);
    2:
      with chFitFitSeries1 do
        if Param[2] = 0 then
          AText := ''
        else
          AText := Format('Peak at x=%f y=%f', [
            -Param[1] / (2 * Param[2]),
            Param[0] - Sqr(Param[1])/(4 * Param[2])
        ]);
  end;
  chFitFitSeries1.Active := true;

  lblFit.Visible := true;
  lblFit.Caption := AText;
end;

procedure TForm1.ctFitDataPointDistanceTool1Measure(
  ASender: TDataPointDistanceTool);
begin
  chFitFitSeries1.Active := false;
end;

procedure TForm1.edEndbarLengthChange(Sender: TObject);
begin
  ctDistance1.PointerStart.VertSize := edEndbarLength.Value;
  ctDistance1.PointerEnd.VertSize := edEndbarLength.Value;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cbHideClick(nil);
  cbRotateLabelClick(nil);
  mDistanceTextChange(nil);
  rgDataPointModeClick(nil);
  rgDrawingModeClick(nil);

  PrepareFitData;
end;

procedure TForm1.mDistanceTextChange(Sender: TObject);
var
  s: String;
begin
  s := mDistanceText.Lines.Text;
  try
    Format(s, [1.0, 1.0]);
    ctDistance1.Marks.Format := s;
    ctDistance2.Marks.Format := s;
  except
  end;
end;

procedure TForm1.PrepareFitData;
const
  N = 50;
  NOISE = 0.5;
var
  i: Integer;
  x, y: Double;
begin
  for i := 0 to N - 1 do begin
    x := -10 + 10 * i / (N - 1);
    y := Sqr(x) * 0.1 + 1;
    chFitLineSeries1.AddXY(x, y + (Random - 1) * NOISE);
  end;
  for i := 0 to N - 1 do begin
    x := 0 + 10 * i / (N - 1);
    y := Cos(x) + x;
    chFitLineSeries1.AddXY(x, y + (Random - 1) * NOISE);
  end;
  chFitFitSeries1.Source := chFitLineSeries1.Source;
end;

procedure TForm1.rgDataPointModeClick(Sender: TObject);
begin
  with ctDistance1 do begin
    DataPointModeStart := TDataPointDistanceTool.TDataPointMode(rgDataPointMode.ItemIndex);
    DataPointModeEnd := DataPointModeStart;
    ctDistance2.DataPointModeStart := DataPointModeStart;
    ctDistance2.DataPointModeEnd := DataPointModeStart;
  end;
  UpdateButtons;
end;

procedure TForm1.rgDrawingModeClick(Sender: TObject);
begin
  ctDistance1.DrawingMode := TChartToolDrawingMode(rgDrawingMode.ItemIndex);
  ctDistance2.DrawingMode := TChartToolDrawingMode(rgDrawingMode.ItemIndex);
  ctCrosshair.DrawingMode := TChartToolDrawingMode(rgDrawingMode.ItemIndex);
  UpdateButtons;
end;

procedure TForm1.rgFitParamCountClick(Sender: TObject);
begin
  chFitFitSeries1.ParamCount := rgFitParamCount.ItemIndex + 1;
end;

procedure TForm1.rgMeasureModeClick(Sender: TObject);
begin
  ctDistance1.MeasureMode := TChartDistanceMode(rgMeasureMode.ItemIndex);
  ctDistance2.MeasureMode := TChartDistanceMode(rgMeasureMode.ItemIndex);
end;

procedure TForm1.rgSnapModeClick(Sender: TObject);
begin
  ctDistance1.DistanceMode := TChartDistanceMode(rgSnapMode.ItemIndex);
  ctDistance2.DistanceMode := TChartDistanceMode(rgSnapMode.ItemIndex);
  ctCrosshair.DistanceMode := TChartDistanceMode(rgSnapMode.ItemIndex);
end;

procedure TForm1.SwitchOptions(
  AOptions: TDataPointDistanceTool.TOptions; AOn: Boolean);
begin
  with ctDistance1 do begin
    if AOn then
      Options := Options + AOptions
    else
      Options := Options - AOptions;
    ctDistance2.Options := Options;
  end;
end;

procedure TForm1.UpdateButtons;
begin
  clrPenColor.Enabled := ctDistance1.DrawingMode=tdmNormal;
  clrFontColor.Enabled := (ctDistance1.DrawingMode=tdmNormal)
    and ctDistance1.Marks.Visible;
  rgSnapMode.Enabled := ctDistance1.DataPointModeStart <> dpmFree;
end;

end.

