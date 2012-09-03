unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, ComCtrls,
  TAChartUtils, TATransformations, TAGraph, TASources, TASeries,
  TATools, TADataTools;

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
    clrBackgroundColor: TColorButton;
    clrFontColor: TColorButton;
    clrPenColor: TColorButton;
    ctDist: TChartToolset;
    ctCrosshair: TDataPointCrosshairTool;
    ctDistance1: TDataPointDistanceTool;
    ctDistance2: TDataPointDistanceTool;
    edEndbarLength: TSpinEdit;
    lblEndBarLength: TLabel;
    mDistanceText: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    RandomChartSource1: TRandomChartSource;
    RandomChartSource2: TRandomChartSource;
    RandomChartSource3: TRandomChartSource;
    rgDataPointMode: TRadioGroup;
    rgDrawingMode: TRadioGroup;
    rgMeasureMode: TRadioGroup;
    rgSnapMode: TRadioGroup;
    StatusBar1: TStatusBar;
    procedure cbFlipLabelClick(Sender: TObject);
    procedure cbHideClick(Sender: TObject);
    procedure cbRotateLabelClick(Sender: TObject);
    procedure cbShowLabelClick(Sender: TObject);
    procedure clrBackgroundColorColorChanged(Sender: TObject);
    procedure clrFontColorColorChanged(Sender: TObject);
    procedure clrPenColorColorChanged(Sender: TObject);
    procedure ctCrosshairDraw(ASender: TDataPointCrosshairTool);
    procedure ctDistance1Measure(
      ASender: TDataPointDistanceTool);
    procedure edEndbarLengthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mDistanceTextChange(Sender: TObject);
    procedure rgDataPointModeClick(Sender: TObject);
    procedure rgDrawingModeClick(Sender: TObject);
    procedure rgMeasureModeClick(Sender: TObject);
    procedure rgSnapModeClick(Sender: TObject);
  private
    procedure SwitchOptions(AOptions: TDataPointDistanceTool.TOptions; AOn: Boolean);
    procedure UpdateButtons;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  TACustomSeries;

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
  ctDistance2.LinePen.Color := clrPenColor.ButtonColor;
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

