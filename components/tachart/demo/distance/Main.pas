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
    ctDistDataPointCrosshairTool1: TDataPointCrosshairTool;
    ctDistDataPointDistanceTool1: TDataPointDistanceTool;
    ctDistDataPointDistanceTool2: TDataPointDistanceTool;
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
    procedure ctDistDataPointCrosshairTool1Draw(ASender: TDataPointCrosshairTool);
    procedure ctDistDataPointDistanceTool1Measure(
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
  ctDistDataPointDistanceTool1.Marks.Visible := cbShowLabel.Checked;
  ctDistDataPointDistanceTool2.Marks.Visible := cbShowLabel.Checked;
  UpdateButtons;
end;

procedure TForm1.clrBackgroundColorColorChanged(Sender: TObject);
begin
  Chart1.BackColor := clrBackgroundColor.ButtonColor;
end;

procedure TForm1.clrFontColorColorChanged(Sender: TObject);
begin
  ctDistDataPointDistanceTool1.Marks.LabelFont.Color := clrFontColor.ButtonColor;
  ctDistDataPointDistanceTool2.Marks.LabelFont.Color := clrFontColor.ButtonColor;
end;

procedure TForm1.clrPenColorColorChanged(Sender: TObject);
begin
  ctDistDataPointDistanceTool1.LinePen.Color := clrPenColor.ButtonColor;
  ctDistDataPointDistanceTool2.LinePen.Color := clrPenColor.ButtonColor;
  ctDistDataPointCrosshairTool1.CrosshairPen.Color := clrPenColor.ButtonColor;
end;

procedure TForm1.ctDistDataPointCrosshairTool1Draw(
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

procedure TForm1.ctDistDataPointDistanceTool1Measure(
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
  ctDistDataPointDistanceTool1.PointerStart.VertSize := edEndbarLength.Value;
  ctDistDataPointDistanceTool1.PointerEnd.VertSize := edEndbarLength.Value;
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
    ctDistDataPointDistanceTool1.Marks.Format := s;
    ctDistDataPointDistanceTool2.Marks.Format := s;
  except
  end;
end;

procedure TForm1.rgDataPointModeClick(Sender: TObject);
begin
  with ctDistDataPointDistanceTool1 do begin
    DataPointMode := TDataPointDistanceTool.TDataPointMode(rgDataPointMode.ItemIndex);
    ctDistDataPointDistanceTool2.DataPointMode := DataPointMode;
  end;
  UpdateButtons;
end;

procedure TForm1.rgDrawingModeClick(Sender: TObject);
begin
  ctDistDataPointDistanceTool1.DrawingMode := TChartToolDrawingMode(rgDrawingMode.ItemIndex);
  ctDistDataPointDistanceTool2.DrawingMode := TChartToolDrawingMode(rgDrawingMode.ItemIndex);
  ctDistDataPointCrosshairTool1.DrawingMode := TChartToolDrawingMode(rgDrawingMode.ItemIndex);
  UpdateButtons;
end;

procedure TForm1.rgMeasureModeClick(Sender: TObject);
begin
  ctDistDataPointDistanceTool1.MeasureMode := TChartDistanceMode(rgMeasureMode.ItemIndex);
  ctDistDataPointDistanceTool2.MeasureMode := TChartDistanceMode(rgMeasureMode.ItemIndex);
end;

procedure TForm1.rgSnapModeClick(Sender: TObject);
begin
  ctDistDataPointDistanceTool1.DistanceMode := TChartDistanceMode(rgSnapMode.ItemIndex);
  ctDistDataPointDistanceTool2.DistanceMode := TChartDistanceMode(rgSnapMode.ItemIndex);
  ctDistDataPointCrosshairTool1.DistanceMode := TChartDistanceMode(rgSnapMode.ItemIndex);
end;

procedure TForm1.SwitchOptions(
  AOptions: TDataPointDistanceTool.TOptions; AOn: Boolean);
begin
  with ctDistDataPointDistanceTool1 do begin
    if AOn then
      Options := Options + AOptions
    else
      Options := Options - AOptions;
    ctDistDataPointDistanceTool2.Options := Options;
  end;
end;

procedure TForm1.UpdateButtons;
begin
  clrPenColor.Enabled := ctDistDataPointDistanceTool1.DrawingMode=tdmNormal;
  clrFontColor.Enabled := (ctDistDataPointDistanceTool1.DrawingMode=tdmNormal)
    and ctDistDataPointDistanceTool1.Marks.Visible;
  rgSnapMode.Enabled := ctDistDataPointDistanceTool1.DataPointMode <> dpmFree;
end;

end.

