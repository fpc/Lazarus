unit frmMainDemo;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Spin, Dialogs,
  TAGraph, TAChartUtils, TACustomSeries, TASeries, TASources,
  TATools, TADataTools, TATransformations;

type
  TMainDemoFrame = class(TFrame)
    cbClipping: TCheckBox;
    cbFlipLabel: TCheckBox;
    cbHide: TCheckBox;
    cbRotateLabel: TCheckBox;
    cbShowLabel: TCheckBox;
    cbTransparency: TCheckBox;
    Chart: TChart;
    ChartLineSeries1: TLineSeries;
    ChartLineSeries2: TLineSeries;
    ChartLineSeries3: TLineSeries;
    AxisTransformationsLeft: TChartAxisTransformations;
    AxisTransformationsLeftLogarithmAxisTransform1: TLogarithmAxisTransform;
    AxisTransformationsRight: TChartAxisTransformations;
    AxisTransformationsRightAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    clrBackgroundColor: TColorButton;
    clrFontColor: TColorButton;
    clrPenColor: TColorButton;
    CrosshairTool: TDataPointCrosshairTool;
    Toolset: TChartToolset;
    DistanceTool1: TDataPointDistanceTool;
    DistanceTool2: TDataPointDistanceTool;
    PanMouseWheelTool: TPanMouseWheelTool;
    edEndbarLength: TSpinEdit;
    lblEndBarLength: TLabel;
    mDistanceText: TMemo;
    ParamsPanel: TPanel;
    InfoPanel: TPanel;
    RandomChartSource1: TRandomChartSource;
    RandomChartSource2: TRandomChartSource;
    RandomChartSource3: TRandomChartSource;
    rgDataPointMode: TRadioGroup;
    rgDrawingMode: TRadioGroup;
    rgMeasureMode: TRadioGroup;
    rgSnapMode: TRadioGroup;
    StatusBar: TStatusBar;
    procedure cbClippingChange(Sender: TObject);
    procedure cbFlipLabelClick(Sender: TObject);
    procedure cbHideClick(Sender: TObject);
    procedure cbRotateLabelClick(Sender: TObject);
    procedure cbShowLabelClick(Sender: TObject);
    procedure cbTransparencyChange(Sender: TObject);
    procedure clrBackgroundColorColorChanged(Sender: TObject);
    procedure CrosshairToolDraw(ASender: TDataPointDrawTool);
    procedure DistanceTool1BeforeKeyDown(ATool: TChartTool; APoint: TPoint);
    procedure DistanceTool1BeforeKeyUp(ATool: TChartTool; APoint: TPoint);
    procedure DistanceTool1Measure(ASender: TDataPointDistanceTool);
    procedure edEndbarLengthChange(Sender: TObject);
    procedure mDistanceTextChange(Sender: TObject);
    procedure rgDataPointModeClick(Sender: TObject);
    procedure rgDrawingModeClick(Sender: TObject);
    procedure rgMeasureModeClick(Sender: TObject);
    procedure rgSnapModeClick(Sender: TObject);
  private
    procedure SwitchOptions(AOptions: TDataPointDistanceTool.TOptions; AOn: Boolean);
    procedure UpdateButtons;

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TMainDemoFrame.Create(AOwner: TComponent);
begin
  inherited;
  clrBackgroundColor.ButtonColor := Chart.BackColor;
  cbHideClick(nil);
  cbRotateLabelClick(nil);
  mDistanceTextChange(nil);
  rgDataPointModeClick(nil);
  rgDrawingModeClick(nil);
end;

procedure TMainDemoFrame.cbFlipLabelClick(Sender: TObject);
begin
  SwitchOptions([dpdoFlipLabel], cbFlipLabel.Checked);
end;

procedure TMainDemoFrame.cbClippingChange(Sender: TObject);
begin
  SwitchOptions([dpdoClipping], cbClipping.Checked);
end;

procedure TMainDemoFrame.cbHideClick(Sender: TObject);
begin
  SwitchOptions([dpdoPermanent], not cbHide.Checked);
end;

procedure TMainDemoFrame.cbRotateLabelClick(Sender: TObject);
begin
  SwitchOptions([dpdoRotateLabel], cbRotateLabel.Checked);
end;

procedure TMainDemoFrame.cbShowLabelClick(Sender: TObject);
begin
  DistanceTool1.Marks.Visible := cbShowLabel.Checked;
  DistanceTool2.Marks.Visible := cbShowLabel.Checked;
  UpdateButtons;
end;

procedure TMainDemoFrame.cbTransparencyChange(Sender: TObject);
begin
  if cbTransparency.Checked then
    DistanceTool1.Transparency := 128
  else
    DistanceTool1.Transparency := 0;
end;

procedure TMainDemoFrame.clrBackgroundColorColorChanged(Sender: TObject);
begin
  Chart.BackColor := clrBackgroundColor.ButtonColor;
end;

procedure TMainDemoFrame.CrosshairToolDraw(ASender: TDataPointDrawTool);
var
  ser: TChartSeries;
begin
  ser := TChartSeries(ASender.Series);
  if ser <> nil then begin
    with ser.Source.Item[ASender.PointIndex]^ do
      StatusBar.SimpleText := Format('Cursor at (%f; %f)', [X, Y]);
  end else
    StatusBar.SimpleText := '';
end;

procedure TMainDemoFrame.DistanceTool1BeforeKeyDown(ATool: TChartTool; APoint:
  TPoint);
const
  ZOOM_FACTOR = 2;
var
  ext: TDoubleRect;
  x, sz, ratio: Double;
begin
  if not (ssShift in ATool.Toolset.DispatchedShiftState) then exit;
  ext := Chart.LogicalExtent;
  if ext.b.x - ext.a.x >= 10 then begin
    x := Chart.XImageToGraph(APoint.X);
    sz := ext.b.x - ext.a.x;
    ratio := (x - ext.a.x) / sz;
    ext.a.x := x - sz * ratio / ZOOM_FACTOR;
    ext.b.x := x + sz * (1 - ratio) / ZOOM_FACTOR;
    Chart.LogicalExtent := ext;
  end;
  ATool.Handled;
end;

procedure TMainDemoFrame.DistanceTool1BeforeKeyUp(ATool: TChartTool; APoint:
  TPoint);
begin
  Unused(APoint);
  Chart.ZoomFull;
  ATool.Handled;
end;

procedure TMainDemoFrame.DistanceTool1Measure(ASender: TDataPointDistanceTool);
const
  DIST_TEXT: array [TChartDistanceMode] of String = ('', 'x ', 'y ');
begin
  with ASender do
    StatusBar.SimpleText := Format(
      'Measured %sdistance between (%f; %f) and (%f; %f): %f', [
      DIST_TEXT[MeasureMode],
      PointStart.GraphPos.X, PointStart.GraphPos.Y,
      PointEnd.GraphPos.X, PointEnd.GraphPos.Y,
      Distance(cuPixel)
    ]);
end;

procedure TMainDemoFrame.edEndbarLengthChange(Sender: TObject);
begin
  DistanceTool1.PointerStart.VertSize := edEndbarLength.Value;
  DistanceTool1.PointerEnd.VertSize := edEndbarLength.Value;
end;

procedure TMainDemoFrame.mDistanceTextChange(Sender: TObject);
var
  s: String;
begin
  s := mDistanceText.Lines.Text;
  try
    Format(s, [1.0, 1.0]);
    DistanceTool1.Marks.Format := s;
    DistanceTool2.Marks.Format := s;
  except
  end;
end;

procedure TMainDemoFrame.rgDataPointModeClick(Sender: TObject);
begin
  with DistanceTool1 do begin
    DataPointModeStart := TDataPointDistanceTool.TDataPointMode(rgDataPointMode.ItemIndex);
    DataPointModeEnd := DataPointModeStart;
    DistanceTool2.DataPointModeStart := DataPointModeStart;
    DistanceTool2.DataPointModeEnd := DataPointModeStart;
  end;
  UpdateButtons;
end;

procedure TMainDemoFrame.rgDrawingModeClick(Sender: TObject);
begin
  DistanceTool1.DrawingMode := TChartToolDrawingMode(rgDrawingMode.ItemIndex);
  DistanceTool2.DrawingMode := TChartToolDrawingMode(rgDrawingMode.ItemIndex);
  CrosshairTool.DrawingMode := TChartToolDrawingMode(rgDrawingMode.ItemIndex);
  UpdateButtons;
end;

procedure TMainDemoFrame.rgMeasureModeClick(Sender: TObject);
begin
  DistanceTool1.MeasureMode := TChartDistanceMode(rgMeasureMode.ItemIndex);
  DistanceTool2.MeasureMode := TChartDistanceMode(rgMeasureMode.ItemIndex);
end;

procedure TMainDemoFrame.rgSnapModeClick(Sender: TObject);
begin
  DistanceTool1.DistanceMode := TChartDistanceMode(rgSnapMode.ItemIndex);
  DistanceTool2.DistanceMode := TChartDistanceMode(rgSnapMode.ItemIndex);
  CrosshairTool.DistanceMode := TChartDistanceMode(rgSnapMode.ItemIndex);
end;

procedure TMainDemoFrame.SwitchOptions(
  AOptions: TDataPointDistanceTool.TOptions; AOn: Boolean);
begin
  with DistanceTool1 do begin
    if AOn then
      Options := Options + AOptions
    else
      Options := Options - AOptions;
    DistanceTool2.Options := Options;
  end;
end;

procedure TMainDemoFrame.UpdateButtons;
begin
  clrPenColor.Enabled := DistanceTool1.DrawingMode=tdmNormal;
  clrFontColor.Enabled := (DistanceTool1.DrawingMode=tdmNormal)
    and DistanceTool1.Marks.Visible;
  rgSnapMode.Enabled := DistanceTool1.DataPointModeStart <> dpmFree;
end;


end.

