unit frmFitDemo;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls,
  TAGraph, TASeries, TAFuncSeries, TAMath, TATools, TADataTools, Types;

type
  TFitDemoFrame = class(TFrame)
    Chart: TChart;
    FitSeries: TFitSeries;
    LineSeries: TLineSeries;
    Toolset: TChartToolset;
    DataPointDistanceTool: TDataPointDistanceTool;
    ZoomDragTool: TZoomDragTool;
    lblFit: TLabel;
    ParamsPanel: TPanel;
    rgFitParamCount: TRadioGroup;
    procedure DataPointDistanceToolBeforeMouseDown(ATool: TChartTool; APoint: TPoint);
    procedure DataPointDistanceToolGetDistanceText(ASender: TDataPointDistanceTool; var AText: String);
    procedure DataPointDistanceToolMeasure(ASender: TDataPointDistanceTool);
    procedure rgFitParamCountClick(Sender: TObject);
  private
    procedure PrepareFitData;

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TFitDemoFrame.Create(AOwner: TComponent);
begin
  inherited;
  PrepareFitData;
end;

procedure TFitDemoFrame.DataPointDistanceToolGetDistanceText(ASender:
  TDataPointDistanceTool; var AText: String);
var
  xmin, xmax: Double;
begin
  xmin := ASender.PointStart.AxisPos.X;
  xmax := ASender.PointEnd.AxisPos.X;
  EnsureOrder(xmin, xmax);
  with FitSeries.FitRange do begin
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

  FitSeries.Active := true;
  FitSeries.ExecFit;
  if FitSeries.ErrorMsg <> '' then
    AText := FitSeries.ErrorMsg
  else
    case rgFitParamCount.ItemIndex of
      0: AText := Format('Mean value: %f', [FitSeries.Param[0]]);
      1: AText := Format('Slope: %f', [FitSeries.Param[1]]);
      2:
        with FitSeries do
          if Param[2] = 0 then
            AText := ''
          else
            AText := Format('Min/max at x=%f y=%f', [
              -Param[1] / (2 * Param[2]),
              Param[0] - Sqr(Param[1])/(4 * Param[2])
          ]);
    end;

  lblFit.Visible := true;
  lblFit.Caption := AText;
end;

procedure TFitDemoFrame.DataPointDistanceToolBeforeMouseDown(ATool:
  TChartTool; APoint: TPoint);
begin
  lblFit.Caption := 'Measuring...';
end;

procedure TFitDemoFrame.DataPointDistanceToolMeasure(ASender:
  TDataPointDistanceTool);
begin
  FitSeries.Active := false;
end;

procedure TFitDemoFrame.PrepareFitData;
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
    LineSeries.AddXY(x, y + (Random - 1) * NOISE);
  end;
  for i := 0 to N - 1 do begin
    x := 0 + 10 * i / (N - 1);
    y := Cos(x) + x;
    LineSeries.AddXY(x, y + (Random - 1) * NOISE);
  end;
  FitSeries.Source := LineSeries.Source;
end;

procedure TFitDemoFrame.rgFitParamCountClick(Sender: TObject);
begin
  FitSeries.ParamCount := rgFitParamCount.ItemIndex + 1;
end;


end.

