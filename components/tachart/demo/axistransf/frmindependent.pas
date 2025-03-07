unit frmIndependent;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls, Spin,
  TAGraph, TAChartAxis, TAChartAxisUtils, TACustomSource, TAAxisSource,
  TASeries, TATransformations;

type
  TIndependentScaleFrame = class(TFrame)
    catIndependent1: TChartAxisTransformations;
    catIndependent1Zoom: TLinearAxisTransform;
    catIndependent2: TChartAxisTransformations;
    catIndependent2Zoom: TLinearAxisTransform;
    Chart: TChart;
    ChartLineSeries1: TLineSeries;
    ChartLineSeries2: TLineSeries;
    fseScale1: TFloatSpinEdit;
    fseScale2: TFloatSpinEdit;
    lblScale1: TLabel;
    lblScale2: TLabel;
    pnlIndependentControls: TPanel;
    rgSyncAxisMarks: TRadioGroup;
    procedure fseScale1Change(Sender: TObject);
    procedure fseScale2Change(Sender: TObject);
    procedure rgSyncAxisMarksClick(Sender: TObject);
  private
    FAxisSource: TCustomAxisChartSource;
    procedure FillIndependentSource;

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TIndependentScaleFrame.Create(AOwner: TComponent);
begin
  inherited;
  FillIndependentSource;
  FAxisSource := TCustomAxisChartSource.Create(Self);
end;

procedure TIndependentScaleFrame.FillIndependentSource;
var
  i: Integer;
  v1, v2: Double;
begin
  RandSeed := 923875;
  v1 := 0;
  v2 := 0;
  for i := 1 to 100 do begin
    v1 += Random - 0.48;
    v2 += Random - 0.52;
    ChartLineSeries1.AddXY(i, v1);
    ChartLineSeries2.AddXY(i, v2);
  end;
end;

procedure TIndependentScaleFrame.fseScale1Change(Sender: TObject);
begin
  catIndependent1Zoom.Scale := fseScale1.Value;
end;

procedure TIndependentScaleFrame.fseScale2Change(Sender: TObject);
begin
  catIndependent2Zoom.Scale := fseScale2.Value;
end;

procedure TIndependentScaleFrame.rgSyncAxisMarksClick(Sender: TObject);
var
  la, ra: TChartAxis;
begin
  la := Chart.LeftAxis;
  ra := Chart.AxisList.GetAxisByAlign(calRight);
  la.Marks.Source := nil;
  ra.Marks.Source := nil;
  case rgSyncAxisMarks.ItemIndex of
    0: begin
      FAxisSource.AxisFrom := ra;
      FAxisSource.AxisTo := la;
      la.Marks.Source := FAxisSource;
    end;
    2: begin
      FAxisSource.AxisFrom := la;
      FAxisSource.AxisTo := ra;
      ra.Marks.Source := FAxisSource;
    end;
  end;
  la.Grid.Visible := rgSyncAxisMarks.ItemIndex <> 1;
end;

end.

