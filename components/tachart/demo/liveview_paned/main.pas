{ This project demonstrates application of a TChartLiveView to a multiple-axis
  chart ("paned chart"). }

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, TAGraph, TATransformations, TASeries, TAChartLiveView;

type

  { TForm1 }

  TForm1 = class(TForm)
    AccelerationAutoScaleTransform: TAutoScaleAxisTransform;
    Bevel1: TBevel;
    btnReset: TButton;
    btnStopResume: TButton;
    Chart1: TChart;
    ChartLiveView1: TChartLiveView;
    cbLiveView: TCheckBox;
    cbFrozenAxes: TCheckBox;
    Label1: TLabel;
    PositionSeries: TLineSeries;
    seViewportSize: TFloatSpinEdit;
    Timer: TTimer;
    VelocitySeries: TLineSeries;
    AccelerationSeries: TLineSeries;
    PositionTransformations: TChartAxisTransformations;
    PositionAutoScaleTransform: TAutoScaleAxisTransform;
    VelocityTransformations: TChartAxisTransformations;
    AccelerationTransformations: TChartAxisTransformations;
    Panel1: TPanel;
    VelocityScaleTransform: TAutoScaleAxisTransform;
    procedure btnResetClick(Sender: TObject);
    procedure btnStopResumeClick(Sender: TObject);
    procedure cbFrozenAxesChange(Sender: TObject);
    procedure cbLiveViewChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure seViewportSizeChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FStartTime: TDateTime;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  //DateUtils,
  Math;

const
  A0 = 10.0;   // (initial) oscillation amplitude
  t0 =  5.0;   // oscillation period
  td = 20.0;   // damping time constant
  TWO_PI = 2.0*pi;

{ TForm1 }

{ Simulates position, velocity and acceleration of an oscillating body. The
  oscillation is damped, i.e. the excursions are decreasing with time. Parameters
  describing this damped oscillation are A0, t0 and td given above. }
procedure TForm1.TimerTimer(Sender: TObject);
var
  t: Double;
  x, v, a: Double;
  exp_factor, sin_factor, cos_factor: Double;
begin
  if PositionSeries.Count = 0 then
    FStartTime := Now();

  t := (Now() - FStartTime) * SecsPerDay;
  exp_factor := exp(-t/td);
  sin_factor := sin(TWO_PI * t / t0);
  cos_factor := cos(TWO_PI * t / t0);

  // Position: an exponentially damped sinusoidal motion
  x := A0 * sin_factor * exp_factor;

  // velocity = dx/dt
  v := 1.0 / (t0*td) * (A0  * exp_factor * (2.0*pi*td * cos_factor - t0 * sin_factor));

  // acceleration = dv/dt = d2x/dt2
  a := x / sqr(td) - sqr(TWO_PI/t0) * x - 2.0*TWO_PI*A0/(t0*td) * exp_factor * cos_factor;

  // Add values to series
  PositionSeries.AddXY(t, x);
  VelocitySeries.AddXY(t, v);
  AccelerationSeries.AddXY(t, a);
end;

{ Toggles between normal view and scrolling live view. In case of normal view
  the entire extent of the chart is shown. }
procedure TForm1.cbLiveViewChange(Sender: TObject);
begin
  ChartLiveView1.Active := cbLiveView.Checked;
  if not ChartLiveView1.Active then
    Chart1.ZoomFull;
end;

{ Toggles between automatic scaling of the axes and predefined axis limits
  (see FormCreate for the predefined values). }
procedure TForm1.cbFrozenAxesChange(Sender: TObject);
var
  i: Integer;
  wasActive: Boolean;
begin
  wasActive := ChartLiveView1.Active;
  ChartLiveView1.Active := false;     // LiveView must be off when changing axes
  for i := 1 to 3 do
    with Chart1.AxisList[i].Range do
    begin
      UseMax := cbFrozenAxes.Checked;
      UseMin := cbFrozenAxes.Checked;
    end;
  ChartLiveView1.Active := wasActive;  // Restore liveview state
end;

{ Resets and starts a new data generation run. }
procedure TForm1.btnResetClick(Sender: TObject);
begin
  Timer.Enabled := false;
  PositionSeries.Clear;
  VelocitySeries.Clear;
  AccelerationSeries.Clear;
  Timer.Enabled := true;
  btnStopResume.Caption := 'Stop';
end;

{ Stop or resume the data generation. When stopped a line break is added to the
  series. }
procedure TForm1.btnStopResumeClick(Sender: TObject);
var
  t: Double;
begin
  Timer.Enabled := not Timer.Enabled;
  if Timer.Enabled then
    btnStopResume.Caption := 'Stop'
  else begin
    btnStopResume.Caption := 'Resume';
    // Add a break to the curves
    t := (Now() - FStartTime) * SecsPerDay;
    PositionSeries.AddXY(t, NaN);
    VelocitySeries.AddXY(t, NaN);
    AccelerationSeries.AddXY(t, NaN);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  { The following settings could also be made in the object inspector. }

  // LiveView must be off when setting up the axes.
  with Chart1.AxisList[1].Range do
  begin
    UseMax := true;
    UseMin := true;
    Max := 10;
    Min := -10;
  end;
  with Chart1.AxisList[2].Range do
  begin
    UseMax := true;
    UseMin := true;
    Max := 15;
    Min := -15;
  end;
  with Chart1.AxisList[3].Range do
  begin
    UseMax := true;
    UseMin := true;
    Max := 15.0;
    Min := -15.0;
  end;

  // Activate the live view
  ChartLiveView1.Active := true;

  // Start the "measurement"
  Timer.Enabled := true;
end;

{ Set the liveview's ViewportSize according to the value in the SpinEdit }
procedure TForm1.seViewportSizeChange(Sender: TObject);
begin
  ChartLiveView1.ViewportSize := seViewportSize.Value;
end;

end.

