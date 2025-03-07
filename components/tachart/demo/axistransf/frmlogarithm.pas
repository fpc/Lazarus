unit frmLogarithm;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, Math,
  Forms, Controls, StdCtrls, ExtCtrls, Spin,
  TAGraph, TAStyles, TASeries, TAFuncSeries, TATransformations;

type
  TLogarithmFrame = class(TFrame)
    catLog: TChartAxisTransformations;
    cbLog: TCheckBox;
    cfsLog: TFuncSeries;
    ChartAxisTransformations1LinearAxisTransform2: TLinearAxisTransform;
    ChartAxisTransformations1LogarithmAxisTransform1: TLogarithmAxisTransform;
    Chart: TChart;
    clsLogPoints: TLineSeries;
    csStripes: TChartStyles;
    lblTolerance: TLabel;
    pnlLogControls: TPanel;
    seTolerance: TSpinEdit;
    procedure cbLogChange(Sender: TObject);
    procedure cfsLogCalculate(const AX: Double; out AY: Double);
    procedure seToleranceChange(Sender: TObject);
  private

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

function MyFunc(AX: Double): Double;
begin
  Result := Power(10, AX) + 3;
end;

constructor TLogarithmFrame.Create(AOwner: TComponent);
var
  i: Integer;
  x: Double;
begin
  inherited Create(AOwner);

  for i := 0 to 50 do begin
    with cfsLog.Extent do
      x := i / 50 * (XMax - XMin) + XMin;
    clsLogPoints.AddXY(x + Random - 0.5, MyFunc(x) + Random - 0.5);
  end;
  seTolerance.Value := Chart.LeftAxis.Intervals.Tolerance;
end;

procedure TLogarithmFrame.cbLogChange(Sender: TObject);
begin
  ChartAxisTransformations1LogarithmAxisTransform1.Enabled := cbLog.Checked;
end;

procedure TLogarithmFrame.cfsLogCalculate(const AX: Double; out AY: Double);
begin
  AY := MyFunc(AX);
end;

procedure TLogarithmFrame.seToleranceChange(Sender: TObject);
begin
  Chart.LeftAxis.Intervals.Tolerance := seTolerance.Value;
end;


end.

