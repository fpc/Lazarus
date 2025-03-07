unit frmParametric;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, math,
  Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Spin,
  TAGraph, TAFuncSeries;

type
  TParametricFrame = class(TFrame)
    Chart: TChart;
    ChartParametricCurveSeries1: TParametricCurveSeries;
    lblA: TLabel;
    lblB: TLabel;
    lblC: TLabel;
    lblD: TLabel;
    lblJ: TLabel;
    lblK: TLabel;
    pnlParametric: TPanel;
    seJ: TSpinEdit;
    seK: TSpinEdit;
    stEq: TStaticText;
    tbA: TTrackBar;
    tbB: TTrackBar;
    tbC: TTrackBar;
    tbD: TTrackBar;
    procedure ChartParametricCurveSeries1Calculate(const AT: Double;
      out AX, AY: Double);
    procedure ParamChange(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TParametricFrame.ChartParametricCurveSeries1Calculate(const AT
  : Double; out AX, AY: Double);
var
  a, b, c, d: Double;
begin
  a := tbA.Position / tbA.Frequency;
  b := tbB.Position / tbB.Frequency;
  c := tbC.Position / tbC.Frequency;
  d := tbD.Position / tbD.Frequency;
  AX := Cos(a * AT) - IntPower(Cos(b * AT), seJ.Value);
  AY := Sin(c * AT) - IntPower(Sin(d * AT), seK.Value);
end;

procedure TParametricFrame.ParamChange(Sender: TObject);
begin
  Chart.Invalidate;
end;

end.

