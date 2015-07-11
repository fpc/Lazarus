unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, Spin, StdCtrls, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, TAGraph, TASeries, TASources, TACustomSource;

type
  TForm1 = class(TForm)
    btnGenerate: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1ManhattanSeries1: TManhattanSeries;
    lblTime: TLabel;
    pnlControls: TPanel;
    seCount: TSpinEdit;
    UserDefinedChartSource1: TUserDefinedChartSource;
    procedure btnGenerateClick(Sender: TObject);
    procedure Chart1AfterDrawBackground(
      ASender: TChart; ACanvas: TCanvas; const ARect: TRect);
    procedure Chart1AfterPaint(ASender: TChart);
    procedure UserDefinedChartSource1GetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LCLIntf, TAChartUtils;

var
  GaussDevAvail: Boolean = false;
  GaussDev: Double = 0.0;

// Create a random number with normal distribution, mean value 0, standard
// deviation 1. See Numerical Recipes.
function RndNormal: Double;
var
  fac, r, v1, v2: Double;
begin
  if GaussDevAvail then
    Result := GaussDev
  else begin
    repeat
      v1 := 2.0 * Random - 1.0;
      v2 := 2.0 * Random - 1.0;
      r := Sqr(v1) + Sqr(v2);
    until (r > 0.0) and (r < 1.0);
    fac := Sqrt(-2.0 * Ln(r) / r);
    GaussDev := v1 * fac;
    Result := v2 * fac;
  end;
  GaussDevAvail := not GaussDevAvail;
end;

var
  VData: array of record X, Y: Double; Color: TColor; end;
  t0: Int64;

{ TForm1 }

procedure TForm1.btnGenerateClick(Sender: TObject);
const
  COLORS: array [1..5] of TColor = (clYellow, clBlue, clGreen, clMaroon, clFuchsia);
var
  i: Integer;
begin
  SetLength(VData, seCount.Value);
  for i := 0 to High(VData) do
    with VData[i] do begin
      X := Random(1000);
      Y := Abs(RndNormal);
      Color := COLORS[Trunc(X / 1000 * Length(COLORS)) + 1];
    end;
  UserDefinedChartSource1.PointsNumber := seCount.Value;
  UserDefinedChartSource1.Reset;
end;

procedure TForm1.Chart1AfterDrawBackground(
  ASender: TChart; ACanvas: TCanvas; const ARect: TRect);
begin
  Unused(ASender);
  Unused(ACanvas, ARect);
  t0 := GetTickCount64;
end;

procedure TForm1.Chart1AfterPaint(ASender: TChart);
begin
  Unused(ASender);
  lblTime.Caption := Format('Time: %d ms', [GetTickCount64 - t0]);
end;

procedure TForm1.UserDefinedChartSource1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  Unused(ASource);
  with VData[AIndex] do begin
    AItem.X := X;
    AItem.Y := Y;
    AItem.Color := Color;
  end;
end;

end.

