unit frmDomain;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Graphics, Forms, Controls, StdCtrls,
  TAGraph, TAChartUtils, TAFuncSeries, TASources, Types, TASeries, TACustomSource;

type
  TDomainFrame = class(TFrame)
    cbDomain: TCheckBox;
    cbRotate: TCheckBox;
    Chart: TChart;
    BarSeries: TBarSeries;
    FuncSeries: TFuncSeries;
    UserDrawnSeries: TUserDrawnSeries;
    ChartXAxis: TConstantLine;
    ChartYAxis: TConstantLine;
    UserDefinedChartSource1: TUserDefinedChartSource;
    procedure cbDomainChange(Sender: TObject);
    procedure cbRotateChange(Sender: TObject);
    procedure FuncSeriesCalculate(const AX: Double; out AY: Double);
    procedure UserDefinedChartSource1GetChartDataItem(ASource:
      TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
    procedure UserDrawnSeriesDraw(ACanvas: TCanvas; const ARect: TRect);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TDomainFrame.cbDomainChange(Sender: TObject);
var
  i: Integer;
begin
  with FuncSeries.DomainExclusions do begin
    Clear;
    Epsilon := 1e-7;
    if cbDomain.Checked then
      for i := -10 to 10 do
        AddPoint(i * Pi);
  end;
end;

procedure TDomainFrame.cbRotateChange(Sender: TObject);
begin
  with FuncSeries do
    if cbRotate.Checked then begin
      AxisIndexX := 0;
      AxisIndexY := 1;
    end
    else begin
      AxisIndexX := 1;
      AxisIndexY := 0;
    end;
end;

procedure TDomainFrame.FuncSeriesCalculate(const AX: Double; out AY:
  Double);
begin
  AY := 1 / Sin(AX);
end;

procedure TDomainFrame.UserDefinedChartSource1GetChartDataItem(ASource:
  TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := AIndex - ASource.PointsNumber / 2;
  AItem.Y := Cos(AItem.X);
end;

procedure TDomainFrame.UserDrawnSeriesDraw(ACanvas: TCanvas;
  const ARect: TRect);
var
  a: TDoublePoint = (X: -1; Y: -1);
  b: TDoublePoint = (X: 1; Y: 1);
  r: TRect;
begin
  Unused(ARect);
  r.TopLeft := Chart.GraphToImage(a);
  r.BottomRight := Chart.GraphToImage(b);
  ACanvas.Pen.Mode := pmCopy;
  ACanvas.Pen.Color := clRed;
  ACanvas.Pen.Style := psDash;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Ellipse(r);
end;

end.

