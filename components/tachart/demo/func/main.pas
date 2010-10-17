unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, StdCtrls, Forms, Graphics, TAFuncSeries, TAGraph, TASeries,
  TASources, TATools;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbDomain: TCheckBox;
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1FuncSeries1: TFuncSeries;
    Chart1UserDrawnSeries1: TUserDrawnSeries;
    Chart1XAxis: TConstantLine;
    Chart1YAxis: TConstantLine;
    ChartColorMap: TChart;
    ChartColorMapColorMapSeries1: TColorMapSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ListChartSource1: TListChartSource;
    PageControl1: TPageControl;
    tsDomain: TTabSheet;
    tsColorMap: TTabSheet;
    UserDefinedChartSource1: TUserDefinedChartSource;
    procedure cbDomainChange(Sender: TObject);
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure Chart1UserDrawnSeries1Draw(ACanvas: TCanvas; const ARect: TRect);
    procedure ChartColorMapColorMapSeries1Calculate(const AX, AY: Double; out
      AZ: Double);
    procedure UserDefinedChartSource1GetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
  end;

var
  Form1: TForm1; 

implementation

uses
  TAChartUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.cbDomainChange(Sender: TObject);
var
  i: Integer;
begin
  with Chart1FuncSeries1.DomainExclusions do begin
    Clear;
    Epsilon := 1e-7;
    if cbDomain.Checked then
      for i := -10 to 10 do
        AddPoint(i * Pi);
  end;
end;

procedure TForm1.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := 1 / Sin(AX);
end;

procedure TForm1.Chart1UserDrawnSeries1Draw(
  ACanvas: TCanvas; const ARect: TRect);
var
  a: TDoublePoint = (X: -1; Y: -1);
  b: TDoublePoint = (X: 1; Y: 1);
  r: TRect;
begin
  r.TopLeft := Chart1.GraphToImage(a);
  r.BottomRight := Chart1.GraphToImage(b);
  ACanvas.Pen.Mode := pmCopy;
  ACanvas.Pen.Color := clRed;
  ACanvas.Pen.Style := psDash;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Ellipse(r);
end;

procedure TForm1.ChartColorMapColorMapSeries1Calculate(
  const AX, AY: Double; out AZ: Double);
begin
  AZ := Sin(10 * Sqr(AX) + 17 * Sqr(AY));
end;

procedure TForm1.UserDefinedChartSource1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := AIndex - ASource.PointsNumber / 2;
  AItem.Y := Cos(AItem.X);
end;

end.

