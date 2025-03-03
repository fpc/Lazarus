unit frmErrorRange;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  Forms, Controls, StdCtrls,
  TAGraph, TACustomSource, TASources, TASeries;

type
  TErrorRangeFrame = class(TFrame)
    Chart: TChart;
    ChartAreaSeries1: TAreaSeries;
    ChartAreaSeries2: TAreaSeries;
    ChartLineSeries1: TLineSeries;
    ChartLineSeries2: TLineSeries;
    Label1: TLabel;
    ChartSourceLine1: TUserDefinedChartSource;
    ChartSourceArea1: TUserDefinedChartSource;
    ChartSourceLine2: TUserDefinedChartSource;
    ChartSourceArea2: TUserDefinedChartSource;
    procedure ChartSourceArea1GetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure ChartSourceArea2GetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure ChartSourceLine1GetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure ChartSourceLine2GetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);

  private
    procedure PopulateSeries;

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

type
  TData = record
    x, y, dy: Double;
  end;
  TDataArray = array of TData;

var
  Data1, Data2: TDataArray;

constructor TErrorRangeFrame.Create(AOwner: TComponent);
begin
  inherited;
  PopulateSeries;
end;

procedure TErrorRangeFrame.ChartSourceArea1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := Data1[AIndex].x;
  AItem.Y := Data1[AIndex].y - Data1[AIndex].dy;
  AItem.YList[0] := Data1[AIndex].y + Data1[AIndex].dy;
end;

procedure TErrorRangeFrame.ChartSourceArea2GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := Data2[AIndex].x;
  AItem.Y := Data2[AIndex].y;
  AItem.YList[0] := -Data2[AIndex].dy;
  AItem.YList[1] := 2*Data2[AIndex].dy;
end;

procedure TErrorRangeFrame.ChartSourceLine1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := Data1[AIndex].X;
  AItem.Y := Data1[AIndex].Y;
end;

procedure TErrorRangeFrame.ChartSourceLine2GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := Data2[AIndex].X;
  AItem.Y := Data2[AIndex].Y;
end;

procedure TErrorRangeFrame.PopulateSeries;
const
  N1 = 20;
  N2 = 25;
  XMIN = -10;
  XMAX = +10;
var
  i: Integer;
begin
  SetLength(Data1, N1);
  for i:=0 to N1-1 do begin
    Data1[i].x := XMIN + (XMAX - XMIN) * i / (N1-1);
    Data1[i].y := sin(Data1[i].x * 0.5);
    Data1[i].dy := randg(0.2, 0.05);
  end;
  ChartSourceLine1.YCount := 1;
  ChartSourceLine1.PointsNumber := N1;
  ChartSourceArea1.YCount := 2;
  ChartSourceArea1.PointsNumber := N1;

  SetLength(Data2, N2);
  for i:=0 to N2-1 do begin
    Data2[i].x := XMIN + (XMAX - XMIN) * i / (N2-1);
    Data2[i].y := cos(Data2[i].x); // + randg(0, 0.2);
    Data2[i].dy := randg(0.2, 0.05);
  end;
  ChartSourceLine2.YCount := 1;
  ChartSourceLine2.PointsNumber := N2;
  ChartSourceArea2.YCount := 3;
  ChartSourceArea2.PointsNumber := N2;

  ChartAreaSeries1.Banded := true;
  ChartAreaSeries2.Banded := true;
end;


end.

