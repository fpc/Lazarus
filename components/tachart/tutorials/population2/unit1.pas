unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, Forms, Controls,
  Graphics, Dialogs, population, TACustomSource, TATransformations, TAChartAxisUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    LeftAxisTransformations: TChartAxisTransformations;
    LeftAxisAutoScaleTransform: TAutoScaleAxisTransform;
    RightAxisTransformations: TChartAxisTransformations;
    LineSeries_male: TLineSeries;
    LineSeries_female: TLineSeries;
    LineSeries_ratio: TLineSeries;
    ChartSource_male: TUserDefinedChartSource;
    ChartSource_female: TUserDefinedChartSource;
    ChartSource_ratio: TUserDefinedChartSource;
    RightAxisAutoScaleTransform: TAutoScaleAxisTransform;
    procedure Chart1AxisList0MarkToText(var AText: String; AMark: Double);
    procedure ChartSourceGetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    PopulationData: TPopulationArray;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  POPULATION_FILE = 'population.txt';

{ TForm1 }

procedure TForm1.ChartSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := PopulationData[AIndex].Age;
  if ASource = ChartSource_male then
    AItem.Y := PopulationData[AIndex].Male / 1e6
  else if ASource = ChartSource_female then
    AItem.Y := PopulationData[AIndex].Female / 1e6
  else
    AItem.Y := PopulationData[AIndex].Ratio / 100;
end;

procedure TForm1.Chart1AxisList0MarkToText(var AText: String; AMark: Double);
begin
  AText := Format('%s M', [AText]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadPopulationData(POPULATION_FILE, PopulationData);
  ChartSource_male.PointsNumber := Length(PopulationData);
  ChartSource_female.PointsNumber := Length(PopulationData);
  ChartSource_ratio.PointsNumber := Length(PopulationData);
end;

end.

