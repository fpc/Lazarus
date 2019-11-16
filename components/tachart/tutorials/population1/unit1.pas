unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TASeries, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, population, TACustomSource;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    ComboBox1: TComboBox;
    Panel1: TPanel;
    UserDefinedChartSource1: TUserDefinedChartSource;
    procedure ComboBox1Select(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UserDefinedChartSource1GetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadPopulationData(POPULATION_FILE, PopulationData);
  UserDefinedChartSource1.PointsNumber := Length(PopulationData);
  Chart1.LeftAxis.Title.Caption := Combobox1.Items[Combobox1.ItemIndex];
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  Chart1.LeftAxis.Title.Caption := Combobox1.Items[Combobox1.ItemIndex];
  UserDefinedChartSource1.Reset;
end;

procedure TForm1.UserDefinedChartSource1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := PopulationData[AIndex].Age;
  case Combobox1.ItemIndex of
    0: AItem.Y := PopulationData[AIndex].Total;
    1: AItem.Y := PopulationData[AIndex].Male;
    2: AItem.Y := PopulationData[AIndex].Female;
    3: AItem.Y := PopulationData[AIndex].Ratio;
  end;
end;

end.

