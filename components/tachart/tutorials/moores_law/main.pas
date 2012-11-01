unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, Forms, Controls,
  Graphics, Dialogs, TACustomSource, TATransformations, TAFuncSeries;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1FitSeries1: TFitSeries;
    Chart1LineSeries1: TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LogarithmAxisTransform1: TLogarithmAxisTransform;
    ListChartSource1: TListChartSource;
    ListChartSource2: TListChartSource;
    procedure Chart1FitSeries1FitComplete(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
const
  MIN = 0;
  MAX = 12;
var
  i: Integer;
  value: double;
begin
  for i := MIN to MAX do begin
    value := IntPower(10, i);
    ListChartSource2.Add(value, value);
  end;
end;

procedure TForm1.Chart1FitSeries1FitComplete(Sender: TObject);
begin
  {
  with Chart1FitSeries1 do
    ShowMessage(Format(
      'Fit result: a = %.9g, b = %.9g',
      [Param[0], Param[1]]
    ));
  }
  Chart1.Title.Text.Add(Format(
    'The number of transistors doubles every %.0f years',
    [ln(2) / Chart1FitSeries1.Param[1]]
  ));
end;

end.

