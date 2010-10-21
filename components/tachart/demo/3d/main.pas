unit main;

{$mode objfpc}{$H+}

interface

uses
  ExtCtrls, Forms, Spin, StdCtrls,
  TAGraph, TASeries, TASources, Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1BarSeries2: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    lblDepth: TLabel;
    lblAxisZ: TLabel;
    pnControls: TPanel;
    RandomChartSource1: TRandomChartSource;
    RandomChartSource2: TRandomChartSource;
    seDepth: TSpinEdit;
    seAxisZ: TSpinEdit;
    procedure FormShow(Sender: TObject);
    procedure seAxisZChange(Sender: TObject);
    procedure seDepthChange(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  Math;

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  seDepth.Value := Chart1.Depth;
end;

procedure TForm1.seAxisZChange(Sender: TObject);
begin
  Chart1.LeftAxis.ZPosition := seAxisZ.Value;
  Chart1.BottomAxis.ZPosition := seAxisZ.Value;
end;

procedure TForm1.seDepthChange(Sender: TObject);
var
  i: Integer;
begin
  Chart1.Depth := seDepth.Value;
  for i := 0 to Chart1.SeriesCount - 1 do
    Chart1.Series[i].Depth := Min(seDepth.Value, 10);
end;

end.

