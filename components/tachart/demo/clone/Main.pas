unit Main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnLine: TButton;
    btnBar: TButton;
    ChartSeries: TChart;
    ChartSeriesBarSeries1: TBarSeries;
    ChartSeriesLineSeries1: TLineSeries;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    tsSeries: TTabSheet;
    procedure btnBarClick(Sender: TObject);
    procedure btnLineClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnBarClick(Sender: TObject);
var
  bs: TBarSeries;
  i: Integer;
begin
  for i := ChartSeries.SeriesCount - 1 downto 0 do
    if ChartSeries.Series[i] is TBarSeries then begin
      bs := TBarSeries.Create(Self);
      bs.Assign(ChartSeries.Series[i]);
      bs.BarOffsetPercent := bs.BarOffsetPercent + 20;
      ChartSeries.AddSeries(bs);
      break;
    end;
end;

procedure TForm1.btnLineClick(Sender: TObject);
var
  ls: TLineSeries;
  i: Integer;
begin
  for i := ChartSeries.SeriesCount - 1 downto 0 do
    if ChartSeries.Series[i] is TLineSeries then begin
      ls := TLineSeries.Create(Self);
      ls.Assign(ChartSeries.Series[i]);
      ls.ZPosition := ls.ZPosition + 5;
      ChartSeries.AddSeries(ls);
      break;
    end;
end;

end.

