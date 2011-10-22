unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, TAGraph, TAMultiSeries, TASeries, TASources, TAStyles;

type

  { TForm1 }

  TForm1 = class(TForm)
    ccsStacked: TCalculatedChartSource;
    cbPercentage: TCheckBox;
    chOHLC: TChart;
    ChartStyles1: TChartStyles;
    chOHLCOpenHighLowCloseSeries1: TOpenHighLowCloseSeries;
    chStackedAreaSeries1: TAreaSeries;
    chStackedLineSeries1: TLineSeries;
    chWhiskers: TChart;
    chStacked: TChart;
    chBubble: TChart;
    Chart1BubbleSeries1: TBubbleSeries;
    chStackedBarSeries1: TBarSeries;
    chWhiskersBoxAndWhiskerSeries1: TBoxAndWhiskerSeries;
    lcsBubble: TListChartSource;
    PageControl1: TPageControl;
    pnStackedControls: TPanel;
    rgStackedSeries: TRadioGroup;
    rcsStacked: TRandomChartSource;
    tsOHLC: TTabSheet;
    tsWhiskers: TTabSheet;
    tsStacked: TTabSheet;
    tsBubble: TTabSheet;
    procedure cbPercentageChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgStackedSeriesClick(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.cbPercentageChange(Sender: TObject);
begin
  ccsStacked.Percentage := cbPercentage.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ylist: array [1..4] of Double;
  i, j: Integer;
  y, y0: Double;
begin
  chWhiskersBoxAndWhiskerSeries1.ListSource.YCount := 5;
  for i := 1 to 6 do begin
    y := Random(80) + 10;
    y0 := y;
    for j := 1 to 4 do begin
      y += Random(20) + 5;
      ylist[j] := y;
    end;
    chWhiskersBoxAndWhiskerSeries1.AddXY(i, y0, ylist);
  end;

  chOHLCOpenHighLowCloseSeries1.ListSource.YCount := 4;
  y := 50;
  for i := 1 to 50 do begin
    y += Random(80) / 10 - 4;
    ylist[1] := y;
    for j := 1 to 3 do begin
      ylist[j] += Random(20) / 10 + 1;
      ylist[j + 1] := ylist[j];
    end;
    chOHLCOpenHighLowCloseSeries1.AddXY(i, y, ylist);
  end;
end;

procedure TForm1.rgStackedSeriesClick(Sender: TObject);
var
  i: Integer;
begin
  i := rgStackedSeries.ItemIndex;
  chStackedAreaSeries1.Active := i = 0;
  chStackedBarSeries1.Active := i = 1;
  chStackedLineSeries1.Active := i = 2;
end;

end.

