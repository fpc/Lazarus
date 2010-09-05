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
    ChartStyles1: TChartStyles;
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
  i, j, y: Integer;
begin
  chWhiskersBoxAndWhiskerSeries1.ListSource.YCount := 5;
  for i := 1 to 6 do begin
    y := Random(80) + 10;
    chWhiskersBoxAndWhiskerSeries1.AddXY(i, y);
    for j := 1 to 4 do begin
      y += Random(20) + 5;
      ylist[j] := y;
    end;
    chWhiskersBoxAndWhiskerSeries1.ListSource.SetYList(i - 1, ylist);
  end;
  rgStackedSeries.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
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

