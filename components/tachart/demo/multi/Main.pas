unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  TAGraph, TAMultiSeries, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    chStackedArea: TChart;
    chStackedAreaAreaSeries1: TAreaSeries;
    chStackedLines: TChart;
    chStackedLinesLineSeries1: TLineSeries;
    chWhiskers: TChart;
    chStackedBars: TChart;
    chBubble: TChart;
    Chart1BubbleSeries1: TBubbleSeries;
    chStackedBarsBarSeries1: TBarSeries;
    chWhiskersBoxAndWhiskerSeries1: TBoxAndWhiskerSeries;
    lcsBubble: TListChartSource;
    PageControl1: TPageControl;
    rcsStacked: TRandomChartSource;
    tsStackedArea: TTabSheet;
    tsStackedLines: TTabSheet;
    tsWhiskers: TTabSheet;
    tsStackedBar: TTabSheet;
    tsBubble: TTabSheet;
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

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
end;

end.

