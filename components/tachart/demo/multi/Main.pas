unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  TAGraph, TAMultiSeries, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    chStackedBars: TChart;
    chBubble: TChart;
    Chart1BubbleSeries1: TBubbleSeries;
    chStackedBarsBarSeries1: TBarSeries;
    lcsBubble: TListChartSource;
    PageControl1: TPageControl;
    rcsStacked: TRandomChartSource;
    tsStackedBar: TTabSheet;
    tsBubble: TTabSheet;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

end.

