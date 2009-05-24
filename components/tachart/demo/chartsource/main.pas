unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    Chart2: TChart;
    Chart2AreaSeries1: TAreaSeries;
    Chart2LineSeries1: TLineSeries;
    ListChartSource1: TListChartSource;
    Memo1: TMemo;
    RandomChartSource1: TRandomChartSource;
    Splitter1: TSplitter;
  end;

var
  Form1: TForm1; 

implementation

initialization
  {$I main.lrs}

end.

