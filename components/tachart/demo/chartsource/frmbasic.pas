unit frmBasic;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls,
  TAGraph, TASources, TASeries;

type
  TBasicFrame = class(TFrame)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    Chart2: TChart;
    Chart2AreaSeries1: TAreaSeries;
    Chart2LineSeries1: TLineSeries;
    Label1: TLabel;
    ListChartSource1: TListChartSource;
    RandomChartSource1: TRandomChartSource;
    Splitter1: TSplitter;
  private

  public

  end;

implementation

{$R *.lfm}

end.

