unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, TAGraph,
  TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    LabelsChartSource: TListChartSource;
    RedBarSeries: TBarSeries;
    BlueBarSeries: TBarSeries;
    YellowBarSeries: TBarSeries;
    RedChartSource: TRandomChartSource;
    BlueChartSource: TRandomChartSource;
    YellowChartSource: TRandomChartSource;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.
