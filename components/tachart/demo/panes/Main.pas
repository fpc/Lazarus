unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  TAChartExtentLink, TAGraph, TASeries, TASources;

type
  TForm1 = class(TForm)
    ChartExtentLink1: TChartExtentLink;
    chLink1: TChart;
    chLink1LineSeries1: TLineSeries;
    chLink2LineSeries1: TLineSeries;
    chLink3LineSeries1: TLineSeries;
    chLink2: TChart;
    chLink3: TChart;
    PageControl1: TPageControl;
    rcsLink1: TRandomChartSource;
    rcsLink2: TRandomChartSource;
    rcsLink3: TRandomChartSource;
    tsLink: TTabSheet;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

end.

