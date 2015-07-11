unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  TAChartExtentLink, TAGraph, TASeries, TASources, TATransformations;

type
  TForm1 = class(TForm)
    catAuto1: TChartAxisTransformations;
    catAuto1AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    catAuto1AutoScaleAxisTransform2: TAutoScaleAxisTransform;
    catAuto1AutoScaleAxisTransform3: TAutoScaleAxisTransform;
    catAuto2: TChartAxisTransformations;
    catAuto3: TChartAxisTransformations;
    chTransform: TChart;
    ChartExtentLink1: TChartExtentLink;
    chLink1: TChart;
    chLink1LineSeries1: TLineSeries;
    chLink2LineSeries1: TLineSeries;
    chLink3LineSeries1: TLineSeries;
    chLink2: TChart;
    chLink3: TChart;
    chTransformLineSeries1: TLineSeries;
    chTransformLineSeries2: TLineSeries;
    chTransformLineSeries3: TLineSeries;
    PageControl1: TPageControl;
    rcsLink1: TRandomChartSource;
    rcsLink2: TRandomChartSource;
    rcsLink3: TRandomChartSource;
    tsTransform: TTabSheet;
    tsLink: TTabSheet;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

end.

