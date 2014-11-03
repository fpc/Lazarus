// NOTE: This is very experimental code. Do not expect it to work!

unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSVG: TButton;
    btnGCode: TButton;
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    procedure btnGCodeClick(Sender: TObject);
    procedure btnSVGClick(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  FPVectorial, SVGVectorialWriter, avisocncgcodewriter, TADrawerFPVectorial,
  TADrawUtils, TADrawerCanvas;

procedure SaveAs(AChart: TChart; AFormat: TvVectorialFormat);
const
(*
    vfUnknown,
    { Multi-purpose document formats }
    vfPDF, vfSVG, vfSVGZ, vfCorelDrawCDR, vfWindowsMetafileWMF, vfODG,
    { CAD formats }
    vfDXF,
    { Geospatial formats }
    vfLAS, vfLAZ,
    { Printing formats }
    vfPostScript, vfEncapsulatedPostScript,
    { GCode formats }
    vfGCodeAvisoCNCPrototipoV5, vfGCodeAvisoCNCPrototipoV6,
    { Formula formats }
    vfMathML,
    { Text Document formats }
    vfODT, vfDOCX, vfHTML,
    { Raster Image formats }
    vfRAW
*)
  ext: array [TvVectorialFormat] of String = (
    '',  // vfUnknown
    'pdf', 'svg', 'svgz', 'cdr', 'wmf', 'odg',
    'dxf',
    'laf', 'laz',
    'ps', 'eps',
    'gcode5', 'gcode6',
    'mathml',
    'odt', 'docx', 'html',
    'raw');
var
  d: TvVectorialDocument;
  v: IChartDrawer;
begin
  d := TvVectorialDocument.Create;
  d.AddPage;
  d.Width := AChart.Width;
  d.Height := AChart.Height;
  v := TFPVectorialDrawer.Create(d.GetCurrentPageAsVectorial);
  v.DoChartColorToFPColor := @ChartColorSysToFPColor;

  with AChart do
    Draw(v, Rect(0, Height, Width, Height*2));
    // why is it necessary to add 1x Height to y?
    // Otherwise the chart would not be on the page.

  d.WriteToFile('test.' + ext[AFormat], AFormat);
end;

{ TForm1 }

procedure TForm1.btnGCodeClick(Sender: TObject);
begin
  SaveAs(Chart1, vfGCodeAvisoCNCPrototipoV5);
end;

procedure TForm1.btnSVGClick(Sender: TObject);
begin
  SaveAs(Chart1, vfSVG);
end;

end.

