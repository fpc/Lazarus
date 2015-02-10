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
  FPVectorial,
  TADrawerFPVectorial, TADrawUtils, TADrawerCanvas;

procedure SaveAs(AChart: TChart; AFormat: TvVectorialFormat);
const
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
  fn: String;
begin
  d := TvVectorialDocument.Create;
  try
    d.Width := AChart.Width;
    d.Height := AChart.Height;
    d.AddPage;
    v := TFPVectorialDrawer.Create(d.GetCurrentPageAsVectorial);
    with AChart do
      Draw(v, Rect(0, 0, Width, Height));
    fn := 'test.' + ext[AFormat];
    d.WriteToFile(fn, AFormat);
    ShowMessage(Format('Chart saved as "%s"', [fn]));
  finally
    d.Free;
  end;
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

