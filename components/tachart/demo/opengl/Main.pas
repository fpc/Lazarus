unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, OpenGLContext, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, TAGraph, TASeries, TASources, GL, GLU;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    OpenGLControl1: TOpenGLControl;
    RandomChartSource1: TRandomChartSource;
    procedure Chart1AfterPaint(ASender: TChart);
    procedure OpenGLControl1Paint(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  TADrawUtils, TADrawerOpenGL, TADrawerCanvas;

procedure TForm1.Chart1AfterPaint(ASender: TChart);
begin
  OpenGLControl1.Invalidate;
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
var
  d: IChartDrawer;
begin
  glClearColor(1.0, 1.0, 1.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  with OpenGLControl1 do
    gluOrtho2D(0, Width, Height, 0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  d := TOpenGLDrawer.Create;
  d.DoChartColorToFPColor := @ChartColorSysToFPColor;
  Chart1.DisableRedrawing;
  Chart1.Title.Text.Text := 'OpenGL';
  Chart1.Draw(d, Rect(0, 0, OpenGLControl1.Width, OpenGLControl1.Height));
  Chart1.Title.Text.Text := 'Standard';
  Chart1.EnableRedrawing;

  OpenGLControl1.SwapBuffers;
end;

end.

