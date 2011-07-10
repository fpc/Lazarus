unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    Chart1BarSeries1: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    Chart1PieSeries1: TPieSeries;
    cbAntialiasing: TCheckBox;
    cbPie: TCheckBox;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    Splitter1: TSplitter;
    procedure cbAntialiasingChange(Sender: TObject);
    procedure cbPieChange(Sender: TObject);
    procedure Chart1AfterPaint(ASender: TChart);
    procedure PaintBox1Paint(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  BGRABitmap, TADrawerBGRA, TADrawerCanvas, TADrawUtils;

{ TForm1 }

procedure TForm1.cbAntialiasingChange(Sender: TObject);
begin
  if cbAntialiasing.Checked then
    Chart1.AntialiasingMode := amOn
  else
    Chart1.AntialiasingMode := amOff;
end;

procedure TForm1.cbPieChange(Sender: TObject);
begin
  Chart1PieSeries1.Active := cbPie.Checked;
end;

procedure TForm1.Chart1AfterPaint(ASender: TChart);
begin
  PaintBox1.Invalidate;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  bmp: TBGRABitmap;
  id: IChartDrawer;
  rp: TChartRenderingParams;
begin
  bmp := TBGRABitmap.Create(PaintBox1.Width, PaintBox1.Height);
  Chart1.DisableRedrawing;
  try
    Chart1.Title.Text.Text := 'BGRABitmap';
    id := TBGRABitmapDrawer.Create(bmp);
    id.DoGetFontOrientation := @CanvasGetFontOrientationFunc;
    rp := Chart1.RenderingParams;
    Chart1.Draw(id, Rect(0, 0, PaintBox1.Width, PaintBox1.Height));
    Chart1.RenderingParams := rp;
    bmp.Draw(PaintBox1.Canvas, 0, 0);
    Chart1.Title.Text.Text := 'Standard';
  finally
    Chart1.EnableRedrawing;
    bmp.Free;
  end;
end;

end.

