unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls, Graphics,
  FPCanvas, Dialogs, Agg_LCL, TAGraph, TASeries, TASources, TADrawerAggPas;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    Chart1BarSeries1: TBarSeries;
    Chart1ConstantLine1: TConstantLine;
    Chart1LineSeries1: TLineSeries;
    Chart1PieSeries1: TPieSeries;
    cbAggPas: TCheckBox;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    procedure cbAggPasClick(Sender: TObject);
    procedure Chart1AfterPaint(ASender: TChart);
    procedure ChartPaint(
      ASender: TChart; const ARect: TRect; var ADoDefaultDrawing: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    FAggCanvas: TAggLCLCanvas;
    FBmp: TBitmap;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.cbAggPasClick(Sender: TObject);
begin
  if cbAggPas.Checked then
    Chart1.OnChartPaint := @ChartPaint
  else
    Chart1.OnChartPaint := nil;
end;

procedure TForm1.Chart1AfterPaint(ASender: TChart);
begin
  PaintBox1.Invalidate;
end;

procedure TForm1.ChartPaint(ASender: TChart; const ARect: TRect;
  var ADoDefaultDrawing: Boolean);
begin
  FAggCanvas.Width := ARect.Right - ARect.Left;
  FAggCanvas.Height := ARect.Bottom - ARect.Top;
  ASender.Draw(TAggPasDrawer.Create(FAggCanvas), ARect);
  FBmp.LoadFromIntfImage(FAggCanvas.Image.IntfImg);
  ASender.Canvas.Draw(0, 0, FBmp);
  ADoDefaultDrawing := false;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FBmp := TBitmap.Create;
  FAggCanvas := TAggLCLCanvas.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FAggCanvas.Free;
  FBmp.Free;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  FAggCanvas.Width := PaintBox1.Width;
  FAggCanvas.Height := PaintBox1.Height;
  Chart1.DisableRedrawing;
  Chart1.Title.Text.Text := 'AggPas';
  Chart1.Draw(TAggPasDrawer.Create(FAggCanvas), PaintBox1.Canvas.ClipRect);
  Chart1.Title.Text.Text := 'Standard';
  Chart1.EnableRedrawing;
  FBmp.LoadFromIntfImage(FAggCanvas.Image.IntfImg);
  PaintBox1.Canvas.Draw(0, 0, FBmp);
end;

end.

