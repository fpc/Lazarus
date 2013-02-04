unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls,
  Graphics, FPCanvas, Dialogs, Agg_LCL, TAGraph, TAGUIConnectorAggPas, TASeries,
  TASources, TADrawerAggPas, TADrawUtils;

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
    ChartGUIConnectorAggPas1: TChartGUIConnectorAggPas;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    procedure cbAggPasClick(Sender: TObject);
    procedure Chart1AfterPaint(ASender: TChart);
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

uses
  TAChartUtils, TADrawerCanvas;

{ TForm1 }

procedure TForm1.cbAggPasClick(Sender: TObject);
begin
  if cbAggPas.Checked then
    Chart1.GUIConnector := ChartGUIConnectorAggPas1
  else
    Chart1.GUIConnector := nil;
end;

procedure TForm1.Chart1AfterPaint(ASender: TChart);
begin
  Unused(ASender);
  PaintBox1.Invalidate;
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
var
  d: IChartDrawer;
begin
  FAggCanvas.Width := PaintBox1.Width;
  FAggCanvas.Height := PaintBox1.Height;
  Chart1.DisableRedrawing;
  Chart1.Title.Text.Text := 'AggPas';
  d := TAggPasDrawer.Create(FAggCanvas);
  d.DoGetFontOrientation := @CanvasGetFontOrientationFunc;
  Chart1.Draw(d, PaintBox1.Canvas.ClipRect);
  Chart1.Title.Text.Text := 'Standard';
  Chart1.EnableRedrawing;
  FBmp.LoadFromIntfImage(FAggCanvas.Image.IntfImg);
  PaintBox1.Canvas.Draw(0, 0, FBmp);
end;

end.

