unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, SysUtils, FileUtil, Forms, Controls, Graphics, FPCanvas,
  Dialogs, Agg_LCL, TAGraph, TASeries, TASources, TADrawerAggPas;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1PieSeries1: TPieSeries;
    PaintBox1: TPaintBox;
    RandomChartSource1: TRandomChartSource;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    FBmp: TBitmap;
    FAggCanvas: TAggLCLCanvas;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  TADrawUtils;

{ TForm1 }

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
  ad: IChartDrawer;
begin
  FAggCanvas.Width := PaintBox1.Width;
  FAggCanvas.Height := PaintBox1.Height;
  ad := TAggPasDrawer.Create(FAggCanvas);
  Chart1.Title.Text.Text := 'AggPas';
  Chart1.Draw(ad, PaintBox1.Canvas.ClipRect);
  Chart1.Title.Text.Text := 'Standard';
  FBmp.LoadFromIntfImage(FAggCanvas.Image.IntfImg);
  PaintBox1.Canvas.Draw(0, 0, FBmp);
end;

end.

