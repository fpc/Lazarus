unit Main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, LR_Class, StdCtrls, SysUtils,
  Forms, Controls, Graphics, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnShowReport: TButton;
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    frReport1: TfrReport;
    ListChartSource1: TListChartSource;
    Panel1: TPanel;
    procedure btnShowReportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure frReport1EnterRect(Memo: TStringList; View: TfrView);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  Math, Printers, TADrawUtils, TADrawerCanvas;

{ TForm1 }

procedure TForm1.btnShowReportClick(Sender: TObject);
begin
  frReport1.ShowReport;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  frReport1.LoadFromFile('chart.lrf');
end;

procedure TForm1.frReport1EnterRect(Memo: TStringList; View: TfrView);
var
  bmp: TBitmap;
  pv: TfrPictureView;
  factor: Double;
begin
  if Memo.Count = 0 then exit;
  if (Memo[0] = 'Chart1') and (View is TfrPictureView) then begin
    pv := TfrPictureView(View);
    factor := Max(Printer.XDpi, Printer.YDpi) / Screen.PixelsPerInch;
    bmp := TBitmap.Create;
    try
      bmp.Width := Round(pv.Width * factor);
      bmp.Height := Round(pv.Height * factor);
      Chart1.Draw(
        TScaledCanvasDrawer.Create(bmp.Canvas, factor, [scaleFont, scalePen]),
        Rect(0, 0, bmp.Width, bmp.Height));
      pv.Picture.Bitmap.Assign(bmp);
    finally
      bmp.Free;
    end;
  end;
end;

end.

