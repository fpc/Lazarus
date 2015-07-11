unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, PrintersDlgs, StdCtrls, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Panel1: TPanel;
    Print: TButton;
    PrintCanvas: TButton;
    PrintDialog1: TPrintDialog;
    RandomChartSource1: TRandomChartSource;
    procedure PrintClick(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

uses
  OSPrinters, TAPrint, Printers;

{$R *.lfm}

{ TForm1 }

procedure TForm1.PrintClick(Sender: TObject);
const
  MARGIN = 10;
var
  r: TRect;
  d: Integer;
begin
  if not PrintDialog1.Execute then exit;
  Printer.BeginDoc;
  try
    r := Rect(0, 0, Printer.PageWidth, Printer.PageHeight div 2);
    d := r.Right - r.Left;
    r.Left += d div MARGIN;
    r.Right -= d div MARGIN;
    d := r.Bottom - r.Top;
    r.Top += d div MARGIN;
    r.Bottom -= d div MARGIN;
    if Sender = PrintCanvas then
      Chart1.PaintOnCanvas(Printer.Canvas, r)
    else
      Chart1.Draw(TPrinterDrawer.Create(Printer), r);
  finally
    Printer.EndDoc;
  end;
end;

end.

