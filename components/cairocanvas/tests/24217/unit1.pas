unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, PrintersDlgs, Forms, Controls, Graphics, Dialogs,
  StdCtrls, CairoPrinter;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    PrintDialog1: TPrintDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
    procedure DrawSample(cnv: TCanvas; XDPI,YDPI: Integer);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses types,printers;

{$R *.lfm}

{ TForm1 }
const str='Text For Extent: Прывітаньне!';
  sTextout='TextOut()';
  sTextRect='TextRect()';

procedure TForm1.Button1Click(Sender: TObject);
begin
  if PrintDialog1.Execute then
  begin
    Printer.Title:='AcairoTest';
    Printer.BeginDoc;

    with Printer do
      DrawSample(Canvas, XDPI, YDPI);

    Printer.EndDoc;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  CairoPrinter: TCairoFilePrinter;
begin
  CairoPrinter := TCairoFilePrinter.create;
  CairoPrinter.CairoBackend:=cbPS;
  //CairoPrinter.CairoBackend:=cbPDF;
  CairoPrinter.FileName:='salida';
  CairoPrinter.BeginDoc;
  with CairoPrinter do
    DrawSample(Canvas, XDPI, YDPI);
  CairoPrinter.EndDoc;
  CairoPrinter.Free;
end;

procedure TForm1.DrawSample(cnv: TCanvas; XDPI, YDPI: Integer);
var
  sz:TSize;
  r:tRect;
  i:integer;
  kx,ky:double;
begin

  cnv.Font.Size:=24;

  r.Left:=round(XDPI*0.5);
  r.Top:=round(YDPI*0.5);

  // paint with textout
  sz:=cnv.TextExtent(sTextOut);
  r.Right:=r.left+sz.cx;
  r.Bottom:=r.Top+sz.cy;
  cnv.Rectangle(r);
  cnv.TextOut(r.left,r.top,sTextOut);

  // paint with TextRect
  r.Top:=round(YDPI*1);
  sz:=cnv.TextExtent(sTextRect);
  r.Right:=r.left+sz.cx;
  r.Bottom:=r.Top+sz.cy;
  cnv.Rectangle(r);
  cnv.TextRect(R,r.left,r.top,sTextRect);

  // rotated text
  r.Left:=round(XDPI*2.5);
  r.Top:=round(YDPI*1);
  cnv.font.Orientation:=900;

  // paint with textout
  sz:=cnv.TextExtent(sTextOut);
  r.Right:=r.left+sz.cy;
  r.Bottom:=r.Top+sz.cx;
  cnv.Rectangle(r);
  cnv.TextOut(r.left,r.bottom,sTextOut);

  inc(r.Left,round(XDPI*0.5));
  // paint with text rect
  sz:=cnv.TextExtent(sTextRect);
  r.Right:=r.left+sz.cy;
  r.Bottom:=r.Top+sz.cx;
  cnv.Rectangle(r);
  // thsi works
  //cnv.TextOut(r.left,r.bottom,sTextOut);
  // thsi does not work
  cnv.TextRect(r,r.left,r.bottom,sTextRect);


  r:=Rect((XDPI*2),(YDPI*5),(XDPI*7),(YDPI*10));
  cnv.ClipRect:=r;
  cnv.Clipping:=true;
  kx:=(r.Right-r.Left)/1000;
  ky:=(r.bottom-r.top)/1000;
  cnv.MoveTo(r.Left,r.Bottom);
  for i:=0 to 999 do
  begin
    cnv.LineTo(r.left+round(kx*i),r.Bottom-round(ky*2000*exp(-sqr(i-500)/500)));
  end;
  cnv.font.Orientation:=0;
  cnv.TextOut((XDPI*2),(YDPI*5),'Clipping doesn''t work for text!');

end;

end.

