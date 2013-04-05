unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, PrintersDlgs, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, CairoPrinter;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    btn24217: TButton;
    btn19435: TButton;
    btnPrintAll: TButton;
    btnOther: TButton;
    chkTests: TCheckGroup;
    PrintDialog1: TPrintDialog;
    procedure btn19435Click(Sender: TObject);
    procedure btnOtherClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btn24217Click(Sender: TObject);
    procedure chkTestsItemClick(Sender: TObject; Index: integer);
    procedure FormPaint(Sender: TObject);
  private
    procedure Draw19435(cnv: TCanvas; XDPI,YDPI: Integer);
    procedure Draw24217(cnv: TCanvas; XDPI,YDPI: Integer);
    procedure DrawOther(cnv: TCanvas; XDPI,YDPI: Integer);
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
      Draw24217(Canvas, XDPI, YDPI);

    Printer.EndDoc;
  end;
end;

procedure TForm1.btn19435Click(Sender: TObject);
var
  CairoPrinter: TCairoFilePrinter;
begin
  CairoPrinter := TCairoFilePrinter.create;
  //CairoPrinter.CairoBackend:=cbPS;
  CairoPrinter.CairoBackend:=cbPDF;
  CairoPrinter.FileName:='19435';
  CairoPrinter.BeginDoc;
  with CairoPrinter do
    Draw19435(Canvas, XDPI, YDPI);
  CairoPrinter.EndDoc;
  CairoPrinter.Free;
end;

procedure TForm1.btnOtherClick(Sender: TObject);
var
  CairoPrinter: TCairoFilePrinter;
begin
  CairoPrinter := TCairoFilePrinter.create;
  //CairoPrinter.CairoBackend:=cbPS;
  CairoPrinter.CairoBackend:=cbPDF;
  CairoPrinter.FileName:='other';
  CairoPrinter.BeginDoc;
  with CairoPrinter do
    DrawOther(Canvas, XDPI, YDPI);
  CairoPrinter.EndDoc;
  CairoPrinter.Free;
end;

procedure TForm1.btn24217Click(Sender: TObject);
var
  CairoPrinter: TCairoFilePrinter;
begin
  CairoPrinter := TCairoFilePrinter.create;
  //CairoPrinter.CairoBackend:=cbPS;
  CairoPrinter.CairoBackend:=cbPDF;
  CairoPrinter.FileName:='salida';
  CairoPrinter.BeginDoc;
  with CairoPrinter do
    Draw24217(Canvas, XDPI, YDPI);
  CairoPrinter.EndDoc;
  CairoPrinter.Free;
end;

procedure TForm1.chkTestsItemClick(Sender: TObject; Index: integer);
begin
  Invalidate;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  ResX,ResY: Integer;
begin
  ResX := Screen.PixelsPerInch;
  ResY := ResX;
  if chkTests.Checked[0] then Draw24217(Canvas, ResX, ResY);
  if chkTests.Checked[1] then Draw19435(Canvas, ResX, ResY);
  if chkTests.Checked[2] then DrawOther(Canvas, ResX, ResY);
end;

procedure TForm1.Draw19435(cnv: TCanvas; XDPI, YDPI: Integer);
const
  prefix = 'Arabic: ';
var
  sz: TSize;
  x,y: Integer;
  fx: Integer;
begin
  with cnv do
  begin
    Font.Color := clDefault;
    Font.Name := 'Sans';
    Font.Size := 24;
    cnv.Brush.Style := bsClear;
    x := Round(XDPI);
    y := Round(YDPI);
    sz := TextExtent(PREFIX);
    fx := Round(250*XDPI/300); // for demostration purposes this should make both
                               // texts overlap both in screen and in printer
    TextOut(x, y, 'English with font ');

    // alternative 1, depend on font size, for Size=24 both texts overlaps
    inc(y, sz.cy);
    TextOut(x,       y, PREFIX);
    TextOut(x+fx,    y, 'اللغة العربية');

    // alternative 2, measure prefix
    inc(y, sz.cy);
    TextOut(x,       y, PREFIX);
    TextOut(x+sz.cx, y, 'اللغة العربية');

    // alternative 3, simpler
    inc(y, sz.cy);
    Textout(x,       y, PREFIX + 'اللغة العربية');
  end;
end;

procedure TForm1.Draw24217(cnv: TCanvas; XDPI, YDPI: Integer);
var
  sz:TSize;
  r:tRect;
  i:integer;
  kx,ky:double;
begin

  cnv.Font.Color := clDefault;
  cnv.Pen.Color := clBlack;
  cnv.Font.Size:=24;
  cnv.Brush.style := bsClear;

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

procedure TForm1.DrawOther(cnv: TCanvas; XDPI, YDPI: Integer);
const
  CTEXT='Hola';
  Par =
    '1.111' + LineEnding+
    '0.000' + LineEnding+
     '.101' + LineEnding+
    'M.Unl' + LineEnding+
    '.UnlM' + LineEnding+
    '01.' + LineEnding+
    '1.0' + LineEnding+
    '.01';
var
  R: TRect;
  sz: TSize;
  style: TTextStyle;
begin

  R := Rect(XDPI, YDPI*2, XDPI*3, round(YDPI*2.5));

  cnv.Font.Name := 'Arial';
  cnv.Font.Size := 40;
  cnv.Font.Color := clBlue;
  cnv.Pen.Color := RGBToColor($AA, $CC, $FF);
  cnv.Brush.Style := bsClear;
  cnv.TextRect(R, R.Left, R.Top, CTEXT);
  cnv.Rectangle(R);

  OffsetRect(R, 5, 5);
  cnv.Pen.Color := RGBToColor($FF, $AA, $AA);
  cnv.Font.Color := clRed;
  cnv.TextOut(R.left, R.Top, CTEXT);
  cnv.Rectangle(R);

  OffsetRect(R, -5, Round(YDPI*0.5));
  sz := cnv.TextExtent('Line1');
  cnv.Font.color := clDefault;
  cnv.TextOut(R.Left, R.Top, 'Line1'); OffsetRect(R, 0, sz.cy);
  cnv.TextOut(R.Left, R.Top, 'Line2'); OffsetRect(R, 0, sz.cy);
  cnv.TextOut(R.Left, R.Top, 'Line3'); OffsetRect(R, 0, sz.cy);

  R := Rect(XDPI*4, YDPI*2, Round(XDPI*6), round(YDPI*6));
  cnv.Font.Name := 'Sans';
  cnv.Font.Size := 40;
  cnv.Font.Color := clGreen;
  cnv.Brush.Style := bsClear;
  cnv.Pen.Color := clSilver;
  style := cnv.TextStyle;
  style.SingleLine := false;
  style.Alignment := taRightJustify;
  cnv.TextStyle := style;
  cnv.TextRect(R, R.Left, R.Top,  Par);
  cnv.Rectangle(R);
end;

end.

