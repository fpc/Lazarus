unit main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpPDF,
  Forms, Graphics, Dialogs, ComCtrls,
  TAGraph, TASeries, TAFuncSeries;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1FuncSeries1: TFuncSeries;
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    tbSaveAsBMP: TToolButton;
    tbSaveAsPNG: TToolButton;
    tbCopyToClipboard: TToolButton;
    tbSaveAsJPEG: TToolButton;
    tbSaveAsSVG: TToolButton;
    tbSaveAsPDF: TToolButton;
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure FormCreate(Sender: TObject);
    procedure tbCopyToClipboardClick(Sender: TObject);
    procedure tbSaveAsBMPClick(Sender: TObject);
    procedure tbSaveAsJPEGClick(Sender: TObject);
    procedure tbSaveAsPDFClick(Sender: TObject);
    procedure tbSaveAsPNGClick(Sender: TObject);
    procedure tbSaveAsSVGClick(Sender: TObject);
  private
    function GetFileName(const AExt: String): String;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  TADrawerSVG, TADrawerCanvas;

{ TForm1 }

procedure TForm1.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := AX * AX / 2;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  RandSeed := 103489;
  for i := 1 to 10 do
    Chart1BarSeries1.AddXY(i, i * i / 2 + Random(6) + 1 + Random);
  SaveDialog1.InitialDir := ExtractFilePath(Application.ExeName);
end;

function TForm1.GetFileName(const AExt: String): String;
begin
  with SaveDialog1 do begin
    FileName := '';
    DefaultExt := AExt;
    if not Execute then Abort;
    Result := FileName;
  end;
end;

procedure TForm1.tbCopyToClipboardClick(Sender: TObject);
begin
  Chart1.CopyToClipboardBitmap;
end;

procedure TForm1.tbSaveAsBMPClick(Sender: TObject);
begin
  Chart1.SaveToBitmapFile(GetFileName('bmp'));
end;

procedure TForm1.tbSaveAsJPEGClick(Sender: TObject);
begin
  Chart1.SaveToFile(TJPEGImage, GetFileName('jpg'));
end;

{ Saves the chart as a pdf file using the fppdf routines of the fcl.
  Based on code by "paweld", https://www.lazarusforum.de/viewtopic.php?p=156147 }
procedure TForm1.tbSaveAsPDFClick(Sender: TObject);
var
  jpg: TJpegImage;
  pdf: TPDFDocument;
  pdfs: TPDFSection;
  pdfp: TPDFPage;
  idx, m, w, h, px, py: Integer;
  scale: Double;
  ms: TMemoryStream;
begin
  jpg := TJpegImage.Create;
  ms := TMemoryStream.Create;
  pdf := TPDFDocument.Create(nil);
  try
    jpg.SetSize(Chart1.Width, Chart1.Height);
    Chart1.PaintTo(jpg.Canvas, 0, 0);
    jpg.SaveToStream(ms);
    ms.Position := 0;
    pdf.StartDocument;
    pdfs := pdf.Sections.AddSection;
    pdfp := pdf.Pages.AddPage;
    pdfp.Orientation := ppoLandscape;
    pdfp.UnitOfMeasure := uomPixels;
    pdfp.PaperType := ptA4;
    pdfs.AddPage(pdfp);

    idx := pdf.Images.AddJPEGStream(ms, jpg.Width, jpg.Height);

    // border 10 mm, converted to px
    m := trunc(mmToPDF(10));
    // If required, reduce the size of the image on the page
    scale := jpg.Width / (pdfp.Paper.W - 2 * m);
    if (jpg.Height / (pdfp.Paper.H - 2 * m)) > scale then
      scale := jpg.Height / (pdfp.Paper.H - 2 * m);
    if scale < 1 then
      scale := 1;
    w := trunc(jpg.Width / scale);
    h := trunc(jpg.Height / scale);
    // Center on the page
    px := (pdfp.Paper.W - w) div 2;
    py := (pdfp.Paper.H - h) div 2;

    pdfp.DrawImage(px, py, w, h, idx);
    pdf.SaveToFile(GetFileName('pdf'));
  finally
    pdf.Free;
    ms.Free;
    jpg.Free;
  end;
end;

procedure TForm1.tbSaveAsPNGClick(Sender: TObject);
begin
  Chart1.SaveToFile(TPortableNetworkGraphic, GetFileName('png'));
end;

procedure TForm1.tbSaveAsSVGClick(Sender: TObject);
begin
  Chart1.SaveToSVGFile(GetFilename('svg'));
end;
{ or ...
var
  fs: TFileStream;
  id: IChartDrawer;
begin
  fs := TFileStream.Create(GetFileName('svg'), fmCreate);
  try
    id := TSVGDrawer.Create(fs, true);
    with Chart1 do
      Draw(id, Rect(0, 0, Width, Height));
  finally
    fs.Free;
  end;
end;   }

end.

