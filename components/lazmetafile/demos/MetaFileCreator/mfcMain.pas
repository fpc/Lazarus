unit mfcMain;

interface

uses
 {$IFDEF FPC}
  SysUtils, Classes, Types, //BMPcomn,
  LCLType, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  lmf, lmfCanvas;
 {$ELSE} 
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Types, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;
 {$ENDIF}

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSave: TButton;
    PaintBox: TPaintBox;
    rbEMF: TRadioButton;
    rbWMF: TRadioButton;
    GroupBox1: TGroupBox;
    ListBox: TListBox;
    procedure btnSaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FMetaFile: {$IFDEF FPC}TlmfImage{$ELSE}TMetaFile{$ENDIF};
    procedure CreateMetaFile(AIndex: Integer);
    procedure SaveMetaFile(AIndex: Integer; AEnhanced: Boolean);

    procedure Draw_ArcTo(ACanvas: TCanvas);
    procedure Draw_Bitmap(ACanvas: TCanvas);
    procedure Draw_Chord(ACanvas: TCanvas);
    procedure Draw_CrossHatch(ACanvas: TCanvas);
    procedure Draw_Ellipse(ACanvas: TCanvas);
    procedure Draw_GradientFill(ACanvas: TCanvas);
    procedure Draw_HorHatch_BkColor(ACanvas: TCanvas);
    procedure Draw_Path(ACanvas: TCanvas);
    procedure Draw_Path_PolygonWinding(ACanvas: TCanvas);
    procedure Draw_PenFromStock(ACanvas: TCanvas);
    procedure Draw_PenStyles(ACanvas: TCanvas; ACosmetic: Boolean);
    procedure Draw_Pie(ACanvas: TCanvas);
    procedure Draw_PolyBezier(ACanvas: TCanvas);
    procedure Draw_Polygon(ACanvas: TCanvas);
    procedure Draw_Polygon_Winding(ACanvas: TCanvas);
    procedure Draw_PolyLine(ACanvas: TCanvas);
    procedure Draw_Rectangle(ACanvas: TCanvas);
    procedure Draw_RoundRect(ACanvas: TCanvas);
    procedure Draw_TextOut(ACanvas: TCanvas);
    procedure Draw_TextRect_Align(ACanvas: TCanvas);
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

{$IFNDEF FPC}
procedure DrawBitmapPartWithStretchDIBits(
  TargetCanvas: TCanvas;
  DestRect: TRect;       // Wohin auf dem Ziel-Canvas gezeichnet wird (und wie groß)
  SourceBitmap: TBitmap;
  SourceRect: TRect      // Welcher Ausschnitt aus der Quell-Bitmap kopiert wird
);
var
  BmpInfo: TBitmapInfo;
  BmpHeader: TBitmapInfoHeader;
  BitsSize: DWORD;
  BitsPointer: Pointer;
begin
  if SourceBitmap.Empty then Exit;

  // 1. Header für die gesamte Quell-Bitmap vorbereiten
  ZeroMemory(@BmpHeader, SizeOf(TBitmapInfoHeader));
  BmpHeader.biSize        := SizeOf(TBitmapInfoHeader);
  BmpHeader.biWidth       := SourceBitmap.Width;
  BmpHeader.biHeight      := -SourceBitmap.Height; // Top-Down (verhindert vertikales Spiegeln)
  BmpHeader.biPlanes      := 1;
  BmpHeader.biBitCount    := 32;
  BmpHeader.biCompression := BI_RGB;

  BmpInfo.bmiHeader := BmpHeader;

  // 2. Speicher für alle Pixel der Quell-Bitmap reservieren
  BitsSize := ((SourceBitmap.Width * BmpHeader.biBitCount + 31) and not 31) div 8 * SourceBitmap.Height;
  GetMem(BitsPointer, BitsSize);
  try
    // 3. Gesamte Bitmap in den geräteunabhängigen Speicher (DIB) laden
    if GetDIBits(SourceBitmap.Canvas.Handle, SourceBitmap.Handle, 0, SourceBitmap.Height,
                 BitsPointer, BmpInfo, DIB_RGB_COLORS) > 0 then
    begin
      SetStretchBltMode(TargetCanvas.Handle, HALFTONE);
      SetBrushOrgEx(TargetCanvas.Handle, 0, 0, nil);

      // 4. Nur den gewünschten Ausschnitt (SourceRect) an das Ziel (DestRect) übertragen
      StretchDIBits(
        TargetCanvas.Handle,
        DestRect.Left, DestRect.Top, DestRect.Width, DestRect.Height, // Ziel: Position & Größe
        SourceRect.Left, SourceRect.Top, SourceRect.Width, SourceRect.Height, // Quelle: Ausschnitt
        BitsPointer,
        BmpInfo,
        DIB_RGB_COLORS,
        SRCCOPY
      );
    end;
  finally
    FreeMem(BitsPointer);
  end;
end;
{$ENDIF}
(*
procedure CreateEmfWithPath;
var
  Metafile: TMetafile;
  MetaCanvas: TMetafileCanvas;
  P: array of TPoint; //[0..3] of TPoint;
begin
  Metafile := TMetafile.Create;
  try
    // Dimensionen des Metafiles festlegen
    Metafile.Width := 400;
    Metafile.Height := 400;

    // Canvas erstellen (0 bedeutet, dass der aktuelle Bildschirmkontext als Referenz dient)
    MetaCanvas := TMetafileCanvas.Create(Metafile, 0);
    try
      // 1. Stift und Pinsel für den Pfad definieren
      MetaCanvas.Pen.Color := clBlue;
      MetaCanvas.Pen.Width := 4;
      MetaCanvas.Brush.Color := clYellow;
      MetaCanvas.Brush.Style := bsSolid;

      // 2. Pfad-Aufzeichnung über die Win32-API starten
      // Wir greifen direkt auf das Handle des Canvas zu
      BeginPath(MetaCanvas.Handle);

      // 3. Pfad-Geometrie definieren
      MoveToEx(MetaCanvas.Handle, 50, 50, nil);
      LineTo(MetaCanvas.Handle, 200, 50);

      // Eine Bézierkurve hinzufügen (Startpunkt ist das aktuelle Ende: 200, 50)
      // Parameter: Handle, Kontrollpunkt1, Kontrollpunkt2, Endpunkt
      SetLength(P, 3);
      P[0] := Point(300, 100);
      P[1] := Point(300, 250);
      P[2] := Point(200, 300);
      PolyBezierTo(MetaCanvas.Handle, P[0], 3);

      LineTo(MetaCanvas.Handle, 50, 300);

      // Pfad schließen (zieht eine Linie zurück zum Startpunkt 50, 50)
      CloseFigure(MetaCanvas.Handle);

      // 4. Pfad-Aufzeichnung beenden
      EndPath(MetaCanvas.Handle);

      // 5. Pfad zeichnen (Füllen UND Umranden)
      // Alternativen: StrokePath (nur Linie) oder FillPath (nur Füllung)
      StrokeAndFillPath(MetaCanvas.Handle);

    finally
      // WICHTIG: Der Canvas MUSS freigegeben werden,
      // bevor das Metafile gespeichert oder gezeichnet werden kann!
      MetaCanvas.Free;
    end;

    // 6. Als echte, unendlich skalierbare EMF-Datei speichern
    Metafile.SaveToFile('path.emf');

  finally
    Metafile.Free;
  end;
end;

procedure CreateEmfWithPolyBezierPath;
var
  Metafile: TMetafile;
  MetaCanvas: TMetafileCanvas;
  Points: array[0..3] of TPoint;
begin
  Metafile := TMetafile.Create;
  try
    Metafile.Width := 400;
    Metafile.Height := 400;

    MetaCanvas := TMetafileCanvas.Create(Metafile, 0);
    try
      MetaCanvas.Pen.Color := clRed;
      MetaCanvas.Pen.Width := 4;
      MetaCanvas.Brush.Style := bsClear; // Nur die Kontur zeichnen

      // Punkte für PolyBezier definieren
      Points[0] := Point(50, 200);   // STARTPUNKT (wichtig bei PolyBezier)
      Points[1] := Point(150, 50);   // Kontrollpunkt 1
      Points[2] := Point(250, 350);  // Kontrollpunkt 2
      Points[3] := Point(350, 200);  // ENDPUNKT

      // Pfad-Aufzeichnung starten
      BeginPath(MetaCanvas.Handle);

      // PolyBezier innerhalb des Pfades aufrufen
      // Parameter: Handle, Zeiger auf das erste Element, Anzahl der Punkte
      PolyBezier(MetaCanvas.Handle, Points[0], Length(Points));

      // Pfad-Aufzeichnung beenden
      EndPath(MetaCanvas.Handle);

      // Pfad zeichnen
      StrokePath(MetaCanvas.Handle);

    finally
      MetaCanvas.Free;
    end;

    Metafile.SaveToFile('PolyBezierPath.emf');
  finally
    Metafile.Free;
  end;
end;
*)

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  SaveMetafile(Listbox.ItemIndex, rbEMF.Checked);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FMetaFile.Free;
end;

procedure TForm1.CreateMetaFile(AIndex: Integer);
const
  METAFILE_WIDTH = 1000;
  METAFILE_HEIGHT = 1000;
var
 {$IFDEF FPC}
  canv: TlmfCanvas;
 {$ELSE}
  canv: TMetafileCanvas;
 {$ENDIF}
  R: TRect;
begin
  FMetafile.Free;

  {$IFDEF FPC}
  FMetafile := TlmfImage.Create;
  FMetafile.Width := METAFILE_WIDTH;
  FMetafile.Height := METAFILE_HEIGHT;
 {$ELSE}  
  FMetafile := TMetafile.Create;
  FMetafile.SetSize(METAFILE_WIDTH, METAFILE_HEIGHT);
 {$ENDIF} 

 {$IFDEF FPC}
  canv := TlmfCanvas.Create(FMetaFile);
 {$ELSE} 
  canv := TMetafileCanvas.Create(FMetafile, 0);
 {$ENDIF} 
  try
  //  SetMapMode(canv.Handle, MM_TEXT); //ANISOTROPIC);
    canv.Brush.Color := clRed;
    canv.Brush.Style := bsSolid;
    case AIndex of
      0: Draw_Rectangle(canv);
      1: Draw_RoundRect(canv);
      2: Draw_Ellipse(canv);
      3: Draw_Polygon(canv);
      4: Draw_Polygon_Winding(canv);
      5: Draw_PolyLine(canv);
      6: Draw_PolyBezier(canv);
      7: Draw_CrossHatch(canv);
      8: Draw_HorHatch_BkColor(canv);
      9: Draw_Pie(canv);
     10: Draw_Chord(canv);
     11: Draw_ArcTo(canv);
     12: Draw_TextOut(canv);
     13: Draw_TextRect_Align(canv);
     14: Draw_PenStyles(canv, true);
     15: Draw_PenStyles(canv, false);
     16: Draw_PenFromStock(canv);
     17: Draw_Path(canv);
     18: Draw_Path_PolygonWinding(canv);
     19: Draw_Bitmap(canv);
     20: Draw_GradientFill(canv);
    end;
  finally
    canv.Free;
  end;
  Paintbox.Invalidate;
end;

procedure TForm1.Draw_ArcTo(ACanvas: TCanvas);
var
  C, P1, P2: TPoint;
begin
  C := Point(500, 500);
  P1 := C + Point(100, 0);
  P2 := C + Point(300, 300);
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := clNavy;
  ACanvas.Pen.Width := 10;
  ACanvas.MoveTo(C.X, C.Y);
  ACanvas.ArcTo(C.X-400, C.Y-300, C.X+400, C.Y+300, P1.X, P1.Y, P2.X, P2.Y);
  ACanvas.Pen.Width := 5;
  ACanvas.Pen.Color := clBlue;
  ACanvas.LineTo(C.X, C.Y);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Width := 0;
  ACanvas.Pen.Color := clGray;
  ACanvas.Rectangle(Rect(C.X-400, C.Y-300, C.X+400, C.Y+300));
end;

procedure TForm1.Draw_Bitmap(ACanvas: TCanvas);
var
  bmp: TBitmap;
begin
  ACanvas.Brush.Color := clWhite;
  ACanvas.Rectangle(0, 0, 1000, 1000);
  bmp := TBitmap.Create;
  try
    bmp.SetSize(10, 10);
    bmp.Canvas.Brush.Color := clBlack;
    bmp.Canvas.FillRect(Rect(0, 0, 10, 10));
    bmp.Canvas.Pixels[0, 0] := clSilver;
    bmp.Canvas.Pixels[1, 1] := clRed;
    bmp.Canvas.Pixels[1, 3] := clBlue;
    bmp.Canvas.Pixels[1, 5] := clGreen;
    bmp.Canvas.Pixels[1, 7] := clFuchsia;
    bmp.Canvas.Pixels[1, 9] := clYellow;
    bmp.Canvas.Pixels[2, 2] := clAqua;
    bmp.Canvas.Pixels[3, 1] := clRed;
    bmp.Canvas.Pixels[3, 3] := clSkyBlue;
    bmp.Canvas.Pixels[3, 5] := clGreen;
    bmp.Canvas.Pixels[3, 7] := clFuchsia;
    bmp.Canvas.Pixels[3, 9] := clYellow;
    bmp.Canvas.Pixels[5, 1] := clRed;
    bmp.Canvas.Pixels[5, 3] := clBlue;
    bmp.Canvas.Pixels[5, 5] := clGreen;
    bmp.Canvas.Pixels[5, 7] := clFuchsia;
    bmp.Canvas.Pixels[5, 9] := clYellow;
    bmp.Canvas.Pixels[7, 1] := clRed;
    bmp.Canvas.Pixels[7, 3] := clBlue;
    bmp.Canvas.Pixels[7, 5] := clGreen;
    bmp.Canvas.Pixels[7, 7] := clPurple;
    bmp.Canvas.Pixels[7, 9] := clYellow;
    bmp.Canvas.Pixels[8, 8] := clAqua;
    bmp.Canvas.Pixels[9, 1] := clRed;
    bmp.Canvas.Pixels[9, 3] := clBlue;
    bmp.Canvas.Pixels[9, 5] := clGreen;
    bmp.Canvas.Pixels[9, 7] := clFuchsia;
    bmp.Canvas.Pixels[9, 9] := clSilver;

    //bmp.SaveToFile('test-bitmap.bmp');

    // StretchBLT
    ACanvas.CopyRect(Rect(10, 10, 210, 210),  bmp.Canvas, Rect( 0,  0, 10, 10));
    ACanvas.CopyRect(Rect(220, 10, 420, 210), bmp.Canvas, Rect( 2,  2,  8,  8));
    ACanvas.CopyRect(Rect(430, 10, 630, 210), bmp.Canvas, Rect(-2, -2,  8,  8));
    ACanvas.CopyRect(Rect(640, 10, 840, 210), bmp.Canvas, Rect( 2,  2, 10, 10));
(*
    ACanvas.CopyRect(Rect(10, 10, 210, 210), bmp.Canvas, Rect(0, 0, 10, 10));
    ACanvas.CopyRect(Rect(220, 10, 420, 210), bmp.Canvas, Rect(0, 0, 2, 2)); //8, 8));
    ACanvas.CopyRect(Rect(430, 10, 630, 210), bmp.Canvas, Rect(1, 1, 3, 3)); //-2, -2, 8, 8));
    ACanvas.CopyRect(Rect(640, 10, 840, 210), bmp.Canvas, Rect(2, 2, 4, 4)); //11, 11));
  *)
    // StretchDIBits
   {$IFDEF FPC}
   {$ELSE}
    DrawBitmapPartWithStretchDIBits(ACanvas, Rect(10, 220, 210, 420),  bmp, Rect( 0,  0, 10, 10));
    DrawBitmapPartWithStretchDIBits(ACanvas, Rect(220, 220, 420, 420), bmp, Rect( 2,  2,  8,  8));
    DrawBitmapPartWithStretchDIBits(ACanvas, Rect(430, 220, 630, 420), bmp, Rect(-2, -2,  8,  8));
    DrawBitmapPartWithStretchDIBits(ACanvas, Rect(640, 220, 840, 420), bmp, Rect( 2,  2, 10, 10));
   {$ENDIF}
  (*
    DrawBitmapPartWithStretchDIBits(ACanvas, Rect(10, 220, 210, 420), bmp, Rect(0, 0, 2, 2)); //10, 10));
    DrawBitmapPartWithStretchDIBits(ACanvas, Rect(220, 220, 420, 420), bmp, Rect(2, 2, 4, 4)); //8, 8));
    DrawBitmapPartWithStretchDIBits(ACanvas, Rect(430, 220, 630, 420), bmp, Rect(-2, -2, 8, 8));
    DrawBitmapPartWithStretchDIBits(ACanvas, Rect(640, 220, 840, 420), bmp, Rect(2, 2, 10, 10));
*)
    // StretchBLT (inverted)
    ACanvas.CopyRect(Rect(210, 430, 10, 610), bmp.Canvas, Rect(0, 0, 10, 10));  // flipped horizontally
    ACanvas.CopyRect(Rect(220, 630, 420, 430), bmp.Canvas, Rect(2, 2, 8, 8));   // flipped vertically
    ACanvas.CopyRect(Rect(630, 630, 430, 430), bmp.Canvas, Rect(-2, -2, 8, 8)); // flipped horiz + vert
    //ACanvas.CopyRect(Rect(640, 430, 840, 630), bmp.Canvas, Rect(2, 2, 10, 10));

    // StretchDIBits (inverted)
   {$IFDEF FPC}
   {$ELSE}
    DrawBitmapPartWithStretchDIBits(ACanvas, Rect(210, 640, 10, 840), bmp, Rect(0, 0, 10, 10));   // flipped horiz
    DrawBitmapPartWithStretchDIBits(ACanvas, Rect(220, 840, 420, 640), bmp, Rect(2, 2, 8, 8));    // flipped vert
    DrawBitmapPartWithStretchDIBits(ACanvas, Rect(630, 840, 430, 640), bmp, Rect(-2, -2, 8, 8));  // flipped horiz + vert
    //DrawBitmapPartWithStretchDIBits(ACanvas, Rect(840, 640, 640, 840), bmp, Rect(2, 2, 10, 10));
   {$ENDIF}

  finally
    bmp.Free;
  end;
end;

procedure TForm1.Draw_Chord(ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := clRed;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Chord(100, 100, 900, 900, 1000, 500, 0, 0);
end;

procedure TForm1.Draw_CrossHatch(ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := clRed;
  ACanvas.Brush.Style := bsCross;
  ACanvas.Rectangle(100, 100, 900, 900);
end;

procedure TForm1.Draw_Ellipse(ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := clRed;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Ellipse(100, 100, 900, 900);
end;

procedure TForm1.Draw_GradientFill(ACanvas: TCanvas);
var
 {$IFDEF FPC}
  R: TRect;
  TLColor, BRColor: TColor;
 {$ELSE}
  vertices: Array[0..1] of TTriVertex;
  mesh: Array[0..0] of TGradientRect;
 {$ENDIF}
begin
 {$IFDEF FPC}
  TLColor := clRed;        // Color of top/left corner
  BRColor := clYellow;     // Color of bottom/right corner
  R := Rect(100, 100, 900, 450);
    ACanvas.GradientFill(R, TLColor, BRColor, gdVertical);
  R := Rect(100, 550, 900, 900);
  if ACanvas is TLmfCanvas then
    ACanvas.GradientFill(R, TLColor, BRColor, gdHorizontal);
 {$ELSE}
  with vertices[0] do
  begin
    X := 100;
    Y := 100;
    Red := $FFFF;
    Green := 0;
    Blue := 0;
  end;
  with vertices[1] do
  begin
    x := 900;
    y := 450;
    Red := $FFFF;
    Green := $FFFF;
    Blue := 0;
  end;
  mesh[0].UpperLeft := 0;
  mesh[0].LowerRight := 1;
  ACanvas.Pen.style := psSolid;     // has no effect
  ACanvas.Pen.Color := clBlack;
  GradientFill(ACanvas.Handle, @vertices, 2, @mesh, 1, GRADIENT_FILL_RECT_V);

  vertices[0].Y := 550;
  vertices[1].Y := 900;
  GradientFill(ACanvas.Handle, @vertices, 2, @mesh, 1, GRADIENT_FILL_RECT_H);
 {$ENDIF}
end;

procedure TForm1.Draw_HorHatch_BkColor(ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := clRed;
  ACanvas.Brush.Style := bsHorizontal;
 {$IFDEF FPC}
  if ACanvas is TlmfCanvas then
  begin
    TlmfCanvas(ACanvas).SetBkColor(clYellow);
    TlmfCanvas(ACanvas).SetBkMode(OPAQUE);
  end;
 {$ELSE}
  SetBkColor(ACanvas.Handle, clYellow);
  SetBkMode(ACanvas.Handle, OPAQUE);
 {$ENDIF}
  ACanvas.Rectangle(100, 100, 900, 900);
end;

procedure TForm1.Draw_Path(ACanvas: TCanvas);
var
  P: Array[0..2] of TPoint;
begin
  ACanvas.Brush.Color := clWhite;
  ACanvas.Rectangle(10, 10, 990, 990);

  P[0] := Point(900, 50);
  P[1] := Point(900, 500);
  P[2] := Point(500, 900);
  ACanvas.Brush.Color := clRed;

 {$IFDEF FPC}
  // No paths in lmfCanvas
 {$ELSE}
  BeginPath(ACanvas.Handle);
    MoveToEx(ACanvas.Handle, 50, 50, nil);
    LineTo(ACanvas.Handle, 500, 50);
    PolyBezierTo(ACanvas.Handle, P[0], 3);
    LineTo(ACanvas.Handle, 50, 900);
    CloseFigure(ACanvas.Handle);
  EndPath(ACanvas.Handle);
  StrokeAndFillPath(ACanvas.Handle);
 {$ENDIF}
end;

procedure TForm1.Draw_Path_PolygonWinding(ACanvas: TCanvas);
var
  P: array[0..4] of TPoint;
begin
 {$IFDEF FPC}
  // No paths in lmfCanvas
 {$ELSE}
  ACanvas.Brush.Color := clWhite;
  ACanvas.Rectangle(0, 0, 1000, 1000);

  P[0] := Point(200, 100);
  P[1] := Point(300, 400);
  P[2] := Point( 80, 200);
  P[3] := Point(320, 200);
  P[4] := Point(100, 400);
  ACanvas.Brush.Color := clGray;
  ACanvas.Pen.Color := clBlack;
  SetPolyFillMode(ACanvas.Handle, WINDING);

  BeginPath(ACanvas.Handle);
    ACanvas.Polygon(P);
  EndPath(ACanvas.Handle);
  StrokeAndFillPath(ACanvas.Handle);

  P[0] := Point(500, 100);
  P[1] := Point(600, 400);
  P[2] := Point(380, 200);
  P[3] := Point(620, 200);
  P[4] := Point(400, 400);
  SetPolyFillMode(ACanvas.Handle, ALTERNATE);
  BeginPath(ACanvas.Handle);
    ACanvas.Polygon(P);
  EndPath(ACanvas.Handle);
  StrokeAndFillPath(ACanvas.Handle);
 {$ENDIF}
end;

procedure TForm1.Draw_PenFromStock(ACanvas: TCanvas);
begin
 {$IFDEF FPC}
  // No stock pens in lmfCanvas
 {$ELSE}
  ACanvas.Brush.Color := clWhite;
  ACanvas.Rectangle(0, 0, 1000, 1000);
  ACanvas.Pen.Color := clRed;
  ACanvas.Pen.Width := 20;
  Rectangle(ACanvas.Handle, 100, 100, 450, 900);

  SelectObject(ACanvas.Handle, $80000000 + BLACK_PEN);
  Rectangle(ACanvas.Handle, 550, 100, 900, 900);
 {$ENDIF}
end;

procedure TForm1.Draw_PenStyles(ACanvas: TCanvas; ACosmetic: Boolean);
var
 {$IFDEF FPC}
  pattern: TPenPattern;
 {$ELSE}
  pattern: Array[0..3] of Integer;
  logBrush: TLogBrush;
  newPen, oldPen: hPen;
  penStyle: Integer;
  {$ENDIF}
  i: Integer;
begin
 {$IFDEF FPC}
  ACanvas.Pen.Cosmetic := ACosmetic;
  if ACanvas is TlmfCanvas then TLmfCanvas(ACanvas).SetBkMode(TRANSPARENT);
  ACanvas.Brush.Style := bsClear;      // To do: This should not be necessary to get clear spaces!
  SetLength(pattern, 4);
 {$ELSE}
  SetBkMode(ACanvas.Handle, TRANSPARENT);
 {$ENDIF}

  ACanvas.MoveTo(100, 100);
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 0;
  ACanvas.LineTo(300, 100);
  ACanvas.Pen.Width := 1;
  ACanvas.LineTo(500, 100);
  ACanvas.Pen.Width := 10;
  ACanvas.LineTo(700, 100);
  aCanvas.Pen.Width := 20;
  ACanvas.LineTo(900, 100);

  // Dash
  ACanvas.MoveTo(100, 200);
  ACanvas.Pen.Style := psDash;
  ACanvas.Pen.Width := 0;
  ACanvas.LineTo(300, 200);
  ACanvas.Pen.Width := 1;
  ACanvas.LineTo(500, 200);
  ACanvas.Pen.Width := 10;
  ACanvas.LineTo(700, 200);
  ACanvas.Pen.Width := 20;
  ACanvas.LineTo(900, 200);

  // Dot
  ACanvas.MoveTo(100, 300);
  ACanvas.Pen.Style := psDot;
  ACanvas.Pen.Width := 0;
  ACanvas.LineTo(300, 300);
  ACanvas.Pen.Width := 1;
  ACanvas.LineTo(500, 300);
  ACanvas.Pen.Width := 10;
  ACanvas.LineTo(700, 300);
  ACanvas.Pen.Width := 20;
  ACanvas.LineTo(900, 300);

  // Dash-dot
  ACanvas.MoveTo(100, 400);
  ACanvas.Pen.Style := psDashDot;
  ACanvas.Pen.Width := 0;
  ACanvas.LineTo(300, 400);
  ACanvas.Pen.Width := 1;
  ACanvas.LineTo(500, 400);
  ACanvas.Pen.Width := 10;
  ACanvas.LineTo(700, 400);
  ACanvas.Pen.Width := 20;
  ACanvas.LineTo(900, 400);

  // Dash-dot-dot
  ACanvas.MoveTo(100, 500);
  ACanvas.Pen.Style := psDashDotDot;
  ACanvas.Pen.Width := 0;
  ACanvas.LineTo(300, 500);
  ACanvas.Pen.Width := 1;
  ACanvas.LineTo(500, 500);
  ACanvas.Pen.Width := 10;
  ACanvas.LineTo(700, 500);
  ACanvas.Pen.Width := 20;
  ACanvas.LineTo(900, 500);

  // User pattern
  pattern[0] := 1;
  pattern[1] := 1;
  pattern[2] := 4;
  pattern[3] := 3;
  if not ACosmetic then
    for i := 0 to 3 do
      pattern[i] := pattern[i]*10;

 {$IFDEF FPC}
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psPattern;
  ACanvas.Pen.SetPattern(pattern);
  ACanvas.MoveTo(100, 600);
  ACanvas.LineTo(500, 600);
 {$ELSE}
  logBrush.lbStyle := BS_SOLID;
  logBrush.lbColor := ColorToRGB(clRed);
  logBrush.lbHatch := 0;

  penStyle := PS_USERSTYLE;
  if ACosmetic then
    penStyle := penStyle or PS_COSMETIC
  else
    penStyle := penStyle or PS_GEOMETRIC;
  newPen := ExtCreatePen(
    penStyle,
    1,             // Pen width
    logBrush,      // Defines how pen strokes are filled
    4,             // Number of pattern elements (stroke-gap-stroke-gap-...)
    @pattern[0]    // Pointer to length of first pattern element
  );

  oldPen := SelectObject(ACanvas.Handle, newPen);
  try
    ACanvas.MoveTo(100, 600);
    ACanvas.LineTo(500, 600);
  finally
    SelectObject(ACanvas.Handle, oldPen);
    DeleteObject(newPen);
  end;
 {$ENDIF}
end;

procedure TForm1.Draw_Pie(ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := clRed;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pie(100, 100, 900, 900, 1000, 500, 0, 1000);
end;

procedure TForm1.Draw_PolyBezier(ACanvas: TCanvas);
var
  P: Array[0..6] of TPoint;
begin
  ACanvas.Pen.Color := clRed;
  ACanvas.Pen.Width := 20;
  P[0] := Point(100, 100);
  P[1] := Point(500, 100);
  P[2] := Point(200, 600);
  P[3] := Point(450, 900);
  P[4] := Point(900, 900);
  P[5] := Point(900, 900);
  P[6] := Point(900, 500);
  ACanvas.PolyBezier(P);
end;

procedure TForm1.Draw_Polygon(ACanvas: TCanvas);
var
  P: array[0..2] of TPoint;
begin
  ACanvas.Brush.Color := clWhite;
  ACanvas.Pen.Width := 50;
  ACanvas.Pen.Color := clRed;
  P[0] := Point(100, 100);
  P[1] := Point(900, 100);
  P[2] := Point(500, 900);
  ACanvas.Polygon(P);
end;

procedure TForm1.Draw_Polygon_Winding(ACanvas: TCanvas);
var
  P: Array[0..4] of TPoint;
begin
  ACanvas.Brush.Color := clRed;
  ACanvas.Brush.Style := bsSolid;
  P[0] := Point(200, 250);
  P[1] := Point(500, 800);
  P[2] := Point(  0, 500);
  P[3] := Point(500, 300);
  P[4] := Point(200, 850);
 {$IFDEF FPC}
  ACanvas.Polygon(P, true);
 {$ELSE}
  SetPolyFillMode(ACanvas.Handle, WINDING);
  ACanvas.Polygon(P);
 {$ENDIF}

  P[0] := Point(600, 250);
  P[1] := Point(900, 800);
  P[2] := Point(400, 500);
  P[3] := Point(900, 300);
  P[4] := Point(600, 850);
 {$IFDEF FPC}
  ACanvas.Polygon(P, false);
 {$ELSE}
  SetPolyFillMode(ACanvas.Handle, ALTERNATE);
  ACanvas.Polygon(P);
 {$ENDIF}
end;

procedure TForm1.Draw_PolyLine(ACanvas: TCanvas);
var
  P: Array[0..2] of TPoint;
begin
  ACanvas.Pen.Color := clRed;
  ACanvas.Pen.Width := 20;
  P[0] := Point(100, 100);
  P[1] := Point(900, 100);
  P[2] := Point(500, 900);
  ACanvas.PolyLine(P);
end;

procedure TForm1.Draw_Rectangle(ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := clRed;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Rectangle(100, 100, 900, 900);
end;

procedure TForm1.Draw_RoundRect(ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := clRed;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.RoundRect(100, 100, 900, 900, 200, 200);
end;

procedure TForm1.Draw_TextOut(ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := clYellow;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Rectangle(10, 10, 990, 990);
  ACanvas.MoveTo(100, 0);  ACanvas.LineTo(100, 1000);
  ACanvas.Moveto(0, 500);  ACanvas.LineTo(1000, 500);
  ACanvas.Brush.Color := clRed;
  ACanvas.Ellipse(100-10, 500-10, 100+10, 500+10);
 {$IFDEF FPC}
  if ACanvas is TlmfCanvas then TLmfCanvas(ACanvas).SetBkMode(TRANSPARENT);
 {$ELSE}
  SetBkMode(ACanvas.Handle, TRANSPARENT);
 {$ENDIF}
  ACanvas.Font.Name := 'Tahoma';
  ACanvas.Font.Size := 72;
  ACanvas.TextOut(100, 500, 'Abc äöüß αβγ АБВГ');
end;

procedure TForm1.Draw_TextRect_Align(ACanvas: TCanvas);
var
  R: TRect;
  txt: String;
 {$IFDEF FPC}
  ts: TTextStyle;
 {$ENDIF}
begin
  R := Rect(100, 100, 900, 900);
  ACanvas.Pen.Color := clGray;
  ACanvas.Brush.Color := clWhite;
  ACanvas.Rectangle(0, 0, 1000, 1000);
  ACanvas.Pen.Color := clSilver;
  ACanvas.MoveTo(100, 0);  ACanvas.LineTo(100, 1000);
  ACanvas.MoveTo(500, 0);  ACanvas.LineTo(500, 1000);
  ACanvas.MoveTo(900, 0);  ACanvas.LineTo(900, 1000);
  ACanvas.MoveTo(0, 100);  ACanvas.LineTo(1000, 100);
  ACanvas.MoveTo(0, 500);  ACanvas.LineTo(1000, 500);
  ACanvas.MoveTo(0, 900);  ACanvas.LineTo(1000, 900);
  ACanvas.Font.Name := 'Noto Serif';
  ACanvas.Font.Size := 28;
 {$IFDEF FPC}
  if ACanvas is TlmfCanvas then TLmfCanvas(ACanvas).SetBkMode(TRANSPARENT);
  ts := ACanvas.Textstyle;

  // Vertically at top
  ts.Layout := tlTop;
  ts.Alignment := taLeftJustify;
  ACanvas.TextRect(R, R.Left, R.Top, 'Left/top', ts);

  ts.Alignment := taCenter;
  ACanvas.TextRect(R, R.Left, R.Top, 'Center/top', ts);

  ts.Alignment := taRightJustify;
  ACanvas.TextRect(R, R.Left, R.Top, 'Right/top', ts);

  // Vertically centered
  ts.Layout := tlCenter;
  ts.Alignment := taLeftJustify;
  ACanvas.TextRect(R, R.Left, R.Top, 'Left/center', ts);

  ts.Alignment := taCenter;
  ACanvas.TextRect(R, R.Left, R.Top, 'Center/center', ts);

  ts.Alignment := taRightJustify;
  ACanvas.TextRect(R, R.Left, R.Top, 'Right/center', ts);

  // Vertically at bottom
  ts.Layout := tlBottom;
  ts.Alignment := taLeftJustify;
  ACanvas.TextRect(R, R.Left, R.Top, 'Left/bottom', ts);

  ts.Alignment := taCenter;
  ACanvas.TextRect(R, R.Left, R.Top, 'Center/bottom', ts);

  ts.Alignment := taRightJustify;
  ACanvas.TextRect(R, R.Left, R.Top, 'Right/bottom', ts);
 {$ELSE}
  SetBkMode(ACanvas.Handle, TRANSPARENT);
  txt := 'Left/top';      ACanvas.TextRect(R, txt, [tfSingleLine, tfLeft,   tfTop]);
  txt := 'Center/top';    ACanvas.TextRect(R, txt, [tfSingleLine, tfCenter, tfTop]);
  txt := 'Right/top';     ACanvas.TextRect(R, txt, [tfSingleLine, tfRight,  tfTop]);
  txt := 'Left/center';   ACanvas.TextRect(R, txt, [tfSingleLine, tfLeft,   tfVerticalCenter]);
  txt := 'Center/center'; ACanvas.TextRect(R, txt, [tfSingleLine, tfCenter, tfVerticalCenter]);
  txt := 'Right/center';  ACanvas.TextRect(R, txt, [tfSingleLine, tfRight,  tfVerticalCenter]);
  txt := 'Left/bottom';   ACanvas.TextRect(R, txt, [tfSingleLine, tfLeft,   tfBottom]);
  txt := 'Center/bottom'; ACanvas.TextRect(R, txt, [tfSingleLine, tfCenter, tfBottom]);
  txt := 'Right/bottom';  ACanvas.TextRect(R, txt, [tfSingleLine, tfRight,  tfBottom]);
 {$ENDIF}
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Listbox.ItemIndex := 0;
  CreateMetaFile(0);
end;

procedure TForm1.ListBoxClick(Sender: TObject);
begin
  CreateMetaFile(Listbox.ItemIndex);
end;

procedure TForm1.PaintBoxPaint(Sender: TObject);
begin
  if FMetafile <> nil then
    Paintbox.Canvas.StretchDraw(Rect(0, 0, Paintbox.Width, Paintbox.Height), FMetaFile);
end;

procedure TForm1.SaveMetaFile(AIndex: Integer; AEnhanced: Boolean);
const
  EXT: array[boolean] of String = ('wmf', 'emf');
var
  fn: String;
begin
  if Assigned(FMetaFile) then
   {$IFDEF FPC}
    fn := Format('Laz-[%s].%s', [Listbox.Items[AIndex], EXT[AEnhanced]]);
    FMetaFile.SaveToLmfFile(fn);
   {$ELSE}
    fn := Format('Delphi-[%s].%s', [Listbox.Items[AIndex], EXT[AEnhanced]]);
    FMetafile.SaveToFile(fn);
   {$ENDIF}
end;


end.
