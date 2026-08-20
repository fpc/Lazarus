unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLType, lmf;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
  private
    FLmfImg: TLmfImage;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  LmfCanvas: TlmfCanvas;
  P: array[0..5] of TPoint;
  P1, P2, C: TPoint;
  R: TRect;
  ts: TTextStyle;
  penPattern: TPenPattern = nil;
  bmp: TCustomBitmap;
  ico: TIcon;
begin
  Width := 610;
  Height := 410;

  FLmfImg := TlmfImage.Create;
  FLmfImg.Width := 6000;  // by default, these are twips
  FLmfImg.Height := 4000;

  LmfCanvas := TlmfCanvas.Create(FLmfImg);
  try
    // Rectangle
    LmfCanvas.Brush.Color := clSkyBlue;
    LmfCanvas.Rectangle(0, 0, FlmfImg.Width, FLmfImg.Height);

    // Line
    LmfCanvas.Pen.Width := 1;
    LmfCanvas.Line(0, 0, FLmfImg.Width, FLmfImg.Height);

    // MoveTo + LineTo
    LmfCanvas.Pen.Width := 30;
    LmfCanvas.MoveTo(0, 1999);
    LmfCanvas.LineTo(2999, 0);

    // FillRect
    LmfCanvas.Brush.Color := clBlue;
    LmfCanvas.FillRect(100, 1000, 1000, 1500);
    LmfCanvas.Brush.Color := clWhite;
    LmfCanvas.FillRect(Rect(150, 1100, 500, 1300));
    Lmfcanvas.Brush.Color := clSilver;
    LmfCanvas.FillRect(200, 1150, 550, 1350);

    // Frame (border using pen)
    LmfCanvas.Pen.Color := clGray;
    LmfCanvas.Pen.Style := psDot;
    LmfCanvas.Pen.Width := 10;
    LmfCanvas.Frame(Rect(200, 1600, 700, 1800));
    LmfCanvas.Frame(250, 1650, 750, 1850);

    // FrameRect (border using brush)
    // Incorrect wmf output (won't fix)
    LmfCanvas.Brush.Color := clRed;
    LmfCanvas.Brush.Style := bsVertical;
    LmfCanvas.FrameRect(Rect(900, 1650, 1400, 1850));
    LmfCanvas.FrameRect(950, 1700, 1450, 1900);

    //Frame3D
    // Incorrect wmf output (won't fix)
    R := Rect(2700, 1000, 3100, 1300);
    lmfCanvas.Brush.Style := bsSolid;
    lmfCanvas.Brush.Color := clWhite;
    lmfCanvas.FillRect(R);
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Frame3d(R, cl3DLight, cl3DShadow, 3);
    R := Rect(3200, 1000, 3600, 1300);
    lmfCanvas.Brush.Color := clWhite;
    lmfCanvas.FillRect(R);
    LmfCanvas.Frame3D(R, 30, bvLowered);

    // RoundRect
    LmfCanvas.Brush.Color := clYellow;
    LmfCanvas.Pen.Color := clOlive;
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Pen.Width := 30;
    LmfCanvas.RoundRect(2600, 400, 3400, 800, 400, 400);
    LmfCanvas.RoundRect(Rect(2700, 500, 3500, 900), 400, 400);

    // Draw an alpha-transparent bitmap (32bpp) --> not supported, convert to
    // 24 bpp and apply mask transparency.
    LmfCanvas.Pen.Width := 0;
    LmfCanvas.Brush.Style := bsClear;
    Lmfcanvas.Rectangle(2000, 50, 2000 + 480, 50 + 480);
    bmp := TPortableNetworkGraphic.Create;
    try
      bmp.PixelFormat := pf32bit;
      bmp.LoadFromFile('../../../../images/general_purpose/Bag_01_48.png');
      LmfCanvas.Draw(2000, 50, bmp);
    finally
      bmp.Free;
    end;

    // Draw a 24-bpp mask-transparent image
    Lmfcanvas.Rectangle(3000, 50, 3000 + 480, 50 + 480);
    bmp := TBitmap.Create;
    try
//      bmp.LoadFromFile('../../../../images/LazarusForm.bmp');
      bmp.LoadFromFile('Help_02_48.bmp');
      bmp.Transparent := true;
//      LmfCanvas.Draw(3000, 50, bmp);
      LmfCanvas.StretchDraw(Rect(3000, 50, 3000+480, 50+480), bmp);
    finally
      bmp.Free;
    end;

    // Draw an icon      // Strange: Powerpoint is not able to display this...
    ico := TIcon.Create;
    try
      ico.LoadFromFile('../../../../images/includefile.ico');
      ico.Transparent := true;
      LmfCanvas.Draw(5000, 1500, ico);
    finally
      ico.Free;
    end;

    // Ellipse
    LmfCanvas.Brush.Style := bsHorizontal;
    LmfCanvas.Brush.Color := clYellow;
    LmfCanvas.Pen.Color := clYellow;
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Pen.Width := 20;
    LmfCanvas.Ellipse(500, 400, 2500, 1400);
    LmfCanvas.Brush.Style := bsFDiagonal;
    LmfCanvas.Brush.Color := clRed;
    LmfCanvas.Pen.Color := clRed;
    LmfCanvas.Ellipse(Rect(600, 550, 2600, 1550));

    // Arc
    P1 := Point(2500, 2600);  // Point on x axis
    P2 := Point(1500, 0);    // Point on y axis
    LmfCanvas.Pen.Color := clRed;
    LmfCanvas.Pen.Width := 30;
    LmfCanvas.Arc(500, 2100, 2500, 3100, P1.X, P1.Y, P2.X, P2.Y);
    LmfCanvas.Pen.Color := clBlue;
    LmfCanvas.Arc(500, 2100, 2500, 3100, 90*16, 120*16);
    LmfCanvas.Pen.Style := psDash;
    LmfCanvas.Pen.Width := 10;
    LmfCanvas.Frame(500, 2100, 2500, 3100);

    // Pie / RadialPie
    LmfCanvas.Brush.Color := $CCCCFF;
    LmfCanvas.Brush.Style := bsSolid;
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Pen.Color := clBlack;
    LmfCanvas.Pen.Width := 10;
    LmfCanvas.Pie(600, 2150, 2400, 3050, P1.X, P1.Y, P2.X, P2.Y);
    LmfCanvas.Brush.Color := $FFCCCC;
    LmfCanvas.RadialPie(600, 2150, 2400, 3050, 90*16, 120*16);

    // Chord
    LmfCanvas.Brush.Color := clRed;
    LmfCanvas.Brush.Style := bsSolid;
    LmfCanvas.Pen.Color := clBlack;
    LmfCanvas.Pen.Width := 20;
    LmfCanvas.Chord(700, 2200, 2300, 3000, P1.X, P1.Y, P2.X, P2.Y);
    LmfCanvas.Brush.Color := clBlue;
    LmfCanvas.Chord(700, 2200, 2300, 3000, 90*16, 120*16);

    // ArcTo
    C := Point(1900, 3000);
    P1 := C + Point(100, 0);
    P2 := C + Point(300, -300);
    LmfCanvas.Pen.Color := clNavy;
    LmfCanvas.Pen.Width := 20;
    LmfCanvas.MoveTo(C.X, C.Y);
    LmfCanvas.ArcTo(C.X-500, C.Y-300, C.X+500, C.Y+300, P1.X, P1.Y, P2.X, P2.Y);
    LmfCanvas.Pen.Width := 10;
    LmfCanvas.Frame(C.X-500, C.Y-300, C.X+500, C.Y+300);
    LmfCanvas.Line(C, P2);

    // AngleArc
    C := Point(4400, 2800);
    LmfCanvas.Pen.Color := clRed;
    LmfCanvas.Pen.Width := 20;
    LmfCanvas.MoveTo(C.X, C.Y);
    LmfCanvas.AngleArc(C.X, C.Y, 500, 45.0, 90.0);
    LmfCanvas.Pen.Width := 1;
    LmfCanvas.Frame(C.X-500, C.Y-500, C.X+500, C.Y+500);

    // Polygon
    P[0] := Point(4000, 0);
    P[1] := Point(4500, 700);
    P[2] := Point(3800, 400);
    P[3] := Point(4500, 0);
    P[4] := Point(4000, 700);
    P[5] := P[0];  // used only by PolyLine demo
    LmfCanvas.Brush.Style := bsSolid;
    LmfCanvas.Brush.Color := clMoneygreen;
    Lmfcanvas.Pen.Color := clGreen;
    LmfCanvas.Polygon(@P[0], 5, false);
    inc(P[0].Y, 750);
    inc(P[1].Y, 750);
    inc(P[2].Y, 750);
    inc(P[3].Y, 750);
    inc(P[4].Y, 750);
    inc(P[5].Y, 750);
    LmfCanvas.Polygon(@P[0], 5, true);
    inc(P[0].Y, 750);
    inc(P[1].Y, 750);
    inc(P[2].Y, 750);
    inc(P[3].Y, 750);
    inc(P[4].Y, 750);
    inc(P[5].Y, 750);
    LmfCanvas.PolyLine(@P[0], 6);

    // GradientFill
    // Incorrect wmf output (won't fix)
    R := Rect(2700, 2500, 3500, 2900);
    LmfCanvas.GradientFill(R, clRed, clYellow, gdVertical);
    LmfCanvas.Pen.Width := 10;
    LmfCanvas.Pen.Color := clBlack;
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Frame(R);
    R := Rect(2700, 2900, 3500, 3300);
    LmfCanvas.GradientFill(R, clRed, clYellow, gdHorizontal);
    LmfCanvas.Frame(R);

    // Text
    LmfCanvas.SetBkMode(OPAQUE);
    LmfCanvas.SetBkColor(clYellow);
    LmfCanvas.Font.Height := -100;
    LmfCanvas.Font.Color := clOlive;
    LmfCanvas.TextOut(200, 200, 'Text drawn by TextOut');

    // TextRect
    R := Rect(0, 0, FLmfImg.Width, FLmfImg.Height);
    ts := LmfCanvas.TextStyle;
    ts.Alignment := taCenter;
    ts.Layout := tlCenter;
    ts.SingleLine := false;
    ts.Clipping := false;
    LmfCanvas.SetBkMode(TRANSPARENT);
    LmfCanvas.Font.Color := clBlue;
    LmfCanvas.Font.Style := [fsBold, fsItalic];
    LmfCanvas.Font.Height := -160;
    LmfCanvas.TextRect(R, 0, 0, 'Text drawn by TextRect' + LineEnding + 'Centered in blue rectangle', ts);

    // Rotated text
    LmfCanvas.SetBkMode(TRANSPARENT);
    LmfCanvas.Font.Height := -120;
    LmfCanvas.Font.Color := clRed;
    LmfCanvas.Font.Style := [];
    while LmfCanvas.Font.Orientation < 3600 do
    begin
      LmfCanvas.TextOut(5300, 800, 'abcdef');
      LmfCanvas.Font.Orientation := LmfCanvas.Font.Orientation + 45*10;
    end;
    LmfCanvas.Font.Orientation := 0;

    // Pen styles
    //LmfCanvas.SetBkMode(OPAQUE);
    //LmfCanvas.SetBkColor(clRed);
    LmfCanvas.Pen.Color := clGreen;
    LmfCanvas.Pen.Width := 10;
    LmfCanvas.Brush.Style := bsClear;  // Clear the gaps
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Line(100, 3200, 1100, 3200);
    LmfCanvas.Pen.Style := psDash;
    LmfCanvas.Line(100, 3250, 1100, 3250);
    LmfCanvas.Pen.Style := psDot;
    LmfCanvas.Line(100, 3300, 1100, 3300);
    LmfCanvas.Pen.Style := psDashDot;
    LmfCanvas.Line(100, 3350, 1100, 3350);
    LmfCanvas.Pen.Style := psDashDotDot;
    LmfCanvas.Line(100, 3400, 1100, 3400);
    SetLength(PenPattern, 4);
    penPattern[0] := 1;  // line
    penPattern[1] := 1;  // space
    penPattern[2] := 4;  // line
    penPattern[3] := 4;  // space
    LmfCanvas.Pen.Style := psPattern;     // Incorrect wmf output
    LmfCanvas.Pen.SetPattern(penPattern);
    LmfCanvas.Line(100, 3450, 1100, 3450);

  finally
    LmfCanvas.Free;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FLmfImg.Free;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F2:
      FLmfImg.SaveToLMFFile('test.wmf');
  end;
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  if Assigned(FLmfImg) then
  begin
    // Paint metafile image on the canvas of the form
    // !!! Activate one of the two next lines !!!
//    Canvas.Draw(10, 10, FLmfImg);
    Canvas.StretchDraw(Rect(5, 5, ClientWidth-5, ClientHeight-5), FLmfImg);
  end;
end;

end.

