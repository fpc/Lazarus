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
  FLmfImg.LogUnitsPerInch := Screen.PixelsPerInch * 10;  // Logical units as 1/10 pixel
  FLmfImg.Width := 600*10;
  FLmfImg.Height := 400*10;

  LmfCanvas := TlmfCanvas.Create(FLmfImg);
  try
    // Rectangle
    LmfCanvas.Brush.Color := clSkyBlue;
    LmfCanvas.Rectangle(0, 0, FlmfImg.Width, FLmfImg.Height);

    // Line
    LmfCanvas.Pen.Width := 1*10;
    LmfCanvas.Line(0, 0, FLmfImg.Width, FLmfImg.Height);

    // MoveTo + LineTo
    LmfCanvas.Pen.Width := 3*10;
    LmfCanvas.MoveTo(0, 200*10-1);
    LmfCanvas.LineTo(300*10-1, 0);

    // FillRect
    LmfCanvas.Brush.Color := clBlue;
    LmfCanvas.FillRect(10*10, 100*10, 100*10, 150*10);
    LmfCanvas.Brush.Color := clWhite;
    LmfCanvas.FillRect(Rect(15*10, 110*10, 50*10, 130*10));
    Lmfcanvas.Brush.Color := clSilver;
    LmfCanvas.FillRect(20*10, 115*10, 55*10, 135*10);

    // Frame (border using pen)
    LmfCanvas.Pen.Color := clGray;
    LmfCanvas.Pen.Style := psDot;
    LmfCanvas.Pen.Width := 1*10;
    LmfCanvas.Frame(Rect(20*10, 160*10, 70*10, 180*10));
    LmfCanvas.Frame(25*10, 165*10, 75*10, 185*10);

    // FrameRect (border using brush)
    // Incorrect wmf output (won't fix)
    LmfCanvas.Brush.Color := clRed;
    LmfCanvas.Brush.Style := bsVertical;
    LmfCanvas.FrameRect(Rect(90*10, 165*10, 140*10, 185*10));
    LmfCanvas.FrameRect(95*10, 170*10, 145*10, 190*10);

    //Frame3D
    // Incorrect wmf output (won't fix)
    R := Rect(270*10, 100*10, 310*10, 130*10);
    lmfCanvas.Brush.Style := bsSolid;
    lmfCanvas.Brush.Color := clWhite;
    lmfCanvas.FillRect(R);
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Frame3d(R, cl3DLight, cl3DShadow, 3*10);
    R := Rect(320*10, 100*10, 360*10, 130*10);
    lmfCanvas.Brush.Color := clWhite;
    lmfCanvas.FillRect(R);
    LmfCanvas.Frame3D(R, 3*10, bvLowered);

    // RoundRect
    LmfCanvas.Brush.Color := clYellow;
    LmfCanvas.Pen.Color := clOlive;
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Pen.Width := 3*10;
    LmfCanvas.RoundRect(260*10, 40*10, 340*10, 80*10, 40*10, 40*10);
    LmfCanvas.RoundRect(Rect(270*10, 50*10, 350*10, 90*10), 40*10, 40*10);

    // Draw an alpha-transparent bitmap (32bpp) --> not supported, convert to
    // 24 bpp and apply mask transparency.
    LmfCanvas.Pen.Width := 0;
    LmfCanvas.Brush.Style := bsClear;
    Lmfcanvas.Rectangle(200*10, 5*10, 200*10 + 48*10, 5*10 + 48*10);
    bmp := TPortableNetworkGraphic.Create;
    try
      bmp.PixelFormat := pf32bit;
      bmp.LoadFromFile('../../../../images/general_purpose/Bag_01_48.png');
      LmfCanvas.Draw(200*10, 5*10, bmp);
    finally
      bmp.Free;
    end;

    // Draw a 24-bpp mask-transparent image
    Lmfcanvas.Rectangle(300*10, 5*10, 300*10 + 48*10, 5*10 + 48*10);
    bmp := TBitmap.Create;
    try
//      bmp.LoadFromFile('../../../../images/LazarusForm.bmp');
      bmp.LoadFromFile('Help_02_48.bmp');
      bmp.Transparent := true;
//      LmfCanvas.Draw(3000, 50, bmp);
      LmfCanvas.StretchDraw(Rect(300*10, 5*10, 300*10+48*10, 50+48*10), bmp);
    finally
      bmp.Free;
    end;

    // Draw an icon      // Strange: Powerpoint is not able to display this...
    ico := TIcon.Create;
    try
      ico.LoadFromFile('../../../../images/includefile.ico');
      ico.Transparent := true;
      LmfCanvas.Draw(500*10, 150*10, ico);
    finally
      ico.Free;
    end;

    // Ellipse
    LmfCanvas.Brush.Style := bsHorizontal;
    LmfCanvas.Brush.Color := clYellow;
    LmfCanvas.Pen.Color := clYellow;
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Pen.Width := 2*10;
    LmfCanvas.Ellipse(50*10, 40*10, 250*10, 140*10);
    LmfCanvas.Brush.Style := bsFDiagonal;
    LmfCanvas.Brush.Color := clRed;
    LmfCanvas.Pen.Color := clRed;
    LmfCanvas.Ellipse(Rect(60*10, 55*10, 260*10, 155*10));

    // Arc
    P1 := Point(250*10, 260*10);  // Point on x axis
    P2 := Point(150*10, 0);    // Point on y axis
    LmfCanvas.Pen.Color := clRed;
    LmfCanvas.Pen.Width := 3*10;
    LmfCanvas.Arc(50*10, 210*10, 250*10, 310*10, P1.X, P1.Y, P2.X, P2.Y);
    LmfCanvas.Pen.Color := clBlue;
    LmfCanvas.Arc(50*10, 210*10, 250*10, 310*10, 90*16, 120*16);
    LmfCanvas.Pen.Style := psDash;
    LmfCanvas.Pen.Width := 1*10;
    LmfCanvas.Frame(50*10, 210*10, 250*10, 310*10);

    // Pie / RadialPie
    LmfCanvas.Brush.Color := $CCCCFF;
    LmfCanvas.Brush.Style := bsSolid;
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Pen.Color := clBlack;
    LmfCanvas.Pen.Width := 1*10;
    LmfCanvas.Pie(60*10, 215*10, 240*10, 305*10, P1.X, P1.Y, P2.X, P2.Y);
    LmfCanvas.Brush.Color := $FFCCCC;
    LmfCanvas.RadialPie(60*10, 215*10, 240*10, 305*10, 90*16, 120*16);

    // Chord
    LmfCanvas.Brush.Color := clRed;
    LmfCanvas.Brush.Style := bsSolid;
    LmfCanvas.Pen.Color := clBlack;
    LmfCanvas.Pen.Width := 2*10;
    LmfCanvas.Chord(70*10, 220*10, 230*10, 300*10, P1.X, P1.Y, P2.X, P2.Y);
    LmfCanvas.Brush.Color := clBlue;
    LmfCanvas.Chord(70*10, 220*10, 230*10, 300*10, 90*16, 120*16);

    // ArcTo
    C := Point(190*10, 300*10);
    P1 := C + Point(10*10, 0);
    P2 := C + Point(30*10, -30*10);
    LmfCanvas.Pen.Color := clNavy;
    LmfCanvas.Pen.Width := 2*10;
    LmfCanvas.MoveTo(C.X, C.Y);
    LmfCanvas.ArcTo(C.X-50*10, C.Y-30*10, C.X+50*10, C.Y+30*10, P1.X, P1.Y, P2.X, P2.Y);
    LmfCanvas.Pen.Width := 1*10;
    LmfCanvas.Frame(C.X-50*10, C.Y-30*10, C.X+50*10, C.Y+30*10);
    LmfCanvas.Line(C, P2);

    // AngleArc
    C := Point(440*10, 280*10);
    LmfCanvas.Pen.Color := clRed;
    LmfCanvas.Pen.Width := 2*10;
    LmfCanvas.MoveTo(C.X, C.Y);
    LmfCanvas.AngleArc(C.X, C.Y, 50*10, 45.0, 90.0);
    LmfCanvas.Pen.Width := 1;
    LmfCanvas.Frame(C.X-50*10, C.Y-50*10, C.X+50*10, C.Y+50*10);

    // Polygon
    P[0] := Point(400*10, 0);
    P[1] := Point(450*10, 70*10);
    P[2] := Point(380*10, 40*10);
    P[3] := Point(450*10, 0);
    P[4] := Point(400*10, 70*10);
    P[5] := P[0];  // used only by PolyLine demo
    LmfCanvas.Brush.Style := bsSolid;
    LmfCanvas.Brush.Color := clMoneygreen;
    Lmfcanvas.Pen.Color := clGreen;
    LmfCanvas.Polygon(@P[0], 5, false);
    inc(P[0].Y, 75*10);
    inc(P[1].Y, 75*10);
    inc(P[2].Y, 75*10);
    inc(P[3].Y, 75*10);
    inc(P[4].Y, 75*10);
    inc(P[5].Y, 75*10);
    LmfCanvas.Polygon(@P[0], 5, true);
    inc(P[0].Y, 75*10);
    inc(P[1].Y, 75*10);
    inc(P[2].Y, 75*10);
    inc(P[3].Y, 75*10);
    inc(P[4].Y, 75*10);
    inc(P[5].Y, 75*10);
    LmfCanvas.PolyLine(@P[0], 6);

    // GradientFill
    // Incorrect wmf output (won't fix)
    R := Rect(270*10, 250*10, 350*10, 290*10);
    LmfCanvas.GradientFill(R, clRed, clYellow, gdVertical);
    LmfCanvas.Pen.Width := 1*10;
    LmfCanvas.Pen.Color := clBlack;
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Frame(R);
    R := Rect(270*10, 290*10, 350*10, 330*10);
    LmfCanvas.GradientFill(R, clRed, clYellow, gdHorizontal);
    LmfCanvas.Frame(R);

    // Text
    LmfCanvas.SetBkMode(OPAQUE);
    LmfCanvas.SetBkColor(clYellow);
    LmfCanvas.Font.Height := -10*10;
    LmfCanvas.Font.Color := clOlive;
    LmfCanvas.TextOut(20*10, 20*10, 'Text drawn by TextOut');

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
    LmfCanvas.Font.Height := -16*10;
    LmfCanvas.TextRect(R, 0, 0, 'Text drawn by TextRect' + LineEnding + 'Centered in blue rectangle', ts);

    // Rotated text
    LmfCanvas.SetBkMode(TRANSPARENT);
    LmfCanvas.Font.Height := -12*10;
    LmfCanvas.Font.Color := clRed;
    LmfCanvas.Font.Style := [];
    while LmfCanvas.Font.Orientation < 3600 do
    begin
      LmfCanvas.TextOut(530*10, 80*10, 'abcdef');
      LmfCanvas.Font.Orientation := LmfCanvas.Font.Orientation + 45*10;
    end;
    LmfCanvas.Font.Orientation := 0;

    // Pen styles
    //LmfCanvas.SetBkMode(OPAQUE);
    //LmfCanvas.SetBkColor(clRed);
    LmfCanvas.Pen.Color := clGreen;
    LmfCanvas.Pen.Width := 1*10;
    LmfCanvas.Brush.Style := bsClear;  // Clear the gaps
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Line(10*10, 320*10, 110*10, 320*10);
    LmfCanvas.Pen.Style := psDash;
    LmfCanvas.Line(10*10, 325*10, 110*10, 325*10);
    LmfCanvas.Pen.Style := psDot;
    LmfCanvas.Line(10*10, 330*10, 110*10, 330*10);
    LmfCanvas.Pen.Style := psDashDot;
    LmfCanvas.Line(10*10, 335*10, 110*10, 335*10);
    LmfCanvas.Pen.Style := psDashDotDot;
    LmfCanvas.Line(10*10, 340*10, 110*10, 340*10);
    SetLength(PenPattern, 4);
    penPattern[0] := 1;  // line
    penPattern[1] := 1;  // space
    penPattern[2] := 4;  // line
    penPattern[3] := 4;  // space
    LmfCanvas.Pen.Style := psPattern;     // Incorrect wmf output
    LmfCanvas.Pen.SetPattern(penPattern);
    LmfCanvas.Line(10*10, 345*10, 110*10, 345*10);

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

