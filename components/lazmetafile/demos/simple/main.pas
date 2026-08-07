unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, lmf;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
  P: array[0..4] of TPoint;
  P1, P2, C: TPoint;
  R: TRect;
  ts: TTextStyle;
begin
  Width := 520;
  Height := 370;

  FLmfImg := TlmfImage.Create;
  FLmfImg.Width := 500;
  FLmfImg.Height := 350;

  LmfCanvas := TlmfCanvas.Create(FLmfImg);
  try
    // Rectangle
    LmfCanvas.Brush.Color := clSkyBlue;
    LmfCanvas.Rectangle(0, 0, 500, 350);

    // Line
    LmfCanvas.Pen.Width := 1;
    LmfCanvas.Line(0, 0, 500, 350);

    // MoveTo + LineTo
    LmfCanvas.Pen.Width := 3;
    LmfCanvas.MoveTo(0, 199);
    LmfCanvas.LineTo(299, 0);

    // FillRect
    LmfCanvas.Brush.Color := clBlue;
    LmfCanvas.FillRect(10, 100, 100, 150);
    LmfCanvas.Brush.Color := clWhite;
    LmfCanvas.FillRect(Rect(15, 110, 50, 130));
    Lmfcanvas.Brush.Color := clSilver;
    LmfCanvas.FillRect(20, 115, 55, 135);

    // Frame (border using pen)
    LmfCanvas.Pen.Color := clGray;
    LmfCanvas.Pen.Style := psDot;
    LmfCanvas.Pen.Width := 1;
    LmfCanvas.Frame(Rect(20, 160, 80, 180));
    LmfCanvas.Frame(25, 165, 85, 185);

    // FrameRect (border using brush)
    LmfCanvas.Brush.Color := clRed;
    LmfCanvas.FrameRect(Rect(90, 165, 150, 185));
    LmfCanvas.FrameRect(95, 170, 155, 190);

    // RoundRect
    LmfCanvas.Brush.Color := clYellow;
    LmfCanvas.Pen.Color := clOlive;
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Pen.Width := 3;
    LmfCanvas.RoundRect(260, 40, 340, 80, 40, 40);
    LmfCanvas.RoundRect(Rect(265, 45, 345, 85), 40, 40);

    // Ellipse
    LmfCanvas.Brush.Style := bsHorizontal;
    LmfCanvas.Brush.Color := clRed;
    LmfCanvas.Pen.Color := clYellow;
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Pen.Width := 2;
    LmfCanvas.Ellipse(50, 50, 250, 150);
    //LmfCanvas.Frame(50, 50, 250, 150);
    LmfCanvas.Ellipse(Rect(55, 55, 255, 155));

    // Arc
    P1 := Point(250, 260);  // Point on x axis
    P2 := Point(150, 0);    // Point on y axis
    LmfCanvas.Pen.Color := clRed;
    LmfCanvas.Pen.Width := 3;
    LmfCanvas.Arc(50, 210, 250, 310, P1.X, P1.Y, P2.X, P2.Y);
    LmfCanvas.Pen.Color := clBlue;
    LmfCanvas.Arc(50, 210, 250, 310, 90*16, 120*16);
    LmfCanvas.Pen.Style := psDash;
    LmfCanvas.Pen.Width := 1;
    LmfCanvas.Frame(50, 210, 250, 310);

    // Pie / RadialPie
    LmfCanvas.Brush.Color := $CCCCFF;
    LmfCanvas.Brush.Style := bsSolid;
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Pen.Color := clBlack;
    LmfCanvas.Pen.Width := 1;
    LmfCanvas.Pie(60, 215, 240, 305, P1.X, P1.Y, P2.X, P2.Y);
    LmfCanvas.Brush.Color := $FFCCCC;
    LmfCanvas.RadialPie(60, 215, 240, 305, 90*16, 120*16);

    // Chord
    LmfCanvas.Brush.Color := clRed;
    LmfCanvas.Brush.Style := bsSolid;
    LmfCanvas.Pen.Color := clBlack;
    LmfCanvas.Pen.Width := 2;
    LmfCanvas.Chord(70, 220, 230, 300, P1.X, P1.Y, P2.X, P2.Y);
    LmfCanvas.Brush.Color := clBlue;
    LmfCanvas.Chord(70, 220, 230, 300, 90*16, 120*16);

    // ArcTo
    C := Point(170, 310);
    P1 := C + Point(100, 0);
    P2 := C + Point(100, -100);
    LmfCanvas.Pen.Color := clNavy;
    LmfCanvas.Pen.Width := 2;
    LmfCanvas.MoveTo(C.X, C.Y);
    LmfCanvas.ArcTo(C.X-60, C.Y-30, C.X+60, C.Y+30, P1.X, P1.Y, P2.X, P2.Y);
    LmfCanvas.Arc(C.X-60, C.Y-30, C.X+60, C.Y+30, P1.X, P1.Y, P2.X, P2.Y);
    LmfCanvas.Pen.Width := 1;;
    LmfCanvas.Frame(C.X-60, C.Y-30, C.X+60, C.Y+30);

    // AngleArc
    C := Point(440, 280);
    LmfCanvas.Pen.Color := clRed;
    LmfCanvas.Pen.Width := 2;
    LmfCanvas.MoveTo(C.X, C.Y);
    LmfCanvas.AngleArc(C.X, C.Y, 50, 45, 90);
    LmfCanvas.Pen.Width := 1;;
    LmfCanvas.Frame(C.X-50, C.Y-50, C.X+50, C.Y+50);

    // Polygon
    P[0] := Point(400, 0);
    P[1] := Point(480, 100);
    P[2] := Point(360, 50);
    P[3] := Point(480, 0);
    P[4] := Point(400, 100);
    LmfCanvas.Brush.Style := bsSolid;
    LmfCanvas.Brush.Color := clMoneygreen;
    Lmfcanvas.Pen.Color := clGreen;
    LmfCanvas.Polygon(@P[0], 5, false);
    inc(P[0].Y, 110);
    inc(P[1].Y, 110);
    inc(P[2].Y, 110);
    inc(P[3].Y, 110);
    inc(P[4].Y, 110);
    LmfCanvas.Polygon(@P[0], 5, true);

    // Text
    LmfCanvas.Font.Size := 12;
    LmfCanvas.Font.Color := clOlive;
    LmfCanvas.TextOut(20, 20, 'Painted in OnCreate');

    // TextRect
    R := Rect(0, 0, FLmfImg.Width, FLmfImg.Height);
    ts := LmfCanvas.TextStyle;
    ts.Alignment := taCenter;
    ts.Layout := tlCenter;
    ts.SingleLine := false;
    LmfCanvas.Font.Color := clBlue;
    LmfCanvas.Font.Style := [fsBold, fsItalic];
    LmfCanvas.Font.Size := 16;
    LmfCanvas.TextRect(R, 0, 0, 'Centered' + LineEnding + 'in rect', ts);

  finally
    LmfCanvas.Free;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FLmfImg.Free;
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  if Assigned(FLmfImg) then
  begin
    // Paint metafile image on the canvas of the form
    Canvas.Draw(10, 10, FLmfImg);
  end;
end;

end.

