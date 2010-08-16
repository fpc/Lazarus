unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, FPimage, agg_fpimage, Agg_LCL;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
  public
    AggLCLCanvas: TAggLCLCanvas;
    Bitmap1: TBitmap;
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  HasFont: Boolean;
  FontFilename: String;
  s: String;
  TxtW: integer;
  TxtH: integer;
  TxtX: Integer;
  TxtY: Integer;
begin
  Bitmap1:=TBitmap.Create;
  AggLCLCanvas:=TAggLCLCanvas.Create;
  with AggLCLCanvas do begin
    Image.PixelFormat:=afpimRGBA32;
    Image.SetSize(500,500);
  end;
  {$IFDEF LCLGtk2}
  HasFont:=true;
  {$ELSE}
  HasFont:=false;
  {$ENDIF}

  // paint to agg canvas
  with AggLCLCanvas do begin
    if HasFont then begin
      FontFilename:=SetDirSeparators('../../verdana.ttf');
      if not FileExistsUTF8(FontFilename) then raise Exception.Create('file not found: '+FontFilename+' CurDir='+GetCurrentDirUTF8);
      Font.LoadFromFile(FontFilename);
      Font.Size:=10;
      Font.Color:=clBlack;
    end;

    // solid white background
    Brush.Color:=clWhite;
    FillRect(0,0,Width,Height);

    Brush.Color:=clRed;
    Pen.Color:=clBlue;
    Pen.Width:=1;

    Line(12,10,22,10);
    Line(10,12,10,22);
    Line(12,12,22,22);

    FillRect(40,10,50,20);
    Ellipse(55,10,65,20);
    GradientFill(Rect(70,10,80,20),clRed,clBlue,gdVertical);
    Frame(85,10,95,20);
    Arc(100,10,110,20, 0,2700);
    Arc(115,10,125,20, 1000,2700);
    Arc(130,10,140,20, 135,5, 130,20);
    Chord(145,10,165,30, 0,2000);
    Chord(170,10,190,30, 1000,2000);
    Chord(195,10,215,30, 205,5, 195,30);
    Pie(220,10,240,30, 230,5, 220,30);
    RadialPie(245,10,265,30, 1000,2000);

    s:='Font.Size='+IntToStr(Font.Size);
    GetTextSize(s,TxtW,TxtH);
    TxtX:=10;
    TxtY:=40;
    FillRect(TxtX,TxtY,TxtX+TxtW,TxtY+TxtH);
    TextOut(TxtX,TxtY,s);

    RoundRect(10,80,30,100,15,15);
    Polyline([Point(35,80),Point(45,80),Point(55,80),Point(55,90),
              Point(55,90),Point(55,100),Point(35,90),Point(35,100)]);
    PolyBezier([Point(35,80),Point(45,80),Point(55,80),Point(55,90),
                Point(55,90),Point(55,100),Point(35,90),Point(35,100)],
                false,false);
  end;

  // convert to LCL native pixel format
  Bitmap1.LoadFromIntfImage(AggLCLCanvas.Image.IntfImg);

  // paint with widgetset to bitmap
  with Bitmap1.Canvas do begin
    Font.Size:=10;
    Font.Color:=clBlack;

    Brush.Color:=clRed;
    Pen.Color:=clBlue;
    Pen.Width:=1;

    Line(24,10,34,10);
    Line(10,24,10,34);
    Line(24,24,34,34);

    FillRect(40,22,50,32);
    Ellipse(55,22,65,32);
    GradientFill(Rect(70,22,80,32),clRed,clBlue,gdVertical);
    Frame(85,22,95,32);
    Arc(100,22,110,32, 0,2700);
    Arc(115,22,125,32, 1000,2700);
    Arc(130,22,140,32, 135,15, 130,32);
    Chord(145,32,165,52, 0,2000);
    Chord(170,32,190,52, 1000,2000);
    Chord(195,32,215,52, 205,27, 195,52);
    Pie(220,32,240,52, 230,27, 220,52);
    RadialPie(245,32,265,52, 1000,2000);

    s:='Font.Size='+IntToStr(Font.Size);
    GetTextSize(s,TxtW,TxtH);
    TxtX:=10;
    TxtY:=60;
    FillRect(TxtX,TxtY,TxtX+TxtW,TxtY+TxtH);
    TextOut(TxtX,TxtY,s);

    RoundRect(10,105,30,125,15,15);
    Polyline([Point(35,105),Point(45,105),Point(55,105),Point(55,115),
              Point(55,115),Point(55,125),Point(35,115),Point(35,125)]);
    PolyBezier([Point(35,105),Point(45,105),Point(55,105),Point(55,115),
                Point(55,115),Point(55,125),Point(35,115),Point(35,125)],
                false,false);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AggLCLCanvas.Free;
  Bitmap1.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.Draw(0,0,Bitmap1);
end;

{$R *.lfm}

end.

