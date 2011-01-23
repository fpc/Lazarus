unit Unit1; 

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
  FontFilename: String;
begin
  Bitmap1:=TBitmap.Create;
  AggLCLCanvas:=TAggLCLCanvas.Create;
  with AggLCLCanvas do begin
    Image.PixelFormat:=afpimRGBA32;
    Image.SetSize(250,250);
  end;

  // paint to agg canvas
  with AggLCLCanvas do begin
    // solid white background
    Brush.Color:=clWhite;
    FillRect(0,0,Width,Height);

    // a star with 5 corners, solid blue outline, filled solid yellow
    Brush.Color:=clYellow;
    Pen.Color:=clBlue;
    Pen.Width:=8;
    Ellipse(20,50,100,100);

    // a star with 5 corners, transparent blue outline, filled transparent yellow
    Brush.FPColor:=FPColor($ffff,$ffff,0,$5000);
    Pen.FPColor:=FPColor(0,0,$ffff,$5000);
    Ellipse(40,65,120,130);

    // solid blue text
    FontFilename:=SetDirSeparators('../../verdana.ttf');
    DebugLn(['TForm1.FormCreate ',FontFilename,' ',FileExistsUTF8(FontFilename)]);
    Font.LoadFromFile(FontFilename);
    Font.Size:=18;
    Font.Color:=clRed;
    TextOut(10,30,'LCL and AggPas');
  end;

  // convert to LCL native pixel format
  Bitmap1.LoadFromIntfImage(AggLCLCanvas.Image.IntfImg);
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

