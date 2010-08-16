unit Unit3;

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

  // paint to agg canvas
  with AggLCLCanvas do begin
    {if HasFont then begin
      FontFilename:=SetDirSeparators('../../verdana.ttf');
      if not FileExists(FontFilename) then raise Exception.Create('file not found: '+FontFilename+' CurDir='+GetCurrentDirUTF8);
      Font.LoadFromFile(FontFilename);}
    Font.Size:=10;
    Font.Color:=clBlack;
    Font.LoadViaPango;

    // solid white background
    Brush.Color:=clWhite;
    FillRect(0,0,Width,Height);

    Brush.Color:=clRed;
    Pen.Color:=clBlue;
    Pen.Width:=1;

    s:='Font.Size='+IntToStr(Font.Size);
    GetTextSize(s,TxtW,TxtH);
    TxtX:=10;
    TxtY:=40;
    FillRect(TxtX,TxtY,TxtX+TxtW,TxtY+TxtH);
    TextOut(TxtX,TxtY,s);
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

    s:='Font.Size='+IntToStr(Font.Size);
    GetTextSize(s,TxtW,TxtH);
    TxtX:=10;
    TxtY:=60;
    FillRect(TxtX,TxtY,TxtX+TxtW,TxtY+TxtH);
    TextOut(TxtX,TxtY,s);
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

