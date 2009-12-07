unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, FPimage, agg_fpimage, Agg_LCL,
  agg_svg_parser_lcl, agg_svg_path_renderer;

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

    p : parser;
    m_path : path_renderer;
    m_min_x, m_min_y, m_max_x, m_max_y: double;
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  HasFont: Boolean;
  FontFilename: String;
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
      if not FileExists(FontFilename) then raise Exception.Create('file not found: '+FontFilename+' CurDir='+GetCurrentDirUTF8);
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

    m_path.Construct;
    m_min_x:=0.0;
    m_min_y:=0.0;
    m_max_x:=0.0;
    m_max_y:=0.0;
    p.Construct(@m_path);
    try
      p.parse(SetDirSeparators('../../svg/tiger.svg'));
      {m_path.arrange_orientations;
      m_path.bounding_rect(@m_min_x ,@m_min_y ,@m_max_x ,@m_max_y );}
    finally
      p.Destruct;
    end;
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

  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AggLCLCanvas.Free;
  Bitmap1.Free;
  m_path.Destruct;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.Draw(0,0,Bitmap1);
end;

initialization
  {$I unit3.lrs}

end.

