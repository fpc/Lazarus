unit main;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, Forms, Controls,
  Graphics, Dialogs, TADrawUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    ListChartSource1: TListChartSource;
    procedure Chart1BeforeCustomDrawBackground(ASender: TChart;
      ADrawer: IChartDrawer; const ARect: TRect; var ADoDefaultDrawing: Boolean
      );
    procedure Chart1BeforeCustomDrawBackWall(ASender: TChart;
      ADrawer: IChartDrawer; const ARect: TRect; var ADoDefaultDrawing: Boolean
      );
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FBackImage: TPicture;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  IntfGraphics;

//{$R splash_logo.res}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FBackImage := TPicture.Create;
  FBackImage.LoadFromFile('..\..\..\..\images\splash_logo.png');
  // above path assumes standard Lazarus installation

  // or, use resources:
  //FBackImage.LoadFromResourceName(HInstance, 'splash_logo');
  // Don't forget to activate the "{$R" directive above.
end;

procedure TForm1.Chart1BeforeCustomDrawBackWall(ASender: TChart;
  ADrawer: IChartDrawer; const ARect: TRect; var ADoDefaultDrawing: Boolean);
var
  bmp: TBitmap;
  img: TLazIntfImage;
begin
  img := TLazIntfImage.Create(0, 0);
  try
    bmp := TBitmap.Create;
    try
      bmp.SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
      bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), FBackImage.Graphic);
      img.LoadFromBitmap(bmp.Handle, bmp.MaskHandle);
    finally
      bmp.Free;
    end;
    ADrawer.PutImage(ARect.Left, ARect.Top, img);
    ADoDefaultDrawing := false;
  finally
    img.Free;
  end;
end;

(* --------------- deprecated, was in use before Laz 2.1 -----------------------
procedure TForm1.Chart1BeforeDrawBackWall(ASender: TChart; ACanvas: TCanvas;
  const ARect: TRect; var ADoDefaultDrawing: Boolean);
begin
  ACanvas.StretchDraw(ARect, FBackImage.Graphic);
  ADoDefaultDrawing := false;
end;
----------------------------------------------------------------------------- *)

procedure TForm1.Chart1BeforeCustomDrawBackground(ASender: TChart;
  ADrawer: IChartDrawer; const ARect: TRect; var ADoDefaultDrawing: Boolean);
var
  bmp: TBitmap;
  img: TLazIntfImage;
begin
  img := TLazIntfImage.Create(0, 0);
  try
    bmp := TBitmap.Create;
    try
      bmp.SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
      bmp.Canvas.GradientFill(Rect(0, 0, bmp.Width, bmp.Height), clSkyBlue, clWhite, gdVertical);
      img.LoadFromBitmap(bmp.Handle, bmp.MaskHandle);
    finally
      bmp.Free;
    end;
    ADrawer.PutImage(ARect.Left, ARect.Top, img);
    ADoDefaultDrawing := false;
  finally
    img.Free;
  end;
end;

(* --------------- deprecated, was in use before Laz 2.1 -----------------------
procedure TForm1.Chart1BeforeDrawBackground(ASender: TChart; ACanvas: TCanvas;
  const ARect: TRect; var ADoDefaultDrawing: Boolean);
begin
  ACanvas.GradientFill(ARect, clSkyBlue, clWhite, gdVertical);
  ADoDefaultDrawing := false;
end;
----------------------------------------------------------------------------- *)

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBackImage.Free;
end;

end.

