unit imagetest;

{$mode objfpc}

interface

uses
  Classes, Math, Forms, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, LCLIntf, LCLType, IntfGraphics, FPImage;

type

  { TfrmImage }

  TfrmImage = class(TForm)
    btnSaveJPEG: TButton;
    btnResize: TButton;
    btnRotate: TButton;
    imageDepths: TImage;
    MyImage: TImage;
    Label1: TLabel;
    trackJPEG: TTrackBar;
    procedure btnResizeClick(Sender: TObject);
    procedure btnRotateClick(Sender: TObject);
    procedure btnSaveJPEGClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure RotateBitmap(ASource: TBitmap; ADest: TCanvas; x, y, Angle: integer);
  end; 

var
  frmImage: TfrmImage;

implementation

{$R *.lfm}

{ TfrmImage }

procedure TfrmImage.FormCreate(Sender: TObject);
var
  MyBitmap: TBitmap;
begin
  // Create a 100x100 bitmap and draw to it
  MyBitmap := TBitmap.Create;
  MyBitmap.PixelFormat := pf4bit;
  MyBitmap.Width  := 80;
  MyBitmap.Height := 80;
//  MyBitmap.
//  MyBitmap.Canvas.Brush.Color := RGBA(0,0,0,0);
  MyBitmap.Canvas.Pen.Color := clBlue;
  MyBitmap.Canvas.Rectangle(20, 20, 60, 60);
  imageDepths.Canvas.Draw(0, 0, MyBitmap);
  //
  MyBitmap.PixelFormat := pf32bit;
  MyBitmap.Width  := 80;
  MyBitmap.Height := 80;
  MyBitmap.Canvas.Brush.Color := TColor($F2F2F2);
  MyBitmap.Canvas.Pen.Color := clBlue;
  MyBitmap.Canvas.Rectangle(20, 20, 60, 60);
  imageDepths.Canvas.Draw(100, 0, MyBitmap);
  MyBitmap.Free;
end;

procedure TfrmImage.RotateBitmap(ASource: TBitmap; ADest: TCanvas; x, y, Angle: integer);
var
  SrcIntfImg, TempIntfImg: TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
  px, py    : Integer;
  CurColor  : TFPColor;
  TempBitmap: TBitmap;

  ToX,ToY   : Integer;
  Xo,Yo     : Integer;
  beta      : Single;
  MinX,MaxX : Integer;
  MinY,MaxY : Integer;
  Dx,Dy     : Integer;

  procedure RotatePts(Var aX,aY : Integer);
  Var Xr,Yr : Integer;
  begin
    //Change new axe
    xr:=aX-Xo;
    yr:=aY-Yo;

    //Rotation
    aX:=Xo+Round(Xr*Cos(Beta)+Yr*Sin(Beta));
    aY:=Yo+Round(Xr*Sin(Beta)*-1+Cos(Beta)*Yr);
  end;

begin
  SrcIntfImg:=TLazIntfImage.Create(0,0);
  SrcIntfImg.LoadFromBitmap(ASource.Handle,ASource.MaskHandle);
  TempIntfImg:=TLazIntfImage.Create(0,0);

  //Calculate the Sin and Cos of beta for later.
  Beta:=(Angle)*Pi/180;

  try
    TempIntfImg.LoadFromBitmap(ASource.Handle,ASource.MaskHandle);
    TempBitmap:=TBitmap.Create;

    Xo:= SrcIntfImg.Width  div 2; //Center of rotation for x
    Yo:= SrcIntfImg.Height div 2; //Center of rotation for y
    px:=xo;
    py:=yo;

    //Calc new size after rotation
    px:=0;
    py:=0;
    RotatePts(px,py);
    toX:=0;
    toY:=SrcIntfImg.Height;
    RotatePts(ToX,ToY);

    MinX:=Min(px+x,Tox+x);
    MaxX:=Max(px+x,Tox+x);
    MinY:=Min(py+y,Toy+y);
    MaxY:=Max(py+y,Toy+y);

    px:=SrcIntfImg.Width;
    py:=0;
    RotatePts(px,py);
    toX:=SrcIntfImg.Width;
    toY:=SrcIntfImg.Height;
    RotatePts(ToX,ToY);

    MaxX:=MaxIntValue([px+x,Tox+x,MaxX]);
    MaxY:=MaxIntValue([py+y,Toy+y,MaxY]);
    MinX:=MinIntValue([px+x,Tox+x,MinX]);
    MinY:=MinIntValue([py+y,Toy+y,MinY]);

    TempIntfImg.Width :=(MaxX-MinX)+1;
    TempIntfImg.Height:=(MaxY-MinY)+1;
    TempIntfImg.FillPixels(FPColor(0, 0, 0, 0));

    Dx:=(TempIntfImg.Width div 2)-Xo;
    Dy:=(TempIntfImg.Height div 2)-Yo;

    for py:=0 to SrcIntfImg.Height-1 do
    begin
      for px:=0 to SrcIntfImg.Width-1 do
      begin
        CurColor:=SrcIntfImg.Colors[px,py];

        ToX:=Px; ToY:=py;
        RotatePts(ToX,ToY);

        try
         TempIntfImg.Colors[ToX+Dx,ToY+Dy]:=CurColor;
        except
        end;
      end;
    end;

    TempIntfImg.CreateBitmaps(ImgHandle,ImgMaskHandle,false);
    TempBitmap.Handle:=ImgHandle;
    TempBitmap.MaskHandle:=ImgMaskHandle;
    ADest.Draw(x-dx,y-dy,TempBitmap);
  finally
    SrcIntfImg.Free;
    TempIntfImg.Free;
    TempBitmap.Free;
  end;
end;

procedure TfrmImage.btnSaveJPEGClick(Sender: TObject);
var
  jpeg: TJPEGImage;
  Points: array[0..2] of TPoint;
  SaveDialog: TSaveDialog;
begin
  jpeg := TJPEGImage.Create;
  SaveDialog := TSaveDialog.Create(nil);
  try
    // Create a blue triangle image
    // on a black background
    jpeg.Width := 100;
    jpeg.Height := 100;
    jpeg.Canvas.Brush.Color := clBlue;
    Points[0] := Point(50, 25);
    Points[1] := Point(25, 75);
    Points[2] := Point(75, 75);
    jpeg.Canvas.Polygon(Points);

    // Prepares the save dialog and the
    // compression configurations
    SaveDialog.DefaultExt := 'jpg';
    jpeg.CompressionQuality := trackJPEG.Position;

    // Saves the file
    if SaveDialog.Execute then
      jpeg.SaveToFile(SaveDialog.FileName);
  finally
    // Releases the objects
    jpeg.Free;
    SaveDialog.Free;
  end;
end;

procedure TfrmImage.btnResizeClick(Sender: TObject);
var
  MyBitmap: TBitmap;
begin
  // Create a 100x100 bitmap and draw to it
  MyBitmap := TBitmap.Create;
  MyBitmap.Width  := 100;
  MyBitmap.Height := 100;
  MyBitmap.Canvas.Brush.Color := clBlue;
  MyBitmap.Canvas.Pen.Color := clBlue;
  MyBitmap.Canvas.Rectangle(20, 20, 80, 80);
  // Now resize it to 200x100
  MyBitmap.Width := 200;
  MyBitmap.Canvas.CopyRect(
      Bounds(0, 0, 200, 100),
      MyBitmap.Canvas,
      Bounds(0, 0, 100, 100));
  MyImage.Canvas.Draw(0, 0, MyBitmap);
  MyBitmap.Free;

  {  LCLIntf.StretchBlt(
      MyBitmap.Canvas.Handle, 0, 0, 200, 100,
      MyBitmap.Canvas.Handle, 0, 0, 100, 100, SRCCOPY);}
end;

procedure TfrmImage.btnRotateClick(Sender: TObject);
var
  MyBitmap: TBitmap;
begin
  // Create a 100x100 bitmap and draw to it
  MyBitmap := TBitmap.Create;
  MyBitmap.Width  := 100;
  MyBitmap.Height := 100;
  MyBitmap.Canvas.Brush.Color := clBlue;
  MyBitmap.Canvas.Pen.Color := clBlue;
  MyBitmap.Canvas.Rectangle(20, 20, 80, 80);
  RotateBitmap(MyBitmap, MyImage.Canvas, 0, 0, 40);
  MyBitmap.Free;
end;

end.

