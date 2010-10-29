unit MainUnit1;

{$mode objfpc}{$H+}

interface

{.$DEFINE DEBUG} //Show draw for rotate
uses
  Classes, SysUtils, LCLType, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, FPImage, IntfGraphics,Math, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Form1Create(Sender: TObject);
    procedure Form1Destroy(Sender: TObject);
  private
    procedure FadeIn(ABitmap: TBitmap; x, y: integer);
    procedure Rotate(ABitmap: TBitmap; aCanvas : TCanvas; x, y, Angle : integer);

  public
    SampleBitmapABitmap: TBitmap;
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  FadeIn(SampleBitmapABitmap,120,120);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Rotate(SampleBitmapABitmap,Canvas,120,120,StrToIntDef(Edit1.Text,90));
end;

procedure TForm1.Form1Create(Sender: TObject);
begin
  SampleBitmapABitmap:=TBitmap.Create;
  SampleBitmapABitmap.LoadFromFile(SetDirSeparators('../../images/lazarusform.bmp'));
end;

procedure TForm1.Form1Destroy(Sender: TObject);
begin
  SampleBitmapABitmap.Free;
end;

procedure TForm1.FadeIn(ABitmap: TBitmap; x, y: integer);
var
  SrcIntfImg, TempIntfImg: TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
  FadeStep: Integer;
  px, py: Integer;
  CurColor: TFPColor;
  TempBitmap: TBitmap;
begin
  SrcIntfImg:=TLazIntfImage.Create(0,0);
  SrcIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);
  TempIntfImg:=TLazIntfImage.Create(0,0);
  TempIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);
  TempBitmap:=TBitmap.Create;
  for FadeStep:=1 to 32 do
  begin
    for py:=0 to SrcIntfImg.Height-1 do
    begin
      for px:=0 to SrcIntfImg.Width-1 do
      begin
        CurColor:=SrcIntfImg.Colors[px,py];
        CurColor.Red:=(CurColor.Red*FadeStep) shr 5;
        CurColor.Green:=(CurColor.Green*FadeStep) shr 5;
        CurColor.Blue:=(CurColor.Blue*FadeStep) shr 5;
        TempIntfImg.Colors[px,py]:=CurColor;
      end;
    end;
    TempIntfImg.CreateBitmaps(ImgHandle,ImgMaskHandle,false);
    TempBitmap.Handle:=ImgHandle;
    TempBitmap.MaskHandle:=ImgMaskHandle;
    Canvas.Draw(x,y,TempBitmap);
  end;
  SrcIntfImg.Free;
  TempIntfImg.Free;
  TempBitmap.Free;
end;

procedure TForm1.Rotate(ABitmap: TBitmap; aCanvas : TCanvas; x, y, Angle: integer);
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
  
  {$IFDEF DEBUG}
  procedure Croix(aX,aY : integer; cl : TColor=clBlack);
  begin
    aCanvas.pen.Color:=cl;
    aCanvas.MoveTo(ax-10,ay);
    aCanvas.LineTo(ax+10,ay);
    aCanvas.MoveTo(ax,ay-10);
    aCanvas.LineTo(ax,ay+10);
  end;
  {$ENDIF}
  
begin
  SrcIntfImg:=TLazIntfImage.Create(0,0);
  SrcIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);
  TempIntfImg:=TLazIntfImage.Create(0,0);
  
  //Calculate the Sin and Cos of beta for later.
  Beta:=(Angle)*Pi/180;

  try
    {$IFDEF DEBUG}
    aCanvas.Brush.Style:=bsSolid;
    aCanvas.Brush.Color:=Color;
    aCanvas.FillRect(Rect(0,0,Width,Height-100));
    aCanvas.Brush.Color:=clWhite;
    {$ENDIF}
    
    TempIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);
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

    {$IFDEF DEBUG}
    aCanvas.Rectangle(0+x,0+y,SrcIntfImg.Width+x,SrcIntfImg.Height+y);
    aCanvas.TextOut(xo+x-20,yo+y-20,Format('(%dx%d)',[MaxX-MinX,MaxY-MinY]));
    {$ENDIF}

    TempIntfImg.Width :=(MaxX-MinX)+1;
    TempIntfImg.Height:=(MaxY-MinY)+1;
    
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

    {$IFDEF DEBUG}
    Croix(xo+x,yo+y,clblue);
    px:=SrcIntfImg.Width;
    py:=SrcIntfImg.Height;
    RotatePts(px,py);
    croix(px+x,py+y);

    px:=SrcIntfImg.Width;
    py:=0;
    RotatePts(px,py);
    croix(px+x,py+y);

    px:=0;
    py:=SrcIntfImg.Height;
    RotatePts(px,py);
    croix(px+x,py+y);

    px:=0;
    py:=0;
    RotatePts(px,py);
    croix(px+x,py+y);
    {$ENDIF}
    
    TempIntfImg.CreateBitmaps(ImgHandle,ImgMaskHandle,false);
    TempBitmap.Handle:=ImgHandle;
    TempBitmap.MaskHandle:=ImgMaskHandle;
    aCanvas.Draw(x-dx,y-dy,TempBitmap);

    {$IFDEF DEBUG}
    aCanvas.Brush.Style:=bsClear;
    aCanvas.Rectangle(x-dx,y-dy,TempBitmap.Width+x-dx,TempBitmap.Height+y-dy);
    {$ENDIF}
  finally
    SrcIntfImg.Free;
    TempIntfImg.Free;
    TempBitmap.Free;
  end;
end;

initialization
  {$I mainunit1.lrs}

end.

