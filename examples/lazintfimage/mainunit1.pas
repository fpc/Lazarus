unit MainUnit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, FPImage, IntfGraphics;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Form1Create(Sender: TObject);
    procedure Form1Destroy(Sender: TObject);
  private
    procedure FadeIn(ABitmap: TBitmap; x, y: integer);
  public
    ABitmap: TBitmap;
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  FadeIn(ABitmap,10,10);
end;

procedure TForm1.Form1Create(Sender: TObject);
begin
  ABitmap:=TBitmap.Create;
  ABitmap.LoadFromFile('../../images/lazarus.xpm');
end;

procedure TForm1.Form1Destroy(Sender: TObject);
begin
  ABitmap.Free;
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
  for FadeStep:=1 to 32 do begin
    for py:=0 to SrcIntfImg.Height-1 do begin
      for px:=0 to SrcIntfImg.Width-1 do begin
        CurColor:=SrcIntfImg.Colors[px,py];
        CurColor.Red:=(CurColor.Red*FadeStep) shr 5;
        CurColor.Green:=(CurColor.Green*FadeStep) shr 5;
        CurColor.Blue:=(CurColor.Blue*FadeStep) shr 5;
        TempIntfImg.Colors[px,py]:=CurColor;
      end;
    end;
    TempIntfImg.CreateBitmap(ImgHandle,ImgMaskHandle,false);
    TempBitmap.Handle:=ImgHandle;
    TempBitmap.MaskHandle:=ImgMaskHandle;
    Canvas.Draw(x,y,TempBitmap);
  end;
  SrcIntfImg.Free;
  TempIntfImg.Free;
  TempBitmap.Free;
end;

initialization
  {$I mainunit1.lrs}

end.

