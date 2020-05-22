{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit IDEImagesIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  // LCL
  LCLProc, LCLType, LResources, ImgList, Controls, Graphics, Buttons;

type

  { TIDEImages }

  TIDEImages = class
  private
    FImages_12: TLCLGlyphs;
    FImages_16: TLCLGlyphs;
    FImages_24: TLCLGlyphs;
    procedure FImages_24_GetWidthForPPI(Sender: TCustomImageList; {%H-}AImageWidth,
      {%H-}APPI: Integer; var AResultWidth: Integer);
    function GetImages(Size: Integer): TLCLGlyphs;
  protected
    function GetImages_12: TLCLGlyphs;
    function GetImages_16: TLCLGlyphs;
    function GetImages_24: TLCLGlyphs;

    class function CreateBitmapFromRes(const ImageName: string): TCustomBitmap;
    class function CreateBestBitmapForScalingFromRes(const ImageName: string;
      const aDefScale: Integer; out aBitmap: TCustomBitmap): Integer;
  public
    destructor Destroy; override;

    class function GetScalePercent: Integer;
    class function ScaleImage(const AImage: TGraphic; out ANewInstance: Boolean;
      TargetWidth, TargetHeight: Integer; const AFactor: Double): TCustomBitmap;
    class function CreateImage(ImageSize: Integer; ImageName: String): TCustomBitmap; deprecated 'Don''t use this, use image lists instead.';
    class function CreateImage(ImageName: String; ImageSize: Integer = 16): TCustomBitmap; deprecated 'Don''t use this, use image lists instead.';
    class procedure AssignImage(const ABitmap: TCustomBitmap; ImageName: String;
      ImageSize: Integer = 16); deprecated 'Use the other overloads instead.';
    procedure AssignImage(const ABitBtn: TCustomBitBtn; ImageName: String;
      ImageSize: Integer = 16);
    procedure AssignImage(const ASpeedButton: TCustomSpeedButton; ImageName: String;
      ImageSize: Integer = 16);
    class function AddImageToImageList(const AImageList: TImageList;
      ImageName: String; ImageSize: Integer = 16): Integer; deprecated 'Don''t use this, use image lists instead.';
    class function ScaledSize(ImageSize: Integer = 16): Integer; deprecated 'Don''t use this, use image lists instead.';

    function LoadImage(ImageSize: Integer; ImageName: String): Integer; deprecated 'Use the other overload instead.';
    function LoadImage(ImageName: String; ImageSize: Integer = 16): Integer;
    function GetImageIndex(ImageSize: Integer; ImageName: String): Integer; deprecated 'Use the other overload instead.';
    function GetImageIndex(ImageName: String; ImageSize: Integer = 16): Integer;

    property Images_12: TLCLGlyphs read GetImages_12;
    property Images_16: TLCLGlyphs read GetImages_16;
    property Images_24: TLCLGlyphs read GetImages_24;
    property Images[Size: Integer]: TLCLGlyphs read GetImages;
  end;

function IDEImages: TIDEImages;

implementation

var
  FIDEImages: TIDEImages;

{ TIDEImages }

function TIDEImages.GetImages_12: TLCLGlyphs;
begin
  if FImages_12 = nil then
  begin
    FImages_12 := TLCLGlyphs.Create(nil);
    FImages_12.Width := 12;
    FImages_12.Height := FImages_12.Width;
    FImages_12.RegisterResolutions([12, 16, 24]);
  end;
  Result := FImages_12;
end;

function TIDEImages.GetImages_16: TLCLGlyphs;
begin
  if FImages_16 = nil then
  begin
    FImages_16 := TLCLGlyphs.Create(nil);
    FImages_16.Width := 16;
    FImages_16.Height := FImages_16.Width;
    FImages_16.RegisterResolutions([16, 24, 32]);
  end;
  Result := FImages_16;
end;

function TIDEImages.GetImages_24: TLCLGlyphs;
begin
  if FImages_24 = nil then
  begin
    FImages_24 := TLCLGlyphs.Create(nil);
    FImages_24.Width := 24;
    FImages_24.Height := FImages_24.Width;
    FImages_24.RegisterResolutions([24, 36, 48]);
  end;
  Result := FImages_24;
end;

class function TIDEImages.GetScalePercent: Integer;
begin
  if ScreenInfo.PixelsPerInchX <= 120 then
    Result := 100 // 100-125% (96-120 DPI): no scaling
  else
  if ScreenInfo.PixelsPerInchX <= 168 then
    Result := 150 // 126%-175% (144-168 DPI): 150% scaling
  else
    Result := Round(ScreenInfo.PixelsPerInchX/96) * 100; // 200, 300, 400, ...
end;

function TIDEImages.LoadImage(ImageSize: Integer; ImageName: String): Integer;
begin
  Result := LoadImage(ImageName, ImageSize);
end;

class function TIDEImages.CreateImage(ImageName: String; ImageSize: Integer
  ): TCustomBitmap;
var
  Grp: TCustomBitmap;
  GrpScaledNewInstance: Boolean;
  ScalePercent, GrpScale: Integer;
begin
  ScalePercent := GetScalePercent;

  Grp := nil;
  try
    GrpScale := CreateBestBitmapForScalingFromRes(ImageName, ScalePercent, Grp);
    if Grp<>nil then
    begin
      Result := ScaleImage(Grp, GrpScaledNewInstance,
        MulDiv(ImageSize, ScalePercent, GrpScale), MulDiv(ImageSize, ScalePercent, GrpScale), ScalePercent / GrpScale);
      if not GrpScaledNewInstance then
        Grp := nil;
      Exit; // found
    end;
  finally
    Grp.Free;
  end;
  Result := nil; // not found
end;

destructor TIDEImages.Destroy;
begin
  FImages_12.Free;
  FImages_16.Free;
  FImages_24.Free;
  inherited Destroy;
end;

procedure TIDEImages.FImages_24_GetWidthForPPI(Sender: TCustomImageList;
  AImageWidth, APPI: Integer; var AResultWidth: Integer);
begin
  if (30<=AResultWidth) and (AResultWidth<=40) then
    AResultWidth := 32;
end;

class procedure TIDEImages.AssignImage(const ABitmap: TCustomBitmap;
  ImageName: String; ImageSize: Integer);
var
  xBmp: TCustomBitmap;
begin
  xBmp := CreateImage(ImageName, ImageSize);
  try
    ABitmap.Assign(xBmp);
  finally
    xBmp.Free;
  end;
end;

class function TIDEImages.AddImageToImageList(const AImageList: TImageList;
  ImageName: String; ImageSize: Integer): Integer;
var
  xBmp: TCustomBitmap;
begin
  Result := -1;
  xBmp := TIDEImages.CreateImage(ImageName, ImageSize);
  try
    Result := AImageList.Add(xBmp, nil);
  finally
    xBmp.Free;
  end;
end;

procedure TIDEImages.AssignImage(const ABitBtn: TCustomBitBtn;
  ImageName: String; ImageSize: Integer);
var
  IL: TLCLGlyphs;
begin
  IL := Images[ImageSize];
  if IL=nil then Exit;
  ABitBtn.Images := IL;
  ABitBtn.ImageIndex := IL.GetImageIndex(ImageName);
end;

procedure TIDEImages.AssignImage(const ASpeedButton: TCustomSpeedButton;
  ImageName: String; ImageSize: Integer);
var
  IL: TLCLGlyphs;
begin
  IL := Images[ImageSize];
  if IL=nil then Exit;
  ASpeedButton.Images := IL;
  ASpeedButton.ImageIndex := IL.GetImageIndex(ImageName);
end;

class function TIDEImages.ScaledSize(ImageSize: Integer): Integer;
begin
  Result := ImageSize * GetScalePercent div 100;
end;

class function TIDEImages.CreateBitmapFromRes(const ImageName: string
  ): TCustomBitmap;
var
  ResHandle: TLResource;
begin
  ResHandle := LazarusResources.Find(ImageName);
  if ResHandle <> nil then
    Result := CreateBitmapFromLazarusResource(ResHandle)
  else
    Result := CreateBitmapFromResourceName(HInstance, ImageName);
end;

class function TIDEImages.CreateBestBitmapForScalingFromRes(const ImageName: string;
  const aDefScale: Integer; out aBitmap: TCustomBitmap): Integer;
begin
  aBitmap := nil;
  Result := aDefScale;
  while (Result > 100) do
  begin
    aBitmap := CreateBitmapFromRes(ImageName+'_'+IntToStr(Result));
    if aBitmap<>nil then Exit;
    if (Result>300) and ((Result div 100) mod 2 = 1) then // 500, 700, 900 ...
      Result := Result + 100;
    Result := Result div 2;
  end;
  aBitmap := CreateBitmapFromRes(ImageName);
  Result := 100;
  if (aBitmap is TBitmap) and (aBitmap.PixelFormat in [pf1bit..pf24bit]) then
  begin
    aBitmap.TransparentColor := aBitmap.Canvas.Pixels[0, aBitmap.Height-1];
    aBitmap.Transparent := True;
  end;
end;

class function TIDEImages.CreateImage(ImageSize: Integer; ImageName: String
  ): TCustomBitmap;
begin
  Result := CreateImage(ImageName, ImageSize);
end;

function TIDEImages.GetImageIndex(ImageSize: Integer; ImageName: String): Integer;
begin
  Result := GetImageIndex(ImageName, ImageSize);
end;

function TIDEImages.GetImageIndex(ImageName: String; ImageSize: Integer): Integer;
var
  List: TLCLGlyphs;
begin
  List := Images[ImageSize];
  if List <> nil then
    Result := List.GetImageIndex(ImageName)
  else
    Result := -1;
end;

function TIDEImages.GetImages(Size: Integer): TLCLGlyphs;
begin
  case Size of
    12: Result := Images_12;
    16: Result := Images_16;
    24: Result := Images_24;
  else
    Result := nil
  end;
end;

function TIDEImages.LoadImage(ImageName: String; ImageSize: Integer): Integer;
begin
  Result := GetImageIndex(ImageName, ImageSize);
end;

class function TIDEImages.ScaleImage(const AImage: TGraphic; out
  ANewInstance: Boolean; TargetWidth, TargetHeight: Integer;
  const AFactor: Double): TCustomBitmap;
var
  Bmp: TBitmap;
  TargetRect: TRect;
begin
  if SameValue(AFactor, 1) and (AImage is TCustomBitmap) then
  begin
    ANewInstance := False;
    Exit(TCustomBitmap(AImage));
  end;

  Bmp := TBitmap.Create;
  try
    Result := Bmp;
    ANewInstance := True;
    if (AImage is TBitmap) and AImage.Transparent then
    begin
      Bmp.PixelFormat := pf24bit;
      Bmp.Canvas.Brush.Color := TBitmap(AImage).TransparentColor;
      Bmp.TransparentColor := TBitmap(AImage).TransparentColor;
      Bmp.Transparent := TBitmap(AImage).Transparent;
    end else
    begin
      {$IFDEF LCLGtk2}
      Bmp.PixelFormat := pf24bit;
      Bmp.Canvas.Brush.Color := clBtnFace;
      Bmp.TransparentColor := clBtnFace;
      Bmp.Transparent := True;
      {$ELSE}
      Bmp.PixelFormat := pf32bit;
      Bmp.Canvas.Brush.Color := TColor($FFFFFFFF);
      {$ENDIF}
    end;
    Bmp.SetSize(TargetWidth, TargetHeight);
    Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);
    TargetRect := Rect(0, 0, Round(AImage.Width*AFactor), Round(AImage.Height*AFactor));
    OffsetRect(TargetRect, (TargetWidth-TargetRect.Right) div 2, (TargetHeight-TargetRect.Bottom) div 2);
    Bmp.Canvas.StretchDraw(TargetRect, AImage);
  except
    FreeAndNil(Result);
    ANewInstance := False;
    raise;
  end;
end;

function IDEImages: TIDEImages;
begin
  if FIDEImages = nil then
    FIDEImages := TIDEImages.Create;
  Result := FIDEImages;
end;

initialization
  FIDEImages := nil;

finalization
  FIDEImages.Free;
  FIDEImages:=nil;

end.
