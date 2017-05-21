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
  Classes, SysUtils, LCLProc, LCLType, ImgList, Controls, Graphics, LResources,
  Math;

type

  { TIDEImages }

  TIDEImages = class
  private
    FImages_12: TCustomImageList;
    FImages_16: TCustomImageList;
    FImages_24: TCustomImageList;
    FImageNames_12: TStringList;
    FImageNames_16: TStringList;
    FImageNames_24: TStringList;
  protected
    function GetImages_12: TCustomImageList;
    function GetImages_16: TCustomImageList;
    function GetImages_24: TCustomImageList;

    class function CreateBitmapFromRes(const ImageName: string): TCustomBitmap;
  public
    constructor Create;
    destructor Destroy; override;

    class function GetScalePercent: Integer;
    class function ScaleImage(const AImage: TCustomBitmap; out ANewInstance: Boolean;
      TargetWidth, TargetHeight: Integer; const AFactor: Double): TCustomBitmap;
    class function CreateImage(ImageSize: Integer; ImageName: String): TCustomBitmap;
    class procedure AssignImage(const ABitmap: TCustomBitmap; ImageName: String;
      ImageSize: Integer = 16);

    function GetImageIndex(ImageSize: Integer; ImageName: String): Integer;
    function LoadImage(ImageSize: Integer; ImageName: String): Integer;

    property Images_12: TCustomImageList read GetImages_12;
    property Images_16: TCustomImageList read GetImages_16;
    property Images_24: TCustomImageList read GetImages_24;
  end;

function IDEImages: TIDEImages;

implementation

var
  FIDEImages: TIDEImages;

{ TIDEImages }

function TIDEImages.GetImages_12: TCustomImageList;
begin
  if FImages_12 = nil then
  begin
    FImages_12 := TImageList.Create(nil);
    FImages_12.Width := MulDiv(12, GetScalePercent, 100);
    FImages_12.Height := FImages_12.Width;
  end;
  Result := FImages_12;
end;

function TIDEImages.GetImages_16: TCustomImageList;
begin
  if FImages_16 = nil then
  begin
    FImages_16 := TImageList.Create(nil);
    FImages_16.Width := MulDiv(16, GetScalePercent, 100);
    FImages_16.Height := FImages_16.Width;
  end;
  Result := FImages_16;
end;

function TIDEImages.GetImages_24: TCustomImageList;
begin
  if FImages_24 = nil then
  begin
    FImages_24 := TImageList.Create(nil);
    FImages_24.Width := MulDiv(24, GetScalePercent, 100);
    FImages_24.Height := FImages_24.Width;
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
    Result := 200; // 200%: 200% scaling
end;

class function TIDEImages.CreateImage(ImageSize: Integer; ImageName: String
  ): TCustomBitmap;
var
  Grp: TCustomBitmap;
  GrpScaledNewInstance: Boolean;
  ScalePercent: Integer;
begin
  ScalePercent := GetScalePercent;

  Grp := nil;
  try
    if ScalePercent<>100 then
    begin
      Grp := CreateBitmapFromRes(ImageName+'_'+IntToStr(ScalePercent));
      if Grp<>nil then
      begin
        Result := Grp;
        Grp := nil;
        Exit; // found
      end;
    end;

    Grp := CreateBitmapFromRes(ImageName);
    if Grp<>nil then
    begin
      Result := ScaleImage(Grp, GrpScaledNewInstance,
        ImageSize*ScalePercent div 100, ImageSize * ScalePercent div 100, ScalePercent / 100);
      if not GrpScaledNewInstance then
        Grp := nil;
      Exit; // found
    end;
  finally
    Grp.Free;
  end;
  Result := nil; // not found
end;

class procedure TIDEImages.AssignImage(const ABitmap: TCustomBitmap;
  ImageName: String; ImageSize: Integer);
var
  xBmp: TCustomBitmap;
begin
  xBmp := TIDEImages.CreateImage(ImageSize, ImageName);
  try
    ABitmap.Assign(xBmp);
  finally
    xBmp.Free;
  end;
end;

constructor TIDEImages.Create;
begin
  FImageNames_12 := TStringList.Create;
  FImageNames_12.Sorted := True;
  FImageNames_12.Duplicates := dupIgnore;
  FImageNames_16 := TStringList.Create;
  FImageNames_16.Sorted := True;
  FImageNames_16.Duplicates := dupIgnore;
  FImageNames_24 := TStringList.Create;
  FImageNames_24.Sorted := True;
  FImageNames_24.Duplicates := dupIgnore;
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

destructor TIDEImages.Destroy;
begin
  FreeAndNil(FImages_12);
  FreeAndNil(FImages_16);
  FreeAndNil(FImages_24);
  FreeAndNil(FImageNames_12);
  FreeAndNil(FImageNames_16);
  FreeAndNil(FImageNames_24);
  inherited Destroy;
end;

function TIDEImages.GetImageIndex(ImageSize: Integer; ImageName: String): Integer;
var
  List: TStringList;
begin
  case ImageSize of
    12: List := FImageNames_12;
    16: List := FImageNames_16;
    24: List := FImageNames_24;
  else
    List := nil;
  end;
  if List <> nil then
  begin
    Result := List.IndexOf(ImageName);
    if Result <> -1 then
      Result := PtrInt(List.Objects[Result]);
  end
  else
    Result := -1;
end;

function TIDEImages.LoadImage(ImageSize: Integer; ImageName: String): Integer;
var
  List: TCustomImageList;
  Names: TStringList;
  Grp: TGraphic;
begin
  Result := GetImageIndex(ImageSize, ImageName);
  if Result <> -1 then Exit;

  case ImageSize of
    12:
      begin
        List := Images_12; // make sure we have a list
        Names := FImageNames_12;
      end;
    16:
      begin
        List := Images_16; // make sure we have a list
        Names := FImageNames_16;
      end;
    24:
      begin
        List := Images_24; // make sure we have a list
        Names := FImageNames_24;
      end;
  else
    Exit;
  end;
  try
    Grp := CreateImage(ImageSize, ImageName);
    try
      if Grp=nil then
        raise Exception.CreateFmt('TIDEImages.LoadImage: %s not found.', [ImageName]);
      if Grp is TCustomBitmap then
        Result := List.Add(TCustomBitmap(Grp), nil)
      else
        Result := List.AddIcon(Grp as TCustomIcon);
    finally
      Grp.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('While loading IDEImages: ' + e.Message);
      Result := -1;
    end;
  end;
  Names.AddObject(ImageName, TObject(PtrInt(Result)));
end;

class function TIDEImages.ScaleImage(const AImage: TCustomBitmap; out
  ANewInstance: Boolean; TargetWidth, TargetHeight: Integer;
  const AFactor: Double): TCustomBitmap;
var
  Bmp: TBitmap;
  TargetRect: TRect;
begin
  if SameValue(AFactor, 1) then
  begin
    ANewInstance := False;
    Exit(AImage);
  end;

  Bmp := TBitmap.Create;
  try
    Result := Bmp;
    ANewInstance := True;
    {$IFDEF LCLGtk2}
    Bmp.PixelFormat := pf24bit;
    Bmp.Canvas.Brush.Color := clBtnFace;
    {$ELSE}
    Bmp.PixelFormat := pf32bit;
    Bmp.Canvas.Brush.Color := TColor($FFFFFFFF);
    {$ENDIF}
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
