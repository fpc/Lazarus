program xpm_to_png;

{$mode objfpc}{$H+}
{$apptype console}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, Math, interfaces, LCLType, Graphics, GraphType, IntfGraphics,
  InterfaceBase
  { you can add units after this };

// portions of this file has been copied from imglist.inc

procedure FillDescription(out ADesc: TRawImageDescription; Width, Height: Integer);
begin
  ADesc.Init;
  ADesc.Format := ricfRGBA;
  ADesc.PaletteColorCount := 0;
  ADesc.MaskBitsPerPixel := 0;
  ADesc.Depth := 32;
  ADesc.Width := Width;
  ADesc.Height := Height;
  ADesc.BitOrder := riboBitsInOrder;
  ADesc.ByteOrder := riboMSBFirst;
  ADesc.LineOrder := riloTopToBottom;
  ADesc.BitsPerPixel := 32;
  ADesc.LineEnd := rileDWordBoundary;
  ADesc.RedPrec := 8; // red precision. bits for red
  ADesc.RedShift := 8;
  ADesc.GreenPrec := 8;
  ADesc.GreenShift := 16;
  ADesc.BluePrec := 8;
  ADesc.BlueShift := 24;
  ADesc.AlphaPrec := 8;
  ADesc.AlphaShift := 0;
end;

procedure InternalSetImage(var RawImage: TRawImage; SrcImage: TRawImage);
var
  Desc: TRawImageDescription absolute SrcImage.Description;

  SrcImg, DstImg: TLazIntfImage;
  SrcHasAlpha, KeepAlpha: Boolean;
  ALeft, ATop: integer;
begin
  SrcHasAlpha := SrcImage.Description.AlphaPrec > 0;
  KeepAlpha := SrcHasAlpha;
  if not SrcHasAlpha and (Desc.BitsPerPixel = 32) and (Desc.Depth = 24) and
    (SrcImage.Mask <> nil) and (Desc.MaskBitsPerPixel > 0)
  then begin
    // Try to squeeze Aplha channel in some unused bits
    if  (Desc.RedShift >= 8)
    and (Desc.GreenShift >= 8)
    and (Desc.BlueShift >= 8)
    then begin
      // there is room at the lsb side
      Desc.AlphaPrec := 8;
      Desc.AlphaShift := 0;
      Desc.Depth := 32;
      SrcHasAlpha := True;
    end
    else if (Desc.RedShift < 24)
        and (Desc.GreenShift < 24)
        and (Desc.BlueShift < 24)
    then begin
      // there is room at the msb side
      Desc.AlphaPrec := 8;
      Desc.AlphaShift := 24;
      Desc.Depth := 32;
      SrcHasAlpha := True;
    end;
  end;

  SrcImg := TLazIntfImage.Create(SrcImage, True);
  if SrcHasAlpha
  then SrcImg.AlphaFromMask(KeepAlpha);

  if not SrcHasAlpha
  then begin
    // Add maskdata to store copied mask, so an alpha can be created
    RawImage.Description.MaskBitsPerPixel := 1;
    RawImage.Description.MaskBitOrder := riboReversedBits;
    RawImage.Description.MaskLineEnd := rileByteBoundary;
    RawImage.Description.MaskShift := 0;
    RawImage.MaskSize := RawImage.Description.MaskBytesPerLine * RawImage.Description.Height;
    RawImage.Mask := GetMem(RawImage.MaskSize);
  end;

  DstImg := TLazIntfImage.Create(RawImage, False);
  ALeft := (RawImage.Description.Width - SrcImage.Description.Width) div 2;
  ATop := (RawImage.Description.Height - SrcImage.Description.Height) div 2;
  DstImg.CopyPixels(SrcImg, ALeft, ATop);
  if not SrcHasAlpha
  then begin
    DstImg.AlphaFromMask;
    FreeMem(RawImage.Mask);
    RawImage.Mask := nil;
    RawImage.MaskSize := 0;
  end;

  DstImg.Free;
  SrcImg.Free;
end;

procedure TransparentCopy(Dest, Source: TCustomBitmap; Width, Height: Integer);
var
  SrcImage, RawImg: TRawImage;
  Img, DeviceImg: TLazIntfImage;
  ImgHandle, MskHandle: HBitmap;
  R: TRect;
begin
  RawImg.Init;
  FillDescription(RawImg.Description, Width, Height);
  RawImg.DataSize := Width * Height * SizeOf(TRGBAQuad);
  RawImg.Data := AllocMem(RawImg.DataSize);
  
  if Source.MaskHandleAllocated then
    MskHandle := Source.MaskHandle
  else
    MskHandle := 0;

  R := Rect(0, 0, Source.Width, Source.Height);
  Widgetset.RawImage_FromBitmap(SrcImage, Source.Handle, MskHandle, @R);
  InternalSetImage(RawImg, SrcImage);

  // force output png with colorformat = 4
  // to do this we need halftone alpha
  if PRGBAQuad(RawImg.Data)[0].Alpha = 0 then
    PRGBAQuad(RawImg.Data)[0].Alpha := $01
  else
  if PRGBAQuad(RawImg.Data)[0].Alpha = $FF then
    PRGBAQuad(RawImg.Data)[0].Alpha := $FE;

  if not Widgetset.RawImage_CreateBitmaps(RawImg, ImgHandle, MskHandle, True)
  then begin
    Img := TLazIntfImage.Create(RawImg, False);
    DeviceImg := TLazIntfImage.Create(0, 0);
    DeviceImg.DataDescription := GetDescriptionFromDevice(0, Width, Height);
    DeviceImg.CopyPixels(Img);
    DeviceImg.GetRawImage(RawImg);
    Widgetset.RawImage_CreateBitmaps(RawImg, ImgHandle, MskHandle);
    DeviceImg.Free;
    Img.Free;
  end;

  Dest.SetHandles(ImgHandle, MskHandle);
  RawImg.FreeData;
end;

var
  Pixmap: TPixmap;
  Png: TPortableNetworkGraphic;
begin
  if (ParamCount = 2) or (ParamCount = 4) then
  begin
    Pixmap := TPixmap.Create;
    try
      Pixmap.LoadFromFile(ParamStr(1));
      Png := TPortableNetworkGraphic.Create;
      try
        if ParamCount > 2 then
        begin
          TransparentCopy(Png, Pixmap, StrToInt(ParamStr(3)), StrToInt(ParamStr(4)));
          if (Pixmap.Width > Png.Width) or (Pixmap.Height > Png.Height) then
            WriteLn(Format('WARNING: %s bigger than %d %d', [ParamStr(1), Png.Width, Png.Height]));
        end
        else
          TransparentCopy(Png, Pixmap, Pixmap.Width, Pixmap.Height);

        Png.SaveToFile(ParamStr(2));
      finally
        Png.Free;
      end;
    finally
      Pixmap.Free;
    end;
  end
  else
    WriteLn('Usage: '+ ExtractFileName(ParamStr(0)) +' <file1.xpm> <file2.png> [new_width new_height]')
end.

