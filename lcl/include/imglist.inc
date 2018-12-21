{%MainUnit ../imglist.pp}

{******************************************************************************
                                  TCustomImageList
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

type
  TImageListSignature = array[0..1] of char;
  TCustomIconAccess = class(TCustomIcon);

const
  SIG_LAZ1 = #1#0;
  SIG_LAZ2 = 'li';
  SIG_LAZ3 = 'Li';
  SIG_LAZ4 = 'Lz';
  SIG_D3   = 'IL';

const
  EffectMap: array[Boolean] of TGraphicsDrawEffect = (
    gdeDisabled,
    gdeNormal
  );

var
  GLCLGlyphs: TLCLGlyphs = nil;

function LCLGlyphs: TLCLGlyphs;
begin
  if GLCLGlyphs=nil then
  begin
    GLCLGlyphs := TLCLGlyphs.Create(nil);
    GLCLGlyphs.RegisterResolutions([16, 24, 32]);
  end;
  Result := GLCLGlyphs;
end;

function GetDefaultGlyph(ResourceName: string; ScalePercent: Integer;
  IgnoreMissingResource: Boolean): TCustomBitmap;
var
  LRes: TLResource;
begin
  if ScalePercent<>100 then
    ResourceName := ResourceName+'_'+IntToStr(ScalePercent);

  Result := CreateBitmapFromResourceName(HINSTANCE, ResourceName);
  if Result=nil then
  begin
    LRes := LazarusResources.Find(ResourceName);
    if LRes<>nil then
      Result := CreateBitmapFromLazarusResource(LRes);
  end;
  if (Result is TBitmap) and not Result.Transparent // BMP left-bottom transparency
  and (Result.Width>0) and (Result.Height>0) then
  begin
    Result.Transparent := True;
    Result.TransparentMode := tmFixed;
    Result.TransparentColor := Result.Canvas.Pixels[0, Result.Height-1];
  end;
  if (Result=nil) and not IgnoreMissingResource then
    raise EResNotFound.CreateFmt(SResNotFound,[ResourceName]);
end;

{------------------------------------------------------------------------------
  Method:  CopyImage
  Params:  Destination, Source: the destination/source canvas
  	       DestinationRect: the rectangle where the image is copied to 
  	       SourceRect: the rectangle containing the part to be copied 
  Returns: Nothing

  Internal routine to copy a rectangle from a source canvas to a rectangle on 
  the destination canvas
 ------------------------------------------------------------------------------}
procedure CopyImage(Destination, Source: TCanvas; DestinationRect, SourceRect: TRect);
begin
  Destination.CopyRect(
    DestinationRect,
    Source,
    SourceRect
  );
end;

{ TScaledImageListResolution }

class function TScaledImageListResolution.Create(
  AResolution: TCustomImageListResolution; const AScaleFactor: Double
  ): TScaledImageListResolution;
begin
  Result := Default(TScaledImageListResolution);
  Result.FResolution := AResolution;
  Result.FScaleFactor := AScaleFactor;
  if Assigned(AResolution) then
  begin
    Result.FWidth := Round(AResolution.Width / AScaleFactor);
    Result.FHeight := Round(AResolution.Height / AScaleFactor);
  end else
  begin
    Result.FWidth := 0;
    Result.FHeight := 0;
  end;
end;

procedure TScaledImageListResolution.Draw(ACanvas: TCanvas; AX, AY,
  AIndex: Integer; AEnabled: Boolean);
begin
  Draw(ACanvas, AX, AY, AIndex, EffectMap[AEnabled]);
end;

procedure TScaledImageListResolution.Draw(ACanvas: TCanvas; AX, AY,
  AIndex: Integer; ADrawEffect: TGraphicsDrawEffect);
begin
  Draw(ACanvas, AX, AY, AIndex, Resolution.FImageList.DrawingStyle, Resolution.FImageList.ImageType, ADrawEffect);
end;

procedure TScaledImageListResolution.Draw(ACanvas: TCanvas; AX, AY,
  AIndex: Integer; ADrawingStyle: TDrawingStyle; AImageType: TImageType;
  AEnabled: Boolean);
begin
  Draw(ACanvas, AX, AY, AIndex, ADrawingStyle, AImageType, EffectMap[AEnabled]);
end;

procedure TScaledImageListResolution.Draw(ACanvas: TCanvas; AX, AY,
  AIndex: Integer; ADrawingStyle: TDrawingStyle; AImageType: TImageType;
  ADrawEffect: TGraphicsDrawEffect);
begin
  if SameValue(FScaleFactor, 1) then
  begin
    Resolution.Draw(ACanvas, AX, AY, AIndex, ADrawingStyle, AImageType, ADrawEffect);
  end else
  begin
    Resolution.StretchDraw(ACanvas, AIndex,
      Rect(AX, AY, AX + Width, AY + Height),
      ADrawEffect<>gdeDisabled);
  end;
end;

procedure TScaledImageListResolution.StretchDraw(Canvas: TCanvas;
  Index: Integer; ARect: TRect; Enabled: Boolean);
begin
  Resolution.StretchDraw(Canvas, Index, ARect, Enabled);
end;

procedure TScaledImageListResolution.DrawOverlay(ACanvas: TCanvas; AX, AY,
  AIndex: Integer; AOverlay: TOverlay; AEnabled: Boolean);
begin
  DrawOverlay(ACanvas, AX, AY, AIndex, AOverlay, EffectMap[AEnabled]);
end;

procedure TScaledImageListResolution.DrawOverlay(ACanvas: TCanvas; AX, AY,
  AIndex: Integer; AOverlay: TOverlay; ADrawEffect: TGraphicsDrawEffect);
begin
  DrawOverlay(ACanvas, AX, AY, AIndex, AOverlay, Resolution.FImageList.DrawingStyle, Resolution.FImageList.ImageType, ADrawEffect);
end;

procedure TScaledImageListResolution.GetBitmap(Index: Integer;
  Image: TCustomBitmap);
begin
  GetBitmap(Index, Image, gdeNormal);
end;

procedure TScaledImageListResolution.GetBitmap(Index: Integer;
  Image: TCustomBitmap; AEffect: TGraphicsDrawEffect);
begin
  Resolution.GetBitmap(Index, Image, AEffect);
end;

function TScaledImageListResolution.GetCount: Integer;
begin
  Result := FResolution.Count;
end;

function TScaledImageListResolution.GetSize: TSize;
begin
  Result.cx := Width;
  Result.cy := Height;
end;

function TScaledImageListResolution.GetValid: Boolean;
begin
  Result := Assigned(FResolution);
end;

procedure TScaledImageListResolution.DrawOverlay(ACanvas: TCanvas; AX, AY,
  AIndex: Integer; AOverlay: TOverlay; ADrawingStyle: TDrawingStyle;
  AImageType: TImageType; ADrawEffect: TGraphicsDrawEffect);
var
  OverlayIndex: Integer;
begin
  OverlayIndex := Resolution.FImageList.FOverlays[AOverlay];

  if SameValue(FScaleFactor, 1) then
  begin
    Resolution.Draw(ACanvas, AX, AY, AIndex, ADrawingStyle, AImageType, ADrawEffect);
    if (OverlayIndex >= 0) and (OverlayIndex < Resolution.Count) then
      Resolution.Draw(ACanvas, AX, AY, OverlayIndex, ADrawingStyle, AImageType, ADrawEffect);
  end else
  begin
    Resolution.StretchDraw(ACanvas, AIndex,
      Rect(AX, AY, AX + Width, AY + Height),
      ADrawEffect<>gdeDisabled);
    if (OverlayIndex >= 0) and (OverlayIndex < Resolution.Count) then
      Resolution.StretchDraw(ACanvas, OverlayIndex,
        Rect(AX, AY, AX + Width, AY + Height),
        ADrawEffect<>gdeDisabled);
  end;
end;

{ TCustomImageListResolutionEnumerator }

constructor TCustomImageListResolutionEnumerator.Create(
  AImgList: TCustomImageList; ADesc: Boolean);
begin
  inherited Create;

  FImgList := AImgList;
  FDesc := ADesc;
  if ADesc then
    FCurrent := FImgList.FData.Count
  else
    FCurrent := -1;
end;

function TCustomImageListResolutionEnumerator.GetCurrent: TCustomImageListResolution;
begin
  Result := FImgList.FData[FCurrent];
end;

function TCustomImageListResolutionEnumerator.GetEnumerator: TCustomImageListResolutionEnumerator;
begin
  Result := Self;
end;

function TCustomImageListResolutionEnumerator.MoveNext: Boolean;
begin
  if FDesc then
  begin
    Dec(FCurrent);
    Result := FCurrent>=0;
  end else
  begin
    Inc(FCurrent);
    Result := FCurrent<FImgList.FData.Count;
  end;
end;

{ TCustomImageListResolution }

function TCustomImageListResolution.Add(Image, Mask: TCustomBitmap): Integer;
var
  ScBmp: TRGBAQuadArray;
begin
  if Image = nil then Exit(-1);

  Result := Count;
  TCustomImageList.ScaleImage(Image, Mask, Width, Height, ScBmp);
  InternalInsert(Result, @ScBmp[0]);
end;

procedure TCustomImageListResolution.AddImages(
  AValue: TCustomImageListResolution);
var
  n: Integer;
  p: PRGBAQuad;
  DataSize: Integer;
  OldCount: Integer;
  Bmp: TBitmap;
  ScBmp: TRGBAQuadArray;
begin
  if (AValue = nil) or (AValue=Self) or (AValue.FCount = 0) then exit;

  if (AValue.Width = Width) and (AValue.Height = Height)
  then begin
    AllocData(FCount + AValue.FCount);
    DataSize := FWidth * FHeight * SizeOf(FData[0]);
    System.Move(AVAlue.FData[0], FData[FCount], AValue.FCount * DataSize);
    OldCount := FCount;
    Inc(FCount, AValue.FCount);
    if HandleAllocated
    then begin
      p := @FData[OldCount];
      for n := OldCount to FCount - 1 do
      begin
        TWSCustomImageListResolutionClass(WidgetSetClass).Insert(Self, n, p);
        Inc(PByte(p), DataSize);
      end;
    end;
  end else
  begin
    for n := 0 to AValue.Count-1 do
    begin
      Bmp := TBitmap.Create;
      try
        AValue.GetBitmap(n, Bmp);
        TCustomImageList.ScaleImage(Bmp, nil, FWidth, FHeight, ScBmp);
        InternalInsert(n, @ScBmp[0]);
      finally
        Bmp.Free;
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageListResolution.AllocData
  Params:  ACount: the amount of images
  Returns: Nothing

  Allocates data for ACount images
 ------------------------------------------------------------------------------}
procedure TCustomImageListResolution.AllocData(ACount: Integer);
var
  n: Integer;
begin
  if FAllocCount >= ACount
  then Exit;

  // calculate number of blocks, add an extra block for the remainder.
  n := ACount mod FImageList.FAllocBy;
  if n <> 0
  then Inc(ACount, FImageList.FAllocBy - n);

  SetLength(FData, ACount * FWidth * FHeight * SizeOf(FData[0]));

  Inc(FAllocCount, ACount);
end;

procedure TCustomImageListResolution.CheckIndex(AIndex: Integer;
  AForInsert: Boolean);
  // aviod exceptionframe generation
  procedure Error;
  begin
    raise EInvalidOperation.Create(SInvalidIndex);
  end;
begin
  if AForInsert
  then begin
    if AIndex > FCount then Error;
  end
  else begin
    if AIndex >= FCount then Error;
  end;
end;

procedure TCustomImageListResolution.Clear;
begin
  if FCount = 0 then Exit;
  if HandleAllocated
  then TWSCustomImageListResolutionClass(WidgetSetClass).Clear(Self);
  SetLength(FData, 0);
  FAllocCount := 0;
end;

procedure TCustomImageListResolution.Delete(AIndex: Integer);
begin
  if AIndex = -1
  then begin
    Clear;
    Exit;
  end;

  CheckIndex(AIndex);

  InternalMove(AIndex, FCount - 1, True);
  Dec(FCount);
  if HandleAllocated
  then TWSCustomImageListResolutionClass(WidgetSetClass).Delete(Self, AIndex);
  // TODO: adjust allocated data
end;

destructor TCustomImageListResolution.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomImageListResolution.Draw(ACanvas: TCanvas; AX, AY,
  AIndex: Integer; AEnabled: Boolean);
begin
  Draw(ACanvas, AX, AY, AIndex, EffectMap[AEnabled]);
end;

procedure TCustomImageListResolution.Draw(ACanvas: TCanvas; AX, AY,
  AIndex: Integer; ADrawingStyle: TDrawingStyle; AImageType: TImageType;
  AEnabled: Boolean);
begin
  Draw(ACanvas, AX, AY, AIndex, ADrawingStyle, AImageType, EffectMap[AEnabled]);
end;

procedure TCustomImageListResolution.Draw(ACanvas: TCanvas; AX, AY,
  AIndex: Integer; ADrawingStyle: TDrawingStyle; AImageType: TImageType;
  ADrawEffect: TGraphicsDrawEffect);
begin
  if (AIndex < 0) or (AIndex >= FCount) then Exit;

  ReferenceNeeded;
  TWSCustomImageListResolutionClass(WidgetSetClass).Draw(Self, AIndex, ACanvas, Rect(AX, AY, FWidth, FHeight),
                          FImageList.BkColor, FImageList.BlendColor, ADrawEffect, ADrawingStyle, AImageType);
end;

procedure TCustomImageListResolution.Draw(ACanvas: TCanvas; AX, AY,
  AIndex: Integer; ADrawEffect: TGraphicsDrawEffect);
begin
  Draw(ACanvas, AX, AY, AIndex, FImageList.DrawingStyle, FImageList.ImageType, ADrawEffect);
end;

procedure TCustomImageListResolution.DrawOverlay(ACanvas: TCanvas; AX, AY,
  AIndex: Integer; AOverlay: TOverlay; AEnabled: Boolean);
begin
  DrawOverlay(ACanvas, AX, AY, AIndex, AOverlay, EffectMap[AEnabled]);
end;

procedure TCustomImageListResolution.DrawOverlay(ACanvas: TCanvas; AX, AY,
  AIndex: Integer; AOverlay: TOverlay; ADrawingStyle: TDrawingStyle;
  AImageType: TImageType; ADrawEffect: TGraphicsDrawEffect);
var
  OverlayIndex: Integer;
begin
  if (AIndex < 0) or (AIndex >= FCount) then Exit;

  ReferenceNeeded;
  TWSCustomImageListResolutionClass(WidgetSetClass).Draw(Self, AIndex, ACanvas, Rect(AX, AY, FWidth, FHeight),
                          FImageList.BkColor, FImageList.BlendColor, ADrawEffect, ADrawingStyle, AImageType);

  OverlayIndex := FImageList.FOverlays[AOverlay];
  if (OverlayIndex < 0) or (OverlayIndex >= FCount) then Exit;

  TWSCustomImageListResolutionClass(WidgetSetClass).Draw(Self, OverlayIndex, ACanvas, Rect(AX, AY, FWidth, FHeight),
                          clNone, FImageList.BlendColor, ADrawEffect, ADrawingStyle, AImageType);
end;

procedure TCustomImageListResolution.DrawOverlay(ACanvas: TCanvas; AX, AY,
  AIndex: Integer; AOverlay: TOverlay; ADrawEffect: TGraphicsDrawEffect);
begin
  DrawOverlay(ACanvas, AX, AY, AIndex, AOverlay, FImageList.DrawingStyle, FImageList.ImageType, ADrawEffect);
end;

procedure TCustomImageListResolution.FillDescription(out
  ADesc: TRawImageDescription);
begin
  ADesc.Init;
  ADesc.Format := ricfRGBA;
  ADesc.PaletteColorCount := 0;
  ADesc.MaskBitsPerPixel := 0;
  ADesc.Depth := 32;
  ADesc.Width := FWidth;
  ADesc.Height := FHeight;
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

procedure TCustomImageListResolution.GetBitmap(Index: Integer;
  Image: TCustomBitmap);
begin
  GetBitmap(Index, Image, gdeNormal);
end;

procedure TCustomImageListResolution.GetBitmap(Index: Integer;
  Image: TCustomBitmap; AEffect: TGraphicsDrawEffect);
var
  RawImg: TRawImage;
  ListImg, DeviceImg: TLazIntfImage;
  ImgHandle, MskHandle: HBitmap;
begin
  if (Index < 0) or (Index >= FCount) or (Image = nil) then Exit;

  GetRawImage(Index, RawImg);

  RawImg.PerformEffect(AEffect, True);

  MskHandle := 0;
  if not CreateCompatibleBitmaps(RawImg, ImgHandle, MskHandle, True)
  then begin
    // bummer, the widgetset doesn't support our 32bit format, try device
    ListImg := TLazIntfImage.Create(RawImg, False);
    DeviceImg := TLazIntfImage.Create(0,0,[]);
    DeviceImg.DataDescription := GetDescriptionFromDevice(0, FWidth, FHeight);
    DeviceImg.CopyPixels(ListImg);
    DeviceImg.GetRawImage(RawImg);
    RawImage_CreateBitmaps(RawImg, ImgHandle, MskHandle);
    DeviceImg.Free;
    ListImg.Free;
  end;
  Image.SetHandles(ImgHandle, MskHandle);

  RawImg.FreeData;
end;

procedure TCustomImageListResolution.GetFullBitmap(Image: TCustomBitmap;
  AEffect: TGraphicsDrawEffect);
var
  RawImg: TRawImage;
  ListImg, DeviceImg: TLazIntfImage;
  ImgHandle, MskHandle: HBitmap;
begin
  if (Count = 0) or (Image = nil) then Exit;

  GetFullRawImage(RawImg);

  RawImg.PerformEffect(AEffect, True);

  MskHandle := 0;
  if not CreateCompatibleBitmaps(RawImg, ImgHandle, MskHandle, True)
  then begin
    // bummer, the widgetset doesn't support our 32bit format, try device
    ListImg := TLazIntfImage.Create(RawImg, False);
    DeviceImg := TLazIntfImage.Create(0,0,[]);
    DeviceImg.DataDescription := GetDescriptionFromDevice(0, Width, Height * Count);
    DeviceImg.CopyPixels(ListImg);
    DeviceImg.GetRawImage(RawImg);
    RawImage_CreateBitmaps(RawImg, ImgHandle, MskHandle);
    DeviceImg.Free;
    ListImg.Free;
  end;
  Image.SetHandles(ImgHandle, MskHandle);

  RawImg.FreeData;
end;

procedure TCustomImageListResolution.GetFullRawImage(out Image: TRawImage);
begin
  Image.Init;

  if (FCount = 0) then Exit;
  FillDescription(Image.Description);
  Image.Description.Height := Height * Count;
  Image.DataSize := Width * Height * Count * SizeOf(FData[0]);
  Image.Data := PByte(FData);
end;

function TCustomImageListResolution.GetHotSpot: TPoint;
begin
  Result := Point(0, 0);
end;

procedure TCustomImageListResolution.GetIcon(Index: Integer; Image: TIcon);
begin
  GetIcon(Index, Image, gdeNormal);
end;

procedure TCustomImageListResolution.GetIcon(Index: Integer; Image: TIcon;
  AEffect: TGraphicsDrawEffect);
var
  RawImg: TRawImage;
  ListImg, DeviceImg: TLazIntfImage;
  IconInfo: TIconInfo;
begin
  if (Count = 0) or (Image = nil) then Exit;

  GetRawImage(Index, RawImg);
  RawImg.PerformEffect(AEffect, True);

  IconInfo.fIcon := True;
  IconInfo.hbmMask := 0;
  if not CreateCompatibleBitmaps(RawImg, IconInfo.hbmColor, IconInfo.hbmMask, True)
  then begin
    // bummer, the widgetset doesn't support our 32bit format, try device
    ListImg := TLazIntfImage.Create(RawImg, False);
    DeviceImg := TLazIntfImage.Create(0,0,[]);
    DeviceImg.DataDescription := GetDescriptionFromDevice(0, FWidth, FHeight);
    DeviceImg.CopyPixels(ListImg);
    DeviceImg.GetRawImage(RawImg);
    RawImage_CreateBitmaps(RawImg, IconInfo.hbmColor, IconInfo.hbmMask);
    DeviceImg.Free;
    ListImg.Free;
  end;
  Image.Handle := CreateIconIndirect(@IconInfo);

  RawImg.FreeData;
end;

procedure TCustomImageListResolution.GetRawImage(Index: Integer; out
  Image: TRawImage);
begin
  Image.Init;

  if (FCount = 0) then Exit;
  CheckIndex(Index);
  FillDescription(Image.Description);
  if Index >= 0 then
  begin
    Image.DataSize := FWidth * FHeight * SizeOf(FData[0]);
    Image.Data := @FData[Index * FWidth * FHeight];
  end;
end;

function TCustomImageListResolution.GetReference: TWSCustomImageListReference;
begin
  if not FReference.Allocated then ReferenceNeeded;
  Result := FReference;
end;

function TCustomImageListResolution.GetReferenceHandle: THandle;
begin
  Result := FReference.Handle;
end;

procedure TCustomImageListResolution.InternalInsert(AIndex: Integer;
  AData: PRGBAQuad);
var
  RawImg: TRawImage;
  R: TRect;
  ImgData: PRGBAQuad;
begin
  CheckIndex(AIndex, True);
  if (AIndex < 0) then
    AIndex := 0;

  Inc(FCount, 1);
  AllocData(FCount);
  if AIndex < FCount - 1 then
    InternalMove(FCount - 1, AIndex, True);

  ImgData := InternalSetData(AIndex, AData);
  if HandleAllocated
  then TWSCustomImageListResolutionClass(WidgetSetClass).Insert(Self, AIndex, ImgData);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageListResolution.InternalMove
  Params:  CurIndex: the index of the image to be moved
           NewIndex: the new index of the image
  Returns: Nothing

  Moves an image from the CurIndex'th location to NewIndex'th location
  without notifying the widgetset
 ------------------------------------------------------------------------------}
procedure TCustomImageListResolution.InternalMove(ACurIndex,
  ANewIndex: Cardinal; AIgnoreCurrent: Boolean);
var
  ImgSize, DataSize: Cardinal;
  p: Pointer;
begin
  ImgSize := FWidth * FHeight;
  DataSize := ImgSize * SizeOf(FData[0]);

  if not AIgnoreCurrent
  then begin
    // store current
    p := GetMem(DataSize);
    System.Move(FData[ACurIndex * ImgSize], p^, DataSize);
  end;

  // move all one up
  if ACurIndex < ANewIndex
  then System.Move(FData[(ACurIndex + 1) * ImgSize], FData[ACurIndex * ImgSize], DataSize * Cardinal(ANewIndex - ACurIndex))
  else System.Move(FData[ANewIndex * ImgSize], FData[(ANewIndex + 1) * ImgSize], DataSize * Cardinal(ACurIndex - ANewIndex));

  if not AIgnoreCurrent
  then begin
    // restore current
    System.Move(p^, FData[ANewIndex * ImgSize], DataSize);
    FreeMem(p);
  end;
end;

procedure TCustomImageListResolution.InternalReplace(AIndex: Integer;
  AData: PRGBAQuad);
var
  RawImage: TRawImage;
  R: TRect;
  ImgData: PRGBAQuad;
  TargetData: array of TRGBAQuad;
  LI: TLazIntfImage;
  X, Y: Integer;
begin
  if (AIndex < 0) then AIndex := 0;
  CheckIndex(AIndex);

  ImgData := InternalSetData(AIndex, AData);
  if HandleAllocated
  then TWSCustomImageListResolutionClass(WidgetSetClass).Replace(Self, AIndex, ImgData);
end;

function TCustomImageListResolution.InternalSetData(AIndex: Integer;
  AData: PRGBAQuad): PRGBAQuad;
begin
  Result := @FData[AIndex * FWidth * FHeight];
  Move(AData^, Result^, FWidth * FHeight * SizeOf(FData[0]));
end;

procedure TCustomImageListResolution.ReadData(AStream: TStream);
var
  Signature: TImageListSignature;
  StreamPos: TStreamSeekType;

  procedure DoReadLaz1;
  var
    i, NewCount, Size: Integer;
    bmp: TBitmap;
  begin
    // provided for compatability for earlier lazarus streams
    NewCount := AStream.ReadWord;
    for i := 0 to NewCount - 1 do
    begin
      bmp := TBitMap.Create;
      Size:=ReadLRSInteger(AStream);
      bmp.LoadFromStream(AStream, Size);
      bmp.Transparent := True;
      Add(bmp, nil);
      bmp.Free;
    end;
  end;

  procedure DoReadLaz2;
  var
    i, NewCount, Size: cardinal;
    bmp: TCustomBitmap;
    Sig: array[0..1] of char;
  begin
    NewCount := ReadLRSCardinal(AStream);
    FWidth := ReadLRSCardinal(AStream);
    FHeight := ReadLRSCardinal(AStream);
    FImageList.FWidth := FWidth;
    FImageList.FHeight := FHeight;
    for i := 0 to NewCount - 1 do
    begin
      Size := ReadLRSCardinal(AStream);
      bmp := nil;
      // Before our TBitmap can have bpm, xpm, png or other content
      // We need to look at signature before loading
      if Size > 2 then
      begin
        AStream.Read(Sig[0], 2);
        if Sig = 'BM' then
          bmp := TBitmap.Create
        else
        if Sig = '/*' then
          bmp := TPixmap.Create
        else
        if Sig = '%P' then
          bmp := TPortableNetworkGraphic.Create
        else
          raise EInvalidGraphicOperation.Create(rsInvalidStreamFormat);
        AStream.Position := AStream.Position - 2;
      end;
      bmp.LoadFromStream(AStream, Size);
      Add(bmp, nil);
      bmp.Free;
    end;
  end;

  procedure DoReadLaz3Header(BStream: TStream);
  begin
    FCount := ReadLRSCardinal(BStream);
    FWidth := ReadLRSCardinal(BStream);
    FHeight := ReadLRSCardinal(BStream);
    AllocData(FCount);
  end;

  procedure DoReadLaz3Data(BStream: TStream);
  begin
    BStream.ReadBuffer(FData[0], FWidth * FHeight * FCount *  SizeOf(FData[0])) ;
  end;

  procedure DoReadLaz3;
  begin
    DoReadLaz3Header(AStream);
    if FCount > 0 then
      DoReadLaz3Data(AStream);
  end;

  procedure DoReadLaz4;
  var
    CompressedSize: Int64;
    I: TMemoryStream = nil;
    D: TDecompressionStream = nil;
  begin
    DoReadLaz3Header(AStream);
    if FCount=0 then
      Exit;
    CompressedSize := ReadLRSInt64(AStream);

    try
      I := TMemoryStream.Create;
      I.CopyFrom(AStream, CompressedSize);
      I.Position := 0;
      D := TDecompressionStream.Create(I);
      DoReadLaz3Data(D);
    finally
      I.Free;
      D.Free;
    end;
  end;

  procedure CreateImagesFromRawImage(IntfImage: TLazIntfImage;
    NewCount: integer);
  var
    RawImage, SubRawImage: TRawImage;
    ImgHandle, MaskHandle: HBitmap;
    Row: Integer;
    Col: Integer;
    ImgRect: TRect;
    Res: Boolean;
    ImgData: TRGBAQuadArray;
  begin
    IntfImage.GetRawImage(RawImage);
    SubRawImage.Init;

    for Row := 0 to (IntfImage.Height div Height) - 1 do
    begin
      if NewCount <= 0 then Break;
      for Col := 0 to (IntfImage.Width div Width) - 1 do
      begin
        if NewCount <= 0 then Break;

        ImgRect := Bounds(Col*Width,Row*Height,Width,Height);
        RawImage.ExtractRect(ImgRect, SubRawImage);
        Res := RawImage_CreateBitmaps(SubRawImage, ImgHandle, MaskHandle);
        SubRawImage.FreeData;
        if not Res
        then raise EInvalidGraphicOperation.Create('TCustomImageList.CreateImagesFromRawImage Create bitmaps');

        TCustomImageList.ScaleImage(ImgHandle, MaskHandle, Rect(0, 0, Width, Height), Width, Height, ImgData);
        InternalInsert(Count, @ImgData[0]);
        DeleteObject(ImgHandle);
        DeleteObject(MaskHandle);
        //DebugLn('CreateImagesFromRawImage B ',Img.Width,',',Img.Height,' ',Count);
        Dec(NewCount);
      end;
    end;
  end;

  procedure ReadDelphiImageAndMask(HasMask: boolean; NewCount: integer);
  var
    IntfImage: TLazIntfImage;
    ImgReader: TFPReaderBMP;
    MaskIntfImage: TLazIntfImageMask;
  begin
    IntfImage:=nil;
    MaskIntfImage:=nil;
    ImgReader:=nil;
    try
      IntfImage:=TLazIntfImage.Create(0,0,[]);
      IntfImage.DataDescription := GetDescriptionFromDevice(0, 0, 0);
      // read the image bmp stream into the IntfImage
      ImgReader:=TFPReaderBMP.Create;
      IntfImage.LoadFromStream(AStream,ImgReader);
      if HasMask then begin
        // create the mask bmp directly into the RawImage
        MaskIntfImage:=TLazIntfImageMask.CreateWithImage(IntfImage);
        MaskIntfImage.LoadFromStream(AStream,ImgReader);
      end;

      CreateImagesFromRawImage(IntfImage,NewCount);
    finally
      // clean up
      ImgReader.Free;
      IntfImage.Free;
      MaskIntfImage.Free;
    end;
  end;

  {$IFDEF SaveDelphiImgListStream}
  procedure SaveImgListStreamToFile;
  var
    CurStreamPos: TStreamSeekType;
    fs: TFileStream;
    i: Integer;
    Filename: string;
  begin
    i:=0;
    repeat
      inc(i);
      Filename:='TCustomImageList'+IntToStr(i)+'.stream';
    until not FileExistsUTF8(Filename);
    CurStreamPos := AStream.Position;
    DebugLn('TCustomImageList.ReadData Saving stream to ',Filename);
    fs:=TFileStream.Create(UTF8ToSys(Filename),fmCreate);
    AStream.Position:=StreamPos;
    fs.CopyFrom(AStream,AStream.Size-AStream.Position);
    fs.Free;
    AStream.Position:=CurStreamPos;
  end;
  {$ENDIF}

var
  HasMask: Boolean;
  NewCount: Integer;
  Size: integer;
begin
  Clear;

  StreamPos := AStream.Position;                // check stream signature
  AStream.Read(Signature, SizeOf(Signature));

  if Signature = SIG_LAZ4
  then begin
    DoReadLaz4;
    Exit;
  end;

  if Signature = SIG_LAZ3
  then begin
    DoReadLaz3;
    Exit;
  end;

  if Signature = SIG_LAZ2
  then begin
    DoReadLaz2;
    Exit;
  end;

  if Signature = SIG_LAZ1
  then begin
    DoReadLaz1;
    Exit;
  end;

  // Delphi streams

  {$IFDEF SaveDelphiImgListStream}
  SaveImgListStreamToFile;
  {$ENDIF}

  if Signature = SIG_D3
  then begin
    AStream.ReadWord; //Skip ?
    NewCount := ReadLRSWord(AStream);
    //DebugLn('NewCount=',NewCount);
    AStream.ReadWord; //Skip Capacity
    AStream.ReadWord; //Skip Grow
    FWidth := ReadLRSWord(AStream);
    //DebugLn('NewWidth=',FWidth);
    FHeight := ReadLRSWord(AStream);
    //DebugLn('NewHeight=',FHeight);
    FImageList.FBKColor := TColor(ReadLRSInteger(AStream));
    // corrent colors - they are stored in windows values
    if TColorRef(FImageList.FBKColor) = CLR_NONE then
      FImageList.FBKColor := clNone
    else
    if TColorRef(FImageList.FBKColor) = CLR_DEFAULT then
      FImageList.FBKColor := clDefault;
    HasMask := (ReadLRSWord(AStream) and 1) = 1;
    AStream.ReadDWord; //Skip ?
    AStream.ReadDWord; //Skip ?

    ReadDelphiImageAndMask(HasMask,NewCount);
  end
  else begin
    // D2 has no signature, so restore original position
    AStream.Position := StreamPos;
    Size:=ReadLRSInteger(AStream);
    NewCount:=ReadLRSInteger(AStream);

    ReadDelphiImageAndMask(false,NewCount);
    AStream.Position := StreamPos+Size;
  end;
end;

procedure TCustomImageListResolution.ReferenceDestroying;
begin
  inherited ReferenceDestroying;

  FImageList.DoDestroyResolutionReference(FWidth, Reference._Handle);
end;

procedure TCustomImageListResolution.StretchDraw(Canvas: TCanvas;
  Index: Integer; ARect: TRect; Enabled: Boolean);
var
  Bmp: TBitmap;
begin
  if ((ARect.Right-ARect.Left)=FWidth) and ((ARect.Bottom-ARect.Top)=FHeight) then
    Draw(Canvas, ARect.Left, ARect.Top, Index, Enabled)
  else
  begin
    Bmp := TBitmap.Create;
    try
      GetBitmap(Index, Bmp, EffectMap[Enabled]);
      Canvas.StretchDraw(ARect, Bmp);
    finally
      Bmp.Free;
    end;
  end;
end;

procedure TCustomImageListResolution.WriteData(AStream: TStream;
  const ACompress: Boolean);

  procedure DoWriteHeader(BStream: TStream);
  begin
    //Count of image
    WriteLRSInteger(BStream,Count);
    WriteLRSInteger(BStream,Width);
    WriteLRSInteger(BStream,Height);
  end;

  procedure DoWriteRawData(BStream: TStream);
  begin
    BStream.Write(FData[0], FWidth * FHeight * FCount * SizeOf(FData[0]));
  end;

  procedure DoWriteCompressedData(BStream: TStream);
  var
    I: TMemoryStream = nil;
    O: TMemoryStream = nil;
    C: TCompressionStream = nil;
  begin
    try
      I := TMemoryStream.Create;
      O := TMemoryStream.Create;
      DoWriteRawData(I);
      I.Position := 0;
      C := TCompressionStream.Create(clMax, O);
      C.CopyFrom(I, I.Size);
      FreeAndNil(C);
      O.Position := 0;
      WriteLRSInt64(AStream, O.Size); // compressed size
      AStream.CopyFrom(O, O.Size);
    finally
      I.Free;
      O.Free;
      C.Free;
    end;
  end;
var
  Signature: TImageListSignature;
begin
  if not ACompress then
  begin
    //Write signature
    Signature:=SIG_LAZ3;
    AStream.Write(Signature,SizeOf(Signature));

    DoWriteHeader(AStream);
    if Count > 0 then
      DoWriteRawData(AStream);
  end else
  begin
    //Write signature
    Signature:=SIG_LAZ4;
    AStream.Write(Signature,SizeOf(Signature));

    DoWriteHeader(AStream);
    if Count > 0 then
      DoWriteCompressedData(AStream);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageListResolution.WSCreateHandle
  Params:  AParams: ignored
  Returns: Handle to created imagelist

  Instructs the widgtset to create an imagelist
 ------------------------------------------------------------------------------}
function TCustomImageListResolution.WSCreateReference(AParams: TCreateParams): PWSReference;
var
  ilc: TWSCustomImageListResolutionClass;
  dt: PRGBAQuad;
begin
  ilc := TWSCustomImageListResolutionClass(WidgetSetClass);
  if FCount = 0 then
    dt := nil
  else
    dt := @FData[0];
  FReference := ilc.CreateReference(Self, FCount, FImageList.FAllocBy, FWidth, FHeight, dt);
  Result := @FReference;
end;

class procedure TCustomImageListResolution.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomImageListResolution;
end;

{ TCustomImageListResolutions }

constructor TCustomImageListResolutions.Create(
  const AImageList: TCustomImageList;
  const AResolutionClass: TCustomImageListResolutionClass);
begin
  inherited Create;
  FList := TObjectList.Create(True);
  FImageList := AImageList;
  FResolutionClass := AResolutionClass;
end;

procedure TCustomImageListResolutions.Clear;
begin
  FList.Clear;
end;

procedure TCustomImageListResolutions.Delete(const AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

destructor TCustomImageListResolutions.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TCustomImageListResolutions.Find(const AImageWidth: Integer; out
  Index: Integer): Boolean;
var
  L, R, I: Integer;
  CompareRes: PtrInt;
begin
  Result := false;
  Index := -1;
  L := 0;
  R := Count - 1;
  while (L<=R) do
  begin
    I := L + (R - L) div 2;
    CompareRes := CompareValue(AImageWidth, Items[I].Width);
    if (CompareRes>0) then
      L := I+1
    else begin
      R := I-1;
      if (CompareRes=0) then begin
         Result := true;
         L := I; // forces end of while loop
      end;
    end;
  end;
  Index := L;
end;

function TCustomImageListResolutions.FindBestToCopyFrom(const ATargetWidth,
  AIgnoreIndex: Integer): Integer;
begin
  for Result := 0 to Count-1 do // find the smallest (but bigger) image
    if (Result<>AIgnoreIndex) and (Items[Result].Width>=ATargetWidth) then
      Exit;

  Result := Count-1; // just pickup the biggest image to scale up
  if Result=AIgnoreIndex then
    Dec(Result);
end;

function TCustomImageListResolutions.FindBestToScaleFrom(
  const ATargetWidth: Integer): Integer;
begin
  if Find(ATargetWidth, Result) then
    Exit;

  Result := FindBestToCopyFrom(ATargetWidth, -1);
end;

function TCustomImageListResolutions.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCustomImageListResolutions.GetImageLists(
  const AImageWidth: Integer): TCustomImageListResolution;
begin
  Result := GetImageLists(AImageWidth, True, csDesigning in FImageList.ComponentState);
end;

function TCustomImageListResolutions.GetImageLists(const AImageWidth: Integer;
  const AScaleFromExisting,
  AutoCreatedInDesignTime: Boolean): TCustomImageListResolution;
var
  I, L: Integer;
begin
  if Find(AImageWidth, I) then
    Result := Items[I]
  else
  begin
    Result := FResolutionClass.Create(nil);
    FList.Insert(I, Result);
    Result.FImageList := FImageList;
    Result.FWidth := AImageWidth;
    if FImageList.Width<>0 then
      Result.FHeight := FImageList.GetHeightForWidth(AImageWidth)
    else
      Result.FHeight := 0;
    Result.FAutoCreatedInDesignTime := AutoCreatedInDesignTime and (AImageWidth<>FImageList.Width);
    if AScaleFromExisting then
    begin
      L := FindBestToCopyFrom(AImageWidth, I);
      if L>=0 then
        Result.AddImages(Items[L]);
    end;
  end;
end;

function TCustomImageListResolutions.GetItems(
  const AIndex: Integer): TCustomImageListResolution;
begin
  Result := TCustomImageListResolution(FList[AIndex]);
end;

{ TCustomImageList }

{------------------------------------------------------------------------------
  Function: TCustomImageList.Add
  Params:   Image: a bitmap image
            Mask: a bitmap which defines the transparent parts of Image
  Returns:  The index of the added image, -1 if unsuccesful.

  Adds one or more (bitmap width / imagelist width) bitmaps to the list.
  If Mask is nil, the image has no transparent parts.

  The image is copied. To add it directly use AddDirect.
 ------------------------------------------------------------------------------}

function TCustomImageList.Add(Image, Mask: TCustomBitmap): Integer;
begin
  Result := Count;
  Insert(Result, Image, Mask);
end;

{------------------------------------------------------------------------------
  Function: TCustomImageList.AddIcon
  Params:   Image: the Icon to be added;
  Returns:  The index of the added icon, -1 if unsuccesfull.

  Adds an icon to the list.
 ------------------------------------------------------------------------------}
function TCustomImageList.AddIcon(Image: TCustomIcon): Integer;
begin
  Result := Count;
  InsertIcon(Result, Image);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.AddImages
  Params:  Value: An imagelist containing images to be added
  Returns: Nothing

  Adds images from another imagelist to the list.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.AddImages(AValue: TCustomImageList);
var
  DataSize: Integer;
  OldCount, I, R: Integer;
  FromR: TCustomImageListResolution;
  First: Boolean;
  Bmp: TBitmap;
  RawImage: TRawImage;
begin
  if (AValue = nil) or (AValue=Self) or (AValue.Count = 0) then exit;

  OldCount := Count;
  First := True;
  Bmp := TBitmap.Create;
  try
    for FromR in AValue.ResolutionsDesc do
    begin
      if First then
      begin // first we assign the biggest images
        for I := 0 to AValue.Count-1 do
        begin
          FromR.GetBitmap(I, Bmp);
          Add(Bmp, nil);
        end;
        First := False;
      end else // then we assign compatible images
      if FData.Find(FromR.Width, R) and (FData[R].Height=FromR.Height) then
      begin
        for I := 0 to AValue.Count-1 do
        begin
          FromR.GetRawImage(I, RawImage);
          FData[R].InternalReplace(OldCount+I, PRGBAQuad(RawImage.Data));
        end;
      end;
    end;
  finally
    Bmp.Free;
  end;
end;

{------------------------------------------------------------------------------
  Function: TCustomImageList.AddMasked
  Params:   Image: A bitmap to be added
            MaskColor: The color acting as transparant color
  Returns:  The index of the added icon, -1 if unsuccesfull.

  Adds one or more (bitmap width / imagelist width) bitmaps to the list. 
  Every occurrence of MaskColor will be converted to transparent.
 ------------------------------------------------------------------------------}
function TCustomImageList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  try
    Result := Count;
    InsertMasked(Result, Image, MaskColor);
  except
    on E: Exception do
    begin
      DebugLn('TCustomImageList.AddMasked ',E.Message);
      Result := -1; // Ignore exceptions, just return -1
    end;
  end;
end;

function TCustomImageList.AddMultipleResolutions(
  Images: array of TCustomBitmap): Integer;

  function FindImage(const ATargetWidth: Integer): TCustomBitmap;
  begin
    for Result in Images do // find the smallest (but bigger) image
    begin
      if (Result.Width>=ATargetWidth) then
        Exit;
    end;

    Result := Images[High(Images)]; // just pickup the biggest image to scale up
  end;
var
  R: TCustomImageListResolution;
  ScBmp: TRGBAQuadArray;
  Image: TCustomBitmap;
begin
  Result := Count;
  CreateDefaultResolution;
  for R in Resolutions do
  begin
    Image := FindImage(R.Width);
    ScaleImage(Image, nil, R.Width, R.Height, ScBmp);
    R.InternalInsert(Result, @ScBmp[0]);
  end;
end;

{------------------------------------------------------------------------------
  function TCustomImageList.AddLazarusResource(const ResourceName: string
    ): integer;
    
  Load TBitmap from lazarus resources and add it.
 ------------------------------------------------------------------------------}
function TCustomImageList.AddLazarusResource(const ResourceName: string; MaskColor: TColor): integer;
var
  Bmp: TCustomBitmap;
begin
  Bmp := CreateBitmapFromLazarusResource(ResourceName);
  if MaskColor <> clNone then
  begin
    Bmp.TransparentColor := MaskColor;
    Bmp.Transparent := True;
  end;
  Result := Add(Bmp, nil);
  Bmp.Free;
end;

function TCustomImageList.AddResourceName(Instance: THandle; const ResourceName: string; MaskColor: TColor): integer;
var
  Grp: TGraphic;
  Bmp: TCustomBitmap absolute Grp;
begin
  Grp := CreateGraphicFromResourceName(Instance, ResourceName);
  if Grp is TCustomBitmap then
  begin
    if MaskColor <> clNone then
    begin
      Bmp.TransparentColor := MaskColor;
      Bmp.Transparent := True;
    end;
    Result := Add(Bmp, nil);
  end
  else
    Result := AddIcon(Grp as TCustomIcon);
  Grp.Free;
end;

function TCustomImageList.AddSlice(Image: TCustomBitmap; AImageRect: TRect
  ): Integer;
var
  R: TCustomImageListResolution;
  ScBmp: TRGBAQuadArray;
begin
  if Image = nil then Exit(-1);

  Result := Count;

  CreateDefaultResolution;
  for R in Resolutions do
  begin
    ScaleImage(Image, nil, AImageRect, R.Width, R.Height, ScBmp);
    R.InternalInsert(Result, @ScBmp[0]);
  end;
end;

function TCustomImageList.AddSliceCentered(Image: TCustomBitmap): Integer;
var
  ImageRect: TRect;
begin
  ImageRect := Rect(0, 0, Width, Height);
  OffsetRect(ImageRect, (Image.Width-Width) div 2, (Image.Height-Height) div 2);
  Result := AddSlice(Image, ImageRect);
end;

function TCustomImageList.AddSliced(Image: TCustomBitmap; AHorizontalCount,
  AVerticalCount: Integer): Integer;
var
  R: TCustomImageListResolution;
  ScBmp: TRGBAQuadArray;
  W, H, I, L, C: Integer;
  Rc: TRect;
begin
  if Image = nil then Exit(-1);

  W := Image.Width div AHorizontalCount;
  H := Image.Height div AVerticalCount;

  C := Count;
  Result := Count;

  CreateDefaultResolution;
  for L := 0 to AVerticalCount-1 do
    for I := 0 to AHorizontalCount-1 do
    begin
      Rc := Rect(I*W, L*H, (I+1)*W, (L+1)*H);
      for R in Resolutions do
      begin
        ScaleImage(Image, nil, Rc, R.Width, R.Height, ScBmp);
        R.InternalInsert(C, @ScBmp[0]);
      end;
      Inc(C);
    end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Assign
  Params:  Source: Source data
  Returns: Nothing

  Very simple assign with stream exchange
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Assign(Source: TPersistent);
Var
  ImgSrc : TCustomImageList;
  FromR, ToR: TCustomImageListResolution;
  DataSize: Integer;
begin
  if (Source=Self) then exit;
  if Source is TCustomImageList then
  begin
    ImgSrc := TCustomImageList(Source);
    BeginUpdate;
    try
      SetWidthHeight(ImgSrc.Width,ImgSrc.Height);
      Clear;

      for FromR in ImgSrc.Resolutions do
      begin
        ToR := FData.GetImageLists(FromR.Width, False, FromR.AutoCreatedInDesignTime);
        ToR.FWidth := FromR.FWidth;
        ToR.FCount := FromR.FCount;
        ToR.AllocData(ToR.FCount);
        if ToR.FCount>0 then
        begin
          DataSize := ToR.FWidth * ToR.FHeight * SizeOf(FData[0]);
          System.Move(FromR.FData[0], ToR.FData[0], ToR.FCount * DataSize);
        end;
      end;
    finally
      EndUpdate;
    end;
  end
  else inherited Assign(Source);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.AssignTo
  Params:  Dest: the destination to assign to
  Returns: Nothing

  Very simple assign with stream exchange
 ------------------------------------------------------------------------------}
procedure TCustomImageList.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomImageList then
    TCustomImageList(Dest).Assign(Self)
  else
    inherited AssignTo(Dest);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.BeginUpdate
  Params:  None
  Returns: Nothing

  Lock the change event for updating.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.BeginUpdate;
begin
  inc(FUpdateCount);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Change
  Params:  None
  Returns: Nothing

  Fires the change event.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Change;
begin
  if (not FChanged) or (FUpdateCount > 0) then exit;
  NotifyChangeLink;
  if Assigned(FOnChange) then FOnChange(Self);
  FChanged := false;
end;

procedure TCustomImageList.CheckIndex(AIndex: Integer; AForInsert: Boolean);
  // aviod exceptionframe generation
  procedure Error;
  begin
    raise EInvalidOperation.Create(SInvalidIndex);
  end;
begin
  if AForInsert
  then begin
    if AIndex > Count then Error;
  end
  else begin
    if AIndex >= Count then Error;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Clear
  Params:  None
  Returns: Nothing

  Clears the list.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Clear;
var
  R: TCustomImageListResolution;
begin
  FData.Clear;

  ClearOverlays;
  FChanged := True;
  Change;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCustomImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeight := 16;
  FWidth := 16;

  Initialize;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.CreateSize
  Params:  AHeight: The height of an image 
           AWidth: The width of an image
  Returns: Nothing

  Runtime constructor for the class with a given width and height.
 ------------------------------------------------------------------------------}
{.$ifdef IMGLIST_KEEP_EXTRA}
constructor TCustomImageList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited Create(nil);
  FHeight := AHeight;
  FWidth := AWidth;
  Initialize;
end;
{.$endif}


{------------------------------------------------------------------------------
  Method:  TCustomImageList.DefineProperties
  Params:  Filer: A filer for our properties
  Returns: Nothing

  Defines the images
 ------------------------------------------------------------------------------}
procedure TCustomImageList.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if (Filer.Ancestor <> nil) and (Filer.Ancestor is TCustomImageList) then
      Result :=  not Equals(Filer.Ancestor)
    else
      Result := Count > 0;
  end;

  function NeedsBitmapAdv: Boolean;
  var
    R: TCustomImageListResolution;
  begin
    for R in Resolutions do
      if not R.AutoCreatedInDesignTime and (R.Width<>Width) then
        Exit(True);
    Result := False;
  end;

var
  ADoWrite: Boolean;
begin
  inherited DefineProperties(Filer);
  ADoWrite := DoWrite;
  Filer.DefineBinaryProperty('Bitmap', @ReadData, @WriteData, ADoWrite);
  Filer.DefineBinaryProperty('BitmapAdv', @ReadAdvData, @WriteAdvData, ADoWrite and NeedsBitmapAdv);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Delete
  Params:  Index: the index of the image to be deleted.
  Returns: Nothing

  Deletes the image identified by Index. An index of -1 deletes all
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Delete(AIndex: Integer);
var
  R: TCustomImageListResolution;
begin
  for R in Resolutions do
    R.Delete(AIndex);

  FChanged := true;
  Change;
end;

procedure TCustomImageList.DeleteResolution(const AWidth: Integer);
var
  I: Integer;
begin
  if FData.Find(AWidth, I) then
    FData.Delete(I);
end;

{------------------------------------------------------------------------------
  Method: TCustomImageList.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCustomImageList.Destroy;
begin
  FData.Free;
  inherited Destroy;
  while FChangeLinkList.Count>0 do
    UnregisterChanges(TChangeLink(FChangeLinkList[0]));
  FreeThenNil(FChangeLinkList);
end;

procedure TCustomImageList.DoDestroyResolutionReference(const AWidth: Integer;
  AResolutionReference: TLCLHandle);
var
  I: Integer;
begin
  if FChangeLinkList=nil then
    Exit;
  for I := 0 to FChangeLinkList.Count-1 do
    if TChangeLink(FChangeLinkList[I]).Sender = Self then
      TChangeLink(FChangeLinkList[I]).DoDestroyResolutionReference(AWidth, AResolutionReference);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Draw
  Params:  Canvas: the canvas to draw on
           X, Y: co-ordinates of the top, left corner of thetarget location 
           Index: index of the image to be drawn
           Enabled: True, draws the image
                    False, draws the image disabled (embossed)
  Returns: Nothing

  Draws the requested image on the given canvas.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer;
  AEnabled: Boolean);
begin
  Draw(ACanvas, AX, AY, AIndex, EffectMap[AEnabled]);
end;

procedure TCustomImageList.Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer;
  ADrawEffect: TGraphicsDrawEffect);
begin
  Draw(ACanvas, AX, AY, AIndex, DrawingStyle, ImageType, ADrawEffect);
end;

procedure TCustomImageList.Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer;
  ADrawingStyle: TDrawingStyle; AImageType: TImageType; AEnabled: Boolean);
begin
  Draw(ACanvas, AX, AY, AIndex, ADrawingStyle, AImageType, EffectMap[AEnabled]);
end;

procedure TCustomImageList.Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer;
  ADrawingStyle: TDrawingStyle; AImageType: TImageType;
  ADrawEffect: TGraphicsDrawEffect);
begin
  GetResolution(FWidth).Draw(ACanvas, AX, AY, AIndex, ADrawingStyle, AImageType, ADrawEffect);
end;

procedure TCustomImageList.DrawForPPI(ACanvas: TCanvas; AX, AY, AIndex: Integer;
  AImageWidthAt96PPI, ATargetPPI: Integer; ACanvasFactor: Double;
  AEnabled: Boolean);
begin
  DrawForPPI(ACanvas, AX, AY, AIndex, AImageWidthAt96PPI, ATargetPPI, ACanvasFactor, EffectMap[AEnabled]);
end;

procedure TCustomImageList.DrawForPPI(ACanvas: TCanvas; AX, AY, AIndex: Integer;
  AImageWidthAt96PPI, ATargetPPI: Integer; ACanvasFactor: Double;
  ADrawEffect: TGraphicsDrawEffect);
var
  R: TScaledImageListResolution;
begin
  R := GetResolutionForPPI(AImageWidthAt96PPI, ATargetPPI, ACanvasFactor);
  R.Draw(ACanvas, AX, AY, AIndex, ADrawEffect);
end;

procedure TCustomImageList.DrawOverlay(ACanvas: TCanvas; AX, AY, AIndex: Integer;
  AOverlay: TOverlay; AEnabled: Boolean = True);
begin
  DrawOverlay(ACanvas, AX, AY, AIndex, AOverlay, EffectMap[AEnabled]);
end;

procedure TCustomImageList.DrawOverlay(ACanvas: TCanvas; AX, AY, AIndex: Integer;
  AOverlay: TOverlay; ADrawEffect: TGraphicsDrawEffect);
begin
  DrawOverlay(ACanvas, AX, AY, AIndex, AOverlay, DrawingStyle, ImageType, ADrawEffect);
end;

procedure TCustomImageList.DrawOverlay(ACanvas: TCanvas; AX, AY, AIndex: Integer;
  AOverlay: TOverlay; ADrawingStyle: TDrawingStyle; AImageType: TImageType;
  ADrawEffect: TGraphicsDrawEffect);
begin
  GetResolution(FWidth).DrawOverlay(ACanvas, AX, AY, AIndex, AOverlay, ADrawingStyle, AImageType, ADrawEffect);
end;


{------------------------------------------------------------------------------
  Method:  TCustomImageList.EndUpdate
  Params:  none
  Returns: Nothing

  Decrements te update lock. When zero, changes are notified when necesary
 ------------------------------------------------------------------------------}
procedure TCustomImageList.EndUpdate;
begin
  if FUpdateCount<=0 then
    RaiseGDBException('');
  dec(FUpdateCount);
  Change;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.GetBitmap
  Params:  Index: the index of the requested image
           Image: a bitmap as a container for the bitmap
  Returns: Nothing

  Creates a copy of the index'th image.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.GetBitmap(Index: Integer; Image: TCustomBitmap);
begin
  GetBitmap(Index, Image, gdeNormal);
end;

procedure TCustomImageList.GetFullBitmap(Image: TCustomBitmap; AEffect: TGraphicsDrawEffect = gdeNormal);
begin
  GetResolution(FWidth).GetFullBitmap(Image, AEffect);
end;

procedure TCustomImageList.GetBitmap(Index: Integer; Image: TCustomBitmap;
  AEffect: TGraphicsDrawEffect);
begin
  GetResolution(FWidth).GetBitmap(Index, Image, AEffect);
end;

function TCustomImageList.GetCount: Integer;
begin
  if FData.Count=0 then
    Exit(0);
  Result := GetResolution(FWidth).Count;
end;

procedure TCustomImageList.GetFullRawImage(out Image: TRawImage);
begin
  GetResolution(FWidth).GetFullRawImage(Image);
end;

function TCustomImageList.GetHeightForPPI(AImageWidth,
  APPI: Integer): Integer;
var
  I: Integer;
begin
  AImageWidth := GetWidthForPPI(AImageWidth, APPI);
  if FData.Find(AImageWidth, I) then
    Result := FData[I].Height
  else
    Result := GetHeightForWidth(AImageWidth)
end;

function TCustomImageList.GetHeightForWidth(AWidth: Integer): Integer;
begin
  if FWidth<>0 then
    Result := AWidth * FHeight div FWidth
  else
    Result := 0;
end;

procedure TCustomImageList.GetIcon(Index: Integer; Image: TIcon; AEffect: TGraphicsDrawEffect);
begin
  GetResolution(FWidth).GetIcon(Index, Image, AEffect);
end;

procedure TCustomImageList.GetIcon(Index: Integer; Image: TIcon);
begin
  GetIcon(Index, Image, gdeNormal);
end;

procedure TCustomImageList.GetRawImage(Index: Integer; out Image: TRawImage);
begin
  GetResolution(FWidth).GetRawImage(Index, Image);
end;

function TCustomImageList.GetReference(
  AImageWidth: Integer): TWSCustomImageListReference;
begin
  Result := GetResolution(AImageWidth).Reference;
end;

function TCustomImageList.GetReferenceForPPI(AImageWidth,
  APPI: Integer): TWSCustomImageListReference;
begin
  Result := GetReference(GetWidthForPPI(AImageWidth, APPI));
end;

function TCustomImageList.GetResolutionForPPI(AImageWidth, APPI: Integer;
  const ACanvasScaleFactor: Double): TScaledImageListResolution;
begin
  if Scaled then
    Result := TScaledImageListResolution.Create(
      GetResolution(GetWidthForPPI(AImageWidth, Round(APPI*ACanvasScaleFactor))),
      ACanvasScaleFactor)
  else
    Result := TScaledImageListResolution.Create(
      GetResolution(GetWidthForPPI(AImageWidth, APPI)), 1);
end;

function TCustomImageList.GetSizeForPPI(AImageWidth, APPI: Integer): TSize;
var
  Res: TCustomImageListResolution;
begin
  Res := GetResolution(GetWidthForPPI(AImageWidth, APPI));
  Result.cx := Res.Width;
  Result.cy := Res.Height;
end;

function TCustomImageList.GetWidthForPPI(AImageWidth,
  APPI: Integer): Integer;
var
  Factor: Integer;
  idx: Integer;
begin
  if AImageWidth<=0 then
    AImageWidth := FWidth;

  if not FScaled then
    Result := AImageWidth
  else begin
    Result := round(AImageWidth * APPI / 96);
    if not FData.Find(Result, idx) then begin
      if APPI <= 120 then
        Factor := 100 // 100-125% (96-120 DPI): no scaling
      else
      if APPI <= 168 then
        Factor := 150 // 126%-175% (144-168 DPI): 150% scaling
      else
        Factor := Round(APPI/96) * 100; // 200, 300, 400, ...
      Result := AImageWidth * Factor div 100;
    end;
  end;

  if Assigned(FOnGetWidthForPPI) then
    FOnGetWidthForPPI(Self, AImageWidth, APPI, Result);
end;

function TCustomImageList.GetResolution(
  AImageWidth: Integer): TCustomImageListResolution;
begin
  if AImageWidth<=0 then
    AImageWidth := FWidth;
  Result := FData.ImageLists[AImageWidth];
end;

function TCustomImageList.GetResolutionByIndex(
  AIndex: Integer): TCustomImageListResolution;
begin
  Result := FData[AIndex];
end;

function TCustomImageList.GetResolutionClass: TCustomImageListResolutionClass;
begin
  Result := TCustomImageListResolution;
end;

function TCustomImageList.GetResolutionCount: Integer;
begin
  Result := FData.Count;
end;

{------------------------------------------------------------------------------
  Function: TCustomImageList.GetHotspot
  Params:   None
  Returns:  The co-ordinates for the hotspot of the drag image

  Returns the co-ordinates for the hotspot of the drag image.
 ------------------------------------------------------------------------------}
function TCustomImageList.GetHotSpot: TPoint;
begin
  Result := GetResolution(FWidth).GetHotSpot;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Initialize
  Params:  None
  Returns: Nothing

  Initializes the internal bitmap structures and the changelink list. 
  It is used by the Create and CreateSize constructors
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Initialize;
begin
  FData := TCustomImageListResolutions.Create(Self, GetResolutionClass);
  FChangeLinkList := TList.Create;
  FAllocBy := 4;
  FBlendColor := clNone;
  FBkColor := clNone;
  FDrawingStyle := dsNormal;
  ClearOverlays;

  if (Height < 1) or (Height > 32768) or (Width < 1)
  then raise EInvalidOperation.Create(SInvalidImageSize);
end;

procedure TCustomImageList.SetWidthHeight(NewWidth, NewHeight: integer);
begin
  if (FHeight=NewHeight) and (FWidth=NewWidth) then exit;
  FHeight := NewHeight;
  FWidth := NewWidth;
  Clear;
end;

procedure TCustomImageList.ClearOverlays;
var
  I: TOverlay;
begin
  for I := Low(TOverlay) to High(TOverlay) do
    FOverlays[I] := -1;
end;

procedure TCustomImageList.CreateDefaultResolution;
begin
  if ResolutionCount=0 then
    GetResolution(FWidth); // create default resolution if needed
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Insert
  Params:  Index: the index of the inserted image
           Image: a bitmap image
           Mask: a bitmap which defines the transparent parts of Image
  Returns: Nothing

  Inserts one or more (bitmap width / imagelist width) bitmaps into the list 
  at the index'th position.  If Mask is nil, the image has no transparent parts.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Insert(AIndex: Integer; AImage, AMask: TCustomBitmap);
var
  R: TCustomImageListResolution;
  ScBmp: TRGBAQuadArray;
begin
  if AImage = nil then Exit;
  
  CreateDefaultResolution;
  for R in Resolutions do
  begin
    ScaleImage(AImage, AMask, R.Width, R.Height, ScBmp);
    R.InternalInsert(AIndex, @ScBmp[0]);
  end;
end;

procedure TCustomImageList.InsertIcon(AIndex: Integer; AIcon: TCustomIcon);
var
  R: TCustomImageListResolution;
  ScBmp: TRGBAQuadArray;
  msk: HBITMAP;
  SortedIcon: TIcon;
begin
  if AIcon = nil then Exit;

  CreateDefaultResolution;
  SortedIcon := TIcon.Create;
  try
    SortedIcon.Assign(AIcon);
    SortedIcon.Sort;
    for R in Resolutions do
    begin
      SortedIcon.Current := GetBestIconIndexForSize(SortedIcon, R.Width);
      if SortedIcon.Masked then
        msk := SortedIcon.MaskHandle
      else
        msk := 0;
      ScaleImage(SortedIcon.BitmapHandle, msk,
        Rect(0, 0, SortedIcon.Width, SortedIcon.Height), R.Width, R.Height, ScBmp);
      R.InternalInsert(AIndex, @ScBmp[0]);
    end;
  finally
    SortedIcon.Free;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.InsertMasked
  Params:  Index: the index of the inserted image
           AImage: A bitmap to be inserted
           MaskColor: The color acting as transparant color
  Returns: Nothing

  Adds one or more (bitmap width / imagelist width) bitmaps to the list. 
  Every occurrence of MaskColor will be converted to transparent.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.InsertMasked(Index: Integer; AImage: TCustomBitmap;
  MaskColor: TColor);
var
  MaskedImage: TBitmap;
  ScBmp: TRGBAQuadArray;
  R: TCustomImageListResolution;
begin
  if AImage = nil then Exit;

  MaskedImage := TBitmap.Create;
  try
    MaskedImage.Assign(AImage);
    MaskedImage.TransparentColor := MaskColor;
    MaskedImage.TransparentMode := tmFixed;
    MaskedImage.Transparent := True;

    CreateDefaultResolution;
    for R in Resolutions do
    begin
      ScaleImage(MaskedImage.BitmapHandle, MaskedImage.MaskHandle, Rect(0, 0, MaskedImage.Width, MaskedImage.Height), R.Width, R.Height, ScBmp);
      R.InternalInsert(Index, @ScBmp[0]);
    end;
  finally
    MaskedImage.Free;
  end;
end;

class procedure TCustomImageList.ScaleImage(const ABitmap,
  AMask: TCustomBitmap; TargetWidth, TargetHeight: Integer;
  var AData: TRGBAQuadArray);
begin
  ScaleImage(ABitmap, AMask, Rect(0, 0, ABitmap.Width, ABitmap.Height), TargetWidth, TargetHeight, AData);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Move
  Params:  CurIndex: the index of the image to be moved
           NewIndex: the new index of the image
  Returns: Nothing

  Moves an image from the CurIndex'th location to NewIndex'th location
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Move(ACurIndex, ANewIndex: Integer);
var
  R: TCustomImageListResolution;
begin
  if ACurIndex = ANewIndex then Exit;
  CheckIndex(ACurIndex);
  CheckIndex(ANewIndex);

  if ACurIndex < 0 then ACurIndex := 0;
  if ANewIndex < 0 then ANewIndex := 0;

  for R in Resolutions do
  begin
    R.InternalMove(ACurIndex, ANewIndex, False);
    if R.HandleAllocated
    then TWSCustomImageListResolutionClass(R.WidgetSetClass).Move(R, ACurIndex, ANewIndex);
  end;

  FChanged := true;
  Change;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.NotifyChangeLink
  Params:  None
  Returns: Nothing

  Internal function to notify the subscribed objects of a change 
  of the imagelist.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.NotifyChangeLink;
var
  nIndex: Integer;
begin
  if FChangeLinkList <> nil then
    with FChangeLinkList do
      for nIndex := 0 to Count - 1 do TChangeLink(Items[nIndex]).Change
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.WriteDefData
  Params:  AStream: The stream to write the data to
  Returns: Nothing

  Writes the imagelist data to stream
 ------------------------------------------------------------------------------}

procedure TCustomImageList.WriteData(AStream: TStream);
begin
  GetResolution(FWidth).WriteData(AStream, False); // Change to ACompress=True after Lazarus 1.10 has been released to retain compatibility with 1.8 that doesn't support imagelist compression
end;

procedure TCustomImageList.WriteAdvData(AStream: TStream);
var
  Signature: TImageListSignature;
  R: TCustomImageListResolution;
  SaveR: array of TCustomImageListResolution;
  I: Integer;
begin
  //Write signature
  Signature:=SIG_LAZ3;
  AStream.Write(Signature,SizeOf(Signature));

  SetLength(SaveR, 0);
  for R in Resolutions do
    if (R.Width<>FWidth) and not R.AutoCreatedInDesignTime then
    begin
      SetLength(SaveR, Length(SaveR)+1);
      SaveR[High(SaveR)] := R;
    end;

  //Count of resolutions
  WriteLRSInteger(AStream, Length(SaveR));

  for R in SaveR do
    R.WriteData(AStream, True);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.ReadDefData
  Params:  AStream: The stream to read the data from
  Returns: Nothing

  Reads the imagelist data from stream
 ------------------------------------------------------------------------------}

procedure TCustomImageList.ReadData(AStream: TStream);
begin
  BeginUpdate;
  try
    GetResolution(FWidth).ReadData(AStream);
  finally
    EndUpdate;
  end;

  FChanged := true;
  Change;
end;

procedure TCustomImageList.ReadAdvData(AStream: TStream);
var
  Signature: TImageListSignature;
  ResCount, I: Integer;
  AStreamPos: Int64;
  ImgCount, ImgWidth, ImgHeight: Cardinal;
  R: TCustomImageListResolution;
begin
  BeginUpdate;
  try
    AStream.Read(Signature, SizeOf(Signature));

    if Signature <> SIG_LAZ3 then
      raise Exception.Create('Invalid BitmapAdv signature.');

    ResCount := ReadLRSInteger(AStream);
    for I := 0 to ResCount-1 do
    begin
      AStreamPos := AStream.Position;
      AStream.Read(Signature, SizeOf(Signature));
      if (Signature <> SIG_LAZ3) and (Signature <> SIG_LAZ4) then
        raise Exception.Create('Invalid BitmapAdv signature.');

      ImgCount := ReadLRSCardinal(AStream);
      ImgWidth := ReadLRSCardinal(AStream);
      ImgHeight := ReadLRSCardinal(AStream);

      R := FData.GetImageLists(ImgWidth, False, False);
      AStream.Position := AStreamPos;
      R.ReadData(AStream);
    end;
  finally
    EndUpdate;
  end;

  FChanged := true;
  Change;
end;

function TCustomImageList.Resolutions: TCustomImageListResolutionEnumerator;
begin
  Result := TCustomImageListResolutionEnumerator.Create(Self, False);
end;

function TCustomImageList.ResolutionsDesc: TCustomImageListResolutionEnumerator;
begin
  Result := TCustomImageListResolutionEnumerator.Create(Self, True);
end;

class procedure TCustomImageList.ScaleImage(const ABitmap, AMask: HBITMAP;
  SourceRect: TRect; TargetWidth, TargetHeight: Integer;
  var AData: TRGBAQuadArray);
var
  Size, X, Y: Integer;
  II: TLazIntfImage;
  FI, ScFI: TFPCompactImgRGBA8Bit;
  ScCanvas: TFPImageCanvas;
  Px: PRGBAQuad;
  C: TFPColor;
begin
  Size := TargetHeight * TargetWidth;
  SetLength(AData, Size);
  FillChar(AData[0], Size * SizeOf(TRGBAQuad), 1);

  FI := nil;
  ScFI := nil;
  ScCanvas := nil;
  II := TLazIntfImage.Create(0, 0);
  try
    II.LoadFromBitmap(ABitmap, AMask);

    FI := TFPCompactImgRGBA8Bit.Create(SourceRect.Right-SourceRect.Left, SourceRect.Bottom-SourceRect.Top);

    for X := SourceRect.Left to SourceRect.Right-1 do
    for Y := SourceRect.Top to SourceRect.Bottom-1 do
    begin
      if (X>=0) and (X<II.Width) and (Y>=0) and (Y<II.Height) then
      begin
        C := II.Colors[X, Y];
        if (AMask<>0) and II.Masked[X, Y] then
          C.Alpha := 0;
      end else
        C := colTransparent;
      FI.Colors[X-SourceRect.Left, Y-SourceRect.Top] := C;
    end;

    if (SourceRect.Right-SourceRect.Left=TargetWidth) and
       (SourceRect.Bottom-SourceRect.Top=TargetHeight) then
      ScFI := FI
    else
    begin
      ScFI := TFPCompactImgRGBA8Bit.Create(TargetWidth, TargetHeight);

      ScCanvas := TFPImageCanvas.create(ScFI);
      ScCanvas.StretchDraw(0,0,TargetWidth,TargetHeight, FI);
    end;

    Px := @AData[0];
    for Y := 0 to TargetHeight-1 do
    for X := 0 to TargetWidth-1 do
    begin
      C := ScFI.Colors[X, Y];
      Px^.Red := Byte(C.Red);
      Px^.Green := Byte(C.Green);
      Px^.Blue := Byte(C.Blue);
      Px^.Alpha := Byte(C.Alpha);
      Inc(Px);
    end;
  finally
    II.Free;
    FI.Free;
    if FI<>ScFI then
    begin
      ScFI.Free;
      ScCanvas.Free;
    end;
  end;
end;

class procedure TCustomImageList.ScaleImage(const ABitmap,
  AMask: TCustomBitmap; SourceRect: TRect; TargetWidth, TargetHeight: Integer;
  var AData: TRGBAQuadArray);
var
  msk: HBITMAP;
begin
  if AMask<>nil then
    msk := AMask.BitmapHandle
  else
  if ABitmap.Masked then
    msk := ABitmap.MaskHandle
  else
    msk := 0;
  ScaleImage(ABitmap.Handle, msk, SourceRect, TargetWidth, TargetHeight, AData);
end;

function TCustomImageList.Equals(Obj: TObject): boolean;
var
  SrcList: TCustomImageList;
  CurStream: TMemoryStream;
  SrcStream: TMemoryStream;
begin
  if Obj is TCustomImageList then begin
    SrcList:=TCustomImageList(Obj);
    Result:=false;
    if SrcList.Count<>Count then exit;
    if Count=0 then exit(true);
    CurStream:=TMemoryStream.Create;
    SrcStream:=TMemoryStream.Create;
    try
      WriteData(CurStream);
      WriteAdvData(CurStream);
      SrcList.WriteData(SrcStream);
      SrcList.WriteAdvData(SrcStream);
      Result:=CompareMemStreams(CurStream,SrcStream);
    finally
      SrcStream.Free;
      CurStream.Free;
    end;
  end else
    {$IF FPC_FULLVERSION>20402}
    Result:=inherited Equals(Obj);
    {$ELSE}
    Result:=false;
    {$ENDIF}
end;

function TCustomImageList.FindResolution(AImageWidth: Integer; out
  AResolution: TCustomImageListResolution): Boolean;
var
  I: Integer;
begin
  Result := FData.Find(AImageWidth, I);
  if Result then
    AResolution := ResolutionByIndex[I]
  else
    AResolution := nil;
end;

function TCustomImageList.GetBestIconIndexForSize(AIcon: TCustomIcon;
  AWidth: Integer): Integer;
var
  MaxWidth, MaxWidthI: Integer;
begin
  MaxWidth := 0;
  for Result := 0 to AIcon.Count-1 do // first try to find the smallest (but bigger) multiple
  begin
    AIcon.Current := Result;
    if (AIcon.Width mod AWidth = 0) then
      Exit;

    if AIcon.Width>MaxWidth then
    begin
      MaxWidth := AIcon.Width;
      MaxWidthI := Result;
    end;
  end;

  Result := MaxWidthI; // just pickup the biggest image
end;

procedure TCustomImageList.Overlay(AIndex: Integer; Overlay: TOverlay);
var
  i: Integer;
begin
  FOverlays[Overlay] := AIndex;
  if AIndex>=0 then
    fHasOverlays:=true
  else if fHasOverlays then begin
    fHasOverlays:=false;
    for i:=Low(TOverlay) to high(TOverlay) do
    begin
      if FOverlays[i]<0 then continue;
      fHasOverlays:=true;
      break;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.RegisterChanges
  Params:  Value: a reference to changelink object
  Returns: Nothing

  Registers an object to get notified of a change of the imagelist.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.RegisterChanges(Value: TChangeLink);
begin
  if (Value <> nil) and (FChangeLinkList.IndexOf(Value) = -1)
  then begin
    Value.Sender := Self;
    FChangeLinkList.Add(Value);
  end;
end;

procedure TCustomImageList.RegisterResolutions(
  const AResolutionWidths: array of Integer);
var
  R: Integer;
begin
  for R in AResolutionWidths do
  begin
    GetResolution(R).AutoCreatedInDesignTime := False;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Replace
  Params:  Index: the index of the replaceded image
           Image: a bitmap image
           Mask: a bitmap which defines the transparent parts of Image
  Returns: Nothing.

  Replaces the index'th image with the image given. If Mask is nil, 
  the image has no transparent parts.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Replace(AIndex: Integer; AImage,
  AMask: TCustomBitmap; const AllResolutions: Boolean);
var
  R: TCustomImageListResolution;
  Data: TRGBAQuadArray;

  procedure _Replace;
  begin
    ScaleImage(AImage, AMask, R.Width, R.Height, Data);
    R.InternalReplace(AIndex, @Data[0]);
  end;
begin
  if AImage = nil then Exit;
  
  if AllResolutions then
  begin
    for R in Resolutions do
      _Replace;
  end else
  begin
    if FindResolution(AImage.Width, R) then
      _Replace;
  end;
end;

procedure TCustomImageList.ReplaceIcon(AIndex: Integer; AIcon: TCustomIcon);
var
  R: TCustomImageListResolution;
  ScBmp: TRGBAQuadArray;
  msk: HBITMAP;
  SortedIcon: TIcon;
begin
  if AIcon = nil then Exit;

  SortedIcon := TIcon.Create;
  try
    SortedIcon.Assign(AIcon);
    SortedIcon.Sort;
    for R in Resolutions do
    begin
      SortedIcon.Current := GetBestIconIndexForSize(SortedIcon, R.Width);
      if SortedIcon.Masked then
        msk := SortedIcon.MaskHandle
      else
        msk := 0;
      ScaleImage(SortedIcon.BitmapHandle, msk,
        Rect(0, 0, SortedIcon.Width, SortedIcon.Height), R.Width, R.Height, ScBmp);
      R.InternalReplace(AIndex, @ScBmp[0]);
    end;
  finally
    SortedIcon.Free;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.ReplaceMasked
  Params:  Index: the index of the replaceded image
           Image: A bitmap image
           MaskColor: The color acting as transparant color
  Returns: Nothing

  Replaces the index'th image with the image given.
  Every occurrence of MaskColor will be converted to transparent.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.ReplaceMasked(Index: Integer;
  NewImage: TCustomBitmap; MaskColor: TColor; const AllResolutions: Boolean);
var
  Bmp: TBitmap;
  R: TCustomImageListResolution;
  Data: TRGBAQuadArray;

  procedure _Replace;
  begin
    ScaleImage(Bmp, nil, R.Width, R.Height, Data);
    R.InternalReplace(Index, @Data[0]);
  end;
begin
  if NewImage = nil then Exit;

  Bmp := TBitmap.Create;
  try
    Bmp.Assign(NewImage);
    Bmp.TransparentColor := MaskColor;
    Bmp.Transparent := True;

    if AllResolutions then
    begin
      for R in Resolutions do
        _Replace;
    end else
    begin
      if FindResolution(NewImage.Width, R) then
        _Replace;
    end;
  finally
    Bmp.Free;
  end;
end;

procedure TCustomImageList.ReplaceSlice(AIndex: Integer; Image: TCustomBitmap;
  AImageRect: TRect; const AllResolutions: Boolean);
var
  R: TCustomImageListResolution;
  Data: TRGBAQuadArray;

  procedure _Replace;
  begin
    ScaleImage(Image, nil, AImageRect, R.Width, R.Height, Data);
    R.InternalReplace(AIndex, @Data[0]);
  end;
begin
  if Image = nil then Exit;

  if AllResolutions then
  begin
    for R in Resolutions do
      _Replace;
  end else
  begin
    if FindResolution(AImageRect.Right-AImageRect.Left, R) then
      _Replace;
  end;
end;

procedure TCustomImageList.ReplaceSliceCentered(AIndex, AImageWidth: Integer;
  Image: TCustomBitmap; const AllResolutions: Boolean);
var
  ImageRect: TRect;
  R: TCustomImageListResolution;
begin
  if not FindResolution(AImageWidth, R) then Exit;
  ImageRect := Rect(0, 0, R.Width, R.Height);
  OffsetRect(ImageRect, (Image.Width-R.Width) div 2, (Image.Height-R.Height) div 2);
  ReplaceSlice(AIndex, Image, ImageRect, AllResolutions);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.SetBkColor
  Params:  Value: The background color
  Returns: Nothing

  Sets the backgroundcolor for the transparen parts.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.SetBkColor(const Value: TColor);
begin
  if FBkColor <> Value
  then begin
    FBkColor := Value;
    FChanged := true;
    Change;
  end;
end;

procedure TCustomImageList.SetDrawingStyle(const AValue: TDrawingStyle);
begin
  if FDrawingStyle=AValue then exit;
  FDrawingStyle:=AValue;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.SetHeight
  Params:  Value: the height of an image
  Returns: Nothing

  Sets the height of an image. If the height differs from the original height,
  the list contents wil be deleted.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.SetHeight(const Value: Integer);
begin
  SetWidthHeight(Width,Value);
end;

procedure TCustomImageList.SetMasked(const AValue: boolean);
begin
  if FMasked=AValue then exit;
  FMasked:=AValue;
end;

procedure TCustomImageList.SetShareImages(const AValue: Boolean);
begin
  if FShareImages=AValue then exit;
  FShareImages:=AValue;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.SetWidth
  Params:  Value: the width of an image
  Returns: Nothing

  Sets the width of an image. If the width differs from the original width,
  the list contents wil be deleted.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.SetWidth(const Value: Integer);
begin
  SetWidthHeight(Value,Height);
end;

procedure TCustomImageList.StretchDraw(Canvas: TCanvas; Index: Integer;
  ARect: TRect; Enabled: Boolean);
var
  I: Integer;
begin
  I := FData.FindBestToScaleFrom(ARect.Right-ARect.Left);
  if I>=0 then
    FData[I].StretchDraw(Canvas, Index, ARect, Enabled);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.UnRegisterChanges
  Params:  Value: a reference to changelink object
  Returns: Nothing

  Unregisters an object for notifications.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.UnRegisterChanges(Value: TChangeLink);
begin
  if (FChangeLinkList<>nil) and (Value.Sender=Self) then
    FChangeLinkList.Remove(Value);
  Value.Sender:=nil;
end;

{******************************************************************************
                                  TChangeLink
 ******************************************************************************}

{------------------------------------------------------------------------------
  Method:  TChangeLink.Change
  Params:  None
  Returns: Nothing

  Fires the OnChange event.
 ------------------------------------------------------------------------------}
procedure TChangeLink.Change;
begin
  if Assigned(FOnChange) then FOnChange(Sender)
end;

{------------------------------------------------------------------------------
  Method: TChangeLink.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TChangeLink.Destroy;
begin
  if Sender <> nil 
  then Sender.UnRegisterChanges(Self);
  inherited Destroy;
end;

procedure TChangeLink.DoDestroyResolutionReference(const AWidth: Integer;
  AResolutionReference: TLCLHandle);
begin
  if Assigned(FOnDestroyResolutionHandle) then
    FOnDestroyResolutionHandle(FSender, AWidth, AResolutionReference);
end;

{ TLCLGlyphs.TEntry }

function TLCLGlyphs_TEntry_Compare(Item1, Item2: Pointer): Integer;
var
  AItem1: TLCLGlyphs.TEntry absolute Item1;
  AItem2: TLCLGlyphs.TEntry absolute Item2;
begin
  Result := CompareStr(AItem1.GlyphName, AItem2.GlyphName);
end;

function TLCLGlyphs_TEntry_CompareKey(Key, Item: Pointer): Integer;
var
  AKey: TLCLGlyphs.PEntryKey absolute Key;
  AItem: TLCLGlyphs.TEntry absolute Item;
begin
  Result := CompareStr(AKey^.GlyphName, AItem.GlyphName);
end;

{ TLCLGlyphs }

constructor TLCLGlyphs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FImageIndexes := TAvgLvlTree.Create(@TLCLGlyphs_TEntry_Compare);
  Scaled := True;
  FMissingResources := gmrIgnoreAll;
end;

destructor TLCLGlyphs.Destroy;
begin
  FImageIndexes.FreeAndClear;
  FImageIndexes.Free;

  inherited Destroy;
end;

function TLCLGlyphs.GetImageIndex(const AResourceName: string): Integer;

  function AddNewBtnImage(Resolution: TResolution): Integer;
  var
    G: TCustomBitmap;
    ImageRect: TRect;
  begin
    G := GetDefaultGlyph(AResourceName, Resolution.ScaleSuffix, True);
    if G=nil then
      Exit(-1);
    try
      ImageRect := Rect(0, 0, Resolution.Width, GetHeightForWidth(Resolution.Width));
      OffsetRect(ImageRect, (G.Width-ImageRect.Right) div 2, (G.Height-ImageRect.Bottom) div 2);
      Result := AddSlice(G, ImageRect);
    finally
      G.Free;
    end;
  end;

  procedure AddBtnImageRes(ImageIndex: Integer; Resolution: TResolution);
  var
    G: TCustomBitmap;
  begin
    G := GetDefaultGlyph(AResourceName, Resolution.ScaleSuffix, FMissingResources in [gmrIgnoreAll, gmrOneMustExist]);
    if G<>nil then
      try
        ReplaceSliceCentered(ImageIndex, Resolution.Width, G, False);
      finally
        G.Free;
      end;
  end;

var
  K: TEntryKey;
  ANode: TAVLTreeNode;
  E: TEntry;
  I, FirstLoadedResolutionI: Integer;
begin
  K.GlyphName := AResourceName;

  ANode := FImageIndexes.FindKey(@K, @TLCLGlyphs_TEntry_CompareKey);
  if ANode<>nil then
    Result := TEntry(ANode.Data).ImageIndex
  else
  begin
    Result := -1;
    FirstLoadedResolutionI := Low(FLoadResolutions)-1;
    for I := High(FLoadResolutions) downto Low(FLoadResolutions) do // start with the biggest one
    begin
      Result := AddNewBtnImage(FLoadResolutions[I]);
      if Result>=0 then
      begin
        FirstLoadedResolutionI := I;
        break;
      end;
    end;
    if (Result>=0) and (FirstLoadedResolutionI>=Low(FLoadResolutions)) then
    begin
      for I := FirstLoadedResolutionI-1 downto Low(FLoadResolutions) do // load the smaller ones
        AddBtnImageRes(Result, FLoadResolutions[I]);
    end else
    if FMissingResources in [gmrAllMustExist, gmrOneMustExist] then
      raise EResNotFound.CreateFmt(SResNotFound,[AResourceName]);

    E := TEntry.Create;
    FImageIndexes.Add(E);
    E.GlyphName := AResourceName;
    E.ImageIndex := Result;
  end;
end;

procedure TLCLGlyphs.RegisterResolutions(const AResolutionWidths,
  AResolutionScaleSuffixes: array of Integer);
var
  I: Integer;
begin
  RegisterResolutions(AResolutionWidths);

  for I := Low(FLoadResolutions) to High(FLoadResolutions) do
    FLoadResolutions[I].ScaleSuffix := AResolutionScaleSuffixes[I];
end;

procedure TLCLGlyphs.RegisterResolutions(
  const AResolutionWidths: array of Integer);
var
  I, LastWidth: Integer;
begin
  inherited RegisterResolutions(AResolutionWidths);

  SetLength(FLoadResolutions, Length(AResolutionWidths));
  LastWidth := 0;
  for I := Low(FLoadResolutions) to High(FLoadResolutions) do
  begin
    if AResolutionWidths[I]<=LastWidth then
      raise Exception.Create('AResolutionWidths not sorted.');
    LastWidth := AResolutionWidths[I];
    FLoadResolutions[I].Width := AResolutionWidths[I];
    FLoadResolutions[I].ScaleSuffix := MulDiv(FLoadResolutions[I].Width, 100, Width);
  end;
end;

procedure TLCLGlyphs.SetWidth100Suffix(const AWidth100Suffix: Integer);
var
  I: Integer;
begin
  for I := 0 to High(FLoadResolutions) do
    FLoadResolutions[I].ScaleSuffix := MulDiv(FLoadResolutions[I].Width, 100, AWidth100Suffix);
end;

procedure InterfaceFinal;
begin
  FreeAndNil(GLCLGlyphs);
end;

initialization
  RegisterInterfaceFinalizationHandler(@InterfaceFinal);
// included by imglist.pp
