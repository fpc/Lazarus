{%MainUnit ../imglist.pp}

{******************************************************************************
                                  TCustomImageList
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

type
  TImageListSignature = array[0..1] of char;

const
  SIG_LAZ1 = #1#0;
  SIG_LAZ2 = 'li';
  SIG_LAZ3 = 'Li';
  SIG_D3   = 'IL';

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
function TCustomImageList.AddIcon(Image: TIcon): Integer;
begin 
  //!!! check one or more

  //No Icon Support yet

  Result := -1;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.AddImages
  Params:  Value: An imagelist containing images to be added
  Returns: Nothing

  Adds images from another imagelist to the list.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.AddImages(AValue: TCustomImageList);
var
  n: Integer;
  p: PRGBAQuad;
  DataSize: Integer;
  OldCount: Integer;
begin
  if (AValue = nil) or (AValue=Self) or (AValue.FCount = 0) then exit;

  AllocData(FCount + AValue.FCount);
  if (AValue.FWidth = FWidth) and (AValue.FHeight = FHeight)
  then begin
    DataSize := FWidth * FHeight * SizeOf(FData[0]);
    System.Move(AVAlue.FData[0], FData[FCount], AValue.FCount * DataSize);
    OldCount := FCount;
    Inc(FCount, AValue.FCount);
    if HandleAllocated
    then begin
      p := @FData[OldCount];
      for n := OldCount to FCount - 1 do
      begin
        TWSCustomImageListClass(WidgetSetClass).Insert(Self, n, p);
        Inc(PByte(p), DataSize);
      end;
    end;
  end
  else begin
    // ToDo:
    raise Exception.Create('TCustomImageList.AddImages not implemented yet for other Width/Height');
  end;
end;

{------------------------------------------------------------------------------
  Function: TCustomImageList.AddMasked
  Params:   Image: A bitmap to be added
            MaskColor: The color acting as transparant color
  Returns:  The index of the added icon, -1 if unsuccesfull.

  Adds one or more (bitmap width / imagelist width) bitmaps to the list. 
  Every occurance of MaskColor will be converted to transparent.
 ------------------------------------------------------------------------------}
function TCustomImageList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  try
    Result := Count;
    InsertMasked(Result, Image, MaskColor);
  except
    on E: Exception do begin
      DebugLn('TCustomImageList.AddMasked ',E.Message);
      Result := -1; // Ignore exceptions, just return -1
    end;
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

{------------------------------------------------------------------------------
  Method:  TCustomImageList.AllocData
  Params:  ACount: the amount of images
  Returns: Nothing

  Allocates data for ACount images
 ------------------------------------------------------------------------------}
procedure TCustomImageList.AllocData(ACount: Integer);
var
  n: Integer;
begin
  if FAllocCount >= ACount
  then Exit;

  // calculate number of blocks, add an extra block for the remainder.
  n := ACount mod FAllocBy;
  if n <> 0
  then Inc(ACount, FAllocBy - n);

  SetLength(FData, ACount * FWidth * FHeight * SizeOf(FData[0]));

  Inc(FAllocCount, ACount);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.InternalInsert
  Params:  AIndex: Index to insert images
           AImage, AMask: handles of Image and Mask
           AWidth, AHeight: Width and Height of AImage and AMask
  Returns: Nothing

  Insert bitmap (with split if necessary) into position AIndex with shifting other
  images
 ------------------------------------------------------------------------------}
procedure TCustomImageList.InternalInsert(AIndex: Integer; AImage, AMask: HBitmap;
  AWidth, AHeight: Integer);
var
  RawImg: TRawImage;
  R: TRect;
  ImgData: PRGBAQuad;
  i, ACount: Integer;
begin
  CheckIndex(AIndex, True);
  if (AIndex < 0) then
    AIndex := 0;

  ACount := AWidth div Width;
  if ACount = 0 then
    ACount := 1;
  Inc(FCount, ACount);
  AllocData(FCount);
  if AIndex < FCount - ACount then
  begin
    for i := 0 to ACount - 1 do
      InternalMove(FCount - i - 1, AIndex + i, True);
  end;

  R := Rect(0, 0, FWidth, FHeight);
  for i := 0 to ACount - 1 do
  begin
    RawImage_FromBitmap(RawImg, AImage, AMask, @R);
    ImgData := InternalSetImage(AIndex + i, RawImg);
    if HandleAllocated
    then TWSCustomImageListClass(WidgetSetClass).Insert(Self, AIndex + i, ImgData);
    inc(R.Left, FWidth);
    inc(R.Right, FWidth);
  end;

  FChanged := true;
  Change;
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
begin
  if (Source=Self) then exit;
  if Source is TCustomImageList then
  begin
    ImgSrc := TCustomImageList(Source);
    BeginUpdate;
    try
      SetWidthHeight(ImgSrc.Width,ImgSrc.Height);
      Clear;
      AddImages(ImgSrc);
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
    if AIndex > FCount then Error;
  end
  else begin
    if AIndex >= FCount then Error;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Clear
  Params:  None
  Returns: Nothing

  Clears the list.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Clear;
begin
  if FCount = 0 then Exit;
  if HandleAllocated
  then TWSCustomImageListClass(WidgetSetClass).Clear(Self);
  SetLength(FData, 0);
  FAllocCount := 0;

  FCount := 0;
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

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Bitmap', @ReadData, @WriteData, DoWrite);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Delete
  Params:  Index: the index of the image to be deleted.
  Returns: Nothing

  Deletes the image identified by Index. An index of -1 deletes all
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Delete(AIndex: Integer);
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
  then TWSCustomImageListClass(WidgetSetClass).Delete(Self, AIndex);
  // TODO: adjust allocated data
  FChanged := true;
  Change;
end;

{------------------------------------------------------------------------------
  Method: TCustomImageList.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCustomImageList.Destroy;
begin
  inherited Destroy;
  while FChangeLinkList.Count>0 do
    UnregisterChanges(TChangeLink(FChangeLinkList[0]));
  FreeThenNil(FChangeLinkList);
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
const
  EffectMap: array[Boolean] of TGraphicsDrawEffect =
  (
    gdeDisabled,
    gdeNormal
  );
begin
  Draw(ACanvas, AX, AY, AIndex, EffectMap[AEnabled]);
end;

procedure TCustomImageList.Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer;
  ADrawEffect: TGraphicsDrawEffect);
begin
  if (AIndex < 0) or (AIndex >= FCount) then Exit;

  ReferenceNeeded;
  TWSCustomImageListClass(WidgetSetClass).Draw(Self, AIndex, ACanvas, Rect(AX, AY, FWidth, FHeight),
                          BkColor, BlendColor, ADrawEffect, DrawingStyle, ImageType);
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
  Method:  TCustomImageList.FillDescription
  Params:  Desc: the description to fill
  Returns: Nothing

  Fills the description with the default info of the imagedata
 ------------------------------------------------------------------------------}
procedure TCustomImageList.FillDescription(out ADesc: TRawImageDescription);
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
var
  RawImg: TRawImage;
  ListImg, DeviceImg: TLazIntfImage;
  ImgHandle, MskHandle: HBitmap;
begin
  if (FCount = 0) or (Image = nil) then Exit;

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

procedure TCustomImageList.GetBitmap(Index: Integer; Image: TCustomBitmap;
  AEffect: TGraphicsDrawEffect);
var
  RawImg: TRawImage;
  ListImg, DeviceImg: TLazIntfImage;
  ImgHandle, MskHandle: HBitmap;
begin
  if (FCount = 0) or (Image = nil) then Exit;

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

procedure TCustomImageList.GetFullRawImage(out Image: TRawImage);
begin
  Image.Init;

  if (FCount = 0) then Exit;
  FillDescription(Image.Description);
  Image.Description.Height := Height * Count;
  Image.DataSize := Width * Height * Count * SizeOf(FData[0]);
  Image.Data := PByte(FData);
end;

procedure TCustomImageList.GetRawImage(Index: Integer; out Image: TRawImage);
begin
  Image.Init;

  if (FCount = 0) then Exit;
  CheckIndex(Index);
  FillDescription(Image.Description);
  Image.DataSize := FWidth * FHeight * SizeOf(FData[0]);
  Image.Data := @FData[Index * FWidth * FHeight];
end;

function TCustomImageList.GetReference: TWSCustomImageListReference;
begin
  if not FReference.Allocated then ReferenceNeeded;
  Result := FReference;
end;

function TCustomImageList.GetReferenceHandle: THandle;
begin
  Result := FReference.Handle;
end;

{------------------------------------------------------------------------------
  Function: TCustomImageList.GetHotspot
  Params:   None
  Returns:  The co-ordinates for the hotspot of the drag image

  Returns the co-ordinates for the hotspot of the drag image.
 ------------------------------------------------------------------------------}
function TCustomImageList.GetHotSpot: TPoint;
begin
  Result := Point(0, 0);
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
  FChangeLinkList := TList.Create;
  FAllocBy := 4;
  FAllocCount := 0;
  FBlendColor := clNone;
  FBkColor := clNone;
  FDrawingStyle := dsNormal;

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

class procedure TCustomImageList.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomImageList;
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
  msk: THandle;
begin
  if AImage = nil then Exit;
  
  if AMask = nil
  then begin
    if AImage.Masked
    then msk := AImage.MaskHandle
    else msk := 0;
  end
  else msk := AMask.Handle;
  
  InternalInsert(AIndex, AImage.Handle, msk, AImage.Width, AImage.Height);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.InsertMasked
  Params:  Index: the index of the inserted image
           AImage: A bitmap to be inserted
           MaskColor: The color acting as transparant color
  Returns: Nothing

  Adds one or more (bitmap width / imagelist width) bitmaps to the list. 
  Every occurance of MaskColor will be converted to transparent.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.InsertMasked(Index: Integer; AImage: TCustomBitmap;
  MaskColor: TColor);
var
  Bmp: TBitmap;
begin
  if AImage = nil then Exit;

  Bmp := TBitmap.Create;
  with Bmp do
  begin
    Assign(AImage);
    TransparentColor := MaskColor;
    Transparent := True;
  end;
  if Bmp.Masked
  then InternalInsert(Index, Bmp.Handle, Bmp.MaskHandle, Bmp.Width, Bmp.Height)
  else InternalInsert(Index, Bmp.Handle, 0, Bmp.Width, Bmp.Height);
  Bmp.Free;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.InternalMove
  Params:  CurIndex: the index of the image to be moved
           NewIndex: the new index of the image
  Returns: Nothing

  Moves an image from the CurIndex'th location to NewIndex'th location
  without notifying the widgetset
 ------------------------------------------------------------------------------}
procedure TCustomImageList.InternalMove(ACurIndex, ANewIndex: Cardinal; AIgnoreCurrent: Boolean);
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

procedure TCustomImageList.InternalReplace(AIndex: Integer; AImage,
  AMask: HBitmap);
var
  RawImage: TRawImage;
  R: TRect;
  ImgData: PRGBAQuad;
begin
  if (AIndex < 0) then AIndex := 0;
  CheckIndex(AIndex);

  R := Rect(0, 0, FWidth, FHeight);
  RawImage_FromBitmap(RawImage, AImage, AMask, @R);
  ImgData := InternalSetImage(AIndex, RawImage);
  if HandleAllocated
  then TWSCustomImageListClass(WidgetSetClass).Replace(Self, AIndex, ImgData);

  FChanged := true;
  Change;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.InternalSetImage
  Params:  AIndex: the index of the location where the image should be set
           AImage: the new image
  Returns: Pointer to the updated image data

  Copies the imagedata into the FData array and then frees the image.
 ------------------------------------------------------------------------------}
function TCustomImageList.InternalSetImage(AIndex: Integer; AImage: TRawImage): PRGBAQuad;
var
  Desc: TRawImageDescription absolute AImage.Description;
  
  RawImg: TRawImage;
  SrcImg, DstImg: TLazIntfImage;
  SrcHasAlpha, KeepAlpha: Boolean;
  
begin
  SrcHasAlpha := AImage.Description.AlphaPrec > 0;
  KeepAlpha := SrcHasAlpha;
  if not SrcHasAlpha and (Desc.BitsPerPixel = 32) and (Desc.Depth = 24) and
    (AImage.Mask <> nil) and (Desc.MaskBitsPerPixel > 0)
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

  SrcImg := TLazIntfImage.Create(AImage, True);
  if SrcHasAlpha
  then SrcImg.AlphaFromMask(KeepAlpha);

  RawImg.Init;
  FillDescription(RawImg.Description);
  Result := @FData[AIndex * FWidth * FHeight];
  RawImg.DataSize := FWidth * FHeight * SizeOf(FData[0]);
  RawImg.Data := PByte(Result);
  if not SrcHasAlpha
  then begin
    // Add maskdata to store copied mask, so an alpha can be created
    RawImg.Description.MaskBitsPerPixel := 1;
    RawImg.Description.MaskBitOrder := riboReversedBits;
    RawImg.Description.MaskLineEnd := rileByteBoundary;
    RawImg.Description.MaskShift := 0;
    RawImg.MaskSize := RawImg.Description.MaskBytesPerLine * PtrUInt(FHeight);
    RawImg.Mask := GetMem(RawImg.MaskSize);
  end;

  DstImg := TLazIntfImage.Create(RawImg, False);
  DstImg.CopyPixels(SrcImg);
  if not SrcHasAlpha
  then begin
    DstImg.AlphaFromMask;
    FreeMem(RawImg.Mask);
    RawImg.Mask := nil;
    RawImg.MaskSize := 0;
  end;

  DstImg.Free;
  SrcImg.Free;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Move
  Params:  CurIndex: the index of the image to be moved
           NewIndex: the new index of the image
  Returns: Nothing

  Moves an image from the CurIndex'th location to NewIndex'th location
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Move(ACurIndex, ANewIndex: Integer);
begin
  if ACurIndex = ANewIndex then Exit;
  CheckIndex(ACurIndex);
  CheckIndex(ANewIndex);

  if ACurIndex < 0 then ACurIndex := 0;
  if ANewIndex < 0 then ANewIndex := 0;

  InternalMove(ACurIndex, ANewIndex, False);
  if HandleAllocated
  then TWSCustomImageListClass(WidgetSetClass).Move(Self, ACurIndex, ANewIndex);

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
  Method:  TCustomImageList.WriteData
  Params:  AStream: The stream to write the data to
  Returns: Nothing

  Writes the imagelist data to stream
 ------------------------------------------------------------------------------}
procedure TCustomImageList.WriteData(AStream: TStream);
var
  Signature: TImageListSignature;
begin
  //Write signature
  Signature:=SIG_LAZ3;
  AStream.Write(Signature,SizeOf(Signature));

  //Count of image
  WriteLRSInteger(AStream,Count);
  WriteLRSInteger(AStream,Width);
  WriteLRSInteger(AStream,Height);

  //images
  AStream.Write(FData[0], FWidth * FHeight * FCount * SizeOf(FData[0]));
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.ReadData
  Params:  AStream: The stream to read the data from
  Returns: Nothing

  Reads the imagelist data from stream
 ------------------------------------------------------------------------------}
procedure TCustomImageList.ReadData(AStream: TStream);
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
    Width := ReadLRSCardinal(AStream);
    Height := ReadLRSCardinal(AStream);
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
  
  procedure DoReadLaz3;
  begin
    FCount := ReadLRSCardinal(AStream);
    FWidth := ReadLRSCardinal(AStream);
    FHeight := ReadLRSCardinal(AStream);
    
    AllocData(FCount);
    AStream.ReadBuffer(FData[0], FWidth * FHeight * FCount *  SizeOf(FData[0])) ;

    FChanged := true;
    Change;
  end;

  procedure CreateImagesFromRawImage(IntfImage: TLazIntfImage;
    NewCount: integer);
  var
    RawImage, SubRawImage: TRawImage;
    ImgHandle, MaskHandle: HBitmap;
    Img: TBitmap;
    Row: Integer;
    Col: Integer;
    ImgRect: TRect;
    Res: Boolean;
  begin
    BeginUpdate;
    try
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
          
          Img := TBitmap.Create;
          Img.SetHandles(ImgHandle, MaskHandle);

          Add(Img, nil);
          Img.Free;
          //DebugLn('CreateImagesFromRawImage B ',Img.Width,',',Img.Height,' ',Count);
          Img := nil;
          Dec(NewCount);
        end;
      end;
    finally
      EndUpdate;
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
  BeginUpdate; // avoid multiple changed calls
  try
    Clear;

    StreamPos := AStream.Position;                // check stream signature
    AStream.Read(Signature, SizeOf(Signature));

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
      FBKColor := TColor(ReadLRSInteger(AStream));
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
    
  finally
    EndUpdate;
  end;
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
      SrcList.WriteData(SrcStream);
      Result:=CompareMemStreams(CurStream,SrcStream);
    finally
      SrcStream.Free;
      CurStream.Free;
    end;
  end else
    {$if not (defined(ver2_2_2) or defined(ver2_2_0))}
    {$IF FPC_FULLVERSION>20402}
    Result:=inherited Equals(Obj);
    {$ELSE}
    Result:=false;
    {$ENDIF}
    {$ELSE}
    Result:=false;
    {$ENDIF}
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

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Replace
  Params:  Index: the index of the replaceded image
           Image: a bitmap image
           Mask: a bitmap which defines the transparent parts of Image
  Returns: Nothing.

  Replaces the index'th image with the image given. If Mask is nil, 
  the image has no transparent parts.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Replace(AIndex: Integer; AImage, AMask: TCustomBitmap);
var
  msk: THandle;
begin
  if AImage = nil then Exit;
  
  if AMask = nil
  then msk := 0
  else msk := AMask.Handle;
  InternalReplace(AIndex, AImage.Handle, msk);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.ReplaceMasked
  Params:  Index: the index of the replaceded image
           Image: A bitmap image
           MaskColor: The color acting as transparant color
  Returns: Nothing

  Replaces the index'th image with the image given.
  Every occurance of MaskColor will be converted to transparent.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.ReplaceMasked(Index: Integer; NewImage: TCustomBitmap; MaskColor: TColor);
var
  Bmp: TBitmap;
begin
  if NewImage = nil then Exit;

  Bmp := TBitmap.Create;
  with Bmp do
  begin
    Assign(NewImage);
    TransparentColor := MaskColor;
    Transparent := True;
  end;

  if Bmp.Masked
  then InternalReplace(Index, Bmp.Handle, Bmp.MaskHandle)
  else InternalReplace(Index, Bmp.Handle, 0);
  Bmp.Free;
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
  bmp: TBitmap;
begin
  if (FCount = 0) or (Index >= FCount) then Exit;

  // ToDo: accelerate
  // temp workaround
  bmp := TBitmap.Create;
  GetBitmap(Index, bmp);
  Canvas.StretchDraw(ARect,  bmp);
  bmp.Free;
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

{------------------------------------------------------------------------------
  Method:  TCustomImageList.WSCreateHandle
  Params:  AParams: ignored
  Returns: Handle to created imagelist

  Instructs the widgtset to create an imagelist
 ------------------------------------------------------------------------------}
function TCustomImageList.WSCreateReference(AParams: TCreateParams): PWSReference;
begin
  FReference := TWSCustomImageListClass(WidgetSetClass).CreateReference(Self, FCount, FAllocBy, FWidth, FHeight, @FData[0]);
  Result := @FReference;
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

// included by imglist.pp
