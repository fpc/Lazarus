{%MainUnit ../imglist.pp}

{******************************************************************************
                                  TCustomImageList
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

function TCustomImageList.Add(Image, Mask: TBitmap): Integer;
begin
  {$ifdef IMGLIST_OLDSTYLE}
  Result:=AddCopy(Image,Mask);
  {$else}
  Result := Count;
  Insert(Result, Image, Mask);
  {$endif}
end;

{$ifdef IMGLIST_OLDSTYLE}
function TCustomImageList.AddDirect(Image, Mask: TBitmap): Integer;
begin
  try
    Result := Count;
    Insert(Result, Image, Mask);
  except
    on E: Exception do begin
      DebugLn('TCustomImageList.Add ',E.Message);
      Result := -1; // Ignore exceptions, just return -1
    end;
  end;
end;
{$endif}

{$ifdef IMGLIST_OLDSTYLE}
function TCustomImageList.AddCopy(SrcImage, SrcMask: TBitmap): Integer;
var
  NewImage: TBitmap;
  NewMask: TBitmap;
begin
  NewImage:=nil;
  NewMask:=nil;
  try
    NewImage := TBitmap.Create;
    NewImage.Assign(SrcImage);
    if Assigned(SrcMask) 
    then begin
      NewMask := TBitmap.Create;
      NewMask.Assign(SrcMask);
    end;
    Result:=AddDirect(NewImage, NewMask);
    NewImage:=nil;
    NewMask:=nil;
  finally
    NewImage.Free;
    NewMask.Free;
  end;
end;
{$endif}

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
  {$ifdef IMGLIST_OLDSTYLE}
  SrcImage: TBitmap;
  SrcMask: TBitmap;
  NewImage: TBitmap;
  NewMask: TBitmap;
  {$else}
  p: PRGBAQuad;
  DataSize: Integer;
  OldCount: Integer;
  {$endif}
begin
  if (AValue = nil) or (AValue=Self) then exit;

  {$ifdef IMGLIST_OLDSTYLE}
  BeginUpdate;
  for n := 0 to AValue.Count - 1 do begin
    SrcImage:=TBitmap(AValue.FImageList[n]);
    if SrcImage<>nil then begin
      NewImage:=TBitmap.Create;
      NewImage.Assign(SrcImage);
      SrcMask:=TBitmap(AValue.FMaskList[n]);
      if SrcMask<>nil then begin
        NewMask:=TBitmap.Create;
        NewMask.Assign(SrcMask);
      end else
        NewMask:=nil;
      AddDirect(NewImage,NewMask);
    end;
  end;
  EndUpdate;
  {$else}
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
      for n := OldCount to FCount - AValue.FCount - 1 do
      begin
        TWSCustomImageListClass(WidgetSetClass).Insert(Self, n, p);
        Inc(p, DataSize);
      end;
    end;
  end
  else begin
  {$note implement}
  end;
  {$endif}
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
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;

  if MaskColor <> clNone then
    Bmp.TransparentColor := MaskColor;
  Bmp.LoadFromLazarusResource(ResourceName);
  {$ifdef IMGLIST_OLDSTYLE}
  Result := AddDirect(Bmp, nil);
  {$else}
  Result := Add(Bmp, nil);
  Bmp.Free;
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.AllocBitmap
  Params:  Amount: the amount of free image position which should be availabe 
  Returns: Nothing

  Checks if there is enough space for Amount images,  increases the internal 
  list if necessary .
 ------------------------------------------------------------------------------}
{$ifdef IMGLIST_OLDSTYLE}
procedure TCustomImageList.AllocBitmap(Amount: Integer);
var
  n: Integer;
begin
  Assert(FAllocCount >= FCount, 'Less space allocated than images');
  if FAllocCount >= FCount + Amount
  then Exit;

  // calculate number of blocks, add an extra block for the remainder.
  n := Amount mod FAllocBy;
  if n <> 0
  then Inc(Amount, FAllocBy - n);

  if FBitMap<>nil then
    FBitMap.Height := FBitMap.Height + Amount * FHeight;
  if FMaskBitmap<>nil then
    FMaskBitmap.Height := FBitMap.Height;

  Inc(FAllocCount, Amount);
end;
{$else}
procedure TCustomImageList.AllocData(ACount: Integer);
var
  n: Integer;
begin
  Assert(FAllocCount >= FCount, 'Less space allocated than images');
  if FAllocCount >= ACount
  then Exit;

  // calculate number of blocks, add an extra block for the remainder.
  n := ACount mod FAllocBy;
  if n <> 0
  then Inc(ACount, FAllocBy - n);

  SetLength(FData, ACount * FWidth * FHeight * SizeOf(FData[0]));

  Inc(FAllocCount, ACount);
end;
{$endif}

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
    ImgSrc:=TCustomImageList(Source);
    SetWidthHeight(ImgSrc.Width,ImgSrc.Height);
    Clear;
    AddImages(ImgSrc);
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
  {$ifdef IMGLIST_OLDSTYLE}
  while Count>0 do
    Delete(0);
  FImageList.Clear;
  FMaskList.Clear;
  {$else}
  if HandleAllocated
  then TWSCustomImageListClass(WidgetSetClass).Clear(Self);
  SetLength(FData, 0);
  FAllocCount := 0;
  {$endif}
  
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

  {$ifdef IMGLIST_OLDSTYLE}
  FImageList := TList.Create;  //shane
  FMaskList := TList.Create;
  {$endif}
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
  {$ifdef IMGLIST_OLDSTYLE}
  FImageList := TList.Create;  //shane
  FMaskList := TList.Create;
  {$endif}
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
   { if Filer.Ancestor <> nil then
    begin
      Result := (not (Filer.Ancestor is TCustomImageList) or
        not Equal(TCustomImageList(Filer.Ancestor)));
    end
    else
    }
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
{$ifdef IMGLIST_OLDSTYLE}
var
  Obj : TObject;
{$ENDIF}
begin
  if AIndex = -1
  then begin
    Clear;
    Exit;
  end;
  
  CheckIndex(AIndex);

  {$ifdef IMGLIST_OLDSTYLE}
  Obj:=TObject(fImageList.Items[AIndex]);
  If Assigned(Obj) then
    Obj.Free;
  fImageList.Delete(AIndex);
  Obj:=TObject(fMaskList.Items[AIndex]);
  If Assigned(Obj) then
    Obj.Free;
  fMaskList.Delete(AIndex);
//  ShiftImages(FBitmap.Canvas, Index, 1);
//  ShiftImages(FMaskBitmap.Canvas, Index, 1);
  FCount := fImageList.Count;
  {$else}
  InternalMove(AIndex, FCount - 1, True);
  Dec(FCount);
  if HandleAllocated
  then TWSCustomImageListClass(WidgetSetClass).Delete(Self, AIndex);
  // TODO: adjust allocated data
  {$endif}
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
{$ifdef IMGLIST_OLDSTYLE}
var
  i: integer;
{$ENDIF}
begin
  {$ifdef IMGLIST_OLDSTYLE}
  FBitmap.Free;
  FBitmap:=nil;
  FMaskBitmap.Free;
  FMaskBitmap:=nil;
  for i:=0 to FImageList.Count-1 do TObject(FImageList[i]).Free;
  for i:=0 to FMaskList.Count-1 do TObject(FMaskList[i]).Free;
  FreeThenNil(FImageList);
  FreeThenNil(FMaskList);
  {$endif}
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
{$ifdef IMGLIST_OLDSTYLE}
var
  aBitmap: TBitmap;
{$ENDIF}
begin
  if (FCount = 0) or (AIndex >= FCount) then Exit;

  {$ifdef IMGLIST_OLDSTYLE}
  aBitmap := TBitmap(FImageList[AIndex]);
  ACanvas.Draw(AX, AY, aBitmap);
  {$else}
  HandleNeeded;
  TWSCustomImageListClass(WidgetSetClass).Draw(Self, AIndex, ACanvas, Rect(AX, AY, FWidth, FHeight),
                          BkColor, BlendColor, ADrawEffect, DrawingStyle, ImageType);
  {$endif}
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
procedure TCustomImageList.GetBitmap(Index: Integer; Image: TBitmap);
begin
  GetBitmap(Index, Image, gdeNormal);
end;

procedure TCustomImageList.GetBitmap(Index: Integer; Image: TBitmap;
  AEffect: TGraphicsDrawEffect);
{$ifndef IMGLIST_OLDSTYLE}
var
  RawImg: TRawImage;
  ListImg, DeviceImg: TLazIntfImage;
  ImgHandle, MskHandle: HBitmap;
{$endif}
begin
  if (FCount = 0) or (Image = nil) then Exit;

  {$ifdef IMGLIST_OLDSTYLE}
  Image.Assign(TBitMap(FImageList.Items[Index]));
  {$else}
  GetRawImage(Index, RawImg);
  
  RawImg.PerformEffect(AEffect, True);

  if not RawImage_CreateBitmaps(RawImg, ImgHandle, MskHandle, True)
  then begin
    // bummer, the widgetset doesn't support our 32bit format, try device
    ListImg := TLazIntfImage.Create(RawImg, False);
    DeviceImg := TLazIntfImage.Create(0, 0);
    DeviceImg.DataDescription := GetDescriptionFromDevice(0, FWidth, FHeight);
    DeviceImg.CopyPixels(ListImg);
    DeviceImg.GetRawImage(RawImg);
    RawImage_CreateBitmaps(RawImg, ImgHandle, MskHandle);
    DeviceImg.Free;
    ListImg.Free;
  end;
  Image.SetHandles(ImgHandle, MskHandle);

  RawImg.FreeData;

  {$endif}
end;

procedure TCustomImageList.GetRawImage(Index: Integer; out Image: TRawImage);
begin
  if (FCount = 0) then Exit;
  CheckIndex(Index);
  Image.Init;
  FillDescription(Image.Description);
  Image.DataSize := FWidth * FHeight * SizeOF(FData[0]);
  Image.Data := @FData[Index * FWidth * FHeight];
end;

{------------------------------------------------------------------------------
  procedure TCustomImageList.GetInternalImage(Index: integer; var Image,
    Mask: TBitmap);

  Returns the bitmaps of the n-th image. The Imagelist can combine several
  images into one bitmap (plus one mask), therefore ImageRect contains the
  bounds of the n-th image on the bitmap.
 ------------------------------------------------------------------------------}
{$ifdef IMGLIST_KEEP_EXTRA}
procedure TCustomImageList.GetInternalImage(Index: integer; var Image,
  Mask: TBitmap; var ImageRect: TRect);
begin
  Image:=TBitmap(FImageList[Index]);
  Mask:=TBitmap(FMaskList[Index]);
  ImageRect:=Rect(0,0,Image.Width,Image.Height);
end;
{$endif}

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
  Method:  TCustomImageList.GetIcon
  Params:  Index: the index of the requested image
           Image: an icon as a container for the bitmap
  Returns: Nothing

  Fetches the index'th image into an icon.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.GetIcon(Index: Integer; Image: TIcon);
begin
  if (Index < 0) or (Index >= FCount)
  then raise EInvalidOperation.Create(SInvalidIndex);

  //No Icon Support yet

end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.GetImages
  Params:  Index: the index of the requested image
           Image: a bitmap as a container for the bitmap
           Mask:  a bitmap as a container for the mask
  Returns: Nothing

  Fetches the index'th image and mask into a bitmap.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.GetImages(Index: Integer; const Image, Mask: TBitmap);
begin
{$ifdef IMGLIST_OLDSTYLE}
  with Image do FBitmap.Canvas.CopyRect(
    Rect(0, 0, Width, Height),
    Canvas,
    Rect(0, Index * FHeight, FWidth, (Index + 1) * FHeight)
  );
  with Mask do FMaskBitmap.Canvas.CopyRect(
    Rect(0, 0, Width, Height),
    Canvas,
    Rect(0, Index * FHeight, FWidth, (Index + 1) * FHeight)
  );
{$else}
{$note implement}
{$endif}
end;

{------------------------------------------------------------------------------
  Function: TCustomImageList.HandleAllocated
  Params:   None
  Returns:  True if a handle is allocated

  This function checks if the internal image is allocated
 ------------------------------------------------------------------------------}
{$ifdef IMGLIST_OLDSTYLE}
function TCustomImageList.HandleAllocated: Boolean;
begin
  Result := (FBitmap <> nil);
end;
{$endif}

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

  {$ifdef IMGLIST_OLDSTYLE}
  FBitmap := TBitmap.Create;
  FBitmap.Height := Height;
  FBitmap.Width := Width;
  FMaskBitmap := TBitmap.Create;
  with FMaskBitmap do
  begin
    Height := Height;
    Width := Width;
    Canvas.Brush.Color := clWhite;
    Monochrome := True;
  end;
  {$endif}
end;

procedure TCustomImageList.SetWidthHeight(NewWidth, NewHeight: integer);
begin
  if (FHeight=NewHeight) and (FWidth=NewWidth) then exit;
  FHeight := NewHeight;
  FWidth := NewWidth;
  {$ifdef IMGLIST_OLDSTYLE}
  FBitMap.Width := 0;
  FBitMap.Height := 0;
  AllocBitmap(0);
  {$endif}
  Clear;
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
procedure TCustomImageList.Insert(AIndex: Integer; AImage, AMask: TBitmap);
{$ifndef IMGLIST_OLDSTYLE}
var
  RawImg: TRawImage;
  R: TRect;
  ImgData: PRGBAQuad;
  msk: THandle;
  i, ACount: Integer;
{$endif}
begin
  if AImage = nil then Exit;

  CheckIndex(AIndex, True);

  if (AIndex < 0) then AIndex := 0;

  {$ifdef IMGLIST_OLDSTYLE}
  FImageList.Insert(AIndex,AImage);
  FMaskList.Insert(AIndex,AMask);
  FCount := FImageList.Count;
  {$else}
  ACount := AImage.Width div Width;
  if ACount = 0 then
    ACount := 1;
  Inc(FCount, ACount);
  AllocData(FCount);
  if AIndex < FCount - ACount then
  begin
    for i := 0 to ACount - 1 do
      InternalMove(FCount - i - 1, AIndex + i, True);
  end;
  
  if AMask = nil
  then begin
    if AImage.MaskHandleAllocated
    then msk := AImage.MaskHandle
    else msk := 0;
  end
  else msk := AMask.MaskHandle;

  R := Rect(0, 0, FWidth, FHeight);
  for i := 0 to ACount - 1 do
  begin
    RawImage_FromBitmap(RawImg, AImage.Handle, msk, R);
    ImgData := InternalSetImage(AIndex + i, RawImg);
    if HandleAllocated
    then TWSCustomImageListClass(WidgetSetClass).Insert(Self, AIndex + i, ImgData);
    inc(R.Left, FWidth);
    inc(R.Right, FWidth);
  end;
  {$endif}

  FChanged := true;
  Change;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.InsertIcon
  Params:  Index: the index of the inserted image
           Image: the Icon to be inserted
  Returns: Nothing

  Inserts an icon into the list at the index'th position.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.InsertIcon(Index: Integer; Image: TIcon);
begin
  CheckIndex(Index, True);

  if (Index < 0) then Index := 0;
  
  //No Icon Support yet
  {$ifndef IMGLIST_OLDSTYLE}
  {$note implement}
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.InsertMasked
  Params:  Index: the index of the inserted image
           Image: A bitmap to be inserted
           MaskColor: The color acting as transparant color
  Returns: Nothing

  Adds one or more (bitmap width / imagelist width) bitmaps to the list. 
  Every occurance of MaskColor will be converted to transparent.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.InsertMasked(Index: Integer; Image: TBitmap;
  MaskColor: TColor);
var
  Mask: TBitmap;
begin
  Mask := TBitmap.Create;
  with Mask do
  begin
    Height := Image.Height;
    Width := Image.Width;
    Assign(Image);
    Mask(MaskColor);
  end;
  Insert(Index, Image, Mask);
  {$ifndef IMGLIST_OLDSTYLE}
  Mask.Free;
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.InternalMove
  Params:  CurIndex: the index of the image to be moved
           NewIndex: the new index of the image
  Returns: Nothing

  Moves an image from the CurIndex'th location to NewIndex'th location
  without notifying the widgetset
 ------------------------------------------------------------------------------}
{$ifndef IMGLIST_OLDSTYLE}
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
{$endif}

{------------------------------------------------------------------------------
  Method:  TCustomImageList.InternalSetImage
  Params:  AIndex: the index of the location where the image should be set
           AImage: the new image
  Returns: Pointer to the updated image data

  Copies the imagedata into the FData array and then frees the image.
 ------------------------------------------------------------------------------}
{$ifndef IMGLIST_OLDSTYLE}
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
  RawImg.DataSize := FWidth * FHeight * SizeOF(FData[0]);
  RawImg.Data := PByte(Result);
  if not SrcHasAlpha
  then begin
    // Add maskdata to store copied mask, so an alpha can be created
    RawImg.Description.MaskBitsPerPixel := 1;
    RawImg.Description.MaskBitOrder := riboReversedBits;
    RawImg.Description.MaskLineEnd := rileByteBoundary;
    RawImg.Description.MaskShift := 0;
    RawImg.MaskSize := RawImg.Description.MaskBytesPerLine * FHeight;
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
{$endif}

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

  {$ifdef IMGLIST_OLDSTYLE}
  FImageList.Move(ACurIndex,ANewIndex);
  FMaskList.Move(ACurIndex,ANewIndex);
  {$else}
  InternalMove(ACurIndex, ANewIndex, False);
  if HandleAllocated
  then TWSCustomImageListClass(WidgetSetClass).Move(Self, ACurIndex, ANewIndex);
  {$endif}

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
  {$ifdef IMGLIST_OLDSTYLE}
  CurImage: TBitMap;
  i: Integer;
  {$ENDIF}
  Signature: TImageListSignature;
begin
  //Write signature
  {$ifdef IMGLIST_OLDSTYLE}
  Signature:=SIG_LAZ2;
  {$else}
  Signature:=SIG_LAZ3;
  {$endif}
  AStream.Write(Signature,SizeOf(Signature));

  //Count of image
  WriteLRSInteger(AStream,Count);
  WriteLRSInteger(AStream,Width);
  WriteLRSInteger(AStream,Height);

  //images
  {$ifdef IMGLIST_OLDSTYLE}
  for i:=0 to Count-1 do
  begin
    CurImage:=TBitmap(FImageList[i]);
    //DebugLn('TCustomImageList.WriteData Position=',AStream.Position,' ',CurImage.Width,',',CurImage.Height);
    CurImage.WriteNativeStream(AStream,true,bnXPixmap);
  end;
  {$else}
  AStream.Write(FData[0], FWidth * FHeight * FCount * SizeOf(FData[0]));
  {$endif}
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
      bmp.ReadStream(AStream, True, Size);
      bmp.Transparent := True;
      {$ifdef IMGLIST_OLDSTYLE}
      AddDirect(bmp, nil);
      {$else}
      Add(bmp, nil);
      bmp.Free;
      {$endif}
    end;
  end;

  procedure DoReadLaz2;
  var
    i, NewCount, Size: cardinal;
    bmp: TBitmap;
  begin
    //DebugLn('TCustomImageList.ReadData DoReadLaz2');
    NewCount := ReadLRSCardinal(AStream);
    Width := ReadLRSCardinal(AStream);
    Height := ReadLRSCardinal(AStream);
    //DebugLn('TCustomImageList.ReadData DoReadLaz2 NewCount=',NewCount,' Width=',Width,' Height=',Height);
    for i := 0 to NewCount - 1 do
    begin
      bmp := TBitMap.Create;
      //DebugLn('TCustomImageList.ReadData DoReadLaz2 i=',i,' ',AStream.Position);
      Size:=ReadLRSCardinal(AStream);
      //DebugLn('TCustomImageList.ReadData DoReadLaz2 Size=',Size,' ',AStream.Position);
      bmp.ReadStream(AStream, True, Size);
      {$ifdef IMGLIST_OLDSTYLE}
      AddDirect(bmp, nil);
      {$else}
      Add(bmp, nil);
      bmp.Free;
      {$endif}
    end;
  end;
  
  procedure DoReadLaz3;
  begin
    FCount := ReadLRSCardinal(AStream);
    FWidth := ReadLRSCardinal(AStream);
    FHeight := ReadLRSCardinal(AStream);
    
    {$ifdef IMGLIST_OLDSTYLE}
    AStream.Seek(FWidth * FHeight * FCount * 4, soCurrent);
    {$else}
    AllocData(FCount);
    AStream.ReadBuffer(FData[0], FWidth * FHeight * FCount *  SizeOf(FData[0])) ;
    {$endif}

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

          {$ifdef IMGLIST_OLDSTYLE}
          AddDirect(Img, nil);
          {$else}
          Add(Img, nil);
          Img.Free;
          {$endif}
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
      IntfImage:=TLazIntfImage.Create(0,0);
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
    until not FileExists(Filename);
    CurStreamPos := AStream.Position;
    DebugLn('TCustomImageList.ReadData Saving stream to ',Filename);
    fs:=TFileStream.Create(Filename,fmCreate);
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
procedure TCustomImageList.Replace(AIndex: Integer; AImage, AMask: TBitmap);
var
  {$ifdef IMGLIST_OLDSTYLE}
  DestinationRect, SourceRect: TRect;
  {$else}
  RawImage: TRawImage;
  R: TRect;
  ImgData: PRGBAQuad;
  msk: THandle;
  {$endif}
begin
  if (AIndex < 0) then AIndex := 0;
  CheckIndex(AIndex);

  {$ifdef IMGLIST_OLDSTYLE}
  DestinationRect := Rect(0, AIndex * FHeight, FWidth, (AIndex + 1) * FHeight);
  SourceRect := Rect(0, 0, FWidth, FHeight);
  CopyImage(FBitmap.Canvas, AImage.Canvas, DestinationRect, SourceRect);
  if AMask <> nil
  then CopyImage(FMaskBitmap.Canvas, AMask.Canvas, DestinationRect, SourceRect)
  else FMaskBitmap.Canvas.FillRect(DestinationRect);
  {$else}
  if AMask = nil
  then msk := 0
  else msk := AMask.Handle;
  R := Rect(0, 0, FWidth, FHeight);
  RawImage_FromBitmap(RawImage, AImage.Handle, msk, R);
  ImgData := InternalSetImage(AIndex, RawImage);
  if HandleAllocated
  then TWSCustomImageListClass(WidgetSetClass).Replace(Self, AIndex, ImgData);
  {$endif}

  FChanged := true;
  Change;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.ReplaceIcon
  Params:  Index: the index of the replaceded image
           Image: an icon image
  Returns: Nothing.

  Replaces the index'th image with the image given.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.ReplaceIcon(Index: Integer; Image: TIcon);
begin
  if (Index > FCount)
  then raise EInvalidOperation.Create(SInvalidIndex);

  if (Index < 0) then Index := 0;

  // No Icon suppport yet
  {$ifndef IMGLIST_OLDSTYLE}
  {$note implement}
  {$endif}
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
procedure TCustomImageList.ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor);
begin
  if (Index >= FCount)
  then raise EInvalidOperation.Create(SInvalidIndex);

  if (Index < 0) then Index := 0;

  {$ifndef IMGLIST_OLDSTYLE}
  {$note implement}
  {$endif}
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

{------------------------------------------------------------------------------
  Method:  TCustomImageList.ShiftImages
  Params:  Source: source canvas on which the images are moved
           Start: start of the image to shift 
           Shift: number of images to shift
  Returns: Nothing

  Internal routine to move images on the internal image list.
 ------------------------------------------------------------------------------}
{$ifdef IMGLIST_OLDSTYLE}
procedure TCustomImageList.ShiftImages(const Source: TCanvas;
  Start, Shift: Integer);
var
  FMoveBitmap: TBitmap;
begin
  try
    FMoveBitmap := TBitmap.Create;
    with FMoveBitmap do
    begin
      Width := FWidth;
      Height := (1 + FCount - Start) * FHeight;
    end;
    FMoveBitmap.Canvas.CopyRect(
      Rect(0, 0, FWidth, FMoveBitmap.Height),
      Source,
      Rect(0, Start * FHeight, FWidth, (FCount + 1) * FHeight)
    );
    Source.CopyRect(
      Rect(0, (Start + Shift) * FHeight, FWidth, (FCount + Shift + 1) * FHeight),
      FMoveBitmap.Canvas,
      Rect(0, 0, FWidth, FMoveBitmap.Height)
    );
  finally
    FMoveBitmap.Free;
  end;
end;
{$endif}

procedure TCustomImageList.StretchDraw(Canvas: TCanvas; Index: Integer; ARect: TRect; Enabled: Boolean);
var
  bmp: TBitmap;
  {$ifdef IMGLIST_OLDSTYLE}
  msk: TBitmap;
  {$ENDIF}
begin
  if (FCount = 0) or (Index >= FCount) then Exit;

  {$ifdef IMGLIST_OLDSTYLE}
  bmp := TBitmap(FImageList[Index]);
  msk := TBitmap(FMaskList[Index]);
  StretchMaskBlt(Canvas.Handle, ARect.Left, ARect.Top, ARect.Right - ARect.Left,
    ARect.Bottom - ARect.Top, bmp.Handle, 0, 0, FWidth, FHeight, msk.Handle, 0, 0, SRCCOPY);
  {$else}
  {$note implement}
  // temp workaround
  bmp := TBitmap.Create;
  GetBitmap(Index, bmp);
  Canvas.StretchDraw(ARect,  bmp);
  bmp.Free;
  {$endif}
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
function TCustomImageList.WSCreateHandle(AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TWSCustomImageListClass(WidgetSetClass).CreateHandle(Self, FCount, FAllocBy, FWidth, FHeight, @FData[0]);
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
