{%MainUnit ../graphics.pp}

{******************************************************************************
                                     TRasterImage
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

procedure TRasterImage.Assign(Source: TPersistent);
var
  SrcImage: TRasterImage absolute Source;
  SrcFPImage: TFPCustomImage absolute Source;
  IntfImage: TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
begin
  if Source = Self then exit;

  if Source is TRasterImage
  then begin
    // TRasterImage can share image data
    
    FTransparent := SrcImage.Transparent;
    FTransparentMode := SrcImage.TransparentMode;
    FTransparentColor := SrcImage.TransparentColor;

    // -> check if already shared
    if SrcImage.FSharedImage <> FSharedImage
    then begin
      // image is not shared => new image data
      // -> free canvas (interface handles)
      FreeCanvasContext;
      // release old FImage
      FSharedImage.Release;
      // share FImage with assigned graphic
      FSharedImage := SrcImage.FSharedImage;
      FSharedImage.Reference;

      // We only can share images of the same type and when neither we or source is updating
      // Since we "share" it first, the unshare code will create a copy
      if (FUpdateCount > 0)
      or (SrcImage.FUpdateCount > 0)
      or not CanShareImage(SrcImage.GetSharedImageClass)
      then begin
        UnshareImage(True);
        FreeSaveStream;
      end;
    end;

    if FUpdateCount = 0
    then Changed(Self);

    Exit;
  end;

  if Source is TFPCustomImage
  then begin
    // todo: base on rawimage
  
    IntfImage := TLazIntfImage.Create(0,0);
    try
      if BitmapHandleAllocated
      then IntfImage.DataDescription := GetDescriptionFromBitmap(BitmapHandle, 0, 0)
      else IntfImage.DataDescription := GetDescriptionFromDevice(0, 0, 0);
      IntfImage.Assign(SrcFPImage);
      IntfImage.CreateBitmaps(ImgHandle, ImgMaskHandle);
      SetHandles(ImgHandle, ImgMaskHandle);
    finally
      IntfImage.Free;
    end;
    if FUpdateCount = 0
    then Changed(Self);
    
    Exit;
  end;

  // fall back to default
  inherited Assign(Source);
end;

procedure TRasterImage.BeginUpdate(ACanvasOnly: Boolean);
begin
  if FUpdateCount = 0
  then begin
    UnshareImage(True);
    FUpdateCanvasOnly := ACanvasOnly;
  end
  else begin
    // if we are updating all, then requesting a canvas only won't change it
    // if we are updating canvas only, then requesting all is an error
    if FUpdateCanvasOnly and not ACanvasOnly
    then raise EInvalidGraphicOperation.Create('cannot begin update all when canvas only update in progress');
  end;

  Inc(FUpdateCount);
end;

procedure TRasterImage.Clear;
begin
  if not Empty then
  begin
    FreeSaveStream;
    SetSize(0, 0);
    if FUpdateCount = 0
    then Changed(Self);
  end;
end;

procedure TRasterImage.BitmapHandleNeeded;
var
  ImgHandle, ImgMaskHandle: HBitmap;
  ImagePtr: PRawImage;
  DevImage: TRawImage;
  DevDesc: TRawImageDescription;
  SrcImage, DstImage: TLazIntfImage;
  QueryFlags: TRawImageQueryFlags;
  W, H: Integer;
begin
  if BitmapHandleAllocated then exit;

  ImagePtr := GetRawImage;
  ImgMaskHandle := 0;

  // create a device compatible image
  W := Width;
  if W < 1 then W := 1;
  H := Height;
  if H < 1 then H := 1;

  if ImagePtr^.Description.Depth = 1
  then QueryFlags := [riqfMono]
  else QueryFlags := [riqfRGB];
  if ImagePtr^.Description.AlphaPrec <> 0
  then Include(QueryFlags, riqfAlpha);
  if ImagePtr^.Description.MaskBitsPerPixel <> 0
  then Include(QueryFlags, riqfMask);
  QueryDescription(DevDesc, QueryFlags, W, H);

  if DevDesc.IsEqual(ImagePtr^.Description)
  then begin
    // image is compatible, so use it
    DstImage := nil;
  end
  else begin
    // create compatible copy
    SrcImage := TLazIntfImage.Create(ImagePtr^, False);
    DstImage := TLazIntfImage.Create(0, 0);
    // create mask for alphachannel when device has no alpha support
    if (DevDesc.AlphaPrec = 0) and (riqfAlpha in QueryFlags)
    then begin
      //add mask if not already queried
      if not (riqfMask in QueryFlags)
      then QueryDescription(DevDesc, [riqfMask, riqfUpdate]);
      DstImage.DataDescription := DevDesc;
      DstImage.CopyPixels(SrcImage, 0, 0, True, $8000);
    end
    else begin
      DstImage.DataDescription := DevDesc;
      DstImage.CopyPixels(SrcImage);
    end;
    SrcImage.Free;
    DstImage.GetRawImage(DevImage);
    ImagePtr := @DevImage;
  end;

  try
    if not RawImage_CreateBitmaps(ImagePtr^, ImgHandle, ImgMaskHandle, DevDesc.MaskBitsPerPixel = 0)
    then begin
      DebugLn('TRasterImage.BitmapHandleNeeded: Unable to create handles, using default');
      // create a default handle
      ImgHandle := CreateDefaultBitmapHandle(DevDesc);
    end;
    UpdateHandles(ImgHandle, ImgMaskHandle);
  finally
    DstImage.Free;
  end;
end;

function TRasterImage.CanShareImage(AClass: TSharedRasterImageClass): Boolean;
begin
  // We only can share images of the same type.
  // And if we are some "end" graphic type
  Result := (AClass <> TSharedCustomBitmap)
        and (AClass = GetSharedImageClass);
end;


procedure TRasterImage.Draw(DestCanvas: TCanvas; const DestRect: TRect);
var
  UseMaskHandle: HBitmap;
  SrcDC: hDC;
  DestDC: hDC;
begin
  if (DestRect.Right<=DestRect.Left) or (DestRect.Bottom<=DestRect.Top)
  or (Width=0) or (Height=0)
  then Exit;

  BitmapHandleNeeded;
  if not BitmapHandleAllocated then Exit;

  if Transparent then
    UseMaskHandle:=MaskHandle
  else
    UseMaskHandle:=0;

  SrcDC := Canvas.GetUpdatedHandle([csHandleValid]);
  DestCanvas.Changing;
  DestDC := DestCanvas.GetUpdatedHandle([csHandleValid]);
  StretchMaskBlt(DestDC,
          DestRect.Left,DestRect.Top,
          DestRect.Right-DestRect.Left,DestRect.Bottom-DestRect.Top,
          SrcDC,0,0,Width,Height, UseMaskHandle,0,0,DestCanvas.CopyMode);
  DestCanvas.Changed;
end;

procedure TRasterImage.EndUpdate;
begin
  if FUpdatecount = 0
  then raise EInvalidGraphicOperation.Create('Endupdate while no update in progress');
  
  Dec(FUpdatecount);
  if FUpdatecount > 0 then Exit;

  if not FUpdateCanvasOnly
  then FreeCanvasContext;
  FreeSaveStream;
  Changed(Self);
end;

constructor TRasterImage.Create;
begin
  inherited Create;
  FSharedImage := GetSharedImageClass.Create;
  FSharedImage.Reference;
  FTransparentColor := clDefault; // for Delphi compatibility. clDefault means:
                                  // use Left,Bottom pixel as transparent pixel
end;

destructor TRasterImage.Destroy;
begin
  FreeCanvasContext;
  FSharedImage.Release;
  FSharedImage := nil;
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

procedure TRasterImage.FreeCanvasContext;
begin
  if (FCanvas <> nil) then TBitmapCanvas(FCanvas).FreeDC;
end;

function TRasterImage.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
    CreateCanvas;
  Result := FCanvas;
end;

procedure TRasterImage.CreateCanvas;
begin
  if FCanvas <> nil then Exit;

  FCanvas := TBitmapCanvas.Create(Self);
  FCanvas.OnChanging := @CanvasChanging;
  FCanvas.OnChange := @Changed;
end;

procedure TRasterImage.FreeImage;
begin
  SetHandle(0);
end;

procedure TRasterImage.LoadFromBitmapHandles(ABitmap, AMask: HBitmap; ARect: PRect);
var
  RawImage: TRawImage;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  //DebugLn('TRasterImage.CreateFromBitmapHandles A SrcRect=',dbgs(SrcRect));
  if not RawImage_FromBitmap(RawImage, ABitmap, AMask, ARect) then
    raise EInvalidGraphicOperation.Create('TRasterImage.LoadFromBitmapHandles Get RawImage');
  ImgHandle:=0;
  ImgMaskHandle:=0;
  try
    //DebugLn('TRasterImage.CreateFromBitmapHandles B SrRect=',dbgs(SrcRect));
    if not RawImage_CreateBitmaps(RawImage, ImgHandle, ImgMaskHandle) then
      raise EInvalidGraphicOperation.Create('TRasterImage.LoadFromBitmapHandles Create bitmaps');
    SetHandles(ImgHandle, ImgMaskHandle);
    ImgHandle:=0;
    ImgMaskHandle:=0;
  finally
    RawImage.FreeData;
    if ImgHandle<>0 then DeleteObject(ImgHandle);
    if ImgMaskHandle<>0 then DeleteObject(ImgMaskHandle);
  end;
end;

procedure TRasterImage.LoadFromDevice(DC: HDC);
var
  IntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  ImgHandle:=0;
  ImgMaskHandle:=0;
  IntfImg:=nil;
  try
    // create the interface image
    IntfImg:=TLazIntfImage.Create(0,0);
    // get a snapshot
    IntfImg.LoadFromDevice(DC);
    // create HBitmap
    IntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle);
    // feed HBitmap into a TRasterImage
    SetHandles(ImgHandle, ImgMaskHandle);
    ImgHandle:=0;
    ImgMaskHandle:=0;
  finally
    IntfImg.Free;
    if ImgHandle<>0 then DeleteObject(ImgHandle);
    if ImgMaskHandle<>0 then DeleteObject(ImgMaskHandle);
  end;
end;

procedure TRasterImage.Mask(ATransparentColor: TColor);
begin
  CreateMask(ATransparentColor);
end;

procedure TRasterImage.SetTransparentColor(AValue: TColor);
begin
  if TransparentColor = AValue then exit;
  FTransparentColor := AValue;
  if TransparentMode <> tmFixed then Exit;

  CreateMask;
end;

procedure TRasterImage.Changed(Sender: TObject);
begin
  if FUpdateCount > 0 then Exit;

  //FMaskBitsValid := False;
  if Sender = FCanvas
  then FreeSaveStream;

  inherited Changed(Sender);
end;

procedure TRasterImage.CanvasChanging(Sender: TObject);
begin
  if FUpdateCount > 0 then Exit;

  // called before the canvas is modified
  // -> make sure the handle is unshared (otherwise the modifications will also
  //    modify all copies)
  // -> Savestream will be freed when changed (so it can be loaded by canvas)
  UnshareImage(True);
end;

procedure TRasterImage.LoadFromStream(AStream: TStream);
begin
  LoadFromStream(AStream, AStream.Size - AStream.Position);
end;

procedure TRasterImage.LoadFromStream(AStream: TStream; ASize: Cardinal);
var
  WorkStream: TMemoryStream;
  OldPos, NewSize: Int64;
begin
  UnshareImage(False);
  if ASize = 0
  then begin
    FreeSaveStream;
    SetSize(0, 0);
    Exit;
  end;

  WorkStream := nil;
  try
    WorkStream := TMemoryStream.Create;
    WorkStream.SetSize(ASize);
    OldPos := AStream.Position;
    WorkStream.CopyFrom(AStream, ASize);
    WorkStream.Position := 0;
    FreeSaveStream;
    ReadStream(WorkStream, ASize);
    NewSize := WorkStream.Position;
    if NewSize < ASize
    then begin
      // the size given was different than the image loaded
      // MWE: original code adjusted the streampos, do we stil need this ?
      AStream.Position := OldPos + NewSize;
      WorkStream.SetSize(NewSize);
    end;
    // Store our worstream as savestream
    FSharedImage.SaveStream := WorkStream;
    WorkStream := nil;
  finally
    // if something went wrong, free the workstream
    WorkStream.Free;
  end;
  if FUpdateCount = 0
  then Changed(Self);
end;

procedure TRasterImage.GetSupportedSourceMimeTypes(List: TStrings);
begin
  if (ClassType=TBitmap) or (ClassType=TPixmap) or (ClassType=TCustomIcon) then
  begin
    List.Clear;
    List.Add(PredefinedClipboardMimeTypes[pcfBitmap]);
    List.Add(PredefinedClipboardMimeTypes[pcfDelphiBitmap]);
    List.Add(PredefinedClipboardMimeTypes[pcfPixmap]);
  end else
    inherited GetSupportedSourceMimeTypes(List);
end;

function TRasterImage.GetTransparent: Boolean;
begin
  {$IFDEF VerboseLCLTodos}{$note add better check for transparency}{$ENDIF}
  // MWE: now tharansparency is set when a maskhandle is assigned, the user can
  // override this by setting it to false, so no mask is used,
  // however this meganism ignores the possible alpha channel, so for now 32bit
  // bitmaps are considered transparent
  // todos:
  //  check for device transparency
  //  check for transparency through palette etc.
  Result := FTransparent;
end;

function TRasterImage.GetWidth: Integer;
var
  Desc: PRawImageDescription;
begin
  Desc := GetRawImageDescription;
  if (Desc = nil) or (Desc^.Format = ricfNone)
  then Result := 0
  else Result := Desc^.Width;
end;

function TRasterImage.HandleAllocated: boolean;
begin
  Result := FSharedImage.FHandle <> 0;
end;

function TRasterImage.GetMimeType: string;
begin
  {$IFDEF VerboseLCLTodos}{$note: implement}{$ENDIF}

  (*
  if (ClassType=TBitmap) or (ClassType=TPixmap) or (ClassType=TCustomIcon) then begin
    if FSharedImage.SaveStream<>nil then begin
      case FSharedImage.SaveStreamType of
      bnXPixmap: Result:=PredefinedClipboardMimeTypes[pcfPixmap];
      else
        Result:=PredefinedClipboardMimeTypes[pcfBitmap];
      end;
    end else
      Result:=PredefinedClipboardMimeTypes[pcfBitmap];
  end else
  *)

  Result:=inherited GetMimeType;
end;

procedure TRasterImage.LoadFromIntfImage(IntfImage: TLazIntfImage);
var
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  IntfImage.CreateBitmaps(ImgHandle, ImgMaskHandle);
  SetHandles(ImgHandle, ImgMaskHandle);
end;

procedure TRasterImage.FreeSaveStream;
begin
  if FSharedImage.FSaveStream = nil then exit;
  //DebugLn(['TRasterImage.FreeSaveStream A ',ClassName,' ',FImage.FSaveStream.Size]);
  UnshareImage(false);
  FreeAndNil(FSharedImage.FSaveStream);
end;

procedure TRasterImage.LoadFromMimeStream(AStream: TStream; const AMimeType: string);
begin
  {$IFDEF VerboseLCLTodos}{$note Make oo}{$ENDIF}
  if (ClassType=TBitmap) or (ClassType=TPixmap) or (ClassType=TCustomIcon) then begin
    if (AnsiCompareText(AMimeType,PredefinedClipboardMimeTypes[pcfBitmap])=0)
    or (AnsiCompareText(AMimeType,PredefinedClipboardMimeTypes[pcfDelphiBitmap])=0)
    or (AnsiCompareText(AMimeType,PredefinedClipboardMimeTypes[pcfPixmap])=0) then
    begin
      LoadFromStream(AStream);
      exit;
    end;
  end;
  inherited LoadFromMimeStream(AStream, AMimeType);
end;

procedure TRasterImage.SaveToStream(AStream: TStream);
  procedure Error;
  begin
    raise FPImageException.Create(rsErrorWhileSavingBitmap);
  end;
var
  Size, BytesWritten: Int64;
begin
  SaveStreamNeeded;
  if FSharedImage.SaveStream = nil
  then Error;

  FSharedImage.SaveStream.Position := 0;
  Size := FSharedImage.SaveStream.Size;
  if AStream is TMemoryStream
  then TMemoryStream(AStream).SetSize(AStream.Position + Size);

  BytesWritten := AStream.CopyFrom(FSharedImage.SaveStream, Size);
  if BytesWritten <> Size
  then Error;
end;

procedure TRasterImage.SetBitmapHandle(AValue: HBITMAP);
begin
  if MaskHandleAllocated
  then SetHandles(AValue, MaskHandle)
  else SetHandles(AValue, 0);
end;

procedure TRasterImage.SetHandle(AValue: THandle);
begin
  if FSharedImage.FHandle = AValue then Exit;
  FreeCanvasContext;
  UnshareImage(False);
  FSharedImage.FHandle := AValue;
  if FUpdateCount = 0
  then Changed(Self);
end;

procedure TRasterImage.SetMaskHandle(AValue: HBITMAP);
begin
  if BitmapHandleAllocated
  then SetHandles(BitmapHandle, AValue)
  else SetHandles(0, AValue);
end;

// release handles without freeing them
// useful for creating a HBitmap
function TRasterImage.ReleaseBitmapHandle: HBITMAP;
begin
  BitmapHandleNeeded;
  FreeCanvasContext;
  Result := InternalReleaseBitmapHandle;
end;

function TRasterImage.ReleaseMaskHandle: HBITMAP;
begin
  MaskHandleNeeded;
  FreeCanvasContext;
  Result := InternalReleaseMaskHandle;
end;

function TRasterImage.ReleasePalette: HPALETTE;
begin
  PaletteNeeded;
  FreeCanvasContext;
  Result := InternalReleasePalette;
end;

procedure TRasterImage.SaveStreamNeeded;
var
  WorkStream: TMemoryStream;
begin
  if FUpdateCount > 0
  then raise EInvalidGraphicOperation.Create('Cannot save image wile update in progress');


  if FSharedImage.SaveStream <> nil then Exit;
  
  WorkStream := TMemoryStream.Create;
  try
    WriteStream(WorkStream);
    // Store our worstream as savestream
    FSharedImage.SaveStream := WorkStream;
    WorkStream := nil;
  finally
    // if something went wrong, free the workstream
    WorkStream.Free;
  end;
end;

function TRasterImage.CreateIntfImage: TLazIntfImage;
begin
  {$IFDEF VerboseLCLTodos}{$note todo: create based on rawimage}{$ENDIF}
  Result := TLazIntfImage.Create(0,0);
  Result.LoadFromBitmap(BitmapHandle, MaskHandle);
end;

procedure TRasterImage.CreateMask(AColor: TColor);
var
  IntfImage: TLazIntfImage;
  x, y, stopx, stopy: Integer;
  ImgHandle, MskHandle: HBitmap;
  TransColor: TColor;
begin
  //DebugLn(['TRasterImage.CreateMask ',Width,'x',Height,' ',Transparent,' ',dbgs(ord(TransparentMode)),' ',dbgs(TransparentColor)]);
  if (Width = 0)
  or (Height = 0)
  or (AColor = clNone)
  or (  (TransparentMode = tmFixed)
    and (TransparentColor = clNone)
    and (AColor = clDefault)
  )
  then begin
    MaskHandle := 0;
    Exit;
  end;
  
  {$IFDEF VerboseLCLTodos}{$note todo: move to IntfImage}{$ENDIF}
  IntfImage := TLazIntfImage.Create(0, 0);
  try
    // load from bitmap needs a mask handle otherwise no mask description is
    // created.
    if MaskHandleAllocated
    then MskHandle := MaskHandle 
    else MskHandle := CreateBitmap(Width, Height, 1, 1, nil);
    IntfImage.LoadFromBitmap(BitmapHandle, MskHandle);
    if not MaskHandleAllocated
    then DeleteObject(MskHandle);
    
    stopx := IntfImage.Width - 1;
    stopy := IntfImage.Height - 1;

    if AColor = clDefault
    then begin
      if (TransparentMode = tmFixed) and (TransparentColor <> clDefault)
      then TransColor := ColorToRGB(TransparentColor)
      else TransColor := FPColorToTColor(IntfImage.Colors[0, stopy]);
    end
    else TransColor := ColorToRGB(AColor);

    for y := 0 to stopy do
      for x := 0 to stopx do
        IntfImage.Masked[x,y] := FPColorToTColor(IntfImage.Colors[x,y]) = TransColor;

    IntfImage.CreateBitmaps(ImgHandle, MskHandle);
    MaskHandle := MskHandle;
    DeleteObject(ImgHandle);
  finally
    IntfImage.Free;
  end;
end;

function TRasterImage.GetEmpty: boolean;
begin
  Result := FSharedImage.IsEmpty;
end;

function TRasterImage.GetHandle: THandle;
begin
  HandleNeeded;
  Result := FSharedImage.FHandle;
end;

function TRasterImage.GetHeight: Integer;
var
  Desc: PRawImageDescription;
begin
  Desc := GetRawImageDescription;
  if (Desc = nil) or (Desc^.Format = ricfNone)
  then Result := 0
  else Result := Desc^.Height;
end;

class function TRasterImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedRasterImage;
end;

procedure TRasterImage.ReadData(Stream: TStream);
  function GetImageClass: TRasterImageClass;
  const
    // need to repeat here since they aren't defined yet
    IconSignature: array [0..3] of char = #0#0#1#0;
    CursorSignature: array [0..3] of char = #0#0#2#0;
  var
    Sig: array[0..7] of Char;
    Position: Int64;
  begin
    Position := Stream.Position;
    Stream.Read(Sig[0], SizeOf(Sig));
    Stream.Position := Position;

    if (Sig[0] = 'B') and (Sig[1] = 'M') then Exit(TBitmap);
    if CompareMem(@Sig[0], @PNGcomn.Signature[0], 8) then Exit(TPortableNetworkGraphic);
    if CompareMem(@Sig[0], @IconSignature[0], 4) then Exit(TIcon);
    if CompareMem(@Sig[0], @CursorSignature[0], 4) then Exit(TCursorImage);
    if TestStreamIsXPM(Stream) then Exit(TPixmap);

    Result := nil;
  end;

var
  Size: Longint;
  ImageClass: TRasterImageClass;
  Image: TRasterImage;
begin
  Stream.Read(Size, SizeOf(Size));
  Size := LEtoN(Size);
  
  // pre laz 0.9.26 there was no strict relation between graphic format and
  // classtype, so we need to check if we need some conversion
  if Size >= 8
  then ImageClass := GetImageClass
  else ImageClass := nil;

  if (ImageClass = nil) or ClassType.InheritsFrom(ImageClass)
  then begin
    // no conversion needed, or it wasn't a known "old" format
    LoadFromStream(Stream, Size);
    Exit;
  end;
  
  Image := ImageClass.Create;
  Image.LoadFromStream(Stream, Size);
  try
    Assign(Image);
  finally
    Image.Free;
  end;
end;

procedure TRasterImage.WriteData(Stream: TStream);
  procedure Error;
  begin
    raise FPImageException.Create(rsErrorWhileSavingBitmap);
  end;
var
  Size: Longint;
begin
  SaveStreamNeeded;
  if FSharedImage.SaveStream = nil
  then Error;

  Size := NtoLE(FSharedImage.SaveStream.Size);
  Stream.Write(Size, SizeOf(Size));
  SaveToStream(Stream);
end;

procedure TRasterImage.SetWidth(AWidth: Integer);
begin
  SetSize(AWidth, Height);
end;

procedure TRasterImage.SetHeight(AHeight: Integer);
begin
  SetSize(Width, AHeight);
end;

procedure TRasterImage.SetTransparentMode(AValue: TTransparentMode);
begin
  if AValue = TransparentMode then exit;
  FTransparentMode := AValue;
  CreateMask;
end;

// included by graphics.pp

