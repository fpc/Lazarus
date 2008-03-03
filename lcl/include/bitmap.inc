{%MainUnit ../graphics.pp}

{******************************************************************************
                                     TBitMap
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

function TestStreamBitmapNativeType(const AStream: TStream): TBitmapNativeType;
begin
  if (AStream is TResourceStream) then
    Result := bnDIB
  else if TestStreamIsBMP(AStream) then
    Result := bnWinBitmap
  else if TestStreamIsXPM(AStream) then
    Result := bnXPixmap
  else if TestStreamIsIcon(AStream) then
    Result := bnIcon
  else if TestStreamIsCursor(AStream) then
    Result := bnCursor
  else
    Result := bnNone;
end;

function TestStreamIsBMP(const AStream: TStream): boolean;
var
  Signature: array[0..1] of Char;
  ReadSize: Integer;
  OldPosition: TStreamSeekType;
begin
  OldPosition:=AStream.Position;
  ReadSize:=AStream.Read(Signature, SizeOf(Signature));
  Result:=(ReadSize=2) and (Signature[0]='B') and (Signature[1]='M');
  //debugln('TestStreamIsBMP ',DbgStr(Signature[0]),' ',DbgStr(Signature[1]));
  AStream.Position:=OldPosition;
end;

procedure TBitMap.Assign(Source: TPersistent);
var
  SrcBitmap: TBitmap absolute Source;
  SrcFPImage: TFPCustomImage absolute Source;
  IntfImage: TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
begin
  if Source = Self then exit;

  if Source is TBitmap
  then begin
    // TBitmap can share image data
    
    // -> check if already shared
    if SrcBitmap.FImage=FImage then exit;// already sharing

    // MWE: dont call ChangingAll, since it will create a new image,
    // which we will replace anyway.
    // ChangingAll(Self);

    //DebugLn(['TBitMap.Assign Self=',ClassName,' Source=',Source.ClassName,' HandleAllocated=',HandleAllocated,' Canvas=',DbgSName(FCanvas)]);
    FTransparent := SrcBitmap.Transparent;

    //DebugLn('TBitMap.Assign A  RefCount=',FImage.RefCount);
    // image is not shared => new image data
    // -> free canvas (interface handles)
    FreeCanvasContext;
    // release old FImage
    FImage.Release;
    // share FImage with assigned graphic
    FImage := SrcBitmap.FImage;
    FImage.Reference;

    // Paul: we can share image data only is ClassTypes are the same
    // if ClassTypes are differ then we should create copy of data and free save stream
    if Source.ClassType <> ClassType then
    begin
      UnshareImage(True);
      FreeSaveStream;
    end;

    //DebugLn(['TBitMap.Assign B ',Width,',',Height,' ',HandleAllocated,' RefCount=',FImage.RefCount]);
    Changed(Self);

    Exit;
  end;

  if Source is TFPCustomImage
  then begin
    // MWE: no need for a changeall, the sethandles will handle this
    // ChangingAll(Self);
    
    IntfImage := TLazIntfImage.Create(0,0);
    try
      if HandleAllocated
      then IntfImage.DataDescription := GetDescriptionFromBitmap(Handle, 0, 0)
      else IntfImage.DataDescription := GetDescriptionFromDevice(0, 0, 0);
      IntfImage.Assign(SrcFPImage);
      IntfImage.CreateBitmaps(ImgHandle, ImgMaskHandle);
      SetHandles(ImgHandle, ImgMaskHandle);
    finally
      IntfImage.Free;
    end;
    Changed(Self);
    
    Exit;
  end;

  if Source = nil
  then begin
    FreeSaveStream;
    SetWidthHeight(0, 0);
    Exit;
  end;
  
  
  // fall back to default
  inherited Assign(Source);
end;

procedure TBitmap.Draw(DestCanvas: TCanvas; const DestRect: TRect);
var
  UseMaskHandle: HBitmap;
  SrcDC: hDC;
  DestDC: hDC;
begin
  if (DestRect.Right<=DestRect.Left) or (DestRect.Bottom<=DestRect.Top)
  or (Width=0) or (Height=0) then begin
    exit;
  end;
  HandleNeeded;
  if HandleAllocated then begin
    if Transparent then
      UseMaskHandle:=MaskHandle
    else
      UseMaskHandle:=0;
    SrcDC:=Canvas.GetUpdatedHandle([csHandleValid]);
    DestCanvas.Changing;
    DestDC:=DestCanvas.GetUpdatedHandle([csHandleValid]);
    StretchMaskBlt(DestDC,
            DestRect.Left,DestRect.Top,
            DestRect.Right-DestRect.Left,DestRect.Bottom-DestRect.Top,
            SrcDC,0,0,Width,Height, UseMaskHandle,0,0,DestCanvas.CopyMode);
    DestCanvas.Changed;
  end;
end;

constructor TBitmap.Create;
begin
  inherited Create;
  FPixelFormat := pfDevice;
  FImage := TBitmapImage.Create;
  FImage.Reference;
  FTransparentColor := clDefault; // for Delphi compatibility. clDefault means:
                                  // use Left,Bottom pixel as transparent pixel
end;

destructor TBitMap.Destroy;
begin
  FreeCanvasContext;
  FImage.Release;
  FImage:=nil;
  FreeThenNil(FCanvas);
  inherited Destroy;
end;

procedure TBitMap.FreeCanvasContext;
begin
  if (FCanvas <> nil) then TBitmapCanvas(FCanvas).FreeDC;
end;

function TBitmap.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
    CreateCanvas;
  Result := FCanvas;
end;

procedure TBitmap.CreateCanvas;
begin
  if (FCanvas <> nil) or (bmisCreatingCanvas in FInternalState) then exit;
  Include(FInternalState,bmisCreatingCanvas);
  try
    FCanvas := TBitmapCanvas.Create(Self);
    FCanvas.OnChanging := @Changing;
    FCanvas.OnChange := @Changed;
  finally
    Exclude(FInternalState,bmisCreatingCanvas);
  end;
end;

procedure TBitMap.FreeImage;
begin
  Handle := 0;
end;

function TBitmap.HandleAllocated: boolean;
begin
  Result := (FImage <> nil) and (FImage.FHandle <> 0);
end;

function TBitmap.MaskHandleAllocated: boolean;
begin
  Result := (FImage <> nil) and (FImage.FMaskHandle <> 0);
end;

function TBitmap.PaletteAllocated: boolean;
begin
  Result := (FImage <> nil) and (FImage.FPalette <> 0);
end;

procedure TBitmap.CreateFromBitmapHandles(ABitmap, AMask: HBitmap; const ARect: TRect);
var
  RawImage: TRawImage;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  //DebugLn('TBitmap.CreateFromBitmapHandles A SrcRect=',dbgs(SrcRect));
  if not RawImage_FromBitmap(RawImage, ABitmap, AMask, ARect) then
    raise EInvalidGraphicOperation.Create('TBitmap.CreateFromBitmapHandles Get RawImage');
  ImgHandle:=0;
  ImgMaskHandle:=0;
  try
    //DebugLn('TBitmap.CreateFromBitmapHandles B SrRect=',dbgs(SrcRect));
    if not RawImage_CreateBitmaps(RawImage, ImgHandle, ImgMaskHandle) then
      raise EInvalidGraphicOperation.Create('TBitmap.CreateFromBitmapHandles Create bitmaps');
    SetHandles(ImgHandle, ImgMaskHandle);
    ImgHandle:=0;
    ImgMaskHandle:=0;
  finally
    RawImage.FreeData;
    if ImgHandle<>0 then DeleteObject(ImgHandle);
    if ImgMaskHandle<>0 then DeleteObject(ImgMaskHandle);
  end;
end;

procedure TBitmap.LoadFromDevice(DC: HDC);
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
    // feed HBitmap into a TBitmap
    SetHandles(ImgHandle, ImgMaskHandle);
    ImgHandle:=0;
    ImgMaskHandle:=0;
  finally
    IntfImg.Free;
    if ImgHandle<>0 then DeleteObject(ImgHandle);
    if ImgMaskHandle<>0 then DeleteObject(ImgMaskHandle);
  end;
end;

function TBitmap.LazarusResourceTypeValid(const ResourceType: string): boolean;
var
  ResType: String;
begin
  if Length(ResourceType) < 3 then Exit(False);

  ResType := UpperCase(ResourceType);
  case ResType[1] of
    'B': begin
      Result := (ResType = 'BMP') or (ResType = 'BITMAP');
    end;
    'X': begin
      Result := Restype = 'XPM';
    end;
  else
    Result := False;
  end;
end;

procedure TBitMap.Mask(ATransparentColor: TColor);
begin
  CreateMask(ATransparentColor);
end;

function TBitmap.GetHandle: HBITMAP;
begin
  if not FImage.HandleAllocated then
    HandleNeeded;
  Result := FImage.FHandle;
end;

function TBitmap.GetHandleType: TBitmapHandleType;
begin
  Result:=FImage.GetHandleType;
end;

function TBitmap.GetMaskHandle: HBITMAP;
begin
  MaskHandleNeeded;
  Result := FImage.FMaskHandle;
end;

procedure TBitmap.SetHandleType(Value: TBitmapHandleType);
begin
  if HandleType=Value then exit;
  DebugLn('TBitmap.SetHandleType TBitmap.SetHandleType not implemented');
end;

procedure TBitmap.SetMonochrome(const AValue: Boolean);
begin
  if Monochrome=AValue then exit;
  if AValue then begin
    FreeImage;
    FImage.FDIB.dsbm.bmPlanes := 1;
    FImage.FDIB.dsbm.bmBitsPixel := 1;
    fPixelFormat:=pf1bit;
  end;
end;

procedure TBitmap.SetPixelFormat(const AValue: TPixelFormat);
begin
  if AValue=PixelFormat then exit;
  FreeImage;
  FPixelFormat:=AValue;
end;

procedure TBitmap.SetTransparentColor(const AValue: TColor);
begin
  if FTransparentColor = AValue then exit;
  FTransparentColor := AValue;
  if FTransparentMode <> tmFixed then Exit;

  CreateMask;
end;

procedure TBitmap.UpdatePixelFormat;
begin
  FPixelFormat := FImage.GetPixelFormat;
end;

procedure TBitmap.Changed(Sender: TObject);
begin
  //FMaskBitsValid := False;
  inherited Changed(Sender);
end;

procedure TBitmap.Changing(Sender: TObject);
// called before the bitmap is modified
// -> make sure the handle is unshared (otherwise the modifications will also
//    modify all copies)
begin
  UnshareImage(true);
  FImage.FDIB.dsbmih.biClrUsed := 0;
  FImage.FDIB.dsbmih.biClrImportant := 0;
  FreeSaveStream;
end;

procedure TBitmap.ChangingAll(Sender: TObject);
begin
  UnshareImage(false);
  Changing(Sender);
end;

procedure TBitMap.HandleNeeded;
const
  BITCOUNT_MAP: array[TPixelFormat] of Byte = (
//  pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit, pfCustom
    0, 1, 4, 8, 15, 16, 24, 32, 0
  );
var
  BitCount: Byte;
  UseWidth,
  UseHeight : Longint;
  OldChangeEvent: TNotifyEvent;
  DC: HDC;
  BI: PBitmapInfo;
begin
  if (FImage.FHandle <> 0) then exit;

  // if the bitmap was loaded, create a handle from stream
  if (FImage.FDIBHandle = 0) and (FImage.FSaveStream <> nil)
  then begin
    FImage.FSaveStream.Position := 0;
    OldChangeEvent := OnChange;
    try
      OnChange := nil;
      LoadFromStream(FImage.FSaveStream); // Current FImage may be destroyed here
    finally
      OnChange := OldChangeEvent;
    end;
  end;

  if (FImage.FHandle <> 0) then exit;
  // otherwise create a default handle

  BitCount := BITCOUNT_MAP[PixelFormat];
  if BitCount = 0
  then begin
    if PixelFormat = pfDevice
    then BitCount := Min(ScreenInfo.ColorDepth, 24) // prevent creation of default alpha channel
    else raise EInvalidOperation.Create(rsUnsupportedBitmapFormat);
  end;

  UseWidth := Width;
  UseHeight := Height;
  if UseWidth < 1 then UseWidth := 1;
  if UseHeight < 1 then UseHeight := 1;

  FImage.FDIB.dsBm.bmBits := nil;
  if BitCount = 1
  then begin
    FImage.FHandle := CreateBitmap(UseWidth, UseHeight, 1, BitCount, nil);
  end
  else begin
    // on windows we need a DIB section
    BI := @FImage.FDIB.dsBmih;
    FillChar(BI^.bmiHeader, SizeOf(BI^.bmiHeader), 0);
    BI^.bmiHeader.biSize := SizeOf(BI^.bmiHeader);
    BI^.bmiHeader.biWidth := UseWidth;
    BI^.bmiHeader.biHeight := -UseHeight; // request top down
    BI^.bmiHeader.biPlanes := 1;
    BI^.bmiHeader.biBitCount := BitCount;
    BI^.bmiHeader.biCompression := BI_RGB;
    DC := GetDC(0);
    FImage.FHandle := CreateDIBSection(DC, BI^, DIB_RGB_COLORS, FImage.FDIB.dsBm.bmBits, 0, 0);
    ReleaseDC(0, DC);
    
    // fallback for other widgetsets not implementing CreateDIBSection
    // we need the DIB section anyway someday if we want a scanline
    if FImage.FHandle = 0
    then FImage.FHandle := CreateBitmap(UseWidth, UseHeight, 1, BitCount, nil);
  end;

  //DebugLn('TBitMap.HandleNeeded Self=',DbgS(Self),' FImage.FHandle=',DbgS(FImage.FHandle),' n=',n);
  FImage.FDIB.dsbm.bmWidth := Width;
  FImage.FDIB.dsbm.bmHeight := Height;
end;

procedure TBitMap.MaskHandleNeeded;
begin
  if (FImage.FMaskHandle <> 0) then exit;
  // not now, breaks alpha images, since they report themselves as transparent
  // while no mask is needed
  // CreateMask;
end;

procedure TBitMap.LoadFromStream(Stream: TStream);
begin
  ReadStream(Stream, true, Stream.Size - Stream.Position);
end;

procedure TBitmap.GetSupportedSourceMimeTypes(List: TStrings);
begin
  if (ClassType=TBitmap) or (ClassType=TPixmap) or (ClassType=TIcon) then
  begin
    List.Clear;
    List.Add(PredefinedClipboardMimeTypes[pcfBitmap]);
    List.Add(PredefinedClipboardMimeTypes[pcfDelphiBitmap]);
    List.Add(PredefinedClipboardMimeTypes[pcfPixmap]);
  end else
    inherited GetSupportedSourceMimeTypes(List);
end;

function TBitmap.GetTransparent: Boolean;
begin
  {$note add better check for transparency }
  // MWE: now tharansparency is set when a maskhandle is assigned, the user can
  // override this by setting it to false, so no mask is used,
  // however this meganism ignores the possible alpha channel, so for now 32bit
  // bitmaps are considered transparent
  // todos:
  //  check for device transparency
  //  check for transparency through palette etc.
  Result := FTransparent or (FPixelFormat = pf32bit);
end;

function TBitmap.GetDefaultMimeType: string;
begin
  if (ClassType=TBitmap) or (ClassType=TPixmap) or (ClassType=TIcon) then begin
    if FImage.SaveStream<>nil then begin
      case FImage.SaveStreamType of
      bnXPixmap: Result:=PredefinedClipboardMimeTypes[pcfPixmap];
      else
        Result:=PredefinedClipboardMimeTypes[pcfBitmap];
      end;
    end else
      Result:=PredefinedClipboardMimeTypes[pcfBitmap];
  end else
    Result:=inherited GetDefaultMimeType;
end;

class function TBitmap.GetFileExtensions: string;
begin
  Result:='bmp;xpm';
end;

procedure TBitmap.LoadFromXPMFile(const Filename: String);
Begin
  LoadFromFile(Filename);
end;

procedure TBitmap.LoadFromIntfImage(IntfImage: TLazIntfImage);
var
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  IntfImage.CreateBitmaps(ImgHandle, ImgMaskHandle);
  SetHandles(ImgHandle, ImgMaskHandle);
end;

function TBitmap.GetMonochrome: Boolean;
begin
  with FImage.FDIB.dsbm do
    Result := (bmPlanes = 1) and (bmBitsPixel = 1);
end;

procedure TBitMap.PaletteNeeded;
begin
  if (FImage.FPalette <> 0) then exit;
  
  // ToDo
  //CreatePalette;
end;

procedure TBitmap.UnshareImage(CopyContent: boolean);
var
  NewImage: TBitmapImage;
  OldImage: TBitmapImage;
  IntfImage: TLazIntfImage;
begin
  if (FImage.RefCount>1) then begin
    //DebugLn('TBitmap.UnshareImage ',ClassName,' ',Width,',',Height,' ',DbgS(Self));
    // release old FImage and create a new one
    NewImage:=TBitmapImage.Create;
    try
      NewImage.Reference;
      if CopyContent and FImage.HandleAllocated
      and (Width>0) and (Height>0) then begin
        // copy content
        IntfImage:=TLazIntfImage.Create(0,0);
        try
          IntfImage.LoadFromBitmap(FImage.FHandle,FImage.FMaskHandle,Width,Height);
          IntfImage.CreateBitmaps(NewImage.FHandle, NewImage.FMaskHandle, not IntfImage.HasMask);
          FillChar(NewImage.FDIB, SizeOf(NewImage.FDIB), 0);
          if NewImage.HandleAllocated then
            GetObject(NewImage.FHandle, SizeOf(NewImage.FDIB), @NewImage.FDIB);
        finally
          IntfImage.Free;
        end;
      end;
      FreeCanvasContext;
      OldImage:=FImage;
      FImage:=NewImage;
      //DebugLn('TBitMap.UnshareImage Self=',DbgS(Self),' FImage.FHandle=',DbgS(FImage.FHandle));
      NewImage:=nil; // transaction sucessful
      OldImage.Release;
    finally
      // in case something goes wrong, keep old and free new
      NewImage.Free;
    end;
    //DebugLn('TBitmap.UnshareImage END ',ClassName,' ',Width,',',Height,' ',DbgS(Self));
  end;
end;

procedure TBitmap.FreeSaveStream;
begin
  if FImage.FSaveStream=nil then exit;
  //DebugLn(['TBitmap.FreeSaveStream A ',ClassName,' ',FImage.FSaveStream.Size]);
  UnshareImage(false);
  FreeAndNil(FImage.FSaveStream);
  FImage.SaveStreamType:=bnNone;
  FImage.SaveStreamClass:=nil;
end;

function TBitmap.GetBitmapNativeType: TBitmapNativeType;
begin
  Result := bnWinBitmap;
end;

procedure TBitmap.ReadStream(Stream: TStream; UseSize: boolean; Size: Longint);

  procedure RaiseInvalidBitmapHeader;
  begin
    debugln('TBitmap.ReadStream.RaiseInvalidBitmapHeader ',
      '"',dbgMemStream(TCustomMemoryStream(Stream),30),'"');
    raise EInOutError.Create(
      'TBitmap.ReadStream: Invalid bitmap format (bmp,xpm,ico)');
  end;
  
  procedure RaiseInvalidSize;
  begin
    debugln('TBitmap.ReadStream.RaiseInvalidSize ',
     ' Size=',dbgs(Size),' Stream.Position=',dbgs(Stream.Position),
     ' Stream.Size=',dbgs(Stream.Size));
    raise EInOutError.Create(
      'TBitmap.ReadStream: Invalid size of bitmap stream (bmp,xpm,ico,cur)');
  end;

var
  WorkStream: TStream;
  StreamType: TBitmapNativeType;
  ReaderClass: TFPCustomImageReaderClass;
  MemStream: TCustomMemoryStream;
  GetSize: Int64;
  OldPosition: Int64;
begin
  //debugln('TBitmap.ReadStream Stream=',DbgSName(Stream),' Stream.Size=',dbgs(Stream.Size),' Stream.Position=',dbgs(Stream.Position),' UseSize=',dbgs(UseSize),' Size=',dbgs(Size));
  WorkStream:=nil;
  try
    // create mem stream if not already done (to read the image type)
    if (Stream is TCustomMemoryStream) then
    begin
      WorkStream := Stream;
    end
    else
    if UseSize then
    begin
      WorkStream := TMemoryStream.Create;
      TMemoryStream(WorkStream).SetSize(Size);
      WorkStream.CopyFrom(Stream, Size);
      WorkStream.Position:=0;
    end
    else
    begin
      // size is unknown and type is not TMemoryStream
      // ToDo: create cache stream from Stream
      WorkStream := Stream;
    end;
    // get image type
    if WorkStream is TCustomMemoryStream then
    begin
      MemStream:=TCustomMemoryStream(WorkStream);
      OldPosition:=MemStream.Position;
      GetSize:=MemStream.Size;
      // workaround for TMemoryStream bug, reading Size sets Position to 0
      MemStream.Position:=OldPosition;
      if UseSize and (Size>GetSize-OldPosition) then
        RaiseInvalidSize;
      StreamType := TestStreamBitmapNativeType(MemStream);
    end else
      StreamType := bnWinBitmap;
    //debugln('TBitmap.ReadStream ',dbgs(ord(StreamType)),' UseSize=',dbgs(UseSize),' Size=',dbgs(Size),' Stream=',DbgSName(Stream));
    ReaderClass := nil;
    case StreamType of
      bnDIB:        ReaderClass := TLazReaderDIB;
      bnWinBitmap:  ReaderClass := TLazReaderBMP;
      bnXPixmap:    ReaderClass := TLazReaderXPM;
      bnIcon:       ReaderClass := TLazReaderIcon;
      bnCursor:     ReaderClass := TLazReaderCursor;
    else
      RaiseInvalidBitmapHeader;
    end;
    ReadStreamWithFPImage(WorkStream, UseSize, Size, ReaderClass);
  finally
    if WorkStream <> Stream then
      WorkStream.Free;
  end;
end;

procedure TBitmap.LoadFromMimeStream(Stream: TStream; const MimeType: string);
begin
  if (ClassType=TBitmap) or (ClassType=TPixmap) or (ClassType=TIcon) then begin
    if (AnsiCompareText(MimeType,PredefinedClipboardMimeTypes[pcfBitmap])=0)
    or (AnsiCompareText(MimeType,PredefinedClipboardMimeTypes[pcfDelphiBitmap])=0)
    or (AnsiCompareText(MimeType,PredefinedClipboardMimeTypes[pcfPixmap])=0) then
    begin
      LoadFromStream(Stream);
      exit;
    end;
  end;
  inherited LoadFromMimeStream(Stream, MimeType);
end;

procedure TBitmap.SetWidthHeight(NewWidth, NewHeight: integer);
begin
  if (FImage.FDIB.dsbm.bmHeight <> NewHeight)
  or (FImage.FDIB.dsbm.bmWidth <> NewWidth) then
  begin
    FreeImage;
    FImage.FDIB.dsbm.bmWidth := NewWidth;
    FImage.FDIB.dsbm.bmHeight := NewHeight;
    Changed(Self);
  end;
end;

procedure TBitmap.WriteStream(Stream: TStream; WriteSize: Boolean);
begin
  WriteStreamWithFPImage(Stream,WriteSize,nil);
end;

procedure TBitmap.StoreOriginalStream(Stream: TStream; Size: integer);
var
  MemStream: TMemoryStream;
begin
  if Stream<>FImage.SaveStream then begin
    MemStream:=TMemoryStream.Create;
    //debugln('TBitmap.StoreOriginalStream Size=',dbgs(Size),' Stream.Position=',dbgs(Stream.Position),' Stream.Size=',dbgs(Stream.Size));
    MemStream.SetSize(Size);
    MemStream.CopyFrom(Stream,Size);
    FreeSaveStream;
    FImage.FSaveStream:=MemStream;
  end;
  FImage.SaveStreamType:=bnNone;
  FImage.SaveStreamClass:=nil;
  FImage.SaveStream.Position:=0;
end;

function TBitmap.CanReadGraphicStreams(AClass: TFPCustomImageWriterClass
  ): boolean;
begin
  Result:=(AClass=GetDefaultFPWriter)
    or (((ClassType=TBitmap) or (ClassType=TPixmap))
        and ((AClass=TFPWriterBMP) or (AClass=TLazWriterXPM)));
end;

procedure TBitmap.SetHandles(ABitmap, AMask: HBITMAP);
var
  IsChanged: Boolean;
begin
  IsChanged := False;

  if FImage.FHandle <> ABitmap
  then begin
    // free old handles
    FreeCanvasContext;
    UnshareImage(false);
    FreeSaveStream;
    FImage.FreeHandle;
    // get the properties from new bitmap
    FImage.FHandle := ABitmap;
    //DebugLn('TBitMap.SetHandle Self=',DbgS(Self),' FImage.FHandle=',DbgS(FImage.FHandle));
    FillChar(FImage.FDIB, SizeOf(FImage.FDIB), 0);
    if FImage.FHandle <> 0 then
      GetObject(FImage.FHandle, SizeOf(FImage.FDIB), @FImage.FDIB);
      
    IsChanged := True;
  end;
  
  

  if FImage.FMaskHandle <> AMask
  then begin
    if not IsChanged
    then begin
      // unshare image and free canvas handle
      UnshareImage(true);
      FreeCanvasContext;
      // free old mask handle
      FImage.FreeMaskHandle;
    end;
    FImage.FMaskHandle := AMask;

    IsChanged := True;
  end;
  
  
  
  if IsChanged then Changed(Self);
end;

{------------------------------------------------------------------------------
  procedure TBitmap.ReadStreamWithFPImage(Stream: TStream; UseSize: boolean;
    Size: Longint; ReaderClass: TFPCustomImageReaderClass);
    
  Clear old bitmap and read new bitmap form stream.
  Stream: source stream. After reading Position will be at end of bitmap.
  UseSize: if True, Size is used. If False then Size is calculated
           automatically.
  Size: Only used when UseSize=True. This amount of bytes is read.
------------------------------------------------------------------------------}
procedure TBitmap.ReadStreamWithFPImage(Stream: TStream; UseSize: boolean;
  Size: Longint; ReaderClass: TFPCustomImageReaderClass);
var
  IntfImg: TLazIntfImage;
  ImgReader: TFPCustomImageReader;
  ImgHandle, ImgMaskHandle: HBitmap;
  NewSaveStream: TMemoryStream;
  SrcStream: TStream;
  OldStreamPosition: TStreamSeekType;
  ImgSize: TStreamSeekType;
  
  procedure StoreOriginal(OriginalStream: TStream; Size: integer);
  begin
    StoreOriginalStream(OriginalStream,Size);
    NewSaveStream:=FImage.SaveStream;
    NewSaveStream.Position:=0;
    // hide SaveStream during reading (so that it won't be destroyed)
    FImage.SaveStream:=nil;
  end;
  
begin
  //debugln('TBitmap.ReadStreamWithFPImage Stream.Size=',dbgs(Stream.Size),' Stream.Position=',dbgs(Stream.Position),' UseSize=',dbgs(UseSize),' Size=',dbgs(Size));

  UnshareImage(false);
  if UseSize and (Size = 0) then begin
    FreeSaveStream;
    SetWidthHeight(0,0);
    exit;
  end;

  IntfImg:=nil;
  ImgReader:=nil;
  NewSaveStream:=nil;
  if UseSize then begin
    // Use the given 'Size' parameter
    StoreOriginal(Stream,Size);
    SrcStream:=NewSaveStream;
  end
  else begin
    FreeSaveStream;
    SrcStream:=Stream;
  end;
  try
    // read image
    IntfImg := TLazIntfImage.Create(0,0);
    {$note set pixelformat based on image, not device}
    // add an extention to the reader, so that we h get called after the header is read
    // the next will cause that all images are loaded in pfDevice format.
    // This is incompatible with delphi
    IntfImg.DataDescription := GetDescriptionFromDevice(0, 0, 0);
    ImgReader := ReaderClass.Create;
    InitFPImageReader(IntfImg, ImgReader);
    OldStreamPosition:=SrcStream.Position;
    IntfImg.LoadFromStream(SrcStream, ImgReader);
    if (ImgReader is TLazReaderBMP) and (TLazReaderBMP(ImgReader).MaskMode = lrmmAuto)
    then FTransparentColor := FPColorToTColor(TLazReaderBMP(ImgReader).MaskColor);

    ImgSize := SrcStream.Position - OldStreamPosition;
    if UseSize
    then begin
      // set position
      if Size<>ImgSize then
        SrcStream.Position:=OldStreamPosition+Size;
    end
    else begin
      // now the size is known -> store stream
      //DebugLn('TBitmap.ReadStreamWithFPImage SrcStream=',SrcStream.ClassName,' ImgSize=',ImgSize);
      SrcStream.Position:=OldStreamPosition;
      StoreOriginal(SrcStream,integer(ImgSize));
    end;
    
    FinalizeFPImageReader(ImgReader);
    ImgMaskHandle := 0;
    IntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, not IntfImg.HasMask);
    SetHandles(ImgHandle, ImgMaskHandle);
  finally
    // set save stream
    FImage.SaveStream:=NewSaveStream;
    if (ReaderClass=TFPReaderBMP) or (ReaderClass=TLazReaderBMP)
    then begin
      FImage.SaveStreamType:=bnWinBitmap;
      FImage.SaveStreamClass:=TFPWriterBMP;
    end
    else if ReaderClass=TLazReaderXPM
    then begin
      FImage.SaveStreamType:=bnXPixmap;
      FImage.SaveStreamClass:=TLazWriterXPM;
    end;
    //DebugLn('TBitmap.ReadStreamWithFPImage ',DbgSName(FImage.SaveStreamClass),' ',DbgSName(ReaderClass));
    // clean up
    IntfImg.Free;
    ImgReader.Free;
  end;
end;

procedure TBitmap.WriteStreamWithFPImage(Stream: TStream; WriteSize: boolean;
  WriterClass: TFPCustomImageWriterClass);

  procedure DoWriteStreamSize(DestStream: TStream; Size: longint);
  begin
    //DebugLn('DoWriteStreamSize ',ClassName,' Size=',Size,' WriteSize=',WriteSize);
    if WriteSize then
      DestStream.WriteBuffer(Size, SizeOf(Size));
  end;

  procedure DoWriteOriginal;
  var
    BytesWritten: Int64;
  begin
    DoWriteStreamSize(Stream, NtoLE(LongInt(FImage.SaveStream.Size)));
    FImage.SaveStream.Position:=0;
    if Stream is TMemoryStream then
      TMemoryStream(Stream).SetSize(Stream.Position+FImage.SaveStream.Size);
    BytesWritten:=Stream.CopyFrom(FImage.SaveStream,FImage.SaveStream.Size);
    if BytesWritten<>FImage.SaveStream.Size then
      raise FPImageException.Create(rsErrorWhileSavingBitmap);
  end;

var
  MemStream: TMemoryStream;
  IntfImg: TLazIntfImage;
  ImgWriter: TFPCustomImageWriter;
  RawImage: TRawImage;
begin
  //DebugLn(['WriteStreamWithFPImage Self=',DbgS(Self),' ',Width,',',Height,' Using SaveStream=',(FImage.SaveStream<>nil) and (FImage.SaveStream.Size>0)]);
  if  (FImage.SaveStream<>nil)
  and (FImage.SaveStream.Size>0)
  and (FImage.SaveStreamType = GetBitmapNativeType)
  and ((FImage.SaveStreamType<>bnNone) or CanReadGraphicStreams(FImage.SaveStreamClass))
  then begin
    // it's a stream format, that this graphic class can read
    // (important for restore)
    DoWriteOriginal;
    exit;
  end;
  //DebugLn('WriteStreamWithFPImage no savestream, creating stream ...');
  
  // write image to temporary stream
  MemStream:=TMemoryStream.Create;
  IntfImg:=nil;
  ImgWriter:=nil;
  try
    IntfImg:=TLazIntfImage.Create(0,0);
    IntfImg.LoadFromBitmap(Handle, FImage.FMaskHandle);
    
    if WriterClass=nil
    then begin
      {$note todo choose the correct writer}
      // Bitmaps support alpha. TBitmaps supports more than these 2 formats anyway.
      // So setting the writer based on a mask is nonsense imo (MWE)

      // automatically use a TFPCustomImageWriterClass
      // .bmp does not support transparency
      // Delphi uses a trick and stores the transparent color in the bottom, left
      // pixel. This changes the bitmap and requires to know the transparency
      // on load.
      // The LCL TBitmap is able to load .xpm and .bmp images. .xpm supports
      // transparency. So, if the images has transparency use .xpm.
      IntfImg.GetRawImage(RawImage);
      if RawImage.IsMasked(true)
      then WriterClass:=TLazWriterXPM
      else WriterClass:=TFPWriterBMP;
    end;

    ImgWriter:=WriterClass.Create;
    InitFPImageWriter(IntfImg, ImgWriter);
    IntfImg.SaveToStream(MemStream,ImgWriter);
    FinalizeFPImageWriter(ImgWriter);
    FreeAndNil(ImgWriter);
    FreeAndNil(IntfImg);
    // save stream, so that further saves will be fast
    MemStream.Position:=0;
    FreeSaveStream;
    FImage.SaveStream:=MemStream;
    if WriterClass=TLazWriterXPM then
      FImage.SaveStreamType:=bnXPixmap
    else if WriterClass=TFPWriterBMP then
      FImage.SaveStreamType:=bnWinBitmap
    else
      FImage.SaveStreamType:=bnNone;
    FImage.SaveStreamClass:=WriterClass;
    MemStream:=nil;
    // copy savestream to destination stream
    DoWriteOriginal;
    {SetLength(s,FImage.SaveStream.Size);
    FImage.SaveStream.Position:=0;
    FImage.SaveStream.Read(s[1],length(s));
    DebugLn(s);}
  finally
    MemStream.Free;
    IntfImg.Free;
    ImgWriter.Free;
  end;
end;

procedure TBitmap.InitFPImageReader(IntfImg: TLazIntfImage; ImgReader: TFPCustomImageReader);
var
  LazReader: TLazReaderBMP absolute ImgReader;
begin
  if not (ImgReader is TLazReaderBMP) then Exit;

  // TransparentMode
  //   tmAuto: use left bottom pixel
  //   tmFixed: use color
  //
  // TransparentColor:
  //   clDefault: use left, bottom pixel color as transparent color (*)
  //   clNone: load image opaque (*)
  //   otherwise: use TransparentColor as transparent color
  //
  //   (*) these are Lazarus extentions
  
  if (TransparentMode = tmAuto) or (TransparentColor = clDefault)
  then begin
    LazReader.MaskMode := lrmmAuto;
  end
  else begin
    if TransparentColor = clNone
    then begin
      LazReader.MaskMode := lrmmNone;
    end
    else begin
      LazReader.MaskMode := lrmmColor;
      LazReader.MaskColor := TColorToFPColor(TransparentColor);
    end;
  end;
end;

procedure TBitmap.InitFPImageWriter(IntfImg: TLazIntfImage; ImgWriter: TFPCustomImageWriter);
(*const
  PixelFormatToBppMap: array[TPixelFormat] of integer =
  (
{ pfDevice } 32,
{ pf1bit   } 1,
{ pf4bit   } 4,
{ pf8bit   } 8,
{ pf15bit  } 15,
{ pf16bit  } 16,
{ pf24bit  } 24,
{ pf32bit  } 32,
{ pfCustom } 32
  );*)
var
  BmpWriter: TFPWriterBMP absolute ImgWriter;
//  bpp: integer;
begin
  if ImgWriter is TFPWriterBMP then
  begin
    // set BPP
    //BmpWriter.BitsPerPixel := PixelFormatToBppMap[PixelFormat];
    // we can also look at PixelFormat, but it can be inexact
    BmpWriter.BitsPerPixel := IntfImg.DataDescription.Depth;
  end;
end;

procedure TBitmap.FinalizeFPImageReader(ImgReader: TFPCustomImageReader);
begin

end;

procedure TBitmap.FinalizeFPImageWriter(ImgWriter: TFPCustomImageWriter);
begin

end;

procedure TBitMap.SaveToStream(Stream: TStream);
begin
  WriteStream(Stream, False);
end;

procedure TBitmap.SetHandle(Value: HBITMAP);
begin
  SetHandles(Value, FImage.FMaskHandle);
end;

procedure TBitmap.SetMaskHandle(NewMaskHandle: HBITMAP);
begin
  SetHandles(FImage.FHandle, NewMaskHandle);
end;

// release handles without freeing them
// useful for creating a HBitmap
function TBitmap.ReleaseHandle: HBITMAP;
begin
  HandleNeeded;
  FreeCanvasContext;
  Result := FImage.ReleaseHandle;
end;

function TBitmap.ReleaseMaskHandle: HBITMAP;
begin
  MaskHandleNeeded;
  FreeCanvasContext;
  Result := FImage.ReleaseMaskHandle;
end;

function TBitmap.ReleasePalette: HPALETTE;
begin
  // ToDo
  {
   PaletteNeeded;
   FreeCanvasContext;
   Result := FImage.ReleasePaletteHandle;
  }
  Result := 0;
end;

class function TBitmap.GetFPReaderForFileExt(const FileExtension: string
  ): TFPCustomImageReaderClass;
begin
  Result:=nil;
  if (AnsiCompareText(ClassName,'TBitmap')=0)
  or (AnsiCompareText(ClassName,'TPixmap')=0) then begin
    if (AnsiCompareText(FileExtension,'.bmp')=0)
    or (AnsiCompareText(FileExtension,'bmp')=0) then
      Result:=TLazReaderBMP
    else if (AnsiCompareText(FileExtension,'.xpm')=0)
    or (AnsiCompareText(FileExtension,'xpm')=0) then
      Result:=TLazReaderXPM;
  end;
end;

class function TBitmap.GetFPWriterForFileExt(const FileExtension: string
  ): TFPCustomImageWriterClass;
begin
  Result:=nil;
  if (AnsiCompareText(ClassName,'TBitmap')=0)
  or (AnsiCompareText(ClassName,'TPixmap')=0) then begin
    if (AnsiCompareText(FileExtension,'.bmp')=0)
    or (AnsiCompareText(FileExtension,'bmp')=0) then
      Result:=TFPWriterBMP
    else if (AnsiCompareText(FileExtension,'.xpm')=0)
    or (AnsiCompareText(FileExtension,'xpm')=0) then
      Result:=TLazWriterXPM;
  end;
end;

class function TBitmap.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  if (AnsiCompareText(ClassName,'TBitmap')=0) then
    Result:=TLazReaderBMP
  else
    Result:=nil;
end;

class function TBitmap.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  if ClassType=TBitmap then
    Result:=TFPWriterBMP
  else
    Result:=nil;
end;

procedure TBitmap.WriteNativeStream(Stream: TStream; WriteSize: Boolean;
  SaveStreamType: TBitmapNativeType);
var
  Writer: TFPCustomImageWriterClass;
begin
  case SaveStreamType of
  bnWinBitmap: Writer:=TFPWriterBMP;
  bnXPixmap: Writer:=TLazWriterXPM;
  else
    RaiseGDBException('Invalid SaveStreamType');
  end;
  WriteStreamWithFPImage(Stream,WriteSize,Writer);
  if (FImage.SaveStream<>nil) and (FImage.SaveStreamType=bnNone) then begin
    FImage.SaveStreamType:=SaveStreamType;
    FImage.SaveStreamClass:=Writer;
  end;
end;

procedure TBitmap.CreateIntfImage(var IntfImage: TLazIntfImage);
begin
  IntfImage:=nil;
  IntfImage:=TLazIntfImage.Create(0,0);
  IntfImage.LoadFromBitmap(Handle,MaskHandle);
end;

function TBitmap.CreateIntfImage: TLazIntfImage;
begin
  Result:=nil;
  CreateIntfImage(Result);
end;

procedure TBitmap.CreateMask(AColor: TColor);
var
  IntfImage: TLazIntfImage;
  x, y, stopx, stopy: Integer;
  ImgHandle, MskHandle: HBitmap;
  TransColor: TColor;
begin
  //DebugLn(['TBitmap.CreateMask ',Width,'x',Height,' ',Transparent,' ',dbgs(ord(TransparentMode)),' ',dbgs(TransparentColor)]);
  if (Width = 0)
  or (Height = 0)
  or (AColor = clNone)
  or (  (FTransparentMode = tmFixed)
    and (FTransparentColor = clNone)
    and (AColor = clDefault)
  )
  then begin
    SetHandles(FImage.FHandle, 0);
    Exit;
  end;
  
  {$note todo: move to IntfImage}
  IntfImage := TLazIntfImage.Create(0, 0);
  try
    // load from bitmap needs a mask handle otherwise no mask description is
    // created.
    MskHandle := FImage.FMaskHandle;
    if MskHandle = 0
    then MskHandle := CreateBitmap(Width, Height, 1, 1, nil);
    IntfImage.LoadFromBitmap(Handle, MskHandle);
    if MskHandle <> FImage.FMaskHandle
    then DeleteObject(MskHandle);
    
    stopx := IntfImage.Width - 1;
    stopy := IntfImage.Height - 1;

    if AColor = clDefault
    then begin
      if (FTransparentMode = tmFixed) and (FTransparentColor <> clDefault)
      then TransColor := ColorToRGB(FTransparentColor)
      else TransColor := FPColorToTColor(IntfImage.Colors[0, stopy]);
    end
    else TransColor := ColorToRGB(AColor);

    for y := 0 to stopy do
      for x := 0 to stopx do
        IntfImage.Masked[x,y] := FPColorToTColor(IntfImage.Colors[x,y]) = TransColor;

    IntfImage.CreateBitmaps(ImgHandle, MskHandle);
    SetHandles(FImage.FHandle, MskHandle);
    DeleteObject(ImgHandle);
  finally
    IntfImage.Free;
  end;
end;

function TBitmap.GetEmpty: boolean;
begin
  Result:=FImage.IsEmpty;
end;

function TBitmap.GetHeight: Integer;
begin
  Result := FImage.FDIB.dsbm.bmHeight;
end;

function TBitmap.GetPalette: HPALETTE;
begin
  PaletteNeeded;
  Result := FImage.FPalette;
end;

function TBitmap.GetResourceType: TResourceType;
begin
  Result := RT_BITMAP;
end;

function TBitmap.GetWidth: Integer;
begin
  Result := FImage.FDIB.dsbm.bmWidth;
end;

procedure TBitmap.ReadData(Stream: TStream);
var
  Size: Longint;
begin
  Stream.Read(Size, SizeOf(Size));
  Size := LEtoN(Size);
  ReadStream(Stream, true, Size);
end;

procedure TBitmap.WriteData(Stream: TStream);
begin
  WriteStream(Stream, True);
end;

procedure TBitmap.SetWidth(NewWidth: Integer);
begin
  SetWidthHeight(NewWidth,Height);
end;

procedure TBitmap.SetHeight(NewHeight: Integer);
begin
  SetWidthHeight(Width,NewHeight);
end;

procedure TBitmap.SetPalette(Value: HPALETTE);
begin
  inherited SetPalette(Value);
end;

procedure TBitmap.SetTransparentMode(Value: TTransparentMode);
begin
  if Value = TransparentMode then exit;
  FTransparentMode := Value;
  CreateMask;
end;

// included by graphics.pp

