{%MainUnit ../graphics.pp}

{******************************************************************************
                                     TBitMap
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  if TestStreamIsBMP(AStream) then
    Result:=bnWinBitmap
  else if TestStreamIsXPM(AStream) then
    Result:=bnXPixmap
  else if TestStreamIsIcon(AStream) then
    Result := bnIcon
  else
    Result:=bnNone;
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
  AStream.Position:=OldPosition;
end;

procedure TBitMap.Assign(Source: TPersistent);
var
  SrcBitmap: TBitmap;
  {$IFNDEF DisableFPImage}
  SrcFPImage: TFPCustomImage;
  IntfImage: TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
  {$ENDIF}
begin
  if Source=Self then exit;
  if Source is TBitmap then begin
    Changing(Self);
    //DebugLn('TBitMap.Assign ',ClassName,' ',Source.ClassName);
    // TBitmap can share image data
    // -> check if already shared
    SrcBitmap:=TBitmap(Source);
    if SrcBitmap.FImage=FImage then exit;

    //DebugLn('TBitMap.Assign A  RefCount=',FImage.RefCount);
    // image is not shared => new image data
    // -> free canvas (interface handles)
    FreeCanvasContext;
    // release old FImage
    FImage.Release;
    // share FImage with assign graphic
    FImage:=SrcBitmap.FImage;
    FImage.Reference;
    //DebugLn('TBitMap.Assign B ',Width,',',Height,' ',HandleAllocated,' RefCount=',FImage.RefCount);
    Changed(Self);
  {$IFNDEF DisableFPImage}
  end else if Source is TFPCustomImage then begin
    Changing(Self);
    SrcFPImage:=TFPCustomImage(Source);
    IntfImage:=TLazIntfImage.Create(0,0);
    try
      if HandleAllocated then
        IntfImage.GetDescriptionFromBitmap(Handle)
      else
        IntfImage.GetDescriptionFromDevice(0);
      IntfImage.Assign(SrcFPImage);
      IntfImage.CreateBitmap(ImgHandle,ImgMaskHandle,false);
      Handle:=ImgHandle;
      MaskHandle:=ImgMaskHandle;
    finally
      IntfImage.Free;
    end;
    Changed(Self);
  {$ENDIF}
  end else
    inherited Assign(Source);
end;

procedure TBitmap.Draw(DestCanvas: TCanvas; const DestRect: TRect);
var
  UseMaskHandle: HBitmap;
  SrcDC: LongWord;
  DestDC: LongWord;
begin
  if (DestRect.Right<=DestRect.Left) or (DestRect.Bottom<=DestRect.Top)
  or (Width=0) or (Height=0) then exit;
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
            SrcDC,0,0,Width,Height, UseMaskHandle,0,0,SRCCOPY);
    DestCanvas.Changed;
  end;
end;

constructor TBitmap.Create;
begin
  inherited Create;
  FPixelFormat := pfDevice;
  FImage := TBitmapImage.Create;
  FImage.Reference;
  FTransparentColor := clNone;
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
  begin
    HandleNeeded;
    CreateCanvas;
  end;
  Result := FCanvas;
end;

procedure TBitmap.CreateCanvas;
begin
  if (FCanvas <> nil) or (bmisCreatingCanvas in FInternalState) then exit;
  Include(FInternalState,bmisCreatingCanvas);
  try
    FCanvas := TBitmapCanvas.Create(Self);
    FCanvas.OnChange := @Changed;
    FCanvas.OnChanging := @Changing;
  finally
    Exclude(FInternalState,bmisCreatingCanvas);
  end;
end;

procedure TBitMap.FreeImage;
begin
  UnshareImage(false);
  Handle := 0;
end;

function TBitmap.HandleAllocated: boolean;
begin
  Result:=(FImage<>nil) and (FImage.FHandle<>0);
end;

function TBitmap.MaskHandleAllocated: boolean;
begin
  Result:=(FImage<>nil) and (FImage.FMaskHandle<>0);
end;

function TBitmap.PaletteAllocated: boolean;
begin
  Result:=FPalette<>0;
end;

procedure TBitmap.CreateFromBitmapHandles(SrcBitmap, SrcMaskBitmap: HBitmap;
  const SrcRect: TRect);
var
  NewRawImage: TRawImage;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  //DebugLn('TBitmap.CreateFromBitmapHandles A SrcRect=',dbgs(SrcRect));
  if not GetRawImageFromBitmap(SrcBitmap,SrcMaskBitmap,SrcRect,NewRawImage) then
    raise EInvalidGraphicOperation.Create('TBitmap.CreateFromBitmapHandles Get RawImage');
  ImgHandle:=0;
  ImgMaskHandle:=0;
  try
    //DebugLn('TBitmap.CreateFromBitmapHandles B SrRect=',dbgs(SrcRect));
    if not CreateBitmapFromRawImage(NewRawImage,ImgHandle,ImgMaskHandle,false) then
      raise EInvalidGraphicOperation.Create('TBitmap.CreateFromBitmapHandles Create bitmaps');
    Handle:=ImgHandle;
    ImgHandle:=0;
    MaskHandle:=ImgMaskHandle;
    ImgMaskHandle:=0;
  finally
    FreeRawImageData(@NewRawImage);
    if ImgHandle<>0 then DeleteObject(ImgHandle);
    if ImgMaskHandle<>0 then DeleteObject(ImgMaskHandle);
  end;
end;

procedure TBitmap.LoadFromDevice(DC: HDC);
{$IFDEF DisableFPImage}
begin
  debugln('TBitmap.LoadFromDevice needs FPImage');
end;
{$ELSE}
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
    IntfImg.CreateBitmap(ImgHandle,ImgMaskHandle,false);
    // feed HBitmap into a TBitmap
    Handle:=ImgHandle;
    ImgHandle:=0;
    MaskHandle:=ImgMaskHandle;
    ImgMaskHandle:=0;
  finally
    IntfImg.Free;
    if ImgHandle<>0 then DeleteObject(ImgHandle);
    if ImgMaskHandle<>0 then DeleteObject(ImgMaskHandle);
  end;
end;
{$ENDIF}

function TBitmap.LazarusResourceTypeValid(const ResourceType: string): boolean;
begin
  Result:=((ClassType=TBitmap) or (ClassType=TPixmap) or (ClassType=TIcon))
     and ((AnsiCompareText(ResourceType,'XPM')=0)
       or (AnsiCompareText(ResourceType,'BMP')=0));
end;

procedure TBitMap.Mask(ATransparentColor: TColor);
begin
  DebugLn('TBitMap.Mask not implemented');
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

procedure TBitMap.HandleNeeded;
var
  n : integer;
  UseWidth,
  UseHeight : Longint;
  OldChangeEvent: TNotifyEvent;
begin
  if (FImage.FHandle <> 0) then exit;

  // if the bitmap was loaded, create a handle from stream
  if (FImage.FDIBHandle = 0) and (FImage.FSaveStream <> nil) then begin
    FImage.FSaveStream.Position := 0;
    OldChangeEvent := OnChange;
    try
      OnChange := nil;
      LoadFromStream(FImage.FSaveStream); // Current FImage may be destroyed here
    finally
      OnChange := OldChangeEvent;
    end;
  end;

  // otherwise create a default handle
  if (FImage.FHandle = 0) then begin
    case PixelFormat of
      pfDevice : n:= ScreenInfo.ColorDepth;
      pf1bit : n:= 1;
      pf4bit : n:= 4;
      pf8bit : n:= 8;
      pf15bit : n:= 15;
      pf16bit : n:= 16;
      pf24bit : n:= 24;
      pf32bit : n:= 32;
      else raise EInvalidOperation.Create(rsUnsupportedBitmapFormat);
    end;
    UseWidth := Width;
    UseHeight := Height;
    if UseWidth<1 then UseWidth:=1;
    if UseHeight<1 then UseHeight:=1;
    FImage.FHandle:= CreateBitmap(UseWidth, UseHeight, 1, n, nil);
    //DebugLn('TBitMap.HandleNeeded Self=',HexStr(Cardinal(Self),8),' FImage.FHandle=',HexStr(Cardinal(FImage.FHandle),8),' n=',n);
    FImage.FDIB.dsbm.bmWidth := Width;
    FImage.FDIB.dsbm.bmHeight := Height;
  end;
end;

procedure TBitMap.MaskHandleNeeded;
begin
  //TODO
end;

procedure TBitmap.LoadFromLazarusResource(const ResName: String);
var
  ms:TMemoryStream;
  res:TLResource;
begin
  res:=LazarusResources.Find(ResName);
  if (res=nil) or (res.Value='') or not LazarusResourceTypeValid(res.ValueType)
  then exit;
  ms:=TMemoryStream.Create;
  try
    ms.Write(res.Value[1],length(res.Value));
    ms.Position:=0;
    LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

procedure TBitMap.LoadFromStream(Stream: TStream);
begin
  ReadStream(Stream, true, Stream.Size - Stream.Position);
end;

procedure TBitMap.LoadFromResourceName(Instance: THandle; const ResName: String);
begin
  DebugLn('ToDo: TBitMap.LoadFromResourceName');
end;

procedure TBitMap.LoadFromResourceID(Instance: THandle; ResID: Integer);
begin
  DebugLn('ToDo: TBitMap.LoadFromResourceID');
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

function TBitmap.GetFileExtensions: string;
begin
  Result:='bmp;xpm';
end;

Procedure TBitmap.LoadFromXPMFile(const Filename: String);
Begin
  LoadFromFile(Filename);
end;

function TBitmap.GetMonochrome: Boolean;
begin
  with FImage.FDIB.dsbm do
    Result := (bmPlanes = 1) and (bmBitsPixel = 1);
end;

procedure TBitMap.PaletteNeeded;
begin
  // ToDo
end;

procedure TBitmap.UnshareImage(CopyContent: boolean);
var
  NewImage: TBitmapImage;
  OldImage: TBitmapImage;
  {$IFNDEF DisableFPImage}
  IntfImage: TLazIntfImage;
  {$ENDIF}
begin
  if (FImage.RefCount>1) then begin
    //DebugLn('TBitmap.UnshareImage ',ClassName,' ',Width,',',Height,' ',HexStr(Cardinal(Self),8));
    // release old FImage and create a new one
    NewImage:=TBitmapImage.Create;
    try
      NewImage.Reference;
      if CopyContent and FImage.HandleAllocated
      and (Width>0) and (Height>0) then begin
        // copy content
        {$IFNDEF DisableFPImage}
        IntfImage:=TLazIntfImage.Create(0,0);
        try
          IntfImage.LoadFromBitmap(FImage.FHandle,FImage.FMaskHandle);
          IntfImage.CreateBitmap(NewImage.FHandle,NewImage.FMaskHandle,false);
          FillChar(NewImage.FDIB, SizeOf(NewImage.FDIB), 0);
          if NewImage.HandleAllocated then
            GetObject(NewImage.FHandle, SizeOf(NewImage.FDIB), @NewImage.FDIB);
        finally
          IntfImage.Free;
        end;
        {$ENDIF}
      end;
      FreeCanvasContext;
      OldImage:=FImage;
      FImage:=NewImage;
      //DebugLn('TBitMap.UnshareImage Self=',HexStr(Cardinal(Self),8),' FImage.FHandle=',HexStr(Cardinal(FImage.FHandle),8));
      NewImage:=nil; // transaction sucessful
      OldImage.Release;
    finally
      // in case something goes wrong, keep old and free new
      NewImage.Free;
    end;
    //DebugLn('TBitmap.UnshareImage END ',ClassName,' ',Width,',',Height,' ',HexStr(Cardinal(Self),8));
  end;
end;

procedure TBitmap.FreeSaveStream;
begin
  if FImage.FSaveStream<>nil then begin
    //DebugLn('TBitmap.FreeSaveStream A ',ClassName,' ',FImage.FSaveStream.Size);
  end;
  FreeAndNil(FImage.FSaveStream);
  FImage.SaveStreamType:=bnNone;
end;

procedure TBitmap.ReadStream(Stream: TStream; UseSize: boolean; Size: Longint);

  procedure RaiseInvalidBitmapHeader;
  begin
    debugln('TBitmap.ReadStream.RaiseInvalidBitmapHeader');
    raise EInOutError.Create(
      'TBitmap.ReadStream: Invalid bitmap format (bmp,xpm,ico)');
  end;
  
  procedure RaiseInvalidSize;
  begin
    debugln('TBitmap.ReadStream.RaiseInvalidSize');
    raise EInOutError.Create(
      'TBitmap.ReadStream: Invalid size of bitmap stream (bmp,xpm,ico)');
  end;

{$IFNDEF DisableFPImage}
var
  CacheStream: TStream;
  StreamType: TBitmapNativeType;
  ReaderClass: TFPCustomImageReaderClass;
  MemStream: TCustomMemoryStream;
begin
  //debugln('TBitmap.ReadStream Stream=',DbgSName(Stream),' Stream.Size=',dbgs(Stream.Size),' Stream.Position=',dbgs(Stream.Position),' UseSize=',dbgs(UseSize),' Size=',dbgs(Size));
  CacheStream:=nil;
  try
    // create mem stream if not already done (to read the image type)
    if (Stream is TCustomMemoryStream) then begin
      CacheStream:=Stream;
    end else if UseSize then begin
      CacheStream:=TMemoryStream.Create;
      CacheStream.CopyFrom(Stream,Size);
      CacheStream.Position:=0;
    end else begin
      // size is unknown and type is not TMemoryStream
      // ToDo: create cache stream from Stream
      CacheStream:=Stream;
    end;
    // get image type
    if CacheStream is TCustomMemoryStream then begin
      MemStream:=TCustomMemoryStream(CacheStream);
      if UseSize and (Size>MemStream.Size-MemStream.Position) then
        RaiseInvalidSize;
      StreamType:=TestStreamBitmapNativeType(MemStream);
    end else
      StreamType:=bnWinBitmap;
    //debugln('TBitmap.ReadStream ',dbgs(ord(StreamType)),' UseSize=',dbgs(UseSize),' Size=',dbgs(Size),' Stream=',DbgSName(Stream));
    ReaderClass:=nil;
    case StreamType of
    bnWinBitmap:  ReaderClass:=TLazReaderBMP;
    bnXPixmap:    ReaderClass:=TLazReaderXPM;
    bnIcon:       ReaderClass:=TLazReaderIcon;
    else
      RaiseInvalidBitmapHeader;
    end;
    ReadStreamWithFPImage(CacheStream,UseSize,Size,ReaderClass);
  finally
    if CacheStream<>Stream then
      CacheStream.Free;
  end;
end;
{$ELSE if DisableFPImage}
var
  MemStream: TMemoryStream;

  procedure CreateEmptyBitmap;
  var
    DIB: TDIBSection;
  begin
    FillChar(DIB, sizeof(DIB), 0);
    //NewImage(0, 0, DIB, False);
  end;

  procedure ReadBMPStream;
  type
    TBitsObj = array[1..1] of byte;
    PBitsObj = ^TBitsObj;
  const
    BI_RGB = 0;
  var
    BmpHead: TBitmapFileHeader;
    ReadSize: integer;
    BmpInfo: PBitmapInfo;
    ImgSize: longint;
    Bits: PBitsObj;
    InfoSize: integer;
    BitsPerPixel, ColorsUsed: integer;
  begin
    FillChar(BmpHead,SizeOf(BmpHead),0);
    ReadSize:=MemStream.Read(BmpHead, SizeOf(BmpHead));
    if (ReadSize<>SizeOf(BmpHead))
    or (BmpHead.bfType <> Word($4D42))
    or (BmpHead.bfOffBits<DWORD(ReadSize))
    then
      RaiseInvalidBitmapHeader;

    InfoSize:=BmpHead.bfOffBits-SizeOf(BmpHead);
    GetMem(BmpInfo,InfoSize);
    try
      ReadSize:=MemStream.Read(BmpInfo^,InfoSize);
      if ReadSize<>InfoSize then
        raise EInOutError.Create(
          'TBitmap.ReadBMPStream: Invalid windows bitmap (info)');
      if BmpInfo^.bmiHeader.biSize<>SizeOf(BitmapInfoHeader) then
        raise EInOutError.Create(
          'TBitmap.ReadBMPStream: OS2 bitmaps are not supported yet');
      if BmpInfo^.bmiHeader.biCompression<>bi_RGB then
        raise EInOutError.Create(
          'TBitmap.ReadBMPStream: RLE compression is not supported yet');

      // Let's now support only 16/24bit bmps! Then we don't need a palette.
      BitsPerPixel:=BmpInfo^.bmiHeader.biBitCount;
      if BitsPerPixel<16 then begin
        ColorsUsed:=BmpInfo^.bmiHeader.biClrUsed;
        if ColorsUsed=0 then ColorsUsed:=1 shl ColorsUsed;
        // s:=SizeOf(TLogPalette)+(ColorsUsed-1)*SizeOf(TPaletteEntry);
      end;
      // Palette is fake now. Then it'll be better!
      // EInOutError.Create('Only truecolor is supported yet.');

      ImgSize:=BmpInfo^.bmiHeader.biSizeImage;
      GetMem(Bits,ImgSize);
      try
        ReadSize:=MemStream.Read(Bits^,ImgSize);
        if ReadSize<>ImgSize then
          raise EInOutError.Create(
            'TBitmap.ReadBMPStream: Invalid windows bitmap (bits)');

        Handle := CreateBitmap(BmpInfo^.bmiHeader.biWidth,
          BmpInfo^.bmiHeader.biHeight, BmpInfo^.bmiHeader.biPlanes,
          BitsPerPixel, Bits);

      finally
        FreeMem(Bits);
      end;
    finally
      FreeMem(BmpInfo);
    end;
  end;
  
  procedure ReadXPMStream;
  var
    XPM: PPChar;
    NewWidth, NewHeight, NewColorCount: integer;
  begin
    XPM:=ReadXPMFromStream(MemStream,Size);
    try
      if not ReadXPMSize(XPM,NewWidth,NewHeight,NewColorCount) then
        raise EInOutError.Create('TBitmap.ReadXPMStream: ERROR: reading xpm');

      // free old pixmap
      // Create the pixmap
      if (FTransparentColor = clNone) or (FTransparentColor = clDefault) then
        // create a transparent pixmap (with mask)
        Handle := CreatePixmapIndirect(XPM, -1)
      else
        // create an opaque pixmap.
        // Transparent pixels are filled with FTransparentColor
        Handle := CreatePixmapIndirect(XPM, ColorToRGB(FTransparentColor));
    finally
      if XPM<>nil then
        FreeMem(XPM);
    end;

    if HandleAllocated then begin
      FWidth:=NewWidth;
      FHeight:=NewHeight;
    end else begin
      FWidth:=0;
      FHeight:=0;
    end;
  end;
  
begin
  UnshareImage(false);

  if Size = 0 then begin
    CreateEmptyBitmap;
    exit;
  end;
  
  // store original stream
  StoreOriginalStream(Stream,Size);
  // hide SaveStream (so, that it won't be destroyed due to the change)
  MemStream:=FImage.SaveStream;
  try
    FImage.SaveStream:=nil;

    // determine stream type
    FImage.SaveStreamType:=TestStreamBitmapNativeType(MemStream);
    // read stream
    case FImage.SaveStreamType of
    bnWinBitmap:  ReadBMPStream;
    bnXPixmap:    ReadXPMStream;
    else
      RaiseInvalidBitmapHeader;
    end;
  finally
    FImage.SaveStream:=MemStream;
  end;
end;
{$ENDIF  if DisableFPImage}

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
{$IFDEF DisableFPImage}
Type
  TBITMAPHEADER = packed record
    FileHeader : tagBitmapFileHeader;
    InfoHeader : tagBitmapInfoHeader;
  end;

var
  MemStream: TMemoryStream;


  Procedure DoWriteStreamSize(DestStream: TStream; Size: longint);
  begin
    if WriteSize then
      DestStream.WriteBuffer(Size, SizeOf(Size));
  end;

  procedure DoWriteOriginal;
  begin
    DoWriteStreamSize(Stream,longint(FImage.SaveStream.Size));
    FImage.SaveStream.Position:=0;
    Stream.CopyFrom(FImage.SaveStream,longint(FImage.SaveStream.Size));
  end;

  Procedure DoWriteSize(Header: TBitmapHeader);
  begin
    DoWriteStreamSize(MemStream,Header.FileHeader.bfSize);
  end;

  Procedure FillBitmapInfo(Bitmap : hBitmap; var Bits : Pointer;
    Var Header : TBitmapHeader);
  var
    ScreenDC, DC : hDC;
    DIB : TDIBSection;
    BitmapHeader : TagBITMAPINFO;
  begin
    FillChar(DIB, SizeOf(DIB), 0);
    GetObject(Bitmap, SizeOf(DIB), @DIB);
    with DIB.dsbm, DIB.dsbmih do
    begin
      biSize := sizeof(DIB.dsbmih);
      biWidth := bmWidth;
      biHeight := bmHeight;
      biPlanes := 1;
      biBitCount := bmPlanes * bmBitsPixel;
      if biSizeImage = 0 then  begin
        biSizeImage := ((bmWidth * biBitCount) + 31) and not 31;
        biSizeImage := biSizeImage div 8;
        biSizeImage := Abs(biSizeImage) * Abs(bmHeight);
      end;
    end;
    Bits := AllocMem(Longint(Dib.dsBmih.biSizeImage)*SizeOf(Byte));
    BitmapHeader.bmiHeader := DIB.dsbmih;
    ScreenDC := GetDC(0);
    DC := CreateCompatibleDC(ScreenDC);
    GetDIBits(DC, Bitmap, 0, Abs(Dib.dsBmih.biHeight), Bits, BitmapHeader, DIB_RGB_COLORS);
    ReleaseDC(0, ScreenDC);
    DeleteDC(DC);
    With Header, Header.FileHeader, Header.InfoHeader do begin
      InfoHeader := BitmapHeader.bmiHeader;
      FillChar(FileHeader, sizeof(FileHeader), 0);
      bfType      := $4D42;
      bfSize      := SizeOf(Header) + biSizeImage;
      bfOffBits   := SizeOf(Header);
    end;
  end;

  Procedure WriteBitmapHeader(Header : TBitmapHeader);
  begin
    MemStream.WriteBuffer(Header, SizeOf(Header));
  end;

  Procedure WriteTRIColorMap(Color : PLongint; size : Longint); //For OS/2 Bitmaps
  var
    I : Longint;
    TRI : RGBTRIPLE;
  begin
    size := size div 3;
    for i := 0 to size - 1 do
    begin
      Tri.rgbtBlue := Blue(Color[i]);
      Tri.rgbtGreen := Green(Color[i]);
      Tri.rgbtRed := Red(Color[i]);
      MemStream.WriteBuffer(Tri, 3);
    end;
  end;

  Procedure WriteQUADColorMap(Color : PLongint; size : Longint); //For MS Bitmaps
  var
    I : Longint;
    Quad : RGBQUAD;
  begin
    size := size div 4;
    for i := 0 to size - 1 do
    begin
      FillChar(QUAD, SizeOf(RGBQUAD),0);
      Quad.rgbBlue := Blue(Color[i]);
      Quad.rgbGreen := Green(Color[i]);
      Quad.rgbRed := Red(Color[i]);
      MemStream.WriteBuffer(Quad, 4);
    end;
  end;

  Procedure WriteColorMap(Header : TBitmapHeader);
  begin
    ///Figure out how to get colors then call Quad/Tri
  end;

  Procedure WritePixels(Bits : PByte; Header : TBitmapHeader);
  begin
    MemStream.WriteBuffer(Bits^, Header.InfoHeader.biSizeImage);
  end;

var
  Bits: PByte;
  Header: TBitmapHeader;
  StreamSize: TStreamSeekType;
{$ENDIF}
begin
  {$IFNDEF DisableFPImage}
  WriteStreamWithFPImage(Stream,WriteSize,TFPWriterBMP);
  if (FImage.SaveStream<>nil) and (FImage.SaveStreamType=bnNone) then
    FImage.SaveStreamType:=bnWinBitmap;
  {$ELSE}
  if (FImage.SaveStream<>nil) and (FImage.SaveStream.Size>0)
  and (FImage.SaveStreamType<>bnNone) then begin
    DoWriteOriginal;
    exit;
  end;
  DebugLn('TBitmap.WriteStream A Warning: creating BMP does not always work ',
    dbgs(FImage.SaveStream<>nil),' ',dbgs(ord(FImage.SaveStreamType)),'. Please use FPImage.');

  // write image in BMP format to temporary stream
  MemStream:=TMemoryStream.Create;
  try
    Bits:=nil;
    FillBitmapInfo(Handle, Bits, Header);
    WriteBitmapHeader(Header);
    WriteColorMap(Header);
    WritePixels(Bits, Header);
    // save stream, so that further saves will be fast
    MemStream.Position:=0;
    FreeSaveStream;
    FImage.SaveStream:=MemStream;
    MemStream:=nil;
    FImage.SaveStreamType:=bnWinBitmap;
    // copy savestream to destination stream
    if WriteSize then begin
      StreamSize:=FImage.SaveStream.Size;
      Stream.WriteBuffer(StreamSize, SizeOf(StreamSize));
    end;
    Stream.CopyFrom(FImage.SaveStream,FImage.SaveStream.Size);
  finally
    ReallocMem(Bits, 0);
    MemStream.Free;
  end;
  {$ENDIF}
end;

procedure TBitmap.StoreOriginalStream(Stream: TStream; Size: integer);
var
  MemStream: TMemoryStream;
begin
  if Stream<>FImage.SaveStream then begin
    MemStream:=TMemoryStream.Create;
    //debugln('TBitmap.StoreOriginalStream Size=',dbgs(Size),' Stream.Position=',dbgs(Stream.Position),' Stream.Size=',dbgs(Stream.Size));
    MemStream.CopyFrom(Stream,Size);
    FreeSaveStream;
    FImage.FSaveStream:=MemStream;
  end;
  FImage.SaveStreamType:=bnNone;
  FImage.SaveStream.Position:=0;
end;

{$IFNDEF DisableFPImage}
{------------------------------------------------------------------------------
  procedure TBitmap.ReadStreamWithFPImage(Stream: TStream; UseSize: boolean;
    Size: Longint; ReaderClass: TFPCustomImageReaderClass);
    
  Clear old bitmap and read new bitmap form stream.
  Stream: source stream. After reading Position will be at end of bitmap.
  UseSize: if True, Size is used. If False then Size is calculated
           automatically.
  Size: Only used when UseSize=True. This amount of bytes are read.
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
    Width:=0;
    Height:=0;
    exit;
  end;

  IntfImg:=nil;
  ImgReader:=nil;
  NewSaveStream:=nil;
  if UseSize then begin
    StoreOriginal(Stream,Size);
    SrcStream:=NewSaveStream;
  end else begin
    FreeSaveStream;
    SrcStream:=Stream;
  end;
  try
    // read image
    IntfImg:=TLazIntfImage.Create(0,0);
    IntfImg.GetDescriptionFromDevice(0);
    ImgReader:=ReaderClass.Create;
    InitFPImageReader(ImgReader);
    OldStreamPosition:=SrcStream.Position;
    IntfImg.LoadFromStream(SrcStream,ImgReader);
    ImgSize:=SrcStream.Position-OldStreamPosition;
    if not UseSize then begin
      // now the size is known -> store stream
      //DebugLn('TBitmap.ReadStreamWithFPImage SrcStream=',SrcStream.ClassName,' ImgSize=',ImgSize);
      SrcStream.Position:=OldStreamPosition;
      StoreOriginal(SrcStream,integer(ImgSize));
    end else begin
      // set position
      if Size<>ImgSize then
        SrcStream.Position:=OldStreamPosition+Size;
    end;
    FinalizeFPImageReader(ImgReader);
    IntfImg.CreateBitmap(ImgHandle,ImgMaskHandle,false);
    Handle:=ImgHandle;
    MaskHandle:=ImgMaskHandle;
  finally
    // set save stream
    FImage.SaveStream:=NewSaveStream;
    // clean up
    IntfImg.Free;
    ImgReader.Free;
  end;
end;

procedure TBitmap.WriteStreamWithFPImage(Stream: TStream; WriteSize: boolean;
  WriterClass: TFPCustomImageWriterClass);

  Procedure DoWriteStreamSize(DestStream: TStream; Size: longint);
  begin
    //DebugLn('DoWriteStreamSize ',ClassName,' Size=',Size,' WriteSize=',WriteSize);
    if WriteSize then
      DestStream.WriteBuffer(Size, SizeOf(Size));
  end;

  procedure DoWriteOriginal;
  begin
    DoWriteStreamSize(Stream,longint(FImage.SaveStream.Size));
    FImage.SaveStream.Position:=0;
    Stream.CopyFrom(FImage.SaveStream,FImage.SaveStream.Size);
  end;

var
  MemStream: TMemoryStream;
  IntfImg: TLazIntfImage;
  ImgWriter: TFPCustomImageWriter;
begin
  //DebugLn('WriteStreamWithFPImage Self=',HexStr(Cardinal(Self),8),' ',Width,',',Height,' Using SaveStream=',(FImage.SaveStream<>nil) and (FImage.SaveStream.Size>0));
  if (FImage.SaveStream<>nil) and (FImage.SaveStream.Size>0) then begin
    DoWriteOriginal;
    exit;
  end;
  //DebugLn('WriteStreamWithFPImage');

  // write image to temporary stream
  MemStream:=TMemoryStream.Create;
  IntfImg:=nil;
  ImgWriter:=nil;
  try
    IntfImg:=TLazIntfImage.Create(0,0);
    IntfImg.LoadFromBitmap(Handle,0);
    ImgWriter:=WriterClass.Create;
    InitFPImageWriter(ImgWriter);
    IntfImg.SaveToStream(MemStream,ImgWriter);
    FinalizeFPImageWriter(ImgWriter);
    FreeAndNil(ImgWriter);
    FreeAndNil(IntfImg);
    // save stream, so that further saves will be fast
    MemStream.Position:=0;
    FreeSaveStream;
    FImage.SaveStream:=MemStream;
    FImage.SaveStreamType:=bnNone;
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

procedure TBitmap.InitFPImageReader(ImgReader: TFPCustomImageReader);
begin

end;

procedure TBitmap.InitFPImageWriter(ImgWriter: TFPCustomImageWriter);
begin

end;

procedure TBitmap.FinalizeFPImageReader(ImgReader: TFPCustomImageReader);
begin

end;

procedure TBitmap.FinalizeFPImageWriter(ImgWriter: TFPCustomImageWriter);
begin

end;
{$ENDIF}

procedure TBitMap.SaveToStream(Stream: TStream);
begin
  WriteStream(Stream, False);
end;

procedure TBitmap.SetHandle(Value: HBITMAP);
begin
  if FImage.FHandle = Value then exit;
  // free old handles
  FreeCanvasContext;
  UnshareImage(false);
  FImage.FreeHandle;
  // get the properties from new bitmap
  FImage.FHandle:=Value;
  //DebugLn('TBitMap.SetHandle Self=',HexStr(Cardinal(Self),8),' FImage.FHandle=',HexStr(Cardinal(FImage.FHandle),8));
  FillChar(FImage.FDIB, SizeOf(FImage.FDIB), 0);
  if FImage.FHandle <> 0 then
    GetObject(FImage.FHandle, SizeOf(FImage.FDIB), @FImage.FDIB);
  Changed(Self);
end;

procedure TBitmap.SetMaskHandle(NewMaskHandle: HBITMAP);
// WARNING: Contrary to Delphi, this can change Handle as well. It depends on
// the widgetset.
begin
  if FImage.FMaskHandle = NewMaskHandle then exit;
  // unshare image and free canvas handle
  UnshareImage(true);
  FreeCanvasContext;
  // create handle
  HandleNeeded;
  // free old mask handle
  FImage.FreeMaskHandle;
  // combine (depending on the interface we will end with one or two handles)
  {$IFDEF VerboseImgMasks}
  DebugLn('TBitmap.SetMaskHandle Before Replace FImage.FHandle=',HexStr(Cardinal(FImage.FHandle),8),
    ' FImage.FMaskHandle=',HexStr(Cardinal(FImage.FMaskHandle),8),
    ' NewMaskHandle=',HexStr(Cardinal(NewMaskHandle),8));
  {$ENDIF}
  ReplaceBitmapMask(FImage.FHandle,FImage.FMaskHandle,NewMaskHandle);
  FTransparent := FImage.FMaskHandle <> 0;
  {$IFDEF VerboseImgMasks}
  DebugLn('TBitmap.SetMaskHandle After Replace FImage.FHandle=',HexStr(Cardinal(FImage.FHandle),8),
    ' FImage.FMaskHandle=',HexStr(Cardinal(FImage.FMaskHandle),8),
    ' NewMaskHandle=',HexStr(Cardinal(NewMaskHandle),8));
  {$ENDIF}
  Changed(Self);
end;

// release handles without freeing them
// useful for creating a HBitmap
Function TBitmap.ReleaseHandle: HBITMAP;
Begin
  //HandleNeeded; Delphi creates a handle. Why?
  FreeCanvasContext;
  Result := FImage.ReleaseHandle;
end;

function TBitmap.ReleasePalette: HPALETTE;
begin
  // ToDo
  Result := 0;
end;

{$IFNDEF DisableFPImage}
function TBitmap.GetFPReaderForFileExt(const FileExtension: string
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

function TBitmap.GetFPWriterForFileExt(const FileExtension: string
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

function TBitmap.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  if (AnsiCompareText(ClassName,'TBitmap')=0) then
    Result:=TLazReaderBMP
  else
    Result:=nil;
end;

function TBitmap.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  if (AnsiCompareText(ClassName,'TBitmap')=0) then
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
  if (FImage.SaveStream<>nil) and (FImage.SaveStreamType=bnNone) then
    FImage.SaveStreamType:=SaveStreamType;
end;

function TBitmap.CreateIntfImage: TLazIntfImage;
begin
  Result:=TLazIntfImage.Create(0,0);
  Result.LoadFromBitmap(Handle,MaskHandle);
end;
{$ENDIF}

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
  Result:=inherited GetPalette;
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
  if Value=TransparentMode then exit;
  DebugLn('Note: TBitmap.SetTransparentMode not implemented');
end;

// included by graphics.pp


{ =============================================================================

  $Log$
  Revision 1.94  2005/01/18 19:46:41  mattias
  added stream size check for TBitmap

  Revision 1.93  2005/01/14 15:43:48  mattias
  added Changed and Changing to TBitmpa.Assign

  Revision 1.92  2004/12/27 17:42:01  micha
  fix transparency while drawing (bug 462)

  Revision 1.91  2004/09/27 10:01:18  mattias
  added TLazIntfImage.TColors property

  Revision 1.90  2004/09/24 10:23:49  mattias
  fixed 1.0.10 compilation

  Revision 1.89  2004/09/11 10:19:07  mattias
  implemented TBitmap.LoadFromDevice

  Revision 1.88  2004/05/14 14:32:49  mattias
  fix for fpc 1.0.10

  Revision 1.87  2004/05/11 11:42:26  mattias
  replaced writeln by debugln

  Revision 1.86  2004/04/23 20:41:11  mattias
  added SetFocus after closing completion form

  Revision 1.85  2004/04/10 17:58:56  mattias
  implemented mainunit hints for include files

  Revision 1.84  2004/04/10 00:11:16  mattias
  added basic TIcon reading  from Colin

  Revision 1.83  2004/04/05 11:41:05  mattias
  fixed retrieving gdkbitmaps  LineEnding=rileDWordBoundary

  Revision 1.82  2004/04/03 16:47:46  mattias
  implemented converting gdkbitmap to RawImage mask

  Revision 1.81  2004/03/28 12:57:03  mattias
  fixed 1.0.10 compilation

  Revision 1.80  2004/03/28 12:49:22  mattias
  implemented mask merge and extraction for raw images

  Revision 1.79  2004/03/22 19:10:04  mattias
  implemented icons for TPage in gtk, mask for TCustomImageList

  Revision 1.78  2004/03/06 15:37:43  mattias
  fixed FreeDC

  Revision 1.77  2004/03/03 23:35:55  mattias
  accelerated TActionList editor  from Radek

  Revision 1.76  2004/02/29 22:51:54  mattias
  added jpeg example

  Revision 1.75  2004/02/29 10:34:00  mattias
  fixed bmp reader reader skipping header

  Revision 1.74  2004/02/28 00:34:35  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.73  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.72  2004/02/24 19:40:17  mattias
  TBitmap can now read form streams without knowing the size

  Revision 1.71  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.69  2004/02/21 01:01:03  mattias
  added uninstall popupmenuitem to package graph explorer

  Revision 1.68  2004/02/19 05:07:16  mattias
  CreateBitmapFromRawImage now creates mask only if needed

  Revision 1.67  2004/02/19 03:20:58  mattias
  fixed scaling TBitmap.Draw

  Revision 1.66  2004/02/17 22:17:40  mattias
  accelerated conversion from data to lrs

  Revision 1.65  2004/02/17 00:32:25  mattias
  fixed TCustomImage.DoAutoSize fixing uninitialized vars

  Revision 1.64  2004/02/11 11:40:18  mattias
  fixes for compilation under fpc 1.0.10

  Revision 1.63  2004/02/11 11:34:15  mattias
  started new TToolBar

  Revision 1.62  2004/02/10 00:05:03  mattias
  TSpeedButton now uses MaskBlt

  Revision 1.61  2004/02/08 11:31:32  mattias
  TMenuItem.Bitmap is now auto created on read. Added TMenuItem.HasBitmap

  Revision 1.60  2004/02/07 20:25:37  mattias
  fixed saving custom TBitBtn kind

  Revision 1.59  2004/02/05 16:28:38  mattias
  fixed unsharing TBitmap

  Revision 1.58  2004/02/04 22:17:09  mattias
  removed workaround VirtualCreate

  Revision 1.57  2004/02/02 22:01:51  mattias
  fpImage is now used as default, deactivate it with -dDisableFPImage

  Revision 1.56  2004/01/26 11:55:35  mattias
  fixed resizing synedit

  Revision 1.55  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.54  2003/12/21 16:01:58  mattias
  workaround for inherited bug in fpc 1.9

  Revision 1.53  2003/11/26 22:05:57  mattias
  fixed TIcon

  Revision 1.52  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.51  2003/11/25 08:59:01  mattias
  fixed a few more black colors

  Revision 1.50  2003/11/23 14:09:45  mattias
  fixed mem leak  thx to Vincent

  Revision 1.49  2003/10/22 18:43:23  mattias
  prepared image sharing

  Revision 1.48  2003/10/22 17:50:16  mattias
  updated rpm scripts

  Revision 1.47  2003/09/12 14:59:43  mattias
  added searching for fpImage reader/writer

  Revision 1.46  2003/09/10 19:15:15  mattias
  implemented copying graphics from/to clipboard

  Revision 1.45  2003/09/08 13:07:17  mattias
  TBitmap now uses fpImage for writing bitmaps

  Revision 1.44  2003/09/08 12:21:48  mattias
  added fpImage reader/writer hooks to TBitmap

  Revision 1.43  2003/09/05 22:13:40  mattias
  implemented TBitmap.Assign(TFPCustomImage)

  Revision 1.42  2003/09/02 21:32:56  mattias
  implemented TOpenPictureDialog

  Revision 1.41  2003/09/02 15:12:21  mattias
  TBitmap.Assign now shares image data

  Revision 1.40  2003/08/20 17:03:48  mattias
  implemented TPixmap and TPortableNetworkGraphic with fpImage

  Revision 1.39  2003/08/16 15:29:56  mattias
  fixed TBitmap.GetHandle

  Revision 1.38  2003/08/10 09:33:43  mattias
  saved bitmap stream is now stored

  Revision 1.37  2003/07/01 09:29:51  mattias
  attaching menuitems topdown

  Revision 1.36  2003/06/30 17:25:26  mattias
  fixed parsing of with do try finally end

  Revision 1.35  2003/06/30 16:31:04  mattias
  fixed find declaration of with A,B do C; statements

  Revision 1.34  2003/06/30 15:13:21  mattias
  fixed releasing bitmap handle

  Revision 1.33  2003/06/30 14:58:29  mattias
  implemented multi file add to package editor

  Revision 1.32  2003/06/30 10:09:46  mattias
  fixed Get/SetPixel for DC without widget

  Revision 1.31  2003/06/25 10:38:28  mattias
  implemented saving original stream of TBitmap

  Revision 1.30  2003/06/07 17:14:11  mattias
  small changes for fpc 1.1

  Revision 1.29  2003/04/03 17:42:13  mattias
  added exception handling for createpixmapindirect

  Revision 1.28  2003/03/12 14:39:29  mattias
  fixed clipping origin in stretchblt

  Revision 1.27  2003/03/11 07:46:43  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.26  2003/02/04 14:36:19  mattias
  fixed set method in OI

  Revision 1.25  2002/12/16 12:12:50  mattias
  fixes for fpc 1.1

  Revision 1.24  2002/12/12 17:47:46  mattias
  new constants for compatibility

  Revision 1.23  2002/11/12 10:16:16  lazarus
  MG: fixed TMainMenu creation

  Revision 1.22  2002/11/09 15:02:06  lazarus
  MG: fixed LM_LVChangedItem, OnShowHint, small bugs

  Revision 1.21  2002/10/25 10:42:08  lazarus
  MG: broke minor circles

  Revision 1.20  2002/09/16 15:42:17  lazarus
  MG: fixed calling DestroyHandle if not HandleAllocated

  Revision 1.19  2002/09/13 16:58:27  lazarus
  MG: removed the 1x1 bitmap from TBitBtn

  Revision 1.18  2002/09/12 05:56:15  lazarus
  MG: gradient fill, minor issues from Andrew

  Revision 1.17  2002/09/10 06:49:19  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.16  2002/09/03 08:07:19  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.15  2002/09/02 08:13:16  lazarus
  MG: fixed GraphicClass.Create

  Revision 1.14  2002/08/15 13:37:57  lazarus
  MG: started menuitem icon, checked, radio and groupindex

  Revision 1.13  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.12  2001/12/21 18:16:59  lazarus
  Added TImage class
  Shane

  Revision 1.11  2001/10/10 17:55:04  lazarus
  MG: fixed caret lost, gtk cleanup, bracket lvls, bookmark saving

  Revision 1.10  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.9  2001/06/26 00:08:35  lazarus
  MG: added code for form icons from Rene E. Beszon

  Revision 1.8  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.7  2001/06/04 09:32:17  lazarus
  MG: fixed bugs and cleaned up messages

  Revision 1.6  2001/03/21 00:20:29  lazarus
  MG: fixed memory leaks

  Revision 1.5  2001/03/19 14:00:50  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.4  2001/03/12 09:40:44  lazarus
  MG: bugfix for readstream

  Revision 1.3  2001/03/05 14:20:04  lazarus
  added streaming to tgraphic, added tpicture

  Revision 1.2  2000/12/29 20:32:33  lazarus
  Speedbuttons can now draw disabled images.
  Shane

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.2  2000/05/09 00:46:41  lazarus
  Changed writelns to Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:55  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.18  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.17  2000/03/21 23:47:33  lazarus
  MWE:
    + Added TBitmap.MaskHandle & TGraphic.Draw & TBitmap.Draw

  Revision 1.16  2000/03/19 03:52:08  lazarus
  Added onclick events for the speedbuttons.
  Shane

  Revision 1.15  2000/03/16 23:58:46  lazarus
  MWE:
    Added TPixmap for XPM support

  Revision 1.14  2000/03/15 20:15:31  lazarus
  MOdified TBitmap but couldn't get it to work
  Shane

  Revision 1.13  2000/03/10 12:51:14  lazarus
  *** empty log message ***

  Revision 1.12  2000/03/07 19:00:15  lazarus
  Minor changes.  Added the skeleton for TSpeedbutton
  Shane

  Revision 1.11  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.10  2000/01/18 22:18:34  lazarus

  Moved bitmap creation into appropriate place. Cleaned up a bit.
  Finished DeleteObject procedure.

  Revision 1.9  1999/12/31 14:58:00  lazarus
  MWE:
    Set unkown VK_ codesto 0
    Added pfDevice support for bitmaps

  Revision 1.8  1999/12/18 18:27:31  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.7  1999/12/14 22:05:37  lazarus
  More changes for TToolbar
  Shane

  Revision 1.6  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.5  1999/11/17 01:16:39  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

}
