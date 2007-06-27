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
  if TestStreamIsBMP(AStream) then
    Result:=bnWinBitmap
  else if TestStreamIsXPM(AStream) then
    Result:=bnXPixmap
  else if TestStreamIsIcon(AStream) then
    Result := bnIcon
  else if TestStreamIsCursor(AStream) then
    Result := bnCursor
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
  //debugln('TestStreamIsBMP ',DbgStr(Signature[0]),' ',DbgStr(Signature[1]));
  AStream.Position:=OldPosition;
end;

procedure TBitMap.Assign(Source: TPersistent);
var
  SrcBitmap: TBitmap;
  SrcFPImage: TFPCustomImage;
  IntfImage: TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
begin
  if Source=Self then exit;
  if Source is TBitmap then begin
    SrcBitmap:=TBitmap(Source);
    // TBitmap can share image data
    // -> check if already shared
    if SrcBitmap.FImage=FImage then exit;// already sharing

    ChangingAll(Self);
    //DebugLn(['TBitMap.Assign Self=',ClassName,' Source=',Source.ClassName,' HandleAllocated=',HandleAllocated,' Canvas=',DbgSName(FCanvas)]);
    FTransparent := SrcBitmap.Transparent;

    //DebugLn('TBitMap.Assign A  RefCount=',FImage.RefCount);
    // image is not shared => new image data
    // -> free canvas (interface handles)
    FreeCanvasContext;
    // release old FImage
    FImage.Release;
    // share FImage with assigned graphic
    FImage:=SrcBitmap.FImage;
    FImage.Reference;
    //DebugLn(['TBitMap.Assign B ',Width,',',Height,' ',HandleAllocated,' RefCount=',FImage.RefCount]);
    Changed(Self);
  end else if Source is TFPCustomImage then begin
    ChangingAll(Self);
    SrcFPImage:=TFPCustomImage(Source);
    IntfImage:=TLazIntfImage.Create(0,0);
    try
      if HandleAllocated then
        IntfImage.GetDescriptionFromBitmap(Handle)
      else
        IntfImage.GetDescriptionFromDevice(0);
      IntfImage.Assign(SrcFPImage);
      IntfImage.CreateBitmap(ImgHandle,ImgMaskHandle,false);
      SetHandles(ImgHandle,ImgMaskHandle);
    finally
      IntfImage.Free;
    end;
    Changed(Self);
  end else if Source = nil then begin
    FreeSaveStream;
    SetWidthHeight(0,0);
  end else
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
    SetHandles(ImgHandle,ImgMaskHandle);
    ImgHandle:=0;
    ImgMaskHandle:=0;
  finally
    FreeRawImageData(@NewRawImage);
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
    IntfImg.CreateBitmap(ImgHandle,ImgMaskHandle,false);
    // feed HBitmap into a TBitmap
    SetHandles(ImgHandle,ImgMaskHandle);
    ImgHandle:=0;
    ImgMaskHandle:=0;
  finally
    IntfImg.Free;
    if ImgHandle<>0 then DeleteObject(ImgHandle);
    if ImgMaskHandle<>0 then DeleteObject(ImgMaskHandle);
  end;
end;

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

procedure TBitmap.SetTransparentColor(const AValue: TColor);
var
  IntfImage: TLazIntfImage;
  y: Integer;
  x: Integer;
  CurColor: TFPColor;
  ImgHandle, ImgMaskHandle: HBitmap;
  r: Byte;
  g: Byte;
  b: Byte;
begin
  if FTransparentColor=AValue then exit;
  FTransparentColor:=AValue;
  if (FTransparentColor and not $ffffff)=0 then begin
    IntfImage:=nil;
    try
      CreateIntfImage(IntfImage);
      r:=Red(FTransparentColor);
      g:=Green(FTransparentColor);
      b:=Blue(FTransparentColor);
      for y:=0 to IntfImage.Height-1 do begin
        for x:=0 to IntfImage.Width-1 do begin
          CurColor:=IntfImage.Colors[x,y];
          if ((CurColor.red shr 8)=r)
          and ((CurColor.green shr 8)=g)
          and ((CurColor.blue shr 8)=b) then begin
            CurColor.alpha:=alphaTransparent;
          end else begin
            CurColor.alpha:=alphaOpaque;
          end;
          IntfImage.Colors[x,y]:=CurColor;
        end;
      end;
      IntfImage.CreateBitmap(ImgHandle,ImgMaskHandle,false);
      SetHandles(ImgHandle,ImgMaskHandle);
    finally
      IntfImage.Free;
    end;
  end;
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
    //DebugLn('TBitMap.HandleNeeded Self=',DbgS(Self),' FImage.FHandle=',DbgS(FImage.FHandle),' n=',n);
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

class function TBitmap.GetFileExtensions: string;
begin
  Result:='bmp;xpm';
end;

Procedure TBitmap.LoadFromXPMFile(const Filename: String);
Begin
  LoadFromFile(Filename);
end;

procedure TBitmap.LoadFromIntfImage(IntfImage: TLazIntfImage);
var
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  IntfImage.CreateBitmap(ImgHandle,ImgMaskHandle,false);
  SetHandles(ImgHandle,ImgMaskHandle);
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
          IntfImage.CreateBitmap(NewImage.FHandle,NewImage.FMaskHandle,false);
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
    if (Stream is TCustomMemoryStream) then begin
      WorkStream:=Stream;
    end else if UseSize then begin
      WorkStream:=TMemoryStream.Create;
      TMemoryStream(WorkStream).SetSize(Size);
      WorkStream.CopyFrom(Stream,Size);
      WorkStream.Position:=0;
    end else begin
      // size is unknown and type is not TMemoryStream
      // ToDo: create cache stream from Stream
      WorkStream:=Stream;
    end;
    // get image type
    if WorkStream is TCustomMemoryStream then begin
      MemStream:=TCustomMemoryStream(WorkStream);
      OldPosition:=MemStream.Position;
      GetSize:=MemStream.Size;
      // workaround for TMemoryStream bug, reading Size sets Position to 0
      MemStream.Position:=OldPosition;
      if UseSize and (Size>GetSize-OldPosition) then
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
      bnCursor:     ReaderClass:=TLazReaderCursor;
    else
      RaiseInvalidBitmapHeader;
    end;
    ReadStreamWithFPImage(WorkStream,UseSize,Size,ReaderClass);
  finally
    if WorkStream<>Stream then
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

procedure TBitmap.SaveToFile(const Filename: string);
begin
  if (ClassType=TBitmap) and (FImage.SaveStreamType<>bnWinBitmap) then begin
    // TBitmap should always save in .bmp format
    // but the current SaveStream is not in .bmp format.
    // -> Clear the SaveStream.
    UnshareImage(true);
    FreeSaveStream;
  end;
  inherited SaveToFile(Filename);
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

procedure TBitmap.SetHandles(NewHandle, NewMaskHandle: HBITMAP);
begin
  if (FImage.FHandle = NewHandle) and (FImage.FMaskHandle = NewMaskHandle) then exit;
  // free old handles
  FreeCanvasContext;
  UnshareImage(false);
  FreeSaveStream;
  FImage.FreeHandle;
  FImage.FreeMaskHandle;
  // get the properties from new bitmap
  FImage.FHandle:=NewHandle;
  //DebugLn('TBitMap.SetHandle Self=',DbgS(Self),' FImage.FHandle=',DbgS(FImage.FHandle));
  FillChar(FImage.FDIB, SizeOf(FImage.FDIB), 0);
  if FImage.FHandle <> 0 then
    GetObject(FImage.FHandle, SizeOf(FImage.FDIB), @FImage.FDIB);
  if FImage.FMaskHandle<>NewMaskHandle then
    ReplaceBitmapMask(FImage.FHandle,FImage.FMaskHandle,NewMaskHandle);
  FTransparent := FImage.FMaskHandle <> 0;
  Changed(Self);
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
    SetHandles(ImgHandle,ImgMaskHandle);
  finally
    // set save stream
    FImage.SaveStream:=NewSaveStream;
    if (ReaderClass=TFPReaderBMP) or (ReaderClass=TLazReaderBMP) then begin
      FImage.SaveStreamType:=bnWinBitmap;
      FImage.SaveStreamClass:=TFPWriterBMP;
    end else if ReaderClass=TLazReaderXPM then begin
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
  if (FImage.SaveStream<>nil) and (FImage.SaveStream.Size>0)
  and ((FImage.SaveStreamType<>bnNone)
    or CanReadGraphicStreams(FImage.SaveStreamClass))
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
    IntfImg.LoadFromBitmap(Handle,FImage.FMaskHandle);
    
    if WriterClass=nil then begin
      // automatically use a TFPCustomImageWriterClass
      // .bmp does not support transparency
      // Delphi uses a trick and stores the transparent color in the bottom, left
      // pixel. This changes the bitmap and requires to know the transparency
      // on load.
      // The LCL TBitmap is able to load .xpm and .bmp images. .xpm supports
      // transparency. So, if the images has transparency use .xpm.
      IntfImg.GetRawImage(RawImage);
      if RawImageMaskIsEmpty(@RawImage,true) then
        WriterClass:=TFPWriterBMP
      else
        WriterClass:=TLazWriterXPM;
      {debugln('WriteStreamWithFPImage WriterClass=',WriterClass.ClassName,' ',RawImageDescriptionAsString(@RawImage),' MaskSize=',dbgs(RawImage.MaskSize));
      debugln(dbgMemRange(RawImage.Mask,RawImage.MaskSize,
                          GetBytesPerLine(RawImage.Description.Width,
                                         RawImage.Description.AlphaBitsPerPixel,
                                         RawImage.Description.AlphaLineEnd)));}
    end;

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

procedure TBitmap.InitFPImageReader(ImgReader: TFPCustomImageReader);
begin
  if ImgReader is TLazReaderBMP then begin
    // TransparentColor:
    //   clDefault: use left, bottom pixel color as transparent color
    //   clNone: load image opaque
    //   otherwise: use TransparentColor as transparent color
    TLazReaderBMP(ImgReader).UseLeftBottomAsTransparent:=
                                                   (TransparentColor=clDefault);
    if (TransparentColor=clDefault) or (TransparentColor=clNone) then
      TLazReaderBMP(ImgReader).TransparentColor:=colTransparent
    else
      TLazReaderBMP(ImgReader).TransparentColor:=
                                              TColorToFPColor(TransparentColor);
    //DebugLn('TBitmap.InitFPImageReader ',dbgs(TLazReaderBMP(ImgReader).TransparentColor));
  end;
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
  FreeSaveStream;
  FImage.FreeHandle;
  // get the properties from new bitmap
  FImage.FHandle:=Value;
  //DebugLn('TBitMap.SetHandle Self=',DbgS(Self),' FImage.FHandle=',DbgS(FImage.FHandle));
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
  DebugLn('TBitmap.SetMaskHandle Before Replace FImage.FHandle=',DbgS(FImage.FHandle),
    ' FImage.FMaskHandle=',DbgS(FImage.FMaskHandle),
    ' NewMaskHandle=',DbgS(NewMaskHandle));
  {$ENDIF}
  ReplaceBitmapMask(FImage.FHandle,FImage.FMaskHandle,NewMaskHandle);
  FTransparent := FImage.FMaskHandle <> 0;
  {$IFDEF VerboseImgMasks}
  DebugLn('TBitmap.SetMaskHandle After Replace FImage.FHandle=',DbgS(FImage.FHandle),
    ' FImage.FMaskHandle=',DbgS(FImage.FMaskHandle),
    ' NewMaskHandle=',DbgS(NewMaskHandle));
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
  if Value=TransparentMode then exit;
  DebugLn('Note: TBitmap.SetTransparentMode not implemented');
end;

// included by graphics.pp

