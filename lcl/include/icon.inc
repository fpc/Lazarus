{%MainUnit ../graphics.pp}

{******************************************************************************
                                    TCustomIcon
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

const
  IconSignature: array [0..3] of char = #0#0#1#0;
  CursorSignature: array [0..3] of char = #0#0#2#0;
  
type
  TIconHeader = {packed} record // packed it not needed
    idReserved: Word; // 0
    idType: Word;     // 1 - Icon, 2 - Cursor
    idCount: Word;    // number of icons in file
  end;

  TIconDirEntry = {packed} record // packing not needed
    bWidth: Byte;           // a value of 0 means 256
    bHeight: Byte;          // a value of 0 means 256
    bColorCount: Byte;      // number of entires in pallette table below
    bReserved: Byte;        // not used  = 0
    case Byte of
      1: (
        // icon
        wPlanes: Word;      // number of planes, should be 0 or 1
        wBpp: Word;         // bits per pixel
        // common
        dwBytesInRes: Longint;  // total number bytes in images including pallette
                                // data: XOR, AND and bitmap info header
        dwImageOffset: Longint; // pos of image as offset from the beginning of file
      );
      2:(
        // cursor
        wXHotSpot: Word;
        wYHotSpot: Word;
      );
  end;
  
  PIconDirEntry = ^TIconDirEntry;


function TestStreamIsIcon(const AStream: TStream): boolean;
var
  Signature: array[0..3] of char;
  ReadSize: Integer;
  OldPosition: TStreamSeekType;
begin
  OldPosition:=AStream.Position;
  ReadSize:=AStream.Read(Signature, SizeOf(Signature));
  Result:=(ReadSize=SizeOf(Signature)) and CompareMem(@Signature,@IconSignature,4);
  AStream.Position:=OldPosition;
end;

function TestStreamIsCursor(const AStream: TStream): boolean;
var
  Signature: array[0..3] of char;
  ReadSize: Integer;
  OldPosition: TStreamSeekType;
begin
  OldPosition:=AStream.Position;
  ReadSize:=AStream.Read(Signature, SizeOf(Signature));
  Result:=(ReadSize=SizeOf(Signature)) and CompareMem(@Signature,@CursorSignature,4);
  AStream.Position:=OldPosition;
end;

////////////////////////////////////////////////////////////////////////////////

{ TSharedIcon }

procedure TSharedIcon.FreeHandle;
begin
  if FHandle = 0 then Exit;

  DestroyIcon(FHandle);
  FHandle := 0;
end;

function TSharedIcon.IsEmpty: boolean;
begin
  Result := inherited IsEmpty and (Count = 0);
end;

class function TSharedIcon.GetImagesClass: TIconImageClass;
begin
  Result := TIconImage;
end;

procedure TSharedIcon.Add(AIconImage: TIconImage);
begin
  FImages.Add(AIconImage);
end;

constructor TSharedIcon.Create;
begin
  inherited Create;
  FImages := TFPList.Create;
end;

procedure TSharedIcon.Delete(Aindex: Integer);
var
  Image: TIconImage;
begin
  Image := TIconImage(FImages[Aindex]);
  FImages.Delete(AIndex);
  Image.Free;
end;

destructor TSharedIcon.Destroy;
begin
  Clear;
  FreeAndNil(FImages);
  inherited Destroy;
end;

procedure TSharedIcon.Clear;
var
  n: Integer;
begin
  for n := 0 to FImages.Count - 1 do
    TObject(FImages[n]).Free;
end;

function TSharedIcon.GetIndex(AFormat: TPixelFormat; AHeight, AWidth: Word): Integer;
var
  //List: TFPList;
  Image: TIconImage;
begin
  for Result := 0 to FImages.Count -1 do
  begin
    Image := TIconImage(FImages[Result]);
    if Image.PixelFormat <> AFormat then Continue;
    if Image.Height <> AHeight then Continue;
    if Image.Width <> AWidth then Continue;
    // found
    Exit;
  end;
  Result := -1;
end;

function TSharedIcon.Count: Integer;
begin
  Result := FImages.Count;
end;

////////////////////////////////////////////////////////////////////////////////

{ TIconImage }

constructor TIconImage.Create(AFormat: TPixelFormat; AHeight, AWidth: Word);
begin
  inherited Create;
  FHeight := AHeight;
  FWidth := AWidth;
  FPixelFormat := AFormat;
end;

constructor TIconImage.Create(const AImage: TRawImage);
begin
  inherited Create;
  FImage := AImage;
  FHeight := FImage.Description.Height;
  FWidth := FImage.Description.Width;
  
  case FImage.Description.Depth of
     1: FPixelFormat := pf1Bit;
     4: FPixelFormat := pf4Bit;
     8: FPixelFormat := pf8Bit;
    15: FPixelFormat := pf15Bit;
    16: FPixelFormat := pf16Bit;
    24: FPixelFormat := pf24Bit;
    32: FPixelFormat := pf32Bit;
  else
    FPixelFormat := pfCustom;
  end;
end;

destructor TIconImage.Destroy;
begin
  if FHandle <> 0
  then DeleteObject(FHandle);
  FHandle := 0;
  if FMaskHandle <> 0
  then DeleteObject(FMaskHandle);
  FMaskHandle := 0;
  if FPalette <> 0
  then DeleteObject(FPalette);
  FPalette := 0;
  FImage.FreeData;
  inherited Destroy;
end;

function TIconImage.GetPalette: HPALETTE;
begin
  // TODO: implement
  Result := FPalette
end;

function TIconImage.ReleaseHandle: HBITMAP;
begin
  Result := Handle;
  FHandle := 0;
end;

function TIconImage.ReleaseMaskHandle: HBITMAP;
begin
  Result := MaskHandle;
  FMaskHandle := 0;
end;

function TIconImage.ReleasePalette: HPALETTE;
begin
  Result := Palette;
  FPalette := 0;
end;

function TIconImage.UpdateHandles(ABitmap, AMask: HBITMAP): Boolean;
begin
  Result := False;
  
  if FHandle <> ABitmap
  then begin
    if FHandle <> 0
    then DeleteObject(FHandle);
    FHandle := ABitmap;
    Result := True;
  end;

  if FMaskHandle <> AMask
  then begin
    if FMaskHandle <> 0
    then DeleteObject(FMaskHandle);
    FMaskHandle := AMask;
    Result := True;
  end;
end;


////////////////////////////////////////////////////////////////////////////////

{ TCustomIcon }

procedure TCustomIcon.Add(AFormat: TPixelFormat; AHeight, AWidth: Word);
begin
  if GetIndex(AFormat, AHeight, AWidth) <> -1
  then raise EInvalidGraphicOperation.Create(rsDuplicateIconFormat);
  
  UnshareImage(True);
  if TSharedIcon(FSharedImage).FImages.Add(TIconImage.Create(AFormat, AHeight, AWidth)) = 0
  then begin
    // First added
    FCurrent := 0;
    UpdateCurrentView;
  end;
end;

procedure TCustomIcon.Assign(Source: TPersistent);
begin
  if Source is TCustomIcon
  then begin
    FCurrent := -1;
  end
  else
  if Source is TRasterImage
  then begin
    Clear;
    
    with TRasterImage(Source) do
      Self.Add(PixelFormat, Height, Width);

    AssignImage(TRasterImage(Source));
    Exit;
  end;

  inherited Assign(Source);

  if Source is TCustomIcon
  then begin
    FCurrent := TCustomIcon(Source).Current;
  end;
end;

procedure TCustomIcon.AssignImage(ASource: TRasterImage);
var
  Image, NewImage: TIconImage;
  RawImg: PRawImage;
begin
  if FCurrent = -1
  then raise EInvalidGraphicOperation.Create(rsIconNoCurrent);

  if ASource = nil
  then raise EInvalidGraphicOperation.Create(rsIconImageEmpty);

  Image := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]);

  if (Image.Width <> ASource.Width)
  or (Image.Height <> ASource.Height)
  then raise EInvalidGraphicOperation.Create(rsIconImageSize);
  
  if Image.PixelFormat <> ASource.PixelFormat
  then raise EInvalidGraphicOperation.Create(rsIconImageFormat);

  UnshareImage(True);
  FreeCanvasContext;
  
  RawImg := ASource.GetRawImagePtr;
  NewImage := TIconImage.Create(Image.PixelFormat, Image.Height, Image.Width);
  try
    NewImage.FImage.Description := RawImg^.Description;

    NewImage.FImage.DataSize := RawImg^.DataSize;
    if NewImage.FImage.DataSize > 0
    then begin
      NewImage.FImage.Data := GetMem(NewImage.FImage.DataSize);
      Move(RawImg^.Data^, NewImage.FImage.Data^, NewImage.FImage.DataSize);
    end;
    NewImage.FImage.MaskSize := RawImg^.MaskSize;
    if NewImage.FImage.MaskSize > 0
    then begin
      NewImage.FImage.Mask := GetMem(NewImage.FImage.MaskSize);
      Move(RawImg^.Mask^, NewImage.FImage.Mask^, NewImage.FImage.MaskSize);
    end;
    NewImage.FImage.PaletteSize := RawImg^.PaletteSize;
    if NewImage.FImage.PaletteSize > 0
    then begin
      NewImage.FImage.Palette := GetMem(NewImage.FImage.PaletteSize);
      Move(RawImg^.Palette^, NewImage.FImage.Palette^, NewImage.FImage.PaletteSize);
    end;

    // this cannot be shcanged without adjusting data
    // NewImage.FImage.Description.MaskBitsPerPixel := 1;

    TSharedIcon(FSharedImage).FImages[FCurrent] := NewImage;
    NewImage := nil;
    Image.Free;

  finally
    NewImage.Free;
  end;

  Changed(Self);
end;

procedure TCustomIcon.Clear;
begin
  if not Empty then
  begin
    FreeSaveStream;
    FSharedImage.Release;
    FSharedImage := GetSharedImageClass.Create;
    FSharedImage.Reference;
    FCurrent := -1;
    Changed(Self);
  end;
end;

function TCustomIcon.BitmapHandleAllocated: boolean;
begin
  Result := (FCurrent <> -1) and (TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).FHandle <> 0);
end;

constructor TCustomIcon.Create;
begin
  inherited Create;
  FCurrent := -1;
end;

procedure TCustomIcon.Delete(Aindex: Integer);
begin
  UnshareImage(True);
  TSharedIcon(FSharedImage).Delete(AIndex);
  if FCurrent = AIndex
  then begin
    FCurrent := -1;
    UpdateCurrentView;
  end
  else if FCurrent > AIndex
  then begin
    Dec(FCurrent);
  end;
end;

function TCustomIcon.GetCount: Integer;
begin
  Result := TSharedIcon(FSharedImage).Count;
end;

procedure TCustomIcon.GetDescription(Aindex: Integer; out AFormat: TPixelFormat; out AHeight, AWidth: Word);
var
  Image: TIconImage;
begin
  Image := TIconImage(TSharedIcon(FSharedImage).FImages[Aindex]);
  AFormat := Image.PixelFormat;
  AHeight := Image.Height;
  AWidth := Image.Width;
end;

class function TCustomIcon.GetFileExtensions: string;
begin
  Result:='ico';
end;

function TCustomIcon.GetBitmapHandle: HBITMAP;
begin
  if FCurrent = -1
  then Result := 0
  else begin
    BitmapHandleNeeded;
    Result := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).Handle;
  end;
end;

function TCustomIcon.GetIndex(AFormat: TPixelFormat; AHeight, AWidth: Word): Integer;
begin
  Result := TSharedIcon(FSharedImage).GetIndex(AFormat, AHeight, AWidth);
end;

function TCustomIcon.GetMasked: Boolean;
begin
  // per definition an icon is maked, but maybe we should make it settable for alpha images
  Result := True;
end;

function TCustomIcon.GetMaskHandle: HBITMAP;
begin
  if FCurrent = -1
  then Result := 0
  else begin
    MaskHandleNeeded;
    Result := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).MaskHandle;
  end;
end;

function TCustomIcon.GetPalette: HPALETTE;
begin
  if FCurrent = -1
  then Result := 0
  else begin
    PaletteNeeded;
    Result := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).Palette;
  end;
end;

function TCustomIcon.GetPixelFormat: TPixelFormat;
begin
  if FCurrent = -1
  then Result := pfCustom
  else Result := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).PixelFormat;
end;

function TCustomIcon.GetRawImagePtr: PRawImage;
begin
  if FCurrent = -1
  then Result := nil
  else Result := @TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).FImage;
end;

function TCustomIcon.GetRawImageDescriptionPtr: PRawImageDescription;
begin
  if FCurrent = -1
  then Result := nil
  else Result := @TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).FImage.Description;
end;

function TCustomIcon.GetTransparent: Boolean;
begin
  Result := True;
end;

class function TCustomIcon.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedIcon;
end;

procedure TCustomIcon.HandleNeeded;
begin
  {$IFDEF VerboseLCLTodos}{$note TODO implement some WSclass call}{$ENDIF}
end;

function TCustomIcon.InternalReleaseBitmapHandle: HBITMAP;
begin
  if FCurrent = -1
  then Result := 0
  else Result := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).ReleaseHandle;
end;

function TCustomIcon.InternalReleaseMaskHandle: HBITMAP;
begin
  if FCurrent = -1
  then Result := 0
  else Result := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).ReleaseMaskHandle;
end;

function TCustomIcon.InternalReleasePalette: HPALETTE;
begin
  if FCurrent = -1
  then Result := 0
  else Result := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).ReleasePalette;
end;

function TCustomIcon.LazarusResourceTypeValid(const ResourceType: string): boolean;
var
  ResType: String;
begin
  if Length(ResourceType) < 3 then Exit(False);

  ResType := UpperCase(ResourceType);
  case ResType[1] of
    'I': begin
      Result := (ResType = 'ICO') or (ResType = 'ICON');
    end;
  else
    Result := inherited LazarusResourceTypeValid(ResType);
  end;
end;

function TCustomIcon.MaskHandleAllocated: boolean;
begin
  Result := (FCurrent <> -1) and (TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).FMaskHandle <> 0);
end;

procedure TCustomIcon.MaskHandleNeeded;
begin
  // Created by bitmaphandle
  BitmapHandleNeeded;
end;

function TCustomIcon.PaletteAllocated: boolean;
begin
  Result := (FCurrent <> -1) and (TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).FPalette <> 0);
end;

procedure TCustomIcon.PaletteNeeded;
begin
  // nothing to do, handled by image itself
end;

procedure TCustomIcon.ReadData(Stream: TStream);
var
  Signature: array [0..3] of Char;
  Size: longint absolute Signature;
  Position: Int64;
begin
  // Check it the stream is prefixed with a size.
  // Delphi doesn't, while we do.

  Position := Stream.Position;
  Stream.Read(Signature, SizeOf(Signature));
  Stream.Position := Position;
  if Cardinal(Signature) = Cardinal(IconSignature)
  then begin
    // Assume Icon - stream without explicit size
    LoadFromStream(Stream);
  end
  else begin
    // use inherited to read, so "old" streams are converted
    inherited;
  end;
end;

procedure TCustomIcon.ReadStream(AStream: TMemoryStream; ASize: Longint);
var
  Header: TIconHeader;
  StreamStart: Int64;
  IconDir: array of TIconDirEntry;
  n: Integer;
  MaxWidth, MaxHeight, MaxDepth: Word;
  BestIndex: Word;
  IconImage: TIconImage;
  IntfImage: TLazIntfImage;
  PNGSig: array[0..7] of Byte;
  PNGReader: TLazReaderPNG;
  DIBReader: TLazReaderDIB;
  ImgReader: TFPCustomImageReader;
  LazReader: ILazImageReader;
  RawImg: TRawImage;
  Depth: Byte;

begin
  StreamStart := AStream.Position;
  AStream.Read(Header, SizeOf(Header));

  {$ifdef FPC_BIG_ENDIAN}
  // adjust header
  Header.idType := LEtoN(Header.idType);
  Header.idCount := LEtoN(Header.idCount);
  {$endif}
  
  if (Header.idType <> 1) and (Header.idType <> 2)
  then raise EInvalidGraphic.Create('Stream is not an Icon type');
  
  if Header.idCount = 0
  then begin
    AStream.Seek(StreamStart + ASize, soBeginning);
    FCurrent := -1;
    Exit;
  end;

  SetLength(IconDir, Header.idCount);
  AStream.Read(IconDir[0], Header.idCount * SizeOf(IconDir[0]));
  
  // Adjust all entries and find best (atm the order: max width, max height, max depth)
  MaxWidth := 0;
  MaxHeight := 0;
  MaxDepth := 0;
  BestIndex := 0;
  PNGReader := nil;
  DIBReader := nil;
  IntfImage := nil;
  try
    for n := 0 to Header.idCount - 1 do
    begin
      {$ifdef FPC_BIG_ENDIAN}
      // adjust entry
      IconDir[n].wXHotSpot := LEtoN(IconDir[n].wXHotSpot);
      IconDir[n].wYHotSpot := LEtoN(IconDir[n].wYHotSpot);
      IconDir[n].dwBytesInRes := LEtoN(IconDir[n].dwBytesInRes);
      IconDir[n].dwImageOffset := LEtoN(IconDir[n].dwImageOffset);
      {$endif}
      
      AStream.Seek(StreamStart + IconDir[n].dwImageOffset, soBeginning);
      
      ImgReader := nil;
      if (IconDir[n].bWidth = 0) or (IconDir[n].bHeight = 0)
      then begin
        // PNG or DIB image
        // Vista icons are PNG in this case, but there exist also "old style" icons
        // with DIB image
        
        // don't use PNGReader.CheckContents(AStream) since it uses internally
        // an exception for checking, which is not "nice" when debugging.
        AStream.Read(PNGSig, SizeOf(PNGSig));
        AStream.Seek(StreamStart + IconDir[n].dwImageOffset, soBeginning);

        if QWord(PNGComn.Signature) = QWord(PNGSig)
        then begin
          if PNGReader = nil
          then PNGReader := TLazReaderPNG.Create;
          ImgReader := PNGReader;
        end;
      end;
      
      if ImgReader = nil
      then begin
        // DIB image
        if DIBReader = nil
        then DIBReader := TLazReaderIconDIB.Create;
        ImgReader := DIBReader;
      end;
      
      // create or reset intfimage
      if IntfImage = nil
      then IntfImage := TLazIntfImage.Create(0,0)
      else IntfImage.SetSize(0,0);

      if Supports(ImgReader, ILazImageReader, LazReader) 
      then LazReader.UpdateDescription := True
      else IntfImage.DataDescription := QueryDescription([riqfRGB, riqfAlpha, riqfMask]); // fallback to default
      ImgReader.ImageRead(AStream, IntfImage);

      // update best image index
      if IntfImage.Height > MaxHeight
      then begin
        MaxHeight := IntfImage.Height;
        BestIndex := n;
      end;

      if (IntfImage.Height = MaxHeight)
      and (IntfImage.Width > MaxWidth)
      then begin
        MaxWidth := IntfImage.Width;
        BestIndex := n;
      end;

      if (IntfImage.Height = MaxHeight)
      and (IntfImage.Width = MaxWidth)
      then begin
        // new icons have bpp in direntry, older not.
        // So use it only for png (which itself is alway at bpp=32)
        if (IconDir[n].bWidth = 0) or (IconDir[n].bHeight = 0)
        then Depth := IconDir[n].wBpp
        else Depth := IntfImage.DataDescription.Depth;

        if Depth > MaxDepth
        then begin
          MaxDepth := Depth;
          BestIndex := n;
        end;
      end;

      // Add image
      IntfImage.GetRawImage(RawImg, True);
      // Paul: don't set MaskBitsPerPixel to zero => windows will fail with no mask
      // Even empty mask is better than no mask. But maybe CreateIconIndirect must be fixed on windows?
      RawImg.Description.MaskBitsPerPixel := 1;
      with TSharedIcon(FSharedImage) do
      begin
        IconImage := GetImagesClass.Create(RawImg);
        if IconImage is TCursorImageImage then
          TCursorImageImage(IconImage).HotSpot := Point(IconDir[n].wXHotSpot, IconDir[n].wYHotSpot);
        FImages.Add(IconImage);
      end;
    end;
  finally
    LazReader := nil;
    DIBReader.Free;
    PNGReader.Free;
    IntfImage.Free;
  end;
  FCurrent := BestIndex;
end;

procedure TCustomIcon.Remove(AFormat: TPixelFormat; AHeight, AWidth: Word);
var
  idx: Integer;
begin
  idx := GetIndex(AFormat, AHeight, AWidth);
  if idx <> -1 then Delete(idx);
end;

procedure TCustomIcon.SetCurrent(const AValue: Integer);
begin
  if FCurrent = AValue then exit;
  FCurrent := AValue;
  UpdateCurrentView;
end;

procedure TCustomIcon.SetHandles(ABitmap, AMask: HBITMAP);
begin
  {$IFDEF VerboseLCLTodos}{$note Implement me (or raise exception)}{$ENDIF}
end;

procedure TCustomIcon.SetMasked(AValue: Boolean);
begin
  // nothing
end;

function TCustomIcon.GetBestApplicationIndex: Integer;
var
  BestCX, BestCY, BestDepth, i, dx, dy, dd: Integer;
  CurRawImage: TRawImage;
  ScreenDC: HDC;
begin
  Result := -1;
  
  BestCX := GetSystemMetrics(SM_CXICON);
  if BestCX = -1 then
    BestCX := 32;

  BestCY := GetSystemMetrics(SM_CYICON);
  if BestCY = -1 then
    BestCY := 32;

  ScreenDC := GetDC(0);
  BestDepth := GetDeviceCaps(ScreenDC, BITSPIXEL);
  ReleaseDC(0, ScreenDC);

  dx := MaxInt;
  dy := MaxInt;
  dd := MaxInt;

  for i := 0 to Count - 1 do
  begin
    CurRawImage := TIconImage(TSharedIcon(FSharedImage).FImages[i]).FImage;
    if Abs(BestCX - CurRawImage.Description.Width) < dx then
    begin
      dx := Abs(BestCX - CurRawImage.Description.Width);
      Result := i;
    end
    else
    if Abs(BestCX - CurRawImage.Description.Width) = dx then
    begin
      if Abs(BestCY - CurRawImage.Description.Height) < dy then
      begin
        dy := Abs(BestCY - CurRawImage.Description.Height);
        Result := i;
      end
      else
      if Abs(BestCY - CurRawImage.Description.Height) = dy then
      begin
        if Abs(BestDepth - CurRawImage.Description.Depth) < dd then
        begin
          dd := Abs(BestDepth - CurRawImage.Description.Depth);
          Result := i;
        end;
      end;
    end
  end;
end;

procedure TCustomIcon.SetPixelFormat(AValue: TPixelFormat);
begin
  raise EInvalidGraphicOperation.Create(rsIconImageFormatChange);
end;

procedure TCustomIcon.SetTransparent(Value: Boolean);
begin
  // nothing
end;

procedure TCustomIcon.SetSize(AWidth, AHeight: integer);
begin
  raise EInvalidGraphicOperation.Create(rsIconImageSizeChange);
end;

procedure TCustomIcon.UnshareImage(CopyContent: boolean);
var
  NewIcon, OldIcon: TSharedIcon;
  n: Integer;
  OldImage, NewImage: TIconImage;
  OldSharedImage: TSharedImage;
begin
  if FSharedImage.RefCount <= 1 then Exit;

  NewIcon := GetSharedImageClass.Create as TSharedIcon;
  try
    NewIcon.Reference;
    if CopyContent
    then begin
      OldIcon := FSharedImage as TSharedIcon;
      for n := 0 to OldIcon.FImages.Count -1 do
      begin
        OldImage := TIconImage(OldIcon.FImages[n]);
        NewImage := TIconImage.Create(OldImage.PixelFormat, OldImage.Height, OldImage.Width);
        NewIcon.FImages.Add(NewImage);
        NewImage.FImage.Description := OldImage.FImage.Description;
        NewImage.FImage.DataSize := OldImage.FImage.DataSize;
        if NewImage.FImage.DataSize > 0
        then begin
          NewImage.FImage.Data := GetMem(NewImage.FImage.DataSize);
          Move(OldImage.FImage.Data^, NewImage.FImage.Data^, NewImage.FImage.DataSize);
        end;
        NewImage.FImage.MaskSize := OldImage.FImage.MaskSize;
        if NewImage.FImage.MaskSize > 0
        then begin
          NewImage.FImage.Mask := GetMem(NewImage.FImage.MaskSize);
          Move(OldImage.FImage.Mask^, NewImage.FImage.Mask^, NewImage.FImage.MaskSize);
        end;
        NewImage.FImage.PaletteSize := OldImage.FImage.PaletteSize;
        if NewImage.FImage.PaletteSize > 0
        then begin
          NewImage.FImage.Palette := GetMem(NewImage.FImage.PaletteSize);
          Move(OldImage.FImage.Palette^, NewImage.FImage.Palette^, NewImage.FImage.PaletteSize);
        end;
      end;
    end;
    FreeCanvasContext;
    OldSharedImage := FSharedImage;
    FSharedImage := NewIcon;

    NewIcon := nil; // transaction sucessful
    OldSharedImage.Release;
  finally
    // in case something goes wrong, keep old and free new
    NewIcon.Free;
  end;
end;

procedure TCustomIcon.UpdateCurrentView;
begin
  {$IFDEF VerboseLCLTodos}{$note implement me}{$ENDIF}
  FreeCanvasContext;
  Changed(Self);
end;

function TCustomIcon.UpdateHandles(ABitmap, AMask: HBITMAP): Boolean;
var
  Image: TIconImage;
begin
  if FCurrent = -1
  then begin
    Result := False;
    Exit;
  end;
  
  Image := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]);
  Result := Image.UpdateHandles(ABitmap, AMask);
end;

procedure TCustomIcon.WriteStream(AStream: TMemoryStream);
begin
  {$IFDEF VerboseLCLTodos}{$note implement me}{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////

{ TIcon }

function TIcon.GetIconHandle: HICON;
begin
  Result := GetHandle;
end;

function TIcon.ReleaseHandle: HICON;
begin
  HandleNeeded;
  Result := FSharedImage.ReleaseHandle;
end;

procedure TIcon.SetIconHandle(const AValue: HICON);
begin
  SetHandle(AValue);
end;

procedure TIcon.HandleNeeded;
var
  IconInfo: TIconInfo;
begin
  if FSharedImage.FHandle <> 0 then Exit;

  IconInfo.fIcon := True;
  IconInfo.hbmMask := MaskHandle;
  IconInfo.hbmColor := BitmapHandle;
  FSharedImage.FHandle := WidgetSet.CreateIconIndirect(@IconInfo);
end;


