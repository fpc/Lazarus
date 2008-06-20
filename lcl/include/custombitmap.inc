{%MainUnit ../graphics.pp}

{******************************************************************************
                                     TCustomBitmap
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


function TCustomBitmap.BitmapHandleAllocated: boolean;
begin
  // for custombitmap handle = bitmaphandle
  Result := FSharedImage.FHandle <> 0;
end;

function TCustomBitmap.CanShareImage(AClass: TSharedRasterImageClass): Boolean;
begin
  Result := (AClass <> TSharedCustomBitmap)
         and inherited CanShareImage(AClass);
end;

procedure TCustomBitmap.Changed(Sender: TObject);
begin
  // When the bitmap is changed by the canvas, the rawimage data isn't valid anymore
  if Sender = FCanvas
  then TSharedCustomBitmap(FSharedImage).FImage.FreeData;
  inherited Changed(Sender);
end;

constructor TCustomBitmap.Create;
begin
  inherited Create;
  FPixelFormat := pfDevice;
end;

procedure TCustomBitmap.HandleNeeded;
begin
  BitmapHandleNeeded;
end;

function TCustomBitmap.MaskHandleAllocated: boolean;
begin
  Result := FMaskHandle <> 0;
end;

procedure TCustomBitmap.MaskHandleNeeded;
begin
  if FMaskHandle <> 0 then exit;
  // not now, breaks alpha images, since they report themselves as transparent
  // while no mask is needed
  // CreateMask;
end;

function TCustomBitmap.PaletteAllocated: boolean;
begin
  Result := TSharedCustomBitmap(FSharedImage).FPalette <> 0;
end;

procedure TCustomBitmap.PaletteNeeded;
begin
  // TODO: implement
end;

procedure TCustomBitmap.RawimageNeeded(ADescOnly: Boolean);
var
  OldChangeEvent: TNotifyEvent;
  ImagePtr: PRawImage;
begin
  ImagePtr := @TSharedCustomBitmap(FSharedImage).FImage;
  if ImagePtr^.Description.Format <> ricfNone
  then begin
    // description valid
    if ADescOnly then Exit;
    if (ImagePtr^.Data <> nil) and (ImagePtr^.DataSize > 0) then Exit;
  end;
  

  // TODO description based on pixelformat
  if FSharedImage.FSaveStream = nil
  then begin
    if FSharedImage.FHandle <> 0
    then begin
      if ADescOnly
      or not RawImage_FromBitmap(ImagePtr^, FSharedImage.FHandle, FMaskHandle)
      then ImagePtr^.Description := GetDescriptionFromBitmap(FSharedImage.FHandle);
      Exit;
    end;
  
    ImagePtr^.Description := GetDescriptionFromDevice(0, 0, 0);
    Exit;
  end;
  
  FSharedImage.FSaveStream.Position := 0;
  OldChangeEvent := OnChange;
  try
    OnChange := nil;
    LoadFromStream(FSharedImage.FSaveStream);
  finally
    OnChange := OldChangeEvent;
  end;
end;

function TCustomBitmap.ReleaseHandle: HBITMAP;
begin
  HandleNeeded;
  Result := FSharedImage.ReleaseHandle;
end;

function TCustomBitmap.LazarusResourceTypeValid(const ResourceType: string): boolean;
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

function TCustomBitmap.GetHandleType: TBitmapHandleType;
begin
  Result := TSharedCustomBitmap(FSharedImage).HandleType;
end;

function TCustomBitmap.GetMaskHandle: HBITMAP;
begin
  MaskHandleNeeded;
  Result := FMaskHandle;
end;

procedure TCustomBitmap.SetHandleType(AValue: TBitmapHandleType);
begin
  if HandleType = AValue then exit;
  DebugLn('TCustomBitmap.SetHandleType TCustomBitmap.SetHandleType not implemented');
end;

procedure TCustomBitmap.SetMonochrome(AValue: Boolean);
begin
  if Monochrome = AValue then exit;
  if not AValue then Exit;
  
  UnshareImage(False);
  TSharedCustomBitmap(FSharedImage).FDIB.dsbm.bmPlanes := 1;
  TSharedCustomBitmap(FSharedImage).FDIB.dsbm.bmBitsPixel := 1;
  FPixelFormat := pf1bit;
end;

procedure TCustomBitmap.SetPixelFormat(AValue: TPixelFormat);
begin
  if AValue = FPixelFormat then Exit;
  FreeImage;
  FPixelFormat := AValue;
end;

procedure TCustomBitmap.SetSize(AWidth, AHeight: integer);
begin
  if  (TSharedCustomBitmap(FSharedImage).FDIB.dsbm.bmHeight = AHeight)
  and (TSharedCustomBitmap(FSharedImage).FDIB.dsbm.bmWidth = AWidth)
  then Exit;

  UnshareImage(False);
  TSharedCustomBitmap(FSharedImage).FDIB.dsbm.bmWidth := AWidth;
  TSharedCustomBitmap(FSharedImage).FDIB.dsbm.bmHeight := AHeight;
  Changed(Self);
end;

procedure TCustomBitmap.UpdatePixelFormat;
begin
  FPixelFormat := TSharedCustomBitmap(FSharedImage).GetPixelFormat;
end;

procedure TCustomBitmap.Changing(Sender: TObject);
// called before the bitmap is modified
// -> make sure the handle is unshared (otherwise the modifications will also
//    modify all copies)
begin
  inherited;
  TSharedCustomBitmap(FSharedImage).FDIB.dsbmih.biClrUsed := 0;
  TSharedCustomBitmap(FSharedImage).FDIB.dsbmih.biClrImportant := 0;
end;

function TCustomBitmap.GetTransparent: Boolean;
begin
  {$IFDEF VerboseLCLTodos}{$note add better check for transparency }{$ENDIF}
  // MWE: now tharansparency is set when a maskhandle is assigned, the user can
  // override this by setting it to false, so no mask is used,
  // however this meganism ignores the possible alpha channel, so for now 32bit
  // bitmaps are considered transparent
  // todos:
  //  check for device transparency
  //  check for transparency through palette etc.
  Result := (FPixelFormat = pf32bit) or inherited GetTransparent;
end;

function TCustomBitmap.GetMonochrome: Boolean;
begin
  with TSharedCustomBitmap(FSharedImage).FDIB.dsbm do
    Result := (bmPlanes = 1) and (bmBitsPixel = 1);
end;

procedure TCustomBitmap.UnshareImage(CopyContent: boolean);
var
  NewImage: TSharedCustomBitmap;
  OldImage: TSharedCustomBitmap;
begin
  if FSharedImage.RefCount <= 1 then Exit;
  
  // release old FImage and create a new one
  OldImage := FSharedImage as TSharedCustomBitmap;
  NewImage := GetSharedImageClass.Create as TSharedCustomBitmap;
  try
    NewImage.Reference;
    if CopyContent and OldImage.ImageAllocated
    then OldImage.FImage.ExtractRect(Rect(0, 0, Width, Height), NewImage.FImage);

    FreeCanvasContext;
    FSharedImage := NewImage;
    NewImage := nil; // transaction sucessful
    OldImage.Release;
  finally
    // in case something goes wrong, keep old and free new
    NewImage.Free;
  end;
end;

function TCustomBitmap.UpdateHandles(ABitmap, AMask: HBITMAP): Boolean;
begin
  // Update sets the handles corresponding to our rawimage and/or savestream, so
  // we dont free them here.
  
  Result := False;

  if FSharedImage.FHandle <> ABitmap
  then begin
    FSharedImage.FreeHandle;
    // get the properties from new bitmap
    FSharedImage.FHandle := ABitmap;
    TSharedCustomBitmap(FSharedImage).UpdateDIB;
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

function TCustomBitmap.GetBitmapHandle: HBITMAP;
begin
  BitmapHandleNeeded;
  Result := FSharedImage.FHandle;
end;

procedure TCustomBitmap.SetHandles(ABitmap, AMask: HBITMAP);
begin
  if FSharedImage.FHandle <> ABitmap
  then begin
    // if the handle is set externally we should unshare ourselves
    FreeCanvasContext;
    UnshareImage(false);
    FreeSaveStream;
    TSharedCustomBitmap(FSharedImage).FreeImage;
  end;

  if UpdateHandles(ABitmap, AMask)
  then Changed(Self);
end;

procedure TCustomBitmap.SetHandle(AValue: THandle);
begin
  // for TCustomBitmap BitmapHandle = Handle

  BitmapHandle := AValue;
end;

function TCustomBitmap.InternalReleaseBitmapHandle: HBITMAP;
begin
  Result := FSharedImage.ReleaseHandle;
end;

function TCustomBitmap.InternalReleaseMaskHandle: HBITMAP;
begin
  Result := FMaskHandle;
  FMaskHandle := 0;
end;

function TCustomBitmap.InternalReleasePalette: HPALETTE;
begin
  Result := TSharedCustomBitmap(FSharedImage).ReleasePalette;
end;

function TCustomBitmap.GetPalette: HPALETTE;
begin
  PaletteNeeded;
  Result := TSharedCustomBitmap(FSharedImage).FPalette;
end;

function TCustomBitmap.GetPixelFormat: TPixelFormat;
begin
  Result := FPixelFormat;
end;

function TCustomBitmap.GetRawImage: PRawImage;
begin
  RawimageNeeded(False);
  Result := @TSharedCustomBitmap(FSharedImage).FImage;
end;

function TCustomBitmap.GetRawImageDescription: PRawImageDescription;
begin
  RawimageNeeded(True);
  Result := @TSharedCustomBitmap(FSharedImage).FImage.Description;
end;

function TCustomBitmap.GetResourceType: TResourceType;
begin
  Result := RT_BITMAP;
end;


