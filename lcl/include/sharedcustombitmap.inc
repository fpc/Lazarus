{%MainUnit ../graphics.pp}

{ TSharedCustomBitmap

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

constructor TSharedCustomBitmap.Create;
begin
  inherited Create;
end;

procedure TSharedCustomBitmap.CreateDefaultHandle(AWidth, AHeight: Integer; ABPP: Byte);
var
  DC: HDC;
  BI: PBitmapInfo;
begin
  if FHandle <> 0 then raise EInvalidOperation.Create('Handle already set');
  
  FDIB.dsBm.bmBits := nil;
  if ABPP = 1
  then begin
    FHandle := CreateBitmap(AWidth, AHeight, 1, ABPP, nil);
    FHandleType := bmDDB;
  end
  else begin
    // on windows we need a DIB section
    BI := @FDIB.dsBmih;
    FillChar(BI^.bmiHeader, SizeOf(BI^.bmiHeader), 0);
    BI^.bmiHeader.biSize := SizeOf(BI^.bmiHeader);
    BI^.bmiHeader.biWidth := AWidth;
    BI^.bmiHeader.biHeight := -AHeight; // request top down
    BI^.bmiHeader.biPlanes := 1;
    BI^.bmiHeader.biBitCount := ABPP;
    BI^.bmiHeader.biCompression := BI_RGB;
    DC := GetDC(0);
    FHandle := CreateDIBSection(DC, BI^, DIB_RGB_COLORS, FDIB.dsBm.bmBits, 0, 0);
    FHandleType := bmDIB;
    ReleaseDC(0, DC);

    // fallback for other widgetsets not implementing CreateDIBSection
    // we need the DIB section anyway someday if we want a scanline
    if FHandle = 0
    then begin
      FHandle := CreateBitmap(AWidth, AHeight, 1, ABPP, nil);
      FHandleType := bmDDB;
    end;
  end;

  FDIB.dsbm.bmWidth := AWidth;
  FDIB.dsbm.bmHeight := AHeight;
end;

destructor TSharedCustomBitmap.Destroy;
begin
  FreeAndNil(FSaveStream);
  FreeHandle;
  FreeImage;
  inherited Destroy;
end;

procedure TSharedCustomBitmap.FreeHandle;
begin
  FreePalette;
  inherited FreeHandle;
end;

procedure TSharedCustomBitmap.FreeImage;
begin
  FImage.FreeData;
  FImage.Description.Format := ricfNone;
end;

procedure TSharedCustomBitmap.FreePalette;
begin
  if FPalette = 0 then Exit;

  DeleteObject(FPalette);
  FPalette := 0;
end;

function TSharedCustomBitmap.GetHeight: Integer;
begin
  if FImage.Description.Format = ricfNone
  then Result := 0
  else Result := FImage.Description.Height;
end;

function TSharedCustomBitmap.HandleAllocated: boolean;
begin
  Result := FHandle <> 0;
end;

function TSharedCustomBitmap.ImageAllocated: boolean;
begin
  Result := FImage.Description.Format <> ricfNone;
end;

function TSharedCustomBitmap.ReleaseHandle: THandle;
begin
  Result := inherited ReleaseHandle;
  FDIB.dsbm.bmBits := nil;
end;

function TSharedCustomBitmap.ReleasePalette: HPALETTE;
begin
  Result := FPalette;
  FPalette := 0;
end;

procedure TSharedCustomBitmap.UpdateDIB;
begin
  FillChar(FDIB, SizeOf(FDIB), 0);
  if FHandle <> 0
  then GetObject(FHandle, SizeOf(FDIB), @FDIB);
end;

function TSharedCustomBitmap.GetPixelFormat: TPixelFormat;
begin
  if HandleType = bmDDB
  then begin
    if FDIB.dsBmih.biBitCount = 1 then Exit(pf1Bit);
    Exit(pfDevice);
  end;
  
  case FDIB.dsBmih.biBitCount of
     1: Exit(pf1Bit);
     4: Exit(pf4Bit);
     8: Exit(pf8Bit);
    16: begin
      if FDIB.dsBmih.biCompression = BI_RGB then Exit(pf15Bit);
      if  (FDIB.dsBmih.biCompression = BI_BITFIELDS)
      and (FDIB.dsBitFields[1] = $7E0) then Exit(pf16Bit);
    end;
    24: Exit(pf24Bit);
    32: begin
      if FDIB.dsBmih.biCompression = BI_RGB then Exit(pf32Bit);
    end;
  end;
  Result := pfCustom;
end;

function TSharedCustomBitmap.GetWidth: Integer;
begin
  if FImage.Description.Format = ricfNone
  then Result := 0
  else Result := FImage.Description.Width;
end;

// included by graphics.pp
