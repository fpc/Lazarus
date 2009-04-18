{%MainUnit ../graphics.pp}

{ TSharedCustomBitmap

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

constructor TSharedCustomBitmap.Create;
begin
  inherited Create;
  FImage.Init;
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

function TSharedCustomBitmap.ReleasePalette: HPALETTE;
begin
  Result := FPalette;
  FPalette := 0;
end;

function TSharedCustomBitmap.GetPixelFormat: TPixelFormat;
begin
  if FImage.Description.Format = ricfNone then Exit(pfDevice);

  case FImage.Description.Depth of
     1: Exit(pf1Bit);
     4: Exit(pf4Bit);
     8: Exit(pf8Bit);
    15: Exit(pf15Bit);
    16: Exit(pf16Bit);
    24: Exit(pf24Bit);
    32: Exit(pf32Bit);
  end;
  Result := pfCustom;
end;

function TSharedCustomBitmap.IsEmpty: boolean;
begin
  Result := inherited IsEmpty and not
    (
      (FImage.Description.Format <> ricfNone) and
      (FImage.Data <> nil) and
      (FImage.DataSize <> 0)
    );
end;

function TSharedCustomBitmap.GetWidth: Integer;
begin
  if FImage.Description.Format = ricfNone
  then Result := 0
  else Result := FImage.Description.Width;
end;

// included by graphics.pp
