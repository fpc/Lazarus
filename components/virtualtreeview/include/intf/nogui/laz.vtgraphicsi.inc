procedure AlphaBlend(Source, Destination: HDC; const R: TRect; const Target: TPoint; Mode: TBlendMode; ConstantAlpha, Bias: Integer);
begin
end;

function CalculateScanline(Bits: Pointer; Width, Height, Row: Integer): Pointer;
begin
  Result := nil;
end;

function GetBitmapBitsFromBitmap(Bitmap: HBITMAP): Pointer;
begin
  Result := nil;
end;
