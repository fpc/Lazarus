// included by buttons.pp

{
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

{------------------------------------------------------------------------------}
{       TButtonGlyph Constructor                                                    }
{------------------------------------------------------------------------------}
constructor TButtonGlyph.Create;
begin
//  Inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := @GlyphChanged;
end;

{------------------------------------------------------------------------------}
{       TButtonGlyph destructor                                                    }
{------------------------------------------------------------------------------}
destructor TButtonGlyph.Destroy;
Begin
  FOriginal.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{       TButtonGlyph SetGlyph                                                  }
{------------------------------------------------------------------------------}
procedure TButtonGlyph.SetGlyph(Value : TBitmap);
var
  GlyphCount : integer;
begin
  if FOriginal = Value then exit;
//  FOriginal.Assign(Value);
  FOriginal.Free;
  FOriginal:= Value;
  FOriginal.OnChange := @GlyphChanged;
  FNumGlyphs:=1;
  if (FOriginal <> nil) and (FOriginal.Height > 0) then begin
    if FOriginal.Width mod FOriginal.Height = 0 then begin
      GlyphCount:= FOriginal.Width div FOriginal.Height;
      if GlyphCount > 4 then GlyphCount:= 1;
      FNumGlyphs:= GlyphCount;
    end;
  end;
  GlyphChanged(FOriginal);
end;

procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
    if Assigned(FOnChange) then FOnChange(Self);
end;

{------------------------------------------------------------------------------}
{       TButtonGlyph Draw                                                      }
{------------------------------------------------------------------------------}
Function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint;  State: TButtonState;  Transparent: Boolean;
  BiDiFlags: Longint): TRect;
var
  gWidth : integer;
  gHeight : integer;
  DestRect: TRect;
  ImgID: integer;
begin
  if FOriginal = nil then Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  
  gWidth := TPixMap(FOriginal).Width;
  gHeight := TPixMap(FOriginal).Height;
  if NumGlyphs > 1 then
    gWidth := gWidth div NumGlyphs;
    
  ImgID:=0;
  case State of
    bsDisabled: if NumGlyphs>1 then ImgID:=1;
    bsDown:  if NumGlyphs>2 then ImgID:=2;
  end;

  Result := Rect((ImgID*gWidth), 0, ((ImgID+1)*gWidth), gHeight);
  DestRect:=Client;
  inc(DestRect.Left,Offset.X);
  inc(DestRect.Top,Offset.Y);
  If DestRect.Right > DestRect.Left + Result.Right - Result.Left then
    DestRect.Right := DestRect.Left + Result.Right - Result.Left;
  If DestRect.Bottom > DestRect.Top + gHeight then
    DestRect.Bottom := DestRect.Top + gHeight;
  If (Result.Right - Result.Left) <> (DestRect.Right - DestRect.Left) then
    Result.Right := Result.Left + DestRect.Right - DestRect.Left;
  If (Result.Bottom - Result.Top) <> (DestRect.Bottom - DestRect.Top) then
    Result.Bottom := Result.Top + DestRect.Bottom - DestRect.Top;
  Canvas.Copyrect(DestRect, TPixmap(FOriginal).Canvas, Result)
end;


{------------------------------------------------------------------------------}
{       TButtonGlyph SetNumGlyphs                                               }
{------------------------------------------------------------------------------}
procedure TButtonGlyph.SetNumGlyphs(Value : TNumGlyphs);
begin
  if Value <> FNumGlyphs then begin
    FNumGlyphs := Value;
    GlyphChanged(FOriginal);
  end;
end;

// included by buttons.pp

