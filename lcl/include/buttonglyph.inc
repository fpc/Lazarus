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
var GlyphCount : integer;
begin
  if FOriginal = Value then exit;
//  FOriginal.Assign(Value);
  FOriginal.Free;
  FOriginal:= Value;
  if (Value <> nil) and (Value.Height > 0) then begin
    if Value.Width mod Value.Height = 0 then begin
      GlyphCount:= Value.Width div Value.Height;
      if GlyphCount > 4 then GlyphCount:= 1;
      NumGlyphs:= GlyphCount;
    end;
  end;
  //Invalidate;
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
  inc(DestRect.Right,Offset.X);
  inc(DestRect.Top,Offset.Y);
  inc(DestRect.Bottom,Offset.Y);
  if not Transparent then
    Canvas.Copyrect(DestRect, TPixmap(FOriginal).Canvas, Result)
  else
    ;    
end;


{------------------------------------------------------------------------------}
{       TButtonGlyph SetNumGlyphs                                               }
{------------------------------------------------------------------------------}
procedure TButtonGlyph.SetNumGlyphs(Value : TNumGlyphs);
begin
  if Value <> FNumGlyphs then begin
    FNumGlyphs := Value;
    if Assigned(FOnChange) then FOnChange(Glyph);
  end;
end;
