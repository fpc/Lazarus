{%MainUnit ../buttons.pp}

{
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

{------------------------------------------------------------------------------}
{       TButtonGlyph Constructor                                                    }
{------------------------------------------------------------------------------}
constructor TButtonGlyph.Create;
begin
//  Inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.Handle := 0;
  FOriginal.OnChange := @GlyphChanged;
end;

{------------------------------------------------------------------------------
       TButtonGlyph destructor
------------------------------------------------------------------------------}
destructor TButtonGlyph.Destroy;
begin
  FOriginal.Free;
  FOriginal:=nil;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
       TButtonGlyph SetGlyph
------------------------------------------------------------------------------}
procedure TButtonGlyph.SetGlyph(Value : TBitmap);
var
  GlyphCount : integer;
begin
  if FOriginal = Value then exit;
  if FOriginal=nil then begin
    FOriginal:=TBitmap.Create;
  end;
  FOriginal.OnChange:=nil;
  FOriginal.Assign(Value);
  FOriginal.OnChange := @GlyphChanged;
  FNumGlyphs:=1;
  if (FOriginal <> nil) and (FOriginal.Height > 0) then begin
    if FOriginal.Width mod FOriginal.Height = 0 then begin
      GlyphCount:= FOriginal.Width div FOriginal.Height;
      if GlyphCount > 4 then GlyphCount:= 1;
      FNumGlyphs:= TNumGlyphs(GlyphCount);
    end;
  end;
  GlyphChanged(FOriginal);
end;

procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
    if Assigned(FOnChange) then FOnChange(Self);
end;

{------------------------------------------------------------------------------
       TButtonGlyph Draw
------------------------------------------------------------------------------}
Function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint;  State: TButtonState;  Transparent: Boolean;
  BiDiFlags: Longint): TRect;
var
  gWidth: integer;
  gHeight: integer;
  DestRect, SrcRect: TRect;
  ImgID: integer;
  UseMaskHandle: HBitmap;
  src_wh, dst_wh: Integer;
begin
  Result:=Client;
  if (FOriginal = nil) then exit;

  gWidth := FOriginal.Width;
  gHeight := FOriginal.Height;
  if (gWidth = 0) or (gHeight = 0)
  or (Client.Left>=Client.Right) or (Client.Top>=Client.Bottom) then Exit;
  
  if NumGlyphs > 1 then
    gWidth := gWidth div NumGlyphs;
    
  ImgID:=0;
  case State of
    bsDisabled:  if NumGlyphs>1 then ImgID:=1;
    bsDown:      if NumGlyphs>2 then ImgID:=2;
    bsExclusive: if NumGlyphs>3 then ImgID:=3;
  end;

  SrcRect := Rect((ImgID*gWidth), 0, ((ImgID+1)*gWidth), gHeight);
  DestRect:=Client;
  

  Inc(DestRect.Left, Offset.X);
  src_wh:=SrcRect.Right-SrcRect.Left; dst_wh:=DestRect.Right-DestRect.Left;
  if (dst_wh>src_wh) then
    DestRect.Right:=DestRect.Left+src_wh // if window for image is wider
  else if (dst_wh<src_wh) then
      SrcRect.Right:=SrcRect.Left+dst_wh; // if image not fits in their window width

  Inc(DestRect.Top, Offset.Y);
  src_wh:=SrcRect.Bottom-SrcRect.Top; dst_wh:=DestRect.Bottom-DestRect.Top;
  if (dst_wh>src_wh) then
    DestRect.Bottom:=DestRect.Top+src_wh // if window for image is higher
  else if (dst_wh<src_wh) then
      SrcRect.Bottom:=SrcRect.Top+dst_wh; // if image not fits in their window height
  
  //Canvas.CopyRect(DestRect, FOriginal.Canvas, SrcRect)
  UseMaskHandle:=FOriginal.MaskHandle;
  MaskBlt(Canvas.GetUpdatedHandle([csHandleValid]),
          DestRect.Left,DestRect.Top,
          DestRect.Right-DestRect.Left,DestRect.Bottom-DestRect.Top,
          FOriginal.Canvas.GetUpdatedHandle([csHandleValid]),
          SrcRect.Left,SrcRect.Top,
          UseMaskHandle,SrcRect.Left,SrcRect.Top);

  // ToDo: VCL returns the text rectangle
  Result:=SrcRect;
end;


{------------------------------------------------------------------------------
       TButtonGlyph SetNumGlyphs
------------------------------------------------------------------------------}
procedure TButtonGlyph.SetNumGlyphs(Value : TNumGlyphs);
begin
  if Value <> FNumGlyphs then begin
    FNumGlyphs := Value;
    GlyphChanged(FOriginal);
  end;
end;

// included by buttons.pp
