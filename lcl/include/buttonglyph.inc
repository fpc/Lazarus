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
Procedure TButtonGlyph.SetGlyph(Value : TBitmap);
Begin
  if FOriginal = Value then exit;
  //Invalidate;
  FOriginal.Free;
  FOriginal := Value;
  //FOriginal.Assign(Value);
end;

{------------------------------------------------------------------------------}
{       TButtonGlyph Draw                                                      }
{------------------------------------------------------------------------------}
Function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint;  const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer;  State: TButtonState;  Transparent: Boolean;
  BiDiFlags: Longint): TRect;
var
  gWidth : integer;
  gHeight : integer;
  DestRect: TRect;
  ImgID: integer;
begin
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
Procedure TButtonGlyph.SetNumGlyphs(Value : TNumGlyphs);
Begin
  if Value <> FNumGlyphs then
     Begin
       FNumGlyphs := Value;
       if Assigned(FOnChange) then FOnChange(Glyph);
     end;

end;

