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
Function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; Transparent: Boolean; BiDiFlags: Longint): TRect;
var
   gWidth : integer;
   gHeight : integer;

begin
   // for default assume only 1 glyph

   gWidth := TPixMap(FOriginal).Width;
   gHeight := TPixMap(FOriginal).Height;
   Result := Rect(0, 0, gWidth - 1, gHeight - 1);

   if NumGlyphs > 1 then
   begin
      gWidth := TPixMap(FOriginal).Width div NumGlyphs;

      if (State = bsDown) and (NumGlyphs < 3) then
         State := bsUp;

      if State = bsDisabled then
         Result := Rect(gWidth, 0, (2 * gWidth) - 1, gHeight - 1)
      else if State = bsDown then
         Result := Rect(2 * gWidth, 0, (3 * gWidth) - 1, gHeight - 1)
      else
         Result := Rect(0, 0, gWidth - 1, gHeidht - 1);
   end

   Canvas.Copyrect(Client, TPixmap(FOriginal).Canvas, Result);
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

