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
W,H : Integer;
Begin
  if NumGlyphs > 1 then
  Begin
    W := TPixMap(FOriginal).Width;
    W := W div NumGlyphs;

    H := TPixMap(FOriginal).Height;
    if (State=bsDown) and (NumGlyphs < 3) then State := bsUp;

    if State= bsDisabled then
      Result:=Rect(W,0,(2*W)-1,H-1)
      //Canvas.Copyrect(Client, TPixmap(FOriginal).Canvas, Rect(W, 0, (2*W)-1, H-1))
    else if State=bsDown then
      Result:=Rect(2*W,0,(3*W)-1, H-1)
      //Canvas.Copyrect(Client, TPixmap(FOriginal).Canvas, Rect(2*W, 0, (3*W)-1, H-1))
    else
      Result:=Rect(0, 0, w-1, H-1);
      //Canvas.Copyrect(Client, TPixmap(FOriginal).Canvas, Rect(0, 0, w-1, H-1));
  end
  else
    Result:=Rect(0, 0, 25, 25);
    //Canvas.Copyrect(Client, TPixmap(FOriginal).Canvas, Rect(0, 0, 25, 25));
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

