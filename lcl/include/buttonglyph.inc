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

Procedure TButtonGlyph.SetGlyph(Value : TBitmap);
Begin
if FOriginal = Value then exit;
//Invalidate;
FOriginal := Value;
//FOriginal.Assign(Value);
end;

Function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; Transparent: Boolean; BiDiFlags: Longint): TRect;
Begin

if State= bsDisabled then
   Begin
    if (TPixMap(FOriginal).Width > 25) then
     Canvas.Copyrect(Client, TPixmap(FOriginal).Canvas, Rect(25, 0, 50, 25))
     else
     Canvas.Copyrect(Client, TPixmap(FOriginal).Canvas, Rect(0, 0, 25, 25));

   end
   else
     Canvas.Copyrect(Client, TPixmap(FOriginal).Canvas, Rect(0, 0, 25, 25));
end;
