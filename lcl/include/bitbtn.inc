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
{       TBitbtn Constructor                                                    }
{------------------------------------------------------------------------------}
constructor TBitBtn.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FCompStyle := csBitBtn;
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := @GlyphChanged;
  {set default alignment}
  Align := alNone;
  FCanvas := TCanvas.Create;
  FKind := bkCustom;
  FLayout := blGlyphLeft;
  FSpacing := 3;
  Setbounds(1,1,75,25);
end;

{------------------------------------------------------------------------------}
{       TBitbtn destructor                                                    }
{------------------------------------------------------------------------------}
destructor TBitbtn.Destroy;
Begin
  FCanvas.Free;
  FGlyph.Free;
  inherited Destroy;
end;

Procedure TBitBtn.Click;
var
  Form : TCustomForm;
Begin
  if FKind = bkClose then Begin
    Form := GetParentForm(Self);
    if Form <> nil then Form.Close
    else
      inherited click;
  end
  else
    inherited Click;
End;

Function TBitbtn.GetGlyph : TBitmap;
Begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;

Function TBitBtn.IsCustom : Boolean;
Begin
  Result := Kind = bkCustom;
end;

Function TBitbtn.IsGlyphStored : Boolean;
begin
  Result := IsCustom;
  If Result then
    Result := TButtonGlyph(FGlyph).Glyph <> nil;
  If Result then
    Result := not TButtonGlyph(FGlyph).Glyph.Empty;
end;

Procedure TBitbtn.SetGlyph(Value : TBitmap);
Begin
  Assert(False, 'Trace:SETGLYPH');
  TButtonGlyph(FGlyph).Glyph := Value;
end;

procedure TBitBtn.GlyphChanged(Sender: TObject);
begin
  if HandleAllocated then begin
    CNSendMessage(LM_IMAGECHANGED,Self,nil);
    Invalidate;
  end;
end;

Procedure TBitBtn.SetKind(Value : TBitBtnKind);
var
  Bitmap1 : TBitmap;
Begin
//  HandleNeeded;
  FKind := Value;
  Assert(False, 'Trace:SETKIND');

  if FKind = bkCustom then Exit;

  //if Glyph <> nil then Glyph.free;
  Bitmap1 := TBitmap.Create;
  Bitmap1.Handle := LoadStockPixmap(BitBtnImages[Value]);

  Caption := BitBtnCaption[fKind];

  Glyph := Bitmap1;
  ModalResult := BitBtnModalResults[Value];

  if (FKind = bkOK) or (FKind = bkYes) then
    Default := True
  else
    Default := False;
end;

Procedure TBitBtn.SetLayout(Value : TButtonLayout);
Begin
  if FLayout = Value then Exit;
  FLayout := Value;
  if HandleAllocated then
    CNSendMessage(LM_LAYOUTCHANGED,Self,nil);
end;


Procedure TBitBtn.SetSpacing(Value : Integer);
Begin
  if (FSpacing = Value) or (Value < 0) then Exit;
  FSpacing := Value;
  if HandleAllocated then
    //still send the layout message because it still calls the same procedure
    CNSendMessage(LM_LAYOUTCHANGED,Self,nil);
end;



