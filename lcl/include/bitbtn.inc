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
  FreeThenNil(FCanvas);
  FreeThenNil(FGlyph);
  inherited Destroy;
end;

Procedure TBitBtn.Click;
var
  Form : TCustomForm;
Begin
  if FKind = bkClose then Begin
    Form := GetParentForm(Self);
    if Form <> nil then begin
      Form.Close;
      exit;
    end;
  end;
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

procedure TBitBtn.ActionChange(Sender: TObject; CheckDefaults: Boolean);

  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clMaroon; // whatever
      Canvas.FillRect(Rect(0,0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index,true);
    end;
  end;

begin
  inherited ActionChange(Sender,CheckDefaults);
  if Sender is TCustomAction then begin
    with TCustomAction(Sender) do begin
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil)
      and (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images,ImageIndex);
    end;
  end;
end;

Procedure TBitBtn.SetKind(Value : TBitBtnKind);
var
  Bitmap1 : TBitmap;
Begin
  FKind := Value;

  if FKind = bkCustom then Exit;

  Bitmap1 := TBitmap.Create;
  Bitmap1.Handle := LoadStockPixmap(BitBtnImages[Value]);
  Glyph := Bitmap1;

  Caption :=GetCaptionOfKind(fKind);
  ModalResult := BitBtnModalResults[Value];
  if not (csLoading in ComponentState) then
    Default := FKind in [bkOk,bkYes];
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

//Return the caption associed with the akind value.
//This function replace BitBtnCaption const because the localizing
//dont work with an const (optimisation of FPC i supose)
function TBitBtn.GetCaptionOfKind(aKind: TBitBtnKind): String;
begin
  Result:='';
  Case aKind of
    bkOK       : Result:=rsmbOK;
    bkCancel   : Result:=rsmbCancel;
    bkHelp     : Result:=rsmbHelp;
    bkYes      : Result:=rsmbYes;
    bkNo       : Result:=rsmbNo;
    bkClose    : Result:=rsmbClose;
    bkAbort    : Result:=rsmbAbort;
    bkRetry    : Result:=rsmbRetry;
    bkIgnore   : Result:=rsmbIgnore;
    bkAll      : Result:=rsmbAll;
  end;
end;

// included by buttons.pp

