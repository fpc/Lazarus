{%MainUnit ../buttons.pp}

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
{       TBitBtn Constructor                                                    }
{------------------------------------------------------------------------------}
constructor TBitBtn.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle := csBitBtn;
  FKind := bkCustom;
  FLayout := blGlyphLeft;
  FSpacing := 3;
  fMargin:=-1;
  FButtonGlyph := TButtonGlyph.Create;
  FButtonGlyph.OnChange := @GlyphChanged;
  Align := alNone;
  SetInitialBounds(0,0,75,25);
  RealizeKind;
end;

{------------------------------------------------------------------------------}
{       TBitBtn destructor                                                     }
{------------------------------------------------------------------------------}
destructor TBitBtn.Destroy;
Begin
  FreeThenNil(FButtonGlyph);
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

Function TBitBtn.GetGlyph : TBitmap;
Begin
  Result := FButtonGlyph.Glyph;
end;

Function TBitBtn.IsGlyphStored: Boolean;
begin
  Result := (Kind = bkCustom) and (FButtonGlyph.Glyph <> nil)
            and (not FButtonGlyph.Glyph.Empty)
            and (FButtonGlyph.Glyph.Width>0) and (FButtonGlyph.Glyph.Height>0);
end;

Procedure TBitBtn.SetGlyph(AValue: TBitmap);
Begin
  FButtonGlyph.Glyph := AValue;
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
  var
    CurGlyph: TBitmap;
  begin
    CurGlyph:=Glyph;
    with CurGlyph do begin
      // ToDo: transparency
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clMaroon; // whatever
      Canvas.FillRect(Rect(0,0,Width, Height));
      ImageList.Draw(Canvas,0,0,Index,true);
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

Procedure TBitBtn.SetKind(AValue: TBitBtnKind);
Begin
  if FKind=AValue then exit;
  FKind := AValue;
  if FKind = bkCustom then Exit;
  
  RealizeKind;
end;

Procedure TBitBtn.SetLayout(AValue: TButtonLayout);
Begin
  if FLayout = AValue then Exit;
  FLayout := AValue;
  if HandleAllocated then
    CNSendMessage(LM_LAYOUTCHANGED,Self,nil);
end;

procedure TBitBtn.SetMargin(const AValue: integer);
begin
  if FMargin=AValue then exit;
  FMargin:=AValue;
  if HandleAllocated then
    //still send the layout message because it still calls the same procedure
    CNSendMessage(LM_LAYOUTCHANGED,Self,nil);
end;

Procedure TBitBtn.SetSpacing(AValue: Integer);
Begin
  if (FSpacing = AValue) or (AValue < 0) then Exit;
  FSpacing := AValue;
  if HandleAllocated then
    //still send the layout message because it still calls the same procedure
    CNSendMessage(LM_LAYOUTCHANGED,Self,nil);
end;

procedure TBitBtn.RealizeKind;
var
  ABitmap: TBitmap;
begin
  if (Kind<>bkCustom) then begin
    ABitmap:=Glyph;
    if ABitmap=nil then
      ABitmap := TBitmap.Create;
    ABitmap.Handle := LoadStockPixmap(BitBtnImages[FKind]);
    Glyph := ABitmap;
  end;

  if not (csLoading in ComponentState) then begin
    Caption :=GetCaptionOfKind(fKind);
    ModalResult := BitBtnModalResults[FKind];
    Default := FKind in [bkOk,bkYes];
  end;
end;

{ Return the caption associated with the akind value.
  This function replaces BitBtnCaption const because the localizing
  dont work with an const array }
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

