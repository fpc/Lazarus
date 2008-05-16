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

{------------------------------------------------------------------------------
       TCustomBitBtn Constructor
------------------------------------------------------------------------------}
constructor TCustomBitBtn.Create(TheOwner: TComponent);
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
  RealizeKind;
end;

{------------------------------------------------------------------------------
       TCustomBitBtn destructor
------------------------------------------------------------------------------}
destructor TCustomBitBtn.Destroy;
Begin
  FreeThenNil(FButtonGlyph);
  inherited Destroy;
end;

procedure TCustomBitBtn.Click;
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

function TCustomBitBtn.GetGlyph : TBitmap;
Begin
  Result := FButtonGlyph.Glyph;
end;

function TCustomBitBtn.GetNumGlyphs: Integer;
begin
  Result := FButtonGlyph.FNumGlyphs;
end;

function TCustomBitBtn.IsGlyphStored: Boolean;
begin
  Result := (Kind = bkCustom) and (FButtonGlyph.Glyph <> nil)
            and (not FButtonGlyph.Glyph.Empty)
            and (FButtonGlyph.Glyph.Width>0) and (FButtonGlyph.Glyph.Height>0);
end;

procedure TCustomBitBtn.SetGlyph(AValue: TBitmap);
Begin
  FButtonGlyph.Glyph := AValue;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBitBtn.GlyphChanged(Sender: TObject);
begin
  if HandleAllocated 
  then begin
    TWSBitBtnClass(WidgetSetClass).SetGlyph(Self, FButtonGlyph);
  end;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBitBtn.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender,CheckDefaults);
  if Sender is TCustomAction then begin
    with TCustomAction(Sender) do begin
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil)
      and (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        ActionList.Images.GetBitmap(ImageIndex, Glyph);
    end;
  end;
end;

procedure TCustomBitBtn.SetKind(AValue: TBitBtnKind);
Begin
  if FKind = AValue then Exit;
  FKind := AValue;
  if FKind = bkCustom then Exit;
  
  RealizeKind;
end;

procedure TCustomBitBtn.SetLayout(AValue: TButtonLayout);
Begin
  if FLayout = AValue then Exit;
  FLayout := AValue;
  if HandleAllocated 
  then TWSBitBtnClass(WidgetSetClass).SetLayout(Self, FLayout);
  AdjustSize;
end;

procedure TCustomBitBtn.SetMargin(const AValue: integer);
begin
  if FMargin = AValue then Exit;
  FMargin := AValue;
  if HandleAllocated 
  then TWSBitBtnClass(WidgetSetClass).SetMargin(Self, FMargin);
  AdjustSize;
end;

procedure TCustomBitBtn.SetNumGlyphs(AValue: Integer);
begin
  if AValue < 0 then AValue := 1;
  if AValue > High(TNumGlyphs) then AValue := High(TNumGlyphs);

  if AValue <> FButtonGlyph.NumGlyphs then
  Begin
    FButtonGlyph.NumGlyphs := TNumGlyphs(AValue);
    Invalidate;
  end;
end;

procedure TCustomBitBtn.SetSpacing(AValue: Integer);
Begin
  if (FSpacing = AValue) or (AValue < -1) then Exit;
  FSpacing := AValue;
  if HandleAllocated 
  then TWSBitBtnClass(WidgetSetClass).SetSpacing(Self, FSpacing);
  AdjustSize;
end;

procedure TCustomBitBtn.RealizeKind;
var
  GlyphValid, Handled: Boolean;
  CustomGlyph: TGraphic;
  BitmapHandle, MaskHandle: HBitmap;
begin
  if (Kind<>bkCustom) then
  begin
    GlyphValid:=false;
    
    // first let the user override
    if GetDefaultBitBtnGlyph <> nil then
    begin
      Handled := False;
      CustomGlyph := GetDefaultBitBtnGlyph(Kind, Handled);
      if Handled then
      begin
        Glyph.Assign(CustomGlyph);
        CustomGlyph.Free;
        GlyphValid := True;
      end;
    end;

    // then ask the widgetset
    if not GlyphValid then 
    begin
      BitmapHandle := LoadStockPixmap(BitBtnImages[FKind], MaskHandle);
      if BitmapHandle <> 0 then
      begin
        Glyph.Handle := BitmapHandle;
        Glyph.MaskHandle := MaskHandle;
        GlyphValid := true;
      end;
    end;
    
    if not GlyphValid then
    begin
      CustomGlyph := GetLCLDefaultBtnGlyph(Kind);
      if CustomGlyph <> nil then
      begin
        Glyph.Assign(CustomGlyph);
        CustomGlyph.Free;
        GlyphValid := True;
      end;
    end;
  end;

  if not (csLoading in ComponentState) then
  begin
    Caption := GetCaptionOfKind(fKind);
    ModalResult := BitBtnModalResults[FKind];
    Default := FKind in [bkOk,bkYes];
    Cancel := FKind in [bkCancel,bkNo];
  end;
end;

{ Return the caption associated with the akind value.
  This function replaces BitBtnCaption const because the localizing
  dont work with an const array }
function TCustomBitBtn.GetCaptionOfKind(aKind: TBitBtnKind): String;
begin
  Result:='';
  case aKind of
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
    bkNoToAll  : Result:=rsmbNoToAll;
    bkYesToAll : Result:=rsmbYesToAll;
  end;
end;

procedure TCustomBitBtn.InitializeWnd;
begin
  inherited InitializeWnd;
  TWSBitBtnClass(WidgetSetClass).SetGlyph(Self, FButtonGlyph);
  TWSBitBtnClass(WidgetSetClass).SetLayout(Self, FLayout);
  TWSBitBtnClass(WidgetSetClass).SetMargin(Self, FMargin);  
  TWSBitBtnClass(WidgetSetClass).SetSpacing(Self, FSpacing);  
end;

procedure TCustomBitBtn.TextChanged;
begin
  inherited TextChanged;
  AdjustSize;
end;

class function TCustomBitBtn.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=75;
  Result.Y:=30;
end;

// included by buttons.pp
