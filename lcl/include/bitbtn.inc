{%MainUnit ../buttons.pp}

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  FDefaultCaption := False;
  FKind := bkCustom;
  FLayout := blGlyphLeft;
  FSpacing := 3;
  FMargin := -1;
  FButtonGlyph := TButtonGlyph.Create;
  FButtonGlyph.NumGlyphs := 1;
  FButtonGlyph.OnChange := @GlyphChanged;
  FButtonGlyph.IsDesigning := csDesigning in ComponentState;
  Align := alNone;
  RealizeKind;
end;

{------------------------------------------------------------------------------
       TCustomBitBtn destructor
------------------------------------------------------------------------------}
destructor TCustomBitBtn.Destroy;
begin
  FreeThenNil(FButtonGlyph);
  inherited Destroy;
end;

procedure TCustomBitBtn.Click;
var
  Form : TCustomForm;
begin
  if FKind = bkClose then begin
    Form := GetParentForm(Self);
    if Form <> nil then begin
      Form.Close;
      exit;
    end;
  end;
  inherited Click;
end;

procedure TCustomBitBtn.LoadGlyphFromLazarusResource(const AName: String);
begin
  Buttons.LoadGlyphFromLazarusResource(FButtonGlyph, AName);
end;

procedure TCustomBitBtn.LoadGlyphFromStock(idButton: Integer);
begin
  Buttons.LoadGlyphFromStock(FButtonGlyph, idButton);
end;

function TCustomBitBtn.CanShowGlyph: Boolean;
begin
  Result := not Glyph.Empty and (FButtonGlyph.Images <> nil);
end;

function TCustomBitBtn.GetGlyph : TBitmap;
begin
  Result := FButtonGlyph.Glyph;
end;

function TCustomBitBtn.GetGlyphShowMode: TGlyphShowMode;
begin
  Result := FButtonGlyph.ShowMode;
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
begin
  FButtonGlyph.Glyph := AValue;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBitBtn.SetGlyphShowMode(const AValue: TGlyphShowMode);
begin
  FButtonGlyph.ShowMode := AValue;
end;

procedure TCustomBitBtn.GlyphChanged(Sender: TObject);
begin
  if HandleAllocated then
    TWSBitBtnClass(WidgetSetClass).SetGlyph(Self, FButtonGlyph);
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBitBtn.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender,CheckDefaults);
  if Sender is TCustomAction then
  begin
    with TCustomAction(Sender) do
    begin
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        ActionList.Images.GetBitmap(ImageIndex, Glyph);
    end;
  end;
end;

procedure TCustomBitBtn.SetKind(AValue: TBitBtnKind);
begin
  if FKind = AValue then Exit;
  FKind := AValue;
  if FKind <> bkCustom then
    RealizeKind;
  if not (csLoading in ComponentState) then
    DefaultCaption := FKind <> bkCustom;
end;

procedure TCustomBitBtn.SetLayout(AValue: TButtonLayout);
begin
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
begin
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
  if (Kind <> bkCustom) then
  begin
    GlyphValid := False;
    
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
      if ThemeServices.GetStockImage(BitBtnImages[Kind], BitmapHandle, MaskHandle) then
      begin
        Glyph.Handle := BitmapHandle;
        Glyph.MaskHandle := MaskHandle;
        GlyphValid := True;
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
    Caption := GetCaptionOfKind(Kind);
    ModalResult := BitBtnModalResults[Kind];
    Default := Kind in [bkOk, bkYes];
    Cancel := Kind in [bkCancel, bkNo];
  end;
end;

{ Return the caption associated with the akind value.
  This function replaces BitBtnCaption const because the localizing
  do not work with an const array }
function TCustomBitBtn.GetCaptionOfKind(AKind: TBitBtnKind): String;
begin
  Result := GetButtonCaption(BitBtnImages[Kind]);
  if Result = '?' then
    Result := '';
end;

class procedure TCustomBitBtn.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomBitBtn;
end;

procedure TCustomBitBtn.InitializeWnd;
begin
  inherited InitializeWnd;
  TWSBitBtnClass(WidgetSetClass).SetGlyph(Self, FButtonGlyph);
  TWSBitBtnClass(WidgetSetClass).SetLayout(Self, FLayout);
  TWSBitBtnClass(WidgetSetClass).SetMargin(Self, FMargin);  
  TWSBitBtnClass(WidgetSetClass).SetSpacing(Self, FSpacing);  
end;

function TCustomBitBtn.IsCaptionStored: Boolean;
begin
  Result := inherited IsCaptionStored and not DefaultCaption;
end;

procedure TCustomBitBtn.Loaded;
begin
  inherited Loaded;
  if (Kind <> bkCustom) and DefaultCaption and (Caption = '') then
  begin
    Caption := GetCaptionOfKind(Kind); // Will trigger TextChanged
    DefaultCaption := True;
  end;
end;

procedure TCustomBitBtn.TextChanged;
begin
  inherited TextChanged;
  AdjustSize;
  DefaultCaption := False;
end;

class function TCustomBitBtn.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 75;
  Result.CY := 30;
end;

procedure TCustomBitBtn.CMAppShowBtnGlyphChanged(var Message: TLMessage);
begin
  if GlyphShowMode = gsmApplication then
    FButtonGlyph.Refresh;
end;

// included by buttons.pp
