{%MainUnit ../buttons.pp}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
  FSpacing := 4;
  FMargin := -1;
  FButtonGlyph := TButtonGlyph.Create;
  FButtonGlyph.NumGlyphs := 1;
  FButtonGlyph.OnChange := @GlyphChanged;
  FButtonGlyph.IsDesigning := csDesigning in ComponentState;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  Align := alNone;
end;

{------------------------------------------------------------------------------
       TCustomBitBtn destructor
------------------------------------------------------------------------------}
destructor TCustomBitBtn.Destroy;
begin
  FreeThenNil(FButtonGlyph);
  FreeAndNil(FImageChangeLink);
  inherited Destroy;
end;

procedure TCustomBitBtn.Click;
var
  Form : TCustomForm;
begin
  { A TBitBtn with Kind = bkClose should
    - Close the ParentForm if ModalResult = mrNone.
      It should not set ParentForm.ModalResult in this case
    - Close a non-modal ParentForm if ModalResult in [mrNone, mrClose]
    - on nested forms it will close the non docked form
    - In all other cases it should behave like any other TBitBtn
  }
  if (FKind = bkClose) then
  begin
    Form := GetTopFormSkipNonDocked(Self);
    if (Form <> nil) then
    begin
      if (ModalResult = mrNone) or
         ((ModalResult = mrClose) and not (fsModal in Form.FormState)) then
      begin
        Form.Close;
        Exit;
      end;
    end;
  end;
  inherited Click;
end;

procedure TCustomBitBtn.LoadGlyphFromResourceName(Instance: THandle; const AName: String);
begin
  Buttons.LoadGlyphFromResourceName(FButtonGlyph, Instance, AName);
end;

procedure TCustomBitBtn.LoadGlyphFromLazarusResource(const AName: String);
begin
  Buttons.LoadGlyphFromLazarusResource(FButtonGlyph, AName);
end;

procedure TCustomBitBtn.LoadGlyphFromStock(idButton: Integer);
begin
  Buttons.LoadGlyphFromStock(FButtonGlyph, idButton);
end;

procedure TCustomBitBtn.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FButtonGlyph <> nil) and (AComponent = FButtonGlyph.ExternalImages) then
    Images := nil;
end;

function TCustomBitBtn.CanShowGlyph(const AWithShowMode: Boolean): Boolean;
begin
  Result := FButtonGlyph.CanShowGlyph;
  if AWithShowMode then
    Result := Result and FButtonGlyph.CanShow;
end;

function TCustomBitBtn.GetGlyph : TBitmap;
begin
  Result := FButtonGlyph.Glyph;
end;

function TCustomBitBtn.GetGlyphShowMode: TGlyphShowMode;
begin
  Result := FButtonGlyph.ShowMode;
end;

function TCustomBitBtn.GetImageIndex: TImageIndex;
begin
  Result := FButtonGlyph.ExternalImageIndex;
end;

function TCustomBitBtn.GetImages: TCustomImageList;
begin
  Result := FButtonGlyph.ExternalImages;
end;

function TCustomBitBtn.GetImageWidth: Integer;
begin
  Result := FButtonGlyph.ExternalImageWidth;
end;

function TCustomBitBtn.GetNumGlyphs: Integer;
begin
  Result := FButtonGlyph.FNumGlyphs;
end;

function TCustomBitBtn.IsGlyphStored: Boolean;
var
  act: TCustomAction;
begin
  if Action <> nil then
  begin
    result := true;
    act := TCustomAction(Action);
    if (act.ActionList <> nil) and (act.ActionList.Images <> nil) and
      (act.ImageIndex >= 0) and (act.ImageIndex < act.ActionList.Images.Count) then
        result := false;
  end 
  else Result := (Kind = bkCustom) and (FButtonGlyph.Glyph <> nil)
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

procedure TCustomBitBtn.SetImageIndex(const aImageIndex: TImageIndex);
begin
  FButtonGlyph.ExternalImageIndex := aImageIndex;
end;

procedure TCustomBitBtn.SetImages(const aImages: TCustomImageList);
begin
  if FButtonGlyph.ExternalImages <> nil then
  begin
    FButtonGlyph.ExternalImages.UnRegisterChanges(FImageChangeLink);
    FButtonGlyph.ExternalImages.RemoveFreeNotification(Self);
  end;
  FButtonGlyph.ExternalImages := aImages;
  if FButtonGlyph.ExternalImages <> nil then
  begin
    FButtonGlyph.ExternalImages.FreeNotification(Self);
    FButtonGlyph.ExternalImages.RegisterChanges(FImageChangeLink);
  end;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBitBtn.SetImageWidth(const aImageWidth: Integer);
begin
  FButtonGlyph.ExternalImageWidth := aImageWidth;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBitBtn.GlyphChanged(Sender: TObject);
begin
  if HandleAllocated then
    TWSBitBtnClass(WidgetSetClass).SetGlyph(Self, FButtonGlyph);
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBitBtn.ImageListChange(Sender: TObject);
begin
  if Sender = Images then
    GlyphChanged(Sender);
end;

procedure TCustomBitBtn.ActionChange(Sender: TObject; CheckDefaults: Boolean);
var
  NewAct: TCustomAction;
  Imgs: TCustomImageList;
  ImgRes: TScaledImageListResolution;
begin
  inherited ActionChange(Sender,CheckDefaults);
  if Sender is TCustomAction then
  begin
    NewAct := TCustomAction(Sender);
    //DebugLn(['TCustomBitBtn.ActionChange: Glyph.Empty=', Glyph.Empty,
    //         ', Action=', NewAct.Caption,
    //         ', ActionList=', NewAct.ActionList,
    //         ', Images=', NewAct.ActionList.Images,
    //         ', ImageIndex=', NewAct.ImageIndex ]);
    if (NewAct.ActionList = nil) or (NewAct.ImageIndex < 0) then Exit;
    Imgs := NewAct.ActionList.Images;
    if (Imgs = nil) or (NewAct.ImageIndex >= Imgs.Count) then Exit;
    //DebugLn([' TCustomBitBtn.ActionChange: Setting image, ImageWidth=', ImageWidth]);
    ImgRes := Imgs.ResolutionForPPI[ImageWidth,Font.PixelsPerInch,GetCanvasScaleFactor];
    ImgRes.GetBitmap(NewAct.ImageIndex, Glyph);
  end;
end;

procedure TCustomBitBtn.SetKind(AValue: TBitBtnKind);
begin
  if FKind = AValue then Exit;
  FKind := AValue;
  if (FKind <> bkCustom) and not (csLoading in ComponentState) then
    RealizeKind(True);
  if not (csLoading in ComponentState) then
    FDefaultCaption := FKind <> bkCustom;
end;

procedure TCustomBitBtn.SetLayout(AValue: TButtonLayout);
begin
  if FLayout = AValue then Exit;
  FLayout := AValue;
  if HandleAllocated then
  begin
    TWSBitBtnClass(WidgetSetClass).SetLayout(Self, FLayout);
    InvalidatePreferredSize;
  end;
  AdjustSize;
end;

procedure TCustomBitBtn.SetMargin(const AValue: integer);
begin
  if FMargin = AValue then Exit;
  FMargin := AValue;
  if HandleAllocated  then
    TWSBitBtnClass(WidgetSetClass).SetMargin(Self, FMargin);
  AdjustSize;
  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TCustomBitBtn.SetNumGlyphs(AValue: Integer);
begin
  if AValue < Low(TNumGlyphs) then AValue := Low(TNumGlyphs);
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
  if HandleAllocated then
    TWSBitBtnClass(WidgetSetClass).SetSpacing(Self, FSpacing);
  AdjustSize;
  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TCustomBitBtn.RealizeKind(ForceDefaults: Boolean);
var
  GlyphValid, Handled: Boolean;
  CustomGlyph: TGraphic;
  BitmapHandle, MaskHandle: HBitmap;
  idButton: LongInt;
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
      if ForceDefaults or (Images=nil) then
      begin
        idButton := BitBtnImages[Kind];
        if (idButton >= Low(BitBtnResNames)) and (idButton <= High(BitBtnResNames))
        and (BitBtnResNames[idButton] <> '') then
        begin
          FButtonGlyph.LCLGlyphName := BitBtnResNames[idButton];
          CustomGlyph := GetLCLDefaultBtnGlyph(Kind);
          try
            if not Assigned(Glyph) then
              Glyph := TBitmap.Create;
            Glyph.Assign(CustomGlyph);
          finally
            CustomGlyph.Free;
          end;
        end
        else
          ImageIndex := -1;
        GlyphValid := True;
      end;
    end;
  end;

  if ForceDefaults then
  begin
    Caption := GetCaptionOfKind(Kind);
    ModalResult := BitBtnModalResults[Kind];
    Default := Kind in [bkOk, bkYes];
    Cancel := Kind in [bkCancel, bkNo];
  end;
end;

procedure TCustomBitBtn.SetDefaultCaption(const AValue: Boolean);
begin
  if FDefaultCaption = AValue then Exit;
  FDefaultCaption := AValue;
  if (Kind <> bkCustom) and DefaultCaption then
  begin
    Caption := GetCaptionOfKind(FKind); // will trigger TextChanged
    FDefaultCaption := True;
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
  if (Kind <> bkCustom) then
    RealizeKind(False);
  if (Kind <> bkCustom) and DefaultCaption and (Caption = '') then
  begin
    Caption := GetCaptionOfKind(Kind); // Will trigger TextChanged
    FDefaultCaption := True;
  end;
end;

procedure TCustomBitBtn.TextChanged;
begin
  inherited TextChanged;
  AdjustSize;
  FDefaultCaption := False;
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
