{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TCoolBar
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

}

{ TCoolBand }

constructor TCoolBand.Create(aCollection: TCollection);
begin
  FBreak := True;
  FColor := clDefault;
  FControl := Nil;
  FFixedBackground := True;
  FImageIndex := -1;
  FMinHeight := cDefMinHeight;
  FMinWidth := cDefMinWidth;
  FParentBitmap := True;
  FParentColor := True;
  FVisible := True;
  FWidth := cDefWidth;
  inherited Create(aCollection);
  Assert(aCollection is TCoolBands, 'TCoolBand.Create: aCollection is not TCoolBands');
  FCoolBar := TCoolBands(aCollection).FCoolBar;
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := @InvalidateCoolBar;
end;

destructor TCoolBand.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TCoolBand.Assign(aSource: TPersistent);
var src: TCoolBand;
    SrcCtrl: TWinControl;
begin
  if aSource is TCoolBand then begin
    src := TCoolBand(aSource);
    Bitmap          := src.Bitmap;
    Break           := src.Break;
    Color           := src.Color;
    FixedBackground := src.FixedBackground;
    FixedSize       := src.FixedSize;
    HorizontalOnly  := src.HorizontalOnly;
    ImageIndex      := src.ImageIndex;
    MinHeight       := src.MinHeight;
    MinWidth        := src.MinWidth;
    ParentBitmap    := src.ParentBitmap;
    ParentColor     := src.ParentColor;
    Text            := src.Text;
    Visible         := src.Visible;
    SrcCtrl := Nil;
    if Assigned(src.Control) then
      SrcCtrl := FCoolBar.Owner.FindComponent(src.Control.Name) as TWinControl;
    Control         := SrcCtrl;
  end else
    inherited Assign(aSource);
end;

procedure TCoolBand.AutosizeWidth;
var h, w: Integer;
begin
  if Assigned(FControl) and FControl.AutoSize then begin
    FControl.GetPreferredSize(w, h);
    if FCoolBar.Vertical then w := h;
    inc(w, CalcControlLeft+FCoolBar.HorizontalSpacing+cDivider);
    Width := Math.max(FMinWidth, w);
  end;
end;

function TCoolBand.CalcControlLeft: Integer;
var aImageSize, xHelp: Integer;
begin
  Result := cGrabIndent+FCoolBar.GrabWidth+FCoolBar.HorizontalSpacing;
  xHelp := Result;
  if (Text <> '') and FCoolBar.ShowText then
    inc(Result, FTextWidth+FCoolBar.HorizontalSpacing);
  if Assigned(FCoolBar.Images) then begin
    if not FCoolBar.Vertical then
      aImageSize := FCoolBar.Images.WidthForPPI[FCoolBar.ImagesWidth, FCoolBar.Font.PixelsPerInch]
    else
      aImageSize := FCoolBar.Images.HeightForPPI[FCoolBar.ImagesWidth, FCoolBar.Font.PixelsPerInch];
    if ImageIndex >= 0 then
      inc(Result, aImageSize+FCoolBar.HorizontalSpacing);
  end;
  if Result = xHelp then inc(Result, FCoolBar.HorizontalSpacing);
end;

function TCoolBand.CalcPreferredHeight: Integer;
begin
  Result := FMinHeight;
  if not FCoolBar.Vertical then begin
    if Assigned(FControl) then
      Result := max(Result, FControl.Height+2*FCoolBar.VerticalSpacing);
    if Assigned(FCoolBar.Images) and (ImageIndex >= 0) then
      Result := max(Result, FCoolBar.Images.HeightForPPI[FCoolBar.ImagesWidth, FCoolBar.Font.PixelsPerInch]+2*FCoolBar.VerticalSpacing);
  end else begin
    if Assigned(FControl) then
      Result := max(Result, FControl.Width+2*FCoolBar.VerticalSpacing);
    if Assigned(FCoolBar.Images) and (ImageIndex >= 0) then
      Result := max(Result, FCoolBar.Images.WidthForPPI[FCoolBar.ImagesWidth, FCoolBar.Font.PixelsPerInch]+2*FCoolBar.VerticalSpacing);
  end;
  if FCoolBar.FShowText then
    Result := max(Result, FCoolBar.FTextHeight+2*FCoolBar.VerticalSpacing);
  //DebugLn('CalcPreferredHeight ', CalcPreferredHeightHor);
end;

function TCoolBand.CalcPreferredWidth: Integer;
begin
  Result := CalcControlLeft;
  if Assigned(Control) then inc(Result, Control.Width+FCoolBar.HorizontalSpacing);
  inc(Result, cDivider);
  Result := max(FMinWidth, Result);
end;

procedure TCoolBand.CalcTextWidth;
begin
  if Assigned(FCoolBar) and not (csLoading in FCoolBar.ComponentState) then
    FTextWidth := FCoolBar.Canvas.TextWidth(FText);
end;

function TCoolBand.GetDisplayName: string;
begin
  Result := Text;
  if Result = '' then Result := ClassName;
end;

function TCoolBand.GetRight: Integer;
begin
  Result := FLeft+FWidth;
end;

function TCoolBand.IsBitmapStored: Boolean;
begin
  Result := not ParentBitmap;
end;

function TCoolBand.IsColorStored: Boolean;
begin
  Result := not ParentColor;
end;

procedure TCoolBand.InvalidateCoolBar(Sender: TObject);
begin
  Changed(False);
end;

function TCoolBand.GetVisible: Boolean;
begin
  Result := FVisible and not (FCoolBar.Vertical and FHorizontalOnly);
end;

procedure TCoolBand.SetBitmap(AValue: TBitmap);
begin
  FParentBitmap := False;
  FBitmap.Assign(AValue);
  Changed(False);
end;

procedure TCoolBand.SetBorderStyle(AValue: TBorderStyle);
begin
  if FBorderStyle = AValue then Exit;
  FBorderStyle := AValue;
  Changed(False);
end;

procedure TCoolBand.SetBreak(AValue: Boolean);
begin
  if FBreak = AValue then Exit;
  FBreak := AValue;
  Changed(True);
end;

procedure TCoolBand.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
  FParentColor := False;
  Changed(False);
end;

procedure TCoolBand.SetControl(AValue: TControl);
var aBand: TCoolBand;
begin
  if FControl = AValue then Exit;
  FControl := AValue;
  if Assigned(AValue) then begin
    AValue.Align := alNone;
    aBand := TCoolBands(Collection).FindBand(AValue);
    if Assigned(aBand) and (aBand <> Self) then aBand.SetControl(Nil);  //remove old association
    AValue.Parent := FCoolBar;
  end;
  Changed(True);
end;

procedure TCoolBand.SetFixedBackground(AValue: Boolean);
begin
  if FFixedBackground = AValue then Exit;
  FFixedBackground := AValue;
  Changed(False);
end;

procedure TCoolBand.SetHorizontalOnly(AValue: Boolean);
begin
  if FHorizontalOnly = AValue then Exit;
  FHorizontalOnly := AValue;
  Changed(FCoolBar.Vertical);
end;

procedure TCoolBand.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex = AValue then Exit;
  FImageIndex := AValue;
  Changed(True);
end;

procedure TCoolBand.SetMinHeight(AValue: Integer);
begin
  if FMinHeight = AValue then Exit;
  FMinHeight := AValue;
  Changed(False);
end;

procedure TCoolBand.SetMinWidth(AValue: Integer);
begin
  if FMinWidth = AValue then Exit;
  FMinWidth := AValue;
  Changed(False);
end;

procedure TCoolBand.SetParentBitmap(AValue: Boolean);
begin
  if FParentBitmap = AValue then Exit;
  FParentBitmap := AValue;
  Changed(False);
end;

procedure TCoolBand.SetParentColor(AValue: Boolean);
begin
  if FParentColor = AValue then Exit;
  FParentColor := AValue;
  Changed(False);
end;

procedure TCoolBand.SetText(const AValue: TTranslateString);
begin
  if AValue = FText then Exit;
  FText := AValue;
  CalcTextWidth;
  Changed(True);
end;

procedure TCoolBand.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then Exit;
  FVisible := AValue;
  if Assigned(FControl) then FControl.Visible := AValue;
  Changed(True);
end;

procedure TCoolBand.SetWidth(AValue: Integer);
begin
  if AValue = FWidth then Exit;
  if AValue < FMinWidth then AValue := FMinWidth;
  FWidth := AValue;
  Changed(True);
end;

{ TCoolBands }

constructor TCoolBands.Create(ACoolBar: TCustomCoolBar);
begin
  inherited Create(TCoolBand);
  FCoolBar := ACoolBar;
end;

function TCoolBands.Add: TCoolBand;
begin
  Result := TCoolBand(inherited Add);
  //DebugLn('TCoolBands.Add');
end;

function TCoolBands.FindBand(AControl: TControl): TCoolBand;
var i: Integer;
begin
  Result := Nil;
  for i := 0 to Count-1 do
    if GetItem(i).FControl = AControl then
      Exit(GetItem(i));
end;

function TCoolBands.FindBandindex(AControl: TControl): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if GetItem(i).FControl = AControl then
      Exit(i);
end;

procedure TCoolBands.Notify(aItem: TCollectionItem; aAction: TCollectionNotification);
begin
  inherited Notify(aItem, aAction);
  if aAction = cnAdded then begin
    //DebugLn('TCoolBands.Notify: aAction = cnAdded');
    TCoolBand(aItem).FCoolBar := FCoolBar;
  end;
end;

procedure TCoolBands.Update(aItem: TCollectionItem);
begin
  inherited Update(aItem);
  if Assigned(FCoolBar) then begin
    //DebugLn('Bands.Update calls CalcAndAlign');
    if not Assigned(aItem) then FCoolBar.CalculateAndAlign;
    FCoolBar.Invalidate;
  end;
end;

function TCoolBands.GetItem(Index: Integer): TCoolBand;
begin
  Result := TCoolBand(inherited GetItem(Index));
end;

function TCoolBands.GetOwner: TPersistent;
begin
  Result := FCoolBar;
end;

procedure TCoolBands.SetItem(Index: Integer; Value: TCoolBand);
begin
  inherited SetItem(Index, Value);
end;

{ TCustomCoolBar }

constructor TCustomCoolBar.Create(AOwner: TComponent);
begin
  FBands := TCoolBands.Create(Self);
  inherited Create(AOwner);
  ControlStyle := ControlStyle-[csSetCaption]
                +[csAcceptsControls, csNoFocus, csOpaque, csParentBackground, csReplicatable];
  Align := alTop;
  Height := 75;
  ParentColor := True;
  ParentFont := True;
  FBandBorderStyle := bsSingle;
  FBandMaximize := bmClick;
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := @BitmapOrImageListChange;
  FBorderEdges := EdgeBorders;
  FBorderLeft := 2;
  FBorderTop := 2;
  FBorderRight := 2;
  FBorderBottom := 2;
  FBorderWidth := 2;
  FGrabStyle := cDefGrabStyle;
  FGrabWidth := cDefGrabWidth;
  FHorizontalSpacing := cDefHorSpacing;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @BitmapOrImageListChange;
  FShowText := True;
  FThemed := True;
  FVerticalSpacing := cDefVertSpacing;
  UseDockManager := True;
end;

destructor TCustomCoolBar.Destroy;
begin
  FImageChangeLink.Free;
  FBitmap.Free;
  FBands.Free;
  inherited Destroy;
end;

function TCustomCoolBar.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

procedure TCustomCoolBar.SetAlign(aValue: TAlign);
var Old: TAlign;
begin
  Old := inherited Align;
  if aValue = Old then Exit;
  inherited Align := aValue;
  if csReading in ComponentState then Exit;
  Vertical := (aValue in [alLeft, alRight]);
end;

procedure TCustomCoolBar.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(Value);
  if Value then CalculateAndAlign;
  Invalidate;
end;

procedure TCustomCoolBar.SetBandBorderStyle(AValue: TBorderStyle);
begin
  if FBandBorderStyle = AValue then Exit;
  FBandBorderStyle := AValue;
  Invalidate;
end;

procedure TCustomCoolBar.SetBands(AValue: TCoolBands);
begin
  FBands.Assign(AValue);
end;

procedure TCustomCoolBar.SetBitmap(AValue: TBitmap);
begin
  FBitmap.Assign(AValue);
end;

procedure TCustomCoolBar.SetCursor(Value: TCursor);
begin
  inherited SetCursor(Value);
  if not FLockCursor then FCursorBkgnd:=Value;
end;

procedure TCustomCoolBar.SetGrabStyle(AValue: TGrabStyle);
begin
  if FGrabStyle = AValue then Exit;
  FGrabStyle := AValue;
  Invalidate;
end;

procedure TCustomCoolBar.SetGrabWidth(AValue: Integer);
begin
  if FGrabWidth = AValue then Exit;
  FGrabWidth := AValue;
  CalculateAndAlign;
  Invalidate;
end;

procedure TCustomCoolBar.SetHorizontalSpacing(AValue: Integer);
begin
  if FHorizontalSpacing=AValue then Exit;
  FHorizontalSpacing:=AValue;
  CalculateAndAlign;
  Invalidate;
end;

procedure TCustomCoolBar.SetImages(AValue: TCustomImageList);
begin
  if Assigned(FImages) then
    FImages.UnRegisterChanges(FImageChangeLink);
  FImages := AValue;
  if Assigned(FImages) then begin
    AValue.RegisterChanges(FImageChangeLink);
    AValue.FreeNotification(Self);
  end;
  CalculateAndAlign;
  Invalidate;
end;

procedure TCustomCoolBar.SetImagesWidth(const aImagesWidth: Integer);
begin
  if FImagesWidth = aImagesWidth then Exit;
  FImagesWidth := aImagesWidth;
  CalculateAndAlign;
  Invalidate;
end;

procedure TCustomCoolBar.SetShowText(AValue: Boolean);
begin
  if FShowText = AValue then Exit;
  FShowText := AValue;
  CalculateAndAlign;
  Invalidate;
end;

procedure TCustomCoolBar.SetThemed(AValue: Boolean);
begin
  if FThemed = AValue then Exit;
  FThemed := AValue;
  Invalidate;
end;


procedure TCustomCoolBar.SetVertical(AValue: Boolean);
var aRect: TRect;
begin
  if FVertical = aValue then Exit;
  FVertical := AValue;
  AdjustSize;
  CalculateAndAlign;
  Invalidate;
end;

procedure TCustomCoolBar.SetVerticalSpacing(AValue: Integer);
begin
  if FVerticalSpacing=AValue then Exit;
  FVerticalSpacing:=AValue;
  CalculateAndAlign;
  Invalidate;
end;

procedure TCustomCoolBar.AlignControls(AControl: TControl; var RemainingClientRect: TRect);
var aAnchor: TAnchorKind;
    i: Integer;
begin
  //DebugLn('AlignControls');
  if wcfAligningControls in FWinControlFlags then Exit;
  if not FRightToLeft then
    aAnchor := akLeft
  else
    aAnchor := akRight;
  for i := 0 to Bands.Count-1 do
    if Assigned(Bands[i].FControl) then begin
      Bands[i].Control.Align := alNone;
      Bands[i].FControl.BorderSpacing.Around := 0;
      Bands[i].FControl.Anchors := [akTop, aAnchor];
      if not Vertical then begin
        Bands[i].FControl.AnchorParallel(aAnchor, Bands[i].FControlLeft, Self);
        Bands[i].FControl.AnchorParallel(akTop, Bands[i].FControlTop, Self);
      end else begin
        Bands[i].FControl.AnchorParallel(akTop, Bands[i].FControlLeft, Self);
        Bands[i].FControl.AnchorParallel(aAnchor, Bands[i].FControlTop, Self);
      end;
    end;
  inherited AlignControls(AControl, RemainingClientRect);
end;

procedure TCustomCoolBar.AutosizeBands;
var i: Integer;
begin
  BeginUpdate;
  for i := 0 to Bands.Count-1 do
    Bands[i].AutosizeWidth;
  EndUpdate;
end;

procedure TCustomCoolBar.BitmapOrImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomCoolBar.CalculateAndAlign;
var i, x, y, aBandHeight, aBorderLeft, aCountM1, aLeft,
    aPrefSize, aStartIndex, aTop, aWidth, NewWidth,NewHeight: Integer;
    aRowEnd: Boolean;
begin
  //DebugLn('CalculateAndAlign');
  if (FUpdateCount > 0) or ([csLoading, csDestroying] * ComponentState <> []) then Exit;
  aCountM1 := FBands.Count-1;
  x := 0;
  for i := 0 to aCountM1 do
    if FBands[i].Visible then inc(x);
  SetLength(FVisiBands, x);
  x := 0;
  for i := 0 to aCountM1 do
    if FBands[i].Visible then begin
      FVisiBands[x] := FBands[i];
      inc(x);
    end;
  aCountM1 := x-1;
  if not Vertical then begin
    if not FRightToLeft then
      aBorderLeft := FBorderLeft
    else
      aBorderLeft := FBorderRight;
    aPrefSize := FBorderTop+FBorderBottom;
  end else begin
    aBorderLeft := FBorderTop;
    aPrefSize := FBorderLeft+FBorderRight-TCoolBand.cDivider;
  end;
  //do not use FBands from this point, only FVisiBands
  aBandHeight := 0;
  aStartIndex := 0;
  //set all Bands in row to uniform height
  aRowEnd := True;
  aLeft := aBorderLeft;
  for i := 0 to aCountM1 do begin
    if aRowEnd or FVisiBands[i].Break then
      aLeft := aBorderLeft;
    if FVisiBands[i].Control is TToolBar then
    begin
      if TToolBar(FVisiBands[i].Control).IsVertical then
        TToolBar(FVisiBands[i].Control).WrapButtons(Height,NewWidth,NewHeight,true)
      else
        TToolBar(FVisiBands[i].Control).WrapButtons(Width,NewWidth,NewHeight,true);
      FVisiBands[i].Control.Width:=NewWidth;
      FVisiBands[i].Control.Height:=NewHeight;
    end;
    aBandHeight := Max(aBandHeight, FVisiBands[i].CalcPreferredHeight);
    aRowEnd := (i = aCountM1);
    inc(aLeft, FVisiBands[i].Width);
    aRowEnd := aRowEnd or ((i < aCountM1) and RowEndHelper(ALeft, i));
    if aRowEnd then begin
      inc(aPrefSize, aBandHeight+TCoolBand.cDivider);
      for y := aStartIndex to i do
        FVisiBands[y].FHeight := aBandHeight;
      aBandHeight := 0;
      aStartIndex := i+1;
    end;
  end;
  if not Vertical then
    aTop := FBorderTop
  else begin
    if not FRightToLeft then
      aTop := FBorderLeft
    else begin
      aTop := FBorderRight;
      if not AutoSize then aPrefSize := Width;
    end;
  end;
  aRowEnd := True;
  include(FWinControlFlags, wcfAligningControls);
  for i := 0 to aCountM1 do begin
    if aRowEnd or FVisiBands[i].Break then
      aLeft := aBorderLeft;
    if not FRightToLeft or Vertical then
      FVisiBands[i].FLeft := aLeft
    else
      FVisiBands[i].FLeft := Width-aLeft-FVisiBands[i].Width;
    FVisiBands[i].FRealLeft := FVisiBands[i].FLeft;
    if not Vertical or not FRightToLeft then
      FVisiBands[i].FTop := aTop
    else
      FVisiBands[i].FTop := Width-aTop-FVisiBands[i].Height;
    if Assigned(FVisiBands[i].Control) then begin
      x := FVisiBands[i].CalcControlLeft;
      aWidth := FVisiBands[i].Width-x-HorizontalSpacing-TCoolBand.cDivider;
      if not FRightToLeft then begin
        inc(x, aLeft);
        if not Vertical then
          FVisiBands[i].Control.Left := x
        else
          FVisiBands[i].Control.Top := x;
        FVisiBands[i].FControlLeft := x-aBorderLeft;
      end else begin
        if not Vertical then begin
          x := FVisiBands[i].FLeft+TCoolBand.cDivider+HorizontalSpacing;
          FVisiBands[i].Control.Left := x;
          FVisiBands[i].FControlLeft := Width-x-Bands[i].FControl.Width-aBorderLeft;
        end else begin
          inc(x, aLeft);
          FVisiBands[i].Control.Top := x;
          FVisiBands[i].FControlLeft := x-aBorderLeft;
        end;
      end;
      if not Vertical then begin
        y := aTop+(FVisiBands[i].FHeight-FVisiBands[i].Control.Height) div 2;
        FVisiBands[i].FControlTop := y-FBorderTop;
        FVisiBands[i].Control.Top := FVisiBands[i].FControlTop+FBorderTop;
        FVisiBands[i].Control.Width := aWidth;
      end else begin
        y := aTop+(FVisiBands[i].FHeight-FVisiBands[i].Control.Width) div 2;
        if not FRightToLeft then begin
          FVisiBands[i].Control.Left := y;
          FVisiBands[i].FControlTop := y-FBorderLeft;
        end else begin
          FVisiBands[i].Control.Left := aPrefSize-y-FVisiBands[i].Control.Width;
          FVisiBands[i].FControlTop := y-FBorderRight;
        end;
        FVisiBands[i].Control.Height := aWidth;
      end;
    end;
    x := FVisiBands[i].Width;
    inc(aLeft, x);
    aRowEnd := IsRowEnd(aLeft, i);
    if aRowEnd or (i = aCountM1) then begin
      if not Vertical then begin
        FVisiBands[i].FRealWidth := x+Width-aLeft-FBorderRight;
        if FRightToLeft then FVisiBands[i].FRealLeft := FBorderLeft;
      end else
        FVisiBands[i].FRealWidth := x+Height-aLeft-FBorderBottom;
    end else
      FVisiBands[i].FRealWidth := x;
    if aRowEnd then inc(aTop, FVisiBands[i].FHeight+TCoolBand.cDivider);
  end;
  if AutoSize then begin
    if aCountM1 >= 0 then DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomCoolBar.CalculateAndAlign'){$ENDIF};
    inc(FUpdateCount);
    try
      InvalidatePreferredSize;
      AdjustSize;
    finally
      if aCountM1 >= 0 then EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomCoolBar.CalculateAndAlign'){$ENDIF};
      dec(FUpdateCount);
    end;
  end;
  exclude(FWinControlFlags, wcfAligningControls);
end;

procedure TCustomCoolBar.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var i, x, aCountM1: Integer;
begin
  aCountM1 := length(FVisiBands)-1;
  if not Vertical then begin
    if aCountM1 >= 0 then
      PreferredHeight := FVisiBands[aCountM1].FTop+FVisiBands[aCountM1].FHeight+FBorderBottom
    else
      PreferredHeight := FBorderTop+FBorderBottom;
    PreferredWidth := 0
  end else begin
    PreferredHeight := 0;
    if aCountM1 >= 0 then begin
      if not FRightToLeft then
        PreferredWidth := FVisiBands[aCountM1].FTop+FVisiBands[aCountM1].FHeight+FBorderRight
      else begin
        PreferredWidth := FBorderLeft+FVisiBands[0].FTop+FVisiBands[0].FHeight-FVisiBands[aCountM1].FTop+FBorderRight;
        x := FVisiBands[aCountM1].FTop-FBorderLeft;
        for i := 0 to aCountM1 do
          FVisiBands[i].FTop := FVisiBands[i].FTop-x;
      end;
    end else
      PreferredWidth := FBorderLeft+FBorderRight;
  end;
end;

function TCustomCoolBar.CalculateRealIndex(AVisibleIndex: Integer): Integer;
var i, aInvisibles, aVisibles: Integer;
begin
  aInvisibles := 0;
  aVisibles := 0;
  for i:=0 to FBands.Count-1 do begin
    if not FBands[i].Visible then
      inc(aInvisibles)
    else
      inc(aVisibles);
    if aVisibles > AVisibleIndex then break;
  end;
  Result := AVisibleIndex+aInvisibles;
end;

procedure TCustomCoolBar.ChangeCursor(ABand, AGrabber: Boolean);
begin
  FLockCursor := True;
  if ABand then begin
    if not AGrabber then
      Cursor := crDefault
    else
      if not Vertical then
        Cursor := crHSplit
      else
        Cursor := crVSplit;
  end else
    Cursor := FCursorBkgnd;
  FLockCursor := False;
end;

procedure TCustomCoolBar.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited CMBiDiModeChanged(Message);
  FRightToLeft := IsRightToLeft;
  CalculateAndAlign;
end;

procedure TCustomCoolBar.CreateWnd;
begin
  inherited CreateWnd;
  FCursorBkgnd := Cursor;
  DoFontChanged;
  CalculateAndAlign;
end;

procedure TCustomCoolBar.DoFontChanged;
var i: Integer;
begin
  if not Canvas.HandleAllocated then
    Exit;
  FTextHeight := Canvas.TextHeight('Žy|');
  for i := 0 to FBands.Count-1 do
    FBands[i].CalcTextWidth;
end;

procedure TCustomCoolBar.DrawTiledBitmap(ARect: TRect; ABitmap: TBitmap);
var i, j, x, y, aWidth, aHeight: Integer;
begin
  aWidth := ABitmap.Width;
  aHeight := ABitmap.Height;
  x := (ARect.Right-ARect.Left) div aWidth;
  y := (ARect.Bottom-ARect.Top) div aHeight;
  if ((ARect.Right-ARect.Left) mod aWidth) =0 then dec(x);
  if ((ARect.Bottom-ARect.Top) mod aHeight) =0 then dec(y);
  Canvas.Clipping := True;
  Canvas.ClipRect := ARect;
  for i := 0 to x do
    for j := 0 to y do
      Canvas.Draw(ARect.Left+i*aWidth, ARect.Top+j*aHeight, ABitmap);
  Canvas.Clipping := False;
end;

procedure TCustomCoolBar.EndUpdate;
begin
  inherited EndUpdate;
  //DebugLn('EndUpdate calls CalculateAndAlign');
  if FUpdateCount = 0 then begin
    DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomCoolBar.EndUpdate'){$ENDIF};
    try
      CalculateAndAlign;
    finally
      EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomCoolBar.EndUpdate'){$ENDIF};
    end;
    Invalidate;
  end;
end;

procedure TCustomCoolBar.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  DoFontChanged;
  //DebugLn('FontChanged calls CalculateAndAlign');
  CalculateAndAlign;
end;

procedure TCustomCoolBar.InsertControl(AControl: TControl; Index: integer);
var aBand: TCoolBand;
begin
  inherited InsertControl(AControl, Index);
  //DebugLn('TCustomCoolBar.InsertControl '+inttostr(FUpdateCount));
  if (AControl is TWinControl) and not (csLoading in ComponentState) then begin
    aBand := Bands.FindBand(AControl);
    if aBand = Nil then begin
      //DebugLn('TCoolBar.InsertControl: Adding band for Comp=' + AControl.Name + ', class=' + AControl.ClassName);
      BeginUpdate;
      aBand := FBands.Add;
      aBand.Control := AControl;
      aBand.Width := aBand.CalcPreferredWidth;
      EndUpdate;
    end;
  end;
end;

procedure TCustomCoolBar.Invalidate;
var aBorderWidth: Integer;
begin
  aBorderWidth := 0;
  if EdgeOuter <> esNone then inc(aBorderWidth);
  if EdgeInner <> esNone then inc(aBorderWidth);
  if (FBorderWidth <> aBorderWidth) or (FBorderEdges <> EdgeBorders) then begin
    FBorderWidth := aBorderWidth;
    FBorderEdges := EdgeBorders;
    if ebLeft in EdgeBorders then
      FBorderLeft := aBorderWidth
    else
      FBorderLeft := 0;
    if ebTop in EdgeBorders then
      FBorderTop := aBorderWidth
    else
      FBorderTop := 0;
    if ebRight in EdgeBorders then
      FBorderRight := aBorderWidth
    else
      FBorderRight := 0;
    if ebBottom in EdgeBorders then
      FBorderBottom := aBorderWidth
    else
      FBorderBottom := 0;
    CalculateAndAlign;
    //DebugLn('Change BorderEdge');
  end;
  inherited Invalidate;
end;

function TCustomCoolBar.IsFirstAtRow(ABand: Integer): Boolean;
begin
  if not Vertical then begin
    if not FRightToLeft then
      Result := (FVisiBands[ABand].FLeft = FBorderLeft)
    else
      Result := (FVisiBands[ABand].Right = Width-FBorderRight);
  end else
    Result := (FVisiBands[ABand].FLeft = FBorderTop);
end;

function TCustomCoolBar.IsRowEnd(ALeft, AVisibleIndex: Integer): Boolean;
begin
  Result := (AVisibleIndex < length(FVisiBands)-1) and RowEndHelper(ALeft, AVisibleIndex);
end;

procedure TCustomCoolBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var aBand: Integer;
    aGrabber: Boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight then
  begin
    Cursor := crDefault;
    Exit;
  end;
  MouseToBandPos(X, Y, aBand, aGrabber);
  FDraggedBandIndex := aBand;
  if (aBand >= 0) and (length(FVisiBands) >= 1) then begin  //hit any Band
    if not aGrabber or IsFirstAtRow(aBand)
      or FFixedSize or FVisiBands[aBand-1].FFixedSize then begin
      if not FFixedOrder then FDragBand := dbMove;  //move Band
    end else begin  //resize Band
      if not FFixedSize and not FVisiBands[aBand-1].FFixedSize then begin
        FDragBand := dbResize;
        if not Vertical then begin
          if not FRightToLeft then
            FDragInitPos := X-FVisiBands[aBand-1].FWidth-FVisiBands[aBand-1].FLeft
          else
            FDragInitPos := FVisiBands[aBand-1].FLeft-X;
        end else
          FDragInitPos := Y-FVisiBands[aBand-1].FWidth-FVisiBands[aBand-1].FLeft;
      end;
    end;
  end;
end;

procedure TCustomCoolBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var aBand: Integer;
    aGrabber: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if length(FVisiBands) > 0 then begin
    case FDragBand of
      dbNone: begin
        MouseToBandPos(X, Y, aBand, aGrabber);
        if aBand >= 0 then begin
          if aGrabber and (aBand > 0) and not FVisiBands[aBand-1].FixedSize
             and not FFixedSize and not IsFirstAtRow(aBand) then
            ChangeCursor(True, True)
          else
            if length(FVisiBands) > 1 then ChangeCursor(not FixedOrder, False);
        end else
          ChangeCursor(False, False);
      end;
      dbResize: begin
        if not Vertical then begin
          if not FRightToLeft then
            FVisiBands[FDraggedBandIndex-1].Width :=
              X-FDragInitPos-FVisiBands[FDraggedBandIndex-1].FLeft
          else
            FVisiBands[FDraggedBandIndex-1].Width :=
              -X-FDragInitPos+FVisiBands[FDraggedBandIndex-1].Right;
        end else
          FVisiBands[FDraggedBandIndex-1].Width :=
            Y-FDragInitPos-FVisiBands[FDraggedBandIndex-1].FLeft
      end;
      dbMove: begin
        if FDraggedBandIndex>-1 then begin
          Cursor := crDrag;
          if DragManager.CanStartDragging(self,-1,X,Y) then
          begin
            DragManager.DragStart(FVisiBands[FDraggedBandIndex].Control, True, -1, True);
            Cursor := crDefault;
            FDraggedBandIndex := -1;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCustomCoolBar.MouseToBandPos(X, Y: Integer; out ABand: Integer; out AGrabber: Boolean);
var i, aCountM1, aLeft, aTop: Integer;
begin
  ABand := low(Integer);
  AGrabber := False;
  aCountM1 := length(FVisiBands)-1;
  if Vertical then begin
    i := Y;
    Y := X;
    X := i;
  end;
  if aCountM1 >= 0 then begin
    if not Vertical or not FRightToLeft then begin
      if Y > (FVisiBands[aCountM1].Top+FVisiBands[aCountM1].Height+TCoolBand.cDivider) then
        ABand := cNewRowBelow  //new row, i.e. free space below the last row
      else
        if Y < 0 then ABand := cNewRowAbove;  //new row, i.e. free space above the first row
    end else begin
      if Y < (FVisiBands[aCountM1].Top) then
        ABand := cNewRowBelow  //new row, i.e. free space below the last row
      else
        if Y > Width then ABand := cNewRowAbove;  //new row, i.e. free space space above the first row
    end;
    if ABand = low(Integer) then
      for i := 0 to aCountM1 do begin
        aLeft := FVisiBands[i].FRealLeft;
        aTop := FVisiBands[i].FTop;
        if PtInRect(Rect(aLeft, aTop, aLeft+FVisiBands[i].FRealWidth,
            aTop+FVisiBands[i].FHeight), Point(X, Y)) then begin
          ABand := i;
          //DebugLn('Mouse over Band ', i);
          if not FRightToLeft or Vertical then
            AGrabber := (X <= (aLeft+GrabWidth+1))
          else
            AGrabber := (X >= (FVisiBands[i].FLeft+FVisiBands[i].Width-GrabWidth-1));
          //DebugLn('Grabber '+BoolToStr(AGrabber), ' hit', ' not hit');
          Exit;  //Exit!
        end;
      end;
  end;
end;

procedure TCustomCoolBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var aBand, w, h, offs: Integer;
    newRowBelow, needRecalc, needBandMaximize, aGrabber: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FBandMaximize<>bmNone then begin
    case FBandMaximize of
      bmClick:   needBandMaximize:=true;
      bmDblClick:needBandMaximize:=ssDouble in Shift;
      else       needBandMaximize:=false;
    end;
    if needBandMaximize then begin
      MouseToBandPos(X, Y, aBand, aGrabber);
      if aGrabber and Assigned(FVisiBands[aBand].control) then begin
        w:=0;
        h:=0;
        FVisiBands[aBand].control.GetPreferredSize(w,h);
        offs:=FVisiBands[aBand].CalcControlLeft
             +FVisiBands[aBand].cDivider + HorizontalSpacing;
        if vertical then
          FVisiBands[aBand].width:=offs+h
        else
          FVisiBands[aBand].width:=offs+w;
        FDraggedBandIndex:=-1;
      end;
    end;
  end;
  if (FDragBand = dbMove) and (FDraggedBandIndex <> -1) then begin
    needRecalc := False;
    MouseToBandPos(X, Y, aBand, newRowBelow);  //newRowBelow is NOT used here
    if aBand >= cNewRowAbove then begin
      if aBand = cNewRowAbove then begin
        if FDraggedBandIndex = 0 then begin
          if FVisiBands[0].FTop = FVisiBands[1].FTop then begin
            FVisiBands[1].FBreak := True;
            needRecalc := True;
          end;
        end else begin
          FVisiBands[1].FBreak := True;
          FVisiBands[FDraggedBandIndex].Index := 0;
        end;
      end else begin
        newRowBelow := (aBand = cNewRowBelow);
        if newRowBelow then aBand := length(FVisiBands)-1;
        if Vertical then X := Y;
        if aBand <> FDraggedBandIndex then begin  //move to new position
          if FVisiBands[FDraggedBandIndex].FBreak and (FDraggedBandIndex < (length(FVisiBands)-1))
            then FVisiBands[FDraggedBandIndex+1].FBreak := True;
          if not newRowBelow and (((not FRightToLeft or Vertical)
            and (X > (FVisiBands[aBand].FLeft+FVisiBands[aBand].Width)))
            or ((FRightToLeft and not Vertical)
            and (X < FVisiBands[aBand].FLeft))) then begin  //beyond the last band in row
            FVisiBands[FDraggedBandIndex].FBreak := False;
            if FDraggedBandIndex > aBand then
              FVisiBands[FDraggedBandIndex].Index := CalculateRealIndex(aBand+1)
            else
              FVisiBands[FDraggedBandIndex].Index := CalculateRealIndex(aBand);
            needRecalc := (FDraggedBandIndex = (aBand+1));
          end else begin  //on another Band
            FVisiBands[FDraggedBandIndex].FBreak := FVisiBands[aBand].Break;
            if FDraggedBandIndex > aBand then begin  //move up or left
              FVisiBands[aBand].FBreak := False;
              FVisiBands[FDraggedBandIndex].Index := CalculateRealIndex(aBand);
            end else begin  //move down or right
              if not newRowBelow then begin
                if (FVisiBands[FDraggedBandIndex].FTop = FVisiBands[aBand].FTop) then begin  //the same row
                  FVisiBands[FDraggedBandIndex].FBreak := False;
                  FVisiBands[FDraggedBandIndex].Index := CalculateRealIndex(aBand);
                end else begin  //other row
                  FVisiBands[aBand].FBreak := False;
                  FVisiBands[FDraggedBandIndex].Index := CalculateRealIndex(aBand-1);
                  needRecalc := (FDraggedBandIndex = (aBand-1));
                end;
              end else begin  //new row
                FVisiBands[FDraggedBandIndex].FBreak := True;
                FVisiBands[FDraggedBandIndex].Index := CalculateRealIndex(aBand);
              end;
            end;
          end;
        end else
          if newRowBelow then begin  //last Band in last row moved to new row
            FVisiBands[aBand].FBreak := True;
            needRecalc:= True;
          end;
      end;
      if needRecalc then begin  //necessary only when no Index is changed
        CalculateAndAlign;
        Invalidate;
      end;
    end;
    Cursor := crDefault;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
  FDragBand := dbNone;
end;

procedure TCustomCoolBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if csDestroying in ComponentState then Exit;
  if Operation = opRemove then begin
    //DebugLn('TCoolBar.Notification: Operation = opRemove');
    if AComponent = FImages then Images := Nil;
  end;
end;

procedure TCustomCoolBar.Paint;
var i: Integer;
    aGrabDetails,aBackground: TThemedElementDetails;
    aGrabStyle: TGrabStyle;
    aRaisedBevel: Boolean;
    aRect: TRect;
const arBevel: array[False..True] of TColor = (clBtnShadow, clBtnHighlight);

  function GetCaptionColorDisabled: TColor;
  var r1, g1, b1: Byte;
      aColor: TColor;
  begin
    aColor := Font.Color;
    if aColor = clDefault then aColor := clBtnText;
    GetRGBValues(ColorToRGB(aColor), r1, g1, b1);
    i := r1 div 3 + g1 div 3 + b1 div 3;
    GetRGBValues(ColorToRGB(Brush.Color), r1, g1, b1);
    i := (i+(r1 div 3 + g1 div 3 + b1 div 3)) div 2;
    Result := RGBToColor(i, i, i);
  end;

  procedure PaintGrabber(aRect: TRect);
  var l, w: SmallInt;
  begin
    case aGrabStyle of
      gsSimple: begin
        Canvas.Pen.Color := clBtnHighlight;
        Canvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
        Canvas.Line(aRect.Left, aRect.Top, aRect.Left, aRect.Bottom+1);
        Canvas.Pen.Color := clBtnShadow;
        Canvas.Line(aRect.Left, aRect.Bottom, aRect.Right, aRect.Bottom);
        Canvas.Line(aRect.Right, aRect.Top, aRect.Right, aRect.Bottom+1);
      end;
      gsDouble: begin
        w := (FGrabWidth-2) div 2;
        Canvas.Pen.Color := clBtnHighlight;
        if not Vertical then begin
          Canvas.Line(aRect.Left, aRect.Top, aRect.Left+w, aRect.Top);
          Canvas.Line(aRect.Left, aRect.Top, aRect.Left, aRect.Bottom+1);
          Canvas.Line(aRect.Right-w, aRect.Top, aRect.Right, aRect.Top);
          Canvas.Line(aRect.Right-w, aRect.Top, aRect.Right-w, aRect.Bottom+1);
        end else begin
          Canvas.Line(aRect.Left, aRect.Top, aRect.Left, aRect.Top+w);
          Canvas.Line(aRect.Left, aRect.Top, aRect.Right+1, aRect.Top);
          Canvas.Line(aRect.Left, aRect.Bottom-w, aRect.Right, aRect.Bottom-w);
          Canvas.Line(aRect.Left, aRect.Bottom-w, aRect.Left, aRect.Bottom+1);
        end;
        Canvas.Pen.Color := clBtnShadow;
        if not Vertical then begin
          Canvas.Line(aRect.Left, aRect.Bottom, aRect.Left+w, aRect.Bottom);
          Canvas.Line(aRect.Left+w, aRect.Top, aRect.Left+w, aRect.Bottom+1);
          Canvas.Line(aRect.Right-w, aRect.Bottom, aRect.Right, aRect.Bottom);
          Canvas.Line(aRect.Right, aRect.Top, aRect.Right, aRect.Bottom+1);
        end else begin
          Canvas.Line(aRect.Left, aRect.Top+w, aRect.Right, aRect.Top+w);
          Canvas.Line(aRect.Right, aRect.Top, aRect.Right, aRect.Top+w+1);
          Canvas.Line(aRect.Left, aRect.Bottom, aRect.Right, aRect.Bottom);
          Canvas.Line(aRect.Right, aRect.Bottom-w, aRect.Right, aRect.Bottom+1);
        end;
      end;
      gsHorLines: begin
        l := (aRect.Bottom-aRect.Top+1) div 3;
        inc(aRect.Top);
        Canvas.Pen.Color := clBtnShadow;
        for w := 0 to l-1 do
          Canvas.Line(aRect.Left, aRect.Top+w*3, aRect.Right, aRect.Top+w*3);
        Canvas.Pen.Color := clBtnHighlight;
        inc(aRect.Top);
        for w := 0 to l-1 do
          Canvas.Line(aRect.Left, aRect.Top+w*3, aRect.Right, aRect.Top+w*3);
      end;
      gsVerLines: begin
        l := (aRect.Right-aRect.Left+1) div 3;
        inc(aRect.Left);
        Canvas.Pen.Color := clBtnShadow;
        for w := 0 to l-1 do
          Canvas.Line(aRect.Left+w*3, aRect.Top, aRect.Left+w*3, aRect.Bottom+1);
        Canvas.Pen.Color := clBtnHighlight;
        inc(aRect.Left);
        for w := 0 to l-1 do
          Canvas.Line(aRect.Left+w*3, aRect.Top, aRect.Left+w*3, aRect.Bottom+1);
      end;
      gsGripper: begin
        dec(aRect.Top);
        inc(aRect.Bottom);
        Canvas.ClipRect := aRect;
        Canvas.Clipping := True;
        ThemeServices.DrawElement(Canvas.Handle, aGrabDetails, aRect);
        Canvas.Clipping := False;
      end;
      gsButton: begin
        dec(aRect.Top);
        inc(aRect.Bottom);
        ThemeServices.DrawElement(Canvas.Handle, aGrabDetails, aRect);
      end;
    end;
  end;

  procedure PaintSeparator(Y: Integer);
  begin
    //DebugLn('PaintSeparator');
    if not Vertical then begin
      Canvas.Pen.Color := arBevel[aRaisedBevel];
      Canvas.Line(FBorderLeft, Y, Width-FBorderRight, Y);
      inc(Y);
      Canvas.Pen.Color := arBevel[not aRaisedBevel];
      Canvas.Line(FBorderLeft, Y, Width-FBorderRight, Y);
    end else begin
      Canvas.Pen.Color := arBevel[aRaisedBevel];
      Canvas.Line(Y, FBorderTop, Y, Height-FBorderBottom);
      inc(Y);
      Canvas.Pen.Color := arBevel[not aRaisedBevel];
      Canvas.Line(Y, FBorderTop, Y, Height-FBorderBottom);
    end;
  end;

var k, x, aCountM1, aLeft, aTop: Integer;
    aRowEnd: Boolean;
    aColor: TColor;
    aDetails: TThemedElementDetails;
    aFlags: Cardinal;
    aImageSize: TSize;
begin
  inherited Paint;
  if Assigned(FImages) then
    aImageSize := FImages.SizeForPPI[ImagesWidth, Font.PixelsPerInch];
  //DebugLn('TCoolBar.Paint');
  //draw Bitmap Background
  if FBitmap.Width > 0 then
    DrawTiledBitmap(ClientRect, FBitmap)
  else begin
    if FThemed then begin
      aBackground:=ThemeServices.GetElementDetails(trRebarRoot);
      ThemeServices.DrawElement(Canvas.Handle,aBackground,ClientRect);
    end;
  end;
  aCountM1 := length(FVisiBands)-1;
  if aCountM1 >= 0 then begin
    aRaisedBevel := (FBandBorderStyle = bsSingle) and
                    (EdgeInner = esLowered) and (EdgeOuter = esRaised);
    aRowEnd := False;
    aGrabStyle := GrabStyle;
    if Vertical then
      case aGrabStyle of
        gsHorLines: aGrabStyle := gsVerLines;
        gsVerLines: aGrabStyle := gsHorLines;
      end;
    case aGrabStyle of
      gsGripper: if not Vertical then
                   aGrabDetails := ThemeServices.GetElementDetails(trGripper)
                 else
                   aGrabDetails := ThemeServices.GetElementDetails(trGripperVert);
      gsButton: aGrabDetails := ThemeServices.GetElementDetails(tbPushButtonNormal);
    end;
    if FShowText or Assigned(FImages) then begin
      if IsEnabled then
        aDetails := ThemeServices.GetElementDetails(tbPushButtonNormal)
      else
        aDetails := ThemeServices.GetElementDetails(tbPushButtonDisabled);
      aFlags := DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX;
      if FRightToLeft then aFlags := aFlags or DT_RTLREADING;
    end;
    if FShowText then begin
      if IsEnabled then
        Canvas.Font.Color := Font.Color
      else
        Canvas.Font.Color := GetCaptionColorDisabled;
    end;
    for i := 0 to aCountM1 do begin
      aLeft := FVisiBands[i].FLeft;
      aTop := FVisiBands[i].FTop;
      aRect := Rect(aLeft, aTop, aLeft+FVisiBands[i].FRealWidth+1, aTop+FVisiBands[i].FHeight);
      //paint Band Background
      if FVisiBands[i].Bitmap.Width > 0 then
        DrawTiledBitmap(aRect, FVisiBands[i].Bitmap)
      else begin
        if not FVisiBands[i].FixedBackground and FVisiBands[i].ParentBitmap
          and (Bitmap.Width > 0) then
          DrawTiledBitmap(aRect, Bitmap)
        else begin
          aColor := FVisiBands[i].FColor;
          if (aColor <> clDefault) and (aColor <> clNone) then begin
            Canvas.Brush.Color := aColor;
            Canvas.FillRect(aRect);
          end;
        end;
      end;
      //paint a Grabber
      if not FRightToLeft or Vertical then
        x := aLeft+TCoolBand.cGrabIndent
      else
        x := aLeft+FVisiBands[i].Width-GrabWidth-TCoolBand.cGrabIndent;
      if not Vertical then
        PaintGrabber(Rect(x, aTop+2, x+GrabWidth-1, aTop+FVisiBands[i].FHeight-3))
      else
        PaintGrabber(Rect(aTop+2, x, aTop+FVisiBands[i].FHeight-3, x+GrabWidth-1));
      if not FRightToLeft or Vertical then
        x := x+GrabWidth+HorizontalSpacing
      else
        x := x-HorizontalSpacing;
      //paint Image
      if Assigned(FImages) and (FVisiBands[i].ImageIndex >= 0) then begin
        if FRightToLeft and not Vertical then dec(x, aImageSize.cx);
        if not Vertical then
          ThemeServices.DrawIcon(Canvas, aDetails,
            Point(x, aTop+(FVisiBands[i].FHeight-aImageSize.cy) div 2),
            FImages, FVisiBands[i].ImageIndex)
        else
          ThemeServices.DrawIcon(Canvas, aDetails,
            Point(aTop+(FVisiBands[i].FHeight-aImageSize.cx) div 2, x),
            FImages, FVisiBands[i].ImageIndex);
        if not FRightToLeft or Vertical then
          inc(x, aImageSize.cx+HorizontalSpacing)
        else
          dec(x, HorizontalSpacing);
      end;
      //paint Text
      if FShowText then begin
        k := aTop + (FVisiBands[i].FHeight - FTextHeight) div 2;
        if not Vertical then begin
          if FRightToLeft then dec(x, FVisiBands[i].FTextWidth);
          Canvas.Font.Orientation := 0;
          aRect := Rect(x, k, x+FVisiBands[i].FTextWidth, k+FTextHeight);
        end else begin
          Canvas.Font.Orientation := 900;
          aRect := Rect(k, x+FVisiBands[i].FTextWidth, k+FVisiBands[i].FTextWidth, x+2*FVisiBands[i].FTextWidth);
        end;
        Canvas.Brush.Style := bsClear;
        Canvas.TextOut(aRect.Left, aRect.Top, FVisiBands[i].Text);
      end;
      if not FRightToLeft or Vertical then
        aRowEnd := IsRowEnd(aLeft, i)
      else
        aRowEnd := IsRowEnd(Width-x, i);
      if BandBorderStyle = bsSingle then begin
        //paint a Separator border below the row of bands ____
        x := aLeft;
        inc(aLeft, FVisiBands[i].Width);
        if (aRowEnd or ((i = aCountM1) and not AutoSize)) then begin
          if not Vertical or not FRightToLeft then
           PaintSeparator(aTop+FVisiBands[i].FHeight)
         else
           PaintSeparator(aTop-TCoolBand.cDivider);
        end;
        if not aRowEnd and (i < aCountM1) then begin
          //paint Divider |
          if not FRightToLeft or Vertical then x := aLeft-TCoolBand.cDivider;
          Canvas.Pen.Color := arBevel[not aRaisedBevel];
          if not Vertical then
            Canvas.Line(x+1, aTop+1, x+1, aTop+FVisiBands[i].FHeight-1)
          else
            Canvas.Line(aTop+1, x+1, aTop+FVisiBands[i].FHeight-1, x+1);
          Canvas.Pen.Color := arBevel[aRaisedBevel];
          if not Vertical then
            Canvas.Line(x, aTop+1, x, aTop+FVisiBands[i].FHeight-1)
          else
            Canvas.Line(aTop+1, x, aTop+FVisiBands[i].FHeight-1, x);
        end;
      end;
    end;
  end;
end;

procedure TCustomCoolBar.RemoveControl(AControl: TControl);
var aBandIndex:Integer;
begin
  inherited RemoveControl(AControl);
  aBandIndex := Bands.FindBandIndex(AControl);
  if aBandIndex > -1 then
  begin
    //DebugLn('TCoolBar.RemoveControl: Comp=' + AControl.Name + ', class=' + AControl.ClassName);
    Bands.Items[aBandIndex].Control := Nil;
    FDragBand:=dbNone;
    FDraggedBandIndex:=-1;
    Bands.Delete(aBandIndex);
    CalculateAndAlign;
    Invalidate;
  end;
end;

function TCustomCoolBar.RowEndHelper(ALeft, AVisibleIdx: Integer): Boolean;
var aLimit: Integer;
begin
  if not Vertical then
    aLimit := Width
  else
    aLimit := Height;
  Result := FVisiBands[AVisibleIdx+1].Break or
    (ALeft+FVisiBands[AVisibleIdx+1].Width-TCoolBand.cDivider >= aLimit);
end;

procedure TCustomCoolBar.WMSize(var Message: TLMSize);
begin
  //DebugLn('WMSize');
  inherited WMSize(Message);
  CalculateAndAlign;
  Invalidate;  //required by GTK2 and WINDOWS
end;

