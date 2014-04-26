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
  FControl := nil;
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

function TCoolBand.CalcPreferredHeight: Integer;
begin
  Result := FMinHeight;
  if assigned(FControl) then
    Result := max(Result, FControl.Height+2*cVertSpacing);
  if FCoolBar.FShowText then
    Result := max(Result, FCoolBar.FTextHeight+2*cVertSpacing);
  if assigned(FCoolBar.Images) and (ImageIndex >= 0) then
    Result := max(Result, FCoolBar.Images.Height+2*cVertSpacing);
end;

function TCoolBand.CalcPrefferedWidth: Integer;
begin
  Result := FCoolBar.GrabWidth+2*cHorSpacing;
  if assigned(Control) then
    inc(Result, Control.Width+cHorSpacing);
  if (FText <> '') and FCoolBar.FShowText then
    inc(Result, FCoolBar.Canvas.TextWidth(FText)+cHorSpacing);
  Result := max(FMinWidth, Result);
end;

function TCoolBand.GetDisplayName: string;
begin
  Result := Text;
  if Result = '' then Result := ClassName;
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
  FCoolBar.BeginUpdate;
  try
    if assigned(AValue) then
    begin
      AValue.Align := alNone;
      aBand := TCoolBands(Collection).FindBand(AValue);
      if assigned(aBand) and (aBand <> Self) then
        aBand.SetControl(Nil);  // Remove old association
      AValue.Parent := FCoolBar;
    end;
    FControl := AValue;
    Changed(True);
  finally
    FCoolBar.EndUpdate;
  end;
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
  if AValue = FText then exit;
  FText := AValue;
  Changed(True);
end;

procedure TCoolBand.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then Exit;
  FVisible := AValue;
  if assigned(FControl) then FControl.Visible := AValue;
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
  Result := nil;
  for i := 0 to Count-1 do
    if GetItem(i).FControl = AControl then
      Exit(GetItem(i));
end;

procedure TCoolBands.Notify(aItem: TCollectionItem; aAction: TCollectionNotification);
begin
  inherited Notify(aItem, aAction);
  case aAction of
    cnAdded: begin
      //DebugLn('TCoolBands.Notify: aAction = cnAdded');
      TCoolBand(aItem).FCoolBar:=FCoolBar;
    end;
    cnExtracting: begin
      //DebugLn('TCoolBands.Notify: aAction = cnExtracting');
    end;
    cnDeleting: begin
      //DebugLn('TCoolBands.Notify: aAction = cnDeleting');
    end;
  end;
end;

procedure TCoolBands.Update(aItem: TCollectionItem);
begin
  inherited Update(aItem);
  if assigned(FCoolBar) then begin
    //DebugLn('Bands.Update calls CalcAndAlign');
    if not assigned(aItem) then FCoolBar.CalculateAndAlign;
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

procedure TCoolBands.SetItem(Index: Integer; aValue: TCoolBand);
begin
  inherited SetItem(Index, aValue);
end;

{ TCustomCoolBar }

constructor TCustomCoolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption]
                + [csAcceptsControls, csNoFocus, csOpaque, csParentBackground, csReplicatable];
  Align := alTop;
  Height := 75;
  ParentColor := True;
  ParentFont := True;
  FBandBorderStyle := bsSingle;
  FBandMaximize := bmClick;
  FBands := TCoolBands.Create(Self);
  FBitmap := TBitmap.Create;
  FBitmap.OnChange:=@BitmapOrImageListChange;
  FGrabStyle := cDefGrabStyle;
  FGrabWidth := cDefGrabWidth;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @BitmapOrImageListChange;
  FShowText := True;
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

procedure TCustomCoolBar.SetBitmap(aValue: TBitmap);
begin
  FBitmap.Assign(aValue);
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

procedure TCustomCoolBar.SetShowText(AValue: Boolean);
begin
  if FShowText = AValue then Exit;
  FShowText := AValue;
  CalculateAndAlign;
  Invalidate;
end;

procedure TCustomCoolBar.SetVertical(aValue: Boolean);
begin
  if FVertical = aValue then Exit;
  FVertical := aValue;
  Invalidate;
end;

procedure TCustomCoolBar.BitmapOrImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomCoolBar.CalculateAndAlign;
var i, x, y, aCountM1, aHeight, aLeft, aStartIndex, aTop, aWidth: Integer;
    aRowEnd: Boolean;
begin
  if (FUpdateCount > 0) or ([csLoading, csDestroying] * ComponentState <> []) then exit;
  //DebugLn('CalculateAndAlign');
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
  //Do not use FBands from this point, only FVisiBands
  aHeight := 0;
  aStartIndex := 0;
  aRowEnd := True;
  if AutoSize and (aCountM1 >= 0) then DisableAutoSizing;
  for i := 0 to aCountM1 do
  begin
    if (FVisiBands[i].Break or Vertical) or aRowEnd then aLeft := cBorderWidth;
    aHeight := Max(aHeight, FVisiBands[i].CalcPreferredHeight);
    inc(aLeft, FVisiBands[i].Width);
    aRowEnd := (i = aCountM1) or ((i < aCountM1)
      and ((FVisiBands[i+1].Break or Vertical)
      or ((aLeft+FVisiBands[i+1].Width) > (ClientWidth-2*cBorderWidth))));
    //Set all Bands in row to uniform height
    if aRowEnd then begin
      for y := aStartIndex to i do
        FVisiBands[y].FHeight := aHeight;
      aHeight := 0;
      aStartIndex := i+1;
    end;
  end;
  aTop := cBorderWidth;
  aRowEnd := True;
  for i := 0 to aCountM1 do
  begin
    if aRowEnd or (FVisiBands[i].Break or Vertical) then aLeft := cBorderWidth;
    FVisiBands[i].FLeft := aLeft;
    FVisiBands[i].FTop := aTop;
    if assigned(FVisiBands[i].Control) then begin
      x := 2+GrabWidth+TCoolBand.cHorSpacing;
      if (FVisiBands[i].Text<>'') and FShowText then
        inc(x, Canvas.TextWidth(FVisiBands[i].Text)+TCoolBand.cHorSpacing);
      if assigned(FImages) and (FVisiBands[i].ImageIndex >=0) then
        inc(x, FImages.Width+TCoolBand.cHorSpacing);
      aWidth := FVisiBands[i].Width-x-TCoolBand.cHorSpacing-cBorderWidth;
      inc(x, aLeft);
      y := aTop+(FVisiBands[i].FHeight-FVisiBands[i].Control.Height) div 2;
      FVisiBands[i].Control.Width:=aWidth;
      FVisiBands[i].Control.AnchorParallel(akLeft, x-cBorderWidth, self);
      FVisiBands[i].Control.AnchorParallel(akTop, y-cBorderWidth, self);
    end;
    x := FVisiBands[i].Width;
    inc(aLeft, x);
    aRowEnd := IsRowEnd(aLeft, i);
    if aRowEnd or (i = aCountM1) then
      FVisiBands[i].FRealWidth := x+ClientWidth-aLeft-cBorderWidth
    else
      FVisiBands[i].FRealWidth := x;
    if aRowEnd then
      inc(aTop, FVisiBands[i].FHeight+cBorderWidth);
  end;
  if AutoSize then begin
    inc(FUpdateCount);
    InvalidatePreferredSize;
    AdjustSize;
    if aCountM1 >= 0 then EnableAutoSizing;
    dec(FUpdateCount);
  end;
  FPrevWidth := Width;
  FPrevHeight := Height;
end;

procedure TCustomCoolBar.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
  WithThemeSpace: Boolean);
var i, aCountM1, aPrefWidth: Integer;
begin
  aCountM1 := length(FVisiBands)-1;
  if aCountM1 >= 0 then
    PreferredHeight := FVisiBands[aCountM1].FTop+FVisiBands[aCountM1].FHeight+2
  else
    PreferredHeight := TCoolBand.cDefMinHeight+4;
  if not FVertical then
    PreferredWidth := 0
  else begin
    aPrefWidth := TCoolBand.cDefMinHeight+4;  //min. Width is ~ 25 pixels
    for i := 0 to aCountM1 do
      aPrefWidth := max(aPrefWidth, FVisiBands[i].Width);
    PreferredWidth := aPrefWidth;
  end;
end;

function TCustomCoolBar.CalculateRealIndex(AVisibleIndex: Integer): Integer;
var i, aInvisibles, aVisibles: Integer;
begin
  aInvisibles := 0;
  aVisibles := 0;
  for i:=0 to FBands.Count-1 do
  begin
    if not FBands[i].Visible then
      inc(aInvisibles)
    else
      inc(aVisibles);
    if aVisibles > AVisibleIndex then break;
  end;
  Result := AVisibleIndex+aInvisibles;
end;

procedure TCustomCoolBar.CreateWnd;
begin
  inherited CreateWnd;
  FDefCursor := Cursor;
  DoFontChanged;
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

procedure TCustomCoolBar.DoFontChanged;
begin
  FTextHeight := Canvas.TextHeight('Žy|');
end;

procedure TCustomCoolBar.EndUpdate;
begin
  inherited EndUpdate;
  //DebugLn('EndUpdate calls CalculateAndAlign');
  CalculateAndAlign;
  Invalidate;
end;

procedure TCustomCoolBar.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  DoFontChanged;
  //DebugLn('FontChanged calls CalculateAndAlign');
  CalculateAndAlign;
end;

function TCustomCoolBar.IsRowEnd(ALeft, AVisibleIndex: Integer): Boolean;
begin
  Result := (AVisibleIndex < (length(FVisiBands)-1))
    and ((FVisiBands[AVisibleIndex+1].Break or Vertical)
    or ((ALeft+FVisiBands[AVisibleIndex+1].Width) > ClientWidth));
end;

procedure TCustomCoolBar.InsertControl(AControl: TControl; Index: integer);
var aBand: TCoolBand;
begin
  inherited InsertControl(AControl, Index);
  //DebugLn('TCustomCoolBar.InsertControl');
  if (FUpdateCount = 0) and (AControl is TWinControl) and
    not (csLoading in ComponentState) then
    begin
      aBand := Bands.FindBand(AControl);
      if aBand = Nil then
      begin
        //DebugLn('TCoolBar.InsertControl: Adding band for Comp=' + AControl.Name + ', class=' + AControl.ClassName);
        BeginUpdate;
        aBand := FBands.Add;
        aBand.Control := AControl;
        aBand.Width := aBand.CalcPrefferedWidth;
        EndUpdate;
      end;
    end;
end;

procedure TCustomCoolBar.RemoveControl(AControl: TControl);
var aBand: TCoolBand;
begin
  inherited RemoveControl(AControl);
  aBand := Bands.FindBand(AControl);
  if assigned(aBand) then begin
    //DebugLn('TCoolBar.RemoveControl: Comp=' + AControl.Name + ', class=' + AControl.ClassName);
    aBand.FControl := nil;
    CalculateAndAlign;
    Invalidate;
  end;
end;

procedure TCustomCoolBar.Loaded;
begin
  inherited Loaded;
  //DebugLn('TCoolBar.Loaded');
  FBands.Update(Nil);
end;

procedure TCustomCoolBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var aBand: Integer;
    aGrabber: Boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);
  MouseToBandPos(X, Y, aBand, aGrabber);
  FDraggedBandIndex := aBand;
  if aBand >= 0 then begin  //Hit any Band
    if not aGrabber or (FVisiBands[aBand].FLeft = cBorderWidth)
      or FFixedSize or FVisiBands[aBand-1].FFixedSize then begin
      if not FFixedOrder then begin  //Move Band
        FDragBand := dbMove;
        Cursor := crDrag;
      end;
    end else begin  //Resize Band
      if not FFixedSize and not FVisiBands[aBand-1].FFixedSize then begin
        FDragBand := dbResize;
        FDragInitPos := X-FVisiBands[aBand-1].FWidth-FVisiBands[aBand-1].FLeft;
      end;
    end;
  end;
end;

procedure TCustomCoolBar.MouseEnter;
begin
  inherited MouseEnter;
  FDefCursor := Cursor;
end;

procedure TCustomCoolBar.MouseLeave;
begin
  inherited MouseLeave;
  Cursor := FDefCursor;
end;

procedure TCustomCoolBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var aBand: Integer;
    aGrabber: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if (FDragBand = dbNone) and not FFixedSize then begin
    MouseToBandPos(X, Y, aBand, aGrabber);
    if (aBand >= 1) and not FVisiBands[aBand-1].FFixedSize then begin
      if aGrabber and (aBand > 0) and (FVisiBands[aBand].FLeft > cBorderWidth) then
        Cursor := crHSplit
      else
        Cursor := FDefCursor;
    end;
  end else
  if FDragBand = dbResize then begin
    FVisiBands[FDraggedBandIndex-1].Width := X-FDragInitPos-FVisiBands[FDraggedBandIndex-1].FLeft;
  end;
end;

procedure TCustomCoolBar.MouseToBandPos(X, Y: Integer; out ABand: Integer; out AGrabber: Boolean);
var i, aCountM1, aLeft, aTop: Integer;
begin
  ABand := low(Integer);
  AGrabber := False;
  aCountM1 := length(FVisiBands)-1;
  if aCountM1 >= 0 then begin
    if Y > (FVisiBands[aCountM1].FTop+FVisiBands[aCountM1].FHeight+cBorderWidth) then
      ABand := -1  // new row, i.e. free space below the last row
    else
      for i := 0 to aCountM1 do
      begin
        aLeft := FVisiBands[i].FLeft;
        aTop := FVisiBands[i].FTop;
        if PtInRect(Rect(aLeft, aTop, aLeft+FVisiBands[i].FRealWidth,
          aTop+FVisiBands[i].FHeight), Point(X, Y)) then
        begin
          ABand := i;
          //DebugLn('Mouse over Band ', i);
          AGrabber := (X <= (aLeft+GrabWidth+1));
          //DebugLn('Grabber '+BoolToStr(AGrabber), ' hit', ' not hit');
          exit;  // EXIT!
        end;
      end;
  end;
end;

procedure TCustomCoolBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var aBand: Integer;
    newRow, needRecalc: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragBand = dbMove then begin
    needRecalc := False;
    MouseToBandPos(X, Y, aBand, newRow);  //newRow is NOT used here
    if aBand >= -1 then begin
      newRow := (aBand = -1);
      if newRow then aBand := length(FVisiBands)-1;
      if aBand <> FDraggedBandIndex then begin  //move to new position
        if (FVisiBands[FDraggedBandIndex].Break or Vertical)
          and (FDraggedBandIndex < (length(FVisiBands)-1))
          then FVisiBands[FDraggedBandIndex+1].FBreak := True;
        if (X > (FVisiBands[aBand].FLeft+FVisiBands[aBand].Width)) then begin  //beyond the last band in row
          FVisiBands[FDraggedBandIndex].FBreak := False;
          if FDraggedBandIndex > aBand then
            FVisiBands[FDraggedBandIndex].Index := CalculateRealIndex(aBand+1)
          else
            FVisiBands[FDraggedBandIndex].Index := CalculateRealIndex(aBand);
          if FDraggedBandIndex = (aBand+1) then needRecalc := True;
        end else begin  //on another Band
          FVisiBands[FDraggedBandIndex].FBreak := FVisiBands[aBand].Break;
          if FDraggedBandIndex > aBand then begin  //move up or left
            FVisiBands[aBand].FBreak := False;
            FVisiBands[FDraggedBandIndex].Index := CalculateRealIndex(aBand);
          end else begin  //move down or right
            if not newRow then begin
              if (FVisiBands[FDraggedBandIndex].FTop = FVisiBands[aBand].FTop) then begin  //the same row
                FVisiBands[FDraggedBandIndex].FBreak := False;
                FVisiBands[FDraggedBandIndex].Index := CalculateRealIndex(aBand);
              end else begin  //other row
                if (not FVertical) and (FVisiBands[FDraggedBandIndex].FLeft > cBorderWidth) then
                  FVisiBands[aBand].FBreak := False;
                if (FVisiBands[FDraggedBandIndex].FLeft = cBorderWidth)
                  and (FVisiBands[aBand].FLeft = cBorderWidth)
                  and (FVertical or ((aBand-FDraggedBandIndex) = 1)
                  or (length(FVisiBands) = (aBand+1))
                  or (FVisiBands[aBand+1].FLeft = cBorderWidth)) then
                    FVisiBands[FDraggedBandIndex].Index := CalculateRealIndex(aBand)
                else
                  FVisiBands[FDraggedBandIndex].Index := CalculateRealIndex(aBand-1);
                if FDraggedBandIndex = (aBand-1) then needRecalc := True;
              end;
            end else begin  //new row
              FVisiBands[FDraggedBandIndex].FBreak := True;
              FVisiBands[FDraggedBandIndex].Index := CalculateRealIndex(aBand);
            end;
          end;
        end;
      end else
        if newRow then begin  //last Band in last row moved to new row
          FVisiBands[aBand].FBreak := True;
          needRecalc:= True;
        end;
      if needRecalc then begin  //necessary only when no Index is changed
        CalculateAndAlign;
        Invalidate;
      end;
    end;
  end;
  if FDragBand > dbNone then begin
    Cursor := FDefCursor;
    FDragBand := dbNone;
  end;
end;

procedure TCustomCoolBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if csDestroying in ComponentState then Exit;
  if Operation = opRemove then begin
    //DebugLn('TCoolBar.Notification: Operation = opRemove');
    if AComponent = FImages then Images := nil;
  end;
end;

procedure TCustomCoolBar.Paint;
var i, x, aCountM1, aLeft, aTop: Integer;
    aRowEnd, aRaisedBevel: Boolean;
    aColor: TColor;
    aDetails, aGrabDetails: TThemedElementDetails;
    aFlags: Cardinal;
    aRect: TRect;

const arBevel: array[False..True] of TColor = (clBtnShadow, clBtnHighlight);

  procedure PaintGrabber(aRect: TRect);
  var l, w: SmallInt;
  begin
    case FGrabStyle of
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
        Canvas.Line(aRect.Left, aRect.Top, aRect.Left+w, aRect.Top);
        Canvas.Line(aRect.Left, aRect.Top, aRect.Left, aRect.Bottom+1);
        Canvas.Line(aRect.Right-w, aRect.Top, aRect.Right, aRect.Top);
        Canvas.Line(aRect.Right-w, aRect.Top, aRect.Right-w, aRect.Bottom+1);
        Canvas.Pen.Color := clBtnShadow;
        Canvas.Line(aRect.Left, aRect.Bottom, aRect.Left+w, aRect.Bottom);
        Canvas.Line(aRect.Left+w, aRect.Top, aRect.Left+w, aRect.Bottom+1);
        Canvas.Line(aRect.Right-w, aRect.Bottom, aRect.Right, aRect.Bottom);
        Canvas.Line(aRect.Right, aRect.Top, aRect.Right, aRect.Bottom+1);
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
    Canvas.Pen.Color := arBevel[aRaisedBevel];
    Canvas.Line(1, Y, ClientWidth-2, Y);
    inc(Y);
    Canvas.Pen.Color := arBevel[not aRaisedBevel];
    Canvas.Line(2, Y, ClientWidth-2, Y);
  end;

begin
  inherited Paint;
  //DebugLn('TCoolBar.Paint');
  //Draw Bitmap Background
  if FBitmap.Width > 0 then DrawTiledBitmap(ClientRect, FBitmap);
  aCountM1 := length(FVisiBands)-1;
  if aCountM1 >= 0 then begin
    if FBandBorderStyle = bsSingle then
      aRaisedBevel := ((EdgeInner = esLowered) and (EdgeOuter = esRaised));
    aRowEnd := False;
    case GrabStyle of
      gsGripper: aGrabDetails := ThemeServices.GetElementDetails(trGripper);
      gsButton: aGrabDetails := ThemeServices.GetElementDetails(tbPushButtonDisabled);
    end;
    if FShowText or assigned(FImages) then begin
      if IsEnabled then
        aDetails := ThemeServices.GetElementDetails(tbPushButtonNormal)
      else
        aDetails := ThemeServices.GetElementDetails(tbPushButtonDisabled);
      aFlags := DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX;
      if IsRightToLeft then aFlags := aFlags or DT_RTLREADING;
    end;
    for i := 0 to aCountM1 do
    begin
      aLeft := FVisiBands[i].FLeft;
      aTop := FVisiBands[i].FTop;
      aRect := Rect(aLeft, aTop, aLeft+FVisiBands[i].FRealWidth+1, aTop+FVisiBands[i].FHeight);
      //Paint Band Background
      if FVisiBands[i].Bitmap.Width > 0 then begin
        DrawTiledBitmap(aRect, FVisiBands[i].Bitmap);
      end else begin
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
      //Paint a Grabber
      x := aLeft+2;
      PaintGrabber(Rect(x, aTop+2, x+GrabWidth-1, aTop+FVisiBands[i].FHeight-3));
      //Paint Image
      x := aLeft+GrabWidth+2+TCoolBand.cHorSpacing;
      if assigned(FImages) and (FVisiBands[i].ImageIndex >= 0) then begin
        ThemeServices.DrawIcon(Canvas, aDetails,
          Point(x, aTop+(FVisiBands[i].FHeight-FImages.Height) div 2),
          FImages, FVisiBands[i].ImageIndex);
        inc(x, FImages.Width+TCoolBand.cHorSpacing);
      end;
      //Paint Text
      if FShowText then begin
        aRect := Rect(x, aTop, x+FVisiBands[i].Width, aTop+FVisiBands[i].FHeight);
        ThemeServices.DrawText(Canvas, aDetails, FVisiBands[i].Text, aRect, aFlags, 0);
      end;
      // Paint a Separator border below the row of bands ____
      inc(aLeft, FVisiBands[i].Width);
      aRowEnd := IsRowEnd(aLeft, i);
      if (aRowEnd or ((i = aCountM1) and not AutoSize) or (Align in [alLeft, alRight]))
        and (FBandBorderStyle = bsSingle)
        then PaintSeparator(aTop+FVisiBands[i].FHeight);
      if not aRowEnd and (i < aCountM1) and (FBandBorderStyle = bsSingle) then begin
        //Paint Divider |
        Canvas.Pen.Color := arBevel[not aRaisedBevel];
        Canvas.Line(aLeft-1, aTop+1, aLeft-1, aTop+FVisiBands[i].FHeight-1);
        Canvas.Pen.Color := arBevel[aRaisedBevel];
        Canvas.Line(aLeft-2, aTop+1, aLeft-2, aTop+FVisiBands[i].FHeight-1);
      end;
    end;
  end;
end;

procedure TCustomCoolBar.Resize;
var aWidth, aHeight: Integer;
begin
  //DebugLn('Resize');
  inherited Resize;
  aWidth := Width;
  aHeight := Height;
  if ((aWidth <> FPrevWidth) or (aHeight <> FPrevHeight))
    and (aWidth*aHeight > 0) and HandleAllocated then
    begin
      //DebugLn('Resize calls CalcAndAlign');
      CalculateAndAlign;
      Invalidate;  //Required by GTK2
    end;
end;

