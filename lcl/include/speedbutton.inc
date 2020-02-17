{%MainUnit ../buttons.pp}

{******************************************************************************
                                   TCustomSpeedButton
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

const
  UpState: array[Boolean] of TButtonState =
  (
{False} bsUp, // mouse in control = false
{True } bsHot // mouse in contorl = true
  );

{------------------------------------------------------------------------------
  Method:  TCustomSpeedButton.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCustomSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyph := TButtonGlyph.Create;
  FGlyph.IsDesigning := csDesigning in ComponentState;
  FGlyph.ShowMode := gsmAlways;
  FGlyph.SetTransparentMode(gtmTransparent);
  FGlyph.OnChange := @GlyphChanged;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  ControlStyle := ControlStyle + [csCaptureMouse]-[csSetCaption, csClickEvents, csOpaque];

  FLayout := blGlyphLeft;
  FAllowAllUp := False;
  FMouseInControl := False;
  FDragging := False;
  FShowAccelChar := True;
  FSpacing := 4;
  FMargin := -1;
  Color := clBtnFace;
  FShowCaption := true;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCustomSpeedButton.Destroy;
begin
  FreeAndNil(FGlyph);
  FreeAndNil(FImageChangeLink);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.FindDownButton: TCustomSpeedButton;

  Searches the speed button with Down=true and the same GroupIndex.
 ------------------------------------------------------------------------------}
function TCustomSpeedButton.FindDownButton: TCustomSpeedButton;

  function FindDown(AWinControl: TWinControl): TCustomSpeedButton;
  var
    i: Integer;
    Child: TControl;
    Button: TCustomSpeedButton;
  begin
    if AWinControl = nil then Exit(nil);
    for i := 0 to AWinControl.ControlCount-1 do
    begin
      Child := AWinControl.Controls[i];
      if Child is TCustomSpeedButton then
      begin
        Button := TCustomSpeedButton(Child);
        if (Button.GroupIndex=GroupIndex) and (Button.Down) then
          Exit(Button);
      end;
      if Child is TWinControl then
      begin
        Result := FindDown(TWinControl(Child));
        if Result <> nil then Exit;
      end;
    end;
    Result := nil;
  end;

begin
  if Down or (GroupIndex=0) then exit(Self);
  Result := FindDown(GetFirstParentForm(Self));
end;

procedure TCustomSpeedButton.Click;
begin
  inherited Click;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetAllowAllUp
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetAllowAllUp(Value : Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetDown
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetDown(Value : Boolean);
var
  OldState: TButtonState;
  OldDown: Boolean;
begin
  //since Down needs GroupIndex, then we need to wait that all properties
  //loaded before we continue
  if (csLoading in ComponentState) then
  begin
    FDownLoaded := Value;
    exit;
  end else
  begin
    if FGroupIndex = 0 then
      Value:= false;
    if FDown <> Value then
    begin
      if FDown and not FAllowAllUp then
        Exit;
      OldDown := FDown;
      FDown := Value;
      OldState := FState;
      if FDown then
        FState := bsExclusive
      else
        FState := UpState[FMouseInControl];
      if (OldDown <> FDown) or (OldState <> FState) then
        Invalidate;
      if Value then
        UpdateExclusive;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetFlat
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then 
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetGlyph
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetGlyph(Value : TBitmap);
begin
  FGlyph.Glyph := Value;
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetGroupIndex
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetGroupIndex(const Value : Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TCustomSpeedButton.SetImageIndex(const aImageIndex: TImageIndex);
begin
  FGlyph.ExternalImageIndex := aImageIndex;
end;

procedure TCustomSpeedButton.SetImages(const aImages: TCustomImageList);
begin
  if FGlyph.ExternalImages <> nil then
  begin
    FGlyph.ExternalImages.UnRegisterChanges(FImageChangeLink);
    FGlyph.ExternalImages.RemoveFreeNotification(Self);
  end;
  FGlyph.ExternalImages := aImages;
  if FGlyph.ExternalImages <> nil then
  begin
    FGlyph.ExternalImages.FreeNotification(Self);
    FGlyph.ExternalImages.RegisterChanges(FImageChangeLink);
  end;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomSpeedButton.SetImageWidth(const aImageWidth: Integer);
begin
  FGlyph.ExternalImageWidth := aImageWidth;
  InvalidatePreferredSize;
  AdjustSize;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetMargin
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetMargin(const Value: integer);
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetNumGlyphs
  Params: Value : Integer = Number of glyphs in the file/resource
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetNumGlyphs(Value : integer);
begin
  if Value < Low(TNumGlyphs) then Value := Low(TNumGlyphs);
  if Value > High(TNumGlyphs) then Value := High(TNumGlyphs);

  if Value <> TButtonGlyph(fGlyph).NumGlyphs then
  begin
    TButtonGlyph(fGlyph).NumGlyphs := TNumGlyphs(Value);
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetSpacing
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetSpacing(const Value: integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TCustomSpeedButton.SetShowAccelChar(Value: boolean);
begin
  If FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;


{------------------------------------------------------------------------------
  procedure TCustomSpeedButton.RealSetText(const Value: TCaption);
 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.RealSetText(const Value: TCaption);
begin
  if Caption = Value then Exit;
  if (Parent<>nil) and (Parent.HandleAllocated) and (not (csLoading in ComponentState)) then
  begin
    InvalidatePreferredSize;
    inherited RealSetText(Value);
    AdjustSize;
  end else
    inherited RealSetText(Value);
  
  Invalidate;
end;

{------------------------------------------------------------------------------
  procedure TCustomSpeedButton.UpdateState(InvalidateOnChange: boolean);
 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.UpdateState(InvalidateOnChange: boolean);
var
  OldState: TButtonState;
begin
  OldState := FState;
  if not IsEnabled then
  begin
    FState := bsDisabled;
    FDragging := False;
  end else
  begin
    if FState = bsDisabled then
    begin
      if FDown and (GroupIndex <> 0) then
        FState := bsExclusive
      else
        FState := UpState[FMouseInControl];
    end
    else
    if (FState in [bsHot, bsDown]) and (not FMouseInControl) and (not FDragging) and (not FDown) then
    begin
      // return to normal
      FState := bsUp;
    end
    else
    if (FState = bsUp) and FMouseInControl then
      FState := bsHot;
  end;
  if FState <> OldState then
    if (Action is TCustomAction) then
      TCustomAction(Action).Checked := FState = bsDown;
  //if InvalidateOnChange then DebugLn(['TCustomSpeedButton.UpdateState ',DbgSName(Self),' InvalidateOnChange=',InvalidateOnChange,' StateChange=',FState<>OldState]);
  if InvalidateOnChange and
     (
       (FState <> OldState) or
       not ThemedElementDetailsEqual(FLastDrawDetails, GetDrawDetails)
     )
  then
    Invalidate;
end;

{------------------------------------------------------------------------------
  function TCustomSpeedButton.GetDrawDetails: TThemedElementDetails;
 ------------------------------------------------------------------------------}
function TCustomSpeedButton.GetDrawDetails: TThemedElementDetails;

  function ButtonPart: TThemedButton;
  begin
    // tbPushButtonNormal, tbPushButtonHot, tbPushButtonPressed,
    // tbPushButtonDisabled, tbPushButtonDefaulted
    
    // no check states available
    Result := tbPushButtonNormal;
    if not IsEnabled then
      Result := tbPushButtonDisabled
    else
    if FState in [bsDown, bsExclusive] then
      Result := tbPushButtonPressed
    else
    if FState = bsHot then
      Result := tbPushButtonHot
    else
      Result := tbPushButtonNormal;
  end;

  function ToolButtonPart: TThemedToolBar;
  begin
    // ttbButtonNormal, ttbButtonHot, ttbButtonPressed, ttbButtonDisabled
    // ttbButtonChecked, ttbButtonCheckedHot
    if not IsEnabled then
      Result := ttbButtonDisabled
    else
    begin
      if Down then
      begin // checked states
        if FMouseInControl then
          Result := ttbButtonCheckedHot
        else
          Result := ttbButtonChecked;
      end
      else
      begin
        if FState in [bsDown, bsExclusive] then
          Result := ttbButtonPressed else
        if FState = bsHot then
          Result := ttbButtonHot
        else
          Result := ttbButtonNormal;
      end;
    end;
  end;
  
begin
  if Flat then
    Result := ThemeServices.GetElementDetails(ToolButtonPart)
  else
    Result := ThemeServices.GetElementDetails(ButtonPart)
end;

procedure TCustomSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
var
  NewAct: TCustomAction;
  Imgs: TCustomImageList;
  ImgRes: TScaledImageListResolution;
begin
  inherited ActionChange(Sender,CheckDefaults);
  if Sender is TCustomAction then
  begin
    NewAct := TCustomAction(Sender);
    if (not CheckDefaults) or (GroupIndex = 0) then
      GroupIndex := NewAct.GroupIndex;
    if (NewAct.ActionList = nil) or (NewAct.ImageIndex < 0) then Exit;
    Imgs := NewAct.ActionList.Images;
    if (Imgs = nil) or (NewAct.ImageIndex >= Imgs.Count) then Exit;
    ImgRes := Imgs.ResolutionForPPI[ImageWidth,Font.PixelsPerInch,GetCanvasScaleFactor];
    ImgRes.GetBitmap(NewAct.ImageIndex, Glyph);
  end;
end;

function TCustomSpeedButton.ButtonGlyph: TButtonGlyph;
begin
  Result := FGlyph;
end;

function TCustomSpeedButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TSpeedButtonActionLink;
end;

class function TCustomSpeedButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 23;
  Result.CY := 22;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.UpdateExclusive
  Params: none
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.UpdateExclusive;
var
  msg : TLMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) and (not (csLoading in ComponentState)) then
  begin
    Msg.Msg := CM_ButtonPressed;
    Msg.WParam := FGroupIndex;
    Msg.LParam := PtrInt(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

{------------------------------------------------------------------------------
  Function: TCustomSpeedButton.GetGlyph
  Params: none
  Returns:  The bitmap

 ------------------------------------------------------------------------------}
function TCustomSpeedButton.GetGlyph : TBitmap;
begin
  Result := FGlyph.Glyph;
end;

function TCustomSpeedButton.IsGlyphStored: Boolean;
var
  act: TCustomAction;
begin
  result := true;
  if Action <> nil then
  begin
    act := TCustomAction(Action);
    if (act.ActionList <> nil) and (act.ActionList.Images <> nil) and
    (act.ImageIndex >= 0) and (act.ImageIndex < act.ActionList.Images.Count) then
      result := false;
  end;
end;

procedure TCustomSpeedButton.SetShowCaption(const AValue: boolean);
begin
  if FShowCaption=AValue then exit;
  FShowCaption:=AValue;
  invalidate;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.GetNumGlyphs
  Params: none
  Returns:  The number stored in TButtonGlyph(FGlyph).NumGlyphs

 ------------------------------------------------------------------------------}
function TCustomSpeedButton.GetNumGlyphs : Integer;
Begin
  Result :=  TButtonGlyph(fGlyph).NumGlyphs;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.GlyphChanged
  Params: Sender - The glyph that changed
  Returns:  zippo

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.GlyphChanged(Sender : TObject);
Begin
  //redraw the button;
  Invalidate;
end;

procedure TCustomSpeedButton.ImageListChange(Sender: TObject);
begin
  if Sender = Images then Invalidate;
end;

function TCustomSpeedButton.DialogChar(var Message: TLMKey): boolean;
begin
  Result := False;
  // Sometimes LM_CHAR is received instead of LM_SYSCHAR, maybe intentionally
  // (LCL handles it) or maybe sent by mistake. In either case exit.
  if (Message.Msg <> LM_SYSCHAR) or not FShowAccelChar then Exit;
  if Enabled and IsAccel(Message.CharCode, Caption) then
  begin
    Result := True;
    if GroupIndex <> 0 then
      SetDown(not FDown);
    Click;
  end else
    Result := inherited DialogChar(Message);
end;

procedure TCustomSpeedButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  r: TRect;
begin
  r:=Rect(0,0,0,0);
  MeasureDraw(false,r,PreferredWidth,PreferredHeight);
end;

procedure TCustomSpeedButton.MeasureDraw(Draw: boolean;
  PaintRect: TRect; out PreferredWidth, PreferredHeight: integer);
var
  GlyphWidth, GlyphHeight: Integer;
  Offset, OffsetCap: TPoint;
  ClientSize, TotalSize, TextSize, GlyphSize: TSize;
  M, S : integer;
  SIndex : Longint;
  TMP : String;
  TextFlags: Integer;
  DrawDetails: TThemedElementDetails;
  FixedWidth: Boolean;
  FixedHeight: Boolean;
  TextRect: TRect;
  HasGlyph: Boolean;
  HasText: Boolean;
  CurLayout: TButtonLayout;
  SysFont: TFont;
begin
  if FGlyph = nil then exit;

  DrawDetails := GetDrawDetails;

  PreferredWidth:=0;
  PreferredHeight:=0;

  if Draw then begin
    FLastDrawDetails:=DrawDetails;
    PaintBackground(PaintRect);
    FixedWidth:=true;
    FixedHeight:=true;
  end else begin
    FixedWidth:=WidthIsAnchored;
    FixedHeight:=HeightIsAnchored;
  end;
  ClientSize.cx:= PaintRect.Right - PaintRect.Left;
  ClientSize.cy:= PaintRect.Bottom - PaintRect.Top;
  //debugln(['TCustomSpeedButton.MeasureDraw Step1 ',DbgSName(Self),' PaintRect=',dbgs(PaintRect)]);
  // compute size of glyph
  GlyphSize := GetGlyphSize(Draw,PaintRect);
  GlyphWidth := GlyphSize.CX;
  GlyphHeight := GlyphSize.CY;
  HasGlyph:=(GlyphWidth<>0) and (GlyphHeight<>0);
  //debugln(['TCustomSpeedButton.MeasureDraw Step2 ',DbgSName(Self),' PaintRect=',dbgs(PaintRect),' GlyphSize=',GlyphWidth,'x',GlyphHeight]);

  // compute size of text
  CurLayout:=BidiAdjustButtonLayout(UseRightToLeftReading, Layout);
  if ShowCaption and (Caption<>'') then begin
    TextRect:=PaintRect;
    // for wordbreak compute the maximum size for the text
    if Margin>0 then
      InflateRect(TextRect,-Margin,-Margin);
    if HasGlyph then
    begin
      if (Spacing>=0) then
        if CurLayout in [blGlyphLeft,blGlyphRight] then
          dec(TextRect.Right,Spacing)
        else
          dec(TextRect.Bottom,Spacing);
      if CurLayout in [blGlyphLeft,blGlyphRight] then
        dec(TextRect.Right,GlyphWidth)
      else
        dec(TextRect.Bottom,GlyphHeight);
    end;
    if not FixedWidth then
    begin
      TextRect.Left:=0;
      TextRect.Right:=High(TextRect.Right) div 2;
    end;
    if not FixedHeight then
    begin
      TextRect.Top:=0;
      TextRect.Bottom:=High(TextRect.Bottom) div 2;
    end;
    TextSize := GetTextSize(Draw,TextRect);
  end else begin
    TextSize.cx:=0;
    TextSize.cy:=0;
  end;
  HasText:=(TextSize.cx <> 0) or (TextSize.cy <> 0);

  if Caption <> '' then
  begin
    TMP := Caption;
    SIndex := DeleteAmpersands(TMP);
    If SIndex > 0 then
      If SIndex <= Length(TMP) then begin
        FShortcut := Ord(TMP[SIndex]);
      end;
  end;

  if HasGlyph and HasText then
    S:= Spacing
  else
    S:= 0;
  M:=Margin;
  if not Draw then
  begin
    if M<0 then M:=2;
    if S<0 then S:=M;
  end;

  // Calculate caption and glyph layout
  if M = -1 then begin
    // auto compute margin to center content
    if S = -1 then begin
      // use the same value for Spacing and Margin
      TotalSize.cx:= TextSize.cx + GlyphWidth;
      TotalSize.cy:= TextSize.cy + GlyphHeight;
      if Layout in [blGlyphLeft, blGlyphRight] then
        M:= (ClientSize.cx - TotalSize.cx) div 3
      else
        M:= (ClientSize.cy - TotalSize.cy) div 3;
      S:= M;
    end else begin
      // fixed Spacing and center content
      TotalSize.cx:= GlyphWidth + S + TextSize.cx;
      TotalSize.cy:= GlyphHeight + S + TextSize.cy;
      if Layout in [blGlyphLeft, blGlyphRight] then
        M:= (ClientSize.cx - TotalSize.cx) div 2
      else
        M:= (ClientSize.cy - TotalSize.cy) div 2;
    end;
  end else begin
    // fixed Margin
    if S = -1 then begin
      // use the rest for Spacing between Glyph and Caption
      TotalSize.cx:= ClientSize.cx - (Margin + GlyphWidth);
      TotalSize.cy:= ClientSize.cy - (Margin + GlyphHeight);
      if Layout in [blGlyphLeft, blGlyphRight] then
        S:= (TotalSize.cx - TextSize.cx) div 2
      else
        S:= (TotalSize.cy - TextSize.cy) div 2;
    end;
  end;

  //debugln(['TCustomSpeedButton.MeasureDraw Step3 ',DbgSName(Self),' PaintRect=',dbgs(PaintRect),' GlyphSize=',GlyphWidth,'x',GlyphHeight,' TextSize=',TextSize.cx,'x',TextSize.cy,' S=',S,' M=',M]);

  if Draw then
  begin
    case CurLayout of
      blGlyphLeft : begin
        Offset.X:= M;
        Offset.Y:= (ClientSize.cy - GlyphHeight) div 2;
        OffsetCap.X:= Offset.X + GlyphWidth + S;
        OffsetCap.Y:= (ClientSize.cy - TextSize.cy) div 2;
      end;
      blGlyphRight : begin
        Offset.X:= ClientSize.cx - M - GlyphWidth;
        Offset.Y:= (ClientSize.cy - GlyphHeight) div 2;
        OffsetCap.X:= Offset.X - S - TextSize.cx;
        OffsetCap.Y:= (ClientSize.cy - TextSize.cy) div 2;
      end;
      blGlyphTop : begin
        Offset.X:= (ClientSize.cx - GlyphWidth) div 2;
        Offset.Y:= M;
        OffsetCap.X:= (ClientSize.cx - TextSize.cx) div 2;
        OffsetCap.Y:= Offset.Y + GlyphHeight + S;
      end;
      blGlyphBottom : begin
        Offset.X:= (ClientSize.cx - GlyphWidth) div 2;
        Offset.Y:= ClientSize.cy - M - GlyphHeight;
        OffsetCap.X:= (ClientSize.cx - TextSize.cx) div 2;
        OffsetCap.Y:= Offset.Y - S - TextSize.cy;
      end;
    end;

    DrawGlyph(Canvas, PaintRect, Offset, FState, Transparent, 0);

    if FShowCaption and (Caption <> '') then
    begin
      with PaintRect, OffsetCap do
      begin
        Left := Left + X;
        Top := Top + Y;
      end;

      TextFlags := DT_LEFT or DT_TOP;
      if UseRightToLeftReading then
        TextFlags := TextFlags or DT_RTLREADING;

      if Draw then
      begin
        SysFont := Screen.SystemFont;
        if  (SysFont.Color=Font.Color)
        and ((SysFont.Name=Font.Name) or IsFontNameDefault(Font.Name))
        and (SysFont.Pitch=Font.Pitch)
        and (SysFont.Style=Font.Style) then
          ThemeServices.DrawText(Canvas, DrawDetails, Caption, PaintRect, TextFlags, 0)
        else
        begin
          Canvas.Brush.Style := bsClear;
          DrawText(Canvas.Handle, PChar(Caption), Length(Caption), PaintRect, TextFlags);
        end;
      end;
    end;
  end else begin
    // measuring, not drawing
    case CurLayout of
      blGlyphLeft, blGlyphRight :
        begin
          PreferredWidth:=2*M+S+GlyphWidth+TextSize.cx;
          PreferredHeight:=2*M+Max(GlyphHeight,TextSize.cy);
        end;
      blGlyphTop, blGlyphBottom :
        begin
          PreferredWidth:=2*M+Max(GlyphWidth,TextSize.cx);
          PreferredHeight:=2*M+S+GlyphHeight+TextSize.cy;
        end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.Paint
  Params: none
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.Paint;
var
  PaintRect: TRect;
  PreferredWidth: integer;
  PreferredHeight: integer;
begin
  UpdateState(false);
  if FGlyph = nil then exit;

  PaintRect:=ClientRect;
  MeasureDraw(true,PaintRect,PreferredWidth,PreferredHeight);

  inherited Paint;
end;

procedure TCustomSpeedButton.PaintBackground(var PaintRect: TRect);
begin
  if not Transparent and ThemeServices.HasTransparentParts(FLastDrawDetails) then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(PaintRect);
  end;
  ThemeServices.DrawElement(Canvas.Handle, FLastDrawDetails, PaintRect);
  PaintRect := ThemeServices.ContentRect(Canvas.Handle, FLastDrawDetails, PaintRect);
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.MouseDown
  Params: Button:
          Shift:
          X, Y:
  Returns:  nothing
 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if csDesigning in ComponentState then exit;

  if (Button = mbLeft) and IsEnabled then
  begin
    if not FDown then
    begin
      FState := bsDown;
      if (Action is TCustomAction) then
        TCustomAction(Action).Checked := False;
      Invalidate;
    end;
    FDragging := True;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.MouseMove
  Params: Shift:
          X, Y:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if csDesigning in ComponentState then exit;

  if FDragging then
  begin
    //DebugLn('Trace:FDragging is true');
    if FDown then
      NewState := bsExclusive
    else
    begin
      if  (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
        NewState := bsDown
      else
        NewState := UpState[FMouseInControl];
    end;

    if NewState <> FState then
    begin
      //debugln(['TCustomSpeedButton.MouseMove ',DbgSName(Self),' fState=',ord(fstate),' NewState=',ord(NewState)]);
      FState := NewState;
      Invalidate;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.MouseUp
  Params: Button:
          Shift:
          X, Y:
  Returns:  nothing
 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomSpeedButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FGlyph<>nil) and (AComponent = FGlyph.ExternalImages) then
    Images := nil;
end;

{------------------------------------------------------------------------------
       TCustomSpeedButton DoMouseUp  "Event Handler"
------------------------------------------------------------------------------}
procedure TCustomSpeedButton.DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
begin
  if not (csNoStdEvents in ControlStyle) then
    with Message do
      MouseUp(Button, KeysToShiftState(Keys), XPos, YPos);
end;

procedure TCustomSpeedButton.WMLButtonDown(var Message: TLMLButtonDown);
begin
  inherited;
  
  // because csClickEvents is not set no csClicked is set in the inherited method
  Include(FControlState, csClicked);
end;

procedure TCustomSpeedButton.WMLButtonDBLCLK(var Message: TLMLButtonDblClk);
begin
  inherited;
  // if in a group, raise dblclick event, otherwise translate to click event
  if Down then
    DblClick
  else
    Click;
end;

class procedure TCustomSpeedButton.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomSpeedButton;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.WMLButtonUp
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.WMLButtonUp(var Message: TLMLButtonUp);
var
  OldState: TButtonState;
  NeedClick: Boolean;
begin
  //DebugLn('TCustomSpeedButton.WMLButtonUp A ',DbgSName(Self),' csCaptureMouse=',DbgS(csCaptureMouse in ControlStyle),' csClicked=',DbgS(csClicked in ControlState));
  if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TCustomSpeedButton.WMLButtonUp ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := False;
  end;

  NeedClick := False;

  if not (csDesigning in ComponentState) and FDragging then
  begin
    OldState := FState;
    FDragging := False;

    if FGroupIndex = 0 then
    begin
      FState := UpState[FMouseInControl];
      if OldState <> FState then
        Invalidate;
    end
    else
    if (Message.XPos >= 0) and (Message.XPos < Width) and (Message.YPos >= 0) and (Message.YPos < Height) then
    begin
      SetDown(not FDown);
      NeedClick := True;
    end;
  end;

  DoMouseUp(Message, mbLeft);

  if csClicked in ControlState then
  begin
    Exclude(FControlState, csClicked);
    //DebugLn('TCustomSpeedButton.WMLButtonUp B ',dbgs(ClientRect.Left),',',dbgs(ClientRect.Top),',',dbgs(ClientRect.Right),',',dbgs(ClientRect.Bottom),' ',dbgs(Message.Pos.X),',',dbgs(Message.Pos.Y));
    if PtInRect(ClientRect, SmallPointToPoint(Message.Pos)) then
    begin
      //DebugLn('TCustomSpeedButton.WMLButtonUp C');
      // Important: Calling Click can invoke modal dialogs, so call this as last
      NeedClick := False;
      Click;
    end;
  end;
  
  if NeedClick then
    Click;
  //DebugLn('TCustomSpeedButton.WMLButtonUp END');
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetLayout
  Params: Value: new layout value
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetLayout(const Value : TButtonLayout);
begin
  if Value <> FLayout then
  begin
    FLayout:= Value;
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetTransparent
  Params: Value: new transparency value
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetTransparent(const AValue: boolean);
const
  MODE: array[Boolean] of TGlyphTransparencyMode = (gtmOpaque, gtmTransparent);
begin
  if AValue = Transparent then Exit;

  if AValue then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];

  FGlyph.SetTransparentMode(MODE[AValue]);
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.CMButtonPressed
  Params: Message:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.CMButtonPressed(var Message : TLMessage);
var
  Sender : TCustomSpeedButton;
begin
  if csDestroying in ComponentState then exit;
  if Message.WParam = WParam(FGroupIndex) then
  begin
    Sender := TCustomSpeedButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := UpState[FMouseInControl];
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TCustomSpeedButton.Loaded;
begin
  inherited Loaded;
  UpdateExclusive;
  if FDownLoaded then
    SetDown(FDownLoaded);
end;

procedure TCustomSpeedButton.LoadGlyphFromResourceName(Instance: THandle; const AName: String);
begin
  Buttons.LoadGlyphFromResourceName(FGlyph, Instance, AName);
end;

procedure TCustomSpeedButton.LoadGlyphFromLazarusResource(const AName: String);
begin
  Buttons.LoadGlyphFromLazarusResource(FGlyph, AName);
end;

function TCustomSpeedButton.GetGlyphSize(Drawing: boolean; PaintRect: TRect): TSize;
var
  AImageRes: TScaledImageListResolution;
  AIndex: Integer;
  AEffect: TGraphicsDrawEffect;
begin
  FGlyph.GetImageIndexAndEffect(Low(TButtonState), Font.PixelsPerInch,
    GetCanvasScaleFactor, AImageRes, AIndex, AEffect);
  Result.CX := AImageRes.Width;
  Result.CY := AImageRes.Height;
end;

function TCustomSpeedButton.GetImageIndex: TImageIndex;
begin
  Result := FGlyph.ExternalImageIndex;
end;

function TCustomSpeedButton.GetImages: TCustomImageList;
begin
  Result := FGlyph.ExternalImages;
end;

function TCustomSpeedButton.GetImageWidth: Integer;
begin
  Result := FGlyph.ExternalImageWidth;
end;

function TCustomSpeedButton.GetTextSize(Drawing: boolean; PaintRect: TRect): TSize;
var
  TMP: String;
  Flags: Cardinal;
  DC: HDC;           // ~bk see : TCustomLabel.CalculateSize
  OldFont: HGDIOBJ;  //               "
begin
  if FShowCaption and (Caption <> '') then
  begin
    TMP := Caption;
    DeleteAmpersands(TMP);
    Flags := DT_CalcRect;
    if not Canvas.TextStyle.SingleLine then
      Inc(Flags, DT_WordBreak);
    DC := GetDC(Parent.Handle);
    try
      OldFont := SelectObject(DC, HGDIOBJ(Font.Reference.Handle));
      DrawText(DC, PChar(TMP), Length(TMP), PaintRect, Flags);
      SelectObject(DC, OldFont);
    finally
      ReleaseDC(Parent.Handle, DC);
    end;
    Result.CY := PaintRect.Bottom - PaintRect.Top;
    Result.CX := PaintRect.Right - PaintRect.Left;
  end
  else
  begin
    Result.CY:= 0;
    Result.CX:= 0;
  end;
end;

function TCustomSpeedButton.GetTransparent: Boolean;
begin
  if FGlyph.TransparentMode = gtmGlyph then
    Result := FGlyph.FOriginal.Transparent
  else
    Result := FGlyph.TransparentMode = gtmTransparent;
end;

function TCustomSpeedButton.DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
  const AOffset: TPoint; AState: TButtonState; ATransparent: Boolean;
  BiDiFlags: Longint): TRect;
begin
  if Assigned(FGlyph) then
    Result := FGlyph.Draw(ACanvas, AClient, AOffset, AState, ATransparent, BiDiFlags,
      Font.PixelsPerInch, GetCanvasScaleFactor)
  else
    Result := Rect(0,0,0,0);
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.CMEnabledChanged
  Params: Message:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.CMEnabledChanged(var Message: TLMessage);
Begin
  //Should create a new glyph based on the new state
  UpdateState(true);
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.MouseEnter
  Params:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.MouseEnter;
begin
  if csDesigning in ComponentState then exit;
  if not FMouseInControl and IsEnabled and (GetCapture = 0) then
  begin
    FMouseInControl := True;
    UpdateState(true);
    inherited MouseEnter;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.MouseLeave
  Params:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.MouseLeave;
begin
  if csDesigning in ComponentState then exit;
  ///DebugLn(['TCustomSpeedButton.MouseLeave ',DbgSName(Self),' FMouseInControl=',FMouseInControl,' FDragging=',FDragging]);
  if FMouseInControl then
  begin
    FMouseInControl := False;
    if IsEnabled then
    begin
      if FDragging and (not MouseCapture) then
      begin
        // something fetched our mouse capture
        FDragging:=false;
      end;
      UpdateState(true);
      inherited MouseLeave;
    end;
  end;
end;

{ TSpeedButtonActionLink }

procedure TSpeedButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TCustomSpeedButton;
end;

function TSpeedButtonActionLink.IsCheckedLinked: Boolean;
var
  SpeedButton: TCustomSpeedButton;
begin
  SpeedButton:=TCustomSpeedButton(FClient);
  Result := inherited IsCheckedLinked
            and (SpeedButton.GroupIndex <> 0)
            and SpeedButton.AllowAllUp
            and (SpeedButton.Down = (Action as TCustomAction).Checked);
end;

function TSpeedButtonActionLink.IsGroupIndexLinked: Boolean;
var
  SpeedButton: TCustomSpeedButton;
begin
  SpeedButton:=TCustomSpeedButton(FClient);
  Result := (SpeedButton is TCustomSpeedButton) and
    (SpeedButton.GroupIndex = (Action as TCustomAction).GroupIndex);
end;

procedure TSpeedButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then TCustomSpeedButton(FClient).GroupIndex := Value;
end;

procedure TSpeedButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then TCustomSpeedButton(FClient).Down := Value;
end;


{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}


// included by buttons.pp
