{%MainUnit ../stdctrls.pp}

{******************************************************************************
                               TCustomLabel
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  TODO:

    - Enable Tabbing/Focusing to focus FocusControl
    - Enable Escaped '&' Shortcut to focus FocusControl
    - Compare/Match AutoSize to Delphi/Kylix's
    - ?? Check For Full Delphi/Kylix Compatibility
    - Support of rotated multiline/wordwrapped text.

}

const
  cMaxLabelSize = 10000;

// AAngle is in radians, counter-clockwise = positive. But: y points down!
function RotatePoint(const APoint: TPoint; AAngle: Double): TPoint;
var
  sa, ca: Double;
begin
  sa := sin(AAngle);
  ca := cos(AAngle);
  Result.X := Round( ca * APoint.X + sa * APoint.Y);
  Result.Y := Round(-sa * APoint.X + ca * APoint.Y);
end;

procedure GetMinMax(x: Integer; var min, max: Integer);
begin
  if x < min then min := x;
  if x > max then max := x;
end;

function RotateRect(AWidth, AHeight: Integer; AAngle: Double): TRect;
var
  P1, P2, P3: TPoint;
begin
  if AAngle = 0 then
    Result := Rect(0, 0, AWidth, AHeight)
  else
  begin
    P1 := RotatePoint(Point(AWidth, 0), AAngle);
    P2 := RotatePoint(Point(0, AHeight), AAngle);
    P3 := Point(P1.X+P2.X, P1.Y+P2.Y);

    Result := Rect(0, 0, 0, 0);
    GetMinMax(P1.X, Result.Left, Result.Right);
    GetMinMax(P2.X, Result.Left, Result.Right);
    GetMinMax(P3.X, Result.Left, Result.Right);
    GetMinMax(P1.Y, Result.Top, Result.Bottom);
    GetMinMax(P2.Y, Result.Top, Result.Bottom);
    GetMinMax(P3.Y, Result.Top, Result.Bottom);
  end;
end;

procedure TCustomLabel.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
// assumes: (Parent <> nil) and Parent.HandleAllocated
var
  AWidth: Integer;
  R: TRect;
  angle: Double;
begin
  if (Parent = nil) or (not Parent.HandleAllocated) then Exit;
  if WidthIsAnchored and WordWrap then
    AWidth:=Width
  else
    AWidth:=cMaxLabelSize;
  AWidth:=Constraints.MinMaxWidth(AWidth);
  CalculateSize(AWidth,PreferredWidth,PreferredHeight);

  if Font.Orientation <> 0 then
  begin
    angle := Font.Orientation / 10 * pi / 180;
    R := RotateRect(PreferredWidth, PreferredHeight, angle);
    PreferredWidth := R.Right - R.Left;
    PreferredHeight := R.Bottom - R.Top;
  end;
end;

procedure TCustomLabel.CalculateSize(MaxWidth: integer; var NeededWidth,
  NeededHeight: integer);
var
  DC, OldHandle: HDC;
  R: TRect;
  OldFont: HGDIOBJ;
  Flags: cardinal;
  LabelText: String;
begin
  LabelText := GetLabelText;
  if LabelText='' then begin
    NeededWidth:=1;
    NeededHeight:=1;
    exit;
  end;

  DC := GetDC(0);
  try
    OldHandle := Canvas.Handle;
    Canvas.Handle := DC;
    R := Rect(0, 0, MaxWidth, cMaxLabelSize);
    OldFont := SelectObject(DC, HGDIOBJ(Font.Reference.Handle));
    Flags := DT_CALCRECT or DT_EXPANDTABS;
    if WordWrap then
      Flags := Flags or DT_WORDBREAK
    else
    if not HasMultiLine then
      Flags := Flags or DT_SINGLELINE;
    if not ShowAccelChar then
      Flags := Flags or DT_NOPREFIX;
    if UseRightToLeftReading then
      Flags := Flags or DT_RTLREADING;

    DoDrawText(R, Flags);
    SelectObject(DC, OldFont);
    NeededWidth := R.Right - R.Left;
    NeededHeight := R.Bottom - R.Top;
    Canvas.Handle := OldHandle;
    //DebugLn(['TCustomLabel.CalculatePreferredSize ',DbgSName(Self),' R=',dbgs(R),' MaxWidth=',MaxWidth,' DT_WORDBREAK=',(DT_WORDBREAK and Flags)>0,' LabelText="',LabelText,'"']);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TCustomLabel.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  UpdateSize;
end;

class function TCustomLabel.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 65;
  Result.CY := 17;
end;

function TCustomLabel.HasMultiLine: boolean;
var
  s: String;
begin
  s := GetLabelText;
  Result := (pos(#10, s) > 0) or (pos(#13, s) > 0);
end;

procedure TCustomLabel.DoAutoSize;
begin
  inherited DoAutoSize;
  //debugln('TCustomLabel.DoAutoSize ',DbgSName(Self),' AutoSizing=',dbgs(AutoSizing),' AutoSize=',dbgs(AutoSize),' Parent=',DbgSName(Parent),' csLoading=',dbgs(csLoading in ComponentState),' Parnet.HandleAllocated=',dbgs((Parent<>nil) and (Parent.HandleAllocated)));
end;

procedure TCustomLabel.DoDrawText(var Rect: TRect; Flags: Longint);
var
  LabelText: string;
  OldFontColor: TColor;
  Rect2: TRect;
begin
  LabelText := GetLabelText;
  OldFontColor := Canvas.Font.Color;
  if not IsEnabled and (Flags and DT_CALCRECT = 0) then
    if ThemeServices.ThemesEnabled then
      Canvas.Font.Color := clGrayText
    else
    begin
      Canvas.Font.Color := clBtnHighlight;
      Rect2 := Rect;
      OffsetRect(Rect2, 1, 1);
      DrawText(Canvas.Handle, PChar(LabelText), Length(LabelText), Rect2, Flags);
      Canvas.Font.Color := clBtnShadow;
    end;
  DrawText(Canvas.Handle, PChar(LabelText), Length(LabelText), Rect, Flags or DT_NOCLIP);
  Canvas.Font.Color := OldFontColor;
end;

procedure TCustomLabel.SetAlignment(Value : TAlignment);
begin
  //debugln('TCustomLabel.SetAlignment Old=',dbgs(ord(Alignment)),' New=',dbgs(ord(Value)),' csLoading=',dbgs(csLoading in ComponentState));
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TCustomLabel.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FFocusControl) and (Operation = opRemove) then
    FFocusControl := nil;
end;

procedure TCustomLabel.SetFocusControl(Value : TWinControl);
begin
  if Value <> FFocusControl then
  begin
    if FFocusControl <> nil then
      FFocusControl.RemoveFreeNotification(Self);
    FFocusControl:= Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TCustomLabel.WMActivate(var Message: TLMActivate);
begin
  if (FFocusControl <> nil) and (FFocusControl.CanFocus) then
    FFocusControl.SetFocus;
end;

function TCustomLabel.GetLabelText: string;
begin
  Result := Caption;
end;

procedure TCustomLabel.SetShowAccelChar(Value : Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    Invalidate;
    UpdateSize;
  end;
end;

procedure TCustomLabel.TextChanged;
begin
  Invalidate;
  UpdateSize;
  AccessibleValue := Caption;
end;

procedure TCustomLabel.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
var
  WidthChanged: Boolean;
begin
  WidthChanged:=AWidth<>Width;
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  if OptimalFill and (not AutoSize) then
    AdjustFontForOptimalFill;
  if WidthChanged and WordWrap then begin
    InvalidatePreferredSize;
    AdjustSize;
  end;
end;

function TCustomLabel.CanTab: boolean;
begin
  Result := False;
end;

constructor TCustomLabel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := [csCaptureMouse, csSetCaption, csClickEvents, csDoubleClicks, csReplicatable];
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FShowAccelChar := True;
  FInternalSetBounds := False;
  Color := clNone;
  AutoSize := True;
  // Accessibility
  AccessibleRole := larLabel;
end;

function TCustomLabel.GetTransparent: boolean;
begin
  Result := Color = clNone;
end;

procedure TCustomLabel.SetColor(NewColor: TColor);
begin
  inherited;

  // if color = clnone then transparent, so not opaque
  if NewColor = clNone then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.SetLayout
  Params: None
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout:= Value;
    Invalidate;
  end;
end;

procedure TCustomLabel.SetTransparent(NewTransparent: boolean);
begin
  if Transparent = NewTransparent then
    exit;
  if NewTransparent then
    Color := clNone
  else
  if Color = clNone then
    Color := clBackground;
end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.SetWordWrap
  Params: None
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomLabel.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Invalidate;
    UpdateSize;
  end;
end;

function TCustomLabel.DialogChar(var Message: TLMKey): boolean;
begin
  Result := False;
  if not FShowAccelChar then exit;
  if FFocusControl = nil then exit;
  if KeyDataToShiftState(Message.KeyData) * [ssCtrl, ssAlt, ssShift] <> [ssAlt] then exit;

  if IsAccel(Message.CharCode, GetLabelText) and FFocusControl.CanFocus then
  begin
    Result := True;
    FFocusControl.SetFocus;
  end else
    Result := inherited DialogChar(Message);
end;

procedure TCustomLabel.Loaded;
begin
  inherited Loaded;
  AdjustSize;
end;

procedure TCustomLabel.UpdateSize;
var
  OldWidth, OldLeft: Integer;
begin
  OldWidth := Width;
  OldLeft := Left;
  InvalidatePreferredSize;
  if OptimalFill and (not AutoSize) then
    AdjustFontForOptimalFill;
  AdjustSize;
  if (AutoSize or OptimalFill) and (Alignment=taRightJustify)
  and (OldWidth<>Width) and (OldLeft=Left) then
    Left := Left-Width+OldWidth;
end;

{-------------------------------------------------------------------------------
  function TCustomLabel.CalcFittingFontHeight(const TheText: string;
    MaxWidth, MaxHeight: Integer;
    var FontSize, NeededWidth, NeededHeight: integer): Boolean;

  Calculates the maximum font size for TheText to fit into MaxWidth and
  MaxHeight.
-------------------------------------------------------------------------------}
function TCustomLabel.CalcFittingFontHeight(const TheText: string;
  MaxWidth, MaxHeight: Integer;
  out FontHeight, NeededWidth, NeededHeight: Integer): Boolean;
var
  R: TRect;
  DC: HDC;
  DrawFlags: UINT;
  OldFont: HGDIOBJ;
  MinFontHeight: Integer;
  MaxFontHeight: Integer;
  TestFont: TFont;
  CurFontHeight: Integer;
begin
  Result := False;
  FontHeight := 0;
  if AutoSizeDelayed or (TheText = '') or (MaxWidth < 1) or (MaxHeight < 1) then
    Exit;
  TestFont := TFont.Create;
  try
    TestFont.Assign(Font);
    MinFontHeight := 4;
    MaxFontHeight := MaxHeight * 2;
    CurFontHeight := (MinFontHeight + MaxFontHeight) div 2;
    DrawFlags := DT_CALCRECT or DT_NOPREFIX or DT_EXPANDTABS;
    if WordWrap then
      DrawFlags := DrawFlags or DT_WORDBREAK;
    R.Left := 0;
    R.Top := 0;
    DC := GetDC(Parent.Handle);
    try
      while (MinFontHeight <= MaxFontHeight) and
        (CurFontHeight >= MinFontHeight) and
        (CurFontHeight <= MaxFontHeight) do
      begin
        TestFont.Height := CurFontHeight; // NOTE: some TFont do not allow any integer
        //debugln('TCustomLabel.CalcFittingFontHeight A ',dbgs(MinFontHeight),'<=',dbgs(AFont.Height),'<=',dbgs(MaxFontHeight));
        OldFont := SelectObject(DC, HGDIOBJ(TestFont.Reference.Handle));
        R.Right := MaxWidth;
        R.Bottom := MaxHeight;
        DrawText(DC, PChar(TheText), Length(TheText), R, DrawFlags);
        SelectObject(DC, OldFont);
        NeededWidth := R.Right - R.Left;
        NeededHeight := R.Bottom - R.Top;
        //debugln('TCustomLabel.CalcFittingFontHeight B NeededWidth=',dbgs(NeededWidth),' NeededHeight=',dbgs(NeededHeight),' MaxWidth=',dbgs(MaxWidth),' MaxHeight=',dbgs(MaxHeight));
        if (NeededWidth > 0) and (NeededWidth <= MaxWidth) and (NeededHeight > 0) and (NeededHeight <= MaxHeight) then
        begin
          // TheText fits into the bounds
          if (not Result) or (FontHeight < TestFont.Height) then
            FontHeight := TestFont.Height;
          Result := True;
          MinFontHeight := CurFontHeight;
          // -> try bigger (binary search)
          CurFontHeight := (MaxFontHeight + CurFontHeight +1 ) div 2; // +1 to round up
          if CurFontHeight = MinFontHeight then
            Break;
        end
        else
        begin
          // TheText does not fit into the bounds
          MaxFontHeight := CurFontHeight - 1;
          // -> try smaller (binary search)
          CurFontHeight := (MinFontHeight + CurFontHeight) div 2;
        end;
      end;
    finally
      ReleaseDC(Parent.Handle, DC);
    end;
  finally
    TestFont.Free;
  end;
end;

function TCustomLabel.ColorIsStored: boolean;
begin
  Result := (Color <> clNone);
end;

{-------------------------------------------------------------------------------
  function TCustomLabel.AdjustFontForOptimalFill: Boolean;
  
  Maximizes Font.Height
  Return true if Font.Height changed.
-------------------------------------------------------------------------------}
function TCustomLabel.AdjustFontForOptimalFill: Boolean;
var
  NeededWidth: Integer;
  NeededHeight: Integer;
  NewFontHeight: Integer;
  OldFontHeight: LongInt;
begin
  Result := False;
  if not CalcFittingFontHeight(GetLabelText, Width, Height, NewFontHeight,
                               NeededWidth, NeededHeight) then Exit;
  if Font.Height = NewFontHeight then Exit;
  //debugln('TCustomLabel.AdjustFontForOptimalFill OldFontHeight=',dbgs(Font.Height),' NewFontHeight=',dbgs(NewFontHeight));
  OldFontHeight := Font.Height;
  Font.Height := NewFontHeight;
  Result := OldFontHeight <> Font.Height;
end;

procedure TCustomLabel.Paint;
var
  R, CalcRect: TRect;
  TextLeft, TextTop: integer;
  Flags: Longint;
  angle: Double;
const
  cAlignment: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
begin
  R := Rect(0,0,Width,Height);
  Canvas.Brush.Color := Color;
  if (Color<>clNone) and not Transparent then
  begin
    Canvas.Brush.Style:=bsSolid;
    Canvas.FillRect(R);
  end;
  Canvas.Brush.Style:=bsClear;
  Canvas.Font := Font;

  Flags := DT_EXPANDTABS;
  if WordWrap then
    Flags := Flags or DT_WORDBREAK
  else
  if not HasMultiLine then
    Flags := Flags or DT_SINGLELINE;
  if not ShowAccelChar then
    Flags := Flags or DT_NOPREFIX;
  if UseRightToLeftReading then
    Flags := Flags or DT_RTLREADING;

  CalcRect := R;
  if Font.Orientation = 0 then
  begin
    Flags := Flags or cAlignment[BidiFlipAlignment(Self.Alignment, UseRightToLeftAlignment)];
    DoDrawText(CalcRect, Flags or DT_CALCRECT);
    if FLayout<>tlTop then
    begin
      case FLayout of
        tlTop: ; // nothing
        tlCenter: OffsetRect(R, 0, (R.Height-CalcRect.Height) div 2);
        tlBottom: OffsetRect(R, 0, R.Height-CalcRect.Height)
      end;
      R.Height := CalcRect.Height;
    end;
  end else
  begin
    // Rotated text. Must be drawn as taLeftJustify/tlTop. No wordbreak and multiline ATM.
    Flags := Flags or DT_SINGLELINE and not DT_WORDBREAK;
    DoDrawText(CalcRect, Flags or DT_CALCRECT);
    angle := Font.Orientation * 0.1 * pi/180;
    CalcRect := RotateRect(CalcRect.Width, CalcRect.Height, angle);
    R := CalcRect;
    case FAlignment of
      taLeftJustify: OffsetRect(R, -CalcRect.Left, 0);
      taCenter: OffsetRect(R, (Width - CalcRect.Width) div 2 - CalcRect.Left, 0);
      taRightJustify: OffsetRect(r, Width - CalcRect.Right, 0);
    end;
    case FLayout of
      tlTop: OffsetRect(R, 0, -CalcRect.Top);
      tlCenter: OffsetRect(R, 0, (Height - CalcRect.Height) div 2 - CalcRect.Top);
      tlBottom: OffsetRect(R, 0, Height - CalcRect.Bottom);
    end;
    OffsetRect(R, -CalcRect.Left-1, -CalcRect.Top-1);
  end;
  //debugln('TCustomLabel.Paint ',dbgs(Alignment=tacenter),' ',dbgs(Layout=tlCenter),' ',dbgs(TextLeft),' TextTop=',dbgs(TextTop),' ',dbgs(R));
  DoDrawText(R, Flags);
end;

procedure TCustomLabel.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  if (Left=aLeft) and (Top=aTop) and (Width=aWidth) and (Height=aHeight) then exit;
  if not FInternalSetBounds and AutoSize and WordWrap then
    InvalidatePreferredSize;
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
end;

procedure TCustomLabel.SetOptimalFill(const AValue: Boolean);
begin
  if FOptimalFill = AValue then Exit;
  FOptimalFill := AValue;
  if OptimalFill and AutoSize then
    AutoSize := False;
  if OptimalFill then
    AdjustFontForOptimalFill;
  Invalidate;
end;

class procedure TCustomLabel.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomLabel;
  RegisterPropertyToSkip(TCustomLabel, 'EllipsisPosition', 'VCL compatibility property', '');
end;

 // included by stdctrls.pp


