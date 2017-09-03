{%MainUnit ../extctrls.pp}

{******************************************************************************
                                TCustomRadioGroup
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
{

  Delphi compatibility:

  - TPanel is compatible with Delphi implementation
}

{------------------------------------------------------------------------------
  constructor TCustomPanel.Create (TheOwner : TComponent);
 ------------------------------------------------------------------------------}
constructor TCustomPanel.Create(TheOwner : TComponent);
begin
  inherited Create (TheOwner);
  FCompStyle:= csPanel;
  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
    csClickEvents, csSetCaption, csDoubleClicks, csReplicatable,
    csNoFocus, csAutoSize0x0, csParentBackground]
    - [csOpaque]; // we need the default background
  FBevelColor := clDefault;
  FBevelOuter := bvRaised;
  FBevelInner := bvNone;
  FBevelWidth := 1;
  FAlignment := taCenter;
  FFullRepaint := True;
  Color := {$ifdef UseCLDefault}clDefault{$else}clBtnFace{$endif};
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  ParentColor := True;
  UseDockManager := True;
  // Accessibility
  AccessibleRole := larGroup;
  AccessibleDescription := rsTPanelAccessibilityDescription;
end;

procedure TCustomPanel.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TCustomPanel.SetBevelColor(AValue: TColor);
begin
  if FBevelColor <> AValue then
  begin
    FBevelColor := AValue;
    Invalidate;
  end;
end;

procedure TCustomPanel.SetBevelWidth(const Value: TBevelWidth);
begin
  if FBevelWidth <> Value then
  begin
    FBevelWidth := Value;
    Perform(CM_BORDERCHANGED, 0, 0);
  end;
end;

procedure TCustomPanel.SetParentBackground(const AParentBackground: Boolean);
begin
  if ParentBackground=AParentBackground then
    Exit;
  if AParentBackground then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  inherited;
end;

procedure TCustomPanel.SetWordwrap(const Value: Boolean);
begin
  if FWordwrap <> Value then
  begin
    FWordwrap := Value;
    Invalidate;
  end;
end;

class procedure TCustomPanel.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomPanel;
end;

procedure TCustomPanel.SetBevelInner(const Value: TPanelBevel);
begin
  if BevelInner <> Value then
  begin
    FBevelInner := Value;
    Perform(CM_BORDERCHANGED, 0, 0);
  end;
end;

procedure TCustomPanel.SetBevelOuter(const Value: TPanelBevel);
begin
  if BevelOuter <> Value then
  begin
    FBevelOuter := Value;
    Perform(CM_BORDERCHANGED, 0, 0);
  end;
end;

procedure TCustomPanel.PaintBevel(var ARect: TRect; ABevel: TPanelBevel);
begin
  if ABevel <> bvNone then
    if BevelColor = clDefault then
      Canvas.Frame3d(ARect, BevelWidth, ABevel)
    else
      Canvas.Frame3d(ARect, BevelColor, BevelColor, BevelWidth);
  // Note: Frame3D inflates ARect
end;

procedure TCustomPanel.Paint;
var
  ARect: TRect;
  TS : TTextStyle;
begin
  ARect := GetClientRect;

  PaintBevel(ARect, BevelOuter);
  InflateRect(ARect, -BorderWidth, -BorderWidth);
  PaintBevel(ARect, BevelInner);

  if Caption <> '' then
  begin
    TS := Canvas.TextStyle;
    TS.Alignment := BidiFlipAlignment(Self.Alignment, UseRightToLeftAlignment);
    if BiDiMode<>bdLeftToRight then
      TS.RightToLeft:= True;
    TS.Layout:= tlCenter;
    TS.Opaque:= false;
    TS.Clipping:= false;
    TS.SystemFont:=Canvas.Font.IsDefault;
    TS.Wordbreak := FWordWrap;
    TS.SingleLine := not FWordwrap;
    if not Enabled then
      if ThemeServices.ThemesEnabled then
        Canvas.Font.Color := clGrayText
      else
      begin
        Canvas.Font.Color := clBtnHighlight;
        OffsetRect(ARect, 1, 1);
        Canvas.TextRect(ARect, ARect.Left, ARect.Top, Caption, TS);
        Canvas.Font.Color := clBtnShadow;
        OffsetRect(ARect, -1, -1);
      end
    else
      Canvas.Font.Color := Font.Color;

    Canvas.TextRect(ARect,ARect.Left,ARect.Top, Caption, TS);
  end;

  inherited Paint;
end;

procedure TCustomPanel.AdjustClientRect(var aRect: TRect);
var
  BevelSize: Integer;
begin
  inherited AdjustClientRect(aRect);

  BevelSize := BorderWidth;
  if (BevelOuter <> bvNone) then
    inc(BevelSize, BevelWidth);
  if (BevelInner <> bvNone) then
    inc(BevelSize, BevelWidth);

  InflateRect(aRect, -BevelSize, -BevelSize);
end;

class function TCustomPanel.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 170;
  Result.CY := 50;
end;

procedure TCustomPanel.Loaded;
begin
  inherited Loaded;
  UpdateParentColorChange;
end;

procedure TCustomPanel.UpdateParentColorChange;
begin
  if ParentColor then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
end;

procedure TCustomPanel.CMParentColorChanged(var Message: TLMessage);
begin
  UpdateParentColorChange;
  inherited;
end;

function TCustomPanel.GetDefaultDockCaption: String;
begin
  Result := Caption;
end;

procedure TCustomPanel.RealSetText(const Value: TCaption);
begin
  if Caption <> Value 
  then begin
    inherited RealSetText(Value);
    Invalidate;
  end;
end;

// included by extctrls.pp
