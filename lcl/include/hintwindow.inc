{%MainUnit ../forms.pp}

{ THintWindow

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
{
   use:
   HintWindow := THintWindow.Create(nil);
   Rect := HintWindow.CalcHintRect(0,'This is the hint', Nil);
   HintWindow.ActivateHint(Rect,'This is the hint');

}

const
  HintBorderWidth = 2;

class destructor THintWindow.Destroy;
begin
  FreeAndNil(FSysHintFont);
end;

constructor THintWindow.Create(AOwner: TComponent);
begin
  // THintWindow has no resource => must be constructed using CreateNew
  inherited CreateNew(AOwner, 1);
  fCompStyle := csHintWindow;
  Parent := nil;
  Color := clInfoBk;
  Font := Screen.HintFont;
  Canvas.Brush.Style := bsClear;
  FAlignment := taLeftJustify;
  BorderStyle := bsNone;
  Caption := '';
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FHideInterval := 3000;
  FAutoHide := False;
  FAutoHideTimer := TCustomTimer.Create(self);
  FAutoHideTimer.Interval := HideInterval;
  FAutoHideTimer.Enabled := False;
  FAutoHideTimer.OnTimer := @AutoHideHint;
end;

destructor THintWindow.Destroy;
begin
  FreeAndNil(FAutoHideTimer);
  inherited Destroy;
end;

procedure THintWindow.SetHideInterval(Value : Integer);
Begin
  FHideInterval := Value;
  if Assigned(FAutoHideTimer) then
    FAutoHideTimer.Interval := FHideInterval;
end;

procedure THintWindow.SetHintRectAdjust(AValue: TRect);
begin
  FHintRect := AValue;
  // Add border
  inc(FHintRect.Right, 4 * HintBorderWidth);
  inc(FHintRect.Bottom, 4 * HintBorderWidth);
end;

class function THintWindow.SysHintFont: TFont;
begin
  if (FSysHintFont = nil) then
  begin
    FSysHintFont := TFont.Create;
    if not WidgetSet.InitStockFont(FSysHintFont, sfHint) then
    begin
      FSysHintFont.FontData := DefFontData;
      FSysHintFont.Color := clInfoText;
    end;
  end;
  Result := FSysHintFont;
end;

class procedure THintWindow.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterHintWindow;
end;

procedure THintWindow.WMNCHitTest(var Message: TLMessage);
begin
  Message.Result := HTTRANSPARENT;
end;

procedure THintWindow.DoShowWindow;
begin
  if (ActiveControl = nil) and (not (csDesigning in ComponentState)) and (Parent=nil) then
  begin
    // automatically choose a control to focus
    {$IFDEF VerboseFocus}
    DebugLn('THintWindow.WMShowWindow ',DbgSName(Self),' Set ActiveControl := ',DbgSName(FindDefaultForActiveControl));
    {$ENDIF}
    ActiveControl := FindDefaultForActiveControl;
  end;
end;

procedure THintWindow.UpdateRegion;
var
  ARect: TRect;
  Details: TThemedElementDetails;
  Rgn: HRGN;
begin
  if not HandleAllocated then
    Exit;

  if UseBGThemes then
  begin
    Details := ThemeServices.GetElementDetails(tttStandardNormal);
    ARect := ClientRect;
    Rgn := ThemeServices.GetDetailRegion(Canvas.Handle, Details, ARect);
    SetWindowRgn(Handle, Rgn, False);
  end
  else
    SetWindowRgn(Handle, 0, False);
end;

function THintWindow.UseFGThemes: Boolean;
begin
  Result := Font.IsEqual(SysHintFont);
end;

procedure THintWindow.SetColor(Value: TColor);
begin
  inherited SetColor(Value);
  UpdateRegion;
end;

function THintWindow.UseBGThemes: Boolean;
begin
  Result := (Color = clInfoBk) or (Color = clDefault);
end;

function THintWindow.GetDrawTextFlags: Cardinal;
var
  EffectiveAlignment: TAlignment;
begin
  Result := DT_NOPREFIX or DT_VCENTER or DT_WORDBREAK;
  EffectiveAlignment := FAlignment;
  if BiDiMode <> bdLeftToRight then
  begin
    Result := Result or DT_RTLREADING;
    //change alignment if is RTL
    if BiDiMode = bdRightToLeft then
    begin
      case FAlignment of
        taLeftJustify: EffectiveAlignment := taRightJustify;
        taRightJustify: EffectiveAlignment := taLeftJustify;
      end;
    end;
  end;
  case EffectiveAlignment of
    taLeftJustify: Result := Result or DT_LEFT;
    taCenter: Result := Result or DT_CENTER;
    taRightJustify: Result := Result or DT_RIGHT;
  end;
end;

procedure THintWindow.SetAutoHide(Value : Boolean);
Begin
  FAutoHide := Value;
  if not Value and Assigned(FAutoHideTimer) then
    FAutoHideTimer.Enabled := False;
end;

procedure THintWindow.AutoHideHint(Sender : TObject);
begin
  if Assigned(FAutoHideTimer) then
    FAutoHideTimer.Enabled := False;
  Visible := False;
end;

procedure THintWindow.Paint;
var
  ThemeBG, ThemeFG: Boolean;
  Details: TThemedElementDetails;
  ARect: TRect;
begin
  if ControlCount > 0 then
    inherited Paint         // The window has a custom control.
  else
  begin
    ThemeBG := UseBGThemes;
    ThemeFG := UseFGThemes;
    ARect := ClientRect;

    if ThemeBG or ThemeFG then
      Details := ThemeServices.GetElementDetails(tttStandardNormal);

    if ThemeBG then
      ThemeServices.DrawElement(Canvas.Handle, Details, ARect)
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.Pen.Width := 1;
      Canvas.FillRect(ARect);
      DrawEdge(Canvas.Handle, ARect, BDR_RAISEDOUTER, BF_RECT);
    end;
    InflateRect(ARect, -2 * HintBorderWidth, -2 * HintBorderWidth);
    if ThemeFG then
      ThemeServices.DrawText(Canvas, Details, Caption, ARect, GetDrawTextFlags, 0)
    else
      DrawText(Canvas.GetUpdatedHandle([csFontValid]), PChar(Caption),
        Length(Caption), ARect, GetDrawTextFlags);
  end;
end;

procedure THintWindow.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateRegion;
end;

class function THintWindow.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 25;
  Result.CY := 25;
end;

procedure THintWindow.ActivateSub;
begin
  SetBounds(FHintRect.Left, FHintRect.Top,
            FHintRect.Right - FHintRect.Left, FHintRect.Bottom - FHintRect.Top);
  Visible := True;
  FAutoHideTimer.Enabled := False;
  FAutoHideTimer.Enabled := FAutoHide;
end;

procedure THintWindow.ActivateHint(const AHint: String);
// Shows simple text hint.
begin
  if FActivating then exit;

  if Visible and (Caption=AHint)
  and EqualRect(FHintRect, BoundsRect) then
  begin
    FActivating := FActivating;
    Exit; // nothing changed, exit -> don't flicker
  end;

  FActivating := True;
  try
    Assert(ControlCount = 0, 'THintWindow.ActivateRendered: ControlCount > 0');
    if Caption<>AHint then
      Hide; // reduce flicker and start animation if hint changed
    Caption := AHint;
    ActivateSub;
  finally
    FActivating := False;
  end;
end;

procedure THintWindow.ActivateHint(ARect: TRect; const AHint: String);
begin
  HintRect := ARect;
  AdjustBoundsForMonitor;
  ActivateHint(AHint);
end;

procedure THintWindow.ActivateWithBounds(ARect: TRect; const AHint: String);
begin
  HintRect := ARect;
  ActivateHint(AHint);
end;

procedure THintWindow.ActivateHintData(ARect: TRect; const AHint: String; AData: pointer);
begin
  HintRect := ARect;
  ActivateHint(AHint);                // AData is not used now.
end;

function THintWindow.CalcHintRect(MaxWidth: Integer; const AHint: String;
  AData: pointer): TRect;
var
  Flags: Cardinal;
  uh: HDC;
  Mon: TMonitor;
begin
  Mon := Screen.MonitorFromPoint(Point(Left, Top)); // don't use Monitor property - it returns wrong monitor for invisible windows.
  if Mon=nil then
    Mon := Screen.Monitors[0];
  if Application.Scaled and Scaled and (Mon<>nil) and (PixelsPerInch<>Mon.PixelsPerInch) then
    AutoAdjustLayout(lapAutoAdjustForDPI, PixelsPerInch, Mon.PixelsPerInch, 0, 0);

  if AHint = '' then
    Exit(Rect(0, 0, 0, 0));
  if MaxWidth <= 0 then
    MaxWidth := Mon.Width - 4 * HintBorderWidth;
  Result := Rect(0, 0, MaxWidth, Mon.Height - 4 * HintBorderWidth);
  Flags := DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK;
  if UseRightToLeftAlignment then Flags := Flags or DT_RTLREADING;
  uh := Canvas.GetUpdatedHandle([csFontValid]);
  if UseFGThemes then
    Result := ThemeServices.GetTextExtent(uh,
      ThemeServices.GetElementDetails(tttStandardNormal), AHint, Flags, @Result)
  else
    DrawText(uh, PChar(AHint), Length(AHint), Result, Flags);
  // compensate for InflateRect in Paint method
  Inc(Result.Right, 4 * HintBorderWidth);
  Inc(Result.Bottom, 4 * HintBorderWidth);
  //debugln('THintWindow.CalcHintRect Result=',dbgs(Result));
end;

procedure THintWindow.AdjustBoundsForMonitor(KeepWidth: Boolean;
  KeepHeight: Boolean);
var
  AMonitor: TMonitor;
  ABounds: TRect;
begin
  AMonitor := Screen.MonitorFromPoint(FHintRect.TopLeft);
  ABounds := AMonitor.WorkareaRect;

  // offset hint to fit into monitor
  if FHintRect.Bottom > ABounds.Bottom then
  begin
    if KeepHeight then
      FHintRect.Top := ABounds.Bottom - (FHintRect.Bottom - FHintRect.Top);
    FHintRect.Bottom := ABounds.Bottom;
  end;
  if FHintRect.Top < ABounds.Top then
  begin
    if KeepHeight then
      FHintRect.Bottom := Min(ABounds.Top + (FHintRect.Bottom - FHintRect.Top), ABounds.Bottom);
    FHintRect.Top := ABounds.Top;
  end;

  if FHintRect.Right > ABounds.Right then
  begin
    if KeepWidth then
      FHintRect.Left := ABounds.Right - (FHintRect.Right - FHintRect.Left);
    FHintRect.Right := ABounds.Right;
  end;
  if FHintRect.Left < ABounds.Left then
  begin
    if KeepWidth then
      FHintRect.Right:= Min(ABounds.Left + (FHintRect.Right - FHintRect.Left), ABounds.Right);
    FHintRect.Left := ABounds.Left;
  end;
end;

function THintWindow.OffsetHintRect(AOffset: TPoint; dy: Integer;
  KeepWidth: Boolean; KeepHeight: Boolean): Boolean;
begin
  Result:=OffsetRect(FHintRect, AOffset.X, AOffset.Y + dy);
  AdjustBoundsForMonitor(KeepWidth, KeepHeight);
end;

procedure THintWindow.InitializeWnd;
begin
  inherited InitializeWnd;
  UpdateRegion;
end;

function THintWindow.IsHintMsg(Msg: TMsg): Boolean;
begin
  case Msg.message of
    LM_KEYFIRST..LM_KEYLAST,
    CM_ACTIVATE, CM_DEACTIVATE,
    CM_APPSYSCOMMAND,
    LM_COMMAND,
    LM_LBUTTONDOWN..LM_MOUSELAST, LM_NCMOUSEMOVE :
      Result := True;
    else
      Result := False;
  end;
end;

procedure THintWindow.ReleaseHandle;
begin
  if HandleAllocated then
    DestroyHandle;
end;

{ THintWindowRendered }

constructor THintWindowRendered.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor THintWindowRendered.Destroy;
begin
  inherited Destroy;
end;

procedure THintWindowRendered.ActivateRendered;
// Shows hint contents which are rendered to Controls[0] by a rendering provider
begin
  if FActivating then exit;
  FActivating := True;
  try
    Assert(ControlCount > 0, 'THintWindowRendered.ActivateRendered: ControlCount = 0');
    ActivateSub;
    Invalidate;
  finally
    FActivating := False;
  end;
end;


// included by forms.pp
