{%MainUnit ../forms.pp}

{ THintWindow

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
{
   use:
   HintWindow := THintWindow.Create(nil);
   Rect := HintWindow.CalcHintRect(0,'This is the hint',nil);
   HintWindow.ActivateHint(Rect,'This is the hint');

}

const HintBorderWidth=2;

constructor THintWindow.Create(AOwner: TComponent);
var
  TheTimer: TCustomTimer;
begin
  inherited Create(AOwner);
  fCompStyle := csHintWindow;
  Parent := nil;
  Color := clInfoBk;
  Canvas.Font := Screen.HintFont;
  Canvas.Brush.Style := bsClear;
  FAlignment := taLeftJustify;
  BorderStyle := bsNone;
  Caption := 'THintWindow';
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FHideInterval := 3000;
  TheTimer := TCustomTimer.Create(self);
  FAutoHideTimer := TheTimer;
  TheTimer.Interval := HideInterval;
  TheTimer.Enabled := False;
  TheTimer.OnTimer := @AutoHideHint;
end;

destructor THintWindow.Destroy;
begin
  fAutoHideTimer.Free;
  fAutoHideTimer:=nil;
  inherited Destroy;
end;

procedure THintWindow.SetHideInterval(Value : Integer);
Begin
  FHideInterval := Value;
  if FAutoHideTimer<>nil then
    TCustomTimer(FAutoHideTimer).Interval := FHideInterval;
end;

class procedure THintWindow.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterHintWindow;
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
  if not (value) and (FAutoHideTimer<>nil) then
    TCustomTimer(FAutoHideTimer).Enabled := False;
end;


procedure THintWindow.AutoHideHint(Sender : TObject);
begin
  if FAutoHideTimer <> nil then
    TCustomTimer(FAutoHideTimer).Enabled := False;
  if Visible then
    Visible := False;
end;

procedure THintWindow.Paint;
var
  ARect: TRect;
  Details: TThemedElementDetails;
begin
  ARect := ClientRect;
  if (Color = clInfoBk) or (Color = clDefault) then // draw using themes
  begin
    Details := ThemeServices.GetElementDetails(tttStandardLink);
    ThemeServices.DrawElement(Canvas.Handle, Details, ARect);
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Width := 1;
    Canvas.FillRect(ARect);
    DrawEdge(Canvas.Handle, ARect, BDR_RAISEDOUTER, BF_RECT);
  end;
  InflateRect(ARect, - 2 * HintBorderWidth, - 2 * HintBorderWidth);
  DrawText(Canvas.GetUpdatedHandle([csFontValid]), PChar(Caption),
    Length(Caption), ARect, GetDrawTextFlags);
end;

class function THintWindow.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 25;
  Result.CY := 25;
end;

procedure THintWindow.ActivateHint(ARect: TRect; const AHint: String);
var
  InvalidateNeeded: Boolean;
  AMonitor: TMonitor;
  ABounds: TRect;
begin
  if FActivating then exit;
  FActivating := True;
  try
    //debugln('THintWindow.ActivateHint OldHint="',DbgStr(Caption),'" NewHint="',DbgStr(AHint),'"');
    InvalidateNeeded := Visible and (Caption <> AHint);
    Caption := AHint;
    AMonitor := Screen.MonitorFromPoint(ARect.TopLeft);
    ABounds := AMonitor.BoundsRect;

    // offset hint to fit into monitor
    if ARect.Bottom > ABounds.Bottom then
    begin
      ARect.Top := ABounds.Bottom - (ARect.Bottom - ARect.Top);
      ARect.Bottom := ABounds.Bottom;
    end;
    if ARect.Top < ABounds.Top then
    begin
      ARect.Bottom := Min(ABounds.Top + (ARect.Bottom - ARect.Top), ABounds.Bottom);
      ARect.Top := ABounds.Top;
    end;

    if ARect.Right > ABounds.Right then
    begin
      ARect.Left := ABounds.Right - (ARect.Right - ARect.Left);
      ARect.Right := ABounds.Right;
    end;
    if ARect.Left < ABounds.Left then
    begin
      ARect.Right:= Min(ABounds.Left + (ARect.Right - ARect.Left), ABounds.Right);
      ARect.Left := ABounds.Left;
    end;

    SetBounds(ARect.Left, ARect.Top,
              ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
    Visible := True;
    TCustomTimer(FAutoHideTimer).Enabled := False;
    TCustomTimer(FAutoHideTimer).Enabled := FAutoHide;
    if InvalidateNeeded then Invalidate;
  finally
    FActivating := False;
  end;
end;

procedure THintWindow.ActivateHintData(ARect: TRect; const AHint: String; AData: pointer);
begin
  ActivateHint(ARect, AHint);
end;

function THintWindow.CalcHintRect(MaxWidth: Integer; const AHint: String;
  AData: Pointer): TRect;
begin
  if MaxWidth <= 0 then
    MaxWidth := Screen.Width - 4 * HintBorderWidth;
  Result := Rect(0, 0, MaxWidth, Screen.Height - 4 * HintBorderWidth);
  if AHint='' then exit;
  DrawText(Canvas.GetUpdatedHandle([csFontValid]), PChar(AHint), Length(AHint),
           Result, DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK);
  inc(Result.Right, 4 * HintBorderWidth);
  inc(Result.Bottom, 4 * HintBorderWidth);
  //debugln('THintWindow.CalcHintRect Result=',dbgs(Result));
end;

procedure THintWindow.ReleaseHandle;
begin
  if HandleAllocated then
    DestroyHandle;
end;

// included by forms.pp
