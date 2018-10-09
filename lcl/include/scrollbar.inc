{%MainUnit ../stdctrls.pp}

{
 TCustomScrollBar

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}



{------------------------------------------------------------------------------}
{  function TCustomScrollBar.Create                                                      }
{------------------------------------------------------------------------------}
constructor TCustomScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csScrollBar;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  TabStop := True;
  ControlStyle := ControlStyle + [csFramed, csDoubleClicks, csOpaque]
                               - [csAcceptsControls, csDoubleClicks,
                                  csCaptureMouse, csSetCaption];
  FKind := sbHorizontal;
  FPosition := 0;
  FMin := 0;
  FMax := 100;
  FSmallChange := 1;
  FLargeChange := 1;
end;

procedure TCustomScrollBar.CreateParams(var Params: TCreateParams);
const
  Kinds: array[TScrollBarKind] of DWORD = (SBS_HORZ, SBS_VERT);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Kinds[FKind];
  FRTLFactor := 1
end;

procedure TCustomScrollBar.CreateWnd;
var
  ScrollInfo: TScrollInfo;
begin
  inherited CreateWnd;
  if not HandleAllocated then RaiseGDBException('TCustomScrollBar.CreateWnd HandleAllocated=false');
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.nMin := FMin;
  ScrollInfo.nMax := FMax;
  ScrollInfo.nPage := FPageSize;
  ScrollInfo.fMask := SIF_PAGE or SIF_Range;
  SetScrollInfo(Handle, SB_CTL, ScrollInfo, False);
  if NotRightToLeft then
    SetScrollPos(Handle, SB_CTL, FPosition, True)
  else
    SetScrollPos(Handle, SB_CTL, FMax - FPosition, True);
end;

function TCustomScrollBar.NotRightToLeft: Boolean;
begin
  Result := True;
end;

procedure TCustomScrollBar.SetKind(Value: TScrollBarKind);
var
  OldWidth: Integer;
  OldHeight: Integer;
begin
  if FKind = Value then Exit;

  FKind := Value;

  // the InterfaceConstraints need to get updated, even when loading
  OldWidth:=Width;
  OldHeight:=Height;
  Constraints.UpdateInterfaceConstraints;

  // switch width and height, but not when loading, because we assume that
  // the lfm contains a consistent combination of kind and (width, height)
  if (csLoading in ComponentState) then Exit;

  if HandleAllocated then
    TWSScrollBarClass(WidgetSetClass).SetKind(Self, FKind = sbHorizontal);

  SetBounds(Left,Top,OldHeight,OldWidth);
end;

procedure TCustomScrollBar.SetParams(APosition, AMin, AMax, APageSize: Integer);
var
  ScrollInfo: TScrollInfo;
begin
  if AMax < AMin then
    raise EInvalidOperation.Create(rsScrollBarOutOfRange);
  if APosition < AMin then APosition := AMin;
  if APosition > AMax then APosition := AMax;
  if APageSize < 0 then APageSize := 0;
  if (FMin <> AMin) or (FMax <> AMax) or (APageSize <> FPageSize) then
  begin
    FMin := AMin;
    FMax := AMax;
    FPageSize := APageSize;
    if HandleAllocated then
    begin
      ScrollInfo.fMask := SIF_PAGE or SIF_Range;
      ScrollInfo.nMin := AMin;
      ScrollInfo.nMax := AMax;
      ScrollInfo.nPage := APageSize;
      SetScrollInfo(Handle, SB_CTL, ScrollInfo, FPosition = APosition);
    end;
  end;
  if FPosition <> APosition then
  begin
    FPosition := APosition;
    if HandleAllocated then
      if NotRightToLeft then
        SetScrollPos(Handle, SB_CTL, FPosition, True)
      else
        SetScrollPos(Handle, SB_CTL, FMax - FPosition, True);
    Change;
  end;
  if HandleAllocated then
    TWSScrollBarClass(WidgetSetClass).SetParams(Self);
end;

procedure TCustomScrollBar.SetParams(APosition, AMin, AMax: Integer);
begin
  SetParams(APosition, AMin, AMax, FPageSize);
end;

procedure TCustomScrollBar.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
  if (Kind=sbHorizontal) and (PreferredHeight=0) then
    PreferredHeight:=GetSystemMetrics(SM_CYHSCROLL);
  if (Kind=sbVertical) and (PreferredWidth=0) then
    PreferredWidth:=GetSystemMetrics(SM_CYVSCROLL);
end;

procedure TCustomScrollBar.SetPosition(Value: Integer);
begin
  SetParams(Value, FMin, FMax, FPageSize);
end;

procedure TCustomScrollBar.SetPageSize(Value: Integer);
begin
  SetParams(FPosition, FMin, FMax, Value);
end;

procedure TCustomScrollBar.SetMin(Value: Integer);
begin
  SetParams(FPosition, Value, FMax, FPageSize);
end;

procedure TCustomScrollBar.SetMax(Value: Integer);
begin
  SetParams(FPosition, FMin, Value, FPageSize);
end;

procedure TCustomScrollBar.Change;
begin
  inherited Changed;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomScrollBar.Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Assigned(FOnScroll) then FOnScroll(Self, ScrollCode, ScrollPos);
end;

procedure TCustomScrollBar.DoScroll(var Message: TLMScroll);
var
  ScrollPos: Integer;
  ScrollCode: TScrollCode;
  NewPos: Longint;
begin
  NewPos := FPosition;
  case Message.ScrollCode of
    SB_LINEUP: begin
      ScrollCode := scLineUp;
      Dec(NewPos, FSmallChange * FRTLFactor);
    end;
    SB_LINEDOWN: begin
      ScrollCode := scLineDown;
      Inc(NewPos, FSmallChange * FRTLFactor);
    end;
    SB_PAGEUP: begin
      ScrollCode := scPageUp;
      Dec(NewPos, FLargeChange * FRTLFactor);
    end;
    SB_PAGEDOWN: begin
      ScrollCode := scPageDown;
      Inc(NewPos, FLargeChange * FRTLFactor);
    end;
    SB_THUMBPOSITION, SB_THUMBTRACK: begin
      if Message.ScrollCode = SB_THUMBPOSITION
      then ScrollCode := scPosition
      else ScrollCode := scTrack;
      { We need to reverse the positioning because SetPosition below calls
        SetParams that reverses the position. This acts as a double negative. }
      if NotRightToLeft
      then NewPos := Message.Pos
      else NewPos := FMax - Message.Pos;
    end;
    SB_TOP: begin
      ScrollCode := scTop;
      NewPos := FMin;
    end;
    SB_BOTTOM: begin
      ScrollCode := scBottom;
      NewPos := FMax;
    end;
    SB_ENDSCROLL: begin
      ScrollCode := scEndScroll;
    end;
  else
    Exit;
  end;

  {see issue #20127 +1 follows winapi bug otherwise under mswindows at max position
  we'll have gap between slider and edge of scrollbar. Gtk2 and Qt are fine with this.}
  if NewPos + 1 > (FMax - FPageSize) + 1 then NewPos := (FMax - FPageSize) + 1;
  if NewPos < FMin then NewPos := FMin;

  ScrollPos := NewPos;
  Scroll(ScrollCode, ScrollPos);
  SetPosition(ScrollPos);
end;

procedure TCustomScrollBar.CNHScroll(var Message: TLMHScroll);
begin
  DoScroll(Message);
end;

procedure TCustomScrollBar.CNVScroll(var Message: TLMVScroll);
begin
  DoScroll(Message);
end;

procedure TCustomScrollBar.CNCtlColorScrollBar(var Message: TLMessage);
begin
//CallWIndowProc is not yet created so no code is here
end;

procedure TCustomScrollBar.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  DefaultHandler(Message);
end;

class procedure TCustomScrollBar.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomScrollBar;
end;

class function TCustomScrollBar.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 121;
  Result.CY := GetSystemMetrics(SM_CYHSCROLL);
end;

// included by stdctrls.pp
