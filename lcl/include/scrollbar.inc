{%MainUnit ../stdctrls.pp}

{
 TCustomScrollBar

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}



{------------------------------------------------------------------------------}
{  function TCustomScrollBar.Create                                                      }
{------------------------------------------------------------------------------}
constructor TCustomScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csScrollBar;
  Width := 121;
  Height := GetSystemMetrics(SM_CYHSCROLL);
  SetBounds(0,0,width,height);
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
  CreateSubClass(Params, 'SCROLLBAR');
  Params.Style := Params.Style or Kinds[FKind];
  if FKind = sbVertical then
      Params.Style := Params.Style or SBS_LEFTALIGN;
  FRTLFactor := 1
end;

procedure TCustomScrollBar.CreateWnd;
var
  ScrollInfo: TScrollInfo;
begin
  inherited CreateWnd;
  if not HandleAllocated then RaiseGDBException('TCustomScrollBar.CreateWnd HandleAllocated=false');
  SetScrollRange(Handle, SB_CTL, FMin, FMax, False);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.nPage := FPageSize;
  ScrollInfo.fMask := SIF_PAGE;
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
  if FKind <> Value then
  begin
    FKind := Value;
    // switch width and height
    OldWidth:=Width;
    OldHeight:=Height;
    if HandleAllocated then
      RecreateWnd;
    SetBounds(Left,Top,OldHeight,OldWidth);
  end;
end;

procedure TCustomScrollBar.SetParams(APosition, AMin, AMax: Integer);
begin
  if AMax < AMin then
    raise EInvalidOperation.Create(rsScrollBarOutOfRange);
  if APosition < AMin then APosition := AMin;
  if APosition > AMax then APosition := AMax;
  if (FMin <> AMin) or (FMax <> AMax) then
  begin
    FMin := AMin;
    FMax := AMax;
    if HandleAllocated then
      SetScrollRange(Handle, SB_CTL, AMin, AMax, FPosition = APosition);
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

procedure TCustomScrollBar.SetPosition(Value: Integer);
begin
  SetParams(Value, FMin, FMax);
end;

procedure TCustomScrollBar.SetPageSize(Value: Integer);
var
  ScrollInfo: TScrollInfo;
begin
  if (FPageSize = Value) or (FPageSize > FMax) then exit;
  FPageSize := Value;
  if HandleAllocated then begin
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.nPage := Value;
    ScrollInfo.fMask := SIF_PAGE;
    SetScrollInfo(Handle, SB_CTL, ScrollInfo, True);
  end;
end;

procedure TCustomScrollBar.SetMin(Value: Integer);
begin
  SetParams(FPosition, Value, FMax);
end;

procedure TCustomScrollBar.SetMax(Value: Integer);
begin
  SetParams(FPosition, FMin, Value);
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
  NewPos: Longint;
  ScrollInfo: TScrollInfo;
begin
  with Message do
  begin
    NewPos := FPosition;
    case TScrollCode(ScrollCode) of
      scLineUp:
        Dec(NewPos, FSmallChange * FRTLFactor);
      scLineDown:
        Inc(NewPos, FSmallChange * FRTLFactor);
      scPageUp:
        Dec(NewPos, FLargeChange * FRTLFactor);
      scPageDown:
        Inc(NewPos, FLargeChange * FRTLFactor);
      scPosition, scTrack:
        with ScrollInfo do
        begin
          cbSize := SizeOf(ScrollInfo);
          fMask := SIF_ALL;
          GetScrollInfo(Handle, SB_CTL, ScrollInfo);
          NewPos := nTrackPos;
          { We need to reverse the positioning because SetPosition below
            calls SetParams that reverses the position. This acts as a
            double negative. }
          if not NotRightToLeft then NewPos := FMax - NewPos;
        end;
      scTop:
        NewPos := FMin;
      scBottom:
        NewPos := FMax;
    end;
    if NewPos < FMin then NewPos := FMin;
    if NewPos > FMax then NewPos := FMax;
    ScrollPos := NewPos;
    Scroll(TScrollCode(ScrollCode), ScrollPos);
    SetPosition(ScrollPos);
  end;
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

// included by stdctrls.pp

