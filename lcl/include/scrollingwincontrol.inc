{
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

procedure TControlScrollBar.SetPosition(Value: Integer);

  Procedure SetVPosition;
  var
    Tmp : Longint;
  begin
    Tmp := FPosition;
    FPosition := Value;
    FControl.ScrollBy(0, Tmp - FPosition);
    if FControl.HandleAllocated
    and (GetScrollPos(FControl.Handle, SB_VERT) <> FPosition) then
      SetScrollPos(FControl.Handle, SB_VERT, FPosition, Visible);
  end;

  Procedure SetHPosition;
  var
    Tmp : Longint;
  begin
    Tmp := FPosition;
    FPosition := Value;
    FControl.ScrollBy(Tmp - FPosition, 0);
    if FControl.HandleAllocated
    and (GetScrollPos(FControl.Handle, SB_HORZ) <> FPosition) then
      SetScrollPos(FControl.Handle, SB_HORZ, FPosition, Visible);
  end;

begin
  if Value < 0 then begin
    SetPosition(0);
    exit;
  end;

  If fControl.AutoScroll then begin
    if FAutoRange < 0 then
      AutoCalcRange;

    if Value > FAutoRange then begin
      SetPosition(FAutoRange);
      exit;
    end;
  end;
  
  if Value>Range then begin
    SetPosition(Range);
    exit;
  end;
  
  if Value=FPosition then exit;

  if Kind = sbVertical then
    SetVPosition
  else
    SetHPosition;
  FControl.UpdateScrollBars;
end;

function TControlScrollBar.GetSize: integer;
var
  KindID: integer;
begin
  if Kind=sbHorizontal then
    KindID:=SM_CXHSCROLL
  else
    KindID:=SM_CXVSCROLL;
  if (FControl<>nil) and (FControl.HandleAllocated) then
    Result:=LCLLinux.GetScrollBarSize(FControl.Handle,KindID)
  else
    Result:=GetSystemMetrics(KindID);
end;

procedure TControlScrollBar.SetRange(Value: Integer);
begin
  If Value < 0 then begin
    Range := 0;
    exit;
  end;
  if FRange=Value then exit;
  FRange := Value;
  FControl.UpdateScrollBars;
end;

procedure TControlScrollBar.SetSize(const AValue: integer);
begin
  // ToDo
end;

procedure TControlScrollBar.SetVisible(Value: Boolean);
begin
  if FVisible = Value then exit;
  FVisible := Value;
  FControl.UpdateScrollBars;
end;

procedure TControlScrollBar.SetSmooth(Value: Boolean);
begin
  if FSmooth = Value then exit;
  FSmooth := Value;
  FControl.UpdateScrollBars;
end;

procedure TControlScrollBar.AutoCalcRange;

  procedure AutoCalcVRange;
  var
    I : Integer;
    TmpRange : Longint;
    IncludeControl : Boolean;
  begin
    TmpRange := 0;
    For I := 0 to FControl.ControlCount - 1 do
      With FControl.Controls[I] do
        if Visible then begin
          IncludeControl := (Align = alTop) or (Align = alNone);
          If IncludeControl then
            TmpRange := Max(TmpRange, Top + Height);
        end;
    Range := TmpRange;
  end;

  procedure AutoCalcHRange;
  var
    I : Integer;
    TmpRange : Longint;
    IncludeControl : Boolean;
  begin
    TmpRange := 0;
    For I := 0 to FControl.ControlCount - 1 do
      With FControl.Controls[I] do
        if Visible then begin
          IncludeControl := (Align = alLeft) or (Align = alNone);
          If IncludeControl then
            TmpRange := Max(TmpRange, Left + Width);
        end;
    Range := TmpRange;
  end;

begin
  if FControl.FAutoScroll then begin
    FVisible := True;
    if Kind = sbVertical then
      AutoCalcVRange
    else
      AutoCalcHRange;
    FControl.UpdateScrollBars;
  end;
end;

procedure TControlScrollBar.UpdateScrollBar;
var
  ScrollInfo: TScrollInfo;
  SBSize : Longint;
  procedure UpdateVScroll;
  begin
    With FControl do begin
      Page := Min(ClientHeight + 1,High(Page));
      ScrollInfo.nPage := Page;

      if Visible then begin
        If (FControl <> nil) and (FControl.HorzScrollBar.Visible) then
          SBSize := FControl.HorzScrollBar.Size
        else
          SBSize := 0;
        FAutoRange := (Range - ClientHeight)*Shortint(Range >= ClientHeight + SBSize);
        ScrollInfo.nMax := Range;
      end
      else
        ScrollInfo.nMax := 0;

      If (Self.Visible and not FAutoScroll)
      or (FAutoScroll and (ScrollInfo.nMax > 0) and (ScrollInfo.nMax > Height))
      then
        Self.FVisible := True
      else
        Self.FVisible := False;
      if HandleAllocated then
        SetScrollInfo(Handle, SB_VERT, ScrollInfo, Self.Visible);
    end;
  end;

  procedure UpdateHScroll;
  begin
    With FControl do begin
      Page := Min(ClientWidth + 1,High(Page));
      ScrollInfo.nPage := Page;

      if Visible then begin
        If (FControl <> nil) and (FControl.VertScrollBar.Visible) then
          SBSize := FControl.VertScrollBar.Size
        else
          SBSize := 0;
        FAutoRange := (Range - ClientWidth)*Shortint(Range >= ClientWidth + SBSize);
        ScrollInfo.nMax := Range;
      end
      else
        ScrollInfo.nMax := 0;

      If (Self.Visible and not FAutoScroll)
      or (FAutoScroll and (ScrollInfo.nMax > 0) and (ScrollInfo.nMax > Width))
      then
        Self.FVisible := True
      else
        Self.FVisible := False;
      if HandleAllocated then
        SetScrollInfo(Handle, SB_HORZ, ScrollInfo, Self.Visible);
    end;
  end;

begin
  FAutoRange := 0;

  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin := 0;
  ScrollInfo.nPos := FPosition;
  ScrollInfo.nTrackPos := FPosition;

  if Kind = sbVertical then
    UpdateVScroll
  else
    UpdateHScroll;

  SetPosition(ScrollInfo.nTrackPos);

  //I am not positive that this is right
  //but it apeared to be when I compared
  //results to Delphi 4
  if Smooth then
    Increment := Page div 10;
end;

procedure TControlScrollBar.ScrollHandler(var Message: TLMScroll);
var
  NewPos: Longint;
begin
  If (csDesigning in FControl.ComponentState) then
    exit; //prevent wierdness in IDE.
    
  with Message do
  begin
    NewPos := FPosition;
    case ScrollCode of
      SB_LINEUP:
        Dec(NewPos, FIncrement);
      SB_LINEDOWN:
        Inc(NewPos, FIncrement);
      SB_PAGEUP:
        Dec(NewPos, FPage);
      SB_PAGEDOWN:
        Inc(NewPos, FPage);
      SB_THUMBPOSITION, SB_THUMBTRACK:
        NewPos := Pos;
      SB_TOP:
        NewPos := 0;
      SB_BOTTOM:
        NewPos := Range;
    end;
    if NewPos < 0 then NewPos := 0;
    if NewPos > Range then NewPos := Range;
    SetPosition(NewPos);
  end;
end;

constructor TControlScrollBar.Create(AControl: TScrollingWinControl; AKind: TScrollBarKind);
begin
  Inherited Create;
  FControl := AControl;
  FKind := AKind;
  FPage := 80;
  FIncrement := 8;
  FPosition := 0;
  FRange := 0;
  //FSmooth := True;
  //FVisible := True;
end;

procedure TControlScrollBar.Assign(Source: TPersistent);
begin
  If Source is TControlScrollBar then begin
    With Source as TControlScrollBar do begin
      Self.Increment := Increment;
      Self.Position := Position;
      Self.Range := Range;
      Self.Visible := Visible
    end;
  end
  else
    inherited Assign(Source);
end;

function TControlScrollBar.IsScrollBarVisible: Boolean;
begin
  Result := (FControl <> nil) and FControl.HandleAllocated and
            (FControl.Visible) and (Self.Visible);
end;

function TControlScrollBar.ScrollPos: Integer;
begin
  if Visible then
    Result:=Position
  else
    Result:=0;
end;

procedure TScrollingWinControl.SetAutoScroll(Value: Boolean);
begin
  if FAutoScroll <> Value then
  begin
    FAutoScroll := Value;
    if Value then begin
      HorzScrollBar.AutoCalcRange;
      VertScrollBar.AutoCalcRange;
    end;
    UpdateScrollBars;
  end;
end;

procedure TScrollingWinControl.CreateWnd;
begin
  inherited CreateWnd;
  UpdateScrollBars;
end;

procedure TScrollingWinControl.AlignControls(AControl: TControl; var ARect: TRect);
begin
  HorzScrollBar.AutoCalcRange;
  VertScrollBar.AutoCalcRange;
  If not AutoScroll then
    UpdateScrollBars;
  inherited AlignControls(AControl, ARect);
end;

Procedure TScrollingWinControl.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  with Message do begin
    FCanvas.Lock;
    try
      FCanvas.Handle := DC;
      try
        FCanvas.Brush.Color := Self.Color;
        FCanvas.FillRect(ClientRect);
      finally
        FCanvas.Handle := 0;
      end;
    finally
      FCanvas.Unlock;
    end;
  end;
end;

procedure TScrollingWinControl.WMPaint(var Message: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  try
    ControlState := ControlState + [csCustomPaint];
    inherited WMPaint(Message);
    ControlState := ControlState - [csCustomPaint];
  finally
    Exclude(FControlState, csCustomPaint);
  end;
end;

Procedure TScrollingWinControl.Paint;
begin
  if Assigned (FOnPaint) then
    FOnPaint(Self);
end;

Procedure TScrollingWinControl.PaintWindow(DC : Hdc);
begin
  try
    FCanvas.Handle := DC;
    try
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TScrollingWinControl.WMSize(var Message: TLMSize);
begin
  inherited;
  if AutoScroll or HorzScrollBar.Visible or VertScrollBar.Visible
  then
    UpdateScrollBars;
end;

procedure TScrollingWinControl.SetHorzScrollBar(Value: TControlScrollBar);
begin
  FHorzScrollbar.Assign(Value);
end;

procedure TScrollingWinControl.SetVertScrollBar(Value: TControlScrollBar);
begin
  FVertScrollbar.Assign(Value);
end;

Procedure TScrollingWinControl.UpdateScrollbars;
begin
  If IsUpdating then
    exit;
  IsUpdating := True;
  FVertScrollbar.UpdateScrollbar;
  FHorzScrollbar.UpdateScrollbar;
  IsUpdating := False;
end;

Function TScrollingWinControl.StoreScrollBars : Boolean;
begin
  Result := Not AutoScroll;
end;

procedure TScrollingWinControl.ScrollBy(DeltaX, DeltaY: Integer);
begin
  Invalidate;
end;

Procedure TScrollingWinControl.WMVScroll(var Message : TLMVScroll);
begin
  VertScrollbar.ScrollHandler(Message);
end;

Procedure TScrollingWinControl.WMHScroll(var Message : TLMHScroll);
begin
  HorzScrollbar.ScrollHandler(Message);
end;

Constructor TScrollingWinControl.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);

  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;

  FVertScrollbar := TControlScrollBar.Create(Self, sbVertical);
  FHorzScrollbar := TControlScrollBar.Create(Self, sbHorizontal);

  ControlStyle := [csAcceptsControls, csClickEvents, csDoubleClicks];

  SetBounds(0,0, 200, 200);
end;

Destructor TScrollingWinControl.Destroy;
begin
  FreeThenNil(FHorzScrollBar);
  FreeThenNil(FVertScrollBar);
  FreeThenNil(FCanvas);
  inherited Destroy;
end;

