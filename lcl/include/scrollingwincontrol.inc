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
    if GetScrollPos(FControl.Handle, SB_VERT) <> FPosition then
      SetScrollPos(FControl.Handle, SB_VERT, FPosition, True);
  end;

  Procedure SetHPosition;
  var
    Tmp : Longint;
  begin
    Tmp := FPosition;
    FPosition := Value;
    FControl.ScrollBy(Tmp - FPosition, 0);
    if GetScrollPos(FControl.Handle, SB_HORZ) <> FPosition then
      SetScrollPos(FControl.Handle, SB_HORZ, FPosition, True);
  end;

begin
  if Value < 0 then begin
    SetPosition(0);
    exit;
  end;

  if FAutoRange < 0 then
    AutoCalcRange;

  if Value > FAutoRange then begin
    SetPosition(FAutoRange);
    exit;
  end;

  if Kind = sbVertical then
    SetVPosition
  else
    SetHPosition;
end;

procedure TControlScrollBar.SetRange(Value: Integer);
begin
  If Value < 0 then begin
    Range := 0;
    exit;
  end;
  FControl.FAutoScroll := False;
  FRange := Value;
  FControl.UpdateScrollBars;
end;

procedure TControlScrollBar.SetVisible(Value: Boolean);
begin
  FVisible := Value;
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
    if Kind = sbVertical then
      AutoCalcVRange
    else
      AutoCalcHRange;
    FControl.FAutoScroll := True;
  end;
end;

procedure TControlScrollBar.UpdateScrollBar;
var
  ScrollInfo: TScrollInfo;

  procedure UpdateVScroll;
  begin
    With FControl do begin
      ScrollInfo.nPage := ClientHeight + 1;

      if Visible then begin
        FAutoRange := (Range - ClientHeight)*Shortint(Range >= ClientHeight);
        ScrollInfo.nMax := Range;
      end
      else
        ScrollInfo.nMax := 0;
      SetScrollInfo(Handle, SB_VERT, ScrollInfo, FVisible or
        (FAutoScroll and (ScrollInfo.nMax > 0) and (ScrollInfo.nMax > Height)));
    end;
  end;

  procedure UpdateHScroll;
  begin
    With FControl do begin
      ScrollInfo.nPage := ClientWidth + 1;

      if Visible then begin
        FAutoRange := (Range - ClientWidth)*Shortint(Range >= ClientWidth);
        ScrollInfo.nMax := Range;
      end
      else
        ScrollInfo.nMax := 0;
      SetScrollInfo(Handle, SB_HORZ, ScrollInfo, FVisible or
        (FAutoScroll and (ScrollInfo.nMax > 0) and (ScrollInfo.nMax > Width)));
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

  SetPosition(FPosition);
end;

constructor TControlScrollBar.Create(AControl: TScrollingWinControl; AKind: TScrollBarKind);
begin
  Inherited Create;
  FControl := AControl;
  FKind := AKind;
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
            (FControl.Visible) and FVisible;
end;

function TControlScrollBar.ScrollPos: Integer;
begin
  Result := Position*ShortInt(Visible);
end;

procedure TScrollingWinControl.SetAutoScroll(Value: Boolean);
begin
  if FAutoScroll <> Value then
  begin
    FAutoScroll := Value;
    if not Value then begin
      HorzScrollBar.Range := 0;
      VertScrollBar.Range := 0;
    end
    else begin
      HorzScrollBar.AutoCalcRange;
      VertScrollBar.AutoCalcRange;
    end;
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
  inherited AlignControls(AControl, ARect);
end;

Procedure TScrollingWinControl.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  with Message do
    try
      FCanvas.Handle := DC;
      try
        FCanvas.Color := Self.Color;
        FCanvas.FillRect(ClientRect);
      finally
        FCanvas.Handle := 0;
      end;
    finally
      FCanvas.Unlock;
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
  VertScrollbar.Position := Message.Pos;
end;

Procedure TScrollingWinControl.WMHScroll(var Message : TLMHScroll);
begin
  HorzScrollbar.Position := Message.Pos;
end;

Constructor TScrollingWinControl.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;

  FAutoScroll := True;
  FVertScrollbar := TControlScrollBar.Create(Self, sbVertical);
  FHorzScrollbar := TControlScrollBar.Create(Self, sbHorizontal);
end;

Destructor TScrollingWinControl.Destroy;
begin
  FHorzScrollBar.Free;
  FVertScrollBar.Free;
  FCanvas.Free;
  inherited Destroy;
end;

