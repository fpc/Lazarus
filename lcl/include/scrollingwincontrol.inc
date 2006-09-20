{%MainUnit ../forms.pp}

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

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

function TScrollingWinControl.GetClientScrollOffset: TPoint;
begin
  if (HorzScrollBar<>nil) and (VertScrollBar<>nil) then begin
    Result.X:=HorzScrollBar.Position;
    Result.Y:=VertScrollBar.Position;
  end else begin
    Result.X:=0;
    Result.Y:=0;
  end;
end;

procedure TScrollingWinControl.AlignControls(AControl: TControl;
  var ARect: TRect);
begin
  if (HorzScrollBar=nil) or (VertScrollBar=nil) then exit;
  HorzScrollBar.AutoCalcRange;
  VertScrollBar.AutoCalcRange;
  If not AutoScroll then
    UpdateScrollBars;
  inherited AlignControls(AControl, ARect);
end;

procedure TScrollingWinControl.DoOnResize;
begin
  inherited DoOnResize;
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

procedure TScrollingWinControl.ComputeScrollbars;

  procedure UpdateRange(p_Bar: TControlScrollBar);
  var
    SBSize: Longint;
    OtherScrollbar: TControlScrollBar;
  begin
    p_Bar.FAutoRange := 0;
    OtherScrollbar := p_Bar.GetOtherScrollBar;
    If OtherScrollbar.FVisible then
      SBSize := OtherScrollbar.Size
    else
      SBSize := 0;
    if p_Bar.Kind = sbVertical then
      SBSize := ClientHeight - SBSize
    else
      SBSize := ClientWidth - SBSize;
    if (p_Bar.fRange>SBSize) and (SBSize>0) then
      p_Bar.FAutoRange := (p_Bar.FRange - SBSize)
    else
      p_Bar.FAutoRange := 0;
  end;
  
  procedure UpdateVisible(p_Bar: TControlScrollBar);
  var
    CurMax: Integer;
  begin
    if p_Bar.Kind = sbVertical then
      CurMax := Height
    else
      CurMax := Width;
    If (p_Bar.FVisible and not FAutoScroll)
      or (FAutoScroll and (p_Bar.FRange > 0) and (p_Bar.FRange > CurMax))
    then
      p_Bar.FVisible := True
    else
      p_Bar.FVisible := False;
  end;
  
begin
  //todo: why doesn't it simply use ClientWidth/Height?
  // page
  HorzScrollbar.FPage := Min(ClientWidth -1, High(HorzScrollbar.FPage));
  VertScrollbar.FPage := Min(ClientHeight -1, High(VertScrollbar.FPage));
  // range
  UpdateRange(VertScrollbar);
  UpdateRange(VertScrollbar);
  // visible
  UpdateVisible(HorzScrollbar);
  UpdateVisible(VertScrollbar);
end;

Procedure TScrollingWinControl.UpdateScrollbars;
begin
  If FIsUpdating then exit;
  FIsUpdating := True;
  try
    ComputeScrollbars;
    FVertScrollbar.UpdateScrollbar;
    FHorzScrollbar.UpdateScrollbar;
  finally
    FIsUpdating := False;
  end;
end;

function TScrollingWinControl.HasVisibleScrollbars: boolean;
begin
  Result:=(VertScrollBar<>nil) and VertScrollBar.Visible
      and (HorzScrollBar<>nil) and HorzScrollBar.Visible;
end;

Function TScrollingWinControl.StoreScrollBars : Boolean;
begin
  Result := Not AutoScroll;
end;

procedure TScrollingWinControl.ScrollBy(DeltaX, DeltaY: Integer);
begin
  if HandleAllocated then begin
    TWSScrollingWinControlClass(WidgetSetClass).ScrollBy(Self, DeltaX, DeltaY);
    //todo: shouldn't call Invalidate. Instead, the TWidgetSet should call it if it needs to.
    Invalidate;
  end;
end;

procedure TScrollingWinControl.ScrollbarHandler(p_ScrollKind: TScrollBarKind; p_OldPosition: Integer);
begin
  if p_ScrollKind = sbVertical then
    ScrollBy(0, FVertScrollBar.Position - p_OldPosition)
  else
    ScrollBy(FHorzScrollBar.Position - p_OldPosition, 0);
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

  FVertScrollbar := TControlScrollBar.Create(Self, sbVertical);
  FHorzScrollbar := TControlScrollBar.Create(Self, sbHorizontal);

  ControlStyle := [csAcceptsControls, csClickEvents, csDoubleClicks];

  SetInitialBounds(0,0, 150, 150);
end;

Destructor TScrollingWinControl.Destroy;
begin
  FreeThenNil(FHorzScrollBar);
  FreeThenNil(FVertScrollBar);
  inherited Destroy;
end;

// included by forms.pp
