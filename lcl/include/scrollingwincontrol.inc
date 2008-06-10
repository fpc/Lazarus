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
  if FAutoScroll = Value then exit;
  FAutoScroll := Value;
  if Value then begin
    HorzScrollBar.AutoCalcRange;
    VertScrollBar.AutoCalcRange;
  end;
  UpdateScrollBars;
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

function TScrollingWinControl.GetLogicalClientRect: TRect;
begin
  Result:=ClientRect;
  {if (FHorzScrollBar.Range>Result.Right)
  or (FVertScrollBar.Range>Result.Bottom) then
    DebugLn(['TScrollingWinControl.GetLogicalClientRect Client=',ClientWidth,'x',ClientHeight,' Ranges=',FHorzScrollBar.Range,'x',FVertScrollBar.Range]);}
  if (FHorzScrollBar.Range>Result.Right) then
    Result.Right:=FHorzScrollBar.Range;
  if (FVertScrollBar.Range>Result.Bottom) then
    Result.Bottom:=FVertScrollBar.Range;
end;

procedure TScrollingWinControl.AlignControls(AControl: TControl;
  var ARect: TRect);
begin
  if (HorzScrollBar=nil) or (VertScrollBar=nil) then exit;
  inherited AlignControls(AControl, ARect);
  HorzScrollBar.AutoCalcRange;
  VertScrollBar.AutoCalcRange;
  if not AutoScroll then
    UpdateScrollBars;
end;

procedure TScrollingWinControl.DoOnResize;
begin
  inherited DoOnResize;
  if AutoScroll or HorzScrollBar.Visible or VertScrollBar.Visible
  then
    UpdateScrollBars;
end;

class function TScrollingWinControl.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=150;
  Result.Y:=150;
end;

procedure TScrollingWinControl.SetHorzScrollBar(Value: TControlScrollBar);
begin
  FHorzScrollbar.Assign(Value);
end;

procedure TScrollingWinControl.SetVertScrollBar(Value: TControlScrollBar);
begin
  FVertScrollbar.Assign(Value);
end;

function TScrollingWinControl.ComputeScrollbars: Boolean;
// true if something changed
// update Page, AutoRange, Visible

  procedure UpdateRange(p_Bar: TControlScrollBar);
  var
    SBSize: Longint;
    OtherScrollbar: TControlScrollBar;
    OldAutoRange: LongInt;
  begin
    OldAutoRange := p_Bar.FAutoRange;
    p_Bar.FAutoRange := 0;
    OtherScrollbar := p_Bar.GetOtherScrollBar;
    if OtherScrollbar.FVisible then
      SBSize := OtherScrollbar.Size
    else
      SBSize := 0;
    if p_Bar.Kind = sbVertical then
      SBSize := ClientHeight - SBSize
    else
      SBSize := ClientWidth - SBSize;
    if (p_Bar.FRange > SBSize) and (SBSize>0) then
      p_Bar.FAutoRange := (p_Bar.FRange - SBSize)
    else
      p_Bar.FAutoRange := 0;
    {$IFDEF VerboseScrollingWinControl}
    if p_Bar.DebugCondition then
      DebugLn(['UpdateRange p_Bar.fRange=',p_Bar.fRange,' SBSize=',SBSize,' ClientWidth=',ClientWidth,' FAutoRange=',p_Bar.FAutoRange]);
    {$ENDIF}
    if OldAutoRange <> p_Bar.FAutoRange then
      Result := true;
  end;
  
  procedure UpdateVisible(p_Bar: TControlScrollBar);
  var
    CurMax: Integer;
    OldVisible: Boolean;
  begin
    OldVisible:=p_Bar.FVisible;
    if p_Bar.Kind = sbVertical then
      CurMax := Height
    else
      CurMax := Width;
    if (p_Bar.FVisible and not FAutoScroll)
      or (FAutoScroll and (p_Bar.FRange > 0) and (p_Bar.FRange > CurMax))
    then
      p_Bar.FVisible := True
    else
      p_Bar.FVisible := False;
    if OldVisible <> p_Bar.FVisible then
      Result := true;
  end;
  
var
  NewPage: Integer;
begin
  Result := false;
  // page
  NewPage := Max(1,Min(ClientWidth -1, High(HorzScrollbar.FPage)));
  if NewPage <> HorzScrollbar.FPage then
  begin
    HorzScrollbar.FPage := NewPage;
    Result := true;
  end;
  NewPage := Max(1,Min(ClientHeight -1, High(VertScrollbar.FPage)));
  if NewPage <> VertScrollbar.FPage then
  begin
    VertScrollbar.FPage := NewPage;
    Result := true;
  end;
  // range
  UpdateRange(HorzScrollbar);
  UpdateRange(VertScrollbar);
  // visible
  UpdateVisible(HorzScrollbar);
  UpdateVisible(VertScrollbar);
end;

procedure TScrollingWinControl.UpdateScrollbars;
begin
  if ([csLoading,csDestroying]*ComponentState<>[]) then exit;
  if not HandleAllocated then exit;
  
  if FIsUpdating then exit;
  FIsUpdating := True;
  try
    ComputeScrollbars; // page, autorange, visible
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

function TScrollingWinControl.StoreScrollBars : Boolean;
begin
  Result := Not AutoScroll;
end;

procedure TScrollingWinControl.ScrollBy(DeltaX, DeltaY: Integer);
begin
  if HandleAllocated then begin
    TWSScrollingWinControlClass(WidgetSetClass).ScrollBy(Self, DeltaX, DeltaY);
    Invalidate;
  end;
end;

procedure TScrollingWinControl.ScrollbarHandler(ScrollKind: TScrollBarKind;
  OldPosition: Integer);
begin
  if ScrollKind = sbVertical then
    ScrollBy(0, FVertScrollBar.Position - OldPosition)
  else
    ScrollBy(FHorzScrollBar.Position - OldPosition, 0);
end;

procedure TScrollingWinControl.Loaded;
begin
  inherited Loaded;
  UpdateScrollbars;
end;

procedure TScrollingWinControl.WMVScroll(var Message : TLMVScroll);
begin
  VertScrollbar.ScrollHandler(Message);
end;

procedure TScrollingWinControl.WMHScroll(var Message : TLMHScroll);
begin
  //DebugLn(['TScrollingWinControl.WMHScroll ',dbgsName(Self)]);
  HorzScrollbar.ScrollHandler(Message);
end;

constructor TScrollingWinControl.Create(TheOwner : TComponent);
begin
  Inherited Create(TheOwner);

  FAutoScroll := true;
  FVertScrollbar := TControlScrollBar.Create(Self, sbVertical);
  FHorzScrollbar := TControlScrollBar.Create(Self, sbHorizontal);

  ControlStyle := [csAcceptsControls, csClickEvents, csDoubleClicks];

  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
end;

destructor TScrollingWinControl.Destroy;
begin
  FreeThenNil(FHorzScrollBar);
  FreeThenNil(FVertScrollBar);
  inherited Destroy;
end;

// included by forms.pp
