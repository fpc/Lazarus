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

const
  IntfBarKind: array[TScrollBarKind] of Integer = (SB_HORZ,SB_VERT);

procedure TControlScrollBar.SetPosition(const Value: Integer);
var
  OldPosition: Integer;
begin
  if Value < 0 then begin
    SetPosition(0);
    exit;
  end;

  If ControlAutoScroll then begin
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

  // scroll content of FControl
  OldPosition:=FPosition;
  FPosition := Value;
  if Kind = sbVertical then
    ScrollControlBy(0, OldPosition - FPosition)
  else
    ScrollControlBy(OldPosition - FPosition, 0);

  // check that the new position is also set on the scrollbar
  if HandleAllocated
  and (GetScrollPos(ControlHandle, IntfBarKind[Kind]) <> FPosition) then
    SetScrollPos(ControlHandle, IntfBarKind[Kind], FPosition, Visible);
end;

function TControlScrollBar.SmoothIsStored: boolean;
begin
  Result:=FSmooth;
end;

function TControlScrollBar.GetIncrement: TScrollBarInc;
begin
  Result:=FIncrement;
end;

function TControlScrollBar.GetPage: TScrollBarInc;
var ScrollInfo: TScrollInfo;
begin
  if HandleAllocated and (not (FControl is TScrollingWinControl)) then begin
    ScrollInfo.fMask:=SIF_PAGE;
    GetScrollInfo(ControlHandle,IntfBarKind[Kind],ScrollInfo);
    FPage:=ScrollInfo.nPage;
  end;
  Result:=FPage;
end;

function TControlScrollBar.GetPosition: Integer;
var ScrollInfo: TScrollInfo;
begin
  if HandleAllocated and (not (FControl is TScrollingWinControl)) then begin
    ScrollInfo.fMask:=SIF_POS;
    GetScrollInfo(ControlHandle,IntfBarKind[Kind],ScrollInfo);
    FPosition:=ScrollInfo.nPos;
  end;
  Result:=FPosition;
end;

function TControlScrollBar.GetRange: Integer;
var ScrollInfo: TScrollInfo;
begin
  if HandleAllocated and (not (FControl is TScrollingWinControl)) then begin
    ScrollInfo.fMask:=SIF_Range+SIF_Page;
    GetScrollInfo(ControlHandle,IntfBarKind[Kind],ScrollInfo);
    FRange:=ScrollInfo.nMax-ScrollInfo.nMin-integer(ScrollInfo.nPage);
  end;
  Result:=FRange;
end;

function TControlScrollBar.GetSmooth: Boolean;
begin
  Result:=FSmooth;
end;

function TControlScrollBar.GetVisible: Boolean;
begin
  if HandleAllocated and (not (FControl is TScrollingWinControl)) then begin
    FVisible:=GetScrollbarVisible(Controlhandle,IntfBarKind[Kind]);
  end;
  Result:=FVisible;
end;

procedure TControlScrollBar.SetIncrement(const AValue: TScrollBarInc);
begin
  // This value is only used by the ScrollHandler procedure
  FIncrement:=AValue;
end;

procedure TControlScrollBar.SetPage(const AValue: TScrollBarInc);
begin
  if FPage=AValue then exit;
  FPage:=AValue;
  ControlUpdateScrollBars;
end;

function TControlScrollBar.VisibleIsStored: boolean;
begin
  Result:=FVisible;
end;

function TControlScrollBar.GetSize: integer;
var
  KindID: integer;
begin
  if Kind=sbHorizontal then
    KindID:=SM_CXHSCROLL
  else
    KindID:=SM_CXVSCROLL;
  if HandleAllocated then
    Result:=LCLIntf.GetScrollBarSize(ControlHandle,KindID)
  else
    Result:=GetSystemMetrics(KindID);
end;

procedure TControlScrollBar.SetRange(const Value: Integer);
begin
  If Value < 0 then begin
    Range := 0;
    exit;
  end;
  if FRange=Value then exit;
  FRange := Value;
  ControlUpdateScrollBars;
end;

procedure TControlScrollBar.SetSize(const AValue: integer);
begin
  Raise EScrollBar.Create('[TControlScrollBar.SetPage] Size is readonly');
end;

procedure TControlScrollBar.SetVisible(const Value: Boolean);
begin
  if FVisible = Value then exit;
  FVisible := Value;
  ControlUpdateScrollBars;
end;

procedure TControlScrollBar.ScrollControlBy(DeltaX, DeltaY: integer);
begin
  if FControl is TScrollingWinControl then
    TScrollingWinControl(FControl).ScrollBy(DeltaX, DeltaY);
end;

procedure TControlScrollBar.SetSmooth(const Value: Boolean);
begin
  // only used by the ScrollHandler procedure
  FSmooth := Value;
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
    i: Integer;
    TmpRange : Longint;
    c: TControl;
  begin
    TmpRange := 0;
    for i := 0 to FControl.ControlCount - 1 do
    begin
      c := FControl.Controls[I];
      if not C.IsControlVisible then Continue;
      if (c.Align <> alLeft) and (c.Align <> alNone) then Continue;
      
      // the left of a control is negative when it is scrolled to the left,
      // so add FPosition
      // MWE: temporary disabled until WM_MOVE messages are send for all gtklayout childs
      TmpRange := Max(TmpRange, {FPosition +} c.Left + c.Width);
      {if c.Left < 0
      then DebugLn('Child.Left = %d, Control.Left = %d', [c.Left, FControl.Left]);}
    end;
    Range := TmpRange;
  end;

begin
  if ControlAutoScroll then begin
    FVisible := True;
    if Kind = sbVertical then
      AutoCalcVRange
    else
      AutoCalcHRange;
  end;
  ControlUpdateScrollBars;
end;

procedure TControlScrollBar.UpdateScrollBar;
var
  ScrollInfo: TScrollInfo;
  SBSize : Longint;
  OtherScrollbar: TControlScrollBar;
  CurMax: Integer;
begin
  FAutoRange := 0;

  if FControl is TScrollingWinControl then begin
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL;
    ScrollInfo.nMin := 0;
    ScrollInfo.nPos := FPosition;
    ScrollInfo.nTrackPos := FPosition;

    With TScrollingWinControl(FControl) do begin
      // page
      if Self.Kind=sbVertical then
        FPage := TScrollBarInc(Min(ClientHeight - 1,High(FPage)))
      else
        FPage := TScrollBarInc(Min(ClientWidth - 1,High(FPage)));
      ScrollInfo.nPage := FPage;

      // range
      OtherScrollbar:=GetOtherScrollBar;
      If OtherScrollbar.FVisible then
        SBSize := OtherScrollbar.Size
      else
        SBSize := 0;
      if Kind=sbVertical then begin
        SBSize:=ClientHeight - SBSize;
      end else begin
        SBSize:=ClientWidth - SBSize;
      end;
      if (fRange>SBSize) and (SBSize>0) then   FAutoRange := (FRange - SBSize)
                                        else   FAutoRange := 0;
      ScrollInfo.nMax := FRange;

      // visible
      if Kind=sbVertical then
        CurMax:=Height
      else
        CurMax:=Width;
      If (Self.FVisible and not FAutoScroll)
      or (FAutoScroll and (ScrollInfo.nMax > 0) and (ScrollInfo.nMax > CurMax))
      then
        Self.FVisible := True
      else
        Self.FVisible := False;

      // transmit scollinfo to interface
      if HandleAllocated then
        SetScrollInfo(Handle, IntfBarKind[Kind], ScrollInfo, Self.FVisible);
    end
  end;

  SetPosition(ScrollInfo.nTrackPos);

  if FControl is TScrollingWinControl then begin
    // I am not positive that this is right, but it apeared to be when I
    // compared results to Delphi 4
    if FSmooth then
      FIncrement := FPage div 10;
  end;
end;

function TControlScrollBar.ControlAutoScroll: boolean;
begin
  if FControl is TScrollingWinControl then
    Result:=TScrollingWinControl(FControl).AutoScroll
  else
    Result:=false;
end;

procedure TControlScrollBar.ScrollHandler(var Message: TLMScroll);
var
  NewPos: Longint;
begin
  If (csDesigning in FControl.ComponentState) then
    exit; //prevent wierdness in IDE.

  NewPos := FPosition;
  case Message.ScrollCode of
    SB_LINEUP:
      Dec(NewPos, FIncrement);
    SB_LINEDOWN:
      Inc(NewPos, FIncrement);
    SB_PAGEUP:
      Dec(NewPos, FPage);
    SB_PAGEDOWN:
      Inc(NewPos, FPage);
    SB_THUMBPOSITION, SB_THUMBTRACK:
      NewPos := Message.Pos;
    SB_TOP:
      NewPos := 0;
    SB_BOTTOM:
      NewPos := Range;
  else
    Exit;
  end;
  if NewPos < 0 then NewPos := 0;
  if NewPos > Range then NewPos := Range;
  SetPosition(NewPos);
end;

procedure TControlScrollBar.ControlUpdateScrollBars;
begin
  if csLoading in FControl.ComponentState then exit;
  if FControl is TScrollingWinControl then
    TScrollingWinControl(FControl).UpdateScrollBars;
end;

function TControlScrollBar.HandleAllocated: boolean;
begin
  Result:=(FControl<>nil) and (FControl.HandleAllocated)
end;

function TControlScrollBar.ControlHandle: HWnd;
begin
  Result:=FControl.Handle;
end;

constructor TControlScrollBar.Create(AControl: TWinControl;
  AKind: TScrollBarKind);
begin
  Inherited Create;
  FControl := AControl;
  FKind := AKind;
  FPage := 80;
  FIncrement := 8;
  FPosition := 0;
  FRange := 0;
  FSmooth := false;
  FVisible := false;
end;

procedure TControlScrollBar.Assign(Source: TPersistent);
begin
  If Source is TControlScrollBar then begin
    With Source as TControlScrollBar do begin
      Self.Increment := Increment;
      Self.Position := Position;
      Self.Range := Range;
      Self.Visible := Visible;
      Self.Smooth := Smooth;
      // page and size depend on FControl, so no need to copy them
    end;
  end
  else
    inherited Assign(Source);
end;

function TControlScrollBar.IsScrollBarVisible: Boolean;
begin
  Result := (FControl <> nil) and FControl.HandleAllocated and
            (FControl.IsControlVisible) and (Self.Visible);
end;

function TControlScrollBar.ScrollPos: Integer;
begin
  if Visible then
    Result:=Position
  else
    Result:=0;
end;

function TControlScrollBar.GetOtherScrollBar: TControlScrollBar;
begin
  if Kind=sbVertical then
    Result:=GetHorzScrollBar
  else
    Result:=GetVertSCrollbar;
end;

function TControlScrollBar.GetHorzScrollBar: TControlScrollBar;
begin
  if FControl is TScrollingWinControl then
    Result:=TScrollingWinControl(FControl).HorzScrollBar;
end;

function TControlScrollBar.GetVertScrollBar: TControlScrollBar;
begin
  if FControl is TScrollingWinControl then
    Result:=TScrollingWinControl(FControl).VertScrollBar;
end;

// included by forms.pp
