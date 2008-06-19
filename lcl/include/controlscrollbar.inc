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
  IntfBarKind: array[TScrollBarKind] of Integer =
  (
    SB_HORZ,
    SB_VERT
  );

procedure TControlScrollBar.SetPosition(const Value: Integer);
var
  OldPosition: Integer;
begin
  if Value < 0 then
  begin
    SetPosition(0);
    exit;
  end;

  if ControlAutoScroll then
  begin
    if FAutoRange < 0 then
      AutoCalcRange;

    if Value > FAutoRange then
    begin
      {$IFDEF VerboseScrollingWinControl}
      if DebugCondition then
        DebugLn(['TControlScrollBar.SetPosition FAutoRange Value=',Value,' > AutoRange=',FAutoRange]);
      {$ENDIF}
      SetPosition(FAutoRange);
      exit;
    end;
  end;

  if Value > Range then
  begin
    {$IFDEF VerboseScrollingWinControl}
    if DebugCondition then
      DebugLn(['TControlScrollBar.SetPosition Range Value=',Value,' > Range=',Range]);
    {$ENDIF}
    SetPosition(Range);
    exit;
  end;

  {$IFDEF VerboseScrollingWinControl}
  if DebugCondition then
    DebugLn(['TControlScrollBar.SetPosition Value=',Value,' FPosition=',FPosition]);
  {$ENDIF}
  if Value = FPosition then
    exit;

  // scroll logical client area of FControl
  OldPosition := FPosition;
  FPosition := Value;
  if FControl is TScrollingWinControl then
    TScrollingWinControl(FControl).ScrollbarHandler(Kind, OldPosition);

  // check that the new position is also set on the scrollbar
  if HandleAllocated and (GetScrollPos(ControlHandle, IntfBarKind[Kind]) <> FPosition) then
  begin
    InvalidateScrollInfo;
    {$IFDEF VerboseScrollingWinControl}
    if DebugCondition then
      DebugLn(['TControlScrollBar.SetPosition FPosition=',FPosition]);
    {$ENDIF}
    // send position to interface and store it back to FPosition (this way LCL will have actual position value)
    FPosition := SetScrollPos(ControlHandle, IntfBarKind[Kind], FPosition, Visible);
  end;
end;

function TControlScrollBar.SmoothIsStored: boolean;
begin
  Result := FSmooth;
end;

function TControlScrollBar.GetIncrement: TScrollBarInc;
begin
  Result := FIncrement;
end;

function TControlScrollBar.GetPage: TScrollBarInc;
var
  ScrollInfo: TScrollInfo;
begin
  if HandleAllocated and (not (FControl is TScrollingWinControl)) then
  begin
    ScrollInfo.fMask := SIF_PAGE;
    GetScrollInfo(ControlHandle, IntfBarKind[Kind], ScrollInfo);
    InvalidateScrollInfo;
    FPage := ScrollInfo.nPage;
  end;
  Result := FPage;
end;

function TControlScrollBar.GetPosition: Integer;
var
  ScrollInfo: TScrollInfo;
begin
  if HandleAllocated and (not (FControl is TScrollingWinControl)) then
  begin
    ScrollInfo.fMask := SIF_POS;
    GetScrollInfo(ControlHandle, IntfBarKind[Kind], ScrollInfo);
    InvalidateScrollInfo;
    FPosition := ScrollInfo.nPos;
  end;
  Result := FPosition;
end;

function TControlScrollBar.GetRange: Integer;
var
  ScrollInfo: TScrollInfo;
begin
  if HandleAllocated and (not (FControl is TScrollingWinControl)) then
  begin
    ScrollInfo.fMask := SIF_Range + SIF_Page;
    GetScrollInfo(ControlHandle, IntfBarKind[Kind], ScrollInfo);
    InvalidateScrollInfo;
    FRange := ScrollInfo.nMax - ScrollInfo.nMin - integer(ScrollInfo.nPage);
  end;
  Result := FRange;
end;

function TControlScrollBar.GetSmooth: Boolean;
begin
  Result := FSmooth;
end;

function TControlScrollBar.GetVisible: Boolean;
begin
  if HandleAllocated and (not (FControl is TScrollingWinControl)) then
  begin
    InvalidateScrollInfo;
    FVisible := GetScrollbarVisible(Controlhandle, IntfBarKind[Kind]);
  end;
  Result := FVisible;
end;

procedure TControlScrollBar.SetIncrement(const AValue: TScrollBarInc);
begin
  // This value is only used by the ScrollHandler procedure
  FIncrement := AValue;
end;

procedure TControlScrollBar.SetPage(const AValue: TScrollBarInc);
begin
  if FPage = AValue then exit;
  FPage := AValue;
  ControlUpdateScrollBars;
end;

function TControlScrollBar.VisibleIsStored: boolean;
begin
  Result := FVisible;
end;

function TControlScrollBar.GetSize: integer;
var
  KindID: integer;
begin
  if Kind = sbHorizontal then
    KindID := SM_CYHSCROLL
  else
    KindID := SM_CXVSCROLL;
  if HandleAllocated then
  begin
    Result := LCLIntf.GetScrollBarSize(ControlHandle,KindID);
    InvalidateScrollInfo;
  end else
    Result := GetSystemMetrics(KindID);
end;

procedure TControlScrollBar.SetRange(const Value: Integer);
begin
  if Value < 0 then
  begin
    Range := 0;
    exit;
  end;
  if FRange = Value then
    exit;
  FRange := Value;
  {$IFDEF VerboseScrollingWinControl}
  if DebugCondition then
    DebugLn(['TControlScrollBar.SetRange ',Self,' fRange=',FRange]);
  {$ENDIF}
  ControlUpdateScrollBars;
end;

procedure TControlScrollBar.SetSize(const AValue: integer);
begin
  raise EScrollBar.Create('[TControlScrollBar.SetPage] Size is readonly');
end;

procedure TControlScrollBar.SetVisible(const Value: Boolean);
begin
  if FVisible = Value then
    exit;
  FVisible := Value;
  ControlUpdateScrollBars;
end;

procedure TControlScrollBar.SetSmooth(const Value: Boolean);
begin
  // only used by the ScrollHandler procedure
  FSmooth := Value;
end;

procedure TControlScrollBar.AutoCalcRange;

  function IsNonAligned(Control: TControl): boolean;
  begin
    Result:=(Control.Align=alNone)
      and (Control.Anchors=[akLeft,akTop])
      and (Control.AnchorSide[akLeft].Control=nil)
      and (Control.AnchorSide[akTop].Control=nil);
  end;

  procedure AutoCalcVRange;
  var
    I: Integer;
    TmpRange: Longint;
    c: TControl;
  begin
    TmpRange := 0;
    For I := 0 to FControl.ControlCount - 1 do begin
      c:=FControl.Controls[I];
      if not c.IsControlVisible then continue;
      if c.Align=alCustom then continue;
      if akBottom in c.Anchors then continue;
      if (c.Align<>alNone) and (akBottom in AnchorAlign[c.Align]) then continue;
      if (FControl.ChildSizing.Layout<>cclNone) and IsNonAligned(c) then continue;
      TmpRange := Max(TmpRange, c.Top + c.Height);
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
    for i := 0 to FControl.ControlCount - 1 do begin
      c:=FControl.Controls[I];
      if not c.IsControlVisible then continue;
      if c.Align=alCustom then continue;
      if akRight in c.Anchors then continue;
      if (c.Align<>alNone) and (akRight in AnchorAlign[c.Align]) then continue;
      if (FControl.ChildSizing.Layout<>cclNone)
      and IsNonAligned(c) then continue;
      TmpRange := Max(TmpRange, c.Left + c.Width);
    end;
    Range := TmpRange;
  end;

begin
  if ControlAutoScroll then
  begin
    FVisible := True;
    if Kind = sbVertical then
      AutoCalcVRange
    else
      AutoCalcHRange;
  end;
end;

procedure TControlScrollBar.UpdateScrollBar;
var
  ScrollInfo: TScrollInfo;
begin
  if HandleAllocated and (FControl is TScrollingWinControl) then
  begin
    FillChar(ScrollInfo,SizeOf(ScrollInfo),0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL;
    ScrollInfo.nMin := 0;
    ScrollInfo.nMax := FRange;
    ScrollInfo.nPos := FPosition;
    ScrollInfo.nPage := FPage;
    ScrollInfo.nTrackPos := FPosition;
    if (not FOldScrollInfoValid) or (not CompareMem(@ScrollInfo,@FOldScrollInfo,SizeOf(TScrollInfo))) then
    begin
      FOldScrollInfo:=ScrollInfo;
      FOldScrollInfoValid := true;
      SetScrollInfo(FControl.Handle, IntfBarKind[Kind], ScrollInfo, FVisible);
    end;
    if FOldHandleVisible<>FVisible then begin
      FOldHandleVisible:=FVisible;
      ShowScrollBar(FControl.Handle,IntfBarKind[Kind],FVisible);
    end;
    {$IFDEF VerboseScrollingWinControl}
    if DebugCondition then
      DebugLn(['TControlScrollBar.UpdateScrollBar ',DbgSName(FControl),' ',DbgSName(Self),' FVisible=',FVisible,' Range=',FRange,' FPosition=',FPosition,' FPage=',FPage,' FAutoRange=',FAutoRange]);
    {$ENDIF}
  end;

  SetPosition(FPosition);

  if FControl is TScrollingWinControl then
  begin
    // I am not positive that this is right, but it appeared to be when I
    // compared results to Delphi 4
    if FSmooth then
      FIncrement := FPage div 10;
  end;
end;

procedure TControlScrollBar.InvalidateScrollInfo;
begin
  FOldScrollInfoValid:=false;
end;

{$ifdef VerboseScrollingWinControl}
function TControlScrollBar.DebugCondition: Boolean;
begin
  Result := (Kind = sbHorizontal);
end;
{$endif}

function TControlScrollBar.ControlAutoScroll: boolean;
begin
  if FControl is TScrollingWinControl then
    Result := TScrollingWinControl(FControl).AutoScroll
  else
    Result := false;
end;

procedure TControlScrollBar.ScrollHandler(var Message: TLMScroll);
var
  NewPos: Longint;
begin
  if (csDesigning in FControl.ComponentState) then
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
  {$IFDEF VerboseScrollingWinControl}
  if DebugCondition then
    DebugLn(['TControlScrollBar.ScrollHandler Message.ScrollCode=',Message.ScrollCode,' FPosition=',FPosition,' NewPos=',NewPos,' Range=',Range]);
  {$ENDIF}
  if NewPos < 0 then
    NewPos := 0;
  if NewPos > FRange then
    NewPos := FRange;
  InvalidateScrollInfo;
  SetPosition(NewPos);
end;

procedure TControlScrollBar.ControlUpdateScrollBars;
begin
  if ([csLoading, csDestroying] * FControl.ComponentState <> []) then
    exit;
  if not HandleAllocated then
    exit;
  if FControl is TScrollingWinControl then
    TScrollingWinControl(FControl).UpdateScrollBars;
end;

function TControlScrollBar.HandleAllocated: boolean;
begin
  Result := (FControl<>nil) and (FControl.HandleAllocated);
end;

function TControlScrollBar.ControlHandle: HWnd;
begin
  Result := FControl.Handle;
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
  if Source is TControlScrollBar then
  begin
    with Source as TControlScrollBar do
    begin
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
    Result := Position
  else
    Result := 0;
end;

function TControlScrollBar.GetOtherScrollBar: TControlScrollBar;
begin
  if Kind = sbVertical then
    Result := GetHorzScrollBar
  else
    Result := GetVertSCrollbar;
end;

function TControlScrollBar.GetHorzScrollBar: TControlScrollBar;
begin
  if FControl is TScrollingWinControl then
    Result := TScrollingWinControl(FControl).HorzScrollBar;
end;

function TControlScrollBar.GetVertScrollBar: TControlScrollBar;
begin
  if FControl is TScrollingWinControl then
    Result := TScrollingWinControl(FControl).VertScrollBar;
end;

// included by forms.pp
