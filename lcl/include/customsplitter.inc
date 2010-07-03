{%MainUnit ../extctrls.pp}

{******************************************************************************
                                   TCustomSplitter
 ******************************************************************************

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

function FindOppositeControl(Control: TControl): TControl;
var
  i,count: Integer;
  CurControl: TControl;
  fRect: TRect;
  fPoint: TPoint;
  alignList: TAlignSet;
begin
  Result := nil;
  count := Control.Parent.ControlCount;
  if count > 0 then
  begin
    fRect := Control.BoundsRect;
    case Control.Align of
      alTop: begin
          fPoint := fRect.BottomRight;
          fPoint.y := fPoint.y+1;
          Dec(fPoint.x);
          alignList := [alTop,alRight,alClient];
        end;
      alBottom: begin
          fPoint := fRect.TopLeft;
          Dec(fPoint.y);
          alignList := [alLeft,alBottom,alClient];
        end;
      alLeft: begin
          fPoint := fRect.BottomRight;
          Inc(fPoint.x);
          Dec(fPoint.y);
          alignList := [alLeft,alClient];
        end;
      alRight: begin
          fPoint := fRect.TopLeft;
          Dec(fPoint.x);
          alignList := [alRight,alClient];
        end;
    end;  // case
    Dec(count);
    for i := 0 to count do
    begin
      CurControl := Control.Parent.Controls[i];
      if (CurControl <> Control) and
         CurControl.Visible and
         (CurControl.Align in alignList) and
         (PtInRect(CurControl.BoundsRect, fPoint)) then Result := CurControl;
      if Assigned(Result) then Break;
    end;  // for i
  end;
end;

function FindVirtualOppositeControl(Control: TControl): TControl;

  function CompPos(CurControl, Control: TControl): Boolean;
  begin
    Result := False;
    case Control.Align of
      alTop:
        if (CurControl.Align = Control.Align) and
           (CurControl.Top >= Control.BoundsRect.Bottom) then Result := True;
      alBottom:
        if (CurControl.Align = Control.Align) and
           (CurControl.BoundsRect.Bottom <= Control.Top) then Result := True;
      alLeft:
        if (CurControl.Align = Control.Align) and
           (CurControl.Left >= Control.BoundsRect.Right) then Result := True;
      alRight:
        if (CurControl.Align = Control.Align) and
           (CurControl.BoundsRect.Right <= Control.Left) then Result := True;
    end;
  end;

  function OppositeControl(CurControl,Control: TControl): Boolean;
  begin
    Result := False;
    case Control.Align of
      alLeft: if (CurControl.Align = alRight) then Result := True;
      alRight: if (CurControl.Align = alLeft) then Result := True;
      alTop: if (CurControl.Align = alBottom) then Result := True;
      alBottom: if (CurControl.Align = alTop) then Result := True;
    end;
  end;

var
  i,count: Integer;
  CurControl: TControl;
begin
  Result := nil;
  count := Control.Parent.ControlCount;
  if count > 0 then
  begin
    Dec(count);
    for i := 0 to count do
    begin
      CurControl := Control.Parent.Controls[i];
      if (CurControl <> Control) then
      begin
        if ((Result = nil) and OppositeControl(CurControl, Control)) or
           (Assigned(Result) and CompPos(CurControl, Result)) then
          Result := CurControl;
      end;
    end;  // for i
  end;
end;

{ TCustomSplitter }

class procedure TCustomSplitter.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomSplitter;
end;

function TCustomSplitter.GetResizeControl: TControl;
begin
  if Align in [alLeft,alRight,alTop,alBottom] then
    Result := FindAlignControl
  else
    Result := AnchorSide[ResizeAnchor].Control;
end;

function TCustomSplitter.GetOtherResizeControl: TControl;
begin
  if Align in [alLeft, alRight, alTop, alBottom] then
    Result := FindAlignOtherControl
  else
    Result := AnchorSide[OppositeAnchor[ResizeAnchor]].Control;
end;

procedure TCustomSplitter.MoveSplitter(Offset: integer);
var
  CurResizeControl,
  LastControl, VirtualOppositeControl, CurOtherResizeControl: TControl;

  function GetParentClientSize: Integer;
  begin
    case ResizeAnchor of
      akLeft, akRight: Result := Parent.ClientWidth;
      akTop, akBottom: Result := Parent.ClientHeight;
    end;
  end;

  function GetControlMinPos(Control: TControl): Integer;
  begin
    if Assigned(Control)
    then
      case ResizeAnchor of
        akLeft,akRight: Result := Control.Left;
        akTop,akBottom: Result := Control.Top;
      end
    else
      case ResizeAnchor of
        akLeft,akTop: Result := 0;
        akRight,akBottom: Result := GetParentClientSize;
      end;
  end;

  function GetControlSize(Control: TControl): Integer;
  begin
    Result := 0;
    if Assigned(Control) then
      case ResizeAnchor of
        akLeft, akRight: Result := Control.Width;
        akTop, akBottom: Result := Control.Height;
      end;
  end;

  function GetControlConstraintsMinSize(Control: TControl): Integer;
  begin
    case ResizeAnchor of
      akLeft, akRight: Result := Control.Constraints.EffectiveMinWidth;
      akTop, akBottom: Result := Control.Constraints.EffectiveMinHeight;
    end;
  end;

  function GetControlConstraintsMaxSize(Control: TControl): Integer;
  begin
    case ResizeAnchor of
      akLeft, akRight: Result := Control.Constraints.EffectiveMaxWidth;
      akTop, akBottom: Result := Control.Constraints.EffectiveMaxHeight;
    end;
  end;

  procedure SetAlignControlSize(NewSize: Integer);
  var
    NewBounds: TRect;
  begin
    NewBounds := CurResizeControl.BoundsRect;
    //DebugLn('SetAlignControlSize ',DbgSName(Self),' CurResizeControl=',DbgSName(CurResizeControl),' OldBounds=',dbgs(NewBounds),' NewSize=',dbgs(NewSize));
    case ResizeAnchor of
      akLeft:
        NewBounds.Right := NewBounds.Left + NewSize;
      akRight:
        NewBounds.Left := NewBounds.Right - NewSize;
      akTop:
        NewBounds.Bottom := NewBounds.Top + NewSize;
      akBottom:
        NewBounds.Top := NewBounds.Bottom - NewSize;
    end;
    //DebugLn('SetAlignControlSize ',DbgSName(Self),' CurResizeControl=',DbgSName(CurResizeControl),' NewBounds=',dbgs(NewBounds));
    CurResizeControl.BoundsRect := NewBounds;
    //DebugLn('SetAlignControlSize ',DbgSName(Self),' CurResizeControl=',DbgSName(CurResizeControl),' NowBounds=',dbgs(CurResizeControl.BoundsRect));
  end;

  procedure DrawAlignControlSize(NewSize: Integer);
  var
    NewRect: TRect;
    OldSize: Integer;
  begin
    // get the splitter position
    NewRect := BoundsRect;
    NewRect.TopLeft := Parent.ClientToScreen(NewRect.TopLeft);
    NewRect.BottomRight := Parent.ClientToScreen(NewRect.BottomRight);

    // offset it accordinly
    OldSize := GetControlSize(CurResizeControl);
    case ResizeAnchor of
      akLeft:
        OffsetRect(NewRect, NewSize - OldSize, 0);
      akRight:
        OffsetRect(NewRect, OldSize - NewSize, 0);
      akTop:
        OffsetRect(NewRect, 0, NewSize - OldSize);
      akBottom:
        OffsetRect(NewRect, 0, OldSize - NewSize);
    end;
    SetRubberBandRect(FSplitterWindow, NewRect);
  end;

  function CalcNewSize(StartSize, EndSize, Offset: Integer): Integer;
  var
    NewSize: Integer;
  begin
    NewSize := GetControlSize(CurResizeControl);
    case ResizeAnchor of
      akLeft, akTop:     Inc(NewSize, Offset);
      akRight, akBottom: Dec(NewSize, Offset);
    end;

    if NewSize > EndSize then NewSize := EndSize;
    if NewSize < StartSize then NewSize := StartSize;

    if AutoSnap and (NewSize < FMinSize) then
      NewSize := StartSize;
    Result := NewSize;
  end;

  function GetVirtualControlMinPos(Control: TControl): Integer;
  begin
    case ResizeAnchor of
      akLeft:   Result := Control.Left;
      akRight:  Result := Control.BoundsRect.Right;
      akTop:    Result := Control.Top;
      akBottom: Result := Control.BoundsRect.Bottom;
    end;
  end;

  function FindClientControl: TControl;
  var
    CurControl: TControl;
    count: Integer;
  begin
    Result := nil;
    for count := Parent.ControlCount - 1 downto 0 do
    begin
      CurControl := Parent.Controls[count];
      if (CurControl <> Self) and
         ((CurControl.Align = alClient) or ((Self.Align in [alTop,alBottom]) and
          (CurControl.Align in [alLeft,alRight]))) then
      begin
        Result := CurControl;
        Break;
      end;
    end;  // for count
  end;  // function FindClientControl

  function FindLastControl(Control: TControl): TControl;
  var
    CurControl: TControl;
  begin
    CurControl := Control;
    while Assigned(CurControl) do
    begin
      Control := CurControl;
      CurControl := FindOppositeControl(Control);
    end;
    Result := Control;
  end;

  function GetParentsClientLimit: integer;
  // returns the maximum size of the CurResizeControl due to parent's client
  // area
  begin
    if ResizeAnchor in [akLeft, akRight] then
    begin
      if ResizeAnchor = akRight then
        Result := CurResizeControl.Left + CurResizeControl.Width - Width
      else
        Result := Parent.ClientWidth - CurResizeControl.Left - Width;
    end else
    begin
      if ResizeAnchor = akBottom then
        Result := CurResizeControl.Top + CurResizeControl.Height - Height
      else
        Result := Parent.ClientHeight - CurResizeControl.Top - Height;
    end;
  end;

  function GetParentsClientSize: integer;
  begin
    if ResizeAnchor in [akLeft, akRight] then
      Result := Parent.ClientWidth
    else
      Result := Parent.ClientHeight;
  end;

var
  StartSize: Integer;
  EndSize: Integer;
  NewSize: Integer;
  i: Integer;
  OffsetMaxLower: integer;
  OffsetMaxUpper: integer;
  CurMaxShrink: integer;
  CurMaxEnlarge: integer;
  NewRect: TRect;
begin
  //DebugLn('TCustomSplitter.MoveSplitter ',DbgSName(Self),' Offset=',dbgs(Offset));
  if Offset = 0 then Exit;

  if Align in [alLeft, alTop, alRight, alBottom] then
  begin
    // aligned Splitter
    // -> consider aligned siblings for minimum and maximum movement

    // get the control to resize
    CurResizeControl := GetResizeControl;
    if not Assigned(CurResizeControl) then Exit;
    CurOtherResizeControl := GetOtherResizeControl;

    // calculate minimum size
    StartSize := 1;
    if not AutoSnap then
      Inc(StartSize, Max(FMinSize, GetControlConstraintsMinSize(CurResizeControl)));
    if StartSize > 1 then Dec(StartSize);

    // calculate maximum size
    if Assigned(CurOtherResizeControl) then
      EndSize := GetControlSize(CurResizeControl) +
                 GetControlSize(CurOtherResizeControl) -
                 Max(FMinSize, GetControlConstraintsMinSize(CurOtherResizeControl))
    else
    begin
      VirtualOppositeControl := FindVirtualOppositeControl(Self);
      LastControl := FindLastControl(Self);
      case ResizeAnchor of
        akLeft, akTop:
        begin
          if Assigned(VirtualOppositeControl) then
            EndSize := GetControlSize(CurResizeControl) +
                       (GetControlMinPos(VirtualOppositeControl) -
                       (GetControlMinPos(LastControl) + GetControlSize(LastControl)))
          else
            EndSize := GetControlSize(CurResizeControl) +
                       (GetParentClientSize -
                       GetControlMinPos(LastControl) - GetControlSize(LastControl))
        end;

        akRight, akBottom:
        begin
          if Assigned(VirtualOppositeControl) then
            EndSize := GetControlSize(CurResizeControl) +
                       (GetControlMinPos(LastControl) -
                       (GetControlMinPos(VirtualOppositeControl) + GetControlSize(VirtualOppositeControl)))
          else
            EndSize := GetControlSize(CurResizeControl) + GetControlMinPos(LastControl);
        end;
      end;
    end;

    //DebugLn('TCustomSplitter.MoveSplitter ',DbgSName(Self),' StartSize=',dbgs(StartSize),' EndSize=',dbgs(EndSize),' Offset=',dbgs(Offset));
    NewSize := CalcNewSize(StartSize, EndSize, Offset);

    // OnCanResize event
    if CheckNewSize(NewSize) then
      if not FSplitDragging or (ResizeStyle = rsUpdate) then
        SetAlignControlSize(NewSize)
      else
        DrawAlignControlSize(NewSize);
  end else
  begin
    // anchored Splitter
    // -> consider anchored siblings for minimum and maximum movement

    // OffsetMaxLower = maximum the Splitter can be moved top/left
    OffsetMaxLower := Max(0, GetControlMinPos(Self) - FMinSize);
    // OffsetMaxUpper = maximum the Splitter can be moved bottom/right
    OffsetMaxUpper := Max(0, GetParentsClientSize -GetControlSize(Self) - GetControlMinPos(Self));
    //DebugLn(['TCustomSplitter.MoveSplitter OffsetMaxLower=',OffsetMaxLower,' OffsetMaxUpper=',OffsetMaxUpper]);
    for i := 0 to AnchoredControlCount - 1 do
    begin
      CurResizeControl := AnchoredControls[i];
      //debugln('TCustomSplitter.MoveSplitter ',DbgSName(Self),' CurResizeControl=',DbgSName(CurResizeControl));
      if (CurResizeControl.AnchorSide[ResizeAnchor].Control = Self) or
         (CurResizeControl.AnchorSide[OppositeAnchor[ResizeAnchor]].Control = Self) then
      begin
        // this control uses this Splitter as anchor
        // => moving the Splitter will resize CurResizeControl
        // => consider the constraints of CurResizeControl
        //    for minimum and maximum movement

        // calculate how much the CurResizeControl can be shrinked
        CurMaxShrink := Max(0, GetControlSize(CurResizeControl) - GetControlConstraintsMinSize(CurResizeControl));
        // calculate how much the CurResizeControl can be enlarged
        CurMaxEnlarge := Max(0, GetControlConstraintsMaxSize(CurResizeControl) - GetControlSize(CurResizeControl));
        if CurMaxEnlarge=0 then
        begin
          CurMaxEnlarge := GetParentsClientSize;
          if GetControlMinPos(CurResizeControl) < 0 then
            dec(CurMaxEnlarge, GetControlMinPos(CurResizeControl));
        end;
        //debugln('TCustomSplitter.MoveSplitter ',DbgSName(Self),' CurResizeControl=',DbgSName(CurResizeControl),' CurMaxShrink=',dbgs(CurMaxShrink),' CurMaxEnlarge=',dbgs(CurMaxEnlarge),' ConstraintsMax=',dbgs(GetControlConstraintsMaxSize(CurResizeControl)));

        // apply to the offset boundaries
        if (CurResizeControl.AnchorSide[akLeft].Control = Self) or
           (CurResizeControl.AnchorSide[akTop].Control = Self) then
        begin
          if CurMaxShrink > 0 then
            OffsetMaxLower := Min(OffsetMaxLower, CurMaxShrink);
          if CurMaxEnlarge > 0 then
            OffsetMaxUpper := Min(OffsetMaxUpper, CurMaxEnlarge);
        end else
        begin
          if CurMaxEnlarge > 0 then
            OffsetMaxLower := Min(OffsetMaxLower, CurMaxEnlarge);
          if CurMaxShrink > 0 then
            OffsetMaxUpper := Min(OffsetMaxUpper, CurMaxShrink);
        end;
      end;
    end;

    // apply the offset boundaries to the offset
    Offset := Max(Min(Offset, OffsetMaxUpper), -OffsetMaxLower);
    //DebugLn(['TCustomSplitter.MoveSplitter Offset=',Offset,' OffsetMaxLower=',OffsetMaxLower,' OffsetMaxUpper=',OffsetMaxUpper]);

    // move splitter
    if not FSplitDragging or (ResizeStyle = rsUpdate) then
    begin
      if ResizeAnchor in [akLeft, akRight] then
        Left := Left + Offset
      else
        Top := Top + Offset;
    end
    else
    begin
      // get the splitter position
      NewRect := BoundsRect;
      NewRect.TopLeft := Parent.ClientToScreen(NewRect.TopLeft);
      NewRect.BottomRight := Parent.ClientToScreen(NewRect.BottomRight);

      if ResizeAnchor in [akLeft, akRight] then
        OffsetRect(NewRect, Offset, 0)
      else
        OffsetRect(NewRect, 0, Offset);
      SetRubberBandRect(FSplitterWindow, NewRect);
    end;
  end;
end;

procedure TCustomSplitter.SetSplitterPosition(NewPosition: integer);
begin
  //DebugLn('TCustomSplitter.SetSplitterPosition ',DbgSName(Self),' NewPosition=',dbgs(NewPosition),' ',dbgs(GetSplitterPosition));
  MoveSplitter(NewPosition - GetSplitterPosition);
end;

function TCustomSplitter.GetSplitterPosition: integer;
begin
  if ResizeAnchor in [akLeft, akRight] then
    Result := Left
  else
    Result := Top;
end;

procedure TCustomSplitter.SetBeveled(const AValue: boolean);
begin
  if FBeveled = AValue then Exit;
  FBeveled := AValue;
  Invalidate;
end;

procedure TCustomSplitter.SetMinSize(const AValue: integer);
begin
  if (FMinSize=AValue) or (AValue < 1) then Exit;
  FMinSize := AValue;
end;

procedure TCustomSplitter.SetResizeAnchor(const AValue: TAnchorKind);
begin
  if FResizeAnchor = AValue then Exit;
  FResizeAnchor := AValue;
  if ResizeAnchor in [akLeft, akRight] then
    Cursor := crHSplit
  else
    Cursor := crVSplit;
  if not (csLoading in ComponentState) then
    Align := alNone;
end;

procedure TCustomSplitter.SetResizeControl(const AValue: TControl);
begin
  if Align in [alLeft, alRight, alTop, alBottom] then
  begin
    if AValue <> nil then
    begin
      case Align of
        alLeft: Left := AValue.Left + 1;
        alTop:  Top := AValue.Top + 1;
        alRight: Left := AValue.Left - 1;
        alBottom: Top := AValue.Top - 1;
      end;
    end;
  end
  else
    AnchorSide[ResizeAnchor].Control := AValue;
end;

procedure TCustomSplitter.StartSplitterMove(const MouseXY: TPoint);
var
  NewRect: TRect;
  Pattern: HBrush;
begin
  if FSplitDragging then Exit;
  FSplitDragging := True;
  FSplitterStartMouseXY := MouseXY;
  FSplitterStartLeftTop := Point(Left, Top);
  if ResizeStyle in [rsLine, rsPattern] then
  begin
    NewRect := BoundsRect;
    NewRect.TopLeft := Parent.ClientToScreen(NewRect.TopLeft);
    NewRect.BottomRight := Parent.ClientToScreen(NewRect.BottomRight);

    if ResizeStyle = rsLine then
      Pattern := GetStockObject(BLACK_BRUSH)
    else
      Pattern := ThemeServices.DottedBrush;

    FSplitterWindow := CreateRubberband(NewRect, Pattern);
  end;
end;

procedure TCustomSplitter.StopSplitterMove(const MouseXY: TPoint);
var
  Offset: Integer;
begin
  if FSplitDragging then
  begin
    case ResizeAnchor of
      akLeft, akRight:
        Offset := (MouseXY.X - FSplitterStartMouseXY.X) - (Self.Left - FSplitterStartLeftTop.X);
      akTop, akBottom:
        Offset := (MouseXY.Y - FSplitterStartMouseXY.Y) - (Self.Top - FSplitterStartLeftTop.Y);
    else
      Offset := 0;
    end;

    FSplitDragging := False;
    if Offset <> 0 then
      MoveSplitter(Offset);

    if Assigned(OnMoved) then OnMoved(Self);
    if ResizeStyle in [rsLine, rsPattern] then
      DestroyRubberBand(FSplitterWindow);
  end;
end;

procedure TCustomSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  MousePos: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  // While resizing X, Y are not valid. Use absolute mouse position.
  if Button = mbLeft then
  begin
    GetCursorPos(MousePos);
    StartSplitterMove(MousePos);
  end;
end;

procedure TCustomSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Offset: Integer;
  MousePos: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  if (ssLeft in Shift) and (Parent <> nil) and (FSplitDragging) then
  begin
    // While resizing X, Y are not valid. Use the absolute mouse position.
    GetCursorPos(MousePos);
    case ResizeAnchor of
      akLeft, akRight:
        Offset := (MousePos.X - FSplitterStartMouseXY.X) - (Self.Left - FSplitterStartLeftTop.X);
      akTop, akBottom:
        Offset := (MousePos.Y - FSplitterStartMouseXY.Y) - (Self.Top - FSplitterStartLeftTop.Y);
    else
      Offset := 0;
    end;

    if Offset <> 0 then
      MoveSplitter(Offset);
  end;
end;

procedure TCustomSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MousePos: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  GetCursorPos(MousePos);
  StopSplitterMove(MousePos);
end;

function TCustomSplitter.FindAlignControl: TControl;
var
  i: Integer;
  CurControl: TControl;
  BestValue: integer;
  
  procedure FindNearerControl(CurValue, Limit: integer);
  begin
    if (CurValue <= Limit) and ((Result = nil) or (BestValue < CurValue)) then
    begin
      BestValue := CurValue;
      Result := CurControl;
    end;
  end;
  
begin
  Result := nil;
  BestValue:=0;
  if (Parent = nil) then Exit;
  if not (Align in [alLeft,alTop,alRight,alBottom]) then exit;
  for i := Parent.ControlCount - 1 downto 0 do
  begin
    CurControl := Parent.Controls[i];
    if (CurControl <> Self) and (CurControl.Visible) and
       ((CurControl.Align = Self.Align) or (CurControl.Align=alClient)) then
    begin
      case Self.Align of
        alLeft:   FindNearerControl(CurControl.Left+CurControl.Width,Left);
        alTop:    FindNearerControl(CurControl.Top+CurControl.Height,Top);
        alRight:  FindNearerControl(-CurControl.Left,-Left-Width);
        alBottom: FindNearerControl(-CurControl.Top,-Top-Height);
      end;
    end;
  end;
end;

function TCustomSplitter.FindAlignOtherControl: TControl;
// if this splitter is aligned, then it returns the control that will be
// resized as well. Normally an alClient aligned control.
// Or: if alTop,alBottom the alLeft/alRight controls.
// And distance to oposite control should be minimal

  function CalcDistance(TestControl: TControl): Integer;
  var
    TestPosition,
    MyPosition: Integer;
  begin
    case Align of
      alLeft:
        begin
          MyPosition := BoundsRect.Right;
          TestPosition := TestControl.Left;
          Result := TestPosition - MyPosition;
        end;
      alRight:
        begin
          MyPosition := Left;
          TestPosition := TestControl.BoundsRect.Right;
          Result := MyPosition - TestPosition;
        end;
      alTop:
        begin
          MyPosition := BoundsRect.Bottom;
          TestPosition := TestControl.Top;
          Result := TestPosition - MyPosition;
        end;
      alBottom:
        begin
          MyPosition := Top;
          TestPosition := TestControl.BoundsRect.Bottom;
          Result := MyPosition - TestPosition;
        end;
      else
        Result := -1;
    end;
  end;

var
  CurControl: TControl;
  i, CurDistance, MinDistance: Integer;
begin
  Result := nil;
  MinDistance := MaxInt;
  for i := Parent.ControlCount-1 downto 0 do
  begin
    CurControl := Parent.Controls[i];
    if (CurControl <> Self) and (CurControl.Visible) and
       (
         (CurControl.Align = alClient) or
         (
           (Self.Align in [alTop, alBottom]) and
           (CurControl.Align in [alLeft, alRight])
         )
       ) then
    begin
      CurDistance := CalcDistance(CurControl);
      if (CurDistance >= 0) and (CurDistance < MinDistance) then
      begin
        Result := CurControl;
        MinDistance := CurDistance;
        if CurDistance = 0 then
          Exit;
      end;
    end;
  end;
end;

procedure TCustomSplitter.SetAlign(Value: TAlign);
var
  OldWidth: Integer;
  OldHeight: Integer;
  OldResizeAnchor: TAnchorKind;
begin
  OldResizeAnchor:=ResizeAnchor;
  case Value of
    alLeft:   FResizeAnchor:=akLeft;
    alTop:    FResizeAnchor:=akTop;
    alRight:  FResizeAnchor:=akRight;
    alBottom: FResizeAnchor:=akBottom;
  end;
  if ((Align = Value) and (OldResizeAnchor = FResizeAnchor)) or (Value = alClient) then
    Exit;
  OldWidth := Width;
  OldHeight := Height;

  DisableAlign;
  try
    inherited SetAlign(Value);

    if ResizeAnchor in [akLeft,akRight] then
      Cursor := crHSplit
    else
      Cursor := crVSplit;

    CheckAlignment;

    // lfm contains correct size already
    if not (csLoading in ComponentState) then
    begin
      if (OldResizeAnchor in [akLeft,akRight])=(ResizeAnchor in [akLeft,akRight]) then
      begin
        // keep width and height
        SetBounds(Left,Top,OldWidth,OldHeight);
      end
      else
      begin
        // resize
        if Align in [alLeft,alRight] then
          Width:=OldHeight
        else if Align in [alTop,alBottom] then
          Height:=OldWidth;
      end;
    end;
  finally
    EnableAlign;
  end;
end;

procedure TCustomSplitter.SetAnchors(const AValue: TAnchors);
begin
  if AValue = Anchors then exit;
  inherited SetAnchors(AValue);
  CheckAlignment;
end;

procedure TCustomSplitter.CheckAlignment;
begin
  case Align of
    alLeft:   Anchors := Anchors - [akRight] + [akLeft];
    alTop:    Anchors := Anchors - [akBottom] + [akTop];
    alRight:  Anchors := Anchors + [akRight] - [akLeft];
    alBottom: Anchors := Anchors + [akBottom] - [akTop];
  end;
end;

function TCustomSplitter.CheckNewSize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnCanResize) then
    OnCanResize(Self, NewSize, Result);
end;

procedure TCustomSplitter.Paint;
  procedure DrawThemedPattern(ARect: TRect);
  const
    GripperDetailsPart: array[Boolean] of TThemedRebar =
    (
      trGripperVert,
      trGripper
    );
  var
    GripperRect: TRect;
    BgPart: TThemedRebar;
    BgDetails, GripperDetails: TThemedElementDetails;
    GripperSize: TSize;
  begin
    GripperDetails := ThemeServices.GetElementDetails(GripperDetailsPart[ResizeAnchor in [akLeft,akRight]]);

    if not Enabled then
      BgPart := trBandDisabled
    else
    if FMouseInControl then
      BgPart := trBandHot
    else
      BgPart := trBandNormal;

    BgDetails := ThemeServices.GetElementDetails(BgPart);
    ThemeServices.DrawElement(Canvas.Handle, BgDetails, ARect, nil);

    if Beveled then
      ThemeServices.DrawEdge(Canvas.Handle, BgDetails, ARect, BDR_RAISEDOUTER,
        BF_ADJUST or BF_RECT, @ARect);

    GripperRect := ARect;
    GripperSize := ThemeServices.GetDetailSize(GripperDetails);
    if (GripperSize.cx <> -1) or (GripperSize.cy <> -1) then
    begin
      if ResizeAnchor in [akLeft,akRight] then
      begin
        if (GripperRect.Bottom - GripperRect.Top) > GripperSize.cy then
        begin
          GripperRect.Top := (GripperRect.Top + GripperRect.Bottom - GripperSize.cy) div 2;
          GripperRect.Bottom := GripperRect.Top + GripperSize.cy;
        end;
      end
      else
      begin
        if (GripperRect.Right - GripperRect.Left) > GripperSize.cx then
        begin
          GripperRect.Left := (GripperRect.Left + GripperRect.Right - GripperSize.cx) div 2;
          GripperRect.Right := GripperRect.Left + GripperSize.cx;
        end;
      end;
    end;

    ThemeServices.DrawElement(Canvas.Handle, GripperDetails, GripperRect);
  end;

begin
  inherited Paint;

  DrawThemedPattern(ClientRect)
end;

procedure TCustomSplitter.MouseEnter;
begin
  inherited MouseEnter;
  if csDesigning in ComponentState then exit;

  if not FMouseInControl and Enabled and (GetCapture = 0) then
  begin
    FMouseInControl := True;
    invalidate;
  end;
end;

procedure TCustomSplitter.MouseLeave;
begin
  inherited MouseLeave;
  if csDesigning in ComponentState then exit;
  
  if FMouseInControl then
  begin
    FMouseInControl := False;
    invalidate;
  end;
end;

constructor TCustomSplitter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FResizeStyle := rsUpdate;
  FAutoSnap := True;
  FBeveled := False;
  FMinSize := 30;
  FMouseInControl := False;
  FResizeAnchor := akLeft;

  Align := alLeft;
  Width := 5;
end;

procedure TCustomSplitter.AnchorSplitter(Kind: TAnchorKind; AControl: TControl);
  
  procedure AnchorSplitterSides(
    ResizeSide,// the side of the Splitter, where AControl is touched and moved
    OppositeResizeSide, // opposite of ResizeSide
    FixedSide1,// the first non moving side
    FixedSide2:// the second non moving side
      TAnchorKind);
  begin
    Anchors:=Anchors-[OppositeResizeSide]+[ResizeSide,FixedSide1,FixedSide2];
    AnchorSide[OppositeResizeSide].Control:=nil;
    AnchorToNeighbour(ResizeSide,0,AControl);
    AnchorParallel(FixedSide1,0,AControl);
    AnchorParallel(FixedSide2,0,AControl);
  end;
  
var
  OldResizeAnchor: TAnchorKind;
  OldWidth: LongInt;
  OldHeight: LongInt;
begin
  OldWidth := Self.Width;
  OldHeight := Self.Height;
  OldResizeAnchor := FResizeAnchor;
  Align := alNone;
  FResizeAnchor := Kind;

  if ResizeAnchor in [akLeft,akRight] then
    Cursor := crHSplit
  else
    Cursor := crVSplit;

  case FResizeAnchor of
    akLeft:   AnchorSplitterSides(akLeft, akRight, akTop, akBottom);
    akRight:  AnchorSplitterSides(akRight, akLeft, akTop, akBottom);
    akTop:    AnchorSplitterSides(akTop, akBottom, akLeft, akRight);
    akBottom: AnchorSplitterSides(akBottom, akTop, akLeft, akRight);
  end;
  
  if (OldResizeAnchor in [akLeft, akRight]) = (ResizeAnchor in [akLeft, akRight]) then
  begin
    // keep width and height
    SetBounds(Left, Top, OldWidth, OldHeight);
  end else begin
    // resize
    if FResizeAnchor in [akLeft, akRight] then
      Width := OldHeight
    else
      Height := OldWidth;
  end;
end;

// included by extctrls.pp
