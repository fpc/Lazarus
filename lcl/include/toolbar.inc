{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TToolbar
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

}

function CompareToolBarControlHorz(Control1, Control2: TControl): integer;
var
  ToolBar: TToolBar;
  Row1: Integer;
  Row2: Integer;
  HalfBtnHeight, BtnHeight: Integer;
begin
  Result := 0;
  if not (Control1.Parent is TToolBar) then Exit;

  ToolBar := TToolBar(Control1.Parent);
  BtnHeight := ToolBar.ButtonHeight;
  if BtnHeight <= 0 then BtnHeight := 1;
  HalfBtnHeight := BtnHeight div 2;

  Row1 := (Control1.Top + HalfBtnHeight) div BtnHeight;
  Row2 := (Control2.Top + HalfBtnHeight) div BtnHeight;
  Result := CompareValue(Row1, Row2);
  if Result = 0 then
  begin
    Result := CompareValue(Control1.Left, Control2.Left);
    if ToolBar.UseRightToLeftAlignment then
      Result:=-Result;
  end;
  if Result = 0 then
  begin
    Row1 := ToolBar.GetControlIndex(Control1);
    Row2 := ToolBar.GetControlIndex(Control2);
    Result := CompareValue(Row1, Row2);
  end;
end;

function CompareToolBarControlVert(Control1, Control2: TControl): integer;
var
  ToolBar: TToolBar;
  Col1: Integer;
  Col2: Integer;
  HalfBtnWidth, BtnWidth: Integer;
begin
  Result := 0;
  if not (Control1.Parent is TToolBar) then Exit;

  ToolBar := TToolBar(Control1.Parent);
  BtnWidth := ToolBar.ButtonWidth;
  if BtnWidth <= 0 then BtnWidth := 1;
  HalfBtnWidth := BtnWidth div 2;

  Col1 := (Control1.Left + HalfBtnWidth) div BtnWidth;
  Col2 := (Control2.Left + HalfBtnWidth) div BtnWidth;
  Result := CompareValue(Col1, Col2);
  if Result = 0 then
    Result := CompareValue(Control1.Top, Control2.Top);
  if Result = 0 then
  begin
    Col1 := ToolBar.GetControlIndex(Control1);
    Col2 := ToolBar.GetControlIndex(Control2);
    Result := CompareValue(Col1, Col2);
  end;
end;

{------------------------------------------------------------------------------
  Method: TToolbar.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TToolBar.Create(TheOwner: TComponent);
var
  Details: TThemedElementDetails;
begin
  inherited Create(TheOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csMenuEvents, csSetCaption, csParentBackground, csOpaque];
  FFlat := True;
  Height := 32;
  Details := ThemeServices.GetElementDetails(ttbSplitButtonDropDownNormal);
  FThemeDropDownWidth := ThemeServices.GetDetailSize(Details).cx;
  Details := ThemeServices.GetElementDetails(ttbDropDownButtonNormal);
  FThemeButtonDropWidth := ThemeServices.GetDetailSize(Details).cx;
  FButtonHeight := -1;
  FButtonWidth := -1;
  FDropDownWidth := -1;
  FNewStyle := True;
  FWrapable := True;
  FButtons := TList.Create;
  FIndent := 1;
  FList := False;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  FDisabledImageChangeLink := TChangeLink.Create;
  FDisabledImageChangeLink.OnChange := @DisabledImageListChange;
  FHotImageChangeLink := TChangeLink.Create;
  FHotImageChangeLink.OnChange := @HotImageListChange;
  EdgeBorders := [ebTop];
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  Align := alTop;
end;

destructor TToolBar.Destroy;
var
  I: Integer;
begin
  for I := 0 to FButtons.Count - 1 do
    if TControl(FButtons[I]) is TToolButton then
      TToolButton(FButtons[I]).FToolBar := nil;

  FreeThenNil(FButtons);
  FreeThenNil(FHotImageChangeLink);
  FreeThenNil(FImageChangeLink);
  FreeThenNil(FDisabledImageChangeLink);
  inherited Destroy;
end;

procedure TToolBar.FlipChildren(AllLevels: Boolean);
begin
  if AllLevels then ;
  // no flipping
end;

procedure TToolBar.CreateWnd;
begin
  BeginUpdate;
  try
    DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TToolBar.CreateWnd'){$ENDIF};
    try
      inherited CreateWnd;
      UpdateVisibleBar;
    finally
      EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TToolBar.CreateWnd'){$ENDIF};
    end;
  finally
    EndUpdate;
  end;
end;

procedure TToolBar.AlignControls(AControl: TControl;
  var RemainingClientRect: TRect);
var
  NewWidth, NewHeight: integer;
begin
  if tbfPlacingControls in FToolBarFlags then exit;
  Include(FToolBarFlags, tbfPlacingControls);
  DisableAlign;
  try
    AdjustClientRect(RemainingClientRect);
    if IsVertical then
      WrapButtons(Height, NewWidth, NewHeight, False)
    else
      WrapButtons(Width, NewWidth, NewHeight, False);
  finally
    Exclude(FToolBarFlags, tbfPlacingControls);
    EnableAlign;
  end;
end;

procedure TToolBar.RepositionButton(Index: Integer);
begin
  if ([csLoading,csDestroying]*ComponentState<>[]) then exit;
  UpdateVisibleBar;
end;

procedure TToolBar.RepositionButtons(Index: Integer);
begin
  UpdateVisibleBar;
end;

procedure TToolBar.SetButtonHeight(const AValue: Integer);
begin
  SetButtonSize(ButtonWidth,AValue);
end;

procedure TToolBar.SetButtonWidth(const AValue: Integer);
begin
  SetButtonSize(AValue,ButtonHeight);
end;

procedure TToolBar.ToolButtonDown(AButton: TToolButton; NewDown: Boolean);
begin
  AButton.Down:=NewDown;
end;

procedure TToolBar.ImageListChange(Sender: TObject);
begin
  if (Sender = Images) then UpdateVisibleBar;
end;

procedure TToolBar.SetShowCaptions(const AValue: Boolean);
begin
  if FShowCaptions = AValue then exit;
  FShowCaptions := AValue;
  UpdateVisibleBar;
end;

procedure TToolBar.CloseCurrentMenu;
begin
  FCurrentMenu.Close;
  // move menu items back
  if Assigned(FSrcMenuItem) then
  begin
    MoveSubMenuItems(FCurrentMenu.Items, FSrcMenuItem);
    if Assigned(FDropDownButton) then
      FDropDownButton.Down := False;
  end;
end;

procedure TToolBar.MoveSubMenuItems(SrcMenuItem, DestMenuItem: TMenuItem);
var
  i: Integer;
  MovingMenuItem: TMenuItem;
begin
  if (SrcMenuItem = nil) or (DestMenuItem = nil) or (SrcMenuItem = DestMenuItem) then
    Exit;
  for i := SrcMenuItem.Count - 1 downto 0 do
  begin
    MovingMenuItem := SrcMenuItem.Items[i];
    SrcMenuItem.Delete(i);
    DestMenuItem.Insert(0, MovingMenuItem);
  end;
end;

procedure TToolBar.AddButton(Button: TToolButton);
begin
  FButtons.Add(Button);
end;

procedure TToolBar.RemoveButton(Button: TToolButton);
begin
  if FDropDownButton=Button then FDropDownButton:=nil;
  FButtons.Remove(Button);
end;

function TToolBar.IsVertical: Boolean;
begin
  if (Parent is TCoolBar) then
    Exit(TCoolBar(Parent).Vertical);

  if Align in [alNone, alClient, alCustom] then
    Result := Height > Width
  else
    Result := Align in [alLeft, alRight];
end;

class procedure TToolBar.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterToolBar;
end;

procedure TToolBar.ApplyFontForButtons;
var
  i: integer;
begin
  for i := 0 to ButtonCount - 1 do
    Buttons[i].Font := Font;
end;

function TToolBar.ButtonHeightIsStored: Boolean;
begin
  Result := FButtonHeight >= 0;
end;

function TToolBar.ButtonWidthIsStored: Boolean;
begin
  Result := FButtonWidth >= 0;
end;

function TToolBar.GetButton(Index: Integer): TToolButton;
begin
  Result := TToolButton(FButtons[Index]);
end;

function TToolBar.GetButtonCount: Integer;
begin
  Result := FButtons.Count;
end;

function TToolBar.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TToolBar.SetList(const AValue: Boolean);
begin
  if FList = AValue then exit;
  FList := AValue;
  UpdateVisibleBar;
end;

procedure TToolBar.SetFlat(const AValue: Boolean);
begin
  if FFlat = AValue then exit;
  FFlat := AValue;
  Invalidate;
end;

procedure TToolBar.SetTransparent(const AValue: Boolean);
begin
  if GetTransparent = AValue then exit;
  if AValue then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  Invalidate;
end;

procedure TToolBar.SetWrapable(const AValue: Boolean);
begin
  if FWrapable = AValue then exit;
  FWrapable := AValue;
  ReAlign;
end;

procedure TToolBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FImages then Images := nil;
    if AComponent = FHotImages then HotImages := nil;
    if AComponent = FDisabledImages then DisabledImages := nil;
  end;
end;

procedure TToolBar.SetImages(const AValue: TCustomImageList);
begin
  if FImages = AValue then Exit;
  if FImages <> nil then
    FImages.UnRegisterChanges(FImageChangeLink);
  FImages := AValue;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  UpdateVisibleBar;
end;

procedure TToolBar.SetImagesWidth(const aImagesWidth: Integer);
begin
  if FImagesWidth = aImagesWidth then Exit;
  FImagesWidth := aImagesWidth;
  UpdateVisibleBar;
end;

procedure TToolBar.DisabledImageListChange(Sender: TObject);
begin
  if (Sender = DisabledImages) then UpdateVisibleBar;
end;

procedure TToolBar.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  inherited;

  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    BeginUpdate;
    try
      if ButtonWidthIsStored then
        ButtonWidth := Round(ButtonWidth * AXProportion);
      if ButtonHeightIsStored then
        ButtonHeight := Round(ButtonHeight * AYProportion);
      if DropDownWidthIsStored then
        DropDownWidth := Round(DropDownWidth * AXProportion);
      FRealizedButtonHeight := 0;
      FRealizedButtonWidth := 0;
      FRealizedDropDownWidth := 0;
      FRealizedButtonDropWidth := 0;
      FToolBarFlags := FToolBarFlags + [tbfUpdateVisibleBarNeeded];
    finally
      EndUpdate;
    end;
  end;
end;

procedure TToolBar.SetDisabledImages(const AValue: TCustomImageList);
begin
  if FDisabledImages = AValue then Exit;
  if FDisabledImages <> nil then
    FDisabledImages.UnRegisterChanges(FDisabledImageChangeLink);
  FDisabledImages := AValue;
  if FDisabledImages <> nil then
  begin
    FDisabledImages.RegisterChanges(FDisabledImageChangeLink);
    FDisabledImages.FreeNotification(Self);
  end;
  UpdateVisibleBar;
end;

procedure TToolBar.SetDropDownWidth(const ADropDownWidth: Integer);
begin
  if FDropDownWidth = ADropDownWidth then Exit;
  FDropDownWidth := ADropDownWidth;
  UpdateVisibleBar;
end;

procedure TToolBar.HotImageListChange(Sender: TObject);
begin
  if (Sender = HotImages) then UpdateVisibleBar;
end;

procedure TToolBar.UpdateVisibleBar;
var
  i: Integer;
begin
  if ([csLoading,csDestroying]*ComponentState<>[]) or (not HandleAllocated) then
  begin
    Include(FToolBarFlags,tbfUpdateVisibleBarNeeded);
    Exit;
  end;
  for i := 0 to FButtons.Count - 1 do
  begin
    TControl(FButtons[i]).InvalidatePreferredSize;
    TControl(FButtons[i]).AdjustSize;
  end;
  AdjustSize;
  Invalidate;
  Exclude(FToolBarFlags,tbfUpdateVisibleBarNeeded);
end;

procedure TToolBar.SetHotImages(const AValue: TCustomImageList);
begin
  if FHotImages = AValue then Exit;
  if FHotImages <> nil then
    FHotImages.UnRegisterChanges(FHotImageChangeLink);
  FHotImages := AValue;
  if FHotImages <> nil then
  begin
    FHotImages.RegisterChanges(FHotImageChangeLink);
    FHotImages.FreeNotification(Self);
  end;
  UpdateVisibleBar;
end;

procedure TToolBar.SetIndent(const AValue: Integer);
begin
  if FIndent = AValue then exit;
  FIndent := AValue;
  UpdateVisibleBar;
end;

procedure TToolBar.Loaded;
begin
  inherited Loaded;
  UpdateVisibleBar;
end;

procedure TToolBar.EndUpdate;
begin
  inherited EndUpdate;
  if FUpdateCount=0 then begin
    if tbfUpdateVisibleBarNeeded in FToolBarFlags then
      UpdateVisibleBar;
  end;
end;

function TToolBar.GetEnumerator: TToolBarEnumerator;
begin
  Result := TToolBarEnumerator.Create(Self);
end;

function TToolBar.GetDropDownWidth: Integer;
begin
  if FDropDownWidth < 0 then
  begin
    if FRealizedDropDownWidth = 0 then
      FRealizedDropDownWidth := ScaleScreenToFont(FThemeDropDownWidth);
    Result := FRealizedDropDownWidth;
  end else
    Result := FDropDownWidth;
end;

function TToolBar.GetButtonDropWidth: Integer;
begin
  if FDropDownWidth < 0 then
  begin
    if FRealizedButtonDropWidth = 0 then
      FRealizedButtonDropWidth := ScaleScreenToFont(FThemeButtonDropWidth);
    Result := FRealizedButtonDropWidth;
  end else
    Result := FDropDownWidth+FThemeButtonDropWidth-FThemeDropDownWidth;
end;

function TToolBar.GetButtonHeight: Integer;
begin
  if FButtonHeight < 0 then
  begin
    if FRealizedButtonHeight = 0 then
      FRealizedButtonHeight := Scale96ToFont(cDefButtonHeight);
    Result := FRealizedButtonHeight;
  end else
    Result := FButtonHeight;
end;

function TToolBar.GetButtonWidth: Integer;
begin
  if FButtonWidth < 0 then
  begin
    if FRealizedButtonWidth = 0 then
      FRealizedButtonWidth := Scale96ToFont(cDefButtonWidth);
    Result := FRealizedButtonWidth;
  end else
    Result := FButtonWidth;
end;

procedure TToolBar.Paint;
begin
  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Color:=clRed;
    Canvas.FrameRect(Clientrect);
  end;
  inherited Paint;
  if Assigned(OnPaint) then
    OnPaint(Self);
end;

procedure TToolBar.SetButtonSize(NewButtonWidth, NewButtonHeight: integer);
var
  CurControl: TControl;
  NewWidth: Integer;
  NewHeight: Integer;
  i, RealButtonWidth, RealButtonHeight: Integer;
  ChangeW, ChangeH: Boolean;
begin
  ChangeW := FButtonWidth <> NewButtonWidth;
  ChangeH := FButtonHeight <> NewButtonHeight;
  if not (ChangeW or ChangeH) then Exit;
  
  FButtonWidth:=NewButtonWidth;
  FButtonHeight:=NewButtonHeight;
  RealButtonWidth := ButtonWidth;
  RealButtonHeight := ButtonHeight;
  if FUpdateCount > 0 then Exit;
  if [csLoading, csDestroying] * ComponentState <> [] then Exit;

  // set all children to ButtonWidth ButtonHeight
  BeginUpdate;
  try
    for i:=ControlCount-1 downto 0 do
    begin
      CurControl := Controls[i];
      CurControl.InvalidatePreferredSize;
      NewWidth := CurControl.Width;
      NewHeight := CurControl.Height;

      // width
      if ChangeW
      and (RealButtonWidth > 0)
      and not CurControl.AutoSize
      and (CurControl is TToolButton)
      and (CurControl.Align in [alNone, alLeft, alRight])
      then begin
        if TToolButton(CurControl).Style in [tbsButton,tbsCheck,tbsDropDown]
        then begin
          CurControl.GetPreferredSize(NewWidth,NewHeight);
          if NewWidth < RealButtonWidth then
            NewWidth := RealButtonWidth;
        end;
      end;
      
      // height
      // in horizontal toolbars the height is set by the toolbar independent of autosize
      if ChangeH
      and (RealButtonHeight > 0)
      and ((Align in [alTop, alBottom]) or not CurControl.AutoSize)
      then NewHeight := RealButtonHeight;
        
      CurControl.SetBounds(CurControl.Left, CurControl.Top, NewWidth, NewHeight);
    end;
  finally
    EndUpdate;
  end;
end;

function TToolBar.CanFocus: Boolean;
begin
  Result := False;
end;

procedure TToolBar.DoAutoSize;
begin
  // children are moved in ControlsAligned independent of AutoSize=true
end;

function TToolBar.DropDownWidthIsStored: Boolean;
begin
  Result := FDropDownWidth >= 0;
end;

procedure TToolBar.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  NewWidth: Integer;
  NewHeight: Integer;
  FixedWidth: Boolean;
begin
  NewWidth:=0;
  NewHeight:=0;

  FixedWidth:=false;
  if (Parent<>nil)
  and (not Parent.AutoSize)
  and AnchorSideLeft.IsAnchoredToParent(akLeft)
  and AnchorSideRight.IsAnchoredToParent(akRight) then begin
    // the width depends on the parent
    // the width is fixed
    FixedWidth:=true;
    WrapButtons(Width, NewWidth, NewHeight, True);
    PreferredWidth := NewWidth;
    PreferredHeight := NewHeight;
    //DebugLn(['TToolBar.CalculatePreferredSize fixed width: ',PreferredWidth,'x',PreferredHeight]);
  end;
  if not FixedWidth then begin
    WrapButtons(Screen.Width,NewWidth,NewHeight,true);
    PreferredWidth := NewWidth;
    PreferredHeight := NewHeight;
  end;
  //DebugLn(['TToolBar.CalculatePreferredSize ',DbgSName(Self),' ',PreferredWidth,'x',PreferredHeight,' Count=',ControlCount]);
end;

{------------------------------------------------------------------------------
  function TToolBar.WrapButtons(var NewWidth, NewHeight: Integer): Boolean;
  
  Position all controls, that have Align=alNone.
  The controls are put from left to right.
  If the controls don't fit in a row and Wrapable=true, then the next row is
  started.
  If Wrapable=false, then the row is wrapped after the first button with
  Wrap=true.
------------------------------------------------------------------------------}
function TToolBar.WrapButtons(UseSize: integer;
  out NewWidth, NewHeight: Integer; Simulate: boolean): Boolean;
var
  ARect: TRect;
  x, y, w, h: Integer;
  NewControlWidth, NewControlHeight: Integer;
  CurControl: TControl;
  ObstacleControls: TFPList;
  FullSizeObstacleControls: TFPList;
  StartX, StartY: Integer;
  Vertical: Boolean; // true = ToolBar is vertical, controls are put in rows
  RowsLeftToRight: Boolean; // rows are left to right
  RealButtonWidth, RealButtonHeight: Integer;

  procedure CalculatePosition;
  var
    AlignedControl: TControl;
    NewBounds: TRect;
    SiblingBounds: TRect;
    j: Integer;
    PreferredBtnWidth, PreferredBtnHeight: Integer;
    Intersects: Boolean;
    IntersectsWithLimitedHeightControl: Boolean;
    StartedAtRowStart: Boolean;
  begin
    // compute the size
    if (CurControl is TToolButton) and (not CurControl.AutoSize) then
    begin
      PreferredBtnWidth := 0;
      PreferredBtnHeight := 0;
      CurControl.GetPreferredSize(PreferredBtnWidth, PreferredBtnHeight);
      if Vertical then
      begin
        // column layout
        NewControlHeight := PreferredBtnHeight;
        NewControlWidth := RealButtonWidth;
      end
      else
      begin
        // row layout
        NewControlHeight := RealButtonHeight;
        NewControlWidth := PreferredBtnWidth;
      end;
      if (TToolButton(CurControl).Style in [tbsButton, tbsDropDown, tbsCheck]) then
      begin
        if Vertical then
        begin
          // column layout
          if (NewControlHeight < RealButtonHeight) then
            NewControlHeight := RealButtonHeight;
        end
        else begin
          // row layout
          if (NewControlWidth < RealButtonWidth) then
            NewControlWidth := RealButtonWidth;
        end;
      end;
      //debugln(['CalculatePosition preferred toolbutton size ',DbgSName(CurControl),' ',NewControlWidth,' ',NewControlHeight]);
    end
    else
    if Vertical then
    begin
      // column layout
      NewControlWidth := RealButtonWidth;
      NewControlHeight := CurControl.Height;
    end
    else
    begin
      // row layout
      NewControlWidth := CurControl.Width;
      NewControlHeight := RealButtonHeight;
    end;

    if Vertical or RowsLeftToRight then
      NewBounds := Bounds(x, y, NewControlWidth, NewControlHeight)
    else
      NewBounds := Bounds(x-NewControlWidth, y, NewControlWidth, NewControlHeight);

    //DebugLn(['CalculatePosition ',DbgSName(CurControl),' NewBounds=',dbgs(NewBounds),' x=',x,' y=',y]);
    if Vertical then
      StartedAtRowStart := y = StartY
    else
      StartedAtRowStart := x = StartX;
    repeat
      // move control until it does not overlap
      IntersectsWithLimitedHeightControl := False;
      j := 0;
      while j < ObstacleControls.Count do
      begin
        AlignedControl := TControl(ObstacleControls[j]);
        SiblingBounds := AlignedControl.BoundsRect;
        Intersects:=(SiblingBounds.Right > NewBounds.Left) and
                    (SiblingBounds.Left < NewBounds.Right) and
                    (SiblingBounds.Bottom > NewBounds.Top) and
                    (SiblingBounds.Top < NewBounds.Bottom);
        if Intersects then
        begin
          //DebugLn(['CalculatePosition Move ',NewBounds.Left,'->',SiblingBounds.Right]);
          if Vertical then
          begin
            // column layout
            NewBounds.Top := SiblingBounds.Bottom;
            NewBounds.Bottom := NewBounds.Top + NewControlHeight;
          end
          else
          begin
            // row layout
            if RowsLeftToRight then
            begin
              NewBounds.Left := SiblingBounds.Right;
              NewBounds.Right := NewBounds.Left + NewControlWidth;
            end else begin
              NewBounds.Right := SiblingBounds.Left;
              NewBounds.Left := NewBounds.Right - NewControlWidth;
            end;
          end;
          j := 0; // check again, needed, because ObstacleControls are not sorted
                // (and can not be sorted, because they can overlap)
          if FullSizeObstacleControls.IndexOf(AlignedControl) < 0 then
            IntersectsWithLimitedHeightControl := True;
        end
        else
          inc(j);
      end;
      if Vertical then
      begin
        // column layout
        if (not Wrapable) or
           (NewBounds.Bottom <= ARect.Bottom) or (NewBounds.Top = StartY) or
           (StartedAtRowStart and not IntersectsWithLimitedHeightControl) then
        begin
          // control fits into the row
          //DebugLn(['CalculatePosition fits: ',DbgSName(CurControl),' ',dbgs(NewBounds)]);
          x := NewBounds.Left;
          y := NewBounds.Top;
          break;
        end;

        // try next row
        NewBounds.Top := StartY;
        NewBounds.Bottom := NewBounds.Top + NewControlHeight;
        inc(NewBounds.Left, RealButtonWidth);
        inc(NewBounds.Right, RealButtonWidth);
      end
      else
      begin
        // row layout
        if (not Wrapable)
        or (StartedAtRowStart and not IntersectsWithLimitedHeightControl)
        or (RowsLeftToRight and ((NewBounds.Left = StartX) or (NewBounds.Right <= ARect.Right)))
        or ((not RowsLeftToRight) and ((NewBounds.Right = StartX) or (NewBounds.Left >= ARect.Left)))
        then begin
          // control fits into the row
          //DebugLn(['CalculatePosition fits: ',DbgSName(CurControl),' ',dbgs(NewBounds)]);
          x := NewBounds.Left;
          y := NewBounds.Top;
          break;
        end;

        //debugln(['CalculatePosition overlaps: ',DbgSName(CurControl),' ',dbgs(NewBounds),' ARect=',DbgS(ARect),' StartX=',StartX]);

        // try next row
        inc(NewBounds.Top, RealButtonHeight);
        inc(NewBounds.Bottom, RealButtonHeight);
        if RowsLeftToRight then
        begin
          NewBounds.Left := StartX;
          NewBounds.Right := NewBounds.Left + NewControlWidth;
        end else begin
          NewBounds.Right := StartX;
          NewBounds.Left := NewBounds.Right - NewControlWidth;
        end;
      end;
      StartedAtRowStart := True;
      //DebugLn('CalculatePosition Next Row ',DbgSName(CurControl),' ',dbgs(NewBounds));
    until false;
  end;

  function AnchoredToParent(AControl: TControl; Side: TAnchorKind): boolean;
  var
    AnchorControl: TControl;
    AnchorSide: TAnchorSideReference;
    p: integer;
  begin
    if not (Side in CurControl.Anchors) then exit(false);
    AnchorControl:=nil;
    CurControl.AnchorSide[Side].GetSidePosition(AnchorControl,AnchorSide,P);
    if AnchorControl=nil then
      AnchorControl:=CurControl;
    Result:=(Side in AnchorControl.Anchors);
  end;

var
  OrderedControls: TFPList;
  CurClientRect: TRect;
  AdjustClientFrame: TRect;
  i: Integer;
  GrowSide: TAnchorKind; // when a line is full, grow the TToolBar in this direction
  SeparatorWidthChange: Boolean;
begin
  //DebugLn(['WrapButtons ',DbgSName(Self),' Wrapable=',Wrapable,' ',dbgs(BoundsRect),' Vertical=',IsVertical,' RTL=',UseRightToLeftAlignment,' Simulate=',Simulate]);
  Result := True;
  RealButtonWidth := ButtonWidth;
  RealButtonHeight := ButtonHeight;
  Vertical := IsVertical;
  NewWidth := 0;
  NewHeight := 0;
  ObstacleControls := TFPList.Create;
  FullSizeObstacleControls := TFPList.Create;
  OrderedControls := TFPList.Create;
  if not Simulate then
    FRowCount := 0;
  DisableAlign;
  BeginUpdate;
  try
    if Vertical then
    begin
      GrowSide := akRight;
      RowsLeftToRight := true;
    end
    else begin
      GrowSide := akBottom;
      RowsLeftToRight:=not UseRightToLeftAlignment;
    end;
    for i:=0 to ControlCount-1 do
    begin
      CurControl := Controls[i];
      if CurControl.Align = alNone then begin
        // this control will be auto positioned and auto sized by this function
        // => set to Left,Top anchoring
        CurControl.Anchors:=[akLeft,akTop];
        CurControl.AnchorSide[akLeft].Control:=nil;
        CurControl.AnchorSide[akTop].Control:=nil;
        OrderedControls.Add(CurControl);
      end else begin
        // this control will be positioned/sized by the default LCL functions
        // the OrderedControls will be positioned around them (without overlapping)
        ObstacleControls.Add(CurControl);
        // check if this obstacle auto grows, for example if this toolbar is
        // aligned to the top, check if the obstacle grows downwards (Align=alLeft)
        if AnchoredToParent(CurControl,GrowSide) then begin
          // this obstacle auto grows (important for the wrap algorithm)
          FullSizeObstacleControls.Add(CurControl);
        end;
      end;
    end;
    // sort OrderedControls
    if Vertical then
      OrderedControls.Sort(TListSortCompare(@CompareToolBarControlVert))
    else
      OrderedControls.Sort(TListSortCompare(@CompareToolBarControlHorz));

    // position OrderedControls
    CurClientRect := ClientRect;
    if Vertical then
      inc(CurClientRect.Bottom, UseSize - Height)
    else
      inc(CurClientRect.Right, UseSize - Width);
    ARect := CurClientRect;
    AdjustClientRect(ARect);
    AdjustClientFrame.Left := ARect.Left - CurClientRect.Left;
    AdjustClientFrame.Top := ARect.Top - CurClientRect.Top;
    AdjustClientFrame.Right := CurClientRect.Right - ARect.Right;
    AdjustClientFrame.Bottom := CurClientRect.Bottom - ARect.Bottom;
    //DebugLn(['TToolBar.WrapButtons ',DbgSName(Self),' ARect=',dbgs(ARect)]);
    // important: top, left button must start in the corner of AdjustClientRect
    // otherwise Toolbar.AutoSize=true will create an endless loop
    if Vertical or RowsLeftToRight then
      StartX := ARect.Left
    else
      StartX := ARect.Right;
    StartY := ARect.Top;
    x := StartX;
    y := StartY;
    //debugln(['TToolBar.WrapButtons Start=',StartX,' ',StartY]);
    for i := 0 to OrderedControls.Count - 1 do
    begin
      CurControl := TControl(OrderedControls[i]);
      if not CurControl.IsControlVisible then
        Continue;
      CalculatePosition;
      //DebugLn(['WrapButtons ',DbgSName(CurControl),' ',x,',',y,',',CurControl.Width,'x',CurControl.Height]);
      if CurControl.AutoSize then
      begin
        w := CurControl.Width;
        h := CurControl.Height;
      end
      else
      begin
        w := NewControlWidth;
        h := NewControlHeight;
      end;

      w := CurControl.Constraints.MinMaxWidth(w);
      h := CurControl.Constraints.MinMaxHeight(h);
      SeparatorWidthChange := (CurControl is TToolButton) and
        (TToolButton(CurControl).Style in [tbsSeparator, tbsDivider]);
      if SeparatorWidthChange then begin
        if not Vertical then begin
          SeparatorWidthChange := (w <> CurControl.Width);
          w := CurControl.Width;
        end else begin
          SeparatorWidthChange := (h <> CurControl.Height);
          h := CurControl.Height;
        end;
      end;
      if Vertical <> FPrevVertical then  //swap h/w when orientation changed
      begin
        if (CurControl is TToolButton) and
          (TToolButton(CurControl).Style in [tbsSeparator, tbsDivider]) then
          begin
            if not Vertical then
              w := CurControl.Height
            else
              h := CurControl.Width;
          end;
      end;
      if (CurControl.Left <> x) or (CurControl.Top <> y) or
         (CurControl.Width <> w) or (CurControl.Height <> h) then
      begin
        //DebugLn(['TToolBar.WrapButtons moving child: ',DbgSName(CurControl),' Old=',dbgs(CurControl.BoundsRect),' New=',dbgs(Bounds(x,y,w,h))]);
        if not Simulate then
        begin
          //DebugLn(['TToolBar.WrapButtons moving child: ',DbgSName(CurControl),' Old=',dbgs(CurControl.BoundsRect),' New=',dbgs(Bounds(x,y,w,h))]);
          if SeparatorWidthChange then
            CurControl.SetBoundsKeepBase(x,y,w,h)
          else
            CurControl.SetBounds(x,y,w,h);
          //DebugLn(['TToolBar.WrapButtons moved child: ',DbgSName(CurControl),' ',dbgs(CurControl.BoundsRect)]);
        end;
      end;
      
      // adjust NewWidth, NewHeight
      if Vertical or RowsLeftToRight then
        NewWidth := Max(NewWidth, x + w + AdjustClientFrame.Right)
      else
        NewWidth := Max(NewWidth, ARect.Right - x + ARect.Left + AdjustClientFrame.Right);
      NewHeight := Max(NewHeight, y + h + AdjustClientFrame.Bottom);

      // step to next position
      if Vertical then
      begin
        inc(y, h);
        if not Wrapable and
          (CurControl is TToolButton) and
          (TToolButton(CurControl).Wrap) then
        begin
          // user forced wrap -> start new line
          y := StartY;
          inc(x, RealButtonWidth);
          if not Simulate then
            inc(FRowCount);
        end;
      end
      else
      begin
        if RowsLeftToRight then
          inc(x, w);
        if not Wrapable and
          (CurControl is TToolButton) and
          (TToolButton(CurControl).Wrap) then
        begin
          // user forced wrap -> start new line
          x := StartX;
          inc(y, RealButtonHeight);
          if not Simulate then
            inc(FRowCount);
        end;
      end;
    end;
  finally
    ObstacleControls.Free;
    OrderedControls.Free;
    FullSizeObstacleControls.Free;
    EndUpdate;
    EnableAlign;
    FPrevVertical := Vertical;
  end;
end;

procedure TToolBar.CNDropDownClosed(var Message: TLMessage);
begin
  CloseCurrentMenu;
end;

procedure TToolBar.AdjustClientRect(var ARect: TRect);
begin
  inherited AdjustClientRect(ARect);
  inc(ARect.Left, Indent);
end;

class function TToolBar.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 150;
  Result.CY := 26;
end;

function TToolBar.FindButtonFromAccel(Accel: Word): TToolButton;
var
  i: Integer;
begin
  for i := 0 to FButtons.Count - 1 do
    if TControl(FButtons[i]) is TToolButton then
    begin
      Result := Buttons[i];
      if Result.Visible and Result.Enabled and IsAccel(Accel, Result.Caption) then
        Exit;
    end;
  Result := nil;
end;

procedure TToolBar.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  ApplyFontForButtons;
  FRealizedButtonWidth := 0;
  FRealizedButtonHeight := 0;
  FRealizedDropDownWidth := 0;
  FRealizedButtonDropWidth := 0;
end;

function TToolBar.CheckMenuDropdown(Button: TToolButton): Boolean;
var
  APoint: TPoint;
begin
  Result := False;
  if Button = nil then
    Exit;
  if Assigned(FCurrentMenu) then
  begin
    CloseCurrentMenu;
    if FCurrentMenuAutoFree then
    begin
      FreeAndNil(FCurrentMenu);
      FCurrentMenuAutoFree := False;
    end;
  end;
  FSrcMenuItem := nil;
  FSrcMenu := nil;
  FDropDownButton := Button;
  if Assigned(Button.DropdownMenu) then
    // the button has a popupenu
    FCurrentMenu := Button.DropdownMenu
  else
  if Assigned(Button.MenuItem) then
  begin
    // the button has a menuitem

    // since the button is clicked - menu item must be clicked too
    Button.MenuItem.Click;
    // -> create a temporary TPopupMenu and move all child menuitems
    FCurrentMenuAutoFree := True;
    FCurrentMenu := TPopupMenu.Create(Self);
    FSrcMenuItem := Button.MenuItem;
    FSrcMenu := FSrcMenuItem.GetParentMenu;
    FCurrentMenu.Items.HelpContext := FSrcMenuItem.HelpContext;
    if Assigned(FSrcMenu) then
      FCurrentMenu.Images := FSrcMenu.Images;
    MoveSubMenuItems(FSrcMenuItem, FCurrentMenu.Items);
  end
  else
    Exit;
  FCurrentMenu.PopupComponent := Self;
  APoint := Button.ClientToScreen(Point(0, Button.ClientHeight));
  if FCurrentMenu.IsRightToLeft then Inc(APoint.X, Button.Width);
  FCurrentMenu.Popup(APoint.X, APoint.Y);
  // The next command will be executed after popup menu close because Popup is a
  // syncronous method. We can't send this message on Menu.Close event because
  // Click happen after the Close event and if we remove all the menu items there
  // we will not be able to handle the Click event
  // we also need to postpone this message to allow after Popup cleanup and click happen
  PostMessage(Handle, CN_DROPDOWNCLOSED, 0, 0);
  Result := True;
end;

procedure TToolBar.ClickButton(Button: TToolButton);
begin
  Button.Click;
end;

