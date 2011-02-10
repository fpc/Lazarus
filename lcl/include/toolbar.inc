{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TToolbar
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

function CompareToolBarControl(Control1, Control2: TControl): integer;
var
  ToolBar: TToolBar;
  Row1: Integer;
  Row2: Integer;
  HalfBtnHeight, BtnHeight: Integer;
begin
  Result := 0;
  if not (Control1.Parent is TToolBar) then Exit;

  ToolBar := TToolBar(Control1.Parent);
  BtnHeight := ToolBar.FRealizedButtonHeight;
  if BtnHeight <= 0 then BtnHeight := 1;
  HalfBtnHeight := BtnHeight div 2;

  Row1 := (Control1.Top + HalfBtnHeight) div BtnHeight;
  Row2 := (Control2.Top + HalfBtnHeight) div BtnHeight;
  Result := CompareValue(Row1, Row2);
  if Result = 0 then
    Result := CompareValue(Control1.Left, Control2.Left);
  if Result = 0 then
  begin
    Row1 := ToolBar.GetControlIndex(Control1);
    Row2 := ToolBar.GetControlIndex(Control2);
    Result := CompareValue(Row1, Row2);
  end;
end;

{------------------------------------------------------------------------------
  Method: TToolbar.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TToolbar.Create(TheOwner : TComponent);
var
  Details: TThemedElementDetails;
begin
  inherited Create(TheOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csMenuEvents, csSetCaption, csParentBackground, csOpaque];
  FFlat := True;
  FButtonWidth := 23;
  FButtonHeight := 22;
  Details := ThemeServices.GetElementDetails(ttbSplitButtonDropDownNormal);
  FDropDownWidth := ThemeServices.GetDetailSize(Details).cx;
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

procedure TToolBar.ControlsAligned;
var
  NewWidth, NewHeight: integer;
begin
  if tbfPlacingControls in FToolBarFlags then exit;
  Include(FToolBarFlags, tbfPlacingControls);
  try
    WrapButtons(Width, NewWidth, NewHeight, False);
  finally
    Exclude(FToolBarFlags, tbfPlacingControls);
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

procedure TToolBar.OnTemporaryPopupMenuClose(Sender: TObject);
begin
  // move menu items back
  if FSrcMenuItem = nil then
    Exit;
  MoveSubMenuItems(FCurrentMenu.Items, FSrcMenuItem);
  if FDropDownButton <> nil then
    FDropDownButton.Down := False;
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
  // any other logic?
  Result := Height > Width;
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
  if Operation = opRemove then begin
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

procedure TToolBar.DisabledImageListChange(Sender: TObject);
begin
  if (Sender = DisabledImages) then UpdateVisibleBar;
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

procedure TToolBar.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TToolBar.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount=0 then begin
    if tbfUpdateVisibleBarNeeded in FToolBarFlags then
      UpdateVisibleBar;
  end;
end;

function TToolBar.GetEnumerator: TToolBarEnumerator;
begin
  Result := TToolBarEnumerator.Create(Self);
end;

procedure TToolBar.Paint;
begin
  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Color:=clRed;
    Canvas.FrameRect(Clientrect);
  end;
  inherited Paint;
end;

procedure TToolBar.SetButtonSize(NewButtonWidth, NewButtonHeight: integer);
var
  CurControl: TControl;
  NewWidth: Integer;
  NewHeight: Integer;
  i: Integer;
  ChangeW, ChangeH: Boolean;
begin
  ChangeW := FButtonWidth <> NewButtonWidth;
  ChangeH := FButtonHeight <> NewButtonHeight;
  if not (ChangeW or ChangeH) then Exit;
  
  FButtonWidth:=NewButtonWidth;
  FButtonHeight:=NewButtonHeight;
  if FUpdateCount > 0 then Exit;
  if [csLoading, csDestroying] * ComponentState <> [] then Exit;

  // set all children to ButtonWidth ButtonHeight
  BeginUpdate;
  try
    for i:=ControlCount-1 downto 0 do
    begin
      CurControl := Controls[i];
      NewWidth := CurControl.Width;
      NewHeight := CurControl.Height;

      // width
      if ChangeW
      and (ButtonWidth > 0)
      and not CurControl.AutoSize
      and (CurControl is TToolButton)
      and (CurControl.Align in [alNone, alLeft, alRight])
      then begin
        if TToolButton(CurControl).Style in [tbsButton,tbsCheck,tbsDropDown]
        then begin
          CurControl.GetPreferredSize(NewWidth,NewHeight);
          if NewWidth < ButtonWidth then
            NewWidth := ButtonWidth;
        end;
      end;
      
      // height
      // in horizontal toolbars the height is set by the toolbar independent of autosize
      if ChangeH
      and (ButtonHeight > 0)
      and ((Align in [alTop, alBottom]) or not CurControl.AutoSize)
      then NewHeight := ButtonHeight;
        
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
function TToolBar.WrapButtons(UseWidth: integer;
  out NewWidth, NewHeight: Integer; Simulate: boolean): Boolean;
var
  ARect: TRect;
  x: Integer;
  y: Integer;
  NewControlWidth: Integer;
  CurControl: TControl;
  ObstacleControls: TFPList;
  FullSizeObstacleControls: TFPList;
  StartX: Integer;
  w: LongInt;
  h: LongInt;

  procedure CalculatePosition;
  var
    AlignedControl: TControl;
    NewBounds: TRect;
    SiblingBounds: TRect;
    j: Integer;
    PreferredBtnWidth: Integer;
    PreferredBtnHeight: Integer;
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
      NewControlWidth := PreferredBtnWidth;
      if (TToolButton(CurControl).Style in [tbsButton, tbsDropDown, tbsCheck]) then
      begin
        if (NewControlWidth < ButtonWidth) then
          NewControlWidth := ButtonWidth;
      end;
    end
    else
      NewControlWidth := CurControl.Width;
    NewBounds := Bounds(x, y, NewControlWidth, ButtonHeight);

    //DebugLn(['CalculatePosition ',DbgSName(CurControl)]);
    StartedAtRowStart:=x=StartX;
    repeat
      // move control to the right, until it does not overlap
      IntersectsWithLimitedHeightControl:=false;
      j:=0;
      while j<ObstacleControls.Count do
      begin
        AlignedControl := TControl(ObstacleControls[j]);
        SiblingBounds := AlignedControl.BoundsRect;
        Intersects:=(SiblingBounds.Right > NewBounds.Left) and
                    (SiblingBounds.Left < NewBounds.Right) and
                    (SiblingBounds.Bottom > NewBounds.Top) and
                    (SiblingBounds.Top < NewBounds.Bottom);
        if Intersects then begin
          //DebugLn(['CalculatePosition Move ',NewBounds.Left,'->',SiblingBounds.Right]);
          NewBounds.Left := SiblingBounds.Right;
          NewBounds.Right := NewBounds.Left + NewControlWidth;
          j:=0; // check again, needed, because ObstacleControls are not sorted
                // (and can not be sorted, because they can overlap)
          if FullSizeObstacleControls.IndexOf(AlignedControl)<0 then
            IntersectsWithLimitedHeightControl:=true;
        end else
          inc(j);
      end;
      if (not Wrapable)
      or (NewBounds.Right <= ARect.Right) or (NewBounds.Left = StartX)
      or (StartedAtRowStart and not IntersectsWithLimitedHeightControl) then
      begin
        // control fits into the row
        //DebugLn(['CalculatePosition fits: ',DbgSName(CurControl),' ',dbgs(NewBounds)]);
        x := NewBounds.Left;
        y := NewBounds.Top;
        break;
      end;

      // try next row
      NewBounds.Left := StartX;
      NewBounds.Right := NewBounds.Left + NewControlWidth;
      inc(NewBounds.Top, ButtonHeight);
      inc(NewBounds.Bottom, ButtonHeight);
      StartedAtRowStart:=true;
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
  GrowSide: TAnchorKind;
begin
  //DebugLn(['WrapButtons ',DbgSName(Self),' Wrapable=',Wrapable,' ',dbgs(BoundsRect)]);
  Result := True;
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
    GrowSide:=akBottom;
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
    if FRealizedButtonHeight = 0 then
      FRealizedButtonHeight := FButtonHeight;
    OrderedControls.Sort(TListSortCompare(@CompareToolBarControl));

    // position OrderedControls
    CurClientRect := ClientRect;
    inc(CurClientRect.Right, UseWidth - Width);
    ARect := CurClientRect;
    AdjustClientRect(ARect);
    AdjustClientFrame.Left := ARect.Left - CurClientRect.Left;
    AdjustClientFrame.Top := ARect.Top - CurClientRect.Top;
    AdjustClientFrame.Right := CurClientRect.Right - ARect.Right;
    AdjustClientFrame.Bottom := CurClientRect.Bottom - ARect.Bottom;
    //DebugLn(['TToolBar.WrapButtons ',DbgSName(Self),' ARect=',dbgs(ARect)]);
    // important: top, left button must start in the AdjustClientRect top, left
    // otherwise Toolbar.AutoSize=true will create an endless loop
    StartX := ARect.Left;
    x := StartX;
    y := ARect.Top;
    for i := 0 to OrderedControls.Count - 1 do
    begin
      CurControl := TControl(OrderedControls[i]);
      if not CurControl.IsControlVisible then
        Continue;
      CalculatePosition;
      //DebugLn(['WrapButtons ',CurControl.Name,':',CurControl.ClassName,' ',x,',',y,',',CurControl.Width,'x',CurControl.Height]);
      if ButtonHeight <= 0 then
        h := CurControl.Height
      else
        h := ButtonHeight;
      if CurControl.AutoSize then
      begin
        w := CurControl.Width;
        h := CurControl.Height;
      end
      else
        w := NewControlWidth;

      w := CurControl.Constraints.MinMaxWidth(w);
      h := CurControl.Constraints.MinMaxWidth(h);
      if (CurControl.Left <> x) or (CurControl.Top <> y) or
         (CurControl.Width <> w) or (CurControl.Height <> h) then
      begin
        //DebugLn(['TToolBar.WrapButtons moving child: ',DbgSName(CurControl),' Old=',dbgs(CurControl.BoundsRect),' New=',dbgs(Bounds(x,y,w,h))]);
        if not Simulate then
        begin
          //DebugLn(['TToolBar.WrapButtons moving child: ',DbgSName(CurControl),' Old=',dbgs(CurControl.BoundsRect),' New=',dbgs(Bounds(x,y,w,h))]);
          CurControl.SetBounds(x,y,w,h); // Note: do not use SetBoundsKeepBase
          //DebugLn(['TToolBar.WrapButtons moved child: ',DbgSName(CurControl),' ',dbgs(CurControl.BoundsRect)]);
        end;
      end;
      
      // adjust NewWidth, NewHeight
      NewWidth := Max(NewWidth, x + w + AdjustClientFrame.Right);
      NewHeight := Max(NewHeight, y + h + AdjustClientFrame.Bottom);

      // step to next position
      inc(x,w);
      if (not Wrapable) and (CurControl is TToolButton) and
         (TToolButton(CurControl).Wrap) then
      begin
        // user forced wrap -> start new line
        x := StartX;
        inc(y, ButtonHeight);
        if not Simulate then
          inc(FRowCount);
      end;
    end;
    FRealizedButtonHeight := FButtonHeight;
  finally
    ObstacleControls.Free;
    OrderedControls.Free;
    FullSizeObstacleControls.Free;
    EndUpdate;
    EnableAlign;
  end;
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
end;

function TToolBar.CheckMenuDropdown(Button: TToolButton): Boolean;
var
  APoint: TPoint;
begin
  Result := False;
  if Button = nil then
    Exit;
  if FCurrentMenu <> nil then
    FCurrentMenu.Close;
  if FCurrentMenuAutoFree then
    FCurrentMenu.Free;
  FCurrentMenu := nil;
  FCurrentMenuAutoFree := False;
  FSrcMenuItem := nil;
  FSrcMenu := nil;
  FDropDownButton := Button;
  if Button.DropdownMenu <> nil then
    // the button has a popupenu
    FCurrentMenu := Button.DropdownMenu
  else
  if Button.MenuItem <> nil then
  begin
    // the button has a menuitem
    // -> create a temporary TPopupMenu and move all child menuitems
    FCurrentMenuAutoFree := True;
    FCurrentMenu := TPopupMenu.Create(Self);
    FSrcMenuItem := Button.MenuItem;
    FSrcMenu := FSrcMenuItem.GetParentMenu;
    FCurrentMenu.Items.HelpContext := FSrcMenuItem.HelpContext;
    if FSrcMenu <> nil then
      FCurrentMenu.Images := FSrcMenu.Images;
    MoveSubMenuItems(FSrcMenuItem, FCurrentMenu.Items);
    FCurrentMenu.OnClose := @OnTemporaryPopupMenuClose;
  end
  else
    Exit;
  FCurrentMenu.PopupComponent := Self;
  APoint := Button.ClientToScreen(Point(0, Button.ClientHeight));
  if FCurrentMenu.IsRightToLeft then Inc(APoint.X, Button.Width);
  FCurrentMenu.Popup(APoint.X, APoint.Y);
  Result := True;
end;

procedure TToolBar.ClickButton(Button: TToolButton);
begin
  Button.Click;
end;

