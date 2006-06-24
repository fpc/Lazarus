{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TToolbar
 ******************************************************************************

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

function CompareToolBarControl(Control1, Control2: TControl): integer;
var
  ToolBar: TToolBar;
  Row1: Integer;
  Row2: Integer;
  BtnHeight: Integer;
begin
  Result:=0;
  if not (Control1.Parent is TToolBar) then Exit;

  ToolBar := TToolBar(Control1.Parent);
  BtnHeight := ToolBar.FRealizedButtonHeight;

  Row1:=(Control1.Top+(BtnHeight div 2)) div ToolBar.FRealizedButtonHeight;
  Row2:=(Control2.Top+(BtnHeight div 2)) div ToolBar.FRealizedButtonHeight;
  if Row1<Row2 then
    Result:=-1
  else if Row1>Row2 then
    Result:=1
  else if Control1.Left<Control2.Left then
    Result:=-1
  else if Control1.Left>Control2.Left then
    Result:=1;
end;

{------------------------------------------------------------------------------
  Method: TToolbar.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TToolbar.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csMenuEvents, csSetCaption];
  FButtonWidth := 23;
  FButtonHeight := 22;
  FDropDownWidth := 10;
  FNewStyle := True;
  FWrapable := True;
  FButtons := TList.Create;
  fCompStyle := csToolbar;
  FIndent := 1;
  FList:=false;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  FDisabledImageChangeLink := TChangeLink.Create;
  FDisabledImageChangeLink.OnChange := @DisabledImageListChange;
  FHotImageChangeLink := TChangeLink.Create;
  FHotImageChangeLink.OnChange := @HotImageListChange;
  EdgeBorders := [ebTop];
  SetInitialBounds(0,0,150,26);
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

procedure TToolBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TToolBar.CreateWnd;
begin
  BeginUpdate;
  inherited CreateWnd;
  UpdateVisibleBar;
  EndUpdate;
end;

procedure TToolBar.ControlsAligned;
var
  NewWidth, NewHeight: integer;
begin
  if tbfPlacingControls in FToolBarFlags then exit;
  Include(FToolBarFlags,tbfPlacingControls);
  try
    WrapButtons(NewWidth,NewHeight);
  finally
    Exclude(FToolBarFlags,tbfPlacingControls);
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
  if FSrcMenuItem=nil then exit;
  MoveSubMenuItems(FCurrentMenu.Items,FSrcMenuItem);
  if FDropDownButton<>nil then
    FDropDownButton.Down:=false;
end;

procedure TToolBar.MoveSubMenuItems(SrcMenuItem, DestMenuItem: TMenuItem);
var
  i: Integer;
  MovingMenuItem: TMenuItem;
begin
  if (SrcMenuItem=nil) or (DestMenuItem=nil) or (SrcMenuItem=DestMenuItem) then
    exit;
  for i := SrcMenuItem.Count - 1 downto 0 do begin
    MovingMenuItem:=SrcMenuItem.Items[i];
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

function TToolBar.GetButton(Index: Integer): TToolButton;
begin
  Result := TToolButton(FButtons[Index]);
end;

function TToolBar.GetButtonCount: Integer;
begin
  Result := FButtons.Count;
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
  if FTransparent = AValue then exit;
  FTransparent := AValue;
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
  if FImages=AValue then exit;
  if FImages <> nil then FImages.UnRegisterChanges(FImageChangeLink);
  FImages := AValue;
  if FImages <> nil then begin
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
  if FDisabledImages=AValue then exit;
  if FDisabledImages <> nil then
    FDisabledImages.UnRegisterChanges(FDisabledImageChangeLink);
  FDisabledImages := AValue;
  if FDisabledImages <> nil then begin
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
begin
  if ([csLoading,csDestroying]*ComponentState<>[]) or (not HandleAllocated) then
  begin
    Include(FToolBarFlags,tbfUpdateVisibleBarNeeded);
    exit;
  end;
  ReAlign;
  Invalidate;
  Exclude(FToolBarFlags,tbfUpdateVisibleBarNeeded);
end;

procedure TToolBar.SetHotImages(const AValue: TCustomImageList);
begin
  if FHotImages=AValue then exit;
  if FHotImages <> nil then
    FHotImages.UnRegisterChanges(FHotImageChangeLink);
  FHotImages := AValue;
  if FHotImages <> nil then begin
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

procedure TToolBar.Paint;
begin
  if csDesigning in ComponentState then begin
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
  CurButton: TToolButton;
begin
  if (FButtonWidth=NewButtonWidth) and (FButtonHeight=NewButtonHeight) then
    exit;
  FButtonWidth:=NewButtonWidth;
  FButtonHeight:=NewButtonHeight;
  if ([csLoading,csDestroying]*ComponentState<>[]) or (FUpdateCount > 0) then
    Exit;
  // set all childs to ButtonWidth ButtonHeight
  BeginUpdate;
  try
    for i:=ControlCount-1 downto 0 do begin
      CurControl:=Controls[i];
      if (CurControl.Align<>alNone) then continue;
      NewWidth:=CurControl.Width;
      if (CurControl is TToolButton) and (not CurControl.AutoSize) then begin
        CurButton:=TToolButton(CurControl);
        if CurButton.Style in [tbsButton,tbsCheck,tbsDropDown] then begin
          CurControl.GetPreferredSize(NewWidth,NewHeight);
          if NewWidth<FButtonWidth then
            NewWidth:=FButtonWidth;
        end;
      end;
      if CurControl.AutoSize then
        NewHeight:=CurControl.Height
      else
        NewHeight:=FButtonHeight;
      CurControl.SetBounds(CurControl.Left,CurControl.Top,
                           NewWidth,NewHeight);
    end;
  finally
    EndUpdate;
  end;
end;

function TToolBar.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := WrapButtons(NewWidth, NewHeight);
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
function TToolBar.WrapButtons(var NewWidth, NewHeight: Integer): Boolean;
var
  i: Integer;
  ARect: TRect;
  x: Integer;
  y: Integer;
  NewControlWidth: Integer;
  CurControl: TControl;
  AlignedControls: TList;
  StartX: Integer;
  OrderedControls: TList;

  procedure CalculatePosition;
  var
    AlignedControl: TControl;
    NewBounds: TRect;
    CurBounds: TRect;
    j: Integer;
    PreferredBtnWidth: Integer;
    PreferredBtnHeight: Integer;
  begin
    if (CurControl is TToolButton)
    and (TToolButton(CurControl).Style in [tbsButton,tbsDropDown,tbsCheck])
    and (not CurControl.AutoSize)
    then begin
      PreferredBtnWidth:=0;
      PreferredBtnHeight:=0;
      CurControl.GetPreferredSize(PreferredBtnWidth,PreferredBtnHeight);
      NewControlWidth:=PreferredBtnWidth;
      if NewControlWidth<ButtonWidth then
        NewControlWidth:=ButtonWidth;
    end else
      NewControlWidth:=CurControl.Width;
    NewBounds:=Bounds(x,y,NewControlWidth,ButtonHeight);
    repeat
      // move control to the right, until it does not overlap
      for j:=0 to AlignedControls.Count-1 do begin
        AlignedControl:=TControl(AlignedControls[j]);
        CurBounds:=Bounds(AlignedControl.Left,AlignedControl.Top,
                          AlignedControl.Width,AlignedControl.Height);
        if (CurBounds.Right>NewBounds.Left)
        and (CurBounds.Left<NewBounds.Right)
        and (CurBounds.Bottom>NewBounds.Top)
        and (CurBounds.Top<NewBounds.Bottom) then begin
          //DebugLn('CalculatePosition Move ',NewBounds.Left,'->',CurBounds.Right);
          NewBounds.Left:=CurBounds.Right;
          NewBounds.Right:=NewBounds.Left+NewControlWidth;
        end;
      end;
      if (not Wrapable) or (NewBounds.Right<=ARect.Right)
      or (NewBounds.Left=StartX) then begin
        // control fits into the row
        x:=NewBounds.Left;
        y:=NewBounds.Top;
        exit;
      end;
      // try next row
      NewBounds.Left:=StartX;
      NewBounds.Right:=NewBounds.Left+NewControlWidth;
      inc(NewBounds.Top,ButtonHeight);
      inc(NewBounds.Bottom,ButtonHeight);
      //DebugLn('CalculatePosition Next Row ',NewBounds.Left,',',NewBounds.Top);
    until false;
  end;

begin
  //DebugLn('WrapButtons ');
  Result:=true;
  NewWidth:=0;
  NewHeight:=0;
  AlignedControls:=TList.Create;
  OrderedControls:=TList.Create;
  DisableAlign;
  BeginUpdate;
  try
    for i:=0 to ControlCount-1 do begin
      CurControl:=Controls[i];
      if CurControl.Align=alNone then
        OrderedControls.Add(CurControl)
      else
        AlignedControls.Add(CurControl)
    end;
    // sort OrderedControls
    if FRealizedButtonHeight=0 then FRealizedButtonHeight:=FButtonHeight;
    OrderedControls.Sort(TListSortCompare(@CompareToolBarControl));
    
    // position OrderedControls
    ARect:=ClientRect;
    AdjustClientRect(ARect);
    StartX:=ARect.Left+Indent;
    x:=StartX;
    y:=ARect.Top;
    i:=0;
    NewControlWidth:=ButtonWidth;
    while i<OrderedControls.Count do begin
      CurControl:=TControl(OrderedControls[i]);
      if CurControl.Align=alNone then begin
        CalculatePosition;
        //DebugLn('WrapButtons ',CurControl.Name,':',CurControl.ClassName,' ',x,',',y,',',CurControl.Width,',',CurControl.Height);
        if CurControl.AutoSize then begin
          // TODO: center vertically
          CurControl.SetBoundsKeepBase(x,y,CurControl.Width,CurControl.Height);
        end else
          CurControl.SetBoundsKeepBase(x,y,NewControlWidth,ButtonHeight);
        inc(x,CurControl.Width);
        
        if (not Wrapable) and (CurControl is TToolButton)
        and (TToolButton(CurControl).Wrap) then begin
          // user forced wrap -> start new line
          x:=StartX;
          inc(y,ButtonHeight);
        end;
      end;
      inc(i);
    end;
    FRealizedButtonHeight:=FButtonHeight;
  finally
    AlignedControls.Free;
    OrderedControls.Free;
    EndUpdate;
    EnableAlign;
  end;
end;

function TToolBar.FindButtonFromAccel(Accel: Word): TToolButton;
var
  i: Integer;
begin
  for i := 0 to FButtons.Count - 1 do
    if TControl(FButtons[i]) is TToolButton then
    begin
      Result := Buttons[i];
      if Result.Visible and Result.Enabled
      and IsAccel(Accel, Result.Caption) then
        Exit;
    end;
  Result := nil;
end;

function TToolBar.CheckMenuDropdown(Button: TToolButton): Boolean;
var
  APoint: TPoint;
begin
  Result := False;
  if Button = nil then Exit;
  if FCurrentMenu<>nil then
    FCurrentMenu.Close;
  if FCurrentMenuAutoFree then FCurrentMenu.Free;
  FCurrentMenu:=nil;
  FCurrentMenuAutoFree:=false;
  FSrcMenuItem:=nil;
  FSrcMenu:=nil;
  FDropDownButton:=Button;
  if Button.DropdownMenu <> nil then
    // the button has a popupenu
    FCurrentMenu := Button.DropdownMenu
  else if Button.MenuItem <> nil then begin
    // the button has a menuitem
    // -> create a temporary TPopupMenu and move all child menuitems
    FCurrentMenuAutoFree:=true;
    FCurrentMenu := TPopupMenu.Create(Self);
    FSrcMenuItem:=Button.MenuItem;
    FSrcMenu := FSrcMenuItem.GetParentMenu;
    FCurrentMenu.Items.HelpContext := FSrcMenuItem.HelpContext;
    if FSrcMenu <> nil then
      FCurrentMenu.Images := FSrcMenu.Images;
    MoveSubMenuItems(FSrcMenuItem,FCurrentMenu.Items);
    FCurrentMenu.OnClose:=@OnTemporaryPopupMenuClose;
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

