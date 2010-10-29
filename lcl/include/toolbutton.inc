{%MainUnit ../comctrls.pp}

{ TToolButton

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

{ TToolButtonActionLink }

procedure TToolButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TToolButton;
end;

function TToolButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked
    and (TToolButton(FClient).Down = (Action as TCustomAction).Checked);
end;

function TToolButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked
   and (TToolButton(FClient).ImageIndex = (Action as TCustomAction).ImageIndex);
end;

procedure TToolButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    TToolButton(FClient).Down := Value;
end;

procedure TToolButtonActionLink.SetImageIndex(Value: Integer);
begin
  {DebugLn(['TToolButtonActionLink.SetImageIndex A ',ClassName,' Client=',
    TToolButton(FClient).Name,' IsImageIndexLinked=',
    IsImageIndexLinked,' Old=',
    TToolButton(FClient).ImageIndex,' New=',Value]);}
  if IsImageIndexLinked then TToolButton(FClient).ImageIndex := Value;
end;

{ TToolButton }

constructor TToolButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FImageIndex := -1;
  FStyle := tbsButton;
  FShowCaption := true;
  ControlStyle := [csCaptureMouse, csSetCaption, csDesignNoSmoothResize];
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

procedure TToolButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  NewFlags: TToolButtonFlags;
begin
  NewFlags := FToolButtonFlags - [tbfPressed, tbfArrowPressed];
  if (Button = mbLeft) then
  begin
    if Enabled then
    begin
      if (Style = tbsDropDown) and (FToolBar <> nil) and (X > ClientWidth - FToolBar.FDropDownWidth) then
        Include(NewFlags, tbfArrowPressed)
      else
        Include(NewFlags, tbfPressed);
    end;
    if NewFlags <> FToolButtonFlags then
    begin
      FToolButtonFlags := NewFlags;
      Invalidate;
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);

  if (Style = tbsDropDown) and (Button = mbLeft) and Enabled then
  begin
    if NewFlags * [tbfArrowPressed] = [] then
      Down := True;
  end;
end;

procedure TToolButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  //DebugLn('TToolButton.MouseMove ',Name,':',ClassName,' ',X,',',Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TToolButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DropDownMenuDropped: Boolean;
begin
  //DebugLn('TToolButton.MouseUp ',Name,':',ClassName,' ',dbgs(ord(Button)),' ',dbgs(X),',',dbgs(Y));
  if (Button = mbLeft) and ([tbfArrowPressed, tbfPressed] * FToolButtonFlags <> []) then
  begin
    Exclude(FToolButtonFlags, tbfPressed);
    Exclude(FToolButtonFlags, tbfArrowPressed);
    Invalidate;
  end;

  inherited MouseUp(Button, Shift, X, Y);
  
  if (Button = mbLeft) then
  begin
    DropDownMenuDropped := False;
    //DebugLn('TToolButton.MouseUp ',Name,':',ClassName,' ',Style=tbsCheck);
    if (Style in [tbsButton, tbsDropDown]) then
    begin
      if (FToolBar <> nil) and FMouseInControl and
         ((Style = tbsButton) or (X > ClientWidth - FToolBar.FDropDownWidth)) then
        DropDownMenuDropped := CheckMenuDropdown;
      Down := False;
    end;

    if FMouseInControl and not DropDownMenuDropped then
    begin
      if (Style = tbsCheck) then
        Down := not Down;
      Click;
    end;
  end;
  Invalidate;
end;

procedure TToolButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if AComponent = DropdownMenu then
      DropdownMenu := nil
    else if AComponent = MenuItem then
      MenuItem := nil;
  end;
end;

procedure TToolButton.Paint;

  procedure DrawDropDownArrow(OwnerDetails: TThemedElementDetails; const DropDownButtonRect: TRect);
  var
    Details: TThemedElementDetails;
    ArrowState: TThemedToolBar;
  begin
    ArrowState := TThemedToolBar(ord(ttbSplitButtonDropDownNormal) + OwnerDetails.State - 1);
    if (tbfArrowPressed in FToolButtonFlags) and FMouseInControl and Enabled then
      ArrowState := ttbSplitButtonDropDownPressed;
    Details := ThemeServices.GetElementDetails(ArrowState);
    if ((FToolBar <> nil) and not FToolBar.Flat) and (Details.State in [1, 4]) then
      Details.State := 2;
    ThemeServices.DrawElement(Canvas.Handle, Details, DropDownButtonRect);
  end;
  
  procedure DrawDivider(Details: TThemedElementDetails; ARect: TRect);
  begin
    // theme services have no strict rule to draw divider in the center,
    // so we should calculate rectangle here
    // on windows 7 divider can't be less than 4 pixels
    if FToolBar.IsVertical then
    begin
      if (ARect.Bottom - ARect.Top) > 4 then
      begin
        ARect.Top := (ARect.Top + ARect.Bottom) div 2 - 2;
        ARect.Bottom := ARect.Top + 4;
      end;
    end
    else
    begin
      if (ARect.Right - ARect.Left) > 4 then
      begin
        ARect.Left := (ARect.Left + ARect.Right) div 2 - 2;
        ARect.Right := ARect.Left + 4;
      end;
    end;
    ThemeServices.DrawElement(Canvas.GetUpdatedHandle([csBrushValid, csPenValid]),
       Details, ARect);
  end;
  
  procedure DrawSeparator(Details: TThemedElementDetails; ARect: TRect);
  begin
    // separator is just an empty space between buttons, so we should not draw anything,
    // but vcl draws line when toolbar is flat, because there is no way to detect
    // space between flat buttons. Better if we draw something too. One of suggestions
    // was to draw 2 lines instead of one divider - this way separator and divider will differ
    if FToolBar.Flat then // draw it only for flat Toolbar
    begin
      if FToolBar.IsVertical then
      begin
        if (ARect.Bottom - ARect.Top) > 8 then
        begin
          ARect.Top := (ARect.Top + ARect.Bottom) div 2 - 4;
          ARect.Bottom := ARect.Top + 4;
          DrawDivider(Details, ARect);
          OffsetRect(ARect, 0, 4);
          DrawDivider(Details, ARect);
        end
        else
          DrawDivider(Details, ARect);
      end
      else
      begin
        if (ARect.Right - ARect.Left) > 8 then
        begin
          ARect.Left := (ARect.Left + ARect.Right) div 2 - 4;
          ARect.Right := ARect.Left + 4;
          DrawDivider(Details, ARect);
          OffsetRect(ARect, 4, 0);
          DrawDivider(Details, ARect);
        end
        else
          DrawDivider(Details, ARect);
      end;
    end;
  end;

var
  PaintRect: TRect;
  ButtonRect: TRect;
  DropDownButtonRect: TRect;
  TextSize: TSize;
  TextPos: TPoint;
  IconSize: TPoint;
  IconPos: TPoint;
  ImgList: TCustomImageList;
  ImgIndex: integer;
  Details, TempDetails: TThemedElementDetails;
begin
  if (FToolBar<>nil) and (ClientWidth>0) and (ClientHeight>0) then
  begin
    PaintRect := ClientRect; // the whole paint area

    // calculate button area(s)
    ButtonRect := PaintRect;
    Details := GetButtonDrawDetail;

    if Style = tbsDropDown then
    begin
      DropDownButtonRect := ButtonRect;
      DropDownButtonRect.Left :=
                        Max(0, DropDownButtonRect.Right - FToolBar.FDropDownWidth);
      ButtonRect.Right := DropDownButtonRect.Left;
    end;

    // calculate text size
    TextSize.cx:=0;
    TextSize.cy:=0;
    if (Style in [tbsButton, tbsDropDown, tbsCheck]) and (FToolBar.ShowCaptions)
    and ((FToolbar.List and ShowCaption) //Allow hide caption only in list mode
         or not FToolBar.List)
       and (Caption <> '')
    then
      TextSize := GetTextSize;

    // calculate icon size
    IconSize := Point(0,0);
    GetCurrentIcon(ImgList, ImgIndex);
    if (ImgList<>nil) then
    begin
      IconSize := Point(ImgList.Width, ImgList.Height);
      if IconSize.y <= 0 then
        IconSize.X := 0;
    end;

    // calculate text and icon position
    TextPos:=Point(0,0);
    IconPos:=Point(0,0);
    if TextSize.cx > 0 then
    begin
      if IconSize.X > 0 then
      begin
        if FToolBar.List then
        begin
          // icon left of text
          IconPos.X:=(ButtonRect.Left+ButtonRect.Right-IconSize.x-TextSize.cx-2) div 2;
          IconPos.Y:=(ButtonRect.Top+ButtonRect.Bottom-IconSize.y) div 2;
          TextPos.X:=IconPos.X+IconSize.X+2;
          TextPos.Y:=(ButtonRect.Top+ButtonRect.Bottom-TextSize.cy) div 2;
        end else
        begin
          // icon above text
          IconPos.X:=(ButtonRect.Left+ButtonRect.Right-IconSize.x) div 2;
          IconPos.Y:=(ButtonRect.Top+ButtonRect.Bottom-IconSize.y-TextSize.cy-2) div 2;
          TextPos.X:=(ButtonRect.Left+ButtonRect.Right-TextSize.cx) div 2;
          TextPos.Y:=IconPos.Y+IconSize.Y+2;
        end;
      end else
      begin
        // only text
        TextPos.X:=(ButtonRect.Left+ButtonRect.Right-TextSize.cx) div 2;
        TextPos.Y:=(ButtonRect.Top+ButtonRect.Bottom-TextSize.cy) div 2;
      end;
    end else
    if IconSize.x>0 then
    begin
      // only icon
      IconPos.X:=(ButtonRect.Left+ButtonRect.Right-IconSize.x) div 2;
      IconPos.Y:=(ButtonRect.Top+ButtonRect.Bottom-IconSize.y) div 2;
    end;

    // draw button
    if (Style in [tbsButton, tbsDropDown, tbsCheck]) then
    begin
      // if toolbar is not flat then normal and disabled state is drawn as hot
      TempDetails := Details;
      if ((FToolBar <> nil) and not FToolBar.Flat) and (TempDetails.State in [1, 4]) then
        TempDetails.State := 2;

      ThemeServices.DrawElement(Canvas.GetUpdatedHandle([csBrushValid, csPenValid]),
         TempDetails, ButtonRect);
      ButtonRect := ThemeServices.ContentRect(Canvas.Handle, TempDetails, ButtonRect);
    end
    else
    if Style = tbsDivider then
    begin
      DrawDivider(Details, ButtonRect);
      ButtonRect := Rect(0, 0, 0, 0); // nothing can be drawn on divider
    end
    else
    if Style = tbsSeparator then
    begin
      DrawSeparator(Details, ButtonRect);
      ButtonRect := Rect(0, 0, 0, 0); // nothing can be drawn on separator
    end;

    // draw dropdown button
    if Style in [tbsDropDown] then
      DrawDropDownArrow(Details, DropDownButtonRect);

    // draw icon
    if (ImgList<>nil) then
    begin
      // Down=True causes the details to be in Checked state (rather than Pushed)
      // That caused downed checkbuttons to draw an image without offset. see bug rep #16975
      if ThemeServices.IsPushed(Details) or ThemeServices.IsChecked(Details) then
      begin
        inc(IconPos.X);
        inc(IconPos.Y);
      end;
      
      ImgList.Draw(Canvas, IconPos.X, IconPos.Y, ImgIndex, Enabled);
    end;

    // draw text
    if (TextSize.cx > 0) then
    begin
      ButtonRect.Left := TextPos.X;
      ButtonRect.Top := TextPos.Y;
      // if State is disabled then change to PushButtonDisabled since
      // ToolButtonDisabled text looks not disabled though windows native toolbutton
      // text drawn with disabled look. For other widgetsets there is no difference which
      // disabled detail to use
      TempDetails := Details;
      if TempDetails.State = 4 then
        TempDetails := ThemeServices.GetElementDetails(tbPushButtonDisabled);
      ThemeServices.DrawText(Canvas, TempDetails, Caption, ButtonRect,
        DT_LEFT or DT_TOP, 0);
    end;

    // draw separator (at runtime: just space, at designtime: a rectangle)
    if (Style = tbsSeparator) and (csDesigning in ComponentState) then
    begin
      Canvas.Brush.Color := clBackground;
      Canvas.Pen.Color := clBlack;
      dec(PaintRect.Right);
      dec(PaintRect.Bottom);
      Canvas.FrameRect(PaintRect);
    end;
  end;
  
  inherited Paint;
end;

procedure TToolButton.Loaded;
begin
  inherited Loaded;
  CopyPropertiesFromMenuItem(FMenuItem);
end;

procedure TToolButton.SetAutoSize(Value: Boolean);
begin
  if Value = AutoSize then exit;
  inherited SetAutoSize(Value);
  RequestAlign;
end;

procedure TToolButton.RealSetText(const AValue: TCaption);
begin
  if ([csLoading,csDestroying]*ComponentState=[]) then
  begin
    InvalidatePreferredSize;
    inherited RealSetText(AValue);
    AdjustSize;
  end
  else inherited RealSetText(AValue);
end;

procedure TToolButton.SetToolBar(NewToolBar: TToolBar);
begin
  if FToolBar = NewToolBar then exit;
  Parent := NewToolBar;
end;

procedure TToolButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
var
  NewAction: TCustomAction;
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then begin
    NewAction:=TCustomAction(Sender);
    if (not CheckDefaults) or (not Down) then
      Down := NewAction.Checked;
    if (not CheckDefaults) or (ImageIndex<0) then
      ImageIndex := NewAction.ImageIndex;
  end;
end;

function TToolButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result:=TToolButtonActionLink;
end;

procedure TToolButton.CopyPropertiesFromMenuItem(const Value: TMenuItem);
begin
  if Value=nil then exit;
  BeginUpdate;
  Action := Value.Action;
  Caption := Value.Caption;
  Down := Value.Checked;
  Enabled := Value.Enabled;
  Hint := Value.Hint;
  ImageIndex := Value.ImageIndex;
  Visible := Value.Visible;
  EndUpdate;
end;

procedure TToolButton.CMHitTest(var Message: TCMHitTest);
begin
  if (not (Style in [tbsDivider, tbsSeparator])) or (DragKind = dkDock) then
    Message.Result := 1
  else
    Message.Result := 0;
end;

class procedure TToolButton.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomToolButton;
end;

procedure TToolButton.MouseEnter;
begin
  //DebugLn('TToolButton.MouseEnter ',Name);
  inherited MouseEnter;
  SetMouseInControl(true);
end;

procedure TToolButton.MouseLeave;
begin
  //DebugLn('TToolButton.MouseLeave ',Name);
  inherited MouseLeave;
  SetMouseInControl(false);
  if (not MouseCapture) and ([tbfPressed, tbfArrowPressed] * FToolButtonFlags <> []) then
  begin
    Exclude(FToolButtonFlags, tbfPressed);
    Exclude(FToolButtonFlags, tbfArrowPressed);
    Invalidate;
  end;
end;

procedure TToolButton.SetDown(Value: Boolean);
var
  StartIndex, EndIndex: integer;
  i: Integer;
  CurButton: TToolButton;
begin
  if Value = FDown then exit;
  if (csLoading in ComponentState) then begin
    FDown:=Value;
    exit;
  end;
  
  //DebugLn('TToolButton.SetDown ',Style=tbsCheck,',',FDown,',',GroupAllUpAllowed);
  if (Style=tbsCheck) and FDown and (not GroupAllUpAllowed) then
    exit;

  FDown := Value;
  
  if (Style=tbsCheck) and FDown and Grouped then begin
    //DebugLn('TToolButton.SetDown B ');
    // uncheck all other in the group
    GetGroupBounds(StartIndex,EndIndex);
    if StartIndex>=0 then begin
      for i:=StartIndex to EndIndex do begin
        CurButton:=FToolBar.Buttons[i];
        if (CurButton<>Self) and (CurButton.FDown) then begin
          CurButton.FDown:=false;
          CurButton.Invalidate;
        end;
      end;
    end;
  end;

  Invalidate;
  if FToolBar <> nil then
    FToolBar.ToolButtonDown(Self,FDown);
end;

procedure TToolButton.SetDropdownMenu(Value: TPopupMenu);
begin
  if Value = FDropdownMenu then exit;
  FDropdownMenu := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TToolButton.SetGrouped(Value: Boolean);
var
  StartIndex, EndIndex: integer;
  CheckedIndex: Integer;
  i: Integer;
  CurButton: TToolButton;
begin
  if FGrouped = Value then exit;
  FGrouped := Value;
  if csLoading in ComponentState then exit;
  
  // make sure, that only one button in a group is checked
  while FGrouped and (Style=tbsCheck) and (FToolBar<>nil) do begin
    GetGroupBounds(StartIndex,EndIndex);
    if StartIndex>=0 then begin
      CheckedIndex:=-1;
      i:=StartIndex;
      while i<=EndIndex do begin
        CurButton:=FToolBar.Buttons[i];
        if CurButton.Down then begin
          if CheckedIndex<0 then
            CheckedIndex:=i
          else begin
            CurButton.Down:=false;
            // the last operation can change everything -> restart
            break;
          end;
        end;
        inc(i);
      end;
      if i>EndIndex then break;
    end;
  end;
end;

procedure TToolButton.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex = Value then exit;
  //debugln('TToolButton.SetImageIndex ',Name,':',ClassName,' Old=',FImageIndex,' New=',Value);
  FImageIndex := Value;
  if IsControlVisible and (FToolBar <> nil) then
  begin
    RefreshControl;
    Invalidate;
  end;
end;

procedure TToolButton.SetMarked(Value: Boolean);
begin
  if FMarked = Value then exit;
  FMarked := Value;
  if FToolBar <> nil then
    Invalidate;
end;

procedure TToolButton.SetIndeterminate(Value: Boolean);
begin
  if FIndeterminate = Value then exit;
  if Value then SetDown(False);
  FIndeterminate := Value;
  if FToolBar <> nil then
    Invalidate;
end;

procedure TToolButton.SetMenuItem(Value: TMenuItem);
begin
  if Value=FMenuItem then exit;
  // copy values from menuitem
  // is menuitem is still loading, skip this
  if (Value <> nil) and (not (csLoading in Value.ComponentState)) then begin
    CopyPropertiesFromMenuItem(Value);
  end;
  FMenuItem := Value;
  if FMenuItem <> nil then
    FMenuItem.FreeNotification(Self);
end;

procedure TToolButton.SetShowCaption(const AValue: boolean);
begin
  if FShowCaption=AValue then exit;
  FShowCaption:=AValue;
  if IsControlVisible then
  begin
    InvalidatePreferredSize;
    UpdateVisibleToolbar;
  end;
end;

procedure TToolButton.SetStyle(Value: TToolButtonStyle);
begin
  if FStyle = Value then exit;
  FStyle := Value;
  InvalidatePreferredSize;
  if IsControlVisible then
    UpdateVisibleToolbar;
end;

procedure TToolButton.SetWrap(Value: Boolean);
begin
  if FWrap = Value then exit;
  FWrap := Value;
  if FToolBar <> nil then
    RefreshControl;
end;

procedure TToolButton.TextChanged;
begin
  inherited TextChanged;
  if FToolbar = nil then Exit;
  if FToolbar.ShowCaptions
  then Invalidate;
end;

procedure TToolButton.SetMouseInControl(NewMouseInControl: Boolean);
begin
  //DebugLn('TToolButton.SetMouseInControl A ',Name,' Old=',FMouseInControl,' New=',NewMouseInControl);
  if FMouseInControl=NewMouseInControl then exit;
  FMouseInControl:=NewMouseInControl;
  //DebugLn('TToolButton.SetMouseInControl B ',Name,' Now=',FMouseInControl,' Down=',Down);
  Invalidate;
end;

procedure TToolButton.CMEnabledChanged(var Message: TLMEssage);
begin
  inherited;
  invalidate;
end;

procedure TToolButton.CMVisibleChanged(var Message: TLMessage);
begin
  if FToolBar <> nil then
    RefreshControl;
end;

procedure TToolButton.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TToolButton.EndUpdate;
begin
  Dec(FUpdateCount);
end;

{------------------------------------------------------------------------------
  procedure TToolButton.GetGroupBounds(var StartIndex, EndIndex: integer);
  
  Return the index of the first and the last ToolButton in the group.
  If no ToolBar then negative values are returned.
  If not in a group then StartIndex=EndIndex.
------------------------------------------------------------------------------}
procedure TToolButton.GetGroupBounds(var StartIndex, EndIndex: integer);
var
  CurButton: TToolButton;
begin
  StartIndex:=Index;
  EndIndex:=StartIndex;
  if (Style<>tbsCheck) or (not Grouped) then exit;
  while (StartIndex>0) do begin
    CurButton:=FToolBar.Buttons[StartIndex-1];
    if (CurButton<>nil) and CurButton.Grouped and (CurButton.Style in [tbsCheck, tbsSeparator, tbsDivider])
    then
      dec(StartIndex)
    else
      break;
  end;
  while (EndIndex<FToolBar.FButtons.Count-1) do begin
    CurButton:=FToolBar.Buttons[EndIndex+1];
    if (CurButton<>nil) and CurButton.Grouped and (CurButton.Style in [tbsCheck, tbsSeparator, tbsDivider])
    then
      inc(EndIndex)
    else
      break;
  end;
end;

function TToolButton.GetIndex: Integer;
begin
  if FToolBar <> nil then
    Result := FToolBar.FButtons.IndexOf(Self)
  else
    Result := -1;
end;

function TToolButton.GetTextSize: TSize;
var
  S: String;
begin
  S := Caption;
  DeleteAmpersands(S);
  Result := Canvas.TextExtent(S)
end;

procedure TToolButton.GetPreferredSize(
  var PreferredWidth, PreferredHeight: integer; Raw: boolean;
  WithThemeSpace: boolean);
begin
  inherited GetPreferredSize(PreferredWidth, PreferredHeight, Raw, WithThemeSpace);

  if FToolbar = nil then Exit;
  if FToolbar.ButtonHeight <= 0 then Exit;
  // buttonheight overrules in hor toolbar
  if FToolbar.Align in [alTop, alBottom]
  then PreferredHeight := FToolbar.ButtonHeight;
end;

function TToolButton.IsWidthStored: Boolean;
begin
  Result := Style in [tbsSeparator, tbsDivider];
end;

procedure TToolButton.RefreshControl;
begin
  UpdateControl;
end;

procedure TToolButton.UpdateControl;
begin
  UpdateVisibleToolbar;
end;

function TToolButton.CheckMenuDropdown: Boolean;
begin
  Result := (not (csDesigning in ComponentState))
            and (((DropdownMenu<>nil) and (DropdownMenu.AutoPopup))
                 or (MenuItem<>nil))
            and (FToolBar <> nil);
  if Result then
    Result:=FToolBar.CheckMenuDropdown(Self);
end;

procedure TToolButton.Click;
begin
  inherited Click;
end;

procedure TToolButton.GetCurrentIcon(var ImageList: TCustomImageList;
  var TheIndex: integer);
begin
  ImageList:=nil;
  TheIndex:=-1;
  if (ImageIndex<0) or (FToolBar=nil) then exit;
  
  if Style in [tbsButton,tbsDropDown,tbsCheck] then begin
    TheIndex:=ImageIndex;
    if Enabled and FMouseInControl then
      // if mouse over button then use HotImages
      ImageList:=FToolBar.HotImages
    else if not Enabled then
      // if button disabled then use HotImages
      ImageList:=FToolBar.DisabledImages;
    if (ImageList=nil) or (ImageList.Count<=ImageIndex) then begin
      // if no special icon available, then try the default Images
      ImageList:=FToolBar.Images;
      if (ImageList=nil) or (ImageList.Count<=ImageIndex) then begin
        // no icon available
        ImageList:=nil;
        TheIndex:=-1;
      end;
    end;
  end;
end;

function TToolButton.IsCheckedStored: Boolean;
begin
  Result := (ActionLink = nil)
            or (not TToolButtonActionLink(ActionLink).IsCheckedLinked);
end;

function TToolButton.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil)
            or (not TToolButtonActionLink(ActionLink).IsImageIndexLinked);
end;

procedure TToolButton.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TCustomAction then begin
    TCustomAction(Dest).Checked := Down;
    TCustomAction(Dest).ImageIndex := ImageIndex;
  end;
end;

function TToolButton.GetButtonDrawDetail: TThemedElementDetails;
var
  ToolDetail: TThemedToolBar;
begin
  if Style = tbsDropDown then
    ToolDetail := ttbSplitButtonNormal
  else
  if Style in [tbsDivider, tbsSeparator] then
    if FToolBar.IsVertical then
      ToolDetail := ttbSeparatorVertNormal
    else
      ToolDetail := ttbSeparatorNormal
  else
    ToolDetail := ttbButtonNormal;
    
  if not Enabled then
    inc(ToolDetail, 3) // ttbButtonDisabled
  else
  begin
    if Down then
    begin // checked states
      if FMouseInControl then
        inc(ToolDetail, 5) // ttbButtonCheckedHot
      else
        inc(ToolDetail, 4) // ttbButtonChecked
    end
    else
    begin
      if (tbfPressed in FToolButtonFlags) and FMouseInControl then
        inc(ToolDetail, 2) else // ttbButtonPressed
      if FMouseInControl then
        inc(ToolDetail, 1); // ttbButtonHot
    end;
  end;
  Result := ThemeServices.GetElementDetails(ToolDetail);
end;

procedure TToolButton.SetParent(AParent: TWinControl);
var
  i: Integer;
  NewWidth: Integer;
  NewHeight: Integer;
begin
  CheckNewParent(AParent);
  if AParent=Parent then exit;
  
  // remove from old button list
  if FToolBar<>nil then
    FToolBar.RemoveButton(Self);
  FToolBar:=nil;
  if AParent is TToolBar then
  begin
    if Style in [tbsButton,tbsDropDown,tbsCheck] then
      NewWidth := TToolBar(AParent).ButtonWidth
    else
      NewWidth := Width;
    NewHeight := TToolBar(AParent).ButtonHeight;
    SetBoundsKeepBase(Left, Top, NewWidth, NewHeight);
  end;
  
  // inherited
  inherited SetParent(AParent);
  
  // add to new button list
  if Parent is TToolBar then
  begin
    FToolBar := TToolBar(Parent);
    i := Index;
    if i < 0 then
      FToolBar.AddButton(Self);
    UpdateVisibleToolbar;
  end;
  //DebugLn(['TToolButton.SetParent A ',Name,' NewIndex=',Index]);
end;

procedure TToolButton.UpdateVisibleToolbar;
begin
  //DebugLn('TToolButton.UpdateVisibleToolbar ',Parent is TToolBar);
  if Parent is TToolBar then
    TToolBar(Parent).UpdateVisibleBar;
end;

function TToolButton.GroupAllUpAllowed: boolean;
var
  StartIndex, EndIndex: integer;
  i: Integer;
  CurButton: TToolButton;
begin
  Result:=true;
  if (Style=tbsCheck) and Grouped then begin
    GetGroupBounds(StartIndex,EndIndex);
    if (StartIndex>=0) then begin
      // allow all up, if one button has AllowAllUp
      Result:=false;
      for i:=StartIndex to EndIndex do begin
        CurButton:=FToolBar.Buttons[i];
        if CurButton.AllowAllUp then begin
          Result:=true;
          break;
        end;
      end;
    end;
  end;
end;

function TToolButton.DialogChar(var Message: TLMKey): boolean;
begin
  if IsAccel(Message.CharCode, Caption) and FToolBar.CanFocus then
  begin
    Click;
    Result := true;
  end else
    Result := inherited;
end;

procedure TToolButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  IconSize: TPoint;
  TextSize: TSize;
  TextPos: TPoint;
  IconPos: TPoint;
  ImgList: TCustomImageList;
  ImgIndex: integer;
begin
  if (FToolBar<>nil) then begin
    PreferredWidth:=0;
    PreferredHeight:=0;

    // calculate text size
    TextSize.cx:=0;
    TextSize.cy:=0;
    if (Style in [tbsButton,tbsDropDown,tbsCheck]) and (FToolBar.ShowCaptions) and
       //Allow hide caption only in list mode
       ((FToolBar.List and ShowCaption) or not FToolBar.List) then
    begin
      if (Caption<>'') then
      begin
        if FToolBar.HandleAllocated then
          TextSize := GetTextSize;
      end;
      // add space around text
      inc(TextSize.cx,4);
      inc(TextSize.cy,4);
    end;

    // calculate icon size
    IconSize:=Point(0,0);
    if (Style in [tbsButton,tbsDropDown,tbsCheck]) then
    begin
      GetCurrentIcon(ImgList,ImgIndex);
      if (ImgList <> nil) then
      begin
        IconSize := Point(ImgList.Width, ImgList.Height);
        if IconSize.y <= 0 then IconSize.X := 0;
      end;
    end;
    // calculate text and icon position
    TextPos:=Point(0,0);
    IconPos:=Point(0,0);
    if TextSize.cx>0 then begin
      if IconSize.X>0 then begin
        if FToolBar.List then begin
          // icon left of text
          TextPos.X:=IconPos.X+IconSize.X+2;
        end else begin
          // icon above text
          TextPos.Y:=IconPos.Y+IconSize.Y+2;
        end;
      end else begin
        // only text
      end;
    end else if IconSize.x>0 then begin
      // only icon
    end;
    
    PreferredWidth:=Max(IconPos.X+IconSize.X,TextPos.X+TextSize.cx);
    PreferredHeight:=Max(IconPos.Y+IconSize.Y,TextPos.Y+TextSize.cy);
    //DebugLn(['TToolButton.CalculatePreferredSize Preferred=',PreferredWidth,',',PreferredHeight,' Icon=',IconPos.X,'+',IconSize.X,' Text=',TextPos.X,'+',TextSize.cx]);
    //DebugLn(['TToolButton.CalculatePreferredSize Preferred=',PreferredWidth,',',PreferredHeight,' Icon=',IconPos.Y,'+',IconSize.Y,' Text=',TextPos.Y,'+',TextSize.cy]);

    // add button frame
    if (Style in [tbsButton, tbsDropDown, tbsCheck]) then
    begin
      inc(PreferredWidth, 4);
      inc(PreferredHeight, 4);
      PreferredWidth := Max(PreferredWidth, FToolBar.ButtonWidth);
      PreferredHeight := Max(PreferredHeight, FToolBar.ButtonHeight);
      if Style = tbsDropDown then
        inc(PreferredWidth, FToolBar.FDropDownWidth);
    end
    else
    if Style = tbsDivider then
      PreferredWidth := 4
    else
    if Style = tbsSeparator then
      PreferredWidth := 8;
  end;
  //DebugLn(['TToolButton.CalculatePreferredSize ',DbgSName(Self),' ',PreferredWidth,',',PreferredHeight,' Caption=',Caption]);
end;

class function TToolButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 23;
  Result.CY := 22;
end;


// included by comctrls.pp

