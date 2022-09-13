{%MainUnit ../comctrls.pp}

{ TToolButton

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
  Result := inherited IsCheckedLinked and
    (TToolButton(FClient).Down = (Action as TCustomAction).Checked);
end;

function TToolButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (TToolButton(FClient).ImageIndex = (Action as TCustomAction).ImageIndex);
end;

procedure TToolButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    TToolButton(FClient).Down := Value;
end;

procedure TToolButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    TToolButton(FClient).ImageIndex := Value;
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
  AccessibleRole := larToolBarButton;
end;

procedure TToolButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

  procedure SendButtonUpMsg;
  var
    msg: TLMMouse;
    pt: TPoint;
  begin
    FillChar({%H-}msg, SizeOf(msg), 0);
    msg.Msg:=LM_LBUTTONUP;
    pt := ScreenToClient(Mouse.CursorPos);
    msg.XPos:=pt.X;
    msg.YPos:=pt.Y;
    WndProc(TLMessage(msg));
  end;
var
  NewFlags: TToolButtonFlags;
  APointInArrow: Boolean;
begin
  //debugln(['TToolButton.MouseDown ',DbgSName(Self)]);
  SetMouseInControl(True);
  NewFlags := FToolButtonFlags - [tbfPressed, tbfArrowPressed];
  if (Button = mbLeft) then
  begin
    APointInArrow := PointInArrow(X, Y);
    //use some threshold to decide if the DropdownMenu should be opened again.
    //  When no DropdownMenu is assigned, FLastDropDownTick is always 0
    //  therefore the condition is always met.
    if Enabled and not(
          (GetTickCount64 < FLastDropDownTick + 100)
      and (APointInArrow or (Style<>tbsDropDown))) then
    begin
      if APointInArrow then
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

  FLastDown := Down;

  inherited MouseDown(Button, Shift, X, Y);

  FLastDropDownTick := 0;
  if (Button = mbLeft) and Enabled and
     (Style in [tbsButton, tbsDropDown, tbsButtonDrop]) then
  begin
    if ((Style in [tbsButton, tbsButtonDrop]) and (tbfPressed in NewFlags) or
        (Style = tbsDropDown) and (tbfArrowPressed in NewFlags)) and
       CheckMenuDropdown then
    begin
      FLastDropDownTick := GetTickCount64;

      //because we show the DropdownMenu in MouseDown, we have to send
      //  LM_LBUTTONUP manually to make it work in all widgetsets!
      // Some widgetsets work without it (e.g. win32) but some don't (e.g. carbon).
      SendButtonUpMsg;
    end else
    begin
      if (Style = tbsDropDown) and 
         (NewFlags * [tbfArrowPressed, tbfPressed] = [tbfPressed])
      then
        Down := True;
    end;
  end;
end;

procedure TToolButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ButtonPressed, ArrowPressed: Boolean;
  Pt: TPoint;
  NewFlags: TToolButtonFlags;
begin
  //DebugLn(['TToolButton.MouseUp ',Name,':',ClassName,' ',dbgs(ord(Button)),' ',X,',',Y]);
  FLastDown := False;
  NewFlags := FToolButtonFlags;
  ButtonPressed := (Button = mbLeft) and (tbfPressed in NewFlags);
  ArrowPressed := (Button = mbLeft) and (tbfArrowPressed in NewFlags);
  if ButtonPressed then
    Exclude(NewFlags, tbfPressed);
  if ArrowPressed then
    Exclude(NewFlags, tbfArrowPressed);
  if (tbfMouseInArrow in NewFlags) and PointInArrow(X, Y) then
    Exclude(NewFlags, tbfMouseInArrow);

  if NewFlags <> FToolButtonFlags then
  begin
    FToolButtonFlags := NewFlags;
    Invalidate;
  end;

  inherited MouseUp(Button, Shift, X, Y);

  if (Button = mbLeft) then
  begin
    if FMouseInControl then
    begin
      Pt := Point(X, Y);
      if not PtInRect(Rect(0,0,Width,Height), Pt) then
        SetMouseInControl(false);
    end;
    if (Style in [tbsButton, tbsDropDown, tbsButtonDrop]) then
      Down := False;
    //button is pressed, but DropdownMenu was not shown
    if FMouseInControl and (FLastDropDownTick = 0) then
    begin
      if ButtonPressed then
      begin
        if (Style = tbsCheck) then
          Down := not Down;
        Click;
      end else
      if ArrowPressed then
        ArrowClick;
      //DON'T USE the tool button (Self) after the click call because it could
      //have been destroyed in the OnClick event handler (e.g. Lazarus IDE does it)!
    end;
  end;
end;

procedure TToolButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = DropdownMenu then
      DropdownMenu := nil
    else
    if AComponent = MenuItem then
      MenuItem := nil;
  end;
end;

procedure TToolButton.Paint;

  procedure DrawDropDownArrow(OwnerDetails: TThemedElementDetails; const DropDownButtonRect: TRect);
  var
    Details: TThemedElementDetails;
    ArrowState: TThemedToolBar;
  begin
    if Style = tbsButtonDrop then
    begin
      if Enabled then
        ArrowState := ttbSplitButtonDropDownNormal
      else
        ArrowState := ttbSplitButtonDropDownDisabled;
    end else
    begin
      ArrowState := TThemedToolBar(ord(ttbSplitButtonDropDownNormal) + OwnerDetails.State - 1);
      if (tbfArrowPressed in FToolButtonFlags) and FMouseInControl and Enabled then
        ArrowState := ttbSplitButtonDropDownPressed
      else
      if (FToolButtonFlags*[tbfMouseInArrow,tbfPressed] = [tbfPressed]) and not FLastDown then
        ArrowState := ttbSplitButtonDropDownHot;
    end;
    Details := ThemeServices.GetElementDetails(ArrowState);
    if (FToolBar <> nil) and (not FToolBar.Flat)
    and (Style <> tbsButtonDrop) and (Details.State in [1, 4])
    then
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
      if (ARect.Bottom - ARect.Top) > 5 then
      begin
        ARect.Top := (ARect.Top + ARect.Bottom) div 2 - 3;
        ARect.Bottom := ARect.Top + 5;
      end;
    end
    else
    begin
      if (ARect.Right - ARect.Left) > 5 then
      begin
        ARect.Left := (ARect.Left + ARect.Right) div 2 - 3;
        ARect.Right := ARect.Left + 5;
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
        if (ARect.Bottom - ARect.Top) >= 10 then
        begin
          ARect.Top := (ARect.Top + ARect.Bottom) div 2 - 5;
          ARect.Bottom := ARect.Top + 5;
          DrawDivider(Details, ARect);
          OffsetRect(ARect, 0, 5);
          DrawDivider(Details, ARect);
        end
        else
          DrawDivider(Details, ARect);
      end
      else
      begin
        if (ARect.Right - ARect.Left) >= 10 then
        begin
          ARect.Left := (ARect.Left + ARect.Right) div 2 - 5;
          ARect.Right := ARect.Left + 5;
          DrawDivider(Details, ARect);
          OffsetRect(ARect, 5, 0);
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
  MainBtnRect: TRect;
  DropDownButtonRect: TRect;
  TextSize: TSize;
  TextPos: TPoint;
  dist, marg: Integer;
  IconSize: TSize;
  IconPos: TPoint;
  ImgList: TCustomImageList;
  ImgIndex: integer;
  Details, TempDetails: TThemedElementDetails;
  ImgEffect: TGraphicsDrawEffect;
begin
  if (FToolBar<>nil) and (ClientWidth>0) and (ClientHeight>0) then
  begin
    PaintRect := ClientRect; // the whole paint area

    // calculate button area(s)
    MainBtnRect := PaintRect;
    ButtonRect := PaintRect;
    Details := GetButtonDrawDetail;

    // OnDrawItem
    if Assigned(FToolBar.OnPaintButton) then
    begin
      if (Style in [tbsButton, tbsDropDown, tbsButtonDrop, tbsCheck]) then
      begin
        TempDetails := Details;
        if ((FToolBar <> nil) and not FToolBar.Flat) and (TempDetails.State in [1, 4]) then
          TempDetails.State := 2;
      end;

      FToolBar.OnPaintButton(Self, TempDetails.State);
      exit;
    end;

    if Style in [tbsDropDown, tbsButtonDrop] then
    begin
      DropDownButtonRect := ButtonRect;
      if Style = tbsDropDown then
        DropDownButtonRect.Left := DropDownButtonRect.Right-FToolBar.DropDownWidth
      else
      begin
        DropDownButtonRect.Left := DropDownButtonRect.Right-FToolBar.ButtonDropWidth;
        DropDownButtonRect.Right := DropDownButtonRect.Left + FToolBar.DropDownWidth;
      end;
      MainBtnRect.Right := DropDownButtonRect.Left;
      if Style = tbsDropDown then
        ButtonRect := MainBtnRect
      else
        Inc(MainBtnRect.Right, cDefButtonDropDecArrowWidth); // tbsButtonDrop ignore extra space between button and arrow
    end
    else
      DropDownButtonRect := Rect(0,0,0,0);

    // calculate text size
    TextSize.cx:=0;
    TextSize.cy:=0;
    if (Style in [tbsButton, tbsDropDown, tbsButtonDrop, tbsCheck]) and (FToolBar.ShowCaptions) and
      ((FToolbar.List and ShowCaption) or not FToolBar.List) and //Allow hide caption only in list mode
      (Caption <> '') then
      TextSize := GetTextSize;

    // calculate icon size
    IconSize := Size(0,0);
    GetCurrentIcon(ImgList, ImgIndex, ImgEffect);
    if (ImgList<>nil) then
    begin
      IconSize := ImgList.SizeForPPI[FToolBar.ImagesWidth, Font.PixelsPerInch];
      if IconSize.cy <= 0 then
        IconSize.cx := 0;
    end;

    // calculate text and icon position
    TextPos:=Point(0,0);
    IconPos:=Point(0,0);
    if TextSize.cx > 0 then
    begin
      if IconSize.cx > 0 then
      begin
        if FToolBar.List then
        begin
          // icon left of text
          dist := FToolbar.Scale96ToFont(cHorIconTextDist);
          IconPos.X:=(MainBtnRect.Left+MainBtnRect.Right-IconSize.cx-TextSize.cx-dist) div 2;
          IconPos.Y:=(MainBtnRect.Top+MainBtnRect.Bottom-IconSize.cy) div 2;
          TextPos.X:=IconPos.X+IconSize.cx+dist;
          TextPos.Y:=(MainBtnRect.Top+MainBtnRect.Bottom-TextSize.cy) div 2;
        end else
        begin
          // icon above text
          dist := cVertIconTextDist;
          IconPos.X:=(MainBtnRect.Left+MainBtnRect.Right-IconSize.cx) div 2;
          IconPos.Y:=(MainBtnRect.Top+MainBtnRect.Bottom-IconSize.cy-TextSize.cy-dist) div 2;
          TextPos.X:=(MainBtnRect.Left+MainBtnRect.Right-TextSize.cx) div 2;
          TextPos.Y:=IconPos.Y+IconSize.cy+dist;
        end;
      end else
      begin
        // only text
        TextPos.X:=(MainBtnRect.Left+MainBtnRect.Right-TextSize.cx) div 2;
        TextPos.Y:=(MainBtnRect.Top+MainBtnRect.Bottom-TextSize.cy) div 2;
      end;
    end else
    if IconSize.cx>0 then
    begin
      // only icon
      IconPos.X:=(MainBtnRect.Left+MainBtnRect.Right-IconSize.cx) div 2;
      IconPos.Y:=(MainBtnRect.Top+MainBtnRect.Bottom-IconSize.cy) div 2;
    end;

    // draw button
    if (Style in [tbsButton, tbsDropDown, tbsButtonDrop, tbsCheck]) then
    begin
      // non-Flat toolbars come from old windows where you was able to set how
      // to draw it by adjusting toolbar window options
      // with current windows toolbars should be drawn using Theme
      // so let's treat flat toolbars as starndard toolbars and draw them using ThemeManager
      // and to draw a non-Flat toolbars we need to somehow mimic always raised state
      // of their buttons - a good way is to draw them using Hot style also for
      // normal and disables states
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
      if ThemeServices.ThemesEnabled then begin
        Details:=ThemeServices.GetElementDetails(ttbSeparatorNormal);
        ThemeServices.DrawElement(Canvas.Handle,Details,ClientRect)
      end else
        DrawSeparator(Details, ButtonRect);
      ButtonRect := Rect(0, 0, 0, 0); // nothing can be drawn on separator
    end;

    // draw dropdown button
    if Style in [tbsDropDown, tbsButtonDrop] then
      DrawDropDownArrow(Details, DropDownButtonRect);

    // draw icon
    if (ImgList<>nil) then
      ImgList.ResolutionForPPI[FToolBar.ImagesWidth, Font.PixelsPerInch, GetCanvasScaleFactor]
        .Draw(Canvas, IconPos.X, IconPos.Y, ImgIndex, ImgEffect);

    // draw text
    if (TextSize.cx > 0) then
    begin
      MainBtnRect.Left := TextPos.X;
      MainBtnRect.Top := TextPos.Y;
      // if State is disabled then change to PushButtonDisabled since
      // ToolButtonDisabled text looks not disabled though windows native toolbutton
      // text drawn with disabled look. For other widgetsets there is no difference which
      // disabled detail to use
      TempDetails := Details;
      if TempDetails.State = 4 then
        TempDetails := ThemeServices.GetElementDetails(tbPushButtonDisabled);
      ThemeServices.DrawText(Canvas, TempDetails, Caption, MainBtnRect,
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

function TToolButton.PointInArrow(const X, Y: Integer): Boolean;
begin
  Result := (Style = tbsDropDown) and (FToolBar <> nil)
    and (Y >= 0) and (Y <= ClientHeight)
    and (X > ClientWidth - FToolBar.DropDownWidth) and (X <= ClientWidth);
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
    GetAccessibleObject.AccessibleName := AValue;
    inherited RealSetText(AValue);
    AdjustSize;
  end
  else
    inherited RealSetText(AValue);
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
  if Sender is TCustomAction then
  begin
    NewAction := TCustomAction(Sender);
    if (not CheckDefaults) or (not Down) then
      Down := NewAction.Checked;
    if (not CheckDefaults) or (ImageIndex<0) then
      ImageIndex := NewAction.ImageIndex;
  end;
end;

procedure TToolButton.ArrowClick;
begin
  if Assigned(FOnArrowClick) then
    FOnArrowClick(Self);
end;

function TToolButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TToolButtonActionLink;
end;

procedure TToolButton.CopyPropertiesFromMenuItem(const Value: TMenuItem);
begin
  if not Assigned(Value) then Exit;
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
  // DebugLn('TToolButton.MouseEnter ',Name);
  inherited MouseEnter;
  SetMouseInControl(true);
end;

procedure TToolButton.MouseLeave;
begin
  // DebugLn('TToolButton.MouseLeave ',Name);
  inherited MouseLeave;

  if not(tbfDropDownMenuShown in FToolButtonFlags) then
  begin
    if (not MouseCapture)
    and ([tbfPressed, tbfArrowPressed, tbfMouseInArrow] * FToolButtonFlags <> []) then
    begin
      Exclude(FToolButtonFlags, tbfPressed);
      Exclude(FToolButtonFlags, tbfArrowPressed);
      Exclude(FToolButtonFlags, tbfMouseInArrow);
    end;
    SetMouseInControl(false);
  end;
end;

procedure TToolButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewFlags: TToolButtonFlags;
begin
  inherited MouseMove(Shift, X, Y);

  if (not MouseCapture) and (Style = tbsDropDown) and (FToolBar <> nil) then
  begin
    NewFlags := FToolButtonFlags;
    if PointInArrow(X, Y) then
      Include(NewFlags, tbfMouseInArrow)
    else
      Exclude(NewFlags, tbfMouseInArrow);

    if NewFlags <> FToolButtonFlags then
    begin
      FToolButtonFlags := NewFlags;
      Invalidate;
    end;
  end;
end;

procedure TToolButton.SetDown(Value: Boolean);
var
  StartIndex, EndIndex: integer;
  i: Integer;
  CurButton: TToolButton;
begin
  if Value = FDown then exit;
  if (csLoading in ComponentState) then
  begin
    FDown := Value;
    Exit;
  end;
  
  //DebugLn('TToolButton.SetDown ',Style=tbsCheck,',',FDown,',',GroupAllUpAllowed);
  if (Style = tbsCheck) and FDown and (not GroupAllUpAllowed) then
    Exit;

  FDown := Value;
  
  if (Style = tbsCheck) and FDown and Grouped then
  begin
    // uncheck all other in the group
    GetGroupBounds(StartIndex, EndIndex);
    if StartIndex >= 0 then
    begin
      for i := StartIndex to EndIndex do
      begin
        CurButton := FToolBar.Buttons[i];
        if (CurButton <> Self) and (CurButton.FDown) then
        begin
          CurButton.FDown := False;
          CurButton.Invalidate;
        end;
      end;
    end;
  end;

  Invalidate;
  if Assigned(FToolBar) then
    FToolBar.ToolButtonDown(Self, FDown);
end;

procedure TToolButton.SetDropdownMenu(Value: TPopupMenu);
begin
  if Value = FDropdownMenu then exit;
  FDropdownMenu := Value;
  if Assigned(Value) then
    Value.FreeNotification(Self);
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
  while FGrouped and (Style = tbsCheck) and Assigned(FToolBar) do
  begin
    GetGroupBounds(StartIndex, EndIndex);
    if StartIndex >= 0 then
    begin
      CheckedIndex := -1;
      i := StartIndex;
      while i <= EndIndex do
      begin
        CurButton := FToolBar.Buttons[i];
        if CurButton.Down then
        begin
          if CheckedIndex < 0 then
            CheckedIndex := i
          else
          begin
            CurButton.Down := False;
            // the last operation can change everything -> restart
            break;
          end;
        end;
        inc(i);
      end;
      if i > EndIndex then break;
    end;
  end;
end;

procedure TToolButton.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex = Value then exit;
  FImageIndex := Value;
  if IsControlVisible and Assigned(FToolBar) then
    Invalidate;
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
  if Value = FMenuItem then Exit;
  // copy values from menuitem
  // is menuitem is still loading, skip this
  if Assigned(Value) and not (csLoading in Value.ComponentState) then
    CopyPropertiesFromMenuItem(Value);
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
  case Value of
    tbsSeparator: begin
      Width := cDefSeparatorWidth;
      Height := cDefSeparatorWidth;
    end;
    tbsDivider: begin
      Width := cDefDividerWidth;
      Height := cDefDividerWidth;
    end;
  end;
  InvalidatePreferredSize;
  if IsControlVisible then
    UpdateVisibleToolbar;
end;

procedure TToolButton.SetWrap(Value: Boolean);
begin
  if FWrap = Value then exit;
  FWrap := Value;
  if Assigned(FToolBar) then
    RefreshControl;
end;

procedure TToolButton.TextChanged;
begin
  inherited TextChanged;
  if FToolbar = nil then Exit;
  if FToolbar.ShowCaptions then
    Invalidate;
end;

procedure TToolButton.SetMouseInControl(NewMouseInControl: Boolean);
begin
  //DebugLn('TToolButton.SetMouseInControl A ',Name,' Old=',FMouseInControl,' New=',NewMouseInControl);
  if FMouseInControl = NewMouseInControl then exit;
  FMouseInControl := NewMouseInControl;
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
  StartIndex := Index;
  EndIndex := StartIndex;
  if (Style <> tbsCheck) or (not Grouped) then exit;
  while (StartIndex>0) do
  begin
    CurButton:=FToolBar.Buttons[StartIndex-1];
    if (CurButton<>nil) and CurButton.Grouped and (CurButton.Style in [tbsCheck, tbsSeparator, tbsDivider]) then
      dec(StartIndex)
    else
      break;
  end;
  while (EndIndex < FToolBar.FButtons.Count-1) do
  begin
    CurButton := FToolBar.Buttons[EndIndex+1];
    if Assigned(CurButton) and CurButton.Grouped and (CurButton.Style in [tbsCheck, tbsSeparator, tbsDivider]) then
      inc(EndIndex)
    else
      break;
  end;
end;

function TToolButton.GetIndex: Integer;
begin
  if Assigned(FToolBar) then
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
var
  RealButtonWidth, RealButtonHeight: Integer;
begin
  inherited GetPreferredSize(PreferredWidth, PreferredHeight, Raw, WithThemeSpace);

  if FToolbar = nil then Exit;
  RealButtonWidth := FToolbar.ButtonWidth;
  RealButtonHeight := FToolbar.ButtonHeight;
  if RealButtonHeight <= 0 then Exit;
  // buttonheight overrules in hor toolbar
  if FToolBar.IsVertical then
    PreferredWidth := RealButtonWidth
  else
    PreferredHeight := RealButtonHeight;
end;

function TToolButton.IsWidthStored: Boolean;
begin
  Result := Style in [tbsSeparator, tbsDivider];
  if FToolBar<>nil then
    Result := Result and FToolBar.IsVertical;
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
  Result := (not (csDesigning in ComponentState)) and
    ((Assigned(DropdownMenu) and (DropdownMenu.AutoPopup)) or Assigned(MenuItem)) and Assigned(FToolBar);
  if Result then
  begin
    Include(FToolButtonFlags, tbfDropDownMenuShown);
    try
      Result := FToolBar.CheckMenuDropdown(Self);
    finally
      Exclude(FToolButtonFlags, tbfDropDownMenuShown);
    end;
  end;
end;

procedure TToolButton.Click;
begin
  inherited Click;
end;

procedure TToolButton.GetCurrentIcon(var ImageList: TCustomImageList;
  var TheIndex: integer; var TheEffect: TGraphicsDrawEffect);
var
  UseAutoEffects: Integer;
begin
  ImageList := nil;
  TheIndex := -1;
  TheEffect := gdeNormal;
  UseAutoEffects := ThemeServices.GetOption(toUseGlyphEffects);
  if (ImageIndex < 0) or (FToolBar = nil) then Exit;
  
  if Style in [tbsButton, tbsDropDown, tbsButtonDrop, tbsCheck] then
  begin
    TheIndex := ImageIndex;
    ImageList := FToolBar.Images;
    if (FToolButtonFlags*[tbfPressed,tbfArrowPressed] = [tbfPressed]) then
    begin
      // if button pressed then use PressedImages // Maybe To-Do ?
      {if (FToolBar.PressedImages <> nil) and (ImageIndex < FToolBar.PressedImages.Count) then
        ImageList := FToolBar.DisabledImages
      else} if UseAutoEffects > 0 then
        TheEffect := gdeShadowed;
    end else
    if Enabled and FMouseInControl then
    begin
      // if mouse over button then use HotImages
      if (FToolBar.HotImages <> nil) and (ImageIndex < FToolBar.HotImages.Count) then
        ImageList := FToolBar.HotImages
      else if UseAutoEffects > 0 then
        TheEffect := gdeHighlighted;
    end else
    if not Enabled then
    begin
      // if button disabled then use DisabledImages
      if (FToolBar.DisabledImages <> nil) and (ImageIndex < FToolBar.DisabledImages.Count) then
        ImageList := FToolBar.DisabledImages
      else
        TheEffect := gdeDisabled;
    end;
  end;
end;

function TToolButton.IsCheckedStored: Boolean;
begin
  Result := (ActionLink = nil) or not TToolButtonActionLink(ActionLink).IsCheckedLinked;
end;

function TToolButton.IsHeightStored: Boolean;
begin
  Result := Style in [tbsSeparator, tbsDivider];
  if FToolBar<>nil then
    Result := Result and not FToolBar.IsVertical;
end;

function TToolButton.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or not TToolButtonActionLink(ActionLink).IsImageIndexLinked;
end;

procedure TToolButton.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TCustomAction then
  begin
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
      if (tbfPressed in FToolButtonFlags) and FMouseInControl then
        inc(ToolDetail, 2) // ttbButtonPressed
      else if FMouseInControl then
        inc(ToolDetail, 5) // ttbButtonCheckedHot
      else
        inc(ToolDetail, 4);// ttbButtonChecked
    end
    else
    begin
      if (tbfPressed in FToolButtonFlags) and FMouseInControl then
        inc(ToolDetail, 2) // ttbButtonPressed
      else if FMouseInControl then
        inc(ToolDetail, 1);// ttbButtonHot
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
  if Assigned(FToolBar) then
    FToolBar.RemoveButton(Self);
  FToolBar := nil;
  if AParent is TToolBar then
  begin
    if not TToolBar(AParent).IsVertical then begin
      if Style in [tbsButton,tbsDropDown,tbsButtonDrop,tbsCheck] then
        NewWidth := TToolBar(AParent).ButtonWidth
      else
        NewWidth := Width;
      NewHeight := TToolBar(AParent).ButtonHeight;
    end else begin
      if Style in [tbsButton,tbsDropDown,tbsButtonDrop,tbsCheck] then
        NewHeight := TToolBar(AParent).ButtonHeight
      else
        NewHeight := Height;
      NewWidth := TToolBar(AParent).ButtonWidth;
    end;
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
  Result := True;
  if (Style = tbsCheck) and Grouped then
  begin
    GetGroupBounds(StartIndex, EndIndex);
    if (StartIndex >= 0) then
    begin
      // allow all up, if one button has AllowAllUp
      Result := False;
      for i := StartIndex to EndIndex do
      begin
        CurButton := FToolBar.Buttons[i];
        if CurButton.AllowAllUp then
        begin
          Result := True;
          break;
        end;
      end;
    end;
  end;
end;

function TToolButton.DialogChar(var Message: TLMKey): boolean;
begin
  if IsAccel(Message.CharCode, Caption) and FToolBar.ShowCaptions then
  begin
    Click;
    Result := true;
  end else
    Result := inherited;
end;

procedure TToolButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  IconSize: TSize;
  TextSize: TSize;
  TextPos: TPoint;
  IconPos: TPoint;
  dist: Integer;
  ImgList: TCustomImageList;
  ImgIndex: integer;
  ImgEffect: TGraphicsDrawEffect;
begin
  if Assigned(FToolBar) then
  begin
    PreferredWidth := 0;
    PreferredHeight := 0;

    // calculate text size
    TextSize.cx := 0;
    TextSize.cy := 0;
    if (Style in [tbsButton, tbsDropDown, tbsButtonDrop, tbsCheck]) and (FToolBar.ShowCaptions) and
       //Allow hide caption only in list mode
       ((FToolBar.List and ShowCaption) or not FToolBar.List) then
    begin
      if (Caption<>'') then
      begin
        if FToolBar.HandleAllocated then
          TextSize := GetTextSize;
      end;
      // add space around text
      dist := FToolbar.Scale96ToFont(4);
      inc(TextSize.cx, dist);
      inc(TextSize.cy, dist);
    end;

    // calculate icon size
    IconSize := Size(0, 0);
    if (Style in [tbsButton, tbsDropDown, tbsButtonDrop, tbsCheck]) then
    begin
      GetCurrentIcon(ImgList, ImgIndex, ImgEffect);
      if Assigned(ImgList) then
      begin
        IconSize := ImgList.SizeForPPI[FToolBar.ImagesWidth, FToolBar.Font.PixelsPerInch];
        if IconSize.cy <= 0 then IconSize.cx := 0;
      end;
    end;
    // calculate text and icon position
    TextPos := Point(0, 0);
    IconPos := Point(0, 0);
    if TextSize.cx > 0 then
    begin
      if IconSize.cx > 0 then
      begin
        if FToolBar.List then
        begin
          // icon left of text
          dist := FToolbar.Scale96ToFont(cHorIconTextDist);
          TextPos.X := IconPos.X + IconSize.cx + dist;
        end
        else
        begin
          // icon above text
          dist := FToolbar.Scale96ToFont(cVertIconTextDist);
          TextPos.Y := IconPos.Y + IconSize.cy + dist;
        end;
      end
      else
      begin
        // only text
      end;
    end
    else
    if IconSize.cx > 0 then
    begin
      // only icon
    end;
    
    PreferredWidth := Max(IconPos.X + IconSize.cx, TextPos.X + TextSize.cx);
    PreferredHeight := Max(IconPos.Y + IconSize.cy, TextPos.Y + TextSize.cy);
    //DebugLn(['TToolButton.CalculatePreferredSize Preferred=',PreferredWidth,',',PreferredHeight,' Icon=',IconPos.X,'+',IconSize.Width,' Text=',TextPos.X,'+',TextSize.cx]);
    //DebugLn(['TToolButton.CalculatePreferredSize Preferred=',PreferredWidth,',',PreferredHeight,' Icon=',IconPos.Y,'+',IconSize.Height,' Text=',TextPos.Y,'+',TextSize.cy]);

    // add button frame
    if (Style in [tbsButton, tbsDropDown, tbsButtonDrop, tbsCheck]) then
    begin
      inc(PreferredWidth, 4);
      inc(PreferredHeight, 4);
      PreferredWidth := Max(PreferredWidth, FToolBar.ButtonWidth);
      PreferredHeight := Max(PreferredHeight, FToolBar.ButtonHeight);
      case Style of
        tbsDropDown: inc(PreferredWidth, FToolBar.DropDownWidth);
        tbsButtonDrop:  inc(PreferredWidth, FToolBar.ButtonDropWidth-cDefButtonDropDecArrowWidth);
      end;
    end
    else
    if Style = tbsDivider then
      if FToolBar.IsVertical then
        PreferredHeight := cDefDividerWidth
      else
        PreferredWidth := cDefDividerWidth
    else
    if Style = tbsSeparator then
      if FToolBar.IsVertical then
        PreferredHeight := cDefSeparatorWidth
      else
        PreferredWidth := cDefSeparatorWidth;
  end;
  //DebugLn(['TToolButton.CalculatePreferredSize ',DbgSName(Self),' ',PreferredWidth,',',PreferredHeight,' Caption=',Caption]);
end;

class function TToolButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 23;
  Result.CY := 22;
end;


// included by comctrls.pp

