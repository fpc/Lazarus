{ TToolButton

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

}

constructor TToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csToolButton;
  ControlStyle := [csCaptureMouse, csSetCaption];
  FImageIndex := -1;
  FStyle := tbsButton;
  SetBounds(1,1,23,22);
end;

procedure TToolButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Style = tbsDropDown) and (Button = mbLeft) and Enabled then
    Down := not Down;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TToolButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if (Style = tbsDropDown) and MouseCapture then
    Down := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
end;

procedure TToolButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and (X >= 0) and (X < ClientWidth) and (Y >= 0) and
    (Y <= ClientHeight) then
  begin
    if Style = tbsDropDown then Down := False;
    Click;
  end;
end;

procedure TToolButton.Click;
begin
  inherited Click;
end;

procedure TToolButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = DropdownMenu then
      DropdownMenu := nil
    else if AComponent = MenuItem then
      MenuItem := nil;
  end;
end;

procedure TToolButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  Pos: Integer;
  Reordered, NeedsUpdate: Boolean;
  ResizeWidth, ResizeHeight: Boolean;
begin
  if ((ALeft <> Left) or (ATop <> Top) or
    (AWidth <> Width) or (AHeight <> Height)) and
    (FUpdateCount = 0) and not (csLoading in ComponentState) and
    (FToolBar <> nil) then
  begin
    Pos := Index;
    FToolbar.HandleNeeded;
    Reordered := FToolBar.ReorderButton(Pos, ALeft, ATop) <> Pos;
    if Reordered then
    begin
      NeedsUpdate := False;
      if Index < Pos then Pos := Index
    end
    else
    begin
      NeedsUpdate := (Style in [tbsSeparator, tbsDivider]) and (AWidth <> Width);
      Reordered := NeedsUpdate;
    end;
    if (Style = tbsDropDown) and (not FToolBar.Flat) then
        AWidth := FToolBar.ButtonWidth + AWidth - Width;
    ResizeWidth := not (Style in [tbsSeparator, tbsDivider]) and
      (AWidth <> FToolBar.ButtonWidth);
    ResizeHeight := AHeight <> FToolBar.ButtonHeight;
    if NeedsUpdate then inherited SetBounds(ALeft, ATop, AWidth, AHeight);
    if csDesigning in ComponentState then
    begin
      if ResizeWidth then FToolBar.ButtonWidth := AWidth;
      if ResizeHeight then FToolBar.ButtonHeight := AHeight;
    end;
    if Reordered and not ResizeWidth and not ResizeHeight then
    begin
      if NeedsUpdate then
        if Style in [tbsSeparator, tbsDivider] then
          FToolBar.RefreshButton(Pos)
        else
          FToolBar.UpdateButton(Pos);
      FToolBar.ResizeButtons;
      FToolBar.RepositionButtons(0);
    end
    else
      FToolBar.RepositionButton(Pos);
  end
  else inherited SetBounds(ALeft, ATop, AWidth, AHeight);

Assert(False, 'Trace:EXITING TTOOLBUTTON.SETBOUNDS');

end;

(*
procedure TToolButton.Paint;
const
  XorColor = $00FFD8CE;
var
  R: TRect;
begin
  if FToolBar = nil then Exit;
  if Style = tbsDivider then
    with Canvas do
    begin
      R := Rect(Width div 2 - 1, 0, Width, Height);
      DrawEdge(Handle, R, EDGE_ETCHED, BF_LEFT)
    end;
  if csDesigning in ComponentState then
    { Draw separator }
    if Style in [tbsSeparator, tbsDivider] then
      with Canvas do
      begin
        Pen.Style := psDot;
        Pen.Mode := pmXor;
        Pen.Color := XorColor;
        Brush.Style := bsClear;
        Rectangle(0, 0, ClientWidth, ClientHeight);
      end
    else if FToolBar.Flat and not Down then
      with Canvas do
      begin
        R := Rect(0, 0, Width, Height);
        DrawEdge(Handle, R, BDR_RAISEDINNER, BF_RECT);
      end;
end;

*)

function TToolButton.GetButtonState: Byte;
begin
  Result := 0;
  if FDown then
    if Style = tbsCheck then
      Result := Result or ButtonStates[tbsChecked]
    else
      Result := Result or ButtonStates[tbsPressed];
  if Enabled and ((FToolBar = nil) or FToolBar.Enabled) then
    Result := Result or ButtonStates[tbsEnabled];
  if not Visible and not (csDesigning in ComponentState) then
    Result := Result or ButtonStates[tbsHidden];
  if FIndeterminate then Result := Result or ButtonStates[tbsIndeterminate];
  if FWrap then Result := Result or ButtonStates[tbsWrap];
  if FMarked then Result := Result or ButtonStates[tbsMarked];
end;

procedure TToolButton.SetAutoSize(const Value: Boolean);
begin
  if Value <> AutoSize then
  begin
    Inherited SetAutoSize(Value);
    UpdateControl;
    if not (csLoading in ComponentState) and (FToolBar <> nil) and
      FToolBar.ShowCaptions then
    begin
      FToolBar.FButtonWidth := 0;
      FToolBar.FButtonHeight := 0;
      FToolBar.RecreateButtons;
    end;
  end;
end;

procedure TToolButton.SetButtonState(State: Byte);
begin
  FDown := State and (TBSTATE_CHECKED or TBSTATE_PRESSED) <> 0;
  Enabled := State and TBSTATE_ENABLED <> 0;
  if not (csDesigning in ComponentState) then
    Visible := State and TBSTATE_HIDDEN = 0;
  FIndeterminate := not FDown and (State and TBSTATE_INDETERMINATE <> 0);
  FWrap := State and TBSTATE_WRAP <> 0;
  FMarked := State and TBSTATE_MARKED <> 0;
end;

procedure TToolButton.SetToolBar(AToolBar: TToolBar);
begin
  if FToolBar <> AToolBar then
  begin
    if FToolBar <> nil then FToolBar.RemoveButton(Self);
    Parent := AToolBar;
    if AToolBar <> nil then AToolBar.InsertButton(Self);
  end;
end;

procedure TToolButton.CMVisibleChanged(var Message: TLMessage);
begin
  if not (csDesigning in ComponentState) and (FToolBar <> nil) then
  begin
    if FToolBar <> nil then
      with FToolBar do
      begin
        Perform(TB_HIDEBUTTON, Index, Longint(Ord(not Self.Visible)));
        { Force a resize to occur }
        if AutoSize then AdjustSize;
      end;
    UpdateControl;
    FToolBar.RepositionButtons(Index);
  end;
end;

procedure TToolButton.CMEnabledChanged(var Message: TLMessage);
begin
  if FToolBar <> nil then
    FToolBar.Perform(TB_ENABLEBUTTON, Index, Ord(Enabled));
end;

procedure TToolButton.CMHitTest(var Message: TCMHitTest);
begin
  Message.Result := Ord(not (Style in [tbsDivider, tbsSeparator]) or (DragKind = dkDock));
end;

procedure TToolButton.SetDown(Value: Boolean);
const
  DownMessage: array[Boolean] of Integer = (TB_PRESSBUTTON, TB_CHECKBUTTON);
begin
  if Value <> FDown then
  begin
    FDown := Value;
    if FToolBar <> nil then
    begin
      FToolBar.Perform(DownMessage[Style = tbsCheck], Index, MakeLong(Ord(Value), 0));
      FToolBar.UpdateButtonStates;
    end;
  end;
end;

procedure TToolButton.SetDropdownMenu(Value: TPopupMenu);
begin
  if Value <> FDropdownMenu then
  begin
    FDropdownMenu := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

procedure TToolButton.SetGrouped(Value: Boolean);
begin
  if FGrouped <> Value then
  begin
    FGrouped := Value;
    UpdateControl;
  end;
end;

procedure TToolButton.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if FToolBar <> nil then
    begin
      RefreshControl;
      FToolBar.Perform(TB_CHANGEBITMAP, Index, Value);
      if FToolBar.Transparent or FToolBar.Flat then Invalidate;
    end;
  end;
end;

procedure TToolButton.SetMarked(Value: Boolean);
begin
  if FMarked <> Value then
  begin
    FMarked := Value;
    if FToolBar <> nil then
      FToolBar.Perform(TB_MARKBUTTON, Index, Longint(Ord(Value)));
  end;
end;

procedure TToolButton.SetIndeterminate(Value: Boolean);
begin
  if FIndeterminate <> Value then
  begin
    if Value then SetDown(False);
    FIndeterminate := Value;
    if FToolBar <> nil then
      FToolBar.Perform(TB_INDETERMINATE, Index, Longint(Ord(Value)));
  end;
end;

procedure TToolButton.SetMenuItem(Value: TMenuItem);
begin
  if Value <> nil then
  begin
    if FMenuItem <> Value then
      Value.FreeNotification(Self);
    Caption := Value.Caption;
    Down := Value.Checked;
    Enabled := Value.Enabled;
    Hint := Value.Hint;
    ImageIndex := Value.ImageIndex;
    Visible := Value.Visible;
  end;
  FMenuItem := Value;
end;

procedure TToolButton.SetStyle(Value: TToolButtonStyle);
begin
    if FStyle <> Value then
  begin
    FStyle := Value;
    Invalidate;
    if not (csLoading in ComponentState) and (FToolBar <> nil) then
    begin
      if FToolBar.ShowCaptions then
      begin
        FToolBar.FButtonWidth := 0;
        FToolBar.FButtonHeight := 0;
        FToolBar.RecreateButtons
      end
      else
      begin
        if Style in [tbsDivider, tbsSeparator] then
          RefreshControl else
          UpdateControl;
        FToolBar.ResizeButtons;
        FToolbar.RepositionButtons(Index);
      end;
      FToolBar.AdjustSize;
    end;
  end;
end;

procedure TToolButton.SetWrap(Value: Boolean);
begin
  if FWrap <> Value then
  begin
    FWrap := Value;
    if FToolBar <> nil then
      RefreshControl;
  end;
end;

procedure TToolButton.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TToolButton.EndUpdate;
begin
  Dec(FUpdateCount);
end;

function TToolButton.GetIndex: Integer;
begin
  if FToolBar <> nil then
    Result := FToolBar.FButtons.IndexOf(Self)
  else
    Result := -1;
end;

function TToolButton.IsWidthStored: Boolean;
begin
  Result := Style in [tbsSeparator, tbsDivider];
end;

procedure TToolButton.RefreshControl;
begin
  if (FToolBar <> nil) and FToolBar.RefreshButton(Index) then
  begin
  //TODO: Finish me!
  end;
end;

procedure TToolButton.UpdateControl;
begin
  if FToolBar <> nil then FToolBar.UpdateButton(Index);
end;

function TToolButton.CheckMenuDropdown: Boolean;
begin
  Result := not (csDesigning in ComponentState) and ((DropdownMenu <> nil) and
    (DropdownMenu.AutoPopup) or (MenuItem <> nil)) and (FToolBar <> nil) and
    FToolBar.CheckMenuDropdown(Self);
end;

function TToolButton.IsCheckedStored: Boolean;
begin
//TODO: TTOOLBUTTON.ISCHECKEDSTORED
  Result := true;
end;

function TToolButton.IsImageIndexStored: Boolean;
begin
//TODO: TTOOLBUTTON.ISIMAGEINDEXSTORED
result := True;
end;

{procedure TToolButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
//TODO: NOT USED YET
end;
}
{function TToolButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TToolButtonActionLink;
end;
}
procedure TToolButton.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
{  if Dest is TCustomAction then
    with TCustomAction(Dest) do
    begin
      Checked := Self.Down;
      ImageIndex := Self.ImageIndex;
    end;
}
end;

procedure TToolButton.ValidateContainer(AComponent: TComponent);
var
  W: Integer;
begin
  inherited ValidateContainer(AComponent);
  if (csLoading in ComponentState) and (AComponent is TToolBar) then
  begin
    if Style in [tbsDivider, tbsSeparator] then
      W := Width else
      W := TToolBar(AComponent).ButtonWidth;
    SetBounds(Left, Top, W, TToolBar(AComponent).ButtonHeight);
  end;
end;

{
  $Log$
  Revision 1.3  2002/09/03 08:07:20  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.2  2002/05/10 06:05:56  lazarus
  MG: changed license to LGPL

  Revision 1.1  2000/07/13 10:28:28  michael
  + Initial import

  Revision 1.2  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:57  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.7  1999/12/30 18:54:36  lazarus
  Fixed the problem that occured when more than one button was added to the toolbar.
  Also, I set it up so practically any widget (component) can be added to the toolbar now.  In main.pp I have a TCOMBOBOX control being added.  I will create a example program and place it into the examples directory.
  Shane

  Revision 1.6  1999/12/29 20:38:23  lazarus
  Modified the toolbar so it now displays itself.  However, I can only add one button at this point.  I will fix that soon....

  Shane

  Revision 1.5  1999/12/23 21:48:13  lazarus
  *** empty log message ***

  Revision 1.4  1999/12/23 19:50:54  lazarus
  Working on the toolbar again.  Haven't been able to get it to display at all yet.

  gtkobject.inc - removed Intsendmessage and Intsendmessage2
  WinControl.inc - addded code to InsertControl so when a control is added to a parent's control list, a CMCONTROLCHANGED message is sent.  This way the parent can react to the addition.

  Shane

  Revision 1.2  1999/12/22 14:37:42  lazarus
  *** empty log message ***

  Revision 1.1  1999/12/22 14:33:36  lazarus
  Initial addition of a few new files.
  Shane

  Revision 1.0  1999/12/09 16:22:19  lazarus
  Templates initially created      SM

}

