{******************************************************************************
                                  TToolbar
 ******************************************************************************

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
{------------------------------------------------------------------------------
  Method: TToolbar.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TToolbar.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csMenuEvents, csSetCaption];
  SetBounds(1,1,150,29);
  Align := alTop;
  EdgeBorders := [ebTop];
  FButtonWidth := 23;
  FButtonHeight := 22;
  FNewStyle := True;
  FWrapable := True;
  FButtons := TList.Create;
  fCompStyle := csToolbar;

  FNullBitmap := TBitmap.Create;
  with FNullBitmap do
  begin
    Width := 1;
    Height := 1;
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Rect(0,0,1,1));
  end;
end;

destructor TToolBar.Destroy;
var
  I: Integer;
begin
  FreeThenNil(FNullBitmap);

  for I := 0 to FButtons.Count - 1 do
    if TControl(FButtons[I]) is TToolButton then
      TToolButton(FButtons[I]).FToolBar := nil;

  FreeThenNil(FButtons);
  inherited Destroy;
end;

procedure TToolBar.CreateParams(var Params: TCreateParams);
const
  TBSTYLE_TRANSPARENT = $8000;  // IE4 style -this is not valid but still needed as a holder
  DefaultStyles =
    CCS_NOPARENTALIGN or CCS_NOMOVEY or CCS_NORESIZE or CCS_NODIVIDER;
  ListStyles: array[Boolean] of DWORD = (0, TBSTYLE_LIST);
  FlatStyles: array[Boolean] of DWORD = (0, TBSTYLE_FLAT);
  TransparentStyles: array[Boolean] of DWORD = (0, TBSTYLE_TRANSPARENT);
begin
  FNewStyle := InitCommonControl(ICC_BAR_CLASSES);

  inherited CreateParams(Params);

  CreateSubClass(Params, TOOLBARCLASSNAME);
  with Params do
  begin
    Style := Style or DefaultStyles or FlatStyles[FFlat] or ListStyles[FList]
             or TransparentStyles[FTransparent];
    if ShowCaptions then
      Style := Style or TBSTYLE_TRANSPARENT;
    WindowClass.style := WindowClass.style and
                           not Cardinal(CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TToolBar.CreateWnd;
const
  TB_SETEXTENDEDSTYLE     = (WM_USER + 84);
  TB_GETEXTENDEDSTYLE     = (WM_USER + 85);
  TBSTYLE_EX_DRAWDDARROWS = $0001;
var
  DisplayDC: HDC;
  SaveFont, StockFont: HFONT;
  TxtMetric: TTextMetric;
begin
  inherited CreateWnd;

  Perform(TB_SETEXTENDEDSTYLE, 0, Perform(TB_GETEXTENDEDSTYLE, 0, 0) or
    TBSTYLE_EX_DRAWDDARROWS);
  FOldHandle := 0;
  StockFont := GetStockObject(SYSTEM_FONT);
  if StockFont <> 0 then
  begin
    DisplayDC := GetDC(0);
    if (DisplayDC <> 0) then
    begin
      SaveFont := SelectObject(DisplayDC, StockFont);
      if (GetTextMetrics(DisplayDC, TxtMetric)) then
        with TxtMetric do
          FHeightMargin := tmHeight - tmInternalLeading - tmExternalLeading + 1;
      SelectObject(DisplayDC, SaveFont);
      ReleaseDC(0, DisplayDC);
    end;
  end;
  RecreateButtons;
  Invalidate;
end;

procedure TToolBar.CreateButtons(NewWidth, NewHeight: Integer);

  function ToolButtonVisible: Boolean;
  var
    I: Integer;
    Control: TControl;
  begin
    for I := 0 to FButtons.Count - 1 do
    begin
      Control := TControl(FButtons[I]);
      if (Control is TToolButton)
      and ((csDesigning in ComponentState) or Control.Visible)
      and not (TToolButton(Control).Style in [tbsSeparator, tbsDivider]) then
      begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;

var
  ImageWidth, ImageHeight: Integer;
  I: Integer;
begin
  if not HandleAllocated then exit;
  BeginUpdate;
  try
    Perform(TB_BUTTONSTRUCTSIZE, SizeOf(TTBButton), 0);
    Perform(TB_SETINDENT, FIndent, 0);
    if FImages <> nil then
    begin
      ImageWidth := FImages.Width;
      ImageHeight := FImages.Height;
    end
    else
    if FDisabledImages <> nil then
    begin
      ImageWidth := FDisabledImages.Width;
      ImageHeight := FDisabledImages.Height;
    end
    else
    if FHotImages <> nil then
    begin
      ImageWidth := FHotImages.Width;
      ImageHeight := FHotImages.Height;
    end
    else
    begin
      ImageWidth := 0;
      ImageHeight := 0;
    end;
    Perform(TB_SETBITMAPSIZE, 0, MakeLParam(ImageWidth, ImageHeight));

    if ShowCaptions and ToolButtonVisible then Dec(NewHeight, FHeightMargin);

    if NewWidth <= 0 then NewWidth := 1;
    if NewHeight <= 0 then NewHeight := 1;
    Perform(TB_SETBUTTONSIZE, 0, MakeLParam(NewWidth, NewHeight));
    FButtonWidth := NewWidth;
    FButtonHeight := NewHeight;
  finally
    EndUpdate;
  end;
  Assert(False, Format('Trace:INTERNALBUTTONCOUNT = %d',[INTERNALBUTTONCOUNT]));
  for I := 0 to InternalButtonCount - 1 do
  begin
    Perform(TB_DELETEBUTTON, 0, 0);
  end;

  UpdateButtons;
  UpdateImages;
  GetButtonSize(FButtonWidth, FButtonHeight);
end;

procedure TToolBar.RepositionButton(Index: Integer);
var
  TBButton: TTBButton;
  Button: TControl;
  R: TRect;
  AdjustY: Integer;
begin
  if (csLoading in ComponentState) or
    (Perform(TB_GETBUTTON, Index, Longint(@TBButton)) = 0)
  then
    Exit;
  if Perform(TB_GETITEMRECT, Index, Longint(@R)) <> 0 then
  begin
    Button := TControl(TBButton.dwData);
    if Button is TToolButton then TToolButton(Button).BeginUpdate;
    try
      if not (Button is TToolButton) then
        with Button do
        begin
          if HandleAllocated and (Button is TWinControl) then
            HandleNeeded;

          BoundsRect := R;
          if Height < R.Bottom - R.Top then
          begin
            AdjustY := (R.Bottom - R.Top - Height) div 2;
            SetBounds(R.Left, R.Top + AdjustY, R.Right - R.Left, Height);
          end;
        end
      else
        Button.BoundsRect := R;
    finally
      if Button is TToolButton then TToolButton(Button).EndUpdate;
    end;
  end;
end;


procedure TToolBar.RepositionButtons(Index: Integer);
var
  I: Integer;
begin
  if (csLoading in ComponentState) or (FUpdateCount > 0) then Exit;
  BeginUpdate;
  try
    for I := InternalButtonCount - 1 downto Index do RepositionButton(I);
  finally
    EndUpdate;
  end;
end;

procedure TToolBar.GetButtonSize(var AWidth, AHeight: Integer);
var
  LastIndex: Integer;
begin
  if HandleAllocated then
  begin
    LastIndex := Perform(TB_GETBUTTONSIZE, 0, 0);
    AHeight := LastIndex shr 16;
    AWidth := LastIndex and $FFFF;
  end;
end;

procedure TToolBar.SetButtonHeight(Value: Integer);
begin
  if Value <> FButtonHeight then
  begin
    FButtonHeight := Value;
    RecreateButtons;
  end;
end;

procedure TToolBar.SetButtonWidth(Value: Integer);
begin
  if Value <> FButtonWidth then
  begin
    FButtonWidth := Value;
    RecreateButtons;
  end;
end;

procedure TToolBar.InsertButton(Control: TControl);
var
  FromIndex, ToIndex: Integer;
begin
  if Control is TToolButton then
     Begin
      TToolButton(Control).FToolBar := Self;
      TToolButton(Control).HandleNeeded;
     end;
  if not (csLoading in Control.ComponentState) then
  begin
    FromIndex := FButtons.IndexOf(Control);
    if FromIndex >= 0 then
      ToIndex := ReorderButton(Fromindex, Control.Left, Control.Top)
    else
    begin
      ToIndex := ButtonIndex(FromIndex, Control.Left, Control.Top);
      FButtons.Insert(ToIndex, Control);
      UpdateItem(TB_INSERTBUTTON, ToIndex, ToIndex);
    end;
  end
  else
  begin
    ToIndex := FButtons.Add(Control);
    if HandleAllocated and (Control is TToolButton) then
      TToolButton(Control).HandleNeeded;
    UpdateButton(ToIndex);
  end;
  if Wrapable then
    RepositionButtons(0)
  else
    RepositionButtons(ToIndex);
  RecreateButtons;
end;

procedure TToolBar.RemoveButton(Control: TControl);
var
  I : Integer;
  Pos: Integer;
begin
  I := FButtons.IndexOf(Control);
  if I >= 0 then
  begin
    if Control is TToolButton then TToolButton(Control).FToolBar := nil;
    Pos := FButtons.Remove(Control);
    Perform(TB_DELETEBUTTON, Pos, 0);
    ResizeButtons;
    if Wrapable then
      RepositionButtons(0)
    else
      RepositionButtons(Pos);
    RecreateButtons;
  end;
end;

function TToolBar.UpdateItem(Message, FromIndex, ToIndex: Integer): Boolean;
var
  Control: TControl;
  Button: TTBButton;
  CaptionText: string;
  Buffer: PChar;
begin
  Control := TControl(FButtons[FromIndex]);
  if Control is TToolButton then
    with TToolButton(Control) do
    begin
      FillChar(Button, SizeOf(Button), 0);
      if Style in [tbsSeparator, tbsDivider] then
      begin
        Button.iBitmap := Width;
        Button.idCommand := -1;
      end
      else
      begin
        if ImageIndex < 0 then
          Button.iBitmap := -2 else
          Button.iBitmap := ImageIndex;
        Button.idCommand := FromIndex;
      end;

      with Button do
      begin
        fsStyle := ButtonStyles[Style];
        if AutoSize then
          fsStyle := fsStyle or TBSTYLE_AUTOSIZE;
      end;
      Button.fsState := GetButtonState;
      if FGrouped then Button.fsStyle := Button.fsStyle or TBSTYLE_GROUP;
      Button.dwData := Longint(Control);
      if ShowCaptions then
      begin
        if Caption <> '' then
          CaptionText := Caption
        else
          { Common control requries at least a space is used when showing button
            captions.  If any one button's caption is empty (-1) then none of
            the buttons' captions will not be displayed. }
          CaptionText := ' ';
        { TB_ADDSTRING requires two null terminators }
        Buffer:=StrAlloc(length(CaptionText)+2);
        try
          StrPCopy(Buffer, CaptionText);
          Buffer[Length(CaptionText)+1] := #0;
          Button.iString := Self.Perform(TB_ADDSTRING, 0, Longint(@Buffer));
        finally
          StrDispose(Buffer);
        end;
      end
      else
        Button.iString := -1;
    end
  else
  begin
    FillChar(Button, SizeOf(Button), 0);
    Button.fsStyle := ButtonStyles[tbsSeparator];
    Button.iBitmap := Control.Width;
    Button.idCommand := -1;
    if not Control.Visible and not (csDesigning in Control.ComponentState) then
      Button.fsState := Button.fsState or ButtonStates[tbsHidden];
    Button.dwData := Longint(Control);
    Button.iString := -1;
  end;

  Result := Self.Perform(Message, ToIndex, Integer(@Button)) <> 0;
end;

function TToolBar.UpdateItem2(Message, FromIndex, ToIndex: Integer): Boolean;
var
  Control: TControl;
  Button: TTBButtonInfo;
  CaptionText: string;
  Buffer: PChar;
begin
  Control := TControl(FButtons[FromIndex]);
  FillChar(Button, SizeOf(Button), 0);
  Button.cbSize := SizeOf(Button);
  if Control is TToolButton then
    with TToolButton(Control) do
    begin
      Button.dwMask := TBIF_STATE or TBIF_STYLE or TBIF_LPARAM or TBIF_COMMAND
        or TBIF_SIZE;
      if Style in [tbsSeparator, tbsDivider] then
      begin
        Button.idCommand := -1;
      end
      else
      begin
        Button.dwMask := Button.dwMask or TBIF_IMAGE;
        if ImageIndex < 0 then
          Button.iImage := -2 else
          Button.iImage := ImageIndex;
        Button.idCommand := FromIndex;
      end;
      with Button do
      begin
        cx := Width;
        fsStyle := ButtonStyles[Style];
        if AutoSize then fsStyle := fsStyle or TBSTYLE_AUTOSIZE;
        if Grouped then Button.fsStyle := Button.fsStyle or TBSTYLE_GROUP;
      end;
      Button.fsState := GetButtonState;
      Button.lParam := Longint(Control);
      if ShowCaptions then
      begin
        if Caption <> '' then
          CaptionText := Caption
        else
          { Common control requries at least a space is used when showing button
            captions.  If any one button's caption is empty (-1) then none of
            the buttons' captions will not be displayed. }
          CaptionText := ' ';
        { TB_ADDSTRING requires two null terminators }
        Buffer:=StrAlloc(length(CaptionText)+2);
        try
          StrPCopy(Buffer, CaptionText);
          Buffer[Length(CaptionText) + 1] := #0;
          //Button.iString := Self.Perform(TB_ADDSTRING, 0, Longint(@Buffer));
        finally
          StrDispose(Buffer);
        end;
        Button.pszText := Buffer;
        Button.cchText := Length(CaptionText);
        Button.dwMask := Button.dwMask or TBIF_TEXT;
      end
      else
      begin
        Button.pszText := nil;
        Button.cchText := 0;
      end;

if Style in [tbsSeparator, tbsDivider] then
begin
  with Button do
  begin
    dwMask := TBIF_STYLE or TBIF_STATE or TBIF_LPARAM;
    fsState := TBSTATE_ENABLED or TBSTATE_WRAP;
    fsStyle := TBSTYLE_BUTTON;
  end;
end;

    end
  else
  begin
    Button.dwMask := TBIF_TEXT or TBIF_STATE or TBIF_STYLE or TBIF_LPARAM or
      TBIF_COMMAND or TBIF_SIZE;
    Button.fsStyle := ButtonStyles[tbsSeparator];
    Button.cx := Control.Width;
    Button.idCommand := -1;
    Button.lParam := Longint(Control);
    Button.pszText := nil;
    Button.cchText := 0;
  end;
  Result := Self.Perform(Message, ToIndex, Integer(@Button)) <> 0;
end;

function TToolBar.RefreshButton(Index: Integer): Boolean;
var
  Style: Longint;
begin
  Result:=false;
  if not (csLoading in ComponentState) and (FUpdateCount = 0)
  or (not HandleAllocated) then
    exit;

  BeginUpdate;
  try
    Style := GetWindowLong(Handle, GWL_STYLE);
    SetWindowLong(Handle, GWL_STYLE, Style and not WS_VISIBLE);
    try
      Result := (Index < InternalButtonCount) and
        UpdateItem(TB_DELETEBUTTON, Index, Index) and
        UpdateItem(TB_INSERTBUTTON, Index, Index);
    finally
      SetWindowLong(Handle, GWL_STYLE, Style);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TToolBar.UpdateButton(Index: Integer);
var
  Style: Longint;
begin
  if (csLoading in ComponentState) or (FUpdateCount > 0)
  or (not HandleAllocated)then Exit;

  BeginUpdate;
  try
    HandleNeeded;
    Style := GetWindowLong(Handle, GWL_STYLE);
    SetWindowLong(Handle, GWL_STYLE, Style and not WS_VISIBLE);
    try
      if Index < InternalButtonCount then
        UpdateItem2(TB_SETBUTTONINFO, Index, Index)
      else
        UpdateItem(TB_INSERTBUTTON, Index, Index);
    finally
      SetWindowLong(Handle, GWL_STYLE, Style);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TToolBar.UpdateButtons;
{const
  BlankButton: TTBButton = (iBitmap: 0; idCommand: 0; fsState: 0;
    fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0); dwData: 0; iString: 0);}
var
  I: Integer;
  Count: Integer;
  Style: Longint;
begin
Assert(False, 'Trace:IN TTOOLBAR.UPDATEBUTTONS');
  if not HandleAllocated then exit;

  BeginUpdate;
  try
    HandleNeeded;
    Style := GetWindowLong(Handle, GWL_STYLE);
    SetWindowLong(Handle, GWL_STYLE, Style and not WS_VISIBLE);
    try
      Count := InternalButtonCount;
      Assert(False, Format('Trace:COUNT = %d    --FBUTTONS.COUNT = %d',[COUNT,FButtons.Count]));
      for I := 0 to FButtons.Count - 1 do
      begin
        if I < Count then
          Begin
          Assert(False, Format('Trace:????CALLING UPDATEITEM2???????????   I , Count = %d,%d',[I,Count]));
          UpdateItem2(TB_SETBUTTONINFO, I, I)
          end
        else
          Begin
          Assert(False, Format('Trace:????INSERT BUTTON HERE???????????   I , Count = %d,%d',[I,Count]));
          UpdateItem(TB_INSERTBUTTON, I, I);
          End;
      end;
    finally
      SetWindowLong(Handle, GWL_STYLE, Style);
    end;
  finally
    EndUpdate;
  end;
  RepositionButtons(0);
end;

procedure TToolBar.UpdateButtonState(Index: Integer);
var
  TBButton: TTBButton;
begin
  if (Perform(TB_GETBUTTON, Index, Integer(@TBButton)) <> 0) then
    with TToolButton(TBButton.dwData) do
    begin
      SetButtonState(TBButton.fsState);
      Self.Perform(TB_SETSTATE, Index, MakeLong(GetButtonState, 0));
    end;
end;

procedure TToolBar.UpdateButtonStates;
var
  I: Integer;
begin
  for I := 0 to FButtons.Count - 1 do
    if TControl(FButtons[I]) is TToolButton then
      UpdateButtonState(I);
end;

procedure TToolBar.SetShowCaptions(Value: Boolean);
begin
  if FShowCaptions <> Value then
  begin
    FShowCaptions := Value;
    if not (csLoading in ComponentState) then
      RecreateWnd;
    AdjustSize;
  end;
end;

function TToolBar.GetButton(Index: Integer): TToolButton;
begin
  Result := TToolButton(FButtons[Index]);
end;

function TToolBar.GetButtonCount: Integer;
begin
  Result := FButtons.Count;
end;

function TToolBar.GetRowCount: Integer;
begin
  Result := Perform(TB_GETROWS, 0, 0);
end;

procedure TToolBar.SetList(Value: Boolean);
begin
  if FList <> Value then
  begin
    FList := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TToolBar.SetFlat(Value: Boolean);
begin
//GTK does not support flat/nof-flat yet but I add the code to stay compatable
  if FFlat <> Value then
  begin
    FFlat := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TToolBar.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TToolBar.SetWrapable(Value: Boolean);
begin
  if FWrapable <> Value then
  begin
    FWrapable := Value;
    if AutoSize then AdjustSize;
  end;
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

procedure TToolBar.LoadImages(AImages: TCustomImageList);
var
  AddBitmap: TTBAddBitmap;
  ReplaceBitmap: TTBReplaceBitmap;
  NewHandle: HBITMAP;

  function GetImageBitmap(ImageList: TCustomImageList): HBITMAP;
  var
    //I: Integer;
    Bitmap: TBitmap;
    R: TRect;
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := ImageList.Width * ImageList.Count;
      Bitmap.Height := ImageList.Height;
      R := Rect(0,0,Width,Height);
      with Bitmap.Canvas do
      begin
        Brush.Color := clBtnFace;
        FillRect(R);
      end;
{TODO: FINISH THIS by adding this function. IMAGELIST_DRAW
      for I := 0 to ImageList.Count - 1 do
        ImageList_Draw(ImageList.Handle, I, Bitmap.Canvas.Handle,I * ImageList.Width, 0, ILD_TRANSPARENT);
}
      Result := Bitmap.ReleaseHandle;
    finally
      Bitmap.Free;
    end;
  end;

begin
  if AImages <> nil then
    NewHandle := GetImageBitmap(AImages)
  else
    with TBitmap.Create do
    try
      Assign(FNullBitmap);
      NewHandle := ReleaseHandle;
    finally
      Free;
    end;
  if FOldHandle = 0 then
  begin
    AddBitmap.hInst := 0;
    AddBitmap.nID := NewHandle;
    Perform(TB_ADDBITMAP, ButtonCount, Longint(@AddBitmap));
  end
  else
  begin
    with ReplaceBitmap do
    begin
      hInstOld := 0;
      nIDOld := FOldHandle;
      hInstNew := 0;
      nIDNew := NewHandle;
      nButtons := ButtonCount;
    end;
    Perform(TB_REPLACEBITMAP, 0, Longint(@ReplaceBitmap));
    if FOldHandle <> 0 then DeleteObject(FOldHandle);
  end;
  FOldHandle := NewHandle;
end;

procedure TToolBar.UpdateImages;
begin
  if FNewStyle then
  begin
    if FImages <> nil then SetImageList(FImages.Handle);
    if FDisabledImages <> nil then SetDisabledImageList(FDisabledImages.Handle);
    if FHotImages <> nil then SetHotImageList(FHotImages.Handle);
  end
  else
    if HandleAllocated then LoadImages(FImages);
end;

procedure TToolBar.ImageListChange(Sender: TObject);
begin
  if HandleAllocated and (Sender = Images) then RecreateButtons;
end;

procedure TToolBar.SetImageList(Value: HImageList);
begin
  if HandleAllocated then Perform(TB_SETIMAGELIST, 0, Value);
  Invalidate;
end;

procedure TToolBar.SetImages(Value: TCustomImageList);
begin
//  if FImages <> nil then FImages.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then
  begin
//    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end
  else
    SetImageList(0);
  RecreateButtons;
end;

procedure TToolBar.DisabledImageListChange(Sender: TObject);
begin
  if HandleAllocated and (Sender = DisabledImages) then RecreateButtons;
end;

procedure TToolBar.SetDisabledImageList(Value: HImageList);
begin
  if HandleAllocated then Perform(TB_SETDISABLEDIMAGELIST, 0, Value);
  Invalidate;
end;

procedure TToolBar.SetDisabledImages(Value: TCustomImageList);
begin
//  if FDisabledImages <> nil then FDisabledImages.UnRegisterChanges(FDisabledImageChangeLink);
  FDisabledImages := Value;
  if FDisabledImages <> nil then
  begin
//    FDisabledImages.RegisterChanges(FDisabledImageChangeLink);
    FDisabledImages.FreeNotification(Self);
  end
  else
    SetDisabledImageList(0);
  RecreateButtons;
end;

procedure TToolBar.HotImageListChange(Sender: TObject);
begin
  if HandleAllocated and (Sender = HotImages) then RecreateButtons;
end;

procedure TToolBar.SetHotImageList(Value: HImageList);
begin
  if HandleAllocated then Perform(TB_SETHOTIMAGELIST, 0, Value);
  Invalidate;
end;

procedure TToolBar.SetHotImages(Value: TCustomImageList);
begin
//  if FHotImages <> nil then FHotImages.UnRegisterChanges(FHotImageChangeLink);
  FHotImages := Value;
  if FHotImages <> nil then
  begin
//    FHotImages.RegisterChanges(FHotImageChangeLink);
    FHotImages.FreeNotification(Self);
  end
  else
    SetHotImageList(0);
  RecreateButtons;
end;

procedure TToolBar.SetIndent(Value: Integer);
begin
  if FIndent <> Value then
  begin
    FIndent := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TToolBar.RecreateButtons;
begin
  if not (csLoading in ComponentState) and HandleAllocated then
  begin
    CreateButtons(FButtonWidth, FButtonHeight);
    ResizeButtons;
  end;
end;

procedure TToolBar.WMKeyDown(var Message: TLMKeyDown);
var
  Item: Integer;
  Button: TToolButton;
  //P: TPoint;
begin
  if FInMenuLoop then
  begin
    Item := Perform(TB_GETHOTITEM, 0, 0);
    case Message.CharCode of
      VK_RETURN, VK_DOWN:
        begin
          if (Item > -1) and (Item < FButtons.Count) then
          begin
            Button := TToolButton(FButtons[Item]);
            Button.ClientToScreen(Point(1, 1));
            ClickButton(Button);
          end;
          { Prevent default processing }
          if Message.CharCode = VK_DOWN then Exit;
        end;
      VK_ESCAPE: CancelMenu;
    end;
  end;
  inherited WMKeyDown(Message);
end;

procedure TToolBar.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Control: TControl;
begin
  for I := 0 to FButtons.Count - 1 do Proc(TComponent(FButtons[I]));
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if (Control.Owner = Root) and (FButtons.IndexOf(Control) = -1) then Proc(Control);
  end;
end;

procedure TToolBar.Loaded;
var
  I: Integer;
begin
  RecreateButtons;
  { Make sure we dock controls after streaming }
  for I := 0 to ControlCount - 1 do
    Controls[I].HostDockSite := Self;
  inherited Loaded;
  ResizeButtons;
  RepositionButtons(0);
end;

procedure TToolBar.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TToolBar.EndUpdate;
begin
  Dec(FUpdateCount);
end;

procedure TToolBar.ResizeButtons;
begin
  if not (csLoading in ComponentState) and HandleAllocated then
  begin
    Perform(TB_AUTOSIZE, 0, 0);
    if AutoSize then AdjustSize;
  end;
end;

function TToolBar.InternalButtonCount: Integer;
begin
//  Result := Perform(TB_BUTTONCOUNT, 0, 0);
  if HandleAllocated then
    Result := CNSendMessage(LM_TB_BUTTONCOUNT,Self,Nil);
end;

function TToolBar.ButtonIndex(OldIndex, ALeft, ATop: Integer): Integer;
var
  Dist, Tmp, Head, Tail: Integer;
  Control: TControl;
  TEmpIndex : Integer;
begin
  if (OldIndex >= 0) and (FButtons.Count <= 1) then
  begin
    Result := OldIndex;
    Exit;
  end;
  { Find row closest to ATop }
  Result := 0;
  if FButtons.Count = 0 then Exit;
  Tmp := 0;
  Head := 0;
  Tail := 0;
  Dist := MaxInt;
  while (Dist > 0) and (Result < FButtons.Count) do
  begin
    if Result <> OldIndex then
    begin
      Control := TControl(FButtons[Result]);
      if (Control is TToolButton) and TToolButton(Control).Wrap or
        (Result = FButtons.Count - 1) then
      begin
        if Abs(ATop - Control.Top) < Dist then
        begin
          Dist := Abs(ATop - Control.Top);
          Head := Tmp;
          Tail := Result;
        end;
        Tmp := Result + 1;
      end;
    end
    else
      Tail := Result;
    Inc(Result);
  end;
  { Find button on Row closest to ALeft }
  if Result=FButtons.Count then dec(Result);
  if FButtons[Result] <> nil then
    for TempIndex := Head to Tail do
      if (TempIndex <> OldIndex) then
        if (ALeft <= TControl(FButtons[Result]).Left) then
          Break;
  { Return old position if new position is last on the row and old position
    was already the last on the row. }
  if (TempIndex = OldIndex + 1) and (OldIndex in [Head..Tail]) then
    Result := OldIndex
    else
    Result := TempIndex;
end;

function TToolBar.ReorderButton(OldIndex, ALeft, ATop: Integer): Integer;
var
  Control: TControl;
begin
  Result := ButtonIndex(OldIndex, ALeft, ATop);
  if Result <> OldIndex then
  begin
    { If we are inserting to the right of our deletion then account for shift }
    if OldIndex < Result then Dec(Result);
    Control := TControl(FButtons[OldIndex]);
    FButtons.Delete(OldIndex);
    FButtons.Insert(Result, Control);
    BeginUpdate;
    try
      Perform(TB_DELETEBUTTON, OldIndex, 0);
      UpdateItem(TB_INSERTBUTTON, Result, Result);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TToolBar.AdjustControl(Control: TControl);
var
  I, Pos: Integer;
  R: TRect;
  Reordered, NeedsUpdate: Boolean;
begin
  Pos := FButtons.IndexOf(Control);
  if Pos = -1 then Exit;
  Reordered := ReorderButton(Pos, Control.Left, Control.Top) <> Pos;
  NeedsUpdate := False;
  if Reordered then
  begin
    I := FButtons.IndexOf(Control);
    if I < Pos then Pos := I;
  end
  else if Perform(TB_GETITEMRECT, Pos, Longint(@R)) <> 0 then
  begin
    NeedsUpdate := Control.Width <> R.Right - R.Left;
    Reordered := NeedsUpdate;
  end;
  if (csDesigning in ComponentState) and (Control.Height <> ButtonHeight) then
    ButtonHeight := Control.Height
  else
    if Reordered then
    begin
      if NeedsUpdate then
        RefreshButton(Pos);
      ResizeButtons;
      RepositionButtons(0);
    end
    else
      RepositionButton(Pos);
end;

procedure TToolBar.AlignControls(AControl: TControl; var Rect: TRect);
begin
  if FUpdateCount > 0 then Exit;
  if AControl = nil then
    RepositionButtons(0)
  else if not (AControl is TToolButton) then
    AdjustControl(AControl);
end;

procedure TToolBar.ChangeScale(M, D: Integer);
begin
end;

procedure TToolBar.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  if not Transparent then
    inherited WMEraseBkgnd(Message)
    else
    DefaultHandler(Message);
end;

procedure TToolBar.WMGetDlgCode(var Message: TLMessage);
begin
  if FInMenuLoop then
    Message.Result := DLGC_WANTARROWS;
end;


procedure TToolBar.WMGetText(var Message: TLMGetText);
begin
//This should NOT be needed in LCL
end;

procedure TToolBar.WMGetTextLength(var Message: TLMGetTextLength);
begin
  Message.Result := Length(FCaption);
end;

procedure TToolBar.WMSetText(var Message: TLMSetText);
begin
  with Message do
    SetString(FCaption, Text, StrLen(pchar(Text)));
end;

procedure TToolBar.WMNotifyFormat(var Message: TLMessage);
begin
//TODO: WMNOTIFYFORMAT
end;

procedure TToolBar.WMSize(var Message: TLMSize);
var
  W, H: Integer;
begin
  inherited WMSize(Message);
  if not AutoSize then
  begin
    W := Width;
    H := Height;
    WrapButtons(W, H);
  end;
end;

procedure TToolBar.WMSysChar(var Message: TLMSysChar);
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then
  begin
    Form.Dispatch(Message);
    Exit;
  end;
end;

procedure TToolBar.WMWindowPosChanged(var Message: TLMWindowPosChanged);
begin
Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
Assert(False, 'Trace:!!!!!!!!!TODO: FINISH WMWindowPosChanged in toolbar.inc!!!!!!!!!!!!!!!!');
Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');

end;

procedure TToolBar.WMWindowPosChanging(var Message: TLMWindowPosChanging);
begin
Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
Assert(False, 'Trace:!!!!!!!!!TODO: FINISH WMWindowPosChanging in toolbar.inc!!!!!!!!!!!!!!!!');
Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
end;

function TToolBar.WrapButtons(var NewWidth, NewHeight: Integer): Boolean;
Begin
//TODO: TToolbar.WrapButtons
Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
Assert(False, 'Trace:!!!!!!!!!TODO: FINISH Wrapbuttons in toolbar.inc!!!!!!!!!!!!!!!!');
Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
result := true;
end;

function TToolBar.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := WrapButtons(NewWidth, NewHeight);
end;

procedure TToolBar.CMControlChange(var Message: TCMControlChange);
begin
Assert(False, 'Trace:TOOLBAR recieved a CMCONTROLCHANGED event');
HandleNeeded;
  with Message do
    if Inserting then
      InsertButton(Control)
    else
      RemoveButton(Control);
end;

procedure TToolBar.CNChar(var Message: TLMChar);
begin

  if FInMenuLoop and not (csDesigning in ComponentState) then
    with Message do
      if Perform(CM_DIALOGCHAR, CharCode, KeyData) <> 0 then
        Result := 1;
end;

procedure TToolBar.CMDialogChar(var Message: TCMDialogChar);
var
  Button: TToolButton;
begin
  if Enabled and Showing and ShowCaptions then
  begin
    Button := FindButtonFromAccel(Message.CharCode);
    if Button <> nil then
    begin
      { Display a drop-down menu after hitting the accelerator key if IE3
        is installed. Otherwise, fire the OnClick event for IE4. We do this
        because the IE4 version of the drop-down metaphor is more complete,
        allowing the user to click a button OR drop-down its menu. }
      if (Button.Style <> tbsDropDown) and
        ((Button.DropdownMenu <> nil) or (Button.MenuItem <> nil)) then
        TrackMenu(Button)
      else
        Button.Click;
      Message.Result := 1;
      Exit;
    end;
  end;
end;

procedure TToolBar.CMEnabledChanged(var Message: TLMessage);
begin
  Broadcast(Message);
end;

procedure TToolBar.CMColorChanged(var Message: TLMessage);
begin
  RecreateWnd;
end;

procedure TToolBar.CMParentColorChanged(var Message: TLMessage);
begin
  if Transparent then Invalidate;
end;

procedure TToolBar.CNSysKeyDown(var Message: TLMSysKeyDown);
begin
  if (Message.CharCode = VK_MENU) then
    CancelMenu;
end;

procedure TToolBar.CMSysFontChanged(var Message: TLMessage);
begin
  RecreateWnd;
end;

procedure TToolBar.CNDropDownClosed(var Message: TLMessage);
begin
  ClearTempMenu;
  FCaptureChangeCancels := True;
end;

procedure TToolBar.CNNotify(var Message: TLMNotify);
var
  Button: TToolButton;
begin
  with Message do
    case NMHdr^.code of
      TBN_DROPDOWN:
        with PNMToolBar(NMHdr)^ do
          { We can safely assume that a TBN_DROPDOWN message was generated by a
            TToolButton and not any TControl. }
          if Perform(TB_GETBUTTON, iItem, Longint(@tbButton)) <> 0 then
          begin
            Button := TToolButton(tbButton.dwData);
            if Button <> nil then
              Button.CheckMenuDropDown;
          end;
    end;
end;

procedure TToolBar.WndProc(var Message: TLMessage);
Begin
//TODO: TToolbar.WndProc

case Message.msg of
  TB_INSERTBUTTON : Assert(False, 'Trace:!!!!!!!!!!!!!!!!!TB_INSERTBUTTON!!!!!!!!!!!!!!');
  LM_NOTIFYFORMAT : Assert(False, 'Trace:!!!!!!!!!!!!!!!!!LM_NOTIFYFORMAT!!!!!!!!!!!!!!');
  LM_EXPOSEEVENT : Assert(False, 'Trace:!!!!!!!!!!!!!!!!!ExposeEvent!!!!!!!!!!!!!!');
  LM_SETSELTEXT : Assert(False, 'Trace:!!!!!!!!!!!!!!!!!LM_SETSELTEXT!!!!!!!!!!!!!!');
  LM_PAINT : Assert(False, 'Trace:!!!!!!!!!!!!!!!!!LM_PAINT!!!!!!!!!!!!!!');
  LM_CONFIGUREEVENT : Assert(False, 'Trace:!!!!!!!!!!!!!!!!!LM_CONFIGUREEVENT!!!!!!!!!!!!!!');
  LM_KILLWORD : Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!LM_KILLWORD!!!!!!!!!!!!!!!!!!!!');
  LM_SETSELMODE : Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!LM_SETSELMODE!!!!!!!!!!!!!!!!');
  LM_SHOWMODAL : Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!LM_SHOWMODAL!!!!!!!!!!!!!!!!!!!');
   end;
  case Message.msg of
    TB_INSERTBUTTON :
      if HandleAllocated then
        CNSendMessage(LM_INSERTTOOLBUTTON,TControl(TTBButton(Pointer(Message.lParam)^).dwData),nil);

    TB_DELETEBUTTON :
      if HandleAllocated then
        CNSendMessage(LM_DELETETOOLBUTTON,TControl(FButtons[Message.wparam]),nil);
    end;


  inherited WndProc(Message);
end;

function TToolBar.FindButtonFromAccel(Accel: Word): TToolButton;
var
  I: Integer;
begin
  for I := 0 to FButtons.Count - 1 do
    if TControl(FButtons[I]) is TToolButton then
    begin
      Result := TToolButton(FButtons[I]);
      if Result.Visible and Result.Enabled and IsAccel(Accel, Result.Caption) then
        Exit;
    end;
  Result := nil;
end;


function ToolMenuGetMsgHook(Code: Integer; WParam: Longint;
  var Msg: TMsg): Longint; stdcall;
Begin
  // ToDo
  Result:=0;
end;

procedure InitToolMenuHooks;
begin
end;

procedure ReleaseToolMenuHooks;
begin
end;


function ToolMenuKeyMsgHook(Code: Integer; WParam: Longint;
  var Msg: TMsg): Longint; stdcall;
begin
  // ToDo
  Result:=0;
end;

procedure InitToolMenuKeyHooks;
begin
end;

procedure ReleaseToolMenuKeyHooks;
begin
end;

procedure TToolBar.ClearTempMenu;
var
  I: Integer;
  Item: TMenuItem;
begin
  if (FButtonMenu <> nil) and (FMenuButton <> nil) and
    (FMenuButton.MenuItem <> nil) and (FTempMenu <> nil) then
  begin
    for I := FTempMenu.Items.Count - 1 downto 0 do
    begin
      Item := FTempMenu.Items[I];
      FTempMenu.Items.Delete(I);
      FButtonMenu.Insert(0, Item);
    end;
    FTempMenu.Free;
    FTempMenu := nil;
    FMenuButton := nil;
    FButtonMenu := nil;
  end;
end;

function TToolBar.CheckMenuDropdown(Button: TToolButton): Boolean;
var
  Hook: Boolean;
  //Menu: TMenu;
  Item: TMenuItem;
  I: Integer;
  //ParentMenu: TMenu;
  APoint: TPoint;
begin
  Result := False;
  if Button = nil then Exit;
  FCaptureChangeCancels := False;
  try
    if Button.DropdownMenu <> nil then
      FTempMenu := Button.DropdownMenu
    else if Button.MenuItem <> nil then
    begin
//TODO: FINISH MENUITEM.CLICK
//      Button.MenuItem.Click;
      ClearTempMenu;
      FTempMenu := TPopupMenu.Create(Self);
//TODO: FINISH Menu BiDiMode and HelpContext and Images  12/21/99
{
      ParentMenu := Button.MenuItem.GetParentMenu;
      if ParentMenu <> nil then
        FTempMenu.BiDiMode := ParentMenu.BiDiMode;

     FTempMenu.HelpContext := Button.MenuItem.HelpContext;
      FTempMenu.TrackButton := tbLeftButton;
      Menu := Button.MenuItem.GetParentMenu;
      if Menu <> nil then
        FTempMenu.Images := Menu.Images;
 }
     FButtonMenu := Button.MenuItem;
      for I := FButtonMenu.Count - 1 downto 0 do
      begin
        Item := FButtonMenu.Items[I];
        FButtonMenu.Delete(I);
        FTempMenu.Items.Insert(0, Item);
      end;
    end
    else
      Exit;
//TODO: Finish SENDCANCELMODE
//    SendCancelMode(nil);
//TODO: Add PopupComponent to Menu
//    FTempMenu.PopupComponent := Self;
    Hook := Button.Grouped or (Button.MenuItem <> nil);
    if Hook then
    begin
      //MenuButtonIndex := Button.Index;
      //MenuToolBar := Self;
      InitToolMenuHooks;
    end;
    Perform(TB_SETHOTITEM, -1, 0);
    try
      APoint := Button.ClientToScreen(Point(0, Button.ClientHeight));
      if FTempMenu.IsRightToLeft then Inc(APoint.X, Button.Width);
//TODO: finish Menu.popup
//      FTempMenu.Popup(APoint.X, APoint.Y);
    finally
      if Hook then ReleaseToolMenuHooks;
    end;
    FMenuButton := Button;
    if StillModal then
      Perform(TB_SETHOTITEM, Button.Index, 0);
    Result := True;
  finally
//TODO: Add POSTMESSAGE
//    PostMessage(Handle, CN_DROPDOWNCLOSED, 0, 0);
  end;
end;

procedure TToolBar.WMSysCommand(var Message: TLMSysCommand);
var
  Button: TToolButton;
begin
  { Enter menu loop if only the Alt key is pressed -- ignore Alt-Space and let
    the default processing show the system menu. }
  if not FInMenuLoop and Enabled and Showing and ShowCaptions then
    with Message do
      if (CmdType and $FFF0 = SC_KEYMENU) and (Key <> VK_SPACE) and
        (Key <> Word(ord('-'))) and (GetCapture = 0) then
      begin
        if Key = 0 then
          Button := nil else
          Button := FindButtonFromAccel(Key);
        if (Key = 0) or (Button <> nil) then
        begin
          TrackMenu(Button);
          Result := 1;
          Exit;
        end;
      end;
end;

procedure TToolBar.ClickButton(Button: TToolButton);
//var
//  P: TPoint;
begin
  FCaptureChangeCancels := False;
  {P := }Button.ClientToScreen(Point(0, 0));
//TODO: Add POSTMESSAGE
//  PostMessage(Handle, LM_LBUTTONDOWN, MK_LBUTTON,
//    Longint(PointToSmallPoint(ScreenToClient(P))));
end;

procedure TToolBar.InitMenu(Button: TToolButton);
begin
  //MenuToolBar2 := Self;
  MouseCapture := True;
  InitToolMenuKeyHooks;
  if Button <> nil then
  begin
    Perform(TB_SETHOTITEM, Button.Index, 0);
    ClickButton(Button);
  end
  else
    Perform(TB_SETHOTITEM, 0, 0);
  if Button = nil then
    FCaptureChangeCancels := True;
end;

procedure TToolBar.CancelMenu;
begin
  if FInMenuLoop then
  begin
    ReleaseToolMenuKeyHooks;
    MouseCapture := False;
  end;
  FInMenuLoop := False;
  FCaptureChangeCancels := False;
  Perform(TB_SETHOTITEM, -1, 0);
end;

function TToolBar.TrackMenu(Button: TToolButton): Boolean;
begin
  if FInMenuLoop then
  begin
    if Button <> nil then
    begin
      ClickButton(Button);
      Result := True;
    end
    else
      Result := False;
    Exit;
  end;

  InitMenu(Button);
  try
    FInMenuLoop := True;
{    repeat
      if Application.Terminated then
        FInMenuLoop := False;
    until not FInMenuLoop;
}
  finally
    CancelMenu;
  end;
  Result := FMenuResult;
end;


{ =============================================================================

  $Log$
  Revision 1.9  2002/12/22 22:55:45  mattias
  fixed toolbar

  Revision 1.8  2002/11/12 10:16:17  lazarus
  MG: fixed TMainMenu creation

  Revision 1.7  2002/10/30 13:20:10  lazarus
  MG: fixed example

  Revision 1.6  2002/05/10 06:05:55  lazarus
  MG: changed license to LGPL

  Revision 1.5  2001/09/30 08:34:50  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.4  2001/06/14 23:13:30  lazarus
  MWE:
    * Fixed some syntax errors for the latest 1.0.5 compiler

  Revision 1.3  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.2  2001/03/12 12:17:01  lazarus
  MG: fixed random function results

  Revision 1.1  2000/07/13 10:28:28  michael
  + Initial import

  Revision 1.3  2000/05/30 22:28:41  lazarus
  MWE:
    Applied patches from Vincent Snijders:
    + Added GetWindowRect
    * Fixed horz label alignment
    + Added vert label alignment

  Revision 1.2  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:57  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.9  2000/03/30 18:07:55  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.8  1999/12/30 19:49:07  lazarus
  *** empty log message ***

  Revision 1.6  1999/12/29 20:38:23  lazarus
  Modified the toolbar so it now displays itself.  However, I can only add one button at this point.  I will fix that soon....

  Shane

  Revision 1.5  1999/12/23 19:50:54  lazarus
  Working on the toolbar again.  Haven't been able to get it to display at all yet.

  gtkobject.inc - removed IntCNSendMessage and IntCNSendMessage2
  WinControl.inc - addded code to InsertControl so when a control is added to a parent's control list, a CMCONTROLCHANGED message is sent.  This way the parent can react to the addition.

  Shane

  Revision 1.1  1999/12/22 14:33:36  lazarus
  Initial addition of a few new files.
  Shane

  Revision 1.1  1999/12/08 16:22:19  lazarus
  Templates initially created      SM

}

