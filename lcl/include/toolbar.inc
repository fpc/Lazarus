{%MainUnit ../comctrls.pp}

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
{const
  TBSTYLE_TRANSPARENT = $8000;  // IE4 style -this is not valid but still needed as a holder
  DefaultStyles =
    CCS_NOPARENTALIGN or CCS_NOMOVEY or CCS_NORESIZE or CCS_NODIVIDER;
  ListStyles: array[Boolean] of DWORD = (0, TBSTYLE_LIST);
  FlatStyles: array[Boolean] of DWORD = (0, TBSTYLE_FLAT);
  TransparentStyles: array[Boolean] of DWORD = (0, TBSTYLE_TRANSPARENT);}
begin
  //FNewStyle := InitCommonControl(ICC_BAR_CLASSES);

  inherited CreateParams(Params);

  {CreateSubClass(Params, TOOLBARCLASSNAME);
  with Params do
  begin
    Style := Style or DefaultStyles or FlatStyles[FFlat] or ListStyles[FList]
             or TransparentStyles[FTransparent];
    if ShowCaptions then
      Style := Style or TBSTYLE_TRANSPARENT;
    WindowClass.style := WindowClass.style and
                           not Cardinal(CS_HREDRAW or CS_VREDRAW);
  end;}
end;

procedure TToolBar.CreateWnd;
{const
  TB_SETEXTENDEDSTYLE     = (WM_USER + 84);
  TB_GETEXTENDEDSTYLE     = (WM_USER + 85);
  TBSTYLE_EX_DRAWDDARROWS = $0001;
var
  DisplayDC: HDC;
  SaveFont, StockFont: HFONT;
  TxtMetric: TTextMetric;}
begin
  BeginUpdate;
  inherited CreateWnd;

  {Perform(TB_SETEXTENDEDSTYLE, 0, LParam(Perform(TB_GETEXTENDEDSTYLE, 0, 0) or
    TBSTYLE_EX_DRAWDDARROWS));
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
  Invalidate;}
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
var
  I: Integer;
begin
  // dock controls after streaming
  // TODO: activate when docking finished
  //for I := 0 to ControlCount - 1 do
  //  Controls[I].HostDockSite := Self;
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
      if CurControl.Align<>alNone then continue;
      NewWidth:=CurControl.Width;
      NewHeight:=FButtonHeight;
      if (CurControl is TToolButton) then begin
        CurButton:=TToolButton(CurControl);
        case CurButton.Style of
        tbsButton,tbsCheck,tbsDropDown:
          NewWidth:=FButtonWidth;
        end;
      end;
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
  begin
    if (CurControl is TToolButton)
    and (TToolButton(CurControl).Style in [tbsButton,tbsDropDown,tbsCheck])
    then
      NewControlWidth:=ButtonWidth
    else
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
  BeginUpdate;
  NewWidth:=0;
  NewHeight:=0;
  AlignedControls:=TList.Create;
  OrderedControls:=TList.Create;
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
    NewControlWidth:=ButtonWidth;
    i:=0;
    while i<OrderedControls.Count do begin
      CurControl:=TControl(OrderedControls[i]);
      if CurControl.Align=alNone then begin
        CalculatePosition;
        //DebugLn('WrapButtons ',CurControl.Name,':',CurControl.ClassName,' ',x,',',y,',',CurControl.Width,',',CurControl.Height);
        CurControl.SetBounds(x,y,NewControlWidth,ButtonHeight);
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

{ =============================================================================

  $Log$
  Revision 1.35  2005/01/03 01:07:08  mattias
  fixed registering TProgressBar, disabled docking in TToolBar, return key for codeexplorer, updated finnish translation

  Revision 1.34  2004/11/28 00:55:44  mattias
  deactivated sending SYSKey messages in gtk intf - they are not used anyway

  Revision 1.33  2004/11/04 00:52:23  marc
  * Changed typing fixes to casting

  Revision 1.32  2004/11/03 22:13:48  marc
  * Fixed fpc stronger typing

  Revision 1.31  2004/09/16 14:32:31  micha
  convert LM_SETSELMODE message to interface method

  Revision 1.30  2004/09/14 14:41:17  micha
  convert LM_INSERTTOOLBUTTON and LM_DELETETOOLBUTTON messages to interface methods; warning: still very ugly code, as if it is "OldToolbar" so probably, obsolete

  Revision 1.29  2004/09/13 14:34:53  micha
  convert LM_TB_BUTTONCOUNT to interface method

  Revision 1.28  2004/07/23 16:44:27  mattias
  activated new TToolbar, old can be activated with -dOldToolBar

  Revision 1.27  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.26  2004/05/10 07:01:51  micha
  fix mainunit typo

  Revision 1.25  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.24  2004/02/23 18:24:38  mattias
  completed new TToolBar

  Revision 1.23  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.21  2004/02/22 15:39:43  mattias
  fixed error handling on saving lpi file

  Revision 1.20  2004/02/22 10:43:20  mattias
  added child-parent checks

  Revision 1.19  2004/02/21 15:37:33  mattias
  moved compiler options to project menu, added -CX for smartlinking

  Revision 1.18  2004/02/12 18:09:10  mattias
  removed win32 specific TToolBar code in new TToolBar, implemented TWinControl.FlipChildren

  Revision 1.17  2004/02/11 11:34:15  mattias
  started new TToolBar

  Revision 1.16  2004/02/04 14:00:45  mattias
  quick fixed removing TToolButton

  Revision 1.15  2004/01/20 21:40:51  marc
  * forgot a line

  Revision 1.14  2004/01/20 21:39:30  marc
  * applied patch from Darek Mazur

  Revision 1.13  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.12  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.11  2003/06/23 09:42:09  mattias
  fixes for debugging lazarus

  Revision 1.10  2002/12/29 11:10:45  mattias
  fixed form FActive, cleanups

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

