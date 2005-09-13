{%MainUnit win32int.pp}

{
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
{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{*************************************************************}
{            callback routines                                }
{*************************************************************}

procedure PrepareSynchronize;
begin
  TWin32WidgetSet(WidgetSet).HandleWakeMainThread(nil);
end;

{ addition XP messages }

const
  WM_THEMECHANGED = $31A;

{-----------------------------------------------------------------------------
  Function: PropEnumProc
  Params: Window - The window with the property
          Str    - The property name
          Data   - The property value
  Returns: Whether the enumeration should continue

  Enumerates and removes properties for the target window
 -----------------------------------------------------------------------------}
Function PropEnumProc(Window: Hwnd; Str: PChar; Data: Handle): LongBool; StdCall;
Begin
  Result:=false;
  if (DWORD(Str) and DWORD($FFFF0000)) = 0 then Exit; // global atom handle
  Assert(False, 'Trace:PropEnumProc - Start');
  Assert(False, Format('Trace:PropEnumProc - Property %S (with value 0x%X) from window 0x%X removed',
    [String(Str), Data, Window]));
  RemoveProp(Window, Str);
  Result := True;
  Assert(False, 'Trace:PropEnumProc - Exit');
End;

{------------------------------------------------------------------------------
 Function: CallDefaultWindowProc
 Params: Window - The window that receives a message
         Msg    - The message received
         WParam - Word parameter
         LParam - Long-integer parameter
 Returns: 0 if Msg is handled; non-zero long-integer result otherwise

 Passes message on to 'default' handler. This can be a control specific window
 procedure or the default window procedure.
 ------------------------------------------------------------------------------}
function CallDefaultWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
  LParam: Windows.LParam): LResult;
var
  PrevWndProc: Windows.WNDPROC;
begin
  PrevWndProc := GetWindowInfo(Window)^.DefWndProc;
  if PrevWndProc = nil then
    Result := Windows.DefWindowProc(Window, Msg, WParam, LParam)
  else
    Result := Windows.CallWindowProc(PrevWndProc, Window, Msg, WParam, LParam);
end;

type
  TEraseBkgndCommand = (ecDefault, ecNoMsg);
const
  EraseBkgndStackMask = $3;
  EraseBkgndStackShift = 2;
var
  EraseBkgndStack: dword;
{$ifdef MSG_DEBUG}
  MessageStackDepth: string;
{$endif}

procedure PushEraseBkgndCommand(Command: TEraseBkgndCommand);
begin
  EraseBkgndStack := (EraseBkgndStack shl EraseBkgndStackShift) or dword(Ord(Command));
end;

function CheckMouseMovement: boolean;
  // returns true if mouse did not move between lmousebutton down
var
  lCursorPos: TPoint;
  moveX, moveY: integer;
begin
  GetCursorPos(lCursorPos);
  moveX := lCursorPos.X - MouseDownPos.X;
  moveY := lCursorPos.Y - MouseDownPos.Y;
  Result := (-3 <= moveX) and (moveX <= 3) and (-3 <= moveY) and (moveY <= 3);
end;

//TODO: added temporarily to fix compilation,
//should probably removed soon, as the LCL does not listen to it.
type
  PLMInsertText = ^TLMInsertText;
  TLMInsertText = record
    Msg : Cardinal;
    NewText : String;
    Length : Integer;
    Position : Integer;
    UserData : Pointer;
  end;

{------------------------------------------------------------------------------
 Function: WindowProc
 Params: Window - The window that receives a message
         Msg    - The message received
         WParam - Word parameter
         LParam - Long-integer parameter
  Returns: 0 if Msg is handled; non-zero long-integer result otherwise

  Handles the messages sent to the specified window, in parameter Window, by
  Windows or other applications
 ------------------------------------------------------------------------------}
Function
{$ifdef MSG_DEBUG}
  RealWindowProc
{$else}
  WindowProc
{$endif}
  (Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
Var
  LMessage: TLMessage;
  PLMsg: PLMessage;
  R: TRect;
  P: TPoint;
  NewLeft, NewTop, NewWidth, NewHeight: integer;
  lWinControl, ChildWinControl: TWinControl;
  TargetObject: TObject;
  WinProcess: Boolean;
  NotifyUserInput: Boolean;
  OverlayWindow: HWND;
  TargetWindow: HWND;
  eraseBkgndCommand: TEraseBkgndCommand;
  winClassName: array[0..19] of char;
  WindowInfo: PWindowInfo;
  Flags: dword;

  LMInsertText: TLMInsertText; // used by CB_INSERTSTRING, LB_INSERTSTRING
  LMScroll: TLMScroll; // used by WM_HSCROLL
  LMKey: TLMKey; // used by WM_KEYDOWN WM_KEYUP
  LMChar: TLMChar; // used by WM_CHAR
  LMMouse: TLMMouse; // used by WM_LBUTTONDBLCLK
  LMMouseMove: TLMMouseMove; // used by WM_MOUSEMOVE
  LMMouseEvent: TLMMouseEvent; // used by WM_MOUSEWHEEL
  LMMove: TLMMove; // used by WM_MOVE
  LMNotify: TLMNotify; // used by WM_NOTIFY
  DrawListItemStruct: TDrawListItemStruct; //used by WM_DRAWITEM

  procedure ShowHideTabPage(NotebookHandle: HWnd; Showing: boolean);
  var
    NoteBook: TCustomNotebook;
    PageIndex, Flags: Integer;
    PageHandle: HWND;
  begin
    Notebook := GetWindowInfo(NotebookHandle)^.WinControl as TCustomNotebook;
    PageIndex := Windows.SendMessage(NotebookHandle, TCM_GETCURSEL, 0, 0);
    if PageIndex = -1 then exit;
    PageHandle := Notebook.CustomPage(PageIndex).Handle;
    if Showing then
      Flags := SW_SHOW
    else
      Flags := SW_HIDE;
    Windows.ShowWindow(PageHandle, Flags);
    if Showing then
      NotebookTabChanged(Notebook, PageIndex);
  end;

  function GetMenuItemObject: TObject;
  var MenuInfo: MENUITEMINFO;
      MainMenuHandle: HMENU;
  begin
    Result:=nil;
    MenuInfo.cbSize:=sizeof(MENUITEMINFO);
    MenuInfo.fMask:=MIIM_DATA;
    {first we have to decide if the command is from a popup menu or from the window main menu}
    //if the 'PopupMenu' property exists, there is a big probability that the command is from a popup menu
    MainMenuHandle := GetWindowInfo(Window)^.PopupMenu;
    if MainMenuHandle<>0 then //processing popup menu
    begin
      WindowInfo^.PopupMenu := 0;
      {GetMenuItemInfo can be FALSE when the 'PopupMenu' property was not
      removed in the last popup menu processing (no menuitem was selected)}
      if GetMenuItemInfo(MainMenuHandle, Lo(WParam), false, @MenuInfo)
        then Result := TObject(MenuInfo.dwItemData);
    end;
    if Result=nil then //if Result is still nil, process main menu
    begin
      MainMenuHandle := GetMenu(Window);
      if GetMenuItemInfo(MainMenuHandle, Lo(WParam), false, @MenuInfo) then
        Result := TObject(MenuInfo.dwItemData);
    end;
  end;

  procedure SendPaintMessage;
  var
    DC, MemDC: HDC;
    MemBitmap, OldBitmap : HBITMAP;
    PS : TPaintStruct;
    MemWidth: Integer;
    MemHeight: Integer;
    PaintMsg: TLMPaint;
    ORect: TRect;
    parLeft, parTop: integer;
    useDoubleBuffer: boolean;
    parentPaint: boolean;
    isNotebook: boolean;
  begin
    // note: ignores the received DC
    // do not use default deliver message
    if lWinControl = nil then
    begin
      lWinControl := GetWindowInfo(Window)^.PWinControl;
      if lWinControl = nil then exit;
    end;

    // create a paint message
    GetClassName(Window, winClassName, 20);
    isNotebook := TWin32WidgetSet(WidgetSet).ThemesActive and
      CompareMem(@winClassName, @TabControlClsName, High(TabControlClsName)+1);
    parentPaint := WindowInfo^.isTabPage or (WindowInfo^.hasTabParent and (WParam <> 0));

    // if painting background of some control for tabpage, don't handle erase background
    // in parent of tabpage
    if WindowInfo^.isTabPage then
    begin
{$ifdef MSG_DEBUG}
      writeln(MessageStackDepth, ' *forcing next WM_ERASEBKGND to disable message');
{$endif}
      PushEraseBkgndCommand(ecNoMsg);
    end;

    // paint optimizations for controls on a tabpage
    if WindowInfo^.hasTabParent and (WParam = 0) and not WindowInfo^.isTabPage then
    begin
      // if this is a groupbox in a tab, then the next erasebackground is for
      // drawing the background of the caption, send paint message then
      // update: tgroupbox does not have csOpaque, so it gets painted


      // if need to start paint, paint by calling parent, and we have no
      // controls, is a native control, use default win32 painting to avoid flicker
      if (lWinControl.ControlCount = 0)
          and not CompareMem(@winClassName, @ClsName, High(ClsName)+1) then
      begin
        // optimization: no child controls -> default painting
        exit;
      end;
    end;

    // check if double buffering is requested
    useDoubleBuffer := (WParam = 0) and lWinControl.DoubleBuffered;
    if useDoubleBuffer then
    begin
      DC := Windows.GetDC(0);
      GetWindowSize(Window, MemWidth, MemHeight);
      MemBitmap := Windows.CreateCompatibleBitmap(DC, MemWidth, MemHeight);
      Windows.ReleaseDC(0, DC);
      MemDC := Windows.CreateCompatibleDC(0);
      OldBitmap := Windows.SelectObject(MemDC, MemBitmap);
      PaintMsg.DC := MemDC;
    end;

    WinProcess := false;
    try
      if WParam = 0 then
      begin
        DC := Windows.BeginPaint(Window, @PS);
      end else begin
        DC := WParam;
      end;
      if parentPaint then
        GetWin32ControlPos(Window, GetParent(Window), parLeft, parTop);
      if not GetLCLClientBoundsOffset(lWinControl, ORect) then
      begin
        ORect.Left := 0;
        ORect.Top := 0;
        { we don't use ORect.Right and ORect.Bottom, initialize here if needed }
      end;
      PaintMsg.Msg := LM_PAINT;
      PaintMsg.PaintStruct := @PS;
      if not useDoubleBuffer then
        PaintMsg.DC := DC;
      if not WindowInfo^.hasTabParent and not isNotebook then
        lWinControl.EraseBackground(PaintMsg.DC);
      if parentPaint then
      begin
        // tabpage parent and got a dc to draw in, divert paint to parent
        MoveWindowOrgEx(PaintMsg.DC, -parLeft, -parTop);
        SendMessage(GetParent(Window), WM_PAINT, PaintMsg.DC, 0);
        MoveWindowOrgEx(PaintMsg.DC, parLeft, parTop);
      end;
      if (WParam = 0) or not WindowInfo^.hasTabParent then
      begin
        MoveWindowOrgEx(PaintMsg.DC, ORect.Left, ORect.Top);
        DeliverMessage(lWinControl, PaintMsg);
        MoveWindowOrgEx(PaintMsg.DC, -ORect.Left, -ORect.Top);
      end;
      if useDoubleBuffer then
        Windows.BitBlt(DC, 0, 0, MemWidth, MemHeight, MemDC, 0, 0, SRCCOPY);
      if WParam = 0 then
        Windows.EndPaint(Window, @PS);
    finally
      if useDoubleBuffer then
      begin
        SelectObject(MemDC, OldBitmap);
//        for debugging purposes: copy rendered bitmap to clipboard
//        Windows.OpenClipboard(0);
//        Windows.EmptyClipboard;
//        Windows.SetClipboardData(CF_BITMAP, MemBitmap);
//        Windows.CloseClipboard;
        DeleteDC(MemDC);
        DeleteObject(MemBitmap);
      end;
    end;
  end;

  procedure CheckListBoxLButtonDown;
  var
    I: Integer;
    ItemRect: Windows.Rect;
    MousePos: Windows.Point;
  begin
    MousePos.X := LMMouse.Pos.X;
    MousePos.Y := LMMouse.Pos.Y;
    for I := 0 to Windows.SendMessage(Window, LB_GETCOUNT, 0, 0) - 1 do
    begin
      Windows.SendMessage(Window, LB_GETITEMRECT, I, LongInt(@ItemRect));
      ItemRect.Right := ItemRect.Left + ItemRect.Bottom - ItemRect.Top;
      if Windows.PtInRect(ItemRect, MousePos) then
      begin
        // item clicked: toggle
        if I < TCheckListBox(lWinControl).Items.Count then
          TCheckListBox(lWinControl).Checked[I] := not TCheckListBox(lWinControl).Checked[I];
        // can only click one item
        exit;
      end;
    end;
  end;

  procedure ClearSiblingRadioButtons(RadioButton: TRadioButton);
  var
    Parent: TWinControl;
    Sibling: TControl;
    i: Integer;
  begin
    Parent := RadioButton.Parent;
    for i:= 0 to Parent.ControlCount - 1 do begin
      Sibling := Parent.Controls[i];
      if (Sibling is TRadioButton) and (Sibling<>RadioButton) and
         (TRadioButton(Sibling).HandleAllocated) then
         Windows.SendMessage(TRadioButton(Sibling).Handle, BM_SETCHECK,
            Windows.WParam(BST_UNCHECKED), 0);
    end;
  end;

  // sets the text of the combobox,
  // because some events are risen, before the text is actually changed
  procedure UpdateComboBoxText(ComboBox: TCustomComboBox);
  var
    Index: Integer;
  begin
    with ComboBox do begin
      Index := ItemIndex;
      // Index might be -1, if current text is not in the list.
      if (Index>=0) then
        Text := Items[Index]
    end;
  end;

  procedure DestroySpinEditBuddy(SpinEditHandle: HWND);
  var
    Buddy: HWND;
  begin
    Buddy := SendMessage(SpinEditHandle, UDM_GETBUDDY, 0, 0);
    DestroyWindow(Buddy);
  end;

  procedure EnableSpinEditBuddy(SpinEditHandle: HWND; Enable: boolean);
  var
    Buddy: HWND;
  begin
    Buddy := SendMessage(SpinEditHandle, UDM_GETBUDDY, 0, 0);
    Windows.EnableWindow(Buddy, Enable);
  end;

  procedure DisposeComboEditWindowInfo(ComboBox: TCustomComboBox);
  var
    Buddy: HWND;
  begin
    Buddy := Windows.GetTopWindow(ComboBox.Handle);
    if Buddy<>HWND(nil) then
      DisposeWindowInfo(Buddy);
  end;

  procedure HandleScrollMessage(LMsg: integer);
  var
    ScrollbarHandle: HWND;
    ScrollInfo: TScrollInfo;
  begin
    ScrollbarHandle := HWND(LParam);
    if ScrollbarHandle<>0 then
      lWinControl := GetWindowInfo(ScrollbarHandle)^.WinControl;
    if lWinControl is TCustomTrackBar 
    then begin
      LMessage.Msg := LM_CHANGED;
      Exit;
    end;

    PLMsg:=@LMScroll;
    with LMScroll do
    begin
      Msg := LMsg;
      ScrollCode := SmallInt(Lo(WParam));
      SmallPos := 0;
      ScrollBar := ScrollbarHandle;
      Pos := 0;
    end;

    if not (Lo(WParam) in [SB_THUMBTRACK, SB_THUMBPOSITION]) then Exit;

    // For thumb messges, retrieve the real position,
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    if Lo(WParam) = SB_THUMBTRACK
    then begin
      ScrollInfo.fMask := SIF_TRACKPOS;
      // older windows versions may not support trackpos, so fill it with some default
      ScrollInfo.nTrackPos := Hi(WParam);
    end
    else begin
      ScrollInfo.fMask := SIF_POS;
      // older windows versions may not support trackpos, so fill it with some default
      ScrollInfo.nPos := Hi(WParam);
    end;

    if ScrollbarHandle <> 0
    then begin
      // The message is send by a scrollbar
      GetScrollInfo(ScrollbarHandle, SB_CTL, ScrollInfo);
    end
    else begin
      // The message is send by a window's standard scrollbar
      if LMsg = LM_HSCROLL
      then GetScrollInfo(Window, SB_HORZ, ScrollInfo)
      else GetScrollInfo(Window, SB_VERT, ScrollInfo);
    end;
   
    with LMScroll do
    begin     
      if Lo(WParam) = SB_THUMBTRACK
      then Pos := ScrollInfo.nTrackPos
      else Pos := (ScrollInfo.nPos and $FFFF0000) or Hi(WParam);   
      // Note on the above
      // When using the scrollwheel, windows sends SB_THUMBTRACK 
      // messages, but only when scroll.max < 32K. So in that case 
      // Hi(WParam) won't cycle.
      // When ending scrollbar tracking we also get those 
      // messages. Now Hi(WParam) is cycling.
      // To get the correct value you need to use GetScrollInfo.
      //
      // Now there is a problem. GetScrollInfo returns the old position
      // while using the wheelmouse, and during tracking it returns
      // the new position.              
      // To get around this, I use the most significant part of the 
      // value returned by ScrollInfo, since that is always correct
      // The missing least significant part is given by Hi(WParam)
      // (since it is cycling) and is also always valid.
      // So the combination of both also is.
      // In the case of the wheelmouse this only works if the 
      // difference between the old pos and the new pos is < 64K.
      // I think we safely can asuume that (you wont get messages
      // anyhow if the range was > 32K :).
      // MWE.
      

      if Pos < High(SmallPos)
      then SmallPos := Pos
      else SmallPos := High(SmallPos);
    end;
  end;

  procedure HandleSetCursor;
  var
    lControl: TControl;
    BoundsOffset: TRect;
  begin
    if (lWinControl <> nil) and not (csDesigning in lWinControl.ComponentState)
      and (Lo(LParam) = HTCLIENT) then
    begin
      Windows.GetCursorPos(Windows.POINT(P));
      Windows.ScreenToClient(Window, Windows.POINT(P));
      if GetLCLClientBoundsOffset(lWinControl.Parent, BoundsOffset) then
      begin
        Dec(P.X, BoundsOffset.Left);
        Dec(P.Y, BoundsOffset.Top);
      end;
      // statictext controls do not get WM_SETCURSOR messages...
      lControl := lWinControl.ControlAtPos(P, false, true);
      if lControl = nil then
        lControl := lWinControl;
      if lControl.Cursor <> crDefault then
      begin
        Windows.SetCursor(Windows.LoadCursor(0, LclCursorToWin32CursorMap[lControl.Cursor]));
        LMessage.Result := 1;
      end;
    end;
    if LMessage.Result = 0 then
    begin
      LMessage.Msg := LM_SETCURSOR;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
    end;
    WinProcess := false;
  end;

  procedure HandleSysCommand;
  var
    ParentForm: TCustomForm;
    prevFocus: HWND;
  begin
    // forward keystroke to show window menu, if parent form has no menu
    // if wparam contains SC_KEYMENU, lparam contains key pressed
    // keymenu+space should always bring up system menu
    if ((WParam and $FFF0) = SC_KEYMENU) and (lWinControl <> nil)
      and (lParam <> VK_SPACE) then
    begin
      ParentForm := GetParentForm(lWinControl);
      if (ParentForm <> nil) and (ParentForm.Menu = nil)
        and (Application <> nil) and (Application.MainForm <> nil)
        and (Application.MainForm <> ParentForm)
        and Application.MainForm.HandleAllocated then
      begin
        targetWindow := Application.MainForm.Handle;
        if IsWindowEnabled(targetWindow) and IsWindowVisible(targetWindow) then
        begin
          prevFocus := Windows.GetFocus;
          Windows.SetFocus(targetWindow);
          PLMsg^.Result := Windows.SendMessage(targetWindow, WM_SYSCOMMAND, WParam, LParam);
          Windows.SetFocus(prevFocus);
          WinProcess := false;
        end;
      end;
    end
  end;

Begin
  Assert(False, 'Trace:WindowProc - Start');

  LMessage.Result := 0;
  LMessage.Msg := LM_NULL;
  PLMsg := @LMessage;
  WinProcess := True;
  NotifyUserInput := False;

  Assert(False, 'Trace:WindowProc - Getting Object With Callback Procedure');
  WindowInfo := GetWindowInfo(Window);
  if WindowInfo^.isChildEdit then
  begin
    lWinControl := WindowInfo^.AWinControl;
    // filter messages we want to pass on to LCL
    if (Msg <> WM_KILLFOCUS) and (Msg <> WM_SETFOCUS) and (Msg <> WM_NCDESTROY)
      and ((Msg < WM_KEYFIRST) or (Msg > WM_KEYLAST))
      and ((Msg < WM_MOUSEFIRST) or (Msg > WM_MOUSELAST)) then
    begin
      Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
      exit;
    end;
  end else begin
    lWinControl := WindowInfo^.WinControl;
  end;

  if WindowInfo^.ignoreNextChar and ((Msg = WM_CHAR) or (Msg = WM_SYSCHAR)) then
  begin
    WindowInfo^.ignoreNextChar := false;
    Result := 1;
    exit;
  end;

  Assert(False, 'Trace:WindowProc - Getting Callback Object');

  Assert(False, 'Trace:WindowProc - Checking Proc');
  Assert(False, Format('Trace:WindowProc - Window Value: $%S-%d; Msg Value: %S; WParam: $%S; LParam: $%S', [IntToHex(Window, 4), Window, WM_To_String(Msg), IntToHex(WParam, 4), IntToHex(LParam, 4)]));

  Case Msg Of
    WM_ACTIVATE:
    Begin
      Case Lo(WParam) Of
        WA_ACTIVE, WA_CLICKACTIVE:
        Begin
          LMessage.Msg := LM_ACTIVATE
        End;
        WA_INACTIVE:
        Begin
          LMessage.Msg := LM_DEACTIVATE;
        End;
      End;
    End;
    WM_ACTIVATEAPP:
    Begin
      if Window = TWin32WidgetSet(WidgetSet).AppHandle then
      begin
        if WParam <> 0 then
        begin
          Windows.SetWindowPos(TWin32WidgetSet(WidgetSet).AppHandle, HWND_TOP,
            0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
        end;
        // activate/deactivate main window
        if (Application <> nil) and (Application.MainForm <> nil) and
            Application.MainForm.HandleAllocated then
        begin
          CallDefaultWindowProc(Application.MainForm.Handle, WM_NCACTIVATE, WParam, 0);
        end;
      end;
    End;
    BM_SETCHECK:
    Begin
      LMessage.Msg := LM_CHANGED;
    End;
    WM_CAPTURECHANGED:
    Begin
      LMessage.Msg := LM_CAPTURECHANGED;
    End;
    CB_DELETESTRING, LB_DELETESTRING:
    Begin
      LMessage.Msg := LM_DELETETEXT;
    End;
    CB_INSERTSTRING, LB_INSERTSTRING:
    Begin
      PLMsg:=@LMInsertText;
      With LMInsertText Do
      Begin
        Msg := LM_INSERTTEXT;
        Position := WParam;
        NewText := PChar(LParam);
        Length := System.Length(NewText);
//        UserData := Pointer(GetWindowLong(Window, GWL_USERDATA));
      End;
    End;
    WM_CHAR:
    Begin
      PLMsg:=@LMChar;
      With LMChar Do
      Begin
        Msg := CN_CHAR;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
        Assert(False,Format('WM_CHAR KeyData= %d CharCode= %d ',[KeyData,CharCode]));
      End;
      WinProcess := false;
    End;
    WM_CLOSE:
    Begin
      if (Window = TWin32WidgetSet(WidgetSet).AppHandle) and
        (Application.MainForm <> nil) then
      begin
        Windows.SendMessage(Application.MainForm.Handle, WM_CLOSE, 0, 0);
      end else begin
        LMessage.Msg := LM_CLOSEQUERY;
      end;
      // default is to destroy window, inhibit
      WinProcess := false;
    End;
    WM_COMMAND:
    Begin
      if LParam=0 then
      begin
        {menuitem or shortcut}
        TargetObject := GetMenuItemObject;
        if TargetObject is TMenuItem then
        begin
          if (Hi(WParam) = 0) or (Hi(WParam) = 1) then
          begin
            LMessage.Msg := LM_ACTIVATE;
            TargetObject.Dispatch(LMessage);
          end;
          lWinControl := nil;
        end;
      end else begin
        lWinControl := GetWindowInfo(LParam)^.WinControl;
        // buddy controls use 'awincontrol' to designate associated wincontrol
        if lWinControl = nil then
          lWinControl := GetWindowInfo(LParam)^.AWinControl;
        if lWinControl is TCustomButton then
          case Hi(WParam) of
            BN_CLICKED:   LMessage.Msg := LM_CLICKED;
            BN_KILLFOCUS: LMessage.Msg := LM_EXIT;
          end
        else if (lWinControl is TCustomEdit) or (lWinControl is TCustomSpinEdit) then
          case Hi(WParam) of
            EN_CHANGE:    LMessage.Msg := CM_TEXTCHANGED;
          end
        else if (lWinControl is TCustomMemo) then
          case Hi(WParam) of
            // multiline edit doesn't send EN_CHANGE, so use EN_UPDATE
            EN_UPDATE:    LMessage.Msg := CM_TEXTCHANGED;
          end
        else if (lWinControl is TCustomListBox) then
          case Hi(WParam) of
            LBN_SELCHANGE:    LMessage.Msg := LM_SELCHANGE;
          end
        else if lWinControl is TCustomCombobox then
          case Hi(WParam) of
            CBN_EDITCHANGE: LMessage.Msg := LM_CHANGED;
            { CBN_EDITCHANGE is only sent after the user changes the edit box.
              CBN_SELCHANGE is sent when the user changes the text by
              selecting in the list, but before text is actually changed.
              itemindex is updated, so set text manually }
            CBN_SELCHANGE: begin
              UpdateComboBoxText(TCustomComboBox(lWinControl));
              LMessage.Msg := LM_CHANGED;
            end;
            { closeup message is sent before text is actually changed... *sigh*
              itemindex is updated, so set text manually }
            CBN_CLOSEUP:
              UpdateComboBoxText(TCustomComboBox(lWinControl));
          end;
      end;

      // no specific message found? try send a general msg
      if (LMessage.Msg = LM_NULL) and (lWinControl <> nil) then
        lWinControl.Perform(CN_COMMAND, WParam, LParam);
    End;
{
  * Besides the fact that LCL does not respond to LM_CREATE, this code is
    probably never reached anyway, as the callback is not set until after
    window creation

    WM_CREATE:
    Begin
      Assert(False, 'Trace:WindowProc - Got WM_CREATE');
      LMessage.Msg := LM_CREATE;
    End;
}
    WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
    begin
      // Groupbox (which is a button) doesn't erase it's background properly
      // it's needed for winxp themes where controls send the WM_ERASEBKGND
      // message to their parent to clear their background and then draw
      // transparently
      // only static and button controls have transparent parts
      // others need to erased with their window color
      // scrollbar also has buttons
      if (Msg = WM_CTLCOLORSTATIC) or (Msg = WM_CTLCOLORBTN)
          or (Msg = WM_CTLCOLORSCROLLBAR) then
      begin
        if GetWindowInfo(LParam)^.hasTabParent then
        begin
          // need to draw transparently, draw background
          GetWin32ControlPos(LParam, Window, P.X, P.Y);
          MoveWindowOrgEx(WParam, -P.X, -P.Y);
          SendMessage(Window, WM_PAINT, WParam, 0);
          MoveWindowOrgEx(WParam, P.X, P.Y);
          LMessage.Result := GetStockObject(HOLLOW_BRUSH);
          SetBkMode(WParam, TRANSPARENT);
          WinProcess := false;
        end else
        if WindowInfo^.isGroupBox then
        begin
          lWinControl.EraseBackground(WParam);
        end;
      end;
      if WinProcess then
      begin
        ChildWinControl := GetWindowInfo(LParam)^.WinControl;
        if ChildWinControl = nil then
          ChildWinControl := GetWindowInfo(LParam)^.AWinControl;
        if ChildWinControl <> nil then
        begin
          Windows.SetTextColor(HDC(WParam), Windows.COLORREF(ColorToRGB(ChildWinControl.Font.Color)));
          Windows.SetBkColor(HDC(WParam), Windows.COLORREF(ColorToRGB(ChildWinControl.Brush.Color)));
          LMessage.Result := LResult(ChildWinControl.Brush.Handle);
          // Override default handling
          WinProcess := false;
        end;
      end;
    end;
    WM_CLEAR:
    begin
      LMessage.Msg := LM_CLEARSEL;
    end;
    WM_COPY:
    Begin
      LMessage.Msg := LM_COPYTOCLIP;
    End;
    WM_CUT:
    Begin
      LMessage.Msg := LM_CUTTOCLIP;
    End;
    WM_DESTROY:
    Begin
      Assert(False, 'Trace:WindowProc - Got WM_DESTROY');
      if lWinControl is TCheckListBox then
        TWin32CheckListBoxStrings.DeleteItemRecords(Window);
      if lWinControl is TCustomSpinEdit then
        DestroySpinEditBuddy(Window);
      if lWinControl is TCustomComboBox then
        DisposeComboEditWindowInfo(TCustomComboBox(lWinControl));
      if WindowInfo^.Overlay<>HWND(nil) then
        Windows.DestroyWindow(WindowInfo^.Overlay);
      LMessage.Msg := LM_DESTROY;
    End;
    WM_DESTROYCLIPBOARD:
    Begin
      if assigned(OnClipBoardRequest) then begin
        OnClipBoardRequest(0, nil);
        OnClipBoardRequest := nil;
        LMessage.Result := 0;
      end;
    End;
    WM_DRAWITEM:
    Begin
      // TODO: this could crash for a MenuItem.
      WindowInfo := GetWindowInfo(PDrawItemStruct(LParam)^.hwndItem);
      if WindowInfo^.WinControl<>nil then
        lWinControl := WindowInfo^.WinControl;
      {$IFDEF MSG_DEBUG|}
      with PDrawItemStruct(LParam)^ do
        writeln(format('Received WM_DRAWITEM type %d handle %x', [ctlType, hwndItem]));
      {$ENDIF}

      if ((lWinControl is TCustomListbox) and
            (TCustomListBox(lWinControl).Style <> lbStandard)) or
         ((lWinControl is TCustomCombobox) and
            ((TCustomCombobox(lWinControl).Style = csOwnerDrawFixed) or
            (TCustomCombobox(lWinControl).Style = csOwnerDrawVariable))) then
      begin
        if PDrawItemStruct(LParam)^.itemID <> dword(-1) then
        begin
          LMessage.Msg := LM_DRAWLISTITEM;
          TLMDrawListItem(LMessage).DrawListItemStruct := @DrawListItemStruct;
          with DrawListItemStruct do begin
            ItemID := PDrawItemStruct(LParam)^.itemID;
            Area := PDrawItemStruct(LParam)^.rcItem;
            ItemState := TOwnerDrawState(PDrawItemStruct(LParam)^.itemState);
            DC := PDrawItemStruct(LParam)^._hDC;
          end;
          WinProcess := false;
        end;
      end else begin
        with TLMDrawItems(LMessage) do
        begin
          Msg := LM_DRAWITEM;
          Ctl := 0;
          DrawItemStruct := PDrawItemStruct(LParam);
        end;
        WinProcess := false;
      end;
    End;
    WM_ENABLE:
    Begin
      If WParam <> 0 Then
        LMessage.Msg := LM_SETEDITABLE;
      If Window=TWin32WidgetSet(WidgetSet).FAppHandle then
        if WParam=0 then
          DisableApplicationWindows(Window)
        else
          EnableApplicationWindows(Window);

      If (lWinControl is TCustomSpinEdit) then
        EnableSpinEditBuddy(Window, WParam<>0);
      // ugly hack to give bitbtns a nice look
      // When no theming active, the internal image needs to be
      // recreated when the enabled state is changed
      if not TWin32WidgetSet(WidgetSet).ThemesActive
      and (lWinControl is TCustomBitBtn)
      then DrawBitBtnImage(TCustomBitBtn(lWinControl), PChar(TCustomBitBtn(lWinControl).Caption));
    End;
    WM_HSCROLL:
      HandleScrollMessage(LM_HSCROLL);
    WM_ERASEBKGND:
    Begin
      eraseBkgndCommand := TEraseBkgndCommand(EraseBkgndStack and EraseBkgndStackMask);
      EraseBkgndStack := EraseBkgndStack shr EraseBkgndStackShift;
      if (eraseBkgndCommand <> ecNoMsg) and not WindowInfo^.hasTabParent then
      begin
        LMessage.Msg := LM_ERASEBKGND;
        LMessage.WParam := WParam;
        LMessage.LParam := LParam;
      end else begin
        if WindowInfo^.hasTabParent and ((lWinControl = nil)
            or not (csOpaque in lWinControl.ControlStyle)) then
          SendPaintMessage;
        LMessage.Result := 1;
      end;
      WinProcess := false;
    End;
    WM_GETDLGCODE:
    Begin
      LMessage.Result := DLGC_WANTALLKEYS;
      WinProcess := false;
    End;
{
    * TODO: make it work... icon does not show up yet, so better disable it
    WM_GETICON:
    begin
      if WindowInfo^.WinControl is TCustomForm then
      begin
        LMessage.Result := TCustomForm(WindowInfo^.WinControl).GetIconHandle;
        WinProcess := false;
      end;
    end;
}
    WM_KEYDOWN:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      With LMKey Do
      Begin
        Msg := CN_KEYDOWN;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
        Assert(False,Format('WM_KEYDOWN KeyData= %d CharCode= %d ',[KeyData,CharCode]));
        Assert(False,'  lWinControl= '+TComponent(lWinControl).Name+':'+lWinControl.ClassName);
      End;
      WinProcess := false;
    End;
    WM_KEYUP:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      With LMKey Do
      Begin
        Msg := CN_KEYUP;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
        Assert(False,Format('WM_KEYUP KeyData= %d CharCode= %d ',[KeyData,CharCode]));
      End;
      WinProcess := false;
    End;
    WM_KILLFOCUS:
    Begin
      LMessage.Msg := LM_KILLFOCUS;
    End;
    //TODO:LM_KILLCHAR,LM_KILLWORD,LM_KILLLINE
    WM_LBUTTONDBLCLK:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      With LMMouse Do
      Begin
        Msg := LM_LBUTTONDBLCLK;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      End;

      // CheckListBox functionality
      if lWinControl is TCheckListBox then
        CheckListBoxLButtonDown;
    End;
    WM_LBUTTONDOWN:
    Begin
      // if mouse-click, focus-change, mouse-click, cursor hasn't moved:
      // simulate double click, assume focus change due to first mouse-click
      if (MouseDownFocusStatus = mfFocusChanged) and (MouseDownFocusWindow = Window)
          and (GetTickCount - MouseDownTime <= GetDoubleClickTime)
          and CheckMouseMovement then
      begin
        PostMessage(Window, WM_LBUTTONDBLCLK, WParam, LParam);
      end;

      MouseDownTime := GetTickCount;
      MouseDownWindow := Window;
      MouseDownFocusWindow := 0;
      MouseDownFocusStatus := mfFocusSense;
      GetCursorPos(MouseDownPos);
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      With LMMouse Do
      Begin
        Msg := LM_LBUTTONDOWN;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      End;

      // CheckListBox functionality
      if lWinControl is TCheckListBox then
        CheckListBoxLButtonDown;
      // RadioButton functionality
      if lWinControl is TRadioButton then
        Windows.SendMessage(Window, BM_SETCHECK, BST_CHECKED, 0);
      // focus window
      if (Windows.GetFocus <> Window) and
          ((lWinControl = nil) or (lWinControl.CanFocus)) then
        Windows.SetFocus(Window);
    End;
    WM_LBUTTONUP:
    Begin
      if (MouseDownWindow = Window) and (MouseDownFocusStatus = mfNone) then
        MouseDownFocusStatus := mfFocusSense;
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      With LMMouse Do
      Begin
        Msg := LM_LBUTTONUP;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      End;
    End;
    WM_MBUTTONDBLCLK:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      With LMMouse Do
      Begin
        Msg := LM_MBUTTONDBLCLK;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      End;
    End;
    WM_MBUTTONDOWN:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      With LMMouse Do
      Begin
        Msg := LM_MBUTTONDOWN;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      End;
    End;
    WM_MBUTTONUP:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      With LMMouse Do
      Begin
        Msg := LM_MBUTTONUP;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      End;
    End;
    WM_MOUSEHOVER:
    Begin
      NotifyUserInput := True;
      LMessage.Msg := LM_ENTER;
    End;
    WM_MOUSELEAVE:
    Begin
      NotifyUserInput := True;
      LMessage.Msg := LM_LEAVE;
    End;
    WM_MOUSEMOVE:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMMouseMove;
      With LMMouseMove Do
      Begin
        Msg := LM_MOUSEMOVE;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
        // check if this is a spurious WM_MOUSEMOVE message, pos not actually changed
        if (XPos = WindowInfo^.MouseX) and (YPos = WindowInfo^.MouseY) then
        begin
          // do not fire message after all (position not changed)
          Msg := LM_NULL;
          NotifyUserInput := false;
        end else
        if WindowInfo <> @DefaultWindowInfo then
        begin
          // position changed, update window info
          WindowInfo^.MouseX := XPos;
          WindowInfo^.MouseY := YPos;
        end;
      End;
    End;
    WM_MOUSEWHEEL:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMMouseEvent;
      With LMMouseEvent Do
      Begin
        X := SmallInt(Lo(LParam));
        Y := SmallInt(Hi(LParam));
        // check if mouse cursor within this window, otherwise send message to window the mouse is hovering over
        P.X := X;
        P.Y := Y;
        TargetWindow := TWin32WidgetSet(WidgetSet).WindowFromPoint(P);
        if TargetWindow = HWND(nil) then
          exit;

        // check if the window is an edit control of a combobox, if so,
        // redirect it to the combobox, not the edit control
        if GetWindowInfo(TargetWindow)^.isComboEdit then
          TargetWindow := Windows.GetParent(TargetWindow);
        // Don't send the message to the overlay window, to avoid recursion
        if (TargetWindow <> Window) and (TargetWindow<>WindowInfo^.Overlay) then
        begin
          Result := SendMessage(TargetWindow, WM_MOUSEWHEEL, WParam, LParam);
          exit;
        end;

        // the mousewheel message is for us
        // windows handles combobox's mousewheel messages
        if lWinControl.FCompStyle <> csComboBox then
        begin
          Msg := LM_MOUSEWHEEL;
          WheelDelta := SmallInt(Hi(WParam));
          State := GetShiftState;
          UserData := Pointer(GetWindowLong(Window, GWL_USERDATA));
          WinProcess := false;
        end;
      end;
    end;
    //TODO:LM_MOVEPAGE,LM_MOVETOROW,LM_MOVETOCOLUMN
    WM_NCACTIVATE:
    begin
      // do not allow main form to be deactivated
      if (Application <> nil) and (Application.MainForm <> nil) and
          Application.MainForm.HandleAllocated and (Window = Application.MainForm.Handle) and
          (WParam = 0) then
      begin
        WParam := 1;
      end;
    end;
    WM_NCHITTEST:
    begin
      if (lWinControl <> nil) and (lWinControl.FCompStyle = csHintWindow) then
      begin
        LMessage.Result := HTTRANSPARENT;
        WinProcess := false;
      end;
    end;
    WM_NCLBUTTONDOWN:
    Begin
      NotifyUserInput := True;
      Assert(False, 'Trace:WindowProc - Got WM_NCLBUTTONDOWN');
    End;
    WM_NOTIFY:
    Begin
      if PNMHdr(LParam)^.code=MCN_SELCHANGE then begin
        LMessage.Msg := LM_CHANGED;
        WindowInfo := GetWindowInfo(PNMHdr(LParam)^.hwndFrom);
        if WindowInfo^.WinControl<>nil then
          lWinControl := WindowInfo^.WinControl;
      end
      else begin
        PLMsg:=@LMNotify;
        With LMNotify Do
        Begin
          Msg := LM_NOTIFY;
          IDCtrl := WParam;
          NMHdr := PNMHDR(LParam);
          With NMHdr^ do
          case code of
            TCN_SELCHANGING:
              ShowHideTabPage(HWndFrom, False);
            TCN_SELCHANGE:
            begin
              ShowHideTabPage(HWndFrom, True);
              idFrom := Windows.SendMessage(HWndFrom, TCM_GETCURSEL, 0, 0);
            end;
          end;
        End;
      end;
    End;
    WM_PAINT:
    Begin
      SendPaintMessage;
      // SendPaintMessage sets winprocess to false
    End;
    WM_PASTE:
    Begin
      LMessage.Msg := LM_PASTEFROMCLIP;
    End;
    WM_RBUTTONDBLCLK:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      With LMMouse Do
      Begin
        Msg := LM_RBUTTONDBLCLK;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      End;
    End;
    WM_RBUTTONDOWN:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      With LMMouse Do
      Begin
        Msg := LM_RBUTTONDOWN;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      End;
    End;
    WM_RBUTTONUP:
    Begin
      NotifyUserInput := True;
      WinProcess := false;
      PLMsg:=@LMMouse;
      With LMMouse Do
      Begin
        Msg := LM_RBUTTONUP;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
        Result := 0;
      End;
    End;
    WM_SETCURSOR:
    begin
      HandleSetCursor;
    end;
    WM_SETFOCUS:
    Begin
      // handle feature mouse-click, setfocus, mouse-click -> double-click
      if (Window <> MouseDownWindow) and (MouseDownFocusStatus <> mfNone) then
      begin
        MouseDownFocusStatus := mfFocusChanged;
        MouseDownFocusWindow := Window;
      end;
      LMessage.Msg := LM_SETFOCUS;
      if (lWinControl <> nil) and (lWinControl.FCompStyle = csEdit) then
        Windows.SendMessage(Window, EM_SETSEL, 0, -1);
    End;
    WM_SHOWWINDOW:
    Begin
      Assert(False, 'Trace:WindowProc - Got WM_SHOWWINDOW');
      With TLMShowWindow(LMessage) Do
      Begin
        Msg := LM_SHOWWINDOW;
        Show := WParam <> 0;
        Status := LParam;
      End;

      if assigned(lWinControl) and ((WParam<>0) or not lWinControl.Visible)
        and ((WParam=0) or lWinControl.Visible)
        and (Application<>nil) and (lWinControl=Application.MainForm) then
      begin
        if WParam=0 then
          Flags := SW_HIDE
        else
          Flags := SW_SHOW;
        Windows.ShowWindow(TWin32WidgetSet(WidgetSet).FAppHandle, Flags);
      end;
    End;
    WM_SYSCHAR:
    Begin
      PLMsg:=@LMChar;
      With LMChar Do
      Begin
        Msg := CN_SYSCHAR;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
        Assert(False,Format('WM_CHAR KeyData= %d CharCode= %d ',[KeyData,CharCode]));
      End;
      WinProcess := false;
    End;
    WM_SYSCOMMAND:
    begin
      HandleSysCommand;
    end;
    WM_SYSKEYDOWN:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      With LMKey Do
      Begin
        Msg := CN_SYSKEYDOWN;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
      End;
      WinProcess := false;
    End;
    WM_SYSKEYUP:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      With LMKey Do
      Begin
        Msg := CN_SYSKEYUP;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
      End;
      WinProcess := false;
    End;
    WM_TIMER:
    Begin
      LMessage.Msg := LM_TIMER;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
    End;
    WM_VSCROLL:
      HandleScrollMessage(LM_VSCROLL);
    WM_WINDOWPOSCHANGED:
    Begin
      With TLMWindowPosMsg(LMessage) Do
      Begin
        Msg := LM_WINDOWPOSCHANGED;
        Unused := WParam;
        WindowPos := PWindowPos(LParam);
      End;
      // cross-interface compatible: complete invalidate on resize
      if (PWindowPos(LParam)^.flags and SWP_NOSIZE) = 0 then
        Windows.InvalidateRect(Window, nil, true);
    End;
    WM_MEASUREITEM:
    Begin
      if LWinControl<>nil then begin
        if LWinControl is TCustomCombobox then begin
          LMessage.Msg := LM_MEASUREITEM;
          LMessage.LParam := LParam;
          LMessage.WParam := WParam;
          Winprocess := False;
        end else
        if WParam=0 then begin
          // todo: it's a menu
        end else begin
          LWinControl := TWinControl(WParam);
          if LWinControl<>nil then begin
            LMessage.Msg := LM_MEASUREITEM;
            LMessage.LParam := LParam;
            LMessage.WParam := WParam;
            Winprocess := False;
          end;
        end;
      end;
    End;
    WM_THEMECHANGED:
    begin
      // winxp theme changed, recheck whether themes are enabled
      TWin32WidgetSet(WidgetSet).UpdateThemesActive;
    end;
  End;

  If WinProcess Then
  begin
    PLMsg^.Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
    WinProcess := false;
  end;

  Case Msg Of
    WM_MOVE:
    Begin
      PLMsg:=@LMMove;
      With LMMove Do
      Begin
        Msg := LM_MOVE;
        // MoveType := WParam;   WParam is not defined!
        MoveType := Move_SourceIsInterface;
        if (lWinControl is TCustomForm)
        and (TCustomForm(lWinControl).Parent=nil) then
        begin
          if Windows.GetWindowRect(Window,@R) then
          begin
            XPos := R.Left;
            YPos := R.Top;
          end else begin
            Msg := LM_NULL;
          end;
        end else begin
          if GetWindowRelativePosition(Window,NewLeft,NewTop) then
          begin
            XPos := NewLeft;
            YPos := NewTop;
          end else begin
            Msg := LM_NULL;
          end;
        end;
        if lWinControl <> nil then begin
          {$IFDEF VerboseSizeMsg}
          writeln('Win32CallBack WM_MOVE ',lWinControl.Name,':',lWinControl.ClassName,
            ' NewPos=',XPos,',',YPos);
          {$ENDIF}
          if (lWinControl.Left=XPos) and (lWinControl.Top=YPos) then
            exit;
        end;
      End;
    End;
    WM_SIZE:
    Begin
      With TLMSize(LMessage) Do
      Begin
        Msg := LM_SIZE;
        SizeType := WParam or Size_SourceIsInterface;
        GetWindowSize(Window, NewWidth, NewHeight);
        Width := NewWidth;
        Height := NewHeight;
        if lWinControl <> nil then
        begin
          {$IFDEF VerboseSizeMsg}
          GetClientRect(Window,R);
          writeln('Win32Callback: WM_SIZE ',lWinControl.Name,':',lWinControl.ClassName,
            ' NewSize=',Width,',',Height,
            ' HasVScroll=',(Windows.GetWindowLong(Window, GWL_STYLE) and WS_VSCROLL) <> 0,
            ' HasHScroll=',(Windows.GetWindowLong(Window, GWL_STYLE) and WS_HSCROLL) <> 0,
            ' OldClientSize=',lWinControl.CachedClientWidth,',',lWinControl.CachedClientHeight,
            ' NewClientSize=',R.Right,',',R.Bottom);
          {$ENDIF}
          if (lWinControl.Width<>Width) or (lWinControl.Height<>Height)
              or lWinControl.ClientRectNeedsInterfaceUpdate then
          begin
            lWinControl.InvalidateClientRectCache(true);
            lWinControl.DoAdjustClientRectChange;
          end;
        end;
        OverlayWindow := GetWindowInfo(Window)^.Overlay;
        if OverlayWindow <> 0 then
          Windows.SetWindowPos(OverlayWindow, HWND_TOP, 0, 0, NewWidth, NewHeight, SWP_NOMOVE);
      End;
    End;
    BM_SETCHECK:
    begin
      if (WParam=BST_CHECKED) and (lWinControl is TRadioButton) then
        ClearSiblingRadioButtons(TRadioButton(lWinControl));
    end;
  end;

  // convert from win32 client to lcl client pos
  if PLMsg = @LMMouseMove then
  begin
    if GetLCLClientBoundsOffset(Window, R) then
    begin
      Dec(LMMouseMove.XPos, R.Left);
      Dec(LMMouseMove.YPos, R.Top);
    end;
  end else
  if PLMsg = @LMMouse then
  begin
    if GetLCLClientBoundsOffset(Window, R) then
    begin
      Dec(LMMouse.XPos, R.Left);
      Dec(LMMouse.YPos, R.Top);
    end;
  end;

  // application processing
  if NotifyUserInput then
    NotifyApplicationUserInput(PLMsg^.Msg);

  if (lWinControl <> nil) and (PLMsg^.Msg <> LM_NULL) then
    DeliverMessage(lWinControl, PLMsg^);

  // respond to result of LCL handling the message
  case PLMsg^.Msg of
    LM_ERASEBKGND, LM_SETCURSOR, LM_RBUTTONUP:
    begin
      if PLMsg^.Result = 0 then
        WinProcess := true;
    end;

    CN_CHAR, CN_SYSCHAR:
    begin
      // if key not yet processed, let windows process it
      WinProcess := LMChar.Result = 0;
      WParam := LMChar.CharCode;
    end;

    CN_KEYDOWN, CN_KEYUP, CN_SYSKEYDOWN, CN_SYSKEYUP:
    begin
      // if key not yet processed, let windows process it
      WinProcess := LMKey.Result = 0;
      WParam := LMKey.CharCode;
    end;

  else
    case Msg of
      WM_LBUTTONDOWN, WM_LBUTTONUP:
      begin
        if MouseDownFocusStatus = mfFocusSense then
          MouseDownFocusStatus := mfNone;
      end;

      WM_NCDESTROY:
      begin
        // free our own data associated with window
        if DisposeWindowInfo(Window) then
          WindowInfo := nil;
        EnumProps(Window, @PropEnumProc);
      end;
    end;
  end;

  if WinProcess then
  begin
    PLMsg^.Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
    case Msg of
      WM_CHAR, WM_KEYDOWN, WM_KEYUP,
      WM_SYSCHAR, WM_SYSKEYDOWN, WM_SYSKEYUP:
      begin
        PLMsg^.Result := 0;
        case Msg of
          WM_CHAR:
          begin
            // if want chars, then handled already
            PLMsg^.Result := CallDefaultWindowProc(Window, WM_GETDLGCODE, 0, 0) and DLGC_WANTCHARS;
            LMChar.CharCode := Word(WParam);
            LMChar.Msg := LM_CHAR;
          end;
          WM_SYSCHAR:
          begin
            LMChar.CharCode := Word(WParam);
            LMChar.Msg := LM_SYSCHAR;
          end;
          WM_KEYDOWN:
          begin
            LMKey.CharCode := Word(WParam);
            LMKey.Msg := LM_KEYDOWN;
          end;
          WM_KEYUP:
          begin
            LMKey.CharCode := Word(WParam);
            LMKey.Msg := LM_KEYUP;
          end;
          WM_SYSKEYDOWN:
          begin
            LMKey.CharCode := Word(WParam);
            LMKey.Msg := LM_SYSKEYDOWN;
          end;
          WM_SYSKEYUP:
          begin
            LMKey.CharCode := Word(WParam);
            LMKey.Msg := LM_SYSKEYUP;
          end;
        end;

        // we cannot tell for sure windows didn't want the key
        // for WM_CHAR check WM_GETDLGCODE/DLGC_WANTCHARS
        // winapi too inconsistent about return value
        if PLMsg^.Result = 0 then
          DeliverMessage(lWinControl, PLMsg^);

        // handle Ctrl-A for edit controls
        if (PLMsg^.Result = 0) and (Msg = WM_KEYDOWN) and (WParam = Ord('A'))
          and (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_MENU) >= 0) then
        begin
          GetClassName(Window, winClassName, 20);
          if CompareMem(@winClassName, @EditClsName, High(EditClsName)+1) then
          begin
            // select all
            Windows.SendMessage(Window, EM_SETSEL, 0, -1);
          end;
        end;
      end;
    end;
  end;

  // ignore WM_(SYS)CHAR message if LCL handled WM_(SYS)KEYDOWN
  if ((Msg = WM_KEYDOWN) or (Msg = WM_SYSKEYDOWN))
      and (WindowInfo <> @DefaultWindowInfo)
      and (PLMsg^.Result <> 0) then
    WindowInfo^.ignoreNextChar := true;
  // stop ignoring if KEYUP has come by (not all keys generate CHAR)
  if ((Msg = WM_KEYUP) or (Msg = WM_SYSKEYUP))
      and (WindowInfo <> @DefaultWindowInfo) then
    WindowInfo^.ignoreNextChar := false;

  { LMMouseEvent and LMInsertText have no Result field }

  if      PLMsg = @LMScroll     then Result := LMScroll.Result
  else if PLMsg = @LMKey        then Result := LMKey.Result
  else if PLMsg = @LMChar       then Result := LMChar.Result
  else if PLMsg = @LMMouse      then Result := LMMouse.Result
  else if PLMsg = @LMMouseMove  then Result := LMMouseMove.Result
  else if PLMsg = @LMMove       then Result := LMMove.Result
  else if PLMsg = @LMNotify     then Result := LMNotify.Result
  else if PLMsg = @LMMouseEvent then Result := 1
  else                               Result := PLMsg^.Result;

  Assert(False, 'Trace:WindowProc - Exit');
End;

{$ifdef MSG_DEBUG}

function WindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
begin
  writeln(MessageStackDepth, 'WindowProc called for window=', window,' msg=', WM_To_String(msg),' wparam=', wparam, ' lparam=',lparam);
  MessageStackDepth := MessageStackDepth + ' ';

  Result := RealWindowProc(Window, Msg, WParam, LParam);

  setlength(MessageStackDepth, length(MessageStackDepth)-1);
end;

{$endif}

{------------------------------------------------------------------------------
 Function: OverlayWindowProc
 Params: Window - The window that receives a message
         Msg    - The message received
         WParam - Word parameter
         LParam - Long-integer parameter
  Returns: 0 if Msg is handled; non-zero long-integer result otherwise

  Handles messages specifically for the window used by GetDesignerDC
 ------------------------------------------------------------------------------}
function OverlayWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
begin
  case Msg of
    WM_ERASEBKGND:
    begin
      Result := 1;
    end;
    WM_KEYFIRST..WM_KEYLAST, WM_MOUSEFIRST..WM_MOUSELAST:
    begin
      // parent of overlay is the form
      Result := Windows.SendMessage(Windows.GetParent(Window), Msg, WParam, LParam);
    end;
    WM_NCDESTROY:
    begin
      // free our own data associated with window
      DisposeWindowInfo(Window);
    end;
  else
    Result := Windows.DefWindowProc(Window, Msg, WParam, LParam);
  end;
end;

{------------------------------------------------------------------------------
 Function: ComboBoxWindowProc
 Params: Window - The window that receives a message
         Msg    - The message received
         WParam - Word parameter
         LParam - Long-integer parameter
  Returns: 0 if Msg is handled; non-zero long-integer result otherwise

  Handles the messages sent to a combobox control by Windows or other
  applications
 ------------------------------------------------------------------------------}
function ComboBoxWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
begin
  // darn MS: if combobox has edit control, and combobox receives focus, it
  // passes it on to the edit, so it will send a WM_KILLFOCUS; inhibit
  // also don't pass WM_SETFOCUS to the lcl,
  // it will get one from the edit control
  if ((Msg = WM_KILLFOCUS) or (Msg = WM_SETFOCUS)) and
     (Windows.GetTopWindow(Window) <> HWND(nil)) then
  begin
    // continue normal processing, don't send to lcl
    Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
  end else begin
    // normal processing
    Result := WindowProc(Window, Msg, WParam, LParam);
  end;
end;

{------------------------------------------------------------------------------
 Procedure: TimerCallBackProc
 Params: window_hnd - handle of window for timer message, not set in this implementation
         msg        - WM_TIMER message
         idEvent    - timer identifier
         dwTime     - current system time

 Calls the timerfunction in the Timer Object in the LCL
 ------------------------------------------------------------------------------}
Procedure TimerCallBackProc(window_hwnd : hwnd; msg : DWORD; idEvent: UINT; dwTime: DWORD); stdcall;
Var
  TimerInfo: PWin32TimerInfo;
  n: Integer;
begin
  n := FTimerData.Count;
  while (n>0) do begin
    dec(n);
    TimerInfo := FTimerData[n];
    if TimerInfo^.TimerID=idEvent then begin
      TimerInfo^.TimerFunc;
      break;
    end;
  end;
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

