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
  TWin32WidgetSet(InterfaceObject).HandleWakeMainThread(nil);
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
  TEraseBkgndCommand = (ecDefault, ecPaint, ecNoMsg);
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
    isNotebook := TWin32WidgetSet(InterfaceObject).ThemesActive and 
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
      if ((GetWindowLong(Window, GWL_STYLE) and BS_GROUPBOX) = BS_GROUPBOX)
          and CompareMem(@winClassName, @ButtonClsName, High(ButtonClsName)+1) then
      begin
  {$ifdef MSG_DEBUG}
        writeln(MessageStackDepth, ' *forcing next WM_ERASEBKGND to send paint message');
  {$endif}
        PushEraseBkgndCommand(ecPaint);
      end;
        
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
  
  procedure DestroySpinEditBuddy(SpinEdit: TCustomSpinEdit);
  var
    Buddy: HWND;
  begin
    Buddy := SendMessage(SpinEdit.Handle, UDM_GETBUDDY, 0, 0);
    DestroyWindow(Buddy);
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
    TargetWindow: HWND;
  begin
    TargetWindow := HWND(LParam);
    if TargetWindow<>0 then
      lWinControl := GetWindowInfo(TargetWindow)^.WinControl;
    if lWinControl is TCustomTrackBar then begin
      LMessage.Msg := LM_CHANGED;
    end
    else begin
      PLMsg:=@LMScroll;
      With LMScroll Do
      Begin
        Msg := LMsg;
        ScrollCode := SmallInt(Lo(WParam));
        Pos := SmallInt(Hi(WParam));
        ScrollBar := TargetWindow;
      End;
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
    if (Msg <> WM_KILLFOCUS) and (Msg <> WM_SETFOCUS)
      and ((Msg < WM_KEYFIRST) or (Msg > WM_KEYLAST))
      and ((Msg < WM_MOUSEFIRST) or (Msg > WM_MOUSELAST)) then
    begin
      Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
      exit;
    end;
  end else begin
    lWinControl := WindowInfo^.WinControl;
  end;
  Assert(False, 'Trace:WindowProc - Getting Callback Object');

  Assert(False, 'Trace:WindowProc - Checking Proc');
  Assert(False, Format('Trace:WindowProc - Window Value: $%S-%d; Msg Value: %S; WParam: $%S; LParam: $%S', [IntToHex(Window, 4), Window, WM_To_String(Msg), IntToHex(WParam, 4), IntToHex(LParam, 4)]));

  Case Msg Of
    LM_MONTHCHANGED..LM_DAYCHANGED:
    Begin
      If lWinControl Is TCalendar Then
        LMessage.Msg := Msg;
    End;
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
      if Window = TWin32WidgetSet(InterfaceObject).AppHandle then
      begin
        if WParam <> 0 then
        begin
          Windows.SetWindowPos(TWin32WidgetSet(InterfaceObject).AppHandle, HWND_TOP, 
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
      if (WParam=BST_CHECKED) and (lWinControl is TRadioButton) then
        ClearSiblingRadioButtons(TRadioButton(lWinControl));
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
        Assert(False,Format('WM_CHAR KeyData= %d CharCode= %d ',[KeyData,CharCode]));
      End;
      WinProcess := false;
    End;
    WM_CLOSE:
    Begin
      if (Window = TWin32WidgetSet(InterfaceObject).AppHandle) and 
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
        if WindowInfo^.isGroupBox then
        begin
          lWinControl.EraseBackground(WParam);
        end else
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
        DestroySpinEditBuddy(TCustomSpinEdit(lWinControl));
      if lWinControl is TCustomComboBox then
        DisposeComboEditWindowInfo(TCustomComboBox(lWinControl));
      if WindowInfo^.Overlay<>HWND(nil) then
        Windows.DestroyWindow(WindowInfo^.Overlay);
      LMessage.Msg := LM_DESTROY;
      PostQuitMessage(0);
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
      lWinControl := WindowInfo^.WinControl;
      {$IFDEF MSG_DEBUG|}
      with PDrawItemStruct(LParam)^ do
        writeln(format('Received WM_DRAWITEM type %d handle %x', [ctlType, hwndItem]));
      {$ENDIF}

      if (lWinControl is TCustomListbox) and
            (TCustomListBox(lWinControl).Style <> lbStandard) then
      begin
        LMessage.Msg := LM_DRAWLISTITEM;
        TLMDrawListItem(LMessage).DrawListItemStruct := @DrawListItemStruct;
        with DrawListItemStruct do begin
          ItemID := PDrawItemStruct(LParam)^.itemID;
          Area := PDrawItemStruct(LParam)^.rcItem;
          ItemState := TOwnerDrawState(PDrawItemStruct(LParam)^.itemState);
          DC := PDrawItemStruct(LParam)^._hDC;
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
      
      // ugly hack to give bitbtns a nice look
      // When no theming active, the internal image needs to be 
      // recreated when the enabled state is changed
      if not TWin32WidgetSet(InterfaceObject).ThemesActive
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
        if eraseBkgndCommand = ecPaint then
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
        TargetWindow := TWin32WidgetSet(InterfaceObject).WindowFromPoint(P);
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

      if assigned(lWinControl) and
        (lWinControl=Application.MainForm) then begin
        if WParam=0 then
          Windows.ShowWindow(TWin32WidgetSet(InterfaceObject).FAppHandle,
            SW_HIDE)
        else
          Windows.ShowWindow(TWin32WidgetSet(InterfaceObject).FAppHandle,
            SW_SHOW)
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
        Msg := LM_SYSKEYDOWN;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
      End;
    End;
    WM_SYSKEYUP:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      With LMKey Do
      Begin
        Msg := LM_SYSKEYUP;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
      End;
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
      if TObject(WParam) is TWinControl then
      begin
        lWinControl := TWinControl(WParam);
        LMessage.Msg := LM_MEASUREITEM;
        LMessage.LParam := LParam;
        LMessage.WParam := WParam;
      end else
        lWinControl := nil;
    End;
    WM_THEMECHANGED:
    begin
      // winxp theme changed, recheck whether themes are enabled
      TWin32WidgetSet(InterfaceObject).UpdateThemesActive;
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
    LM_ERASEBKGND, LM_SETCURSOR:
    begin
      if LMessage.Result = 0 then
        WinProcess := true;
    end;

    CN_CHAR, CN_SYSCHAR:
    begin
      // if key not yet processed, let windows process it
      WinProcess := LMChar.CharCode <> VK_UNKNOWN;
      WParam := LMChar.CharCode;
    end;
   
    CN_KEYDOWN, CN_KEYUP, CN_SYSKEYDOWN, CN_SYSKEYUP:
    begin
      // if key not yet processed, let windows process it
      WinProcess := LMKey.CharCode <> VK_UNKNOWN;
      WParam := LMKey.CharCode;
    end;
    
  else
    case Msg of
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
    if PLMsg^.Result = 0 then
    begin
      case Msg of
        WM_CHAR, WM_KEYDOWN, WM_KEYUP,
        WM_SYSCHAR, WM_SYSKEYDOWN, WM_SYSKEYUP:
        begin
          case Msg of
            WM_CHAR: 
            begin
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

          // windows didn't want the key
          DeliverMessage(lWinControl, PLMsg^);
        end;
      end;
    end;
  end;
  
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

{
  $Log$
  Revision 1.194  2005/03/03 13:13:51  vincents
  fixed thread synchronize support for fpc 1.9.9 using WakeMainThread

  Revision 1.193  2005/02/10 21:07:29  micha
  let general WindowProc also handle childedit of combobox, reduces code duplication, implements doubleclick for combobox

  Revision 1.192  2005/02/07 19:10:31  micha
  revert unwanted commit

  Revision 1.191  2005/02/07 18:53:35  micha
  implement size constraints for calendar (it is not resizeable)

  Revision 1.190  2005/02/07 09:07:38  micha
  improve windows-has-handled-key check

  Revision 1.189  2005/02/05 20:42:04  vincents
  added TLMInsertText (temporarily?)

  Revision 1.188  2005/02/03 20:19:17  micha
  fix bug 267: reroute menu key to main form if form has no menu

  Revision 1.187  2005/02/03 15:06:30  micha
  translate and route WM_SYSCHAR messages, fix WM_SYSKEYDOWN and WM_SYSKEYUP messages

  Revision 1.186  2005/02/01 15:48:03  vincents
  fixed handling WM_MOUSEWHEEL in designer form (bug 647).

  Revision 1.185  2005/01/21 22:07:10  micha
  simpler overlay window

  Revision 1.184  2005/01/15 10:09:23  micha
  fix bug 558: title bar redraw (win32)
  USE_SYNCHRONIZE enabled per default for 1.9.x

  Revision 1.183  2005/01/10 20:30:10  vincents
  fixed fpc 1.0.x compilation

  Revision 1.182  2005/01/08 22:19:51  micha
  fix cursor handling, and statictext alignment

  Revision 1.181  2005/01/07 10:51:07  vincents
  send LM_CHANGE message for trackbar

  Revision 1.180  2005/01/03 15:50:35  micha
  WM_SIZE can also mean windowstate change, always send message

  Revision 1.179  2005/01/03 12:15:54  micha
  keep the mainform (ie. lazarus main ide bar) active

  Revision 1.178  2005/01/02 22:27:15  micha
  focus window only if possible

  Revision 1.177  2005/01/02 22:18:52  micha
  focus window on left-click

  Revision 1.176  2004/12/31 16:29:58  micha
  prevent reporting of spurious mousemove messages

  Revision 1.175  2004/12/27 12:08:54  micha
  set focus to a control on the newly focussed tabpage of a notebook (fixes bug 501)

  Revision 1.174  2004/12/27 10:18:21  micha
  implement support for tthread.synchronize

  Revision 1.173  2004/12/17 09:00:47  vincents
  set version to 0.9.3

  Revision 1.172  2004/12/16 14:29:12  micha
  fix showmodal to remember disabled windows (fixes bug 478, and more)

  Revision 1.171  2004/12/15 17:51:16  micha
  fix clientrect size on show form with menu (lazarus ide, component palette height too great)

  Revision 1.170  2004/12/01 22:38:36  vincents
  fixed error, when more than one timer was disabled in a timer callback.

  Revision 1.169  2004/11/26 12:26:13  vincents
  implemented SelectionChange event

  Revision 1.168  2004/11/26 11:23:38  vincents
  fixed sending two lm_setfocus message for combobox.

  Revision 1.167  2004/11/21 21:09:29  vincents
  use message name instead of number in debug message.
  postponed removing properties until wm_ncdestroy.

  Revision 1.166  2004/11/18 14:58:06  vincents
  destroy overlay window, when destroying the owner

  Revision 1.165  2004/11/17 23:06:29  vincents
  centralized dispose windowinfo
  removed debug info

  Revision 1.164  2004/11/17 22:20:42  vincents
  fixed memleak with windowinfo of comboedit.

  Revision 1.163  2004/11/16 12:21:51  vincents
  fix copy/paste error in key handler.

  Revision 1.162  2004/11/15 15:52:48  vincents
  fix crash when dropping down a owner drawn combobox

  Revision 1.161  2004/11/15 08:00:05  micha
  forms with WS_POPUP style have a parent, GetParent(...)=0 is not reliable, and here redundant
  tidy code a bit

  Revision 1.160  2004/11/12 15:33:13  vincents
  fix changing key in OnKeyPress event.

  Revision 1.159  2004/11/12 15:30:30  vincents
  send CM_TEXTCHANGED for TCustomMemo

  Revision 1.158  2004/11/08 13:20:11  vincents
  destroy SpinEdit's buddy when destroying SpinEdit.

  Revision 1.157  2004/11/07 17:58:17  micha
  fix message handling (use actual message, not default), fixes edits begin readonly

  Revision 1.156  2004/11/05 15:23:23  micha
  correct silly mistake: "as" operator also checks for nil

  Revision 1.155  2004/11/04 17:02:10  micha
  cleanup: remove redundant typecasts and checks

  Revision 1.154  2004/11/04 09:19:34  micha
  respond to LCL result of handling the message (fixes black background in listbox)

  Revision 1.153  2004/11/03 16:32:54  micha
  fix drawing of listbox, combobox, edit to be non-transparent even in winxp tabpage (fixes black background upon scroll)

  Revision 1.152  2004/10/29 18:55:30  micha
  speed up menu creation

  Revision 1.151  2004/10/29 14:24:17  micha
  make code possible safer, with better typecasts and use of GlobalAddAtom, because windows are global

  Revision 1.150  2004/10/29 11:00:32  micha
  fix typo: the atom var is WindowInfoAtom
  use WM_NCDESTROY, it is later than WM_DESTROY

  Revision 1.149  2004/10/29 10:38:39  micha
  fix typo, use wparam, not lparam

  Revision 1.148  2004/10/29 09:52:08  micha
  fix crash on showing tabpage
  fix painting of radiobutton in groupbox (non-tabpage-parent)

  Revision 1.147  2004/10/28 21:00:56  micha
  convert GetProp and SetProp usage to one Atom pointing to a record of fields

  Revision 1.146  2004/10/28 07:43:29  micha
  experiment: use CS_SAVEBITS class style on tabpages to reduce flickering

  Revision 1.145  2004/10/27 20:58:58  micha
  fix winxp theming for tabcontrols (shaded background)

  Revision 1.144  2004/10/17 14:53:48  micha
  use font/brush of "parent", if this is a buddy window

  Revision 1.143  2004/10/15 09:51:09  micha
  splitup of CreateComponent to widgetset CreateHandle methods

  Revision 1.142  2004/10/06 10:52:46  micha
  split up common dialogs code

  Revision 1.141  2004/10/05 20:14:19  micha
  fix update edit text for generic edit controls

  Revision 1.140  2004/09/24 21:34:14  micha
  convert LM_CREATE message to interface methods
  remove SendMsgToInterface, CNSendMessage and related methods
  remove TWidgetSet.IntSendMessage3; all LCL to interface messages have been converted

  Revision 1.139  2004/09/13 13:13:46  micha
  convert LM_SHOWMODAL to interface methods

  Revision 1.138  2004/09/07 14:13:16  micha
  fix key handling of maskedit (inhibit backspace handling)

  Revision 1.137  2004/08/29 12:35:07  vincents
  Don't use handles in test if form is MainForm

  Revision 1.136  2004/08/27 19:06:40  micha
  select all text upon entry in edit control

  Revision 1.135  2004/08/27 15:13:03  micha
  fix choosing taskbar button "close" to close app

  Revision 1.134  2004/08/25 15:04:44  micha
  use new lcl interface methods instead of messages (for win32; twsbitbtn)

  Revision 1.133  2004/08/24 15:51:49  micha
  remove obsolete code which has not been enabled the last year

  Revision 1.132  2004/08/16 18:15:28  vincents
  The taskbar button is now hidden, if the main form is hidden

  Revision 1.131  2004/08/04 07:32:01  micha
  fix win32 keyhandling, send cn_char

  Revision 1.130  2004/08/01 14:21:06  micha
  better pre/post-intf key handling

  Revision 1.129  2004/07/27 00:07:48  marc
  * Fixed disabled state of bitbtns

  Revision 1.128  2004/07/15 10:43:39  mattias
  added TCustomButton, TCustomBitBtn, TCustomSpeedButton

  Revision 1.127  2004/07/12 13:04:14  micha
  cleanup: move default handling to DefaultHandler

  Revision 1.126  2004/07/07 08:15:28  micha
  fix not listening to Key := #0 in WM_CHAR message

  Revision 1.125  2004/07/02 20:24:51  micha
  fix wm_getdlgcode to get default code, then let the user override

  Revision 1.124  2004/07/01 20:42:11  micha
  implement better ExecuteXXAction design; break dependency on TButton class in TCustomForm

  Revision 1.123  2004/06/30 10:38:51  micha
  fix setcursor to only change cursor within client area, not on scrollbars etc.

  Revision 1.122  2004/06/29 14:38:28  micha
  fix default button notification win32 intf

  Revision 1.121  2004/06/29 10:23:00  micha
  fix cnkeydown to check wm_getdlgcode result
  fix win32 intf to also send wm_keydown of cn_keydown wasn't processed

  Revision 1.120  2004/06/20 20:36:55  micha
  remove old obsolete/commented toolbutton code
  rename lazarusform classname to window, because we use it for panels, notebookpages, etc too

  Revision 1.119  2004/06/20 13:58:15  micha
  fix combobox edit being gray

  Revision 1.118  2004/06/19 15:10:04  micha
  fix spinedit not firing onchange event

  Revision 1.117  2004/06/18 20:47:33  vincents
  fixed pasting from clipboard

  Revision 1.116  2004/06/18 19:55:43  micha
  fix xp themes drawing image on bitbtn

  Revision 1.115  2004/06/17 21:33:01  micha
  fix scroll message handling for comboboxes

  Revision 1.114  2004/06/15 15:37:29  micha
  fix groupbox background erasing

  Revision 1.113  2004/06/14 12:54:02  micha
  fix designer cursor to not set Form.Cursor directly

  Revision 1.112  2004/06/13 13:30:21  micha
  fix wm_setcursor to return TRUE if processed

  Revision 1.111  2004/06/10 13:26:10  micha
  try fix LM_ERASEBKGND bug: not responding to user Result

  Revision 1.110  2004/06/02 19:32:05  vincents
  fix out of bounds error, when canceling combobox selection

  Revision 1.109  2004/05/30 20:17:55  vincents
  changed radiobutton style to BS_RADIOBUTTON to prevent test program from hanging.

  Revision 1.108  2004/05/29 11:45:19  micha
  cleanup lcl<->win32 bounds code, remove duplicate code

  Revision 1.107  2004/05/23 13:35:19  micha
  fix multiple mouse wheel messages

  Revision 1.106  2004/05/20 21:28:54  marc
  * Fixed win32 listview

  Revision 1.105  2004/05/14 21:37:29  vincents
  combobox change event after selecting in list fixed

  Revision 1.104  2004/05/14 17:48:39  micha
  fix itemheight of listbox, handle measureitem message

  Revision 1.103  2004/05/13 12:17:00  micha
  fixes to designer window mouse/key handling:
  - capture all events
  - resizing window on which the overlay is, resizes the overlay too

  Revision 1.102  2004/05/12 15:43:48  micha
  fix key handling to fire CN_KEYxxx

  Revision 1.101  2004/05/08 07:21:10  micha
  fix closeup event; set text manually because windows has only set itemindex, but not the text yet

  Revision 1.100  2004/04/11 10:19:28  micha
  cursor management updated:
  - lcl notifies interface via WSControl.SetCursor of changes
  - fix win32 interface to respond to wm_setcursor callback and set correct cursor

  Revision 1.99  2004/04/10 17:54:52  micha
  - added: [win32] mousewheel default handler sends scrollbar messages
  - fixed: lmsetcursor; partial todo

  Revision 1.98  2004/03/05 18:37:46  micha
  prevent selection invalid page

  Revision 1.97  2004/03/05 12:16:08  micha
  fix designer (overlay) window transparency issue
  fix releasedesignerdc to use correct window

  Revision 1.96  2004/03/05 01:04:21  marc
  * Renamed TWin32Object to TWin32WidgetSet

  Revision 1.95  2004/03/04 21:16:15  micha
  remove workaround; fpc bug fixed

  Revision 1.94  2004/02/26 21:01:42  micha
  fixed: focussing issue combobox

  Revision 1.93  2004/02/21 10:11:36  micha
    1. pressing the Return key in ObjectInspector when editing a value throws an exception
    2. placing TPairSplitter component on the form produces "Division by zero"

  Revision 1.92  2004/02/12 18:03:15  mattias
  fixed combobox enabled  from Vincent

  Revision 1.91  2004/01/29 20:57:57  micha
  fixed: use result of message passed to lcl (fixes synedit editing)

  Revision 1.90  2004/01/20 22:14:27  micha
  REVERTED: "try register globally unique properties"; implemented new WindowFromPoint not returning window if from different process (tip from vincent)

  Revision 1.89  2004/01/20 10:26:41  micha
  try register globally unique properties

  Revision 1.88  2004/01/19 21:51:50  micha
  prevent premature window destroy by default wm_close

  Revision 1.87  2004/01/12 08:20:50  micha
  implement overlay window for designer

  Revision 1.86  2004/01/07 18:05:46  micha
  add TWinControl.DoubleBuffered property which is a hint for the interface to do double-buffering for this control

  Revision 1.85  2004/01/03 21:06:06  micha
  - fix win32/checklistbox
  - implement proper lcl to interface move/size notify via setwindowpos
  - fix treeview to use inherited canvas from customcontrol
  - implement double buffering in win32

  Revision 1.84  2004/01/03 11:57:48  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.83  2003/12/30 08:38:03  micha
  enable selection of checklistbox items (from vincent)

  Revision 1.82  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.81  2003/12/27 16:26:55  micha
  remove redundant window property "lazarus" (from martin)

  Revision 1.80  2003/12/26 15:29:33  mattias
  various combobox and other fixes from  Vincent

  Revision 1.79  2003/12/23 16:49:48  micha
  fix mousewheel message, short signed int params

  Revision 1.78  2003/12/19 21:34:53  micha
  fix compiler problem; wrong code for constants

  Revision 1.77  2003/12/19 18:18:17  micha
  fix window activation z-order

  Revision 1.76  2003/12/17 16:06:43  micha
  bring windows to top on application activation

  Revision 1.75  2003/12/15 21:57:16  micha
  checklistbox, implement object+checked; from vincent

  Revision 1.74  2003/12/13 19:44:42  micha
  hintwindow, color, rectangle size fixes

  Revision 1.73  2003/11/28 19:54:42  micha
  fpc 1.0.10 compatibility

  Revision 1.72  2003/11/25 21:20:38  micha
  implement tchecklistbox

  Revision 1.71  2003/11/25 14:21:28  micha
  new api lclenable,checkmenuitem according to list

  Revision 1.70  2003/11/21 20:32:01  micha
  cleanups; wm_hscroll/wm_vscroll fix

  Revision 1.69  2003/11/18 07:20:39  micha
  added "included by" notice at top of file

  Revision 1.68  2003/11/10 19:05:44  mattias
  fixed some bugs by Andreas

  Revision 1.67  2003/11/08 17:41:03  micha
  compiler warning cleanups

  Revision 1.66  2003/11/03 16:57:47  peter
    * change $ifdef ver1_1 to $ifndef ver1_0 so it works also with
      fpc 1.9.x

  Revision 1.65  2003/10/21 15:06:27  micha
  spinedit fix; variables cleanup

  Revision 1.64  2003/10/16 20:35:37  mattias
  added missing definitions

  Revision 1.63  2003/10/06 10:50:10  mattias
  added recursion to InvalidateClientRectCache

  Revision 1.62  2003/10/02 11:18:09  mattias
  clean ups from Karl

  Revision 1.61  2003/09/27 09:52:44  mattias
  TScrollBox for win32 intf from Karl

  Revision 1.60  2003/09/24 20:43:27  mattias
  fixed wordwrap from Micha

  Revision 1.59  2003/09/21 10:42:48  mattias
  implemented TBitBtn Text+Caption from Micha

  Revision 1.58  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.57  2003/09/09 11:11:02  mattias
  fixed win32 intf showmoadl and lazconf

  Revision 1.56  2003/09/08 12:21:48  mattias
  added fpImage reader/writer hooks to TBitmap

  Revision 1.55  2003/08/25 16:18:15  mattias
  fixed background color of TPanel and clicks of TSpeedButton from Micha

  Revision 1.54  2003/08/23 21:17:09  mattias
  several fixes for the win32 intf, added pending OnResize events

  Revision 1.53  2003/08/17 12:51:35  mattias
  added directory selection dialog from Vincent

  Revision 1.52  2003/08/17 12:26:00  mattias
  fixed parts of the win32 intf size system

  Revision 1.51  2003/08/13 21:23:10  mattias
  fixed log

  Revision 1.50  2003/08/13 16:26:07  mattias
  fixed combobox height from Karl

  Revision 1.49  2003/08/12 14:02:54  mattias
  fixed keypress/keyup, createcaret on synedit focus

  Revision 1.48  2003/08/11 20:18:46  mattias
  fixed position of control in TGroupBox from Micha

  Revision 1.47  2003/08/11 18:10:41  mattias
  fixed combobox height from Micha

  Revision 1.46  2003/08/09 16:30:33  mattias
  fixed LM_ShowModal for win32 intf from Karl

  Revision 1.45  2003/07/31 19:56:50  mattias
  fixed double messages SETLabel

  Revision 1.44  2003/07/26 13:48:45  mattias
  fixed other messages bigger than TLMessage

  Revision 1.43  2003/07/26 13:26:56  mattias
  fixed WindowProc

  Revision 1.42  2003/07/26 10:30:44  mattias
  rewritten WM_COMMAND by Micha

  Revision 1.41  2003/07/20 06:27:19  mattias
  fixed GetWindowRelativePosition

  Revision 1.40  2003/07/07 08:31:54  mattias
  added an InvalidateClientRectCache to WM_SIZE

  Revision 1.39  2003/07/07 07:59:34  mattias
  made Size_SourceIsInterface a flag

  Revision 1.38  2003/07/06 21:35:55  mattias
  fixed typo

  Revision 1.37  2003/07/06 20:40:34  mattias
  TWinControl.WmSize/Move now updates interface messages smarter

  Revision 1.36  2003/07/04 10:12:16  mattias
  added default message handler to win32 interface

  Revision 1.35  2003/07/03 21:39:44  mattias
  fixed remove props on destroy from Micha

  Revision 1.34  2003/07/03 08:05:53  mattias
  fixed Criticalsection from Vincent

  Revision 1.33  2003/07/02 20:18:28  mattias
  more cleanups from Micha

  Revision 1.32  2003/07/01 22:02:55  mattias
  fixed formstyle and redrawing from Micha

  Revision 1.31  2003/06/29 20:07:07  mattias
  fixed using DC from Micha

  Revision 1.30  2003/06/28 16:20:19  mattias
  fixed some win32 intf warnings

  Revision 1.29  2003/06/28 06:33:11  mattias
  fixed control resizing from Karl Brandt

  Revision 1.28  2003/06/27 21:41:14  mattias
  fixed formresize from Micha

  Revision 1.27  2003/06/25 15:27:18  mattias
  fixed timer calling conventions from Micha

  Revision 1.26  2003/03/25 08:12:39  mattias
  patch from Martin Smat for menu items and default messages

  Revision 1.25  2003/03/18 18:23:07  mattias
  popupmenus for win32 intf from Martin Smat

  Revision 1.24  2003/01/27 11:25:40  mattias
  menu accelerator patch from Martin Smat

  Revision 1.23  2003/01/19 10:57:46  mattias
  fix WindowProc now react on menu item click from Martin

  Revision 1.22  2002/12/28 09:42:12  mattias
  toolbutton patch from Martin Smat

  Revision 1.21  2002/12/16 09:02:27  mattias
  applied win32 notebook patch from Vincent

  Revision 1.20  2002/12/09 17:53:22  mattias
  Patch from Martin for reszing windows

  Revision 1.19  2002/12/04 20:39:16  mattias
  patch from Vincent: clean ups and fixed crash on destroying window

  Revision 1.18  2002/11/26 20:51:05  mattias
  applied clipbrd patch from Vincent

  Revision 1.17  2002/11/23 13:48:48  mattias
  added Timer patch from Vincent Snijders

  Revision 1.16  2002/11/15 23:43:54  mattias
  applied patch from Karl Brandt

  Revision 1.15  2002/08/28 17:28:11  lazarus
  Keith: Win32 fixes.  Much appreciation to Markus Lüdin.

  Revision 1.14  2002/08/13 07:08:25  lazarus
  MG: added gdkpixbuf.pp and changes from Andrew Johnson

  Revision 1.13  2002/06/08 19:18:34  lazarus
  Keith: Fixed some bugs that were brought to my attention; fixed compilation problem.

  Revision 1.12  2002/05/10 07:43:48  lazarus
  MG: updated licenses

  Revision 1.11  2002/04/03 01:52:42  lazarus
  Keith: Removed obsolete code, in preperation of a pending TWin32Object cleanup

  Revision 1.10  2002/02/07 08:35:12  lazarus
  Keith: Fixed persistent label captions and a few less noticable things

  Revision 1.9  2002/02/04 10:54:33  lazarus
  Keith:
    * Fixes for Win32
    * Added new listviewtest.pp example

  Revision 1.8  2002/02/03 06:06:25  lazarus
  Keith: Fixed Win32 compilation problems

  Revision 1.7  2002/01/31 09:32:07  lazarus
  Keith:
    * Open and save dialogs can now coexist in apps (however, only one of each type of common dialog can be used per app :( )
    * Fixed make all
    * Fixed crash in Windows 98/ME

  Revision 1.6  2002/01/25 19:42:56  lazarus
  Keith: Improved events and common dialogs on Win32

  Revision 1.5  2002/01/25 13:22:56  lazarus
  Keith: Added initial support for events

  Revision 1.4  2002/01/17 03:17:44  lazarus
  Keith: Fixed TCustomPage creation

  Revision 1.3  2002/01/05 13:16:08  lazarus
  MG: win32 interface update from Keith Bowes

  Revision 1.2  2001/08/02 12:58:35  lazarus
  MG: win32 interface patch from Keith Bowes

  Revision 1.1  2000/07/13 10:28:30  michael
  + Initial import
}
