{%MainUnit win32int.pp}

{
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
{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

type
  TWinControlAccess = class(TWinControl);
{*************************************************************}
{            callback routines                                }
{*************************************************************}

procedure PrepareSynchronize;
begin
  TWin32WidgetSet(WidgetSet).HandleWakeMainThread(nil);
end;

{-----------------------------------------------------------------------------
  Function: PropEnumProc
  Params: Window - The window with the property
          Str    - The property name
          Data   - The property value
  Returns: Whether the enumeration should continue

  Enumerates and removes properties for the target window
 -----------------------------------------------------------------------------}
function PropEnumProc(Window: Hwnd; Str: PChar; Data: Handle): LongBool; stdcall;
begin
  Result:=false;
  if PtrUInt(Str) <= $FFFF then exit; // global atom handle
  Assert(False, 'Trace:PropEnumProc - Start');
  Assert(False, Format('Trace:PropEnumProc - Property %S (with value 0x%X) from window 0x%X removed',
    [String(Str), Data, Window]));
  RemoveProp(Window, Str);
  Result := True;
  Assert(False, 'Trace:PropEnumProc - Exit');
end;

function WndClassName(Wnd: HWND): String; inline;
var
  winClassName: array[0..19] of char;
begin
  GetClassName(Wnd, @winClassName, 20);
  Result := winClassName;
end;

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
  
  function IsComboboxAndHasEdit(Window: HWnd): Boolean;
  var
    Info: TComboboxInfo;
  begin
    Result := WndClassName(Window) = ComboboxClsName;
    if not Result then
      Exit;
    Info.cbSize := SizeOf(Info);
    Win32Extra.GetComboBoxInfo(Window, @Info);
    Result := (Info.hwndItem <> 0) and GetWindowInfo(Info.hwndItem)^.isComboEdit;
  end;
var
  PrevWndProc: Windows.WNDPROC;
{$ifdef MSG_DEBUG}
  depthLen: integer;
{$endif}
  setComboWindow: boolean;
begin
{$ifdef MSG_DEBUG}
  depthLen := Length(MessageStackDepth);
  if depthLen > 0 then
    MessageStackDepth[depthLen] := '#';
{$endif}
  PrevWndProc := GetWindowInfo(Window)^.DefWndProc;
  if (PrevWndProc = nil) or (PrevWndProc = @WindowProc) // <- prevent recursion
  then begin
    if UnicodeEnabledOS
    then Result := Windows.DefWindowProcW(Window, Msg, WParam, LParam)
    else Result := Windows.DefWindowProc(Window, Msg, WParam, LParam)
  end
  else begin
    // combobox child edit weirdness: combobox handling WM_SIZE will compare text
    // to list of strings, and if appears in there, will set the text, and select it
    // WM_GETTEXTLENGTH, WM_GETTEXT, WM_SETTEXT, EM_SETSEL
    // combobox sends WM_SIZE to itself indirectly, check recursion
    setComboWindow :=
        (Msg = WM_SIZE) and
        (ComboBoxHandleSizeWindow = 0) and
        IsComboboxAndHasEdit(Window);
    if setComboWindow then
      ComboBoxHandleSizeWindow := Window;
    Result := Windows.CallWindowProc(PrevWndProc, Window, Msg, WParam, LParam);
    if setComboWindow then
      ComboBoxHandleSizeWindow := 0;
  end;
{$ifdef MSG_DEBUG}
  if depthLen > 0 then
    MessageStackDepth[depthLen] := ' ';
{$endif}
end;

type
  TEraseBkgndCommand = (ecDefault, ecDiscard, ecDiscardNoRemove, ecDoubleBufferNoRemove);
const
  EraseBkgndStackMask = $3;
  EraseBkgndStackShift = 2;
var
  EraseBkgndStack: dword = 0;

{$ifdef MSG_DEBUG}
function EraseBkgndStackToString: string;
var
  I: dword;
begin
  SetLength(Result, 8);
  for I := 0 to 7 do
    Result[8-I] := char(ord('0') + ((EraseBkgndStack shr (I*2)) and $3));
end;
{$endif}

procedure PushEraseBkgndCommand(Command: TEraseBkgndCommand);
begin
{$ifdef MSG_DEBUG}
  case Command of
    ecDiscard: DebugLn(MessageStackDepth,
      ' *forcing next WM_ERASEBKGND to discard message');
    ecDiscardNoRemove: DebugLn(MessageStackDepth,
      ' *forcing next WM_ERASEBKGND to discard message, no remove');
    ecDoubleBufferNoRemove: DebugLn(MessageStackDepth,
      ' *forcing next WM_ERASEBKGND to use double buffer, after that, discard no remove');
  end;
  DebugLn(MessageStackDepth, ' *erasebkgndstack: ', EraseBkgndStackToString);
{$endif}
  EraseBkgndStack := (EraseBkgndStack shl EraseBkgndStackShift) or dword(Ord(Command));
end;

var
  DoubleBufferDC: HDC = 0;
  DoubleBufferBitmap: HBITMAP = 0;
  DoubleBufferBitmapWidth: integer = 0;
  DoubleBufferBitmapHeight: integer = 0;

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

function GetNeedParentPaint(AWindowInfo: PWindowInfo; AWinControl: TWinControl): boolean;
begin
  Result := AWindowInfo^.needParentPaint
    and ((AWinControl = nil) or not (csOpaque in AWinControl.ControlStyle));
end;

type
  TCustomListViewAccess = class(TCustomListView)
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
function
{$ifdef MSG_DEBUG}
  RealWindowProc
{$else}
  WindowProc
{$endif}
  (Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  LMessage: TLMessage;
  menuItem: TObject;
  menuHDC: HDC;
  PLMsg: PLMessage;
  R: TRect;
  P: TPoint;
  NewLeft, NewTop, NewWidth, NewHeight: integer;
  lWinControl, ChildWinControl: TWinControl;
  ChildWindowInfo: PWindowInfo;
  TargetObject: TObject;
  WinProcess: Boolean;
  NotifyUserInput: Boolean;
  OverlayWindow: HWND;
  TargetWindow: HWND;
  eraseBkgndCommand: TEraseBkgndCommand;
  WindowInfo: PWindowInfo;
  Flags: dword;
  WindowDC: HDC;

  LMScroll: TLMScroll; // used by WM_HSCROLL
  LMKey: TLMKey; // used by WM_KEYDOWN WM_KEYUP
  LMChar: TLMChar; // used by WM_CHAR
  LMMouse: TLMMouse; // used by WM_LBUTTONDBLCLK
  LMMouseMove: TLMMouseMove; // used by WM_MOUSEMOVE
  LMMouseEvent: TLMMouseEvent; // used by WM_MOUSEWHEEL
  LMMove: TLMMove; // used by WM_MOVE
  LMNotify: TLMNotify; // used by WM_NOTIFY
  DrawListItemStruct: TDrawListItemStruct; //used by WM_DRAWITEM
  CancelEndSession : Boolean;//use by WM_QUERYENDSESSION

  NMHdr: PNMHdr absolute LParam; // used by WM_NOTIFY
  TmpSize: TSize; // used by WM_MEASUREITEM
  Info: TComboboxInfo;

  function ShowHideTabPage(NotebookHandle: HWnd; Showing: boolean): integer;
  var
    NoteBook: TCustomNotebook;
    PageIndex, Flags: Integer;
    PageHandle: HWND;
  begin
    Notebook := GetWindowInfo(NotebookHandle)^.WinControl as TCustomNotebook;
    PageIndex := Windows.SendMessage(NotebookHandle, TCM_GETCURSEL, 0, 0);
    PageIndex := NotebookPageRealToLCLIndex(Notebook, PageIndex);
    if PageIndex = -1 then exit;
    PageHandle := Notebook.CustomPage(PageIndex).Handle;
    if Showing then
      Flags := SW_SHOW
    else
      Flags := SW_HIDE;
    Windows.ShowWindow(PageHandle, Flags);
    Windows.RedrawWindow(PageHandle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_ERASE);
    if Showing then
      NotebookFocusNewControl(Notebook, PageIndex);
    Result := PageIndex;
  end;

  function GetMenuParent(ASearch, AParent: HMENU): HMENU;
  var
    c, i: integer;
    sub: HMENU;
  begin
    c := GetMenuItemCount(AParent);
    for i:= 0 to c - 1 do
    begin
      sub := GetSubMenu(AParent, i);
      if sub = ASearch
      then begin
        Result := AParent;
        Exit;
      end;

      Result := GetMenuParent(ASearch, sub);
      if Result <> 0 then Exit;
    end;
    Result := 0;
  end;

  function GetPopMenuItemObject: TObject;
  var
    MainMenuHandle: HMENU;
    MenuInfo: MENUITEMINFO;
  begin
    MenuInfo.cbSize := MMenuItemInfoSize;
    MenuInfo.fMask := MIIM_DATA;

    MainMenuHandle := GetMenuParent(HMENU(WParam), GetMenu(Window));
    if GetMenuItemInfo(MainMenuHandle, LOWORD(LParam), true, @MenuInfo)
    then Result := TObject(MenuInfo.dwItemData)
    else Result := nil;
  end;

  function GetMenuItemObject: TObject;
  var
    MenuInfo: MENUITEMINFO;
    PopupMenu: TPopupMenu;
  begin
    // first we have to decide if the command is from a popup menu
    // or from the window main menu
    // if the 'PopupMenu' property exists, there is a big probability
    // that the command is from a popup menu
    PopupMenu := WindowInfo^.PopupMenu;
    if PopupMenu <> nil
    then begin
      WindowInfo^.PopupMenu := nil;
      Result := PopupMenu.FindItem(LOWORD(WParam), fkCommand);
      if Result <> nil then Exit;
    end;

    // nothing found, process main menu
    MenuInfo.cbSize := MMenuItemInfoSize;
    MenuInfo.fMask := MIIM_DATA;

    if GetMenuItemInfo(GetMenu(Window), LOWORD(WParam), false, @MenuInfo)
    then Result := TObject(MenuInfo.dwItemData)
    else Result := nil;
  end;

  function GetIsNativeControl(AWindow: HWND): Boolean;
  begin
    Result := WndClassName(AWindow) <> ClsName;
  end;
  
  procedure SendPaintMessage(ControlDC: HDC);
  var
    DC: HDC;
    DoubleBufferBitmapOld: HBITMAP;
    PaintRegion: HRGN;
    PS : TPaintStruct;
    PaintMsg: TLMPaint;
    ORect: TRect;
    WindowOrg: Windows.POINT;
{$ifdef DEBUG_DOUBLEBUFFER}
    ClipBox: Windows.RECT;
{$endif}
    ParentPaintWindow: HWND;
    WindowWidth, WindowHeight: Integer;
    DCIndex: integer;
    parLeft, parTop: integer;
    useDoubleBuffer: boolean;
    isNotebook: boolean;
    isNativeControl: boolean;
    needParentPaint: boolean;
    lNotebookFound: boolean;
  begin
    // note: ignores the received DC
    // do not use default deliver message
    if lWinControl = nil then
    begin
      lWinControl := GetWindowInfo(Window)^.PWinControl;
      if lWinControl = nil then exit;
    end;

    // create a paint message
    isNotebook := ThemeServices.ThemesEnabled and (WndClassName(Window) = TabControlClsName);
    isNativeControl := GetIsNativeControl(Window);
    ParentPaintWindow := 0;
    needParentPaint := GetNeedParentPaint(WindowInfo, lWinControl);
    // if needParentPaint and not isTabPage then background will be drawn in
    // WM_ERASEBKGND and WM_CTLCOLORSTATIC for native controls
    // sent by default paint handler
    if WindowInfo^.isTabPage or (needParentPaint
      and (not isNativeControl or (ControlDC <> 0))) then
    begin
      ParentPaintWindow := Window;
      lNotebookFound := false;
      while (ParentPaintWindow <> 0) and not lNotebookFound do
      begin
        // notebook is parent of window that has istabpage
        if GetWindowInfo(ParentPaintWindow)^.isTabPage then
          lNotebookFound := true;
        ParentPaintWindow := Windows.GetParent(ParentPaintWindow);
      end;
    end;

    // if painting background of some control for tabpage, don't handle erase background
    // in parent of tabpage
    if WindowInfo^.isTabPage then
      PushEraseBkgndCommand(ecDiscard);

    // check if double buffering is requested
    useDoubleBuffer := (ControlDC = 0) and (lWinControl.DoubleBuffered
      or ThemeServices.ThemesEnabled);
{$ifdef MSG_DEBUG}
    if useDoubleBuffer and (DoubleBufferDC <> 0) then
    begin
      DebugLn('ERROR: RECURSIVE PROBLEM! DOUBLEBUFFERED PAINT');
      useDoubleBuffer := false;
    end;
{$endif}
    if useDoubleBuffer then
    begin
      DoubleBufferDC := Windows.CreateCompatibleDC(0);
      GetWindowSize(Window, WindowWidth, WindowHeight);
      if (DoubleBufferBitmapWidth < WindowWidth) or (DoubleBufferBitmapHeight < WindowHeight) then
      begin
        DC := Windows.GetDC(0);
        if DoubleBufferBitmap <> 0 then
          Windows.DeleteObject(DoubleBufferBitmap);
        DoubleBufferBitmapWidth := WindowWidth;
        DoubleBufferBitmapHeight := WindowHeight;
        DoubleBufferBitmap := Windows.CreateCompatibleBitmap(DC, WindowWidth, WindowHeight);
        Windows.ReleaseDC(0, DC);
      end;
      DoubleBufferBitmapOld := Windows.SelectObject(DoubleBufferDC, DoubleBufferBitmap);
      PaintMsg.DC := DoubleBufferDC;
    end;

{$ifdef MSG_DEBUG}
    if useDoubleBuffer then
      DebugLn(MessageStackDepth, ' *double buffering on DC: ', IntToHex(DoubleBufferDC, 8))
    else
      DebugLn(MessageStackDepth, ' *painting, but not double buffering');
{$endif}

    WinProcess := false;
    try
      if ControlDC = 0 then
      begin
        // ignore first erase background on themed control, paint will do everything
        if ThemeServices.ThemesEnabled then
          PushEraseBkgndCommand(ecDoubleBufferNoRemove);
        DC := Windows.BeginPaint(Window, @PS);
        if ThemeServices.ThemesEnabled then
          EraseBkgndStack := EraseBkgndStack shr EraseBkgndStackShift;
        if useDoubleBuffer then
        begin
{$ifdef DEBUG_DOUBLEBUFFER}
          ORect.Left := 0;
          ORect.Top := 0;
          ORect.Right := DoubleBufferBitmapWidth;
          ORect.Bottom := DoubleBufferBitmapHeight;
          Windows.FillRect(DoubleBufferDC, ORect, GetSysColorBrush(COLOR_DESKTOP));
{$endif}
          PaintRegion := CreateRectRgn(0, 0, 1, 1);
          if GetRandomRgn(DC, PaintRegion, SYSRGN) = 1 then
          begin
            // winnt returns in screen coordinates
            // win9x returns in window coordinates
            if Win32Platform = VER_PLATFORM_WIN32_NT then
            begin
              WindowOrg.X := 0;
              WindowOrg.Y := 0;
              Windows.ClientToScreen(Window, WindowOrg);
              Windows.OffsetRgn(PaintRegion, -WindowOrg.X, -WindowOrg.Y);
            end;
            Windows.SelectClipRgn(DoubleBufferDC, PaintRegion);
          end;
{$ifdef DEBUG_DOUBLEBUFFER}
          Windows.GetClipBox(DoubleBufferDC, ClipBox);
          DebugLn('Double buffering in DC ', IntToHex(DoubleBufferDC, 8), ' with clipping rect (',
            IntToStr(ClipBox.Left), ',', IntToStr(ClipBox.Top), ';',
            IntToStr(ClipBox.Right), ',', IntToStr(ClipBox.Bottom), ')');
{$endif}
          // a copy of the region is selected into the DC, so we
          // can free our region immediately
          Windows.DeleteObject(PaintRegion);
        end;
      end else begin
        DC := ControlDC;
        PaintRegion := 0;
      end;
      if ParentPaintWindow <> 0 then
        GetWin32ControlPos(Window, ParentPaintWindow, parLeft, parTop);
      //Is not necessary to check the result of GetLCLClientBoundsOffset since
      //the false condition (lWincontrol = nil or lWincontrol <> TWinControl) is never met
      //The rect is always initialized with 0
      GetLCLClientBoundsOffset(lWinControl, ORect);
      PaintMsg.Msg := LM_PAINT;
      PaintMsg.PaintStruct := @PS;
      if not useDoubleBuffer then
        PaintMsg.DC := DC;
      if not needParentPaint and not isNotebook then
      begin
        // send through message to allow message override, moreover use SendMessage
        // to allow subclass window proc override this message too
        Include(TWinControlAccess(lWinControl).FWinControlFlags, wcfEraseBackground);
        SendMessage(lWinControl.Handle, WM_ERASEBKGND, Windows.WPARAM(PaintMsg.DC), 0);
        Exclude(TWinControlAccess(lWinControl).FWinControlFlags, wcfEraseBackground);
      end;
      if ParentPaintWindow <> 0 then
      begin
{$ifdef MSG_DEBUG}
        DebugLn(MessageStackDepth, ' *painting background by sending paint message to parent window ',
          IntToHex(ParentPaintWindow, 8));
{$endif}
        // tabpage parent and got a dc to draw in, divert paint to parent
        DCIndex := Windows.SaveDC(PaintMsg.DC);
        MoveWindowOrgEx(PaintMsg.DC, -parLeft, -parTop);
        Windows.SendMessage(ParentPaintWindow, WM_PAINT, Windows.WParam(PaintMsg.DC), 0);
        Windows.RestoreDC(PaintMsg.DC, DCIndex);
      end;
      if (ControlDC = 0) or not needParentPaint then
      begin
        DCIndex := Windows.SaveDC(PaintMsg.DC);
        MoveWindowOrgEx(PaintMsg.DC, ORect.Left, ORect.Top);
{$ifdef DEBUG_DOUBLEBUFFER}
        Windows.GetClipBox(PaintMsg.DC, ClipBox);
        DebugLn('LCL Drawing in DC ', IntToHex(PaintMsg.DC, 8), ' with clipping rect (',
          IntToStr(ClipBox.Left), ',', IntToStr(ClipBox.Top), ';',
          IntToStr(ClipBox.Right), ',', IntToStr(ClipBox.Bottom), ')');
{$endif}
        DeliverMessage(lWinControl, PaintMsg);
        Windows.RestoreDC(PaintMsg.DC, DCIndex);
      end;
      if useDoubleBuffer then
        Windows.BitBlt(DC, 0, 0, WindowWidth, WindowHeight, DoubleBufferDC, 0, 0, SRCCOPY);
      if ControlDC = 0 then
        Windows.EndPaint(Window, @PS);
    finally
      if useDoubleBuffer then
      begin
        SelectObject(DoubleBufferDC, DoubleBufferBitmapOld);
        DeleteDC(DoubleBufferDC);
        DoubleBufferDC := 0;
{$ifdef DEBUG_DOUBLEBUFFER}
        if CopyBitmapToClipboard then
        begin
//          Windows.OpenClipboard(0);
//          Windows.EmptyClipboard;
//          Windows.SetClipboardData(CF_BITMAP, DoubleBufferBitmap);
//          Windows.CloseClipboard;
          CopyBitmapToClipboard := false;
        end;
{$endif}
      end;
    end;
  end;

  procedure SendParentPaintMessage(Window, Parent: HWND; ControlDC: HDC);
  begin
    GetWin32ControlPos(Window, Parent, P.X, P.Y);
    MoveWindowOrgEx(ControlDC, -P.X, -P.Y);
    SendPaintMessage(ControlDC);
    MoveWindowOrgEx(ControlDC, P.X, P.Y);
  end;

  procedure CheckListBoxLButtonDown;
  var
    I: Integer;
    ItemRect: Windows.Rect;
    MousePos: Windows.Point;
    Message: TLMessage;
  begin
    MousePos.X := LMMouse.Pos.X;
    MousePos.Y := LMMouse.Pos.Y;
    for I := 0 to Windows.SendMessage(Window, LB_GETCOUNT, 0, 0) - 1 do
    begin
      Windows.SendMessage(Window, LB_GETITEMRECT, I, PtrInt(@ItemRect));
      ItemRect.Right := ItemRect.Left + ItemRect.Bottom - ItemRect.Top;
      if Windows.PtInRect(ItemRect, MousePos) then
      begin
        // item clicked: toggle
        if I < TCheckListBox(lWinControl).Items.Count then
        begin
          TCheckListBox(lWinControl).Toggle(I);
          Message.Msg := LM_CHANGED;
          Message.WParam := I;
          DeliverMessage(lWinControl, Message);
        end;
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

  procedure DestroyFloatSpinEditBuddy(SpinEditHandle: HWND);
  var
    Buddy: HWND;
  begin
    Buddy := SendMessage(SpinEditHandle, UDM_GETBUDDY, 0, 0);
    DestroyWindow(Buddy);
  end;

  procedure EnableFloatSpinEditBuddy(SpinEditHandle: HWND; Enable: boolean);
  var
    Buddy: HWND;
  begin
    Buddy := SendMessage(SpinEditHandle, UDM_GETBUDDY, 0, 0);
    Windows.EnableWindow(Buddy, Enable);
  end;
  
  procedure EnableChildWindows(WinControl: TWinControl; Enable: boolean);
  var
    i: integer;
    ChildControl: TWinControl;
  begin
    for i := 0 to WinControl.ControlCount-1 do
    begin
      if WinControl.Controls[i] is TWinControl then
      begin
        ChildControl := TWinControl(WinControl.Controls[i]);
        if Enable then
        begin
           if ChildControl.Enabled then
             EnableWindow(ChildControl.Handle, true);
        end
        else
          EnableWindow(ChildControl.Handle, false);

        EnableChildWindows(ChildControl, Enable);
      end;
    end;
  end;

  procedure DisposeComboEditWindowInfo(ComboBox: TCustomComboBox);
  var
    Buddy: HWND;
    Info: TComboboxInfo;
  begin
    Info.cbSize := SizeOf(Info);
    Win32Extra.GetComboBoxInfo(Combobox.Handle, @Info);
    Buddy := Info.hwndItem;
    if Buddy <> HWND(nil) then
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
    if lWinControl is TCustomTrackBar then
    begin
      LMessage.Msg := LM_CHANGED;
      exit;
    end;

    PLMsg:=@LMScroll;
    with LMScroll do
    begin
      Msg := LMsg;
      ScrollCode := LOWORD(WParam);
      SmallPos := 0;
      ScrollBar := ScrollbarHandle;
      Pos := 0;
    end;

    if not (LOWORD(WParam) in [SB_THUMBTRACK, SB_THUMBPOSITION])
    then begin
      WindowInfo^.TrackValid := False;
      Exit;
    end;

    // Note on thumb tracking
    // When using the scrollwheel, windows sends SB_THUMBTRACK
    // messages, but only when scroll.max < 32K. So in that case
    // Hi(WParam) won't cycle.
    // When ending scrollbar tracking we also get those
    // messages. Now Hi(WParam) is cycling.
    // To get the correct value you need to use GetScrollInfo.
    //
    // Now there is a problem. GetScrollInfo returns always the old
    // position. So in case we get track messages, we'll keep the
    // last trackposition.
    // To get the correct position, we use the most significant
    // part of the last known value (or the value returned by
    // ScrollInfo). The missing least significant part is given
    // by Hi(WParam), since it is cycling, the or of both will give
    // the position
    // This only works if the difference between the last pos and
    // the new pos is < 64K, so it might fail if we don't get track
    // messages
    // MWE.


    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    if LOWORD(WParam) = SB_THUMBTRACK
    then begin
      ScrollInfo.fMask := SIF_TRACKPOS;
      // older windows versions may not support trackpos, so fill it with some default
      if WindowInfo^.TrackValid
      then ScrollInfo.nTrackPos := (WindowInfo^.TrackPos and $FFFF0000) or HIWORD(WParam)
      else ScrollInfo.nTrackPos := HIWORD(WParam);
    end
    else begin
      ScrollInfo.fMask := SIF_POS;
      ScrollInfo.nPos := HIWORD(WParam);
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

    if LOWORD(WParam) = SB_THUMBTRACK
    then begin
      LMScroll.Pos := ScrollInfo.nTrackPos;
      WindowInfo^.TrackPos := ScrollInfo.nTrackPos;
      WindowInfo^.TrackValid := True;
    end
    else begin
      if WindowInfo^.TrackValid
      then LMScroll.Pos := (WindowInfo^.TrackPos and $FFFF0000) or HIWORD(WParam)
      else LMScroll.Pos := (ScrollInfo.nPos and $FFFF0000) or HIWORD(WParam);
    end;

    if LMScroll.Pos < High(LMScroll.SmallPos)
    then LMScroll.SmallPos := LMScroll.Pos
    else LMScroll.SmallPos := High(LMScroll.SmallPos);
  end;

  procedure HandleSetCursor;
  var
    lControl: TControl;
    BoundsOffset: TRect;
    ACursor: TCursor;
  begin
    if (lWinControl <> nil) and not (csDesigning in lWinControl.ComponentState)
      and (LOWORD(LParam) = HTCLIENT) then
    begin
      Windows.GetCursorPos(Windows.POINT(P));
      Windows.ScreenToClient(Window, Windows.POINT(P));
      if GetLCLClientBoundsOffset(lWinControl, BoundsOffset) then
      begin
        Dec(P.X, BoundsOffset.Left);
        Dec(P.Y, BoundsOffset.Top);
      end;
      ACursor := Screen.Cursor;
      if ACursor = crDefault then
      begin
        // statictext controls do not get WM_SETCURSOR messages...
        lControl := lWinControl.ControlAtPos(P, [capfOnlyClientAreas,
          capfAllowWinControls, capfHasScrollOffset]);
        if lControl = nil then
          lControl := lWinControl;
        if lControl.Cursor <> crDefault then
          ACursor := lControl.Cursor;
      end;
      if ACursor <> crDefault then
      begin
        Windows.SetCursor(Screen.Cursors[ACursor]);
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
    case (WParam and $FFF0) of
      SC_KEYMENU:
        if (lWinControl <> nil) and (lParam <> VK_SPACE) then
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
        end;

      SC_MINIMIZE:
        begin

          if (Application <> nil) and (lWinControl <> nil)
              and (Application.MainForm <> nil)
              and (Application.MainForm = lWinControl) then
                Window := TWin32WidgetSet(WidgetSet).AppHandle;//redirection

         if (Window = TWin32WidgetSet(WidgetSet).AppHandle)
              and (Application <> nil)
              and (Application.MainForm<>nil) then
          begin
              Windows.SetWindowPos(Window, HWND_TOP,
                Application.MainForm.Left, Application.MainForm.Top,
                Application.MainForm.Width, 0, SWP_NOACTIVATE);
                if Application.MainForm.HandleAllocated then
                 Windows.ShowWindow(Application.MainForm.Handle, SW_HIDE);

              Application.IntfAppMinimize;
          end;
        end;

      SC_RESTORE:
        begin

         if (Window = TWin32WidgetSet(WidgetSet).AppHandle)
              and (Application <> nil)
              and (Application.MainForm<>nil)
              and Application.MainForm.HandleAllocated then
          begin
            PLMsg^.Result := Windows.DefWindowProc(Window, WM_SYSCOMMAND, WParam, LParam);
            Windows.ShowWindow(Application.MainForm.Handle, SW_RESTORE);
            if Windows.IsWindowEnabled(Application.MainForm.Handle) then
              Windows.SetActiveWindow(Application.MainForm.Handle);
            WinProcess := false;

            Application.IntfAppRestore;
          end;
        end;
    end;
  end;

  function IsComboEditSelection: boolean;
  begin
    Result := WindowInfo^.isComboEdit and (ComboBoxHandleSizeWindow = Windows.GetParent(Window));
  end;

  procedure HandleSpinEditChange(ASpinEdit: TCustomFloatSpinEdit);
  var
    lWindowInfo: PWindowInfo;
  begin
    lWindowInfo := GetWindowInfo(ASpinEdit.Handle);
    if lWindowInfo = @DefaultWindowInfo then exit;

    lWindowInfo^.spinValue := ASpinEdit.StrToValue(ASpinEdit.Text);
    
    LMessage.Msg := CM_TEXTCHANGED;
  end;

  procedure HandleSpinEditDeltaPos(AUpDownMsg: PNMUpDown);
  var
    SpinEdit: TCustomFloatSpinEdit;
    spinHandle: HWND;
    newValue: single;
  begin
    SpinEdit := TCustomFloatSpinEdit(WindowInfo^.WinControl);
    
    NewValue := SpinEdit.GetLimitedValue(
      WindowInfo^.spinValue + AUpDownMsg^.iDelta * SpinEdit.Increment);
    WindowInfo^.spinValue := NewValue;

    spinHandle := AUpDownMsg^.hdr.hwndFrom;
    UpdateFloatSpinEditText(SpinEdit, NewValue);
    Windows.SendMessage(spinHandle, UDM_SETPOS32, 0, 500);
  end;

  procedure SetMinMaxInfo(var MinMaxInfo: TMINMAXINFO);
    procedure SetWin32SizePoint(AWidth, AHeight: integer; var pt: TPoint);
    var
      IntfWidth, IntfHeight: integer;
    begin
      // 0 means no constraint
      if (AWidth=0) and (AHeight=0) then exit;

      IntfWidth := AWidth;
      IntfHeight := AHeight;
      LCLFormSizeToWin32Size(TCustomForm(lWinControl), IntfWidth, IntfHeight);

      if AWidth>0 then
        pt.X:= IntfWidth;
      if AHeight>0 then
        pt.Y := IntfHeight;
    end;
  begin
    if (lWinControl=nil) or not (lWinControl is TCustomForm) then exit;
    with lWinControl.Constraints do begin
      SetWin32SizePoint(MinWidth, MinHeight, MinMaxInfo.ptMinTrackSize);
      SetWin32SizePoint(MaxWidth, MaxHeight, MinMaxInfo.ptMaxSize);
      SetWin32SizePoint(MaxWidth, MaxHeight, MinMaxInfo.ptMaxTrackSize);
    end;
  end;

  procedure HandleListViewCustomDraw(ALV: TCustomListViewAccess);
    function ConvState(const State: uint): TCustomDrawState;
    begin
      Result := [];
      if state and CDIS_CHECKED <> 0 then Include(Result, cdsChecked);
      if state and CDIS_DEFAULT <> 0 then Include(Result, cdsDefault);
      if state and CDIS_DISABLED <> 0 then Include(Result, cdsDisabled);
      if state and CDIS_FOCUS <> 0 then Include(Result, cdsFocused);
      if state and CDIS_GRAYED <> 0 then Include(Result, cdsGrayed);
      if state and CDIS_HOT <> 0 then Include(Result, cdsHot);
      if state and CDIS_INDETERMINATE <> 0 then Include(Result, cdsIndeterminate);
      if state and CDIS_MARKED <> 0 then Include(Result, cdsMarked);
      if state and CDIS_SELECTED <> 0 then Include(Result, cdsSelected);
    end;

  const
    CDRFRESULT: array[TCustomDrawResultFlag] of Integer = (
      CDRF_SKIPDEFAULT,
      CDRF_NOTIFYPOSTPAINT,
      CDRF_NOTIFYITEMDRAW,
      CDRF_NOTIFYSUBITEMDRAW,
      CDRF_NOTIFYPOSTERASE,
      CDRF_NOTIFYITEMERASE
    );
  var
    DrawInfo: PNMLVCustomDraw absolute NMHdr;
    Stage: TCustomDrawStage;
    DrawResult: TCustomDrawResult;
    ResultFlag: TCustomDrawResultFlag;
  begin
    lmNotify.result := CDRF_DODEFAULT;
    WinProcess := False;
    case DrawInfo^.nmcd.dwDrawStage and $7 of //Get drawing state
      CDDS_PREPAINT:  Stage := cdPrePaint;
      CDDS_POSTPAINT: Stage := cdPostPaint;
      CDDS_PREERASE:  Stage := cdPreErase;
      CDDS_POSTERASE: Stage := cdPostErase;
    else
      Exit;
    end;

    case DrawInfo^.nmcd.dwDrawStage and (CDDS_ITEM or CDDS_SUBITEM) of
      0:
        DrawResult := ALV.IntfCustomDraw(dtControl, Stage, -1, -1, [], @DrawInfo^.nmcd.rc); //Whole control
      CDDS_ITEM:
        DrawResult := ALV.IntfCustomDraw(dtItem, Stage, DrawInfo^.nmcd.dwItemSpec, -1, ConvState(DrawInfo^.nmcd.uItemState), nil);
      CDDS_SUBITEM:
        begin
          // subitem 0 is handled by dtItem
          if DrawInfo^.iSubItem = 0 then Exit;
          DrawResult := ALV.IntfCustomDraw(dtItem, Stage, DrawInfo^.nmcd.dwItemSpec, DrawInfo^.iSubItem, ConvState(DrawInfo^.nmcd.uItemState), nil);
        end;
    else
      Exit;
    end;

    for ResultFlag := Low(ResultFlag) to High(ResultFlag) do
    begin
      if ResultFlag in DrawResult
      then lmNotify.result := lmNotify.result or CDRFRESULT[ResultFlag];
    end;
  end;
  
  procedure HandleDropFiles;
  var
    Files: Array of String;
    Drop: HDROP;
    L: LongWord;
    I, C: Integer;
  {$IFDEF WindowsUnicodeSupport}
    AnsiBuffer: string;
    WideBuffer: WideString;
  {$ENDIF}
  begin
    Drop := HDROP(WParam);
    try
      C := DragQueryFile(Drop, $FFFFFFFF, nil, 0); // get dropped files count
      if C <= 0 then Exit;
      
      SetLength(Files, C);
      for I := 0 to C - 1 do
      begin
        {$IFDEF WindowsUnicodeSupport}
        if UnicodeEnabledOS then
        begin
          L := DragQueryFileW(Drop, I, nil, 0); // get I. file name length
          SetLength(WideBuffer, L);
          L := DragQueryFileW(Drop, I, @WideBuffer[1], L + 1);
          SetLength(WideBuffer, L);
          Files[I] := UTF8Encode(WideBuffer);
        end
        else
        begin
          L := DragQueryFile(Drop, I, nil, 0); // get I. file name length
          SetLength(AnsiBuffer, L);
          L := DragQueryFile(Drop, I, @AnsiBuffer[1], L + 1);
          SetLength(WideBuffer, L);
          Files[I] := ANSIToUTF8(AnsiBuffer);
        end;
        {$ELSE}
        L := DragQueryFile(Drop, I, nil, 0); // get I. file name length
        SetLength(Files[I], L);
        DragQueryFile(Drop, I, PChar(Files[I]), L + 1);
        {$ENDIF}
      end;

      if Length(Files) > 0 then
      begin
        if lWinControl is TCustomForm then
          (lWinControl as TCustomForm).IntfDropFiles(Files);
        if Application <> nil then
          Application.IntfDropFiles(Files);
      end;
    finally
      DragFinish(Drop);
    end;
  end;

  // Gets the cursor position relative to a given window
  function GetClientCursorPos(ClientWindow: HWND) : TSmallPoint;
  var
    P: TPoint;
  begin
    Windows.GetCursorPos(P);
    //if the mouse is not over the window is better to set to 0 to avoid weird behaviors
    if Windows.WindowFromPoint(P) = ClientWindow then
      Windows.ScreenToClient(ClientWindow, P)
    else
    begin
      P.X:=0;
      P.Y:=0;
    end;
    Result := PointToSmallPoint(P);
  end;

  // returns false if the UnicodeChar is not handled
  function HandleUnicodeChar(AChar: Word): boolean;
  var
    UTF8Char: TUTF8Char;
  begin
    Result := false;
  
    UTF8Char := UTF8Encode(WideChar(AChar));
    
    lWinControl := WindowInfo^.WinControl;
    if assigned(lWinControl) then
    begin
      Result:= lWinControl.IntfUTF8KeyPress(UTF8Char, 1, False);

      if UTF8Char='' then
        Result:= true;
    end;
  end;
  
begin
  Assert(False, 'Trace:WindowProc - Start');
  
  LMessage.Result := 0;
  LMessage.Msg := LM_NULL;
  PLMsg := @LMessage;
  WinProcess := True;
  NotifyUserInput := False;

  Assert(False, 'Trace:WindowProc - Getting Object with Callback Procedure');
  WindowInfo := GetWindowInfo(Window);
  if WindowInfo^.isChildEdit then
  begin
    // combobox child edit weirdness
    // prevent combobox WM_SIZE message to get/set/compare text to list, to select text
    if IsComboEditSelection then
    begin
      case Msg of
        WM_GETTEXTLENGTH, EM_SETSEL:
        begin
          Result := 0;
          exit;
        end;
        WM_GETTEXT:
        begin
          if WParam > 0 then
            PChar(LParam)^ := #0;
          Result := 0;
          exit;
        end;
      end;
    end;
    lWinControl := WindowInfo^.AWinControl;
    // filter messages we want to pass on to LCL
    if (Msg <> WM_KILLFOCUS) and (Msg <> WM_SETFOCUS) and (Msg <> WM_NCDESTROY)
      and ((Msg < WM_KEYFIRST) or (Msg > WM_KEYLAST))
      and ((Msg < WM_MOUSEFIRST) or (Msg > WM_MOUSELAST)) then
    begin
      Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
      exit;
    end
    else
    if (Msg = WM_KILLFOCUS) or (Msg = WM_SETFOCUS) then
    begin
      // if focus jumps inside combo then no need to notify LCL
      Info.cbSize := SizeOf(Info);
      Win32Extra.GetComboBoxInfo(lWinControl.Handle, @Info);
      if (HWND(WParam) = Info.hwndList) or
         (HWND(WParam) = Info.hwndItem) or
         (HWND(WParam) = Info.hwndCombo) then
      begin
        Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
        exit;
      end;
    end;
  end else begin
    lWinControl := WindowInfo^.WinControl;
  end;
{$ifdef MSG_DEBUG}
  DebugLn(MessageStackDepth, 'lWinControl: ',DbgSName(lWinControl));
{$endif}
  if (IgnoreNextCharWindow <> 0) and ((Msg = WM_CHAR) or (Msg = WM_SYSCHAR)) then
  begin
    if IgnoreNextCharWindow = Window then
    begin
      IgnoreNextCharWindow := 0;
{$ifdef MSG_DEBUG}
      DebugLn(MessageStackDepth, ' *ignoring this character');
{$endif}
      Result := 1;
      exit;
    end;
    IgnoreNextCharWindow := 0;
  end;

  Assert(False, 'Trace:WindowProc - Getting Callback Object');

  Assert(False, 'Trace:WindowProc - Checking Proc');
  Assert(False, Format('Trace:WindowProc - Window Value: $%S-%d; Msg Value: %S; WParam: $%S; LParam: $%S', [IntToHex(Window, 4), Window, WM_To_String(Msg), IntToHex(WParam, sizeof(WParam)*4), IntToHex(LParam, sizeof(LParam)*4)]));

  case Msg of
    WM_ACTIVATE:
    begin
      case LOWORD(WParam) Of
        WA_ACTIVE, WA_CLICKACTIVE:
        begin
          LMessage.Msg := LM_ACTIVATE;
        end;
        WA_INACTIVE:
        begin
          LMessage.Msg := LM_DEACTIVATE;
        end;
      end;
    end;
    WM_ACTIVATEAPP:
    begin
      if Window = TWin32WidgetSet(WidgetSet).AppHandle then
      begin
        if WParam <> 0 then // activated
        begin
          //WriteLn('Restore');
          RestoreStayOnTopFlags(Window);
          Windows.SetWindowPos(TWin32WidgetSet(WidgetSet).AppHandle, HWND_TOP,
            0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
        end
        else
        begin // deactivated
          //WriteLn('Remove');
          RemoveStayOnTopFlags(Window);
          // this is workarond. After RemoveStayOnTopFlags previously stay on top windows are
          // stil on top. So if you switch application they will be on top before second switch
          TargetWindow := GetNextWindow(TWin32WidgetSet(WidgetSet).AppHandle, GW_HWNDNEXT);
          if TargetWindow = 0 then
            TargetWindow := HWND_BOTTOM;
          Windows.SetWindowPos(TWin32WidgetSet(WidgetSet).AppHandle, TargetWindow,
            0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
        end;

        // activate/deactivate main window
        if (Application <> nil) and (Application.MainForm <> nil) and
            Application.MainForm.HandleAllocated then
        begin
          //WriteLn('Activate/Deactivate main window');
          CallDefaultWindowProc(Application.MainForm.Handle, WM_NCACTIVATE, WParam, 0);
        end;
      end;
    end;
    BM_SETCHECK:
    begin
      LMessage.Msg := LM_CHANGED;
    end;
    WM_CAPTURECHANGED:
    begin
      LMessage.Msg := LM_CAPTURECHANGED;
    end;
    WM_CHAR:
    begin
      {$ifdef WindowsUnicodeSupport}
      // first send a IntfUTF8KeyPress to the LCL
      // if the key was not handled send a CN_CHAR for AnsiChar<=#127
      if not HandleUnicodeChar(Word(WParam)) then
      begin
        PLMsg:=@LMChar;
        with LMChar do
        begin
          Msg := CN_CHAR;
          KeyData := LParam;
          if UnicodeEnabledOS then
            CharCode := Word(Char(WideChar(WParam)))
          else
            CharCode := Word(WParam);
          Result := 0;
          Assert(False,Format('WM_CHAR KeyData= %d CharCode= %d ',[KeyData,CharCode]));
        end;
        WinProcess := false;
      end
      else
        WinProcess := true;
      {$else}
      PLMsg:=@LMChar;
      with LMChar do
      begin
        Msg := CN_CHAR;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
        Assert(False,Format('WM_CHAR KeyData= %d CharCode= %d ',[KeyData,CharCode]));
      end;
      WinProcess := false;
      {$endif}
    end;

    WM_MENUCHAR:
    begin
      PLMsg^.Result := FindMenuItemAccelerator(chr(LOWORD(WParam)), LParam);
      WinProcess := false;
    end;

    WM_CLOSE:
    begin
      if (Window = TWin32WidgetSet(WidgetSet).AppHandle) and
        (Application.MainForm <> nil) then
      begin
        Windows.SendMessage(Application.MainForm.Handle, WM_CLOSE, 0, 0);
      end
      else begin
        LMessage.Msg := LM_CLOSEQUERY;
      end;
      // default is to destroy window, inhibit
      WinProcess := false;
    end;
    
    WM_INITMENUPOPUP:
    begin
      if HIWORD(lParam) = 0 then //if not system menu
      begin
        TargetObject := GetPopMenuItemObject;
        if TargetObject is TMenuItem then
        begin
          LMessage.Msg := LM_ACTIVATE;
          TargetObject.Dispatch(LMessage);
          lWinControl := nil;
        end;
      end;
    end;
    
    WM_COMMAND:
    begin
      if LParam=0 then
      begin
        {menuitem or shortcut}
        TargetObject := GetMenuItemObject;
        if TargetObject is TMenuItem then
        begin
          if (HIWORD(WParam) = 0) or (HIWORD(WParam) = 1) then
          begin
            LMessage.Msg := LM_ACTIVATE;
            TargetObject.Dispatch(LMessage);
          end;
          lWinControl := nil;
        end;
      end
      else begin
        lWinControl := GetWindowInfo(HWND(LParam))^.WinControl;
        // buddy controls use 'awincontrol' to designate associated wincontrol
        if lWinControl = nil then
          lWinControl := GetWindowInfo(HWND(LParam))^.AWinControl;
        if lWinControl is TCustomButton then
          case HIWORD(WParam) of
            BN_CLICKED:   LMessage.Msg := LM_CLICKED;
            BN_KILLFOCUS: LMessage.Msg := LM_EXIT;
          end
        else
        if (lWinControl is TCustomEdit) then
        begin
          if (lWinControl is TCustomMemo) then
          case HIWORD(WParam) of
            // multiline edit doesn't send EN_CHANGE, so use EN_UPDATE
            EN_UPDATE: LMessage.Msg := CM_TEXTCHANGED;
          end
          else
          case HIWORD(WParam) of
            EN_CHANGE:
              if (lWinControl is TCustomFloatSpinEdit) then
                HandleSpinEditChange(TCustomFloatSpinEdit(lWinControl))
              else LMessage.Msg := CM_TEXTCHANGED;
          end;
        end
        else if (lWinControl is TCustomListBox) then
          case HIWORD(WParam) of
            LBN_SELCHANGE:  LMessage.Msg := LM_SELCHANGE;
          end
        else if lWinControl is TCustomCombobox then
          case HIWORD(WParam) of
            CBN_EDITCHANGE: LMessage.Msg := LM_CHANGED;
            { CBN_EDITCHANGE is only sent after the user changes the edit box.
              CBN_SELCHANGE is sent when the user changes the text by
              selecting in the list, but before text is actually changed.
              itemindex is updated, so set text manually }
            CBN_SELCHANGE: begin
              UpdateComboBoxText(TCustomComboBox(lWinControl));
              LMessage.Msg := LM_SELCHANGE;
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
    end;

    WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
    begin
      // it's needed for winxp themes where controls send the WM_ERASEBKGND
      // message to their parent to clear their background and then draw
      // transparently
      // only static and button controls have transparent parts
      // others need to erased with their window color
      // scrollbar also has buttons
      WindowDC := HDC(WParam);
      ChildWindowInfo := GetWindowInfo(HWND(LParam));
      ChildWinControl := ChildWindowInfo^.WinControl;
      if ChildWinControl = nil then
        ChildWinControl := ChildWindowInfo^.AWinControl;
      case Msg of
        WM_CTLCOLORSTATIC,
        WM_CTLCOLORBTN: begin
          if GetNeedParentPaint(ChildWindowInfo, ChildWinControl)
            and not ChildWindowInfo^.ThemedCustomDraw then
          begin
            // need to draw transparently, draw background
            SendParentPaintMessage(HWND(LParam), Window, WindowDC);
            LMessage.Result := GetStockObject(HOLLOW_BRUSH);
            SetBkMode(WindowDC, TRANSPARENT);
            WinProcess := false;
          end;
        end;
        WM_CTLCOLORSCROLLBAR: begin
          WinProcess := false;
        end;
      end;

      if WinProcess then
      begin
        if ChildWinControl <> nil then
        begin
          Windows.SetTextColor(WindowDC, Windows.COLORREF(ColorToRGB(ChildWinControl.Font.Color)));
          Windows.SetBkColor(WindowDC, Windows.COLORREF(ColorToRGB(ChildWinControl.Brush.Color)));
          LMessage.Result := LResult(ChildWinControl.Brush.Reference.Handle);
          //DebugLn(['WindowProc ', ChildWinControl.Name, ' Brush: ', LMessage.Result]);
          // Override default handling
          WinProcess := false;
        end;
      end;
    end;
    WM_CLEAR:
    begin
      LMessage.Msg := LM_CLEAR;
    end;
    WM_COPY:
    begin
      LMessage.Msg := LM_COPY;
    end;
    WM_CUT:
    begin
      LMessage.Msg := LM_CUT;
    end;
    WM_DESTROY:
    begin
      Assert(False, 'Trace:WindowProc - Got WM_DESTROY');
      if lWinControl is TCheckListBox then
        TWin32CheckListBoxStrings.DeleteItemRecords(Window);
      if lWinControl is TCustomFloatSpinEdit then
        DestroyFloatSpinEditBuddy(Window);
      if lWinControl is TCustomComboBox then
        DisposeComboEditWindowInfo(TCustomComboBox(lWinControl));
      if WindowInfo^.Overlay<>HWND(nil) then
        Windows.DestroyWindow(WindowInfo^.Overlay);
      LMessage.Msg := LM_DESTROY;
    end;
    WM_DESTROYCLIPBOARD:
    begin
      if assigned(OnClipBoardRequest) then begin
        {$IFDEF VerboseWin32Clipbrd}
        debugln('WM_DESTROYCLIPBOARD');
        {$ENDIF}
        OnClipBoardRequest(0, nil);
        OnClipBoardRequest := nil;
        LMessage.Result := 0;
      end;
    end;
    WM_DRAWITEM:
    begin
      if (WParam = 0) and (PDrawItemStruct(LParam)^.ctlType = ODT_MENU) then
      begin
        menuItem := TObject(PDrawItemStruct(LParam)^.itemData);
        if menuItem is TMenuItem then
        begin
          DrawMenuItem(TMenuItem(menuItem), PDrawItemStruct(LParam)^._hDC, PDrawItemStruct(LParam)^.rcItem, PDrawItemStruct(LParam)^.itemState and ODS_SELECTED <> 0);
        end
      end;

      // TODO: this could crash for a MenuItem.
      WindowInfo := GetWindowInfo(PDrawItemStruct(LParam)^.hwndItem);
      if WindowInfo^.WinControl<>nil then
        lWinControl := WindowInfo^.WinControl;
      {$IFDEF MSG_DEBUG}
      with PDrawItemStruct(LParam)^ do
        debugln(format('Received WM_DRAWITEM type %d handle %x', [ctlType, hwndItem]));
      {$ENDIF}

      if (lWinControl<>nil) and
         (((lWinControl is TCustomListbox) and
            (TCustomListBox(lWinControl).Style <> lbStandard)) or
         ((lWinControl is TCustomCombobox) and
            ((TCustomCombobox(lWinControl).Style = csOwnerDrawFixed) or
            (TCustomCombobox(lWinControl).Style = csOwnerDrawVariable)))) then
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
          if WindowInfo <> @DefaultWindowInfo then
          begin
            WindowInfo^.DrawItemIndex := PDrawItemStruct(LParam)^.itemID;
            WindowInfo^.DrawItemSelected := (PDrawItemStruct(LParam)^.itemState
              and ODS_SELECTED) = ODS_SELECTED;
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
    end;
    WM_ENABLE:
    begin
      if WParam <> 0 Then
        LMessage.Msg := LM_SETEDITABLE;
      if Window = TWin32WidgetSet(WidgetSet).FAppHandle then
        if WParam = 0 then
        begin
          RemoveStayOnTopFlags(Window);
          DisableApplicationWindows(Window);
        end
        else
        begin
          RestoreStayOnTopFlags(Window);
          EnableApplicationWindows(Window);
        end;
        
      // disable child windows of for example groupboxes, but not of forms
      if assigned(lWinControl) and not (lWinControl is TCustomForm) then
        EnableChildWindows(lWinControl, WParam<>0);

      If (lWinControl is TCustomFloatSpinEdit) then
        EnableFloatSpinEditBuddy(Window, WParam<>0);
      // ugly hack to give bitbtns a nice look
      // When no theming active, the internal image needs to be
      // recreated when the enabled state is changed
      if not ThemeServices.ThemesEnabled
      and (lWinControl is TCustomBitBtn)
      then DrawBitBtnImage(TCustomBitBtn(lWinControl), TCustomBitBtn(lWinControl).Caption);
      // if it is groupbox and themed app then invalidate it on enable change
      // to redraw graphic controls on it (issue 0007877)
      if ThemeServices.ThemesAvailable and (lWinControl is TCustomGroupBox) then
        lWinControl.Invalidate;
    end;
    WM_ERASEBKGND:
    begin
      eraseBkgndCommand := TEraseBkgndCommand(EraseBkgndStack and EraseBkgndStackMask);
{$ifdef MSG_DEBUG}
      case eraseBkgndCommand of
        ecDefault: DebugLn(MessageStackDepth, ' *command: default');
        ecDiscardNoRemove, ecDiscard: DebugLn(MessageStackDepth, ' *command: completely ignore');
        ecDoubleBufferNoRemove: DebugLn(MessageStackDepth, ' *command: use double buffer');
      end;
      DebugLn(MessageStackDepth, ' *erasebkgndstack: ', EraseBkgndStackToString);
{$endif}
      if eraseBkgndCommand = ecDoubleBufferNoRemove then
      begin
        if DoubleBufferDC <> 0 then
          WParam := Windows.WParam(DoubleBufferDC);
        if WindowInfo^.isTabPage then
          EraseBkgndStack := (EraseBkgndStack and not ((1 shl EraseBkgndStackShift)-1))
            or dword(ecDiscardNoRemove);
      end else if eraseBkgndCommand <> ecDiscardNoRemove then
        EraseBkgndStack := EraseBkgndStack shr EraseBkgndStackShift;
      if eraseBkgndCommand in [ecDiscard, ecDiscardNoRemove] then
      begin
        Result := 0;
        exit;
      end;
      if not GetNeedParentPaint(WindowInfo, lWinControl) or (eraseBkgndCommand = ecDoubleBufferNoRemove) then
      begin
        if ThemeServices.ThemesEnabled and WindowInfo^.isGroupBox and (lWinControl <> nil) then
        begin
          // Groupbox (which is a button) doesn't erase it's background properly; force repaint
          lWinControl.EraseBackground(HDC(WParam));
          LMessage.Result := 1;
        end
        else
        begin
          LMessage.Msg := LM_ERASEBKGND;
          LMessage.WParam := WParam;
          LMessage.LParam := LParam;
        end;
      end else
      begin
        SendPaintMessage(HDC(WParam));
        LMessage.Result := 1;
      end;
      WinProcess := False;
    end;
    WM_EXITMENULOOP:
      // is it a popup menu
      if longbool(WPARAM) and assigned(WindowInfo^.PopupMenu) then
        WindowInfo^.PopupMenu.Close;
    WM_GETDLGCODE:
    begin
      LMessage.Result := DLGC_WANTALLKEYS;
      WinProcess := false;
    end;
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
    WM_GETMINMAXINFO:
      SetMinMaxInfo(PMINMAXINFO(LParam)^);
    WM_HSCROLL:
      HandleScrollMessage(LM_HSCROLL);
    WM_KEYDOWN:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      with LMKey Do
      begin
        Msg := CN_KEYDOWN;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
        Assert(False,Format('WM_KEYDOWN KeyData= %d CharCode= %d ',[KeyData,CharCode]));
        Assert(False,'  lWinControl= '+TComponent(lWinControl).Name+':'+lWinControl.ClassName);
      end;
      WinProcess := false;
    end;
    WM_KEYUP:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      with LMKey Do
      begin
        Msg := CN_KEYUP;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
        Assert(False,Format('WM_KEYUP KeyData= %d CharCode= %d ',[KeyData,CharCode]));
      end;
      WinProcess := false;
    end;
    WM_KILLFOCUS:
    begin
{$ifdef DEBUG_CARET}
      DebugLn('WM_KILLFOCUS received for window ', IntToHex(Window, 8));
{$endif}
      LMessage.Msg := LM_KILLFOCUS;
      LMessage.WParam := WParam;
    end;
    //TODO:LM_KILLCHAR,LM_KILLWORD,LM_KILLLINE
    WM_LBUTTONDBLCLK:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_LBUTTONDBLCLK;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
      end;

      // CheckListBox functionality
      if lWinControl is TCheckListBox then
        CheckListBoxLButtonDown;
    end;
    WM_LBUTTONDOWN:
    begin
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
      with LMMouse Do
      begin
        Msg := LM_LBUTTONDOWN;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
      end;

      // CheckListBox functionality
      if lWinControl is TCheckListBox then
        CheckListBoxLButtonDown;
      // focus window
      if (Windows.GetFocus <> Window) and
          ((lWinControl = nil) or (lWinControl.CanFocus)) then
        Windows.SetFocus(Window);

    end;
    WM_LBUTTONUP:
    begin
      if (MouseDownWindow = Window) and (MouseDownFocusStatus = mfNone) then
        MouseDownFocusStatus := mfFocusSense;
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_LBUTTONUP;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
      end;
    end;
    WM_MBUTTONDBLCLK:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_MBUTTONDBLCLK;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
      end;
    end;
    WM_MBUTTONDOWN:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_MBUTTONDOWN;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
      end;
    end;
    WM_MBUTTONUP:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_MBUTTONUP;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
      end;
    end;
    WM_MOUSEHOVER:
    begin
      NotifyUserInput := True;
      LMessage.Msg := LM_ENTER;
    end;
    WM_MOUSELEAVE:
    begin
      NotifyUserInput := True;
      LMessage.Msg := LM_LEAVE;
    end;
    WM_MOUSEMOVE:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouseMove;
      with LMMouseMove Do
      begin
        Msg := LM_MOUSEMOVE;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
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
      end;
    end;
    WM_MOUSEWHEEL:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouseEvent;
      with LMMouseEvent Do
      begin
        X := GET_X_LPARAM(LParam);
        Y := GET_Y_LPARAM(LParam);
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

        // check InMouseWheelRedirection to prevent recursion
        if not InMouseWheelRedirection and (TargetWindow <> Window) then
        begin
          InMouseWheelRedirection := true;
          Result := SendMessage(TargetWindow, WM_MOUSEWHEEL, WParam, LParam);
          InMouseWheelRedirection := false;
          exit;
        end;

        // the mousewheel message is for us
        Msg := LM_MOUSEWHEEL;
        Button := LOWORD(WParam);
        WheelDelta := SmallInt(HIWORD(WParam));
        State := GetShiftState;
        Result := 0;
        UserData := Pointer(GetWindowLong(Window, GWL_USERDATA));
        WinProcess := false;
      end;
    end;
    WM_DROPFILES:
      begin
        {$IFDEF EnableWMDropFiles}
        LMessage.Msg := LM_DROPFILES;
        LMessage.WParam := WParam;
        LMessage.LParam := LParam;
        {$ENDIF}
        
        HandleDropFiles;
      end;
    //TODO:LM_MOVEPAGE,LM_MOVETOROW,LM_MOVETOCOLUMN
    WM_NCACTIVATE:
    begin
      // only allow main form to be deactivated, if it is disabled
      if (WParam=0) and (Application <> nil) and (Application.MainForm <> nil) and
          Application.MainForm.HandleAllocated and (Window = Application.MainForm.Handle) and
          Windows.IsWindowEnabled(Window) then
      begin
        WParam := 1;
      end;
    end;
    WM_NCHITTEST:
    begin
      if (lWinControl <> nil) then
      begin
        if (lWinControl.FCompStyle = csHintWindow) then
        begin
          LMessage.Result := HTTRANSPARENT;
          WinProcess := false;
        end
        else
        if (lWinControl is TCustomGroupBox) then
        begin
          LMessage.Result := HTCLIENT;
          WinProcess := false;
        end;
      end;
    end;
    WM_NCLBUTTONDOWN:
    begin
      NotifyUserInput := True;
      Assert(False, 'Trace:WindowProc - Got WM_NCLBUTTONDOWN');

      //Drag&Dock support TCustomForm => Start BeginDrag()
      if lWinControl <> nil then
      begin
        if WParam = HTCAPTION  then
        begin
          if lWinControl is TCustomForm then
          begin
            if (TWinControlAccess(lWinControl).DragKind = dkDock) and
               (TWinControlAccess(lWinControl).DragMode = dmAutomatic) then
              lWinControl.BeginDrag(true);
          end;
        end;
      end;

    end;
    WM_NOTIFY:
    begin
      WindowInfo := GetWindowInfo(PNMHdr(LParam)^.hwndFrom);
{$ifdef MSG_DEBUG}
      DebugLn([MessageStackDepth, 'Notify code: ', PNMHdr(LParam)^.code]);
{$endif}
      case PNMHdr(LParam)^.code of
        MCN_SELCHANGE:
        begin
          LMessage.Msg := LM_CHANGED;
          if WindowInfo^.WinControl <> nil then
            lWinControl := WindowInfo^.WinControl;
        end;
        UDN_DELTAPOS:
        begin
          if WindowInfo^.WinControl <> nil then
            HandleSpinEditDeltaPos(PNMUpDown(LParam));
        end;
        NM_CLICK, NM_RCLICK:
        begin
          // A listview doesn't get a WM_LBUTTONUP, WM_RBUTTONUP message,
          // because it keeps the message in its own event loop,
          // see msdn article about "Default List-View Message Processing"
          // therefore we take this notification and create a
          // LM_LBUTTONUP, LM_RBUTTONUP message out of it
          if (WindowInfo^.WinControl <> nil) and
            (WindowInfo^.WinControl is TCustomListView) then
          begin
            WinProcess := False;
            lWinControl := WindowInfo^.WinControl;
            PLMsg:=@LMMouse;
            with LMMouse Do
            begin
              if PNMHdr(LParam)^.code = NM_CLICK then
                Msg := LM_LBUTTONUP
              else
                Msg := LM_RBUTTONUP;
              Pos := GetClientCursorPos(PNMHdr(LParam)^.hwndFrom);
              Keys := 0;  // I don't know how to get this information
              Result := 0;
              // to make correct event sequence in LCL we should postpone this message
              // since we are here after call of CallDefaultWindowProc
              
              // TODO: prevent getting more than one Up, Down message by LCL
              PostMessage(Window, Msg, 0, MakeLParam(Pos.x, Pos.y));
              Exit;
            end;
          end;
        end;
      else
        PLMsg:=@LMNotify;
        with LMNotify Do
        begin
          Msg := LM_NOTIFY;
          IDCtrl := WParam;
          NMHdr := PNMHDR(LParam);
          with NMHdr^ do
            case code of
              TCN_SELCHANGE:
                idFrom := ShowHideTabPage(HWndFrom, true);
              NM_CUSTOMDRAW:
              begin
                if WindowInfo^.WinControl is TCustomListView
                then HandleListViewCustomDraw(TCustomListViewAccess(WindowInfo^.WinControl))
                else if GetNeedParentPaint(WindowInfo, lWinControl)
                  and WindowInfo^.ThemedCustomDraw then
                begin
                  case PNMCustomDraw(LParam)^.dwDrawStage of
                    CDDS_PREPAINT:
                    begin
                      Result := CDRF_NOTIFYITEMDRAW;
                      WinProcess := false;
                    end;
                    CDDS_ITEMPREPAINT:
                    begin
                      case PNMCustomDraw(LParam)^.dwItemSpec of
                        TBCD_TICS, TBCD_CHANNEL:
                          SendParentPaintMessage(Window, Windows.GetParent(Window),
                            PNMCustomDraw(LParam)^.hDC);
                      end;
                      Result := CDRF_DODEFAULT;
                      WinProcess := false;
                    end;
                  end;
                end;
              end;
            end;
        end;
      end;
    end;
    WM_PAINT:
    begin
      SendPaintMessage(HDC(WParam));
      // SendPaintMessage sets winprocess to false
    end;
    WM_PRINTCLIENT:
    begin
      if ((LParam and PRF_CLIENT) = PRF_CLIENT) and (lWinControl <> nil) then
        SendPaintMessage(HDC(WParam));
    end;
    WM_PASTE:
    begin
      LMessage.Msg := LM_PASTE;
    end;
    WM_RBUTTONDBLCLK:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_RBUTTONDBLCLK;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
      end;
    end;
    WM_RBUTTONDOWN:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_RBUTTONDOWN;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
        Result := 0;
      end;
    end;
    WM_RBUTTONUP:
    begin
      NotifyUserInput := True;
      WinProcess := false;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_RBUTTONUP;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
        Result := 0;
      end;
    end;
    WM_CONTEXTMENU:
    begin
      WinProcess := false;
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse do
      begin
        Msg := LM_CONTEXTMENU;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        //Only keyboard triggered contextmenu (Shift-F10) should be sent to LCL
        //but calling default handler is necessary. This schema avoids parent recursion
        //and also keeps default popupmenu (TMemo)
        if XPos = -1 then
          Pos := GetClientCursorPos(Window)
        else
          lWinControl:=nil; // make sure no message is sent to the LCL
        Result := 0;
      end;
    end;
    WM_SETCURSOR:
    begin
      HandleSetCursor;
    end;
    WM_SETFOCUS:
    begin
{$ifdef DEBUG_CARET}
      DebugLn('WM_SETFOCUS received for window ', IntToHex(Window, 8));
{$endif}
      // handle feature mouse-click, setfocus, mouse-click -> double-click
      if (Window <> MouseDownWindow) and (MouseDownFocusStatus <> mfNone) then
      begin
        MouseDownFocusStatus := mfFocusChanged;
        MouseDownFocusWindow := Window;
      end;
      LMessage.Msg := LM_SETFOCUS;
      if (lWinControl <> nil) and (lWinControl is TCustomEdit) then
        Windows.SendMessage(Window, EM_SETSEL, 0, -1);
      // RadioButton functionality
      if (lWinControl <> nil) and (lWinControl is TRadioButton) then
        Windows.PostMessage(Window, BM_SETCHECK, BST_CHECKED, 0);
    end;
    WM_SHOWWINDOW:
    begin
      Assert(False, 'Trace:WindowProc - Got WM_SHOWWINDOW');
      with TLMShowWindow(LMessage) Do
      begin
        Msg := LM_SHOWWINDOW;
        Show := WParam <> 0;
        Status := LParam;
      end;
      //DebugLn(GetStackTrace(false));
      if assigned(lWinControl) and ((WParam<>0) or not lWinControl.Visible)
        and ((WParam=0) or lWinControl.Visible)
        and (Application<>nil) and (lWinControl=Application.MainForm) then
      begin
        if WParam=0 then
          Flags := SW_HIDE
        else
          Flags := SW_SHOWNOACTIVATE;
        Windows.ShowWindow(TWin32WidgetSet(WidgetSet).FAppHandle, Flags);
      end;
    end;
    WM_SYSCHAR:
    begin
      PLMsg:=@LMChar;
      with LMChar Do
      begin
        Msg := CN_SYSCHAR;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
        Assert(False,Format('WM_CHAR KeyData= %d CharCode= %d ',[KeyData,CharCode]));
      end;
      WinProcess := false;
    end;
    WM_SYSCOMMAND:
    begin
      HandleSysCommand;
    end;
    WM_SYSKEYDOWN:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      with LMKey Do
      begin
        Msg := CN_SYSKEYDOWN;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
      end;
      WinProcess := false;
    end;
    WM_SYSKEYUP:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      with LMKey Do
      begin
        Msg := CN_SYSKEYUP;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
      end;
      WinProcess := false;
    end;
    WM_TIMER:
    begin
      LMessage.Msg := LM_TIMER;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
    end;
    WM_VSCROLL:
      HandleScrollMessage(LM_VSCROLL);
    WM_WINDOWPOSCHANGED:
    begin
      with TLMWindowPosMsg(LMessage) Do
      begin
        Msg := LM_WINDOWPOSCHANGED;
        Unused := WParam;
        WindowPos := PWindowPos(LParam);
      end;
      // cross-interface compatible: complete invalidate on resize
      if (PWindowPos(LParam)^.flags and SWP_NOSIZE) = 0 then
        Windows.InvalidateRect(Window, nil, true);
    end;
    WM_MEASUREITEM:
    begin
      if WParam = 0 then begin
        menuItem := TObject(PMeasureItemStruct(LParam)^.itemData);
        if menuItem is TMenuItem then
        begin
          menuHDC := GetDC(Window);
          TmpSize := MenuItemSize(TMenuItem(menuItem), menuHDC);
          PMeasureItemStruct(LParam)^.itemWidth := TmpSize.cx;
          PMeasureItemStruct(LParam)^.itemHeight := TmpSize.cy;
          ReleaseDC(Window, menuHDC);
          Winprocess := False;
        end else
          DebugLn('WM_MEASUREITEM for a menuitem catched but menuitem is not TmenuItem');
      end;
      if LWinControl<>nil then begin
        if LWinControl is TCustomCombobox then begin
          LMessage.Msg := LM_MEASUREITEM;
          LMessage.LParam := LParam;
          LMessage.WParam := WParam;
          Winprocess := False;
        end else
        if WParam <> 0 then begin
          LWinControl := TWinControl(WParam);
          if LWinControl<>nil then begin
            LMessage.Msg := LM_MEASUREITEM;
            LMessage.LParam := LParam;
            LMessage.WParam := WParam;
            Winprocess := False;
          end;
        end;
      end;
    end;
    WM_THEMECHANGED:
    begin
      // winxp theme changed, recheck whether themes are enabled
      ThemeServices.UpdateThemes;
      ThemeServices.IntfDoOnThemeChange;
    end;

    { >= WM_USER }

    WM_LCL_SOCK_ASYNC:
    begin
      if (Window = TWin32WidgetSet(WidgetSet).AppHandle) and
          Assigned(TWin32WidgetSet(WidgetSet).FOnAsyncSocketMsg) then
        exit(TWin32WidgetSet(WidgetSet).FOnAsyncSocketMsg(WParam, LParam))
    end;
  else
    // pass along user defined messages
    if Msg >= WM_USER then
    begin
      LMessage.Msg := Msg;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
      WinProcess := False;
    end;
  end;

  if WinProcess then
  begin
    PLMsg^.Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
    WinProcess := False;
  end;

  case Msg of
    WM_MOVE:
    begin
      PLMsg:=@LMMove;
      with LMMove Do
      begin
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
          DebugLn('Win32CallBack WM_MOVE ', dbgsName(lWinControl),
            ' NewPos=',dbgs(XPos),',',dbgs(YPos));
          {$ENDIF}
          if (lWinControl.Left=XPos) and (lWinControl.Top=YPos) then
            exit;
        end;
      end;
    end;
    WM_SIZE:
    begin
      with TLMSize(LMessage) Do
      begin
        Msg := LM_SIZE;
        SizeType := WParam or Size_SourceIsInterface;
        if Window = TWin32WidgetSet(WidgetSet).AppHandle then
        begin
          lWinControl := Application.MainForm;
          Window := Application.MainForm.Handle;
        end;
        GetWindowSize(Window, NewWidth, NewHeight);
        Width := NewWidth;
        Height := NewHeight;
        if lWinControl <> nil then
        begin
          {$IFDEF VerboseSizeMsg}
          GetClientRect(Window,R);
          DebugLn('Win32Callback: WM_SIZE '+ dbgsName(lWinControl)+
            ' NewSize=', dbgs(Width)+','+dbgs(Height)+
            ' HasVScroll='+dbgs((GetWindowLong(Window, GWL_STYLE) and WS_VSCROLL) <> 0)+
            ' HasHScroll='+dbgs((GetWindowLong(Window, GWL_STYLE) and WS_HSCROLL) <> 0)+
            ' OldClientSize='+dbgs(lWinControl.CachedClientWidth)+','+dbgs(lWinControl.CachedClientHeight)+
            ' NewClientSize='+dbgs(R.Right)+','+dbgs(R.Bottom));
          {$ENDIF}
          if (lWinControl.Width<>Width) or (lWinControl.Height<>Height)
              or lWinControl.ClientRectNeedsInterfaceUpdate then
          begin
            lWinControl.InvalidateClientRectCache(true);
            lWinControl.DoAdjustClientRectChange;
            if (lWinControl is TCustomPage)
            and (lWinControl.Parent is TCustomNotebook) then
            begin
              // the TCustomPage size is the ClientRect size of the parent
              // => invalidate the Parent.ClientRect
              lWinControl.Parent.InvalidateClientRectCache(false);
            end;
          end;
        end;
        OverlayWindow := GetWindowInfo(Window)^.Overlay;
        if OverlayWindow <> 0 then
          Windows.SetWindowPos(OverlayWindow, HWND_TOP, 0, 0, NewWidth, NewHeight, SWP_NOMOVE);
      end;
    end;
    BM_SETCHECK:
    begin
      if (WParam=BST_CHECKED) and (lWinControl is TRadioButton) then
        ClearSiblingRadioButtons(TRadioButton(lWinControl));
    end;
    WM_ENDSESSION:
    begin
      if (Application<>nil) and (TWin32WidgetSet(WidgetSet).AppHandle=Window) and
         (WParam>0) and (LParam=0) then
      begin
        LMessage.Msg := LM_NULL; // no need to go through delivermessage
        Application.IntfEndSession();
        LMessage.Result := 0;
      end;
    end;

    WM_QUERYENDSESSION:
    begin
      if (Application<>nil) and (TWin32WidgetSet(WidgetSet).AppHandle=Window) and
         (LParam=0) then
      begin
        LMessage.Msg := LM_NULL; // no need to go through delivermessage
        CancelEndSession := LMessage.Result=0;
        Application.IntfQueryEndSession(CancelEndSession);
        if CancelEndSession
          then LMessage.Result := 0
          else LMessage.Result := 1;
      end;
    end;
    WM_NCPAINT:
    begin
      if (lWinControl <> nil) and TWin32ThemeServices(ThemeServices).ThemesEnabled and
         not (lWinControl is TCustomForm) and
         ((lWinControl is TCustomControl) or
          ((lWinControl is TCustomStaticText) and (TCustomStaticText(lWinControl).BorderStyle = sbsSingle))
         ) then
      begin
        TWin32ThemeServices(ThemeServices).PaintBorder(lWinControl, True);
        LMessage.Result := 0;
      end;
    end;
  end;

  // convert from win32 client to lcl client pos.
  //
  // hack to prevent GetLCLClientBoundsOffset from changing mouse client
  // coordinates for TScrollingWinControls, this is required in
  // IsControlMouseMsg and ControlAtPos where unscrolled client coordinates
  // are expected.
  if (PLMsg = @LMMouseMove) and not (lWinControl is TScrollingWinControl) then
  begin
    if GetLCLClientBoundsOffset(lWinControl, R) then
    begin
      Dec(LMMouseMove.XPos, R.Left);
      Dec(LMMouseMove.YPos, R.Top);
    end;
  end else
  if (PLMsg = @LMMouse) and not (lWinControl is TScrollingWinControl) then
  begin
    if GetLCLClientBoundsOffset(lWinControl, R) then
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
    LM_ERASEBKGND, LM_SETCURSOR, LM_RBUTTONUP, LM_CONTEXTMENU, LM_MOUSEWHEEL:
    begin
      if PLMsg^.Result = 0 then
        WinProcess := true;
    end;

    CN_CHAR, CN_SYSCHAR:
    begin
      // if key not yet processed, let windows process it
      WinProcess := LMChar.Result = 0;
      {$IFDEF WindowsUnicodeSupport}
      if UnicodeEnabledOS then
        WParam := Word(WideChar(Char(LMChar.CharCode)))
      else
      {$ENDIF}
        WParam := LMChar.CharCode;
    end;

    CN_KEYDOWN, CN_KEYUP, CN_SYSKEYDOWN, CN_SYSKEYUP:
    begin
      // if key not yet processed, let windows process it
      WinProcess := LMKey.Result = 0;
      WParam := LMKey.CharCode;
    end;

    LM_NOTIFY:
    begin
      with LMNotify.NMHdr^ do
        case code of
          TCN_SELCHANGING:
            if LMNotify.Result = 0 then
              ShowHideTabPage(HWndFrom, False);
        end;
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
        // free extra parent window for inner groupbox (if used)
        if WindowInfo^.ParentPanel<>HWND(nil) then
          Windows.DestroyWindow(WindowInfo^.ParentPanel);
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
          if WndClassName(Window) = EditClsName then
          begin
            // select all
            Windows.SendMessage(Window, EM_SETSEL, 0, -1);
          end;
        end;
      end;
    end;
  end;

  // ignore WM_(SYS)CHAR message if LCL handled WM_(SYS)KEYDOWN
  if ((Msg = WM_KEYDOWN) or (Msg = WM_SYSKEYDOWN)) then
  begin
    if (PLMsg^.Result <> 0) then
    begin
{$ifdef MSG_DEBUG}
      debugln(MessageStackDepth, ' *ignore next character');
{$endif}
      IgnoreNextCharWindow := Window;
    end else begin
      // stop ignoring if KEYUP has come by (not all keys generate CHAR)
      // assume WM_CHAR is always preceded by WM_KEYDOWN
{$ifdef MSG_DEBUG}
      if IgnoreNextCharWindow <> 0 then
        debugln(MessageStackDepth, ' *stop ignoring next character');
{$endif}
      IgnoreNextCharWindow := 0;
    end;
  end;

  { LMInsertText has no Result field }

  if      PLMsg = @LMScroll     then Result := LMScroll.Result
  else if PLMsg = @LMKey        then Result := LMKey.Result
  else if PLMsg = @LMChar       then Result := LMChar.Result
  else if PLMsg = @LMMouse      then Result := LMMouse.Result
  else if PLMsg = @LMMouseMove  then Result := LMMouseMove.Result
  else if PLMsg = @LMMove       then Result := LMMove.Result
  else if PLMsg = @LMNotify     then Result := LMNotify.Result
  else if PLMsg = @LMMouseEvent then Result := LMMouseEvent.Result
  else                               Result := PLMsg^.Result;

  Assert(False, 'Trace:WindowProc - Exit');
end;

{$ifdef MSG_DEBUG}

function WindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
begin
  DebugLn(MessageStackDepth, 'WindowProc called for window=', IntToHex(Window, 8),' msg=',
    WM_To_String(msg),' wparam=', IntToHex(WParam, sizeof(WParam)*2), ' lparam=', IntToHex(lparam, sizeof(lparam)*2));
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
var
  Parent: HWND;
  Owner: TWinControl;
  Control: TControl;
  P: TPoint;
begin
  case Msg of
    WM_ERASEBKGND:
    begin
      Result := 1;
    end;
    WM_NCHITTEST:
    begin
      // By default overlay window handle all mouse messages
      Result := HTCLIENT;
      
      // Check if overlayed control want to handle mouse messages
      Parent := Windows.GetParent(Window);
      Owner := GetWindowInfo(Parent)^.WinControl;
      P.x := GET_X_LPARAM(lParam);
      P.y := GET_Y_LPARAM(lParam);
      Windows.ScreenToClient(Parent, P);
      if (Owner is TCustomForm) then
      begin
        // ask form about control under mouse. we need TWinControl
        Control := Owner.ControlAtPos(P, [capfAllowWinControls]);        
        if (Control <> nil) and not (Control is TWinControl) then
          Control := Control.Parent;
      end
      else
        Control := nil;
      if (Control <> nil) then
      begin
        // Now ask control is it needs mouse messages
        MapWindowPoints(Parent, TWinControl(Control).Handle, P, 1);
        if TWSWinControlClass(TWinControl(Control).WidgetSetClass).GetDesignInteractive(TWinControl(Control), P) then
          Result := HTTRANSPARENT
      end;
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
    if UnicodeEnabledOS
    then Result := Windows.DefWindowProcW(Window, Msg, WParam, LParam)
    else Result := Windows.DefWindowProc(Window, Msg, WParam, LParam);
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
procedure TimerCallBackProc(window_hwnd : hwnd; msg : DWORD; idEvent: UINT; dwTime: DWORD); stdcall;
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
