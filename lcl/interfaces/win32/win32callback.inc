{%MainUnit win32int.pp}

{
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
  RemoveProp(Window, Str);
  Result := True;
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
    Result := WndClassName(Window) = LCLComboboxClsName;
    if not Result then
      Exit;
    Info.cbSize := SizeOf(Info);
    Win32Extra.GetComboBoxInfo(Window, @Info);
    Result := (Info.hwndItem <> 0) and GetWin32WindowInfo(Info.hwndItem)^.isComboEdit;
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
  PrevWndProc := GetWin32WindowInfo(Window)^.DefWndProc;
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
    if UnicodeEnabledOS then
      Result := Windows.CallWindowProcW(PrevWndProc, Window, Msg, WParam, LParam)
    else
      Result := Windows.CallWindowProc(PrevWndProc, Window, Msg, WParam, LParam);
    if setComboWindow then
      ComboBoxHandleSizeWindow := 0;
  end;
{$ifdef MSG_DEBUG}
  if depthLen > 0 then
    MessageStackDepth[depthLen] := ' ';
{$endif}
end;

procedure DrawParentBackground(Window: HWND; ControlDC: HDC);
var
  Parent: HWND;
  P: TPoint;
begin
  if ThemeServices.ThemesEnabled then
    ThemeServices.DrawParentBackground(Window, ControlDC, nil, False)
  else
  begin
    Parent := Windows.GetParent(Window);
    P.X := 0;
    P.Y := 0;
    Windows.MapWindowPoints(Window, Parent, P, 1);
    Windows.OffsetViewportOrgEx(ControlDC, -P.X, -P.Y, P);
    Windows.SendMessage(Parent, WM_ERASEBKGND, ControlDC, 0);
    Windows.SendMessage(Parent, WM_PRINTCLIENT, ControlDC, PRF_CLIENT);
    Windows.SetViewportExtEx(ControlDC, P.X, P.Y, nil);
  end;
end;

type
  TEraseBkgndCommand =
  (
    ecDefault,             // todo: add comments
    ecDiscard,             //
    ecDiscardNoRemove,     //
    ecDoubleBufferNoRemove //
  );
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

type
  TDoubleBuffer = record
    DC: HDC;
    Bitmap: HBITMAP;
    BitmapWidth: integer;
    BitmapHeight: integer;
  end;

var
  CurDoubleBuffer: TDoubleBuffer = (DC: 0; Bitmap: 0; BitmapWidth: 0; BitmapHeight: 0);
  DisabledForms: TList = nil;
  CurrentWindow: HWND = 0;

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

function GetNeedParentPaint(AWindowInfo: PWin32WindowInfo; AWinControl: TWinControl): boolean;
begin
  Result := AWindowInfo^.needParentPaint
    and ((AWinControl = nil) or not (csOpaque in AWinControl.ControlStyle));
  if ThemeServices.ThemesEnabled then
    Result := Result or (Assigned(AWinControl) and ([csParentBackground, csOpaque] * AWinControl.ControlStyle = [csParentBackground]));
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
  ChildWindowInfo: PWin32WindowInfo;
  TargetObject: TObject;
  WinProcess, WmSysCommandProcess: Boolean;
  NotifyUserInput: Boolean;
  OverlayWindow: HWND;
  TargetWindow: HWND;
  eraseBkgndCommand: TEraseBkgndCommand;
  WindowInfo: PWin32WindowInfo;
  Flags: dword;
  WindowDC: HDC;
  WindowPlacement: TWINDOWPLACEMENT;
  WindowColor: TColor;

  LMScroll: TLMScroll; // used by WM_HSCROLL
  LMKey: TLMKey; // used by WM_KEYDOWN WM_KEYUP
  LMChar: TLMChar; // used by WM_CHAR
  LMMouse: TLMMouse; // used by WM_LBUTTONDBLCLK
  LMContextMenu: TLMContextMenu;
  LMMouseMove: TLMMouseMove; // used by WM_MOUSEMOVE
  LMMouseEvent: TLMMouseEvent; // used by WM_MOUSEWHEEL
  LMMove: TLMMove; // used by WM_MOVE
  LMNotify: TLMNotify; // used by WM_NOTIFY
  DrawListItemStruct: TDrawListItemStruct; //used by WM_DRAWITEM
  CancelEndSession : Boolean;//use by WM_QUERYENDSESSION

  NMHdr: PNMHdr absolute LParam; // used by WM_NOTIFY
  TmpSize: TSize; // used by WM_MEASUREITEM
  Info: TComboboxInfo;
  OrgCharCode: word; // used in WM_CHAR handling

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
    if GetMenuItemInfo(MainMenuHandle, LOWORD(LParam), true, @MenuInfo) then
      Result := TObject(MenuInfo.dwItemData)
    else
      Result := nil;
  end;

  function GetMenuItemObject(ByPosition: Boolean): TObject;
  var
    MenuInfo: MENUITEMINFO;
    PopupMenu: TPopupMenu;
  begin
    // first we have to decide if the command is from a popup menu
    // or from the window main menu
    // if the 'PopupMenu' property exists, there is a big probability
    // that the command is from a popup menu

    PopupMenu := WindowInfo^.PopupMenu;
    if Assigned(PopupMenu) then
    begin
      Result := PopupMenu.FindItem(LOWORD(Integer(WParam)), fkCommand);
      if Assigned(Result) then
        Exit;
    end;

    // nothing found, process main menu
    MenuInfo.cbSize := MMenuItemInfoSize;
    MenuInfo.fMask := MIIM_DATA;

    if GetMenuItemInfo(GetMenu(Window), LOWORD(Integer(WParam)), ByPosition, @MenuInfo) then
      Result := TObject(MenuInfo.dwItemData)
    else
      Result := nil;
  end;

  function GetIsNativeControl(AWindow: HWND): Boolean;
  var
    S: String;
  begin
    S := WndClassName(AWindow);
    Result := (S <> ClsName) and (S <> ClsHintName);
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
    isNativeControl: boolean;
    needParentPaint: boolean;
    BufferWasSaved: Boolean;
    BackupBuffer: TDoubleBuffer;
  begin
    // note: ignores the received DC
    // do not use default deliver message
    if lWinControl = nil then
    begin
      lWinControl := GetWin32WindowInfo(Window)^.PWinControl;
      if lWinControl = nil then exit;
    end;

    // create a paint message
    isNativeControl := GetIsNativeControl(Window);
    needParentPaint := GetNeedParentPaint(WindowInfo, lWinControl);
    // if needParentPaint and not isTabPage then background will be drawn in
    // WM_ERASEBKGND and WM_CTLCOLORSTATIC for native controls
    // sent by default paint handler
    if WindowInfo^.isTabPage or (needParentPaint and (not isNativeControl or (ControlDC <> 0))) then
      ParentPaintWindow := Windows.GetParent(Window)
    else
      ParentPaintWindow := 0;

    // if painting background of some control for tabpage, don't handle erase background
    // in parent of tabpage
    if WindowInfo^.isTabPage then
      PushEraseBkgndCommand(ecDiscard);

    // check if double buffering is requested
    useDoubleBuffer := (ControlDC = 0) and (lWinControl.DoubleBuffered or ThemeServices.ThemesEnabled);
    if useDoubleBuffer then
    begin
      if CurDoubleBuffer.DC <> 0 then
      begin
        // we've been called from another paint handler. To prevent killing of
        // not own DC and HBITMAP lets save then and restore on exit
        BackupBuffer := CurDoubleBuffer;
        FillChar(CurDoubleBuffer, SizeOf(CurDoubleBuffer), 0);
        BufferWasSaved := True;
      end
      else
        BufferWasSaved := False;
      CurDoubleBuffer.DC := Windows.CreateCompatibleDC(0);
      GetWindowSize(Window, WindowWidth, WindowHeight);
      if (CurDoubleBuffer.BitmapWidth < WindowWidth) or (CurDoubleBuffer.BitmapHeight < WindowHeight) then
      begin
        DC := Windows.GetDC(0);
        if CurDoubleBuffer.Bitmap <> 0 then
          Windows.DeleteObject(CurDoubleBuffer.Bitmap);
        CurDoubleBuffer.BitmapWidth := WindowWidth;
        CurDoubleBuffer.BitmapHeight := WindowHeight;
        CurDoubleBuffer.Bitmap := Windows.CreateCompatibleBitmap(DC, WindowWidth, WindowHeight);
        Windows.ReleaseDC(0, DC);
      end;
      DoubleBufferBitmapOld := Windows.SelectObject(CurDoubleBuffer.DC, CurDoubleBuffer.Bitmap);
      PaintMsg.DC := CurDoubleBuffer.DC;
    end;

{$ifdef MSG_DEBUG}
    if useDoubleBuffer then
      DebugLn(MessageStackDepth, ' *double buffering on DC: ', IntToHex(CurDoubleBuffer.DC, sizeof(HDC)*2))
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
          ORect.Right := CurDoubleBuffer.BitmapWidth;
          ORect.Bottom := CurDoubleBuffer.BitmapHeight;
          Windows.FillRect(CurDoubleBuffer.DC, ORect, GetSysColorBrush(COLOR_DESKTOP));
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
            Windows.SelectClipRgn(CurDoubleBuffer.DC, PaintRegion);
          end;
{$ifdef DEBUG_DOUBLEBUFFER}
          Windows.GetClipBox(CurDoubleBuffer.DC, ClipBox);
          DebugLn('Double buffering in DC ', IntToHex(CurDoubleBuffer.DC, sizeof(HDC)*2),
            ' with clipping rect (',
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
      if not needParentPaint then
      begin
        // send through message to allow message override, moreover use SendMessage
        // to allow subclass window proc override this message too
        Include(TWinControlAccess(lWinControl).FWinControlFlags, wcfEraseBackground);
        Windows.SendMessage(lWinControl.Handle, WM_ERASEBKGND, Windows.WPARAM(PaintMsg.DC), 0);
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
        DrawParentBackground(Window, PaintMsg.DC);
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
        Windows.BitBlt(DC, 0, 0, WindowWidth, WindowHeight, CurDoubleBuffer.DC, 0, 0, SRCCOPY);
      if ControlDC = 0 then
        Windows.EndPaint(Window, @PS);
    finally
      if useDoubleBuffer then
      begin
        SelectObject(CurDoubleBuffer.DC, DoubleBufferBitmapOld);
        DeleteDC(CurDoubleBuffer.DC);
        CurDoubleBuffer.DC := 0;
        if BufferWasSaved then
        begin
          if CurDoubleBuffer.Bitmap <> 0 then
            DeleteObject(CurDoubleBuffer.Bitmap);
          CurDoubleBuffer := BackupBuffer;
        end;
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

  procedure ClearSiblingRadioButtons(RadioButton: TRadioButton);
  var
    Parent: TWinControl;
    Sibling: TControl;
    WinControl: TWinControlAccess absolute Sibling;
    LParamFlag: LRESULT;
    i: Integer;
  begin
    Parent := RadioButton.Parent;
    for i:= 0 to Parent.ControlCount - 1 do
    begin
      Sibling := Parent.Controls[i];
      if (Sibling is TRadioButton) and (Sibling <> RadioButton) then
      begin
        // Pass previous state through LParam so the event handling can decide
        // when to propagate LM_CHANGE (New State <> Previous State)
        LParamFlag := Windows.SendMessage(WinControl.WindowHandle, BM_GETCHECK, 0, 0);
        // Pass SKIP_LMCHANGE through LParam if previous state is already unchecked
        if LParamFlag = BST_UNCHECKED then
          LParamFlag := SKIP_LMCHANGE;
        Windows.SendMessage(WinControl.WindowHandle, BM_SETCHECK,
          Windows.WParam(BST_UNCHECKED), Windows.LParam(LParamFlag));
      end;
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
    if Info.hwndItem <> Info.hwndCombo then
      Buddy := Info.hwndItem
    else
      Buddy := 0;
    if Buddy <> 0 then
      DisposeWindowInfo(Buddy);
  end;

  procedure HandleScrollMessage(LMsg: integer);
  var
    ScrollInfo: TScrollInfo;
  begin
    with LMScroll do
    begin
      Msg := LMsg;
      ScrollCode := LOWORD(WParam);
      SmallPos := 0;
      ScrollBar := HWND(LParam);
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
      then ScrollInfo.nTrackPos := Integer(WindowInfo^.TrackPos and $FFFF0000) or HIWORD(WParam)
      else ScrollInfo.nTrackPos := HIWORD(WParam);
    end
    else begin
      ScrollInfo.fMask := SIF_POS;
      ScrollInfo.nPos := HIWORD(WParam);
    end;

    if LParam <> 0
    then begin
      // The message is send by a scrollbar
      GetScrollInfo(HWND(LParam), SB_CTL, ScrollInfo);
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
      then LMScroll.Pos := LongInt(WindowInfo^.TrackPos and $FFFF0000) or HIWORD(WParam)
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
      ACursor := Screen.Cursor;
      if ACursor = crDefault then
      begin
        Windows.GetCursorPos(Windows.POINT(P));
        Windows.ScreenToClient(Window, Windows.POINT(P));
        if GetLCLClientBoundsOffset(lWinControl, BoundsOffset) then
        begin
          Dec(P.X, BoundsOffset.Left);
          Dec(P.Y, BoundsOffset.Top);
        end;
        // TGraphicControl controls do not get WM_SETCURSOR messages...
        lControl := lWinControl.ControlAtPos(P, [capfOnlyClientAreas,
          capfAllowWinControls, capfHasScrollOffset, capfRecursive]);
        if lControl = nil then
          lControl := lWinControl;
        ACursor := lControl.Cursor;
      end;
      if ACursor <> crDefault then
      begin
        // DebugLn('Set cursor. Control = ', LControl.Name, ' cur = ',ACursor);
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
    WinProcess := False;
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
          if (ParentForm <> nil) and ((ParentForm.Menu = nil) or (csDesigning in ParentForm.ComponentState))
            and (Application <> nil) and (Application.MainForm <> nil)
            and (Application.MainForm <> ParentForm)
            and Application.MainForm.HandleAllocated then
          begin
            TargetWindow := Application.MainFormHandle;
            if IsWindowEnabled(TargetWindow) and IsWindowVisible(TargetWindow) then
            begin
              prevFocus := Windows.GetFocus;
              Windows.SetFocus(targetWindow);
              PLMsg^.Result := Windows.SendMessage(TargetWindow, WM_SYSCOMMAND, WParam, LParam);
              Windows.SetFocus(prevFocus);
              WinProcess := False;
            end;
          end;
        end;

      SC_MINIMIZE:
        begin
          if Assigned(Application) then
          begin
            if Assigned(lWinControl) and (Application.MainForm = lWinControl) and not Application.MainFormOnTaskBar then
              Window := Win32WidgetSet.AppHandle;//redirection

            if (Window = Win32WidgetSet.AppHandle) and not Application.MainFormOnTaskBar then
            begin
              HidePopups(Win32WidgetSet.AppHandle);
              if Assigned(Application.MainForm) then
              begin
                Windows.SetWindowPos(Window, HWND_TOP,
                  Application.MainForm.Left, Application.MainForm.Top,
                  Application.MainForm.Width, 0, SWP_NOACTIVATE);
                if Application.MainForm.HandleAllocated then
                  Windows.ShowWindow(Application.MainFormHandle, SW_HIDE);
              end;

              PLMsg^.Result := Windows.DefWindowProc(Window, WM_SYSCOMMAND, WParam, LParam);
              WinProcess := False;
              Application.IntfAppMinimize;
            end
            else
            if Assigned(lWinControl) and (lWinControl = Application.MainForm) then
            begin
              PLMsg^.Result := Windows.DefWindowProc(Window, WM_SYSCOMMAND, WParam, LParam);
              WinProcess := False;
              Application.IntfAppMinimize;
            end;
          end;
        end;

      SC_RESTORE:
        begin
          if Assigned(Application) then
          begin
            if (Window = Win32WidgetSet.AppHandle) and not Application.MainFormOnTaskBar then
            begin
              PLMsg^.Result := Windows.DefWindowProc(Window, WM_SYSCOMMAND, WParam, LParam);
              WinProcess := False;
              if Assigned(Application.MainForm) and Application.MainForm.HandleAllocated then
              begin
                if Application.MainForm.HandleObjectShouldBeVisible then
                  Windows.ShowWindow(Application.MainFormHandle, SW_SHOWNA);
                RestorePopups;
              end;
              Application.IntfAppRestore;
            end
            else
            if Assigned(lWinControl) and (lWinControl = Application.MainForm) then
            begin
              Application.IntfAppRestore;
            end;
          end;
        end;
    end;
  end;

  function IsComboEditSelection: boolean;
  begin
    Result := WindowInfo^.isComboEdit and (ComboBoxHandleSizeWindow = Windows.GetParent(Window));
  end;

  procedure HandleBitBtnCustomDraw(ABitBtn: TCustomBitBtn);
  var
    DrawInfo: PNMCustomDraw absolute NMHdr;
    ARect: TRect;
    ShowFocus: Boolean;
  begin
    case DrawInfo^.dwDrawStage of
      CDDS_PREPAINT, CDDS_POSTPAINT:
      begin
        lmNotify.Result := CDRF_DODEFAULT or CDRF_NOTIFYPOSTPAINT;
        WinProcess := False;
        if ABitBtn.Focused then
        begin
          if WindowsVersion >= wv2000 then
            ShowFocus := (Windows.SendMessage(ABitBtn.Handle, WM_QUERYUISTATE, 0, 0) and UISF_HIDEFOCUS) = 0
          else
            ShowFocus := True;
          if ShowFocus then
          begin
            ARect := DrawInfo^.rc;
            InflateRect(ARect, -3, -3);
            if not IsRectEmpty(ARect) then
              Windows.DrawFocusRect(DrawInfo^.hdc, ARect);
          end;
        end;
      end;
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
          Files[I] := UTF16ToUTF8(WideBuffer);
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
          TCustomForm(lWinControl).IntfDropFiles(Files);
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
  function HandleUnicodeChar(var AChar: Word): boolean;
  var
    OldUTF8Char, UTF8Char: TUTF8Char;
    WS: WideString;
  begin
    Result := False;
    UTF8Char := UTF16ToUTF8(WideString(WideChar(AChar)));
    OldUTF8Char := UTF8Char;
    if Assigned(lWinControl) then
    begin
      // if somewhere key is changed to '' then don't process this message
      WinProcess := not lWinControl.IntfUTF8KeyPress(UTF8Char, 1, False);
      // if somewhere key is changed then don't perform a regular keypress
      Result := not WinProcess or (UTF8Char <> OldUTF8Char);
      if UTF8Char <> OldUTF8Char then
      begin
        WS := UTF8ToUTF16(UTF8Char);
        if Length(WS) > 0 then
          AChar := Word(WS[1])
        else
          AChar := 0;
      end;
    end;
  end;

  procedure UpdateUIState(CharCode: Word);
  // This piece of code is taken from ThemeMgr.pas of Mike Lischke
  // Beginning with Windows 2000 the UI in an application may hide focus rectangles and accelerator key indication.
  // We have to take care to show them if the user starts navigating using the keyboard.

    function FindParentForm: TCustomForm; inline;
    begin
      if lWinControl <> nil then
        Result := GetParentForm(lWinControl)
      else
      if Application <> nil then
        Result := Application.MainForm
      else
        Result := nil;
    end;

  var
    ParentForm: TCustomForm;
  begin
    case CharCode of
      VK_LEFT..VK_DOWN, VK_TAB:
        begin
          ParentForm := FindParentForm;
          if ParentForm <> nil then
            SendMessage(ParentForm.Handle, WM_CHANGEUISTATE, MakeLong(UIS_CLEAR, UISF_HIDEFOCUS), 0);
        end;
      VK_MENU:
        begin
          ParentForm := FindParentForm;
          if ParentForm <> nil then
            SendMessage(ParentForm.Handle, WM_CHANGEUISTATE, MakeLong(UIS_CLEAR, UISF_HIDEACCEL), 0);
        end;
    end;
  end;

begin
  FillChar(LMessage, SizeOf(LMessage), 0);
  PLMsg := @LMessage;
  WinProcess := True;
  NotifyUserInput := False;

  // WriteLn('Msg: ', WM_To_String(Msg));
  WindowInfo := GetWin32WindowInfo(Window);
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
    {for ComboBox IME sends WM_IME_NOTIFY with WParam=WM_IME_ENDCOMPOSITION}
    if (Msg = WM_IME_NOTIFY) and (WPARAM=WM_IME_ENDCOMPOSITION) then
      WindowInfo^.IMEComposed:=True;
    
    // filter messages we want to pass on to LCL
    if (Msg <> WM_KILLFOCUS) and (Msg <> WM_SETFOCUS) and (Msg <> WM_NCDESTROY)
      and not ((Msg >= WM_CUT) and (Msg <= WM_CLEAR))
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

  case Msg of
    WM_NULL:
      if (Window = Win32WidgetSet.AppHandle) then
      begin
        CheckSynchronize;
        TWin32Widgetset(Widgetset).CheckPipeEvents;
      end;
    WM_ENTERIDLE: Application.Idle(False);
    WM_ACTIVATE:
    begin
      case LOWORD(WParam) of
        WA_ACTIVE, WA_CLICKACTIVE:
        begin
          LMessage.Msg := LM_ACTIVATE;
          LMessage.WParam := WParam;
          LMessage.LParam := LParam;
        end;
        WA_INACTIVE:
        begin
          LMessage.Msg := LM_DEACTIVATE;
          LMessage.WParam := WParam;
          LMessage.LParam := LParam;
        end;
      end;
    end;
    WM_IME_ENDCOMPOSITION:
    begin
      {IME Windows the composition has finished}
      if Assigned(WindowInfo) then WindowInfo^.IMEComposed:=True;    
    end;
    WM_CANCELMODE:
    begin
      LMessage.Msg := LM_CANCELMODE;
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
      OrgCharCode := Word(WParam);
      if not HandleUnicodeChar(OrgCharCode) then
      begin
        PLMsg := @LMChar;
        with LMChar do
        begin
          Msg := CN_CHAR;
          KeyData := LParam;
          if UnicodeEnabledOS then
            CharCode := Word(Char(WideChar(WParam)))
          else
            CharCode := Word(WParam);
          OrgCharCode := CharCode;
          Result := 0;
        end;
        WinProcess := false;
      end
      else
        WParam := OrgCharCode;
      {$else}
      PLMsg:=@LMChar;
      with LMChar do
      begin
        Msg := CN_CHAR;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
      end;
      WinProcess := false;
      {$endif}
    end;

    WM_MENUCHAR:
    begin
      PLMsg^.Result := FindMenuItemAccelerator(LOWORD(WParam), HMENU(LParam));
      WinProcess := false;
    end;

    WM_CLOSE:
    begin
      if (Window = Win32WidgetSet.AppHandle) and Assigned(Application.MainForm) then
        Windows.SendMessage(Application.MainFormHandle, WM_CLOSE, 0, 0)
      else
        LMessage.Msg := LM_CLOSEQUERY;
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
    
    WM_MENUSELECT:
    begin
      TargetObject := GetMenuItemObject((HIWORD(Integer(WParam)) and MF_POPUP) <> 0);
      if TargetObject is TMenuItem then
        TMenuItem(TargetObject).IntfDoSelect
      else
        Application.Hint := '';
    end;
    
    WM_COMMAND:
    begin
      if LParam = 0 then
      begin
        {menuitem or shortcut}
        TargetObject := GetMenuItemObject(False);
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
        ChildWindowInfo := GetWin32WindowInfo(HWND(LParam));
        lWinControl := ChildWindowInfo^.WinControl;
        // buddy controls use 'awincontrol' to designate associated wincontrol
        if lWinControl = nil then
          lWinControl := ChildWindowInfo^.AWinControl;

        if Assigned(ChildWindowInfo^.ParentMsgHandler) then
        begin
          if ChildWindowInfo^.ParentMsgHandler(lWinControl,
            Window, WM_COMMAND, WParam, LParam, LMessage.Result, WinProcess) then Exit(LMessage.Result);
        end;

        // TToggleBox is a TCustomCheckBox too, but we don't want to handle
        // state changes of TToggleBox ourselfves
        if (lWinControl is TCustomCheckBox) and not (lWinControl is TToggleBox) then
        begin
          case HIWORD(WParam) of
            BN_CLICKED:
              begin
                // to allow cbGrayed state at the same time as not AllowGrayed
                // in checkboxes (needed by dbcheckbox for null fields) we need
                // to handle checkbox state ourselves, according to msdn state
                // sequence goes from checked->cleared->grayed etc.
                Flags := SendMessage(lWinControl.Handle, BM_GETCHECK, 0, 0);
                //do not update the check state if is TRadioButton and is already checked
                if (Flags <> BST_CHECKED) or not (lWinControl is TRadioButton) then
                begin
                  if (Flags=BST_CHECKED) then
                    Flags := BST_UNCHECKED
                  else
                  if (Flags=BST_UNCHECKED) and
                     TCustomCheckbox(lWinControl).AllowGrayed then
                    Flags := BST_INDETERMINATE
                  else
                    Flags := BST_CHECKED;
                  //pass 0 through LParam to force sending LM_CHANGE
                  Windows.SendMessage(lWinControl.Handle, BM_SETCHECK,
                    Windows.WPARAM(Flags), 0);
                end;
                LMessage.Msg := LM_CLICKED;
              end;
            BN_KILLFOCUS:
              LMessage.Msg := LM_EXIT;
          end
        end else
        if lWinControl is TButtonControl then
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
            EN_CHANGE: LMessage.Msg := CM_TEXTCHANGED;
          end;
        end
        else if (lWinControl is TCustomListBox) then
          case HIWORD(WParam) of
            LBN_SELCHANGE:  LMessage.Msg := LM_SELCHANGE;
          end
        else if lWinControl is TCustomCombobox then
          case HIWORD(WParam) of
            CBN_DROPDOWN: (lWinControl as TCustomCombobox).IntfGetItems;
            CBN_EDITCHANGE: LMessage.Msg := LM_CHANGED;
            { CBN_EDITCHANGE is only sent after the user changes the edit box.
              CBN_SELCHANGE is sent when the user changes the text by
              selecting in the list, but before text is actually changed.
              itemindex is updated, so set text manually }
            CBN_SELCHANGE:
            begin
              UpdateComboBoxText(TCustomComboBox(lWinControl));
              SendSimpleMessage(lWinControl, LM_CHANGED);
              LMessage.Msg := LM_SELCHANGE;
            end;
            CBN_CLOSEUP:
            begin
              // according to msdn CBN_CLOSEUP can happen before CBN_SELCHANGE and
              // unfortunately it is simple truth. but we need correct order in the LCL
              PostMessage(lWinControl.Handle, CN_COMMAND, WParam, LParam);
              Exit;
            end;
          end;
      end;

      // no specific message found? try send a general msg
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
      ChildWindowInfo := GetWin32WindowInfo(HWND(LParam));
      if Assigned(ChildWindowInfo^.ParentMsgHandler) then
      begin
        if ChildWindowInfo^.ParentMsgHandler(lWinControl,
          Window, Msg, WParam, LParam, LMessage.Result, WinProcess) then Exit(LMessage.Result);
      end;
      WindowDC := HDC(WParam);
      ChildWinControl := ChildWindowInfo^.WinControl;
      if ChildWinControl = nil then
        ChildWinControl := ChildWindowInfo^.AWinControl;

      case Msg of
        WM_CTLCOLORSTATIC,
        WM_CTLCOLORBTN: begin
          if GetNeedParentPaint(ChildWindowInfo, ChildWinControl) and
            not ChildWindowInfo^.ThemedCustomDraw then
          begin
            // need to draw transparently, draw background
            DrawParentBackground(HWND(LParam), WindowDC);
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
          WindowColor := ChildWinControl.Font.Color;
          if WindowColor = clDefault then
            WindowColor := ChildWinControl.GetDefaultColor(dctFont);
          Windows.SetTextColor(WindowDC, ColorToRGB(WindowColor));
          WindowColor := ChildWinControl.Brush.Color;
          if WindowColor = clDefault then
            WindowColor := ChildWinControl.GetDefaultColor(dctBrush);
          Windows.SetBkColor(WindowDC, ColorToRGB(WindowColor));
          LMessage.Result := LResult(ChildWinControl.Brush.Reference.Handle);
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
      if CurrentWindow=Window then
        CurrentWindow := 0;
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
          DrawMenuItem(TMenuItem(menuItem), PDrawItemStruct(LParam)^._hDC,
             PDrawItemStruct(LParam)^.rcItem,
             PDrawItemStruct(LParam)^.itemAction,
             PDrawItemStruct(LParam)^.itemState);
        end;
        with TLMDrawItems(LMessage) do
        begin
          Msg := LM_DRAWITEM;
          Ctl := 0;
          DrawItemStruct := PDrawItemStruct(LParam);
        end;
        WinProcess := false;
      end
      else
      begin
        WindowInfo := GetWin32WindowInfo(PDrawItemStruct(LParam)^.hwndItem);
        if WindowInfo^.WinControl<>nil then
          lWinControl := WindowInfo^.WinControl;
        {$IFDEF MSG_DEBUG}
        with PDrawItemStruct(LParam)^ do
          debugln(format('Received WM_DRAWITEM type %d handle %x', [ctlType, integer(hwndItem)]));
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
            with DrawListItemStruct do
            begin
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
        end else
        begin
          with TLMDrawItems(LMessage) do
          begin
            Msg := LM_DRAWITEM;
            Ctl := 0;
            DrawItemStruct := PDrawItemStruct(LParam);
          end;
          WinProcess := false;
        end;
      end;
    end;
    WM_ENABLE:
    begin
      if WParam <> 0 Then
        LMessage.Msg := LM_SETEDITABLE;
      if Window = Win32WidgetSet.AppHandle then
        if WParam = 0 then
        begin
          RemoveStayOnTopFlags(Window);
          DisabledForms := Screen.DisableForms(nil, DisabledForms);
        end
        else
        begin
          RestoreStayOnTopFlags(Window);
          Screen.EnableForms(DisabledForms);
        end;
        
      // disable child windows of for example groupboxes, but not of forms
      if Assigned(lWinControl) and not (lWinControl is TCustomForm) then
        EnableChildWindows(lWinControl, WParam<>0);

      // ugly hack to give bitbtns a nice look
      // When no theming active, the internal image needs to be
      // recreated when the enabled state is changed
      if not ThemeServices.ThemesEnabled
      and (lWinControl is TCustomBitBtn)
      then DrawBitBtnImage(TCustomBitBtn(lWinControl), TCustomBitBtn(lWinControl).Caption);
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
        if CurDoubleBuffer.DC <> 0 then
          WParam := Windows.WParam(CurDoubleBuffer.DC);
        if WindowInfo^.isTabPage then
          EraseBkgndStack := (EraseBkgndStack and not ((1 shl EraseBkgndStackShift)-1))
            or dword(ecDiscardNoRemove);
      end
      else
      if eraseBkgndCommand <> ecDiscardNoRemove then
        EraseBkgndStack := EraseBkgndStack shr EraseBkgndStackShift;
      if eraseBkgndCommand in [ecDiscard, ecDiscardNoRemove] then
      begin
        Result := 0;
        exit;
      end;
      if not GetNeedParentPaint(WindowInfo, lWinControl) or (eraseBkgndCommand = ecDoubleBufferNoRemove) then
      begin
        LMessage.Msg := LM_ERASEBKGND;
        LMessage.WParam := WParam;
        LMessage.LParam := LParam;
      end else
      begin
        if not ThemeServices.ThemesEnabled then
          SendPaintMessage(HDC(WParam));
        LMessage.Result := 1;
      end;
      WinProcess := False;
    end;
    WM_EXITMENULOOP:
      // is it a popup menu
      if longbool(WPARAM) and Assigned(WindowInfo^.PopupMenu) then
        WindowInfo^.PopupMenu.Close;
    WM_GETDLGCODE:
    begin
      LMessage.Result := DLGC_WANTALLKEYS;
      WinProcess := False;
    end;
    WM_HELP:
    begin
      LMessage.Msg := LM_HELP;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
      // Don't ask windows to process the message here. It will be processed
      // either by TCustomForm LM_HELP handler or passed to parent by DefaultHandler
      WinProcess := False;
    end;
    WM_HOTKEY:
    begin
      LMessage.Msg := WM_HOTKEY;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
      WinProcess := false;
    end;
    WM_HSCROLL,
    WM_VSCROLL:
    begin
      PLMsg := @LMScroll;
      if LParam <> 0 then
      begin
        ChildWindowInfo := GetWin32WindowInfo(HWND(LParam));
        lWinControl := ChildWindowInfo^.WinControl;
        if Assigned(ChildWindowInfo^.ParentMsgHandler) then
        begin
          if ChildWindowInfo^.ParentMsgHandler(lWinControl,
            Window, Msg, WParam, LParam, PLMsg^.Result, WinProcess) then Exit(PLMsg^.Result);
        end;
      end;
      HandleScrollMessage(Msg);
    end;
    WM_KEYDOWN:
    begin
      NotifyUserInput := True;
      PLMsg := @LMKey;
      UpdateUIState(Word(WParam));
      with LMKey Do
      begin
        Msg := CN_KEYDOWN;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
      end;
      if Assigned(WindowInfo) then WindowInfo^.IMEComposed:=False;
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
      end;
      WinProcess := false;
      if Assigned(WindowInfo) and WindowInfo^.IMEComposed then LMKey.Msg:=LM_NULL;
    end;
    WM_KILLFOCUS:
    begin
{$ifdef DEBUG_CARET}
      DebugLn(['WM_KILLFOCUS received for window ', IntToHex(Window, 8), ' NewFocus = ', IntToHex(WParam, 8), ' Text = ', WndText(WParam)]);
{$endif}
      LMessage.Msg := LM_KILLFOCUS;
      LMessage.WParam := WParam;
    end;
    //TODO:LM_KILLCHAR,LM_KILLWORD,LM_KILLLINE
    WM_LBUTTONDBLCLK:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      // always within the time-window
      if (MouseDownCount < 1) or (MouseDownCount > 4) then MouseDownCount := 1;
      inc(MouseDownCount);
      MouseDownTime := GetTickCount;
      with LMMouse Do
      begin
        case MouseDownCount of
          2: Msg := LM_LBUTTONDBLCLK;
          3: Msg := LM_LBUTTONTRIPLECLK;
          4: Msg := LM_LBUTTONQUADCLK;
          else Msg := LM_LBUTTONDOWN;
        end;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
      end;
    end;
    WM_LBUTTONDOWN:
    begin
      if (MouseDownCount < 1) or (MouseDownCount > 4) then MouseDownCount := 1;
      // if mouse-click, focus-change, mouse-click, cursor hasn't moved:
      // simulate double click, assume focus change due to first mouse-click
      if (MouseDownFocusStatus = mfFocusChanged) and (MouseDownFocusWindow = Window)
          and (GetTickCount - MouseDownTime <= GetDoubleClickTime)
          and CheckMouseMovement then
      begin
        inc(MouseDownCount);
        PostMessage(Window, WM_LBUTTONDBLCLK, WParam, LParam);
      end
      else if (MouseDownWindow = Window)
          and (GetTickCount - MouseDownTime <= GetDoubleClickTime)
          and CheckMouseMovement then
        inc(MouseDownCount)
      else
        MouseDownCount := 1;

      MouseDownTime := GetTickCount;
      MouseDownWindow := Window;
      MouseDownFocusWindow := 0;
      MouseDownFocusStatus := mfFocusSense;
      GetCursorPos(MouseDownPos);
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        case MouseDownCount of
          2: Msg := LM_LBUTTONDBLCLK;
          3: Msg := LM_LBUTTONTRIPLECLK;
          4: Msg := LM_LBUTTONQUADCLK;
          else Msg := LM_LBUTTONDOWN;
        end;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
      end;
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
    WM_XBUTTONDBLCLK:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_XBUTTONDBLCLK;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
      end;
    end;
    WM_XBUTTONDOWN:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_XBUTTONDOWN;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
      end;
    end;
    WM_XBUTTONUP:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_XBUTTONUP;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
      end;
    end;
    WM_MOUSEHOVER:
    begin
      NotifyUserInput := True;
      LMessage.Msg := LM_MOUSEENTER;
    end;
    WM_MOUSELEAVE:
    begin
      NotifyUserInput := True;
      LMessage.Msg := LM_MOUSELEAVE;
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
        // check if mouse cursor within this window, otherwise send message to
        // window the mouse is hovering over
        P.X := X;
        P.Y := Y;
        TargetWindow := TWin32WidgetSet(WidgetSet).WindowFromPoint(P);
        if (TargetWindow = 0) or not IsWindowEnabled(TargetWindow) then
          exit;

        // check if the window is an edit control of a combobox, if so,
        // redirect it to the combobox, not the edit control
        if GetWin32WindowInfo(TargetWindow)^.isComboEdit then
          TargetWindow := Windows.GetParent(TargetWindow);

        // check InMouseWheelRedirection to prevent recursion
        if not InMouseWheelRedirection and (TargetWindow <> Window) then
        begin
          InMouseWheelRedirection := true;
          Result := SendMessage(TargetWindow, WM_MOUSEWHEEL, WParam, LParam);
          InMouseWheelRedirection := false;
          exit;
        end
        else
        if TargetWindow <> Window then
          exit;
        // the mousewheel message is for us
        Msg := LM_MOUSEWHEEL;
        Windows.ScreenToClient(TargetWindow, P);
        X := P.X;
        Y := P.Y;
        Button := LOWORD(Integer(WParam));
        WheelDelta := SmallInt(HIWORD(Integer(WParam)));
        State := KeysToShiftState(Button);
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
    WM_NCHITTEST:
    begin
      LMessage.msg := LM_NCHITTEST;
      LMessage.wParam := WParam;
      LMessage.lParam := LParam;
    end;
    WM_NCLBUTTONDOWN:
    begin
      LMessage.Msg := Msg;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
      NotifyUserInput := True;

      //Drag&Dock support TCustomForm => Start BeginDrag()
      if (lWinControl <> nil) and not (csDesigning in lWinControl.ComponentState) then
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
      // I see no other way to prevent crash at moment. This message calls WM_CLOSE
      // which frees our form and we get a destructed lWinControl
      lWinControl := nil;
    end;
    WM_NCMOUSEMOVE, WM_NCMOUSEHOVER:
    begin
      LMessage.Msg := Msg;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
      NotifyUserInput := True;
      Application.DoBeforeMouseMessage(nil);
    end;
    WM_NOTIFY:
    begin
      WindowInfo := GetWin32WindowInfo(PNMHdr(LParam)^.hwndFrom);
{$ifdef MSG_DEBUG}
      DebugLn([MessageStackDepth, 'Notify code: ', PNMHdr(LParam)^.code]);
{$endif}
      if Assigned(WindowInfo) and Assigned(WindowInfo^.ParentMsgHandler) then
      begin
        LMNotify.Result := 0;
        if WindowInfo^.ParentMsgHandler(WindowInfo^.WinControl,
          Window, WM_NOTIFY, WParam, LParam, LMNotify.Result, WinProcess) then Exit(LMNotify.Result);
      end;
      case PNMHdr(LParam)^.code of
        MCN_SELCHANGE:
        begin
          LMessage.Msg := LM_CHANGED;
          if WindowInfo^.WinControl <> nil then
            lWinControl := WindowInfo^.WinControl;
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
              NM_CUSTOMDRAW:
              begin
                if WindowInfo^.WinControl is TCustomBitBtn then
                  HandleBitBtnCustomDraw(TCustomBitBtn(WindowInfo^.WinControl))
                else
                if GetNeedParentPaint(WindowInfo, lWinControl) and WindowInfo^.ThemedCustomDraw then
                begin
                  case PNMCustomDraw(LParam)^.dwDrawStage of
                    CDDS_PREPAINT:
                    begin
                      Result := CDRF_NOTIFYITEMDRAW;
                      WinProcess := false;
                    end;
                    CDDS_ITEMPREPAINT:
                    begin
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
      WinProcess := False;
      NotifyUserInput := True;
      PLMsg := @LMContextMenu;
      with LMContextMenu do
      begin
        Msg := LM_CONTEXTMENU;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        hWnd := Window;
        Result := 0;
      end;
    end;
    WM_SETCURSOR:
    begin
      HandleSetCursor;
    end;
    CM_ACTIVATE:
    begin
      if (Window = Win32WidgetSet.AppHandle) then
      begin
        if not IsIconic(Window) and IsWindow(WindowLastFocused) then
          SetFocus(WindowLastFocused);
        Result := 0;
        Exit;
      end;
      WinProcess := False;
    end;
    WM_SETFOCUS:
    begin
{$ifdef DEBUG_CARET}
      DebugLn('WM_SETFOCUS received for window ', IntToHex(Window, 8));
{$endif}
      // move focus to another application window but process event first
      if (Window = Win32WidgetSet.AppHandle) then
        PostMessage(Window, CM_ACTIVATE, 0, 0)
      else
        WindowLastFocused := Window;
      // handle feature mouse-click, setfocus, mouse-click -> double-click
      if (Window <> MouseDownWindow) and (MouseDownFocusStatus <> mfNone) then
      begin
        MouseDownFocusStatus := mfFocusChanged;
        MouseDownFocusWindow := Window;
      end;
      LMessage.Msg := LM_SETFOCUS;
    end;
    WM_SHOWWINDOW:
    begin
      with TLMShowWindow(LMessage) Do
      begin
        Msg := LM_SHOWWINDOW;
        Show := WParam <> 0;
        Status := LParam;
      end;
      if Assigned(lWinControl) and ((WParam <> 0) or not lWinControl.Visible) and
          ((WParam = 0) or lWinControl.Visible) and Assigned(Application) and
          (lWinControl = Application.MainForm) and not Application.MainFormOnTaskBar then
      begin
        if WParam=0 then
          Flags := SW_HIDE
        else
          Flags := SW_SHOWNOACTIVATE;
        Windows.ShowWindow(Win32WidgetSet.AppHandle, Flags);
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
      end;
      WinProcess := false;
    end;
    WM_SYSCOMMAND:
    begin
      HandleSysCommand;
      LMessage.Msg := Msg;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
      WmSysCommandProcess := WinProcess;
      WinProcess := False;
    end;
    WM_SYSKEYDOWN:
    begin
      NotifyUserInput := True;
      UpdateUIState(Word(WParam));
      PLMsg := @LMKey;
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
    WM_WINDOWPOSCHANGING:
    begin
      with TLMWindowPosMsg(LMessage) Do
      begin
        Msg := LM_WINDOWPOSCHANGING;
        Unused := WParam;
        WindowPos := PWindowPos(LParam);
      end;
    end;
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
        Windows.InvalidateRect(Window, nil, True);
    end;
    WM_MEASUREITEM:
    begin
      case PMeasureItemStruct(LParam)^.CtlType of
        ODT_MENU:
          begin
            menuItem := TObject(PMeasureItemStruct(LParam)^.itemData);
            if menuItem is TMenuItem then
            begin
              menuHDC := GetDC(Window);
              TmpSize := MenuItemSize(TMenuItem(menuItem), menuHDC);
              PMeasureItemStruct(LParam)^.itemWidth := TmpSize.cx;
              PMeasureItemStruct(LParam)^.itemHeight := TmpSize.cy;
              ReleaseDC(Window, menuHDC);
              Winprocess := False;
            end
            else
              DebugLn('WM_MEASUREITEM for a menuitem catched but menuitem is not TmenuItem');
          end;
        else
        if WParam <> 0 then
        begin
          lWinControl := TWinControl(WParam);
          if Assigned(lWinControl) then
          begin
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
      if Window = Win32WidgetSet.AppHandle then
      begin
        ThemeServices.UpdateThemes;
        Graphics.UpdateHandleObjects;
        ThemeServices.IntfDoOnThemeChange;
      end;
    end;
    WM_UPDATEUISTATE:
    begin
      if ThemeServices.ThemesEnabled then
        InvalidateRect(Window, nil, True);
    end;

    { >= WM_USER }

    WM_LCL_SOCK_ASYNC:
    begin
      if (Window = Win32WidgetSet.AppHandle) and
          Assigned(Win32WidgetSet.FOnAsyncSocketMsg) then
        Exit(Win32WidgetSet.FOnAsyncSocketMsg(WParam, LParam))
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
    WM_ACTIVATEAPP:
    begin
      if Window = Win32WidgetSet.AppHandle then
      begin
        if WParam <> 0 then // activated
        begin
          //WriteLn('Restore');
          RestoreStayOnTopFlags(Window);
          if Assigned(Application) then
            Application.IntfAppActivate;
        end
        else
        begin // deactivated
          //WriteLn('Remove');
          RemoveStayOnTopFlags(Window);
          if Assigned(Application) then
            Application.IntfAppDeactivate;
        end;
      end;
    end;
    WM_MOVE:
    begin
      PLMsg:=@LMMove;
      with LMMove Do
      begin
        Msg := LM_MOVE;
        // MoveType := WParam;   WParam is not defined!
        MoveType := Move_SourceIsInterface;
        if GetWindowLong(Window, GWL_STYLE) and WS_CHILD = 0 then
        begin
          WindowPlacement.length := SizeOf(WindowPlacement);
          if IsIconic(Window) and GetWindowPlacement(Window, @WindowPlacement) then
          begin
            with WindowPlacement.rcNormalPosition do
            begin
              XPos := Left;
              YPos := Top;
            end;
          end
          else
          if Windows.GetWindowRect(Window, @R) then
          begin
            XPos := R.Left;
            YPos := R.Top;
          end
          else
            Msg := LM_NULL;
        end else
        begin
          if GetWindowRelativePosition(Window, NewLeft, NewTop) then
          begin
            XPos := NewLeft;
            YPos := NewTop;
          end
          else
            Msg := LM_NULL;
        end;
        if lWinControl <> nil then
        begin
          {$IFDEF VerboseSizeMsg}
          DebugLn('Win32CallBack WM_MOVE ', dbgsName(lWinControl),
            ' NewPos=',dbgs(XPos),',',dbgs(YPos));
          {$ENDIF}
          if (lWinControl.Left = XPos) and (lWinControl.Top = YPos) then
            Exit;
        end;
      end;
    end;
    WM_SIZE:
    begin
      with TLMSize(LMessage) do
      begin
        Msg := LM_SIZE;
        SizeType := WParam or Size_SourceIsInterface;
        if Window = Win32WidgetSet.AppHandle then
        begin
          if Assigned(Application.MainForm) and Application.MainForm.HandleAllocated then
          begin
            lWinControl := Application.MainForm;
            Window := Application.MainFormHandle;
          end;
        end;
        GetWindowSize(Window, NewWidth, NewHeight);
        Width := NewWidth;
        Height := NewHeight;
        if Assigned(lWinControl) then
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
          if (lWinControl.Width <> Width) or
             (lWinControl.Height <> Height) or
             lWinControl.ClientRectNeedsInterfaceUpdate then
            lWinControl.DoAdjustClientRectChange
          else
          // If we get form size message then we probably changed it state
          // (minimized/maximized -> normal). Form adjust its clientrect in the
          // second WM_SIZE but WM_MOVE also updates clientrect without adjustment
          // thus we need to call DoAdjustClientRectChange. It is safe since this
          // methods checks whether it need to adjust something really.
          if (lWinControl is TCustomForm) and (lWinControl.Parent = nil) and
             (WParam = Size_Restored) then
            lWinControl.DoAdjustClientRectChange(False);
        end;
        OverlayWindow := GetWin32WindowInfo(Window)^.Overlay;
        if OverlayWindow <> 0 then
          Windows.SetWindowPos(OverlayWindow, HWND_TOP, 0, 0, NewWidth, NewHeight, SWP_NOMOVE);
      end;
    end;
    BM_SETCHECK:
    begin
      //LParam holds BST_CHECKED, BST_UNCHECKED or SKIP_LMCHANGE;
      if LParam <> SKIP_LMCHANGE then
        LMessage.Msg := LM_CHANGED;
      if lWinControl is TRadioButton then
      begin
        //Uncheck siblings
        if WParam = BST_CHECKED then
          ClearSiblingRadioButtons(TRadioButton(lWinControl));
      end;
    end;
    WM_ENDSESSION:
    begin
      if Assigned(Application) and (Win32WidgetSet.AppHandle = Window) and
         (WParam > 0) then
      begin
        // look at WM_QUERYENDSESSION about LParam
        LMessage.Msg := LM_NULL; // no need to go through delivermessage
        Application.IntfEndSession();
        LMessage.Result := 0;
      end;
    end;

    WM_QUERYENDSESSION:
    begin
      if Assigned(Application) and (Win32WidgetSet.AppHandle = Window) then
      begin
        LMessage.Msg := LM_NULL; // no need to go through delivermessage
        CancelEndSession := LMessage.Result=0;
        // it is possible to pass whether user LogOff or Shutdonw through a flag
        // but seems there is no way to do this in a cross-platform way =>
        // skip it for now
        Application.IntfQueryEndSession(CancelEndSession);
        if CancelEndSession
          then LMessage.Result := 0
          else LMessage.Result := 1;
      end;
    end;
    WM_NCPAINT:
    begin
      if (lWinControl <> nil) and TWin32ThemeServices(ThemeServices).ThemesEnabled and
         not (lWinControl is TCustomForm) and (lWinControl is TCustomControl) then
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
  begin
    CurrentWindow := Window;
    NotifyApplicationUserInput(PLMsg^.Msg);
    if CurrentWindow=0 then
      // Invalidate associated lWinControl if current window has been destroyed
      lWinControl := nil;
  end;

  if Assigned(lWinControl) and (PLMsg^.Msg <> LM_NULL) then
    DeliverMessage(lWinControl, PLMsg^);

  // respond to result of LCL handling the message
  case PLMsg^.Msg of
    LM_ERASEBKGND, LM_SETCURSOR, LM_RBUTTONUP, LM_CONTEXTMENU, LM_MOUSEWHEEL:
    begin
      if PLMsg^.Result = 0 then
        WinProcess := True;
    end;
    WM_SYSCOMMAND:
    begin
      WinProcess := WmSysCommandProcess;
    end;

    CN_CHAR, CN_SYSCHAR:
    begin
      // if key not yet processed, let windows process it
      WinProcess := LMChar.Result = 0;
      {$IFDEF WindowsUnicodeSupport}
      if UnicodeEnabledOS then
      begin
        // if charcode was modified by LCL, convert ansi char
        // to unicode char, if not change was made WParam has
        // the right unicode char so just use it.
        if (LMChar.Result = 1) or (OrgCharCode <> LMChar.CharCode) then
          WParam := Word(WideChar(LMChar.CharCode));
      end
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

    if (Msg=WM_CHAR) and (WParam=VK_RETURN) and
       ((lWinControl is TCustomEdit) or (lWinControl is TCustomCombobox))
      // and not TCustomEdit(lWinControl).WindowsSpecificOptionToEnableBeep
    then
      // this thing will beep, don't call defaultWindowProc
    else
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
        if (lWinControl <> nil) and (PLMsg^.Result = 0) and (LMKey.CharCode<>0) then
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
      Owner := GetWin32WindowInfo(Parent)^.WinControl;
      P.x := GET_X_LPARAM(lParam);
      P.y := GET_Y_LPARAM(lParam);
      Windows.ScreenToClient(Parent, P);
      if (Owner is TCustomForm) then
      begin
        // ask form about control under mouse. we need TWinControl
        Control := Owner.ControlAtPos(P, [capfAllowWinControls, capfRecursive]);
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
