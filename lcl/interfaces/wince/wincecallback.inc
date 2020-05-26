{%MainUnit winceint.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
type
  TWinControlAccess = class(TWinControl);

{*************************************************************}
{            callback routines                                }
{*************************************************************}

{-----------------------------------------------------------------------------
  Function: PropEnumProc
  Params: Window - The window with the property
          Str    - The property name
          Data   - The property value
  Returns: Whether the enumeration should continue

  Enumerates and removes properties for the target window
 -----------------------------------------------------------------------------}
function PropEnumProc(Window: Hwnd; Str: PChar; Data: Handle): LongBool; cdecl;
begin
  Result:=false;
  if PtrUInt(Str) <= $FFFF then exit; // global atom handle
  //DebugLn('Trace:PropEnumProc - Start');
  //DebugLn(Format('Trace:PropEnumProc - Property %S (with value 0x%X) from window 0x%X removed',[String(Str), Data, Window]));
  RemoveProp(Window, Str);
  Result := True;
  //DebugLn('Trace:PropEnumProc - Exit');
end;

function WndClassName(Wnd: HWND): WideString; inline;
var
  winClassName: array[0..19] of WideChar;
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
var
  PrevWndProc: Windows.WNDPROC;
  setComboWindow: boolean;
  WindowInfo: PWindowInfo;
begin
  {$ifdef MSG_DEBUG}
    DebugLn('Trace:CallDefaultWindowProc - Start');
  {$endif}

  WindowInfo := GetWindowInfo(Window);
  PrevWndProc := WindowInfo^.DefWndProc;

  if (PrevWndProc = nil) or (PrevWndProc = @WindowProc) // <- prevent recursion
    or ((WindowInfo^.WinControl <> nil) and (WindowInfo^.WinControl is TTabSheet)) then
  begin
    {$ifdef MSG_DEBUG}
      DebugLn('Trace:CallDefaultWindowProc - A');
    {$endif}
    Result := Windows.DefWindowProcW(Window, Msg, WParam, LParam)
  end
  else begin
    {$ifdef MSG_DEBUG}
      DebugLn('Trace:CallDefaultWindowProc - B ' + IntToHex(PtrInt(PrevWndProc), 8));
    {$endif}
    // combobox child edit weirdness: combobox handling WM_SIZE will compare text
    // to list of strings, and if appears in there, will set the text, and select it
    // WM_GETTEXTLENGTH, WM_GETTEXT, WM_SETTEXT, EM_SETSEL
    // combobox sends WM_SIZE to itself indirectly, check recursion
    Result := Windows.CallWindowProc(PrevWndProc, Window, Msg, WParam, LParam);
  end;
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

function CheckMouseMovement: boolean;
  // returns true if mouse did not move between lmousebutton down
var
  lCursorPos: TPoint;
  moveX, moveY: integer;
begin
Result := true;
{  GetCursorPos(lCursorPos);
  moveX := lCursorPos.X - MouseDownPos.X;
  moveY := lCursorPos.Y - MouseDownPos.Y;
  Result := (-3 <= moveX) and (moveX <= 3) and (-3 <= moveY) and (moveY <= 3);}
end;

//roozbeh...any need for this?as we do not have opaque in wince!
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
    LParam: Windows.LParam): LResult; {$ifdef win32}stdcall{$else}cdecl{$endif};
Var
  LMessage: TLMessage;
  menuItem: TObject;
  menuHDC: HDC;
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
  WindowInfo: PWindowInfo;
  Flags: dword;
  ChildWindowInfo: PWindowInfo;
  WindowColor: Integer;

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

  OrgCharCode: word; // used in WM_CHAR handling


  NMHdr: PNMHdr absolute LParam; // used by WM_NOTIFY
  TmpSize: TSize; // used by WM_MEASUREITEM
  {$ifndef win32}
  Info: SHRGINFO; // used by SHRecognizeGesture in WM_LBUTTONDOWN
  {$endif}

  function ShowHideTabPage(NotebookHandle: HWnd; Showing: boolean): integer;
  var
    NoteBook: TCustomTabControl;
    PageIndex, Flags: Integer;
    PageHandle: HWND;
    WindowInfo: PWindowInfo;
  begin
    Notebook := GetWindowInfo(NotebookHandle)^.WinControl as TCustomTabControl;
    PageIndex := Windows.SendMessageW(NotebookHandle, TCM_GETCURSEL, 0, 0);
    PageIndex := NotebookPageRealToLCLIndex(Notebook, PageIndex);
    if PageIndex = -1 then exit;

    //    PageHandle := Notebook.CustomPage(PageIndex).Handle;
    WindowInfo := GetWindowInfo(Notebook.CustomPage(PageIndex).Handle);
    PageHandle := WindowInfo^.ParentPanel;

    if Showing then
      Flags := SW_SHOW
    else
      Flags := SW_HIDE;
    Windows.ShowWindow(PageHandle, Flags);
    Windows.RedrawWindow(PageHandle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_ERASE);
    Result := PageIndex;
  end;

  function GetMenuItemObject: TObject;
  var
    MenuInfo: MENUITEMINFO;
    MainMenuHandle: HMENU;
    PopupMenu: TPopupMenu;
    Index: Integer;
  begin
    Result := nil;
    FillChar(MenuInfo, SizeOf(MenuInfo), 0);
    MenuInfo.cbSize := SizeOf(MENUITEMINFO);
    MenuInfo.fMask := MIIM_DATA;
    {first we have to decide if the command is from a popup menu or from the window main menu}
    //if the 'PopupMenu' property exists, there is a big probability that the command is from a popup menu
    PopupMenu := WindowInfo^.PopupMenu;
    if PopupMenu <> nil then //processing popup menu
    begin
      WindowInfo^.PopupMenu := nil;
      // LOWORD(WParam) = MENUITEM_ID
      Result := PopupMenu.FindItem(LOWORD(WParam), fkCommand);
    end;
    {$ifndef win32}
    if Result = nil then //if Result is still nil, process main menu
    begin
      MainMenuHandle := SHFindMenuBar(Window);
      // roozbeh : this is the only way we have
      Index := MenuItemsList.IndexOf(IntToStr(LOWORD(WParam)));
      if (Index >= 0) and (Index < MenuItemsList.Count) then
        Result := MenuItemsList.Objects[Index];
    {
      This way returns False for GetMenuItemInfo(W) - extended error is so:
      This function is only valid in win32 mode

      if GetMenuItemInfo(MainMenuHandle, LOWORD(WParam), false, @MenuInfo) then
        Result := TObject(MenuInfo.dwItemData)
      else
        DebugLn(['GetMenuItemInfo failed: ', GetLastErrorText(GetLastError)]);
     }
    end;
    {$endif}

    {$ifdef VerboseWinCEMenu}
    if Result <> nil then
      DebugLn(Format('[wincecallback] GetMenuItemObject found menu item %s caption %s',
        [TMenuItem(Result).Name, TMenuItem(Result).Caption]));
    {$endif}
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
    ParentPaintWindow: HWND;
    WindowWidth, WindowHeight: Integer;
    DCIndex: integer;
    parLeft, parTop: integer;
    useDoubleBuffer: boolean;
    isNativeControl: boolean;
    needParentPaint: boolean;
    lNotebookFound: boolean;
    BufferWasSaved: Boolean;
    BackupBuffer: TDoubleBuffer;
  begin
    // note: ignores the received DC
    // do not use default deliver message
    if lWinControl = nil then
    begin
      lWinControl := GetWindowInfo(Window)^.PWinControl;
      if lWinControl = nil then exit;
    end;

    {$IFDEF DEBUG_WINCE_LABELS}
    DebugLn(Format('[SendPaintMessage]: Control:%s', [lWinControl.Name]));
    {$ENDIF}

    // create a paint message
    isNativeControl := GetIsNativeControl(Window);
    ParentPaintWindow := 0;
    needParentPaint := GetNeedParentPaint(WindowInfo, lWinControl);
    // if needParentPaint and not isTabPage then background will be drawn in
    // WM_ERASEBKGND and WM_CTLCOLORSTATIC for native controls
    // sent by default paint handler
    if {WindowInfo^.isTabPage or} (needParentPaint
      and (not isNativeControl or (ControlDC <> 0))) then
    begin
      ParentPaintWindow := Window;
      lNotebookFound := false;
      while (ParentPaintWindow <> 0) and not lNotebookFound do
      begin
        // notebook is parent of window that has istabpage
{        if GetWindowInfo(ParentPaintWindow)^.isTabPage then
          lNotebookFound := true;}
        ParentPaintWindow := Windows.GetParent(ParentPaintWindow);
      end;
    end;

    // if painting background of some control for tabpage, don't handle erase background
    // in parent of tabpage
{    if WindowInfo^.isTabPage then
      PushEraseBkgndCommand(ecDiscard);}

    // check if double buffering is requested
    useDoubleBuffer := (ControlDC = 0) and (lWinControl.DoubleBuffered);
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

    WinProcess := false;
    try
      if ControlDC = 0 then
      begin
        // ignore first erase background on themed control, paint will do everything
        DC := Windows.BeginPaint(Window, @PS);
        if useDoubleBuffer then
        begin
          PaintRegion := CreateRectRgn(0, 0, 1, 1);
     {     if GetRandomRgn(DC, PaintRegion, SYSRGN) = 1 then
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
          end;}
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
        Windows.SendMessageW(lWinControl.Handle, WM_ERASEBKGND, Windows.WPARAM(PaintMsg.DC), 0);
        Exclude(TWinControlAccess(lWinControl).FWinControlFlags, wcfEraseBackground);
      end;
      if ParentPaintWindow <> 0 then
      begin
        // tabpage parent and got a dc to draw in, divert paint to parent
        DCIndex := Windows.SaveDC(PaintMsg.DC);

        {$ifdef DEBUG_WINDOW_ORG}
        LCLIntf.GetWindowOrgEx(PaintMsg.DC, @WindowOrg);
        DebugLn(
          Format(':> [SendPaintMessage 1] Control=%s DC=%d Moving WindowOrg from %d,%d by DX=%d DY=%d',
          [lWinControl.Name, PaintMsg.DC, WindowOrg.X, WindowOrg.Y, -parLeft, -parTop]));
        {$endif}

        MoveWindowOrgEx(PaintMsg.DC, -parLeft, -parTop);
        Windows.SendMessageW(ParentPaintWindow, WM_PAINT, Windows.WParam(PaintMsg.DC), 0);
        Windows.RestoreDC(PaintMsg.DC, DCIndex);
      end;
      if (ControlDC = 0) or not needParentPaint then
      begin
        DCIndex := Windows.SaveDC(PaintMsg.DC);

        {$ifdef DEBUG_WINDOW_ORG}
        LCLIntf.GetWindowOrgEx(PaintMsg.DC, @WindowOrg);
        DebugLn(
          Format(':> [SendPaintMessage 2] Control=%s DC=%d Moving WindowOrg from %d,%d by DX=%d DY=%d',
          [lWinControl.Name, PaintMsg.DC, WindowOrg.X, WindowOrg.Y, -parLeft, -parTop]));
        {$endif}

        MoveWindowOrgEx(PaintMsg.DC, ORect.Left, ORect.Top);
        DeliverMessage(lWinControl, PaintMsg);
        Windows.RestoreDC(PaintMsg.DC, DCIndex);

        {$ifdef DEBUG_WINDOW_ORG}
        LCLIntf.GetWindowOrgEx(PaintMsg.DC, @WindowOrg);
        DebugLn(
          Format(':> [SendPaintMessage 3] Control=%s DC=%d WindowOrg=%d,%d',
          [lWinControl.Name, PaintMsg.DC, WindowOrg.X, WindowOrg.Y]));
        {$endif}
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
      end;
    end;
    {$ifdef DEBUG_WINDOW_ORG}
    DebugLn(':< [SendPaintMessage] Finish');
    {$endif}
  end;

  procedure SendParentPaintMessage(Window, Parent: HWND; ControlDC: HDC);
  {$ifdef DEBUG_WINDOW_ORG}
  var
    Point: TPoint;
  {$endif}
  begin
    GetWin32ControlPos(Window, Parent, P.X, P.Y);

    {$ifdef DEBUG_WINDOW_ORG}
    LCLIntf.GetWindowOrgEx(ControlDC, @Point);
    DebugLn(
      Format(':> [SendParentPaintMessage] DC=%d Moving WindowsOrg From %d,%d By DX=%d DY=%d',
      [ControlDC, Point.X, Point.Y, P.X, P.Y]));
    {$endif}

    MoveWindowOrgEx(ControlDC, -P.X, -P.Y);
    SendPaintMessage(ControlDC);
    MoveWindowOrgEx(ControlDC, P.X, P.Y);

    {$ifdef DEBUG_WINDOW_ORG}
    DebugLn(':< [SendParentPaintMessage] Finish');
    {$endif}
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
    for I := 0 to Windows.SendMessageW(Window, LB_GETCOUNT, 0, 0) - 1 do
    begin
      Windows.SendMessageW(Window, LB_GETITEMRECT, I, PtrInt(@ItemRect));
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

  procedure DestroyFloatSpinEditBuddy(SpinEditHandle: HWND);
  var
    Buddy: HWND;
  begin
    Buddy := SendMessageW(SpinEditHandle, UDM_GETBUDDY, 0, 0);
    DestroyWindow(Buddy);
  end;

  procedure EnableFloatSpinEditBuddy(SpinEditHandle: HWND; Enable: boolean);
  var
    Buddy: HWND;
  begin
    Buddy := SendMessageW(SpinEditHandle, UDM_GETBUDDY, 0, 0);
    Windows.EnableWindow(Buddy, Enable);
  end;

  procedure DisposeComboEditWindowInfo(ComboBox: TCustomComboBox);
  var
    Buddy: HWND;
  begin
   { Buddy := Windows.GetTopWindow(ComboBox.Handle);
    if Buddy<>HWND(nil) then
      DisposeWindowInfo(Buddy);}
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


    // For thumb messges, retrieve the real position,
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    if Lo(WParam) = SB_THUMBTRACK
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
      and (Lo(LParam) = HTCLIENT) then
    begin
      Windows.GetCursorPos(@(Windows.POINT(P)));
      Windows.ScreenToClient(Window, @(Windows.POINT(P)));
      if GetLCLClientBoundsOffset(lWinControl, BoundsOffset) then
      begin
        Dec(P.X, BoundsOffset.Left);
        Dec(P.Y, BoundsOffset.Top);
      end;
      ACursor := Screen.RealCursor;
      if ACursor = crDefault then
      begin
        // statictext controls do not get WM_SETCURSOR messages...
        lControl := lWinControl.ControlAtPos(P, [capfOnlyClientAreas,
          capfAllowWinControls, capfHasScrollOffset, capfRecursive]);
        if lControl = nil then
          lControl := lWinControl;
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
              PLMsg^.Result := Windows.SendMessageW(targetWindow, WM_SYSCOMMAND, WParam, LParam);
              Windows.SetFocus(prevFocus);
              WinProcess := false;
            end;
          end;
        end;

     //roozbeh : we do not have these in wince!
     { SC_MINIMIZE:
        begin

          if (Application <> nil) and (lWinControl <> nil)
              and (Application.MainForm <> nil)
              and (Application.MainForm = lWinControl) then
                Window := TWinCEWidgetSet(WidgetSet).AppHandle;//redirection

         if (Window = TWinCEWidgetSet(WidgetSet).AppHandle)
              and (Application <> nil)
              and (Application.MainForm<>nil) then
          begin
              Windows.SetWindowPos(Window, HWND_TOP,
                Application.MainForm.Left, Application.MainForm.Top,
                Application.MainForm.Width, 0, SWP_NOACTIVATE);
                if Application.MainForm.HandleAllocated then
                 Windows.ShowWindow(Application.MainForm.Handle,SW_HIDE);

              Application.IntfAppMinimize;
          end;
        end;}

      {SC_RESTORE:
        begin

         if (Window = TWinCEWidgetSet(WidgetSet).AppHandle)
              and (Application <> nil)
              and (Application.MainForm<>nil)
              and Application.MainForm.HandleAllocated then
          begin
            PLMsg^.Result := Windows.DefWindowProc(Window, WM_SYSCOMMAND, WParam, LParam);
            Windows.ShowWindow(Application.MainForm.Handle,SW_SHOW);
            if Windows.IsWindowEnabled(Application.MainForm.Handle)
             then Windows.SetActiveWindow(Application.MainForm.Handle);
            WinProcess := false;

            Application.IntfAppRestore;
          end;
        end;}
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
    newValue: Double;
  begin
    SpinEdit := TCustomFloatSpinEdit(WindowInfo^.WinControl);

    if not SpinEdit.ReadOnly then
    begin
      NewValue := SpinEdit.GetLimitedValue(
        WindowInfo^.spinValue + AUpDownMsg^.iDelta * SpinEdit.Increment);
      WindowInfo^.spinValue := NewValue;

      spinHandle := AUpDownMsg^.hdr.hwndFrom;
      UpdateFloatSpinEditText(SpinEdit, NewValue);
      Windows.SendMessage(spinHandle, UDM_SETPOS32, 0, 500);
    end;
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
    with lWinControl.Constraints do
    begin
      SetWin32SizePoint(MinWidth, MinHeight, MinMaxInfo.ptMinTrackSize);
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
    WinProcess := false;
    case DrawInfo^.dwDrawStage and $7 of //Get drawing state
      CDDS_PREPAINT:  Stage := cdPrePaint;
      CDDS_POSTPAINT: Stage := cdPostPaint;
      CDDS_PREERASE:  Stage := cdPreErase;
      CDDS_POSTERASE: Stage := cdPostErase;
    else
      Exit;
    end;

    case DrawInfo^.dwDrawStage and (CDDS_ITEM or CDDS_SUBITEM) of
      0: begin //Whole control
        DrawResult := ALV.IntfCustomDraw(dtControl, Stage, -1, -1, [], @DrawInfo^.rc);
      end;
      CDDS_ITEM: begin
        DrawResult := ALV.IntfCustomDraw(dtItem, Stage, DrawInfo^.dwItemSpec, -1, ConvState(DrawInfo^.uItemState), nil);
      end;
      CDDS_SUBITEM: begin
        // subitem 0 is handled by dtItem
        if DrawInfo^.iSubItem = 0 then Exit;
        DrawResult := ALV.IntfCustomDraw(dtItem, Stage, DrawInfo^.dwItemSpec, DrawInfo^.iSubItem, ConvState(DrawInfo^.uItemState), nil);
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

  // returns false if the UnicodeChar is not handled
  function HandleUnicodeChar(var AChar: Word): boolean;
  var
    OldUTF8Char, UTF8Char: TUTF8Char;
    WS: WideString;
  begin
    Result := False;
    UTF8Char := UTF16ToUTF8(widestring(WideChar(AChar)));
    OldUTF8Char := UTF8Char;
    if Assigned(lWinControl) then
    begin
      // if somewhere key is changed to '' then don't process this message
      Result := lWinControl.IntfUTF8KeyPress(UTF8Char, 1, False);
      // if somewhere key is changed then don't perform a regular keypress
      Result := Result or (UTF8Char <> OldUTF8Char);
      if Result then
      begin
        WS := UTF8ToUTF16(UTF8Char);
        if Length(WS) > 0 then
          AChar := Word(WS[1])
        else
          AChar := 0;
      end;
    end;
  end;


begin
  //DebugLn('Trace:WindowProc - Start');

  LMessage.Result := 0;
  LMessage.Msg := LM_NULL;
  PLMsg := @LMessage;
  WinProcess := True;
  NotifyUserInput := False;

  //DebugLn('Trace:WindowProc - Getting Object with Callback Procedure');
  WindowInfo := GetWindowInfo(Window);

//----------------------------------------
//roozbeh added for testing paint messages
//----------------------------------------

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
    end;
  end else begin
    lWinControl := WindowInfo^.WinControl;
  end;
{$ifdef MSG_DEBUG}
  DebugLn('WindowProc lWinControl: ',DbgSName(lWinControl));
{$endif}
  if (IgnoreNextCharWindow <> 0) and ((Msg = WM_CHAR) or (Msg = WM_SYSCHAR)) then
  begin
    if IgnoreNextCharWindow = Window then
    begin
      IgnoreNextCharWindow := 0;
      Result := 1;
      exit;
    end;
    IgnoreNextCharWindow := 0;
  end;

  {$ifdef MSG_DEBUG}
    DebugLn('Trace:WindowProc - Case Msg of');
  {$endif}

  case Msg Of
    WM_NULL:
    begin
      CheckSynchronize;
      TWinCEWidgetset(Widgetset).CheckPipeEvents;
    end;
    WM_ACTIVATE:
    begin
      LMessage.Msg := LM_ACTIVATE;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
      case Lo(WParam) Of
        WA_ACTIVE, WA_CLICKACTIVE:
        begin
          // Upon activation of a form, rebuild it's menu
          if lWinControl is TCustomForm then
          begin
            {$ifdef VerboseWinCEMenu}
            DebugLn('[wincecallback] WM_ACTIVATE -> Rebuilding the menu');
            {$endif}
            if (lWinControl as TCustomForm).Menu <> nil then
            begin
              TWinCEWidgetset(Widgetset).SetMenu(lWinControl.Handle,
                (lWinControl as TCustomForm).Menu.Handle);
            end
            else
            begin
              TWinCEWidgetset(Widgetset).SetMenu(lWinControl.Handle, 0);
            end;
          end;
        end;
      end;
    end;
    WM_CAPTURECHANGED:
    begin
      LMessage.Msg := LM_CAPTURECHANGED;
    end;
    WM_CHAR:
    begin
      // first send a IntfUTF8KeyPress to the LCL
      // if the key was not handled send a CN_CHAR for AnsiChar<=#127
      OrgCharCode := Word(WParam);
      if not HandleUnicodeChar(OrgCharCode) then
      begin
        PLMsg:=@LMChar;
        with LMChar Do
        begin
          Msg := CN_CHAR;
          KeyData := LParam;
          CharCode := Word(Char(WideChar(WParam)));
          OrgCharCode := CharCode;
          Result := 0;
          //DebugLn(Format('WM_CHAR KeyData= %d CharCode= %d ',[KeyData,CharCode]));
        end;
        WinProcess := false;
      end
      else
      begin
        WParam := OrgCharCode;
        WinProcess := OrgCharCode<>0;
      end;
    end;
    WM_MENUCHAR:
    begin
      PLMsg^.Result := FindMenuItemAccelerator(chr(LOWORD(WParam)), LParam);
      WinProcess := false;
    end;
    WM_CLOSE:
    begin
      if (Window = TWinCEWidgetSet(WidgetSet).AppHandle) and
        (Application.MainForm <> nil) then
      begin
        Windows.SendMessage(Application.MainForm.Handle, WM_CLOSE, 0, 0);
      end else begin
        LMessage.Msg := LM_CLOSEQUERY;
      end;
      // default is to destroy window, inhibit
      WinProcess := false;
    end;
    {
      WM_COMMAND

      Parameters:

      wNotifyCode = HIWORD(wParam);
      wID = LOWORD(wParam);
      hwndCtl = (HWND) lParam;

      If wID is IDOK, then we have received a close message from the
      "OK" button in the title of dialog windows.
    }
    WM_COMMAND:
    begin
      { Handles the "OK" button in the title bar of dialogs }
      if Lo(wParam) = IDOK then
      begin
        if (lWinControl is TCustomForm) and (fsModal in TCustomForm(lWinControl).FormState) then
          TCustomForm(lWinControl).ModalResult := mrOK
        else
          SendMessage(Window, WM_CLOSE, 0, 0);
      end
      else
      begin
        { Handles other reasons for WM_COMMAND }

        if Hi(WParam) < 2 then //1 for accelerator 0 for menu
        begin
          {$ifdef VerboseWinCEMenu}
          DebugLn('[wincecallback] Hi(WParam) < 2');
          {$endif}
          TargetObject := GetMenuItemObject();
        end
        else // menuitem or shortcut
        begin
          TargetObject := nil;
        end;

        if (TargetObject <> nil) and (TargetObject is TMenuItem) then
        begin
          {$ifdef VerboseWinCEMenu}
          DebugLn('[wincecallback] Sending menuitem Click');
          {$endif}

          LMessage.Msg := LM_ACTIVATE;
          TargetObject.Dispatch(LMessage);
          lWinControl := nil;
        end
        else
        begin
          lWinControl := GetWindowInfo(LParam)^.WinControl;
          // buddy controls use 'awincontrol' to designate associated wincontrol
          if lWinControl = nil then
            lWinControl := GetWindowInfo(LParam)^.AWinControl;

          if lWinControl is TCustomCheckbox then begin
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
          end
          // WM_COMMAND in the win32 interface to a button generates an OnClick event
          // But in Windows CE this generates two OnClick events when the <ENTER> key
          // is pressed in a button. For this is required for button clicking.
          // See bugs 17680 and 17702
          else if lWinControl is TCustomButton then
          begin
            case Hi(WParam) of
              BN_CLICKED:   LMessage.Msg := LM_CLICKED;
              BN_KILLFOCUS: LMessage.Msg := LM_EXIT;
            end
          end
          // TCustomMemo and TCustomFloatSpinEdit are also TCustomEdit
          // so do not check first only for TCustomEdit
          // Fixes bug http://bugs.freepascal.org/view.php?id=14378
          else if (lWinControl is TCustomEdit) then
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
            case Hi(WParam) of
              LBN_SELCHANGE:  LMessage.Msg := LM_SELCHANGE;
            end
          else if lWinControl is TCustomCombobox then
            case Hi(WParam) of
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
        if (LMessage.Msg = LM_NULL) and (lWinControl <> nil) then
          lWinControl.Perform(CN_COMMAND, WParam, LParam);
      end;
    end;
{
  * Besides the fact that LCL does not respond to LM_CREATE, this code is
    probably never reached anyway, as the callback is not set until after
    window creation

    WM_CREATE:
    begin
      //DebugLn('Trace:WindowProc - Got WM_CREATE');
      LMessage.Msg := LM_CREATE;
    end;
}
    WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
    begin
      { it's needed for winxp themes where controls send the WM_ERASEBKGND
        message to their parent to clear their background and then draw
        transparently
        only static and button controls have transparent parts
        others need to erased with their window color
        scrollbar also has buttons

        Handling this message is the method provided to change the background
        color of many Windows Standard Controls, like TGroupBox, TCheckBox, etc.

        Note that here ChildWinControl represents the control about to be
        painted. A RadioButton for example. lWinControl represents the Form.
      }
      ChildWindowInfo := GetWindowInfo(LParam);
      ChildWinControl := ChildWindowInfo^.WinControl;
      if ChildWinControl = nil then
        ChildWinControl := ChildWindowInfo^.AWinControl;
      case Msg of
        WM_CTLCOLORSTATIC,
        WM_CTLCOLORBTN: begin
          if GetNeedParentPaint(ChildWindowInfo, ChildWinControl) then
          begin
            // need to draw transparently, draw background
            SendParentPaintMessage(HWND(LParam), Window, HDC(WParam));
            LMessage.Result := GetStockObject(HOLLOW_BRUSH);
            SetBkMode(WParam, TRANSPARENT);
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
          Windows.SetTextColor(HDC(WParam), ColorToRGB(WindowColor));
          WindowColor := ChildWinControl.Brush.Color;
          if WindowColor = clDefault then
            WindowColor := ChildWinControl.GetDefaultColor(dctBrush);
          Windows.SetBkColor(HDC(WParam), ColorToRGB(WindowColor));
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
      //DebugLn('Trace:WindowProc - Got WM_DESTROY');
      if lWinControl is TCheckListBox then
        TWinCECheckListBoxStrings.DeleteItemRecords(Window);
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
//        {$IFDEF VerboseWin32Clipbrd}
//        debugln('WM_DESTROYCLIPBOARD');
//        {$ENDIF}
        OnClipBoardRequest(0, nil);
        OnClipBoardRequest := nil;
        LMessage.Result := 0;
      end;
    end;
    WM_DRAWITEM:
    begin
      // TODO: this could crash for a MenuItem.
      WindowInfo := GetWindowInfo(PDrawItemStruct(LParam)^.hwndItem);
      if WindowInfo^.WinControl<>nil then
        lWinControl := WindowInfo^.WinControl;
      {$IFDEF MSG_DEBUG}
      with PDrawItemStruct(LParam)^ do
        writeln(format('Received WM_DRAWITEM type %d handle %x', [ctlType, integer(hwndItem)]));
      {$ENDIF}

      if ((lWinControl is TCustomListbox) and
            (TCustomListBox(lWinControl).Style <> lbStandard)) or
         ((lWinControl is TCustomCombobox) and
             TCustomCombobox(lWinControl).Style.IsOwnerDrawn)
      then
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
      end else
      if (lWinControl is TCustomBitBtn) and (PDrawItemStruct(LParam)^.ctlType = ODT_BUTTON) then
      begin
        DrawBitBtnImage(TCustomBitBtn(lWinControl), PDrawItemStruct(LParam));
        WinProcess := False;
      end
      else
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
    WM_ENABLE:
    begin
      if WParam <> 0 then
        LMessage.Msg := LM_SETEDITABLE;
      if Window = TWinCEWidgetSet(WidgetSet).FAppHandle then
        if WParam = 0 then
          DisabledForms := Screen.DisableForms(nil, DisabledForms)
        else
          Screen.EnableForms(DisabledForms);

      if (lWinControl is TCustomFloatSpinEdit) then
        EnableFloatSpinEditBuddy(Window, WParam<>0);
    end;
    WM_ENTERIDLE: Application.Idle(False);
    WM_ERASEBKGND:
    begin
      eraseBkgndCommand := TEraseBkgndCommand(EraseBkgndStack and EraseBkgndStackMask);
      if eraseBkgndCommand = ecDoubleBufferNoRemove then
      begin
        if CurDoubleBuffer.DC <> 0 then
          WParam := Windows.WParam(CurDoubleBuffer.DC);
{        if WindowInfo^.isTabPage then
          EraseBkgndStack := (EraseBkgndStack and not ((1 shl EraseBkgndStackShift)-1))
            or dword(ecDiscardNoRemove);}
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
    WM_HSCROLL, WM_VSCROLL:
      HandleScrollMessage(Msg);
    WM_KEYDOWN:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      with LMKey Do
      begin
        Msg := CN_KEYDOWN;
        KeyData := LParam;
        // Work around to get readings of some barcode scanners
        if (WParam = 0) and (WinCEWidgetset.BarcodeScannerWorkaround) then
          WParam := VK_0;
        CharCode := Word(WParam);
        Result := 0;
        //DebugLn(Format('WM_KEYDOWN KeyData= %d CharCode= %d ',[KeyData,CharCode]));
        //DebugLn('  lWinControl= '+TComponent(lWinControl).Name+':'+lWinControl.ClassName);
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
        //DebugLn(Format('WM_KEYUP KeyData= %d CharCode= %d ',[KeyData,CharCode]));
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
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      end;

      // CheckListBox functionality
      if lWinControl is TCheckListBox then
        CheckListBoxLButtonDown;
    end;
    WM_LBUTTONDOWN:
    begin
      {$ifndef win32}
      // Gesture recognition process to enable popup menus.
      if (lWinControl.PopupMenu <> nil) then
      begin
        Info.cbSize := SizeOf(SHRGINFO);
        Info.dwFlags := SHRG_RETURNCMD;
        Info.hwndClient := Window;
        Info.ptDown.x := Lo(LParam);
        Info.ptDown.y := Hi(LParam);

        SHRecognizeGesture(Info);
      end;
      {$endif}

      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_LBUTTONDOWN;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        Keys := WParam;
      end;
(*
      if (GetTickCount-MouseDownTime < GetDoubleClickTime()) and
         (MouseDownWindow = Window) and
         (Abs(LMMouse.XPos - MouseDownPos.x) < GetSystemMetrics(SM_CXDOUBLECLK)) and
         (Abs(LMMouse.YPos - MouseDownPos.y) < GetSystemMetrics(SM_CYDOUBLECLK)) then
      begin
        LMMouse.Msg := LM_LBUTTONDBLCLK;
      end;
*)
      MouseDownTime := GetTickCount();
      MouseDownWindow := Window;
      GetCursorPos(MouseDownPos);

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
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
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
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
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
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
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
      end;
    end;
    WM_MOUSEWHEEL:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouseEvent;
      with LMMouseEvent Do
      begin
        X := SmallInt(Lo(LParam));
        Y := SmallInt(Hi(LParam));
        // check if mouse cursor within this window, otherwise send message to window the mouse is hovering over
        P.X := X;
        P.Y := Y;
        TargetWindow := TWinCEWidgetSet(WidgetSet).WindowFromPoint(P);
        if TargetWindow = HWND(nil) then
          exit;




        // check if the window is an edit control of a combobox, if so,
        // redirect it to the combobox, not the edit control
        if GetWindowInfo(TargetWindow)^.isComboEdit then
        begin
          TargetWindow := Windows.GetParent(TargetWindow);
          if (TargetWindow <> Window) then
          begin
            Result := SendMessage(TargetWindow, WM_MOUSEWHEEL, WParam, LParam);
            exit;
          end;
        end;

        // the mousewheel message is for us
        // windows handles combobox's mousewheel messages
        if (lWinControl=nil) or (lWinControl.FCompStyle <> csComboBox) then
        begin
          Msg := LM_MOUSEWHEEL;
          Button := Lo(WParam);
          WheelDelta := SmallInt(Hi(WParam));
          State := KeysToShiftState(Button);
          UserData := Pointer(GetWindowLong(Window, GWL_USERDATA));
          WinProcess := false;
        end;
      end;
    end;
    {$IFDEF EnableWMDropFiles}
    WM_DROPFILES:
      begin
        LMessage.Msg := LM_DROPFILES;
        LMessage.WParam := WParam;
        LMessage.LParam := LParam;
      end;
    {$ENDIF}
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
      if (lWinControl <> nil) then begin
        if (lWinControl.FCompStyle = csHintWindow) then
        begin
          LMessage.Result := HTTRANSPARENT;
          WinProcess := false;
        end
        else if (lWinControl is TCustomGroupBox) then
        begin
          LMessage.Result := HTCLIENT;
          WinProcess := false;
        end;
      end;
    end;
    WM_NCLBUTTONDOWN:
    begin
      NotifyUserInput := True;
      //DebugLn('Trace:WindowProc - Got WM_NCLBUTTONDOWN');
    end;
    WM_NOTIFY:
    begin
      WindowInfo := GetWindowInfo(PNMHdr(LParam)^.hwndFrom);

      case PNMHdr(LParam)^.code of
        0 :;
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
                then HandleListViewCustomDraw(TCustomListViewAccess(WindowInfo^.WinControl));
              end;
            end;
        end;
      end;
    end;
    WM_PAINT:
    begin
      {$IFDEF DEBUG_WINCE_LABELS}
      DebugLn('[WM_PAINT]');
      {$ENDIF}
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
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
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
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
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
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
        Result := 0;
      end;
    end;
    WM_CONTEXTMENU:
    begin
      WinProcess := false;
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
    WM_SETFOCUS:
    begin
{$ifdef DEBUG_CARET}
      DebugLn('WM_SETFOCUS received for window ', IntToHex(Window, 8));
{$endif}
      LMessage.Msg := LM_SETFOCUS;
    end;
    WM_SHOWWINDOW:
    begin
      //DebugLn('Trace:WindowProc - Got WM_SHOWWINDOW');
      with TLMShowWindow(LMessage) Do
      begin
        Msg := LM_SHOWWINDOW;
        Show := WParam <> 0;
        Status := LParam;
      end;

      if assigned(lWinControl) and ((WParam<>0) or not lWinControl.Visible)
        and ((WParam=0) or lWinControl.Visible)
        and (Application<>nil) and (lWinControl=Application.MainForm) then
      begin
        if WParam=0 then
          Flags := SW_HIDE
        else
          Flags := SW_SHOWNOACTIVATE;
        Windows.ShowWindow(TWinCEWidgetSet(WidgetSet).FAppHandle, Flags);
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
        //DebugLn(Format('WM_CHAR KeyData= %d CharCode= %d ',[KeyData,CharCode]));
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
      if LWinControl<>nil then
      begin
        if LWinControl is TCustomCombobox then
        begin
          LMessage.Msg := LM_MEASUREITEM;
          LMessage.LParam := LParam;
          LMessage.WParam := WParam;
          Winprocess := False;
        end else
        if WParam <> 0 then
        begin
          LWinControl := TWinControl(WParam);
          if LWinControl<>nil then
          begin
            LMessage.Msg := LM_MEASUREITEM;
            LMessage.LParam := LParam;
            LMessage.WParam := WParam;
            Winprocess := False;
          end;
        end;
      end;
    end;
    WM_LCL_SOCK_ASYNC:
    begin
      if (Window = TWinCEWidgetSet(WidgetSet).AppHandle) and
          Assigned(TWinCEWidgetSet(WidgetSet).FOnAsyncSocketMsg) then
        exit(TWinCEWidgetSet(WidgetSet).FOnAsyncSocketMsg(WParam, LParam))
    end;
    {$ifdef wince}
    WM_HOTKEY:
    begin
      // Implements back-key sending to edits, instead of hiding the form
      // See http://bugs.freepascal.org/view.php?id=16699
      if HIWORD(lParam) = VK_ESCAPE then
      begin
        SHSendBackToFocusWindow(Msg, wParam, lParam);
        Exit;
      end;
    end;
    {$endif}
  else
    // pass along user defined messages
    if Msg >= WM_USER then
    begin
      LMessage.Msg := Msg;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
      WinProcess := false;
    end;
  end;

  {$ifdef MSG_DEBUG}
    DebugLn('Trace:WindowProc - End Case Msg of');
  {$endif}

  if WinProcess Then
  begin
    {$ifdef MSG_DEBUG}
      DebugLn('Trace:WindowProc - if WinProcess Then');
    {$endif}
    PLMsg^.Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
    WinProcess := false;
    {$ifdef MSG_DEBUG}
      DebugLn('Trace:WindowProc - End if WinProcess Then');
    {$endif}
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
          DebugLn('WinCECallBack WM_MOVE ', dbgsName(lWinControl),
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
        if Window = TWinCEWidgetSet(WidgetSet).AppHandle then
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
          DebugLn('WinCECallback: WM_SIZE '+ dbgsName(lWinControl)+
            ' NewSize='+ dbgs(Width)+','+dbgs(Height)+
            ' OldClientSize='+dbgs(lWinControl.CachedClientWidth)+','+dbgs(lWinControl.CachedClientHeight)+
            ' NewClientSize='+dbgs(R.Right)+','+dbgs(R.Bottom));
          {$ENDIF}
          if (lWinControl.Width<>Width) or (lWinControl.Height<>Height)
              or lWinControl.ClientRectNeedsInterfaceUpdate then
          begin
            lWinControl.DoAdjustClientRectChange;
            if (lWinControl is TCustomPage)
            and (lWinControl.Parent is TCustomTabControl) then
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

    //roozbeh : do not have these on ce!
    {WM_ENDSESSION:
    begin
      if (Application<>nil) and (TWinCEWidgetSet(WidgetSet).AppHandle=Window) and
         (WParam>0) and (LParam=0) then
      begin
        LMessage.Msg := LM_NULL; // no need to go through delivermessage
        Application.IntfEndSession();
        LMessage.Result := 0;
      end;
    end;

    WM_QUERYENDSESSION:
    begin
      if (Application<>nil) and (TWinCEWidgetSet(WidgetSet).AppHandle=Window) and
         (LParam=0) then
      begin
        LMessage.Msg := LM_NULL; // no need to go through delivermessage
        CancelEndSession := LMessage.Result=0;
        Application.IntfQueryEndSession(CancelEndSession);
        if CancelEndSession
          then LMessage.Result := 0
          else LMessage.Result := 1;
      end;
    end;}

  end;

  // convert from wince client to lcl client pos.
  //
  // hack to prevent GetLCLClientBoundsOffset from changing mouse client
  // coordinates for TScrollingWinControls, this is required in
  // IsControlMouseMsg and ControlAtPos where unscrolled client coordinates
  // are expected.
  if (PLMsg = @LMMouseMove) and not (lWinControl is TScrollingWinControl) then
  begin
    if GetLCLClientBoundsOffset(Window, R) then
    begin
      Dec(LMMouseMove.XPos, R.Left);
      Dec(LMMouseMove.YPos, R.Top);
    end;
  end else
  if (PLMsg = @LMMouse) and not (lWinControl is TScrollingWinControl) then
  begin
    if GetLCLClientBoundsOffset(Window, R) then
    begin
      Dec(LMMouse.XPos, R.Left);
      Dec(LMMouse.YPos, R.Top);
    end;
  end;

  // application processing
  if NotifyUserInput then
    NotifyApplicationUserInput(lWinControl, PLMsg^.Msg);

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
      if (LMChar.Result=1) or (OrgCharCode<>LMChar.CharCode) then
          WParam := Word(WideChar(Char(LMChar.CharCode)));
//    WParam := LMChar.CharCode;
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
          TCN_SELCHANGE:
            NotebookFocusNewControl(GetWindowInfo(hwndFrom)^.WinControl as TCustomTabControl, idFrom);
        end;
    end;

  else
    case Msg of
      WM_NCDESTROY:
      begin
        //roozbeh : test this.... WinCE does not support WM_NCDESTROY message #24452
        // free our own data associated with window
        if DisposeWindowInfo(Window) then
          WindowInfo := nil;
        //EnumProps(Window, @PropEnumProc);
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
            PLMsg^.Result := CallDefaultWindowProc(Window, WM_GETDLGCODE, WParam, 0) and DLGC_WANTCHARS;
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

{        // handle the hardware back key (= VK_ESCAPE) for edit controls
        // It should work as the backspace
        if (PLMsg^.Result = 0) and (Msg = WM_KEYDOWN) and (WParam = VK_ESCAPE)
          {and (GetKeyState(VK_CONTROL) >= 0) and (GetKeyState(VK_MENU) >= 0)} then
        begin
          if (WndClassName(Window) = EditClsName) then
          begin
            Windows.SendMessage(Window, WM_KEYDOWN, VK_BACK, -1);
          end;
        end;}
      end;
    end;
  end;

  // ignore WM_(SYS)CHAR message if LCL handled WM_(SYS)KEYDOWN
  if ((Msg = WM_KEYDOWN) or (Msg = WM_SYSKEYDOWN)) then
  begin
    if (PLMsg^.Result <> 0) then
    begin
      IgnoreNextCharWindow := Window;
    end else begin
      // stop ignoring if KEYUP has come by (not all keys generate CHAR)
      // assume WM_CHAR is always preceded by WM_KEYDOWN
      IgnoreNextCharWindow := 0;
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

  //DebugLn('Trace:WindowProc - Exit');
end;

{$ifdef MSG_DEBUG}

function WindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; {$ifdef wince}cdecl;{$else}stdcall;{$endif}
begin
  DebugLn('WindowProc called for window=', IntToHex(Window, 8),' msg=',
    WM_To_String(msg),' wparam=', IntToHex(WParam, 8), ' lparam=', IntToHex(lparam, 8));

  Result := RealWindowProc(Window, Msg, WParam, LParam);

  DebugLn('Finished WindowProc');
end;
{$endif}

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
    LParam: Windows.LParam): LResult; cdecl;
begin
  // darn MS: if combobox has edit control, and combobox receives focus, it
  // passes it on to the edit, so it will send a WM_KILLFOCUS; inhibit
  // also don't pass WM_SETFOCUS to the lcl,
  // it will get one from the edit control
  if ((Msg = WM_KILLFOCUS) or (Msg = WM_SETFOCUS)) and
     (GetTopWindow(Window) <> HWND(nil)) then
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

procedure TimerCallBackProc(window_hwnd : hwnd; msg , idEvent: UINT; dwTime: DWORD);
 {$ifdef win32}stdcall{$else}cdecl{$endif};
Var
  TimerInfo: PWinCETimerInfo;
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
