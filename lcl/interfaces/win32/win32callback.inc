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
  Assert(False, 'Trace:PropEnumProc - Start');
  Assert(False, Format('Trace:PropEnumProc - Property %S (with value 0x%X) from window 0x%X removed', [String(Str), Data, Window]));
  RemoveProp(Window, Str);
  Result := True;
  Assert(False, 'Trace:PropEnumProc - Exit');
End;

{------------------------------------------------------------------------------
 Function: WindowProc
 Params: Window - The window that receives a message
         Msg    - The message received
         WParam - Word parameter
         LParam - Long-integer parameter
  Returns: 0 if Msg is handled; non-zero long-integer result otherwise

  Handles the messages sent to the current window by Windows or other
  applications
 ------------------------------------------------------------------------------}
Function WindowProc(Window: HWnd; Msg: UInt;
  WParam: WParam; LParam: LParam): LResult; stdcall;
Var
  LMessage: TLMessage;
  PLMsg: PLMessage;
  R: TRect;
  OwnerObject: TObject;
  WinProcess: Boolean;
  PrevWndProc: Pointer;
  
  LMInsertText: TLMInsertText; // used by CB_INSERTSTRING, LB_INSERTSTRING

  procedure ShowHideTabPage(NotebookHandle: HWnd; Showing: boolean);
  var
    NoteBook: TCustomNotebook;
    PageIndex: integer;
    PageHandle: HWND;
  begin
    Notebook := TObject(GetProp(NotebookHandle, 'Lazarus')) as TCustomNotebook;
    PageIndex := SendMessage(NotebookHandle, TCM_GETCURSEL, 0, 0);
    PageHandle := Notebook.Page[PageIndex].Handle;
    if Showing
      then ShowWindow(PageHandle, SW_SHOW)
      else ShowWindow(PageHandle, SW_HIDE);
  end;

  function GetMenuItemObject: TObject;
  var MenuInfo: MENUITEMINFO;
      MainMenuHandle: HMENU;
  begin
    Result:=nil;
    MenuInfo.cbSize:=sizeof(MENUITEMINFO);
    MenuInfo.fMask:=MIIM_DATA;
    {first we have to decide if the command is from a popup menu or from the window main menu}
    MainMenuHandle := HMENU(GetProp(Window, 'PopupMenu')); //if the 'PopupMenu' property exists, there is a big probability that the command is from a popup menu
    if MainMenuHandle<>0 then //processing popup menu
    begin
      RemoveProp(Window, 'PopupMenu');
      {GetMenuItemInfo can be FALSE when the 'PopupMenu' property was not
      removed in the last popup menu processing (no menuitem was selected)}
      if GetMenuItemInfo(MainMenuHandle, Lo(WParam), false, @MenuInfo)
        then Result := TObject(MenuInfo.dwItemData);
    end;
    if Result=nil then //if Result is still nil, process main menu
    begin
      MainMenuHandle := GetMenu(Window);
      GetMenuItemInfo(MainMenuHandle, Lo(WParam), false, @MenuInfo);
      Result := TObject(MenuInfo.dwItemData);
    end;
  end;
  
Begin
  Assert(False, 'Trace:WindowProc - Start');

  Result := 0;
  LMessage.Msg := -1;
  PLMsg := @LMessage;
  WinProcess := True;

  Assert(False, 'Trace:WindowProc - Getting Object With Callback Procedure');
  OwnerObject := TObject(GetProp(Window, 'Lazarus'));
  Assert(False, 'Trace:WindowProc - Getting Callback Object');

  Assert(False, 'Trace:WindowProc - Checking Proc');
  Assert(False, Format('Trace:WindowProc - Window Value: $%S-%d; Msg Value: %S; WParam: $%S; LParam: $%S', [IntToHex(Window, 4), Window, WM_To_String(Msg), IntToHex(WParam, 4), IntToHex(LParam, 4)]));
  
  Case Msg Of
    LM_MONTHCHANGED..LM_DAYCHANGED:
    Begin
      If OwnerObject Is TCalendar Then
        LMessage.Msg := Msg;
    End;
    WM_ACTIVATE:
    Begin
      Case Lo(WParam) Of
        WA_ACTIVE:
        Begin
          LMessage.Msg := LM_ACTIVATE;
        End;
        WA_INACTIVE:
        Begin
          LMessage.Msg := LM_DEACTIVATE;
        End;
      End;
    End;
    WM_CAPTURECHANGED:
    Begin
      LMessage.Msg := LM_CHANGED;
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
        // need to init to nil because typecasting LMessage struct
        Integer(NewText) := 0;
        NewText := PChar(LParam);
        Length := System.Length(NewText);
//        UserData := Pointer(GetWindowLong(Window, GWL_USERDATA));
      End;
    End;
    WM_CLOSE:
    Begin
      LMessage.Msg := LM_CLOSEQUERY;
    End;
    WM_COMMAND:
    Begin
      OwnerObject := TObject(GetProp(LParam, 'Lazarus'));
      if LParam=0 then OwnerObject := GetMenuItemObject; {menuitem or shortcut}

// is this for speedbuttons?
//      If ((OwnerObject Is TControl) And (Not (OwnerObject Is TButton))) Then
//        CallEvent(OwnerObject, TControl(OwnerObject).OnClick, Nil, etNotify)

      if OwnerObject is TMenuItem then
      begin
        if (Hi(WParam) = 0) or (Hi(WParam) = 1) then
          LMessage.Msg := LM_ACTIVATE;
      end else if OwnerObject is TButton then
      begin
        case Hi(WParam) of
          BN_CLICKED:   LMessage.Msg := LM_CLICKED;
          BN_KILLFOCUS: LMessage.Msg := LM_EXIT;
        end;
      end else if OwnerObject is TEdit then
        case Hi(WParam) of
          EN_CHANGE:    LMessage.Msg := CM_TEXTCHANGED;
      end;
      // no specific message found? try send a general msg
      if LMessage.Msg = -1 then
        if OwnerObject is TControl then
          TControl(OwnerObject).Perform(CN_COMMAND, WParam, LParam);
    End;
    WM_CREATE:
    Begin
      Assert(False, 'Trace:WindowProc - Got WM_CREATE');
      LMessage.Msg := LM_CREATE;
    End;
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
      EnumProps(Window, @PropEnumProc);
      LMessage.Msg := LM_DESTROY;
      PostQuitMessage(0);
    End;
    WM_ENABLE:
    Begin
      If WParam <> 0 Then
        LMessage.Msg := LM_SETEDITABLE;
    End;
    WM_HSCROLL:
    Begin
      With TLMScroll(LMessage) Do
      Begin
        Msg := LM_HSCROLL;
        ScrollCode := Lo(WParam);
        Pos := Hi(WParam);
        ScrollBar := LParam;
      End;
    End;
    WM_ERASEBKGND:
    Begin
      LMessage.Msg := LM_ERASEBKGND;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
      Result := 1;
      WinProcess := false;
    End;
    WM_KEYDOWN:
    Begin
      With TLMKey(LMessage) Do
      Begin
        Msg := LM_SYSKEYDOWN;
        KeyData := LParam;
        CharCode := WParam;
      End;
    End;
    WM_KEYUP:
    Begin
      With TLMKey(LMessage) Do
      Begin
        Msg := LM_SYSKEYUP;
        KeyData := LParam;
        CharCode := WParam;
      End;
    End;
    WM_KILLFOCUS:
    Begin
      LMessage.Msg := LM_KILLFOCUS;
    End;
    //TODO:LM_KILLCHAR,LM_KILLWORD,LM_KILLLINE
    WM_LBUTTONDBLCLK:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_LBUTTONDBLCLK;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_LBUTTONDOWN:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_LBUTTONDOWN;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_LBUTTONUP:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_LBUTTONUP;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_MBUTTONDBLCLK:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_MBUTTONDBLCLK;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_MBUTTONDOWN:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_MBUTTONDOWN;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_MBUTTONUP:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_MBUTTONUP;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_MOUSEHOVER:
    Begin
      LMessage.Msg := LM_ENTER;
    End;
    WM_MOUSELEAVE:
    Begin
      LMessage.Msg := LM_LEAVE;
    End;
    WM_MOUSEMOVE:
    Begin
      With TLMMouseMove(LMessage) Do
      Begin
        Msg := LM_MOUSEMOVE;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_MOUSEWHEEL:
    Begin
      With PLMMouseEvent(@LMessage)^ Do
      Begin
        Msg := LM_MOUSEWHEEL;
        WheelDelta := Hi(WParam);
        X := Lo(LParam);
        Y := Hi(LParam);
        State := GetShiftState;
        UserData := Pointer(GetWindowLong(Window, GWL_USERDATA));
      End;
    End;
    WM_MOVE:
    Begin
      With TLMMove(LMessage) Do
      Begin
        Msg := LM_MOVE;
        // MoveType := WParam;   WParam is not defined!
        MoveType := Move_SourceIsInterface;
        //If OwnerObject Is TCustomForm then begin
          If Windows.GetParent(Window) = 0 then begin 
          Windows.GetWindowRect(Window,@R);
          XPos := R.Left;
          YPos := R.Top;
        end 
        Else begin
          XPos := LoWord(LParam);
          YPos := HiWord(LParam);
        end;  
      End;
    End;
    //TODO:LM_MOVEPAGE,LM_MOVETOROW,LM_MOVETOCOLUMN
    WM_NCLBUTTONDOWN:
    Begin
      Assert(False, 'Trace:WindowProc - Got WM_NCLBUTTONDOWN');
    End;
    WM_NOTIFY:
    Begin
      With TLMNotify(LMessage) Do
      Begin
        Msg := LM_NOTIFY;
        IDCtrl := WParam;
        NMHdr := PNMHDR(LParam);
        With NMHdr^ do
        Begin
          If code = TCN_SELCHANGING
            Then ShowHideTabPage(HWndFrom, False);
          If code = TCN_SELCHANGE Then
          begin
            ShowHideTabPage(HWndFrom, True);
            idFrom := SendMessage(HWndFrom, TCM_GETCURSEL, 0, 0);
          end;
        end;
      End;
    End;
    WM_PAINT:
    Begin
      With TLMPaint(LMessage) Do
      Begin
        Msg := LM_PAINT;
//        DC := GetDC(Window);
//        Unused := 0;
//        ReleaseDC(Window, DC);
        DC := WParam;
      End;
      WinProcess := false;
    End;
    WM_PASTE:
    Begin
      LMessage.Msg := LM_PASTEFROMCLIP;
    End;
    WM_RBUTTONDBLCLK:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_RBUTTONDBLCLK;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_RBUTTONDOWN:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_RBUTTONDOWN;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_RBUTTONUP:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_RBUTTONUP;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
        Result := 0;
      End;
    End;
    WM_SETFOCUS:
    Begin
      LMessage.Msg := LM_SETFOCUS;
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
    End;
    WM_SIZE:
    Begin
      With TLMSize(LMessage) Do
      Begin
        Msg := LM_SIZE;
        SizeType := WParam or Size_SourceIsInterface;
        if OwnerObject is TWinControl then
          TWinControl(OwnerObject).InvalidateClientRectCache;
        Width := LoWord(LParam);
        Height := HiWord(LParam);
        // adjust size for scrollbars
        if (Windows.GetWindowLong(Window, GWL_STYLE) and WS_VSCROLL) <> 0 then
          Width := Width - GetSystemMetrics(SM_CXVSCROLL);
        if (Windows.GetWindowLong(Window, GWL_STYLE) and WS_HSCROLL) <> 0 then
          Height := Height - GetSystemMetrics(SM_CYHSCROLL);
      End;
    End;
    WM_SYSKEYDOWN:
    Begin
      With TLMKey(LMessage) Do
      Begin
        Msg := LM_SYSKEYDOWN;
        KeyData := LParam;
        CharCode := WParam;
      End;
    End;
    WM_SYSKEYUP:
    Begin
      With TLMKey(LMessage) Do
      Begin
        Msg := LM_SYSKEYUP;
        KeyData := LParam;
        CharCode := WParam;
      End;
    End;
    WM_TIMER:
    Begin
      LMessage.Msg := LM_TIMER;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
    End;
    WM_VSCROLL:
    Begin
      With TLMScroll(LMessage) Do
      Begin
        Msg := LM_VSCROLL;
        ScrollCode := Lo(WParam);
        Pos := Hi(WParam);
        ScrollBar := LParam;
      End;
    End;
    WM_WINDOWPOSCHANGED:
    Begin
      With TLMWindowPosMsg(LMessage) Do
      Begin
        Msg := LM_WINDOWPOSCHANGED;
        Unused := WParam;
        WindowPos := PWindowPos(LParam);
      End;
//      GetClientRect(Window, R);
//      SendMessage(Window, WM_MOVE, 0, MakeLParam(PWindowPos(LParam)^.x, PWindowPos(LParam)^.y));
//      SendMessage(Window, WM_SIZE, 0, MakeLParam(R.right-R.left, R.bottom-R.top));
//      WinProcess:=false;
    End;
  End;

  If WinProcess Then
  begin
    PrevWndProc := GetProp(Window, 'DefWndProc');
    if PrevWndProc = nil then
      Result := DefWindowProc(Window, Msg, WParam, LParam)
    else
      Result := CallWindowProc(PrevWndProc, Window, Msg, WParam, LParam);
  end;

  {$IFDEF VER1_1} 
    List := TMsgArray(GetProp(Window, 'MsgList'));
    If Pointer(List) <> Nil Then
      For C := 0 To Length(List) Do
        If List[C] = PLMsg^.Msg Then
        Begin
          DeliverMessage(OwnerObject, PLMsg^);
          Exit;
        End;
  {$ELSE VER1_1}
    If (OwnerObject <> Nil) And (PLMsg^.Msg <> -1) Then
      DeliverMessage(OwnerObject, PLMsg^);
  {$ENDIF VER1_1}

  Assert(False, 'Trace:WindowProc - Exit');
End;

{------------------------------------------------------------------------------
 Function: TimerWindowProc
 Params: Window - The window that receives a message for the timer window
         Msg    - The message received
         WParam - Word parameter
         LParam - Long-integer parameter
  Returns: 0 if Msg is handled; non-zero long-integer result otherwise

  Handles the messages sent to the timer window by Windows
 ------------------------------------------------------------------------------}
FUNCTION TimerWindowProc(window_hwnd : hwnd; msg : DWORD; wParam : WPARAM; lParam : LPARAM) : LRESULT; stdcall;
Var
  TimerInfo: PWin32TimerInfo;
begin
  if (msg = WM_TIMER) and (WParam<>0) then begin
    TimerInfo := PWin32TimerInfo(WParam);
    TimerInfo^.TimerFunc;
    Result := 0;
  end else Result := DefWindowProc(window_hwnd, Msg, WParam, LParam);
end;

{------------------------------------------------------------------------------
 Function: ToolBtnWindowProc
 Params: Window_hwnd - The window that receives a message for the window
         Msg         - The message received
         WParam      - Word parameter
         LParam      - Long-integer parameter
  Returns: non-zero long-integer

  Handles the messages sent to the toolbar button by Windows
 ------------------------------------------------------------------------------}
Function ToolBtnWindowProc(window_hwnd : hwnd; msg : DWORD; wParam : WPARAM; lParam : LPARAM) : LRESULT; stdcall;
begin
  Result := DefWindowProc(window_hwnd, Msg, WParam, LParam);
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{
  $Log$
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
  Keith: Fixed TPage creation

  Revision 1.3  2002/01/05 13:16:08  lazarus
  MG: win32 interface update from Keith Bowes

  Revision 1.2  2001/08/02 12:58:35  lazarus
  MG: win32 interface patch from Keith Bowes

  Revision 1.1  2000/07/13 10:28:30  michael
  + Initial import
}
