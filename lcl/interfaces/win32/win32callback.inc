// included by win32int.pp

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

{ addition XP messages }

const
  WM_THEMECHANGED = $31A;

{-----------------------------------------------------------------------------
  Function: DisableWindowsProc
  Params: Window - handle of toplevel windows to be disabled
          Data   - handle of current window form
  Returns: Whether the enumeration should continue

  Used in LM_SHOWMODAL to disable the windows of application thread
  except the current form.
 -----------------------------------------------------------------------------}
Function DisableWindowsProc(Window: Hwnd; Data: LParam): LongBool; StdCall;
var
  Buffer: array[0..15] of Char;
Begin
  Result:=true;
  // Don't disable the current window form
  if Window=HWND(Data) then exit;

  // Don't disable any ComboBox listboxes
  if (GetClassName(Window, @Buffer, sizeof(Buffer))<sizeof(Buffer))
    and (stricomp(Buffer, 'ComboLBox')=0) then exit;

  EnableWindow(Window,False);
End;

{-----------------------------------------------------------------------------
  Function: EnableWindowsProc
  Params: Window - handle of toplevel windows to be enabled
          Data   - nothing
  Returns: Whether the enumeration should continue

  Called when handling WM_DESTROY for a modal form to enable
  the windows of application thread
 -----------------------------------------------------------------------------}
Function EnableWindowsProc(Window: Hwnd; Data: LParam): LongBool; StdCall;
Begin
  Result:=true;
  EnableWindow(Window,True);
End;

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
  Assert(False, Format('Trace:PropEnumProc - Property %S (with value 0x%X) from window 0x%X removed', [String(Str), Data, Window]));
  RemoveProp(Window, Str);
  Result := True;
  Assert(False, 'Trace:PropEnumProc - Exit');
End;                                              

{------------------------------------------------------------------------------
 Function: CallDefaultWndProc
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
  PrevWndProc: Pointer;
begin
  PrevWndProc := Pointer(Windows.GetProp(Window, 'DefWndProc'));
  if PrevWndProc = nil then
    Result := Windows.DefWindowProc(Window, Msg, WParam, LParam)
  else
    Result := Windows.CallWindowProc(Windows.WNDPROC(PrevWndProc), Window, Msg, WParam, LParam);
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
Function WindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
Var
  LMessage: TLMessage;
  PLMsg: PLMessage;
  R: TRect;
  P: TPoint;
  NewLeft, NewTop, NewWidth, NewHeight: integer;
  OwnerObject: TObject;
  MsgObject: TObject;
  TheWinControl: TWinControl;
  WinProcess: Boolean;
  NotifyUserInput: Boolean;
  OverlayWindow: HWND;
  TargetWindow: HWND;

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
{$IFDEF VER1_1_MSG}  // should be 'ifndef VER1_0' when we fix TMsgArray
  List: TMsgArray;
  C: Integer;
{$ENDIF}

  procedure ShowHideTabPage(NotebookHandle: HWnd; Showing: boolean);
  var
    NoteBook: TCustomNotebook;
    PageIndex, Flags: Integer;
    PageHandle: HWND;
  begin
    Notebook := TObject(GetProp(NotebookHandle, 'Wincontrol')) as TCustomNotebook;
    PageIndex := Windows.SendMessage(NotebookHandle, TCM_GETCURSEL, 0, 0);
    if PageIndex = -1 then exit;
    PageHandle := Notebook.CustomPage(PageIndex).Handle;
    if Showing then
      Flags := SW_SHOW
    else
      Flags := SW_HIDE;
    Windows.ShowWindow(PageHandle, Flags)
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
    AWinControl: TWinControl;
    PaintMsg: TLMPaint;
    ORect: TRect;
  begin
    // note: ignores the received DC
    // do not use default deliver message
    if (OwnerObject=nil) or (not (OwnerObject is TWinControl)) then
      exit;              
    
    // create a paint message
    WinProcess := false;
    AWinControl := TWinControl(OwnerObject);
    if AWinControl.DoubleBuffered then
    begin
      DC := Windows.GetDC(0);
      GetWindowSize(Window, MemWidth, MemHeight);
      MemBitmap := Windows.CreateCompatibleBitmap(DC, MemWidth, MemHeight);
      Windows.ReleaseDC(0, DC);
      MemDC := Windows.CreateCompatibleDC(0);
      OldBitmap := Windows.SelectObject(MemDC, MemBitmap);
      PaintMsg.DC := MemDC;
    end;
    
    if not GetLCLClientBoundsOffset(AWinControl.Handle, ORect) then
    begin
      ORect.Left := 0;
      ORect.Top := 0;
      { we don't use ORect.Right and ORect.Bottom, initialize here if needed }
    end;
    try
      DC := Windows.BeginPaint(Window, @PS);
      PaintMsg.Msg := LM_PAINT;
      PaintMsg.PaintStruct := @PS;
      if not AWinControl.DoubleBuffered then
        PaintMsg.DC := DC;
      AWinControl.EraseBackground(PaintMsg.DC);
      MoveWindowOrgEx(PaintMsg.DC, ORect.Left, ORect.Top);
      DeliverMessage(OwnerObject, PaintMsg);
      MoveWindowOrgEx(PaintMsg.DC, -ORect.Left, -ORect.Top);
      if AWinControl.DoubleBuffered then
        Windows.BitBlt(DC, 0, 0, MemWidth, MemHeight, MemDC, 0, 0, SRCCOPY);
      Windows.EndPaint(Window, @PS);
    finally
      if AWinControl.DoubleBuffered then
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
        if I < TCheckListBox(OwnerObject).Items.Count then
          TCheckListBox(OwnerObject).Checked[I] := not TCheckListBox(OwnerObject).Checked[I];
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
         Windows.SendMessage(TRadioButton(Sibling).Handle, BM_SETCHECK, Windows.WParam(BST_UNCHECKED), 0);
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
      // Index might be -1,
      // if current text is not in the list.
      if (Index>=0) then
        Text := Items[Index]
    end;
  end;

Begin
  Assert(False, 'Trace:WindowProc - Start');

  LMessage.Result := 0;
  LMessage.Msg := LM_NULL;
  PLMsg := @LMessage;
  WinProcess := True;
  NotifyUserInput := False;

  Assert(False, 'Trace:WindowProc - Getting Object With Callback Procedure');
  OwnerObject := TObject(GetProp(Window, 'Wincontrol'));
  if OwnerObject is TWinControl then begin
    TheWinControl := TWinControl(OwnerObject);
  end else begin
    TheWinControl := nil;
  end;
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
      if WParam <> 0 then
        if Window = TWin32WidgetSet(InterfaceObject).AppHandle then
          Windows.SetWindowPos(TWin32WidgetSet(InterfaceObject).AppHandle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
    End;
    BM_SETCHECK:
    Begin
      LMessage.Msg := LM_CHANGED;
      if (WParam=BST_CHECKED) and (OwnerObject is TRadioButton) then
        ClearSiblingRadioButtons(TRadioButton(OwnerObject));
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
        Msg := LM_CHAR;
        KeyData := LParam;
        CharCode := Word(WParam);
        Assert(False,Format('WM_CHAR KeyData= %d CharCode= %d ',[KeyData,CharCode]));
      End;
    End;
    WM_CLOSE:
    Begin
      LMessage.Msg := LM_CLOSEQUERY;
      // default is to destroy window, inhibit
      WinProcess := false;
    End;
    WM_COMMAND:
    Begin
      if LParam=0 then OwnerObject := GetMenuItemObject {menuitem or shortcut}
      else OwnerObject := TObject(GetProp(LParam, 'Wincontrol'));      

// is this for speedbuttons?
//      If ((OwnerObject Is TControl) And (Not (OwnerObject Is TButton))) Then
//        CallEvent(OwnerObject, TControl(OwnerObject).OnClick, Nil, etNotify)

      if OwnerObject is TMenuItem then
      begin
        if (Hi(WParam) = 0) or (Hi(WParam) = 1) then
          LMessage.Msg := LM_ACTIVATE;
      end else if OwnerObject is TButton then
        case Hi(WParam) of
          BN_CLICKED:   LMessage.Msg := LM_CLICKED;
          BN_KILLFOCUS: LMessage.Msg := LM_EXIT;
        end
      else if (OwnerObject is TEdit) or (OwnerObject is TCustomSpinEdit) then
        case Hi(WParam) of
          EN_CHANGE:    LMessage.Msg := CM_TEXTCHANGED;
        end
      else if OwnerObject is TCustomCombobox then
        case Hi(WParam) of
          CBN_EDITCHANGE: LMessage.Msg := LM_CHANGED;
          { CBN_EDITCHANGE is only sent after the user changes the edit box.
            CBN_SELCHANGE is sent when the user changes the text by
            selecting in the list, but before text is actually changed.
            itemindex is updated, so set text manually }
          CBN_SELCHANGE: begin
            UpdateComboBoxText(TCustomComboBox(OwnerObject));
            LMessage.Msg := LM_CHANGED;
          end;
          { closeup message is sent before text is actually changed... *sigh*
            itemindex is updated, so set text manually }
          CBN_CLOSEUP:
            UpdateComboBoxText(TCustomComboBox(OwnerObject));
        end;
      // no specific message found? try send a general msg
      if LMessage.Msg = LM_NULL then
        if OwnerObject is TControl then
          TControl(OwnerObject).Perform(CN_COMMAND, WParam, LParam);
    End;
    WM_CREATE:
    Begin
      Assert(False, 'Trace:WindowProc - Got WM_CREATE');
      LMessage.Msg := LM_CREATE;
    End;
    WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
    Begin
      if HWnd(Windows.GetProp(Window, 'Overlay')) = HWnd(LParam) then
      begin
        LMessage.Result := Windows.GetStockObject(NULL_BRUSH);
        Windows.SetBkMode(HDC(WParam), TRANSPARENT);
        WinProcess := false;
      end else begin
        MsgObject := TObject(GetProp(LParam, 'WinControl'));
        if MsgObject is TWinControl then
        begin
          Windows.SetTextColor(HDC(WParam), Windows.COLORREF(ColorToRGB(TWinControl(MsgObject).Font.Color)));
          Windows.SetBkColor(HDC(WParam), Windows.COLORREF(ColorToRGB(TWinControl(MsgObject).Brush.Color)));
          LMessage.Result := LResult(TWinControl(MsgObject).Brush.Handle);
          // Override default handling
          WinProcess := false;
        end;
      end;
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
      If OwnerObject Is TCustomForm then
        If fsModal in TCustomForm(OwnerObject).FormState then
          EnumThreadWindows(GetWindowThreadProcessId(Window,nil),@EnableWindowsProc, Window);
      if OwnerObject is TCheckListBox then
        TWin32CheckListBoxStrings.DeleteItemRecords(Window);
      EnumProps(Window, @PropEnumProc);
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
      OwnerObject := TObject(WParam);
      if (OwnerObject is TCustomListbox) and
            (TCustomListBox(OwnerObject).Style <> lbStandard) then
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
    End;
    WM_HSCROLL:
    Begin
      PLMsg:=@LMScroll;
      With LMScroll Do
      Begin
        Msg := LM_HSCROLL;
        ScrollCode := SmallInt(Lo(WParam));
        Pos := SmallInt(Hi(WParam));
        ScrollBar := HWND(LParam);
        if ScrollBar <> 0 then
        begin
          // send message to actual scrollbar window
          OwnerObject := TObject(Windows.GetProp(ScrollBar, 'Wincontrol'));
        end;
      End;
    End;
    WM_ERASEBKGND:
    Begin 
      LMessage.Msg := LM_ERASEBKGND;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
      WinProcess := false;
    End;
    WM_GETDLGCODE:
    Begin
      LMessage.Msg := LM_GETDLGCODE;
      if OwnerObject is TEdit then
      begin
        WinProcess := false;
        LMessage.Result := DLGC_WANTALLKEYS;
      end;
    End;
    WM_KEYDOWN:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      With LMKey Do
      Begin
        Msg := CN_KEYDOWN;
        KeyData := LParam;
        CharCode := Word(WParam);
        Assert(False,Format('WM_KEYDOWN KeyData= %d CharCode= %d ',[KeyData,CharCode]));
        Assert(False,'  OwnerObject= '+TComponent(OwnerObject).Name+':'+OwnerObject.ClassName);
      End;
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
        Assert(False,Format('WM_KEYUP KeyData= %d CharCode= %d ',[KeyData,CharCode]));
      End;
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
      if OwnerObject is TCheckListBox then
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
      if OwnerObject is TCheckListBox then
        CheckListBoxLButtonDown;
      // RadioButton functionality
      if OwnerObject is TRadioButton then
        Windows.SendMessage(Window, BM_SETCHECK, BST_CHECKED, 0);
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
        if Windows.GetProp(TargetWindow, 'ComboEdit') <> 0 then
          TargetWindow := Windows.GetParent(TargetWindow);
        if TargetWindow <> Window then
        begin
          Result := SendMessage(TargetWindow, WM_MOUSEWHEEL, WParam, LParam);
          exit;
        end;

        // the mousewheel message is for us
        // windows handles combobox's mousewheel messages
        if TWinControl(OwnerObject).FCompStyle <> csComboBox then
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
    WM_NCHITTEST:
    begin
      if (OwnerObject is TControl) then
      begin
        if TControl(OwnerObject).FCompStyle = csHintWindow then
        begin
          LMessage.Result := HTTRANSPARENT;
          WinProcess := false;
        end;
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
        LMMouse.Result := 0;
      End;
    End;
    WM_SETCURSOR:
    begin
      if (TheWinControl <> nil) and (TheWinControl.Cursor <> crDefault) and
        not (csDesigning in TheWinControl.ComponentState) then
      begin
        Windows.SetCursor(Windows.LoadCursor(0, LclCursorToWin32CursorMap[TheWinControl.Cursor]));
        LMessage.Result := 1;
      end else begin
        LMessage.Msg := LM_SETCURSOR;
        LMessage.WParam := WParam;
        LMessage.LParam := LParam;
      end;
      WinProcess := false;
    end;
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
    WM_SYSKEYDOWN:
    Begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      With LMKey Do
      Begin
        Msg := LM_SYSKEYDOWN;
        KeyData := LParam;
        CharCode := Word(WParam);
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
      PLMsg:=@LMScroll;
      With LMScroll Do
      Begin
        Msg := LM_VSCROLL;
        ScrollCode := SmallInt(Lo(WParam));
        Pos := SmallInt(Hi(WParam));
        ScrollBar := HWND(LParam);
        if ScrollBar <> 0 then
        begin
          // send message to actual scrollbar window
          OwnerObject := TObject(Windows.GetProp(ScrollBar, 'Wincontrol'));
        end;
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
      // cross-interface compatible: complete invalidate on resize
      if (PWindowPos(LParam)^.flags and SWP_NOSIZE) = 0 then
        Windows.InvalidateRect(Window, nil, true);
    End;
    WM_MEASUREITEM:
    Begin
      OwnerObject := TObject(WParam);
      LMessage.Msg := LM_MEASUREITEM;
      LMessage.LParam := LParam;
      LMessage.WParam := WParam;
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
        If (Windows.GetParent(Window) = 0)
        and (OwnerObject is TCustomForm)
        and (TCustomForm(OwnerObject).Parent=nil) then begin
          if Windows.GetWindowRect(Window,@R) then
          begin
            XPos := R.Left;
            YPos := R.Top;
          end else begin
            Msg := LM_NULL;
          end;
        end
        Else begin
          if GetWindowRelativePosition(Window,NewLeft,NewTop) then
          begin
            XPos := NewLeft;
            YPos := NewTop;
          end else begin
            Msg := LM_NULL;
          end;
        end;
        if TheWinControl <> nil then begin
          {$IFDEF VerboseSizeMsg}
          writeln('Win32CallBack WM_MOVE ',TheWinControl.Name,':',TheWinControl.ClassName,
            ' NewPos=',XPos,',',YPos);
          {$ENDIF}
          if (TheWinControl.Left=XPos) and (TheWinControl.Top=YPos) then
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
        if (OwnerObject is TWinControl) then begin
          TheWinControl:=TWinControl(OwnerObject);
          {$IFDEF VerboseSizeMsg}
          GetClientRect(Window,R);
          writeln('Win32Callback: WM_SIZE ',TheWinControl.Name,':',TheWinControl.ClassName,
            ' NewSize=',Width,',',Height,
            ' HasVScroll=',(Windows.GetWindowLong(Window, GWL_STYLE) and WS_VSCROLL) <> 0,
            ' HasHScroll=',(Windows.GetWindowLong(Window, GWL_STYLE) and WS_HSCROLL) <> 0,
            ' OldClientSize=',TheWinControl.CachedClientWidth,',',TheWinControl.CachedClientHeight,
            ' NewClientSize=',R.Right,',',R.Bottom);
          {$ENDIF}
          if (TheWinControl.Width=Width)
          and (TheWinControl.Height=Height)
          and (not TheWinControl.ClientRectNeedsInterfaceUpdate) then
            exit;
          TheWinControl.InvalidateClientRectCache(true);
        end;
        OverlayWindow := Windows.GetProp(Window, 'Overlay');
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

  {$IFDEF VER1_1_MSG}
    List := TMsgArray(GetProp(Window, 'MsgList'));
    If Pointer(List) <> Nil Then
      For C := 0 To Length(List) Do
        If List[C] = PLMsg^.Msg Then
        Begin
          DeliverMessage(OwnerObject, PLMsg^);
          Exit;
        End;
  {$ELSE VER1_1_MSG}
    If (OwnerObject <> Nil) And (PLMsg^.Msg <> LM_NULL) Then
    Begin
      DeliverMessage(OwnerObject, PLMsg^);
    End;
  {$ENDIF VER1_1_MSG}

  case Msg of
    WM_ERASEBKGND:
    begin
      // Groupbox (which is a button) doesn't erase it's background properly
      // it's needed for winxp themes where controls send the WM_ERASEBKGND
      // message to their parent to clear their background and then draw
      // transparently
      if (LMessage.Result = 0) and (TheWinControl <> nil) and 
        (TheWinControl.FCompStyle = csGroupBox) then
      begin
        TheWinControl.EraseBackground(WParam)
      end else begin
        WinProcess := true;
      end;
    end;
    WM_SETCURSOR:
    begin
      if LMessage.Result = 0 then
        WinProcess := true;
    end;
  end;

  if WinProcess then
  begin    
    PLMsg^.Result := CallDefaultWindowProc(Window, Msg, WParam, LParam); 
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
      Windows.PostMessage(TWinControl(Windows.GetProp(Window, 'Wincontrol')).Handle, Msg, WParam, LParam);
    end;
  else
    Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
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
  if (Msg = WM_KILLFOCUS) and (Windows.GetTopWindow(Window) <> HWND(nil)) then
  begin
    // continue normal processing, don't send to lcl
    Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
  end else begin
    // normal processing
    Result := WindowProc(Window, Msg, WParam, LParam);
  end;
end;
    
{------------------------------------------------------------------------------
 Function: ChildEditWindowProc
 Params: Window - The window that receives a message
         Msg    - The message received
         WParam - Word parameter
         LParam - Long-integer parameter
  Returns: 0 if Msg is handled; non-zero long-integer result otherwise

  Handles the messages sent to the edit child of a (combobox, spinedit) control 
  by Windows or other applications
 ------------------------------------------------------------------------------}
function ChildEditWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  LMessage: TLMessage;
  LMKey: TLMKey;
  PLMsg: PLMessage;
  NotifyUserInput: Boolean;
begin
  // filter messages we want to send to LCL
  if (Msg = WM_KILLFOCUS) or (Msg = WM_SETFOCUS) or 
     ((Msg >= WM_KEYFIRST) and (Msg <= WM_KEYLAST)) then
  begin
    // TODO: code copied from WindowProc
    PLMsg := @LMessage;
    case Msg of
      WM_KILLFOCUS, WM_SETFOCUS:
      begin
        LMessage.Msg := Msg;
      end;
      WM_KEYDOWN:
      begin
        NotifyUserInput := True;
        PLMsg:=@LMKey;
        with LMKey Do
        begin
          Msg := CN_KEYDOWN;
          KeyData := LParam;
          CharCode := Word(WParam);
          Assert(False,Format('WM_KEYDOWN KeyData= %d CharCode= %d ',[KeyData,CharCode]));
        end;
      end;
      WM_KEYUP:
      begin
        NotifyUserInput := True;
        PLMsg:=@LMKey;
        with LMKey do
        begin
          Msg := CN_KEYUP;
          KeyData := LParam;
          CharCode := Word(WParam);
          Assert(False,Format('WM_KEYUP KeyData= %d CharCode= %d ',[KeyData,CharCode]));
        end;
      end;
    end;
    // application processing
    if NotifyUserInput then
      NotifyApplicationUserInput(PLMsg^.Msg);

    DeliverMessage(TObject(Windows.GetProp(GetParent(Window), 'Wincontrol')), PLMsg^);
  end;
    
  Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
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
  Result := Windows.DefWindowProc(window_hwnd, Msg, WParam, LParam);
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
    if TimerInfo^.TimerID=idEvent
      then TimerInfo^.TimerFunc;
  end;
end;

{------------------------------------------------------------------------------
 Function: BrowseForFolderCallback
 Params: Window_hwnd - The window that receives a message for the window
         Msg         - The message received
         LParam      - Long-integer parameter
         lpData      - Data parameter, contains initial path.
  Returns: non-zero long-integer

  Handles the messages sent to the toolbar button by Windows
 ------------------------------------------------------------------------------}
Function BrowseForFolderCallback(hwnd : Handle; uMsg : UINT;
  lParam, lpData : LPARAM) : Integer; stdcall;
begin
  case uMsg of
    BFFM_INITIALIZED:
        // Setting root dir
        SendMessage(hwnd, BFFM_SETSELECTION, ULONG(True), lpData);
    //BFFM_SELCHANGED
    //  : begin
    //    if Assigned(FOnSelectionChange) then .....
    //    end;
  end;
  Result := 0;
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{
  $Log$
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
