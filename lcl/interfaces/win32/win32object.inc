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

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
Constructor TWin32WidgetSet.Create;
Begin
  Inherited Create;
  FTimerData  := TList.Create;
  FMetrics.cbSize := SizeOf(FMetrics);
  FMetricsFailed := not Windows.SystemParametersInfo(SPI_GETNONCLIENTMETRICS,
    SizeOf(FMetrics), @FMetrics, 0);
  if FMetricsFailed then
  begin
    FMetrics.iMenuHeight := GetSystemMetrics(SM_CYMENU);
  end;
  OnClipBoardRequest := nil;
  // see if XP themes are available, first check if correct
  // common control library is loaded for themes support
  if ((GetFileVersion('comctl32.dll') shr 16) and $FFFF) >= 6 then
  begin
    FThemeLibrary := LoadLibrary('uxtheme.dll');
    if FThemeLibrary <> 0 then
    begin
      // load functions
      Pointer(IsThemeActive) := GetProcAddress(FThemeLibrary, 'IsThemeActive');
      Pointer(IsAppThemed) := GetProcAddress(FThemeLibrary, 'IsAppThemed');
    end else begin
      IsThemeActive := nil;
      IsAppThemed := nil;
    end;
  end;
  Pointer(InitCommonControlsEx) := GetProcAddress(GetModuleHandle('comctl32.dll'), 'InitCommonControlsEx');

  // init
  UpdateThemesActive;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
Destructor TWin32WidgetSet.Destroy;
var
  n: integer;
  TimerInfo : PWin32TimerInfo;
Begin
  Assert(False, 'Trace:TWin32WidgetSet is being destroyed');

  n := FTimerData.Count;
  if (n > 0) then
  begin
    Writeln(Format('[TWin32WidgetSet.Destroy] WARNING: There are %d TimerInfo structures left, I''ll free them' ,[n]));
    while (n > 0) do
    begin
      dec(n);
      TimerInfo := PWin32Timerinfo(FTimerData[n]);
      Dispose(TimerInfo);
      FTimerData.Delete(n);
    end;
  end;

  if FStockNullBrush <> 0 then
  begin
    DeleteObject(FStockNullBrush);
    DeleteObject(FStockBlackBrush);
    DeleteObject(FStockLtGrayBrush);
    DeleteObject(FStockGrayBrush);
    DeleteObject(FStockDkGrayBrush);
    DeleteObject(FStockWhiteBrush);
  end;

  if FStatusFont <> 0 then
  begin
    Windows.DeleteObject(FStatusFont);
    Windows.DeleteObject(FMessageFont);
  end;

  FTimerData.Free;

  if FAppHandle <> 0 then
    DestroyWindow(FAppHandle);

  Windows.UnregisterClass(@ClsName, System.HInstance);

  if FThemeLibrary <> 0 then
    FreeLibrary(FThemeLibrary);

  inherited Destroy;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppInit
  Params:  None
  Returns: Nothing

  Initialize Windows
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.AppInit;
Var
  LogBrush: TLOGBRUSH;
  SysMenu: HMENU;
  ICC: TINITCOMMONCONTROLSEX;
Begin
  Assert(False, 'Trace:Win32Object.Init - Start');
  If Not WinRegister then
  Begin
    Assert(False, 'Trace:Win32Object.Init - Register Failed');
    writeln('Trace:Win32Object.Init - Register Failed');
    Exit;
  End;

  //Init stock objects;
  LogBrush.lbStyle := BS_NULL;
  FStockNullBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := $000000;
  FStockBlackBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbColor := $C0C0C0;
  FStockLtGrayBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbColor := $808080;
  FStockGrayBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbColor := $404040;
  FStockDkGrayBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbColor := $FFFFFF;
  FStockWhiteBrush := CreateBrushIndirect(LogBrush);

  if FMetricsFailed then
  begin
    FStatusFont := Windows.GetStockObject(DEFAULT_GUI_FONT);
    FMessageFont := Windows.GetStockObject(DEFAULT_GUI_FONT);
  end else begin
    FStatusFont := Windows.CreateFontIndirect(@FMetrics.lfStatusFont);
    FMessageFont := Windows.CreateFontIndirect(@FMetrics.lfMessageFont);
  end;

  //TODO: Remove when the WS interface is implemented
  //      Common controls only need to be initialized when used
  //      So they are initialized in the CreateHandle for common controls
  InitCommonControls;
  ICC.dwSize := SizeOf(TINITCOMMONCONTROLSEX);
  ICC.dwICC := ICC_DATE_CLASSES;
  if InitCommonControlsEx <> nil then
    InitCommonControlsEx(@ICC);

  // Create parent of all windows, `button on taskbar'
  FAppHandle := CreateWindow(@ClsName, PChar(Application.Title), WS_POPUP or
    WS_CLIPSIBLINGS or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX,
    0, {Windows.GetSystemMetrics(SM_CXSCREEN) div 2,}
    0, {Windows.GetSystemMetrics(SM_CYSCREEN) div 2,}
    0, 0, HWND(nil), HMENU(nil), HInstance, nil);

  // remove useless menuitems from sysmenu
  SysMenu := Windows.GetSystemMenu(FAppHandle, False);
  Windows.DeleteMenu(SysMenu, SC_MAXIMIZE, MF_BYCOMMAND);
  Windows.DeleteMenu(SysMenu, SC_SIZE, MF_BYCOMMAND);
  Windows.DeleteMenu(SysMenu, SC_MOVE, MF_BYCOMMAND);
  
  Assert(False, 'Trace:Win32Object.Init - Exit');
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.GetOwnerHandle
  Params:  ADialog - dialog to get 'guiding parent' window handle for
  Returns: A window handle

  Returns window handle to be used as 'owner handle', ie. so that the user must
  finish the dialog before continuing
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetOwnerHandle(ADialog : TCommonDialog): HWND;
begin
  with ADialog do
  begin
    if Owner Is TWinControl then
      Result := TWinControl(Owner).Handle
{
    // TODO: fix Application.Handle to be the same as FAppHandle
    else if Owner Is TApplication then
      Result := TApplication(Owner).Handle
}
    else
      Result := FAppHandle;
  end;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.GetText
  Params:  Sender - The control to retrieve the text from
           Data   - Holds the string
  Returns: Whether the text has been retrieved

  Retrieves the text from a Windows control. This is a replacement for
  the LM_GetText message.
 ------------------------------------------------------------------------------}
Function TWin32WidgetSet.GetText(Sender: TComponent; Handle: HWND; var Data: String): Boolean;
Var
  CapLen: Cardinal;
  Caption: PChar;
Begin
  Assert(False, Format('Trace:TWin32WidgetSet.GetText - Start --> %S', [Sender.ClassName]));
  Data := '';
  Result := True;
  Case TControl(Sender).FCompStyle Of
    csComboBox:
    Begin
      // + 1 = terminating null character
      CapLen := Windows.SendMessage(Handle, WM_GETTEXTLENGTH, 0, 0) + 1;
      Caption := StrAlloc(CapLen);
      Windows.SendMessage(Handle, WM_GETTEXT, CapLen, LPARAM(Caption));
      Data := StrPas(Caption);
      StrDispose(Caption);
    End;
    csEdit, csMemo:
    Begin
      CapLen := GetWindowTextLength(Handle);
      Caption := StrAlloc(CapLen + 1);
      GetWindowText(Handle, Caption, CapLen + 1);
      Data := StrPas(Caption);
      StrDispose(Caption);
    End;
    Else
      Result := False;
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.SetLabel
  Params:   Sender - The object to which to apply the label
            Data   - Pointer to the label
  Returns: Nothing

  Sets the label text on a window
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.SetLabel(Sender: TObject; Data: Pointer);

  Procedure SetMenuItemCaption;
  var MenuInfo: MENUITEMINFO;
      Style: integer;
  Begin
    if TMenuItem(Sender).Caption = '-' then Style := MFT_SEPARATOR
    else Style := MFT_STRING;
    with MenuInfo do
    begin
      cbsize:=sizeof(MENUITEMINFO);
      {In Win32 Menu items that are created without a initial caption default to disabled,
       the next three lines are to counter that.}
      fMask:=MIIM_STATE;
      GetMenuItemInfo(TMenuItem(Sender).Parent.Handle,
                      TMenuItem(Sender).Command, false, @MenuInfo);
      if TMenuItem(Sender).Enabled then
        fState := fState and DWORD(not (MFS_DISABLED or MFS_GRAYED));

      fMask:=MIIM_TYPE or MIIM_STATE;
      fType:=Style;
      dwTypeData:=Data;
      if Data <> nil then
        cch := StrLen(PChar(Data));
    end;
    SetMenuItemInfo(TMenuItem(Sender).Parent.Handle,
                    TMenuItem(Sender).Command, false, @MenuInfo);
    // owner could be a popupmenu too
    if (TMenuItem(Sender).Owner is TWinControl) and
        TWinControl(TMenuItem(Sender).Owner).HandleAllocated and
        ([csLoading,csDestroying] * TWinControl(TMenuItem(Sender).Owner).ComponentState = []) then
      DrawMenuBar(TWinControl(TMenuItem(Sender).Owner).Handle);
  End;
  
  procedure SetPageCaption(const Page:TCustomPage);
  var
    TCI: TC_ITEM;
    PageIndex: integer;
    NotebookHandle: HWND;
  begin
    Assert(False, 'Trace: TWin32WidgetSet.SetLabel - Got csPage');
    PageIndex := Page.PageIndex;
    NotebookHandle := Page.Parent.Handle;
    // We can't set label of a page not yet added,
    // Check for valid page index
    if (PageIndex>=0) and
      (PageIndex < Windows.SendMessage(NotebookHandle, TCM_GETITEMCOUNT,0,0)) then
    begin
      // retrieve page handle from tab as extra check (in case page isn't added yet).
      TCI.mask := TCIF_PARAM;
      Windows.SendMessage(NotebookHandle, TCM_GETITEM, PageIndex, LPARAM(@TCI));
      if dword(TCI.lParam)=Page.Handle then
      begin
        Assert(False, Format('Trace:TWin32WidgetSet.SetLabel - label --> %S', [String(PChar(Data))]));
        TCI.mask := TCIF_TEXT;
        TCI.pszText := Data;
        Windows.SendMessage(NotebookHandle, TCM_SETITEM, PageIndex, LPARAM(@TCI));
      end;
    end;
  end;

Var
  Handle: HWnd;
  TCI: TC_ITEM;
Const
  TermChar: PChar = #0#0;
Begin
  If Sender is TMenuItem then
  begin
    SetMenuItemCaption;
    exit;
  end;
  If Sender Is TWinControl Then
    Assert(False, Format('Trace:[TWin32WidgetSet.SetLabel] %S --> label %S', [Sender.ClassName, TControl(Sender).Caption]))
  Else
    Assert(False, Format('Trace:WARNING: [TWin32WidgetSet.SetLabel] %S --> No Decendant of TWinControl', [Sender.ClassName]));

  Handle := (Sender As TWinControl).Handle;
  Assert(Handle<>0,'Trace:WARNING: [TWin32WidgetSet.SetLabel] --> Got NULL handle');
  Assert(False, 'Trace:Setting the label in TWin32WidgetSet.SetLabel');

  Case TControl(Sender).FCompStyle Of
    csBitBtn:
      DrawBitBtnImage(TCustomBitBtn(Sender), PChar(Data));
    csFileDialog, csOpenFileDialog, csSaveFileDialog, csSelectDirectoryDialog,
    csColorDialog, csFontDialog:
    Begin
      Assert(False, Format('Trace:TWin32WidgetSet.SetLabel - Got %S', [CS_To_String(TControl(Sender).FCompStyle)]));
      Assert(False, 'Trace:TWin32WidgetSet.SetLabel - I''m not sure if this''ll work');
      Assert(False, Format('Trace:Is Sender a TCommonDialog - %S', [BOOL_RESULT[Sender Is TCommonDialog]]));
      If Sender Is TCommonDialog Then
        TCommonDialog(Sender).Title := StrPas(Data)
      Else
        TWinControl(Sender).Caption := StrPas(Data);
      Assert(False, Format('Trace:TWin32WidgetSet.SetLabel - Leaving %S', [CS_To_String(TControl(Sender).FCompStyle)]));
    End;
    csComboBox:
    Begin
      Assert(False, Format('Trace:TWin32WidgetSet.SetLabel - Got %S', [CS_To_String(TControl(Sender).FCompStyle)]));
      Assert(False, Format('Trace:TWin32WidgetSet.SetLabel - label --> %S', [String(PChar(Data))]));
      if TCustomComboBox(Sender).Style = csDropDownList then
        Windows.SendMessage(Handle, CB_SELECTSTRING, -1, LPARAM(Data))
      else
        Windows.SendMessage(Handle, WM_SETTEXT, 0, LPARAM(Data));
    End;
    csMemo:
    Begin
      SendMessage(Handle, WM_SETTEXT, 0, LPARAM(Data));
    End;
    csNotebook:
    Begin
      Assert(False, 'Trace: TWin32WidgetSet.SetLabel - Got csNotebook');
      with TLMNotebookEvent(Data^) do
      if Parent=Sender then
      begin
        TCI.mask := TCIF_TEXT;
        Assert(False, Format('Trace:TWin32WidgetSet.SetLabel - label --> %S', [Str]));
        TCI.pszText := PChar(Str);
        Windows.SendMessage(TCustomNotebook(Sender).Handle, TCM_SETITEM, Page, LPARAM(@TCI));
      end
    End;
    csPage:
      SetPageCaption(TCustomPage(Sender));
    csToolButton:
    Begin
      Data := StrCat(Data, TermChar);
      SendMessage(TControl(Sender).Parent.Handle, TB_ADDSTRING, 0, MakeLong(Word(Integer(Data)), 0));
    End;
  Else
    Windows.SetWindowText(Handle, Data);
  End;
  Assert(False, Format('Trace:[TWin32WidgetSet.SetLabel] %S --> END', [Sender.ClassName]));
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.IntSendMessage3
  Params:  LM_Message - message to be processed
           Sender     - sending control
           Data       - pointer to message-specific data (optional)
  Returns: depends on the message and the sender

  Processes messages from different components.

  WARNING: the result of this function sometimes is not always really an
           integer!!!!!
 ------------------------------------------------------------------------------}
Function TWin32WidgetSet.IntSendMessage3(LM_Message: Integer; Sender: TObject; Data: Pointer): Integer;
Var
  //Bitmap: HBITMAP; // Pixel map type image
  //CBI: COMBOBOXINFO;
  DC: HDC;
  Handle: HWND;
  I, Num, Idx: Integer;
  ListItemIndex: TListItem;
  LVI: LV_ITEM;
  PStr, PStr2: PChar;
  SizeRect: TRECT; // used by LM_SETSIZE,LM_INVALIDATE,LM_CLB_SET_CHECKED and LM_REDRAW
  S: String;
  TBB: TBBUTTON;
  WindowStyle: Integer; //used by LM_SETTABPOSITION
  AMenu: TMenu;
  AccelTable: HACCEL;
  TheWinControl: TWinControl;

Begin
  Result := 0;   //default value just in case nothing sets it
  Assert(False, 'Trace:IntSendMessage3 - Start, Received (' + GetMessageName(LM_Message) + ')');
  Assert(False, 'Trace:IntSendMessage3 - Value of Sender = $' + IntToHex(LongInt(Sender), 8));
  Case LM_Message Of
    LM_CREATE:
      CreateComponent(Sender);
    LM_SETCOLOR:
      SetColor(Sender);
    LM_SETPIXEL:
      SetPixel(Sender, Data);
    LM_GETPIXEL:
      GetPixel(Sender, Data);
    LM_SHOWHIDE:
    Begin
      Assert(False, Format('Trace:  [TWin32WidgetSet.IntSendMessage3] %s --> Show/Hide', [Sender.ClassName]));
      ShowHide(Sender);
    End;
    LM_SETLABEL:
      SetLabel(Sender, Data);
    LM_GETVALUE:
      Result := GetValue(Sender, Data);
    LM_SETVALUE:
      Result := SetValue(Sender, Data);
    LM_SETPROPERTIES:
      Result := SetProperties(Sender);
    LM_SETDESIGNING:
      if Data<>nil then EnableWindow((Sender As TWinControl).Handle, boolean(Data^));
    LM_RECREATEWND:
      Result := RecreateWnd(TWinControl(Sender));
    LM_ATTACHMENU:
      AttachMenu(Sender);
        //SH: think of TBitmap.handle!!!!
    LM_SCREENINIT:
    Begin
      if Sender=nil then Handle := GetDesktopWindow
      else Handle := ObjectToHwnd(Sender);
      DC := Windows.GetDC(Handle);
      //WriteLn('LM_SCREENINIT called --> should go to TWin32WidgetSet.Init');
      //WriteLn('TODO: check this');
      PLMScreenInit(Data)^.PixelsPerInchX := GetDeviceCaps(DC, LogPixelsX);
      PLMScreenInit(Data)^.PixelsPerInchY := GetDeviceCaps(DC, LogPixelsY);
      PLMScreenInit(Data)^.ColorDepth := GetDeviceCaps(DC, BitsPixel);
      ReleaseDC(Handle, DC);
    End;
  Else
  Begin
    Handle := ObjectToHWND(Sender);
    If Handle = HWND(Nil) Then
    Begin
      //Assert (False, Format('Trace:[TWin32WidgetSet.IntSendMessage3] %S --> got Handle = Nil', [Sender.ClassName]));
      //Handle := (Sender As TWinControl).Handle;
      //TWinControl(Sender).Handle := Handle;
      //Assert(False, Format('Trace:[TWin32WidgetSet.IntSendMessag3] Sender is %S', [Sender.ClassName]));
      Exit;
    End;
    Case LM_Message of
      LM_APPENDTEXT:
      Begin
        if (Data <> nil) and (PChar(Data)^ <> #0) then 
        begin
          GetText(TComponent(Sender), Handle, S);
          S := S + PChar(Data);
          SetLabel(Sender, PChar(S));
        end;
      End;
      LM_SETTEXT:
        SetText(Handle, Data);
      LM_ADDCHILD:
      Begin
        Assert(False, 'Trace:Adding a child to Parent');
        If (TWinControl(Sender).Parent is TToolbar) Then
        Begin
          Exit;
        End
        Else
        Begin
          With (Sender As TWinControl) Do
          Begin
            Assert(False, Format('Trace:[TWin32WidgetSet.IntSendMessage3] %S --> Calling Add Child: %S', [Parent.ClassName, ClassName]));
            AddChild(Parent.Handle, Handle);
          End;
        End;
      End;
      LM_CLB_SETCHECKED:
      begin
        with TLMSetChecked(Data^) do
        begin
          TWin32CheckListBoxStrings((Sender as TCheckListBox).Items).Checked[Index] := Checked;

          // redraw control
          Windows.SendMessage(Handle, LB_GETITEMRECT, Index, LPARAM(@SizeRect));
          Windows.InvalidateRect(Handle, @SizeRect, FALSE);
        end;
      end;
      LM_CLB_GETCHECKED:
      begin
        Result := integer(TWin32CheckListBoxStrings((Sender as TCheckListBox).Items).Checked[PInteger(data)^]);
      end;
      LM_LV_DELETEITEM:
      Begin
        If Sender Is TListView Then
          ListView_DeleteItem(Handle, Integer(Data^));
      End;
      LM_LV_CHANGEITEM:
      Begin
        If Sender Is TListView Then
        Begin                   
          idx := Integer(Data^);
          ListItemIndex := (Sender As TListView).Items[idx];
          With LVI Do
          Begin
            Mask := LVIF_TEXT;
            IItem := idx;
            iSubItem := 0;
            pszText := PChar(ListItemIndex.Caption);
            ListView_SetItem(Handle, LVI);
          End;
          num := TListView(Sender).Columns.Count - 1;
          if num > ListItemIndex.SubItems.Count
          then num := ListItemIndex.SubItems.Count;
          for i := 1 to num do
          begin            
            LVI.iSubItem := i;
            LVI.pszText := PChar(ListItemIndex.SubItems[i - 1]);
            ListView_SetItem(Handle, LVI);
          end;
          for i := num + 1 to TListView(Sender).Columns.Count - 1 do
          begin
            LVI.iSubItem := i;
            LVI.pszText := #0;                
            ListView_SetItem(Handle, LVI);
          end;
        End;
      End;
      LM_LV_ADDITEM:
      Begin
        If Sender Is TListView Then
        Begin
          idx := TListView(Sender).Items.Count - 1;
          ListItemIndex := TListView(Sender).Items[idx];
          With LVI Do
          Begin
            Mask := LVIF_TEXT;
            iItem := idx;    
            iSubItem := 0;
            pszText := PChar(ListItemIndex.Caption);
          End;
          ListView_InsertItem(Handle, LVI);
          num := TListView(Sender).Columns.Count - 1;
          if num > ListItemIndex.SubItems.Count
          then num := ListItemIndex.SubItems.Count;
          for i := 1 to num do
          begin            
            LVI.iSubItem := i;
            LVI.pszText := PChar(ListItemIndex.SubItems[i - 1]);
            ListView_SetItem(Handle, LVI);
          end;
          for i := num + 1 to TListView(Sender).Columns.Count - 1 do
          begin
            LVI.iSubItem := i;
            LVI.pszText := #0;
            ListView_SetItem(Handle, LVI);
          end;
          
        End;
      End;
      LM_BRINGTOFRONT:
      Begin
        Assert(False, 'Trace:TODO: [TWin32WidgetSet.IntSendMessage3] - LM_BRINGTOFRONT');
        BringWindowToTop(Handle);
      End;
      LM_BTNDEFAULT_CHANGED:
      Begin
        WindowStyle := Windows.GetWindowLong(Handle, GWL_STYLE) and not (BS_DEFPUSHBUTTON or BS_PUSHBUTTON);
        If TCustomButton(Sender).Default then
          WindowStyle := WindowStyle or BS_DEFPUSHBUTTON
        else
          WindowStyle := WindowStyle or BS_PUSHBUTTON;
        Windows.SendMessage(Handle, BM_SETSTYLE, WindowStyle, 1);
      End;
      LM_DESTROY:
      Begin
        If (Sender Is TWinControl) Or (Sender Is TCommonDialog) Then
        Begin
          If Handle <> 0 Then
          begin
            AccelTable := Windows.GetProp(Handle, 'Accel');
            if AccelTable <> 0 then
              DestroyAcceleratorTable(AccelTable);
            DestroyWindow(Handle);
          end;
        End
        Else If Sender Is TMenu Then
        Begin
          If Handle <> 0 Then
            DestroyMenu(Handle)
        End
        Else If Sender Is TMenuItem Then
        Begin
          { not assigned when this the menuitem of a TMenu; handle is destroyed above }
          if Assigned(TMenuItem(Sender).Parent) then
            DeleteMenu((Sender as TMenuItem).Parent.Handle, TMenuItem(Sender).Command, MF_BYCOMMAND);
          AMenu:=TMenuItem(Sender).GetParentMenu;
          if (AMenu<>nil) and (AMenu.Parent<>nil)
          and (AMenu.Parent is TCustomForm)
          and TCustomForm(AMenu.Parent).HandleAllocated 
          and not (csDestroying in AMenu.Parent.ComponentState) then
            DrawMenuBar(TCustomForm(AMenu.Parent).Handle);
        End
        Else
          Assert(False, Format('Trace:I don''t know how to destroy component %S', [Sender.ClassName]));
      End;
      LM_DRAGINFOCHANGED:
      Begin
      End;
      //TBitBtn
      LM_IMAGECHANGED, LM_LAYOUTCHANGED:
      Begin
        if Sender is TCustomBitBtn then
          DrawBitBtnImage(TCustomBitBtn(Sender), PChar(TCustomBitBtn(Sender).Caption));
      End;
{Displays a menu and makes it available for selection. Applications can use this function to display context-sensitive menus,
and will typically supply NULL for the parent_menu_shell, parent_menu_item, func and data parameters.
The default menu positioning function will position the menu at the current pointer position.
menu :  a GtkMenu.
parent_menu_shell :     the menu shell containing the triggering menu item.
parent_menu_item :      the menu item whose activation triggered the popup.
func :  a user supplied function used to position the menu.
data :  user supplied data to be passed to func.
button :        the button which was pressed to initiate the event.
activate_time : the time at which the activation event occurred.
}
      LM_POPUPSHOW:
      Begin
        SetProp(FAppHandle, 'PopupMenu', Pointer(TPopupMenu(Sender).Handle));
        TrackPopupMenuEx(TPopupMenu(Sender).Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON,
          TPoint(Data^).x, TPoint(Data^).y, FAppHandle, Nil);
      End;
      LM_SETFILTER:
      Begin
        //The filter is processed/set inside Execute/CreateCommonDialog
        //not to say that the code is buggy
        {PStr := StrAlloc(Length(TFileDialog(Sender).Filter) + 1);
        Try
          StrPCopy(PStr, TFileDialog(Sender).Filter);
          LPOpenFileName(@Sender)^.LPStrFilter := PStr; <- Invalid Typecast
        Finally
          StrDispose(pStr);
        End; }
      End;
      LM_SETFILENAME:
      Begin
        //The filename is processed/set inside Execute/CreateCommonDialog
        //not to say that the code is buggy
        {PStr := StrAlloc(Length(TFileDialog(Sender).FileName) + 1);
        Try
          StrPCopy(PStr, TFileDialog(Sender).FileName);
          LPOpenFileName(@Sender)^.LPStrFile := PStr; <- Invalid Typecast
        Finally
          StrDispose(pStr);
        End; }
      End;
      LM_SETFOCUS:
      Begin
        If Handle <> 0 Then
          SetFocus(Handle);
      End;
      LM_SETFONT:
      begin
        if Sender is TControl then begin
                Windows.SendMessage(Handle, WM_SETFONT, windows.wParam(integer(data)), 1);
        end;
      end;
      LM_SETSIZE:
      Begin
        If (Sender Is TWinControl) Then // Handle is already tested --> see above
        With PRect(Data)^ do
        begin
          TheWinControl:=TWinControl(Sender);
          SizeRect := PRect(Data)^;
          case TheWinControl.FCompStyle of
            csForm:
            begin
              // the LCL defines the size of a form without border, win32 with.
              // -> adjust size according to BorderStyle
              Windows.AdjustWindowRectEx(@SizeRect, BorderStyleToWin32Flags(TCustomForm(Sender).BorderStyle), false,
                  BorderStyleToWin32FlagsEx(TCustomForm(Sender).BorderStyle));
            end;
          end;
          ResizeChild(TheWinControl, Left, Top,
             SizeRect.Right - SizeRect.Left, SizeRect.Bottom - SizeRect.Top);
        end; //with
      end; //LM_SETSIZE
      LM_SHOWMODAL:
      Begin
        If Sender Is TCommonDialog Then
          Exit
        Else begin
          EnumThreadWindows(GetWindowThreadProcessId(Handle,nil),@DisableWindowsProc, Handle);
          ShowWindow(Handle, SW_Show);
        end;
      End;
      LM_TB_BUTTONCOUNT:
      Begin
        If Sender Is TToolbar Then
          Result := SendMessage(Handle, TB_BUTTONCOUNT, 0, 0)
        Else
          Result := -1;
      End;
      //SH: think of TCanvas.handle!!!!
      LM_REDRAW:
      Begin
        Assert(False, Format('Trace:[TWin32WidgetSet.IntSendMessage3] %S --> Redraw', [Sender.ClassName]));
        If Sender Is TCanvas Then
          ReDraw(TCanvas(Sender))
        Else If Not (Sender Is TCustomSpeedbutton) Then
          ReDraw(Sender)
        Else If Sender Is TCustomSpeedButton Then
          If TCustomSpeedButton(Sender).Visible Then
            TCustomSpeedButton(Sender).Perform(LM_PAINT, 0, 0)
          Else
          Begin
            SizeRect := TCustomSpeedButton(sender).BoundsRect;
            InvalidateRect(TCustomSpeedButton(Sender).Parent.Handle, @SizeRect, True);
          End;
      End;
      LM_ADDPAGE:
      Begin
        Assert(False, Format('Trace:[TWin32WidgetSet.IntSendMessage3] %S --> Add NB page: %S', [Sender.ClassName, TLMNotebookEvent(Data^).Child.ClassName]));
        AddNBPage(Sender as TCustomNotebook, TLMNotebookEvent(Data^).Child as TCustomPage, TLMNotebookEvent(Data^).Page);
      End;
      LM_REMOVEPAGE:
      Begin
        RemoveNBPage(Sender as TCustomNotebook, TLMNotebookEvent(Data^).Page);
      End;
      LM_SHOWTABS:
      Begin
        Result := Ord(True);
        if TLMNotebookEvent(Data^).ShowTabs then
        begin
          AddAllNBPages(Sender as TCustomNotebook);
        end else begin
          RemoveAllNBPages(Sender as TCustomNotebook);
        end;
      End;
      LM_SETTABPOSITION :
      Begin
        // VS: not tested
        With TLMNotebookEvent(Data^) Do
        Begin
          WindowStyle := Windows.GetWindowLong((Sender As TWinControl).Handle, GWL_STYLE);
          Case TTabPosition(TabPosition^) Of
            tpTop:
              WindowStyle := WindowStyle and not(TCS_VERTICAL or TCS_MULTILINE or TCS_BOTTOM);
            tpBottom:
              WindowStyle := (WindowStyle or TCS_BOTTOM) and not (TCS_VERTICAL or TCS_MULTILINE);
            tpLeft:
              WindowStyle := (WindowStyle or TCS_VERTICAL or TCS_MULTILINE) and not TCS_RIGHT;
            tpRight:
              WindowStyle := WindowStyle or (TCS_VERTICAL or TCS_RIGHT or TCS_MULTILINE);
          End;
          Windows.SetWindowLong(TWinControl(Sender).Handle, GWL_STYLE, WindowStyle);
        End;
      End;
      LM_INSERTTOOLBUTTON:
      Begin
        if Sender is TToolButton then
        begin
          Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!');
          Assert(False, 'Trace:Toolbutton being inserted');
          Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!');
          If (Sender is TWinControl) Then
          Begin
            PStr := StrAlloc(Length(TToolButton(Sender).Caption) + 1);
            StrPCopy(PStr, TToolButton(Sender).Caption);
            PStr2 := StrAlloc(Length(TControl(Sender).Hint) + 1);
            StrPCopy(PStr2, TControl(Sender).Hint);
          End
          Else
          Begin
            Raise Exception.Create('Can not assign this control to the toolbar');
            Exit;
          End;

          Num := TToolbar(TWinControl(Sender).Parent).Buttonlist.IndexOf(TControl(Sender));
          If Num < 0 Then
            Num := TToolbar(TWinControl(Sender).Parent).Buttonlist.Count + 1;
          Assert(False, Format('Trace:Num = %d in LM_INSERTTOOLBUTTON', [Num]));

          {Make sure it's created!!}
          If Handle = 0 Then
            IntSendMessage3(LM_CREATE, Sender, Nil);

          With tbb Do
          Begin
            iBitmap := Num;
            idCommand := Num;
            fsState := TBSTATE_ENABLED;
            fsStyle := TBSTYLE_BUTTON;
            iString := Integer(PStr);
          End;

          SendMessage(TWinControl(Sender).Parent.Handle, TB_BUTTONSTRUCTSIZE, SizeOf(TBBUTTON), 0);
          SendMessage(TWinControl(Sender).Parent.Handle, TB_ADDBUTTONS, 1, LParam(LPTBButton(@tbb)));
          StrDispose(pStr);
          StrDispose(pStr2);
          Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!');
        end;
      End;
      LM_DELETETOOLBUTTON:
      Begin
        SendMessage((Sender As TWinControl).Parent.Handle, TB_DELETEBUTTON, 0, 0);
      End;
      LM_INVALIDATE:
      Begin
        Assert(False, 'Trace:Trying to invalidate window... !!!');
        GetClientRect(Handle, SizeRect);
        InvalidateRect(Handle, @SizeRect, True);
      End;
      LM_SETFORMICON:
      Begin
        SetClassLong(Handle, GCL_HIcon, integer(Data));
      End;
      LM_GETITEMS :
      Begin
        If (Sender as TControl).fCompStyle = csCListBox
          Then Data := TWin32CListStringList.Create(Handle,TWinControl(Sender))
        Else
          If (Sender Is TCheckListBox)
            Then Data := TWin32CheckListBoxStrings.Create(Handle,TWinControl(Sender))
          Else Data := TWin32ListStringList.Create(Handle,TWinControl(Sender));
        Result := Integer(Data);
        Windows.SetProp(Handle, 'List', Result);
      End;
      LM_GETTEXT :
      Begin
        Result := Integer(GetText(TComponent(Sender), Handle, PString(Data)^));
      End;
      LM_GETITEMINDEX :
      Begin
        Case (Sender as TControl).FCompStyle Of
          csComboBox:
          Begin
            Result := SendMessage(Handle, CB_GETCURSEL, 0, 0);
            if Result = LB_ERR Then
            Begin
              Assert(False, 'Trace:[TWin32WidgetSet.IntSendMessage3] Could not retrieve item index via LM_GETITEMINDEX; try selecting an item first');
              Result := -1;
            End;
          End;
          csListBox, csCListBox:
          Begin
            Result := SendMessage(Handle, LB_GETCURSEL, 0, 0);
            If Result = LB_ERR Then
            Begin
              Assert(False, 'Trace:[TWin32WidgetSet.IntSendMessage3] Could not retrieve item index via LM_GETITEMINDEX; try selecting an item first');
              Result := -1;
            End;
          End;
          csNotebook:
          Begin
            TLMNotebookEvent(Data^).Page := SendMessage(Handle, TCM_GETCURSEL, 0, 0);
          End;
        End;
      End;
      LM_SETITEMINDEX :
      Begin
        Case (Sender as TControl).FCompStyle Of
          csComboBox: Windows.SendMessage(Handle, CB_SETCURSEL, Windows.WParam(Integer(Data)), 0);
          csListBox, csCListBox:
          Begin
            If TListBox(Sender).MultiSelect Then
              Windows.SendMessage(Handle, LB_SETSEL, Windows.WPARAM(TRUE), Windows.LParam(Integer(Data)))
            Else
              Windows.SendMessage(Handle, LB_SETCURSEL, Windows.WParam(Integer(Data)), 0);
          End;
          csNotebook:
          Begin
            Assert(False, 'Trace:Setting Page to ' + IntToStr(TLMNotebookEvent(Data^).Page));
            ChangeActivePage(TLMNotebookEvent(Data^));
          End;
        End;
      End;
      LM_GETSELSTART:
      Begin
        If (Sender as TControl).FCompStyle = csComboBox Then
        Begin
          Result := Low(Windows.SendMessage(Handle, CB_GETEDITSEL, Windows.WPARAM(nil), Windows.LPARAM(nil)));
        End;
      End;
      LM_GETSELLEN:
      Begin
        If (Sender as TControl).FCompStyle = csComboBox then
        Begin
          Result := Windows.SendMessage(Handle, CB_GETEDITSEL, Windows.WPARAM(nil), Windows.LPARAM(nil));
        End;
      End;
      LM_GETLIMITTEXT:
      Begin
        If (Sender as TControl).FCompStyle = csComboBox Then
        Begin
          Result := Integer(GetProp(Handle, 'LIMIT_TEXT'));
        End;
      End;
      LM_SETSELSTART:
      Begin
        if Sender is TControl then
        case TControl(Sender).FCompStyle of
          csComboBox:
            begin
              SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(integer(Data), integer(Data)));
            end;
          csEdit:
            begin
              SendMessage(Handle, EM_SETSEL, Windows.WParam(Data), Windows.LParam(Data));
            end;
        end;
      End;
      LM_SETSELLEN:
      Begin
        if Sender is TControl then
        Case TControl(Sender).FCompStyle of
          csComboBox:
            begin
              Windows.SendMessage(Handle, CB_GETEDITSEL, Windows.Wparam(@i), Windows.LParam(@Num));
              Num := i + Integer(Data);
              Windows.SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam( i, Num ));
            end;
          csEdit:
            begin
              Windows.SendMessage(Handle, EM_GETSEL, Windows.Wparam(@i), Windows.LParam(@Num));
              Num := i + Integer(Data);
              Windows.SendMessage(Handle, EM_SETSEL, Windows.WParam(i), Windows.LParam(Num));
            end;
        end;
      End;
      LM_GETLINECOUNT:
      Begin
        If Sender Is TCustomMemo Then
          Result := SendMessage(Handle, EM_GETLINECOUNT, 0, 0);
      End;
      LM_GETSELCOUNT:
      Begin
        If Sender Is TCustomListBox then
        begin
          // LB_GETSELCOUNT only works for multiple-selection listboxes
          if TCustomListBox(Sender).MultiSelect then
            Result := Windows.SendMessage(Handle, LB_GETSELCOUNT, 0, 0)
          else
            if Windows.SendMessage(Handle, LB_GETCURSEL, 0, 0) = LB_ERR then
              Result := 0
            else
              Result := 1;
        end;
      End;
      LM_GETSEL:
      Begin
        If Sender Is TCustomListBox then
          Result := Windows.SendMessage(Handle, LB_GETSEL, Windows.WParam(Data^), 0);
      End;
      LM_SETLIMITTEXT:
      Begin
        If (Sender Is TControl) Then
          SetLimitText(Handle, Word(Data^));
      End;
      LM_SORT:
      Begin
        If  (Sender Is TControl) And Assigned(Data) Then
        Begin
          Case TControl(Sender).FCompStyle Of
            csComboBox, csListBox:
              TWin32ListStringList(TLMSort(Data^).List).Sorted :=
                TLMSort(Data^).IsSorted;
            csCListBox:
              TWin32CListStringList(TLMSort(Data^).List).Sorted :=
                TLMSort(Data^).IsSorted;
          End
        End
      End;
      LM_SETSEL:
      Begin
        If (Sender is TControl) And (TControl(Sender).FCompStyle In [csListBox, csCListBox]) And Assigned(Data) Then
        Begin
          If TControl(Sender).FCompStyle = csListBox Then
          Begin
            If TLMSetSel(Data^).Selected Then
              Windows.SendMessage(Handle, LB_SELITEMRANGE, Windows.WParam(True), Windows.LPARAM(MakeLParam(0, 0)))
            Else
              Windows.SendMessage(Handle, LB_SELITEMRANGE, Windows.WParam(False), Windows.LPARAM(MakeLParam(0, 0)));
          End
        End;
      End;
      LM_SETSELMODE:
      Begin
      If Sender Is TCustomListBox Then
        RecreateWnd(TWinControl(Sender));
      End;
      LM_SETBORDER:
      Begin
        If Sender is TControl Then
        Begin
          If (TControl(Sender).FCompStyle = csListBox)
          Or (TControl(Sender).FCompStyle = csCListBox) Then
          Begin
            If TCustomListBox(Sender).BorderStyle = TBorderStyle(bsSingle) Then
              SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) Or WS_EX_CLIENTEDGE)
            Else
              SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) And Not WS_EX_CLIENTEDGE);
          End
        End;
      End;
      LM_SETSHORTCUT:
      Begin
        If Sender is TMenuItem Then
        Begin
          SetLabel(Sender, LPSTR(TMenuItem(Sender).Caption+#9+ShortCutToText(ShortCut(TLMShortCut(Data^).NewKey, TLMShortCut(Data^).NewModifier))));
          if (TMenuItem(Sender).Owner is TWinControl) and TMenuItem(Sender).HandleAllocated then
          begin
            SetAccelKey(TWinControl(TMenuItem(Sender).Owner).Handle, TMenuItem(Sender).Command, TLMShortCut(Data^).NewKey, TLMShortCut(Data^).NewModifier);
          end else begin
            WriteLn('LM_SETSHORTCUT: unable to set shortcut, menu has no window handle');
          end;
        End;
      End;
      Else
        Assert(True, Format ('WARNING: Unhandled message %d in IntSendMessage3 send by %s --> message:Redraw', [LM_Message, Sender.ClassName]));
        // unhandled message
      End; // end of 2nd case
    End; // end of else-part of 1st case
  End; // end of 1st case
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.SetCallback
  Params: Msg    - message for which to set a callback
          Sender - object to which callback will be sent
  Returns:  nothing

  Applies a Message to the sender
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.SetCallback(Msg: LongInt; Sender: TObject);
Var
  {$IFDEF VER1_1_MSG}
  List: TMsgArray;
  {$ENDIF}
  Window: HWnd;
Begin
  Assert(False, 'Trace:TWin32WidgetSet.SetCallback - Start');
  Assert(False, Format('Trace:TWin32WidgetSet.SetCallback - Class Name --> %S', [Sender.ClassName]));
  Assert(False, Format('Trace:TWin32WidgetSet.SetCallback - Message Name --> %S', [GetMessageName(Msg)]));
  If Sender Is TControlCanvas Then
    Window := TControlCanvas(Sender).Handle
  Else If Sender Is TCustomForm Then
    Window := TCustomForm(Sender).Handle
  Else
    Window := TWinControl(Sender).Handle;
  if Window=0 then exit;

  {$IFDEF VER1_1_MSG}
    List := TMsgArray(GetProp(Window, 'MsgList'));
    SetLength(List, Length(List) + 1);
    List[Length(List) + 1] := Msg;
    SetProp(Window, 'MsgList', Pointer(List));
  {$ENDIF}

  Assert(False, 'Trace:TWin32WidgetSet.SetCallback - Exit');
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.RemoveCallbacks
  Params:   Sender - object from which to remove callbacks
  Returns:  nothing

  Removes Call Back Signals from the sender
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.RemoveCallbacks(Sender: TObject);
Var
  {$IFDEF VER1_1_MSG}
  List: TMsgArray;
  {$ENDIF}
  Window: HWnd;
Begin
  If Sender Is TControlCanvas Then
    Window := TControlCanvas(Sender).Handle
  Else If Sender Is TCustomForm Then
    Window := TCustomForm(Sender).Handle
  Else
    Window := (Sender as TWinControl).Handle;
  if Window=0 then exit;

  {$IFDEF VER1_1_MSG}
    List := TMsgArray(GetProp(Window, 'MsgList'));
    Pointer(List) := Nil;
    SetProp(Window, 'MsgList', Pointer(List));
  {$ENDIF}
End;

function TWin32WidgetSet.InitHintFont(HintFont: TObject): Boolean;
begin
  TFont(HintFont).Name := FMetrics.lfStatusFont.lfFaceName;
  TFont(HintFont).Style := [];
  TFont(HintFont).Height := FMetrics.lfStatusFont.lfHeight;
  TFont(HintFont).Color := clInfoText;
  TFont(HintFont).Pitch := fpDefault;
  Result := true;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.HandleEvents
  Params:  None
  Returns: Nothing

  Handle all pending messages
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.HandleEvents;
var
  AMessage: TMsg;
  AccelTable: HACCEL;
Begin
  While PeekMessage(AMessage, HWnd(Nil), 0, 0,PM_REMOVE) Do
  Begin
    AccelTable := HACCEL(Windows.GetProp(AMessage.HWnd, 'Accel'));
    If (AccelTable = HACCEL(nil)) or (TranslateAccelerator(AMessage.HWnd, AccelTable, @AMessage) = 0) Then
    Begin
      TranslateMessage(@AMessage);
      DispatchMessage(@AMessage);
    End;
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.WaitMessage
  Params:  None
  Returns: Nothing

  Passes execution control to Windows
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.WaitMessage;
Begin
  Assert(False, 'Trace:TWin32WidgetSet.WaitMessage - Start');
  Windows.WaitMessage;
  Assert(False,'Trace:Leave wait message');
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Tells Windows to halt and destroy
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.AppTerminate;
Begin
  Assert(False, 'Trace:TWin32WidgetSet.AppTerminate - Start');
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.RecreateWnd
  Params: Sender - The sending object
  Returns: 0

  Creates a window again
 ------------------------------------------------------------------------------}

Function TWin32WidgetSet.RecreateWnd(Sender: TWinControl): Integer;
Var
  AParent : TWinControl;
Begin
  With Sender do
  Begin
    AParent := Parent;
    // Destroy the window
    Parent := Nil;
    // Recreate the window
    Parent := AParent;
    Result:= Integer(Sender.Handle <> 0);
    ResizeChild(Sender,Left,Top,Width,Height);
    ShowHide(Sender);
  End;
End;

{------------------------------------------------------------------------------
  Function: CreateTimer
  Params: Interval:
          TimerFunc: Callback
  Returns: a Timer id (use this ID to destroy timer)

  Design: A timer which calls TimerCallBackProc, is created.
    The TimerCallBackProc calls the TimerFunc.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : integer;
var
  TimerInfo: PWin32TimerInfo;
begin
  Assert(False,'Trace:Create Timer: ' + IntToStr(Interval));
  Result := 0;
  if (Interval > 0) and (TimerFunc <> nil) then begin
    New(TimerInfo);
    TimerInfo^.TimerFunc := TimerFunc;
    TimerInfo^.TimerID := Windows.SetTimer(0, 0, Interval, @TimerCallBackProc);
    if TimerInfo^.TimerID=0 then
      dispose(TimerInfo)
    else begin
      FTimerData.Add(TimerInfo);
      Result := TimerInfo^.TimerID;
    end;
  end;
  Assert(False,'Trace:Result: ' + IntToStr(result));
end;

{------------------------------------------------------------------------------
  Function: DestroyTimer
  Params: TimerHandle
  Returns:
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.DestroyTimer(TimerHandle: Integer) : boolean;
var
  n : integer;
  TimerInfo : PWin32Timerinfo;
begin
  Result:= false;
  Assert(False,'Trace:removing timer: '+ IntToStr(TimerHandle));
  n := FTimerData.Count;
  while (n>0) do begin
    dec(n);
    TimerInfo := FTimerData[n];
    if (TimerInfo^.TimerID=UINT(TimerHandle)) then
    begin
      Result := Boolean(Windows.KillTimer(0, UINT(TimerHandle)));
      FTimerData.Delete(n);
      Dispose(TimerInfo);
    end;
  end;
  Assert(False,'Trace:Destroy timer Result: '+ BOOL_RESULT[result]);
end;

procedure TWin32WidgetSet.AttachMenuToWindow(AMenuObject: TComponent);
var
  AMenu: TMenu;
  AWinControl: TWinControl;
begin
  AMenu := AMenuObject as TMenu;
  if AMenu.FCompStyle = csMainMenu then
  begin
    AWinControl := TWinControl(AMenu.Owner);
    Windows.SetMenu(AWinControl.Handle, AMenu.Handle);
    // inform LCL of changed client size
    AWinControl.DoAdjustClientRectChange;
  end;
end;

{ Private methods (in no significant order) }

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.WinRegister
  Params: None
  Returns: If the window was successfully regitered

  Registers the main window class
 ------------------------------------------------------------------------------}
Function TWin32WidgetSet.WinRegister: Boolean;
Var
  WindowClass: WndClass;
Begin
  Assert(False, 'Trace:WinRegister - Start');
  With WindowClass Do
  Begin
    Style := 0{CS_HRedraw or CS_VRedraw};
    LPFnWndProc := @WindowProc;
    CbClsExtra := 40;
    CbWndExtra := 40;
    HInstance := System.HInstance;
    HIcon := LoadIcon(0, IDI_Application);
    HCursor := LoadCursor(0, IDC_Arrow);
    HBrBackground := 0; {GetSysColorBrush(Color_BtnFace);}
    LPSzMenuName := Nil;
    LPSzClassName := @ClsName;
  End;
  Result := Windows.RegisterClass(@WindowClass) <> 0;
  Assert(False, 'Trace:WinRegister - Exit');
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.UpdateThemesActive
  Params:  None
  Returns: Nothing

  Updates the field FThemesActive to save whether xp themes are active
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.UpdateThemesActive;
begin
  if (IsThemeActive <> nil) and (IsAppThemed <> nil) then
    FThemesActive := IsThemeActive() and IsAppThemed()
  else
    FThemesActive := false;
end;

const
  BUTTON_IMAGELIST_ALIGN_LEFT   = 0;
  BUTTON_IMAGELIST_ALIGN_RIGHT  = 1;
  BUTTON_IMAGELIST_ALIGN_TOP    = 2;
  BUTTON_IMAGELIST_ALIGN_BOTTOM = 3;
  BUTTON_IMAGELIST_ALIGN_CENTER = 4;

  BCM_FIRST = $1600;
  BCM_GETIDEALSIZE  = BCM_FIRST + 1;
  BCM_SETIMAGELIST  = BCM_FIRST + 2;
  BCM_GETIMAGELIST  = BCM_FIRST + 3;
  BCM_SETTEXTMARGIN = BCM_FIRST + 4;
  BCM_GETTEXTMARGIN = BCM_FIRST + 5;

  { - you do need to destroy the imagelist yourself. 
    - you'll need 5 images to support all themed xp button states...

    Image 0 = normal
    Image 1 = mouse hover
    Image 2 = button down
    Image 3 = button disabled
    Image 4 = button focus       
  }

  XPBitBtn_ImageIndexToEnabled: array[0..4] of Boolean = 
    (true, true, true, false, true);
  
type
  BUTTON_IMAGELIST = packed record
    himl: Windows.HIMAGELIST;
    margin: Windows.RECT;
    uAlign: UINT;
  end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.DrawBitBtnImage
  Params:  BitBtn: The TCustomBitBtn to update the image of
           ButtonCaption: new button caption
  Returns: Nothing

  Updates the button image combining the glyph and caption
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.DrawBitBtnImage(BitBtn: TCustomBitBtn;
  ButtonCaption: PChar);
var
  BitmapHandle: HBITMAP; // Handle of the button glyph
  BitBtnLayout: TButtonLayout; // Layout of button and glyph
  BitBtnHandle: HWND; // Handle to bitbtn window
  BitBtnDC: HDC; // Handle to DC of bitbtn window
  OldFontHandle: HFONT; // Handle of previous font in hdcNewBitmap
  hdcNewBitmap: HDC; // Device context of the new Bitmap
  BitmapInfo: BITMAP; // Buffer for bitmap
  TextSize: SIZE; // For computing the length of button caption in pixels
  OldBitmap: HBITMAP; // Handle to the old selected bitmap
  NewBitmap: HBITMAP; // Handle of the new bitmap
  XDestBitmap, YDestBitmap: integer; // X,Y coordinate of destination rectangle for bitmap
  XDestText, YDestText: integer; // X,Y coordinates of destination rectangle for caption
  newWidth, newHeight: integer; // dimensions of new combined bitmap
  BitmapRect: Windows.RECT;
  oldImageList: HIMAGELIST;
  ButtonImageList: BUTTON_IMAGELIST;
  I: integer;

  procedure DrawBitmap(Enabled: boolean);
  var
    SrcDC, MaskDC: HBITMAP;
    MaskBmp, OldSrcBmp, OldMaskBmp: HBITMAP;
    BkColor: TColorRef;
    
    OldBitmapHandle: HBITMAP; // Handle of the provious bitmap in hdcNewBitmap
    BitmapFlags: integer; // flags for glyph (enabled or disabled)
    TextFlags: integer; // flags for caption (enabled or disabled)
  begin
    BitmapFlags := DST_BITMAP;
    TextFlags := DST_PREFIXTEXT;

    OldBitmapHandle := SelectObject(hdcNewBitmap, NewBitmap);
    if Enabled or FThemesActive
    then begin
      // themed 
      if not Enabled 
      then begin
        BitmapFlags := BitmapFlags or DSS_DISABLED;
        TextFlags := TextFlags or DSS_DISABLED;
      end;
      
      Windows.FillRect(hdcNewBitmap, BitmapRect, BitBtn.Brush.Handle);
      if BitmapHandle <> 0 
      then DrawState(hdcNewBitmap, 0, nil, BitmapHandle, 0, XDestBitmap, YDestBitmap, 0, 0, BitmapFlags);
    end
    else begin
      // non themed 
      // When disabled only create a black and white image
      // Let windows itself draw the disabled state
      Windows.FillRect(hdcNewBitmap, BitmapRect, Windows.GetStockObject(WHITE_BRUSH));
      if BitmapHandle <> 0 
      then begin
        // Create a source DC
        SrcDC := CreateCompatibleDC(hdcNewBitmap);
        OldSrcBmp := SelectObject(SrcDC, BitmapHandle);
        // Create a mask DC
        MaskBmp := CreateBitmap(BitmapInfo.bmWidth, BitmapInfo.bmHeight, 1, 1, nil);
        MaskDC := CreateCompatibleDC(hdcNewBitmap);
        OldMaskBmp := SelectObject(MaskDC, MaskBmp);
        // Create the black and white image
        BkColor := SetBkColor(SrcDC, BitBtn.Brush.Color);
        BitBlt(MaskDC, 0, 0, BitmapInfo.bmWidth, BitmapInfo.bmHeight, SrcDC, 0, 0, SrcCopy);
        SetBkColor(SrcDC, BkColor);
        // Draw the black and white image
        BitBlt(hdcNewBitmap, XDestBitmap, YDestBitmap, BitmapInfo.bmWidth, BitmapInfo.bmHeight,
               MaskDC, 0, 0, SRCCOPY);  
  
        SelectObject(SrcDC, OldSrcBmp);
        DeleteDC(SrcDC);
  
        SelectObject(MaskDC, OldMaskBmp);
        DeleteDC(MaskDC);
        DeleteObject(MaskBmp);
      end;  
    end;
    
    SetBkMode(hdcNewBitmap, TRANSPARENT);
    DrawState(hdcNewBitmap, 0, nil, LPARAM(ButtonCaption), 0, XDestText, YDestText, 0, 0, TextFlags);
    SelectObject(hdcNewBitmap, OldBitmapHandle);
  end;
  
begin
  // gather info about bitbtn
  BitBtnHandle := BitBtn.Handle;
  if BitBtn.Glyph.Empty then
  begin
    BitmapHandle := 0;
    BitmapInfo.bmWidth := 0;
    BitmapInfo.bmHeight := 0;
  end else begin
    BitmapHandle := BitBtn.Glyph.Handle;
    Windows.GetObject(BitmapHandle, sizeof(BitmapInfo), @BitmapInfo);
  end;
  BitBtnLayout := BitBtn.Layout;
  BitBtnDC := GetDC(BitBtnHandle);
  hdcNewBitmap := CreateCompatibleDC(BitBtnDC);
  OldFontHandle := SelectObject(hdcNewBitmap, BitBtn.Font.Handle);
  GetTextExtentPoint32(hdcNewBitmap, LPSTR(ButtonCaption), Length(ButtonCaption), TextSize);
  // calculate size of new bitmap
  case BitBtnLayout of
    blGlyphLeft, blGlyphRight:
    begin
      newWidth := TextSize.cx + BitmapInfo.bmWidth;
      if BitmapHandle <> 0 then
        inc(newWidth, 2);
      newHeight := TextSize.cy;
      if newHeight < BitmapInfo.bmHeight then
        newHeight := BitmapInfo.bmHeight;
      YDestBitmap := (newHeight - BitmapInfo.bmHeight) shr 1;
      YDestText := (newHeight - TextSize.cy) shr 1;
    end;
    blGlyphTop, blGlyphBottom:
    begin
      newWidth := TextSize.cx;
      if newWidth < BitmapInfo.bmWidth then
        newWidth := BitmapInfo.bmWidth;
      newHeight := TextSize.cy + BitmapInfo.bmHeight;
      if BitmapHandle <> 0 then
        inc(newHeight, 2);
      XDestBitmap := (newWidth - BitmapInfo.bmWidth) shr 1;
      XDestText := (newWidth - TextSize.cx) shr 1;
    end;
  end;
  case BitBtnLayout of
    blGlyphLeft: 
    begin
      XDestBitmap := 0;
      XDestText := newWidth - TextSize.cx;
    end;
    blGlyphRight: 
    begin
      XDestBitmap := newWidth - BitmapInfo.bmWidth;
      XDestText := 0;
    end;
    blGlyphTop: 
    begin
      YDestBitmap := 0;
      YDestText := newHeight - TextSize.cy;
    end;
    blGlyphBottom: begin
      YDestBitmap := newHeight - BitmapInfo.bmHeight;
      YDestText := 0;
    end;
  end;
  // create new
  if (newWidth = 0) and (newHeight = 0) then
    NewBitmap := 0
  else
    NewBitmap := CreateCompatibleBitmap(BitBtnDC, newWidth, newHeight);
  BitmapRect.left := 0;
  BitmapRect.top := 0;
  BitmapRect.right := newWidth;
  BitmapRect.bottom := newHeight;
  // destroy previous bitmap, set new bitmap
  if FThemesActive then
  begin
    // winxp draws BM_SETIMAGE bitmap with old style button!
    // need to use BCM_SETIMAGELIST
    oldImageList := Windows.SendMessage(BitBtnHandle, BCM_GETIMAGELIST, 0, LPARAM(@ButtonImageList)); 
    if oldImageList <> 0 then
      oldImageList := ButtonImageList.himl;
    if NewBitmap <> 0 then
    begin
      ButtonImageList.himl := ImageList_Create(newWidth, newHeight, ILC_COLORDDB or ILC_MASK, 5, 0);
      ButtonImageList.margin.left := 2;
      ButtonImageList.margin.right := 2;
      ButtonImageList.margin.top := 2;
      ButtonImageList.margin.bottom := 2;
      ButtonImageList.uAlign := BUTTON_IMAGELIST_ALIGN_CENTER;
      // for some reason, if bitmap added to imagelist, need to redrawn, otherwise it's black!?
      for I := 0 to 4 do
      begin
        DrawBitmap(XPBitBtn_ImageIndexToEnabled[I]);
        ImageList_AddMasked(ButtonImageList.himl, NewBitmap, ColorToRGB(BitBtn.Brush.Color));
      end;
    end else begin
      ButtonImageList.himl := 0;
    end;
    Windows.SendMessage(BitBtnHandle, BCM_SETIMAGELIST, 0, LPARAM(@ButtonImageList));
    if oldImageList <> 0 then
      ImageList_Destroy(oldImageList);
    if NewBitmap <> 0 then
      DeleteObject(NewBitmap);
  end else begin
    OldBitmap := Windows.SendMessage(BitBtnHandle, BM_GETIMAGE, IMAGE_BITMAP, 0);
    if NewBitmap <> 0 then
      DrawBitmap(BitBtn.Enabled);
    Windows.SendMessage(BitBtnHandle, BM_SETIMAGE, IMAGE_BITMAP, NewBitmap);
    if OldBitmap <> 0 then
      DeleteObject(OldBitmap);
  end;
  SelectObject(hdcNewBitmap, OldFontHandle);
  DeleteDC(hdcNewBitmap);
  ReleaseDC(BitBtnHandle, BitBtnDC);
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.PaintPixmap
  Params: Surface    - The surface onto which to paint the pixmap
          PixmapData - Data necessary in drawing the pixmap
  Returns: Nothing

  Paints a pixmap on a surface (control).
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.PaintPixmap(Surface: TObject; PixmapData: Pointer);
Var
  DC: HDC;
  Pixmap: HIcon;
Begin
  DC := GetDC((Surface As TWinControl).Handle);
  Pixmap := CreatePixmapIndirect(PixmapData, 0);
  DrawIcon(DC, TWinControl(Surface).Left, TWinControl(Surface).Top, Pixmap);
  ReleaseDC(TWinControl(Surface).Handle, DC);
  DeleteObject(Pixmap);
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.NormalizeIconName
  Params: IconName - The name of the icon to normalize
  Returns: Nothing

  Adjusts an icon name to the proper format
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.NormalizeIconName(Var IconName: String);
Var
  IcoLen: Integer;
Begin
  DoDirSeparators(IconName);
  IcoLen := Pos('.xmp', LowerCase(IconName));
  If IcoLen <> 0 Then
  Begin
    Delete(IconName, IcoLen, Length('.xpm'));
    Insert('.ico', IconName, Length(IconName));
  End
End;

Procedure TWin32WidgetSet.NormalizeIconName(Var IconName: PChar);
Var
  Str: String;
Begin
  Str := String(IconName);
  NormalizeIconName(Str);
  IconName := StrToPChar(Str);
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.SetLimitText
  Params: Window - The window that has the text to be limited
          Limit  - Number of characters to limit the text to
  Returns: Nothing

  Sets the text limit
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.SetLimitText(Window: HWND; Limit: Word);
Var
  Cls: array[0..5] of Char;
  Msg: Cardinal;
  Str: String;
Begin
  GetClassName(Window, @Cls[0], 5);
  Str := Lowercase(PChar(@Cls[0]));
  If Str = 'edit' Then
    Msg := CB_LIMITTEXT
  Else If Str = 'combo' Then
    Msg := EM_LIMITTEXT
  Else
    Exit;

  SendMessage(Window, Msg, Limit, 0);
  SetProp(Window, 'LIMIT_TEXT', @Limit);
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.CreateCommonDialog
  Params: Sender - The sending object
  Returns: Nothing

  Creates a common dialog
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.CreateCommonDialog(Sender: TCommonDialog; CompStyle: Integer);
Const
  { 16 basic RGB colors; names listed in comments for debugging }
  CustomColors: Array[1..16] Of DWord = (
  0, //Black
  $C0C0C0, //Silver
  $808080, //Gray
  $FFFFFF, //White
  $000080, //Maroon
  $0000FF, //Red
  $800080, //Purple
  $FF00FF, //Fuchsia
  $008000, //Green
  $00FF00, //Lime
  $008080, //Olive
  $00FFFF, //Yellow
  $800000, //Navy
  $FF0000, //Blue
  $808000, //Teal
  $FFFF00 //Aqua
  );
Var
  CC: TChooseColor;
  CF: TChooseFont;
  LF: LogFont;
  OpenFile: OpenFileName;
  Ret: WinBool;
  FName: PChar;
  FFilter: string;
  SizeStr:Integer;

  Function GetFlagsFromOptions(Options: TOpenOptions): DWord;
  Begin
    Result := 0;
    If ofAllowMultiSelect In Options Then Result := Result Or OFN_ALLOWMULTISELECT;
    If ofCreatePrompt In Options Then Result := Result Or OFN_CREATEPROMPT;
    If Not (ofOldStyleDialog In Options) Then Result := Result Or OFN_EXPLORER;
    If ofExtensionDifferent In Options Then Result := Result Or OFN_EXTENSIONDIFFERENT;
    If ofFileMustExist In Options Then Result := Result Or OFN_FILEMUSTEXIST;
    If ofHideReadOnly In Options Then Result := Result Or OFN_HIDEREADONLY;
    If ofNoChangeDir In Options Then Result := Result Or OFN_NOCHANGEDIR;
    If ofNoDereferenceLinks In Options Then Result := Result Or OFN_NODEREFERENCELINKS;
    If ofNoLongNames In Options Then  Result := Result Or OFN_NOLONGNAMES;
    If ofNoNetworkButton In Options Then Result := Result Or OFN_NONETWORKBUTTON;
    If ofNoReadOnlyReturn In  Options Then Result := Result Or OFN_NOREADONLYRETURN;
    If ofNoTestFileCreate In Options Then Result := Result Or OFN_NOTESTFILECREATE;
    If ofNoValidate In Options Then Result := Result Or OFN_NOVALIDATE;
    If ofOverwritePrompt In Options Then Result := Result Or OFN_OVERWRITEPROMPT;
    If ofPathMustExist In Options Then Result := Result Or OFN_PATHMUSTEXIST;
    If ofReadOnly In Options Then Result := Result Or OFN_READONLY;
    If ofShareAware In Options Then Result := Result Or OFN_SHAREAWARE;
    If ofShowHelp In Options Then Result := Result Or OFN_SHOWHELP;
  End;

  Function GetFlagsFromOptions(Options : TFontDialogOptions): DWord;
  Begin
    Result := 0;
    If fdAnsiOnly In Options Then Result := Result Or CF_ANSIONLY;
    If fdTrueTypeOnly In Options Then Result := Result Or CF_TTONLY;
    If fdEffects In Options Then Result := Result Or CF_EFFECTS;
    If fdFixedPitchOnly In Options then Result := Result Or CF_FIXEDPITCHONLY;
    If fdForceFontExist In Options then Result := Result Or CF_FORCEFONTEXIST;
    If fdNoFaceSel In Options then Result := Result Or CF_NOFACESEL;
    If fdNoOEMFonts In Options then Result := Result Or CF_NOOEMFONTS;
    If fdNoSimulations In Options then Result := Result Or CF_NOSIMULATIONS;
    If fdNoSizeSel In Options then Result := Result Or CF_NOSIZESEL;
    If fdNoStyleSel In Options then Result := Result Or CF_NOSTYLESEL;
    If fdNoVectorFonts In Options then Result := Result Or CF_NOVECTORFONTS;
    If fdShowHelp In Options then Result := Result Or CF_SHOWHELP;
    If fdWysiwyg In Options then Result := Result Or CF_WYSIWYG;
    If fdLimitSize In Options then Result := Result Or CF_LIMITSIZE;
    If fdScalableOnly In Options then Result := Result Or CF_SCALABLEONLY;
    If fdApplyButton In Options then Result := Result Or CF_APPLY;
  End;

  procedure ReplacePipe(var AFilter:string);
  var i:integer;
  begin
    for i:=1 to length(AFilter) do
      if AFilter[i]='|' then AFilter[i]:=#0;
    AFilter:=AFilter + #0#0;
  end;

  Procedure SetFilesProperty(AFiles:TStrings);
  Var I:Integer;
  begin
    I:=Length(FName);
    If I < OpenFile.nFileOffset then begin
      Inc(FName,Succ(I));
      I:=Length(FName);
      While I > 0 do
      begin
        AFiles.Add(ExpandFileName(StrPas(FName)));
        Inc(FName,Succ(I));
        I:=Length(FName);
      end;
    end
    Else
      AFiles.Add(StrPas(FName));
  end;

  Procedure SetFilesPropertyForOldStyle(AFiles:TStrings);
  Var
    SelectedStr:String;
    I,Start:Integer;
  begin
  SelectedStr:=StrPas(FName);
  I:=Pos(' ',SelectedStr);
  If I = 0 then
    AFiles.Add(SelectedStr)
  Else begin
    Delete(SelectedStr,1,I);
    SelectedStr:=SelectedStr+' ';
    Start:=1;
    For I:= 1 to Length(SelectedStr)  do
      If SelectedStr[I] =  ' ' then begin
        AFiles.Add(ExpandFileName(Copy(SelectedStr,Start,I - Start)));
        Start:=Succ(I);
      End;
    End;
  end;

Begin
  Assert(False, 'Trace:TWin32WidgetSet.CreateCommonDialog - Start');
  Assert(False, Format('Trace:TWin32WidgetSet.CreateCommonDialog - class name --> ', [Sender.ClassName]));
  case CompStyle of
  csColorDialog:
  Begin
    //CC := LPChooseColor(@Sender)^;
    ZeroMemory(@CC, SizeOf(TChooseColor));
    With CC Do
    Begin
      LStructSize := SizeOf(TChooseColor);
      HWndOwner := GetOwnerHandle(Sender);
      RGBResult := ColorToRGB(TColorDialog(Sender).Color);
      LPCustColors := @CustomColors;
      Flags := CC_FullOpen Or CC_RGBInit;
    End;
    Ret := ChooseColor(@CC)
  End;
  csOpenFileDialog, csSaveFileDialog:
  With TOpenDialog(Sender) do
  Begin
    //TODO: HistoryList
    If ofAllowMultiSelect in Options Then
      SizeStr:=15*MAX_PATH  // Tested with 210 selected files
    Else
      SizeStr:=MAX_PATH;
    GetMem(FName,SizeStr+2);
    FillChar(FName^, SizeStr+2, 0);
    StrLCopy(FName,PChar(Filename),SizeStr);
    If Filter <> '' Then Begin
      FFilter := Filter;
      ReplacePipe(FFilter);
    End
    Else
      FFilter:='All File Types(*.*)'+#0+'*.*'+#0#0; // Default -> avoid empty combobox
    ZeroMemory(@OpenFile, SizeOf(OpenFileName));
    With OpenFile Do
    Begin
      LStructSize := SizeOf(OpenFileName);
      HWndOwner := GetOwnerHandle(Sender);
      LPStrFilter := PChar(FFilter);
      LPStrFile := FName;
      LPStrTitle := PChar(Title);
      LPStrInitialDir := PChar(InitialDir);
      NMaxFile := SizeStr;
      Flags := GetFlagsFromOptions(Options);
    End;
    If CompStyle = csSaveFileDialog Then
      Ret := GetSaveFileName(@OpenFile)
    Else
      Ret := GetOpenFileName(@OpenFile);

    Files.Clear;
    If Ret Then Begin
      If Not (ofOldStyleDialog In Options) Then // Win32 returns diferent types of strings
        SetFilesProperty(Files)
      Else
        SetFilesPropertyForOldStyle(Files);
      FileName := Files[0];
    End
    Else
      FileName := '';

    FreeMem(OpenFile.LPStrFile,SizeStr+2); // FName Address is changed, so free the initial @
  End;
  csFontDialog:
  With TFontDialog(Sender) do
  Begin
    ZeroMemory(@CF, SizeOf(TChooseFont));
    ZeroMemory(@LF, SizeOf(LogFont));
    With LF Do
    Begin
      LFHeight := Font.Height;
      LFFaceName := TFontDataName(Font.Name);
      If (fsBold In Font.Style) then LFWeight:= FW_BOLD;
      LFItalic := Byte(fsItalic In Font.Style);
      LFStrikeOut := Byte(fsStrikeOut In Font.Style);
      LFUnderline := Byte(fsUnderline In Font.Style);
      LFCharSet := Font.CharSet;
    End;
    With CF Do
    Begin
      LStructSize := SizeOf(TChooseFont);
      HWndOwner := GetOwnerHandle(Sender);
      LPLogFont := @LF;
      Flags := GetFlagsFromOptions(Options);
      Flags := Flags Or CF_INITTOLOGFONTSTRUCT Or CF_BOTH;
      RGBColors := Font.Color;
    End;
    Ret := ChooseFont(@CF);
  End;
  End;//case

  If Ret Then  Begin
    If CompStyle = csFontDialog then  Begin
      TFontDialog(Sender).Font.Assign(LF);
      TFontDialog(Sender).Font.Color := CF.RGBColors;
    End
    Else If CompStyle = csColorDialog then
      TColorDialog(Sender).Color := CC.RGBResult;
    Sender.UserChoice := mrOK;
  End
  Else
    Sender.UserChoice := mrCancel;

  Assert(False, 'Trace:TWin32WidgetSet.CreateCommonDialog - Exit');
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.CreateSelectDirectoryDialog
  Params: Sender - The sending object: a TSelectDirectoryDialog object
  Returns: Nothing

  Creates a common dialog
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.CreateSelectDirectoryDialog(Sender: TSelectDirectoryDialog);
var
  bi : TBrowseInfo;
  Buffer : PChar;
  iidl : PItemIDList;
  InitialDir: string;
Begin
  Buffer := CoTaskMemAlloc(MAX_PATH);
  InitialDir := Sender.InitialDir;
  if length(InitialDir)>0 then begin
    // remove the \ at the end.
    if Copy(InitialDir,length(InitialDir),1)=PathDelim then
      InitialDir := copy(InitialDir,1, length(InitialDir)-1);
    // if it is a rootdirectory, then the InitialDir must have a \ at the end.
    if Copy(InitialDir,length(InitialDir),1)=DriveDelim then
      InitialDir := InitialDir + PathDelim;
  end;
  With bi do
  Begin
    hwndOwner := GetOwnerHandle(Sender);
    pidlRoot := nil;
    pszDisplayName := Buffer;
    lpszTitle := PChar(Sender.Title);
    ulFlags := BIF_RETURNONLYFSDIRS;
    lpfn := @BrowseForFolderCallback;
    // this value will be passed to callback proc as lpData
    lParam := LclType.LParam(PChar(InitialDir));
  End;

  iidl := SHBrowseForFolder(@bi);

  If Assigned(iidl) Then
  Begin
    SHGetPathFromIDList(iidl, Buffer);
    CoTaskMemFree(iidl);
    Sender.FileName := Buffer;
    Sender.UserChoice := mrOK;
  End
  Else Sender.UserChoice := mrCancel;

  CoTaskMemFree(Buffer);
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.UpdateStatusBarPanel
  Params: StatusPanel - StatusPanel which needs to be update
  Returns: Nothing

  Called by StatusBarPanelUpdate and StatusBarSetText
  Everything is updated except the panel width
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.UpdateStatusBarPanel(StatusPanel: TStatusPanel);
var
  BevelType: integer;
  Text: string;
begin
  Text := StatusPanel.Text;
  case StatusPanel.Alignment of
    taCenter: Text := #9 + Text;
    taRightJustify: Text := #9#9 + Text;
  end;
  case StatusPanel.Bevel of
    pbNone: BevelType := Windows.SBT_NOBORDERS;
    pbLowered: BevelType := 0;
    pbRaised: BevelType := Windows.SBT_POPOUT;
  end;
  Windows.SendMessage(StatusPanel.StatusBar.Handle, SB_SETTEXT, StatusPanel.Index or BevelType, LPARAM(PChar(Text)));
end;

procedure TWin32WidgetSet.UpdateStatusBarPanelWidths(StatusBar: TStatusBar);
var
  Rights: PInteger;
  PanelIndex: integer;
  CurrentRight: integer;
begin
  if StatusBar.Panels.Count=0 then begin
    Windows.SendMessage(StatusBar.Handle, SB_SETPARTS, 0, 0);
    exit;
  end;
  Getmem(Rights, StatusBar.Panels.Count * sizeof(integer));
  try
    CurrentRight := 0;
    for PanelIndex := 0 to StatusBar.Panels.Count-2 do begin
      CurrentRight := CurrentRight + StatusBar.Panels[PanelIndex].Width;
      Rights[PanelIndex] := CurrentRight;
    end;
    Rights[StatusBar.Panels.Count-1] := -1; //Last extends to end;
    Windows.SendMessage(StatusBar.Handle, SB_SETPARTS, StatusBar.Panels.Count, LPARAM(Rights));
  finally
    Freemem(Rights);
  end;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.ResizeChild
  Params:  Sender                   - the object which invoked this function
           Left, Top, Width ,Height - new dimensions for the control
  Returns: Nothing

  Resize a window
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.ResizeChild(Sender: TWinControl;
  Left, Top, Width, Height: Integer);

Var
{$IFDEF VerboseSizeMsg}
  OldLeft: Integer;
  OldTop: Integer;
{$ENDIF}
  WinHandle, BuddyHandle: HWND;
  StringList: TWin32ListStringList;
  suppressMove: boolean;
Begin
  // if not Sender.HandleAllocated then exit; --> Already checked (LM_SETSIZE and LM_RECREATEWND)
  {$IFDEF VerboseSizeMsg}
  OldLeft:=Left;
  OldTop:=Top;
  {$ENDIF}
  LCLBoundsToWin32Bounds(Sender, Left, Top, Width, Height);
  {$IFDEF VerboseSizeMsg}
  writeln('TWin32WidgetSet.ResizeChild A ',AWinControl.Name,':',AWinControl.ClassName,
    ' LCL=',OldLeft,',',OldTop,',',Width,',',Height,
    ' Win32=',Left,',',Top,',',Width,',',Height,
    '');
  {$ENDIF}
  WinHandle := Sender.Handle;
  suppressMove := false;
  case Sender.FCompStyle of
  csSpinEdit:
  begin
    // detach from buddy first
    BuddyHandle := Windows.SendMessage(WinHandle, UDM_SETBUDDY, 0, 0);
    MoveWindow(BuddyHandle, Left, Top, Width, Height, True);
    // reattach
    Windows.SendMessage(WinHandle, UDM_SETBUDDY, BuddyHandle, 0);
    suppressMove := true;
  end;
  
  csGroupBox:
  begin
    // check if we have a ``container'', if so, move that
    BuddyHandle := Windows.GetProp(WinHandle, 'ParentPanel');
    if BuddyHandle <> 0 then
    begin
      MoveWindow(BuddyHandle, Left, Top, Width, Height, false);
      Left := 0;
      Top := 0;
    end;
  end;
  
  else
    if Sender is TCustomComboBox then
    begin
      StringList := TWin32ListStringList(Windows.GetProp(WinHandle, 'List'));
      if StringList <> nil then
        Height := StringList.ComboHeight;
    end;
  end;
  
  if not suppressMove then
    MoveWindow(WinHandle, Left, Top, Width, Height, True);
  LCLControlSizeNeedsUpdate(Sender,False);
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AddChild
  Params:  Parent - Parent to which the child will be added
           Child  - Child to add
  Returns: Nothing

  Adds A Child to a Parent
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.AddChild(Parent, Child: HWND);
var
  ParentPanel: HWND;
Begin
  Assert(False, 'Trace:AddChild - Parent Window Handle is $' + IntToHex(LongInt(Parent), 8));
  Assert(False, 'Trace:AddChild - Child Window Handle is $' + IntToHex(LongInt(Child), 8));
  // handle groupbox exception
  ParentPanel := Windows.GetProp(Child, 'ParentPanel');
  if ParentPanel <> 0 then
    Child := ParentPanel;
  SetParent(Child, Parent);
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.SetText
  Params:  Child - Window to add the text
           Data  - The text to add
  Returns: Nothing

  Sets the text of a control.

  WARNING: This should possibly be merged with the SetLabel method!
           It's only left in here for TStatusBar right now cause it
           may be nice to use it with different panels.
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.SetText(Window: HWND; Data: Pointer);
Begin
  Case PLMSetControlText(Data)^.FCompStyle Of
    csStatusBar:
    Begin
      Windows.SendMessage(Window, SB_SETTEXT, Windows.WParam(PLMSetControlText(Data)^.Panel),
        Windows.LParam(LPSTR(PLMSetControlText(Data)^.UserData)));
    End
    Else
      AssertEx('STOPPOK: [TWin32WidgetSet.SetText] Possible superfluous use of SetText, use SetLabel instead!', False, 2);
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet SetColor
  Params: Sender - The sending object
  Returns: Nothing

  Changes the form's default background color
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.SetColor(Sender: TObject);
Begin

End;              

{------------------------------------------------------------------------------
  Function: TWin32WidgetSet.CreateComponent
  Params:   Sender - object for which to create visual representation
  Returns:  nothing

  Tells Windows to create a control
 ------------------------------------------------------------------------------}
Function TWin32WidgetSet.CreateComponent(Sender: TObject): THandle;
Var
  Buddy, Parent, Handle, Window: HWnd;
  CompStyle, Left, Top, Height, Width: Integer;
  AlternateCreateWindow: Boolean;
  MenuHandle: HMENU;
  Caption : String;
  Flags, FlagsEx: DWord;
  SubClassWndProc: Pointer;
  StrCaption: PChar;
  WindowTitle: PChar;
  pClassName: PChar;
  R: TRect;
  //TCI: TC_ITEM;
Const
  ComboBoxStyles: array[TComboBoxStyle] of DWORD = (
    CBS_DROPDOWN, CBS_SIMPLE, CBS_DROPDOWNLIST,
    CBS_DROPDOWNLIST or CBS_OWNERDRAWFIXED,
    CBS_DROPDOWNLIST or CBS_OWNERDRAWVARIABLE);
          
  LISTVIEWSTYLES: array[TViewStyle] of DWORD = (
    LVS_LIST, LVS_REPORT
  );
Begin
  Assert(False, 'Trace:CreateComponent - Start');
  Assert(False, 'Trace:CreateComponent - Value of Sender is $' + IntToHex(LongInt(Sender), 8));
  Assert(False, 'Trace:CreateComponent - 1');
   
  Result := 0;
   
  Flags := WS_CHILD or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
  FlagsEx := 0;
  Assert(False, 'Trace:Setting flags');
  Window := HWND(Nil);
  Buddy := HWND(Nil);
  Assert(False, 'Trace:Setting window');

  If (Sender Is TWinControl) And (TWinControl(Sender).Parent <> Nil) Then
  Begin
    Assert(False, Format('Trace:TWin32WidgetSet.CreateComponent - %S parent --> %S', [Sender.ClassName, TWinControl(Sender).Parent.ClassName]));
    Parent := TWinControl(Sender).Parent.Handle;
    Assert(False, Format('Trace:TWin32WidgetSet.CreateComponent - parent handle --> 0x%X', [Parent]));
    Assert(False, 'Trace:Setting parent');
  End
  Else
    Parent := FAppHandle;

  SubClassWndProc := @WindowProc;
  AlternateCreateWindow := false;
  CompStyle := csNone;
  WindowTitle := nil;
  Assert(False, 'Trace:Setting compstyle');
  //Caption := '';
  Assert(False, 'Trace:Setting caption');
  If (Sender Is TControl) Then
  Begin
    Caption := TControl(Sender).Caption;
    CompStyle := TControl(Sender).FCompStyle;
    Height := TControl(Sender).Height;
    Left := TControl(Sender).Left;
    //Parent := TControl(Sender).Parent;
    Top := TControl(Sender).Top;
    Width := TControl(Sender).Width;
    if TControl(Sender).Visible then
      Flags := Flags or WS_VISIBLE;
    if csAcceptsControls in TControl(Sender).ControlStyle then
      FlagsEx := FlagsEx or WS_EX_CONTROLPARENT;
    if TControl(Sender).TabStop then
      Flags := Flags or WS_TABSTOP;
    Assert(False, 'Trace:Setting dimentions');
    LCLBoundsToWin32Bounds(Sender, Left, Top, Width, Height);
    if Sender is TCustomControl then
      if TCustomControl(Sender).BorderStyle = bsSingle then
        FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
    {$IFDEF VerboseSizeMsg}
    writeln('TWin32WidgetSet.CreateComponent A ',TControl(Sender).Name,':',TControl(Sender).ClassName,' ',Left,',',Top,',',Width,',',Height);
    {$ENDIF}
  End
  Else If (Sender Is TMenuItem) Then
  Begin
    Assert(False, 'Trace:[TWin32WidgetSet.CreateComponent] - Sender is a menu item');
    Caption := TMenuItem(Sender).Caption;
    Assert(False, Format('Trace:[TWin32WidgetSet.CreateComponent] - Caption set to %S', [Caption]));
    CompStyle := TMenuItem(Sender).FCompStyle;
    Assert(False, Format('Trace:[TWin32WidgetSet.CreateComponent] - CompStyle set to %S', [CS_To_String(CompStyle)]));
  End
  Else If (Sender Is TMenu) { Or (Sender Is TPopupMenu) } Then
    CompStyle := TMenu(Sender).FCompStyle
  Else If (Sender Is TCommonDialog) Then
    CompStyle := TCommonDialog(Sender).FCompStyle;

  Assert(False, Format('Trace:TWin32WidgetSet.CreateComponent - Creating component %S with the caption of %S', [Sender.ClassName, Caption]));
  Assert(False, Format('Trace:TWin32WidgetSet.CreateComponent - Left: %D, Top: %D, Width: %D, Height: %D, Parent handle: 0x%X, instance handle: 0x%X', [Left, Top, Width, Height, Parent, HInstance]));

  // until here remove when debug not needed
  StrCaption := PChar(Caption);
  Assert(False, 'Trace:CreateComponent - Control Style is ' + CS_To_String(CompStyle));

  Case CompStyle Of
    // controls with a window handle
    csBitBtn:
    Begin
      pClassName := 'BUTTON';
      if TCustomBitBtn(Sender).Default Then
        Flags := Flags or BS_DEFPUSHBUTTON
      else
        Flags := Flags or BS_PUSHBUTTON;
      Flags := Flags or BS_BITMAP;
      WindowTitle := nil;
    End;
    csButton:
    Begin
      Assert(False, 'Trace:CreateComponent - Creating Button');
      if TCustomButton(Sender).Default Then
        Flags := Flags or BS_DEFPUSHBUTTON
      else
        Flags := Flags or BS_PUSHBUTTON;
      pClassName := 'BUTTON';
      WindowTitle := StrCaption;
    End;
    csCalendar:
    Begin
      pClassName := 'SysMonthCal32';
      WindowTitle := StrCaption;
      Flags := WS_CHILD or WS_VISIBLE;
      SubClassWndProc := nil;
    End;
    csCheckbox:
    Begin
      pClassName := 'BUTTON';
      WindowTitle := StrCaption;
      Flags := Flags Or BS_AUTOCHECKBOX;
    End;
    csComboBox:
    Begin
      Flags := Flags or ComboBoxStyles[TCustomComboBox(Sender).Style];
      If TComboBox(Sender).Sorted Then
        Flags:= Flags or CBS_SORT;
      pClassName := 'COMBOBOX';
      Flags := Flags or WS_VSCROLL or CBS_AUTOHSCROLL or CBS_HASSTRINGS;
      SubClassWndProc := @ComboBoxWindowProc;
    End;
    csListBox, csCheckListBox:
    Begin
      With TCustomListBox(Sender) do
      Begin
        If Sorted Then
          Flags:= Flags or LBS_SORT;
        If MultiSelect Then
          if ExtendedSelect then
            Flags:= Flags or LBS_EXTENDEDSEL
          else
            Flags:= Flags or LBS_MULTIPLESEL;
        if CompStyle = csCheckListBox then
          Flags := Flags or LBS_OWNERDRAWFIXED
        else case Style of
          lbOwnerDrawFixed: Flags := Flags or LBS_OWNERDRAWFIXED;
          lbOwnerDrawVariable: Flags := Flags or LBS_OWNERDRAWVARIABLE;
        end;
      end;
      FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
      pClassName := 'LISTBOX';
      Flags := Flags or (WS_VSCROLL or LBS_NOINTEGRALHEIGHT or LBS_HASSTRINGS);
    End;
    csCListBox:
    Begin
      With TCustomListBox(Sender) do
      Begin
        If Sorted Then
          Flags:= Flags or LBS_SORT;
        If MultiSelect Then
          if ExtendedSelect then
            Flags:= Flags or LBS_EXTENDEDSEL
          else
            Flags:= Flags or LBS_MULTIPLESEL;
      End;
      FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
      pClassName := 'LISTBOX';
      Flags := Flags or LBS_MULTICOLUMN or WS_HSCROLL;
    End;
    csEdit:
    Begin
      FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
      pClassName := 'EDIT';
      WindowTitle := StrCaption;
      Flags := Flags Or ES_AUTOHSCROLL;
    End;
    csArrow, csFixed, csToolButton:
    Begin
      Assert(False, 'Trace:TODO: Figure out what component style csFixed is and code the component.  No component created.');
      pClassName := @ClsName;
      WindowTitle := StrCaption;
      SubClassWndProc := nil;
    End;
    csForm:
    Begin
      Assert(False, 'Trace:CreateComponent - Creating a Form Window');
      Flags := BorderStyleToWin32Flags(TCustomForm(Sender).BorderStyle);
      FlagsEx := BorderStyleToWin32FlagsEx(TCustomForm(Sender).BorderStyle);
      if (TCustomForm(Sender).FormStyle in fsAllStayOnTop)
      and (not (csDesigning in TCustomForm(Sender).ComponentState)) then
        FlagsEx := FlagsEx or WS_EX_TOPMOST;
      pClassName := @ClsName;
      WindowTitle := StrCaption;
      Left := LongInt(CW_USEDEFAULT);
      Top := LongInt(CW_USEDEFAULT);
      Width := LongInt(CW_USEDEFAULT);
      Height := LongInt(CW_USEDEFAULT);
      SubClassWndProc := nil;
    End;
    csHintWindow:
    Begin
      pClassName := @ClsName;
      WindowTitle := StrCaption;
      Flags := WS_POPUP;
      FlagsEx := FlagsEx or WS_EX_TOOLWINDOW;
      Left := LongInt(CW_USEDEFAULT);
      Top := LongInt(CW_USEDEFAULT);
      Width := LongInt(CW_USEDEFAULT);
      Height := LongInt(CW_USEDEFAULT);
      SubClassWndProc := nil;
    End;
    {csFrame,} csGroupBox:
    Begin
      if FThemesActive and (Sender is TWinControl) and (TWinControl(Sender).Parent <> nil) and 
        (TWinControl(Sender).Parent.FCompStyle = csGroupBox) then
      begin
        // the parent of this groupbox is another groupbox: there is a bug in 
        // drawing the caption in that case, the caption of the child groupbox
        // is drawn in system font, make an intermediate "ParentPanel", then
        // the bug is hidden. Use 'ParentPanel' property of groupbox window
        // to determine reference to this parent panel
        // do not use 'ParentPanel' property for other controls!
        Parent := CreateWindowEx(0, @ClsName, nil, WS_CHILD or WS_CLIPSIBLINGS or (Flags and WS_VISIBLE), 
          Left, Top, Width, Height, Parent, 0, HInstance, nil);
        Buddy := Parent;
        Left := 0;
        Top := 0;
        Flags := Flags or WS_VISIBLE;
      end;
      pClassName := 'BUTTON';
      WindowTitle := StrCaption;
      Flags := Flags Or BS_GROUPBOX;
    End;
    csLabel:
    Begin
      pClassName := 'STATIC';
      WindowTitle := StrCaption;
      Flags := Flags Or SS_LEFT;
    End;
    csPairSplitter, csPairSplitterSide:
    begin
      pClassName := 'STATIC';
      WindowTitle := StrCaption;
    end;
    csListView:
    Begin
      pClassName := WC_LISTVIEW;
      WindowTitle := StrCaption;
      Flags := Flags or LISTVIEWSTYLES[TListView(Sender).ViewStyle] or LVS_SINGLESEL;
      FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;   
    End;
    csMemo:
    Begin
      Assert(False, 'Trace:TWin32WidgetSet.CreateComponent - Creating a MEMO...');
      Flags := Flags Or ES_AUTOVSCROLL Or ES_MULTILINE Or ES_WANTRETURN;
      If TCustomMemo(Sender).ReadOnly Then
        Flags := Flags Or ES_READONLY;
      If not TCustomMemo(Sender).WordWrap Then
        Flags := Flags Or ES_AUTOHSCROLL;
      Case TCustomMemo(Sender).ScrollBars Of
        ssHorizontal:
          Flags := Flags Or WS_HSCROLL;
        ssVertical:
          Flags := Flags Or WS_VSCROLL;
        ssBoth:
          Flags := Flags Or WS_HSCROLL Or WS_VSCROLL;
      End;
      FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
      pClassName := 'EDIT';
      WindowTitle := StrCaption;
    End;
    csNotebook:
    Begin
      pClassName := WC_TABCONTROL;
    End;
    csRadioButton:
    Begin
      pClassName := 'BUTTON';
      WindowTitle := StrCaption;
      // BS_AUTORADIOBUTTON may hang the application,
      // if the radiobuttons are not consecutive controls.
      Flags := Flags Or BS_RADIOBUTTON;
    End;
    csScrollBar:
    Begin
      Case TScrollBar(Sender).Kind Of
        sbHorizontal:
          Flags := Flags Or SBS_HORZ;
        sbVertical:
          Flags := Flags Or SBS_VERT;
      End;
      pClassName := 'SCROLLBAR';
    End;
    csScrollBox:
    Begin
      //Todo: Make control respond to user scroll request
      FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
      pClassName := @ClsName;
      Flags := Flags or WS_HSCROLL or WS_VSCROLL;
      SubClassWndProc := nil;
    End;
    csScrolledWindow:
    Begin
      Assert(False, 'TRACE: CreateComponent - creating a scrolled window');
      pClassName := @ClsName;
      WindowTitle := StrCaption;
      Flags := WS_OVERLAPPEDWINDOW or WS_HSCROLL or WS_VSCROLL or WS_VISIBLE;
      Left := LongInt(CW_USEDEFAULT);
      Top := LongInt(CW_USEDEFAULT);
      Width := LongInt(CW_USEDEFAULT);
      Height := LongInt(CW_USEDEFAULT);
      SubClassWndProc := nil;
    End;
    csStatusBar:
    Begin
      Assert(False, 'TRACE:CreateComponent - Creating Status Bar');
      pClassName := STATUSCLASSNAME;
      WindowTitle := StrCaption;
      Left := LongInt(CW_USEDEFAULT);
      Top := LongInt(CW_USEDEFAULT);
      Width := LongInt(CW_USEDEFAULT);
      Height := LongInt(CW_USEDEFAULT);
    End; 
(*    
    csGTKTable:
    Begin
      // Commented out because of error in 1.0.5 (bug?)
      //Assert(False, 'Trace:TODO: Create GTK Table.  I''m not sure how to do this (or even if this is needed in Win32), but I assume an array (or TList) of records containing the rows and columns and the properties(x, y, width, height, etc) of everything.  If you think you can help, be my guest.');
      MessageBox(GetDesktopWindow, 'csGTKTable expected to be created', Nil, MB_OK);
      //TControl(Sender).FCompStyle := csForm;
      //IntSendMessage3(LM_CREATE, Sender, Nil);
      Assert(False, 'TRACE:GTK Table not created');
    End;
*)    
    csToggleBox:
    Begin
      Assert(False, 'TRACE: CreateComponent - Creating toggle box');
      pClassName := 'BUTTON';
      WindowTitle := StrCaption;
      Flags := Flags Or BS_AUTOCHECKBOX Or BS_PUSHLIKE;
    End;
    csToolBar:
    Begin
      pClassName := TOOLBARCLASSNAME;
      Flags := Flags OR CCS_ADJUSTABLE;
    End;
    // TCustomPage - Notebook page
    csPage:
    Begin
      Assert(False, 'Trace:Create a csPage component.');
      pClassName := @ClsName;
      Flags := Flags and DWORD(not WS_VISIBLE);
      SubClassWndProc := nil;
    End;
    csPanel:
    Begin
      Assert(False, 'Trace:Create a csPanel component.');
      pClassName := @ClsName;
      SubClassWndProc := nil;
    End;
    csProgressBar:
    Begin
      with TProgressBar(Sender) do
      begin
        if Smooth then
          Flags := Flags or PBS_SMOOTH;
        if (Orientation = pbVertical) or (Orientation = pbTopDown) then
          Flags := Flags or PBS_VERTICAL;
      end;
      pClassName := PROGRESS_CLASS;
    End;
    csTrackBar:
    Begin
      Assert(False, 'TRACE:CreateComponent - Creating a Track Bar (if we''re lucky)');
      pClassName := TRACKBAR_CLASS;
      WindowTitle := StrCaption;
    End;
  else
    AlternateCreateWindow := true;
    case CompStyle of
      // these controls create no window handle using CreateWindowEx
      csAlignment:
      Begin
        Assert(False, 'Trace:TODO: Code csAlignment.  If anyone knows how to do this, please do.');
        Handle:=TWinControl(Sender).Handle;
        GetClientRect(Handle, R);
        MoveWindow(Handle, R.Right - Left, R.Bottom - Top, (R.Right - R.Left) - (Left Div 2), (R.Bottom - R.Top) - (Top Div 2), True);
        Window := Handle;
      End;
      csFileDialog, csOpenFileDialog, csSaveFileDialog,
      csColorDialog, csFontDialog:
      Begin
        CreateCommonDialog(TCommonDialog(Sender),CompStyle);
      End;
      csSelectDirectoryDialog:
        CreateSelectDirectoryDialog(TSelectDirectoryDialog(Sender));
(*
      csFont:
      Begin
        Assert(False, 'Trace:CreateComponent - Creating a font');
        With LPLogFont(@Sender)^ Do
          Window := CreateFont(LFHeight, LFWidth, LFEscapement, LFOrientation, LFWeight, LFItalic, LFUnderLine, LFStrikeOut, LFCharSet, LFOutPrecision, LFClipPrecision, LFQuality, LFPitchAndFamily, LFFaceName);
      End;
*)      
      csImage:
      Begin
        // nothing to do
      End;
      csMainMenu, csMenuBar:
      begin
        Window := CreateMenu;
      end;
      csMenuItem, csPopupMenu:
      begin
        Window := CreatePopupMenu;
      end;
      csSpinEdit:
      Begin
        //this needs to be created in the actual code because it requires a gtkadjustment Win32Control
        Buddy := CreateWindowEx(WS_EX_CLIENTEDGE, 'EDIT', StrCaption, Flags Or ES_AUTOHSCROLL, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
        Window := CreateUpDownControl(Flags or DWORD(WS_BORDER or UDS_ALIGNRIGHT or UDS_NOTHOUSANDS or UDS_ARROWKEYS or UDS_WRAP or UDS_SETBUDDYINT),
          0, 0,       // pos -  ignored for buddy
          0, 0,       // size - ignored for buddy
          Parent, 0, HInstance, Buddy,
          Trunc(TSpinEdit(Sender).MaxValue),
          Trunc(TSpinEdit(Sender).MinValue),
          Trunc(TSpinEdit(Sender).Value));
      End;
    end;
  End; {Case}

  if not AlternateCreateWindow then
  begin
    if (Flags and WS_CHILD) <> 0 then
    begin
      // menu handle is also for specifying a control id if this is a child
      MenuHandle := HMENU(Sender);
    end else begin
      MenuHandle := HMENU(nil);
    end;
    Window := CreateWindowEx(FlagsEx, pClassName, WindowTitle, Flags,
        Left, Top, Width, Height, Parent, MenuHandle, HInstance, Nil);  
    if Window = 0 then
    begin
      raise exception.create('failed to create win32 control, error: '+IntToStr(GetLastError()));
    end;
    { after creating a child window the following happens:
      1) the previously bottom window is thrown to the top
      2) the created window is added at the bottom
      undo this by throwing them both to the bottom again }
    Windows.SetWindowPos(Windows.GetTopWindow(Parent), HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    Windows.SetWindowPos(Window, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  end;
  
  Result := Window;      
  

  If (Sender Is TWinControl) Or (CompStyle = csImage) Then
  Begin
    TWinControl(Sender).Handle := Window;
    If Window <> HWND(Nil) Then
    begin
      Windows.SetProp(Window, 'Wincontrol', dword(Sender));
      if SubClassWndProc <> nil then
        Windows.SetProp(Window, 'DefWndProc', Windows.SetWindowLong(Window, GWL_WNDPROC, LongInt(SubClassWndProc)));
      // subclass edit control of combobox too
      case CompStyle of
      csCombobox:
      begin
        Buddy := Windows.GetTopWindow(Window);
        Windows.SetProp(Buddy, 'ComboEdit', 1);
        SubClassWndProc := @ChildEditWindowProc;
      end;

      csGroupBox:
      begin
        if Buddy <> 0 then
        begin
          Windows.SetProp(Window, 'ParentPanel', Parent);
          // no need to subclass this parentpanel
          Buddy := 0;
        end;
      end;

      csSpinEdit:
      begin
        SubClassWndProc := @ChildEditWindowProc;
      end;

      end;
      Windows.SendMessage(Window, WM_SETFONT, WParam(FMessageFont), 0);
    end;
    If Buddy <> HWND(Nil) Then
    begin
      Windows.SetProp(Buddy, 'AWincontrol', dword(Sender));
      Windows.SetProp(Buddy, 'DefWndProc', Windows.SetWindowLong(Buddy, GWL_WNDPROC, LongInt(SubClassWndProc)));
      Windows.SendMessage(Buddy, WM_SETFONT, WParam(FMessageFont), 0);
    end;
  End
  Else If (Sender Is TMenuItem) Then
    TMenuItem(Sender).Handle := Window
  Else If (Sender Is TMenu) Then
    TMenu(Sender).Items.Handle := Window
  Else If (Sender Is TCommonDialog) Then
    TCommonDialog(Sender).Handle := Window
  Else
  Begin
    If (Sender Is TControl) Then
    Begin
      Assert(False, 'Trace:CreateComponent - Assigning window to TControl');
      //(Sender As TControl).Handle := Window;
    End
    Else
      If (Sender Is TControlCanvas) Then
      Begin
        Assert(False, 'Trace:CreateComponent - Assigning window to TControlCanvas');
        TControlCanvas(Sender).Handle := Window;
      End
      Else If (Sender Is TFont) Then
      Begin
        Assert(False, 'Trace:CreateComponent - Assigning P to TFont');
        TFont(Sender).Handle := Window;
      End;
  End;

  case CompStyle of
    csCListBox:
    begin
      Windows.SendMessage(Window, LB_SETCOLUMNWIDTH, Windows.WPARAM(TCListBox(Sender).Width Div (TCListBox(Sender).ListColumns)), 0);
    end;
    {csFrame,} csGroupBox:
      TWinControl(Sender).InvalidateClientRectCache(true);
    csListView:
      SetOwner(Window, Sender);
    csPage:
      ShowWindow(Window, SW_HIDE);
    csStatusBar:
      StatusBarUpdate(Sender);
  end;

  Assert(False, 'Trace:Leaving CreateComponent');
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AssignSelf
  Params: Window - The window to assign
          Data   - The data to assign to the window
  Returns: Nothing

  Assigns data to a window
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AssignSelf(Window: HWnd; Data: Pointer);
begin
  Assert(False, 'Trace:[TWin32WidgetSet.AssignSelf] Trying to code it.  It''s probably wrong.');
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.ShowHide
  Params: Sender - The sending object
  Returns: Nothing

  Shows or hides a control
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.ShowHide(Sender: TObject);
Var
  Handle: HWND;
  ParentPanel: HWND;
Begin
  If (TControl(Sender).FCompStyle = csPage) or (TControl(Sender).FCompStyle = csToolButton) then exit;
  Handle := ObjectToHWND(Sender);
  ParentPanel := Windows.GetProp(Handle, 'ParentPanel');
  if ParentPanel <> 0 then
    Handle := ParentPanel;
  If TControl(Sender).HandleObjectShouldBeVisible Then
  Begin
    Assert(False, 'Trace: [TWin32WidgetSet.ShowHide] Showing the window');
    if TControl(Sender).FCompStyle = csHintWindow then
    begin
      Windows.SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
    end else begin
      Windows.ShowWindow(Handle, SW_SHOW);
    end;
    If (Sender Is TCustomForm) Then
      SetClassLong(Handle, GCL_HICON, LONG(TCustomForm(Sender).GetIconHandle));
  End
  Else
  Begin
    Assert(False, 'TRACE: [TWin32WidgetSet.ShowHide] Hiding the window');
    If Sender Is TCustomForm then
      If fsModal in TCustomForm(Sender).FormState then
        EnumThreadWindows(GetWindowThreadProcessId(Handle,nil),@EnableWindowsProc, Handle);
    ShowWindow(Handle, SW_HIDE);
  End;
End;

{ -----------------------------------------------------------------------------
  Method: TWin32WidgetSet.AddAllNBPages
  Params: Notebook - A notebook control
  Returns: Nothing

  Adds all pages to notebook (showtabs becomes true)
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AddAllNBPages(Notebook: TCustomNotebook);
var
  TCI: TC_ITEM;
  I, Res: Integer;
  lPage: TCustomPage;
begin
  for I := 0 to Notebook.PageCount - 1 do
  begin
    lPage := Notebook.Page[I];
    // check if already shown
    TCI.Mask := TCIF_PARAM;
    Res := Windows.SendMessage(Notebook.Handle, TCM_GETITEM, I, LPARAM(@TCI));
    if (Res = 0) or (dword(TCI.lParam) <> lPage.Handle) then
    begin
      TCI.Mask := TCIF_TEXT or TCIF_PARAM;
      TCI.pszText := PChar(lPage.Caption);
      TCI.lParam := lPage.Handle;
      Windows.SendMessage(Notebook.Handle, TCM_INSERTITEM, I, LPARAM(@TCI));
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.RemoveAllNBPages
  Params: Notebook - The notebook control
  Returns: Nothing

  Removes all pages from a notebook control (showtabs becomes false)
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.RemoveAllNBPages(Notebook: TCustomNotebook);
var
  I: Integer;
  R: TRect;
begin
  for I := Notebook.PageCount - 1 downto 0 do
    RemoveNBPage(Notebook, I);
  // Adjust page size to fit in tabcontrol, need bounds of notebook in client of parent
  Self.GetClientRect(Notebook.Handle, R);
  R.Right := R.Right - R.Left;
  R.Bottom := R.Bottom - R.Top;
  for I := 0 to Notebook.PageCount - 1 do
    ResizeChild(Notebook.Page[I], R.Left, R.Top, R.Right, R.Bottom);
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.ChangeActivePage
  Params: Data - The notebook setitemindex message
  Returns: Nothing

  Changes the active page to one specified in data
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.ChangeActivePage(const Data: TLMNotebookEvent);
var
  OldPageIndex: Integer;
  PageHandle: HWND;
  Handle: HWND;
begin
  with Data do
  begin
    Handle := TWinControl(Parent).Handle;
    OldPageIndex := SendMessage(Handle, TCM_GETCURSEL, 0, 0);
    Windows.SendMessage(Handle, TCM_SETCURSEL, Windows.WPARAM(Page), 0);
    if not (csDestroying in (Parent as TCustomNotebook).ComponentState) then
    begin
      // create handle if not already done, need to show!
      if (Page>=0) and (Page < TCustomNotebook(Parent).PageCount) then
      begin
        PageHandle := TCustomNotebook(Parent).CustomPage(Page).Handle;
        SetWindowPos(PageHandle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW);
      end;
      if (OldPageIndex>=0) and (OldPageIndex<>Page)
      and (OldPageIndex < TCustomNotebook(Parent).PageList.Count)
      and (TCustomNotebook(Parent).CustomPage(OldPageIndex).HandleAllocated)
        then ShowWindow(TCustomNotebook(Parent).CustomPage(OldPageIndex).Handle, SW_HIDE);
    end;
  end;
end;

{ -----------------------------------------------------------------------------
  Method: TWin32WidgetSet.AddNBPage
  Params: Notebook - A notebook control
          NewPage - Page to insert
          Index  - The position in the notebook to insert the page
  Returns: Nothing

  Adds a new page to a notebook
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.AddNBPage(Notebook: TCustomNotebook; NewPage: TCustomPage; Index: Integer);
Var
  TCI: TC_ITEM;
  OldPageIndex: integer;
  R: TRect;
Begin
  Assert(False, 'Trace:TWin32WidgetSet.AddNBPage - Start');
  With Notebook do
  Begin
    TCI.Mask := TCIF_TEXT or TCIF_PARAM;
    TCI.pszText := PChar(NewPage.Caption);
    // store handle as extra data, so we can verify we got the right page later
    TCI.lParam := NewPage.Handle;
    Windows.SendMessage(Handle, TCM_INSERTITEM, Index, LPARAM(@TCI));
    // Adjust page size to fit in tabcontrol, need bounds of notebook in client of parent
    Self.GetClientRect(Handle, R);
    IntSendMessage3(LM_SETSIZE, NewPage, @R);
    // Do the page switch. The are no tabcontrol notifications so we have to
    // do the hiding and showing ourselves.
    OldPageIndex := SendMessage(Handle,TCM_GETCURSEL,0,0);
    SendMessage(Handle,TCM_SETCURSEL,NewPage.PageIndex,0);
    ShowWindow(NewPage.Handle, SW_SHOW);
    if (OldPageIndex>=0) and (OldPageIndex<>NewPage.PageIndex)
      then ShowWindow(Page[OldPageIndex].Handle, SW_HIDE);
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.RemoveNBPage
  Params: Parent - The notebook control
          Index  - The page to delete
  Returns: Nothing

  Removes a page from a notebook control
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.RemoveNBPage(Notebook: TCustomNotebook; Index: Integer);
Begin
  Assert(false, 'Trace:Removing a notebook page');
  Windows.SendMessage(Notebook.Handle, TCM_DELETEITEM, Windows.WPARAM(Index), 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.ReDraw
  Params: Child - Component to redraw
  Returns: Nothing

  Redraws a component
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.ReDraw(Child: TObject);
Begin
  Assert(False, 'TRACE:[TWin32WidgetSet.ReDraw] Redrawing...');
  Assert(False, 'TRACE:Invalidating the window');
  IntSendMessage3(LM_INVALIDATE, Child, Nil);
  Assert(False, 'TRACE:Updating the window');
  UpdateWindow(TWinControl(Child).Handle);
  Assert(False, 'TRACE:[TWin32WidgetSet.ReDraw] Finished redrawing');
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.SetPixel
  Params:  Sender - the lcl object which called this func via SendMessage
           Data   - pointer to a TLMSetGetPixel record
  Returns: nothing

  Set the color of the specified pixel on the window?screen?object?
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.SetPixel(Sender: TObject; Data: Pointer);
Var
  DC: HDC;
  Handle: HWnd;
Begin
  Assert(False, 'Trace:TODO: Implement TWin32WidgetSet.SetPixel');
  Handle :=(Sender As TWinControl).Handle;
  DC := GetDC(Handle);
  With TLMSetGetPixel(Data^) Do
    Windows.SetPixel(DC, X, Y, PixColor);
  ReleaseDC(Handle, DC);
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.GetPixel
  Params:  Sender - the lcl object which called this func via SenMessage
           Data   - pointer to a TLMSetGetPixel record
  Returns: nothing

  Get the color of the specified pixel on the window?screen?object?
 -----------------------------------------------------------------------------}
Procedure TWin32WidgetSet.GetPixel(Sender: TObject; Data: Pointer);
Var
  DC: HDC;
  Handle: HWnd;
Begin
  Handle := (Sender As TWinControl).Handle;
  DC := GetDC(Handle);
  With TLMSetGetPixel(Data^) Do
    PixColor := Windows.GetPixel(DC, X, Y);
  ReleaseDC(Handle, DC);
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.GetValue
  Params:  Sender - the lcl object which called this func via SenMessage
           Data   - pointer to component specific variable
  Returns: currently always 0

  Depending on the compStyle, this function will get the current value
  of a Window and save it in the variable referenced by 'Data'.

  This function should be used to synchronize the state of an lcl-object
  with the corresponding Windows object.
 ------------------------------------------------------------------------------}
Function TWin32WidgetSet.GetValue(Sender: TObject; Data: Pointer): Integer;
Var
  Handle: HWnd;
  ST: SystemTime;
Begin
  Result := 0;     // default if nobody sets it

  If Sender Is TWinControl Then
    Assert(False, Format('Trace:[TWin32WidgetSet.GetValue] %S', [Sender.ClassName]))
  Else
    Assert(False, Format('Trace:WARNING: [TWin32WidgetSet.GetValue] %S --> No Decendant of TWinControl', [Sender.ClassName]));

  Handle := TWinControl(Sender).Handle;
  Assert (Handle = 0, 'Trace:WARNING: TWin32WidgetSet.GetValue --> got no window');

  Case TControl(Sender).FCompStyle Of
    csTrackbar:
      If (Handle <> HWnd(Nil)) Then
      Begin
        LResult(Data^) := SendMessage(Handle, TBM_GETRANGEMAX, 0, 0) - SendMessage(Handle, TBM_GETRANGEMIN, 0, 0);
      End
      Else
        LResult(Data^) := 0;
    csRadiobutton, csCheckbox:
      case SendMessage(Handle, BM_GETCHECK, 0, 0) of
        BST_CHECKED: TCheckBoxState(Data^) := cbChecked;
        BST_INDETERMINATE: TCheckBoxState(Data^) := cbGrayed;
        BST_UNCHECKED: TCheckBoxState(Data^) := cbUnChecked;
      end;
    csCalendar:
    Begin
      SendMessage(Handle,MCM_GETCURSEL, 0, Integer(@ST));
      With ST Do
        TLMCalendar(Data^).Date := EncodeDate(WYear,WMonth,WDay);
    End;
    csSpinEdit:
    Begin
      Single(Data^) := SendMessage(Handle, UDM_GETPOS, 0, 0);
    End;
    Else
      Assert (True, Format('WARNING:[TWin32WidgetSet.GetValue]] failed for %S', [Sender.ClassName]));
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.SetValue
  Params:  Sender - the lcl object which called this func via SendMessage
           Data   - pointer to component specific variable
  Returns: currently always 0

  Depending on the CompStyle, this function will apply the parameter 'data'
  to the Windows object repesenting the lcl-object which called the function.

  This function should be used in cases where the most common property
  of an object has changed (e.g. the position of a trackbar). If more than
  one property changed use the SetProperties function instead;
 ------------------------------------------------------------------------------}
Function TWin32WidgetSet.SetValue(Sender: TObject; Data: Pointer): Integer;
Var
  Handle: HWnd;
  ST: SystemTime;
  
Begin
  Result := 0;     // default if nobody sets it

  If Sender Is TWinControl Then
    Assert(False, Format('Trace:[TWin32WidgetSet.SetValue] %S', [Sender.ClassName]))
  Else
    Assert(False, Format('Trace:WARNING:[TWin32WidgetSet.SetValue] %S --> No Decendant of TWinControl', [Sender.ClassName]));

  Handle := TWinControl(Sender).Handle;
  If Handle = HWnd(Nil) Then
    Assert (False, 'Trace:WARNING:[TWin32WidgetSet.SetValue] --> got no window');

  Case TControl(Sender).FCompStyle Of
    csArrow:
    Begin
      // TODO: Add code to implement arrow-widget handling
    End;
    csCalendar:
    Begin
      With ST Do
        DecodeDate(TLMCalendar(Data^).Date,WYear,WMonth,WDay);
      SendMessage(Handle,MCM_SETCURSEL, 0, Integer(@ST));
    End;
    csProgressBar:
      Windows.SendMessage(Handle, PBM_SETPOS, Windows.WPARAM(Data^), 0);
    csTrackbar:
    Begin
      If Handle = HWnd(Nil) Then
        Exit;
      Assert(False, 'TRACE:Setting the track bar value.');
      Windows.SendMessage(Handle, TBM_SETPOS, Windows.WPARAM(True), Windows.LPARAM(Data^));
    End;
    csRadioButton, csCheckbox:
    Begin
      If TCheckBoxState(Data^) = cbChecked Then
        Windows.SendMessage(Handle, BM_SETCHECK, Windows.WParam(BST_CHECKED), 0)
      Else If TCheckboxState(Data^) = cbUnchecked Then
        Windows.SendMessage(Handle, BM_SETCHECK, Windows.WParam(BST_UNCHECKED), 0)
      Else
        Windows.SendMessage(Handle, BM_SETCHECK, Windows.WParam(BST_INDETERMINATE), 0);
    End
    Else
      Assert (True, Format('Trace:WARNING: [TWin32WidgetSet.SetValue] failed for %S', [Sender.ClassName]));
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.SetProperties
  Params:  Sender - the lcl object which called this func via SenMessage
  Returns: currently always 0

  Depending on the compStyle, this function will apply all properties of
  the calling object to the corresponding Window.
 ------------------------------------------------------------------------------}
Function TWin32WidgetSet.SetProperties(Sender: TObject): Integer;
const
  LVS_TYPEMASK = LVS_LIST or LVS_REPORT or LVS_SMALLICON or LVS_ICON;
  LISTVIEWSTYLES: array[TViewStyle] of DWORD = (
    LVS_LIST, LVS_REPORT);
  EDITSTYLES: array[TEditCharCase] of integer = (
    0, ES_UPPERCASE, ES_LOWERCASE);
  EDIT_STYLEMASK = ES_UPPERCASE or ES_LOWERCASE;
Var
  Handle: HWND;
  I, Count: Integer;
  LVC: LV_COLUMN;
  Style: dword;
  H: THandle;
begin
  Result := 0; // default if nobody sets it

  If Sender Is TWinControl Then
    Assert(False, Format('Trace:[TWin32WidgetSet.SetProperties] %S', [Sender.ClassName]))
  Else
    Assert(False, Format('Trace:WARNING: [TWin32WidgetSet.SetProperties] %S --> No Decendant of TWinControl', [Sender.ClassName]));

  Handle := TWinControl(Sender).Handle;
  If Handle = HWND(Nil) Then
    Assert (False, 'Trace:WARNING: [TWin32WidgetSet.SetProperties] --> got nil pointer');

  Case TControl(Sender).FCompStyle Of

  csEdit:
    With (TCustomEdit(Sender)) Do
    Begin
      Windows.SendMessage(Handle, EM_SETREADONLY, Windows.WPARAM(ReadOnly), 0);
      Windows.SendMessage(Handle, EM_LIMITTEXT, Windows.WPARAM(MaxLength), 0);
      UpdateWindowStyle(Handle, EDITSTYLES[CharCase], EDIT_STYLEMASK);
    End;

  csListView:
    With TListView(Sender) Do
    Begin
      Style := dword(GetWindowLong(Handle, GWL_STYLE));
      if (Style and LVS_TYPEMASK) <> LISTVIEWSTYLES[ViewStyle]
      then begin
        Style := Style and not LVS_TYPEMASK or LISTVIEWSTYLES[ViewStyle];
        SetWindowLong(Handle, GWL_STYLE, Style);
      end;
      
      if ViewStyle = vsReport Then
      Begin 
//        H := ListView_GetHeader(Handle);
        H := SendMessage(Handle, $1000 + 31 {LVM_GETHEADER}, 0, 0);
        Count := Header_GetItemCount(H);
        
        For I := 0 To Columns.Count - 1 Do
        Begin
          With LVC Do
          Begin
            Mask := LVCF_FMT Or LVCF_TEXT Or LVCF_WIDTH;
            Fmt := Integer(Columns.Items[I].Alignment);
            CX := Columns.Items[I].Width;
            PSzText := PChar(Columns.Items[I].Caption);
          End;
          if i >= Count 
          then ListView_InsertColumn(Handle, i, lvc)
          else ListView_SetColumn(Handle, I, LVC);
        End;
        for i := Columns.Count to Count - 1 do
          ListView_DeleteColumn(Handle, i);  
        Count := Header_GetItemCount(H);
      End;
      //If Sorted Then
        //ListView_SortItems(Handle, @CompareFunc, 0);
      If MultiSelect Then
        SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) And Not LVS_SINGLESEL);
      If SmallImages <> Nil Then
        ListView_SetImageList(Handle, SmallImages.Handle, LVSIL_NORMAL);
    End;

  csProgressBar:
    With TProgressBar(Sender) Do
    Begin
      { smooth and vertical need window recreation }
      if ((GetWindowLong(Handle, GWL_STYLE) and PBS_SMOOTH  ) <>
           Integer(Smooth) * PBS_SMOOTH) or
         ((GetWindowLong(Handle, GWL_STYLE) and PBS_VERTICAL) <>
           Integer((Orientation = pbVertical) or (Orientation = pbTopDown)) * PBS_VERTICAL) then
        Self.RecreateWnd(TWinControl(Sender));

      SendMessage(Handle, PBM_SETRANGE, 0, MakeLParam(Min, Max));
      SendMessage(Handle, PBM_SETPOS, Position, 0);

{ TODO: Implementable?
      If BarShowText Then
      Begin
        SetWindowText(Handle, StrToPChar((Sender As TControl).Caption));
      End
      Else
        SetWindowText(Handle, Nil);
}
    End;

  csScrollBar:
    With (TScrollBar(Sender)) Do
    Begin
      SendMessage(Handle, SBM_SETRANGE, Min, Max);
      SendMessage(Handle, SBM_SETPOS, Position, LPARAM(True));
      Case Kind Of
        sbHorizontal:
          SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) Or SBS_HORZ);
        sbVertical:
          SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) Or SBS_VERT);
      End;
      Assert(False, 'Trace:TODO: [TWin32WidgetSet.SetProperties] Set up step and page increments for csScrollBar');
    End;

  csSpinEdit:
    Begin
      SendMessage(Handle, UDM_SETRANGE, 0, MakeLong(Trunc(TSpinEdit(Sender).MaxValue), Trunc(TSpinEdit(Sender).MinValue)));
      SendMessage(Handle, UDM_SETPOS, 0, MakeLong(Trunc(TSpinEdit(Sender).Value), 0));
    End;

  csTrackbar:
    With(TCustomTrackBar(Sender)) Do
    Begin
      Windows.SendMessage(Handle, TBM_SETRANGEMAX, Windows.WPARAM(True), Max);
      Windows.SendMessage(Handle, TBM_SETRANGEMIN, Windows.WPARAM(True), Min);
      Windows.SendMessage(Handle, TBM_SETPOS, Windows.WPARAM(True), Position);
      Windows.SendMessage(Handle, TBM_SETLINESIZE, 0, LineSize);
      Windows.SendMessage(Handle, TBM_SETPAGESIZE, 0, PageSize);
      Case Orientation Of
        trVertical:
          SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) Or TBS_VERT);
        trHorizontal:
          SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) Or TBS_HORZ);
      End;
      If ShowScale Then
      Begin
        Case ScalePos of
          trLeft:
            SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) Or TBS_LEFT Or TBS_VERT);
          trRight:
            SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) Or TBS_RIGHT Or TBS_VERT);
          trTop:
            SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) Or TBS_TOP Or TBS_HORZ);
          trBottom:
            SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) Or TBS_BOTTOM Or TBS_HORZ);
        End;
      End;
      //Not here (Delphi compatibility)
    End;

  csLabel:
    With TLabel(Sender) Do
    Begin
      if Wordwrap then
      begin
        Style := SS_LEFTNOWORDWRAP;
      end else begin
        case Alignment of
          taLeftJustify:
            Style := SS_LEFT;
          taCenter:
            Style := SS_CENTER;
          taRightJustify:
            Style := SS_RIGHT;
        else
          Style := SS_LEFT; // default, shouldn't happen
        end;
      end;
      case Layout of
        tlTop:
          Style := Style or BS_TOP;
        tlCenter:
          Style := Style or BS_VCENTER;
        tlBottom:
          Style := Style or BS_BOTTOM;
      else
        Style := Style or BS_BOTTOM; //default, shouldn't happen
      end;
      SetWindowLong(Handle, GWL_STYLE, (GetWindowLong(Handle, GWL_STYLE) and $FFFF0000) or Style);
      Assert(False, 'TRACE:Wordwrapping of labels is not currently implemented');
      Assert(False, 'Trace:TODO: Code wordwrapping labels');
    End;
  Else
    Assert (True, Format('WARNING: [TWin32WidgetSet.SetProperties] failed for %S', [Sender.ClassName]));
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AttachMenu
  Params:  Sender - the lcl object which called this func
  Returns: nothing

  Attaches the calling Menu to its Parent
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.AttachMenu(Sender: TObject);
var MenuInfo: MENUITEMINFO;
    ParentMenuHandle: HMenu;
    ParentOfParent: HMenu;
    Msg: TLMShortCut;
    AMenuItem: TMenuItem;

  function GetCheckBitmap(checked: boolean): HBitmap;
  {TODO: create "checked" icon}
  var hbmpCheck, hbmpTrans, hbmpMask: HBITMAP;
      hbrTrans: HBRUSH;
      OldCheckMark, OldOrigBitmap, OldTransBitmap: HBITMAP;
      hdcNewBitmap, hdcOrigBitmap, hdcTransBitmap: HDC;
      hdcScreen: HDC;
      maxWidth, newWidth, bmpWidth: integer;
      maxHeight, newHeight, bmpHeight: integer;
  begin
    maxWidth:=GetSystemMetrics(SM_CXMENUCHECK);
    maxHeight:=GetSystemMetrics(SM_CYMENUCHECK);
    if (maxWidth>=AMenuItem.Bitmap.Width) and (maxHeight>=AMenuItem.Bitmap.Height) then Result:=AMenuItem.Bitmap.Handle
    else
    begin
      bmpWidth := AMenuItem.Bitmap.Width;
      bmpHeight := AMenuItem.Bitmap.Height;
      newWidth := min(maxWidth, bmpWidth);
      newHeight := min(maxHeight, bmpHeight);
      hdcScreen := GetDC(GetDesktopWindow);
      hdcOrigBitmap  := CreateCompatibleDC(hdcScreen);
      hdcNewBitmap   := CreateCompatibleDC(hdcScreen);
      hdcTransBitmap := CreateCompatibleDC(hdcScreen);
      hbmpCheck := CreateCompatibleBitmap(hdcScreen, newWidth, newHeight);
      hbmpTrans := CreateCompatibleBitmap(hdcScreen, bmpWidth, bmpHeight);
      hbmpMask  := AMenuItem.Bitmap.MaskHandle;
      ReleaseDC(GetDesktopWindow, hdcScreen);
      hbrTrans := CreateSolidBrush(GetSysColor(COLOR_MENU));
      OldOrigBitmap  := SelectObject(hdcOrigBitmap, AMenuItem.Bitmap.Handle);
      OldCheckmark   := SelectObject(hdcNewBitmap, hbmpCheck);
      OldTransBitmap := SelectObject(hdcTransBitmap, hbmpTrans);
      // fill transparent-bitmap with transparent color
      FillRect(hdcTransBitmap, RECT(0, 0, bmpWidth, bmpHeight), hbrTrans);
      // blit menu icon transparently
      MaskBlt(hdcTransBitmap, 0, 0, bmpWidth, bmpHeight, hdcOrigBitmap, 0, 0, hbmpMask, 0, 0);
      // scale to correct size
      StretchBlt(hdcNewBitmap, 0, 0, newWidth, newHeight, hdcTransBitmap, 0, 0, bmpWidth, bmpHeight, SRCCOPY);
      // free mem
      SelectObject(hdcOrigBitmap, OldOrigBitmap);
      SelectObject(hdcTransBitmap, OldTransBitmap);
      SelectObject(hdcNewBitmap, OldCheckmark);
      DeleteDC(hdcOrigBitmap);
      DeleteDC(hdcTransBitmap);
      DeleteDC(hdcNewBitmap);
      DeleteObject(hbmpTrans);
      DeleteObject(hbrTrans);
      {TODO: Add hbmpCheck into a list of object they must be deleted}
      Result := hbmpCheck;
    end;
  end;

Begin
  AMenuItem:=TMenuItem(Sender);
  ParentMenuHandle := AMenuItem.Parent.Handle;

  {Following part fixes the case when an item is added in runtime
  but the parent item has not defined the submenu flag (hSubmenu=0) }
  if AMenuItem.Parent.Parent<>nil then
  begin
    ParentOfParent := AMenuItem.Parent.Parent.Handle;
    with MenuInfo do begin
      cbSize:=sizeof(MENUITEMINFO);
      fMask:=MIIM_SUBMENU;
    end;
    GetMenuItemInfo(ParentOfParent, AMenuItem.Parent.Command,
                    false, @MenuInfo);
    if MenuInfo.hSubmenu=0 then // the parent menu item is not yet defined with submenu flag
    begin
      MenuInfo.hSubmenu:=ParentMenuHandle;
      SetMenuItemInfo(ParentOfParent, AMenuItem.Parent.Command,
                      false, MenuInfo);
    end;
  end;

  with MenuInfo do begin
    cbsize:=sizeof(MENUITEMINFO);
    if AMenuItem.Enabled then fState:=MFS_ENABLED else fstate:=MFS_GRAYED;
    if AMenuItem.Checked then fState:=fState or MFS_CHECKED;
    fMask:=MIIM_ID or MIIM_DATA or MIIM_STATE or MIIM_TYPE;
    wID:=AMenuItem.Command; {value may only be 16 bit wide!}
    dwItemData:=integer(Sender);
    // Note: can't use "and MFT_STRING", because MFT_STRING is zero :-)
    if (AMenuItem.Count > 0) then 
    begin
      fMask := fMask or MIIM_SUBMENU;
      hSubMenu := AMenuItem.Handle;
    end else
      hSubMenu := 0;
    if AMenuItem.Caption <> '-' then
    begin
      fType:=MFT_STRING;
      if AMenuItem.ShortCut <> 0 then
      begin
        Msg.Handle:=hSubMenu;
        ShortCutToKey(AMenuItem.ShortCut, Msg.NewKey, Msg.NewModifier);
        dwTypeData:=LPSTR(AMenuItem.Caption+#9+ShortCutToText(ShortCut(Msg.NewKey, Msg.NewModifier)));
      end else begin
        dwTypeData:=LPSTR(AMenuItem.Caption);
      end;
      cch:=StrLen(dwTypeData);
    end else begin
      fType:=MFT_SEPARATOR;
      dwTypeData:=nil;
      cch:=0;
    end;
    if AmenuItem.HasIcon then {adds the menuitem icon}
    begin
      fMask:=fMask or MIIM_CHECKMARKS;
      hbmpUnchecked:=GetCheckBitmap(false);
      hbmpChecked:=0;
      {TODO: add support for getting icon from SubmenuImages as it will be
       implemented in LCL}
    end;
  end;
  if dword(InsertMenuItem(ParentMenuHandle, AMenuItem.Parent.IndexOf(AMenuItem), true, @MenuInfo)) = 0 then
    DebugLn('InsertMenuItem failed with error: ', IntToStr(Windows.GetLastError));
  // owner could be a popupmenu too
  if (TMenuItem(Sender).Owner is TWinControl) and
      TWinControl(TMenuItem(Sender).Owner).HandleAllocated and
      ([csLoading,csDestroying] * TWinControl(TMenuItem(Sender).Owner).ComponentState = []) then
    DrawMenuBar(TWinControl(TMenuItem(Sender).Owner).Handle);
End;


{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.SetOwner
  Params: Window - Window to which an owner will be set
          Owner  - The owner to set
  Returns: Nothing

  Assigns an owner object to a window
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.SetOwner(Window: HWND; Owner: TObject);
Begin
  SetProp(Window, 'MsgList', Nil);
End;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{
  $Log$
  Revision 1.226  2004/08/23 21:29:40  vincents
  Don't show taskbar button, until MainForm is shown.

  Revision 1.225  2004/08/09 21:12:43  mattias
  implemented FormStyle fsSplash for splash screens

  Revision 1.224  2004/07/27 00:07:48  marc
  * Fixed disabled state of bitbtns

  Revision 1.223  2004/07/16 21:49:00  mattias
  added RTTI controls

  Revision 1.222  2004/07/15 10:43:39  mattias
  added TCustomButton, TCustomBitBtn, TCustomSpeedButton

  Revision 1.221  2004/07/08 20:42:24  micha
  cleanup: do not free nil resource

  Revision 1.220  2004/07/08 20:29:05  micha
  fix win95 compatibility, do not send seperate lm_setshortcut message

  Revision 1.219  2004/07/04 20:05:44  micha
  set properties before setting subclassed window proc

  Revision 1.218  2004/06/30 20:59:11  micha
  initialize common controls: date picker

  Revision 1.217  2004/06/29 21:38:43  vincents
  fix fpc 1.0.x compilation

  Revision 1.216  2004/06/29 14:38:28  micha
  fix default button notification win32 intf

  Revision 1.215  2004/06/29 08:03:08  micha
  fix showtabs for win32 interface

  Revision 1.214  2004/06/20 21:21:49  micha
  fix GetVisible to return this control's visibility, instead introduce IsVisible to check for recursive visibility

  Revision 1.213  2004/06/20 20:36:55  micha
  remove old obsolete/commented toolbutton code
  rename lazarusform classname to window, because we use it for panels, notebookpages, etc too

  Revision 1.212  2004/06/20 13:58:15  micha
  fix combobox edit being gray

  Revision 1.211  2004/06/20 13:00:04  micha
  fix groupbox-parent bug condition, use buddy

  Revision 1.210  2004/06/19 15:10:04  micha
  fix spinedit not firing onchange event

  Revision 1.209  2004/06/18 20:47:34  vincents
  fixed pasting from clipboard

  Revision 1.208  2004/06/18 20:15:06  micha
  remove obsolete LM_LOADXPM message

  Revision 1.207  2004/06/18 19:55:43  micha
  fix xp themes drawing image on bitbtn

  Revision 1.206  2004/06/17 19:54:19  micha
  fix bug in drawing groupbox caption when it is a child of another groupbox

  Revision 1.205  2004/06/15 15:37:29  micha
  fix groupbox background erasing

  Revision 1.204  2004/06/15 14:41:38  micha
  fix drawing bug in xp with themes

  Revision 1.203  2004/06/10 18:14:09  vincents
  converted win32proc.inc to unit

  Revision 1.202  2004/05/31 19:32:34  vincents
  fixed using ecUpperCase in win32

  Revision 1.201  2004/05/31 08:21:52  mattias
  fsStayOnTop is now ignored in design mode

  Revision 1.200  2004/05/30 20:17:55  vincents
  changed radiobutton style to BS_RADIOBUTTON to prevent test program from hanging.

  Revision 1.199  2004/05/27 15:04:53  vincents
  checking a radiobutton unchecks its siblibg radiobuttons

  Revision 1.198  2004/05/21 11:18:30  micha
  use unsigned integer for control styles; fixes compiler warning

  Revision 1.197  2004/05/21 09:03:55  micha
  implement new borderstyle
  - centralize to twincontrol (protected)
  - public expose at tcustomcontrol to let interface access it

  Revision 1.196  2004/05/20 21:28:54  marc
  * Fixed win32 listview

  Revision 1.195  2004/05/16 20:59:53  vincents
  page caption not retrieved from interface, fixed setting caption for page not yet added

  Revision 1.194  2004/05/14 17:48:39  micha
  fix itemheight of listbox, handle measureitem message

  Revision 1.193  2004/05/14 15:20:47  micha
  fix sizing when menu is attached to window

  Revision 1.192  2004/05/12 15:11:46  micha
  fix sizing/non-sizing border sizes

  Revision 1.191  2004/05/12 09:46:25  micha
  fix toolbar buttons by handling them as customcontrols
  remove handledialogmessage, now handled in lcl

  Revision 1.190  2004/05/09 19:14:45  micha
  fix update menu caption at runtime (code accidently commented by me, i think)

  Revision 1.189  2004/05/09 16:24:51  micha
  fix tab order by fixing z-order at control creation

  Revision 1.188  2004/05/08 12:32:32  micha
  fix combobox height; whatever the lcl passes as height for combobox, always calculate our own

  Revision 1.187  2004/04/18 20:11:08  micha
  fix label alignment left, center, right

  Revision 1.186  2004/04/11 10:19:28  micha
  cursor management updated:
  - lcl notifies interface via WSControl.SetCursor of changes
  - fix win32 interface to respond to wm_setcursor callback and set correct cursor

  Revision 1.185  2004/04/11 07:00:30  micha
  speedup: don't redraw menubar if form is being destroyed

  Revision 1.184  2004/04/10 17:54:52  micha
  - added: [win32] mousewheel default handler sends scrollbar messages
  - fixed: lmsetcursor; partial todo

  Revision 1.183  2004/03/18 22:26:24  mattias
  fixed grids TComboBox from Jesus

  Revision 1.182  2004/03/07 12:55:53  micha
  don't create window for image

  Revision 1.181  2004/03/05 15:41:42  micha
  fix appendtext to actually *append* text, not replace

  Revision 1.180  2004/03/05 13:57:25  micha
  fix bug 208: cursor changes when creating submenu

  Revision 1.179  2004/03/05 01:04:21  marc
  * Renamed TWin32Object to TWin32WidgetSet

  Revision 1.178  2004/03/04 21:16:15  micha
  remove workaround; fpc bug fixed

  Revision 1.177  2004/02/28 14:38:47  micha
  fix transparent images for menuitems

  Revision 1.176  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.175  2004/02/26 21:01:42  micha
  fixed: focussing issue combobox

  Revision 1.174  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.172  2004/02/21 13:35:15  micha
  fixed: name clash SetCursor (message LM_SETCURSOR), and inherited SetCursor (winapi)

  Revision 1.171  2004/02/21 10:11:36  micha
    1. pressing the Return key in ObjectInspector when editing a value throws an exception
    2. placing TPairSplitter component on the form produces "Division by zero"

  Revision 1.170  2004/02/20 19:52:18  micha
  fixed: tarrow crash in win32
  added: interface function DrawArrow to draw themed arrow

  Revision 1.169  2004/02/19 14:37:20  micha
  fixed: memo eats return key (from vincent)

  Revision 1.168  2004/02/16 22:01:31  marc
  * Applied patch from Martin Smat
    this patch fixes showing menuitem initially defined as Checked=true
    or Enabled=false

  Revision 1.167  2004/02/15 19:26:48  micha
  fixed: remove GetAncestor dependency; code obsolete? works without too...

  Revision 1.166  2004/02/09 19:52:52  mattias
  implemented ByteOrder for TLazIntfImage and added call of to LM_SETFONT

  Revision 1.165  2004/02/06 23:58:44  marc
  + patch from Jesus Reyes, it enables TCustomEdit SelStart/Length

  Revision 1.164  2004/02/02 16:56:44  micha
  implement GetControlConstraints for combobox

  Revision 1.163  2004/02/01 09:58:21  mattias
  fixed showing statusbar at designtime  from vincent

  Revision 1.162  2004/01/22 18:22:37  mattias
  applied patch for dir dlgs from Vincent

  Revision 1.161  2004/01/21 10:19:16  micha
  enable tabstops for controls; implement tabstops in win32 intf

  Revision 1.160  2004/01/20 22:14:27  micha
  REVERTED: "try register globally unique properties"; implemented new WindowFromPoint not returning window if from different process (tip from vincent)

  Revision 1.158  2004/01/20 10:26:41  micha
  try register globally unique properties

  Revision 1.158  2004/01/12 08:36:34  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.157  2004/01/07 18:04:09  micha
  fix getselcount message for non-multiple-selection listbox

  Revision 1.156  2004/01/03 11:57:48  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.155  2003/12/29 21:56:08  micha
  fix menuitem icon and index (from martin)

  Revision 1.154  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.153  2003/12/27 16:47:18  micha
  fix dialogs owner handle, fixes focusing issue

  Revision 1.152  2003/12/27 16:26:55  micha
  remove redundant window property "lazarus" (from martin)

  Revision 1.151  2003/12/21 11:51:35  micha
  use oldstyledialog flag

  Revision 1.150  2003/12/20 12:54:34  micha
  fix spinedit value retrieval

  Revision 1.149  2003/12/19 21:34:53  micha
  fix compiler problem; wrong code for constants

  Revision 1.148  2003/12/19 18:20:02  micha
  delay property removal until wm_destroy (thx vincent)

  Revision 1.147  2003/12/19 18:18:17  micha
  fix window activation z-order

  Revision 1.146  2003/12/18 10:59:51  micha
  fix notebook page out of bounds while destroying

  Revision 1.145  2003/12/18 10:27:26  micha
  fix fpc 1.9.x compile, limittext, mem free

  Revision 1.144  2003/12/18 10:17:00  micha
  remove non-useful variable wndlist (thx vincent)

  Revision 1.143  2003/12/18 08:51:01  micha
  fix accelerators: now registered per window

  Revision 1.142  2003/12/16 21:04:02  micha
  fix menuitem icon patch, hdcScreen released too soon

  Revision 1.141  2003/12/15 21:57:16  micha
  checklistbox, implement object+checked; from vincent

  Revision 1.140  2003/12/14 20:49:22  micha
  hintwindow focus fix

  Revision 1.139  2003/12/14 19:18:04  micha
  hint fixes: parentfont, font itself, showing/hiding + more

  Revision 1.138  2003/12/13 19:44:42  micha
  hintwindow, color, rectangle size fixes

  Revision 1.137  2003/12/07 22:40:09  mattias
  fixed resizing larger menu icons   from Martin Smat

  Revision 1.136  2003/11/28 19:54:42  micha
  fpc 1.0.10 compatibility

  Revision 1.135  2003/11/25 21:20:38  micha
  implement tchecklistbox

  Revision 1.134  2003/11/25 14:21:28  micha
  new api lclenable,checkmenuitem according to list

  Revision 1.133  2003/11/22 23:56:33  mattias
  fixed win32 intf menu height from Wojciech

  Revision 1.132  2003/11/21 20:32:01  micha
  cleanups; wm_hscroll/wm_vscroll fix

  Revision 1.131  2003/11/21 08:40:54  micha
  menuitems gone that have images, init bug

  Revision 1.130  2003/11/18 07:20:40  micha
  added "included by" notice at top of file

  Revision 1.129  2003/11/16 17:13:20  marc
  * Applied patch from Martin Smat

  Revision 1.128  2003/11/16 16:59:02  marc
  * Fixed DrawOwnerButton

  Revision 1.127  2003/11/09 10:35:19  mattias
  started Menu icons for win32 intf from Martin Smat

  Revision 1.126  2003/11/08 17:41:03  micha
  compiler warning cleanups

  Revision 1.125  2003/10/29 19:47:29  mattias
  fixed win32 compiling

  Revision 1.124  2003/10/29 15:24:28  micha
  fix popupmenu av

  Revision 1.123  2003/10/29 14:24:21  micha
  amenuobject compilation fix

  Revision 1.122  2003/10/28 14:25:37  mattias
  fixed unit circle

  Revision 1.121  2003/10/26 17:34:41  micha
  new interface method to attach a menu to window

  Revision 1.120  2003/10/23 07:45:49  micha
  cleanups; single parent window (single taskbar button)

  Revision 1.119  2003/10/21 15:06:27  micha
  spinedit fix; variables cleanup

  Revision 1.118  2003/10/06 10:53:25  mattias
  fixed redrawing BitBtns from Micha

  Revision 1.117  2003/10/06 10:50:10  mattias
  added recursion to InvalidateClientRectCache

  Revision 1.116  2003/10/02 11:18:09  mattias
  clean ups from Karl

  Revision 1.115  2003/09/30 13:05:59  mattias
  removed FMainForm by Micha

  Revision 1.114  2003/09/27 09:52:44  mattias
  TScrollBox for win32 intf from Karl

  Revision 1.113  2003/09/24 20:43:27  mattias
  fixed wordwrap from Micha

  Revision 1.112  2003/09/21 10:42:48  mattias
  implemented TBitBtn Text+Caption from Micha

  Revision 1.111  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.110  2003/09/18 12:17:25  mattias
  fixed is checks for TCustomXXX controls

  Revision 1.109  2003/09/18 12:15:01  mattias
  fixed is checks for TCustomXXX controls

  Revision 1.108  2003/09/14 09:43:45  mattias
  fixed common dialogs from Karl

  Revision 1.107  2003/09/08 13:29:55  mattias
  clean up

  Revision 1.106  2003/09/08 12:21:48  mattias
  added fpImage reader/writer hooks to TBitmap

  Revision 1.105  2003/09/06 18:37:18  mattias
  fixed checkbox state and typecast bugs

  Revision 1.104  2003/08/31 17:30:49  mattias
  fixed TControl painting for win32

  Revision 1.103  2003/08/31 14:48:15  mattias
  replaced some  as  from Micha

  Revision 1.102  2003/08/30 18:55:42  mattias
  implemented sticked windows from Micha

  Revision 1.101  2003/08/28 09:10:01  mattias
  listbox and comboboxes now set sort and selection at handle creation

  Revision 1.100  2003/08/28 08:14:10  mattias
  implementation of win32 intf borderstyle from Karl

  Revision 1.99  2003/08/27 15:15:42  mattias
  improved setprop from Micha

  Revision 1.98  2003/08/27 09:33:26  mattias
  implements SET_LABEL from Micha

  Revision 1.97  2003/08/27 08:14:37  mattias
  fixed system fonts for win32 intf

  Revision 1.96  2003/08/26 08:12:33  mattias
  applied listbox/combobox patch from Karl

  Revision 1.95  2003/08/25 16:18:16  mattias
  fixed background color of TPanel and clicks of TSpeedButton from Micha

  Revision 1.94  2003/08/23 21:17:09  mattias
  several fixes for the win32 intf, added pending OnResize events

  Revision 1.93  2003/08/23 11:30:51  mattias
  fixed SetComboHeight in win32 intf and finddeclaration of overloaded proc definition

  Revision 1.92  2003/08/22 07:58:38  mattias
  started componenttree

  Revision 1.91  2003/08/21 06:52:47  mattias
  size fixes from Karl

  Revision 1.90  2003/08/19 13:15:18  mattias
  fixed notebook size from Micha

  Revision 1.89  2003/08/17 12:51:35  mattias
  added directory selection dialog from Vincent

  Revision 1.88  2003/08/17 12:26:00  mattias
  fixed parts of the win32 intf size system

  Revision 1.87  2003/08/14 10:36:55  mattias
  added TSelectDirectoryDialog

  Revision 1.86  2003/08/13 21:23:10  mattias
  fixed log

  Revision 1.85  2003/08/13 16:26:07  mattias
  fixed combobox height from Karl

  Revision 1.84  2003/08/12 16:09:54  mattias
  fixed sizing from Karl

  Revision 1.83  2003/08/12 14:02:54  mattias
  fixed keypress/keyup, createcaret on synedit focus

  Revision 1.82  2003/08/11 20:18:46  mattias
  fixed position of control in TGroupBox from Micha

  Revision 1.81  2003/08/09 16:30:34  mattias
  fixed LM_ShowModal for win32 intf from Karl

  Revision 1.80  2003/07/30 21:56:32  marc
  * Fixed LM_APPENDTEXT buffer overrun

  Revision 1.79  2003/07/30 17:41:06  mattias
  added LM_APENDTEXT from Martin Smat

  Revision 1.78  2003/07/28 06:42:42  mattias
  removed debuggging SetName, Patch from Karl Brandt

  Revision 1.77  2003/07/26 10:33:34  mattias
  fixed GetText from Martin

  Revision 1.76  2003/07/26 10:30:44  mattias
  rewritten WM_COMMAND by Micha

  Revision 1.75  2003/07/25 09:28:03  mattias
  fixed notebook page resize from Micha

  Revision 1.74  2003/07/04 17:46:27  mattias
  fixed notebook positioning from Micha

  Revision 1.73  2003/07/04 11:12:27  mattias
  improved default handler from Micha

  Revision 1.72  2003/07/04 10:12:16  mattias
  added default message handler to win32 interface

  Revision 1.71  2003/07/03 18:10:55  mattias
  added fontdialog options to win32 intf from Wojciech Malinowski

  Revision 1.70  2003/07/03 17:19:19  mattias
  added RectVisible from Micha

  Revision 1.69  2003/07/03 08:05:53  mattias
  fixed Criticalsection from Vincent

  Revision 1.68  2003/07/02 20:18:28  mattias
  more cleanups from Micha

  Revision 1.67  2003/07/02 19:35:26  mattias
  fixed AV on start from Vincent

  Revision 1.66  2003/07/02 15:57:04  mattias
  added LCL to win32 cursor mapping from Micha

  Revision 1.65  2003/07/02 15:56:15  mattias
  fixed win32 painting and started creating bitmaps from rawimages

  Revision 1.64  2003/07/01 22:02:55  mattias
  fixed formstyle and redrawing from Micha

  Revision 1.63  2003/06/29 07:16:17  mattias
  fixed compiler warnings

  Revision 1.62  2003/06/28 16:20:19  mattias
  fixed some win32 intf warnings

  Revision 1.61  2003/06/28 13:11:40  mattias
  fixed destroying windows from Micha

  Revision 1.60  2003/06/28 12:49:26  mattias
  fixed LM_SETSIZE from Micha

  Revision 1.59  2003/06/26 14:46:24  mattias
  fixed menu attaching from Micha

  Revision 1.58  2003/06/26 14:24:50  mattias
  fixed progressbar SET_PROPERTIES from Micha

  Revision 1.57  2003/06/25 20:49:58  mattias
  fixed menu destroy from Micha

  Revision 1.56  2003/06/25 20:48:48  mattias
  fixed progressbar from Micha

  Revision 1.55  2003/06/25 15:27:18  mattias
  fixed timer calling conventions from Micha

  Revision 1.54  2003/06/24 21:40:23  mattias
  fixed menu

  Revision 1.53  2003/06/24 15:57:55  mattias
  applied win32 menu patch from Micha Nelissen

  Revision 1.52  2003/06/24 08:32:03  mattias
  applied menu fix from Micha Nelissen

  Revision 1.51  2003/03/25 08:12:39  mattias
  patch from Martin Smat for menu items and default messages

  Revision 1.50  2003/03/18 18:23:07  mattias
  popupmenus for win32 intf from Martin Smat

  Revision 1.49  2003/03/11 23:14:20  mattias
  added TControl.HandleObjectShouldBeVisible

  Revision 1.48  2003/03/06 17:15:49  mattias
  applied patch from Martin Smat fixing LM_SETSIZE, LM_SETDESIGNING, LM_SCREENINIT

  Revision 1.47  2003/02/16 00:43:55  mattias
  fix from Martin Smat for TFileDialogs

  Revision 1.46  2003/02/08 10:37:32  mattias
  applied patch from Martin for TFileDialog

  Revision 1.45  2003/02/01 12:56:10  lazarus
  Keith: My brother fixed the problem where menu items default to disbaled.

  Revision 1.44  2003/01/27 11:25:40  mattias
  menu accelerator patch from Martin Smat

  Revision 1.43  2003/01/19 10:57:46  mattias
  fix WindowProc now react on menu item click from Martin

  Revision 1.42  2003/01/12 19:09:19  mattias
  patch from Martin Smat for dis/enabling menuitems

  Revision 1.41  2003/01/08 18:04:21  mattias
  patch from Martin Smat fixing submenus and menu underscores

  Revision 1.40  2003/01/01 10:46:59  mattias
  fixes for win32 listbox/combobox from Karl Brandt

  Revision 1.39  2002/12/29 18:17:49  mattias
  patch from Martin Smat fixing creating handles

  Revision 1.38  2002/12/28 21:44:51  mattias
  further cleanup

  Revision 1.37  2002/12/28 21:38:50  mattias
  cleanups

  Revision 1.36  2002/12/28 09:42:12  mattias
  toolbutton patch from Martin Smat

  Revision 1.35  2002/12/20 19:08:24  mattias
  notebook patch from vincent

  Revision 1.34  2002/12/19 19:55:37  mattias
  Fixed sending wrong List

  Revision 1.33  2002/12/16 09:02:27  mattias
  applied win32 notebook patch from Vincent

  Revision 1.32  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.31  2002/12/13 10:10:44  mattias
  fixed illegal type cast

  Revision 1.30  2002/12/04 20:39:16  mattias
  patch from Vincent: clean ups and fixed crash on destroying window

  Revision 1.29  2002/12/04 19:25:10  mattias
  fix for resizing window with a menu from Martin Smat

  Revision 1.28  2002/11/26 20:51:05  mattias
  applied clipbrd patch from Vincent

  Revision 1.27  2002/11/23 13:48:48  mattias
  added Timer patch from Vincent Snijders

  Revision 1.26  2002/11/15 23:43:54  mattias
  applied patch from Karl Brandt

  Revision 1.25  2002/09/10 06:49:24  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.24  2002/08/29 17:55:04  lazarus
  Keith: Removed form sizing hack.

  Revision 1.23  2002/08/28 17:28:11  lazarus
  Keith: Win32 fixes.  Much appreciation to Markus Lüdin.

  Revision 1.22  2002/08/25 21:33:54  lazarus
  Keith: Minor sizing enhancements

  Revision 1.21  2002/06/08 19:18:34  lazarus
  Keith: Fixed some bugs that were brought to my attention; fixed compilation problem.

  Revision 1.20  2002/05/10 07:43:48  lazarus
  MG: updated licenses

  Revision 1.19  2002/04/03 03:41:29  lazarus
  Keith:
    * Removed more obsolete code
    * Compiles again!

  Revision 1.18  2002/04/03 01:52:42  lazarus
  Keith: Removed obsolete code, in preperation of a pending TWin32Object cleanup

  Revision 1.17  2002/03/17 21:36:52  lazarus
  Keith: Fixed Win32 compilation problems

  Revision 1.16  2002/02/07 08:35:12  lazarus
  Keith: Fixed persistent label captions and a few less noticable things

  Revision 1.15  2002/02/04 10:54:33  lazarus
  Keith:
    * Fixes for Win32
    * Added new listviewtest.pp example

  Revision 1.14  2002/02/03 06:06:25  lazarus
  Keith: Fixed Win32 compilation problems

  Revision 1.13  2002/02/01 10:13:09  lazarus
  Keith: Fixes for Win32

  Revision 1.12  2002/01/31 09:32:07  lazarus
  Keith:
    * Open and save dialogs can now coexist in apps (however, only one of each type of common dialog can be used per app :( )
    * Fixed make all
    * Fixed crash in Windows 98/ME

  Revision 1.11  2002/01/25 19:42:56  lazarus
  Keith: Improved events and common dialogs on Win32

  Revision 1.10  2002/01/24 07:34:50  lazarus
  Keith: Fixed some bugs

  Revision 1.9  2002/01/21 08:42:06  lazarus
  Keith: Fixed some run-time exceptions for FPC 1.1

  Revision 1.8  2002/01/18 09:07:44  lazarus
  Keith: Fixed menu creation

  Revision 1.7  2002/01/18 00:02:45  lazarus
  Keith: TCustomPage can now be a parent

  Revision 1.6  2002/01/17 03:17:44  lazarus
  Keith: Fixed TCustomPage creation

  Revision 1.4  2002/01/05 13:16:09  lazarus
  MG: win32 interface update from Keith Bowes

  Revision 1.3  2001/11/01 22:40:13  lazarus
  MG: applied Keith Bowes win32 interface updates

  Revision 1.2  2001/08/02 12:58:35  lazarus
  MG: win32 interface patch from Keith Bowes

  Revision 1.1  2000/07/13 10:28:31  michael
  + Initial import

 }

