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
  Method: TWin32Object.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
Constructor TWin32Object.Create;
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
  FNextControlId := 0;
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
Destructor TWin32Object.Destroy;
var
  n: integer;
  TimerInfo : PWin32TimerInfo;
Begin
  Assert(False, 'Trace:TWin32Object is being destroyed');

  n := FTimerData.Count;
  if (n > 0) then
  begin
    Writeln(Format('[TWin32Object.Destroy] WARNING: There are %d TimerInfo structures left, I''ll free them' ,[n]));
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
  Windows.UnregisterClass(@ToolBtnClsName, System.HInstance);

  Inherited Destroy;
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.AppInit
  Params:  None
  Returns: Nothing

  Initialize Windows
 ------------------------------------------------------------------------------}
Procedure TWin32Object.AppInit;
Var
  LogBrush: TLOGBRUSH;
  SysMenu: HMENU;
Begin
  Assert(False, 'Trace:Win32Object.Init - Start');
  If Not WinRegister then
  Begin
    Assert(False, 'Trace:Win32Object.Init - Register Failed');
    writeln('Trace:Win32Object.Init - Register Failed');
    Exit;
  End;
  If Not ToolBtnWinRegister then
  Begin
    Assert(False, 'Trace:Win32Object.Init - Toolbar button Register Failed');
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

  InitCommonControls;

  // Create parent of all windows, `button on taskbar'
  FAppHandle := CreateWindow(@ClsName, PChar(Application.Title), WS_POPUP or
    WS_CLIPSIBLINGS or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX or WS_VISIBLE,
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
  Method: TWin32Object.GetOwnerHandle
  Params:  ADialog - dialog to get 'guiding parent' window handle for
  Returns: A window handle

  Returns window handle to be used as 'owner handle', ie. so that the user must
  finish the dialog before continuing
 ------------------------------------------------------------------------------}
function TWin32Object.GetOwnerHandle(ADialog : TCommonDialog): HWND;
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
  Method: TWin32Object.GetText
  Params:  Sender - The control to retrieve the text from
           Data   - Holds the string
  Returns: Whether the text has been retrieved

  Retrieves the text from a Windows control. This is a replacement for
  the LM_GetText message.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetText(Sender: TComponent; Handle: HWND; var Data: String): Boolean;
Var
  CapLen: Cardinal;
  Caption: PChar;
  Ctrl: TCustomNotebook;
  TCI: TC_ITEM;
Begin
  Assert(False, Format('Trace:TWin32Object.GetText - Start --> %S', [Sender.ClassName]));
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
    csPage:
    Begin
      Assert(False, 'Trace:TWin32Object.GetText - csPage: Start');
      Ctrl := (TCustomPage(Sender).Parent As TCustomNotebook);
      Caption := StrAlloc(MAX_PATH);
      try
        TCI.mask := TCIF_TEXT;
        TCI.cchTextMax := MAX_PATH;
        TCI.pszText := Caption;
        Assert(False, 'Trace:TWin32Object.GetText - Getting the text');
        Result := (Windows.Sendmessage(Ctrl.Handle, TCM_GETITEM,
          TCustomPage(Sender).PageIndex, LPARAM(@TCI))<>0);
        if Result
          then Data := StrPas(Caption);
        Assert(False, Format('Trace:TWin32Object.GetText - Returning the text --> %S', [Data]));
      finally
        StrDispose(Caption);
      end;
      Assert(False, 'Trace:TWin32Object.GetText - csPage: Exit');
    End;
    Else
      Result := False;
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.SetLabel
  Params:   Sender - The object to which to apply the label
            Data   - Pointer to the label
  Returns: Nothing

  Sets the label text on a window
 ------------------------------------------------------------------------------}
Procedure TWin32Object.SetLabel(Sender: TObject; Data: Pointer);

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
    end;
    SetMenuItemInfo(TMenuItem(Sender).Parent.Handle,
                    TMenuItem(Sender).Command, false, @MenuInfo);
    // owner could be a popupmenu too
{
    if (TMenuItem(Sender).Owner is TWinControl) and
        TWinControl(TMenuItem(Sender).Owner).HandleAllocated then
      DrawMenuBar(TWinControl(TMenuItem(Sender).Owner).Handle);
}
  End;

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
    Assert(False, Format('Trace:[TWin32Object.SetLabel] %S --> label %S', [Sender.ClassName, TControl(Sender).Caption]))
  Else
    Assert(False, Format('Trace:WARNING: [TWin32Object.SetLabel] %S --> No Decendant of TWinControl', [Sender.ClassName]));

  Handle := (Sender As TWinControl).Handle;
  Assert(Handle<>0,'Trace:WARNING: [TWin32Object.SetLabel] --> Got NULL handle');
  Assert(False, 'Trace:Setting the label in TWin32Object.SetLabel');

  Case TControl(Sender).FCompStyle Of
    csBitBtn:
      IntSendMessage3(LM_IMAGECHANGED, Sender, Nil);
    csFileDialog, csOpenFileDialog, csSaveFileDialog, csSelectDirectoryDialog,
    csColorDialog, csFontDialog:
    Begin
      Assert(False, Format('Trace:TWin32Object.SetLabel - Got %S', [CS_To_String(TControl(Sender).FCompStyle)]));
      Assert(False, 'Trace:TWin32Object.SetLabel - I''m not sure if this''ll work');
      Assert(False, Format('Trace:Is Sender a TCommonDialog - %S', [BOOL_RESULT[Sender Is TCommonDialog]]));
      If Sender Is TCommonDialog Then
        TCommonDialog(Sender).Title := StrPas(Data)
      Else
        TWinControl(Sender).Caption := StrPas(Data);
      Assert(False, Format('Trace:TWin32Object.SetLabel - Leaving %S', [CS_To_String(TControl(Sender).FCompStyle)]));
    End;
    csComboBox:
    Begin
      Assert(False, Format('Trace:TWin32Object.SetLabel - Got %S', [CS_To_String(TControl(Sender).FCompStyle)]));
      Assert(False, Format('Trace:TWin32Object.SetLabel - label --> %S', [String(PChar(Data))]));
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
      Assert(False, 'Trace: TWin32Object.SetLabel - Got csNotebook');
      with TLMNotebookEvent(Data^) do
      if Parent=Sender then
      begin
        TCI.mask := TCIF_TEXT;
        Assert(False, Format('Trace:TWin32Object.SetLabel - label --> %S', [Str]));
        TCI.pszText := PChar(Str);
        Windows.SendMessage(TCustomNotebook(Sender).Handle, TCM_SETITEM, Page, LPARAM(@TCI));
      end
    End;
    csPage:
    begin
      Assert(False, 'Trace: TWin32Object.SetLabel - Got csPage');
      // We can't set label of a page not yet added
      if TCustomPage(Sender).PageIndex < Windows.SendMessage(TCustomPage(Sender).Parent.Handle, TCM_GETITEMCOUNT,0,0) then
      begin
        Assert(False, Format('Trace:TWin32Object.SetLabel - label --> %S', [String(PChar(Data))]));
        TCI.mask := TCIF_TEXT;
        TCI.pszText := Data;
        Windows.SendMessage(TCustomPage(Sender).Parent.Handle, TCM_SETITEM, TCustomPage(Sender).PageIndex, LPARAM(@TCI));
      end;
    End;
    csToolButton:
    Begin
      Data := StrCat(Data, TermChar);
      SendMessage(TControl(Sender).Parent.Handle, TB_ADDSTRING, 0, MakeLong(Word(Integer(Data)), 0));
    End;
  Else
    Windows.SetWindowText(Handle, Data);
  End;
  Assert(False, Format('Trace:[TWin32Object.SetLabel] %S --> END', [Sender.ClassName]));
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.IntSendMessage3
  Params:  LM_Message - message to be processed
           Sender     - sending control
           Data       - pointer to message-specific data (optional)
  Returns: depends on the message and the sender

  Processes messages from different components.

  WARNING: the result of this function sometimes is not always really an
           integer!!!!!
 ------------------------------------------------------------------------------}
Function TWin32Object.IntSendMessage3(LM_Message: Integer; Sender: TObject; Data: Pointer): Integer;
Var
  XPMFIle : TFileStream;
  //Bitmap: HBITMAP; // Pixel map type image
  //CBI: COMBOBOXINFO;
  DC: HDC;
  Handle: HWND;
  I, Num: Integer;
  ListItemIndex: TListItem;
  LVI: LV_ITEM;
  PStr, PStr2: PChar;
  SizeRect: TRECT; // used by LM_SETSIZE,LM_INVALIDATE,LM_CLB_SET_CHECKED and LM_REDRAW
  S: String;
  TBB: TBBUTTON;
  WindowStyle: Integer; //used by LM_SETTABPOSITION
  OldPageIndex: Integer; //used by LM_SETITEMINDEX of a csNotebook
  AMenu: TMenu;
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
      Assert(False, Format('Trace:  [TWin32Object.IntSendMessage3] %s --> Show/Hide', [Sender.ClassName]));
      ShowHide(Sender);
    End;
    LM_SETCURSOR:
      LmSetCursor(Sender);
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
    LM_APPENDTEXT:
    Begin
      if (Data <> nil)
      and (PChar(Data)^ <> #0)
      then begin
        S := (Sender as TCustomMemo).Text + PChar(Data);
        SetLabel(Sender, PChar(S));
      end;
    End;
    LM_SCREENINIT:
    Begin
      if Sender=nil then Handle := GetDesktopWindow
      else Handle := ObjectToHwnd(Sender);
      DC := Windows.GetDC(Handle);
      //WriteLn('LM_SCREENINIT called --> should go to TWin32Object.Init');
      //WriteLn('TODO: check this');
      PLMScreenInit(Data)^.PixelsPerInchX := GetDeviceCaps(DC, LogPixelsX);
      PLMScreenInit(Data)^.PixelsPerInchY := GetDeviceCaps(DC, LogPixelsY);
      PLMScreenInit(Data)^.ColorDepth := GetDeviceCaps(DC, BitsPixel);
      ReleaseDC(Handle, DC);
    End;
    LM_LOADXPM:
      Begin
        If (Sender is TBitmap) Then
        Begin
         //Until CreateBitmap supports setting Data
         //GTK requires the Bitmap loading to be done
         //by the interface, aka here, so a hack...
          XPMFile := TFileStream.Create(String(Data), fmOpenRead);
          Try
            (Sender as TBitmap).LoadFromStream(XPMFile);
          Finally
            XPMFile.Free;
          end;
          {Assert(False, Format('Trace:Bitmap name: %S', [StrPas(Data)]));
          //SData := String(Data);
          NormalizeIconName(Data);
          //Data := PChar(SData);
          Bitmap := LoadImage(0, LPCTSTR(Data), IMAGE_ICON, 0, 0, LR_DefaultSize Or LR_LoadFromFile);
          Assert(False, 'Trace:1');
          If Bitmap = HBITMAP(Nil) Then
            Assert(False, 'Trace:BITMAP NOT LOADED!');}
        End;
      End;
  Else
  Begin
    Handle := ObjectToHWND(Sender);
    If Handle = HWND(Nil) Then
    Begin
      //Assert (False, Format('Trace:[TWin32Object.IntSendMessage3] %S --> got Handle = Nil', [Sender.ClassName]));
      //Handle := (Sender As TWinControl).Handle;
      //TWinControl(Sender).Handle := Handle;
      //Assert(False, Format('Trace:[TWin32Object.IntSendMessag3] Sender is %S', [Sender.ClassName]));
      Exit;
    End;
    Case LM_Message of
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
            Assert(False, Format('Trace:[TWin32Object.IntSendMessage3] %S --> Calling Add Child: %S', [Parent.ClassName, ClassName]));
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
          Num := Integer(Data^);
          ListView_SetItemCount(Handle, Num);
          ListItemIndex := (Sender As TListView).Items[Num];
          With LVI Do
          Begin
            Mask := LVIF_TEXT;
            IItem := Num;
            PSzText := PChar(ListItemIndex.Caption);
            WriteLn('item: ', Num, ', caption: ', String(PSzText));
            ListView_SetItem(Handle, LVI);
          End;
          For I := 0 To ListItemIndex.SubItems.Count - 1 Do
          Begin
            With LVI Do
            Begin
              Mask := LVIF_TEXT;
              IItem := Num;
              ISubItem := I + 1;
              PSzText := PChar(ListItemIndex.SubItems.Strings[I]);
              ListView_SetItem(Handle, LVI);
            End;
          End;
        End;
      End;
      LM_LV_ADDITEM:
      Begin
        If Sender Is TListView Then
        Begin
          ListItemIndex := TListView(Sender).Items[TListView(Sender).Items.Count - 1];
          With LVI Do
          Begin
            Mask := LVIF_TEXT;
            IItem := TListView(Sender).Items.Count - 1;
            CCHTextMax := MAX_PATH;
            PSzText := StrAlloc(Length(ListItemIndex.Caption) + 1);
            StrPCopy(PSzText, ListItemIndex.Caption);
            ListView_InsertItem(Handle, LVI);
            ListView_Update(Handle, IItem);
            StrDispose(PSzText);
          End;
        End;
      End;
      LM_BRINGTOFRONT:
      Begin
        Assert(False, 'Trace:TODO: [TWin32Object.IntSendMessage3] - LM_BRINGTOFRONT');
        BringWindowToTop(Handle);
      End;
      LM_BTNDEFAULT_CHANGED:
      Begin
        If (TButton(Sender).Default) And (SendMessage(Handle, BM_GETSTATE, 0, 0) = BST_FOCUS) Then
          Windows.SendMessage(Handle, BM_SETSTATE, Windows.WPARAM(True), 0);
      End;
      LM_DESTROY:
      Begin
        If (Sender Is TWinControl) Or (Sender Is TCommonDialog) Then
        Begin
          If Handle <> 0 Then
          begin
            DestroyAcceleratorTable(Windows.GetProp(Handle, 'Accel'));
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
          and TCustomForm(AMenu.Parent).HandleAllocated then
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
        GetClientRect(Handle, SizeRect);
        InvalidateRect(Handle, @SizeRect, True);
      End;
{Displays a menu and makes it available for selection. Applications can use this function to display context-sensitive menus,
and will typically supply NULL for the parent_menu_shell, parent_menu_item, func and data parameters.
The default menu positioning function will position the menu at the current pointer position.
menu :	a GtkMenu.
parent_menu_shell :	the menu shell containing the triggering menu item.
parent_menu_item :	the menu item whose activation triggered the popup.
func :	a user supplied function used to position the menu.
data :	user supplied data to be passed to func.
button :	the button which was pressed to initiate the event.
activate_time :	the time at which the activation event occurred.
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
              case TCustomForm(Sender).BorderStyle of
                bsSizeable,bsSingle,bsDialog:
                  Windows.AdjustWindowRect(@SizeRect,WS_OVERLAPPEDWINDOW,false);
                bsToolWindow,bsSizeToolWin:
                  Windows.AdjustWindowRectEx(@SizeRect,WS_OVERLAPPEDWINDOW,false,WS_EX_TOOLWINDOW);
                //bsNone: -> Do Nothing
              end;
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
        Assert(False, Format('Trace:[TWin32Object.IntSendMessage3] %S --> Redraw', [Sender.ClassName]));
        If Sender Is TCanvas Then
          ReDraw(TCanvas(Sender))
        Else If Not (Sender Is TSpeedbutton) Then
          ReDraw(Sender)
        Else If Sender Is TSpeedButton Then
          If TSpeedbutton(Sender).Visible Then
            TSpeedButton(Sender).Perform(LM_PAINT, 0, 0)
          Else
          Begin
            SizeRect := TSpeedButton(sender).BoundsRect;
            InvalidateRect(TSpeedButton(Sender).Parent.Handle, @SizeRect, True);
          End;
      End;
      LM_ADDPAGE:
      Begin
        Assert(False, Format('Trace:[TWin32Object.IntSendMessage3] %S --> Add NB page: %S', [Sender.ClassName, TLMNotebookEvent(Data^).Child.ClassName]));
        AddNBPage(Sender as TCustomNotebook, TLMNotebookEvent(Data^).Child as TCustomPage, TLMNotebookEvent(Data^).Page);
      End;
      LM_REMOVEPAGE:
      Begin
        RemoveNBPage(TControl(Sender), TLMNotebookEvent(Data^).Page);
      End;
      LM_SHOWTABS:
      Begin
        Result := Ord(True);
        (Sender As TWinControl).Visible := TLMNotebookEvent(Data^).ShowTabs;
        ShowHide(Sender);
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
              Assert(False, 'Trace:[TWin32Object.IntSendMessage3] Could not retrieve item index via LM_GETITEMINDEX; try selecting an item first');
              Result := -1;
            End;
          End;
          csListBox, csCListBox:
          Begin
            Result := SendMessage(Handle, LB_GETCURSEL, 0, 0);
            If Result = LB_ERR Then
            Begin
              Assert(False, 'Trace:[TWin32Object.IntSendMessage3] Could not retrieve item index via LM_GETITEMINDEX; try selecting an item first');
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
            with TLMNotebookEvent(Data^) do
            begin
              OldPageIndex := SendMessage(Handle,TCM_GETCURSEL,0,0);
              Windows.SendMessage(Handle,TCM_SETCURSEL, Windows.WPARAM(Page),0);
              if (Page>=0)
              And ((Parent As TCustomNotebook).CustomPage(Page).HandleAllocated)
                then ShowWindow(TCustomNotebook(Parent).CustomPage(Page).Handle, SW_SHOW);
              if (OldPageIndex>=0) and (OldPageIndex<>Page)
              and (OldPageIndex < TCustomNotebook(Parent).PageList.Count)
              and (TCustomNotebook(Parent).CustomPage(OldPageIndex).HandleAllocated)
                then ShowWindow(TCustomNotebook(Parent).CustomPage(OldPageIndex).Handle, SW_HIDE);
            end;
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
          SendMessage(Handle, CB_SETEDITSEL, 0,
                      MakeLParam(Word(Integer(Data)), High(Word)));
            end;
          csEdit:
            begin
              //WriteLn('LM_SETSELSTART: Start=',Integer(Data));
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
          Windows.SendMessage(Handle, CB_SETCURSEL, Windows.WParam(Data), 0);
            end;
          csEdit:
            begin
              Windows.SendMessage(Handle, EM_GETSEL, Windows.Wparam(@i), Windows.LParam(@Num));
              Num := i + Integer(Data);
              //WriteLn('LM_SETSELLEN: Start=',i, ' End= ',Num, 'Length=',Integer(Data));
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
  Method: TWin32Object.SetCallback
  Params: Msg    - message for which to set a callback
          Sender - object to which callback will be sent
  Returns:  nothing

  Applies a Message to the sender
 ------------------------------------------------------------------------------}
Procedure TWin32Object.SetCallback(Msg: LongInt; Sender: TObject);
Var
  {$IFDEF VER1_1_MSG}
  List: TMsgArray;
  {$ENDIF}
  Window: HWnd;
Begin
  Assert(False, 'Trace:TWin32Object.SetCallback - Start');
  Assert(False, Format('Trace:TWin32Object.SetCallback - Class Name --> %S', [Sender.ClassName]));
  Assert(False, Format('Trace:TWin32Object.SetCallback - Message Name --> %S', [GetMessageName(Msg)]));
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

  Assert(False, 'Trace:TWin32Object.SetCallback - Exit');
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.RemoveCallbacks
  Params:   Sender - object from which to remove callbacks
  Returns:  nothing

  Removes Call Back Signals from the sender
 ------------------------------------------------------------------------------}
Procedure TWin32Object.RemoveCallbacks(Sender: TObject);
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

function TWin32Object.InitHintFont(HintFont: TObject): Boolean;
begin
  TFont(HintFont).Name := FMetrics.lfStatusFont.lfFaceName;
  TFont(HintFont).Style := [];
  TFont(HintFont).Height := FMetrics.lfStatusFont.lfHeight;
  TFont(HintFont).Color := clInfoText;
  TFont(HintFont).Pitch := fpDefault;
  Result := true;
end;

{------------------------------------------------------------------------------
  Method: TWin32Object.HandleEvents
  Params:  None
  Returns: Nothing

  Handle all pending messages
 ------------------------------------------------------------------------------}
Procedure TWin32Object.HandleEvents;
var
  AMessage: TMsg;
  AccelTable: HACCEL;

  function HandleDialogMessage: Boolean;
  var
    TopParent, TmpParent: HWnd;
  begin
    TmpParent := AMessage.HWnd;
    repeat
      TopParent := TmpParent;
      TmpParent := Windows.GetParent(TmpParent);
    until TmpParent = HWND(nil);
    Result := Windows.IsDialogMessage(TopParent, @AMessage);
  end;

Begin
  While PeekMessage(AMessage, HWnd(Nil), 0, 0,PM_REMOVE) Do
  Begin
    AccelTable := HACCEL(Windows.GetProp(AMessage.HWnd, 'Accel'));
    If (AccelTable = HACCEL(nil)) or (TranslateAccelerator(AMessage.HWnd, AccelTable, @AMessage) = 0) Then
    Begin
      if not HandleDialogMessage then
      Begin
        TranslateMessage(@AMessage);
        DispatchMessage(@AMessage);
      End;
    End;
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.WaitMessage
  Params:  None
  Returns: Nothing

  Passes execution control to Windows
 ------------------------------------------------------------------------------}
Procedure TWin32Object.WaitMessage;
Begin
  Assert(False, 'Trace:TWin32Object.WaitMessage - Start');
  Windows.WaitMessage;
  Assert(False,'Trace:Leave wait message');
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.AppTerminate
  Params:  None
  Returns: Nothing

  Tells Windows to halt and destroy
 ------------------------------------------------------------------------------}
Procedure TWin32Object.AppTerminate;
Begin
  Assert(False, 'Trace:TWin32Object.AppTerminate - Start');
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.RecreateWnd
  Params: Sender - The sending object
  Returns: 0

  Creates a window again
 ------------------------------------------------------------------------------}

Function TWin32Object.RecreateWnd(Sender: TWinControl): Integer;
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
function TWin32Object.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : integer;
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
function TWin32Object.DestroyTimer(TimerHandle: Integer) : boolean;
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

procedure TWin32Object.AttachMenuToWindow(AMenuObject: TComponent);
var
  AMenu: TMenu;
begin
  AMenu := AMenuObject as TMenu;
  if AMenu.FCompStyle = csMainMenu then
    Windows.SetMenu(TWinControl(AMenu.Owner).Handle, AMenu.Handle);
end;

{ Private methods (in no significant order) }

{------------------------------------------------------------------------------
  Method: TWin32Object.WinRegister
  Params: None
  Returns: If the window was successfully regitered

  Registers the main window class
 ------------------------------------------------------------------------------}
Function TWin32Object.WinRegister: Boolean;
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

Function TWin32Object.ToolBtnWinRegister: boolean;
var WinClass: WndClass;
begin
  with WinClass do begin
    Style := 0{CS_HRedraw or CS_VRedraw};
    lpfnWndProc := @ToolBtnWindowProc;
    cbClsExtra := 40;
    cbWndExtra := 40;
    hInstance := System.HInstance;
    hIcon := 0{LoadIcon(0, IDI_Application)};
    hCursor := 0{LoadCursor(0, IDC_Arrow)};
    hbrBackground := GetStockObject(WHITE_BRUSH);
    lpszMenuName := nil;
    lpszClassName := @ToolBtnClsName;
  end;
  Result := Windows.RegisterClass(@WinClass) <> 0;
end;

{------------------------------------------------------------------------------
  Method: TWin32Object.PaintPixmap
  Params: Surface    - The surface onto which to paint the pixmap
          PixmapData - Data necessary in drawing the pixmap
  Returns: Nothing

  Paints a pixmap on a surface (control).
 ------------------------------------------------------------------------------}
Procedure TWin32Object.PaintPixmap(Surface: TObject; PixmapData: Pointer);
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
  Method: TWin32Object.NormalizeIconName
  Params: IconName - The name of the icon to normalize
  Returns: Nothing

  Adjusts an icon name to the proper format
 ------------------------------------------------------------------------------}
Procedure TWin32Object.NormalizeIconName(Var IconName: String);
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

Procedure TWin32Object.NormalizeIconName(Var IconName: PChar);
Var
  Str: String;
Begin
  Str := String(IconName);
  NormalizeIconName(Str);
  IconName := StrToPChar(Str);
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.SetLimitText
  Params: Window - The window that has the text to be limited
          Limit  - Number of characters to limit the text to
  Returns: Nothing

  Sets the text limit
 ------------------------------------------------------------------------------}
Procedure TWin32Object.SetLimitText(Window: HWND; Limit: Word);
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
  Method: TWin32Object.CreateCommonDialog
  Params: Sender - The sending object
  Returns: Nothing

  Creates a common dialog
 ------------------------------------------------------------------------------}
Procedure TWin32Object.CreateCommonDialog(Sender: TCommonDialog; CompStyle: Integer);
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
  Assert(False, 'Trace:TWin32Object.CreateCommonDialog - Start');
  Assert(False, Format('Trace:TWin32Object.CreateCommonDialog - class name --> ', [Sender.ClassName]));
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

  Assert(False, 'Trace:TWin32Object.CreateCommonDialog - Exit');
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.CreateSelectDirectoryDialog
  Params: Sender - The sending object: a TSelectDirectoryDialog object
  Returns: Nothing

  Creates a common dialog
 ------------------------------------------------------------------------------}
Procedure TWin32Object.CreateSelectDirectoryDialog(Sender: TSelectDirectoryDialog);
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
  Method: TWin32Object.UpdateStatusBarPanel
  Params: StatusPanel - StatusPanel which needs to be update
  Returns: Nothing

  Called by StatusBarPanelUpdate and StatusBarSetText
  Everything is updated except the panel width
 ------------------------------------------------------------------------------}
procedure TWin32Object.UpdateStatusBarPanel(StatusPanel: TStatusPanel);
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

procedure TWin32Object.UpdateStatusBarPanelWidths(StatusBar: TStatusBar);
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

const

  IDC_ARROW     = MakeIntResource(32512);
  IDC_IBEAM     = MakeIntResource(32513);
  IDC_WAIT      = MakeIntResource(32514);
  IDC_CROSS     = MakeIntResource(32515);
  IDC_UPARROW   = MakeIntResource(32516);
  IDC_SIZE      = MakeIntResource(32640);
  IDC_ICON      = MakeIntResource(32641);
  IDC_SIZENWSE  = MakeIntResource(32642);
  IDC_SIZENESW  = MakeIntResource(32643);
  IDC_SIZEWE    = MakeIntResource(32644);
  IDC_SIZENS    = MakeIntResource(32645);
  IDC_SIZEALL   = MakeIntResource(32646);
  IDC_NO        = MakeIntResource(32648);
  IDC_HAND      = MakeIntResource(32649);
  IDC_APPSTARTING = MakeIntResource(32650);
  IDC_HELP      = MakeIntResource(32651);
  IDC_NODROP    = MakeIntResource(32767);
  IDC_DRAG      = MakeIntResource(32766);
  IDC_HSPLIT    = MakeIntResource(32765);
  IDC_VSPLIT    = MakeIntResource(32764);
  IDC_MULTIDRAG = MakeIntResource(32763);
  IDC_SQLWAIT   = MakeIntResource(32762);
  IDC_HANDPT    = MakeIntResource(32761);

  LclCursorToWin32CursorMap: array[crLow..crHigh] of PChar = (
     IDC_SIZEALL, IDC_HANDPT, IDC_HELP, IDC_APPSTARTING, IDC_NO, IDC_SQLWAIT,
     IDC_MULTIDRAG, IDC_VSPLIT, IDC_HSPLIT, IDC_NODROP, IDC_DRAG, IDC_WAIT,
     IDC_UPARROW, IDC_SIZEWE, IDC_SIZENWSE, IDC_SIZENS, IDC_SIZENESW, IDC_SIZE,
     IDC_IBEAM, IDC_CROSS, IDC_ARROW, IDC_ARROW, IDC_ARROW);

{------------------------------------------------------------------------------
  Method: TWin32Object.LmSetCursor
  Params:  Sender - the control which invoked this method
  Returns: Nothing

  Sets the cursor for a window

  WARNING: Sender will be casted to TControl, CLEANUP!
 ------------------------------------------------------------------------------}
Procedure TWin32Object.LmSetCursor(Sender: TObject);
Var
  Cursor: PChar;
  Res: HCURSOR;
Begin
  Assert(False, 'Trace:TWin32Object.LmSetCursor - Start');
  Assert(False, Format('Trace:TWin32Object.LmSetCursor - Sender --> %S', [Sender.ClassName]));
  Assert(False, 'Trace:TWin32Object.LmSetCursor - Getting the cursor');
  Cursor := LclCursorToWin32CursorMap[TControl(Sender).Cursor];
  Assert(False, 'Trace:TWin32Object.LmSetCursor - Loading the cursor');
  Res := LoadCursor(0, Cursor);
  Assert(False, Format('Trace:Cursor handle --> 0x%X', [Res]));
  Assert(False, 'Trace:TWin32Object.LmSetCursor - Exit');
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.ResizeChild
  Params:  Sender                   - the object which invoked this function
           Left, Top, Width ,Height - new dimensions for the control
  Returns: Nothing

  Resize a window
 ------------------------------------------------------------------------------}
Procedure TWin32Object.ResizeChild(Sender: TWinControl;
  Left, Top, Width, Height: Integer);

Var
{$IFDEF VerboseSizeMsg}
  OldLeft: Integer;
  OldTop: Integer;
{$ENDIF}
  WinHandle, BuddyHandle: HWND;
Begin
  // if not Sender.HandleAllocated then exit; --> Already checked (LM_SETSIZE and LM_RECREATEWND)
  {$IFDEF VerboseSizeMsg}
  OldLeft:=Left;
  OldTop:=Top;
  {$ENDIF}
  LCLBoundsToWin32Bounds(Sender, Left, Top, Width, Height);
  {$IFDEF VerboseSizeMsg}
  writeln('TWin32Object.ResizeChild A ',AWinControl.Name,':',AWinControl.ClassName,
    ' LCL=',OldLeft,',',OldTop,',',Width,',',Height,
    ' Win32=',Left,',',Top,',',Width,',',Height,
    '');
  {$ENDIF}
  WinHandle := Sender.Handle;
  if Sender.FCompStyle = csSpinEdit then
  begin
    // detach from buddy first
    BuddyHandle := Windows.SendMessage(WinHandle, UDM_SETBUDDY, 0, 0);
    MoveWindow(BuddyHandle, Left, Top, Width, Height, True);
    // reattach
    Windows.SendMessage(WinHandle, UDM_SETBUDDY, BuddyHandle, 0);
  end else begin
    MoveWindow(WinHandle, Left, Top, Width, Height, True);
  end;
  LCLControlSizeNeedsUpdate(Sender,False);
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.AddChild
  Params:  Parent - Parent to which the child will be added
           Child  - Child to add
  Returns: Nothing

  Adds A Child to a Parent
 ------------------------------------------------------------------------------}
Procedure TWin32Object.AddChild(Parent, Child: HWND);
Begin
  Assert(False, 'Trace:AddChild - Parent Window Handle is $' + IntToHex(LongInt(Parent), 8));
  Assert(False, 'Trace:AddChild - Child Window Handle is $' + IntToHex(LongInt(Child), 8));
  SetParent(Child, Parent);
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.SetText
  Params:  Child - Window to add the text
           Data  - The text to add
  Returns: Nothing

  Sets the text of a control.

  WARNING: This should possibly be merged with the SetLabel method!
           It's only left in here for TStatusBar right now cause it
           may be nice to use it with different panels.
 ------------------------------------------------------------------------------}
Procedure TWin32Object.SetText(Window: HWND; Data: Pointer);
Begin
  Case PLMSetControlText(Data)^.FCompStyle Of
    csStatusBar:
    Begin
      Windows.SendMessage(Window, SB_SETTEXT, Windows.WParam(PLMSetControlText(Data)^.Panel),
        Windows.LParam(LPSTR(PLMSetControlText(Data)^.UserData)));
    End
    Else
      AssertEx('STOPPOK: [TWin32Object.SetText] Possible superfluous use of SetText, use SetLabel instead!', False, 2);
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32Object SetColor
  Params: Sender - The sending object
  Returns: Nothing

  Changes the form's default background color
 ------------------------------------------------------------------------------}
Procedure TWin32Object.SetColor(Sender: TObject);
Begin

End;

{------------------------------------------------------------------------------
  Function: TWin32Object.CreateComponent
  Params:   Sender - object for which to create visual representation
  Returns:  nothing

  Tells Windows to create a control
 ------------------------------------------------------------------------------}
Function TWin32Object.CreateComponent(Sender: TObject): THandle;
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
    Assert(False, Format('Trace:TWin32Object.CreateComponent - %S parent --> %S', [Sender.ClassName, TWinControl(Sender).Parent.ClassName]));
    Parent := TWinControl(Sender).Parent.Handle;
    Assert(False, Format('Trace:TWin32Object.CreateComponent - parent handle --> 0x%X', [Parent]));
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
    {$IFDEF VerboseSizeMsg}
    writeln('TWin32Object.CreateComponent A ',TControl(Sender).Name,':',TControl(Sender).ClassName,' ',Left,',',Top,',',Width,',',Height);
    {$ENDIF}
  End
  Else If (Sender Is TMenuItem) Then
  Begin
    Assert(False, 'Trace:[TWin32Object.CreateComponent] - Sender is a menu item');
    Caption := TMenuItem(Sender).Caption;
    Assert(False, Format('Trace:[TWin32Object.CreateComponent] - Caption set to %S', [Caption]));
    CompStyle := TMenuItem(Sender).FCompStyle;
    Assert(False, Format('Trace:[TWin32Object.CreateComponent] - CompStyle set to %S', [CS_To_String(CompStyle)]));
  End
  Else If (Sender Is TMenu) { Or (Sender Is TPopupMenu) } Then
    CompStyle := TMenu(Sender).FCompStyle
  Else If (Sender Is TCommonDialog) Then
    CompStyle := TCommonDialog(Sender).FCompStyle;

  If Caption = '' Then
    Caption := CS_To_String(CompStyle);

  Assert(False, Format('Trace:TWin32Object.CreateComponent - Creating component %S with the caption of %S', [Sender.ClassName, Caption]));
  Assert(False, Format('Trace:TWin32Object.CreateComponent - Left: %D, Top: %D, Width: %D, Height: %D, Parent handle: 0x%X, instance handle: 0x%X', [Left, Top, Width, Height, Parent, HInstance]));

  // until here remove when debug not needed
  If Caption = '' Then
    Caption := 'Blank';
  StrCaption := StrAlloc(Length(Caption) + 1);
  StrPCopy(StrCaption, Caption);
  Assert(False, 'Trace:CreateComponent - Control Style is ' + CS_To_String(CompStyle));

  Case CompStyle Of
    // controls with a window handle
    csBitBtn:
    Begin
      pClassName := 'BUTTON';
      if TBitBtn(Sender).Default Then
        Flags := Flags or BS_DEFPUSHBUTTON
      else
        Flags := Flags or BS_PUSHBUTTON;
      Flags := Flags or BS_OWNERDRAW;
      WindowTitle := nil;
      IntSendMessage3(LM_LOADXPM, Sender, StrCaption);
    End;
    csButton:
    Begin
      Assert(False, 'Trace:CreateComponent - Creating Button');
      if TButton(Sender).Default Then
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
    csImage:
    Begin
      // nothing to do
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
    csArrow, csFixed:
    Begin
      Assert(False, 'Trace:TODO: Figure out what component style csFixed is and code the component.  No component created.');
      pClassName := @ClsName;
      WindowTitle := StrCaption;
      SubClassWndProc := nil;
    End;
    csForm:
    Begin
      Assert(False, 'Trace:CreateComponent - Creating a Form Window');
      Flags:= WS_OVERLAPPEDWINDOW or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
      Case TCustomForm(Sender).BorderStyle of
      //bsSizeable:; -> Default
      bsSingle:
        Flags:= Flags and DWORD(not WS_THICKFRAME);
      bsDialog:
        Flags:= Flags and DWORD(not (WS_THICKFRAME or WS_MINIMIZEBOX or WS_MAXIMIZEBOX));
      bsNone:
        Flags:= WS_POPUP or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
      bsToolWindow:
      Begin
        FlagsEx:=WS_EX_TOOLWINDOW;
        Flags:= Flags and DWORD(not WS_THICKFRAME);
      End;
      bsSizeToolWin:
        FlagsEx:=WS_EX_TOOLWINDOW;
      End;//case

      If TCustomForm(Sender).FormStyle = fsStayOnTop Then
        FlagsEx:= FlagsEx or WS_EX_TOPMOST;
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
      Flags := Flags Or LVS_LIST Or LVS_SINGLESEL;
    End;
    csMemo:
    Begin
      Assert(False, 'Trace:TWin32Object.CreateComponent - Creating a MEMO...');
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
      Flags := Flags Or BS_AUTORADIOBUTTON;
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
    csToolButton:
    Begin
      pClassName := ToolBtnClsName;
      Flags := 0;
      SubClassWndProc := nil;
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
      csMainMenu, csMenuBar, csMenuItem:
      Begin
        Window := CreateMenu;
      End;
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
      csPopupMenu:
      Begin
        Window := CreatePopupMenu;
      End;
    end;
  End; {Case}

  if not AlternateCreateWindow then
  begin
    if (Flags and WS_CHILD) <> 0 then
    begin
      // menu handle is also for specifying a control id if this is a child
      MenuHandle := HMENU(FNextControlId);
      Inc(FNextControlId);
    end else begin
      MenuHandle := HMENU(nil);
    end;
    Window := CreateWindowEx(FlagsEx, pClassName, WindowTitle, Flags,
        Left, Top, Width, Height, Parent, MenuHandle, HInstance, Nil);
  end;
  
  Result := Window;

  If (Sender Is TWinControl) Or (CompStyle = csImage) Then
  Begin
    TWinControl(Sender).Handle := Window;
    If Window <> HWND(Nil) Then
    begin
      if SubClassWndProc <> nil then
        Windows.SetProp(Window, 'DefWndProc', Windows.SetWindowLong(Window, GWL_WNDPROC, LongInt(SubClassWndProc)));
      // subclass edit control of combobox too
      if CompStyle = csCombobox then
      begin
        Buddy := Windows.GetTopWindow(Window);
	if Buddy <> HWND(nil) then
	  Windows.SetProp(Buddy, 'DefWndProc', Windows.SetWindowLong(Buddy, GWL_WNDPROC, LongInt(@ComboBoxEditWindowProc)));
      end;
      Windows.SendMessage(Window, WM_SETFONT, WParam(FMessageFont), 0);
    end;
    If Buddy <> HWND(Nil) Then
      Windows.SendMessage(Buddy, WM_SETFONT, WParam(FMessageFont), 0);
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

  Try
    StrDispose(StrCaption);
  Except
    Assert(False, 'Trace:Warning: Tried to dispose a string that was not allocated');
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
  Method: TWin32Object.AssignSelf
  Params: Window - The window to assign
          Data   - The data to assign to the window
  Returns: Nothing

  Assigns data to a window
 ------------------------------------------------------------------------------}
procedure TWin32Object.AssignSelf(Window: HWnd; Data: Pointer);
begin
  Assert(False, 'Trace:[TWin32Object.AssignSelf] Trying to code it.  It''s probably wrong.');
end;

{------------------------------------------------------------------------------
  Method: TWin32Object.ShowHide
  Params: Sender - The sending object
  Returns: Nothing

  Shows or hides a control
 ------------------------------------------------------------------------------}
Procedure TWin32Object.ShowHide(Sender: TObject);
Var
  Handle: HWND;
Begin
  If (TControl(Sender).FCompStyle = csPage) or (TControl(Sender).FCompStyle = csToolButton) then exit;
  Handle := ObjectToHWND(Sender);
  If TControl(Sender).HandleObjectShouldBeVisible Then
  Begin
    Assert(False, 'Trace: [TWin32Object.ShowHide] Showing the window');
    if TControl(Sender).FCompStyle = csHintWindow then
    begin
      //  NOTE: for some reason, 1.9.x generates the wrong constant here...
      //  NOTE: same for 1.0.11 (FIXES_1_0_0 branch), 1.0.10 does seem to work
      Windows.SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, $253{SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER});
    end else begin
      Windows.ShowWindow(Handle, SW_SHOW);
    end;
    If (Sender Is TCustomForm) Then
      SetClassLong(Handle, GCL_HICON, LONG(TCustomForm(Sender).GetIconHandle));
  End
  Else
  Begin
    Assert(False, 'TRACE: [TWin32Object.ShowHide] Hiding the window');
    If Sender Is TCustomForm then
      If fsModal in TCustomForm(Sender).FormState then
        EnumThreadWindows(GetWindowThreadProcessId(Handle,nil),@EnableWindowsProc, Handle);
    ShowWindow(Handle, SW_HIDE);
  End;
End;

{ -----------------------------------------------------------------------------
  Method: TWin32Object.AddNBPage
  Params: Notebook - A notebook control
          NewPage - Page to insert
          Index  - The position in the notebook to insert the page
  Returns: Nothing

  Adds a new page to a notebook
 ------------------------------------------------------------------------------}
Procedure TWin32Object.AddNBPage(Notebook: TCustomNotebook; NewPage: TCustomPage; Index: Integer);
Var
  TabControlItem: TC_ITEM;
  PageCaption: string;
  OldPageIndex: integer;
  R: TRect;
Begin
  Assert(False, 'Trace:TWin32Object.AddNBPage - Start');
  With Notebook, TabControlItem do
  Begin
    Mask := 0;
    // Before adding, the page caption comes from TControl.FCaption.
    // After adding, it is read from the TabControl,
    // so we cache it temporarily for later use.
    PageCaption := NewPage.Caption;
    Windows.SendMessage(Handle, TCM_INSERTITEM, Index, integer(@TabControlItem));
    // Set page caption in tabcontrol
    SetLabel(NewPage, PChar(PageCaption));
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
  Method: TWin32Object.RemoveNBPage
  Params: Parent - The notebook control
          Index  - The page to delete
  Returns: Nothing

  Removes a page from a notebook control
 ------------------------------------------------------------------------------}
Procedure TWin32Object.RemoveNBPage(Parent: TObject; Index: Integer);
Begin
  Assert(false, 'Trace:Removing a notebook page');
  Windows.SendMessage((Parent As TCustomNotebook).Handle, TCM_DELETEITEM, Windows.WPARAM(Index), 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.ReDraw
  Params: Child - Component to redraw
  Returns: Nothing

  Redraws a component
 ------------------------------------------------------------------------------}
Procedure TWin32Object.ReDraw(Child: TObject);
Begin
  Assert(False, 'TRACE:[TWin32Object.ReDraw] Redrawing...');
  Assert(False, 'TRACE:Invalidating the window');
  IntSendMessage3(LM_INVALIDATE, Child, Nil);
  Assert(False, 'TRACE:Updating the window');
  UpdateWindow(TWinControl(Child).Handle);
  Assert(False, 'TRACE:[TWin32Object.ReDraw] Finished redrawing');
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.SetPixel
  Params:  Sender - the lcl object which called this func via SendMessage
           Data   - pointer to a TLMSetGetPixel record
  Returns: nothing

  Set the color of the specified pixel on the window?screen?object?
 ------------------------------------------------------------------------------}
Procedure TWin32Object.SetPixel(Sender: TObject; Data: Pointer);
Var
  DC: HDC;
  Handle: HWnd;
Begin
  Assert(False, 'Trace:TODO: Implement TWin32Object.SetPixel');
  Handle :=(Sender As TWinControl).Handle;
  DC := GetDC(Handle);
  With TLMSetGetPixel(Data^) Do
    Windows.SetPixel(DC, X, Y, PixColor);
  ReleaseDC(Handle, DC);
end;

{------------------------------------------------------------------------------
  Method: TWin32Object.GetPixel
  Params:  Sender - the lcl object which called this func via SenMessage
           Data   - pointer to a TLMSetGetPixel record
  Returns: nothing

  Get the color of the specified pixel on the window?screen?object?
 -----------------------------------------------------------------------------}
Procedure TWin32Object.GetPixel(Sender: TObject; Data: Pointer);
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
  Method: TWin32Object.GetValue
  Params:  Sender - the lcl object which called this func via SenMessage
           Data   - pointer to component specific variable
  Returns: currently always 0

  Depending on the compStyle, this function will get the current value
  of a Window and save it in the variable referenced by 'Data'.

  This function should be used to synchronize the state of an lcl-object
  with the corresponding Windows object.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetValue(Sender: TObject; Data: Pointer): Integer;
Var
  Handle: HWnd;
  ST: SystemTime;
Begin
  Result := 0;     // default if nobody sets it

  If Sender Is TWinControl Then
    Assert(False, Format('Trace:[TWin32Object.GetValue] %S', [Sender.ClassName]))
  Else
    Assert(False, Format('Trace:WARNING: [TWin32Object.GetValue] %S --> No Decendant of TWinControl', [Sender.ClassName]));

  Handle := TWinControl(Sender).Handle;
  Assert (Handle = 0, 'Trace:WARNING: TWin32Object.GetValue --> got no window');

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
      Assert (True, Format('WARNING:[TWin32Object.GetValue]] failed for %S', [Sender.ClassName]));
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.SetValue
  Params:  Sender - the lcl object which called this func via SendMessage
           Data   - pointer to component specific variable
  Returns: currently always 0

  Depending on the CompStyle, this function will apply the parameter 'data'
  to the Windows object repesenting the lcl-object which called the function.

  This function should be used in cases where the most common property
  of an object has changed (e.g. the position of a trackbar). If more than
  one property changed use the SetProperties function instead;
 ------------------------------------------------------------------------------}
Function TWin32Object.SetValue(Sender: TObject; Data: Pointer): Integer;
Var
  Cur: PChar;
  Handle: HWnd;
  ST: SystemTime;

Begin
  Result := 0;     // default if nobody sets it

  If Sender Is TWinControl Then
    Assert(False, Format('Trace:[TWin32Object.SetValue] %S', [Sender.ClassName]))
  Else
    Assert(False, Format('Trace:WARNING:[TWin32Object.SetValue] %S --> No Decendant of TWinControl', [Sender.ClassName]));

  Handle := TWinControl(Sender).Handle;
  If Handle = HWnd(Nil) Then
    Assert (False, 'Trace:WARNING:[TWin32Object.SetValue] --> got no window');

  Case TControl(Sender).FCompStyle Of
    csArrow:
    Begin
      Case TLMArrow(Data^).ArrowType Of
        atUp:
          Cur := IDC_UPARROW;
        atLeft, atRight:
          Cur := IDC_SIZEWE;
        atDown:
          Cur := IDC_SIZENS;
      End;
      SetClassLong(Handle, GCL_HCursor, LoadCursor(HInst(Nil), Cur));
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
    csRadiobutton, csCheckbox:
    Begin
      If TCheckBoxState(Data^) = cbChecked Then
        Windows.SendMessage(Handle, BM_SETCHECK, Windows.WParam(BST_CHECKED), 0)
      Else If TCheckboxState(Data^) = cbUnchecked Then
        Windows.SendMessage(Handle, BM_SETCHECK, Windows.WParam(BST_UNCHECKED), 0)
      Else
        Windows.SendMessage(Handle, BM_SETCHECK, Windows.WParam(BST_INDETERMINATE), 0);
    End
    Else
      Assert (True, Format('Trace:WARNING: [TWin32Object.SetValue] failed for %S', [Sender.ClassName]));
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.SetProperties
  Params:  Sender - the lcl object which called this func via SenMessage
  Returns: currently always 0

  Depending on the compStyle, this function will apply all properties of
  the calling object to the corresponding Window.
 ------------------------------------------------------------------------------}
Function TWin32Object.SetProperties(Sender: TObject): Integer;
Var
  Handle: HWND;
  I: Integer;
  LVC: LV_COLUMN;
  Style: Integer;
begin
  Result := 0; // default if nobody sets it

  If Sender Is TWinControl Then
    Assert(False, Format('Trace:[TWin32Object.SetProperties] %S', [Sender.ClassName]))
  Else
    Assert(False, Format('Trace:WARNING: [TWin32Object.SetProperties] %S --> No Decendant of TWinControl', [Sender.ClassName]));

  Handle := TWinControl(Sender).Handle;
  If Handle = HWND(Nil) Then
    Assert (False, 'Trace:WARNING: [TWin32Object.SetProperties] --> got nil pointer');

  Case TControl(Sender).FCompStyle Of

  csEdit:
    With (TCustomEdit(Sender)) Do
    Begin
      Windows.SendMessage(Handle, EM_SETREADONLY, Windows.WPARAM(ReadOnly), 0);
      Windows.SendMessage(Handle, EM_LIMITTEXT, Windows.WPARAM(MaxLength), 0);
    End;

  csListView:
    With TListView(Sender) Do
    Begin
      If ViewStyle = vsReport Then
      Begin
        For I := 0 To Columns.Count - 1 Do
        Begin
          With LVC Do
          Begin
            Mask := LVCF_FMT Or LVCF_TEXT Or LVCF_WIDTH;
            Fmt := Integer(Columns.Items[I].Alignment);
            CX := Columns.Items[I].Width;
            PSzText := PChar(Columns.Items[I].Caption);
          End;
          ListView_SetColumn(Handle, I, LVC);
        End;
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
      Assert(False, 'Trace:TODO: [TWin32Object.SetProperties] Set up step and page increments for csScrollBar');
    End;

  csSpinEdit:
    Begin
      SendMessage(Handle, UDM_SETRANGE, 0, MakeLong(Trunc(TSpinEdit(Sender).MaxValue), Trunc(TSpinEdit(Sender).MinValue)));
      SendMessage(Handle, UDM_SETPOS, 0, MakeLong(Trunc(TSpinEdit(Sender).Value), 0));
    End;

  csTrackbar:
    With(TTrackBar(Sender)) Do
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
      Case Alignment of
        taLeftJustify:
          Style := SS_LEFT;
        taCenter:
          Style := SS_CENTER;
        taRightJustify:
          Style := SS_CENTER;
      Else
        Style := SS_LEFT; // default, shouldn't happen
      End;
      Case Layout of
        tlTop:
          Style := Style Or BS_TOP;
        tlCenter:
          Style := Style Or BS_VCENTER;
        tlBottom:
          Style := Style Or BS_BOTTOM;
        Else
          Style := Style Or BS_BOTTOM; //default, shouldn't happen
      End;
      // Experimental wordwrapping support
      If Wordwrap Then
        Style := Style and not SS_LEFTNOWORDWRAP
      Else
        Style := Style or SS_LEFTNOWORDWRAP;

      SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or Style);
      Assert(False, 'TRACE:Wordwrapping of labels is not currently implemented');
      Assert(False, 'Trace:TODO: Code wordwrapping labels');
    End;
  Else
    Assert (True, Format('WARNING: [TWin32Object.SetProperties] failed for %S', [Sender.ClassName]));
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.AttachMenu
  Params:  Sender - the lcl object which called this func
  Returns: nothing

  Attaches the calling Menu to its Parent
 ------------------------------------------------------------------------------}
Procedure TWin32Object.AttachMenu(Sender: TObject);
var MenuInfo: MENUITEMINFO;
    MenuHandle: HMenu;
    ParentMenuHandle: HMenu;
    ParentOfParent: HMenu;
    Style: integer;
    Mask: integer;
    Msg: TLMShortCut;
    AMenuItem: TMenuItem;

  function GetCheckBitmap(checked: boolean): HBitmap;
  {TODO: create "checked" icon}
  var hbmpCheck: HBitmap;
      OldCheckMark: HBitmap;
      OldOrigBitmap: HBitmap;
      hdcNewBitmap: HDC;
      hdcOrigBitmap: HDC;
      hdcScreen: HDC;
      maxWidth: integer;
      maxHeight: integer;
      newWidth: integer;
      newHeight: integer;
  begin
    maxWidth:=GetSystemMetrics(SM_CXMENUCHECK);
    maxHeight:=GetSystemMetrics(SM_CYMENUCHECK);
    if (maxWidth>=AMenuItem.Bitmap.Width) and (maxHeight>=AMenuItem.Bitmap.Height) then Result:=AMenuItem.Bitmap.Handle
    else
    begin
      newWidth:=min(maxWidth, AMenuItem.Bitmap.Width);
      newHeight:=min(maxHeight, AMenuItem.Bitmap.Height);
      hdcScreen:=GetDC(GetDesktopWindow);
      hdcOrigBitmap:=CreateCompatibleDC(hdcScreen);
      hdcNewBitmap:=CreateCompatibleDC(hdcScreen);
      hbmpCheck:=CreateCompatibleBitmap(hdcScreen, newWidth, newHeight);
      ReleaseDC(GetDesktopWindow, hdcScreen);
      OldOrigBitmap:=SelectObject(hdcOrigBitmap, AMenuItem.Bitmap.Handle);
      OldCheckmark:=SelectObject(hdcNewBitmap, hbmpCheck);
      StretchBlt(hdcNewBitmap, 0, 0, newWidth, newHeight, hdcOrigBitmap, 0, 0, AMenuItem.Bitmap.Width, AMenuItem.Bitmap.Height, SRCCOPY);
      SelectObject(hdcOrigBitmap, OldOrigBitmap);
      hbmpCheck:=SelectObject(hdcNewBitmap, OldCheckmark);
      DeleteDC(hdcNewBitmap);
      DeleteDC(hdcOrigBitmap);
      {TODO: Add hbmpCheck into a list of object they must be deleted}
      Result:=hbmpCheck;
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

  Mask := MIIM_ID or MIIM_DATA or MIIM_STATE;
  Style:=0;
  if (AMenuItem.Count > 0) then Mask := Mask or MIIM_SUBMENU;
  MenuHandle := AMenuItem.Handle;
  with MenuInfo do begin
    cbsize:=sizeof(MENUITEMINFO);
    if AMenuItem.Enabled then fState:=MFS_ENABLED else fstate:=MFS_GRAYED;
    if AMenuItem.Checked then fState:=fState or MFS_CHECKED;
    fMask:=Mask;
    fType:=Style;
    wID:=AMenuItem.Command; {value may only be 16 bit wide!}
    hSubmenu:=MenuHandle;
    dwItemData:=integer(Sender);
    if AmenuItem.HasIcon then {adds the menuitem icon}
    begin
      fMask:=fMask or MIIM_CHECKMARKS;
      hbmpUnchecked:=GetCheckBitmap(false);
      hbmpChecked:=0;
      {TODO: add support for getting icon from SubmenuImages as it will be
       implemented in LCL}
    end;
  end;
  InsertMenuItem(ParentMenuHandle, AMenuItem.Parent.IndexOf(AMenuItem), true, @MenuInfo);
  if AMenuItem.ShortCut <> 0 then
  begin
    Msg.Handle:=MenuHandle;
    ShortCutToKey(AMenuItem.ShortCut, Msg.NewKey, Msg.NewModifier);
    IntSendMessage3(LM_SETSHORTCUT, Sender, @Msg);
  end
  else SetLabel(Sender, LPSTR(AMenuItem.Caption));
End;


{------------------------------------------------------------------------------
  Method: TWin32Object.SetOwner
  Params: Window - Window to which an owner will be set
          Owner  - The owner to set
  Returns: Nothing

  Assigns an owner object to a window
 ------------------------------------------------------------------------------}
Procedure TWin32Object.SetOwner(Window: HWND; Owner: TObject);
Begin
  SetProp(Window, 'MsgList', Nil);
End;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{
  $Log$
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

