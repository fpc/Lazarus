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
Var
  AcTbl: Array[1..50] Of ACCEL;
Begin
  Inherited Create;
  FKeyStateList := TList.Create;
  FDeviceContexts := TDynHashArray.Create(-1);
  FGDIObjects := TDynHashArray.Create(-1);
  FMessageQueue := TList.Create;
  FAccelGroup := CreateAcceleratorTable(LPACCEL(@AcTbl), High(AcTbl));
  FTimerData  := TList.Create;
  FTimerWindow := 0;
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
      TimerInfo := PWin32Timerinfo (FTimerData.Items[n]);
      Dispose (TimerInfo);
      FTimerData.Delete(n);
    end;
  end;
  Assert(False,'Trace:FTimerWindow: ' +  IntToStr(FTimerWindow));
  if FTimerWindow<>0 then
  begin
    DestroyWindow(FTimerWindow);
    FTimerWindow := 0;
  end;

  FMessageQueue.Free;
  FDeviceContexts.Free;
  FGDIObjects.Free;
  FKeyStateList.Free;
  FTimerData.Free;

  DestroyAcceleratorTable(FAccelGroup);

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
Begin
  Assert(False, 'Trace:Win32Object.Init - Start');
  If Not WinRegister then
  Begin
    Assert(False, 'Trace:Win32Object.Init - Register Failed');
    writeln('Trace:Win32Object.Init - Register Failed');
    Exit;
  End;
  If Not TimerWinRegister then
  Begin
    Assert(False, 'Trace:Win32Object.Init - LCLTimerWindow Register Failed');
    Exit;
  End;

  FToolTipWindow := CreateWindowEx(WS_EX_TOPMOST, TOOLTIPS_CLASS, NULL, WS_POPUP Or TTS_NOPREFIX Or TTS_ALWAYSTIP, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, FParentWindow, HMENU(NULL), HInstance, NULL);
  Windows.SendMessage(FParentWindow, TTM_ACTIVATE, WPARAM(True), 0);

  try
    FTimerWindow :=CreateWindowEx(0, @TimerClsName, nil, WS_POPUPWINDOW, 0, 0, 640, 480, 0, 0, System.HInstance, nil);
  except
  // Sometimes an access violation occurs, I don't know why
    on E: Exception do
    begin
      FTimerWindow := 0;
      writeln('Win32Object.Init: Failed to create a hidden window to receive timer messages');
      writeln('Exception: ',E.Message);
    end;
  end;
  Assert(False,'Trace:FTimerWindow: ' + IntToStr(FTimerWindow));
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

  InitCommonControls;

  Assert(False, 'Trace:Win32Object.Init - Exit');
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.GetText
  Params:  Sender - The control to retrieve the text from
           Data   - Holds the string
  Returns: Whether the text has been retrieved

  Retrieves the text from a Windows control. This is a replacement for
  the LM_GetText message.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetText(Sender: TComponent; Var Data: String): Boolean;
Var
  CapLen: Cardinal;
  Caption: PChar;
  Ctrl: TNotebook;
  TCI: TC_ITEM;
Begin
  Assert(False, Format('Trace:TWin32Object.GetText - Start --> %S', [Sender.ClassName]));
  Data := '';
  Result := True;
  Case (Sender as TControl).FCompStyle Of
    csComboBox:
    Begin
      CapLen := SendMessage((Sender As TWinControl).Handle, CB_GETLBTEXTLEN, CNSendMessage(LM_GETITEMINDEX, Self, Nil), 0);
      Caption := StrAlloc(CapLen + 1);
      SendMessage((Sender As TWinControl).Handle, CB_GETLBTEXT, CNSendMessage(LM_GETITEMINDEX, Self, Nil), LPARAM(Caption));
      Data := StrPas(Caption);
      StrDispose(Caption);
    End;
    csEdit, csMemo:
    Begin
      CapLen := GetWindowTextLength((Sender As TWinControl).Handle);
      Caption := StrAlloc(CapLen + 1);
      GetWindowText((Sender As TWinControl).Handle, Caption, CapLen + 1);
      Data := StrPas(Caption);
      StrDispose(Caption);
    End;
    csPage:
    Begin 
      Assert(False, 'Trace:TWin32Object.GetText - csPage: Start');
      Ctrl := ((Sender As TPage).Parent As TNotebook);
      Caption := StrAlloc(MAX_PATH);
      try
        TCI.mask := TCIF_TEXT;
        TCI.cchTextMax := MAX_PATH;
        TCI.pszText := Caption;
        Assert(False, 'Trace:TWin32Object.GetText - Getting the text');
        Result := (Windows.Sendmessage(Ctrl.Handle, TCM_GETITEM, (Sender as TPage).PageIndex, LPARAM(@TCI))<>0);
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
  // Result := Data <> '';
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.SetLabel
  Params:   Sender - The object to which to apply the label
            Data   - Pointer to the label
  Returns: Nothing

  Sets the label text on a window
 ------------------------------------------------------------------------------}
Procedure TWin32Object.SetLabel(Sender: TObject; Data: Pointer);
Var
  Handle, HOwner: HWnd;
  R: TRect;
  TCI: TC_ITEM;
Const
  TermChar: PChar = #0#0;
Begin
  If Sender Is TWinControl Then
    Assert(False, Format('Trace:[TWin32Object.SetLabel] %S --> label %S', [Sender.ClassName, TControl(Sender).Caption]))
  Else
    Assert(False, Format('Trace:WARNING: [TWin32Object.SetLabel] %S --> No Decendant of TWinControl', [Sender.ClassName]));

  Handle := (Sender As TWinControl).Handle;
  HOwner := GetAncestor(Handle, GA_ROOTOWNER);
  Assert(Handle = 0, 'Trace:WARNING: [TWin32Object.SetLabel] --> Got NULL handle');
  Assert(False, 'Trace:Setting the label in TWin32Object.SetLabel');
 
  Case TControl(Sender).FCompStyle Of
    csBitBtn:
      IntSendMessage3(LM_IMAGECHANGED, Sender, Nil);
    csColorDialog, csFileDialog, csFontDialog:
    Begin
      Assert(False, Format('Trace:TWin32Object.SetLabel - Got %S', [CS_To_String(TControl(Sender).FCompStyle)]));
      Assert(False, 'Trace:TWin32Object.SetLabel - I''m not sure if this''ll work');
      Assert(False, Format('Trace:Is Sender a TCommonDialog - %S', [BOOL_RESULT[Sender Is TCommonDialog]]));
      If Sender Is TCommonDialog Then
        (Sender As TCommonDialog).Title := StrPas(Data)
      Else
        (Sender As TWinControl).Caption := StrPas(Data);
      Assert(False, Format('Trace:TWin32Object.SetLabel - Leaving %S', [CS_To_String(TControl(Sender).FCompStyle)]));
    End;
    csComboBox:
    Begin
      Assert(False, Format('Trace:TWin32Object.SetLabel - Got %S', [CS_To_String(TControl(Sender).FCompStyle)]));
      Assert(False, Format('Trace:TWin32Object.SetLabel - label --> %S', [String(PChar(Data))]));
      Assert(False, 'Trace:TWin32Object.SetLabel - I''m not sure if this''ll work');
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
        Windows.SendMessage((Sender As TCustomNotebook).Handle, TCM_SETITEM, Page, LPARAM(@TCI));
      end
    End;
    csPage:
    begin
      Assert(False, 'Trace: TWin32Object.SetLabel - Got csPage');
      // We can't set label of a page not yet added
      if (Sender As TPage).PageIndex < Windows.SendMessage((Sender As TPage).Parent.Handle, TCM_GETITEMCOUNT,0,0) then
      begin
        Assert(False, Format('Trace:TWin32Object.SetLabel - label --> %S', [String(PChar(Data))]));
        TCI.mask := TCIF_TEXT;
        TCI.pszText := Data;
        Windows.SendMessage((Sender As TPage).Parent.Handle, TCM_SETITEM, (Sender As TPage).PageIndex, LPARAM(@TCI));
      end;
    End;
    csToolButton:
    Begin
      Data := StrCat(Data, TermChar);
      SendMessage(Handle, TB_ADDSTRING, 0, MakeLong(Word(Integer(Data)), 0));
    End;
    Else
      SetWindowText(Handle, Data);

    If TControl(Sender).FCompStyle = csLabel Then
    Begin
      GetClientRect(HOwner, R);
      InvalidateRect(HOwner, @R, True);
      UpdateWindow(HOwner);
    End;

  End;
  Assert(False, Format('Trace:[TWin32Object.SetLabel] %S --> END', [Sender.ClassName]));
End;
{$C+}
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
  Bitmap: HBITMAP; // Pixel map type image
  CBI: COMBOBOXINFO;
  DC: HDC;
  Handle: HWND;
  I, Num: Integer;
  ListItemIndex: TListItem;
  LVI: LV_ITEM;
  PStr, PStr2: PChar;
  R, R2: TRECT;
  SelectionMode: DWORD; // currently only used for listboxes
  TBB: Array[0..1] Of TBBUTTON; // Limited to 2 buttons at present
  WindowStyle: Integer; //used by LM_SETTABPOSITION
  OldPageIndex: Integer; //used by LM_SETITEMINDEX of a csNotebook
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
      SetCursor(Sender);
    LM_SETLABEL:
      SetLabel(Sender, Data);
    LM_GETVALUE:
      Result := GetValue(Sender, Data);
    LM_SETVALUE:
      Result := SetValue(Sender, Data);
    LM_SETPROPERTIES:
      Result := SetProperties(Sender);
    LM_SETDESIGNING:
      EnableWindow((Sender As TWinControl).Handle, False);
    LM_RECREATEWND:
      Result := RecreateWnd(Sender);
    LM_ATTACHMENU:
      AttachMenu(Sender);
        //SH: think of TBitmap.handle!!!!
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
            SendMessage(Handle, BM_SETSTATE, WPARAM(True), 0);
        End;
        LM_DESTROY:
        Begin
          If (Sender Is TWinControl) Or (Sender Is TCommonDialog) Then 
          Begin
            If Handle <> 0 Then
              DestroyWindow(Handle);
          End
          Else If Sender Is TMenu Then
            If Handle <> 0 Then
              DestroyMenu(Handle)
          Else
            Assert(False, Format('Trace:I don''t know how to destroy component %S', [Sender.ClassName]));
        End;
        LM_DRAGINFOCHANGED:
        Begin
        End;
        //TBitBtn
        LM_IMAGECHANGED, LM_LAYOUTCHANGED:
        Begin
          Assert(False, 'Trace:[TWin32Object.IntSendMessage3 - Got LM_IMAGECHANGED or LM_LAYOUTCHANGED');
          Assert(False, 'Trace:********************');
          Assert(False, 'Trace:1');
          Bitmap := (Sender As TBitBtn).Glyph.Handle;
          SendMessage(Handle, BM_SETIMAGE, IMAGE_BITMAP, Bitmap);
          SetWindowText(Handle, PChar((Sender As TWinControl).Caption));
          Assert(False, 'Trace:5');
          Assert(False, 'Trace:********************');
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
          TrackPopupMenuEx(HMENU(TWinControl(Sender).Handle), TPM_LEFTALIGN, TControl(Sender).Left, TControl(Sender).Top, TWinControl(Sender).Parent.Handle, Nil);	
        End;
        LM_SETFILTER:
        Begin 
          PStr := StrAlloc(Length(TFileDialog(Sender).Filter) + 1);
          Try
            StrPCopy(PStr, TFileDialog(Sender).Filter);
            If Sender Is TFileDialog Then
              LPOpenFileName(@Sender)^.LPStrFilter := PStr;
          Finally
            StrDispose(pStr);
          End;
        End; 
        LM_SETFILENAME: 
        Begin 
          PStr := StrAlloc(Length(TFileDialog(Sender).FileName) + 1);
          Try
            StrPCopy(PStr, TFileDialog(Sender).FileName);
            If Sender Is TFileDialog Then
              LPOpenFileName(@Sender)^.LPStrFile := PStr;
          Finally
            StrDispose(pStr);
          End;
        End; 
        LM_SETFOCUS:
        Begin
          If Handle <> 0 Then
            SetFocus(Handle);
        End;
        LM_SETSIZE:
        Begin
          If Sender Is TWinControl Then
          begin
            With PRect(Data)^ do
            begin
              R := Rect(Left,Top,Right,Bottom);
            {Get the width and height for the form}
            If TControl(Sender).FCompStyle = csForm Then
              begin
                 R.Right:=Left + Right;
                 R.Bottom:=Top + Bottom;
                 if (Sender is TCustomForm) and ((Sender as TCustomForm).Menu<>nil) then Windows.AdjustWindowRect(@R,WS_OVERLAPPEDWINDOW,true)
                 else Windows.AdjustWindowRect(@R,WS_OVERLAPPEDWINDOW,false);
                 R := Rect(Left, Top, R.Right - R.Left, R.Bottom - R.Top)
              end
            else if TControl(Sender).FCompStyle = csPage then
              begin
                if (Sender As TWinControl).Parent.Handle<>0 then
                begin
                  Windows.GetClientRect((Sender As TWinControl).Parent.Handle,@R);
                  Windows.SendMessage((Sender As TWinControl).Parent.Handle, TCM_AdjustRect, 0, LPARAM(@R));
                  R := Rect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
                end
              end;
            If TWinControl(Sender).HandleAllocated Then
                  ResizeChild(Sender, R.Left, R.Top, R.Right , R.Bottom);
            end;
          end;
        End;
        LM_SHOWMODAL:
        Begin
          If Sender Is TCommonDialog Then
          Begin
            // Should be done elsewhere (eg via SetLabel) not here!
            PStr := StrAlloc(Length(TCommonDialog(Sender).Title) + 1);
            Try
              StrPCopy(PStr, TCommonDialog(Sender).Title);
              LPOpenFileName(@Sender)^.LPStrTitle := PStr;
            Finally
               StrDispose(PStr);
            End;
          End;
          SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_Style) Or WS_DLGFRAME);
          ShowWindow(Handle, SW_Show);
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
              (Sender As TSpeedButton).Perform(LM_PAINT, 0, 0)
            Else
            Begin
              R2 := TSpeedButton(sender).BoundsRect;
              InvalidateRect(TSpeedButton(Sender).Parent.Handle, @R2, True);
            End;
        End;
        LM_ADDPAGE:
        Begin
          Assert(False, Format('Trace:[TWin32Object.IntSendMessage3] %S --> Add NB page: %S', [Sender.ClassName, TLMNotebookEvent(Data^).Child.ClassName]));
          AddNBPage(Sender as TCustomNotebook, TLMNotebookEvent(Data^).Child as TPage, TLMNotebookEvent(Data^).Page);
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
            Windows.SetWindowLong((Sender As TWinControl).Handle, GWL_STYLE, WindowStyle);
          End;
        End;
        LM_INSERTTOOLBUTTON:
        Begin
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

          With tbb[Num] Do
          Begin
            iBitmap := Num;
            idCommand := Num;
            fsState := TBSTATE_ENABLED;
            iString := Integer(PStr);
          End;

          SendMessage(Handle, TB_BUTTONSTRUCTSIZE, SizeOf(tbb), 0);
          SendMessage(Handle, TB_ADDBUTTONS, WParam(UInt(IntSendMessage3(LM_TB_BUTTONCOUNT, Sender, Nil) + 1)), LParam(LPTBButton(@tbb)));
          StrDispose(pStr);
          StrDispose(pStr2);
          Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!');
        End;
        LM_DELETETOOLBUTTON:
        Begin
          SendMessage((Sender As TToolbar).Parent.Handle, TB_DELETEBUTTON, WParam(Data^), 0); // Assuming Data is the button to remove
        End;
        LM_INVALIDATE: 
        Begin 
          Assert(False, 'Trace:Trying to invalidate window... !!!');
          GetClientRect(Handle, R);
          InvalidateRect(Handle, @R, True);
        End;
        LM_SETFORMICON:
        Begin
          SetClassLong(Handle, GCL_HIcon, (Sender As TForm).GetIconHandle);
        End;
        LM_SCREENINIT:
        Begin
          DC := GetDC(Handle);
          WriteLn('LM_SCREENINIT called --> should go to TWin32Object.Init');
          WriteLn('TODO: check this');
          PLMScreenInit(Data)^.PixelsPerInchX := GetDeviceCaps(DC, LogPixelsX);
          PLMScreenInit(Data)^.PixelsPerInchY := GetDeviceCaps(DC, LogPixelsY);
          PLMScreenInit(Data)^.ColorDepth := GetDeviceCaps(DC, BitsPixel);
          ReleaseDC(Handle, DC);
        End;  
        LM_GETITEMS :
        Begin
          If (Sender as TControl).fCompStyle = csCListBox Then
          Begin
            Data := TWin32CListStringList.Create(Handle);
            Result := Integer(Data);
          End 
          Else
          Begin
            Data := TWin32ListStringList.Create(Handle{Control});
            Result := Integer(Data);      
          End;
        End;
        LM_GETTEXT :
        Begin
          Result := Integer(GetText(Sender As TComponent,PString(Data)^));
        End;
        LM_GETITEMINDEX :
        Begin
          Case (Sender as TControl).FCompStyle Of
            csListBox, csCListBox: 
            Begin
              If TListBox(Sender).MultiSelect Then
              Begin
                Result := SendMessage(Handle, LB_GETSELITEMS, 0, LParam(@Result));
              End
              Else
              Begin
                Result := SendMessage(Handle, LB_GETCURSEL, 0, 0);
                If Result = LB_ERR Then
                Begin
                  Assert(False, 'Trace:[TWin32Object.IntSendMessage3] Could not retrieve item index via LM_GETITEMINDEX; try selecting an item first');
                  Result := -1;
                End;
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
            csComboBox: SendMessage(Handle, CB_SETCURSEL, WParam(Integer(Data)), 0);
            csListBox, csCListBox:
            Begin
              If TListBox(Sender).MultiSelect Then
                SendMessage(Handle, LB_SETSEL, WPARAM(TRUE), LParam(Integer(Data)))
              Else
                SendMessage(Handle, LB_SETCURSEL, WParam(Integer(Data)), 0);
            End;
            csNotebook:
            Begin
              Assert(False, 'Trace:Setting Page to ' + IntToStr(TLMNotebookEvent(Data^).Page));
              with TLMNotebookEvent(Data^) do
              begin
                OldPageIndex := SendMessage(Handle,TCM_GETCURSEL,0,0);
                SendMessage(Handle,TCM_SETCURSEL,WPARAM(Page),0);
{                if (Page>=0) And ((Parent As TCustomNotebook).Page[Page].HandleAllocated)
                  then ShowWindow((Parent As TCustomNotebook).Page[Page].Handle, SW_SHOW);}
                if (OldPageIndex>=0) and (OldPageIndex<>Page) and ((Parent As TCustomNotebook).Page[OldPageIndex].HandleAllocated)
                  then ShowWindow((Parent As TCustomNotebook).Page[OldPageIndex].Handle, SW_HIDE);
              end;
            End;
          End;
        End;  
        LM_GETSELSTART:
        Begin
          If (Sender as TControl).FCompStyle = csComboBox Then
          Begin
            Result := Low(SendMessage(Handle, CB_GETEDITSEL, WPARAM(NULL), LPARAM(NULL)));           
          End;
        End;
        LM_GETSELLEN:
        Begin
          If (Sender as TControl).FCompStyle = csComboBox then
          Begin
            Result := SendMessage(Handle, CB_GETEDITSEL, WPARAM(NULL), LPARAM(NULL));
            Result := High(Result) - Low(Result);
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
          If (Sender Is TControl) And (TControl(Sender).FCompStyle = csComboBox) Then
            SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Word(Integer(Data)), -1));
        End;    
        LM_SETSELLEN:
        Begin
          If (Sender Is TControl) And (TControl(Sender).FCompStyle = csComboBox) Then
          Begin
            SendMessage(Handle, CB_SETCURSEL, WParam(Data), 0);
          End;
        End;
        LM_GETLINECOUNT:
        Begin
          If Sender Is TMemo Then
            Result := SendMessage(Handle, EM_GETLINECOUNT, 0, 0);
        End;
        LM_GETSELCOUNT:
        Begin
          Case (Sender as TControl).FCompStyle Of
            csListBox, csCListBox:
              Result := SendMessage(Handle, LB_GETSELCOUNT, 0, 0);
          End;
        End;
        LM_GETSEL:
        Begin
          If ((Sender As TWinControl).FCompStyle = csListBox) Or ((Sender As TControl).FCompStyle = csCListBox) then
          Begin
            Result := Windows.SendMessage(Handle, LB_GETSEL, WParam(Data), 0);
          End
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
                TWin32ListStringList(TLMSort(Data^).List).Sorted := TLMSort(Data^).IsSorted;
              csCListBox:
                TWin32CListStringList(TLMSort(Data^).List).Sorted := TLMSort(Data^).IsSorted;
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
                SendMessage(Handle, LB_SELITEMRANGE, WParam(True), MakeLParam(0, 0))
              Else
                SendMessage(Handle, LB_SELITEMRANGE, WParam(False), MakeLParam(0, 0));
            End
          End;  
        End;
        LM_SETSELMODE:
        Begin
          If (Sender is TControl) And (TControl(Sender).fCompStyle In [csListBox, csCListBox]) And Assigned(data) Then 
          Begin
            If TLMSetSelMode(Data^).MultiSelect Then 
            Begin
              If TLMSetSelMode(Data^).ExtendedSelect Then
                SelectionMode := LBS_EXTENDEDSEL
              Else
                SelectionMode := LBS_MULTIPLESEL;
            End 
            Else 
              SelectionMode:= 0;
            SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_Style) Or SelectionMode);
          End;
        End;
        LM_SETBORDER:
        Begin
          If Sender is TControl Then
          Begin
            If (TControl(Sender).fCompStyle = csListBox) Or (TControl(Sender).FCompStyle = csCListBox) Then
            Begin
              If TListBox(Sender).BorderStyle = TBorderStyle(bsSingle) Then
                SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) Or WS_EX_CLIENTEDGE)
              Else
                SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) And Not WS_EX_CLIENTEDGE);
            End
          End;
        End;
        Else
          Assert(True, Format ('WARNING: Unhandled message %d in IntSendMessage3 send by %s --> message:Redraw', [LM_Message, Sender.ClassName]));
          // unhandled message
      End; // end of 2nd case
    End; // end of else-part of 1st case
  End; // end of 1st case
End;
{$C-}
{------------------------------------------------------------------------------
  Method: TWin32Object.SetCallback
  Params: Msg    - message for which to set a callback
          Sender - object to which callback will be sent
  Returns:  nothing

  Applies a Message to the sender
 ------------------------------------------------------------------------------}
Procedure TWin32Object.SetCallback(Msg: LongInt; Sender: TObject);
Var
  List: TMsgArray;
  Window: HWnd;
Begin
  Assert(False, 'Trace:TWin32Object.SetCallback - Start');
  Assert(False, Format('Trace:TWin32Object.SetCallback - Class Name --> %S', [Sender.ClassName]));
  Assert(False, Format('Trace:TWin32Object.SetCallback - Message Name --> %S', [GetMessageName(Msg)]));
  If Sender Is TControlCanvas Then
    Window := (Sender As TControlCanvas).Handle
  Else If Sender Is TCustomForm Then
    Window := (Sender As TCustomForm).Handle
  Else
    Window := (Sender as TWinControl).Handle;
  
  {$IFDEF VER1_1}
    List := TMsgArray(GetProp(Window, 'MsgList'));
    SetLength(List, Length(List) + 1);
    List[Length(List) + 1] := Msg;
    SetProp(Window, 'MsgList', Pointer(List));
  {$ENDIF}

  //SetProp(Window, 'MsgColl', List);
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
  List: TMsgArray;
  Window: HWnd;
Begin
  If Sender Is TControlCanvas Then
    Window := (Sender As TControlCanvas).Handle
  Else If Sender Is TCustomForm Then
    Window := (Sender As TCustomForm).Handle
  Else
    Window := (Sender as TWinControl).Handle;

  {$IFDEF VER1_1}
    List := TMsgArray(GetProp(Window, 'MsgList'));
    Pointer(List) := Nil;
    SetProp(Window, 'MsgList', Pointer(List));
  {$ENDIF}
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.HandleEvents
  Params:  None
  Returns: Nothing

  Handle all pending messages
 ------------------------------------------------------------------------------}
Procedure TWin32Object.HandleEvents;
var
  AMessage: TMsg;
Begin
  While PeekMessage(AMessage, HWnd(Nil), 0, 0,PM_REMOVE) Do
  Begin
    If TranslateAccelerator(AMessage.HWnd, FAccelGroup, @AMessage) = 0 Then
    Begin
      TranslateMessage(@AMessage);
      DispatchMessage(@AMessage);
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
Var
  MainForm: TForm;
Begin
  Assert(False, 'Trace:TWin32Object.AppTerminate - Start');
  MainForm := Nil;
  If Application.MainForm <> Nil Then
    MainForm := Application.MainForm
  Else If FMainForm <> Nil then
    MainForm := FMainForm;
  if MainForm<> Nil then
    DestroyWindow(MainForm.Handle);
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.RecreateWnd
  Params: Sender - The sending object
  Returns: 0

  Creates a window again
 ------------------------------------------------------------------------------}
  
Function TWin32Object.RecreateWnd(Sender: TObject): Integer;
Var
  AParent : TWinControl;
Begin
  //could we just call IntSendMessage??

  //destroy old widget
  If TWinControl(Sender).Handle <> 0 Then
    DestroyWindow(TWinControl(Sender).Handle);

  AParent := TWinControl(Sender).Parent;
  AParent.RemoveControl(TControl(Sender));

  TWinControl(Sender).Parent := Nil;
  TWinControl(Sender).Parent := AParent;

  ResizeChild(Sender, TWinControl(Sender).Left, TWinControl(Sender).Top, TWinControl(Sender).Width, TWinControl(Sender).Height);
  ShowHide(Sender);

  Result := 0;
End;

{------------------------------------------------------------------------------
  Function: CreateTimer
  Params: Interval:
          TimerFunc: Callback
  Returns: a Timer id (use this ID to destroy timer)

  Design: A timer which sends a wm_timr is created,
    the wm_timer is received by a hidden window,
    which calls the TimerFunc in its window proc.
 ------------------------------------------------------------------------------}
function TWin32Object.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : integer;
var
  TimerInfo: PWin32TimerInfo;
begin
  Assert(False,'Trace:Create Timer: ' + IntToStr(Interval));
  Result := 0;
  if (FTimerWindow<>0) and (Interval >= 1) and (TimerFunc <> nil)
  then begin
    New(TimerInfo);
    TimerInfo^.TimerFunc := TimerFunc;
    Result:= Windows.SetTimer(FTimerWindow,integer(TimerInfo),Interval,nil);
    if Result = 0 then
      Dispose(TimerInfo)
    else begin
      TimerInfo^.TimerHandle:= Result;
      FTimerData.Add(TimerInfo);
    end;
  end;
  Assert(False,'Trace:Result: ' + IntToStr(result));
end;

{------------------------------------------------------------------------------
  Function: DestroyTimer
  Params: TimerHandle
  Returns:
 ------------------------------------------------------------------------------}
function TWin32Object.DestroyTimer(TimerHandle: integer) : boolean;
var
  n : integer;
  TimerInfo : PWin32Timerinfo;
begin
  Result:= false;
  Assert(False,'Trace:removing timer: '+ IntToStr(TimerHandle));
  n := FTimerData.Count;
  while (n > 0) do begin
    dec (n);
    TimerInfo := PWin32Timerinfo(FTimerData.Items[n]);
    if (TimerInfo^.TimerHandle=TimerHandle) then
    begin
      result := Windows.KillTimer(FTimerWindow, integer(TimerInfo));
      FTimerData.Delete(n);
      Dispose(TimerInfo);
    end;
  end;
  Assert(False,'Trace:Destroy timer Result: '+ BOOL_RESULT[result]);
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
    Style := CS_HRedraw or CS_VRedraw;
    LPFnWndProc := WndProc(@WindowProc);
    CbClsExtra := 40;
    CbWndExtra := 40;
    HInstance := System.HInstance;
    HIcon := LoadIcon(0, IDI_Application);
    HCursor := LoadCursor(0, IDC_Arrow);
    HBrBackground := GetSysColorBrush(Color_BtnFace);
    LPSzMenuName := Nil;
    LPSzClassName := @ClsName;
  End;
  Result := Windows.RegisterClass(@WindowClass) <> 0;
  Assert(False, 'Trace:WinRegister - Exit');
End;

Function TWin32Object.TimerWinRegister: boolean;
var
  WinClass: WndClassEx;
begin
  with WinClass do begin
    cbSize := Sizeof(WndClassEx);
    Style := CS_HRedraw or CS_VRedraw;
    lpfnWndProc := WndProc(@TimerWindowProc);
    cbClsExtra := 0;
    cbWndExtra := 0;
    hInstance := System.HInstance;
    _hIcon := LoadIcon(0, IDI_Application);
    hCursor := LoadCursor(0, IDC_Arrow);
    hbrBackground := GetStockObject(LTGRAY_BRUSH);
    lpszMenuName := nil;
    lpszClassName := @TimerClsName;
    hIconSm := LoadIcon(0, IDI_APPLICATION);
  end;
  result := RegisterClassEx(@WinClass) <> 0;
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
  DrawIcon(DC, (Surface As TWinControl).Left, (Surface As TWinControl).Top, Pixmap);
  ReleaseDC((Surface As TWinControl).Handle, DC);
End;
  
{------------------------------------------------------------------------------
  Method: TWin32Object.NormalizeIconName
  Params: IconName - The name of the icon to normalize
  Returns: Nothing

  Adjusts an icon name to the proper format
 ------------------------------------------------------------------------------}
Procedure TWin32Object.NormalizeIconName(Var IconName: String);
Var
  IcoLen: Byte;
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
  Cls: PChar;
  Msg: Cardinal;
  Str: String;
Begin
  GetClassInfo(Window, @Cls, 5);
  Str := LowerCase(String(PChar(@Cls)));
  If Str = 'edit' Then
    Msg := CB_LIMITTEXT
  Else If Str = 'combo' Then
    Msg := EM_LIMITTEXT
  Else
    Exit;

  SendMessage(Window, Msg, Limit, 0);
  SetProp(Window, 'LIMIT_TEXT', @Limit);

  If WndList.IndexOf(Pointer(Window)) = -1 Then
  Begin
    WndList.Capacity := WndList.Count;
    WndList.Add(Pointer(Window));
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.CreateCommonDialog
  Params: Sender - The sending object
  Returns: Nothing

  Creates a common dialog
 ------------------------------------------------------------------------------}
Procedure TWin32Object.CreateCommonDialog(Sender: TCommonDialog);
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
  Ret: Boolean;

  Function GetFlagsFromOptions(Options: TOpenOptions): DWord;
  Begin
    Result := 0;
    If ofAllowMultiSelect In Options Then
      Result := Result Or OFN_ALLOWMULTISELECT;
    If ofCreatePrompt In Options Then
      Result := Result Or OFN_CREATEPROMPT;
    If Not (ofOldStyleDialog In Options) Then
      Result := Result Or OFN_EXPLORER;
    If ofExtensionDifferent In Options Then
      Result := Result Or OFN_EXTENSIONDIFFERENT;
    If ofFileMustExist In Options Then
      Result := Result Or OFN_FILEMUSTEXIST;
    If ofHideReadOnly In Options Then
      Result := Result Or OFN_HIDEREADONLY;
    If ofNoChangeDir In Options Then
      Result := Result Or OFN_NOCHANGEDIR;
    If ofNoDereferenceLinks In Options Then
      Result := Result Or OFN_NODEREFERENCELINKS;
    If ofNoLongNames In Options Then
      Result := Result Or OFN_NOLONGNAMES;
    If ofNoNetworkButton In Options Then
      Result := Result Or OFN_NONETWORKBUTTON;
    If ofNoReadOnlyReturn In  Options Then
      Result := Result Or OFN_NOREADONLYRETURN;
    If ofNoTestFileCreate In Options Then
      Result := Result Or OFN_NOTESTFILECREATE;
    If ofNoValidate In Options Then
      Result := Result Or OFN_NOVALIDATE;
    If ofOverwritePrompt In Options Then
      Result := Result Or OFN_OVERWRITEPROMPT;
    If ofPathMustExist In Options Then
      Result := Result Or OFN_PATHMUSTEXIST;
    If ofReadOnly In Options Then
      Result := Result Or OFN_READONLY;
    If ofShareAware In Options Then
      Result := Result Or OFN_SHAREAWARE;
    If ofShowHelp In Options Then
      Result := Result Or OFN_SHOWHELP;
  End;
Begin
  Assert(False, 'Trace:TWin32Object.CreateCommonDialog - Start');
  Assert(False, Format('Trace:TWin32Object.CreateCommonDialog - class name --> ', [Sender.ClassName]));
  If Sender Is TColorDialog Then
  Begin
    CC := LPChooseColor(@Sender)^;
    ZeroMemory(@CC, SizeOf(TChooseColor));
    With CC Do
    Begin
      LStructSize := SizeOf(TChooseColor);
      HWndOwner := ((Sender As TComponent).Owner As TWinControl).Handle;
      RGBResult := (Sender As TColorDialog).Color;
      LPCustColors := @CustomColors;
      Flags := CC_FullOpen Or CC_RGBInit;
    End;
    Ret := ChooseColor(@CC)
  End
  Else If Sender Is TFileDialog Then
  Begin
    If Sender Is TOpenDialog Then
    Begin
      OpenFile := LPOpenFileName(@Sender)^;
      ZeroMemory(@OpenFile, SizeOf(OpenFileName));
      With OpenFile Do
      Begin
        LStructSize := SizeOf(OpenFileName);
        HWndOwner := ((Sender As TComponent).Owner As TWinControl).Handle;
        LPStrFilter := PChar((Sender As TOpenDialog).Filter);
        {If (Sender As TOpenDialog).FileName <> '' Then
          LPStrFile := PChar((Sender As TOpenDialog).FileName);}
        LPStrFileTitle := PChar((Sender As TOpenDialog).Title);
        LPStrInitialDir := PChar((Sender As TOpenDialog).InitialDir);
          Flags := GetFlagsFromOptions((Sender As TOpenDialog).Options);
      End;
      Ret := GetOpenFileName(@OpenFile)
    End
    Else If Sender Is TSaveDialog Then
    Begin
      OpenFile := LPOpenFileName(@Sender)^;
      ZeroMemory(@OpenFile, SizeOf(OpenFileName));
      With OpenFile Do
      Begin
        LStructSize := SizeOf(OpenFileName);
        HWndOwner := ((Sender As TComponent).Owner As TWinControl).Handle;
        LPStrFilter := PChar((Sender As TSaveDialog).Filter);
        {If (Sender As TSaveDialog).FileName <> '' Then
          LPStrFile := PChar((Sender As TSaveDialog).FileName);}
        LPStrFileTitle := PChar((Sender As TSaveDialog).Title);
        LPStrInitialDir := PChar((Sender As TSaveDialog).InitialDir);
        If Sender Is TOpenDialog Then
          Flags := GetFlagsFromOptions((Sender As TOpenDialog).Options);
      End;
      Ret := GetSaveFileName(@OpenFile);
    End;
  End
  Else If Sender Is TFontDialog Then
  Begin
    CF := LPChooseFont(@Sender)^;
    ZeroMemory(@CF, SizeOf(TChooseFont));
    LF.LFFaceName := (Sender As TFontDialog).Font.Name;
    With CF Do
    Begin
      LStructSize := SizeOf(TChooseFont);
      HWndOwner := ((Sender As TComponent).Owner As TWinControl).Handle;
      LPLogFont := @LF;
      Flags := CF_EFFECTS Or CF_FORCEFONTEXIST Or CF_INITTOLOGFONTSTRUCT Or CF_SCREENFONTS;
      //RGBColors := (Sender As TFontDialog).Color;
    End;
    Ret := ChooseFont(@CF);
  End;

  If Ret Then
    (Sender As TCommonDialog).UserChoice := mrOK
  Else
    (Sender As TCommonDialog).UserChoice := mrCancel;

  Assert(False, 'Trace:TWin32Object.CreateCommonDialog - Exit');
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.SetCursor
  Params:  Sender - the control which invoked this method
  Returns: Nothing

  Sets the cursor for a window
  
  WARNING: Sender will be casted to TControl, CLEANUP!
 ------------------------------------------------------------------------------}
Procedure TWin32Object.SetCursor(Sender: TObject);
Var
  Cursor: PChar;
  Res: HCURSOR;
Begin
  Assert(False, 'Trace:TWin32Object.SetCursor - Start');
  Assert(False, Format('Trace:TWin32Object.SetCursor - Sender --> %S', [Sender.ClassName]));
  Assert(False, 'Trace:TWin32Object.SetCursor - Getting the window');
  Assert(False, 'Trace:TWin32Object.SetCursor - Getting the cursor');
  Cursor := MakeIntResource(Integer(TControl(Self).Cursor));
  Assert(False, 'Trace:TWin32Object.SetCursor - Loading the cursor');
  Res := LoadCursor(0, Cursor);
  Assert(False, Format('Trace:Cursor handle --> 0x%X', [Res]));
  Assert(False, 'Trace:TWin32Object.SetCursor - Exit');
End;

{------------------------------------------------------------------------------
  Method: TWin32Object.ResizeChild
  Params:  Sender                   - the object which invoked this function
           Left, Top, Width ,Height - new dimensions for the control
  Returns: Nothing

  Resize a window
 ------------------------------------------------------------------------------}
Procedure TWin32Object.ResizeChild(Sender: TObject; Left, Top, Width, Height: Integer);
Var
  DC: HDC;
  Handle: HWND;
  TM: TEXTMETRICA;
Begin
  Handle := (Sender As TWinControl).Handle;
  If (TControl(Sender).Parent Is TCustomGroupBox) Then
  Begin
    DC := GetDC(Handle);
    GetTextMetrics(DC, TM);
    Top := Top + TM.TMHeight;
    ReleaseDC(Handle, DC);
  End;
  If Handle <> HWND(Nil) Then
    MoveWindow(Handle, Left, Top, Width, Height, True)
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
Type
  PMsg = ^TLMSetControlText;
Begin
  Case PMsg(Data)^.FCompStyle Of
    csStatusBar:
    Begin
      SendMessage(Window, SB_SETTEXT, WParam(PMsg(Data)^.Panel), LParam(LPSTR(PMsg(Data)^.UserData)));
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
Var
  DC: HDC;
Begin
  With Sender Do
  Begin
    If Sender Is TWincontrol Then
    Begin
      With TWincontrol(Sender) Do
      Begin
        // Temphack to set backcolor, till better solution
        If HandleAllocated Then
        Begin
          DC := GetDC(Handle);
          SetBKColor(Handle, Color);
          ReleaseDC(Handle, DC);
        End;
      End;
    End;
  End;
  //NOT USED RIGHT NOW..........CAUSES ALL FORMS TO USE THESE COLORS!!!!!!
End;

{------------------------------------------------------------------------------
  Function: TWin32Object.CreateComponent
  Params:   Sender - object for which to create visual representation
  Returns:  nothing

  Tells Windows to create a control
 ------------------------------------------------------------------------------}
Procedure TWin32Object.CreateComponent(Sender: TObject);
Var
  AccelIndex: Byte;
  AProcess: TProcess;
  Buddy, Handle, Window: HWnd;
  Caption : String;
  CompStyle, I, J, K, Left, Top: Integer;
  DC: HDC;
  Flags: DWord;
  Height, Width: Integer;
  Parent: HWND;
  PStr, StrTemp: PChar;
  R: TRect;
  TCI: TC_ITEM;
Const
  BitsPerPixel: Array[Boolean] Of Cardinal = (3, 1);
Begin
  Assert(False, 'Trace:CreateComponent - Start');
  Assert(False, 'Trace:CreateComponent - Value of Sender is $' + IntToHex(LongInt(Sender), 8));

  Assert(False, 'Trace:CreateComponent - 1');

  Flags := WS_Child Or WS_Visible;
  Assert(False, 'Trace:Setting flags');
  Window := HWND(Nil);
  Assert(False, 'Trace:Setting window');

  If (Sender Is TWinControl) And ((Sender As TWinControl).Parent <> Nil) Then
  Begin
    Assert(False, Format('Trace:TWin32Object.CreateComponent - %S parent --> %S', [Sender.ClassName, TWinControl(Sender).Parent.ClassName]));
    If Not ((Sender As TWinControl).Parent Is TPage) Then
      Parent := (Sender As TWinControl).Parent.Handle
    Else
      Parent :=((Sender As TWinControl).Parent As TPage).Parent.Handle;
    Assert(False, Format('Trace:TWin32Object.CreateComponent - parent handle --> 0x%X', [Parent]));
    
    Assert(False, 'Trace:Setting parent');
  End
  Else
    Parent := 0;

  CompStyle := csNone;
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
    Assert(False, 'Trace:Setting dimentions');
  End
  Else If (Sender Is TMenuItem) Then
  Begin
    Assert(False, 'Trace:[TWin32Object.CreateComponent] - Sender is a menu item');
    Caption := TMenuItem(Sender).Caption;
    Assert(False, Format('Trace:[TWin32Object.CreateComponent] - Caption set to %S', [Caption]));
    CompStyle := TMenuItem(Sender).FCompStyle;
    Assert(False, Format('Trace:[TWin32Object.CreateComponent] - CompStyle set to %S', [CS_To_String(CompStyle)]));
    Handle := TMenuItem(Sender).Handle;
    Assert(False, Format('Trace:[TWin32Object.CreateComponent] - Handle set to %D', [Handle]));
  End
  Else If (Sender Is TMenu) Or (Sender Is TPopupMenu) Then
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
  StrTemp := StrAlloc(Length(Caption) + 1);
  StrPCopy(StrTemp, Caption);
  Assert(False, 'Trace:CreateComponent - Control Style is ' + CS_To_String(CompStyle));

  Case CompStyle Of
    csAlignment:
    Begin
      Assert(False, 'Trace:TODO: Code csAlignment.  If anyone knows how to do this, please do.');
      GetClientRect(Handle, R);
      MoveWindow(Handle, R.Right - Left, R.Bottom - Top, (R.Right - R.Left) - (Left Div 2), (R.Bottom - R.Top) - (Top Div 2), True);
      Window := Handle;
      SetName(Window, StrTemp);
    End;
    csArrow:
    Begin
      Assert(False, 'Trace:TODO: TWin32Object.CreateComponent - Figure out what a csArrow is and code it');
      Assert(False, 'Trace:TWin32Object.CreateComponent - Creating a cursor. This will have to be good enough for now.');
      LoadCursor(HInst(Nil), IDC_SizeWE);
    End;
    csBitBtn:
    Begin
      Window := CreateWindow('BUTTON', Nil, Flags Or BS_BITMAP, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      IntSendMessage3(LM_LOADXPM, Sender, StrTemp);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csButton:
    Begin
      Assert(False, 'Trace:CreateComponent - Creating Button');
      If Not (Sender As TButton).Default Then
        Flags := Flags Or BS_PUSHBUTTON
      Else
        Flags := Flags Or BS_DEFPUSHBUTTON;
      Window := CreateWindow('BUTTON', StrTemp, Flags, Left, Top, Width, Height - 8, Parent, HMENU(Nil), HInstance, Nil);
      If Window <> HWND(Nil) Then
        SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csCalendar:
    Begin
      Assert(False, 'Trace:TODO: TWin32Object.CreateComponent - Code style csCalendar');
      Assert(False, 'Trace:TODO: TWin32Object.CreateComponent (style csCalendar) - Opening the date/time control applet.  This will have to be good enough for now.');
      AProcess := TProcess.Create(TComponent(Sender).Owner);
      AProcess.CommandLine := 'control timedate.cpl';
      AProcess.Options := [poNoConsole];
      AProcess.Execute;
      Window := HWND(AProcess);
      //Window := CreateWindow('CalendarWndClass', StrTemp, Flags, Left, Top, Width, Height, Parent, HMenu(Nil), HInstance, Nil);
      SetName(Window, StrTemp);
      AProcess.Free;
      AProcess := Nil;
    End;
    csCanvas:
    Begin
      Assert(False, 'Trace:TODO: Code TWin32Object.CreateComponent: style csCanvas');
      Window := CreateWindow(ClsName, StrTemp, WS_DLGFRAME Or WS_POPUP Or WS_VISIBLE, Left, Top, Width, Height, HWND(Nil), HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csCheckbox:
    Begin
      Window := CreateWindow('BUTTON', StrTemp, Flags Or BS_AUTOCHECKBOX, Left, Top, Width, Height, Parent, HMenu(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csComboBox:
    Begin
      Window := CreateWindow('COMBOBOX', Nil, Flags Or CBS_DROPDOWN, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csImage:
    Begin
      DC := GetDC(Handle);
      With TImage(Sender).Picture.Bitmap Do
        Window := CreateBitmap(Width, Height, GetDeviceCaps(DC, PLANES), BitsPerPixel[Monochrome], Nil);
      SetOwner(Window, Sender);
      SetName(Window, StrTemp);
      ReleaseDC(Handle, DC);
    End;
    csListBox:
    Begin
      Window := CreateWindow('LISTBOX', Nil, Flags, Left, Top, Width, Width, Parent, HMENU(Nil), HInstance, Nil);
      SendMessage(Window, LB_ADDSTRING, 0, LPARAM(LPCTSTR(StrTemp)));
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csCListBox:
    Begin
      Window := CreateWindow('LISTBOX', Nil, Flags Or LBS_MULTICOLUMN, Left, Top, Width, Width, Parent, HMENU(Nil), HInstance, Nil);
      SendMessage(Window, LB_SETCOLUMNWIDTH, WPARAM((Sender As TCListBox).Width Div ((Sender As TCListBox).ListColumns)), 0);
      SendMessage(Window, LB_ADDSTRING, 0, LPARAM(LPCTSTR(StrTemp)));
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csEdit:
    Begin
      Window := CreateWindowEx(WS_EX_CLIENTEDGE, 'EDIT', StrTemp, Flags Or ES_AUTOHSCROLL, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csColorDialog, csFileDialog, csFontDialog:
    Begin
      CreateCommonDialog(TCommonDialog(Sender));
    End;
    csFixed:
    Begin
      Assert(False, 'Trace:TODO: Figure out what component style csFixed is and code the component.  No component created.');
      Window := CreateWindow(ClsName, StrTemp, WS_OVERLAPPEDWINDOW Or WS_HSCROLL Or WS_VSCROLL, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csFont:
    Begin
      Assert(False, 'Trace:CreateComponent - Creating a font');
      With LPLogFont(@Sender)^ Do
        Window := CreateFont(LFHeight, LFWidth, LFEscapement, LFOrientation, LFWeight, LFItalic, LFUnderLine, LFStrikeOut, LFCharSet, LFOutPrecision, LFClipPrecision, LFQuality, LFPitchAndFamily, LFFaceName);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csForm:
    Begin
      Assert(False, 'Trace:CreateComponent - Creating a Form Window');
//  Not needed since the size is set later
//      If Left = 0 Then
//        Left := CW_USEDEFAULT;
//      If Top = 0 Then
//        Top := CW_USEDEFAULT;
//      If Width = 0 Then
//        Width := CW_USEDEFAULT;
//      If Height = 0 Then
//        Width := CW_USEDEFAULT;
//      Window := CreateWindow(ClsName, StrTemp, WS_OVERLAPPEDWINDOW, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      try
        Window := CreateWindow(ClsName, StrTemp, WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, Parent, HMENU(Nil), HInstance, Nil);
      except
        writeln('Exception occured creating window');
      end;
      If (FMainForm = Nil) And (Application.MainForm = Nil) Then
        FMainForm := TForm(Sender);
      FParentWindow := Window;
      Assert(False, 'Trace:CreateComponent - Form Window Handle Value = $' + IntToHex(Window, 8));
      Assert(False, 'Trace:Creating a Form - SetProp');
      //SetProp(Window, 'Lazarus', Sender);
      SetProp(Window, 'Lazarus', Sender);
      If Window = 0 then
      Begin
        MessageBox(0, 'csForm CreateWindow Failed', nil, mb_Ok);
        Exit;
      End;
      SetName(Window, StrTemp);
      //LazObject := Sender;
    End;
    csMainForm:
    Begin
      Assert(False, 'Trace:CreateComponent - Creating a MainForm for Win32 --------------------------------------');
      PStr := StrAlloc(Length('TForm') + 1);
      StrPCopy(PStr, 'TForm');
      Window := CreateWindow(pStr, ClsName, WS_OverlappedWindow, CW_UseDefault, CW_UseDefault, CW_UseDefault, CW_UseDefault, Parent, HMENU(Nil), HInstance, Nil);
      FParentWindow := Window;
      StrDispose(PStr);
      Assert(False, 'Trace:CreateComponent - MainForm Window Handle Value = $' + IntToHex(Window, 8));
      Assert(False, 'Trace:Creating a Form - MainForm SetProp');
      SetProp(Window, 'Lazarus', Sender);
      If Window = 0 Then
      Begin
        MessageBox(0, 'csMainForm CreateWindow Failed', nil, mb_Ok);
        Exit;
      End;
      SetName(Window, strTemp);
    End;
    csFrame:
    Begin
      Window := CreateWindow('BUTTON', StrTemp, Flags Or BS_GROUPBOX Or BS_TOP, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csHintWindow:
    Begin
      Assert(False, 'Trace:TWin32Object.CreateComponent - Got style csHintWindow');
      Assert(False, 'Trace:TWin32Object.CreateComponent (style csHintWindow) - Creating a window.  TODO: Create a small dialog box for hints');
      TControl(Sender).FCompStyle := csForm;
      IntSendMessage3(LM_CREATE, Sender, Nil);
    End;
    csLabel:
    Begin
      Window := CreateWindow('STATIC', StrTemp, Flags Or SS_LEFT Or SS_SIMPLE, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csListView:
    Begin
      Window := CreateWindow(WC_LISTVIEW, StrTemp, Flags Or LVS_LIST Or LVS_SINGLESEL, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      SetOwner(Window, Sender);
      SetName(Window, StrTemp);
    End;
    csMemo:
    Begin
      Assert(False, 'Trace:TWin32Object.CreateComponent - Creating a MEMO...');
      Flags := Flags Or ES_AUTOHSCROLL Or ES_AUTOVSCROLL Or ES_MULTILINE;
      If (Sender As TMemo).ReadOnly Then
        Flags := Flags Or ES_ReadOnly;
      Case (Sender As TCustomMemo).ScrollBars Of
        ssHorizontal:
          Flags := Flags Or WS_HSCROLL;
        ssVertical:
          Flags := Flags Or WS_VSCROLL;
        ssBoth:
          Flags := Flags Or WS_HSCROLL Or WS_VSCROLL;
      End;
      Window := CreateWindowEx(WS_EX_CLIENTEDGE, 'EDIT', StrTemp, Flags, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csMainMenu, csMenuBar:
    Begin
      Window := CreateMenu;
      FMenu := Window;
      Assert(False, Format('Trace:Main menu owner --> %S', [((Sender As TComponent).Owner As TWinControl).ClassName]));
      //DrawMenuBar(((Sender As TComponent).Owner As TWinControl).Handle);
      With (Sender As TMenu) Do
      Begin
        StrDispose(StrTemp);
        For I := 0 To Items.Count - 1 Do
        Begin
          Assert(False, Format('Trace:TWin32Object.CreateComponent - Menu class: %S, menu-item class: %S, item: %D, Caption: %S', [ClassName, Items[I].ClassName, I, Items[I].Caption]));
          StrTemp := StrAlloc(Length(Items[I].Caption) + 1);
          StrPCopy(StrTemp, Items[I].Caption);
          Items[I].Handle := CreateMenu;
          AppendMenu(Window, MF_POPUP, Items[I].Handle, StrTemp);
          Self.SetProp(Items[I].Handle, 'Lazarus', Items[I]);
          Self.SetName(Items[I].Handle, StrTemp);
          For J := 0 To Items[I].Count - 1 Do
          Begin
            Assert(False, Format('Trace:TWin32Object.CreateComponent - Menu class: %S, menu-item class: %S, item: %D, Caption: %S', [ClassName, Items[I].Items[J].ClassName, J, Items[I].Items[J].Caption]));
            Inc(FControlIndex);
            StrDispose(StrTemp);
            StrTemp := StrAlloc(Length(Items[I].Items[J].Caption) + 1);
            StrPCopy(StrTemp, Items[I].Items[J].Caption);
            Items[I].Items[J].Handle := CreatePopupMenu;
            Windows.AppendMenu(Items[I].Handle, MF_STRING, FControlIndex, StrTemp);
            Self.SetProp(Items[I].Items[J].Handle, 'Lazarus', Items[I].Items[J]);
            Self.SetName(Items[I].Items[J].Handle, StrTemp);
            If Items[I].Items[J].Count > 0 Then
            Begin
              For K := 0 To Items[I].Items[J].Count - 1 Do
              Begin
                Assert(False, Format('Trace:TWin32Object.CreateComponent - Menu class: %S, menu-item class: %S, item: %D, Caption: %S', [ClassName, Items[I].Items[J].Items[K].ClassName, K, Items[I].Items[J].Items[K].Caption]));
                Inc(FControlIndex);
                StrDispose(StrTemp);
                StrTemp := StrAlloc(Length(Items[I].Items[J].Items[K].Caption) + 1);
                StrPCopy(StrTemp, Items[I].Items[J].Items[K].Caption);
                Items[I].Items[J].Items[K].Handle := CreatePopupMenu;
                Windows.AppendMenu(Items[I].Items[J].Handle, MF_STRING, FControlIndex, StrTemp);
                Self.SetProp(Items[I].Items[J].Items[K].Handle, 'Lazarus', Items[I].Items[J].Items[K]);
                Self.SetName(Items[I].Items[J].Items[K].Handle, StrTemp);
              End;
            End;
          End;
          Windows.DrawMenuBar(((Sender As TComponent).Owner As TWinControl).Handle);
          Assert(False, Format('Trace:Menu handle: 0x%X, item handle: 0x%X', [Window, Items[I].Handle]));
        End;
      End;
      Windows.SetMenu(((Sender As TComponent).Owner As TWinControl).Handle, Window);
      Windows.DrawMenuBar(((Sender As TComponent).Owner As TWinControl).Handle);
      Self.SetProp(Window, 'Lazarus', Sender);
      Self.SetName(Window, StrTemp);
    End;
    csMenuItem:
    Begin
      Window := CreateMenu;
      // Do I append the menu here or in AttachMenu?  Setting a property for now.
      //AppendMenu((Sender As TMenu).Parent.Handle, MF_Enabled Or MF_Popup Or MF_String, Window,  StrTemp);
      SetProp(Window, 'MenuCaption', StrTemp);
      AccelIndex := Pos('&', Caption);
      If AccelIndex <> 0 Then
        SetAccelKey(Window, Nil);
      //SetProp(Window, 'Lazarus', Sender);
      //SetName(Window, StrTemp);
    End;
    csNotebook:
    Begin
      Window := CreateWindow(WC_TABCONTROL, Nil, Flags Or WS_CLIPSIBLINGS, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csPanel:
    Begin
      Assert(False, 'Trace:TODO: TWin32Object.CreateComponent - code TPanel');
      TControl(Sender).FCompStyle := csGroupBox;
      CNSendMessage(LM_CREATE, Sender, Nil);
    End;
    csRadioButton:
    Begin
      Window := CreateWindow('BUTTON', StrTemp, Flags Or BS_AUTORADIOBUTTON, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csScrollBar:
    Begin
      Case TScrollBar(Sender).Kind Of
        sbHorizontal:
          Flags := Flags Or SBS_HORZ;
        sbVertical:
          Flags := Flags Or SBS_VERT;
      End;
      Window := CreateWindow('SCROLLBAR', Nil, Flags, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csScrolledWindow:
    Begin
      Assert(False, 'TRACE: CreateComponent - creating a scrolled window');
      Window := CreateWindow(ClsName, strTemp, WS_OVERLAPPEDWINDOW Or WS_HSCROLL Or WS_VSCROLL Or WS_Visible, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, HWND(Nil), HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, strTemp);
    End;
    csSpeedButton:
    Begin
      Assert(False, 'Trace:TODO: Code the speed button control');
      Window := CreateWindow('Button', StrTemp, Flags Or BS_PUSHBUTTON, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csSpinEdit:
    Begin
      Assert(False, 'Trace:TODO: Create a spin edit control.  What is a spin edit contol anyway?');
      //this needs to be created in the actual code because it requires a gtkadjustment Win32Control
      Inc(FControlIndex);
      Buddy := CreateWindowEx(WS_EX_CLIENTEDGE, 'EDIT', StrTemp, Flags Or ES_AUTOHSCROLL, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      Window := CreateUpDownControl(Flags Or WS_Border, Left + Width + 10, Top, 10, Height, Parent, FControlIndex, HInstance, Buddy, 0, 100, Trunc((Sender As TSpinEdit).Value));
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
      Assert(False, 'TRACE:Spin edit control not created');
    End;
    csStatusBar:
    Begin
      Assert(False, 'TRACE:CreateComponent - Creating Status Bar');
      Inc(FControlIndex);
      Window := CreateStatusWindow(Flags, StrTemp, Parent, FControlIndex);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csGTKTable:
    Begin
      // Commented out because of error in 1.0.5 (bug?)
      //Assert(False, 'Trace:TODO: Create GTK Table.  I''m not sure how to do this (or even if this is needed in Win32), but I assume an array (or TList) of records containing the rows and columns and the properties(x, y, width, height, etc) of everything.  If you think you can help, be my guest.');
      MessageBox(GetDesktopWindow, 'csGTKTable expected to be created', Nil, MB_OK);
      //TControl(Sender).FCompStyle := csForm;
      //IntSendMessage3(LM_CREATE, Sender, Nil);
      Assert(False, 'TRACE:GTK Table not created');
    End;
    csToggleBox:
    Begin
      Assert(False, 'TRACE: CreateComponent - Creating toggle box');
      Window := CreateWindow('BUTTON', StrTemp, Flags Or BS_AUTOCHECKBOX Or BS_PUSHLIKE, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csToolBar:
    Begin
      Window := CreateWindow(TOOLBARCLASSNAME, LPSTR(Nil), Flags OR CCS_ADJUSTABLE, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csToolButton:
    Begin
      Window := IntSendMessage3(LM_INSERTTOOLBUTTON, Sender, Pointer((Sender As TToolButton).Index));
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csGroupBox:
    Begin
      Assert(False, 'Trace:TODO: Code csGroupBox.  Is this the same as csFrame?');
      TControl(Sender).FCompStyle := csFrame;
      IntSendMessage3(LM_CREATE, Sender, Nil);
    End;
    // TPage - Notebook page
    csPage:
    Begin
      Assert(False, 'Trace:Create a csPage component.');
      Window := CreateWindow('STATIC', Nil, WS_CHILD, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      ShowWindow(Window, SW_HIDE);
      SetProp(Window, 'Lazarus', Sender);
    End;
    csPopupMenu:
    Begin
      Window := CreatePopupMenu;
      FSubMenu := Window;
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csProgressBar:
    Begin
      Window := CreateWindow(PROGRESS_CLASS, NULL, Flags, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
    csTrackBar:
    Begin
      Assert(False, 'TRACE:CreateComponent - Creating a Track Bar (if we''re lucky)');
      Window := CreateWindow(TRACKBAR_CLASS, StrTemp, Flags, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
      SetProp(Window, 'Lazarus', Sender);
      SetName(Window, StrTemp);
    End;
  End; {Case}

  If (Sender Is TWinControl) Or (CompStyle = csImage) Then 
  Begin
    TWinControl(Sender).Handle := Window;
    If Window <> HWND(Nil) Then
      SetProp(Window, 'Sender', @Sender);
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
        (Sender As TControlCanvas).Handle := Window;
      End
      Else If (Sender Is TFont) Then
      Begin
        Assert(False, 'Trace:CreateComponent - Assigning P to TFont');
        (Sender As TFont).Handle := Window;
      End;
  End;

  SetLCLObject(Window, Sender);

  If Window = HWnd(Nil) Then
  Begin
    SetProp(Window, 'Style', Pointer(GetWindowLong(Window, GWL_Style)));
    SetProp(Window, 'ExStyle', Pointer(GetWindowLong(Window, GWL_ExStyle)));
  End;

  Try
    StrDispose(StrTemp);
  Except
    Assert(False, 'Trace:Warning: Tried to dispose a string that was not allocated');
  End;

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
  SetProp(Window, 'Self', Data);
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
  Handle := ObjectToHWND(Sender);
  If TControl(Sender).FCompStyle = csPage then exit;
  If TControl(Sender).Visible Then
  Begin
    Assert(False, 'Trace: [TWin32Object.ShowHide] Showing the window');
    ShowWindow(Handle, SW_SHOW);
    If (Sender Is TCustomForm) Then
      SetClassLong(Handle, GCL_HIcon, TCustomForm(Sender).GetIconHandle);
  End
  Else
  Begin
    Assert(False, 'TRACE: [TWin32Object.ShowHide] Hiding the window');
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
Procedure TWin32Object.AddNBPage(Notebook: TCustomNotebook; NewPage: TPage; Index: Integer);
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
    // Adjust page size to fit in tabcontrol, no bounds needed.
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
  SendMessage((Parent As TNotebook).Handle, TCM_DELETEITEM, WPARAM(Index), 0);
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
  ST: TSystemTime;
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
        Integer(Data^) := Round(SendMessage(Handle, TBM_GETRANGEMAX, 0, 0) - SendMessage(Handle, TBM_GETRANGEMIN, 0, 0));
      End
      Else
        Integer(Data^) := 0;
    csRadiobutton, csCheckbox:
      If SendMessage(Handle, BM_GETSTATE, 0, 0) = BST_CHECKED Then
        TCheckBoxState(Data^) := cbChecked
      Else If SendMessage(Handle, BM_GETSTATE, 0, 0) = BST_UNCHECKED Then
        TCheckBoxState(Data^) := cbUnChecked;
    csCalendar:
    Begin
      GetLocalTime(ST);
      With PSystemTime(@ST)^ Do
      Begin
        TLMCalendar(Data^).Date := StrToDate(IntToStr(WMonth) + '/' + IntToStr(WDay) + '/' + IntToStr(WYear));
      End;
    End;
    csSpinEdit:
    Begin
      Integer(Data^) := SendMessage(Handle, UDM_GETPOS, 0, 0);
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
  Date: TDateTime;
  Handle: HWnd;
  HTkn: Integer;
  IsNT: Boolean;
  OTP: LUID;
  OTPS: DWord;
  OVI: OSVersionInfo;
  ST: SystemTime;
  TkP: Token_Privileges;
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
      OVI.DWOSVersionInfoSize := SizeOf(OVI);
      GetVersionEx(@OVI);
      IsNT := OVI.DWPlatformID = VER_PLATFORM_WIN32_NT;
      If IsNT Then
      Begin
        MessageBox(Handle, 'Can not set the time on Windows NT without certain priviledges', Nil, MB_OK Or MB_IconInformation);
        TkP.PrivilegeCount := 1;
        PInteger(@TkP.Privileges[0].LUID)^ := StrToInt(SE_SystemTime_Name);
        TkP.Privileges[0].Attributes := SE_Privilege_Enabled;
        OpenProcessToken(GetCurrentProcess, Token_Adjust_Privileges, @HTkn);
        AdjustTokenPrivileges(HTkn, False, @TkP, SizeOf(TkP), @OTP, @OTPS);
      End;
      Date := TLMCalendar(Data^).Date;
      With St Do
      Begin
        WYear := StrToInt(FormatDateTime('yyyy', Date));
        WMonth := StrToInt(FormatDateTime('mm', Date));
        WDay := StrToInt(FormatDateTime('dd', Date));
      End;
      SetLocalTime(ST);
      If IsNT Then
        AdjustTokenPrivileges(HTkn, False, @OTP, OTPS, Nil, Nil);
    End;
    csProgressBar:
      SendMessage(Handle, PBM_SETPOS, WPARAM(Data^), 0);
    csTrackbar:
    Begin
      If Handle = HWnd(Nil) Then
        Exit;
      Assert(False, 'TRACE:Setting the track bar value.');
      SendMessage(Handle, TBM_SETPOS, WPARAM(True), LPARAM(Data^));
    End;
    csRadiobutton, csCheckbox:
    Begin
      If TCheckBoxState(Data^) = cbChecked Then
        SendMessage(Handle, BM_SETCHECK, WParam(BST_CHECKED), 0)
      Else If TCheckboxState(Data^) = cbUnchecked Then
        SendMessage(Handle, BM_SETCHECK, WParam(BST_UNCHECKED), 0)
      Else
        SendMessage(Handle, BM_SETCHECK, WParam(BST_INDETERMINATE), 0);
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
  Style: DWord;
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
        SendMessage(Handle, EM_SETREADONLY, WPARAM(ReadOnly), 0);
        SendMessage(Handle, EM_LIMITTEXT, MaxLength, 0);
      End;
    csListView:
    Begin
      With TCustomListView(Sender) Do
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
    End;
    csProgressBar:
      With (TProgressBar(Sender)) Do
      Begin
        SendMessage(Handle, PBM_SETRANGE, 0, MakeLParam(SendMessage(Handle, PBM_GETRANGE, WPARAM(True), 0), 0));
        SendMessage(Handle, PBM_SETPOS, Position, 0);
        If Smooth Then
          SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) Or PBS_SMOOTH)
        Else
          SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) And Not PBS_SMOOTH);
        Case Orientation Of
          pbVertical:
            SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) Or PBS_VERTICAL);
          pbRightToLeft:
          Begin
            Assert(False, 'TRACE:TRYING to create a right-to-left progress bar');
            SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) And Not WS_EX_LTRREADING);
          End;
          pbTopDown:
          Begin
            Assert(False, 'TRACE: TRYING to create a vertical, top-to-bottom progress bar');
            SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) Or PBS_VERTICAL);
            SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) And Not WS_EX_LTRLEADING);
          End;
          Else { pbHorizontal is default }
          Begin
            SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) And Not PBS_VERTICAL);
            SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) Or WS_EX_LTRLEADING);
          End;
        End;
        If BarShowText Then 
        Begin
          SetWindowText(Handle, StrToPChar((Sender As TControl).Caption));
        End
        Else
          SetWindowText(Handle, Nil);
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
      SendMessage(Handle, UDM_SETPOS, 0, MakeLong(Trunc(TSpinEdit(Sender).Value), 0));
    End;
    csTrackbar: 
      With(TTrackBar(Sender)) Do
      Begin
        SendMessage(Handle, TBM_SETRANGEMAX, WPARAM(True), Max);
        SendMessage(Handle, TBM_SETRANGEMIN, WPARAM(True), Min);
        SendMessage(Handle, TBM_SETPOS, WPARAM(True), Position);
        SendMessage(Handle, TBM_SETLINESIZE, 0, LineSize);
        SendMessage(Handle, TBM_SETPAGESIZE, 0, PageSize);
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
            Style := Style Or SS_LEFT;
          taCenter:
            Style := Style Or SS_CENTER;
          taRightJustify:
            Style := Style Or SS_CENTER;
          Else 
            Style := STYLE Or SS_LEFT; // default, shouldn't happen
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
          Style := Style And Not SS_LEFTNOWORDWRAP
        Else
          Style := Style Or SS_LEFTNOWORDWRAP;

        SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) Or Style);
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
Begin

End;

{------------------------------------------------------------------------------
  Method: TWin32Object.SetName
  Params: Window - The window to which to assign a name
          Value  - The value to assign
  Returns: Nothing

  Assigns a name to a window
 ------------------------------------------------------------------------------}
Procedure TWin32Object.SetName(Window: HWND; Value: PChar);
Begin
  SetProp(Window, 'Name', Value);
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
  SetProp(Window, 'Lazarus', Owner);
  SetProp(Window, 'MsgList', Nil);
End;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{
  $Log$
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
  Keith: TPage can now be a parent

  Revision 1.6  2002/01/17 03:17:44  lazarus
  Keith: Fixed TPage creation

  Revision 1.4  2002/01/05 13:16:09  lazarus
  MG: win32 interface update from Keith Bowes

  Revision 1.3  2001/11/01 22:40:13  lazarus
  MG: applied Keith Bowes win32 interface updates

  Revision 1.2  2001/08/02 12:58:35  lazarus
  MG: win32 interface patch from Keith Bowes

  Revision 1.1  2000/07/13 10:28:31  michael
  + Initial import

 }
 
