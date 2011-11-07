{$MainUnit customdrawnwscontrols.pp}

procedure PrepareCreateWindow(const AWinControl: TWinControl;
  const CreateParams: TCreateParams; out Params: TCreateWindowExParams);
begin
  Fillchar(Params,Sizeof(Params),0);
  with Params do
  begin
    Window := HWND(nil);
    Buddy := HWND(nil);
    WindowTitle := '';
    SubClassWndProc := @WindowProc;

    Flags := CreateParams.Style;
    FlagsEx := CreateParams.ExStyle;

    // Never set the parent of a window to AppHandle,
    // otherwise wince will really try to make it a child
    Parent := CreateParams.WndParent;

    StrCaption := CreateParams.Caption;

    Left := CreateParams.X;
    Top := CreateParams.Y;
    Width := CreateParams.Width;
    Height := CreateParams.Height;

    LCLBoundsToWin32Bounds(AWinControl, Left, Top, Width, Height);
    if AWinControl is TCustomControl then
      if TCustomControl(AWinControl).BorderStyle = bsSingle then
        FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;

    {$IFDEF VerboseSizeMsg}
    Debugln('PrepareCreateWindow ',AWinControl.Name,':',AWinControl.ClassName,
      ' ',dbgs(Left),',',dbgs(Top),',',dbgs(Width),',',dbgs(Height));
    {$ENDIF}
  end;
end;

procedure FinishCreateWindow(const AWinControl: TWinControl; var Params: TCreateWindowExParams;
  const AlternateCreateWindow: boolean);
var
  lhFont: HFONT;
begin
  if not AlternateCreateWindow then
  begin
    with Params do
    begin
      if (Flags and WS_CHILD) <> 0 then
      begin
        // menu handle is also for specifying a control id if this is a child
        MenuHandle := HMENU(AWinControl);
      end else begin
        MenuHandle := HMENU(nil);
      end;

      Window := CreateWindowExW(
       FlagsEx,            // Extra Flags
       pClassName,         // Name of the registered class
       PWideChar(UTF8Decode(WindowTitle)),// Title of the window
       Flags,              // Style of the window
       Left,               // x-position (at beginning)
       Top,                // y-position (at beginning)
       Width,              // window width
       Height,             // window height
       Parent,             // handle to parent or owner window
       MenuHandle,         // handle to menu
       System.HInstance,   // handle to application instance
       nil);               // pointer to window-creation data

      if Window = 0 then
      begin
        raise exception.create('failed to create win32 control, error: '+IntToStr(GetLastError())
         + ' WindowTitle: ' + WindowTitle);
      end;
    end;
    { after creating a child window the following happens:
      1) the previously bottom window is thrown to the top
      2) the created window is added at the bottom
      undo this by throwing them both to the bottom again }
    { not needed anymore, tab order is handled entirely by LCL now
    Windows.SetWindowPos(Windows.GetTopWindow(Parent), HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    Windows.SetWindowPos(Window, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    }
  end;
  with Params do
  begin
    if Window <> HWND(Nil) then
    begin
      // some controls (combobox) immediately send a message upon setting font
      WindowInfo := AllocWindowInfo(Window);
      WindowInfo^.WinControl := AWinControl;
      AWinControl.Handle := Window;
      if SubClassWndProc <> nil then
        WindowInfo^.DefWndProc := Windows.WNDPROC(Windows.SetWindowLongW(
          Window, GWL_WNDPROC, LongInt(SubClassWndProc)));
      {$ifdef MSG_DEBUG}
        DebugLn('Trace:FinishCreateWindow - ', AWinControl.Name,
          ' SubClassWndProc: ', IntToHex(PtrInt(SubClassWndProc), 8),
          ' DefWndProc: ', IntToHex(PtrInt(WindowInfo^.DefWndProc), 8));
      {$endif}
//      if AWinControl.Font.IsDefault then
//        lhFont := WinCEWidgetset.MessageFont
//      else
//        lhFont := AWinControl.Font.Reference.Handle;
//      Windows.SendMessage(Window, WM_SETFONT, lhFont, 0)
    end;
  end;
end;

class procedure TCDWSWinControl.AddControl(const AControl: TControl);
var
  ParentPanelHandle, ParentHandle, ChildHandle: HWND;
begin
  {$ifdef OldToolbar}
  if (AControl.Parent is TToolbar) then
    exit;
  {$endif}

  with TWinControl(AControl) do
  begin
    //DebugLn(Format('Trace:[TCDWSWinControl.AddControl] %S --> Calling Add Child: %S', [Parent.ClassName, ClassName]));
    ParentHandle := Parent.Handle;
    ChildHandle := Handle;
  end;

  //DebugLn('Trace:AddControl - Parent Window Handle is $' + IntToHex(LongInt(ParentHandle), 8));
  //DebugLn('Trace:AddControl - Child Window Handle is $' + IntToHex(LongInt(ChildHandle), 8));
  // handle groupbox exception
  ParentPanelHandle := GetWindowInfo(ChildHandle)^.ParentPanel;
  if ParentPanelHandle <> 0 then
    ChildHandle := ParentPanelHandle;
  SetParent(ChildHandle, ParentHandle);
end;

class function  TCDWSWinControl.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  AText := '';
  Result := false;
end;

class procedure TCDWSWinControl.SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
begin
  RecreateWnd(AWinControl);
end;

class procedure TCDWSWinControl.SetChildZPosition(
  const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer;
  const AChildren: TFPList);
var
  AfterWnd: hWnd;
  n, StopPos: Integer;
  Child: TWinControl;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetChildZPosition')
  then Exit;
  if not WSCheckHandleAllocated(AChild, 'SetChildZPosition (child)')
  then Exit;

  if ANewPos = 0 // bottom
  then AfterWnd := HWND_BOTTOM
  else if ANewPos >= AChildren.Count - 1
  then AfterWnd := HWND_TOP
  else begin
    // Search for the first child above us with a handle
    // the child list is reversed form the windows order.
    // So the first window is the top window and is the last child
    // if we don't find a allocated handle then we are effectively not moved
    AfterWnd := 0;
    if AOldPos > ANewPos
    then StopPos := AOldPos              // The child is moved to the bottom, oldpos is on top of it
    else StopPos := AChildren.Count - 1; // the child is moved to the top

    for n := ANewPos + 1 to StopPos do
    begin
      Child := TWinControl(AChildren[n]);
      if Child.HandleAllocated
      then begin
        AfterWnd := Child.Handle;
        Break;
      end;
    end;

    if AfterWnd = 0 then Exit; // nothing to do
  end;
  Windows.SetWindowPos(AChild.Handle, AfterWnd, 0, 0, 0, 0,
    SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOOWNERZORDER or
    SWP_NOSIZE or SWP_NOSENDCHANGING);
end;

{------------------------------------------------------------------------------
  Method:  SetBounds
  Params:  AWinControl                  - the object which invoked this function
           ALeft, ATop, AWidth, AHeight - new dimensions for the control
  Pre:     AWinControl.HandleAllocated
  Returns: Nothing

  Resize a window
 ------------------------------------------------------------------------------}
class procedure TCDWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  IntfLeft, IntfTop, IntfWidth, IntfHeight: integer;
  suppressMove: boolean;
begin
  IntfLeft := ALeft; IntfTop := ATop;
  IntfWidth := AWidth; IntfHeight := AHeight;
  LCLBoundsToWin32Bounds(AWinControl, IntfLeft, IntfTop, IntfWidth, IntfHeight);
  {$IFDEF VerboseSizeMsg}
  Debugln('TCDWSWinControl.ResizeWindow A ',AWinControl.Name,':',AWinControl.ClassName,
    ' LCL=',dbgs(ALeft),',',dbgs(ATop),',',dbgs(AWidth)+','+dbgs(AHeight),
    ' Win32=',dbgs(IntfLeft)+','+dbgs(IntfTop)+','+dbgs(IntfWidth),',',dbgs(IntfHeight),
    '');
  {$ENDIF}
  suppressMove := false;
  AdaptBounds(AWinControl, IntfLeft, IntfTop, IntfWidth, IntfHeight, suppressMove);
  // Some controls, like spins, may set suppressMove in AdaptBounds
  if not suppressMove then
    MoveWindow(AWinControl.Handle, IntfLeft, IntfTop, IntfWidth, IntfHeight, true);

  LCLControlSizeNeedsUpdate(AWinControl, false);
end;

class procedure TCDWSWinControl.SetColor(const AWinControl: TWinControl);
begin
  // TODO: to be implemented, had no implementation in LM_SETCOLOR message
end;

class procedure TCDWSWinControl.SetFont(const AWinControl: TWinControl; const AFont: TFont);
begin
  Windows.SendMessage(AWinControl.Handle, WM_SETFONT, Windows.WParam(AFont.Reference.Handle), 1);
end;

class procedure TCDWSWinControl.SetText(const AWinControl: TWinControl; const AText: string);
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText') then Exit;

  Windows.SetWindowTextW(AWinControl.Handle, PWideChar(UTF8Decode(AText)));
end;

class procedure TCDWSWinControl.ConstraintsChange(const AWinControl: TWinControl);
begin
  // TODO: implement me!
end;

class function TCDWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  {$ifdef VerboseWinCE}
  DebugLn(' TWinCEWSWinControl.CreateHandle ');
  {$endif}
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ClsName;
    WindowTitle := StrCaption;
    SubClassWndProc := nil;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class procedure TCDWSWinControl.DestroyHandle(const AWinControl: TWinControl);
var
  Handle: HWND;
begin
  Handle := AWinControl.Handle;
  DestroyWindow(Handle);
end;

class procedure TCDWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
  // lpRect = nil updates entire client area of window
  InvalidateRect(AWinControl.Handle, nil, true);
end;

class procedure TCDWSWinControl.ShowHide(const AWinControl: TWinControl);
var
  Handle: HWND;
//  ParentPanel: HWND;
  Flags: dword;
begin
  //if (TControl(Sender).FCompStyle = csPage) or (TControl(Sender).FCompStyle = csToolButton) then exit;
  Handle := ObjectToHWND(AWinControl);
//  ParentPanel := GetWindowInfo(Handle)^.ParentPanel;
//  if ParentPanel <> 0 then
//    Handle := ParentPanel;
  if AWinControl.HandleObjectShouldBeVisible then
  begin
    //DebugLn('Trace: [TWinCEWidgetSet.ShowHide] Showing the window');
    if AWinControl.FCompStyle = csHintWindow then
    begin
      Windows.SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
    end else begin
      Flags := SW_SHOW;
      if (AWinControl is TCustomForm) and
        (Application.ApplicationType = atDesktop) then
        case TCustomForm(AWinControl).WindowState of
          wsMaximized: Flags := SW_SHOWMAXIMIZED;
          wsMinimized: Flags := SW_SHOWMINIMIZED;
        end;
      Windows.ShowWindow(Handle, Flags);
      { ShowWindow does not send WM_SHOWWINDOW when creating overlapped maximized window }
      { TODO: multiple WM_SHOWWINDOW when maximizing after initial show? }
      if Flags = SW_SHOWMAXIMIZED then
        Windows.SendMessage(Handle, WM_SHOWWINDOW, 1, 0);
    end;
    if (AWinControl is TCustomForm) then
    begin
      if TCustomForm(AWinControl).BorderStyle <> bsDialog then
      begin
        SetClassLong(Handle, GCL_HICONSM, LONG(TCustomForm(AWinControl).SmallIconHandle));
        SetClassLong(Handle, GCL_HICON, LONG(TCustomForm(AWinControl).BigIconHandle));
      end
      else
      begin
        SetClassLong(Handle, GCL_HICONSM, 0);
        SetClassLong(Handle, GCL_HICON, 0);
      end;
    end;
  end
  else
  begin
    //DebugLn('TRACE: [TWinCEWidgetSet.ShowHide] Hiding the window');
    ShowWindow(Handle, SW_HIDE);
  end;
end;

