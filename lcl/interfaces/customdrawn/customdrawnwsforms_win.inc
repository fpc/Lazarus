{$MainForm customdrawnwsforms.pp}

type
  TWinControlAccess = class(TWinControl)
  end;

  TNCCreateParams = record
    WinControl: TWinControl;
    DefWndProc: WNDPROC;
    Handled: Boolean;
  end;
  PNCCreateParams = ^TNCCreateParams;

{ TCDWSCustomForm }

class function TCDWSCustomForm.CalcBorderIconsFlags(const AForm: TCustomForm): dword;
var
  BorderIcons: TBorderIcons;
begin
  Result := 0;
  BorderIcons := AForm.BorderIcons;
  if (biSystemMenu in BorderIcons) or (csDesigning in AForm.ComponentState) then
    Result := Result or WS_SYSMENU;
  if GetDesigningBorderStyle(AForm) in [bsNone, bsSingle, bsSizeable] then
  begin
    if biMinimize in BorderIcons then
      Result := Result or WS_MINIMIZEBOX;
    if biMaximize in BorderIcons then
      Result := Result or WS_MAXIMIZEBOX;
  end;
end;

class function TCDWSCustomForm.CalcBorderIconsFlagsEx(const AForm: TCustomForm): DWORD;
var
  BorderIcons: TBorderIcons;
begin
  Result := 0;
  BorderIcons := AForm.BorderIcons;
  if GetDesigningBorderStyle(AForm) in [bsSingle, bsSizeable, bsDialog] then
  begin
    if biHelp in BorderIcons then
      Result := Result or WS_EX_CONTEXTHELP;
  end;
end;

class procedure TCDWSCustomForm.CalcFormWindowFlags(const AForm: TCustomForm;
  var Flags, FlagsEx: dword);
begin
  // clear all styles which can be set by border style and icons
  Flags := Flags and not (WS_POPUP or WS_BORDER or WS_CAPTION or WS_THICKFRAME or
    WS_DLGFRAME or WS_MINIMIZEBOX or WS_MAXIMIZEBOX or WS_SYSMENU);
  FlagsEx := FlagsEx and not (WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE or
    WS_EX_TOOLWINDOW or WS_EX_CONTEXTHELP);
  // set border style flags
  Flags := Flags or CalcBorderStyleFlags(AForm);
  FlagsEx := FlagsEx or CalcBorderStyleFlagsEx(AForm);
  if (AForm.FormStyle in fsAllStayOnTop) and not (csDesigning in AForm.ComponentState) then
    FlagsEx := FlagsEx or WS_EX_TOPMOST;
  Flags := Flags or CalcBorderIconsFlags(AForm);
  FlagsEx := FlagsEx or CalcBorderIconsFlagsEx(AForm);
end;

{class procedure TCDWSCustomForm.CalculateDialogPosition(var Params: TCreateWindowExParams;
  Bounds: TRect; lForm: TCustomForm);
begin
  if lForm.Position in [poDefault, poDefaultPosOnly] then
  begin
    Params.Left := CW_USEDEFAULT;
    Params.Top := CW_USEDEFAULT;
  end
  else
  begin
    Params.Left := Bounds.Left;
    Params.Top := Bounds.Top;
  end;
  if lForm.Position in [poDefault, poDefaultSizeOnly] then
  begin
    Params.Width := CW_USEDEFAULT;
    Params.Height := CW_USEDEFAULT;
  end
  else
  begin
    Params.Width := Bounds.Right - Bounds.Left;
    Params.Height := Bounds.Bottom - Bounds.Top;
  end;
end;}

class function TCDWSCustomForm.GetDesigningBorderStyle(const AForm: TCustomForm): TFormBorderStyle;
begin
  if csDesigning in AForm.ComponentState then
    Result := bsSizeable
  else
    Result := AForm.BorderStyle;
end;

class function TCDWSCustomForm.CalcBorderStyleFlags(const AForm: TCustomForm): DWORD;
begin
  Result := WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  case GetDesigningBorderStyle(AForm) of
    bsSizeable, bsSizeToolWin:
      Result := Result or (WS_OVERLAPPED or WS_THICKFRAME or WS_CAPTION);
    bsSingle, bsToolWindow:
      Result := Result or (WS_OVERLAPPED or WS_BORDER or WS_CAPTION);
    bsDialog:
      Result := Result or (WS_POPUP or WS_BORDER or WS_CAPTION);
    bsNone:
      if (AForm.Parent = nil) and (AForm.ParentWindow = 0) then
        Result := Result or WS_POPUP;
  end;
end;

class function TCDWSCustomForm.CalcBorderStyleFlagsEx(const AForm: TCustomForm): DWORD;
begin
  Result := 0;
  case GetDesigningBorderStyle(AForm) of
    bsDialog:
      Result := WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
    bsToolWindow, bsSizeToolWin:
      Result := WS_EX_TOOLWINDOW;
  end;
end;

class procedure TCDWSCustomForm.AdjustFormBounds(const AForm: TCustomForm; out SizeRect: TRect);
begin
  // the LCL defines the size of a form without border, win32 with.
  // -> adjust size according to BorderStyle
  SizeRect := AForm.BoundsRect;
  Windows.AdjustWindowRectEx(@SizeRect, CalcBorderStyleFlags(AForm) or CalcBorderIconsFlags(AForm),
    False, CalcBorderStyleFlagsEx(AForm) or CalcBorderIconsFlagsEx(AForm));
end;

class procedure TCDWSCustomForm.WSWinControl_SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
{$ifndef WinCE}
var
  IntfLeft, IntfTop, IntfWidth, IntfHeight: integer;
  suppressMove: boolean;
  WindowPlacement: TWINDOWPLACEMENT;
  lWindowInfo: TWindowInfo;
  lHandle: HWND;
begin
  IntfLeft := ALeft;
  IntfTop := ATop;
  IntfWidth := AWidth;
  IntfHeight := AHeight;
  LCLBoundsToWin32Bounds(AWinControl, IntfLeft, IntfTop);
  {$IFDEF VerboseCDForms}
  DebugLn('[TCDWSCustomForm.WSWinControl_SetBounds] A ', dbgsName(AWinControl),
    ' LCL=',Format('%d, %d, %d, %d', [ALeft,ATop,AWidth,AHeight]),
    ' Win32=',Format('%d, %d, %d, %d', [IntfLeft,IntfTop,IntfWidth,IntfHeight])
    );
  {$ENDIF}
  suppressMove := False;
  AdaptBounds(AWinControl, IntfLeft, IntfTop, IntfWidth, IntfHeight, suppressMove);
  if not suppressMove then
  begin
    lWindowInfo := TWindowInfo(AWinControl.Handle);
    lHandle := lWindowInfo.NativeHandle;
    WindowPlacement.length := SizeOf(WindowPlacement);
    if Windows.IsIconic(lHandle) and
      Windows.GetWindowPlacement(lHandle, @WindowPlacement) then
    begin
      WindowPlacement.rcNormalPosition := Bounds(IntfLeft, IntfTop, IntfWidth, IntfHeight);
      Windows.SetWindowPlacement(lHandle, @WindowPlacement);
    end
    else
      Windows.SetWindowPos(lHandle, 0,
        IntfLeft, IntfTop, IntfWidth, IntfHeight, SWP_NOZORDER or SWP_NOACTIVATE);
  end;
  LCLControlSizeNeedsUpdate(AWinControl, True);
end;
{$else}
var
  IntfLeft, IntfTop, IntfWidth, IntfHeight: integer;
  suppressMove: boolean;
begin
  IntfLeft := ALeft; IntfTop := ATop;
  IntfWidth := AWidth; IntfHeight := AHeight;
  LCLBoundsToWin32Bounds(AWinControl, IntfLeft, IntfTop);
  {$IFDEF VerboseSizeMsg}
  Debugln('TWinCEWSWinControl.ResizeWindow A ',AWinControl.Name,':',AWinControl.ClassName,
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
{$endif}

class function TCDWSCustomForm.DoCreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
end;

class procedure TCDWSCustomForm.DoShowHide(const AWinControl: TWinControl);
begin
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomForm.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Windows CE Form, initializes it according to it´s properties and shows it
 ------------------------------------------------------------------------------}
class function TCDWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  lForm: TCustomForm absolute AWinControl;
  SystemMenu: HMenu;
  // Create Params
  Parent, Window: HWND;
  Left, Top, Height, Width: integer;
  WindowInfo, BuddyWindowInfo: TWindowInfo;
  MenuHandle: HMENU;
  Flags, FlagsEx: dword;
  SubClassWndProc: pointer;
  WindowTitle: widestring;
  pClassName: PWideChar;
  //
  NCCreateParams: TNCCreateParams;
  AErrorCode: DWORD;
  //
  BorderStyle: TFormBorderStyle;
  WR: Windows.RECT;
  lWinBounds, lOldLCLBounds, lNewLCLBounds: TRect;
{$ifdef WinCE}
begin
  {$ifdef VerboseWinCE}
  DebugLn('TWinCEWSCustomForm.CreateHandle');
  {$endif}
  NCCreateParams.DefWndProc := nil;
  NCCreateParams.WinControl := AWinControl;
  NCCreateParams.Handled := False;

  // general initialization of Params

  //Fillchar(Params,Sizeof(Params),0);
  Window := HWND(nil);
  WindowTitle := UTF8ToUTF16(AParams.Caption);
  //SubClassWndProc := @WindowProc;

  Flags := AParams.Style;
  FlagsEx := AParams.ExStyle;

  // Never set the parent of a window to AppHandle,
  // otherwise wince will really try to make it a child
  Parent := AParams.WndParent;

  Left := AParams.X;
  Top := AParams.Y;
  Width := AParams.Width;
  Height := AParams.Height;

  LCLBoundsToWin32Bounds(AWinControl, Left, Top);
  //    if AWinControl is TCustomControl then
  //      if TCustomControl(AWinControl).BorderStyle = bsSingle then
  //        FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
  //    SetStdBiDiModeParams(AWinControl, Params);

  // customization of Params
  // Different from win32
  SubClassWndProc := nil; // Otherwise crash in wince, works in win32
  BorderStyle := TCustomForm(AWinControl).BorderStyle;

  // Same as in win32
  CalcFormWindowFlags(lForm, Flags, FlagsEx);
  pClassName := @ClsName;
  WindowTitle := UTF8ToUTF16(AParams.Caption);

  // Get the difference between the client and window sizes
  lWinBounds := lForm.BoundsRect;
  Windows.AdjustWindowRectEx(@lWinBounds, Flags, false, FlagsEx);

  if Application.ApplicationType in [atPDA, atKeyPadDevice, atDefault] then
  begin
    // Gets the work area
    Windows.SystemParametersInfo(SPI_GETWORKAREA, 0, @WR, 0);

    { The position and size of common windows is ignored on PDA mode,
      and a position and size that covers the whole workarea excluding
      the menu is used. The Workarea size automatically excludes the
      Taskbar.

      Simply using CM_USEDEFAULT produces a too large Height, which
      covers the menus. So the workarea size is detected (which ignores
      the Taskbar).

      In some devices subtracting the menu size seams to work better, but
      others, if no menu is present, it's a big problem.
    }
    if (BorderStyle <> bsDialog) and (BorderStyle <> bsNone) then
    begin
      Left := WR.Left;
      Top := WR.Top;
      Height := WR.Bottom - WR.Top;
      Width := WR.Right - WR.Left;

      // Update the position of the window for the LCL
      AWinControl.BoundsRect := Bounds(
        Left, Top, Width, Height);
    end
    else if (BorderStyle = bsDialog) then
    {
      For dialogs, the window is put in the middle of the screen.

      On normal dialogs we need to take into consideration the size of
      the window decoration.

      For the Top and Left coordinates, using CM_USEDEFAULT produces
      a wrong and bad result. Using the Workarea rectagle works fine
      for most devices, but not all, so we put the dialog in the center.
    }
    begin
      Top := WR.Top + (WR.Bottom - WR.Top) div 2
        - (lWinBounds.Bottom - lWinBounds.Top) div 2;
      Left := WR.Left + (WR.Right - WR.Left) div 2
        - (lWinBounds.Right - lWinBounds.Left) div 2;
      Height := lWinBounds.Bottom - lWinBounds.Top;
      Width := lWinBounds.Right - lWinBounds.Left;

      // Update the position of the window for the LCL
      lOldLCLBounds := lForm.BoundsRect;
      lNewLCLBounds.Left := Left - (lWinBounds.Left - lOldLCLBounds.Left);
      lNewLCLBounds.Top := Top - (lWinBounds.Top - lOldLCLBounds.Top);
      lNewLCLBounds.Right := Left + Width
        - (lWinBounds.Right - lOldLCLBounds.Right);
      lNewLCLBounds.Bottom := Top + Height
        - (lWinBounds.Bottom - lOldLCLBounds.Bottom);
      AWinControl.BoundsRect := lNewLCLBounds;
    end
    else { BorderStyle = bsNone }
    { On borderless Windows we allow the user full control of the
      window position
    }
    begin
      //CalculateDialogPosition(Params, lWinBounds, lForm);
    end;
  end
  else
  begin
    { On Desktop mode we need to take into consideration the size of
      the window decoration }
    //CalculateDialogPosition(Params, lWinBounds, lForm);
  end;

  // create window
  Window := CreateWindowExW(FlagsEx, pClassName,
    PWideChar(WindowTitle), Flags,
    Left, Top, Width, Height, Parent, 0, HInstance, @NCCreateParams);

  Result := Window;
end;
{$else}
begin
  {$ifdef VerboseCDForms}
  DebugLn(Format(':>[TCDWSCustomForm.CreateHandle] AWincontrol=%x left=%d Top=%d'
    + ' Width=%d Height=%d', [PtrInt(AWincontrol), AWinControl.Top, AWinControl.Left,
    AParams.Width, AParams.Height]));
  {$endif}

  NCCreateParams.DefWndProc := nil;
  NCCreateParams.WinControl := AWinControl;
  NCCreateParams.Handled := False;

  // general initialization of Params

  //Fillchar(Params,Sizeof(Params),0);
  Window := HWND(nil);
  WindowTitle := UTF8ToUTF16(AParams.Caption);
  //SubClassWndProc := @WindowProc;

  Flags := AParams.Style;
  FlagsEx := AParams.ExStyle;

  // Never set the parent of a window to AppHandle,
  // otherwise wince will really try to make it a child
  Parent := AParams.WndParent;

  Left := AParams.X;
  Top := AParams.Y;
  Width := AParams.Width;
  Height := AParams.Height;

  LCLBoundsToWin32Bounds(AWinControl, Left, Top);
  //    if AWinControl is TCustomControl then
  //      if TCustomControl(AWinControl).BorderStyle = bsSingle then
  //        FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
  //    SetStdBiDiModeParams(AWinControl, Params);

  // customization of Params
  if (Parent = 0) then
  begin
    if not Application.MainFormOnTaskBar then
      Parent := CDWidgetSet.AppHandle
    else
    if (AWinControl <> Application.MainForm) then
    begin
      if Assigned(Application.MainForm) and Application.MainForm.HandleAllocated then
        Parent := Application.MainFormHandle
      else
        Parent := CDWidgetSet.AppHandle;
    end;
  end;
  CalcFormWindowFlags(lForm, Flags, FlagsEx);
  pClassName := @ClsName[0];
  AdjustFormBounds(lForm, lWinBounds);
  if (lForm.Position in [poDefault, poDefaultPosOnly]) and not (csDesigning in lForm.ComponentState) then
  begin
    Left := CW_USEDEFAULT;
    Top := CW_USEDEFAULT;
  end
  else
  begin
    Left := lWinBounds.Left;
    Top := lWinBounds.Top;
  end;
  if (lForm.Position in [poDefault, poDefaultSizeOnly]) and not (csDesigning in lForm.ComponentState) then
  begin
    Width := CW_USEDEFAULT;
    Height := CW_USEDEFAULT;
  end
  else
  begin
    Width := lWinBounds.Right - lWinBounds.Left;
    Height := lWinBounds.Bottom - lWinBounds.Top;
  end;
  //SubClassWndProc := @CustomFormWndProc;
  if not (csDesigning in lForm.ComponentState) and lForm.AlphaBlend
  and ((WindowsVersion >= wv8) or (Parent = 0)) then
    FlagsEx := FlagsEx or WS_EX_LAYERED;

  //SetStdBiDiModeParams(AWinControl, Params);
  // create window

  Window := CreateWindowExW(FlagsEx, pClassName,
    PWideChar(WindowTitle), Flags,
    Left, Top, Width, Height, Parent, 0, HInstance, @NCCreateParams);

  if Window = 0 then
  begin
    AErrorCode := GetLastError;
    DebugLn(['Failed to create win32 control, error: ', AErrorCode, ' : ', GetLastErrorText(AErrorCode)]);
    raise Exception.Create('Failed to create win32 control, error: ' + IntToStr(AErrorCode) + ' : ' + GetLastErrorText(AErrorCode));
  end;
  WindowInfo := TWindowInfo.Create;
  WindowInfo.LCLForm := TCustomForm(AWinControl);
  WindowInfo.NativeHandle := Window;

  AddFormWithCDHandle(WindowInfo);
  AWinControl.Handle := HWND(WindowInfo);
  Result := HWND(WindowInfo);

  // remove system menu items for bsDialog
  if (lForm.BorderStyle = bsDialog) and not (csDesigning in lForm.ComponentState) then
  begin
    SystemMenu := GetSystemMenu(Window, False);
    DeleteMenu(SystemMenu, SC_RESTORE, MF_BYCOMMAND);
    DeleteMenu(SystemMenu, SC_SIZE, MF_BYCOMMAND);
    DeleteMenu(SystemMenu, SC_MINIMIZE, MF_BYCOMMAND);
    DeleteMenu(SystemMenu, SC_MAXIMIZE, MF_BYCOMMAND);
    DeleteMenu(SystemMenu, 1, MF_BYPOSITION); // remove the separator between move and close
  end;

  // Beginning with Windows 2000 the UI in an application may hide focus
  // rectangles and accelerator key indication. According to msdn we need to
  // initialize all root windows with this message
  if WindowsVersion >= wv2000 then
    Windows.SendMessage(Window, WM_CHANGEUISTATE,
      MakeWParam(UIS_INITIALIZE, UISF_HIDEFOCUS or UISF_HIDEACCEL), 0);

  {$ifdef VerboseCDForms}
  DebugLn(Format(':<[TCDWSCustomForm.CreateHandle] Result=%x',
    [PtrInt(Result)]));
  {$endif}
end;
{$endif}

class procedure TCDWSCustomForm.DestroyHandle(const AWinControl: TWinControl);
begin
end;

class procedure TCDWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
 const ABorderIcons: TBorderIcons);
var
  lWindowInfo: TWindowInfo;
  lHandle: HWND;
begin
  lWindowInfo := TWindowInfo(AForm.Handle);
  lHandle := lWindowInfo.NativeHandle;
  UpdateWindowStyle(lHandle, CalcBorderIconsFlags(AForm),
    WS_SYSMENU or WS_MINIMIZEBOX or WS_MAXIMIZEBOX);
end;

class procedure TCDWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
          const AFormBorderStyle: TFormBorderStyle);
begin
  RecreateWnd(AForm);
end;

class procedure TCDWSCustomForm.SetBounds(const AWinControl: TWinControl;
    const ALeft, ATop, AWidth, AHeight: Integer);
var
  AForm: TCustomForm absolute AWinControl;
  CurRect, SizeRect: Windows.RECT;
  L, T, W, H: Integer;
  lWindowInfo: TWindowInfo;
  lHandle: HWND;
begin
  {$ifdef VerboseCDForms}
  DebugLn(Format('[TCDWSCustomForm.SetBounds] AWinControl=%x'
    + ' ALeft=%d ATop=%d AWidth=%d AHeight=%d',
    [PtrInt(AWinControl), ALeft, ATop, AWidth, AHeight]));
  {$endif}
  // the LCL defines the size of a form without border, win32 with.
  // -> adjust size according to BorderStyle
  SizeRect := Bounds(ALeft, ATop, AWidth, AHeight);

  lWindowInfo := TWindowInfo(AWinControl.Handle);
  lHandle := lWindowInfo.NativeHandle;

  Windows.AdjustWindowRectEx(@SizeRect, CalcBorderStyleFlags(AForm) or CalcBorderIconsFlags(AForm),
    False, CalcBorderStyleFlagsEx(AForm) or CalcBorderIconsFlagsEx(AForm));


  L := ALeft;
  T := ATop;
  W := SizeRect.Right - SizeRect.Left;
  H := SizeRect.Bottom - SizeRect.Top;

  // we are calling setbounds in TWinControl.Initialize
  // if position is default it will be changed to designed. We do not want this.
  if wcfInitializing in TWinControlAccess(AWinControl).FWinControlFlags then
  begin
    if Windows.GetWindowRect(lHandle, CurRect) then
    begin
      if AForm.Position in [poDefault, poDefaultPosOnly] then
      begin
        L := CurRect.Left;
        T := CurRect.Top;
      end;

      if AForm.Position in [poDefault, poDefaultSizeOnly] then
      begin
        W := CurRect.Right - CurRect.Left;
        H := CurRect.Bottom - CurRect.Top;
      end;
    end;
  end;

  // rect adjusted, pass to inherited to do real work
  WSWinControl_SetBounds(AWinControl, L, T, W, H);
end;
(*var
  SizeRect: Windows.RECT;
  BorderStyle: TFormBorderStyle;
  WR: Windows.RECT;
begin
  { User selected LCL window size }
  SizeRect.Top := ATop;
  SizeRect.Left := ALeft;
  SizeRect.Bottom := ATop + AHeight;
  SizeRect.Right := ALeft + AWidth;

  BorderStyle := TCustomForm(AWinControl).BorderStyle;

  { Verifies if the size should be overriden, acording to the ApplicationType }
  if (Application.ApplicationType in [atPDA, atKeyPadDevice, atDefault]) then
  begin
    { We should never move forms which are in full-screen mode }
    if (BorderStyle <> bsDialog) and (BorderStyle <> bsNone) then Exit;

    { For dialogs, the window is put in the middle of the screen. }
    if (BorderStyle = bsDialog) then
    begin
      Windows.SystemParametersInfo(SPI_GETWORKAREA, 0, @WR, 0);
      SizeRect.Top := WR.Top + (WR.Bottom - WR.Top) div 2
          - AHeight div 2;
      SizeRect.Left := WR.Left + (WR.Right - WR.Left) div 2
          - AWidth div 2;
      SizeRect.Bottom := SizeRect.Top + AHeight;
      SizeRect.Right := SizeRect.Left + AWidth;
    end;
    { On borderless Windows we allow the user full control of the window position }
  end;

  { the LCL defines the size of a form without border, CDapi with.
    -> adjust size according to BorderStyle
    Must be done after setting sizeRect }
  Windows.AdjustWindowRectEx(@SizeRect, BorderStyleToWinAPIFlags(
      BorderStyle), false, BorderStyleToWinAPIFlagsEx(TCustomForm(AWinControl), BorderStyle));

  // rect adjusted, pass to inherited to do real work
  TCDWSWinControl.SetBounds(AWinControl, SizeRect.Left, SizeRect.Top,
    SizeRect.Right - SizeRect.Left, SizeRect.Bottom - SizeRect.Top);

  {$IFDEF VerboseSizeMsg}
  DebugLn(
    Format('[TCDWSCustomForm.SetBounds]: Name:%s Request x:%d y:%d w:%d h:%d'
    + ' SizeRect x:%d y:%d w:%d h:%d',
    [AWinControl.Name, ALeft, ATop, AWidth, AHeight,
    SizeRect.Left, SizeRect.Top,
    SizeRect.Right - SizeRect.Left, SizeRect.Bottom - SizeRect.Top]));
  {$ENDIF}
end;   *)

class procedure TCDWSCustomForm.SetIcon(const AForm: TCustomForm; const Small, Big: HICON);
var
  lWindowInfo: TWindowInfo;
  lHandle: HWND;
begin
  if not WSCheckHandleAllocated(AForm, 'SetIcon') then
    Exit;
  lWindowInfo := TWindowInfo(AForm.Handle);
  lHandle := lWindowInfo.NativeHandle;
  Windows.SendMessage(lHandle, WM_SETICON, ICON_SMALL, LPARAM(Small));
  Windows.SendMessage(lHandle, WM_SETICON, ICON_BIG, LPARAM(Big));
end;

class procedure TCDWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
var
  OldStyle, NewStyle: DWord;
  Visible, Active: Boolean;
begin
  if not WSCheckHandleAllocated(AForm, 'SetShowInTaskbar') then
    Exit;
  if Assigned(Application) and (AForm = Application.MainForm) then
    Exit;

  OldStyle := GetWindowLong(AForm.Handle, GWL_EXSTYLE);
  NewStyle := OldStyle;
  if AValue = stAlways then
    NewStyle := NewStyle or WS_EX_APPWINDOW
  else
    NewStyle := NewStyle and not WS_EX_APPWINDOW;
  if OldStyle = NewStyle then exit;

  // to apply this changes we need either to hide window or recreate it. Hide is
  // less difficult
  Visible := IsWindowVisible(AForm.Handle);
  Active := GetForegroundWindow = AForm.Handle;
  if Visible then
    ShowWindow(AForm.Handle, SW_HIDE);

  SetWindowLong(AForm.Handle, GWL_EXSTYLE, NewStyle);

  // now we need to restore window visibility with saving focus
  if Visible then
    if Active then
      ShowWindow(AForm.Handle, SW_SHOW)
    else
      ShowWindow(AForm.Handle, SW_SHOWNA);
end;

class procedure TCDWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
var
  lWindowInfo: TWindowInfo;
  lHandle: HWND;
begin
  lWindowInfo := TWindowInfo(ACustomForm.Handle);
  lHandle := lWindowInfo.NativeHandle;
  Windows.ShowWindow(lHandle, SW_SHOW);
  Windows.BringWindowToTop(lHandle);
end;

class procedure TCDWSCustomForm.ShowHide(const AWinControl: TWinControl);
const
  VisibilityToFlag: array[Boolean] of UINT = (SWP_HIDEWINDOW, SWP_SHOWWINDOW);
var
  lWindowInfo: TWindowInfo;
  lHandle: HWND;
begin
  lWindowInfo := TWindowInfo(AWinControl.Handle);
  lHandle := lWindowInfo.NativeHandle;
  Windows.SetWindowPos(lHandle, 0, 0, 0, 0, 0,
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or VisibilityToFlag[AWinControl.HandleObjectShouldBeVisible])
end;

class function TCDWSCustomForm.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  AText := '';
end;

class function TCDWSCustomForm.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  S: String;
begin
  Result := GetText(AWinControl, S);
  if Result
  then ALength := Length(S);
end;

class procedure TCDWSCustomForm.SetText(const AWinControl: TWinControl; const AText: String);
var
  lWindowInfo: TWindowInfo;
  lHandle: HWND;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText') then Exit;
  lWindowInfo := TWindowInfo(AWinControl.Handle);
  lHandle := lWindowInfo.NativeHandle;

  Windows.SetWindowTextW(lHandle, PWideChar(UTF8Decode(AText)));
end;

class function TCDWSCustomForm.GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
var
  lWindowInfo: TWindowInfo;
  lHandle: HWND;
begin
  {$ifdef VerboseCDForms}
  DebugLn(':>[TCDWSCustomForm.GetClientBounds]');
  {$endif}
  lWindowInfo := TWindowInfo(AWinControl.Handle);
  lHandle := lWindowInfo.NativeHandle;
  Result := LCLIntf.GetClientBounds(lHandle, ARect);
  {$ifdef VerboseCDForms}
  DebugLn(Format(':<[TCDWSCustomForm.GetClientBounds] AWincontrol=%x Rect.left=%d Rect.Top=%d'
    + ' Rect.Width=%d Rect.Height=%d', [PtrInt(AWincontrol), ARect.Top, ARect.Left,
    ARect.Right-ARect.Left, ARect.Bottom-ARect.Top]));
  {$endif}
end;

class function TCDWSCustomForm.GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  {$ifdef VerboseCDForms}
  DebugLn('[TCDWSCustomForm.GetClientRect]');
  {$endif}
  Result := LCLIntf.GetClientRect(AWincontrol.Handle, ARect);
end;


