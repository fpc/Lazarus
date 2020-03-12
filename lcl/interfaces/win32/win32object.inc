{%MainUnit win32int.pp}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
constructor TWin32WidgetSet.Create;
begin
  FPendingWaitHandlerIndex := -1;
  inherited Create;
  FTimerData  := TFpList.Create;
  FMetrics.cbSize := SizeOf(FMetrics);
  FMetricsFailed := not Windows.SystemParametersInfo(SPI_GETNONCLIENTMETRICS,
    SizeOf(FMetrics), @FMetrics, 0);
  if FMetricsFailed then
  begin
    FMetrics.iMenuHeight := GetSystemMetrics(SM_CYMENU);
    FDefaultFont := GetStockObject(DEFAULT_GUI_FONT);
  end
  else
  begin
    FDefaultFont := Windows.CreateFontIndirect(FMetrics.lfMessageFont);
  end;
  OnClipBoardRequest := nil;
  Pointer(InitCommonControlsEx) := GetProcAddress(GetModuleHandle(comctl32), 'InitCommonControlsEx');
  FCommonControlsVersion := GetFileVersion(comctl32);
  FDotsPatternBitmap := 0;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TWin32WidgetSet.Destroy;
var
  n: integer;
  TimerInfo : PWin32TimerInfo;
begin
  n := FTimerData.Count;
  if (n > 0) then
  begin
    DebugLn(Format('[TWin32WidgetSet.Destroy] WARNING: There are %d TimerInfo structures left, I''ll free them' ,[n]));
    while (n > 0) do
    begin
      dec(n);
      TimerInfo := PWin32Timerinfo(FTimerData[n]);
      Dispose(TimerInfo);
      FTimerData.Delete(n);
    end;
  end;

  FTimerData.Free;
  
  if FDotsPatternBitmap <> 0 then
    DeleteObject(FDotsPatternBitmap);

  if FAppHandle <> 0 then
  begin
    {$ifdef RedirectDestroyMessages}
    SetWindowLong(FAppHandle, GWL_WNDPROC, PtrInt(@DestroyWindowProc));
    {$endif}
    DestroyWindow(FAppHandle);
  end;
  Windows.UnregisterClassW(@ClsNameW, System.HInstance);
  Windows.UnregisterClassW(@ClsHintNameW, System.HInstance);
  if FDefaultFont <> 0 then
    Windows.DeleteObject(FDefaultFont);
  inherited Destroy;
end;

procedure TWin32WidgetSet.DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean);
begin
  if AEnabled then SetStretchBltMode(CanvasHandle, HALFTONE)
    else SetStretchBltMode(CanvasHandle, COLORONCOLOR);
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppInit
  Params:  None
  Returns: Nothing

  Initialize Windows
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppInit(var ScreenInfo: TScreenInfo);
const
  Win32ClassesToInit: array[0..5] of DWord = (ICC_DATE_CLASSES,
    ICC_UPDOWN_CLASS, ICC_TAB_CLASSES, ICC_PROGRESS_CLASS, ICC_BAR_CLASSES,
    ICC_PAGESCROLLER_CLASS);
var
  ICC: TINITCOMMONCONTROLSEX;
  Handle: HWND;
  DC: HDC;
  AIcon: HICON;
  i: integer;
begin
  if not WinRegister then
  begin
    DebugLn('Trace:Win32Object.Init - Register Failed');
    Exit;
  end;

  OleInitialize(nil);
  //TODO: Remove when the WS interface is implemented
  //      Common controls only need to be initialized when used
  //      So they are initialized in the CreateHandle for common controls
  InitCommonControls;
  if InitCommonControlsEx <> nil then
  begin
    ICC.dwSize := SizeOf(TINITCOMMONCONTROLSEX);
    for i := Low(Win32ClassesToInit) to High(Win32ClassesToInit) do
    begin
      ICC.dwICC := Win32ClassesToInit[i];
      InitCommonControlsEx(@ICC);
    end;
  end;

  // Create parent of all windows, 'button on taskbar'
  if not IsLibrary then
  begin
    CreateAppHandle;
    // set nice main icon
    AIcon := Windows.LoadIcon(MainInstance, 'MAINICON');
    AppSetIcon(AIcon, AIcon);
  end;

  // initialize ScreenInfo
  Handle := GetDesktopWindow;
  DC := Windows.GetDC(Handle);
  ScreenInfo.PixelsPerInchX := GetDeviceCaps(DC, LOGPIXELSX);
  ScreenInfo.PixelsPerInchY := GetDeviceCaps(DC, LOGPIXELSY);
  ScreenInfo.ColorDepth := GetDeviceCaps(DC, BITSPIXEL);
  Windows.ReleaseDC(Handle, DC);

  // Thread.Synchronize support
  WakeMainThread := @HandleWakeMainThread;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppMinimize
  Params:  None
  Returns: Nothing

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppMinimize;
var
  AForm: TCustomForm;
  i: Integer;
begin
  FAppMinimizing := True;
  // issue #26463
  if Assigned(Application) and Application.MainFormOnTaskBar
  and Assigned(Application.MainForm) and Application.MainForm.HandleAllocated then
  begin
    HidePopups(Application.MainFormHandle);
    Application.MainForm.WindowState := wsMinimized;
  end
  else
  if Assigned(Application) and (Application.ModalLevel > 0) then
  begin
    for i := Screen.CustomFormZOrderCount - 1 downto 0 do
    begin
      AForm := Screen.CustomFormsZOrdered[i];
      if AForm.HandleAllocated and AForm.Visible and (fsModal in AForm.FormState) then
        ShowWindow(AForm.Handle, SW_SHOWMINIMIZED);
    end;
    ShowWindow(Win32WidgetSet.AppHandle, SW_SHOWMINNOACTIVE);
  end
  else
    Windows.SendMessage(FAppHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);

  FAppMinimizing := False;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppRestore
  Params:  None
  Returns: Nothing

  Restore minimized whole application from taskbar
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppRestore;
var
  AForm: TCustomForm;
  i: Integer;
begin
  // issue #26463
  if Assigned(Application) and Application.MainFormOnTaskBar
  and Assigned(Application.MainForm) and Application.MainForm.HandleAllocated then
  begin
    ShowWindow(Application.MainForm.Handle, SW_RESTORE);
    RestorePopups;
    if (Screen.ActiveControl<>nil) and Screen.ActiveControl.HandleAllocated then
      SetFocus(Screen.ActiveControl.Handle);
  end
  else
  if Assigned(Application) and (Application.ModalLevel > 0) then
  begin
    ShowWindow(FAppHandle, SW_RESTORE);
    for i := Screen.CustomFormZOrderCount - 1 downto 0 do
    begin
      AForm := Screen.CustomFormsZOrdered[i];
      if AForm.HandleAllocated and AForm.Visible and (fsModal in AForm.FormState) then
      begin
        ShowWindow(AForm.Handle, SW_RESTORE);
      end;
    end;
  end
  else
    Windows.SendMessage(FAppHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;


{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppBringToFront
  Params:  None
  Returns: Nothing

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppBringToFront;
var
  Window: HWND;
begin
  if Application.MainFormOnTaskBar and (Application.MainFormHandle<>0) then
    Window := GetLastActivePopup(Application.MainFormHandle)
  else
    Window := GetLastActivePopup(AppHandle);
  if not((Window <> 0) and IsWindowVisible(Window) and IsWindowEnabled(Window)) then
  begin
    if Assigned(Screen) and Assigned(Screen.ActiveCustomForm) and Screen.ActiveCustomForm.HandleAllocated then
      Window := Screen.ActiveCustomForm.Handle
    else
    if Assigned(Application) and Application.MainFormOnTaskBar then
      Window := Application.MainFormHandle
    else
      Window := FAppHandle;
  end;
  if (Window <> 0) and IsWindowVisible(Window) and IsWindowEnabled(Window) then
    Windows.SetForegroundWindow(Window);
end;

procedure TWin32WidgetSet.SetDesigning(AComponent: TComponent);
begin
  //if Data<>nil then EnableWindow((AComponent As TWinControl).Handle, boolean(Data^));
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.SetCallback
  Params: Msg    - message for which to set a callback
          Sender - object to which callback will be sent
  Returns:  nothing

  Applies a Message to the sender
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.SetCallback(Msg: LongInt; Sender: TObject);
var
  Window: HWnd;
begin
  if Sender is TControlCanvas then
    Window := TControlCanvas(Sender).Handle
  else 
    Window := TWinControl(Sender).Handle;

  if Window = 0 then Exit;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.RemoveCallbacks
  Params:   Sender - object from which to remove callbacks
  Returns:  nothing

  Removes Call Back Signals from the sender
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.RemoveCallbacks(Sender: TObject);
var
  Window: HWnd;
begin
  if Sender is TControlCanvas then
    Window := TControlCanvas(Sender).Handle
  else 
  if Sender is TCustomForm then
    Window := TCustomForm(Sender).Handle
  else
    Window := (Sender as TWinControl).Handle;

  if Window = 0 then Exit;
end;

function TWin32WidgetSet.InitStockFont(AFont: TObject; AStockFont: TStockFont): Boolean;
var
  Font: TFont absolute AFont;
  LogFont: TLogFont;

  procedure AssignDefault;
  var
    LogFont: TLogFont;
  begin
    GetObject(DefaultFont, SizeOf(LogFont), @LogFont);
    Font.Assign(LogFont);
  end;

begin
  case AStockFont of
    sfSystem:
      AssignDefault;
    sfHint:
      begin
        if FMetricsFailed then
          AssignDefault
        else
          Font.Assign(FMetrics.lfStatusFont);
        Font.Color := clInfoText;
      end;
    sfIcon:
      begin
        if SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(LogFont), @LogFont, 0) then
          Font.Assign(LogFont)
        else
          AssignDefault
      end;
    sfMenu:
      begin
        if FMetricsFailed then
          AssignDefault
        else
          Font.Assign(FMetrics.lfMenuFont);
        Font.Color := clMenuText;
      end;
  end;
  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppProcessMessages
  Params:  None
  Returns: Nothing

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppProcessMessages;
var
  AMessage: TMsg;
  retVal, index: dword;
  pHandles: Windows.LPHANDLE;

    procedure CallWaitHandler;
    begin
      FWaitHandlers[index].OnEvent(FWaitHandlers[index].UserData, 0);
    end;

begin
  repeat
    if FPendingWaitHandlerIndex >= 0 then
    begin
      index := FPendingWaitHandlerIndex;
      FPendingWaitHandlerIndex := -1;
      CallWaitHandler;
    end;
{$ifdef DEBUG_ASYNCEVENTS}
    if Length(FWaitHandles) > 0 then
      DebugLn('[ProcessMessages] WaitHandleCount=', IntToStr(FWaitHandleCount),
        ', WaitHandle[0]=', IntToHex(FWaitHandles[0], 8));
{$endif}
    if FWaitHandleCount > 0 then
      pHandles := @FWaitHandles[0]
    else
      pHandles := nil;
    retVal := Windows.MsgWaitForMultipleObjects(FWaitHandleCount,
      pHandles, False, 0, QS_ALLINPUT);
    if (retVal < WAIT_OBJECT_0 + FWaitHandleCount) then
    begin
      index := retVal-WAIT_OBJECT_0;
      CallWaitHandler;
    end else
    if retVal = WAIT_OBJECT_0 + FWaitHandleCount then
    begin
      while PeekMessage(AMessage, HWnd(nil), 0, 0, PM_REMOVE) do
      begin
        if AMessage.message = WM_QUIT then
        begin
          PostQuitMessage(AMessage.wParam);
          break;
        end;
        // Handle MDI form accelerators
        if Assigned(Application) and
           Assigned(Application.MainForm) and
           (Application.MainForm.FormStyle=fsMDIForm) and
           TranslateMDISysAccel(Win32WidgetSet.MDIClientHandle, @AMessage)
        then begin
          // handled by TranslateMDISysAccel
        end else begin
          TranslateMessage(@AMessage);
          DispatchMessageW(@AMessage);
        end;
      end;
    end else
    if retVal = WAIT_TIMEOUT then
    begin
      // check for pending to-be synchronized methods
      CheckSynchronize;
      CheckPipeEvents;
      break;
    end else
    if retVal = $FFFFFFFF then
    begin
      DebugLn('[TWin32WidgetSet.AppProcessMessages] MsgWaitForMultipleObjects returned: ', IntToStr(GetLastError));
      break;
    end;
  until false;
end;

procedure TWin32WidgetSet.CheckPipeEvents;
var
  lHandler: PPipeEventInfo;
  lBytesAvail: dword;
  SomethingChanged: Boolean;
  ChangedCount:integer;
begin
  lHandler := FWaitPipeHandlers;
  ChangedCount:=0;
  while (lHandler <> nil) and (ChangedCount<10) do
  begin
    SomethingChanged:=true;
    if Windows.PeekNamedPipe(lHandler^.Handle, nil, 0, nil, @lBytesAvail, nil) then
    begin
      if lBytesAvail <> 0 then
        lHandler^.OnEvent(lHandler^.UserData, [prDataAvailable])
      else
        SomethingChanged := false;
    end else
      lHandler^.OnEvent(lHandler^.UserData, [prBroken]);
    if SomethingChanged then
      lHandler := FWaitPipeHandlers
    else begin
      lHandler := lHandler^.Next;
      ChangedCount := 0;
    end;
    inc(ChangedCount);
  end;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Passes execution control to Windows
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppWaitMessage;
var
  retVal, timeout: DWord;
  pHandles: Windows.LPHANDLE;
begin
  RedrawMenus;
  if FWaitPipeHandlers <> nil then
    timeout := 100
  else
    timeout := INFINITE;
  if FWaitHandleCount > 0 then
    pHandles := @FWaitHandles[0]
  else
    pHandles := nil;
  retVal := Windows.MsgWaitForMultipleObjects(FWaitHandleCount, pHandles,
    false, timeout, QS_ALLINPUT);
  if (retVal < WAIT_OBJECT_0 + FWaitHandleCount) then
    FPendingWaitHandlerIndex := retVal - WAIT_OBJECT_0;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Tells Windows to halt and destroy
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppTerminate;
begin
  OleUninitialize;
end;

procedure TWin32WidgetSet.AppSetIcon(const Small, Big: HICON);
begin
  if FAppHandle <> 0 then
  begin
    Windows.SendMessage(FAppHandle, WM_SETICON, ICON_SMALL, LPARAM(Small));
    SetClassLongPtr(FAppHandle, GCL_HICONSM, LONG_PTR(Small));

    Windows.SendMessage(FAppHandle, WM_SETICON, ICON_BIG, LPARAM(Big));
    SetClassLongPtr(FAppHandle, GCL_HICON, LONG_PTR(Big));
  end;
end;

procedure TWin32WidgetSet.AppSetTitle(const ATitle: string);
begin
  if FAppHandle <> 0 then
  begin
    Windows.SetWindowTextW(FAppHandle, PWideChar(UTF8ToUTF16(ATitle)));
  end;
end;

procedure TWin32WidgetSet.AppSetVisible(const AVisible: Boolean);
begin
  if (FAppHandle <> 0) and not (Assigned(Application) and Application.MainFormOnTaskBar) then
  begin
    if AVisible then
      Windows.ShowWindow(FAppHandle, SW_SHOW)
    else
      Windows.ShowWindow(FAppHandle, SW_HIDE);
  end;
end;

function TWin32WidgetSet.AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
  if not IsLibrary then
    RemoveStayOnTopFlags(FAppHandle, ASystemTopAlso);
  Result := True;
end;

function TWin32WidgetSet.AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
  if not IsLibrary then
    RestoreStayOnTopFlags(FAppHandle);
  Result := True;
end;

procedure TWin32WidgetSet.AppSetMainFormOnTaskBar(const DoSet: Boolean);
begin
  // 1. Update the visibility of the TaskBar window
  if DoSet then
    ShowWindow(AppHandle, SW_HIDE)
  else
    ShowWindow(AppHandle, SW_SHOW);
  // 2. Recreate the main form - so it will (not) have an own taskbar item and WndParent = 0 (AppHandle)
  if Assigned(Application.MainForm) and Application.MainForm.HandleAllocated then
    RecreateWnd(Application.MainForm);
end;

function TWin32WidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpWin32;
end;

function TWin32WidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
  case ACapability of
    lcAsyncProcess: Result := LCL_CAPABILITY_YES;
    lcModalWindow: Result := LCL_CAPABILITY_NO;
    lcDragDockStartOnTitleClick: Result := LCL_CAPABILITY_YES;
    lcApplicationWindow: Result := LCL_CAPABILITY_YES;
    lcLMHelpSupport: Result := LCL_CAPABILITY_YES;
    lcNeedMininimizeAppWithMainForm: Result := LCL_CAPABILITY_NO;
    lcSendsUTF8KeyPress: Result := LCL_CAPABILITY_YES;
    lcTransparentWindow: Result := LCL_CAPABILITY_YES;
    lcAllowChildControlsInNativeControls: Result := LCL_CAPABILITY_YES;
    lcTextHint:
    begin
      if (ComCtlVersion >= ComCtlVersionIE6) then
        Result := LCL_CAPABILITY_YES
      else
        Result := LCL_CAPABILITY_NO;
    end;
  else
    Result := inherited;
  end;
end;

{------------------------------------------------------------------------------
  function: CreateTimer
  Params: Interval:
          TimerFunc: Callback
  Returns: a Timer id (use this ID to destroy timer)

  Design: A timer which calls TimerCallBackProc, is created.
    The TimerCallBackProc calls the TimerFunc.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc) : THandle;
var
  TimerInfo: PWin32TimerInfo;
begin
  Result := 0;
  if (Interval > 0) and (TimerFunc <> nil) then begin
    New(TimerInfo);
    TimerInfo^.TimerFunc := TimerFunc;
    TimerInfo^.TimerID := Win32Extra.SetTimer(0, 0, Interval, @TimerCallBackProc);
    if TimerInfo^.TimerID=0 then
      dispose(TimerInfo)
    else begin
      FTimerData.Add(TimerInfo);
      Result := TimerInfo^.TimerID;
    end;
  end;
end;

{------------------------------------------------------------------------------
  function: DestroyTimer
  Params: TimerHandle
  Returns:
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.DestroyTimer(TimerHandle: THandle) : boolean;
var
  n : integer;
  TimerInfo : PWin32Timerinfo;
begin
  Result:= false;
  n := FTimerData.Count;
  while (n>0) do begin
    dec(n);
    TimerInfo := FTimerData[n];
    if (TimerInfo^.TimerID=UINT_PTR(TimerHandle)) then
    begin
      Result := Boolean(Win32Extra.KillTimer(0, UINT_PTR(TimerHandle)));
      FTimerData.Delete(n);
      Dispose(TimerInfo);
    end;
  end;
end;

procedure TWin32WidgetSet.HandleWakeMainThread(Sender: TObject);
begin
  // wake up GUI thread by sending a message to it
  if FAppHandle <> 0 then
    Windows.PostMessage(FAppHandle, WM_NULL, 0, 0);
end;

procedure TWin32WidgetSet.UpdateMDIClientBounds;

  function CalculateClientArea: TRect;
  var
    I: Integer;
  begin
    Windows.GetClientRect(Application.MainFormHandle, Result);
    for I := 0 to Application.MainForm.ControlCount - 1 do
      if Application.MainForm.Controls[I].Visible then
        case Application.MainForm.Controls[I].Align of
          alLeft: Inc(Result.Left, Application.MainForm.Controls[I].Width);
          alTop: Inc(Result.Top, Application.MainForm.Controls[I].Height);
          alRight: Dec(Result.Right, Application.MainForm.Controls[I].Width);
          alBottom: Dec(Result.Bottom, Application.MainForm.Controls[I].Height);
        end;
  end;

var
  R: TRect;
begin
  if not (Assigned(Application.MainForm) and (Application.MainForm.FormStyle=fsMDIForm)) then Exit;
  R := CalculateClientArea;
  MoveWindow(Win32WidgetSet.MDIClientHandle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, True);
end;

{ Private methods (in no significant order) }

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.WinRegister
  Params: None
  Returns: If the window was successfully registered

  Registers the main window class
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.WinRegister: Boolean;
var
  WindowClassW: WndClassW;
begin
  with WindowClassW do
  begin
    Style := CS_DBLCLKS;
    LPFnWndProc := @WindowProc;
    CbClsExtra := 0;
    CbWndExtra := 0;
    hInstance := System.HInstance;
    hIcon := Windows.LoadIcon(MainInstance, 'MAINICON');
    if hIcon = 0 then
     hIcon := Windows.LoadIcon(0, IDI_APPLICATION);
    hCursor := Windows.LoadCursor(0, IDC_ARROW);
    hbrBackground := 0;
    LPSzMenuName := nil;
    LPSzClassName := @ClsNameW;
  end;
  Result := Windows.RegisterClassW(@WindowClassW) <> 0;
  if Result then
  begin
    with WindowClassW do
    begin
      style := style or CS_SAVEBITS;
      if WindowsVersion >= wvXP then
        style := style or CS_DROPSHADOW;
      hIcon := 0;
      hbrBackground := 0;
      LPSzClassName := @ClsHintNameW;
    end;
    Result := Windows.RegisterClassW(@WindowClassW) <> 0;
  end;
end;

procedure TWin32WidgetSet.CreateAppHandle;
var
  SysMenu: HMENU;
begin
  FAppHandle := CreateWindowW(@ClsNameW,
    PWideChar(UTF8ToUTF16(Application.Title)),
    WS_POPUP or WS_CLIPSIBLINGS or WS_SYSMENU or WS_MINIMIZEBOX,
    0, {Windows.GetSystemMetrics(SM_CXSCREEN) div 2,}
    0, {Windows.GetSystemMetrics(SM_CYSCREEN) div 2,}
    0, 0, HWND(nil), HMENU(nil), HInstance, nil);
  AllocWindowInfo(FAppHandle);
  // remove useless menuitems from sysmenu
  SysMenu := Windows.GetSystemMenu(FAppHandle, False);
  Windows.DeleteMenu(SysMenu, SC_MAXIMIZE, MF_BYCOMMAND);
  Windows.DeleteMenu(SysMenu, SC_SIZE, MF_BYCOMMAND);
  Windows.DeleteMenu(SysMenu, SC_MOVE, MF_BYCOMMAND);
end;

function TWin32WidgetSet.GetMDIClientHandle: HWND;
const
  MDIClientW: array[0..9] of WideChar = ('M', 'D', 'I', 'C', 'L', 'I', 'E', 'N', 'T', #0);
var
  CCS: TCLIENTCREATESTRUCT;
begin
  if (FMDIClientHandle=0) and
     Assigned(Application) and
     Assigned(Application.MainForm) and
     (Application.MainForm.FormStyle=fsMDIForm) then
  begin
    CCS.hWindowMenu := 0;
    CCS.idFirstChild := 0;
    FMDIClientHandle := CreateWindowW(@MDIClientW, nil,
      WS_CHILD or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_VSCROLL or WS_HSCROLL,
      0, 0, 0, 0, Application.MainForm.Handle, 0, HInstance, @CCS);
    ShowWindow(FMDIClientHandle, SW_SHOW);
  end;
  Result := FMDIClientHandle;
end;

function TWin32WidgetSet.CreateThemeServices: TThemeServices;
begin
  Result := TWin32ThemeServices.Create;
end;

function TWin32WidgetSet.GetAppHandle: THandle;
begin
  Result:= FAppHandle;
end;

procedure TWin32WidgetSet.SetAppHandle(const AValue: THandle);
begin
  // Do it only if handle is not yet created (for example for DLL initialization)
  FAppHandle := AValue;
end;

function TWin32WidgetSet.GetDotsPatternBitmap: HBitmap;
const
  Dots: array[0..3] of Word = ($55, $AA, $55, $AA);
begin
  if FDotsPatternBitmap = 0 then
    FDotsPatternBitmap := CreateBitmap(4, 4, 1, 1, @Dots);
  Result := FDotsPatternBitmap;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.DCReDraw
  Params: CanvasHandle - HDC to redraw
  Returns: Nothing

  Redraws (the window of) a canvas
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.DCRedraw(CanvasHandle: HDC);
begin
  // TODO: implement me!
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.SetPixel
  Params:  Canvas - canvas to set color on
           X, Y   - position
           AColor - new color for specified position
  Returns: nothing

  Set the color of the specified pixel on the canvas
 ------------------------------------------------------------------------------}
var
  IntSetPixel : function (DC:HDC; X, Y:longint; cl:Windows.COLORREF):Windows.COLORREF; stdcall = nil;

function VistaSetPixel(DC:HDC; X,Y:longint; cl:Windows.COLORREF):Windows.COLORREF; stdcall;
var
  pen, oldpen: HPEN;
  p : Windows.TPOINT;
begin
  if x and $100=0 then
    Result:=Windows.SetPixel(DC,X,Y,cl)
  else
  begin
    pen := Windows.CreatePen(PS_SOLID, 1, ColorToRGB(cl));
    oldpen := Windows.SelectObject(DC, pen);
    Windows.MoveToEx(DC, X, Y, @p);
    Windows.LineTo(DC, X, Y + 1);
    Windows.SelectObject(DC, oldpen);
    Windows.DeleteObject(pen);
    Windows.MoveToEx(DC, P.X, P.Y, nil);
    Result:=cl;
  end;
end;

procedure TWin32WidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
begin
  IntSetPixel(CanvasHandle, X, Y, ColorToRGB(AColor));
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.GetPixel
  Params:  Canvas - canvas to get color from
           X, Y   - position
  Returns: Color at specified point

  Get the color of the specified pixel on the canvas
 -----------------------------------------------------------------------------}
function  TWin32WidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
begin
  Result := Windows.GetPixel(CanvasHandle, X, Y);
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}
