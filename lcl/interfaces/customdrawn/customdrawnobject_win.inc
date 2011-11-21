{%MainUnit customdrawnint.pas}

{******************************************************************************
                                   customdrawnobject_win.inc
 ******************************************************************************

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

function TCDWidgetSet.WinRegister: Boolean;
var
  WindowClass: Windows.WndClassW;
begin
  FillChar(WindowClass, SizeOf(WindowClass), #0);

  WindowClass.LPFnWndProc := @WindowProc;
  WindowClass.Style := CS_DBLCLKS;// wince uses: CS_HREDRAW or CS_VREDRAW;
  WindowClass.hIcon := Windows.LoadIcon(System.hInstance, 'MAINICON');
  if WindowClass.hIcon = 0 then
    WindowClass.hIcon := Windows.LoadIcon(0, IDI_APPLICATION);
  WindowClass.hCursor := Windows.LoadCursor(0, IDC_ARROW);
  WindowClass.hbrBackground := 0;
  WindowClass.LPSzMenuName := nil;
  WindowClass.LPSzClassName := @ClsName;
  Result := Windows.RegisterClassW(@WindowClass) <> 0;

  if Result then
  begin
    WindowClass.style := WindowClass.style or CS_SAVEBITS;
    if WindowsVersion >= wvXP then
      WindowClass.style := WindowClass.style or CS_DROPSHADOW;
    WindowClass.hIcon := 0;
    WindowClass.hbrBackground := 0;
    WindowClass.LPSzClassName := @ClsHintName;
    Result := Windows.RegisterClassW(@WindowClass) <> 0;
  end;
end;

procedure TCDWidgetSet.CreateAppHandle;
var
  SysMenu: HMENU;
begin
  FAppHandle := CreateWindowExW(0, @ClsName,
    PWideChar(UTF8ToUTF16(Application.Title)),
    WS_POPUP or WS_CLIPSIBLINGS or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX,
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

{------------------------------------------------------------------------------
  Method: TCDWidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.BackendCreate;
var
  Font: THandle;
begin
  inherited Create;
  FTimerData := TList.Create;

  // Metrics always fail because SPI_GETNONCLIENTMETRICS doesn't exist under WinCE
  // So we need to get the fonts by other means
  FMetrics.cbSize := SizeOf(FMetrics);
  FMetricsFailed := True;

  FMetrics.iMenuHeight := GetSystemMetrics(SM_CYMENU);
  Font := GetStockObject(SYSTEM_FONT); // MSDN Docs say its not necessary to destroy results from GetStockObject
  GetObject(Font, SizeOf(FMetrics.lfMessageFont), @FMetrics.lfMessageFont);
  GetObject(Font, SizeOf(FMetrics.lfCaptionFont), @FMetrics.lfCaptionFont);
  GetObject(Font, SizeOf(FMetrics.lfStatusFont), @FMetrics.lfStatusFont);
  GetObject(Font, SizeOf(FMetrics.lfMenuFont), @FMetrics.lfMenuFont);

  CDWidgetSet := Self;
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  destructor for the class.
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.BackendDestroy;
{var
  n: integer;
  TimerInfo : PWinCETimerInfo;}
begin
  //DebugLn('Trace:TWinCEWidgetSet is being destroyed');

{  n := FTimerData.Count;
  if (n > 0) then
  begin
    DebugLn(Format('[TWinCEWidgetSet.Destroy] WARNING: There are %d TimerInfo structures left, I''ll free them' ,[n]));
    while (n > 0) do
    begin
      dec(n);
      TimerInfo := PWinCETimerinfo(FTimerData[n]);
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

  inherited Destroy;}
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppInit
  Params:  None
  Returns: Nothing

  initialize Windows
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
var
  Handle: HWND;
  DC: HDC;
//  Flags : integer;
begin
  {$ifdef VerboseWinCE}
  DebugLn('TWinCEWidgetSet.AppInit');
  {$endif}

  // WinRegister
  if not WinRegister() then
  begin
    DebugLn('TCDWidgetSet.AppInit RegisterClassW failed.');
    Exit;
  end;
  
  { Initializes the application type }
  {if Application.ApplicationType = atDefault then
    Application.ApplicationType := GetWinCEPlatform();}

  //Init stock objects;
  FStockNullBrush := Windows.CreateSolidBrush(0);
  FStockBlackBrush := Windows.CreateSolidBrush($000000);
  FStockLtGrayBrush := Windows.CreateSolidBrush($C0C0C0);
  FStockGrayBrush := Windows.CreateSolidBrush($808080);
  FStockDkGrayBrush := Windows.CreateSolidBrush($404040);
  FStockWhiteBrush := Windows.CreateSolidBrush($FFFFFF);

  // From bug 15058: DEFAULT_GUI_FONT fails in some devices and in the emulator too
  // It isn't even in the WinCE GetStockObject MSDN Docs and is strongly recommended
  // against in Win32 Docs. SYSTEM_FONT is also recommended against in Win32 Docs,
  // but it seams to work in Windows CE
  FStatusFont := Windows.GetStockObject(SYSTEM_FONT);
  FMessageFont := Windows.GetStockObject(SYSTEM_FONT);

  // Create parent of all windows, 'button on taskbar'
  if not IsLibrary then
  begin
    CreateAppHandle;
    // set nice main icon
//    AIcon := Windows.LoadIcon(MainInstance, 'MAINICON');
//    AppSetIcon(AIcon, AIcon);
  end;

  // set nice main icon
  SendMessage(FAppHandle, WM_SETICON, ICON_BIG,
    Windows.LoadIcon(MainInstance, 'MAINICON'));

  // Felipe: This commented code looks unnecessary to me

  // remove useless menuitems from sysmenu
//  SysMenu := Windows.GetSystemMenu(FAppHandle, False);
//  Windows.DeleteMenu(SysMenu, SC_MAXIMIZE, MF_BYCOMMAND);
//  Windows.DeleteMenu(SysMenu, SC_SIZE, MF_BYCOMMAND);
//  Windows.DeleteMenu(SysMenu, SC_MOVE, MF_BYCOMMAND);

  // initialize ScreenInfo
  Handle := GetDesktopWindow;
  DC := Windows.GetDC(Handle);
  ScreenInfo.PixelsPerInchX := GetDeviceCaps(DC, LOGPIXELSX);
  ScreenInfo.PixelsPerInchY := GetDeviceCaps(DC, LOGPIXELSY);
  ScreenInfo.ColorDepth := GetDeviceCaps(DC, BITSPIXEL);
  ReleaseDC(Handle, DC);

  // Thread.Synchronize support
  //WakeMainThread := @HandleWakeMainThread;
end;

procedure TCDWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
var
  AMessage: TMsg;
begin
//  inherited AppRun(ALoop);

  while Windows.GetMessage(@AMessage, 0, 0, 0) do
  begin
    Windows.TranslateMessage(@AMessage);
    Windows.DispatchMessage(@AMessage);
  end;
end;

(*
function TWinCEWidgetSet.GetAppHandle: THandle;
begin
  Result:= FAppHandle;
end;

procedure TWinCEWidgetSet.SetAppHandle(const AValue: THandle);
begin
  // Do it only if handle is not yet created (for example for DLL initialization)
  // if handle is already created we can't reassign it
  if AppHandle = 0 then
    FAppHandle := AValue;
end;*)

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppMinimize
  Params:  None
  Returns: Nothing

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppMinimize;
begin
//  Windows.SendMessage(FAppHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppRestore
  Params:  None
  Returns: Nothing

  Restore minimized whole application from taskbar
 ------------------------------------------------------------------------------}

procedure TCDWidgetSet.AppRestore;
begin
//  Windows.SendMessage(FAppHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;


{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppBringToFront
  Params:  None
  Returns: Nothing

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppBringToFront;
begin
  Windows.SetForegroundWindow(FAppHandle);
end;

(*
procedure TWinCEWidgetSet.SetDesigning(AComponent: TComponent);
begin
  //if Data<>nil then EnableWindow((AComponent As TWinControl).Handle, boolean(Data^));
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.SetCallback
  Params: Msg    - message for which to set a callback
          Sender - object to which callback will be sent
  Returns:  nothing

  Applies a Message to the sender
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.SetCallback(Msg: LongInt; Sender: TObject);
var
  Window: HWnd;
begin
  //DebugLn('Trace:TWinCEWidgetSet.SetCallback - Start');
  //DebugLn(Format('Trace:TWinCEWidgetSet.SetCallback - Class Name --> %S', [Sender.ClassName]));
  //DebugLn(Format('Trace:TWinCEWidgetSet.SetCallback - Message Name --> %S', [GetMessageName(Msg)]));
  if Sender Is TControlCanvas then
    Window := TControlCanvas(Sender).Handle
  else if Sender Is TCustomForm then
    Window := TCustomForm(Sender).Handle
  else
    Window := TWinControl(Sender).Handle;
  if Window=0 then exit;

  //DebugLn('Trace:TWinCEWidgetSet.SetCallback - Exit');
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.RemoveCallbacks
  Params:   Sender - object from which to remove callbacks
  Returns:  nothing

  Removes Call Back Signals from the sender
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.RemoveCallbacks(Sender: TObject);
var
  Window: HWnd;
begin
  if Sender Is TControlCanvas then
    Window := TControlCanvas(Sender).Handle
  else if Sender Is TCustomForm then
    Window := TCustomForm(Sender).Handle
  else
    Window := (Sender as TWinControl).Handle;
  if Window=0 then exit;
end;*)

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppProcessMessages
  Params:  None
  Returns: Nothing

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppProcessMessages;
var
  AMessage: TMsg;
  retVal, index: dword;
  pHandles: Windows.LPHANDLE;
begin
  repeat
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

    //roozbeh:added
    if FWaitHandleCount = 0 then
      retVal := WAIT_OBJECT_0;

    if (WAIT_OBJECT_0 <= retVal) and (retVal < WAIT_OBJECT_0 + FWaitHandleCount) then
    begin
      index := retVal-WAIT_OBJECT_0;
      FWaitHandlers[index].OnEvent(FWaitHandlers[index].UserData, 0);
    end else
    if retVal = WAIT_OBJECT_0 + FWaitHandleCount then
    begin
      while PeekMessage(AMessage, HWnd(nil), 0, 0, PM_REMOVE) do
      begin
        TranslateMessage(@AMessage);
        DispatchMessage(@AMessage);
      end;
      if FWaitHandleCount = 0 then
        break;
    end else
    if retVal = WAIT_TIMEOUT then
    begin
      // check for pending to-be synchronized methods
      CheckSynchronize;
      //CheckPipeEvents;
      break;
    end else
    if retVal = $FFFFFFFF then
    begin
      DebugLn('[TWinCEWidgetSet.AppProcessMessages] MsgWaitForMultipleObjects returned: ', IntToStr(GetLastError));
      break;
    end;
  until false;
end;
(*
procedure TWinCEWidgetSet.CheckPipeEvents;
var
  lHandler: PPipeEventInfo;
//  lBytesAvail: dword;
//  SomethingChanged: Boolean;
  ChangedCount:integer;
begin
  lHandler := FWaitPipeHandlers;
  ChangedCount := 0;
  while (lHandler <> nil) and (ChangedCount < 10) do
  begin
    {
    roozbeh : ooops not supported
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
    inc(ChangedCount);}
  end;
end;*)

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Passes execution control to Windows
 ------------------------------------------------------------------------------}
//roozbeh:new update...whole procedure body is added.what is it?
procedure TCDWidgetSet.AppWaitMessage;
var
  timeout, retVal: DWord;
  pHandles: Windows.LPHANDLE;
begin
//  RedrawMenus;
  //DebugLn('Trace:TWinCEWidgetSet.WaitMessage - Start');
  if FWaitPipeHandlers <> nil then
    timeout := 100
  else
    timeout := INFINITE;


  if FWaitHandleCount > 0 then
    pHandles := @FWaitHandles[0]
  else
    pHandles := nil;
//roozbeh...remove raise after testing!
  retVal := Windows.MsgWaitForMultipleObjects(FWaitHandleCount, pHandles,
    false, timeout, QS_ALLINPUT);

  if retVal = $FFFFFFFF then
    RaiseGDBException('Failaure on MsgWaitForMultipleObjects');

  //DebugLn('Trace:Leave wait message');
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Tells Windows to halt and destroy
 ------------------------------------------------------------------------------}

procedure TCDWidgetSet.AppTerminate;
begin
  //DebugLn('Trace:TWinCEWidgetSet.AppTerminate - Start');
end;

procedure TCDWidgetSet.AppSetIcon(const Small, Big: HICON);
begin
  if FAppHandle <> 0 then
  begin
    Windows.SendMessage(FAppHandle, WM_SETICON, ICON_SMALL, LPARAM(Small));
    SetClassLong(FAppHandle, GCL_HICONSM, LONG(Small));

    Windows.SendMessage(FAppHandle, WM_SETICON, ICON_BIG, LPARAM(Big));
    SetClassLong(FAppHandle, GCL_HICON, LONG(Big));
  end;
end;

procedure TCDWidgetSet.AppSetTitle(const ATitle: string);
begin
  Windows.SetWindowTextW(FAppHandle, PWideChar(UTF8ToUTF16(ATitle)));
end;

procedure TCDWidgetSet.AppSetVisible(const AVisible: Boolean);
begin
  if (FAppHandle <> 0) and not (Assigned(Application) and Application.MainFormOnTaskBar) then
  begin
    if AVisible then
      Windows.ShowWindow(FAppHandle, SW_SHOW)
    else
      Windows.ShowWindow(FAppHandle, SW_HIDE);
  end;
end;

function TCDWidgetSet.AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
  if not IsLibrary then
    RemoveStayOnTopFlags(FAppHandle, ASystemTopAlso);
  Result := True;
end;

function TCDWidgetSet.AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
  if not IsLibrary then
    RestoreStayOnTopFlags(FAppHandle);
  Result := True;
end;

procedure TCDWidgetSet.AppSetMainFormOnTaskBar(const DoSet: Boolean);
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

{------------------------------------------------------------------------------
  function: CreateTimer
  Params: Interval:
          TimerFunc: Callback
  Returns: a Timer id (use this ID to destroy timer)

  Design: A timer which calls TimerCallBackProc, is created.
    The TimerCallBackProc calls the TimerFunc.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc) : THandle;
var
  TimerInfo: PWinCETimerInfo;
begin
  //DebugLn('Trace:Create Timer: ' + IntToStr(Interval));
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
  //DebugLn('Trace:Result: ' + IntToStr(result));
end;

{------------------------------------------------------------------------------
  function: DestroyTimer
  Params: TimerHandle
  Returns:
 ------------------------------------------------------------------------------}
function TCDWidgetSet.DestroyTimer(TimerHandle: THandle) : boolean;
var
  n : integer;
  TimerInfo : PWinCETimerinfo;
begin
  Result:= false;
  //DebugLn('Trace:removing timer: '+ IntToStr(TimerHandle));
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
  //DebugLn('Trace:Destroy timer Result: '+ BOOL_RESULT[result]);
end;
(*
procedure TWinCEWidgetSet.HandleWakeMainThread(Sender: TObject);
begin
  // wake up GUI thread by sending a message to it
  Windows.PostMessage(AppHandle, WM_NULL, 0, 0);
end;
*)
{ Private methods (in no significant order) }

(*
{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.ShowHide
  Params: Sender - The sending object
  Returns: Nothing

  Shows or hides a control
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.ShowHide(Sender: TObject);
var
  Handle: HWND;
//  ParentPanel: HWND;
  Flags: dword;
begin
  //if (TControl(Sender).FCompStyle = csPage) or (TControl(Sender).FCompStyle = csToolButton) then exit;
  Handle := ObjectToHWND(Sender);
//  ParentPanel := GetWindowInfo(Handle)^.ParentPanel;
//  if ParentPanel <> 0 then
//    Handle := ParentPanel;
  if TControl(Sender).HandleObjectShouldBeVisible then
  begin
    //DebugLn('Trace: [TWinCEWidgetSet.ShowHide] Showing the window');
    if TControl(Sender).FCompStyle = csHintWindow then
    begin
      Windows.SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
    end else begin
      Flags := SW_SHOW;
      if (TControl(Sender) is TCustomForm) and
        (Application.ApplicationType = atDesktop) then
        case TCustomForm(Sender).WindowState of
          wsMaximized: Flags := SW_SHOWMAXIMIZED;
          wsMinimized: Flags := SW_SHOWMINIMIZED;
        end;
      Windows.ShowWindow(Handle, Flags);
      { ShowWindow does not send WM_SHOWWINDOW when creating overlapped maximized window }
      { TODO: multiple WM_SHOWWINDOW when maximizing after initial show? }
      if Flags = SW_SHOWMAXIMIZED then
        Windows.SendMessage(Handle, WM_SHOWWINDOW, 1, 0);
    end;
    if (Sender is TCustomForm) then
    begin
      if TCustomForm(Sender).BorderStyle <> bsDialog then
      begin
        SetClassLong(Handle, GCL_HICONSM, LONG(TCustomForm(Sender).SmallIconHandle));
        SetClassLong(Handle, GCL_HICON, LONG(TCustomForm(Sender).BigIconHandle));
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

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.DCReDraw
  Params: CanvasHandle - HDC to redraw
  Returns: Nothing

  Redraws (the window of) a canvas
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.DCRedraw(CanvasHandle: HDC);
begin
  // TODO: implement me!
  //DebugLn('TRACE:[TWinCEWidgetSet.ReDraw] Redrawing...');
  //DebugLn('TRACE:Invalidating the window');
  //DebugLn('TRACE:Updating the window');
  //DebugLn('TRACE:[TWinCEWidgetSet.ReDraw] Finished redrawing');
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.SetPixel
  Params:  Canvas - canvas to set color on
           X, Y   - position
           AColor - new color for specified position
  Returns: nothing

  Set the color of the specified pixel on the canvas
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
begin
  Windows.SetPixel(CanvasHandle, X, Y, TColor(ColorToRGB(AColor)));
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.GetPixel
  Params:  Canvas - canvas to get color from
           X, Y   - position
  Returns: Color at specified point

  Get the color of the specified pixel on the canvas
 -----------------------------------------------------------------------------}

function TWinCEWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
begin
  Result := Windows.GetPixel(CanvasHandle, X, Y);
end;
*)

