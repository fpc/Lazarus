{%MainUnit win32int.pp}

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
constructor TWin32WidgetSet.Create;
begin
  inherited Create;
  FTimerData  := TList.Create;
  FMetrics.cbSize := SizeOf(FMetrics);
  FMetricsFailed := not Windows.SystemParametersInfo(SPI_GETNONCLIENTMETRICS,
    SizeOf(FMetrics), @FMetrics, 0);
  if FMetricsFailed then
  begin
    FMetrics.iMenuHeight := GetSystemMetrics(SM_CYMENU);
  end;
  OnClipBoardRequest := nil;
  Pointer(InitCommonControlsEx) := GetProcAddress(GetModuleHandle(comctl32), 'InitCommonControlsEx');
  FCommonControlsVersion := GetFileVersion(comctl32);
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
  Assert(False, 'Trace:TWin32WidgetSet is being destroyed');

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

  if FAppHandle <> 0 then
    DestroyWindow(FAppHandle);

  Windows.UnregisterClass(@ClsName, System.HInstance);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppInit
  Params:  None
  Returns: Nothing

  Initialize Windows
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppInit(var ScreenInfo: TScreenInfo);
var
  ICC: TINITCOMMONCONTROLSEX;
  SysMenu: HMENU;
  Handle: HWND;
  DC: HDC;
begin
  Assert(False, 'Trace:Win32Object.Init - Start');
  if not WinRegister then
  begin
    Assert(False, 'Trace:Win32Object.Init - Register Failed');
    DebugLn('Trace:Win32Object.Init - Register Failed');
    Exit;
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
    WS_CLIPSIBLINGS or WS_SYSMENU or WS_MINIMIZEBOX,
    0, {Windows.GetSystemMetrics(SM_CXSCREEN) div 2,}
    0, {Windows.GetSystemMetrics(SM_CYSCREEN) div 2,}
    0, 0, HWND(nil), HMENU(nil), HInstance, nil);
  AllocWindowInfo(FAppHandle);

  // set nice main icon
  Windows.SendMessage(FAppHandle, WM_SETICON, ICON_BIG,
    LParam(Windows.LoadIcon(MainInstance, 'MAINICON')));
  // remove useless menuitems from sysmenu
  SysMenu := Windows.GetSystemMenu(FAppHandle, False);
  Windows.DeleteMenu(SysMenu, SC_MAXIMIZE, MF_BYCOMMAND);
  Windows.DeleteMenu(SysMenu, SC_SIZE, MF_BYCOMMAND);
  Windows.DeleteMenu(SysMenu, SC_MOVE, MF_BYCOMMAND);

  // initialize ScreenInfo
  Handle := GetDesktopWindow;
  DC := Windows.GetDC(Handle);
  ScreenInfo.PixelsPerInchX := GetDeviceCaps(DC, LOGPIXELSX);
  ScreenInfo.PixelsPerInchY := GetDeviceCaps(DC, LOGPIXELSY);
  ScreenInfo.ColorDepth := GetDeviceCaps(DC, BITSPIXEL);
  ReleaseDC(Handle, DC);

  // Thread.Synchronize support
  WakeMainThread := @HandleWakeMainThread;

  Assert(False, 'Trace:Win32Object.Init - Exit');
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppMinimize
  Params:  None
  Returns: Nothing

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppMinimize;
begin
  Windows.SendMessage(FAppHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppRestore
  Params:  None
  Returns: Nothing

  Restore minimized whole application from taskbar
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppRestore;
begin
  Windows.SendMessage(FAppHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;


{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppBringToFront
  Params:  None
  Returns: Nothing

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppBringToFront;
begin
  Windows.SetForegroundWindow(FAppHandle);
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
Procedure TWin32WidgetSet.SetCallback(Msg: LongInt; Sender: TObject);
Var
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
  Window: HWnd;
Begin
  If Sender Is TControlCanvas Then
    Window := TControlCanvas(Sender).Handle
  Else If Sender Is TCustomForm Then
    Window := TCustomForm(Sender).Handle
  Else
    Window := (Sender as TWinControl).Handle;
  if Window=0 then exit;
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
  Method: TWin32WidgetSet.AppProcessMessages
  Params:  None
  Returns: Nothing

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppProcessMessages;
var
  AMessage: TMsg;
  AccelTable: HACCEL;
  retVal, index: dword;
  pHandles: Windows.LPHANDLE;
begin
  repeat
{$ifdef DEBUG_ASYNCEVENTS}
    if Length(FWaitHandles) > 0 then
      DebugLn('[ProcessMessages] WaitHandleCount=', IntToStr(FWaitHandleCount),
        ', WaitHandle[0]=', IntToHex(FWaitHandles[0], 8));
{$endif}
    pHandles := nil;
    if FWaitHandleCount > 0 then
      pHandles := @FWaitHandles[0];
    retVal := Windows.MsgWaitForMultipleObjects(FWaitHandleCount,
      pHandles, false, 0, QS_ALLINPUT);
    if (WAIT_OBJECT_0 <= retVal) and (retVal < WAIT_OBJECT_0 + FWaitHandleCount) then
    begin
      index := retVal-WAIT_OBJECT_0;
      FWaitHandlers[index].OnEvent(FWaitHandlers[index].UserData, 0);
    end else
    if retVal = WAIT_OBJECT_0 + FWaitHandleCount then
    begin
      while PeekMessage(AMessage, HWnd(Nil), 0, 0,PM_REMOVE) do
      begin
        AccelTable := GetWindowInfo(AMessage.HWnd)^.Accel;
        if (AccelTable = HACCEL(nil))
          or (TranslateAccelerator(AMessage.HWnd, AccelTable, @AMessage) = 0) then
        begin
          TranslateMessage(@AMessage);
          DispatchMessage(@AMessage);
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
End;

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
  timeout: dword;
  pHandles: Windows.LPHANDLE;
begin
  RedrawMenus;
  Assert(False, 'Trace:TWin32WidgetSet.WaitMessage - Start');
  if FWaitPipeHandlers <> nil then
    timeout := 100
  else
    timeout := INFINITE;
  pHandles := nil;
  if FWaitHandleCount > 0 then
    pHandles := @FWaitHandles[0];
  Windows.MsgWaitForMultipleObjects(FWaitHandleCount, pHandles,
    false, timeout, QS_ALLINPUT);
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

procedure TWin32WidgetSet.AppSetTitle(const ATitle: string);
begin
  {$ifdef WindowsUnicodeSupport}
  if UnicodeEnabledOS 
  then Windows.SetWindowTextW(FAppHandle, PWideChar(Utf8Decode(ATitle)))
  else Windows.SetWindowText(FAppHandle, PChar(Utf8ToAnsi(ATitle)));
  {$else}
  Windows.SetWindowText(FAppHandle, PChar(ATitle));
  {$endif}
end;

function TWin32WidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpWin32;
end;

{------------------------------------------------------------------------------
  Function: CreateTimer
  Params: Interval:
          TimerFunc: Callback
  Returns: a Timer id (use this ID to destroy timer)

  Design: A timer which calls TimerCallBackProc, is created.
    The TimerCallBackProc calls the TimerFunc.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : THandle;
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
function TWin32WidgetSet.DestroyTimer(TimerHandle: THandle) : boolean;
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

procedure TWin32WidgetSet.HandleWakeMainThread(Sender: TObject);
begin
  // wake up GUI thread by sending a message to it
  Windows.PostMessage(AppHandle, WM_NULL, 0, 0);
end;

procedure TWin32WidgetSet.AttachMenuToWindow(AMenuObject: TComponent);
var
  AMenu: TMenu;
  AWinControl: TWinControl;
begin
  AMenu := AMenuObject as TMenu;
  if AMenu is TMainMenu then
  begin
    AWinControl := TWinControl(AMenu.Owner);
    Windows.SetMenu(AWinControl.Handle, AMenu.Handle);
    //Set the right order menu after attach the menu
    SetMenuFlag(AMenu.Handle, MFT_RIGHTORDER or MFT_RIGHTJUSTIFY, AMenu.IsRightToLeft);
    AddToChangedMenus(AWinControl.Handle);
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
  WindowClassW: WndClassW;
Begin
  Assert(False, 'Trace:WinRegister - Start');
  if UnicodeEnabledOS 
  then begin
    with WindowClassW do
    begin
      Style := CS_DBLCLKS{CS_HRedraw or CS_VRedraw};
      LPFnWndProc := @WindowProc;
      CbClsExtra := 0;
      CbWndExtra := 0;
      hInstance := System.HInstance;
      hIcon := Windows.LoadIcon(MainInstance, 'MAINICON');
      if hIcon = 0 then
       hIcon := Windows.LoadIcon(0, IDI_APPLICATION);
      hCursor := Windows.LoadCursor(0, IDC_ARROW);
      hbrBackground := 0; {GetSysColorBrush(Color_BtnFace);}
      LPSzMenuName := Nil;
      LPSzClassName := PWideChar(WideString(ClsName));
    end;
    Result := Windows.RegisterClassW(@WindowClassW) <> 0;
  end
  else begin
    with WindowClass do
    begin
      Style := CS_DBLCLKS{CS_HRedraw or CS_VRedraw};
      LPFnWndProc := @WindowProc;
      CbClsExtra := 0;
      CbWndExtra := 0;
      hInstance := System.HInstance;
      hIcon := Windows.LoadIcon(MainInstance, 'MAINICON');
      if hIcon = 0 then
       hIcon := Windows.LoadIcon(0, IDI_APPLICATION);
      hCursor := Windows.LoadCursor(0, IDC_ARROW);
      hbrBackground := 0; {GetSysColorBrush(Color_BtnFace);}
      LPSzMenuName := Nil;
      LPSzClassName := @ClsName; 
    end;
    Result := Windows.RegisterClass(@WindowClass) <> 0;
  end;
  Assert(False, 'Trace:WinRegister - Exit');
end;

function TWin32WidgetSet.CreateThemeServices: TThemeServices;
begin
  Result := TWin32ThemeServices.Create;
end;

 {------------------------------------------------------------------------------
  Function: TWin32WidgetSet.CreateComponent
  Params:   Sender - object for which to create visual representation
  Returns:  nothing

  Tells Windows to create a control
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateComponent(Sender: TObject): THandle;
begin
  DebugLn('WARNING: TWin32WidgetSet.CreateComponent is deprecated, should not be reachable!!');
  Result := 0;
end;

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
procedure TWin32WidgetSet.ShowHide(Sender: TObject);
var
  Handle: HWND;
  ParentPanel: HWND;
  Flags: dword;
begin
  Handle := ObjectToHWND(Sender);
  ParentPanel := GetWindowInfo(Handle)^.ParentPanel;
  if ParentPanel <> 0 then
    Handle := ParentPanel;
  if TControl(Sender).HandleObjectShouldBeVisible then
  begin
    Assert(False, 'Trace: [TWin32WidgetSet.ShowHide] Showing the window');
    if TControl(Sender).FCompStyle = csHintWindow then
    begin
      Windows.SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
    end else
    begin
      Flags := SW_SHOW;
      if TControl(Sender) is TCustomForm then
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
    if (Sender Is TCustomForm) then
      SetClassLong(Handle, GCL_HICON, LONG(TCustomForm(Sender).GetIconHandle));
  end
  else
  begin
    Assert(False, 'TRACE: [TWin32WidgetSet.ShowHide] Hiding the window');
    ShowWindow(Handle, SW_HIDE);
  end;
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
  Assert(False, 'TRACE:[TWin32WidgetSet.ReDraw] Redrawing...');
  Assert(False, 'TRACE:Invalidating the window');
  Assert(False, 'TRACE:Updating the window');
  Assert(False, 'TRACE:[TWin32WidgetSet.ReDraw] Finished redrawing');
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.SetPixel
  Params:  Canvas - canvas to set color on
           X, Y   - position
           AColor - new color for specified position
  Returns: nothing

  Set the color of the specified pixel on the canvas
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
begin
  Windows.SetPixel(CanvasHandle, X, Y, AColor);
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
