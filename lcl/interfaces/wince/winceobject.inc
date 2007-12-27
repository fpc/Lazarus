{%MainUnit winceint.pp}

{******************************************************************************
                                   winceobject.inc
 ******************************************************************************

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

{ TWinCEWidgetSet }

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TWinCEWidgetSet.Create;
begin
  Inherited Create;
  FTimerData  := TList.Create;
  FMetrics.cbSize := SizeOf(FMetrics);
  //roozbeh:new stuff seems to be always true as we dont have that flag under ce??!!
  FMetricsFailed := True;
  //FMetricsFailed := not Windows.SystemParametersInfo(SPI_GETNONCLIENTMETRICS,
  //  SizeOf(FMetrics), @FMetrics, 0);
  if FMetricsFailed then
  begin
    FMetrics.iMenuHeight := GetSystemMetrics(SM_CYMENU);
  end;
//roozbeh what does it mean?!
  WinCEWidgetSet := Self;
end;
{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
Destructor TWinCEWidgetSet.Destroy;
var
  n: integer;
  TimerInfo : PWinCETimerInfo;
Begin
  Assert(False, 'Trace:TWin32WidgetSet is being destroyed');

  n := FTimerData.Count;
  if (n > 0) then
  begin
    DebugLn(Format('[TWin32WidgetSet.Destroy] WARNING: There are %d TimerInfo structures left, I''ll free them' ,[n]));
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

  inherited Destroy;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppInit
  Params:  None
  Returns: Nothing

  Initialize Windows
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
var
  Handle: HWND;
  DC: HDC;
  Flags : integer;
begin
  {$ifdef VerboseWinCE}
  WriteLn('TWinCEWidgetSet.AppInit');
  {$endif}

  if not WinRegister then
  begin
    WriteLn('TWinCEWidgetSet.AppInit failed.');
    Exit;
  end;
  
    //Init stock objects;
  FStockNullBrush := Windows.CreateSolidBrush(0);
  FStockBlackBrush := Windows.CreateSolidBrush($000000);
  FStockLtGrayBrush := Windows.CreateSolidBrush($C0C0C0);
  FStockGrayBrush := Windows.CreateSolidBrush($808080);
  FStockDkGrayBrush := Windows.CreateSolidBrush($404040);
  FStockWhiteBrush := Windows.CreateSolidBrush($FFFFFF);

  if FMetricsFailed then
  begin
    FStatusFont := Windows.GetStockObject(DEFAULT_GUI_FONT);
    FMessageFont := Windows.GetStockObject(DEFAULT_GUI_FONT);
  end else begin
    FStatusFont := Windows.CreateFontIndirect(@FMetrics.lfStatusFont);
    FMessageFont := Windows.CreateFontIndirect(@FMetrics.lfMessageFont);
  end;

  // Create parent of all windows, 'button on taskbar'
//does this work on wince?!
  FAppHandle := CreateWindow(@ClsName, StringToPWideChar(Application.Title),
  WS_POPUP or WS_CLIPSIBLINGS or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX,
    0, {Windows.GetSystemMetrics(SM_CXSCREEN) div 2,}
    0, {Windows.GetSystemMetrics(SM_CYSCREEN) div 2,}
    0, 0, HWND(nil), HMENU(nil), HInstance, nil);
  AllocWindowInfo(FAppHandle);

  // set nice main icon
  SendMessage(FAppHandle, WM_SETICON, ICON_BIG,
    Windows.LoadIcon(MainInstance, 'MAINICON'));
  // remove useless menuitems from sysmenu
  {}
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
  WakeMainThread := @HandleWakeMainThread;
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppMinimize
  Params:  None
  Returns: Nothing

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.AppMinimize;
begin
//  Windows.SendMessage(FAppHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppRestore
  Params:  None
  Returns: Nothing

  Restore minimized whole application from taskbar
 ------------------------------------------------------------------------------}

procedure TWinCEWidgetSet.AppRestore;
begin
//  Windows.SendMessage(FAppHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;


{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppBringToFront
  Params:  None
  Returns: Nothing

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.AppBringToFront;
begin
  Windows.SetForegroundWindow(FAppHandle);
end;


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
Procedure TWinCEWidgetSet.SetCallback(Msg: LongInt; Sender: TObject);
Var
  Window: HWnd;
Begin
  Assert(False, 'Trace:TWinCEWidgetSet.SetCallback - Start');
  Assert(False, Format('Trace:TWinCEWidgetSet.SetCallback - Class Name --> %S', [Sender.ClassName]));
  Assert(False, Format('Trace:TWinCEWidgetSet.SetCallback - Message Name --> %S', [GetMessageName(Msg)]));
  If Sender Is TControlCanvas Then
    Window := TControlCanvas(Sender).Handle
  Else If Sender Is TCustomForm Then
    Window := TCustomForm(Sender).Handle
  Else
    Window := TWinControl(Sender).Handle;
  if Window=0 then exit;

  Assert(False, 'Trace:TWinCEWidgetSet.SetCallback - Exit');
End;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.RemoveCallbacks
  Params:   Sender - object from which to remove callbacks
  Returns:  nothing

  Removes Call Back Signals from the sender
 ------------------------------------------------------------------------------}
Procedure TWinCEWidgetSet.RemoveCallbacks(Sender: TObject);
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

//roozbeh : how can we have hints on ce?
{
function TWinCEWidgetSet.InitHintFont(HintFont: TObject): Boolean;
begin
  TFont(HintFont).Name := FMetrics.lfStatusFont.lfFaceName;
  TFont(HintFont).Style := [];
  TFont(HintFont).Height := FMetrics.lfStatusFont.lfHeight;
  TFont(HintFont).Color := clInfoText;
  TFont(HintFont).Pitch := fpDefault;
  Result := true;
end;
}
{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppProcessMessages
  Params:  None
  Returns: Nothing

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.AppProcessMessages;
var
  AMessage: TMsg;
  AccelTable: HACCEL;
  retVal, index: dword;
Begin
  repeat
{$ifdef DEBUG_ASYNCEVENTS}
    if Length(FWaitHandles) > 0 then
      DebugLn('[ProcessMessages] WaitHandleCount=', IntToStr(FWaitHandleCount),
        ', WaitHandle[0]=', IntToHex(FWaitHandles[0], 8));
{$endif}
    retVal := Windows.MsgWaitForMultipleObjects(FWaitHandleCount,
      FWaitHandles[0], false, 0, QS_ALLINPUT);

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

    if FWaitHandleCount = 0 then
     break;

    end
    else
    if retVal = $FFFFFFFF then
    begin
      DebugLn('[TWinCEWidgetSet.AppProcessMessages] MsgWaitForMultipleObjects returned: ', IntToStr(GetLastError));
      break;
    end;

  until false;
End;

procedure TWinCEWidgetSet.CheckPipeEvents;
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
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Passes execution control to Windows
 ------------------------------------------------------------------------------}
//roozbeh:new update...whole procedure body is added.what is it?
procedure TWinCEWidgetSet.AppWaitMessage;
var
  timeout,retVal: dword;
begin
  RedrawMenus;
  Assert(False, 'Trace:TWin32WidgetSet.WaitMessage - Start');
  if FWaitPipeHandlers <> nil then
    timeout := 100
  else
    timeout := INFINITE;

//roozbeh...remove raise after testing!
  retVal := Windows.MsgWaitForMultipleObjects(FWaitHandleCount, FWaitHandles[0],
    false, timeout, QS_ALLINPUT);

  if retVal = $FFFFFFFF then
    RaiseGDBException('Failaure on MsgWaitForMultipleObjects');

  Assert(False,'Trace:Leave wait message');
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Tells Windows to halt and destroy
 ------------------------------------------------------------------------------}

procedure TWinCEWidgetSet.AppTerminate;
begin
  Assert(False, 'Trace:TWin32WidgetSet.AppTerminate - Start');
//  roozbeh
//  not existed in win32
//  AppTerminated := True;
//  PostQuitMessage(0);
end;


procedure TWinCEWidgetSet.AppSetTitle(const ATitle: string);
var
tmpText : PWideChar;
begin
  tmpText := StringToPWideChar(ATitle);
  Windows.SetWindowText(FAppHandle, @tmpText);
  FreeMem(tmpText);
end;


function TWinCEWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpWinCE;
end;

{------------------------------------------------------------------------------
  Function: CreateTimer
  Params: Interval:
          TimerFunc: Callback
  Returns: a Timer id (use this ID to destroy timer)

  Design: A timer which calls TimerCallBackProc, is created.
    The TimerCallBackProc calls the TimerFunc.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : THandle;
var
  TimerInfo: PWinCETimerInfo;
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
function TWinCEWidgetSet.DestroyTimer(TimerHandle: THandle) : boolean;
var
  n : integer;
  TimerInfo : PWinCETimerinfo;
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

procedure TWinCEWidgetSet.HandleWakeMainThread(Sender: TObject);
begin
  // wake up GUI thread by sending a message to it
  Windows.PostMessage(AppHandle, WM_NULL, 0, 0);
end;

procedure TWinCEWidgetSet.AttachMenuToWindow(AMenuObject: TComponent);
var
  AMenu: TMenu;
  AWinControl: TWinControl;
begin
  AMenu := AMenuObject as TMenu;
  if AMenu is TMainMenu then
  begin
    AWinControl := TWinControl(AMenu.Owner);
    CeSetMenu(AWinControl.Handle, AMenu.Handle);
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
Function TWinCEWidgetSet.WinRegister: Boolean;
Var
  WindowClass: Windows.WndClass;
Begin
  FillChar(WindowClass, SizeOf(WindowClass), #0);

  with WindowClass do
  begin
    LPFnWndProc := @WindowProc;
    Style := CS_HREDRAW or CS_VREDRAW;
    CbClsExtra := 0;
    CbWndExtra := 0;
    hInstance := System.HInstance;
    hIcon := Windows.LoadIcon(System.hInstance, nil);
    if hIcon = 0 then
      hIcon := Windows.LoadIcon(0, IDI_APPLICATION);
    hCursor := LoadCursor(0, IDC_ARROW);
    hbrBackground := GetSysColorBrush(Color_BtnFace);
    LPSzMenuName := nil;
    LPSzClassName := @ClsName;
  end;
  
  Result := Windows.RegisterClass(@WindowClass) <> 0;
End;




{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.ShowHide
  Params: Sender - The sending object
  Returns: Nothing

  Shows or hides a control
 ------------------------------------------------------------------------------}
Procedure TWinCEWidgetSet.ShowHide(Sender: TObject);
Var
  Handle: HWND;
  ParentPanel: HWND;
  Flags: dword;
Begin
  //If (TControl(Sender).FCompStyle = csPage) or (TControl(Sender).FCompStyle = csToolButton) then exit;
  Handle := ObjectToHWND(Sender);
//  ParentPanel := GetWindowInfo(Handle)^.ParentPanel;
//  if ParentPanel <> 0 then
//    Handle := ParentPanel;
  If TControl(Sender).HandleObjectShouldBeVisible Then
  Begin
    Assert(False, 'Trace: [TWin32WidgetSet.ShowHide] Showing the window');
    if TControl(Sender).FCompStyle = csHintWindow then
    begin
      Windows.SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
    end else begin
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
    If (Sender Is TCustomForm) Then
      SetClassLong(Handle, GCL_HICON, LONG(TCustomForm(Sender).GetIconHandle));
  End
  Else
  Begin
    Assert(False, 'TRACE: [TWin32WidgetSet.ShowHide] Hiding the window');
    ShowWindow(Handle, SW_HIDE);
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.DCReDraw
  Params: CanvasHandle - HDC to redraw
  Returns: Nothing

  Redraws (the window of) a canvas
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.DCRedraw(CanvasHandle: HDC);
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
procedure TWinCEWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
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

function TWinCEWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer
  ): TGraphicsColor;
begin
  Result := Windows.GetPixel(CanvasHandle, X, Y);
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}
