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

  WinCEWidgetSet := Self;
end;

destructor TWinCEWidgetSet.Destroy;
begin
  WinCEWidgetSet := nil;

  inherited Destroy;
end;

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


  // Create parent of all windows, `button on taskbar'
//does this work on wince?!
  FAppHandle := CreateWindow(@ClsName, CreatePWideCharFromString(Application.Title),
  WS_POPUP or WS_CLIPSIBLINGS or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX,
    0, {Windows.GetSystemMetrics(SM_CXSCREEN) div 2,}
    0, {Windows.GetSystemMetrics(SM_CYSCREEN) div 2,}
    0, 0, HWND(nil), HMENU(nil), HInstance, nil);
  AllocWindowInfo(FAppHandle);

  // set nice main icon
  SendMessage(FAppHandle, WM_SETICON, ICON_BIG,
    Windows.LoadIcon(MainInstance, 'MAINICON'));
  // remove useless menuitems from sysmenu

 // initialize ScreenInfo
  Handle := GetDesktopWindow;
  DC := Windows.GetDC(Handle);
  ScreenInfo.PixelsPerInchX := GetDeviceCaps(DC, LOGPIXELSX);
  ScreenInfo.PixelsPerInchY := GetDeviceCaps(DC, LOGPIXELSY);
  ScreenInfo.ColorDepth := GetDeviceCaps(DC, BITSPIXEL);
  ReleaseDC(Handle, DC);

end;

procedure TWinCEWidgetSet.AppMinimize;
begin

end;

procedure TWinCEWidgetSet.AppBringToFront;
begin
  Windows.SetForegroundWindow(FAppHandle);
end;

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


procedure TWinCEWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer;
  AColor: TGraphicsColor);
begin

end;

function TWinCEWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer
  ): TGraphicsColor;
begin
  Result:=clNone;
end;

procedure TWinCEWidgetSet.DCRedraw(CanvasHandle: HDC);
begin

end;

procedure TWinCEWidgetSet.SetDesigning(AComponent: TComponent);
begin

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
      DebugLn('[TWinCEWidgetSet.AppProcessMessages] MsgWaitForMultipleObjects returned: ', IntToStr(GetLastError));
      break;
    end;
  until false;
End;

procedure TWinCEWidgetSet.CheckPipeEvents;
var
  lHandler: PPipeEventInfo;
  lBytesAvail: dword;
begin
  lHandler := FWaitPipeHandlers;
  while lHandler <> nil do
  begin
    {
    roozbeh : ooops not supported
    if Windows.PeekNamedPipe(lHandler^.Handle, nil, 0, nil, @lBytesAvail, nil) then
    begin
      if lBytesAvail <> 0 then
        lHandler^.OnEvent(lHandler^.UserData, [prDataAvailable]);
    end else}
      lHandler^.OnEvent(lHandler^.UserData, [prBroken]);
    lHandler := lHandler^.Next;
  end;
end;

procedure TWinCEWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
var
  AMessage: TMsg;
begin
  {$ifdef VerboseWinCE}
  WriteLn('TWinCEWidgetSet.AppRun');
  {$endif}

  while Windows.GetMessage(@AMessage, 0, 0, 0) and (not AppTerminated) do
  begin
    Windows.TranslateMessage(@AMessage);
    Windows.DispatchMessage(@AMessage);
  end;
end;

procedure TWinCEWidgetSet.AppWaitMessage;
begin

end;

procedure TWinCEWidgetSet.AppTerminate;
begin
  AppTerminated := True;
end;

function TWinCEWidgetSet.InitHintFont(HintFont: TObject): Boolean;
begin

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
//roozbeh this is somehow very difficut to achieve for now!
//    Windows.SetMenu(AWinControl.Handle, AMenu.Handle);
    AddToChangedMenus(AWinControl.Handle);
  end;
end;

function TWinCEWidgetSet.CreateComponent(Sender: TObject): THandle;
begin

end;

function TWinCEWidgetSet.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc
  ): integer;
begin

end;

function TWinCEWidgetSet.DestroyTimer(TimerHandle: Integer): boolean;
begin

end;

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

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}
